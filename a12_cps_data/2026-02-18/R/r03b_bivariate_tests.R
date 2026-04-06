
library(tidyverse)

# load function to access variables in globals.yaml
source(here::here("a11_sow3_longitudinal", "get_global.R"))

# load addl functions
source(file.path(
    get_global("project_dir"),
    "R",
    get_global("code_files") |> pluck("functions")
))

# school_df <- read_rds(file.path(
#     get_global("project_dir"),
#     "data",
#     get_global("analytic_files") |>
#         pluck("school_chars")
# ))

student_df <- read_rds(file.path(
    get_global("project_dir"),
    "data",
    get_global("analytic_files") |>
        pluck("student_counts")
 ))

gend_df <- student_df |>
    mutate(
        student_count_total = sum(student_count, na.rm = TRUE),
        name1               = glue::glue(
            "gend_{x}_yes",
            x = str_trunc(snakecase::to_snake_case(
                as.character(student_gender)
                ), 20)
            ),
        .by = c("school_id", "data_schoolyear")
        ) |>
    summarise(
        value1 = sum(student_count, na.rm = TRUE),
        .by    = c(
            "school_id", "data_schoolyear", "student_count_total",
            "name1"
            )
        ) |>
    mutate(
        name2  = gsub("yes", "no", name1),
        value2 = student_count_total - value1
        )
re_df <- student_df |>
    mutate(
        student_count_total = sum(student_count, na.rm = TRUE),
        name1               = glue::glue(
            "re_{x}_yes",
            x = str_trunc(snakecase::to_snake_case(
                as.character(student_race)
            ), 20)
        ),
        .by = c("school_id", "data_schoolyear")
    ) |>
    summarise(
        value1 = sum(student_count, na.rm = TRUE),
        .by    = c(
            "school_id", "data_schoolyear", "student_count_total",
            "name1"
        )
    ) |>
    mutate(
        name2  = gsub("yes", "no", name1),
        value2 = student_count_total - value1
    )






student_df_agg <- student_df |>
    mutate(
        student_days_yes = round(student_days_present),
        student_days_no = round(student_days_absent)
        ) |>
    summarise(
        across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
        .by = c("school_id", "data_schoolyear", "data_date")
        ) |>
    arrange(data_date) |>
    mutate(
        school_id = factor(school_id),
        data_schoolyear = factor(data_schoolyear),
        data_schoolyear_order = as.numeric(data_schoolyear)
        )

student_df_agg <- full_join(
    student_df_agg,
    list(gend_df, re_df) |>
        map(~{
            full_join(
                .x |>
                    pivot_wider(
                        names_from = name1,
                        values_from = value1,
                        id_cols = c("school_id", "data_schoolyear"),
                        values_fill = 0
                    ),
                .x |>
                    pivot_wider(
                        names_from = name2,
                        values_from = value2,
                        id_cols = c("school_id", "data_schoolyear"),
                        values_fill = 0
                    ),
                by = c("school_id", "data_schoolyear")
            )
        }) |>
        reduce(full_join, by = c("school_id", "data_schoolyear")) |>
        janitor::clean_names(),

    by = c("school_id", "data_schoolyear")
)





mods_1way <- grep("yes", names(student_df_agg), value = TRUE) |>
    set_names() |>
    map(safely(function(v) {
        eval(bquote(
            geepack::geeglm(
                .(as.formula(glue::glue(
                    "cbind({yes}, {no}) ~ data_schoolyear",
                    yes = v,
                    no  = gsub("yes", "no", v)
                    ))),
                id     = school_id,
                family = binomial(link = "logit"),
                waves  = data_schoolyear_order,
                data   = student_df_agg |>
                    filter(if_any(
                        all_of(c(v, gsub("yes", "no", v))),
                        ~ .x > 0
                        ))
                )
            ))
        }))

mods_1way |>
    transpose() |>
    pluck("error") |>
    compact()

mods_1way <- mods_1way |>
    transpose() |>
    pluck("result") |>
    compact()



student_df_agg |>
    filter(if_any(
        all_of(c("re_middle_eastern_or_yes", gsub("yes", "no", "re_middle_eastern_or_yes"))),
        ~ .x > 0
    ))

write_rds(file.path(
    dir_create_if(file.path(get_global("project_dir"), "output", Sys.Date())),
    "gee_mods_1way.rds"
))


map(mods_1way, ~{
    emmeans::emmeans(.x, ~ data_schoolyear, type = "response")
    })


emmeans::emmeans(first(mods_1way), ~ data_schoolyear, type = "response") |>
    emmeans::contrast("trt.vs.ctrl", infer = TRUE, adjust = "mvt")
emmeans::emmeans(first(mods_1way), ~ data_schoolyear, type = "response") |>
    emmeans::contrast("consec", infer = TRUE)



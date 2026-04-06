
# load libraries
library(tidyverse)
library(arsenal)


# load function to access variables in globals.yaml
source(here::here("a11_sow3_longitudinal", "get_global.R"))

# load addl functions
source(file.path(
    get_global("project_dir"),
    "R",
    get_global("code_files") |> pluck("functions")
))

# Load Data ---------------------------------------------------------------

# maybe run data preprocessing code
# source(file.path(get_global("project_dir"), "R", "2025-12-17", "preproc.R"))

# add_todo("Add data as-of dates to globals.yaml instead of hard-coding file paths.")

# # load cleaned datasets
# student_df <- read_rds(file.path(
#   get_global("project_dir"),
#   "data", "2025-12-17",
#   "student_counts_2025-12-17.rds"
# ))
#
# add_todo("Fix discrepancies in school region and wave.")
#
# school_df <- read_rds(file.path(
#   get_global("project_dir"),
#   "data", "2025-12-17",
#   "school_chars_2025-12-17.rds"
#     )) |>
#     arrange(school_id, wave) |>
#     mutate(wave = last(wave), .by = school_id) |>
#     distinct()
#
# add_todo("Remove school-level chars from use_df; merge them in when needed.")
#
# use_df <- read_rds(file.path(
#   get_global("project_dir"),
#   "data", "2025-12-17",
#   "inhaler_use_analytic_2025-12-17.rds"
#   )) |>
#   select(-any_of(names(school_df)), school_id) |>
#   relocate(school_id)


# school_df <- read_rds(file.path(
#     get_global("project_dir"),
#     "data",
#     get_global("analytic_files") |>
#         pluck("school_chars")
#     ))
#
# student_df <- read_rds(file.path(
#     get_global("project_dir"),
#     "data",
#     get_global("analytic_files") |>
#         pluck("student_counts")
#     ))
#
# use_df <- read_rds(file.path(
#     get_global("project_dir"),
#     "data",
#     get_global("analytic_files") |>
#         pluck("inhaler_use_analytic")
#     ))

# -------------------------------------------------------------------------
#
# # Table 1: Demographics
# # add_todo("Look into scoping table-specific objects.")
#
# # Part 1a: School Level
#
# # join school-level variables and summarized student-level variables
# t1a_df <- right_join(
#
#     read_analytic("school_chars") |>
#         distinct(school_id, grade_level, region, region_3grp, wave),
#
#     read_analytic("student_counts") |>
#         summarise(
#             attendance = weighted.mean(average_attendance, student_count),
#             n_students = sum(student_count),
#             across(matches("count_yes"), list(
#                 # n   = ~ sum(.x, na.rm = TRUE),
#                 pct = ~ sum(.x, na.rm = TRUE) / sum(student_count)
#             ),
#             .names = "{.fn}_{gsub('_count', '', .col)}"
#             ),
#             .by = c(school_id, data_schoolyear)
#         ),
#     by = "school_id"
# ) #|>

    # stack different variables
    # pivot_longer(where(is.numeric), names_to = "variable") |>
    #
    # # for each school, variable:
    # ## identify baseline value; populate for all years
    # mutate(
    #     baseline_schoolyear = "2021-2022",
    #     baseline_value = case_when(
    #         data_schoolyear == baseline_schoolyear ~ value
    #     )
    # ) |>
    # group_by(school_id, variable) |>
    # fill(baseline_value, .direction = "downup") |>
    # ungroup() |>
    #
    # ## calculate absolute and relative change
    # ## from baseline to each subsequent year
    # mutate(
    #     abs_change = case_when(
    #         data_schoolyear != baseline_schoolyear ~  (value - baseline_value)
    #     ),
    #     rel_change = case_when(
    #         data_schoolyear == baseline_schoolyear ~ NA,
    #         baseline_value == 0 ~ NA,
    #         .default = ((value - baseline_value) / baseline_value)
    #     )
    # )

# names(t1a_df)

# investigate rows with missing data
# anti_join(
#   t1a_df,
#   na.omit(t1a_df)
# )

# calculate p-values for change from 2022-2025
# t1a_pvals <- list(
#   n_students = t1a_df |>
#     pivot_wider(names_from = data_schoolyear, values_from = n_students, id_cols = school_id) |>
#     mutate(n_students_change = `2025` - `2022`) |>
#     pull(n_students_change) |>
#     t.test(),
#   attendance = t1a_df |>
#     pivot_wider(names_from = data_schoolyear, values_from = attendance, id_cols = school_id) |>
#     mutate(n_students_change = `2025` - `2022`) |>
#     pull(n_students_change) |>
#     t.test(),
#   grade_level = htestClust::chisqtestClust(
#     x        = t1a_df_complete$grade_level,
#     y        = t1a_df_complete$data_schoolyear,
#     id       = t1a_df_complete$school_id,
#     variance = "sand.est"
#   ),
#   region_3grp = htestClust::chisqtestClust(
#     x        = t1a_df_complete$region_3grp,
#     y        = t1a_df_complete$data_schoolyear,
#     id       = t1a_df_complete$school_id,
#     variance = "sand.est"
#     )
#   ) |>
#   map_dfr(broom::tidy, .id = "variable")

# t1a_df
#
# # summary stats
# t1a <- arsenal::tableby(
#     data_schoolyear ~ overall + region_3grp + grade_level + n_students + attendance,
#     data     = t1a_df |> mutate(overall = "overall"),
#     total    = FALSE,
#     test     = FALSE,
#     digits.n = NA
#
#     ) |>
#     summary(text = TRUE) |>
#     as.data.frame() |>
#     rename(variable = 1) #|>
#
#   # add pvals
#   left_join(
#     t1a_pvals[, c("variable", "p.value", "method")],
#     by = "variable"
#   )

# Part 1b: Student Level

# calculate p-vals for change from 2022-2025
# t1b_pvals <- c(
#   list(
#     student_gender = student_df |>
#       filter(data_schoolyear %in% c(2022, 2025)) |>
#       summarise(
#         value = sum(student_count),
#         .by = c(data_schoolyear, student_gender)
#       ) |>
#       pivot_wider(
#         names_from = data_schoolyear
#       ) |>
#       na.omit() |>
#       column_to_rownames("student_gender"),
#
#     student_race = student_df |>
#       filter(data_schoolyear %in% c(2022, 2025)) |>
#       summarise(
#         value = sum(student_count),
#         .by = c(data_schoolyear, student_race)
#       ) |>
#       pivot_wider(
#         names_from = data_schoolyear
#       ) |>
#       na.omit() |>
#       column_to_rownames("student_race")
#   ),
#
#   student_df |>
#     filter(data_schoolyear %in% c(2022, 2025)) |>
#     summarise(
#       across(c(matches("yes")), sum),
#       .by        = c(data_schoolyear)
#     ) |>
#     rename_with(~ gsub("_yes", "", .x)) |>
#     pivot_longer(-c(1:2), values_to = "yes") |>
#     mutate(no = student_count - yes) |>
#     (\(.) split(., .$name))() |>
#     map(~{.x |> column_to_rownames("data_schoolyear") |> select(yes, no)})
#   ) |>
#   map_dfr(~{
#     .x |>
#       as.matrix() |>
#       chisq.test() |>
#       broom::tidy()
#     }, .id = "variable")


# # summary stats
# t1b <- list(
#     read_analytic("student_counts") |>
#         summarise(
#             n = sum(student_count),
#             .by = c(data_schoolyear, student_gender)
#         ) |>
#         mutate(
#             value = sprintf(
#                 "%s (%.1f%%)",
#                 format(n, big.mark = ","),
#                 (n / sum(n))*100
#             ),
#             name = paste0(student_gender),
#             .by = data_schoolyear
#         ) |>
#         add_case(name = "student_gender", data_schoolyear = "2021-2022", .before = 1),
#     read_analytic("student_counts") |>
#         summarise(
#             n = sum(student_count),
#             .by = c(data_schoolyear, student_race)
#         ) |>
#         mutate(
#             value = sprintf(
#                 "%s (%.1f%%)",
#                 format(n, big.mark = ","),
#                 (n / sum(n))*100
#             ),
#             name = paste0(student_race),
#             .by = data_schoolyear
#         ) |>
#         add_case(name = "student_race", data_schoolyear = "2021-2022", .before = 1),
#
#     read_analytic("student_counts") |>
#         summarise(
#             across(c("student_count", matches("yes")), sum),
#             .by        = c(data_schoolyear)
#         ) |>
#         mutate(across(
#             matches("yes$"),
#             ~ sprintf(
#                 "%s (%.1f%%)",
#                 format(.x, big.mark = ","),
#                 (.x / student_count)*100
#             )
#         ),
#         student_count = as.character(student_count)
#
#         ) |>
#         pivot_longer(c("student_count", matches("yes")))
# ) |>
#     map_dfr( ~{
#         .x |>
#             pivot_wider(
#                 names_from  = data_schoolyear,
#                 values_from = value,
#                 id_cols     = name
#             ) |>
#             rename(variable = name)
#     }) #|>


# # add p-values
# left_join(
#   t1b_pvals |>
#     select(variable, p.value, method),
#   by = "variable"
# )

# rbind(t1a, t1b) |>
#     write_rds(file.path(
#         dir_create_if(file.path(get_global("project_dir"), "output", Sys.Date())),
#         "Table1_Descriptive.rds"
#         ))


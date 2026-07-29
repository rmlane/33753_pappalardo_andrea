# 2026-07-28
# Process new CPS data extract; merge with previously processed variables

standardize_school_names <- function(vec) {
  v2 <- vec
  
  v2 <- gsub("0 Pilot", "", v2)
  v2 <- snakecase::to_snake_case(v2)
  v2 <- gsub("_", "", v2)
  
  case_when(
    v2 == "cant" ~ "canty",
    v2 == "kenwood" ~ "kenwoodhs",
    v2 == "lindbloom" ~ "lindblomhs",
    .default = v2
    ) |> 
    toupper()
}

# load raw data from file
df_raw <- boxr::box_read(
  file_id = params$raw_data_cps_counts$box_id,
  sheet = "Updated SY22, 23, 24 and 25 Ast"
  ) 

# tidy variables
df_mod <- df_raw |> 
  janitor::clean_names() |> 
  
  # count variables
  mutate(
    student_count = asthma_count,
    asthma_count  = student_count*(asthma_status == "Yes"),
    .after        = asthma_status
  ) |> 
  
  # calculate attendance variables
  mutate(
    days_per_stu_expected = 176,
    days_per_row_expected = days_per_stu_expected*student_count,
    days_per_row_present  = (average_attendance/100)*days_per_row_expected,
    days_per_row_absent   = (1 - (average_attendance/100))*days_per_row_expected
    ) |> 
  
  # categorical variables
  mutate(
    data_schoolyear   = enrollment_year,
    school_match_name = standardize_school_names(student_current_school),
    student_race      = fct_collapse(
      snakecase::to_title_case(student_race, parsing_option = 0),
      "Asian, Hawaiian, or Pacific Islander" = c(
        "Asian",
        "Asian or Pacific Islander",
        "Hawaiian or Pacific Islander"
      ),
      "Missing" = "N/a"
    ),
    student_gender = fct_recode(
      student_gender,
      Missing = "@ERR"
      ),
    asthma_status = fct_recode(
      asthma_status,
      "Asthma Dx"    = "Yes",
      "No Asthma Dx" = "No"
      ) 
    ) |> 
  
  left_join(
    box_file_attr(
      "cps_school_characteristics.csv",
      dir_id = params$analytic_data_dir_id
    ) |> 
      pluck("id") |> 
      boxr::box_read() |> 
      distinct(school_id, school_match_name),
    
    by = "school_match_name"
  )




# calculate majority race/ethnicity per school, schoolyear
df_mod <- full_join(
  df_mod,
  df_mod |> 
    summarise(
      student_count = sum(student_count, na.rm = TRUE),
      .by           = c(school_id, data_schoolyear, student_race),
    ) |> 
    mutate(
      re_majority = student_count / sum(student_count, na.rm = TRUE),
      .by         = c(school_id, data_schoolyear)
    ) |> 
    filter(
      student_count == max(student_count), 
      .by = c(school_id, data_schoolyear)
    ) |> 
    mutate(
      re_majority = case_when(
        re_majority >= 0.5 ~ student_race,
        .default = "No Majority"
      )
    ) |> 
    distinct(school_id, data_schoolyear, re_majority),
  
  by = c("school_id", "data_schoolyear")
)


# summarize count vars at the school and schoolyear level
school_counts <- list(
  overall = df_mod |> mutate(by_group = "Overall"),
  asthma  = df_mod |> mutate(by_group = asthma_status),
  race    = df_mod |> mutate(by_group = student_race),
  gender  = df_mod |> mutate(by_group = student_gender)
  ) |> 
  
  imap(~{
    .x |> 
      mutate(by_var = .y) |> 
      summarise(
        across(
          c(matches("_count"), matches("days_per_row")),
          ~ sum(.x)
          ),
        .by = c(school_id, data_schoolyear, by_var, by_group)
        ) |> 
      mutate(
        across(
          matches("_count"), ~ .x / student_count, 
          .names = "{gsub('count', 'pct', .col)}"
          ),
        across(
          matches("days_per_row"), ~ .x / days_per_row_expected,
          .names = "{gsub('days', 'pct_days', .col)}"
          ),
        by_group = snakecase::to_snake_case(as.character(by_group))
      ) |> 
      pivot_wider(
        names_from  = c(by_var, by_group),
        values_from = c(matches("_count"), matches("days_per_row")),
        id_cols     = c(school_id, data_schoolyear),
        names_glue  = "{.value}.{by_var}.{by_group}"
      )
    }) |> 
  
  
  reduce(
    full_join, 
    by = c("school_id", "data_schoolyear")
    ) |> 
  
  rename_with(~ gsub("overall.overall", "overall", .x)) |> 
  
  full_join(
    df_mod |> 
      distinct(
        school_id, data_schoolyear, days_per_stu_expected, re_majority
        ),
    by = c("school_id", "data_schoolyear")
  ) |> 
  
  box_write_if_diff(
    f_name  = "cps_school_level_student_counts.csv",
    comment = paste(
      "(renamed from school_level_student_counts.csv): School- and schoolyear-level student counts, 2021-2022 to 2024-2025.",
      "Based on CPS extract file ", params$raw_data_cps_counts$filename,
      ", sheet", params$raw_data_cps_counts$sheet, ". ",
      "Includes counts of students overall and with asthma, allergy,",
      "support plans, and select census variables. Also includes counts ",
      "of attendance days (present, absent). "
  ))






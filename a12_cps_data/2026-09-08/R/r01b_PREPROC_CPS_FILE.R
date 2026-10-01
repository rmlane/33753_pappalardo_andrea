# 2026-07-28
# Process new CPS data extract

# load raw data from file
df_raw <- boxr::box_read(
  file_id = params$raw_data_cps_counts$box_id,
  sheet   = params$raw_data_cps_counts$sheet
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
    days_per_student_expected = 176,
    days_per_row_expected = days_per_student_expected*student_count,
    days_per_row_present  = (average_attendance/100)*days_per_row_expected,
    days_per_row_absent   = (1 - (average_attendance/100))*days_per_row_expected
    ) |> 
  
  # categorical variables
  mutate(
    data_schoolyear   = enrollment_year,
    short_name        = str_trim(gsub("0 Pilot", "", student_current_school)),
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
    ) 

# calculate majority race/ethnicity per school, schoolyear
df_mod <- full_join(
  df_mod,
  df_mod |> 
    summarise(
      student_count = sum(student_count, na.rm = TRUE),
      .by           = c(school_match_name, data_schoolyear, student_race),
    ) |> 
    mutate(
      re_majority = student_count / sum(student_count, na.rm = TRUE),
      .by         = c(school_match_name, data_schoolyear)
    ) |> 
    filter(
      student_count == max(student_count), 
      .by = c(school_match_name, data_schoolyear)
    ) |> 
    mutate(
      re_majority = case_when(
        re_majority >= 0.5 ~ student_race,
        .default = "No Majority"
      )
    ) |> 
    distinct(school_match_name, data_schoolyear, re_majority),
  
  by = c("school_match_name", "data_schoolyear")
)


# summarize count vars at the school and schoolyear level
df_proc <- list(
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
          ~ coalesce(sum(.x, na.rm = TRUE), 0)
          ),
        .by = c(school_match_name, data_schoolyear, by_var, by_group)
        ) |> 
      
      # attendance rates
      mutate(
        across(
          c("days_per_row_present", "days_per_row_absent"), 
          ~ .x / days_per_row_expected,
          .names = "{gsub('days', 'pct_days', .col)}"
          ),
        across(
          c("days_per_row_present", "days_per_row_absent"), 
          ~ .x / student_count,
          .names = "{gsub('per_row', 'per_student', .col)}"
        ),
        by_group = snakecase::to_snake_case(as.character(by_group))
      ) |> 
      pivot_wider(
        names_from  = c(by_var, by_group),
        values_from = c(matches("_count"), matches("days_per")),
        id_cols     = c(school_match_name, data_schoolyear),
        names_glue  = "{.value}.{by_var}.{by_group}"
      )
    }) |> 
  
  
  # combine
  reduce(
    full_join, 
    by = c("school_match_name", "data_schoolyear")
    ) |> 
  rename_with(~ gsub(".overall.overall", "", .x)) |> 
  
  mutate(
    across(matches("count"), ~ coalesce(.x, 0))
    ) |> 
  
  mutate(
    across(
      c(matches("_count$"), matches("student_count\\.")),
      ~ .x / student_count, 
      .names = "{gsub('count', 'pct', .col)}"
      )
    ) |> 
  
  # add other school chars
  full_join(
    df_mod |> 
      distinct(
        school_match_name, short_name, data_schoolyear, 
        days_per_student_expected, re_majority
        ),
    by = c("school_match_name", "data_schoolyear")
  ) |> 

  box_write_if_diff(
    f_name  = "cps_school_level_student_counts.csv",
    comment = paste(
      "School- and schoolyear-level student counts, 2021-2022 to 2024-2025.",
      "Based on CPS extract file ", params$raw_data_cps_counts$filename,
      ", sheet", params$raw_data_cps_counts$sheet, ". ",
      "Includes counts of students overall and with asthma, allergy,",
      "support plans, and select census variables; race/ethnicicity majority",
      "category; and counts",
      "of attendance days (present, absent) per school and schoolyear,",
      "overall; stratified by race/ethnicity; stratified by gender; and ",
      "stratified by asthma status."
  ))






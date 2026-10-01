# 2026-09-24
# Process new CPS data extract (rec'd Sept 2026)

# load raw data from file
df_raw <- boxr::box_read(
  file_id = params$raw_data_cps_counts$box_id,
  sheet   = params$raw_data_cps_counts$sheet
  ) 

# tidy variables
df_xtabs <- df_raw |> 
  janitor::clean_names() |>
  mutate(
    # categorical variables
    school_match_name = standardize_school_names(student_current_school),
    data_schoolyear   = enrollment_year,
    # short_name        = str_trim(gsub("0 Pilot", "", student_current_school)),
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
    ),
    
    # count variables
    student_count = asthma_count,
    asthma_count  = student_count*(asthma_status == "Asthma Dx"),
    
    male_count = student_count*(student_gender == "Male"),
    female_count = student_count*(student_gender == "Female"),
    
    # attendance variables
    days_per_student_expected = 176,
    days_per_row_expected = days_per_student_expected*student_count,
    days_per_row_present  = (average_attendance/100)*days_per_row_expected,
    days_per_row_absent   = (1 - (average_attendance/100))*days_per_row_expected,
    
    .keep   = "unused"
  ) |> 
  
  select(-any_of(c("district_name"))) |> 
  mutate(across(where(is.character), ~ factor(.x))) |> 
  relocate(where(negate(is.numeric)), .before = 1) |> 
  
  # write to file
  box_write_if_diff(
    f_name  = "cps_student_crosstabs.csv",
    comment = paste(
      "School- and schoolyear-specific student counts, 2021-2022 to 2025-2026.",
      "Based on CPS extract file ", params$raw_data_cps_counts$filename,
      ", sheet", params$raw_data_cps_counts$sheet, ". ",
      "Includes counts of students with asthma, allergy,",
      "support plans, and select census variables by ",
      "race/ethnicity, gender, and asthma status. Includes ",
      "attendance days (present, absent) by ",
      "race/ethnicity, gender, and asthma status."
    )
  )


# summarise at the school + schoolyear level
df_schools <- full_join(
  df_xtabs |> 
    summarise(
      across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
      .by = c("data_schoolyear", "school_match_name", "days_per_student_expected")
    ) |> 
    mutate(
      across(
        ends_with("_count"), 
        ~ (.x / student_count)*100,
        .names = "{gsub('count', 'pct', .col)}"
      ),
      
      male_female_ratio = (male_count / female_count)
    ),
  
  # calculate r/e majority 
  df_xtabs |> 
    summarise(
      student_count = sum(student_count, na.rm = TRUE),
      .by           = c(school_match_name, data_schoolyear, student_race),
    ) |> 
    mutate(
      re_majority = student_count / sum(student_count, na.rm = TRUE),
      .by         = c(school_match_name, data_schoolyear)
    ) |> 
    filter(
      student_count == max(student_count, na.rm = TRUE), 
      .by = c(school_match_name, data_schoolyear)
    ) |> 
    mutate(
      re_majority = fct_infreq(case_when(
        re_majority >= 0.5 ~ student_race,
        .default = "No Majority"
      ))
    ) |> 
    distinct(school_match_name, data_schoolyear, re_majority),
  by = c("school_match_name", "data_schoolyear")
  ) |> 
  relocate(where(negate(is.numeric))) |> 
  
  # write to file
  box_write_if_diff(
  f_name  = "cps_school_level_counts.csv",
  comment = paste(
    "School-level summaries of schoolyear-specific student",
    "characteristics, 2021-2022 to 2025-2026.",
    "Based on CPS extract file ", params$raw_data_cps_counts$filename,
    ", sheet", params$raw_data_cps_counts$sheet, ". ",
    "Includes one-way school-level counts of students with asthma, allergy,",
    "support plans, select census variables, and attendance",
    "days (present, absent). Includes one-way school-level counts of students",
    "by gender; majority race/ethnicity category; and gender ratio."
)
)





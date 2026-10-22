# 2026-10-13
# Process new CPS data extract (rec'd Sept 2026)

# load raw data from file
cps_raw <- openxlsx::read.xlsx(
  nested_here("data-raw", raw_files$cps_counts$filename),
  sheet = raw_files$cps_counts$sheet
  )

# tidy variables
cps_xtabs <- cps_raw |> 
  janitor::clean_names() |>
  mutate(
    # categorical variables
    school_match_name = standardize_school_names(student_current_school),
    data_schoolyear   = enrollment_year,
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
  write_if_diff(
    f_name  = nested_here("data", "cps_student_crosstabs.csv"),
    comment = c(
      "School- and schoolyear-specific student counts, 2021-2022 to 2025-2026.",
      paste0("Based on CPS extract file ", raw_files$cps_counts$filename,
      ", sheet", raw_files$cps_counts$sheet, ". "),
      "Includes counts of students with asthma, allergy, support plans, and ",
      "select census variables by race/ethnicity, gender, and asthma status.",
      "Includes attendance days (present, absent) by race/ethnicity, gender,",
      "and asthma status."
    )
  )

# subgroup counts, attendance
cps_demog_wide <- list(
  asthma = "asthma_status",
  gender = "student_gender",
  race   = "student_race"
  ) |> 
  imap(~{
    cps_xtabs |> 
      rename_with(~ "level", .cols = all_of(c(.x))) |> 
      summarise(
        xname                 = .y,
        count                 = sum(student_count, na.rm = TRUE),
        days_per_row_expected = sum(days_per_row_expected, na.rm = TRUE),
        days_per_row_present  = sum(days_per_row_present, na.rm = TRUE),
        days_per_student_present = days_per_row_present / count,
        average_attendance    = (days_per_row_present / days_per_row_expected)*100,
        
        days_per_row_absent   = sum(days_per_row_absent, na.rm = TRUE),
        days_per_student_absent = days_per_row_absent / count,
        .by                   = c(school_match_name, data_schoolyear, level)
      ) |> 
      mutate(
        pct = count / sum(count),
        .by = c(school_match_name, data_schoolyear)
      ) |> 
      
      # rename attendance variables for clarity
      rename_with(~ gsub("days_per_row", "days_per_school", .x)) |> 
      
      pivot_wider(
        names_from  = c(xname, level),
        values_from = where(is.numeric),
        id_cols     = c(school_match_name, data_schoolyear),
        names_glue  = "{xname}__{level}__{.value}",
        values_fill = 0
      ) |>
      janitor::clean_names()
  }) |> 
  reduce(full_join, by = c("school_match_name", "data_schoolyear")) |> 
  
  # rename for clarity
  mutate(
    asthma_count = asthma_asthma_dx_count,
    asthma_pct   = asthma_asthma_dx_pct
    )


# summarise at the school + schoolyear level
cps_schoolwide <- list(
  
  # schoolwide counts
  cps_xtabs |>
    summarise(
      across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
      .by = c("data_schoolyear", "school_match_name", 
              "days_per_student_expected")
      ) |> 
    # rename attendance variables for clarity
    rename_with(~ gsub("days_per_row", "days_per_school", .x)) |> 
    
    mutate(
      across(
        ends_with("_count"), 
        ~ (.x / student_count)*100, 
        .names = "{gsub('count', 'pct', .col)}"
        ),
      
      days_per_student_present = days_per_school_present / student_count,
      days_per_student_absent  = days_per_school_absent / student_count,
      average_attendance       = (days_per_school_present / days_per_school_expected)*100,
      
      .by = c("data_schoolyear", "school_match_name")
      ),
  
  # subgroup counts
  cps_demog_wide,
  
  # r/e majority 
  cps_xtabs |> 
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
    distinct(school_match_name, data_schoolyear, re_majority)
  ) |> 
  
  reduce(
    full_join,
    by = c("school_match_name", "data_schoolyear")
    ) |> 
  
  relocate(where(negate(is.numeric))) |> 
  
  # write to file
  write_if_diff(
    f_name  = nested_here("data", "cps_school_level_counts.csv"),
    comment = c(
      "School-level summaries of schoolyear-specific student",
      "characteristics, 2021-2022 to 2025-2026.",
      "Based on CPS extract file ", raw_files$cps_counts$filename,
      ", sheet", raw_files$cps_counts$sheet, ". ",
      "Includes one-way school-level counts of students with asthma, allergy,",
      "support plans, select census variables, and attendance",
      "days (present, absent). Includes one-way school-level counts of students",
      "by gender; majority race/ethnicity category; and gender ratio."
  )
)


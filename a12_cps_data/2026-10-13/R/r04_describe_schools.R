library(arsenal)
tableby(
  data_schoolyear ~ 
    notest(n_schools, cat.stats = "count", cat.simplify = TRUE) 
  + grade_level 
  + region 
  + rpl_ser 
  
  + student_count
  
  + re_majority 
  + race_american_indian_pct
  + race_asian_hawaiian_or_pacific_islander_pct
  + race_black_non_hispanic_pct
  + race_hispanic_pct
  + race_multi_pct
  + race_white_non_hispanic_pct
  + race_middle_eastern_or_north_african_pct
  + race_missing_pct

  + gender_female_pct
  + gender_male_pct
  + gender_non_binary_pct
  + gender_missing_pct
  
  
  + asthma_pct 
  + food_allergy_pct 
  + non_food_allergy_pct 
  + student_504_pct 
  + iep_pct 
  + esl_pct 
  + frm_pct 
  + homelessness_pct
  
  + notest(days_per_student_expected, numeric.stats = "mean", 
           numeric.simplify = TRUE, digits = 0)
  
  + days_per_student_present
  + days_per_student_absent
  + average_attendance
  
  + asthma_asthma_dx_days_per_student_present
  + asthma_asthma_dx_days_per_student_absent
  + asthma_asthma_dx_average_attendance
  
  + asthma_no_asthma_dx_days_per_student_present
  + asthma_no_asthma_dx_days_per_student_absent
  + asthma_no_asthma_dx_average_attendance
  ,
  data    = read_rds(nested_here("data", "cps_schools_analytic.rds")),
  control = tableby.control(
    test           = FALSE,
    total          = FALSE,
    digits.n	     = NA,
    digits        = 4,
    numeric.stats  = c("Nmiss", "medianrange", "q1q3",  "meansd")
    )
  ) |> 
  summary(text = TRUE) |> 
  as.data.frame() |>
  write_rds(nested_here("output", "describe_schools.rds"))

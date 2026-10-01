tableby(
  data_schoolyear ~ 
    notest(n_schools, cat.stats = "count", cat.simplify = TRUE) 
  + grade_level 
  + region_name 
  + student_count 
  
  + re_majority 
  + student_pct.race.american_indian
  + student_pct.race.asian_hawaiian_or_pacific_islander
  + student_pct.race.black_non_hispanic
  + student_pct.race.hispanic
  + student_pct.race.multi
  + student_pct.race.white_non_hispanic
  + student_pct.race.middle_eastern_or_north_african
  + student_pct.race.missing
  
  + student_pct.gender.female
  + student_pct.gender.male
  + student_pct.gender.non_binary
  + student_pct.gender.missing
  
  + rpl_ser 
  
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
  + pct_days_per_row_present
  
  + days_per_student_present.asthma.asthma_dx
  + days_per_student_absent.asthma.asthma_dx
  + pct_days_per_row_present.asthma.asthma_dx
  ,
  data    = schools |> mutate(across(matches("pct"), ~ .x*100)),
  control = tableby.control(
    test           = FALSE,
    total          = FALSE,
    digits.n	     = NA,
    digits        = 2,
    numeric.stats  = c("Nmiss", "medianrange", "q1q3",  "meansd")
    )
  ) |> 
  summary(text = TRUE) |> 
  as.data.frame() |>
  write_rds(nested_here("output", "describe_schools.rds"))
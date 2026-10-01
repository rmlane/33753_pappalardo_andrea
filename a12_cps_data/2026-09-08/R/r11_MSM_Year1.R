# Develop poisson models for outcome (days attended)
attd_mod_df <- schools |> 
  relocate(
    school_id, data_schoolyear,
    student_count, impl_days, n_events, 
    matches("days_per_row")
  ) |> 
  select(-matches("\\."), student_pct.gender.male) |> 
  filter(impl_schoolyear == data_schoolyear) |>
  arrange(data_schoolyear, school_id) |> 
  mutate(
    across(all_of(c("data_schoolyear", "school_id")), fct_inorder),
    across(matches("days_per"), round),
    impl_student_days = impl_days*student_count,
    event_rate        = (n_events / impl_student_days)*(1000*176) # events per 1000 students per year in program
    ) |>
  
  left_join(
    read_rds(nested_here("data", "iptw_weights_year1.rds")) |> 
      select(school_id, iptw_stab, iptw_stab_trunc),
    by = c("school_id")
  )


#############################################################################

# Marginal
attd_mod_marg <- glm(
  days_per_row_present ~ offset(log(days_per_row_expected)),
  family  = poisson(link = "log"),
  data    = attd_mod_df,
  weights = iptw_stab_trunc
)

#############################################################################

# Denominator: conditional

library(splines)
default::default(ns) <- list(df = 2, intercept = FALSE)
attd_mod_cond_max <- glm(
  days_per_row_present ~ offset(log(days_per_row_expected)) +
    data_schoolyear
  + poly(event_rate, 3)
  + ns(student_count)
  + ns(impl_days)
  + re_majority 
  + grade_level 
  + ns(student_pct.gender.male)
  + ns(rpl_ser_imp)
  + ns(asthma_pct)
  + ns(student_504_pct)
  
  # (possible to drop)
  + ns(iep_pct)
  + ns(food_allergy_pct) 
  + ns(esl_pct)
  + ns(frm_pct)
  + ns(homelessness_pct),
  
  family  = poisson(link = "log"),
  data    = attd_mod_df,
  weights = iptw_stab_trunc
)

summary(attd_mod_cond_max)

drops <- map(1:5, ~{
  combn(c(
    "ns(iep_pct)",
    "ns(food_allergy_pct) ",
    "ns(esl_pct)",
    "ns(frm_pct)",
    "ns(homelessness_pct)"
  ), .x, simplify = FALSE) |> 
    map(unlist) |> 
    map(str_trim) |> 
    map(paste, collapse = " + ")
  }) |> 
  unlist() |> 
  str_trim() |> 
  set_names()

drop_mods <- map(glue::glue(". ~ . - ({drops})"), as.formula) |> 
  map(~{update(attd_mod_cond_max, .x)})
names(drop_mods) <- drops
write_rds(drop_mods, nested_here("output", "attd_drop_mod.rds"))

drop_aic <- c(list("--" = attd_mod_cond_max), drop_mods) |> 
  map_dfr(broom::glance, .id = "model")
write_rds(drop_aic, nested_here("output", "attd_drop_mod_aicyear1.rds"))

drop_aic |> 
  pivot_longer(c(AIC, BIC)) |> 
  arrange(value, .by = name)  |> 
  filter(value - min(value) <= 2, .by = name) |> 
  pull(model)

attd_mod_best <- attd_mod_cond_max
write_rds(attd_mod_best, nested_here("output", "attd_mod_condyear1.rds"))


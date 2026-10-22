# Develop poisson models for outcome (days attended)
attd_mod_df <- schools |> 
  relocate(
    school_id, data_schoolyear,
    student_count, impl_days, n_events, 
    matches("days_per_row")
  ) |> 
  select(-matches("\\."), student_pct.gender.male) |> 
  filter(impl_days > 0) |>
  arrange(data_schoolyear, school_id) |> 
  mutate(
    across(all_of(c("data_schoolyear", "school_id")), fct_inorder),
    across(matches("days_per"), round),
    impl_student_days = impl_days*student_count,
    event_rate        = (n_events / impl_student_days)*(1000*176) # events per 1000 students per year in program
    ) |>
  
  left_join(
    read_rds(nested_here("data", "iptw_weights_xsec.rds")) |> 
      select(school_id, data_schoolyear, iptw_stab_trunc),
    by = c("school_id")
  )


#############################################################################

library(geepack)
# Marginal
attd_mod_df <- geeglm(
  days_per_row_present ~ data_schoolyear + offset(log(days_per_row_expected)),
  id      = school_id,
  family  = poisson(link = "log"),
  data    = attd_mod_df,
  waves   = data_schoolyear,
  corstr  = "exchangeable",
  weights = iptw_stab_trunc
)

#############################################################################

# Denominator: conditional

library(splines)
default::default(ns) <- list(df = 2, intercept = FALSE)
attd_mod_max <- geeglm(
  days_per_row_present ~ data_schoolyear + offset(log(days_per_row_expected))
  + poly(event_rate, 3)
  + ns(student_count)
  + ns(impl_days)
  + re_majority 
  + grade_level 
  + impl_schoolyear
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
  
  id      = school_id,
  family  = poisson(link = "log"),
  data    = attd_mod_df,
  waves   = data_schoolyear,
  corstr  = "exchangeable",
  weights = iptw_stab_trunc
)

summary(attd_mod_max)

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
  map(~{update(attd_mod_max, .x)})
names(drop_mods) <- drops
write_rds(drop_mods, nested_here("output", "attd_drop_mod.rds"))

drop_qic <- c(list("--" = attd_mod_max), drop_mods) |> 
  map_dfr(~{QIC(.x) |> unlist() |> as.list() |> data.frame()}, .id = "model")
write_rds(drop_qic, nested_here("output", "attd_drop_mod_qic.rds"))

drop_qic |> 
  pivot_longer(-c("model", "params")) |> 
  filter(value == min(value), .by = name) |> 
  pull(model)  

attd_mod_best <- attd_mod_max
write_rds(attd_mod_best, nested_here("output", "attd_mod_cond.rds"))


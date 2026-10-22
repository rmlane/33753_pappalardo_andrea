# Develop poisson models for exposure (utilization count)

# Reported inhaler uses per student-days of program enrollment
util_mod_df <- schools |> 
  relocate(
    school_id, data_schoolyear,
    student_count, impl_days, n_events
  ) |> 
  select(-matches("\\."), student_pct.gender.male) |> 
  filter(data_schoolyear == impl_schoolyear) |> 
  mutate(
    impl_student_days = impl_days*student_count, 
    .after            = 2
    ) |> 
  arrange(data_schoolyear, school_id) |> 
  mutate(across(all_of(c("data_schoolyear", "school_id")), fct_inorder)) |> 
  droplevels()

#############################################################################

# model events per student per 1 year

pois_mod_marg <- glm(
  n_events ~ offset(log(impl_student_days)),
  data   = util_mod_df,
  family = poisson(link = "log")
)
write_rds(pois_mod_marg, nested_here("output", "iptw_mod_marg_year1.rds"))

pois_mod_cond <- glm(
  n_events ~ 
  + re_majority 
  + grade_level 
  + data_schoolyear
  + ns(impl_days) 
  + ns(student_count)
  + ns(student_pct.gender.male)
  + ns(rpl_ser_imp)
  + ns(asthma_pct)
  + ns(student_504_pct)
  
  # (possible to drop)
  + ns(iep_pct)
  + ns(food_allergy_pct) 
  + ns(esl_pct)
  + ns(frm_pct)
  + ns(homelessness_pct)
  
  + offset(log(impl_student_days)),
  
  data   = util_mod_df,
  family = poisson(link = "log")
)

summary(pois_mod_cond)

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
  map(~{update(pois_mod_cond, .x)})
names(drop_mods) <- drops
write_rds(drop_mods, nested_here("output", "iptw_drop_mods_year1.rds"))

(drop_aic <- c(list("--" = pois_mod_cond), drop_mods) |> 
  map_dfr(broom::glance, .id = "model"))
write_rds(drop_aic, nested_here("output", "iptw_drop_mod_aic_year1.rds"))

drop_aic |> 
  pivot_longer(c(AIC, BIC)) |> 
  arrange(value, .by = name)  |> 
  filter(value - min(value) <= 2, .by = name) |> 
  pull(model)

iptw_denom_best <- drop_mods[["ns(iep_pct) + ns(food_allergy_pct) + ns(esl_pct)"]]

write_rds(iptw_denom_best, nested_here("output", "iptw_mod_cond_year1.rds"))

#############################################################################


util_mod_df$pred_numer <- predict(
  read_rds(nested_here("output", "iptw_mod_marg_year1.rds")),
  type = "response"
  )

util_mod_df$iptw_numer <- unlist(map2(
  util_mod_df$n_events,
  util_mod_df$pred_numer,
  dpois
))


util_mod_df$pred_denom <- predict(
  read_rds(nested_here("output", "iptw_mod_cond_year1.rds")),
  type = "response"
)

util_mod_df$iptw_denom <- unlist(map2(
  util_mod_df$n_events,
  util_mod_df$pred_denom,
  dpois
))

util_mod_df$iptw_stab <- util_mod_df$iptw_numer / util_mod_df$iptw_denom

util_mod_df |> 
  dplyr::select(school_id, data_schoolyear, n_events, matches("pred_"), matches("iptw")) |> 
  mutate(
    iptw_stab_01 = quantile(iptw_stab, 0.01),
    iptw_stab_99 = quantile(iptw_stab, 0.99),
    iptw_stab_trunc = case_when(
      iptw_stab < iptw_stab_01 ~ iptw_stab_01,
      iptw_stab > iptw_stab_99 ~ iptw_stab_99,
      .default = iptw_stab
      )
  ) |> 
  write_rds(nested_here("data", "iptw_weights_year1.rds"))

# Develop poisson models for exposure (utilization count)

# Reported inhaler uses per student-days of program enrollment
util_mod_df <- schools |> 
  relocate(
    school_id, data_schoolyear,
    student_count, impl_days, n_events
  ) |> 
  select(-matches("\\."), student_pct.gender.male) |> 
  filter(impl_days > 0) |>
  mutate(
    impl_student_days = impl_days*student_count, 
    .after            = 2
    ) |> 
  arrange(data_schoolyear, school_id) |> 
  mutate(across(all_of(c("data_schoolyear", "school_id")), fct_inorder)) 


# add covariate values from previous schoolyear
util_mod_df <- left_join(
  util_mod_df,
  schools |> 
    select(any_of(names(util_mod_df))) |> 
    mutate(
      next_schoolyear = fct_recode(
        data_schoolyear,
        "2022-2023" = "2021-2022",
        "2023-2024" = "2022-2023",
        "2024-2025" = "2023-2024",
        "2025-2026" = "2024-2025"
      ),
      .keep = "unused",
      .after = 1
    ),
  by     = c("school_id", "data_schoolyear" = "next_schoolyear"),
  suffix = c("", ".lastyear")
  ) |> 
  droplevels()


# Numerator: marginal
util_mod_numer <- geepack::geeglm(
  n_events ~ data_schoolyear + offset(log(impl_student_days)),
  family = poisson(link = "log"),
  data   = util_mod_df,
  id     = school_id,
  waves  = data_schoolyear,
  corstr = "exchangeable"
)

util_mod_df$pred_numer <- predict(
  util_mod_numer,
  type = "response"
  )

util_mod_df$iptw_numer <- unlist(map2(
  util_mod_df$n_events,
  util_mod_df$pred_numer,
  dpois
  ))

write_rds(util_mod_numer, nested_here("output", "iptw_mod_marg.rds"))

#############################################################################

# Denominator: conditional

library(splines)
default::default(ns) <- list(df = 2, intercept = FALSE)
iptw_denom_max <- geepack::geeglm(
  # n_events ~ data_schoolyear + offset(log(impl_student_days)),
  n_events ~ 
    data_schoolyear
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
  + ns(homelessness_pct)
  
  + offset(log(impl_student_days)),
  
  family = poisson(link = "log"),
  data   = util_mod_df,
  id     = school_id,
  waves  = data_schoolyear,
  corstr = "exchangeable"
)

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
  map(~{update(iptw_denom_max, .x)})
names(drop_mods) <- drops
write_rds(drop_mods, nested_here("output", "iptw_drop_mods.rds"))

drop_qic <- c(list("--" = iptw_denom_max), drop_mods) |> 
  map_dfr(geepack::QIC, .id = "dropped")
write_rds(drop_qic, nested_here("output", "iptw_drop_mod_qic.rds"))

drop_qic |> 
  pivot_longer(-c("dropped", "params")) |> 
  filter(value == min(value), .by = name)

iptw_denom_best <- drop_mods[["ns(iep_pct) + ns(food_allergy_pct) + ns(esl_pct)"]]

write_rds(iptw_denom_best, nested_here("output", "iptw_mod_cond.rds"))

#############################################################################


util_mod_df$pred_numer <- predict(
  read_rds(nested_here("output", "iptw_mod_marg.rds")),
  type = "response"
  )

util_mod_df$iptw_numer <- unlist(map2(
  util_mod_df$n_events,
  util_mod_df$pred_numer,
  dpois
))


util_mod_df$pred_denom <- predict(
  read_rds(nested_here("output", "iptw_mod_cond.rds")),
  type = "response"
)

util_mod_df$iptw_denom <- unlist(map2(
  util_mod_df$n_events,
  util_mod_df$pred_denom,
  dpois
))

util_mod_df$iptw_stab <- util_mod_df$iptw_numer / util_mod_df$iptw_denom

util_mod_df |> 
  select(school_id, data_schoolyear, n_events, matches("pred_"), matches("iptw")) |> 
  mutate(
    iptw_stab_01 = quantile(iptw_stab, 0.01),
    iptw_stab_99 = quantile(iptw_stab, 0.99),
    iptw_stab_trunc = case_when(
      iptw_stab < iptw_stab_01 ~ iptw_stab_01,
      iptw_stab > iptw_stab_99 ~ iptw_stab_99,
      .default = iptw_stab
      )
  ) |> 
  write_rds(nested_here("data", "iptw_weights.rds"))

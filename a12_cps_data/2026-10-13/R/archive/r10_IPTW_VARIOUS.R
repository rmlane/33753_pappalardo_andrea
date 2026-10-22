
# Setup -------------------------------------------------------------------

# load libraries
library(ipw)


# Prepare data for utilization analysis
util_mod_df <- full_join(
  
  # select time-varying chars
  schools |> 
    filter(!(data_schoolyear == "2021-2022")) |> 
    mutate(
      # event_rate        = (n_events / (student_count))*1000, # events per 1000 students per year
      # impl_student_days = impl_days*student_count, 
      # impl_event_rate   = (n_events / impl_student_days)*1000,
      male_pct          = student_pct.gender.male,
      impl_exposure = case_when(
        data_schoolyear == impl_schoolyear ~ 
          impl_days / days_per_student_expected,
        .default = impl_binary
        ),
      impl_years = floor(
        impl_days / (days_per_student_expected + 1)
        )
      
      ) |> 
    select(!matches("\\.")) |> 
    select(
      school_id,
      matches("day"),
      matches("data"),
      matches("impl"),
      matches("event"),
      student_count, asthma_count, asthma_pct, matches("pct")
      ) |>
    rename_with(~ paste0(.x, ".tv"), -c("school_id", "data_schoolyear")),
  
  # add baseline chars
  schools |> 
    arrange(school_id, data_schoolyear) |> 
    slice(1, .by = school_id) |> 
    rename(male_pct = student_pct.gender.male) |> 
    select(!matches("\\.")) |> 
    select(
      school_id, grade_level, region_name, 
      student_count, re_majority, wave, rpl_ser,
      matches("pct")
      ) |> 
    rename_with(~ paste0(.x, ".t0"), -c("school_id")) |> 
    mutate(rpl_ser.t0 = coalesce(rpl_ser.t0, 0.5)),
  
  by     = "school_id"
  ) |> 
  
  # add select yoy changes
  full_join(
    schools |> 
      mutate(across(
        all_of(c(
          "pct_days_per_row_present",
          "pct_days_per_row_absent",
          "asthma_pct",
          "food_allergy_pct",
          "non_food_allergy_pct",
          "student_504_pct",
          "iep_pct",
          "esl_pct",
          "frm_pct",
          "homelessness_pct"
          )),
        list(
          `2022-2023` = ~ case_when(
            data_schoolyear == "2021-2022" ~ (-1)*(.x),
            data_schoolyear == "2022-2023" ~ ( 1)*(.x),
            .default = 0
            ),
          `2023-2024` = ~ case_when(
            data_schoolyear == "2022-2023" ~ (-1)*(.x),
            data_schoolyear == "2023-2024" ~ ( 1)*(.x),
            .default = 0
            ),
          `2024-2025` = ~ case_when(
            data_schoolyear == "2023-2024" ~ (-1)*(.x),
            data_schoolyear == "2024-2025" ~ ( 1)*(.x),
            .default = 0
            )
          ),
        .names = "{.col}_abs_change.{.fn}"
        )) |> 
      summarise(
        across(matches("abs_change"), ~ sum(.x)),
        .by = c(school_id)
      ) |> 
      pivot_longer(-school_id) |> 
      separate(name, c("name", "data_schoolyear"), sep = "\\.") |> 
      pivot_wider(),
  
  by = c("school_id", "data_schoolyear")
)




# Exposure Allocation Models ----------------------------------------------

# point treatment: implementation order
baseline_mod_df <- util_mod_df |> 
  dplyr::select(school_id, matches("t0")) |> 
  distinct()

# weights for treatment:

# P[T = 1|Z]/P[T = 1|Z, X0]
# T  = treatment 
# Z  = baseline moderators, covariates
# X0 = baseline/pre-treatment variables; potential confounders that may influence the probabilities of treatment assignment

# ordinal point treatment
# T  = implementation order (4 levels, ordinal)
# Z  = t0 characteristics unrelated to change in attendance
# X0 = t0 related to change in attendance
w_t_wave <- ipwpoint(
  exposure    = wave.t0,
  family      = "ordinal",
  link        = "logit",
  numerator   = ~ 1,
  denominator = ~ asthma_pct.t0 +
                  grade_level.t0 + 
                  student_count.t0 + 
                  re_majority.t0 + 
                  rpl_ser.t0 + 
                  student_count.t0 + 
                  pct_days_per_row_present.t0 + 
                  food_allergy_pct.t0 + 
                  non_food_allergy_pct.t0 + 
                  student_504_pct.t0 + 
                  iep_pct.t0 + 
                  esl_pct.t0 + 
                  frm_pct.t0 + 
                  homelessness_pct.t0 + 
                  male_pct.t0,
  data     = baseline_mod_df,
  trunc       = 0.01
)

summary(w_t_wave$ipw.weights)
summary(w_t_wave$weights.trunc)








# ordinal, point treatment
# T  = implementation wave (4 levels)
# Z  = t0 characteristics
# X0 = t0 asthma
w_t_baseline_ord <- ipwpoint(
  exposure    = wave.t0, 
  family      = "ordinal",
  link        = "logit",
  numerator   = ~ #asthma_pct.t0 +
    grade_level.t0 + 
    student_count.t0 + 
    re_majority.t0 + 
    rpl_ser.t0 + 
    student_count.t0 + 
    pct_days_per_row_present.t0 + 
    food_allergy_pct.t0 + 
    non_food_allergy_pct.t0 + 
    student_504_pct.t0 + 
    iep_pct.t0 + 
    esl_pct.t0 + 
    frm_pct.t0 + 
    homelessness_pct.t0 + 
    male_pct.t0,
  denominator = ~ asthma_pct.t0 +
    grade_level.t0 + 
    student_count.t0 + 
    re_majority.t0 + 
    rpl_ser.t0 + 
    student_count.t0 + 
    pct_days_per_row_present.t0 + 
    food_allergy_pct.t0 + 
    non_food_allergy_pct.t0 + 
    student_504_pct.t0 + 
    iep_pct.t0 + 
    esl_pct.t0 + 
    frm_pct.t0 + 
    homelessness_pct.t0 + 
    male_pct.t0,
  data     = baseline_mod_df,
  trunc       = 0.01
)

summary(w_t_baseline_ord$ipw.weights)
summary(w_t_baseline_ord$weights.trunc)
summary(w_t_baseline_ord$num.mod)
summary(w_t_baseline_ord$den.mod)

MASS::polr(wave.t0 ~ asthma_pct.t0 +
             grade_level.t0 + 
             student_count.t0 + 
             re_majority.t0 + 
             rpl_ser.t0 + 
             student_count.t0 + 
             pct_days_per_row_present.t0 + 
             food_allergy_pct.t0 + 
             non_food_allergy_pct.t0 + 
             student_504_pct.t0 + 
             iep_pct.t0 + 
             esl_pct.t0 + 
             frm_pct.t0 + 
             homelessness_pct.t0 + 
             male_pct.t0,
           data = baseline_mod_df)

# weights for mediator: 

# If M is continuous, then the weights are given by a ratio of the probabilities from the p.d.f.s, ϕ(M|T, Z)/ϕ(M|T, Z, X0, X1).

# M = mediator (T causes a change in M; M causes a change in Y)
# X1 = post-treatment confounder; a confounder of M and Y that has itself been influenced by T

# "In all situations, the weights for M include T in both the numerator and denominator models for the weights."

# continuous mediator treatment
# T  = implementation status (impl_binary)
# Z  = t0 characteristics
# X0 = t0 asthma
# X1 = t1 modifiable chars
# M  = t1 asthma

w_m_xsec <- ipwpoint(
  # exposure    = asthma_pct.tv, 
  exposure    = asthma_pct_abs_change, 
  family      = "gaussian",
  numerator   = ~ impl_binary.tv +
    # asthma_pct.t0 +
    grade_level.t0 + 
    student_count.t0 + 
    re_majority.t0 + 
    rpl_ser.t0 + 
    student_count.t0 + 
    food_allergy_pct.t0 + 
    non_food_allergy_pct.t0 + 
    student_504_pct.t0 + 
    iep_pct.t0 + 
    esl_pct.t0 + 
    frm_pct.t0 + 
    homelessness_pct.t0 + 
    male_pct.t0,
  denominator = ~ impl_binary.tv +
    asthma_pct.t0 +
    grade_level.t0 + 
    student_count.t0 + 
    re_majority.t0 + 
    rpl_ser.t0 + 
    student_count.t0 +
    food_allergy_pct.t0 + 
    non_food_allergy_pct.t0 + 
    student_504_pct.t0 + 
    iep_pct.t0 + 
    esl_pct.t0 + 
    frm_pct.t0 + 
    homelessness_pct.t0 + 
    male_pct.t0
  +
    
    food_allergy_pct_abs_change + 
    non_food_allergy_pct_abs_change + 
    student_504_pct_abs_change + 
    iep_pct_abs_change + 
    esl_pct_abs_change + 
    frm_pct_abs_change + 
    homelessness_pct_abs_change,
  data     = xsec_mod_df,
  trunc       = 0.01
)

summary(w_m_xsec$ipw.weights)
summary(w_m_xsec$weights.trunc)
summary(w_m_xsec$num.mod)
summary(w_m_xsec$den.mod)





# MSM for the causal effect of T on M. Incorporates weights for T.

# E[M|T=t] = B0M + B1(t)

# Causal effect of T on M = B1


# MSM for the causal effect of T on Y. Weights are the product of the weights for T and M

# E[Y|M=m, T=t] = B0Y + B2(m) + B3(t) + B4(tm)






# see van der Wal, W. M., & Geskus, R. B. (2011). ipw: An R Package for Inverse Probability Weighting. Journal of Statistical Software, 43(13), 1–23. https://doi.org/10.18637/jss.v043.i13


# "In a point treatment situation we can adjust for a set of confounders C
# when estimating the effect of discrete exposure A
# by weighting observations i by the inverse probability weights"

# "weighting by wi creates a pseudopopulation in which C no longer predicts A"

# "To increase the association between
# the numerator and denominator, further stabilizing the weights, one can condition both in the
# numerator and denominator of (3) on a set of time-fixed covariates V that are related to A.
# For instance, when a researcher believes that sex does not influence the outcome of interest,
# but the distribution of exposure level varies between both sexes, sex could be included in V .
# It must be noted that confounding caused by baseline covariates that are used as stabilization
# factors, is not adjusted for (Cole and Hernan 2008). Adjustment for such covariates could be
# # made by including them in the MSM, at the cost of possibly inducing non-collapsibility."

# Exposure: Utilization during current year (binary, monotonic)

w_t_tv <- ipwtm(
  exposure = any_events.tv, # at least one rescue inhaler use in current year
  # exposure    = event_rate, # rescue inhaler uses per 1000 students per year
  family      = "ordinal",
  link        = "logit",
  numerator   = ~ asthma_pct.t0 +
    grade_level.t0 + 
    student_count.t0 + 
    re_majority.t0 + 
    rpl_ser.t0 + 
    student_count.t0 + 
    pct_days_per_row_present.t0 + 
    food_allergy_pct.t0 + 
    non_food_allergy_pct.t0 + 
    student_504_pct.t0 + 
    iep_pct.t0 + 
    esl_pct.t0 + 
    frm_pct.t0 + 
    homelessness_pct.t0 + 
    male_pct.t0,
  denominator = ~ asthma_pct.t0 +
    grade_level.t0 + 
    student_count.t0 + 
    re_majority.t0 + 
    rpl_ser.t0 + 
    student_count.t0 + 
    pct_days_per_row_present.t0 + 
    food_allergy_pct.t0 + 
    non_food_allergy_pct.t0 + 
    student_504_pct.t0 + 
    iep_pct.t0 + 
    esl_pct.t0 + 
    frm_pct.t0 + 
    homelessness_pct.t0 + 
    male_pct.t0 +
    
    asthma_pct_abs_change +
    food_allergy_pct_abs_change + 
    non_food_allergy_pct_abs_change + 
    student_504_pct_abs_change + 
    iep_pct_abs_change + 
    esl_pct_abs_change + 
    frm_pct_abs_change + 
    homelessness_pct_abs_change,
  id          = school_id,
  timevar     = as.numeric(data_schoolyear),
  type        = "first", # first increase above 0 = crossover event
  data        = util_mod_df,
  trunc       = 0.01
  )

summary(w_t_tv$ipw.weights)
summary(w_t_tv$weights.trunc)
summary(w_t_tv$den.mod)





# Time-varying exposure

# The factors in the numerator of (5) contain the probability of the 
# observed exposure status at each time point, aik, given 
# * the observed exposure history up to the previous timepoint, aik-1
# * the observed time-fixed covariates, vi

# The factors in the denominator of (5) contain the probability of the 
# observed exposure status at each time point, given 
# * the observed exposure history up to the previous time point, 
# the observed history of time-varying confounders up to each time point, cik
# the observed time-fixed covariates





















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

#############################################################################

library(glmtoolbox)
# library(MASS)





theta_marg <- glm.nb(n_events ~ data_schoolyear + offset(log(impl_student_days)),
       data   = util_mod_df) |> pluck("theta")
theta_cond <- glm.nb(
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
  
  data   = util_mod_df
) |> pluck("theta")


# Numerator: marginal
util_mod_numer <- glmgee(
  n_events ~ data_schoolyear + offset(log(impl_student_days)),
  id     = school_id,
  family = MASS::negative.binomial(theta = theta_marg, link = "log"),
  data   = util_mod_df,
  waves  = as.numeric(data_schoolyear),
  corstr = "Exchangeable"
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

write_rds(util_mod_numer, nested_here("output", "iptw_mod_marg_nb.rds"))

#############################################################################

# Denominator: conditional

library(splines)
default::default(ns) <- list(df = 2, intercept = FALSE)
iptw_denom_max <- glmgee(
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
  
  family = negative.binomial(theta = theta_cond, link = "log"),
  data   = util_mod_df,
  id     = school_id,
  waves  = as.numeric(data_schoolyear),
  corstr = "Exchangeable"
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
write_rds(drop_mods, nested_here("output", "iptw_drop_mods_nb.rds"))

drop_qic <- c(list("--" = iptw_denom_max), drop_mods) |> 
  map(QIC) |> 
  unlist() |> 
  enframe(name = "model", value = "QIC") |> 
  
  full_join(
    c(list("--" = iptw_denom_max), drop_mods) |> 
      map(QIC, u = TRUE) |> 
      unlist() |> 
      enframe(name = "model", value = "QICu")
  )
write_rds(drop_qic, nested_here("output", "iptw_drop_mod_qic_nb.rds"))

drop_qic |> 
  pivot_longer(c(QIC, QICu)) |> 
  filter(value == min(value), .by = name) |> 
  pull(model)  

iptw_denom_best <- drop_mods[["ns(iep_pct) + ns(food_allergy_pct) + ns(esl_pct) + ns(frm_pct) + ns(homelessness_pct)"]]

write_rds(iptw_denom_best, nested_here("output", "iptw_mod_cond_nb.rds"))

#############################################################################


util_mod_df$pred_numer <- predict(
  read_rds(nested_here("output", "iptw_mod_marg_nb.rds")),
  type = "response"
  )

util_mod_df$iptw_numer <- unlist(map2(
  util_mod_df$n_events,
  util_mod_df$pred_numer,
  ~{dnbinom(x = .x, mu = .y, size = theta_marg)}
))


util_mod_df$pred_denom <- predict(
  read_rds(nested_here("output", "iptw_mod_cond_nb.rds")),
  type = "response"
)

util_mod_df$iptw_denom <- unlist(map2(
  util_mod_df$n_events,
  util_mod_df$pred_denom,
  ~{dnbinom(x = .x, mu = .y, size = theta_cond)}
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
  write_rds(nested_here("data", "iptw_weights_nb.rds"))

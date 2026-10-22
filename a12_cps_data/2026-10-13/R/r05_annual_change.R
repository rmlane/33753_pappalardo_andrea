# Develop poisson models for count/rate variables to 
# estimate districtwide averages; compare districtwide
# averages across time

count_mods <- c(
  "asthma_count",
  "food_allergy_count",
  "non_food_allergy_count",
  "student_504_count",
  "iep_count",
  "esl_count",
  "frm_count",
  "homelessness_count"
  ) |>
  set_names() |> 
  map(~{
    eval(bquote(
    
      geepack::geeglm(
        arsenal::formulize(.x, "data_schoolyear"),
        offset = log(student_count),
        family = poisson(link = "log"),
        data   = read_rds(nested_here("data/cps_schools_analytic.rds")),
        id     = school_id,
        waves  = data_schoolyear,
        corstr = "exchangeable"
      )
    
    ))
  }) |> 
  
  write_if_diff(
    nested_here("output", "models", "yearly_change_counts.rds")
    )




asthma_attd_counts <- read_rds(nested_here("data", "cps_schools_analytic.rds")) |>
  pivot_longer(c(
    "asthma_asthma_dx_days_per_school_expected",
    "asthma_asthma_dx_days_per_school_present",
    "asthma_no_asthma_dx_days_per_school_expected",
    "asthma_no_asthma_dx_days_per_school_present"
  )) |> 
  select(school_id, data_schoolyear, name, value) |> 
  
  separate(name, c("asthma_status", "name"), sep = "_dx_") |> 
  mutate(value = round(value)) |> 
  pivot_wider(values_fill = 0) |> 
  filter(days_per_school_expected > 0)

schooldays_mod <- list(
  overall = geepack::geeglm(
    days_per_school_present ~ data_schoolyear,
    offset = log(days_per_school_expected),
    family = poisson(link = "log"),
    data   = read_rds(nested_here("data/cps_schools_analytic.rds")) |> 
      mutate(across(matches("days_per"), ~ round(.x))),
    id     = school_id,
    waves  = data_schoolyear,
    corstr = "exchangeable"
  ),
  
  by_asthma = geepack::geeglm(
    days_per_school_present ~ data_schoolyear*asthma_status,
    offset = log(days_per_school_expected),
    family = poisson(link = "log"),
    data   = asthma_attd_counts,
    id     = school_id,
    waves  = data_schoolyear,
    corstr = "exchangeable"
  )
) |> 
  
  write_if_diff(
    nested_here("output", "models", "yearly_change_attd.rds")
  )

























map(count_mods, ~{
  emmeans::emmeans(
    .x, 
    revpairwise ~ data_schoolyear, 
    offset = log(1000), 
    type   = "response",
    infer  = TRUE
  ) |> 
    c(list(offset = "per 1000 students"))
})

emmeans::emmeans(
  schooldays_mod, 
  revpairwise ~ data_schoolyear, 
  offset = log(176), # per schoolyear per student
  type   = "response",
  infer  = TRUE
  ) |> 
  c(list(offset = "per student per schoolyear (176 days expected)"))





schooldays_mod <- geepack::geeglm(
  round(days_per_student_present) ~ data_schoolyear + 
    offset(log(round(days_per_student_expected))),
  family = poisson(link = "log"),
  data   = read_rds(nested_here("data/cps_schools_analytic.rds")),
  id     = school_id,
  waves  = data_schoolyear,
  corstr = "exchangeable"
)



asthma_attd_counts <- read_rds(nested_here("data", "cps_schools_analytic.rds")) |>
  pivot_longer(c(
    "asthma_asthma_dx_days_per_school_expected",
    "asthma_asthma_dx_days_per_school_present",
    "asthma_no_asthma_dx_days_per_school_expected",
    "asthma_no_asthma_dx_days_per_school_present"
  )) |> 
  select(school_id, data_schoolyear, name, value) |> 
  
  separate(name, c("asthma_status", "name"), sep = "_dx_") |> 
  mutate(value = round(value)) |> 
  pivot_wider(values_fill = 0) |> 
  filter(days_per_school_expected > 0)

geepack::geeglm(
  days_per_school_present ~ data_schoolyear + asthma_status,
  offset = log(days_per_school_expected),
  family = poisson(link = "log"),
  data   = asthma_attd_counts,
  id     = school_id,
  waves  = data_schoolyear,
  corstr = "exchangeable"
)






# yoy_change$n_events <- emmeans::emmeans(
#   util_mod, 
#   consec ~ data_schoolyear, 
#   offset = log(176*1000), # events per 1000 students per schoolyear
#   type   = "response",
#   infer  = TRUE
# ) |> 
#   c(list(offset = "per 1000 students per schoolyear of program enrollment (176 days)"))



yoy_change <- transpose(yoy_change)

yearly_estimates <- map2_dfr(
  yoy_change$emmeans,
  yoy_change$offset,
  ~{output <- capture.output(.x)
  
  data.frame(.x) |> 
    mutate(
      rate_offset = .y,
      comments    = output[(which(str_length(output) == 0)+1):length(output)] |> 
        str_trim() |> 
        paste(collapse = "; ")
    ) 
  }, .id = "variable") |> 
  relocate(rate_offset, .after = variable) |> 
  select(-any_of(c("null", "t.ratio", "p.value")))|> 
  janitor::clean_names()


yoy_absolute <-  map2_dfr(
  yoy_change$emmeans,
  yoy_change$offset,
  ~{output <- capture.output(.x)
  
  emmeans::regrid(.x) |>
    emmeans::contrast("consec", infer = c(TRUE, FALSE)) |> 
    data.frame() |> 
    mutate(
      comments    = output[(which(str_length(output) == 0)+1):length(output)] |> 
        str_trim() |> 
        paste(collapse = "; ")
    ) |> 
    rename(abs_change = estimate)
  }, .id = "variable") |> 
  janitor::clean_names()

yoy_ratio <- yoy_change$contrasts |>
  map_dfr(~{
    output <- capture.output(.x)
    
    data.frame(.x) |> 
      mutate(
        comments = output[(which(str_length(output) == 0)+1):length(output)] |> 
          str_trim() |> 
          paste(collapse = "; ")
      )
  }, .id = "variable") |> 
  janitor::clean_names()

yoy_change <- list(yoy_absolute, yoy_ratio) |> 
  map(~{
    .x |> 
      mutate(
        data_schoolyear  = gsub("\\(([0-9\\-]+)\\) . \\(([0-9\\-]+)\\)", "\\1", contrast),
        change_since_schoolyear = gsub("\\(([0-9\\-]+)\\) . \\(([0-9\\-]+)\\)", "\\2", contrast),
        .keep = "unused",
        .after = 1
      )
  }) |> 
  
  reduce(
    full_join,
    by = c("variable", "data_schoolyear", "change_since_schoolyear"),
    suffix = c(".abs_change", ".ratio")
  )

full_join(
  yearly_estimates,
  yoy_change,
  by     = c("variable", "data_schoolyear"),
  suffix = c(".rate", ".change")
  ) |> 
  
  write_rds(nested_here("output", "annual_change.rds"))
  
  
  
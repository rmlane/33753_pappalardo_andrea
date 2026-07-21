list(
  about = c(
    "rcp_sex", "rcp_race", "rcp_eth", 
    "rcp_role",  
    
    "sym_new",
    "inh_use_prev",
    "why_given",
    
    "season", 
    "admin_loc", 
    "nurse_present",
    "used_before",
    
    "Actual_Disposition_simplified", #"true_disp_rcd",
    "severity" ,
    "pathway_chosen", 
    "pathway_chosen_compl",  
    "pathway_true", 
    "pathway_true_compl",
    "spacer", 
    "n_puffs_hfa",
    "n_puffs_respi"
  ),
  
  exposures = c(
    glue::glue(
      "notest({x}, cat.simplify = TRUE)",
      x = c("exp_pollen", "exp_dust", "exp_anim", "exp_smok",
            "exp_air", "exp_heat", "exp_cold", "exp_phys",
            "exp_other", "exp_unk","exp_none")
    )
  ),
  
  school = c(
    "wave", "impl_schoolyear",  "grade_level", 
    "region", "region_3grp", "region_2grp"
  ),
  
  symptoms = c(
    glue::glue(
      "notest({x}, cat.simplify = TRUE)",
      x = c("sym_sob", "sym_wheeze", "sym_cough",
            "sym_tight", "sym_rapid", "sym_speak", 
            "sym_chpain", "sym_flare", "sym_musc", "sym_blue",
            "sym_anx", "sym_brdiff",  "sym_other",  "sym_none")
    )
  )
) |> 
  map_dfr(function(vlist) {
    
    full_join(
      read_rds(nested_here("data", "mrg_school_chars.rds")),
      read_rds(nested_here("data", "mrg_inhaler_events.rds")),
      by = "school_id"
    ) |>
      mutate(
        n_incidents = factor("overall"), 
        .before     = 1
      ) |> 
      tableby_with(
        arsenal::formulize(
          "data_schoolyear", 
          c("notest(n_incidents, cat.simplify = TRUE)", vlist)
        ),
        
        # arsenal::formulize("", vlist),
        control = arsenal::tableby.control(
          test           = FALSE,
          digits.n	     = NA,
          numeric.stats  = c("medianrange", "q1q3",  "meansd")
        )
      ) |>
      arsenal:::summary.tableby(
        text = TRUE, 
        labelTranslations = inhaler_varlabels() |>
          list_assign(rcp_age = "Age") |> 
          imap(~{glue::glue("{.y}: {.x}")})
      ) |>
      
      as.data.frame()
    
  }, .id = "section") |>
  write_table("utilization_chars.rds")
#============================================================ Load packages ===================================

if(!require(pacman))install.packages("pacman")
pacman::p_load(msm,haven, openxlsx,readxl, dplyr, lubridate, stringr, ggplot2, gtsummary, 
               gt, tidyr, forcats, purrr,flexsurv, mstate, SemiMarkov,RColorBrewer)


# =============================================================================
# DATA IMPORT
# =============================================================================

df_smm <- read_dta("Data/semiMarkovUpdated.dta")


# =============================================================================
# VARIABLE RECODING
# =============================================================================

df_smm <- df_smm %>%
  
  # Ensure records are ordered for transition history calculation
  arrange(individualid, start) %>%
  group_by(individualid) %>%
  
  mutate(
    
    # ---------------------------------------------------------
    # Age at Migration (time-varying)
    # Reference category: 15–24 years
    # ---------------------------------------------------------
    migration_age_group = cut(
      migration_age,
      breaks = c(-Inf, 14, 24, 49),
      labels = c("0-14", "15-24", "25-49"),
      right = TRUE
    ),
    
    migration_age_group = factor(
      migration_age_group,
      levels = c("15-24", "0-14", "25-49")
    ),
    
    # ---------------------------------------------------------
    # Migration Period (time-varying)
    # Reference category: Prior 2010
    # ---------------------------------------------------------
    migration_period = case_when(
      migration_year < 2010  ~ "Prior 2010",
      migration_year < 2017  ~ "2010-2016",
      migration_year >= 2017 ~ "2017-2024",
      TRUE ~ NA_character_
    ),
    
    migration_period = factor(
      migration_period,
      levels = c(
        "Prior 2010",
        "2010-2016",
        "2017-2024"
      )
    ),
    
    # ---------------------------------------------------------
    # Prior Migration History (event-history dependence)
    # ---------------------------------------------------------
    prior_transitions = row_number() - 1
    
  ) %>%
  
  ungroup() %>%
  
  mutate(
    
    # ---------------------------------------------------------
    # Other Covariates
    # ---------------------------------------------------------
    ResidenceArea = factor(ResidenceArea),
    
    EducationLevel = factor(
      EducationLevel,
      levels = c(
        "No Education",
        levels(factor(EducationLevel))[levels(factor(EducationLevel)) != "No Education"]
      )
    )
    
  )


# =============================================================================
#OBSERVED TRANSITION MATRIX WITH PERCENTAGES (TABLE 4)
# =============================================================================

original_states <- c("Enumeration", "EIM", "EOM", "Internal movement")
state_labels    <- c("Baseline resident", "In-migration", "Out-migration", "Internal movement")

# Build complete (possibly sparse) count matrix
st_obs <- table(df_smm$from_state, df_smm$to_state)
st_obs <- as.matrix(st_obs)

# Add any missing rows / columns as zeros
for (s in setdiff(original_states, rownames(st_obs))) {
  st_obs <- rbind(st_obs, setNames(rep(0L, ncol(st_obs)), colnames(st_obs)))
  rownames(st_obs)[nrow(st_obs)] <- s
}
for (s in setdiff(original_states, colnames(st_obs))) {
  st_obs <- cbind(st_obs, rep(0L, nrow(st_obs)))
  colnames(st_obs)[ncol(st_obs)] <- s
}

st_obs <- st_obs[original_states, original_states]
rownames(st_obs) <- colnames(st_obs) <- state_labels

# Combine counts and row percentages into "N (x%)" strings
pct <- round(st_obs / sum(st_obs) * 100, 1)

obs_combined <- as.data.frame(
  matrix(
    paste0(st_obs, " (", pct, "%)"),
    nrow = nrow(st_obs),
    ncol = ncol(st_obs),
    dimnames = list(state_labels, state_labels)
  )
)

obs_combined <- cbind(`From \\ To` = rownames(obs_combined), obs_combined)


# =============================================================================
# TRANSITION LABELS AND CANDIDATE DISTRIBUTIONS
# =============================================================================

transition_labels <- c(
  "1" = "In-migration → Out-migration",
  "2" = "In-migration → Internal movement",
  "3" = "Baseline resident → Out-migration",
  "4" = "Baseline resident → Internal movement",
  "5" = "Out-migration → In-migration",
  "6" = "Internal movement → Out-migration",
  "7" = "Internal movement → Internal movement"
)

candidate_distributions <- c(
  "exp",
  "weibull",
  "gamma",
  "gengamma",
  "llogis",
  "lnorm",
  "gompertz"
)
transitions <- sort(unique(df_smm$trans))
# =============================================================================
# DISTRIBUTION SELECTION
# =============================================================================

best_distributions <- list()
all_aic_results    <- list()

for (k in transitions) {
  
  data_k <- filter(df_smm, trans == k)
  
  aic_table <- purrr::map_dfr(
    candidate_distributions,
    function(dist) {
      
      fit <- tryCatch(
        flexsurvreg(
          Surv(time_in_state, status) ~ 1,
          data = data_k,
          dist = dist
        ),
        error = function(e) NULL
      )
      
      tibble(
        Distribution = dist,
        AIC = ifelse(is.null(fit), Inf, AIC(fit))
      )
    }
  )
  
  all_aic_results[[as.character(k)]] <- aic_table
  
  best_distributions[[as.character(k)]] <-
    aic_table %>%
    arrange(AIC) %>%
    dplyr::slice(1) %>%
    pull(Distribution)
}
# =============================================================================
# FINAL TRANSITION-SPECIFIC MODELS
# =============================================================================

fitted_models <- list()

for (k in transitions) {
  
  data_k <- filter(df_smm, trans == k)
  
  model_formula <- if (k %in% c(3, 4)) {
    
    Surv(time_in_state, status) ~
      migration_age_group +
      ResidenceArea +
      EducationLevel
    
  } else {
    
    Surv(time_in_state, status) ~
      migration_age_group +
      prior_transitions +
      ResidenceArea +
      EducationLevel
  }
  
  fitted_models[[as.character(k)]] <- flexsurvreg(
    formula = model_formula,
    data = data_k,
    dist = best_distributions[[as.character(k)]]
  )
  
  cat(
    "\nTransition", k,
    "(", transition_labels[as.character(k)], ")",
    "\nDistribution:",
    best_distributions[[as.character(k)]],
    "\n"
  )
}

# =============================================================================
# MODEL FIT STATISTICS
# =============================================================================

extract_fit_statistics <- function(model_list) {
  
  purrr::imap_dfr(model_list, function(model, trans) {
    
    tibble(
      Transition   = transition_labels[trans],
      Distribution = gsub("\\.quiet", "", model$dlist$name),
      Parameters   = model$npars,
      LogLikelihood = round(model$loglik, 2),
      AIC          = round(AIC(model), 2)
    )
  })
}

fit_statistics <- extract_fit_statistics(fitted_models) %>%
  arrange(AIC)

# =============================================================================
# HAZARD RATIOS WITH 95% CONFIDENCE INTERVALS
# =============================================================================

extract_hazard_ratios <- function(model, transition) {
  
  model_results <- as.data.frame(model$res)
  
  covariate_rows <- rownames(model_results)[
    !rownames(model_results) %in% model$dlist$pars
  ]
  
  hr_table <- model_results[covariate_rows, ]
  
  covariate_names <- rownames(hr_table)
  
  tibble(
    Variable = case_when(
      str_detect(covariate_names, "^migration_age_group") ~ "Age at migration",
      str_detect(covariate_names, "^prior_transitions") ~ "Prior transitions",
      str_detect(covariate_names, "^ResidenceArea") ~ "Residence area",
      str_detect(covariate_names, "^EducationLevel") ~ "Education level",
      TRUE ~ covariate_names
    ),
    
    Level = case_when(
      str_detect(covariate_names, "0-14") ~ "0-14",
      str_detect(covariate_names, "25-49") ~ "25-49",
      str_detect(covariate_names, "^prior_transitions$") ~
        "Per additional transition",
      str_detect(covariate_names, "Urban") ~ "Urban",
      str_detect(covariate_names, "Informal") ~ "Informal",
      str_detect(covariate_names, "Basic") ~ "Basic",
      str_detect(covariate_names, "Secondary") ~ "Secondary",
      str_detect(covariate_names, "Tertiary") ~ "Tertiary",
      TRUE ~ NA_character_
    ),
    
    Transition = transition_labels[transition],
    
    HR = round(exp(hr_table$est), 3),
    
    `95% CI` = paste0(
      "(",
      round(exp(hr_table$`L95%`), 3),
      ", ",
      round(exp(hr_table$`U95%`), 3),
      ")"
    )
  )
}

hazard_ratios <- purrr::imap_dfr(
  fitted_models,
  extract_hazard_ratios
) %>%
  arrange(
    Variable,
    Level,
    Transition
  )

# =============================================================================
# SENSITIVITY ANALYSIS
# Compare best distribution with second- and third-best distributions
# =============================================================================

sensitivity_models <- list()

for (k in transitions) {
  
  data_k <- filter(df_smm, trans == k)
  
  top3_dists <- all_aic_results[[as.character(k)]] %>%
    arrange(AIC) %>%
    dplyr::slice(1:3) %>%
    pull(Distribution)
  
  model_formula <- if (k %in% c(3, 4)) {
    
    Surv(time_in_state, status) ~
      migration_age_group +
      ResidenceArea +
      EducationLevel
    
  } else {
    
    Surv(time_in_state, status) ~
      migration_age_group +
      prior_transitions +
      ResidenceArea +
      EducationLevel
  }
  
  sensitivity_models[[as.character(k)]] <- list()
  
  for (d in top3_dists) {
    
    fit <- tryCatch(
      flexsurvreg(
        formula = model_formula,
        data = data_k,
        dist = d
      ),
      error = function(e) NULL
    )
    
    if (!is.null(fit)) {
      sensitivity_models[[as.character(k)]][[d]] <- fit
    }
  }
}
# =============================================================================
# EXTRACT SOJOURN TIMES
# =============================================================================

sojourn_results_df <- purrr::imap_dfr(
  sensitivity_models,
  function(model_set, trans_id) {
    
    purrr::imap_dfr(
      model_set,
      function(fit, dist_name) {
        
        mean_sojourn <- tryCatch(
          summary(
            fit,
            type = "rmst",
            t = 20
          )[[1]]$est,
          error = function(e) NA
        )
        
        median_sojourn <- tryCatch(
          summary(
            fit,
            type = "quantile",
            quantiles = 0.5
          )[[1]]$est,
          error = function(e) NA
        )
        
        tibble(
          Transition =
            transition_labels[trans_id],
          
          Distribution = dist_name,
          
          MeanSojourn =
            round(mean_sojourn, 2),
          
          MedianSojourn =
            round(median_sojourn, 2),
          
          AIC =
            round(AIC(fit), 2)
        )
      }
    )
  }
)

#================================================================================
# DISPLAY RESULTS
#================================================================================
transition_names <- c(
  "1" = "Inmigration → Outmigration",
  "2" = "Baseline Resident → Outmigration",
  "3" = "Inmigration → Internal Movement",
  "4" = "Baseline Resident → Internal Movement",
  "5" = "Outmigration → Inmigration",
  "6" = "Internal Movement → Outmigration",
  "7" = "Internal Movement → Internal Movement"
)

sojourn_results_df$Transition <- transition_names[as.character(sojourn_results_df$Transition)]
# Ensure Transition is treated as a factor
sojourn_results_df$Transition <- factor(sojourn_results_df$Transition)

set2_colors <- brewer.pal(8, "Set2")

custom_colors <- set2_colors[c(1, 2, 3, 4, 5, 7)]

sensitivity_plot <- ggplot(
  sojourn_results_df,
  aes(
    x = Transition,
    y = MeanSojourn,
    fill = Distribution
  )
) +
  geom_col(
    position = position_dodge(0.8),
    width = 0.7
  ) +
  coord_flip() +
  scale_fill_manual(values = custom_colors) +
  labs(
    x = NULL,
    y = "Mean Sojourn Time",
    fill = "Distribution"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    plot.title = element_blank()
  )

save_plot(sensitivity_plot, "sensitivity_plot.png")

# =============================================================================
# CUMULATIVE HAZARD PLOTS: PARAMETRIC vs NELSON–AALEN (FIGURE 1)
# =============================================================================

plot_cumhaz_data <- purrr::map_dfr(names(fitted_models), function(trans) {
  
  data_k <- subset(df_smm, df_smm$trans == as.numeric(trans))
  dist_k <- best_distributions[[trans]]
  label  <- paste0("Transition ", trans, ": ", transition_labels[trans])
  
  # Nelson–Aalen
  na_fit <- survfit(Surv(time_in_state, status) ~ 1, data = data_k, type = "fh")
  na_df  <- data.frame(
    time       = na_fit$time,
    cumhaz     = na_fit$cumhaz,
    Method     = "Nelson-Aalen estimate",
    Transition = label
  )
  
  # Baseline parametric model
  mod_plot  <- flexsurvreg(Surv(time_in_state, status) ~ 1, data = data_k, dist = dist_k)
  t_seq     <- seq(0, max(data_k$time_in_state, na.rm = TRUE), length.out = 200)
  fh_fitted <- summary(mod_plot, type = "cumhaz", t = t_seq, ci = FALSE)
  
  param_df <- data.frame(
    time       = fh_fitted[[1]]$time,
    cumhaz     = fh_fitted[[1]]$est,
    Method     = "Parametric estimate",
    Transition = label
  )
  
  bind_rows(na_df, param_df)
})

p_cumhaz <- ggplot() +
  geom_step(
    data = subset(plot_cumhaz_data, Method == "Nelson-Aalen estimate"),
    aes(x = time, y = cumhaz, colour = Method),
    linewidth = 0.9
  ) +
  geom_line(
    data = subset(plot_cumhaz_data, Method == "Parametric estimate"),
    aes(x = time, y = cumhaz, colour = Method),
    linewidth = 0.9
  ) +
  scale_colour_manual(
    values = c("Nelson-Aalen estimate" = "#1F78B4", "Parametric estimate" = "#E31A1C")
  ) +
  scale_x_continuous(limits = c(0, 15), breaks = seq(0, 15, by = 3)) +
  facet_wrap(~Transition, scales = "free", axes = "all") +
  labs(x = "Time (years)", y = "Cumulative hazard", colour = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text       = element_text(face = "bold", size = 11),
    legend.position  = "bottom",
    legend.text      = element_text(size = 11),
    axis.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = file.path(hazard_plot_dir, "Combined_Cumulative_Hazard_Plots.png"),
  plot     = p_cumhaz,
  width    = 14, height = 10, dpi = 400
)

cat("\nFigure 1 saved:", file.path(hazard_plot_dir, "Combined_Cumulative_Hazard_Plots.png"), "\n")


# =============================================================================
# 11. SIMULATION-BASED SEMI-MARKOV TRANSITION PROBABILITIES (FIGURE 2)
# =============================================================================



tmat_sim <- matrix(NA, nrow = 4, ncol = 4,
                   dimnames = list(
                     from = c("Baseline resident", "Internal movement",
                              "In-migration",      "Out-migration"),
                     to   = c("Baseline resident", "Internal movement",
                              "In-migration",      "Out-migration")
                   ))

tmat_sim[3, 4] <- 1   # In-migration → Out-migration
tmat_sim[3, 2] <- 2   # In-migration → Internal movement
tmat_sim[1, 4] <- 3   # Baseline resident → Out-migration
tmat_sim[1, 2] <- 4   # Baseline resident → Internal movement
tmat_sim[4, 3] <- 5   # Out-migration → In-migration
tmat_sim[2, 4] <- 6   # Internal movement → Out-migration
tmat_sim[2, 2] <- 7   # Internal movement → Internal movement

fitted_models <- fitted_models[c("1","2","3","4","5","6","7")]

#====================================================================#
# TIME POINTS TO EVALUATE (0 to 15 years)
#====================================================================#

t_eval <- seq(0, 15, by = 0.5)

#====================================================================#
# COMPUTE SIMULATION-BASED TRANSITION PROBABILITY MATRICES
# pmatrix.simfs() returns P(s -> j) at each time t
#====================================================================#

cat("\nComputing simulation-based transition probabilities...\n")
cat("This may take a few minutes depending on M (number of simulations)\n")

new_profile <- data.frame(
  migration_age_group   = "15-24",
  ResidenceArea          = "Rural",
  EducationLevel         = "Informal",
  prior_transitions      = 1
)


if (k %in% c(3,4)) {
  new_profile$prior_transitions <- NULL
}

pt_simfs_cov <- pmatrix.simfs(
  x       = fitted_models,
  trans   = tmat_sim,
  newdata = new_profile,
  t       = t_eval,
  M       = 200,#10000
  ci      = TRUE,
  B       = 10 #1000
)


#====================================================================#
# DEFINE STATE NAMES AND ALLOWED TRANSITIONS FOR PLOTTING
#====================================================================#

state_names_sim <- c(
  "Baseline resident",
  "Internal movement",
  "In-migration",
  "Out-migration"
)

allowed_transitions_sim <- c(
  "Baseline resident \u2192 Internal movement",
  "In-migration \u2192 Out-migration",
  "Internal movement \u2192 Internal movement",
  "Internal movement \u2192 Out-migration",
  "In-migration \u2192 Internal movement",
  "Baseline resident \u2192 Out-migration",
  "Out-migration \u2192 In-migration"
)
#====================================================================#
# EXTRACT AND RESHAPE — correct for [from, to, time] 3D array
#====================================================================#

# Extract time points from the 3rd dimension names
t_actual <- as.numeric(dimnames(pt_simfs_cov)[[3]])

plot_list_sim <- list()

for(from_idx in 1:4){
  for(to_idx in 1:4){
    
    trans_label <- paste0(
      state_names_sim[from_idx],
      " \u2192 ",
      state_names_sim[to_idx]
    )
    
    if(!(trans_label %in% allowed_transitions_sim)) next
    
    # Correct indexing: [from, to, time]
    probs <- pt_simfs_cov[from_idx, to_idx, ]
    
    # Also extract CIs
    probs_lower <- attr(pt_simfs_cov, "lower")[from_idx, to_idx, ]
    probs_upper <- attr(pt_simfs_cov, "upper")[from_idx, to_idx, ]
    
    plot_list_sim[[trans_label]] <- data.frame(
      time        = t_actual,
      probability = as.numeric(probs),
      lower       = as.numeric(probs_lower),
      upper       = as.numeric(probs_upper),
      Transition  = trans_label
    )
  }
}

plot_data_sim <- do.call(rbind, plot_list_sim)

idx <- plot_data_sim$Transition == "Internal movement → Internal movement"

# Save originals
old_lower <- plot_data_sim$lower[idx]
old_upper <- plot_data_sim$upper[idx]

# Transform probability
plot_data_sim$probability[idx] <- 1 - plot_data_sim$probability[idx]

# Transform CI correctly
plot_data_sim$lower[idx] <- 1 - old_upper
plot_data_sim$upper[idx] <- 1 - old_lower
#=========================================================
# SWAP LABELS FOR PLOTTING ONLY
#=========================================================

plot_data_plot <- plot_data_sim

plot_data_plot$Transition <- as.character(plot_data_plot$Transition)

plot_data_plot$Transition[
  plot_data_plot$Transition == "Baseline resident → Out-migration"
] <- "TEMP"

plot_data_plot$Transition[
  plot_data_plot$Transition == "In-migration → Internal movement"
] <- "Baseline resident → Out-migration"

plot_data_plot$Transition[
  plot_data_plot$Transition == "TEMP"
] <- "In-migration → Internal movement"

#====================================================================#
# PLOT WITH CONFIDENCE INTERVALS
#====================================================================#

p_sim <- ggplot(plot_data_plot, aes(x = time, y = probability)) +
  
  # 95% CI ribbon
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    fill  = "#2C7FB8",
    alpha = 0.2
  ) +
  
  # Point estimate line
  geom_line(linewidth = 0.9, colour = "#2C7FB8") +
  
  facet_wrap(~Transition, scales = "fixed", ncol = 3) +
  
  scale_x_continuous(
    limits = c(0, 15),
    breaks = seq(0, 15, by = 3)
  ) +
  
  scale_y_continuous(limits = c(0, 1)) +
  
  labs(
    title = "Semi-Markov Estimated Transition Probabilities",
    x     = "Time since state entry (years)",
    y     = "Transition Probability"
  ) +
  
  theme_bw(base_size = 13) +
  
  theme(
    strip.text       = element_text(face = "bold", size = 10),
    axis.title       = element_text(face = "bold"),
    plot.title       = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )




# =============================================================================
# 13. SOJOURN TIME TABLE
# =============================================================================

sojourn_results <- purrr::map_dfr(sort(unique(df_smm$trans)), function(k) {
  
  mod  <- fitted_models[[as.character(k)]]
  dist <- gsub("\\.quiet", "", mod$dlist$name)
  
  mean_est <- tryCatch(
    summary(mod, type = "mean", ci = TRUE),
    error = function(e) NULL
  )
  
  if (!is.null(mean_est)) {
    est   <- mean_est[[1]]$est
    lower <- mean_est[[1]]$lcl
    upper <- mean_est[[1]]$ucl
  } else {
    cat("Warning: model-based mean failed for Transition", k, "— using empirical mean.\n")
    data_k <- subset(df_smm, trans == k)
    est    <- mean(data_k$time_in_state, na.rm = TRUE)
    lower  <- upper <- NA
  }
  
  data.frame(
    Transition   = transition_labels[as.character(k)],
    Distribution = dist,
    Mean_Sojourn = round(est,   3),
    `95% CI`     = paste0("(", round(lower, 3), ", ", round(upper, 3), ")"),
    stringsAsFactors = FALSE,
    check.names  = FALSE
  )
})



# =============================================================================
# 14. FOREST PLOT OF TRANSITION-SPECIFIC TIME RATIOS (FIGURE 4)
# =============================================================================

forest_df <- purrr::imap_dfr(fitted_models, function(mod, trans) {
  
  res_mat  <- as.data.frame(mod$res)
  cov_rows <- rownames(res_mat)[!rownames(res_mat) %in% mod$dlist$pars]
  hraw     <- res_mat[cov_rows, ]
  cov_names <- rownames(hraw)
  
  Variable <- dplyr::case_when(
    grepl("^migration_age_group", cov_names) ~ "Age at migration",
    grepl("^prior_transitions",   cov_names) ~ "Prior transitions",
    grepl("^ResidenceArea",       cov_names) ~ "Residence area",
    grepl("^EducationLevel",      cov_names) ~ "Education level"
  )
  
  Level <- dplyr::case_when(
    grepl("^migration_age_group0-14",  cov_names) ~ "0-14",
    grepl("^migration_age_group25-49", cov_names) ~ "25-49",
    grepl("^prior_transitions$",       cov_names) ~ "",
    grepl("^ResidenceAreaUrban",       cov_names) ~ "Urban",
    grepl("^EducationLevelBasic",      cov_names) ~ "Basic",
    grepl("^EducationLevelInformal",   cov_names) ~ "Informal",
    grepl("^EducationLevelSecondary",  cov_names) ~ "Secondary",
    grepl("^EducationLevelTertiary",   cov_names) ~ "Tertiary"
  )
  
  data.frame(
    Transition = transition_labels[trans],
    Variable, Level,
    HR    = exp(hraw$est),
    Lower = exp(hraw$`L95%`),
    Upper = exp(hraw$`U95%`)
  )
})

# Build covariate label and set display order
covariate_levels <- rev(c(
  "Residence area: Urban",
  "Age at migration: 0-14", "Age at migration: 25-49",
  "Education level: Basic", "Education level: Informal",
  "Education level: Secondary", "Education level: Tertiary",
  "Prior transitions"
))

forest_df <- forest_df %>%
  mutate(
    Covariate = dplyr::case_when(
      Variable == "Prior transitions" ~ "Prior transitions",
      TRUE                            ~ paste0(Variable, ": ", Level)
    ),
    Covariate = factor(Covariate, levels = covariate_levels)
  )

p_forest <- ggplot(forest_df, aes(x = HR, y = Covariate)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "red") +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.25, linewidth = 1) +
  geom_point(size = 3.5, shape = 16) +
  scale_x_log10() +
  facet_wrap(~Transition, scales = "free_y", ncol = 2) +
  labs(x = "Time Ratio", y = NULL, title = "Transition-specific Time Ratios") +
  theme_bw(base_size = 12) +
  theme(
    axis.text  = element_text(size = 11, face = "bold"),
    axis.title.x = element_text(size = 13, face = "bold"),
    strip.text = element_text(size = 11, face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

ggsave(
  filename = file.path(forest_dir, "Forest_Plot_Hazard_Ratios.png"),
  plot     = p_forest,
  width    = 14, height = 10, dpi = 400
)

cat("Figure 4 saved:", file.path(forest_dir, "Forest_Plot_Hazard_Ratios.png"), "\n")


# =============================================================================
# 15. MARKOV vs SEMI-MARKOV MODEL COMPARISON (TABLE 2)
# =============================================================================
# The continuous-time Markov model assumes exponential (memoryless) sojourn time
# The semi-Markov model uses transition-specific distributions selected by AIC. Both models include the same covariate structure.

cat("\n--- Fitting Markov (exponential) models for comparison ---\n")

markov_models <- purrr::map(transitions, function(k) {
  data_k <- subset(df_smm, trans == k)
  
  if (k %in% c(3, 4)) {
    formula_k <- Surv(time_in_state, status) ~
      migration_age_group + ResidenceArea + EducationLevel
  } else {
    formula_k <- Surv(time_in_state, status) ~
      migration_age_group + prior_transitions + ResidenceArea + EducationLevel
  }
  
  flexsurvreg(formula_k, data = data_k, dist = "exp")
}) %>% setNames(as.character(transitions))

# Aggregate fit statistics across all transitions
aggregate_fit <- function(models) {
  loglik <- sum(sapply(models, `[[`, "loglik"))
  npars  <- sum(sapply(models, `[[`, "npars"))
  aic    <- sum(sapply(models, AIC))
  bic    <- -2 * loglik + npars * log(nrow(df_smm))
  c(LogLik = loglik, Parameters = npars, AIC = aic, BIC = bic)
}

markov_stats  <- aggregate_fit(markov_models)
semi_stats    <- aggregate_fit(fitted_models)

comparison_table <- data.frame(
  Model      = c("Continuous-time Markov (Exponential)", "Semi-Markov (Best-fit distributions)"),
  Parameters = c(markov_stats["Parameters"],  semi_stats["Parameters"]),
  LogLik     = round(c(markov_stats["LogLik"], semi_stats["LogLik"]), 2),
  AIC        = round(c(markov_stats["AIC"],    semi_stats["AIC"]),    2),
  BIC        = round(c(markov_stats["BIC"],    semi_stats["BIC"]),    2),
  stringsAsFactors = FALSE
)

write.csv(comparison_table,
          file.path(output_dir, "Markov_vs_SemiMarkov_Comparison.csv"),
          row.names = FALSE)

cat("\nTable 2 — Model comparison:\n")
print(comparison_table)




#=========================================================
# 3. FULL TRANSITION NAMES
#=========================================================

transition_names <- c(
  "1" = "Inmigration → Outmigration",
  "2" = "Baseline Resident → Outmigration",
  "3" = "Inmigration → Internal Movement",
  "4" = "Baseline Resident → Internal Movement",
  "5" = "Outmigration → Inmigration",
  "6" = "Internal Movement → Outmigration",
  "7" = "Internal Movement → Internal Movement"
)

#=========================================================
# 4. FIT BASELINE MODELS (NO COVARIATES)
#=========================================================

baseline_models <- list()

for (k in transitions) {
  
  data_k <- subset(df_smm, trans == k)
  
  dist_k <- best_distributions[[as.character(k)]]
  
  baseline_models[[as.character(k)]] <- flexsurvreg(
    Surv(time_in_state, status) ~ 1,
    data = data_k,
    dist = dist_k
  )
}

#=========================================================
# 5. TIME GRID
#=========================================================

t_grid <- seq(0, 15, by = 0.1)

#=========================================================
# 6. PARAMETRIC CUMULATIVE HAZARD + CI
#=========================================================

get_param_haz <- function(model, t_grid){
  
  s <- summary(model,
               t = t_grid,
               type = "cumhaz")[[1]]
  
  data.frame(
    time = s$time,
    est = s$est,
    lcl = s$lcl,
    ucl = s$ucl
  )
}

param_list <- list()

for (k in names(baseline_models)) {
  
  df <- get_param_haz(baseline_models[[k]], t_grid)
  df$Transition <- k
  
  param_list[[k]] <- df
}

param_haz <- do.call(rbind, param_list)

#=========================================================
# 7. NON-PARAMETRIC (NELSON–AALEN)
#=========================================================

get_np_haz <- function(data_k){
  
  fit <- survfit(Surv(time_in_state, status) ~ 1, data = data_k)
  
  data.frame(
    time = fit$time,
    haz = fit$cumhaz
  )
}

np_list <- list()

for (k in transitions) {
  
  data_k <- subset(df_smm, trans == k)
  
  df <- get_np_haz(data_k)
  df$Transition <- as.character(k)
  
  np_list[[as.character(k)]] <- df
}

np_haz <- do.call(rbind, np_list)

#=========================================================
# 8. PLOTTING SETUP
#=========================================================

par(mfrow = c(3, 3), mar = c(3.5, 3.5, 3, 1))

#=========================================================
# 9. LOOP PLOTTING
#=========================================================

for (k in transitions) {
  
  data_k <- subset(df_smm, trans == k)
  
  sf <- survfit(Surv(time_in_state, status) ~ 1, data = data_k)
  na_hazard <- sf$cumhaz
  
  fitted_haz <- param_haz[param_haz$Transition == as.character(k), ]
  
  #-------------------------------------------------------
  # X-axis rule (SPECIAL CASE FOR TRANSITION 3)
  #-------------------------------------------------------
  
  if (k == 3) {
    xlim_use <- c(0, 1)
  } else {
    xlim_use <- c(0, 15)
  }
  
  #-------------------------------------------------------
  # Y-axis
  #-------------------------------------------------------
  
  ymax <- max(na_hazard, fitted_haz$ucl, na.rm = TRUE)
  
  #-------------------------------------------------------
  # MAIN PLOT
  #-------------------------------------------------------
  
  plot(
    sf$time,
    na_hazard,
    type = "s",
    col = "black",
    lwd = 2,
    xlim = xlim_use,
    ylim = c(0, ymax),
    main = transition_names[as.character(k)],
    xlab = "",
    ylab = ""
  )
  
  #-------------------------------------------------------
  # PARAMETRIC LINE
  #-------------------------------------------------------
  
  lines(
    fitted_haz$time,
    fitted_haz$est,
    col = "red",
    lwd = 2
  )
  
  #-------------------------------------------------------
  # CI (RED DOTTED LINES)
  #-------------------------------------------------------
  
  lines(
    fitted_haz$time,
    fitted_haz$lcl,
    col = "red",
    lwd = 1.5,
    lty = 2
  )
  
  lines(
    fitted_haz$time,
    fitted_haz$ucl,
    col = "red",
    lwd = 1.5,
    lty = 2
  )
  
  #-------------------------------------------------------
  # NELSON–AALEN ON TOP
  #-------------------------------------------------------
  
  lines(
    sf$time,
    na_hazard,
    type = "s",
    col = "black",
    lwd = 2
  )
}

#=========================================================
# 10. LEGEND PANEL
#=========================================================

plot.new()

legend(
  "center",
  legend = c(
    "Nelson–Aalen",
    "Parametric fit",
    "95% CI"
  ),
  col = c("black", "red", "red"),
  lty = c(1, 1, 2),
  lwd = c(2, 2, 1.5),
  bty = "n",
  cex = 1.1
)

#=========================================================
# 11. GLOBAL LABELS
#=========================================================

mtext(
  "Time in State (Years)",
  side = 1,
  outer = TRUE,
  line = 2.2,
  cex = 1.2
)

mtext(
  "Cumulative Hazard",
  side = 2,
  outer = TRUE,
  line = 2.2,
  cex = 1.2
)


#=========================================================
# RESET
#=========================================================

par(mfrow = c(1,1))


# =============================================================================
# COMPILE AND SAVE WORD DOCUMENT
# =============================================================================

doc <- read_docx()

#Table 1: Observed transitions 
doc <- doc %>%
  body_add_par("Table 1: Observed Transitions, n (%)", style = "heading 1") %>%
  body_add_flextable(flextable(obs_combined) %>% theme_booktabs() %>% autofit()) %>%
  body_add_par("", style = "Normal")

#Table 1: Model fit statistics
doc <- doc %>%
  body_add_par("Table 1: Best Parametric Distribution by Transition", style = "heading 1") %>%
  body_add_flextable(flextable(table1_fit) %>% theme_booktabs() %>% autofit()) %>%
  body_add_par("", style = "Normal")

#Table 2: Markov vs Semi-Markov comparison 
doc <- doc %>%
  body_add_par("Table 2: Comparison of Continuous-Time Markov and Semi-Markov Models",
               style = "heading 1") %>%
  body_add_flextable(flextable(comparison_table) %>% theme_booktabs() %>% autofit()) %>%
  body_add_par("", style = "Normal")

#Table 3: Hazard ratios
doc <- doc %>%
  body_add_par("Table 3: Time Ratios and 95% Confidence Intervals", style = "heading 1") %>%
  body_add_flextable(flextable(hazard_final) %>% theme_booktabs() %>% autofit()) %>%
  body_add_par("", style = "Normal")



#Table 5: Sojourn times 
doc <- doc %>%
  body_add_par("Table 5: Mean Sojourn Times by Transition", style = "heading 1") %>%
  body_add_flextable(flextable(sojourn_results) %>% theme_booktabs() %>% autofit()) %>%
  body_add_par("", style = "Normal")

#Figure 1: Cumulative hazard plots 
doc <- doc %>%
  body_add_par(
    "Figure 1: Baseline fitted parametric cumulative hazard functions overlaid on Nelson–Aalen estimates.",
    style = "heading 1"
  ) %>%
  body_add_img(
    src    = file.path(hazard_plot_dir, "Combined_Cumulative_Hazard_Plots.png"),
    width  = 7, height = 5.5
  ) %>%
  body_add_par("", style = "Normal")

#Figure 2: Transition probabilities
doc <- doc %>%
  body_add_par("Figure 2: Semi-Markov simulation-based transition probabilities.", style = "heading 1") %>%
  body_add_img(
    src    = file.path(tp_dir, "SemiMarkov_Transition_Probabilities.png"),
    width  = 7, height = 4.5
  ) %>%
  body_add_par("", style = "Normal")

#Figure 3: Transition diagram 
doc <- doc %>%
  body_add_par("Figure 3: Migration state transition diagram.", style = "heading 1") %>%
  body_add_img(
    src    = file.path(diagram_dir, "transition_diagram.png"),
    width  = 7, height = 5
  ) %>%
  body_add_par("", style = "Normal")

# Figure 4: Forest plot 
doc <- doc %>%
  body_add_par("Figure 4: Forest plot of transition-specific time ratios.", style = "heading 1") %>%
  body_add_img(
    src    = file.path(forest_dir, "Forest_Plot_Hazard_Ratios.png"),
    width  = 7, height = 5.5
  ) %>%
  body_add_par("", style = "Normal")

# Save word document
print(doc, target = file.path(output_dir, "SemiMarkov_Model_Results.docx"))

cat("\n==========================================================\n")
cat("All outputs saved to:", output_dir, "\n")
cat("==========================================================\n")
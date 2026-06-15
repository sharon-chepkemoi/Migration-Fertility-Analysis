
if(!require(pacman))install.packages("pacman")
pacman::p_load(msm,haven, openxlsx,readxl, dplyr, lubridate, stringr, ggplot2, gtsummary, 
               gt, tidyr, forcats, purrr,flexsurv, mstate, SemiMarkov,RColorBrewer)


#================================== Data Manipulations =============================

df_smm <- read_dta("semiMarkov_data.dta")
  
  
df_smm <- df_smm %>%
  mutate(
    EducationLevel = case_when(
      EducationLevel %in% c("Quranic", "Madarsa") ~ "Informal",
      EducationLevel %in% c("", "Others")|is.na(EducationLevel)  ~ "No Education",
      TRUE                                        ~ as.character(EducationLevel)
    )
  )

df_smm$age_group <- as.factor(df_smm$age_group)

df_smm <- df_smm %>%
  mutate(
    ResidenceArea = case_when(
      ResidenceArea %in% c("Pre-Urban", "Urban") ~ "Urban",
      TRUE                                        ~ ResidenceArea
    )
  )

ids_needed <- df_smm %>%
  dplyr::select(individualid) %>%          # replace 'id' with your ID column name
  distinct()

dob_data <- df %>%
  filter(individualid %in% ids_needed$individualid) %>%
   dplyr::select(individualid, DateofBirth) %>%
  distinct(individualid, .keep_all = TRUE)# keep matching IDs

final_data <- df_smm %>%
  left_join(dob_data, by = "individualid")


data <- final_data %>%
  mutate(
    migration_year = year(start),
    
    # age at migration
    migration_age = floor(
      time_length(interval(DateofBirth, start), "years")
    )
  )

data <- data %>%
  mutate(
    migration_age_group = case_when(
      migration_age <= 14 ~ "0-14",
      migration_age >= 15 & migration_age <= 24 ~ "15-24",
      migration_age >= 25 & migration_age <= 34 ~ "25-34",
      migration_age >= 35 & migration_age <= 49 ~ "35-49",
      TRUE ~ NA_character_
    )
  )


write_dta(data, "semiMarkovUpdated.dta")

#===================================================================================
df_semiMarkov <- read_dta("Data/semiMarkovUpdated.dta")

df_semiMarkov <- df_semiMarkov %>%
  mutate(
    migration_age_group2 = case_when(
      migration_age <= 14 ~ "0-14",
      migration_age >= 15 & migration_age <= 24 ~ "15-24",
      migration_age >= 25 & migration_age <= 49 ~ "25-49",
      TRUE ~ NA_character_
    )
  )


df_semiMarkov <- df_semiMarkov %>%   # order transitions by time
  group_by(individualid) %>%
  mutate(
    prior_transitions = row_number() - 1
  ) %>%
  ungroup()


df_semiMarkov <- df_semiMarkov %>%
  mutate(
    prior_transition_group = case_when(
      prior_transitions <= 0  ~ "0",
      prior_transitions == 1 ~ "1",
      prior_transitions >= 1 & prior_transitions <= 8 ~ "1+",
      TRUE ~ NA_character_
    )
  )

df_semiMarkov <- df_semiMarkov %>%
  mutate(
    migration_year_group = case_when(
      migration_year < 2010 ~ "Before 2010",
      migration_year >= 2010 & migration_year < 2017 ~ "2010-2016",
      migration_year >= 2017 & migration_year <= 2024 ~ "2017-2024",
      TRUE ~ NA_character_
    )
  )



#================================================================================
# STEP 1: IDENTIFY BEST DISTRIBUTION PER TRANSITION
#================================================================================
distributions <- c("exp", "weibull", "gamma", "gengamma",
                   "llogis", "lnorm", "gompertz")

# Identify all unique transitions 
transitions <- sort(unique(df_semiMarkov$trans))


best_distributions <- list()
all_aic_results <- list()

for (k in transitions) {
  
  cat("\n--- Testing distributions for Transition", k, "---\n")
  
  # Subset transition data
  data_k <- subset(df_semiMarkov, trans == k)
  
  aic_values <- c()
  
  for (dist_name in distributions) {
    
    fit <- tryCatch({
      
      flexsurvreg(
        Surv(time_in_state, status) ~ 1,
        data = data_k,
        dist = dist_name
      )
      
    }, error = function(e) NULL)
    
    if (!is.null(fit)) {
      
      aic_values[dist_name] <- AIC(fit)
      
      cat(dist_name, "AIC:", AIC(fit), "\n")
    }
  }
  
  # Store AIC table
  aic_df <- data.frame(
    Transition = k,
    Distribution = names(aic_values),
    AIC = as.numeric(aic_values)
  )
  
  all_aic_results[[as.character(k)]] <- aic_df
  
  # Best distribution
  best_dist <- names(which.min(aic_values))
  
  best_distributions[[as.character(k)]] <- best_dist
  
  cat(">> Best distribution for Transition",
      k,
      "is:",
      best_dist,
      "with AIC:",
      min(aic_values),
      "\n")
}

# Combine AIC results
all_aic_results_df <- do.call(rbind, all_aic_results)

#============================================================== model with covariates ================

fitted_models <- list()

for (k in transitions) {
  
  data_k <- subset(df_semiMarkov, trans == k)
  
  #--------------------------------------------------
  # Reference categories
  #--------------------------------------------------
  
  data_k$EducationLevel <- relevel(
    factor(data_k$EducationLevel),
    ref = "No Education"
  )
  
  data_k$ResidenceArea <- relevel(
    factor(data_k$ResidenceArea),
    ref = "Rural"
  )
  
  # data_k$migration_year_group <- relevel(
  #   factor(data_k$migration_year_group),
  #   ref = "Before 2010"
  # )
  
  data_k$migration_age_group2 <- relevel(
    factor(data_k$migration_age_group2),
    ref = "15-24"
  )
  
  #--------------------------------------------------
  # Best distribution
  #--------------------------------------------------
  
  dist_k <- best_distributions[[as.character(k)]]
  
  #--------------------------------------------------
  # Model formula
  # Exclude prior_transition_group for transition 4
  #--------------------------------------------------
  
  if (k %in% c(4,3)) {
    
    model_formula <- Surv(time_in_state, status) ~
      migration_age_group2 +
      ResidenceArea +
      EducationLevel
    
  } else {
    
    #data_k$prior_transition_group <- relevel(
    #  factor(data_k$prior_transition_group),
    #  ref = "0"
    #)
    
    model_formula <- Surv(time_in_state, status) ~
      migration_age_group2 +
      prior_transitions +
      ResidenceArea +
      EducationLevel
  }
  
  #--------------------------------------------------
  # Fit final model
  #--------------------------------------------------
  
  model_fit <- flexsurvreg(
    model_formula,
    data = data_k,
    dist = dist_k
  )
  
  fitted_models[[as.character(k)]] <- model_fit
  
  cat("\n======================================================\n")
  cat("Final Model for Transition", k,
      "(", dist_k, "distribution )\n")
  cat("======================================================\n")
  
  print(model_fit)
}


#===================================================================================================
#Interaction effects
#======================================================================================================
#=========================================================
# COMPARE INTERACTION MODELS
#=========================================================

interaction_results <- data.frame()

for (k in transitions) {
  
  data_k <- subset(df_semiMarkov, trans == k)
  
  #--------------------------------------------------
  # Reference categories
  #--------------------------------------------------
  
  data_k$EducationLevel <- relevel(
    factor(data_k$EducationLevel),
    ref = "No Education"
  )
  
  data_k$ResidenceArea <- relevel(
    factor(data_k$ResidenceArea),
    ref = "Rural"
  )
  
  data_k$migration_year_group <- relevel(
    factor(data_k$migration_year_group),
    ref = "Before 2010"
  )
  
  data_k$migration_age_group2 <- relevel(
    factor(data_k$migration_age_group2),
    ref = "15-24"
  )
  
  dist_k <- best_distributions[[as.character(k)]]
  
  #--------------------------------------------------
  # Base covariates
  #--------------------------------------------------
  
  if (k %in% c(3,4)) {
    
    base_formula <- Surv(time_in_state, status) ~
      migration_age_group2 +
      ResidenceArea +
      EducationLevel
    
    age_res_formula <- Surv(time_in_state, status) ~
      migration_age_group2 * ResidenceArea +
      EducationLevel
    
    edu_res_formula <- Surv(time_in_state, status) ~
      migration_age_group2 +
      EducationLevel * ResidenceArea
    
    both_formula <- Surv(time_in_state, status) ~
      migration_age_group2 * ResidenceArea +
      EducationLevel * ResidenceArea
    
  } else {
    
    base_formula <- Surv(time_in_state, status) ~
      migration_age_group2 +
      prior_transitions +
      ResidenceArea +
      EducationLevel
    
    age_res_formula <- Surv(time_in_state, status) ~
      migration_age_group2 * ResidenceArea +
      prior_transitions +
      EducationLevel
    
    edu_res_formula <- Surv(time_in_state, status) ~
      migration_age_group2 +
      prior_transitions +
      EducationLevel * ResidenceArea
    
    both_formula <- Surv(time_in_state, status) ~
      migration_age_group2 * ResidenceArea +
      prior_transitions +
      EducationLevel * ResidenceArea
  }
  
  #--------------------------------------------------
  # Fit models
  #--------------------------------------------------
  
  model_base <- flexsurvreg(
    base_formula,
    data = data_k,
    dist = dist_k
  )
  
  model_age_res <- flexsurvreg(
    age_res_formula,
    data = data_k,
    dist = dist_k
  )
  
  model_edu_res <- flexsurvreg(
    edu_res_formula,
    data = data_k,
    dist = dist_k
  )
  
  model_both <- flexsurvreg(
    both_formula,
    data = data_k,
    dist = dist_k
  )
  
  #--------------------------------------------------
  # Store AICs
  #--------------------------------------------------
  
  interaction_results <- rbind(
    interaction_results,
    data.frame(
      Transition = k,
      Distribution = dist_k,
      No_Interaction = AIC(model_base),
      Age_Residence = AIC(model_age_res),
      Education_Residence = AIC(model_edu_res),
      Both_Interactions = AIC(model_both)
    )
  )
  
  cat("\n=====================================\n")
  cat("Transition:", k, "\n")
  cat("=====================================\n")
  
  print(
    data.frame(
      Model = c(
        "No Interaction",
        "Age × Residence",
        "Education × Residence",
        "Both Interactions"
      ),
      AIC = c(
        AIC(model_base),
        AIC(model_age_res),
        AIC(model_edu_res),
        AIC(model_both)
      )
    )
  )
}

interaction_results

model_5 <- model_edu_res   # for transition 5 best model
model_7 <- model_both      # for transition 7 best model


#================================================================================
# STEP 3: SENSITIVITY ANALYSIS
# Compare best distribution with next two best distributions
#================================================================================

sensitivity_models <- list()

for (k in transitions) {
  
  cat("\n====================================================\n")
  cat("Sensitivity Analysis for Transition", k, "\n")
  cat("====================================================\n")
  
  #--------------------------------------------------
  # Subset data for transition
  #--------------------------------------------------
  
  data_k <- subset(df_semiMarkov, trans == k)
  
  #--------------------------------------------------
  # Reference categories
  #--------------------------------------------------
  
  data_k$EducationLevel <- relevel(
    factor(data_k$EducationLevel),
    ref = "No Education"
  )
  
  data_k$ResidenceArea <- relevel(
    factor(data_k$ResidenceArea),
    ref = "Rural"
  )
  
  data_k$migration_year_group <- relevel(
    factor(data_k$migration_year_group),
    ref = "Before 2010"
  )
  
  data_k$migration_age_group2 <- relevel(
    factor(data_k$migration_age_group2),
    ref = "15-24"
  )
  
  #--------------------------------------------------
  # Define model formula
  # Exclude prior_transition_group for transitions 3 and 4
  #--------------------------------------------------
  
  if (k %in% c(4, 3)) {
    
    model_formula <- Surv(time_in_state, status) ~
      migration_age_group2 +
      migration_year_group +
      ResidenceArea +
      EducationLevel
    
  } else {
    
    # data_k$prior_transition_group <- relevel(
    #   factor(data_k$prior_transition_group),
    #   ref = "0-2"
    # )
    
    model_formula <- Surv(time_in_state, status) ~
      migration_age_group2 +
      prior_transitions +
      migration_year_group +
      ResidenceArea +
      EducationLevel
  }
  
  #--------------------------------------------------
  # Get top 3 distributions based on AIC
  #--------------------------------------------------
  
  aic_k <- all_aic_results[[as.character(k)]]
  
  aic_k <- aic_k[order(aic_k$AIC), ]
  
  top3_dists <- head(aic_k$Distribution, 3)
  
  sensitivity_models[[as.character(k)]] <- list()
  
  #--------------------------------------------------
  # Fit models using top 3 distributions
  #--------------------------------------------------
  
  for (d in top3_dists) {
    
    cat("\nFitting:", d, "\n")
    
    fit <- tryCatch({
      
      flexsurvreg(
        model_formula,
        data = data_k,
        dist = d
      )
      
    }, error = function(e) {
      
      cat("Error fitting", d, "for transition", k, "\n")
      return(NULL)
    })
    
    if (!is.null(fit)) {
      
      sensitivity_models[[as.character(k)]][[d]] <- fit
      
      cat("AIC:", fit$AIC, "\n")
    }
  }
}


#================================================================================
# STEP 5: EXTRACT SOJOURN TIMES
# Compare MEAN and MEDIAN sojourn times
#================================================================================

sojourn_results <- list()

for(k in transitions){
  
  model_set <- sensitivity_models[[as.character(k)]]
  
  temp <- list()
  
  for(d in names(model_set)){
    
    fit <- model_set[[d]]
    
    #---------------- Restricted Mean Sojourn Time (RMST)
    # More stable than unrestricted mean
    mean_time <- tryCatch({
      
      summary(
        fit,
        type = "rmst",
        t = 20
      )[[1]]$est
      
    }, error = function(e) NA)
    
    #---------------- Median Sojourn Time
    median_time <- tryCatch({
      
      summary(
        fit,
        type = "quantile",
        quantiles = 0.5
      )[[1]]$est
      
    }, error = function(e) NA)
    
    temp[[d]] <- data.frame(
      Transition = k,
      Distribution = d,
      MeanSojourn = mean_time,
      MedianSojourn = median_time,
      AIC = AIC(fit)
    )
  }
  
  sojourn_results[[as.character(k)]] <- do.call(rbind, temp)
}

sojourn_results_df <- do.call(rbind, sojourn_results)

#================================================================================
# STEP 6: DISPLAY RESULTS
#================================================================================

cat("\n====================================================\n")
cat("SOJOURN TIME SENSITIVITY RESULTS\n")
cat("====================================================\n")

print(sojourn_results_df)



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

#---------------------------------------------------
# BAR PLOT: Mean Sojourn Time by Distribution
#---------------------------------------------------
set2_colors <- brewer.pal(8, "Set2")

custom_colors <- set2_colors[c(1, 2, 3, 4, 5, 7)]  # skip 6, include 7
ggplot(sojourn_results_df,
       aes(x = Transition,
           y = MeanSojourn,
           fill = Distribution)) +
  
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8),
           width = 0.7) +
  
  scale_fill_manual(values = custom_colors) +
  
  labs(
    title = "Sensitivity Analysis of Mean Sojourn Times",
    x = "Transition",
    y = "Mean Sojourn Time",
    fill = "Distribution"
  ) +
  coord_flip()+
  theme_bw(base_size = 14) +
  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "right"
  )


#========================================== Transition probabilities =============================================

#====================================================================#
# 6. SEMI-MARKOV TRANSITION PROBABILITIES via pmatrix.simfs()
#====================================================================#


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
  migration_age_group2   = "15-24",
  ResidenceArea          = "Rural",
  EducationLevel         = "Informal",
  migration_year_group   = "2010-2016",
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
  M       = 10000,
  ci      = TRUE,
  B       = 1000
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
t_actual <- as.numeric(dimnames(pt_simfs)[[3]])

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
    probs <- pt_simfs[from_idx, to_idx, ]
    
    # Also extract CIs
    probs_lower <- attr(pt_simfs, "lower")[from_idx, to_idx, ]
    probs_upper <- attr(pt_simfs, "upper")[from_idx, to_idx, ]
    
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

#====================================================================#
# SAVE & ADD TO WORD
#====================================================================#

ggsave(
  filename = file.path(tp_dir, "SemiMarkov_Transition_Probabilities.png"),
  plot     = p_sim,
  width    = 14,
  height   = 8,
  dpi      = 400
)

doc <- doc %>%
  body_add_par(
    "Figure: Semi-Markov simulation-based transition probabilities.",
    style = "heading 1"
  ) %>%
  body_add_img(
    src    = file.path(tp_dir, "SemiMarkov_Transition_Probabilities.png"),
    width  = 7,
    height = 4.5
  ) %>%
  body_add_par("", style = "Normal")

cat("\nSemi-Markov transition probability plots generated successfully\n")





#============================================== model fitness ===================================================

#=========================================================
# 2. BEST DISTRIBUTIONS (ASSUMED ALREADY COMPUTED)
#=========================================================

# transitions must already exist
# best_distributions must already exist

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
  
  data_k <- subset(df_semiMarkov, trans == k)
  
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
  
  data_k <- subset(df_semiMarkov, trans == k)
  
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
  
  data_k <- subset(df_semiMarkov, trans == k)
  
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

# mtext(
#   "Baseline Cumulative Hazard: Parametric vs Non-Parametric",
#   side = 3,
#   outer = TRUE,
#   line = 0.5,
#   cex = 1.3,
#   font = 2
# )

#=========================================================
# RESET
#=========================================================

par(mfrow = c(1,1))
#===========================================================
# 2. BASELINE MODELS (NO COVARIATES)
#===========================================================

baseline_models <- list()

for (k in transitions) {
  
  data_k <- subset(df_semiMarkov, trans == k)
  
  dist_k <- best_distributions[[as.character(k)]]
  
  baseline_models[[as.character(k)]] <- flexsurvreg(
    Surv(time_in_state, status) ~ 1,
    data = data_k,
    dist = dist_k
  )
}

#===========================================================
# 3. TIME GRID
#===========================================================

t_grid <- seq(0, 15, by = 0.5)

#===========================================================
# 4. PARAMETRIC CUMULATIVE HAZARD (WITH CI)
#===========================================================

get_param_haz <- function(model, t_grid){
  
  s <- summary(model,
               t = t_grid,
               type = "cumhaz")[[1]]
  
  data.frame(
    time = s$time,
    haz = s$est,
    lower = s$lcl,
    upper = s$ucl
  )
}

param_list <- list()

for (k in names(baseline_models)) {
  
  df <- get_param_haz(baseline_models[[k]], t_grid)
  df$Transition <- paste("Transition", k)
  
  param_list[[k]] <- df
}

param_haz <- do.call(rbind, param_list)
param_haz$type <- "Parametric"

#===========================================================
# 5. NON-PARAMETRIC CUMULATIVE HAZARD (NELSON–AALEN)
#===========================================================

get_np_haz <- function(data_k){
  
  fit <- survfit(Surv(time_in_state, status) ~ 1, data = data_k)
  
  data.frame(
    time = fit$time,
    haz = fit$cumhaz,
    lower = NA,
    upper = NA
  )
}

np_list <- list()

for (k in transitions) {
  
  data_k <- subset(df_semiMarkov, trans == k)
  
  df <- get_np_haz(data_k)
  df$Transition <- paste("Transition", k)
  
  np_list[[as.character(k)]] <- df
}

np_haz <- do.call(rbind, np_list)
np_haz$type <- "Non-parametric"

#===========================================================
# 6. COMBINE DATA
#===========================================================

haz_data <- rbind(param_haz, np_haz)

#===========================================================
# 7. FINAL PLOT
#===========================================================

ggplot(haz_data, aes(x = time, y = haz, color = type, fill = type)) +
  
  # Parametric CI
  geom_ribbon(
    data = subset(haz_data, type == "Parametric"),
    aes(ymin = lower, ymax = upper),
    alpha = 0.25,
    colour = NA
  ) +
  
  # Lines
  geom_line(linewidth = 1) +
  
  # Facets per transition
  facet_wrap(~Transition, scales = "free_y", ncol = 3) +
  
  labs(
    title = "Baseline Cumulative Hazard: Parametric vs Non-Parametric",
    x = "Time (years)",
    y = "Cumulative hazard"
  ) +
  
  theme_bw(base_size = 13) +
  
  theme(
    legend.title = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )





#=========================================================
# SETTINGS
#=========================================================
tmax <- 15
par(mfrow = c(3, 3), mar = c(3.5, 3.5, 3, 1))

# Ensure consistent ordering
transition_ids <- names(baseline_models)

#=========================================================
# LOOP THROUGH TRANSITIONS
#=========================================================
for (k in transition_ids) {
  
  data_k <- subset(df_semiMarkov, trans == as.numeric(k))
  
  sf <- survfit(Surv(time_in_state, status) ~ 1, data = data_k)
  na_hazard <- sf$cumhaz
  
  fitted_model <- baseline_models[[k]]
  
  fitted_haz <- summary(
    fitted_model,
    t = seq(0, tmax, by = 0.1),
    type = "cumhaz"
  )[[1]]
  
  #-------------------------------------------------------
  # X-axis always 0–15
  #-------------------------------------------------------
  xlim_use <- c(0, tmax)
  
  #-------------------------------------------------------
  # Y-axis range including CI
  #-------------------------------------------------------
  ymax <- max(
    na_hazard,
    fitted_haz$ucl,
    na.rm = TRUE
  )
  
  #=======================================================
  # MAIN PLOT
  #=======================================================
  
  plot(
    sf$time,
    na_hazard,
    type = "s",
    col = "black",
    lwd = 2,
    xlim = xlim_use,
    ylim = c(0, ymax),
    main = paste("Transition", k),   # <-- numeric labels
    xlab = "",
    ylab = "",
    cex.main = 1
  )
  
  #=======================================================
  # PARAMETRIC CI BAND
  #=======================================================
  
  polygon(
    x = c(fitted_haz$time, rev(fitted_haz$time)),
    y = c(fitted_haz$lcl, rev(fitted_haz$ucl)),
    border = NA,
    col = adjustcolor("red", alpha.f = 0.20)
  )
  
  # Parametric line
  lines(
    fitted_haz$time,
    fitted_haz$est,
    col = "red",
    lwd = 2
  )
  
  # Re-draw Nelson–Aalen
  lines(
    sf$time,
    na_hazard,
    type = "s",
    col = "black",
    lwd = 2
  )
}

#=========================================================
# LEGEND PANEL (LAST EMPTY SLOT)
#=========================================================

plot.new()

legend(
  "center",
  legend = c(
    "Nelson–Aalen (Non-parametric)",
    "Parametric cumulative hazard",
    "95% Confidence Interval"
  ),
  col = c("black", "red", "red"),
  lty = c(1, 1, NA),
  lwd = c(2, 2, NA),
  pch = c(NA, NA, 15),
  pt.cex = 2,
  bty = "n",
  cex = 1.1
)

#=========================================================
# GLOBAL LABELS
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

mtext(
  "Baseline Cumulative Hazard: Parametric vs Nelson–Aalen",
  side = 3,
  outer = TRUE,
  line = 0.5,
  cex = 1.3,
  font = 2
)







#=========================================================
# FIT NULL (NO-COVARIATE) MODELS
#=========================================================

null_models <- list()

for(k in transitions){

  data_k <- subset(df_semiMarkov, trans == k)

  dist_k <- best_distributions[[as.character(k)]]

  null_models[[as.character(k)]] <- flexsurvreg(
    Surv(time_in_state, status) ~ 1,
    data = data_k,
    dist = dist_k
  )
}

#=========================================================
# TRANSITION LABELS
#=========================================================

plot_order <- c(4, 3, 2, 7, 6, 1, 5)

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
# PLOT LAYOUT
#=========================================================

par(
  mfrow = c(3, 3),
  mar = c(3, 3, 3, 1),
  oma = c(4, 4, 3, 1)
)

#=========================================================
# LOOP THROUGH TRANSITIONS
#=========================================================

for(k in plot_order){

  data_k <- subset(df_semiMarkov, trans == k)

  mod <- null_models[[as.character(k)]]

  #-------------------------------------------------------
  # Nelson-Aalen estimate
  #-------------------------------------------------------

  sf <- survfit(
    Surv(time_in_state, status) ~ 1,
    data = data_k
  )

  na_hazard <- -log(sf$surv)

  #-------------------------------------------------------
  # Parametric cumulative hazard + 95% CI
  #-------------------------------------------------------

  tmax <- max(data_k$time_in_state, na.rm = TRUE)

  tgrid <- seq(0, tmax, length.out = 500)

  fitted_haz <- summary(
    mod,
    type = "cumhaz",
    t = tgrid,
    ci = TRUE
  )[[1]]

  #-------------------------------------------------------
  # Custom zoom for selected transitions
  #-------------------------------------------------------

  xlim_use <- c(0, tmax)

  if(k == 3) xlim_use <- c(0, 0.6)
  if(k == 4) xlim_use <- c(0, 1)

  #-------------------------------------------------------
  # Y-axis range including CI
  #-------------------------------------------------------

  ymax <- max(
    na_hazard,
    fitted_haz$ucl,
    na.rm = TRUE
  )

  #-------------------------------------------------------
  # Base plot
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
    ylab = "",
    cex.main = 0.8
  )

  #-------------------------------------------------------
  # Parametric 95% CI band
  #-------------------------------------------------------

  polygon(
    x = c(
      fitted_haz$time,
      rev(fitted_haz$time)
    ),
    y = c(
      fitted_haz$lcl,
      rev(fitted_haz$ucl)
    ),
    border = NA,
    col = adjustcolor("red", alpha.f = 0.20)
  )

  #-------------------------------------------------------
  # Parametric fitted curve
  #-------------------------------------------------------

  lines(
    fitted_haz$time,
    fitted_haz$est,
    col = "red",
    lwd = 2
  )

  #-------------------------------------------------------
  # Re-draw Nelson-Aalen on top
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
# LEGEND PANEL
#=========================================================

plot.new()

legend(
  "center",
  legend = c(
    "Nelson–Aalen",
    "Parametric fit",
    "95% CI"
  ),
  col = c(
    "black",
    "red",
    "red"
  ),
  lty = c(
    1,
    1,
    NA
  ),
  lwd = c(
    2,
    2,
    NA
  ),
  pch = c(
    NA,
    NA,
    15
  ),
  pt.cex = 2,
  bty = "n",
  cex = 1.0
)

#=========================================================
# GLOBAL LABELS
#=========================================================

mtext(
  "Time in State (Years)",
  side = 1,
  outer = TRUE,
  line = 2.2,
  cex = 1.1
)

mtext(
  "Cumulative Hazard",
  side = 2,
  outer = TRUE,
  line = 2.2,
  cex = 1.1
)

mtext(
  "Parametric versus Nelson–Aalen Cumulative Hazard Estimates",
  side = 3,
  outer = TRUE,
  line = 1,
  cex = 1.2,
  font = 2
)

#=========================================================
# RESET GRAPHICS
#=========================================================

par(mfrow = c(1,1))





#============================================== Time Ratios =========================================================


#---------------------------------------------------------------
# REQUIRED ORDER OF TRANSITIONS
#---------------------------------------------------------------
plot_order <- c(4, 3, 2, 7, 6, 1, 5)

transition_names <- c(
  "1" = "Inmigration → Outmigration",
  "2" = "Baseline Resident → Outmigration",
  "3" = "Inmigration → Internal Movement",
  "4" = "Baseline Resident → Internal Movement",
  "5" = "Outmigration → Inmigration",
  "6" = "Internal Movement → Outmigration",
  "7" = "Internal Movement → Internal Movement"
)

#---------------------------------------------------------------
# Plot layout
#---------------------------------------------------------------


#---------------------------------------------------------------
# LOOP THROUGH ORDERED TRANSITIONS
#---------------------------------------------------------------
for (k in plot_order) {
  
  mod <- fitted_models[[as.character(k)]]
  d_name <- best_distributions[[as.character(k)]]
  trans_label <- transition_names[as.character(k)]
  
  #------------------- ZOOM RULES -------------------
  
  if (k == 3) {
    
    plot(
      mod,
      type = "cumhaz",
      xlim = c(0, 0.6),
      main = paste0(trans_label, "\n(", d_name, ")"),
      xlab = "",
      ylab = "",
      col = "red",
      col.obs = "black",
      lwd = 2,
      conf.int = FALSE,
      cex.main = 0.75
    )
    
  } else if (k == 4) {
    
    plot(
      mod,
      type = "cumhaz",
      xlim = c(0, 1),
      main = paste0(trans_label, "\n(", d_name, ")"),
      xlab = "",
      ylab = "",
      col = "red",
      col.obs = "black",
      lwd = 2,
      conf.int = FALSE,
      cex.main = 0.75
    )
    
  } else {
    
    plot(
      mod,
      type = "cumhaz",
      main = paste0(trans_label, "\n(", d_name, ")"),
      xlab = "",
      ylab = "",
      col = "red",
      col.obs = "black",
      lwd = 2,
      conf.int = FALSE,
      cex.main = 0.75
    )
  }
}

#---------------------------------------------------------------
# EMPTY 9th PANEL FOR LEGEND
#---------------------------------------------------------------
plot.new()

legend(
  "center",
  legend = c("Nelson–Aalen", "Parametric"),
  col = c("black", "red"),
  lty = 1,
  lwd = 2,
  bty = "n",
  cex = 1.1
)

#---------------------------------------------------------------
# GLOBAL TITLE
#---------------------------------------------------------------
# 1. Main Global Title (Top)


# 2. Global X-Axis (Bottom)
mtext(
  "Time in State (Years)",
  side = 1, # 1 = Bottom
  outer = TRUE,
  cex = 1.1,
  line = 2.5 # Adjusts how far below the plots it sits
)

# 3. Global Y-Axis (Left)
mtext(
  "Cumulative Hazard",
  side = 2, # 2 = Left
  outer = TRUE,
  cex = 1.1,
  line = 2.5 # Adjusts how far left of the plots it sits
)
#---------------------------------------------------------------
# RESET
#---------------------------------------------------------------
par(mfrow = c(1, 1))



#----------------------------------------------------------------------------------------------
#---------------------------------------------------------------
# Plot layout for the distributions
#---------------------------------------------------------------
par(
  mfrow = c(3, 3),
  mar = c(3, 3, 3, 1), # Individual plot margins
  oma = c(4, 4, 4, 0), # Outer margins for global labels
  xpd = FALSE
)

#---------------------------------------------------------------
# Loop through transitions to plot density distributions
#---------------------------------------------------------------
for (k in transitions) {
  
  # Subset data for the specific transition
  data_k <- subset(df_smm, trans == k)
  
  # Get label details
  d_name <- best_distributions[[as.character(k)]]
  trans_label <- transition_names[as.character(k)]
  
  # Calculate empirical density
  dens <- density(data_k$time_in_state, na.rm = TRUE)
  
  # Set specific X-axis limits to match your hazard plots exactly
  if (k == 3) {
    x_lims <- c(0, 0.6) # Tailored to your updated plot limits
  } else if (k == 4) {
    x_lims <- c(0, 1)
  } else if (k == 5) {
    x_lims <- c(0, 30)
  } else {
    x_lims <- c(0, 18)  # Default max range for most other transitions
  }
  
  # Plot the density distribution
  plot(
    dens,
    xlim = x_lims,
    main = paste0(trans_label, "\n(Model: ", d_name, ")"),
    xlab = "",  # Suppress individual label
    ylab = "",  # Suppress individual label
    col = "darkblue",
    lwd = 2,
    cex.main = 0.8
  )
  
  # Optional: Add a subtle shaded area under the curve
  polygon(dens, col = rgb(0, 0, 1, 0.1), border = NA)
  
  # Optional: Add a rug plot at the bottom to see individual data points
  rug(data_k$time_in_state, col = "darkgray")
}

#---------------------------------------------------------------
# Global Title and Axis Names
#---------------------------------------------------------------
# Main Title
mtext(
  "Distribution of Observed Time in State by Transition",
  side = 3,
  outer = TRUE,
  cex = 1.2,
  line = 1
)

# Global X-Axis
mtext(
  "Time in State (Years)",
  side = 1,
  outer = TRUE,
  cex = 1.1,
  line = 2.5
)

# Global Y-Axis
mtext(
  "Density",
  side = 2,
  outer = TRUE,
  cex = 1.1,
  line = 2.5
)

# Reset plotting layout
par(mfrow = c(1, 1))


#======================================================
# Define the state names in the exact order of your grid
state_names <- c("EIM", "Enumeration", "EOM", "Internal movement")

# Create the transition matrix (byrow = TRUE fills it row by row)
tmat <- matrix(c(
  NA, NA,  1,  2,  # From Inmigration to -> others
  NA, NA,  3,  4,  # From Baseline Resident to -> others
  5, NA,  NA,  NA,  # From Internal to -> others
  NA, NA, 6, 7   # From Outmigration to -> others
), nrow = 4, ncol = 4, byrow = TRUE)

# Assign names for absolute clarity
rownames(tmat) <- colnames(tmat) <- state_names

# Print it to verify it looks correct
print(tmat)


# Align your models to match the structural matrix slot numbers
aligned_models <- list(
  fitted_models[["1"]], # Slot 1
  fitted_models[["2"]], # Slot 2
  fitted_models[["3"]], # Slot 3
  fitted_models[["4"]], # Slot 4
  fitted_models[["5"]], # Slot 5
  fitted_models[["6"]], # Slot 6
  fitted_models[["7"]]  # Slot 7
)

# 1. Define a specific covariate profile (Must match your model's variables exactly)
# Set global parameters
max_time <- 5
time_grid <- seq(0, max_time, by = 0.5)
colors_vector <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3")

#---------------------------------------------------------------------
# Helper Function: Simulates and plots state occupancy for a profile
#---------------------------------------------------------------------
run_and_plot_occupancy <- function(profile, plot_title) {
  set.seed(123) # Lock seed inside function for exact reproducibility
  
  # Run micro-simulation
  simulated_paths <- sim.fmsm(
    x       = aligned_models,
    trans   = tmat,
    t       = max_time,
    newdata = profile,
    start   = 2, # Enumeration
    M       = 50000,
    tidy    = FALSE
  )
  
  # Process proportions
  occupancy_matrix <- matrix(0, nrow = length(time_grid), ncol = ncol(tmat))
  
  for (i in seq_along(time_grid)) {
    t_val <- time_grid[i]
    
    current_states <- sapply(1:50000, function(id) {
      times <- simulated_paths$t[id, ]
      states <- simulated_paths$st[id, ]
      valid_idx <- which(times <= t_val)
      if (length(valid_idx) == 0) return(2) 
      return(states[max(valid_idx)])
    })
    
    state_counts <- table(factor(current_states, levels = 1:4))
    occupancy_matrix[i, ] <- state_counts / 50000
  }
  
  # Draw individual plot panel
  matplot(
    time_grid, 
    occupancy_matrix, 
    type = "l", 
    lty  = 1, 
    lwd  = 2,
    col  = colors_vector,
    main = plot_title,
    xlab = "Years in State",
    ylab = "Probability",
    ylim = c(0, 1),
    cex.main = 0.95
  )
}

#---------------------------------------------------------------------
# Configuration: Setup Grid Layout
#---------------------------------------------------------------------
# 2 Rows, 3 Columns Grid
# Left column: Residence | Middle column: Education | Right column: Age Groups
par(
  mfrow = c(3, 3),
  mar = c(2, 2, 3, 1), # Reduced individual margins to prevent crowd
  oma = c(4, 4, 4, 0), # Added 4 lines of space to Bottom (1) and Left (2)
  xpd = FALSE
)
par(
  mfrow = c(1, 3),
  mar   = c(2, 2, 3, 1),
  oma   = c(2, 0, 3, 0) # Bottom space reserved for global legend
)

#---------------------------------------------------------------------
# Pair 1: Urban vs. Rural (Holding Age=1, Education="Basic" constant)
#---------------------------------------------------------------------
profile_urban <- data.frame(age_group = 1, ResidenceArea = "Urban", EducationLevel = "Basic")
run_and_plot_occupancy(profile_urban, "Residence: Urban")

profile_rural <- data.frame(age_group = 1, ResidenceArea = "Rural", EducationLevel = "Basic")
run_and_plot_occupancy(profile_rural, "Residence: Rural")

#---------------------------------------------------------------------
# Pair 2: No Education vs. Tertiary (Holding Age=1, Residence="Urban")
#---------------------------------------------------------------------
# Note: Ensure "No education" and "Tertiary" match your exact data labels!
profile_no_edu <- data.frame(age_group = 1, ResidenceArea = "Rural", EducationLevel = "No Education")
run_and_plot_occupancy(profile_no_edu, "Education: None")

profile_tert_edu <- data.frame(age_group = 1, ResidenceArea = "Rural", EducationLevel = "Tertiary")
run_and_plot_occupancy(profile_tert_edu, "Education: Tertiary")

#---------------------------------------------------------------------
# Pair 3: Age Group 1 vs. Age Group 3 (Holding Residence="Urban", Education="Basic")
#---------------------------------------------------------------------
profile_age1 <- data.frame(age_group = 1, ResidenceArea = "Rural", EducationLevel = "Basic")
run_and_plot_occupancy(profile_age1, "Age Group: 1")

profile_age3 <- data.frame(age_group = 3, ResidenceArea = "Rural", EducationLevel = "Basic")
run_and_plot_occupancy(profile_age3, "Age Group: 3")

#---------------------------------------------------------------------
# Global Layout Adjustments (Adding Shared Legend & Title)
#---------------------------------------------------------------------
# Turn off inner clipping to draw in the outer margin space
par(xpd = TRUE)

# Global Figure Title
mtext(
  "Demographic Comparison of Predicted Semi-Markov State Occupancies", 
  side = 3, outer = TRUE, line = 0.5, cex = 1.2, font = 2
)

# Add single global legend centered at the absolute bottom
legend(
  x      = -4.2, # Adjust horizontally depending on graphics window size
  y      = -0.3, # Places it cleanly beneath the plot panels
  legend = state_names,
  col    = colors_vector,
  lty    = 1,
  lwd    = 3,
  horiz  = TRUE,
  bty    = "n",
  cex    = 1.1
)

# Reset plotting grid structure back to default
par(mfrow = c(1, 1), xpd = FALSE)

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# Global Simulation Parameters
max_time <- 2
time_grid <- seq(0, max_time, by = 0.5)

#---------------------------------------------------------------------
# 1. Reusable Extraction Function for Outmigration (State 3)
#---------------------------------------------------------------------
get_eom_probabilities <- function(profile) {
  set.seed(123) # Lock seed for simulation consistency
  
  simulated_paths <- sim.fmsm(
    x       = aligned_models,
    trans   = tmat,
    t       = max_time,
    newdata = profile,
    start   = 2, # Starts entirely in Enumeration
    M       = 50000,
    tidy    = FALSE
  )
  
  eom_probs <- numeric(length(time_grid))
  
  for (i in seq_along(time_grid)) {
    t_val <- time_grid[i]
    
    current_states <- sapply(1:50000, function(id) {
      times <- simulated_paths$t[id, ]
      states <- simulated_paths$st[id, ]
      valid_idx <- which(times <= t_val)
      if (length(valid_idx) == 0) return(2) 
      return(states[max(valid_idx)])
    })
    
    # Extract proportion in State 3 (EOM)
    eom_probs[i] <- sum(current_states == 3) / 50000
  }
  return(eom_probs)
}

#---------------------------------------------------------------------
# 2. Configure 1x3 Plot Layout Matrix with Room for Global Labels
#---------------------------------------------------------------------
par(
  mfrow = c(1, 3), 
  mar   = c(3, 3, 3.5, 1.5), # Standard space for axis ticks and subtitles
  oma   = c(4, 4, 4, 0),     # Extra bottom and left margin for global text
  cex.axis = 0.9
)












#================================================================== Descriptive ================================================
# Make sure you have already applied your clean-up logic for EducationLevel if needed
# e.g., df_smm <- df_smm %>% mutate(EducationLevel = case_when(...))

cat("====================================================================\n")
cat("          RUNNING MULTI-STATE COHORT PATHWAY ANALYSIS              \n")
cat("====================================================================\n\n")

# 1. Summarize each individual's complete history
cohort_summary <- df_smm %>%
  arrange(individualid, time1) %>% # Sort chronologically per person
  group_by(individualid) %>%
  summarise(
    # A. Identify how they entered the population (their very first 'from_state')
    entry_state = first(from_state),
    
    # B. Did they ever experience internal movement? 
    # (Checks if 'Internal movement' appears anywhere in their rows)
    had_internal = any(from_state == "Internal movement" | to_state == "Internal movement"),
    
    # C. Did they ever out-migrate?
    # (Checks if 'EOM' appears anywhere in their rows)
    had_outmigration = any(from_state == "EOM" | to_state == "EOM"),
    
    # D. Track total records per person
    total_records = n()
  ) %>%
  ungroup() %>%
  # Refine classifications into mutually exclusive pathways
  mutate(
    pathway = case_when(
      had_outmigration                   ~ "Outmigration Episode",
      had_internal & !had_outmigration   ~ "Internal Movement Only",
      TRUE                               ~ "No Subsequent Relocation"
    ),
    # Group entry states safely
    entry_group = case_when(
      entry_state %in% c("Enumeration", "Birth")          ~ "Born/Enumerated in DSA",
      entry_state == "EIM"                                ~ "In-migrants",
      is.na(entry_state) | trimws(entry_state) == ""      ~ "Other/Unknown Entry",
      TRUE                                                ~ "Other/Unknown Entry"
    )
  )

#=====================================================================
# 2. Compute and Display High-Level Cohort Entry Breakdown
#=====================================================================
cat("--- 1. OVERALL POPULATION ENTRY BREAKDOWN ---\n")
entry_table <- cohort_summary %>%
  count(entry_group) %>%
  mutate(percentage = round((n / sum(n)) * 100, 1))

print(entry_table)
cat("\n--------------------------------------------------------------------\n\n")

#=====================================================================
# 3. Pathway Sub-Analysis: Among Born or Enumerated
#=====================================================================
cat("--- 2. PATHWAYS AMONG BORN / ENUMERATED IN DSA ---\n")
born_enumerated_sub <- cohort_summary %>%
  filter(entry_group == "Born/Enumerated in DSA")

born_table <- born_enumerated_sub %>%
  count(pathway) %>%
  mutate(percentage = round((n / sum(n)) * 100, 1))

print(born_table)
cat("\n--------------------------------------------------------------------\n\n")

#=====================================================================
# 4. Pathway Sub-Analysis: Among In-Migrants
#=====================================================================
cat("--- 3. PATHWAYS AMONG IN-MIGRANTS ---\n")
inmigrant_sub <- cohort_summary %>%
  filter(entry_group == "In-migrants")

inmigrant_table <- inmigrant_sub %>%
  count(pathway) %>%
  mutate(percentage = round((n / sum(n)) * 100, 1))

print(inmigrant_table)
cat("\n--------------------------------------------------------------------\n\n")

#=====================================================================
# 5. Pathway Sub-Analysis: Among Other / Unknown Entries
#=====================================================================
cat("--- 4. PATHWAYS AMONG OTHER / UNKNOWN ENTRIES ---\n")
unknown_sub <- cohort_summary %>%
  filter(entry_group == "Other/Unknown Entry")

# Check if there are actually any individuals in this group to avoid division errors
if (nrow(unknown_sub) > 0) {
  unknown_table <- unknown_sub %>%
    count(pathway) %>%
    mutate(percentage = round((n / sum(n)) * 100, 1))
  print(unknown_table)
} else {
  cat("No individuals found with an Other/Unknown entry state.\n")
}
cat("\n====================================================================\n")
#==============================================================================


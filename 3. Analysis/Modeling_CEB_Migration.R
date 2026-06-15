
# ============================================================= Loading libraries ================================================
pacman::p_load(tidyverse, MASS, pscl, Metrics,performance, broom.helpers,
               gtsummary,flextable,officer,knitr, kableExtra,patchwork, scales )

# =========================================================
# 1. FACTOR SPECIFICATION (REFERENCE LEVELS)
# ========================================================

df_modeling <- df_modeling %>%
  mutate(
    migration_status = factor(
      migration_status,
      levels = c("Never migrated", "Internal only", "Ever external")
    ),
    
    Maritalstatus = factor(
      Maritalstatus,
      levels = c("Single/Relationship not recorded",
                 "Married",
                 "Separated/Divorced",
                 "Widowed")
    ),
    
    ResidenceArea = factor(
      ResidenceArea,
      levels = c("Rural", "Urban")
    ),
    
    EducationLevel = factor(
      EducationLevel,
      levels = c("No education",
                 "Informal education",
                 "Basic",
                 "Secondary",
                 "Tertiary")
    )
  )



# =========================================================
# 2. POISSON MODEL
# =========================================================

m_pois <- glm(
  NumberOfChildrenEverBorn ~ age_group + migration_status +
    EducationLevel + Maritalstatus + ResidenceArea,
  family = poisson(link = "log"),
  data = df_modeling
)

summary(m_pois)

# =========================================================
# 3. OVERDISPERSION CHECK
# =========================================================

overdispersion_ratio <- sum(residuals(m_pois, type = "pearson")^2) /
  df.residual(m_pois)

overdispersion_ratio

# =========================================================
# 4. DESCRIPTIVE OVERDISPERSION TABLE
# =========================================================

count_col <- df_modeling$NumberOfChildrenEverBorn %>% na.omit()

overdispersion_table <- tibble(
  Variable = "NumberOfChildrenEverBorn",
  Observations = length(count_col),
  Mean = mean(count_col),
  Variance = var(count_col),
  Dispersion_Ratio = var(count_col) / mean(count_col)
) %>%
  mutate(
    Mean = round(Mean, 2),
    Variance = round(Variance, 2),
    Dispersion_Ratio = round(Dispersion_Ratio, 2)
  )

# =========================================================
# 5. EXPORT TABLE TO WORD
# =========================================================

ft <- flextable(overdispersion_table) %>%
  autofit()

doc <- read_docx() %>%
  body_add_par("Overdispersion Summary: Children Ever Born", style = "heading 1") %>%
  body_add_flextable(ft)

print(
  doc,
  target = file.path(output_Dir, "Overdispersion_Summary.docx")
)

# ==================== NEGATIVE BINOMIAL MODEL ====================

# all covariates model
ceb_covars_nb <- glm.nb(NumberOfChildrenEverBorn ~ age_group + migration_status + 
                          Maritalstatus + EducationLevel + ResidenceArea,
                        data = df_modeling)
summary(ceb_covars_nb)

# ==================== ZERO-INFLATED NEGATIVE BINOMIAL REGRESSION ====================


# all covariates 
zinb_model <- zeroinfl(NumberOfChildrenEverBorn ~ age_group + migration_status + 
                         Maritalstatus + EducationLevel + ResidenceArea,
                       data = df_modeling, dist = "negbin")
summary(zinb_model)

# ==================== HURDLE MODEL ====================
# Negative binomial hurdle model (often better for overdispersed count data)
hurdle_nb <- hurdle(NumberOfChildrenEverBorn ~ age_group + migration_status + 
                      Maritalstatus + EducationLevel + ResidenceArea,
                    data = df_modeling,
                    dist = "negbin",
                    zero.dist = "binomial")

summary(hurdle_nb)


# ==================== MODEL COMPARISON ====================
# Model comparison statistics
# =========================================================

extract_fit_stats <- function(model, model_name, data, response) {
  
  pred <- predict(model, type = "response")
  
  tibble(
    Model = model_name,
    LogLikelihood = as.numeric(logLik(model)),
    AIC = AIC(model),
    BIC = BIC(model),
   R2 = pscl::pR2(model)[["McFadden"]],
    MAE = Metrics::mae(data[[response]], pred)
  )
}

model_comparison <- bind_rows(
  extract_fit_stats(
    ceb_covars_nb,
    "Negative Binomial",
    df_modeling,
    "NumberOfChildrenEverBorn"
  ),
  extract_fit_stats(
    zinb_model,
    "Zero-Inflated NB",
    df_modeling,
    "NumberOfChildrenEverBorn"
  ),
  extract_fit_stats(
    hurdle_nb,
    "Hurdle NB",
    df_modeling,
    "NumberOfChildrenEverBorn"
  )
) %>%
  mutate(
    LogLikelihood = round(LogLikelihood, 2),
    AIC = round(AIC, 2),
    BIC = round(BIC, 2),
    R2 = round(R2,2),
    MAE = round(MAE, 3)
  ) %>%
  arrange(AIC)

model_comparison

ft <- flextable(model_comparison) %>%
  theme_booktabs() %>%
  autofit()

doc <- read_docx() %>%
  body_add_par(
    "Model Comparison Statistics",
    style = "heading 1"
  ) %>%
  body_add_flextable(ft)

print(
  doc,
  target = file.path(output_Dir, "Model_Comparison.docx")
)

# ==============================================================================
# OBSERVED VS PREDICTED DISTRIBUTION OF CHILDREN EVER BORN
# ==============================================================================

# Observed distribution

max_children <- max(df_modeling[[response_var]], na.rm = TRUE)

observed_distribution <- df_modeling %>%
  count(NumberOfChildrenEverBorn) %>%
  mutate(
    Proportion = n / sum(n),
    Model      = "Observed"
  ) %>%
  dplyr::select(
    NumberOfChildrenEverBorn,
    Proportion,
    Model
  )

# ------------------------------------------------------------------------------
# Function: Predicted distribution for Negative Binomial model
# ------------------------------------------------------------------------------

get_nb_distribution <- function(model, model_name) {
  
  mu_hat <- predict(model, type = "response")
  
  probs <- sapply(
    0:max_children,
    function(k) {
      mean(
        dnbinom(
          x    = k,
          mu   = mu_hat,
          size = model$theta
        )
      )
    }
  )
  
  tibble(
    NumberOfChildrenEverBorn = 0:max_children,
    Proportion = probs,
    Model      = model_name
  )
}

# ------------------------------------------------------------------------------
# Predicted distributions
# ------------------------------------------------------------------------------

pred_nb_distribution <- get_nb_distribution(
  ceb_covars_nb,
  "Negative Binomial"
)

pred_zinb_distribution <- tibble(
  NumberOfChildrenEverBorn = 0:max_children,
  Proportion = colMeans(
    predict(zinb_model, type = "prob")
  ),
  Model = "Zero-Inflated NB"
)

pred_hurdle_distribution <- tibble(
  NumberOfChildrenEverBorn = 0:max_children,
  Proportion = colMeans(
    predict(hurdle_nb, type = "prob")
  ),
  Model = "Hurdle NB"
)

# ------------------------------------------------------------------------------
# Combine observed and predicted distributions
# ------------------------------------------------------------------------------

distribution_comparison <- bind_rows(
  observed_distribution,
  pred_nb_distribution,
  pred_zinb_distribution,
  pred_hurdle_distribution
)
# ==============================================================================
# PLOT 1: OBSERVED VS PREDICTED DISTRIBUTION
# ==============================================================================
distribution_plot <- ggplot(
  distribution_comparison,
  aes(
    x = NumberOfChildrenEverBorn,
    y = Proportion,
    colour = Model,
    shape = Model,
    group = Model
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.2) +
  scale_color_brewer(palette = "Set1") +
  labs(
    x = "Number of Children Ever Born",
    y = "Proportion"
  ) +
  coord_cartesian(xlim = c(0, 12)) +
  plot_fun()
# ==============================================================================
# PLOT 2: OBSERVED VS PREDICTED ZEROS
# ==============================================================================

zero_plot <- ggplot(
  zero_comparison_long,
  aes(x = Model, y = Rate, fill = Type)
) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  
  geom_text(
    aes(label = scales::percent(Rate, accuracy = 0.1)),
    position = position_dodge(0.8),
    vjust = -0.4,
    size = 4,
    colour = "black"
  ) +
  
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.1))
  ) +
  
  labs(
    x = NULL,
    y = "Proportion of Zeros (%)"
  ) +
  
  plot_fun()
# ==============================================================================
# COMBINED FIGURE
# ==============================================================================

combined_plot <- zero_plot + distribution_plot +
  patchwork::plot_layout(widths = c(1, 1)) &
  plot_fun()

# SAVE FIGURE

save_fun(combined_plot, "Observed_vs_Predicted_Model_Fit.png", 12, 6)
save_fun(combined_plot, "Observed_vs_Predicted_Model_Fit.tiff", 12, 6)


# ==============================================================================
# ZINB Model results, quick reporting using gt summary table
#===============================================================================
zinb_table <- tbl_regression(
  zinb_model,
  exponentiate = TRUE,
  intercept = TRUE,
  tidy_fun = broom.helpers::tidy_zeroinfl
)

zinb_flex <- as_flex_table(zinb_table)

# ---------------------------
# Extract tidy model output
# ---------------------------
zinb_tidy <- broom.helpers::tidy_zeroinfl(zinb_model) %>%
  mutate(
    IRR = exp(estimate),
    p.value = signif(p.value, 3)
  )

# ---------------------------
#Split model components
# ---------------------------

count_results <- zinb_tidy %>%
  filter(component == "conditional") %>%
  transmute(
    term,
    IRR = round(IRR, 3),
    p_count = format.pval(p.value, digits = 3, eps = 0.001)
  )

zero_results <- zinb_tidy %>%
  filter(component == "zero_inflated") %>%
  transmute(
    term,
    OR = round(IRR, 3),
    p_zero = format.pval(p.value, digits = 3, eps = 0.001)
  )

# ---------------------------
# Merge results
# ---------------------------
final_table <- full_join(count_results, zero_results, by = "term") %>%
  mutate(
    block = case_when(
      grepl("age_group", term)        ~ "Age group",
      grepl("migration_status", term) ~ "Migration status",
      grepl("EducationLevel", term)        ~ "Education level",
      grepl("Maritalstatus", term)    ~ "Marital status",
      grepl("ResidenceArea", term)    ~ "Residence area",
      TRUE                            ~ "Intercept"
    ),
    
    # clean variable names
    term = term %>%
      gsub("age_group", "", .) %>%
      gsub("migration_status", "", .) %>%
      gsub("EducationLevel", "", .) %>%
      gsub("Maritalstatus", "", .) %>%
      gsub("ResidenceArea", "", .)
  )

# ---------------------------
# Create flextable and save
# ---------------------------
ft <- flextable(final_table) %>%
  autofit() %>%
  theme_booktabs() %>%
  set_caption("Zero-Inflated Negative Binomial Model Results")

doc <- read_docx() %>%
  body_add_par("ZINB Model Results", style = "heading 1") %>%
  body_add_flextable(ft)

print(doc, target = file.path(output_Dir," zinb_results.docx"))


#=================================================================== END ==================================================
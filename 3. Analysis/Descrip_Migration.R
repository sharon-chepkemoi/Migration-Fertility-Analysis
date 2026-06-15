#-------------------------------- load packages -------------------------------------------------------------

if(!require(pacman))install.packages("pacman")
pacman::p_load(haven, openxlsx,readxl, dplyr, lubridate, stringr, ggplot2, gtsummary, 
               gt, tidyr, forcats, purrr, survival)

#--------------------------------- Load modeling  data ----------------------------------------------------

df <- readRDS("Data/msm_data3.rds")


# ==============================================================================
# POPULATION DYNAMICS AND MIGRATION RATES
# ==============================================================================

# ---------------------------------------------------------
# 1. Baseline population (2006)
# ---------------------------------------------------------

baseline_population <- df %>%
  mutate(event_year = lubridate::year(as.Date(event_date))) %>%
  filter(event_year == 2006) %>%
  distinct(individualid) %>%
  nrow()

# ---------------------------------------------------------
# 2. Annual demographic events
# ---------------------------------------------------------

annual_events <- df %>%
  mutate(event_year = lubridate::year(as.Date(event_date))) %>%
  group_by(event_year, event_type) %>%
  summarise(
    n_individuals = n_distinct(individualid),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from  = event_type,
    values_from = n_individuals,
    values_fill = 0
  ) %>%
  transmute(
    event_year,
    births      = Birth,
    immigrants  = EIM,
    emigrants   = EOM + Birth_Outmigration
  ) %>%
  arrange(event_year)

# ---------------------------------------------------------
# 3. Estimate annual population size
# ---------------------------------------------------------

population_data <- annual_events %>%
  mutate(population = NA_real_)

baseline_row <- which(population_data$event_year == 2006)

population_data$population[baseline_row] <- baseline_population

for (i in (baseline_row + 1):nrow(population_data)) {
  
  population_data$population[i] <-
    population_data$population[i - 1] +
    population_data$births[i] +
    population_data$immigrants[i] -
    population_data$emigrants[i]
}

population_data <- population_data %>%
  filter(event_year >= 2006)

# ---------------------------------------------------------
# 4. Calculate migration rates
# ---------------------------------------------------------

migration_rates <- population_data %>%
  mutate(
    Inmigration_Rate  = (immigrants / population) * 1000,
    Outmigration_Rate = (emigrants / population) * 1000,
    Net_Migration_Rate =
      ((immigrants - emigrants) / population) * 1000
  ) %>%
  dplyr::select(
    event_year,
    Inmigration_Rate,
    Outmigration_Rate,
    Net_Migration_Rate
  ) %>%
  pivot_longer(
    cols = -event_year,
    names_to = "Rate_Type",
    values_to = "Rate"
  ) %>%
  filter(event_year > 2006, event_year < 2025)

# =========================================================
# FIGURE 1: POPULATION SIZE OVER TIME
# =========================================================

population_plot <- ggplot(
  population_data,
  aes(x = event_year, y = population)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_y_continuous(
    limits = c(20000, 60000),
    breaks = seq(20000, 60000, by = 10000)
  ) +
  scale_x_continuous(
    breaks = seq(2006, 2025, by = 3)
  ) +
  labs(
    title = "Annual Population Size",
    x = "Year",
    y = "Population"
  ) +
  theme_plot()

# =========================================================
# FIGURE 2: MIGRATION RATES OVER TIME
# =========================================================

migration_plot <- ggplot(
  migration_rates,
  aes(
    x = event_year,
    y = Rate,
    colour = Rate_Type
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_x_continuous(
    breaks = seq(2007, 2024, by = 2)
  ) +
  labs(
    title = "Migration Rates",
    x = "Year",
    y = "Rate per 1,000 Women",
    colour = NULL
  ) +
  theme_plot()

# =========================================================
# COMBINED FIGURE
# =========================================================

combined_plot <- population_plot | migration_plot

#save plot
save_plot(combined_plot, "Population_dynamics.png", 8,4)
save_plot(combined_plot, "Population_dynamics.tiff",8,4)


# ==============================================================================
# FOLLOW-UP DISTRIBUTION PER INDIVIDUAL
# ==============================================================================

followup_distribution <- df %>%
  count(individualid, name = "n_followups") %>%
  count(n_followups, name = "Frequency") %>%
  mutate(
    Proportion = Frequency / sum(Frequency)
  )

follow_up_plot <- ggplot(
  followup_distribution,
  aes(x = n_followups, y = Proportion * 100)
) +
  geom_col(fill = "steelblue") +
  geom_text(
    aes(
      label = paste0(
        Frequency,
        " (",
        scales::percent(Proportion, accuracy = 0.1),
        ")"
      )
    ),
    vjust = -0.3,
    size = 3.5
  ) +
  scale_x_continuous(
    breaks = seq(
      min(followup_distribution$n_followups),
      max(followup_distribution$n_followups),
      by = 1
    ),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  scale_y_continuous(
    limits = c(0, 80),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Number of Follow-ups",
    y = "Proportion (%)"
  ) +
  theme_plot()

follow_up_plot


# =========================================================
# COMBINED FIGURE
# =========================================================

#save plot
save_plot(follow_up_plot, "follow_up_plot.png", 6,4)
save_plot(follow_up_plot, "follow_up_plot.tiff",6,4)




# =============================================================================
# 12. TRANSITION DIAGRAM (FIGURE 3)
# =============================================================================

state_names_diag <- c("Baseline resident", "Internal movement", "In-migration", "Out-migration")

# Sequential transition numbers for diagram labels (must match tmat_sim)
seq_matrix <- matrix(0, nrow = 4, ncol = 4,
                     dimnames = list(state_names_diag, state_names_diag))

seq_matrix["Baseline resident", "Internal movement"] <- 1
seq_matrix["Baseline resident", "Out-migration"]     <- 2
seq_matrix["In-migration",      "Internal movement"] <- 3
seq_matrix["Internal movement", "Internal movement"] <- 4
seq_matrix["Internal movement", "Out-migration"]     <- 5
seq_matrix["In-migration",      "Out-migration"]     <- 6
seq_matrix["Out-migration",     "In-migration"]      <- 7

png(
  file.path(diagram_dir, "transition_diagram.png"),
  width = 14, height = 10, units = "in", res = 600
)

par(font = 2, cex = 1.5, mar = c(4, 4, 6, 4), xpd = TRUE)

plotmat(
  t(seq_matrix),
  name      = paste0(state_names_diag, " (", 1:4, ")"),
  box.type  = "square",
  box.prop  = 0.4,
  box.size  = 0.12,
  box.col   = c("#A6CEE3", "#B2DF8A", "#FDBF6F", "#FB9A99"),
  arr.length = 0.25,
  arr.width  = 0.25,
  self.cex   = 0.8,
  self.shifty = 0.06,
  lwd       = 2,
  box.lwd   = 2,
  cex.txt   = 1.2,
  txt.font  = 2,
  cex       = 1.2,
  main      = "Migration Transition Diagram"
)

dev.off()

cat("Figure 3 saved:", file.path(diagram_dir, "transition_diagram.png"), "\n")

# Print transition mapping for reference
cat("\n--- Transition number mapping ---\n")
for (i in seq_len(nrow(seq_matrix))) {
  for (j in seq_len(ncol(seq_matrix))) {
    if (seq_matrix[i, j] > 0) {
      cat(sprintf("  Transition %d: %s -> %s\n",
                  seq_matrix[i, j], state_names_diag[i], state_names_diag[j]))
    }
  }
}


#========================================================================= END =======================


















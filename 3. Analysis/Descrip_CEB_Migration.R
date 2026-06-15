
if(!require(pacman))install.packages("pacman")
pacman::p_load(haven, openxlsx,readxl, dplyr, lubridate, stringr, ggplot2, gtsummary,  gt, tidyr, forcats, purrr, survival,msm,patchwork)

# =========================================== LOAD DATA========================================================



df <- readRDS(file.path(output_Dir, "Cleaned_Basse_Gambia_dataset.rds"))


# ===============================================================================================================
# 1. DATA RECODING & FEATURE CREATION
# ================================================================================================================

df <- df %>%
  mutate(
    across(
      c(
        `Age(yrs)`,
        AgeAtPartnership,
        NumberOfChildrenEverBorn,
        AverageBirthSpacingMonths
      ),
      as.numeric
    )
  ) %>%
  rename(age = `Age(yrs)`)

overall_median <- median(df$AgeAtPartnership, na.rm = TRUE)

df <- df %>%
  mutate(
    # Residence area
    ResidenceArea = forcats::fct_recode(
      ResidenceArea,
      "Urban" = "Pre-Urban"
    ),
    
    # Age groups
    age_group = cut(
      age,
      breaks = c(15, 24, 34, 49),
      labels = c("15–24", "25–34", "35–49"),
      include.lowest = TRUE
    ),
    
    # Education categories
    EducationLevel = case_when(
      EducationLevel %in% c("NO", "OT") | is.na(EducationLevel) ~ "No education",
      EducationLevel %in% c("MA", "QE", "SN") ~ "Informal education",
      EducationLevel %in% c("LB", "UB") ~ "Basic",
      EducationLevel %in% c("JS", "SS") ~ "Secondary",
      EducationLevel %in% c("CU", "VO") ~ "Tertiary",
      TRUE ~ EducationLevel
    ),
    
    EducationLevel = factor(
      EducationLevel,
      levels = c(
        "No education",
        "Informal education",
        "Basic",
        "Secondary",
        "Tertiary"
      )
    ),
    
    # Marital status categories
    Maritalstatus = case_when(
      Maritalstatus %in% c("Death", "Widowed") ~ "Widowed",
      Maritalstatus %in% c("Separated", "Divorce") ~ "Separated/Divorced",
      TRUE ~ Maritalstatus
    ),
    
    # Replace implausible partnership ages
    AgeAtPartnership = if_else(
      AgeAtPartnership < 10,
      overall_median,
      AgeAtPartnership
    )
  )

# =========================================================
# 2. MIGRATION STATUS CONSTRUCTION
# =========================================================

migration_status_df <- df %>%
  group_by(individualid) %>%
  summarise(
    born_enumerated = any(event_type %in% c("Birth", "Enumeration")),
    ever_internal   = any(event_type %in% c("IIM", "IOM")),
    ever_external   = any(event_type %in% c("EIM", "EOM", "Birth_Outmigration")),
    .groups = "drop"
  ) %>%
  mutate(
    migration_status = case_when(
      ever_external ~ "Ever external",
      ever_internal & !ever_external ~ "Internal only",
      born_enumerated & !ever_internal & !ever_external ~ "Never migrated",
      TRUE ~ "Other"
    )
  ) %>%
  distinct(individualid, .keep_all = TRUE) %>%
  dplyr::select(individualid, migration_status)


df_with_status <- df %>%
  mutate(individualid = as.character(individualid)) %>%
  left_join(
    migration_status_df %>%
      mutate(individualid = as.character(individualid)),
    by = "individualid"
  )


# =========================================================
# 3. KEEP ONE OBSERVATION PER INDIVIDUAL
# =========================================================

df_one <- df_with_status %>%
  arrange(individualid)%>%
  group_by(individualid)%>%
  dplyr::slice(1) %>%
  ungroup()


# =========================================================
# 4. MODELING DATASET
# =========================================================

model_vars <- c(
  "NumberOfChildrenEverBorn",
  "age",
  "AgeAtPartnership",
  "age_group",
  "ResidenceArea",
  "EducationLevel",
  "Maritalstatus",
  "migration_status"
)

df_modeling <- df_one %>%
  dplyr::select(all_of(model_vars))


# =========================================================
# 5. DESCRIPTIVE TABLE (gtsummary → flextable → Word)
# =========================================================

table1 <- df_modeling %>%
  tbl_summary(
    by = migration_status,
    statistic = list(
      all_continuous() ~ "{mean} ({sd}) | {median} ({p25}, {p75})",
      NumberOfChildrenEverBorn ~ "{mean} ({sd})",
      age ~ "{mean} ({sd}) | {median} ({p25}, {p75})",
      AgeAtPartnership ~ "{median} ({p25}, {p75})"
    ),
    missing = "ifany"
  ) %>%
  add_overall() %>%
  add_p(
    test = list(
      all_continuous() ~ "kruskal.test",
      all_categorical() ~ "chisq.test"
    )
  ) %>%
  modify_header(label ~ "Characteristic") %>%
  bold_labels()

ft <- as_flex_table(table1)

doc <- read_docx() %>%
  body_add_par(
    "Descriptive Table: Migration and Socio-demographic Characteristics",
    style = "heading 1"
  ) %>%
  body_add_flextable(ft)

print(doc, target = file.path(output_Dir, "Descriptive_Table_Migration.docx"))


# =========================================================
# 6. CHILDREN EVER BORN (CEB) PREPARATION
# =========================================================

ceb_summary <- df_modeling %>%
  summarise(
    mean_ceb = mean(NumberOfChildrenEverBorn, na.rm = TRUE),
    var_ceb  = var(NumberOfChildrenEverBorn, na.rm = TRUE),
    prop_zero = mean(NumberOfChildrenEverBorn == 0, na.rm = TRUE)
  )


# =========================================================
# 7. CEB GROUPING
# =========================================================

df_modeling <- df_modeling %>%
  mutate(
    ceb_group = case_when(
      NumberOfChildrenEverBorn == 0 ~ "0",
      NumberOfChildrenEverBorn <= 3 ~ "1-3",
      NumberOfChildrenEverBorn <= 6 ~ "4-6",
      TRUE ~ "7+"
    ),
    ceb_group = factor(ceb_group, levels = c("0", "1-3", "4-6", "7+"))
  )


# =========================================================
# 8. DISTRIBUTION PLOT (OVERALL)
# =========================================================

count1 <- df_modeling %>%
  count(ceb_group) %>%
  mutate(percent = 100 * n / sum(n))

p1 <- ggplot(count1, aes(x = ceb_group, y = percent)) +
  geom_col(fill = "#2C7FB8") +
  geom_text(aes(label = sprintf("%.1f%%", percent)), vjust = -0.4, size = 4) +
  labs(
    title = "Overall Distribution of Children Ever Born",
    x = "Children Ever Born",
    y = "Proportion (%)"
  ) +
  theme_plot()


# =========================================================
# 9. CEB BY AGE GROUP
# =========================================================

count2 <- df_modeling %>%
  count(age_group, ceb_group) %>%
  group_by(age_group) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  ungroup()

p2 <- ggplot(count2, aes(x = ceb_group, y = percent, fill = age_group)) +
  geom_col(position = position_dodge(0.85)) +
  geom_text(
    aes(label = sprintf("%.1f%%", percent)),
    position = position_dodge(0.85),
    vjust = -0.3,
    size = 4
  ) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Children Ever Born by Age Group",
    x = "Children Ever Born",
    y = "Proportion (%)",
    fill = "Age Group"
  ) +
  theme_plot()


comb <- p1 + p2 +
  patchwork::plot_layout(widths = c(1, 1.5))

save_plot(comb, "CEB_combined_plot.png", 12, 6)
save_plot(comb, "CEB_combined_plot.tiff", 12, 6)


# =========================================================
# 10. MEAN CEB BY MIGRATION STATUS
# =========================================================

means <- df_modeling %>%
  group_by(migration_status) %>%
  summarise(
    n = n(),
    mean = mean(NumberOfChildrenEverBorn, na.rm = TRUE),
    se = sd(NumberOfChildrenEverBorn, na.rm = TRUE) / sqrt(n),
    lower = mean - 1.96 * se,
    upper = mean + 1.96 * se,
    .groups = "drop"
  ) %>%
  mutate(
    migration_status = factor(
      migration_status,
      levels = c("Never migrated", "Internal only", "Ever external"),
      labels = c("Non-migrants", "Internal migrants", "External migrants")
    )
  )


mean_CEB <- ggplot(means, aes(x = migration_status, y = mean, fill = migration_status)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15) +
  labs(
    y = "Mean Children Ever Born (95% CI)",
    x = NULL
  ) +
  scale_fill_manual(values = c(
    "Non-migrants" = "#ef767a",
    "Internal migrants" = "#49beaa",
    "External migrants" = "#456990"
  )) +
  theme_plot() +
  theme(legend.position = "none")


save_plot(mean_CEB, "mean_CEB_migration.png")
save_plot(mean_CEB, "mean_CEB_migration.tiff")


#-------------------------------------------------------- END -----------------------------------------------

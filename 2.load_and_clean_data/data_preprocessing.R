
#----------------------------------------------------------- Load packages --------------------------------------------------

if(!require(pacman))install.packages("pacman")
pacman::p_load(haven, openxlsx,readxl, dplyr, lubridate, stringr, ggplot2, gtsummary,  gt, tidyr, forcats, purrr, DiagrammeR)

#-----------------------------------------------------------  Load clean data ----------------------------------------------------

df_migration <- readRDS(file.path(output_Dir, "Cleaned_MRC_Gambia_dataset.rds"))


#--------------------------------------------------------- Internal movement --------------------------------------------------------

df_migration <- df_migration %>%
  group_by(individualid, event_date) %>%
  mutate(
    # if both IIM and IOM present on this day, flag them
    has_IIM_IOM = all(c("IIM", "IOM") %in% event_type)
  ) %>%
  summarise(
    event_type = if (first(has_IIM_IOM)) {
      # collapse only IIM and IOM
      c("IIM,IOM", setdiff(unique(event_type), c("IIM", "IOM")))
    } else {
      unique(event_type)
    },
    across(-c(event_type, has_IIM_IOM), ~ first(.)),
    .groups = "drop"
  )

#rename IIM, IOM, TO internal movement
df_migration <- df_migration %>%
  mutate(event_type = case_when(
    event_type %in% c("IIM", "IOM", "IIM,IOM") ~ "Internal movement",
    TRUE ~ event_type
  ))

###############################################################################
#remove internal movements happening same day with EOM
###############################################################################

df_migration2 <- df_migration %>%
  group_by(individualid, event_date) %>%
  mutate(has_EOM = any(event_type == "EOM")) %>%
  filter(!(has_EOM & event_type == "Internal movement")) %>%
  dplyr::select(-has_EOM) %>%
  ungroup()

################################ 
#PULL ids with internal movements after EOM
#####################################

ids_EOM_IM <- df_migration2 %>%
  arrange(individualid, event_date) %>%
  group_by(individualid) %>%
  mutate(next_event = lead(event_type)) %>%
  filter(event_type == "EOM" & next_event == "Internal movement") %>%
  distinct(individualid) %>%
  pull(individualid)


###############################################################################
#Keep valid internal movements
################################################################################
df_migration3 <- df_migration2 %>%
  arrange(individualid, event_date) %>%
  group_by(individualid) %>%
  mutate(
    # Track the most recent EOM date per person
    last_EOM = ifelse(event_type == "EOM", event_date, as.Date(NA)),
    last_EOM = zoo::na.locf(last_EOM, na.rm = FALSE),  # carry forward
    # Track the most recent EIM date per person
    last_EIM = ifelse(event_type == "EIM", event_date, as.Date(NA)),
    last_EIM = zoo::na.locf(last_EIM, na.rm = FALSE)
  ) %>%
  # Keep everything, but drop Internal movement after EOM if no EIM has occurred after that EOM
  filter(!(event_type == "Internal movement" & !is.na(last_EOM) & (is.na(last_EIM) | last_EIM < last_EOM))) %>%
  dplyr::select(-last_EOM, -last_EIM) %>%
  ungroup()

# ################################################################################
# # pull ids with internal movement leading to EIM (NOT same day)
# ################################################################################
# 
# ids_IM_EIM <- df_migration %>%
#   mutate(event_date = as.Date(event_date)) %>%
#   arrange(individualid, event_date) %>%
#   group_by(individualid) %>%
#   mutate(
#     next_event = lead(event_type),
#     next_date  = lead(event_date)
#   ) %>%
#   filter(
#     event_type == "Internal movement",
#     next_event == "EIM",
#     next_date > event_date        # <-- exclude same-day
#   ) %>%
#   distinct(individualid) %>%
#   pull(individualid)
# 
# # remove ONLY the EIM records that follow internal movement
# df_migration <- df_migration %>%
#   filter(!(individualid %in% ids_IM_EIM & event_type == "EIM"))
# 
# ################################################################################
# # pull ids with internal movement leading to Enumeration (NOT same day)
# ################################################################################
# 
# ids_IM_ENU <- df_migration %>%
#   mutate(event_date = as.Date(event_date)) %>%
#   arrange(individualid, event_date) %>%
#   group_by(individualid) %>%
#   mutate(
#     next_event = lead(event_type),
#     next_date  = lead(event_date)
#   ) %>%
#   filter(
#     event_type == "Internal movement",
#     next_event == "Enumeration",
#     next_date > event_date        # <-- exclude same-day
#   ) %>%
#   distinct(individualid) %>%
#   pull(individualid)
# 
# # remove those individuals entirely (as per your original logic)
# df_migration <- df_migration %>%
#   filter(!(individualid %in% ids_IM_ENU))

#================================================================= modeling preparation ====================================


df_migration3 <- df_migration3 %>%
  mutate(event_type = case_when(event_type == "Birth_Outmigration" ~ "EOM",
                                # condition then value
                                TRUE ~ event_type               # keep existing value otherwise
  ))



df_migration <- df_migration3 %>%
  group_by(individualid) %>%
  arrange(event_date, .by_group = TRUE) %>%
  mutate(
    start = lag(event_date, default = first(event_date)),
    stop  = event_date,
    
    time1 = as.numeric(difftime(start, first(start), units = "days")) / 30.4375,
    time2 = as.numeric(difftime(stop,  first(start), units = "days")) / 30.4375,
    
    from_state = lag(event_type, default = NA_character_),
    to_state   = event_type
  ) %>%
  ungroup()

df_migration <- df_migration %>%
  group_by(individualid) %>%
  mutate(
    t = as.numeric(difftime(stop, first(start), units = "days"))/ 30.4375   # months since baseline
  ) %>%
  ungroup()

# Create status: first row per individual = censored (0),
#    intermediate rows that represent transitions = 1,
#    last row per individual = censored (0).



#numeric event indicator
df_migration <- df_migration %>%
  mutate(Event_1 = as.numeric(factor(event_type,
                                     levels = c("Birth", "Birth_Outmigration", "EIM", "Enumeration", "EOM", "Internal movement"))))

#===================================================================== Save data ================================================

saveRDS(df_migration,file.path(output_Dir, "msm_data.rds"))

        


#=================================================================== END ===================================





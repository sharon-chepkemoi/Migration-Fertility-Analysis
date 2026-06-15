
#-------------------------------------------------------- load packages -------------------------------------------------------------

if(!require(pacman))install.packages("pacman")
pacman::p_load(haven, openxlsx,readxl, dplyr, lubridate, stringr, ggplot2, gtsummary,  gt, tidyr, forcats, purrr)

#=========================================================  Read datasets =============================================================

dfs<- read_excel_allsheets('Data/Data.xlsx')

df_demographics <-  dfs[[1]]

inmigration_df <-  dfs[[2]]

outmigration_df <- dfs[[3]]

Enumeration_data <-  readxl::read_excel("Data/APHRC's request - Enum dates.xlsx")

#========================================================= Check length of IDs ==========================================================

#demographic and inmigration
common_ids_1 <- intersect(df_demographics$IndividualID, inmigration_df$individualid)

#demographic and outmigration
common_ids_2 <- intersect(df_demographics$IndividualID, outmigration_df$individualid)

#inmigration and outmigration
common_ids_3 <- intersect(inmigration_df$individualid, outmigration_df$individualid)

cat("Common IDs (Demographics vs Inmigration):", length(common_ids_1), "\n")
cat("Common IDs (Demographics vs Outmigration):", length(common_ids_2), "\n")
cat("Common IDs (Inmigration vs Outmigration):", length(common_ids_3), "\n")

#check if some ids are in outmigration and not in inmigration data 
missing_ids <- setdiff(outmigration_df$individualid, inmigration_df$individualid) # some ids exist left join wont work

#===================================================== merge migration datasets =====================================================================

# Prepare in-migration
in_mig_long <- inmigration_df %>%
  rename(event_date = start_date,
         event_origin = origin,
         event_type = migration_type,
         event_reason = reason) %>%
  mutate(migration_direction = "in")

# Prepare out-migration
out_mig_long <- outmigration_df %>%
  rename(event_date = start_date,
         event_destination = destination,
         event_type = migration_type,
         event_reason = reason) %>%
  mutate(migration_direction = "out")

# Combine by aligning columns
migration_long <- full_join(
  in_mig_long, out_mig_long,
  by = c("individualid", "event_date", "event_type", "event_reason")
) %>%
  mutate(
    migration_direction = coalesce(migration_direction.x, migration_direction.y),
    event_origin = event_origin,
    event_destination = event_destination
  ) %>%
  select(individualid, event_date, event_type, event_reason,
         migration_direction, event_origin, event_destination)


#=====================================================  merge migration and demographic datasets ===================================================
##########################
# check first if all ids in migration data exist in demographics, if so then left join migration information
##########################

df_demographics <- df_demographics %>%
  dplyr::rename(individualid = IndividualID) 

#check
missing <- setdiff(migration_long$individualid, df_demographics$individualid) 
missing_ids <- setdiff(df_demographics$individualid, migration_long$individualid) 
missing_df <- df_demographics %>%
  filter(individualid %in% missing_ids)

#merge data
df_Gambia <- dplyr:: left_join(df_demographics, migration_long, by = "individualid")

df_enum <- Enumeration_data %>%
  transmute(
    individualid,
    event_type = "Enumeration",
    event_date = enumerationDate,
    event_origin = "Enume"
  )

###########################
#check if all the ids in enumeration with dates are in that data
##############################
missing_ids <- setdiff(df_Gambia$individualid, df_enum$individualid) #553 ids in df_Gambia not in df_enu

##########################
#select ids from df_enu which are present in df_Gambia , then full join
##########################

df_enum_filtered <- df_enum %>%
  semi_join(df_Gambia, by = "individualid") %>%
  select(individualid, event_type, event_date, event_origin)

# Extract events from df_Gambia
df_gambia_events <- df_Gambia %>%
  select(individualid, event_type, event_date, event_reason, event_origin, event_destination)

# Stack the events together
all_events <- bind_rows(df_gambia_events, df_enum_filtered) %>%
  arrange(individualid, event_type, event_date, event_reason, event_origin, event_destination)


#Join back other variables from df_Gambia
final_df <- all_events %>%
  left_join(
    df_Gambia %>% 
      select(-all_of(c("event_type", "event_date", "event_reason", "event_origin", "event_destination"))) %>% 
      distinct(individualid, .keep_all = TRUE), 
    by = "individualid"
  )


#=================================================== Manipulations ==============================================================

#############################################
# when date of birth = event date, event = birth
#################################################

df_MRC <- final_df %>%
  mutate(event_type = case_when(
    DateofBirth == event_date  ~ "Birth",
    TRUE                                    ~ event_type   # keep existing if neither
  ))

# update missing event type 

df_MRC <- df_MRC %>%
  mutate(
    event_type = if_else(is.na(event_type), "EIM/ENU", event_type),
    event_date = if_else(is.na(event_date), startdate, event_date)
  )

#===================================================== Clean migration histories ===========================================

#order dates and events
df_Gambia_clean <- df_MRC %>%
  group_by(individualid, event_date) %>%
  arrange(individualid, event_date,
          factor(event_type, levels = c("Enumeration","EIM/ENU","Birth","EIM","IOM","IIM" ,"EOM"))) %>%
  ungroup()

#Remove cases where the event date comes before birth date
df <- df_Gambia_clean %>%
  filter(event_date >= DateofBirth)


##########################################
#remove cases EOM and EIM happening the same date (Assume that no event happened that day (from HDSS specialist))
#########################################

df2 <- df %>%
  group_by(individualid, event_date) %>%
  filter(!(any(event_type == "EOM") & any(event_type == "EIM"))) %>%
  ungroup()


###############################################################################
# If EIM/ENU and Enumeration are happening for an id, keep that comes first
###############################################################################
df3 <- df2 %>%
  group_by(individualid) %>%
  arrange(event_date, .by_group = TRUE) %>%
  filter(
    # keep everything that is not EIM/ENU or Enumeration
    !(event_type %in% c("EIM/ENU", "Enumeration")) |
      # if it's EIM/ENU or Enumeration, keep only the first such event
      row_number() == which(event_type %in% c("EIM/ENU", "Enumeration"))[1]
  ) %>%
  ungroup()

################################################################################
# if EIM and Enumeration are following each other, keep that comes first
################################################################################

df4<- df3 %>%
  arrange(individualid, event_date) %>%
  group_by(individualid) %>%
  mutate(row_num = row_number()) %>%
  # only check the first two events
  mutate(
    drop_flag = case_when(
      row_num <= 2 &
        all(c("EIM", "Enumeration") %in% event_type[1:2]) &
        event_date == max(event_date[1:2]) ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  filter(!drop_flag) %>%
  select(-row_num, -drop_flag) %>%
  ungroup()


################################################################################
#If the event is EIM/ENU and event_year is 2006, then put Enumeration otherwise leave the way it is
################################################################################

df5 <- df4 %>%
  mutate(
    event_type = case_when(
      event_type == "EIM/ENU" & lubridate::year(event_date) == 2006 ~ "Enumeration",
      TRUE ~ event_type
    )
  )

################################################################################
# Cases where origin is ENU, change that to Enumeration
################################################################################

df6 <- df5 %>%
  mutate(
    event_type = case_when(
      event_origin == "ENU" ~ "Enumeration",
      TRUE ~ as.character(event_type)
    )
  )

################################################################################
# If Enumeration is not the first, remove that enumeration record from that individual
################################################################################
df7 <- df6 %>%
  arrange(individualid, event_date) %>%
  group_by(individualid) %>%
  mutate(first_event = first(event_type)) %>%
  filter(!(event_type == "Enumeration" & first_event != "Enumeration")) %>%
  select(-first_event) %>%
  ungroup()

################################################################################
#if EOM destination is starting with 80 then change EOM to IOM,
#if EIM and origin start with 80, change  EIM TO IIM
################################################################################

df8 <- df7 %>%
  mutate(
    event_type = case_when(
      event_type == "EOM" & str_starts(event_destination, "80") ~ "IOM",
      event_type == "EIM" & str_starts(event_origin, "80")      ~ "IIM",
      TRUE                                               ~ event_type
    )
  )

################################################################################
#If EOM is followed by IIM and origin and destination start with 80, then replace EOM with IOM (801210103010)
################################################################################

df9 <- df8%>%
  arrange(individualid, event_date) %>%  # make sure sorted by person and time
  group_by(individualid) %>%
  mutate(
    next_event = lead(event_type),
    next_origin = lead(event_origin),
    event_type = ifelse(
      event_type == "EOM" &
        next_event == "IIM" &
        grepl("^80", event_destination) &
        grepl("^80", next_origin),
      "IOM",
      event_type
    )
  ) %>%
  select(-next_event, -next_origin) %>%
  ungroup()

################################################################################
#Remove duplicates entries, same date, same id, same event
################################################################################
df10 <- df9 %>%
  group_by(individualid, event_date, event_type) %>%  # check duplicates on these
  dplyr::slice(1) %>%                                       # keep only the first record
  ungroup()


################################################################################
#If we have EIM and then later followed by EIM not necessary be next in series
#without EOM then take fisr EIM
################################################################################

df11 <- df10 %>%
  arrange(individualid, event_date) %>%
  group_by(individualid) %>%
  mutate(
    block = cumsum(event_type == "EOM")  # new block after every EOM
  ) %>%
  group_by(individualid, block) %>%
  mutate(
    eim_index = ifelse(event_type == "EIM", cumsum(event_type == "EIM"), NA),
    keep = ifelse(event_type == "EIM" & eim_index > 1, FALSE, TRUE)  # only first EIM kept
  ) %>%
  ungroup() %>%
  filter(keep) %>%
  select(-block, -eim_index, -keep)


################################################################################
#EOM followed by EOM take first one
################################################################################
df12 <- df11 %>%
  arrange(individualid, event_date) %>%
  group_by(individualid) %>%
  mutate(
    # Create a "block" that resets whenever EIM appears
    block = cumsum(event_type == "EIM")
  ) %>%
  group_by(individualid, block) %>%
  mutate(
    eom_index = ifelse(event_type == "EOM", cumsum(event_type == "EOM"), NA),
    keep = ifelse(event_type == "EOM" & eom_index > 1, FALSE, TRUE)  # keep only first EOM
  ) %>%
  ungroup() %>%
  filter(keep) %>%
  select(-block, -eom_index, -keep)

################################################################################
#Outmigrants at birth
################################################################################

df13 <- df12 %>%
  group_by(individualid) %>%
  arrange(event_date, .by_group = TRUE) %>%
  mutate(
    next_event_type = lead(event_type),
    next_event_date = lead(event_date),
    event_type = if_else(
      event_type == "Birth" &
        !is.na(next_event_type) &                     # make sure next_event_type exists
        next_event_type == "EIM" &
        !is.na(next_event_date) & next_event_date > event_date,
      "Birth_Outmigration",
      event_type
    )
  ) %>%
  select(-next_event_type, -next_event_date) %>%
  ungroup()


################################################################################
# For the invalid cases (first event is neither of Enumeration, Birth, EIM,Birth_outmigration),
#check the first event if it is EOM, and event_date 2006, then add Enumeration and event_date to (2006-05-01),
# if it is IIM OR IOM and later own followed by EIM without prior EOM, then remove the first entries of 
#IOM or IIM and let EIM be the first event. If first event is IIM OR IOM happening in 2006 without later EIM before EOM,
#then add Enumeration  (2006 - 05 -01)
################################################################################

invalid_ids <- df13 %>%
  group_by(individualid) %>%
  arrange(startdate, .by_group = TRUE) %>%
  slice(1) %>%   # first event per individual
  filter(!event_type %in% c("Enumeration", "Birth", "EIM","Birth_Outmigration")) %>%
  pull(individualid)

df_invalid <- df13 %>% filter(individualid %in% invalid_ids)
df_valid   <- df13 %>% filter(!individualid %in% invalid_ids)

# --- Step 2: apply rules only to invalid IDs ---

# Case A: First event = EOM in 2006
case_A <- df_invalid %>%
  group_by(individualid) %>%
  slice_head(n = 1) %>%
  filter(event_type == "EOM", year(event_date) == 2006) %>%
  mutate(event_type = "Enumeration", event_date = as.Date("2006-05-01"))



case_B_ids <- df_invalid %>%
  filter(event_type %in% c("IIM", "IOM", "EIM", "EOM")) %>%
  group_by(individualid) %>%
  arrange(event_date, .by_group = TRUE) %>%
  summarise(
    first_event = first(event_type),
    first_eim_date = suppressWarnings(min(event_date[event_type == "EIM"], na.rm = TRUE)),
    first_eom_date = suppressWarnings(min(event_date[event_type == "EOM"], na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  filter(
    first_event %in% c("IIM", "IOM"),              # starts with IIM/IOM
    !is.infinite(first_eim_date),                  # must have an EIM
    is.infinite(first_eom_date) | first_eim_date < first_eom_date  # EIM before any EOM
  ) %>%
  pull(individualid)

# --- Step 2: drop the leading IIM/IOM rows for those IDs ---
df_case_B <- df_invalid %>%
  group_by(individualid) %>%
  arrange(event_date, .by_group = TRUE) %>%
  mutate(first_event = first(event_type)) %>%
  filter(!(individualid %in% case_B_ids &
             row_number() == 1 &
             first_event %in% c("IIM", "IOM"))) %>%
  select(-first_event) %>%
  ungroup()

# Case C: First event = IIM/IOM in 2006, and no EIM before EOM
case_C <- df_invalid %>%
  group_by(individualid) %>%
  slice_head(n = 1) %>%
  filter(event_type %in% c("IIM", "IOM"), year(event_date) == 2006) %>%
  filter(!(individualid %in% case_B_ids)) %>%
  mutate(event_type = "Enumeration", event_date = as.Date("2006-05-01"))

dfs <- list(df_valid, df_case_B, case_A, case_C)

dfs <- lapply(dfs, function(d) {
  d %>% mutate(event_date = as.Date(event_date))
})

df_valid  <- dfs[[1]]
df_case_B <- dfs[[2]]
case_A    <- dfs[[3]]
case_C    <- dfs[[4]]

# --- Step 3: recombine ---
df_final <- bind_rows(
  df_valid,       # untouched valid IDs
  df_case_B,      # cleaned invalids (case B applied)
  case_A, case_C  # added Enumeration rows
) %>%
  arrange(individualid, event_date)


################################################################################
# Identify individuals with a valid first event
################################################################################

valid_first_ids <- df_final %>%
  group_by(individualid) %>%
  arrange(startdate, .by_group = TRUE) %>%
  slice(1) %>%                              # first event per individual
  filter(event_type %in% c("Enumeration", "Birth", "EIM","Birth_Outmigration")) %>%
  pull(individualid)                        # get the IDs

#  Keep all events for these individuals
df_filtered_all_events <- df_final %>%
  filter(individualid %in% valid_first_ids)


df_filtered <- df_filtered_all_events %>%
  group_by(individualid) %>%
  mutate(enum_count = sum(event_type == "Enumeration")) %>%
  filter(!(event_type == "Enumeration" & enum_count > 1 & event_origin != "ENU")) %>%
  select(-enum_count) %>%
  ungroup()

df_filtered2 <- df_filtered %>%
  group_by(individualid, event_date) %>%
  # If both Birth and Enumeration exist, remove Enumeration
  filter(!(any(event_type == "Birth") & event_type == "Enumeration")) %>%
  ungroup()


###########################################################################
# Check cases where birth and enumeration both exist in same id
#Check cases where Enumeration is followed by EIM OR Birth
#check cases where Birth is fo;;owed by EIM or Enumeration
#check cases where EIM is followed by Birth or Enumeration
# Check cases where EIM if followed by EIM or where EOM if followed by EOM
#################################################################################

df_checks <- df_filtered2 %>%
  arrange(individualid, event_date) %>%
  group_by(individualid) %>%
  mutate(next_event = lead(event_type)) %>%
  ungroup()

# Birth and Enumeration both exist in same id
case_birth_enum <- df_checks %>%
  group_by(individualid) %>%
  filter(any(event_type == "Birth") & any(event_type == "Enumeration")) %>%
  ungroup()

# Enumeration followed by EIM or Birth
case_enum_follow <- df_checks %>%
  filter(event_type == "Enumeration" & next_event %in% c("EIM", "Birth", "Birth_Outmigration"))

#Birth followed by EIM or Enumeration
case_birth_follow <- df_checks %>%
  filter(event_type == "Birth" & next_event %in% c("EIM", "Enumeration"))

#EIM followed by Birth or Enumeration
case_eim_follow <- df_checks %>%
  filter(event_type == "EIM" & next_event %in% c("Birth", "Enumeration"))

# EIM followed by EIM
case_eim_eim <- df_checks %>%
  filter(event_type == "EIM" & next_event == "EIM")

# 6. EOM followed by EOM
case_eom_eom <- df_checks %>%
  filter(event_type == "EOM" & next_event == "EOM")


################################################################################ 
#Remove Enumeration record if being followed by Birth, EIM, Birth-Outmigration
################################################################################

df_filtered3 <- df_filtered2 %>%
  group_by(individualid) %>%
  arrange(event_date, .by_group = TRUE) %>%       # order events chronologically
  mutate(
    next_event = lead(event_type)                 # look ahead to next event
  ) %>%
  filter(
    !(
      event_type == "Enumeration" &
        next_event %in% c("Birth", "EIM", "Birth_Outmigration")
    )
  ) %>%
  ungroup()



df_filtered4 <- df_filtered3 %>%
  arrange(individualid, event_date) %>%
  group_by(individualid) %>%
  mutate(next_event = lead(event_type)) %>%
  ungroup() %>%
  group_by(individualid) %>%
  arrange(event_date, .by_group = TRUE) %>%
  mutate(
    event_type = case_when(
      event_type == "Birth" & next_event == "EIM" ~ "Birth_Outmigration",
      TRUE ~ event_type
    )
  ) %>%
  ungroup()

#=============================================================== Columns to drop ===================================================

var_drop <- c("migration_direction", "next_event")

df_filtered4 <- df_filtered4 %>%
  dplyr::select(-all_of(var_drop))


#================================================================ Recode some variables ===========================================
#Education level
df_filtered4 <- df_filtered4 %>%
  mutate(EducationLevel = case_when( EducationLevel %in% c("NO") ~ "No Education",
                                     EducationLevel %in% c("CU", "VO") ~ "Tertiary",
                                     EducationLevel %in% c("JS", "SS") ~ "Secondary",
                                     EducationLevel %in% c("LB", "UB") ~ "Basic",
                                     EducationLevel %in% c("MA") ~ "Madarsa",
                                     EducationLevel %in% c("QE") ~ "Quranic",
                                     EducationLevel %in% c("SN") ~ "Senegalish/French school",
                                     EducationLevel %in% c("OT") ~ "Others"
  ))

#Numeric variables
df_filtered4 <- df_filtered4 %>%
  mutate(across(
    c(`Age(yrs)`, AgeAtPartnership, NumberOfChildrenEverBorn, AverageBirthSpacingMonths),
    as.numeric,
    .names = "{.col}"   # keep original names
  )) %>%
  # optional: rename Age(yrs) to age for easier use
  rename(age = `Age(yrs)`)

#marital status
df_filtered4 <- df_filtered4 %>%
  mutate(Maritalstatus = case_when( Maritalstatus %in% c("Death", "Widowed") ~ "Widowed",
                                    Maritalstatus %in% c("Separated", "Divorce") ~ "Separated/Divorce",
                                    TRUE ~ Maritalstatus
                                    
  ))


#Age ata partnership
check <- df_filtered4 %>% #they were 21 cases
  filter(AgeAtPartnership <10)

# Calculate overall median (ignoring NAs)
overall_median <- median(df_filtered4$AgeAtPartnership, na.rm = TRUE)

# Replace values < 10 with overall median
df_filtered4 <- df_filtered4 %>%
  mutate(AgeAtPartnership = ifelse(AgeAtPartnership < 10, overall_median, AgeAtPartnership))


#=================================================================== Save clean data ========================================

saveRDS(df_filtered4,
        file.path(output_Dir, "Cleaned_MRC_Gambia_dataset2.rds")
)



#--------------------------------------------------------------------- END --------------------------------------------------------------

#------------------------------------------------------
# Function to create a descriptive gt summary table
#--------------------------------------------------------

library(dplyr)
library(gtsummary)
library(gt)


create_descr_table <- function(data, vars, by_var = death_year, caption = "Respondent’s characteristic (N = {N})") {
  
  data %>%
    select(all_of(vars)) %>%
    tbl_summary(
      by = {{ by_var }},
      type = all_dichotomous() ~ "categorical",
      statistic = list(
        all_categorical() ~ "{n} ({p}%)"
      ),
      digits = list(
        all_categorical() ~ c(0, 1)
      ),
      missing = "ifany"
    ) %>%
    add_overall() %>%
    modify_caption(caption) %>%
    as_gt()
}

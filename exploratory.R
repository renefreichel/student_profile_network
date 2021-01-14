library(tidyverse)
library(janitor)

cohorts <- c("1718", "1819", "1920")

# Load and combine the three CSVs
raw_data <- data.frame()
for (cohort in cohorts) {
  file <- paste0("data/anonymized_cohort", cohort, ".csv")
  
  cohort_data <- read_csv2(file) %>%
    clean_names() %>% 
    mutate(
      # Add a new column "cohort"
      cohort = cohort
    )

  
  # Add new cohorts data to joined data frame
  raw_data <- bind_rows(
    raw_data,
    cohort_data
  )
}

# Create a tidy dataframe with a subset of the data
tidy_data <- raw_data %>% 
  transmute(
    # Parse the grade as a decimal number
    grade = parse_number(
      offic_cijfer,
      locale = locale(decimal_mark = ",", grouping_mark = ".")
    ) %>%
      suppressWarnings,
    # Put all special grades (i.e. letters) into a separate column
    grade_special = if_else(is.na(grade), offic_cijfer, as.character(NA)),
    # Rename & select other columns
    specialisation = specialisatie,
    course_id = studiegids,
    student_id = masked_id,
    # The short description has different column names in different cohorts
    short_description = if_else(is.na(omschr_10), omschr_11, omschr_10),
    long_description = curs_omschr_l,
    cohort = cohort,
    rank = rangnummer,
    country = land
  )

# A few quick counts to see how much data is available
tidy_data %>%
  distinct(student_id, specialisation) %>% 
  count(specialisation)

tidy_data %>%
  distinct(student_id, specialisation, cohort) %>% 
  count(specialisation, cohort)

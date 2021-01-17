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

# Compute relative grades
tidy_data <- tidy_data %>%
  # Calculate grades relative to *average course grade*
  group_by(cohort, course_id) %>% 
  mutate(
    # Z-Scored
    z_grade_course = scale(grade),
    # Only entered on mean
    m_grade_course = grade - mean(grade, na.rm = T)
  ) %>% 
  ungroup() %>% 
  # Calculate grades relative to *student's average grade*
  group_by(cohort, student_id) %>% 
  mutate(
    # Z-Scored
    z_grade_student = scale(grade),
    # Only entered on mean
    m_grade_student = grade - mean(grade, na.rm = T)
  ) %>% 
  ungroup()

# A few quick counts to see how much data is available
tidy_data %>%
  distinct(student_id, specialisation) %>% 
  count(specialisation)

tidy_data %>%
  distinct(student_id, specialisation, cohort) %>% 
  count(specialisation, cohort)

# Subset of data with useful specialisation info
spec_data <- tidy_data %>% 
  filter(!is.na(specialisation) & specialisation != "0")

# Some more descriptibe counts
spec_data %>% distinct(student_id) %>% nrow()
spec_data %>% distinct(student_id, .keep_all = T) %>% group_by(specialisation) %>% count()
spec_data %>% distinct(student_id, .keep_all = T) %>% group_by(specialisation, cohort) %>% count()

# TODO: maybe match courses across years e.g. 7201622PXY is similar to 7201610PXY

# Get a list of courses with many students
top_courses_info <- spec_data %>%
  # Only count courses w/ grades
  filter(!is.na(grade)) %>% 
  count(course_id, short_description, cohort) %>% 
  group_by(course_id) %>% 
  # Minimum N across cohorts
  mutate(min_n = min(n)) %>% 
  # Only courses with at least 100 grades in each cohort
  filter(min_n > 100)
top_courses <- top_courses_info %>%
  pull(course_id)

# Transform data into wide format with one row per student and
# one column per (top) course. Only using the top courses here
# to avoid a ton of NAs in the matrix
top_course_grades_wide <- spec_data %>%
  filter(course_id %in% top_courses) %>% 
  pivot_wider(
    id_cols = student_id,
    names_from = short_description,
    
    # This can be adjusted to use a different kind of relative or absolute grade
    values_from = m_grade_course
  )

# Matrix of specialisations in wide format
specialisations_wide <- spec_data %>% 
  distinct(student_id, .keep_all = T) %>% 
  mutate(one = 1) %>% 
  pivot_wider(id_cols = student_id, names_from = specialisation, values_from = one) %>% 
  mutate(across(everything(), ~replace_na(.x, 0)))

# Fit some first, very preliminary networks
# these are currently fitted over all specialisations, because the N is so small
# at only N = ~500
library(bootnet)
nw <- estimateNetwork(top_course_grades_wide %>% select(-student_id), default = "pcor", missing = "pairwise")
plot(nw)

nw <- estimateNetwork(top_course_grades_wide %>% select(-student_id), default = "EBICglasso", missing = "pairwise")
plot(nw)


# Explore possible relationships in a correlation plot
joined_matrix <- left_join(top_course_grades_wide, specialisations_wide)

corrs <- cor(joined_matrix %>% select(-student_id), use = "pairwise")
corrplot::corrplot(corrs)

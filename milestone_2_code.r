# Generate suitable descriptive statistics of the 
# 1. Distribution of academic field, 
# - Shown by generating a bar chart of sex vs academic field (Shows absolute number)
# - Pie chart shows percentage of men / women who choose a particular field

# 2. Year in which they attained their highest degree, 
# - Histogram shows this for absolute values
# - Density plot shows this for genders (as a percentage of that gender)

# 3. Year they were hired at the university, 
# - Time series chart shows how many men / women were hired at Uni each year (absolute values)
# - Density plot shows the total percentage of women and men hired in a given year

# 4. Academic rank in 1995 
# - Bar chart showing the total counts
# - Percentage of total women and total men in 1995 who hold a particular rank

# 5. monthly salary in 1995 by sex.
# - Boxplot showing the mean and the variance of the salaries by sex
# Time series chart showing how salary of men and women have changed over time

# Write a paragraph commenting on any differences you observe between men and women faculty in these measurements. 

library(dplyr)    # for data manipulation
library(ggplot2)  # for plotting
library(scales)   # for percentage scales in some plots (e.g. scale_y_continuous(labels=percent))

# Read in the dataset and generate insights about the data
salary_data <- read.table(file = "salary.txt", header = TRUE)

data_insights <- function(df) {
  # Basic dimensions
  n_rows <- nrow(df)
  n_cols <- ncol(df)
  
  # Identify numeric columns
  numeric_vars <- sapply(df, is.numeric)
  n_numeric <- sum(numeric_vars)
  
  # Identify categorical columns (factors or character)
  categorical_vars <- sapply(df, function(x) is.factor(x) || is.character(x))
  n_categorical <- sum(categorical_vars)
  
  # Print the insights
  cat("Data Frame Insights:\n")
  cat("Number of rows:", n_rows, "\n")
  cat("Number of columns:", n_cols, "\n")
  cat("Number of numeric columns:", n_numeric, "\n")
  cat("Number of categorical columns:", n_categorical, "\n\n")
  
  cat("Numeric Variables:\n")
  print(names(df)[numeric_vars])
  cat("\nCategorical Variables:\n")
  print(names(df)[categorical_vars])
}

data_insights(salary_data)


salary_data <- salary_data %>%
  arrange(id, year)


salary_data <- salary_data %>%
  group_by(id) %>%
  mutate(salary_increment = salary - dplyr::lag(salary)) %>%
  ungroup()  # It's a good practice to ungroup after performing grouped operations

write.table(salary_data, file = "updated_salary.txt", sep = "\t", row.names = FALSE, quote = FALSE)



print("=============================================================")
degree_counts_by_gender <- table(salary_data$sex, salary_data$deg)
print("Highest degree attained vs Sex")
print(degree_counts_by_gender)
print("=============================================================")
fields_of_work_and_gender <- table(salary_data$sex, salary_data$field)
print("Distribution of Academic field by Sex")
print(fields_of_work_and_gender)
print("=============================================================")
rank_and_gender <- table(salary_data$sex, salary_data$rank)
print("Rank attained vs Sex")
print(rank_and_gender)
print("=============================================================")
summary_of_year_of_highest_degree_by_sex <- aggregate(yrdeg ~ sex, data = salary_data, FUN = summary)
print("Summary of 'Year in Which highest degree was attained' vs Sex")
print(summary_of_year_of_highest_degree_by_sex)
print("=============================================================")
summary_of_year_of_hire_by_sex <- aggregate(startyr ~ sex, data = salary_data, FUN = summary)
print("Summary of Year of hire vs Sex")
print(summary_of_year_of_hire_by_sex)
print("=============================================================")
summary_of_salary_by_sex <- aggregate(salary ~ sex, data = salary_data, FUN = summary)
print("Summary of Salary vs Sex")
print(summary_of_salary_by_sex)
print("=============================================================")

# 1. Distribution of academic field


# (a) Bar Chart: sex vs academic field (absolute number)
p_field_bar <- ggplot(salary_data, aes(x = field, fill = sex)) +
  geom_bar(position = "dodge") +
  labs(title = "Fields of Work by Sex (Absolute Count)",
       x = "Field",
       y = "Count") +
  theme_minimal()
print(p_field_bar)

# (b) Pie Chart: percentage of men/women in each field
#   - We group by sex and field, calculate n, then proportion of each field within that sex.
pie_data <- salary_data %>%
  group_by(sex, field) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(sex) %>%
  mutate(prop = n / sum(n))  # proportion within that sex

p_field_pie <- ggplot(pie_data, aes(x = "", y = prop, fill = field)) +
  # Using geom_bar(stat = "identity") to plot exactly the 'prop' values
  geom_bar(stat = "identity", width = 1, color = "white") +
  # Convert stacked bar to a pie chart
  coord_polar("y", start = 0) +
  # Separate pies for Men and Women
  facet_wrap(~ sex) +
  # Format the y-axis as percentages
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  # Here we add the text labels with percentages INSIDE the slices
  geom_text(aes(label = scales::percent(prop, accuracy = 1)),
            position = position_stack(vjust = 0.5),
            size = 3,  # Adjust text size as desired
            color = "white") + 
  labs(title = "Pie Chart of Academic Field by Sex (Percentage)",
       x = NULL,
       y = NULL) +
  # Remove background elements and fix aspect ratio to get a perfect circle
  theme_void() +
  theme(
    legend.position = "right",
    aspect.ratio = 1   # ensures the facet is a circle
  )

print(p_field_pie)


# 2. Year in which they attained their highest degree


yrdeg_means <- salary_data %>%
  group_by(sex) %>%
  summarise(mean_yrdeg = mean(yrdeg, na.rm = TRUE))

p7 <- ggplot(salary_data, aes(yrdeg))
plot_of_sex_vs_yrdeg <- p7 + geom_density(aes(fill = factor(sex)), alpha = 0.8) + 
    geom_vline(
    data = yrdeg_means,
    aes(xintercept = mean_yrdeg),
    linetype = "dashed",
    size = 1
  ) +
  labs(title = "Sex vs Year when Highest Degree was attained (Density Plot)", 
       x = "Year in which Highest Degree was Attained",
       fill = "sex")
print(plot_of_sex_vs_yrdeg)


# 3. Year they were hired at the university


# (a) Time series chart: how many men/women were hired each year (absolute values)
hire_data <- salary_data %>%
  group_by(startyr, sex) %>%
  summarise(count = n(), .groups = 'drop')

avg_years <- hire_data %>%
  group_by(sex) %>%
  summarise(
    avg_year = sum(startyr * count) / sum(count),  # Weighted mean
    .groups = 'drop'
  )

# 2. Create the original time series plot
p_hire_timeseries <- ggplot(hire_data, aes(x = startyr, y = count, color = sex)) +
  geom_line() +
  # 3. Add vertical lines for each sex's average hire year
  geom_vline(
    data = avg_years,
    aes(xintercept = avg_year, color = sex),
    linetype = "dashed",
    size = 1
  ) +
  labs(
    title = "Number of Men and Women Hired Each Year",
    x = "Year of Hire",
    y = "Count"
  ) +
  theme_minimal()

print(p_hire_timeseries)


# (b) Density plot: total percentage of women and men hired in a given year
p_hire_dens <- ggplot(salary_data, aes(x = startyr, fill = sex)) +
  geom_density(alpha = 0.3, position = "fill") +
  labs(title = "Percentage of Hires by Year (Men vs Women)",
       x = "Year of Hire",
       y = "Proportion") +
  scale_y_continuous(labels = percent) +
  theme_minimal()
print(p_hire_dens)


# 4. Academic rank in 1995


rank_95_data <- salary_data %>% filter(year == 95)

# (a) Bar chart showing the total counts
p_rank_95_count <- ggplot(rank_95_data, aes(x = rank, fill = sex)) +
  geom_bar(position = "dodge") +
  labs(title = "Academic Rank in 1995 - Total Counts",
       x = "Rank",
       y = "Count") +
  theme_minimal()
print(p_rank_95_count)

# (b) Percentage of total women/men in 1995 who hold a particular rank
p_rank_95_percent <- ggplot(rank_95_data, aes(x = rank, fill = sex)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  labs(title = "Academic Rank in 1995 - Percentage by Sex",
       x = "Rank",
       y = "Percentage") +
  theme_minimal()
print(p_rank_95_percent)


# 5. Monthly salary in 1995 by sex, and time series of salary over all years


salary_95_data <- salary_data %>% filter(year == 95)

# (a) Boxplot of monthly salary in 1995 by sex
p_salary_95_box <- ggplot(salary_95_data, aes(x = sex, y = salary, fill = sex)) +
  geom_boxplot() +
  labs(title = "Salary Distribution by Sex in 1995",
       x = "Sex",
       y = "Salary") +
  theme_minimal()
print(p_salary_95_box)

# (b) Time series chart showing how average salary of men and women changed over time
salary_trend <- salary_data %>%
  group_by(year, sex) %>%
  summarise(mean_salary = mean(salary, na.rm = TRUE), .groups = 'drop')

p_salary_trend <- ggplot(salary_trend, aes(x = year, y = mean_salary, color = sex)) +
  geom_line() +
  labs(title = "Average Salary by Year and Sex",
       x = "Year",
       y = "Average Salary") +
  theme_minimal()
print(p_salary_trend)



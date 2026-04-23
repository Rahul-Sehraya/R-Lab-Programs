library(ggplot2)
library(dplyr)
library(tidyr)

# Load the dataset
imported_data <- read.csv("D:/R Studio Project/Students.csv")

# 1) College with the most AI usage (Top 10) // horizontal bar graph
imported_data %>%
  group_by(College_Name) %>%
  summarise(Total_Usage = sum(Daily_Usage_Hours, na.rm = TRUE)) %>%
  arrange(desc(Total_Usage)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(College_Name, Total_Usage), y = Total_Usage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Colleges by Total AI Usage Hours", x = "College", y = "Total Hours") +
  theme_minimal()

# 2) Most popular AI used // horizontal bar graph
imported_data %>%
  separate_rows(AI_Tools_Used, sep = ", ") %>%
  count(AI_Tools_Used, sort = TRUE) %>%
  ggplot(aes(x = reorder(AI_Tools_Used, n), y = n)) +
  geom_bar(stat = "identity", fill = "coral") +
  coord_flip() +
  labs(title = "Most Popular AI", x = "AI Tool", y = "Number of Students") +
  theme_minimal()

# 3 & 8) Average AI usage/Time Spent across all Students // histogram and line-chart
avg_usage <- mean(imported_data$Daily_Usage_Hours, na.rm = TRUE)

ggplot(imported_data, aes(x = Daily_Usage_Hours)) +
  geom_histogram(binwidth = 0.5, fill = "seagreen", color = "white") +
  geom_vline(xintercept = avg_usage, color = "red", linetype = "dashed", linewidth = 1) + 
  annotate("text", x = avg_usage + 1, y = 100, label = paste("Avg:", round(avg_usage, 2)), color = "red") +
  labs(title = "Distribution of Daily AI Usage Hours", x = "Hours per Day", y = "Frequency") +
  theme_minimal()

# 4) Overall professor approval rate // pie chart
imported_data %>%
  count(Do_Professors_Allow_Use) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = "", y = n, fill = Do_Professors_Allow_Use)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Professor AI Approval Rate") +
  theme_void()

# 5) Professor preferred AI // bar graph
imported_data %>%
  count(Preferred_AI_Tool, sort = TRUE) %>%
  ggplot(aes(x = reorder(Preferred_AI_Tool, n), y = n)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Preferred AI Tools", x = "AI Tool", y = "Frequency") +
  theme_minimal()

# 6) State with the most AI usage // horizontal bar graph
imported_data %>%
  filter(!is.na(State) & State != "") %>%
  group_by(State) %>%
  summarise(Total_Usage = sum(Daily_Usage_Hours, na.rm = TRUE)) %>%
  arrange(desc(Total_Usage)) %>%
  ggplot(aes(x = reorder(State, Total_Usage), y = Total_Usage)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  coord_flip() +
  labs(title = "AI Usage by State", x = "State", y = "Total Hours") +
  theme_minimal()

# 7) Perceived impact on grades // box plot + violin 
ggplot(imported_data, aes(x = "", y = Impact_on_Grades)) +
  geom_violin(fill = "lightblue", color = "blue") +
  geom_boxplot(width = 0.1, fill = "white") + 
  labs(title = "Distribution of Grade Impact", y = "Score (-5 to 5)") +
  theme_minimal()

# 9) Percentage of students willing to pay for higher tier // bar graph
imported_data %>%
  count(Willing_to_Pay_for_Access) %>%
  ggplot(aes(x = Willing_to_Pay_for_Access, y = n, fill = Willing_to_Pay_for_Access)) +
  geom_bar(stat = "identity") +
  labs(title = "Willingness to Pay for Premium AI", x = "Willing to Pay", y = "Student Count") +
  theme_minimal()

# 10) Stream with the highest usage // bar graph
imported_data %>%
  group_by(Stream) %>%
  summarise(Avg_Usage = mean(Daily_Usage_Hours, na.rm = TRUE)) %>%
  arrange(desc(Avg_Usage)) %>%
  ggplot(aes(x = reorder(Stream, Avg_Usage), y = Avg_Usage)) +
  geom_bar(stat = "identity", fill = "darkred") +
  labs(title = "Average AI Usage by Academic Stream", x = "Stream", y = "Average Daily Hours") +
  theme_minimal()

# 11) Most common use case // horizontal bar graph
imported_data %>%
  separate_rows(Use_Cases, sep = ", ") %>%
  count(Use_Cases, sort = TRUE) %>%
  ggplot(aes(x = reorder(Use_Cases, n), y = n)) +
  geom_bar(stat = "identity", fill = "cadetblue") +
  coord_flip() +
  labs(title = "Common AI Use Cases among Students", x = "Use Case", y = "Frequency") +
  theme_minimal()



#12) frequency bar graph of #7 
ggplot(imported_data, aes(x = factor(Impact_on_Grades))) +
  geom_bar(fill = "lightblue", color = "blue") +
  labs(
    title = "Distribution of Grade Impact",
    x = "Score (-5 to 5)",
    y = "Count"
  ) +
  theme_minimal()

# 13) STANDARD DEVIATION — SD of key numeric variables 
#
# Shows how spread out the data is across the 4 main numeric variables
bell_data <- imported_data %>%
  select(Daily_Usage_Hours, Trust_in_AI_Tools, Impact_on_Grades, Awareness_Level) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value")

bell_stats <- bell_data %>%
  group_by(Variable) %>%
  summarise(Mean = mean(Value, na.rm = TRUE), SD = sd(Value, na.rm = TRUE))

bell_curves <- bell_stats %>%
  rowwise() %>%
  reframe(
    x = seq(Mean - 3.5 * SD, Mean + 3.5 * SD, length.out = 300),
    y = dnorm(x, mean = Mean, sd = SD),
    Variable = Variable
  )


bell_labels <- bell_stats %>%
  mutate(label = paste0("μ = ", round(Mean, 2), "\nσ = ", round(SD, 2)))

ggplot(bell_data, aes(x = Value)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 0.5, fill = "steelblue", color = "white", alpha = 0.5) +
  geom_line(data = bell_curves, aes(x = x, y = y),
            color = "red", linewidth = 1.2, inherit.aes = FALSE) +
  geom_text(data = bell_labels,
            aes(x = Mean, y = Inf, label = label),
            vjust = 1.5, hjust = 0.5, size = 3.5,
            color = "black", fontface = "bold", inherit.aes = FALSE) +
  facet_wrap(~Variable, scales = "free") +
  labs(
    title = "Bell Curve (Normal Distribution) of Key Numeric Variables",
    subtitle = "Blue = actual data distribution | Red curve = normal fit | σ = standard deviation",
    x = "Value",
    y = "Density"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 11))


# 14) CORRELATION — between all numeric variables // heatmap
#
# Dark blue = strong positive, dark red = strong negative.
numeric_data <- imported_data %>%
  select(Daily_Usage_Hours, Trust_in_AI_Tools, Impact_on_Grades, Awareness_Level, Year_of_Study)

cor_matrix <- cor(numeric_data, use = "complete.obs")

cor_df <- as.data.frame(cor_matrix)
cor_df$Var1 <- rownames(cor_df)
cor_df <- cor_df %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "Correlation")

ggplot(cor_df, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), size = 4, fontface = "bold") +
  scale_fill_gradient2(
    low = "darkred", mid = "white", high = "steelblue",
    midpoint = 0, limits = c(-1, 1)
  ) +
  labs(
    title = "Correlation Heatmap of Numeric Variables",
    subtitle = "Values range from -1 (negative) to +1 (positive correlation)",
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


# 15) REGRESSION — Daily Usage Hours vs Impact on Grades // scatter + regression line
#
# Applying regression in this dataset: does spending more time on AI tools actually improve (or hurt) grades?
ggplot(imported_data, aes(x = Daily_Usage_Hours, y = Impact_on_Grades)) +
  geom_jitter(alpha = 0.4, color = "steelblue", width = 0.1, height = 0.1) +
  geom_smooth(method = "lm", color = "red", se = TRUE, linewidth = 1.2) +
  labs(
    title = "Regression: Daily AI Usage Hours vs Impact on Grades",
    subtitle = "Red line = linear regression fit | Shaded area = 95% confidence interval",
    x = "Daily Usage Hours",
    y = "Impact on Grades (-5 to +5)"
  ) +
  theme_minimal()

lm_model <- lm(Impact_on_Grades ~ Daily_Usage_Hours, data = imported_data)
cat("\n===== Regression Summary: Usage Hours → Grade Impact =====\n")
print(summary(lm_model))


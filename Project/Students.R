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

#generate EDA of Big Mart Dataset and Train Data Set 
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(RColorBrewer)
train <- read.csv("D:/MCA_Practicals/Sem_2/R_lab/bigmart.xls", na.strings = c("", " ", "NA"), header = TRUE)

# 1. WEIGHT vs FAT CONTENT — Box Plot
ggplot(train, aes(x = Item_Fat_Content, y = Item_Weight, fill = Item_Fat_Content)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1.5,
               outlier.fill = "white", width = 0.5) +
  stat_summary(fun = mean, geom = "point",
               shape = 23, size = 3, fill = "white") +
  scale_fill_manual(values = c("Low Fat" = "#5DCAA5",
                               "Regular" = "#F0997B")) +
  labs(
    title = "Item Weight Distribution by Fat Content",
    subtitle = "Diamond = mean | Box = IQR | Whiskers = 1.5× IQR",
    x = "Fat Content",
    y = "Item Weight (g)",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))


# 2. MOST SOLD ITEM TYPE — Horizontal Bar
train %>%
  group_by(Item_Type) %>%
  summarise(Total_Sales = sum(Item_Outlet_Sales, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales)) %>%
  ggplot(aes(x = reorder(Item_Type, Total_Sales),
             y = Total_Sales,
             fill = Total_Sales)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = scales::comma(round(Total_Sales))),
            hjust = -0.1, size = 3.2) +
  scale_fill_gradient(low = "#B5D4F4", high = "#185FA5") +
  scale_y_continuous(labels = scales::comma,
                     expand = expansion(mult = c(0, 0.15))) +
  coord_flip() +
  labs(
    title = "Total Sales by Item Type",
    subtitle = "Sum of Item_Outlet_Sales across all outlets",
    x = NULL,
    y = "Total Sales"
  ) +
  theme_minimal(base_size = 13)


# 3. TOP 15 HIGHEST-EARNING ITEMS
train %>%
  mutate(Revenue = Item_MRP * Item_Outlet_Sales) %>%
  group_by(Item_Identifier) %>%
  summarise(Total_Revenue = sum(Revenue, na.rm = TRUE)) %>%
  slice_max(Total_Revenue, n = 15) %>%
  ggplot(aes(x = reorder(Item_Identifier, Total_Revenue),
             y = Total_Revenue,
             fill = Total_Revenue)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = scales::dollar(round(Total_Revenue / 1e3), suffix = "K")),
            hjust = -0.1, size = 3.2) +
  scale_fill_gradient(low = "#9FE1CB", high = "#0F6E56") +
  coord_flip() +
  labs(
    title = "Top 15 Highest-Earning Items",
    x = "Item Identifier",
    y = "Total Revenue"
  ) +
  theme_minimal(base_size = 13)


# 4. OUTLET TYPE FREQUENCY BY LOCATION
train %>%
  distinct(Outlet_Identifier, .keep_all = TRUE) %>%
  count(Outlet_Location_Type, Outlet_Type) %>%
  ggplot(aes(x = Outlet_Location_Type, y = n, fill = Outlet_Type)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = n),
            position = position_dodge(width = 0.7),
            vjust = -0.4) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Outlet Type Frequency by Location Tier",
    x = "Location Type",
    y = "Number of Outlets"
  ) +
  theme_minimal()


# 5. ITEM TYPE PIE CHART
item_counts <- train %>%
  count(Item_Type) %>%
  mutate(pct = n / sum(n))

ggplot(item_counts, aes(x = "", y = n, fill = Item_Type)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Item Type Composition")


# 6. AVERAGE VISIBILITY BY ITEM TYPE
train %>%
  group_by(Item_Type) %>%
  summarise(Avg_Visibility = mean(Item_Visibility, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(Item_Type, Avg_Visibility),
             y = Avg_Visibility,
             fill = Avg_Visibility)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Average Item Visibility") +
  theme_minimal()


# 7. VISIBILITY BY STORE SIZE
train %>%
  filter(!is.na(Outlet_Size)) %>%
  ggplot(aes(x = Outlet_Size,
             y = Item_Visibility,
             fill = Outlet_Size)) +
  geom_boxplot() +
  labs(title = "Item Visibility by Store Size") +
  theme_minimal()


# 8. STORE SIZE BEFORE VS AFTER 2000
train %>%
  filter(!is.na(Outlet_Size)) %>%
  distinct(Outlet_Identifier, .keep_all = TRUE) %>%
  mutate(Era = ifelse(Outlet_Establishment_Year < 2000,
                      "Pre-2000", "Post-2000")) %>%
  count(Era, Outlet_Size) %>%
  ggplot(aes(x = Era, y = n, fill = Outlet_Size)) +
  geom_col(position = "fill") +
  labs(title = "Store Size Mix by Era") +
  theme_minimal()


# 9. MRP vs SALES
ggplot(train, aes(x = Item_MRP,
                  y = Item_Outlet_Sales)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "MRP vs Sales") +
  theme_minimal()



# 10. MRP DISTRIBUTION
ggplot(train, aes(x = Item_Type, y = Item_MRP)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "MRP Distribution by Item Type") +
  theme_minimal()


# 11. SALES BY OUTLET SIZE
train %>%
  filter(!is.na(Outlet_Size)) %>%
  ggplot(aes(x = Outlet_Size,
             y = Item_Outlet_Sales,
             fill = Outlet_Size)) +
  geom_boxplot() +
  labs(title = "Outlet Sales by Store Size") +
  theme_minimal()
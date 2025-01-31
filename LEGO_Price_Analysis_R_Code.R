# Error handling for package loading
suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(scales)
  library(gridExtra)
  library(corrplot)
  library(car)
  library(stats)
  library(viridis)
  library(patchwork)
  library(ggridges)
})

# Custom theme for consistent visualization
my_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 10),
    legend.position = "bottom",
    plot.margin = margin(10, 10, 10, 10)
  )

# Data Import and Cleaning
tryCatch({
  # Correct:
  lego_data <- read_csv("original_lego_sets_analysis_v8.csv",
                        na = c("", "NA", "N/A")
  ) %>%
    filter(piece_count >= 10)  # Remove "non-set items"
}, error = function(e) {
  stop("Error reading CSV file: ", e$message)
})

# Check for missing values
missing_data <- colSums(is.na(lego_data))
if (any(missing_data > 0)) {
  warning("Missing values found in dataset")
  print(missing_data[missing_data > 0])
}

# Basic Statistical Analysis

# Distribution Analysis
price_distribution <- ggplot(lego_data, aes(x = price_per_piece)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  geom_vline(aes(xintercept = median(price_per_piece)), 
             color = "blue", linetype = "dashed", size = 1) +
  scale_x_continuous(labels = scales::dollar_format()) +
  labs(title = "Distribution of Price per Piece",
       x = "Price per Piece",
       y = "Density") +
  my_theme

# Price vs Piece Count Analysis with Confidence Intervals
scatter_plot <- ggplot(lego_data, aes(x = piece_count, y = price)) +
  geom_point(aes(color = age_range), alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", fill = "pink") +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_color_viridis_d() +
  labs(title = "Price vs Piece Count",
       x = "Number of Pieces",
       y = "Price") +
  my_theme

# Age Range Analysis with Error Bars
age_analysis <- lego_data %>%
  group_by(age_range) %>%
  summarise(
    avg_price = mean(price),
    avg_pieces = mean(piece_count),
    avg_price_per_piece = mean(price_per_piece),
    se_price_per_piece = sd(price_per_piece) / sqrt(n()),
    n_sets = n()
  ) %>%
  filter(n_sets > 5)  # Only include age ranges with enough data

age_plot <- ggplot(age_analysis, 
                   aes(x = reorder(age_range, avg_price_per_piece), 
                       y = avg_price_per_piece)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = avg_price_per_piece - se_price_per_piece,
                    ymax = avg_price_per_piece + se_price_per_piece),
                width = 0.2) +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(title = "Average Price per Piece by Age Range",
       x = "Age Range",
       y = "Average Price per Piece") +
  my_theme

#Statistical Tests

# Normality test for price per piece
shapiro_test <- shapiro.test(lego_data$price_per_piece)

# ANOVA for age ranges with effect size
age_anova <- aov(price_per_piece ~ age_range, data = lego_data)
eta_squared <- summary(age_anova)[[1]]$"Sum Sq"[1] / 
  sum(summary(age_anova)[[1]]$"Sum Sq")
tukey_results <- TukeyHSD(age_anova)

# Correlation Analysis with significance
numeric_cols <- lego_data %>%
  select(price, piece_count, price_per_piece) %>%
  cor()

correlation_plot <- corrplot(numeric_cols, 
                             method = "color", 
                             type = "upper",
                             addCoef.col = "black", 
                             tl.col = "black",
                             sig.level = 0.05,
                             insig = "blank")

# Advanced Visualizations

# Price Point Distribution with Density
price_points <- ggplot(lego_data, aes(x = cut_width(price, width = 10))) +
  geom_bar(fill = "skyblue", alpha = 0.7) +
  coord_flip() +
  labs(title = "Distribution of Price Points",
       x = "Price Range",
       y = "Count") +
  my_theme

# Piece Count Distribution with Density
piece_dist <- ggplot(lego_data, aes(x = cut_width(piece_count, width = 100))) +
  geom_bar(fill = "skyblue", alpha = 0.7) +
  coord_flip() +
  labs(title = "Distribution of Piece Counts",
       x = "Piece Range",
       y = "Count") +
  my_theme

# Value Analysis by Price Range with Confidence Intervals
value_analysis <- lego_data %>%
  mutate(price_range = cut_width(price, width = 50)) %>%
  group_by(price_range) %>%
  summarise(
    avg_price_per_piece = mean(price_per_piece),
    se_price_per_piece = sd(price_per_piece) / sqrt(n()),
    n_sets = n()
  )

value_plot <- ggplot(value_analysis, 
                     aes(x = price_range, y = avg_price_per_piece)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_errorbar(aes(ymin = avg_price_per_piece - se_price_per_piece,
                    ymax = avg_price_per_piece + se_price_per_piece),
                width = 0.2) +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(title = "Average Price per Piece by Price Range",
       x = "Price Range",
       y = "Average Price per Piece") +
  my_theme

# Best Value Analysis

# Find best value sets in each price range with confidence intervals
price_brackets <- lego_data %>%
  mutate(price_bracket = case_when(
    price < 20 ~ "Under $20",
    price < 50 ~ "$20-50",
    price < 100 ~ "$50-100",
    price < 200 ~ "$100-200",
    TRUE ~ "Over $200"
  )) %>%
  group_by(price_bracket) %>%
  slice_min(order_by = price_per_piece, n = 5)

# Create visualization for best value sets
best_value_plot <- ggplot(price_brackets, 
                          aes(x = reorder(name, -price_per_piece), 
                              y = price_per_piece)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
  facet_wrap(~price_bracket, scales = "free") +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(title = "Best Value Sets by Price Range",
       x = "Set Name",
       y = "Price per Piece") +
  my_theme +
  theme(axis.text.y = element_text(size = 8))

# Advanced Statistical Analysis

# Outlier Detection with Z-scores and IQR
outliers <- lego_data %>%
  mutate(
    price_zscore = scale(price_per_piece),
    is_outlier = abs(price_zscore) > 2
  )

# IQR Analysis
Q1 <- quantile(lego_data$price_per_piece, 0.25)
Q3 <- quantile(lego_data$price_per_piece, 0.75)
IQR <- Q3 - Q1

# Create box plot with outliers highlighted
box_plot <- ggplot(outliers, aes(x = "Price per Piece", y = price_per_piece)) +
  geom_boxplot(outlier.colour = "red", fill = "skyblue", alpha = 0.7) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(title = "Price per Piece Distribution with Outliers",
       x = "",
       y = "Price per Piece") +
  my_theme

# Save Plots 

# Combine plots using patchwork
basic_analysis <- (price_distribution + scatter_plot) / 
  (age_plot + price_points)

advanced_analysis <- (piece_dist + value_plot) /
  (box_plot + best_value_plot)

# Save plots with error handling
tryCatch({
  ggsave("basic_analysis.png", basic_analysis, 
         width = 15, height = 10, dpi = 300)
  ggsave("advanced_analysis.png", advanced_analysis, 
         width = 15, height = 10, dpi = 300)
}, error = function(e) {
  warning("Error saving plots: ", e$message)
})

# Summary Statistics
summary_stats <- lego_data %>%
  summarise(
    total_sets = n(),
    avg_price = mean(price),
    median_price = median(price),
    avg_pieces = mean(piece_count),
    median_pieces = median(piece_count),
    avg_price_per_piece = mean(price_per_piece),
    median_price_per_piece = median(price_per_piece),
    ci_lower = mean(price_per_piece) - 
      qt(0.975, n()-1) * sd(price_per_piece)/sqrt(n()),
    ci_upper = mean(price_per_piece) + 
      qt(0.975, n()-1) * sd(price_per_piece)/sqrt(n())
  )

# Print results with proper formatting
cat("\nSummary Statistics:\n")
print(summary_stats)

cat("\nShapiro-Wilk Test Results:\n")
print(shapiro_test)

cat("\nANOVA Results:\n")
print(summary(age_anova))
cat("\nEffect size (Eta squared):", eta_squared, "\n")

cat("\nTukey Test Results:\n")
print(tukey_results)

# More Insights

# Age progression analysis with confidence intervals
age_progression <- lego_data %>%
  group_by(age_range) %>%
  summarise(
    avg_price_per_piece = mean(price_per_piece),
    se_price_per_piece = sd(price_per_piece) / sqrt(n()),
    avg_pieces = mean(piece_count),
    n_sets = n()
  ) %>%
  filter(n_sets >= 5) %>%
  arrange(avg_price_per_piece)

# Ridge plot for price distribution by age range
ridge_plot <- ggplot(lego_data, 
                     aes(x = price_per_piece, 
                         y = age_range, 
                         fill = age_range)) +
  geom_density_ridges(alpha = 0.6) +
  scale_x_continuous(labels = scales::dollar_format()) +
  scale_fill_viridis_d() +
  labs(title = "Price per Piece Distribution by Age Range",
       x = "Price per Piece",
       y = "Age Range") +
  my_theme +
  theme(legend.position = "none")

# Save additional plots with error handling
tryCatch({
  ggsave("age_distribution.png", ridge_plot, 
         width = 12, height = 8, dpi = 300)
}, error = function(e) {
  warning("Error saving ridge plot: ", e$message)
})

# Enhanced visualizations for LEGO analysis

# Enhanced visualizations for LEGO analysis

# LEGO themed color palette
lego_colors <- c("#006DB7", "#D01012", "#F6B03B", "#00AF4D", "#E77C00")

# Price Distribution with Enhanced Styling
price_distribution_enhanced <- ggplot(lego_data, aes(x = price_per_piece)) +
  geom_histogram(aes(y = ..density..), bins = 50, 
                fill = lego_colors[1], color = "white", alpha = 0.8) +
  geom_density(color = lego_colors[2], size = 1.2) +
  geom_vline(aes(xintercept = median(price_per_piece)), 
             color = lego_colors[3], linetype = "dashed", linewidth = 1) +
  scale_x_continuous(labels = scales::dollar_format(), 
                    breaks = scales::pretty_breaks(n = 10)) +
  labs(title = "Distribution of LEGO Price per Piece",
       subtitle = "With density curve and median line",
       x = "Price per Piece",
       y = "Density",
       caption = "Median shown as dashed line") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray50"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.caption = element_text(size = 8, color = "gray50"),
    panel.grid.minor = element_blank()
  )

# Enhanced Scatter Plot with Regression and Confidence Bands
scatter_plot_enhanced <- ggplot(lego_data, aes(x = piece_count, y = price)) +
  geom_point(aes(color = age_range), alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", color = lego_colors[2], fill = "gray80", alpha = 0.2) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_color_viridis_d() +
  labs(title = "LEGO Set Prices vs. Piece Count",
       subtitle = "By age range with linear trend",
       x = "Number of Pieces",
       y = "Price",
       color = "Age Range") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray50"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

# Enhanced Box Plot by Age Range
box_plot_enhanced <- ggplot(lego_data, aes(x = reorder(age_range, price_per_piece, FUN = median), 
                                          y = price_per_piece, fill = age_range)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.fill = "white") +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_fill_viridis_d() +
  coord_flip() +
  labs(title = "Price per Piece Distribution by Age Range",
       subtitle = "Box plots showing median, quartiles, and outliers",
       x = "Age Range",
       y = "Price per Piece") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray50"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

# Enhanced Ridge Plot
ridge_plot_enhanced <- ggplot(lego_data, 
                            aes(x = price_per_piece, 
                                y = reorder(age_range, price_per_piece, FUN = median),
                                fill = age_range)) +
  geom_density_ridges(alpha = 0.7, scale = 0.9, rel_min_height = 0.01) +
  scale_x_continuous(labels = scales::dollar_format()) +
  scale_fill_viridis_d() +
  labs(title = "Price per Piece Distribution Across Age Ranges",
       subtitle = "Density distributions showing price patterns",
       x = "Price per Piece",
       y = "Age Range") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray50"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

# Piece Count Distribution by Price Range
piece_count_by_price <- lego_data %>%
  mutate(price_category = cut(price,
                             breaks = c(0, 20, 50, 100, 200, Inf),
                             labels = c("Under $20", "$20-50", "$50-100", "$100-200", "Over $200")))

piece_dist_enhanced <- ggplot(piece_count_by_price, 
                            aes(x = piece_count, y = price_category, fill = price_category)) +
  geom_violin(alpha = 0.7, scale = "width") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_fill_viridis_d() +
  labs(title = "Piece Count Distribution by Price Range",
       subtitle = "Violin plots showing piece count patterns",
       x = "Number of Pieces",
       y = "Price Range") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray50"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

# Save each plot individually
ggsave("Lego Price Project/price_distribution.png", price_distribution_enhanced,
       width = 12, height = 8, dpi = 300)

ggsave("Lego Price Project/scatter_plot.png", scatter_plot_enhanced,
       width = 12, height = 8, dpi = 300)

ggsave("Lego Price Project/box_plot.png", box_plot_enhanced,
       width = 12, height = 8, dpi = 300)

ggsave("Lego Price Project/ridge_plot.png", ridge_plot_enhanced,
       width = 12, height = 8, dpi = 300)

ggsave("Lego Price Project/piece_distribution.png", piece_dist_enhanced,
       width = 12, height = 8, dpi = 300)
# ----------------------------
# 1. LIBRARY SETUP
# ----------------------------
library(readxl)       # For reading Excel files
library(janitor)      # For data cleaning
library(dplyr)        # For data manipulation
library(stringr)      # For string operations
library(purrr)        # For functional programming
library(readr)        # For reading CSV files
library(ggplot2)      # For data visualization
library(lubridate)    # For date handling
library(furrr)        # For parallel processing
library(scales)       # For formatting scales
library(patchwork)    # For combining plots

# Configure parallel processing
plan(multisession, workers = max(1, availableCores() - 1))

# ----------------------------
# 2. DATA LOADING
# ----------------------------
# Load main dataset and clean column names
dataset <- read_csv("2024-srcsc-superlife-inforce-dataset.csv", skip = 3) %>% clean_names()

# Load auxiliary datasets
interventions <- read_excel("srcsc-2024-interventions.xlsx", skip = 14) %>% clean_names()
economic <- read_excel("srcsc-2024-lumaria-economic-data.xlsx", skip = 10) %>% clean_names()
mortality <- read_excel("srcsc-2024-lumaria-mortality-table.xlsx", skip = 12) %>% clean_names()
mortality_impact <- read_excel("Intervention_Mortality_Impact_Full.xlsx") %>% clean_names()

# ----------------------------
# 3. DATA PREPROCESSING
# ----------------------------
# Filter active policies only
dataset <- dataset %>%
  filter(is.na(death_indicator), is.na(lapse_indicator)) %>%
  select(-death_indicator, -year_of_death, -lapse_indicator, -year_of_lapse, -cause_of_death) %>%
  mutate(across(c(policy_type, sex, smoker_status, underwriting_class, 
                  urban_vs_rural, region, distribution_channel), as.factor))

# One-Hot Encoding for customer characteristics
customers_dummy <- dataset %>%
  mutate(
    age_26_40 = between(issue_age, 26, 40),
    age_41_55 = between(issue_age, 41, 55),
    age_56_65 = between(issue_age, 56, 65),
    sex_f = sex == "F",
    sex_m = sex == "M",
    smoker_ns = smoker_status == "NS",
    smoker_s = smoker_status == "S",
    risk_low = str_detect(underwriting_class, "low risk"),
    risk_mod = str_detect(underwriting_class, "moderate risk"),
    risk_high = str_detect(underwriting_class, "high risk"),
    urban = urban_vs_rural == "Urban",
    rural = urban_vs_rural == "Rural",
    agent = distribution_channel == "Agent",
    online = distribution_channel == "Online",
    telemarketer = distribution_channel == "Telemarketer"
  ) %>%
  mutate(across(everything(), as.integer)) %>%
  mutate(policy_number = c(1:nrow(dataset)))

# ----------------------------
# 4. INTERVENTIONS PROCESSING
# ----------------------------
# Join intervention information
intervention_impact <- mortality_impact %>%
  inner_join(interventions, by = "intervention_name")

# Calculate % of customers matching each intervention
intervention_match_summary <- pmap_dfr(intervention_impact, function(...) {
  row <- tibble(...)
  zero_cols <- names(row)[which(row == 0)]
  zero_cols <- setdiff(zero_cols, c("intervention_name", "intervention_group"))
  
  filtered_customers <- customers_dummy %>%
    filter(if_all(all_of(zero_cols), ~ . == 0))
  
  tibble(
    intervention_name = row$intervention_name,
    matched_percent = nrow(filtered_customers) / nrow(customers_dummy)
  )
})

intervention_impact <- intervention_impact %>%
  left_join(intervention_match_summary, by = "intervention_name")

# Extract numbers from description strings
intervention_clean <- intervention_impact %>%
  mutate(
    mortality_avg = str_extract_all(approximate_impact_on_mortality_rates, "\\d+") %>%
      map_dbl(~ mean(as.numeric(.))),
    cost_avg = str_extract_all(approximate_per_capita_cost, "\\d+") %>%
      map_dbl(~ mean(as.numeric(.)))
  )

# Plot mortality reduction effectiveness
ggplot(intervention_clean, aes(x = reorder(intervention_name, mortality_avg), y = mortality_avg)) +
  geom_col(fill = "lightsteelblue") +
  coord_flip() +
  labs(title = "Average Mortality Reduction Effectiveness", x = "", y = "% Mortality Reduction")
ggsave("plots/mortality_reduction.png")

# Summarize by intervention group
summary_by_group <- intervention_clean %>%
  group_by(intervention_group) %>%
  summarise(
    avg_mortality_reduction = mean(mortality_avg, na.rm = TRUE),
    avg_per_capita_cost = mean(cost_avg, na.rm = TRUE),
    avg_matched_percent = mean(matched_percent, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    cost_per_mortality_reduction = avg_per_capita_cost / avg_mortality_reduction
  ) %>%
  arrange(
    desc(avg_matched_percent),
    cost_per_mortality_reduction
  ) %>%
  select(intervention_group, avg_matched_percent, cost_per_mortality_reduction)

# ----------------------------
# INTERVENTION GROUP COMPARISON PLOTS
# ----------------------------

# 1. Customer Match Rate Plot
matched_plot <- ggplot(summary_by_group, 
                       aes(x = reorder(intervention_group, avg_matched_percent), 
                           y = avg_matched_percent)) +
  geom_col(fill = "lightsteelblue", alpha = 0.8) +
  geom_text(aes(label = scales::percent(avg_matched_percent, accuracy = 0.1)), 
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(title = "Customer Match Rate by Intervention Group",
       x = "Intervention Group",
       y = "Match Rate") +
  theme_minimal()

# 2. Cost Efficiency Plot
cost_eff_plot <- ggplot(summary_by_group, 
                        aes(x = reorder(intervention_group, cost_per_mortality_reduction), 
                            y = cost_per_mortality_reduction)) +
  geom_col(fill = "lightsteelblue", alpha = 0.8) +
  geom_text(aes(label = scales::dollar(cost_per_mortality_reduction, accuracy = 1)), 
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(title = "Cost per Unit Mortality Reduction",
       x = "Intervention Group",
       y = "Cost ($) / 1% Mortality Reduction") +
  theme_minimal()

# 3. Combined Effectiveness vs Cost Plot
combined_plot <- ggplot(summary_by_group, 
                        aes(x = avg_matched_percent, 
                            y = cost_per_mortality_reduction,
                            color = intervention_group)) +
  geom_point(size = 5, alpha = 0.7) +
  geom_text(aes(label = intervention_group), 
            vjust = -1, size = 3.5, check_overlap = TRUE) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Intervention Group Effectiveness vs Cost",
       x = "Customer Match Rate",
       y = "Cost per 1% Mortality Reduction",
       color = "Intervention Group") +
  theme_minimal() +
  theme(legend.position = "none")

# Save plots
ggsave("plots/matched_percent_by_group.png", matched_plot, width = 8, height = 5)
ggsave("plots/cost_efficiency_by_group.png", cost_eff_plot, width = 8, height = 5)
ggsave("plots/intervention_group_comparison.png", combined_plot, width = 9, height = 6)

# Export results
write_csv(summary_by_group, "intervention_summary.csv")

# Select Group 6 interventions
intervention_clean <- intervention_clean %>%
  filter(str_detect(intervention_group, "Group 6"))

# ----------------------------
# 5. INTERVENTION TIMELINE
# ----------------------------
# Create timeline data
interventions <- tibble(
  Intervention = c(
    "Financial Literacy Workshops", 
    "Sun Safety Awareness",
    "Hydration Campaigns",
    "Online Health Resources",
    "Personalized Health Plans",
    "Holistic Health Assessments",
    "Educational Workshops",
    "Environmental Wellness",
    "Community Gardens",
    "Travel Safety Tips"
  ),
  Start = as.Date(c(
    "2024-01-01", "2024-01-01", "2024-01-01", "2024-01-01", 
    "2024-01-01", "2024-01-01", "2024-01-01", "2024-01-01",
    "2024-01-01", "2024-01-01"
  )),
  End = as.Date(c(
    "2034-12-31", "2025-12-31", "2026-12-31", "2034-12-31",
    "2034-12-31", "2034-12-31", "2027-12-31", "2027-12-31",
    "2027-12-31", "2027-12-31"
  )),
  Frequency = c(
    "2 years", "6 months", "1 week", "1 month",
    "6 months", "6 months", "6 months", "6 months", 
    "1 year", "1 year"
  ),
  Group = c(
    "Financial Group",
    "Behavioral Group",
    "Behavioral Group",
    "Update Group",
    "Health Plan Group",
    "Health Plan Group",
    "Education Group",
    "Education Group",
    "Education Group",
    "Education Group"
  )
)

# Function to generate schedule
generate_schedule <- function(name, start, end, freq, group) {
  if(freq == "2 years") {
    seq_dates <- seq(start, end, by = "2 years")
  } else if(freq == "6 months") {
    seq_dates <- seq(start, end, by = "6 months")
  } else if(freq == "1 week") {
    seq_dates <- seq(start, end, by = "1 week")
  } else if(freq == "1 month") {
    seq_dates <- seq(start, end, by = "1 month")
  } else if(freq == "1 year") {
    seq_dates <- seq(start, end, by = "1 year")
  } else {
    seq_dates <- NULL
  }
  
  tibble(
    Intervention = name,
    Date = seq_dates,
    Group = group
  )
}

# Create detailed schedule
detailed_schedule <- interventions %>%
  rowwise() %>%
  do(generate_schedule(.$Intervention, .$Start, .$End, .$Frequency, .$Group)) %>%
  ungroup() %>%
  mutate(Intervention = factor(Intervention, levels = rev(unique(interventions$Intervention))))

# Plot timeline
timeline_plot <- ggplot(detailed_schedule, aes(x = Date, y = Intervention, color = Group)) +
  geom_point(size = 2) +
  scale_x_date(breaks = seq(as.Date("2024-01-01"), as.Date("2034-12-31"), by = "2 years"),
               date_labels = "%Y") +
  scale_color_brewer(palette = "Pastel1") +
  labs(title = "INTERVENTION IMPLEMENTATION TIMELINE",
       x = "Time", y = "", color = "Group") +
  theme_minimal()

ggsave("plot/intervention_timeline.png", timeline_plot, width = 10, height = 6)

# ----------------------------
# 6. SIMULATION

# Sample data
set.seed(123)
# Read timeline data
timeline_data <- read_excel("timeline_data.xlsx")
customers_dummy <- customers_dummy[sample(nrow(dataset), 200), ]

# ----------------------------
# Core Calculation Functions
calculate_payout <- function(policy_type, issue_year, current_year, face_amount) {
  ifelse((policy_type == 'T20') & (current_year - issue_year >= 20), 0, face_amount)
}

calculate_mortality <- function(issue_age, issue_year, current_year, mortality) {
  age <- issue_age + (current_year - issue_year)
  if(age > nrow(mortality) | age < 1) return(1)
  mortality[age, 'mortality_rate'][[1]]
}

calculate_intervention_cost <- function(policy_type, issue_year, current_year, timeline_data, intervention_clean) {
  if((policy_type == 'T20') & (current_year - issue_year >= 20)) return(0)
  
  year_col <- paste0("year_", current_year)
  if(!year_col %in% names(timeline_data)) return(0)
  
  timeline_data %>%
    left_join(intervention_clean, by = c('intervention' = 'intervention_name')) %>%
    filter(!is.na(.data[[year_col]]), .data[[year_col]] != 0) %>%
    summarise(total_cost = sum(.data[[year_col]] * cost_avg, na.rm = TRUE)) %>%
    pull(total_cost)
}

calculate_mortality_reduction <- function(i, customers_dummy, intervention_clean) {
  traits <- c(
    "age_26_40", "age_41_55", "age_56_65",
    "sex_f", "sex_m",
    "smoker_ns", "smoker_s",
    "risk_low", "risk_mod", "risk_high",
    "urban", "rural",
    "agent", "online", "telemarketer"
  )
  
  customer_data <- customers_dummy %>%
    filter(policy_number == i) %>%
    select(all_of(traits))
  
  result_table <- intervention_clean %>%
    select(intervention_name, mean_mortality_reduction = mortality_avg, all_of(traits)) %>%
    mutate(
      binary_flag = pmap_int(select(., all_of(traits)), function(...) {
        intervention_traits <- list(...)
        any_conflict <- any(map2_lgl(intervention_traits, customer_data, 
                                     ~ (.x == 0) && (.y == 1)))
        as.integer(!any_conflict)
      })
    ) %>%
    filter(binary_flag == 1)
  
  if (nrow(result_table) == 0) 0 else sum(result_table$mean_mortality_reduction, na.rm = TRUE)
}

# Yearly simulation function
simulate_year <- function(current_year, n_simulations = 10) {
  cat("Simulating year", current_year, "...\n")
  
  results <- future_map_dfr(1:nrow(customers_dummy), function(i) {
    policy <- customers_dummy[i,]
    issue_yr <- policy$issue_year
    if(issue_yr > current_year) return(tibble(
      simulation_id = integer(),
      with_intervention = numeric(),
      without_intervention = numeric(),
      difference = numeric(),
      intervention_cost = numeric()
    ))
    
    # Calculate cumulative mortality rates
    p_cumulative_with <- 0
    p_cumulative_without <- 0
    
    for(year in issue_yr:(current_year-1)) {
      mort_rate <- calculate_mortality(policy$issue_age, issue_yr, year, mortality)
      k <- calculate_mortality_reduction(i, customers_dummy, intervention_clean)
      p_cumulative_with <- p_cumulative_with + mort_rate * (1 - k)
      p_cumulative_without <- p_cumulative_without + mort_rate
    }
    
    current_mort <- calculate_mortality(policy$issue_age, issue_yr, current_year, mortality)
    k_current <- calculate_mortality_reduction(i, customers_dummy, intervention_clean)
    
    # Calculate payout and intervention cost
    payout_val <- calculate_payout(policy$policy_type, issue_yr, current_year, policy$face_amount)
    cost_val <- calculate_intervention_cost(policy$policy_type, issue_yr, current_year, timeline_data, intervention_clean)
    
    # Run simulations
    map_dfr(1:n_simulations, ~{
      # With intervention
      u <- runif(1)
      status_with <- case_when(
        u < p_cumulative_with ~ "died_before",
        (u < p_cumulative_with + current_mort * (1 - k_current)) & (u > p_cumulative_with) ~ "died_current",
        TRUE ~ "survived"
      )
      
      # Without intervention
      status_without <- case_when(
        u < p_cumulative_without ~ "died_before",
        (u < p_cumulative_without + current_mort) & (u > p_cumulative_without) ~ "died_current",
        TRUE ~ "survived"
      )
      
      # Return results
      tibble(
        simulation_id = .x,
        with_intervention = case_when(
          status_with == "died_before" ~ 0,
          status_with == "died_current" ~ payout_val,
          TRUE ~ 0
        ),
        without_intervention = case_when(
          status_without == "died_before" ~ 0,
          status_without == "died_current" ~ payout_val,
          TRUE ~ 0
        ),
        intervention_cost = case_when(
          status_without == "died_current" ~ cost_val,
          status_with != "died_before" ~ 0,
          TRUE ~ cost_val
        )
      )
    })
  }, .progress = TRUE)
  
  # Aggregate results
  results %>%
    group_by(simulation_id) %>%
    summarise(
      total_with = sum(with_intervention, na.rm = TRUE),
      total_without = sum(without_intervention, na.rm = TRUE),
      total_intervention_cost = sum(intervention_cost, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(year = current_year,
           total_difference = total_without - total_intervention_cost - total_with,
    )
}

# Run simulation for 2024-2054
years <- 2024:2054
simulation_results <- map_dfr(years, simulate_year, .progress = TRUE)

# ----------------------------
# 7. RESULTS VISUALIZATION
# ----------------------------
# Aggregate results
summary_data <- simulation_results %>%
  group_by(year) %>%
  summarise(
    avg_savings = mean(total_difference),
    sd_saving = sd(total_difference),
    avg_cost = mean(total_intervention_cost),
    sd_cost = sd(total_intervention_cost),
    .groups = "drop"
  )

# Cost comparison plot
saving_plot <- ggplot(summary_data, aes(x = year)) +
  geom_line(aes(y = avg_savings, color = "Annual payout difference with program"), size = 1.2) +
  geom_ribbon(aes(ymin = 0, ymax = avg_savings, fill = "Savings"), alpha = 0.3) +
  scale_y_continuous(labels = dollar_format(scale = 1e-6, suffix = "M")) +
  scale_color_manual(values = "#377EB8") +
  scale_fill_manual(values = c("Savings" = "lightsteelblue")) +
  labs(title = "Annual Cost Comparison",
       x = "Year", y = "Cost ($)")

# Save plot
ggsave("plots/saving_plot.png", saving_plot, width = 10, height = 6)

# Export results
write.csv(simulation_results, "output/simulation_results.csv")
write.csv(summary_data, "output/summary_results.csv")

# Export detailed results
simulation_results %>% 
  group_by(year) %>%
  summarise(
    total_saving = mean(total_difference), 
    sd_saving = sd(total_difference), 
    total_cost = mean(total_intervention_cost),
    sd_cost = sd(total_intervention_cost)
  ) %>%
  distinct() %>%
  write.csv("output/detailed_results.csv")

# ----------------------------
# 8. COST ANALYSIS AND NPV (5% DISCOUNT RATE)
# ----------------------------

# Calculate average and standard deviation of cost per year
cost_by_year <- simulation_results %>% 
  group_by(year) %>%
  summarise(
    total_cost = mean(total_intervention_cost),
    sd_cost = sd(total_intervention_cost)
  ) %>%
  ungroup()

# Plot: Line chart with ribbon for ±1 SD
cost_plot = ggplot(cost_by_year, aes(x = year, y = total_cost)) +
  geom_line(color = "lightsteelblue", size = 1.2) +
  geom_ribbon(aes(ymin = total_cost - sd_cost, ymax = total_cost + sd_cost),
              fill = "lightsteelblue", alpha = 0.3) +
  labs(
    title = "Annual Intervention Cost and Standard Deviation",
    x = "Year",
    y = "Cost (Million USD)"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

# Calculate cost ratios by intervention
intervention_ratios = intervention_clean %>%
  inner_join(timeline_data, by = c('intervention_name' = 'intervention')) %>%
  dplyr::select(intervention_name, cost_avg, starts_with('year')) %>%
  pivot_longer(
    cols = starts_with("year_"),
    names_to = "year",
    names_prefix = "year_",
    values_to = "count"
  ) %>%
  mutate(
    year = as.integer(year),
    total_cost = cost_avg * count
  ) %>%
  group_by(year) %>%
  mutate(
    year_total_cost = sum(total_cost, na.rm = TRUE),
    cost_ratio = ifelse(year_total_cost == 0, 0, total_cost / year_total_cost)
  ) %>%
  ungroup()

# Plot cost allocation (first 6 years)
plot_data <- intervention_ratios %>%
  filter(year >= 2024 & year <= 2029)

# Get color palette
n_colors <- length(unique(plot_data$intervention_name))
pastel_palette <- c(brewer.pal(9, "Pastel1"), brewer.pal(8, "Pastel2"))
pastel_colors <- pastel_palette[1:n_colors]

# Create stacked bar chart
cost_ratio_plot = ggplot(plot_data, aes(x = factor(year), y = cost_ratio, fill = intervention_name)) +
  geom_bar(stat = "identity", position = "fill", color = "white") +
  scale_fill_manual(values = pastel_colors) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Intervention Cost Allocation by Year (2024–2029)",
    x = "Year",
    y = "Cost Ratio (%)",
    fill = "Intervention"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 9),
    legend.title = element_text(face = "bold")
  )

# Create formatted table
intervention_table <- intervention_ratios %>%
  filter(year >= 2024 & year <= 2029) %>%
  mutate(cost_ratio_percent = round(cost_ratio * 100, 1)) %>%
  select(intervention_name, year, cost_ratio_percent) %>%
  tidyr::pivot_wider(names_from = year, values_from = cost_ratio_percent)

# Export formatted table
intervention_table %>%
  kable("html", col.names = c("Intervention", "2024", "2025", "2026", "2027", "2028", "2029"), 
        caption = "Cost Allocation (%) by Intervention and Year") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE, font_size = 12)

# Save plots
ggsave("plots/cost_by_year.png", cost_plot, width = 8, height = 5)
ggsave("plots/cost_ratio.png", cost_ratio_plot, width = 8, height = 5)

# Calculate savings
saving_by_year <- simulation_results %>% 
  group_by(year) %>%
  summarise(
    total_saving = mean(total_difference), 
    sd_saving = sd(total_difference)
  ) %>%
  ungroup()

saving_plot = ggplot(saving_by_year, aes(x = year, y = total_saving)) +
  geom_line(color = "lightsteelblue", size = 1.2) +
  geom_ribbon(aes(ymin = total_saving - sd_saving, ymax = total_saving + sd_saving),
              fill = "lightsteelblue", alpha = 0.3) +
  labs(
    title = "Annual Savings with Standard Deviation",
    x = "Year",
    y = "Savings (Million USD)"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

ggsave('plots/saving_plot.png', saving_plot, width = 8, height = 5)

# Calculate NPV with 5% discount rate
npv_cost <- cost_by_year %>%
  mutate(
    discounted_cost = total_cost / (1 + 0.05)^(year - min(year))
  ) %>%
  summarise(
    npv = sum(discounted_cost),
    total_undiscounted = sum(total_cost)
  )

# ----------------------------
# 9. NPV ANALYSIS FOR COST DIFFERENCE
# ----------------------------

# Calculate cost difference by year
difference_by_year <- simulation_results %>%
  group_by(year) %>%
  summarise(
    avg_difference = mean(total_difference),
    .groups = "drop"
  )

# Calculate NPV for difference with 5% rate
npv_difference <- difference_by_year %>%
  mutate(
    discounted_difference = avg_difference / (1 + 0.05)^(year - min(year))
  ) %>%
  summarise(
    npv_difference = sum(discounted_difference),
    total_undiscounted_difference = sum(avg_difference)
  )

# Plot cost difference by year
difference_plot <- ggplot(difference_by_year, aes(x = year, y = avg_difference)) +
  geom_col(fill = "lightsteelblue", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Average Annual Cost Difference",
       subtitle = "With vs Without Intervention",
       x = "Year",
       y = "Cost Difference") +
  theme_minimal()

# Save plot
ggsave("plots/difference_by_year.png", difference_plot, width = 8, height = 5)

# ----------------------------
# 11. NPV DISTRIBUTION ANALYSIS (FULL VERSION)
# ----------------------------

# Calculate NPV for each simulation (5% rate)
npv_results <- simulation_results %>%
  group_by(simulation_id) %>%
  summarise(
    npv_difference = sum(total_difference / (1 + 0.05)^(year - min(year))),
    npv_cost = sum(total_intervention_cost / (1 + 0.05)^(year - min(year))),
    .groups = "drop"
  )

# Calculate key statistics
npv_stats <- npv_results %>%
  summarise(
    mean_diff = mean(npv_difference),
    median_diff = median(npv_difference),
    sd_diff = sd(npv_difference),
    prob_negative = mean(npv_difference < 0),
    prob_positive = mean(npv_difference > 0),
    min_diff = min(npv_difference),
    max_diff = max(npv_difference)
  )

# 1. NPV Difference Distribution Plot (FULL)
npv_diff_dist <- ggplot(npv_results, aes(x = npv_difference)) +
  # Histogram with density
  geom_histogram(aes(y = ..density..), 
                 bins = 30,
                 fill = "lightsteelblue",
                 color = "white",
                 alpha = 0.8) +
  # Density curve
  geom_density(color = "#377EB8", size = 1.2) +
  # Zero line
  geom_vline(xintercept = 0, 
             linetype = "dashed", 
             color = "red",
             size = 1) +
  # Mean line
  geom_vline(xintercept = npv_stats$mean_diff, 
             linetype = "dashed", 
             color = "darkblue",
             size = 1) +
  # Highlight negative NPV area
  geom_area(data = ~ {
    dens <- density(.x$npv_difference)
    tibble(x = dens$x, y = dens$y) %>% filter(x < 0)
  },
  aes(x = x, y = y),
  fill = "red",
  alpha = 0.3) +
  # Statistical annotations
  annotate("text",
           x = quantile(npv_results$npv_difference, 0.1),
           y = Inf,
           vjust = 2,
           label = paste0(
             "Mean = ", dollar(npv_stats$mean_diff), "\n",
             "P(NPV < 0) = ", percent(npv_stats$prob_negative, accuracy = 0.1), "\n",
             "Min = ", dollar(npv_stats$min_diff), "\n",
             "Max = ", dollar(npv_stats$max_diff)
           ),
           color = "black",
           size = 4) +
  scale_x_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M"),
                     breaks = scales::pretty_breaks(n = 8)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "NPV DIFFERENCE DISTRIBUTION",
       subtitle = "Intervention vs No Intervention (5% Discount Rate)",
       x = "NPV Difference ($)",
       y = "Density",
       caption = "Red area shows negative NPV (unfavorable)") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray50"),
    panel.grid.minor = element_blank()
  )

# 2. NPV Cost Distribution Plot (FULL)
npv_cost_dist <- ggplot(npv_results, aes(x = npv_cost)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 30,
                 fill = "lightsteelblue",
                 color = "white",
                 alpha = 0.8) +
  geom_density(color = "#377EB8", size = 1.2) +
  geom_vline(xintercept = mean(npv_results$npv_cost), 
             linetype = "dashed", 
             color = "darkblue",
             size = 1) +
  annotate("text",
           x = quantile(npv_results$npv_cost, 0.1),
           y = Inf,
           vjust = 2,
           label = paste0(
             "Mean = ", dollar(mean(npv_results$npv_cost)), "\n",
             "Min = ", dollar(min(npv_results$npv_cost)), "\n",
             "Max = ", dollar(max(npv_results$npv_cost))
           ),
           color = "black",
           size = 4) +
  scale_x_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  labs(title = "NPV INTERVENTION COST DISTRIBUTION",
       subtitle = "Total Discounted Cost (5% Discount Rate)",
       x = "NPV Cost ($)",
       y = "Density") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray50")
  )

# Save high-quality plots
ggsave("plots/FULL_npv_difference_dist.png", npv_diff_dist, 
       width = 10, height = 6, dpi = 300)
ggsave("plots/FULL_npv_cost_dist.png", npv_cost_dist, 
       width = 10, height = 6, dpi = 300)

# Export detailed statistics
npv_stats %>%
  mutate(across(where(is.numeric), ~ dollar(.x, scale = 1e-6, suffix = "M"))) %>%
  write_csv("output/FULL_npv_statistics.csv")

# ----------------------------
# 12. DISCOUNT RATE SENSITIVITY ANALYSIS
# ----------------------------

# Function to calculate NPV with different rates
calculate_npv <- function(rate) {
  simulation_results %>%
    group_by(simulation_id) %>%
    summarise(
      npv = sum(total_difference / (1 + rate)^(year - min(year)))
    ) %>%
    pull(npv) %>%
    mean()
}

# Create rate grid from 1% to 10%
rates_grid <- seq(0.01, 0.10, by = 0.01)

# Calculate NPV for each rate
sensitivity_results <- map_dbl(rates_grid, calculate_npv)

# Create results dataframe
sensitivity_df <- tibble(
  discount_rate = rates_grid,
  mean_npv = sensitivity_results
)

# Plot sensitivity
sensitivity_plot <- ggplot(sensitivity_df, aes(x = discount_rate, y = mean_npv)) +
  geom_line(color = "#377EB8", size = 1.5) +
  geom_point(color = "#377EB8", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = dollar_format(scale = 1e-6, suffix = "M")) +
  labs(title = "Discount Rate Sensitivity Analysis",
       x = "Discount Rate",
       y = "Mean NPV ($)") +
  theme_minimal()

ggsave("plots/discount_rate_sensitivity.png", sensitivity_plot, width = 8, height = 6)
# EDA
video_data = read.table("videodata.txt", header=TRUE)
video_multiple = read.table("videoMultiple.txt", header=TRUE)

video_data[video_data == 99] <- NA
video_multiple[video_multiple == 99] <- NA

summary(video_data)
summary(video_multiple)

# QUESTION 01
# point estimate
n <- nrow(video_data)
played_count <- sum(video_data$time > 0) 
point_estimate_fraction <- played_count / n
# interval estimate
z <- 1.96
lower_interval_estimate_fraction <- point_estimate_fraction - z * sqrt(point_estimate_fraction * (1 - point_estimate_fraction) / n)
upper_interval_estimate_fraction <- point_estimate_fraction + z * sqrt(point_estimate_fraction * (1 - point_estimate_fraction) / n)

# QUESTION 02
library(ggplot2)

video_data$freq <- factor(video_data$freq, 
                          levels = c(1, 2, 3, 4), 
                          labels = c("Daily", "Weekly", "Monthly", "Semesterly"))

# Create the first histogram for 'freq'
plot_freq <- ggplot(video_data, aes(x = freq)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Frequency of Video Game Play", x = "Frequency", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create the second histogram for 'time'
plot_time <- ggplot(video_data, aes(x = time)) +
  geom_histogram(binwidth = 5, fill = "lightgreen") +
  labs(title = "Time of Video Game Play", x = "Time Played a Week Prior (hours)", y = "Count") +
  theme_minimal()

# Combine the two plots using gridExtra
library(gridExtra)
grid.arrange(plot_freq, plot_time, ncol = 2)

library(pander)
pander(summary(video_data$freq))
pander(summary(video_data$time))

# QUESTION 03
hist(video_data$time, xlab = 'Number of Hours Played in the Week Prior to the Survey', main = 'Frequency of Hours Playing Video Games')
set.seed(371)
shuffle.ind = sample(1:nrow(video_data))
boot.population <- rep(video_data$time[shuffle.ind], length.out = 314)
# Choose our first sample
sample1 <- sample(boot.population, size = 91, replace = FALSE)

# Choose 400 samples from our bootstrap population and store them in a 2D Array
# Each row represents a bootstrap sample of size 91 (we have 400 rows/samples)
# Each column represents an element (we have 91 elements)
set.seed(6653)
B = 400
boot.sample <- array(dim = c(B, 91))
for (i in 1:B) {
  boot.sample[i, ] <- sample(boot.population, size = 91, replace = FALSE)
}

# Calculate the sample mean of each bootstrap sample
boot.mean <- apply(X = boot.sample, MARGIN = 1, FUN = mean)

# Histogram of Bootstrap sample means
hist(boot.mean, breaks = 20, probability = TRUE, density = 20, col = 3, border = 3)
lines(density(boot.mean, adjust = 2), col = 2)

# QQ plot of Bootstrap sample means
par(pty = 's')
qqnorm(boot.mean)
qqline(boot.mean)

# point estimate
# Close to the sample average
point_estimate_average <- mean(boot.mean)

# Bootstrap confidence interval
# Find the values between 2.5% to 97.5% to capture the middle 95% of the data.
interval_estimate <- c(quantile(boot.mean, 0.025), quantile(boot.mean, 0.975))
lower_interval_estimate_average <- interval_estimate[[1]]
upper_interval_estimate_average <- interval_estimate[[2]]

# QUESTION 04
library(ggplot2)
# Summing counts for each category
genre_counts <- colSums(video_multiple[, c("action", "adv", "sim", "sport", "strategy")], na.rm = TRUE)
reason_counts <- colSums(video_multiple[, c("relax", "coord", "challenge", "master", "bored")], na.rm = TRUE)
dislike_counts <- colSums((video_multiple[, c("graphic", "time", "frust", "lonely", "rules", "cost", "boring", "friends", "point")]), na.rm = TRUE)

# Creating individual data frames for each category
genre_df <- data.frame(Category = names(genre_counts), Count = as.numeric(genre_counts))
reason_df <- data.frame(Reason = names(reason_counts), Count = as.numeric(reason_counts))
dislike_df <- data.frame(Reason_Dislike = names(dislike_counts), Count = as.numeric(dislike_counts))

# Creating the bar plot for game genres
ggplot(genre_df, aes(x = Category, y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Counts of Game Genres", x = "Game Genre", y = "Count") +
  theme_minimal()

# Creating the bar plot for reasons to play video games
ggplot(reason_df, aes(x = Reason, y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Counts of Reasons to Play Video Games", x = "Reason", y = "Count") +
  theme_minimal()

# Creating the bar plot for reasons to dislike video games
ggplot(dislike_df, aes(x = Reason_Dislike, y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Counts of Reasons to Dislike Video Games", x = "Reason", y = "Count") +
  theme_minimal()

reasons_played <- list()
for (col in c("graphic", "relax", "coord", "challenge", "master", "bored")){
  mean_value <- mean(video_multiple[[col]], na.rm = TRUE)
  reasons_played[[col]] <- mean_value
}

sum(is.na(video_multiple$action))

T2_pros <- sort(unlist(reasons_played), decreasing = TRUE)

disadvantages <- list()
for (col in c("time", "frust", "lonely", "rules", "cost", "bored", "friends", "point")){
  mean_value2 <- mean(video_multiple[[col]], na.rm = TRUE)
  disadvantages[[col]] <- mean_value2
}
T2_cons <- sort(unlist(disadvantages), decreasing = TRUE)

reasons_for_likeliness <- c('relaxation', 'master', 'time', 'cost')

# QUESTION 05
library(pander)
males <- which(video_data$sex == 1)
females <- which(video_data$sex == 0)

for_pay <- which(video_data$work >= 1)
no_pay <- which(video_data$work == 0)

computer <- which(video_data$own == 1)
no_computer <- which(video_data$own == 0)

pander(colSums(Filter(is.numeric, video_multiple), na.rm = TRUE))

library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

# Summarize the means and calculate standard error for each group
calculate_summary <- function(data, group_indices, group_name) {
  data %>%
    mutate(group = case_when(
      row_number() %in% group_indices ~ group_name,
      TRUE ~ "Other"
    )) %>%
    group_by(group) %>%
    summarize(
      mean_action = mean(action, na.rm = TRUE),
      mean_strategy = mean(strategy, na.rm = TRUE),
      mean_sport = mean(sport, na.rm = TRUE),
      n = n(),  # Sample size
      se_time = sd(time, na.rm = TRUE) / sqrt(n),
      se_cost = sd(cost, na.rm = TRUE) / sqrt(n),
      se_frust = sd(frust, na.rm = TRUE) / sqrt(n)
    ) %>%
    pivot_longer(cols = starts_with("mean"), names_to = "Variable", values_to = "Mean") %>%
    pivot_longer(cols = starts_with("se_"), names_to = "SE_Variable", values_to = "SE") %>%
    mutate(SE_Variable = gsub("se_", "", SE_Variable)) # Clean up SE variable names
}

# Calculate summaries for each category
sex_data <- calculate_summary(video_multiple, males, "Males")
sex_data <- bind_rows(sex_data, calculate_summary(video_multiple, females, "Females"))

pay_data <- calculate_summary(video_multiple, for_pay, "For Pay")
pay_data <- bind_rows(pay_data, calculate_summary(video_multiple, no_pay, "No Pay"))

computer_data <- calculate_summary(video_multiple, computer, "Computer")
computer_data <- bind_rows(computer_data, calculate_summary(video_multiple, no_computer, "No Computer"))

# Create bar plots with error bars
create_plot <- function(data, title) {
  ggplot(data, aes(x = group, y = Mean, fill = Variable)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
    geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                  position = position_dodge(0.7), width = 0.25) +
    labs(title = title, x = "Group", y = "Mean Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10),
          plot.title = element_text(size = 10))
}

# Create plots
sex_plot <- create_plot(sex_data, "Mean Values of Action, Strategy, and Sport by Sex")
pay_plot <- create_plot(pay_data, "Mean Values of Action, Strategy, and Sport by Pay")
computer_plot <- create_plot(computer_data, "Mean Values of Action, Strategy, and Sport by Computer Use")

# Arrange plots side by side
grid.arrange(sex_plot, pay_plot, ncol = 2)
computer_plot

# Summarize the means and calculate standard error for each group
calculate_summary <- function(data, group_indices, group_name) {
  data %>%
    mutate(group = case_when(
      row_number() %in% group_indices ~ group_name,
      TRUE ~ "Other"
    )) %>%
    group_by(group) %>%
    summarize(
      mean_time = mean(time, na.rm = TRUE),
      mean_cost = mean(cost, na.rm = TRUE),
      mean_frust = mean(frust, na.rm = TRUE),
      n = n(),  # Sample size
      se_relax = sd(relax, na.rm = TRUE) / sqrt(n),
      se_master = sd(master, na.rm = TRUE) / sqrt(n),
      se_bored = sd(bored, na.rm = TRUE) / sqrt(n)
    ) %>%
    pivot_longer(cols = starts_with("mean"), names_to = "Variable", values_to = "Mean") %>%
    pivot_longer(cols = starts_with("se_"), names_to = "SE_Variable", values_to = "SE") %>%
    mutate(SE_Variable = gsub("se_", "", SE_Variable)) # Clean up SE variable names
}

# Calculate summaries for each category
sex_data <- calculate_summary(video_multiple, males, "Males")
sex_data <- bind_rows(sex_data, calculate_summary(video_multiple, females, "Females"))

pay_data <- calculate_summary(video_multiple, for_pay, "For Pay")
pay_data <- bind_rows(pay_data, calculate_summary(video_multiple, no_pay, "No Pay"))

computer_data <- calculate_summary(video_multiple, computer, "Computer")
computer_data <- bind_rows(computer_data, calculate_summary(video_multiple, no_computer, "No Computer"))

# Create bar plots with error bars
create_plot <- function(data, title) {
  ggplot(data, aes(x = group, y = Mean, fill = Variable)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
    geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                  position = position_dodge(0.7), width = 0.25) +
    labs(title = title, x = "Group", y = "Mean Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10),
          plot.title = element_text(size = 10))
}

# Create plots
sex_plot <- create_plot(sex_data, "Mean Values of Relax, Master, and Bored by Sex")
pay_plot <- create_plot(pay_data, "Mean Values of Relax, Master, and Bored by Sex")
computer_plot <- create_plot(computer_data, "Mean Values of Relax, Master, and Bored by Sex")

# Arrange plots side by side
grid.arrange(sex_plot, pay_plot, ncol = 2)
computer_plot

# Summarize the means and calculate standard error for each group
calculate_summary <- function(data, group_indices, group_name) {
  data %>%
    mutate(group = case_when(
      row_number() %in% group_indices ~ group_name,
      TRUE ~ "Other"
    )) %>%
    group_by(group) %>%
    summarize(
      mean_time = mean(time, na.rm = TRUE),
      mean_cost = mean(cost, na.rm = TRUE),
      mean_frust = mean(frust, na.rm = TRUE),
      n = n(),  # Sample size
      se_time = sd(time, na.rm = TRUE) / sqrt(n),
      se_cost = sd(cost, na.rm = TRUE) / sqrt(n),
      se_frust = sd(frust, na.rm = TRUE) / sqrt(n)
    ) %>%
    pivot_longer(cols = starts_with("mean"), names_to = "Variable", values_to = "Mean") %>%
    pivot_longer(cols = starts_with("se_"), names_to = "SE_Variable", values_to = "SE") %>%
    mutate(SE_Variable = gsub("se_", "", SE_Variable)) # Clean up SE variable names
}

# Calculate summaries for each category
sex_data <- calculate_summary(video_multiple, males, "Males")
sex_data <- bind_rows(sex_data, calculate_summary(video_multiple, females, "Females"))

pay_data <- calculate_summary(video_multiple, for_pay, "For Pay")
pay_data <- bind_rows(pay_data, calculate_summary(video_multiple, no_pay, "No Pay"))

computer_data <- calculate_summary(video_multiple, computer, "Computer")
computer_data <- bind_rows(computer_data, calculate_summary(video_multiple, no_computer, "No Computer"))

# Create bar plots with error bars
create_plot <- function(data, title) {
  ggplot(data, aes(x = group, y = Mean, fill = Variable)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
    geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                  position = position_dodge(0.7), width = 0.25) +
    labs(title = title, x = "Group", y = "Mean Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10),
          plot.title = element_text(size = 10))
}

# Create plots
sex_plot <- create_plot(sex_data, "Mean Values of Time, Cost, and Frust by Sex")
pay_plot <- create_plot(pay_data, "Mean Values of Time, Cost, and Frust by Pay")
computer_plot <- create_plot(computer_data, "Mean Values of Time, Cost, and Frust by Computer Use")

# Arrange plots side by side
grid.arrange(sex_plot, pay_plot, ncol = 2)
computer_plot

# QUESTION 06 (EXTRA CREDIT)
library(ggplot2)
distr_of_grades <- ggplot(video_data, aes(x = as.factor(grade))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(x = "grade", y = "Count", title = "Distribution of Grades") +
  theme_minimal()

grade_prop <- prop.table(table(video_data$grade, useNA = 'no'))

counts <- table(video_data$grade, useNA = 'no')
# We need to modify the observed counts to have a 0 to account for the missing grade of D.
observed_counts <- c(0, 8, 52, 31)
chi_squared_test <- chisq.test(observed_counts, p = c(0.1, 0.4, 0.3, 0.2))

observed_counts2 <- c(4, 8, 52, 31)
chi_squared_test2 <- chisq.test(observed_counts2, p = c(0.1, 0.4, 0.3, 0.2))


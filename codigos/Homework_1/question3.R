rm(list = ls()) #Cleaning environment
options(digits = 4)

# Set your folder path correctly (adjust if necessary before running!)
library("rstudioapi") 
k <- getSourceEditorContext()$path 
setwd(dirname(k)) 
#folder_path <- "/Users/fabiomachadomilan/Estatistica/codigos/Homework_1/"
load(paste0("data_group.RData"))
data_g10 <- data_group[1:10, ]

cat("CHECKING DATA LOADING\n")
cat("   > data_g10\n")
print(head(data_g10))
cat("\n > data_group\n")
print(tail(data_group))

cat("\n")
cat("Question 3: Low usage analysis\n")

# ==============================================================================
# 1. Analyse system usage for each season of the year.
# For each season, calculate the mean, median, and standard deviation of total_user,
# and determine the proportion of days classified as low_usage. Construct a boxplot
# comparing the seasons and discuss the main differences observed.
# ==============================================================================

cat("\n3.1 Per season usage\n")
cat("\n First 10th verification")
cat("\n Season: spring (only for the 10th)\n")

data_g10_spring <- subset(data_g10, season == 1)
spring_g10_total_users <- data_g10_spring$total_user

sum_10_spring <- sum(spring_g10_total_users)
n_10_spring <- length(spring_g10_total_users)
manual_mean_10spring <- sum_10_spring / n_10_spring

mean_10spring <- mean(spring_g10_total_users)
cat("   Manual mean = ", sum_10_spring, " / ", n_10_spring, " = ", mean_10spring, "\n", sep = "")

spring_g10_total_users_ordered <- sort(spring_g10_total_users)
cat("   Ordered values: ", spring_g10_total_users_ordered, "\n", sep = " ")
manual_median_10spring <- (spring_g10_total_users_ordered[5] + spring_g10_total_users_ordered[6]) / 2
cat("   Median = (", spring_g10_total_users_ordered[5], " + ", spring_g10_total_users_ordered[6], ") / 2 = ", manual_median_10spring, "\n", sep = "")

median_10spring <- median(spring_g10_total_users)


squared_deviations_10spring <- (spring_g10_total_users - manual_mean_10spring)^2
sum_sqr_dev_10spring <- sum(squared_deviations_10spring)
manual_variance_10spring <- sum_sqr_dev_10spring / (n_10_spring - 1)
manual_std_10spring <- sqrt(manual_variance_10spring)
cat("   Standard Deviation = ", manual_std_10spring, "\n", sep = "")

std_10spring <- sd(spring_g10_total_users)


manual_prop_low_10spring <- sum(data_g10_spring$low_usage) / nrow(data_g10_spring)
cat("   Proportion of low_usage days = ", manual_prop_low_10spring, "\n", sep = "")

prop_low_10spring <- mean(data_g10_spring$low_usage)

cat("\n   COMPARATIONS")
cat("\n   Manual mean: ", manual_mean_10spring, " | R: ", mean_10spring, " | Diference: ", abs(manual_mean_10spring - mean_10spring), "\n", sep = "")
cat("   Manual median: ", manual_median_10spring, " | R: ", median_10spring, " | Diference: ", abs(manual_median_10spring - median_10spring), "\n", sep = "")
cat("   Manual std: ", manual_std_10spring, " | R: ", std_10spring, " | Diference: ", abs(manual_std_10spring - std_10spring), "\n", sep = "")
cat("   Manual low_usage days: ", manual_prop_low_10spring, " | R: ", prop_low_10spring, " | Diference: ", abs(manual_prop_low_10spring - prop_low_10spring), "\n", sep = "")

cat("\n All data_group")
cat("\n Season: SPRING\n")

data_spring <- subset(data_group, season == 1)
spring_total_users <- data_spring$total_user
mean_spring <- mean(spring_total_users)
median_spring <- median(spring_total_users)
std_spring <- sd(spring_total_users)
prop_low_spring <- mean(data_spring$low_usage)

cat("     n: ", length(spring_total_users),  "\n", sep = "")
cat("     mean: ", mean_spring,  "\n", sep = "")
cat("     median: ", median_spring,  "\n", sep = "")
cat("     std: ", std_spring,  "\n", sep = "")
cat("     low_usage days: ", prop_low_spring,  "\n", sep = "")

cat("\n Season: SUMMER\n")

data_summer <- subset(data_group, season == 2)
summer_total_users <- data_summer$total_user
mean_summer <- mean(summer_total_users)
median_summer <- median(summer_total_users)
std_summer <- sd(summer_total_users)
prop_low_summer <- mean(data_summer$low_usage)

cat("     n: ", length(summer_total_users),  "\n", sep = "")
cat("     mean: ", mean_summer,  "\n", sep = "")
cat("     median: ", median_summer,  "\n", sep = "")
cat("     std: ", std_summer,  "\n", sep = "")
cat("     low_usage days: ", prop_low_summer,  "\n", sep = "")

cat("\n Season: AUTUMN\n")

data_autumn <- subset(data_group, season == 3)
autumn_total_users <- data_autumn$total_user
mean_autumn <- mean(autumn_total_users)
median_autumn <- median(autumn_total_users)
std_autumn <- sd(autumn_total_users)
prop_low_autumn <- mean(data_autumn$low_usage)

cat("     n: ", length(autumn_total_users),  "\n", sep = "")
cat("     mean: ", mean_autumn,  "\n", sep = "")
cat("     median: ", median_autumn,  "\n", sep = "")
cat("     std: ", std_autumn,  "\n", sep = "")
cat("     low_usage days: ", prop_low_autumn,  "\n", sep = "")

cat("\n Season: WINTER\n")

data_winter <- subset(data_group, season == 4)
winter_total_users <- data_winter$total_user
mean_winter <- mean(winter_total_users)
median_winter <- median(winter_total_users)
std_winter <- sd(winter_total_users)
prop_low_winter <- mean(data_winter$low_usage)

cat("     n: ", length(winter_total_users),  "\n", sep = "")
cat("     mean: ", mean_winter,  "\n", sep = "")
cat("     median: ", median_winter,  "\n", sep = "")
cat("     std: ", std_winter,  "\n", sep = "")
cat("     low_usage days: ", prop_low_winter,  "\n", sep = "")

pdf(file = "/Users/fabiomachadomilan/Estatistica/Docs/assets/boxplot_season.pdf", width = 8, height = 6)

data_group$season_name <- factor(data_group$season, 
                                levels = c(1, 2, 3, 4), 
                                labels = c("Spring", "Summer", "Autumn", "Winter"))

boxplot(total_user ~ season_name, 
        data = data_group,
        main = "Boxplot: Total Users per Season",
        xlab = "Season",
        ylab = "Total Users",
        col = c("lightgreen", "orange", "yellow", "lightblue"))

dev.off()

# ==============================================================================
# 2. Analyse system usage as a function of weather conditions. 
# For each condition, calculate the mean and standard deviation
# of total_user, and determine the proportion of days classified as low_usage. 
# Use appropriate statistical measures and plots to compare the weather conditions 
# (“Clear”, “Cloudy”, “Light Rain”, and “Heavy Rain).
# ==============================================================================

cat("\n3.2 Per weather condition usage\n")
cat("\n First 10th verification")
cat("\n Weather Condition: clear \n")

data_g10_clear <- subset(data_g10, weathersit == 1)
clear_g10_total_users <- data_g10_clear$total_user

sum_10_clear <- sum(clear_g10_total_users)
n_10_clear <- length(clear_g10_total_users)
manual_mean_10clear <- sum_10_clear / n_10_clear

mean_10clear <- mean(clear_g10_total_users)
cat("   Manual mean = ", sum_10_clear, " / ", n_10_clear, " = ", mean_10clear, "\n", sep = "")

squared_deviations_10clear <- (clear_g10_total_users - manual_mean_10clear)^2
sum_sqr_dev_10clear <- sum(squared_deviations_10clear)
manual_variance_10clear <- sum_sqr_dev_10clear / (n_10_clear - 1)
manual_std_10clear <- sqrt(manual_variance_10clear)
cat("   Standard Deviation = ", manual_std_10clear, "\n", sep = "")

std_10clear <- sd(clear_g10_total_users)

# Added low usage for 10th clear
manual_prop_low_10clear <- sum(data_g10_clear$low_usage) / nrow(data_g10_clear)
prop_low_10clear <- mean(data_g10_clear$low_usage)

cat("\n   COMPARATIONS")
cat("\n   Manual mean: ", manual_mean_10clear, " | R: ", mean_10clear, " | Diference: ", abs(manual_mean_10clear - mean_10clear), "\n", sep = "")
cat("   Manual std: ", manual_std_10clear, " | R: ", std_10clear, " | Diference: ", abs(manual_std_10clear - std_10clear), "\n", sep = "")
cat("   Manual low_usage: ", manual_prop_low_10clear, " | R: ", prop_low_10clear, " | Diference: ", abs(manual_prop_low_10clear - prop_low_10clear), "\n", sep = "")

cat("\n Weather Condition: cloudy \n")

data_g10_cloudy <- subset(data_g10, weathersit == 2)
cloudy_g10_total_users <- data_g10_cloudy$total_user

sum_10_cloudy <- sum(cloudy_g10_total_users)
n_10_cloudy <- length(cloudy_g10_total_users)
manual_mean_10cloudy <- sum_10_cloudy / n_10_cloudy

mean_10cloudy <- mean(cloudy_g10_total_users)
cat("   Manual mean = ", sum_10_cloudy, " / ", n_10_cloudy, " = ", mean_10cloudy, "\n", sep = "")

squared_deviations_10cloudy <- (cloudy_g10_total_users - manual_mean_10cloudy)^2
sum_sqr_dev_10cloudy <- sum(squared_deviations_10cloudy)
manual_variance_10cloudy <- sum_sqr_dev_10cloudy / (n_10_cloudy - 1)
manual_std_10cloudy <- sqrt(manual_variance_10cloudy)
cat("   Standard Deviation = ", manual_std_10cloudy, "\n", sep = "")

std_10cloudy <- sd(cloudy_g10_total_users)
  
# Added low usage for 10th cloudy
manual_prop_low_10cloudy <- sum(data_g10_cloudy$low_usage) / nrow(data_g10_cloudy)
prop_low_10cloudy <- mean(data_g10_cloudy$low_usage)
  
cat("\n   COMPARATIONS")
cat("\n   Manual mean: ", manual_mean_10cloudy, " | R: ", mean_10cloudy, " | Diference: ", abs(manual_mean_10cloudy - mean_10cloudy), "\n", sep = "")
cat("   Manual std: ", manual_std_10cloudy, " | R: ", std_10cloudy, " | Diference: ", abs(manual_std_10cloudy - std_10cloudy), "\n", sep = "")
cat("   Manual low_usage: ", manual_prop_low_10cloudy, " | R: ", prop_low_10cloudy, " | Diference: ", abs(manual_prop_low_10cloudy - prop_low_10cloudy), "\n", sep = "")

cat("\n All data_group")
cat("\n Weather Condition: CLEAR\n")

data_clear <- subset(data_group, weathersit == 1)
clear_total_users <- data_clear$total_user

mean_clear <- mean(clear_total_users)
std_clear <- sd(clear_total_users)
prop_low_clear <- mean(data_clear$low_usage)
cat("     n: ", length(clear_total_users),  "\n", sep = "")
cat("     mean: ", mean_clear,  "\n", sep = "")
cat("     std: ", std_clear,  "\n", sep = "")
cat("     low_usage days: ", prop_low_clear, "\n", sep = "")

cat("\n Weather Condition: CLOUDY\n")

data_cloudy <- subset(data_group, weathersit == 2)
cloudy_total_users <- data_cloudy$total_user

mean_cloudy <- mean(cloudy_total_users)
std_cloudy <- sd(cloudy_total_users)
prop_low_cloudy <- mean(data_cloudy$low_usage)
cat("     n: ", length(cloudy_total_users),  "\n", sep = "")
cat("     mean: ", mean_cloudy,  "\n", sep = "")
cat("     std: ", std_cloudy,  "\n", sep = "")
cat("     low_usage days: ", prop_low_cloudy, "\n", sep = "")


cat("\n Weather Condition: LIGHT RAIN\n")

data_light_rain <- subset(data_group, weathersit == 3)
light_rain_total_users <- data_light_rain$total_user

mean_light_rain <- mean(light_rain_total_users)
std_light_rain <- sd(light_rain_total_users)
prop_low_light_rain <- mean(data_light_rain$low_usage)
cat("     n: ", length(light_rain_total_users),  "\n", sep = "")
cat("     mean: ", mean_light_rain,  "\n", sep = "")
cat("     std: ", std_light_rain,  "\n", sep = "")
cat("     low_usage days: ", prop_low_light_rain, "\n", sep = "")


cat("\n Weather Condition: HEAVY RAIN\n")

data_heavy_rain <- subset(data_group, weathersit == 4)
# Checked with na.rm = TRUE in case category 4 has 0 days
if(nrow(data_heavy_rain) > 0) {
  heavy_rain_total_users <- data_heavy_rain$total_user
  mean_heavy_rain <- mean(heavy_rain_total_users)
  std_heavy_rain <- sd(heavy_rain_total_users)
  prop_low_heavy_rain <- mean(data_heavy_rain$low_usage)
  
  cat("     n: ", length(heavy_rain_total_users),  "\n", sep = "")
  cat("     mean: ", mean_heavy_rain,  "\n", sep = "")
  cat("     std: ", std_heavy_rain,  "\n", sep = "")
  cat("     low_usage days: ", prop_low_heavy_rain, "\n", sep = "")
} else {
  cat("     No Heavy Rain days in the dataset.\n")
}

pdf(file = "/Users/fabiomachadomilan/Estatistica/Docs/assets/boxplot_weather.pdf", width = 7, height = 4.5)

# Plotting the Weather condition comparison
data_group$weather_name <- factor(data_group$weathersit, 
                                  levels = c(1, 2, 3, 4), 
                                  labels = c("Clear", "Cloudy", "Light Rain", "Heavy Rain"))

boxplot(total_user ~ weather_name, 
        data = data_group,
        main = "Boxplot: Total Users per Weather",
        xlab = "Weather Condition",
        ylab = "Total Users",
        col = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c"))
dev.off()

# ==============================================================================
# 3. Investigate the relationship between temperature and total_user.
# Construct a scatter plot and calculate the correlation coefficient.
# Interpret the direction and strength of the association and discuss possible
# limitations of the correlation coefficient in describing the observed relationship.
# ==============================================================================
cat("\n3.3 Temperature vs Total Users Correlation\n")
cat("\n First 10th verification\n")

temp_10 <- data_g10$temp
users_10 <- data_g10$total_user
n_10 <- length(temp_10)

mean_temp_10 <- mean(temp_10)
mean_users_10 <- mean(users_10)

# Deviations from the mean
dev_temp_10 <- temp_10 - mean_temp_10
dev_users_10 <- users_10 - mean_users_10

sum_prod_10 <- sum(dev_temp_10 * dev_users_10)
sum_sq_temp_10 <- sum(dev_temp_10^2)
sum_sq_users_10 <- sum(dev_users_10^2)

# Manual Pearson correlation formula
manual_r_10 <- sum_prod_10 / sqrt(sum_sq_temp_10 * sum_sq_users_10)
r_10 <- cor(temp_10, users_10)

cat("   Manual r = ", manual_r_10, "\n", sep="")

cat("\n   COMPARATIONS\n")
cat("   Manual r: ", manual_r_10, " | R: ", r_10, " | Diference: ", abs(manual_r_10 - r_10), "\n", sep="")


cat("\n All data_group\n")
# Cor in R for the full dataset
r_all <- cor(data_group$temp, data_group$total_user)
cat("   Correlation coefficient (r) = ", r_all, "\n", sep="")

pdf(file = "/Users/fabiomachadomilan/Estatistica/Docs/assets/scatter_temp.pdf", width = 12, height = 9)

# Scatter plot for total_user vs Temperature
plot(data_group$temp, data_group$total_user,
     pch = 19, col = "darkblue",
     main = "Scatter Plot: Temperature vs. Total Users",
     xlab = "Temperature", ylab = "Total Users")
dev.off()


# ==============================================================================
# 4. Based on the analyses performed in this question, 
# select two characteristics that appear to be most strongly 
# associated with system usage. Justify your choice based on 
# the statistical and/or graphical results.
# ==============================================================================

cat("\n3.4 Final Feature Selection\n")
cat("\n My choice: Temperature and Weather Conditions.\n")
cat(" Reason: Linear correlation confirmed by 'r' and scatter plot. Boxplots show maximum values declining when tending to rain, which makes a lot of sense since it is a bike system.\n")

pdf(file = "/Users/fabiomachadomilan/Estatistica/Docs/assets/boxplots_temp.pdf", width = 14, height = 4.5)
# Putting the final justification plots side-by-side
par(mfrow = c(1, 2))

# Plot A: Temperature by Season
boxplot(temp ~ season_name, 
        data = data_group,
        main = "Temperature by Season", 
        xlab = "Season", 
        ylab = "Temperature",
        col = c("lightgreen", "orange", "yellow", "lightblue"))

# Plot B: Temperature by Weather Condition
boxplot(temp ~ weather_name, 
        data = data_group,
        main = "Temperature by Weather Condition", 
        xlab = "Weather Condition", 
        ylab = "Temperature",
        col = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c"))

par(mfrow = c(1, 1)) # Reset plot window
dev.off()
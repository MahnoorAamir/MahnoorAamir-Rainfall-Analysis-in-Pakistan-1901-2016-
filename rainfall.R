


# Load the libraries
library(ggplot2)
library(dplyr)

# Load the dataset
rainfall_data <- read.csv("rainfall_1901_2016_pak.csv")

# View the first few rows of the dataset
head(rainfall_data)
View(rainfall_data)
# Convert Month to a factor with proper ordering
rainfall_data$Month <- factor(rainfall_data$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

# Create a Date column for better plotting
rainfall_data$Date <- as.Date(with(rainfall_data, paste(Year, Month, "1", sep = "-")), "%Y-%B-%d")


# Plot monthly rainfall over time
ggplot(rainfall_data, aes(x = Date, y = Rainfall....MM.)) +
  geom_line(color = "blue") +
  labs(title = "Monthly Rainfall in Pakistan (1901-2016)",
       x = "Year",
       y = "Rainfall (mm)") +
  theme_minimal()


# Calculate annual rainfall
annual_rainfall <- rainfall_data %>%
  group_by(Year) %>%
  summarise(Total_Rainfall = sum(Rainfall....MM.))


# Plot annual rainfall over time
ggplot(annual_rainfall, aes(x = Year, y = Total_Rainfall)) +
  geom_line(color = "red") +
  labs(title = "Annual Rainfall in Pakistan (1901-2016)",
       x = "Year",
       y = "Total Rainfall (mm)") +
  theme_minimal()

# Boxplot of monthly rainfall
ggplot(rainfall_data, aes(x = Month, y =Rainfall....MM.)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Monthly Rainfall Distribution in Pakistan (1901-2016)",
       x = "Month",
       y = "Rainfall (mm)") +
  theme_minimal()

# Aggregate data by month
monthly_rainfall <- rainfall_data %>%
  group_by(Month) %>%
  summarise(Average_Rainfall = mean(Rainfall....MM.))


# Plot average monthly rainfall
ggplot(monthly_rainfall, aes(x = Month, y = Average_Rainfall)) +
  geom_bar(stat = "identity", fill = "#808080") +
  labs(title = "Average Monthly Rainfall in Pakistan (1901-2016)",
       x = "Month",
       y = "Average Rainfall (mm)") +
  theme_minimal() +
  scale_x_discrete(labels = month.abb)


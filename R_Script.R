#import libraries 
library(readr)
library(ggplot2)
library(forecast)
library(stats)
#import data from csv file 
data <- read_csv("Housing_Market_EU_dataset.csv")
View(data)
summary(data)
data_Infla <- read_csv("Inflation_EU_dataset.csv")
View(data_Infla)
data_Const <- read_csv("Construction_Price _EU.csv")
View(data_Const)

#cleaning and processing data 
clean_data <- na.omit(data)
clean_data$OBS_VALUE <- as.numeric(clean_data$OBS_VALUE, errors = "coerce")
data_filtered_const <- data_Const[complete.cases(data_Const$OBS_VALUE), ]

#analyzing data :

#plot1 housing prices % time period  
ggplot(data, aes(x = TIME_PERIOD, y = OBS_VALUE)) +
  geom_line() +
  geom_hline(yintercept = 100, color = "green", linetype = "dashed") +
  geom_hline(yintercept = 75, color = "green", linetype = "dashed") +
  geom_vline(xintercept = 2015, color = "red", linetype = "dashed") +
  labs(
    title = "House Prices in EU Countries",
    x = "Time Period",
    y = "House Price Index"
  ) +
  theme_light()

#plot2 inflation rate in the EU % time period

ggplot(data_Infla, aes(x = TIME_PERIOD, y = OBS_VALUE)) +
  geom_line() +
  geom_vline(xintercept = 2015, color = "red", linetype = "dashed") +
  labs(
    title = "Inflation in EU Countries",
    x = "Time Period",
    y = "Inflation rate Index"
  ) +
  theme_light()+
  scale_x_continuous(
    breaks = seq(min(data_Infla$TIME_PERIOD), max(data_Infla$TIME_PERIOD), by = 2),  # Espacement de 2 entre les valeurs
    labels = seq(min(data_Infla$TIME_PERIOD), max(data_Infla$TIME_PERIOD), by = 2)  # Étiquettes personnalisées
  )+
  scale_y_continuous(
    breaks = seq(min(data_Infla$OBS_VALUE), max(data_Infla$OBS_VALUE), by = 5),  # Espacement de 2 entre les valeurs
    labels = seq(min(data_Infla$OBS_VALUE), max(data_Infla$OBS_VALUE), by = 5)  # Étiquettes personnalisées
  )

#plot3 construction price in the EU % time period

ggplot(data_filtered_const, aes(x = TIME_PERIOD, y = OBS_VALUE)) +
  geom_line() +
  geom_hline(yintercept = 100, color = "green", linetype = "dashed") +
  geom_hline(yintercept = 75, color = "green", linetype = "dashed") +
  geom_vline(xintercept = 2015, color = "red", linetype = "dashed") +
  labs(
    title = "Construction price in EU Countries",
    x = "Time Period",
    y = "contruction price Index"
  ) +
  theme_light()+
  scale_x_continuous(
    breaks = seq(min(data_filtered_const$TIME_PERIOD), max(data_filtered_const$TIME_PERIOD), by = 2),  # Espacement de 2 entre les valeurs
    labels = seq(min(data_filtered_const$TIME_PERIOD), max(data_filtered_const$TIME_PERIOD), by = 2)  # Étiquettes personnalisées
  )





#check for correlation between housing prices and inflation : 

  #data and data_infl dont have the same dimension so we r gonna subset from the bigger one 

n_rows_data <- nrow(data)
n_rows_data_Infla <- nrow(data_Infla)
print(n_rows_data)
print(n_rows_data_Infla )
min_length <- min(n_rows_data, n_rows_data_Infla)
data_subset <- head(data, n = min_length)

 #calcul correlation 

correlation <- cor(data_subset$OBS_VALUE, data_Infla$OBS_VALUE)

# Plot a scatter plot to visualize the correlation
plot(data_subset$OBS_VALUE,data_Infla$OBS_VALUE, 
     main = paste("Correlation =", round(correlation, 2)),
     xlab = "Housing Prices Index", ylab = "Inflation Index")

# Add a trendline to the scatter plot
abline(lm(data_subset$OBS_VALUE ~ data_Infla$OBS_VALUE), col = "red",lwd = 3)






#check for correlation between housing prices and construction price index : 

  #data and data_cont dont have the same dimension  so we r gonna subset from the bigger one 
  # data_cont have Null values in the column OBS_VALUE so we r gonna filter them out 

n_rows_data <- nrow(data) 
n_rows_data_Const <- nrow(data_filtered_const)
print(n_rows_data)
print(n_rows_data_Const )
min_length <- min(n_rows_data, n_rows_data_Const)
data_subset <- head(data, n = min_length)

  #the follwing test to check is the column has been succesfullt filtered 

if (anyNA(data_subset$OBS_VALUE) || anyNA(data_filtered_const$OBS_VALUE)) {
  stop("There are missing values in the data. Please handle them before calculating correlation.")
}

#calcul correlation 

correlation <- cor(data_subset$OBS_VALUE, data_filtered_const$OBS_VALUE)
print(correlation)

# Plot a scatter plot to visualize the correlation
plot(data_subset$OBS_VALUE,data_filtered_const$OBS_VALUE, 
     main = paste("Correlation =", round(correlation, 2)),
     xlab = "Housing Prices Index", ylab = "Construction prices Index")

# Add a trendline to the scatter plot
abline(lm(data_subset$OBS_VALUE ~ data_filtered_const$OBS_VALUE), col = "red",lwd = 3)



#prediction of the behavior of the Housing market in the following years (would it stay in crisis or would the prices be regulated)

#create a time series object 
data_ts <- ts(data$OBS_VALUE, start = 2005, end = 2021, frequency = 1)

# Train an ARIMA model
arima_model <- auto.arima(data_ts)

# Make predictions using the ARIMA model
forecast_values <- forecast(arima_model, h = 5)  # Forecasting the next 3 periods
print(forecast_values)



# Visualize the original time series data and the forecasted values
plot(forecast_values, main = "ARIMA Forecast", xlab = "Time", ylab = "Housing Price Index", ylim = c(min(data_ts), max(forecast_values$upper)))
lines(data_ts, col = "blue")  # Original data
lines(forecast_values$mean, col = "red")     # Forecasted values
legend("topright", legend = c("Original", "Forecast"), col = c("blue", "red"), lty = 1)


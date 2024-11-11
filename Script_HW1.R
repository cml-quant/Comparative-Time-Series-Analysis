# install.packages("moments")
# install.packages("gridExtra")
# install.packages("png")
library(moments)
library(gridExtra)
library(grid)

data <- read.csv('./amd_nvidia.csv', header = TRUE, sep = ',')


Nvidia <- data[data$PERMNO == 86580,]
AMD <- data[data$PERMNO == 61241,]
Adjusted_price_nvidia <- Nvidia$PRC / Nvidia$CFACPR
Adjusted_price_amd <- AMD$PRC / AMD$CFACPR


AMD$date <- as.Date(as.character(AMD$date), format = "%Y%m%d")
Nvidia$date <- as.Date(as.character(Nvidia$date), format = "%Y%m%d")

AMD_Return <- c(NA, diff(log(Adjusted_price_amd)))
Nvidia_Return <- c(NA, diff(log(Adjusted_price_nvidia)))


update.packages(ask = FALSE)

mean_amd <- mean(AMD_Return, na.rm = TRUE)
sd_amd <- sd(AMD_Return, na.rm = TRUE)
skew_amd <- skewness(AMD_Return, na.rm = TRUE)
kurt_amd <- kurtosis(AMD_Return, na.rm = TRUE)

mean_nvda <- mean(Nvidia_Return, na.rm = TRUE)
sd_nvda <- sd(Nvidia_Return, na.rm = TRUE)
skew_nvda <- skewness(Nvidia_Return, na.rm = TRUE)
kurt_nvda <- kurtosis(Nvidia_Return, na.rm = TRUE)

summary_stats <- data.frame(
  Stock = c("AMD", "Nvidia"),
  Mean = c(mean_amd, mean_nvda),
  SD = c(sd_amd, sd_nvda),
  Skewness = c(skew_amd, skew_nvda),
  Kurtosis = c(kurt_amd, kurt_nvda)
)
print(summary_stats)

correlation <- cor(AMD_Return, Nvidia_Return, use = "complete.obs")
print(correlation)


plot(AMD$date, Adjusted_price_amd, type = 'l', col = "darkblue", main = 'Adjusted Price of AMD (Fig.1)', xlab = 'Date', ylab = 'Adjusted Price')
plot(Nvidia$date, Adjusted_price_nvidia, type = 'l', col = "brown", main = 'Adjusted Price of Nvidia (Fig.2)', xlab = 'Date', ylab = 'Adjusted Price')
plot(AMD$date[-1], AMD_Return[-1], type = 'l', col = 'darkblue', main = 'Daily Log Returns of AMD (Fig.3)', xlab = 'Date', ylab = 'Log Return')
plot(Nvidia$date[-1], Nvidia_Return[-1], type = 'l', col = 'brown', main = 'Daily Log Returns of Nvidia (Fig.4)', xlab = 'Date', ylab = 'Log Return')

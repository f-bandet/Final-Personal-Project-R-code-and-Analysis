# Name: Faye Bandet
# Date: 12/3/2019
# ISTA 116 Section <B> || Jacob Heller
# Final Project R Script

# GLOBAL:
# Car speed text files can be opened if saved in Documents or sometimes user files. I tested this program on multiple computers, and each one saved the files in different locations, which affected the program's ability to open files to run the program. As long as files can be accessed and opened the program runs perfectly.
car.speeds.100 <- read.delim("car-speeds-100.txt")
car.speeds <- read.delim("car-speeds.txt")
car.speeds     # car.speeds data set has 500 obs, which is why '500' is used in naming vars.
car.speeds.100
spLim <- 25 # Speed limit in miles per hour (mph).
up <- subset(car.speeds, direction == "Up")   
num.Up <- nrow(up)  # 250 
down <- subset(car.speeds, direction == "Down")
num.Down <- nrow(down) # 250

# Histograms:                            
hUp <- hist(up$speed, probability = TRUE)   
hDown <- hist(down$speed, probability = TRUE)
h100 <- hist(car.speeds.100$speed.mph., probability = TRUE)
h500 <- hist(car.speeds$speed, probability = TRUE)
# The histogram of car.speeds.100 appears to have a right skew and a normal fairly unimodal distribution. 
# The histogram of car.speeds has a very slight right skew but appears strongly unimodal, with normal distribution. 
# The histogram of car.speeds-Up doesnt have visible skew, and appears unimodal, with normal distribution. 
# The histogram of car.speeds-Down has right skew and appears unimodal, with normal distribution.

# Q-Q PLOTS:
plot(qqnorm(up$speed))
plot(qqnorm(down$speed))
plot(qqnorm(car.speeds$speed))
plot(qqnorm(car.speeds.100$speed.mph.))
# Show distributions and check varience/linearity

# CONTINGENCY TABLES:
car.speeds$speeding <- factor(ifelse(car.speeds$speed < 25, 'not speeding', 'speeding'))
table(car.speeds$speeding)     # not speeding = 296,  speeding = 204
car.speeds.100$speeding <- factor(ifelse(car.speeds.100$speed.mph. < 25, 'not speeding', 'speeding'))
table(car.speeds.100$speeding) # not speeding = 65, speeding = 35
car.speeds.up <- ifelse(up$speed < 25, 'not speeding', 'speeding')
table(car.speeds.up)           # not speeding = 120, speeding = 130
car.speeds.down <- ifelse(down$speed < 25, 'not speeding', 'speeding')
table(car.speeds.down)         # not speeding = 176,  speeding = 74

# PROPORTION TABLES:
prop.table(table(car.speeds.100$speed.mph. > spLim))
prop.table(table(car.speeds$speed > spLim))
prop.table(table(car.speeds.up)) 
prop.table(table(car.speeds.down))
# In the car.speeds.100 data set 35% of drivers were speeding, and 65% of drivers drove at or under the speed limit. 
# In the car.speeds data set 40.8% of drivers were speeding and 59.2% of drivers drove at or under the speed limit. 
# In the car.speeds.up data set 52% of drivers were speeding and 48% of drivers drove at or under the speed limit. 
# In the car.speeds.down data set 29.6% of drivers were speeding and 70.4% of drivers drove at or under the speed limit.

# PROPORTION TEST:
prop.test(num.Up, num.Down, p = NULL, correct = TRUE)
# 1-sample proportions test with continuity correction
# data:  num.Up out of num.Down, null probability 0.5
# X-squared = 248, df = 1, p-value < 2.2e-16
# alternative hypothesis: true p is not equal to 0.5
# 95 percent confidence interval: 0.9811361 - 1.0000000

# T-TESTS (one sample, then two sample):
t.test(car.speeds$speed, conf.level = .95)
# t = 135.89, df = 499, p-value < 2.2e-16
# 95 percent confidence interval:  23.63762 - 24.33118
# mean = 23.9844 (within the confidance interval)
t.test(car.speeds.100$speed.mph., conf.level = .95)
# t = 66.915, df = 99, p-value < 2.2e-16
# 95 percent confidence interval:  23.13686 - 24.55094
# mean = 23.8439 (within the confidance interval)
t.test(up$speed, down$speed)
# t = 7.575, df = 496.06, p-value = 1.771e-13
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:  1.877221 - 3.192059
# mean of x = 25.25172, mean of y = 22.71708
t.test(car.speeds$speed, car.speeds.100$speed.mph.)
# t = 0.35332, df = 151.73, p-value = 0.7243
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:  -0.6451485  - 0.9261485
# mean of x = 23.9844, mean of y = 23.8439

# STANDARD DEVIATION:
sd(car.speeds$speed)           # 3.946692
sd(car.speeds.100$speed.mph.)  # 3.563338
sd(table(down))                # 0.6068254
sd(table(up))                  # 0.6023039

# STANDARD ERROR, CRITICAL VALUES:
se.500 <- sd(car.speeds$speed)/sqrt(length(car.speeds$speed))
se.500   # 0.1765014
se.100 <- sd(car.speeds.100$speed.mph.)/sqrt(length(car.speeds.100$speed.mph.))
se.100   # 0.3563338
se.up <- sd(up$speed)/sqrt(length(up$speed))
se.up    # 0.2438958
se.down <- sd(down$speed)/sqrt(length(down$speed))
se.down  # 0.2290758
cv.500 <- qnorm((1-(1-.95)/2)*se.500)
cv.500   # -0.9459427
cv.100 <- qnorm((1-(1-.95)/2)*se.100)
cv.100   # -0.3922807
cv.up <- qnorm((1-(1-.95)/2)*se.up)
cv.up    #-0.7134025
cv.down <- qnorm((1-(1-.95)/2)*se.down)
cv.down  #-0.7609318

# MODES:
table(round(up$speed))   # 25 mph with 31 obs
table(round(down$speed)) # 23 mph with 33 obs
table(round(car.speeds.100$speed.mph.)) # 22 mph and 24 mph are both mode with 15 obs each.  
table(round(car.speeds$speed)) # 25 mph with 56 obs
# Most common speeds are at or below the spLim. Note that observations are rounded to the nearest whole number.

# SUMMARY of DATA SETS:
summary(car.speeds.100$speed.mph.)
# Min.   1st Qu.  Median  Mean  3rd Qu.   Max. 
# 16.27   21.56   23.52   23.84   25.73   34.06 
summary(car.speeds$speed)
# Min.  1st Qu.  Median   Mean   3rd Qu.   Max. 
# 10.27   21.52   24.02   23.98   26.45   34.97
summary(up)
# Min.  1st Qu.  Median   Mean   3rd Qu.   Max.   Observations
# 15.08   22.50   25.16   25.25   28.16   34.97   250
summary(down)
# Min.  1st Qu.  Median   Mean   3rd Qu.   Max.   Observations
# 10.27   20.47   22.89   22.72   25.35   32.95   250

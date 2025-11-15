############################################################
# Introduction to Data Analysis
# Source: Data Analysis 1.pdf 
# Packages used:
#   - foreign : import SPSS .sav files
#   - psych   : descriptive statistics
#   - moments : skewness and kurtosis
#   - ggplot2 : graphs and visualisation
############################################################

# ---- Install & load packages ----
pkgs <- c("foreign", "psych", "moments", "ggplot2")
new_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs)

library(foreign)  # SPSS import
library(psych)    # descriptive stats
library(moments)  # skewness, kurtosis
library(ggplot2)  # plotting

# ---- Working directory (Slides pp. 21–22) ----
# Check the working directory
getwd()
# Change the working directory (Windows chooser)
# setwd("path given as a results of getwd()")

# ---- SPSS files: read 'lottery.sav' (Slide p. 23) ----
# Import the specific file (select lottery.sav)
lottery <- read.spss(file.choose(), to.data.frame = TRUE)

# ---- Data structure & view (Slides pp. 24–25) ----
str(lottery)                 # -> Inspect variable types and levels.
View(lottery)                # -> Opens data viewer.

# ---- Managing R Packages (Slides pp. 26–30) ----
library()                    # -> Lists available packages.
search()                     # -> Shows currently attached packages.
library(help = "psych")      # -> Opens psych documentation in Help pane.
update.packages()            # -> Checks for updates (expect prompts).
detach("package:psych", unload = TRUE)  # -> Demonstrates removing a package.

# ---- Keeping track of your work (Slides pp. 31–32) ----
savehistory(file = "mylog.Rhistory")  # -> Saves console history to file.
history()                             # -> Displays last ~25 commands.

# ---- Data screening: missing data (Slides pp. 34–37) ----
rowSums(is.na(lottery))      # -> NAs by row; large counts flag problematic cases.
colSums(is.na(lottery))      # -> NAs by variable; guides cleaning priorities.
# List rows with missing values
lottery[!complete.cases(lottery), ]
# Create a new dataset without missing data
lottery1 <- na.omit(lottery) # -> Use when analysis must exclude any missingness.
lottery1[!complete.cases(lottery1),] # -> no missing cases!!!

# ---- Data manipulation (Slides pp. 38–43) ----
# Subsetting variables
lottery2 <- lottery[, 1:4]   # first 4 variables
lottery3 <- lottery[c("Sex", "Years", "Income")]
lottery3 <- lottery[c(1, 2, 5)]  # selecting chosen variables (by index)

# Subsetting observations
lottery4 <- lottery[34:43, ]  # select 10 observations
lottery5 <- lottery[which(lottery$Sex == "Male"), ]  # males only

# Subsetting observations and variables
lottery6 <- lottery[1:30, 1:4]  # first 30 rows, first 4 variables

# Subsetting using subset()
lottery7 <- subset(lottery, Sex == "Female" & Lottery == "Yes")

# ---- Frequencies (Slides pp. 48–55) ----
# Absolute frequencies
summary(lottery$LotteryPlaying)  # -> Modal category = largest count.

# Absolute frequencies graph
plot(lottery$LotteryPlaying,
     xlab = "Frequency of playing lottery",
     ylab = "Absolute frequency", ylim = c(0, 250))
# -> Taller bars indicate more common categories (descriptive only).

# Relative frequencies
lotteryPlaying.table <- table(lottery$LotteryPlaying)
relfreq <- prop.table(lotteryPlaying.table)
relfreq                        # -> Shares per category; sum = 1.

# Relative frequencies graphs
plot(relfreq, xlab = "Frequency of playing lottery",
     ylab = "Relative frequency", type = "l")
plot(relfreq, xlab = "Frequency of playing lottery",
     ylab = "Relative frequency", type = "b")
plot(relfreq, xlab = "Frequency of playing lottery",
     ylab = "Relative frequency", type = "h")
# NOTE (from slide p. 54): it shows `lottery$lotteryPlaying` (lowercase);
# use the actual column name `LotteryPlaying` as above to avoid errors.

# Cumulative relative frequencies (Slide p. 54–55)
# (Slide repeats creation of table; using the one already created)
cumfreq <- cumsum(relfreq)
cumfreq                       # -> Running total of relative frequencies.
plot(cumfreq, xlab = "Frequency of playing lottery",
     ylab = "Cumulative relative frequency", type = "b")
# -> Curve approaches 1; steep steps identify dominant categories.

# ================================================================
# Descriptive statistics example: IDI (Slides pp. 57–71, 73–78)
# ================================================================

# Importing data
# Original slide (CSV): 
# idi <- read.csv(file.choose(), header = TRUE)
# Using SPSS .sav to match your files:
idi <- read.spss(file.choose(), to.data.frame = TRUE)  # select IDI.sav

# Calculate mean for one variable and across a range (Slide p. 59)
meanfixed2011 <- mean(idi$Fixed2011)
meanfixed2011                 # -> Central tendency (affected by outliers).
apply(idi[, 2:7], 2, mean)    # -> Means for columns 2:7.

# Median (Slide p. 60)
median(idi$Fixed2011)         # -> Middle value; robust to skew/outliers.

# Mode function (Slide p. 63)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Mode(idi$Fixed2011)           # -> Most frequent value in Fixed2011.

# Variance & SD (Slides pp. 65–66)
var(idi$Fixed2011)            # -> Dispersion around the mean (squared units).
sd(idi$Fixed2011)             # -> Average deviation from the mean (original units).

# Range & Min/Max (Slide p. 68)
min(idi$Fixed2011)
max(idi$Fixed2011)
max(idi$Fixed2011) - min(idi$Fixed2011)  # range

# IQR & Descriptives with psych (Slide p. 70)
IQR(idi$Fixed2011)            # -> Spread of middle 50% (robust).
describe(idi$Fixed2011)       # -> n, mean, sd, median, range, skew, kurtosis, etc.

# Descriptive statistics by groups using tapply (Slide p. 71)
sd <- tapply(idi$Fixed2012, idi$HDI_level, sd)
sd                             # -> Group SDs; compare variability by HDI level.
mean <- tapply(idi$Fixed2011, idi$HDI_level, mean)
mean                           # -> Group means; larger = higher fixed lines.
# (Note: 'mean' object shadows base::mean; used here as in slides.)

# ---- Boxplots (Slides pp. 73–76) ----
boxplot(idi$Fixed2011)        # -> Box shows median & IQR; points outside = outliers.
boxplot(idi[, 2:7], las = 1)  # -> Side-by-side for selected scale variables.
boxplot(idi[, 2:7], las = 2)  # -> Rotated labels for readability.

# Boxplot by HDI level (Slide p. 76)
idi$HDI_leveln <- as.numeric(factor(idi$HDI_level,
                                    levels = c("low", "medium", "high", "very high")))
boxplot(idi$Fixed2011 ~ idi$HDI_leveln, data = idi,
        main = "Box plot by HDI level", xlab = "HDI level", ylab = "Fixed2011")
# -> Compare medians/spreads across HDI groups; non-overlap suggests differences


# ---- Normality checks (Slides pp. 77–78, 80) ----
qqnorm(idi$Fixed2011); qqline(idi$Fixed2011)
# -> If points follow the line closely, normality is plausible; strong deviations
# suggest non-normality (favor non-parametric summaries/tests if needed).
skewness(idi$Fixed2011)   # -> >0 positive skew; <0 negative skew.
kurtosis(idi$Fixed2011)   # -> Around 3 ~ normal; >3 heavy tails, <3 light tails.
shapiro.test(idi$Fixed2011)

# ================================================================
# Plotting graphs using ggplot2 (Slides pp. 81–105)
# ================================================================

# Call datasets from ggplot2 and read help (Slides pp. 81–82, 92)
ggplot2::mpg
?mpg # Get more information on the data
ggplot2::diamonds
?diamonds

# Scatter plots & aesthetics (Slides pp. 83–91)
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg) + geom_point(mapping = aes(x = hwy, y = cyl))    # demonstration
ggplot(data = mpg) + geom_point(mapping = aes(x = class, y = drv))  # demonstration
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = class))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, size = class))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, alpha = class))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, shape = class))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy), color = "blue")
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy), shape = 22)

# Bar charts & stats per group (Slides pp. 93–100)
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut))
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))
ggplot(data = diamonds) +
  stat_summary(mapping = aes(x = cut, y = depth),
               fun.ymin = min, fun.ymax = max, fun.y = median)
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, color = cut))
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = cut))
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = clarity))
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")
# -> Compare counts/proportions; bars or stacked fills reveal composition across groups.

# Boxplots with ggplot2 (Slides pp. 101–102)
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + geom_boxplot()
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + geom_boxplot() + coord_flip()

# Visualizing distributions (Slides pp. 103–105)
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut))              # discrete
ggplot(data = diamonds) + geom_histogram(mapping = aes(x = carat), binwidth = 0.5)
ggplot(data = diamonds) + geom_histogram(mapping = aes(x = carat, fill = cut), binwidth = 0.5)
# -> For continuous variables, assess shape/spread; multimodality or skew informs
# whether parametric assumptions (e.g., normality) are reasonable.

# ================================================================
# Quick base histograms (Slides pp. 106–108)
# ================================================================
hist(idi$Mobile2011)                                   # quick look at distribution
hist(idi$Mobile2011, breaks = 50, col = "red")         # more bins, colored

# Histogram with Normal Curve overlay (Slide p. 108)
x <- idi$Fixed2011
h <- hist(x, breaks = 10, col = "red", xlab = "Fixed 2011",
          main = "Histogram with Normal Curve")
xfit <- seq(min(x), max(x), length = 40)
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
yfit <- yfit * diff(h$mids[1:2]) * length(x)
lines(xfit, yfit, col = "blue", lwd = 2)
# -> Visual check: if blue curve aligns with bars, normal approximation is reasonable.

############################################################
# End of script
############################################################

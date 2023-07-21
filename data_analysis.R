# Load the "readxl" library, which allows reading data from Excel files.
library(readxl)

# Read the data from the "pokemon.xlsx" Excel file and store it in the "pok" data frame.
pok <- read_excel("pokemon.xlsx")

# View the contents of the "pok" data frame in a spreadsheet-like viewer.
View(pok)

# Extract the "HP" column from the "pok" data frame and assign it to the variable "duration."
duration = pok$HP

# Compute the range (difference between the maximum and minimum values) of the "duration" variable.
range(duration)

# Compute the minimum value of the "duration" variable.
min(duration)

# Create a sequence of breaks from 1 to 255 with an interval of 10 and store it in the "breaks" variable.
breaks = seq(1, 255, by = 10)

# Display the "breaks" vector.
breaks

# Cut the "duration" variable into intervals defined by the "breaks" vector, with right-closed intervals.
# Store the result in the "duration.cut" variable.
duration.cut = cut(duration, breaks, right = FALSE)

# Count the frequency of each interval in the "duration.cut" variable and create a frequency table.
# Store the result in the "duration.freq" variable.
duration.freq = table(duration.cut)

# Display the frequency table.
duration.freq

# Display summary statistics (e.g., mean, median, quartiles) of the "pok" data frame.
summary(pok)

# Create a histogram of the "duration" variable with right-closed intervals.
hist(duration, right = FALSE)

# Compute relative frequencies by dividing the frequency of each interval by the total number of rows in the data frame "pok."
# Store the result in the "duration.relfreq" variable.
duration.relfreq = duration.freq / nrow(pok)

# Display the relative frequencies.
duration.relfreq

# Calculate cumulative frequencies by taking the cumulative sum of the "duration.freq" variable.
# Store the result in the "duration.cumfreq" variable.
duration.cumfreq = cumsum(duration.freq)

# Display the cumulative frequencies.
duration.cumfreq

# Calculate cumulative relative frequencies by dividing the cumulative frequencies by the total number of rows in the data frame "pok."
# Store the result in the "duration.cumrelfreq" variable.
duration.cumrelfreq = duration.cumfreq / nrow(pok)

# Create a new vector "cumrelfreq0" by adding a 0 at the beginning of "duration.cumrelfreq."
cumrelfreq0 = c(0, duration.cumrelfreq)

# Create a plot to visualize the cumulative relative frequencies with breaks on the x-axis and cumulative relative frequencies on the y-axis.
plot(breaks, cumrelfreq0, main = "Weights", xlab = "weights", ylab = "Cumulative frequency")

# Add lines to the plot, connecting each break point with its corresponding cumulative relative frequency.
lines(breaks, cumrelfreq0)

# Create a stem-and-leaf plot for the "HP" attribute.
stem(pok$HP)

# Create a horizontal boxplot to visualize the distribution of the "duration" variable.
boxplot(duration, horizontal = TRUE)

# Create a bar plot to visualize the frequency distribution of the "duration.freq" variable.
barplot(duration.freq)

# Create a dot plot to visualize the "HP" attribute.
dotchart(pok$HP)

# Create a table "mytable" that shows the counts of Pokémon by generation.
mytable <- table(pok$Generation)

# Create labels for the pie chart by combining the generation names and their corresponding counts.
lbls <- paste(names(mytable), "\n", mytable, sep="")

# Create a pie chart to visualize the distribution of Pokémon generations.
pie(mytable, labels = lbls, main = "Pie Chart of generations")

# Perform a paired t-test to compare "Attack" and "Defense" attributes of Pokémon.
t.test(pok$Attack, pok$Defense, paired = TRUE)

# Perform a one-sample t-test to compare "HP" attribute against a specific value (3 in this case).
t.test(pok$HP, mu = 3)

# Perform a chi-square test of independence between "Attack" and "Defense" attributes.
chisq.test(pok$Attack, pok$Defense)

# Perform Fisher's exact test to compare "Attack" and "Defense" attributes using a Monte Carlo simulation for p-value estimation.
fisher.test(pok$Attack, pok$Defense, simulate.p.value = TRUE)

# Load a custom R function for creating cross-tabulation tables.
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

# Create a cross-tabulation table between "Generation" and "Type" attributes.
crosstab(pok, row.vars = "Generation", col.vars = "Type", type = "c")

# Create a cross-tabulation table between "Generation", "Type", and "Legendary" attributes.
crosstab(pok, row.vars = c("Generation", "Type"), col.vars = "Legendary", type = "t")

# Calculate the Pearson correlation coefficient between "Attack" and "Defense" attributes.
cor(pok$Attack, pok$Defense, method = "pearson")

# Perform a hypothesis test to determine if there is a significant correlation between "Attack" and "Defense" attributes.
cor.test(pok$Attack, pok$Defense, method = "pearson")

# Calculate the Pearson covariance between "Attack" and "Defense" attributes.
cov(pok$Attack, pok$Defense, method = "pearson")

# Create a linear regression model with "Attack" as the dependent variable and "Defense" as the independent variable.
mod <- lm(pok$Defense ~ pok$Attack)

# Display a summary of the linear regression model.
summary(mod)

# Create a boxplot to compare the distribution of the "Legendary" attribute.
boxplot(pok$Legendary)

# Perform a one-sample t-test to determine if the mean "Legendary" attribute is significantly different from 0.
t.test(pok$Legendary, mu = 0, alternative = "two.sided", conf.level = 0.95, var.eq = FALSE, paired = FALSE)

# Calculate the error margin for the mean "HP" attribute using the t-distribution and standard deviation.
error <- qt(0.975, df = length(pok$HP) - 1) * sd(pok$HP) / sqrt(length(pok$HP))

# Calculate the left and right bounds for a confidence interval of the mean "HP" attribute.
left <- mean(pok$HP) - error
right <- mean(pok$HP) + error

# Display the left and right bounds of the confidence interval.
left
right

# Perform a Wilcoxon signed-rank test to compare "Attack" and "Defense" attributes non-parametrically.
res <- wilcox.test(pok$Attack, pok$Defense)

# Display the result of the Wilcoxon signed-rank test.
res

# Load required libraries
library(readxl)

# Load data from the "pokemon.xlsx" file
pok <- read_excel("pokemon.xlsx")

# View the first few rows of the data frame to check the data
head(pok)

# Check the structure of the data frame
str(pok)

# Summary statistics of the data frame
summary(pok)

# Create a histogram to visualize the frequency distribution of "Type"
library(ggplot2)

ggplot(pok, aes(x = Type)) +
  geom_bar(fill = "blue") +
  labs(title = "Frequency of Pokémon Types", x = "Type", y = "Frequency")

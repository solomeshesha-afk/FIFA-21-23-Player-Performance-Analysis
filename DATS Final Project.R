# Read in datasets
fifa21 <- read.csv("players_fifa21.csv")
fifa22 <- read.csv("players_fifa22.csv")
fifa23 <- read.csv("players_fifa23.csv")


library(dplyr)

# Filter dataset to only the variables we will be using
fifa21 <- fifa21 %>%
  select(ID, Name, FullName, Age, Height, Weight, Nationality, Overall, Potential)
fifa22 <- fifa22  %>%
  select(ID, Name, FullName, Age, Height, Weight, Nationality, Overall, Potential)
fifa23 <- fifa23 %>%
  select(ID, Name, FullName, Age, Height, Weight, Nationality, Overall, Potential)

# Range for numeric variables: Age
range(fifa21$Age, na.rm = TRUE)
range(fifa22$Age, na.rm = TRUE)
range(fifa23$Age, na.rm = TRUE)

# Range for numeric variables: Height
range(fifa21$Height, na.rm = TRUE)
range(fifa22$Height, na.rm = TRUE)
range(fifa23$Height, na.rm = TRUE)

# Range for numeric variables: Weight
range(fifa21$Weight, na.rm = TRUE)
range(fifa22$Weight, na.rm = TRUE)
range(fifa23$Weight, na.rm = TRUE)

# Range for numeric variables: Overall 
range(fifa21$Overall, na.rm = TRUE)
range(fifa22$Overall, na.rm = TRUE)
range(fifa23$Overall, na.rm = TRUE)

# Range for numeric variables: Potential
range(fifa21$Potential, na.rm = TRUE)
range(fifa22$Potential, na.rm = TRUE)
range(fifa23$Potential, na.rm = TRUE)

# No. of Observations for each variable 
colSums(!is.na(fifa21))
colSums(!is.na(fifa22))
colSums(!is.na(fifa22))

# Checks if there are any missing values in dataset
any(is.na(fifa21))
any(is.na(fifa22))
any(is.na(fifa23))

any(duplicated(fifa21))
any(duplicated(fifa22))
any(duplicated(fifa23))

sum(duplicated(fifa21))
fifa21[duplicated(fifa21), ]

# No. of Observations for each variable 
colSums(!is.na(fifa21))
colSums(!is.na(fifa22))
colSums(!is.na(fifa23))   

# Any missing values?
any(is.na(fifa21))
any(is.na(fifa22))
any(is.na(fifa23))

# Any duplicate rows?
any(duplicated(fifa21))
any(duplicated(fifa22))
any(duplicated(fifa23))

# Checks how many duplicate rows?
sum(duplicated(fifa21))
sum(duplicated(fifa22))
sum(duplicated(fifa23))

# View the duplicate rows (use the same object you tested)
fifa21[duplicated(fifa21), ]
fifa22[duplicated(fifa22), ]
fifa23[duplicated(fifa23), ]

#Removes all duplciate rows in the dataframe
fifa21 <- fifa21[!duplicated(fifa21), ]
fifa22 <- fifa22[!duplicated(fifa22), ]
fifa23 <- fifa23[!duplicated(fifa23), ]

# Re-checks for duplicate values
sum(duplicated(fifa21))
sum(duplicated(fifa22))
sum(duplicated(fifa23))


# Early Visual Exploration 


fifa21_summary <- fifa21 %>%
  group_by(Nationality) %>%
  summarise(
    Players = n()
  )

fifa22_summary <- fifa22 %>%
  group_by(Nationality) %>%
  summarise(
    Players = n()
  )

fifa23_summary <- fifa23 %>%
  group_by(Nationality) %>%
  summarise(
    Players = n()
  )

top_countries <- fifa21_summary %>%
  top_n(10, Players)

ggplot(top_countries, aes(x = reorder(Nationality, -Players), y = Players)) +
  geom_bar(stat = "identity", fill = "chartreuse4") +
  labs(
    x = "Country",
    y = "Number of Players",
    title = "Top 10 Countries by Number of FIFA Players",
    caption = "Source: Kaggle"
  ) +
  theme_minimal()


library(tidymodels)
library(tidyverse) 
library(infer)
set.seed(66)

fifa_all <- bind_rows(
  mutate(fifa21, Version = "FIFA21"),
  mutate(fifa22, Version = "FIFA22"),
  mutate(fifa23, Version = "FIFA23")
) 

  age_summary <- fifa_all |>
    group_by(Age) |>
    summarize(
      mean_overall = mean(Overall, na.rm = TRUE), 
      n = n()
    ) |>
      filter(n>=100) |>
      arrange(desc(mean_overall))
  
    age_summary  
  
  peak_age <- age_summary |>
    slice(1) |>
    pull(Age)
  
  
  peak_mean <- fifa_all |>
    filter(Age == peak_age) |>
    summarize(mean_overall = mean(Overall, na.rm = TRUE)) |>
    pull(mean_overall)
  
  peak_age
  peak_mean
  
  mu0 <- 75
  
  null_dist <- fifa_all |>
    filter(Age == peak_age) |>
    specify(response = Overall) |>
    hypothesize(null = "point", mu = mu0) |>
    generate(reps = 10000, type = "bootstrap") |>
    calculate(stat = "mean")
  
  p_value <- null_dist |>
    get_p_value(obs_stat = peak_mean, direction = "two_sided")
  
  peak_age
  peak_mean
  p_value
  
  null_dist |>
    ggplot(aes(x = stat)) + 
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    geom_vline(xintercept = peak_mean, color = "red", linewidth = 1.2) + 
    labs(
      title = "Bootstrap Null Distribution of Mean Overall Rating", 
      subtitle = "Vertical line = Observed mean overall rating at age 34(69.50)",
      x = "Bootstrap Mean Overall Rating under H0: u = 75",
      y = "Count"
    ) +
    theme_minimal(base_size = 14)
  

  
# We looked at whether or not soccer players reach their peak overall at the age of 34
#  using the three FIFA data sets. Based on the data we got, age 34 had the highest average rating
# among different age groups with at least 100 players, testing whether players gave a  mean overall rating of 75. 
# According to https://www.ea.com/games/fifa/fifa-23/ultimate-team/item-guide, when having a rating of 75 it is refereed as Gold Tier cards. The null hypothesis stated the population mean rating at the age of 34
# equals the overall rating 75(H0: u = 75), but the alternative said it is not equal to 75
# (Ha: μ ≠ 75). We used the bootstrap method with 10,000 samples to show the null distribution, with the assumption the population mean equals 75.
# We compared the observed mean from our code(69.50) with the bootstrap distribution.
# Since the p-value p < 0.0001 which is less than 0.05, this means our result is very significant and really unlikely because of random chance. In conclusion, we reject the null
# hypothesis and conclude that, based on the information we've collected,  most soccer players at age 34 have an overall rating lower than 75.



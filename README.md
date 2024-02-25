# exercise-03
---
title: "EDA-Challenge.Rmd"
author: "Jessica V"
date: "`r Sys.Date()`"
output: html_document
---
#Here I am reading the dataset
"https://raw.githubusercontent.com/difiore/ada-2024-datasets/main/data-wrangling.csv"

```{r}
f <- "https://raw.githubusercontent.com/difiore/ada-2024-datasets/main/data-wrangling.csv"

d <- read_csv(f, col_names = TRUE)

```

#Here we can look at the variables it contains

```{r}
names(d)
```

#1 Here I am creating a new variable named BSD (body size dimorphism) which is the ratio of average male to female body mass.

# I need to use the following Body_mass_male_mean"  "Body_mass_female_mean"
```{r}
BSD <- d$ratio <- d$Body_mass_male_mean/d$Body_mass_female_mean

BSD

d$BSDcol <- c(BSD)
```

#2 Here I need to create a new variable named sex_ratio, which is the ratio of the number of adult females to adult males in a typical group


```{r}
d$ratio <- d$AdultFemale/d$AdultMales
sex_ratio <- d$ratio <- d$AdultFemale/d$AdultMales
sex_ratio
```

##3 Here I need to create a new variable name DI (for "defensibility index"), which is the ratioo of dat range lendg to the diameter of the home range
# home range here is a circel calculated AREA= pixr^2
# diameter = 2(sqrt(Area/pi))

```{r}
d$diametercol <- (2*(sqrt((d$HomeRange_km2)/pi)))
DI <- d$ratio <- d$DayLength_km/d$diametercol
  
d
```

#4 Here I have to plot the relationship between day range length and time spent moving, for these primate species overall.

Do species that spend more time moving travel farther overall? How about within any particular primate family? Should you transform either of these variables?
```{r}
d %>%
    ggplot(aes(x = log(d$DayLength_km), y = log(d$Move))) +
    geom_point() + ggtitle ("Relatinoship between day range length and time spent moving for all represented primate species in data 'd'") + theme(plot.title = element_text(size=10)) + geom_smooth(method = "lm", fullrange = FALSE, na.rm = TRUE)
```
## According to this plot ^ species that spend more time moving travel further overall.
When splitting this by family we see that this hold true for : Atelidae, Cebidae, Hylobatidae
# Here I have to plot the relationship between day range length and time spent moving, for these primate species by family
```{R}
p <- ggplot(data = d, aes(x = log(d$DayLength_km), y = log(d$Move),
    color = factor(Family)))  
p <- p + geom_point(na.rm = TRUE)
p <- p + xlab("log(Day Length)") + ylab("log(Time Spent Moving)")
p <- p + geom_smooth(method = "lm", fullrange = FALSE, na.rm = TRUE)
p
```
# OR like this
```{r}
d %>%
    ggplot(aes(x = log(d$DayLength_km), y = log(d$Move))) +
    geom_point() + ggtitle ("Relatinoship between day range length and time spent moving by Family") + theme(plot.title = element_text(size=10)) + facet_wrap(~Family, ncol = 4) + theme(legend.position = "none") + geom_smooth(method = "lm", fullrange = FALSE, na.rm = TRUE)


```
#5 Plot the relationship between day range length and time group size, overall and by family. Do species that live in larger groups travel farther overall? How about within any particular primate family? Should you transform either of these variables?

Overall primates that travel live in larger groups travel further

```{r}

d %>%
    ggplot(aes(x = log(d$DayLength_km), y = log(d$MeanGroupSize))) +
    geom_point() + ggtitle ("Relatinoship between day range length and  mean group size for all represented primate species in data 'd'") + theme(plot.title = element_text(size=10)) + geom_smooth(method = "lm", fullrange = FALSE, na.rm = TRUE)
```

#5 part 2

Primates who live in larger goups tend to travel further by family with the exception of Hylobatidae.
```{r}
d %>%
    ggplot(aes(x = log(d$DayLength_km), y = log(d$MeanGroupSize))) +
    geom_point() + ggtitle ("Relatinoship between day range length and time spent moving by Family") + theme(plot.title = element_text(size=10)) + facet_wrap(~Family, ncol = 4) + theme(legend.position = "none") + geom_smooth(method = "lm", fullrange = FALSE, na.rm = TRUE)
```

#6 Here I plot the relationship between body size dimorphism and canine size dimorphism overall.

Do taxa with greater size dimorphism also show greater canine dimorphism?
overall species with greater size dimorphism also show greater canine dimorphism, this does not hold true for each family.
```{r}
d %>%
    ggplot(aes(x = log(d$BSDcol), y = log(d$Canine_Dimorphism))) +
    geom_point() + ggtitle ("Relatinoship between body size dimorphism & canine size dimorphism for all represented primate species in data 'd'") + theme(plot.title = element_text(size=9.5))  + geom_smooth(method = "lm", fullrange = FALSE, na.rm = TRUE)
```

# #6 part 2 Here I plot the relationship between body size dimorphism and canine size dimorphism by family.


```{r}

d %>%
    ggplot(aes(x = log(d$BSDcol), y = log(d$Canine_Dimorphism))) +
    geom_point() + ggtitle ("Relatinoship between body size dimorphism & canine size dimorphism by Family") + theme(plot.title = element_text(size=10)) + facet_wrap(~Family, ncol = 4) + theme(legend.position = "none") + geom_smooth(method = "lm", fullrange = FALSE, na.rm = TRUE)

```

#7
Create a new variable named diet_strategy that is “frugivore” if fruits make up >50% of the diet, “folivore” if leaves make up >50% of the diet, and “omnnivore” if neither of these is true. Then, do boxplots of group size for species with different dietary strategies. Do frugivores live in larger groups than folivores?

frugivore
folivore
omnivore

```{r}
d$diet_strategy <-c (mutate(d, diet = ifelse(Fruit >= 50, "frugivore", ifelse(Leaves >= 50, "folivore",
    "omnivore"))))
diet
mutate(d, diet = case_when(Fruit >= 50 ~ "frugivore", Leaves >= 50 ~ "folivore",
    Fruit < 50 & Leaves < 50 ~ "omnivore")))

```

##8 Here in one line of code, using {dplyr} verbs and the forward pipe (%>% or |>) operator, do the following:
-Add a variable, Binomial to the data frame d, which is a concatenation of the Genus and Species…
-Trim the data frame to only include the variables Binomial, Family, Brain_size_species_mean, and Body_mass_male_mean…
-Group these variables by Family…
-Calculate the average value for Brain_size_species_mean and Body_mass_male_mean per Family (remember, you may need to specify na.rm = TRUE)…
-And arrange by increasing average brain size
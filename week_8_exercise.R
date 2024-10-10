#Title: Week_8_exercise
#uthor: Spencer Parr
#date: 10/10/24

library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)

fish_density<- read_csv("data/fish_psu_density.csv")
fish_frequency<-read_csv("data/length_frequency.csv")
raw_fish<-read_csv("data/usvi_2021_fish_raw.csv")
fish_outliers<- read_csv("data/fish_psu_density_with_outliers.csv")
#################################

# Question 1: Box and whicker plots of fish density

  #first step:group by year, region, strat, and species_CD for both data frames 
real_density <- fish_density %>%
  group_by(YEAR, REGION, STRAT, SPECIES_CD) %>%
  summarize(mean_density = mean(density, na.rm = TRUE))


outlier_density <- fish_outliers %>%
  group_by(YEAR, REGION, STRAT, SPECIES_CD) %>%
  summarize(mean_density = mean(density, na.rm = TRUE))

# Create a box and whisker of density grouped by SPECIES_CD, faceted by YEAR, REGION, and STRAT

# box and whisker for real_density
ggplot(real_density, aes(x = SPECIES_CD, y = mean_density)) +
  geom_boxplot(
    outlier.colour = "blue",  # Handles the outlier appearance
    outlier.shape = 8,     # Default shape for outliers
    outlier.size = 4,     # Size of the outliers
    outlier.stroke = 0.5,   # Thickness of the outlier points
    notch = FALSE,          # Option to show a notch
    notchwidth = 0.5        # Notch width if notch = TRUE
  ) +
  geom_jitter(               # Adds jittered points on top of the boxplot
    aes(color = SPECIES_CD),  # Color jittered points by species
    size = 1.2,              # Size of the jittered points
    alpha = 0.6,             # Transparency of jittered points
    width = 0.15,            # Control the width of the jitter
    height = 0               # Jitter only on the x-axis, not on y
  ) +
  facet_wrap(~REGION, scale="free_y")+ #this is for question two, facet by region 
  labs(title = "Boxplot with Jittered Points", x = "Species Code", y = "Density") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#box and whisker for outlier density 
ggplot(outlier_density, aes(x = SPECIES_CD, y = mean_density)) +
  geom_boxplot(
    outlier.colour = "blue",  # Handles the outlier appearance
    outlier.shape = 8,     # Default shape for outliers
    outlier.size = 4,     # Size of the outliers
    outlier.stroke = 0.5,   # Thickness of the outlier points
    notch = FALSE,          # Option to show a notch
    notchwidth = 0.5        # Notch width if notch = TRUE
  ) +
  geom_jitter(               # Adds jittered points on top of the boxplot
    aes(color = SPECIES_CD),  # Color jittered points by species
    size = 1.2,              # Size of the jittered points
    alpha = 0.6,             # Transparency of jittered points
    width = 0.15,            # Control the width of the jitter
    height = 0               # Jitter only on the x-axis, not on y
  ) +
  facet_wrap(~REGION, scale="free_y")+ #this is for question two, facet by region 
  labs(title = "Boxplot with Jittered Points", x = "Species Code", y = "Density") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# Question 3 create a scatter plot of STRAT-level mean density vs. standard deviation 


STRAT_fish <- fish_density %>%
  group_by(REGION, STRAT, SPECIES_CD) %>%
  summarize(
    mean_density = mean(density),  
    sd_dens = sd(density)          
  )
  
ggplot(STRAT_fish, aes(mean_density,sd_dens, shape=REGION))+
  geom_point()

#Question 4: Create another scatter plot like #3 using the fish_psu_density_w_outliers dataset to see how outliers affect the plots
STRAT_outlier <- fish_outliers %>%
  group_by(REGION, STRAT, SPECIES_CD) %>%
  summarize(
    mean_density = mean(density),  
    sd_dens = sd(density))
  
ggplot(STRAT_outlier, aes(mean_density,sd_dens, shape=REGION))+
  geom_point()



#Question 5 create a length frequency diagram 

#gonna have to pivot the plot 

# Step 1: Pivot the data from wide to long format
long_frequency <- pivot_longer(fish_frequency, 
                          cols = c("red hind", "french grunt"), 
                          names_to = "species", 
                          values_to = "frequency")

# Step 2: Create a histogram with both species
ggplot(long_frequency, aes(x = bin, y = frequency, fill = species)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  labs(title = "Length Frequency Histogram for Red Hind and French Grunt", 
       x = "Length Bin (cm)", 
       y = "Frequency") +
  theme_minimal() +
  scale_fill_manual(values = c("red hind" = "red", "french grunt" = "blue"))


ggplot(long_frequency, aes(bin, frequency, fill = species))+
         geom_bar(
           stat="identity" )


#Question 6: 

ggplot(raw_fish, aes(SPECIES_CD, NUMBER_OF_INDIVIDUALS, fill=SPECIES_CD))+
  geom_bar(stat="identity")

species_data <- raw_fish[raw_fish$SPECIES_CD == "SPA AURO", ]

Auro<- raw_fish%>% 
  filter(SPECIES_CD == "SPA AURO")

flav<- raw_fish %>% 
  filter(SPECIES_CD == "HAE FLAV")

# Create the histogram for NUMBER_OF_INDIVIDUALS
ggplot(flav, aes(x = NUMBER_OF_INDIVIDUALS)) +
  geom_bar()+
  labs(title = flav$SPECIES_CD)

ggplot(Auro, aes(x = NUMBER_OF_INDIVIDUALS)) +
  geom_bar()+
  labs(title = Auro$SPECIES_CD)


#ok now we can create a for loop 
species_list <- unique(raw_fish$SPECIES_CD)
for (species in species_list) {
  
  # Filter 
  species_data <- raw_fish %>% 
    filter(SPECIES_CD == species)
  
  # histogram
  p <- ggplot(species_data, aes(x = NUMBER_OF_INDIVIDUALS)) +
    geom_bar() +
    labs(title = unique(species_data$SPECIES_CD)) +
    theme_minimal()
  
  # Print
  print(p)
}

  



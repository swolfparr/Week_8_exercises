data <- read.csv("data/fish_psu_density.csv")
data_w_outliers <- read.csv("data/fish_psu_density_with_outliers.csv")
raw_fish <- read_csv("data/usvi_2021_fish_raw.csv")
length_freq <- read_csv("data/length_frequency.csv")

# Part 1:Boxplot
boxplot_w_outliers <- data_w_outliers %>% 
  group_by(YEAR, REGION, STRAT, SPECIES_CD) %>% 
  summarise(mean_den = mean(density), sd = sd(density)) %>%
  ungroup() %>%
    ggplot(aes(x = SPECIES_CD, y = mean_den)) + 
    geom_boxplot(outlier.colour="blue", outlier.shape=8, outlier.size=4) +
    geom_jitter(shape=16, position=position_jitter(0.2))  + 
    facet_wrap(~REGION)

boxplot <- data %>% 
  group_by(YEAR, REGION, STRAT, SPECIES_CD) %>% 
  summarise(mean_den = mean(density), sd = sd(density)) %>%
  ungroup() %>% 
    ggplot(aes(x = SPECIES_CD, y = mean_den)) + 
    geom_boxplot(outlier.colour="blue", outlier.shape=8, outlier.size=4) +
    geom_jitter(shape=16, position=position_jitter(0.2)) + 
    facet_wrap(~REGION, scale = "free_y")


#Part 2: Scatter Plot
dens_v_sd_w_outliers <- data_w_outliers %>%
  group_by(REGION, STRAT, SPECIES_CD) %>% 
  summarise(mean_den = mean(density), sd = sd(density)) %>% 
    ggplot(aes(mean_den,sd, shape = REGION)) + 
    geom_point()

dens_v_sd <- data %>%
  group_by(REGION, STRAT, SPECIES_CD) %>% 
  summarise(mean_den = mean(density), sd = sd(density)) %>% 
  ggplot(aes(mean_den,sd, shape = REGION)) + 
  geom_point()

# Part 3: Histograms

#Length Frequency
lf <- length_freq %>%  
  pivot_longer(cols = !bin, names_to = "species", values_to = "frequency") %>% 
  ggplot(aes(as.factor(bin), frequency, fill = species)) + 
  geom_bar(stat = "identity", 
            position = position_dodge2(0.85), 
            linewidth = .25, 
            color="black", 
            size=.5, 
            alpha = 0.35)

# loop to create histograms for the count of 'NUMBER_Of_INDIVIDUALS' for each species
for (f in unique(raw_fish$SPECIES_CD)) { 
  p <- raw_fish %>% filter(SPECIES_CD == f) %>% 
    ggplot(aes(x = NUMBER_OF_INDIVIDUALS)) +
    geom_bar(color = "black", fill = "white") +
    labs(title = f)
  
  print(p)
}

# Loop to create histograms for the count of 'MAX_LENGTH' for each species
for (f in unique(raw_fish$SPECIES_CD)) { 
  p <- raw_fish %>% filter(SPECIES_CD == f) %>% 
    ggplot(aes(x = MAX_LENGTH)) +
    geom_bar(color = "black", fill = "white") +
    labs(title = f)
  
  print(p)
}

# Example of a function that can be used to create the histograms
single_plot <- function(x) {
  p <- raw_fish %>% filter(SPECIES_CD == x) %>% 
    ggplot(aes(x = NUMBER_OF_INDIVIDUALS)) +
    geom_histogram(color = "black", fill = "white") +
    labs(title = x)
  
  print(p)
}

# Using the function in a for loop
for (f in unique(raw_fish$SPECIES_CD)) { 
  single_plot(f)
}

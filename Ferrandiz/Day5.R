#Data visualization- quantitative data
#Discrete: countable and has c;ear space between values, whole numbers only
#Continuous: can take any value within some interval, decimals- price of houses, water temp, wind speed



library(tidyverse)
theme_set(theme_light())
install.packages('taylor')
library(taylor)
names(taylor_all_songs)

taylor_all_songs <- taylor_all_songs |> 
  mutate(duration = duration_ms / 60000)

summary(taylor_all_songs$duration)
#standard deviation
sd(taylor_all_songs$duration, na.rm = TRUE)

taylor_all_songs %>% ggplot(aes(x= duration)) + geom_boxplot() +
  theme(axis.text.y = element_blank())
#the thing about box plots is that they do not display the full distribution shape and does not display modes
#however, it does display outliers, percentiles, spread, skew

taylor_all_songs %>% ggplot(aes(x= duration)) + geom_histogram()
#it displays full shape of the distribution, easy to interpret
#BUT you have to mindful on h
#how many numbre of bins and bin locations


library(ggbeeswarm)
install.packages("ggbeeswarm")

taylor_all_songs %>%  ggplot(aes(x= duration, y= "")) +
  geom_beeswarm(cex=2)
#it displays each data point, and eay to view full shpae o fhte distibution
#YET it can be overbearing with large dataset 

#Violin plot with box plot
taylor_all_songs %>% ggplot(aes(x= duration, y="")) +
  geom_violin() + geom_boxplot(width=0.4)
#displays ful shape
#the summary of data is via density estimate

#ECDF plot, display full distribution
taylor_all_songs %>% ggplot(aes(x= duration)) + stat_ecdf()
#displays all data, as n -> infinity, the ECDF F^ n(x) converges to the true CDF F(x)

#RUG plots display raw data
taylor_all_songs %>% ggplot(aes(x=duration)) +
  geom_rug(alpha= 0.5)
#displays raw data points, supplement for summaries and 2D plots
#Can be overbearing fo rlarge datasets

taylor_all_songs |> 
  ggplot(aes(x = duration)) +
  geom_histogram() +
  geom_rug(alpha = 0.5)
taylor_all_songs |> 
  ggplot(aes(x = duration)) +
  stat_ecdf() +
  geom_rug(alpha = 0.5)

#2D PLOTS

#Making scatter plots- use geom_point displaying join bivariate distribution
taylor_all_songs %>% ggplot(aes(x= loudness, y=energy)) +
  geom_point(color = "darkred", size=4 ,alpha=0.5) #alpha adjusts the transperancy of points color

#Correlation coefficient
cor(taylor_all_songs$loudness, taylor_all_songs$energy,
    use="complete.obs")
#the default correlation you get form cor() is pearsons correlation coefficient
#others: spearmans, rank correlation coefficient

#PLOTTING LINEAR REGRESSION
taylor_all_songs %>% ggplot(aes(x=loudness, y= energy)) +
  geom_point(color = "darkred", size=4, alpha= 0.5) +
  geom_smooth(methond= "lm", linewidth = 2)

#PAIRS plot
install.packages('GGally')
library(GGally)
taylor_all_songs %>% select(danceability, energy, loudness,tempo) %>% 
  ggpairs()
#Continuous by categorical:side by side plot
taylor_all_songs %>% filter(album_name %in% c("Lover","folklore","evermore","Midnights")) %>% 
  ggplot(aes(x=duration, y= album_name)) +
  geom_violin() +geom_boxplot(width=0.4)

#continuous by categorical:color
taylor_all_songs %>% filter(album_name%in%c("Lover","folklore","evermore","Midnights")) %>% 
  ggplot(aes(x=duration,color=album_name))+
  stat_ecdf(linewidth=1) +
  scale_color_albums()+ #from the taylor package
  theme(legend.position = "bottom")

#Continuous by categorical:ridgeline plot(joyplot)
install.packages('ggridges')
library(ggridges)
taylor_all_songs %>% filter(album_name %in% c('Lover',"folklore","evermore","Midnights")) %>% 
  ggplot(aes(x= duration, y= album_name)) +
  geom_density_ridges(scale=1)

#Histograms
taylor_all_songs %>% filter(album_name %in% c('Lover',"evermore","folklore","Midnights")) %>% 
  ggplot(aes(x=duration,fill=album_name ))+
  geom_histogram(alpha=0.6,bins=15)+
  scale_fill_albums()

#Facets
#difference between facet_wrap and facet_grid
taylor_all_songs %>% filter(album_name %in% c("Lover","folklore","evermore","Midnights")) %>% 
  ggplot(aes(x=duration)) + geom_histogram(bins = 15) +
  facet_wrap(~album_name,nrow = 1)

taylor_all_songs |> 
  filter(album_name %in% c("Lover", "folklore", "evermore", "Midnights")) |>
  ggplot(aes(x = duration)) +
  geom_histogram(bins = 15) +
  facet_grid(album_name ~ ., margins = TRUE)

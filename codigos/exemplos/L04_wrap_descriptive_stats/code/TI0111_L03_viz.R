# TI0111 - L03 - Data visualization examples
rm(list=ls())
graphics.off()
getwd()
#setwd('your_path_here')

library(ggplot2)
library(RColorBrewer)
display.brewer.all()
clrb <- brewer.pal(9, 'Blues')

##### Data Visualisation ######
?diamonds

#### Univariate analysis ####N
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut)) +
  theme(text = element_text(size=12)) 
ggsave('Figures/diamonds_bar.pdf', device='pdf', width=4, height=4)

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill=clarity))+
  theme(text = element_text(size=12)) 
#ggsave('Figures/bar_color.pdf', device='pdf', width=4, height=4)

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = color))+
  theme(text = element_text(size=18))+
  coord_flip()
#ggsave('Figures/diamonds_bar2_color.pdf', device='pdf', width=10, height=4)
    
bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = color), 
    show.legend = FALSE,
    width = 1) + 
  theme(aspect.ratio = 1, text = element_text(size=12)) +
  labs(x = NULL, y = NULL)

bar + coord_polar()
#ggsave('Figures/diamonds_bar_polar.pdf', device='pdf', width=4, height=4)
##ggsave('Figures/diamonds_bar_flipped.pdf', device='pdf', width=4, height=4)

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = price), binwidth = 1000)
# #ggsave('Figures/diamonds_hist_plain.pdf', device='pdf', width=4, height=4) 

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = price, fill=cut), binwidth = 1000) +
  labs(x = 'Diamond price [US dollars]', y =NULL)
# #ggsave('Figures/diamonds_hist_class.pdf', device='pdf', width=10, height=4)

ggplot(data = diamonds, mapping = aes(x = carat, y = cut)) + 
  geom_boxplot()+
  theme(text = element_text(size=12))
# #ggsave('Figures/diamonds_hist_boxes.pdf', device='pdf', width=4, height=4)

ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = carat, y = cut),
    fun.min = min,
    fun.max = max,
    fun = median
  )

##### Bivariate analysis #######
# Example
Temperature <- c(24.2,22.7,30.5,28.6,25.5,32.0,28.6,26.5,25.3,26.0,24.4,24.8,20.6,25.1,21.4,23.7, 23.9,25.2,27.4,28.3,28.8,26.6)
Icecream <- c(25,31,36,33,19,24,27,25,16,14,22,23,20,25,25,23,27,30,33,32,35,24)

df_temp_ice <-data.frame(Temperature,Icecream)

# Basic scatter plot
ggplot(df_temp_ice, aes(x=Temperature, y=Icecream)) +
  geom_point(color=clrb[4],fill=clrb[8],shape=21,alpha=0.5,size=8)+
  theme(text = element_text(size=12))+
  ylim(5,40)+xlim(18,35)
# #ggsave('Figures/temp_scatter.pdf', device='pdf', width=4, height=4)

# Basic scatter plot - Diamonds
ggplot(diamonds, aes(x=carat, y=price)) +
  geom_point(
    color="blue",
    fill="#69b3a2",
    shape=20,
    alpha=0.25,
    size=2)

ggplot(diamonds, aes(x=carat, y=price, color=depth)) + 
  geom_point(size=1) +
  theme_minimal()

# 
ggplot(diamonds, aes(x=carat, y=price)) + 
  geom_point(shape = 21, fill = "lightgray", 
             color = "black", size = 1.5) +
  facet_wrap(~ cut, ncol=3) +
  theme_minimal() +
  ggtitle("Scatterplots of Carat and Price, by Cut") 

# Correlation 
# install.packages("ggcorrplot")
library(ggcorrplot)

data <- diamonds[1:5000, c(1,5:6)]
# plot(data , pch=20 , cex=1.5 , col="blue")

correlation_matrix <- round(cor(data),2)
head(correlation_matrix)

ggcorrplot(correlation_matrix, 
           method ="square")+
  theme_minimal() 
  
##### Time Series #########
?economics
head(economics)

# Line plot
p <- ggplot(economics, aes(x=date, y=psavert)) +
  geom_line(color=clrb[7]) + 
  xlab("") +
  ylab("Personal Savings Rate") +
  theme_minimal() +
  theme(axis.text=element_text(size=14),axis.title.y =element_text(size=14)) 
p

# Set axis limits c(min, max)
dmin <- min(economics$date)
dmax <- max(economics$date)
p + scale_x_date(limits = c(dmin, dmax), date_labels = "%d %b %Y")
# #ggsave('Figures/time_series_example.pdf', device='pdf', width=10, height=4)




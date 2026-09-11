# TI0111 - Data visualization examples
rm(list=ls())
getwd()

###
library(tidyverse)
?mpg 

ggplot(data = mpg) + 
  geom_bar(mapping = aes(x = class)) +
  theme(text = element_text(size=10))
ggsave('Figures/mpg_bar.pdf', device='pdf', width=4, height=4)

ggplot(data = mpg) + 
  geom_bar(mapping = aes(x = class, fill=class))+
  theme(text = element_text(size=8))
ggsave('Figures/mpg_bar_color.pdf', device='pdf', width=4, height=4)

ggplot(data = mpg) + 
  geom_bar(mapping = aes(x = class, fill = trans))+
  theme(text = element_text(size=8))
ggsave('Figures/mpg_bar2_color.pdf', device='pdf', width=4, height=4)
    
bar <- ggplot(data = mpg) + 
  geom_bar(
    mapping = aes(x = class, fill = class), 
    show.legend = FALSE,
    width = 1) + 
  theme(aspect.ratio = 1, text = element_text(size=8)) +
  labs(x = NULL, y = NULL)

bar + coord_polar()
ggsave('Figures/mpg_bar_polar.pdf', device='pdf', width=4, height=4)

bar + coord_flip()
ggsave('Figures/mpg_bar_flipped.pdf', device='pdf', width=4, height=4)

ggplot(data = mpg) +
  geom_histogram(mapping = aes(x = displ), binwidth = 0.5)
ggsave('Figures/mpg_hist_plain.pdf', device='pdf', width=4, height=4) 

ggplot(data = mpg) +
  geom_histogram(mapping = aes(x = displ, fill=class), binwidth = 0.5)+
  labs(x = 'Engine Displacement [litres]', y = NULL)
ggsave('Figures/mpg_hist_class.pdf', device='pdf', width=4, height=4)

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()+
  theme(text = element_text(size=10))
ggsave('Figures/mpg_hist_boxes.pdf', device='pdf', width=4, height=4)

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()
ggsave('Figures/mpg_hist_boxes_flipped.pdf', device='pdf', width=4, height=4)

ggplot(data = mpg) + 
  stat_summary(
    mapping = aes(x = class, y = hwy),
    fun.min = min,
    fun.max = max,
    fun = median
  )
  
## Kernel density estimation - 
attach(mpg)
hist(displ, breaks=10)


displ_kde = density(displ)
plot(displ_kde, lwd=3)

displ_kde
displ_kde$x
displ_kde$y
  # x: location of grid point where density being evaluated
  # y: the density value corresponds to the point 'x'


## adjust smoothing bandwidth
displ_kde = density(displ, bw=1)
plot(displ_kde, lwd=3)

displ_kde = density(displ, bw=10)
plot(displ_kde, lwd=3)

### How smoothing bandwidth affects the density estimate.
h_seq = c(seq(from=0.5, to=2, by=0.1), seq(from=2.5,to=7.0,by=0.5),
          seq(from=7.2, to=9, by=0.2))
h_seq
for(h0 in h_seq){
  plot(density(displ, bw=h0), lwd=3, col='dodgerblue',
       main=paste('h =',h0), xlab='X')
  Sys.sleep(1)
}

## change the grid points
displ_kde = density(displ, from = 0, to=200, n=5000)
  # n: number of grid points
plot(displ_kde, lwd=3)


## different kernel
displ_kde = density(displ, kernel = 'rectangular')
plot(displ_kde, lwd=3)

displ_kde = density(displ, kernel='epanechnikov')
plot(displ_kde, lwd=3)


## effect of kernel
displ_kde1 = density(displ)
displ_kde2 = density(displ, kernel = 'rectangular')
displ_kde3 = density(displ, kernel='epanechnikov')

plot(displ_kde1, lwd=2, col='red')
lines(displ_kde2, lwd=2, col='black')
lines(displ_kde3, lwd=2, col='blue')
legend('topleft',c('Gaussian','Rectangular','Epanechnikov'),
       lwd=6, col=c('red','black','blue'))  
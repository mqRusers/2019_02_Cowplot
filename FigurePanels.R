#########################################
## Saving time with ggplot and cowplot ##
#########################################

## I would normally empty any stored objects at the beginning
## of a new script, just to improve performance and avoid mix-ups
## but I don't know if you have saved your work and sessions so
## only run this if you have objects with the same name and know what you're doing
## rm(list=ls())

# 1) Loading libraries
## you can use install.packages() to install them
## use `control + shift + c` to uncomment large chunks of code
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("cowplot")

library(tidyr) # A tidy way to write and read code
library(dplyr) # Useful functions to select and group data
library(ggplot2) # To make pretty plots
library(cowplot) # To build panels
 
# 2) the `iris` dataset
## R has built-in datasets that are loaded with the environment
## they allow people to develop easily reproducible code

## Let's have a look at the most famous one in ecology, `iris`

head(iris,10)

## We're gonna ask (I) how sepal widths differ between species,
## and (II) if sepal lengths vary with sepal widths similarly between species.

## Pretty figures are worth better than all the p-values in the world,
## so we'll focus on that during this R user group session.

#########
### I ###
#########

## `boxplot()` and `geom_boxplot` are pretty similar
boxplot(Petal.Width ~ Species, data=iris)

ggplot(iris,aes(x=Species, y=Petal.Width))+
  geom_boxplot()

## but if you want to make more complex figures,
## ggplot is more flexible and easier to read

## This is base R:
boxplot(Petal.Width ~ Species, data=iris,
        range=+Inf, 
        staplelty = 0, 
        col=c('red','blue','green'), 
        xlab='species', 
        ylab='petal width',
        names=c("I. setosa", "I. versicolor", "I. virginica"),
        whisklty = 1
)
stripchart(Petal.Width ~ Species, data=iris, 
           vertical = TRUE, 
           method = "jitter",
           add=T,
           pch = 20,
           group.names=c("I. setosa", "I. versicolor", "I. virginica")
)
legend("bottomright",legend=c("I. setosa", "I. versicolor", "I. virginica"),
       fill=c('red','blue','green')
)

## It is just a simple plot but already a nightmare 

## ggplot does this in six lines:
ggplot(iris,aes(x=Species, y=Petal.Width, fill=Species))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width = 0.2)+
  xlab("species")+
  ylab("petal width")+
  scale_x_discrete(labels=c("I. setosa", "I. versicolor", "I. virginica"))

## And it's so easy to do it for Petal.Length too ! 12 characters to change
ggplot(iris,aes(x=Species, y=Petal.Length, fill=Species))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width = 0.2)+
  xlab("species")+
  ylab("petal length")+
  scale_x_discrete(labels=c("I. setosa", "I. versicolor", "I. virginica"))

## Note how to use one line per characteristic and the + at the end.
## This allows to comment any line to come back to it later

## Ok this is cool but your reviewers will probably want you to report
## group mean +/- sd.
## This is how I used to it, it's fine for three groups,
## but can rapidly become annoying
levels(iris$Species)
mean(iris$Petal.Width[iris$Species=='setosa'])
mean(iris$Petal.Width[iris$Species=='versicolor'])
mean(iris$Petal.Width[iris$Species=='virginica'])
sd(iris$Petal.Width[iris$Species=='setosa'])
sd(iris$Petal.Width[iris$Species=='versicolor'])
sd(iris$Petal.Width[iris$Species=='virginica'])

## it's hard to read, and guess what your reviewers are going to 
## ask you to provide se or median instead anyway.

## tidyr and dplyr allow you to get this info quickly
## and pretty similarly to the way ggplot displays them:
iris %>%
  gather(type,measurement,Petal.Length:Petal.Width) %>% #from wide to long format
  group_by(Species,type) %>%
  summarise(mean=mean(measurement),
            sd=sd(measurement),
            n=n())

############
#### II ####
############

## Let's look at (II) how the ratios of petal lengths to widths vary across species

## This is base R, I won't bother making a nicer figure with it,
## we're already convinced ggplot is better
levels(iris$Species)
plot(Petal.Width~Petal.Length,data=iris,pch=20,
     xlab='petal length',
     ylab='petal width',
     col=c('red','blue','green')[iris$Species])
clip(
  min(iris$Petal.Length[iris$Species=="setosa"]),
  max(iris$Petal.Length[iris$Species=="setosa"]), 
  -100, #can't even use -Inf here, so you'll have to adapt that for each dataset
  +100)
abline(lm(Petal.Width~Petal.Length, data=iris[iris$Species=="setosa",]),col="red")
clip(
  min(iris$Petal.Length[iris$Species=="versicolor"]),
  max(iris$Petal.Length[iris$Species=="versicolor"]), 
  -100, 
  +100)
abline(lm(Petal.Width~Petal.Length, data=iris[iris$Species=="versicolor",]), col="blue")
clip(
  min(iris$Petal.Length[iris$Species=="virginica"]),
  max(iris$Petal.Length[iris$Species=="virginica"]), 
  -100, 
  +100)
abline(lm(Petal.Width~Petal.Length, data=iris[iris$Species=="virginica",]), col="green")
legend("bottomright",legend=c("I. setosa", "I. versicolor", "I. virginica"),
       fill=c('red','blue','green')
)

## Well you get the idea... and that is just for three species,
## just imagine if we had this at two locations

## ggplot:
ggplot(iris,aes(x=Petal.Length,y=Petal.Width,colour=Species, group=Species))+
  geom_point()+ 
  geom_smooth(method = "lm", fill = NA)+
  xlab("petal length")+
  ylab("petal width")

## Now, imagine if we had this at two locations:
iris$location<-rep(c("Blue Mountains","Nowra"),dim(iris)[2])
head(iris)

## well it's still super easy !
ggplot(iris,aes(x=Petal.Length,y=Petal.Width,colour=Species, group=Species))+
  geom_point()+ 
  geom_smooth(method = "lm", fill = NA)+
  xlab("petal length")+
  ylab("petal width")+
  facet_wrap(~location)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(colour="white", fill="white"))

iris %>%
  gather(type,measurement,Petal.Length:Petal.Width) %>%
  group_by(Species,type,location) %>%
  summarise(mean=mean(measurement),
            sd=sd(measurement),
            n=n())

## This is getting painful to copy and paste, so just save it as an object...
results<-iris %>%
  gather(type,measurement,Petal.Length:Petal.Width) %>%
  group_by(Species,type,location) %>%
  summarise(mean=mean(measurement),
            sd=sd(measurement),
            n=n())

## ...then as a table you can copy and paste in your manuscript
write.csv(results,"table 1 - mean - sd - n.csv",row.names=F)


## Ok now let's play with cowplot to make a nice panel
## I'd like 
## i) the two boxplots side by side
## ii) the scatter plot under that
## iii) one common legend
## iv) panel names A B C

## First we need to store our plots into objects,
## and the two first boxplots shouldn't have a legend.

## Of course in a real script we wouldn't rewrite that,
## but here we do just to show how compact the final code is.
## *ALL* you need is this code below:

boxplot.width <- ggplot(iris,aes(x=Species, y=Petal.Width, fill=Species))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width = 0.2)+
  xlab("species")+
  ylab("petal width")+
  scale_x_discrete(labels=c("I. setosa", "I. versicolor", "I. virginica"))

boxplot.length <- ggplot(iris,aes(x=Species, y=Petal.Length, fill=Species))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width = 0.2)+
  xlab("species")+
  ylab("petal length")+
  scale_x_discrete(labels=c("I. setosa", "I. versicolor", "I. virginica"))

scatterplot <- ggplot(iris,aes(x=Petal.Length,y=Petal.Width,colour=Species, group=Species))+
  geom_point()+ 
  geom_smooth(method = "lm", fill = NA)+  # if you remove the fill = NA, you get a 95% confidence interval
  xlab("petal length")+
  ylab("petal width")+
  facet_wrap(~location)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(colour="white", fill="white"))

top<-plot_grid(boxplot.width + theme(legend.position="none"),
               boxplot.length + theme(legend.position="none"),
               labels = c('A', 'B'),
               align = 'h')
plot_grid(top,scatterplot,labels=c('','C'),ncol=1, rel_heights=c(1,1.8))

## How cool is that? Let's not mess around with the format,
## we'll save this as a high quality compressed TIFF image that any journal will accept:
ggsave("Figures/Figure 1.tiff",
       compression="lzw", #make sure to always include this, or your file will be heavy
       width=220,height=200,units="mm")

## Now you just need to cite the packages, why struggle?
sessionInfo()
citation()
citation("ggplot2")
citation("cowplot")
citation("tidyr")
citation("dplyr")

## To share your code and allow your results to be reproduced by anybody at anytime,
## save these outputs and share them online
sessionInfo()
dput(iris)

## One last thing, if you want to add a common legend while keeping the
## same width, you can use the function get_legend
irislegend<-get_legend(boxplot.length)
plot_grid(top,
          irislegend,
          rel_widths = c(2, 0.3))

## Feel free to try ggplot and cowplot on your own data in the time we have left
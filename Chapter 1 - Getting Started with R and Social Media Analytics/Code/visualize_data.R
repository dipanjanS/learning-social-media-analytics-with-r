
# load the data
data(iris)
library(ggplot2)
library(lattice)


# base plotting system
boxplot(Sepal.Length~Species,data=iris, 
        xlab="Species", ylab="Sepal Length", main="Iris Boxplot")

# lattice plotting system
bwplot(Sepal.Length~Species,data=iris, 
       xlab="Species", ylab="Sepal Length", main="Iris Boxplot")

# ggplot2 plotting system
ggplot(data=iris, aes(x=Species, y=Sepal.Length)) + 
         geom_boxplot(aes(fill=Species)) + 
         ylab("Sepal Length") + 
         ggtitle("Iris Boxplot") +
         stat_summary(fun.y=mean, geom="point", shape=5, size=4) + 
         theme_bw()


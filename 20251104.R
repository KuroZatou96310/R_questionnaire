


pca.USArrests <- prcomp(USArrests, scale. = TRUE)
summary(pca.USArrests)
biplot(pca.USArrests)


str(pca.USArrests)






pca.iris <- prcomp(iris[, -5])
win.graph()
plot(pca.iris$x, type = "n")
iris.lab <- factor(
  rep(c("S"
        ,
        "C"
        ,
        "V"), each=50))
text(pca.iris$x,label=iris.lab,
     col = as.integer(iris.lab))








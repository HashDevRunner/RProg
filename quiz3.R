## 1 
library(datasets)
data(iris)

mean_sepal_virginica <- mean(iris[iris$Species == "virginica",
                                  names(iris) %in% c("Sepal.Length")])

## 2
apply(iris[, 1:4], 2, mean)

## 3
data(mtcars)

## 4
ave_4cyl <- mean(mtcars[mtcars$cyl == 4,
                        names(mtcars) %in% c("hp")])
ave_8cyl <- mean(mtcars[mtcars$cyl == 8,
                        names(mtcars) %in% c("hp")])
diff <- ave_4cyl - ave_8cyl
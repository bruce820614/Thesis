cf_2016 <- read.csv("D:\\論文\\Final_Data\\Company_Report\\culture_fit_2016_TF.csv", header = F)
sim_Eu <- function(x,y){
  sqrt((x-y) %*% (x-y))
}
sim_pear <- function(x, y) {
  (((x - mean(x)) %*% (y - mean(y)))) / 
    (sqrt((x - mean(x)) %*% (x - mean(x))) * 
       sqrt((y - mean(y)) %*% (y - mean(y))))
}
sim_cos <- function(x, y) {
  x %*% y / (sqrt(x %*% x) * sqrt(y %*% y))  
}
x <- cf_2016[12,]
x <- x[-1]
x <- as.numeric(x)
y <- cf_2016[11,]
y <- y[-1]
y <- as.numeric(y)
sim_Eu(x,y)
sim_pear(x,y)
sim_cos(x,y)

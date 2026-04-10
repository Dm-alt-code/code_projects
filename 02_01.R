
c(seq(0, 1, 0.1), seq(2, 10, 1))

c(seq(0, 1, 0.1), 2:10)


housing <- read.table(file = "D:\\5 курс\\Багатовимірний статистичний аналіз\\Data for R_ Zelterman\\housing.txt")
housing

x <- housing$Apartment # temporary copies of the data
y <- housing$House
plot(x, y, xlab = "Apartment", ylab = "House",
     pch = 19, col = 2, cex = 1.25) # initial scatterplot

ch <- chull(x, y)

chl <- c(ch, ch[1])

x[chl]
y[chl]



for (i in 1 : 5) # number of onion layers
{
  ch <- chull(x, y) # indices of convex hull
  chl <- c(ch, ch[1]) # loop back to the first point
  lines(x[chl], y[chl], type = "l",
        col = 3) # draw the layer
  x <- x[ -ch] # peel away the layer
  y <- y[ -ch]
} # ... and repeat











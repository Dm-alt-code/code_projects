
#  Figure 3.6: Simple plot of housing data with no plot options

housing <- read.table(file = "D:\\5 курс\\Багатовимірний статистичний аналіз\\Data for R_ Zelterman\\housing.txt")
housing

x <- housing$Apartment
y <- housing$House
plot(x, y, xlab = "Apartment", ylab = "House",
     pch = 19, col = 2, cex = 1.25)

ch <- chull(x, y)

chl <- c(ch, ch[1])

x[chl]
y[chl]



for (i in 1 : 5)
{
  ch <- chull(x, y)
  chl <- c(ch, ch[1])
  lines(x[chl], y[chl], type = "l",
        col = 3)
  x <- x[ -ch]
  y <- y[ -ch]
}




### pdf(file="house1.pdf")
plot(housing, cex.lab = 1.25)
### dev.off()  # close house1.pdf

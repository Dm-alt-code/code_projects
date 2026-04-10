id <- dist(scale(iris[ , 1 : 4])) # find euclidean distances
fit <- hclust(id) # build the tree
library(ape) # graphics for trees
plot(as.phylo(fit), type="phylogram", cex = .5, label.offset = .1,
     tip.col = c(rep("red", 50), rep("blue", 50), rep("green", 50)))


id <- dist(scale(iris[ , 1 : 4]), method =  "maximum")
fit <- hclust(id) # build the tree

plot(as.phylo(fit), type="phylogram", cex = .5, label.offset = .1,
     tip.col = c(rep("red", 50), rep("blue", 50), rep("green", 50)))


id <- dist(scale(iris[ , 1 : 4]), method =  "manhattan")
fit <- hclust(id) # build the tree

plot(as.phylo(fit), type="phylogram", cex = .5, label.offset = .1,
     tip.col = c(rep("red", 50), rep("blue", 50), rep("green", 50)))


id <- dist(scale(iris[ , 1 : 4]), method =  "canberra")
fit <- hclust(id) # build the tree

plot(as.phylo(fit), type="phylogram", cex = .5, label.offset = .1,
     tip.col = c(rep("red", 50), rep("blue", 50), rep("green", 50)))



# Gower distance
library(cluster)
id <- daisy(scale(iris[ , 1 : 4])) # find euclidean distances
fit <- hclust(id) # build the tree
plot(as.phylo(fit), type="phylogram", cex = .5, label.offset = .1,
     tip.col = c(rep("red", 50), rep("blue", 50), rep("green", 50)))



id <- dist(scale(iris[ , 1 : 4]), p = 1)
fit <- hclust(id) # build the tree

plot(as.phylo(fit), type="phylogram", cex = .5, label.offset = .1,
     tip.col = c(rep("red", 50), rep("blue", 50), rep("green", 50)))


id <- dist(scale(iris[ , 1 : 4]), p = 10)
fit <- hclust(id) # build the tree

plot(as.phylo(fit), type="phylogram", cex = .5, label.offset = .1,
     tip.col = c(rep("red", 50), rep("blue", 50), rep("green", 50)))



Cmed <- read.table(file = "D:\\5 êóğñ\\Áàãàòîâèì³ğíèé ñòàòèñòè÷íèé àíàë³ç\\11_01\\Canmed.txt",
                   header = TRUE, row.names = 1)
Cmed <- Cmed[-14, ] # omit "Canada total" row
d <- dist(scale(Cmed)) # distances between provinces
clust <- hclust(d) # build hierarchical cluster
plot(clust, xlab = "", sub = "", main = "", ylab = "", yaxt = "n")

Cmed <- Cmed[-14, ] # omit "Canada total" row
d <- dist(scale(Cmed), method =  "maximum")
clust <- hclust(d) # build hierarchical cluster
plot(clust, xlab = "", sub = "", main = "", ylab = "", yaxt = "n")


Cmed <- Cmed[-14, ] # omit "Canada total" row
d <- dist(scale(Cmed), method =  "manhattan")
clust <- hclust(d) # build hierarchical cluster
plot(clust, xlab = "", sub = "", main = "", ylab = "", yaxt = "n")

Cmed <- Cmed[-14, ] # omit "Canada total" row
d <- dist(scale(Cmed), method =  "canberra")
clust <- hclust(d) # build hierarchical cluster
plot(clust, xlab = "", sub = "", main = "", ylab = "", yaxt = "n")

# without scale
Cmed <- Cmed[-14, ] # omit "Canada total" row
d <- dist(Cmed) 
clust <- hclust(d) # build hierarchical cluster
plot(clust, xlab = "", sub = "", main = "", ylab = "", yaxt = "n")

Cmed <- Cmed[-14, ] # omit "Canada total" row
d <- dist(Cmed, method =  "maximum")
clust <- hclust(d) # build hierarchical cluster
plot(clust, xlab = "", sub = "", main = "", ylab = "", yaxt = "n")


Cmed <- Cmed[-14, ] # omit "Canada total" row
d <- dist(Cmed, method =  "manhattan")
clust <- hclust(d) # build hierarchical cluster
plot(clust, xlab = "", sub = "", main = "", ylab = "", yaxt = "n")

Cmed <- Cmed[-14, ] # omit "Canada total" row
d <- dist(Cmed, method =  "canberra")
clust <- hclust(d) # build hierarchical cluster
plot(clust, xlab = "", sub = "", main = "", ylab = "", yaxt = "n")


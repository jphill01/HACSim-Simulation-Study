setwd("/Users/jarrettphillips/desktop/HACSim Simulation Study Paper/Supplemental Information/p = 0.90/hypothetical species")

# pop size = 1000

a <- read.table(file.choose(), sep = ",") # 90
b <- read.table(file.choose(), sep = ",") # 4545 
cc <- read.table(file.choose(), sep = ",") # 303030


# pop size = 10000

e <- read.table(file.choose(), sep = ",") # 90
f <- read.table(file.choose(), sep = ",") # 4545 
gg <- read.table(file.choose(), sep = ",") # 303030


# pop size = 100000

i <- read.table(file.choose(), sep = ",") # 90
j <- read.table(file.choose(), sep = ",") # 4545 
k <- read.table(file.choose(), sep = ",") # 303030


# pop size = 10000000

l <- read.table(file.choose(), sep = ",") # 90
m <- read.table(file.choose(), sep = ",") # 4545
n <- read.table(file.choose(), sep = ",") # 303030




# Mann-Whitney U tests - two-tailed at 5% significance level

wilcox.test(a$V2, e$V2) # W = 1305.5, p-value = 0.01797, significant
wilcox.test(a$V2, i$V2) # W = 1394.5, p-value = 0.002038, significant
wilcox.test(a$V2, l$V2) # W = 1425, p-value = 0.0008615, significant
wilcox.test(e$V2, i$V2) # W = 1130.5, p-value = 0.3424, not significant
wilcox.test(e$V2, l$V2) # W = 1129, p-value = 0.3484, not significant
wilcox.test(i$V2, l$V2) # W = 986, p-value = 0.8335, significant


wilcox.test(b$V2, f$V2) # W = 215, p-value = 1.386e-07, significant
wilcox.test(b$V2, m$V2) # W = 172, p-value = 1.099e-08, significant
wilcox.test(b$V2, j$V2) # W = 303, p-value = 1.347e-05, significant
wilcox.test(f$V2, j$V2) # W = 832.5, p-value = 0.2525, not significant
wilcox.test(f$V2, m$V2) # W = 697, p-value = 0.7986, not significant
wilcox.test(j$V2, m$V2) # W = 588.5, p-value = 0.166, not significant

wilcox.test(c$V2, g$V2) # W = 540.5, p-value = 0.06236, not significant
wilcox.test(c$V2, k$V2) # W = 628, p-value = 0.001252, significant
wilcox.test(c$V2, n$V2) # W = 616, p-value = 0.002377, significant 
wilcox.test(g$V2, k$V2) # W = 538, p-value = 0.06774, not significant
wilcox.test(g$V2, n$V2) # W = 520, p-value = 0.1227, not significant
wilcox.test(k$V2, n$V2) # W = 410, p-value = 0.8762, not significant

library(dunn.test)

# 90

res90 <- c(a$V2, e$V2, i$V2, l$V2)
g90 <- factor(rep(1:4, c(nrow(a), nrow(e), nrow(i), nrow(l))), labels = c("1000", "10000", "100000", "10000000"))

# 4545

res4545 <- c(b$V2, f$V2, j$V2, m$V2)
g4545 <- factor(rep(1:4, c(nrow(b), nrow(f), nrow(j), nrow(m))), labels = c("1000", "10000", "100000", "10000000"))

# 303030

res303030 <- c(cc$V2, gg$V2, k$V2, n$V2)
g303030 <- factor(rep(1:4, c(nrow(cc), nrow(gg), nrow(k), nrow(n))), labels = c("1000", "10000", "100000", "10000000"))


## Kruskal Wallis test ##

kruskal.test(res90, g90)

## Dunn's test ##

dunn.test(res90, g90, method = "bonferroni", alpha = 0.1)

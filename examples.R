
# https://www.r-bloggers.com/r-for-ecologists-simulating-species-area-curves-linear-vs-nonlinear-regression/

plots <- array(dim=c(5,20))

species <- c(letters[1:20])


View(plots)
View(species)

probs <- numeric(20)
probs[1:8] <- runif(8, 0, 0.1)
for(i in 9:20){
  probs[i] <- runif(1, 0, 1-sum(probs[1:i-1]))
}


sum(probs)


for(i in 1:20){
  plots[,i] <- sample(species, size=5, replace=T, prob=probs)
}


SAR.mat <- array(dim=c(20,20))

for(j in 1:20){
  for(i in 1:20){
    plot.index <- sample(1:20, j, replace=F)
    SAR.plot <- c(plots[,plot.index])
    SAR.mat[i,j] <- length(unique(SAR.plot))
  }
}



areas <- 1:20
means <- apply(SAR.mat, MARGIN=2, mean)
lower.95 <- apply(SAR.mat, MARGIN=2, function(x) quantile(x, 0.025))
upper.95 <- apply(SAR.mat, MARGIN=2, function(x) quantile(x, 0.975))

par(mar=c(4,4,1,1)+0.2)
plot(areas, means, type='n', xlab=expression('Area '*(m^-2)),
     ylab='Species Richness', cex.lab=1.2,
     ylim=c(0,12))
polygon(x=c(areas, rev(areas)),
        y=c(lower.95, rev(upper.95)),
        col='grey90')
lines(areas, means, lwd=2)

View(means)
View(areas)

SAR.mod <- lm(log(means) ~ log(areas))
summary(SAR.mod)

curve(exp(coef(SAR.mod)[1])*x^coef(SAR.mod)[2], add=T, from=0, to=20, col='red', lwd=2)


SAR.nls <- nls(means ~ a*areas^b,
               start=list('a'=exp(coef(SAR.mod)[1]),
                          'b'=coef(SAR.mod)[2]))

curve(coef(SAR.nls)[1]*x^coef(SAR.nls)[2], add=T, from=0, to=20, col='blue', lwd=2)

legend('topleft', lty=1, col=c('black', 'red', 'blue'),
       legend=c('Median Species Richness', 'Linear Model Fit', 'Nonlinear Model Fit'),
       cex=0.8,
       bty='n')






# https://stackoverflow.com/questions/52652195/convert-rarefaction-plots-from-vegan-to-ggplot2-in-r




library(ggplot2)
library(reshape2)

library(vegan)    
data(BCI)


View(BCI)
p<-poolaccum(BCI, permutations = 50)
View(p)

p.plot<-plot(p, display = c("chao", "jack1", "jack2"))
p.plot

chao <- data.frame(summary(p)$chao,check.names = FALSE)
View(chao)

colnames(chao) <- c("N", "Chao", "lower2.5", "higher97.5", "std")

chao_melt <- melt(chao, id.vars = c("N","std"))

ggplot(data = chao_melt, aes(x = N, y = value, group = variable)) +
  geom_line(aes(color = variable)) 



# vegan

data(dune)
data(dune.env)
attach(dune.env)

View(Management)
pool <- specpool(dune, Management)
pool


op <- par(mfrow=c(1,2))
boxplot(specnumber(dune) ~ Management, col="hotpink", border="cyan3",
        notch=TRUE)
boxplot(specnumber(dune)/specpool2vect(pool) ~ Management, col="hotpink",
        border="cyan3", notch=TRUE)
par(op)
data(BCI)
## Accumulation model
pool <- poolaccum(BCI)
summary(pool, display = "chao")
plot(pool)
## Quantitative model
estimateR(BCI[1:5,])


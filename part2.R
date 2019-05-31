
library(RColorBrewer)

rules$desc <- NA
rules$desc[1] <- "y5"
rules$desc[2] <- "y3"
rules$desc[3] <- "y3"
rules$desc[8] <- "y3"
rules$desc[9] <- "y3"
rules$desc[11] <- "y2"
rules$desc[7] <- "y2"
rules$desc[6] <- "y2"
rules$desc[10] <- "y4"
rules$desc[4] <- "y1"
rules$desc[5] <- "y6"

# eq = function(x){x*x}
rules <- rules[!duplicated(rules$desc), ]
rules <- rules[order(rules$desc),]
png('rplot.png', width = 6, height = 4, units = 'in', res = 600)


#dev.new(width=24,height=16,noRStudioGD = TRUE)
val <-  function(x,a,b,scalar,weight) { (1/(1+(exp(((x/scalar)-a)*b))))*weight }
plotcol <- brewer.pal(6,"Accent")
plot(1,type='n',xlim=c(0,2000),ylim=c(0,max(rules$weight)),xlab='Distance', ylab='Weight',bty="n")
for (s in 1:nrow(rules)) {
  lines(val(1:2000,a=rules$a[s],b=rules$b[s],scalar=rules$scalar[s],weight=rules$weight[s]), type='l',col=plotcol[s], lwd=2 )
}
legend("topright",legend=rules$desc, col=plotcol, lty=c(1,1),cex=0.8,bty="n")



dev.off()

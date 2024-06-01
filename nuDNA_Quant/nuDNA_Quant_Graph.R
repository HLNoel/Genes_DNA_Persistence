#line graph for nuclear DNA  
#***************************************

library(dplyr)

df <- read.csv("nuclearDNA_quantity.csv")

Donor1 <-filter(df,donor_id == "Donor1")
Donor2 <-filter(df,donor_id == "Donor2")
Donor3 <-filter(df,donor_id == "Donor3")
Time <-c(1,2,3,4,5,6)

#nuDNA quantity composite line graph 
pdf("nuDNA_Composite.pdf", width =11,height=6)
plot(1, type="n", xlim=c(1,6),ylim=c(0,0.015), ylab="nuDNA Quantity (ng/ul)", xlab = "Week", main= "", axes=FALSE, col="white")
# points and line for samples 
Quant1 <-as.vector(tapply(Donor1$quantity_nudna, INDEX=Donor1$week, FUN=mean, simplify = TRUE), mode="numeric")
lines (Time, Quant1,lty=2, lwd=3, col="blue")
errorbarsQuant1 <- as.vector(tapply(Donor1$quantity_nudna, INDEX=Donor1$week, FUN=sd, simplify = TRUE), mode="numeric")
arrows (Time, Quant1, Time, Quant1 + errorbarsQuant1, length=0.03, angle=90, col="blue")
arrows (Time, Quant1, Time, Quant1 - errorbarsQuant1, length=0.03, angle=90,col="blue")
Quant2 <-as.vector(tapply(Donor2$quantity_nudna, INDEX=Donor2$week, FUN=mean, simplify = TRUE), mode="numeric")
lines (Time, Quant2,lty=1, lwd=3, col="black")
errorbarsQuant2 <- as.vector(tapply(Donor2$quantity_nudna, INDEX=Donor2$week, FUN=sd, simplify = TRUE), mode="numeric")
arrows (Time, Quant2, Time, Quant2 + errorbarsQuant2, length=0.03, angle=90, col="black")
arrows (Time, Quant2, Time, Quant2 - errorbarsQuant2, length=0.03, angle=90,col="black")
Quant3 <-as.vector(tapply(Donor3$quantity_nudna, INDEX=Donor3$week, FUN=mean, simplify = TRUE), mode="numeric")
lines (Time, Quant3,lty=2, lwd=3, col="orange")
errorbarsQuant3 <- as.vector(tapply(Donor3$quantity_nudna, INDEX=Donor3$week, FUN=sd, simplify = TRUE), mode="numeric")
arrows (Time, Quant3, Time, Quant3 + errorbarsQuant3, length=0.03, angle=90, col="orange")
arrows (Time, Quant3, Time, Quant3 - errorbarsQuant3, length=0.03, angle=90,col="orange")
axis(1, at=1:6, lab=1:6)
axis(2, at = seq(0,0.015, 0.005), las = 1, 
     labels = prettyNum(seq(0, 0.015, 0.005), 
                        big.mark = ',', 
                        scientific = FALSE),
     cex.axis=0.8, cex.lab=0.8, mgp = c(3,0.5,0))
legend("topright", legend=c("Donor 1", "Donor 2", "Donor 3"),
       col=c("blue", "black", "orange"), lty=2:1,lwd=3, cex=1)
dev.off()
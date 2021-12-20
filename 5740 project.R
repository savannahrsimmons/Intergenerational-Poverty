require(deSolve)

rhs <- function(t, x, parms){
	BA <- x["BA"]
	OA <- x["OA"]
	BC <- x["BC"]
	OC <- x["OC"]

	dBA <- gOB*OC + gBB*BC - dB*BA - cBO*BA + cOB*OA
	dOA <- gBO*BC + gOO*OC - dO*OA - cOB*OA + cBO*BA
	dBC <- bB*BA - gBO*BC - gBB*BC
	dOC <- bO*OA - gOB*OC - gOO*OC
	
	return(list(c(dBA,dOA,dBC,dOC)))
}
# N is just general scope here to adjust graph
N <- 1000

bB <- .055
bO <- .04

g <- 1/18
gBB <- g * .95
gBO <- g * .05
gOO <- g * .97
gOB <- g * .03


dB <- 1/62
dO <- 1/68

cOB <- .01
cBO <- .06

init <- c(110,1000,30,200)
names(init) <- c("BA","OA","BC","OC")

tmax <- 100
times <- seq(from=0,to=tmax,by=1)

povout <- as.data.frame(ode(y = init,times = times, func = rhs))

xmax <- tmax
ymax = 5*N + 500

par(mfrow=c(1,2))
plot(BA ~ time,povout,type="l",lwd=3,xlim=c(0,xmax),ylim=c(0,ymax),
     ylab="# People in each state",main="Increased BA Birth Rate",
     xlab = "Time in years", cex.axis=1.2,cex.lab=1.2,col="red")

lines(OA ~ time,povout,type="l",lwd=3,xlim=c(0,xmax),col="orange")
lines(BC ~ time,povout,type="l",lwd=3,xlim=c(0,xmax),col="blue")
lines(OC ~ time,povout,type="l",lwd=3,xlim=c(0,xmax),col="green")
legend(x="topright", c("BA","OA","BC","OC"),col= c("red","orange",
           "blue","green"),lwd= 2)

# Manually adding grid because the built-in is stupid
abline(h = c(0,500,1000,1500,2000,2500,3000,3500,4000,4500,5000,5500),
    lty = 2, col = "grey")
abline(v = c(0,10,20,30,40,50,60,70,80,90,100),  lty = 2, col = "grey")

BAf <- povout[[2]][tmax+1]
OAf <- povout[[3]][tmax+1]
BCf <- povout[[4]][tmax+1]
OCf <- povout[[5]][tmax+1]
Af <- BAf + OAf
Cf <- BCf + OCf
Bf <- BAf + BCf
Pop <- Af + Cf

print(tail(povout))

cat("\nTotal adults = ", Af, "\n")
cat("Total children = ", Cf, "\n")
cat("Total pop = ", Pop, "\n")
cat("BA to total adults = ", BAf / Af, "\n")
cat("BC to total children = ", BCf / Cf, "\n")
cat("BA + BC to total pop = ", Bf / Pop, "\n")


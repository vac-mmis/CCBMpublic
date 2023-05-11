setwd(paste(system("rc --home",intern=TRUE),"/Examples/ABC",sep=""))

abcMake <- function() {
	system("./abc.sh")
	system("./abcx.sh")
}

abcLoad <- function() {
	x1 <- read.table("abc-p1-exact.dat",header=TRUE)
	x2 <- read.table("abc-p2-exact.dat",header=TRUE)
	a1 <- read.table("abc-p1-approx.dat",header=TRUE)
	a2 <- read.table("abc-p2-approx.dat",header=TRUE)
	list(x1=x1,x2=x2,a1=a1,a2=a2)
}

abcPlot <- function(d) {
	with(d,{
		plot(x1$Time,x1$LL,ylim=c(-30,0),type="n",ylab="Likelihood",xlab="Time")
		abline(v=c(6,7),col="gray")
		lines(x1$Time,x1$LL)
		lines(a1$Time,a1$LL,lt=3)

		lines(x2$Time,x2$LL,col=2)
		lines(a2$Time,a2$LL,col=2,lt=3)
	})
	legend("topright",lt=c(1,3,1,3),col=c(1,1,2,2),
        legend=c("Exact, P1","Approx, P1","Exact P2","Approx P2"))
}

abcPlot(d)

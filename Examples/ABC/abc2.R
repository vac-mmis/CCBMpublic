# Main interface routine: 'abcplots'
#

# vals <- function(x) apply(x[-1],1,mean,trim=0.1)
vals <- function(x) apply(x[-1],1,median)



doPlots <- function(u1i,u2i,w1i,w2i,ylim1=c(-32,0),ylim2=c(-2,6)) {
	xx <- u1i$V1
	u1iv <- vals(u1i)
	plot(xx,u1iv,ylim=ylim1,main="Median Logliks",pch=2,type="l",ylab="Loglik",xlab="Time")
	points(xx,vals(w1i),cex=0.5,col=1,pch=1)
	lines(xx,vals(u2i),pch=6,col=2)
	points(xx,vals(w2i),pch=1,cex=0.5,col=2)
	arrows(7,u1iv[8]-5.5,7,u1iv[8]-1,length=0.05,angle=20,lwd=2,col="orange")
	text(7,u1iv[8]-5.5,"Enter Bob",pos=1,cex=0.7)
	legend("topright",col=c(1,2,1,2),
		pch=c(NA,NA,1,1),
		lwd=c(1,1,NA,NA),
		legend=c("std goal, relv. ops",
				 "alt goal, relv. ops",
				 "std goal, all ops",
				 "alt goal, all ops"
		)
	)

	boxplot(t(u1i[,-1]),ylim=ylim1,names=0:26,main="Loglik run data",ylab="Loglik",xlab="Time")
	boxplot(t(u2i[,-1]),border=rgb(1,0,0,0.8),ylim=c(-32,0),add=TRUE,names=NA)
	arrows(8,u1iv[8]-5.5,8,u1iv[8]-1,length=0.05,angle=20,lwd=2,col="orange")
	text(8,u1iv[8]-5.5,"Enter Bob",pos=1,cex=0.7)
	legend("topright",col=c(1,2),
		lwd=c(1,1),
		legend=c("std goal, relv. ops",
				 "alt goal, relv. ops")
	)

	ttx <- 3:27
	ttc <- rgb(0,0,1,0.6)
#	tty <- sapply(ttx,function(i) t.test(u1i[i,-1],u2i[i,-1])$conf.int)
	tty <- sapply(ttx,function(i) {w<-wilcox.test(t(u1i[i,-1]),t(u2i[i,-1]),conf.int=TRUE); c(w$conf.int,w$estimate,w$p.value)})
	plot(ttx,tty[3,],col="red",lwd=1,main="Loglik Difference",ylim=ylim2,xlab="Time",ylab=expression(LL[std]-LL[alt]),type="n")
	diffs <- u1i[,-1]-u2i[,-1]
#	boxplot(t(diffs),main="Loglik Difference",ylim=ylim2,names=0:26,xlab="Time",ylab=expression(LL[std]-LL[alt]))
	abline(h=0)
	lines(ttx,tty[1,],col=ttc,lwd=2)
	lines(ttx,tty[2,],col=ttc,lwd=2)
	lines(ttx,tty[3,],col="red",lwd=1,type="b")
#	par(new=TRUE)
#	plot(ttx,tty[4,],col="green",lwd=2,xaxt="n",log="y",ylim=c(1e-20,1),axes=FALSE,xlab=NA,ylab=NA)
#	axis(4)
#	stats<-boxplot(t(diffs),main="Loglik Difference",ylim=ylim2,names=0:26,xlab="Time",ylab=expression(LL[std]-LL[alt]))
#	lines(vals(u1i)-vals(u2i),col="red",lwd=2,lty=2)
#	lines(stats$conf[1,],col="red",lwd=1)
#	lines(stats$conf[2,],col="red",lwd=1)
	legend("topleft",lwd=c(2,2),col=c("blue","red"),legend=c(
		"95% CI",
		expression(Median(LL[std]-LL[alt]))))
}

rc3home<-system("rc --home",intern=TRUE)
fids <- c("u1","u2","w1","w2")

abcplots <-  function(N=1000,informed=TRUE) {
	if(informed) {
		what <- "informed"
		ylim1 <- c(-32,0)
		ylim2 <- c(-2,6)
	} else {
		what <- "blind"
		ylim1 <- c(-50,0)
		ylim2 <- c(-4,8)		
	}
	files <- paste(rc3home,"/Examples/ABC/abc2b-results-np",as.integer(N),"/",fids,"-",what,".dat",sep="")
	u1i   <- read.table(files[1])
	u2i   <- read.table(files[2])
	w1i   <- read.table(files[3])
	w2i   <- read.table(files[4])

	oldp<-par(mfcol=c(1,3),mar=c(3,3,2,1),mgp=c(2,1,0),oma=c(0,0,2,1))
	doPlots(u1i,u2i,w1i,w2i,ylim1,ylim2)
	mtext(paste("Heuristics=",what,"; N=",as.integer(N),sep=""),outer=TRUE)
	par(oldp)
}

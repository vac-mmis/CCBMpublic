#setwd("~/Source/HMM-Tools/RuleCompiler/RuleCompiler3/XYRouter-AllGoalsAndInits")
xyll <- read.table("xyrouter-ll-all.dat",header=TRUE)
xyll.labels <- levels(as.factor(xyll$Init))
xyll.breaks<-rev(c(0,seq(-160,-179,by=-1),seq(-180,-400,by=-10),seq(-500,-1600,by=-100),-100000))
ncol<-length(xyll.breaks)
cols<-c(gray((0:(ncol-3))/(ncol-3)),"magenta")
ddd <- read.table("./xyrouter-informed.trace")
estimates <- ddd[,2:3]
l <- read.table("../Data/r218-ur-xy.dat")


showplots <- function(useTikz=FALSE) {
	if(useTikz) {
		ex1<-"$\\P{y_{1:t}}{Init=21, Goal=ij}$"
		ex2<-"$\\P{y_{1:t}}{Init=ij, Goal=55}$"
		ex3<-"$\\P{y_{1:t}}{Init, Goal}$"
	} else {
		ex1<-expression(p(paste(y[1:T]," | ",list(Init==21,Goal==ij))))
		ex2<-expression(p(paste(y[1:T]," | ",list(Init==ij,Goal==55))))
		ex3<-expression(p(paste(y[1:T]," | ",list(Init,Goal))))
	}
	oldp<-par(mfrow=c(2,2),mgp=c(2,0.7,0),mar=c(3.5,3.5,2.5,0.5))

	c <- c(0.7,2.1,3.5,4.9,6.3)
	cell.centers=expand.grid(x=c,y=c)

	plot(cell.centers,ylim=c(0,7),xlim=c(0,7),xaxs="i",yaxs="i",main="Cell Layout + Observations",xaxt="n",yaxt="n",type="n")
	axis(1,at=c);axis(2,at=c)
	abline(h=c+0.7,v=c+0.7,col="gray")
	lines(l,col="green",lwd=2)
	lines(estimates,col="red",lwd=2,lt=2)
	text(cell.centers,labels=apply(expand.grid(1:5,1:5),1,paste,collapse=""),cex=0.7)
	legend(3.5,6.1,legend=c("Observations","Estimate"),lt=c(1,2),lwd=2,col=c(3,2),bg=gray(0.9),box.col=NA,cex=0.7)

	image(c,c,as.matrix(unstack(subset(xyll,Init==21,select=c("yG","LL")),LL~yG)),col=cols,breaks=xyll.breaks,xaxt="n",yaxt="n",main=ex1,xlab="i",ylab="j")
	axis(1,at=c,labels=1:5);axis(2,at=c,labels=1:5)
	lines(l,col="green",lwd=2)
	lines(estimates,col="red",lwd=2,lt=2)

	image(c,c,as.matrix(unstack(subset(xyll,Goal==55,select=c("yI","LL")),LL~yI)),col=cols,breaks=xyll.breaks,xaxt="n",yaxt="n",main=ex2,xlab="i",ylab="j")
	axis(1,at=c,labels=1:5);axis(2,at=c,labels=1:5)
	lines(l,col="green",lwd=2)
	lines(estimates,col="red",lwd=2,lt=2)

	c<-1:25
	#xyll$LL[xyll$LL < -200] <- -200
	xyll.unst <- as.matrix(unstack(xyll,LL ~ Goal))
	#image(c,c,-log(abs(xyll.unst)),col=c(heat.colors(256),"cyan"),xlab="Init",ylab="Goal",xaxt="n",yaxt="n")
	image(c,c,xyll.unst,col=cols,breaks=xyll.breaks,xlab="Init",ylab="Goal",xaxt="n",yaxt="n",main=ex3)
	axis(1,at=c,labels=xyll.labels)
	axis(2,at=c,labels=xyll.labels)

	par(oldp)
}

printplots <- function() {
	pdf("xyrouter-all-ll-plots.pdf")
	showplots()
	dev.off()
}


#require(tikzDevice)
#
#if(!exists("tmpack")) {
#	tmpack<<-getOption("tikzMetricPackages")
#}
#
#options(tikzMetricPackages = c(tmpack,"\\usepackage{Sabon}"))
#options(tikzDocumentDeclaration="\\documentclass[12pt]{article}\n")
#tikzPlots <- function() {
#	tikz(standAlone=FALSE)
#	showplots(useTikz=TRUE)
#	dev.off()
#}

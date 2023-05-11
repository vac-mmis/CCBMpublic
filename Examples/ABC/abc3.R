setwd(paste(system("rc --home",intern=TRUE),"/Examples/ABC",sep=""))

plotset <- function(what=6,hInfo=TRUE) {
nps <- as.integer(c(100,1000,10000,100000,200000,300000)) 
npl <- c("100","1k","10k","100k","200k","300k")
informed<-if(hInfo) "informed" else "blind"
vnames=c("lo.ex.","1st Q","median","3d Q","up.ex.","max")
runs <- as.integer(0:49)
datafile<-sprintf("abc3-output-%s.rda",informed)

fetchrun<-function(np) function(run) read.table(sprintf("abc3-output-%s/%d-%02d.dat",informed,np,run),header=TRUE)[,-1]

fetchone<-function(np) {
	tabs <- lapply(runs,fetchrun(np))
	ps <- t(sapply(tabs,function(x) x$Particles))
	ss <- t(sapply(tabs,function(x) x$States))
	list(Particles=ps,
		 ParticleStats=boxplot(ps,plot=FALSE),
	     States=ss,
	     StateStats=boxplot(ss,plot=FALSE),
	     StatesMax=apply(ss,2,max))
}

if(search()[2] != paste("file",datafile,sep=":")) {
	tryCatch(attach(datafile),
		error = function(e) {
			cat("Recomputing data",datafile,"\n")
			data<-lapply(nps,fetchone)
			names(data)<-nps
			states.max.img<-sapply(data,function(x) x$StatesMax)
			save(data,states.max.img,file=datafile)
			attach(datafile)
		}
	)
}

xx<-0:26
ttl<-paste(vnames[what],"# Occupied States")
plot(xx,1:27,ylim=c(1,40),log="y",xlab="Time",ylab="#States",main=ttl,type="n")
ci<-0
for(p in data) {
	ci <- ci+1
	if(what>5)
		lines(xx,p$StatesMax,col=ci,lwd=2)
	else
		lines(xx,p$StateStat$stats[what,],col=ci,lwd=2)
}
legend("topright",
	legend=npl,
	col=1:ci,
	lwd=1,
	title="# Particles"
)
mtext(paste("Heuristics =",informed),side=2,line=3.5)

pmat <- persp(x=xx,y=1:ci,log(states.max.img),
	          expand=1,theta=30,phi=20,ltheta=-120,d=2,
	          border=NA,
	          xlab="Time",ylab="#Particles",zlab="#States",main=ttl)
while(ci>0) {
	lines(trans3d(xx,ci,log(data[[ci]]$StatesMax),pmat),col=ci,lwd=2)
	ci<-ci-1
}

plot(xx,xx,ylim=c(0,1),xlab="Time",ylab="Fraction",main="Fraction Particles Surviving",type="n")
ci<-0
for(p in data) {
	ci <- ci+1
	lines(xx,p$ParticleStats$stats[3,]+0.005*ci-0.006,col=ci,lwd=2)
}
legend("top",
	legend=npl,
	col=1:ci,
	lwd=1,
	title="# Particles"
)
detach()
}

doplot<-function(what=6) {
	oldp<-par(mfrow=c(2,3),mar=c(3,3,2,1),mgp=c(2,1,0),oma=c(0,2,0,0))
	plotset(what=what)
	plotset(what=what,hInfo=FALSE)
	par(oldp)
}

doplot(6)


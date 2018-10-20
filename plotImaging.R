plotimaging<-function(mean, stdev) {
	n<-length(mean)
	t<-c(1:n)
	t2<-c(t,rev(t))
	up<-mean+stdev
	max<-max(up)
	limmax<-(max-max%%5)+5
	low<-mean-stdev
	min<-min(low)
	limmin<-(min-(min%%5))
	area<-c(up,rev(low))
	plot(t,ylim=c(limmin,limmax),mean,type="l")
	polygon(t2,area,col=rgb(0.5,0.5,0.5,0.3),border=NA)
}
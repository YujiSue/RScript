roungGraph<-function(data, rad, unit) {
	par(pin=c(5,5))
	plot(0,0,pch=4,xlim=c(-rad-1,rad+1), ylim=c(-rad-1,rad+1))
	col<-ncol(data)
	colname<-names(data)
	rows<-c()
	minVal<-1
	maxVal<-0
	for(i in 1:col) {
		dat<-data[,i]
		dat<-dat[!is.na(dat)]
		rows<-append(rows,length(dat))
		m<-min(dat)
		if (m < minVal) minVal<-m
		m<-max(dat)
		if (m > maxVal) maxVal<-m
	}
	#max1<-ceiling(maxVal/unit)*unit
	max1<-5
	#min1<-floor(minVal/unit)*unit
	min1<-0.5
	range<-(max1-min1)
	ang<-2*pi/sum(rows)
	theta<-2*pi
	#lines(c(0,(rad+1)*cos(theta)),c(0,(rad+1)*sin(theta)),lwd=0.5)
	#for(k in 0:5) {
	#	text((rad+k*0.2)*cos(theta),(rad+k*0.2)*sin(theta)+0.1,min1+k*0.2*range,cex=0.75,srt=-90)
	#	lines(c((rad+k*0.2)*cos(theta),(rad+k*0.2)*cos(theta)),c(0,0.02),lwd=0.5)
	#}
	bpx<-c()
	bpy<-c()
	px<-c()
	py<-c()
	border<-c()
	for(i in 1:col) {
		text((rad+1)*cos(theta-0.1),(rad+1)*sin(theta-0.1),colname[i],srt=-90+theta*180.0/pi)
		for(j in 1:rows[i]) {
			val<-(data[j,i]-min1)/range+rad
			bpx<-append(bpx,rad*cos(theta))
			bpy<-append(bpy,rad*sin(theta))
			px<-append(px,val*cos(theta))
			py<-append(py,val*sin(theta))
			theta<-theta-ang
			bpx<-append(bpx,rad*cos(theta))
			bpy<-append(bpy,rad*sin(theta))
			px<-append(px,val*cos(theta))
			py<-append(py,val*sin(theta))
		}
		border<-append(border,theta)
		#lines(c(rad*cos(theta),(rad+1)*cos(theta)),c(rad*sin(theta),(rad+1)*sin(theta)),lwd=2.0)
	}
	polygon(px,py,col=rgb(0,0,1.0))
	polygon(bpx,bpy,col="white")
	for(c in 1:length(border)) {
		lines(c(rad*cos(border[c]),(rad+1)*cos(border[c])),c(rad*sin(border[c]),(rad+1)*sin(border[c])),lwd=2.0)
	}
	num<-3
	for(k in 0:num) {
		text((rad+k/num)*cos(theta),(rad+k/num)*sin(theta)+0.1,min1+k/num*range,cex=0.75,srt=-90)
		lines(c((rad+k/num)*cos(theta),(rad+k/num)*cos(theta)),c(0,0.02),lwd=0.25)
	}
}

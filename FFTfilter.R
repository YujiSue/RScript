# Low/High/Band pass filter fot 1D time series data using FFT
FFTfilter<-function(data, type, low, high) {
	length<-length(data)
	average = ave(data)
	data_<-data-average
	data_t<-fft(data_)
	data_i<-c()
	if(type=="l") {
		init<-low
		end<-length-low+2
		data_t[init:end]=0
	}
	else if(type=="h") {
		init<-high
		end<-length-high+2
		data_t[1:init]=0
		data_t[end:length]=0
	}
	else {
		init<-length-high+2
		end<-length-low+2
		data_t[low:high]=0
		data_t[init:end]=0
	}
	data_i<-fft(data_t,inverse=TRUE)/length
	return(Re(data_i))
}
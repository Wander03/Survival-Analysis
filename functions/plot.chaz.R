plot.chaz <- function(KM.obj,plot="TRUE") {
	ti <- summary(KM.obj)$time
	di <- summary(KM.obj)$n.event
	ni <- summary(KM.obj)$n.risk

	#Est Cumulative Hazard Function
	est.cum.haz <- 1:(length(ti))
	for (i in 1:(length(ti)))
 		est.cum.haz[i] <- sum(di[1:i]/ni[1:i])
	
	plot.chaz <- 1:length(KM.obj$time)
	for (i in 1:length(plot.chaz))
   	plot.chaz[i] <- sum((KM.obj)$n.event[1:i]/(KM.obj)$n.risk[1:i])
	
	if (plot=="TRUE") {
		plot((KM.obj)$time,plot.chaz,type="s",xlab="Time",ylab="Cumulative Hazard",main=expression(paste(tilde(H),(t)["NA"])))
	}
	return(list(est.chaz=plot.chaz,time=(KM.obj)$time))
}


plot.haz <- function(KM.obj,plot="TRUE") {
	ti <- summary(KM.obj)$time
	di <- summary(KM.obj)$n.event
	ni <- summary(KM.obj)$n.risk

	#Est Hazard Function
	est.haz <- 1:(length(ti))
	for (i in 1:(length(ti)-1))
    		est.haz[i] <- di[i]/(ni[i]*(ti[i+1]-ti[i]))
	est.haz[length(ti)] <- est.haz[length(ti)-1]
	
	if (plot=="TRUE") {
		plot(ti,est.haz,type="s",xlab="Time",ylab="Hazard Rate",main=expression(paste(hat(h),(t)[KM])))
	}
	return(list(est.haz=est.haz,time=ti))
}


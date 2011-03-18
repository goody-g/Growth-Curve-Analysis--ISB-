#BGN FUNCTION
gSplineFit <- function(time, data, control = gControl(), for.bs=F) {
# Fits growth curve data with a smooth spline and extracts parameters.

# Arg 
#	time - A vector of time values, of same length as data, in decimal form
#	data - vector of OD measurements, same length as time
#	for.bs - logical object used when calling gSplineFit specifying whether just for Bootsrap - changes precision preferences for faster run time. TODO (use)
#	control -  control object of class "gControl". See documentation for more information.

# Returns
# 	spline.fit - containing information about spline fit, and derived paramters. class: "gSplineFit"

#USER: 
version <- 20100831

#Chose which options for smooth.spline depending whether SplineFit is being used to make bootstrap samples or not.
if (for.bs == TRUE) {
	ifelse (control$cv.bs == T, 
	try(gc.spline <- smooth.spline(x=time, y=data, spar=control$spar.bs, cv = T)), 
	try(gc.spline <- smooth.spline(x=time, y=data, spar=control$spar.bs, cv = F)))
}
if (for.bs == FALSE) {
	ifelse ((control$cv.spline == T),
	try(gc.spline <- smooth.spline(x=time, y=data, spar=control$spar, cv = T, df=3)),
	try(gc.spline <- smooth.spline(x=time, y=data, spar=control$spar, cv = F, df=9)))
}
#Check that the smooth.spline worked.
if (is.null(gc.spline)==TRUE) {
	warning("Spline could not be fitted to data!")
	}

# Find parameters and other information.

# Growth rate (mu)
dydt.spline 	<-	predict(gc.spline, time, deriv=1)				#First derivative of spline fit - later used to max growth rate
dydtdt.spline	<-	predict(gc.spline, time, deriv=2)				# Second derivative - use for analysis of max growth rate
index.mu.spline	<-	which.max(dydt.spline$y)						#index at which maximum growth rate occurs
time.mu.spline	<-	dydt.spline$x[index.mu.spline]					#time at which maximum growth rate occurs.
mu.spline		<-	max(dydt.spline$y) 								#dydt.max	#same as dydt.spline$y[time.mu]? 


#Find Lag time (A)
y.mu.spline		<-	predict(gc.spline, x=time.mu.spline)[["y"]]			#Y location of maximum growth rate, found to be on spline fit
y.mu.diff 		<- y.mu.spline-gc.spline$y[1]
lambda.spline	<- 	time.mu.spline-(y.mu.diff/mu.spline)			#Lag time, x intercept of maximum slope.oo

# Maximum Growth rate
index.A 		<- which.max(gc.spline$y)
time.A 			<- gc.spline$x[index.A]
A				<- gc.spline$y[index.A]

#Initial OD
initial.od 		<- gc.spline$y[1]

#Final Time
time.max 		<- max(time)
time.max.index 	<- which.max(time)

#Final Slope
trajectory 		<- dydt.spline$y[time.max.index]

#Max second deriv
max.secderiv.index <- which.max(dydtdt.spline$y)
max.secderiv.time <- gc.spline$x[max.secderiv.index]
max.secderiv	<- dydtdt.spline$y[max.secderiv.index] # TODO: Does this work?

# Areas under curve
integral.spline <- 	low.integrate(gc.spline$x, gc.spline$y)			#Should we use low.integrate? Something simpler instead? low.integrate from grofit package.

#Hill Function fitting: TODO: make this better
#hill.fit <- gFitHill(time, data, A=A, time.A=time.A, lag=lambda.spline, control=control)
#if (class(hill.fit) == "nls"){
#hill.fit.coef <- coef(hill.fit)
#k0 <- as.numeric(hill.fit.coef[1])
#k1 <- as.numeric(hill.fit.coef[2])
#k2 <- as.numeric(hill.fit.coef[3])
#l1 <- as.numeric(hill.fit.coef[4])
#l2 <- as.numeric(hill.fit.coef[5])
#h1 <- as.numeric(hill.fit.coef[6])
#h11 <- as.numeric(hill.fit.coef[7])
#h2 <- as.numeric(hill.fit.coef[8])
#}

#ex: 
# To add an estimate designated Monkey
# monk <- gFitMonkey(time, data, A)

#USER: ADD PARAMETER ESTIMATE FUNCTIONS HERE! Above this line. Also add to list output under parameters!



#USER: Update control$parameter.columns when update this.
spline.fit <-list(version = version, raw.time = time, raw.data = data, deriv=dydt.spline, secderiv=dydtdt.spline, 
	fit.time = gc.spline$x, fit.data=gc.spline$y, spline=gc.spline,
	graph=list(time.mu=time.mu.spline, time.A=time.A, y.mu=y.mu.spline),#Get rid of this
	parameters=list(A=A, mu=mu.spline, lambda=lambda.spline, time.mu=time.mu.spline, 
		time.A=time.A, y.mu=y.mu.spline, iniial.od=initial.od, y.mu=y.mu.spline, integral=integral.spline, 
		time.max=time.max, trajectory=trajectory, max.secderiv.index=max.secderiv.index,
		max.secderiv.time=max.secderiv.time, max.secderiv=max.secderiv
		#, k0=k0, k1=k1, k2=k2,l1=l1, l2=l2, h1=h1, h11=h11, h2=h2 #for hill function, add for other functions too.
		))

class(spline.fit) <- "gSplineFit"

spline.fit
}
#END FUNCTION

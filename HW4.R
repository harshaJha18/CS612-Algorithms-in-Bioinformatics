mynorm<-function(v) {
	magn=sqrt(sum(v*v))
	result<-v/magn
	return (result)
}

# Cross product of two vectors
mycross<-function(v1,v2){
	xcomp<-v1[2]*v2[3]-v1[3]*v2[2]
	ycomp<-v1[3]*v2[1]-v2[3]*v1[1]
	zcomp<-v1[1]*v2[2]-v1[2]*v2[1]
	vec<-cbind(xcomp,ycomp,zcomp)
	return(vec)
}


# calculate the angle between two vectors, assuming normalized
myangle<-function(v1,v2){
return(acos(sum(v1*v2)))
}


quaternion<-function(a,b,c,d)
{
	q<-c(a,b,c,d)
	return(q)
}

# Create quaternion from angle and axis
quaternion<-function(angle,axis)
{
	a<-cos(angle/2)
	axis=axis*sin(angle/2)
	q<-c(a,axis)
	return(q)	
}

# Return the inverse of a quaternion
qinv<-function(q)
{
	qstar<-c(q[1],-q[2],-q[3],-q[4])
	return(qstar)
}

password<-function(x=8){
	ifelse(x>8,y<-(x-8),y<-0)
	pw<-c(sample(letters,1),sample(2:9,1),sample(LETTERS,2,
replace=TRUE),sample(2:9,1),sample(letters,1+y,replace=TRUE),sample(LETTERS,2,replace=TRUE))
		pwc<-paste(c(pw),collapse='')
		disclaimer<-paste(c("There are ",(8+y)," characters in this password and no zeros or ones"),collapse='')
		print(pwc)
		print(disclaimer)
}

hilbert<-function(n){
	i<-1:n
	1/outer(i-1,i,"+")
}

n<-1000
r<-numeric(n)
for(i in 1:n) {
	x<-rnorm(n)
	r[i]<-mean(x)
}

dept=read.csv("dept.csv")
dept
dept$name=trimws(dept$name)



dept$name =gsub("[[:digit:]]","",dept$name)

dept
dept$name=gsub("[[:punct:]]","",dept$name)

dept
mean_nofs=mean(dept$nofs,na.rm=TRUE)
mean_nofs
dept$nofs=ifelse(is.na(dept$nofs),mean_nofs,dept$nofs)
dept
Q=quantile(dept$nofs,probs=c(0.25,0.75),na.rm=FALSE)

iqr=IQR(dept$nofs)
low=Q[1]-1.5*iqr
up=Q[2]+1.5*iqr
low
up

#dept$name=trimws(dept$name)
dept=subset(dept,dept$nofs<up & dept$nofs>low)
dept
dept$name=tolower(dept$name)
dept
dept$name=trimws(dept$name)
dept

dept=dept[-which(dept$nofs>60),]
dept
dept=dept[-which(dept$nofs<2),]
dept

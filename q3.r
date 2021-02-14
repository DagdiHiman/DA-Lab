
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






#dept=dept[-grep("[[:alpha:]]",dept$nofs),]
#dept
mean_nofs=mean(dept$nofs,na.rm=TRUE)
mean_nofs
dept=data.frame(lapply(dept,function(x) ifelse(is.na(x),mean_nofs,x)))
dept
#Q=quantile(dept$nofs,probs=c(0.25,0.75),na.rm=FALSE)
iqr=IQR(dept$nofs)
low=Q[1]-1.5*iqr
up=Q[2]+1.5*iqr
dept=subset(dept,dept$nofs<up & dept$nofs>low)
dept
dept=dept[-which(dept$nofs>60),]
dept
dept=dept[-which(dept$nofs<2),]
dept
dept=dept[-grep("\\d",dept$name),]
dept
dept

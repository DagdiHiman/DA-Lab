fac = read.csv("faculty.csv",header = TRUE, sep = ",")
fac

fac$name = tolower(fac$name)
fac$name = trimws(fac$name)
fac

fac$name = gsub("[[:punct:]]","",fac$name)
fac$name = gsub("[[:digit:]]","",fac$name)
fac

fac$sal = gsub("[[:alpha:]]",NA,fac$sal)
fac

fac=fac[-which(fac$sal<0),]
fac

mean = mean(as.numeric(fac$sal), na.rm = TRUE)
mean = round(mean,0)
mean

fac$sal = ifelse(is.na(fac$sal), mean, fac$sal)
fac

fac$sal = as.numeric(format(fac$sal))

q = quantile(fac$sal, probs = c(0.25,0.75),na.rm = FALSE)
q
iqr = IQR(fac$sal)
iqr
low=q[1]-1.5*iqr
low
up=q[2]+1.5*iqr
up
fac = subset(fac, fac$sal>low & fac$sal<up)
fac


stud = read.csv("stud.csv", header = TRUE, sep = ",")
stud

stud$name = tolower(stud$name)
stud$name = trimws(stud$name)
stud

stud$name = gsub("[[:digit:]]","",stud$name)
stud$name = gsub("[[:punct:]]","",stud$name)
stud

stud = stud[-which(stud$cgpa<0),]
stud = stud[-which(stud$cgpa>10),]
stud

mean = mean(stud$cgpa, na.rm=TRUE)
mean

stud$cgpa = ifelse(is.na(stud$cgpa), mean, stud$cgpa)
stud

q = quantile(stud$cgpa, probs = c(0.25,0.75), na.rm = FALSE)
q
iqr=IQR(stud$cgpa)
iqr
low=q[1]-1.5*iqr
up=q[2]+1.5*iqr
low
up
stud = subset(stud, stud$cgpa>low & stud$cgpa<up)
stud

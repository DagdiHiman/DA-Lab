#a
stud=read.csv('stud1.csv')
stud
stud[4:8]
mean=colMeans(stud[4:8])
mean
mean=round(mean,digits=0)
mean
labels=c("phy","chem","maths","foc","cad")
boxplot(stud[4:8],names.arg=labels,col=rainbow(5))
pie(mean,labels,col=rainbow(5))
barplot(mean,names.arg=labels,col=rainbow(5))

#b
elect=read.csv('elect.csv')
elect
votes=unlist(elect$votes)
votes
votes_per=round(votes/sum(votes)*100)
votes_per
party_lab=c("A","B","C","D","E","F","G","H","I","J","K","L")
#party_lab=paste(party_lab,votes_per,sep=" ")   #concatination function
#party_lab
#party_lab=paste(party_lab,"%",sep=" ")
party_lab
#par(mar=rep(2,4))
boxplot(elect$votes,names.arg=party_lab)
pie(votes_per,party_lab,col=rainbow(11))
barplot(votes_per,names.arg=party_lab,col=rainbow(5))

elect=read.csv('elect2.csv')
elect
boxplot(elect,names.arg=party_lab)

#c
stud1=read.csv('stud1.csv')
stud1
stud1$avg=rowMeans(stud1[4:8])
stud1
female=stud1[stud1$gender=="F",]
female
male=stud1[stud1$gender=="M",]
male
str(stud1)
female_avg=mean(female$avg)
female_avg
male_avg=mean(male$avg)
male_avg
gender_avg=c(female_avg,male_avg)
gender_avg
gender_avg_per=round(gender_avg/sum(gender_avg)*100)
gender_avg_per
boxplot(male$avg,female$avg)
pie(gender_avg_per,c("male","female"))
barplot(gender_avg_per,names.arg=c("male","female"))


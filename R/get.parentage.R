`get.parentage` <-
function(colony.object,pairwise=FALSE){



if(pairwise==FALSE){			
Offspring<-colony.object$offspring
Offspring<-merge(Offspring,colony.object$maternity,by.x="offspringID",by.y="OffspringID",sort=FALSE,all.x=TRUE)
Offspring<-merge(Offspring,colony.object$paternity,by.x="offspringID",by.y="OffspringID",sort=FALSE,all.x=TRUE)



Offspring$InferredMum1<-as.character(Offspring$InferredMum1)
Offspring$InferredMum1[is.na(Offspring$InferredMum1)]<-"Unknown"
Offspring$InferredMum1<-as.factor(Offspring$InferredMum1)


Offspring$InferredDad1<-as.character(Offspring$InferredDad1)
Offspring$InferredDad1[is.na(Offspring$InferredDad1)]<-"Unknown"
Offspring$InferredDad1<-as.factor(Offspring$InferredDad1)


Offspring<-merge(Offspring,colony.object$mothers,by.x="InferredMum1",by.y="motherID")
Offspring<-merge(Offspring,colony.object$fathers,by.x="InferredDad1",by.y="fatherID")

			xx<-as.data.frame(table(Offspring$InferredMum1,Offspring$InferredDad1))
			names(xx)<-c("mums","dads","Freq")
			
			xx2<-expand.grid(mums=colony.object$mothers$motherID,dads=colony.object$fathers$fatherID)
			xx2$Freq<-rep(0,dim(xx2)[1])
			
			omit.these<-paste(xx$mums,xx$dads)
			
			temp<-paste(xx2$mums,xx2$dads)
			
			xx2<-xx2[!(temp%in%omit.these),]
			
			xx<-rbind(xx2,xx)
			
			return(xx)
			
}else{

Offspring<-colony.object$offspring
Offspring<-merge(Offspring,colony.object$pairwise.maternity,by.x="offspringID",by.y="OffspringID",sort=FALSE,all.x=TRUE)
names(Offspring)[names(Offspring)=="CandidateID"]<-"CandidateMum1"
names(Offspring)[names(Offspring)=="Confidence"]<-"CandidateMum1.Confidence"

Offspring<-merge(Offspring,colony.object$pairwise.paternity,by.x="offspringID",by.y="OffspringID",sort=FALSE,all.x=TRUE)
names(Offspring)[names(Offspring)=="CandidateID"]<-"CandidateDad1"
names(Offspring)[names(Offspring)=="Confidence"]<-"CandidateDad1.Confidence"


Offspring$CandidateMum1<-as.character(Offspring$CandidateMum1)
Offspring$CandidateMum1[is.na(Offspring$CandidateMum1)]<-"Unknown"
Offspring$CandidateMum1<-as.factor(Offspring$CandidateMum1)

Offspring$CandidateDad1<-as.character(Offspring$CandidateDad1)
Offspring$CandidateDad1[is.na(Offspring$CandidateDad1)]<-"Unknown"
Offspring$CandidateDad1<-as.factor(Offspring$CandidateDad1)

Offspring<-merge(Offspring,colony.object$mothers,by.x="CandidateMum1",by.y="mother")
Offspring<-merge(Offspring,colony.object$fathers,by.x="CandidateDad1",by.y="father")


			xx<-as.data.frame(table(Offspring$CandidateMum1,Offspring$CandidateDad1))
			names(xx)<-c("mums","dads","Freq")
			
			xx2<-expand.grid(mums=colony.object$mothers$motherID,dads=colony.object$fathers$fatherID)
			xx2$Freq<-rep(0,dim(xx2)[1])
			
			omit.these<-paste(xx$mums,xx$dads)
			
			temp<-paste(xx2$mums,xx2$dads)
			
			xx2<-xx2[!(temp%in%omit.these),]
			
			xx<-rbind(xx2,xx)
			
			return(xx)
		}}


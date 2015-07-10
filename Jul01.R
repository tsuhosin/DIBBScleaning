##########################################
# Description of DIBSS survery 0629 data #
##########################################
##update 07/10~07/05/2015 : general data description before data cleaning
##update 07/06/2015 : data cleaning and merge data
##update 07/07/2015 : Run a factor analysis across knowledges sharing inside and outside the group, 
#                     Rename variables according to constructs (see questions, altr, rep, learn, rel),
#                     Run correlation for q13 and 14, individual and across, after that factor analysis (conformative: 4 factors)


#setwd("~/Google Drive/DIBBS/DIBBS")
#setwd("~/Google Drive/02 research projects/DIBBS/DIBBS")
#install.packages("xlsx")
#install.packages("pastecs")
#install.packages("prettyR")
#install.packages("psych")
#install.packages("data.table")
#install.packages("nFactors")
#install.packages("MRCV")
#install.packages("GPArotation")
library(pastecs)
library(psych)
library(xlsx)
library(prettyR)
library(data.table)
library(MRCV)
library(GPArotation)
create  <-function(...) paste(substitute(list(...)))[-1] # function for indexing variables



dibbs0<-read.xlsx("NSF_NanoHub_0629_r1.xlsx",1,sheetName="NSF_NanoHub")
### Is it OK? : drop the raw with $Q1=4 // drop the section variables
dibbs<-dibbs0[-which(dibbs0$Q1==4),!names(dibbs0) %in% c("Q6","Q12","Q15","Q46","Q47")]
rownames(dibbs)<-NULL
########################
#treat inverse variables
#########################
dibbs$Q8.1_6<-ifelse(dibbs$Q8.1_6<8,7-dibbs$Q8.1_6,dibbs$Q8.1_6)
dibbs$Q7.1_6<-7-dibbs$Q7.1_6
##################################
#create dummy variable (grouping)#
##################################
## research_usage ##
# if Q4=34, then research_usage=2(non user ; ), otherwise research_usage=1(user)

research<-rep(0,dim(dibbs)[1])
research<-ifelse((dibbs$Q4==34),2,1)

## group ##
# if Q9=1, group=1(group), otherwise research_usage=2(non group) #

group<-rep(0,dim(dibbs)[1])
group<-ifelse((dibbs$Q9==1),1,2)

## combine dummy variable to data ##
dibbs1<-cbind(research,group,dibbs)


## rename the group (inside and outside) variable ##
setnames(dibbs1,paste("Q11.1_",6:10,sep=""),paste("group.in",1:5,sep=""))
setnames(dibbs1,paste("Q11.2_",6:10,sep=""),paste("group.out",1:5,sep=""))
## rename Q16~Q21
setnames(dibbs1,c(create(Q16.1_3, Q16.1_6, Q16.1_7, Q16.1_8, Q16.1_9, Q17.1_11, Q17.1_12, Q17.1_13, Q17.1_14, Q17.1_15, Q17.2_11, Q17.2_12, Q17.2_13, Q17.2_14, Q17.2_15, Q18.1_10, Q18.1_11, Q18.1_12, Q18.1_13, Q18.1_14, Q19.1_10, Q19.1_11, Q19.1_12, Q19.1_13, Q19.1_14, Q19.2_10, Q19.2_11, Q19.2_12, Q19.2_13, Q19.2_14, Q20.1_33, Q20.1_34, Q20.1_35, Q20.1_36, Q20.1_37, Q20.1_39, Q20.1_40, Q20.1_41, Q21.1_33, Q21.1_34, Q21.1_35, Q21.1_36, Q21.1_37, Q21.1_39, Q21.1_40, Q21.1_41)),
         c(paste("kn_access_ng",1:5,sep=""), paste("kn_access_g_i",1:5,sep=""), paste("kn_access_g_o",1:5,sep=""), paste("kn_protect_ng",1:5, sep=""), paste("kn_protect_g_i",1:5,sep=""), paste("kn_protect_g_o",1:5,sep=""),paste("risk_lossIP_nrs",1:3,sep=""), paste("risk_fear_nrs",1:3,sep=""), paste("risk_recipr_nrs",1:2,sep=""), paste("risk_lossIP_rs",1:3,sep=""), paste("risk_fear_rs",1:3,sep=""), paste("risk_recipr_rs",1:2,sep="")))
## rename Q22~Q23
setnames(dibbs1,c(create(Q22.1_10, Q22.1_11, Q22.1_12, Q22.1_14, Q22.1_16, Q22.1_18, Q22.1_19, Q22.1_20, Q23.1_10, Q23.1_11, Q23.1_12, Q23.1_13, Q23.1_14, Q23.1_18, Q23.1_19, Q23.1_20, Q23.1_23, Q23.1_25, Q23.1_26, Q23.1_27, Q23.1_28)),
         c(paste("comm_virt_co_ng",1:2,sep=""), paste("comm_offline_ng",1:6,sep=""), paste("comm_virt_co_n",1:2,sep=""), paste("comm_offline_g",1:4,sep=""),paste("comm_NanoHub_g",1:5,sep=""), paste("comm_offline_g",5:6,sep="")))
setnames(dibbs1,c(create(Q24.1_43, Q24.1_44, Q24.1_45, Q24.1_46, Q25.1_51, Q25.1_52, Q25.1_53, Q25.1_54, Q25.1_55, Q25.1_56, Q26.1_47, Q26.1_48, Q26.1_49, Q26.1_50, Q26.1_51, Q26.1_52, Q26.1_53, Q26.1_54, Q27.1_51, Q27.1_52, Q27.1_53, Q27.1_54, Q28.1_20, Q28.1_22, Q28.1_23, Q28.1_24, Q28.1_25, Q28.1_26, Q28.1_27, Q28.1_28, Q28.1_29, Q29_3, Q29_4, Q29_5, Q29_6, Q30, Q31, Q32, Q33, Q34, Q35.1_1, Q35.1_2, Q35.1_3)),
         c(paste("soc_ties",1:4,sep=""), paste("tms_g",1:6,sep=""), paste("norm_g",1:8,sep=""), paste("past_protect",1:4,sep=""), create(skill_elec_fun, skill_elec_nanotrans, skill_elec_theory, skill_matr, skill_photo, skill_mechn, skill_bio, skill_chem, skill_comp, use_purp_sc_rsch, use_purp_learn, use_purp_t_1t1, use_purp_t_course, degree, institution, resp_country, use_nanoHub, use_any_nanoHub), paste("mth_bias",1:3,sep="")))

###############################
# creating merged varibales #
##############################


## Know_creation ##
# Merge Q7(research=2) & Q8(research=1)
know_cr<-matrix(0,dim(dibbs)[1],6,)
ncc<-character()
for (i in c(1:6)){
  know_cr[,i]<-ifelse(is.na(dibbs[[paste("Q7.1_",i,sep="")]]),dibbs[[paste("Q8.1_",i,sep="")]],dibbs[[paste("Q7.1_",i,sep="")]])
  ncc[i]<-paste("know_cr",i,sep="")
}
as.data.frame(know_cr)
colnames(know_cr)<-ncc
head(know_cr)


###  merge(Q10&Q11_1) or (Q10&Q11_2) ???? ####
## know_sharing_ng vs.know_sharing_ginside ##
cd1<-matrix(0,dim(dibbs)[1],6)
ncd<-c(paste("know_shr_ng.in",c(1:5),sep=""))
for (i in c(1:4,6)){
  for (j in c(6:10)){
    cd1[,i]<-ifelse(is.na(dibbs[[paste("Q10.1_",i,sep="")]]),
                      dibbs[[paste("Q11.1_",j,sep="")]],dibbs[[paste("Q10.1_",i,sep="")]])
  }
}
know_shr_ng.in<-as.data.frame(cd1[,-5]) #merged non group and inside
colnames(know_shr_ng.in)<-ncd
head(know_shr_ng.in)


## know_sharing_gnon group vs.know_sharing_goutside ##
cd2<-matrix(0,dim(dibbs)[1],6)
ncd<-c(paste("know_shr_ng.out",c(1:5),sep=""))
for (i in c(1:4,6)){
  for (j in c(6:10)){
    cd2[,i]<-ifelse(is.na(dibbs[[paste("Q10.1_",i,sep="")]]),
                    dibbs[[paste("Q11.2_",j,sep="")]],dibbs[[paste("Q10.1_",i,sep="")]])
  }
}
know_shr_ng.out<-as.data.frame(cd2[,-5]) #NEW VARIABLE merging non group with outside
colnames(know_shr_ng.out)<-ncd
head(know_shr_ng.out)

## 07/07/15: merge Q13 Q14 to  altru,rel,learn,rep for researchers and learners Q13 is reserachers and Q14 is learners (non researchers)##

ch.nm013<-c(create(Q13.1_8,Q13.1_11,Q13.1_12,Q13.1_13,Q13.1_23,Q13.1_25,Q13.1_26,Q13.1_27,Q13.1_28,Q13.1_29,Q13.1_30,Q13.1_31,Q13.1_32,Q13.1_33))                   
ch.nm014<-c(create(Q14.1_8,Q14.1_11,Q14.1_12,Q14.1_13,Q14.1_23,Q14.1_25,Q14.1_26,Q14.1_27,Q14.1_28,Q14.1_29,Q14.1_30,Q14.1_31,Q14.1_32,Q14.1_33))
ch.nm1<-c(create(altru1,altru2,altru3,rep1,rep2,rep3,rel1,rel2,rel3,rel4,learn1,learn2,learn3,learn4))

cd3<-matrix(0,dim(dibbs)[1],length(ch.nm013))
for (k in 1:length(ch.nm013)){
    cd3[,k]<-ifelse(is.na(dibbs[[ch.nm013[k]]]),dibbs[[ch.nm014[k]]],dibbs[[ch.nm013[k]]])
  }
benefit<-as.data.frame(cd3)
colnames(benefit)<-ch.nm1
head(benefit)

##risk##
ch.nm020<-c(paste("risk_lossIP_nrs",1:3,sep=""), paste("risk_fear_nrs",1:3,sep=""), paste("risk_recipr_nrs",1:2,sep=""))
ch.nm021<-c(paste("risk_lossIP_rs",1:3,sep=""), paste("risk_fear_rs",1:3,sep=""), paste("risk_recipr_rs",1:2,sep=""))
ch.nm2<-c(paste("lossIP",1:3,sep=""), paste("fear",1:3,sep=""), paste("recipr",1:2,sep=""))

cd4<-matrix(0,dim(dibbs)[1],length(ch.nm020))
for (k in 1:length(ch.nm020)){
  cd4[,k]<-ifelse(is.na(dibbs1[[ch.nm020[k]]]),dibbs1[[ch.nm021[k]]],dibbs1[[ch.nm020[k]]])
}
risk<-as.data.frame(cd4)
colnames(risk)<-ch.nm2
head(risk)


##communication##
ch.nm022<-c(paste("comm_virt_co_ng",1:2,sep=""),paste("comm_offline_ng",1:6,sep=""))
ch.nm023<-c(paste("comm_virt_co_n",1:2,sep=""),paste("comm_offline_g",1:4,sep=""),paste("comm_offline_g",5:6,sep=""))
ch.nm3<-c(paste("virt_co",1:2,sep=""),paste("offline",1:6,sep=""))

cd5<-matrix(0,dim(dibbs)[1],length(ch.nm022))
for (k in 1:length(ch.nm022)){
  cd5[,k]<-ifelse(is.na(dibbs1[[ch.nm023[k]]]),dibbs1[[ch.nm022[k]]],dibbs1[[ch.nm023[k]]])
}
communication<-as.data.frame(cd5)
colnames(communication)<-ch.nm3
head(communication)




## combine merged variable to data ##
dibbs1<-cbind(dibbs1, know_cr, know_shr_ng.in, know_shr_ng.out, benefit, risk, communication)

head(dibbs1)


########################
# decriptive statistcis 07/05 version#
#######################

#stat.desc(dibbs1)
#describe(dibbs1,num.desc=c("mean","median","max","var","sd","valid.n"))

#stat.desc(dibbs2)
#describe(dibbs2,num.desc=c("mean","median","max","var","sd","valid.n"))


#########################
# Redefine variables
########################

##===define descrete variables

dsc_var<-c(create(research, group), paste("kn_protect_ng",1:5,sep=""), paste("kn_protect_g_i",1:5,sep=""), paste("kn_protect_g_o",1:5,sep=""), create(degree, institution, resp_country, use_nanoHub, use_any_nanoHub))
text_var<-c(create(Q1_TEXT, Q5_TEXT, Q9_TEXT))

##====define multiple response variables

#generate dummy variable.
r1<-as.numeric(dibbs1$research==1)
r2<-as.numeric(dibbs1$research==2)

g1<-as.numeric(dibbs1$research==1)
g2<-as.numeric(dibbs1$research==2)

dibbs1<-cbind(dibbs1,r1,r2,g1,g2)

multi_var_rsch<-c(create(r1,r2, use_purp_sc_rsch, use_purp_learn, use_purp_t_1t1, use_purp_t_course))
multi_var_gr<-c(create(g1,g2, use_purp_sc_rsch, use_purp_learn, use_purp_t_1t1, use_purp_t_course))

d1<-dibbs1[multi_var_rsch]
d1[is.na(d1)]<-0
d2<-dibbs1[multi_var_gr]
d2[is.na(d2)]<-0

item.response.table(data = d1, I = 2, J=4)
item.response.table(data = d2, I = 2, J = 4)


#frequency table and barplot for descrete vairables

for (i in 1:length(dsc_var)) {
  print(freq(dibbs1[dsc_var[i]]))
  barplot(table(dibbs1[dsc_var[i]]),main=dsc_var[i],axes=T,las=3)
}


for (i in 1:length(text_var)) {
 a<-table(dibbs1[text_var[i]])
 print(a)
}


##===define continuous variables

#07/06/15 : Q7~Q12 define con. variables (know_cr, know_shr_ng.in, know_shr_ng.out)
# 07/07/15 : Q13~Q14 define con. variables (benefit:dataframe)
con_var<-c(colnames(know_cr), colnames(know_shr_ng.in), colnames(know_shr_ng.out), colnames(benefit),paste("kn_access_ng",1:5,sep=""),paste("kn_access_g_i",1:5,sep=""),paste("kn_access_g_o",1:5,sep=""), colnames(risk), colnames(communication), paste("soc_ties",1:4,sep=""), paste("tms_g",1:6,sep=""), paste("norm_g",1:8,sep=""), paste("past_protect",1:4,sep=""), 
           create(skill_elec_fun, skill_elec_nanotrans, skill_elec_theory, skill_matr, skill_photo, skill_mechn, skill_bio, skill_chem, skill_comp))


head(dibbs1)

## describe for continuous variables (tatal & by dummy variable)
# 07/06/15 # 07/07/15
describe(dibbs1[con_var])
describeBy(dibbs1[con_var],dibbs1$research,skew=F, ranges=F)
describeBy(dibbs1[con_var],dibbs1$group,skew=F, ranges=F)







#histogram for continuous variables

for (i in 1:length(con_var)) {
  hist(table(dibbs1[con_var[i]]),main=con_var[i], freq = FALSE, ylim=c(0,0.25))
  curve(dnorm(x, mean=mean(unlist(dibbs1[con_var[i]]), na.rm=TRUE), sd=sd(unlist(dibbs1[con_var[i]]),na.rm=TRUE)), col="blue", lwd=2, add=TRUE)
  lines(density(unlist(dibbs1[con_var[i]]),na.rm=TRUE),col = "red", lwd=2)
}


#################
# Subset data   #
#################

#Subset by research variable#
dibbs1rsc<-dibbs1[research==1,]
dibbs1nrsc<-dibbs1[research==2,]

#Subset by group variable#
dibbs1gr<-dibbs1[group==1,]
dibbs1ngr<-dibbs1[group==2,]


##################
# correlation    #
##################

#colnames(know_cr), colnames(know_shr_ng.in), colnames(know_shr_ng.out),
#dibbs1rsc<-dibbs1[research==1,]
#dibbs1nrsc<-dibbs1[research==2,]

#dibbs1gr<-dibbs1[group==1,]
#dibbs1ngr<-dibbs1[group==2,]


## correlation of knowledge creation variable ##

cor(dibbs1[colnames(know_cr)]) #correlation of whole observation
cor(dibbs1rsc[colnames(know_cr)])#correlation within research group
cor(dibbs1nrsc[colnames(know_cr)])#correlation within nonrearch group

## correlation of knowledge sharing variable ##

##correlation of group variable

#correlation between inside vs. outside 
cor(dibbs1gr[paste("group.in",1:5,sep="")],dibbs1gr[paste("group.out",1:5,sep="")],use="complete")
cor(dibbs1gr[paste("group.in",1:5,sep="")]) #correlation whithin inside
cor(dibbs1gr[paste("group.out",1:5,sep="")],use="complete") #correlation within outside


##correlation of nongroup varaible

cor(dibbs1ngr[paste("Q10.1_",c(1:4,6),sep="")],use="complete") #correlation within group variable


##07/07/15
##correlation of benefit (altru,rel,learn,rep)

cor(dibbs1[colnames(benefit)]) #correlation of whole observation 
cor(dibbs1rsc[colnames(benefit)]) #correlation within research group
cor(dibbs1nrsc[colnames(benefit)]) #correlation within nonrearch group

##07/09/15
##correlation of kn_access ##

##correlation of group variable
#correlation between inside vs. outside 
cor(dibbs1gr[paste("kn_access_g_i",1:5,sep="")],dibbs1gr[paste("kn_access_g_o",1:5,sep="")],use="complete")
cor(dibbs1gr[paste("kn_access_g_i",1:5,sep="")]) #correlation whithin inside
cor(dibbs1gr[paste("kn_access_g_o",1:5,sep="")],use="complete") #correlation within outside


##correlation of nongroup varaible

cor(dibbs1ngr[paste("kn_access_ng",1:5,sep="")],use="complete") #correlation within group variable


##correlation of risk

cor(dibbs1[colnames(risk)]) #correlation of whole observation 
cor(dibbs1rsc[colnames(risk)]) #correlation within research group
cor(dibbs1nrsc[colnames(risk)]) #correlation within nonrearch group

##correlation of communication


cor(dibbs1[colnames(communication)]) #correlation of whole observation 
cor(dibbs1gr[colnames(communication)]) #correlation within research group
cor(dibbs1ngr[colnames(communication)]) #correlation within nonrearch group



##############
## factor analysis
##############
##########
## know_cr
##########
# Determine Number of Factors to Extract
library(nFactors)
ev1 <- eigen(know_cr) # get eigenvalues
ap1 <- parallel(subject=nrow(know_cr),var=ncol(know_cr),
               rep=1000,cent=.05)
nS1 <- nScree(x=ev1$values, aparallel=ap1$eigen$qevpea)
plotnScree(nS1)

# Maximum Likelihood Factor Analysis
# entering raw data and extracting 2 factors, with varimax rotation 
fit1 <- fa(know_cr,2,n.obs=71)
print(fit1, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2 
load1 <- fit1$loadings 
plot(fit1)
plot(load1) # set up plot 
text(load1,labels=colnames(know_cr),cex=.7) # add variable names
fa.diagram(fit1)

## inside 
dgi<-dibbs1gr[c(paste("group.in",1:5,sep=""))]
ev2 <- eigen(dgi) # get eigenvalues
ap2 <- parallel(subject=nrow(know_cr),var=ncol(know_cr),
                rep=1000,cent=.1)
nS2 <- nScree(x=ev1$values, aparallel=ap1$eigen$qevpea)
plotnScree(nS1)

# Maximum Likelihood Factor Analysis
# entering raw data and extracting 2 factors, 
# with varimax rotation 
fit2 <- factanal(dibbs1gr[paste("group.in",1:5,sep="")], 2, rotation="varimax")
print(fit2, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2 
load2 <- fit2$loadings[,1:2] 
plot(load2,type="p",col=2) # set up plot 
text(load2,labels=colnames(benefit),cex=.7) # add variable names

## outside 

# Maximum Likelihood Factor Analysis
# entering raw data and extracting 2 factors, 
# with varimax rotation 
#fit3 <- factanal(dibbs1gr[paste("group.out",1:5,sep="")], 2, rotation="varimax")
#print(fit3, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2 
#load3 <- fit3$loadings[,1:2] 
#plot(load3,type="p",col=2) # set up plot 
#text(load3,labels=colnames(benefit),cex=.7) # add variable names

###########
## benefit
##########

# Maximum Likelihood Factor Analysis
# entering raw data and extracting 4 factors, 
# with varimax rotation 
fit4 <- factanal(benefit, 4, rotation="varimax")
print(fit4, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2 
load4 <- fit4$loadings[,1:2] 
plot(load4,type="p",col=2) # set up plot 
text(load4,labels=colnames(benefit),cex=.7) # add variable names


safaaa
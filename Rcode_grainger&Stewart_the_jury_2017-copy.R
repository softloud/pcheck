#####Simulate data from Young et al 2017################################
# setwd("~/REFRESH")
library(ggplot2)
library(truncnorm)#trucated between 1 & 5 as it is on a likert scale
library(car)
library(heplots)
library(grid)
set.seed(10010)# ensure simulation is reproducable
#Frequency ANOVA simulation
nSim=10000
runTest <- function(){
  NoneT1<-rtruncnorm(n=469,a=1,b=5,2.44,0.74)
  NoneT2<-rtruncnorm(n=469,a=1,b=5,2.57,0.76)
  NoneT3<-rtruncnorm(n=469,a=1,b=5,2.44,0.77)
  ENewT1<-rtruncnorm(105,a=1,b=5,2.55,0.69)
  ENewT2<-rtruncnorm(105,a=1,b=5,2.63,0.75)
  ENewT3<-rtruncnorm(105,a=1,b=5,2.47,0.72)
  FaceT1<-rtruncnorm(510,a=1,b=5,2.45,0.88)
  FaceT2<-rtruncnorm(510,a=1,b=5,2.47,0.91)
  FaceT3<-rtruncnorm(510,a=1,b=5,2.41,0.91)
  MagT1<-rtruncnorm(327,a=1,b=5,2.38,0.79)
  MagT2<-rtruncnorm(327,a=1,b=5,2.36,0.81)
  MagT3<-rtruncnorm(327,a=1,b=5,2.37,0.8)
  EnewFaceT1<-rtruncnorm(134,a=1,b=5,2.46,0.82)
  EnewFaceT2<-rtruncnorm(134,a=1,b=5,2.54,0.81)
  EnewFaceT3<-rtruncnorm(134,a=1,b=5,2.48,0.84)
  EnewMagT1<-rtruncnorm(116,a=1,b=5,2.52,0.73)
  EnewMagT2<-rtruncnorm(116,a=1,b=5,2.49,0.84)
  EnewMagT3<-rtruncnorm(116,a=1,b=5,2.48,0.83)
  FaceMagT1<-rtruncnorm(250,a=1,b=5,2.46,0.81)
  FaceMagT2<-rtruncnorm(250,a=1,b=5,2.44,0.81)
  FaceMagT3<-rtruncnorm(250,a=1,b=5,2.48,0.83)
  AllIntT1<-rtruncnorm(107,a=1,b=5,2.36,0.83)
  AllIntT2<-rtruncnorm(107,a=1,b=5,2.42,0.88)
  AllIntT3<-rtruncnorm(107,a=1,b=5,2.41,0.9)

  Nonedf<-data.frame(NoneT1,NoneT2,NoneT3)
  Nonedf<-setNames(Nonedf,c("T1","T2","T3"))
  Magdf<-data.frame(MagT1,MagT2,MagT3)
  Magdf<-setNames(Magdf,names(Nonedf))
  EnewFacedf<-data.frame(EnewFaceT1,EnewFaceT2,EnewFaceT3)
  EnewFacedf<-setNames(EnewFacedf,names(Nonedf))
  Facedf<-data.frame(FaceT1,FaceT2,FaceT3)
  Facedf<-setNames(Facedf,names(Nonedf))
  ENewdf<-data.frame(ENewT1,ENewT2,ENewT3)
  ENewdf<-setNames(ENewdf,names(Nonedf))
  EnewMagdf<-data.frame(EnewMagT1,EnewMagT2,EnewMagT3)
  EnewMagdf<-setNames(EnewMagdf,names(Nonedf))
  FaceMagdf<-data.frame(FaceMagT1,FaceMagT2,FaceMagT3)
  FaceMagdf<-setNames(FaceMagdf,names(Nonedf))
  AllIntdf<-data.frame(AllIntT1,AllIntT2,AllIntT3)
  AllIntdf<-setNames(AllIntdf,names(Nonedf))
  mainDF<-rbind(Nonedf,Magdf,ENewdf,Facedf,EnewFacedf,EnewMagdf,FaceMagdf,AllIntdf)
  head(mainDF)
  IntType<-c((rep("None",469)),
             (rep("Magazine",327)),
             (rep("E-Newsletter", 105)),
             (rep("Facebook", 510)),
             (rep("E-Newsletter & Facebook", 134)),
             (rep("E-Newsletter & Magazine", 116)),
             (rep("Facebook & Magazine", 250)),
             (rep("All Interventions", 107)))
  dim(mainDF)
  mainDF$IntType<-IntType
  mainDF$ID<-1:2018
  multmodel<-lm(cbind(T1,T2,T3) ~ 1,data=mainDF)
  Time<-factor(c("T1","T2","T3"), ordered=F)
  mod=Anova(multmodel,idata=data.frame(Time),idesign=~Time,type="III")
  summary(mod,multivariate=F)[[4]][2,6]

}
p<-replicate(nSim, runTest())# run the simulation

plot(density(p),main="Distribution of P-values")
p<-as.data.frame(p)
ggplot(p,aes(p))+geom_density(fill="blue") +
  geom_vline(xintercept = 0.05,colour="red", linetype = "longdash")+
  theme_classic()
power=sum(p<=0.05)/nSim
power

# ####plot density
library(reshape)
mainDF$ID<-NULL
meltmain<-melt(mainDF)
head(meltmain)
ggplot(meltmain) + geom_density(aes(x = value,
                                 y = ..density.., colour = variable,fill=variable))+
  theme_classic()


####Figure 1


##figure 1a
plot1<-ggplot(meltmain) + geom_density(aes(x = value,
                                    y = ..density.., colour = variable,fill=variable))+
  labs(x="Frequency of waste")+
  theme_classic()
plot1 + annotation_custom(
  grob = textGrob(label = "A", hjust = 0, gp = gpar(cex = 1.5)),
  ymin = 0.4,      # Vertical position of the textGrob
  ymax = 0.5,
  xmin = 0.5,         # Note: The grobs are positioned outside the plot area
  xmax = 1.5)


##figure 1b
plot2<-ggplot(p,aes(p))+geom_density(fill="blue") +
  geom_vline(xintercept = 0.05,colour="red", linetype = "longdash")+
  labs(x="p")+
  theme_classic()
plot2 + annotation_custom(
  grob = textGrob(label = "B", hjust = 0, gp = gpar(cex = 1.5)),
  ymin = 6,      # Vertical position of the textGrob
  ymax = 6.5,
  xmin = 0.75,         # Note: The grobs are positioned outside the plot area
  xmax = 1)

#multiplot(plot1,plot2)
# ###Frequency T test
nSim=10000
runTest3 <- function(){
  E1<-rnorm(469,2.47,0.910)
  E2<-rnorm(469,2.41,0.910)
  mdiff<-mean(E1-E2)
  mod<-t.test(E1, E2, paired = TRUE)
  mod$p.value
  list(p=mod$p.value,diff=mdiff)
  }
p1<-replicate(nSim, runTest3())
p1<-as.data.frame(p1)
p1<-t(p1)
p1<-as.data.frame(unlist(p1))
head(p1)
dim(p1)
p1$type=c(rep("p",nSim),rep("dif",nSim))
p1$value<-p1$`unlist(p1)`
p1$`unlist(p1)`<-NULL
names(p1)
diff<-p1[10001:20000,]
p2<-p1[1:10000,]



ggplot(p2,aes(value))+geom_density(fill="blue") +
  geom_vline(xintercept = 0.05,colour="red", linetype = "longdash")+
  theme_classic()

df<-data.frame(diff,p2)

power=sum(p2<=0.05)/nSim
power
####plot difference
ggplot(df,aes(value.1,abs(value)))+geom_point(col="blue")+
  geom_vline(xintercept = 0.05,colour="red", linetype = "longdash")+
  labs(x="p value", y= "Absolute difference in means")

##figure 2a
plot3<-ggplot(p2,aes(value))+geom_density(fill="blue") +
  geom_vline(xintercept = 0.05,colour="red", linetype = "longdash")+
  labs(x="p value")+
  theme_classic()
plot3 + annotation_custom(
  grob = textGrob(label = "A", hjust = 0, gp = gpar(cex = 1.5)),
  ymin = 1.5,      # Vertical position of the textGrob
  ymax = 2,
  xmin = 0.75,         # Note: The grobs are positioned outside the plot area
  xmax = 1)


##figure 2b
plot4<-ggplot(df,aes(value.1,abs(value)))+geom_point(col="blue")+
  geom_vline(xintercept = 0.05,colour="red", linetype = "longdash")+
  labs(x="p value", y= "Absolute difference in means")+
  theme_classic()
plot4 + annotation_custom(
  grob = textGrob(label = "B", hjust = 0, gp = gpar(cex = 1.5)),
  ymin = 0.2,      # Vertical position of the textGrob
  ymax = 0.3,
  xmin = 0.75,         # Note: The grobs are positioned outside the plot area
  xmax = 1)


# Clear workspace
rm(list = ls())

# Clear plots
dev.off()

# Clear console
cat("\014")  # ctrl+L


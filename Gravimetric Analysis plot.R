library("readxl")
library(ggplot2)
dataset1 <- read_excel("D:\\data.xlsx", sheet ="Sheet1")


p<- list()
for (i in 2:19) {
  Plot_Fun(i)
}


Plot_Fun<- function(i) {
  print(i)
  Shake<- data.frame(dataset1[1:5, i])
  Shake<- lapply(Shake,as.numeric)
  Shake<-cbind(data.frame(dataset1[1:5, 1]), Shake)
  Shake<-cbind(Condition='Shaking', Shake)
  ShakeDev<- data.frame(dataset1[7:11, i])
  Shake$dev= ShakeDev
  
  StrainName<- names(dataset1[i])
  StrainName<-toString(StrainName)
  
  colnames(Shake)[4]="StDev"
  colnames(Shake)[3]= "Strain"
  
  Shake[, 2]<- sapply(Shake[, 2], as.numeric)
  Shake[, 4]<- sapply(Shake[, 4], as.numeric)
  Shake[, 4]<- sapply(Shake[, 4], as.numeric)
  
  #-----------------
  NoShake<- data.frame(dataset1[14:18, i])
  NoShake<- lapply(NoShake,as.numeric)
  NoShake<-cbind(data.frame(dataset1[1:5, 1]), NoShake)
  NoShake<-cbind(Condition='No Shaking', NoShake)
  NoShakeDev<- data.frame(dataset1[20:24, i])
  NoShake$dev= NoShakeDev
  colnames(NoShake)[4]="StDev"
  colnames(NoShake)[3]= "Strain"
  
  NoShake[, 2]<- sapply(NoShake[, 2], as.numeric)
  NoShake[, 4]<- sapply(NoShake[, 4], as.numeric)
  NoShake[, 4]<- sapply(NoShake[, 4], as.numeric)
  #---------------------
  
  Df<- rbind(Shake, NoShake)
  Df
  p=ggplot(Df, aes(x=Time, y=Strain, group=Condition, color=Condition)) + geom_line() + geom_point() +
    labs(subtitle=paste("Strain No ",StrainName), 
         y="biomass, g", 
         x="Time, days",

    )+
    geom_errorbar(aes(ymin=Strain-StDev, ymax=Strain+StDev), width=0.4) +
    geom_line(size=0.9) + geom_point(size=1.6)+
    ylim(0,0.3)+
    scale_color_brewer(palette="Paired")+theme_minimal()
  
  ggsave(p, file=paste0("Actinobacteria_Growth_Curve_", i-1,".png"), width = 14, height = 10, units = "cm")
  #print(p)
  
}

rm(Shake)
rm(NoShake)






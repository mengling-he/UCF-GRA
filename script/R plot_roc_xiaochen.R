rm(list=ls())
library(tidyr) #gathering function and spread function
library(ggplot2) #plot package
library(gridExtra) #pair plots
library(plotROC)
library(pROC)

#data import
bad <- read.csv("/Users/xiaochenzhu/Dropbox/PhD/data/Datafacial/New folder/BadDPMCropVGGFeatureSet3cityblockNormalized.txt")
good <- read.csv("/Users/xiaochenzhu/Dropbox/PhD/data/Datafacial/New folder/GoodDPMCropVGGFeatureSet3cityblockNormalized.txt")
ugly <- read.csv("/Users/xiaochenzhu/Dropbox/PhD/data/Datafacial/New folder/UglyDPMCropVGGFeatureSet3cityblockNormalized.txt")
good$targetFilenames <- 0
good <- as.matrix(good[,c(1:3,5)])
good[which(good[,1]==good[,2]),3] <- 1
good <- as.data.frame(good)
good$simValues <- as.numeric(as.character(good$simValues))
names(good)[3] <- 'D'

bad$targetFilenames <- 0
bad <- as.matrix(bad[,c(1:3,5)])
bad[which(bad[,1]==bad[,2]),3] <- 1
bad <- as.data.frame(bad)
bad$simValues <- as.numeric(as.character(bad$simValues))
names(bad)[3] <-'D'

NROW(good)+NROW(bad)+NROW(ugly)



ugly$targetFilenames <- 0
ugly <- as.matrix(ugly[,c(1:3,5)])
ugly[which(ugly[,1]==ugly[,2]),3] <- 1
ugly <- as.data.frame(ugly)
ugly$simValues <- as.numeric(as.character(ugly$simValues))
names(ugly)[3] <- 'D'


#remove outliers from data
good.clean <- good[-which(good$simValues==1000),]
bad.clean <- bad[-which(bad$simValues==1000),]
ugly.clean <- ugly[-which(ugly$simValues==1000),]

#improt feature data
feature <- read.csv("/Users/xiaochenzhu/Dropbox/PhD/data/Datafacial/nd1-subjects.csv")

#split disease and nondisease
good_i <- good.clean[which(good.clean$D=='0'),]
bad_i <- bad.clean[which(bad.clean$D=='0'),]
ugly_i <- ugly.clean[which(ugly.clean$D=='0'),]

good_g3 <- good.clean[which(good.clean$D=='1'),]
bad_g3 <- bad.clean[which(bad.clean$D=='1'),]
ugly_g3 <- ugly.clean[which(ugly.clean$D=='1'),]

# ind_g <- sample(seq_len(NROW(good_i)),10000) # sampling indicator form a sequence with the length of sample size
# ind_b <- sample(seq_len(NROW(bad_i)),10000) # sampling indicator form a sequence with the length of sample size
# ind_u <- sample(seq_len(NROW(ugly_i)),10000) # sampling indicator form a sequence with the length of sample size
# 
# good_i3 <- good_i[ind_g,]
# bad_i3 <- bad_i[ind_b,]
# ugly_i3 <- ugly_i[ind_u,]


good_i3 <- read.table("/Users/xiaochenzhu/Dropbox/PhD/data/good_i3.txt", sep="\t",header = T)
bad_i3 <- read.table("/Users/xiaochenzhu/Dropbox/PhD/data/bad_i3.txt", sep="\t",header = T)
ugly_i3 <- read.table("/Users/xiaochenzhu/Dropbox/PhD/data/ugly_i3.txt", sep="\t",header = T)


for(i in 1:NROW(good_g3))
{
  #set G1 represent the gender of subject 1 and G2 be the gender of subject 2
  #set B1 represent the year of born of subject 1 and B2 be the class of subject 2
  #in gender, 0 means female and 1 means male. This is only for convenient. Note that the data tarnsformed to numerical data since is easy to calculate in the future
  good_g3$G1[i]<- as.numeric(feature$gender[substr(good_g3[i,1],5,9)==substr(feature$subject_id,5,9)])-1
  good_g3$G2[i]<- as.numeric(feature$gender[substr(good_g3[i,2],5,9)==substr(feature$subject_id,5,9)])-1
  good_g3$B1[i]<- as.numeric(feature$yob[substr(good_g3[i,1],5,9)==substr(feature$subject_id,5,9)])
  good_g3$B2[i]<- as.numeric(feature$yob[substr(good_g3[i,2],5,9)==substr(feature$subject_id,5,9)])
  
}
for(i in 1:NROW(good_i3))
{
  #set G1 represent the gender of subject 1 and G2 be the gender of subject 2
  #set B1 represent the year of born of subject 1 and B2 be the class of subject 2
  #in gender, 0 means female and 1 means male. This is only for convenient. Note that the data tarnsformed to numerical data since is easy to calculate in the future
  good_i3$G1[i]<- as.numeric(feature$gender[substr(good_i3[i,1],5,9)==substr(feature$subject_id,5,9)])-1
  good_i3$G2[i]<- as.numeric(feature$gender[substr(good_i3[i,2],5,9)==substr(feature$subject_id,5,9)])-1
  good_i3$B1[i]<- as.numeric(feature$yob[substr(good_i3[i,1],5,9)==substr(feature$subject_id,5,9)])
  good_i3$B2[i]<- as.numeric(feature$yob[substr(good_i3[i,2],5,9)==substr(feature$subject_id,5,9)])
}


for(i in 1:NROW(bad_g3))
{
  #set G1 represent the gender of subject 1 and G2 be the gender of subject 2
  #set B1 represent the year of born of subject 1 and B2 be the class of subject 2
  #in gender, 0 means female and 1 means male. This is only for convenient. Note that the data tarnsformed to numerical data since is easy to calculate in the future
  bad_g3$G1[i]<- as.numeric(feature$gender[substr(bad_g3[i,1],5,9)==substr(feature$subject_id,5,9)])-1
  bad_g3$G2[i]<- as.numeric(feature$gender[substr(bad_g3[i,2],5,9)==substr(feature$subject_id,5,9)])-1
  bad_g3$B1[i]<- as.numeric(feature$yob[substr(bad_g3[i,1],5,9)==substr(feature$subject_id,5,9)])
  bad_g3$B2[i]<- as.numeric(feature$yob[substr(bad_g3[i,2],5,9)==substr(feature$subject_id,5,9)])
}
for(i in 1:NROW(bad_i3))
{
  #set G1 represent the gender of subject 1 and G2 be the gender of subject 2
  #set B1 represent the year of born of subject 1 and B2 be the class of subject 2
  #in gender, 0 means female and 1 means male. This is only for convenient. Note that the data tarnsformed to numerical data since is easy to calculate in the future
  bad_i3$G1[i]<- as.numeric(feature$gender[substr(bad_i3[i,1],5,9)==substr(feature$subject_id,5,9)])-1
  bad_i3$G2[i]<- as.numeric(feature$gender[substr(bad_i3[i,2],5,9)==substr(feature$subject_id,5,9)])-1
  bad_i3$B1[i]<- as.numeric(feature$yob[substr(bad_i3[i,1],5,9)==substr(feature$subject_id,5,9)])
  bad_i3$B2[i]<- as.numeric(feature$yob[substr(bad_i3[i,2],5,9)==substr(feature$subject_id,5,9)])
}


for(i in 1:NROW(ugly_g3))
{
  #set G1 represent the gender of subject 1 and G2 be the gender of subject 2
  #set B1 represent the year of born of subject 1 and B2 be the class of subject 2
  #in gender, 0 means female and 1 means male. This is only for convenient. Note that the data tarnsformed to numerical data since is easy to calculate in the future
  ugly_g3$G1[i]<- as.numeric(feature$gender[substr(ugly_g3[i,1],5,9)==substr(feature$subject_id,5,9)])-1
  ugly_g3$G2[i]<- as.numeric(feature$gender[substr(ugly_g3[i,2],5,9)==substr(feature$subject_id,5,9)])-1
  ugly_g3$B1[i]<- as.numeric(feature$yob[substr(ugly_g3[i,1],5,9)==substr(feature$subject_id,5,9)])
  ugly_g3$B2[i]<- as.numeric(feature$yob[substr(ugly_g3[i,2],5,9)==substr(feature$subject_id,5,9)])
}
for(i in 1:NROW(ugly_i3))
{
  #set G1 represent the gender of subject 1 and G2 be the gender of subject 2
  #set B1 represent the year of born of subject 1 and B2 be the class of subject 2
  #in gender, 0 means female and 1 means male. This is only for convenient. Note that the data tarnsformed to numerical data since is easy to calculate in the future
  ugly_i3$G1[i]<- as.numeric(feature$gender[substr(ugly_i3[i,1],5,9)==substr(feature$subject_id,5,9)])-1
  ugly_i3$G2[i]<- as.numeric(feature$gender[substr(ugly_i3[i,2],5,9)==substr(feature$subject_id,5,9)])-1
  ugly_i3$B1[i]<- as.numeric(feature$yob[substr(ugly_i3[i,1],5,9)==substr(feature$subject_id,5,9)])
  ugly_i3$B2[i]<- as.numeric(feature$yob[substr(ugly_i3[i,2],5,9)==substr(feature$subject_id,5,9)])
}

ugly_g3$GD <- 0
ugly_i3$GD <- 0
bad_g3$GD <- 0
bad_i3$GD <- 0
good_g3$GD <- 1
good_i3$GD <- 1
ugly_g3$BD <- 0
ugly_i3$BD <- 0
bad_g3$BD <- 1
bad_i3$BD <- 1
good_g3$BD <- 0
good_i3$BD <- 0
ugly_g3$Q <- 'Ugly'
ugly_i3$Q <-'Ugly'
bad_g3$Q <- 'Bad'
bad_i3$Q <- 'Bad'
good_g3$Q <- 'Good'
good_i3$Q <- 'Good'
ugly_g3$Dis <- 'Genuine'
ugly_i3$Dis <- 'Imposter'
bad_g3$Dis <- 'Genuine'
bad_i3$Dis <- 'Imposter'
good_g3$Dis <- 'Genuine'
good_i3$Dis <- 'Imposter'

face_sample <- rbind(good_g3, good_i3, bad_g3, bad_i3, ugly_g3, ugly_i3) 

sim_data <- data.frame(Score=face_sample$simValues, Group=face_sample$Dis, D=abs(as.numeric(face_sample$D)-2), Gender=face_sample$G1, YOB=face_sample$B1,
                       Good=face_sample$GD, Bad = face_sample$BD, Quality = face_sample$Q)
sim_data$Sf <- 0
sim_data$Sm <- 0

sim_data[which(sim_data$Gender==0),]$Sf <- sim_data[which(sim_data$Gender==0),]$Score
sim_data[which(sim_data$Gender==1),]$Sm <- sim_data[which(sim_data$Gender==1),]$Score

sim_data[which(sim_data$Sf==0),]$Sf <- NA
sim_data[which(sim_data$Sm==0),]$Sm <- NA


windows(w=5,h=3)
ggplot(sim_data) + 
  geom_density(aes(x = Score, linetype = Group), 
               alpha = 0.1,show.legend=FALSE) +#show.legend=FALSE means elimate the square outside the legned
  stat_density(aes(x = Score, linetype = Group),
               geom="line",position="identity") +
  scale_linetype_manual(values=c("solid", "dotted"))+
  theme(#plot.title = element_text(hjust = 0.5), #change the position of title
    #legend.position="none"#, #remove legend
    #     axis.title.x=element_blank(), axis.title.y=element_blank(), #remove the title of x and y axis
    #    axis.text.x = element_text(size=7))#change the size of scale text
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.text=element_text(size=20), #legend text size
    axis.title = element_text(size = 20), #axis title size
    axis.text = element_text(size = 18),#, #axis text size
    legend.key.size =  unit(0.5, "in")#, #axis text size
  )


windows(w=5,h=3)
ggplot(sim_data[which(sim_data$Gender==0),]) + 
  geom_density(aes(x = Score, linetype = Group), 
               alpha = 0.1,show.legend=FALSE) +#show.legend=FALSE means elimate the square outside the legned
  stat_density(aes(x = Score, linetype = Group),
               geom="line",position="identity")+
  scale_linetype_manual(values=c("solid", "dotted"))+
  theme(#plot.title = element_text(hjust = 0.5), #change the position of title
    legend.position="none"#, #remove legend
    #     axis.title.x=element_blank(), axis.title.y=element_blank(), #remove the title of x and y axis
    #    axis.text.x = element_text(size=7))#change the size of scale text
  )


windows(w=5,h=3)
ggplot(sim_data[which(sim_data$Gender==1),]) + 
  geom_density(aes(x = Score, linetype = Group), 
               alpha = 0.1,show.legend=FALSE) +#show.legend=FALSE means elimate the square outside the legned
  stat_density(aes(x = Score, linetype = Group),
               geom="line",position="identity") +
  scale_linetype_manual(values=c("solid", "dotted"))+
  theme(#plot.title = element_text(hjust = 0.5), #change the position of title
    legend.position="none"#, #remove legend
    #     axis.title.x=element_blank(), axis.title.y=element_blank(), #remove the title of x and y axis
    #    axis.text.x = element_text(size=7))#change the size of scale text
  )


test <-data.frame(D = sim_data$D, Pooled = sim_data$Score, Female = sim_data$Sf, Male = sim_data$Sm)
longtest <- melt_roc(test, "D", c("Pooled", "Female","Male"))
longtest$name <- factor(longtest$name, levels = c("Pooled", "Female","Male")) 

windows(w=5,h=4.5)
ggplot(longtest,aes(d = D, m = M,linetype = name)) + geom_roc(n.cuts = 0) +
  labs(y = "True Positive Rate", x="False Positive Rate") +  #change the title of axis
  theme(legend.title = element_blank(),  #remove legend title
        legend.position = c(0.8, 0.3),
        legend.text=element_text(size=20), #legend text size
        axis.title = element_text(size = 20), #axis title size
        axis.text = element_text(size = 18),#, #axis text size
        legend.key.size =  unit(0.5, "in"))#, #axis text size
#        panel.grid.major = element_blank(), 
#        panel.grid.minor = element_blank(), #remove grid
#        panel.background = element_blank(), #remove color
#        axis.line = element_line(colour = "black"))#add x and y axis




####multi plot

p1 <- ggplot(sim_data) + 
  geom_density(aes(x = Score, linetype = Group), 
               alpha = 0.1,show.legend=FALSE) +#show.legend=FALSE means elimate the square outside the legned
  stat_density(aes(x = Score, linetype = Group),
               geom="line",position="identity") +
  scale_linetype_manual(values=c("solid", "dotted"))+
  ggtitle("Pooled Data") + #add title
  theme(plot.title = element_text(hjust = 0.5), #change the position of title
    #legend.position="none"#, #remove legend
    #     axis.title.x=element_blank(), axis.title.y=element_blank(), #remove the title of x and y axis
    #    axis.text.x = element_text(size=7))#change the size of scale text
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.text=element_text(size=20), #legend text size
    axis.title = element_text(size = 20), #axis title size
    axis.text = element_text(size = 18),#, #axis text size
    legend.key.size =  unit(0.5, "in")#, #axis text size
    
  )

p2 <- ggplot(sim_data[which(sim_data$Gender==0),]) + 
  geom_density(aes(x = Score, linetype = Group), 
               alpha = 0.1,show.legend=FALSE) +#show.legend=FALSE means elimate the square outside the legned
  stat_density(aes(x = Score, linetype = Group),
               geom="line",position="identity")+
  scale_linetype_manual(values=c("solid", "dotted"))+
  ggtitle("Female") + #add title
  theme(plot.title = element_text(hjust = 0.5), #change the position of title
    legend.position="none", #remove legend
    #     axis.title.x=element_blank(), axis.title.y=element_blank(), #remove the title of x and y axis
    #    axis.text.x = element_text(size=7))#change the size of scale text
    legend.text=element_text(size=20), #legend text size
    axis.title = element_text(size = 20), #axis title size
    axis.text = element_text(size = 18),#, #axis text size
    legend.key.size =  unit(0.5, "in")#, #axis text size
  )


p3 <- ggplot(sim_data[which(sim_data$Gender==1),]) + 
  geom_density(aes(x = Score, linetype = Group), 
               alpha = 0.1,show.legend=FALSE) +#show.legend=FALSE means elimate the square outside the legned
  stat_density(aes(x = Score, linetype = Group),
               geom="line",position="identity") +
  scale_linetype_manual(values=c("solid", "dotted"))+
  ggtitle("Male") + #add title
  theme(plot.title = element_text(hjust = 0.5), #change the position of title
    legend.position="none", #remove legend
    #     axis.title.x=element_blank(), axis.title.y=element_blank(), #remove the title of x and y axis
    #    axis.text.x = element_text(size=7))#change the size of scale text
    legend.text=element_text(size=20), #legend text size
    axis.title = element_text(size = 20), #axis title size
    axis.text = element_text(size = 18),#, #axis text size
    legend.key.size =  unit(0.5, "in")#, #axis text size
  )


test <-data.frame(D = sim_data$D, Pooled = sim_data$Score, Female = sim_data$Sf, Male = sim_data$Sm)
longtest <- melt_roc(test, "D", c("Pooled", "Female","Male"))
longtest$name <- factor(longtest$name, levels = c("Pooled", "Female","Male")) 

p4 <- ggplot(longtest,aes(d = D, m = M,linetype = name)) + geom_roc(n.cuts = 0) +
  labs(y = "True Positive Rate", x="False Positive Rate") +  #change the title of axis
  theme(legend.title = element_blank(),  #remove legend title
        legend.position = c(0.8, 0.3),    legend.text=element_text(size=20), #legend text size
        axis.title = element_text(size = 20), #axis title size
        axis.text = element_text(size = 18),#, #axis text size
        legend.key.size =  unit(0.5, "in")#, #axis text size
  )





windows()
grid.arrange(arrangeGrob(p1, p2, p3, nrow = 3), # Second row with 2 plots in 2 different columns
             p4,
             ncol = 2)   

###quality
sim_data2 <- data.frame(Score=face_sample$simValues, Group=face_sample$Dis, D=abs(as.numeric(face_sample$D)-2), Gender=face_sample$G1, YOB=face_sample$B1,
                       Good=face_sample$GD, Bad = face_sample$BD, Quality = face_sample$Q)
sim_data2$Sg <- 0
sim_data2$Sb <- 0
sim_data2$Su <- 0

sim_data2[which(sim_data2$Q=='Good'),]$Sg <- sim_data2[which(sim_data2$Q=='Good'),]$Score
sim_data2[which(sim_data2$Q=='Bad'),]$Sb <- sim_data2[which(sim_data2$Q=='Bad'),]$Score
sim_data2[which(sim_data2$Q=='Ugly'),]$Su <- sim_data2[which(sim_data2$Q=='Ugly'),]$Score

sim_data2[which(sim_data2$Sg==0),]$Sg <- NA
sim_data2[which(sim_data2$Sb==0),]$Sb <- NA
sim_data2[which(sim_data2$Su==0),]$Su <- NA





windows(w=5,h=5)
ggplot(sim_data,aes(color = Group)) + 
  geom_density(aes(x = Score, linetype = Group), 
               alpha = 0.1,show.legend=FALSE) +#show.legend=FALSE means elimate the square outside the legned
  stat_density(aes(x = Score, linetype = Group),
               geom="line",position="identity") +
  ggtitle("Pooled") +
  scale_color_manual(values=c("black","black")) +
  scale_linetype_manual(values=c("solid", "dotted"))+
  theme(plot.title = element_text(hjust = 0.5,size=22), #change the position and size of title
    #legend.position="none"#, #remove legend
    #     axis.title.x=element_blank(), axis.title.y=element_blank(), #remove the title of x and y axis
    #    axis.text.x = element_text(size=7))#change the size of scale text
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.text=element_text(size=20), #legend text size
    axis.title = element_text(size = 20), #axis title size
    axis.text = element_text(size = 18),#, #axis text size
    legend.key.size =  unit(0.5, "in")#, #axis text size
  )

windows(w=5,h=5)
ggplot(sim_data2[which(sim_data2$Q=="Good"),],aes(color = Group)) + 
  geom_density(aes(x = Score, linetype = Group), 
               alpha = 0.1,show.legend=FALSE) +#show.legend=FALSE means elimate the square outside the legned
  stat_density(aes(x = Score, linetype = Group),
               geom="line",position="identity")+
  scale_linetype_manual(values=c("solid", "dotted"))+
  ggtitle("Good") + #add title
  scale_color_manual(values=c("blue","blue")) +
  theme(plot.title = element_text(hjust = 0.5,size=22), #change the position and size of title
        legend.position="none",
        #legend.text=element_text(size=18), #legend text size
        #legend.title = element_blank(),
        axis.title = element_text(size = 20), #axis title size
        axis.text = element_text(size = 18)#axis text size
        #legend.key.size =  unit(0.5, "in") #legend key size 
  )


windows(w=5,h=5)
ggplot(sim_data2[which(sim_data2$Q=="Bad"),],aes(color = Group)) + 
  geom_density(aes(x = Score, linetype = Group), 
               alpha = 0.1,show.legend=FALSE) +#show.legend=FALSE means elimate the square outside the legned
  stat_density(aes(x = Score, linetype = Group),
               geom="line",position="identity") +
  scale_linetype_manual(values=c("solid", "dotted"))+
  ggtitle("Bad") + #add title
  scale_color_manual(values=c("dark green","dark green")) +
  theme(plot.title = element_text(hjust = 0.5,size=22), #change the position and size of title
        legend.position="none",
        #legend.text=element_text(size=18), #legend text size
        #legend.title = element_blank(),
        axis.title = element_text(size = 20), #axis title size
        axis.text = element_text(size = 18)#axis text size
        #legend.key.size =  unit(0.5, "in") #legend key size 
  )

windows(w=5,h=5)
ggplot(sim_data2[which(sim_data2$Q=="Ugly"),],aes(color = Group)) + 
  geom_density(aes(x = Score, linetype = Group), 
               alpha = 0.1,show.legend=FALSE) +#show.legend=FALSE means elimate the square outside the legned
  stat_density(aes(x = Score, linetype = Group),
               geom="line",position="identity") +
  scale_linetype_manual(values=c("solid", "dotted"))+
  ggtitle("Ugly") + #add title
  scale_color_manual(values=c("red","red")) +
  theme(plot.title = element_text(hjust = 0.5,size=22), #change the position and size of title
        legend.position="none",
        #legend.text=element_text(size=18), #legend text size
        #legend.title = element_blank(),
        axis.title = element_text(size = 20), #axis title size
        axis.text = element_text(size = 18)#axis text size
        #legend.key.size =  unit(0.5, "in") #legend key size 
  )

test <-data.frame(D = sim_data2$D, Pooled = sim_data2$Score, Good = sim_data2$Sg, Bad = sim_data2$Sb, Ugly = sim_data2$Su)
longtest <- melt_roc(test, "D", c("Pooled", "Good","Bad", 'Ugly'))
longtest$name <- factor(longtest$name, levels = c("Pooled", "Good","Bad", 'Ugly')) 

windows(w=5,h=5)
ggplot(longtest,aes(d = D, m = M,linetype = name, color = name)) + geom_roc(n.cuts = 0) +
  labs(y = "True Positive Rate", x="False Positive Rate") +  #change the title of axis
  scale_linetype_manual(values=c("solid","longdash", "dotted", 'twodash'))+
  scale_color_manual(values=c("black","blue", "dark green", 'red')) +
  theme(legend.title = element_blank(),  #remove legend title
        legend.position = c(0.7, 0.3),
        legend.text=element_text(size=20), #legend text size
        axis.title = element_text(size = 20), #axis title size
        axis.text = element_text(size = 18),#, #axis text size
        legend.key.size =  unit(0.5, "in"))#, #axis text size
#        panel.grid.major = element_blank(), 
#        panel.grid.minor = element_blank(), #remove grid
#        panel.background = element_blank(), #remove color
#        axis.line = element_line(colour = "black"))#add x and y axis


windows()
grid.arrange(arrangeGrob(p1, p5, p6, p7, nrow = 4), # Second row with 2 plots in 2 different columns
             p8,
             ncol = 2)   


###combine plot
p5 <- ggplot(sim_data2[which(sim_data2$Q=="Good"),]) + 
  geom_density(aes(x = Score, linetype = Group), 
               alpha = 0.1,show.legend=FALSE) +#show.legend=FALSE means elimate the square outside the legned
  stat_density(aes(x = Score, linetype = Group),
               geom="line",position="identity")+
  scale_linetype_manual(values=c("solid", "dotted"))+
  ggtitle("Good") + #add title
  theme(plot.title = element_text(hjust = 0.5), #change the position of title
    legend.position="none"#, #remove legend
    #     axis.title.x=element_blank(), axis.title.y=element_blank(), #remove the title of x and y axis
    #    axis.text.x = element_text(size=7))#change the size of scale text
  )


p6 <- ggplot(sim_data2[which(sim_data2$Q=="Bad"),]) + 
  geom_density(aes(x = Score, linetype = Group), 
               alpha = 0.1,show.legend=FALSE) +#show.legend=FALSE means elimate the square outside the legned
  stat_density(aes(x = Score, linetype = Group),
               geom="line",position="identity") +
  scale_linetype_manual(values=c("solid", "dotted"))+
  ggtitle("Bad") + #add title
  theme(plot.title = element_text(hjust = 0.5), #change the position of title
    legend.position="none"#, #remove legend
    #     axis.title.x=element_blank(), axis.title.y=element_blank(), #remove the title of x and y axis
    #    axis.text.x = element_text(size=7))#change the size of scale text
  )

p7 <- ggplot(sim_data2[which(sim_data2$Q=="Ugly"),]) + 
  geom_density(aes(x = Score, linetype = Group), 
               alpha = 0.1,show.legend=FALSE) +#show.legend=FALSE means elimate the square outside the legned
  stat_density(aes(x = Score, linetype = Group),
               geom="line",position="identity") +
  scale_linetype_manual(values=c("solid", "dotted"))+
  ggtitle("Ugly") + #add title
  theme(plot.title = element_text(hjust = 0.5), #change the position of title
    legend.position="none"#, #remove legend
    #     axis.title.x=element_blank(), axis.title.y=element_blank(), #remove the title of x and y axis
    #    axis.text.x = element_text(size=7))#change the size of scale text
  )

test <-data.frame(D = sim_data2$D, Pooled = sim_data2$Score, Good = sim_data2$Sg, Bad = sim_data2$Sb, Ugly = sim_data2$Su)
longtest <- melt_roc(test, "D", c("Pooled", "Good","Bad", 'Ugly'))
longtest$name <- factor(longtest$name, levels = c("Pooled", "Good","Bad", 'Ugly')) 

p8 <- ggplot(longtest,aes(d = D, m = M,linetype = name)) + geom_roc(n.cuts = 0) +
  labs(y = "True Positive Rate", x="False Positive Rate") +  #change the title of axis
  scale_linetype_manual(values=c("solid","longdash", "dotted", 'twodash'))+
  theme(legend.title = element_blank(),  #remove legend title
        legend.position = c(0.8, 0.3))


windows()
grid.arrange(arrangeGrob(p1, p5, p6, p7, nrow = 4), # Second row with 2 plots in 2 different columns
             p8,
             ncol = 2)   

###yob
sim_data3 <- data.frame(Score=face_sample$simValues, Group=face_sample$Dis, D=abs(as.numeric(face_sample$D)-2), Gender=face_sample$G1, Year=face_sample$B1,
                        Good=face_sample$GD, Bad = face_sample$BD, Quality = face_sample$Q)


sim_data3$YOB <- NULL
for (i in 1:NROW(sim_data3)) {
  if (sim_data3$Year[i]<=1979) {
    sim_data3$YOB[i] = '1947-1979'
  } else if (sim_data3$Year[i]<=1983) {
    sim_data3$YOB[i] = '1980-1983' 
  } else {
    sim_data3$YOB[i] = '1984-1987'
  }
}


sim_data3$S7 <- 0
sim_data3$S0 <- 0
sim_data3$S4 <- 0

sim_data3[which(sim_data3$YOB=='1947-1979'),]$S7 <- sim_data3[which(sim_data3$YOB=='1947-1979'),]$Score
sim_data3[which(sim_data3$YOB=='1980-1983'),]$S0 <- sim_data3[which(sim_data3$YOB=='1980-1983'),]$Score
sim_data3[which(sim_data3$YOB=='1984-1987'),]$S4 <- sim_data3[which(sim_data3$YOB=='1984-1987'),]$Score

sim_data3[which(sim_data3$S7==0),]$S7 <- NA
sim_data3[which(sim_data3$S0==0),]$S0 <- NA
sim_data3[which(sim_data3$S4==0),]$S4 <- NA




windows(w=5,h=3)
ggplot(sim_data3[which(sim_data3$YOB=='1947-1979'),]) + 
  geom_density(aes(x = Score, linetype = Group), 
               alpha = 0.1,show.legend=FALSE) +#show.legend=FALSE means elimate the square outside the legned
  stat_density(aes(x = Score, linetype = Group),
               geom="line",position="identity")+
  scale_linetype_manual(values=c("solid", "dotted"))+
  theme(#plot.title = element_text(hjust = 0.5), #change the position of title
    legend.position="none"#, #remove legend
    #     axis.title.x=element_blank(), axis.title.y=element_blank(), #remove the title of x and y axis
    #    axis.text.x = element_text(size=7))#change the size of scale text
  )


windows(w=5,h=3)
ggplot(sim_data3[which(sim_data3$YOB=='1980-1983'),]) + 
  geom_density(aes(x = Score, linetype = Group), 
               alpha = 0.1,show.legend=FALSE) +#show.legend=FALSE means elimate the square outside the legned
  stat_density(aes(x = Score, linetype = Group),
               geom="line",position="identity") +
  scale_linetype_manual(values=c("solid", "dotted"))+
  theme(#plot.title = element_text(hjust = 0.5), #change the position of title
    legend.position="none"#, #remove legend
    #     axis.title.x=element_blank(), axis.title.y=element_blank(), #remove the title of x and y axis
    #    axis.text.x = element_text(size=7))#change the size of scale text
  )

windows(w=5,h=3)
ggplot(sim_data3[which(sim_data3$YOB=='1984-1987'),]) + 
  geom_density(aes(x = Score, linetype = Group), 
               alpha = 0.1,show.legend=FALSE) +#show.legend=FALSE means elimate the square outside the legned
  stat_density(aes(x = Score, linetype = Group),
               geom="line",position="identity") +
  scale_linetype_manual(values=c("solid", "dotted"))+
  theme(#plot.title = element_text(hjust = 0.5), #change the position of title
    legend.position="none"#, #remove legend
    #     axis.title.x=element_blank(), axis.title.y=element_blank(), #remove the title of x and y axis
    #    axis.text.x = element_text(size=7))#change the size of scale text
  )

test <-data.frame(D = sim_data3$D, Pooled = sim_data3$Score, YOB1947to1979 = sim_data3$S7, YOB1980to1983 = sim_data3$S0, YOB1984to1987 = sim_data3$S4)
longtest <- melt_roc(test, "D", c("Pooled", "YOB1947to1979","YOB1980to1983", 'YOB1984to1987'))
longtest$name <- factor(longtest$name, levels = c("Pooled", "YOB1947to1979","YOB1980to1983", 'YOB1984to1987')) 

windows(w=5,h=4.5)
ggplot(longtest,aes(d = D, m = M,linetype = name)) + geom_roc(n.cuts = 0) +
  labs(y = "True Positive Rate", x="False Positive Rate") +  #change the title of axis
  scale_linetype_manual(values=c("solid","longdash", "dotted", 'twodash'), 
                        labels=c("Pooled", "1947-1979","1980-1983", '1984-1987'))+
  theme(legend.title = element_blank(),  #remove legend title
        legend.position = c(0.7, 0.3),
        legend.text=element_text(size=20), #legend text size
        axis.title = element_text(size = 20), #axis title size
        axis.text = element_text(size = 18),#axis text size
        legend.key.size =  unit(0.5, "in"))#, #axis text size
#        panel.grid.major = element_blank(), 
#        panel.grid.minor = element_blank(), #remove grid
#        panel.background = element_blank(), #remove color
#        axis.line = element_line(colour = "black"))#add x and y axis

###combine plot
p9 <- ggplot(sim_data3[which(sim_data3$YOB=='1947-1979'),]) + 
  geom_density(aes(x = Score, linetype = Group), 
               alpha = 0.1,show.legend=FALSE) +#show.legend=FALSE means elimate the square outside the legned
  stat_density(aes(x = Score, linetype = Group),
               geom="line",position="identity")+
  scale_linetype_manual(values=c("solid", "dotted"))+
  ggtitle("1947-1979") + #add title
  theme(plot.title = element_text(hjust = 0.5), #change the position of title
    legend.position="none"#, #remove legend
    #     axis.title.x=element_blank(), axis.title.y=element_blank(), #remove the title of x and y axis
    #    axis.text.x = element_text(size=7))#change the size of scale text
  )


p10 <- ggplot(sim_data3[which(sim_data3$YOB=='1980-1983'),]) + 
  geom_density(aes(x = Score, linetype = Group), 
               alpha = 0.1,show.legend=FALSE) +#show.legend=FALSE means elimate the square outside the legned
  stat_density(aes(x = Score, linetype = Group),
               geom="line",position="identity") +
  scale_linetype_manual(values=c("solid", "dotted"))+
  ggtitle("1980-1983") + #add title
  theme(plot.title = element_text(hjust = 0.5), #change the position of title
    legend.position="none"#, #remove legend
    #     axis.title.x=element_blank(), axis.title.y=element_blank(), #remove the title of x and y axis
    #    axis.text.x = element_text(size=7))#change the size of scale text
  )

p11 <- ggplot(sim_data3[which(sim_data3$YOB=='1984-1987'),]) + 
  geom_density(aes(x = Score, linetype = Group), 
               alpha = 0.1,show.legend=FALSE) +#show.legend=FALSE means elimate the square outside the legned
  stat_density(aes(x = Score, linetype = Group),
               geom="line",position="identity") +
  scale_linetype_manual(values=c("solid", "dotted"))+
  ggtitle("1984-1987") + #add title
  theme(plot.title = element_text(hjust = 0.5), #change the position of title
    legend.position="none"#, #remove legend
    #     axis.title.x=element_blank(), axis.title.y=element_blank(), #remove the title of x and y axis
    #    axis.text.x = element_text(size=7))#change the size of scale text
  )


test <-data.frame(D = sim_data3$D, Pooled = sim_data3$Score, YOB1947to1979 = sim_data3$S7, YOB1980to1983 = sim_data3$S0, YOB1984to1987 = sim_data3$S4)
longtest <- melt_roc(test, "D", c("Pooled", "YOB1947to1979","YOB1980to1983", 'YOB1984to1987'))
longtest$name <- factor(longtest$name, levels = c("Pooled", "YOB1947to1979","YOB1980to1983", 'YOB1984to1987')) 

p12 <- ggplot(longtest,aes(d = D, m = M,linetype = name)) + geom_roc(n.cuts = 0) +
  labs(y = "True Positive Rate", x="False Positive Rate") +  #change the title of axis
  scale_linetype_manual(values=c("solid","longdash", "dotted", 'twodash'), 
                        labels=c("Pooled", "1947-1979","1980-1983", '1984-1987'))+
  theme(legend.title = element_blank(),  #remove legend title
        legend.position = c(0.8, 0.3))


windows()
grid.arrange(arrangeGrob(p1, p9, p10, p11, nrow = 4), # Second row with 2 plots in 2 different columns
             p12,
             ncol = 2)   








X <- ceiling(runif(5000,0,3))
D <- round(runif(5000,0,1))
e <- rnorm(5000,0,1)
Y <- 2+2*X+2*D+e 
#Y <- 2+2*X+1*D+e +X*D*1
toy <- data.frame(Score = Y, D = D, Covariate = X)
toy$Group <- 0
toy[which(toy$D==0),]$Group = 'imposter'
toy[which(toy$D==1),]$Group = 'genuine'

windows()
ggplot(toy) + 
  geom_density(aes(x = Score, fill = Group, color = Group), 
               alpha = 0.5,show.legend=FALSE) +#show.legend=FALSE means elimate the square outside the legned
  stat_density(aes(x = Score, linetype = Group),
               geom="line",position="identity")+
  scale_linetype_manual(values=c("solid", "dotted"))+
  theme(plot.title = element_text(hjust = 0.5), #change the position of title
        legend.position="none"#, #remove legend
        #     axis.title.x=element_blank(), axis.title.y=element_blank(), #remove the title of x and y axis
        #    axis.text.x = element_text(size=7))#change the size of scale text
  )
###combine plot
p0 <- ggplot(toy) + 
  geom_density(aes(x = Score, linetype = Group, color = Group), 
               alpha = 0.5,show.legend=FALSE) +#show.legend=FALSE means elimate the square outside the legned
  stat_density(aes(x = Score, linetype = Group),
               geom="line",position="identity")+
  scale_linetype_manual(values=c("solid", "dotted"))+
  theme(plot.title = element_text(hjust = 0.5), #change the position of title
        #legend.position="none"#, #remove legend
        axis.title.x=element_blank(), #axis.title.y=element_blank(), #remove the title of x and y axis
        #    axis.text.x = element_text(size=7))#change the size of scale text
        legend.title = element_blank(),
        legend.position = "bottom"
  )

p1 <- ggplot(toy[which(toy$Covariate==1),]) + 
  geom_density(aes(x = Score, linetype = Group, color = Group), 
               alpha = 0.5,show.legend=FALSE) +#show.legend=FALSE means elimate the square outside the legned
  stat_density(aes(x = Score, linetype = Group),
               geom="line",position="identity")+
  scale_linetype_manual(values=c("solid", "dotted"))+
  ggtitle("Covariate X=1") + #add title
  xlim(0,13)+
  theme(plot.title = element_text(hjust = 0.5), #change the position of title
        legend.position="none"#, #remove legend
        #     axis.title.x=element_blank(), axis.title.y=element_blank(), #remove the title of x and y axis
        #    axis.text.x = element_text(size=7))#change the size of scale text
  )


p2 <- ggplot(toy[which(toy$Covariate==2),]) + 
  geom_density(aes(x = Score, linetype = Group, color = Group), 
               alpha = 0.5,show.legend=FALSE) +#show.legend=FALSE means elimate the square outside the legned
  stat_density(aes(x = Score, linetype = Group),
               geom="line",position="identity")+
  scale_linetype_manual(values=c("solid", "dotted"))+
  xlim(0,13)+
  ggtitle("Covariate X=2") + #add title
  theme(plot.title = element_text(hjust = 0.5), #change the position of title
        legend.position="none"#, #remove legend
        #     axis.title.x=element_blank(), axis.title.y=element_blank(), #remove the title of x and y axis
        #    axis.text.x = element_text(size=7))#change the size of scale text
  )

p3 <- ggplot(toy[which(toy$Covariate==3),]) + 
  geom_density(aes(x = Score, linetype = Group, color = Group), 
               alpha = 0.5,show.legend=FALSE) +#show.legend=FALSE means elimate the square outside the legned
  stat_density(aes(x = Score, linetype = Group),
               geom="line",position="identity")+
  scale_linetype_manual(values=c("solid", "dotted"))+
  xlim(0,13)+
  ggtitle("Covariate X=3") + #add title
  theme(plot.title = element_text(hjust = 0.5), #change the position of title
        legend.position="none"#, #remove legend
        #     axis.title.x=element_blank(), axis.title.y=element_blank(), #remove the title of x and y axis
        #    axis.text.x = element_text(size=7))#change the size of scale text
  )

toy$C1 <- 0
toy$C2 <- 0
toy$C3 <- 0

toy[which(toy$Covariate==1),]$C1 <- toy[which(toy$Covariate==1),]$Score
toy[which(toy$Covariate==2),]$C2 <- toy[which(toy$Covariate==2),]$Score
toy[which(toy$Covariate==3),]$C3 <- toy[which(toy$Covariate==3),]$Score

toy[which(toy$C1==0),]$C1 <- NA
toy[which(toy$C2==0),]$C2 <- NA
toy[which(toy$C3==0),]$C3 <- NA


test <-data.frame(D = toy$D, Pooled = toy$Score, Covariate1 = toy$C1, Covariate2 = toy$C2, Covariate3 = toy$C3)
longtest <- melt_roc(test, "D", c("Pooled", "Covariate1","Covariate2", 'Covariate3'))
longtest$name <- factor(longtest$name, levels = c("Pooled", "Covariate1","Covariate2", 'Covariate3')) 

p4 <- ggplot(longtest,aes(d = D, m = M,linetype = name, color = name)) + geom_roc(n.cuts = 0) +
  labs(y = "True Positive Rate", x="False Positive Rate") +  #change the title of axis
  scale_linetype_manual(values=c("solid","longdash", "dotted", 'twodash'), 
                        labels=c("Pooled", "Covariate X=1","Covariate X=2", 'Covariate X=3'))+
  scale_color_manual(values=c("red","blue", "green", 'black'), 
                      labels=c("Pooled", "Covariate X=1","Covariate X=2", 'Covariate X=3')) +
  theme(legend.title = element_blank(),  #remove legend title
        legend.position = c(0.8, 0.3))


windows()
grid.arrange(arrangeGrob(p0, p1, p2, p3, nrow = 4), # Second row with 2 plots in 2 different columns
             p4,
             ncol = 2)   





#X in 2 dimension
X <- ceiling(runif(5000,0.7,2))
D <- round(runif(5000,0,1))
e <- rnorm(5000,0,1)
#Y <- 2+2*X+2*D+e 
Y <- 2+2*X+1*D+e +X*D*1
toy <- data.frame(Score = Y, D = D, Covariate = X)
toy$Group <- 0
toy[which(toy$D==0),]$Group = 'imposter'
toy[which(toy$D==1),]$Group = 'genuine'

###combine plot
windows(w=5,h=5)  
ggplot(toy) + 
  geom_density(aes(x = Score, linetype = Group, color = Group), 
               alpha = 0.5,show.legend=FALSE) +#show.legend=FALSE means elimate the square outside the legned
  stat_density(aes(x = Score, linetype = Group),
               geom="line",position="identity")+
  ggtitle("Pooled") + #add title
  scale_linetype_manual(values=c("solid", "dotted"))+
  geom_vline(xintercept = 7, color = "red")+
 # xlim(0,13)+
  theme(plot.title = element_text(hjust = 0.5,size=22), #change the position of title
        #legend.position="none"#, #remove legend
        #axis.title.x=element_blank(), #axis.title.y=element_blank(), #remove the title of x and y axis
        #    axis.text.x = element_text(size=7))#change the size of scale text
        legend.text=element_text(size=20), #legend text size
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.title = element_text(size = 20), #axis title size
        axis.text = element_text(size = 18),#, #axis text size
        legend.key.size =  unit(0.5, "in")) #legend key size 


windows(w=5,h=5)  
ggplot(toy[which(toy$Covariate==1),]) + 
  geom_density(aes(x = Score, linetype = Group, color = Group), 
               alpha = 0.5,show.legend=FALSE) +#show.legend=FALSE means elimate the square outside the legned
  stat_density(aes(x = Score, linetype = Group),
               geom="line",position="identity")+
  scale_linetype_manual(values=c("solid", "dotted"))+
  ggtitle("Covariate X=1") + #add title
  geom_vline(xintercept = 7, color = "red")+
 # xlim(0,13)+
  theme(plot.title = element_text(hjust = 0.5,size=22), #change the position and size of title
        legend.position="none",
        #legend.text=element_text(size=18), #legend text size
        #legend.title = element_blank(),
        axis.title = element_text(size = 20), #axis title size
        axis.text = element_text(size = 18)
        #legend.key.size =  unit(0.5, "in")#, #axis text size
        )


windows(w=5,h=5)  
ggplot(toy[which(toy$Covariate==2),]) + 
  geom_density(aes(x = Score, linetype = Group, color = Group), 
               alpha = 0.5,show.legend=FALSE) +#show.legend=FALSE means elimate the square outside the legned
  stat_density(aes(x = Score, linetype = Group),
               geom="line",position="identity")+
  scale_linetype_manual(values=c("solid", "dotted"))+
  geom_vline(xintercept = 7, color = "red")+
  ggtitle("Covariate X=2") + #add title
 # xlim(0,13)+
  theme(plot.title = element_text(hjust = 0.5,size=22), #change the position and size of title
        legend.position="none",
        #legend.text=element_text(size=18), #legend text size
        #legend.title = element_blank(),
        axis.title = element_text(size = 20), #axis title size
        axis.text = element_text(size = 18)#axis text size
        #legend.key.size =  unit(0.5, "in") #legend key size 
        )


# windows()
# grid.arrange(p1, p2, nrow = 2)# Second row with 2 plots in 2 different columns


toy$C1 <- 0
toy$C2 <- 0

toy[which(toy$Covariate==1),]$C1 <- toy[which(toy$Covariate==1),]$Score
toy[which(toy$Covariate==2),]$C2 <- toy[which(toy$Covariate==2),]$Score

toy[which(toy$C1==0),]$C1 <- NA
toy[which(toy$C2==0),]$C2 <- NA


test <-data.frame(D = toy$D, Pooled = toy$Score, Covariate1 = toy$C1, Covariate2 = toy$C2)
longtest <- melt_roc(test, "D", c("Pooled", "Covariate1","Covariate2"))
longtest$name <- factor(longtest$name, levels = c("Pooled", "Covariate1","Covariate2")) 



# c11 <- ecdf(test[which(test$D==1),]$Covariate1)(7)
# c10 <- ecdf(test[which(test$D==0),]$Covariate1)(7)
# 
# c21 <- ecdf(test[which(test$D==1),]$Covariate2)(7)
# c20 <- ecdf(test[which(test$D==0),]$Covariate2)(7)
# 
# p1 <- ecdf(test[which(test$D==1),]$Pooled)(7)
# p0 <- ecdf(test[which(test$D==0),]$Pooled)(7)
# 
# expoint <- data.frame(x=c(p0,c10,c20), y=c(p1,c11,c21))

windows(w=5,h=5)
ggplot(longtest,aes(d = D, m = M,linetype = name, color = name)) + geom_roc(n.cuts = 0) +
  labs(y = "True Positive Rate", x="False Positive Rate") +  #change the title of axis
  scale_linetype_manual(values=c("solid", "dotted", 'twodash'), 
                        labels=c("Pooled", "Covariate X=1","Covariate X=2"))+
  scale_color_manual(values=c("red","blue", "black"), 
                     labels=c("Pooled", "Covariate X=1","Covariate X=2")) +
   theme(legend.title = element_blank(),  #remove legend title
        legend.position = c(0.7, 0.3),
        legend.text=element_text(size=20), #legend text size
        axis.title = element_text(size = 20), #axis title size
        axis.text = element_text(size = 18),#, #axis text size
        legend.key.size =  unit(0.5, "in")) #legend key size 
#        panel.grid.major = element_blank(), 
#        panel.grid.minor = element_blank(), #remove grid
#        panel.background = element_blank(), #remove color
#        axis.line = element_line(colour = "black"))#add x and y axis
# 



# windows()
# grid.arrange(p0,
#              arrangeGrob(p1, p2, nrow = 2), # Second row with 2 plots in 2 different columns
#              p3,
#              ncol = 3)  #define the ratio of the width for two columns 






#X in 2 dimension
e1 <- rnorm(5000,6,1)
e2 <- rnorm(5000,4,1)
e3 <- rnorm(5000,2,1)
e <- rnorm(5000,0,1)
D <- round(runif(5000,0,1))
#Y <- 2+2*X+2*D+e 
Y1 <- 3+(1-D)* e +D*e1
Y2 <- 3+(1-D)* e +D*e2
Y3 <- 3+(1-D)* e +D*e3
toy1 <- data.frame(Score = Y1, D = D)
toy2 <- data.frame(Score = Y2, D = D)
toy3 <- data.frame(Score = Y3, D = D)
toy1$Group <- 0
toy2$Group <- 0
toy3$Group <- 0
toy1[which(toy1$D==0),]$Group = 'imposter'
toy1[which(toy1$D==1),]$Group = 'genuine'
toy2[which(toy2$D==0),]$Group = 'imposter'
toy2[which(toy2$D==1),]$Group = 'genuine'
toy3[which(toy3$D==0),]$Group = 'imposter'
toy3[which(toy3$D==1),]$Group = 'genuine'

###combine plot
windows(w=5,h=5)  
ggplot(toy1) + 
  geom_density(aes(x = Score, linetype = Group, color = Group), 
               alpha = 0.5,show.legend=FALSE) +#show.legend=FALSE means elimate the square outside the legned
  stat_density(aes(x = Score, linetype = Group),
               geom="line",position="identity")+
  scale_linetype_manual(values=c("solid", "dotted"))+
  xlim(0,12)+
  scale_color_manual(values=c("red","red")) +
  theme(#plot.title = element_text(hjust = 0.5,size=22), #change the position of title
        legend.position="none", #remove legend
        #axis.title.x=element_blank(), #axis.title.y=element_blank(), #remove the title of x and y axis
        #    axis.text.x = element_text(size=7))#change the size of scale text
        legend.text=element_text(size=20), #legend text size
        legend.title = element_blank(),
        #legend.position = "bottom",
        axis.title = element_text(size = 20), #axis title size
        axis.text = element_text(size = 18),#, #axis text size
        legend.key.size =  unit(0.5, "in")) #legend key size 


windows(w=5,h=5)  
ggplot(toy2) + 
  geom_density(aes(x = Score, linetype = Group, color = Group), 
               alpha = 0.5,show.legend=FALSE) +#show.legend=FALSE means elimate the square outside the legned
  stat_density(aes(x = Score, linetype = Group),
               geom="line",position="identity")+
  scale_linetype_manual(values=c("solid", "dotted"))+
  xlim(0,12)+
  scale_color_manual(values=c("blue","blue")) +
  theme(#plot.title = element_text(hjust = 0.5,size=22), #change the position and size of title
        legend.position="none",
        #legend.text=element_text(size=18), #legend text size
        #legend.title = element_blank(),
        axis.title = element_text(size = 20), #axis title size
        axis.text = element_text(size = 18)
        #legend.key.size =  unit(0.5, "in")#, #axis text size
  )


windows(w=5,h=5)  
ggplot(toy3) + 
  geom_density(aes(x = Score, linetype = Group, color = Group), 
               alpha = 0.5,show.legend=FALSE) +#show.legend=FALSE means elimate the square outside the legned
  stat_density(aes(x = Score, linetype = Group),
               geom="line",position="identity")+
  scale_linetype_manual(values=c("solid", "dotted"))+
  xlim(0,12)+
  scale_color_manual(values=c("black","black")) +
  theme(#plot.title = element_text(hjust = 0.5,size=22), #change the position and size of title
        legend.position="none",
        #legend.text=element_text(size=18), #legend text size
        #legend.title = element_blank(),
        axis.title = element_text(size = 20), #axis title size
        axis.text = element_text(size = 18)#axis text size
        #legend.key.size =  unit(0.5, "in") #legend key size 
  )


# windows()
# grid.arrange(p1, p2, nrow = 2)# Second row with 2 plots in 2 different columns


toy1$C1 <- 0
toy1$C2 <- 0

toy[which(toy$Covariate==1),]$C1 <- toy[which(toy$Covariate==1),]$Score
toy[which(toy$Covariate==2),]$C2 <- toy[which(toy$Covariate==2),]$Score

toy[which(toy$C1==0),]$C1 <- NA
toy[which(toy$C2==0),]$C2 <- NA


test <-data.frame(D = toy$D, Pooled = toy$Score, Covariate1 = toy$C1, Covariate2 = toy$C2)
longtest <- melt_roc(test, "D", c("Pooled", "Covariate1","Covariate2"))
longtest$name <- factor(longtest$name, levels = c("Pooled", "Covariate1","Covariate2")) 



# c11 <- ecdf(test[which(test$D==1),]$Covariate1)(7)
# c10 <- ecdf(test[which(test$D==0),]$Covariate1)(7)
# 
# c21 <- ecdf(test[which(test$D==1),]$Covariate2)(7)
# c20 <- ecdf(test[which(test$D==0),]$Covariate2)(7)
# 
# p1 <- ecdf(test[which(test$D==1),]$Pooled)(7)
# p0 <- ecdf(test[which(test$D==0),]$Pooled)(7)
# 
# expoint <- data.frame(x=c(p0,c10,c20), y=c(p1,c11,c21))

windows(w=5,h=5)
ggplot(longtest,aes(d = D, m = M,linetype = name, color = name)) + geom_roc(n.cuts = 0) +
  labs(y = "True Positive Rate", x="False Positive Rate") +  #change the title of axis
  scale_linetype_manual(values=c("solid", "dotted", 'twodash'), 
                        labels=c("Pooled", "Covariate X=1","Covariate X=2"))+
  scale_color_manual(values=c("red","blue", "black"), 
                     labels=c("Pooled", "Covariate X=1","Covariate X=2")) +
  theme(legend.title = element_blank(),  #remove legend title
        legend.position = c(0.7, 0.3),
        legend.text=element_text(size=20), #legend text size
        axis.title = element_text(size = 20), #axis title size
        axis.text = element_text(size = 18),#, #axis text size
        legend.key.size =  unit(0.5, "in")) #legend key size 
#        panel.grid.major = element_blank(), 
#        panel.grid.minor = element_blank(), #remove grid
#        panel.background = element_blank(), #remove color
#        axis.line = element_line(colour = "black"))#add x and y axis
# 



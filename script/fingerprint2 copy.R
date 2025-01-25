#1.fingerprint data####################
SD4_comparisons <- read.delim("/Users/xiaochenzhu/Dropbox/PhD/data/SD4_comparisons.txt", header=FALSE)
#“f” - the first instance 
#“s” - the second instance
#G-genuine I-imposter D-differ
#feature
SD04_FingerFeature <- read.delim("/Users/xiaochenzhu/Dropbox/PhD/data/SD04_FingerFeature.txt", sep=" ", header=T)
#prepare for feature attach
SD04_FingerFeature <- SD04_FingerFeature[sort(SD04_FingerFeature$ID),]
finger_sample <- SD4_comparisons[-which(SD4_comparisons$V3=='D'),] #Drop all the data with category D
finger_sample$D <- 0
finger_sample[which(finger_sample$V3=='G'),]$D <- 1
# head(finger_sample)
# 1 f0001_01 f0001_01  G 642 1
# 2 f0001_01 f0002_05  I   6 0
# 3 f0001_01 f0003_10  I   9 0
# 4 f0001_01 f0004_05  I   7 0
# 5 f0001_01 f0005_03  I   4 0
# 6 f0001_01 f0006_09  I   4 0


# head(SD04_FingerFeature)
# ID Gender Class
# 1 f0001_01      M     W
# 2 f0002_05      M     R
# 3 f0003_10      F     L
# 4 f0004_05      M     R
# 5 f0005_03      M     A
# 6 f0006_09      M     A
factor(SD04_FingerFeature$Class)
# Levels: A L R T W
#attached feature
finger_sample$rows1 <- match(finger_sample$V1,SD04_FingerFeature$ID)
finger_sample$Gender1 <- SD04_FingerFeature$Gender[finger_sample$rows1]
finger_sample$Class1 <- SD04_FingerFeature$Class[finger_sample$rows1]

finger_sample$rows2 <- match(finger_sample$V2,SD04_FingerFeature$ID)
finger_sample$Gender2 <- SD04_FingerFeature$Gender[finger_sample$rows2]
finger_sample$Class2 <- SD04_FingerFeature$Class[finger_sample$rows2]

#Gender: male - 1 female - 0
finger_sample$G1 <- 0
finger_sample[which(finger_sample$Gender1 =='M'),]$G1 <- 1
finger_sample$G2 <- 0
finger_sample[which(finger_sample$Gender2 =='M'),]$G2 <- 1

#Class
finger_sample$A1 <- 0
finger_sample[which(finger_sample$Class1 =='A'),]$A1 <- 1
finger_sample$A2 <- 0
finger_sample[which(finger_sample$Class2 =='A'),]$A2 <- 1
finger_sample$L1 <- 0
finger_sample[which(finger_sample$Class1 =='L'),]$L1 <- 1
finger_sample$L2 <- 0
finger_sample[which(finger_sample$Class2 =='L'),]$L2 <- 1
finger_sample$R1 <- 0
finger_sample[which(finger_sample$Class1 =='R'),]$R1 <- 1
finger_sample$R2 <- 0
finger_sample[which(finger_sample$Class2 =='R'),]$R2 <- 1
finger_sample$W1 <- 0
finger_sample[which(finger_sample$Class1 =='W'),]$W1 <- 1
finger_sample$W2 <- 0
finger_sample[which(finger_sample$Class2 =='W'),]$W2 <- 1

# head(finger_sample)
# V1       V2 V3  V4 D rows1 Gender1 Class1 rows2 Gender2 Class2 G1 G2 A1 A2 L1 L2 R1 R2 W1 W2
# 1 f0001_01 f0001_01  G 642 1     1       M      W     1       M      W  1  1  0  0  0  0  0  0  1  1
# 2 f0001_01 f0002_05  I   6 0     1       M      W     2       M      R  1  1  0  0  0  0  0  1  1  0
# 3 f0001_01 f0003_10  I   9 0     1       M      W     3       F      L  1  0  0  0  0  1  0  0  1  0
# 4 f0001_01 f0004_05  I   7 0     1       M      W     4       M      R  1  1  0  0  0  0  0  1  1  0
# 5 f0001_01 f0005_03  I   4 0     1       M      W     5       M      A  1  1  0  1  0  0  0  0  1  0
# 6 f0001_01 f0006_09  I   4 0     1       M      W     6       M      A  1  1  0  1  0  0  0  0  1  0

finger_new <- data.frame(ID1=finger_sample$V1, ID2=finger_sample$V2, score=finger_sample$V4, d=finger_sample$D, 
                         gender1=finger_sample$G1, gender2=finger_sample$G2, arch1=finger_sample$A1, arch2=finger_sample$A2, 
                         left_loop1=finger_sample$L1, left_loop2 = finger_sample$L2, right_loop1=finger_sample$R1, 
                         right_loop2 = finger_sample$R2, whorl1=finger_sample$W1, whorl2 = finger_sample$W2,
                         class1=finger_sample$Class1, class2 = finger_sample$Class2)
# head(finger_new)
# ID1      ID2 score d gender1 gender2 arch1 arch2 left_loop1 left_loop2 right_loop1 right_loop2 whorl1
# 1 f0001_01 f0001_01   642 1       1       1     0     0          0          0           0           0      1
# 2 f0001_01 f0002_05     6 0       1       1     0     0          0          0           0           1      1
# 3 f0001_01 f0003_10     9 0       1       0     0     0          0          1           0           0      1
# 4 f0001_01 f0004_05     7 0       1       1     0     0          0          0           0           1      1
# 5 f0001_01 f0005_03     4 0       1       1     0     1          0          0           0           0      1
# 6 f0001_01 f0006_09     4 0       1       1     0     1          0          0           0           0      1

# summary(finger_new)
# score                d                gender1          gender2           arch1            arch2       
# Min.   :   0.000   Min.   :0.0000000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
# 1st Qu.:   6.000   1st Qu.:0.0000000   1st Qu.:1.0000   1st Qu.:1.0000   1st Qu.:0.0000   1st Qu.:0.0000  
# Median :   7.000   Median :0.0000000   Median :1.0000   Median :1.0000   Median :0.0000   Median :0.0000  
# Mean   :   7.651   Mean   :0.0005556   Mean   :0.8125   Mean   :0.8125   Mean   :0.1999   Mean   :0.1999  
# 3rd Qu.:   9.000   3rd Qu.:0.0000000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:0.0000   3rd Qu.:0.0000  
# Max.   :1487.000   Max.   :1.0000000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
# left_loop1       left_loop2      right_loop1      right_loop2         whorl1        whorl2   
# Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0   Min.   :0.0  
# 1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0   1st Qu.:0.0  
# Median :0.0000   Median :0.0000   Median :0.0000   Median :0.0000   Median :0.0   Median :0.0  
# Mean   :0.2004   Mean   :0.2004   Mean   :0.2008   Mean   :0.2008   Mean   :0.2   Mean   :0.2  
# 3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.0   3rd Qu.:0.0  
# Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0   Max.   :1.0  

#write.csv(finger_new, "/Users/xiaochenzhu/Dropbox/PhD/data/fingerprint_all.csv")

#2.saperate data into multiple groups to remove corr#############
finger_new <- read.csv("/Users/xiaochenzhu/Dropbox/PhD/data/fingerprint_all.csv") #load data set

#match the categoary
finger_i <- finger_new[which(finger_new$d=='0'),] #assign the match and nonmatch into two groups
finger_g <- finger_new[which(finger_new$d=='1'),]
# Levels: A L R T W
finger_i1 <- finger_i[which(finger_i$class1=="T" & finger_i$class2=="T"),]


#old method
set.seed(2019)
AllID0 <- SD04_FingerFeature$ID #AllID is the vector of IDs
AllID <- substring(AllID0, 2)
hf <- round(length(AllID)/3) #find the smallest integer that half the ID
ind1 <- sample(1:length(AllID),hf) #random select half of the ID
group1 <- AllID[ind1] #split all IDs into three groups
group1.5 <- AllID[-ind1]
ind2 <- sample(1:length(group1.5),hf)
group2 <- group1.5[ind2] #sec group
group3 <- group1.5[-ind2] #third group

#finger_i1 <- finger_new[which(finger_new$d=='0'),] #assign the match and nonmatch into two groups
#finger_g <- finger_new[which(finger_new$d=='1'),]

#remove all duplicate for genuines note that genuine only chack one subject since it comes from a comparison with same person
finger_indg1 <- which(!is.na(match(substring(finger_g$ID1,2), group1)))
finger_g1 <- finger_g[finger_indg1,]
finger_g3 <- finger_g1[order(finger_g1$score, decreasing = FALSE),]
finger_g6 <- finger_g3[!duplicated(substring(finger_g3[,1],2)),]

#remove all duplicate for imposters
finger_ind1 <- which(!is.na(match(substring(finger_i1$ID1,2), group2)))
finger_i2 <- finger_i1[finger_ind1,]
finger_ind2 <- which(!is.na(match(substring(finger_i2$ID2,2), group3)))
finger_i3 <- finger_i2[finger_ind2,]
finger_i4 <- finger_i3[order(finger_i3$score, decreasing = TRUE),]
finger_i5 <- finger_i4[!duplicated(substring(finger_i4[,1],2)),]
finger_i6 <- finger_i5[!duplicated(substring(finger_i5[,2],2)),]

#match the sample size
ss <- min(NROW(finger_i6),NROW(finger_g6))
matchss1 <- sample(1:NROW(finger_i6),ss)
matchss2 <- sample(1:NROW(finger_g6),ss)
finger_i7 <- finger_i6[matchss1,]
finger_g7 <- finger_g6[matchss2,]

# NROW(finger_i7)
# [1] 115 - A
# [1] 114 - L
# [1] 105 - R
# [1] 102 - T
# [1] 101 - W
# NROW(finger_g7)
# [1] 115 - A
# [1] 114 - L
# [1] 105 - R
# [1] 102 - T
# [1] 101 - W

#further assign it to a data.frame
sim_data_g <- data.frame(Y=finger_g7$score, D=finger_g7$d, gender1=finger_g7$gender1, gender2=finger_g7$gender2) 
sim_data_i <- data.frame(Y=finger_i7$score, D=finger_i7$d, gender1=finger_i7$gender1, gender2=finger_i7$gender2)

sim_data <- rbind(sim_data_g,sim_data_i) #combine genuine and imposter to one dataset

################################################################
####################VI. Figures and Tables######################
################################################################
#1.mean and SD for mean differences and ROCs####################
x_hat <- c(1,0) #three ages
p_hat <- c(0.1,0.3,0.5,0.7,0.9) #five fpr for ROC study
#p_hat <- seq(0.001,0.999,0.001) #for next section, run this line instead of previous line
coef1 <- data.frame(int = c(), D = c(), B = c(), DB = c()) #creat matrix to storage results
coef2 <- data.frame(int = c(), D = c(), B = c(), DB = c()) #creat matrix to storage results
n <- NROW(sim_data_g) #sample size n (m+n=N)
N_RESAMPLING <- 1000 #number of iterations
roc1 <- matrix(nrow=N_RESAMPLING, ncol=length(x_hat)*length(p_hat)) #creat matrix to storage results
roc2 <- matrix(nrow=N_RESAMPLING, ncol=length(x_hat)*length(p_hat)) #creat matrix to storage results

for(i in 1:N_RESAMPLING) {
  set.seed(1988+i)
  sub = sample(1:n, n, replace=TRUE) #do resampling for bootstrap
  re_data_g=sim_data_g[sub,]
  re_data_i=sim_data_i[sub,]
  re_data <- rbind(re_data_g,re_data_i)
  
  coef <- lm(Y~D+gender1+gender2+gender1:D, data=re_data)
  for (j in 1:length(coef))
    #coefficient values for the linear regression
    coef1[i,j] <- coef[j]
  #obtain roc
  for(j in 1:length(x_hat)){
    for (k in 1:length(p_hat)){
      roc1[i,(j-1)*length(p_hat)+k] <- roc_fn(x1=x_hat[j],x2=x_hat[j], p=p_hat[k], X1=re_data$gender1,
                                              X2=re_data$gender2, D=re_data$D, Y=re_data$Y, beta_11=coef1[i,2],
                                              beta_12=coef1[i,5],beta_21=coef1[i,3],beta_22=0) 
    }
  }
}

for(i in 1:N_RESAMPLING) {
  set.seed(1988+i)
  sub = sample(1:n, n, replace=TRUE)
  re_data_g=sim_data_g[sub,]
  re_data_i=sim_data_i[sub,]
  re_data <- rbind(re_data_g,re_data_i)
  coef_temp <- conquantcvx(re_data$Y, cbind(re_data$gender1,re_data$gender2), re_data$D, x_max = c(1,1)) #gender = {0,1}
  #coefficient values for the constrained linear regression
  for (j in 1:length(coef_temp))
    coef2[i,j] <- coef_temp[j]
  #ROC from constrained linear regression
  for(j in 1:length(x_hat)){
    for (k in 1:length(p_hat)){
      roc2[i,(j-1)*length(p_hat)+k] <- roc_fn(x1=x_hat[j],x2=x_hat[j], p=p_hat[k], X1=re_data$gender1,
                                              X2=re_data$gender2, D=re_data$D, Y=re_data$Y, beta_11=coef2[i,2],
                                              beta_12=coef2[i,5],beta_21=coef2[i,3],beta_22=0) 
    }
  }
}


#2.results of mean and SD for mean differences and ROCs - Table 3 and Table 4####################
#1) mean difference
#beta_D + beta_XD * X
#coef are including 6 elements - intercept, X1, X2, D, X1D, X2D
#mean difference for each age of unconstrained linear regression
coef1$male=coef1[,4]+coef1[,5] 
coef1$female=coef1[,4] 
#mean difference for each age of constrained linear regression
coef2$male=coef2[,4]+coef2[,5] #similar as above
coef2$female=coef2[,4]

## Bismillah ##
setwd("C:/Users/Dissa/Documents/Bismillah/Step 2/(Terbaru) Hasil Analisis/Output R/Output Kualitatif")

#LIBRARY 
library(readxl)
library(FactoMineR) 
library(stats) 
library(datasets) 
library(magrittr) 
library(dplyr) 
library(tidyverse) 
library(writexl) 
library(distances) 
library(ca) 
library("factoextra") 
library(ggplot2) 

dataraw <- read_excel(file.choose()) ###read file : raw data kategori uji dependensi
head(dataraw)

#1. Analisis Independensi dengan Uji Chi-Square
#var kecamatan vs x1 
#var1 
chisquare1 = chisq.test(table(cbind(dataraw[1],dataraw[2]))) 
chisquare1 
#var kecamatan vs x12
#var2 
chisquare2 = chisq.test(table(cbind(dataraw[1],dataraw[3]))) 
chisquare2 
#var kecamatan vs x3
#var3 
chisquare3 = chisq.test(table(cbind(dataraw[1],dataraw[4]))) 
chisquare3
#var kecamatan vs x4 
#var4 
chisquare4 = chisq.test(table(cbind(dataraw[1],dataraw[5]))) 
chisquare4 
#var kecamatan vs x5 
#var5 
chisquare5 = chisq.test(table(cbind(dataraw[1],dataraw[6]))) 
chisquare5 
#var kecamatan vs x6 
#var6 
chisquare6 = chisq.test(table(cbind(dataraw[1],dataraw[7]))) 
chisquare6 


#MULTIPLE CORRESPONDENCE ANALYSIS 
##Matriks Indikator 
indikator<-read.csv(file.choose(),header=TRUE,sep=";") ###read file : data clean matriks indikator
Z <- as.matrix(indikator) 
Z 
dim(Z)
##Matriks Burt 
B<-t(Z)%*%Z 
B 
dim(B)
burt = data.frame(as.matrix(B)) 
write_xlsx(burt, "Matriks Burt.xlsx") 


##Matriks Korespondensi Burt 
g<-sum(B) 
g 

P<-B/g 
P 
KB = data.frame(as.matrix(P)) 
dim(KB)
write_xlsx(KB, "Matriks Korespondensi Burt.xlsx") 

##Matriks Residual Standar 
###Proporsi kolom Burt 
cm <- apply(P, 2, sum) #penjumlahan terhadap kolom dari matriks korespondensi 
cm 
PKB = data.frame(as.matrix(cm)) 
dim(PKB)
write_xlsx(PKB, "Proporsi Kolom Burt.xlsx") 

eP <- cm %*% t(cm) 
eP 

diagC = diag(cm) 
diagC 

###matriks residual standar 
S = solve(sqrt(diagC))%*%(P - eP)%*%solve(sqrt(diagC)) 
S 
RS = data.frame(as.matrix(S)) 
dim(RS)
write_xlsx(RS, "Matriks Residual Standar.xlsx") 

##Inersia manual 
datamj <-read.csv(file.choose(),header=TRUE,sep=";") ###read file : data kualitatif setelah chi-square
J<-dim(indikator)[2] 
Q<-dim(datamj[,-1])[2] 
dec<-eigen(S) 
delt<-dec$values[1:(J-Q)] 
expl<-100*(delt/sum(delt)) 
lam<-delt^2 
expl2<-100*(lam/sum(lam)) 
rbind(round(lam,3),round(expl2,1)) 
options(max.print = 10000) # tambahan Awalia

#memanggil nilai eigen
dec$value

#memanggil vektor eigen
dec$vectors

##Inersia MCA package MCA 
data2 <- MCA(datamj, graph=FALSE, method="Burt") 
data2$eig 
eigen = data.frame(as.matrix(data2$eig)) 
write_xlsx(eigen, "eigen.xlsx") 

##Inersia MCA package mjca 
require(ca)
library(ca)
datamj <-read.csv(file.choose(),header=TRUE,sep=";") ###read file : data plot
mca2 <- mjca(datamj, nd=NA, lambda = "Burt", reti=TRUE)
mca2$sv^2 
summary(mca2) 

##Koordinat Standar 
KS<-cacoord(mca2,type = c("standard"), dim = NA) 
KSC<-KS$columns 
KSC 
K_S = data.frame(as.matrix(KSC)) 
dim(K_S)
write_xlsx(K_S, "Koordinat Standar.xlsx") 

##Koordinat Utama 
KU<-cacoord(mca2,type = c("principal"), dim = NA) 
KUC<-KU$columns 
KUC 
K_U = data.frame(as.matrix(KUC)) 
dim(K_U)
write_xlsx(K_U, "Koordinat Utama.xlsx") 

#MATRIKS JARAK EUCLIDEAN 
distance=dist(KUC,method="euclidean") 
jarak=as.matrix(distance) 
jarak
Jarak=data.frame(as.matrix(distance)) 
write_xlsx(Jarak, "jarak euclidean.xlsx") 


### syntax gres
ku_gres<-mca2$colpcoord[,1:23]
distance=dist(ku_gres,method="euclidean") 
jarak1=as.matrix(distance) 
jarak1
Jarak1=data.frame(as.matrix(distance)) 
write_xlsx(Jarak, "jarak euclidean 1.xlsx") 

#Mencari jarak terkecil antara kategori di setiap variabel dan Kecamatan 
### Sesuaikan X mana aja yang signifikan

#X1 
nilai_minimum_per_baris <- apply(jarak[1:31, 32:35], MARGIN = 1, FUN = 
                                   min) 
for (i in 1:length(nilai_minimum_per_baris)) { 
  kolom_minimum <- which(jarak[1:31, 32:35][i,] == 
                           nilai_minimum_per_baris[i]) 
  cat(paste("Nilai minimum pada baris", i , "kolom ke-", kolom_minimum, ": ", 
            nilai_minimum_per_baris[i], "\n")) 
} 
#X2 
nilai_minimum_per_baris <- apply(jarak[1:31, 36:38], MARGIN = 1, FUN = 
                                   min) 
for (i in 1:length(nilai_minimum_per_baris)) { 
  kolom_minimum <- which(jarak[1:31, 36:38][i,] == 
                           nilai_minimum_per_baris[i]) 
  cat(paste("Nilai minimum pada baris", i , "kolom ke-", kolom_minimum, ": ", 
            nilai_minimum_per_baris[i], "\n")) 
} 
#X3 
nilai_minimum_per_baris <- apply(jarak[1:31, 39:41], MARGIN = 1, FUN = 
                                   min) 
for (i in 1:length(nilai_minimum_per_baris)) { 
  kolom_minimum <- which(jarak[1:31, 39:41][i,] == 
                           nilai_minimum_per_baris[i]) 
  cat(paste("Nilai minimum pada baris", i , "kolom ke-", kolom_minimum, ": ", 
            nilai_minimum_per_baris[i], "\n")) 
} 
#X4 
nilai_minimum_per_baris <- apply(jarak[1:31, 42:59], MARGIN = 1, FUN = 
                                   min) 
for (i in 1:length(nilai_minimum_per_baris)) { 
  kolom_minimum <- which(jarak[1:31, 42:59][i,] == 
                           nilai_minimum_per_baris[i]) 
  cat(paste("Nilai minimum pada baris", i , "kolom ke-", kolom_minimum, ": ", 
            nilai_minimum_per_baris[i], "\n")) 
}

#X5 
nilai_minimum_per_baris <- apply(jarak[1:31, 60:61], MARGIN = 1, FUN = 
                                   min) 
for (i in 1:length(nilai_minimum_per_baris)) { 
  kolom_minimum <- which(jarak[1:31, 60:61][i,] == 
                           nilai_minimum_per_baris[i]) 
  cat(paste("Nilai minimum pada baris", i , "kolom ke-", kolom_minimum, ": ", 
            nilai_minimum_per_baris[i], "\n")) 
} 



###### Batas Korelasi Kosinus #####
setwd("C:/Users/Dissa/Documents/Bismillah/Step 2/(Terbaru) Hasil Analisis/Output R/Output Kuantitatif")

#PRE-PROCESSING DATA METRIK 
n = 31
dtmetrik <-read.csv(file.choose(),header=TRUE,sep=";", dec=",") ###read file : data kuantitatif 
dtmetrik 
str(dtmetrik) 

#KORELASI KOSINUS 
##Korelasi Kosinus antara KUC_K dan data metrik 
###Perhitungan Selisih Data dengan Rata-rata 
y7 <- dtmetrik$X7 - mean(dtmetrik$X7) 
y8 <- dtmetrik$X8 - mean(dtmetrik$X8) 
y9 <- dtmetrik$X9 - mean(dtmetrik$X9) 

###Perhitungan Standar Deviasi 
sd7 <- sd(dtmetrik$X7)*sqrt((n-1)/n) 
sd8 <- sd(dtmetrik$X8)*sqrt((n-1)/n) 
sd9 <- sd(dtmetrik$X9)*sqrt((n-1)/n) 

###Perhitungan Standarisasi Data 
z7 <- y7/sd7 
z8 <- y8/sd8 
z9 <- y9/sd9 
zscore <- data.frame(dtmetrik$Nama.Kecamatan, X7=c(z7),X8=c(z8),X9=c(z9)) 
zscore 
std = data.frame(as.matrix(zscore)) 
write_xlsx(std, "data standarisasi.xlsx") 


rownames(zscore)=zscore[,1] 
dtmetrik = zscore[,-1] 
dtmetrik 

KUC_K=KUC[1:31,] #memilih baris 1-31 
KUC_K 
asos1 = function(KUC_K, dtmetrik){ 
  if (nrow(KUC_K)==nrow(dtmetrik)) { 
    col = ncol(KUC_K) 
    fz = matrix(0,ncol(KUC_K),ncol(dtmetrik)) 
    for (i in 1:col) { 
      fz[i,] = colSums(KUC_K[,i]*dtmetrik) 
    } 
    flsqr = colSums(KUC_K^2) 
    zsqr = colSums(dtmetrik^2) 
    rho = matrix(0,ncol(KUC_K),ncol(dtmetrik))
    for (i in 1:ncol(KUC_K)) { 
      for (j in 1:ncol(dtmetrik)) { 
        rho[i,j] = fz[i,j]/(sqrt(flsqr[i])*sqrt(zsqr[j])) 
      }} 
    return(rho) 
  } 
  if (nrow(KUC_K)!=nrow(dtmetrik)) { 
    print() 
  } 
} 
d=asos1(KUC_K, dtmetrik) 
d 
dim(d) 
matriksr=data.frame(as.matrix(d)) 
write_xlsx(matriksr, "Matriks R.xlsx") 

##Koordinat vektor kuantitatif (psi) 
ev = diag(mca2$sv^2) ###eigen value 
ev 
psi = (ev^(1/2))%*%d ###alpha=1/2 
psi 
matrikspsi=data.frame(as.matrix(psi)) 
write_xlsx(matrikspsi, "Matriks Psi.xlsx") 

##Asosiasi Kosinus antara KUC_K dan psi 
col = ncol(KUC_K) 
fz = matrix(0,ncol(KUC_K),ncol(psi)) 
for (i in 1:col) { 
  fz[i,] = colSums(KUC_K[,i]*psi) 
} 
flsqr = rowSums(KUC_K^2) 
zsqr = colSums(psi^2) 
rho = matrix(0,ncol(KUC_K),ncol(psi)) 
for (i in 1:ncol(KUC_K)) { 
  for (j in 1:ncol(psi)) { 
    rho[i,j] = fz[i,j]/(sqrt(flsqr[i])*sqrt(zsqr[j])) 
  } 
} 
rho 
#Menyimpan Rho 
Rho = data.frame(as.matrix(rho)) 
write_xlsx(Rho, "Rho.xlsx")  


library(tidyverse)
library(ggridges)
library(roll)
library(RolWinMulCor)
library(tidyr)
library(BINCOR)

data = read_csv("../Data/phenology_data.csv")

wide<-pivot_wider(data,
  names_from="variable", values_from = value)

wide_sub<-wide[,c("year","daphnia","clearwater","id")]

#test<-roll_cor(wide_sub$daphnia, wide_sub$clearwater, width = 10, min_obs = 1)
plot(test)


###########
#Alequash
AL<-wide_sub[wide_sub$id=="AL",]
AL2<-na.omit(AL)

AL2_sub<-AL2[,c(1:3)]

data_mat<-as.matrix(AL2_sub)

test<-rolwincor_1win(data_mat, varX="daphnia", varY="clearwater", CorMethod="pearson", widthwin=3,
                     Align="center", pvalcorectmethod="BH", rmltrd=TRUE, Scale=TRUE)


test<-rolwincor_heatmap(data_mat, varX="daphnia", varY="clearwater",  CorMethod="pearson", 
                  typewidthwin="FULL", widthwin_1=10, 
                  widthwin_N=dim(inputdata)[1], Align="center", 
                  pvalcorectmethod="BH", rmltrd=TRUE, Scale=TRUE)


plot_heatmap(data_mat, test$matcor, test$pvalscor, 
             test$left_win, test$righ_win, test$Windows, KCASE="BIVAR", 
             typewidthwin="FULL", varX="daphnia", varY="clearwater", coltsX="red", CEXLAB=1.15, 
             CEXAXIS=1.65, coltsY="black", LWDtsX=2, LWDtsY=2)

plot.new()
#######################################

#BM
BM<-wide_sub[wide_sub$id=="BM",]
BM2<-na.omit(BM)

BM2_sub<-BM2[,c(1:3)]

data_mat<-as.matrix(BM2_sub)

test<-rolwincor_1win(data_mat, varX="daphnia", varY="clearwater", CorMethod="pearson", widthwin=3,
                     Align="center", pvalcorectmethod="BH", rmltrd=TRUE, Scale=TRUE)


test<-rolwincor_heatmap(data_mat, varX="daphnia", varY="clearwater",  CorMethod="pearson", 
                        typewidthwin="FULL", widthwin_1=10, 
                        widthwin_N=dim(inputdata)[1], Align="center", 
                        pvalcorectmethod="BH", rmltrd=TRUE, Scale=TRUE)


plot_heatmap(data_mat, test$matcor, test$pvalscor, 
             test$left_win, test$righ_win, test$Windows, KCASE="BIVAR", 
             typewidthwin="FULL", varX="daphnia", varY="clearwater", coltsX="red", CEXLAB=1.15, 
             CEXAXIS=1.65, coltsY="black", LWDtsX=2, LWDtsY=2)

plot.new()
#######################################

#ME
ME<-wide_sub[wide_sub$id=="ME",]
ME2<-na.omit(ME)

ME2_sub<-ME2[,c(1:3)]

data_mat<-as.matrix(ME2_sub)

test<-rolwincor_1win(data_mat, varX="daphnia", varY="clearwater", CorMethod="pearson", widthwin=3,
                     Align="center", pvalcorectmethod="BH", rmltrd=TRUE, Scale=TRUE)


test<-rolwincor_heatmap(data_mat, varX="daphnia", varY="clearwater",  CorMethod="pearson", 
                        typewidthwin="FULL", widthwin_1=10, 
                        widthwin_N=dim(inputdata)[1], Align="center", 
                        pvalcorectmethod="BH", rmltrd=TRUE, Scale=TRUE)


plot_heatmap(data_mat, test$matcor, test$pvalscor, 
             test$left_win, test$righ_win, test$Windows, KCASE="BIVAR", 
             typewidthwin="FULL", varX="daphnia", varY="clearwater", coltsX="red", CEXLAB=1.15, 
             CEXAXIS=1.65, coltsY="black", LWDtsX=2, LWDtsY=2)
plot.new()
#######################################

#MO
MO<-wide_sub[wide_sub$id=="MO",]
MO2<-na.omit(MO)

MO2_sub<-MO2[,c(1:3)]

data_mat<-as.matrix(MO2_sub)

test<-rolwincor_1win(data_mat, varX="daphnia", varY="clearwater", CorMethod="pearson", widthwin=3,
                     Align="center", pvalcorectmethod="BH", rmltrd=TRUE, Scale=TRUE)


test<-rolwincor_heatmap(data_mat, varX="daphnia", varY="clearwater",  CorMethod="pearson", 
                        typewidthwin="FULL", widthwin_1=10, 
                        widthwin_N=dim(inputdata)[1], Align="center", 
                        pvalcorectmethod="BH", rmltrd=TRUE, Scale=TRUE)


plot_heatmap(data_mat, test$matcor, test$pvalscor, 
             test$left_win, test$righ_win, test$Windows, KCASE="BIVAR", 
             typewidthwin="FULL", varX="daphnia", varY="clearwater", coltsX="red", CEXLAB=1.15, 
             CEXAXIS=1.65, coltsY="black", LWDtsX=2, LWDtsY=2)

plot.new()


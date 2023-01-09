library(tidyverse)
library(ggridges)
library(roll)
library(RolWinMulCor)
library(tidyr)
library(BINCOR)

data = read_csv("C:/Users/kreinl1/OneDrive/OneDrive - UW-Madison/GitHub/ntl_phenology/Data/analysis_ready/final_combined_dates_filled_v2.csv")

data %>% drop_na(daynum)
data = subset(data, select = -c(sampledate,filled_flag,daynum_fill) )

wide<-pivot_wider(data,
  names_from="metric", values_from = daynum)

test<-data.frame(wide[,(4:15)])

cormat<-cor(x=test, method = "pearson", use = "pairwise.complete.obs")

cormat_p<-cor_pmat(x=test, method = "pearson", use = "pairwise.complete.obs")

library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

dev.off()

library(ggplot2)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_raster()+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

#####################
library(rstatix)
test0<-data.frame(wide[,(4:15)])

cor.mat <- test %>% cor_mat()

cor.mat %>% cor_get_pval()

cor.mat %>%
  cor_reorder() %>%
  pull_lower_triangle() %>%
  cor_plot(label = TRUE)


###########

OL<-wide %>% 
  filter(lakeid=="BM"|lakeid=="CR"|lakeid=="SP"|lakeid=="TR")
  
test<-data.frame(OL[,(4:15)])

cor.mat <- test %>% cor_mat()

cor.mat %>% cor_get_pval()

cor.mat %>%
  cor_reorder() %>%
  pull_lower_triangle() %>%
  cor_plot(label = TRUE)


EU<-wide %>% 
  filter(lakeid=="ME"|lakeid=="MO"|lakeid=="WI")

test<-data.frame(EU[,(4:15)])

cor.mat <- test %>% cor_mat()

cor.mat %>% cor_get_pval()

cor.mat %>%
  cor_reorder() %>%
  pull_lower_triangle() %>%
  cor_plot(label = TRUE) 


DYS<-wide %>% 
  filter(lakeid=="CB"|lakeid=="TB")

test<-data.frame(DYS[,(4:15)])

cor.mat <- test %>% cor_mat()

cor.mat %>% cor_get_pval()

cor.mat %>%
  cor_reorder() %>%
  pull_lower_triangle() %>%
  cor_plot(label = TRUE)

  

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


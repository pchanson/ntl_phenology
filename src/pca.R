library(tidyverse)
library(factoextra)
library(corrplot)

# data
dates = read_csv("./Data/phenology_data.csv") %>% 
  filter(variable %in% c('straton', 'stratoff', 'energy', 'stability', 'anoxia',
                                                                         'iceon', 'iceoff', 'daphnia', 'chla', 'doc')) %>% 
  select(-decade)

dates_wide = dates %>% 
  pivot_wider(id_cols = c("id", "year"), names_from = "variable", values_from = "value")

dates_wide_complete = na.omit(dates_wide)

pca = prcomp(dates_wide_complete[, 3:ncol(dates_wide)], scale=T, center=T)

get_eig(pca)


fviz_eig(pca)
fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

var = get_pca_var(pca)
inds = get_pca_ind(pca)

corrplot(var$cos2, is.corr=FALSE)
mtext("PC-variable cos2 correlation", side=3, line=0.5, outer=T, cex.=1.5)

pca_res = dates_wide_complete[, c("id", "year")]
pca_res$PC1 = pca$x[,1]
pca_res$PC2 = pca$x[,2]
pca_res$PC3 = pca$x[,3]
pca_res$PC4 = pca$x[,4]

pca_res$id = factor(pca_res$id, levels = c('AL', 'BM','CR', 'SP','TR','CB', 'TB', 'FI', 'ME', 'MO', 'WI'), ordered = T)


# calc correlation of pca1
p1_corr = pca_res %>%
  select(id, year, PC1) %>% 
  arrange(id) %>% 
  pivot_wider(names_from = "id", values_from="PC1") %>%
  column_to_rownames(var = "year") %>% 
  cor(use="complete.obs")

par(oma=c(0,0,2,0))
corrplot(p1_corr, method="circle")
mtext("PC1 (19.5%)", side=3, outer=T, line=0.5, cex=1.5)

# calc correlation of pca2
p2_corr = pca_res %>%
  select(id, year, PC2) %>% 
  arrange(id) %>% 
  pivot_wider(names_from = "id", values_from="PC2") %>%
  column_to_rownames(var = "year") %>% 
  cor(use="complete.obs")

par(oma=c(0,0,2,0))
corrplot(p2_corr, method="circle")
mtext("PC2 (18.5%)", side=3, outer=T, line=0.5, cex=1.5)

# calc correlation of pca3
p3_corr = pca_res %>%
  select(id, year, PC3) %>% 
  arrange(id) %>% 
  pivot_wider(names_from = "id", values_from="PC3") %>%
  column_to_rownames(var = "year") %>% 
  cor(use="complete.obs")

corrplot(p3_corr, method="circle")

# calc correlation of pca3
p4_corr = pca_res %>%
  select(id, year, PC4) %>% 
  arrange(id) %>% 
  pivot_wider(names_from = "id", values_from="PC4") %>%
  column_to_rownames(var = "year") %>% 
  cor(use="complete.obs")

corrplot(p4_corr, method="circle")

# go thru, plot time series of each pairwise lake 
lake_combos = expand_grid(L1 = c('AL', 'BM','CR', 'SP','TR','CB', 'TB', 'ME', 'MO', 'WI'), 
                          L2 = c('AL', 'BM','CR', 'SP','TR','CB', 'TB', 'ME', 'MO', 'WI')) %>% 
  filter(L1 != L2)

for(i in 1:nrow(lake_combos)){
  l1 = lake_combos[i, "L1"]
  l2 = lake_combos[i, "L2"]
  
  p1_data = pca_res %>% 
    filter(id %in% c(l1, l2)) %>% 
    select(id, year, PC1)
  
  p2_data = pca_res %>% 
    filter(id %in% c(l1, l2)) %>% 
    select(id, year, PC2)
  
  g1 <- ggplot() +
    geom_point(data = p1_data, aes(year, PC1, color=id)) +
    geom_line(data = p1_data, aes(year, PC1, color=id)) +
    ylab('PC1') + xlab('') +
    scale_color_brewer(palette = "Set2") +
    theme_bw() +
    ggtitle(paste0('Pearson ', round(p1_corr[pull(l1), pull(l2)], 3)))
  
  ggsave(file = paste0('./Figures/PCA/lake_pairwise_timeseries/pc1/',l1,'_',l2,'.png'), g1, dpi = 500, width =9, height = 3)
  
  g2 <- ggplot() +
    geom_point(data = p2_data, aes(year, PC2, color=id)) +
    geom_line(data = p2_data, aes(year, PC2, color=id)) +
    ylab('PC2') + xlab('') +
    scale_color_brewer(palette = "Set2") +
    theme_bw() +
    ggtitle(paste0('Pearson ', round(p2_corr[pull(l1), pull(l2)], 3)))
  
  ggsave(file = paste0('./Figures/PCA/lake_pairwise_timeseries/pc1/',l1,'_',l2,'.png'), g1, dpi = 500, width =9, height = 3)
  ggsave(file = paste0('./Figures/PCA/lake_pairwise_timeseries/pc2/',l1,'_',l2,'.png'), g2, dpi = 500, width =9, height = 3)
}


# look at just physical variables

# data
dates_phys = dates %>% 
  filter(variable %in% c('straton', 'stratoff', 'energy', 'stability', 'anoxia',
                         'iceon', 'iceoff'))

dates_phys_wide = dates_phys %>% 
  pivot_wider(id_cols = c("id", "year"), names_from = "variable", values_from = "value")

dates_phys_wide_complete = na.omit(dates_phys_wide)

pca_phys = prcomp(dates_phys_wide_complete[, 3:ncol(dates_phys_wide)], scale=T, center=T)

get_eig(pca_phys)


fviz_eig(pca_phys_res)
fviz_pca_var(pca_phys,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

var_phys = get_pca_var(pca_phys)
inds_phy = get_pca_ind(pca_phys)

corrplot(var_phys$cos2, is.corr=FALSE)
mtext("PC-variable cos2 correlation", side=3, line=0.5, outer=T, cex.=1.5)

pca_phys_res = dates_phys_wide_complete[, c("id", "year")]
pca_phys_res$PC1 = pca_phys$x[,1]
pca_phys_res$PC2 = pca_phys$x[,2]
pca_phys_res$PC3 = pca_phys$x[,3]
pca_phys_res$PC4 = pca_phys$x[,4]

pca_phys_res$id = factor(pca_phys_res$id, levels = c('AL', 'BM','CR', 'SP','TR','CB', 'TB', 'FI', 'ME', 'MO', 'WI'), ordered = T)


# calc correlation of pca1
p1_phys_corr = pca_phys_res %>%
  select(id, year, PC1) %>% 
  arrange(id) %>% 
  pivot_wider(names_from = "id", values_from="PC1") %>%
  column_to_rownames(var = "year") %>% 
  cor(use="complete.obs")

par(oma=c(0,0,2,0))
corrplot(p1_phys_corr, method="circle")
mtext("PC1 (phys-only) (26.9%)", side=3, outer=T, line=0.5, cex=1.5)

# calc correlation of pca2
p2_phys_corr = pca_phys_res %>%
  select(id, year, PC2) %>% 
  arrange(id) %>% 
  pivot_wider(names_from = "id", values_from="PC2") %>%
  column_to_rownames(var = "year") %>% 
  cor(use="complete.obs")

par(oma=c(0,0,2,0))
corrplot(p2_phys_corr, method="circle")
mtext("PC2 (phys-only) (25.5%)", side=3, outer=T, line=0.5, cex=1.5)

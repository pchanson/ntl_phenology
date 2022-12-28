# make and save final figures for manuscript

library(tidyverse)
library(ggridges)
library(lubridate)
library(scales)
library(corrplot)
library(factoextra)
library(roll)
library(RolWinMulCor)
library(BINCOR)
library(corrr)
library(patchwork)
library(MetBrewer)
library(kohonen)
library(RColorBrewer)

# read in data
dat = read_csv("Data/analysis_ready/final_combined_dates_filled_v2.csv")
dat$sampledate = as.Date(paste0(dat$year-1, "-12-31")) + dat$daynum_fill

table(dat$metric)

# Fig 1: Ridges
vars_order = c("iceoff", "straton",  "chlor_spring", "secchi_openwater_max", "secchi_openwater_min", "daphnia_biomass", "zoopDensity", "doc_epiMax", "totpuf_hypoMin",  "totpuf_epiMax", "anoxia_summer", "stability", "energy", "totpuf_epiMin", "totpuf_hypoMax", "stratoff", "iceon")
vars_label = c("ice off", "strat onset", "spring bloom", "SecchiMax","SecchiMin", "daphnia", "zoopDensity", "DOC", "TP hypo min", "TP epi max",  "anoxia",  "stability", "energy", "TP epi min", "TP hypo max", "strat offset", "ice on")

# add and extra lake in N
lakes_order = c("AL", "BM", "CB", "CR", "SP", "TB", "TR", "", "FI", "ME", "MO", "WI")
# vars_order = c("iceoff", "straton", "chlor_spring", "secchi_openwater", "daphnia_biomass", "doc", "chlor_all", "anoxia_summer", "stability", "energy", "chlor_fall", "stratoff", "iceon")

empty_lake = dat %>% 
  select(metric) %>% 
  mutate(lakeid = "") %>% 
  distinct()

pRidges = dat %>% 
  full_join(empty_lake) %>% 
  filter(metric %in% vars_order) %>% 
  mutate(lakeid = factor(lakeid, levels = lakes_order),
         metric = factor(metric, levels = rev(vars_order), labels = rev(vars_label))) %>% 
  ggplot() + 
  stat_density_ridges(aes(x = as.Date(daynum, origin = as.Date('2019-01-01')), 
                          y= metric, col = metric, fill = metric), 
                      alpha = 0.5, quantile_lines = T, quantiles = 2) +
  scale_fill_manual(values=met.brewer("Archambault", length(vars_order))) + 
  scale_color_manual(values=met.brewer("Archambault", length(vars_order))) +
  scale_x_date(labels = date_format("%b")) +
  facet_wrap(~ (lakeid)) +
  xlab('') + ylab('')+
  theme_minimal(base_size = 6) + 
  theme(axis.text = element_text(size=6),
        strip.text=element_text(size=10),
        panel.spacing.y=unit(0.4,"lines")) +
  guides(fill="none", color="none"); pRidges

ggsave("Figures/manuscript/Figure1.png",
       pRidges,
       width=6,
       height=5,
       units="in",
       dpi=500,
       bg="white"
       )

# Fig 2: PCA loading
dat_pca = dat %>% 
  filter(metric %in% vars_order) %>% 
  mutate(lakeid = factor(lakeid, levels = lakes_order),
         metric = factor(metric, levels = rev(vars_order), labels = rev(vars_label))) %>% 
  select(lakeid, year, metric, daynum_fill) %>% 
  pivot_wider(names_from = "metric", values_from="daynum_fill") %>% 
  filter(lakeid != "WI")

pca = prcomp(dat_pca[, 3:ncol(dat_pca)], scale=T, center=T)
lakeid_pca = lakes_order[!(lakes_order %in% c("", "WI"))]

# fviz_eig(pca)
pPCAload = fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             arrowsize=1,
             labelsize=3
) +
  labs(x="PC 1", y="PC 2", title="", color="Contribution")+ 
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=16))

ggsave("Figures/manuscript/OldFigure_PCAloading.jpeg",
       pPCAload,
       width=6,
       height=4,
       units="in",
       dpi=300,
       bg="white"
)

# Fig 3: PCA correlations
var = get_pca_var(pca)
inds = get_pca_ind(pca)

pca_res = dat_pca[, c("lakeid", "year")]
pca_res$PC1 = pca$x[,1]
pca_res$PC2 = pca$x[,2]
pca_res$PC3 = pca$x[,3]
pca_res$PC4 = pca$x[,4]

pca_res_cor = pca_res %>% 
  pivot_longer(cols = c("PC1", "PC2", "PC3", "PC4")) %>% 
  pivot_wider(names_from = "lakeid", values_from="value") %>% 
  arrange(name, year)


pca_matrix_p1 = pca_res_cor %>% 
  filter(name == "PC1") %>% 
  select(-year) %>% 
  summarise(as.data.frame(cor(.[,lakeid_pca], use="complete.obs"))) %>% 
  as.matrix() 
rownames(pca_matrix_p1) = lakeid_pca
# diag(pca_matrix) = 0

pca_matrix_p2 = pca_res_cor %>% 
  filter(name == "PC2") %>% 
  select(-year) %>% 
  summarise(as.data.frame(cor(.[,lakeid_pca], use="complete.obs"))) %>% 
  as.matrix() 
rownames(pca_matrix_p2) = lakeid_pca

jpeg("Figures/manuscript/OldFigure_PCAcorrs.jpeg",
     width=8, height=4, units="in", 
     res=300)
par(mfrow=c(1,2))
corrplot(pca_matrix_p1, method="circle", title="PC 1", mar=c(0,0,2,0), cex.main=1.75)
corrplot(pca_matrix_p2, method="circle", title = "PC 2", mar=c(0,0,2,0), cex.main=1.75)
dev.off()

# Fig 4: SOM clusters, SOM drivers
source('./src/Functions/coolBlueHotRed.R')

dat_wide = dat %>% 
  filter(metric %in% vars_order & lakeid != "WI") %>% 
  pivot_wider(id_cols = c("lakeid", "year"), names_from = "metric", values_from = "daynum_fill") 

n <- 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
names <- paste0(dat_wide$lakeid)
n.colors <- c(rep(col_vector[1],sum(!is.na(match(names, 'AL')))),
              rep(col_vector[2],sum(!is.na(match(names, 'BM')))),
              rep(col_vector[3],sum(!is.na(match(names, 'CB')))),
              rep(col_vector[4],sum(!is.na(match(names, 'CR')))),
              rep(col_vector[5],sum(!is.na(match(names, 'FI')))),
              rep(col_vector[6],sum(!is.na(match(names, 'ME')))),
              rep(col_vector[7],sum(!is.na(match(names, 'MO')))),
              rep(col_vector[8],sum(!is.na(match(names, 'SP')))),
              rep(col_vector[9],sum(!is.na(match(names, 'TB')))),
              rep(col_vector[10],sum(!is.na(match(names, 'TR')))),
              rep(col_vector[11],sum(!is.na(match(names, 'WI')))))

data_train_matrix_unscaled <- dat_wide %>% 
  filter(!(lakeid == "WI" & year == 2011)) %>%
  select(all_of(vars_order)) %>% 
  # scale() %>% 
  as.matrix()

data_train_matrix <- dat_wide %>% 
  filter(!(lakeid == "WI" & year == 2011)) %>%
  select(all_of(vars_order)) %>% 
  scale() %>% 
  as.matrix()

set.seed(1)
som_grid <- somgrid(xdim =7, ydim=7, topo="hexagonal")
som_model <- som(data_train_matrix, 
                 grid=som_grid, 
                 rlen=500, 
                 alpha=c(0.05,0.01), 
                 keep.data = TRUE)

par(mar=c(5,5,5,5))
plot(som_model, type="changes")
plot(som_model, type="count")

som_cluster <- cutree(hclust(dist(som_model$codes[[1]])),3)
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2', "#7f7f7f", "#bcbd22", "#17becf","#00FF00", "#9F81F7"
                    ,"#FFFF00", "#F6CECE","#610B21")

jpeg("Figures/manuscript/FigureSI_SOMmap.jpeg",
     width=6, height=6, units="in",
     res=300)
plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters",
     label = names, pchs = names, cex =0.7) 
add.cluster.boundaries(som_model, som_cluster)
dev.off()


# plot SOM cluster info
som_output_lakeYear = data.frame(
  Unit = som_model$unit.classif,
  Lake = names)

unit_to_cluster = data.frame(
  Unit = as.numeric(str_remove(names(som_cluster), "V")),
  Cluster = unname(som_cluster)
)  
lakeColors_df = data.frame(
  Lake = c("AL", "BM", "CR", "SP", "TR", "CB", "TB", "ME", "MO", "FI"),
  Color = c("#d0d1e6", "#a6bddb", "#74a9cf", "#2b8cbe", "#045a8d", "#cc4c02", "#8c2d04", "#bae4b3", "#74c476", "#238b45")
)

Lake_colors = str_to_upper(lakeColors_df$Color)
names(Lake_colors) = lakeColors_df$Lake

som_output_lakeYear = som_output_lakeYear %>% 
  left_join(unit_to_cluster) %>% 
  bind_cols(as.data.frame(data_train_matrix)) %>% 
  left_join(lakeColors_df)

p4_clusterLakes = som_output_lakeYear %>% 
  mutate(LakeType = NA) %>% 
  mutate(LakeType = ifelse(Lake %in% c("FI", "ME", "MO"), "Southern", LakeType)) %>% 
  mutate(LakeType = ifelse(Lake %in% c("TB", "CB"), "North. Bog", LakeType)) %>% 
  mutate(LakeType = ifelse(is.na(LakeType), "Northern", LakeType)) %>% 
  count(Cluster, Lake, Color) %>% 
  mutate(Lake = factor(Lake, levels = c("AL", "BM", "CR", "SP", "TR", "CB", "TB", "ME", "MO", "FI"), ordered=T)) %>% 
  ggplot(aes(x=Cluster, y=n, fill=Lake)) + 
  geom_bar(stat="identity") +
  coord_flip() +
  theme_bw() +
  scale_fill_manual(values=Lake_colors) +
  labs(y="Number of Lake-Years")

ggsave("Figures/manuscript/Figure2_lakeClusters.jpeg",
       p4_clusterLakes,
       width=6,
       height=6,
       units="in",
       dpi=300,
       bg="white"
)

clusterColors = c("1" = "#cc4c02", "2" = "#74c476", "3" = "#2b8cbe")

# plot of all cluster mean-scaled-dates
all_cluster_devs = som_output_lakeYear %>% 
  pivot_longer(cols = vars_order) 

p4_SOMinputs_all = all_cluster_devs%>% 
  mutate(name = factor(name, levels = rev(vars_order), labels = rev(vars_label), ordered = T))  %>% 
  ggplot(aes(x=name, y=value, fill=as.factor(Cluster), group=interaction(Cluster, name)))+
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme_bw() +
  coord_flip() +
  geom_hline(yintercept = 0, size=1)+
  labs(x="Event", y="Relative Timing", fill="Cluster") +
  scale_fill_manual(values=clusterColors)

ggsave("Figures/manuscript/Figure3_SOMinputs_all.jpeg",
       p4_SOMinputs_all,
       width=4,
       height=6,
       units="in",
       dpi=300,
       bg="white"
)

sig_clusterVars = som_output_lakeYear %>% 
  pivot_longer(cols = vars_order) %>% 
  group_by(Cluster, name) %>% 
  summarise(firstQuart = boxplot.stats(value)$stats[2],
            thirdQuart = boxplot.stats(value)$stats[4]) %>% 
  filter(!(0 > firstQuart & 0 < thirdQuart)) %>% 
  select(Cluster, name)

unique_cluster_vars = all_cluster_devs %>% 
  select(Cluster, name) %>% 
  distinct()

p4_SOMinputs_sig = sig_clusterVars %>% 
  left_join(all_cluster_devs) %>% 
  full_join(unique_cluster_vars) %>% 
  mutate(name = factor(name, levels = rev(vars_order), labels = rev(vars_label), ordered = T)) %>% 
  ggplot(aes(x=name, y=value, fill=as.factor(Cluster))) +
  geom_boxplot(position = position_dodge(preserve = "single")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme_bw() +
  coord_flip() +
  geom_hline(yintercept = 0, size=2) +
  labs(x="Event", y="Relative Timing", fill="Cluster")

ggsave("Figures/manuscript/FigureSI_SOMinputs_sig.jpeg",
       p4_SOMinputs_sig,
       width=4,
       height=6,
       units="in",
       dpi=300,
       bg="white"
)

# try SOM w/o ice dates
data_train_matrix_noIce <- dat_wide %>% 
  filter(!(lakeid == "WI" & year == 2011)) %>%
  select(all_of(vars_order)) %>% 
  select(-iceon, -iceoff) %>% 
  scale() %>% 
  as.matrix()

set.seed(1)
som_grid <- somgrid(xdim =7, ydim=7, topo="hexagonal")
som_model_noIce <- som(data_train_matrix_noIce, 
                 grid=som_grid, 
                 rlen=500, 
                 alpha=c(0.05,0.01), 
                 keep.data = TRUE)

par(mar=c(5,5,5,5))
plot(som_model_noIce, type="changes")
plot(som_model_noIce, type="count")

som_cluster_noIce <- cutree(hclust(dist(som_model_noIce$codes[[1]])),3)
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2', "#7f7f7f", "#bcbd22", "#17becf","#00FF00", "#9F81F7"
                    ,"#FFFF00", "#F6CECE","#610B21")

plot(som_model_noIce, type="mapping", bgcol = pretty_palette[som_cluster_noIce], main = "Clusters",
     label = names, pchs = names, cex =0.7) 
add.cluster.boundaries(som_model_noIce, som_cluster_noIce)


# plot SOM cluster info
som_output_lakeYear_noIce = data.frame(
  Unit = som_model_noIce$unit.classif,
  Lake = names)

unit_to_cluster_noIce = data.frame(
  Unit = as.numeric(str_remove(names(som_cluster_noIce), "V")),
  Cluster = unname(som_cluster_noIce)
)  
lakeColors_df = data.frame(
  Lake = c("AL", "BM", "CR", "SP", "TR", "CB", "TB", "ME", "MO", "FI"),
  Color = c("#d0d1e6", "#a6bddb", "#74a9cf", "#2b8cbe", "#045a8d", "#cc4c02", "#8c2d04", "#bae4b3", "#74c476", "#238b45")
)

Lake_colors = str_to_upper(lakeColors_df$Color)
names(Lake_colors) = lakeColors_df$Lake

som_output_lakeYear_noIce = som_output_lakeYear_noIce %>% 
  left_join(unit_to_cluster_noIce) %>% 
  bind_cols(as.data.frame(data_train_matrix_noIce)) %>% 
  left_join(lakeColors_df)

p4_clusterLakes_noIce = som_output_lakeYear_noIce %>% 
  mutate(LakeType = NA) %>% 
  mutate(LakeType = ifelse(Lake %in% c("FI", "ME", "MO"), "Southern", LakeType)) %>% 
  mutate(LakeType = ifelse(Lake %in% c("TB", "CB"), "North. Bog", LakeType)) %>% 
  mutate(LakeType = ifelse(is.na(LakeType), "Northern", LakeType)) %>% 
  count(Cluster, Lake, Color) %>% 
  mutate(Lake = factor(Lake, levels = c("AL", "BM", "CR", "SP", "TR", "CB", "TB", "ME", "MO", "FI"), ordered=T)) %>% 
  ggplot(aes(x=Cluster, y=n, fill=Lake)) + 
  geom_bar(stat="identity") +
  coord_flip() +
  theme_bw() +
  scale_fill_manual(values=Lake_colors) +
  labs(y="Number of Lake-Years")

p4_clusterLakes_noIce
clusterColors = c("1" = "#cc4c02", "2" = "#74c476", "3" = "#2b8cbe")

# plot of all cluster mean-scaled-dates
all_cluster_devs_noIce = som_output_lakeYear_noIce %>% 
  pivot_longer(cols = vars_order[!vars_order %in% c("iceon", "iceoff")]) 

p4_SOMinputs_all_noIce = all_cluster_devs_noIce %>% 
  mutate(name = factor(name, levels = rev(vars_order), labels = rev(vars_label), ordered = T))  %>% 
  ggplot(aes(x=name, y=value, fill=as.factor(Cluster), group=interaction(Cluster, name)))+
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme_bw() +
  coord_flip() +
  geom_hline(yintercept = 0, size=1)+
  labs(x="Event", y="Relative Timing", fill="Cluster") +
  scale_fill_manual(values=clusterColors)

p4_SOMinputs_all_noIce

# OLDER CODE for visualizing the map

# the 4x4 first?
# g <- somgrid(xdim = 4, ydim = 4, topo = "rectangular" )
# 
# map <- som(dat_wide %>% 
#              select(all_of(vars_order))%>% 
#              as.matrix(),
#            grid = g,
#            alpha = c(0.05, 0.01),
#            radius = 1)
# 
# plot(map, type='codes',palette.name = rainbow, labels = names)
# 
# plot(map, type='mapping',col = n.colors,
#      label = names,pchs = names)

# layout(matrix(1:4,ncol=2,nrow=2,byrow=T))
vars_plot = c("iceoff", "secchi_openwater", "anoxia_summer", "iceon")
vars_lab = c("ice off", "secchi", "anoxia", "ice on")
for(i in 1:4){
  par(cex.main=2)
  var <- vars_plot[i] #define the variable to plot 
  var_unscaled <- aggregate(as.numeric(data_train_matrix_unscaled[,var]), by=list(som_model$unit.classif), FUN=mean, simplify=TRUE)[,2] 
  # plot(som_model, type = "property", property=var_unscaled, 
  #      main=sprintf(
  #        vars_lab[i]), 
  #      palette.name=coolBlueHotRed, cex=1.75)
  # add.cluster.boundaries(som_model, som_cluster)
}
# get rid of single WI data point that's always out on it's own?

# length(vars_order)
# layout(matrix(1:12,ncol=4,nrow=3,byrow=T))
# # par()
# for(i in 1:11){
#   par(cex.main=1)
#   plot(som_model, type = "property", property=som_model$codes[[1]][,i], main=sprintf(vars_order[i]), palette.name=coolBlueHotRed)
#   add.cluster.boundaries(som_model, som_cluster)
# }


# SI: N vs. S. PCA loading

# SI: all SOM drivers

# others? map? table of lake characteristics?

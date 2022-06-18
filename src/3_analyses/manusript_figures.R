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
dat = read_csv("Data/analysis_ready/final_combined_dates_filled_v1.csv")
dat$sampledate = as.Date(paste0(dat$year-1, "-12-31")) + dat$daynum_fill

# Fig 1: Ridges
vars_order = c("iceoff", "straton",  "chlor_spring", "secchi_openwater", "daphnia_biomass", "doc",  "anoxia_summer", "stability", "energy", "stratoff", "iceon")

# add and extra lake in N
lakes_order = c("AL", "BM", "CB", "CR", "SP", "TB", "TR", "", "FI", "ME", "MO", "WI")

# vars_order = c("iceoff", "straton", "chlor_spring", "secchi_openwater", "daphnia_biomass", "doc", "chlor_all", "anoxia_summer", "stability", "energy", "chlor_fall", "stratoff", "iceon")
vars_order = c("iceoff", "straton",  "chlor_spring", "secchi_openwater", "daphnia_biomass", "doc",  "anoxia_summer", "stability", "energy", "stratoff", "iceon")

empty_lake = dat %>% 
  select(metric) %>% 
  mutate(lakeid = "") %>% 
  distinct()

pRidges = dat %>% 
  full_join(empty_lake) %>% 
  filter(metric %in% vars_order) %>% 
  mutate(lakeid = factor(lakeid, levels = lakes_order),
         metric = factor(metric, levels = rev(vars_order), labels = rev(c("ice off", "strat onset", "spring bloom", "clearwater", "daphnia", "DOC",  "anoxia", "stability", "energy", "strat offset", "ice on")))) %>% 
  ggplot() + 
  stat_density_ridges(aes(x = as.Date(daynum, origin = as.Date('2019-01-01')), 
                          y= metric, col = metric, fill = metric), 
                      alpha = 0.5, quantile_lines = T, quantiles = 2) +
  scale_fill_manual(values=met.brewer("Archambault", 11)) + 
  scale_color_manual(values=met.brewer("Archambault", 12)) +
  scale_x_date(labels = date_format("%b")) +
  facet_wrap(~ (lakeid)) +
  xlab('') + ylab('')+
  theme_minimal(base_size = 6) + 
  theme(axis.text = element_text(size=6),
        strip.text=element_text(size=10),
        panel.spacing.y=unit(0.4,"lines")) +
  guides(fill="none", color="none")

ggsave("Figures/manuscript/Figure1.jpeg",
       pRidges,
       width=6,
       height=4,
       units="in",
       dpi=300,
       bg="white"
       )

# Fig 2: PCA loading
dat_pca = dat %>% 
  filter(metric %in% vars_order) %>% 
  mutate(lakeid = factor(lakeid, levels = lakes_order),
         metric = factor(metric, levels = rev(vars_order), labels = rev(c("ice off", "            strat onset", "spring bloom", "clearwater", "daphnia", "DOC",  "     anoxia", "stability", "energy", "strat offset", "    ice on")))) %>% 
  select(lakeid, year, metric, daynum_fill) %>% 
  pivot_wider(names_from = "metric", values_from="daynum_fill")

pca = prcomp(dat_pca[, 3:ncol(dat_pca)], scale=T, center=T)
lakeid_pca = lakes_order[lakes_order != ""]

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

ggsave("Figures/manuscript/Figure2.jpeg",
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

jpeg("Figures/manuscript/Figure3.jpeg",
     width=8, height=4, units="in", 
     res=300)
par(mfrow=c(1,2))
corrplot(pca_matrix_p1, method="circle", title="PC 1", mar=c(0,0,2,0), cex.main=1.75)
corrplot(pca_matrix_p2, method="circle", title = "PC 2", mar=c(0,0,2,0), cex.main=1.75)
dev.off()

# Fig 4: SOM clusters, SOM drivers
source('./src/Functions/coolBlueHotRed.R')

dat_wide = dat %>% 
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

data_train_matrix <- dat_wide %>% 
  select(all_of(vars_order)) %>% 
  scale() %>% 
  as.matrix()

som_grid <- somgrid(xdim = 9, ydim=9, topo="hexagonal")

set.seed(222)
# the 4x4 first?
g <- somgrid(xdim = 4, ydim = 4, topo = "rectangular" )

map <- som(dat_wide %>% 
             select(all_of(vars_order))%>% 
             as.matrix(),
           grid = g,
           alpha = c(0.05, 0.01),
           radius = 1)

plot(map, type='codes',palette.name = rainbow, labels = names, )

plot(map, type='mapping',col = n.colors,
     label = names,pchs = names)

# then 9x9
som_grid <- somgrid(xdim = 7, ydim=7, topo="hexagonal")
som_model <- som(data_train_matrix, 
                 grid=som_grid, 
                 rlen=1000, 
                 alpha=c(0.05,0.01), 
                 keep.data = TRUE)

par(mar=c(5,5,5,5))
plot(som_model, type="changes")
plot(som_model, type="count")

som_cluster <- cutree(hclust(dist(som_model$codes[[1]])),4)
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2', "#7f7f7f", "#bcbd22", "#17becf","#00FF00", "#9F81F7"
                    ,"#FFFF00", "#F6CECE","#610B21")



plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters",
     label = names, pchs = names, cex =0.7) 
add.cluster.boundaries(som_model, som_cluster)

# SI: N vs. S. PCA loading

# SI: all SOM drivers

# others? map? table of lake characteristics?

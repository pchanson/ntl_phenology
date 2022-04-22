# script to analyze and visualize the results of network connectivity analysis
# CDB, 2022-04-24

library(tidyverse)
library(scico)

# read in data
nc_all = read_csv("Data/analysis_ready/network_conn_alllakes.csv") %>% 
  mutate(row =row + 1, column = column + 1) # python is zero indexed 
nc_s = read_csv("Data/analysis_ready/network_conn_Slakes.csv") %>% 
  mutate(row =row + 1, column = column + 1) # python is zero indexed
nc_n = read_csv("Data/analysis_ready/network_conn_Nlakes.csv") %>% 
  mutate(row =row + 1, column = column + 1) # python is zero indexed
dict_all = read_csv("Data/derived/netcon_dictionary_all.csv")
dict_s = read_csv("Data/derived/netcon_dictionary_S.csv")
dict_n = read_csv("Data/derived/netcon_dictionary_N.csv")

# metrics_ordered = c("iceoff", "straton", "chlor_spring", "secchi_openwater", "daphnia_biomass", "doc", "anoxia_summer", "stability", "energy", "chlor_fall", "stratoff", "iceon")
metrics_ordered = c("iceoff", "straton", "chlor_all", "secchi_openwater", "daphnia_biomass", "doc", "anoxia_summer", "stability", "energy", "stratoff", "iceon")

# add the lakes and variables corresponding to each node id
nc_all = left_join(nc_all, dict_all %>% rename(row = node)) %>% 
  rename(lakeid_focal = lakeid, metric_focal = metric) %>% 
  left_join(dict_all %>% rename(column = node)) %>% 
  rename(lakeid_input = lakeid, metric_input = metric) %>% 
  mutate(metric_input = factor(metric_input, levels = metrics_ordered, ordered = T))%>% 
  mutate(metric_focal = factor(metric_focal, levels = metrics_ordered, ordered = T))

nc_s = left_join(nc_s, dict_s %>% rename(row = node)) %>% 
  rename(lakeid_focal = lakeid, metric_focal = metric) %>% 
  left_join(dict_s %>% rename(column = node)) %>% 
  rename(lakeid_input = lakeid, metric_input = metric) %>% 
  mutate(metric_input = factor(metric_input, levels = metrics_ordered, ordered = T))%>% 
  mutate(metric_focal = factor(metric_focal, levels = metrics_ordered, ordered = T))


nc_n = left_join(nc_n, dict_n %>% rename(row = node)) %>% 
  rename(lakeid_focal = lakeid, metric_focal = metric) %>% 
  left_join(dict_n %>% rename(column = node)) %>% 
  rename(lakeid_input = lakeid, metric_input = metric) %>% 
  mutate(metric_input = factor(metric_input, levels = metrics_ordered, ordered = T))%>% 
  mutate(metric_focal = factor(metric_focal, levels = metrics_ordered, ordered = T))

# look at values and set own thresholds
nc_n %>% 
  filter(lakeid_focal == lakeid_input) %>% 
  ggplot(aes(x=value)) +
  geom_histogram(bins=50) +
  facet_wrap(~lakeid_focal) +
  theme_bw() +
  geom_vline(aes(xintercept=inhib_thresh), color="red")+
  geom_vline(aes(xintercept=excit_thresh), color="red") # looks okay

# southern lakes
nc_s %>% 
  filter(lakeid_focal == lakeid_input) %>% 
  ggplot(aes(x=value)) +
  geom_histogram(bins=50, color="black", fill="lightblue") +
  facet_wrap(~lakeid_focal) +
  theme_bw() +
  geom_vline(aes(xintercept=inhib_thresh), color="red")+
  geom_vline(aes(xintercept=excit_thresh), color="red")

nc_s$excit_own = as.numeric(NA)
nc_s$inhib_own = as.numeric(NA)
nc_s = nc_s %>% 
  mutate(excit_own = ifelse(lakeid_focal == "FI", -0.035, excit_own),
         inhib_own = ifelse(lakeid_focal == "FI", 0.075, inhib_own),
         excit_own = ifelse(lakeid_focal == "ME", -0.04, excit_own),
         inhib_own = ifelse(lakeid_focal == "ME", 0.08, inhib_own),
         excit_own = ifelse(lakeid_focal == "MO", -0.05, excit_own),
         inhib_own = ifelse(lakeid_focal == "MO", 0.1, inhib_own),
         excit_own = ifelse(lakeid_focal == "WI", -0.05, excit_own),
         inhib_own = ifelse(lakeid_focal == "WI", 0.05, inhib_own))

nc_s %>% 
  filter(lakeid_focal == lakeid_input) %>% 
  ggplot(aes(x=value)) +
  geom_histogram(bins=50, color="black", fill="lightblue") +
  facet_wrap(~lakeid_focal) +
  theme_bw() +
  geom_vline(aes(xintercept=inhib_thresh), color="red")+
  geom_vline(aes(xintercept=excit_thresh), color="red")+
  geom_vline(aes(xintercept=inhib_own), color="blue")+
  geom_vline(aes(xintercept=excit_own), color="blue")

# northern lakes
nc_n %>% 
  filter(lakeid_focal == lakeid_input) %>% 
  ggplot(aes(x=value)) +
  geom_histogram(bins=50, color="black", fill="lightblue") +
  facet_wrap(~lakeid_focal) +
  theme_bw() +
  geom_vline(aes(xintercept=inhib_thresh), color="red")+
  geom_vline(aes(xintercept=excit_thresh), color="red")

nc_n$excit_own = as.numeric(NA)
nc_n$inhib_own = as.numeric(NA)
nc_n = nc_n %>% 
  mutate(excit_own = ifelse(lakeid_focal == "AL", -0.012, excit_own),
         inhib_own = ifelse(lakeid_focal == "AL", inhib_thresh, inhib_own),
         excit_own = ifelse(lakeid_focal == "BM", -0.01, excit_own),
         inhib_own = ifelse(lakeid_focal == "BM", inhib_thresh, inhib_own),
         excit_own = ifelse(lakeid_focal == "CB", -0.012, excit_own),
         inhib_own = ifelse(lakeid_focal == "CB", inhib_thresh, inhib_own),
         excit_own = ifelse(lakeid_focal == "CR", -0.012, excit_own),
         inhib_own = ifelse(lakeid_focal == "CR", inhib_thresh, inhib_own),
         excit_own = ifelse(lakeid_focal == "SP", -0.01, excit_own),
         inhib_own = ifelse(lakeid_focal == "SP", 0.035, inhib_own),
         excit_own = ifelse(lakeid_focal == "TB", -0.01, excit_own),
         inhib_own = ifelse(lakeid_focal == "TB", inhib_thresh, inhib_own),
         excit_own = ifelse(lakeid_focal == "TR", -0.01, excit_own),
         inhib_own = ifelse(lakeid_focal == "TR", inhib_thresh, inhib_own))

nc_n %>% 
  filter(lakeid_focal == lakeid_input) %>% 
  ggplot(aes(x=value)) +
  geom_histogram(bins=50, color="black", fill="lightblue") +
  facet_wrap(~lakeid_focal) +
  theme_bw() +
  geom_vline(aes(xintercept=inhib_thresh), color="red")+
  geom_vline(aes(xintercept=excit_thresh), color="red")+
  geom_vline(aes(xintercept=inhib_own), color="blue")+
  geom_vline(aes(xintercept=excit_own), color="blue")

# analyze the proportion of excitatory connections
nc_s %>%
  filter(lakeid_focal == lakeid_input) %>% 
  group_by(lakeid_focal) %>% 
  summarise(prop_excit = sum(value < excit_own) / n()) 

nc_n %>%
  filter(lakeid_focal == lakeid_input) %>% 
  group_by(lakeid_focal) %>% 
  summarise(prop_excit = sum(value < excit_own) /n()) 

# try visualizing the number of connections
nc_s %>% 
  filter(lakeid_focal == lakeid_input ) %>% 
  group_by(metric_focal, metric_input) %>% 
  summarise(Excitatory = sum(value < excit_own),
            Inhibitory = sum(value > inhib_own),
            Nonsignif = sum(value > excit_own & value < inhib_own)) %>% 
  pivot_longer(cols = c("Excitatory", "Inhibitory", "Nonsignif")) %>% 
  mutate(value = ifelse(value == 0, NA, value)) %>% 
  ggplot(aes(x=metric_input, y=metric_focal, size=value, color=as.factor(value))) +
  geom_point() + 
  theme_bw()+ 
  # scale_color_viridis_c() +
  facet_wrap(~name)+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Driver Metric", y = "Affected Metric", color="Number", size="Number") +
  ggtitle("Number of southern lakes with excitatory, inhibitory, or nonsig. connections")

# ggsave("Figures/network_connectivity/node_connections_sLakes.png", pS, width=1000, height=450, dpi=120, units = "px")

nc_n %>% 
  filter(lakeid_focal == lakeid_input ) %>% 
  group_by(metric_focal, metric_input) %>% 
  summarise(Excitatory = sum(value < excit_own),
            Inhibitory = sum(value > inhib_own),
            Nonsignif = sum(value > excit_own & value < inhib_own)) %>% 
  pivot_longer(cols = c("Excitatory", "Inhibitory", "Nonsignif")) %>% 
  mutate(value = ifelse(value == 0, NA, value)) %>% 
  ggplot(aes(x=metric_input, y=metric_focal, size=value, color=as.factor(value))) +
  geom_point() + 
  theme_bw()+ 
  # scale_color_viridis_c() +
  facet_wrap(~name)+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Driver Metric", y = "Affected Metric", color="Number", size="Number") +
  ggtitle("Number of northern lakes with excitatory, inhibitory, or nonsig. connections")

# analyze each lake spearately
lakes = c("AL", "BM", "CB", "CR", "FI", "ME", "MO", "SP", "TB", "TR", "WI")
hold_res = list()
for(i in 1:length(lakes)){
  lake_res = read_csv(paste0("Data/analysis_ready/netcon_indiv_lakes/", lakes[i], ".csv")) %>% 
    mutate(row= row+1, column=column+1)
  lake_dict = read_csv(paste0("Data/derived/individual_netcon_data/", lakes[i], "_dictionary.csv"))
  
  lake_all = left_join(lake_res, lake_dict %>% rename(row = node)) %>% 
    rename(lakeid_focal = lakeid, metric_focal = metric) %>% 
    left_join(lake_dict %>% rename(column = node)) %>% 
    rename(lakeid_input = lakeid, metric_input = metric) %>% 
    mutate(metric_input = factor(metric_input, levels = metrics_ordered, ordered = T))%>% 
    mutate(metric_focal = factor(metric_focal, levels = metrics_ordered, ordered = T)) %>% 
    select(lakeid_focal, metric_focal, metric_input, value, inhib_thresh, excit_thresh)
  
  hold_res[[i]] = lake_all
}
indiv_lakes = bind_rows(hold_res)

# set own thresholds
indiv_lakes %>% 
  ggplot(aes(x=value)) +
  geom_histogram(bins=100, color="black", fill="lightblue") +
  facet_wrap(~lakeid_focal, scales="free_x") +
  theme_bw() +
  geom_vline(aes(xintercept=inhib_thresh), color="red")+
  geom_vline(aes(xintercept=excit_thresh), color="red")+
  geom_vline(aes(xintercept=0), color="black")

# try being more restrictive
indiv_lakes$excit_own = as.numeric(NA)
indiv_lakes$inhib_own = as.numeric(NA)
indiv_lakes = indiv_lakes %>% 
  mutate(excit_own = ifelse(lakeid_focal == "AL", -0.18, excit_own),
         inhib_own = ifelse(lakeid_focal == "AL", 0.5, inhib_own),
         excit_own = ifelse(lakeid_focal == "BM", -0.25, excit_own),
         inhib_own = ifelse(lakeid_focal == "BM", 0.8, inhib_own),
         excit_own = ifelse(lakeid_focal == "CB", -0.25, excit_own),
         inhib_own = ifelse(lakeid_focal == "CB", 0.5, inhib_own),
         excit_own = ifelse(lakeid_focal == "CR", excit_thresh, excit_own),
         inhib_own = ifelse(lakeid_focal == "CR", 0.3, inhib_own),
         excit_own = ifelse(lakeid_focal == "FI", -0.55, excit_own),
         inhib_own = ifelse(lakeid_focal == "FI", 1.8, inhib_own),
         excit_own = ifelse(lakeid_focal == "ME", -0.95, excit_own),
         inhib_own = ifelse(lakeid_focal == "ME", 1, inhib_own),
         excit_own = ifelse(lakeid_focal == "MO", excit_thresh, excit_own),
         inhib_own = ifelse(lakeid_focal == "MO", 1, inhib_own),
         excit_own = ifelse(lakeid_focal == "SP", -0.25, excit_own),
         inhib_own = ifelse(lakeid_focal == "SP", 0.53, inhib_own),
         excit_own = ifelse(lakeid_focal == "TB", excit_thresh, excit_own),
         inhib_own = ifelse(lakeid_focal == "TB", 0.75, inhib_own),
         excit_own = ifelse(lakeid_focal == "TR", -0.25, excit_own),
         inhib_own = ifelse(lakeid_focal == "TR", inhib_thresh, inhib_own),
         excit_own = ifelse(lakeid_focal == "WI", -0.5, excit_own),
         inhib_own = ifelse(lakeid_focal == "WI", 2, inhib_own))

indiv_lakes %>% 
  ggplot(aes(x=value)) +
  geom_histogram(bins=100, color="black", fill="lightblue") +
  facet_wrap(~lakeid_focal, scales="free_x") +
  theme_bw() +
  geom_vline(aes(xintercept=inhib_thresh), color="red")+
  geom_vline(aes(xintercept=excit_thresh), color="red")+
  geom_vline(aes(xintercept=inhib_own), color="blue")+
  geom_vline(aes(xintercept=excit_own), color="blue")



indiv_lakes$connection_type = "NA"
indiv_lakes = indiv_lakes %>% 
  mutate(connection_type = ifelse(connection_type == "NA" & value < excit_own, "excitatory", connection_type)) %>% 
  mutate(connection_type = ifelse(connection_type == "NA" & value > inhib_own, "inhibitory", connection_type)) %>% 
  mutate(connection_type = ifelse(connection_type == "NA", "nonsignif.", connection_type))

indiv_lakes$metric_focal = factor(indiv_lakes$metric_focal, levels=rev(metrics_ordered), ordered = T)
indiv_lakes$metric_input = factor(indiv_lakes$metric_input, levels=metrics_ordered, ordered = T)

lakes_ordered = c("FI", "ME", "MO", "WI", "AL", "TR", "BM", "CR", "SP", "CB", "TB")
indiv_lakes$lakeid_focal = factor(indiv_lakes$lakeid_focal, levels = lakes_ordered, ordered=T)

prop_excit = indiv_lakes %>% 
  group_by(lakeid_focal) %>% 
  summarise(N_excit = round(sum(connection_type == "excitatory") / n() * 100, 1)) %>% 
  arrange(desc(N_excit)) %>% 
  mutate(labs = paste0(lakeid_focal, " (",N_excit, "% excit.)"))

indiv_lakes %>% 
  left_join(prop_excit) %>% 
  mutate(lakeid_focal = factor(lakeid_focal, levels = prop_excit$lakeid_focal, labels = prop_excit$labs, ordered=T)) %>% 
  group_by(metric_focal, metric_input) %>% 
  ggplot(aes(x=metric_input, y=metric_focal, fill=connection_type)) +
  geom_tile(color="black") + 
  theme_bw()+ 
  # scale_fill_viridis_d() +
  scale_fill_manual(values=c("excitatory" = "#3399FF", "inhibitory" = "#FF3333", "nonsignif." = "grey96"))+
  facet_wrap(~lakeid_focal)+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Driver Metric", y = "Affected Metric", color="Number", size="Number") +
  ggtitle("Excitatory, inhibitory, or nonsig. connections by lake") +
  theme(strip.text=element_text(size=10), plot.title = element_text(size=16))

# ggsave("Figures/network_connectivity/node_connections_indivAnalyzed.png", pI, width=1000, height=700, dpi=120, units = "px")

comb_indiv_lakes_all = indiv_lakes %>% 
  group_by(metric_focal, metric_input) %>% 
  summarise(N_excit = sum(connection_type == "excitatory"), 
            N_inhib = sum(connection_type == "inhibitory"),
            N_nonsig = sum(connection_type == "nonsignif.")) %>% 
  pivot_longer(cols = c("N_excit", "N_inhib", "N_nonsig")) %>% 
  mutate(name = str_remove(name, "N_"),
         metric_focal = factor(metric_focal, rev(metrics_ordered), ordered = T), 
         metric_input = factor(metric_input, metrics_ordered, ordered = T)) %>% 
  ungroup()

pIE = comb_indiv_lakes_all %>% 
  filter(name == "excit") %>% 
  ggplot(aes(x=metric_input, y=metric_focal, fill=value)) +
  geom_tile(color="black") + 
  theme_bw()+ 
  scale_fill_scico(palette="imola") +
  # scale_fill_manual(values=c("excitatory" = "#3399FF", "inhibitory" = "#FF3333", "nonsignif." = "grey96"))+
  # facet_wrap(~name)+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Driver Metric", y = "Affected Metric", color="Number", size="Number") +
  ggtitle("Excitatory connections across all lakes") +
  theme( plot.title = element_text(size=14)) +
  labs(fill="N lakes")

# ggsave("Figures/network_connectivity/excit_connections_indivAnalyzed_allLakes.png", pIE, width=600, height=550, dpi=120, units = "px")

pIE_s = indiv_lakes %>% 
  filter(lakeid_focal %in% c("FI", "MO", "ME", "WI")) %>% 
  group_by(metric_focal, metric_input) %>% 
  summarise(N_excit = sum(connection_type == "excitatory"), 
            N_inhib = sum(connection_type == "inhibitory"),
            N_nonsig = sum(connection_type == "nonsignif.")) %>% 
  pivot_longer(cols = c("N_excit", "N_inhib", "N_nonsig")) %>% 
  mutate(name = str_remove(name, "N_"),
         metric_focal = factor(metric_focal, rev(metrics_ordered), ordered = T), 
         metric_input = factor(metric_input, metrics_ordered, ordered = T)) %>% 
  ungroup() %>% 
  filter(name == "excit") %>% 
  ggplot(aes(x=metric_input, y=metric_focal, fill=value)) +
  geom_tile(color="black") + 
  theme_bw()+ 
  scale_fill_scico(palette="imola") +
  # scale_fill_manual(values=c("excitatory" = "#3399FF", "inhibitory" = "#FF3333", "nonsignif." = "grey96"))+
  # facet_wrap(~name)+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Driver Metric", y = "Affected Metric", color="Number", size="Number") +
  ggtitle("Excitatory connections across S lakes") +
  theme( plot.title = element_text(size=14)) +
  labs(fill="N lakes")

# ggsave("Figures/network_connectivity/excit_connections_indivAnalyzed_sLakes.png", pIE_s, width=600, height=550, dpi=120, units = "px")

pIE_n = indiv_lakes %>% 
  filter(!(lakeid_focal %in% c("FI", "MO", "ME", "WI"))) %>% 
  group_by(metric_focal, metric_input) %>% 
  summarise(N_excit = sum(connection_type == "excitatory"), 
            N_inhib = sum(connection_type == "inhibitory"),
            N_nonsig = sum(connection_type == "nonsignif.")) %>% 
  pivot_longer(cols = c("N_excit", "N_inhib", "N_nonsig")) %>% 
  mutate(name = str_remove(name, "N_"),
         metric_focal = factor(metric_focal, rev(metrics_ordered), ordered = T), 
         metric_input = factor(metric_input, metrics_ordered, ordered = T)) %>% 
  ungroup() %>% 
  filter(name == "excit") %>% 
  ggplot(aes(x=metric_input, y=metric_focal, fill=value)) +
  geom_tile(color="black") + 
  theme_bw()+ 
  scale_fill_scico(palette="imola") +
  # scale_fill_manual(values=c("excitatory" = "#3399FF", "inhibitory" = "#FF3333", "nonsignif." = "grey96"))+
  # facet_wrap(~name)+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Driver Metric", y = "Affected Metric", color="Number", size="Number") +
  ggtitle("Excitatory connections across N lakes") +
  theme( plot.title = element_text(size=14)) +
  labs(fill="N lakes")

# ggsave("Figures/network_connectivity/excit_connections_indivAnalyzed_nLakes.png", pIE_n, width=600, height=550, dpi=120, units = "px")

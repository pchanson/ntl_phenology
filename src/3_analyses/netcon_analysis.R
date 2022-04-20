# script to analyze and visualize the results of network connectivity analysis
# CDB, 2022-04-24

library(tidyverse)

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

metrics_ordered = c("iceoff", "straton", "chlor_spring", "secchi_openwater", "daphnia_biomass", "doc", "anoxia_summer", "stability", "energy", "chlor_fall", "stratoff", "iceon")

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



# analyze the proportion of excitatory connections
nc_all %>%
  filter(lakeid_focal == lakeid_input) %>% 
  group_by(lakeid_focal) %>% 
  summarise(prop_excit = sum(value < excit_thresh) / n()) 

nc_s %>%
  filter(lakeid_focal == lakeid_input) %>% 
  group_by(lakeid_focal) %>% 
  summarise(prop_excit = sum(value < excit_thresh) / n()) 

nc_n %>%
  filter(lakeid_focal == lakeid_input) %>% 
  group_by(lakeid_focal) %>% 
  summarise(prop_excit = sum(value < excit_thresh) /n()) 

# try visualizing the number of connections
nc_s %>% 
  filter(lakeid_focal == lakeid_input ) %>% 
  group_by(metric_focal, metric_input) %>% 
  summarise(Excitatory = sum(value < excit_thresh),
            Inhibitory = sum(value > inhib_thresh),
            Nonsignif = sum(value > excit_thresh & value < inhib_thresh)) %>% 
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

nc_n %>% 
  filter(lakeid_focal == lakeid_input ) %>% 
  group_by(metric_focal, metric_input) %>% 
  summarise(Excitatory = sum(value < excit_thresh),
            Inhibitory = sum(value > inhib_thresh),
            Nonsignif = sum(value > excit_thresh & value < inhib_thresh)) %>% 
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

nc_all %>% 
  filter(lakeid_focal == lakeid_input ) %>% 
  group_by(metric_focal, metric_input) %>% 
  summarise(Excitatory = sum(value < excit_thresh),
            Inhibitory = sum(value > inhib_thresh),
            Nonsignif = sum(value > excit_thresh & value < inhib_thresh)) %>% 
  pivot_longer(cols = c("Excitatory", "Inhibitory", "Nonsignif")) %>% 
  mutate(value = ifelse(value == 0, NA, value)) %>% 
  ggplot(aes(x=metric_input, y=metric_focal, size=value, color=as.factor(value))) +
  geom_point() + 
  theme_bw()+ 
  # scale_color_viridis_c() +
  facet_wrap(~name)+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Driver Metric", y = "Affected Metric", color="Number", size="Number") +
  ggtitle("Number of all lakes with excitatory, inhibitory, or nonsig. connections")

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
indiv_lakes$connection_type = "NA"
indiv_lakes = indiv_lakes %>% 
  mutate(connection_type = ifelse(connection_type == "NA" & value < excit_thresh, "excitatory", connection_type)) %>% 
  mutate(connection_type = ifelse(connection_type == "NA" & value > inhib_thresh, "inhibitory", connection_type)) %>% 
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
  theme(strip.text=element_text(size=14), plot.title = element_text(size=18))

# look at filling chl time series with temp and DO
library(tidyverse)
library(lubridate)

dat0 = read_csv("Data/raw/chl_from_server_files.csv")
dat = dat0 %>% 
  rename(lakeid = Lake, sampledate = `Sample Date`, depth = `Depth (m)`,
         corrected_chlor_fluor = `corrected chloro a`, uncorr_chlor_fluor = `uncor. chloro a`) %>% 
  filter(depth == "0-2" & lakeid %in% c("FI", "ME", "MO", "WI")) %>% 
  mutate(sampledate = mdy(sampledate), year=year(sampledate), daynum=yday(sampledate)) %>% 
  group_by(lakeid, sampledate, depth, year, daynum) %>% 
  summarise(across(c(corrected_chlor_fluor, uncorr_chlor_fluor), mean, na.rm=T)) %>% 
  filter(year %in% 2002:2004)

dat %>% 
  ggplot(aes(x=sampledate, y=uncorr_chlor_fluor)) +
  geom_point() +
  geom_line() +
  facet_wrap(~lakeid) +
  theme_bw()


# get rid of the outliers
dat_analyze = dat %>% 
  filter((lakeid == "FI" & uncorr_chlor_fluor < 7.5) |
           (lakeid == "ME" & uncorr_chlor_fluor < 2.55) |
           (lakeid == "MO" & uncorr_chlor_fluor < 5) |
           (lakeid == "WI" & uncorr_chlor_fluor < 17.5) )


dat_analyze %>% 
  ggplot(aes(x=sampledate, y=uncorr_chlor_fluor)) +
  geom_point() +
  geom_line() +
  facet_wrap(~lakeid) +
  theme_bw()

# POTENTIAL TO-DO: limit dates to after iceoff


peaks_spring = dat_analyze %>% 
  filter(daynum <= 175) %>% 
  group_by(lakeid, year) %>%
  slice_max(uncorr_chlor_fluor) %>% 
  mutate(metric = "chlor_spring") %>% 
  select(lakeid, year, metric, sampledate, daynum)

peaks_fall = dat_analyze %>% 
  filter(daynum > 175) %>% 
  group_by(lakeid, year) %>%
  slice_max(uncorr_chlor_fluor) %>% 
  mutate(metric = "chlor_fall") %>% 
  select(lakeid, year, metric, sampledate, daynum)

peaks_all = dat_analyze %>% 
  group_by(lakeid, year) %>%
  slice_max(uncorr_chlor_fluor) %>% 
  mutate(metric = "chlor_all") %>% 
  select(lakeid, year, metric, sampledate, daynum)
  

peaks_out = bind_rows(peaks_spring, peaks_fall, peaks_all) %>% 
  arrange(lakeid, year, metric)

write_csv(peaks_out, "Data/derived/chl_filled_peaks.csv")




# ======================================================================================
# code below was for fitting chl~wtemp+o2 model; instead using "bad" chl values above
# ======================================================================================

# library(xgboost)
# library(caret)

# 
# # read in data
# inUrl3 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/29/29/03e232a1b362900e0f059859abe8eb97"
# infile3 <- tempfile()
# download.file(inUrl3, infile3, method = "libcurl")
# tempdo0 <- read_csv(infile3) 
# chl0 =  read_csv( "Data/raw/ntl38_v5.csv")
# thermo = read_csv("Data/derived/thermocline.csv")
# 
# # clean up data
# tempdo = tempdo0 %>% 
#   filter(lakeid %in% c("FI", "ME", "MO", "WI") & depth == 0)
# 
# chl_want = chl0 %>% 
#   filter(depth_range_m == "0-2")
# 
# chl_means = chl_want %>% 
#   group_by(lakeid, year4, sampledate) %>% 
#   summarise(correct_chl_fluor = mean(correct_chl_fluor, na.rm=T)) %>% 
#   ungroup() %>% 
#   filter(!is.na(correct_chl_fluor)) %>% 
#   mutate(sampledate = lubridate::mdy(sampledate)) %>% 
#   mutate(daynum = lubridate::yday(sampledate))
# 
# # combine
# comb = full_join(chl_means, tempdo) %>% left_join(thermo)
# 
# # fit regression
# lm_sat1 = lm(correct_chl_fluor~wtemp+lakeid+o2sat, data=comb) # thermdepth_m doesn't help
# summary(lm_sat1)
# lm_sat2 = lm(correct_chl_fluor~(wtemp+o2sat)*lakeid, data=comb) # thermdepth_m doesn't help
# summary(lm_sat2)
# 
# lm_conc1 = lm(correct_chl_fluor~wtemp+lakeid+o2, data=comb) # thermdepth_m doesn't help
# summary(lm_conc1)
# lm_conc2 = lm(correct_chl_fluor~(wtemp+o2)*lakeid, data=comb) # thermdepth_m doesn't help
# summary(lm_conc2)
# 
# # create modeled TS
# comb$mod_chl_sat1 = predict(lm_sat1, comb)
# comb$mod_chl_sat2 = predict(lm_sat2, comb)
# comb$mod_chl_conc1 = predict(lm_conc1, comb)
# comb$mod_chl_conc2 = predict(lm_conc2, comb)
# 
# cor(comb[, c("correct_chl_fluor", "mod_chl_sat1", "mod_chl_sat2", "mod_chl_conc1", "mod_chl_conc2")], use="complete.obs")
# p1 = comb %>% 
#   ggplot(aes(x=correct_chl_fluor, y=mod_chl_sat2, color=lakeid)) +
#   geom_point() +
#   theme_bw() +
#   ggtitle("model = Chl ~ \n (wtemp(0m) + o2sat(0m)) * lakeid") + 
#   labs(x="observed chl", y="modeled chl")
# # pull out peaks
# actual_peaks = comb %>% 
#   group_by(lakeid, year4) %>% 
#   slice_max(correct_chl_fluor) %>% 
#   select(lakeid, year4, daynum) %>% 
#   rename(daynum_obs = daynum)
# 
# modeled_peaks = comb %>% 
#   group_by(lakeid, year4) %>% 
#   slice_max(mod_chl_sat2) %>% 
#   select(lakeid, year4, daynum) %>% 
#   rename(daynum_mod = daynum)
# 
# # compare modeled to actual peaks
# results = full_join(actual_peaks, modeled_peaks) 
# 
# cor = results %>% 
#   group_by(lakeid) %>% 
#   summarise(r = paste("r =", round(cor(daynum_obs, daynum_mod, use="complete.obs"), 3)))
# 
# p2 = results %>% 
#   ggplot(aes(x=daynum_obs, y=daynum_mod)) +
#   geom_point() + 
#   facet_wrap(~lakeid) + 
#   theme_bw() +
#   ggtitle("chl peak date comparison") +
#   labs(x="observed", "modeled") + 
#   geom_text(aes(label=r), data=cor, x=100, y=325)
# 
# gridExtra::grid.arrange(p1, p2, ncol=2, top="linear regression")
# 
# # save mod_chl_conc2 spring, fall, and all peaks
# peaks_all = comb %>% 
#   group_by(lakeid, year4) %>% 
#   summarise(chlor_all = daynum[which.max(mod_chl_conc2)])
# peaks_spring = comb %>% 
#   filter(daynum <= 175) %>% 
#   group_by(lakeid, year4) %>% 
#   summarise(chlor_spring = daynum[which.max(mod_chl_conc2)])
# peaks_fall = comb %>% 
#   filter(daynum > 175) %>% 
#   group_by(lakeid, year4) %>% 
#   summarise(chlor_fall = daynum[which.max(mod_chl_conc2)])
# 
# peaks_out = full_join(peaks_all, peaks_spring) %>% 
#   full_join(peaks_fall) %>% 
#   pivot_longer(cols=c("chlor_all", "chlor_spring", "chlor_fall"), 
#                names_to="metric",
#                values_to="daynum") %>% 
#   rename(year=year4) %>% 
#   mutate(sampledate = as.Date(paste0(year - 1, "-12-31")) + daynum)
# 
# write_csv(peaks_out, "Data/derived/chl_modeled_peaks.csv")
# 
# 
# # try XGBoost
# set.seed(10)
# comb_hold = na.omit(comb[, c("lakeid", "sampledate", "year4", "daynum", "wtemp", "o2", "o2sat", "thermdepth_m", "correct_chl_fluor")])
# inds = createDataPartition(comb_hold$correct_chl_fluor, p = 0.5, list=F)
# train = comb_hold[inds, ]
# test = comb_hold[-inds,]
# 
# train_x = data.matrix(train[, c("lakeid", "wtemp", "o2sat")])
# train_y = data.matrix(train[,"correct_chl_fluor"])
# 
# test_x = data.matrix(test[,  c("lakeid", "wtemp", "o2sat")])
# test_y = data.matrix(test[, "correct_chl_fluor"])
# 
# xgb_train = xgb.DMatrix(data = train_x, label = train_y)
# xgb_test = xgb.DMatrix(data = test_x, label = test_y)
# 
# xgbc = xgboost(data = xgb_train, max.depth = 2, nrounds = 50, subsample=0.5)
# # print(xgbc)
# 
# pred_y = predict(xgbc, xgb_test)
# plot(test_y, pred_y);abline(0,1, col="red")
# summary(lm(test_y~pred_y))
# mse = mean((test_y - pred_y)^2)
# mae = caret::MAE(test_y, pred_y)
# rmse = caret::RMSE(test_y, pred_y)
# 
# cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)
# x = 1:length(test_y)
# plot(x, test_y, col = "red", type = "l")
# lines(x, pred_y, col = "blue", type = "l")
# legend(x = 1, y = 38,  legend = c("original test_y", "predicted test_y"), 
#        col = c("red", "blue"), box.lty = 1, cex = 0.8, lty = c(1, 1))
# 
# # try xgboost peaks
# comb_hold$mod_chl_sat = predict(xgbc, data.matrix(comb_hold[, c("lakeid", "wtemp", "o2sat")]))
# p1x = comb_hold %>% 
#   ggplot(aes(x=correct_chl_fluor, y=mod_chl_sat, color=lakeid)) +
#   geom_point() +
#   theme_bw() +
#   ggtitle("model = \n xgboost(wtemp(0m), o2sat(0m)), lakeid") + 
#   labs(x="observed chl", y="modeled chl")
# 
# actual_peaks = comb_hold %>% 
#   group_by(lakeid, year4) %>% 
#   slice_max(correct_chl_fluor) %>% 
#   select(lakeid, year4, daynum) %>% 
#   rename(daynum_obs = daynum)
# 
# modeled_peaks = comb_hold %>% 
#   group_by(lakeid, year4) %>% 
#   slice_max(mod_chl_sat) %>% 
#   select(lakeid, year4, daynum) %>% 
#   rename(daynum_mod = daynum)
# 
# # compare modeled to actual peaks
# results = full_join(actual_peaks, modeled_peaks) 
# 
# cor = results %>% 
#   group_by(lakeid) %>% 
#   summarise(r = paste("r =", round(cor(daynum_obs, daynum_mod), 3)))
# 
# p2x = results %>% 
#   ggplot(aes(x=daynum_obs, y=daynum_mod)) +
#   geom_point() + 
#   facet_wrap(~lakeid) + 
#   theme_bw() +
#   ggtitle("chl peak date comparison") +
#   labs(x="observed", y="modeled") + 
#   geom_text(aes(label=r), data=cor, x=100, y=275)
# 
# gridExtra::grid.arrange(p1x, p2x, ncol=2, top="XGBoost")
# 

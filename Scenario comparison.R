#Scenario model for "Harvest Control Rules for Tuna in the WCPFC: Implications for Indonesia, Philippines, and Vietnam" (Bailey, Willis, et al. 2023) for WWF Sustainable Tuna Partnership
#Code by Ciara Willis (2023) with assistance from Mike Spence (author of LeMaRns package)

#compare scenarios at equilibrium

#setup-----
library(LeMaRns)
library(tidyverse)
library(reshape)
library(ggsci)
library(scales)

txts=10
txtl=15
theme_set(theme_gray())
theme_replace(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA),
              axis.title.x = element_text(colour = "black", size = txtl),
              axis.title.y = element_text(colour = "black", size = txtl, angle = 90),
              axis.text.x = element_text(colour = "black", size = txts), 
              axis.text.y = element_text(colour = "black", size = txts),
              legend.title =  element_text(colour = "black", size = txtl),
              legend.text =  element_text(colour = "black", size = txts),
              strip.text = element_text(size=txtl),
              strip.background = element_rect(color="black", fill=NA))

gearorder = c("INLL", "INPS", "INHL", "INSSS", "INPL",  "PHPS", "PHHL", "PHSSS",  "VNLL", "VNPS", "VNSSS", "otherLL", "otherPS")
speciesorder = c("SKJ", "YFT", "BET")

colgear = c(pal_npg("nrc", alpha = 1)(10), "#7ac143", "navy", "#ffdd00")
colspecies = c("#4DBBD5FF", "#ffc20e", "#00A087FF")
colscenario = pal_npg("nrc")(9)[c(5,2:4)]



#SSB and total catch as % of real reported values----
SSB_base = read.csv("figures/basemodel_SSB_totalcatch_vreal_bysp.csv")
SSB_base$scenario = "Base"
SSB_FAD = read.csv("figures/FADreduction_SSB_totalcatch_vreal_bysp.csv")
SSB_FAD$scenario = "FADreduction"
SSB_prey = read.csv("figures/PreySust_SSB_totalcatch_vreal_bysp.csv")
SSB_prey$scenario = "PreySust"
SSB_BET_YFT = read.csv("figures/BET_YFT_SSB_totalcatch_vreal_bysp.csv")
SSB_BET_YFT$scenario = "BET_YFT"


SSB_all = rbind(SSB_base, SSB_FAD, SSB_prey, SSB_BET_YFT)
SSB_allm = melt(SSB_all, id.vars = c("X", "scenario"))
colnames(SSB_allm) = c("type", "scenario", "species", "value")
head(SSB_allm)

ggplot(SSB_allm, aes(scenario, value, fill = species))+
  facet_wrap(.~type, scales = "free")+
  theme(axis.text.x = element_text(angle = 45),
        axis.title.x = element_blank()
        )+
  geom_col(position = "dodge")+
  scale_fill_manual(values = colspecies)+
  labs(y = "% of reported value", fill = "Species") #x = "Scenario", 
ggsave("figures/scenariocomparison_SSB_catch_percentreal.png", height = 7, width = 9, units = "in")

ggplot(SSB_allm[which(SSB_allm$scenario!="Base"),], aes(scenario, value, fill = species))+
  facet_wrap(.~type, scales = "free")+
  theme(axis.text.x = element_text(angle = 45),
        axis.title.x = element_blank()
  )+
  geom_hline(yintercept = 100)+
  geom_col(position = "dodge")+
  scale_fill_manual(values = colspecies)+
  
  labs(y = "% of reported value", fill = "Species") #x = "Scenario", 
ggsave("figures/scenariocomparison_SSB_catch_percentreal_pres.png", height = 4, width = 7, units = "in")


  

#Catch by sp and gear----
catch_base = read.csv("figures/basemodel_model_vs_realcatches.csv")
catch_base$scenario = "Base"
catch_FAD = read.csv("figures/FADreduction_model_vs_realcatches.csv")
catch_FAD$scenario = "FADreduction"
catch_prey = read.csv("figures/PreySust_model_vs_realcatches.csv")
catch_prey$scenario = "PreySust"
catch_BET_YFT = read.csv("figures/BET_YFT_model_vs_realcatches.csv")
catch_BET_YFT$scenario = "BET_YFT"


catch_all = rbind(catch_base, catch_FAD, catch_prey, catch_BET_YFT)
head(catch_all)
catch_all$gear = factor(catch_all$gear, levels = gearorder)
catch_all$species = factor(catch_all$species, levels = speciesorder)

# ggplot(catch_all, aes(gear, modelcatch, fill = species))+
#   facet_wrap(.~scenario)+
#   geom_col()
# 
# ggplot(catch_all, aes(scenario, modelcatch, fill = gear))+
#   # facet_wrap(.~scenario)+
#   geom_col(position = "dodge")
# 
# ggplot(catch_all, aes(gear, modelcatch, fill = scenario))+
#   # facet_wrap(.~scenario)+
#   geom_col(position = "dodge")
# 
# ggplot(catch_all, aes(gear, modelcatch, fill = scenario))+
#   # facet_wrap(.~gear)+
#   geom_col(position = "dodge")

ggplot(catch_all, aes(scenario, modelcatch, fill = species))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90),
        legend.position = c(.32,.08)
        )+
  facet_wrap(.~gear, scales = "free")+
  geom_col()+
  scale_fill_manual(values = colspecies)+
  labs(title = "Catch (mt)")
ggsave("figures/scenariocomparison_catchbygear.png", height = 10, width = 9, units = "in")


# ggplot(catch_all, aes(species, modelcatch, fill = gear))+
#   # facet_grid(scenario~species, scales = "free")+
#   facet_grid(species~scenario, scales = "free")+
#   geom_col()+
#   scale_fill_manual(values = colgear)+
#   scale_y_continuous(labels = scientific)+
#   labs(y = "Catch (mt)")

ggplot(catch_all, aes(scenario, modelcatch, fill = gear))+
  # facet_grid(scenario~species, scales = "free")+
  facet_wrap(.~species, scales = "free")+
  geom_col()+
  scale_fill_manual(values = colgear)+
  scale_y_continuous(labels = scientific)+
  labs(y = "Catch (mt)")
ggsave("figures/scenariocomparison_catchbyspecies.png", height = 7, width = 15, units = "in")


ggplot(catch_all, aes(scenario, modelcatch, fill= species))+
  # facet_grid(scenario~species, scales = "free")+
  # facet_wrap(.~species, scales = "free")+
  geom_col()+
  scale_fill_manual(values = colspecies)+
  scale_y_continuous(labels = scientific)+
  labs(y = "Catch (mt)")
ggsave("figures/scenariocomparison_catchbyspeciestotal.png", height = 7, width = 15, units = "in")

#BET_YFT rebuild: lower PS efforts reduce their catch while increasing LL's efficiency and thus catch (presumably)


catch_all[which(catch_all$gear=="otherPS"),]
catch_all[which(catch_all$gear=="otherLL"),]


#Rent per gear----
rentg_base = read.csv("figures/basemodel_modeloutput_tuna_econ.csv")
rentg_base$scenario = "Base"
rentg_FAD = read.csv("figures/FADreduction_modeloutput_tuna_econ.csv")
rentg_FAD$scenario = "FADreduction"
rentg_prey = read.csv("figures/PreySust_modeloutput_tuna_econ.csv")
rentg_prey$scenario = "PreySust"
rentg_BET_YFT = read.csv("figures/BET_YFT_modeloutput_tuna_econ.csv")
rentg_BET_YFT$scenario = "BET_YFT"

rentg_all = rbind(rentg_base, rentg_FAD, rentg_prey, rentg_BET_YFT)
head(rentg_all)
rentg_all$gear = factor(rentg_all$gear, levels = gearorder)


# ggplot(rentg_all, aes(scenario, profit))+ #fill = gear
#   facet_wrap(.~gear, scales = "free")+
#   geom_col()+
#   scale_fill_manual(values = colgear)
# 
# 
# ggplot(rentg_all, aes(scenario, profit, fill = gear))+
#   # facet_wrap(.~gear, scales = "free")+
#   geom_col()+
#   scale_fill_manual(values = colgear)

ggplot(rentg_all, aes(gear, profit, fill = scenario))+
  theme(axis.title.x = element_blank(),
        legend.title = element_blank()
        )+
  # facet_wrap(.~gear, scales = "free")+
  geom_col(position = "dodge")+
  scale_fill_manual(values = colscenario)+
  # scale_fill_npg()+
  labs(y = "Rent (USD)")
ggsave("figures/scenariocomparison_profitbygear.png", height = 7, width = 15, units = "in")
ggsave("figures/scenariocomparison_profitbygear_pres.png", height = 2.5, width = 9, units = "in")

ggplot(rentg_all, aes(scenario, profit, fill = gear))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
        )+
  facet_wrap(.~scenario, scales = "free_x")+
  geom_col(position = "dodge")+
  scale_fill_manual(values = colgear)
ggsave("figures/scenariocomparison_profitbygear_minimalist.png", height = 7, width = 15, units = "in")

# ggplot(rentg_all, aes(gear, profit, fill = gear))+
#   facet_wrap(.~scenario, scales = "fixed")+
#   geom_col(position = "dodge")+
#   scale_fill_manual(values = colgear)


#Rent per gear as a % of base-----
rentg_allb = rentg_all[which(rentg_all$scenario=="Base"),]
rentg_alls = rentg_all[which(rentg_all$scenario!="Base"),]

rentg_allb3 = rbind(rentg_allb,rentg_allb,rentg_allb)

dim(rentg_allb3)
dim(rentg_alls)

rentg_alls$RentPercent = (rentg_alls$profit / rentg_allb3$profit)*100


ggplot(rentg_alls, aes(gear, RentPercent, fill = scenario))+
  theme(axis.title.x = element_blank(),
        legend.title = element_blank()
  )+
  geom_hline(yintercept = 100)+
  # facet_wrap(.~gear, scales = "free")+
  geom_col(position = "dodge")+
  scale_fill_manual(values = colscenario[2:4])+
  # scale_fill_npg()+
  labs(y = "Rent as % of base model \n")
ggsave("figures/scenariocomparison_profitbygear_presprecent.png", height = 2.5, width = 9, units = "in")

#Rent per country----
rentc_base = read.csv("figures/basemodel_modeloutput_tuna_econ_countrysum.csv")
rentc_base$scenario = "Base"
rentc_FAD = read.csv("figures/FADreduction_modeloutput_tuna_econ_countrysum.csv")
rentc_FAD$scenario = "FADreduction"
rentc_prey = read.csv("figures/PreySust_modeloutput_tuna_econ_countrysum.csv")
rentc_prey$scenario = "PreySust"
rentc_BET_YFT = read.csv("figures/BET_YFT_modeloutput_tuna_econ_countrysum.csv")
rentc_BET_YFT$scenario = "BET_YFT"

rentc_all = rbind(rentc_base, rentc_FAD, rentc_prey, rentc_BET_YFT)
head(rentc_all)
colnames(rentc_all)[1] = "country"

ggplot(rentc_all, aes(country, profit, fill = scenario))+
  theme(axis.title.x = element_blank())+
  geom_col(position = "dodge")+
  scale_fill_manual(values = colscenario)+
  labs(y = "Rent (USD)")
ggsave("figures/scenariocomparison_profitbycountry.png", height = 4, width = 5, units = "in")




#estimate fuel use----
#Tuna longliners have the highest consumption of fuel per ton of catch: on average over four times as much as purse seiners. Small-scale fisheries fall between the two, consuming about twice as much fuel per ton as seiners(Gillett, 2009)

#assume PL, HL fall under "SSF" for this purpose

#use the ratio of Gillettfuelindex/catch * catch/effort to estimate fuel/effort (see google sheet)

fuel_base = read.csv("figures/basemodel_fuel.csv")
fuel_base$scenario = "Base"
fuel_FAD = read.csv("figures/FADreduction_fuel.csv")
fuel_FAD$scenario = "FADreduction"
fuel_prey = read.csv("figures/PreySust_fuel.csv")
fuel_prey$scenario = "PreySust"
fuel_BET_YFT = read.csv("figures/BET_YFT_fuel.csv")
fuel_BET_YFT$scenario = "BET_YFT"


fuel_all = rbind(fuel_base, fuel_FAD, fuel_prey, fuel_BET_YFT)
head(fuel_all)
fuel_all$gear = factor(fuel_all$gear, levels = gearorder)

head(fuel_all)

fuel_all$fuelindex = fuel_all$effort * fuel_all$fuelunit


ggplot(fuel_all, aes(scenario, fuelindex, fill = gear))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
        )+
  geom_col()+
  scale_fill_manual(values = colgear)+
  labs(y = "Fuel index", fill = "Gear")
ggsave("figures/scenariocomparison_fuelindex.png", height = 7, width = 7, units = "in")
ggsave("figures/scenariocomparison_fuelindex_pres.png", height = 5, width = 4, units = "in")




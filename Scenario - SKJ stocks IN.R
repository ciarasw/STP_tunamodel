#Scenario model for "Harvest Control Rules for Tuna in the WCPFC: Implications for Indonesia, Philippines, and Vietnam" (Bailey, Willis, et al. 2023) for WWF Sustainable Tuna Partnership
#Code by Ciara Willis (2023) with assistance from Mike Spence (author of LeMaRns package)

#scenario: Objective: Rebuild SKJ stocks

# Scenario setup:
#decrease efforts for all gears that catch large quantities of SKJ in
#1) IN only
#2) IN & PH
#3) IN, PH, & VN

#setup----
library(LeMaRns)
library(tidyverse)
library(reshape)
library(ggsci)
library(scales)

load("code/Base_model_LHparams.RData")
load("code/base_model_N0.RData")

scenario = "SKJ_IN"


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



#catchabilities----
#gear order: IN (LL, PS, HL, SSS, PL), PH (PS, HL, SSS), VN (LL, PS, SSS), otherLL, otherPS

#INLL=vector(),INPS=vector(),INHL=vector(),INSSS=vector(),INPL=vector(),
# PHPS=vector(),PHHL=vector(),PHSSS=vector(),
# VNLL=vector(),VNPS=vector(),VNSSS=vector(),

#vulnerabilities by gear type
va4 = data.frame(
  LL=vector(),PS=vector(),HL=vector(),SSS=vector(),PL=vector(),
  sp=vector(),age=vector(),size=vector())
for (ii in 1:nfish) {
  la = mid
  
  age = mid
  
  ghat = 5 #Std in vulnerability
  ahatLL = c(400, 109, 98) #Size at 50% vulnerability
  
  
  ahatPS = c(30, 400, 400)
  lh1p = c(30, 60, 40) #lower limit of dome (cm)
  lh2p = c(80, 160, 120) #upper lim
  sd1p = c(5, 10, 10) #sd dome lower
  sd2p = c(1, 10, 10)
  
  #HL
  ahatHL = c(78, 120, 120) #Size at 50% vulnerability
  
  #SSS
  lh1 = c(15, 30, 30)
  lh2 = c(70, 90, 90)
  sd1 = c(5, 5, 5)
  sd2 = c(5, 20, 20)
  
  #PL
  lh1pl = c(40, 10, 20)
  lh2pl = c(70, 100, 100)
  sd1pl = c(5, 5, 5)
  sd2pl = c(20, 20, 20)
  
  
  valLL = sapply(age,plogis,ahatLL[ii],ghat)
  valPS = sapply(age,plogis,ahatPS[ii],ghat)
  vadPS = (1/(1+exp(-(1/sd1p[ii])*(la-lh1p[ii]))))*(1/(1+exp((1/sd2p[ii])*(la-lh2p[ii]))))
  valHL = sapply(age,plogis,ahatHL[ii],ghat) #log for HL
  vadSSS = (1/(1+exp(-(1/sd1[ii])*(la-lh1[ii]))))*(1/(1+exp((1/sd2[ii])*(la-lh2[ii])))) 
  vadPL = (1/(1+exp(-(1/sd1pl[ii])*(la-lh1pl[ii]))))*(1/(1+exp((1/sd2pl[ii])*(la-lh2pl[ii])))) 
  
  if (ii==1) {
    va = rbind(valLL, valPS, valHL, vadSSS, vadPL)
  }
  
  if (ii==2 | ii==3) {
    va = rbind(valLL, vadPS, valHL, vadSSS, vadPL)
  }
  
  rownames(va)=c("LL", "PS", "HL", "SSS", "PL")
  
  # va
  
  vat=t(va)
  vat=as.data.frame(vat)
  vat$sp=as.factor(ifelse(ii==1, "SKJ", ifelse(ii==2, "YFT", "BET")))
  vat$age=age
  vat$size=la
  vat$sizeclass = 1:32
  
  # vat
  
  va4=rbind(va4, vat)
  
}



va4m=melt(va4, id.vars = c("age", "size", "sp", "sizeclass"))

ggplot(va4m, aes(x=size, y=value))+
  facet_grid(.~sp, scales = "free")+
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA))+ #publication
  theme(axis.text.x = element_text(colour = "black", size = txts),
        axis.title.x = element_text(size=txtl), 
        axis.text.y = element_text(colour = "black", size = txts),
        axis.title.y = element_text(size=txtl),
        legend.text = element_text(size=txtl),
        legend.title = element_blank(), 
        legend.key = element_rect(colour="transparent", fill="white"),
        legend.position = "top")+
  theme(strip.text = element_text(size=txtl))+
  geom_line(aes(colour=variable, linetype=variable), size=1.5)+
  scale_color_manual(values = c("orangered2", "gold2", "deepskyblue", "midnightblue", "#00a78e"))+
  labs(y = "Vulnerability", x = "Size (cm)")
ggsave(paste("figures/", scenario, "_vuln_by_gear.png", sep = ""), height = 7, width = 11, units = "in")


head(va4m)
head(va4)


#multiply by max catchability to get Qs
max_catch = data.frame(
  INLL = c(rep(0,nsc), rep(9.3009e-11, nsc), rep(1.283179e-10,nsc)),
  otherLL = c(rep(0,nsc), rep(1.176e-10, nsc), rep(2.955e-10,nsc)),
  VNLL = c(rep(0,nsc), rep(3.9606e-10*0.5, nsc), rep(1.31005e-10*0.45,nsc)),
  INPS = c(rep(1.05672e-05,nsc), rep(7.452e-06,nsc), rep(1.495584e-06,nsc)),
  PHPS = c(rep(4.158336e-06,nsc), rep(3.3858e-06,nsc), rep(9.78592e-07,nsc)),
  VNPS = c(rep(1.311975e-05,nsc), rep(1.8711e-06,nsc), rep(3.889557e-06,nsc)),
  otherPS = c(rep(8.63226e-06,nsc), rep(4.06944e-06,nsc), rep(4.597969e-06,nsc)),
  INHL = c(rep(4.008125e-07,nsc), rep(1.097712e-06,nsc), rep(2.205053e-07,nsc)),
  PHHL = c(rep(4.8763e-08,nsc), rep(3.86958e-07,nsc), rep(4.66774e-08,nsc)),
  INSSS = c(rep(1.24236e-06,nsc), rep(2.121e-06,nsc), rep(1.391362e-06,nsc)),
  PHSSS = c(rep(1.7136e-06,nsc), rep(1.89072e-06,nsc), rep(5.18466e-07,nsc)),
  VNSSS = c(rep(8.568e-06,nsc), rep(5.3934e-07,nsc), rep(8.142575e-07,nsc)),
  INPL = c(rep(1.32804e-05,nsc), rep(6.27125e-06,nsc), rep(1.318248e-06,nsc))
)



Qs1 = va4
Qs1$INLL = Qs1$LL*max_catch$INLL
Qs1$INPS = Qs1$PS*max_catch$INPS*0.8
Qs1$INHL = Qs1$HL*max_catch$INHL
Qs1$INSSS = Qs1$SSS*max_catch$INSSS
Qs1$INPL = Qs1$PL*max_catch$INPL
Qs1$PHPS = Qs1$PS*max_catch$PHPS*0.8
Qs1$PHHL = Qs1$HL*max_catch$PHHL
Qs1$PHSSS = Qs1$SSS*max_catch$PHSSS
Qs1$VNLL = Qs1$LL*max_catch$VNLL
Qs1$VNPS = Qs1$PS*max_catch$VNPS*0.8
Qs1$VNSSS = Qs1$SSS*max_catch$VNSSS
Qs1$otherLL = Qs1$LL*max_catch$otherLL
Qs1$otherPS = Qs1$PS*max_catch$otherPS*0.8


head(Qs1)

Qs = array(c(Qs1$INLL, Qs1$INPS, Qs1$INHL, Qs1$INSSS, Qs1$INPL,  Qs1$PHPS, Qs1$PHHL, Qs1$PHSSS,  Qs1$VNLL, Qs1$VNPS, Qs1$VNSSS, Qs1$otherLL, Qs1$otherPS),
           dim = c(32,3,13))
dimnames(Qs)[[3]] <- gearorder
# Qs


#run model----
T_params <- LeMansParam(df=T_par, phi_min = 0.05, 
                        tau=T_tau, 
                        other=NS_other)
T_params@Qs <- Qs

eff_red = 0.9 #reduce effort for scenario

effort_mat <- matrix(0, 50, dim(Qs)[3])
effort_mat[,1] = 5e7 #INLL #order of gears needs to match order in Qs
effort_mat[,2] = 3000*eff_red #INPS
effort_mat[,3] = 50000 #INHL
effort_mat[,4] = 39000*eff_red #INSSS
effort_mat[,5] = 4500*eff_red #INPL
effort_mat[,6] = 4300 #PHPS
effort_mat[,7] = 45000 #PHHL
effort_mat[,8] = 10000 #PHSSS 
effort_mat[,9] = 4.59e7 #VNLL
effort_mat[,10] = 1300 #VNPS
effort_mat[,11] = 2000 #VNSSS
effort_mat[,12] = 3.23e8 #otherLL
effort_mat[,13] = 43930 #otherPS

colnames(effort_mat) <- gearorder
# head(effort_mat)

# effort_mat <- matrix(0, 50, dim(T_params@Qs)[3])
model_run <- run_LeMans(T_params, years=50, effort=effort_mat, N0=base_model)

#SSB----
biomass <- data.frame(get_SSB(T_params, model_run))
names(biomass) <- T_params@species_names
biomass <- biomass[2:nrow(biomass), ]/1e6 # Extract required rows and change units to tonnes (/1e6)
biomass$Year <- rep(1:50, each = 20)

biomass2 <- biomass %>%
  group_by(Year) %>%
  summarise_all(list(mean), na.rm = T)

out <- list(biomass = biomass2)

biomass_final = as.data.frame(out$biomass) #in mt
biomass_finalm = melt(biomass_final, id.vars = "Year")
head(biomass_finalm)
colnames(biomass_finalm) = c("Year", "Species", "Biomass") #catch in mt

# ggplot(biomass_finalm, aes(Year, Biomass, colour = Species))+
#   geom_point()+
#   geom_line()+
#   labs(title = "Biomass (mt)")

#compare to Williams & Reid 2018 stock assessments (all SSB)
biomass_real_v = c(6.2e6,   3.6e6,   1.4e6) 
biomass_real_fmsy = c(1100947, 860326, 320162)
biomass_real_latest = c(0.414*6220675, 0.54*3641228, 0.38*1395173) #from 2020 stock reports (the word docs)
# (biomass_real_fmsy/biomass_real_v)*100 #18 24 23% --> expect to see #s like this for fished model biomass/virgin biomass
(biomass_final[dim(biomass_final)[1],-1]/biomass_real_v)*100 #% of actual biomass virgin unfished
# (biomass_final[dim(biomass_final)[1],-1]/biomass_real_fmsy)*100 #% of actual biomass at MSY
(biomass_final[dim(biomass_final)[1],-1]/biomass_real_latest)*100 #% of actual biomass "current" 




#catch per gear per time step----
CPG = get_CPG(inputs = T_params, outputs = model_run, effort = effort_mat)
CPG = CPG/1e6 #convert to mt

dimnames(CPG)[[2]] <- gearorder
dimnames(CPG)[[3]] <- paste("TimeStep", seq(1:1000))

CPG_df = data.frame(INLL=vector(), INPS=vector(), INHL=vector(), INSSS=vector(), INPL=vector(),  PHPS=vector(), PHHL=vector(), PHSSS=vector(),  VNLL=vector(), VNPS=vector(), VNSSS=vector(), otherLL=vector(), otherPS=vector(), year=vector(), species = vector())

for (i in seq(from = 1, to = (1000-20), by = 20)) {
  # i = 1
  x = CPG[,,i] + CPG[,,i+1] + CPG[,,i+2] + CPG[,,i+3] + CPG[,,i+4] + CPG[,,i+5] + CPG[,,i+6] + CPG[,,i+7] + CPG[,,i+8] + CPG[,,i+9] + CPG[,,i+10] + CPG[,,i+11] + CPG[,,i+12] + CPG[,,i+13] + CPG[,,i+14] + CPG[,,i+15] + CPG[,,i+16] + CPG[,,i+17] + CPG[,,i+18] + CPG[,,i+19] + CPG[,,i+20]
  # x
  x1 = as.data.frame(x)
  x1$year = i
  x1$species = c("SKJ", "YFT", "BET")
  CPG_df = rbind(CPG_df, x1)
  
}

CPG_df$year = rep(1:49, each = 3)

CPG = CPG_df
CPGm = melt(CPG, id.vars = c("year", "species"))
head(CPGm)
colnames(CPGm) = c("year", "species", "gear", "catch")

CPG_30 = CPG[which(CPG$year==30),]
CPG_30m = melt(CPG_30, id.vars = c("year", "species"))
colnames(CPG_30m) = c("year", "species", "gear", "catch")
CPG_30m$species = factor(CPG_30m$species, levels = c("SKJ", "YFT", "BET"))


ggplot(CPG_30m, aes(gear, catch, fill = species))+
  theme(axis.title.x = element_blank())+
  geom_col()+
  scale_fill_manual(values = colspecies)+
  scale_y_continuous(labels = scientific)+
  labs(y = "Catch (mt)")
ggsave(paste("figures/", scenario, "_catch_by_gear.png", sep = ""), height = 7, width = 9, units = "in")


ggplot(CPG_30m, aes(species, catch, fill = gear))+
  theme(axis.title.x = element_blank())+
  geom_col()+
  scale_fill_manual(values = colgear)+
  scale_y_continuous(labels = scientific)+
  labs(y = "Catch (mt)")
ggsave(paste("figures/", scenario, "_catch_by_species.png", sep = ""), height = 7, width = 9, units = "in")


ggplot(CPG_30m, aes(species, catch, fill = gear))+
  facet_wrap(.~species, scales = "free")+
  theme(axis.title.x = element_blank())+
  geom_col()+
  scale_fill_manual(values = colgear)+
  scale_y_continuous(labels = scientific)+
  labs(y = "Catch (mt)")
ggsave(paste("figures/", scenario, "_catch_by_species_facet.png", sep = ""), height = 7, width = 9, units = "in")

#compare to reality
catches_gearsp_real = c(NA, 7599, 3619, #INLL from WCPFC yearbook
                        96161, 45817.6, 1273.4, #INPS
                        18749.8, 71042.8, 4516.6, #INHL (SS hook and line)
                        82283.8, 46027.8, 7815.8, #INSSS (other)
                        86190.4, 20447.8, 1142.8, #INPL
                        54429.2, 29421.4, 1187.6, #PNPS
                        2046.4, 22418.6, 876.2, #PNHL
                        28837.2, 10508.6, 743.2, #PNSSS (other + ringnet)
                        NA, 14954, 1683, #VNLL from ACE tables
                        52359, 4903, 1408, #VNPS
                        29030, 590, 237, #VNSSS (gillnet)
                        NA, 62447.2, 54697.6, #otherLL (total WCPO - above LL values) 
                        1147050.8, 359858, 56131 #otherPS (all)
)
catches_gearsp_real_df = as.data.frame(catches_gearsp_real)
catches_gearsp_real_df$gear = as.vector(
  rep(gearorder, each = 3)
)

#sum "real" values for catch by sp and compare to catch from 2017
#skj
sumS = sum(na.omit(catches_gearsp_real[seq(from = 1, to = 37, by =3)]))
#yft
sumY = sum(catches_gearsp_real[seq(from = 2, to = 38, by =3)])
#bet
sumB = sum(na.omit(catches_gearsp_real[seq(from = 3, to = 39, by =3)]))

modelvreal = data.frame(gear = rep(gearorder, each = 3), species = rep(c("SKJ", "YFT", "BET"), 13 ))
modelvreal$modelvreal = (CPG_30m$catch/catches_gearsp_real)*100 #first guess 
modelvreal$modelcatch = CPG_30m$catch
modelvreal

write.csv(modelvreal, paste("figures/", scenario, "_model_vs_realcatches.csv", sep = ""))

(sum(CPG_30m$catch)/sum(c(1624162, 670890, 126929)))*100 #2017 catches by sp. should be almost exactly 100%

CPG_30_spsum = rowSums(CPG_30[,1:13])

#by species
summ_sp = data.frame(SKJ = vector(), YFT = vector(), BET = vector())
summ_sp[1,] = (biomass_final[dim(biomass_final)[1],-1]/biomass_real_latest)*100
summ_sp[2,] =((CPG_30_spsum)/(c(sumS, sumY, sumB)))*100
rownames(summ_sp)  = c("SSBvReal", "CatchvReal")
summ_sp

write.csv(summ_sp, paste("figures/", scenario, "_SSB_totalcatch_vreal_bysp.csv", sep = ""))



#economics----

#costs
effort_summary = as.data.frame(effort_mat[1,])
colnames(effort_summary) = c("effort")
effort_summary$gear = row.names(effort_summary)
rownames(effort_summary) = NULL


effort_summary$unitcost = as.vector(c(1, 22000, 2000, 100, 3000, #IN
                                      22000, 2000, 100, #PH
                                      1, 22000, 100, #VN
                                      1, 22000 #other
))

effort_summary$totalcost = effort_summary$effort * effort_summary$unitcost


#prices
CPG_30m$unitprice = as.vector(c(0, 8000, 11000, #INLL
                                1600, 2300, 2600, #INPS
                                1850, 7700, 10700, #INHL
                                1300, 2000, 2300, #INSSS
                                1850, 2550, 2850, #INPL
                                1600, 2300, 2600, #PHPS
                                1850, 7700, 10700, #PHHL
                                1300, 2000, 2300, #PHSSS
                                0, 8000, 11000, #VNLL
                                1600, 2300, 2600, #VNPS
                                1300, 2000, 2300, #VNSSS
                                0, 8000, 11000, #otherLL
                                1600, 2300, 2600 #otherPS
))
CPG_30m$exvesselprice = CPG_30m$catch * CPG_30m$unitprice

prices_gear = CPG_30m %>% group_by(gear) %>% summarise(exvesselprice = sum(exvesselprice))

tuna_econ = merge(effort_summary, prices_gear, by = "gear")
tuna_econ$profit = tuna_econ$exvesselprice - tuna_econ$totalcost
tuna_econ  = tuna_econ[,-5]
tuna_econ$gear = factor(tuna_econ$gear, levels = gearorder)

write.csv(tuna_econ, paste("figures/", scenario, "_modeloutput_tuna_econ.csv", sep = ""))

ggplot(tuna_econ, aes(gear, profit))+
  theme(axis.title.x = element_blank())+
  geom_col()+
  labs(y = "Rent (USD)")
ggsave(paste("figures/", scenario, "_profit_by_gear.png", sep = ""), height = 7, width = 9, units = "in")


#by country
econ_IN = colSums(tuna_econ[1:5,-1])
# econ_IN$country = "IN"
econ_PH = colSums(tuna_econ[8:10,-1])
# econ_PH$country = "PH"
econ_VN = colSums(tuna_econ[11:13,-1])
# econ_VN$country = "VN"
econ_other = colSums(tuna_econ[6:7,-1])
# econ_other$country = "other"

econ_bycountry = as.data.frame(rbind(econ_IN, econ_PH, econ_VN, econ_other))
write.csv(econ_bycountry, paste("figures/", scenario, "_modeloutput_tuna_econ_countrysum.csv", sep = ""))


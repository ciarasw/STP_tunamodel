#Base model for "Harvest Control Rules for Tuna in the WCPFC: Implications for Indonesia, Philippines, and Vietnam" (Bailey, Willis, et al. 2023) for WWF Sustainable Tuna Partnership
#Code by Ciara Willis (2023) with assistance from Mike Spence (author of LeMaRns package)


#set up basic realistic scenario with the countries' gears

#country order: Indonesia, Philippines, Vietnam
#gear order: IN (LL, PS, HL, SSS, PL), PH (PS, HL, SSS), VN (LL, PS, SSS)
#INLL, INPS, INHL, INSSS, INPL,  PHPS, PHHL, PHSSS,  VNLL, VNPS, VNSSS, otherLL, otherPS



#setup & import parameters----
library(LeMaRns)
library(tidyverse)
library(reshape)
library(ggsci)
library(scales)
# library(ggbreak)

#set plot aesthetics
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
# show_col(colspecies)


speciesnames = c("Skipjack", "Yellowfin", "Bigeye")
names(speciesnames) = speciesorder



#read in tuna parameters
T_par = read.csv("code/LeMaRns/tuna_setupLeMaRns.csv")
T_par = T_par[,c("species_names",	"Linf",	"W_a",	"W_b",	"k",	"Lmat",	"a",	"b")]

#food web --> no food web component to this work, so all 0
T_tau=matrix(data=0, nrow = 3, ncol=3)

#species independent param
nfish = nrow(T_par)
nsc = 32
maxsize = max(T_par$Linf)*1.01
l_bound <- seq(0, maxsize, maxsize/nsc); l_bound <- l_bound[-length(l_bound)]
u_bound <- seq(maxsize/nsc, maxsize, maxsize/nsc)
mid <- l_bound+(u_bound-l_bound)/2


#species specific
#size, growth, maturity
Linf = T_par$Linf
W_a = T_par$W_a
W_b = T_par$W_b
k = T_par$k
Lmat = T_par$Lmat

#time steps
tmp <- calc_phi(k, Linf, nsc, nfish, u_bound, l_bound, calc_phi_min=FALSE,phi_min = 0.05)
phi <- tmp$phi
phi_min <- tmp$phi_min

#calc ration & growth
tmp <- calc_ration_growthfac(k, Linf, nsc, nfish, l_bound, u_bound, mid, W_a, W_b
                             , phi_min)
ration <- tmp$ration
sc_Linf <- tmp$sc_Linf
wgt <- tmp$wgt
g_eff <- tmp$g_eff

#proportion individuals mature
mature <- calc_mature(Lmat, nfish, mid, kappa=rep(10, nfish), sc_Linf)

#food web "other food" --> can ignore this
other <- NS_other



#Recruitment
stored_rec_funs <- get_rec_fun(rep("hockey-stick", nfish))
recruit_params <- do.call("Map", c(c, list(a=NS_par$a[1:nfish], b=NS_par$b[1:nfish])))


#Mortality (M1 = background/natural)
M1 <- calc_M1(nsc, sc_Linf, phi_min,
              natmort_opt=rep("std_RNM", length(sc_Linf)),
              Nmort=rep(0.8, length(sc_Linf)),
              prop=rep(0.75, length(sc_Linf)))

#predation stuff - ignore
prefs <- calc_prefs(pred_mu=-2.25, pred_sigma=0.5, wgt, sc_Linf)
suit_M2 <- calc_suit_vect(nsc, nfish, sc_Linf, prefs, T_tau)


# save.image(file = "code/Base_model_LHparams.RData")


#calc Qs----
#gear order: IN (LL, PS, HL, SSS, PL), PH (PS, HL, SSS), VN (LL, PS, SSS), otherLL, otherPS


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
  
  
  vat=t(va)
  vat=as.data.frame(vat)
  vat$sp=as.factor(ifelse(ii==1, "SKJ", ifelse(ii==2, "YFT", "BET")))
  vat$age=age
  vat$size=la
  vat$sizeclass = 1:32
  
  va4=rbind(va4, vat)
  
}



va4m=melt(va4, id.vars = c("age", "size", "sp", "sizeclass"))

#plot vulnerabilities
ggplot(va4m, aes(x=size, y=value))+
  facet_grid(.~sp, scales = "free",
             labeller = labeller(sp = speciesnames)
             )+
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
ggsave("figures/basemodel_vuln_by_gear.png", height = 7, width = 11, units = "in")
ggsave("figures/basemodel_vuln_by_gear_pres.png", height = 4, width = 7, units = "in")


head(va4m)
head(va4)


#multiply vulnerability by max catchability to get Qs
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
Qs1$INPS = Qs1$PS*max_catch$INPS
Qs1$INHL = Qs1$HL*max_catch$INHL
Qs1$INSSS = Qs1$SSS*max_catch$INSSS
Qs1$INPL = Qs1$PL*max_catch$INPL
Qs1$PHPS = Qs1$PS*max_catch$PHPS
Qs1$PHHL = Qs1$HL*max_catch$PHHL
Qs1$PHSSS = Qs1$SSS*max_catch$PHSSS
Qs1$VNLL = Qs1$LL*max_catch$VNLL
Qs1$VNPS = Qs1$PS*max_catch$VNPS
Qs1$VNSSS = Qs1$SSS*max_catch$VNSSS
Qs1$otherLL = Qs1$LL*max_catch$otherLL
Qs1$otherPS = Qs1$PS*max_catch$otherPS


head(Qs1)

Qs = array(c(Qs1$INLL, Qs1$INPS, Qs1$INHL, Qs1$INSSS, Qs1$INPL,  Qs1$PHPS, Qs1$PHHL, Qs1$PHSSS,  Qs1$VNLL, Qs1$VNPS, Qs1$VNSSS, Qs1$otherLL, Qs1$otherPS),
           dim = c(32,3,13))
dimnames(Qs)[[3]] <- gearorder
# Qs



#run model: basic "real" efforts----
T_params <- LeMansParam(df=T_par, phi_min = 0.05, 
                        tau=T_tau, 
                        other=NS_other)
T_params@Qs <- Qs

effort_mat <- matrix(0, 50, dim(Qs)[3])
effort_mat[,1] = 5e7 #INLL
effort_mat[,2] = 3000 #INPS
effort_mat[,3] = 50000 #INHL
effort_mat[,4] = 39000 #INSSS
effort_mat[,5] = 4500 #INPL
effort_mat[,6] = 4300 #PHPS
effort_mat[,7] = 45000 #PHHL
effort_mat[,8] = 10000 #PHSSS 
effort_mat[,9] = 4.59e7 #VNLL
effort_mat[,10] = 1300 #VNPS
effort_mat[,11] = 2000 #VNSSS
effort_mat[,12] = 3.23e8 #otherLL
effort_mat[,13] = 43930 #otherPS

colnames(effort_mat) <- gearorder
head(effort_mat)

# effort_mat <- matrix(0, 50, dim(T_params@Qs)[3])
model_run <- run_LeMans(T_params, years=50, effort=effort_mat)



#save base model long term equil biomasses
base_model = model_run@N[,,600]
# class(base_model)
save(base_model, file = "code/base_model_N0.RData")



#plot SSbiomass, compare to real SSbiomass----
#calc catch & biomass by time step
catch <- data.frame(t(colSums(model_run@Catch))) #per species, by time step. manually checked a few time steps to confirm working as expected
biomass <- data.frame(get_SSB(T_params, model_run))

names(catch) <- T_params@species_names
names(biomass) <- T_params@species_names

# head(catch)
tail(catch)
tail(biomass)

# Extract required rows and change units to tonnes (/1e6)
catch <- catch[2:nrow(catch), ]/1e6 #
biomass <- biomass[2:nrow(biomass), ]/1e6

catch$Year <- rep(1:50, each = 20)
biomass$Year <- rep(1:50, each = 20)


# Calculate the total catch and biomass of each species per year
catch2 <- catch %>%
  group_by(Year) %>%
  summarise_all(list(sum), na.rm = T)
# colSums((catch[which(catch$Year==1),])) #yes, matches catch2

biomass2 <- biomass %>%
  group_by(Year) %>%
  summarise_all(list(mean), na.rm = T)
# colMeans((biomass[which(biomass$Year==1),])) #yes, matches biomass2

# Combine the outputs into a list
out <- list(catch = catch2, biomass = biomass2)


catch_final = as.data.frame(out$catch)
sum(catch_final[30,])
catch_finalm = melt(catch_final, id.vars = "Year")
head(catch_finalm)
colnames(catch_finalm) = c("Year", "Species", "Catch") #catch in mt

#compare to Williams & Reid 2018 stock assessments
catch_realistic = c(1624162, 670890, 126929) #2017 catches
sum(catch_realistic) #~2.5 MT
(catch_final[dim(catch_final)[1],-1]/catch_realistic) * 100
(sum(catch_final[dim(catch_final)[1],-1])/sum(catch_realistic))*100



ggplot(catch_finalm, aes(Year, Catch, colour = Species))+
  geom_point()+
  geom_line()+
  scale_color_manual(values = colspecies)


#plot biomass 

biomass_final = as.data.frame(out$biomass) #in mt
biomass_finalm = melt(biomass_final, id.vars = "Year")
head(biomass_finalm)
colnames(biomass_finalm) = c("Year", "Species", "Biomass") #catch in mt

ggplot(biomass_finalm, aes(Year, Biomass, colour = Species))+
  geom_point()+
  geom_line()+
  labs(title = "Biomass (mt)")

# tail(biomass_final)

#compare to Williams & Reid 2018 stock assessments (all SSB)
biomass_real_v = c(6.2e6,   3.6e6,   1.4e6) 
biomass_real_fmsy = c(1100947, 860326, 320162)
biomass_real_latest = c(0.414*6220675, 0.54*3641228, 0.38*1395173) #from 2020 stock reports (the word docs)
# (biomass_real_fmsy/biomass_real_v)*100 #18 24 23% --> expect to see #s like this for fished model biomass/virgin biomass
(biomass_final[dim(biomass_final)[1],-1]/biomass_real_v)*100 #% of actual biomass virgin unfished
# (biomass_final[dim(biomass_final)[1],-1]/biomass_real_fmsy)*100 #% of actual biomass at MSY
(biomass_final[dim(biomass_final)[1],-1]/biomass_real_latest)*100 #% of actual biomass "current" 










#extract catch per gear per time step----
CPG = get_CPG(inputs = T_params, outputs = model_run, effort = effort_mat)
CPG = CPG/1e6 #convert to mt

#get_CPG returns an array with dimensions nfish, dim(Qs[3]) and the number of time steps, where the i,j,kth element denotes the total catch of the ith species by the jth gear in the kth time step.

# head(CPG)
dim(CPG) #sp, gears, timesteps
# dimnames(CPG)[[1]] <- c("SKJ", "YFT", "BET")
dimnames(CPG)[[2]] <- gearorder
dimnames(CPG)[[3]] <- paste("TimeStep", seq(1:1000))
CPG[,,1]
# class(CPG)


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

dim(CPG_df)
CPG_df$year = rep(1:49, each = 3)
head(CPG_df, 10)

CPG = CPG_df
# head(CPG)
CPGm = melt(CPG, id.vars = c("year", "species"))
head(CPGm)
colnames(CPGm) = c("year", "species", "gear", "catch")



CPG_30 = CPG[which(CPG$year==30),]
CPG_30m = melt(CPG_30, id.vars = c("year", "species"))
colnames(CPG_30m) = c("year", "species", "gear", "catch")
CPG_30m$species = factor(CPG_30m$species, levels = speciesorder)


ggplot(CPG_30m, aes(gear, catch, fill = species))+
  theme(axis.title.x = element_blank())+
  geom_col()+
  scale_fill_manual(values = colspecies)+
  scale_y_continuous(labels = scientific)+
  labs(y = "Catch (mt)")
ggsave("figures/basemodel_catch_by_gear.png", height = 7, width = 9, units = "in")

ggplot(CPG_30m[which(CPG_30m$gear!="otherPS"),], aes(gear, catch, fill = species))+
  theme(axis.title.x = element_blank())+
  geom_col()+
  scale_fill_manual(values = colspecies)+
  scale_y_continuous(labels = scientific)+
  # scale_y_break(c(1.5e5, 1.5e6))+
  labs(y = "Catch (mt)")
ggsave("figures/basemodel_catch_by_gear_pres.png", height = 2.5, width = 9, units = "in")


ggplot(CPG_30m, aes(species, catch, fill = gear))+
  theme(axis.title.x = element_blank())+
  geom_col()+
  scale_fill_manual(values = colgear)+
  scale_y_continuous(labels = scientific)+
  labs(y = "Catch (mt)")
ggsave("figures/basemodel_catch_by_species.png", height = 7, width = 9, units = "in")


ggplot(CPG_30m, aes(species, catch, fill = gear))+
  facet_wrap(.~species, scales = "free")+
  theme(axis.title.x = element_blank())+
  geom_col()+
  scale_fill_manual(values = colgear)+
  scale_y_continuous(labels = scientific)+
  labs(y = "Catch (mt)")
ggsave("figures/basemodel_catch_by_species_facet.png", height = 7, width = 9, units = "in")


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
                        NA, 62447.2, 54697.6, #otherLL (total WCPO LL - above LL values) 
                        1147050.8, 359858, 56131 #otherPS
                        )

sum(na.omit(catches_gearsp_real[1:11]))/sum(na.omit(catches_gearsp_real))

catches_gearsp_real_df = as.data.frame(catches_gearsp_real)
catches_gearsp_real_df$gear = as.vector(
  rep(gearorder, each = 3)
                                        )
catches_gearsp_real_df$gear = factor(catches_gearsp_real_df$gear, levels = gearorder)

catches_gearsp_real_df$species = as.vector(rep(c("SKJ", "YFT", "BET"), 13 ))
catches_gearsp_real_df$species = factor(catches_gearsp_real_df$species, levels = speciesorder)

ggplot(catches_gearsp_real_df, aes(gear, catches_gearsp_real, fill = species))+
  theme(axis.title.x = element_blank())+
  geom_col()+
  scale_fill_manual(values = colspecies)+
  scale_y_continuous(labels = scientific)+
  labs(y = "Real reported catches (mt)")
ggsave("figures/real_catch_by_gear.png", height = 7, width = 9, units = "in")


#sum "real" values for catch by sp and compare to catch from 2017
#skj
sumS = sum(na.omit(catches_gearsp_real[seq(from = 1, to = 37, by =3)]))
sumS/1624162 #1
#yft
sumY = sum(catches_gearsp_real[seq(from = 2, to = 38, by =3)])
sumY/670890 #1
#bet
sumB = sum(na.omit(catches_gearsp_real[seq(from = 3, to = 39, by =3)]))
sumB/126929 #1

# ye2.actual=data.frame(SJ=c(0,1350000),
                      # YF=c(85000,440000),
                      # BE=c(60000,60000))

modelvreal = data.frame(gear = rep(gearorder, each = 3), species = rep(c("SKJ", "YFT", "BET"), 13 ))
modelvreal$modelvreal = (CPG_30m$catch/catches_gearsp_real)*100 #first guess 
modelvreal$modelcatch = CPG_30m$catch
modelvreal

# hist(modelvreal$modelvreal)

write.csv(modelvreal, "figures/basemodel_model_vs_realcatches.csv")

(sum(CPG_30m$catch)/sum(c(1624162, 670890, 126929)))*100 #2017 catches by sp. should be almost exactly 100%


CPG_30_spsum = rowSums(CPG_30[,1:13])


#by species
summ_sp = data.frame(SKJ = vector(), YFT = vector(), BET = vector())
summ_sp[1,] = (biomass_final[dim(biomass_final)[1],-1]/biomass_real_latest)*100
summ_sp[2,] =((CPG_30_spsum)/(c(sumS, sumY, sumB)))*100
rownames(summ_sp)  = c("SSBvReal", "CatchvReal")
summ_sp

write.csv(summ_sp, "figures/basemodel_SSB_totalcatch_vreal_bysp.csv")


#estimate fuel use----
fuel = as.data.frame(effort_mat[1,])
colnames(fuel) = c("effort")
fuel$gear = rownames(fuel)
           
fuel$fuelunit = ifelse(
  fuel$gear == "INLL" | fuel$gear == "VNLL" | fuel$gear == "otherLL", 1.45e-3, ifelse(
    fuel$gear == "INPS" | fuel$gear == "PHPS" | fuel$gear == "VNPS" | fuel$gear == "otherPS", 36, 9
  )
)

head(fuel)

write.csv(fuel, "figures/basemodel_fuel.csv")


#economics----
#cost = unit cost * effort (from eff matrix)
#revenue = yield * price per mt per gear


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
                                1850, 7000, 10000, #INHL
                                1300, 2000, 2300, #INSSS
                                1850, 2550, 2850, #INPL
                                1600, 2300, 2600, #PHPS
                                1850, 7000, 10000, #PHHL
                                1300, 2000, 2300, #PHSSS
                                0, 8000, 11000, #VNLL
                                1600, 2300, 2600, #VNPS
                                1300, 2000, 2300, #VNSSS
                                0, 8000, 11000, #otherLL
                                1600, 2300, 2600 #otherPS
                                ))
CPG_30m$exvesselprice = CPG_30m$catch * CPG_30m$unitprice
head(CPG_30m)


# ggplot(CPG_30m, aes(gear, exvesselprice, fill = species))+
  # geom_col()

prices_gear = CPG_30m %>% group_by(gear) %>% summarise(exvesselprice = sum(exvesselprice))


tuna_econ = merge(effort_summary, prices_gear, by = "gear")
# tuna_econ = tuna_econ[order()]
tuna_econ$profit = tuna_econ$exvesselprice - tuna_econ$totalcost
tuna_econ  = tuna_econ[,-5]
tuna_econ$gear = factor(tuna_econ$gear, levels = gearorder)

write.csv(tuna_econ, "figures/basemodel_modeloutput_tuna_econ.csv")

ggplot(tuna_econ, aes(gear, profit))+
  theme(axis.title.x = element_blank())+
  geom_col()+
  labs(y = "Rent (USD)")
ggsave("figures/basemodel_profit_by_gear.png", height = 7, width = 9, units = "in")

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
write.csv(econ_bycountry, "figures/basemodel_modeloutput_tuna_econ_countrysum.csv")



#DADASET MODIFICATION

###----------------------------------------------------------------####




install.packages("ggpubr")

#--Importando a base e carregando os packages--------------------------------------------


setwd("C:/Users/user/Desktop/Base")
base <- read.csv("base_score_norm.csv")  #Importando a base
stats <- read.csv("scores_idade.csv")    #Importando os scores

library(ggplot2)
library(ggpubr)
library(dplyr)
library("RColorBrewer")


my_colors<-c("#FFFF00", "#FFCC00", "#FF6600", 
             "#CC0000", "#660000")





#--Construção dos gráficos---------------------------------------------------------------
#working

prop_atc_idade <- ggplot(subset(base, Idade %in% c("14","15","16","17","18"),),
                         aes(x = score_atc, color=`Idade`))+
  geom_density()+ scale_color_manual(values=c(my_colors)) + 
  geom_vline(xintercept=stats[1,3], linetype="dashed", color="#FFFF00")+
  geom_vline(xintercept=stats[2,3], linetype="dashed", color="#FFCC00")+
  geom_vline(xintercept=stats[3,3], linetype="dashed", color="#FF6600")+
  geom_vline(xintercept=stats[4,3], linetype="dashed", color="#CC0000")+
  geom_vline(xintercept=stats[5,3], linetype="dashed", color="#660000")+
  xlab("Autoconhecimento")+
  ylab("Densidade")

print(prop_atc_idade)

ggsave("prop_atc_idade.png")


#---------------------------------------------------------------------------------------------#

#working
prop_aut_idade <- ggplot(subset(base, Idade %in% c("14","15","16","17","18")),
                         aes(x = score_aut, color=`Idade`))+
  geom_density()+ scale_color_manual(values=c(my_colors)) +
  geom_vline(xintercept=stats[1,4], linetype="dashed", color="#FFFF00")+
  geom_vline(xintercept=stats[2,4], linetype="dashed", color="#FFCC00")+
  geom_vline(xintercept=stats[3,4], linetype="dashed", color="#FF6600")+
  geom_vline(xintercept=stats[4,4], linetype="dashed", color="#CC0000")+
  geom_vline(xintercept=stats[5,4], linetype="dashed", color="#660000")+
  xlab("Autoeficácia")+
  ylab("Densidade")

print(prop_aut_idade)


#---------------------------------------------------------------------------------------------#


prop_ctd_idade <- ggplot(subset(base, Idade %in% c("14","15","16","17","18")),
                         aes(x = score_ctd, color=`Idade`))+
  geom_density()+ scale_color_manual(values=c(my_colors)) +
  geom_vline(xintercept=stats[1,5], linetype="dashed", color="#FFFF00")+
  geom_vline(xintercept=stats[2,5], linetype="dashed", color="#FFCC00")+
  geom_vline(xintercept=stats[3,5], linetype="dashed", color="#FF6600")+
  geom_vline(xintercept=stats[4,5], linetype="dashed", color="#CC0000")+
  geom_vline(xintercept=stats[5,5], linetype="dashed", color="#660000")+
  xlab("Capacidade de Tomar Decisão")+
  ylab("Densidade")


print(prop_ctd_idade)


#-----------------------------------------------------------------------------------------------#


prop_mot_idade <- ggplot(subset(base, Idade %in% c("14","15","16","17","18")),
                         aes(x = score_mot, color=`Idade`))+
  geom_density()+ scale_color_manual(values=c(my_colors)) +
  geom_vline(xintercept=stats[1,6], linetype="dashed", color="#FFFF00")+
  geom_vline(xintercept=stats[2,6], linetype="dashed", color="#FFCC00")+
  geom_vline(xintercept=stats[3,6], linetype="dashed", color="#FF6600")+
  geom_vline(xintercept=stats[4,6], linetype="dashed", color="#CC0000")+
  geom_vline(xintercept=stats[5,6], linetype="dashed", color="#660000")+
  xlab("Motivação")+
  ylab("Densidade")

print(prop_mot_idade)
#-----------------------------------------------------------------------------------------------#

prop_adp_idade <- ggplot(subset(base, Idade %in% c("14","15","16","17","18")),
                         aes(x = score_adp, color=`Idade`))+
  geom_density()+ scale_color_manual(values=c(my_colors)) +
  geom_vline(xintercept=stats[1,7], linetype="dashed", color="#FFFF00")+
  geom_vline(xintercept=stats[2,7], linetype="dashed", color="#FFCC00")+
  geom_vline(xintercept=stats[3,7], linetype="dashed", color="#FF6600")+
  geom_vline(xintercept=stats[4,7], linetype="dashed", color="#CC0000")+
  geom_vline(xintercept=stats[5,7], linetype="dashed", color="#660000")+
  xlab("Adaptabilidade")+
  ylab("Densidade")


print(prop_adp_idade )

#-----------------------------------------------------------------------------------------------#


prop_aaa_idade <- ggplot(subset(base, Idade %in% c("14","15","16","17","18")),
                         aes(x = score_aaa, color=`Idade`))+
  geom_density()+ scale_color_manual(values=c(my_colors)) +
  geom_vline(xintercept=stats[1,8], linetype="dashed", color="#FFFF00")+
  geom_vline(xintercept=stats[2,8], linetype="dashed", color="#FFCC00")+
  geom_vline(xintercept=stats[3,8], linetype="dashed", color="#FF6600")+
  geom_vline(xintercept=stats[4,8], linetype="dashed", color="#CC0000")+
  geom_vline(xintercept=stats[5,8], linetype="dashed", color="#660000")+
  xlab("Aprender à Aprender")+
  ylab("Densidade")

print(prop_aaa_idade)
#------------------------------------------------------------------------------------------------#

prop_ldc_idade <- ggplot(subset(base, Idade %in% c("14","15","16","17","18")),
                         aes(x = score_ldc, color=`Idade`))+
  geom_density()+ scale_color_manual(values=c(my_colors)) +
  geom_vline(xintercept=stats[1,9], linetype="dashed", color="#FFFF00")+
  geom_vline(xintercept=stats[2,9], linetype="dashed", color="#FFCC00")+
  geom_vline(xintercept=stats[3,9], linetype="dashed", color="#FF6600")+
  geom_vline(xintercept=stats[4,9], linetype="dashed", color="#CC0000")+
  geom_vline(xintercept=stats[5,9], linetype="dashed", color="#660000")+
  xlab("Lócus de controle")+
  ylab("Densidade")

print(prop_ldc_idade)

#-------------------------------------------------------------------------------------------------#

prop_mdc_idade <- ggplot(subset(base, Idade %in% c("14","15","16","17","18")),
                         aes(x = score_mdc, color=`Idade`))+
  geom_density()+ scale_color_manual(values=c(my_colors)) +
  geom_vline(xintercept=stats[1,10], linetype="dashed", color="#FFFF00")+
  geom_vline(xintercept=stats[2,10], linetype="dashed", color="#FFCC00")+
  geom_vline(xintercept=stats[3,10], linetype="dashed", color="#FF6600")+
  geom_vline(xintercept=stats[4,10], linetype="dashed", color="#CC0000")+
  geom_vline(xintercept=stats[5,10], linetype="dashed", color="#660000")+
  xlab("Mentalidade de Crescimento")+
  ylab("Densidade")

print(prop_mdc_idade)


#---------------------------------------------------------------------------------------------------#
prop_plc_idade <- ggplot(subset(base, Idade %in% c("14","15","16","17","18")),
                         aes(x = score_plc, color=`Idade`))+
  geom_density()+ scale_color_manual(values=c(my_colors)) +
  geom_vline(xintercept=stats[1,11], linetype="dashed", color="#FFFF00")+
  geom_vline(xintercept=stats[2,11], linetype="dashed", color="#FFCC00")+
  geom_vline(xintercept=stats[3,11], linetype="dashed", color="#FF6600")+
  geom_vline(xintercept=stats[4,11], linetype="dashed", color="#CC0000")+
  geom_vline(xintercept=stats[5,11], linetype="dashed", color="#660000")+
  xlab("Planejamento de carreira")+
  ylab("Densidade")

print(prop_plc_idade)

#-----------------------------------------------------------------------------------------------------#

prop_atm_idade <- ggplot(subset(base, Idade %in% c("14","15","16","17","18")),
                         aes(x = score_atm, color=`Idade`))+
  geom_density()+ scale_color_manual(values=c(my_colors)) +
  geom_vline(xintercept=stats[1,12], linetype="dashed", color="#FFFF00")+
  geom_vline(xintercept=stats[2,12], linetype="dashed", color="#FFCC00")+
  geom_vline(xintercept=stats[3,12], linetype="dashed", color="#FF6600")+
  geom_vline(xintercept=stats[4,12], linetype="dashed", color="#CC0000")+
  geom_vline(xintercept=stats[5,12], linetype="dashed", color="#660000")+
  xlab("Autonomia")+
  ylab("Densidade")

print(prop_atm_idade)

#-------------------------------------------------------------------------------------------------------#

prop_cli_idade <- ggplot(subset(base, Idade %in% c("14","15","16","17","18")),
                         aes(x = score_cli, color=`Idade`))+
  geom_density()+ scale_color_manual(values=c(my_colors)) +
  geom_vline(xintercept=stats[1,13], linetype="dashed", color="#FFFF00")+
  geom_vline(xintercept=stats[2,13], linetype="dashed", color="#FFCC00")+
  geom_vline(xintercept=stats[3,13], linetype="dashed", color="#FF6600")+
  geom_vline(xintercept=stats[4,13], linetype="dashed", color="#CC0000")+
  geom_vline(xintercept=stats[5,13], linetype="dashed", color="#660000")+
  xlab("Capacidade de liderar")+
  ylab("Densidade")

print(prop_plc_idade)

#-------------------------------------------------------------------------------------------------------#


prop_cpt_idade <- ggplot(subset(base, Idade %in% c("14","15","16","17","18")),
                         aes(x = score_cpt, color=`Idade`))+
  geom_density()+ scale_color_manual(values=c(my_colors)) +
  geom_vline(xintercept=stats[1,14], linetype="dashed", color="#FFFF00")+
  geom_vline(xintercept=stats[2,14], linetype="dashed", color="#FFCC00")+
  geom_vline(xintercept=stats[3,14], linetype="dashed", color="#FF6600")+
  geom_vline(xintercept=stats[4,14], linetype="dashed", color="#CC0000")+
  geom_vline(xintercept=stats[5,14], linetype="dashed", color="#660000")+
  xlab("Capacidade de priorizar tarefas")+
  ylab("Densidade")

print(prop_cpt_idade)


#------------------------------------------------------------------------------------------------------#

prop_crp_idade <- ggplot(subset(base, Idade %in% c("14","15","16","17","18")),
                         aes(x = score_crp, color=`Idade`))+
  geom_density()+ scale_color_manual(values=c(my_colors)) +
  geom_vline(xintercept=stats[1,15], linetype="dashed", color="#FFFF00")+
  geom_vline(xintercept=stats[2,15], linetype="dashed", color="#FFCC00")+
  geom_vline(xintercept=stats[3,15], linetype="dashed", color="#FF6600")+
  geom_vline(xintercept=stats[4,15], linetype="dashed", color="#CC0000")+
  geom_vline(xintercept=stats[5,15], linetype="dashed", color="#660000")+  xlab("Capacidade de resolver problemas")+
  ylab("Densidade")

print(prop_crp_idade)


#--------------------------------------------------------------------------------------------------------#


prop_cmd_idade <- ggplot(subset(base, Idade %in% c("14","15","16","17","18")),
                         aes(x = score_cmd, color=`Idade`))+
  geom_density()+ scale_color_manual(values=c(my_colors)) +
  geom_vline(xintercept=stats[1,16], linetype="dashed", color="#FFFF00")+
  geom_vline(xintercept=stats[2,16], linetype="dashed", color="#FFCC00")+
  geom_vline(xintercept=stats[3,16], linetype="dashed", color="#FF6600")+
  geom_vline(xintercept=stats[4,16], linetype="dashed", color="#CC0000")+
  geom_vline(xintercept=stats[5,16], linetype="dashed", color="#660000")+
  xlab("Competências Digitais")+
  ylab("Densidade")

print(prop_cmd_idade)


#---------------------------------------------------------------------------------------------------------#

prop_cmn_idade <- ggplot(subset(base, Idade %in% c("14","15","16","17","18")),
                         aes(x = score_cmn, color=`Idade`))+
  geom_density()+ scale_color_manual(values=c(my_colors)) +
  geom_vline(xintercept=stats[1,17], linetype="dashed", color="#FFFF00")+
  geom_vline(xintercept=stats[2,17], linetype="dashed", color="#FFCC00")+
  geom_vline(xintercept=stats[3,17], linetype="dashed", color="#FF6600")+
  geom_vline(xintercept=stats[4,17], linetype="dashed", color="#CC0000")+
  geom_vline(xintercept=stats[5,17], linetype="dashed", color="#660000")+
  xlab("Comunicação")+
  ylab("Densidade")

print(prop_cmn_idade)

#---------------------------------------------------------------------------------------------------------#



prop_coo_idade <- ggplot(subset(base, Idade %in% c("14","15","16","17","18")),
                         aes(x = score_coo, color=`Idade`))+
  geom_density()+ scale_color_manual(values=c(my_colors)) +
  geom_vline(xintercept=stats[1,18], linetype="dashed", color="#FFFF00")+
  geom_vline(xintercept=stats[2,18], linetype="dashed", color="#FFCC00")+
  geom_vline(xintercept=stats[3,18], linetype="dashed", color="#FF6600")+
  geom_vline(xintercept=stats[4,18], linetype="dashed", color="#CC0000")+
  geom_vline(xintercept=stats[5,18], linetype="dashed", color="#660000")+
  xlab("Cooperatividade")+
  ylab("Densidade")

print(prop_coo_idade)


#--------------------------------------------------------------------------------------------------------#

prop_flx_idade <- ggplot(subset(base, Idade %in% c("14","15","16","17","18")),
                         aes(x = score_flx, color=`Idade`))+
  geom_density()+ scale_color_manual(values=c(my_colors)) +
  geom_vline(xintercept=stats[1,19], linetype="dashed", color="#FFFF00")+
  geom_vline(xintercept=stats[2,19], linetype="dashed", color="#FFCC00")+
  geom_vline(xintercept=stats[3,19], linetype="dashed", color="#FF6600")+
  geom_vline(xintercept=stats[4,19], linetype="dashed", color="#CC0000")+
  geom_vline(xintercept=stats[5,19], linetype="dashed", color="#660000")+
  xlab("Flexibilidade")+
  ylab("Densidade")

prop_rec_idade <- ggplot(subset(base, Idade %in% c("14","15","16","17","18")),
                         aes(x = score_rec, color=`Idade`))+
  geom_density()+ scale_color_manual(values=c(my_colors)) +
  geom_vline(xintercept=stats[1,20], linetype="dashed", color="#FFFF00")+
  geom_vline(xintercept=stats[2,20], linetype="dashed", color="#FFCC00")+
  geom_vline(xintercept=stats[3,20], linetype="dashed", color="#FF6600")+
  geom_vline(xintercept=stats[4,20], linetype="dashed", color="#CC0000")+
  geom_vline(xintercept=stats[5,20], linetype="dashed", color="#660000")+
  xlab("Responsabilidade e compromisso")+
  ylab("Densidade")

prop_tol_idade <- ggplot(subset(base, Idade %in% c("14","15","16","17","18")),
                         aes(x = score_tol, color=`Idade`))+
  geom_density()+ scale_color_manual(values=c(my_colors)) +
  geom_vline(xintercept=stats[1,21], linetype="dashed", color="#FFFF00")+
  geom_vline(xintercept=stats[2,21], linetype="dashed", color="#FFCC00")+
  geom_vline(xintercept=stats[3,21], linetype="dashed", color="#FF6600")+
  geom_vline(xintercept=stats[4,21], linetype="dashed", color="#CC0000")+
  geom_vline(xintercept=stats[5,21], linetype="dashed", color="#660000")+
  xlab("Tolerância a frutração")+
  ylab("Densidade")

#--Gráficos------------------------------------------------------------------------------


?ggsave

ggsave(plot = prop_atc_idade,"prop_atc_idade.png")
ggsave(plot = prop_aut_idade,"prop_aut_idade.png")
ggsave(plot = prop_ctd_idade,"prop_ctd_idade.png")
ggsave(plot = prop_mot_idade,"prop_mot_idade.png")
ggsave(plot = prop_adp_idade,"prop_adp_idade.png")
ggsave(plot = prop_aaa_idade,"prop_aaa_idade.png")
ggsave(plot = prop_ldc_idade,"prop_ldc_idade.png")
ggsave(plot = prop_mdc_idade,"prop_mdc_idade.png")
ggsave(plot = prop_plc_idade,"prop_plc_idade.png")
ggsave(plot = prop_atm_idade,"prop_atm_idade.png")
ggsave(plot = prop_cli_idade,"prop_cli_idade.png")
ggsave(plot = prop_cpt_idade,"prop_cpt_idade.png")
ggsave(plot = prop_crp_idade,"prop_crp_idade.png")
ggsave(plot = prop_cmd_idade,"prop_cmd_idade.png")
ggsave(plot = prop_cmn_idade,"prop_cmn_idade.png")
ggsave(plot = prop_coo_idade,"prop_coo_idade.png")
ggsave(plot = prop_flx_idade,"prop_flx_idade.png")
ggsave(plot = prop_rec_idade,"prop_rec_idade.png")
ggsave(plot = prop_tol_idade,"prop_tol_idade.png")










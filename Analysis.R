library(dplyr)
library(lme4)
library(lmerTest)
library(car)

# Wave 1 exclusions
# exclude participants with no data for calories  
ELSA12 <- ELSA11[complete.cases(ELSA11[, "escore_phdi"]),] 
nrow(ELSA12) # 15081

# exclude participants with extreme calorie intake 
quantile(ELSA12$calorias, probs = seq(0, 1, 0.01), na.rm = TRUE)
# 1% = 1146.9083, 5% = 1514.8349, 95% = 5263.3154, 99% = 7166.6020

ELSA13 <- subset(ELSA12, calorias >= 1146.9083 & calorias <= 7166.6020) #99TH
nrow(ELSA13) 


# exclude participants with missing data for cognition 
ELSA14 <- ELSA13[complete.cases(ELSA13[, c("a_memoria", "a_memoria_tardia", 
                                           "a_memoria_reconhecimento", 
                                           "a_fluencia_animais", 
                                           "a_fluencia_letraf", 
                                           "a_trilha_segundos")]),] 
nrow(ELSA14)

# exclude participants with missing data for covariates
ELSA15 <- ELSA14[complete.cases(ELSA14[, c("AGEPART", "a_rendapercapita", 
                                           "RCTA8", "a_escolar", "a_imc1", 
                                           "a_imc2", "a_has2_2", "a_sfcvdhard",
                                           "a_dm_3", "a_fumante", "a_usodealcool", 
                                           "a_sintdep", "a_ativfisica", 
                                           "race")]),] 
nrow(ELSA15)

# exclude participants who take medicine for psychiatric disease (neuroleptics, 
# antiparkinsonian agents, and anticonvulsants) 
ELSA16 <- ELSA15[ELSA15$cog_drug == 0, ] 
nrow(ELSA16)

#exclude participants with Parkinson or Dementia 
ELSA18 <- ELSA16[ELSA16$ParkDem == 0, ] 
nrow(ELSA18)

# Waves 2 and 3 exclusions
# Wave 2 deceased
ELSA18a <- subset(ELSA18, Obito2 == 0) 
ELSA18a %>% count(Obito2) #check
nrow(ELSA18a)

# Wave 2 lost to follow-up
ELSA18b <- subset(ELSA18a, Participacao2 == 1) 
ELSA18b %>% count(Participacao2) #check
nrow(ELSA18b)

# Wave 2 without cognitive testing (< 55 years)
ELSA18c <- subset(ELSA18b, AGEPART2 >= 55) 
ELSA18c %>% count(AGEPART2) #check
nrow(ELSA18c)

# Wave 2 missing data for cognition
ELSA18d <- ELSA18c[complete.cases(ELSA18c[, c("a_memoria2", "a_memoria_tardia2", 
                                           "a_memoria_reconhecimento2", 
                                           "a_fluencia_animais2", 
                                           "a_fluencia_letraf2", 
                                           "a_trilha_segundos2")]),]
nrow(ELSA18d)

# Reintroduce < 55 years in wave 2
ELSA18e <- subset(ELSA18b, AGEPART2 < 55) #create subset with only <55 years
nrow(ELSA18e)
ELSA18f <- bind_rows(x = ELSA18d, y = ELSA18e) #add 
nrow(ELSA18f)

# Wave 3 deceased
ELSA18g <- subset(ELSA18f, Obito3 == 0) 
ELSA18g %>% count(Obito3) #check
nrow(ELSA18g)

# Wave 3 lost to follow-up
ELSA18h <- subset(ELSA18g, Participacao3 == 1) 
ELSA18h %>% count(Participacao3) #check
nrow(ELSA18h)

# Wave 3 missing data for cognition
ELSA18i <- ELSA18h[complete.cases(ELSA18h[, c("a_memoria3", "a_memoria_tardia3", 
                                           "a_memoria_reconhecimento3", 
                                           "a_fluencia_animais3", 
                                           "a_fluencia_letraf3", 
                                           "a_trilha_segundos3")]),]
nrow(ELSA18i)

# Z-score 
ELSA18$ZscoreMemory1 <- ((ELSA18$a_memoria - mean(ELSA18$a_memoria, na.rm = TRUE)) / 
                               sd(ELSA18$a_memoria, na.rm = TRUE)) #memory learning
ELSA18$ZscoreMemoryDelayed1 <- ((ELSA18$a_memoria_tardia - 
                                      mean(ELSA18$a_memoria_tardia, na.rm = TRUE)) / 
                               sd(ELSA18$a_memoria_tardia, na.rm = TRUE)) #memory delayed
ELSA18$ZscoreMemoryRec1 <- ((ELSA18$a_memoria_reconhecimento - 
                                  mean(ELSA18$a_memoria_reconhecimento, na.rm = TRUE)) / 
                               sd(ELSA18$a_memoria_reconhecimento, na.rm = TRUE)) #memory recognition
ELSA18$ZscoreAnimal1 <- ((ELSA18$a_fluencia_animais - 
                               mean(ELSA18$a_fluencia_animais, na.rm = TRUE)) / 
                               sd(ELSA18$a_fluencia_animais, na.rm = TRUE)) #verbal fluency animals
ELSA18$ZscoreLetterF1 <- ((ELSA18$a_fluencia_letraf - 
                                mean(ELSA18$a_fluencia_letraf, na.rm = TRUE)) / 
                               sd(ELSA18$a_fluencia_letraf, na.rm = TRUE)) #verbal fluency letter f
ELSA18$ZscoreTrail1a <- ((ELSA18$a_trilha_segundos -
                               mean(ELSA18$a_trilha_segundos, na.rm = TRUE)) / 
                               sd(ELSA18$a_trilha_segundos, na.rm = TRUE)) #trails
ELSA18$ZscoreTrail1 <- ELSA18$ZscoreTrail1a * (-1) #trails

#Create global cognition scores
# Memory
ELSA19 <- ELSA18 %>% 
  rowwise() %>%
  mutate(MeanZscoreMemoryTotal1 = (ZscoreMemory1 + ZscoreMemoryDelayed1 + 
                                         ZscoreMemoryRec1) / 3) #mean memory zscore

# Executive function
ELSA20 <- ELSA19 %>% 
  rowwise() %>%
  mutate(MeanZscoreExecutive1 = (ZscoreAnimal1 + ZscoreLetterF1 + ZscoreTrail1) / 3) #mean executive zscore


# Verbal Fluency
ELSA21 <- ELSA20 %>% 
  rowwise() %>%
  mutate(MeanZscoreFluency1 = (ZscoreAnimal1 + ZscoreLetterF1) / 2) #mean verbal fluency zscore


# Global cognition
ELSA22 <- ELSA21 %>% 
  rowwise() %>%
  mutate(MeanZscoreGlobal1 = (ZscoreMemory1 + ZscoreMemoryDelayed1 + 
                                ZscoreMemoryRec1 + ZscoreAnimal1 + 
                                ZscoreLetterF1 + ZscoreTrail1) / 6) #mean global zscore


# Divide EAT-Lancet score into quintiles
ELSA30 <- ELSA22
ELSA30$phdi_quintiles <- as.factor(ntile(ELSA30$escore_phdi, 5)) 

ELSA30 %>%
  dplyr::count(phdi_quintiles)


# Regressions
## Memory
### Unadjusted
Mem_0 <- lmer(MemoriaGlobal ~ as.factor(phdi_quintiles)*Idade + 
                (1 + Idade | idelsa), weights = gweight_final, ELSA30)
summary(Mem_0) #results
confint(Mem_0, method = "Wald") #confidence interval
Anova(Mem_0, type = "III") #chi-square wald from car package (p for trend)

### Model 1
Mem_1 <- lmer(MemoriaGlobal ~ as.factor(phdi_quintiles)*Idade +
                                          (1 + Idade | idelsa) + age_a + RCTA8 + escolar + 
                                          a_rendapercapita + race, 
                                        weights = gweight_final, ELSA30)
summary(Mem_1) #results
confint(Mem_1 , method = "Wald") #confidence interval
Anova(Mem_1, type = "III") #chi-square wald from car package (p for trend)

### Model 2
Mem_2 <- lmer(MemoriaGlobal ~ as.factor(phdi_quintiles)*Idade + 
                                          (1 + Idade | idelsa) + age_a + RCTA8 + escolar + 
                                          a_rendapercapita + race + a_usodealcool + 
                                          a_fumante + a_dm_3 + a_has2_2 + a_sfcvdhard + 
                                          a_ativfisica + a_sintdep + calorias + 
                                          a_imc1,, weights = gweight_final, ELSA30)
summary(Mem_2) #results
confint(Mem_2, method = "Wald") #confidence interval
Anova(Mem_2, type = "III") #chi-square wald from car package (p for trend)

## Verbal Fluency
### Unadjusted
Flu_0 <- lmer(FluenciaGlobal ~ as.factor(phdi_quintiles)*Idade + 
                (1 + Idade | idelsa), weights = gweight_final, ELSA30)
summary(Flu_0) #results
confint(Flu_0, method = "Wald") #confidence interval
Anova(Flu_0, type = "III") #chi-square wald from car package (p for trend)

### Model 1
Flu_1 <- lmer(FluenciaGlobal ~ as.factor(phdi_quintiles)*Idade +
                                          (1 + Idade | idelsa) + age_a + RCTA8 + escolar + 
                                          a_rendapercapita + race, 
                                        weights = gweight_final, ELSA30)
summary(Flu_1) #results
confint(Flu_1 , method = "Wald") #confidence interval
Anova(Flu_1, type = "III") #chi-square wald from car package (p for trend)

### Model 2
Flu_2 <- lmer(FluenciaGlobal ~ as.factor(phdi_quintiles)*Idade + 
                                          (1 + Idade | idelsa) + age_a + RCTA8 + escolar + 
                                          a_rendapercapita + race + a_usodealcool + 
                                          a_fumante + a_dm_3 + a_has2_2 + a_sfcvdhard + 
                                          a_ativfisica + a_sintdep + calorias + 
                                          a_imc1, weights = gweight_final, ELSA30)
summary(Flu_2) #results
confint(Flu_2, method = "Wald") #confidence interval
Anova(Flu_2, type = "III") #chi-square wald from car package (p for trend)

## Trail
### Unadjusted
Tri_0 <- lmer(Trilhas ~ as.factor(phdi_quintiles)*Idade + 
                (1 + Idade | idelsa), weights = gweight_final, ELSA30)
summary(Tri_0) #results
confint(Tri_0, method = "Wald") #confidence interval
Anova(Tri_0, type = "III") #chi-square wald from car package (p for trend)

### Model 1
Tri_1 <- lmer(Trilhas ~ as.factor(phdi_quintiles)*Idade +
                                          (1 + Idade | idelsa) + age_a + RCTA8 + escolar + 
                                          a_rendapercapita + race, 
                                        weights = gweight_final, ELSA30)
summary(Tri_1) #results
confint(Tri_1 , method = "Wald") #confidence interval
Anova(Tri_1, type = "III") #chi-square wald from car package (p for trend)

### Model 2
Tri_2 <- lmer(Trilhas ~ as.factor(phdi_quintiles)*Idade + 
                                          (1 + Idade | idelsa) + age_a + RCTA8 + escolar + 
                                          a_rendapercapita + race + a_usodealcool + 
                                          a_fumante + a_dm_3 + a_has2_2 + a_sfcvdhard + 
                                          a_ativfisica + a_sintdep + calorias + 
                                          a_imc1, weights = gweight_final, ELSA30)
summary(Tri_2) #results
confint(Tri_2, method = "Wald") #confidence interval
Anova(Tri_2, type = "III") #chi-square wald from car package (p for trend)

## Global
### Unadjusted
Glo_0 <- lmer(CognicaoGlobal ~ as.factor(phdi_quintiles)*Idade + 
                (1 + Idade | idelsa), weights = gweight_final, ELSA30)
summary(Glo_0) #results
confint(Glo_0, method = "Wald") #confidence interval
Anova(Glo_0, type = "III") #chi-square wald from car package (p for trend)

### Model 1
Glo_1 <- lmer(CognicaoGlobal ~ as.factor(phdi_quintiles)*Idade +
                                          (1 + Idade | idelsa) + age_a + RCTA8 + escolar + 
                                          a_rendapercapita + race, 
                                        weights = gweight_final, ELSA30)
summary(Glo_1) #results
confint(Glo_1 , method = "Wald") #confidence interval
Anova(Glo_1, type = "III") #chi-square wald from car package (p for trend)

### Model 2
Glo_2 <- lmer(CognicaoGlobal ~ as.factor(phdi_quintiles)*Idade + 
                                          (1 + Idade | idelsa) + age_a + RCTA8 + escolar + 
                                          a_rendapercapita + race + a_usodealcool + 
                                          a_fumante + a_dm_3 + a_has2_2 + a_sfcvdhard + 
                                          a_ativfisica + a_sintdep + calorias + 
                                          a_imc1, weights = gweight_final, ELSA30)
summary(Glo_2) #results
confint(Glo_2, method = "Wald") #confidence interval
Anova(Glo_2, type = "III") #chi-square wald from car package (p for trend)

# Plots
## Plot Memory
MemT3 <- plot_model(Mem_2, type = "pred", 
                       terms = c("Idade", "phdi_quintiles"), ci.lvl = NA)
MemT32 <- MemT3 +
  aes(linetype=group, color=group) + #keeps only one legend
  scale_linetype_manual(labels = c('1', '2', '3', '4', '5'), 
                        values = c("solid", "dashed", "dotdash", "dotted", "twodash")) + 
  scale_color_manual(labels = c('1', '2', '3', '4', '5'), 
                     values = c("#D43F3A", "#298ACD", "#5CB85C", "#9632B8", "#FA8128")) +
  scale_y_continuous(limits = c(-0.4, 0.5)) + #change limits of y axis 
  labs(title = " ", #add title (in this case, no title)
       subtitle = "p = 0.046", #add p-value as a subtitle
       y = "Memory", #add y-axis label
       x = "Age (years)", #add x-axis label
       linetype = "Quintile of PHDI",
       color = "Quintile of PHDI") +  #only one legend
  theme(plot.title = element_text(hjust = 0.5), #center title
        plot.subtitle = element_text(hjust = 1), #right align p-value
        axis.line.x = element_line(color = "black"), #add x axis line
        axis.line.y = element_line(color = "black"), #add y axis line
        panel.background = element_blank(), #delete gray background
        panel.grid = element_blank()) #delete grid lines

MemT32 #visualize

## Plot Verbal fluency
FluT3 <- plot_model(Flu_2, type = "pred", 
                       terms = c("Idade", "phdi_quintiles"), ci.lvl = NA)
FluT32 <- FluT3 +
  aes(linetype=group, color=group) + #keeps only one legend
  scale_linetype_manual(labels = c('1', '2', '3', '4', '5'), 
                        values = c("solid", "dashed", "dotdash", "dotted", "twodash")) + 
  scale_color_manual(labels = c('1', '2', '3', '4', '5'), 
                     values = c("#D43F3A", "#298ACD", "#5CB85C", "#9632B8", "#FA8128")) +
  scale_y_continuous(limits = c(-0.4, 0.5)) + #change limits of y axis 
  labs(title = " ", #add title (in this case, no title)
       subtitle = "p = 0.279", #add p-value as a subtitle
       y = "Verbal fluency", #add y-axis label
       x = "Age (years)", #add x-axis label
       linetype = "Quintile of PHDI",
       color = "Quintile of PHDI") +  #only one legend
  theme(plot.title = element_text(hjust = 0.5), #center title
        plot.subtitle = element_text(hjust = 1), #right align p-value
        axis.line.x = element_line(color = "black"), #add x axis line
        axis.line.y = element_line(color = "black"), #add y axis line
        panel.background = element_blank(), #delete gray background
        panel.grid = element_blank()) #delete grid lines

FluT32 #visualize

## Plot executive function
TrailT3 <- plot_model(Tri_2, type = "pred", 
                       terms = c("Idade", "phdi_quintiles"), ci.lvl = NA)
TrailT32 <- TrailT3 +
  aes(linetype=group, color=group) + #keeps only one legend
  scale_linetype_manual(labels = c('1', '2', '3', '4', '5'), 
                        values = c("solid", "dashed", "dotdash", "dotted", "twodash")) + 
  scale_color_manual(labels = c('1', '2', '3', '4', '5'), 
                     values = c("#D43F3A", "#298ACD", "#5CB85C", "#9632B8", "#FA8128")) +
  scale_y_continuous(limits = c(-0.4, 0.5)) + #change limits of y axis 
  labs(title = " ", #add title (in this case, no title)
       subtitle = "p = 0.531", #add p-value as a subtitle
       y = "Executive function", #add y-axis label
       x = "Age (years)", #add x-axis label
       linetype = "Quintile of PHDI",
       color = "Quintile of PHDI") +  #only one legend
  theme(plot.title = element_text(hjust = 0.5), #center title
        plot.subtitle = element_text(hjust = 1), #right align p-value
        axis.line.x = element_line(color = "black"), #add x axis line
        axis.line.y = element_line(color = "black"), #add y axis line
        panel.background = element_blank(), #delete gray background
        panel.grid = element_blank()) #delete grid lines

TrailT32 #visualize

## Plot global cognition
GlobalT3 <- plot_model(Glo_2, type = "pred", 
                       terms = c("Idade", "phdi_quintiles"), ci.lvl = NA)
GlobalT32 <- GlobalT3 +
  aes(linetype=group, color=group) + #keeps only one legend
  scale_linetype_manual(labels = c('1', '2', '3', '4', '5'), 
                        values = c("solid", "dashed", "dotdash", "dotted", "twodash")) + 
  scale_color_manual(labels = c('1', '2', '3', '4', '5'), 
                     values = c("#D43F3A", "#298ACD", "#5CB85C", "#9632B8", "#FA8128")) +
  scale_y_continuous(limits = c(-0.4, 0.5)) + #change limits of y axis 
  labs(title = " ", #add title (in this case, no title)
       subtitle = "p = 0.090", #add p-value as a subtitle
       y = "Global cognition", #add y-axis label
       x = "Age (years)", #add x-axis label
       linetype = "Quintile of PHDI",
       color = "Quintile of PHDI") +  #only one legend
  theme(plot.title = element_text(hjust = 0.5), #center title
        plot.subtitle = element_text(hjust = 1), #right align p-value
        axis.line.x = element_line(color = "black"), #add x axis line
        axis.line.y = element_line(color = "black"), #add y axis line
        panel.background = element_blank(), #delete gray background
        panel.grid = element_blank()) #delete grid lines

GlobalT32 #visualize

### Combine all graphs
CombinedEAT_main <- ggarrange(MemT32, FluT32, TrailT32, GlobalT32,
                           labels = c("A", "B", "C", "D"),
                           ncol = 2, nrow = 2,
                           common.legend = TRUE, legend = "bottom")
ggexport(CombinedEAT_main2)

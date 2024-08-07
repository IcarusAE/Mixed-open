library(tidyverse)
library(readxl)

#<#############################################################################>
# 7 Einlesen und Vorbereiten der Daten----
#<#############################################################################>

#* 7.2.1 Einlesen study-data sheet----
#<#############################################################################>
study_data <- read_excel("customer_aggression.xlsx",
                         sheet = "study_data")

glimpse(study_data)




# Rekodierungen von Moderatoren
#<===========================================================>

study_data %>% 
  count(job_cat, sort = TRUE)


# Möglichkeit 1: Gering besetzte level automatisch zusammenlegen
study_data %>% 
  mutate(job_cat2 = fct_lump(job_cat, n =3)) %>% 
  count(job_cat2, sort = TRUE)


# Möglichkeit 2: Eigen bestimmte Kategorien
study_data <- study_data %>% 
  mutate(job_cat2 = fct_collapse(job_cat, 
    'MixedJobs' = c('Mixed services','Others'),
    'CallCenter' = '42 Customer service clerks',
    'ServiceWorkers' = c('31 Science and engineering associate professionals',
                         '32 Health associate professionals',
                         '51 Personal service workers',
                         '52 Sales workers',
                         '54 Protective services workers')
   )
  ) 


# Check
study_data %>% 
  count(job_cat, job_cat2) %>% 
  spread(job_cat2, n)





#* 7.2.2	Einlesen der ES-Daten----
#<#############################################################################>

es_data <- read_excel("customer_aggression.xlsx",
                         sheet = "es_data")



#* 7.2.4 Mergen---- 
#<#############################################################################>

mdata <- study_data %>%
  left_join(es_data, by = "sample_id") %>%   #Joinen/mergen
  select(-summary,                           #Unnötige Variablen raus
         -ref,
         -job,
         -comment,
         -variables,
         -definition,
         -scale,
         -smpl.item,
         -label_x,
         -label_y,
         -pdf,
         -EmotLab) %>%
  filter(!is.na(corr_label1)) %>%   # Leere Zellen/unnötige Korrelationen raus
  mutate(sample_id = as.factor(sample_id)) #sample ID als factor

mdata




library(tidyverse)
library(metafor)

# Im folgenden werden die einzelnen Schritte separat behandelt (analog zum Skript)
# Anschließend folgt die Beschreibung der three-level MA. Zum Schluss wird ein 
# gesamter workflow beschrieben, der in einer Meta-analyse so durchexerziert wird



#<#############################################################################>
#* 7.3	Vorbereitung der Daten: Berechnung der sampling variance----
#<#############################################################################>

data_ca_dist <- mdata %>%
  filter(corr_label1 == "ca.distress") %>%   #Korrelation auswählen
  mutate(es_id = full_seq(1:nrow(.), 1)) %>%  #ES ID anlegen
  escalc(measure="COR", data=., ri=r, ni=n, append=T) %>%  #Smpl var. berechnen
  tibble()    #In tibble umwandeln (escalc generiert einen dataframe)

data_ca_dist #checken




#<#############################################################################>
# 8 Two level Modelle----
#<#############################################################################>


ca_dist_metafor <- rma(
  yi,
  vi,
  data = data_ca_dist,
  method = "ML"
)
summary(ca_dist_metafor)

confint(ca_dist_metafor)




#<#############################################################################>
# 9	Heterogenität und Outlier-Analyse---- 
#<#############################################################################>

# Die Outlieranalyse basiert auf dem Two-level modell. Dies ist der (einzige)
# Grund, überhaupt ein Two-level Modell zu machen. Später (siehe Abschnitt "workflow")
# Wird zu erst ein three-level modell gemacht und anschließend ein two-level modell
# für die Analyse von Outliern und Publication bias



#* 9.1 Forest plot----
#<#############################################################################>

#windows()
forest(ca_dist_metafor, slab=paste(data_ca_dist$source, sep = ","))



#* 9.2 Outlier----
#<####################################>

# Visualisierung
#<====================================>
# Plot
# windows()
plot(influence(ca_dist_metafor))



# Wer sind die Studien?
data_ca_dist %>% 
  slice(41, 42)  



# Robustness check: Two-level model ohne outlier 
#<====================================>
# Base R variante
rma(
  yi, 
  vi, 
  data = data_ca_dist[-c(41, 42), ]) %>% 
  summary() 

# Tidy / pipeline-Variante
data_ca_dist %>% 
  slice(-c(41,42)) %>% 
  rma(
    yi, 
    vi, 
    data = . ) %>% 
  summary()




#<#############################################################################>
# 10 Publication bias----
#<#############################################################################>

#* 10.1 Funnel plot----
#<#############################################################################>

funnel(ca_dist_metafor)

#funnel plot ohne die outlier 41 und 42
data_ca_dist %>% 
  slice(-c(41,42)) %>% 
  rma(
    yi, 
    vi, 
    data = . ) %>% 
  funnel()



#* 10.2	Egger's Regression test----
#<#############################################################################>

regtest(ca_dist_metafor)



#* 10.3	Rangkorrelationentest (Begg & Mazumdar)----
#<#############################################################################>

ranktest(ca_dist_metafor)




#* 10.4	Trim & Fill----
#<#############################################################################>

trimfill(ca_dist_metafor)



#* 10.5	Integration aller Analysen mit einer eigenen Funktion----
#<#############################################################################>

pub_bias(ca_dist_metafor)




#<#############################################################################>
# 11 Three level Modelle----
#<#############################################################################>

#* 11.2	Three-level-MA mit metafor---- 
#<#############################################################################>

ca_dist_metafor <- rma.mv(
  yi,
  vi,
  random = ~ 1 | sample_id / es_id,
  data = data_ca_dist,
  method = "ML"
)

summary(ca_dist_metafor)

confint(ca_dist_metafor)





#* 11.3	Three-level-MA mit metaSEM ----
#<#############################################################################>


ca_dist_metaSEM <- meta3L(
  y = yi,
  v = vi,
  cluster = sample_id,
  data = data_ca_dist
)
summary(ca_dist_metaSEM)



#* 11.4	Berechnung von k, m und N----
#<#############################################################################>

data_ca_dist %>% 
  summarise(k = n_distinct(sample_id), 
            m = n(), 
            N = sum(n[!duplicated(sample_id)])
  )





#* 11.5	Workflow und meta_table() Funktion---- 
#<#############################################################################>

# Hier erfolgt eine Wiederholung der o.g. Teile in der von mir angewendeten
# Reihenfolge

# 1)	Subdatensatz ziehen
data_ca_dist <- mdata %>%
  filter(corr_label1 == "ca.distress") %>% 
  mutate(es_id = full_seq(1:nrow(.), 1)) %>% 
  escalc(measure="COR", data=., ri=r, ni=n, append=T) %>% 
  mutate(sample_id = as.factor(sample_id)) %>% 
  tibble()
data_ca_dist


# 2)	Three level Modelle rechnen
# Metafor
ca_dist_metafor <- rma.mv(
  yi,
  vi,
  random = ~ 1 | sample_id / es_id,
  data = data_ca_dist,
  method = "ML"
)

# metaSEM
ca_dist_metaSEM <- meta3L(
  y = yi,
  v = vi,
  cluster = sample_id,
  data = data_ca_dist
)
summary(ca_dist_metaSEM)


# 3)	Mit meta_table integrieren
meta_table(data_ca_dist, ca_dist_metafor, ca_dist_metaSEM)


# 4)	Two level Modell rechnen
ca_dist_2L <- rma(yi, vi, data = data_ca_dist, method="ML")


# 5)	Publication bias und outlier
#	Publication bias
pub_bias(ca_dist_2L)

#	Outlier
windows()
plot(influence(ca_dist_2L))

# Outlier eliminieren
rma(
  yi, 
  vi, 
  data = data_ca_dist[-c(41, 42), ]) %>% 
  summary()





#<#############################################################################>
# 12	Test von Moderatoren----
#<#############################################################################>

#* 12.2 Kontinuierliche Moderatoren----
#<#############################################################################>

# Metafor
#<----------------------->
ca_dist_pctfem_mf <- rma.mv(yi, vi, random = ~ 1 | sample_id/es_id, 
                            mods =~ pct_fem,
                            data = data_ca_dist, 
                            method="ML")
summary(ca_dist_pctfem_mf)




# MetaSEM
#<----------------------->
ca_dist_pctfem_m3 <- meta3L(y = yi, v = vi,  
                            cluster = sample_id,  
                            x = pct_fem, 
                            data = data_ca_dist)

summary(ca_dist_pctfem_m3)




#* 12.4	Kategoriale Moderatoren---- 
#<#############################################################################>

#* 12.4.1	ME model mit metafor
#<----------------------->

# No-intercept model
ca_dist_jobcat_mf <- rma.mv(yi, vi, random = ~ 1 | sample_id/es_id, 
                            mods =~ job_cat2 - 1,
                            data = data_ca_dist, 
                            method="ML")
summary(ca_dist_jobcat_mf)

# With-intercept model
ca_dist_jobcat_mf <- rma.mv(yi, vi, random = ~ 1 | sample_id/es_id, 
                            mods =~ job_cat2,
                            data = data_ca_dist, 
                            method="ML")
summary(ca_dist_jobcat_mf)


# Referenz wecheln
ca_dist_jobcat_mf <- rma.mv(yi, vi, random = ~ 1 | sample_id/es_id, 
                            mods =~ relevel(job_cat2, ref = "CallCenter"),
                            data = data_ca_dist, 
                            method="ML")
summary(ca_dist_jobcat_mf)




# 12.4.2	Berechnung mit metaSEM
#<----------------------->

# Dummies fuer meta3L bilden

data_ca_dist <- data_ca_dist %>% 
  mutate(
    jobcat_CallCenter = as.numeric(job_cat2 == "CallCenter"), 
    jobcat_Mixed      = as.numeric(job_cat2 == "MixedJobs")
  )

# checken
data_ca_dist %>% 
  count(job_cat2, jobcat_CallCenter, jobcat_Mixed)


# Modell
ca_dist_time_m3 <- meta3L(y = yi, v = vi,  
                          cluster = sample_id,  
                          x = cbind(jobcat_CallCenter, jobcat_Mixed), 
                          data = data_ca_dist)

summary(ca_dist_time_m3)


# 12.5	Berechnung von k, m und N pro Moderator-Kategorie
#<----------------------->
data_ca_dist %>% 
  group_by(job_cat2) %>%  
  summarise(k = n_distinct(sample_id), 
            m = n(), 
            N = sum(n[!duplicated(sample_id)])
  ) %>% 
  print(n=nrow(.)) %>%
  #select(-1) %>% 
  write.table(., "clipboard", sep="\t", row.names=FALSE, col.names=FALSE)






#* 12.7	Workflow und mara_table-Funktion----
#<#############################################################################>

# 1) k, m und N berechnen
data_ca_dist %>% 
  group_by(job_cat2) %>%  
  summarise(k = n_distinct(sample_id), 
            m = n(), 
            N = sum(n[!duplicated(sample_id)])
  ) %>% 
  print(n=nrow(.)) %>%
  #select(-1) %>% 
  write.table(., "clipboard", sep="\t", row.names=FALSE, col.names=FALSE)


# 2) Dummies fuer meta3L bilden
data_ca_dist <- data_ca_dist %>% 
  mutate(
    jobcat_CallCenter = as.numeric(job_cat2 == "CallCenter"), 
    jobcat_Mixed      = as.numeric(job_cat2 == "MixedJobs")
  )


# 3) metaSEM und metafor model rechnen  
ca_dist_time_m3 <- meta3L(y = yi, v = vi,  
                          cluster = sample_id,  
                          x = cbind(jobcat_CallCenter, jobcat_Mixed), 
                          data = data_ca_dist)


ca_dist_jobcat_mf <- rma.mv(yi, vi, random = ~ 1 | sample_id/es_id, 
                            mods =~ job_cat2 - 1,
                            data = data_ca_dist, 
                            method="ML")



# 4) Integrieren und in die Exceltabelle kopieren
mara_table(ca_dist_jobcat_mf, ca_dist_time_m3)



# 5)	Zum Schluss noch der Chi-Quadrat-Differenztest
ca_dist_jobcat_mf <- rma.mv(yi, vi, random = ~ 1 | sample_id/es_id, 
                            mods =~ job_cat2,
                            data = data_ca_dist, 
                            method="ML")
summary(ca_dist_jobcat_mf)



# CLAIMS ANALYSIS
# GEORGIA JENKINS, 13 FEB 2024

# BACKGROUND -------------------------------------------------------------------
# Aim: Come up with a list of top 10 referral providers based on cost and volume, 
# in the immediate vicinity of UHC, for colonoscopy procedures 

# CPT codes: 45378,45380,45384, 45385,45388  
# Year: 2020

# Data set: a set of claims that encompass all colonoscopies in the UHC catchment area.

rm(list=ls())

pkgs = c('haven','tidyverse','dplyr','lubridate','zoo',"data.table", "DT","flextable",'ggplot2','scales')
eval(parse(text =paste0('suppressPackageStartupMessages(library(',pkgs,')',')') ))

# Load the data set
colonoscopies = read.csv("claims_data.csv") 

# Get a first look at the data 
names(colonoscopies) # 28 columns
summary(colonoscopies) 
length(colonoscopies$clm_num) # 2,099 claims

# CLEANING: MATCH ASSIGNMENT CRITERIA ------------------------------------------

# Confirm provider zip codes are located near UHC
table(colonoscopies$prov_city,colonoscopies$prov_zip) # All in Manhattan

# Confirm CPT codes are 45378, 45380,45384, 45385, & 45388 
table(colonoscopies$cpt_code) #Includes EDG codes 43200,43235,43239. Excludes 45388
# Remove rows with extra CPT codes 
cpt.code.list = c(45378,45380,45384,45385,45388)
colonoscopies = colonoscopies %>% filter(is.element(cpt_code,cpt.code.list))

# Check summary again
summary(colonoscopies) # 1375 records remaining

# Filter to procedures that happened in 2020
colonoscopies$procedure.date = as.Date(colonoscopies$start_dte,"%m/%d/%Y")
# View(colonoscopies[is.na(colonoscopies$procedure.date),]) # invalid date: 2/29/2019
colonoscopies2020 = colonoscopies %>% filter(year(procedure.date) == 2020)
# Check procedure end date
max(as.Date(colonoscopies2020$end_dte,"%m/%d/%Y")) #All procedures ended in 2020

# Check summary again
summary(colonoscopies2020) # 773 records remaining

# CLEANING: DUPLICATES & INVALID VALUES ----------------------------------------

# Paid amount == 999999 ----
# # The max paid_amount == 999999. Look for a pattern, assess prevalence.
# View(colonoscopies2020[colonoscopies2020$paid_amount == 999999,])
# # Only 4 records, 2 sets of duplicate claim numbers with different CPT codes. 
# # All 4 have ICD codes D126, D125, & Z1211. Look at all combos of these codes.
# View(colonoscopies2020[(colonoscopies2020$icd10_1 == "Z1211" |
#                          colonoscopies2020$icd10_2 == "Z1211" |
#                          colonoscopies2020$icd10_3 == "Z1211") & 
#                          (colonoscopies2020$icd10_1 == "D125" |
#                             colonoscopies2020$icd10_2 == "D125" |
#                             colonoscopies2020$icd10_3 == "D125")&
#                          (colonoscopies2020$icd10_1 == "D126" |
#                             colonoscopies2020$icd10_2 == "D126" |
#                             colonoscopies2020$icd10_3 == "D126"),])
# # All 16 have 2 CPT codes per claim number. Always 45380 with 45384 or 45385.
# # The rows with 45380 are paid if they have a modifier code. 
# # If the modifier isn't there, the claim is rejected. 
# # The rejected 45380 claims have paid amount == 0, which doesn't explain 999999.
# # Assume 999999 represents an error code. 
# # Maybe it was incorrectly submitted and cancelled by the provider? 
# # Remove claims with 999999 paid amount (4 claims)
colonoscopies2020 = colonoscopies2020 %>% filter(paid_amount != 999999)

# Duplicates ----
# Check for exact duplicates
sum(duplicated(colonoscopies2020)==T) # no exact duplicate rows

# # Check for claim number duplicates
# View(colonoscopies2020[duplicated(colonoscopies2020$clm_num),]) 
# # 131 claim number dups, but maybe multiple procedures. Don't filter yet.

# Check for rows where claim number, dates, provider, & cpt_code are the same
colonoscopies2020 = colonoscopies2020 %>%
  group_by(clm_num, provider, start_dte, end_dte, cpt_code) %>%
  mutate(
    dupe = ifelse(length(clm_num)>1,1,0)
  )
# View(colonoscopies2020[colonoscopies2020$dupe == 1,])
# # 12 sets of dupes. Each set has one claim with paid_amount == 0 & one > 0.

# Remove duplicates, but for each set of dups, keep the row that has a paid_amount
colonoscopies2020$dupe = ifelse(colonoscopies2020$dupe == 1 & colonoscopies2020$paid_amount == 0,1,0)
# View(colonoscopies2020[colonoscopies2020$dupe == 1,])
colonoscopies2020 = colonoscopies2020 %>% filter(dupe == 0)

# Check other variables in case you missed anything ----
# From Scott: Can assume all reject codes mean the same thing 
unique(colonoscopies2020$reject_code) #NA == not rejected

unique(colonoscopies2020$modifier) #"" == not modified
# Modifier 59: distinct service
# Modifier SU - more complex/more time required than usual
# Modifier 33: preventive service
# Modifier 53: partially completed procedure

unique(colonoscopies2020$provider) #19 providers
unique(colonoscopies2020$specialty_type_desc) #Internal med, gastroenterology
unique(colonoscopies2020$place_service) #All == 11
summary(colonoscopies2020) #757 records

# QUESTIONS -----------------------------------------------------------------

# Make plot colors
seiu.purple = rgb(72,42,132,maxColorValue = 255)
seiu.yellow = rgb(255,242,0,maxColorValue = 255)

# 1A.	Total volume of colonoscopy procedures ----
total.vol.data = colonoscopies2020 %>%
  group_by(as.yearmon(procedure.date)) %>%
  summarise(no.of.procedures = length(clm_num),
            no.of.patients = length(unique(clm_num)))
total.vol.data = rename(total.vol.data,procedure.date = names(total.vol.data[1]))

# Total volume plot
total.vol.plot = ggplot(total.vol.data,
                        aes(x = procedure.date,
                            y = no.of.procedures)) +
  geom_line(aes(linetype = "Procedures"),linewidth = 1.2,color = seiu.purple) +
  geom_point(color = seiu.purple)+
  geom_line(aes(y = no.of.patients, linetype = "Patients"),linewidth = 1.2,color = seiu.purple) +
  labs(title = "Monthly Volume of Colonoscopy Procedures, 2020",
       subtitle = paste("Annual Volume:",sum(total.vol.data$no.of.procedures),"procedures,",
                        sum(total.vol.data$no.of.patients),"patients"))+
  scale_linetype_manual(name = "Volume Metric",
                        values = c("Procedures" = "solid","Patients" = "dotted"),
                        labels = c("Procedures","Patients"),
                        guide = guide_legend(override.aes = list(linetype=c("solid","dotted"))))+
  scale_x_yearmon(name = NULL, labels = format(total.vol.data$procedure.date,"%b '%y"),
                  breaks = total.vol.data$procedure.date)+
  scale_y_continuous(name = "Volume (counts)", 
                     limits = c(0,max(total.vol.data$no.of.procedures) + 5))+
  theme_bw(base_size = 16) + 
  theme(axis.text.x = element_text(angle = 45,vjust=1,hjust=1),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

# 1B. & 2. Top 10 providers ----

# Make by-provider data frame with volume, mean & median costs per procedure
total.vol.by.provider = colonoscopies2020 %>%
  filter(is.na(reject_code)) %>% #added retroactively, see "Reject codes" section
  group_by(provider) %>%
  summarise(no.of.procedures = length(clm_num),
            no.of.patients = length(unique(clm_num)),
            mean.cost = mean(paid_amount),
            median.cost = median(paid_amount))
# Sort providers by volume
total.vol.by.provider = total.vol.by.provider[order(total.vol.by.provider$no.of.procedures,decreasing = T),]
# Get the top 10 by volume
top.10.providers = total.vol.by.provider[1:10,]
# 2 providers performed the majority of procedures

# Reject codes ----
# # MICH1336 (9th place) has cost == 0
# View(colonoscopies2020[colonoscopies2020$provider == "MICH1336",])
# # Because all their claims were rejected! Remove MICH1336?
# # 10th and 11th place were tied, so that should give us a nice round top 10. 

# # Check for other rejected claims among the top 10
# rejects = colonoscopies2020 %>% filter(is.element(provider,top.10.providers$provider) &
#                                          !is.na(reject_code))
# table(rejects$provider) #HON 32/389 (8%), NANC 12/289 (4%), MICH1336 4/4 (100%)
# 
# # Go back up to total.vol.by.provider and filter out the rejects.
# # Better for getting cost estimates since rejected claims are paid $0
# # Shouldn't affect volume rankings much. 

# Top 10 Table ----
# Make top 10 into a pretty table with a color gradient on the costs
top.10.providers = rename(top.10.providers,c(`Provider` = "provider",`Procedures` = "no.of.procedures",
                                             `Patients` = "no.of.patients", `Mean` = "mean.cost",`Median`="median.cost"))
top.10.providers$Mean = round(top.10.providers$Mean,digits = 0)
top.10.providers$Median = round(top.10.providers$Median,digits = 0)

top10table = flextable(top.10.providers)
top10table =  add_header_row(top10table,
                     colwidths = c(1,2, 2),
                     values = c("","Volume", "Cost per Procedure")
)

top10table = theme_box(top10table)
top10table = align(top10table, part = "header", align = "center")
top10table = align(top10table, j = 1, part = "header", align = "left")
top10table = align(top10table, j = 1, part = "body", align = "left")
top10table = align(top10table, j = 2:5, part = "body", align = "center")

colourer <- col_numeric(
  # palette =c("forestgreen","darkorange","firebrick"),
  # palette = rev(hcl.colors(6,"RdYlGn")),
  palette = c("lavender",seiu.purple),
  domain = c(3100,1100))
top10table = bg(top10table,
  bg = colourer,
  j = 4:5,
  part = "body")

# Export table from the viewer

# Top 3 providers ----
top3providers = top.10.providers[1:3,]
top3.all.procedures = colonoscopies2020 %>% filter(is.element(provider,top3providers$Provider) & is.na(reject_code))

# Add CPT code labels
top3.all.procedures = top3.all.procedures %>%
  mutate(cpt_label = case_when(
    cpt_code == 45378 ~ paste0("CPT Code: ",cpt_code,"\nDIAGNOSTIC"),
    cpt_code == 45380 ~ paste0("CPT Code: ",cpt_code,"\nWITH BIOPSY,\nSINGLE OR MULTIPLE"),
    cpt_code == 45384 ~ paste0("CPT Code: ",cpt_code,"\nWITH REMOVAL BY\nHOT BIOPSY FORCEPS"), 
    cpt_code == 45385 ~ paste0("CPT Code: ",cpt_code,"\nWITH REMOVAL BY\nSNARE TECHNIQUE"), 
    cpt_code == 45388 ~ paste0("CPT Code: ",cpt_code,"\nWITH ABLATION")
  ))
# For top 3, break down cost by CPT code
top.3.costs.by.cpt = top3.all.procedures %>%
  group_by(provider,cpt_code,cpt_label) %>%
  summarise(`Mean` = mean(paid_amount),
            `Median` = median(paid_amount),
            `no.of.procedures` = n())

# Top 3 mean cost heat map by CPT code ----
mean.cost.by.cpt = ggplot(top.3.costs.by.cpt, aes(x = provider, y = factor(cpt_label), fill = `Mean`)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lavender", 
                      high = seiu.purple,
                      breaks = c(min(top.3.costs.by.cpt$Mean),max(top.3.costs.by.cpt$Mean)),
                      labels = c("Lowest cost",
                                 "Highest cost")) +
  labs(title = "Mean Colonoscopy Costs by Provider",
       subtitle = '"COLONOSCOPY, FLEXIBLE" CPT Codes',
       x = "Provider",
       y = NULL,
       fill =  NULL) +
  geom_text(aes(label = paste0(dollar(`Mean`,accuracy = 1),
                "\nn = ",no.of.procedures)),size = 5)+
  theme_bw(base_size = 16) + 
  theme(axis.text.x = element_text(angle = 45,vjust=1,hjust=1),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold"))

# # Not much difference between median and mean, just show the means plot
# Medians plot ----
# median.cost.by.cpt = ggplot(top.3.costs.by.cpt, aes(x = provider, y = factor(cpt_label), fill = `Median`)) +
#   geom_tile(color = "white") +
#   scale_fill_gradient(low = "lavender", 
#                       high = seiu.purple,
#                       breaks = c(min(top.3.costs.by.cpt$Median),max(top.3.costs.by.cpt$Median)),
#                       labels = c("Lowest cost",
#                                  "Highest cost")) +
#   labs(title = "Median Colonoscopy Costs by Provider, 2020",
#        subtitle = '"COLONOSCOPY, FLEXIBLE" CPT Codes',
#        x = "Provider",
#        y = NULL,
#        fill = NULL) +
#   geom_text(aes(label = dollar(`Median`,accuracy = 1)),size = 5)+
#   # scale_fill_manual()+
#   theme_bw(base_size = 16) + 
#   theme(axis.text.x = element_text(angle = 45,vjust=1,hjust=1),
#         panel.grid = element_blank(),
#         plot.title = element_text(face = "bold"))
# Hide medians plot ----

# 3. Statistical significance ----
# Are the costs per procedure statistically different between the top 3 providers by volume? 

# Check assumptions for an ANOVA
# Independence observations assumed

# # Normal dist
# hist(top3.all.procedures$paid_amount[top3.all.procedures$provider == top3providers$Provider[1]])
# hist(top3.all.procedures$paid_amount[top3.all.procedures$provider == top3providers$Provider[2]])
# hist(top3.all.procedures$paid_amount[top3.all.procedures$provider == top3providers$Provider[3]])
# boxplot(top3.all.procedures$paid_amount[top3.all.procedures$provider == top3providers$Provider[1]])
# boxplot(top3.all.procedures$paid_amount[top3.all.procedures$provider == top3providers$Provider[2]])
# boxplot(top3.all.procedures$paid_amount[top3.all.procedures$provider == top3providers$Provider[3]])
# # Looks like there is clustering and outliers. Run Shapiro-Wilk test on each group
# shapiro.test(top3.all.procedures$paid_amount[top3.all.procedures$provider == top3providers$Provider[1]]) #p ~ 0
# shapiro.test(top3.all.procedures$paid_amount[top3.all.procedures$provider == top3providers$Provider[2]]) #p ~ 0
# shapiro.test(top3.all.procedures$paid_amount[top3.all.procedures$provider == top3providers$Provider[3]]) #p ~ 0
# Non-normal!

# # Homogeneity of variances 
# var(top3.all.procedures$paid_amount[top3.all.procedures$provider == top3providers$Provider[1]]) #76586
# var(top3.all.procedures$paid_amount[top3.all.procedures$provider == top3providers$Provider[2]]) #133474
# var(top3.all.procedures$paid_amount[top3.all.procedures$provider == top3providers$Provider[3]]) #20869
# # Unequal variances!

# Independent, non-parametric, unequal variances, and 3 groups
# Run a Kruskal-Wallis test
kw.result = kruskal.test(paid_amount ~ provider,
             data = top3.all.procedures) # p ~ 0 

# Check for differences between each pairing 
# Dunn's test
library(dunn.test)
dunn.test(top3.all.procedures$paid_amount, top3.all.procedures$provider,method="bonferroni", table = T)


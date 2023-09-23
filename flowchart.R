################################
## Data/numbers for flowchart ##
################################
## Load functions
source("functions/functions.R")

## Import data
datasets <- rofi::import_data()

## Merge data
combined.datasets <- rofi::merge_data(datasets)
## SIC(!) Temp fix
combined.datasets[combined.datasets$tra_id == "92386","tra_DodsfallsanalysGenomford"] <- 1
## Create OFI column
combined.datasets$ofi <- create_ofi2(combined.datasets)

#Remove documentation OFI
combined.datasets$ofi <- ifelse(grepl("Dokumetation|dokumentation|Dokumentation", combined.datasets$Problemomrade_.FMP), "No", combined.datasets$ofi)

dataset.clean.af <- clean_audit_filters(combined.datasets)

## Separate and store cases without known outcome
missing.outcome <- is.na(dataset.clean.af$ofi)
n.missing.outcome <- sum(missing.outcome)
dataset.clean.af <- dataset.clean.af[!missing.outcome, ]

## sep and store cases with age < 15
age15 <- dataset.clean.af[dataset.clean.af$pt_age_yrs <= 14, ]
n.under.15 <- nrow(age15)

dataset.comp.age <- dataset.clean.af[dataset.clean.af$pt_age_yrs > 14, ]

## Fix formating and remove wrong values like 999
clean.dataset <- clean_all_predictors(dataset.comp.age)

n.not.ofi.screened <- sum(is.na(combined.datasets$ofi))


df <- subset(clean.dataset, clean.dataset$tra_DodsfallsanalysGenomford == 1)
n.df <- nrow(df)
df2 <- subset(clean.dataset, clean.dataset$Fr1.14 == 2 | clean.dataset$Fr1.14 == 3)
#df3 <- subset(clean.dataset,clean.dataset$tra_id == 77521)
df2 <- df2[,c("VK_avslutad","tra_DodsfallsanalysGenomford","Problemomrade_.FMP","Fr1.14","tra_id","ofi","Deceased")]
### Yes and No from mortality conferance
n.df.ofi <- sum(df$ofi == "Yes", na.rm = TRUE)
n.df.no.ofi <- sum(df$ofi == "No", na.rm = TRUE)

### Patients who have gone trough dödsfall but have no consensus
df.exc <- df[is.na(df$ofi) == TRUE, ]

######################################
# qd = quality database = överlevare #
######################################
qd <- subset(clean.dataset, clean.dataset$VK_avslutad == "Yes")

qd<- subset(qd, is.na(qd$tra_DodsfallsanalysGenomford) == TRUE  | qd$tra_DodsfallsanalysGenomford == 2 )

n.qd <- nrow(qd)
n.tot <- n.qd + n.df

clean.qd <- qd
audit.qd <- clean.qd[clean.qd$VK_hlr_thorak == "Yes" |
                       clean.qd$VK_sap_less90 == "Yes" | 
                       clean.qd$VK_leverskada == "Yes" |
                       clean.qd$VK_gcs_less9_ej_intubTE == "Yes" |
                       clean.qd$VK_mjaltskada == "Yes" |
                       clean.qd$VK_mer_30min_DT == "Yes" |
                       clean.qd$VK_mass_transf == "Yes" |
                       clean.qd$VK_mer_60min_interv == "Yes" |
                       clean.qd$VK_iss_15_ej_iva == "Yes" |
                       clean.qd$VK_ej_trombrof_TBI_72h == "Yes", ]

n.audit.qd <- nrow(audit.qd)
#### Patients with all filters No and VK_avslutad = YES - > Still have 2 OFI!!!?? 
allneg.ofi <- clean.qd[clean.qd$VK_hlr_thorak == "No" &
                         clean.qd$VK_sap_less90 == "No" & 
                         clean.qd$VK_leverskada == "No" &
                         clean.qd$VK_gcs_less9_ej_intubTE == "No" &
                         clean.qd$VK_mjaltskada == "No" &
                         clean.qd$VK_mer_30min_DT == "No" &
                         clean.qd$VK_mass_transf == "No" &
                         clean.qd$VK_mer_60min_interv == "No" &
                         clean.qd$VK_iss_15_ej_iva == "No" &
                         clean.qd$VK_ej_trombrof_TBI_72h == "No" &
                         clean.qd$VK_annat == "No" &
                         clean.qd$ofi == "Yes" , ]

n.allneg.ofi <- nrow(allneg.ofi)
### Exkluden in first step

allneg.no.ofi <- clean.qd[clean.qd$VK_hlr_thorak == "No" &
                            clean.qd$VK_sap_less90 == "No" & 
                            clean.qd$VK_leverskada == "No" &
                            clean.qd$VK_gcs_less9_ej_intubTE == "No" &
                            clean.qd$VK_mjaltskada == "No" &
                            clean.qd$VK_mer_30min_DT == "No" &
                            clean.qd$VK_mass_transf == "No" &
                            clean.qd$VK_mer_60min_interv == "No" &
                            clean.qd$VK_iss_15_ej_iva == "No" &
                            clean.qd$VK_ej_trombrof_TBI_72h == "No" &
                            clean.qd$VK_annat == "No" &
                            clean.qd$ofi == "No" &
                            clean.qd$VK_avslutad == "Yes" , ]


n.allneg.no.ofi <- nrow(allneg.no.ofi)

### Selected VK_Annat

n.annat <- nrow(clean.qd[clean.qd$VK_hlr_thorak == "No" &
                           clean.qd$VK_sap_less90 == "No" & 
                           clean.qd$VK_leverskada == "No" &
                           clean.qd$VK_gcs_less9_ej_intubTE == "No" &
                           clean.qd$VK_mjaltskada == "No" &
                           clean.qd$VK_mer_30min_DT == "No" &
                           clean.qd$VK_mass_transf == "No" &
                           clean.qd$VK_mer_60min_interv == "No" &
                           clean.qd$VK_iss_15_ej_iva == "No" &
                           clean.qd$VK_ej_trombrof_TBI_72h == "No" &
                           clean.qd$VK_annat == "Yes" &
                           clean.qd$VK_avslutad == "Yes" , ])



### To get the two nurse cohort - First take the cohort with some VK_ and VK_avslutad. Then check which have no OFI.
mom <- clean.qd[clean.qd$VK_hlr_thorak == "Yes" |
                  clean.qd$VK_sap_less90 == "Yes" | 
                  clean.qd$VK_leverskada == "Yes" |
                  clean.qd$VK_gcs_less9_ej_intubTE == "Yes" |
                  clean.qd$VK_mjaltskada == "Yes" |
                  clean.qd$VK_mer_30min_DT == "Yes" |
                  clean.qd$VK_mass_transf == "Yes" |
                  clean.qd$VK_mer_60min_interv == "Yes" |
                  clean.qd$VK_iss_15_ej_iva == "Yes" |
                  clean.qd$VK_ej_trombrof_TBI_72h == "Yes"|
                  clean.qd$VK_annat == "Yes", ]

nurse <- mom[mom$ofi == "No" & is.na(mom$Problemomrade_.FMP) == TRUE, ]
nurse.mom <- nrow(mom[is.na(mom$ofi) == FALSE & is.na(mom$Problemomrade_.FMP) == FALSE, ])

n.2nurse.no.ofi <- nrow(nurse) 

## To get the Mortality group just check the cases that hade any VK and then if thay have something in problemområde

MoM <- mom[is.na(mom$Problemomrade_.FMP) == FALSE , ] 

n.MoM <- nrow(MoM)+n.allneg.ofi

n.MoM.no.ofi <- sum(MoM$ofi == "No")
n.MoM.ofi <- sum(mom$ofi == "Yes")+n.allneg.ofi
p.MoM.ofi <- round((n.MoM.ofi/n.qd)*100,digits=2)

####################
# Create flowchart #
####################

DiagrammeR::grViz("
digraph graph2 {
fontname=Helvetica
graph [layout = dot, splines=ortho, nodesep=0.3, fontname = \"helvetica\"]
node [shape=rounded, fontsize = 15, width = 3.3, fontname = \"helvetica\"]
edge [arrowsize = 0.7, arrowhead=vee, labelfontsize=13, fontname = \"helvetica\"]
swetrau [label = 'Trauma Registry (n=@@1)']
excluded [label = <
Excluded (n=@@17)<br ALIGN='LEFT'/>
&#8226; Not screened for OFI (n=@@18)<br ALIGN='LEFT'/>
&#8226; Under the age of 15 (n=@@19)<br ALIGN='LEFT'/>
>]
death [label = 'Death* (n=@@22)']
alive [label = 'Survival* (n=@@21)']
eligible [label = 'Included in analysis (n=@@2)']
audit [label = 'Flagged by Audit filters (n=@@3)']
annat [label = 'Flagged by nurse (n=@@4)']
nurse [label = 'Review by two nurses(n=@@5)']
MoM [label = 'Morbidity conference (n=@@6)']
df [label = 'Mortality conference (n=@@8)']
ofi [label = 'OFI (n=@@9)']
noofi [label = 'no OFI (n=@@10)']
'1' [shape = point, width = 0, height = 0]
swetrau -> '1' [arrowhead = none]
'1' -> excluded [minlen=1.35]
'1' -> eligible [arrowhead = none]
eligible -> alive -> audit -> nurse
nurse -> MoM [headlabel = 'n=@@23', labeldistance=3 , labelangle=-70]
eligible -> death -> df 
alive -> annat -> nurse
alive -> noofi [headlabel = 'n=@@14', labelangle=-20, labeldistance=2.75]
alive -> MoM [headlabel = 'n=@@16', labelangle=-65, labeldistance=2] # Detta borde gå till MoM
df -> noofi [headlabel = 'n=@@12', labelangle=-70, labeldistance=2.5]
df -> ofi [headlabel = 'n=@@11', labelangle=-70, labeldistance=2.25]
nurse -> noofi [headlabel = 'n=@@13', labelangle=-70, labeldistance=3]
MoM -> noofi [headlabel = 'n=@@7', labelangle=-70, labeldistance=2.75]
MoM -> ofi [headlabel  = 'n=@@15', labelangle=-70, labeldistance=2.5]
  subgraph {
    rank = same; excluded; '1';
  }
}
[1]: nrow(combined.datasets)  
[2]: nrow(clean.dataset)
[3]: n.audit.qd
[4]: n.annat
[5]: n.annat + n.audit.qd
[6]: n.MoM
[7]: n.MoM.no.ofi
[8]: n.df
[9]: n.df.ofi + n.MoM.ofi
[10]: n.df.no.ofi + n.MoM.no.ofi + n.2nurse.no.ofi + n.allneg.no.ofi
[11]: n.df.ofi
[12]: n.df.no.ofi
[13]: n.2nurse.no.ofi
[14]: n.allneg.no.ofi
[15]: n.MoM.ofi
[16]: n.allneg.ofi
[17]: n.not.ofi.screened + n.under.15
[18]: n.not.ofi.screened
[19]: n.under.15
[20]: nrow(clean.dataset) - (n.not.ofi.screened + n.under.15)
[21]: n.qd
[21]: n.df
[23]: nurse.mom
") %>% DiagrammeRsvg::export_svg() %>% charToRaw() %>% rsvg::rsvg_pdf("manuscripts/ofi-flowchart.pdf")

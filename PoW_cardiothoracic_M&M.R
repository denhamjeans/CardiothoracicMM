##Purpose: PowerPoint M&M Automation Script for PoW Cardiothoracics Dept
##Author: Rory Denham
##Date Created: 24/11/2018

#Load Libraries ------------------------
library(officer)
library(magrittr)
library(tidyverse)
library(flextable)
library(janitor)
library(lubridate)
library(scales)
library(stringr)
library(tibble)

#Load Data -----------------------------

#####
#change this file accordingly >>>>D:/M&M_Query_Data/ "insert pathfile here"
PathFile <- "M&M_AUG_SEPT_DATA"
#####

#change to use readr package - require downloading the package first
MM.CaseStudy <- read_csv(file = file.path("D:", PathFile, "qry_M&M_CaseStudy.csv"))
MM.Inf <-       read_csv(file = file.path("D:", PathFile, "qry_MMPres_Inf.csv"))
MM.Pres_Main <- read_csv(file = file.path("D:", PathFile, "qry_MMPres_Main.csv"))
MM.ReAdm <-     read_csv(file = file.path("D:", PathFile, "qry_MMPres_ReAdm.csv"))
MM.RTT <-       read_csv(file = file.path("D:", PathFile, "qry_MMPres_RTT.csv"))

#Data cleaning and wrangling ------------

#####CaseStudy Dataset#####

str(MM.CaseStudy)
#Fix Variable formats
MM.CaseStudy$OpDate <- as.Date(MM.CaseStudy$OpDate, "%d/%m/%Y")

#####Pres_Main Dataset#####

str(MM.Pres_Main)
MM.Pres_Main$AdmDate <- as.Date(MM.Pres_Main$AdmDate, "%d/%m/%Y")
MM.Pres_Main$ConsultDate <- as.Date(MM.Pres_Main$ConsultDate, "%d/%m/%Y")
MM.Pres_Main$OpDate <- as.Date(MM.Pres_Main$OpDate, "%d/%m/%Y")
MM.Pres_Main$DischargeDate <- as.Date(MM.Pres_Main$DischargeDate, "%d/%m/%Y")
MM.Pres_Main$PO_ICUAdmDt <- as.Date(MM.Pres_Main$PO_ICUAdmDt, "%d/%m/%Y")
MM.Pres_Main$PO_ExtbDt <- as.Date(MM.Pres_Main$PO_ExtbDt, "%d/%m/%Y")

MM.Pres_Main$PreOp_Comments <- as.character(MM.Pres_Main$PreOp_Comments)
MM.Pres_Main$PO_Comments <- as.character(MM.Pres_Main$PO_Comments)


#create date number variable
MM.Pres_Main$Num_Months <- month(MM.Pres_Main$OpDate) - month(min(MM.Pres_Main$OpDate)) + 1
#add month variable
MM.Pres_Main %<>% mutate(Month = format(OpDate, "%B"))



# Determine how many dates selected -----
First.Month <- min(MM.Pres_Main$Num_Months)
Second.Month <- max(MM.Pres_Main$Num_Months)
No.Months <- Second.Month - First.Month + 1

First.Month <- format(min(MM.Pres_Main$OpDate), "%B")
Second.Month <- format(max(MM.Pres_Main$OpDate), "%B")

#number of cases ------------------------
Total.Cases <- MM.Pres_Main %>%
  group_by(Month) %>%
  summarise(n = n())


#Presentation setup ---------------------------------------------------


# Tables and Graphs for slides ---------------------

# Slide 1 ------------------------------------------
#caseload by category

if(No.Months == 1) {                              #if one month is present within M&M dataset create counts for each surgery type
  slide1.data <- MM.Pres_Main %>% 
    group_by(OpCategory, Month) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count)) %>%
    spread(Month, Count) %>%
    adorn_totals("row") %>%                         #add totals to the bottom of the count summary to give total cases per month
    mutate("Percentage" = percent(.[[2]] / Total.Cases[[1, 'n']])) #add percentages for each surgery category as a proportion of the total cases (1 month)
  
  for (i in seq_along(unique(slide1.data$OpCategory))) {        #loop is used to create a list of operations within each category which are present this month
    x <- MM.Pres_Main %>%                                 #if major OPcategories were not used to create the table in this fashion the table would be too large
      select(OpCategory, OpDescription) %>%
      filter(OpCategory == Unique.Category[i])
    slide1.data[i, "Description"] <- paste(unique(x$OpDescription), collapse = ", ")
  }
  
} else if(No.Months == 2) {                       #if one month is present within M&M dataset create counts for each surgery type
  slide1.data <- MM.Pres_Main %>% 
    group_by(OpCategory, Month) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count)) %>%
    spread(Month, Count) %>%
    adorn_totals("row") %>%                       #add totals to the bottom of the count summary to give total cases per month
    mutate("Percentage" = percent(.[[2]] / Total.Cases[[1, 'n']]),  #add percentages for each surgery category as a proportion of the total cases (2 months)
           "Percentage2" = percent(.[[3]] / Total.Cases[[2, 'n']]))
  

  Unique.Category <- unique(slide1.data$OpCategory)
  for (e in 1:2) {                                  #loop is used to create a list of operations within each category which are present this month
    for (i in seq_along(Unique.Category)) {       #if major OPcategories were not used to create the table in this fashion the table would be too large
      x <- MM.Pres_Main %>%
        select(OpCategory, OpDescription, Num_Months) %>%
        filter(OpCategory == Unique.Category[i] & Num_Months == e)
      slide1.data[i, paste("Description", e)] <- paste(unique(x$OpDescription), collapse = ", ")
    }
  }
  
  slide1.data <- slide1.data[c(1, 2, 4, 6, 3, 5, 7)]
}



# Slide 2 ------------------------------------------
#Caseload historical comparison
slide2.data <- MM.Pres_Main %>%
  group_by(Month) %>%
  summarise("Elective Count" = sum(Op_StatusPOW == 1, na.rm = TRUE),
            "Urgent Count" = sum(Op_StatusPOW == 2, na.rm = TRUE),
            "Emergency Count" = sum(Op_StatusPOW == 3, na.rm = TRUE),
            "Redo" = sum(Op_Incidence > 1, na.rm = TRUE), 
            "Mean Age" = mean(Op_Age, na.rm = TRUE), 
            "Median Age" = median(Op_Age, na.rm = TRUE),
            "Mean Logistic EUROSCORE" = mean(EUROScoreLogistic, na.rm = TRUE),
            "Median Logistic EUROSCORE" = median(EUROScoreLogistic, na.rm = TRUE)
            )
  
  #t() %>% #transpose the dataframe. Needs fixing.
  #mutate("Percentage" = percent(.[[2]] / Total.Cases[['n']])) %>%   #add percentages needs fixing.
  #mutate("Percentage2" = percent(.[[3]] / Total.Cases[['n']])) 



# Slide 3 ------------------------------------------
#Caseload by consultant

slide3.data <- MM.Pres_Main %>%                #count operation categories by consultant
  select(OpConsultant, OpCategory) %>%
  group_by(OpConsultant, OpCategory) %>%
  summarise(count = n()) %>%
  spread(OpCategory, count) 

fncols <- function(data, cname) {       #function adds op category full of NA if it is missing to create a fixed number of columns 
  add <-cname[!cname%in%names(data)]
  
  if(length(add)!=0) data[add] <- NA
  data
}

slide3.data <- fncols(slide3.data, c("OpConsultant", "CABG","CABG/Other","CABG/Valve", "CABG/Valve/Other", "Other", "Valve", "Valve/Other")) #run function to add missing cols

slide3.data <- slide3.data[, c("OpConsultant", "CABG","CABG/Other", "Valve", "Valve/Other", "CABG/Valve", "CABG/Valve/Other", "Other")]

slide3.data <- cbind(slide3.data, TOTAL = rowSums(slide3.data[2:length(slide3.data)], na.rm = TRUE))        #add totals



# slide 4 ------------------------------------------
#Mortalities <30days

slide4.data1 <- MM.Pres_Main %>% 
  group_by(Month) %>%
  summarise(Mortalities = sum(FU_Death30d, na.rm = TRUE)) %>%
  mutate("Percentage" = percent(.[[2]] / Total.Cases[['n']]),                #add count percentages
         "Historical" = rep("1.5%", length(slide4.data1$Mortalities)))       #add historical percentages

slide4.data2 <- MM.Pres_Main %>%
  group_by(Month) %>%
  filter(FU_Death30d == 1) %>%
  select(Pt_LName, OpConsultantInitials, Month, Op_Age, Op_StatusPOW, EUROScoreLogistic, Death_DaysPO, FU_DeathCause1, FU_DeathNotes)

if (dim(slide4.data2)[1] == 0) {
  slide4.data2[1,] <- NA
}



# slide 5 ------------------------------------------
#Outpatient Wait day plot + mean/medians talbe

#Summary Stats Table
slide5.data <- MM.Pres_Main %>%
  filter(Adm_Elective == 1) %>%
  group_by(Month) %>%
  mutate(waitdays = OpDate - ConsultDate) %>%
  select(Month, waitdays) %>%
  summarise(Mean = round(mean(waitdays, na.rm = TRUE), 1), Median = median(waitdays, na.rm = TRUE)) %>%
  rbind(list("Historical", 67.7, 68))

###Variable setup for graphing

#highlight max values for each of the waiting periods
slide5.plotdata <- MM.Pres_Main %>%
  filter(Adm_Elective == 1) %>%
  select(OpConsultant, Pt_LName, OpDate, ConsultDate) %>%
  group_by(OpConsultant) %>%
  mutate(Waitdays = as.numeric(OpDate - ConsultDate))

slide5.plotdata.maxwait <- slide5.plotdata %>%
  group_by(OpConsultant) %>%
  summarise(Max_wait = max(Waitdays))

#Create plot from the data
slide5.plot <- ggplot(slide5.plotdata, aes(Pt_LName, Waitdays)) + 
  geom_col() +
  facet_wrap(~OpConsultant, scales = "free_y") +
  geom_hline(aes(yintercept=90, linetype = "90 Days"), color="#009E73", size = 1) +     #10 day inpatient wait time
  geom_hline(aes(yintercept=slide5.data[[dim(slide5.data)[1], 2]], linetype = "Historical Average"), colour="#D55E00", size = 1) +   #historical mean inpatient wait time
  geom_hline(aes(yintercept=mean(slide5.plotdata$Waitdays, na.rm = TRUE), linetype = "Group Average"), colour="#000000", size = 1)

#Format plot labels and add layout changes
slide5.plot <- slide5.plot + labs(x = "", y = "Wait Period (Days)", 
                                  caption = "*Values calculated from Consult Date \n *Consult Date defined as 'First consult where patient is deemed suitable for surgery'") +
  theme_minimal() +
  scale_linetype_manual(name = "", values = c(2, 1, 2), 
                        guide = guide_legend(override.aes = list(color = c("#009E73", "#000000", "#D55E00")))) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom") +
  coord_flip() #flip the axes for readability



# slide 6 ------------------------------------------
#Outpatient Wait >90 day Patients

#summary Stats Table
slide6.data1 <- MM.Pres_Main %>%
  filter(Adm_Elective == 1) %>%
  mutate(Waitdays = OpDate - ConsultDate) %>% 
  group_by(Month) %>%
  summarise("Total Outpatient Cases" = n(),
            ">90 Day Wait" = sum(Waitdays>90, na.rm = TRUE)) %>%
  arrange(Month) %>%
  mutate("Percentage" = percent(`>90 Day Wait` / `Total Outpatient Cases`),
         "Historical" = rep("9.7%", length(slide4.data1$Mortalities)))

#Patients With >90 Days Wait
slide6.data2 <- MM.Pres_Main %>%
  filter(Adm_Elective == 1) %>%
  mutate(Waitdays = OpDate - ConsultDate) %>%
  filter(Waitdays > 90) %>%
  arrange(Month, desc(Waitdays)) %>%
  select(Pt_LName, OpConsultant, OpDescription, Month, Waitdays, "Pre-OP Comments" = PreOp_Comments)



# slide 7 ------------------------------------------
#Inpatient Wait day plot + mean/medians table

#Summary Stats Table
slide7.data <- MM.Pres_Main %>%
  filter(Adm_Elective == 0) %>%
  group_by(Month) %>%
  mutate(waitdays = OpDate - ConsultDate) %>%
  select(Month, waitdays) %>%
  summarise(Mean = round(mean(waitdays, na.rm = TRUE), 1), Median = median(waitdays, na.rm = TRUE)) %>%
  rbind(list("Historical", 5.8, 4))

###Variable setup for graphing

#highlight max values for each of the waiting periods
slide7.plotdata <- MM.Pres_Main %>%
  filter(Adm_Elective == 0) %>%
  select(OpConsultant, Pt_LName, OpDate, ConsultDate) %>%
  group_by(OpConsultant) %>%
  mutate(Waitdays = as.numeric(OpDate - ConsultDate))

#highlight cases of interest on the graph 
slide7.plotdata.maxwait <- slide7.plotdata %>%
  group_by(OpConsultant) %>%
  summarise(Max_wait = max(Waitdays))

#Create plot from the data
slide7.plot <- ggplot(slide7.plotdata, aes(Pt_LName, Waitdays)) + 
  geom_col() +
  facet_wrap(~OpConsultant, scales = "free_y") +
  geom_hline(aes(yintercept=10, linetype = "10 Days"), colour="#009E73", size = 1) +     #10 day inpatient wait time
  geom_hline(aes(yintercept=slide7.data[[dim(slide7.data)[1], 2]], linetype = "Historical Average"), colour="#D55E00", size = 1) +  #historical mean inpatient wait time
  geom_hline(aes(yintercept=mean(slide7.plotdata$Waitdays, na.rm = TRUE), linetype = "Group Average"), colour="#000000", size = 1) #group average
  
#Format plot labels and add layout changes
slide7.plot <- slide7.plot + labs(x = "", y = "Wait Period (Days)", 
                                  caption = "*Values calculated from Consult Date \n *Consult Date defined as 'First consult where patient is deemed suitable for surgery'") +
  theme_minimal() +
  scale_linetype_manual(name = "", values = c(2, 1, 2), 
                        guide = guide_legend(override.aes = list(color = c("#009E73", "#000000", "#D55E00")))) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom") +
  coord_flip() #flip the axes for readability



# slide 8 ------------------------------------------
#Outpatient Wait >10 day Patients

#summary Stats Table
slide8.data1 <- MM.Pres_Main %>%
  filter(Adm_Elective == 0) %>%
  mutate(Waitdays = OpDate - ConsultDate) %>% 
  group_by(Month) %>%
  summarise("Total Inpatient Cases" = n(),
            ">10 Day Wait" = sum(Waitdays>10, na.rm = TRUE)) %>%
  arrange(Month) %>%
  mutate("Percentage" = percent(`>10 Day Wait` / `Total Inpatient Cases`),
         "Historical" = rep("6.8%", length(slide4.data1$Mortalities)))

#Patients With >10 Days Wait
slide8.data2 <- MM.Pres_Main %>%
  filter(Adm_Elective == 0) %>%
  mutate(Waitdays = OpDate - ConsultDate) %>%
  filter(Waitdays > 10) %>%
  arrange(Month, desc(Waitdays)) %>%
  select(Pt_LName, OpConsultant, OpDescription, Month, Waitdays, "Pre-OP Comments" = PreOp_Comments)



# slide 9 ------------------------------------------
# ICU Stay (days)

#Summary Stats Table
slide9.data <- MM.Pres_Main %>%
  group_by(Month) %>%
  select(Month, ICUDays, ICUReqDays) %>%
  summarise(Mean = round(mean(ICUDays, na.rm = TRUE), 1), Median = round(median(ICUDays, na.rm = TRUE), 1)) %>%
  rbind(list("Historical", 2.7, 1.9))


###Variable setup for graphing
#fundamental plotting data
slide9.plotdata <- MM.Pres_Main %>%
  select(Pt_LName, OpConsultant, Month, ICUDays, ICUReqDays) %>%
  arrange(Month, OpConsultant)

#ICU bedblock data
ICUbedblock <- MM.Pres_Main %>%
  mutate(BedBlock = (ICUDays - ICUReqDays)) %>%
  select(Pt_LName, Pt_FName, ICUReqDays, ICUDays, BedBlock) %>%
  filter(ICUReqDays > 0) %>%
  summarise(BedBlockaverageperpatient = mean(BedBlock, na.rm = TRUE),
            maxbedblock = max(BedBlock, na.rm = TRUE)
            )

#Create plot from the data
slide9.plot <- ggplot(slide9.plotdata, aes(Pt_LName, ICUDays)) + 
  geom_col() +
  facet_wrap(~Month, scales = "free_y") +
  geom_hline(aes(yintercept= 3, linetype = "3 Days"), colour="#009E73", size = 1) +     #3 day ICU Stay
  geom_hline(aes(yintercept= slide9.data[[dim(slide9.data)[1], 2]], linetype = "Historical Average"), colour="#D55E00", size = 1) +  #historical mean inpatient wait time
  geom_hline(aes(yintercept= mean(slide9.plotdata[["ICUDays"]], na.rm = TRUE), linetype = "Group Average"), colour="#000000", size = 1) + #group average
  geom_hline(aes(yintercept= ICUbedblock[["BedBlockaverageperpatient"]], linetype = "Average Bed Block Per Patient"), colour="#0072B2", size = 1) #average bedblock
  
#Format plot labels and add layout changes
slide9.plot <- slide9.plot + labs(x = "", y = "ICU Length of Stay (Days)") +
  theme_minimal() +
  scale_linetype_manual(name = "", values = c(2, 1, 2, 2), 
                        guide = guide_legend(override.aes = list(color = c("#009E73", "#000000", "#D55E00", "#0072B2")))) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom") +
  coord_flip() #flip the axes for readability



# slide 10 ------------------------------------------
# Prolonged ICU Stay (3 days)

#ICU bedblock data        (repeated for clarity)
slide10.data1 <- MM.Pres_Main %>% 
  group_by(Month) %>%
  mutate(BedBlock = (ICUDays - ICUReqDays)) %>%
  select(Pt_LName, Pt_FName, Month, ICUReqDays, ICUDays, BedBlock) %>%
  filter(ICUReqDays > 0) %>%
  summarise("LOS > 3 Days" = sum(ICUDays>3),
            "Percentage" = percent(sum(ICUDays>3)/n()),
            "Historical" = rep("6.8%"),
            "Mean Bed Block Per Patient (Hours)" = mean(BedBlock, na.rm = TRUE)*24,
            "Max Bed Block (Days)" = max(BedBlock, na.rm = TRUE)
            )

#Patients With >90 Days Wait
slide10.data2 <- MM.Pres_Main %>%
  filter(ICUDays > 3) %>%
  arrange(Month, desc(ICUDays)) %>%
  select(Pt_LName, OpConsultant, OpCategory, Month, ICUReqDays, ICUDays, "Pre-OP Comments" = PreOp_Comments)



# slide 11 ------------------------------------------
# Post-OP LOS (days)

#Summary Stats Table
slide11.data <- MM.Pres_Main %>%
  mutate("Post-OP LOS" = DischargeDate - OpDate) %>%
  select(Month, `Post-OP LOS`) %>%
  group_by(Month) %>%
  summarise(Mean = round(mean(`Post-OP LOS`, na.rm = TRUE), 1), Median = round(median(`Post-OP LOS`, na.rm = TRUE), 1)) %>%
  rbind(list("Historical", 10.1, 7))


###Variable setup for graphing
#fundamental plotting data
slide11.plotdata <- MM.Pres_Main %>%
  mutate("Post-OP LOS" = as.numeric(DischargeDate - OpDate)) %>%
  select(Pt_LName, OpConsultant, Month, "Post-OP LOS") %>%
  arrange(Month, OpConsultant)

#Create plot from the data
slide11.plot <- ggplot(slide11.plotdata, aes(Pt_LName, `Post-OP LOS`)) + 
  geom_col() +
  facet_wrap(~Month, scales = "free_y") +
  geom_hline(aes(yintercept= 10, linetype = "10 Days"), colour="#009E73", size = 1) +     #10 day post op Stay
  geom_hline(aes(yintercept= slide11.data[[dim(slide11.data)[1], 2]], linetype = "Historical Average"), colour="#D55E00", size = 1) +  #historical mean inpatient wait time
  geom_hline(aes(yintercept= mean(slide11.plotdata[["Post-OP LOS"]], na.rm = TRUE), linetype = "Group Average"), colour="#000000", size = 1) #group average

#Format plot labels and add layout changes
slide11.plot <- slide11.plot + 
  labs(x = "",
       y = "Post-OP Length of Stay (Days)") +
  theme_minimal() +
  scale_linetype_manual(name = "", values = c(2, 1, 2), 
                        guide = guide_legend(override.aes = list(color = c("#009E73", "#000000", "#D55E00")))) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom") +
  coord_flip() #flip the axes for readability



# slide 12 ------------------------------------------
# Prolonged Post-OP LOS (>10 days)

slide12.data1 <- MM.Pres_Main %>% 
  mutate("Post-OP LOS" = DischargeDate - OpDate) %>%
  group_by(Month) %>%
  summarise("Count of Prolonged Stays (>10 Days)" = sum(`Post-OP LOS`>10),
            "Percentage" = percent(sum(`Post-OP LOS`>10)/n()),
            "Historical" = rep("26.1%"))

#Patients With >10 Days Wait
slide12.data2 <- MM.Pres_Main %>%
  mutate("Post-OP LOS" = DischargeDate - OpDate) %>%  
  filter(`Post-OP LOS` > 10) %>%
  arrange(Month, desc(`Post-OP LOS`)) %>%
  select(Pt_LName, OpConsultant, OpCategory, Month, `Post-OP LOS`, "Pre-OP Comments" = PO_Comments)



# slide 13 ------------------------------------------
# Post-op AMI or Cardiac Arrest

slide13.data1 <- MM.Pres_Main %>% 
  select(Month, PO_AMI, PO_CardArrest) %>%
  group_by(Month) %>%
  summarise("AMI" = paste(sum(PO_AMI), percent(sum(PO_AMI, na.rm = TRUE)/n()), collapse = " "),
            "Post-OP Cardiac Arrest" = paste(sum(PO_CardArrest), percent(sum(PO_CardArrest, na.rm = TRUE)/n()), collapse = " ")) %>%
  rbind(list("Historical (Per Month)", "0.4%", "1.6%"))

slide13.data2 <- MM.Pres_Main %>%
  mutate("Post-OP LOS" = DischargeDate - OpDate) %>%
  filter(`PO_AMI` == 1 | `PO_CardArrest` == 1) %>%
  arrange(Month, desc(PO_AMI), desc(PO_CardArrest)) %>%
  select(Pt_LName, OpConsultant, OpCategory, Month, `Post-OP Cardiac Arrest` = PO_CardArrest,`Post-OP LOS`, "Pre-OP Comments" = PO_Comments)

if(nrow(slide13.data2)==0){
  slide13.data2[1,] <- NA
}



# slide 14 ------------------------------------------
# IABP Use


slide14.data1 <- MM.Pres_Main %>% 
  select(Month, IABP, IABP_When) %>%
  group_by(Month, IABP_When) %>%
  summarise("Count" = n()) %>%
  filter(IABP_When != 0) %>%
  spread(Month, Count) %>%
  adorn_totals("row")

if(No.Months == 2 & is.element('Pre-Op', slide14.data1[['IABP_When']]) == FALSE) { #this sequence adds any missing rows for visual purposes (2 month data)
  slide14.data1 <- rbind(slide14.data1, c('Pre-Op', 0, 0))
} else if(is.element('Intra-Op', slide14.data1[['IABP_When']]) == FALSE) {
  slide14.data1 <- rbind(slide14.data1, c('Intra-Op', 0, 0))
} else if(is.element('Post-Op', slide14.data1[['IABP_When']]) == FALSE) {
  slide14.data1 <- rbind(slide14.data1, c('Post-Op', 0, 0))
}

if(No.Months == 1 & is.element('Pre-Op', slide14.data1[['IABP_When']]) == FALSE) { #this sequence adds any missing rows for visual purposes (1 month data)
  slide14.data1 <- rbind(slide14.data1, c('Pre-Op', 0))
} else if(is.element('Intra-Op', slide14.data1[['IABP_When']]) == FALSE) {
  slide14.data1 <- rbind(slide14.data1, c('Intra-Op', 0))
} else if(is.element('Post-Op', slide14.data1[['IABP_When']]) == FALSE) {
  slide14.data1 <- rbind(slide14.data1, c('Post-Op', 0))
}

IABP_when_order <- c("Pre-Op", "Intra-Op", "Post-Op", "Total") #create desired order for rows
slide14.data1 %<>%
  slice(match(IABP_when_order, IABP_When))                 #add match dataframe to the ordered vector

slide14.data1$`Historical (per month)` <- (c("91.3%", "2.6%", "6.1%", "11.2%")) #add historical data for comparison

if(No.Months == 2) {
  slide14.data1 %<>% mutate("Percentage" = percent(as.numeric(.[[2]]) / as.numeric(.[[4, 2]])),  #add percentages for each surgery category as a proportion of the total cases (2 months)
                            "Percentage2" = percent(as.numeric(.[[3]]) / as.numeric(.[[4, 3]])))
} 

if(No.Months == 1) {
  slide14.data1 %<>% mutate("Percentage" = percent(.[[2]] / Total.Cases[[1, length(.[2])]]))  #add percentages for each surgery category as a proportion of the total cases (1 months)
}  

slide14.data1 <- slide14.data1[, c(1, 2, 5, 3, 6, 4)] #reorder the columns


#patients with IABP insertions
slide14.data2 <- MM.Pres_Main %>%                #count operation categories by consultant
  select(Pt_LName, OpConsultant, OpDescription,  Month, IABP_Indication, IABP_When) %>%
  group_by(Month) %>%
  filter(IABP_Indication != 0)


# slide 15 ------------------------------------------
# Blood Transfusions 

slide15.data2 <- MM.Pres_Main %>%
  group_by(OperationID) %>% 
  mutate(Bld_Tot_All = sum(Bld_Tot_RBC, Bld_Tot_FFP, Bld_Tot_Plt, Bld_Tot_Cryo, Bld_Tot_FVIIa_mg, Bld_Tot_PTX)) %>% 
  select(OperationID, Month, Pt_LName, OpCategory, OpConsultantInitials, Bld_Tot_RBC, Bld_Tot_FFP, Bld_Tot_Plt, Bld_Tot_Cryo, Bld_Tot_FVIIa_mg, Bld_Tot_PTX, Bld_Tot_All) %>% 
  mutate(`Recieved_Bld_Prod` = if_else(Bld_Tot_All > 0, 1, 0),
         `RBC > 4 U` = if_else(Bld_Tot_RBC > 4, 1, 0),
         `RBC > 15 U` = if_else(Bld_Tot_RBC > 15, 1, 0),
         `Any Blood Prod > 15 U` = if_else(Bld_Tot_All > 15, 1, 0))

slide15.data1 <- slide15.data2 %>%  
  group_by(Month) %>% 
  summarise(`Recieved Blood Product` = paste(sum(`Recieved_Bld_Prod`), ' (',percent(sum(`Recieved_Bld_Prod`)/n()),')', sep = ""),
            `RBC > 4 Units` = paste(sum(`RBC > 4 U`), ' (', percent(sum(`RBC > 4 U`)/n()),')', sep = ""),
            `RBC > 15 Units` = paste(sum(`RBC > 15 U`), ' (', percent(sum(`RBC > 15 U`)/n()), ')', sep = ""),
            `Any Blood Prod > 15 Units` = paste(sum(`Any Blood Prod > 15 U`), ' (', percent(sum(`Any Blood Prod > 15 U`)/n()), ')', sep = "")) %>%
  rbind(list("Historical (Per Month)", "61.8%", "13.1%", "0.9%", "6.5%"))

slide15.data2 %<>% 
  filter(Bld_Tot_All >= 10) %>% 
  arrange(Month, desc(Bld_Tot_All)) %>% 
  select(-one_of("OperationID", "Recieved_Bld_Prod", "RBC > 4 U", "RBC > 15 U", "Any Blood Prod > 15 U"))

slide15.data2 <- slide15.data2[, 2:length(slide15.data2)]
  
# slide 16 ------------------------------------------
# Reintubation & Return to ICU




##########################################################################################

# Text for Slides -----------------------

# Intro summary slide
intro <- "Earlier this year we talked about how limited housing supply was helping to drive accelerating house prices across the country. In such an environment you would expect to see housing vacancies decline. Indeed, if you look at the rate of rental or homeowner vacancies you see a substantial reduction. But if we look a little closer at the housing inventory data something curious emerges."

# Set captions --------------------------

#Date caption for title
if(No.Months == 1){
  captitle <- paste(First.Month, " 2018")
} else if(No.Months == 2){
  captitle <- paste(First.Month, " and ", Second.Month, " 2018")
}

#footer for the presentation
myftr<-"RoryDenham R to PowerPoint"


# Build the Deck ------------------------


read_pptx("Powerpoint_Templates/ppt.pptx") %>%
  layout_summary()

read_pptx("Powerpoint_Templates/ppt.pptx") %>%
  layout_properties(layout = "Content Only", master = "Office Theme")

annotate_base(path = "Powerpoint_Templates/ppt.pptx", output_file = "annotated_layout_base_edit.pptx")


doc <- read_pptx("Powerpoint_Templates/ppt.pptx") %>%
  # Title Slide
  # image size needs fixing
  add_slide(layout="Title Slide", master="Office Theme") %>%
  ph_with_text(type = "ctrTitle", str = "POWH Cardiac Surgery Data Review\n&\nMorbidity and Mortality") %>% 
  ph_with_text(type = "subTitle", str = captitle) %>% 
  ph_with_img(type = "pic", index = 1, src = "images/POWH_final.png", height = 1.5, width = 1.5) %>%
  ph_with_img(type = "pic", index = 2, src = "images/SESLHDlogo.png", height = 1.33, width = 5.09) %>%
  ph_with_text(type = "ftr", str = myftr) %>%
  
  # Slide 1 - Caseload:
  add_slide(layout = "Content Small Title", master = "Office Theme") %>% 
  ph_with_text(type = "title", index=1,str = "Caseload") %>%
  ph_with_table(type = "body", value = slide1.data) %>%
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "1" ) %>%
  ph_with_text(type = "dt", str = format(Sys.Date(),"%B %d,%Y")) %>%
    
  # Slide 2 - Caseload historical comparison:
  add_slide(layout = "Content Small Title", master = "Office Theme") %>%
  ph_with_text(type = "title", index=1,str = "Caseload Historical Comparison") %>% 
  ph_with_table(type = "body", value = slide2.data) %>%
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "2" ) %>%
  ph_with_text(type = "dt", str = format(Sys.Date(),"%B %d,%Y")) %>%
  
  # Slide 3 - Caseload by Consultant:
  add_slide(layout = "Content Small Title", master = "Office Theme") %>%
  ph_with_text(type = "title", index=1,str = "Caseload by Consultant") %>% 
  ph_with_table(type = "body", value = slide3.data) %>%
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "3" ) %>%
  ph_with_text(type = "dt", str = format(Sys.Date(),"%B %d,%Y")) %>%
    
  # Slide 4 - Mortalities < 30 days:
  add_slide(layout = "Double Table Small Title", master = "Office Theme") %>%
  ph_with_text(type = "title", index=1,str = "Mortalities < 30 days") %>% 
  ph_with_table(type = "body", index=1, value = slide4.data1) %>%
  ph_with_table(type = "body", index=2, value = slide4.data2) %>%
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "4" ) %>%
  ph_with_text(type = "dt", str = format(Sys.Date(),"%B %d,%Y")) %>%
    
  # Slide 5 - Outpatient Wait Days:
  add_slide(layout = "Table Graph and Small Title", master = "Office Theme") %>%
  ph_with_text(type = "title", index = 1, str = "Outpatient Wait Days") %>% 
  ph_with_gg(type = "body", index = 1, value = slide5.plot) %>%
  ph_with_table(type = "body", index = 2, value = slide5.data) %>%
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "5" ) %>%
  ph_with_text(type = "dt", str = format(Sys.Date(),"%B %d,%Y")) %>%
    
  # Slide 6 - Outpatient Wait > 90:
  add_slide(layout = "Double Table Small Title", master = "Office Theme") %>%
  ph_with_text(type = "title", index = 1, str = "Outpatient Wait > 90") %>%
  ph_with_table(type = "body", index=1, value = slide6.data1) %>%
  ph_with_table(type = "body", index=2, value = slide6.data2) %>%
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "6" ) %>%
  ph_with_text(type = "dt", str =format(Sys.Date(),"%B %d,%Y")) %>%
    
  # Slide 7 - Inpatient Wait Days:
  add_slide(layout = "Table Graph and Small Title", master = "Office Theme") %>%
  ph_with_text(type = "title", index = 1, str = "Inpatient Wait Days") %>% 
  ph_with_gg(type = "body", index = 1, value = slide7.plot) %>%
  ph_with_table(type = "body", index = 2, value = slide7.data) %>%
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "7" ) %>%
  ph_with_text(type = "dt", str = format(Sys.Date(),"%B %d,%Y")) %>%    
  
  # Slide 8 - Inpatient Wait > 10 days:
  add_slide(layout = "Double Table Small Title", master = "Office Theme") %>%
  ph_with_text(type = "title", index=1,str = "Inpatient Wait > 10 days") %>% 
  ph_with_table(type = "body", index=1, value = slide8.data1) %>%
  ph_with_table(type = "body", index=2, value = slide8.data2) %>%
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "8" ) %>%
  ph_with_text(type = "dt", str =format(Sys.Date(),"%B %d,%Y")) %>%
  
  # Slide 9 - ICU Stay (days):
  add_slide(layout = "Table Graph and Small Title", master = "Office Theme") %>%
  ph_with_text(type = "title", index=1,str = "ICU Stay (days)") %>% 
  ph_with_gg(type = "body", index = 1, value = slide9.plot) %>%
  ph_with_table(type = "body", index = 2, value = slide9.data) %>%
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "9" ) %>%
  ph_with_text(type = "dt", str =format(Sys.Date(),"%B %d,%Y")) %>%
  
  # Slide 10 - Prolonged ICU Stay (>3 days):
  add_slide(layout = "Double Table Small Title", master = "Office Theme") %>%
  ph_with_text(type = "title", index=1,str = "Prolonged ICU Stay (>3 days)") %>% 
  ph_with_table(type = "body", index=1, value = slide10.data1) %>%
  ph_with_table(type = "body", index=2, value = slide10.data2) %>%
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "10" ) %>%
  ph_with_text(type = "dt", str =format(Sys.Date(),"%B %d,%Y")) %>%
  
  # Slide 11 - Post-OP LOS (days):
  add_slide(layout = "Table Graph and Small Title", master = "Office Theme") %>%
  ph_with_text(type = "title", index=1,str = "Post-OP LOS (days)") %>% 
  ph_with_gg(type = "body", index = 1, value = slide11.plot) %>%
  ph_with_table(type = "body", index = 2, value = slide11.data) %>%
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "11" ) %>%
  ph_with_text(type = "dt", str =format(Sys.Date(),"%B %d,%Y")) %>%
  
  # Slide 12 - Prolonged Post-OP LOS (>10 days):
  add_slide(layout = "Double Table Small Title", master = "Office Theme") %>%
  ph_with_text(type = "title", index=1,str = "Prolonged Post-OP LOS (>10 days)") %>% 
  ph_with_table(type = "body", index=1, value = slide12.data1) %>%
  ph_with_table(type = "body", index=2, value = slide12.data2) %>%
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "12" ) %>%
  ph_with_text(type = "dt", str =format(Sys.Date(),"%B %d,%Y")) %>%  

  # Slide 13 - Post-op AMI or Cardiac Arrest:
  add_slide(layout = "Double Table Small Title", master = "Office Theme") %>%
  ph_with_text(type = "title", index=1,str = "Post-op AMI or Cardiac Arrest") %>% 
  ph_with_table(type = "body", index=1, value = slide13.data1) %>%
  ph_with_table(type = "body", index=2, value = slide13.data2) %>%
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "13" ) %>%
  ph_with_text(type = "dt", str =format(Sys.Date(),"%B %d,%Y")) %>%

  # slide 14 - IABP Use
  add_slide(layout = "Double Table Small Title", master = "Office Theme") %>%
  ph_with_text(type = "title", index=1,str = "IABP Use") %>% 
  ph_with_table(type = "body", index=1, value = slide14.data1) %>%
  ph_with_table(type = "body", index=2, value = slide14.data2) %>%
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "14" ) %>%
  ph_with_text(type = "dt", str =format(Sys.Date(),"%B %d,%Y"))

  # slide 15 - Transfusions
  add_slide(layout = "Double Table Small Title", master = "Office Theme") %>%
  ph_with_text(type = "title", index=1,str = "Transfusions") %>% 
  ph_with_table(type = "body", index=1, value = slide10.data1) %>%
  ph_with_table(type = "body", index=2, value = slide10.data2) %>%
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "15" ) %>%
  ph_with_text(type = "dt", str =format(Sys.Date(),"%B %d,%Y"))

  # slide 16 - Reintubation & Return to ICU
  add_slide(layout = "Double Table Small Title", master = "Office Theme") %>%
  ph_with_text(type = "title", index=1,str = "Reintubation & Return to ICU") %>% 
  ph_with_table(type = "body", index=1, value = slide10.data1) %>%
  ph_with_table(type = "body", index=2, value = slide10.data2) %>%
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "16" ) %>%
  ph_with_text(type = "dt", str =format(Sys.Date(),"%B %d,%Y"))

  # slide 17 - Return to Theatre
  add_slide(layout = "Double Table Small Title", master = "Office Theme") %>%
  ph_with_text(type = "title", index=1,str = "Return to Theatre") %>% 
  ph_with_table(type = "body", index=1, value = slide10.data1) %>%
  ph_with_table(type = "body", index=2, value = slide10.data2) %>%
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "17" ) %>%
  ph_with_text(type = "dt", str =format(Sys.Date(),"%B %d,%Y"))

  # slide 18 - Infections
  add_slide(layout = "Double Table Small Title", master = "Office Theme") %>%
  ph_with_text(type = "title", index=1,str = "Infections") %>% 
  ph_with_table(type = "body", index=1, value = slide10.data1) %>%
  ph_with_table(type = "body", index=2, value = slide10.data2) %>%
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "18" ) %>%
  ph_with_text(type = "dt", str =format(Sys.Date(),"%B %d,%Y"))

  # slide 19 - Infection details
  add_slide(layout = "Double Table Small Title", master = "Office Theme") %>%
  ph_with_text(type = "title", index=1,str = "Infection details") %>% 
  ph_with_table(type = "body", index=1, value = slide10.data1) %>%
  ph_with_table(type = "body", index=2, value = slide10.data2) %>%
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "19" ) %>%
  ph_with_text(type = "dt", str =format(Sys.Date(),"%B %d,%Y"))
    
  # slide 20 - Atrial Fibrillation & Arrhythmias
  add_slide(layout = "Double Table Small Title", master = "Office Theme") %>%
  ph_with_text(type = "title", index=1,str = "Atrial Fibrillation & Arrhythmias") %>% 
  ph_with_table(type = "body", index=1, value = slide10.data1) %>%
  ph_with_table(type = "body", index=2, value = slide10.data2) %>%
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "20" ) %>%
  ph_with_text(type = "dt", str =format(Sys.Date(),"%B %d,%Y"))
  
  # slide 21 - Neurological Events
  add_slide(layout = "Double Table Small Title", master = "Office Theme") %>%
  ph_with_text(type = "title", index=1,str = "Neurological Events") %>% 
  ph_with_table(type = "body", index=1, value = slide10.data1) %>%
  ph_with_table(type = "body", index=2, value = slide10.data2) %>%
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "21" ) %>%
  ph_with_text(type = "dt", str =format(Sys.Date(),"%B %d,%Y"))
  
  # slide 22 - Other Post-Op Complications
  add_slide(layout = "Double Table Small Title", master = "Office Theme") %>%
  ph_with_text(type = "title", index=1,str = "Other Post-Op Complications") %>% 
  ph_with_table(type = "body", index=1, value = slide10.data1) %>%
  ph_with_table(type = "body", index=2, value = slide10.data2) %>%
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "22" ) %>%
  ph_with_text(type = "dt", str =format(Sys.Date(),"%B %d,%Y"))
  
  # slide 23 - Readmitted <30 days 
  add_slide(layout = "Double Table Small Title", master = "Office Theme") %>%
  ph_with_text(type = "title", index=1,str = "Readmitted <30 days") %>% 
  ph_with_table(type = "body", index=1, value = slide10.data1) %>%
  ph_with_table(type = "body", index=2, value = slide10.data2) %>%
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "23" ) %>%
  ph_with_text(type = "dt", str =format(Sys.Date(),"%B %d,%Y"))
  
  # Print to save powerpoint

  if(No.Months == 1){
    MM.filename <- paste(First.Month,"_2018_M&M.pptx", sep = "")
  } else if(No.Months == 2){
    MM.filename <- paste(First.Month,Second.Month,"_2018_M&M.pptx", sep = "")
  }

  print(doc, target = MM.filename) %>% 
  invisible()

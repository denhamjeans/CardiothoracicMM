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
  group_by(Num_Months) %>%
  summarise(avg = mean(Op_Age), median(Op_Age))



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
  mutate("Percentage" = percent(.[[2]] / Total.Cases[['n']])) 

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
                                  title = "Outpatient Wait Days", 
                                  caption = "*Values calculated from consult date \n *Consult date defined as 'First consult where patient is deemed suitable for surgery'") +
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
                                  title = "Inpatient Wait Days", 
                                  caption = "*Values calculated from consult date \n *Consult date defined as 'First consult where patient is deemed suitable for surgery'") +
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





########################################################################################

#sample code for MnMs

####Age
MM.Pres_Main %>%
  group_by(Num_Months) %>%
  summarise(avg = mean(Op_Age), median(Op_Age))

####Caseload by consultant
Caseload.Consultant <- MM.Pres_Main %>%
  group_by(OpConsultant, OpCategory) %>%
  summarise(count=n()) %>%
  spread(OpConsultant, count) %>%
  adorn_totals("row")

####ICU mean/medians
ICU <- MM.Pres_Main %>%
  mutate(BedBlock = (ICUDays - ICUReqDays)) %>%
  select(Pt_LName, Pt_FName, ICUReqDays, ICUDays, BedBlock) %>%
  summarise(ICUdaysMean = mean(ICUDays, na.rm = TRUE),
            ICUdaysmedian = median(ICUDays, na.rm = TRUE)
            )

####ICU bedblock
ICUbedblock <- MM.Pres_Main %>%
  mutate(BedBlock = (ICUDays - ICUReqDays)) %>%
  select(Pt_LName, Pt_FName, ICUReqDays, ICUDays, BedBlock) %>%
  filter(ICUReqDays > 0) %>%
  summarise(BedBlockaverageperpatient = mean(BedBlock, na.rm = TRUE) * 24,
            maxbedblock = max(BedBlock, na.rm = TRUE) *24
            )

####Outpatients mean/medians
Outpatients.Inpatients <- MM.Pres_Main %>%
  mutate(waitdays = OpDate - ConsultDate) %>%
  select(Adm_Elective, waitdays) %>%
  group_by(Adm_Elective) %>%
  summarise(Mean = mean(waitdays, na.rm = TRUE), median = median(waitdays, na.rm = TRUE))

####Length of Stay
LOS <- MM.Pres_Main %>%
  mutate(LengthOfStay = DischargeDate - OpDate) %>%
  select(Pt_LName, Pt_FName, DischargeDate, OpDate, LengthOfStay) %>%
  summarise(Mean = mean(LengthOfStay, na.rm = TRUE),
            median = median(LengthOfStay, na.rm = TRUE),
            )

LOS.over100 <- MM.Pres_Main %>%
  mutate(LengthOfStay = DischargeDate - OpDate) %>%
  select(Pt_LName, Pt_FName, DischargeDate, OpDate, LengthOfStay, Num_Months) %>%
  filter(LengthOfStay>10) %>%
  arrange(desc(LengthOfStay)) %>%
  spread(Num_Months, LengthOfStay)

####IABP
IABP.data <- MM.Pres_Main %>%
  filter(IABP==1) %>%
  select(Pt_LName, Pt_FName, OpConsultantInitials, OpDescription, Num_Months, IABP, IABP_When, IABP_Indication)


#### Transfusions
Transfusions <- MM.Pres_Main %>%
  select(Num_Months, Pt_LName, Pt_FName, OpConsultantInitials, OpDescription, Bld_Tot_RBC, Bld_Tot_FFP, Bld_Tot_Plt, Bld_Tot_Cryo, Bld_Tot_FVIIa_mg, Bld_Tot_PTX) %>%
  mutate(Total_Blood_Products = Bld_Tot_RBC + Bld_Tot_FFP + Bld_Tot_Plt + Bld_Tot_Cryo) %>%
  group_by(Num_Months) %>%
  summarise(numberoftransfusions = sum(Total_Blood_Products>1),
            RBCover4 = sum(Bld_Tot_RBC>4),
            RBCover10 = sum(Bld_Tot_RBC>15),
            counttotalover15 = sum(Total_Blood_Products >15))


Many.Transfusions <- MM.Pres_Main %>%
  select(Num_Months, Pt_LName, Pt_FName, OpConsultantInitials, OpDescription, Bld_Tot_RBC, Bld_Tot_FFP, Bld_Tot_Plt, Bld_Tot_Cryo, Bld_Tot_FVIIa_mg, Bld_Tot_PTX) %>%
  mutate(Total_Blood_Products = Bld_Tot_RBC + Bld_Tot_FFP + Bld_Tot_Plt + Bld_Tot_Cryo) %>%
  group_by(Num_Months) %>%
  filter(Total_Blood_Products>=10) %>%
  arrange(Num_Months, desc(Total_Blood_Products))

####stroke patients
stroke.data <- MM.Pres_Main %>%
  filter(PO_Stroke>0)

####AF patients
af.data <- MM.Pres_Main %>%
  filter(PO_AF>0) %>%
  group_by(Num_Months) %>%
  summarise(count = n())

#### Temporary pacing
temppacing.data <- MM.Pres_Main %>%
  filter(PO_TempPacing >0) %>%
  group_by(Num_Months) %>%
  summarise(count = n())

####PPM insertion
PPM.data <- MM.Pres_Main %>%
  filter(PO_PPMforHB >0 | PO_PPMforBrady > 0) %>%
  group_by(Num_Months) %>%
  summarise(count = n())

####Cardioversion
cardioversion.data <- MM.Pres_Main %>%
  filter(PO_DCCV >0) %>%
  group_by(Num_Months) %>%
  summarise(count = n())

####Ventilation
venilation.data <- MM.Pres_Main %>%
  mutate(VentilationTime = PO_ExtbDt - PO_ICUAdmDt) %>%
  select(Pt_LName, Pt_FName, Num_Months, VentilationTime) %>%
  filter(VentilationTime > 1) %>%
  group_by(Num_Months) %>%
  summarise(count = n())
  
####Reintubated
Reintubation.data <- MM.Pres_Main %>%
  


####Pneumothorax
Pneumothorax.data <- MM.Pres_Main %>%
  filter(PO_PTX >0) %>%
  group_by(Num_Months) %>%
  summarise(count = n())

####DVT patient
DVT.data <- MM.Pres_Main %>%
  filter(PO_DVT > 0) %>%
  select(Pt_LName, Pt_FName,OpConsultantInitials, OpDescription, PO_DVT)












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

#Example captions and footer for the presentations
cap1<-"During the Great Recession homeowner vacancy rates spiked, and gradually came back down. Rental vacancy rates did not spike nearly as much, but also came down in recent years as housing markets have gotten pretty tight."
cap2<-"The top two panels show the vacant for rent vacant for sale units that make up the rental and homeowner vacancy rates. The bottom right panel shows year-round vacant units which have been rented or sold but the new renters or owners have not moved in yet. That has a pretty clear seasonal pattern, matching the rhythm of the U.S. housing market, but remains constant at a little under one percent. The bottom left panel shows the share of housing units that are vacant and held off the market."
cap3<-"The year-round vacant other category has increased almost a full percentage point since 2005. Just to be clear, that's a lot of housing units. A one percentage point increase corresponds to over one million housing units. The largest component, taking up about a quarter are those units vacant due to personal/family reasons. This includes situations where the owner is in assisted living and not occupying the unit."
cap4<-"The U.S. Census Bureau began tracking a breakdown of the other category since 2012. This chart shows the breakdown of the percent distribution for the second quarter of 2017."
cap5<-"The share vacant due to foreclosure has declined quite a lot since 2012 when the U.S. housing market was still early in recovery. Those foreclosed housing units have largely moved through the system."
myftr<-"RoryDenham R to PowerPoint"


# Build the Deck ------------------------


read_pptx("Powerpoint_Templates/blank.pptx") %>%
  layout_summary()

read_pptx("Powerpoint_Templates/blank.pptx") %>%
  layout_properties(layout = "Content Only", master = "Office Theme")

annotate_base(path = "Powerpoint_Templates/blank.pptx", output_file = "annotated_layout_base_edit.pptx")


doc <- read_pptx("Powerpoint_Templates/blank.pptx") %>%
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
  ph_with_text(type = "body", index=1,str = "Caseload Historical Comparison") %>% 
  ph_with_table(type = "body", value = slide2.data) %>%
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "1" ) %>%
  ph_with_text(type = "dt", str = format(Sys.Date(),"%B %d,%Y")) %>%
  
  # Slide 3 - Caseload by Consultant:
  add_slide(layout = "Content Small Title", master = "Office Theme") %>%
  ph_with_text(type = "body", index=1,str = "Caseload Historical Comparison") %>% 
  ph_with_table(type = "body", value = slide3.data) %>%
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "1" ) %>%
  ph_with_text(type = "dt", str = format(Sys.Date(),"%B %d,%Y")) %>%
    
  # Slide 4 - Mortalities < 30 days:
  add_slide(layout = "Content Small Title", master = "Office Theme") %>%
  ph_with_text(type = "body", index=2,str = cap3) %>% 
  ph_with_img(type = "body", index = 1, src = "img/chart3.png") %>%
  ph_with_text(type = "title", index=1,str = "Growth comes from other category") %>%
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "4" ) %>%
  ph_with_text(type = "dt", str = format(Sys.Date(),"%B %d,%Y")) %>%
    
  # Slide 5 - Outpatient Wait Days:
  add_slide(layout = "Table Graph and Small Title", master = "Office Theme") %>%
  ph_with_text(type = "body", index= 1, str = "Outpatient Wait Days") %>% 
  ph_with_gg(type = "body", index = 2, ) %>%
  ph_with_table(type = "body", )
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "5" ) %>%
  ph_with_text(type = "dt", str = format(Sys.Date(),"%B %d,%Y")) %>%
    
  # Slide 6 - Outpatient Wait > 100:
  add_slide(layout = "Content Small Title", master = "Office Theme") %>%
  ph_with_text(type = "body", index=2,str = cap5) %>% 
  ph_with_img(type = "body", index = 1, src = "img/chart5.png") %>%
  ph_with_text(type = "title", index=1,str = "Distribution of other category over time") %>%
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "6" ) %>%
  ph_with_text(type = "dt", str =format(Sys.Date(),"%B %d,%Y")) %>%
    
  # Slide 7 - Inpatient Wait Days:
  add_slide(layout = "Table Graph and Small Title", master = "Office Theme") %>%
  ph_with_text(type = "body", index= 1, str = "Outpatient Wait Days") %>% 
  ph_with_gg(type = "body", index = 2, ) %>%
  ph_with_table(type = "body", )
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "5" ) %>%
  ph_with_text(type = "dt", str = format(Sys.Date(),"%B %d,%Y")) %>%    
    
  # Slide 8 - Inpatient Wait > 10 days:
  add_slide(layout = "Content Small Title", master = "Office Theme") %>%
  ph_with_text(type = "body", index=2,str = cap5) %>% 
  ph_with_img(type = "body", index = 1, src = "img/chart5.png") %>%
  ph_with_text(type = "title", index=1,str = "Distribution of other category over time") %>%
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "6" ) %>%
  ph_with_text(type = "dt", str =format(Sys.Date(),"%B %d,%Y")) %>%
      
  # Print to save powerpoint

  if(No.Months == 1){
    MM.filename <- paste(First.Month,"_2018_M&M.pptx", sep = "")
  } else if(No.Months == 2){
    MM.filename <- paste(First.Month,Second.Month,"_2018_M&M.pptx", sep = "")
  }

  print(doc, target = MM.filename) %>% 
  invisible()

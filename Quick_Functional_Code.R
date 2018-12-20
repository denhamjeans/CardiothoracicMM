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




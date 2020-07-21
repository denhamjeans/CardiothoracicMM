SELECT 
 tbl_Patient.PatientID,
 tbl_Admission.AdmissionID,
 tbl_Operation.OperationID,
 AdmHospital.label AS Adm_Hospital,
 TFHospital.label AS TF_Hospital,
 UCASE(CONCAT(tbl_Patient.Pt_LName, ", ", LEFT(tbl_Patient.Pt_FName,1))) AS Pt_LName,
 tbl_Patient.Pt_FName,
 tbl_Patient.Pt_MRN_Pub,
 timestampdiff(YEAR, tbl_Patient.Pt_DOB, tbl_Operation.OpDate) AS EuroScore_age,
 IF(Sex.label='Female', 1, 0) AS EuroScore_sex,
 tbl_preopstatus.PreOp_Dialysis AS EuroScore_ri_dialysis,
 tbl_preopstatus.PreOp_RenalImpair AS EuroScore_ri_impairment,
 CASE WHEN Sex.label = 'Female' THEN 
  ROUND((140-(timestampdiff(YEAR, tbl_Patient.Pt_DOB, tbl_Operation.OpDate)))*tbl_Operation.Weight*0.85/(tbl_pathology.Path_PreOpCreat*1000*0.0113*72),2)
 WHEN Sex.label = 'Male' THEN
  ROUND((140-(timestampdiff(YEAR, tbl_Patient.Pt_DOB, tbl_Operation.OpDate)))*tbl_Operation.Weight/(tbl_pathology.Path_PreOpCreat*1000*0.0113*72), 2)
 ELSE NULL END AS EuroScore_ri_creatinine_clearance,
 tbl_preopstatus.PreOp_PVD AS EuroScore_eca,
 NULL AS EuroScore_pm,
 if(Prev_CardiacProcedure = 0, 0, Prev_CardiacSurgery) AS EuroScore_pcs,
 PreOp_RespDisease AS EuroScore_cld,
 IF(PreOp_IE = 0, 0, PreOp_IEActive) AS EuroScore_ae,
 IF(PreOp_ArrhyVentType = 2 OR PreOp_ArrhyVentType = 1 OR PreOp_Resus = 1 OR PreOp_CardiogenicShock = 1 OR IABP_When = 1 OR PreOp_RenalImpair = 1 OR PreOp_Dialysis = 1, 1, 0) AS EuroScore_cps,
 IF(PreOp_DiabControl = 4, 1, 0) AS EuroScore_doi,
 PreOp_NYHA as EuroScore_nyha,
 IF(PreOp_CCS = 4, 1, 0) AS EuroScore_ccs,
 CASE WHEN Diag_HaemoEF IS NOT NULL THEN Diag_HaemoEF
	WHEN Diag_HaemoLevel = 1 THEN 65
	WHEN Diag_HaemoLevel = 2 THEN 55
	WHEN Diag_HaemoLevel = 3 THEN 35
	WHEN Diag_HaemoLevel = 4 THEN 25
	ELSE NULL END AS EuroScore_lv_ef,
 PreOp_MI AS EuroScore_rmi,
 PreOp_PulmHTN AS EuroScore_ph,
 tbl_Operation.Op_StatusPOW AS EuroScore_em,
 CASE WHEN ref_procedure.CABG = 1 AND ref_procedure.IsoProcedure = 1 THEN 1
	WHEN ref_procedure.CABG = 0 AND ref_procedure.IsoProcedure = 1 THEN 2
    WHEN ROUND (   
        ((
            LENGTH(ref_procedure.OpDescription)
            - LENGTH( REPLACE ( ref_procedure.OpDescription, "/", "") ) 
        ) / LENGTH("/")) + 1        
    ) = 2 THEN 3
    WHEN ROUND (   
        ((
            LENGTH(ref_procedure.OpDescription)
            - LENGTH( REPLACE ( ref_procedure.OpDescription, "/", "") ) 
        ) / LENGTH("/")) + 1        
    ) > 2 THEN 4
    ELSE NULL END AS EuroScore_woi,
 ref_Procedure.Aorta AS EuroScore_sta,
 tbl_operation.OpDate,
 ref_Procedure.OpDescription,
 ref_Procedure.OpCategory,
 ref_Procedure.Other,
 tbl_Operation.Op_StatusPOW,
 tbl_Operation.Op_Incidence,
 timestampdiff(YEAR, tbl_Patient.Pt_DOB, tbl_Operation.OpDate) AS Op_Age,
 tbl_ProcedureStaff.Dr_LName AS OpConsultant,
 tbl_ProcedureStaff.Dr_Initials AS OpConsultantInitials,
 tbl_ProcedureStaff_1.Dr_LName AS AdmConsultant,
 tbl_ProcedureStaff_1.Dr_Initials AS AdmConsultantInitials,
 tbl_Operation.Op_Assist1,
 tbl_Operation.Op_Assist2,
 tbl_Operation.Op_Assist3,
 tbl_Operation.Op_Anaesthetist1,
 tbl_Operation.Op_Anaesthetist2,
 tbl_Admission.Adm_Elective,
 tbl_Operation.Op_Incidence,
 tbl_Admission.AdmDate,
 tbl_Admission.ConsultDate,
 tbl_Admission.DischargeDate,
  CAST(CONCAT(
			 DATE_FORMAT(tbl_operation.OpDate, "%Y-%m-%d"), " ",
			 DATE_FORMAT(tbl_CPB.CPB_StartTime, "%H:%i:%s")
			 ) AS DATETIME) AS `CPB_StartTime`,
  CAST(CONCAT(
			 DATE_FORMAT(tbl_operation.OpDate, "%Y-%m-%d"), " ",
			 DATE_FORMAT(tbl_CPB.CPB_EndTime, "%H:%i:%s")
			 ) AS DATETIME) AS `CPB_EndTime`,
 TIMEDIFF(CPB_EndTime, CPB_StartTime) AS `CPB_PerfTime`,
 tbl_Admission.Adm_Comments,
 tbl_PreOpStatus.PreOp_Comments,
 tbl_IntraOpSupport.IABP,
 IABP_W.label AS IABP_When,
 IABPIndic.label AS IABP_Indication,
 tbl_Transfusion.Bld_Tot_RBC,
 tbl_Transfusion.Bld_Tot_FFP,
 tbl_Transfusion.Bld_Tot_Plt,
 tbl_Transfusion.Bld_Tot_Cryo,
 tbl_Transfusion.Bld_Tot_FVIIa_mg,
 tbl_Transfusion.Bld_Tot_PTX,
 CAST(CONCAT(
			 DATE_FORMAT(tbl_PostOpStatus.PO_ICUAdmDt, "%Y-%m-%d"), " ",
			 DATE_FORMAT(tbl_PostOpStatus.PO_ICUAdmTm, "%H:%i:%s")
			 ) AS DATETIME) AS `PO_ICUAdmDt`,
 CAST(CONCAT(
			 DATE_FORMAT(tbl_IntraOpSupport.IntubationDt, "%Y-%m-%d"), " ",
			 DATE_FORMAT(tbl_IntraOpSupport.IntubationTm, "%H:%i:%s")
			 ) AS DATETIME) AS `IntubationDt`,
 CAST(CONCAT(
			 DATE_FORMAT(tbl_PostOpStatus.PO_ExtbDt, "%Y-%m-%d"), " ",
			 DATE_FORMAT(tbl_postopstatus.PO_ExtbTm, "%H:%i:%s")
			 ) AS DATETIME) AS `PO_ExtbDt`,
 TIMEDIFF(
			CAST(CONCAT(
						DATE_FORMAT(tbl_PostOpStatus.PO_ExtbDt, "%Y-%m-%d"), " ",
						DATE_FORMAT(tbl_postopstatus.PO_ExtbTm, "%H:%i:%s")
						) AS DATETIME),
			CAST(CONCAT(
						DATE_FORMAT(tbl_PostOpStatus.PO_ICUAdmDt, "%Y-%m-%d"), " ",
						DATE_FORMAT(tbl_postopstatus.PO_ICUAdmTm, "%H:%i:%s")
						) AS DATETIME)
			) AS POST_OP_VENT_DURATION,
 IF(TIMEDIFF(CAST(CONCAT(
						  DATE_FORMAT(tbl_PostOpStatus.PO_ExtbDt, "%Y-%m-%d"), " ",
						  DATE_FORMAT(tbl_postopstatus.PO_ExtbTm, "%H:%i:%s")
						  ) AS DATETIME),
				   CAST(CONCAT(
							   DATE_FORMAT(tbl_PostOpStatus.PO_ICUAdmDt, "%Y-%m-%d"), " ",
							   DATE_FORMAT(tbl_postopstatus.PO_ICUAdmTm, "%H:%i:%s")
							   ) AS DATETIME)) > '24:00:00',
			  1, 0) AS `PO_Vent>24hr`,
 CAST(CONCAT(
			 DATE_FORMAT(tbl_PostOpStatus.IABP_InDt, "%Y-%m-%d"), " ",
			 DATE_FORMAT(tbl_PostOpStatus.IABP_InTm, "%H:%i:%s")
			 ) AS DATETIME) AS `IABP_InDt`,
 CAST(CONCAT(
			 DATE_FORMAT(tbl_PostOpStatus.IABP_OutDt, "%Y-%m-%d"), " ",
			 DATE_FORMAT(tbl_PostOpStatus.IABP_OutTm, "%H:%i:%s")
			 ) AS DATETIME) AS `IABP_OutDt`,
 tbl_PostOpStatus.PO_Drain4Hr,
 tbl_PostOpStatus.PO_Drain6Hr,
 tbl_PostOpStatus.PO_Drain24Hr,
 CAST(CONCAT(
			 DATE_FORMAT(tbl_PostOpStatus.PO_ReIntbDt, "%Y-%m-%d"), " ",
			 DATE_FORMAT(tbl_PostOpStatus.PO_ReIntbTm, "%H:%i:%s")
			 ) AS DATETIME) AS `PO_ReIntbDt`,
 CAST(CONCAT(
			 DATE_FORMAT(tbl_PostOpStatus.PO_ReExtbDt, "%Y-%m-%d"), " ",
			 DATE_FORMAT(tbl_PostOpStatus.PO_ReExtbTm, "%H:%i:%s")
			 ) AS DATETIME) AS `PO_ReExtbDt`,
 CAST(CONCAT(
			 DATE_FORMAT(tbl_PostOpStatus.PO_IDCOutDate, "%Y-%m-%d"), " ",
			 DATE_FORMAT(tbl_PostOpStatus.PO_IDC_OutTime, "%H:%i:%s")
			 ) AS DATETIME) AS `PO_IDCOutDate`,
 CAST(CONCAT(
			 DATE_FORMAT(tbl_PostOpStatus.PO_RFWDt, "%Y-%m-%d"), " ",
			 DATE_FORMAT(tbl_PostOpStatus.PO_RFWTm, "%H:%i:%s")
			 ) AS DATETIME) AS `PO_RFWDt`,
 CAST(CONCAT(
			 DATE_FORMAT(tbl_PostOpStatus.PO_ICUOutDt, "%Y-%m-%d"), " ",
			 DATE_FORMAT(tbl_PostOpStatus.PO_ICUOutTm, "%H:%i:%s")
			 ) AS DATETIME) AS `PO_ICUOutDt`,
 tbl_PostOpStatus.ICUDays,
 tbl_PostOpStatus.ICUReqDays,
 tbl_PostOpStatus.PO_ReturnICU,
 tbl_PostOpStatus.PO_Reintubation,
 tbl_PostOpStatus.PO_RTT,
 tbl_PostOpStatus.PO_Arryhthmia,
 tbl_PostOpStatus.PO_AF,
 tbl_PostOpStatus.PO_CardArrest,
 tbl_PostOpStatus.PO_ArrhySpecify,
 tbl_PostOpStatus.PO_TempPacing,
 tbl_PostOpStatus.PO_DCCV,
 tbl_PostOpStatus.PO_PPMforHB,
 tbl_PostOpStatus.PO_PPMforBrady,
 tbl_PostOpStatus.PO_Infection,
 tbl_PostOpStatus.PO_PE,
 tbl_PostOpStatus.PO_PTX,
 tbl_PostOpStatus.PO_HTX,
 tbl_PostOpStatus.PO_PleralEff,
 tbl_PostOpStatus.PO_ARDS,
 tbl_PostOpStatus.PO_NewRenalFx,
 tbl_PostOpStatus.PO_Dialysis,
 tbl_PostOpStatus.PO_TIA,
 tbl_PostOpStatus.PO_Stroke,
 tbl_PostOpStatus.PO_COMA,
 tbl_PostOpStatus.PO_AMI,
 tbl_PostOpStatus.PO_ALI,
 tbl_PostOpStatus.PO_AortaDiscet,
 tbl_PostOpStatus.PO_DVT,
 tbl_PostOpStatus.PO_Anticoag,
 tbl_PostOpStatus.PO_AnticoagType,
 tbl_PostOpStatus.PO_GITCompl,
 tbl_PostOpStatus.PO_GITType,
 tbl_PostOpStatus.PO_CardioShock,
 tbl_PostOpStatus.PO_MODS,
 tbl_PostOpStatus.PO_Comments,
 tbl_30dFollowUp.FU_ReAdm30d,
 tbl_30dFollowUp.FU_Death,
 tbl_30dFollowUp.FU_Death30d,
 tbl_30dFollowUp.FU_DeathDt,
 tbl_30dFollowUp.Death_DaysPO,
 DeathCause1.label AS FU_DeathCause1,
 DeathCause2.label AS FU_DeathCause2,
 DeathCategory.label AS FU_DeathCategory,
 DeathLocation.label AS FU_DeathLocation,
 tbl_30dFollowUp.FU_DeathNotes
 
FROM 
 tbl_Patient
 RIGHT JOIN tbl_Admission ON tbl_Patient.PatientID = tbl_Admission.PatientID
 RIGHT JOIN tbl_Operation ON tbl_Admission.AdmissionID = tbl_Operation.AdmissionID
 LEFT JOIN tbl_ProcedureStaff AS tbl_ProcedureStaff_1 ON tbl_Admission.Adm_Consultant = tbl_ProcedureStaff_1.StaffID 
 LEFT JOIN tbl_PostOpStatus ON tbl_PostOpStatus.OperationID = tbl_Operation.OperationID
 LEFT JOIN tbl_Transfusion ON tbl_Transfusion.OperationID = tbl_Operation.OperationID
 LEFT JOIN tbl_PreOpStatus ON tbl_PreOpStatus.OperationID = tbl_Operation.OperationID
 LEFT JOIN tbl_IntraOpSupport ON tbl_Operation.OperationID = tbl_IntraOpSupport.OperationID
 LEFT JOIN tbl_ProcedureStaff ON tbl_Operation.Op_Consultant = tbl_ProcedureStaff.StaffID
 LEFT JOIN tbl_pathology ON tbl_pathology.OperationID = tbl_operation.OperationID
 LEFT JOIN ref_Procedure ON tbl_Operation.Op_Description = ref_Procedure.ProcedureID
 LEFT JOIN tbl_CPB ON tbl_CPB.OperationID = tbl_Operation.OperationID
 LEFT JOIN tbl_diagnostics ON tbl_diagnostics.OperationID = tbl_Operation.OperationID
 LEFT JOIN tbl_30dFollowUp ON tbl_Operation.AdmissionID = tbl_30dFollowUp.AdmissionID
 LEFT JOIN tbl_prevcardproc ON tbl_Operation.OperationID = tbl_prevcardproc.OperationID
 LEFT JOIN domains AS IABP_W ON tbl_IntraOpSupport.IABP_When = IABP_W.code AND IABP_W.domain = 'When'
 LEFT JOIN domains AS IABPIndic ON tbl_IntraOpSupport.IABP_Indication = IABPIndic.code AND IABPIndic.domain = 'IABPIndic'
 LEFT JOIN domains AS DeathCause1 ON tbl_30dFollowUp.FU_DeathCause1 = DeathCause1.code AND DeathCause1.domain = 'DeathCause'
 LEFT JOIN domains AS DeathCause2 ON tbl_30dFollowUp.FU_DeathCause2 = DeathCause2.code AND DeathCause2.domain = 'DeathCause2'
 LEFT JOIN domains AS DeathCategory ON tbl_30dFollowUp.FU_DeathCategory = DeathCategory.code AND DeathCategory.domain = 'DeathCategory'
 LEFT JOIN domains AS DeathLocation ON tbl_30dFollowUp.FU_DeathLocation = DeathLocation.code AND DeathLocation.domain = 'DeathLocation'
 LEFT JOIN domains AS TFHospital ON tbl_Admission.Adm_TransfHosp = TFHospital.code AND TFHospital.domain = 'TFHospital'
 LEFT JOIN domains AS AdmHospital ON tbl_Admission.Adm_Hospital = AdmHospital.code AND AdmHospital.domain = 'Hospital'
 LEFT JOIN domains AS Sex ON tbl_patient.Pt_Gender = Sex.code AND Sex.domain = 'Gender'

WHERE cts_db.tbl_Operation.OpDate BETWEEN CAST({MinDate} AS DATE) AND CAST({MaxDate} AS DATE);

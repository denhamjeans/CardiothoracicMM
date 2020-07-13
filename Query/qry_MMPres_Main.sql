SELECT 
 tbl_Patient.PatientID,
 tbl_Admission.AdmissionID,
 tbl_Operation.OperationID,
 AdmHospital.label AS Adm_Hospital,
 TFHospital.label AS TF_Hospital,
 UCASE(CONCAT(tbl_Patient.Pt_LName, ", ", LEFT(tbl_Patient.Pt_FName,1))) AS Pt_LName,
 tbl_Patient.Pt_FName,
 tbl_Patient.Pt_MRN_Pub,
 tbl_operation.OpDate,
 ref_Procedure.OpDescription,
 ref_Procedure.OpCategory,
 ref_Procedure.Other,
 tbl_Operation.Op_StatusPOW,
 tbl_Operation.Op_Incidence,
 tbl_Operation.Op_Age,
 tbl_IntraOpSupport.EUROScore,
 tbl_IntraOpSupport.EUROScoreLogistic,
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
 tbl_CPB.CPB_PerfTime,
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
 tbl_PostOpStatus.PO_ICUAdmDt,
 tbl_PostopStatus.PO_ICUAdmTm,
 tbl_PostOpStatus.IABP_InDt,
 tbl_PostOpStatus.IABP_InTm,
 tbl_PostOpStatus.IABP_OutDt,
 tbl_PostOpStatus.IABP_OutTm,
 tbl_IntraOpSupport.IntubationDt,
 tbl_IntraOpSupport.IntubationTm,
 tbl_PostOpStatus.PO_ExtbDt,
 tbl_PostOpStatus.PO_ExtbTm,
 tbl_PostOpStatus.PO_Drain4Hr,
 tbl_PostOpStatus.PO_Drain6Hr,
 tbl_PostOpStatus.PO_Drain24Hr,
 tbl_PostOpStatus.PO_ReIntbDt,
 tbl_PostOpStatus.PO_ReIntbTm,
 tbl_PostOpStatus.PO_ReExtbDt,
 tbl_PostOpStatus.PO_ReExtbTm,
 tbl_PostOpStatus.PO_IDCOutDate,
 tbl_PostOpStatus.PO_IDC_OutTime,
 tbl_PostOpStatus.PO_RFWDt,
 tbl_PostOpStatus.PO_RFWTm,
 tbl_PostOpStatus.PO_ICUOutDt,
 tbl_PostOpStatus.PO_ICUOutTm,
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
 LEFT JOIN ref_Procedure ON tbl_Operation.Op_Description = ref_Procedure.ProcedureID
 LEFT JOIN tbl_CPB ON tbl_CPB.OperationID = tbl_Operation.OperationID
 LEFT JOIN tbl_30dFollowUp ON tbl_Operation.AdmissionID = tbl_30dFollowUp.AdmissionID
 LEFT JOIN domains AS IABP_W ON tbl_IntraOpSupport.IABP_When = IABP_W.code AND IABP_W.domain = 'When'
 LEFT JOIN domains AS IABPIndic ON tbl_IntraOpSupport.IABP_Indication = IABPIndic.code AND IABPIndic.domain = 'IABPIndic'
 LEFT JOIN domains AS DeathCause1 ON tbl_30dFollowUp.FU_DeathCause1 = DeathCause1.code AND DeathCause1.domain = 'DeathCause'
 LEFT JOIN domains AS DeathCause2 ON tbl_30dFollowUp.FU_DeathCause2 = DeathCause2.code AND DeathCause2.domain = 'DeathCause2'
 LEFT JOIN domains AS DeathCategory ON tbl_30dFollowUp.FU_DeathCategory = DeathCategory.code AND DeathCategory.domain = 'DeathCategory'
 LEFT JOIN domains AS DeathLocation ON tbl_30dFollowUp.FU_DeathLocation = DeathLocation.code AND DeathLocation.domain = 'DeathLocation'
 LEFT JOIN domains AS TFHospital ON tbl_Admission.Adm_TransfHosp = TFHospital.code AND TFHospital.domain = 'TFHospital'
 LEFT JOIN domains AS AdmHospital ON tbl_Admission.Adm_Hospital = AdmHospital.code AND AdmHospital.domain = 'Hospital'

WHERE cts_db.tbl_Operation.OpDate BETWEEN CAST({MinDate} AS DATE) AND CAST({MaxDate} AS DATE);

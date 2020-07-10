SELECT 
 CONCAT(tbl_Patient.Pt_LName, " ( ", tbl_ProcedureStaff.Dr_Initials, ") ", tbl_Procedure.OpDescription) AS PatientDetails,
 tbl_Readmission.ReAdm_DaysPO AS DaysPO,
 tbl_TFHospitals.TF_Hospital AS Facility,
 tbl_Readmission.ReAdm_LOS AS LengthOfStay,
 tbl_Readmission.ReAdm_Reason,
 tbl_Readmission.ReAdm_Notes,
 tbl_Operation.OpDate
FROM 
 tbl_Patient 
 RIGHT JOIN tbl_Admission ON tbl_Patient.PatientID = tbl_Admission.PatientID
 RIGHT JOIN tbl_Operation ON tbl_Admission.AdmissionID = tbl_Operation.AdmissionID
 LEFT JOIN tbl_Readmission ON tbl_Readmission.AdmissionID = tbl_Admission.AdmissionID
 LEFT JOIN tbl_TFHospitals ON tbl_Readmission.ReAdm_Location = tbl_TFHospitals.HospCode
 LEFT JOIN tbl_Procedure ON tbl_Operation.Op_Description = tbl_Procedure.ProcedureID
 LEFT JOIN qry_MM_DateRange ON tbl_Operation.OperationID = qry_MM_DateRange.OperationID ON tbl_Admission.AdmissionID = tbl_Operation.AdmissionID
 LEFT JOIN tbl_ProcedureStaff ON tbl_Admission.Adm_Consultant = tbl_ProcedureStaff.StaffID
 
WHERE 
 tbl_Readmission.ReAdm_DaysPO<31
 
ORDER BY 
 tbl_Patient.Pt_LName" ( "tbl_ProcedureStaff]![Dr_Initials]+") "+[tbl_Procedure]![OpDescription], tbl_Readmission.ReAdm_DaysPO;

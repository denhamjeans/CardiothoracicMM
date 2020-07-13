SELECT 
 CONCAT(tbl_Patient.Pt_LName, " ( ", tbl_ProcedureStaff.Dr_Initials, ") ", ref_Procedure.OpDescription) AS PatientDetails,
 tbl_Readmission.ReAdm_DaysPO AS DaysPO,
 TFHospitals.label AS Facility,
 tbl_Readmission.ReAdm_LOS AS LengthOfStay,
 tbl_Readmission.ReAdm_Reason,
 tbl_Readmission.ReAdm_Notes,
 tbl_Operation.OpDate
FROM 
 tbl_Patient 
 RIGHT JOIN tbl_Admission ON tbl_Patient.PatientID = tbl_Admission.PatientID
 RIGHT JOIN tbl_Operation ON tbl_Admission.AdmissionID = tbl_Operation.AdmissionID
 LEFT JOIN tbl_Readmission ON tbl_Readmission.AdmissionID = tbl_Admission.AdmissionID
 LEFT JOIN cts_db.domains AS TFHospitals ON TFHospitals.code = tbl_Readmission.ReAdm_Location AND TFHospitals.domain = 'TFHospital'
 LEFT JOIN ref_Procedure ON tbl_Operation.Op_Description = ref_Procedure.ProcedureID
 LEFT JOIN tbl_ProcedureStaff ON tbl_Admission.Adm_Consultant = tbl_ProcedureStaff.StaffID
 
WHERE 
 tbl_Readmission.ReAdm_DaysPO<31 AND cts_db.tbl_Operation.OpDate BETWEEN CAST({MinDate} AS DATE) AND CAST({MaxDate} AS DATE)
 
ORDER BY 
 CONCAT(tbl_Patient.Pt_LName, " ( ", tbl_ProcedureStaff.Dr_Initials, ") ", ref_Procedure.OpDescription), tbl_Readmission.ReAdm_DaysPO;

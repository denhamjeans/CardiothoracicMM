USE cts_db;
SELECT CONCAT(tbl_Patient.Pt_LName, " (", tbl_ProcedureStaff.Dr_Initials, ") ", ref_Procedure.OpDescription) AS 'Patient Details',
 tbl_ReturnToTheatre.RTT_Specify, 
 tbl_ReturnToTheatre.RTT_PODay,
 tbl_Operation.OpDate
FROM 
 tbl_Patient 
 RIGHT JOIN tbl_Admission ON tbl_Patient.PatientID = tbl_Admission.PatientID
 RIGHT JOIN tbl_Operation ON tbl_Admission.AdmissionID = tbl_Operation.AdmissionID
 INNER JOIN tbl_ReturnToTheatre ON tbl_Operation.OperationID = tbl_ReturnToTheatre.OperationID
 INNER JOIN ref_Procedure ON tbl_Operation.Op_Description = ref_Procedure.ProcedureID
 INNER JOIN tbl_ProcedureStaff ON tbl_Admission.Adm_Consultant = tbl_ProcedureStaff.StaffID
 WHERE OpDate BETWEEN CAST('2018-01-01' AS DATE) AND CAST('2019-07-31' AS DATE);


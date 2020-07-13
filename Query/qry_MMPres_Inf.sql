SELECT CONCAT(tbl_Patient.Pt_LName, ' (', tbl_ProcedureStaff.Dr_Initials, ') ', ref_Procedure.OpDescription) AS 'Patient Details',
 InfSite.label AS 'InfSite', 
 AccessSiteInfDepth.label AS AccessInfDepth,
 DonorSiteInfDepth.label AS DonorSiteDepth,
 Inf_SiteSpecify,
 Inf_Organism,
 OpDate
 
FROM 
 tbl_Patient 
 RIGHT JOIN tbl_Admission ON tbl_Patient.PatientID = tbl_Admission.PatientID
 RIGHT JOIN tbl_Operation ON tbl_Admission.AdmissionID = tbl_Operation.AdmissionID
 LEFT JOIN ref_Procedure ON tbl_Operation.Op_Description = ref_Procedure.ProcedureID
 INNER JOIN tbl_Infection ON tbl_Operation.OperationID = tbl_Infection.OperationID
 LEFT JOIN domains AS InfSite ON InfSite.code = tbl_Infection.Inf_Site AND InfSite.domain = "InfSite"
 LEFT JOIN domains AS AccessSiteInfDepth ON AccessSiteInfDepth.code = tbl_Infection.Inf_DepthAccess AND AccessSiteInfDepth.domain = "InfDepth" 
 LEFT JOIN domains AS DonorSiteInfDepth ON DonorSiteInfDepth.code = tbl_Infection.Inf_DepthDonor AND DonorSiteInfDepth.domain = "InfDepth" 
 LEFT JOIN tbl_ProcedureStaff ON tbl_Admission.Adm_Consultant = tbl_ProcedureStaff.StaffID

WHERE cts_db.tbl_Operation.OpDate BETWEEN CAST({MinDate} AS DATE) AND CAST({MaxDate} AS DATE);

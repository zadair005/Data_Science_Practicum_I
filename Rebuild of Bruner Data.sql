--Rebuild of Bruner Patient Population Dataset
SELECT
	pe.PAT_ID, --Id of the Patient
	pe.PAT_ENC_CSN_ID, --Contact Serial Num of the patient
	pe.PAT_ENC_DATE_REAL,
	pe.CONTACT_DATE, --Date of Contact
	pe.ENC_TYPE_C, --Type of Contact
	pe.DEPARTMENT_ID, --Department where the contact happened
	pe.BP_SYSTOLIC, --Type of blood pressure reading
	pe.BP_DIASTOLIC, --Type of blood pressure reading 2
	pe.TEMPERATURE, --Body temperature of patient
	pe.PULSE, -- heart rate of patient
	pe.WEIGHT, --Patient weight
	pe.HEIGHT, --Patient height
	pe.RESPIRATIONS, --Patients respirations rate
	pe.CHECKIN_TIME, -- Check in time
	pe.CHECKOUT_TIME, -- Check out time
	pe.ENC_CLOSE_DATE, --Date encounter closed
	pe.SERV_AREA_ID,  --Id of service area
	pe.BMI, --Patient BMI
	pe.BSA, --Patient Body Surface Area
	pe2.PHYS_BP, --Patient's Bloodpressure that was entered at the encounter
	pe2.PAT_PAIN_SCORE_C, --Patients pain score
	pe4.FAMILY_SIZE, --# in the patient's family
	pe4.FAMILY_INCOME, --Family income
	pe4.PAT_HOMELESS_YN, --Is the patient homeless?
	pe4.TOBACCO_USE_VRFY_YN, --Does the patient use tobacco? Y = Yes, NULL = ?, N = No
	p.PAT_ID, --Unique Patient ID
	p.PAT_NAME, --Patint Name
	--p.ADD_LINE_1, --Address Line 1
	--p.ADD_LINE_2, --Address Line 2
	p.CITY, --Patient City
	p.STATE_C, --Patient State
	p.COUNTY_C, --Patient County
	zcc.NAME "County_Name", --Patient County Name
	p.ZIP, --Patient Zip Code
	p.BIRTH_DATE, --Patient Birthdate
	p.SEX_C, --Patient Gender 1 = Female, 2 = Male, 3 = Unknown
	p.ETHNIC_GROUP_C, --Patient Race 1 = Non-Hispanic, 2 = Hispanic, 3 = Unknown
	p.MARITAL_STATUS_C, --Patient Marital status 1 = Single, 2 = Married, 3 = Legally Separated, 4 = Divorced, 5 = Widowed, 
	--6 = Unknown, 7 = Significant Other, 8 = Domestic Partner, 9 = Common Law, 10 = Annulled, 11 = Interlocutory, 12 = Never Married,
	--13 = Polygamous
	p.LANGUAGE_C, --Language patient speaks
	p.DEF_FIN_CLASS_C, --Patient Financial Class
	p.CUR_PCP_PROV_ID, --Patient's PCP ID
	p.CUR_PRIM_LOC_ID, --PCP Location ID
	p.PAT_MRN_ID, --Patient Medical Record Number
	loc.SERV_AREA_ID, --Service area ID
	loc.LOC_NAME, --Location Name
	had.DX_ID, --List of diagnoses for the appointment 
	edg.CURRENT_ICD10_LIST, --ICD-10 Code
	edg.DX_NAME, -- ICD Description
	had.LINE --Diagnos
FROM PAT_ENC pe --Patient data for each Encounter pt.1
	INNER JOIN PAT_ENC_2 pe2 --Patient data for each Encounter pt.2
		ON pe.PAT_ENC_CSN_ID = pe2.PAT_ENC_CSN_ID
	INNER JOIN PAT_ENC_4 pe4 --Patient data for each Encounter pt.3
		ON pe.PAT_ENC_CSN_ID = pe4.PAT_ENC_CSN_ID
	INNER JOIN PATIENT p --Patient demographic information
		ON pe.PAT_ID = p.PAT_ID
		AND pe.PAT_ENC_CSN_ID = p.MEDS_LAST_REV_CSN
	INNER JOIN CLARITY_LOC loc --Get's us the Bruner Locations
		ON pe.SERV_AREA_ID = loc.SERV_AREA_ID
	INNER JOIN HSP_ADMIT_DIAG had --Connect ICD to Patient
		ON had.PAT_ENC_CSN_ID = pe.PAT_ENC_CSN_ID
		AND had.PAT_ID = pe.PAT_ID
		AND had.PAT_ENC_DATE_REAL = pe.PAT_ENC_DATE_REAL
	INNER JOIN CLARITY_EDG edg --Get's us the ICD codes & descriptions
		ON edg.DX_ID = had.DX_ID
	LEFT OUTER JOIN ZC_COUNTY zcc --Gives us the actual county name and not just a numbered code
		ON zcc.COUNTY_C = p.COUNTY_C
WHERE pe.CONTACT_DATE >= '2017-01-01' --Only patients that visited since the start of 2017
	AND pe.SERV_AREA_ID = '2' --The service area for all the of the Bruner Clinic patients
	AND loc.LOC_NAME LIKE '%Bruner%' --Will restrict to strictly Bruner Clinic patients
	AND edg.CURRENT_ICD10_LIST IN ('I10','I15.9','I15.1','I15.1','I15.0','O10.919', 'G93.2', 'I12.9','I27.20','I27.21','I27.21, K76.6','I27.24',
		'I87.309', 'K76.6', 'E03.9', 'E78.5',--Hypertension Codes
		'E11.9','E11.65','E11.29','E11.8','E11.42','E11.69','E11.21','E11.22','E11.40','E11.319','E11.59','E10.9','E11.41','E13.9','E11.3293',
		'E13.8','E11.311', 'E08.621, L97.509', 'E10.10','E10.22, N18.5', 'E10.3511','E10.35.22','E10.621, L97.529', 'E10.649','E10.9','E11.10',
		'E11.22, N18.5, Z79.4','E11.3522','E11.3591', 'E11.42','E11.40, G99.2','E11.51','E11.52','E11.610','E11.621, L97.509','E11.8','E11.9',
		'E11.9, Z79.4','E13.10','E13.22, N18.5, E13.65', 'E13.621, L97.505','O24.113','O24.414','O24.419', 'O24.419, Z79.4', --Diabetes Codes
		'E66.9','E66.01','O99.212','O99.210','O99.214','O99.213', -- Obesity Codes
		'J45.40','J45.20','J45.30','J45.901','J45.909','J45.41','J45.21','J45.31', 'J69.8','J82','J60','R06.2','J45.2', 'J45.21','J45.22','J45.3',
		'J45.31','J45.32','J45.4','J45.40','J45.42','J45.5','J45.50','J45.51','J45.52','J45.9','J45.90','J45.902','J45.99','J45.990','J45.991',
		'J45.998','J67.8', --Asthma Codes
		'M17.0','M17.11','M17.12','M19.011','M19.90','M19.071','M13.0','M05.79','M06.9','M19.072','M15.0','M05.79','M06.09','M05.711',
		'A49.9, M01.X0','IMO0002','M00.051','M00.061','M00.80','M00.852','M00.859','M00.861','M00.9','M06.831','M06.832','M12.552',
		'M12.562','M13.871','M13.872','M15.4','M16.0','M16.11','M16.12','M16.32','M16.50','M16.51','M16.52','M16.9','M17.10','M17.31','M17.2',
		'M17.32','M17.9','M18.0','M18.11','M18.12','M18.9','M19.012','M19.019','M19.021', 'M19.022','M19.031','M19.032','M19.041','M19.041, M19.042',
		'M19.042','M19.049','M19.079','M19.111','M19.112','M19.141, M19.142','M19.171','M19.172','M19.211','M19.211, M19.212','M19.212','M19.231',
		'M19.232','M19.271','M19.272','M46.58','M47.10','M47.12','M47.16', 'M47.22','M47.26','M47.27','M47.812','M47.816','M47.817','M47.818',
		'M47.819','M47.892','M47.896','M47.897', --Arthritis
		'J44.9','J44.1','J44','J44.0','I27.9', -- Chronic Obstructive Pulmonary Disease (COPD)
		'B18.2', 'B18.1','K75.4','K75.81','B19.20','B19.10','B17.9','B18.2, K74.60','B19.20','K70.10','K74.69, B19.20','B17.10', --Hepatitis
		'N19','N18.9','N18.3','N18.5','N18.4','N18.2','N18.1', 'N27.1','N17','N17.1','N17.2','N17.2','N17.8','N17.9', --Weak or Failing Kidneys
		'I25.10','I11.0','I25.119','I125.118','I25.110', -- Coronary Heart Disease (CHD)
		'C00.0','C01','C02.3','C02.1','C02.9','C03.9','C05.9','C06.9','C07','C08.1','C08.9','C09.9','C11.1','C13.9','C14.0','C15.4','C16.0','C16.9',
		'C17.0','C17.1','C18.0','C18.2','C18.3','C18.4','C18.5', 'C77.2','C18.6','C18.7','C18.9','C18.9, C78.00','C19','C20','C21.0','C21.8','C23',
		'C24.1','C25.1','C25.2','C25.7','C25.9','C26.0','C31.0','C32.0','C32.8','C32.9','C34.10','C34.10','C34.11','C34.12', 'C34.2','C34.30','C34.31',
		'C34.32','C34.81','C34.90','C34.91','C34.92','C37','C40.22','C41.2','C43.59','C43.61','C43.9','C44.02','C44.209','C44.300','C44.301','C44.309',
		'C44.310','C44.311','C44.319','C44.321','C44.40','C44.509','C44.602','C44.609','C44.90','C49.6','C50.219','C50.519','C50.522','C50.911, C50.912',
		'C50.911, C77.3','C50.911, Z17.0','C50.912, C77.3','C50.919, Z15.02', 'C50.922','C51.9','C53.0','C53.1','C53.9','C54.1','C54.9, D63.0','C56.1, C56.2',
		'C57.02','C60.2','C61','C56.9','C57.02','C60.9','C61, C77.5','C61, C79.51','C62.90', 'C62.92','C64.1','C64.2','C64.9','C65.2','C65.9','C66.1','C66.9',
		'C67.0','C67.2','C67.4','C67.5','C67.8','C67.9','C68.0','C69.90','C69.92','C71.3','C71.8','C71.9','C73','C73, C79.51','C76.0','C76.8','C77.0','C77.3',
		'C78.00','C78.6','C78.7','C79.31','C79.51','C78.7','C78.9','C79.9, C73','C80.1','C44.519','C44.622', 'C48.2','C49.4','C50.111','C50.112','C50.212',
		'C50.311','C50.312','C50.411','C50.412','C50.811','C50.812','C50.911','C50.912','C50.919','C52','C54.8','C54.9','C55','C56.1','C56.9','C61','C71.2',
		'C71.8','C71.9','C73','C76.2','C78.00','C78.01','C78.02','C78.5','C78.6','C79.31','C79.51','C79.89','C80.0','C80.1','C83.30','C83.33','C83.35','C83.39',
		'C85.90','C90.00','D05.10','D05.11','D05.12','D06.9','D18.01','D18.03','D22.30','D22.4','D22.5','D22.61','D22.62','D22.9','D25.0','D25.1','D25.2',
		'D25.9','D37.6','D39.12','D47.2','D47.3','D48.5','D48.62','D48.9') --Cancer Codes
		

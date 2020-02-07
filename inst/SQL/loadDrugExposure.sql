select ac.*, de.DRUG_EXPOSURE_ID, de.DRUG_CONCEPT_ID, de.DRUG_EXPOSURE_START_DATE,
  de.drug_exposure_end_date, de.QUANTITY, de.DAYS_SUPPLY, de.SIG, de.DRUG_SOURCE_VALUE, 
  de.ROUTE_SOURCE_VALUE, de.DOSE_UNIT_SOURCE_VALUE
from @resultDatabaseSchema.dbo.@cohortTable ac
left join @cdmDatabaseSchema.DRUG_EXPOSURE de
on ac.subject_id = de.person_id
where ac.cohort_definition_id = @cohort_definition_id

SELECT demographic.*,DATEDIFF(day, cohort_start_date,allergy_visit_end_date) as follow_up_duration
FROM (SELECT cohort_definition_id, person_id, gender_concept_id,age, weight, height, cast( cast(weight as float)/((cast(height as float)/100)*(cast(height as float)/100)) as float) as BMI
      FROM ( SELECT cohort_definition_id, ROW_NUMBER() OVER (PARTITION BY v.cohort_definition_id, v.subject_id ORDER BY abs(DATEDIFF(day, cohort_start_date, measurement_date)/365)) as row_num, v.SUBJECT_ID as person_id, gender_concept_id, YEAR(cohort_start_date)-year_of_birth as age, DATEDIFF(day, cohort_start_date, measurement_date)/365 as measuretime, weight as weight, height as height
             FROM (SELECT * FROM @resultDatabaseSchema.@cohortTable)v
      JOIN @cdmDatabaseSchema.PERSON ON v.SUBJECT_ID = person.person_id
      LEFT JOIN (SELECT height.person_id,height.value_as_number as height,weight.value_as_number as weight,height.MEASUREMENT_date as measurement_date
                 FROM (SELECT * FROM @cdmDatabaseSchema.measurement WHERE measurement_concept_id = 3023540) height 
                 INNER JOIN (SELECT * FROM @cdmDatabaseSchema.measurement WHERE measurement_concept_id = 3025315) weight
                 ON height.person_id = weight.person_id and (height.MEASUREMENT_date = weight.MEASUREMENT_date OR height.MEASUREMENT_datetime = weight.MEASUREMENT_datetime)
		             WHERE height.value_as_number !=0 and weight.value_as_number != 0)c ON person.person_id = c.person_id) k
      WHERE row_num = 1 and age >=12 ) demographic
LEFT JOIN (SELECT SUBJECT_ID,b.cohort_start_date, max(visit_end_date) as allergy_visit_end_date
           FROM (SELECT asthma_cohort.* , visit_end_date
                 FROM (select subject_id, min(cohort_start_date) as cohort_start_date FROM @resultDatabaseSchema.@cohortTable GROUP BY SUBJECT_ID)asthma_cohort
                 JOIN @cdmDatabaseSchema.VISIT_OCCURRENCE
                 ON asthma_cohort.SUBJECT_ID = visit_occurrence.person_id WHERE care_site_id = 899 and visit_end_date < '2020-01-01')b 
           GROUP BY SUBJECT_ID,b.cohort_start_date)visit
ON demographic.person_id = visit.SUBJECT_ID
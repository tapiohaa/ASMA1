
/*
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###         SAS script 0_visits_diagnoses.sas     ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###           Funded Primary Care: RDD            ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Read and mutate data on ICD-10 and SPAT codes to obtain health checks.

Inputs: tutkpalv_u1418_tmp tutkpalv_u1418_tmp_2 tutkpalv_u1418_icd tutkpalv_u1418_icd_2
Output: visits_diagnoses

Libnames: */

libname hilmo "D:\d66\external";
libname interm "W:\ASMA1\data\intermediate";

/*###
###*/


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Read data on contacts that included diagnosis codes. ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

data icd_codes;
	set hilmo.tutkpalv_u1418_tmp hilmo.tutkpalv_u1418_tmp_2;
run;

data spat_codes;
	set hilmo.tutkpalv_u1418_icd hilmo.tutkpalv_u1418_icd_2;
run;

proc sort data=icd_codes;
	by id;
run;

proc sort data=spat_codes;
	by id;
run;


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Use SPAT codes to extract health checks. ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

/* SPAT:

School and student healthcare, health checks: SPAT1363, SPAT1364, SPAT1365, SPAT1366, SPAT1367
Health checks for military call ups: SPAT1321 */

data spat_codes1;
	set spat_codes;
	
	* Take only the health checks listed above;
	where toimenpide in ('SPAT1363', 'SPAT1364', 'SPAT1365', 'SPAT1366', 'SPAT1367', 'SPAT1321');
	
	/* Create dummies for different kinds of health checks. */
	
	if toimenpide in ('SPAT1363', 'SPAT1364', 'SPAT1365', 'SPAT1366', 'SPAT1367') then health_check=1;
	else if toimenpide in ('SPAT1321') then army_check=1;
	
	drop jarjestys toimenpide;

	rename id=event;
	
	length health_check 3;

run;


* Aggregate (sum) diagnoses at the event level;

proc sql;
	create table spat_codes2 as
	select event, sum(health_check) as health_check, sum(army_check) as army_check
	from spat_codes1
	group by event;
quit;


/* If the event has a code for a military call up health check, we set the other dummy to zero. */

data spat_codes3;
	set spat_codes2;
	
	if army_check > 0 then 
		do;
		army_check=1;
		health_check=0;
		end;
	else if health_check > 0 then 
		do;
		health_check=1;
		army_check=0;
		end;

	* Compute row sums (should be 1);
	row_sum = health_check + army_check;

	length health_check army_check row_sum 3;
run;


proc sql;
	select max(row_sum) as max_row_sum
	from spat_codes3;
run;
quit;


/* To reduce the size of the file, we use the following code for the event:
	2 = health check for military call ups
	3 = health check in student healthcare. */

data spat_codes4;
	set spat_codes3;

	if army_check=1 then health_check_type=2;
	else if health_check=1 then health_check_type=3;

	length health_check_type 3;

	keep event health_check_type;

run;


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Use ICD-10 codes to extract health checks. ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

/* ICD-10:

School and student healthcare, health checks: Z10.8
Health checks for military call ups: Z02.3 */

data icd_codes1;
	set icd_codes;

	/* Create dummies for different types of health checks. */

	if icd10 in ('Z10.8') then health_check=1;
	if icd10 in ('Z02.3') then army_check=1;

	rename id=event;

	drop icd10 jarjestys;

	length health_check army_check 3;
run;

data icd_codes1;
	set icd_codes1;
	where not missing(health_check) or not missing(army_check);
run;


* Aggregate (sum) diagnoses at the event level;

proc sql;
	create table icd_codes2 as
	select event, sum(health_check) as health_check, sum(army_check) as army_check
	from icd_codes1
	group by event;
quit;


/* If the event has a code for a military call up health check, we set other dummies to zero.
	If the visits is not a military call up health check but a student healthcare health check, 
	we set other dummies to zero.*/

data icd_codes3;
	set icd_codes2;
	
	* Mutate the number of diagnoses to an indicator variable;
	if health_check > 0 then health_check = 1;
	else health_check = 0; 
	if army_check > 0 then army_check = 1;
	else army_check = 0;

	if army_check > 0 then health_check=0;
	else if health_check > 0 then army_check=0;

	* Compute row sums (should be 1);
	row_sum = health_check + army_check;

	length health_check army_check 3;
run;


proc sql;
	select max(row_sum) as max_row_sum
	from icd_codes3;
quit;

/* To reduce the size of the file, we use the following code for the event:
	2 = health check for military call ups
	3 = health check in student healthcare */

data icd_codes4;
	set icd_codes3;

	if army_check=1 then diagnosis_type=2;
	else if health_check=1 then diagnosis_type=3;

	length diagnosis_type 3;

	keep event diagnosis_type;

run;


* Merge ICD and SPAT codes;

proc sort data=icd_codes4;
	by event;
run;

proc sort data=spat_codes4;
	by event;
run;

data codes;
	merge icd_codes4 spat_codes4;
	by event;
run;


/* There ICD and SPAT codes aren't necessarily the same if they both exist. Let's change this: */

data interm.visits_diagnoses;
	set codes;

	if missing(diagnosis_type) then diagnosis_type = health_check_type;
	if not missing(health_check_type) and health_check_type < diagnosis_type then diagnosis_type = health_check_type;

	drop health_check_type;

run;


* End;


/*
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###            SAS script 0_visits.sas            ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###           Funded Primary Care: RDD            ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

Content: Read primary care visits from Avohilmo for 2011-2019 and produce a table 
to which we can later merge FOLK data.

Inputs: tutkpalv_u1418_ammatti + tutkpalv_u1418_ammattioikeudet +
		tutkpalv_u1418_avohilmo201X_s where x in (1:8) +
		thl4768_avohilmo_2019_Y_s where Y in (1:2) + visits_diagnoses
Output: Output: visits_quality_1x where x in (1:9) +
		visits_diagnoses_1x where x in (1:8) +
		visits_main_1x where x in (1:9)

Libnames: */

libname hilmo "D:\d66\external";
libname hilmon "D:\d66\external\THL_aineisto_2019_2020";
libname raw "W:\ASMA1\data\raw";
libname interm "W:\ASMA1\data\intermediate";

/*###
###*/


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Load classifications for doctors and nurses. ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

* TK ammattiluokitus 2001;
data tk_codes(drop=avo_raportointiryhma_nimi);
	set hilmo.tutkpalv_u1418_ammatti;
	where avo_raportointiryhma_koodi in ('10','30'); * Doctors and nurses;
	rename tarkin_taso_koodi = ammatti;
run;

proc sort data=tk_codes;
	by avo_raportointiryhma_koodi;
run;

* Valvira ammattioikeudet 2008;
data valv_codes(drop=avo_raportointiryhma_nimi);
	set hilmo.tutkpalv_u1418_ammattioikeudet;
	where avo_raportointiryhma_koodi in ('10','30'); * Doctors and nurses;
	rename ammattioikeus_koodi = kaynti_ammattioikeus;
run;

proc sort data=valv_codes;
	by avo_raportointiryhma_koodi;
run;

/*
TK ammattiluokitus 2001:
Doctors: '222','2221','22211','22212','22213'
Nurses: '323','3231','32311','32312','32313','32314','32315','3232'

Valvira ammattioikeudet 2008
Doctors: '000','001','002','031','032','034','701','702','717','718','720','810','811','900','901','724','910','812'
Nurses: '100','300','400','503','508','509','710','730','740','780','790','800','820','727','728','803'
*/


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Read and mutate primary care visit data on uncancelled visits. ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

* 2011-2018;

options nolabel;

%MACRO read_data;
%DO i = 1 %TO 8;

data visits_1&i;
	set hilmo.tutkpalv_u1418_avohilmo201&i._s;


	/* Not all data are read:
		1) IDs must be observed.
		2) Time stamps must be observed.
		3) birthdays must be observed.
		4) We only take i) curative outpatient care (T11),
			ii) school and student health care (T27), and
			iii) a class containing military call up health checks (T29).
		5) Of the contacts, we take those visits where the client physically
			visited professional.
		6) The contact was not cancelled. */

	where not missing(shnro) 
		and not missing(kaynti_alkoi) 
		and not missing(asiakas_syntymaaika)
		and kaynti_palvelumuoto in ('T11','T27','T29')
		and kaynti_yhteystapa = 'R10'
		and missing(peruutus_ajankohta) and missing(peruutus_syy);
	

	/* Create variable profession such that:
		-1 = other than doctors, nurses and public health nurses + missing values
		1 = doctors
		2 = nurses and public health nurses */

	ammatti = put(kaynti_ammatti, 6. -L); 

	if ammatti in ('222','2221','22211','22212','22213') or 
		kaynti_ammattioikeus in ('000','001','002','031','032','034','701','702','717','718','720','810','811','900','901','724','910','812') 
		then profession = 1;
	else if ammatti in ('323','3231','32311','32312','32313','32314','32315') or
		kaynti_ammattioikeus in ('100','300','400','503','508','509','710','730','740','780','790','800','820','727','728','803') 
		then profession = 2;
	else if missing(ammatti) and missing(kaynti_ammattioikeus) then profession = -1;
	else profession = -1;

	* Date - drop time stamp;
	date = dhms(datepart(kaynti_alkoi), 0, 0 , 0);

	* Keep only relevant variables;
	keep id shnro asiakas_syntymaaika profession kaynti_palvelumuoto date;

	* Rename columns;
	rename id=event shnro=id kaynti_palvelumuoto=service_type
		asiakas_syntymaaika=birth_date;

	length profession 3;

run;

%END;
%MEND read_data;

%read_data;


* 2019;

%MACRO read_data;
%DO i = 1 %TO 2;

data visits_19_&i;
	set hilmon.thl4768_avohilmo_2019_&i._s;

	/* Not all data are read:
		1) IDs must be observed.
		2) Time stamps must be observed.
		3) birthdays must be observed.
		4) We only take curative outpatient care (T11), but NOT,
			i) school and student health care (T27), or
			ii) a class containing military call up health checks (T29).
			Thus, we use only 2011-2018 to plot health checks.
		5) Of the contacts, we take those visits where the client physically
			visited professional.
		6) The contact was not cancelled. */

	where not missing(shnro) 
		and not missing(kaynti_alkoi) 
		and not missing(asiakas_syntymaaika)
		and kaynti_palvelumuoto in ('T11')
		and kaynti_yhteystapa = 'R10'
		and missing(peruutus_ajankohta) and missing(peruutus_syy);
	

	/* Create variable profession such that:
		-1 = other than doctors, nurses and public health nurses + missing values
		1 = doctors
		2 = nurses and public health nurses */

	ammatti = put(kaynti_ammatti, 6. -L); 

	if ammatti in ('222','2221','22211','22212','22213') or 
		kaynti_ammattioikeus in ('000','001','002','031','032','034','701','702','717','718','720','810','811','900','901','724','910','812') 
		then profession = 1;
	else if ammatti in ('323','3231','32311','32312','32313','32314','32315') or
		kaynti_ammattioikeus in ('100','300','400','503','508','509','710','730','740','780','790','800','820','727','728','803') 
		then profession = 2;
	else if missing(ammatti) and missing(kaynti_ammattioikeus) then profession = -1;
	else profession = -1;

	* Patient birth date and visit date as datetimes;
	d_birth = input(scan(asiakas_syntymaaika, 1, ' '), ??DDMMYY10.);
	d_visit = input(scan(kaynti_alkoi, 1, ' '), ??DDMMYY10.);
	birth_date = dhms(d_birth, 0, 0 , 0);
	date = dhms(d_visit, 0, 0 , 0);

	* Keep only relevant variables;
	keep kaynti_id shnro profession kaynti_palvelumuoto birth_date date;

	* Rename columns;
	rename kaynti_id=event shnro=id kaynti_palvelumuoto=service_type;

	length profession 3;

run;

%END;
%MEND read_data;

%read_data;

* Concatenate;
data visits_19;
	set visits_19_1 visits_19_2;
run;


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) Create data for quality checks. ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

/* We will assess the quality of the data from the following perspective:
	Have all health centers managed to transfer the data to the national
	register in every month during the study period? Likely not.
		
	With this objective in mind, we take all primary care GP visits from the above 
	datasets (no age restriction). These are later merged to the FOLK data and then we 
	keep only those visits where the patient had the unique Finnish ID (almost every person). 
	We drop such duplicates where the patient has more than one GP visit the same day. */

%MACRO visits_quality;
%DO i = 1 %TO 9;

* Subset the data;

data visits_quality_1&i;
	set visits_1&i;

	where service_type = 'T11'
		and profession = 1;

	* Create variables for birth year, month and day;
	birth_year = year(datepart(birth_date));
	birth_month = month(datepart(birth_date));
	birth_day = day(datepart(birth_date));
		
	keep date id birth_year birth_month birth_day;

	length birth_year birth_month birth_day 3;
run;

* Take only unique id-datetime rows;

proc sort data=visits_quality_1&i NODUPRECS;
	by id date birth_year birth_month birth_day;
run;

* Create variables for the month, day and year of the visits;

data visits_quality_1&i;
	set visits_quality_1&i;

	visits_date = datepart(date);
	day = day(visits_date);
	month = month(visits_date);
	year = year(visits_date);

	drop date visits_date;
	length month year day 3;

run;

* Save to cvs;

proc export data=visits_quality_1&i
	outfile= "W:\ASMA1\data\intermediate\visits_quality_1&i..csv"
	dbms=csv;
run;

%END;
%MEND visits_quality;

%visits_quality;


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4) Merge diagnosis codes to visit data. ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

* Read data on diagnoses codes to be merged (left join) to visits;

data diagnoses;
	set interm.visits_diagnoses;
run;


* Implement the merge;

%MACRO visits_merge;
%DO i = 1 %TO 9;

proc sql;
	create table data_1&i as
	select * from visits_1&i as x left join diagnoses as y
	on x.event = y.event;
quit;

* Create a variable for weekday, day, month and year of the visit;

data data_1&i;
	set data_1&i;
	
	visits_date = datepart(date);
	month = month(visits_date);
	year = year(visits_date);
	day = day(visits_date);
	wday = weekday(visits_date);
	
	drop visits_date event;
	length month year day wday 3;
	
run;

%END;
%MEND visits_merge;

%visits_merge;


* We do not have diagnoses for year 2019;
data data_19;
	set data_19;
	diagnosis_type = .;
run;


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 5) Create RDD data for assessing identifying assumptions. ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

/* Next, we will create two datasets (for different purposes).
	The first contains only those visits that contain one of the 
	diagnosis types we listed in 0_visits_diagnoses.sas (a health check 
	or a conscript health check). These data will be used in 
	examining identifying assumptions. The second dataset will include
	our main outcome: curative GP visits. In both cases, weekends are excluded,
	and we drop duplicates if there are more than one similar contact occurring
	at the same time. */
	

%MACRO visits_diagnoses;
%DO i = 1 %TO 8;

data visits_diagnoses_1&i;
	set data_1&i;
	
	where not missing(diagnosis_type) and wday in (2:6);

	* Count the age in full years at the time of the visit;
	age_y = intck('year', datepart(birth_date), datepart(date), 'C');

	drop wday service_type profession;
	length age_y 3;
	
run;

/* Subset again: take only those who were aged 16 to 20 years old at
	the time of the visit. */

data visits_diagnoses_1&i;
	set visits_diagnoses_1&i;

	where age_y in (16:20);
	
	* Create variables for birth year, month and day;
	birth_year = year(datepart(birth_date));
	birth_month = month(datepart(birth_date));
	birth_day = day(datepart(birth_date));

	drop birth_date age_y;
	length birth_year birth_month birth_day 3;

run;

/* We drop duplicates (same person, same diagnosis_type and same date and time). */

proc sort data=visits_diagnoses_1&i NODUPRECS;
	by _all_;
run;

* Drop date;

data visits_diagnoses_1&i;
	set visits_diagnoses_1&i;
	
	drop date;

run;

* Save;

proc export data=visits_diagnoses_1&i
	outfile= "W:\ASMA1\data\intermediate\visits_diagnoses_1&i..csv"
	dbms=csv;
run;

%END;
%MEND visits_diagnoses;

%visits_diagnoses;


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 6) Create RDD data for main analysis. ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

%MACRO visits_main;
%DO i = 1 %TO 9;

data visits_main_1&i;
	set data_1&i;
	
	* Take only curative GP visits;
	where service_type = 'T11' 
		and profession = 1
		and wday in (2:6);

	* Count the age in full years at the time of the visit;
	age_y = intck('year', datepart(birth_date), datepart(date), 'C');

	drop wday service_type profession diagnosis_type;
	length age_y 3;
	
run;

/* Subset again: take only those who were aged 16 to 20 years old at
	the time of the visit. */

data visits_main_1&i;
	set visits_main_1&i;

	where age_y in (16:20);
	
	* Create variables for birth year, month, and day;
	birth_year = year(datepart(birth_date));
	birth_month = month(datepart(birth_date));
	birth_day = day(datepart(birth_date));

	drop birth_date age_y;
	length birth_year birth_month birth_day 3;

run;

/* We drop duplicates (same person, same diagnosis_type and same date and time). */

proc sort data=visits_main_1&i NODUPRECS;
	by _all_;
run;

* Drop date;

data visits_main_1&i;
	set visits_main_1&i;
	
	drop date;

run;

* Save;

proc export data=visits_main_1&i
	outfile= "W:\ASMA1\data\intermediate\visits_main_1&i..csv"
	dbms=csv;
run;

%END;
%MEND visits_main;

%visits_main;

* End;



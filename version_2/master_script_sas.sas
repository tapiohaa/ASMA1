
/*
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###         SAS script master_script_sas.sas      ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###           Funded Primary Care: RDD            ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: This proposes an order with which to run SAS scripts. 

Note: We do not recommend running all the scripts at once as the SAS work
	library should be (manually) made empty between runs. However, if the scripts
	are run at once, the running time is approximately 180 minutes.

Libnames: */

* Folders for processed datasets;
libname raw "W:\ASMA1\data\raw";
libname interm "W:\ASMA1\data\intermediate";

* Avohilmo visit data;
libname hilmo "D:\d66\external";
libname hilmon "D:\d66\external\THL_aineisto_2019_2020";

* Data on specialized healthcare contacts;
libname hosp "D:\d66\external\THL_aineisto_2019_2020";

* FOLK modules basic, income, and family;
libname fbasic "D:\ready-made\FOLK_perus_11a";
libname fincome "D:\ready-made\CONTINUOUS\FOLK_TULO_C";
libname ffamily "D:\ready-made\FOLK_perh_11a";

libname toimtu "D:\d66\external";


/*###
###*/

* Remove comment symbols before running the scipts one at a time;

/* 

filename storage "W:\ASMA1\data";

* Mutate socioeconomic data to a proper form;
%inc storage("0_folk_data.sas");					* Approximately 35 minutes;
* Inputs: folk_20112020_tua_perus22tot_1 + folk_20112019_tua_tulo21tot_1 + folk_tulo_2020_1 +;
*		 folk_20112020_tua_perh21tot_1 + tutkpalv_u1418_toitu_2012_2018_s (.sas7bdat);
* Output: folk_data_20xx where xx in (11:20) (.csv);

* Read data on diagnoses to extract health checks and acute and mental health visits;
%inc storage("0_visits_diagnoses.sas");				* Approximately 17 minutes;
* Inputs: tutkpalv_u1418_tmp + tutkpalv_u1418_tmp_2 + tutkpalv_u1418_icd + tutkpalv_u1418_icd_2 (.sas7bdat);
* Output: visits_diagnoses (.sas7bdat);

* Read primary care data to construct the following datasets;
*	1) data for quality checks (all primary care visits);
*	2) data for assumption checks (visits containing target diagnoses;
*		codes around the 18th birthday);
*	3) data for main analysis (curative GP visits around the 18th birthday);
%inc storage("0_visits.sas");						* Approximately 111 minutes; 
* Inputs: tutkpalv_u1418_ammatti + tutkpalv_u1418_ammattioikeudet + ;
*			tutkpalv_u1418_avohilmo201X_s where x in (1:8) + visits_diagnoses (.sas7bdat) +
*			thl4768_avohilmo_2019_Y_s where Y in (1:2);
* Outputs: visits_quality_1x + visits_main_1x where x in (1:9) +
*			visits_diagnoses_1x where x in (1:8) (.csv);
		
* Read data on referrals to specialized healthcare;
%inc storage("0_referrals.sas");				* Approximately 16 minutes;
* Inputs: tutkpalv_1418_thl_hilmoX_s where X in (1:2) (.sas7bdat);
* Output: referrals_201x where x in (1:9) (.sas7bdat);

*/


* End;



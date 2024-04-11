* “Religion and Innovation” by Roland Bénabou, Davide Ticchi and Andrea Vindigni  
* American Economic Review: Papers & Proceedings, 2015, 105(5), 346-351. 
* http://dx.doi.org/10.1257/aer.p20151032

* Generate the log-file to store details of the variables used and the summary statistics.
log using "D:\aer2015btv\aerpp2015btv-progam-part1.log", replace

* PART 1: This do-file allows to generate the new variables used in the empirical work and to obtain some summary statistics.

* Use the World Value Survey Longitudinal file (waves 1980-2005) to be downloaded from http://www.worldvaluessurvey.org
* The dataset we downloaded from the WVS website (named "wvs1981_2008_v20090914_stata.dta") is not provided.
* The dataset provided with this program (named "aerpp2015btv-dataset.dta") contains only the variables used in the paper.

use "D:\aer2015btv\wvs1981_2008_v20090914_stata.dta", clear

set more off

* Keep variables

keep E220 E219 E234 E046 A189 A195 F198 E047 A029 A034 A039 F034 A006 F050 F063 F028 X001 X003 X025 X045 X047 X049 F025 S003 S002 S017 S002

* The wave is denoted by the variable S002 that ranges from 1 to 5

tab S002
tab S002, nolabel

* Generate the variable year from the variable wave S002

gen year = 1980
replace year = 1990 if S002 == 2
replace year = 1995 if S002 == 3
replace year = 2000 if S002 == 4
replace year = 2005 if S002 == 5

tab year

* RELIGIOSITY VARIABLES

* Religious person is generated from F034

tab F034
tab F034, nolabel

* Generate the variable Religious person F034rp: equal 1 if the individual is religious and 0 otherwise

gen F034rp=F034
replace F034rp=0 if F034rp==2
replace F034rp=0 if F034rp==3
replace F034rp=0 if F034rp==4

tab F034rp

* Importance of religion is A006

tab A006
tab A006, nolabel

* Generate A006m: (Re)normalization of A006 by changing the sign so that higher values correspond to being more religious

gen A006m=-A006

tab A006m

* Belief in God is F050: dummy variable with 1 denoting the individual believe in God (correct scale)

tab F050
tab F050, nolabel

* Importance of God is F063: variable from 0 to 10 (correct scale)

tab F063
tab F063, nolabel

* Church attendance is F028

tab F028
tab F028, nolabel

* Generate F028m: (Re)normalization of F028 by changing the sign so that higher values correspond to being more religious

gen F028m=-F028

tab F028m


* CONTROL VARIABLES

* The variable gender is X001: male = 1, female = 2

tab X001
tab X001, nolabel

* Generate X001m where 1 corresponds to female and 0 to men

gen X001m=X001
replace X001m=0 if X001==1
replace X001m=1 if X001==2

tab X001m

* The variable X003 is Age

tab X003

* The variable X025 is Education

tab X025
tab X025, nolabel

* Social class is X045: from 1 to 5 with higher values denoting lower classes

tab X045
tab X045, nolabel

* Generate X045m: (Re)normalization of X045 by changing the sign so that higher values correspond a higher social class

gen X045m=-X045

tab X045m

* The variable X047 is Income

tab X047
tab X047, nolabel

* X049 corresponds to the size of town; higher values correspond to larger towns

tab X049
tab X049, nolabel

* Generate X049m (size of town) where missing values of X049 denoted by 9 are dropped

gen X049m=X049
replace X049m=. if X049==9

tab X049m

* Religious denomination is F025

tab F025
tab F025, nolabel

* Country is S003

tab S003
tab S003, nolabel


* DEPENDENT VARIABLES

* (Re-)normalize the dependent variables such that at higher values correspond a more open attitude towards science and technology

* Dependent variables Table 1

* E220 is “We depend too much on science and not enough on faith” from disagree to agree, meaning that higher values correspond to "anti-innovation" attitude
tab E220
tab E220, nolabel

* Generate E220m “Too much dependence on science vs faith: disagree”
* (Re)normalization of E220 by changing the sign so that higher values correspond a more "pro-innovation" attitude.

gen E220m=-E220

tab E220m

* E219 is “Science and technology make our way of life change too fast”, from disagree to agree, meaning that higher values correspond to "anti-innovation" attitude

tab E219
tab E219, nolabel
 
* Generate E219m “Science and technology make our way of life change too fast: disagree”
* (Re)normalization of E219 by changing the sign so that higher values correspond a more "pro-innovation" attitude.

gen E219m=-E219

tab E219m

* E234 is “The world is better off, or worse off, because of science and technology”, from worse off to better off. 
* The order in the answer is fine. We call it “Science & technology make world better off: agree (E234)” 

tab E234
tab E234, nolabel

* E046 is “New ideas are better than old: agree (E046)”

tab E046
tab E046, nolabel

* Variable A189 “Schwartz: It is important to this person to think up new ideas and be creative”, from Very much like me to Not at all like me

tab A189
tab A189, nolabel

* Generate A189m “Importance of new ideas & being creative: agree”
* (Re)normalization of E219 by changing the sign so that higher values correspond a more "pro-innovation" attitude.

gen A189m=-A189

tab A189m

* Variable A195 “Schwartz: It is important to this person adventure and taking risks”, from Very much like me to Not at all like me

tab A195
tab A195, nolabel

* Generate A195m “Importance of adventure & risk taking: agree”
* (Re)normalization of A195 by changing the sign so that higher values correspond a more "pro-innovation" attitude.

gen A195m=-A195

tab A195m

* Variable F198 is Fate versus control with 1 equal to Everything is determined by fate and 10 corresponding to People shape their fate themselves. 
* We summarize it as “People shape their own fate: agree (F198)”

tab F198
tab F198, nolabel
	
* Variable E047 is Personal characteristics: Changes, worry or welcome possibility.
* The value 1 corresponds to I worry about difficulties changes may cause, 10 corresponds to I welcome possibilities that something new is beginning.
* Relabeled as “Attitude toward change: welcome possibility”

tab E047
tab E047, nolabel

* Dependent variables in Table 2

* Variable A029 “Importance of child independence”	

tab A029
tab A029, nolabel

* Variable A034 “Importance of child imagination”	

tab A034
tab A034, nolabel

* Variable A039 “Importance of child determination”	

tab A039
tab A039, nolabel


* Generate the labels for the tables

lab var E220m "Too much dependence on science vs faith: disagree (E220m)"
lab var E219m "Science & technology change life too fast: disagree (E219m)"
lab var E234 "Science & technology make world better off: agree (E234)"
lab var E046 "New ideas are better than old: agree (E046)"
lab var A189m "Importance of new ideas & being creative: agree (A189m)"
lab var A195m "Importance of adventure & risk taking: agree (A195m)"
lab var F198 "People shape their own fate: agree (F198)"
lab var E047 "Attitude toward change: welcome possibility (E047)"
lab var A029 "Importance of child independence (A029)"	
lab var A034 "Importance of child imagination (A034)"	
lab var A039 "Importance of child determination (A039)"	

lab var F034rp "Religious person"
lab var A006m "Importance of religion"
lab var F050 "Belief in God"
lab var F063 "Importance of God"
lab var F028m "Church attendance"

lab var X001m "Female"
lab var X003 "Age"
lab var X025 "Education"
lab var X045m "Social Class"
lab var X047 "Income"


* Save the dataset with a new name

save "D:\aer2015btv\aerpp2015btv-dataset.dta", replace

log close

* “Religion and Innovation” by Roland Bénabou, Davide Ticchi and Andrea Vindigni  
* American Economic Review: Papers & Proceedings, 2015, 105(5), 346-351. 
* http://dx.doi.org/10.1257/aer.p20151032

* Generate the log-file to save also the coefficients of control variables that are not reported in the tables.

log using "D:\aer2015btv\aerpp2015btv-progam-part2.log", replace


* PART 2: This do-file allows to reproduce our empirical results and to generate Tables 1 and 2. 
* The associated dataset named "aerpp2015btv-dataset.dta" is the one included in the folder.

use "D:\aer2015btv\aerpp2015btv-dataset.dta", clear

eststo clear

set more off


* TABLE 1: SCIENCE AND TECHNOLOGY, NEW VS. OLD IDEAS, CREATIVITY, RISK-TAKING, SHAPING OWN FATE, AND CHANGE
* COLUMNS 1-3

* E220m: columns 1a-1d

eststo: reg E220m F034rp X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg E220m A006m X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

* The following estimate allows to check the absence of observations for such specification
* eststo: reg E220m F050 X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg E220m F063 X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg E220m F028m X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)


* E219m: columns 2a-2d

eststo: reg E219m F034rp X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg E219m A006m X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

* The following estimate allows to check the absence of observations for such specification
* eststo: reg E219m F050 X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg E219m F063 X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg E219m F028m X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)


* E234: columns 3a-3d

eststo: reg E234 F034rp X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg E234 A006m X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

* The following estimate allows to check the absence of observations for such specification
* eststo: reg E234 F050 X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg E234 F063 X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg E234 F028m X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)


esttab using "D:\aer2015btv\Table-1-col-1-2-3.rtf", b(%9.3f) se ar2 ///
order(F034rp A006m F063 F028m) ///
drop(*X049m *F025 *S003 *year) star(* 0.10 ** 0.05 *** 0.01) label compress nobaselevels replace
eststo clear


* TABLE 1: SCIENCE AND TECHNOLOGY, NEW VS. OLD IDEAS, CREATIVITY, RISK-TAKING, SHAPING OWN FATE, AND CHANGE
* COLUMNS 4-6

* E046: columns 4a-4e

eststo: reg E046 F034rp X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg E046 A006m X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg E046 F050 X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg E046 F063 X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg E046 F028m X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)


* A189m: columns 5a-5e

eststo: reg A189m F034rp X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg A189m A006m X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg A189m F050 X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg A189m F063 X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg A189m F028m X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)


* A195m: columns 6a-6e

eststo: reg A195m F034rp X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg A195m A006m X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg A195m F050 X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg A195m F063 X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg A195m F028m X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)


esttab using "D:\aer2015btv\Table-1-col-4-5-6.rtf", b(%9.3f) se ar2 ///
order(F034rp A006m F050 F063 F028m) ///
drop(*X049m *F025 *S003 *year) star(* 0.10 ** 0.05 *** 0.01) label compress nobaselevels replace
eststo clear


* TABLE 1: SCIENCE AND TECHNOLOGY, NEW VS. OLD IDEAS, CREATIVITY, RISK-TAKING, SHAPING OWN FATE, AND CHANGE
* COLUMNS 7-8

* F198: columns 7a-7e

eststo: reg F198 F034rp X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg F198 A006m X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg F198 F050 X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg F198 F063 X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg F198 F028m X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)


* E047: The following estimate allows to check the absence of observations for such specifications

* eststo: reg E047 F034rp X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

* eststo: reg E047 A006m X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

* eststo: reg E047 F050 X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

* eststo: reg E047 F063 X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

* eststo: reg E047 F028m X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)


* E047: columns 8a-8e (control include: female, age, country, year). This is the specification reported in the paper.

eststo: reg E047 F034rp X001m X003 i.S003 i.year [w = S017], vce(r)

eststo: reg E047 A006m X001m X003 i.S003 i.year [w = S017], vce(r)

eststo: reg E047 F050 X001m X003 i.S003 i.year [w = S017], vce(r)

eststo: reg E047 F063 X001m X003 i.S003 i.year [w = S017], vce(r)

eststo: reg E047 F028m X001m X003 i.S003 i.year [w = S017], vce(r)


* E047: columns 8f-8l (control include: female, age, country, year, and religious denomination).

eststo: reg E047 F034rp X001m X003 i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg E047 A006m X001m X003 i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg E047 F050 X001m X003 i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg E047 F063 X001m X003 i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg E047 F028m X001m X003 i.F025 i.S003 i.year [w = S017], vce(r)


esttab using "D:\aer2015btv\Table-1-col-7-8.rtf", b(%9.3f) se ar2 ///
order(F034rp A006m F050 F063 F028m) ///
drop(*X049m *F025 *S003 *year) star(* 0.10 ** 0.05 *** 0.01) label compress nobaselevels replace
eststo clear


* TABLE 2 — MOST IMPORTANT QUALITIES FOR CHILDREN TO HAVE

* A029: columns 1a-1e

eststo: reg A029 F034rp X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg A029 A006m X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg A029 F050 X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg A029 F063 X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg A029 F028m X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)


* A034: columns 2a-2e

eststo: reg A034 F034rp X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg A034 A006m X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg A034 F050 X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg A034 F063 X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg A034 F028m X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)


* A039: columns 3a-3e

eststo: reg A039 F034rp X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg A039 A006m X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg A039 F050 X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg A039 F063 X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)

eststo: reg A039 F028m X001m X003 X025 X045m X047 i.X049m i.F025 i.S003 i.year [w = S017], vce(r)


esttab using "D:\aer2015btv\Table-2.rtf", b(%9.3f) se ar2 ///
order(F034rp A006m F050 F063 F028m) ///
drop(*X049m *F025 *S003 *year) star(* 0.10 ** 0.05 *** 0.01) label compress nobaselevels replace
eststo clear


log close

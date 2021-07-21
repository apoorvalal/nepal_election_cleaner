use "../local_election_results/1992 Local Election (clean)_caste.dta", clear
keep if LastName != ""
keep last_name prob* caste* vollan*
bys last_name: keep if _n == _N
export delimited using "../tmp/caste_mappings_nepal.csv", replace

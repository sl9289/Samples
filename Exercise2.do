/* Topic: Data Cleaning and Textual Analysis
   Editor: Siqi Li
*/
  
global loc "/Users/siqili/Downloads"
global nyt "$loc/NYT"

import excel "$nyt/NYT Data", sheet("Sheet1") clear
replace C = "Rachel Renee Russell" in 75
replace B = "Dork Diaries Book 10: Tales From a Not-So-perfect Pet Sitter" in 75
replace A = "." in 75
gen day = substr(A,-9,2)
gen year = substr(A,-4,.)
gen month = substr(A,-7,3)
generate month_num = month(date(month, "M"))
destring day, replace
destring year, replace
rename B Book_title
split C, p(" and ", " & ", " with ", " (author)")

gen first_name1 = "" //Generate last name and first name for author1
quietly forval i = 1/`=_N' {
	local first_space = strpos(C1[`i'], " ")
	replace first_name1 = substr(C1[`i'], 1, `first_space' - 1) in `i'
}

gen last_name1_1 = ""
quietly forval i = 1/`=_N' {
	local first_space = strpos(reverse(C1[`i']), " ")
	if `first_space' == 0 {
		replace last_name1_1 = reverse(C1[`i']) in `i'
	}
	else {
		replace last_name1_1 = substr(reverse(C1[`i']), 1, `first_space' - 1) in `i'
	}
}
gen last_name1 = reverse(last_name1_1)
drop last_name1_1

gen first_name2 = "" //Generate last name and first name for author2
quietly forval i = 1/`=_N' {
	local first_space = strpos(C2[`i'], " ")
	replace first_name2 = substr(C2[`i'], 1, `first_space' - 1) in `i'
}

gen last_name2_1 = ""
quietly forval i = 1/`=_N' {
	local first_space = strpos(reverse(C2[`i']), " ")
	replace last_name2_1 = substr(reverse(C2[`i']), 1, `first_space' - 1) in `i'
}

gen last_name2 = reverse(last_name2_1)
drop last_name2_1

split C3, p(" ") //Generate last name and first name for author3
rename C31 First_name3
rename C32 Last_name3
drop C1 C2 C3
order Book_title, after(month_num)
save "$nyt/NYT Data", replace

use "$nyt/NYT Data", clear
log using "$nyt/bestselling_tables.log"
bysort month_num year: tab Book_title //Calculate the frequency of bestselling titles by month and year
log close

ssc install txttool
txttool Book_title , gen(cleaned) //There are 200 unique words in book title
txttool Book_title , replace bagwords
sum w_* //Show the summary of word frequency of book title: high mean refers to high word frequency
di 75*0.4 //So,"the" has the most frequency of 30
di 75*0.186667 //"of" has the second mostfrequency of 14
di 75*0.146667 //"a" has the third most frequency of 11
di 75*0.066667 //"i","one" have the frequency of 5
di 75*0.053333 //"black","my","from","book" have the frequency of 4
di 75*0.04 //"american","long","in" have the frequency of 3
di 75*0.026667 //"sing","an","dear","and","dont","make","woman","story","for","to","by","dork","diaries","tables","everything","game","bo" have the frequency of 2
// Others only have the frequency of 1

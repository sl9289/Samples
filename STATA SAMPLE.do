// 1.Topic: The Value-at-Risk of Japanese Yen Exchange Rate Volatility Based on The EGARCH Model 

** test for stationary 
use "D:\lsq\nyu\time seires\paper_raw.dta",clear
tsline d_ler
dfuller ler, lag(1)
pperron ler ,regress //test stationary of ler-nonstationary
dfgls d_ler, maxlag(12)
pperron d_ler ,regress //test stationary of d_ler-stationary

** test for conditional heteroskedasticity
arima d_ler, arima(1,0,0) nolog
predict e_hat,resid 
reg e_hat
estat archlm
gen e2=e_hat*e_hat
wntestq e2

** skewness, kurtosis
sktest d_ler //not normal dis
sum d_ler, detail //skewness=-0.467; kurtosis=4.185-left fat tail

**graph
twoway(tsline ler, yaxis(1)) (tsline d_ler, yaxis(2))

** mean equation(AR1+x)
ac d_ler //(AR1)
pac d_ler 
arima d_ler, arima(1,0,0) nolog //best
estat ic
arima d_ler, arima(1,0,1) nolog
estat ic
drop time_c
save "D:\lsq\nyu\time seires\paper_exchange rate.dta"

**add interest rate: 10-year interest rate
use "D:\lsq\nyu\time seires\paper_interest rate1.dta",clear
gen time = date( var1,"YMD")
format time %tddmy
tsset time
sort us_interest time
sort jp_interest time
gen in_dif=us_interest- jp_interest
gen time_c = _n
tsset time_c
gen d_in_dif = d.in_dif
merge 1:1 time using "D:\lsq\nyu\time seires\paper_exchange rate.dta"
keep if _merge==3
drop _merge time_c
gen time_c = _n
tsset time_c
save "D:\lsq\nyu\time seires\paper_final.dta"

** select p q and error distribution of EGARCH+GARCH
arch ler,het(in_dif) arima(1,1,0) arch(1) egarch(1) nolog 
est store egarchn
arch d_ler,het(in_dif) arima(1,0,0) arch(1) egarch(1) distribution(t) //better
est store egarcht
esttab egarchn egarcht, mtitle scalar(ll aic bic)

**VAR-causality effect
use "D:\lsq\nyu\time seires\paper_final.dta",clear
replace d_in_dif=0 if d_in_dif==.
tsline in_dif
dfgls in_dif, maxlag(12) //nonstarionary
pperron in_dif ,regress //test stationary of interest rate
dfgls d_in_dif, maxlag(12) //starionary
pperron d_in_dif ,regress //test stationary of d_interest rate

**test for cointegration
vecrank ler in_dif //johansen test
reg ler in_dif
predict e_hat,resid
dfuller e_hat
ssc install egranger
egranger in_dif ler 
egranger ler in_dif //not cointegrated

** short term relationship
varsoc d_ler d_in_dif 
var d_ler d_in_dif, lag(3)
vargranger // cause-add this term in grach 

**SVAR
matrix A1 = (1,0 \ .,1)
matrix B1 = (.,0 \ 0,.)
matrix list A1
matrix list B1
svar d_in_dif d_ler, lags(1/2) aeq(A1) beq(B1) 
matrix Aest=e(A)
matrix Best=e(B)
matrix chol_var=inv(Aest)*Best
matrix list chol_var //1 unit change in the interest rate differential causes 0.87% change in the exchange rate
  
**VAR to forecast d_in
use "D:\lsq\nyu\time seires\paper_final.dta",clear
var d_ler d_in_dif, lag(1/2)
varfcast compute, dynamic(398) step(10)
keep time time_c d_in_dif_f
rename d_in_dif_f d_in_dif
keep if time==.
save "D:\lsq\nyu\time seires\paper_var.dta"

use "D:\lsq\nyu\time seires\paper_final.dta",clear
append using "D:\lsq\nyu\time seires\paper_var.dta",force
save "D:\lsq\nyu\time seires\paper_finalver.dta"

**predict(arch d_ler, arima(1,0,0) arch(1) egarch(1) )
use "D:\lsq\nyu\time seires\paper_finalver.dta", clear
arch d_ler, arima(1,0,0) arch(1) egarch(1) 
predict ht2, variance dynamic(398) //ht

**model check
arch d_ler,het(in_dif) arima(1,0,0) arch(1) egarch(1) distribution(t)
predict new, y dynamic(410)

**calculate VaR 
gen VaR_up2=1.65*sqrt(ht2)
gen VaR_down2=-1.65*sqrt(ht2)
tsline VaR_up VaR_down d_ler f_d_ler, xline(300)

**forecast
forecast create garch
estimates store fd_ler
forecast estimates fd_ler
forecast solve, begin(398) prefix(_f)
gen fd_ler=_fd_ler if time_c>=300

// 2.Topic: Raw survey data cleaning

**check & revise student ID numbers
use "D:\lsq\CCAP_CAU_student\raw\student.dta",clear
replace c13_c=c13
replace c13_c="a" if c13=="密"
replace c13_c="." if c13!=""&c13!="1"&c13!="2"&c13!="a"&c13!="b"
replace id = subinstr(id, "A"，"", .)
replace id = subinstr(id, "B"，"", .)
replace id = subinstr(id, "大"，"", .)
replace id = subinstr(id, "小"，"", .)
replace id="21064172" if id="210641100"
drop if id=="34514106"
replace id="34413205" if id=="344132"
replace id="34413249" if id=="344132049"
replace id="34613137" if id=="346131"
replace id="35313115" if id=="353131315"
replace id="41213127" if id=="412131027"
replace id="35114119" if id=="351141"
replace id="35114124" if id=="351141024"
replace id="35114141" if id=="351141042"
replace id="17023225" if name=="刘玉嘉"
replace id="17023210" if name=="边晓颖"
replace id="17023202" if name=="周婷珠"
replace id="17023241" if name=="刘雨娜"
replace id="17023213" if name=="耿雅琪"
replace id="17023224" if name=="骆诚"&id=="无编码-006"
replace id="17023208" if name=="张诗玉"
replace id="17023236" if name=="张耀天"
replace id="17023201" if name=="董轩"
replace id="17023207" if name=="李世洁"
replace id="17023219" if name=="王婷婷"&id=="无编码-011"
replace id="17023222" if name=="胡宁"
replace id="17023242" if name=="孙琪"
replace id="17023232" if name=="边晓军"
replace id="17023218" if name=="白添函"
replace id="17023203" if name=="李宇航"&id=="无编码-016"
replace id="17023209" if name=="王宇涵"&id=="无编码-017"
replace id="17023238" if name=="王怡琳"
replace id="17023217" if name=="刘玉成"
replace id="17023233" if name=="孟令华"
replace id="17023206" if name=="孔健杰"
replace id="17023221" if name=="张文文"
replace id="17023220" if name=="何欣悦"
replace id="17023215" if name=="周子浩"
replace id="17023227" if name=="刘旭"&id=="无编码-025"
replace id="17023205" if name=="刘浩然"&id=="无编码-026"
replace id="17023231" if name=="崔俊浩"
replace id="17023230" if name=="张国傲"
replace id="17023212" if name=="贾艺森"
replace id="17023229" if name=="张宇峰"&id=="无编码-030"
replace id="17023234" if name=="刘海川"
replace id="17023228" if name=="韩秋雨"
replace id="34513154" if name=="贾亚辉(IQ)"
replace id="34513155" if name=="纪永??"
drop if id=="无编码-001"
drop if id=="无编码-002"
drop if id=="无编码-003"
destring id,replace force
duplicates list id
drop if id==17023202& grade==""
drop if id==17023203& grade==""
drop if id==17023205& grade==""
drop if id==17023208& grade==""
drop if id==17023210& grade==""
drop if id==17023212& grade==""
drop if id==17023213& grade==""
drop if id==17023215& grade==""
drop if id==17023217& grade==""
drop if id==17023219& grade==""
drop if id==17023220& grade==""
drop if id==17023222& grade==""
drop if id==17023225& grade==""
drop if id==17023227& grade==""
drop if id==17023230& grade==""
drop if id==17023231& grade==""
drop if id==17023232& grade==""
drop if id==17023233& grade==""
drop if id==17023236& grade==""
replace id=21073546 if name=="乔志琪"
replace id=35114118 if name=="朱秀秀"
replace id=35413116 if name=="周毅然"
drop if id==.
save "D:\lsq\CCAP_CAU_student\dtadta\studentscore.dta", replace

use "D:\lsq\CCAP_CAU_student\raw\student2018_v2.dta",clear
replace id = subinstr(id, "A"，"", .)
replace id = subinstr(id, "B"，"", .)
replace id = subinstr(id, "大"，"", .)
replace id = subinstr(id, "小"，"", .)
replace id="21064172" if id="210641100"
drop if id=="34514106"
replace id="34413205" if id=="344132"
replace id="34413249" if id=="344132049"
replace id="34613137" if id=="346131"
replace id="35313115" if id=="353131315"
replace id="41213127" if id=="412131027"
replace id="35114119" if id=="351141"
replace id="35114101" if id=="35114101"
replace id="35114102" if id=="35114102"
replace id="35114124" if id=="351141024"
replace id="35114141" if id=="351141042"
replace id="17023225" if name=="刘玉嘉"
replace id="17023210" if name=="边晓颖"
replace id="17023202" if name=="周婷珠"
replace id="17023241" if name=="刘雨娜"
replace id="17023213" if name=="耿雅琪"
replace id="17023224" if name=="骆诚"&id=="无编码-006"
replace id="17023208" if name=="张诗玉"
replace id="17023236" if name=="张耀天"
replace id="17023201" if name=="董轩"
replace id="17023207" if name=="李世洁"
replace id="17023219" if name=="王婷婷"&id=="无编码-011"
replace id="17023222" if name=="胡宁"
replace id="17023242" if name=="孙琪"
replace id="17023232" if name=="边晓军"
replace id="17023218" if name=="白添函"
replace id="17023203" if name=="李宇航"&id=="无编码-016"
replace id="17023209" if name=="王宇涵"&id=="无编码-017"
replace id="17023238" if name=="王怡琳"
replace id="17023217" if name=="刘玉成"
replace id="17023233" if name=="孟令华"
replace id="17023206" if name=="孔健杰"
replace id="17023221" if name=="张文文"
replace id="17023220" if name=="何欣悦"
replace id="17023215" if name=="周子浩"
replace id="17023227" if name=="刘旭"&id=="无编码-025"
replace id="17023205" if name=="刘浩然"&id=="无编码-026"
replace id="17023231" if name=="崔俊浩"
replace id="17023230" if name=="张国傲"
replace id="17023212" if name=="贾艺森"
replace id="17023229" if name=="张宇峰"&id=="无编码-030"
replace id="17023234" if name=="刘海川"
replace id="17023228" if name=="韩秋雨"
replace id="34513154" if name=="贾亚辉(IQ)"
replace id="34513155" if name=="纪永??"
drop if id=="无编码-001"
drop if id=="无编码-002"
drop if id=="无编码-003"
destring id,replace force
duplicates list id 
drop if id==17023202& height==""
drop if id==17023203& height==""
drop if id==17023205& height==""
drop if id==17023208& height==""
drop if id==17023210& height==""
drop if id==17023212& height==""
drop if id==17023213& height==""
drop if id==17023215& height==""
drop if id==17023217& height==""
drop if id==17023219& height==""
drop if id==17023220& height==""
drop if id==17023222& height==""
drop if id==17023225& height==""
drop if id==17023227& height==""
drop if id==17023230& height==""
drop if id==17023231& height==""
drop if id==17023232& height==""
drop if id==17023233& height==""
drop if id==17023236& height==""
replace id=21073546 if name=="乔志琪"
replace id=35114118 if name=="朱秀秀"
replace id=35413116 if name=="周毅然"
drop if id==.
merge 1:1 id using "D:\lsq\CCAP_CAU_student\dta\studentscore-1.dta",force
drop _merge
save "D:\lsq\CCAP_CAU_student\dta\student-2.dta",replace

** calculate students' TMISS scores 
use "D:\lsq\CCAP_CAU_student\dta\studentscore.dta",clear
keep if filegrade==6 //calculate scores for 6th graders
g chinesescore=0
foreach x of varlist c1_c c2_c c3_c c4_c c5_c c6_c c7_c c8_c c9_c c24_c c25_c c26_c c27_c c41_c{
replace  chinesescore=chinesescore+1 if `x'==1
}
replace  chinesescore=chinesescore+1 if c40_c=="2"
replace  chinesescore=chinesescore+1 if c42_c=="4"
replace  chinesescore=chinesescore+1 if c43_c=="3"
replace  chinesescore=chinesescore+1 if c44_c=="6"
replace  chinesescore=chinesescore+1 if c45_c=="5"
foreach x of varlist c11_c c12_c c13_c c14_c c29_c c32_c c35_c c38_c c47_c {
replace  chinesescore=chinesescore+1 if `x'=="a"
}
foreach x of varlist c10_c c18_c c19_c c21_c c22_c c31_c c36_c c37_c c39_c c48_c{
replace  chinesescore=chinesescore+1 if `x'=="b"
}
foreach x of varlist c15_c c17_c c20_c c23_c c28_c c30_c c33_c c34_c {
replace  chinesescore=chinesescore+1 if `x'=="c"
}
foreach x of varlist c16_c c46_c {
replace  chinesescore=chinesescore+1 if `x'=="d"
}

g mathscore=0
foreach x of varlist m12_c m13_c m26_c {
replace  mathscore=mathscore+1 if `x'=="a"
}
foreach x of varlist m1_c m3_c m4_c m6_c m9_c m14_c m15_c m20_c m21_c m22_c m28_c m29_c {
replace  mathscore=mathscore+1 if `x'=="b"
}
foreach x of varlist m2_c m7_c m8_c m10_c m16_c m18_c m23_c m24_c m25_c m27_c {
replace  mathscore=mathscore+1 if `x'=="c"
}
foreach x of varlist m11_c m17_c m19_c m30_c {
replace  mathscore=mathscore+1 if `x'=="d"
}
replace  mathscore=mathscore+1 if m5_c=="e"
save "D:\lsq\CCAP_CAU_student\调研数据append\调研数据append\dta\studentscore6.dta",replace

use "D:\lsq\CCAP_CAU_student\调研数据append\调研数据append\dta\studentscore.dta",clear
drop if filegrade==6 //calculate scores for 5th graders
g chinesescore=0
foreach x of varlist c1_c c2_c c3_c c4_c c5_c c6_c c7_c c8_c c9_c c10_c c12_c c13_c c27_c c28_c c29_c c30_c c31_c c45_c {
replace  chinesescore=chinesescore+1 if `x'==1
}
replace  chinesescore=chinesescore+1 if c44_c=="2"
replace  chinesescore=chinesescore+1 if c46_c=="4"
replace  chinesescore=chinesescore+1 if c47_c=="3"
replace  chinesescore=chinesescore+1 if c48_c=="6"
replace  chinesescore=chinesescore+1 if c49_c=="5"
foreach x of varlist c16_c c22_c c23_c c34_c c36_c c40_c {
replace  chinesescore=chinesescore+1 if `x'=="a"
}
foreach x of varlist c14_c c15_c c17_c c19_c c20_c c24_c c26_c c32_c c35_c {
replace  chinesescore=chinesescore+1 if `x'=="b"
}
foreach x of varlist c18_c c21_c c33_c c38_c c39_c c41_c {
replace  chinesescore=chinesescore+1 if `x'=="c"
}
foreach x of varlist c25_c c37_c c42_c {
replace  chinesescore=chinesescore+1 if `x'=="d"
}

g mathscore=0
foreach x of varlist m2_c m11_c m15_c m28_c {
replace  mathscore=mathscore+1 if `x'=="a"
}
foreach x of varlist m1_c m3_c m4_c m5_c m6_c m9_c m10_c m12_c m13_c m14_c m15_c m19_c m21_c m22_c m26_c {
replace  mathscore=mathscore+1 if `x'=="b"
}
foreach x of varlist m8_c m16_c m18_c m20_c m23_c m24_c m25_c m27_c m29_c m30_c {
replace  mathscore=mathscore+1 if `x'=="c"
}
foreach x of varlist m7_c m9_c m17_c {
replace  mathscore=mathscore+1 if `x'=="d"
}
replace  mathscore=mathscore+1 if m18_c=="e"
append using "D:\lsq\CCAP_CAU_student\dta\studentscore6.dta" //append scores of two grades

**calculate the Big Five personalities scores
forval i=63(1)76 {
g s`i'_c1=.
}
foreach x of varlist s63_c s64_c s65_c s66_c s67_c s68_c s69_c s70_c s71_c s72_c s73_c s74_c s75_c s76_c {
replace `x'1=0 if `x'==.
replace `x'1=1 if `x'==1
replace `x'1=2 if `x'==2
replace `x'1=3 if `x'==3
replace `x'1=4 if `x'==4
replace `x'1=5 if `x'==5
}
g esteem1=s63_c1+s64_c1+s65_c1+s66_c1+s67_c1+s68_c1+s69_c1+s70_c1+s71_c1+s72_c1+s73_c1+s74_c1+s75_c1+s76_c1

g s84_c1=.
g s86_c1=.
g s89_c1=.
g s90_c1=.
g s83_c1=s83_c
g s85_c1=s85_c
g s87_c1=s87_c
g s88_c1=s88_c
foreach x of varlist s83_c s84_c s85_c s86_c s87_c s88_c s89_c s90_c {
replace `x'1=0 if `x'==. | x'==6
replace `x'1=1 if `x'==5
replace `x'1=2 if `x'==4
replace `x'1=3 if `x'==3
replace `x'1=4 if `x'==2
replace `x'1=5 if `x'==1
}
g grit1=(s83_c1+s84_c1+s85_c1+s86_c1+s87_c1+s88_c1+s89_c1+s90_c1)/8
replace grit1=. if grit1==0
 
forval i=91(1)110 {
g s`i'_c1=.
}
foreach x of varlist s91_c s92_c s93_c s94_c s95_c s96_c s97_c s98_c s99_c s100_c s101_c s102_c s103_c s104_c s105_c s106_c s107_c s108_c s109_c s110_c s111_c {
replace `x'1=0 if `x'==1 | `x'=="1"
replace `x'1=1 if `x'==2 | `x'=="2"
replace `x'1=2 if `x'==3 | `x'=="3"
replace `x'1=3 if `x'==4 | `x'=="4"
}
g dep1=s91_c1+s92_c1+s93_c1+s94_c1+s95_c1+s96_c1+s97_c1+s98_c1+s99_c1+s100_c1+s101_c1+s102_c1+s103_c1+s104_c1+s105_c1+s106_c1+s107_c1+s108_c1+s109_c1+s110_c1
g depress1=.
replace depress1=0 if dep1<=20
replace depress1=1 if dep1>20

g s122_c1=.
g s124_c1=.
g s127_c1=.
g s128_c1=.
g s121_c1=s121_c
g s123_c1=s123_c
g s125_c1=s125_c
g s126_c1=s126_c
foreach x of varlist s121_c s122c s123_c s124_c s125_c s126_c s127_c s128_c {
replace `x'1=0 if `x'==. 
replace `x'1=1 if `x'==5
replace `x'1=2 if `x'==4
replace `x'1=3 if `x'==3
replace `x'1=4 if `x'==2
replace `x'1=5 if `x'==1
}
replace grit1=(s121_c1+s122_c1+s123_c1+s124_c1+s125_c1+s126_c1+s127_c1+s128_c1)/8 if grit1==.
replace grit1=. if grit1==0

g s111_c1=6-s111_c
g s116_c1=s116_c
g sde1=(s111_c1+s116_c1)/2
g s117_c1=6-s117_c
g s112_c1=s112_c
g sdn1=(s112_c1+s117_c1)/2
g s113_c1=6-s113_c
g s118_c1=s118_c
g sdo1=(s113_c1+s118_c1)/2
g s114_c1=6-s114_c
g s119_c1=s119_c
g sdg1=(s114_c1+s119_c1)/2
g s115_c1=6-s115_c
g s120_c1=s120_c
g sdv1=(s115_c1+s120_c1)/2
keep id province filegrade county town school grade class mainteacher mathscore chinesescore esteem1 grit1 depress1 dep1 sde1 sdn1 sdo1 sdg1 sdv1
save "D:\lsq\CCAP_CAU_student\dta\studentscore-1.dta",replace

**clean student tables in 2017 and 2018
use "D:\lsq\CCAP_CAU_student\raw\student-1(2017).dta", clear
rename s1 g1 
decode s1_c,gen(s1_c1)
replace s1_c1="1" if s1_c1=="男"
replace s1_c1="2" if s1_c1=="女"
drop s1_c
rename s1_c1 s1_c
order s1_c,after(g1)
rename s1_c g1_c 
destring g1_c,replace
recast float g1_c
rename s4 g2 
rename s4_c g2_c //Chinese zodiac
rename s7 g3 
rename s7_c g3_c //boarding student or not
rename s8 g4 
rename s8_c g4_c //attended preschool or not
rename s10 g5
rename s10_cc g5_c //attended kindergarten or not
rename s13a g6a
rename s13b g6b
rename s13c g6c
rename s14 g7
rename s14_cc g7_c
rename s15 g8
rename s15_cc g8_c
rename s16 g9
rename s16_cc g9_c
rename s17 g10
rename s17_cc g10_c
rename s24 g11
rename s24_cc g11_c
rename s25 g12
rename s25_cc g12_c
rename s26 g13
rename s26_cc g13_c
rename s27 g14
rename s27_cc g14_c
rename s31 g15
rename s31_cc g15_c
rename s32 g16
rename s32_cc g16_c
rename s33 g17
rename s33_cc g17_c
rename s34 g18
rename s34_cc g18_c
rename s35 g19
rename s35_cc g19_c
rename s36 g20
rename s36_cc g20_c
rename s37 g21
rename s37_cc g21_c
rename s50 g22
rename s50_c1c g22_c
rename s51 g23
rename s51_cc g23_c
rename s54 g24a
rename s55 g24
rename s56 g25
rename s56a g26
rename s56a_cc g26_c
rename s56aa g26a
rename s56aa_c g26a_c
rename s57 g27
rename s57_cc g27_c
rename s58 g28
rename s58_cc g28_c
rename s59 g29
rename s59_cc g29_c
rename s60 g30
rename s60_cc g30_c
rename s61 g31
rename s65 g32
rename s66 g33
rename prid g33_c
rename s72 g34
rename s72_cc g34_c
rename s73 g35
rename s73_cc g35_c
rename s74 g36
rename s74_c g36_c
rename s75 g37
rename s75_cc g37_c
rename s18 g38
rename s18_cc g38_c
rename s19 g39
rename s19_cc g39_c

rename s20 g40
rename s20_cc g40_c
rename s21 g41
rename s21_cc g41_c
rename s22 g42
rename s22_cc g42_c
rename s23 g43
rename s23_cc g43_c
rename s77 g44
rename s77_cc g44_c
rename s78 g45
rename s78_cc g45_c
rename s79 g46
rename s79_cc g46_c
rename s80 g47
rename s80_cc g47_c
rename s81 g48
rename s81_cc g48_c
rename s82 g49
rename s82_cc g49_c

gen chil_n= s45_cc+ s46_cc+ s48_cc+ s49_cc //add a new variable to count the number of children
rename xdjm g50_c

gen g51_c=1 if g50_c==0 //add a new variable to mark if this child is the only child
replace g51_c=0 if g50_c==1|g50_c==2|g50_c==3|g50_c==4|g50_c==5|g50_c==6|g50_c==7
label var g51_c "是否是独生子女"

gen g7_c2=1 if g7_c=="7" //add a new variable to count the number of family members
forval i=0(1)6 {
replace g7_c2=`i'+1 if g50_c=="`i'" & g7_c2==.
}
replace g7_c2=2 if g7_c=="1"|g7_c=="2"|g7_c=="3"|g7_c=="4"|g7_c=="5"|g7_c=="6"
replace g7_c2=3 if g7_c=="1,3"|g7_c=="1,4"|g7_c=="1,6"|g7_c=="1,7"|g7_c=="2,1"|g7_c=="2,3"|g7_c=="2,4"|g7_c=="2,5"|g7_c=="2,6"|g7_c=="2,7"|g7_c=="3,2"|g7_c=="3,4"|g7_c=="3,6"|g7_c=="3,7"|g7_c=="4,1"|g7_c=="4,2"|g7_c=="4,3"|g7_c=="4,5"|g7_c=="4,7"|g7_c=="5,6"|g7_c=="6,3"|g7_c=="6,5"|g7_c=="7,4"
replace g7_c2=4 if g7_c=="1,3,4"|g7_c=="1,3,5"|g7_c=="1,4,2"|g7_c=="1,4,5"|g7_c=="1,4,7"|g7_c=="2,1,4"|g7_c=="2,3,1"|g7_c=="2,3,4"|g7_c=="2,3,5"|g7_c=="2,4,1"|g7_c=="2,4,3"|g7_c=="2,4,6"|g7_c=="2,4,7"|g7_c=="2,5,6"|g7_c=="2,6,5"|g7_c=="2,6,7"|g7_c=="3,1,4"|g7_c=="3,4,1"|g7_c=="3,4,2"|g7_c=="3,4,7"|g7_c=="4,2,1"|g7_c=="4,2,7"|g7_c=="4,3,1"|g7_c=="4,3,7"|g7_c=="4,5,7"|g7_c=="5,6,2"|g7_c=="5,6,7"|g7_c=="6,5,2"|g7_c=="7,3,4"
replace g7_c2=5 if g7_c=="1,3,4,7"|g7_c=="2,3,4,5"|g7_c=="2,3,4,6"|g7_c=="2,3,4,7"|g7_c=="2,5,6,7"|g7_c=="3,1,4,2"|g7_c=="3,4,5,2"|g7_c=="3,4,5,6"|g7_c=="4,2,1,3"|g7_c=="6,5,4,3"
replace g7_c2=6 if g7_c=="2,3,4,5,1"|g7_c=="2,3,4,5,6"
replace g7_c2=7 if g7_c=="4,2,3,5,6,7"
keep id pro name height weight county town grade mainteacher mathscore esteem1 grit1 dep1 depress1 sde1 sdn1 sdo1 sdg1 sdv1 g1 g1_c g2 g2_c g3 g3_c g4 g4_c g5 g5_c g6a g6b g6c g7 g7_c g7_c2 g8 g8_c g9 g9_c g10 g10_c g11 g11_c g12 g12_c g13 g13_c g14 g14_c g15 g15_c g16 g16_c g17 g17_c g18 g18_c g19 g19_c g20 g20_c g21 g21_c g22 g22_c g23 g23_c g24 g24a g25 g26 g26_c g26a g26a_c g27 g27_c g28 g28_c g29 g29_c g30 g30_c g31 g32 g33 g33_c g34 g34_c g35 g35_c g36 g36_c g37 g37_c g38 g38_c g39 g39_c g40 g40_c g41 g41_c g42 g42_c g43 g43_c g44 g44_c g45 g45_c g46 g46_c g47 g47_c g48 g48_c g49 g49_c g50_c g51_c  
g year=2017
save "D:\lsq\CCAP_CAU_student\dtanew\student1.dta",replace

use "D:\lsq\CCAP_CAU_student\dta\student-2.dta", clear
drop height
destring height_c, gen(height) force

drop if grade=="1"|grade=="生" //check grade
replace grade="4" if grade=="4 "
destring grade,replace

drop if s1=="1,2"|s1=="3"|s1=="??" //check gender
destring s1,replace
rename s1 g1
destring s1_c,replace
rename s1_c g1_c
recast float g1_c

drop s3_c //check age
forval i=10(1)16 {
replace s3_c1=`i' if s2_c=="`i'" 
}
gen s3_c1="10" if s2_c=="10.5"|s2_c=="10.7"|s2_c=="10~11"|s2_c=="10周"|s2_c=="10周岁"|s2_c=="10岁半"
replace s3_c1="11" s2_c=="11+1"|s2_c=="11.5"|s2_c=="11半"|s2_c=="11周"|s2_c=="11岁8个月"|s2_c=="11周岁"|s2_c=="11岁半"|s2_c=="快11"
replace s3_c1="12" s2_c=="12.5"|s2_c=="12周"|s2_c=="12周岁"|s2_c=="虚13"
replace s3_c1="13" s2_c=="13.5"|s2_c=="13岁半"|s2_c=="快13岁了"
destring s3_c1,replace
gen s3_c=2018-s3_c1
recast int s3_c
order s3_c, after(s3)
rename s3 g2 
rename s3_c g2_c 

forval i=7(1)9{ // check boarding student, preschool attendance and kindergarten attendance
drop s`i'_c 
gen s`i'_c="1" if s`i'=="1"
replace s`i'_c="2" if s`i'=="2"
destring s`i'_c,replace
recast int s`i'_c
order s`i'_c, after(s`i')
}
rename s7 g3 
rename s7_c g3_c 
rename s8 g4 
rename s8_c g4_c 
rename s9 g5
rename s9_c g5_c

rename s6aa_c g6a //check the number of families
rename s6ab_c g6b
rename s6ac_c g6c
rename s10 g7
rename s10_c g7_c
gen g7_c2=2 if g7_c=="1" | g7_c=="2"| g7_c=="3"| g7_c=="4"| g7_c=="5"| g7_c=="6"| g7_c=="7" 
replace g7_c2=3 if g7_c=="1,2" | g7_c=="1,3"| g7_c=="1,4"| g7_c=="1,5"| g7_c=="1,6"| g7_c=="1,7"| g7_c=="2,1"| g7_c=="2,3"| g7_c=="2,4"| g7_c=="2,5"| g7_c=="2,6"| g7_c=="2,7"| g7_c=="3,1"| g7_c=="3,2"| g7_c=="3,4"| g7_c=="3,5"| g7_c=="3,6"| g7_c=="3,7"| g7_c=="4,1"| g7_c=="4,2"| g7_c=="4,3"| g7_c=="4,5"| g7_c=="4,6"| g7_c=="4,7"| g7_c=="4,8"| g7_c=="5,2"| g7_c=="5,6"| g7_c=="6,1"| g7_c=="6,2"| g7_c=="6,5"| g7_c=="6,7"| g7_c=="7,4"
replace g7_c2=4 if g7_c=="1,2,3" | g7_c=="1,2,4"| g7_c=="1,2,5"| g7_c=="1,2,6"| g7_c=="1,2,7"| g7_c=="1,2,8"| g7_c=="1,3,2"| g7_c=="1,3,4"| g7_c=="1,3,5"| g7_c=="1,3,7"| g7_c=="1,4,2"| g7_c=="1,4,3"| g7_c=="1,4,5"| g7_c=="1,4,6"| g7_c=="1,4,7"| g7_c=="1,5,6"| g7_c=="2,1,3"| g7_c=="2,1,4"| g7_c=="2,1,7"| g7_c=="2,3,1"| g7_c=="2,3,4"| g7_c=="2,1,7"| g7_c=="2,3,1"| g7_c=="2,3,4"| g7_c=="2,3,5"| g7_c=="2,3,6"| g7_c=="2,3,7"| g7_c=="2,4,3"| g7_c=="2,4,6"| g7_c=="2,4,7"| g7_c=="2,5,6"| g7_c=="2,6,3"| g7_c=="2,6,4"| g7_c=="2,6,5"| g7_c=="2,6,7"| g7_c=="2,7,1"| g7_c=="3,2,4"| g7_c=="3,4,1"| g7_c=="3,4,2"| g7_c=="3,4,5"| g7_c=="3,4,6"| g7_c=="3,4,7"| g7_c=="3,5,6"| g7_c=="3,4,6"| g7_c=="3,4,7"| g7_c=="4,1,2"| g7_c=="4,1,3"| g7_c=="4,1,7"| g7_c=="4,2,1"| g7_c=="4,2,3"| g7_c=="4,3,1"| g7_c=="4,3,7"| g7_c=="4,4,2"| g7_c=="4,5,6"| g7_c=="4,6,2"| g7_c=="4,6,5"| g7_c=="4,7,3"| g7_c=="5,1,2"| g7_c=="5,6,2"| g7_c=="5,6,4"| g7_c=="5,6,7"| g7_c=="6,3,5"| g7_c=="7,1,2"| g7_c=="7,3,4"
replace g7_c2=5 if g7_c=="1,2,3,4"| g7_c=="1,2,3,5"| g7_c=="1,2,3,6"| g7_c=="1,2,3,7"| g7_c=="1,2,4,3"| g7_c=="1,2,4,5"| g7_c=="1,2,4,6"| g7_c=="1,2,4,7"| g7_c=="1,2,5,3"| g7_c=="1,2,5,6"| g7_c=="1,2,5,7"| g7_c=="1,2,6,4"| g7_c=="1,2,6,5"| g7_c=="1,2,6,7"| g7_c=="1,3,2,4"| g7_c=="1,3,4,7"| g7_c=="1,3,7,4"| g7_c=="1,4,2,3"| g7_c=="1,4,2,7"| g7_c=="1,4,3,7"| g7_c=="1,5,6,7"| g7_c=="1,6,4,2"| g7_c=="2,1,3,4"| g7_c=="2,1,4,3"| g7_c=="2,3,4,1"| g7_c=="2,3,4,6"| g7_c=="2,3,4,7"| g7_c=="2,4,1,3"| g7_c=="2,5,6,1"| g7_c=="2,5,6,7"| g7_c=="3,1,2,4"| g7_c=="3,1,4,2"| g7_c=="3,4,5,6"| g7_c=="4,1,2,3"| g7_c=="4,3,1,2"| g7_c=="4,3,6,7"| g7_c=="5,6,1,2"
replace g7_c2=6 if g7_c=="1,2,3,4,5" | g7_c=="1,2,3,4,6"| g7_c=="1,2,3,4,7"| g7_c=="1,2,3,5,6"| g7_c=="1,2,4,5,7"| g7_c=="1,2,4,5,6"| g7_c=="1,2,4,6,7"| g7_c=="1,2,4,7,3"| g7_c=="1,2,5,6,4"| g7_c=="1,2,5,6,7"| g7_c=="1,2,6,4,3"| g7_c=="1,3,4,5,6"| g7_c=="2,3,4,5,6"| g7_c=="2,4,3,5,6"| g7_c=="4,6,1,2,7"| g7_c=="7,1,2,3,4"
replace g7_c2=7 if g7_c=="1,2,3,4,5,6" | g7_c=="1,2,3,4,5,7"| g7_c=="1,2,3,4,6,7" | g7_c=="1,2,3,7,5,6"| g7_c=="1,2,5,6,5,7"| g7_c=="1,2,6,5,3,4"| g7_c=="1,4,5,6,3,2"
replace g7_c2=8 if g7_c=="1,2,3,4,5,6,7"  

replace s11_c="1" if s11_c=="1,2"|s11_c=="1,2,3,4"|s11_c=="4,3,1,2"|s11_c=="1,2,7"
replace s11_c="2" if s11_c=="2,3,4"|s11_c=="2,4"
replace s11_c="3" if s11_c=="3,1"
replace s11_c="." if s11_c=="不知"|s11_c=="不知道"|s11_c=="妈妈和我一起住"|s11_c=="无"|s11_c=="没"|s11_c=="空"

replace s12_c="1" if s12_c=="1,2"|s12_c=="1,2,3"|s12_c=="1,2,3,4"|s12_c=="4,3,1,2"|s12_c=="1,2,7"
replace s12_c="4" if s12_c=="7"
replace s12_c="." if s12_c=="不知"|s12_c=="不知道"|s12_c=="无"|s12_c=="没"|s12_c=="空"
destring s11_c, replace
destring s12_c, replace

rename s11 g8
rename s11_c g8_c
rename s12 g9
rename s12_c g9_c

drop s14_c //check the father's age
gen s14_c="23" if s14=="23"
forval i=24(1)65 {
replace s14_c="`i'" if s14=="`i'" 
}
replace s14_c="30" if s14=="大约30多"|s14=="大概30以上"|s14=="大概30几"|s14=="30不到"|s14=="大于30"|s14=="三十多"|s14=="30几"|s14=="30多"|s14=="30多岁"|s14=="30多或40"|s14=="30岁以上"|s14=="30左右"
replace s14_c="31" if s14=="31、32"|s14=="31或32"|s14=="大约31"
replace s14_c="32" if s14=="30~34"|s14=="32"|s14=="32、33或34"|s14=="大约32"|s14=="大约32岁"
replace s14_c="35" if s14=="30-40"|s14=="30～40"|s14=="35岁"|s14=="大概35岁"|s14=="大约35"
replace s14_c="36" if s14=="36多"|s14=="大约36"
replace s14_c="37" if s14=="35-40"|s14=="35~40"|s14=="37、38"|s14=="大概37"|s14=="大约37"
replace s14_c="38" if s14=="38周岁"|s14=="≈38"|s14=="约38"
replace s14_c="39" if s14=="39或40"|s14=="大概 39 40"|s14=="大概39"|s14=="大约39岁"
replace s14_c="40" if s14=="30-50"|s14=="大约40"|s14=="大约40多"|s14=="大约40多岁"|s14=="大约40岁"|s14=="大概40"|s14=="大概40多"|s14=="大概40多岁"|s14=="40几"|s14=="40多"|s14=="40多岁"|s14=="40岁以上"|s14=="40岁左右"|s14=="40左右"|s14=="≈40"
replace s14_c="42" if s14=="40-44"|s14=="40-45"|s14=="42以上"|s14=="42岁"|s14=="大约42"
replace s14_c="43" if s14=="43岁"|s14=="估计43岁"|s14=="大概43"|s14=="约43"
replace s14_c="44" if s14=="44~45"|s14=="44岁"|s14=="大约44"
replace s14_c="45" if s14=="40~50"|s14=="大概45岁"|s14=="大概45"|s14=="大约45"|s14=="大约45岁左右"|s14=="40～50"|s14=="45以下"|s14=="45岁"|s14=="45左右"|s14=="牛"
replace s14_c="47" if s14=="47岁"|s14=="大概47岁"
replace s14_c="50" if s14=="大概50多"|s14=="大约50"|s14=="大概50岁"|s14=="50~60左右"|s14=="50多"|s14=="50多岁"|s14=="≈50"
replace s14_c="51" if s14=="51岁"
replace s14_c="57" if s14=="55~60"
replace s14_c="62" if s14=="大月62"
destring s14_c, replace
recast float s14_c
order s14_c,after(s14)
rename s14 g10 
rename s14_c g10_c 

drop s15_c //check the father's educational background
gen s15_c="1" if s15=="1"|s15=="1,2"
replace s15_c="2" if s15=="2"|s15=="1,2"
replace s15_c="3" if s15=="2,3"|s15=="3"
replace s15_c="4" if s15=="4"|s15=="1,2,3,4"|s15=="2,3,4"|s15=="3,4"|s15=="4,1"|s15=="4,2"|s15=="4,2,3"|s15=="4,3"
replace s15_c="5" if s15=="5"|s15=="2,3,4,5"|s15=="3,5"|s15=="4,5"
replace s15_c="6" if s15=="6"|s15=="1,2,3,4,5,6"|s15=="1,6"|s15=="2,3,4,5,6"|s15=="2,4,3,6"|s15=="3,6"|s15=="7"
destring s15_c, replace
recast float s15_c
order s15_c,after(s15)
rename s15 g11 
rename s15_c g11_c

drop s16_c //check the mother's age
gen s16_c="20" if s16=="20"|s16=="20多"
forval i=21(1)63 {
replace s16_c="`i'" if s16=="`i'" 
}
replace s16_c="23" if s16=="大约23"
replace s16_c="27" if s16=="20-35"
replace s16_c="28" if s16=="大概28岁"
replace s16_c="30" if s16=="大约30多"|s16=="大概30多"|s16=="大概30几"|s16=="大概30"|s16=="大于30"|s16=="大30多"|s16=="30、31"|s16=="30不到"|s16=="30多"|s16=="30多岁"|s16=="30岁"|s16=="30岁以上"|s16=="30左右"
replace s16_c="31" if s16=="32、31"
replace s16_c="32" if s16=="30-34"|s16=="32岁"|s16=="32或33"|s16=="大约32"
replace s16_c="33" if s16=="33岁"|s16=="大约33岁"
replace s16_c="34" if s16=="大约34岁"
replace s16_c="35" if s16=="30-40"|s16=="大约35"|s16=="30～40"|s16=="35以上"|s16=="大概35"
replace s16_c="36" if |s16=="36多"|s16=="兔36"
replace s16_c="37" if s16=="30~45"|s16=="大约37岁"|s16=="大约37"|s16=="30～45"|s16=="37周岁"|s16=="37岁"|s16=="37左右"
replace s16_c="38" if s16=="≈38"|s16=="大概 38 39"
replace s16_c="39" if s16=="30~49"|s16=="约为39"|s16=="约39"|s16=="39左右"|s16=="≈39"|s16=="大概39"
replace s16_c="40" if s16=="大约40岁"|s16=="大约40多岁"|s16=="大约40"|s16=="大概40多"|s16=="大概40以上"|s16=="40以上"|s16=="40几"|s16=="40多"|s16=="40多岁"|s16=="40岁"|s16=="40岁以上"|s16=="40岁左右"
replace s16_c="42" if s16=="42岁"|s16=="估计42岁"
replace s16_c="43" if s16=="43~44"|s16=="大概43"|s16=="大约43"|s16=="约43"
replace s16_c="44" if s16=="大约44"
replace s16_c="45" if s16=="40~50"|s16=="大约40～50"|s16=="40～50"|s16=="45以下"|s16=="45左右"
replace s16_c="46" if s16=="大约46"|s16=="大约46岁左右"
replace s16_c="47" if s16=="47岁"|s16=="大概47岁"
replace s16_c="50" if s16=="50多"|s16=="50岁"|s16=="≈50"
replace s16_c="56" if s16=="大概56"
replace s16_c="60" if s16=="大月60"
destring s16_c, replace
recast float s16_c
order s16_c,after(s16)
rename s16 g12 
rename s16_c g12_c

drop s17_c //check the mother's educational background
gen s17_c="1" if s17=="1"|s17=="0"
replace s17_c="2" if s17=="2"|s17=="1,2"
replace s17_c="3" if s17=="3"|s17=="1,2,3"|s17=="2,3"|s17=="3,2"
replace s17_c="4" if s17=="4"|s17=="1,3,4"|s17=="2,3,4"|s17=="3,4"
replace s17_c="5" if s17=="5"|s17=="2,3,4,5"|s17=="4,5"|s17=="5,4"
replace s17_c="6" if s17=="6"|s17=="1,2,3,4,5,6"|s17=="2,3,4,6"|s17=="2,4,6"|s17=="4,5,6"|s17=="4,6"|s17=="4,6,5"|s17=="7"
destring s17_c, replace
recast float s17_c
order s17_c,after(s17)
rename s17 g13 
rename s17_c g13_c

drop s18_c //check the type of house the family lives in
gen s18_c="1" if s18=="1"|s18=="毛草房"|s18=="瓦房"|s18=="现在是1 不过在老家买了2"
replace s18_c="2" if s18=="2"
destring s18_c, replace
order s18_c,after(s18)
rename s18 g14 
rename s18_c g14_c

drop s19_c //check if there is a TV at home
gen s19_c="1" if s19=="1"
replace s19_c="2" if s19=="2"|s19=="0"|s19=="不"
destring s19_c, replace
order s19_c,after(s19)
rename s19 g15 
rename s19_c g15_c

drop s20_c //check if there is a microwave oven at home
gen s20_c="1" if s20=="1"
replace s20_c="2" if s20=="2"|s20=="不"
destring s20_c, replace
order s20_c,after(s20)
rename s20 g16
rename s20_c g16_c

drop s21_c //check if there is a refrigerator at home
gen s21_c="1" if s21=="1"|s21=="a"
replace s21_c="2" if s21=="2"
destring s21_c, replace
order s21_c,after(s21)
rename s21 g17 
rename s21_c g17_c

drop s22_c //check if there is a computer at home
gen s22_c="1" if s22=="1"
replace s22_c="2" if s22=="2"|s22=="无"
destring s22_c, replace
order s22_c,after(s22)
rename s22 g18 
rename s22_c g18_c

drop s23_c //check if the Internet is available
gen s23_c="1" if s23=="1"|s23=="ok"
replace s23_c="2" if s23=="2"|s23=="不"
destring s23_c, replace
order s23_c,after(s23)
rename s23 g19 
rename s23_c g19_c

drop s24_c //check if there is an AC at home
gen s24_c="1" if s24=="1"
replace s24_c="2" if s24=="2"
destring s24_c, replace
order s24_c,after(s24)
rename s24 g20 
rename s24_c g20_c

drop s25_c //check if there is a washer at home
gen s25_c="1" if s25=="1"
replace s25_c="2" if s25=="2"|s25=="不"
destring s25_c, replace
order s25_c,after(s25)
rename s25 g21 
rename s25_c g21_c

drop s29_c //check how to go to school usually
gen s29_c="1" if s29=="1"
replace s29_c="2" if s29=="2"
replace s29_c="3" if s29=="3"
replace s29_c="4" if s29=="4"
replace s29_c="5" if s29=="5"
destring s29_c, replace
recast float s29_c
order s29_c,after(s29)
rename s29 g22 
rename s29_c g22_c

drop s30_c //check the commuting time
gen s30_c="1" if s30=="1"|s30=="(家长接送) 1"
forval i=2(1)5 {
replace s30_c="`i'" if s30=="`i'" 
}
destring s30_c, replace
recast float s30_c
order s30_c,after(s30)
rename s30 g23 
rename s30_c g23_c

rename s32 g24a 
rename s33 g24
rename s34 g25

drop s35_c //check if a class leader
gen s35_c="1" if s35=="1"|s35=="没有"|s35=="学生"
replace s35_c="2" if s35=="2"|s35=="体育委员"|s35=="副班长"|s35=="副班长 组长"|s35=="卫生委员"|s35=="安全委员"|s35=="宿管"|s35=="消毒"|s35=="班长"|s35=="管小组的芝麻官"|s35=="组长"|s35=="语文"|s35=="语文组长"|s35=="语文课代表"
destring s35_c, replace
recast float s35_c
order s35_c,after(s35)
rename s35 g26 
rename s35_c g26_c
rename s35a g26a
rename s35a_c g26a_c

drop s36_c //check the lunch expenses
gen s36_c="0" if s36=="0"|s36=="学校"|s36=="学校吃"|s36=="家里"|s36=="无"|s36=="没有"|s36=="没"|s36=="没花钱"|s36=="空"|s36=="家里吃"|s36=="在家吃午饭"|s36=="在家吃饭"|s36=="在家里吃"|s36=="在校吃"|s36=="在家吃"|s36=="在家"|s36=="在家吃 不花钱"|s36=="NO"|s36=="一分不要"|s36=="不发钱"|s36=="不一定"|s36=="不在这吃"|s36=="不收钱"|s36=="不有花钱"|s36=="不用"|s36=="不用线"|s36=="不用花钱"|s36=="不用钱"|s36=="不用钱我家种菜吃"|s36=="不花"|s36=="不花 再学校吃"|s36=="不花(0元)"|s36=="不花在家里吃"|s36=="不花钱"|s36=="不花钱 在家"|s36=="不要"|s36=="不要钱"|s36=="不需要"|s36=="免"|s36=="免费"|s36=="否"|s36=="回家"|s36=="回家吃"|s36=="回家吃"|s36=="回家吃饭"|s36=="一天不花"|s36=="不"|s36=="不0"|s36=="0、回家吃"|s36=="0、在家吃"|s36=="0、在家里"|s36=="0、没花"|s36=="0元"|s36=="0元(下午5元)"|s36=="0元(在家吃)"|s36=="0元/回家吃"|s36=="0元/天"
forval i=0.1(0.1)200 {
replace s36_c="`i'" if s36=="`i'" 
}
replace s36_c="0.2" if s36=="1元5天"
replace s36_c="0.5" if s36=="5角"
replace s36_c="0.6" if s36=="3元5天"
replace s36_c="0.8" if s36=="4元/5天"|s36=="8角"|s36=="8角钱"
replace s36_c="1.5" if s36=="1-2"
replace s36_c="2" if s36=="1、3"|s36=="2元/次"|s36=="2元、中年一次"
replace s36_c="2.5" if s36=="1~4"|s36=="2-3"
replace s36_c="3" if s36=="1~5"|s36=="3元1上午"
replace s36_c="3.5" if s36=="1~5"|s36=="3、4"|s36=="3~4"|s36=="2或5"|s36=="2-5"|s36=="2~5"|s36=="2-5"
replace s36_c="4" if s36=="3-5"|s36=="3~5"|s36=="3或5"|s36=="4元"|s36=="4元(除星期六七)"
replace s36_c="4.5" if s36=="3-6"|s36=="4-5"|s36=="4~5"|s36=="4、5"
replace s36_c="5" if s36=="10元2天"|s36=="5元左右"|s36=="5元以内"|s36=="1天5元"|s36=="4-6"|s36=="4~6"
replace s36_c="5.5" if s36=="5-6"|s36=="5~6"
replace s36_c="6" if s36=="1天6块"|s36=="在学校吃6元"|s36=="2-10"|s36=="5-7"
replace s36_c="6.5" if s36=="3~10"
replace s36_c="7.5" if s36=="10-5"|s36=="5-10"|s36=="5~10"
replace s36_c="7" if s36=="7元一餐"
replace s36_c="8" if s36=="6-10"|s36=="6~10"|s36=="6到10"|s36=="快餐8元/天"
replace s36_c="8.5" if s36=="7-10"
replace s36_c="9" if s36=="8-10"
replace s36_c="10" if s36=="5~15"|s36=="8~12"|s36=="10以上"|s36=="10元以下"|s36=="10元左右"|s36=="10左右"
replace s36_c="11" if s36=="10 12"|s36=="10-12"|s36=="10-12分"
replace s36_c="12.5" if s36=="10~15"
replace s36_c="15" if s36=="10~20"|s36=="15以下"|s36=="15元以上"|s36=="15元以内"|s36=="15元左右"
replace s36_c="15.5" if s36=="15-16"
replace s36_c="16" if s36=="12-20"|s36=="16元左右"
replace s36_c="16.5" if s36=="15-18"
replace s36_c="17" if s36=="在学校17"
replace s36_c="17.5" if s36=="5-20"
replace s36_c="20" if s36=="1天20元"|s36=="20元以上"|s36=="20元左右"|s36=="20左右"
replace s36_c="25" if s36=="20-30"|s36=="20~30"|s36=="20至30"
replace s36_c="30" if s36=="10~50"|s36=="30元内"|s36=="30多"
replace s36_c="35" if s36=="30-40"
replace s36_c="40" if s36=="30-50"|s36=="40以下"|s36=="40天"
replace s36_c="45" if s36=="40~50"|s36=="约40~50"
replace s36_c="48" if s36=="36-60"
replace s36_c="50" if s36=="100元/2天"
replace s36_c="57.5" if s36=="35~100"
replace s36_c="60" if s36=="40~80"
replace s36_c="75" if s36=="50-100"
replace s36_c="80" if s36=="60~100"|s36=="80多元"
replace s36_c="100" if s36=="100以内"|s36=="100元以内"|s36=="100多"
replace s36_c="175" if s36=="50-200"
destring s36_c, replace
recast float s36_c
order s36_c,after(s36)
rename s36 g27 
rename s36_c g27_c

drop s37_c //check pocket money
gen s37_c="1" if s37=="1"|s37=="有时候"|s37=="有时"|s37=="10"|s37=="8"|s37=="7"|s37=="50"|s37=="5"|s37=="40"|s37=="4"|s37=="34"|s37=="3"|s37=="25"|s37=="20元"|s37=="20"|s37=="15元"|s37=="15"|s37=="10元"|s37=="12"|s37=="14元"
replace s37_c="2" if s37=="2"|s37=="0"|s37=="0元"|s37=="不"|s37=="不是"|s37=="不知道"|s37=="回家吃"|s37=="无"|s37=="没有"|s37=="空"
destring s37_c, replace
recast float s37_c
order s37_c,after(s37)
rename s37 g28 
rename s37_c g28_c

drop s38_c
gen s38_c="0" if s38=="0"|s38=="0、没有"|s38=="家长不给"|s38=="家长没有给过我零花钱"|s38=="家长给不要"|s38=="我不要零花钱"|s38=="无"|s38=="没"|s38=="没有"|s38=="没有 0"|s38=="没有零花钱"|s38=="否"|s38=="从来不花"|s38=="一分钱的没有"|s38=="一般不用"|s38=="不"|s38=="不是"|s38=="不用"|s38=="不给"|s38=="不给钱"|s38=="不要"|s38=="一般不花"|s38=="0元"|s38=="0毛"|s38=="0没有"
forval i=0.1(0.05)100 {
replace s38_c="`i'" if s38=="`i'" 
}
replace s38_c="0.1" if s38=="0.5元5天"|s38=="1毛"
replace s38_c="0.2" if s38=="5天1元"
replace s38_c="0.25" if s38=="1元4天"
replace s38_c="0.28" if s38=="0.28"
replace s38_c="0.3" if s38=="3毛"
replace s38_c="0.42" if s38=="0.42"
replace s38_c="0.5" if s38=="0-1"|s38=="5毛"|s38=="5角"|s38=="10天2元"|s38=="1-5角"|s38=="1-0"|s38=="0~1"|s38=="0或1"|s38=="1-0"
replace s38_c="0.6" if s38=="5天3元"
replace s38_c="0.75" if s38=="1元5角2天"|s38=="3元/4"|s38=="5角-1元钱"
replace s38_c="1" if s38=="0~2"|s38=="5天5元"|s38=="5元5天"|s38=="1元"|s38=="1天1块"|s38=="1元1天"|s38=="0或2"|s38=="10元/10天"|s38=="10天10元"
replace s38_c="1.25" if s38=="0.5-2"
replace s38_c="1.5" if s38=="1-2"|s38=="1或2"|s38=="1或2元左右"|s38=="1元或2元"|s38=="1元或两元"|s38=="1~2"|s38=="1元5角"
replace s38_c="2" if s38=="3/1"|s38=="2元"|s38=="1~3"|s38=="1或3"|s38=="1-3"|s38=="1-3元"|s38=="10元/5天"
replace s38_c="2.5" if s38=="0-5"|s38=="两三元"|s38=="5元或2元"|s38=="5元2天"|s38=="3或2"|s38=="3~2"|s38=="3-2"|s38=="2或3"|s38=="2至3"|s38=="2天3-7元"|s38=="2元至3元"|s38=="2、3元"|s38=="2~3"|s38=="2/3"|s38=="2-3"|s38=="1-4"|s38=="1~4"
replace s38_c="2.75" if s38=="2.5~3"
replace s38_c="3" if s38=="1-5"|s38=="平均3元左右"|s38=="5或1"|s38=="3元以内"|s38=="3元"|s38=="3、早饭"|s38=="2~3~4"|s38=="2-4"|s38=="1至5元"|s38=="1/5"|s38=="1~5"|s38=="1或5"|s38=="1至5"
replace s38_c="3.5" if s38=="1-6"|s38=="3~4"|s38=="3、4"|s38=="3-4"|s38=="2-5"|s38=="2~5"
replace s38_c="4" if s38=="3-5"|s38=="5到3"|s38=="4元多"|s38=="3至5"|s38=="3/5"|s38=="3~5"
replace s38_c="4.25" if s38=="1.5,7"
replace s38_c="4.5" if s38=="1-8"|s38=="2-7"|s38=="4~5"|s38=="4-5"|s38=="4.5块"
replace s38_c="5" if s38=="10元2天"|s38=="5左右"|s38=="5块"|s38=="5块以下"|s38=="5以上"|s38=="5元之内"|s38=="5元以上"|s38=="5元以内"|s38=="5元左右"|s38=="5以内"|s38=="5元"|s38=="4~6"|s38=="25元5天"|s38=="3-7"
replace s38_c="5.5" if s38=="1-10"|s38=="5或6"|s38=="5.5"|s38=="5-6"|s38=="10~1"|s38=="1~10"|s38=="1或10"
replace s38_c="6" if s38=="2-10"|s38=="5-7"
replace s38_c="6.5" if s38=="3-10"
replace s38_c="7" if s38=="4-10"
replace s38_c="7.5" if s38=="10或5"|s38=="5至10"|s38=="5或10"|s38=="5到10"|s38=="5元或10元"|s38=="5-10"|s38=="5~10"
replace s38_c="10" if s38=="10、5"|s38=="10元以上"|s38=="10元以下"|s38=="10元以内"|s38=="10元"|s38=="10元/10元"
replace s38_c="12.5" if s38=="10-15"|s38=="5-20"
replace s38_c="14" if s38=="好像是14元"
replace s38_c="15" if s38=="10-20"|s38=="10~20"|s38=="10或20"
replace s38_c="20" if s38=="10~30"|s38=="二十多"|s38=="20多"|s38=="20元以内"
replace s38_c="25" if s38=="0-50"|s38=="20-30"|s38=="20~30"
replace s38_c="30" if s38==s38=="30"
replace s38_c="35" if s38=="20-50"|s38=="50~20"
replace s38_c="60" if s38=="20-100"
replace s38_c="70" if s38=="70多"
replace s38_c="75" if s38=="100-50"
replace s38_c="100" if s38=="1百多"
destring s38_c, replace
recast float s38_c
order s38_c,after(s38)
rename s38 g29 
rename s38_c g29_c

drop s39_c //check if have ever repeated
gen s39_c="1" if s39=="1"
replace s39_c="2" if s39=="2"|s39=="0"|s39=="不"|s39=="没"|s39=="没有"
destring s39_c, replace
recast float s39_c
order s39_c,after(s39)
rename s39 g30 
rename s39_c g30_c

rename s40 g31
rename s41 g32
rename s43 g33
rename s43_c g33_c
rename province pro 
keep id pro name height weight county town grade mainteacher mathscore esteem1 grit1 dep1 depress1 sde1 sdn1 sdo1 sdg1 sdv1 g1 g1_c g2 g2_c g3 g3_c g4 g4_c g5 g5_c g6a g6b g6c g7 g7_c g7_c2 g8 g8_c g9 g9_c g10 g10_c g11 g11_c g12 g12_c g13 g13_c g14 g14_c g15 g15_c g16 g16_c g17 g17_c g18 g18_c g19 g19_c g20 g20_c g21 g21_c g22 g22_c g23 g23_c g24 g24a g25 g26 g26_c g26a g26a_c g27 g27_c g28 g28_c g29 g29_c g30 g30_c g31 g32 g33 g33_c g34 g34_c g35 g35_c g36 g36_c g37 g37_c g38 g38_c g39 g39_c g40 g40_c g41 g41_c g42 g42_c g43 g43_c g44 g44_c g45 g45_c g46 g46_c g47 g47_c g48 g48_c g49 g49_c 
g year=2018
save "D:\lsq\CCAP_CAU_student\dtanew\student2(1).dta",replace

**merge final student table with parent table after cleaning
import excel "D:\lsq\CCAP_CAU_student\raw\家长表-2018.xlsx", firstrow clear
rename 问卷编码 id 
rename 学生姓名 name
rename 年级 grade
replace id = subinstr(id, "A","", .) //clean ID
replace id = subinstr(id, "B","", .)
replace id = subinstr(id, "大","", .)
replace id = subinstr(id, "小","", .)
replace id="34413205" if id=="344132"
replace id="34413249" if id=="344132049"
replace id="34613137" if id=="346131"
replace id="35313115" if id=="353131315"
replace id="41213127" if id=="412131027"
replace id="35114119" if id=="A351141"
replace id="35114124" if id=="A351141024"
replace id="35114141" if id=="A351141042"
replace id="17023225" if name=="刘玉嘉"
replace id="17023210" if name=="边晓颖"
replace id="17023202" if name=="周婷珠"
replace id="17023241" if name=="刘雨娜"
replace id="17023213" if name=="耿雅琪"
replace id="17023224" if name=="骆诚"&id=="无编码-006"
replace id="17023208" if name=="张诗玉"
replace id="17023236" if name=="张耀天"
replace id="17023201" if name=="董轩"
replace id="17023207" if name=="李世洁"
replace id="17023219" if name=="王婷婷"&id=="无编码-011"
replace id="17023222" if name=="胡宁"
replace id="17023242" if name=="孙琪"
replace id="17023232" if name=="边晓军"
replace id="17023218" if name=="白添函"
replace id="17023203" if name=="李宇航"&id=="无编码-016"
replace id="17023209" if name=="王宇涵"&id=="无编码-017"
replace id="17023238" if name=="王怡琳"
replace id="17023217" if name=="刘玉成"
replace id="17023233" if name=="孟令华"
replace id="17023206" if name=="孔健杰"
replace id="17023221" if name=="张文文"
replace id="17023220" if name=="何欣悦"
replace id="17023215" if name=="周子浩"
replace id="17023227" if name=="刘旭"&id=="无编码-025"
replace id="17023205" if name=="刘浩然"&id=="无编码-026"
replace id="17023231" if name=="崔俊浩"
replace id="17023230" if name=="张国傲"
replace id="17023212" if name=="贾艺森"
replace id="17023229" if name=="张宇峰"&id=="无编码-030"
replace id="17023234" if name=="刘海川"
replace id="17023228" if name=="韩秋雨"
replace id="34513154" if name=="贾亚辉(IQ)"
replace id="34513155" if name=="纪永??"
drop if id=="无编码-001"
drop if id=="无编码-002"
drop if id=="无编码-003"

destring id,replace force
duplicates list id
drop if id==17023202& grade==""
drop if id==17023203& grade==""
drop if id==17023205& grade==""
drop if id==17023208& grade==""
drop if id==17023210& grade==""
drop if id==17023212& grade==""
drop if id==17023213& grade==""
drop if id==17023215& grade==""
drop if id==17023217& grade==""
drop if id==17023219& grade==""
drop if id==17023220& grade==""
drop if id==17023222& grade==""
drop if id==17023225& grade==""
drop if id==17023227& grade==""
drop if id==17023230& grade==""
drop if id==17023231& grade==""
drop if id==17023232& grade==""
drop if id==17023233& grade==""
drop if id==17023236& grade==""
replace id=21073546 if name=="乔志琪"
replace id=35114118 if name=="朱秀秀"
replace id=35413116 if name=="周毅然"
drop if id==.

rename 家里有几个孩子 children 
replace children="." if children=="2个堂哥"|children=="2个孙辈"|children=="不知道"|children=="没有" 
replace children="2" if children=="现在2"
destring children, replace
gen g50_c=children-1

rename B1a该生是否有兄弟姐妹哥哥11是2否 g51 
gen g51_c=1 if g51=="1"|g51=="√"|g51=="2"|g51=="3"|g51=="4"|g51=="5"|g51=="6"|g51=="7"|g51=="8"|g51=="9"|g51=="10"|g51=="11" 
replace g51_c=0 if g51=="0"|g51=="2"|g51=="1"
keep id g50_c g51_c

merge 1:1 id using"D:\lsq\CCAP_CAU_student\dtanew\student2(1).dta",force
drop if _merge==1
drop _merge
save "D:\lsq\CCAP_CAU_student\dtanew\student2.dta",replace

use "D:\lsq\CCAP_CAU_student\dtanew\student1.dta",clear
append using "D:\lsq\CCAP_CAU_student\dtanew\student2.dta",force
sort id year
order id year
save "D:\lsq\CCAP_CAU_student\dtanew\final.dta",replace

**clean school table
import excel "D:\lsq\CCAP_CAU_student\学校表1.xlsx", firstrow clear
rename 省市 pro
rename 问卷编码 id
replace id="4261" if id=="42615"| id=="42616"
replace id="3511" if id=="A3511"
destring id, replace
rename 区县 county
rename 乡镇 town
rename 学校名称 school
rename 被访者姓名 name
rename 被访者性别1男2女 sex
rename 被访者职务 post
rename 在本校时间年 year
replace year="18" if year=="2000"
replace year="12" if year=="2006"
replace year="10" if year=="2008-2018"
replace year="4" if year=="201402"
replace year="3" if year=="2015"
replace year="2" if year=="201608"| year=="2016年至今"
replace year="1" if year=="2017"| year=="201711"
replace year="0.2" if year=="201808"
destring year, gen(year_c)
rename 被访者电话 tel
rename 电子邮箱 email
rename 贵校是哪一年建校的 s1
gen s1_c=s1
replace s1_c=1978 if s1_c==78
replace s1_c=1949 if s1_c==194906
replace s1_c=1980 if s1_c==198009
replace s1_c=1997 if s1_c==199708
replace s1_c=1999 if s1_c==199902| s1_c==199909
replace s1_c=2014 if s1_c==201403
recast int s1_c
order s1_c, after(s1)
rename 贵校在过去的五年中搬迁过几次 s2
gen s2_c=s2
replace s2_c="0" if s2_c=="无"| s2_c=="未"| s2_c=="未搬迁"| s2_c=="没"
destring s2_c,replace
rename 贵校的学生总数 s3
replace s3="700" if s3=="700人左右"
destring s3,replace
rename 贵校是否是完小有六年级1是有2否 s3_1
gen s3_1c=s3_1
replace s3_1c="1" if s3_1=="九年一贯制"
destring s3_1c,replace
order s3_1c,after(s3_1)
rename 平均每个班级的学生数 s4
gen s4_c=s4
replace s4_c="42" if s4=="38-46"
replace s4_c="45" if s4=="45人左右"
replace s4_c="60" if s4=="60多"
replace s4_c="20" if s4=="不足20"
destring s4_c,replace
order s4_c,after(s4)
rename 贵校实际有多少名教师 s5
replace s5="." if s5=="不了解"
destring s5,replace
rename 其中女老师多少人 s6
rename 全部老师中有多少名小教特级教师 s7
gen s7_c=s7
replace s7_c="0" if s7=="无"|s7=="无特级教师(有中小学高级教师)"
destring s7_c,replace
rename 其中小教特级女教师 s8
rename 全部老师中有多少名小教高级教师 s9
gen s9_c=s9
replace s9_c="0" if s9=="无"
replace s9_c="." if s9=="不了解"
destring s9_c,replace
rename 其中小教高级女教师 s10
rename 全部老师中有多少名小教一级教师 s11
gen s11_c=s11
replace s11_c="0" if s11=="无"
replace s11_c="." if s11=="不了解"
destring s11_c,replace
rename 其中小教一级女教师 s12
rename 全部老师中有多少名小教二级教师 s13
gen s13_c=s13
replace s13_c="0" if s13=="无"
replace s13_c="." if s13=="不了解"
destring s13_c,replace
rename 其中女小教二级教师 s14
rename 贵校有多少个教师编制 s15
rename 贵校有多少名非教师职工 s16
rename 其中女性的非教师职工多少个 s17
rename 贵校的占地面积平方米 s18
gen s18_c=s18
replace s18_c="10000" if s18=="15亩"
replace s18_c="30000" if s18=="30000多"
replace s18_c="4666.7" if s18=="7亩"
replace s18_c="26666.7" if s18=="40亩"
replace s18_c="." if s18=="不清楚"|s18=="空" 
destring s18_c,replace
rename 贵校有没有阅览室1有2没有 s19
rename 贵校是否开设电脑课程1是2否 s20
rename 贵校是否采用电脑辅助教学1是2否 s21
rename 其中数学课是否采用电脑辅助教学1是2否 s22
gen s22_c=s22
rename a学校电脑是否可以上网1是2否 s22a
rename b学校总计有多少台电脑可供学生使用是2否 s22b
rename 学校有没有学生食堂1有2没有 s23
gen s23_c=s23
rename 学校的食堂是哪一年修建的 s24
rename 学生食堂的供餐方式1私人承包2学校直接雇人给学生做 s25
rename a其他说明 s26
rename 学生在食堂吃早餐的伙食费每学期是多少钱元学期 s27
rename 学生在食堂吃午餐的伙食费每学期是多少钱元学期 s28
rename 学生在食堂吃晚餐的伙食费每学期是多少钱元学期 s29
drop year
gen year=2018
keep id year school s1 s1_c s3 s4 s4_c s5 s7 s7_c s9 s9_c s11 s11_c s13 s13_c s18 s19 s20 s21 s22 s22_c s22a s23 s23_c
save "D:\lsq\CCAP_CAU_student\dtanew\school(2018).dta", replace

use"D:\lsq\CCAP_CAU_student\raw\school2017.dta",clear //append with 2017 school data
replace s1="2002" if s1=="2002年8月"
destring s1, replace
drop year
gen year=2017
keep id year school s1 s1_c s3 s4 s4_c s5 s7 s7_c s9 s9_c s11 s11_c s13 s13_c s18 s19 s20 s21 s22 s22_c s22a s23 s23_c
append using "D:\lsq\CCAP_CAU_student\dtanew\school(2018).dta",force
sort id year
order id year
gen s7_t=s7_c/s5
gen s9_t=s9_c/s5
gen s11_t=s11_c/s5
gen s13_t=s13_c/s5
gen school1=id
save "D:\lsq\CCAP_CAU_student\dtanew\school.dta", replace

**check teachers' information
import excel "D:\lsq\CCAP_CAU_student\raw\老师表-2018.xlsx", firstrow clear
rename 问卷编码 id
replace id = subinstr(id, "-1","", .)
replace id = subinstr(id, "-2","", .)
replace id = subinstr(id, "-3","", .)
replace id = subinstr(id, "(1)","", .)
replace id = subinstr(id, "(2)","", .)
duplicates drop id, force
replace id="21031D" if id=="210331D"
replace id="21041D" if id=="210441D"
replace id="21054B" if id=="210548B"
replace id="21054D" if id=="210548D"
replace id="21054N" if id=="210548N"
replace id="21084D" if id=="2108D"
replace id="21094B" if id=="210946B"
replace id="21094D" if id=="210946D"
replace id="21094N" if id=="210946N"
replace id="21043B" if id=="211043B"
replace id="21043D" if id=="211043D"
replace id="21043N" if id=="211043N"
replace id="21044B" if id=="210441B"
replace id="43114b" if id=="43114b1"
drop if id=="43114b2"
replace id="43214b" if id=="43214b1"
drop if id=="43214b2"
replace id="35113b" if id=="A3511b"
replace id="35113d" if id=="A3511d"
replace id="35113n" if id=="a3511n"
drop if id=="id"
rename 班 class
drop if class=="(1)(2)"|class=="(2)(3)"|class=="1、2"
destring class,replace
rename 您的性别1男2女 c2
destring c2,replace
rename 您的年龄周岁 c3
destring c3,replace
rename 您的最高学历是1小学2初中3高中4中专 c5
drop if c5=="1,5"
destring c5,replace
rename 您的教龄年 c7
replace c7="0" if c7=="0(刚来)"|c7=="0、今年刚考录"|c7=="新教师"
replace c7="0.5" if c7=="2个月"
replace c7="55" if c7=="1963"|c7=="19631220"
destring c7,replace
rename 您的职称是1小教二级2小教一级3小教高级4 c8
replace c8="2" if c8=="4"& a其他注明=="中一"
replace c8="3" if c8=="4"& a其他注明=="中学高级教师(副高)"
replace c8="2" if c8=="4"& a其他注明=="中小学一级"
replace c8="0" if c8=="4"& a其他注明=="中小学三级"
replace c8="1" if c8=="4"& a其他注明=="中小学二级"
replace c8="3" if c8=="4"& a其他注明=="中小学高级教师"
replace c8="2" if c8=="4"& a其他注明=="中教一级"
replace c8="1" if c8=="4"& a其他注明=="中教二级"
replace c8="0" if c8=="4"& a其他注明=="代课老师"
replace c8="0" if c8=="4"& a其他注明=="因是代课老师 主管部门不给评职称"
replace c8="0" if c8=="4"& a其他注明=="新任"
replace c8="0" if c8=="4"& a其他注明=="无"
replace c8="0" if c8=="4"& a其他注明=="无职称"
replace c8="0" if c8=="4"& a其他注明=="暂无"
replace c8="0" if c8=="4"& a其他注明=="未参评"
replace c8="0" if c8=="4"& a其他注明=="未定级"
replace c8="0" if c8=="4"& a其他注明=="本校不评职称"
replace c8="0" if c8=="4"& a其他注明=="正在准备"
replace c8="0" if c8=="4"& a其他注明=="民办"
replace c8="0" if c8=="4"& a其他注明=="没有评职称"
replace c8="0" if c8=="4"& a其他注明=="没有转正"
replace c8="0" if c8=="4"& a其他注明=="特岗"
replace c8="0" if c8=="4"& a其他注明=="还未定级"
destring c8,gen(c8_c1)
order c8_c1,after(c8)
rename 您在这所学校几年了 c9
replace c9="0" if c9=="0、刚来"|c9=="20180901"|c9=="刚来"|c9=="新教师"
replace c9="0.3" if c9=="2个月"
replace c9="0.5" if c9=="半年"|c9=="半"
replace c9="1" if c9=="不满1年"|c9=="不满一年"
replace c9="2.5" if c9=="2年半"
destring c9,gen(c9_c)
rename 您带这个班几个学期了 c10
replace c10="0.5" if c10=="半"|c10=="半个"
destring c10,gen(c10_c)
rename 您上学期在这所学校当老师每月的收入是多少元包括基本工资 c11
replace c11="2000" if c11=="刚上岗还未领到工资"|c11=="工资未发 不知"|c11=="新教师"|c11=="新教师未发"|c11=="无"|c11=="无(台京学校)"|c11=="暂无"|c11=="暂未发工资"|c11=="没发 刚入职"
replace c11="2000" if c11=="2000以上"|c11=="2000多"|c11=="2000左右"
replace c11="2300" if c11=="2300多"
replace c11="2600" if c11=="2600元左右"
replace c11="3000" if c11=="3000以上"|c11=="3000左右"|c11=="3000元左右"
replace c11="3500" if c11=="3000-4000"|c11=="3500元左右"|c11=="3500左右"|c11=="约3500"
replace c11="3700" if c11=="3700左右"
replace c11="4350" if c11=="4200-4500"
replace c11="4700" if c11=="4700左右"
replace c11="5000" if c11=="5000元左右"|c11=="4500-5500"
replace c11="5800" if c11=="5800左右"
replace c11="6000" if c11=="6000元左右"|c11=="6000多元(打卡五千多)"|c11=="六千多"
destring c11,gen(c11_c)
rename 一个学年开几次家长会 c16
replace c16="0" if c16=="刚入职 未开"|c16=="无"|c16=="未开"
replace c16="1.5" if c16=="1-2"
replace c16="3" if c16=="2~4"
destring c16,gen(c16_c)
keep if 老师类型1班主任2数学3语文=="3"

**add teaching method related variables
rename 课上学生在小组内互相学习而不是只有您一个人讲课1 c17
replace c17="2" if c17=="2,3"
destring c17,gen(c17_c)
rename 课上会开展翻转课堂即让学生备课授课您点评1每 c18
destring c18,gen(c18_c)
rename 您会让学生分成小组一起解题或完成一些任务1每节课上 c19
destring c19,gen(c19_c)
rename 您会依据同学们学习能力的高低布置不同的作业1每节课上 c20
replace c20="1" if c20=="1,3"
destring c20,gen(c20_c)
rename 您2017年以来是否参加过教师培训1是2否 c21
destring c21,gen(c21_c)
rename 您2017年以来参加过几次教师培训1一次2两次 c22
destring c22,gen(c22_c)
keep id class c2 c3 c5 c7 c8 c8_c1 c9 c9_c c11 c11_c c17 c17_c c18 c18_c c19 c19_c c20 c20_c c21 c21_c c22 c22_c 

ssc install catenate
gen id_school=substr(id,1,5)
drop if class==.
catenate id_class = id_school class, p(no)
save "D:\lsq\CCAP_CAU_student\dtanew\chitea.dta", replace

**add teaching evaluation related variables
use "D:\lsq\CCAP_CAU_student\raw\student2018_v2.dta", clear
keep id name height s181 s182 s183 s184 s185 s186 s187 s188 s189 s190 s191 s192 s193 s194 s195 s196 s197 s198 s199 s200 s201 s202
foreach x of varlist s181_c s182 s183 s184 s185 s186 s187 s188 s189 s190 s191 s192 s193 s194 s195 s196 {
gen `x'_c="1" if `x'=="1"
replace `x'_c="2" if `x'=="2"
replace `x'_c="3" if `x'=="3"
replace `x'_c="4" if `x'=="4"
destring `x'_c, replace
order `x'_c, after(`x')
rename `x' c`x'
rename `x'_c c`x'_c
}

gen s197_c="1" if s197=="1"|s197=="男女平等"
replace s197_c="2" if s197=="2"|s197=="对女生更好"
replace s197_c="3" if s197=="3"
destring s197_c, replace
order s197_c, after(s197)
rename s197 c197
rename s197_c c197_c

gen s198_c="." if s198=="N"|s198=="√"|s198=="一个星期"|s198=="一学期"|s198=="不尽的"|s198=="不是"|s198=="不清楚"|s198=="不知"|s198=="不知道"|s198=="不知到"|s198=="不知道 每次"|s198=="不记得"  
replace s198_c="0" if s198=="0"|s198=="NO"|s198=="x"|s198=="不"|s198=="没"|s198=="没有"
forval i=1(0.5)13 {
replace s198_c="`i'" if s198=="`i'"
}
forval i=14(1)365 {
replace s198_c="`i'" if s198=="`i'"
}
replace s198_c="1.5" if s198=="0~3"|s198=="1-2"|s198=="1~2"|s198=="1、2"
replace s198_c="1" if s198=="1万"|s198=="1个星期一次"
replace s198_c="2" if s198=="2次以上"|s198=="大约2"
replace s198_c="2.5" if s198=="2-3"|s198=="2~3"|s198=="2、3"|s198=="3或2"|s198=="二天一次"
replace s198_c="3" if s198=="1-5"|s198=="2~4"|s198=="3"
replace s198_c="3.5" if s198=="3-4"|s198=="3~4"|s198=="3、4"|s198=="3到4"|s198=="3到4次"|s198=="3或4"
replace s198_c="4" if s198=="大约4次"|s198=="4"
replace s198_c="4.5" if s198=="4-5"|s198=="4~5"|s198=="四五次"
replace s198_c="5" if s198=="4-6"|s198=="5 如多次"|s198=="一天一次"|s198=="全"|s198=="全交"|s198=="全交了"|s198=="全批"|s198=="全改"|s198=="全部"|s198=="全都"|s198=="天天"|s198=="天天改"|s198=="每天"|s198=="每天都改"|s198=="每天都批"
replace s198_c="5.5" if s198=="1-10"|s198=="4~7"|s198=="5-6"|s198=="5~6"|s198=="5、6"
replace s198_c="6" if s198=="5~7"|s198=="6"
replace s198_c="6.5" if s198=="6-7"|s198=="6.5"
replace s198_c="7" if s198=="4-10"|s198=="7"
replace s198_c="7.5" if s198=="5-10"|s198=="5~10"|s198=="7-8"|s198=="7、8"
replace s198_c="8" if s198=="8次以上"|s198=="8"
replace s198_c="8.5" if s198=="7-10"
replace s198_c="9.5" if s198=="10~9"
replace s198_c="10" if s198=="10次以上"|s198=="10几次"|s198=="10以上"|s198=="10次多"
replace s198_c="12.5" if s198=="10-15"|s198=="10~15"
replace s198_c="15" if s198=="10-20"|s198=="15"
replace s198_c="20" if s198=="大约20"||s198=="好多次大概20多次"|s198=="20"
replace s198_c="90" if s198=="90次以上"
replace s198_c="1000000" if s198=="1000000"
destring s198_c,replace
winsor s198_c ,gen(s198_c1) p(0.01) 
order s198_c, after(s198)
order s198_c1, after(s198_c)
rename s198 c198
rename s198_c c198_c
rename s198_c1 c198_c1

gen s199_c="0" if s199=="0"|s199=="x"|s199=="×"|s199=="不"|s199=="几乎没有"|s199=="基本没有"|s199=="没"|s199=="没了"|s199=="没有"
replace s199_c="0.5" if s199=="0-1"
forval i=1(0.5)11 {
replace s199_c="`i'" if s199=="`i'"
}
forval i=12(1)103 {
replace s199_c="`i'" if s199=="`i'"
}
replace s199_c="1" if s199=="1个星期一次"|s199=="1次以上"
replace s199_c="1.5" if s199=="1-2"|s199=="1~2"|s199=="1、2"|s199=="1两次"
replace s199_c="2" if s199=="1-3"|s199=="1~3"
replace s199_c="2.5" if s199=="1-4"|s199=="1~4"|s199=="2~3"|s199=="2、3"|s199=="3~2"|s199=="2、3次"|s199=="2到3次"|s199=="2或3"|s199=="2-3"
replace s199_c="3" if s199=="2-4"
replace s199_c="3.5" if s199=="3~4"|s199=="3、4"|s199=="3、4次"|s199=="3到4"|s199=="4-3"|s199=="2-5"
replace s199_c="4" if s199=="3~5"|s199=="3-5"
replace s199_c="4.5" if s199=="1~8"|s199=="2~5"|s199=="3-6"|s199=="3~6"|s199=="4-5"|s199=="4~5"|s199=="4、5"|s199=="5-4"
replace s199_c="5" if s199=="3~7"|s199=="4~6"|s199=="5以上"|s199=="每天"
replace s199_c="5.5" if s199=="1-10"|s199=="1~10"|s199=="4~7"|s199=="5-6"|s199=="5~6"|s199=="5、6"|s199=="5或6"|s199=="五、六次"|s199=="五六次"|s199=="大概5-6次"|s199=="5以上"
replace s199_c="6" if s199=="5-7"|s199=="6"
replace s199_c="7.5" if s199=="5-10"|s199=="7.5"
replace s199_c="8.5" if s199=="8、9次"
replace s199_c="10" if s199=="1-10-20"|s199=="10多次"|s199=="10次以上"|s199=="9-11"
replace s199_c="10.5" if s199=="1-20"
replace s199_c="60" if s199=="50~70"|s199=="60"
replace s199_c="100000" if s199=="100000"
destring s199_c,replace
winsor s199_c ,gen(s199_c1) p(0.01) 
order s199_c, after(s199)
order s199_c1, after(s199_c)
rename s199 c199
rename s199_c c199_c
rename s199_c1 c199_c1

gen s200_c="0" if s200=="0"|s200=="x"|s200=="无"|s200=="没"|s200=="没有"
forval i=1(0.5)8 {
replace s200_c="`i'" if s200=="`i'"
}
forval i=9(1)70 {
replace s200_c="`i'" if s200=="`i'"
}
replace s200_c="1.5" if s200=="1-2"|s200=="1~2"
replace s200_c="2" if s200=="1~3"|s200=="2次"
replace s200_c="2.5" if s200=="2-3"|s200=="2~3"|s200=="3-2"
replace s200_c="3" if s200=="1-5"|s200=="2-4"
replace s200_c="3.5" if s200=="2~5"|s200=="3-4"|s200=="3、4"
replace s200_c="4.5" if s200=="3-6"|s200=="4.5"
replace s200_c="7.5" if s200=="7~8"
replace s200_c="10" if s200=="10几次"|s200=="10"
replace s200_c="11" if s200=="8+3"|s200=="11"
replace s200_c="14.5" if s200=="13~16"
destring s200_c,replace 
winsor s200_c ,gen(s200_c1) p(0.01) 
order s200_c, after(s200)
order s200_c1, after(s200_c)
rename s200 c200
rename s200_c c200_c
rename s200_c1 c200_c1

gen s201_c="0" if s201=="0"|s201=="x"|s201=="×"|s201=="不"|s201=="无"|s201=="没"|s201=="没有"
forval i=1(0.5)8 {
replace s201_c="`i'" if s201=="`i'"
}
forval i=9(1)70 {
replace s201_c="`i'" if s201=="`i'"
}
replace s201_c="1" if s201=="每周一次"
replace s201_c="1.5" if s201=="1-2"|s201=="1~2"|s201=="1、2"|s201=="1或2"
replace s201_c="2" if s201=="1-3"|s201=="2"
replace s201_c="2.5" if s201=="1-4"|s201=="2-3"
replace s201_c="3" if s201=="1-5"|s201=="3几次"|s201=="3"
replace s201_c="4" if s201=="1~7"|s201=="2-6"|s201=="4"
replace s201_c="5.5" if s201=="5-6"
replace s201_c="6" if s201=="5-7"|s201=="6"
destring s201_c,replace
winsor s201_c ,gen(s201_c1) p(0.01) 
order s201_c, after(s201)
order s201_c1, after(s201_c)
rename s201 c201
rename s201_c c201_c
rename s201_c1 c201_c1

gen s202_c="0" if s202=="0"|s202=="没有"
forval i=1(1)4 {
replace s202_c="`i'" if s202=="`i'"
}
destring s202_c, replace
order s202_c, after(s202)
rename s202 c202
rename s202_c c202_c

**merge teaching evaluation table with teaching method table
replace id = subinstr(id, "A"，"", .)
replace id = subinstr(id, "B"，"", .)
replace id = subinstr(id, "大"，"", .)
replace id = subinstr(id, "小"，"", .)
drop if id=="34514106"
replace id="34413205" if id=="344132"
replace id="34413249" if id=="344132049"
replace id="34613137" if id=="346131"
replace id="35313115" if id=="353131315"
replace id="41213127" if id=="412131027"
replace id="35114119" if id=="351141"
replace id="35114124" if id=="351141024"
replace id="35114141" if id=="351141042"
replace id="17023225" if name=="刘玉嘉"
replace id="17023210" if name=="边晓颖"
replace id="17023202" if name=="周婷珠"
replace id="17023241" if name=="刘雨娜"
replace id="17023213" if name=="耿雅琪"
replace id="17023224" if name=="骆诚"&id=="无编码-006"
replace id="17023208" if name=="张诗玉"
replace id="17023236" if name=="张耀天"
replace id="17023201" if name=="董轩"
replace id="17023207" if name=="李世洁"
replace id="17023219" if name=="王婷婷"&id=="无编码-011"
replace id="17023222" if name=="胡宁"
replace id="17023242" if name=="孙琪"
replace id="17023232" if name=="边晓军"
replace id="17023218" if name=="白添函"
replace id="17023203" if name=="李宇航"&id=="无编码-016"
replace id="17023209" if name=="王宇涵"&id=="无编码-017"
replace id="17023238" if name=="王怡琳"
replace id="17023217" if name=="刘玉成"
replace id="17023233" if name=="孟令华"
replace id="17023206" if name=="孔健杰"
replace id="17023221" if name=="张文文"
replace id="17023220" if name=="何欣悦"
replace id="17023215" if name=="周子浩"
replace id="17023227" if name=="刘旭"&id=="无编码-025"
replace id="17023205" if name=="刘浩然"&id=="无编码-026"
replace id="17023231" if name=="崔俊浩"
replace id="17023230" if name=="张国傲"
replace id="17023212" if name=="贾艺森"
replace id="17023229" if name=="张宇峰"&id=="无编码-030"
replace id="17023234" if name=="刘海川"
replace id="17023228" if name=="韩秋雨"
replace id="34513154" if name=="贾亚辉(IQ)"
replace id="34513155" if name=="纪永??"
drop if id=="无编码-001"
drop if id=="无编码-002"
drop if id=="无编码-003"
destring id,replace force
duplicates list id 
drop if id==17023202& height==""
drop if id==17023203& height==""
drop if id==17023205& height==""
drop if id==17023208& height==""
drop if id==17023210& height==""
drop if id==17023212& height==""
drop if id==17023213& height==""
drop if id==17023215& height==""
drop if id==17023217& height==""
drop if id==17023219& height==""
drop if id==17023220& height==""
drop if id==17023222& height==""
drop if id==17023225& height==""
drop if id==17023227& height==""
drop if id==17023230& height==""
drop if id==17023231& height==""
drop if id==17023232& height==""
drop if id==17023233& height==""
drop if id==17023236& height==""
replace id=21073546 if name=="乔志琪"
replace id=35114118 if name=="朱秀秀"
replace id=35413116 if name=="周毅然"
drop if id==.

tostring id, gen(idn) 
gen id_class=substr(idn,1,6)
merge n:n id_class using "D:\lsq\CCAP_CAU_student\dtanew\chitea.dta",force
keep if _merge==3
drop _merge
drop id_class id_school class height
gen year=2018
order year,after(idn)
save "D:\lsq\CCAP_CAU_student\dtanew\chinesetea.dta",replace

**clean seat location table
use "D:\lsq\CCAP_CAU_student\raw\teacher.dta", clear
gen id1=substr(id_c,6,1)
drop if id1=="m"
gen id_school=substr(id_c,1,5)
ssc install catenate
catenate id_class = id_school class, p(no)
keep id_class t22 t22_c t21 t21_c
save "D:\lsq\CCAP_CAU_student\dtanew\teacher_b.dta", replace

use "D:\lsq\CCAP_CAU_student\raw\student-1(2017).dta", clear
tostring id, gen(idn)
gen id_class=substr(idn,1,6)
merge n:n id_class using"D:\lsq\CCAP_CAU_student\dtanew\teacher_b.dta"
drop if _merge==2
drop _merge

//check and revise rows
replace s62="." if s62=="4(3)班"|s62=="??"|s62=="√"|s62=="不子"|s62=="不排"|s62=="不知道"|s62=="不确定"|s62=="中排"|s62=="中间"|s62=="倒"|s62=="右"|s62=="后"|s62=="后排"|s62=="四年级"|s62=="最"|s62=="等球"|s62=="没有"|s62=="论换"
replace s62="1" if s62=="中间第一排左边"
replace s62="3" if s62=="1或5"|s62=="3(2)"|s62=="三(1)"|s62=="中间弟3"|s62=="第4组的第3"|s62=="第三组第8排"
replace s62="5" if s62=="5、2"|s62=="中间第5排"
replace s62="7" if s62=="7排半"
replace s62="13" if s62=="3列13个"
list id_class if s62=="倒数1"
list t21_c if id_class=="422143"
replace s62="10" if s62=="倒数1"
list id_class if s62=="倒数2"
list t21_c if id_class=="230933"
replace s62="7" if s62=="倒数2"|s62=="倒数3排"|s62=="倒数第1"

list id_class if s62=="倒数第2"
list t21_c if id_class=="130241"
replace s62="4" if s62=="倒数第2"&id_class=="130241"
list t21_c if id_class=="210241"
replace s62="7" if s62=="倒数第2"&id_class=="210241"
list t21_c if id_class=="210631"
replace s62="7" if s62=="倒数第2"&id_class=="210631"
list t21_c if id_class=="230131"
replace s62="." if s62=="倒数第2"&id_class=="230131"
list t21_c if id_class=="424132"
replace s62="6" if s62=="倒数第2"&id_class=="424132"

list id_class if s62=="倒数第2排"
list t21_c if id_class=="210641"
replace s62="7" if s62=="倒数第2排"&id_class=="210641"
list t21_c if id_class=="210832"
replace s62="7" if s62=="倒数第2排"&id_class=="210832"
list t21_c if id_class=="220144"
replace s62="7" if s62=="倒数第2排"&id_class=="220144"
list t21_c if id_class=="230933"
replace s62="7" if s62=="倒数第2排"&id_class=="230933"
list t21_c if id_class=="412141"
replace s62="9" if s62=="倒数第2排"&id_class=="412141"
list t21_c if id_class=="413141"
replace s62="7" if s62=="倒数第2排"&id_class=="413141"
list t21_c if id_class=="412141"
replace s62="8" if s62=="倒数第2排"&id_class=="416141"
list t21_c if id_class=="421132"
replace s62="8" if s62=="倒数第2排"&id_class=="421132"

list id_class if s62=="倒数第3排"
list t21_c if id_class=="110531"
replace s62="5" if s62=="倒数第3排"&id_class=="110531"
list t21_c if id_class=="220144"
replace s62="6" if s62=="倒数第3排"&id_class=="220144"
list t21_c if id_class=="321132"
replace s62="6" if s62=="倒数第3排"&id_class=="321132"
list t21_c if id_class=="423143"
replace s62="8" if s62=="倒数第3排"&id_class=="423143"

list id_class if s62=="倒数第3"
list t21_c if id_class=="411141"
replace s62="8" if s62=="倒数第3"

list id_class if s62=="-3"
list t21_c if id_class=="341131"
replace s62="4" if s62=="-3"

list id_class if s62=="倒数第一排"
list t21_c if id_class=="110933"
replace s62="." if s62=="倒数第一排"&id_class=="110933"
list t21_c if id_class=="260132"
replace s62="7" if s62=="倒数第一排"&id_class=="260132"

list id_class if s62=="倒数第三排"
list t21_c if id_class=="230941"
replace s62="6" if s62=="倒数第三排"&id_class=="230941"
list t21_c if id_class=="260132"
replace s62="5" if s62=="倒数第三排"&id_class=="260132"

list id_class if s62=="倒数第二"
list t21_c if id_class=="210231"
replace s62="7" if s62=="倒数第二"&id_class=="210231"
list t21_c if id_class=="411133"
replace s62="9" if s62=="倒数第二"&id_class=="411133"
list t21_c if id_class=="412141"
replace s62="9" if s62=="倒数第二"&id_class=="412141"
list t21_c if id_class=="453133"
replace s62="9" if s62=="倒数第二"&id_class=="453133"

list id_class if s62=="倒数第二排"
list t21_c if id_class=="210241"
replace s62="7" if s62=="倒数第二排"&id_class=="210241"
list t21_c if id_class=="240431"
replace s62="4" if s62=="倒数第二排"&id_class=="240431"

list id_class if s62=="到1"
list t21_c if id_class=="416141"
replace s62="9" if s62=="到1"
list id_class if s62=="到数第2排"
list t21_c if id_class=="110331"
replace s62="." if s62=="到数第2排"
list id_class if s62=="到后"
list t21_c if id_class=="425131"
replace s62="." if s62=="到后"
list id_class if s62=="到数第2排"
list t21_c if id_class=="110331"
replace s62="." if s62=="到数第2排"
list id_class if s62=="到数第三排"
list t21_c if id_class=="422131"
replace s62="9" if s62=="到数第三排"
list id_class if s62=="到第4排"
list t21_c if id_class=="425131"
replace s62="." if s62=="到第4排"
list id_class if s62=="后1"
list t21_c if id_class=="416141"
replace s62="9" if s62=="后1"

list id_class if s62=="后3排"
list t21_c if id_class=="341141"
replace s62="5" if s62=="后3排"&id_class=="341141"
list t21_c if id_class=="422131"
replace s62="9" if s62=="后3排"&id_class=="422131"

list id_class if s62=="后七排"
list t21_c if id_class=="250242"
replace s62="." if s62=="后七排"
list id_class if s62=="后座第3排"
list t21_c if id_class=="210241"
replace s62="6" if s62=="后座第3排"
list id_class if s62=="后数第一"
list t21_c if id_class=="423143"
replace s62="10" if s62=="后数第一"
list id_class if s62=="后面1排"
list t21_c if id_class=="422131"
replace s62="11" if s62=="后面1排"
list id_class if s62=="后面第3排"
list t21_c if id_class=="422131"
replace s62="9" if s62=="后面第3排"
list id_class if s62=="左边第二排最后"
list t21_c if id_class=="220144"
replace s62="8" if s62=="左边第二排最后"

list id_class if s62=="最后"
list t21_c if id_class=="110441"
replace s62="5" if s62=="最后"&id_class=="110441"
list t21_c if id_class=="110632"
replace s62="5" if s62=="最后"&id_class=="110632"
list t21_c if id_class=="110841"
replace s62="." if s62=="最后"&id_class=="110841"
list t21_c if id_class=="111432"
replace s62="." if s62=="最后"&id_class=="111432"
list t21_c if id_class=="111632"
replace s62="6" if s62=="最后"&id_class=="111632"
list t21_c if id_class=="130241"
replace s62="5" if s62=="最后"&id_class=="130241"
list t21_c if id_class=="130541"
replace s62="6" if s62=="最后"&id_class=="130541"
list t21_c if id_class=="210231"
replace s62="8" if s62=="最后"&id_class=="210231"
list t21_c if id_class=="230141"
replace s62="." if s62=="最后"&id_class=="230141"
list t21_c if id_class=="230234"
replace s62="8" if s62=="最后"&id_class=="230234"
list t21_c if id_class=="230333"
replace s62="8" if s62=="最后"&id_class=="230333"
list t21_c if id_class=="240332"
replace s62="6" if s62=="最后"&id_class=="240332"
list t21_c if id_class=="250131"
replace s62="8" if s62=="最后"&id_class=="250131"
list t21_c if id_class=="412141"
replace s62="10" if s62=="最后"&id_class=="412141"
list t21_c if id_class=="422143"
replace s62="10" if s62=="最后"&id_class=="422143"

list id_class if s62=="最后一排"
list t21_c if id_class=="230632"
replace s62="6" if s62=="最后一排"&id_class=="230632"
list t21_c if id_class=="412131"
replace s62="10" if s62=="最后一排"&id_class=="412131"

list id_class if s62=="最后1排"
list t21_c if id_class=="110232"
replace s62="6" if s62=="最后1排"&id_class=="110232"
list t21_c if id_class=="110632"
replace s62="5" if s62=="最后1排"&id_class=="110632"
list t21_c if id_class=="110641"
replace s62="6" if s62=="最后1排"&id_class=="110641"
list t21_c if id_class=="110841"
replace s62="." if s62=="最后1排"&id_class=="110841"
list t21_c if id_class=="111241"
replace s62="4" if s62=="最后1排"&id_class=="111241"
list t21_c if id_class=="111341"
replace s62="." if s62=="最后1排"&id_class=="111341"
list t21_c if id_class=="160131"
replace s62="7" if s62=="最后1排"&id_class=="160131"
list t21_c if id_class=="355131"
replace s62="9" if s62=="最后1排"&id_class=="355131"
list t21_c if id_class=="412141"
replace s62="10" if s62=="最后1排"&id_class=="412141"
list t21_c if id_class=="413141"
replace s62="8" if s62=="最后1排"&id_class=="413141"
list t21_c if id_class=="415143"
replace s62="8" if s62=="最后1排"&id_class=="415143"
list t21_c if id_class=="416141"
replace s62="9" if s62=="最后1排"&id_class=="416141"
list t21_c if id_class=="421132"
replace s62="9" if s62=="最后1排"&id_class=="421132"
list t21_c if id_class=="422131"
replace s62="11" if s62=="最后1排"&id_class=="422131"
list t21_c if id_class=="422143"
replace s62="10" if s62=="最后1排"&id_class=="422143"
list t21_c if id_class=="423143"
replace s62="10" if s62=="最后1排"&id_class=="423143"
list t21_c if id_class=="444131"
replace s62="7" if s62=="最后1排"&id_class=="444131"

list id_class if s62=="最后一排"
list t21_c if id_class=="110232"
replace s62="6" if s62=="最后一排"&id_class=="110232"
list t21_c if id_class=="110331"
replace s62="." if s62=="最后一排"&id_class=="110331"
list t21_c if id_class=="110432"
replace s62="5" if s62=="最后一排"&id_class=="110432"
list t21_c if id_class=="110441"
replace s62="5" if s62=="最后一排"&id_class=="110441"
list t21_c if id_class=="110641"
replace s62="6" if s62=="最后一排"&id_class=="110641"
list t21_c if id_class=="110943"
replace s62="7" if s62=="最后一排"&id_class=="110943"
list t21_c if id_class=="111141"
replace s62="6" if s62=="最后一排"&id_class=="111141"
list t21_c if id_class=="111632"
replace s62="6" if s62=="最后一排"&id_class=="111632"
list t21_c if id_class=="130233"
replace s62="6" if s62=="最后一排"&id_class=="130233"
list t21_c if id_class=="140331"
replace s62="6" if s62=="最后一排"&id_class=="140331"
list t21_c if id_class=="150141"
replace s62="5" if s62=="最后一排"&id_class=="150141"
list t21_c if id_class=="160142"
replace s62="6" if s62=="最后一排"&id_class=="160142"
list t21_c if id_class=="160241"
replace s62="7" if s62=="最后一排"&id_class=="160241"
list t21_c if id_class=="170131"
replace s62="7" if s62=="最后一排"&id_class=="170131"
list t21_c if id_class=="220232"
replace s62="8" if s62=="最后一排"&id_class=="220232"
list t21_c if id_class=="230731"
replace s62="18" if s62=="最后一排"&id_class=="230731"
list t21_c if id_class=="260132"
replace s62="7" if s62=="最后一排"&id_class=="260132"
list t21_c if id_class=="270133"
replace s62="7" if s62=="最后一排"&id_class=="270133"
list t21_c if id_class=="270144"
replace s62="8" if s62=="最后一排"&id_class=="270144"
list t21_c if id_class=="411133"
replace s62="10" if s62=="最后一排"&id_class=="411133"
list t21_c if id_class=="412141"
replace s62="10" if s62=="最后一排"&id_class=="412141"
list t21_c if id_class=="414131"
replace s62="8" if s62=="最后一排"&id_class=="414131"

list id_class if s62=="最后一排(8)"
list t21_c if id_class=="210241"
replace s62="8" if s62=="最后一排(8)"
list id_class if s62=="最后第二排"
list t21_c if id_class=="416131"
replace s62="9" if s62=="最后第二排"

destring s62, gen(s62_c)
winsor s62_c ,gen(s62_c1) p(0.01) 
order s62_c, after(s62)
order s62_c1,after(s62_c)

tab s62_c //eliminate outliers of rows
list id_class if s62_c==84
list t21_c if id_class=="431142"
replace s62_c=5 if s62_c==84 
list id_class if s62_c==3451
list t21_c if id_class=="451131"
replace s62_c=7 if s62_c==3451
list id_class if s62_c==64
list t21_c if id_class=="230234"
replace s62_c=8 if s62_c==64
list id_class if s62_c==60
list t21_c if id_class=="421132"
replace s62_c=9 if s62_c==60
list id_class if s62_c==53
list t21_c if id_class=="344141"
replace s62_c=7 if s62_c==53
list id_class if s62_c==50
list t21_c if id_class=="160142"
replace s62_c=6 if s62_c==50 & id_class=="160142"
list t21_c if id_class=="210135"
replace s62_c=8 if s62_c==50 & id_class=="210135"
list id_class if s62_c==49
list t21_c if id_class=="230933"
replace s62_c=8 if s62_c==49
list id_class if s62_c==43
list t21_c if id_class=="230442" 
replace s62_c=7 if s62_c==43& id_class=="230442"
list t21_c if id_class=="270133" 
replace s62_c=7 if s62_c==43& id_class=="270133"
list t21_c if id_class=="413141" 
replace s62_c=8 if s62_c==43& id_class=="413141"
list id_class if s62_c==36
list t21_c if id_class=="230234"
replace s62_c=8 if s62_c==36
list id_class if s62_c==34
list t21_c if id_class=="453141"
replace s62_c=10 if s62_c==34
list id_class if s62_c==33
list t21_c if id_class=="111642"
replace s62_c=5 if s62_c==33
list id_class if s62_c==31
list t21_c if id_class=="270144"
replace s62_c=8 if s62_c==31
list id_class if s62_c==29
list t21_c if id_class=="130341"
replace s62_c=7 if s62_c==29
list id_class if s62_c==28
list t21_c if id_class=="111231"
replace s62_c=5 if s62_c==28
list id_class if s62_c==25
list t21_c if id_class=="413141"
list t21_c if id_class=="431131"
replace s62_c=8 if s62_c==25
list id_class if s62_c==20
list t21_c if id_class=="230241"
list t21_c if id_class=="230333"
replace s62_c=8 if s62_c==20
replace s62_c=4 if s62_c==19
replace s62_c=4 if s62_c==12 & id_class=="130332"
list id_class if s62_c==17
list t21_c if id_class=="150141"
replace s62_c=5 if s62_c==17
list id_class if s62_c==16
list t21_c if id_class=="230531"
replace s62_c=7 if s62_c==16
list id_class if s62_c==15
list t21_c if id_class=="230941"    
replace s62_c=8 if s62_c==15& id_class=="230941"
replace s62_c=8 if s62_c==15& id_class=="413141"
replace s62_c=4 if s62_c==15& id_class=="240342"
replace s62_c=7 if s62_c==15& id_class=="342141"
list id_class if s62_c==14
list t21_c if id_class=="230431" 
replace s62_c=7 if s62_c==14& id_class=="230431"
list t21_c if id_class=="230531" 
replace s62_c=7 if s62_c==14& id_class=="230531"
list t21_c if id_class=="411133" 
replace s62_c=10 if s62_c==14& id_class=="411133"
list id_class if s62_c==13
list t21_c if id_class=="210331" 
replace s62_c=6 if s62_c==13 & id_class=="210331"
list t21_c if id_class=="240431" 
replace s62_c=5 if s62_c==13 & id_class=="210331"
list t21_c if id_class=="324131" 
replace s62_c=9 if s62_c==13 & id_class=="324131"
list t21_c if id_class=="433131" 
replace s62_c=6 if s62_c==13 & id_class=="433131"
list id_class if s62_c==12
list t21_c if id_class=="210331" 
replace s62_c=6 if s62_c==12 & id_class=="210331"
list t21_c if id_class=="270133"
replace s62_c=7 if s62_c==12 & id_class=="270133" 
list t21_c if id_class=="322131" 
replace s62_c=6 if s62_c==12 & id_class=="322131"
list t21_c if id_class=="324131" 
replace s62_c=9 if s62_c==12 & id_class=="324131"
list t21_c if id_class=="415132"
replace s62_c=8 if s62_c==12 & id_class=="415132"
replace s62_c=6 if s62_c==18 & id_class=="230731"
replace s62_c=8 if s62_c==18 & id_class=="230941"
replace s62_c=5 if s62_c==13 & id_class=="240431"
replace s62_c=5 if s62_c==6 & id_class=="240431"
replace s62_c=6 if s62_c==11 & id_class=="110232"
replace s62_c=6 if s62_c==11 & id_class=="111432"
list s62_c if id_class=="230333"
replace s62_c=8 if s62_c==11 & id_class=="230333"
list s62_c if id_class=="230632"
replace s62_c=8 if s62_c==11 & id_class=="230632"
replace s62_c=8 if s62_c==10 & id_class=="230632"
list s62_c if id_class=="250232"
replace s62_c=8 if s62_c==11 & id_class=="250232"
replace s62_c=8 if s62_c==10 & id_class=="250232"
list s62_c if id_class=="331131"
replace s62_c=7 if s62_c==11 & id_class=="331131"
list s62_c if id_class=="454141"
replace s62_c=8 if s62_c==11 & id_class=="454141"

//check and revise columns
replace s63="." if s63=="1排"|s63=="??"|s63=="不知道"|s63=="不确定"|s63=="倒"|s63=="右"|s63=="后"|s63=="否"|s63=="左"|s63=="倒数第三"|s63=="后面第3排"|s63=="很后面两打"|s63=="忘了"|s63=="是后面"|s63=="没有"|s63=="第一排 第二排 第三排"|s63=="第列"|s63=="能"
replace s63="1" if s63=="1列2个"|s63=="1组"|s63=="左边上排第1"|s63=="左边的第1排"
replace s63="2" if s63=="左边的向右起第2个"|s63=="2、左边"
replace s63="3" if s63=="1、5"|s63=="3、6"|s63=="现在：3 不确定,还会换坐位的"
replace s63="4" if s63=="4左 5右"|s63=="左4"|s63=="左4列"|s63=="第4列"
replace s63="5" if s63=="4,6"|s63=="5中"|s63=="5组"|s63=="6,4"
replace s63="10" if s63=="4,5,11"
list id_class if s63=="中"
list t22_c if id_class=="130332"
replace s63="." if s63=="中"&id_class=="130332"
list t22_c if id_class=="344132"
replace s63="." if s63=="中"&id_class=="344132"
list t22_c if id_class=="421145"
replace s63="5" if s63=="中"&id_class=="421145"
list t22_c if id_class=="425131"
replace s63="." if s63=="中"&id_class=="425131"

list id_class if s63=="中间"
list t22_c if id_class=="210631"
replace s63="5" if s63=="中间"&id_class=="210631"
list t22_c if id_class=="412141"
replace s63="5" if s63=="中间"&id_class=="412141"
list t22_c if id_class=="422131"
replace s63="5" if s63=="中间"&id_class=="422131"

list id_class if s63=="右4"
list t22_c if id_class=="453133"
replace s63="5" if s63=="右4"

list id_class if s63=="3、右"
list t22_c if id_class=="230431"
replace s63="7" if s63=="3、右"

list id_class if s63=="左边中间"
list t22_c if id_class=="425131"
replace s63="." if s63=="左边中间"

list id_class if s63=="最后"
list t22_c if id_class=="160142"
replace s63="7" if s63=="最后"&id_class=="160142"
list t22_c if id_class=="250131"
replace s63="8" if s63=="最后"&id_class=="250131"
list t22_c if id_class=="422131"
replace s63="9" if s63=="最后"&id_class=="422131"

list id_class if s63=="最后1"
list t22_c if id_class=="416141"
replace s63="8" if s63=="最后1"&id_class=="416141"

list id_class if s63=="最后1列"
list t22_c if id_class=="160131"
replace s63="7" if s63=="最后1列"&id_class=="160131"
list t22_c if id_class=="412141"
replace s63="9" if s63=="最后1列"&id_class=="412141"
list t22_c if id_class=="422143"
replace s63="10" if s63=="最后1列"&id_class=="422143"
list t22_c if id_class=="424141"
replace s63="4" if s63=="最后1列"&id_class=="424141"

list id_class if s63=="最后一列"
list t22_c if id_class=="111432"
replace s63="." if s63=="最后一列"&id_class=="111432"
list t22_c if id_class=="140142"
replace s63="7" if s63=="最后一列"&id_class=="140142"
list t22_c if id_class=="160142"
replace s63="7" if s63=="最后一列"&id_class=="160142"
list t22_c if id_class=="170232"
replace s63="7" if s63=="最后一列"&id_class=="170232"
list t22_c if id_class=="230234"
replace s63="9" if s63=="最后一列"&id_class=="230234"
list t22_c if id_class=="230632"
replace s63="8" if s63=="最后一列"&id_class=="230632"
list t22_c if id_class=="445132"
replace s63="8" if s63=="最后一列"&id_class=="445132"

replace s63="10" if s63=="11"&id_class=="422143"
replace s63="9" if s63=="90"&id_class=="422143"
replace s63="7" if s63=="72"&id_class=="210931"
replace s63="6" if s63=="67"&id_class=="111642"
replace s63="5" if s63=="6"&id_class=="111642"
replace s63="5" if s63=="54"&id_class=="210441"
replace s63="10" if s63=="12"&id_class=="210441"
replace s63="7" if s63=="47"&id_class=="344132"
replace s63="7" if s63=="12"&id_class=="344132"
replace s63="4" if s63=="45"&id_class=="354131"
replace s63="4" if s63=="43"&id_class=="355131"
replace s63="3" if s63=="33"&id_class=="355131"
replace s63="2" if s63=="21"&id_class=="355131"
replace s63="4" if s63=="43"&id_class=="453141"
replace s63="3" if s63=="34"&id_class=="451131"
replace s63="3" if s63=="35"&id_class=="451131"
replace s63="3" if s63=="33"&id_class=="351131"
replace s63="2" if s63=="26"&id_class=="351131"
replace s63="1" if s63=="14"&id_class=="351131"
replace s63="3" if s63=="33"&id_class=="422131"
replace s63="10" if s63=="11"&id_class=="422131"
replace s63="10" if s63=="13"&id_class=="422131"
replace s63="3" if s63=="30"&id_class=="456141"
replace s63="2" if s63=="29"&id_class=="130341"
replace s63="2" if s63=="28"&id_class=="345141"
replace s63="2" if s63=="27"&id_class=="411133"
replace s63="8" if s63=="12"&id_class=="411133"
replace s63="2" if s63=="25"&id_class=="240332"
replace s63="2" if s63=="24"&id_class=="343131"
replace s63="2" if s63=="23"&id_class=="421132"
replace s63="2" if s63=="22"&id_class=="260241"
replace s63="2" if s63=="20"&id_class=="210231"
replace s63="2" if s63=="20"&id_class=="260132"
replace s63="9" if s63=="18"&id_class=="210735"
replace s63="9" if s63=="18"&id_class=="353131"
replace s63="7" if s63=="17"&id_class=="230131"
replace s63="7" if s63=="17"&id_class=="230431"
replace s63="9" if s63=="14"&id_class=="230531"
replace s63="9" if s63=="16"&id_class=="230531"
replace s63="9" if s63=="10"&id_class=="230531"
replace s63="6" if s63=="16"&id_class=="240441"
replace s63="10" if s63=="16"&id_class=="333131"
replace s63="8" if s63=="16"&id_class=="445132"
replace s63="12" if s63=="15"&id_class=="250338"
replace s63="8" if s63=="15"&id_class=="414131"
replace s63="7" if s63=="14"&id_class=="140242"
replace s63="4" if s63=="14"&id_class=="170143"
replace s63="8" if s63=="14"&id_class=="270133"
replace s63="8" if s63=="14"&id_class=="341131"
replace s63="8" if s63=="13"&id_class=="413141"
replace s63="8" if s63=="13"&id_class=="211031"
replace s63="10" if s63=="13"&id_class=="421145"
replace s63="8" if s63=="12"&id_class=="230731"
replace s63="8" if s63=="11"&id_class=="230731"  
replace s63="10" if s63=="12"&id_class=="423143"
replace s63="10" if s63=="11"&id_class=="423143"
replace s63="7" if s63=="12"&id_class=="443131"
replace s63="8" if s63=="11"&id_class=="220242"

destring s63, gen(s63_c)
winsor s63_c ,gen(s63_c1) p(0.01) 
order s63_c, after(s63)
order s63_c1,after(s63_c)

keep id s62 s62_c s62_c1 s63 s63_c s63_c1
save "D:\lsq\CCAP_CAU_student\dtanew\location_2017.dta"

**check and revise the peer's ID number
use "D:\lsq\CCAP_CAU_student\dtanew\final.dta", clear
keep id year mainteacher name g32 g33
keep if year==2018
 
list if mainteacher=="董芳" & g33=="" //check by class
replace g33="11063207" if g32=="11063207 徐晓雨" 
list if mainteacher=="陈然" & g33==""
replace g33="11093317" if g32=="郭佳奥" & mainteacher=="陈然"
list if mainteacher=="高敏" & g33==""
replace g33="11094385" if g32=="张盼盼" & mainteacher=="高敏"
list if mainteacher=="胥金霞" & g33==""
replace g33="11134197" if g32=="田二强" & mainteacher=="胥金霞"
replace g33="11134198" if g32=="詹传坤" & mainteacher=="胥金霞"
replace g33="11134128" if g32=="叶舒晴" & mainteacher=="胥金霞"
list if mainteacher=="李贵民" & g33==""
replace g33="13024198" if g32=="张晓慧" & mainteacher=="李贵民"
list if mainteacher=="张智勇" & g33==""
replace g33="13053227" if g32=="洪梦瑶" & mainteacher=="张智勇"
list if mainteacher=="石敬" & g33==""
replace g33="14023218" if g32=="邵文阳" & mainteacher=="石敬"
replace g33="14023291" if g32=="高佳怡" & mainteacher=="石敬"
replace g33="14023294" if g32=="李航" & mainteacher=="石敬"
replace g33="14023297" if g32=="吕哲" & mainteacher=="石敬"
replace g33="14023298" if g32=="宁志新" & mainteacher=="石敬"
replace g33="14023299" if g32=="贾冰娴" & mainteacher=="石敬"
list if mainteacher=="邢咏雪" & g33==""
replace g33="14034111" if g32=="张顺豪" & mainteacher=="邢咏雪"
list if mainteacher=="张旋" & g33==""
replace g33="17023205" if g32=="刘浩然" & mainteacher=="张旋"
list if mainteacher=="吴丹" & g33==""
replace g33="17023230" if g32=="张国傲" & mainteacher=="吴丹"
replace g33="17023224" if g32=="骆诚" & mainteacher=="吴丹"
replace g33="17023203" if g32=="李宇航" & mainteacher=="吴丹"
list if mainteacher=="丁捷" & g33==""
replace g33="21013513" if g32=="马杰" & mainteacher=="丁捷"
replace g33="21013529" if g32=="周芯瑶" & mainteacher=="丁捷"
replace g33="21013537" if g32=="于家豪" & mainteacher=="丁捷"
list if mainteacher=="刘敏" & g33==""
replace g33="21014108" if g32=="王冰沁" & mainteacher=="刘敏"
replace g33="21014118" if g32=="张欣怡" & mainteacher=="刘敏"
list if mainteacher=="潘忆洁" & g33==""
replace g33="21023176" if g32=="李宁宁" & mainteacher=="潘忆洁"
replace g33="21023131" if g32=="王泽旭" & mainteacher=="潘忆洁"
replace g33="21023125" if g32=="谢嘉成" & mainteacher=="潘忆洁"
list if mainteacher=="周慧" & g33==""
replace g33="21033150" if g32=="徐君毅" & mainteacher=="周慧"
list if mainteacher=="陈蓓" & g33==""
replace g33="21073536" if g32=="耿新月" & mainteacher=="陈蓓"
list if mainteacher=="许令华" & g33=="" 
replace g33="21084220" if g32=="万杨梅" & mainteacher=="许令华"
replace g33="21084215" if g32=="潘思衡" & mainteacher=="许令华"
list if mainteacher=="罗慧敏" & g33==""
replace g33="21103168" if g32=="李鹏博" & mainteacher=="罗慧敏"
list if mainteacher=="程洁" & g33==""
replace g33="23013110" if g32=="王道远" & mainteacher=="程洁"
list if mainteacher=="蒋媛媛" & g33==""
replace g33="23014132" if g32=="徐美娟" & mainteacher=="蒋媛媛"
replace g33="23014166" if g32=="刘佳瑞" & mainteacher=="蒋媛媛"
list if mainteacher=="刘瑞" & g33==""
replace g33="23023459" if g32=="颜梦琪" & mainteacher=="刘瑞"
replace g33="23023425" if g32=="刘子甲" & mainteacher=="刘瑞"
replace g33="23023416" if g32=="赵悦彤" & mainteacher=="刘瑞"
replace g33="23023444" if g32=="段承浩" & mainteacher=="刘瑞"
replace g33="23023482" if g32=="段苏芝" & mainteacher=="刘瑞"
replace g33="23023411" if g32=="王鸿运" & mainteacher=="刘瑞"
replace g33="23023429" if g32=="钟心怡" & mainteacher=="刘瑞"
list if mainteacher=="左宝勤" & g33==""
replace g33="23024111" if g32=="刘甜馨" & mainteacher=="左宝勤"
replace g33="23024104" if g32=="史美佳" & mainteacher=="左宝勤"
replace g33="23024123" if g32=="赵毅" & mainteacher=="左宝勤"
replace g33="23024157" if g32=="胡志勇" & mainteacher=="左宝勤"
replace g33="23024154" if g32=="陶金辉" & mainteacher=="左宝勤"
replace g33="23024162" if g32=="李雨涵" & mainteacher=="左宝勤"
replace g33="23024133" if g32=="符锦睿" & mainteacher=="左宝勤"
replace g33="23024143" if g32=="杜雲雲" & mainteacher=="左宝勤"
replace g33="23024134" if g32=="江奇勋" & mainteacher=="左宝勤"
replace g33="23024137" if g32=="黄慧" & mainteacher=="左宝勤"
replace g33="23024149" if g32=="李怡航" & mainteacher=="左宝勤"
replace g33="23024173" if g32=="张文胜" & mainteacher=="左宝勤"
replace g33="23024117" if g32=="王佳乐" & mainteacher=="左宝勤"
replace g33="23024127" if g32=="张雨洁" & mainteacher=="左宝勤"
replace g33="23024120" if g32=="侯晓晓" & mainteacher=="左宝勤"
replace g33="23024126" if g32=="沈瑶佳" & mainteacher=="左宝勤"
replace g33="23024107" if g32=="王韬" & mainteacher=="左宝勤"
replace g33="23024112" if g32=="谢朱庆" & mainteacher=="左宝勤"
replace g33="23024113" if g32=="周晓蝶" & mainteacher=="左宝勤"
list if mainteacher=="孙玲" & g33==""
replace g33="23033344" if g32=="徐鹏鹏" & mainteacher=="孙玲"
replace g33="23033354" if g32=="王紫娴" & mainteacher=="孙玲"
replace g33="23033365" if g32=="郑玉凤" & mainteacher=="孙玲"
replace g33="23033310" if g32=="刘佳辉" & mainteacher=="孙玲"
replace g33="23033319" if g32=="康凯" & mainteacher=="孙玲"
replace g33="23033335" if g32=="汪凤余" & mainteacher=="孙玲"
replace g33="23033347" if g32=="杜子豪" & mainteacher=="孙玲"
replace g33="23033374" if g32=="高奇" & mainteacher=="孙玲"
replace g33="23033311" if g32=="李可可" & mainteacher=="孙玲"
replace g33="23033307" if g32=="高强乐" & mainteacher=="孙玲"
replace g33="23033332" if g32=="郑金凤" & mainteacher=="孙玲"
replace g33="23033377" if g32=="汪小雪" & mainteacher=="孙玲"
replace g33="23033361" if g32=="王宇" & mainteacher=="孙玲"
replace g33="23033373" if g32=="郑娜娜" & mainteacher=="孙玲"
list if mainteacher=="马勤" & g33==""
replace g33="23044268" if g32=="王子俊" & mainteacher=="马勤"
replace g33="23044240" if g32=="徐阳" & mainteacher=="马勤"
replace g33="23044221" if g32=="刘子豪" & mainteacher=="马勤"
replace g33="23044273" if g32=="徐浩" & mainteacher=="马勤"
list if mainteacher=="陈芹" & g33==""
replace g33="23084125" if g32=="孙杰" & mainteacher=="陈芹"
list if g33=="" in 2332/2375
replace g33="23063214" if g32=="吴紫嫣" in 2332/2375
replace g33="23063216" if g32=="马树婷" in 2332/2375
replace g33="23063249" if g32=="陈君如" in 2332/2375
replace g33="23063255" if g32=="许琪琪" in 2332/2375
replace g33="23063205" if g32=="刘雪婷" in 2332/2375
replace g33="23063248" if g32=="刘苏琦" in 2332/2375
replace g33="23063227" if g32=="马红军" in 2332/2375
replace g33="23063250" if g32=="万波文" in 2332/2375
replace g33="23063217" if g32=="孙梦瑶" in 2332/2375
replace g33="23063242" if g32=="戚浩冉" in 2332/2375
replace g33="23063232" if g32=="黄天凤" in 2332/2375
replace g33="23063239" if g32=="郭子阳" in 2332/2375
replace g33="23063238" if g32=="侯浩哲" in 2332/2375
replace g33="23063235" if g32=="彭鸿雨" in 2332/2375
replace g33="23063210" if g32=="李宇轩" in 2332/2375
replace g33="23063211" if g32=="侯安娜" in 2332/2375
replace g33="23063223" if g32=="黄裕龙" in 2332/2375
replace g33="23063226" if g32=="王雪晨" in 2332/2375
replace g33="23063246" if g32=="董可欣" in 2332/2375
list if mainteacher=="张敏" & g33==""
replace g33="23064125" if g32=="陈佳怡" & mainteacher=="张敏"
list if mainteacher=="李正兵" & g33==""
replace g33="23073181" if g32=="史星辉" & mainteacher=="李正兵"
replace g33="23073162" if g32=="刘家贤" & mainteacher=="李正兵"
replace g33="23073163" if g32=="朱日明" & mainteacher=="李正兵"
list if mainteacher=="徐颖" & g33==""
replace g33="23094149" if g32=="高少博" & mainteacher=="徐颖"
replace g33="23094183" if g32=="曹文俊" & mainteacher=="徐颖"
list if mainteacher=="赵虹" & g33==""
replace g33="24044131" if g32=="周星博" & mainteacher=="赵虹"
list if mainteacher=="章霞" & g33==""
replace g33="25023221" if g32=="张博超" & mainteacher=="章霞"
list if mainteacher=="赵茂达" & g33==""
replace g33="25024207" if g32=="龚松涛" & mainteacher=="赵茂达"
list if mainteacher=="梁贵" & g33==""
replace g33="26013230" if g32=="严斌" & mainteacher=="梁贵"
list if g33=="" in 3166/3208
replace g33="26013230" if g32=="邓涵" in 3166/3208
list if mainteacher=="张盼" & g33=="" 
replace g33="27013329" if g32=="薛英杰" & mainteacher=="张盼"
list if mainteacher=="丁珊珊" & g33==""
replace g33="27014457" if g32=="李小满" & mainteacher=="丁珊珊"
list if mainteacher=="蔡舒" & g33==""
replace g33="31213137" if g32=="张浩威" & mainteacher=="蔡舒"
replace g33="31213141" if g32=="张帅" & mainteacher=="蔡舒"
replace g33="31213113" if g32=="王博洋" & mainteacher=="蔡舒"
list if mainteacher=="程敏" & g33==""
replace g33="31214206" if g32=="熊新定" & mainteacher=="程敏"
list if mainteacher=="李梅" & g33==""
replace g33="31214226" if g32=="张浩" & mainteacher=="李梅"
replace g33="31214217" if g32=="刘文宇" & mainteacher=="李梅"
replace g33="31214216" if g32=="杨波" & mainteacher=="李梅"
replace g33="31214230" if g32=="王延浩" & mainteacher=="李梅"
list if mainteacher=="宋晶龄" & g33==""
replace g33="31313229" if g32=="陈天佑,朱明轩" & mainteacher=="宋晶龄"
list if g33=="" & mainteacher=="" in 3528/3610
replace g33="31214206" if g32=="熊新定" in 3528/3610
replace g33="31214211" if g32=="张玉婷" in 3528/3610
replace g33="31214256" if g32=="张雨欣" in 3528/3610
replace g33="31313229" if g32=="陈天佑" in 3528/3610
replace g33="31214247" if g32=="王慧" in 3528/3610
replace g33="31214221" if g32=="高姗姗" in 3528/3610
replace g33="31313236" if g32=="陈晓雅" in 3528/3610
replace g33="31313238" if g32=="刘文博" in 3528/3610
replace g33="31313246" if g32=="沈奥琪" in 3528/3610
replace g33="31313243" if g32=="朱明轩" in 3528/3610
replace g33="31313233" if g32=="梁明珠" in 3528/3610
replace g33="31313221" if g32=="方梦" in 3528/3610
replace g33="31313254" if g32=="吴薇" in 3528/3610
replace g33="31313205" if g32=="沈瑞星" in 3528/3610
replace g33="31313203" if g32=="崔雯雯" in 3528/3610
replace g33="31313245" if g32=="高雅洁" in 3528/3610
list if mainteacher=="王栋" & g33==""
replace g33="31314106" if g32=="陈浩男" & mainteacher=="王栋"
list if mainteacher=="周峰" & g33==""
replace g33="32113280" if g32=="张广政" & mainteacher=="周峰"
replace g33="32113231" if g32=="王来军" & mainteacher=="周峰"
replace g33="32113207" if g32=="黄欣悦" & mainteacher=="周峰"
replace g33="32113212" if g32=="朱怡" & mainteacher=="周峰"
list if mainteacher=="赵莉" & g33==""
replace g33="32114239" if g32=="许涵" & mainteacher=="赵莉"
replace g33="32114270" if g32=="罗宁" & mainteacher=="赵莉"
replace g33="32114217" if g32=="罗翔" & mainteacher=="赵莉"
list if mainteacher=="何伟龙" & g33=="" 
replace g33="32213134" if g32=="杜天佑" & mainteacher=="何伟龙"
replace g33="32213114" if g32=="王云飞" & mainteacher=="何伟龙"
replace g33="32213151" if g32=="魏星宇" & mainteacher=="何伟龙"
replace g33="32213117" if g32=="陈庆朗" & mainteacher=="何伟龙"
replace g33="32213119" if g32=="余孝甜" & mainteacher=="何伟龙"
list if mainteacher=="刘培柱" & g33==""
replace g33="32214210" if g32=="张德宇" & mainteacher=="刘培柱"
replace g33="32214232" if g32=="王乐" & mainteacher=="刘培柱"
replace g33="32214218" if g32=="陈庆奥" & mainteacher=="刘培柱"
list if mainteacher=="张荣娟" & g33==""
replace g33="32314236" if g32=="王后帅" & mainteacher=="刘培柱"
replace g33="32314228" if g32=="杜友谊" & mainteacher=="刘培柱"
replace g33="32314208" if g32=="翟付豪" & mainteacher=="刘培柱"
replace g33="32314232" if g32=="周如辉" & mainteacher=="刘培柱"
replace g33="32314233" if g32=="毕雪蕊" & mainteacher=="刘培柱"
replace g33="32314221" if g32=="储士月" & mainteacher=="刘培柱"
replace g33="32314201" if g32=="沈守豪" & mainteacher=="刘培柱"
replace g33="32314252" if g32=="黄国涛" & mainteacher=="刘培柱"
replace g33="32314245" if g32=="李孝文" & mainteacher=="刘培柱"
replace g33="32314271" if g32=="谢辉贤" & mainteacher=="刘培柱"
replace g33="32314270" if g32=="王磊" & mainteacher=="刘培柱"
list if mainteacher=="徐厚军" & g33==""
replace g33="32614224" if g32=="陈珂悦" & mainteacher=="徐厚军"
list if mainteacher=="耿丹丹" & g33==""
replace g33="33114172" if g32=="王亮" & mainteacher=="耿丹丹"
list if mainteacher=="李万里" & g33==""
replace g33="34114104" if g32=="王少文" & mainteacher=="李万里"
replace g33="34114113" if g32=="姜硕凡" & mainteacher=="李万里"
replace g33="34114108" if g32=="姜梦雪" & mainteacher=="李万里"
replace g33="34114145" if g32=="高甜甜" & mainteacher=="李万里"
replace g33="34114145" if g32=="王少文" in 5213/5242
list if g33=="" & mainteacher=="" in 5213/5242
replace g33="34114156" if g32=="姜慧" in 5213/5242
replace g33="34114135" if g32=="陈若兰" in 5213/5242
replace g33="34114148" if g32=="王天乐" in 5213/5242
replace g33="34114127" if g32=="伊鑫诺" in 5213/5242
replace g33="34114155" if g32=="刘祥" in 5213/5242
list if mainteacher=="田嘉敏" & g33==""
replace g33="34214156" if g32=="汪振友" & mainteacher=="田嘉敏"
list if mainteacher=="冯素莲" & g33==""
replace g33="34313115" if g32=="丁山耑雪" & mainteacher=="冯素莲"
list if mainteacher=="郭海" & g33==""
replace g33="34314114" if g32=="刘启迪" & mainteacher=="郭海"
list if mainteacher=="张芳勤" & g33==""
replace g33="34413210" if g32=="高浩月" & mainteacher=="张芳勤"
replace g33="34413203" if g32=="梁文慧" & mainteacher=="张芳勤"
replace g33="34413243" if g32=="于浩冉" & mainteacher=="张芳勤"
replace g33="34413218" if g32=="马泽洋" & mainteacher=="张芳勤"
replace g33="34413208" if g32=="王运" & mainteacher=="张芳勤"
replace g33="34413214" if g32=="付翔" & mainteacher=="张芳勤"
list if mainteacher=="付老师" & g33==""
replace g33="34414115" if g32=="张迪迪" & mainteacher=="付老师"
list if mainteacher=="刘老师" & g33==""
replace g33="34414112" if g32=="张雨豪" & mainteacher=="刘老师"
list if g33=="" & mainteacher=="" in 5699/5735
replace g33="35114126" if g32=="康晶晶" in 5699/5735
list if mainteacher=="姚文春" & g33=="" | mainteacher=="姚老师" & g33==""
replace g33="35313148" if g32=="孙家辉" & mainteacher=="姚文春" | g32=="孙家辉" & mainteacher=="姚老师"
replace g33="35313120" if g32=="徐红旺" & mainteacher=="姚文春" | g32=="徐红旺" & mainteacher=="姚老师"
list if mainteacher=="汤全文" & g33==""
replace g33="35513117" if g32=="李心辰" & mainteacher=="汤全文"
replace g33="35513156" if g32=="李博" & mainteacher=="汤全文"
list if mainteacher=="王梅梅" & g33==""
replace g33="35514160" if g32=="柳豪" & mainteacher=="王梅梅"
list if mainteacher=="张越" & g33==""
replace g33="41513281" if g32=="周越" & mainteacher=="张越"
list if mainteacher=="王新峰" & g33==""
replace g33="41514307" if g32=="陈悦" & mainteacher=="王新峰"
list if g33=="" & mainteacher=="" in 6910/7050
replace g33="42113248" if g32=="项雨晨" in 6910/7050
replace g33="42113231" if g32=="张雨梦" in 6910/7050
replace g33="42113294" if g32=="陈念念" in 6910/7050
replace g33="42113256" if g32=="李璐" in 6910/7050
replace g33="42113230" if g32=="余汉雯" in 6910/7050
replace g33="42113291" if g32=="魏研" in 6910/7050
replace g33="42113267" if g32=="冯紫琰" in 6910/7050
replace g33="42113281" if g32=="宗傲红" in 6910/7050
replace g33="42113283" if g32=="李玲" in 6910/7050
replace g33="42113274" if g32=="陈奥运" in 6910/7050
replace g33="42114562" if g32=="李雪飞" in 6910/7050
replace g33="42114565" if g32=="周金凤" in 6910/7050
replace g33="42114537" if g32=="陈莉" in 6910/7050
replace g33="42114560" if g32=="张毅然" in 6910/7050
replace g33="42114577" if g32=="陈茂" in 6910/7050
replace g33="42114531" if g32=="谢豪杰" in 6910/7050
replace g33="42114529" if g32=="张浩然" in 6910/7050
replace g33="42114559" if g32=="汪海涛" in 6910/7050
replace g33="42114502" if g32=="刁宝龙" in 6910/7050
replace g33="42114541" if g32=="陈亿" in 6910/7050
list if mainteacher=="刘文" & g33==""
replace g33="42614109" if g32=="付俊林" & mainteacher=="刘文"
 
//correct answer format
replace g33="" if g33=="没有" |g33=="29 42"|g33=="344132"|g33=="没"|g33=="没得"|g33=="不在"|g33=="太多了,不知道"|g33=="OK"|g33=="本班没有好朋友"|g33=="不知道" |g33=="无" |g33=="不知道什么意思" |g33=="不" |g33=="都不知道" |g33=="空" |g33=="不知道(她没来)"|g33=="他喜欢和我玩"|g33=="它好"|g33=="没有 走了"|g33=="没有 转走"|g32=="温嘉庆"
replace g33="13023318" if g33=="13023318 不知道"
replace g33="14034128" if g33=="安家祥"
replace g33="21013529" if g33=="不知道 21013529"
replace g33="23033321" if g33=="刘家乐"|g32=="刘家乐"
replace g33="23043131" if g33=="23043131 02"
replace g33="31313237" if g33=="杨彩云"
replace g33="34113108" if g33=="姜君月"|g33=="姜君月 34113108"
replace g33="34213130" if g33=="李紫童"
replace g33="23014189" if g32=="李文慧"
replace g33="52016227" if g33=="晏庆铭"
replace g33="55116219" if g33=="金建业"
replace g33="55416121" if g33=="谢雨欣"
replace g33="45614168" if g33=="非??有好友"
replace g33="45614168" if g33=="32号"
replace g33="11093317" if g32=="郭佳奥" & mainteacher=="陈然"
replace g33="51216215" if g32=="普彦霜 闫伟鑫 赵凌霄 时春沂 毕继壹 妥建涛" 
replace g33="51216203" if g32=="普彦霖 张凤斌" | g32=="闫伟鑫 普彦霖 张风斌 妥建涛" | g32=="赵凌霄 张凤斌 许晁 普彦霖" | g32=="张凤斌 毕继壹 受建涛"
replace g33="52015187" if g32=="黄礼科"
replace g33="52015119" if g32=="吕昕洋"
replace g33="52015115" if g32=="侯可欣"
replace g33="52025282" if g32=="张皓 周治伟 杨师"
replace g33="52025229" if g32=="李琦 罗颖"
replace g33="52025232" if g32=="刘冬雪"
replace g33="52215235" if g32=="陆涛"
replace g33="53016116" if g32=="李杨豪"
replace g33="53045132" if g32=="余爱艳"
replace g33="53045107" if g32=="李自蓉"
replace g33="53045110" if g32=="张超凡" & g33==""
replace g33="53045138" if g32=="陈艳"
replace g33="54215111" if g32=="史亚军"
replace g33="54215129" if g32=="杨文丽"
replace g33="54215104" if g32=="王秀英"
replace g33="55116206" if g32=="王娇"
replace g33="52025212" if g32=="珠玥璇 刘坤程 余梓棋"
replace g33="42113256" if g32=="李璐" | g32=="李璐 张梦楠"
replace g33="21023131" if g32=="王泽旭"
replace g33="41514312" if g32=="郭帅"
replace g33="21023107" if g32=="李俊希"
replace g33="34114156" if g32=="姜慧"
replace g33="35413116" if g32=="周毅??"
replace g33="23043126" if g32=="23043126"
replace g33="23063108" if g32=="23063108"
replace g33="23074176" if g32=="23074176"
replace g33="23074138" if g32=="23074138"
replace g33="31413248" if g32=="31413248"
replace g33="3253112" if g32=="3253112"
replace g33="35413110" if g32=="35413110"
replace g33="18423014" if g32=="18423014" 
replace g33="13023318" if g33=="13023318 不知道" 
replace g33="31313237" if g33=="我们写(回来后要做的工作)" 
replace g33="14034111" if g33=="140341111" 
replace g33="11113284" if g33=="111132" 
replace g33="14024228" if g33=="140224228" 
replace g33="21013529" if g33=="29" & g32=="周芯瑶"
replace g33="" if g33=="33" & g32=="贾博麟"
replace g33="" if g33=="72" & g32=="张"
replace g33="21064186" if g33=="86" & g32=="温家城"
replace g33="" if g33=="08" & g32=="陆君洛"
replace g33="21084255" if g33=="55" & g32=="饶子夫"
replace g33="21084213" if g33=="13" & g32=="魏静雯"
replace g33="21084269" if g33=="69" & g32=="宋佳慧"
replace g33="21084225" if g33=="25" & g32=="丁浩宇"
replace g33="22023210" if g33=="20" & g32=="杨成媛"
replace g33="23053106" if g33=="06" & g32=="马博文"
replace g33="24043159" if g33=="59" & g32=="陈俊如"
replace g33="" if g33=="68" & g32=="洪恒圣"
replace g33="" if g33=="79" & g32=="严学阳"
replace g33="32113231" if g33=="31" & g32=="王来军"
replace g33="" if g33=="2" & g32=="王全"
replace g33="41513253" if g33=="53" & g32=="罗兰"
replace g33="35314237" if g33=="10" & g32=="徐想"
replace g33="35314210" if g33=="10" & g32=="王新凌"
replace g33="35613109" if g33=="9" & g32=="陈梦雪"
replace g33="41513235" if g33=="35" & g32=="朱欣冉"
replace g33="41513250" if g33=="50" & g32=="陈荣浩"
replace g33="45513153" if g33=="53" & g32=="黄乐乐"
replace g33="54316123" if g33=="23" & g32=="陈嘉萍"
replace g33="21033131" if g33=="都不知道" & g32=="李瑞 王世桀 贺林风"
replace g33="21024129" if g33=="2102429" 
replace g33="21103124" if g33=="201103124"
replace g33="21033140" if g33=="230140" 
replace g33="21033135" if g33=="210331535" 
replace g33="21043153" if g33=="210453" 
replace g33="21043112" if g33=="1043112" 
replace g33="21063174" if g33=="2106314" 
replace g33="21063138" if g33=="210631" 
replace g33="21063125" if g33=="2103525" 
replace g33="21053815" if g33=="2105382815" 
replace g33="" if g33=="412131103" 
replace g33="21074112" if g33=="2107412" 
replace g33="21084260" if g33=="2108460" 
replace g33="21094607" if g33=="2109467" 
replace g33="32114240" if g33=="3214240" 
replace g33="41214135" if g33=="3514236" 
replace g33="35413113" if g33=="354131313"  
replace g33="41614181" if g33=="4161481" 
replace g33="21103123" if g33=="2101103124" 
replace g33="21104302" if g33=="2110432" 
replace g33="21104303" if g33=="2114303" 
replace g33="21104313" if g33=="2114314" 
replace g33="21104049" if g33=="2110449" 
replace g33="22023235" if g33=="2202335" 
replace g33="22024274" if g33=="2024274" 
replace g33="32112387" if g33=="3211387" 
replace g33="22024219" if g33=="2202419" 
replace g33="22024216" if g33=="2202421" 
replace g33="23013131" if g33=="230131311" 
replace g33="23013143" if g33=="2301311883" 
replace g33="23023473" if g33=="230234783" 
replace g33="35514104" if g33=="355514104" 
replace g33="23043169" if g33=="2195140169" 
replace g33="25023272" if g33=="2502372" 
replace g33="25023270" if g33=="2502370" 
replace g33="25023275" if g33=="2502375" 
replace g33="25013153" if g33=="2501353" 
replace g33="25023254" if g33=="250232254" 
replace g33="25023217" if g33=="250243217" 
replace g33="26024150" if g33=="2604150" 
replace g33="26023161" if g33=="260233161"
replace g33="23054163" if g33=="230541653"
replace g33="23054115" if g33=="230541155" 
replace g33="24033243" if g33=="2403343" 
replace g33="24033206" if g33=="2403306" 
replace g33="24033238" if g33=="4033238" 
replace g33="24033253" if g33=="240332553" 
replace g33="24034257" if g33=="249034257"
replace g33="" if g33=="23035031 42" 
replace g33="27014471" if g33=="270144071" 
replace g33="31113106" if g33=="311106" 
replace g33="31113143" if g33=="31114-3" 
replace g33="32114233" if g33=="3214233" 
replace g33="23074146" if g33=="321131155" 
replace g33="33314137" if g33=="333141137" 
replace g33="" if g33=="4151325823" 
replace g33="31113115" if g33=="311131115" 
replace g33="31514202" if g33=="341514202" 
replace g33="31613117" if g33=="316131171" 
replace g33="31614116" if g33=="3161116" 
replace g33="32414120" if g33=="324144166" 
replace g33="32513137" if g33=="325513137" 
replace g33="32514136" if g33=="325114136" 
replace g33="33113108" if g33=="3313108" 
replace g33="32614251" if g33=="326144251" 
replace g33="34413255" if g33=="3441325" 
replace g33="34214170" if g33=="342170"
replace g33="34413254" if g33=="344132154" 
replace g33="34514105" if g33=="34551405" 
replace g33="35514116" if g33=="3551416" 
replace g33="35614130" if g33=="3561315" 
replace g33="41113362" if g33=="411113362" 
replace g33="41214133" if g33=="4121414633" 
replace g33="41214111" if g33=="412141111" 
replace g33="41413105" if g33=="414131057" 
replace g33="41114114" if g33=="411114114" 
replace g33="41114142" if g33=="411142" 
replace g33="41513219" if g33=="4153219"
replace g33="51025421" if g33=="514025421"
replace g33="53025120" if g33=="5302520"
replace g33="53025129" if g33=="5302529"
replace g33="53025104" if g33=="5302504"
replace g33="53066250" if g33=="5306625"
replace g33="54015103" if g33=="540151003"
replace g33="55015419" if g33=="550154219"
replace g33="55116219" if g33=="55163?9"
replace g33="55416124" if g33=="554161024"
replace g33="45614119" if g33=="456141119"
replace g33="42214354" if g33=="4221354" 
replace g33="43113112" if g33=="433113112" 
replace g33="44114131" if g33=="4414131" 
replace g33="44214108" if g33=="4421411708"
replace g33="44414101" if g33=="4441401" 
replace g33="44414124" if g33=="4414124" 
replace g33="44513229" if g33=="4451329"
replace g33="45113107" if g33=="451131107"
replace g33="45113198" if g33=="4511398" 
replace g33="45213225" if g33=="4521325" 
replace g33="45313342" if g33=="453133" 
replace g33="45413148" if g33=="4541378" 
replace g33="45413161" if g33=="44131" 
replace g33="45414150" if g33=="415414150" 
replace g33="45613117" if g33=="456131" 
replace g33="45613110" if g33=="456131" 
replace g33="" if g33=="456141116" 
replace g33="" if g33=="456141121" 
replace g33="45614118" if g33=="456141108" 
replace g33="51025447" if g33=="5105447" 
replace g33="" if g33=="5102515" 
replace g33="51315241" if g33=="5131521241" 
replace g33="51315229" if g33=="5131529" 
replace g33="51415121" if g33=="5141521" 
replace g33="52015160" if g33=="520151" 
replace g33="52015118" if g33=="5201518" 
replace g33="52015156" if g33=="5201552" 
replace g33="52015452" if g33=="520151520" 
replace g33="52015104" if g33=="5201514" 
replace g33="52016237" if g33=="5201637" 
replace g33="52025250" if g33=="520252050" 
replace g33="52036209" if g33=="5203629" 
replace g33="53015113" if g33=="530151013" 
replace g33="53016110" if g33=="5301610" 
replace g33="53025118" if g33=="5302518" 
replace g33="53025117" if g33=="5302517" 
replace g33="53025148" if g33=="5302548" 
replace g33="53025114" if g33=="5302514" 
replace g33="53025137" if g33=="5302504" 
replace g33="53025136" if g33=="5302504"  
replace g33="53025135" if g33=="5302535" 
replace g33="53025126" if g33=="5302526" 
replace g33="53025146" if g33=="5302548" 
replace g33="53065110" if g33=="5306510"  
replace g33="53065110" if g33=="530065110" 
replace g33="53066205" if g33=="530660205"  
replace g33="54015105" if g33=="540151" 
replace g33="54026217" if g33=="5401610817" 
replace g33="54026217" if g33=="540217" 
replace g33="54115130" if g33=="5411512030" 
replace g33="" if g33=="5411611" 
replace g33="55416121" if g33=="55416121?" 
replace g33="45613100" if g33=="456131100" 
replace g33="44413119" if g33=="4441319" 
replace g33="44314124" if g33=="4431424" 
replace g33="24034201" if g33=="240334201" 
replace g33="23094174" if g33=="2309474" 
replace g33="" if g33=="04" 
 
replace g33="11014328" if g33=="1,101,432,811,024,320" //only keep one best friend
replace g33="21083220" if g33=="21083220,21083273"
replace g33="25024272" if g33=="25024272,25024252"
replace g33="23044250" if g33=="23044250,23044222"
replace g33="41114102" if g33=="411141 41114102"
replace g33="41114164" if g33=="41114164 41114112"
replace g33="41114150" if g33=="41114150 41114152"
replace g33="41114150" if g33=="41114128,41114144"
replace g33="41114150" if g33=="41114183,41114129"
replace g33="42313143" if g33=="42313143(转走今日无他)"
replace g33="42313176" if g33=="42313176(已转走)"
replace g33="42314368" if g33=="42314368、42314342"
replace g33="42314301" if g33=="42314368、42314301" 
replace g33="42314326" if g33=="42314326、42314301"
replace g33="44114125" if g33=="44114125 44114117"
replace g33="44313122" if g33=="44313122 44313152"
replace g33="44313119" if g33=="44313119 44313148"
replace g33="42314301" if g33=="01 42314301" 
replace g33="44313119" if g33=="01 44313119" 
replace g33="44313147" if g33=="44313147 44313144"
replace g33="44313128" if g33=="44313128 44313147"
replace g33="44313128" if g33=="44313128 44313144"
replace g33="44314124" if g33=="44314124 44314140"
replace g33="44314137" if g33=="44314137 44314116"
replace g33="44314148" if g33=="44314148 44314128"
replace g33="44314110" if g33=="44314110 44314116"
replace g33="44314103" if g33=="44314103 44314108"
replace g33="44314125" if g33=="44314125 44314111"
replace g33="44413113" if g33=="44413113 44413114"
replace g33="44413123" if g33=="44413123 44413124"
replace g33="44413114" if g33=="44413114 44413103"
replace g33="44413106" if g33=="44413106 44413132"
replace g33="44413124" if g33=="44413124 44413104"
replace g33="44413145" if g33=="44413145 44413104"
replace g33="44413106" if g33=="44413106 44413121"
replace g33="44413130" if g33=="44413130 44413125"
replace g33="44413142" if g33=="44413142 44413122"
replace g33="44414147" if g33=="44414147 44414161"
replace g33="44414126" if g33=="44414126 44414110"
replace g33="44414115" if g33=="44414115 44414141"
replace g33="44414108" if g33=="44414108 44414103"
replace g33="44514131" if g33=="44514131 44514135"
replace g33="44514104" if g33=="44514104 44514111"
replace g33="44514136" if g33=="44514136 44514148"
replace g33="45113171" if g33=="45113171 45113169"
replace g33="45113171" if g33=="45113171 45113166"
replace g33="45113183" if g33=="45113183 45113118"
replace g33="45113166" if g33=="45113166 45113169"
replace g33="45314174" if g33=="45314174 45314176"
replace g33="45413135" if g33=="45413135号"
replace g33="45514119" if g33=="45514119、45514151、45514130"
replace g33="51026453" if g33=="51026453 51026450"
replace g33="51216237" if g33=="51216237 51216230"
replace g33="51216220" if g33=="51216220 51216227"
replace g33="51216227" if g33=="51216227 51216218"
replace g33="51216220" if g33=="51216220 51216218"
replace g33="54036123" if g33=="54036123 54036129"
replace g33="45113130" if g33=="45113130、45113197"
replace g33="21034128" if g33=="2103410128"
replace g33="33113120" if g33=="3313120"
replace g33="33114173" if g33=="331141703"
 
destring g33, gen(g33_c) force
recast long g33_c
g g33_c1=strpos(g33,",")!=0 //all have only one best friend√
save "D:\lsq\CCAP_CAU_student\dtanew\peerid(2018).dta",replace

use "D:\lsq\CCAP_CAU_student\dtanew\final.dta",clear //substitute the peer ID into the final data set
keep if year==2018 
drop g33_c
merge 1:1 id using "D:\lsq\CCAP_CAU_student\dtanew\peerid(2018).dta"
drop _merge
order g33_c, after(g33)
order g33_c1, after(g33_c)
save "D:\lsq\CCAP_CAU_student\dtanew\final1.dta",replace

use "D:\lsq\CCAP_CAU_student\dtanew\student1.dta",clear //append two years' data sets
append using "D:\lsq\CCAP_CAU_student\dtanew\final1.dta",force
sort id year
order esteem1, after(mathscore)
order esteem2, after(esteem1)
order grit1, after(esteem2)
save "D:\lsq\CCAP_CAU_student\dtanew\final(new).dta",replace

**adjust the school's final scores for children
use "D:\lsq\CCAP_CAU_student\dtanew\final(new).dta",clear
replace g24a = "." if g24a == "#不知"|g24a == "?" |g24a == "??" |g24a == "a" |g24a == "不" |g24a == "不太清楚" |g24a == "不明" |g24a == "大约" |g24a == "不清楚"|g24a == "不清除"|g24a == "不知"|g24a == "不知道"|g24a == "不知到"|g24a == "不记得"|g24a == "不记的"|g24a == "不识"|g24a == "大??"|g24a == "大概"|g24a == "完全不知道"|g24a == "忘"|g24a == "忘了" |g24a == "忘记"|g24a == "忘记了" |g24a == "无"|g24a == "无信息"|g24a == "无成绩"|g24a == "望了"|g24a == "未"|g24a == "没有"|g24a == "没有考"|g24a == "没考"|g24a == "秘密"|g24a == "空"|g24a == "缺"|g24a == "缺考"|g24a == "记不清"|g24a == "请假"|g24a == "不之"|g24a == "不几格"|g24a == "不积格"|g24a == "不能说"|g24a == "不记得了"|g24a == "否"|g24a == "大概考了360多分"|g24a == "完了"|g24a == "有??"|g24a == "望记"|g24a == "未考"|g24a == "语"|g24a == "15703871358 语文"|g24a == "999"                                                                                 
replace g24a = "1.5" if g24a == "1、2"|g24a == "1,2" //adjust the Chinese score
replace g24a = "5" if g24a == "5以下"
replace g24a = "20" if g24a == "20多"|g24a == "20多分"
replace g24a = "25" if g24a == "18、32" 
replace g24a = "27" if g24a == "大约27"
replace g24a = "27.5" if g24a == "15到40" 
replace g24a = "30.3" if g24a == "23、38、30"
replace g24a = "30" if g24a == "30+"|g24a == "30:00"|g24a == "30几"|g24a == "30多"
replace g24a = "33.5" if g24a == "32、35"
replace g24a = "40" if g24a == "40多"|g24a == "40多分" 
replace g24a = "44" if g24a == "44多点"
replace g24a = "45" if g24a == "40 50" 
replace g24a = "46" if g24a == "大既46"
replace g24a = "46.75" if g24a == "26 30 50 81"
replace g24a = "50" if g24a == "50多"|g24a == "50以上"|g24a == "50多分"|g24a == "大约50多"
replace g24a = "52.5" if g24a == "大概45~60"
replace g24a = "55" if g24a == "50-60"|g24a == "50~60几" 
replace g24a = "55.5" if g24a == "65、45"
replace g24a = "59" if g24a == "不及格"|g24a == "不过" 
replace g24a = "60" if g24a == "54~66"|g24a == "60以上" |g24a == "60几" |g24a == "60多分" |g24a == "60多"|g24a == "不 60"|g24a == "不60"|g24a == "大概60"|g24a == "大约60分"|g24a == "60分以上"
replace g24a = "61" if g24a == "大约61"
replace g24a = "63" if g24a == "60~66"
replace g24a = "65" if g24a == "60-70多分" |g24a == "60或70"|g24a == "60 70"
replace g24a = "67.5" if g24a == "60-75"
replace g24a = "69" if g24a == "六十九分"|g24a == "大约96"
replace g24a = "70" if g24a == "60-80"|g24a == "70以上"|g24a == "70分以上"|g24a == "70多"|g24a == "七十多"|g24a == "七十几"|g24a == "大概七十多"|g24a == "大概70多"|g24a == "大约70"|g24a == "70多分"
replace g24a = "71" if g24a == "70、71、72"
replace g24a = "74.5" if g24a == "70-79"
replace g24a = "75" if g24a == "60～90" |g24a == "70-80"|g24a == "80-70"|g24a == "大约75"
replace g24a = "78" if g24a == "本期中78"
replace g24a = "80" if g24a == "60-100"|g24a == "80以上"|g24a == "80以内"|g24a == "80分以上"|g24a == "80多"|g24a == "80多分"|g24a == "80左右"|g24a == "八十多分"|g24a == "大概80多" |g24a == "大约80多分"|g24a == "8?"|g24a == "大约80多"|g24a == "大约八十多"|g24a == "70-90"
replace g24a = "81" if g24a == "约81"
replace g24a = "82" if g24a == "大概79-85"
replace g24a = "82.5" if g24a == "80~85"
replace g24a = "84" if g24a == "良"
replace g24a = "83.5" if g24a == "82到85"
replace g24a = "85" if g24a == "80至90"|g24a == "85以上"|g24a == "85分以上"|g24a == "大概85以上"|g24a == "忘了大约=85"|g24a == "85左右"    
replace g24a = "87.5" if g24a == "85-90"
replace g24a = "89" if g24a == "≈89"
replace g24a = "90" if g24a == "90多"|g24a == "90以上"|g24a == "90分以上"|g24a == "90多分"|g24a == "90左右"|g24a == "大概90多"|g24a == "大约90"|g24a == "九十多"|g24a == "约90"|g24a == "约90几"|g24a == "约90多"|g24a == "90+"|g24a == "90几分"   
replace g24a = "91.5" if g24a == "92-91" 
replace g24a = "92" if g24a == "92 好像"|g24a == "大概92"
replace g24a = "93" if g24a == "大约93分"
replace g24a = "94" if g24a == "大约94"
replace g24a = "95" if g24a == "95分以上"|g24a == "95以上"
replace g24a = "96" if g24a == "大约96"|g24a == "大概96"
replace g24a = "97" if g24a == "大约97"
destring g24a, replace
winsor g24a ,gen(g24anew) p(0.01)
order g24anew, after(g24a)

replace g24 = "." if g24 == "??9"|g24 == "?" |g24 == "??" |g24 == "I don't known" |g24 == "NO" |g24 == "b" |g24 == "不" |g24 == "不之" |g24 == "不明" |g24 == "不清楚" |g24 == "不清除" |g24 == "不知"|g24 == "不知到" |g24 == "不知道"|g24 == "不能说" |g24 == "不记得" |g24 == "不记的" |g24 == "否"|g24 == "大??" |g24 == "大概"|g24 == "大约"|g24 == "学数"|g24 == "完了"|g24 == "完全不知道"|g24 == "忘"|g24 == "忘了" |g24 == "忘记"|g24 == "忘记了"|g24 == "我忘了"|g24 == "我转入"|g24 == "新转来" |g24 == "无"|g24 == "无信息"|g24 == "有??"|g24 == "望了"|g24 == "忘记"|g24 == "未"|g24 == "未考"|g24 == "没有"|g24 == "没有考"|g24 == "没考"|g24 == "没领通知单"|g24 == "秘密"|g24 == "空"|g24 == "缺"|g24 == "缺考"|g24 == "记不清"|g24 == "新转入"|g24 == "不记得了"|g24 == "望记"|g24 == "999"|g24 == "13523993059 数学"                  
replace g24 = "3.5" if g24 == "3、4" //adjust the Math score
replace g24 = "4" if g24 == "4以上"
replace g24 = "10" if g24 == "10+"|g24 == "10多"
replace g24 = "20" if g24 == "20多"|g24 == "20多分"|g24 == "15到35"|g24 == "20多分"
replace g24 = "26" if g24 == "31:21" 
replace g24 = "30" if g24 == "30多"|g24 == "30多分"
replace g24 = "30.3" if g24 == "23、38、30"
replace g24 = "35" if g24 == "50、20"
replace g24 = "37" if g24 == "大既37"
replace g24 = "40" if g24 == "40以上"|g24 == "40多分"|g24 == "50 30" |g24 == "大概40多"|g24 == "大约40多"|g24 == "40多"
replace g24 = "45" if g24 == "30-60"
replace g24 = "50" if g24 == "50以上"|g24 == "50多"|g24 == "50多分"
replace g24 = "55" if g24 == "50-60"
replace g24 = "57.5" if g24 == "大概45~70"
replace g24 = "58.5" if g24 == "60、57"
replace g24 = "59" if g24 == "不及格"|g24 == "不过" 
replace g24 = "60" if g24 == "60以上"|g24 == "60几"|g24 == "60多"|g24 == "60多分"|g24 == "50 30"|g24 == "≈60" |g24 == "大概60" 
replace g24 = "62.5" if g24 == "60~65" 
replace g24 = "63" if g24 == "59-67"
replace g24 = "65" if g24 == "60-70"|g24 == "60或70"|g24 == "65分以上"|g24 == "不65"
replace g24 = "70" if g24 == "70以上"|g24 == "70分以上"|g24 == "70多"|g24 == "70左右"|g24 == "七十分"|g24 == "大概70"|g24 == "大概70多"
replace g24 = "72" if g24 == "不 72"
replace g24 = "72.5" if g24 == "70到80"|g24 == "70到75"
replace g24 = "73" if g24 == "大约73"
replace g24 = "75" if g24 == "70-80"|g24 == "70~80以内"|g24 == "70至80"
replace g24 = "76" if g24 == "忘了=76"|g24 == "70~80以内"|g24 == "本期中76"
replace g24 = "76.5" if g24 == "大约76.5"
replace g24 = "77" if g24 == "大约77"
replace g24 = "80" if g24 == "60-100"|g24 == "60～90"|g24 == "80以上"|g24 == "80几"|g24 == "80分以上"|g24 == "80多"|g24 == "80多分"|g24 == "8?"|g24 == "八十几"|g24 == "八十多分" |g24 == "大概80以上"|g24 == "大概80多"|g24 == "大约80多"|g24 == "大约80多分"|g24 == "80几分"|g24 == "80左右"|g25 == "80分以上"|g25 == "80左右"   
replace g24 = "84" if g24 == "大概84"|g24 == "大约84" |g24 == "良"
replace g24 = "85" if g24 == "大概85分"|g24 == "85分以上不到90"|g24 == "大约85分"|g24 == "80~90"
replace g24 = "86" if g24 == "大概86"|g24 == "大约86"|g24 == "86约" 
replace g24 = "86.5" if g24 == "大概83-90"
replace g24 = "87.5" if g24 == "80分以上不到90"|g24 == "85-90"
replace g24 = "88" if g24 == "80-96"
replace g24 = "90" if g24 == "90以上"|g24 == "90分以上"|g24 == "90多"|g24 == "90多分"|g24 == "九十多" |g24 == "大概九十多"|g24 == "大约90"|g24 == "90几"|g24 == "约90多"|g24 == "约90几"|g24 == "90+" 
replace g24 = "90.5" if g24 == "大约90.5"|g24 == "约90"
replace g24 = "91" if g24 == "大概91"|g24 == "92、90"
replace g24 = "92.5" if g24 == "90-95"
replace g24 = "94" if g24 == "大约94"
replace g24 = "95" if g24 == "95左右"|g24 == "约95"|g24 == "95以上"
replace g24 = "96" if g24 == "大约96"
replace g24 = "97" if g24 == "97 好像"
replace g24 = "98" if g24 == "约98"
replace g24 = "101" if g24 == "96+5" //可能是附加题
replace g24 = "102" if g24 == "92+10" 
replace g24 = "103" if g24 == "93+10"
replace g24 = "104" if g24 == "94+10"
replace g24 = "105" if g24 == "95+10"
replace g24 = "106" if g24 == "96+10"
replace g24 = "108" if g24 == "98+10" 
replace g24 = "109" if g24 == "99+10"
replace g24 = "360" if g24 == "大概考了360多分"
destring g24, replace
winsor g24 ,gen(g24new) p(0.01)
order g24new, after(g24)

replace g25 = "." if g25 == "10以上"|g25 == "10月20日"|g25 == "10月20日"|g25 == "?" |g25 == "??"|g25 == "I don't know"|g25 == "I don't known"|g25 == "NO"|g25 == "a"|g25 == "b"|g25 == "c"|g25 == "~"|g25 == "上学还没学英语哪"|g25 == "不"|g25 == "不 没考"|g25 == "不可能"|g25 == "不明"|g25 == "不有考"|g25 == "不清"|g25 == "不清楚"|g25 == "不清除"|g25 == "不知"|g25 == "不知道"|g25 == "不知到"|g25 == "不考"|g25 == "不考英语"|g25 == "不至到"|g25 == "不记得"|g25 == "不记的"|g25 == "不道"|g25 == "从来不考"|g25 == "否"|g25 == "大??"|g25 == "大概"|g25 == "大约"|g25 == "完全不知道"|g25 == "忘"|g25 == "忘了"|g25 == "忘记"|g25 == "忘记了"|g25 == "我忘了"|g25 == "无"|g25 == "无信息"|g25 == "望了"|g25 == "未"|g25 == "未考"|g25 == "没"|g25 == "没孝"|g25 == "没学过"|g25 == "没有"|g25 == "没有学"|g25 == "没有考"|g25 == "没有考试"|g25 == "没有英语课"|g25 == "没考"|g25 == "没考过"|g25 == "秘密"|g25 == "空"|g25 == "记不清"|g25 == "上学期没英语"|g25 == "不之"|g25 == "不能说"|g25 == "不记得了"|g25 == "完了"|g25 == "望记"|g25 == "有??"|g25 == "没记得"|g25 == "??0"|g25 == "英语"|g25 == "上学期没有英语"|g25 == "999"
replace g25 = "4" if g25 == "4以上" //adjust the English score
replace g25 = "5.5" if g25 == "5、6"
replace g25 = "10" if g25 == "10多"
replace g25 = "13.5" if g25 == "6:21"
replace g25 = "20" if g25 == "20多"|g25 == "20多分"
replace g25 = "37.5" if g25 == "15到60"|g25 == "大概30-45"
replace g25 = "30" if g25 == "30几"|g25 == "30多分"|g25 == "30多"
replace g25 = "30.3" if g25 == "23、38、30"
replace g25 = "34" if g25 == "大约34"
replace g25 = "38" if g25 == "34 35 45"
replace g25 = "40" if g25 == "40以上"|g25 == "40几"|g25 == "40分以上"|g25 == "40多分"|g25 == "40多"|g25 == "大概40"
replace g25 = "41" if g25 == "本期中41"
replace g25 = "45" if g25 == "45多分"|g25 == "50-40"|g25 == "30-60"
replace g25 = "50" if g25 == "50多"|g25 == "50多分"|g25 == "大概50多"|g25 == "大约50多"
replace g25 = "55" if g25 == "50 60"|g25 == "50-60"|g25 == "50多~60多"
replace g25 = "59" if g25 == "不及合"|g25 == "不及格"|g25 == "不过"|g25 == "没及格"
replace g25 = "60" if g25 == "60以上"|g25 == "60分以上"|g25 == "60以上"|g25 == "60多"|g25 == "60多分"|g25 == "大概60"
replace g25 = "62.5" if g25 == "60-65"
replace g25 = "65" if g25 == "60-70"|g25 == "大约65"
replace g25 = "67" if g25 == "大约67"
replace g25 = "70" if g25 == "70以上"|g25 == "70几"|g25 == "70多"|g25 == "7?"|g25 == "七十多"|g25 == "七十多分"|g25 == "不70"|g25 == "大约七十多"
replace g25 = "75" if g25 == "60～90"|g25 == "70-80"|g25 == "70或80"|g25 == "70至80"|g25 == "80~70"|g25 == "不 75"|g25 == "大约75分"|g25 == "≈75"
replace g25 = "76" if g25 == "大约76"
replace g25 = "80" if g25 == "80以上"|g25 == "80几分"|g25 == "80多"|g25 == "80多分"|g25 == "八十多分"|g25 == "大概八十多"|g25 == "大约80"|g25 == "满分50分 我考了40分等级:a"|g25 == "大概考了80多分"|g25 == "80分以上"|g25 == "80左右"
replace g25 = "82" if g25 == "满分60分 考了41"
replace g25 = "82.5" if g25 == "80-85"
replace g25 = "84" if g25 == "84左右"|g25 == "大约84"
replace g25 = "85" if g25 == "80~90以内"|g25 == "优"|g25 == "满分60分 得51分"|g25 == "良"
replace g25 = "86" if g25 == "大概86"
replace g25 = "90" if g25 == "90以上"|g25 == "90分以上"|g25 == "90多"|g25 == "90多分"|g25 == "90左右"|g25 == "大约90分"|g25 == "忘了=90"|g25 == "约90几"|g25 == "约90多"|g25 == "90分"
replace g25 = "92" if g25 == "大概92"
replace g25 = "93" if g25 == "大约93"
replace g25 = "94" if g25 == "94分以上"
replace g25 = "95" if g25 == "95左右"|g25 == "约95"|g25 == "95以上"|g25 == "95+"
replace g25 = "96" if g25 == "约96"
replace g25 = "100" if g25 == "大概100"
destring g25, replace
winsor g25 ,gen(g25new) p(0.01)
order g25new, after(g25)
winsor height ,gen(heightnew) p(0.01) 
order heightnew, after(height)

**check and revise weight
replace weight = "29" if weight == "28-30" 
replace weight = "32.35" if weight == "31、33.7" 
replace weight = "." if weight == "不知道"
replace weight = "." if weight == "??"
destring weight, replace
winsor weight ,gen(weightnew) p(0.01)
order weightnew, after(weight)

**check if live with parents
replace g7_c = "." if g7_c == "不"|g7_c == "不知道"|g7_c == "厕所"|g7_c == "忘了"|g7_c == "我爸在外面打工"|g7_c == "我现在一个人住"|g7_c == "空"|g7_c == "自己"|g7_c == "0"
replace g7_c = "7" if g7_c == "哥哥"|g7_c == "姥姥" 
replace g7_c = "1,2" if g7_c == "a,b"|g7_c == "12"|g7_c == "21"
replace g7_c = "2,3" if g7_c == "2,03"
replace g7_c = "3,4" if g7_c == "3,-4"|g7_c == "34"
replace g7_c = "1,6" if g7_c == "1,6,a"
replace g7_c = "2,1,4" if g7_c == "2,01,4"
g g7_c1=strpos(g7,"1,2")!=0 
label define g7_c2 1 "yes" 0 "no" 
label values g7_c1 g7_c2
order g7_c1, after(g7_c)

**revise the grade
list id if year==2018 & grade==. in 1/20873 
list grade if id==11024303 & year==2017
replace grade=5 if id==11024303 & year==2018
list grade if id==11024324 & year==2017
replace grade=5 if id==11024324 & year==2018
list grade if id==11063221 & year==2017
replace grade=3 if id==11063221 & year==2018
list grade if id==14023201 & year==2017
replace grade=4 if id==14023201 & year==2018
list grade if id==14023203 & year==2017
replace grade=4 if id==14023203 & year==2018
list grade if id==14023204 & year==2017
replace grade=4 if id==14023204 & year==2018
list grade if id==14023209 & year==2017
replace grade=4 if id==14023209 & year==2018
list grade if id==14023213 & year==2017
replace grade=4 if id==14023213 & year==2018
list grade if id==14023215 & year==2017
replace grade=4 if id==14023215 & year==2018
list grade if id==14023220 & year==2017
replace grade=4 if id==14023220 & year==2018
list grade if id==14023222 & year==2017
replace grade=4 if id==14023222 & year==2018
list grade if id==14023223 & year==2017
replace grade=4 if id==14023223 & year==2018
list grade if id==14023225 & year==2017
replace grade=4 if id==14023225 & year==2018
list grade if id==14023228 & year==2017
replace grade=4 if id==14023228 & year==2018
list grade if id==14023237 & year==2017
replace grade=4 if id==14023237 & year==2018
list grade if id==14023238 & year==2017
replace grade=4 if id==14023238 & year==2018
list grade if id==14023239 & year==2017
replace grade=4 if id==14023239 & year==2018
list grade if id==14023240 & year==2017
replace grade=4 if id==14023240 & year==2018
list grade if id==14023243 & year==2017
replace grade=4 if id==14023243 & year==2018
list grade if id==17013148 & year==2017
replace grade=4 if id==17013148 & year==2018
list grade if id==21014136 & year==2017
replace grade=6 if id==21014136 & year==2018
list grade if id==21024158 & year==2017
replace grade=6 if id==21024158 & year==2018
list grade if id==21034110 & year==2017
replace grade=6 if id==21034110 & year==2018
list grade if id==21034115 & year==2017
replace grade=6 if id==21034115 & year==2018
list grade if id==21034129 & year==2017
replace grade=6 if id==21034129 & year==2018
list grade if id==21034136 & year==2017
replace grade=6 if id==21034136 & year==2018
list grade if id==21034146 & year==2017
replace grade=6 if id==21034146 & year==2018
list grade if id==21044101 & year==2017
replace grade=6 if id==21044101 & year==2018
list grade if id==21044116 & year==2017
replace grade=6 if id==21044116 & year==2018
list grade if id==21044144 & year==2017
replace grade=6 if id==21044144 & year==2018
list grade if id==21044147 & year==2017
replace grade=6 if id==21044147 & year==2018
list grade if id==21054814 & year==2017
replace grade=6 if id==21054814 & year==2018
list grade if id==21083232 & year==2017
replace grade=5 if id==21083232 & year==2018
list grade if id==21083244 & year==2017
replace grade=5 if id==21083244 & year==2018
list grade if id==21083264 & year==2017
replace grade=5 if id==21083264 & year==2018
list grade if id==23023406 & year==2017
replace grade=5 if id==23023406 & year==2018
list grade if id==26014130 & year==2017
replace grade=6 if id==26014130 & year==2018
list grade if id==26014136 & year==2017
replace grade=6 if id==26014136 & year==2018
list grade if id==26014150 & year==2017
replace grade=6 if id==26014150 & year==2018
list grade if id==31113105 & year==2017
replace grade=5 if id==31113105 & year==2018
list grade if id==31113123 & year==2017
replace grade=5 if id==31113123 & year==2018
list grade if id==31514210 & year==2017
replace grade=6 if id==31514210 & year==2018
list grade if id==32513112 & year==2017
replace grade=5 if id==32513112 & year==2018
list grade if id==34114123 & year==2017
replace grade=6 if id==34114123 & year==2018
list grade if id==34114129 & year==2017
replace grade=6 if id==34114129 & year==2018
list grade if id==34114131 & year==2017
replace grade=6 if id==34114131 & year==2018
list grade if id==34114134 & year==2017
replace grade=6 if id==34114134 & year==2018
list grade if id==34114135 & year==2017
replace grade=6 if id==34114135 & year==2018
list grade if id==34114138 & year==2017
replace grade=6 if id==34114138 & year==2018
list grade if id==34114142 & year==2017
replace grade=6 if id==34114142 & year==2018
list grade if id==34114145 & year==2017
replace grade=6 if id==34114145 & year==2018
list grade if id==34114146 & year==2017
replace grade=6 if id==34114146 & year==2018
list grade if id==34114148 & year==2017
replace grade=6 if id==34114148 & year==2018
list grade if id==34214104 & year==2017
replace grade=6 if id==34214104 & year==2018
list grade if id==34613137 & year==2017
replace grade=5 if id==34613137 & year==2018
list grade if id==34614137 & year==2017
replace grade=6 if id==34614137 & year==2018
list grade if id==35313130 & year==2017
replace grade=5 if id==35313130 & year==2018
list grade if id==35313148 & year==2017
replace grade=5 if id==35313148 & year==2018
list grade if id==35613132 & year==2017
replace grade=5 if id==35613132 & year==2018
save "D:\lsq\CCAP_CAU_student\dtanew\finalS.dta",replace

**merge the final student data set with the final school data set
use "D:\lsq\CCAP_CAU_student\dtanew\finalS.dta",clear
tostring id, gen(idn)
gen school1=substr(idn,1,4)
destring school1,replace
merge m:m school1 using "D:\lsq\CCAP_CAU_student\dtanew\school.dta"
keep if _merge==3|_merge==1
drop idn _merge
save "D:\lsq\CCAP_CAU_student\dtanew\stu_sch(1).dta",replace

//check and make up this stu_sch data set
use "D:\lsq\CCAP_CAU_student\dtanew\stu_sch(1).dta",clear
keep if year==2017
keep id  year g1_c g2_c g4_c g5_c g10_c g11_c g12_c g13_c g7_c2 g50_c g51_c s1_c
rename g1_c g1_c_2017 
rename g2_c g2_c_2017
rename g4_c g4_c_2017
rename g5_c g5_c_2017
rename g10_c g10_c_2017
rename g11_c g11_c_2017
rename g12_c g12_c_2017
rename g13_c g13_c_2017
rename g7_c2 g7_c2_2017
rename g50_c g50_c_2017
rename g51_c g51_c_2017
rename s1_c s1_c_2017
rename year year2017
save "D:\lsq\CCAP_CAU_student\stu_sch(2017).dta",replace

use "D:\lsq\CCAP_CAU_student\stu_sch(1).dta",clear
keep if year==2018
keep id year g1_c g2_c g4_c g5_c g10_c g11_c g12_c g13_c g7_c2 g50_c g51_c s1_c
rename g1_c g1_c_2018 
rename g2_c g2_c_2018
rename g4_c g4_c_2018
rename g5_c g5_c_2018
rename g10_c g10_c_2018
rename g11_c g11_c_2018
rename g12_c g12_c_2018
rename g13_c g13_c_2018
rename g7_c2 g7_c2_2018
rename g50_c g50_c_2018
rename g51_c g51_c_2018
rename s1_c s1_c_2018
rename year year2018
save "D:\lsq\CCAP_CAU_student\dtanew\stu_sch(2018).dta",replace

use"D:\lsq\CCAP_CAU_student\dtanew\stu_sch(2018).dta",clear
merge 1:1 id using "D:\lsq\CCAP_CAU_student\dtanew\stu_sch(2017).dta"
keep if _merge==3
drop _merge
merge 1:n id using "D:\lsq\CCAP_CAU_student\dtanew\stu_sch(1).dta"
drop _merge
sort id year
replace year=2017 if year2017==. & year2018==2018 & year==.
replace year=2018 if year2018==. & year2017==2017 & year==. //make up 'year'
gen sex=1 if g1_c_2018==g1_c_2017 
replace sex=0 if g1_c_2018!=g1_c_2017
replace g1_c=g1_c_2017 if g1_c==. & year==2018 //make up 'gender'
replace g1_c=g1_c_2018 if g1_c==. & year==2017
replace g2_c=g2_c_2017 if g2_c==. & year==2018 //make up 'age'
replace g2_c=g2_c_2018 if g2_c==. & year==2017
replace g4_c=g4_c_2017 if g4_c==. & year==2018 //make up 'preschool' 
replace g4_c=g4_c_2018 if g4_c==. & year==2017
replace g5_c=g5_c_2017 if g5_c==. & year==2018 //make up 'kindergarten'
replace g5_c=g5_c_2018 if g5_c==. & year==2017
replace g10_c=g10_c_2017+1 if g10_c==. & year==2018 //make up 'fa_age'
replace g10_c=g10_c_2018-1 if g10_c==. & year==2017
replace g12_c=g12_c_2017+1 if g12_c==. & year==2018 //make up 'mo_age'
replace g12_c=g12_c_2018-1 if g12_c==. & year==2017
replace g11_c=g11_c_2017 if g11_c==. & year==2018 //make up 'fa_edu'
replace g11_c=g11_c_2018 if g11_c==. & year==2017
replace g13_c=g13_c_2017 if g13_c==. & year==2018 //make up 'mo_edu'
replace g13_c=g13_c_2018 if g13_c==. & year==2017
replace g51_c=g51_c_2017 if g51_c==. & year==2018 //make up 'onlychild'
replace g51_c=g51_c_2018 if g51_c==. & year==2017
replace g50_c=g50_c_2017 if g50_c==. & year==2018 //make up 'chil_n'
replace g50_c=g50_c_2018 if g50_c==. & year==2017
replace s1_c=s1_c_2017 if s1_c==. & year==2018 //make up 'esta_school'
replace s1_c=s1_c_2018 if s1_c==. & year==2017
drop year2017 year2018 sex g1_c_2018 g2_c_2018 g4_c_2018 g5_c_2018 g10_c_2018 g11_c_2018 g12_c_2018 g13_c_2018 g50_c_2018 g51_c_2018 g7_c2_2018 s1_c_2018 g1_c_2017 g2_c_2017 g4_c_2017 g5_c_2017 g10_c_2017 g11_c_2017 g12_c_2017 g13_c_2017 g50_c_2017 g51_c_2017 g7_c2_2017 s1_c_2017
save "D:\lsq\CCAP_CAU_student\dtanew\stu_sch.dta",replace

**import the appearance score, iq and location(2017) variables
use"D:\lsq\CCAP_CAU_student\dtanew\stu_sch.dta",clear 
keep if year==2018
merge 1:1 id using"D:\lsq\CCAP_CAU_student\dtanew\chinesesocre(2018).dta"
keep if _merge == 3| _merge == 1
drop _merge
save "D:\lsq\CCAP_CAU_student\dtanew\finalS（2018）.dta",replace 

use"D:\lsq\CCAP_CAU_student\dtanew\stu_sch.dta",clear
keep if year==2017
merge 1:1 id using"D:\lsq\CCAP_CAU_student\dta\data-appearance_2017.dta"
keep if _merge == 3| _merge == 1
drop _merge
merge 1:1 id using"D:\lsq\CCAP_CAU_student\dtanew\iq_2017.dta"
drop _merge
merge 1:1 id using"D:\lsq\CCAP_CAU_student\dtanew\location_2017.dta"
drop _merge
save "D:\lsq\CCAP_CAU_student\dtanew\finalS（2017）.dta",replace

use "D:\lsq\CCAP_CAU_student\dtanew\finalS（2017）.dta",clear 
rename id peerid, replace 
keep peerid mainteacher grade height heightnew weight weightnew mathscore esteem1 esteem2 dep1 depress1 sde1 sdn1 sdo1 sdg1 sdv1 grit1 g1 g1_c g2 g2_c g3 g3_c g4 g4_c g5 g5_c g6a g6b g6c g7 g7_c g7_c1 g8 g8_c g9 g9_c g10 g10_c g38 g38_c g39 g39_c g40 g40_c g41 g41_c g42 g42_c g43 g43_c g11 g11_c g12 g12_c g13 g13_c g14 g14_c g15 g15_c g16 g16_c g17 g17_c g18 g18_c g19 g19_c g20 g20_c g21 g21_c g22 g22_c g23 g23_c g24a g24anew g24 g24new g25 g25new g26 g26_c g26a g26a_c g27 g27_c g28 g28_c g29 g29_c g30 g30_c g31 g32 g33 g34 g34_c g35 g35_c g36 g36_c g37 g37_c g44 g44_c g45 g45_c g46 g46_c g47 g47_c g48 g48_c g49 g49_c face_m face_f g50_c g51_c g7_c2 school s1 s1_c s3 s4 s4_c s5 s7 s7_c s9 s9_c s11 s11_c s13 s13_c s18 s19 s20 s21 s22 s22_c s22a s23 s23_c iqrwscore s62 s62_c s62_c1 s63 s63_c s63_c1 
foreach v of varlist _all {
rename `v' P`v'
}
rename Ppeerid peerid, replace
save "D:\lsq\CCAP_CAU_student\dtanew\PfinalS（2017）.dta",replace 

use "D:\lsq\CCAP_CAU_student\dtanew\finalS（2017）.dta",clear
rename g33_c peerid, replace
merge m:1 peerid using "D:\lsq\CCAP_CAU_student\dtanew\PfinalS（2017）.dta"
keep if _merge == 3| _merge == 1
drop _merge
order peerid, before(Pgrade)
sort id 
save "D:\lsq\CCAP_CAU_student\dtanew\FINALP（2017）.dta",replace

use "D:\lsq\CCAP_CAU_student\dtanew\finalS（2018）.dta",clear
keep id mainteacher grade height heightnew weight weightnew mathscore chinesescore esteem1 esteem2 dep1 depress1 sde1 sdn1 sdo1 sdg1 sdv1 grit1 g1 g1_c g2 g2_c g3 g3_c g4 g4_c g5 g5_c g6a g6b g6c g7 g7_c g7_c1 g8 g8_c g9 g9_c g10 g10_c g38 g38_c g39 g39_c g40 g40_c g41 g41_c g42 g42_c g43 g43_c g11 g11_c g12 g12_c g13 g13_c g14 g14_c g15 g15_c g16 g16_c g17 g17_c g18 g18_c g19 g19_c g20 g20_c g21 g21_c g22 g22_c g23 g23_c g24a g24anew g24 g24new g25 g25new g26 g26_c g26a g26a_c g27 g27_c g28 g28_c g29 g29_c g30 g30_c g31 g32 g33 g34 g34_c g35 g35_c g36 g36_c g37 g37_c g44 g44_c g45 g45_c g46 g46_c g47 g47_c g48 g48_c g49 g49_c g50_c g51_c g7_c2 school s1 s1_c s3 s4 s4_c s5 s7 s7_c s9 s9_c s11 s11_c s13 s13_c s18 s19 s20 s21 s22 s22_c s22a s23 s23_c  
foreach v of varlist _all {
rename `v' P`v'
}
rename Pid peerid, replace
save "D:\lsq\CCAP_CAU_student\dtanew\PfinalS（2018）.dta",replace 

use "D:\lsq\CCAP_CAU_student\dtanew\finalS（2018）.dta",clear
rename g33_c peerid, replace
merge m:1 peerid using "D:\lsq\CCAP_CAU_student\dtanew\PfinalS（2018）.dta"
keep if _merge == 3| _merge == 1
drop _merge
order peerid, before(Pgrade)
sort id 
save "D:\lsq\CCAP_CAU_student\dtanew\FINALP（2018）.dta",replace

use "D:\lsq\CCAP_CAU_student\dtanew\FINALP（2018）.dta",clear 
append using "D:\lsq\CCAP_CAU_student\dtanew\FINALP（2017）.dta",force
sort id year
order id year
ssc install center
center face_m face_f Pface_m Pface_f , prefix(z_) standardize //standardize the appearance score
save "D:\lsq\CCAP_CAU_student\dtanew\FINAL.dta",replace

**form the usable panel data
use "D:\lsq\CCAP_CAU_student\dtanew\FINAL.dta",clear 
duplicates report id //14650 obs
bys id : gen n=_N
keep if n>=2
drop n
tostring id, gen(idn) 
save "D:\lsq\CCAP_CAU_student\dtanew\FINAL(panel).dta",replace

**calculate household assets
use "D:\lsq\CCAP_CAU_student\dtanew\FINAL(panel).dta",clear
foreach var of varlist g15_c g16_c g21_c g17_c g18_c Pg15_c Pg16_c Pg21_c Pg17_c Pg18_c {
replace `var'=0 if `var'==.
}
gen asset=g15_c*4243+g16_c*400+g21_c*1460+g17_c*3175+g18_c*4164 
gen lasset=log(asset+1)  
label var lasset "Log asset"
gen Passet=Pg15_c*4243+Pg16_c*400+Pg21_c*1460+Pg17_c*3175+Pg18_c*4164 
gen Plasset=log(Passet+1)  
label var Plasset "Peers' Log asset"
save "D:\lsq\CCAP_CAU_student\dtanew\asset(panel).dta",replace

**import teachers' supplemental data
use "D:\lsq\CCAP_CAU_student\dtanew\asset(panel).dta",clear
gen id_class=substr(idn,1,6)
merge n:n id_class using "D:\lsq\CCAP_CAU_student\dtanew\banzhuren.dta"
drop if _merge==2
drop _merge
keep if year==2017
save "D:\lsq\CCAP_CAU_student\dtanew\FINAL(panel)_ban2017.dta",replace

use "D:\lsq\CCAP_CAU_student\dtanew\FINAL(panel)_ban.dta",clear
keep if year==2018
merge n:n id using "D:\lsq\CCAP_CAU_student\dtanew\chinesetea.dta"
drop if _merge==2
drop _merge
append using "D:\lsq\CCAP_CAU_student\dtanew\FINAL(panel)_ban2017.dta"
save "D:\lsq\CCAP_CAU_student\dtanew\FINAL(panel)_chi.dta",replace

**import reasons of peer selection data
use "D:\lsq\CCAP_CAU_student\dta\student-2.dta",clear
tab s42_c
drop if s42_c=="0"|s42_c=="7"| s42_c=="52025205"| s42_c=="不"| s42_c=="不知道"| s42_c=="仝嘉伟"| s42_c=="无"| s42_c=="是弟"|s42_c=="没"| s42_c=="没有"| s42_c=="王雪晨"| s42_c=="谢远归"
keep id s42 s42_c s42a
replace s42_c="4,5" if s42_c=="4,6"

gen s42_c1=1 if strmatch(s42_c, "*1*")
replace s42_c1=1 if s42a=="(因为她经常帮助我的学习。)"|s42=="一起作业"
replace s42_c1=0 if s42_c1!=1
replace s42_c1=1 if strmatch(s42a, "*学习*")
label var s42_c1 "study"

gen s42_c2=1 if strmatch(s42_c, "*2*")
replace s42_c2=0 if s42_c2!=1
replace s42_c2=1 if s42a=="(他傻)好欺负"|s42a=="不爱生气 脾气好"|s42a=="不跟我吵架"|s42a=="乐观"|s42a=="人好 不会总生气"|s42a=="人好,不发脾气"
replace s42_c2=1 if strmatch(s42a, "*搞笑*")
replace s42_c2=1 if strmatch(s42a, "*脾气*")
replace s42_c2=1 if strmatch(s42a, "*幽默*")
replace s42_c2=1 if strmatch(s42a, "*性格*")
replace s42_c2=1 if strmatch(s42a, "*善*")
replace s42_c2=1 if strmatch(s42a, "*诚实*")
replace s42_c2=1 if strmatch(s42a, "*开心*")
replace s42_c2=1 if strmatch(s42a, "*乐观*")
replace s42_c2=1 if strmatch(s42a, "*活泼*")
label var s42_c2 "personalities"

gen s42_c3=1 if strmatch(s42_c, "*3*")
replace s42_c3=0 if s42_c3!=1
label var s42_c3 "sport"

gen s42_c4=1 if strmatch(s42_c, "*4*")
replace s42_c4=0 if s42_c4!=1
replace s42_c4=1 if s42a=="(长得帅)"|s42a=="不知道因为他和我长得很像"|s42a=="丑、笨、胖、高"
replace s42_c4=1 if strmatch(s42a, "*帅*")
replace s42_c4=1 if strmatch(s42a, "*长相*")
label var s42_c4 "appearance"

gen s42_c5=1 if s42a=="(乐于助人)"|s42a=="(因为我俩有难同当有福同享)"|s42a=="为人友善"|s42a=="为人好"|s42a=="为人真诚 做事快"|s42a=="义气"|s42a=="乐于分享"|s42a=="乐于助人"|s42a=="乐于帮人"|s42a=="乐于助他/她人"|s42a=="互相帮助"|s42a=="认为好"|s42a=="人品"|s42a=="人品共同爱好"|s42a=="人品好 心地善良"|s42a=="人好"|s42a=="人好 诚实"
replace s42_c5=1 if strmatch(s42a, "*乐于*")
replace s42_c5=1 if strmatch(s42a, "*帮*")
replace s42_c5=1 if strmatch(s42a, "*人好*")
replace s42_c5=1 if strmatch(s42a, "*义气*")
replace s42_c5=1 if strmatch(s42a, "*信任*")
replace s42_c5=0 if s42_c5!=1
label var s42_c5 "moral"

gen s42_c6=1 if s42a=="1-5年级和他一个班"|s42a=="1年级在一起"|s42a=="2-5年级一直都在一个班"|s42=="3年同学"|s42=="3年都是同学"|s42a=="从2年级一起玩到现在了"|s42a=="从小一起玩"|s42a=="从小一起长大"|s42a=="从小一起长大 一起玩"|s42a=="从小到大一直玩"|s42a=="从小到大一起玩"|s42a=="从小和她是朋友"|s42a=="从小在一起"|s42a=="从小在一起玩"|s42a=="从小学就和他读到现在"|s42a=="从小就在一起"|s42a=="从小就认识"
replace s42_c6=1 if strmatch(s42a, "*从小*")
replace s42_c6=1 if strmatch(s42a, "*幼儿园*")
replace s42_c6=1 if strmatch(s42a, "*一直*")
replace s42_c6=1 if strmatch(s42a, "*陪伴*")
replace s42_c6=1 if strmatch(s42a, "*闺蜜*")
replace s42_c6=0 if s42_c6!=1
label var s42_c6 "duration"

gen s42_c7=1 if s42a=="(家离得很近 兴趣爱好和我相似)"|s42=="一起上学"| s42a=="一起玩 家离得近"
replace s42_c7=1 if strmatch(s42a, "*小区*")
replace s42_c7=1 if strmatch(s42a, "*邻居*")
replace s42_c7=0 if s42_c7!=1
label var s42_c7 "distance"

gen s42_c8=1 if s42a=="一块长大"|s42a=="一块长大的"|s42=="一家人"|s42a=="一起长大"|s42a=="两家关系较好、相处时间较长吧！玩得来"|s42a=="亲人"|s42a=="亲属"|s42a=="亲情"|s42a=="亲戚"|s42a=="亲戚关系"
replace s42_c8=1 if strmatch(s42a, "*亲*")
replace s42_c8=1 if strmatch(s42a, "*近*")
replace s42_c8=1 if strmatch(s42a, "*哥*")
replace s42_c8=1 if strmatch(s42a, "*姐*")
replace s42_c8=1 if strmatch(s42a, "*妹*")
replace s42_c8=1 if strmatch(s42a, "*弟*")
replace s42_c8=1 if strmatch(s42a, "*家人*")
replace s42_c8=0 if s42_c8!=1
label var s42_c8 "family"

gen s42_c9=1 if s42a=="一起玩"|s42a=="一起玩 一起同干同苦"|s42a=="一起玩很开心"|s42a=="一起玩耍"|s42a=="上网"|s42a=="与我兴趣相似"|s42a=="书法、武功"|s42a=="书法武功"|s42a=="人品共同爱好"
replace s42_c9=1 if strmatch(s42a, "*兴趣*")
replace s42_c9=1 if strmatch(s42a, "*合得来*")
replace s42_c9=1 if strmatch(s42a, "*游戏*")
replace s42_c9=0 if s42_c9!=1
label var s42_c9 "hobbies"
save "D:\lsq\CCAP_CAU_student\dtanew\whypeer.dta",replace

use "D:\lsq\CCAP_CAU_student\dtaft\FINAL(panel)_all.dta",clear
merge n:n id using "D:\lsq\CCAP_CAU_student\dtanew\whypeer.dta"
drop if _merge==2
drop _merge

**check and revise the classsize
list id_class if t19==99 
replace t19=48 if id_class=="422131"
list id_class if t19==97
count if id_class=="412141"
replace t19=83 if id_class=="412141"
list id_class if t19==96
count if id_class=="423131"
replace t19=80 if id_class=="423131"
count if id_class=="423143"
replace t19=75 if id_class=="423143"
list id_class if t19==94
count if id_class=="421132"
replace t19=74 if id_class=="421132"
count if id_class=="422143"
replace t19=79 if id_class=="422143"
list id_class if t19==92
count if id_class=="412141"
replace t19=83 if id_class=="412141"
list id_class if t19==90
count if id_class=="412131"
replace t19=69 if id_class=="412131"
list id_class if t19==86
count if id_class=="421145"
replace t19=66 if id_class=="421145"
list id_class if t19==78
count if id_class=="411141"
replace t19=73 if id_class=="411141"
list id_class if t19==77
count if id_class=="456131"
replace t19=34 if id_class=="456131"
list id_class if t19==62
count if id_class=="210241"
replace t19=44 if id_class=="210241"
count if id_class=="323142"
replace t19=39 if id_class=="323142"
count if id_class=="332132"
replace t19=28 if id_class=="332132"
list id_class if t19==7
count if id_class=="442141"
replace t19=7 if id_class=="442141"
list id_class if t19==12
count if id_class=="110331"
replace t19=9 if id_class=="110331"
list id_class if t19==15
count if id_class=="446141"
replace t19=8 if id_class=="446141"
list id_class if t19==17
count if id_class=="442141"
replace t19=7 if id_class=="442141"
save "D:\lsq\CCAP_CAU_student\dtanew\FINAL(panel)_all.dta",replace

**create the peer selection feature data set
use "D:\lsq\CCAP_CAU_student\dtanew\FINAL(panel)_all.dta" ,clear
duplicates tag peerid, generate(peernum)
replace peernum=. if peernum==2122
keep peerid peernum
save "D:\lsq\CCAP_CAU_student\dtanew\peernum.dta" ,replace
use "D:\lsq\CCAP_CAU_student\dtanew\FINAL(panel).dta" ,clear
duplicates drop peerid,force
keep peerid Pgrade Pmainteacher Pheight Pweight Pweightnew Pmathscore Pesteem1 Pesteem2 Pgrit1 Pdep1 Pdepress1 Psde1 Psdn1 Psdo1 Psdg1 Psdv1 Pg1 Pg1_c Pg2 Pg2_c Pg3 Pg3_c Pg4 Pg4_c Pg5 Pg5_c Pg6a Pg6b Pg6c Pg7 Pg7_c Pg7_c1 Pg8 Pg8_c Pg9 Pg9_c Pg10 Pg10_c Pg38 Pg38_c Pg39 Pg39_c Pg40 Pg40_c Pg41 Pg41_c Pg42 Pg42_c Pg43 Pg43_c Pg11 Pg11_c Pg12 Pg12_c Pg13 Pg13_c Pg14 Pg14_c Pg15 Pg15_c Pg16 Pg16_c Pg17 Pg17_c Pg18 Pg18_c Pg19 Pg19_c Pg20 Pg20_c Pg21 Pg21_c Pg22 Pg22_c Pg23 Pg23_c Pg24a Pg24anew Pg24 Pg24new Pg25 Pg25new Pg26 Pg26_c Pg26a Pg26a_c Pg27 Pg27_c Pg28 Pg28_c Pg29 Pg29_c Pg30 Pg30_c Pg31 Pg32 Pg33 Pg34 Pg34_c Pg35 Pg35_c Pg36 Pg36_c Pg37 Pg37_c Pg44 Pg44_c Pg45 Pg45_c Pg46 Pg46_c Pg47 Pg47_c Pg48 Pg48_c Pg49 Pg49_c Pface_m Pface_f 
merge 1:n peerid using"D:\lsq\CCAP_CAU_student\dtanew\peernum.dta"
drop _merge
order peernum, before(peerid)
save "D:\lsq\CCAP_CAU_student\dtanew\Pchoice.dta",replace 

use "D:\lsq\CCAP_CAU_student\dtanew\FINAL(panel).dta",clear //if two years peer is the same person
keep if year==2017
keep id name peerid
gen peer1=peerid
drop if peer1==.
save "D:\lsq\CCAP_CAU_student\dtanew\peervary(2017).dta",replace
use "D:\lsq\CCAP_CAU_student\dtanew\FINAL(panel).dta",clear 
keep if year==2018
keep id name peerid
gen peer2=peerid
drop if peer2==.
save "D:\lsq\CCAP_CAU_student\peervary(2018).dta",replace
use "D:\lsq\CCAP_CAU_student\dtanew\peervary(2018).dta",clear
merge 1:1 id using "D:\lsq\CCAP_CAU_student\dtanew\peervary(2017).dta"
drop _merge
gen peervary=1 if peer1!=peer2
replace peervary=0 if peer1==peer2
tab peervary //1317 chidren with the unchanged peer
save "D:\lsq\CCAP_CAU_student\dtanew\peervary.dta",replace

**descriptive statistical
use "D:\lsq\CCAP_CAU_student\dtanew\FINAL(panel)_all.dta",clear
replace pro=3 if id==34613137  //migrant
keep pro year s1 s1_c s4 s4_c s19 s20 s21 s22 s22_c s22a s7_t s9_t s11_t s13_t t2 t3 t5 t7 t8_c1 t9 t9_c t10 t10_c t11 t11_c t16 t16_c c181 c181_c c182 c182_c c183 c183_c c184 c184_c c185 c185_c c186 c186_c c187 c187_c c188 c188_c c189 c189_c c190 c190_c c191 c191_c c192 c192_c c193 c193_c c194 c194_c c195 c195_c c196 c196_c c197 c197_c c198_c1 c199 c199_c c199_c1 c200 c200_c c200_c1 c201 c201_c c201_c1 c202 c202_c c2 c3 c5 c7 c8_c1 c9 c9_c c11 c11_c c17 c17_c c18 c18_c c19 c19_c c20 c20_c c21 c21_c c22 c22_c m2 m3 m5 m7 m8_c1 m9 m9_c m11 m11_c m17 m17_c m18 m18_c m19 m19_c m20 m20_c m21 m21_c m22 m22_c m203 m203_c m204 m204_c m205 m205_c m206 m206_c m207 m207_c m208 m208_c m209 m209_c m210 m210_c m211 m211_c m212 m212_c m213 m213_c m214 m214_c m215 m215_c m216 m216_c m217 m217_c m218 m218_c m219 m219_c m220 m220_c m220_c1 m221 m221_c m221_c1 m222 m222_c m222_c1 m223 m223_c m223_c1 m224 m224_c
keep if pro==1| pro==2
set matsize 800
outreg2 using "D:\lsq\1.doc",replace sum(log) keep(s1_c s4_c s19 s20 s21 s22_c s7_t s9_t s11_t s13_t) title(mig)
replace t3=. if t3==2
outreg2 using "D:\lsq\ban.doc",replace sum(log) keep(t2 t3 t5 t7 t8_c1 t9_c t10_c t11_c t16_c) title(mig)
outreg2 using "D:\lsq\math.doc",replace sum(log) keep( m2 m3 m5 m7 m8_c1 m9_c m11_c m17_c m18_c m19_c m20_c m21_c m22_c m203_c m205_c m206_c m207_c m210_c m211_c m212_c m213_c m214_c m215_c m216_c m217_c m218_c m219_c m220_c1 m221_c1 m222_c1 m223_c1 m224_c ) title(mig)
outreg2 using "D:\lsq\chinese.doc",replace sum(log) keep( c2 c3 c5 c7 c8_c1 c9_c c11_c c17_c c18_c c19_c c20_c c21_c c22_c c181_c c183_c c184_c c185_c c188_c c189_c c190_c c191_c c192_c c193_c c194_c c195_c c196_c c197_c c198_c1 c199_c1 c200_c1 c201_c1 c202_c ) title(mig)

use "D:\lsq\CCAP_CAU_student\dtanew\FINAL(panel)_all.dta",clear
replace pro=3 if id==34613137 //rural
winsor m7, gen(m7_c) p(0.01)
keep pro year s1 s1_c s4 s4_c s19 s20 s21 s22 s22_c s22a s7_t s9_t s11_t s13_t t2 t3 t5 t7 t8_c1 t9 t9_c t10 t10_c t11 t11_c t16 t16_c c181 c181_c c182 c182_c c183 c183_c c184 c184_c c185 c185_c c186 c186_c c187 c187_c c188 c188_c c189 c189_c c190 c190_c c191 c191_c c192 c192_c c193 c193_c c194 c194_c c195 c195_c c196 c196_c c197 c197_c c198_c1 c199 c199_c c199_c1 c200 c200_c c200_c1 c201 c201_c c201_c1 c202 c202_c c2 c3 c5 c7 c8_c1 c9 c9_c c11 c11_c c17 c17_c c18 c18_c c19 c19_c c20 c20_c c21 c21_c c22 c22_c m2 m3 m5 m7 m8_c1 m9 m9_c m11 m11_c m17 m17_c m18 m18_c m19 m19_c m20 m20_c m21 m21_c m22 m22_c m203 m203_c m204 m204_c m205 m205_c m206 m206_c m207 m207_c m208 m208_c m209 m209_c m210 m210_c m211 m211_c m212 m212_c m213 m213_c m214 m214_c m215 m215_c m216 m216_c m217 m217_c m218 m218_c m219 m219_c m220 m220_c m220_c1 m221 m221_c m221_c1 m222 m222_c m222_c1 m223 m223_c m223_c1 m224 m224_c
keep if pro==3| pro==4 
set matsize 800
outreg2 using "D:\lsq\2.doc",replace sum(log) keep(s1_c s4_c s19 s20 s21 s22_c s7_t s9_t s11_t s13_t) title(mig)
replace t3=. if t3==2 
outreg2 using "D:\lsq\ban2.doc",replace sum(log) keep(t2 t3 t5 t7 t8_c1 t9_c t10_c t11_c t16_c) title(mig)
outreg2 using "D:\lsq\math2.doc",replace sum(log) keep( m2 m3 m5 m7 m8_c1 m9_c m11_c m17_c m18_c m19_c m20_c m21_c m22_c m203_c m205_c m206_c m207_c m210_c m211_c m212_c m213_c m214_c m215_c m216_c m217_c m218_c m219_c m220_c1 m221_c1 m222_c1 m223_c1 m224_c ) title(mig)

use "D:\lsq\CCAP_CAU_student\dtanew\FINAL(panel)_all.dta",clear
replace pro=3 if id==34613137 //compare with individuals
keep pro year s1 s1_c s4 s4_c s19 s20 s21 s22 s22_c s22a s7_t s9_t s11_t s13_t t2 t3 t5 t7 t8_c1 t9 t9_c t10 t10_c t11 t11_c t16 t16_c c181 c181_c c182 c182_c c183 c183_c c184 c184_c c185 c185_c c186 c186_c c187 c187_c c188 c188_c c189 c189_c c190 c190_c c191 c191_c c192 c192_c c193 c193_c c194 c194_c c195 c195_c c196 c196_c c197 c197_c c198_c1 c199 c199_c c199_c1 c200 c200_c c200_c1 c201 c201_c c201_c1 c202 c202_c c2 c3 c5 c7 c8_c1 c9 c9_c c11 c11_c c17 c17_c c18 c18_c c19 c19_c c20 c20_c c21 c21_c c22 c22_c m2 m3 m5 m7 m8_c1 m9 m9_c m11 m11_c m17 m17_c m18 m18_c m19 m19_c m20 m20_c m21 m21_c m22 m22_c m203 m203_c m204 m204_c m205 m205_c m206 m206_c m207 m207_c m208 m208_c m209 m209_c m210 m210_c m211 m211_c m212 m212_c m213 m213_c m214 m214_c m215 m215_c m216 m216_c m217 m217_c m218 m218_c m219 m219_c m220 m220_c m220_c1 m221 m221_c m221_c1 m222 m222_c m222_c1 m223 m223_c m223_c1 m224 m224_c
set matsize 800
outreg2 using "D:\lsq\0.doc",replace sum(log) keep(s1_c s4_c s19 s20 s21 s22_c s7_t s9_t s11_t s13_t) title(mig)
outreg2 using "D:\lsq\ban0.doc",replace sum(log) keep(t2 t3 t5 t7 t8_c1 t9_c t10_c t11_c t16_c) title(mig)
outreg2 using "D:\lsq\math0.doc",replace sum(log) keep( m2 m3 m5 m7 m8_c1 m9_c m11_c m17_c m18_c m19_c m20_c m21_c m22_c m203_c m205_c m206_c m207_c m210_c m211_c m212_c m213_c m214_c m215_c m216_c m217_c m218_c m219_c m220_c1 m221_c1 m222_c1 m223_c1 m224_c ) title(mig)
outreg2 using "D:\lsq\chinese0.doc",replace sum(log) keep( c2 c3 c5 c7 c8_c1 c9_c c11_c c17_c c18_c c19_c c20_c c21_c c22_c c181_c c183_c c184_c c185_c c188_c c189_c c190_c c191_c c192_c c193_c c194_c c195_c c196_c c197_c c198_c1 c199_c1 c200_c1 c201_c1 c202_c ) title(mig)

histogram mathscore,bin(10) norm 
tw (kdensity math ) (kdensity chinese ) (kdensity english), legend(label(1 "math") label(2 "chinese") label(3 "english") col(3))
kdensity mathscore , normal
tw (kdensity mathscore if grade==3 ) (kdensity mathscore if grade==4 ), legend(label(1 "3rd") label(2 "4th") col(2))

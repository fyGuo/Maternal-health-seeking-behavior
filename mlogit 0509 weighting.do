use "D:\tang_data\cluster0509.dta",clear

*设置复杂抽样调查参数

svyset psu [pw=wmweight], strata(stratum)
#total birth
table cm11
************************************
*woman's age
table wb4

*birth order
table cm11

*marital status
table ma1
drop if ma1==9
fvset base 3 ma1

*woman's education level
table welevel

*women's france fluency
table wm15
drop if wm15=="NA"
gen wmfrance=0
replace wmfrance=1 if wm15=="3"

*Are women covered by a health insurance
table insurance
drop if insurance==9
fvset base 2 insurance

* read newspaper or not
table mt1
drop if mt1==9
gen newspaper=1
replace newspaper=0 if mt1==0

* listen to radio
table mt2
drop if mt2==9
gen radio=1
replace radio=0 if mt2==0

* tv
table mt3
drop if mt3==9
gen tv=1
replace tv=0 if mt3==0

* computer
table mt4
drop if mt4==9
gen computer=1
replace computer=0 if mt4==2


*unwanted pregnant
table db2
drop if db2==9
replace db2=0 if db2==1
replace db2=1 if db2==2

*died child
table cm8
replace cm8=0 if cm8==2

*urban or rural
table hh6
gen urban=0
replace urban=1 if hh6==1

*household wealth group
table windex5

*householder's gender
table hhsex
gen hhmale=1
replace hhmale=0 if hhsex==2

*householder's ethnics
table hc2
drop if hc2=="NA"
gen ethnics=1
replace ethnics=1 if hc2=="1"
replace ethnics=2 if hc2=="2"
replace ethnics=3 if hc2=="3"
replace ethnics=4 if hc2=="4"
replace ethnics=5 if hc2=="5"
replace ethnics=6 if hc2=="6"


*household's religion 
table hc1a
drop if hc1a=="NA"
drop if hc1a=="99"
gen religion=4
replace religion=1 if hc1a=="1"
replace religion=2 if hc1a=="2"
replace religion=3 if hc1a=="6"
replace religion=0 if hc1a=="97"
fvset base 0 religion

* hosehold head's france
table hh17
gen hhfrance=0
replace hhfrance=1 if hh17==3

*household header's education level
table helevel
drop if helevel==9

global controlvar "urban i.windex5 hhmale i.ethnics i.religion hhfrance i.helevel"
global intervenvar "wb4 cm11 i.ma1  i.welevel wmfrance insurance newspaper radio tv computer db2 cm8"
mlogit cluster wb4 cm11 i.ma1  i.welevel wmfrance insurance newspaper radio tv computer db2 cm8/*
*/  $controlvar , base(2)  vce(cluster psu)
mlogtest,iia
estat ic


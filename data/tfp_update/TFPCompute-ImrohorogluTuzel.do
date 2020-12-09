
***************************************************************
* Ayse Imrohoroglu and Selale Tuzel, June 27, 2015
* This Stata code generates TFP estimates for Compustat firms.
*
* Inputs: 
* 1. Compustat FUNDA file, columns: gvkey fyear at capx dp dpact emp oibdp ppegt sale
* 2. Compustat NAMES file, columns: gvkey sic naics year1 year2
* 3. prices_updated.dta file
*
* Output:
* 1. Rolling window production function estimates (coefficients for labor and capital) are stored in matrix b
* 2. TFPData_updated.csv 
***************************************************************



* main Compustat (funda) input
* gvkey fyear at capx dp dpact emp oibdp ppegt sale
use "compustat_data.dta", clear

* merge with Compustat names file to get the first year in Compustat
* gvkey sic naics year1 year2
merge m:1 gvkey using "compustat_names.dta", keep(matched)
drop _merge 
gen Compustatage = fyear - year1
drop if Compustatage<2

*********
* Get the first year of exit from Compustat
* Note that there are no exits from Compustat before 1966
sum year2
return list
scalar firstexit=r(min)
********;


drop if sale<0.1 | at<0.1 | at==. | emp<0.1 | ppegt<0.1 | dp==. | dpact==. | dp==0 | dpact==0 
drop if emp==. | oibdp==.
drop if sale==. | ppegt==. 
drop if  capx==. | capx<=0 

* merge wages, GDP deflator (for output) and investment deflator (for investment, capital)
* fyear wage_ind gdp_ind nonres_ind
merge m:1 fyear using "prices_updated.dta", keep(match)

drop _merge

replace wage=wage/1000 //figures are in millions and number of employees in thousands, so we make wage in thousands (when multiplied, whe get expenses in millions)

sort gvkey fyear

* compute value added
gen total_expense=sale-oibdp
gen labor_expense=emp*wage
gen materials=total_expense-labor_expense
gen value_added=sale-materials

* compute the age of capital
gen cap_age=dpact/dp
gen ave_age=cap_age
replace ave_age = ( cap_age[_n-1] + cap_age[_n] )/2 if fyear==fyear[_n-1]+1
replace ave_age = ( cap_age[_n-2] + cap_age[_n-1] + cap_age[_n] )/3 if fyear==fyear[_n-1]+1 & fyear==fyear[_n-2]+2
gen age=round(ave_age,1) 
gen cap_year=fyear-age
replace cap_year=1951 if cap_year<1951 

destring sic, generate(SIC)
* drop financials and utilities
drop if SIC>=4900 & SIC<4999
drop if SIC>=6000 & SIC<6999


rename gdp_ind gdp_def
rename wage_ind wages
rename nonres_ind inv_def

* match PPE with the investment price deflator from year cap_year
rename fyear year
rename cap_year fyear
merge m:1 fyear using "prices_updated.dta", keep(match)
drop _merge gdp_ind wage_ind
rename fyear cap_year
rename year fyear
rename nonres_ind past_inv_def


* compute the "quantities" of output, investment, and capital
sort gvkey fyear
gen adj_value=value_added/gdp_def
gen adj_cap=ppegt/past_inv_def
gen adj_inv=capx/inv_def
by gvkey: gen lag_cap = adj_cap[_n-1] if fyear==fyear[_n-1]+1

* additional filters for correct calculation of value added
drop if materials<0.01
drop if adj_value<0.01



egen company = group(gvkey)
drop at dp dpact oibdp 
drop ppegt sale wages total_expense labor_expense materials value_added cap_age ave_age age cap_year adj_cap capx
drop gdp_def inv_def past_inv_def sic naics year1 


rename fyear year
rename adj_inv i
rename emp l
rename adj_value y
rename lag_cap k




drop if year<1962
replace y=log(y)
replace i=log(i)
replace l=log(l)
replace k=log(k)

drop if k==. | y==. | l==.
drop if i==. 

tsset company year
order company year SIC y l k i

set matsize 11000
* generate 3-digit SIC codes
replace SIC=floor(SIC/10) 


sum year
return list
scalar last=r(max)

*define exit
gen exit = 0
replace exit=1 if year==year2& year!=last


gen i2=i^2
gen k2=k^2
gen ik=i*k



order company year SIC y l k i i2 k2 ik
gen ind_year=year*1000+SIC

gen capital=0
gen labor=0

sum year
return list
scalar min=r(min)+1 
scalar max=r(max)
scalar size=max-min+1

matrix betas=J(2,size,0)

scalar col=0
gen TFP=.

* expanding window production function estimation
forvalues q=`=min'/`=max'{

	* generate exit probabilities
	if `q'>`=firstexit' {
		probit exit i k ik i2 k2 if year <=`q' 
		predict exit_prob if year <=`q' 
	} 
	else {
	gen exit_prob = 0 
	}
	
    
	* first stage regression, estimate labor coef.
	areg y l k i i2 k2 ik  if year <=`q', absorb(ind_year)

	predict res, residuals
	ereturn list
	matrix betas_1=e(b)
	scalar col=col+1
	matrix betas[1,col]=betas_1[1,1]
	replace labor=betas_1[1,1] if year==`q'
	
	* second stage regression to estimate the coef. for capital
	gen Q= _b[i]*i + _b[k]*k + _b[i2]*i2 + _b[k2]*k2 + _b[ik]*ik 
	gen y_al= Q + res
	nl (y_al = {b_0=0} + {b_1=0.3}*k + {b_2=0.5}*(L.Q - {b_1=0.3}*L.k)+{b_exit=0}*L.exit_prob) if year <=`q' & !missing(L.k) & !missing(L.exit_prob)
	

	

	ereturn list
	matrix betas_2=e(b)
	matrix betas[2,col]=betas_2[1,2]
	replace capital=betas_2[1,2] if year==`q'
	
	* compute TFP
	replace TFP=y_al-k*capital if TFP==.
	
	drop res Q exit_prob y_al
	
	}
	
matrix b=betas'

* Production function estimates
* Coefficients for capital and labor, expanding window estimates
matrix list b




drop if TFP==.
drop if year==1962

* delete observations if there are fewer than five observations from that industry
* (due to industry specific time dummies)
bysort year SIC: gen a=_N
drop if a<5


destring gvkey, replace
keep gvkey year TFP
rename year fyear
order gvkey fyear TFP
sort gvkey fyear

* TFP estimates
outsheet using "TFPData_updated.csv", comma replace


proc import datafile="F:\Local Disk F\Following_the_trend_Github\Futures data\SCF\SCF_20170307.csv" 
out=Quandl_new dbms=csv replace; 
getnames=no; 
run;

data Quandl_new;
	set Quandl_new;
	v1=substr(var1,length(var1)-1,2);
	v2=substr(var1,length(var1)-3,1);
    v3=substr(var1,length(var1)-5,2);
	format var2 date9.; 
run;

/*create contract types dataset */

data Method;
   infile datalines delimiter=','; 
   input Symbol $ type $30.;
   datalines; 
   EN, Raw prices Last day 
   FN, Raw prices First day 
   ON, Raw prices Open interest 
   EF, Forward Panama Last day 
   FF, Forward Panama First day 
   OF, Forward Panama Open interest 
   EB, Backward Panama Last day 
   FB, Backward Panama First day 
   OB, Backward Panama Open interest 
   ER, Backwards ratio Last day 
   FR, Backwards ratio First day 
   OR, Backwards ratio Open interest 
   EW, Calendar weighted Last day 
   FW, Calendar weighted First day
   ;



/*Create actual results dataset */
data Actual_results;
   infile datalines delimiter=','; 
   input Asset $ Symbol $ Ret;
   datalines;                      
 Cattle, LC, 4.52
 Copper, HG, 8.90
 Corn, C, -3.19
 Gold, GC, 5.36
 Heatoil, HO, 9.79
 Hogs, LN, 3.39
 Natgas, NG, -9.74
 Platinum, PL, 13.15
 Silver, SI, 3.17
 Soybeans, S, 5.57
 Soyoil, BO, 1.07
 Wheat, W, -1.84
 S&P500, SP, 3.47
 US10, TY, 3.80
 US30, US, 9.50
 AUDUSD, AD, 1.85
 CADUSD, CD, 0.60
;

data method;
	set method;
	nrow=_N_;
run;

/*Analysis for diff methods*/
data new;
	set Quandl_new;
	if v1='OR' and v2=1;
	if v3='LC' and var2>='01JAN1965'd or v3='HG' and var2>='01JAN1977'd or v3='C' and var2>='01JAN1965'd 
    or v3='GC' or v3='HO' or v3='LN' or v3='NG' or v3='PL' and var2>='01JAN1992'd or v3='SI' and var2>='01JAN1965'd
    or v3='S' and var2>='01JAN1965'd or v3='BO' and var2>='01OCT1990'd or v3='W' and var2>='01JAN1965'd
    or v3='SP' or v3='TY' or v3='US' and var2>='01JAN1990'd 
	or v3='AD' and var2>='01MAR1972'd or v3='CD' and var2>='01MAR1972'd;
run;


data new;
	set new;
	by var1;
	ret=log(VAR6/lag(VAR6));
	if first.var1=1 then ret=.;	
run;

proc means data=new noprint;
  by var1;
  var ret;
  output out=OR mean=OR;
quit;

data OR;
  set OR;
  drop _Type_ _Freq_;
  Var=1;
  Stat='Mean';
  OR=OR*252*100;
  Symbol=substr(var1,length(var1)-5,2);
run;

proc sql;
  create table Comp as
  select distinct a.*, b.OR
  from Comp as a, OR as b
  where a.Symbol=b.Symbol;
quit;

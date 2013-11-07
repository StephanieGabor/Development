*  Program...........: CSickBank.PRG
*  Author............: Stefan Gabor 
*  Project...........: Carver Human Resources Zoo62
*  Created...........: November 08, 2013
*  Code page.........: 1252 (WINDOWS)
*  Copyright.........: (c) Carver Technologies 2013
*
*  Description.......: The class handles sick, mobile and compensatory days 
*                    : calculation. 
*                    : The class is build around the plan id. The key point is 
*                    : that the class is currently called from two different places 
*                    : and it is easily expandable.
*                    :    1) annual entitlement 
*                    :    2) time sheet posting. 
*                    :
* Specification      : 1 - The class handles real time entitlement for companies 
*                    :     which do not use the payroll system and the event 
*                    :     is triggered from the timesheet
*                    : 2 - Handles entitlement of multiple banks of 
*                    :     the same type simultaneously
*                    :
*	Model             : GetYTDVacDays__XXXXXXX(), where "XXXXXXX" 
*                    :     must be replaced by the plan id.
*                    :     Get the YTD (Year To Date) entitlement of number 
*                    :     of sick/mobile/compensatory days based on a 
*                    :     predefined redefined formula. 
*							:
* Call sequence      : 1 - GetSickBankObject(number of months of seniority) 
*                    :     in order to get a memory reference to the object. 
*                    :     The method within creates and sets certain 
*                    :     properties to an object which is then return to 
*                    :     the calling module. The reason behind it, 
*                    :     is that certain properties of the object may need 
*                    :     to be set from the calling module environment, 
*                    :     so the programmer needs to be able to do this. 
*                    : 2 - PostEntitlement(object) - this call will perform 
*                    :		the entitlement calculation.
*                    :
*                    :
*#########################################################

**** Class definition 
define class CSickBank as custom 

nFullMonthServicePCT = .5  		&& Precent given for full month of service 
nMontlyAVGWorkHours = 130			&& Monthly average working hours  
nAPMCP_DAYSWKPERYEAR = 248.6		&& APMCP maximum yearly working days
nMALAPMCP_SICKDAYSPERYEAR = 5		&& APMCP maximum sick days per year  
nSEB_DAYSWKPERYEAR = 260 			&& SEB maximum yearly working days 
nMALSEB_SICKDAYSPERYEAR = 10		&& SEB maximum sick days per year  
nMOBSEB_MOBILEDAYSPERYEAR = 6		&& SEB & ATT maximum mobile days per year  
nCOMPENSATORY_TIME_YEAR = 5		&& Compensatory time / year 
nMALSEB_WAITINGPERIODDAYS= 4		&& SEB waiting period number of days 

nATT_DAYSWKPERYEAR = 260 			&& ATT maximum yearly working days 
nMALATT_SICKDAYSPERYEAR = 10		&& ATT maximum sick days per year  
nMALATT_WAITINGPERIODDAYS= 4		&& ATT waiting period number of days 

*** Property 
cLogStatus 	= "startlog.txt" 		&& start logging 
cLogPath = "\TEMP\" 					&& log folder 
_DEBUG = .T.							&& set .T. for debugging 


*=========================================================
#define POST_ENTITLEMENT
*=========================================================
procedure PostEntitlement(toRS As Object)
*** Main entry in the program and it matches 
* the event which trigger this action. 
* For example, in this case, the action is triggered from 
* Plans:Banks\Entitlement screen.
*
* PARAMETERS:
*		toRS - object which holds all the fields needed 
*				 for calculation.
*
if type("toRS")!="O" or isnull(toRS)
	this.WriteLog("DepositBeginningOfYear() - Invalid Object!")
	return loRS 
endif 

with toRS 
	*** Get vacation days 
	this.GetPRORATA()
endwith 

*** Debug 
this.SpoolObjectProperties(toRS)

use in select("TVac")
return toRS 

endproc

*=========================================================
procedure GetSickBankObject(tnSenMths)
*** This procedure calls a method the create a sick bank 
* object and then returns it to the calling module.
* The idea behind it is to allow different environments 
* to set certain properties on the object before posting 
* the entitlement; all posting entitlement processing 
* it is performed on the object, making it environement 
* independent 
*
local loRS
store null to loRS 

*** set step on 
if !used("qAPLAN") or (tnSenMths = 0)
	this.WriteLog("GetSickBankObject() - qAPLAN not opened!")
	return loRS 
endif 

*** Create a Sick bank object 
loRS = this.CreateSickBankObject()
with loRS 
	*** Initialize some properties of the object 
	this.SetSickBankProperty(tnSenMths) 
endwith 

*** Debug 
*** this.SpoolObjectProperties(loRS)

return loRS 
endproc

*==========================================================
protected procedure SetSickBankProperty(tnSenMths)
*** Populate the objects' constants 
*
local loBizSched, ldSeniorField 
local llMonthlyCalc, llYearlyCalc
store null to loBizSched

*** ID's 
if empty(.nPersId) and type("lnPersId") = "N"
	.nPersId = lnPersId
endif
if empty(.cPlanId)
	.cPlanId = trim(qAPLAN.AP_PLANID)
endif	
if empty(.cBankId)
	.cBankId = trim(qAPLAN.AP_BANKID)
endif	
if empty(.cDepositOPT)
	.cDepositOPT = trim(qAPLAN.AP_DEPOOPT)
endif 

*** Load union into object 
if used("vJobhist") and empty(.cUnionId)
	.cUnionId = trim(vJobhist.H_UNION)
endif	
if used("qJobhist") and empty(.cUnionId)
	.cUnionId = trim(qJobhist.H_UNION)
endif	

*** Load schedule id into object 
if used("vJobhist") and empty(.cSchedId)
	.cSchedId = trim(vJobhist.H_SCHEDID)
endif 
if used("qJobhist") and empty(.cSchedId)
	.cSchedId = trim(qJobhist.H_SCHEDID)
endif 

*** Load jobhist effdt into object 
if used("vJobhist") and empty(.dEffDt)
	.dEffDt = vJobhist.H_EFFDT 
endif 
if used("qJobhist") and empty(.dEffDt)
	.dEffDt = qJobhist.H_EFFDT 
endif 

*** Rest of the properties 
.nSenMths = tnSenMths 
if tnSenMths > 0 
	.nSenYears = int(tnSenMths/12)
endif 	

*** Different date field for seniority 
if used("vPERS") and !empty(qAPLAN.AP_SENDT)
	ldSeniorField = trim(qAPLAN.AP_SENDT)
	ldSeniorField = strtran(ldSeniorField, "PERS", "vPERS")
	ldSeniorField = evaluate(ldSeniorField)

	.nGOVTSenYears = year(date())-year(ldSeniorField )
	.nGOVTSenMths = .nGOVTSenYears*12 
endif 

*** Current Year Entitlement 
.dStartDt = qAPLAN.AP_EFFDT
.dEndDt = qAPLAN.AP_ENDDT

*** Last year entitlement dates 
.dLYStartDt = ;
	MONDAY(date(year(.dStartDt)-1,month(.dStartDt),day(.dStartDt))+7, 7)
.dLYEndDt = .dStartDt - 1 

llMonthlyCalc = ToLogical(qAPLAN.AP_MCALC)
llYearlyCalc = ToLogical(qAPLAN.AP_YCALC)

*** Get the counter - Monthly deposit 
if llMonthlyCalc 
	.cTCNT = trim(qAPLAN.AP_MTCNT)
endif 
	
*** Yearly deposit 
if llYearlyCalc
	.cTCNT = trim(qAPLAN.AP_YTCNT)
endif 

*** Get the bank unit HOURS or DAYS 
.cBankUnit = trim(tbleval("TIMEBANK", .cBankId, "TBLC1"))

loBizSched = GetBiz("SCHED")
if isnull(loBizSched)
	store null to loBizSched
	return
endif 

*** Get the average working HOURS/DAY 
.nHRSPDY = loBizSched.WkCal("AVHRSPDY", ;
			.cSchedId, .dEffDt )

store null to loBizSched

return 
endproc

*==========================================================
protected procedure GetPRORATA()
*** Gets the current year's number of vaction days, 
* to which an employee is entitled, based on its seniority.
* 
* NOTE: The program handles calls to the entitlment vacation 
* days calculations methods by individual plans which are 
* configured in the interface.  
*
local lcCommand
store "" to lcCommand 

*** Generic stuff that applies to plan 
this.GetPRORATAEntitlement(.dStartDt,.dEndDt)
	
*** Include current transactions from timesheet, 
* that are in progress of posting.  
if nvl(.IsTSRealTime,.f.) = .t.
	this.GetPRORATA_TIMESHEET()
	this.GetYTDBankDeposit()
endif 

*** Plan specific stuff 
if lower(pemstatus(this, "GetSickDays_" + ;
	trim(.cPlanId), 3)) = "method"
	
	lcCommand = "this.GetSickDays_"+trim(.cPlanId)+"()"
	return &lcCommand
endif 

return 
endproc

*==========================================================
protected procedure GetPRORATAEntitlement(tdStartDt,tdEndDt)
*** The procedure is used to calculate the number of days, 
* which an employee is entitle to take during current year, 
* based on an interval date range.
*
local lnSelect, lnCount, lnFMS, lcXls 
store 0 to lnCount, lnFMS 

lnSelect = select()

** Create a cursor to hold the values 
use in select("qWKHours")
create cursor qWKHours (IYear N(4), IMonth N(3), ;
	StartDt D, EndDt D, nFMS N(2,0), ;
	SchedWKHrs N(14, 4), MthAVGHrs N(14, 4), RealWKHrs N(14, 4))

index on str(IYear,4) + str(IMonth, 2) tag TWKHRS  

*** I assume that referential year starts from 1st day of the month
.dStartOfMonthDt = day1(tdStartDt) 

*** set step on 
for lnCount = 1 to 12 
	.dStartOfMonthDt = gomonth(.dStartOfMonthDt, iif(lnCount=1,0,1))
	.dEndOfMonthDt = day1(gomonth(.dStartOfMonthDt,1))-1

	*** MYTD scheduled to worked
	.nMTDSchedToWork = TimeWorked("HOURS", .nPersId, ;
			.dStartOfMonthDt,.dEndOfMonthDt)

	*** MYTD hours worked
	.nMTDHoursWorked = TimeWorked("HOURS", .nPersId, ;
			.dStartOfMonthDt,.dEndOfMonthDt,.cTCNT)

	*** Get the total of hours worked 
	.nTotHoursWorked = .nTotHoursWorked + .nMTDHoursWorked

	*** Assign 1 day for every 1/2 of month or more of service 
	lnFMS = this.GetFullMonthOfService() 
	.nFullMonthOfService = .nFullMonthOfService + lnFMS 

	*** DEBUG 
	insert into qWKHours (IYear, IMonth, StartDt, EndDt, ;
			nFMS, SchedWKHrs, MthAVGHrs, RealWKHrs) ;
	values(year(.dStartOfMonthDt), month(.dStartOfMonthDt), ;
			.dStartOfMonthDt, .dEndOfMonthDt, lnFMS, ;
			.nMTDSchedToWork, this.nMontlyAVGWorkHours, ;
			.nMTDHoursWorked )
next 

*** if the bank is in hours 
* we need to convert to days 
if "H"$.cBankUnit and .nHRSPDY != 0 
	.nTotDaysWorked = round(.nTotHoursWorked/.nHRSPDY,0)
endif 
	
if this._DEBUG	
	select qWKHours 
	go top in qWKHours 
	lcXls = this.cLogPath+"_SICK_"+lower(alias())+".xls"
	copy to (lcXls) type xls 
endif 

use in select("qWKHours")
select( lnSelect )

return 
endproc

*==========================================================
protected procedure GetPRORATA_TIMESHEET()
*** The procedure gets the total number of hours from  
* timetmp based on a counter built for timedt. 
* It is used to include this total in the prorata 
* calculation for sick days
*
local lcWhere, loBizTimetmp

*** set step on
store "" to lcWhere
store null to loBizTimetmp

if empty(.dTSStartDt) or empty(.dTSEndDt)
	return 
endif 	

text to lcWhere textmerge noshow pretext 1+4
	TT_PERSID = PERSNUMBER 
	AND COUNTER
	AND TT_EFFDT BETWEEN 'STARTDATE' AND 'ENDDATE' 
endtext 

lcWhere = strtran(lcWhere,"PERSNUMBER", transform(.nPersId)) 
lcWhere = strtran(lcWhere,"COUNTER", .cTSTCNT) 
lcWhere = strtran(lcWhere,"STARTDATE", dtoc(.dTSStartDt)) 
lcWhere = strtran(lcWhere,"ENDDATE", dtoc(.dTSEndDt)) 

loBizTimetmp = Getbiz("TIMETMP1")
loBizTimetmp = loBizTimetmp.GetList("/CURSOR=qTimeT2", ;
					"sum(TT_HOURS) as TOTHRS", lcWhere)

if !isnull(loBizTimetmp)
	.nTSWorkedHH = nvl(qTimeT2.TOTHRS, 0)

	if (.nHRSPDY != 0 and qTimeT2.TOTHRS != 0)
		.nTSWorkedDays = round(qTimeT2.TOTHRS/.nHRSPDY,2)
	endif 
		
	use in select("qTimeT2")
endif 	

store null to loBizTimetmp
return

*==========================================================
protected procedure GetYTDBankDeposit()
local lcWhere, loBizTimeDt 

*** set step on
store "" to lcWhere
store null to loBizTimeDt 

if empty(.dStartDt) or empty(.dEndDt)
	return 
endif 	

text to lcWhere textmerge noshow pretext 1+4
	T_PERSID = PERSNUMBER 
	AND LEFT(T_OPT,3) = 'TOPTION'
	AND T_EFFDT BETWEEN 'STARTDATE' AND 'ENDDATE' 
endtext 

lcWhere = strtran(lcWhere,"PERSNUMBER", transform(.nPersId)) 
lcWhere = strtran(lcWhere,"TOPTION", left(.cDepositOPT,3))
lcWhere = strtran(lcWhere,"STARTDATE", dtoc(.dStartDt)) 
lcWhere = strtran(lcWhere,"ENDDATE", dtoc(.dEndDt)) 

loBizTimeDt = Getbiz("TIMEDT")
loBizTimeDt = loBizTimeDt.GetList("/CURSOR=qTimeDt1", ;
					"SUM(T_HOURS) AS T_HOURS", lcWhere)

if !isnull(loBizTimeDt)
	.nYTDBankDeposit = nvl(qTimeDt1.T_HOURS, 0)

	use in select("qTimeDt1")
endif 	

store null to loBizTimeDt
return

*=========================================================
#define GET_SICK_DAYS_BY_PLAN 
*=========================================================
protected procedure GetSickDays_MALSEB()
*** Gets the current year's number of sick days, 
* to which an employee is entitled, maximum 10 
* (+/-) 4 waiting period in days 
*

*** Add the days worked from timesheet 
.nTotDaysWorked = .nTotDaysWorked + .nTSWorkedDays 

if .nTotDaysWorked = 0
	this.WriteLog("GetSickDays_MALSEB() - " + ;
			"nTotDaysWorked = " + transform(nTotDaysWorked))
	return 
endif 	

*** Get the MAX number of working days per union  
if .nTotDaysWorked > this.nSEB_DAYSWKPERYEAR
	.nMAXDaysWorked = this.nSEB_DAYSWKPERYEAR
	.nTotDaysWorked = .nMAXDaysWorked 
endif 	

*** 10 sick days / 260 days per year 
.nYTDSickDays=(this.nMALSEB_SICKDAYSPERYEAR/this.nSEB_DAYSWKPERYEAR)*;
			.nTotDaysWorked 

*** Sum up the totals  
.nTotSickDays = .nYTDSickDays
.nTotSickHH = this.ConvertTimeBankUnit(.nTotSickDays)

*** Entitlement Year Yo Date - real time 
.nEntitleHH = (.nTotSickHH - .nYTDBankDeposit)
if (.nHRSPDY != 0)
	.nEntitleDays = .nEntitleHH / .nHRSPDY
endif 

return
endproc

*=========================================================
protected procedure GetSickDays_MALATT()
*** Gets the current year's number of sick days, 
* to which an employee is entitled, maximum 10 
* (+/-) 4 waiting period in days 
*
** set step on 

*** Add the days worked from timesheet 
.nTotDaysWorked = .nTotDaysWorked + .nTSWorkedDays 

if .nTotDaysWorked = 0
	this.WriteLog("GetSickDays_MALATT() - " + ;
			"nTotDaysWorked = " + transform(.nTotDaysWorked))
	return 
endif 	

*** Get the MAX number of working days per union  
if .nTotDaysWorked > this.nATT_DAYSWKPERYEAR
	.nMAXDaysWorked = this.nATT_DAYSWKPERYEAR
	.nTotDaysWorked = .nMAXDaysWorked 
endif 	

*** 10 sick days / 260 days per year 
.nYTDSickDays=(this.nMALATT_SICKDAYSPERYEAR/this.nATT_DAYSWKPERYEAR)*;
			.nTotDaysWorked

*** Sum up the totals  
.nTotSickDays = .nYTDSickDays
.nTotSickHH = this.ConvertTimeBankUnit(.nTotSickDays)

*** Entitlement Year Yo Date - real time 
.nEntitleHH = (.nTotSickHH - .nYTDBankDeposit)
if (.nHRSPDY != 0)
	.nEntitleDays = .nEntitleHH / .nHRSPDY
endif 

return
endproc

*=========================================================
protected procedure GetSickDays_MALAPMCP()
*** Gets the current year's number of sick days, 
* to which an employee is entitled, maximum 10 
* (+/-) 4 waiting period in days 
* APMCP ratio (5/248.6) ~ .0201126307320998
*

*** set step on 
*** Add the days worked from timesheet 
.nTotDaysWorked = .nTotDaysWorked + .nTSWorkedDays 

if .nTotDaysWorked = 0
	this.WriteLog("GetSickDays_MALAPMCP() - " + ;
			"nTotDaysWorked = " + transform(nTotDaysWorked))
	return 
endif 	

*** Get the MAX number of working days per union  
if .nTotDaysWorked > this.nAPMCP_DAYSWKPERYEAR
	.nMAXDaysWorked = this.nAPMCP_DAYSWKPERYEAR
	.nTotDaysWorked = .nMAXDaysWorked 
endif 	

*** 10 sick days / 260 days per year 
.nYTDSickDays=(this.nMALAPMCP_SICKDAYSPERYEAR/this.nAPMCP_DAYSWKPERYEAR)*;
			.nTotDaysWorked

*** Sum up the totals  
.nTotSickDays = .nYTDSickDays
.nTotSickHH = this.ConvertTimeBankUnit(.nTotSickDays)

*** Entitlement Year Yo Date - real time 
.nEntitleHH = (.nTotSickHH - .nYTDBankDeposit)
if (.nHRSPDY != 0)
	.nEntitleDays = .nEntitleHH / .nHRSPDY
endif 

return
endproc

*=========================================================
protected procedure GetSickDays_MALGOUV()
*** Gets the current year's number of sick days, 
* to which an employee is entitled, maximum 12 
* (+/-) 4 waiting period in days 
* Assign 1 day for every >= 1/2 month of service 
*
if .nFullMonthOfService = 0
	this.WriteLog("GetSickDays_MALGOUV() - " + ;
		"nFullMonthOfService="+transform(.nFullMonthOfService))
	return 
endif 

.nTotSickDays = .nFullMonthOfService 
.nTotSickHH = .nTotSickHH + ;
		this.ConvertTimeBankUnit(.nTotSickDays)

return
endproc

*=========================================================
#define GET_MOBILE_DAYS_BY_PLAN 
*=========================================================
protected procedure GetSickDays_MOBSEB()
*** Gets the current year's number of mobile days, 
* to which an employee is entitled, maximum 6  
*

*** Add the days worked from timesheet 
.nTotDaysWorked = .nTotDaysWorked + .nTSWorkedDays 

if .nTotDaysWorked = 0
	this.WriteLog("GetSickDays_MOBSEB() - " + ;
			"nTotDaysWorked = " + transform(nTotDaysWorked))
	return 
endif 	

*** Get the MAX number of working days per union  
if .nTotDaysWorked > this.nSEB_DAYSWKPERYEAR
	.nMAXDaysWorked = this.nSEB_DAYSWKPERYEAR
	.nTotDaysWorked = .nMAXDaysWorked 
endif 	

*** 6 sick days / 260 days per year 
.nYTDSickDays=(this.nMOBSEB_MOBILEDAYSPERYEAR/this.nSEB_DAYSWKPERYEAR)*;
			.nTotDaysWorked 

*** Sum up the totals  
.nTotSickDays = .nYTDSickDays
.nTotSickHH = this.ConvertTimeBankUnit(.nTotSickDays)

*** Entitlement Year Yo Date - real time 
.nEntitleHH = (.nTotSickHH - .nYTDBankDeposit)
if (.nHRSPDY != 0)
	.nEntitleDays = .nEntitleHH / .nHRSPDY
endif 

return
endproc

*=========================================================
protected procedure GetSickDays_MOBATT()
*** Gets the current year's number of mobile days, 
* to which an employee is entitled, maximum 6  
*

*** Add the days worked from timesheet 
.nTotDaysWorked = .nTotDaysWorked + .nTSWorkedDays 

if .nTotDaysWorked = 0
	this.WriteLog("GetSickDays_MOBSEB() - " + ;
			"nTotDaysWorked = " + transform(nTotDaysWorked))
	return 
endif 	

*** Get the MAX number of working days per union  
if .nTotDaysWorked > this.nSEB_DAYSWKPERYEAR
	.nMAXDaysWorked = this.nSEB_DAYSWKPERYEAR
	.nTotDaysWorked = .nMAXDaysWorked 
endif 	

*** 6 sick days / 260 days per year 
.nYTDSickDays=(this.nMOBSEB_MOBILEDAYSPERYEAR/this.nSEB_DAYSWKPERYEAR)*;
			.nTotDaysWorked 

*** Sum up the totals  
.nTotSickDays = .nYTDSickDays
.nTotSickHH = this.ConvertTimeBankUnit(.nTotSickDays)

*** Entitlement Year Yo Date - real time 
.nEntitleHH = (.nTotSickHH - .nYTDBankDeposit)
if (.nHRSPDY != 0)
	.nEntitleDays = .nEntitleHH / .nHRSPDY
endif 

return
endproc

*=========================================================
#define GET_COMPENSATORY_TIME_DAYS_BY_PLAN 
*=========================================================
protected procedure GetSickDays_TSSEB()
*** Gets the current year's number of days, 
* of compensatory time to which an employee is 
* yearly entitled, maximum 5.  
*

*** Add the days worked from timesheet 
.nTotDaysWorked = .nTotDaysWorked + .nTSWorkedDays 

if .nTotDaysWorked = 0
	this.WriteLog("GetSickDays_TSSEB() - " + ;
			"nTotDaysWorked = " + transform(nTotDaysWorked))
	return 
endif 	

*** Get the MAX number of working days per union  
if .nTotDaysWorked > this.nSEB_DAYSWKPERYEAR
	.nMAXDaysWorked = this.nSEB_DAYSWKPERYEAR
	.nTotDaysWorked = .nMAXDaysWorked 
endif 	

*** 5 sick days / 260 days per year 
.nYTDSickDays=(this.nCOMPENSATORY_TIME_YEAR/this.nSEB_DAYSWKPERYEAR)*;
			.nTotDaysWorked 

*** Sum up the totals  
.nTotSickDays = .nYTDSickDays
.nTotSickHH = this.ConvertTimeBankUnit(.nTotSickDays)

*** Entitlement Year Yo Date - real time 
.nEntitleHH = (.nTotSickHH - .nYTDBankDeposit)
if (.nHRSPDY != 0)
	.nEntitleDays = .nEntitleHH / .nHRSPDY
endif 

return
endproc

*=========================================================
#define HELPER_METHODS 
*=========================================================
protected procedure TRound( tnNumber )
* Custom rounding procedure (SFPQ) for APMCP members 
* Rounding union rule 
* 		0.1	0.0
* 		0.2	0.0
* 		0.3	0.5
* 		0.4	0.5
* 		0.5	0.5
* 		0.6	0.5
* 		0.7	0.5
* 		0.8	1.0
* 		0.9	1.0
*
local lnMasterNum, lnRndNumber
*
lnMasterNum = 5 
lnRndNumber = 0

if tnNumber < 1 
	tnNumber = tnNumber*10
endif 

if tnNumber <= lnMasterNum 
	lnRndNumber = iif((abs(tnNumber-lnMasterNum)>2), ;
					0, rndup(tnNumber, 5))
else
	lnRndNumber = iif((abs(tnNumber-lnMasterNum)>2), ;
					rndup(tnNumber, 10), rnddn(tnNumber, 5))
endif 	

if lnRndNumber > 0 
	lnRndNumber = (lnRndNumber/10)
endif 

this.WriteLog("TRound() - " + ;
		"lnRndNumber = "+transform(lnRndNumber))

return lnRndNumber
endproc

*=========================================================
protected procedure GetFullMonthOfService()
*** Assign 1 day off if more than 1/2 of month 
* has been worked 
*
local lnFullMonthOfService, lnSchedHrsOfService 
store 0 to lnFullMonthOfService, lnSchedHrsOfService

*** Not worked at all in a given period 
if empty(.nMTDHoursWorked)
	return lnFullMonthOfService
endif 	

*** if we can not get his schedule for some reason
* we will use the monthly average as working hours 
lnSchedHrsOfService = .nMTDSchedToWork 
if lnSchedHrsOfService = 0 
	lnSchedHrsOfService = this.nMontlyAVGWorkHours
endif 
	
lnFullMonthOfService = iif(lnSchedHrsOfService >= ;
	round(lnSchedHrsOfService*this.nFullMonthServicePCT,0),1,0) 

return lnFullMonthOfService

*==========================================================
protected procedure ConvertTimeBankUnit(tnNumber)
*** Bank conversion unit procedure.
* If the bank in TBL is in HOURS we need to convert 
* the days in hours 
*
local lnCvtUnit
lnCvtUnit = 0

if !inlist(.cBankUnit,"H","J")
	this.WriteLog("ConvertTimeBankUnit() - " + ;
			"cBankUnit = "+transform(.cBankUnit))

	return lnCvtUnit 
endif 

if empty(tnNumber)
	this.WriteLog("ConvertTimeBankUnit() - " + ;
			"tnNumber = "+transform(tnNumber))

	return lnCvtUnit 
endif 

*** Convert DAYS -> HOURS
if "H"$.cBankUnit
	lnCvtUnit = .nHRSPDY * tnNumber 
endif 
	
*** Convert HOURS -> DAYS 	
if ("J"$.cBankUnit and .nHRSPDY != 0)
	lnCvtUnit = tnNumber / .nHRSPDY
endif 

return lnCvtUnit

*=========================================================
procedure CreateSickBankObject()
*** Object factory for vacation object 

local loR

loR = createobject("empty")

*** Id's 
addproperty(loR, "cBankId", "")
addproperty(loR, "cPlanId", "")
addproperty(loR, "cUnionId", "")
addproperty(loR, "nPersId", 0)

*** Plan Depot OPT 
addproperty(loR, "cDepositOPT", "")

*** Scheduler Id 
addproperty(loR, "cSchedId", "")

*** Seniority 
addproperty(loR, "nSenMths", 0)
addproperty(loR, "nSenYears", 0)

*** Counter for transactions 
addproperty(loR, "cTCNT", "")

*** Plan dates interval 
addproperty(loR, "dStartDt", {})
addproperty(loR, "dEndDt", {})
addproperty(loR, "dStartOfMonthDt", {})
addproperty(loR, "dEndOfMonthDt", {})
addproperty(loR, "dLYStartDt", {})
addproperty(loR, "dLYEndDt", {})

*** Effective Date 
addproperty(loR, "dEffDt", {})

*** Month To Date (MTD)
*** Entitlement calculation 
addproperty(loR, "nMTDSchedToWork", 0)
addproperty(loR, "nMTDHoursWorked", 0)
addproperty(loR, "nTotHoursWorked", 0)
addproperty(loR, "nTotDaysWorked", 0)
addproperty(loR, "nMAXDaysWorked", 0)
addproperty(loR, "nFullMonthOfService", 0)

*** Schedule average hours/day 
addproperty(loR, "nHRSPDY", 0)

*** Bank unit 
addproperty(loR, "cBankUnit", "")

*** Employee totals 
addproperty(loR, "nTotSickDays", 0)
addproperty(loR, "nTotSickHH", 0)
addproperty(loR, "nYTDSickDays", 0)
addproperty(loR, "nYTDSickHH", 0)

*** Employee entitlement 
addproperty(loR, "nEntitleDays", 0)
addproperty(loR, "nEntitleHH", 0)

*** Timesheet posting parameters  
addproperty(loR, "IsTSRealTime", .f.)
addproperty(loR, "dTSStartDt", {})
addproperty(loR, "dTSEndDt", {})
addproperty(loR, "cTSTCNT", "")
addproperty(loR, "nTSWorkedDays", 0)
addproperty(loR, "nTSWorkedHH", 0)

*** Year To Date deposit 
addproperty(loR, "nYTDBankDeposit", 0)

**** ERROR Handler 
addproperty(loR, "cERROR", "")

return loR

*=========================================================
procedure SpoolObjectProperties(toR As Object)
*** S.G. - Logs all object properies 
local lcAsisField

if isnull(toR)
	return 
endif 

with this 

if !._DEBUG or empty(.cLogPath) 
	return 
endif 

.WriteLog(" ... " + CRLF)
=amembers(gaRS, toR, 1)
for lnN = 1 to alen(gaRS, 1)
	lcAsisField = "toR." + trim(gaRS[lnN, 1])
	.WriteLog("SpoolObjectProperties() :: " + ;
		trim(lcAsisField) + " = " + ;
		transform(evaluate(lcAsisField)))
next 
endwith 

return 

*=========================================================
procedure WriteLog(pcLog)
*** DEBUG procedure 
local lcLogfile, lcStartLog 

with this 

if !._DEBUG or empty(.cLogPath) 
	return 
endif 
	
pcLog = trim(pcLog) + chr(13) + chr(10) 
lcLogfile = .cLogPath + "_" + ;
	leftto(lower(trim(transform(program()))),".")+".log"

lcStartLog=.cLogPath+.cLogStatus
if file(lcStartLog)
	=strtofile(transform(datetime()) + " - " + ;
			trim(pcLog), lcLogfile, .t.)
endif 

endwith 
return

enddefine 

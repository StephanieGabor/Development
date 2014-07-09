*=================================================================================
*  Program...........: CSickBank.PRG
*  Author............: Stefan Gabor 
*  Project...........: Carver Human Resources Zoo62
*  Created...........: November 08, 2013
*  Code page.........: 1252 (WINDOWS)
*  Copyright.........: (c) Carver Technologies 2013
*
*  Description.......: The class handles sick, mobile and compensatory 
*                    : days calculation. 
*                    : The class is build around the plan id. The key
*                    : point is that the class is currently called
*                    : from two different places and it is easily expandable.
*                    :    1) annual entitlement 
*                    :    2) time sheet posting. 
*                    :
* Specification      : 1 - The class handles real time entitlement for
*                    :     companies which do not use the payroll system 
*                    :     and the event is triggered from the timesheet
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
*                    : Banks In (1, 2, 5, 7)
*                    :
*                    :
*=================================================================================
*
*#########################################################

**** Class definition 
define class CSickBank as custom 

nFullMonthServicePCT = .5  			&& Precent given for full month of service 
nMontlyAVGWorkHours = 130				&& Monthly average working hours  
nAPMCP_DAYSWKPERYEAR = 248.6			&& APMCP maximum yearly working days
nMALAPMCP_SICKDAYSPERYEAR = 5			&& APMCP maximum sick days per year  
nGOUV_SICKDAYSPERYEAR = 12				&& GOUV maximum sick days per year
nSEB_DAYSWKPERYEAR = 260 				&& SEB maximum yearly working days 
nMALSEB_SICKDAYSPERYEAR = 10			&& SEB maximum sick days per year  
nMOBSEB_MOBILEDAYSPERYEAR = 6			&& SEB & ATT maximum mobile days per year  
nMALSEB_WAITINGPERIODDAYS= 4			&& SEB waiting period number of days 

nATT_DAYSWKPERYEAR = 260 				&& ATT maximum yearly working days 
nMALATT_SICKDAYSPERYEAR = 10			&& ATT maximum sick days per year  
nMALATT_WAITINGPERIODDAYS= 4			&& ATT waiting period number of days 
nRECAPMCP_STARTDATE= {1991/07/25} 	&& APMCP compensation start date  

nSEB_OVERTIMEDAYSPERYEAR = 5			&& SEB maximum overtime days per year  


*** Property 
cLogStatus 	= "startlog.txt" 			&& start logging 
cLogPath = "\TEMP\" 						&& log folder 
_DEBUG = .T.								&& set .T. for debugging 
oRS = null									&& Object container 

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
	this.WriteLog("PostEntitlement() - Invalid Object!")
	return loRS 
endif 

*** Validation of plan dates and timesheet dates 
if !this.ValidateEntitlement(toRS)
	this.SpoolObjectProperties(toRS)
	return toRS 
endif 

this.oRS = toRS
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
protected procedure ValidateEntitlement(toRS As Object)
* The timesheet dates must be within the plan date range 
* in order to propely do real time bank entitlement.
*
local llOk 
llOk = .t. 

if toRS.IsTSRealTime
	*** If TS start date is greater than the plan end date.
	if toRS.dTSStartDt > toRS.dEndDt
		llOk = .f.	
	endif 
endif 

if !llOk 
	toRS.cERROR="Plan date range & TS date range are not in sync."
endif 

return llOk

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
local loBizSched, ldSeniorField, lcMAXBalance, lcMINBalance
local llMonthlyCalc, llYearlyCalc
local loBizJobhist, loCsrJH

store null to loBizSched, loBizJobhist, loCsrJH
store "" to lcMAXBalance, lcMINBalance

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

loBizJobhist = GetBiz("JOBHIST")
if isnull(loBizJobhist)
	return
endif 

*** Many issue with different JOBHIST aliases 
* Get the latest instead ...  	
loCsrJH = loBizJobhist.GetPersAndJobhistByPersidAndDate( ;
			"/CURSOR=qqJH", "*", lnPersid, date())
if isnull(loCsrJH)
	return
endif 

*** Load union into object 
if empty(.cUnionId)
	.cUnionId = trim(qqJH.H_UNION)
endif	

*** Load schedule id into object 
if empty(.cSchedId)
	.cSchedId = trim(qqJH.H_SCHEDID)
endif 

*** Load pay group into object 
if empty(.cPayGRP)
	.cPayGRP = trim(qqJH.H_PAYGRP)
endif 

*** Load jobhist effdt into object 
if empty(.dEffDt)
	.dEffDt = qqJH.H_EFFDT 
endif 

*** Recuparation bank APMCP 
if inlist(.cUnionId, "APMCP")
	do case 
	case "APM35"$.cSchedId 
		.nRECAPMCP_DPY = 5
	case "RETR28"$.cSchedId 
		.nRECAPMCP_DPY = 4
	case "RETR21"$.cSchedId
		.nRECAPMCP_DPY = 3
	case "RETR14"$.cSchedId
		.nRECAPMCP_DPY = 2
	case "RETR4V"$.cSchedId
		.nRECAPMCP_DPY = 4
	endcase 
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

	.nGTSenYears = year(date())-year(ldSeniorField )
	.nGTSenMths = .nGTSenYears*12 
endif 

*** Hired date 
.dOriginalHiredDt = qqJH.E_ORIGHIRE 

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

*** Get MAX & MIN of the plan 
if val(qAPLAN.AP_BALMIN) != 0 ;
or empty(qAPLAN.AP_BALMIN)
	.nPlanBalMin = val(qAPLAN.AP_BALMIN)
else 
	lcMINBalance = trim(qAPLAN.AP_BALMIN)

	do case 
	case used("qJobhist") 
		lcMINBalance = strtran(upper(lcMINBalance),"JOBHIST","qJobhist")
	case used("vJobhist")
		lcMINBalance = strtran(upper(lcMINBalance),"JOBHIST","vJobhist")
	endcase 

	.nPlanBalMin = evaluate(lcMINBalance)
endif 	

if val(qAPLAN.AP_BALMAX) != 0 ;
or empty(qAPLAN.AP_BALMAX)
	.nPlanBalMax = val(qAPLAN.AP_BALMAX)
else 
	lcMAXBalance = trim(qAPLAN.AP_BALMAX)

	do case 
	case used("qJobhist") 
		lcMAXBalance = strtran(upper(lcMAXBalance),"JOBHIST","qJobhist")
	case used("vJobhist")
		lcMAXBalance = strtran(upper(lcMAXBalance),"JOBHIST","vJobhist")
	endcase 
	
	.nPlanBalMax = evaluate(lcMAXBalance)
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

*** Add pay calendar dates 
this.GetPayPeriod()

store null to loBizSched, loBizJobhist, loCsrJH
use in select("qqJH")

return 
endproc

*=========================================================
procedure GetPayPeriod()
*** The PIVOT query give the data on multiple lines, 
* however, we need to have the data summarized by 
* expense id and by PERSID. - S.G.
*  
local loBizPayno, loBizTimeDt
local lcPayNo, lcLastPayNo

store null to loBizPayno, loBizTimeDt
store "" to lcPayGRP, lcPayNo, lcLastPayNo

loBizPayno = GetBiz("PAYNO")
loBizTimeDt = GetBiz("TIMEDT")

lcPayNo = .cPayGRP + left(dtos(date()), 4) + "01"
.dPayPeriodStartDt=loBizPayno.GetValueById(lcPayNo, "PN_STARTDT")

lcLastPayNo = loBizPayno.GetLastPayNo(left(lcPayNo, 8), "/REG")
.dPayPeriodEndDt=loBizPayno.GetValueById(lcLastPayNo, "PN_ENDDT")

store null to loBizPayno, loBizTimeDt

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

*** Include current transactions from timesheet, 
* that are in progress of posting.  
if nvl(.IsTSRealTime,.f.) = .t.
	this.GetPRORATA_TIMESHEET()
	this.GetYTDBankDeposit()
endif 

*** Generic stuff that applies to plan 
this.GetPRORATAEntitlement(.dStartDt, .dEndDt)

*** Plan specific stuff 
if lower(pemstatus(this, "GetMethod_" + ;
	trim(.cPlanId), 3)) = "method"
	
	lcCommand = "this.GetMethod_"+trim(.cPlanId)+"()"
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
local lnSelect, lnCount, lnFMS, lcCSV 
store 0 to lnCount, lnFMS 

lnSelect = select()

** Create a cursor to hold the values 
use in select("qWKHours")
create cursor qWKHours (IYear N(4), IMonth N(3), ;
	StartDt D, EndDt D, nFMS N(2,0), ;
	SchedWKHrs N(14, 4), MthAVGHrs N(14, 4), RealWKHrs N(14, 4))

index on str(IYear,4) + str(IMonth, 2) tag TWKHRS  

if .dTSEndDt < tdStartDt  
	this.WriteLog("GetMethod_MALSEB() - " + ;
		"The plan start date is outside of the timesheet dates range!")

	.cERROR = "USR:1001"
	return 
endif 

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
	lcCSV = this.cLogPath + lower(alias()) + ".csv"
	copy to (lcCSV) type csv 
	.cTABLE = filetostr(lcCSV)
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

*** Timesheet containts dates from previous plan period 
if (year(.dTSEndDt)-year(.dTSStartDt))>0
	lcWhere = strtran(lcWhere,"STARTDATE", dtoc(JAN1(.dTSEndDt))) 
else	
	lcWhere = strtran(lcWhere,"STARTDATE", dtoc(.dTSStartDt)) 
endif
lcWhere = strtran(lcWhere,"ENDDATE", dtoc(.dTSEndDt)) 

*** Debug 
this.WriteLog("GetPRORATA_TIMESHEET() - " + ;
	"lcWhere = " + transform(lcWhere))

loBizTimetmp = Getbiz("TIMETMP1")
loBizTimetmp = loBizTimetmp.GetList("/CURSOR=qTimeT2", ;
					"sum(TT_HOURS) as TOTHRS", lcWhere)

if !isnull(loBizTimetmp)
	.nTSWorkedHH = nvl(qTimeT2.TOTHRS, 0)

	if (.nHRSPDY != 0 and qTimeT2.TOTHRS != 0)
		.nTSWorkedDays = qTimeT2.TOTHRS/.nHRSPDY
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
	AND T_OPT = 'TOPTION'
	AND T_EFFDT BETWEEN 'STARTDATE' AND 'ENDDATE' 
endtext 

lcWhere = strtran(lcWhere,"PERSNUMBER", transform(.nPersId)) 
lcWhere = strtran(lcWhere,"TOPTION", .cDepositOPT)
lcWhere = strtran(lcWhere,"STARTDATE", dtoc(.dStartDt)) 
lcWhere = strtran(lcWhere,"ENDDATE", dtoc(.dEndDt)) 

loBizTimeDt = Getbiz("TIMEDT")
loBizTimeDt = loBizTimeDt.GetList("/CURSOR=qTimeDt1", ;
					"SUM(T_HOURS) AS T_HOURS", lcWhere)

*** Debug 
this.WriteLog("GetYTDBankDeposit() - " + ;
	"lcWhere = " + transform(lcWhere))

if !isnull(loBizTimeDt)
	.nYTDBankDeposit = nvl(qTimeDt1.T_HOURS, 0)

	use in select("qTimeDt1")
endif 	

store null to loBizTimeDt
return

*=========================================================
protected procedure GetScheduleToWorkByPayPeriod
lparameters tnSickDaysPerYear, tnDaysWorkPerYear
*** Get how many days the employee is scheduled 
* to work in this pay period 
*
local loBizJobhist, lnSchedHoliHours
store null to loBizJobhist
store 0 to lnSchedHoliHours

if !inlist(.cBankId, "2", "7") and ;
empty(tnSickDaysPerYear) or empty(tnDaysWorkPerYear)
	return 
endif 	

if !empty(.dTSStartDt) and !empty(.dTSEndDt)

	*** It does not include paid holidays 
	*/ .nSchedDaysWkByPayPeriod = TimeWorked("DAYS", ;
	*/		.nPersId, .dTSStartDt, .dTSEndDt)
	*/.nSchedHoursWkByPayPeriod = TimeWorked("HOURS", ;
	*/		.nPersId, .dTSStartDt, .dTSEndDt)

	*** Scheduled to work by pay period 
	loBizJobhist = GetBiz("JOBHIST")
	if isnull(loBizJobhist)
		return 
	endif 

	.nSchedHoursWkByPayPeriod = loBizJobhist.GetTotalHours( ;
			"/PDHOURS/INCLINAC/INCLLTD/INCLALD", ;
			.nPersId, .dTSStartDt, .dTSEndDt)	
	
	lnSchedHoliHours = loBizJobhist.GetTotalHours( ;
			"/FHOURS/INCLINAC/INCLLTD/INCLALD", ;
			.nPersId, .dTSStartDt, .dTSEndDt)	

	if (.nSchedHoursWkByPayPeriod > 0 and .nHRSPDY != 0)
		.nSchedDaysWkByPayPeriod=.nSchedHoursWkByPayPeriod/.nHRSPDY
	endif 

	if (lnSchedHoliHours > 0 and .nHRSPDY != 0)
		.nSchedHolidays=lnSchedHoliHours/.nHRSPDY
	endif 

	*** GOUV 
	if inlist(.cBankId, "2") 
		.cCustomBankDeposit = iif(gcLang="F",;
				"1 jour >= 1/2 MCS","1 day >= 1/2 FMS")
		return 
	endif 
		
	*** RECAPMCP
	if inlist(.cBankId, "7") 
		.cCustomBankDeposit = iif(gcLang="F", ;
			lstr(.nRECAPMCP_DPY,0)+ ;
			" jours par année"," days per year")
	endif 

	.nRegularBankDeposit = ;
			(tnSickDaysPerYear/tnDaysWorkPerYear) * ;
				.nSchedHoursWkByPayPeriod

endif 
store null to loBizJobhist

return 

*=========================================================
#define GET_SICK_DAYS_BY_PLAN 
*=========================================================
protected procedure GetMethod_MALSEB()
*** Gets the current year's number of sick days, 
* to which an employee is entitled, maximum 10 
* (+/-) 4 waiting period in days 
*

*** Verify that the yearly entitlment has been completed.
if .dTSStartDt > .dEndDt
	this.WriteLog("GetMethod_MALSEB() - " + ;
		"The entitlement has not been rolled over!")

	.cERROR = "The entitlement has not been rolled over!"
endif 

*** Sick bank should not take the previous year hours  
*** Total days worked are populated by the timesheet  
.nTotDaysWorked = .nTotDaysWorked + .nTSWorkedDays 
.nTotHoursWorked = .nTotDaysWorked *.nHRSPDY

if .nTotDaysWorked = 0
	.cERROR="nTotDaysWorked = "+transform(.nTotDaysWorked)
	.cBatchNo = "MALSEB-ON HOLD"

	this.SpoolObjectProperties(this.oRS)
	return 
endif 	

*** Get the MAX number of working days per union  
if .nTotDaysWorked > this.nSEB_DAYSWKPERYEAR
	.nMAXDaysWorked = this.nSEB_DAYSWKPERYEAR
	
	.nTotDaysWorked = .nMAXDaysWorked 
	.nTotHoursWorked = .nTotDaysWorked *.nHRSPDY
endif 	

*** Add scheduled hours to work by pay period 
this.GetScheduleToWorkByPayPeriod( ;
	this.nMALSEB_SICKDAYSPERYEAR,this.nSEB_DAYSWKPERYEAR)

*** 10 sick days / 260 days per year 
.nWorkDPY = this.nSEB_DAYSWKPERYEAR
.nEntitleDPY = this.nMALSEB_SICKDAYSPERYEAR
.nYTDSickDays=(this.nMALSEB_SICKDAYSPERYEAR / ;
			this.nSEB_DAYSWKPERYEAR)*.nTotDaysWorked 

*** Sum up the totals  
.nTotSickDays = .nYTDSickDays
.nTotSickHH = this.ConvertTimeBankUnit(.nTotSickDays)

*** Do not allow more than maximum 
if round((.nTotSickHH-.nYTDBankDeposit),4) > round(.nRegularBankDeposit,4)
	.nCalculatedBankDeposit = .nTotSickHH 
	.nTotSickHH = (.nYTDBankDeposit+.nRegularBankDeposit)
endif 	

*** Entitlement Year Yo Date - real time 
.nEntitleHH = (.nTotSickHH - .nYTDBankDeposit)
if (.nHRSPDY != 0)
	.nEntitleDays = .nEntitleHH / .nHRSPDY
endif 

*** Entitlement adjustment between 
* regular and calculated. 
if .nCalculatedBankDeposit > 0 and ;
	.nCalculatedBankDeposit != .nRegularBankDeposit

	.nENTAdjHH = .nCalculatedBankDeposit - .nRegularBankDeposit
	if (.nHRSPDY != 0  and .nENTAdjHH != 0)
		.nENTAdjDays = .nENTAdjHH / .nHRSPDY
	endif 	
endif 

return
endproc

*=========================================================
protected procedure GetMethod_MALATT()
*** Gets the current year's number of sick days, 
* to which an employee is entitled, maximum 10 
* (+/-) 4 waiting period in days 
*

*** Verify that the yearly entitlment has been completed.
if .dTSStartDt > .dEndDt
	this.WriteLog("GetMethod_MALATT() - " + ;
		"The entitlement has not been rolled over!")

	.cERROR = "The entitlement has not been rolled over!"
endif 

*** set step on 
*** Sick bank should not take the previous year hours  
*** Total days worked are populated by the timesheet  
.nTotDaysWorked = .nTotDaysWorked + .nTSWorkedDays 
.nTotHoursWorked = .nTotDaysWorked *.nHRSPDY

if .nTotDaysWorked = 0
	.cERROR="nTotDaysWorked = "+transform(.nTotDaysWorked)
	.cBatchNo = "MALATT-ON HOLD"

	this.SpoolObjectProperties(this.oRS)
	return 
endif 	

*** Get the MAX number of working days per union  
if .nTotDaysWorked > this.nATT_DAYSWKPERYEAR
	.nMAXDaysWorked = this.nATT_DAYSWKPERYEAR

	.nTotDaysWorked = .nMAXDaysWorked 
	.nTotHoursWorked = .nTotDaysWorked *.nHRSPDY
endif 	

*** Add scheduled hours to work by pay period 
.nWorkDPY = this.nATT_DAYSWKPERYEAR
.nEntitleDPY = this.nMALATT_SICKDAYSPERYEAR
this.GetScheduleToWorkByPayPeriod( ;
	this.nMALATT_SICKDAYSPERYEAR,this.nATT_DAYSWKPERYEAR)

*** 10 sick days / 260 days per year 
.nYTDSickDays = this.nMALATT_SICKDAYSPERYEAR / ;
		this.nATT_DAYSWKPERYEAR*.nTotDaysWorked

*** Sum up the totals  
.nTotSickDays = .nYTDSickDays
.nTotSickHH = this.ConvertTimeBankUnit(.nTotSickDays)

*** Do not allow more than maximum 
if round((.nTotSickHH-.nYTDBankDeposit),4) > round(.nRegularBankDeposit,4)
	.nCalculatedBankDeposit = .nTotSickHH 
	.nTotSickHH = (.nYTDBankDeposit+.nRegularBankDeposit)
endif 	

*** Entitlement Year Yo Date - real time 
.nEntitleHH = (.nTotSickHH - .nYTDBankDeposit)
if (.nHRSPDY != 0)
	.nEntitleDays = .nEntitleHH / .nHRSPDY
endif 

*** Entitlement adjustment between 
* regular and calculated. 
if .nCalculatedBankDeposit > 0 and ;
	.nCalculatedBankDeposit != .nRegularBankDeposit

	.nENTAdjHH = .nCalculatedBankDeposit - .nRegularBankDeposit
	if (.nHRSPDY != 0 and .nENTAdjHH != 0)
		.nENTAdjDays = .nENTAdjHH / .nHRSPDY
	endif 	
endif 

return
endproc

*=========================================================
protected procedure GetMethod_MALAPMCP()
*** Gets the current year's number of sick days, 
* to which an employee is entitled, maximum 10 
* (+/-) 4 waiting period in days 
* APMCP ratio (5/248.6) ~ .0201126307320998
*
*** Verify that the yearly entitlment has been completed.
if .dTSStartDt > .dEndDt
	this.WriteLog("GetMethod_MALAPMCP() - " + ;
		"The entitlement has not been rolled over!")

	.cERROR = "The entitlement has not been rolled over!"
endif 

*** Sick bank should not take the previous year hours  
*** Total days worked are populated by the timesheet  
.nTotDaysWorked = .nTotDaysWorked + .nTSWorkedDays 
.nTotHoursWorked = .nTotDaysWorked *.nHRSPDY

if .nTotDaysWorked = 0
	.cERROR="nTotDaysWorked = "+transform(.nTotDaysWorked)
	.cBatchNo = "MALAPMCP-ON HOLD"

	this.SpoolObjectProperties(this.oRS)
	return 
endif 	

*** Get the MAX number of working days per union  
if .nTotDaysWorked > this.nAPMCP_DAYSWKPERYEAR
	.nMAXDaysWorked = this.nAPMCP_DAYSWKPERYEAR

	.nTotDaysWorked = .nMAXDaysWorked 
	.nTotHoursWorked = .nTotDaysWorked *.nHRSPDY
endif 	

*** Add scheduled hours to work by pay period 
.nWorkDPY = this.nAPMCP_DAYSWKPERYEAR
.nEntitleDPY = this.nMALAPMCP_SICKDAYSPERYEAR
this.GetScheduleToWorkByPayPeriod( ;
	this.nMALAPMCP_SICKDAYSPERYEAR,this.nAPMCP_DAYSWKPERYEAR)

*** 5 sick days / 248.60 days per year 
.nYTDSickDays=(this.nMALAPMCP_SICKDAYSPERYEAR / ;
		this.nAPMCP_DAYSWKPERYEAR)*.nTotDaysWorked

*** Sum up the totals  
.nTotSickDays = .nYTDSickDays
.nTotSickHH = this.ConvertTimeBankUnit(.nTotSickDays)

*** Do not allow more than maximum 
if round((.nTotSickHH-.nYTDBankDeposit),4) > round(.nRegularBankDeposit,4)
	.nCalculatedBankDeposit = .nTotSickHH 
	.nTotSickHH = (.nYTDBankDeposit+.nRegularBankDeposit)
endif 	

*** Entitlement Year Yo Date - real time 
.nEntitleHH = (.nTotSickHH - .nYTDBankDeposit)
if (.nHRSPDY != 0)
	.nEntitleDays = .nEntitleHH / .nHRSPDY
endif 

*** Entitlement adjustment between 
* regular and calculated. 
if .nCalculatedBankDeposit > 0 and ;
	.nCalculatedBankDeposit != .nRegularBankDeposit

	.nENTAdjHH = .nCalculatedBankDeposit - .nRegularBankDeposit
	if (.nHRSPDY != 0 and .nENTAdjHH != 0)
		.nENTAdjDays = .nENTAdjHH / .nHRSPDY
	endif 	
endif 

return
endproc

*=========================================================
protected procedure GetMethod_MALGOUV()
*** Gets the current year's number of sick days, 
* to which an employee is entitled, maximum 12 
* (+/-) 4 waiting period in days 
* Assign 1 day for every >= 1/2 month of service 
*
if .nFullMonthOfService = 0
	this.WriteLog("GetMethod_MALGOUV() - " + ;
		"nFullMonthOfService="+transform(.nFullMonthOfService))
endif 

*** The timesheet start date over the end date of the plan 
* and timesheet end date less than the start date of the plan 
if .dTSStartDt > .dEndDt 
	this.WriteLog("GetMethod_MALGOUV() - " + ;
		"The entitlement has not been rolled over!")

	.cERROR = "The entitlement has not been rolled over!"
endif 

.nTotSickDays = .nFullMonthOfService 
.nTotSickHH = .nTotSickHH + ;
		this.ConvertTimeBankUnit(.nTotSickDays)

*** Entitlement Year Yo Date - real time 
.nEntitleHH = (.nTotSickHH - .nYTDBankDeposit)
.nENTAdjHH = .nEntitleHH - .nRegularBankDeposit
if (.nHRSPDY != 0 and .nENTAdjHH != 0)
	.nEntitleDays = .nEntitleHH / .nHRSPDY
	.nENTAdjDays = .nENTAdjHH / .nHRSPDY
endif 

.nWorkDPY = this.nAPMCP_DAYSWKPERYEAR
.nEntitleDPY = this.nGOUV_SICKDAYSPERYEAR
*** Add scheduled hours to work by pay period 
.nRegularBankDeposit = ;
		iif((.cBankUnit="H"),.nEntitleHH,.nEntitleDays)
this.GetScheduleToWorkByPayPeriod( ;
	.nRegularBankDeposit, this.nGOUV_SICKDAYSPERYEAR)

return
endproc

*=========================================================
#define GET_MOBILE_DAYS_BY_PLAN 
*=========================================================
protected procedure GetMethod_MOBSEB()
*** Gets the current year's number of mobile days, 
* to which an employee is entitled, maximum 6  
*
*** Verify that the yearly entitlment has been completed.
if .dTSStartDt > .dEndDt
	this.WriteLog("GetMethod_MALATT() - " + ;
		"The entitlement has not been rolled over!")

	.cERROR = "The entitlement has not been rolled over!"
endif 

*** Add the days worked from timesheet 
.nTotDaysWorked = .nTotDaysWorked + .nTSWorkedDays 
.nTotHoursWorked = .nTotDaysWorked *.nHRSPDY

if .nTotDaysWorked = 0
	.cERROR="nTotDaysWorked = "+transform(.nTotDaysWorked)
	.cBatchNo = "MOBSEB-ON HOLD"

	this.SpoolObjectProperties(this.oRS)
	return 
endif 	

*** Get the MAX number of working days per union  
if .nTotDaysWorked > this.nSEB_DAYSWKPERYEAR
	.nMAXDaysWorked = this.nSEB_DAYSWKPERYEAR

	.nTotDaysWorked = .nMAXDaysWorked 
	.nTotHoursWorked = .nTotDaysWorked *.nHRSPDY
endif 	

*** Add scheduled hours to work by pay period 
.nWorkDPY = this.nSEB_DAYSWKPERYEAR
.nEntitleDPY = this.nMOBSEB_MOBILEDAYSPERYEAR
this.GetScheduleToWorkByPayPeriod( ;
	this.nMOBSEB_MOBILEDAYSPERYEAR,this.nSEB_DAYSWKPERYEAR)

*** 6 sick days / 260 days per year 
.nYTDSickDays = (this.nMOBSEB_MOBILEDAYSPERYEAR / ;
		this.nSEB_DAYSWKPERYEAR) * .nTotDaysWorked 

*** Sum up the totals  
.nTotSickDays = .nYTDSickDays
.nTotSickHH = this.ConvertTimeBankUnit(.nTotSickDays)

*** Do not allow more than maximum 
if round((.nTotSickHH-.nYTDBankDeposit),4) > round(.nRegularBankDeposit,4)
	.nCalculatedBankDeposit = .nTotSickHH 
	.nTotSickHH = (.nYTDBankDeposit+.nRegularBankDeposit)
endif 	

*** Entitlement Year Yo Date - real time 
.nEntitleHH = (.nTotSickHH - .nYTDBankDeposit)
if (.nHRSPDY != 0)
	.nEntitleDays = .nEntitleHH / .nHRSPDY
endif 

*** Entitlement adjustment between 
* regular and calculated. 
if .nCalculatedBankDeposit > 0 and ;
	.nCalculatedBankDeposit != .nRegularBankDeposit

	.nENTAdjHH = .nCalculatedBankDeposit - .nRegularBankDeposit
	if (.nHRSPDY != 0 and .nENTAdjHH != 0)
		.nENTAdjDays = .nENTAdjHH / .nHRSPDY
	endif 	
endif 

return
endproc

*=========================================================
protected procedure GetMethod_MOBATT()
*** Gets the current year's number of mobile days, 
* to which an employee is entitled, maximum 6  
*
*** Verify that the yearly entitlment has been completed.
if .dTSStartDt > .dEndDt
	this.WriteLog("GetMethod_MOBATT() - " + ;
		"The entitlement has not been rolled over!")

	.cERROR = "The entitlement has not been rolled over!"
endif 

*** Add the days worked from timesheet 
.nTotDaysWorked = .nTotDaysWorked + .nTSWorkedDays 
.nTotHoursWorked = .nTotDaysWorked *.nHRSPDY

if .nTotDaysWorked = 0
	.cERROR="nTotDaysWorked = "+transform(.nTotDaysWorked)
	.cBatchNo = "MOBATT-ON HOLD"

	this.SpoolObjectProperties(this.oRS)
	return 
endif 	

*** Get the MAX number of working days per union  
if .nTotDaysWorked > this.nSEB_DAYSWKPERYEAR
	.nMAXDaysWorked = this.nSEB_DAYSWKPERYEAR

	.nTotDaysWorked = .nMAXDaysWorked 
	.nTotHoursWorked = .nTotDaysWorked *.nHRSPDY
endif 	

*** Add scheduled hours to work by pay period 
.nWorkDPY = this.nSEB_DAYSWKPERYEAR
.nEntitleDPY = this.nMOBSEB_MOBILEDAYSPERYEAR
this.GetScheduleToWorkByPayPeriod( ;
	this.nMOBSEB_MOBILEDAYSPERYEAR,this.nSEB_DAYSWKPERYEAR)

*** 6 sick days / 260 days per year 
.nYTDSickDays = this.nMOBSEB_MOBILEDAYSPERYEAR / ;
		this.nSEB_DAYSWKPERYEAR * .nTotDaysWorked 

*** Sum up the totals  
.nTotSickDays = .nYTDSickDays
.nTotSickHH = this.ConvertTimeBankUnit(.nTotSickDays)

*** Do not allow more than maximum 
if round((.nTotSickHH-.nYTDBankDeposit),4) > round(.nRegularBankDeposit,4)
	.nCalculatedBankDeposit = .nTotSickHH 
	.nTotSickHH = (.nYTDBankDeposit+.nRegularBankDeposit)
endif 	

*** Entitlement Year Yo Date - real time 
.nEntitleHH = (.nTotSickHH - .nYTDBankDeposit)
if (.nHRSPDY != 0)
	.nEntitleDays = .nEntitleHH / .nHRSPDY
endif 

*** Entitlement adjustment between 
* regular and calculated. 
if .nCalculatedBankDeposit > 0 and ;
	.nCalculatedBankDeposit != .nRegularBankDeposit

	.nENTAdjHH = .nCalculatedBankDeposit - .nRegularBankDeposit
	if (.nHRSPDY != 0 and .nENTAdjHH != 0)
		.nENTAdjDays = .nENTAdjHH / .nHRSPDY
	endif 	
endif 

return
endproc

*=========================================================
#define GET_OVERTIME_BY_PLAN 
*=========================================================
protected procedure GetMethod_TSSEB()
*** Gets the current year's number of overtime days, 
* to which an employee is entitled, maximum 5. 
* TSSEB ratio (5/260) 
*
* OVERTIME - too complicated the client will handle manually 
*

return
endproc

*=========================================================
protected procedure GetMethod_TSAPMCP()
*** Gets the current year's number of overtime days, 
* to which an employee is entitled, maximum 5. 
* APMCP ratio (5/248.6) ~ .0201126307320998
*
*
* OVERTIME - too complicated the client will handle manually 
*

return
endproc

*=========================================================
#define GET_COMPENSATORY_TIME_DAYS_BY_PLAN 
*=========================================================
protected procedure GetMethod_RECAPMCP()
*** Gets the current year's number of days, 
* of compensatory time to which an employee is 
* yearly entitled, maximum 5.  
*
*** Verify that the yearly entitlment has been completed.
if .dTSStartDt > .dEndDt
	this.WriteLog("GetMethod_RECAPMCP() - " + ;
		"The entitlement has not been rolled over!")

	.cERROR = "The entitlement has not been rolled over!"
endif 

*** Add the days worked from timesheet 
.nTotDaysWorked = .nTotDaysWorked + .nTSWorkedDays 
.nTotHoursWorked = .nTotDaysWorked *.nHRSPDY

*** set step on 
if .nTotDaysWorked = 0
	.cERROR="nTotDaysWorked = "+transform(.nTotDaysWorked)
	.cBatchNo = "RECAPMCP-ON HOLD"

	this.SpoolObjectProperties(this.oRS)
	return 
endif 	

if .dOriginalHiredDt < this.nRECAPMCP_STARTDATE
	this.WriteLog("GetMethod_RECAPMCP() - " + ;
			"nRECAPMCP_STARTDATE = " + ;
			transform(this.nRECAPMCP_STARTDATE))
	return 
endif 

*** Get the MAX number of working days per union  
if .nTotDaysWorked > this.nAPMCP_DAYSWKPERYEAR
	.nMAXDaysWorked = this.nAPMCP_DAYSWKPERYEAR

	.nTotDaysWorked = .nMAXDaysWorked 
	.nTotHoursWorked = .nTotDaysWorked *.nHRSPDY
endif 	

*** Add scheduled hours to work by pay period 
.nWorkDPY = this.nAPMCP_DAYSWKPERYEAR
.nEntitleDPY = .nRECAPMCP_DPY

this.GetScheduleToWorkByPayPeriod( ;
	.nRECAPMCP_DPY,this.nAPMCP_DAYSWKPERYEAR)

*** 5 sick days / 260 days per year 
.nYTDSickDays = .nRECAPMCP_DPY / ;
		this.nAPMCP_DAYSWKPERYEAR * .nTotDaysWorked 

*** Sum up the totals  
.nTotSickDays = .nYTDSickDays
.nTotSickHH = this.ConvertTimeBankUnit(.nTotSickDays)

*** Do not allow more than maximum 
if round((.nTotSickHH-.nYTDBankDeposit),4) > round(.nRegularBankDeposit,4)
	.nCalculatedBankDeposit = .nTotSickHH 
	.nTotSickHH = (.nYTDBankDeposit+.nRegularBankDeposit)
endif 	

*** Entitlement Year Yo Date - real time 
.nEntitleHH = (.nTotSickHH - .nYTDBankDeposit)
if (.nHRSPDY != 0)
	.nEntitleDays = .nEntitleHH / .nHRSPDY
endif 

*** Entitlement adjustment between 
* regular and calculated. 
if .nCalculatedBankDeposit > 0 and ;
	.nCalculatedBankDeposit != .nRegularBankDeposit

	.nENTAdjHH = .nCalculatedBankDeposit - .nRegularBankDeposit
	if (.nHRSPDY != 0 and .nENTAdjHH != 0)
		.nENTAdjDays = .nENTAdjHH / .nHRSPDY
	endif 	
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
* has been worked.
* We need to take into account the transaction 
* from timesheet that are posted realtime.
*
local lnFullMonthOfService, lnSchedHrsOfService
store 0 to lnFullMonthOfService, lnSchedHrsOfService

*** Take into account TS 
if nvl(.IsTSRealTime,.f.) = .t. and ;
between(month(.dStartOfMonthDt),month(.dTSStartDt),month(.dTSEndDt))
	.nMTDHoursWorked = .nMTDHoursWorked + .nTSWorkedHH
endif 

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
	
lnFullMonthOfService = iif(.nMTDHoursWorked >= ;
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
addproperty(loR, "nGTSenMths", 0)
addproperty(loR, "nGTSenYears", 0)

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

*** Pay Calendar  
addproperty(loR, "cPayGRP", "")
addproperty(loR, "dPayPeriodStartDt", {})
addproperty(loR, "dPayPeriodEndDt", {})
addproperty(loR, "cPayCycle", "")
addproperty(loR, "cPayNo", "")
addproperty(loR, "nSFPQPayNo", 0)
addproperty(loR, "IsThe2ndPay", .f.)

*** Month To Date (MTD)
*** Entitlement calculation 
addproperty(loR, "nMTDSchedToWork", 0)
addproperty(loR, "nMTDHoursWorked", 0)
addproperty(loR, "nTotHoursWorked", 0)
addproperty(loR, "nTotDaysWorked", 0)
addproperty(loR, "nMAXDaysWorked", 0)
addproperty(loR, "nFullMonthOfService", 0)
addproperty(loR, "nRECAPMCP_DPY", 0)
addproperty(loR, "dOriginalHiredDt", {})

*** Schedule average hours/day 
addproperty(loR, "nHRSPDY", 0)
addproperty(loR, "nSchedDaysWkByPayPeriod", 0)
addproperty(loR, "nSchedHoursWkByPayPeriod", 0)
addproperty(loR, "nSchedHolidays", 0)
addproperty(loR, "nRegularBankDeposit", 0)
addproperty(loR, "nCalculatedBankDeposit", 0)
addproperty(loR, "cCustomBankDeposit", "")

*** Bank unit 
addproperty(loR, "cBankUnit", "")

*** Employee totals 
addproperty(loR, "nTotSickDays", 0)
addproperty(loR, "nTotSickHH", 0)
addproperty(loR, "nYTDSickDays", 0)
addproperty(loR, "nYTDSickHH", 0)
addproperty(loR, "nYTDVacDays", 0)
addproperty(loR, "nYTDVacHH", 0)
addproperty(loR, "nTotVacDays", 0)
addproperty(loR, "nTotVacHH", 0)

*** Employee entitlement 
addproperty(loR, "nEntitleDays", 0)
addproperty(loR, "nEntitleHH", 0)
addproperty(loR, "nENTAdjDays", 0)
addproperty(loR, "nENTAdjHH", 0)

*** Custom fields by Union 
*** APMCP & GOVT  
addproperty(loR, "nAPTotVacDays", 0)
addproperty(loR, "nAPTotVacHH", 0)
addproperty(loR, "nGTTotVacDays", 0)
addproperty(loR, "nGTTotVacHH", 0)

*** Timesheet posting parameters  
addproperty(loR, "IsTSRealTime", .f.)
addproperty(loR, "dTSStartDt", {})
addproperty(loR, "dTSEndDt", {})
addproperty(loR, "cTSTCNT", "")
addproperty(loR, "nTSWorkedDays", 0)
addproperty(loR, "nTSWorkedHH", 0)

*** Plan MIN & MAX
addproperty(loR, "nPlanBalMin", 0)
addproperty(loR, "nPlanBalMax", 0)

*** Year To Date deposit 
addproperty(loR, "nYTDBankDeposit", 0)

**** ERROR Handler 
addproperty(loR, "cERROR", "")
addproperty(loR, "cBatchNo", "")

**** FMS & Vacation rules tables 
addproperty(loR, "cTABLE", "")

**** Rules
addproperty(loR, "nWorkDPY", 0)
addproperty(loR, "nEntitleDPY", 0)

*** Vacation rule matrix 
addproperty(loR, "cRuleMatrix", "")
addproperty(loR, "nMatrixRecNo", 0)

return loR

*=========================================================
procedure SpoolObjectProperties(toR As Object)
*** S.G. - Logs all object properies 
local lcAsisField, lcMemo, loCursor
local lcUNIQID, lcDBF, lcKEY, lcAction
local lcOLDUSER, lcNEWUSER, ltOLDDTM, lcOLDPERS
local lcNEWPERS

store "" to lcMemo, lcMemoT, lcUNIQID, lcDBF, lcKEY 
store "" to lcAction, lcOLDUSER, lcNEWUSER, lcNEWPERS
store {} to ltOLDDTM
store null to loCursor

if isnull(toR)
	return 
endif 

with this 

if !._DEBUG or empty(.cLogPath) 
	return 
endif 

=amembers(gaRS, toR, 1)
for lnN = 1 to alen(gaRS, 1)
	lcAsisField = "toR." + alltrim(gaRS[lnN, 1])
	lcMemo = lcMemo + ;
			trim(lcAsisField) + "=" + ;
			transform(evaluate(lcAsisField))+CRLF 
next 

*** set step on 
if !empty(lcMemo)
	lcMemo = strtran(lcMemo, "'", "''")

	lcUNIQID = uniqid()
	lcDBF = "TIMETMP"
	lcKEY = nvl(toR.cBatchNo, "")
	lcAction = "P" 
	lcOLDUSER = lstr(gnMyPersId)
	lcNEWUSER = name(gnMyPersId)
	lcOLDPERS = lstr(toR.nPersId,0)
	ltOLDDTM = datetime()
	lcNEWPERS = nvl(toR.cBankId, "")

	text to lcSql textmerge noshow pretext 1+4 
		insert into Audite ( 
				AU_DBF, AU_KEY, AU_ACTION, 
				AU_OLDUSER, AU_OLDDTM, AU_OLDPERS, 
				AU_OLDVAL, AU_NEWUSER, AU_NEWDTM, 
				AU_NEWPERS, AU_NEWVAL, AU_UNIQID, AU_DATE)
		values 
				('<<lcDBF>>', '<<lcKEY>>', '<<lcAction>>', 
					'<<lcOLDUSER>>', '<<ltOLDDTM>>', '<<lcOLDPERS>>', 
					'<<lcMemo>>', '<<lcNEWUSER>>', '<<ltOLDDTM>>', 
					'<<lcNEWPERS>>', '<<lcMemoT>>', '<<lcUNIQID>>', '<<ltOLDDTM>>')
	endtext 

	*** Pass trough SQL 
	loCursor = goDatamgr.SQLexec("", ;
			set("datasession"),goDataMgr.ConnectionHandle,;
			lcSql)

	if isnull(loCursor)
		return 
	endif 	
endif 

store null to loCursor
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

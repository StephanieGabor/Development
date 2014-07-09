*=================================================================================
*  Program...........: CVacationBank.PRG
*  Author............: Stefan Gabor 
*  Project...........: Carver Human Resources Zoo62
*  Created...........: Spetember 10, 2013
*  Code page.........: 1252 (WINDOWS)
*  Copyright.........: (c) Carver Technologies 2013
*
*  Description.......: The class handles vacations days calculation. 
*							: The class is roughly divided into 4 main components:
*							: 1 - BuiltVacationTable_XXXXXXX 
*							:		This is the procedure which builds a matrix/table 
*                    :     based on customer specific parameters. 
*                    :     Some of these parameters include: 
*							:		years of seniority, number of hours/days last year, etc. 
*							: 2 - GetVacationDays_XXXXXXX
*							:		The procedure uses the matrix/table "TVac" and returns 
*                    :     the number of vacation days to which an employee 
*                    :     is entitled.  
*							: 3 - SaveToCursor_XXXXXXX
*							:		The procedure saves the vacation entitlement matrix 
*                    :     into a FOXPRO table.
*							: 4 - GetYTDVacDays__XXXXXXX
*                    : 		The procedure scans the vacation entitlement table, 
*                    :     "TVac", and returns the number of vacations days 
*                    :     to which an employee is entitled to date.
*                    :
*                    :
*  Technical Notes...: Model
*                    :		GetVacationDays_XXXXXXX()
*                    :		BuildVacationTable_XXXXXXX()
*                    :		SaveToCursor_XXXXXXX()
*                    :		GetYTDVacDays__XXXXXXX()
*                    :
*							: GetVacationDays_XXXXXXX, where "XXXXXXX" 
*							: 		must be replaced by the plan id. The procedure
*							:		calculates the current year's number of vacation days, 
*							:		to which an employee is entitled, based on its seniority.
*                    :
*                    : BuildVacationTable_XXXXXXX(), where "XXXXXXX" 
*							: 		must be replaced by the plan id. The procedure
*							: 		is used to build a vacation entitlement matrix and 
*							:		saves it to table "TVac".
*                    :
*                    : SaveToCursor_XXXXXXX(), where "XXXXXXX" 
*							: 		must be replaced by the plan id.
*							:		Vacation days are calculated based on a given matrix. 
* 							:		The matrix is stored into a table "TVac" which is used  
* 							:		to get the number of vacation days/hours for an employee.
*                    :
*							: GetYTDVacDays__XXXXXXX(), where "XXXXXXX" 
*							: 		must be replaced by the plan id.
*							: 		Get the YTD (Year To Date) entitlement of number 
*							:		of vacation days from the vacation matrix (table TVac). 
*                    :
*                    :
*                    :
*                    : Banks In (3, 4, 8)
*                    :
*                    :
*
*=================================================================================
*
*#########################################################
define class CVacationBank as custom 
*#########################################################

*** Properties 
lIsBuildAllBanksOnce = .t. 		&& Build all vacation banks at the same time  
nFullMonthServicePCT = .5  		&& Precent given for full month of service 
nMontlyAVGWorkHours = 130			&& Monthly average working hours  
nSEB_DAYSWKPERYEAR = 260 			&& SEB maximum yearly working days 
nAPMCP_DAYSWKPERYEAR = 248.6		&& APMCP maximum yearly working days
nWorkDaysYear = 0.0 					&& How many working days in a year 
cVacBankCursor = ""					&& Place holder for TVac cursor 

nSEB_UNION_LEAVE_DAYS=30			&& Union leave SEB 
nAPMCP_UNION_LEAVE_DAYS=25			&& Union leave APMCP
nSEB_FLEX_HOURS=6.5					&& Flex SEB 

cLogStatus 	= "startlog.txt" 		&& start logging 
cLogPath = "\TEMP\" 					&& log folder 
_DEBUG = .T.							&& set .T. for debugging 
oRS = null								&& Object container 

*=========================================================
#define MAIN_ENTRY    
*=========================================================
procedure DepositBeginningOfYear(toRS As Object)
*** This is the main entry in the program and it matches 
* the event which trigger this action. 
* For example, in this case, the action is triggered from 
* Plans:Banks\Entitlement screen.
*
* PARAMETERS:
*		tnSenMths - number of months of seniority
*
if type("toRS")!="O" or isnull(toRS)
	this.WriteLog("DepositBeginningOfYear() - Invalid Object!")
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
procedure GetVacBankObject(tnSenMths)
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

if !used("qAPLAN") and !used("qAPLAN104")
	this.WriteLog("GetVacBankObject() - qAPLAN not opened!")
	return loRS 
endif 

*** set step on 
if empty(tnSenMths) 
	this.WriteLog("GetVacBankObject() - tnSenMths = " + ;
						transform(tnSenMths) )
	return loRS 
endif 

if !used("qAPLAN") and used("qAPLAN104")
	select * from qAPLAN104 ;
	into cursor qAPLAN
	go top in qAPLAN
endif 

*** Create a Sick bank object 
loRS = this.CreateVacBankObject()
with loRS 
	*** Initialize some properties of the object 
	this.SetVacBankProperty(tnSenMths) 
endwith 

*** Debug 
*** this.SpoolObjectProperties(loRS)

return loRS 
endproc

*==========================================================
protected procedure SetVacBankProperty(tnSenMths)
*** Populate the objects' constants 
*
local loBizSched, ldSeniorField 
local llMonthlyCalc, llYearlyCalc
local loBizJobhist, loCsrJH

store null to loBizSched, loBizJobhist, loCsrJH

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
	.cDepositOPT = trim(qAPLAN.AP_BANKID)
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

.nSenMths = tnSenMths 
.nSenYears = int(tnSenMths/12)

*** Different date field for seniority 
if !empty(qAPLAN.AP_SENDT)
	ldSeniorField = upper(trim(qAPLAN.AP_SENDT))
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

*** Get the counter
.cTCNT = trim(qAPLAN.AP_YTCNT)

*** Get MAX & MIN of the plan 
if val(qAPLAN.AP_BALMIN) != 0 ;
or empty(qAPLAN.AP_BALMIN)
	.nPlanBalMin = val(qAPLAN.AP_BALMIN)
else 
	.nPlanBalMin = evaluate(qAPLAN.AP_BALMIN)
endif 	
if val(qAPLAN.AP_BALMAX) != 0 ;
or empty(qAPLAN.AP_BALMAX)
	.nPlanBalMax = val(qAPLAN.AP_BALMAX)
else 
	.nPlanBalMax = evaluate(qAPLAN.AP_BALMAX)
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

*=========================================================
protected procedure GetScheduleToWorkByPayPeriod
lparameters tnVacUnitPerYear, tnDaysWorkPerYear
*** Get how many days the employee is scheduled 
* to work in this pay period 
*
local loBizJobhist, lnSchedHoliHours 
store null to loBizJobhist
store 0 to lnSchedHoliHours

*** set step on 
if empty(.dPayPeriodStartDt) or empty(.dPayPeriodEndDt)
	return 
endif 	

if !empty(.dPayPeriodStartDt) and !empty(.dPayPeriodEndDt)
	*** Scheduled to work by pay period 
	loBizJobhist = GetBiz("JOBHIST")
	if isnull(loBizJobhist)
		return 
	endif 

	.nSchedHoursWkByPayPeriod = loBizJobhist.GetTotalHours( ;
			"/HOURS/INCLINAC/INCLLTD/INCLALD", ;
			.nPersId, .dPayPeriodStartDt, .dPayPeriodEndDt)	

	lnSchedHoliHours = loBizJobhist.GetTotalHours( ;
			"/FHOURS/INCLINAC/INCLLTD/INCLALD", ;
			.nPersId, .dPayPeriodStartDt, .dPayPeriodEndDt)	

	if (.nSchedHoursWkByPayPeriod > 0 and .nHRSPDY != 0)
		.nSchedDaysWkByPayPeriod=.nSchedHoursWkByPayPeriod/.nHRSPDY
		.nTSWorkedHH = .nSchedHoursWkByPayPeriod
	endif 

	if (lnSchedHoliHours > 0 and .nHRSPDY != 0)
		.nSchedHolidays=lnSchedHoliHours/.nHRSPDY
	endif 	

	*** APMCP vs GOVNT 
	if inlist(.cBankId, "3") and inlist(.cUnionId, "APMCP")
		.cCustomBankDeposit = lstr(.nRECAPMCP_DPY,0) + ;
		iif(gcLang="F", " jours par année"," days per year")
	endif 

	.nRegularBankDeposit = nvl(tnVacUnitPerYear, 0.0)
endif 
store null to loBizJobhist

return 

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

*** set step on 
*** Generic stuff that applies to plan 
this.GetPRORATAEntitlement(.dLYStartDt,.dLYEndDt)
if this.lIsBuildAllBanksOnce
	this.BuildVacationTables()
endif 
	
*** Plan specific stuff 
if lower(pemstatus(this, "GetVacationDays_" + ;
	trim(.cPlanId), 3)) = "method"
	
	lcCommand = "this.GetVacationDays_"+trim(.cPlanId)+"()"
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

*** I assume that referential year starts from 1st day of the month
.dStartOfMonthDt = day1(tdStartDt) 

for lnCount = 1 to 12 
	.dStartOfMonthDt = gomonth(.dStartOfMonthDt, iif(lnCount=1,0,1))
	.dEndOfMonthDt = day1(gomonth(.dStartOfMonthDt,1))-1

	*** MYTD scheduled to worked
	.nMTDSchedToWork = TimeWorked("HOURS", lnPersId, ;
			.dStartOfMonthDt,.dEndOfMonthDt)

	*** MYTD hours worked
	.nMTDHoursWorked = TimeWorked("HOURS", lnPersId, ;
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

*=========================================================
#define GET_VACATION_DAYS_BY_PLAN 
*=========================================================
protected procedure GetVacationDays_VACSEB()
*** Gets the current year's number of vacation days, 
* to which an employee is entitled, based on its seniority.
* The program handles Year To Date (YTD) entitlement.
*
local lnTotDaysWork
store 0 to lnTotDaysWork

if .nTotDaysWorked = 0
	.cERROR = "USR:1000"
	.cBatchNo = "VACSEB-"+iif(gcLang="F","EN ATTENTE","ON HOLD")

	this.GetScheduleToWorkByPayPeriod( ;
		.nRegularBankDeposit, this.nSEB_DAYSWKPERYEAR)

	return 
endif 	

*** set step on 
*** Build the vacation matrix for the plan 
.cBatchNo = "VACSEB-OK"
if empty(this.cVacBankCursor)
	this.BuildVacationTable_VACSEB()
endif 

select TVac
go top in TVac 
if reccount("TVac") = 0 
	return 
endif 
	
* Get the MAX number of vacation days allowed by union 
lnTotDaysWork = .nTotDaysWorked 
.nMAXDaysWorked = .nTotDaysWorked
.nWorkDPY = this.nSEB_DAYSWKPERYEAR
if .nTotDaysWorked > this.nSEB_DAYSWKPERYEAR
	.nMAXDaysWorked = this.nSEB_DAYSWKPERYEAR
	lnTotDaysWork = .nMAXDaysWorked 
endif 	

do case 
case .nSenYears < 5 
	locate for TVac.TS5YLess = lnTotDaysWork and !eof()
case .nSenYears >= 5 and .nSenYears < 10 
	locate for TVac.TS5210Y = lnTotDaysWork and !eof()
case .nSenYears >= 10  
	locate for TVac.TS10YMore = lnTotDaysWork and !eof()
endcase 

*** Exact number of days 
if found()
	.nTotVacDays = TVac.VacDays
	.nTotVacHH = this.ConvertTimeBankUnit(.nTotVacDays)
else 
	*** Get YearToDate (YTD) entitlement
	this.GetYTDVacDays_VACSEB(lnTotDaysWork)
	.nYTDVacHH = this.ConvertTimeBankUnit(.nYTDVacDays)

	.nTotVacDays = .nYTDVacDays
	.nTotVacHH = .nYTDVacHH 

	*** Identical to SICK object 	
	.nEntitleDays = .nYTDVacDays
	.nEntitleHH = .nYTDVacHH  
endif 	

*** Maximum allowed vacation days per year  
this.SetMAXVacDays_VACSEB()

*** Add scheduled hours to work by pay period 
.nRegularBankDeposit = ;
		iif((.cBankUnit="H"),.nEntitleHH,.nEntitleDays)
this.GetScheduleToWorkByPayPeriod( ;
	.nRegularBankDeposit, this.nSEB_DAYSWKPERYEAR)

return
endproc

*==========================================================
protected procedure GetVacationDays_VACAPMCP()
*** Gets the current year's number of vaction days, 
* to which an employee is entitled, based on its seniority.
* The program handles Year To Date (YTD) entitlement.
*
* Relationship between SFPQ(APMCP) and Government employees
*		APMCP - gets values from row: Écart SFPQ
*
*	Année			1	12	13	14	15	16	17	18	19	20	21	22	23	24	25
*	Ans			1	12	13	14	15	16	17	18	19	20	21	22	23	24	25
*	SFPQ			20	21	21	22	22	23	23	24	24	25	25	25	25	25	30
*	Gouv			20	20	20	20	20	20	21	21	22	22	23	23	24	24	25
*	Écart SFPQ	0	1	1	2	2	3	2	3	2	3	2	2	1	1	5
*	Écart Hrs	0	7	7	14	14	21	14	21	14	21	14	14	7	7	35
*
local lnTotDaysWork
store 0 to lnTotDaysWork 

if empty(.nTotDaysWorked) or empty(.nTotHoursWorked)
	.cBatchNo = "VACAPMCP-"+iif(gcLang="F","EN ATTENTE","ON HOLD")
	.cERROR = "USR:1000"

	this.GetScheduleToWorkByPayPeriod ( ;
		.nRegularBankDeposit, this.nAPMCP_DAYSWKPERYEAR)

	return 
endif 	

*** set step on 
*** Build the vacation matrix for the plan 
.cBatchNo = "VACAPMCP-OK"
if empty(this.cVacBankCursor)
	this.BuildVacationTable_VACAPMCP()
endif 

select TVac 
go top in TVac 

* Get the MAX number of vacation days allowed by union 
lnTotDaysWork = .nTotDaysWorked 
.nMAXDaysWorked = .nTotDaysWorked
.nWorkDPY = this.nAPMCP_DAYSWKPERYEAR
if .nTotDaysWorked > this.nAPMCP_DAYSWKPERYEAR
	.nMAXDaysWorked = this.nAPMCP_DAYSWKPERYEAR
	lnTotDaysWork = .nMAXDaysWorked
endif 	

do case 
case .nSenYears < 12
	locate for TVac.TA12YLess = lnTotDaysWork and !eof()
case .nSenYears >= 12 and .nSenYears =< 13 
	locate for TVac.TA12213Y = lnTotDaysWork and !eof()
case .nSenYears >= 14 and .nSenYears =< 15 
	locate for TVac.TA14215Y = lnTotDaysWork and !eof()
case .nSenYears >= 16 and .nSenYears =< 17 
	locate for TVac.TA16217Y = lnTotDaysWork and !eof()
case .nSenYears >= 18 and .nSenYears =< 19
	locate for TVac.TA18219Y = lnTotDaysWork and !eof()
case .nSenYears >= 20 and .nSenYears =< 24
	locate for TVac.TA20224Y = lnTotDaysWork and !eof()
case .nSenYears >= 25 
	locate for TVac.TA25YMore = lnTotDaysWork and !eof()
endcase 

*** Exact number of days 
if found()
	.nAPTotVacDays = TVac.VacDays
	.nAPTotVacHH = this.ConvertTimeBankUnit(.nAPTotVacDays)
else 
	*** Get YearToDate (YTD) entitlement 
	this.GetYTDVacDays_VACAPMCP(lnTotDaysWork)

	.nAPTotVacDays = .nYTDVacDays
	.nAPTotVacHH = this.ConvertTimeBankUnit(.nAPTotVacDays)
endif 	

*** Get Govmnt (YTD) entitlement 
this.GetYTDVacDays_VACGOUV(lnTotDaysWork)
.nGTTotVacDays = .nYTDVacDays
.nGTTotVacHH = this.ConvertTimeBankUnit(.nGTTotVacDays)

*** If did not work a full year do not allow
* any vacation days.
if (.nAPTotVacDays - .nGTTotVacDays) > 0 
	.nTotVacDays = (.nAPTotVacDays - .nGTTotVacDays)
	.nTotVacHH = this.ConvertTimeBankUnit(.nTotVacDays)
	
	*** Identical to SICK object 	
	.nEntitleDays = .nTotVacDays
	.nEntitleHH = .nTotVacHH
endif 

*** Maximum allowed vacation days per year  
this.SetMAXVacDays_VACAPMCP()

*** Get the EXTRA vacation days 
this.GetAPMCPExtraDays()

*** Add scheduled hours to work by pay period 
.nRegularBankDeposit = ;
		iif((.cBankUnit="H"),.nEntitleHH,.nEntitleDays)
this.GetScheduleToWorkByPayPeriod( ;
	.nRegularBankDeposit, this.nAPMCP_DAYSWKPERYEAR)

return
endproc

*==========================================================
protected procedure GetVacationDays_VACGOUV()
*** Gets the current year's number of vaction days, 
* to which an employee is entitled, based on its seniority.
* The program handles Year To Date (YTD) entitlement.
*
local lnTotDaysWork
store 0 to lnTotDaysWork 

if .nTotDaysWorked = 0
	.cERROR = "USR:1000"
	.cBatchNo="VACGOUV-"+iif(gcLang="F","EN ATTENTE","ON HOLD")

	this.GetScheduleToWorkByPayPeriod( ;
		.nRegularBankDeposit, this.nAPMCP_DAYSWKPERYEAR)

	return 
endif 	

*** Build the vacation matrix for the plan 
.cBatchNo = "VACGOUV-OK"
if empty(this.cVacBankCursor)
	this.BuildVacationTable_VACGOUV()
endif 

select TVac 
go top in TVac 

* Get the MAX number of vacation days allowed by union 
lnTotDaysWork = .nTotDaysWorked
.nMAXDaysWorked = .nTotDaysWorked
.nWorkDPY = this.nAPMCP_DAYSWKPERYEAR
if .nTotDaysWorked > this.nAPMCP_DAYSWKPERYEAR
	.nMAXDaysWorked = this.nAPMCP_DAYSWKPERYEAR
	lnTotDaysWork = .nMAXDaysWorked
endif

do case 
case .nSenYears > 1 and .nSenYears < 17 
	locate for TVac.TG1217Y = lnTotDaysWork and !eof()
case inlist(.nSenYears, 17, 18) 
	locate for TVac.TG17218Y = lnTotDaysWork and !eof()
case inlist(.nSenYears, 19, 20) 
	locate for TVac.TG19220Y = lnTotDaysWork and !eof()
case inlist(.nSenYears, 21, 22) 
	locate for TVac.TG21222Y = lnTotDaysWork and !eof()
case inlist(.nSenYears, 23, 24) 
	locate for TVac.TG23224Y = lnTotDaysWork and !eof()
case .nSenYears >= 25 
	locate for TVac.TG25YMore = lnTotDaysWork and !eof()
endcase 

*** Exact number of days 
if found()
	.nTotVacDays = TVac.VacDays
	.nTotVacHH = this.ConvertTimeBankUnit(.nTotVacDays)
else 
	*** Get YearToDate (YTD) entitlement 
	this.GetYTDVacDays_VACGOUV(lnTotDaysWork)
	.nYTDVacHH = this.ConvertTimeBankUnit(.nYTDVacDays)

	.nTotVacDays = .nYTDVacDays
	.nTotVacHH = .nYTDVacHH 
	
	*** Identical to SICK object 	
	.nEntitleDays = .nTotVacDays
	.nEntitleHH = .nTotVacHH
endif 	

*** Maximum allowed vacation days per year  
this.SetMAXVacDays_VACGOUV()

*** Add scheduled hours to work by pay period 
.nRegularBankDeposit = ;
		iif((.cBankUnit="H"),.nEntitleHH,.nEntitleDays)
this.GetScheduleToWorkByPayPeriod( ;
	.nRegularBankDeposit, this.nAPMCP_DAYSWKPERYEAR)

return
endproc

*=========================================================
#define GET_YEAR_TO_DATE_VACATION_DAYS 
*** VACATT - is configured only trough the UI 
*=========================================================
protected procedure GetYTDVacDays_VACSEB(tnTotDaysWork)
*** Get the YTD(Year To Date) entitlement from 
* the vacation matrix
*
local lcField, lnEntitleVacDays
store "" to lcField 
store 0 to lnEntitleVacDays

if empty(tnTotDaysWork)
	this.WriteLog("GetYTDVacDays_VACSEB() - " + ;
			"tnTotDaysWork=" + transform(tnTotDaysWork))
	return lnEntitleVacDays
endif 	

do case 
case .nSenYears < 5 
	lcField = "TS5YLess"
case .nSenYears >= 5 and .nSenYears < 10 
	lcField = "TS5210Y"
case .nSenYears >= 10  
	lcField = "TS10YMore"
endcase 

if empty(lcField)
	return lnEntitleVacDays
endif 	

select TVac 
go top in TVac 
scan 
	if &lcField > tnTotDaysWork
		exit 
	endif 

	lnEntitleVacDays = TVac.VacDays 
endscan  

*** Update the object 
.nYTDVacDays = lnEntitleVacDays
.nMatrixRecNo = recno("TVac")

return 
endproc

*==========================================================
protected procedure SetMAXVacDays_VACSEB()
*** Get the total allowed per year
* based on seniority 
*
local lnSelect 

if !used("TVac")
	return 
endif 	

lnSelect = select()

*** Get the MAX vacation days 
do case 
case .nSenYears < 5 
	locate for TVac.TS5YLess = 0 and !eof()
case .nSenYears >= 5 and .nSenYears < 10 
	locate for TVac.TS5210Y = 0 and !eof()
case .nSenYears >= 10  
	locate for TVac.TS10YMore = 0 and !eof()
endcase 

if !bof()
	skip - 1
endif 
.nEntitleDPY = nvl(TVac.VacDays, 0)

select (lnSelect)

return 
endproc

*==========================================================
protected procedure GetYTDVacDays_VACAPMCP(tnTotDaysWork)
*** Get the YTD(Year To Date) entitlement from 
* the vacation matrix
*
local lcField, lnEntitleVacDays
store "" to lcField 
store 0 to lnEntitleVacDays

if empty(tnTotDaysWork)
	this.WriteLog("GetYTDVacDays_VACAPMCP() - " + ;
			"tnTotDaysWork=" + transform(tnTotDaysWork))
	return lnEntitleVacDays
endif 	

do case 
case .nSenYears < 12
	lcField = "TA12YLess"
case .nSenYears >= 12 and .nSenYears =< 13 
	lcField = "TA12213Y"
case .nSenYears >= 14 and .nSenYears =< 15 
	lcField = "TA14215Y"
case .nSenYears >= 16 and .nSenYears =< 17 
	lcField = "TA16217Y"
case .nSenYears >= 18 and .nSenYears =< 19
	lcField = "TA18219Y"
case .nSenYears >= 20 and .nSenYears =< 24
	lcField = "TA20224Y"
case .nSenYears >= 25 
	lcField = "TA25YMore"
endcase 

if empty(lcField)
	return lnEntitleVacDays
endif 	

select TVac 
go top in TVac 
scan 
	if &lcField > tnTotDaysWork
		exit 
	endif 

	lnEntitleVacDays = TVac.VacDays 
endscan  

*** Update the object 
.nYTDVacDays = lnEntitleVacDays
.nMatrixRecNo = recno("TVac")

return 
endproc

*==========================================================
protected procedure SetMAXVacDays_VACAPMCP()
*** Get the total allowed per year
* based on seniority 
local lnSelect 

if !used("TVac")
	return 
endif 	

lnSelect = select()

*** Get the MAX vacation days 
do case 
case .nSenYears < 12
	locate for TVac.TA12YLess = 0 and !eof()
case .nSenYears >= 12 and .nSenYears =< 13 
	locate for TVac.TA12213Y = 0 and !eof()
case .nSenYears >= 14 and .nSenYears =< 15 
	locate for TVac.TA14215Y = 0 and !eof()
case .nSenYears >= 16 and .nSenYears =< 17 
	locate for TVac.TA16217Y = 0 and !eof()
case .nSenYears >= 18 and .nSenYears =< 19
	locate for TVac.TA18219Y = 0 and !eof()
case .nSenYears >= 20 and .nSenYears =< 24
	locate for TVac.TA20224Y = 0 and !eof()
case .nSenYears >= 25 
	locate for TVac.TA25YMore = 0 and !eof()
endcase 

if !bof()
	skip - 1
endif 
.nEntitleDPY = nvl(TVac.VacDays, 0)

select(lnSelect)

return 
endproc

*==========================================================
protected procedure GetYTDVacDays_VACGOUV(tnTotDaysWork)
*** Get the YTD(Year To Date) entitlement from 
* the vacation matrix
*
local lcField, lnEntitleVacDays
store "" to lcField 
store 0 to lnEntitleVacDays

*** stefan
if empty(tnTotDaysWork)
	this.WriteLog("GetYTDVacDays_VACGOUV() - " + ;
			"tnTotDaysWork=" + transform(tnTotDaysWork))
	return lnEntitleVacDays
endif 	

do case 
case .nGTSenYears > 1 and .nGTSenYears < 17 
	lcField = "TG1217Y"
case inlist(.nGTSenYears, 17, 18) 
	lcField = "TG17218Y"
case inlist(.nGTSenYears, 19, 20) 
	lcField = "TG19220Y"
case inlist(.nGTSenYears, 21, 22) 
	lcField = "TG21222Y"
case inlist(.nGTSenYears, 23, 24) 
	lcField = "TG23224Y"
case .nGTSenYears >= 25 
	lcField = "TG25YMore"
endcase 

if empty(lcField)
	return lnEntitleVacDays
endif 	

select TVac 
go top in TVac 
scan 
	if &lcField > tnTotDaysWork
		exit 
	endif 

	lnEntitleVacDays = TVac.VacDays 
endscan  

*** Update the object 
.nYTDVacDays = lnEntitleVacDays
.nMatrixRecNo = recno("TVac")

return 
endproc

*=========================================================
protected procedure SetMAXVacDays_VACGOUV()
*** Get the total allowed per year
* based on seniority 
local lnSelect 

if !used("TVac")
	return 
endif 	

lnSelect = select()

*** Get the MAX vacation days 
do case 
case .nGTSenYears > 1 and .nGTSenYears < 17 
	locate for TVac.TG1217Y = 0 and !eof()
case inlist(.nGTSenYears, 17, 18) 
	locate for TVac.TG17218Y = 0 and !eof()
case inlist(.nGTSenYears, 19, 20) 
	locate for TVac.TG19220Y = 0 and !eof()
case inlist(.nGTSenYears, 21, 22) 
	locate for TVac.TG21222Y = 0 and !eof()
case inlist(.nGTSenYears, 23, 24) 
	locate for TVac.TG23224Y = 0 and !eof()
case .nGTSenYears >= 25 
	locate for TVac.TG25YMore = 0 and !eof()
endcase 

if !bof()
	skip - 1
endif 
.nEntitleDPY = nvl(TVac.VacDays, 0)

select(lnSelect)

return
endproc
*=========================================================
#define GET_UNION_LEAVE
*=========================================================
protected procedure GetVacationDays_SYNSEB()
*** Gets the current year's union leave paid 
* number of days, to which an employee that belongs 
* to a union is entitled.
*
if .dPayPeriodStartDt > .dStartDt or ;
this.nSEB_UNION_LEAVE_DAYS <= 0 
	.cERROR = "USR:1000"
	.cBatchNo="SYNSEB-"+iif(gcLang="F","EN ATTENTE","ON HOLD")

	this.GetScheduleToWorkByPayPeriod( ;
		.nRegularBankDeposit, this.nSEB_UNION_LEAVE_DAYS)

	return 
endif 	

.cBatchNo = "SYNSEB-OK"

.nTotVacDays = this.nSEB_UNION_LEAVE_DAYS
.nTotVacHH = this.ConvertTimeBankUnit(.nTotVacDays)

*** Identical to SICK object 	
.nEntitleDays = .nTotVacDays
.nEntitleHH = .nTotVacHH

.nWorkDPY = this.nSEB_DAYSWKPERYEAR
.nEntitleDPY = .nTotVacDays

*** Add scheduled hours to work by pay period 
.nRegularBankDeposit = ;
		iif((.cBankUnit="H"),.nEntitleHH,.nEntitleDays)
this.GetScheduleToWorkByPayPeriod( ;
	.nRegularBankDeposit, this.nSEB_UNION_LEAVE_DAYS)

return
endproc

*=========================================================
protected procedure GetVacationDays_SYNAPMCP()
*** Gets the current year's union leave paid 
* number of days, to which an employee that belongs 
* to a union is entitled.
*
if .dPayPeriodStartDt > .dStartDt or ;
this.nAPMCP_UNION_LEAVE_DAYS <= 0 
	.cERROR = "USR:1000"
	.cBatchNo="SYNAPMCP-"+iif(gcLang="F","EN ATTENTE","ON HOLD")

	this.GetScheduleToWorkByPayPeriod( ;
		.nRegularBankDeposit, this.nAPMCP_UNION_LEAVE_DAYS)

	return 
endif 	

.cBatchNo = "SYNAPMCP-OK"

.nTotVacDays = this.nAPMCP_UNION_LEAVE_DAYS
.nTotVacHH = this.ConvertTimeBankUnit(.nTotVacDays)

*** Identical to SICK object 	
.nEntitleDays = .nTotVacDays
.nEntitleHH = .nTotVacHH

.nWorkDPY = this.nAPMCP_DAYSWKPERYEAR
.nEntitleDPY = .nTotVacDays

*** Add scheduled hours to work by pay period 
.nRegularBankDeposit = ;
		iif((.cBankUnit="H"),.nEntitleHH,.nEntitleDays)
this.GetScheduleToWorkByPayPeriod( ;
	.nRegularBankDeposit, this.nAPMCP_UNION_LEAVE_DAYS)

return
endproc

*=========================================================
#define GET_VACATION_FLEX
*=========================================================
protected procedure GetVacationDays_FLEX()
*** Gets the current year's union leave paid 
* number of days, to which an employee that belongs 
* to a union is entitled.
*
if this.nSEB_FLEX_HOURS <= 0 
	this.WriteLog("GetVacationDays_FLEX() - " + ;
			"nSEB_FLEX_HOURS = " + ;
			transform(this.nSEB_FLEX_HOURS))
	return 
endif 	

.nTotVacDays = 1
.nTotVacHH = this.nSEB_FLEX_HOURS

.nEntitleDPY = .nTotVacDays

return 

*=========================================================
#define BUILD_VACATION_TABLES
*=========================================================
protected procedure GetVacationTableCursor()
*** Create vacation table cursor 
*
if used("TVac")
	use in ("TVac")
endif 
	
create cursor TVac (VacDays N(10,2), ;
		TA12YLess N(10,2), TA12213Y N(10,2), TA14215Y N(10,2), ;
		TA16217Y N(10,2), TA18219Y N(10,2), TA20224Y N(10,2), ;
		TA25YMore N(10,2), ;
		TS5YLess N(10,2), TS5210Y N(10,2), TS10YMore N(10,2), ;
		TG1217Y N(10,2), TG17218Y N(10,2), TG19220Y N(10,2), ;
		TG21222Y N(10,2), TG23224Y N(10,2), TG25YMore N(10,2)	 )

select TVac
index on VacDays tag tVAC 
go top in TVac

this.cVacBankCursor = trim(alias())

return 

*==========================================================
protected procedure BuildVacationTables()
*** The procedure builds a vacation matrix, TVac table, 
* for a given number of unions: SEB, APMCP, SFPQ.
* This process is completed at at the very beggining 
* and it is controlled by field: this.lIsBuildAllBanksOnce
* The reason for this is that some union vacations days 
* are based on some other union vacation days,  
* for example: APMCP & SFPQ
*
* The table is then used to get the number of vacation 
* days, for a given employee, based on two 
* parameters: 	- Years of seniority 
* 					- Year To Date (YTD) entitlement  
*
local lnSelect, llOk 
llOk = .t. 

lnSelect = select()

llOk = iif(llOk, this.GetVacationTableCursor(),.f.)
llOk = iif(llOk, this.BuildVacationTable_VACSEB(),.f.)
llOk = iif(llOk, this.BuildVacationTable_VACAPMCP(),.f.)
llOk = iif(llOk, this.BuildVacationTable_VACGOUV(),.f.)

if this._DEBUG 
	select TVac 
	go top in TVac
	lcFile = this.cLogPath + lower(trim(alias())) + ".csv" 
	copy to (lcFile) type csv 
	go top in TVac
	
	.cRuleMatrix = filetostr(lcFile)
endif 
	
select(lnSelect)

return
endproc

*==========================================================
protected procedure BuildVacationTable_VACSEB()
*** The procedure is used to build a vacation 
* entitlement matrix individually for a specific plan. 
*
* Reference: CC 2011-2014 SEB.PDF 
*		ANNEXE <I> - TABLE D'ACCUMULATION DE VACANCES
*
local lnCumulativeRatio, lnWorkHoursDay, lnMaxCounter
local lnSelect, lnI, lnN, lcFile
*
store 0 to lnCumulativeRatio, lnWorkHoursDay, lnMaxCounter
store 0 to lnI, lnN 
store "" to lcFile

lnSelect = select()

if !used("TVac")
	this.GetVacationTableCursor()
endif 	

select TVac
go top in TVac

replace all TS5YLess 	with 0 in TVac 
replace all TS5210Y 		with 0 in TVac 
replace all TS10YMore 	with 0 in TVac 

go top in TVac

for lnN = 1 to 3  
	lnCumulativeRatio = 0 
	this.nWorkDaysYear = 260  

	do case 
	case lnN = 1 	
		lnWorkHoursDay = 6.5 
	case lnN = 2 
		lnWorkHoursDay = 5.2  
	case lnN = 3 	
		lnWorkHoursDay = 4.3    
	endcase 

	lnMaxCounter = int(this.nWorkDaysYear/lnWorkHoursDay)
	lnMaxCounter = (lnMaxCounter/2) 

	for lnI = .5 to lnMaxCounter step .5 
		lnCumulativeRatio = lnCumulativeRatio + lnWorkHoursDay
		
		if (this.nWorkDaysYear-lnCumulativeRatio) < lnWorkHoursDay
			lnCumulativeRatio = lnCumulativeRatio + ;
					(this.nWorkDaysYear-lnCumulativeRatio)
		endif 	

		this.SaveToCursor_VACSEB(lnN, lnI, lnCumulativeRatio)
	next 
next 

if this._DEBUG 
	select TVac 
	go top in TVac
	lcFile = this.cLogPath + trim(.cPlanId) + ;
				lower(trim(alias())) + ".csv" 
	copy to (lcFile) type csv 

	.cRuleMatrix = filetostr(lcFile)


****** TO REMOVE ********************************
	lcFile = this.cLogPath + trim(.cPlanId) + ;
				lower(trim(alias())) + ".dbf" 
	go top in TVac
	copy to (lcFile)
	go top in TVac
*************************************************	
	
endif 
	
select(lnSelect)

return
endproc

*==========================================================
protected procedure BuildVacationTable_VACAPMCP()
*** The procedure is used to build a vacation 
* entitlement matrix
*
* Reference: APMCP CC2009 - 2014.PDF 
*		ANNEXE <D> - VACANCE - TABLE D'ACCUMULATION  
*
local lnCumulativeRatio, lnWorkHoursDay, lnMaxCounter
local lnSelect, lnI, lnN, lcFile
*
store 0 to lnCumulativeRatio, lnWorkHoursDay, lnMaxCounter
store 0 to lnI, lnN
store "" to lcFile

lnSelect = select()

if !used("TVac")
	this.GetVacationTableCursor()
endif 	

select TVac 
go top in TVac

replace all TA12YLess 	with 0 in TVac 
replace all TA12213Y 	with 0 in TVac
replace all TA14215Y 	with 0 in TVac
replace all TA16217Y 	with 0 in TVac
replace all TA18219Y 	with 0 in TVac
replace all TA20224Y 	with 0 in TVac
replace all TA25YMore 	with 0 in TVac
go top in TVac

for lnN = 1 to 7 
	lnCumulativeRatio = 0 
	this.nWorkDaysYear = 248.6 

	do case 
	case lnN = 1 	
		lnWorkHoursDay = 6.2 
	case lnN = 2 
		lnWorkHoursDay = 5.9  
	case lnN = 3 	
		lnWorkHoursDay = 5.6 
	case lnN = 4 
		lnWorkHoursDay = 5.4  
	case lnN = 5 
		lnWorkHoursDay = 5.2  
	case lnN = 6 	
		lnWorkHoursDay = 4.9 
	case lnN = 7 
		lnWorkHoursDay = 4.1   
	endcase 

	lnMaxCounter = int(this.nWorkDaysYear/lnWorkHoursDay)
	lnMaxCounter = (lnMaxCounter/2) 

	for lnI = .5 to lnMaxCounter step .5 
		lnCumulativeRatio = lnCumulativeRatio + lnWorkHoursDay
		
		if (this.nWorkDaysYear-lnCumulativeRatio) < lnWorkHoursDay
			lnCumulativeRatio = lnCumulativeRatio + ;
				(this.nWorkDaysYear-lnCumulativeRatio)
		endif 	

		this.SaveToCursor_VACAPMCP(lnN, lnI, lnCumulativeRatio)
	next 
next 

if this._DEBUG 
	select TVac 
	go top in TVac
	lcFile = this.cLogPath + trim(.cPlanId) + ;
			lower(trim(alias()))+ ".csv" 
	copy to (lcFile) type csv
	go top in TVac

	.cRuleMatrix = filetostr(lcFile)

endif 

*** Fix some exceptions in the vacation days matrix 	
this.Fix_24DAYS_EXEPTION()

select(lnSelect)

return
endproc

*==========================================================
protected procedure BuildVacationTable_VACGOUV()
*** The procedure is used to build a vacation 
* entitlement matrix individually for a specific plan. 
*
* Reference: UMANA - BANQUES.XLSX 
*		VACANCES - VACANCES GOUV 
*
local lnCumulativeRatio, lnWorkHoursDay, lnMaxCounter
local lnSelect, lnI, lnN, lcFile
*
store 0 to lnCumulativeRatio, lnWorkHoursDay, lnMaxCounter
store 0 to lnI, lnN
store "" to lcFile

lnSelect = select()

if !used("TVac")
	this.GetVacationTableCursor()
endif 	

select TVac
go top in TVac

replace all TG1217Y 		with 0 in TVac
replace all TG17218Y 	with 0 in TVac
replace all TG19220Y 	with 0 in TVac
replace all TG21222Y 	with 0 in TVac
replace all TG23224Y 	with 0 in TVac
replace all TG25YMore 	with 0 in TVac

go top in TVac

for lnN = 1 to 6 
	lnCumulativeRatio = 0 
	this.nWorkDaysYear = 248.6 

	do case 
	case lnN = 1 	
		lnWorkHoursDay = 6.2 
	case lnN = 2 
		lnWorkHoursDay = 5.9  
	case lnN = 3 	
		lnWorkHoursDay = 5.6 
	case lnN = 4 
		lnWorkHoursDay = 5.4  
	case lnN = 5 
		lnWorkHoursDay = 5.2  
	case lnN = 6 	
		lnWorkHoursDay = 4.9 
	endcase 

	lnMaxCounter = int(this.nWorkDaysYear/lnWorkHoursDay)
	lnMaxCounter = (lnMaxCounter/2) 

	for lnI = .5 to lnMaxCounter step .5 
		lnCumulativeRatio = lnCumulativeRatio + lnWorkHoursDay
		
		if (this.nWorkDaysYear-lnCumulativeRatio) < lnWorkHoursDay
			lnCumulativeRatio = lnCumulativeRatio + ;
				(this.nWorkDaysYear-lnCumulativeRatio)
		endif 	

		this.SaveToCursor_VACGOUV(lnN, lnI, lnCumulativeRatio)
	next 
next 

if this._DEBUG
	select TVac 
	go top in TVac
	lcFile = this.cLogPath + trim(.cPlanId) + ;
			lower(trim(alias()))+ ".csv" 
	copy to (lcFile) type csv
	go top in TVac
endif 

*** Fix some exceptions in the vacation days matrix 	
this.Fix_24DAYS_EXEPTION()

select(lnSelect)

return
endproc

*==========================================================
protected procedure Fix_24DAYS_EXEPTION()
*** Handle exeption for vacation day 24 (APMCP & SFPQ)
*
local lnId, lnDayRatio, lnVacDays, lnWorkDaysYear
*
lnDayRatio = 5.2
lnVacDays = 0.0 
lnWorkDaysYear = 248.6

if !used("TVac")
	return 
endif 

this.WriteLog("Fix_24DAYS_EXEPTION() ... ")

lnId = 24.00
select TVac 
if seek(lnId, "TVac", "tVAC")
	if empty(TVac.TA18219Y)
		replace TA18219Y with lnWorkDaysYear in TVac 
		replace TG23224Y with lnWorkDaysYear in TVac 
		
		lnId = 23.00  
		=seek(lnId, "TVac", "tVAC")
		lnVacDays = TVac.TA18219Y 
		
		lnVacDays = lnVacDays + lnDayRatio
		if (lnVacDays < lnWorkDaysYear) 
			skip in TVac 
			if (TVac.VacDays = 23.50 and !eof())
				replace next 1 TA18219Y with lnVacDays in TVac 
				replace next 1 TG23224Y with lnVacDays in TVac 
			endif 	
		endif 
	endif 
endif 

return 
endproc

*=========================================================
#define SAVE_TO_CURSORS 
*=========================================================
protected procedure SaveToCursor_VACSEB(tnCounter, ;
	tnId, tnValue)
*** Vacation days are calculated based on a given matrix. 
* The matrix is stored into a table "TVac" which is later 
* used to get the number of vacation days/hours for 
* an employee.
*
select TVac
go top in TVac
if !seek(tnId, "TVac", "tVAC")
	append blank in TVac
	replace VacDays 	with tnId in TVac 
endif 

do case 
case tnCounter = 1 
	replace TS5YLess with tnValue in TVac 
case tnCounter = 2
	replace TS5210Y with tnValue in TVac 
case tnCounter = 3 
	replace TS10YMore with tnValue in TVac 
endcase 

return 
endproc 

*==========================================================
protected procedure SaveToCursor_VACAPMCP(tnCounter, ;
	tnId, tnValue)
*** Vacation days are calculated based on a given matrix. 
* The matrix is stored into a table "TVac" which is later 
* used to get the number of vacation days/hours for 
* an employee.
*
select TVac
go top in TVac
if !seek(tnId, "TVac", "tVAC")
	append blank in TVac
	replace VacDays 	with tnId in TVac 
endif 

do case 
case tnCounter = 1 
	replace TA12YLess with tnValue in TVac 
case tnCounter = 2
	replace TA12213Y with tnValue in TVac 
case tnCounter = 3 
	replace TA14215Y with tnValue in TVac 
case tnCounter = 4 
	replace TA16217Y with tnValue in TVac 
case tnCounter = 5 
	replace TA18219Y with tnValue in TVac 
case tnCounter = 6 
	replace TA20224Y with tnValue in TVac 
case tnCounter = 7
	replace TA25YMore with tnValue in TVac 
endcase 

return 
endproc 

*==========================================================
protected procedure SaveToCursor_VACGOUV(tnCounter, ;
	tnId, tnValue)
*** Vacation days are calculated based on a given matrix. 
* The matrix is stored into a table "TVac" which is later 
* used to get the number of vacation days/hours for 
* an employee.
*
select TVac
go top in TVac
if !seek(tnId, "TVac", "tVAC")
	append blank in TVac
	replace VacDays 	with tnId in TVac 
endif 

do case 
case tnCounter = 1 
	replace TG1217Y 	with tnValue in TVac 
case tnCounter = 2
	replace TG17218Y	with tnValue in TVac 
case tnCounter = 3 
	replace TG19220Y	with tnValue in TVac 
case tnCounter = 4 
	replace TG21222Y	with tnValue in TVac 
case tnCounter = 5 
	replace TG23224Y	with tnValue in TVac 
case tnCounter = 6 
	replace TG25YMore	with tnValue in TVac 
endcase 

return 
endproc 

*=========================================================
#define HELPER_METHODS 
*=========================================================
protected procedure GetAPMCPExtraDays()
*** APMCP employees which come from GOV'T 
* get an extra of 5 days of vacation 
*
* Relationship between SFPQ(APMCP) and Government employees
*		APMCP - gets values from row: Écart SFPQ
*
*	Année			1	12	13	14	15	16	17	18	19	20	21	22	23	24	25
*	Ans			1	12	13	14	15	16	17	18	19	20	21	22	23	24	25
*	SFPQ			20	21	21	22	22	23	23	24	24	25	25	25	25	25	30
*	Gouv			20	20	20	20	20	20	21	21	22	22	23	23	24	24	25
*	Écart SFPQ	0	1	1	2	2	3	2	3	2	3	2	2	1	1	5
*	Écart Hrs	0	7	7	14	14	21	14	21	14	21	14	14	7	7	35
*
if empty(.nSenYears) or empty(.nGTSenYears)
	return 
endif 
	
do case 
case .nSenYears >= 12 and .nSenYears <= 13 and .nGTSenYears = 20 
	.nRECAPMCP_DPY = 1
case .nSenYears >= 14 and .nSenYears <= 15 and .nGTSenYears = 20 
	.nRECAPMCP_DPY = 2
case .nSenYears > 15 and .nSenYears <= 16 and .nGTSenYears = 20 
	.nRECAPMCP_DPY = 3
case .nSenYears > 16 and .nSenYears <= 17 and .nGTSenYears = 21
	.nRECAPMCP_DPY = 2
case .nSenYears > 17 and .nSenYears <= 18 and .nGTSenYears = 21
	.nRECAPMCP_DPY = 3
case .nSenYears > 18 and .nSenYears <= 19 and .nGTSenYears = 22
	.nRECAPMCP_DPY = 2
case .nSenYears > 19 and .nSenYears <= 20 and .nGTSenYears = 22
	.nRECAPMCP_DPY = 3
case .nSenYears > 20 and .nSenYears <= 22 and .nGTSenYears = 23
	.nRECAPMCP_DPY = 2
case .nSenYears > 22 and .nSenYears <= 24 and .nGTSenYears = 24
	.nRECAPMCP_DPY = 1
case .nSenYears >= 25 and .nGTSenYears = 25  
	.nRECAPMCP_DPY = 5
endcase 
	
return 

*=========================================================
protected procedure GetFullMonthOfService()
*** Assign 1 day off if more than 1/2 of month 
* has been worked 
*
local lnFullMonthOfService, lnSchedHrsOfService 
local lnActualHrsOfService

store 0 to lnFullMonthOfService, lnSchedHrsOfService
store 0 to lnActualHrsOfService

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

lnActualHrsOfService = .nMTDHoursWorked
lnFullMonthOfService = iif(lnActualHrsOfService >= ;
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
			"cBankUnit = " + transform(.cBankUnit))
	return lnCvtUnit 
endif 

if empty(tnNumber)
	this.WriteLog("ConvertTimeBankUnit() - " + ;
			"tnNumber = " + transform(tnNumber))
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
procedure CreateVacBankObject()
*** Object factory for vacation object 

local loR

loR = createobject("empty")

*** Id's 
addproperty(loR, "cBankId", "")
addproperty(loR, "cPlanId", "")
addproperty(loR, "cUnionId", "")
addproperty(loR, "nPersId", "")

*** Plan Depot OPT 
addproperty(loR, "cDepositOPT", "")

*** Scheduler Id 
addproperty(loR, "cSchedId", "")

*** Seniority fields 
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
local lcAsisField, lcMemo, lcMemoT, loCursor
local lcUNIQID, lcDBF, lcKEY, lcAction
local lcOLDUSER, lcNEWUSER, ltOLDDTM, lcOLDPERS
local lcMHeader, lcNEWPERS

store "" to lcMemo, lcMemoT, lcUNIQID, lcDBF, lcKEY
store "" to lcAction, lcOLDUSER, lcNEWUSER, lcMHeader
store "" to lcNEWPERS
store {} to ltOLDDTM
store null to loCursor

if isnull(toR)
	return 
endif 

with this 

if !._DEBUG
	return 
endif 

=amembers(gaRS, toR, 1)
for lnN = 1 to alen(gaRS, 1)
	lcAsisField = "toR." + alltrim(gaRS[lnN, 1])

	if inlist(upper(alltrim(gaRS[lnN, 1])), "CRULEMATRIX")
		lcMemoT = lcMemoT + ;
				trim(lcAsisField) + "=" + ;
				transform(evaluate(lcAsisField))+CRLF 

		lcMHeader = leftto(transform(evaluate(lcAsisField)),CRLF)
		lcMemo = lcMemo + ;
				trim(lcAsisField) + "=" + lcMHeader + CRLF 
	else  
		lcMemo = lcMemo + ;
				trim(lcAsisField) + "=" + ;
				transform(evaluate(lcAsisField))+CRLF 
	endif 
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

if !this._DEBUG or empty(this.cLogPath) 
	return 
endif 
	
pcLog = trim(pcLog) + chr(13) + chr(10) 
lcLogfile = this.cLogPath + "_" + ;
	leftto(lower(trim(transform(program()))),".")+".log"

lcStartLog=this.cLogPath+this.cLogStatus
if file(lcStartLog)
	=strtofile(transform(datetime()) + " - " + ;
			trim(pcLog), lcLogfile, .t.)
endif 

return

enddefine 

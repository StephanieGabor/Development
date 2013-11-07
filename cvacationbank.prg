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
*							:		This is the procedure which builds a matrix/table based on 
*							:		customer specific parameters. Some of these parameters include: 
*							:		years of seniority, number of hours/days last year, etc. 
*							: 2 - GetVacationDays_XXXXXXX
*							:		The procedure uses the matrix/table "TVac" and returns the number
*							:		of vacation days to which an employee is entitled.  
*							: 3 - SaveToCursor_XXXXXXX
*							:		The procedure saves the vacation entitlement matrix into a FOXPRO table.
*							: 4 - GetYTDVacDays__XXXXXXX
*                    : 		The procedure scans the vacation entitlement table, "TVac", 
*							:		and returns the number of vacations days to which an employee 
*							:		is entitled to date.
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

cLogStatus 	= "startlog.txt" 		&& start logging 
cLogPath = "\TEMP\" 					&& log folder 
_DEBUG = .T.							&& set .T. for debugging 

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

*** set step on 
if !used("qAPLAN") or (tnSenMths = 0)
	this.WriteLog("GetVacBankObject() - qAPLAN not opened!")
	return loRS 
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
	.cDepositOPT = trim(qAPLAN.AP_BANKID)
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

.nSenMths = tnSenMths 
.nSenYears = int(tnSenMths/12)

*** Different date field for seniority 
if !empty(qAPLAN.AP_SENDT)
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

*** Get the counter
.cTCNT = trim(qAPLAN.AP_YTCNT)

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
	lcXls = this.cLogPath + lower(alias()) + ".xls"
	copy to (lcXls) type xls 
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
if .nTotDaysWorked = 0
	this.WriteLog("GetVacationDays_VACSEB() - " + ;
			".nTotDaysWorked=" + transform(.nTotDaysWorked))
	return 
endif 	

*** set step on 
*** Build the vacation matrix for the plan 
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
endif 	

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
if .nTotDaysWorked = 0
	this.WriteLog("GetVacationDays_VACAPMCP() - " + ;
			".nTotDaysWorked=" + transform(.nTotDaysWorked))
	return 
endif 	

*** set step on 
*** Build the vacation matrix for the plan 
if empty(this.cVacBankCursor)
	this.BuildVacationTable_VACAPMCP()
endif 

select TVac 
go top in TVac 

* Get the MAX number of vacation days allowed by union 
lnTotDaysWork = .nTotDaysWorked 
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
	.nAPMCPTotVacDays = TVac.VacDays
	.nAPMCPTotVacHH = this.ConvertTimeBankUnit(.nAPMCPTotVacDays)
else 
	*** Get YearToDate (YTD) entitlement 
	this.GetYTDVacDays_VACAPMCP(lnTotDaysWork)

	.nAPMCPTotVacDays = .nYTDVacDays
	.nAPMCPTotVacHH = this.ConvertTimeBankUnit(.nAPMCPTotVacDays)
endif 	

*** Get Govmnt (YTD) entitlement 
this.GetYTDVacDays_VACGOUV(this.nAPMCP_DAYSWKPERYEAR)
.nGVNTTotVacDays = .nYTDVacDays
.nGVNTTotVacHH = this.ConvertTimeBankUnit(.nGVNTTotVacDays)

*** If did not work a full year do not allow
* any vacation days.
if (.nAPMCPTotVacDays - .nGVNTTotVacDays) > 0 
	.nTotVacDays = (.nAPMCPTotVacDays - .nGVNTTotVacDays)
	.nTotVacHH = this.ConvertTimeBankUnit(.nTotVacDays)
endif 

return
endproc

*==========================================================
protected procedure GetVacationDays_VACGOUV()
*** Gets the current year's number of vaction days, 
* to which an employee is entitled, based on its seniority.
* The program handles Year To Date (YTD) entitlement.
*
local lnTotDaysWork
if .nTotDaysWorked = 0
	this.WriteLog("GetVacationDays_VACGOUV() - " + ;
			".nTotDaysWorked=" + transform(.nTotDaysWorked))
	return 
endif 	

*** Build the vacation matrix for the plan 
if empty(this.cVacBankCursor)
	this.BuildVacationTable_VACGOUV()
endif 

select TVac 
go top in TVac 

* Get the MAX number of vacation days allowed by union 
lnTotDaysWork = .nTotDaysWorked 
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
endif 	

return
endproc

*=========================================================
#define GET_YEAR_TO_DATE_VACATION_DAYS 
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

if empty(tnTotDaysWork)
	this.WriteLog("GetYTDVacDays_VACGOUV() - " + ;
			"tnTotDaysWork=" + transform(tnTotDaysWork))
	return lnEntitleVacDays
endif 	

do case 
case .nGOVTSenYears > 1 and .nGOVTSenYears < 17 
	lcField = "TG1217Y"
case inlist(.nGOVTSenYears, 17, 18) 
	lcField = "TG17218Y"
case inlist(.nGOVTSenYears, 19, 20) 
	lcField = "TG19220Y"
case inlist(.nGOVTSenYears, 21, 22) 
	lcField = "TG21222Y"
case inlist(.nGOVTSenYears, 23, 24) 
	lcField = "TG23224Y"
case .nGOVTSenYears >= 25 
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
	
return 
endproc

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
			lower(trim(alias()))+ sys(2015) + ".xls" 
	copy to (lcFile) type xls 
	go top in TVac
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
			lower(trim(alias()))+ sys(2015) + ".xls" 
	copy to (lcFile) type xls 
	go top in TVac
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
			lower(trim(alias()))+ sys(2015) + ".xls" 
	copy to (lcFile) type xls 
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
protected procedure GetFullMonthOfService()
*** Assign 1 day off if more than 1/2 of month 
* has been worked 
*
local lnFullMonthOfService, lnSchedHrsOfService 
store 0 to lnFullMonthOfService, lnSchedHrsOfService

*** Not worked at all in a given period 
if empty(.nMTDHoursWorked)
	this.WriteLog("GetFullMonthOfService() - " + ;
			"lnFullMonthOfService = " + ;
			transform(lnFullMonthOfService))

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
addproperty(loR, "nGOVTSenMths", 0)
addproperty(loR, "nGOVTSenYears", 0)

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

*** Year To Date (YTD)
*** Employee entitlement 
addproperty(loR, "nYTDVacDays", 0)
addproperty(loR, "nYTDVacHH", 0)
addproperty(loR, "nTotVacDays", 0)
addproperty(loR, "nTotVacHH", 0)

*** Custom fields by Union 
*** APMCP & GOVT  
addproperty(loR, "nAPMCPTotVacDays", 0)
addproperty(loR, "nAPMCPTotVacHH", 0)
addproperty(loR, "nGVNTTotVacDays", 0)
addproperty(loR, "nGVNTTotVacHH", 0)

*** Timesheet posting parameters  
addproperty(loR, "IsTSRealTime", .f.)
addproperty(loR, "dTSStartDt", {})
addproperty(loR, "dTSEndDt", {})
addproperty(loR, "cTSTCNT", "")
addproperty(loR, "nTSWorkedDays", 0)
addproperty(loR, "nTSWorkedHH", 0)

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

if !this._DEBUG or empty(this.cLogPath) 
	return 
endif 

this.WriteLog(" ... " + CRLF)
=amembers(gaRS, toR, 1)
for lnN = 1 to alen(gaRS, 1)
	lcAsisField = "toR." + trim(gaRS[lnN, 1])
	this.WriteLog("SpoolObjectProperties() :: " + ;
		trim(lcAsisField) + " = " + ;
		transform(evaluate(lcAsisField)))
next 

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

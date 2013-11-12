*=========================================================
*  Program...........: bizTIMETMP1_Custom
*  Author............: Stefan Gabor 
*  Project...........: SFPQ 
*  Created...........: October 10, 2013
*  Code page.........: 1252 (WINDOWS)
*  Copyright.........: (c) Carver Technologies 2013
*  Description.......: New rounding scheduler schema 
*                    : 
*                    : The custom timesheet bizTimetmp
*                    : should inherit from this class
*                    : 
*  Classes...........: bizTIMETMP1_Custom as bizTIMETMP1
*                    : 
*  Modification      : Stefan - 2013/04/20 --- SFPQ  
*							: The rounding to schedule requirements for 
*							: employees with a schedule.
*
*	(1)->08:00<-(2)	(3)->12:00<-(4)  	(5)->12:30<-(6)	(7)->15:00<-(8) 
*
*	OPTIONS 				: SHIFT (08:00 - 12:00	12:30 - 15:00)
*							: (1) Start Shift Round Up 
*							: (2) Start Shift Round Down 
*							: (3) Start Lunch Round Up
*                    : (4) Start Lunch Round Down 
*							: (5) End Lunch Round Up
*                    : (6) End Lunch Round Down 
*							: (7) End Shift Round Up
*                    : (8) End Shift Round Down 
*							:
*                    : BLUES worker can be easily extented.
*                    : The field loR.UsedHM contains 
*							: the adjusted values to be further used.
*  						:
*							: Major changes (SFPQ) - 2013/10/15
*							: Entitlement of sick banks during  
*							: the timesheet posting. They use 
*							: some specific business rules where 
*							: the normal entitlement can not be 
*							: linked to the PAYGRP. Moreover, 
*							: two sick banks need to run the 
*							: entitlement in parallel.
*
*#########################################################
 define class bizTIMETMP1_Custom ;
 					as bizTIMETMP1 of bizTIMETMP1.PRG
*#########################################################
*** Properties:
cPrimeOPT = ""
cJobCatId = ""									&& Filter for Job Category 

nRndUpShiftStartMM = 0		 				&& Before the shift starts round up 
nRndDownShiftStartMM = 0	 				&& After the shift starts round down  
nRndUpLunchStartMM = 0 						&& Before lunch starts round up 
nRndDownLunchStartMM = 0 					&& After lunch started round down
nRndUpLunchEndMM = 0 						&& Before lunch ends round up   
nRndDownLunchEndMM = 0	 					&& After lunch ends round down 
nRndUpShiftEndMM = 0			 				&& Before the shift ends round up 
nRndDownShiftEndMM = 0	 					&& After the shift ends round down  

cBANK_REALTIME_ENTITLEMENT="1,2,5,6"	&& Banks real time entitlement  

cLogStatus = "startlog.txt" 				&& triggers for start logging 
cLogPath = "\TEMP\" 							&& log folder 
_DEBUG = .T.									&& set .T. for debugging 

nRoundInOutTo = 0                   	&& default rounding  
*=========================================================
#define SCHEDULE_ROUNDING_SCHEMA_AnalyseInOut
*** Procedures related to AnalyseInOut which is use 
* as rounding schema from punch clock & timesheet 
*=========================================================
procedure IsRequiredRoundingToSchedule()
*** Filter all employees which get roudup  
* to their schedules - S.G. 
local llReturn 

if !used("qJobhist")
	return llReturn 
endif 

*** If the record is modified manually  
* do not apply any rounding 
if used("vTimetmp") ;
and ttod(vTimetmp.TT_MODDT) <> {^1900/01/01} ;
and (vTimetmp.TT_ENTERDT <> vTimetmp.TT_MODDT)

	*** debug 
	this.WriteLog("IsRequiredRoundingToSchedule() - " + ;
			"TT_MODDT = " + transform(vTimetmp.TT_MODDT))
	this.WriteLog("IsRequiredRoundingToSchedule() - " + ;
			"TT_ENTERDT = " + transform(vTimetmp.TT_ENTERDT))

	return llReturn 
endif 
	
*** Filter by JOBCAT 
if inset(qJobhist.H_JOBCAT, this.cJobCatId)
	llReturn = .t.
endif 

return llReturn

*=========================================================
procedure AnalyseInOut_AdjustStartShift(tlShiftStartUp, ;
		tlShiftStartDown)
*** Adjust the start of the shift UP & DOWN 
local lnStartOfShiftHM, lnSchedStartOfShiftHM
local lcStartOfShiftHM

store "" to lcStartOfShiftHM
store 0 to lnStartOfShiftHM, lnSchedStartOfShiftHM

if !tlShiftStartUp and !tlShiftStartDown
	return 
endif 	

*** Adjust the start of shift time UP to the schedule 
if tlShiftStartUp
	*** if on schedule .UsedHM is empty 
	lcStartOfShiftHM = left(.UsedHM,4)
	lnStartOfShiftHM = HMtoM(alltrim(lcStartOfShiftHM))
	lnSchedStartOfShiftHM = HMtoM(alltrim(loSched.StartHM))

	*** Round the shift start UP to the shedule 
	if lnStartOfShiftHM < lnSchedStartOfShiftHM ;
	and castx(lnSchedStartOfShiftHM-lnStartOfShiftHM, "N")<=this.nRndUpShiftStartMM
		.UsedHM = stuff(.UsedHM, 1, 4, loSched.StartHM)
	endif 

	*** debug 
	this.WriteLog("AnalyseInOut_AdjustStartShift() - " + ;
			"lnIntervals = " + transform(lnIntervals))
	this.WriteLog("AnalyseInOut_AdjustStartShift() - " + ;
			"lcStartOfShiftHM = " + transform(lcStartOfShiftHM))
	this.WriteLog("AnalyseInOut_AdjustStartShift() - " + ;
			"lnStartOfShiftHM = " + transform(lnStartOfShiftHM))
	this.WriteLog("AnalyseInOut_AdjustStartShift() - " + ;
			"lnSchedStartOfShiftHM = " + transform(lnSchedStartOfShiftHM))
endif 


*** Adjust the start of shift time DOWN to the schedule 
if tlShiftStartDown 
	*** if on schedule .UsedHM is empty 
	lcStartOfShiftHM = left(.UsedHM,4)
	lnStartOfShiftHM = HMtoM(alltrim(lcStartOfShiftHM))
	lnSchedStartOfShiftHM = HMtoM(alltrim(loSched.StartHM))

	*** Round the shift start DOWN to the shedule 
	if lnStartOfShiftHM > lnSchedStartOfShiftHM ;
	and castx(lnStartOfShiftHM-lnSchedStartOfShiftHM, "N")<=this.nRndDownShiftStartMM
		.UsedHM = stuff(.UsedHM, 1, 4, loSched.StartHM)
	endif 

	*** debug 
	this.WriteLog("AnalyseInOut_AdjustStartShift() - " + ;
			"lnIntervals = " + transform(lnIntervals))
	this.WriteLog("AnalyseInOut_AdjustStartShift() - " + ;
			"lcStartOfShiftHM = " + transform(lcStartOfShiftHM))
	this.WriteLog("AnalyseInOut_AdjustStartShift() - " + ;
			"lnStartOfShiftHM = " + transform(lnStartOfShiftHM))
	this.WriteLog("AnalyseInOut_AdjustStartShift() - " + ;
			"lnSchedStartOfShiftHM = " + transform(lnSchedStartOfShiftHM))
endif 

return 


*=========================================================
procedure AnalyseInOut_AdjustEndShift(tlShiftEndUp, ;
		tlShiftEndDown)
*** Adjust the end of the shift UP & DOWN 
local lnEndOfShiftHM, lnSchedEndOfShiftHM
local lcEndOfShiftHM

store "" to lcEndOfShiftHM
store 0 to lnEndOfShiftHM, lnSchedEndOfShiftHM

if !tlShiftEndUp and !tlShiftEndDown
	return 
endif 
	
*** Adjust the end of shift time UP to the schedule 
if tlShiftEndUp 
	lcEndOfShiftHM = right(mline(.UsedHM,lnIntervals),4)
	lnEndOfShiftHM = HMtoM(alltrim(lcEndOfShiftHM))
	lnSchedEndOfShiftHM = HMtoM(alltrim(loSched.EndHM))

	*** Round the shift end UP to the shedule 
	if lnEndOfShiftHM < lnSchedEndOfShiftHM ;
	and castx(lnSchedEndOfShiftHM-lnEndOfShiftHM, "N")<=this.nRndUpShiftEndMM
		.UsedHM = substr(.UsedHM,1,len(.UsedHM)-4)+loSched.EndHM
	endif 

	*** debug 
	this.WriteLog("AnalyseInOut_AdjustEndShift() - " + ;
			"lnIntervals = " + transform(lnIntervals))
	this.WriteLog("AnalyseInOut_AdjustEndShift() - " + ;
			"lcEndOfShiftHM = " + transform(lcEndOfShiftHM))
	this.WriteLog("AnalyseInOut_AdjustEndShift() - " + ;
			"lnEndOfShiftHM = " + transform(lnEndOfShiftHM))
	this.WriteLog("AnalyseInOut_AdjustEndShift() - " + ;
			"lnSchedEndOfShiftHM = " + transform(lnSchedEndOfShiftHM))
endif 


*** Adjust the end of shift time DOWN to the schedule 
if tlShiftEndDown  
	lcEndOfShiftHM = right(mline(.UsedHM,lnIntervals),4)
	lnEndOfShiftHM = HMtoM(alltrim(lcEndOfShiftHM))
	lnSchedEndOfShiftHM = HMtoM(alltrim(loSched.EndHM))

	*** Round the shift end DOWN to the shedule 
	if lnEndOfShiftHM > lnSchedEndOfShiftHM ;
	and castx(lnEndOfShiftHM-lnSchedEndOfShiftHM, "N")<=this.nRndDownShiftEndMM
		.UsedHM = substr(.UsedHM,1,len(.UsedHM)-4)+loSched.EndHM
	endif 

	*** debug 
	this.WriteLog("AnalyseInOut_AdjustEndShift() - " + ;
			"lnIntervals = " + transform(lnIntervals))
	this.WriteLog("AnalyseInOut_AdjustEndShift() - " + ;
			"lcEndOfShiftHM = " + transform(lcEndOfShiftHM))
	this.WriteLog("AnalyseInOut_AdjustEndShift() - " + ;
			"lnEndOfShiftHM = " + transform(lnEndOfShiftHM))
	this.WriteLog("AnalyseInOut_AdjustEndShift() - " + ;
			"lnSchedEndOfShiftHM = " + transform(lnSchedEndOfShiftHM))
endif 

return 

*=========================================================
procedure AnalyseInOut_AdjustStartLunch(tlLunchStartUp, ;
			tlLunchStartDown)
*** Adjust the start of the lunch UP & DOWN 
local lnStartExHM, lnSchedStartExHM
local lcStartExHM

store "" to lcStartExHM
store 0 to lnStartExHM, lnSchedStartExHM

if !tlLunchStartUp and !tlLunchStartDown
	return 
endif 
	
*** Adjust the break start time UP to the schedule 
if tlLunchStartUp	
	lcStartExHM = substr(.UsedHM, 6, 4) 
	lnStartExHM = HMtoM(alltrim(lcStartExHM))
	lnSchedStartExHM = HMtoM(alltrim(loSched.StartEXHM))

	*** Round the start lunch time UP to the shedule 
	if lnStartExHM < lnSchedStartExHM ;
	and castx(lnSchedStartExHM-lnStartExHM, "N")<=this.nRndUpLunchStartMM
		.UsedHM = stuff(.UsedHM, 6, 4, loSched.StartExHM)
	endif 

	*** debug 
	this.WriteLog("AnalyseInOut_AdjustStartLunch() - " + ;
			"lnIntervals = " + transform(lnIntervals))
	this.WriteLog("AnalyseInOut_AdjustStartLunch() - " + ;
			"lcStartExHM = " + transform(lcStartExHM))
	this.WriteLog("AnalyseInOut_AdjustStartLunch() - " + ;
			"lnStartExHM = " + transform(lnStartExHM))
	this.WriteLog("AnalyseInOut_AdjustStartLunch() - " + ;
			"lnSchedStartExHM = " + transform(lnSchedStartExHM))
endif 


*** Adjust the break start time DOWN to the schedule 
if tlLunchStartDown
	lcStartExHM = substr(.UsedHM, 6, 4) 
	lnStartExHM = HMtoM(alltrim(lcStartExHM))
	lnSchedStartExHM = HMtoM(alltrim(loSched.StartEXHM))

	*** Round the start lunch time UP to the shedule 
	if lnStartExHM > lnSchedStartExHM ;
	and castx(lnStartExHM-lnSchedStartExHM, "N") <= this.nRndDownLunchStartMM
		.UsedHM = stuff(.UsedHM, 6, 4, loSched.StartExHM)
	endif 

	*** debug 
	this.WriteLog("AnalyseInOut_AdjustStartLunch() - " + ;
			"lnIntervals = " + transform(lnIntervals))
	this.WriteLog("AnalyseInOut_AdjustStartLunch() - " + ;
			"lcStartExHM = " + transform(lcStartExHM))
	this.WriteLog("AnalyseInOut_AdjustStartLunch() - " + ;
			"lnStartExHM = " + transform(lnStartExHM))
	this.WriteLog("AnalyseInOut_AdjustStartLunch() - " + ;
			"lnSchedStartExHM = " + transform(lnSchedStartExHM))
endif 

return 


*=========================================================
procedure AnalyseInOut_AdjustEndLunch(tlLunchEndUp, ;
		tlLunchEndDown )
*** Adjust the end of the lunch UP & DOWN 
local lnStartExHM, lnSchedStartExHM
local lcStartExHM

store "" to lcStartExHM
store 0 to lnStartExHM, lnSchedStartExHM

if !tlLunchEndUp and !tlLunchEndDown 
	return 
endif 
	
*** Adjust the lunch end time UP to the schedule 
if tlLunchEndUp
	lcEndExHM = left(mline(.UsedHM,2),4)
	lnEndExHM = HMtoM(alltrim(lcEndExHM))
	lnSchedEndExHM = HMtoM(alltrim(loSched.EndEXHM))

	*** Round lunch end time UP to the shedule 
	if lnEndExHM < lnSchedEndExHM ;
	and castx(lnSchedEndExHM-lnEndExHM, "N") <= this.nRndUpLunchEndMM
		.UsedHM = stuff(.UsedHM, 12, 4, loSched.EndExHM)
	endif 

	*** debug 
	this.WriteLog("AnalyseInOut_AdjustEndLunch() - " + ;
			"lnIntervals = " + transform(lnIntervals))
	this.WriteLog("AnalyseInOut_AdjustEndLunch() - " + ;
			"lcEndExHM = " + transform(lcEndExHM))
	this.WriteLog("AnalyseInOut_AdjustEndLunch() - " + ;
			"lnEndExHM = " + transform(lnEndExHM))
	this.WriteLog("AnalyseInOut_AdjustEndLunch() - " + ;
			"lnSchedEndExHM = " + transform(lnSchedEndExHM))
endif 


*** Adjust the lunch end time DOWN to the schedule 
if tlLunchEndDown	
	lcEndExHM = left(mline(.UsedHM,2),4)
	lnEndExHM = HMtoM(alltrim(lcEndExHM))
	lnSchedEndExHM = HMtoM(alltrim(loSched.EndEXHM))

	*** Round the time IN from lunch to shedule 
	if lnEndExHM > lnSchedEndExHM ;
	and castx(lnEndExHM-lnSchedEndExHM, "N") <= this.nRndDownLunchEndMM
		.UsedHM = stuff(.UsedHM, 12, 4, loSched.EndExHM)
	endif 

	*** debug 
	this.WriteLog("AnalyseInOut_AdjustEndLunch() - " + ;
			"lnIntervals = " + transform(lnIntervals))
	this.WriteLog("AnalyseInOut_AdjustEndLunch() - " + ;
			"lcEndExHM = " + transform(lcEndExHM))
	this.WriteLog("AnalyseInOut_AdjustEndLunch() - " + ;
			"lnEndExHM = " + transform(lnEndExHM))
	this.WriteLog("AnalyseInOut_AdjustEndLunch() - " + ;
			"lnSchedEndExHM = " + transform(lnSchedEndExHM))
endif 

return 

*=========================================================
procedure AnalyseInOut_GetRoundingSchema()
*** There is a difference between bizPlanDt and bizSched 
* the way the object loSched is built. 
* The procedure adds "Rules" property the the loSched 
* object and assigns the values from sched.SC_RULES to 
* this new property.
*
local lnSelect, loBizSched, loCsrSched, lcSchedId 

store "" to lcSchedId
store null to loBizSched, loCsrSched 

lnSelect = select()

addproperty(loSched, "Rules", "")
loBizSched = this.oBizMgr.GetBiz("SCHED")

if isnull(loBizSched)
	select(lnSelect)
	return
endif 

lcSchedId = trim(loSched.BaseSchedId)
loCsrSched = loBizSched.GetById( ;
				"/VIEW=vSched", "*", lcSchedId)

if !isnull(loCsrSched)
	loSched.Rules = trim(vSched.SC_RULES)
endif 
	
use in select("vSched")
store null to loBizSched, loCsrSched 

select(lnSelect)
return 

*=========================================================
procedure AnalyseInOut_Init()
*** Take all properties from the employe's scheduler 
* [SC_RULES] field and save their values to 
* class' properties 
*
local array laRoundSchema(20) 
local lnRSchemaCnt, lcProperty, lnPropertyValue, lcCommand

store 0 to lnRSchemaCnt, lnPropertyValue
store "" to lcProperty, lcCommand

with this 

if type("loSched") != "O" ;
or isnull(loSched)
	return 
endif 

if type("loSched.Rules") = "U" 
	.AnalyseInOut_GetRoundingSchema()
endif 

if empty(loSched.Rules)
	return 
endif 

lnRSchemaCnt = parse(loSched.Rules, @laRoundSchema, CRLF, "|", "/ALL")
if lnRSchemaCnt < 1 
	return 
endif 
	
for lnI = 1 to alen(laRoundSchema,1)
	if !empty(laRoundSchema[lnI])
		lcProperty = "this." + trim(leftto(laRoundSchema[lnI],"="))

		if type(lcProperty) != "U" 
			lnPropertyValue = val(rightFrom(laRoundSchema[lnI], "="))
			lcCommand = lcProperty + "=" + transform(lnPropertyValue)

			*** debug 
			.WriteLog("AnalyseInOut_Init() - " + ;
					"lcCommand = " + transform(lcCommand))

			*/ Apply the settings to class properties 
			&lcCommand
		endif 
	endif 
next 

*** debug 
this.WriteLog("AnalyseInOut_Init() - " + ;
		"nRndUpShiftStartMM = "+transform(.nRndUpShiftStartMM)+CRLF+;
		"nRndDownShiftStartMM = "+transform(.nRndDownShiftStartMM)+CRLF+;
		"nRndUpLunchStartMM = "+transform(.nRndUpLunchStartMM)+CRLF+;
		"nRndDownLunchStartMM = "+transform(.nRndDownLunchStartMM)+CRLF+;
		"nRndUpLunchEndMM = "+transform(.nRndUpLunchEndMM)+CRLF+;
		"nRndDownLunchEndMM = "+transform(.nRndDownLunchEndMM)+CRLF+;
		"nRndUpShiftEndMM = "+transform(.nRndUpShiftEndMM)+CRLF+;
		"nRndDownShiftEndMM = "+transform(.nRndDownShiftEndMM))

endwith 

return 

*=========================================================
procedure AnalyseInOut_AdjustTimeToScheduleHM()
*** Adjust the end of shift time and the break time, 
* start and end, according to employe's schedule.

* I do not want to make these private due to
* risk of tight coupling. - S.G. 
local llShiftStartUp, llShiftStartDown 
local llShiftEndUp, llShiftEndDown
local llLunchStartUp, llLunchStartDown  
local llLunchEndUp, llLunchEndDown 
local lnLunchBreak 

store 0 to lnLunchBreak 

*** Filter the employees 
if !this.IsRequiredRoundingToSchedule()
	return 
endif 	

*** Load class' properties from the table 
this.AnalyseInOut_Init()

*** start of shift
llShiftStartUp = (this.nRndUpShiftStartMM > 0)
llShiftStartDown = (this.nRndDownShiftStartMM > 0)

*** end of shift 
llShiftEndUp = (this.nRndUpShiftEndMM > 0)
llShiftEndDown = (this.nRndDownShiftEndMM> 0)

*** start of lunch 
llLunchStartUp = (this.nRndUpLunchStartMM > 0)
llLunchStartDown = (this.nRndDownLunchStartMM > 0)

*** end of lunch 
llLunchEndUp = (this.nRndUpLunchEndMM > 0)
llLunchEndDown = (this.nRndDownLunchEndMM > 0)

*** Handle shift (start and end) rounding 
this.AnalyseInOut_AdjustStartShift(llShiftStartUp,llShiftStartDown)
this.AnalyseInOut_AdjustEndShift(llShiftEndUp,llShiftEndDown)

*** Start adjusting the lunch time 
lnLunchBreak = occurs(CRLF, .UsedHM)
if (lnLunchBreak = 0 or lnLunchBreak > 1)
	return 
endif 
	
*** Handle lunch (start and end) rounding 
this.AnalyseInOut_AdjustStartLunch(llLunchStartUp,llLunchStartDown)
this.AnalyseInOut_AdjustEndLunch(llLunchEndUp,llLunchEndDown)

return 


*=========================================================
#define POSTING_PROCEDURES
*** Procedures related to saving the expenses
* from timesheet 
*=========================================================
procedure Postbatch_FinishPerson(tnPersId)
*** Use this for posting premiums, etc.
*
local llOk
llOk = .t.

if vartype(tnPersId) != "N" ;
or empty(tnPersId)
	return 
endif 

with this 

llOk = iif(llOk, .Postbatch_Primes(tnPersId), .f.)
llOk = iif(llOk, .Postbatch_Expenses(tnPersId), .f.)
llOk = iif(llOk, .Postbatch_Deposits(tnPersId), .f.)

endwith 

return llOk 

*=========================================================
procedure Postbatch_Primes(tnPersId)
local lnSelect, lcDestination, lnPrimeAMT, lnPrimeRate 

store "" to lcDestination
store 0 to lnPrimeAMT, lnPrimeRate 

lnSelect = Select()

scan for TT_PERSID = tnPersId and !eof()
	if nvl(TT_PRIME,0) > 0 

		scatter memo memvar 

		lcDestination = tbleval("PRIME", this.cPrimeOPT, "TBLC1")
		lnPrimeAMT = val(tbleval("PRIME", this.cPrimeOPT, "TBLC2"))
		lnPrimeRate = val(tbleval("PRIME", this.cPrimeOPT, "TBLC2")) 


		m.T_PERSID = m.TT_PERSID
		m.T_EFFDT  = m.TT_EFFDT
		m.T_UNITCD = "H"
		m.T_UNIQID = uniqid()
		m.T_ENTERBY = gcUser
		m.T_USER = gcUser
		m.T_ENTERDT = datetime()
		m.T_DEST = lcDestination
		m.T_AMT = lnPrimeAMT * nvl(TT_PRIME,0)
		m.T_UNITRATE = lnPrimeRate
		m.T_SOURCE = m.TT_SOURCE 
		m.T_PROJECT = m.TT_PROJECT 
		m.T_PAYNO = qJobhist488.H_PAYGRP
		m.T_ENTITY = evaluate("qJobhist488.H_" + c_tdentity)
		m.T_OPT = this.cPrimeOPT
		m.T_HOURS = nvl(TT_PRIME,0)
		m.T_TYPE = "P"

		Insert Into vTimedt488 From Memvar
	else
		loop 
	endif 
endscan 

select(lnSelect)
return 

*=========================================================
procedure Postbatch_Expenses(tnPersId)
local lnSelect, lnUnitRate, lnAMT

store 0 to lnUnitRate, lnAMT

if type("TT__EUNIT") = "U" ;
or type("TT__EURATE") = "U"
	return 
endif 
	
lnSelect = Select()

scan for TT_PERSID = tnPersId and !eof()
	if !empty(TT__EUNIT) and !empty(TT__EURATE)

		scatter memo memvar 

		lnUnitRate = m.TT__EURATE  
		lnAMT = m.TT__EUNIT * lnUnitRate 

		m.T_PERSID = m.TT_PERSID
		m.T_EFFDT  = m.TT_EFFDT
		m.T_UNITCD = "*"
		m.T_UNIQID = uniqid()
		m.T_ENTERBY = gcUser
		m.T_USER = gcUser
		m.T_ENTERDT = datetime()
		m.T_DEST = "*"
		m.T_AMT = lnAMT 
		m.T_UNITRATE = lnUnitRate 
		m.T_SOURCE = m.TT_SOURCE 
		m.T_PROJECT = m.TT_PROJECT 
		m.T_PAYNO = qJobhist488.H_PAYGRP
		m.T_ENTITY = evaluate("qJobhist488.H_" + c_tdentity)
		m.T_OPT = m.TT_OPT
		m.T_HOURS = 0.0
		m.T_TYPE = "P"
		m.T__EUNIT = m.TT__EUNIT
		m.T__EURATE = m.TT__EURATE 

		Insert Into vTimedt488 From Memvar
	else
		loop 
	endif 
endscan 

select(lnSelect)
return 

*=========================================================
procedure Postbatch_Deposits(tnPersId)
* Allow the option to accumulate simultaneously 
* into 1..N bank plans. 
*
local lnSelect, llOk 
local lcDEST, lcOPT, lcTYPE, lcTSOURCE, lcBankSet
local lnUnitRate, lnAMT, lnSenMths, lnSickBanksCnt
local array laSickBank(1)
local loSickBank, loSickParm
llOk = .t. 

store "" to lcDEST, lcOPT, lcTYPE, lcBankSet
store 0 to lnAMT, lnUnitRate, lnSickBanksCnt, lnSenMths
store null to loSickBank, loSickParm 

with this 

if !used("qAPLAN")
	this.WriteLog("Postbatch_Deposits() - " + ;
			"The table qAPLAN is not opened!")
	return 
endif 	

lnSelect = Select()

*** Get seniority months 
if !empty(qJobhist.E_LASTHIRE)
	lnSenMths = months(qJobhist.E_LASTHIRE, date()) 
endif 	

*** Get all the plans from Jobhist 
this.GetEmployeeSICKBanks(@lcBankSet)
if empty(lcBankSet)
	this.WriteLog("Postbatch_Deposits() - " + ;
			"lcBankSet = "+transform(lcBankSet))
	return 
endif 

*** Put sick banks set into the array  
lnSickBanksCnt=parse(lcBankSet, @laSickBank, CRLF, " ", "/ALL")
if empty(lnSickBanksCnt)
	this.WriteLog("Postbatch_Deposits() - " + ;
		"lnSickBanksCnt = "+transform(lnSickBanksCnt))
	return 
endif 

*** Create class instance 	
loSickBank = newobject("CSickBank", "CSickBank.prg")
if isnull(loSickBank) or type("loSickBank")!="O" 
	this.WriteLog("Postbatch_Deposits() - " + ;
		"Unable to create an instance of CSickBank!")
	loSickBank = null 
	return
endif 

*** set step on 
for lnJ = 1 to alen(laSickBank,1)

	*** Position record into qAPLAN 
	select qAPLAN 
	locate for qAPLAN.AP_PLANID = trim(laSickBank[lnJ]) and !eof()
	if !found()
		this.WriteLog("Postbatch_Deposits() - " + ;
				"Unable to find plan: " + transform(laSickBank[lnJ]))
		loop
	endif 	

	*** Call into sick bank object 
	loSickParm = loSickBank.GetSickBankObject(lnSenMths)
	if isnull(loSickParm) or type("loSickParm")!="O" 
		this.WriteLog("Postbatch_Deposits() - " + ;
			"Unable to get the object from the call into CSickBank!")
		exit 
	endif 	

	*** Set custom properties 
	if !this.SetSickObjectProperty(@loSickParm)
		loSickParm.cERROR = "ERROR: Setting up object custom property!"
		exit 
	endif 		
	
	if empty(loSickParm.cDepositOPT)
		loSickParm.cERROR = "Unable to get the DEPOOPT from APLAN!"
		exit 
	endif 

	*** do entitlement 
	loSickBank.PostEntitlement(@loSickParm)

	Select(lnSelect)
	if !empty(nvl(loSickParm.nTotSickHH,0)) ;
	or !empty(nvl(loSickParm.nTotSickDays,0))

		scatter memo memvar 

		lcOPT = trim(qAPLAN.AP_DEPOOPT)
		lcDEST= tbleval("MISCBANK", lcOPT, "TBLC1")
		lcTYPE = "-"
		lcTSOURCE = "F" 						&& TIMESHEET 

		m.T_PERSID = loSickParm.nPersId
		m.T_EFFDT  = nvl(loSickParm.dTSEndDt, datetime())
		m.T_UNITCD = nvl(loSickParm.cBankUnit,"")
		m.T_UNIQID = uniqid()
		m.T_ENTERBY = gcUser
		m.T_USER = gcUser
		m.T_ENTERDT = datetime()
		m.T_DEST = lcDEST 
		m.T_AMT = lnAMT 
		m.T_UNITRATE = lnUnitRate
		m.T_SOURCE = lcTSOURCE 
		m.T_PROJECT = ""
		m.T_PAYNO = qJobhist488.H_PAYGRP
		m.T_ENTITY = evaluate("qJobhist488.H_" + c_tdentity)
		m.T_OPT = lcOPT
		m.T_TYPE = lcTYPE

		if "H"$nvl(loSickParm.cBankUnit,"")
			m.T_HOURS = nvl(loSickParm.nEntitleHH,0)
		else 
			m.T_HOURS = nvl(loSickParm.nEntitleDays,0)
		endif 

		Insert Into vTimedt488 From Memvar
	endif 	
		
	store null to loSickParm
next 

.WriteLog("Postbatch_Deposits() - Done.")

endwith 

store null to loSickBank, loSickParm

select(lnSelect)
return llOk 

*==========================================================
protected procedure SetSickObjectProperty(toRS)
*** Populate the objects' constants 
*
local lnSelect, lcEXPR, llOk 
store "" to lcEXPR
llOk = .t. 

if type("toRS")!="O" or isnull(toRS)
	toRS.cERROR = "SetSickObjectProperty() - return 1"
	return .f. 
endif 

if !used("vTimetmp")
	toRS.cERROR = "SetSickObjectProperty() - return 2"
	return .f. 
endif 

if !used("qBatch")
	toRS.cERROR = "SetSickObjectProperty() - return 3"
	return .f. 
endif 

lnSelect = select()

*** Get min, max dates from timesheet 
select TT_PERSID, ;
	min(TT_EFFDT) As MinEffDt, max(TT_EFFDT) As MaxEffDt ;
from vTimetmp ;
where TT_PERSID = qBatch.H_PERSID ;
group by TT_PERSID ;
into Cursor qTSDates 

if reccount("qTSDates")=0 
	toRS.cERROR = "The cursor qTSDates is empty!"
	return .f. 
endif 

*** Real time posting of timesheet 
toRS.IsTSRealTime = .t. 

*** Pay period interval 
toRS.dTSStartDt = MinEffDt 
toRS.dTSEndDt = MaxEffDt

*** Get the right PERSID
toRS.nPERSID = qTSDates.TT_PERSID

*** Get the right counter 
toRS.cTCNT = toRS.cPlanId

*** Strip counter to use timetmp  
lcEXPR = TCNTVAL("EXPR", toRS.cTCNT)
if !empty(lcEXPR)
	lcEXPR = strtran(lcEXPR, "and T_UNITCD='H'","")
	lcEXPR = strtran(lcEXPR, "T_","TT_")
else
	toRS.cERROR = "The counter expression is empty."
endif
toRS.cTSTCNT = lcEXPR

use in select("qTSDates")

select(lnSelect)
return llOk 

*=========================================================
procedure GetEmployeeSICKBanks(tcBankSet)
* The procedure returns all the SICK banks that are 
* configured on the JOBHIST table based on TIMEBANK 
* TBLNAME(F) in the tbl.
*
local lnI, lcPlanId, lcBankId, lcBkTypeName, lcBankSet 
local lnSelect 

store "" to lcPlanId, lcBankId, lcBkTypeName, lcBankSet

if type("tcBankSet") != "C"
	return ""
endif 	

lnSelect = select()

for lnI = 1 to 9
	lcPlanId = trim(evaluate("qJobhist.H_APLAN"+lstr(lnI)))
	if !empty(lcPlanId)

		select qAPLAN 
		locate for qAPLAN.AP_PLANID = lcPlanId and !eof()
		if found()			
			lcBankId = trim(qAPLAN.AP_BANKID)
		endif 

		if !empty(lcBankId)
			if lcBankId$this.cBANK_REALTIME_ENTITLEMENT
				tcBankSet = tcBankSet + lcPlanId + " "
			endif
		endif 
	endif
	
	select qJobhist
next

if !empty(tcBankSet)
	tcBankSet = trim(tcBankSet)
endif 

*** debug 
this.WriteLog("GetEmployeeSICKBanks() - " + ;
		"tcBankSet = "+transform(tcBankSet))

select(lnSelect)
return tcBankSet

*=========================================================
procedure SpoolObjectProperties(toR As Object)
*** S.G. - Logs all object properies 
local lcAsisField

if isnull(toR)
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

*#########################################################
enddefine

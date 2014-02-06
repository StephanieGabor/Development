*========================================================
*  Program...........: bhvTimeTmp1_Custom
*  Author............: Mike Gagnon
*  Project...........: Ski Bromont
*  Created...........: May 1, 2011
*  Code page.........: 1252 (WINDOWS)
*  Copyright.........: (c) Carver Technologies 2011
*  Description.......: Behavior for TimeTmp1 (Timesheet) form
*                    :
*                    : *** CUSTOM VERSION FOR SKI BROMONT ***
*                    :
*  Technical Notes...: NOTE: This is a subclass of
*                    :       bhvTIMETMP1.
*                    :
*                    :
*  Major modifs......: October - 2013 
*                    : Transfer some of the common functionalities  
*                    : of the timeshhet and punch clock to the 
*                    : timesheet Biz object. 
*                    : Use the Analyser object to calculate all  
*                    : timesheet details.
*                    : 
*
*
*
*
*#########################################################
define class bhvTimeTmp1_custom ;
		as bhvTimeTmp1 of bhvTimeTmp1.prg
*#########################################################

*** Properties 
csrBankHrs = "qSchedList"
nSchedHours= 0.0 
cSchedHours= "" 
cOptType = "" 
oBankHours = null
cSchedId = ""
cKilometerYTD = ""

*** Calendar 
lUseSidebar = .t.

*** timesheet posting 
lAllowPostBeforeAllSigned = .t.
lPostOnlyValidated = .t.

*-- How many periods in the future can employee go to?
nFuturePeriods = Null		&& Null = no restriction

*-- Time BANKIDs can deposit surplus in
cDepositSurplusIntoBankids = "6"
cBank6DepositOpt = ""
cBank9DepositOpt = ""

lShowPlanNames = .T. 

*-- Time BANKIDs can withdraw deficit from
cWithdrawDeficitFromBankids = "1359"		&& Bankids.  E.g. "13"

*** OPTs to use for bank withdrawal transactions.
* Create one per bankid, replacing the Z with the Bankid
cBank1WithdrawalOpt = ""	
cBank2WithdrawalOpt = ""
cBank3WithdrawalOpt = ""
cBank5WithdrawalOpt = ""
cBank6WithdrawalOpt = ""
cBank9WithdrawalOpt = ""

*** Option to reverse 
cRegOptToReverse = "REGU" 
cRegOpts = "REGU"

*-- Page 2.
cPg2Tot1_Caption = "REG"
cPg2Tot2_Caption = "PRIME"
cPg2Tot3_Caption = "ABS"
cPg2Tot4_Caption = "TSUP"
cPg2Tot5_Caption = "C/D"

*-- Holiday Handling
cPreloadHolidaysFor = ".t."				&& Logical expression
cHolidayOpt = "FERIE"

*** Debugging  
cLogStatus = "startlog.txt" 				&& triggers for start logging 
cLogPath = "\TEMP\" 							&& log folder 
_DEBUG = .T.									&& set .T. for debugging 

*========================================================
procedure Init()
parameters loThisform, pcSubType, pnPersId

dodefault(loThisform, pcSubType, pnPersId)

*** Create cursors to hold schedule values 
this.CreateScheduleCursor()

return

*=========================================================
procedure FindObj()
dodefault()

this.oBankHours = findobj(this.oThisform, "BankHours")

return

*=========================================================
procedure ReleaseObjRefs
store null to .oBankHours

dodefault()

return

*==========================================================
procedure cmdGo_EnableDisable(poThis)
**** Here 
if !used("vNew")
	return .f. 
endif 	

if empty(vNew.TT__EUNIT) ;
and empty(vNew.TT__EURATE)
	return dodefault(poThis)
else 
	return this.oThisform.Updmode <> " " ;
			and ( !empty(vNew.TT__EUNIT) ;
			or !empty(vNew.TT__EURATE) )
endif 

*==========================================================
procedure Opt_Valid(poThis)
*
dodefault(poThis)

if type("poThis.Parent.cboExpense") = "O"
	poThis.Parent.cboExpense.DisplayValue = ""
	poThis.Parent.cboExpense.Requery(Optlist.TblId)
endif 
this.SetScreen(poThis,Optlist.TT_TYPE)

return

*==========================================================
procedure EmplDay_Valid(poThis)
*
if dodefault(poThis)
	this.FillInScheduleList()
endif 
	
return 

*=========================================================
procedure Init_CreateVNew()
local lnSelect 

if !dodefault()
	return 
endif 	

lnSelect = select()

if !used("vNew")
	return 
endif 

select vNew 
alter table vNew add column TT__EUNIT N(10,4)	
alter table vNew add column TT__EURATE N(10,2)	

select(lnSelect)
return 

*=========================================================
procedure CreateScheduleCursor()
*
with this 

if !empty(.csrBankHrs)
	use in select(.csrBankHrs)

	create cursor (.csrBankHrs) ;
		(cHours C(5), cDescr C(30), nHours N(9,4), ;
		 cInOut C(25), cSched C(25))
endif 					

endwith 

return 

*=========================================================
procedure FillInScheduleList()
***
local loBizPlanDt, lcWkHours, lnN, lcDays
local lcSchedType, lnSelect, ldEffdt
private loSched, loRndSchema, lcAMPM 

store null to loBizPlanDt
store 0 to lnWkHours, lnN
store "" to lcDays, lcInOut, lcAMPM
store {} to ldEffdt 

with this

if empty(.PersId)
	return 
endif 

do case
case vNew.TT_day = 0
	ldEffdt = {}
case between(vNew.TT_Day, 1, 7)
	ldEffdt = this.dFromDt + vNew.TT_Day - 1
case between(vNew.TT_Day, 9, 15)
	ldEffdt = this.dFromDt + vNew.TT_Day - 2
endcase 

loBizPlanDt = this.oBizmgr.GetBiz("PLANDT")
loSched = loBizPlandt.GetSched("/SCHED /OBJECT", ;
			 this.PersId, ldEffdt, ldEffdt)

if isnull(loSched) 
	return 
endif 	

lnSelect = select()
this.cSchedId = trim(loSched.BaseSchedId) 

*** set step on 
if reccount(.csrBankHrs) > 0 
	delete all in (.csrBankHrs)
	go top in (.csrBankHrs)
endif 	

for lnN = 1 to 4 
	store "" to lcDays, lcInOut
	store 0 to lnWkHours
	
	do case 
	case lnN = 1
		if empty(loSched.Type)
			lcDays = iif(gcLang="E", "No schedule", "Pas de calendrier.")
			lnN = 5 
		else 
			lcDays = iif(gcLang="E", "(Unknown)", "(Aucun)")
		endif 
		lnWkHours = 0
	case lnN = 2 
		lcDays = iif(gcLang="E", "Full day", "Journée complète") 
		lnWkHours = loSched.Hours
		if !empty(loSched.EndExHM) and !empty(loSched.EndExHM) 
			lcInOut = trim(loSched.StartHM)+"-"+trim(loSched.StartExHM)+;
			   	CRLF + trim(loSched.EndExHM)+"-"+trim(loSched.EndHM)
		else
			lcInOut = trim(loSched.StartHM)+"-"+trim(loSched.EndHM)
		endif
	case lnN = 3
		if !empty(loSched.StartExHM)
			lcDays = "AM"
			lcAMPM = trim(loSched.StartHM)+"-"+trim(loSched.StartExHM)
		   lcInOut = trim(loSched.StartHM)+"-"+trim(loSched.StartExHM)
			lnWkHours = HMtoH(trim(loSched.StartExHM)) ;
					   	- HMtoH(trim(loSched.StartHM))
		endif 
	case lnN = 4
		if !empty(loSched.EndExHM)
			lcDays = "PM"
			lcAMPM = trim(loSched.EndExHM)+"-"+trim(loSched.EndHM)
		   lcInOut = trim(loSched.EndExHM)+"-"+trim(loSched.EndHM)		  
			lnWkHours = HMtoH(trim(loSched.EndHM)) ;
					   	- HMtoH(trim(loSched.EndExHM))
		endif 
	endcase 

	if !empty(lcDays)
		lcWkHours = HtoHM(lnWkHours, -1)
		lcInOut = chrtran(lcInOut, ":", "")

		insert into (.csrBankHrs) (cHours, cDescr, nHours, cInOut, cSched);
			values (lcWkHours, lcDays, lnWkHours, lcInOut, lcAMPM)
	endif 
next 

*** select (.csrBankHrs)
*** browse normal 
endwith

select(lnSelect)
store null to loBizPlanDt, loSched

return

*=========================================================
procedure BankHours_Valid (poThis)
*
if !dodefault(poThis)
	return 
endif 	

with this 

.nSchedHours = qSchedList.nHours 
.cSchedHours = trim(qSchedList.cHours)

replace vNew.TT_HOURS with .nSchedHours in vNew 
replace vNew.TT_INOUT with trim(qSchedList.cInOut) in vNew 

endwith 

return

*=========================================================
#define SCREEN_LAYOUT 
*** Procedures related to screen layout of controls 
*=========================================================
procedure SetScreen(poThis, tcTranxType)
*** This procedure refreshes the GRID columns 
* base on transaction type  
* OPTIONS:	1 - AMTOPT 
*				2 - ABSENCE - can by taken by 1/2 day  
*				3 - PRIME - Expenses
*
local lcTranxType, llExpenseAccount, lcOPTValue
store "" to lcTranxType, lcOPTValue

*
with this 

lcTranxType = trim(tcTranxType)
if lcTranxType = "P"
	llExpenseAccount=ToLogical(tbleval("PRIME", Optlist.TBLID, "TBLC6"))
endif 

.RefreshScreen(poThis.Parent,lcTranxType,llExpenseAccount)

*** Refresh the EXPENSES 
if llExpenseAccount
	if type("poThis.Parent.cboExpense") = "O" ;
	and poThis.Parent.cboExpense.Visible 
		if (atw("-",poThis.Parent.Opt.DisplayValue)>0)
			lcOPTValue = trim(leftto(poThis.Parent.Opt.DisplayValue,"-"))
			poThis.Parent.cboExpense.Requery(lcOPTValue)
		endif 
	endif 
endif 

.oThisform.Refresh()
endwith 

return 

*==========================================================
procedure RefreshScreen(poThis,tcTranxType,tlExpenseAccount)
*** This procedure refreshes the GRID columns 
* base on OPT values 
* OPTIONS:	1 - AMTOPT 
*				2 - PRIME - Expenses
*
local llAbsence 
this.cOptType = upper(trim(tcTranxType))
llAbsence = (this.cOptType = "A")

if type("poThis.GrdEmpl") != "O" 
	return 
endif

with poThis

	*** Show regular time input 
	.Hours.visible = (!llAbsence and !tlExpenseAccount)
	.Hours.LabelObj.visible = (!llAbsence and !tlExpenseAccount)

	*** Do not show TxtInOut for expenses   
	***.TxtInOut.visible = !tlExpenseAccount

	*** Show absences input 
	.BankHours.visible = (llAbsence and !tlExpenseAccount)
	.BankHours.LabelObj.visible = (llAbsence and !tlExpenseAccount) 

	*** Show expense account input 
	.txtEUnit.visible = tlExpenseAccount
	.txtEUnit.LabelObj.visible = tlExpenseAccount
	.cboExpense.visible = tlExpenseAccount
	.cboExpense.LabelObj.visible = tlExpenseAccount
	
	.Refresh()	
endwith 

return 

*==========================================================
procedure GrdEmpl_SetProperties(poThis)
local lcDynamicBackColor

if !used("vTimetmp")
	return
endif

with poThis

.RecordSource = "vTimeTmp"
.ColumnCount = 8
.RowHeight = 21
lcDynamicBackColor = ;
		"iif(vTimetmp.TT_EFFDT<thisform.oBhv.dFromDt, " + ;
		"rgb(232,232,255), iif((vTimetmp.TT_HOURS=0), " + ;
		"rgb(241,250,220), iif(dow(vTimetmp.TT_EFFDT)%2=0,"+;
		"rgb(255,255,255), rgb(255,255,192))))"
		
with .Column1
	.ControlSource = [vTimeTmp.vvShowday]
	.Width = 106
	.Alignment = 0		&& Middle Left
	.ReadOnly = .t.
	.SelectOnEntry = .f.
	.DynamicBackcolor = lcDynamicBackColor
endwith

with .Column2
	.ControlSource = [vTimeTmp.TT_ShowOPT]
	.Width = 150
	.Alignment = 0		&& Middle Left
	.ReadOnly = .t.
	.SelectOnEntry = .f.
	.DynamicBackcolor = lcDynamicBackColor
endwith

with .Column3
	.ControlSource = [vTimeTmp.vvHHMM]
	.Width = 50
	.Alignment = 1		&& Middle Right
	.Format = "R"
	.InputMask = "99999:99"
	.DynamicBackcolor = lcDynamicBackColor
endwith

with .Column4
	.Width = 75
	.ControlSource = ;
			[iif(vTimetmp.TT_INOUT="", "", ] + ;
			[transf(left(vTimeTmp.TT_INOUT,5),"@R 99:99") + "-" + ] + ;
			[transf(substr(mline(vTimetmp.TT_INOUT,memlines(vTimetmp.TT_INOUT)),6,4),"@R 99:99"))]
	.ReadOnly = .t.
	.DynamicBackcolor = lcDynamicBackColor
endwith

with .Column5
	.Width = 90
	.ControlSource = [tblevalb("JOB", vTimeTmp.TT_JOBID)]
	.ReadOnly = .t.
	.DynamicBackcolor = lcDynamicBackColor
endwith

*** Add the EXPENSE UNIT column 
with .Column6
	.ControlSource = [vTimeTmp.TT__EUNIT]
	.Width = 60
	.Alignment = 1		&& Middle Right
	.Format = "@Z"
	.InputMask = "999.99"
	.DynamicBackcolor = lcDynamicBackColor
endwith

*** Add the EXPENSE UNIT column 
with .Column7
	.ControlSource = [vTimeTmp.TT__EURATE]
	.Width = 56
	.Alignment = 1		&& Middle Right
	.Format = "@Z"
	.InputMask = "999.99"
	.DynamicBackcolor = lcDynamicBackColor
endwith

with .Column8
	.Width = 113
	.ControlSource = [trim(left(vTimeTmp.TT_Notes,127))]
	.ReadOnly = .t.
	.DynamicBackcolor = lcDynamicBackColor
endwith

*** Set read-only prop based on signed status
this.GrdEmpl_EnableDisable(poThis)

endwith
return

*=========================================================
procedure GrdEmpl_InitNewRow
*** Use this to protect/unprotect columns
nodefault

return

*==========================================================
procedure GrdBatch_SetProperties(poThis)
*** Resize the column to match the totals 
dodefault(poThis)

with poThis

*** Wider column range 
* accept for approval 
*!*	.Column3.Width = 164
*!*	.Column4.Width = 240
*!*	.Column5.Width = 70
*!*	.Column6.Width = 55
*!*	.Column7.Width = 55
*!*	.Column8.Width = 55
*!*	.Column9.Width = 55
*!*	.Column10.Width = 55
*!*	.Column11.Width = 55

.Column3.Width = 125
.Column4.Width = 140
.Column5.Width = 70
.Column6.Width = 43
.Column7.Width = 50
.Column8.Width = 50
.Column9.Width = 50
.Column10.Width = 50
.Column11.Width = 52

endwith 

return 

endproc

*=========================================================
procedure LoadQBatch(pnPersid)
*** Called from PageActivate() 
*** when batch tab (page 2) activated

local loQBatch1, lcFields, llFirstTime, lnSaveRecno
local loCurTot, lcAlias, lnSelect

lnSelect = select()

select QBATCH
llFirstTime = (reccount() = 0)
lnSaveRecno = iif(eof(), 0, recno())

if llFirstTime
	append from (dbf("qBossed")) for H_PERSID <> 0
	replace all BA_NAME with name(H_Persid)
endif

*** It would have been if this field could have been 
*** dynamically build - Stefan 
lcFields = ;
			"TT_PERSID, TT_OPT, " + ;
		 	"sum(round(tt_hours*60,0)) as tt_minutes, " + ;
		 	"00000.0000 as tt_hours, " + ;
			"0 as TT_EXCEPTN, " + ;
			"max(nvl(TT_SIGNBY,space(20))) as TT_SIGNBY, " + ;
			"max(nvl(TT_VALBY,space(20))) as TT_VALBY, " + ;
			"max(nvl(TT_POSTBY,space(20))) as TT_POSTBY, " + ;
			"max(TT_MOTIF) as TT_MOTIF, " + ;
			"max(TT_ENTITY) as TT_ENTITY, " + ;
			"max(TT_JOBID) as TT_JOBID, " + ;
			"max(TT_STATIS) as TT_STATIS, " + ;
			"max(TT_PROJECT) as TT_PROJECT, " + ;
			"max(left(TT_NOTES,10)) as TT_NOTES, " + ;
			"max(left(TT_MSG1,254)) as TT_MSG1, " + ;
			"'     ' as TT_TBLC8, " + ;
		 	"ROUND(SUM(TT__EUNIT),4) TT__EUNIT, " + ;
		 	"ROUND(SUM(TT__EURATE),4) TT__EURATE"
			
loQBatch1 = this.oBiz.GetList (;
				"/cursor=qBatch1", ;
				lcFields, ;
				"TT_BATCH='" + trim(this.cBatch) + "'", ;
				"TT_PERSID, TT_OPT", ;
				"TT_PERSID, TT_OPT")

*** Fix up HOURS from minutes
replace all TT_HOURS with TT_MINUTES / 60

*** Fill in TBLC8
set order to TBLID in Optlist
set relation to TT_OPT into Optlist
replace all TT_TBLC8 with Optlist.TBLC8
set relation to
set order to 0 in Optlist

*** Determine what is Exceptional
this.LoadQBatch_FlagExceptions()

*** Summarize totals
this.LoadQBatch_Summarize()

index on TT_PERSID tag TT_PERSID

*** Merge into QBatch
select QBatch
set relation to H_PERSID into qBatch2
replace all ;
		BA_TOT1 with qbatch2.TT_TOT1, ;
		BA_TOT2 with qbatch2.TT_TOT2, ;
		BA_TOT3 with qbatch2.TT_TOT3, ;
		BA_TOT4 with qbatch2.TT_TOT4, ;
		BA_TOT5 with qbatch2.TT_TOT5, ;
		BA_SIGNED with qbatch2.TT_SIGNED, ;
		BA_VALED with qbatch2.TT_VALED, ;
		BA_EXCEPTN with qbatch2.TT_EXCEPTN=1, ;
		BA_POSTED with qbatch2.TT_POSTED

replace all ;
		BA_Total with BA_TOT1+BA_TOT2+BA_TOT3+BA_TOT4+BA_TOT5

replace all ;
		BA_TOT1HM  with HtoHM(BA_TOT1, 3, "/DELIM"), ;
		BA_TOT2HM  with HtoHM(BA_TOT2, 3, "/DELIM"), ;
		BA_TOT3HM  with HtoHM(BA_TOT3, 3, "/DELIM"), ;
		BA_TOT4HM  with HtoHM(BA_TOT4, 3, "/DELIM"), ;
		BA_TOT5HM  with HtoHM(BA_TOT5, 3, "/DELIM"), ;
		BA_TOTALHM with HtoHM(BA_TOTAL, 3, "/DELIM")

release loQBatch1
use in select("qBatch2")

select QBatch
index on BA_NAME tag BA_NAME

*** Calculer les totaux des totaux
SELECT qBatch
SUM ba_tot1 TO this.oThisform.nBatchTot1
SUM ba_tot2 TO this.oThisform.nBatchTot2
SUM ba_tot3 TO this.oThisform.nBatchTot3
SUM ba_tot4 TO this.oThisform.nBatchTot4
SUM ba_tot5 TO this.oThisform.nBatchTot5
if type("this.oThisform.nBatchTotal") = "N"
	SUM ba_Total TO this.oThisform.nBatchTotal
endif

*** Set filter based on what user has checked
this.QBatch_SetFilter()

if llFirstTime or lnSaveRecno = 0
	if !empty(pnPersid)
		locate for H_PERSID = pnPersid
	else
		this.InitNewPers()
	endif
else
	do GOTO with lnSaveRecno, "qBatch" 
endif

select (lnSelect)
return


*=========================================================
procedure LoadQBatch_Summarize()
*** A separate method to facilitate overriding

set enginebehavior 70
select TT_PERSID, ;
		 (TT_SIGNBY<>"") as TT_SIGNED, ;
		 (TT_VALBY<>"") as TT_VALED, ;
		 (TT_POSTBY<>"") as TT_POSTED, ;
		 max(TT_EXCEPTN) as TT_EXCEPTN, ;
		 sum(iif("1"$TT_TBLC8, TT_HOURS, 0) + 000.00) as TT_TOT1, ;
		 sum(iif("2"$TT_TBLC8, TT_HOURS, 0) + 000.00) as TT_TOT2, ;
		 sum(iif("3"$TT_TBLC8, TT_HOURS, 0) + 000.00) as TT_TOT3, ;
		 sum(iif("4"$TT_TBLC8, TT_HOURS, 0) + 000.00) as TT_TOT4, ;
		 sum(TT__EUNIT*TT__EURATE) as TT_TOT5, ;
		 max(TT_MOTIF) as TT_MOTIF, ;
		 max(TT_ENTITY) as TT_ENTITY, ;
		 max(TT_JOBID) as TT_JOBID, ;
		 max(TT_STATIS) as TT_STATIS, ;
		 max(TT_PROJECT) as TT_PROJECT, ;
		 max(left(TT_NOTES,10)) as TT_NOTES, ;
		 max(left(TT_MSG1, 254)) as TT_MSG1 ;
	from qBatch1 ;
	group by TT_PERSID ;
	into cursor qBatch2
set enginebehavior 80

return

*=========================================================
procedure GrdEmpl_DblClick(poThis)
local lnSelect, lnWkHours, ltDateTime  

dodefault()

lnSelect = select()
*
with this

	*** Replace ASA the user double click so that 
	* TS sees that the current record is being modified
	ltDateTime = datetimex()
	replace next 1 TT_MODDT with ltDateTime in vTimetmp 
	replace next 1 TT_MODDT with ltDateTime in vNew 

	.nSchedHours = vTimetmp.TT_HOURS
	.cSchedHours = trim(vTimetmp.TT_INOUT)

	*** Refresh screen 
	.SetScreen(poThis,vTimetmp.TT_TYPE)

	*** Position the record in the cursor 
	if !empty(.cSchedHours)
		select (.csrBankHrs) 
		locate for trim(cInOut) = .cSchedHours and !eof()

		poThis.Parent.BankHours.DisplayValue = trim(cHours)
	endif 

	poThis.Parent.cboExpense.DisplayValue = vNEW.TT__EURATE
	poThis.Parent.cboExpense.Refresh()
endwith 

select(lnSelect)
return 

*==========================================================
procedure TxtInOut_Clicked(poThis)
local lcComm1

if trim(gcLang) = "E" 
	lcComm1= "This value [" + trim(poThis.Value) + ;
		"] cannot be modified manually!"+CRLF+;
		"Please use the drop-down list for another selection."  
else
	lcComm1 = "Cette valeur [" + trim(poThis.Value) + ;
	"] ne peut pas être modifié manuellement!"+CRLF+;
	"S.V.P. utiliser la liste déroulente pour une autre sélection." 
endif 

alert(name(this.PersId)+CRLF+lcComm1, ;
		0, "\!\?\<OK")

return 

*==========================================================
procedure cmdClear_Valid()

dodefault()

this.RefreshControl()

return 

*==========================================================
procedure RefreshControl()

with this 

.cSchedHours= "" 
if !empty(.cSchedHours)
	go top in (.csrBankHrs) 
endif 

.oThisform.Pgf1.Page1.BankHours.DisplayValue = ""
.oThisform.Pgf1.Page1.BankHours.Refresh()

endwith 

return 

*=========================================================
#define VALIDATION_METHODS 
*=========================================================
procedure txtInOut_Valid_MinimumLunchBreak
lparameters toAnalysis, lcLunchBreakMM
*** Verify that the total time taken for lunch respects  
* the union agreement. The numeric value it is specified 
* in the TBL and these values need to be specified by 
* the schedule id.
* 
* Ex. SC_SCHEDID = FLE325, TBL = "_SCHEDULE", TBLC8 = 45 
* which means that everyone who has the schedule FLE325 
* need to take at least 45 minutes for lunch break.
*
local loSched, loBizPlanDt
local lcStartExHM, lcEndExHM
local lnStartExHM, lnEndExHM
local ldEffdt, llOk 

llOk = .t. 
store null to loSched, loBizPlanDt
store "" to lcStartExHM, lcEndExHM
store 0 to lnStartExHM, lnEndExHM
store {} to ldEffdt 

if empty(this.cSchedId)
	ldEffdt = ttod(toAnalysis.StartDTM) 
	loBizPlanDt = this.oBizmgr.GetBiz("PLANDT")
	if !isnull(loBizPlanDt)
		loSched = loBizPlandt.GetSched("/SCHED /OBJECT", ;
					 this.PersId, ldEffdt, ldEffdt)
	endif 

	if isnull(loSched)
		store null to loSched, loBizPlanDt
		return llOk
	endif 
	this.cSchedId = trim(loSched.BaseSchedId) 
endif 

if memlines(toAnalysis.UsedHM)<2
	return llOk
endif 

*** Get the minimum lunch break from TBL
lcLunchBreakMM = tbleval("_SCHEDULE", this.cSchedId, "TBLC8")
*** debug 
this.WriteLog("txtInOut_Valid_MinimumLunchBreak() - " + ;
		"lcLunchBreakMM = " + transform(lcLunchBreakMM))

if val(lcLunchBreakMM)=0
	return llOk 
endif 

*** Get the total lunch break taken 
lcStartExHM = rightfrom(mline(toAnalysis.UsedHM, 1), "-")
lnStartExHM = HMtoM(trim(lcStartExHM))
lcEndEXHM = leftto(mline(toAnalysis.UsedHM, 2), "-")
lnEndEXHM = HMtoM(trim(lcEndEXHM))

*** No lunch break 
if empty(lcStartExHM) and empty(lcEndEXHM)
	return llOk 
endif 

*** Is the lunch break taken higher than the minimum 
* defined by the union 
if lnEndEXHM > lnStartExHM ;
	and castx(lnEndEXHM-lnStartEXHM,"N") < val(lcLunchBreakMM)

	*** Override the end of lunch 
	lnEndEXHM = lnStartExHM + val(lcLunchBreakMM) 
	lcEndEXHM = MtoHM(lnEndEXHM, "/TIME")
	lcEndEXHM = chrtran(lcEndEXHM, ":", "")
	toAnalysis.UsedHM = mline(toAnalysis.UsedHM,1)+CRLF+;
			stuff(mline(toAnalysis.UsedHM,2), 1, 4, lcEndEXHM)

	*** debug 
	this.WriteLog("txtInOut_Valid_MinimumLunchBreak() - " + ;
			"lcEndEXHM = " + transform(lcEndEXHM))
	this.WriteLog("txtInOut_Valid_MinimumLunchBreak() - " + ;
			"UsedHM = " + transform(toAnalysis.UsedHM))
	
	return !llOk 
endif 

return llOk 
endproc

*=========================================================
procedure txtInOut_Valid_Custom(poThis, loAnalysis)
*** Called from txtInOUt_Valid. Return .f. to suppress 
*   standard alert message. You can still display your own
*
local llOk, lcLunchBreakMM
local lcComm1, lcComm2, lcComm3

store "" to lcLunchBreakMM 
llOk = .t. 

if trim(gcLang) = "E" 
	lcComm1 = "The minimum time required for the lunch break "+;
				"has not been respected!" 
	lcComm2 = "The time required for the meal: "
	lcComm3 = " minutes will be removed from your time."
else
	lcComm1 = "Le temps minimum requis pour la pause de midi "+;
				"n'a pas été respectée!"
	lcComm2 = "Le temps obligatoire pour le repas: "
	lcComm3 = " minutes va être enlevé du votre temps."
endif 

*** set step on 
*** Store the values after they have been rounded 
if empty(loAnalysis.UsedHM)
	return llOk 
endif 

*** Validate minimum amount of time taken for lunch break
llOk = iif(llOk, ;
	this.txtInOut_Valid_MinimumLunchBreak(loAnalysis,@lcLunchBreakMM),.f.)
if !llOk 
	alert(name(this.PersId)+CRLF+lcComm1+CRLF+;
			lcComm2+trim(lcLunchBreakMM)+lcComm3,0, "\!\?\<OK")
endif 

replace TT_PUNCH with trim(loAnalysis.OrigDTM)
replace TT_INOUT with trim(loAnalysis.UsedHM) 

*** do not show the generic messages 
return iif(llOk, .f., llOk)
endproc

*=========================================================
procedure ValidateSheet_CheckOneDay(ldEffdt,lnCount,lnRegHrs)
*** We need to credit a number of hours for each day 
* at the signature. For example, an employee works 5.45 hours
* but he gets paid 7 hours. 
* We need to generate a transaction with an absence code
* for the difference in hours on daily basis. - S.G.
*
local lnSelect, lcAbsenceId, lnDeficitHours  

store "" to lcAbsenceId
store 0 to lnDeficitHours 

if !this.lSigning
	return 
endif

lnSelect = select()

*** set step on 
*** Filter out the dates 
select TT_EFFDT ;
from vTimetmp with (buffering=.t.) ;
where TT_EFFDT = ldEffdt and TT_OPT<>'**' ;
into cursor qTTmp 

if reccount("qTTmp") < 1 
	*** debug 
	this.WriteLog("ValidateSheet_CheckOneDay() - " + ;
				"qTTmp.RecordCount = 0.")
	
	use in select("qTTmp")
	select (lnSelect)
	return 
endif 


with this 

*** Get the absence code from TBL
lcAbsenceId = tbleval("_SCHEDULE",qJobhist.H_SCHEDID,"TBLC7")

*** debug 
.WriteLog("ValidateSheet_CheckOneDay() - " + ;
			"lcAbsenceId = " + transform(lcAbsenceId))

if empty(qJobhist.H_SCHEDID) or empty(lcAbsenceId) 
	use in select("qTTmp")
	select (lnSelect)
	return 
endif 	

select vTimeTmp 
locate for vTimeTmp.TT_EFFDT=qTTmp.TT_EFFDT and !eof()
if found()
	lnDeficitHours = .ValidateSheet_GetExtendedHours( ;
					qJobhist.H_SCHEDID, vTimeTmp.TT_EFFDT, lnRegHrs)

	if !empty(lnDeficitHours)
		.AddTrans("/SAVE", vTimetmp.TT_EFFDT, lnDeficitHours, ;
				lcAbsenceId, "A")
	endif 
endif 

endwith 

select (lnSelect)
return 

*======================================================================
procedure ValidateSheet_GetExtendedHours(tcSchedId,tdEffDt,tnRegHours)
*** The procedure return the hour's difference between the scheduler
* "general" and "detail" tabs. 
* For example, the "general" tab is configured to work 7 hours per day 
* while the "detail" tab shows that the actual number of hours 
* that the employee needs to work is 5.45.
* The employee is paid for 7 hours and not 5.45. S.G.
*
local lnHMtoHM
local loSched, lnSelect

lnSelect = select()

store null to loSched
store 0 to lnHMtoHM

with this 

*** set step on 
loSched = .oBizSched.WkCal("OBJECT", tcSchedId, tdEffDt)
if isnull(loSched)
	return lnHMtoHM 
endif 

if loSched.AVGHRSPDY > 0 
	lnHMtoHM = loSched.AVGHRSPDY-tnRegHours
endif 

*** Debug 
.WriteLog("ValidateSheet_GetExtendedHours() - " + ;
		"AVGHRSPDY, tnRegHours, lnHMtoHM = " + ;
		transform(loSched.AVGHRSPDY) + ",  " + ;
		transform(tnRegHours) + ",  " + ;
		transform(lnHMtoHM))
endwith 

store null to loSched
select(lnSelect)

return lnHMtoHM

*=========================================================
procedure InitNewPers_Custom()
*** For cusotmization.  Set Plages, etc.
*   Info about the current person is in qJobhist. It
*   contains both PERS and JOBHIST fields.
local lnSelect, loSched, lcPayGRP

store null to loSched
store "" to lcPayGRP

if !used("qJobhist")
	return 
endif 
	
lnSelect = select()

with this 

*** set step on 
*** Get the hours from the schedule instead of qJobhist 
loSched = .oBizSched.WkCal("OBJECT", qJobhist.H_SCHEDID, ;
		qJobhist.H_EFFDT)

if !isnull(loSched) and type("loSched") = "O" 
	.nMaxRegHoursWeek = nvl(loSched.AVGHRSPER,0)
	.nMinRegHoursWeek = nvl(loSched.AVGHRSPER,0)
else 
	.nMaxRegHoursWeek = nvl(qJobhist.H_HRSPER,0)
	.nMinRegHoursWeek = nvl(qJobhist.H_HRSPER,0)
endif 

lcPayGRP = trim(tbleval("PAYGRP", qJobhist.H_PAYGRP, "TBLC1"))
if "B"$lcPayGRP
	.nMaxRegHoursWeek = .nMaxRegHoursWeek * 2 
	.nMinRegHoursWeek = .nMinRegHoursWeek * 2 
endif 

.WriteLog("InitNewPers_Custom() - " + ;
		"nMaxRegHoursWeek = " + transform(.nMaxRegHoursWeek)+;
		",  nMaxRegHoursWeek = "+transform(.nMinRegHoursWeek))

.InitBankWithdrawal()
.getKilometersYTD()

endwith 

store null to loSched
select (lnSelect)
return 

*======================================================================
procedure InitBankWithdrawal()
*
local lnDifferential
lnDifferential = MoreC(qJobhist.H_MORE, "DIFFERENT")

if !used("qJobhist")
	return 
endif 

with this 

if (atw("ATT",trim(qJobhist.H_SCHEDID))>0)
	.cWithdrawDeficitFromBankids = "135"
	.cBank1WithdrawalOpt = "MALATT"
	.cBank3WithdrawalOpt = "VACATT"
	.cBank5WithdrawalOpt = "MOBATT"
	return 
endif 

do case 
*** SEB 
case "SEB"$alltrim(qJobhist.H_UNION)
	.cWithdrawDeficitFromBankids = "1359"
	.cBank1WithdrawalOpt = "MALSEB10"
	.cBank3WithdrawalOpt = "VACSEB10"
	.cBank5WithdrawalOpt = "MOBSEB"
	.cBank9WithdrawalOpt = "CFLEXI"

	.cDepositSurplusIntoBankids = "69"
	.cBank6DepositOpt = "DEP6"
	.cBank9DepositOpt = "DEP9"
*** GOUV 
case inlist(alltrim(qJobhist.H_UNION),"APMCP","SFPQ");
	and !empty(lnDifferential)

	.cWithdrawDeficitFromBankids = "123"
	.cBank1WithdrawalOpt = "MALAPM5"
	.cBank2WithdrawalOpt = "MALGOUV"
	.cBank3WithdrawalOpt = "VACAPM10"
*** APMCP
case "APMCP"$alltrim(qJobhist.H_UNION) ;
	and empty(lnDifferential)

	.cWithdrawDeficitFromBankids = "1369"
	.cBank1WithdrawalOpt = "MALAPM5"
	.cBank3WithdrawalOpt = "VACAPM10"
	.cBank6WithdrawalOpt = "CRECUP"

	.cDepositSurplusIntoBankids = "9"
	.cBank6DepositOpt = "DEP6"
*** NONSYN
case "NONSYN"$alltrim(qJobhist.H_UNION)
	.cWithdrawDeficitFromBankids = "9"
	.cBank9WithdrawalOpt = "CFLEXI"

	.cDepositSurplusIntoBankids = "9"
	.cBank9DepositOpt = "DEP9"
endcase 

.WriteLog("InitBankWithdrawal() = " + CRLF +;
		" cWithdrawDeficitFromBankids = " + ;
			transform(.cWithdrawDeficitFromBankids)+CRLF+;
		" cBank1WithdrawalOpt = " + ;
			transform(.cBank1WithdrawalOpt)+CRLF+;
		" cBank2WithdrawalOpt = " + ;
			transform(.cBank2WithdrawalOpt)+CRLF+;
		" cBank3WithdrawalOpt = " + ;
			transform(.cBank3WithdrawalOpt)+CRLF+;
		" cBank5WithdrawalOpt = " + ;
			transform(.cBank5WithdrawalOpt)+CRLF+;
		" cBank6WithdrawalOpt = " + ;
			transform(.cBank6WithdrawalOpt)+CRLF+;
		" cBank9WithdrawalOpt = " + ;
			transform(.cBank9WithdrawalOpt))

endwith 
return 

*======================================================================
#define HELPER_METHODS 
*======================================================================
procedure getKilometersYTD()
*** This procedure counts the total number of kilometers that 
* has been entered in the timehseet through expense account 
* by an employee. The method uses a counter to do this.
*
local loCsrTemp 
local lcPayGRP, lcPayNo, lcLastPayNo, ldFrom, ldThru
local lcCNTWhere, lcWhere 

*** set step on 
loCsrTemp = null 
store "" to lcPayGRP, lcPayNo, lcLastPayNo, ldFrom, ldThru
store "" to lcCNTWhere, lcWhere 

if !used("qJobhist")
	return 
endif 	

with this 

*** set step on 
lnPersId = this.PersId  
lcPayGRP = trim(qJobhist.H_PAYGRP)
lcPayNo = lcPayGRP + left(dtos(date()), 4) + "01"

ldFrom = .oBizPayno.GetValueById(lcPayNo, "PN_STARTDT")

lcLastPayNo = .oBizPayno.GetLastPayNo(left(lcPayNo, 8), "/REG")
ldThru = .oBizPayno.GetValueById(lcLastPayNo, "PN_ENDDT")

lcFields = "COALESCE(sum(T__EUNIT),0) AS TOTKMS"
lcCNTWhere = TCNTVAL("EXPR$","KM")

if empty(lcCNTWhere)
	store null to loCsrTemp
	return 
endif 	

lcWhere = Makefor("", lcCNTWhere, ;
				"T_PERSID=" + tochar(lnPersId, "/DTOS/QUOTES"), ;
				iif(empty(ldFrom), "", "T_EFFDT>=" + ;
						tochar(ldFrom, "/SQL/QUOTES")), ;
				iif(empty(ldThru), "", "T_EFFDT<=" + ;
						tochar(ldThru, "/SQL/QUOTES")))

loCsrTemp = .oBizTimedt.GetListByRange (;
				"/CURSOR=qCntKms", lcFields, ;
				lcWhere, .f., "", "", "", "", ;
				"", "")

if !used("qCntKms")
	return 
endif 

.cKilometerYTD = round(nvl(qCntKms.TOTKMS,0),2)

store null to loBizPayno, loBizTimeDt, loCsrTemp
use in select("qCntKms")

endwith 

return
endproc

*======================================================================
procedure cmdLog_GetLogFile( poThis, pnLogIndex)
*** DEBUG procedure 
local lcLogFileName 
local array laPath[1] 

lcLogFileName = this.cLogPath
do case 
case pnLogIndex = 1 
	lcLogFileName = lcLogFileName + ;
			"_bhvtimetmp1_custom.log" 
case pnLogIndex = 2 
	lcLogFileName = lcLogFileName + ;
			"_biztimetmp1_custom.log" 
case pnLogIndex = 3 
	lcLogFileName = lcLogFileName + ;
			"_cvacationbank.log" 
case pnLogIndex = 4
	lcLogFileName = lcLogFileName + ;
			"_csickbank.log" 
otherwise 
	store "" to lcLogFileName
endcase 

with this 

.WriteLog("lcLogFileName = " + ;
		transform(lcLogFileName))

if !file(lcLogFileName)
	lcLogFileName = ""
endif 

endwith 

return lcLogFileName 

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


*======================================================================
#define HOT_FIXES 
*======================================================================
procedure ValidateSheet_HourDeficit(lnRegHrs)
*** Employee has not worked enough hours this week.
*   Ask what bank to withdraw from.
*
private lnBankid, lcBankid, lnR, lcR, lnRows, lnReply
private llOK, lnMargin, lcMargin, lcOpt, lnSurplus, lnDefault
private lcBankid1, lcBankid2, lcBankid3, lcBankId4
private lcForClause, lnWeeks
private all like lcBankname*
private all like lcBankInfo*
private all like lcHHMM*
private all like lnHours*
private all like lnMargin*
private lcMessage, lctake, lntake, loThisform

store null to loThisform

for lnN = 1 to 9
	lcN = str(lnN,1)
	store 999999 to ("lnMargin"+lcN)
	store 0 to ("lnHours"+lcN)
	store "" to ("lcHHMM"+lcN), ("lcBankid"+lcN), ("lcBankName"+lcN)
	store "" to ("lcBankInfo"+lcN)
endfor

lnWeeks = int(this.nDaysPer / 7)

if !this.lRequireMinHours
	if gcLang = "E"
		lnReply = alert("You have only entered " + longtime(lnRegHrs) + ;
				  ",;which is less than the minimum of " + ;
				  longtime(this.nMinRegHoursWeek) + ;
				  ";;Voulez vous combler cette différence?", ;
				  0, "\!\<Oui|Combler et signer;" + ;
				  	  "\<Non|Signer sans combler;" + ;
				  	  "\?\<Annuler|Ne pas signer", ;
				  "/LEFT")
	else
		lnReply = alert("Vous n'avez saisi que " + longtime(lnRegHrs) + ;
				  " de temps régulier,;qui est inférieur au minimum de " + ;
				  longtime(this.nMinRegHoursWeek) + ;
				  ";;Voulez vous combler cette différence?", ;
				  0, "\!\<Oui|Combler et signer;" + ;
				  	  "\<Non|Signer sans combler;" + ;
				  	  "\?\<Annuler|Ne pas signer", ;
				  "/LEFT")
	endif
	if lnReply > 1
		*** Get out. Return .t. if NO, .f. if CANCEL
		return lnReply = 2
	endif
endif

*** Determine which banks he can withdraw from
lnRows = 0
lnDeficit = this.nMinRegHoursWeek - lnRegHrs
lntake = lndeficit
lctake = HtoHM(lntake, 3, "/DELIM")

if this.lShowBanksInReverseOrder
	lcForClause = "9 to 1 step -1"
else
	lcForClause = "1 to 9"
endif

for lnBankid = &lcForClause
	lcBankid = str(lnBankid,1)
	if !lcBankid $ this.cWithdrawDeficitFromBankids
		*** Not a valid bank for withdrawal
		loop
	endif
	if empty(qJobhist.H_APLAN&lcBankid)
		*** Employee doesn't have this bank
		loop
	endif

	*** Found one.  Check current balance and max.
	select qTimsum
	locate for TS_BANKID = lcBankid
	if !found()
		*** Strange
		loop
	endif

	*** Stefan change 
	*** set step on 
	lnMargin = iif(lcBankid$this.cBanksToValidate,TS_NEWBAL,999999)
	if lnMargin <= TS_MIN
		*** Empty 
		loop
	endif

	*** Prepare to add a row to the subform
	lnRows = lnRows + 1
	lcR = str(lnRows,1)
	lcBankid&lcR = lcBankid
	lcBankname&lcR = iif(this.lShowPlanNames, ;
				tbleval("APLAN", qJobhist.H_APLAN&lcbankid), ;
				tbleval("TIMEBANK", lcBankid))
	store min(lnMargin, lnDeficit) to ("lnMargin"+lcR)
	if lnMargin < lnDeficit
		lcMargin = longtime(lnMargin)
		lcBankinfo&lcR = ;
					"Vouz pouvez retirer jusqu'à " + lcMargin + " ici"
	else
		lcBankinfo&lcR = ;
					"Vous pouvez retirer du temps ici"
	endif

	*** Preload with default amount
	lnDefault = min(lnTake, lnMargin)
	store lnDefault to ("lnHours"+lcR)
	store HtoHM(lnDefault,3, "/DELIM") to ("lcHHMM"+lcR)
	lnTake = lnTake - lnDefault
endfor

lctake = HtoHM(lntake, 3, "/DELIM")

if lnRows = 0
	*** No bank withdrawls possible
	if gcLang = "E"
		alert("You have only entered " + longtime(lnreghrs) + ;
				",;which is less than the " + ;
				longtime(this.nMinRegHoursWeek) + ;
				" hours required for this period." + ;
				".;;Enter the additonal hours as absenced if you need to.", ;
				0, "\!\?\<OK", "/LEFT")
	else
		alert("Vous avez saisi " + longtime(lnreghrs) + " de temps," + ;
				";qui est moins que le temps reglementaire requis de " + ;
				longtime(this.nMinRegHoursWeek) + ;
				".;;Saisissez les heures manquantes comme absences s'il faut.", ;
				0, "\!\?\<OK", "/LEFT")
	endif
	select vTimetmp
	return .f.
endif

if gcLang = "E"
	lcMessage = "You have entered " + longtime(lnRegHrs) + ;
			  " of regular time, which is less then  minimum allowed of " + ;
			  longtime(this.nMinRegHoursWeek) + ;
			  ". How to fill this difference?"
else
	lcMessage = "Vous avez saisi " + longtime(lnRegHrs) + ;
			  " de temps régulier, qui est inferieur a votre minimum requis de " + ;
			  longtime(this.nMinRegHoursWeek) + ;
			  ". Comment combler cette difference ?"
endif

llOK = DoModalForm("Timetmp1Deficit", "", this)
if !nvl(llOK, .f.)
	select vTimetmp
	return .f.
endif

waitwind()

select vTimetmp

if !this.ValidateSheet_HourDeficit_Validate()
	select vTimetmp
	return .f.
endif

if !this.ValidateSheet_HourDeficit_CreateWithdrawals()
	select vTimetmp
	return .f.
endif

select vTimetmp
return

enddefine
*EOF######################################################

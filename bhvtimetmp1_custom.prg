*========================================================
*  Program...........: bhvTimeTmp1_Custom
*  Author............: Stefan Gabor 
*  Project...........: SFPQ 
*  Created...........: February 12, 2014
*  Code page.........: 1252 (WINDOWS)
*  Copyright.........: (c) Carver Technologies 2014
*  Description.......: Behavior for TimeTmp1 (Timesheet) form
*                    :
*                    : *** CUSTOM VERSION FOR SFPQ ***
*                    :
*  Technical Notes...: NOTE: This is a subclass of
*                    :       bhvTIMETMP1.
*                    :
*                    :
*  Major modifs......: February - 2014 
*                    : Use the Analyser object to calculate all  
*                    : timesheet details.
*                    : 
*
*  Modification......: August - 2014 
*                    : Remove the EXPENCES from totals 
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

nMaxExpense = 0.00
nEXPUnitRate = 0.0

*** Calendar 
lUseSidebar = .t.

*** timesheet posting 
lAllowPostBeforeAllSigned = .t.
lPostOnlyValidated = .t.

*-- Can employee sign without minimum hours?
lRequireMinHours = .t.

*-- How many periods in the future can employee go to?
nFuturePeriods = Null		&& Null = no restriction

*-- Time BANKIDs can deposit surplus in
cDepositSurplusIntoBankids = "6"
cBank6DepositOpt = ""
cBank9DepositOpt = ""

lShowPlanNames = .T. 
lShowDeficitDialogForSingleBank= .f.
lShowSurplusDialogForSingleBank= .f. 

*-- Time BANKIDs can withdraw deficit from
cWithdrawDeficitFromBankids = "69"		&& Bankids.  E.g. "13"

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
cPg2Tot5_Caption = "N/F"

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
local lcMaxExpense
*
dodefault(poThis)

store "" to lcMaxExpense 

lcMaxExpense = tbleval("PRIME", Optlist.TBLID, "TBLC7")
this.nMaxExpense = val(lcMaxExpense) 

if !(type("poThis.Parent.txtExpense")="O") 
	return 
endif 

this.nEXPUnitRate = this.nMaxExpense 
poThis.Parent.txtExpense.Value = this.nMaxExpense

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
alter table vNew add column TT__ATY M 
alter table vNew add column TT__LOC M

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

*** set step on 
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
		
			*** set step on 
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
	*** .Hours.LabelObj.visible = (!llAbsence and !tlExpenseAccount)
	.Hours.LabelObj.visible = (!llAbsence)

	*** Do not show TxtInOut for expenses   
	***.TxtInOut.visible = !tlExpenseAccount

	*** Show absences input 
	.BankHours.visible = (llAbsence and !tlExpenseAccount)
	.BankHours.LabelObj.visible = (llAbsence and !tlExpenseAccount) 

	*** Show expense account input 
	.txtEUnit.visible = tlExpenseAccount
	.txtEUnit.LabelObj.visible = tlExpenseAccount
	.txtExpense.visible = tlExpenseAccount
	.txtExpense.LabelObj.visible = tlExpenseAccount
	.txtActivity.visible = tlExpenseAccount
	.txtLocation.visible = tlExpenseAccount
	
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
.ColumnCount = 9
.RowHeight = 21

lcDynamicBackColor = ;
		"iif((!empty(vTimetmp.TT_SIGNBY) AND vTimetmp.TT__EURATE>0), " + ;
		"rgb(255,183,183), iif(vTimetmp.TT_EFFDT < thisform.oBhv.dFromDt, " + ;
		"rgb(232,232,255), iif((vTimetmp.TT_HOURS = 0), " + ;
		"rgb(241,250,220), iif(dow(vTimetmp.TT_EFFDT)%2=0," + ;
		"rgb(255,255,255), rgb(255,255,192)))))"

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

*** Add the EXPENSE UNIT column 
with .Column5
	.ControlSource = [vTimeTmp.TT__EUNIT]
	.Width = 60
	.Alignment = 1		&& Middle Right
	.Format = "@Z"
	.InputMask = "999.99"
	.DynamicBackcolor = lcDynamicBackColor
endwith

*** Add the EXPENSE UNIT column 
with .Column6
	.ControlSource = [vTimeTmp.TT__EURATE]
	.Width = 56
	.Alignment = 1		&& Middle Right
	.Format = "@Z"
	.InputMask = "999.99"
	.DynamicBackcolor = lcDynamicBackColor
endwith

with .Column7
	.Width = 165
	.ControlSource = [trim(left(vTimeTmp.TT__ATY,127))]
	.ReadOnly = .t.
	.DynamicBackcolor = lcDynamicBackColor
endwith

with .Column8
	.Width = 125
	.ControlSource = [trim(left(vTimeTmp.TT__LOC,127))]
	.ReadOnly = .t.
	.DynamicBackcolor = lcDynamicBackColor
endwith

with .Column9
	.Width = 85
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

*** Wider column range accept for approval 
.Column3.Width = 164
.Column4.Width = 240
.Column5.Width = 70
.Column6.Width = 55
.Column7.Width = 55
.Column8.Width = 55
.Column9.Width = 55

.Column10.Width = 55
.Column10.InputMask = iif(this.lShowMgrPageAsHHMM, "XXXXXX", "9999.99")

.Column11.Width = 55
.Column11.InputMask = iif(this.lShowMgrPageAsHHMM, "XXXXXX", "9999.99")

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
		 	"ROUND(SUM(TT__EURATE),4) TT__EURATE," + ;
			"MAX(left(TT__ATY,254)) as TT__ATY, " + ;
			"MAX(left(TT__LOC,254)) as TT__LOC"

			
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

* BA_TOT65 - expenses
* It should not be included in the totals.
replace all ;
		BA_Total with BA_TOT1+BA_TOT2+BA_TOT3+BA_TOT4 && +BA_TOT5

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

*** sum(TT__EUNIT*TT__EURATE) as TT_TOT5

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
		 sum(iif("5"$TT_TBLC8, ;
		 	(nvl(TT__EUNIT,0)*nvl(TT__EURATE,0)),0)+000.00) as TT_TOT5, ;
		 max(TT_MOTIF) as TT_MOTIF, ;
		 max(TT_ENTITY) as TT_ENTITY, ;
		 max(TT_JOBID) as TT_JOBID, ;
		 max(TT_STATIS) as TT_STATIS, ;
		 max(TT_PROJECT) as TT_PROJECT, ;
		 max(left(TT_NOTES,10)) as TT_NOTES, ;
		 max(left(TT_MSG1, 254)) as TT_MSG1, ;
		 MAX(left(TT__ATY,254)) as TT__ATY, ;
		 MAX(left(TT__LOC,254)) as TT__LOC ;
	from qBatch1 ;
	group by TT_PERSID ;
	into cursor qBatch2
set enginebehavior 80

return

*=========================================================
procedure GrdEmpl_DblClick(poThis)
local lnSelect, lnWkHours, ltDateTime  
local lcMaxExpense, lcComm1, lcComm2, lnReply   

store "" to lcMaxExpense
store 0 to lnReply 

if trim(gcLang) = "E" 
	lcComm1="The record has been signed and can no longer be modified!"  
	lcComm2="This record has been signed! Would you like to modify it?"
else
	lcComm1="L'enregistrement a été signé et ne peut plus être modifié!"
	lcComm2="Cet enregistrement a été déjà signé! Voulez-vous le modifier?" 
endif 

*** Allow only "S" to modify a sign expense record 
* in the timesheet 
if !empty(vTimetmp.TT_SIGNBY)
	if "M"$this.cRole  
		alert(name(this.PersId)+CRLF+lcComm1, ;
				0, "\!\?\<OK")
		return .f.			
	else 
		lnReply = alert(lcComm2,0, "\!\<Oui;\?\<Non")
		if lnReply <> 1
			return .f.
		endif 	
	endif 
endif 	

dodefault(poThis)

lnSelect = select()
*
with this

	*** Replace ASA the user double click so that 
	* TS sees that the current record is being modified
	ltDateTime = datetimex()
	replace next 1 TT_MODDT with ltDateTime in vTimetmp 
	replace next 1 TT_MODDT with ltDateTime in vNew 

	*** Unsign the expenses 
	if lnReply = 1 
		replace next 1 TT_SIGNBY 	with "" in vTimetmp 
		replace next 1 TT_SIGNDTM 	with {} in vTimetmp 
	endif 

	.nSchedHours = vTimetmp.TT_HOURS
	.cSchedHours = trim(vTimetmp.TT_INOUT)

	*** Get the maximum allowed 
	lcMaxExpense=tbleval("PRIME",trim(vTimetmp.TT_OPT),"TBLC7")
	.nMaxExpense = val(lcMaxExpense)
	.nEXPUnitRate= vNEW.TT__EURATE

	*** Refresh screen 
	.SetScreen(poThis,vTimetmp.TT_TYPE)

	*** Position the record in the cursor 
	if !empty(.cSchedHours)
		select (.csrBankHrs) 
		locate for trim(cInOut) = .cSchedHours and !eof()

		poThis.Parent.BankHours.DisplayValue = trim(cHours)
	endif

	poThis.Parent.txtExpense.Refresh()
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

.cWithdrawDeficitFromBankids= "9"
.cDepositSurplusIntoBankids = "9"

.cBank9DepositOpt = "DEP9"

do case 
*** H_JOBCAT = 'DIRCONTR'
case "NONSYN"$trim(qJobhist.H_UNION) ;
	and trim(qJobhist.H_JOBCAT) = "DIRCONTR"

	.cBank9WithdrawalOpt = "VARIA"

*** H_JOBCAT IN ('ELUEXECU','ELUREGIO')
case "SFPQ"$trim(qJobhist.H_UNION) ;
	and inlist(trim(qJobhist.H_JOBCAT),"ELUEXECU","ELUREGIO")

	.cBank9WithdrawalOpt = "VARIA"

*** SEB 
case "SEB"$trim(qJobhist.H_UNION)

	.cBank9WithdrawalOpt = "CFLEXI"

*** APMCP 
case "APMCP"$trim(qJobhist.H_UNION) 

	.cBank9WithdrawalOpt = "VARIA"

endcase 

.WriteLog("InitBankWithdrawal() = " + ;
		"cBank9WithdrawalOpt = " + ;
			transform(.cBank9WithdrawalOpt))

endwith 
return 

*=========================================================
procedure ValidateSheet_CheckOneDay(ldEffdt,lnCount,lnRegHrs)
*** We need to credit a number of hours for each day 
* at the signature. For example, an employee works 5.45 hours
* but he gets paid 7 hours. 
* We need to generate a transaction with an absence code
* for the difference in hours on daily basis. - S.G.
*
local lnSelect, lcAbsenceId, lnDeficitHours 
local llERROR, lnEntryMade 

store "" to lcAbsenceId
store 0 to lnDeficitHours, lnEntryMade

if !this.lSigned
	return dodefault(ldEffdt, lnCount, lnRegHrs)
endif

lnSelect = select()

*** Get the absence code from TBL
lcAbsenceId = tbleval("_SCHEDULE",qJobhist.H_SCHEDID,"TBLC7")
if empty(lcAbsenceId) 
	select (lnSelect)
	return dodefault(ldEffdt,lnCount,lnRegHrs)
endif 

*** Was the entry for the day already done?
select vTimetmp
count to lnEntryMade for ;
		TT_EFFDT = ldEffdt and TT_OPT = lcAbsenceId

*** Filter out the dates 
select TT_OPT, TT_EFFDT ;
from vTimetmp with (buffering=.t.) ;
where TT_EFFDT = ldEffdt and TT_OPT<>'**' ;
into cursor qTTmp 

*** Several validations 
llERROR = iif(!llERROR, (lnEntryMade > 0), .t.)
llERROR = iif(!llERROR, (reccount("qTTmp")<1), .t.)
llERROR = iif(!llERROR, (inlist(TT_OPT,lcAbsenceId)), .t.)
llERROR = iif(!llERROR, empty(qJobhist.H_SCHEDID), .t.)

if llERROR 
	this.WriteLog("ValidateSheet_CheckOneDay() - ldEffdt = "+;
			transform(ldEffdt))

	use in select("qTTmp")
	select (lnSelect)

	return dodefault(ldEffdt,lnCount,lnRegHrs)
endif 

*** set step on 
with this 

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

use in select("qTTmp")

select (lnSelect)
return 

*=========================================================
#define VALIDATION_METHODS 
*=========================================================
procedure cmdGo_Valid(poThis)
***
local lcTTOPT 
store "" to lcTTOPT 

if type("C_TTPREGOPT") != "U" 
	lcTTOPT = alltrim(C_TTPREGOPT)
endif 

if empty(lcTTOPT)
	lcTTOPT = "REGU"
endif 	

if dodefault(poThis)
	*** Refresh screen 
	this.SetScreen(poThis, lcTTOPT)
endif 

return 

*==========================================================
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

*======================================================================
procedure TxtExpense_valid(poThis)
*** 
local lcMessage 

if gcLang = "E"
	lcMessage = "The maximum value has been reached: " 
else
	lcMessage = "La valeur maximale a été atteinte: " 
endif

if !used("vNEW")
	return 
endif 	

if !(type("poThis.Parent.txtExpense")="O")
	return 
endif 

if vNEW.TT__EURATE > this.nMaxExpense
	alert(lcMessage+lstr(this.nMaxExpense,2),0,"\!\<Ok")

	poThis.Parent.txtExpense.Value = this.nMaxExpense
	poThis.Parent.txtExpense.Refresh()
endif

if dodefault()
	this.oThisform.Pgf1.Page1.cmdGo.EnableDisable()
	this.oThisform.Refresh()
endif 

return   

*======================================================================
procedure cmdExpense_valid(poThis)
*** Signs weekly expenses by updating the fields:
*		- TT_SIGNDTM with datetimex()
*		- TT_SIGNBY  with gcUser
*		
local llOk, loRS, lcComm1

if trim(gcLang) = "F" 
	lcComm1 = "Pour la période: "
	lcComm2 = " -dépenses ont été signés avec succès."  
else
	lcComm1 = "For the period: "
	lcComm2 = " -expenses have been successfully signed." 
endif 

llOk = .f. 
loRS = this.oBiz.PostWeeklyExpenses(this)	
if isnull(loRS)
	return llOk 
endif 
	
llOk = loRS.lOk 
if llOk 
	lcPayPeriod = dtoc(loRS.dFromDt)+" - "+dtoc(loRS.dThruDt)

	alert( lcComm1 + lcPayPeriod + chr(13) + ;
			lstr(loRS.nCount,0) + lcComm2, ;
			0, "\!\?\<OK")
	
	this.oThisform.release()
endif 

return llOk

*==========================================================
procedure PrintRec()
*** Timesheet reports 
*
local lcList, lcList2, lcRptAccess, lnRptNo, lnI  
local loBizRepolist 

loBizRepolist = null 
store "" to lcList, lcList2

local array laReportList[3]
laReportList[1] = 9001
laReportList[2] = 9002 
laReportList[3] = 9003 

dimension OPTIONS[4]
OPTIONS[1] = .t.
OPTIONS[2] = this.dFromDt
OPTIONS[3] = this.dThruDt
OPTIONS[4] = trim(this.cBatch)

*** set step on 
with this 

.odset.save()

if isnull(loBizRepolist)
	loBizRepolist = .oBizMgr.GetBiz("REPOLIST")
endif 

for lnI = 1 to alen(laReportList, 1)
	lnRptNo = laReportList[lnI]

	lcRptAccess = loBizRepolist.GetValue("RP_RPTNO="+;
			transform(lnRptNo), "RP_ACCESS")
			
	if lcRptAccess$gcBelongs or empty(lcRptAccess)
		do case 
		case lnI = 1
			lcList = lcList + lstr(lnRptNo,0) +" "
		case lnI = 2
			lcList2 = lcList2 + lstr(lnRptNo,0) +" "
		otherwise 
			lcList = lcList + lstr(lnRptNo,0) +" "
			lcList2 = lcList2 + lstr(lnRptNo,0) +" "
		endcase 
	endif 
next 

if .oThisform.pgf1.activepage = 1
    do quickprint with "/dest=prompt", lcList, ;
    		"between(TT_BATCH,'"+left(.cBatch, 9) + ;
    		"','"+left(.cBatch, 9)+"~')"+;
    		" and TT_PERSID="+transform(.PersId), ;
    		OPTIONS
else 
	do quickprint with "/dest=prompt", lcList2, ;
			"TT_BATCH='"+.cBatch+"'", OPTIONS
endif

endwith 

laReportList = null 
return 

*======================================================================
#define HELPER_METHODS 
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
procedure cmdMod_Valid(poThis)
*
if empty(vTimetmp.TT_SIGNBY)
	dodefault(poThis)
	return 
endif 
	
if !empty(vTimetmp.TT__EUNIT) ;
or !empty(vTimetmp.TT__EURATE)
	*** set step on 
	loGrdEmpl = findobj(this.oThisform, "GrdEmpl")
	this.GrdEmpl_DblClick(loGrdEmpl)
endif 

return

enddefine
*Eof######################################################

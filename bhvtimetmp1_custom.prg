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

*** Calendar 
lUseSidebar = .t.

*** timesheet posting 
lAllowPostBeforeAllSigned = .t.

*-- Page 2.
cPg2Tot1_Caption = "REG"
cPg2Tot2_Caption = "PRIME"
cPg2Tot3_Caption = "ABS"
cPg2Tot4_Caption = "TSUP"
cPg2Tot5_Caption = "C/D"

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
		(cHours C(5),cDescr C(30),nHours N(9,4),cInOut C(25))
endif 					

endwith 

return 

*=========================================================
procedure FillInScheduleList()
***
local loBizPlanDt,loBizTimetmp,lcWkHours,lnN,lcDays
local lcSchedType, lnSelect, ldEffdt
private loSched, loRndSchema

store null to loBizPlanDt, loRndSchema, loBizTimetmp
store 0 to lnWkHours, lnN
store "" to lcDays, lcInOut
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

*** set step on 
loBizTimetmp = this.oBizMgr.GetBiz("TIMETMP1")
if !isnull(loBizTimetmp)
	loRndSchema = loBizTimetmp.GetRoundingSchemaObject(loSched)
endif 

if reccount(.csrBankHrs) > 0 
	delete all in (.csrBankHrs)
	go top in (.csrBankHrs)
endif 	

for lnN = 1 to 4 
	store "" to lcDays, lcInOut
	store 0 to lnWkHours
	
*!*		if type("loRndSchema")="O"
*!*		   this.GetScheduleListRounded(@lcDays,@lcInOut,@lnWkHours,lnN)
*!*		else 	   
		
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
				lcDays = trim(loSched.StartHM)+"-"+trim(loSched.StartExHM)
			   lcInOut = trim(loSched.StartHM)+"-"+trim(loSched.StartExHM)
				lnWkHours = HMtoH(trim(loSched.StartExHM)) ;
						   	- HMtoH(trim(loSched.StartHM))
			endif 
		case lnN = 4
			if !empty(loSched.EndExHM)
				lcDays = trim(loSched.EndExHM)+"-"+trim(loSched.EndHM)
			   lcInOut = trim(loSched.EndExHM)+"-"+trim(loSched.EndHM)		  
				lnWkHours = HMtoH(trim(loSched.EndHM)) ;
						   	- HMtoH(trim(loSched.EndExHM))
			endif 
		endcase 
*!*		endif 

	if !empty(lcDays)
		lcWkHours = HtoHM(lnWkHours, -1)
		lcInOut = chrtran(lcInOut, ":", "")

		insert into (.csrBankHrs) (cHours, cDescr, nHours, cInOut);
			values (lcWkHours, lcDays, lnWkHours, lcInOut)
	endif 
next 

*** select (.csrBankHrs)
*** browse normal 

endwith

select(lnSelect)

store null to loBizPlanDt, loBizTimetmp
store null to loSched, loRndSchema

return

*=========================================================
procedure GetScheduleListRounded
lparameters tcDays, tcInOut, tnWkHours, tnN
*** Apply the rounding scheme to the schedule 
* In cases where we have to treat less than 1 full day 
* in the timesheet the rounding procedure can be used 
* adjust the schedule so that we get the exact amount 
* of hours in the timesheet.
*
*
*
local lnHMtoM
local lcStartHM, lcStartExHM, lcEndExHM, lcEndHM

store "" to lcStartHM, lcStartExHM, lcEndExHM, lcEndHM
store 0 to lnHMtoM 

with loRndSchema

*** Before the shift starts round up
lcStartHM = trim(loSched.StartHM)
if !empty(.nRndUpShiftStartMM)
	lnHMtoM = HMtoM(lcStartHM)
	lcStartHM = MtoHM(lnHMtoM+.nRndUpShiftStartMM,"/TIME")
endif 
*** After the shift starts round down
if !empty(.nRndDownShiftStartMM)
	lnHMtoM = HMtoM(lcStartHM)
	lcStartHM = MtoHM(lnHMtoM-.nRndDownShiftStartMM,"/TIME")
endif 

*** Before lunch starts round up
lcStartExHM = trim(loSched.StartExHM)
if !empty(.nRndUpLunchStartMM)
	lnHMtoM = HMtoM(lcStartExHM)
	lcStartExHM = MtoHM(lnHMtoM+.nRndUpLunchStartMM,"/TIME")
endif 
*** After lunch started round down
if !empty(.nRndDownLunchStartMM)
	lnHMtoM = HMtoM(lcStartExHM)
	lcStartExHM = MtoHM(lnHMtoM-.nRndUpLunchStartMM,"/TIME")
endif 

*** Before lunch ends round up
lcEndExHM = trim(loSched.EndExHM)
if !empty(.nRndUpLunchEndMM)
	lnHMtoM = HMtoM(lcEndExHM)
	lcEndExHM = MtoHM(lnHMtoM+.nRndUpLunchEndMM,"/TIME")
endif 
*** After lunch ends round down 
if !empty(.nRndDownLunchEndMM)
	lnHMtoM = HMtoM(lcEndExHM)
	lcEndExHM = MtoHM(lnHMtoM-.nRndDownLunchEndMM,"/TIME")
endif 
	
*** Before the shift ends round up
lcEndHM = trim(loSched.EndHM)
if !empty(.nRndUpShiftEndMM)
	lnHMtoM = HMtoM(lcEndHM)
	lcEndHM = MtoHM(lnHMtoM+.nRndUpShiftEndMM,"/TIME")
endif 
*** After the shift ends round down
if !empty(.nRndDownShiftEndMM)
	lnHMtoM = HMtoM(lcEndHM)
	lcEndHM = MtoHM(lnHMtoM-.nRndUpShiftEndMM,"/TIME")
endif 

*** Build the string and return
do case 
case tnN = 1
	if empty(loSched.Type)
		tcDays = iif(gcLang="E", "No schedule", "Pas de calendrier.")
		tnN = 5 
	else 
		tcDays = iif(gcLang="E", "(Unknown)", "(Aucun)")
	endif 
	tnWkHours = 0
case tnN = 2 
	tcDays = iif(gcLang="E", "Full day", "Journée complète") 
	tnWkHours = loSched.Hours
	if !empty(lcEndExHM) and !empty(lcEndExHM) 
		tcInOut = trim(lcStartHM)+"-"+trim(lcStartExHM)+;
		   	CRLF + trim(lcEndExHM)+"-"+trim(lcEndHM)
	else
		tcInOut = trim(lcStartHM)+"-"+trim(lcEndHM)
	endif
case tnN = 3
	if !empty(lcStartExHM)
		tcDays = trim(lcStartHM)+"-"+trim(lcStartExHM)
	   tcInOut = trim(lcStartHM)+"-"+trim(lcStartExHM)
		tnWkHours = HMtoH(trim(lcStartExHM)) ;
				   	- HMtoH(trim(lcStartHM))
	endif 
case tnN = 4
	if !empty(lcEndExHM)
		tcDays = trim(lcEndExHM)+"-"+trim(lcEndHM)
	   tcInOut = trim(lcEndExHM)+"-"+trim(lcEndHM)		  
		tnWkHours = HMtoH(trim(lcEndHM)) ;
				   	- HMtoH(trim(lcEndExHM))
	endif 
endcase 
	
endwith 

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
local lcTranxType, llExpenseAccount
*
with this 

lcTranxType = trim(tcTranxType)
if lcTranxType = "P"
	llExpenseAccount=ToLogical(tbleval("PRIME", Optlist.TBLID, "TBLC6"))
endif 
.RefreshScreen(poThis.Parent,lcTranxType,llExpenseAccount)

if llExpenseAccount
	if type("poThis.Parent.cboExpense") = "O"
		lcValue = trim(leftto(poThis.Parent.cboExpense.DisplayValue,"-"))
		poThis.Parent.cboExpense.Requery(lcValue)
	endif 
endif 

*** Is this guy job to refresh 
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
	.TxtInOut.visible = !tlExpenseAccount

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
	.Width = 86
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

*=========================================================
#define Page2_Stuff
*==========================================================
procedure GrdBatch_SetProperties(poThis)
*** Resize the column to match the totals 
dodefault(poThis)

with poThis

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
local lnSelect, lnWkHours  

dodefault()

lnSelect = select()
*
with this

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
procedure txtInOut_Valid_Custom(poThis, loAnalysis)
*** Called from txtInOUt_Valid. Return .f. to suppress 
*   standard alert message. You can still display your own
*
local llOk 
llOk = .t. 

*** Store the values after they have been rounded 
if !empty(loAnalysis.UsedHM)
	replace TT_PUNCH with trim(loAnalysis.ORIGDTM)
	replace TT_INOUT with trim(loAnalysis.UsedHM) 
endif 

return llOk 
endproc


enddefine

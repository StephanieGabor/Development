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

*=========================================================
procedure InitNewPers()
*** Fill the cursor schedule values 
*
if dodefault()
	this.FillInScheduleList()
endif 	

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
		(cHours C(5),cDescr C(30),nHours N(9,4),cInOut C(20))
endif 					

endwith 

return 

*=========================================================
procedure FillInScheduleList()
***
local loBizPlanDt, loSched, lcWkHours, lnN, lcDays
local lcSchedType, lnSelect

store null to loBizPlanDt, lcInOut
store 0 to lnWkHours, lnN
store "" to lcDays, lcInOut

with this

if empty(.PersId)
	return 
endif 

lnSelect = select()

loBizPlanDt = this.oBizmgr.GetBiz("PLANDT")
loSched = loBizPlandt.GetSched("/SCHED /OBJECT", ;
			 this.PersId, date(), date())

if reccount(.csrBankHrs) > 0 
	delete all in (.csrBankHrs)
	go top in (.csrBankHrs)
endif 	

for lnN = 1 to 4 
	store "" to lcDays, lcInOut
	lnWkHours = 0 

	do case
	case lnN = 1
		lcDays = iif(gcLang="E", "(Unknown)", "(Aucun)")
		lnWkHours = 0
	case lnN = 2
		lcDays = iif(gcLang="E", "Full day", "Journée complète") 
	   lnWkHours = loSched.Hours
	   lcInOut = trim(loSched.StartHM)+"-"+trim(loSched.StartExHM)+;
		   CRLF + trim(loSched.EndExHM)+"-"+trim(loSched.EndHM)		   
	case lnN = 3
		if !empty(loSched.StartExHM)
			lcDays = trim(loSched.StartHM)+"-"+trim(loSched.StartExHM)
			lnWkHours = hmtoh(trim(loSched.StartExHM)) ;
					   	- hmtoh(trim(loSched.StartHM))
		   lcInOut = trim(loSched.StartHM)+"-"+trim(loSched.StartExHM)
		endif 
	case lnN = 4
		if !empty(loSched.EndExHM)
			lcDays = trim(loSched.EndExHM)+"-"+trim(loSched.EndHM)
			lnWkHours = hmtoh(trim(loSched.EndHM)) ;
					   	- hmtoh(trim(loSched.EndExHM))
		   lcInOut = trim(loSched.EndExHM)+"-"+trim(loSched.EndHM)		   
		endif 
   endcase

	if !empty(lcDays)
		lcWkHours = HtoHM(lnWkHours, -1)

		insert into (.csrBankHrs) (cHours, cDescr, nHours, cInOut);
			values (lcWkHours, lcDays, lnWkHours, lcInOut)
	endif 
next 

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
	.Format = "R"
	.InputMask = "999.99"
	.DynamicBackcolor = lcDynamicBackColor
endwith

*** Add the EXPENSE UNIT column 
with .Column7
	.ControlSource = [vTimeTmp.TT__EURATE]
	.Width = 56
	.Alignment = 1		&& Middle Right
	.Format = "R"
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


enddefine

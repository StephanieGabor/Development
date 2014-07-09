*  Program...........: ShowTimsumSummary
*  Author............: Tom Green
*  Project...........: Human Resources
*  Created...........: October 31, 2006 (Halloween!)
*  Code page.........: 1252 (WINDOWS)
*  Copyright.........: (c) Carver Technologies 2006
*  Description.......: Shows a window for display only of
*                    : time bank balances.  Called from
*                    : bhvJOBHIST and possibly timesheet
*                    : 
*                    : NOTE that there is no security on
*                    : scTIMSUM.  If you don't want the user
*                    : to see his banks, don't call this prog.
*                    : 
*  Calling Samples...: *** This permits ShowTimsumSummary()
*                    : *   to be overridden in CUSTOM.APP
*                    : lcCommand = ;
*                    :     [ShowTimsumSummary("/MODAL",]+;
*                    :     transform(this.Persid) + [)]
*                    : CustomExecute(lcCommand)
*                    : 
*                    : 
*  Parameter List....: 1. Switches:
*                    :    /MODAL : Show as modal form
*                    :    /TIMETMP : Include unposted time
*                    :        from TIMETMP even if arg.3
*                    :        is not passed.
*                    :    /PROMPT : Ask if should show TIMETMP.
*                    : 
*                    :    NOTE: /TIMETMP implies planned absences
*                    :    as well
*                    : 
*                    : 2. Persid
*                    : 
*                    : 3. TIMETMP alias, view, or cursor
*                    :    If this is passed, unposted time
*                    :    is added in.  TT_DEST and TT_HOURS
*                    :    must exist.  If not passed, only
*                    :    TIMSUM info is used.
*                    : 
*                    : Return: void
*                    : 
*                    : 
*  Status, ToDo......: 
*                    : 
*                    : 
*  Technical Notes...: Uses frmBrowse, with columns defined in
*                    : UIINFO key "TIMSUM_SUMMARY"
*                    : 
*                    : Typically there is only one unposted
*                    : time sheet at a time.  We assume this
*                    : is the case without validation when
*                    : Arg.3 is passed
*                    : 
*                    : There is also the danger that TIMETMP
*                    : includes transactions from different
*                    : years.  However, typically a year will
*                    : not be rolled over until the timesheet
*                    : period is closed.
*                    : 
*  Major modifs......: 
*                    : 
*                    : 

parameters pcSwitches, lnPersid, poTimeTmp

*** Get cursor or banks
local loCsrTimsum, llPrompt, lnReply
local lnSelect
local lcCarryTxt, lcEarnTxt, lcTakenTxt, lcAccrueTxt, lcBalTxt
local lnBalMin, lnBalMax

private lcTimetmpAlias, lnHrsPdy, llModal, llTimetmp, llHasPlanned
private lcDaysAbbrev, lcHoursAbbrev, lcCurrency, lnPersid		&& Not local!
private loBizTimsum, loBizJobhist, ldPlannedFrom

pcSwitches = iif(vartype(pcSwitches)="C", upper(pcSwitches), "")
llModal = "/MODAL" $ pcSwitches
llTimetmp = "/TIMETMP" $ pcSwitches
llPrompt = "/PROMPT" $ pcSwitches
llHasPlanned = .f.

lnSelect = select()

if llPrompt
	if gcLang = "E"
		lnReply = alert( ;
				"Include unposted timesheet entries?", ;
				0, "\!\<Yes;\<No;\?\<Cancel")
	else
		lnReply = alert( ;
				"Inclure des entrées non-reportées de la feuille de temps ?", ;
				0, "\!\<Oui;\<Non;\?\<Annuler")
	endif
	if lnReply = 3
		return .f.
	endif
	llTimetmp = (lnReply = 1)
endif

waitwind()

loBizTimsum = GetBiz("TIMSUM")
loBizJobhist = GetBiz("JOBHIST")

*** Get PERS and current JOBHIST info into qPERS77
loCsrJobhist = loBizJobhist.GetPersAndJobhistByPersid (;
					"/cursor=qPers77 /CURRENT", lnPersid)

*** Load time bank balances into qTimsum77
loCsrTimsum = loBizTimsum.GetList("/cursor=qTimsum77", ;
					"", "TS_STAT=0 and TS_PERSID=" + ;
					transform(lnPersid), ;
					"TS_BANKID")

*** Add planned and display fields
select qTimsum77
alter table qTimsum77 ;
		add BankName C(30) ;
		add PlanName C(30) ;
		add CarryTxt C(40) ;
		add EarnTxt C(40) ;
		add TakenTxt C(40) ;
		add YearTxt C(40) ;
		add BalTxt C(40) ;
		add AccrueTxt C(40) ;
		add AccBalTxt C(40) ;
		add TS_PLAN N(10,2) ;
		add TS_PLANOK N(10,2) ;
		add TS_PLANPND N(10,2) ;
		add PlanTxt C(40) ;
		add PlanOKTxt C(40) ;
		add PlanPndTxt C(40) ;
		add AvailTxt C(40) ;
		add BalStat N(2) ;
		add Drill1 M ;
		add Drill2 M ;
		add Drill3 M

index on TS_BANKID tag TS_BANKID


*** Add in unposted time from time sheet
lcTimetmpAlias = GetAlias(poTimetmp)
if !empty(lcTimetmpAlias) or llTimetmp
	loBizTimsum.AccumulateTimetmp("", lnPersid, ;
				lcTimetmpAlias, "qTimsum77")
endif

lcDaysAbbrev = goMsgMgr.GetText("UI.COMMON.UNITS.DAYS_SHORT")
lcHoursAbbrev = goMsgMgr.GetText("UI.COMMON.UNITS.HOURS_SHORT")
lcCurrency = goMsgMgr.GetText("UI.COMMON.CURRENCY_SYMBOL")

lnHrsPdy = hrspdy(lnPERSID, date(), .t.)

*** Show planned absences starting on this date
ldPlannedFrom = monday(date(), C_WEEKSTRT) - 7

*** set step on 
*** In this program. Creates relation qTimsum77 -> qPlan77
if llTimetmp
	if ShowTimsumSummary_GetPlannedAbs()
		locate for TS_PLAN <> 0
		llHasPlanned = found()
	endif
endif

*** Convert numeric to text
scan
	replace ;
			BankName with tbleval("TIMEBANK", TS_BANKID), ;
			PlanName with tbleval("APLAN", TS_PLANID)

*!*		replace TS_Earn with TS_Earn + ExtractFromVirtualBank(TS_PlanId) 

	replace CarryTxt with ConvertTimsumUnits( ;
				"", TS_Units, TS_Carry, TS_CarryD, lnHrsPdy)

	replace EarnTxt with ConvertTimsumUnits( ;
				"", TS_Units, TS_Earn, TS_EarnD, lnHrsPdy)

	replace TakenTxt with ConvertTimsumUnits( ;
				"", TS_Units, TS_Taken, TS_TakenD, lnHrsPdy)

	replace AccrueTxt with ConvertTimsumUnits( ;
				"/Z", TS_Units, TS_Accrue, TS_AccrueD, lnHrsPdy)

	replace BalTxt with ConvertTimsumUnits( ;
				"", TS_Units, ;
				TS_Carry + TS_Earn - TS_Taken, ;
				TS_CarryD + TS_EarnD - TS_TakenD,;
				lnHrsPdy)

	*** Planned absences. Approved and Pending
	replace PlanTxt with ConvertTimsumUnits( ;
				"", "H", ;
				TS_Plan, 0, lnHrsPdy)
	replace PlanOKTxt with ConvertTimsumUnits( ;
				"", "H", ;
				TS_PlanOK, 0, lnHrsPdy)
	replace PlanPndTxt with ConvertTimsumUnits( ;
				"", "H", ;
				TS_PlanPnd, 0, lnHrsPdy)

	if TS_UNITS <> "$"
		replace AvailTxt with ConvertTimsumUnits( ;
				"", chrtran(TS_Units, "$", ""), ;
				TS_Carry + TS_Earn - TS_Taken - TS_Plan, ;
				0, lnHrsPdy)
	endif

	*** These are not displayed in default version, but
	*   are available for custom version.
	replace YearTxt with ConvertTimsumUnits( ;
				"/Z", TS_Units, ;
				TS_Earn - TS_Taken, ;
				TS_EarnD - TS_TakenD,;
				lnHrsPdy)

	replace AccBalTxt with ConvertTimsumUnits( ;
				"/Z", TS_Units, ;
				TS_Carry + TS_Accrue + TS_Earn - TS_Taken, ;
				TS_CarryD + TS_AccrueD + TS_EarnD - TS_TakenD,;
				lnHrsPdy)

	*** Check limits
	loLimits = loBizTImsum.GetLimits("", lnPersid, ;
					TS_Bankid, TS_ENDDT, ;
					"qTimsum77")

	lnBalMin = nvl(loLimits.nBalMin,-99999999) - .01
	lnBalMax = nvl(loLimits.nBalMax, 99999999) + .01

	replace BalStat with icase ( ;
				TS_CARRY+TS_EARN-TS_TAKEN < lnBalMin, -1, ;
				TS_CARRY+TS_EARN-TS_TAKEN > lnBalMax, 1, 0) 

	*** For some reason, slashes don't work. So switches are
	*   done with $-signes instead
	if TS_CARRY<>0 or TS_EARN<>0 or TS_TAKEN<>0
		if llTimetmp
			replace Drill1 with [<a href="show_timsum_trans('$TIMETMP',] + ;
					lstr(lnPersid) + ",'" + TS_BANKID + [')">]
		else
			replace Drill1 with [<a href="show_timsum_trans('',] + ;
					lstr(lnPersid) + ",'" + TS_BANKID + [')">]
		endif
	endif
	if TS_ACCRUE <> 0
		replace Drill2 with [<a href="show_timsum_trans('$ACCRUE',] + ;
			lstr(lnPersid) + ",'" + TS_BANKID + [')">]
	endif
		replace Drill3 with [<a href="show_timsum_trans('$PLAN',] + ;
			lstr(lnPersid) + ",'" + TS_BANKID + [')">]

endscan

waitwind("","/CLEAR")


*** Show the form
do ShowForm

*** Clean up
use in select("qTimsum77")
select (lnSelect)

return

*=========================================================
procedure ShowTimsumSummary_GetPlannedAbs

local loCsrPlanned, loBizPayno, ldPayPeriodEnds
local ldPlannedFrom, loBizEvent, lcBankid, lnBankid
local lnMinutes, lnHours, lnQty, ldYearEnd, lcDest

loBizPayno = GetBiz("PAYNO")
loBizEvent = GetBiz("EVENT")

ldPayPeriodEnds = loBizPayno.GetCurrentPayno(;
						qPers77.H_PAYGRP, date(), "PN_ENDDT")
ldPayPeriodEnds = evl(ldPayPeriodEnds, date())
ldPlannedFrom = AddDays(ldPayPeriodEnds,1)

*** Since we are getting records for all banks, we
*   can't specify the bank end date since it might vary.
*   All we know is it must be in the current year.
*   We will remove future years below.

**  Feb 24, 2012 - TCG. I restricted it to EVENT only because
*   records in TIMEDT are alsready in the balance -- I THINK
*   /// TODO - Check above assumption ////
loCsrPlanned = loBizTimsum.GetPlannedAbs(;
				"/cursor /event " + iif(llTimetmp, "/EXCLTIMETMP", ""), ;
				lnPersid, "", ldplannedFrom, ;
				gomonth(date(),13))

if isnull(loCsrPlanned)
	*** Some sort of error. Can't get planned absences
	select qTimsum77
	return .f.
endif

scan
	lcBankid = V_BANKID
	if empty(lcBankid)
		*** I don't think this should be possible
		suspendif(".t.")
		loop
	endif

	if !seek(lcBankid, "qTimsum77", "TS_BANKID")
		*** Has transaction but not bank!
		loop
	endif

	lnBankid = val(lcBankid)
	ldYearEnd = todate(qTimsum77.TS_ENDDT)

*** 2013.01.13 - Fixed to use value from cursor, which properly
*   excludes absences already posted
*-TG-		*** Only include time within the current bank year
*-TG-		lnMinutes = loBizEvent.Session_Duration(;
*-TG-						V_SESS, 0, ldPlannedFrom, ;
*-TG-						ldYearEnd)

	lnMinutes = V_HOURS * 60
	if substr(V_DEST,2) = "D"
		*** Deposited, change sign.  Unlikely.
		lnMinutes = - Minutes
	endif
	lnHours = round(lnMinutes / 60, 2)

	*** Accumulate into qTimsum77
	if V_PLAN = "A"
		*** Awaiting approval (pending)
		replace in qTimsum77 ;
				TS_PLANPND with TS_PLANPND + lnHours
	else
		*** Approved
		replace in qTimsum77 ;
				TS_PLANOK with TS_PLANOK + lnHours
	endif	
	replace in qTimsum77 ;
				TS_PLAN with TS_PLAN + lnHours
endscan

select qTimsum77
return

*=========================================================
procedure ConvertTimsumUnits (pcSwitches, pcUnits, ;
					pnQty, pnDollars, pnHrsPdy)
local lcResult, llBlankIfZero, lnQty
llBlankIfZero = "/Z" $ pcSwitches

lcResult = ""
pnQty = round(pnQty,2)

do case
case pnQty = 0
	if llBlankIfZero
		lcResult = ""
	else
		lcResult = iif(gcLang="E", "None", "Rien")
	endif

case "H" $ pcUnits
	*** Hours
	lcResult = ShowHrs(pnQty)
	if !empty(pnHrsPdy)
		lcResult = lcResult + " (Estimé " + ;
				lstr(pnQty/pnHrsPdy) + " jours)"
	endif

case "J" $ pcUnits or "D" $ pcUnits
	*** Days
	lcResult = lstr(pnQty) + " " + lcDaysAbbrev
	if !empty(pnHrsPdy)
		lcResult = lcResult + " (" + ;
		ShowHrs(pnQty * pnHrsPdy) + ")"
	endif

case pcUnits = "$"
	*** Dollars only. Use pnQTY argument
	lcResult = iif(empty(lcResult), "", ;
					lcResult + ", ") + ;
					lcCurrency + ltrim(str(pnQty,12,2))
endcase

if "$" $ pcUnits and pcUnits <> "$"
	*** Dollars AND days or hours
	if llBlankIfZero and pnDollars = 0 ;
	and (pcUnits = "$" or pnQty = 0)
		*** Dollars = 0 and nothing else in field
	else
		lcResult = iif(empty(lcResult), "", ;
					lcResult + ", ") + ;
					lcCurrency + ltrim(str(pnDollars,12,2))
	endif
endif

return lcResult

*=========================================================
procedure ShowHrs(pnHrs)

local lcHHMM

do case
case abs(pnHrs) = 1
	*** Exactly one hour or -1 hour. Use singular form
	return transform(pnHrs) + " " + ;
			 left(lcHoursAbbrev, len(lcHoursAbbrev)-1)
case int(pnHrs) = pnHrs
	*** Whole hours
	return transform(pnHrs) + " " + lcHoursAbbrev
case abs(pnHrs) < 1
	*** Minutes only
	return transform(round(pnHrs * 60,0)) + " min."
otherwise
	*** Show in hours and minutes
	return iif(C_TDHHMM, ;
		HtoHM(pnHrs, -1, "/DELIM"), lstr(pnHrs)) + ;
		" " + lcHoursAbbrev
endcase

*=========================================================
procedure ShowForm

local lcHTML

*** Prepare HTML
lcHTML = HtmlMerge("/FILE", "TIMSUM_SUMMARY_@.HTM", "qTimsum77")

*** Show the form
if llModal
	*-- Modal version.  Can't use drill-down
	StartHTMLForm( ;
			"/MODAL /ID=TIMSUM_SUMMARY /SAVEPOSITION", ;
			iif(gcLang="E", "Bank Balances", "Soldes des Banques") + ;
			" - " + name(lnPersid), ;
			lcHTML, null, lnPersid)
else
	*-- Non-modal version allows drill-down
	StartHTMLForm( ;
			"/ID=TIMSUM_SUMMARY /ALLOWMULTIPLE " + ;
			"/HEIGHT=550 /WIDTH=530 /SAVEPOSITION" + ;
			"/SUBTYPE=" + lstr(lnPersid), ;
			iif(gcLang="E", "Bank Balances", "Soldes des Banques") + ;
			" - " + name(lnPersid), ;
			lcHTML, null, lnPersid)
endif

return

*=========================================================
procedure ExtractFromVirtualBank(tcPlanId)
*** Get the number of hours from the virtual bank
*
local lcVirtualBankId, lnVirtualHRS  

store "" to lcVirtualBankId
store 0 to lnVirtualAMT 

if empty(tcPlanId)
	return 
endif 	

lcVirtualBankId = "V" + alltrim(tcPlanId)

*** Get signed only transaction 
lnVirtualHRS = val(tbleval("MISCBANK",lcVirtualBankId,"TBLC6"))

*** Get posted transaction 
lnVirtualHRS = lnVirtualHRS + ;
			val(tbleval("MISCBANK",lcVirtualBankId,"TBLC7"))
return lnVirtualHRS 


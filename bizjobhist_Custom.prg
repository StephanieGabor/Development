*  Program...........: bizJobhist_Custom
*  Author............: Stefan Gabor 
*  Project...........: Carver Technologies Inc. 
*  Created...........: June 20, 2014
*  Code page.........: 1252 (WINDOWS)
*  Copyright.........: (c) Carver Informatique Inc. 2014
*  Description.......: 
*                    : 
*                    : 
*                    : 
*  Classes...........: bizJobhist_Custom As as bizJobhist 
*                    : 
*                    : 
*                    : 

*##########################################################
 define CLASS bizJobhist_Custom ;
		 as bizJobhist of bizJobhist.prg
*##########################################################


*==========================================================
***	HOT FIX	*** STEFAN 
*==========================================================
procedure GetTotalHours(pcSwitches, ;
				pnPersid, pdFrom, pdThru)
*** Like the same method in bizSched, but received a persid.
*   Dates are REQUIRED. INcludes both start and end dates.

*** pcSwitches:
*		/HOURS (Default) or /FHOURS or /PDHOURS
*     /INCLINAC, /INCLLTD or /INCLALD
*   Switches are passed directly to bizSched.GetTotalHours().

local loCursor, ldDate, ldStart, lnN, lnSelect, lnDays, lnHours, lnTotal
local lcSchedid, llInclInac, llInclLtd

if empty(pdFrom) or empty(pdThru) or pdFrom > pdThru
	return 0
endif

pcSwitches = iif(vartype(pcSwitches)="C", upper(pcSwitches), "")
llInclInac = "/INCLINAC" $ pcSwitches
llInclLTD = "/INCLLTD" $ pcSwitches or "/INCLALD" $ pcSwitches

lnSelect = select()
loCursor = this.GetByPersid("/cursor", ;
				"H_PERSID, H_EFFDT, H_ENDDT, H_SCHEDID, H_JSTAT, H_HRSPER", ;
				pnPersid, ;
				pdFrom, pdThru, "H_EFFDT<>H_ENDDT")
go top
if eof()
	select (lnSelect)
	return 0
endif

*** Initialize loop
lnTotal = 0
ldStart = todate(pdFrom)
lnDays = todate(pdThru) - ldStart
for lnN = 0 to lnDays
	ldDate = ldStart + lnN
	if ldDate < H_EFFDT
		*** Before first record
		ldDate = todate(H_EFFDT)
		if ldDate > pdThru
			exit
		else
			*-- Skip directly to first date and continue
			lnN = todate(H_EFFDT) - ldStart
		endif
	endif
	if H_ENDDT <> {} and H_ENDDT <= lddate
		locate rest for H_ENDDT = {} or H_ENDDT > ldDate
		if eof()
			*** Should not happen.
			loop
		endif
	endif

	if H_JSTAT <> C_JSEMPL and ;
			not (H_JSTAT = "INAC" and llInclInac) and ;
			not (inlist(H_JSTAT,"LTD","ALD") and llInclLTD)
		*** Terminated or bad status
		if H_ENDDT = {}
			exit
		else
			loop
		endif
	endif

	*** Get Sched
	lcSchedid = trim(H_SCHEDID)
	if lcSchedid = ""
		*** For weekdays use H_HRSPER / 5
		*-- Could do much more efficiently, but normally this
		*   is only used for a small time interval
		lnHours = iif(between(dow(ldDate), 2, 6), H_HRSPER / 5, 0)
	else
		if isnull(this.oBizSched)
			this.oBizSched = this.oBizMgr.GetBiz("SCHED")
		endif
		lnHours = this.oBizSched.GetTotalHours(pcSwitches, ;
					lcSchedid, ldDate, ldDate)
	endif
	lnTotal = lnTotal + lnHours 
endfor

loCursor = null
select (lnSelect)
return round(lnTotal, 2)


*##########################################################
enddefine


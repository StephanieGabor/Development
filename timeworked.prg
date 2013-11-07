*  Program...........: TimeWorked
*  Author............: Marat Chariev
*  Project...........: Human Resources
*  Created...........: June 16, 2012
*  Code page.........: 1252 (WINDOWS)
*  Copyright.........: (c) Carver Technologies 2012
*  Description.......: Returns number of hours worked,
*                    :   according to JOBHIST schedule and
*                    :   while employee is EMPL status
*                    : Or, counts hours worked
*                    :   using the INCLUDE counter. Scheduled
*                    :   hours (JOBHIST hours) are ignored.
*							; MINUS hours in the EXCLUDE counter
*                    : 
*                    :   (The excluded counter is LWOP (sans-solde
*                    :   transactions, etc.)  Do NOT put overtime
*                    :   hours in that counter -- just UNPAID 
*                    :   transactions.)
*                    : 
*                    : DESIGNED for calculating VACATION 
*                    : and SICK entitlement
*                    : 
*                    : 
*  Calling Samples...: 
*                    : 
*  Parameter List....: 1. Option: TOT or HOURS (hours)
*                    :    Future: DAYS, CURSOR
*                    : 
*                    : 2. Employee ID
*                    : 
*                    : 3. Start Date
*                    : 
*                    : 4. End date
*                    : 
*                    : 5. Counter to INCLUDE. Used for
*                    :    ONCALL employees only.
*                    :    - You can leave this empty to have
*                    :      the system use JOBHIST schedule
*                    :      for all employees, including ON-CALL.
*                    : 
*                    : 6. Counter to EXCLUDE. Used for
*                    :    non-ONCALL employee. Put LWOP etc.
*                    : 
*                    : 7. TIMEDT alias. Optional
*                    : 
*                    : 8. Jobhist alias. Optionsl
*                    : 
*                    : Returns number or cursor.
*                    : 
*                    : 
*                    : 
*                    : 
*  Status, ToDo......: 
*                    : 
*                    : 
*  Technical Notes...: 
*                    : 
*                    : 
*  Major modifs......:  
*                    : 
*                    : 

*=========================================================

parameters pcOption, pnPersid, pdFrom, pdThru, ;
				pcINCLtcnt, pcEXCLtcnt, pcTimedtAlias, ;
				pcJobhistAlias


local loCsrJobhist, loBizJobist, ldDate, lcPstat
local lnSelect, lnHours, ldDate, lnHours1
local lcTCNTopt

pcOption = upper(alltrim(pcOption))
if !inlist(pcOption, "TOT", "HOURS", "DAYS")
	waitwind ("TimeWorked: Options supported: TOT, HOURS, DAYS")
	return null
endif

if type("llVALIDEXP")="L"
	return 0
endif

if empty(pdFrom) or empty(pdthru) or pdThru < pdFrom
	*** Error
	return 0
endif

lnSelect = select()
lcTCNTopt = iif(pcOption='DAYS', 'DAYS', "TOT")

if empty(pcJobhistAlias)
	loBizJobhist = Getbiz("JOBHIST")
	loCsrJobhist = loBizJobhist.GetByPersId ("/cursor", ;
					"H_EFFDT, H_ENDDT, H_SCHEDID, H_JSTAT, H_PSTAT", ;
					pnPersid, pdFrom, pdThru, ;
					"H_EFFDT<>H_ENDDT")
	pcJobhistAlias = alias()
else
	select (pcJobhistAlias)
endif

*** Get status as of today
ldDate = icase(pdFrom > date(), pdFrom, ;
					pdThru < date(), pdThru, date())

locate for H_EFFDT <= ldDate and (H_ENDDT={} or H_ENDDT>ldDate)

lcPSTAT = alltrim(&pcJobhistalias..H_PSTAT)

*** I'll need to create "Hours worked" counter for the
*	 follow PSTATs 
*-- MC	if inlist(lcPSTAT, "PERPAR", "AUXT", "AUXAOP") ;
*-- MC		and !empty(pcINCLtcnt)

*-- If counter is provided - use counter
if !empty(pcINCLtcnt)
	*** At DDO ALL employees enter ALL their hours in TIMEDT!
	lnHours = TCNTVAL(lcTcntOpt, pcINCLtcnt, pnPersid, ;
					 pdFrom, pdthru)
else
	*** PERM employees. These employees enter exceptions only.
	*** Look thru the days and check their schedules
	*** Or, count number of the hours employees
	*** suppuse to work between dates provided 
	*** according their schedules
	ldDate = todate(pdFrom)
	lnHours = 0
	do while ldDate <= pdThru
		*** In case schedule changed during the period.
		locate for H_EFFDT <= ldDate ;
				 and (H_ENDDT={} or H_ENDDT>ldDate)
		
		if !found() or H_JSTAT <> C_JSEMPL
			*** Not active employee
			ldDate = ldDate + 1
			loop
		endif
		
		if inlist(lcPSTAT, "PERPAR", "AUXT", "AUXAOP") ;
			and !empty(pcINCLtcnt)
			*** "On-call" at that time, and did not have a schedule
			ldDate = ldDate + 1
			loop
		endif

		lnHours1 = WKCAL("HOURS", H_SCHEDID, ldDate)
		lnHours = lnHours + iif(pcOption="DAYS", iif(lnHours1>0,1,0), lnHours1)
		ldDate = ldDate + 1
	enddo

	*** Now remove any unpaid leave (LWOP)
	if !empty(pcEXCLtcnt)
		lnHours1 = TCNTVAL(lcTcntOpt, pcEXCLtcnt, pnPersid, ;
					 pdFrom, pdthru)
		lnHours = max(0, lnHours - lnHours1)
	endif
endif

select (lnSelect)
return lnHours

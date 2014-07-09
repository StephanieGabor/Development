*  Program...........: bizTimsum_Custom
*  Author............: Tom Green
*  Project...........: Human Resource system
*  Created...........: June 25, 2002
*  Code page.........: 1252 (WINDOWS)
*  Copyright.........: (c) Carver Technologies Inc. 2006
*  Description.......: NOTE: In this vesion of time banks
*                    : module, the AP_EFFDT, and AP_ENDDT
*                    : are always the full year -- even
*                    : if employee starts mid-year, or
*                    : is terminated mid-year.
*                    : 
*                    : 
*                    : 
*  Classes...........: bizTIMSUM as BIZ
*                    : 
*                    : 
*                    : Modifié par Akli pour Adapter
*                    : les engins pour les banques 
*                    : de temp 
*                    : Méthodes ajoutées :
*                    : SetTimsum, Fix_End_TS_Dates
*							: AddRecTimsum
*#########################################################
 define class bizTimsum_Custom ;
 					as bizTimsum of bizTimsum.PRG
*#########################################################


*=========================================================
					**** HOT FIX	*****
*=========================================================
protected procedure GetPlannedAbs_Timedt(pcSwitches, ;
				lnPersid, pcBankid, pdFrom, pdThru, ;
				pnPlanned, pnPlannedD)

*/// TODO - Lots of combinations left to be tested

* Used for both totals and detail.
*   - If /TOTALS, returns pnPlanned and pnPlannedD
*   - If /CURSOR, returns a cursor with details
* plPlanned and pnPlannedD are OUTPUT parameters. 

*** This is very quick and dirty. It does NOT handle
*   ronding, multipliers, etc.  It is here mainly so that
*   ShowTimsumSummary() doesn't crash.

local lcWhere, loCursor, lnSelect, ldFuture, lcUnits

*** Select from TIMEDT starting with pdFrom, or tomorrow
*   if pdFrom is future.

ldFuture = max(date()+1, pdFrom)

if !empty(pdThru) and pdThru <= date()
	lcWhere = "1=2"
else
	lcWhere = "T_PERSID=" + lstr(lnPersid) + ;
				 " and T_EFFDT>=" + tochar(ldFuture, "/DTOS/QUOTES") + ;
				 iif(empty(pdthru), "", ;
				 " and T_EFFDT<=" + tochar(pdThru, "/DTOS/QUOTES"))
	if empty(pcBankid)
		lcWhere = lcWhere + " and inlist(T_DEST, '1P','1W'," + ;
				"'2P','2W','3P','3W','4P','4W','5P','5W'," + ;
				"'6p','6W','7P','7W','8P','8W','9P','9W')"
	else
		lcWhere = lcWhere + " and inlist(T_DEST, '" + pcBankid + "P','" + ;
					 pcBankid + "W')"
	endif
endif

if llTotals
	*** This will not work properly if pcBANKID is empty
	lcUNITS = iif(empty(pcBankid), "H", ;
				 TBLEVAL("TIMEBANK", pcBANKID, "TBLC1"))

	lnSelect = select()
	loCursor = goDataMgr.GetCursor("", ;
				  set("datasession"), "TIMEDT", ;
				  "sum(T_HOURS) as T_HOURS," + ;
				  "sum(T_AMT) as T_AMT", lcWhere)

	do case
	case lcUnits = "$"
		*** Dollars only
		pnPlanned = nvl(T_AMT, 0)
		pnPlannedD = nvl(T_AMT, 0)
	case left(lcUnits,1) = "H"
		*** Hours or Hours+Dollars
		pnPlanned = nvl(T_HOURS, 0)
		pnPlannedD = nvl(T_AMT, 0)
	otherwise
		*** Days or Days+Dollars
		pnPlanned = round(nvl(T_HOURS,0) / lnHrsPdy,4)
		pnPlannedD = nvl(T_AMT, 0)
	endcase
	loCursor = .t.
	select (lnSelect)
else
	loCursor = goDataMgr.GetCursor(pcSwitches, ;
				set("datasession"), "TIMEDT", ;
				"T_PERSID as V_PERSID, " + ;
				"substr(T_DEST,1,1) as V_BANKID, " + ;
				"T_DEST as V_DEST, " + ;
				"T_OPT as V_ETYPE, " + ;
				"T_EFFDT as V_EFFDT, " + ;
				"T_EFFDT as V_ENDDT, " + ;
				"T_HOURS as V_HOURS, " + ;
				"T_AMT as V_TOTAL, " + ;
				"T_NOTES as V_NOTES," + ;
				"T_UNIQID as V_UNIQID", ;
				lcWhere)
	alter table (alias()) ;
			add V_SESS M ;
			add V_UNITS C(2) ;
			add V_QTY N(10,2) ;
			add V_PLAN C(1)

	replace all V_PLAN with "P"
	replace all V_UNITS with tbleval("TIMEBANK", V_BANKID, "TBLC1")
	
	if V_HOURS != 0 and lnHrsPdy != 0
		replace all V_QTY with iif(left(V_UNITS,1) = "D", ;
					round(V_HOURS / lnHrsPdy, 2), val(V_UNITS))
	endif 

	browse normal 
endif

return loCursor

*#########################################################
enddefine


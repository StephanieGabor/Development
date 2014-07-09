*  Program...........: IRDATT01
*  Author............: Tom Green
*  Project...........: IRDA. custom 
*  Created...........: November 1, 2006
*  Code page.........: 1252 (WINDOWS)
*  Copyright.........: (c) Carver Technologies 2006
*  Description.......: Extraction from TIMETMP
*                    : for detail report OMHQTT01
*                    : 
*                    : (Copied from OMHQTT01)
*                    : 
*  Calling Samples...: Called as INIT from repolist
*                    : 
*  Parameter List....: None.
*                    : 
*                    : 
*                    : 

local loCsrTimeTmp, loBizJobhist, loCsrJobhist
local lcJHWhere, lcWhere, loBizPers
local lcBatch, lnPersid, lnEngineBehavior, lcString,lcString2 
lcString = ""
lcString2=""
lnEngineBehavior = set("EngineBehavior")
*** Exclude header transactions
lcSelect = Makefor("", lcSelect, "TT_PERSID<>0")

loCsrTimeTmp = null

if "PERS." $ upper(lcSelect) or "E_" $ upper(lcSelect)
	loCsrTimetmp = goDataMgr.GetCursor(;
			"/cursor=qTIMETMP /LeaveOnServer", ;
			set("Datasession"), ;
			"TIMETMP join PERS on TT_PERSID=E_PERSID", ;
			"timetmp.*, PERS.E_PAYGRP", ;
			lcSelect, ;
			"TT_BATCH, TT_PERSID, TT_EFFDT, TT_OPT,TT_DEST")

else
	loCsrTimetmp = goDataMgr.GetCursor(;
			"/cursor=qTIMETMP /LeaveOnServer", ;
			set("Datasession"), ;
			"TIMETMP", ;
			"*", ;
			lcSelect, ;
			"TT_BATCH, TT_PERSID, TT_EFFDT, TT_OPT,TT_DEST")
endif
alter TABLE qTimetmp add column tt_summary m 
if isnull(loCsrTimetmp) or type("TT_BATCH") <> "C"
	*** Error
	alert("Erreur lors de l'extraction")
	llError = .t.
	return .f.
endif

*** Delete header records unless only record for batch
index on TT_BATCH + str(tt_PERSID) tag SHOW
set order to SHOW descending
lcBatch = ""
lnPersid = -1
scan
	if TT_PERSID <> lnPersid or TT_Batch <> lcBatch
		*** New batch
		lcBatch = trim(TT_Batch)
		lnPersid = TT_Persid
	else
		*** Same batch
		if TT_TYPE = "*"
			*** Header record
			delete
		endif
	endif
endscan

set order to
go top
set enginebehavior 70 
SELECT tt_opt,;
		 tt_showopt, ;
		 sum(round(tt_hours*60,0)) as tt_minutes, ;
		 00000.0000 as tt_hours,tt_dest  ;
		FROM qTimetmp ;
		GROUP BY tt_opt ;
		order by TT_OPT ;
		INTO CURSOR qOptTOT readwrite where tt_opt <> "**"
replace all TT_HOURS with TT_MINUTES / 60
select qOptTOT 
scan
  if "P" $ qOptTOT.TT_DEST
  		lcString = lcString+alltrim(qOptTot.tt_showopt)+': '+alltrim(HtoHM(qOptTot.TT_hours, 10, "/DELIM"))+chr(13)+chr(10)
  endif
  if "D" $ qOptTOT.tt_dest
  		lcString2 = lcString2+"Mise en Banque "+alltrim(qOptTot.tt_showopt)+': '++alltrim(HtoHM(qOptTot.TT_hours, 10, "/DELIM"))+chr(13)+chr(10)
  endif
endscan
select sum(tt_hours) as tt_tothours from qOptTOT where "P" $ tt_dest into cursor tmppaid
lcString2 = lcString2+"Heures payées :" +alltrim(HtoHM(tt_tothours, 10, "/DELIM"))
select qTIMETMP
update qtimetmp set tt_summary = lcString+lcString2
go top 
AddRptVa("ldFrom", stod(left(TT_Batch,8)))
AddRptVa("ldThru", ldFrom + 13)



loBizJobhist = loSession.oBizMgr.GetBiz("JOBHIST")
loBizPers = loSession.oBizMgr.GetBiz("PERS")

lcJHWhere = "H_PERSID in (select TT_PERSID from ##qTimeTmp)"

*** Get JOBHIST records
loCsrJobhist = loBizJobhist.GetListByDate( ;
	  		   "/cursor=Jobhist/LEmpl", ;
	  		   "", ;
			   lcJHWhere, ;
			   ldFrom, ldThru)
index on H_PERSID tag H_PERSID
loDset.AddView("oJOBHIST", loCsrJOBHIST)

*** Get PERS records
loCsrPers = loBizPers.GetList( ;
				"/cursor=PERS", ;
				"", ;
				"E_PERSID in (select TT_PERSID from ##qTimeTmp)")
index on E_PERSID tag E_PERSID
loDset.AddView("oPERS", loCsrPERS)

*** Reopen qTimetmp again as TIMETMP
use in (select("TIMETMP"))
select 0
use (dbf("qTimetmp")) again alias TIMETMP

*** Relate qTIMETMP -> JOBHIST, TIMETMP, PERS
select qTimeTmp
set relation to TT_PERSID into JOBHIST, ;
					 recno() into Timetmp, ;
					 TT_PERSID into PERS

loDset.oView = loCsrTimetmp
return

*==========================================================
procedure VarList (paVarlist)
*** Specifies globals variables needed by the report.
*   These will be added to the FRX if displaying to screen.
external array paVarlist

do AddRow with paVarList, "ldFrom"
do AddRow with paVarList, "ldThru"

return

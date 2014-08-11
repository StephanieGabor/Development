*======================================================================
*  Program...........: T28 
*  Author............: Stefan Gabor 
*  Project...........: SFPQ 
*  Created...........: June 10, 2014
*  Code page.........: 1252 (WINDOWS)
*  Copyright.........: (c) Carver Technologies 2014
*  						:
*	Precondition		: The bank entitlement posting programs, 
*							: SFPQ in particular, take a snapshot of all 
* 							: parameters that are involved in calculating 
*							: a given bank entitlement and saves all parameters, 
*                    : through an object, in the AUDITE table. 
*							:
*  Description.......: Moreover, the program takes the entries from 
*							: the AUDITE table, parses the objects' properties 
*							: and data, and builds several cursors which are 
*							: ultimately used to present this information 
*							: into a dashboard to a user friendly format.
*                    :
*                    : *** CUSTOM VERSION FOR SFPQ ***
*                    :
*  Technical Notes...: NOTE: The AUDITE table containts RowId field.
*                    : ALTER TABLE audite ADD RowId int identity(1, 1)
*							:
*                    :
*  Major modifs......: NONE 
*                    : 
*                    : 
*                    : 
*							:
*======================================================================
***
*

*** Global variable are needed to interact with 
* base class "Grd" class refreshing methods.
public gnPTPersId, gnPTBankId, gdPostedDT, gnPostedBy

store 0 to gnPTPersId, gnPTBankId, gnPostedBy
store {} to gdPostedDT

if !GetTemporaryTables()
	return 
endif 	

*** Parsing object methods & main process 
=RunProcess()

*** set step on
select RowId, AU_KEY, AU_OLDUSER, ;
	AU_OLDDTM, AU_OLDPERS, AU_NEWPERS ;
from vTSAudit ;
into cursor qTSAudit 

select vTSAudit
index on RowId tag T0 

select qTSAudit
index on RowId tag T1 

select qObjFields
go top in qObjFields
index on RowId tag T2 

select qCNTH 
go top in qCNTH
index on RowId tag T3 

select qCNTA 
go top in qCNTA
index on RowId tag T4 

select qFMS 
go top in qFMS
index on RowId tag T5 

select qRMatrix
go top in qRMatrix
index on RowId tag T6 

select qPVTAudit
go top in qPVTAudit
index on RowId tag T7 

*** set step on 
select qTSAudit
set relation to RowId into vTSAudit additive  
set relation to RowId into qObjFields additive 
set relation to RowId into qCNTH additive 
set relation to RowId into qCNTA additive 
set relation to RowId into qFMS additive 
set relation to RowId into qRMatrix additive 
set relation to RowId into qPVTAudit additive 

do form IDashboard 

return 

*==========================================================
procedure GetTemporaryTables()
* Creates several cursors and opens AUDITE table. 
*
local lcSql, loCursor, llOk 
store "" to lcSql 
store null to loCursor

lcSql = GetSQLQuery()
if empty(lcSql)
	return 
endif 

*** set step on 
create cursor qObjFields ( RowId I, ;
									pName C(25), ;
									pSName C (50), ;
									pValue C(25) )

create cursor qPVTAudit (RowId I)
create cursor qRMatrix (RowId I) 

*** Pass trough SQL -- PRODUCTION 
loCursor = goDatamgr.SQLexec("/CURSOR=qTSAudit", ;
		set("datasession"), goDataMgr.ConnectionHandle, ;
		lcSql)

if isnull(loCursor)
	return llOk 
endif 

select * from qTSAudit ;
into cursor vTSAudit readwrite 

select vTSAudit
go top in vTSAudit

store null to loCursor
llOk = reccount("vTSAudit") > 0 
return llOk 

*=========================================================
procedure GetSQLQuery()
*** Procedure that builds the main cursor 
* with TIMETMP posted transactions. 
*
local lcSql, ldStartDT, ldEndDt 

store "" to lcSql
ldStartDT = date() - (365*2)
ldEndDt = date() + 15 

text to lcSql noshow pretext 1+4 textmerge 
	select RowId, AU_KEY, AU_OLDUSER, AU_OLDDTM, 
		AU_OLDPERS, AU_OLDVAL, AU_NEWVAL, AU_NEWPERS
	from  Audite
	where AU_DBF = 'TIMETMP' 
		and AU_ACTION = 'P'
		and AU_OLDDTM BETWEEN '<<ldStartDT>>' AND '<<ldEndDt>>'
	order by RowId
endtext 

return lcSql 
*==========================================================
procedure RunProcess()
*** Scans the AUDITE table and fills up 
* all temporary cursors.
*
local lcTable, lcMemo 
store "" to lcTable, lcMemo

wait window "Processing.Please wait ..." nowait 

*** set step on 
select vTSAudit
go top in vTSAudit
scan 
	lcMemo = alltrim(vTSAudit.AU_OLDVAL)
	if !empty(lcMemo)
		select vTSAudit
		for lnI = 1 to memlines(AU_OLDVAL)
			lcMemo = alltrim(mline(AU_OLDVAL, lnI))

			if left(lcMemo, 4) = "toR."
				lcTable = leftto(lcMemo, "=")
				lcTable = strtran(lcTable, "toR.", "")
			endif 	

			=CreatePropertyTable(lcMemo, vTSAudit.RowId)
			=FillInTable(lcMemo, vTSAudit.RowId, lcTable)
		next 	
	endif
	
	select vTSAudit
endscan

wait clear 

return 
endproc

*==========================================================
procedure CreatePropertyTable(tcMemoLine, tnRowId)
*** Process a single MEMO line and create several  
* cursors to hold the data by type 
*
local lcFieldName, lcFieldVal, lcObjStub, lnSelect  
store "" to lcFieldName, lcFieldVal
lcObjStub = "toR."

if empty(tcMemoLine)
	return 
endif 	

if !inlist(left(tcMemoLine, 4), lcObjStub)
	return 
endif 	

tcMemoLine = strtran(tcMemoLine, (lcObjStub), "")
lcFieldName = leftto(tcMemoLine, "=")
lcFieldVal = rightFrom(tcMemoLine, "=")

if empty(lcFieldName)
	return 
endif 	

lnSelect = select()

do case 
case inlist("CTABLE", lcFieldName) 
	=CreateFMSTable(lcFieldVal, tnRowId)
case inlist("CTSTCNT", lcFieldName) 
	=CreateCNTTable(lcFieldVal, tnRowId)
case inlist("CRULEMATRIX", lcFieldName) 
	=CreateRULEMATRIXTable(lcFieldVal, tnRowId)
otherwise 
	insert into qObjFields (RowId, pName, pSName, pValue) ;
		values (tnRowId, lcFieldName, "", lcFieldVal)

	=CreatePIVOTTable(lcFieldName, tnRowId)
endcase 

select(lnSelect)

return 
endproc

*==========================================================
procedure CreateFMSTable(tcFieldHeader, tnRowId)
*** Creates Full Month of Service (FMS) cursor 
* to hold the data
*
local lnSelect, lnI, lnHeaderCount, lcField
store 0 to lnI, lnHeaderCount 
store "" to lcField 

if empty(tcFieldHeader)
	return 
endif 	

if used("qFMS") and reccount("qFMS")>0 
	return 
endif 	

lnSelect = select()

=alines(aTBArray, tcFieldHeader,",")
lnHeaderCount = alen(aTBArray, 1)

if lnHeaderCount <= 0 
	return 
endif 	

create cursor qFMS (RowId I) 
for lnI = 1 to lnHeaderCount
	lcField = alltrim(aTBArray[lnI])

	alter table qFMS add &lcField C(15) 
next 

store null to aTBArray
select(lnSelect)

return 
endproc 

*==========================================================
procedure CreateCNTTable(tcFieldHeader, tnRowId)
*** Creates Counter (CNT) cursor 
* to hold the data
*
local lnSelect 

lnSelect = select()

if !used("qCNTH")
	create cursor qCNTH (RowId I, CodeId C(25))
endif 

if !used("qCNTA") 
	create cursor qCNTA (RowId I, CodeId C(25)) 
endif 	

select (lnSelect)

return 
endproc

*==========================================================
procedure CreateRULEMATRIXTable(tcFieldHeader, tnRowId)
*** Creates Union Rules Matrix (RULEMATRIX) cursor 
* to hold the data
*
local lnSelect, lnI, lnHeaderCount, lcField
store 0 to lnI, lnHeaderCount 
store "" to lcField 

if empty(tcFieldHeader)
	return 
endif 	

if used("qRMatrix") and reccount("qRMatrix")>0 
	return 
endif 	

lnSelect = select()

=alines(aTBArray, tcFieldHeader,",")
lnHeaderCount = alen(aTBArray, 1)

if lnHeaderCount <= 0 
	return 
endif 	

*** create cursor qRMatrix (RowId I) 
for lnI = 1 to lnHeaderCount
	lcField = alltrim(aTBArray[lnI])

	alter table qRMatrix add &lcField C(15) 
next 

store null to aTBArray
select(lnSelect)

return 
endproc 

*==========================================================
procedure CreatePIVOTTable(tcFieldName, tnRowId)
*** Creates the master data table, which contains all  
* objects' properties with their data plus each object 
* description for each property.
*
local lnSelect, lcFieldV, lcFieldD
store "" to lcFieldV, lcFieldD

if empty(tcFieldName)
	return 
endif 

lnSelect = select()

*** Remove the type of the object 
lcFieldV = substr(alltrim(tcFieldName),2,10)
lcFieldD = "_" + substr(lcFieldV,2,10) 
select qPVTAudit
if type(lcFieldV) != "C"
	alter table qPVTAudit add &lcFieldV C(25)
	alter table qPVTAudit add &lcFieldD C(75)
endif

select( lnSelect )

return 
endproc

*==========================================================
procedure FillInTable(tcMemoLine, tnRowId, tcProperty)
**** Fills in all cursor table with their respective  
* data from the data object.
*
local lnSelect 

if empty(tcMemoLine)
	return 
endif 

tcMemoLine = strtran(tcMemoLine, "toR.", "")
lcPropertyName = leftto(tcMemoLine, "=")
lcPropertyVal = rightFrom(tcMemoLine, "=")

lnSelect = select()

do case 
case inlist("CTABLE", tcProperty)
	if empty(lcPropertyVal)
		lcPropertyVal = tcMemoLine
	endif 
	return AppendToFMSTable(lcPropertyVal, tnRowId)
case inlist("CTSTCNT", lcPropertyName)
	return AppendToCNTTable(lcPropertyVal, tnRowId)
case inlist("CRULEMATRIX", tcProperty)
	lcPropertyVal = rightfrom(alltrim(vTSAudit.AU_NEWVAL),CRLF)
	return AppendToRULEMATRIXTable(lcPropertyVal, tnRowId)
otherwise 
	return AppendToPVTTable(lcPropertyName,lcPropertyVal,tnRowId)
endcase 
	
select(lnSelect)

return 
endproc

*==========================================================
procedure AppendToFMSTable(tcMemoLine, tnRowId)
*** Process a single MEMO line and fills qFMS cursor.  
*
local lnSelect, lnI, lnHeaderCount
local lcFieldName, lcFieldVal 

store 0 to lnI, lnHeaderCount 
store "" to lcFieldName, lcFieldVal

if empty(tcMemoLine)
	return 
endif 	

=alines(aTBArray, tcMemoLine,",")
lnHeaderCount = alen(aTBArray, 1)

*** set step on 
if lnHeaderCount <= 0 
	return 
endif 	

if !used("qFMS")
	return 
endif 

*** Header only - do not add the record 
if val(aTBArray[1]) = 0
	return 
endif 	

lnSelect = select()
	
select qFMS 
append blank in qFMS
replace next 1 RowId with tnRowId 

for lnI = 1 to lnHeaderCount
	lcFieldName = alltrim(field(lnI+1))
	lcFieldVal = alltrim(aTBArray[lnI])

	replace next 1 (lcFieldName) with (lcFieldVal)
next 

store null to aTBArray
select(lnSelect)

return 
endproc

*==========================================================
procedure AppendToCNTTable(tcFieldHeader,tnRowId)
*** Process a single MEMO line and fills qCNTA (absences),
* qCNTH (hours) cursors.
*
local lnSelect, lnI, lnHeaderCount, lcField
local lcRegHours, lcAbsences 

store 0 to lnI, lnHeaderCount 
store "" to lcField, lcRegHours, lcAbsences 

if empty(tcFieldHeader)
	return 
endif 	

*** Handle regular hours 
lcRegHours = extract(tcFieldHeader, ;
					"((TT_TYPE='H' and not (TT_OPT in (", ")")

if !empty(lcRegHours) and !used("qCNTH")
	return 
endif 	

lnSelect = select()

lcRegHours = chrtran(lcRegHours, "'","")
=alines(aTBArray, lcRegHours, ",")
for lnI = 1 to alen(aTBArray, 1)
	insert into qCNTH ( RowId, CodeId ) ;
		values (tnRowId, aTBArray[lnI])
next 
store null to aTBArray


*** Handle absences 
lcAbsences = extract(tcFieldHeader, ;
			"(TT_TYPE='A' and not (TT_OPT in (", ")")

if !empty(lcAbsences) and !used("qCNTA")
	return 
endif 
	
lcAbsences = chrtran(lcAbsences, "'","")
=alines(aTBArray, lcAbsences, ",")
for lnI = 1 to alen(aTBArray, 1)
	insert into qCNTA ( RowId, CodeId ) ;
		values (tnRowId, aTBArray[lnI])
next 

store null to aTBArray
select(lnSelect)

return 
endproc 

*==========================================================
procedure AppendToRULEMATRIXTable(tcMemoLine, tnRowId)
*** Process a single MEMO line and fills qRMATRIX cursor.
*
local lnSelect, lnI, lnK, lnHeaderCount
local lcMemoLine, lcFieldName, lcFieldVal 

store 0 to lnI, lnHeaderCount 
store "" to lcFieldName, lcFieldVal, lcMemoLine

if empty(tcMemoLine)
	return 
endif 	

if !used("qRMATRIX")
	return 
endif 

=alines(aTHArray, tcMemoLine, CRLF)
lnHeaderCount = alen(aTHArray, 1)

*** set step on 
if lnHeaderCount <= 0 
	return 
endif 	

lnSelect = select()

for lnK = 1 to alen(aTHArray, 1)
	lcMemoLine = alltrim(aTHArray[lnK])
	
	if empty(lcMemoLine)
		loop
	endif 	
		
	=alines(aTBArray, lcMemoLine, ",")
	lnHeaderCount = alen(aTBArray, 1)

	select qRMATRIX
	append blank in qRMATRIX
	replace next 1 RowId with tnRowId 

	for lnI = 1 to lnHeaderCount
		lcFieldName = alltrim(field(lnI+1))
		lcFieldVal = alltrim(aTBArray[lnI])

		replace next 1 (lcFieldName) with (lcFieldVal)
	next 

	store null to aTBArray
next 

store null to aTHArray
select(lnSelect)

return 
endproc

*==========================================================
procedure AppendToPVTTable
lparameters tcPropertyName, tcPropertyVal, tnRowId
*** Process a single MEMO line and fills qPVTAudit cursor.
*
local lnSelect, lcFieldName
store "" to lcFieldName

if empty(tnRowId)
	return 
endif 

if empty(tcPropertyName) or empty(tcPropertyVal)
	return 
endif 	

lnSelect = select()

*** set step on 
*** Remove the type of the object 
lcFieldName = substr(alltrim(tcPropertyName),2,10)
lcFieldDesc = "_" + substr(lcFieldName, 2, 10)

select qPVTAudit
locate for RowId = tnRowId and !eof()
if !found()
	append blank in qPVTAudit
endif 

if type(lcFieldName) = "C"
	replace next 1 RowId with tnRowId 
	replace next 1 &lcFieldName with tcPropertyVal
	replace next 1 &lcFieldDesc with ;
					GetObjLabelText(tcPropertyName)
endif 
	
select( lnSelect )
	
return
endproc


*==========================================================
procedure RefreshObject(toForm, tlVisible)
*** Refreshes the screen controls based on the type 
* of time bank selected.
*
with toForm

	*** Formula 
	.Lbl13.visible = tlVisible
	.Lbl14.visible = tlVisible

	*** Left square 	
	.Lbl8.visible = tlVisible
	.ShpBox13.visible = tlVisible

	.txtTSStartDT.visible = tlVisible
	.txtTSStartDT.LabelObj.visible = tlVisible
	.txtTSEndDT.visible = tlVisible 
	.txtTSEndDT.LabelObj.visible = tlVisible

	.txtTSWkDays.visible = tlVisible 
	.txtTSWkDays.LabelObj.visible = tlVisible
	.txtTSWkHrs.visible = tlVisible 
	.txtTSWkHrs.LabelObj.visible = tlVisible

	.ShpBox1.visible = tlVisible
	.ShpBox1.LabelObj.visible = tlVisible

	.ShpBox11.Height = iif(tlVisible, 123, 380)

	.Refresh()
endwith

return 
endproc

*==========================================================
procedure RefreshData(tcPersId,tcBankId,tcDate,tcPostedBy) 
*** Filters the data based on several parameters from UI,
* such as: PERSID, BANKID, POSTED DATE, POSTED BY USER ID.  
*
local lnSelect

if !used("qTSAudit")
	return 
endif 	

lnSelect = select()

gnPTPersId = val(tcPersId)
gnPTBankId = val(tcBankId)
gdPostedDT = ctod(tcDate)
gnPostedBy = val(tcPostedBy)

wait window "Refreshing. Please wait ..." nowait 

*** set step on 
select qTSAudit
go top in qTSAudit

do case 
*** 0-0-0-0
case empty(gnPTPersId) and empty(gnPTBankId) and ;
		empty(gdPostedDT) and empty(gnPostedBy)
	
	set filter to 

*** 1-0-0-0
case !empty(gnPTPersId) and empty(gnPTBankId) and ;
		empty(gdPostedDT) and empty(gnPostedBy)

	set filter to qTSAudit.AU_OLDPERS = gnPTPersId
	
*** 1-1-0-0
case !empty(gnPTPersId) and !empty(gnPTBankId) and ;
		empty(gdPostedDT) and empty(gnPostedBy)

	set filter to qTSAudit.AU_OLDPERS = gnPTPersId and ;
				qTSAudit.AU_NEWPERS = gnPTBankId

*** 1-1-1-0
case !empty(gnPTPersId) and !empty(gnPTBankId) and ; 
		!empty(gdPostedDT) and empty(gnPostedBy)

	set filter to qTSAudit.AU_OLDPERS = gnPTPersId and ;
				qTSAudit.AU_NEWPERS = gnPTBankId and ;
			 	ttod(qTSAudit.AU_OLDDTM) = gdPostedDT
*** 1-1-1-1
case !empty(gnPTPersId) and !empty(gnPTBankId) and ;
		!empty(gdPostedDT) and !empty(gnPostedBy)

	set filter to qTSAudit.AU_OLDPERS = gnPTPersId and ;
				qTSAudit.AU_NEWPERS = gnPTBankId and ;
			 	ttod(qTSAudit.AU_OLDDTM) = gdPostedDT and ;
			 	val(qTSAudit.AU_OLDUSER) = gnPostedBy

*** 0-1-1-1
case empty(gnPTPersId) and !empty(gnPTBankId) and ;
		!empty(gdPostedDT) and !empty(gnPostedBy)

	set filter to qTSAudit.AU_NEWPERS = gnPTBankId and ;
				ttod(qTSAudit.AU_OLDDTM) = gdPostedDT and ;
			 	val(qTSAudit.AU_OLDUSER) = gnPostedBy

*** 0-0-1-1
case empty(gnPTPersId) and empty(gnPTBankId) and ;
		!empty(gdPostedDT) and !empty(gnPostedBy)

	set filter to ttod(qTSAudit.AU_OLDDTM) >= gdPostedDT and ;
			 	val(qTSAudit.AU_OLDUSER) = gnPostedBy

*** 0-0-0-1
case empty(gnPTPersId) and empty(gnPTBankId) and ;
		empty(gdPostedDT) and !empty(gnPostedBy)

	set filter to val(qTSAudit.AU_OLDUSER) = gnPostedBy


*** 0-1-0-0
case empty(gnPTPersId) and !empty(gnPTBankId) and ;
		empty(gdPostedDT) and empty(gnPostedBy)

	set filter to qTSAudit.AU_NEWPERS = gnPTBankId 

*** 0-0-1-0
case empty(gnPTPersId) and empty(gnPTBankId) and ;
		!empty(gdPostedDT) and empty(gnPostedBy)

	set filter to ttod(qTSAudit.AU_OLDDTM) = gdPostedDT

*** 0-1-0-1
case empty(gnPTPersId) and !empty(gnPTBankId) and ;
	empty(gdPostedDT) and !empty(gnPostedBy)

	set filter to qTSAudit.AU_NEWPERS = gnPTBankId and ;
			ttod(qTSAudit.AU_OLDDTM) = gdPostedDT 

*** 1-0-1-0
case !empty(gnPTPersId) and empty(gnPTBankId) and ;
	!empty(gdPostedDT) and empty(gnPostedBy)

	set filter to qTSAudit.AU_OLDPERS = gnPTPersId and ;
			ttod(qTSAudit.AU_OLDDTM) = gdPostedDT 

*** 1-0-1-1
case !empty(gnPTPersId) and empty(gnPTBankId) and ;
	!empty(gdPostedDT) and empty(gnPostedBy)

	set filter to qTSAudit.AU_OLDPERS = gnPTPersId and ;
			ttod(qTSAudit.AU_OLDDTM) = gdPostedDT and ;
			val(qTSAudit.AU_OLDUSER) = gnPostedBy

*** 1-1-0-1
case !empty(gnPTPersId) and !empty(gnPTBankId) and ;
	empty(gdPostedDT) and !empty(gnPostedBy)

	set filter to qTSAudit.AU_OLDPERS = gnPTPersId and ;
			qTSAudit.AU_NEWPERS = gnPTBankId and ;
			ttod(qTSAudit.AU_OLDDTM) = gdPostedDT 

endcase 
go top in qTSAudit

select (lnSelect)

wait clear 

return 
endproc

*==========================================================
procedure GetObjLabelText( pName )
*** For now, hard code the object's property description 
* to be changed later -
*  
if !used("qObjFields")
	return 
endif 	

local lcObjDesc
store "" to lcObjDesc
	
do case 
case upper(pName) = "CBANKID" 
	lcObjDesc = iif(gcLang="F","Numéro de banque","Bank number")
case upper(pName) = "CBANKUNIT" 
	lcObjDesc = iif(gcLang="F","Unités","Units")
case upper(pName) = "CBATCHNO" 
	lcObjDesc = iif(gcLang="F","Numéro de lot","Batch number")
case upper(pName) = "CDEPOSITOPT" 
	lcObjDesc = iif(gcLang="F","Code de dépôt","Deposit Id")
case upper(pName) = "CPAYCYCLE" 
	lcObjDesc = iif(gcLang="F","Calendrier de paie","Pay calendar")
case upper(pName) = "CPAYGRP" 
	lcObjDesc = iif(gcLang="F","Groupe de paie","Pay group")
case upper(pName) = "CPLANID" 
	lcObjDesc = iif(gcLang="F","Code de plan","Plan Id")
case upper(pName) = "CSCHEDID" 
	lcObjDesc = iif(gcLang="F","Code d'horaire","Schedule Id")
case upper(pName) = "CTCNT" 
	lcObjDesc = iif(gcLang="F","Compteur","Counter")
case upper(pName) = "CUNIONID" 
	lcObjDesc = iif(gcLang="F","Syndicat","Union")
case upper(pName) = "DEFFDT" 
	lcObjDesc = iif(gcLang="F","Dernière mouvement","Last mouvement")
case inlist(upper(pName), "DENDDT", "DLYENDDT", "DPAYPERIODENDDT")
	lcObjDesc = iif(gcLang="F","Fin","End")
case inlist(upper(pName), "DSTARTDT", "DLYSTARTDT", "DPAYPERIODSTARTDT")
	lcObjDesc = iif(gcLang="F","Début","Start")
case inlist(upper(pName), "DTSSTARTDT")
	lcObjDesc = iif(gcLang="F","Début période courante",;
			"Start current period")
case inlist(upper(pName), "DTSENDDT")
	lcObjDesc = iif(gcLang="F","Fin période courante",;
			"End current period")
case upper(pName) = "DORIGINALHIREDDT" 
	lcObjDesc = iif(gcLang="F","Date d'embauche","Hired date")
case upper(pName) = "NCALCULATEDBANKDEPOSIT" 
	lcObjDesc = iif(gcLang="F", ;
			"Renflouement en excess calculé pour cette période", ;
			"Excess of entitlement calculated for this period")
case upper(pName) = "NENTADJDAYS" 
	lcObjDesc = iif(gcLang="F", ;
			"Nombre des jours pour cette période", ;
			"Number of days for this period")
case upper(pName) = "NENTADJHH" 
	lcObjDesc = iif(gcLang="F", ;
			"Nombre des heures pour cette période", ;
			"Number of hours for this period")
case upper(pName) = "NENTITLEDAYS" 
	lcObjDesc = iif(gcLang="F", "Nombre des jours", "Number of days")
case upper(pName) = "NENTITLEDPY" 
	lcObjDesc = iif(gcLang="F", ;
			"N - Nombre total de jours maladie/vacances autorisés par an ", ;
			"N - Total number of days of sick / vacation allowed per year ")
case upper(pName) = "NENTITLEHH" 
	lcObjDesc = iif(gcLang="F", "Nombre des heures", "Number of hours")
case upper(pName) = "NFULLMONTHOFSERVICE" 
	lcObjDesc = iif(gcLang="F", "Mois complets de service", ;
			"Full months of service")
case upper(pName) = "NHRSPDY" 
	lcObjDesc = iif(gcLang="F","Moyen heures/jour","Average hours/day")
case upper(pName) = "NMAXDAYSWORKED" 
	lcObjDesc = iif(gcLang="F","Nombre maximal de jours travaillés",;
			"Maximum number of days worked")
case upper(pName) = "NMTDSCHEDTOWORK" 
	lcObjDesc = iif(gcLang="F","Mois à ce jour prévue à travailler",;
			"Month to date scheduled to work")
case upper(pName) = "NPERSID" 
	lcObjDesc = iif(gcLang="F","Numéro d'employé","Employe Id")
case upper(pName) = "NPLANBALMAX" 
	lcObjDesc = "Maximum"
case upper(pName) = "NPLANBALMIN" 
	lcObjDesc = "Minimum"
case upper(pName) = "NREGULARBANKDEPOSIT" 
	lcObjDesc = iif(gcLang="F", "Dépôt régulier", "Regular deposit")
case upper(pName) = "CCUSTOMBANKDEPOSIT" 
	lcObjDesc = iif(gcLang="F", "Formule de dépôt", "Deposit formula")
case upper(pName) = "NSCHEDDAYSWKBYPAYPERIOD" 
	lcObjDesc = iif(gcLang="F","Jours prévus pour trav.", ;
			"Days planned to work")
case upper(pName) = "NSCHEDHOURSWKBYPAYPERIOD" 
	lcObjDesc = iif(gcLang="F","Heures prévus pour trav.", ;
			"Hours planned to work")
case upper(pName) = "NSCHEDHOLIDAYS" 
	lcObjDesc = iif(gcLang="F","Nombre de jours féries", ;
			"Number of public holidays")
case upper(pName) = "NSENMTHS" 
	lcObjDesc = iif(gcLang="F","Nombre de mois","Number of months")
case upper(pName) = "NSENYEARS" 
	lcObjDesc = iif(gcLang="F","Nombre d'années","Number of years")
case upper(pName) = "NTOTDAYSWORKED" 
	lcObjDesc = iif(gcLang="F", ;
			"Nombre de jours travaillés", "Number of days worked")
case upper(pName) = "NTOTHOURSWORKED" 
	lcObjDesc = iif(gcLang="F", ;
			"Nombre des heures travaillées", "Number of hours worked")
case upper(pName) = "NTOTSICKDAYS" 
	lcObjDesc = iif(gcLang="F", ;
			"Renflouement selon les jours travaillés", ;
			"Entitlement based on number of days worked")
case upper(pName) = "NTOTSICKHH" 
	lcObjDesc = iif(gcLang="F", ;
			"Renflouement selon les heures travaillés", ;
			"Entitlement based on number of hours worked")
case upper(pName) = "NTSWORKEDDAYS" 
	lcObjDesc = iif(gcLang="F", ;
			"Nombre des jours inclus ","Number of days included")
case upper(pName) = "NTSWORKEDHH" 
	lcObjDesc = iif(gcLang="F", ;
			"Nombre des heures inclus ","Number of hours included ")
case upper(pName) = "NWORKDPY" 
	lcObjDesc = iif(gcLang="F", ;
			"T - Nombre total de jours de travail par an ", ;
			"T - Total number of working days per year ")
case upper(pName) = "NYTDBANKDEPOSIT" 
	lcObjDesc = iif(gcLang="F", ;
			"Renflouement déposé dans la banque jusqu'à date","Entitlement deposited in bank to date")
case upper(pName) = "NYTDSICKDAYS" 
	lcObjDesc = iif(gcLang="F", ;
			"Année à ce jour les jours de maladie","YTD sick days")
case upper(pName) = "NYTDSICKHH" 
	lcObjDesc = iif(gcLang="F", ;
			"Année à ce jour les heures de maladie","YTD sick hours")
case upper(pName) = "NAPTOTVACHH" 
	lcObjDesc = iif(gcLang="F","Nombre d'heures accumulées (estimation)","Number of hours accumulated (estimation)")
case upper(pName) = "NAPTOTVACDAYS" 
	lcObjDesc = iif(gcLang="F","Nombre de jours accumulées","Number of days accumulated")
case upper(pName) = "NGTTOTVACHH" 
	lcObjDesc = iif(gcLang="F","Nombre d'heures accumulées (estimation)","Number of hours accumulated (estimation)")
case upper(pName) = "NGTTOTVACDAYS" 
	lcObjDesc = iif(gcLang="F","Nombre de jours accumulées","Number of days accumulated")
case upper(pName) = "CPAYNO" 
	lcObjDesc = iif(gcLang="F","Période de paie (Carver)","Pay period (Carver)")
case upper(pName) = "NSFPQPAYNO" 
	lcObjDesc = iif(gcLang="F","Estimation de la période de paie (SFPQ)","Estimation of pay period (SFPQ)")
endcase 

return lcObjDesc 


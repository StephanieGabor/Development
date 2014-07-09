*==========================================================
*  Program...........: InitT20 
*  Author............: Stefan G.
*  Project...........: Zoo62
*  Created...........: January 10, 2014
*  Code page.........: 1252 (WINDOWS)
*  Copyright.........: (c) Tom Green 2014
*  Description.......: Init program for PERS reports.
*                    : 3-tier version
*                    : 
*                    : 
*                    : 
*  Calling Samples...: 
*                    : 
*  Parameter List....: None
*                    : OPTIONS[1] : always .t.
*                    : OPTIONS[2] : Permanent status only (logical)
*                    : OPTIONS[3] : AsOf date
*                    : 
*                    : 
*                    : 
*                    :
*==========================================================
private llDEBUG 

local lcTBLTYPE, lcEXPENSE, lcPERSID, lcPRIMEID, lcSql
local lcPERSID, lcEXPENSE, lcWhere_PERSID, lcWhere_PRIME
local llcWhere_BATCH, ldFrom, ldThru
local loCursor

store "" to lcTBLTYPE, lcEXPENSE, lcPERSID, lcPRIMEID, lcSql
store "" to lcPERSID, lcEXPENSE, lcWhere_PERSID, lcWhere_PRIME
store "" to lcWhere_BATCH
store {} to ldFrom, ldThru
store null to loCursor

llDEBUG = .t. 				&& Save the cursors to TEMP directory  

therm(.02)

*** set step on 
*** Default settings for expenses into TBL
lcTBLTYPE= "PRIME"
lcEXPENSE= "OUI"

if type("OPTIONS[1]") <> "L"
	dimension OPTIONS[3]
	OPTIONS[2] = date()-6
	OPTIONS[3] = date()
endif

*** Create report variables
do AddRptVa with "ldAsOf", ;
	iif(type("OPTIONS[3]")="D" and !empty(OPTIONS[3]), ;
		OPTIONS[3], date())
lcAsof = tochar(ldAsOf, "/quotes")

do AddRptVa with "ldFrom", OPTIONS[2]
do AddRptVa with "ldThru", OPTIONS[3]

*** Get the PERS ID's from selection  
if (atw("inlist(pers.E_PERSID",lcSELECT) > 0)
	lcPERSID = EXTRACT(lcSELECT,"inlist(pers.E_PERSID,", ")")

	if !empty(lcPERSID)
		lcWhere_PERSID = "AND timetmp.TT_PERSID IN (" + lcPERSID + ")"
	endif 
endif 

*** Called from timesheet 
if empty(lcPERSID)
	if (atw("TT_PERSID=",lcSELECT) > 0)
		lcPERSID = EXTRACT(lcSELECT,"and TT_PERSID=", "")

		if !empty(lcPERSID)
			lcWhere_PERSID = "AND timetmp.TT_PERSID IN (" + lcPERSID + ")"
		endif 
	endif 
endif 

*** Get the PRIME ID's from selection  
if (atw("inlist(timetmp.TT_OPT",lcSELECT) > 0)
	lcPRIMEID = EXTRACT(lcSELECT,"inlist(timetmp.TT_OPT,", ")")

	if !empty(lcPRIMEID)
		lcWhere_PRIME = "AND timetmp.TT_OPT IN (" + lcPRIMEID + ")"
	endif 
endif 

*** Get the BATCH from selection  
if type("OPTIONS[4]") = "C" ;
and !empty(OPTIONS[4])
	lcWhere_BATCH = "AND timetmp.TT_BATCH IN ('" + OPTIONS[4] + "')"
endif 

*** Get selection date
* Not using "tochar()" since it keeps the "{}" 
if type("OPTIONS[2]") = "D" and !empty(OPTIONS[2])
	ldFrom = OPTIONS[2]
endif 

if type("OPTIONS[3]") = "D" and !empty(OPTIONS[3])
	ldThru = OPTIONS[3]
endif 

*** Get current pay period date range 
if empty(ldFrom) and empty(ldThru)
	=GetPayPeriod(@ldFrom, @ldThru)
endif 
	
lcSql = GetPIVOTQuery()
if empty(lcSql)
	return 
endif 	

*** set step on 
lcSql = strtran(lcSql, [PERSID_FILTER], lcWhere_PERSID)
lcSql = strtran(lcSql, [PRIME_FILTER], lcWhere_PRIME)
lcSql = strtran(lcSql, [BATCH_FILTER], lcWhere_BATCH)
lcSql = strtran(lcSql, [FROMDT_FILTER], ;
			iif(type("ldFrom")="D",dtoc(ldFrom),ldFrom))
lcSql = strtran(lcSql, [THRUDT_FILTER], ;
			iif(type("ldThru")="D",dtoc(ldThru),ldThru))

*** Pass trough SQL 
loCursor = goDatamgr.SQLexec("/CURSOR=vTmpTBL", ;
		set("datasession"), goDataMgr.ConnectionHandle, ;
		lcSql)

if isnull(loCursor)
	return 
endif 
	
select * from vTmpTBL ;
into cursor qTmpTBL readwrite 
use in select("vTmpTBL")
	
*** set step on 
select qTmpTBL
go top in qTmpTBL 
index on str(TT_PERSID,8)+dtoc(TT_EFFDT)+trim(TT_INOUT) tag TTBL1

=CreatePIVOTTable(4, field(4))
=FillPIVOTTable(field(4),field(5),field(6),field(3),field(7),field(8))
=ComputeTotals()
=FillInTime()
=SpoolOut()

select csrTmpTBL
set relation to str(TT_PERSID,8)+dtoc(TT_EFFDT)+trim(TT_INOUT) into qTmpTBL 

store null to loCursor
return 

*=========================================================
procedure GetPIVOTQuery()
*** Procedure that builds the main PIVOT cursor 
* based on TIMETMP table and TBL.
*
local lcSqlQuery
store "" to lcSqlQuery

text to lcSqlQuery textmerge noshow pretext 1+4 
	--- Build temporary table to PIVOT data  
	BEGIN
		select a.TT_PERSID, a.TT_EFFDT, a.TT_INOUT,
			a.TT_OPT, a.TT__EURATE, 
			SUM(a.TT__EUNIT) as TT__EUNIT,
			MAX(a.TT__ATY) as TT__ATY,
			MAX(a.TT__LOC) as TT__LOC,
			MAX(a.E_LNAME) as E_LNAME, 
			MAX(a.E_FNAME) as E_FNAME, 
			MAX(a.E_SIN) as E_SIN, 
			MAX(a.E_ADDR1) as E_ADDR1, 
			MAX(a.E_ADDR2) as E_ADDR2, 
			MAX(a.E_CITY) as E_CITY,
			MAX(a.E_PROV) as E_PROV, 
			MAX(a.E_POSTCD) as E_POSTCD, 
			MAX(a.E_COUNTRY) as E_COUNTRY, 
			MAX(a.E_UNION) as E_UNION, 
			MAX(a.E_JOBID) as E_JOBID, 
			MAX(a.J_JOBTITLF) as J_JOBTITLF
		from 
		(
			SELECT timetmp.TT_PERSID, 
				pers.E_LNAME, pers.E_FNAME, pers.E_SIN,
				pers.E_ADDR1, pers.E_ADDR2, pers.E_CITY,
				pers.E_PROV, pers.E_POSTCD, pers.E_COUNTRY,
				pers.E_UNION, pers.E_JOBID, job.J_JOBTITLF,
				timetmp.TT_EFFDT, 
				[TT_INOUT]=cast(ltrim(rtrim(timetmp.TT_INOUT)) as varchar(25)),
				timetmp.TT_UNIQID, timetmp.TT_OPT, 
				timetmp.TT__EUNIT, timetmp.TT__EURATE,
				CASE WHEN (tbl.tblTYPE='PRIME' AND tbl.TBLC6 IN('OUI','YES')) 
					THEN CAST(1 AS NUMERIC(3,0)) 
					ELSE CAST(0 AS NUMERIC(3,0)) 
					END AS IsExpense,  
				RTRIM(timetmp.TT__ATY) as TT__ATY,
				RTRIM(timetmp.TT__LOC) as TT__LOC
			FROM tbl
			INNER JOIN timetmp ON tbl.TBLID = timetmp.TT_OPT
			RIGHT OUTER JOIN PERS ON timetmp.TT_PERSID = PERS.E_PERSID
			INNER JOIN job ON pers.E_JOBID = job.J_JOBID
			WHERE (tbl.tblTYPE='PRIME' AND tbl.TBLC6 IN('OUI','YES'))
				AND timetmp.TT_POSTBY = ''
				PERSID_FILTER
				BATCH_FILTER
				PRIME_FILTER
		) a 
			where a.TT_EFFDT >= 'FROMDT_FILTER' 
				and a.TT_EFFDT <= 'THRUDT_FILTER'
		GROUP BY a.TT_PERSID, a.TT_EFFDT, a.TT_INOUT, a.TT_OPT, a.TT__EURATE
	END
endtext 

return lcSqlQuery

*=========================================================
procedure CreatePIVOTTable(tnKeyNo, tcOPT)
*** Create a new cursor that will be used as PIVOT table
* Three new fields have are added to the new cursor 
* for each expense type:
*		cID 1..25 = Expense ID 
*		Rate 1..25 = Rate for each expense type 
*		Unit 1..25 = Price unit for each expense type 
* Now, all these fields will be filled with the values 
* from TT__EUNIT and TT__EURATE (2 custom fields) into  
* a linear format instead of tabular.
*
* PARMETERS: 
* 		tnKeyNo: How many PK are needed for the report. In 
*					this case, TT_PERSID & TT_EFFDT. 
*		tcOPT: Expense type for which we need create the 
*            3 new fields (Id1..25, Rate1..25, Unit1..25)
*
local lnSelect, lnI, lnJ, lcField   
local lcIdField, lcRateField, lcUnitField
local lcInOutField, lcTotField, lcActField, lcLocField

store "" to lcField, lcInOutField, lcTotField
store "" to lcIdField, lcRateField, lcUnitField 
store "" to lcActField, lcLocField

store 0 to lnI, lnJ

lnSelect = select()

select qTmpTBL
copy structure to csrTmpTBL 
use csrTmpTBL in 0 exclusive 
index on str(TT_PERSID,8)+dtoc(TT_EFFDT) tag TTBL2 

*** Keep only the key columns (TT_PERSID, TT_EFFDT)
* into the new cursor. 
select qTmpTBL
for lnI = tnKeyNo to fcount()
	lcField = trim(field(lnI))
	alter table csrTmpTBL drop column &lcField
next

*** Create 25 columns for each expense type  
* 1st = Id, 2nd = Unit 
for lnJ = 1 to 25 
	lcIdField = "Id" + lstr(lnJ)
	lcRateField = "Rate" + lstr(lnJ)
	lcUnitField = "Unit" + lstr(lnJ)
	lcInOutField= "InOut" + lstr(lnJ)
	lcTotField = "Tot" + lstr(lnJ)
	lcActField = "Activ" + lstr(lnJ)
	lcLocField = "Locat" + lstr(lnJ)

	alter table csrTmpTBL add column &lcIdField C(25)
	alter table csrTmpTBL add column &lcRateField N(12,4)
	alter table csrTmpTBL add column &lcUnitField N(12,4)
	alter table csrTmpTBL add column &lcInOutField C(50)
	alter table csrTmpTBL add column &lcTotField N(12,4)
	alter table csrTmpTBL add column &lcActField M 
	alter table csrTmpTBL add column &lcLocField M 
next 

*** Create headers place holders for the report 
alter table csrTmpTBL add column cHeadId C(125)
alter table csrTmpTBL add column cHeadR C(125)
alter table csrTmpTBL add column FromTime C(10)
alter table csrTmpTBL add column ThruTime C(10)

select qTmpTBL
go top in qTmpTBL

select(lnSelect)
return 

*=========================================================
procedure FillPIVOTTable
lparameters tcOPT,tcRateFld,tcUnitFld,tcInOutFld,tcActFld,tcLocFld
*** The procedure has two tasks: 
* 		1 - Populate the PIVOT cursor
* 		2 - Builds the report header dynamically.
*		1 - Backtracking - It helps the group the expenses 
*         and all their details under the same 
*         column numbers.
*     2 - Builds the report header dynamically - The data, 
*         Id's and Rate, it is used to build two strings 
*         and then saved into the PIVOT table. 
*         In fact, these fields are used as report header. 
*
* PARMETERS: 
*		tcOPT: Field for which we need create 3 new fields
*				 (ID, UNIT, RATE, INOUT)
* 		tcRateFld: Field name which contains the RATE 
* 		tcUnitFld: Field name which contains the UNIT 
*
local lnSelect, lnI, lnK, lcField, lcInOutHHMM
local lcHeadRate, lcHeadUnit, lcIdField, lcExpId
local lnRate, lnUnit 
local lcRateField, lcUnitField, lcInOutField
local lcActField, lcLocField, lcInOUT, lcInOUTVal

store "" to lcHeadRate, lcHeadId, lcIdField, lcExpId
store "" to lcRateField, lcUnitField, lcInOutField
store "" to lcInOutHHMM, lcActField, lcLocField
store "" to lcInOUT, lcInOUTVal
store 0 to lnI, lnK, lnRate, lnUnit 

lnSelect = select()

use in select("qTblHead")
create cursor qTblHead ( ;
	PersId N(8,0), cId C(10), RateId C(12), UnitId C(12))

*** Build the header for each record by EFFDT 
* Saveral records might have the same expense ID.
select qTmpTBL
go top in qTmpTBL
do while !eof()
	lcPersId = trim(field(1))
	lcEffDt = trim(field(2))

	lcInOUT = trim(field(3))

	lnK = 0 
	lnI = 1 
	lnPersId = nvl(evaluate(lcPersId),0)
	do while evaluate(lcPersId) = lnPersId and !eof()

		if lnI = 1 
			lnJ = lnI
		else
			lnJ = 1 
		endif 

		ldEffDt = nvl(evaluate(lcEffDt),{})
		do while evaluate(lcPersId) = lnPersId and ;
			evaluate(lcEffDt) = ldEffDt and !eof()

			lcInOUTVal = nvl(evaluate(lcInOUT),"")
			do while evaluate(lcPersId) = lnPersId and ;
				evaluate(lcEffDt) = ldEffDt and ;
				evaluate(lcInOUT) = lcInOUTVal and !eof()

				select csrTmpTBL 
				locate for evaluate(lcPersId) = lnPersId and ;
						evaluate(lcEffDt) = ldEffDt and !eof()
				if !found()
					append blank in csrTmpTBL
					replace (lcPersId) with lnPersId 	in csrTmpTBL
					replace (lcEffDt) with ldEffDt  		in csrTmpTBL
					replace (lcInOUT) with lcInOUTVal	in csrTmpTBL
				endif 

				*** Search for the expense ID 
				lcExpId = evaluate("qTmpTBL."+tcOPT)
				=GetIndexOffset(lnPersId, lcExpId, @lnI, @lnK)

				select qTmpTBL
				lcIdField = "Id" + lstr(lnI)
				lcRateField = "Rate" + lstr(lnI)
				lcUnitField = "Unit" + lstr(lnI)
				lcInOutField = "InOut" + lstr(lnI)
				lcActField = "Activ" + lstr(lnI)
				lcLocField = "Locat" + lstr(lnI)
				
				select csrTmpTBL 		
				replace (lcIdField)		with evaluate("qTmpTBL."+tcOPT) 
				replace (lcRateField)	with evaluate("qTmpTBL."+tcRateFld)
				replace (lcUnitField)	with evaluate("qTmpTBL."+tcUnitFld)
				replace (lcActField)		with evaluate("qTmpTBL."+tcActFld)
				replace (lcLocField)		with evaluate("qTmpTBL."+tcLocFld)

				lcInOutHHMM = alltrim(evaluate("qTmpTBL."+tcInOutFld))
				if !empty(lcInOutHHMM)
					lcInOutHHMM = left(lcInOutHHMM,2) + ":" + ;
							substr(lcInOutHHMM,3,5) + ":" + substr(lcInOutHHMM,8)
				endif 
				replace (lcInOutField)	with lcInOutHHMM 

				select qTblHead
				locate for PersId=lnPersId and trim(cId)=trim(lcExpId) and !eof()
				if !found()
					store 0 to lnRate, lnUnit 
					lnRate = evaluate("csrTmpTBL." + lcRateField)
					lnUnit = evaluate("csrTmpTBL." + lcUnitField)
					
					insert into qTblHead (PersId, cId, RateId, UnitId) ;
						values (lnPersId, trim(evaluate("csrTmpTBL."+lcIdField)), ;
									alltrim(str(lnRate,12,2)), alltrim(str(lnUnit,12,2)))
				endif
					
				select qTmpTBL
				skip 

				lnJ = lnJ + 1 

				if lnK > 0 
					lnI = lnK 
					lnJ = lnK 
					lnK = 0 
				else 	
					lnI = lnI + 1 
				endif
			enddo

			select qTmpTBL
		enddo 

***	set step on 
		select qTmpTBL
	enddo 
				
	select qTmpTBL 
enddo 


*** Build the header by PERSID 
select qTblHead
go top in qTblHead
do while !eof()
	lcPersId = trim(field(1))

	store "" to lcHeadRate, lcHeadId

	lnPersId = nvl(evaluate(lcPersId),0)
	do while evaluate(lcPersId) = lnPersId and !eof()

		lcHeadId = lcHeadId + padc(upper(trim(cId)), 8, " ") + "|"
		lcHeadRate = lcHeadRate + padc(trim(RateId)+ " $", 8, " ") + "|"

		select qTblHead
		skip
	enddo 
	
	*** cHeadR & cHeadU contain the header of 
	* the report by PERSID 
	select csrTmpTBL 
	go top in csrTmpTBL 
	replace all cHeadId with trim(lcHeadId) ;
			for evaluate(field(1)) = lnPersId 
	replace all cHeadR with trim(lcHeadRate) ;
			for evaluate(field(1)) = lnPersId 

	select qTblHead
enddo 

select csrTmpTBL 
go top in csrTmpTBL 

select(lnSelect)
return 

*=========================================================
procedure GetIndexOffset(tnPersId, tcExpId, tnI, tnK)
*** Gets the index offset of the PIVOT table. For example, 
* if the previous date ends at column 5 the next one 
* should start at column 6.
* 
local lnSelect

lnSelect = select()

tcExpId = alltrim(tcExpId)

select qTblHead
locate for PersId=tnPersId and trim(cId)=tcExpId and !eof()
if found()
	tnK = tnI 
	tnI = recno("qTblHead")
endif 

select(lnSelect)		
return 

*=========================================================
procedure ComputeTotals()
*** Calculate the grand total by PERSID from all 
* the 25 subtotals columns in order to print on 
* the report a single column.
* 
local lnSelect
local lcField, lcUnit, lcRate, lcTotal
store "" to lcField, lcUnit, lcRate, lcTotal

lnSelect = select()

*** set step on 
select csrTmpTBL
go top in csrTmpTBL
do while !eof()

	lnPersId = csrTmpTBL.TT_PERSID 
	do while lnPersId = csrTmpTBL.TT_PERSID and !eof()
	
		for lnI = 1 to 25 
			lcField = "Id" + lstr(lnI) 

			lcUnit = "Unit" + lstr(lnI) 
			lcRate = "Rate" + lstr(lnI) 
			lcTotal= "Tot" + lstr(lnI)

			if !empty(evaluate(lcField))
				lnRecNo = recno("csrTmpTBL")
				calculate sum(&lcRate*&lcUnit) ;
					for csrTmpTBL.TT_PERSID=lnPersId to lnTot  

				replace all (lcTotal) with lnTot ;
					for csrTmpTBL.TT_PERSID=lnPersId

				if lnRecNo > 0 
					go lnRecNo in csrTmpTBL
				endif 
			endif 	
		next 

		select csrTmpTBL
		skip
	enddo 
enddo 					

select csrTmpTbl 
go top in csrTmpTbl
*** browse normal 

select (lnSelect)
return 

*=========================================================
procedure FillInTime()
*** Fill into a single field the start and end time 
* of an expense. We will always print only from 
* the 1st populated column due to N..1 relationship.
* 
local lcField, lcFromTime, lcThruTime
store "" to lcField, lcFromTime, lcThruTime
*
select csrTmpTBL
go top in csrTmpTBL
do while !eof()
	for lnI = 1 to 25 
		lcField = "InOut" + lstr(lnI) 
		if !empty(evaluate(lcField))
			lcFromTime = alltrim(leftto(evaluate(lcField),"-"))
			lcThruTime = alltrim(rightfrom(allt(evaluate(lcField)),"-"))

			replace next 1 FromTime with lcFromTime in csrTmpTBL
			replace next 1 ThruTime with lcThruTime in csrTmpTBL

			exit 
		endif 	
	next 

	select csrTmpTBL
	skip
enddo 					

return

*=========================================================
procedure GetPayPeriod(tdFrom, tdThru)
*** The PIVOT query give the data on multiple lines, 
* however, we need to have the data summarized by 
* expense id and by PERSID. - S.G.
*  
local loBizPayno, loBizTimeDt
local lcPayGRP, lcPayNo, lcLastPayNo

store null to loBizPayno, loBizTimeDt
store "" to lcPayGRP, lcPayNo, lcLastPayNo

loBizPayno = GetBiz("PAYNO")
loBizTimeDt = GetBiz("TIMEDT")

lcPayGRP = "QUIN"
lcPayNo = lcPayGRP + left(dtos(date()), 4) + "01"

tdFrom = loBizPayno.GetValueById(lcPayNo, "PN_STARTDT")

lcLastPayNo = loBizPayno.GetLastPayNo(left(lcPayNo, 8), "/REG")
tdThru = loBizPayno.GetValueById(lcLastPayNo, "PN_ENDDT")

store null to loBizPayno, loBizTimeDt

return 

*=========================================================
procedure SpoolOut()
*** This procedure is used for debugging purposes.
* It saves the cursors into CARVER's 
* default TEMP directory if _DEBUG = .T.
*
local lcTEMPDir, lcDbf
store "" to lcTEMPDir, lcDbf

if !llDEBUG or type("goIni")="U" 
	return 
endif 	

lcTEMPDir = trim(addbs(goIni.Temp)) 
if empty(lcTEMPDir)
	return 
endif 

if used("qTmpTBL")
	select qTmpTBL
	go top in qTmpTBL

	lcDbf = lcTEMPDir+alltrim(alias())+".dbf"
	copy to (lcDbf )
	go top in qTmpTBL
endif 
	
if used("csrTmpTbl")
	select csrTmpTbl
	go top in csrTmpTbl

	lcDbf = lcTEMPDir+alltrim(alias())+".dbf"
	copy to (lcDbf )
	go top in csrTmpTbl
endif 

if used("qTblHead")
	select qTblHead
	go top in qTblHead

	lcDbf = lcTEMPDir+alltrim(alias())+".dbf"
	copy to (lcDbf )
	go top in qTblHead
endif 

return 
endproc

*  Program...........: bhvJobhist_Custom
*  Author............: Stefan Gabor
*  Project...........: *** SFPQ CUSTOM ***
*  Created...........: October 23, 2013
*  Code page.........: 1252 (WINDOWS)
*  Copyright.........: (c) Carver Technologies Inc. 2010
*  Description.......: Custom subclass of form behavior class
*                    : 
*                    : 
*  Modification      : Stefan - 2013/10/23 ---  
*                    : The procedure has been created to handle 
*							: an extra salary field for a given employee. 
*							: The client, SFPQ, have two types of 
*                    : employees: their own and some other institution
*                    : employees for which SFPQ pays only a portion
*                    : of their annual salary.
*							: Both salary options need to be shown on the 
*                    : screen, however, the only logic has been 
*                    : implemented yet is related to annual salary 
*                    : (SFPQ and 3rd party institution)
*                    : 
*                    : 
*                    : 
*#########################################################
 define class bhvJobhist_Custom ;
 			  as BHVJobhist of bhvJobhist.prg
*#########################################################

*** Properties:
nTAnnual=0.0 


*==========================================================
*** HOT FIX - Avoid updating the client - S.G.
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

*========================================================
procedure Init(toThisform, tcSubType, tcInitialKey)
*
dodefault(toThisform, tcSubType, tcInitialKey)

this.LoadGvtSalaryOntoForm()

return

*==========================================================
procedure LoadGvtSalaryOntoForm()
*** Loads government salary field onto the form 
* The 3rd party salary info comes from H__OANNAUL field 
* and it is handle in the cursor qJobhist.
* 
if !used("vJobhist") or reccount("vJobhist")<=0 
	return 
endif 

local lnSelect, lnRecordNo 
lnRecordNo = recno("vJobhist")
*
lnSelect = select()

*** Get original index records 
use in select("qJobhist")
select H_PERSID,H_EFFDT,H_ONDATE,H_UNIQID, ;
		 	00000000000.00 as vvOANNUAL ;
from vJobHist readwrite ;
Into cursor qJobhist

if reccount("qJobhist")<=0
	select(lnSelect)
	return 
endif 

select qJobHist
index on iif(empty(H_ONDATE), "99999999", dtos(H_ONDATE)) + ;
			dtos(H_EFFDT) + H_UNIQID tag TUNIQ

*** Fill the 2nd cursor for government salary 
select vJobhist
go top in vJobhist
scan 
	if seek(dtos(vJobhist.H_ONDATE) + dtos(vJobhist.H_EFFDT) + ;
			vJobhist.H_UNIQID, "qJobhist", "TUNIQ" )

		replace vvOANNUAL with ;
				val(decrypt(vJobhist.H__OANNUAL,gcEWORD)) in qJobhist
	endif 
	
	select vJobHist 
	this.nTAnnual= nvl(vJobhist.vvAnnual,0) + ;
			nvl(qJobhist.vvOAnnual, 0)
endscan 

if (lnRecordNo > 0 or lnRecordNo = -1)
	go lnRecordNo in vJobhist 
endif 	

select(lnSelect)
return 

*==========================================================
procedure SaveGovntSalaryFromForm()
*** Save government's salary into the custom field
* "H__OANNUAL" encrypted. 
*
local lnSelect 
if !used("qJobhist") 
	return
endif 

lnSelect = select()

select vJobhist
if type("H__OANNUAL") != "U"
	replace next 1 H__OANNUAL with ;
		encrypt(str(qJobhist.vvOANNUAL,14,2), "JAZZ") in vJobhist  
endif

select(lnSelect)
return 

*=========================================================
procedure BeforeRefresh()

dodefault()

if !used("qJobhist") 
	return
endif 
this.nTAnnual= vJobhist.vvAnnual + qJobhist.vvOAnnual

return 

*==========================================================
procedure AfterCancel()
*** Position the record into qJobhist
*
dodefault()

if !used("vJobhist") or reccount("vJobhist")=0
	return 
endif 

this.LoadGvtSalaryOntoForm()

*** Reposition VJOBHIST to last record edited
=seek(dtos(vJobhist.H_ONDATE) + dtos(vJobhist.H_EFFDT) + ;
		vJobhist.H_UNIQID, "qJobhist", "TUNIQ" )

return 

*==========================================================
procedure AfterMove (pcNewKey)
*** Position the record into qJobhist
*
dodefault()

this.LoadGvtSalaryOntoForm()

if !used("vJobhist") or reccount("vJobhist")=0
	return 
endif 

=seek(dtos(vJobhist.H_ONDATE) + dtos(vJobhist.H_EFFDT) + ;
		vJobhist.H_UNIQID, "qJobhist", "TUNIQ" )

return 

*=========================================================
procedure Flush()

dodefault()

if !used("qJobhist") 
	return
endif 

this.SaveGovntSalaryFromForm()
return

*=========================================================
procedure TAnnual_valid(poThis)
*
local llOk
llOk = .T. 

if !used("qJobhist") 
	return
endif 

if !empty(this.nTAnnual)
	llOk = iif(llOk, this.RecalculateSalary(), .f.)
	llOk = iif(llOk, this.oThisform.refresh(), .f.)
	
	if !llOk
		this.CancelRec()
	endif 	
endif 

return

*=========================================================
procedure OthAnnual_valid(poThis)
*
local llOk
llOk = .T. 

if !used("qJobhist") 
	return
endif 

if !empty(qJobHist.vvOAnnual) ;
and !empty(this.nTAnnual)
	llOk = iif(llOk, this.RecalculateSalary(), .f.)
	llOk = iif(llOk, this.oThisform.refresh(), .f.)
	
	if !llOk
		this.CancelRec()
	endif 	
endif 

return 

*=========================================================
procedure RecalculateSalary()
* Calcutate salary information every time when either 
* H_ANNUAL or H__OANNUAL changes
*
local lnSFPQANNUAL, lnPAYRATIO, lnUNITRATE 
local lnHRSWKLY, lnSALARY, lnOTHRATE

store 0 to lnSFPQANNUAL, lnHRSWKLY, lnOTHRATE
store 0 to lnPAYRATIO, lnUNITRATE, lnSALARY

if !this.ValidateTBLInfo(@lnHRSWKLY,@lnPAYRATIO)
	return .f. 
endif 

lnSFPQANNUAL=round((this.nTAnnual-qJobHist.vvOAnnual), 2)
lnSALARY=round((lnSFPQANNUAL/lnPAYRATIO), 2)
lnUNITRATE=round((lnSALARY/lnHRSWKLY), 2)
lnOTHRATE=round(((qJobHist.vvOAnnual/lnPAYRATIO)/lnHRSWKLY),2)

if !this.ValidateSalaryInfo(lnSFPQANNUAL)
	return .f. 
endif 

replace vvAnnual with lnSFPQANNUAL in vJobHist
replace vvAnnualFT with lnSFPQANNUAL in vJobHist
replace vvSalary with lnSALARY in vJobHist
replace vvUnitRate with lnUNITRATE in vJobHist
replace vvOthRate with lnOTHRATE in vJobHist

this.SaveGovntSalaryFromForm()
return

*=========================================================
procedure ValidateTBLInfo(tnHRSWKLY, tnPAYRATIO)
* Validation procedure when recalculation salary info 
* in terms of TBL params - See: RecalculateSalary()
*
local lcPAYGRP, lcComm1, lcComm2 
store "" to lcPAYGRP, lcComm1, lcComm2 

*** Get payroll info 
lcPAYGRP=trim(tbleval("PAYGRP",vJobhist.H_PAYGRP,"TBLC1"))
tnPAYRATIO=val(tbleval("PERIOD",lcPAYGRP,"TBLC1"))

if gcLang = "E"
	lcComm1 = "Please verify the pay group and try again!"
	lcComm2 = "Please verify the pay group [ "+;
					lcPAYGRP+" ] ratio!"
else
	lcComm1 = "Veuillez vérifier le groupe de paie et réessayer!"
	lcComm2 = "Veuillez vérifier le groupe de paie [ "+;
					lcPAYGRP+" ] ratio!"
endif 

*** Validation 
if empty(lcPAYGRP)
	alert(lcComm1, 0, "\!\?\<OK")
	return .f. 
endif 	

if empty(tnPAYRATIO)
	alert(lcComm2, 0, "\!\?\<OK")
	return .f. 
endif 

*** Get the hours worked by pay period based 
* on H_HRSPER from Jobhist   
if "B"$lcPAYGRP 
	tnHRSWKLY = vJobHist.H_HRSPER * 2 
endif 

return ((tnHRSWKLY > 0) and (tnPAYRATIO > 0))
endproc

*=========================================================
procedure ValidateSalaryInfo(tnANNUALSalary)
* Validation procedure in terms of annual salary 
*
local lcComm1 
if gcLang = "E"
	lcComm1 = "Annual salary cannot be zero or negative!"
else
	lcComm1 = "Le salaire annuel ne peut pas être zéro ou négative!"
endif 

if (tnANNUALSalary <= 0)
	alert(lcComm1, 0, "\!\?\<OK")
	return .f. 
endif 

return 
endproc



enddefine
*#########################################################

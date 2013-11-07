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
if !used("vJobhist")
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
	this.nTAnnual= vJobhist.vvAnnual + qJobhist.vvOAnnual
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
this.nTAnnual= vJobhist.vvAnnual + qJobhist.vvOAnnual

return 

*==========================================================
procedure AfterCancel()
*** Position the record into qJobhist
*
dodefault()

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

=seek(dtos(vJobhist.H_ONDATE) + dtos(vJobhist.H_EFFDT) + ;
		vJobhist.H_UNIQID, "qJobhist", "TUNIQ" )

return 

*=========================================================
procedure Flush()

dodefault()

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

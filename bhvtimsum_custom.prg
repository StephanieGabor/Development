*  Program...........: bhvTIMSUM_Custom 
*  Author............: Stefan Gabor 
*  Project...........: Carver Human Resources Zoo62
*  Created...........: June 2014 
*  Code page.........: 1252 (WINDOWS)
*  Copyright.........: (c) Carver Technologies Inc. 2014 
*  Description.......: Behavior class for form
*                    : 
*                    : 
*  Classes...........: bhvTimsum_Custom as bhvTimsum
*                    : 
*                    : 
*                    : 
*#########################################################
 define class bhvTimsum_Custom ;
 					as bhvTimsum of bhvTimsum.PRG
*#########################################################
*** Properties:


*==========================================================
procedure cmdInquiry_Valid(poThis)
***
*
local loException 
store null to loException 

if isnull(poThis)
	return 
endif 	

try 

	do T28.PRG

catch to loException 

	=messagebox( loException.Message, 0, "Umana" )

endtry 
 
return 
endproc


*#########################################################
enddefine

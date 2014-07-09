*  Program...........: bhvsched_Custom
*  Author............: Mike Gagnon
*  Project...........: SFPQ based on D.D.O.
*  Created...........: June 27, 2014
*  Code page.........: 1252 (WINDOWS)
*  Copyright.........: (c) Carver Technologies 2014
*  Description.......: Behavior for sched (sched6) form
*                    : 
*                    : *** CUSTOM VERSION FOR Shawinigan  ***
*                    : *** HOT FIX ONLY ***
*                    : 
*                    : 
*  Technical Notes...: NOTE: This is a subclass of
*                    :       bhvSched.
*                    : 
*                    : 
*                    : 
*                    : 
*                    : 

*#########################################################
 define class bhvsched_custom as bhvsched ;
 			  of bhvsched.prg
*#########################################################
*=========================================================
procedure CntTimeRangeWork_Valid(poThis)
this.CalcHoursWorked(poThis)
poThis.parent.refresh()

*=========================================================
procedure CntTimeRangeBreak_Valid(poThis)
this.CalcHoursWorked(poThis)
poThis.parent.refresh()

*##################################################
enddefine


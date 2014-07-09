*========================================================
*  Program...........: bizTBL_custom
*  Author............: Stefan Gabor 
*  Project...........: SFPQ 
*  Created...........: July 7, 2014
*  Code page.........: 1252 (WINDOWS)
*  Copyright.........: (c) Carver Technologies 2014
*  Description.......: *** HOT FIX *** 
*                    :
*                    : *** CUSTOM VERSION FOR SFPQ ***
*                    :
*  Description		   : ERROR - line no: 627 
*                    : 1707: Structural .CDX file is not found.
*                    : 
*                    : 
*#########################################################
define class bizTBL_custom ;
		as bizTBL of bizTBL.prg
*#########################################################


*==========================================================
*** HOT FIX *** "DESCR" - Single value TBL error  
*==========================================================
procedure GetTbltypeValueById(pcTbltype, pcExpr)

local lcValue, lcExpr, lcWhere, llHidden

*** BYPASS ERROR 
if type("pcExpr") = "C" ;
and upper(pcExpr) = "DESCR"
	pcExpr = "TBLDESCR" + gcLang
endif 

lcExpr = alltrim(evl(pcExpr, "TBLNAM" + gcLang))
lcExpr = iif(upper(lcExpr)="TYPE", "TBLID", lcExpr)

pcTbltype = alltrim(upper(pcTbltype))

lcWhere = "TBLTYPE='" + pcTbltype + "'" + ;
			 " and inlist(TBLID, '+++','***')"
lcValue = goDataMgr.GetValue(lcWhere, "TBL", lcExpr)

if vartype(lcValue) = "C"
	lcValue = alltrim(lcValue)
endif

if empty(lcValue) and upper(lcExpr) = "TYPE"
	*** Maybe a hidden table
	llHidden = goDataMgr.Exists("TBL", ;
				  "TBLTYPE='" + pcTbltype + "'")
	return iif(llHidden, "HID", "")
else
	return lcValue
endif

*==========================================================
*** HOT FIX *** 1707: Structural .CDX file is not found.
*==========================================================
procedure GetPickList (pcSwitches, pcTblType, pcFilter)
*** Generates a pick list to select from

*** Switches:	/CURSOR=...
*              /OBSolete = Include obsolete records
*              /NOCACHE
*** pcTbltype = TBLTYPE  = list of table types (+++)
*               TBLTYPEA = tbltype +++ or ***
*               DBFS = list of DBFS from DATADICT
*               FIELDS:<dbfname> = list of fields & expressions
*               FIELDS1:<dbfname> = list of real fields
*               ENUM: <dbfname>.<Fieldname>
*               ENUM:MEMVAR.<memvar-name>
*               ENUM:NCONFIG.<CF_UNIQID value>

local loSetExact
loSetExact = SetTo("On", "Exact")

this.ErrorID = ""
this.ErrorArgs = ""

if empty(pcSwitches)
	this.ErrorID = "BIZ.COMMON.ARG1_REQUIRED"
	this.ErrorArgs = ""
	return null
endif

private lcType, lcAlias, lcTblType, laFldList, laDbfList
local llObsolete, lcInactive, lcInactiveFld, lcWhere, loCursor
local lcDescrFld
local lcFilter, lcXML, llNoCache
local lcRedirDBF, lcRedirKey, lcRedirName, lcRedirFilter
local lcRedirBiz, loRedirBiz, llRedirect, lnShowCodes

*** Analyse arguments
pcSwitches = upper(alltrim(pcSwitches))
pcFilter = evl(pcFilter, "")
lcTBLTYPE = upper(alltrim(pcTblType))
llObsolete = "/OBS" $ upper(pcSwitches)
lnShowCodes = extract(pcSwitches, "/SHOWCODES=","/")
lnShowCodes = iif(empty(lnShowCodes), -1, val(lnShowCodes))

lcType = ""
lcAlias = ""		&& Changed below
if !this.GetDest(@pcSwitches, @lcType, @lcAlias)
	return .f.
endif
lcAlias = chrtran(lcAlias, ":.", "__")

*** Get Redirection info
llRedirect = goBzgTbl.GetRedirectInfo(pcTblType, ;
		@lcRedirDBF, @lcRedirKey, @lcRedirName, ;
		@lcRedirFilter, @lcRedirBiz)

*** These tables cannot be cached
llNoCache = inlist(lcTbltype, "PERS", "FIELDS") ;
				or "/NOCACHE" $ pcSwitches ;
				or (vartype(pcFilter)="C" and "TBLC" $ upper(pcFilter))


*//////////////////////////// To do ///////////
if lcTYPE <> "CURSOR"
	this.ErrorID = "BIZ.COMMON.ONLY_CURSOR_SUPPORTED"
	return .f.
endif

do case
case left(lcTbltype,5) = "ENUM:"
	*** Handle ENUMs as a special case
	lcTbltype = chrtran(lcTbltype, ":", "_")
	loCursor = this.GetPicklist_FromEnum(pcSwitches, lcTblType, pcFilter)

case inlist(lcTbltype, "TBLTYPE", "TBLTYPEA")
	*** Return list of table types (+++ records) available
	if pcFilter = ".T." or lcTbltype = "TBLTYPEA"
		*** Special case for use with cboTbltest
		lcWhere = "inlist(TBLID,'+++','***')"
	else
		lcWhere = "TBLID='+++'"
		if vartype(pcFilter) = "C"
			lcWhere = makefor("", lcWhere, ;
						 strtranc(pcFilter, "TBLID", "TBLTYPE"))
		endif
	endif
	loCursor = goDataMgr.GetCursor(pcSwitches, ;
			set("datasession"), ;
			"TBL", ;
			"space(100) as DISPNAME," + ;
				"TBLTYPE as TBLID, '    ' as TBLSORT, " + ;
				"TBLNAM"+gcLANG+" as TBLNAME", ;
			lcWhere)

case lcTBLTYPE = "DBFS"
	*** Build picklist of DBFS from datadict info
	pcFilter = strtran(pcFilter, "TBLID", "FIELD_NAME")

	*** Go to datadict to get list of DBFS
	dimension laDBFList[1,1]

	lcFilter = Makefor("", ;
					"DD_TYPE='DBF' and !left(FIELD_NAME,1) $ 'OVQ' and DD_DBF=''", ;
					pcFilter)
	goDatadict.Getlist("/ARRAY=laDbfList", ;
			"DD_NAM"+gcLang+ ", FIELD_NAME, FIELD_TYPE, " + ;
			"DD_DESCR"+gcLang, ;
			lcFilter, "FIELD_NAME")

	if empty(lcAlias)
		lcAlias = "Q" + sys(2015)
	endif
	create cursor (lcAlias) ( ;
			TBLNAME C(30), ;
			TBLID C(16), ;
			FIELD_TYPE C(1), ;
			TBLDESCR M)
	append from array laDbfList

	alter table (lcAlias) ;
				add DispName C(100) ;
				add TblSort C(4)

	*** Create cursor object
	loCursor = goDataMgr.CreateCursorObject(;
				"", set("Datasession"), ;
				lcAlias, "TBL")

case left(lcTbltype,6) = "FIELDS"
	*** pcTbltype = "FIELDS:<dbf>" or "FIELD1:<dbf>"
	lcDBF = rightfrom(pcTbltype, ":")
	lcAlias = leftto(lcAlias, ":")
	if empty(lcAlias)
		lcAlias = "Q" + sys(2015)
	endif
	pcFilter = strtran(pcFilter, "TBLID", "FIELD_NAME")

	*** Go to datadict
	dimension laFldList[1,1]
	goDatadict.GetPicklist("/ARRAY=laFldList", lcDBF, pcFilter)
	create cursor (lcAlias) ( ;
			TBLNAME C(30), ;
			TBLID C(16), ;
			FIELD_TYPE C(1), ;
			FIELD_LEN N(3), ;
			FIELD_DEC N(1))
	append from array laFldList
	if lcTBLTYPE = "FIELDS1"
		*** Only real fields
		delete for "(" $ TBLID			&& )
	endif
	alter table (lcAlias) ;
				add DispName C(100) ;
				add TblSort C(4)

	*** Create cursor object
	loCursor = goDataMgr.CreateCursorObject(;
				"", set("Datasession"), ;
				lcAlias, "TBL")

otherwise
	*** Look for it in cache first
	if llNoCache
		lcXML = ""
	else
		lcXML = goBzgTbl.GetPicklist(pcSwitches, lcTblType)
	endif

	do case
	case !empty(lcXML)
		*** Found in cache.
		if empty(lcAlias)
			lcAlias = "Q"+substr(sys(2015),2)
		endif

		*** Create cursor object
		loCursor = goDataMgr.CreateCursorObject(;
					"", set("Datasession"), ;
					lcAlias, "TBL")

		*** Convert to XML.  Must do in current datasession
		xmltocursorx(lcXML, lcAlias)
		select evl(lcAlias,0)

		*** Add fields not cached
		if type(lcAlias + ".DISPNAME") = "U"
			alter table (lcAlias) ;
					add DISPNAME C(100)
		endif
		if type(lcAlias + ".TBLNAME") = "U"
			alter table (lcAlias) ;
					add TBLNAME C(100)
			replace all TBLNAME with ;
					iif(LongName <> "", LongName, ShortName)
		endif

		*** Delete items to filter out
		if !empty(pcFilter)
			*** New syntax in case pcFILTER is of form
			*   "TBLID in (...)
			delete from (alias()) where !(&pcFilter)
		endif

		*** Delete inactive entries if not wanted
		if !llObsolete and type("TBLINACTIV") $ "LC"
			*** What field is it stored in
			if type("TBLINACTIV") = "L"
				delete for TBLINACTIV
			else
				delete for TBLINACTIV = "T"
			endif
		endif

	case llRedirect
		*** Release old loCursor on TBL
		loCursor = null

		*** Create cursor on new table where info really is
		if empty(lcRedirBiz)
			if lcTblType = "PERS"
				*** Should not happen
				set step on
			endif

			*** Simple redirection
			lcInactiveFld = goDataDict.GetInactiveField(lcTbltype)
			if !llObsolete and lcInactiveFld > ""
				*** Table has in ACTIV field
				lcInactive = "," + leftto(lcRedirKey, "_") + "_INACTIV as TBLINACTIV"
			else
				lcInactive = ""
			endif

			lcDescrFld = goDataDict.GetDescrField(lcTbltype)
			if !empty(lcDescrFld)
				lcDescrFld = ", TBLDESCR as " + lcDescrFld
			endif

			lcFilter = iif(empty(PCFILTER) ;
							or vartype(PCFILTER) <> "C", "", PCFILTER)

			if "TBLID" $ lcFilter
				lcFilter = strtranc(lcFilter, "TBLID", lcRedirKey)
			endif

			loCursor = goDataMgr.GetCursor (pcSwitches, set("datasession"), ;
					lcRedirDBF, ;
					lcRedirKey + " as TBLID, " + ;
						strtran(lcRedirName, "@", gcLANG) + " as TBLNAME, " + ;
					"' ' as TBLSORT" + lcInactive + lcDescrFld, ;
					makefor("", lcRedirFilter))

		else
			*** Object-oriented redirection, handled by
			*   biz<RealTbl>.TblPicklist()
			loRedirBiz = this.oBizMgr.GetBiz (lcRedirBiz)
			lcWhere = iif(llNoCache, pcFilter, "")
			loCursor = loRedirBiz.TblPickList (;
					"/CURSOR" + iif(empty(lcAlias), "", "=" + lcAlias) + ;
					" /INACTIVE", ;
					lcWhere)

			if !type("TBLDESCR") $ "CM"
				delete tag all
				alter table (alias()) ;
						add TBLDESCR M
			endif
		endif

		*** Save in cache
		if !llNoCache
			*-- Need to convert to XML in current datasession,
			*   because goBzgTbl is in datasession 1.
			*   Need schema in case all values are numeric, in
			*   which case resulting field would be numeric.
			cursortoxml(0, "lcXML", 3, 1, 0, "1")
			goBzgTbl.SavePicklistInCache(lcTblType, lcXML)

			*** Delete unwanted items
			if !empty(pcFilter)
				*** New syntax in case pcFILTER is of form
				*   "TBLID in (...)
				delete from (alias()) where !(&pcFilter)
			endif
		endif

		*** Do this after saving in cache to avoid saving
		*   lots of spaces.
		if type("DISPNAME") <> "C"
			alter table (alias()) ;
					add DISPNAME C(100)
		endif

		*** Delete obsolete items... after saving in cache
		if !llObsolete and type("TBLINACTIV") = "L"
			delete for TBLINACTIV
		endif

	otherwise
		*** Table in TBL
		loCursor = this.GetList(pcSwitches, ;
				pcTBLTYPE, ;
				"TBLID, TBLNAM" + gcLANG + " as SHORTNAME," + ;
				"TBLLNAM" + gcLANG + " as LONGNAME," + ;
				"TBLSORT, TBLC1, TBLC2, TBLC3, TBLC4, " + ;
				"TBLC5, TBLC6, TBLC7, TBLC8, TBLTYPE, TBLINACTIV, " + ;
				"TBLDESCR"+gcLang+" as TBLDESCR", ;
				"", ;
				"TBLSORT,TBLID")

		*** Save in cache
		*-- Need to convert to XML in current datasession,
		*   because goBzgTbl is in datasession 1.
		*   Need schema in case all values are numberic, in
		*   which case resulting field would be numeric.
		cursortoxml(0, "lcXML", 3, 1, 0, "1")
		goBzgTbl.SavePicklistInCache(lcTblType, lcXML)

		*** Delete unwanted items
		if !empty(pcFilter)
			*** New syntax in case pcFILTER is of form
			*   "TBLID in (...)
			delete from (alias()) where !(&pcFilter)
		endif
		if !llObsolete
			delete for TBLINACTIV
		endif

		alter table (alias()) ;
				add DISPNAME C(100) ;
				add TBLNAME C(100)
		replace all TBLNAME with ;
				iif(LongName <> "", LongName, ShortName)

	endcase
endcase

*** Compose the name to display
replace all DispName with CodeAndText(TBLID, TBLNAME, lnShowCodes)

*** Index it
index on TBLID tag TBLID
index on TBLSORT + TBLID tag TBLSHOW

loCursor.Workarea = select()
return loCursor

enddefine
*##########################################################


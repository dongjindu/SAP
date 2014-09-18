************************************************************************
* Created by  : 100565
* Created on  : 01.26.2005
* Description : Upload enteries for ZTPM_PLANDBD
*
*
* Modification Log
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT ZCPM_CRE_BW_TBL_ENT_ZTPM_PLAND   NO STANDARD PAGE HEADING
                             MESSAGE-ID ZMPM LINE-SIZE 200.

FIELD-SYMBOLS : <FS>.
tables: ztpm_monbd,
        ztpm_PLANDBD.
*** For File Select
DATA: WA_FNAME LIKE RLGRAP-FILENAME,
      WA_RC LIKE SY-SUBRC.

*** For Cerate Functional Location BAPI
DATA: EXTERNAL_NUMBER	LIKE	BAPI_ITOB_PARMS-EQUIPMENT,
      DATA_GENERAL	LIKE	BAPI_ITOB,
      DATA_SPECIFIC	LIKE	BAPI_ITOB_EQ_ONLY,
      VALID_DATE	      LIKE	BAPI_ITOB_PARMS-INST_DATE,
      DATA_INSTALL	LIKE	BAPI_ITOB_EQ_INSTALL,
      RETURN	      LIKE	BAPIRET2.

*** Uploading Data Fields
DATA: BEGIN OF IT_TREC OCCURS 0,
        EQUNR LIKE ITOBATTR-EQUNR, 	"Equipment number
        DATSL LIKE RM63E-DATSL,    	"Date valid
        EQTYP LIKE RM63E-EQTYP,	"Equipment category

        EQART LIKE ITOB-EQART,	"Type of Technical Object
        SHTXT LIKE ITOB-SHTXT,      "Description of technical object
        BEGRU LIKE ITOB-BEGRU,	"authorization group
        BRGEW LIKE ITOB-BRGEW,	"Weight of object
        GROES LIKE ITOB-GROES,	"Size/dimension
        INVNR LIKE ITOB-INVNR,	"Inventory number
        INBDT LIKE ITOB-INBDT,	"Start-up Date
        WAERS LIKE ITOB-WAERS,	"Currency Key
        ANSWT LIKE ITOB-ANSWT,	"Acquisition value
        ANSDT LIKE ITOB-ANSDT,	"Acquisition date
        HERST LIKE ITOB-HERST,	"Manufacturer of asset
        HERLD LIKE ITOB-HERLD,	"Country of manufacture
        TYPBZ LIKE ITOB-TYPBZ,	"Manufacturer model number
        BAUJJ LIKE ITOB-BAUJJ,	"Year of construction
        MAPAR LIKE ITOB-MAPAR,  	"Manufacturer part number
        SERGE LIKE ITOB-SERGE,	"Manufacturer serial number
        SWERK LIKE ITOB-SWERK,	"Maintenance plant
        STORT LIKE ITOB-STORT, 	"Asset location
        MSGRP LIKE ITOB-MSGRP,	"Room
        BEBER LIKE ITOB-BEBER,	"Plant section
        ARBPL LIKE ITOBATTR-ARBPL,	"Object ID of PP work center
        ABCKZ LIKE ITOB-ABCKZ,	"ABC indicator for technical object
        EQFNR LIKE ITOB-EQFNR,	"Sort field
***  Organization
        BUKRS LIKE ITOB-BUKRS,	"Company Code
        GSBER LIKE ITOB-GSBER,	"Business Area
        ANLNR LIKE ITOB-ANLNR,	"Main asset number
        ANLUN LIKE ITOB-ANLUN,	"Asset sub-number
        KOSTL LIKE ITOB-KOSTL,	"Cost Center
        DAUFN LIKE ITOB-DAUFN,	"Standing order number
        AUFNR LIKE ITOB-AUFNR,	"Settlement order
        IWERK LIKE ITOB-IWERK,	"Maintenance Planning Plant
        INGRP LIKE ITOB-INGRP,	"Planner Group for Plant Maintenance
***  Structure
        TPLNR LIKE ITOB-TPLNR,	"Functional location label
        HEQUI  LIKE ITOB-HEQUI, 	"Superior Equipment
        POSNR LIKE ITOB-POSNR,   	"Equipment position
      END OF IT_TREC.


DATA: BEGIN OF IT_T_REC OCCURS 0,
MANDT LIKE ZTPM_OPTIME-MANDT,
SHOP LIKE ZTPM_OPTIME-SHOP,
AJAHR LIKE ZTPM_OPTIME-AJAHR,
ZMONTH LIKE ZTPM_OPTIME-ZMONTH,
END OF IT_T_REC.

DATA: IT_T_REC1 LIKE ZTPM_PLANDBD OCCURS 0 WITH HEADER LINE.
****For Error Message
DATA: WA_RETURN LIKE BAPIRET2 .

DATA: BEGIN OF IT_ERROR OCCURS 0,
        TYPE LIKE BAPIRET2-TYPE,
        ID LIKE BAPIRET2-ID,
        NUMBER LIKE BAPIRET2-NUMBER,
        MESSAGE LIKE BAPIRET2-MESSAGE,
      END OF IT_ERROR.


SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
PARAMETER: P_FILE  LIKE RLGRAP-FILENAME MODIF ID BCD.
SELECTION-SCREEN PUSHBUTTON /70(10) PH_FNAME USER-COMMAND CHG.
SELECTION-SCREEN END OF BLOCK BLOCK1.

******************* INITIALIZATION ********************************
*******************************************************************
INITIALIZATION.
  MOVE 'Find' TO PH_FNAME.

***************** AT SELECTION-SCREEN ******************************
********************************************************************
AT SELECTION-SCREEN.
  CASE SY-UCOMM.
    WHEN 'CHG'.
      CLEAR: SY-UCOMM.
      PERFORM FILENAME_GET.

    WHEN 'ONLI'.
      CLEAR: SY-UCOMM.
      PERFORM MAIN_PROCESS.

    WHEN OTHERS.
      CLEAR: SY-UCOMM.

  ENDCASE.
******** START-OF-SELECTION *****************
*********************************************
START-OF-SELECTION.
*********************************************


******** END-OF-SELECTION *******************
*********************************************
END-OF-SELECTION.
*********************************************
*&---------------------------------------------------------------------*
*&      Form  MAIN_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAIN_PROCESS.
  CLEAR:  IT_ERROR, IT_ERROR[],
          IT_TREC,  IT_TREC[].

  IF P_FILE IS INITIAL.
    PERFORM FILENAME_GET.
  ENDIF.

  PERFORM EXCEL_FILE_UPLOAD  TABLES   IT_T_REC
                             USING    P_FILE
                                      WA_RC.

 LOOP AT IT_T_REC.
**    PERFORM CREATE_FUNCLOC.
**PERFORM UPDATE_TABLES.
*MODIFY ZTPM_MONBD FROM TABLE IT_T_REC .
*WRITE:/ IT_T_REC-SHOP, IT_T_REC-ZMONTH.
MOVE-CORRESPONDING IT_T_REC TO IT_T_REC1.
APPEND IT_T_REC1.
 ENDLOOP.
 LOOP AT IT_T_REC1.
*WRITE:/ IT_T_REC1-SHOP, IT_T_REC1-ZMONTH.
MODIFY ZTPM_PLANDBD FROM TABLE IT_T_REC1.
ENDLOOP.

  PERFORM WRITE_MESSAGE.

ENDFORM.                    " MAIN_PROCESS


*&---------------------------------------------------------------------*
*&      Form  FILENAME_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILENAME_GET.
*** Call file selector
  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            MASK             = ',*.XLS,*.XLS.'
            MODE             = 'O'
            TITLE            = 'Select Upload File'
       IMPORTING
            FILENAME         = P_FILE
            RC               = WA_RC
       EXCEPTIONS
            INV_WINSYS       = 1
            NO_BATCH         = 2
            SELECTION_CANCEL = 3
            SELECTION_ERROR  = 4
            OTHERS           = 5.

ENDFORM.                    " FILENAME_GET
*&---------------------------------------------------------------------*
*&      Form  excel_FILE_upload
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_TAB  text
*      -->P_P_FNAME  text
*      -->P_P_RC  text
*----------------------------------------------------------------------*
FORM EXCEL_FILE_UPLOAD TABLES   IT_T_REC
                       USING    P_FNAME
                                P_RC.

  DATA : IT_INTERN TYPE  KCDE_CELLS OCCURS 0 WITH HEADER LINE.
  DATA : LV_INDEX TYPE I.
  DATA : LV_START_COL TYPE I VALUE '1',
         LV_START_ROW TYPE I VALUE '1',
         LV_END_COL   TYPE I VALUE '256',
         LV_END_ROW   TYPE I VALUE '65536'.

*** Data transfer from PC files to Internal Table.
  CALL FUNCTION 'KCD_EXCEL_OLE_TO_INT_CONVERT'
       EXPORTING
            FILENAME                = P_FNAME
            I_BEGIN_COL             = LV_START_COL
            I_BEGIN_ROW             = LV_START_ROW
            I_END_COL               = LV_END_COL
            I_END_ROW               = LV_END_ROW
       TABLES
            INTERN                  = IT_INTERN
       EXCEPTIONS
            INCONSISTENT_PARAMETERS = 1
            UPLOAD_OLE              = 2.

* mapping Excel cells to Interanl Table fields
  MOVE : SY-SUBRC TO P_RC.
  CHECK NOT IT_INTERN[] IS INITIAL.

*-- Delete Header line: row from 1 to 2
  DELETE IT_INTERN WHERE ROW LE 2.

  SORT IT_INTERN BY ROW COL.

  LOOP AT IT_INTERN.
    MOVE : IT_INTERN-COL TO LV_INDEX.
    ASSIGN COMPONENT LV_INDEX OF STRUCTURE IT_T_REC TO <FS>.
    MOVE : IT_INTERN-VALUE TO <FS>.
    AT END OF ROW.
      APPEND IT_T_REC.
      CLEAR IT_T_REC.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " excel_FILE_upload
*&---------------------------------------------------------------------*
*&      Form  create_funcloc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_FUNCLOC.

  PERFORM FILL_PARAMETERS.


  CALL FUNCTION 'BAPI_EQUI_CREATE'
       EXPORTING
            EXTERNAL_NUMBER   = EXTERNAL_NUMBER
            DATA_GENERAL      = DATA_GENERAL
            DATA_SPECIFIC     = DATA_SPECIFIC
            VALID_DATE        = SY-DATUM
            DATA_INSTALL      = DATA_INSTALL
       IMPORTING
            EQUIPMENT         = EXTERNAL_NUMBER
            DATA_GENERAL_EXP  = DATA_GENERAL
            DATA_SPECIFIC_EXP = DATA_SPECIFIC
            RETURN            = RETURN.

  IF RETURN-TYPE  = 'E'.
    ROLLBACK WORK.
    CALL FUNCTION 'BALW_BAPIRETURN_GET2'
         EXPORTING
              TYPE   = RETURN-TYPE
              CL     = RETURN-ID
              NUMBER = RETURN-NUMBER
              PAR1   = RETURN-MESSAGE_V1
              PAR2   = RETURN-MESSAGE_V2
              PAR3   = RETURN-MESSAGE_V3
              PAR4   = RETURN-MESSAGE_V4
         IMPORTING
              RETURN = WA_RETURN.

    CLEAR:  IT_ERROR.
    MOVE-CORRESPONDING WA_RETURN TO IT_ERROR.
    APPEND IT_ERROR.
  ELSE.
    COMMIT WORK.
  ENDIF.

ENDFORM.                    " create_funcloc
*&---------------------------------------------------------------------*
*&      Form  WRITE_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_MESSAGE.
  ULINE.
  WRITE: / IT_ERROR-MESSAGE.
ENDFORM.                    " WRITE_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  fill_parameters
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_PARAMETERS.
**** Number of Equipment to be Created (Initial => Internal Assignment)
  EXTERNAL_NUMBER  = IT_TREC-EQUNR. 	"Equipment number

**** Valid-From Date for Equipment to be Created
  VALID_DATE       = IT_TREC-DATSL.    	"Date valid

**** General Data for Technical Objects
  DATA_GENERAL-AUTHGRP    = IT_TREC-BEGRU.	"authorization group
  DATA_GENERAL-OBJECTTYPE = IT_TREC-EQART.	"Type
  DATA_GENERAL-INVENTORY  = IT_TREC-INVNR.	"Inventory number
  DATA_GENERAL-OBJ_SIZE   = IT_TREC-GROES.	"Size/dimension
  DATA_GENERAL-OBJ_WEIGHT = IT_TREC-BRGEW.	"Weight of object
*DATA_GENERAL-UNIT_OF_WT	Unit of weight
*DATA_GENERAL-UNIT_ISO		ISO code for unit of measurement
  DATA_GENERAL-ACQDATE    = IT_TREC-ANSDT.	"Acquisition date
  DATA_GENERAL-ACQUISVAL  = IT_TREC-ANSWT.	"Acquisition value
  DATA_GENERAL-CURRENCY   = IT_TREC-WAERS.	"Currency Key
*DATA_GENERAL-CURR_ISO		ISO currency code
  DATA_GENERAL-MANFACTURE   = IT_TREC-HERST.	"Manufacturer of asset
  DATA_GENERAL-MANCOUNTRY   = IT_TREC-HERLD.	"Country of manufacture
*DATA_GENERAL-COUNTR_ISO	Country ISO code
  DATA_GENERAL-MANSERNO   = IT_TREC-SERGE.	"serial number
  DATA_GENERAL-MANMODEL   = IT_TREC-TYPBZ.	"model number
  DATA_GENERAL-CONSTYEAR  = IT_TREC-BAUJJ.	"Year of construction
*  DATA_GENERAL-CONSTMONTH = IT_TREC-BAUMM. 	"Month of construction
  DATA_GENERAL-START_FROM = IT_TREC-INBDT.	"Start-up Date
  DATA_GENERAL-PLANPLANT  = IT_TREC-IWERK.	"Planning Plant
*DATA_GENERAL-CONSTTYPE		Construction type material of the
*object
  DATA_GENERAL-MANPARNO     =	IT_TREC-MAPAR.  	"part number
  DATA_GENERAL-PLANGROUP    = IT_TREC-INGRP.	"Planner Group
*DATA_GENERAL-CATPROFILE	Catalog Profile
*DATA_GENERAL-WORK_CTR		Object ID of the Work Center
  DATA_GENERAL-DESCRIPT	 = IT_TREC-SHTXT.	"Description
  DATA_GENERAL-ABCINDIC	    = IT_TREC-ABCKZ.	"ABC indicator
  DATA_GENERAL-SORTFIELD    = IT_TREC-EQFNR.	"Sort field
  DATA_GENERAL-MAINTPLANT   = IT_TREC-SWERK.	"Maintenance plant
  DATA_GENERAL-MAINTLOC	    = IT_TREC-STORT. 	"Asset location
  DATA_GENERAL-MAINTROOM    = IT_TREC-MSGRP.	"Room
  DATA_GENERAL-PLSECTN	    = IT_TREC-BEBER.	"Plant section
  DATA_GENERAL-PP_WKCTR    = IT_TREC-ARBPL.	"Object ID
  DATA_GENERAL-BUS_AREA	    = IT_TREC-GSBER.	"Business Area
  DATA_GENERAL-COSTCENTER   = IT_TREC-KOSTL.	"Cost Center
*  DATA_GENERAL-WBS_ELEM	    = IT_TREC-PROID.	"WBS element
  DATA_GENERAL-COMP_CODE    = IT_TREC-BUKRS.	"Company Code
  DATA_GENERAL-ASSET_NO	    = IT_TREC-ANLNR.	"Main asset number
  DATA_GENERAL-SUB_NUMBER   = IT_TREC-ANLUN.	"Asset sub-number
  DATA_GENERAL-STANDORDER   = IT_TREC-DAUFN.	"Standing order number
  DATA_GENERAL-SETTLORDER   = IT_TREC-AUFNR.	"Settlement order

**** Equipment-Specific data
  DATA_SPECIFIC-EQUICATGRY  = IT_TREC-EQTYP.	"Equipment category.

**** Installation Date for Equipment
  DATA_INSTALL-FUNCLOC	= IT_TREC-TPLNR.	"Functional location
*label
  DATA_INSTALL-SUPEQUI	= IT_TREC-HEQUI. 	"Superior Equipment
  DATA_INSTALL-INST_POS	= IT_TREC-POSNR.  "Equipment position

ENDFORM.                    " fill_parameters
*&---------------------------------------------------------------------*
*&      Form  UPDATE_TABLES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_TABLES.
*MODIFY ZTPM_MONBD FROM TABLE IT_T_REC .

ENDFORM.                    " UPDATE_TABLES

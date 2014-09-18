************************************************************************
* Created by  : Myoungho Park
* Created on  : 2003.08.04.
* Description : Create Functional Location
*
*
* Modification Log
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT ZCPM01R_FUNCLOC_CREATE NO STANDARD PAGE HEADING
                             MESSAGE-ID ZMPM LINE-SIZE 200.

FIELD-SYMBOLS : <FS>.

*** For File Select
DATA: WA_FNAME LIKE RLGRAP-FILENAME,
      WA_RC LIKE SY-SUBRC.

*** For Cerate Functional Location BAPI
DATA: DATA_GENERAL_EXP LIKE BAPI_ITOB,
      DATA_SPECIFIC_EXP LIKE BAPI_ITOB_FL_ONLY,
      RETURN LIKE BAPIRET2,
      EXTERNAL_NUMBER LIKE BAPI_ITOB_PARMS-FUNCLOC,
      LABELING_SYSTEM LIKE BAPI_ITOB_PARMS-LABEL_SYST,
      DATA_GENERAL LIKE BAPI_ITOB,
      DATA_SPECIFIC LIKE BAPI_ITOB_FL_ONLY.

***  Upload Data Fields
DATA: BEGIN OF IT_TREC OCCURS 0,
*** Initial
        STRNO LIKE IFLOS-STRNO,   	"Functional location label
        TPLKZ LIKE RILO0-TPLKZ,	
                   "Functional location structure indicator
        FLTYP LIKE IFLO-FLTYP,	"Functional location category
*** General
*        EQART LIKE ITOB-EQART,      "Type of Technical Object
        INBDT LIKE ITOB-INBDT,      "Start-up Date
*** Location
        SWERK LIKE ITOB-SWERK,	"Maintenance plant
        STORT LIKE ITOB-STORT,	"Asset location
        MSGRP LIKE ITOB-MSGRP,	"Room
        BEBER LIKE ITOB-BEBER,	"Plant section
        ARBPL LIKE ITOBATTR-ARBPL, 	"Object ID of PP work center
        BUKRS LIKE ITOB-BUKRS,	"Company Code
        GSBER LIKE ITOB-GSBER,	"Business Area
        KOSTL LIKE ITOB-KOSTL,	"Cost Center
        IWERK LIKE ITOB-IWERK,      "Maintenance Planning Plant
        INGRP LIKE ITOB-INGRP,	"Planner Group for Plant Maintenance
        GEWRK LIKE ITOBATTR-GEWRK,	"Object ID of the Work Center
        RBNR  LIKE ITOB-RBNR, 	"Catalog Profile
*** Structure
        TPLMA LIKE IFLO-TPLMA,	"Superior functional location
        PLTXT LIKE IFLO-PLTXT, 	"Description of technical object
        POSNR LIKE ITOB-POSNR,
                        "Position in superior technical object
        IEQUI LIKE ITOBATTR-IEQUI,	
           "Installation of equipment allowed at the functional location
        EINZL LIKE ITOBATTR-EINZL, 	
           "Single equipment installation at functional location
      END OF IT_TREC.

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

  PERFORM EXCEL_FILE_UPLOAD  TABLES   IT_TREC
                             USING    P_FILE
                                      WA_RC.

  LOOP AT IT_TREC.
    PERFORM CREATE_FUNCLOC.
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
FORM EXCEL_FILE_UPLOAD TABLES   IT_TREC
                       USING    P_FNAME
                                P_RC.

  DATA : IT_INTERN TYPE  KCDE_CELLS OCCURS 0 WITH HEADER LINE.
  DATA : LV_INDEX TYPE I.
  DATA : LV_START_COL TYPE I VALUE '1',
         LV_START_ROW TYPE I VALUE '1',
         LV_END_COL   TYPE I VALUE '256',
         LV_END_ROW   TYPE I VALUE '65536'.

*** Data transfer from PC files to internal Table.
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

* mapping cells to fields
  MOVE : SY-SUBRC TO P_RC.

  CHECK NOT IT_INTERN[] IS INITIAL.

*-- Delete Header line: row from 1 to 2
  DELETE IT_INTERN WHERE ROW LE 2.

  SORT IT_INTERN BY ROW COL.

  LOOP AT IT_INTERN.
    MOVE : IT_INTERN-COL TO LV_INDEX.
    ASSIGN COMPONENT LV_INDEX OF STRUCTURE IT_TREC TO <FS>.
    MOVE : IT_INTERN-VALUE TO <FS>.
    AT END OF ROW.
      APPEND IT_TREC.
      CLEAR IT_TREC.
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
**** Functional location label
  EXTERNAL_NUMBER = IT_TREC-STRNO.

****  DATA_GENERAL Feilds Mapping
*  DATA_GENERAL-OBJECTTYPE =	IT_TREC-EQART.
  DATA_GENERAL-START_FROM = 	IT_TREC-INBDT.
  DATA_GENERAL-PLANPLANT  =	IT_TREC-IWERK.
  DATA_GENERAL-PLANGROUP  =   IT_TREC-INGRP.
  DATA_GENERAL-CATPROFILE =	IT_TREC-RBNR.
  DATA_GENERAL-WORK_CTR   =	IT_TREC-GEWRK.
  DATA_GENERAL-DESCRIPT   =	IT_TREC-PLTXT.
  DATA_GENERAL-MAINTPLANT =	IT_TREC-SWERK.
  DATA_GENERAL-MAINTLOC	  =   IT_TREC-STORT.
  DATA_GENERAL-MAINTROOM  =   IT_TREC-MSGRP.
  DATA_GENERAL-PLSECTN	  =   IT_TREC-BEBER.
  DATA_GENERAL-PP_WKCTR	  =   IT_TREC-ARBPL.
  DATA_GENERAL-BUS_AREA	  =   IT_TREC-GSBER.
  DATA_GENERAL-COSTCENTER =   IT_TREC-KOSTL.
  DATA_GENERAL-COMP_CODE  =   IT_TREC-BUKRS.

*** DATA_SPECIFIC Fields Mapping
  DATA_SPECIFIC-STRIND	  = IT_TREC-TPLKZ.
  DATA_SPECIFIC-CATEGORY  = IT_TREC-FLTYP.
  DATA_SPECIFIC-SUPFLOC	  = IT_TREC-TPLMA.
  DATA_SPECIFIC-EQINSTALL = IT_TREC-IEQUI.
  DATA_SPECIFIC-EQSINGLE  = IT_TREC-EINZL.
  DATA_SPECIFIC-POSNR     = IT_TREC-POSNR.

**** Create Functional Location using BAPI..
  CALL FUNCTION 'BAPI_FUNCLOC_CREATE'
       EXPORTING
            EXTERNAL_NUMBER   = EXTERNAL_NUMBER
            LABELING_SYSTEM   = LABELING_SYSTEM
            DATA_GENERAL      = DATA_GENERAL
            DATA_SPECIFIC     = DATA_SPECIFIC
       IMPORTING
*          FUNCTLOCATION     =
            DATA_GENERAL_EXP  = DATA_GENERAL_EXP
            DATA_SPECIFIC_EXP = DATA_SPECIFIC_EXP
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

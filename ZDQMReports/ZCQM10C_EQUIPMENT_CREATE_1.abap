************************************************************************
* Program Name      : ZCQM10C_EQUIPMENT_CREATE
* Author            : SeungLyong, Lee
* Creation Date     : 2003.08.06.
* Specifications By : SeungLyong, Lee
* Pattern           : 2.2 Call BAPI Function + 2.3 Call Transaction
* Development Request No : UD1K901760
* Addl Documentation:
* Description       : Equipment Master Upload
*   - Using BAPI Function : BAPI_EQUI_CREATE
*    -> If the BAPI runs successfully, table RETURN contains
*       no messages of type 'E'.
*   - <Caution>
*     This BAPI does not change the database. To change the database,
*     call BAPI BapiService.TransactionCommit afterwards.
*      -> 'BAPI_TRANSACTION_COMMIT' or 'BAPI_TRANSACTION_ROLLBACK'
*
*  => For Asset assignment, Delete BAPI fuction logic and use BDC 'IE01'
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 10/24/2004 SL.LEE                    modify User interface
* 12/15/2004 WS.KIM                    Use BDC for Equip creation
*
************************************************************************

REPORT ZCQM10C_EQUIPMENT_CREATE_1   NO STANDARD PAGE HEADING
                                  MESSAGE-ID ZMQM LINE-SIZE 200.

TYPE-POOLS: VRM.

*-- Tables :
TABLES : EQUI. "/Equipment master

**-- Bapi return message table : LOG TABLE
DATA: IT_RETURN	  LIKE BAPIRET2 OCCURS 0 WITH HEADER LINE.

DATA   BEGIN OF IT_MSG OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA   END OF IT_MSG.


* BDC Tables
DATA : BEGIN OF BDC_TAB OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA : END OF BDC_TAB.

****For Error Message
DATA: WA_RETURN LIKE BAPIRET2 .

*** Uploading Data Fields
DATA: BEGIN OF IT_EQUI OCCURS 0,
       EQTYP  TYPE EQTYP, "Equipment category
       EQUNR  TYPE EQUNR, "/Equipment No.=> Blank: Internal
       SHTXT  TYPE KTX01, "/Description of technical object
       EQART  TYPE EQART, "/Type of Technical Object
       BRGEW(20),"  TYPE obj_weight, "/Weight of object
       GEWEI  TYPE WEIGHT_UNIT, "/Unit of weight
       GROES  TYPE GROSS, "/Size/dimension
       INVNR  TYPE INVNR, "/Inventory number
       INBDT  TYPE ILOM_DATAB, "/Start-up Date of the Technical Object
       ANSWT(20),  "TYPE answt, "/Acquisition value
       WAERS  TYPE WAERS, "/Currency Key
       ANSDT  TYPE ANDTI, "/Acquisition date
       HERST  TYPE HERST, "/Manufacturer of asset
       HERLD  TYPE HERLD, "/Country of manufacture
       TYPBZ  TYPE TYPBZ, "/Manufacturer model number
       BAUJJ  TYPE BAUJJ, "/Year of construction
       BAUMM  TYPE BAUMM, "/Month of construction
       SERGE  TYPE SERGE, "/Manufacturer serial number
       SWERK  TYPE SWERK, "/Maintenance plant
       ARBPL  TYPE ARBPL, "/Work Center
       ABCKZ  TYPE ABCKZ, "/ABC indicator for technical object
       BUKRS  TYPE BUKRS, "/Company Code
       ANLNR  TYPE ANLN1, "/Main asset number
       ANLUN  TYPE ANLN2, "/Asset sub-number
       KOSTL  TYPE KOSTL, "/Cost Center
       GEWRK  TYPE GEWRK, "/PP work center

*-- Maintenace Address window data for BDC : Start
       TITLE_MEDI TYPE AD_TITLETX,  "/Text Title
       NAME1      TYPE AD_NAME1,  "/Name of address mainternace window
       STREET     TYPE AD_STREET, "/Street
*       HOUSE_NUM1 type AD_HSNM1,  "/House No.
       POST_CODE1 TYPE AD_PSTCD1, "/Postal code
       CITY1      TYPE AD_CITY1,  "/City
       COUNTRY    TYPE LAND1,     "/Country
       REGION     TYPE REGIO,     "/Region
       PO_BOX     TYPE AD_POBX,   "/PO Box
       POST_CODE2 TYPE AD_PSTCD2, "/Postab code
       POST_CODE3 TYPE AD_PSTCD3, "/Company postal code
       LANGU      TYPE SPRAS,     "/Language
       TEL_NUMBER TYPE AD_TLNMBR1, "/Telephone
       TEL_EXTENS TYPE AD_TLXTNS1, "/First telephone no.: extension
       FAX_NUMBER TYPE AD_FXNMBR1, "/First fax no.: dialling code+number
       FAX_EXTENS TYPE AD_FXXTNS1, "/First fax no.: extension
       SMTP_ADDR  TYPE AD_SMTPADR, "/Internet mail (SMTP) address
*-- Maintenace Address window data for BDC : End

*-- Class Overview data for BDC : Start
       CLASS      TYPE KLASSE_D,  "/Class
       ATNAM      TYPE ATNAM,     "/Characteristic Name
       ATWRT      TYPE ATWRT,     "/Characteristic value
*-- Class Overview data for BDC : End
*-- Partners data for BDC : Start
       PARVW_AB   TYPE PARVW,   "/Department resp. Partner Function
       PARNR_AB   TYPE PARNR,   "/Department resp.
       PARVW_VW   TYPE PARVW,   "/Person respons. Partner Function
       PARNR_VW   TYPE PARNR,   "/Person respons.
*-- Partners data for BDC : End
      END OF IT_EQUI.


*//Data(Global Fileds) ;(WA_)  == Local? ?? ; (LO_)
*                 Flag ;(FL_), Counter;(CT_), Temp;(TMP_)
DATA : WA_REPID LIKE SYST-REPID,
       WA_DYNNR LIKE SYST-DYNNR.

*/ Result variables
DATA : WA_SUCCESS_CNT TYPE I,
       WA_FAILED_CNT  TYPE I,
       WA_TOTAL_ENTR  TYPE I.

**-- Constants
CONSTANTS : C_MARK  TYPE C VALUE 'X'.

**-- List box variables
DATA: WA_NAME  TYPE VRM_ID,
      IT_LIST  TYPE VRM_VALUES,
      WA_VALUE LIKE LINE OF IT_LIST.
DATA : P_NUMBER TYPE EQUNR.
DATA   BEGIN OF LT_MSG OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA   END OF LT_MSG.

*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
*#### Selection Screen ####
SELECTION-SCREEN BEGIN OF BLOCK BLK WITH FRAME  TITLE TEXT-T01.
PARAMETERS : P_FILE   LIKE RLGRAP-FILENAME   OBLIGATORY.
*SELECTION-SCREEN ULINE.
*PARAMETERS : P_TEST  TYPE BAPIFLAG DEFAULT 'X' AS LISTBOX
*                                             VISIBLE LENGTH 14.

SELECTION-SCREEN ULINE.
PARAMETERS : P_MODE   TYPE TB_BDCMODE DEFAULT 'N' AS LISTBOX
                                         VISIBLE LENGTH 25.
SELECTION-SCREEN END OF BLOCK BLK .


*//-- Remarked  : because using BDC
*AT SELECTION-SCREEN OUTPUT.
*  PERFORM SCREEN_MODIFY.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.

  PERFORM GET_SELECT_EXCEL_FILE  USING P_FILE.



START-OF-SELECTION.
  PERFORM F_EXCEL_UPLOAD  TABLES   IT_EQUI
                          USING    P_FILE.
  IF IT_EQUI[] IS INITIAL.
    MESSAGE E000(ZMQM)
            WITH 'Entries not founded. Please check File'(E15).
    EXIT.
  ENDIF.

END-OF-SELECTION.
*//-- Remarked  : because using BDC
*  PERFORM EXECUTE_BAPI_FUNC. "/execute BAPI Function

  PERFORM BDC_PROCESS.


  PERFORM WRITE_LOG_MESSAGE.

TOP-OF-PAGE.

  PERFORM WRITE_HEADER.

END-OF-PAGE.

*&------------------------------------------------------------------*
*&      Form  get_select_Excel_file
*&------------------------------------------------------------------*
FORM GET_SELECT_EXCEL_FILE   USING PW_FILE.
  WA_REPID = SY-REPID.
  WA_DYNNR = SY-DYNNR.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
       EXPORTING
            PROGRAM_NAME  = WA_REPID
            DYNPRO_NUMBER = WA_DYNNR
            FIELD_NAME    = ' '
            STATIC        = ' '
            MASK          = ' '
       CHANGING
            FILE_NAME     = PW_FILE
       EXCEPTIONS
            MASK_TOO_LONG = 1
            OTHERS        = 2.
  IF SY-SUBRC < 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " get_select_Excel_file
*&------------------------------------------------------------------*
*&      Form  EXECUTE_BAPI_FUNC
*&------------------------------------------------------------------*
FORM EXECUTE_BAPI_FUNC.

**** For Cerate Functional Location BAPI
*  DATA: LW_EXTERNAL_NUMBER	LIKE	BAPI_ITOB_PARMS-EQUIPMENT,
*        LW_DATA_GENERAL	        LIKE	BAPI_ITOB,
*        LW_DATA_SPECIFIC	LIKE	BAPI_ITOB_EQ_ONLY,
*        LW_VALID_DATE	        LIKE	BAPI_ITOB_PARMS-INST_DATE,
*        LW_DATA_INSTALL	        LIKE	BAPI_ITOB_EQ_INSTALL,
*        LW_RETURN               LIKE    BAPIRET2.
*
*  DATA : LW_SUBRC LIKE SY-SUBRC.
*  DATA : LW_RETURN2 LIKE BAPIRET2.
*
*  DATA : LW_EQUI_INDEX LIKE SY-TABIX,
*         LW_PROG_TEXT(132) TYPE C.
*
*  DESCRIBE TABLE IT_EQUI LINES WA_TOTAL_ENTR."/Total Equipment
*
*  LOOP AT IT_EQUI.
*    LW_EQUI_INDEX = SY-TABIX.
*
*    CLEAR : LW_EXTERNAL_NUMBER,  LW_DATA_GENERAL,  LW_DATA_SPECIFIC,
*            LW_VALID_DATE,   LW_DATA_INSTALL,   LW_RETURN.
**///-- Fill Interface parameter for BAPI Function
*
***** Number of Equipment to be Created (Initial => Internal Assignment)
*    MOVE :
*     IT_EQUI-EQTYP TO LW_DATA_SPECIFIC-EQUICATGRY, "/Equipment category
*     IT_EQUI-EQART TO LW_DATA_GENERAL-OBJECTTYPE,  "/Type of Tech
*Object
*     IT_EQUI-INVNR TO LW_DATA_GENERAL-INVENTORY, "/Inventory number
*     IT_EQUI-GROES TO LW_DATA_GENERAL-OBJ_SIZE, "/Size/dimension
*     IT_EQUI-BRGEW TO LW_DATA_GENERAL-OBJ_WEIGHT, "/Weight of object
*     IT_EQUI-GEWEI TO LW_DATA_GENERAL-UNIT_OF_WT, "/Unit of weight
*     IT_EQUI-ANSDT TO LW_DATA_GENERAL-ACQDATE, "/Acquisition date
*     IT_EQUI-ANSWT TO LW_DATA_GENERAL-ACQUISVAL, "/Acquisition value
*     IT_EQUI-WAERS TO LW_DATA_GENERAL-CURRENCY, "/Currency Key
*     IT_EQUI-HERST TO LW_DATA_GENERAL-MANFACTURE, "/Man. of asset
*     IT_EQUI-HERLD TO LW_DATA_GENERAL-MANCOUNTRY, "/Country of man.
*     IT_EQUI-SERGE TO LW_DATA_GENERAL-MANSERNO, "/Man serial number
*     IT_EQUI-TYPBZ TO LW_DATA_GENERAL-MANMODEL, "/Man. model number
*     IT_EQUI-BAUJJ TO LW_DATA_GENERAL-CONSTYEAR, "/Year of construction
*     IT_EQUI-BAUMM TO LW_DATA_GENERAL-CONSTMONTH, "/Month of const.
*     IT_EQUI-INBDT TO LW_DATA_GENERAL-START_FROM. "/Start-up Date of
*the
*    "  Technical Object
**    MOVE :
**     IT_EQUI-GEWRK TO LW_DATA_GENERAL-PP_WKCTR "/Obj.ID of PP W.C
**     IT_EQUI-ARBPL TO LW_DATA_GENERAL-WORK_CTR "/Object ID of the W.C
*    PERFORM GET_WORKCENTER_OBJID   USING : IT_EQUI-ARBPL
*                                           LW_DATA_GENERAL-WORK_CTR,
*                                           IT_EQUI-GEWRK
*                                           LW_DATA_GENERAL-PP_WKCTR.
*
*
*    MOVE :
*     IT_EQUI-SHTXT TO LW_DATA_GENERAL-DESCRIPT, "/Descript of tech. obj
*.
*     IT_EQUI-ABCKZ TO LW_DATA_GENERAL-ABCINDIC, "/ABC indicator
*     IT_EQUI-SWERK TO LW_DATA_GENERAL-MAINTPLANT. "/Maintenance plant
*
**    MOVE :
**    IT_EQUI-KOSTL TO LW_DATA_GENERAL-COSTCENTER.  "/Cost Center
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*         EXPORTING
*              INPUT  = IT_EQUI-KOSTL
*         IMPORTING
*              OUTPUT = LW_DATA_GENERAL-COSTCENTER.
*
*
*
*    MOVE :
*      IT_EQUI-BUKRS TO LW_DATA_GENERAL-COMP_CODE, "/Company Code
*      IT_EQUI-ANLNR TO LW_DATA_GENERAL-ASSET_NO, "/Main asset number
*      IT_EQUI-ANLUN TO LW_DATA_GENERAL-SUB_NUMBER, "/Asset sub-number
*
*      SY-DATUM TO LW_DATA_GENERAL-READ_CRDAT,
*      SY-UNAME TO LW_DATA_GENERAL-READ_CRNAM.
*
*
*
**-- Execute BAPI Function.
**    CALL FUNCTION 'BAPI_EQUI_CREATE'
**         EXPORTING
**              external_number   = lw_external_number
**              data_general      = lw_data_general
**              data_specific     = lw_data_specific
**              valid_date        = it_equi-inbdt
**              data_install      = lw_data_install
**         IMPORTING
**              equipment         = lw_external_number
**              data_general_exp  = lw_data_general
**              data_specific_exp = lw_data_specific
**              return            = lw_return.
*
*
**-/// After BAPI execution////
*    CLEAR IT_RETURN.
***      insert division message for each Class Upload
*    CONCATENATE IT_EQUI-SHTXT
*                ':'
*                   INTO  IT_RETURN-MESSAGE SEPARATED BY SPACE.
*
*
**/--    Error Check and Log Message Backup.
**   -> If the BAPI runs successfully, table RETURN contains no messages
**      of type 'E'.
*
*    IF SY-SUBRC NE 0.
*      PERFORM BAPI_SERVICE_ROLLBACK.
*      MESSAGE E000(ZMQM) WITH 'Error founded in BAPI Processing. '(E02)
*.
*      CONCATENATE IT_RETURN-MESSAGE
*                  '=>'
*                  TEXT-E02
*      INTO  IT_RETURN-MESSAGE SEPARATED BY SPACE.
*      WA_FAILED_CNT = WA_FAILED_CNT + 1.
*    ENDIF.
*
*
*    IF LW_RETURN-TYPE  = 'E' OR LW_RETURN-TYPE  = 'A'.
*      PERFORM BAPI_SERVICE_ROLLBACK.
*      CONCATENATE IT_RETURN-MESSAGE
*                   '=> Failed'
*                     INTO  IT_RETURN-MESSAGE SEPARATED BY SPACE.
*      WA_FAILED_CNT = WA_FAILED_CNT + 1.
*    ELSE.
*
**--- Maintenance Address and Class /Characteristic view
*      PERFORM BAPI_SERVICE_COMMIT.
*
*      CONCATENATE  LW_EXTERNAL_NUMBER
*                   '-'
*                   IT_EQUI-SHTXT
*                   INTO LW_PROG_TEXT  SEPARATED BY SPACE.
*
*      PERFORM DISPLAY_PROGRESS_BAR  USING  LW_PROG_TEXT
*                                           LW_EQUI_INDEX
*                                           WA_TOTAL_ENTR.
*
*      CLEAR : LW_SUBRC, LW_RETURN2.
*      MOVE : LW_EXTERNAL_NUMBER TO IT_EQUI-EQUNR.
*
**      PERFORM ADD_DATA_EQUIP_PARVW_MA  USING LW_EXTERNAL_NUMBER
**                                             IT_EQUI
**                                             LW_SUBRC
**                                             LW_RETURN2.
*
*      IF LW_SUBRC IS INITIAL.
**-----------------------------------------------------------
*        CONCATENATE IT_RETURN-MESSAGE
*                     '=> Success '
*                     LW_EXTERNAL_NUMBER
*                       INTO  IT_RETURN-MESSAGE SEPARATED BY SPACE.
*        WA_SUCCESS_CNT = WA_SUCCESS_CNT + 1.
*
*      ELSE.
*
*        PERFORM BAPI_SERVICE_ROLLBACK.
*        CONCATENATE IT_RETURN-MESSAGE
*             '=> Failed.'
*                       INTO  IT_RETURN-MESSAGE SEPARATED BY SPACE.
*        WA_FAILED_CNT = WA_FAILED_CNT + 1.
*
*      ENDIF.
*    ENDIF.
*
*    APPEND IT_RETURN.
*    APPEND LW_RETURN TO IT_RETURN.
*    APPEND LW_RETURN2 TO IT_RETURN.
*    APPEND INITIAL LINE TO  IT_RETURN.
*
*  ENDLOOP.

ENDFORM.                    " EXECUTE_BAPI_FUNC
*&------------------------------------------------------------------*
*&      Form  F_EXCEL_UPLOAD
*&------------------------------------------------------------------*
FORM F_EXCEL_UPLOAD  TABLES   P_TABLE
                      USING   P_FILENAME  LIKE RLGRAP-FILENAME.



  DATA : LT_ITAB TYPE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE.
  DATA : LW_INDEX LIKE SY-TABIX.
  DATA : LW_START_COL TYPE I VALUE '1',
         LW_START_ROW TYPE I VALUE '1',
         LW_END_COL   TYPE I VALUE '256',
         LW_END_ROW   TYPE I VALUE '65536'.
  FIELD-SYMBOLS : <LW_FS>.

  DATA : LW_FIELD_TYPE.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
       EXPORTING
            FILENAME                = P_FILENAME
            I_BEGIN_COL             = LW_START_COL
            I_BEGIN_ROW             = LW_START_ROW
            I_END_COL               = LW_END_COL
            I_END_ROW               = LW_END_ROW
       TABLES
            INTERN                  = LT_ITAB
       EXCEPTIONS
            INCONSISTENT_PARAMETERS = 1
            UPLOAD_OLE              = 2
            OTHERS                  = 3.


  IF SY-SUBRC NE 0.
    MESSAGE E000(ZMQM) WITH 'File Upload Failed !'(E10).
    STOP.
  ENDIF.


  CHECK NOT LT_ITAB[] IS INITIAL.

*-- Delete Header line: row from 1 to 2
  DELETE LT_ITAB WHERE ROW LE 2.


  SORT LT_ITAB BY ROW COL.
  REFRESH P_TABLE.

  LOOP AT LT_ITAB.
    MOVE : LT_ITAB-COL TO LW_INDEX.
    ASSIGN COMPONENT LW_INDEX OF STRUCTURE P_TABLE TO <LW_FS>.

    DESCRIBE FIELD <LW_FS> TYPE LW_FIELD_TYPE.

    IF LW_FIELD_TYPE = 'D'.  "'MM/DD/YYYY" " Date type Conversion
      CONCATENATE LT_ITAB-VALUE+6(4)    "YEAR  (YYYY)
                  LT_ITAB-VALUE+0(2)    "MONTH (MM)
                  LT_ITAB-VALUE+3(2)    "DAY   (DD)
                              INTO <LW_FS>.
    ELSE.
      MOVE : LT_ITAB-VALUE TO <LW_FS>.
    ENDIF.

    AT END OF ROW.
      APPEND P_TABLE.
      CLEAR P_TABLE.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " F_EXCEL_UPLOAD
*&------------------------------------------------------------------*
*&      Form  WRITE_HEADER
*&------------------------------------------------------------------*
FORM WRITE_HEADER.
  WRITE :
    'Equipment Master Uploading  Log'(H01).
  ULINE.
  WRITE : 'Total of Equipment Master : '(H02) ,
          WA_TOTAL_ENTR.
  NEW-LINE.
  WRITE : 'Success entries           : '(H03) ,
          WA_SUCCESS_CNT.
  NEW-LINE.
  WRITE : 'Failed entries            : '(H04),
          WA_FAILED_CNT.
  ULINE.
ENDFORM.                    " WRITE_HEADER
*&-----------------------------------------------------------------*
*&      Form  WRITE_LOG_MESSAGE
*&-----------------------------------------------------------------*
FORM WRITE_LOG_MESSAGE.

  LOOP AT IT_RETURN WHERE TYPE = ' '.
    WRITE IT_RETURN-MESSAGE.
    NEW-LINE.
  ENDLOOP.

  ULINE.

  LOOP AT IT_RETURN.
    IF    IT_RETURN-TYPE IS INITIAL AND
      NOT IT_RETURN-MESSAGE IS INITIAL.
      NEW-LINE.
    ENDIF.

    WRITE : IT_RETURN-TYPE,
            IT_RETURN-ID,
            IT_RETURN-NUMBER,
  AT (150)  IT_RETURN-MESSAGE,
*            IT_RETURN-LOG_NO,
*            IT_RETURN-LOG_MSG_NO,
*            IT_RETURN-MESSAGE_V1,
*            IT_RETURN-MESSAGE_V2,
*            IT_RETURN-MESSAGE_V3,
*            IT_RETURN-MESSAGE_V4,
            IT_RETURN-PARAMETER,
*            IT_RETURN-ROW,
            IT_RETURN-FIELD.
*            IT_RETURN-SYSTEM.
    NEW-LINE.
  ENDLOOP.
*  LOOP AT lt_msg.
*    WRITE : / lt_msg-MSGV1.
*  ENDLOOP.
ENDFORM.                    " WRITE_LOG_MESSAGE

*&------------------------------------------------------------------*
*&      Form  BAPI_SERVICE_ROLLBACK
*&------------------------------------------------------------------*
FORM BAPI_SERVICE_ROLLBACK.
  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
* IMPORTING
*   RETURN        =     .

ENDFORM.                    " BAPI_SERVICE_ROLLBACK
*&------------------------------------------------------------------*
*&      Form  BAPI_SERVICE_COMMIT
*&------------------------------------------------------------------*
FORM BAPI_SERVICE_COMMIT.
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
       EXPORTING
            WAIT = C_MARK.
* IMPORTING
*   RETURN        = .

ENDFORM.                    " BAPI_SERVICE_COMMIT
*&---------------------------------------------------------------------*
*&      Form  DYNPRO
*&---------------------------------------------------------------------*
FORM DYNPRO USING DYNBEGIN NAME VALUE.
  IF DYNBEGIN = 'X'.
    CLEAR BDC_TAB.
    MOVE : NAME  TO BDC_TAB-PROGRAM,
           VALUE TO BDC_TAB-DYNPRO,
           'X'   TO BDC_TAB-DYNBEGIN.
    APPEND BDC_TAB.
  ELSE.
    CLEAR BDC_TAB.
    MOVE : NAME  TO BDC_TAB-FNAM,
           VALUE TO BDC_TAB-FVAL.
    APPEND BDC_TAB.
  ENDIF.
ENDFORM.                    "DYNPRO
*&------------------------------------------------------------------*
*&      Form  GET_WORKCENTER_OBJID
*&-----------------------------------------------------------------*
FORM GET_WORKCENTER_OBJID USING    P_ARBPL
                                   P_OBJID.

  SELECT SINGLE OBJID INTO P_OBJID
     FROM CRHD
       WHERE OBJTY = 'A'
         AND ARBPL = P_ARBPL.

ENDFORM.                    " GET_WORKCENTER_OBJID
*&---------------------------------------------------------------------*
*&      Form  GET_BAPI_RETURN_MSG
*&---------------------------------------------------------------------*
FORM GET_BAPI_RETURN_MSG USING    P_MSGTYP
                                  P_MSGID
                                  P_MSGNR
                                  P_MSGV1
                                  P_MSGV2
                                  P_MSGV3
                                  P_MSGV4
                         CHANGING PW_RETURN2 STRUCTURE BAPIRET2.

  DATA : LW_TYPE	  LIKE	BAPIRETURN-TYPE,
         LW_CL	  LIKE	SY-MSGID,
         LW_NUMBER  LIKE	SY-MSGNO,
         LW_PAR1	  LIKE	SY-MSGV1,
         LW_PAR2	  LIKE	SY-MSGV2,
         LW_PAR3	  LIKE	SY-MSGV3,
         LW_PAR4	  LIKE	SY-MSGV4.


  LW_TYPE   = P_MSGTYP.
  LW_CL     = P_MSGID.
  LW_NUMBER = P_MSGNR.
  LW_PAR1   = P_MSGV1.
  LW_PAR2   = P_MSGV2.
  LW_PAR3   = P_MSGV3.
  LW_PAR4   = P_MSGV4.

  CALL FUNCTION 'BALW_BAPIRETURN_GET2'
       EXPORTING
            TYPE   = LW_TYPE
            CL     = LW_CL
            NUMBER = LW_NUMBER
            PAR1   = LW_PAR1
            PAR2   = LW_PAR2
            PAR3   = LW_PAR3
            PAR4   = LW_PAR4
       IMPORTING
            RETURN = PW_RETURN2.

ENDFORM.                    " GET_BAPI_RETURN_MSG
*&-----------------------------------------------------------------*
*&      Form  DISPLAY_PROGRESS_BAR
*&-----------------------------------------------------------------*
FORM DISPLAY_PROGRESS_BAR USING P_TEXT
                                P_CUR
                                P_TOT.

  DATA : LW_PERCENTAGE TYPE I.
  IF P_TOT IS INITIAL.
    LW_PERCENTAGE = 50.
  ELSE.
    LW_PERCENTAGE = P_CUR / P_TOT * 100.
  ENDIF.
  CALL FUNCTION 'TB_PROGRESS_INDICATOR'
       EXPORTING
            PERCENTAGE = LW_PERCENTAGE
            TEXT       = P_TEXT.

ENDFORM.                    " DISPLAY_PROGRESS_BAR
*&---------------------------------------------------------------------*
*&      Form  bdc_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BDC_PROCESS.
  LOOP AT IT_EQUI.
    CLEAR P_NUMBER.
    PERFORM BDC_HEAD USING IT_EQUI
                     CHANGING P_NUMBER.

    CHECK NOT P_NUMBER IS INITIAL.

    PERFORM ADD_DATA_EQUIP_PARVW_MA  USING P_NUMBER
                                           IT_EQUI
                                           SY-SUBRC.
  ENDLOOP.                                           "LW_RETURN2.

ENDFORM.                    " bdc_process
*&---------------------------------------------------------------------*
*&      Form  SCREEN_MODIFY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SCREEN_MODIFY.
  WA_NAME = 'P_TEST'.
  WA_VALUE-KEY = 'X'. WA_VALUE-TEXT = 'Simulation'.
  APPEND WA_VALUE TO IT_LIST.
  WA_VALUE-KEY = ' '. WA_VALUE-TEXT = 'Write'.
  APPEND WA_VALUE TO IT_LIST.
  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            ID     = WA_NAME
            VALUES = IT_LIST.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  bdc_head
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EQUI  text
*      <--P_P_NUMBER  text
*----------------------------------------------------------------------*
FORM BDC_HEAD USING    P_EQUI
              CHANGING PP_NUMBER.

  DATA   BEGIN OF LT_MSG OCCURS 0.
          INCLUDE STRUCTURE BDCMSGCOLL.
  DATA   END OF LT_MSG.
  DATA : G_DATE(10).

  REFRESH : BDC_TAB, LT_MSG.

*  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
*       EXPORTING
*            input  = sy-datum
*       IMPORTING
*            output = g_date.


  CONCATENATE SY-DATUM+4(2) SY-DATUM+6(2) SY-DATUM(4) INTO G_DATE.

  PERFORM DYNPRO  USING:
       'X'   'SAPMIEQ0'          '0100',
       ' '   'BDC_OKCODE'        '/00',
       ' '   'RM63E-DATSL'       G_DATE,
       ' '   'RM63E-EQtyp'       'Z',    "/Calibration Equipment type

       'X'   'SAPMIEQ0'          '0101',                    " 1 VIEW
       ' '   'BDC_OKCODE'        '=T\02',
       ' '   'ITOB-BRGEW'        IT_EQUI-BRGEW,
       ' '   'ITOB-EQART'        IT_EQUI-EQART,
       ' '   'ITOB-GEWEI'        IT_EQUI-GEWEI,
       ' '   'ITOB-EQART'        IT_EQUI-EQART,
       ' '   'ITOB-ANSWT'        IT_EQUI-ANSWT,
       ' '   'ITOB-WAERS'        IT_EQUI-WAERS,
       ' '   'ITOB-HERST'        IT_EQUI-HERST,
       ' '   'ITOB-HERLD'        IT_EQUI-HERLD,
       ' '   'ITOB-INVNR'        IT_EQUI-INVNR,
       ' '   'ITOB-INBDT'        G_DATE,
       ' '   'ITOB-ANSDT'        G_DATE,
       ' '   'ITOB-TYPBZ'        IT_EQUI-TYPBZ,
       ' '   'ITOB-BAUJJ'        IT_EQUI-BAUJJ,
       ' '   'ITOB-BAUMM'        IT_EQUI-BAUMM,
       ' '   'ITOB-SERGE'        IT_EQUI-SERGE,
       ' '   'ITOB-GROES'        IT_EQUI-GROES,
       ' '   'ITOB-SHTXT'        IT_EQUI-SHTXT,
       ' '   'ITOB-DATAB'        G_DATE,

       'X'   'SAPMIEQ0'          '0101',
       ' '   'BDC_OKCODE'        '=T\03',
       ' '   'ITOB-SWERK'        IT_EQUI-SWERK,
       ' '   'ITOBATTR-ARBPL'    IT_EQUI-ARBPL,
       ' '   'ITOB-ABCKZ'        IT_EQUI-ABCKZ,
       ' '   'ITOB-SHTXT'        IT_EQUI-SHTXT,
       ' '   'ITOB-DATAB'        G_DATE.

*-- Add 27/01/2004
*-- Click Partner
  PERFORM DYNPRO  USING:
      'X'   'SAPMIEQ0'          '0101',
      ' '   'BDC_OKCODE'        '=PART',
      ' '   'ITOB-BUKRS'        IT_EQUI-BUKRS,
      ' '   'ITOB-KOSTL'        IT_EQUI-KOSTL,
*       ' '   'ITOB-IWERK'        it_equi-iwerk,
      ' '   'ITOBATTR-GEWRK'        IT_EQUI-GEWRK,
*       ' '   'ITOB-WERGW'        it_equi-wergw,
      ' '   'ITOB-SHTXT'        IT_EQUI-SHTXT,
      ' '   'ITOB-DATAB'        G_DATE.

*-- Change Equipment Partner
  PERFORM DYNPRO  USING:
      'X'   'SAPLIPAR'          '0200',
      ' '   'BDC_OKCODE'        '=BACK',
*      ' '   'IHPA-PARVW(01)'      it_equi-parvw_ab, "/
      ' '   'IHPA-PARNR(01)'      IT_EQUI-PARNR_AB, "/
*      ' '   'IHPA-PARVW(02)'      it_equi-parvw_vw, "/
      ' '   'IHPA-PARNR(02)'      IT_EQUI-PARNR_VW. "/
*-- Add 27/01/2004

*---- Save
  PERFORM DYNPRO  USING:
      'X'   'SAPMIEQ0'          '0101',
      ' '   'BDC_OKCODE'        '=BU'.


  CLEAR LT_MSG[].
  CALL TRANSACTION 'IE01'    USING BDC_TAB
                           UPDATE 'S'
                           MODE P_MODE
                        MESSAGES INTO LT_MSG.

  READ TABLE LT_MSG WITH KEY MSGTYP = 'S'.
  MOVE LT_MSG-MSGV1 TO PP_NUMBER.



ENDFORM.                    " bdc_head
*&------------------------------------------------------------------*
*&      Form  ADD_DATA_EQUIP_PARVW_MA
*&------------------------------------------------------------------*
FORM ADD_DATA_EQUIP_PARVW_MA USING   P_EQUNR    TYPE EQUNR
                                     PW_EQUI     STRUCTURE IT_EQUI
                                     P_SUBRC     LIKE SY-SUBRC.
  "PW_RETURN2  TYPE BAPIRET2.

  DATA : LW_EQUNR TYPE EQUNR.

  REFRESH : BDC_TAB, LT_MSG.



  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            INPUT  = P_EQUNR
       IMPORTING
            OUTPUT = LW_EQUNR.


*- IE02-Change Equipment
  PERFORM DYNPRO  USING:
      'X'   'SAPMIEQ0'          '0100',
      ' '   'BDC_OKCODE'        '/00',
      ' '   'RM63E-EQUNR'       LW_EQUNR.

*-- Click Maintenance Address
  PERFORM DYNPRO  USING:
      'X'   'SAPMIEQ0'          '0101',
      ' '   'BDC_OKCODE'        '=ADRE'.

*-- Maintenance Address POPUP Window
  PERFORM DYNPRO  USING:
      'X'   'SAPLSZA1'          '0201',
      ' '   'BDC_OKCODE'        '=CONT',
      ' '   'SZA1_D0100-TITLE_MEDI'  PW_EQUI-TITLE_MEDI, "/Company
      ' '   'ADDR1_DATA-NAME1'       PW_EQUI-NAME1,    "/Company name
*      ' '   'ADDR1_DATA-SORT1'       PW_EQUI-SORT1, "/
      ' '   'ADDR1_DATA-STREET'      PW_EQUI-STREET, "/STREET
      ' '   'ADDR1_DATA-POST_CODE1'  PW_EQUI-POST_CODE1, "/Postal code
      ' '   'ADDR1_DATA-CITY1'       PW_EQUI-CITY1,     "/
      ' '   'ADDR1_DATA-COUNTRY'     PW_EQUI-COUNTRY, "/Country
      ' '   'ADDR1_DATA-REGION'      PW_EQUI-REGION, "/ REGION
      ' '   'ADDR1_DATA-PO_BOX'      PW_EQUI-PO_BOX, "/PO_BOX
      ' '   'ADDR1_DATA-POST_CODE2'  PW_EQUI-POST_CODE2, "/POST_CODE2
      ' '   'ADDR1_DATA-POST_CODE3'  PW_EQUI-POST_CODE3, "/Comp. p. code
      ' '   'ADDR1_DATA-LANGU'       PW_EQUI-LANGU, "/Language
      ' '   'SZA1_D0100-TEL_NUMBER'  PW_EQUI-TEL_NUMBER, "/TEL_NUMBER
      ' '   'SZA1_D0100-TEL_EXTENS'  PW_EQUI-TEL_EXTENS, "/TEL_EXTENS
      ' '   'SZA1_D0100-FAX_NUMBER'  PW_EQUI-FAX_NUMBER, "/FAX_NUMBER
      ' '   'SZA1_D0100-FAX_EXTENS'  PW_EQUI-FAX_EXTENS, "/FAX_EXTENS
      ' '   'SZA1_D0100-SMTP_ADDR'   PW_EQUI-SMTP_ADDR. "/SMTP_ADDR

**-- Click Partner
*  PERFORM dynpro  USING:
*      'X'   'SAPMIEQ0'          '0101',
*      ' '   'BDC_OKCODE'        '=PART'.
**-- Change Equipment Partner
*  PERFORM dynpro  USING:
*      'X'   'SAPLIPAR'          '0200',
*      ' '   'BDC_OKCODE'        '=BACK',
*      ' '   'IHPA-PARVW(01)'      pw_equi-parvw_ab, "/
*      ' '   'IHPA-PARNR(01)'      pw_equi-parnr_ab, "/
*      ' '   'IHPA-PARVW(02)'      pw_equi-parvw_vw, "/
*      ' '   'IHPA-PARNR(02)'      pw_equi-parnr_vw. "/


*-- Click Class Overview
  PERFORM DYNPRO  USING:
      'X'   'SAPMIEQ0'          '0101',
      ' '   'BDC_OKCODE'        '=KL'.
*-  Class
  PERFORM DYNPRO  USING:
      'X'   'SAPLCLCA'          '0602',
      ' '   'BDC_OKCODE'        '=ENTE',
      ' '   'RMCLF-KLART'        '002'. "/

  PERFORM DYNPRO  USING:
      'X'   'SAPLCLFM'          '0500',
      ' '   'BDC_OKCODE'        '=AUSW',
      ' '   'RMCLF-KREUZ(01)'   'X',
      ' '   'RMCLF-CLASS(01)'   PW_EQUI-CLASS. "/

  PERFORM DYNPRO  USING:
      'X'   'SAPLCTMS'          '0109',
      ' '   'BDC_OKCODE'        '=BACK',
      ' '   'RCTMS-MNAME(01)'    PW_EQUI-ATNAM, "/
      ' '   'RCTMS-MWERT(01)'    PW_EQUI-ATWRT. "/

  PERFORM DYNPRO  USING:
      'X'   'SAPLCLFM'          '0500',
      ' '   'BDC_OKCODE'        '=ENDE'.

*-- Save
  PERFORM DYNPRO  USING:
      'X'   'SAPMIEQ0'          '0101',
      ' '   'BDC_OKCODE'        '=BU'.


*  SET PARAMETER ID 'EQN' FIELD ''.
*  SET PARAMETER ID 'EQN' FIELD lw_equnr.
  CLEAR: LT_MSG[].
  CALL TRANSACTION 'IE02'    USING BDC_TAB
                           UPDATE 'S'
                           MODE P_MODE
                        MESSAGES INTO LT_MSG.

  READ TABLE LT_MSG WITH KEY MSGTYP = 'E'.

  IF SY-SUBRC = 0.
    P_SUBRC = '4'.

  ELSE.
    READ TABLE LT_MSG WITH KEY MSGTYP = 'A'.

    IF SY-SUBRC = 0.
      P_SUBRC = '8'.

    ELSE.
      READ TABLE LT_MSG WITH KEY MSGTYP = 'S'
                                 MSGNR  = '817'.

    ENDIF.
  ENDIF.


*  PERFORM GET_BAPI_RETURN_MSG  USING LT_MSG-MSGTYP
*                                     LT_MSG-MSGID
*                                     LT_MSG-MSGNR
*                                     LT_MSG-MSGV1
*                                     LT_MSG-MSGV2
*                                     LT_MSG-MSGV3
*                                     LT_MSG-MSGV4
*                               CHANGING PW_RETURN2.
*



ENDFORM.                    " ADD_DATA_EQUIP_PARVW_MA

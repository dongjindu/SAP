************************************************************************
* Program Name      : ZCQM13C_CHAR_MASTER_BAPI
* Author            : SeungLyong, Lee
* Creation Date     : 2003.10.20.
* Specifications By : SeungLyong, Lee
* Pattern           : 2.Conversion - 2.2 Call BAPI function
* Development Request No :
* Addl Documentation:
* Description       : Characteristic Upload
*   - Using BAPI Function : BAPI_CHARACT_CREATE
*    -> If the BAPI runs successfully, table RETURN contains
*       no messages of type 'E'.
*   - <Caution>
*     This BAPI does not change the database. To change the database,
*     call BAPI BapiService.TransactionCommit afterwards.
*      -> 'BAPI_TRANSACTION_COMMIT' or 'BAPI_TRANSACTION_ROLLBACK'
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************


REPORT  ZCQM13C_CHAR_MASTER_BAPI NO STANDARD PAGE HEADING
                                          LINE-SIZE 250.


TYPE-POOLS: VRM.

*-- Declaration Tables
TABLES :  CABN,
          CABNT,
          CAWNT,
          CAWN,
          TCME.

*-- Interface table format for Excel file of Material Inspection type
DATA: BEGIN OF IT_CABN OCCURS 0,
        ATNAM   TYPE ATNAM,      "/Characteristic
        ATBEZ   TYPE ATBEZ,      "/Description
        DATUV   TYPE DATUV,      "/Valid from date
        ATKLA   TYPE ATKLA,      "/Chars group
        ATMST   TYPE ATMST,      "/Status of Characteristic
        FORMAT  TYPE ATUDF,      "/Data Type
        ANZST   TYPE ANZST,      "/No. of chars.
*        KLART   TYPE KLASSENART, "/Class type : restriction exist.
        ATWRT   TYPE ATWRT,      "/Char. Value
        ATWTB   TYPE ATWTB,      "/Char. Value Descriptions
      END OF IT_CABN.
*-- Notes of IT_CABN-ANZST - No. of chars.
* ANZST : If a characteristic has data type "character" (CHAR), you
*  can maintain a language-dependent description for values. This
*  description can be up to 30 characters long, regardless of
*  the number you enter here.



*-- Bapi return message table
DATA : IT_RETURN  LIKE BAPIRET2 OCCURS 0 WITH HEADER LINE.

**- Structure for IT_CABN Modfy for Using At Event
DATA: BEGIN OF ST_CABN OCCURS 0,
        ATNAM   TYPE ATNAM,      "/Characteristic
        DATUV   TYPE DATUV,      "/Valid from date
        ATBEZ   TYPE ATBEZ,      "/Description
        ATKLA   TYPE ATKLA,      "/Chars group
        ATMST   TYPE ATMST,      "/Status of Characteristic
        FORMAT  TYPE ATUDF,      "/Data Type
        ANZST   TYPE ANZST,      "/No. of chars.
      END OF ST_CABN.

*//Data(Global Fileds) ;(WA_)  == Local? ?? ; (LO_)
*                 Flag ;(FL_), Counter;(CT_), Temp;(TMP_)
DATA : WA_REPID LIKE SYST-REPID,
       WA_DYNNR LIKE SYST-DYNNR.

*/ Result variables
DATA : WA_SUCCESS_CNT TYPE I,
       WA_FAILED_CNT  TYPE I,
       WA_TOTAL_ENTR  TYPE I.

*// Structures


**-- Constants
CONSTANTS : C_MARK  TYPE C VALUE 'X'.

**-- List box variables
DATA: WA_NAME  TYPE VRM_ID,
      IT_LIST  TYPE VRM_VALUES,
      WA_VALUE LIKE LINE OF IT_LIST.

*#### Selection Screen ####
SELECTION-SCREEN BEGIN OF BLOCK BLK WITH FRAME  TITLE TEXT-T01.
PARAMETERS : P_FILE   LIKE RLGRAP-FILENAME   OBLIGATORY.
SELECTION-SCREEN ULINE.
PARAMETERS : P_TEST  TYPE BAPIFLAG DEFAULT 'X' AS LISTBOX
                                             VISIBLE LENGTH 14.
SELECTION-SCREEN END OF BLOCK BLK .

AT SELECTION-SCREEN OUTPUT.
  SET TITLEBAR '1000'.
  WA_NAME = 'P_TEST'.
  WA_VALUE-KEY = 'X'. WA_VALUE-TEXT = 'Simulation'.
  APPEND WA_VALUE TO IT_LIST.
  WA_VALUE-KEY = ' '. WA_VALUE-TEXT = 'Write'.
  APPEND WA_VALUE TO IT_LIST.
  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            ID     = WA_NAME
            VALUES = IT_LIST.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.

  PERFORM GET_SELECT_EXCEL_FILE  USING P_FILE.


START-OF-SELECTION.

  PERFORM F_EXCEL_UPLOAD  TABLES   IT_CABN
                          USING    P_FILE.
  IF IT_CABN[] IS INITIAL.
    MESSAGE E000(ZMQM)
            WITH 'Entries not founded. Please check File'(E15).
    EXIT.
  ENDIF.

  PERFORM MODIFY_IT_CABN.


END-OF-SELECTION.

  PERFORM EXECUTE_BAPI_FUNC. "/execute BAPI Function

  PERFORM WRITE_LOG_MESSAGE.



TOP-OF-PAGE.

  PERFORM WRITE_HEADER.

END-OF-PAGE.

*&------------------------------------------------------------------*
*&      Form  F_EXCEL_UPLOAD
*&------------------------------------------------------------------*
FORM F_EXCEL_UPLOAD  TABLES   P_TABLE
                      USING    P_FILENAME  LIKE RLGRAP-FILENAME.



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
      WRITE '-------------------------------------------------------'.
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

ENDFORM.                    " WRITE_LOG_MESSAGE
*&------------------------------------------------------------------*
*&      Form  WRITE_HEADER
*&------------------------------------------------------------------*
FORM WRITE_HEADER.
  WRITE :
    'Characteristic Uploading  Log'(H01).
  ULINE.
  WRITE : 'Total of Characteristic   : '(H02) ,
          WA_TOTAL_ENTR.
  NEW-LINE.
  WRITE : 'Success entries           : '(H03) ,
          WA_SUCCESS_CNT.
  NEW-LINE.
  WRITE : 'Failed entries            : '(H04),
          WA_FAILED_CNT.
  ULINE.
ENDFORM.                    " WRITE_HEADER

*&------------------------------------------------------------------*
*&      Form  MODIFY_IT_CABN
*&------------------------------------------------------------------*
FORM MODIFY_IT_CABN.

  DATA : LW_ATNAM TYPE ATNAM.
  DATA : LW_CABN_INDEX LIKE SY-TABIX.

  LOOP AT IT_CABN.
    LW_CABN_INDEX = SY-TABIX.

    IF     LW_ATNAM  NE IT_CABN-ATNAM AND
       NOT IT_CABN-ATNAM IS INITIAL.

      LW_ATNAM = IT_CABN-ATNAM.
      CLEAR ST_CABN.
      MOVE-CORRESPONDING IT_CABN TO ST_CABN.

    ENDIF.

    MOVE-CORRESPONDING ST_CABN TO IT_CABN.

    MODIFY IT_CABN INDEX LW_CABN_INDEX.
  ENDLOOP.

ENDFORM.                    " MODIFY_IT_CABN
*&-----------------------------------------------------------------*
*&      Form  EXECUTE_BAPI_FUNC
*&-----------------------------------------------------------------*
FORM EXECUTE_BAPI_FUNC.

  DATA : LW_CABN LIKE IT_CABN.

*-- Parameters for interface of BAPI
*<Import>
  DATA :
    LW_CHAR_DETAIL   LIKE  BAPICHARACTDETAIL,      "/Char. Attributes
    LW_CHANGENUMBER  LIKE  BAPICHARACTKEY-CHANGENUM, "/Change Number
    LW_KEYDATE       LIKE  BAPICHARACTKEY-KEYDATE.  "/Key Date(Valid)
*<Export>
*<Tables>
  DATA :
   LT_CHAR_DESCR      LIKE BAPICHARACTDESCR     "/Char. Description
                       OCCURS 0 WITH HEADER LINE,
   LT_CHAR_VALUESNUM  LIKE BAPICHARACTVALUESNUM  "/Allowed Value for NUM
                       OCCURS 0 WITH HEADER LINE," Characteristics
   LT_CHAR_VALUESCHAR LIKE BAPICHARACTVALUESCHAR  "/Allowed Values for
                       OCCURS 0 WITH HEADER LINE, " CHAR Characteristics
   LT_CHAR_VALUESCURR LIKE BAPICHARACTVALUESCURR  "/Allowed Values for
                       OCCURS 0 WITH HEADER LINE, "CURR Characteristics
   LT_CHAR_VALUESDESCR LIKE BAPICHARACTVALUESDESCR  "Value Description
                        OCCURS 0 WITH HEADER LINE,
   LT_CHAR_REFERENCES  LIKE BAPICHARACTREFERENCES "/Entries for
                         OCCURS 0 WITH HEADER LINE, "Reference Char.
   LT_CHAR_RESTRICTIONS LIKE BAPICHARACTRESTRICTIONS "/Restrictions to
                        OCCURS 0 WITH HEADER LINE.  "Class Types
  DATA : LT_RETURN  LIKE  BAPIRET2
                          OCCURS 10 WITH HEADER LINE."Error Messages


*-- Mapping
  LOOP AT IT_CABN.
    CLEAR LW_CABN.

    MOVE-CORRESPONDING IT_CABN TO LW_CABN.

    AT NEW ATNAM.  "/Characteristic Start
      CLEAR : LW_CHAR_DETAIL, LW_CHANGENUMBER, LW_KEYDATE.
*      CLEAR : LT_CHAR_DESCR, LT_CHAR_VALUESNUM, LT_CHAR_VALUESCHAR,
*              LT_CHAR_VALUESCURR, LT_CHAR_VALUESDESCR,
*              LT_CHAR_REFERENCES , LT_CHAR_RESTRICTIONS, LT_RETURN.
      REFRESH : LT_CHAR_DESCR, LT_CHAR_VALUESNUM, LT_CHAR_VALUESCHAR,
              LT_CHAR_VALUESCURR, LT_CHAR_VALUESDESCR,
              LT_CHAR_REFERENCES , LT_CHAR_RESTRICTIONS, LT_RETURN.

      WA_TOTAL_ENTR = WA_TOTAL_ENTR + 1.  "/Count total entries

*--     Key date (Valid from)
      MOVE : LW_CABN-DATUV   TO LW_KEYDATE.

*--     Characteristic Attributes
      MOVE :
        LW_CABN-ATNAM   TO LW_CHAR_DETAIL-CHARACT_NAME,
        LW_CABN-FORMAT  TO LW_CHAR_DETAIL-DATA_TYPE,
        LW_CABN-ANZST   TO LW_CHAR_DETAIL-LENGTH,
*        LW_CABN-        TO LW_CHAR_DETAIL-DECIMALS,
        LW_CABN-ATMST   TO LW_CHAR_DETAIL-STATUS,
        LW_CABN-ATKLA   TO LW_CHAR_DETAIL-CHARACT_GROUP.
*        'X'          TO LW_CHAR_DETAIL-VALUE_ASSIGNMENT,"/Single Value

*--    Characteristic Descriptions
      CLEAR : LT_CHAR_DESCR.
      MOVE :
        SY-LANGU       TO LT_CHAR_DESCR-LANGUAGE_INT,
        LW_CABN-ATBEZ  TO LT_CHAR_DESCR-DESCRIPTION.

      APPEND :   LT_CHAR_DESCR.

    ENDAT.

*--  Values table of Characterisitic
    AT NEW  ATWRT.  "/each Value of Characterisitic(Data type : CHAR)
      CLEAR : LT_CHAR_VALUESCHAR,
*              LT_CHAR_VALUESNUM, LT_CHAR_VALUESCURR,
              LT_CHAR_VALUESDESCR,
*              LT_CHAR_REFERENCES , LT_CHAR_RESTRICTIONS,
              LT_RETURN.

*--    Characteristic Value table(Data type : CHAR)
      MOVE :
        LW_CABN-ATWRT  TO LT_CHAR_VALUESCHAR-VALUE_CHAR.
*        LW_CABN-  TO LT_CHAR_VALUESCHAR-VALUE_CHAR_HIGH,
*        LW_CABN-  TO LT_CHAR_VALUESCHAR-DEFAULT_VALUE.

      APPEND : LT_CHAR_VALUESCHAR.

*--    Characteristic Value Description  table for all Data types
      MOVE :
         SY-LANGU      TO LT_CHAR_VALUESDESCR-LANGUAGE_INT,
*         LW_CABN-  TO LT_CHAR_VALUESDESCR-LANGUAGE_ISO,
         LW_CABN-ATWRT  TO LT_CHAR_VALUESDESCR-VALUE_CHAR,
         LW_CABN-ATWTB  TO LT_CHAR_VALUESDESCR-DESCRIPTION.


      APPEND : LT_CHAR_VALUESDESCR.

    ENDAT.

*--  Execute BAPI Function for Characteristic Upload
    AT END OF ATNAM.  "/ at end of Characteristic, Execute Update.

      CHECK NOT LW_CABN IS INITIAL.

      CALL FUNCTION 'BAPI_CHARACT_CREATE'
           EXPORTING
                CHARACTDETAIL       = LW_CHAR_DETAIL
*                CHANGENUMBER        = LW_CHANGENUMBER
                KEYDATE             = LW_KEYDATE
           TABLES
                CHARACTDESCR        = LT_CHAR_DESCR
*                CHARACTVALUESNUM    = LT_CHAR_VALUESNUM
                CHARACTVALUESCHAR   = LT_CHAR_VALUESCHAR
*                CHARACTVALUESCURR   = LT_CHAR_VALUESCURR
                CHARACTVALUESDESCR  = LT_CHAR_VALUESDESCR
*                CHARACTREFERENCES   = LT_CHAR_REFERENCES
*                CHARACTRESTRICTIONS = LT_CHAR_RESTRICTIONS
                RETURN              = LT_RETURN.

*/// After BAPI execution////
      CLEAR IT_RETURN.
**      insert division message for each Characteristic Upload
      CONCATENATE LW_CABN-ATNAM
                  ':'
                  LW_CABN-ATBEZ
                     INTO  IT_RETURN-MESSAGE SEPARATED BY SPACE.


*/--    Error Check and Log Message Backup.
*   -> If the BAPI runs successfully, table RETURN contains no messages
*      of type 'E'.
      READ TABLE LT_RETURN WITH KEY TYPE = 'E'. "/ Error

      IF SY-SUBRC = 0.  "//Error Founded
        PERFORM BAPI_SERVICE_ROLLBACK.
        CONCATENATE IT_RETURN-MESSAGE
                     '=> Failed'
                       INTO  IT_RETURN-MESSAGE SEPARATED BY SPACE.
        WA_FAILED_CNT = WA_FAILED_CNT + 1.

      ELSE.
        READ TABLE LT_RETURN WITH KEY TYPE = 'A'.  "/Fatal Error

        IF SY-SUBRC = 0.  "//Error Founded
          PERFORM BAPI_SERVICE_ROLLBACK.
          CONCATENATE IT_RETURN-MESSAGE
                       '=> Failed'
                         INTO  IT_RETURN-MESSAGE SEPARATED BY SPACE.
          WA_FAILED_CNT = WA_FAILED_CNT + 1.

        ELSE.      "/// runs successfully
          READ TABLE LT_RETURN  WITH KEY TYPE = 'S'  "/already exists
                                         NUMBER = '002'.
          IF SY-SUBRC NE 0.  "Success
            IF P_TEST = SPACE. "/Only Write Mode
              PERFORM BAPI_SERVICE_COMMIT.
            ELSE.
              PERFORM BAPI_SERVICE_ROLLBACK. "/for unlock Char.
            ENDIF.

            CONCATENATE IT_RETURN-MESSAGE
                         '=> Success'
                           INTO  IT_RETURN-MESSAGE SEPARATED BY SPACE.
            WA_SUCCESS_CNT = WA_SUCCESS_CNT + 1.

          ELSE.  "/Fail - already exist.
            PERFORM BAPI_SERVICE_ROLLBACK.
            CONCATENATE IT_RETURN-MESSAGE
                         '=> Failed'
                           INTO  IT_RETURN-MESSAGE SEPARATED BY SPACE.
            WA_FAILED_CNT = WA_FAILED_CNT + 1.
          ENDIF.
        ENDIF.

      ENDIF.

      APPEND IT_RETURN.
      APPEND LINES OF LT_RETURN TO IT_RETURN.
      APPEND INITIAL LINE TO  IT_RETURN.

    ENDAT.

  ENDLOOP.

ENDFORM.                    " EXECUTE_BAPI_FUNC
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
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
* EXPORTING
*   WAIT          =
* IMPORTING
*   RETURN        = .


ENDFORM.                    " BAPI_SERVICE_COMMIT

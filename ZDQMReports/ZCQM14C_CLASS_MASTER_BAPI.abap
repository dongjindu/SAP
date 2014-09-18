************************************************************************
* Program Name      : ZCQM14C_CLASS_MASTER_BAPI
* Author            : SeungLyong, Lee
* Creation Date     : 2003.10.24.
* Specifications By : SeungLyong, Lee
* Pattern           : 2.Conversion - 2.2 Call BAPI function
* Development Request No :
* Addl Documentation:
* Description       : Characteristic Upload
*   - Using BAPI Function : BAPI_CLASS_CREATE
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


REPORT  ZCQM14C_CLASS_MASTER_BAPI NO STANDARD PAGE HEADING
                                          LINE-SIZE 250.


TYPE-POOLS: VRM.

*-- Class tables
TABLES : KLAH, "/Class Header
         KSML, "/Characteristics of a class
         TCLU, "/Class status
         TCLA, "/Class types
         TCLT. "/Classifiable Objects

*-- Declaration Tables - Characteristic
TABLES :  CABN,
          CABNT,
          CAWNT,
          CAWN,
          TCME.


*-- Interface table format for Excel file
DATA: BEGIN OF IT_KLAH OCCURS 0,
        CLASS   TYPE KLASSE_D,   "/Class
        KLART   TYPE KLASSENART, "/Class type
        KLBEZ   TYPE KLSBEZ,     "/Class Description
        STATU   TYPE KLSTATUS,	 "/Class status
        KLAGR   TYPE KLASSENGR,  "/Class Group
        VONDT   TYPE VONDAT,     "/Valid from
        ATNAM   TYPE ATNAM,      "/Characteristics of a Class
      END OF IT_KLAH.


*-- Bapi return message table
DATA : IT_RETURN  LIKE BAPIRET2 OCCURS 0 WITH HEADER LINE.

**- Structure for IT_KLAH Modfy for Using At Event
DATA: BEGIN OF ST_KLAH OCCURS 0,
        CLASS   TYPE KLASSE_D,   "/Class
        KLART   TYPE KLASSENART, "/Class type
        KLBEZ   TYPE KLSBEZ,     "/Class Description
        STATU   TYPE KLSTATUS,	 "/Class status
        KLAGR   TYPE KLASSENGR,  "/Class Group
      END OF ST_KLAH.

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

  PERFORM F_EXCEL_UPLOAD  TABLES   IT_KLAH
                          USING    P_FILE.
  IF IT_KLAH[] IS INITIAL.
    MESSAGE E000(ZMQM)
            WITH 'Entries not founded. Please check File'(E15).
    EXIT.
  ENDIF.

  PERFORM MODIFY_IT_KLAH.


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
    MESSAGE E000(ZMPS) WITH 'File Upload Failed !'(E10).
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
    'Class Master Uploading  Log'(H01).
  ULINE.
  WRITE : 'Total of Class Master     : '(H02) ,
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
*&      Form  MODIFY_IT_KLAH
*&------------------------------------------------------------------*
FORM MODIFY_IT_KLAH.

  DATA : LW_CLASS TYPE KLASSE_D.
  DATA : LW_KLAH_INDEX LIKE SY-TABIX.

  LOOP AT IT_KLAH.
    LW_KLAH_INDEX = SY-TABIX.

    IF     LW_CLASS  NE IT_KLAH-CLASS AND
       NOT IT_KLAH-CLASS IS INITIAL.

      LW_CLASS = IT_KLAH-CLASS.
      CLEAR ST_KLAH.
      MOVE-CORRESPONDING IT_KLAH TO ST_KLAH.

    ENDIF.

    MOVE-CORRESPONDING ST_KLAH TO IT_KLAH.

    MODIFY IT_KLAH INDEX LW_KLAH_INDEX.
  ENDLOOP.

ENDFORM.                    " MODIFY_IT_KLAH
*&-----------------------------------------------------------------*
*&      Form  EXECUTE_BAPI_FUNC
*&-----------------------------------------------------------------*
FORM EXECUTE_BAPI_FUNC.

  DATA : LW_KLAH LIKE IT_KLAH.

*-- Parameters for interface of BAPI
*<Import>
  DATA :
    LW_CLASSNUMNEW     LIKE BAPI_CLASS_KEY-CLASSNUM,  "/Class
    LW_CLASSTYPENEW    LIKE BAPI_CLASS_KEY-CLASSTYPE, "/Class Type
    LW_CHANGENUMBER    LIKE BAPI1003_KEY-CHANGENUMBER,"/Change Number
    LW_CLASSBASICDATA  LIKE BAPI1003_BASIC,           "/Basic Data
    LW_CLASSDOCUMENT   LIKE BAPI1003_DOCU,       "/Document Information
    LW_CLASSADDITIONAL LIKE BAPI1003_ADD,        "/Additional Data
    LW_CLASSSTANDARD   LIKE BAPI1003_STAND.      "/Standards Data
*<Export>
*<Tables>
  DATA :
*     - Class Description / Keywords
    LT_CLASSDESCRIPTIONS  LIKE BAPI1003_CATCH
                         OCCURS 10 WITH HEADER LINE,
*     - Long Texts
  LT_CLASSLONGTEXTS  LIKE BAPI1003_LONGTEXT
                       OCCURS 10 WITH HEADER LINE,
*     - Characteristics
  LT_CLASSCHARACTERISTICS  LIKE BAPI1003_CHARACT
                       OCCURS 10 WITH HEADER LINE,
*     - Overwritten Characteristic Attributes
  LT_CHARACTOVERWRITE  LIKE BAPI1003_CHARACT_OVERWR
                       OCCURS 10 WITH HEADER LINE,
*     - Overwritten Characteristic Values
  LT_CHARACTVALUEOVERWRITE  LIKE BAPI1003_CHARACT_VALUE_OVR
                       OCCURS 10 WITH HEADER LINE,
*     - Overwritten Characteristic Descriptions
  LT_CHARACTVALUETEXTOVR  LIKE BAPI1003_CHARVALTEXT
                       OCCURS 10 WITH HEADER LINE.


  DATA : LT_RETURN  LIKE  BAPIRET2
                         OCCURS 10 WITH HEADER LINE."/Error Messages


*-- Mapping
  LOOP AT IT_KLAH.
    CLEAR LW_KLAH.

    MOVE-CORRESPONDING IT_KLAH TO LW_KLAH.

    AT NEW CLASS.  "/Characteristic Start
      CLEAR : LW_CLASSNUMNEW, LW_CLASSTYPENEW, LW_CHANGENUMBER,
              LW_CLASSBASICDATA, LW_CLASSDOCUMENT, LW_CLASSADDITIONAL,
              LW_CLASSSTANDARD.
      REFRESH : LT_CLASSDESCRIPTIONS, LT_CLASSLONGTEXTS,
                LT_CLASSCHARACTERISTICS, LT_CHARACTOVERWRITE,
                LT_CHARACTVALUEOVERWRITE, LT_CHARACTVALUETEXTOVR.

      REFRESH :LT_RETURN.

      WA_TOTAL_ENTR = WA_TOTAL_ENTR + 1.  "/Count total Class entries

*///--<<Imports>>
**--     Key date (Valid from)

*-- Class
      MOVE : LW_KLAH-CLASS TO LW_CLASSNUMNEW.  "/Class number
*-- Class type
      MOVE : LW_KLAH-KLART TO LW_CLASSTYPENEW. "/Class type

*-- Change number

*-- Basic Data
      MOVE :
       LW_KLAH-STATU  TO LW_CLASSBASICDATA-STATUS, "/Class status
       LW_KLAH-KLAGR  TO LW_CLASSBASICDATA-CLASSGROUP, "/Class Group
       LW_KLAH-VONDT  TO LW_CLASSBASICDATA-VALID_FROM, "/Valid-from date
*       LW_KLAH-  TO LW_CLASSBASICDATA-VALID_TO, "/ Valid-to date
       'X'  TO LW_CLASSBASICDATA-SAME_VALUE_NO,"/No Check Same Class
       ' '  TO LW_CLASSBASICDATA-SAME_VALUE_W, "/Check Same Cl with W
       ' '  TO LW_CLASSBASICDATA-SAME_VALUE_E. "/Check Same Cl with E
*       LW_KLAH-  TO LW_CLASSBASICDATA-LOCAL_CLASS,"/Local class
*       LW_KLAH-  TO LW_CLASSBASICDATA-KATALOG."/Name of ext. catalog

*-- Document Information
*       MOVE : LW_KLAH-      TO LW_CLASSDOCUMENT-, "/

*-- Additional Data
*       MOVE :
*        LW_KLAH-  TO LW_CLASSADDITIONAL-, "/

*-- Standards Data
*       MOVE :
*         LW_KLAH-  TO LW_CLASSSTANDARD-, "/


*///--<<Tables>>
*-- Class Description / Keywords
      CLEAR    : LT_CLASSDESCRIPTIONS.
      MOVE :
        SY-LANGU  TO LT_CLASSDESCRIPTIONS-LANGU, "/Language key
*          LW_KLAH-  TO LT_CLASSDESCRIPTIONS-LANGU_ISO, "/Keywords
        LW_KLAH-KLBEZ   TO LT_CLASSDESCRIPTIONS-CATCHWORD, "/Keywords
*          LW_KLAH-  TO LT_CLASSDESCRIPTIONS-INSERT_BEFORE, "/
        ' '       TO LT_CLASSDESCRIPTIONS-DELETE_FLAG."/Deletion ind

      APPEND LT_CLASSDESCRIPTIONS.

*-- Long Texts
*        CLEAR : LT_CLASSLONGTEXTS.
*         MOVE :
*           SY-LANGU       TO LT_CLASSLONGTEXTS-LANGU,
*           LW_KLAH-KLBEZ  TO LT_CLASSLONGTEXTS-TEXT_DESCR.
*        APPEND LT_CLASSLONGTEXTS.

    ENDAT.

**-- Interface table format for Excel file
*DATA: BEGIN OF IT_KLAH OCCURS 0,
*        CLASS   TYPE KLASSE_D,   "/Class
*        KLART   type KLASSENART, "/Class type
*        KLBEZ   type KLSBEZ,     "/Class Description
*        STATU   type KLSTATUS,	 "/Class status
*        KLAGR   type KLASSENGR,  "/Class Group
*        ATNAM   TYPE ATNAM,      "/Characteristics of a Class
*      END OF IT_KLAH.

*--  Characterisitic
    AT NEW  ATNAM.  "/each Characteristic (Data type : CHAR)
*-- Characteristics
      CLEAR : LT_CLASSCHARACTERISTICS.
         MOVE :
         LW_KLAH-ATNAM TO LT_CLASSCHARACTERISTICS-NAME_CHAR."/Char. name
*        LW_KLAH-  TO LT_CLASSCHARACTERISTICS-CODE_LETTER, "/
*        LW_KLAH-  TO LT_CLASSCHARACTERISTICS-INSERT_BEFORE, "/

      APPEND LT_CLASSCHARACTERISTICS.

*-- Overwritten Characteristic Attributes
*-- Overwritten Characteristic Values
*-- Overwritten Characteristic Descriptions

    ENDAT.

*--  Execute BAPI Function for Class Upload
    AT END OF CLASS.  "/ at end of Class, Execute BAPI.

      CHECK NOT LW_KLAH IS INITIAL.

      CALL FUNCTION 'BAPI_CLASS_CREATE'
           EXPORTING
                CLASSNUMNEW           = LW_CLASSNUMNEW
                CLASSTYPENEW          = LW_CLASSTYPENEW
                CHANGENUMBER          = LW_CHANGENUMBER
                CLASSBASICDATA        = LW_CLASSBASICDATA
                CLASSDOCUMENT         = LW_CLASSDOCUMENT
                CLASSADDITIONAL       = LW_CLASSADDITIONAL
                CLASSSTANDARD         = LW_CLASSSTANDARD
           TABLES
                RETURN                = LT_RETURN
                CLASSDESCRIPTIONS     = LT_CLASSDESCRIPTIONS
                CLASSLONGTEXTS        = LT_CLASSLONGTEXTS
                CLASSCHARACTERISTICS  = LT_CLASSCHARACTERISTICS
                CHARACTOVERWRITE      = LT_CHARACTOVERWRITE
                CHARACTVALUEOVERWRITE = LT_CHARACTVALUEOVERWRITE
                CHARACTVALUETEXTOVR   = LT_CHARACTVALUETEXTOVR.


*/// After BAPI execution////
      CLEAR IT_RETURN.
**      insert division message for each Class Upload
      CONCATENATE LW_KLAH-CLASS
                  ':'
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

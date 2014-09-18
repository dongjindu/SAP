************************************************************************
* Program Name      : ZSQM03U_CALIBRATION_FORM
* Author            : SeungLyong, Lee
* Creation Date     : 2004.03.31.
* Specifications By : SeungLyong, Lee
* Pattern           : 1.1.3 General
* Development Request No : UD1K908885
* Addl Documentation:
* Description       : Certificate of Calibration Using Samrt
*                     Forms
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT  ZSQM03U_CALIBRATION_FORM    .

*&&& Data Declaration.  &&&*
TYPE-POOLS VRM.     "//Value Request Manager: Types & Constants
*TYPE-POOLS CXTAB .  "//Table_control Object type pool
TABLES : FELD.      "//Screen Object Structure

*-- SAP Scripts Object Interface
TABLES : THEAD. "/SAPscript: Text Header

*//Tables;(TABLES : Table_Name  "//Table Description)
TABLES : QMEL.   "/Quality Notification


*//Structures Declaration(TABLES : Structure Name."/Description)
TABLES : ZSQM_CALI_CERTI_HEADER,  "/Calibration Report Header
         ZSQM_CALI_CERTI_RESULT.  "/Calibration Report Result Item List
TABLES : ZSQM_INSP_RESULT_04.       "/Document NO Str - by Object


*//InfoType;()
*//Cluster or Import Parameter;(Parameter Name)

*//Controls(for only Screen Control Element);(TC_ , or TS_)
*-- TABLE CONTROL
*CONTROLS: TC_9100  TYPE TABLEVIEW USING SCREEN 9100.

*//Type (Table Structure);(TY_ )- Table or Structure


*//Constants ;(C_) ==> True:'X' or '1' False:Space or '0'
CONSTANTS : C_MARK   VALUE 'X'.

*//Data(Global Fileds) ;(GL_)


**//-- Global : used Variable just in this Program
*-- Function Control
DATA : OK_CODE LIKE SY-UCOMM.

*-- "/Smart Form Name
DATA : WA_FORMNAME TYPE TDSFNAME  VALUE 'ZSQM03_CALIBRATION_V2'.


*-- Global : used Variable just in this Program
*--         for Function 'BAPI_INSPLOT_GETDETAIL'
DATA : WA_GENERAL_DATA	LIKE	BAPI2045D_IL0,	       "General data
       WA_TASK_LIST_DATA	LIKE	BAPI2045D_IL1, "Task List Assign
       WA_STOCK_DATA	LIKE	BAPI2045D_IL2,	       "Stock Data
       WA_ACCOUNTING	LIKE	BAPI2045D_IL3,         "Account Assign
       WA_USAGE_DECISION	LIKE	BAPI2045D_IL4, "Usage Decision
       WA_CUSTOMER_DATA	LIKE	BAPI2045CI,            "Cust. CI_QALS
       WA_RETURN	        LIKE	BAPIRETURN1.   "Return Values

DATA : WA_NAME TYPE TDOBNAME.

*-- User Confirm for pop-up Message
DATA : WA_ANSWER TYPE C.

*//Data(Work Area or (Internal) Structures);(WA_ )(ST_)?



*//Internal Tables and Index Fields;(IT_), (I_)

*-- Internal Tables for Function 'BAPI_INSPLOT_GETDETAIL'
DATA : IT_SYS_STATUS  LIKE BAPI2045SS OCCURS 5 WITH HEADER LINE,
                                           "Inspection Lot System Status
       IT_USER_STATUS LIKE BAPI2045US OCCURS 5 WITH HEADER LINE,
                                     "User Status for the Inspection Lot
       IT_MAT_DOC     LIKE BAPI2045MD OCCURS 2 WITH HEADER LINE.
"References Between Inspection Lots and Material Documents

DATA : IT_PLANMK LIKE ZSQM_REF_DOC OCCURS 10 WITH HEADER LINE.
"PLMKB OCCURS 10 WITH HEADER LINE.
"Characteristic document table

*- Calibration Results List
DATA : IT_CALI_RESULT LIKE ZSQM_CALI_CERTI_RESULT
                               OCCURS 10  WITH HEADER LINE.

*- Internal Table : MS Number  Document OBJ Keys
DATA : IT_MS_DOC_OBJ LIKE ZSQM_INSP_RESULT_03 OCCURS 10
                                                   WITH HEADER LINE.

*- MS Document No Table
DATA : IT_MS_DOC     LIKE ZSQM_INSP_RESULT_04 OCCURS 10
                                                   WITH HEADER LINE.

*-- Internal Tables for Long Text of Notification
DATA : IT_REMARK LIKE TLINE OCCURS 10.


*//Ranges; (R_)
*RANGES :  "/

*//Field Symbols; <FS_>
*-- TABLE CONTROLS VARIABLE(field-symbols)
*FIELD-SYMBOLS: <TC>  TYPE CXTAB_CONTROL. "table control
*"                              Table_control Object(CXTAB)

*//Field Group;

***//& Selection Screen Definition(Parameters Select-Option)
*-- Paramerters : (P_), Select-Options : (S_)
SELECTION-SCREEN BEGIN OF BLOCK BLK WITH FRAME  TITLE TEXT-T01.
PARAMETERS : P_PRUEFL  TYPE QPLOS OBLIGATORY."/Inspection Lot Number

SELECTION-SCREEN END OF BLOCK BLK .

*-- Seclection Screen Flow Logic Control Event Handling
*AT SELECTION-SCREEN ON ( ON END OF, ON VALUE-REQUEST FOR,
* ON HELP-REQUEST FOR, ON RADIOBUTTON GROUP, ON BLOCK OUTPUT,
* ON EXIT-COMMAND )


AT SELECTION-SCREEN OUTPUT.
SET TITLEBAR '1000'.

*-- Selection for Selection Screen
START-OF-SELECTION.

END-OF-SELECTION.

  CLEAR ZSQM_CALI_CERTI_HEADER.

*-- Get Data of Calibration  Header Data.
*  PERFORM GET_DATA_FROM_DB  USING ZSQM_CALI_CERTI_HEADER
*                                  P_PRUEFL .     "/Inspec Lot No.

  PERFORM GET_DATA_FROM_DB2  USING ZSQM_CALI_CERTI_HEADER
                                  P_PRUEFL .     "/Inspec Lot No.

*-- Get other Data of Calibration  Header Data
*  PERFORM GET_ETC_N_TEXT_DATA  USING ZSQM_CALI_CERTI_HEADER
*                                     P_PRUEFL.


**-- Get Calibration Result List
  PERFORM GET_CALIBRATION_RESULT_LIST  TABLES IT_CALI_RESULT
                                              IT_PLANMK.


*-- Get Document Data Using Function Module
*--  'CQ_DB_PLMK_READ' Result table.
  PERFORM GET_MS_NUMBER.

*-- Print out Form
  PERFORM PRINT_FORMS  USING WA_FORMNAME.


*// Event Handling(Except Selection Screen (Flow)event)
*Load of Program.

*Initialization.

***//Macro Definitions
*DEFINE RANGE_MACRO.
*END-OF-DEFINITION.

**<<<<<<<<< Program Main / Subroutine / Flow Logic >>>>>>>>>>>>**
*&------------------------------------------------------------------*
*&      Form  PRINT_FORMS
*&------------------------------------------------------------------*
FORM PRINT_FORMS   USING P_FORMNAME  TYPE TDSFNAME.
  DATA :LW_FUNCNAME TYPE RS38L_FNAM .

  CLEAR :LW_FUNCNAME.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
       EXPORTING
            FORMNAME           = P_FORMNAME
*            VARIANT            = ' '
*            DIRECT_CALL        = ' '
       IMPORTING
            FM_NAME            = LW_FUNCNAME
       EXCEPTIONS
            NO_FORM            = 1
            NO_FUNCTION_MODULE = 2
            OTHERS             = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CHECK SY-SUBRC = 0.


** "/Dev-'/1BC/1BCDWB/SF00000029'
  CALL FUNCTION LW_FUNCNAME
       EXPORTING
            I_CALIBR         = ZSQM_CALI_CERTI_HEADER
       TABLES
            T_RESULT         = IT_CALI_RESULT
            T_MS_DOC         = IT_MS_DOC
       EXCEPTIONS
            FORMATTING_ERROR = 1
            INTERNAL_ERROR   = 2
            SEND_ERROR       = 3
            USER_CANCELED    = 4
            OTHERS           = 5.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.





ENDFORM.                    " PRINT_FORMS
*&---------------------------------------------------------------------*
*&      Form  GET_REPORT_NO_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_REPORT  text
*      -->P_P_QMNUM  text
*----------------------------------------------------------------------*
FORM GET_REPORT_NO_LIST TABLES  PT_REPORT STRUCTURE ZSQM_INSP_RESULT_04
                        USING   P_OBJKY.

  DATA : LW_OBJKY LIKE DRAD-OBJKY.

  MOVE: P_OBJKY TO LW_OBJKY.

  SELECT DOKNR OBJKY
  INTO CORRESPONDING FIELDS OF TABLE   PT_REPORT
    FROM DRAD
      WHERE OBJKY = LW_OBJKY.

ENDFORM.                    " GET_REPORT_NO_LIST
*&---------------------------------------------------------------------*
*&      Form  GET_LONGTEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_QMEL_LTQM  text
*      -->P_0165   text
*      -->P_0166   text
*      -->P_P_QMNUM  text
*----------------------------------------------------------------------*
FORM GET_LONGTEXT TABLES   PT_TLINE      STRUCTURE TLINE
                  USING    VALUE(P_TDOBJECT)  TYPE TDOBJECT
                           VALUE(P_TDID)      TYPE TDID
                           P_NAME             TYPE TDOBNAME.
  REFRESH PT_TLINE.
  CLEAR THEAD.

  MOVE : P_TDOBJECT TO THEAD-TDOBJECT,
         P_NAME     TO THEAD-TDNAME,
         P_TDID     TO THEAD-TDID,
         SY-LANGU   TO THEAD-TDSPRAS.

  CALL FUNCTION 'READ_TEXT'
       EXPORTING
            CLIENT                  = SY-MANDT
            ID                      = THEAD-TDID
            LANGUAGE                = THEAD-TDSPRAS
            NAME                    = THEAD-TDNAME
            OBJECT                  = THEAD-TDOBJECT
       TABLES
            LINES                   = PT_TLINE
       EXCEPTIONS
            ID                      = 1
            LANGUAGE                = 2
            NAME                    = 3
            NOT_FOUND               = 4
            OBJECT                  = 5
            REFERENCE_CHECK         = 6
            WRONG_ACCESS_TO_ARCHIVE = 7
            OTHERS                  = 8.
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.


ENDFORM.                    " GET_LONGTEXT
*&------------------------------------------------------------------*
*&      Form  GET_DATA_FROM_DB
*&------------------------------------------------------------------*
FORM GET_DATA_FROM_DB USING   PS_CALI  STRUCTURE ZSQM_CALI_CERTI_HEADER
                              VALUE(P_PRUEFLOS) TYPE QPLOS.

  CLEAR : WA_GENERAL_DATA, WA_TASK_LIST_DATA, WA_STOCK_DATA,
          WA_ACCOUNTING, WA_USAGE_DECISION, WA_CUSTOMER_DATA,
          WA_RETURN.

*-- Get Detail Inspection Information Using Bapi Function
*-    BAPI_INSPLOT_GETDETAIL : Load Detail Data and Usage Decision
*-                             for Inspection Lot

  CALL FUNCTION 'BAPI_INSPLOT_GETDETAIL'
       EXPORTING
            NUMBER                = P_PRUEFLOS
*            LANGUAGE             =
       IMPORTING
            GENERAL_DATA          = WA_GENERAL_DATA
            TASK_LIST_DATA        = WA_TASK_LIST_DATA
            STOCK_DATA            = WA_STOCK_DATA
            ACCOUNTING_DATA       = WA_ACCOUNTING
            USAGE_DECISION_DATA   = WA_USAGE_DECISION
            CUSTOMER_INCLUDE_DATA = WA_CUSTOMER_DATA
            RETURN                = WA_RETURN
       TABLES
            SYSTEM_STATUS         = IT_SYS_STATUS
            USER_STATUS           = IT_USER_STATUS
            MATERIAL_DOCUMENTS    = IT_MAT_DOC.


*-- Mapping fields of BAPI Interface Parameters and PS_CALI

  MOVE : WA_GENERAL_DATA-INSPLOT          TO PS_CALI-PRUEFLOS,

*         WA_TASK_LIST_DATA-TASK_LIST_TYPE    TO PS_CALI-PLNTY,
*         WA_TASK_LIST_DATA-TASK_LIST_NUMBER  TO PS_CALI-PLNNR,

         WA_ACCOUNTING-ORDERID               TO PS_CALI-AUFNR,

*         WA_USAGE_DECISION-UD_RECORDED_ON_DATE   TO PS_CALI-VDATUM,
         WA_USAGE_DECISION-UD_CHANGED_ON_DATE    TO PS_CALI-VDATUM,
         WA_USAGE_DECISION-UD_CATALOG_TYPE       TO PS_CALI-VKATART,
         WA_USAGE_DECISION-UD_CODE_GROUP         TO PS_CALI-VCODEGRP,
         WA_USAGE_DECISION-UD_CODE               TO PS_CALI-VCODE,
         WA_USAGE_DECISION-IND_UD_LONG_TEXT     TO PS_CALI-LTEXTKZ.

ENDFORM.                    " GET_DATA_FROM_DB
*&-----------------------------------------------------------------*
*&      Form  GET_ETC_N_TEXT_DATA
*&-----------------------------------------------------------------*
FORM GET_ETC_N_TEXT_DATA USING PS_CALI STRUCTURE ZSQM_CALI_CERTI_HEADER
                         VALUE(P_PRUEFLOS) TYPE QPLOS.

*-- Get Equipment Data of Inslpection Lot
*  SELECT SINGLE A~EQUNR B~OBJNR C~EQKTX
*                B~HERST B~TYPBZ B~GROES B~BRGEW B~GEWEI
*      INTO CORRESPONDING FIELDS OF PS_CALI
*        FROM ( QAPP AS A         INNER JOIN EQUI AS B
*          ON A~EQUNR = B~EQUNR ) INNER JOIN EQKT AS C
*          ON B~EQUNR = C~EQUNR
*         WHERE A~PRUEFLOS = P_PRUEFLOS
*           AND C~SPRAS = SY-LANGU.

**-- Get Department and Text "/Related Human Resource(HR)
*  PERFORM GET_DEPARTMENT_DATA  USING PS_CALI-OBJNR     "/Noti Object
*                                     PS_CALI-PARNR_DEP "/Depart. No
*                                     PS_CALI-ORGTX_DEP "/Department T
*                                     'AB'.          "/Partner Function
*
**-- GET Charge and Name.
*  PERFORM GET_CHARGE_DATA     USING PS_CALI-OBJNR
*                                    PS_CALI-PARNR_CHG
*                                    PS_CALI-NAME_CHG
*                                    'VU'.          "/Partner Function
*
**-- Get Type of Inspection
*  PERFORM GET_TYPE_OF_INSPECTION   USING   PS_CALI-AUFNR
*                                  CHANGING PS_CALI-AUART
*                                           PS_CALI-TXT.
*
**-- Get Equipment User Status.
*  PERFORM GET_USER_STATUS_OF_OBJNR    USING PS_CALI-OBJNR
*                                   CHANGING PS_CALI-TXT30.

*-- Usage Decision Code Text
  PERFORM GET_CODE_TEXTS   USING    PS_CALI-VKATART
                                    PS_CALI-VCODEGRP
                                    PS_CALI-VCODE
                                    ''
                           CHANGING PS_CALI-KURZTEXT_V.



ENDFORM.                    " GET_ETC_N_TEXT_DATA
*&------------------------------------------------------------------*
*&      Form  GET_CODE_TEXTS
*&------------------------------------------------------------------*
FORM GET_CODE_TEXTS USING        P_KATALOGART
                                 P_CODEGRUPPE
                                 P_CODE
                           VALUE(P_VERSION)
                    CHANGING     P_KURZTEXT.

  DATA : LW_VERSION TYPE QVERSNR.
  IF P_VERSION = ''.
    LW_VERSION = '000001'.
  ELSE.
    MOVE P_VERSION TO LW_VERSION.
  ENDIF.

  SELECT SINGLE KURZTEXT   INTO P_KURZTEXT
     FROM QPCT
       WHERE KATALOGART  = P_KATALOGART
         AND CODEGRUPPE  = P_CODEGRUPPE
         AND CODE        = P_CODE
         AND VERSION     = LW_VERSION
         AND SPRACHE     = SY-LANGU.

  CHECK SY-SUBRC NE 0.
  CLEAR P_KURZTEXT.

ENDFORM.                    " GET_CODE_TEXTS
*&---------------------------------------------------------------------*
*&      Form  GET_DEPARTMENT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_NOTI_HDR_OBJNR  text
*      -->P_P_NOTI_HDR_PARNR_DEP  text
*      -->P_P_NOTI_HDR_ORGTX_DEP  text
*      -->P_0193   text
*----------------------------------------------------------------------*
FORM GET_DEPARTMENT_DATA USING    P_OBJNR
                                  P_PARNR_DEP
                                  P_ORGTX_DEP
                                  VALUE(P_PARVW).

  CHECK NOT P_OBJNR IS INITIAL.

  SELECT SINGLE A~ORGTX B~PARNR
                     INTO (P_ORGTX_DEP ,P_PARNR_DEP)
     FROM T527X AS A INNER JOIN IHPA AS B
       ON A~ORGEH = B~PARNR
       WHERE B~OBJNR = P_OBJNR
         AND B~PARVW = P_PARVW.

  CHECK SY-SUBRC NE 0.
  CLEAR : P_PARNR_DEP, P_ORGTX_DEP.


ENDFORM.                    " GET_DEPARTMENT_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_CHARGE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PS_CALI_OBJNR  text
*      -->P_PS_CALI_PARNR_CHG  text
*      -->P_PS_CALI_NAME_CHG  text
*      -->P_0468   text
*----------------------------------------------------------------------*
FORM GET_CHARGE_DATA    USING    P_OBJNR        "/Noti. Object No.
                                 P_PARNR        "/Partner No
                                 P_NAME_TEXTC   "/Partner Name Text
                                 VALUE(P_PARVW).  "/Partnet Function

  CHECK NOT P_OBJNR IS INITIAL.

  SELECT SINGLE  A~NAME_TEXTC B~PARNR
                           INTO (P_NAME_TEXTC ,P_PARNR)
      FROM USER_ADDR AS A INNER JOIN IHPA AS B
        ON A~BNAME = B~PARNR
        WHERE B~OBJNR = P_OBJNR
          AND B~PARVW = P_PARVW.

  CHECK SY-SUBRC NE 0.
  CLEAR: P_NAME_TEXTC ,P_PARNR.

ENDFORM.                     " GET_CHARGE_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_TYPE_OF_INSPECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PS_CALI_AUFNR  text
*      <--P_PS_CALI_AUART  text
*      <--P_PS_TXT  text
*----------------------------------------------------------------------*
FORM GET_TYPE_OF_INSPECTION USING    P_AUFNR
                            CHANGING P_AUART
                                     P_TXT.

  CHECK NOT P_AUFNR IS INITIAL.

  SELECT SINGLE A~AUART B~TXT
       INTO (P_AUART, P_TXT)
         FROM AUFK AS A INNER JOIN T003P AS B
           ON A~AUART = B~AUART
           WHERE A~AUFNR = P_AUFNR
             AND B~SPRAS = SY-LANGU.


ENDFORM.                    " GET_TYPE_OF_INSPECTION
*&---------------------------------------------------------------------*
*&      Form  GET_CALIBRATION_RESULT_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_CALI_RESULT  text
*      -->P_IT_PLANMK  text
*----------------------------------------------------------------------*
FORM GET_CALIBRATION_RESULT_LIST
         TABLES   PT_RESULT STRUCTURE ZSQM_CALI_CERTI_RESULT
                  PT_PLANMK STRUCTURE ZSQM_REF_DOC. "PLMKB.

  REFRESH PT_RESULT.

  SELECT  DISTINCT QMTB_WERKS PMETHODE PMTVERSION
         INTO CORRESPONDING FIELDS OF TABLE PT_PLANMK
         FROM QALS AS A
              INNER JOIN PLMK AS B
              ON   A~PLNTY = B~PLNTY
              AND  A~PLNNR = B~PLNNR
*              AND  A~PLNAL = B~PLNKN
              AND  A~ZAEHL = B~ZAEHL
         WHERE A~PRUEFLOS = P_PRUEFL.




*  CALL FUNCTION 'CQ_DB_PLMK_READ'
*       EXPORTING
*            PLNNR      = WA_TASK_LIST_DATA-TASK_LIST_NUMBER
*            PLNTY      = WA_TASK_LIST_DATA-TASK_LIST_TYPE
*       TABLES
*            PLANMK     = PT_PLANMK
*       EXCEPTIONS
*            NO_RECORDS = 1
*            OTHERS     = 2.
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.

*  CHECK SY-SUBRC = 0 AND NOT PT_PLANMK[] IS INITIAL.
*
*  LOOP AT PT_PLANMK.
*    CLEAR PT_RESULT.
*    MOVE-CORRESPONDING PT_PLANMK TO PT_RESULT.
*
**-- Catalog Text
*    PERFORM GET_CATALOG_TEXT   USING    PT_RESULT-KATALGART1
*                                        PT_RESULT-AUSWMENGE1
*                                        PT_RESULT-AUSWMGWRK1
*                               CHANGING PT_RESULT-KTX01
*                                        PT_RESULT-LTEXTV.
*
*
**-- Get Measuredata and error
*    PERFORM GET_MEASURE_DATA   USING P_PRUEFL
*                                     PT_RESULT-PLNKN
*                                     PT_RESULT-MERKNR
*                            CHANGING PT_RESULT-KATALGART_C
*                                     PT_RESULT-GRUPPE_C
*                                     PT_RESULT-CODE_C
*                                     PT_RESULT-VERSION_C
*                                     PT_RESULT-KURZTEXT_C
*                                     PT_RESULT-MBEWERTG.
*
**   -- Valuation Domain Fixed Value Text
*    IF NOT PT_RESULT-MBEWERTG IS INITIAL.
*      PERFORM GET_VALUATION_TEXT   USING    PT_RESULT-MBEWERTG
*                                   CHANGING PT_RESULT-DTEXT_MBEWERTG.
*
*    ENDIF.
*
*    APPEND PT_RESULT.
*  ENDLOOP.


ENDFORM.                    " GET_CALIBRATION_RESULT_LIST
*&-----------------------------------------------------------------*
*&      Form  GET_CATALOG_TEXT
*&-----------------------------------------------------------------*
FORM GET_CATALOG_TEXT USING    P_KATALGART
                               P_AUSWMENGE
                               P_AUSWMGWRK
                      CHANGING P_KTX01
                               P_LTEXTV.

  SELECT SINGLE KTX01 LTEXTV
     INTO (P_KTX01, P_LTEXTV)
       FROM QPAM
         WHERE WERKS      = P_AUSWMGWRK
           AND KATALOGART = P_KATALGART
           AND AUSWAHLMGE = P_AUSWMENGE.


ENDFORM.                    " GET_CATALOG_TEXT
*&---------------------------------------------------------------------*
*&      Form  GET_MEASURE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_PRUEFL  text
*      -->P_PT_RESULT_PLNKN  text
*      -->P_PT_RESULT_MERKNR  text
*      <--P_PT_RESULT_KATALGART_C  text
*      <--P_PT_RESULT_GRUPPE_C  text
*      <--P_PT_RESULT_CODE_C  text
*      <--P_PT_RESULT_VERSION_C  text
*      <--P_PT_RESULT_KURZTEXT_C  text
*----------------------------------------------------------------------*
FORM GET_MEASURE_DATA USING    P_PRUEFL
                               P_PLNKN   "/VORGLFNR
                               P_MERKNR
                      CHANGING P_KATALGART_C
                               P_GRUPPE_C
                               P_CODE_C
                               P_VERSION_C
                               P_KURZTEXT_C
                               P_MBEWERTG.

  SELECT SINGLE KATALGART1
                GRUPPE1
                CODE1
                VERSION1
                MBEWERTG
          INTO (P_KATALGART_C, P_GRUPPE_C, P_CODE_C,
                P_VERSION_C, P_MBEWERTG)
       FROM QASR
         WHERE  PRUEFLOS = P_PRUEFL
*           AND  VORGLFNR = P_PLNKN
           AND  MERKNR   = P_MERKNR.

  CHECK SY-SUBRC = 0.

  PERFORM GET_CODE_TEXTS   USING    P_KATALGART_C
                                    P_GRUPPE_C
                                    P_CODE_C
                                    P_VERSION_C
                           CHANGING P_KURZTEXT_C.

ENDFORM.                    " GET_MEASURE_DATA
*&-----------------------------------------------------------------*
*&      Form  GET_VALUATION_TEXT
*&-----------------------------------------------------------------*
FORM GET_VALUATION_TEXT USING    P_MBEWERTG
                        CHANGING P_DTEXT.

  DATA : LWA_MBEWERTG TYPE DOMVALUE_L.
  MOVE P_MBEWERTG TO LWA_MBEWERTG .

  CALL FUNCTION 'C_DIC_DOMAIN_VALUE_TEXT_READ'
       EXPORTING
            NAME      = 'QEEBEWERTG'
            SPRAS     = SY-LANGU
            VALUE     = LWA_MBEWERTG
       IMPORTING
            TEXT      = P_DTEXT
       EXCEPTIONS
            NOT_FOUND = 1
            OTHERS    = 2.
  IF SY-SUBRC <> 0.
    CLEAR P_DTEXT.
  ENDIF.


ENDFORM.                    " GET_VALUATION_TEXT
*&---------------------------------------------------------------------*
*&      Form  GET_MS_NUMBER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_MS_NUMBER.

*-- Make  Document Object Key
*--  When OBJKY will be composed by concatenating of QMTB_WERKS,PMETHODE
*--  and  PMTVERSION. So it can't use original PMETHODE.
*--  PMETHODE field data will be moved to LV_PMETHOD, which is filled
*--  with '#'. and OBJKY will be concatenated Using LV_PMETHOD. Then
*--  Character '#' will be convert to SPACE.
  CLEAR IT_MS_DOC_OBJ. REFRESH IT_MS_DOC_OBJ.
  DATA : LV_PMETHOD(8).

  LOOP AT IT_PLANMK.

    MOVE-CORRESPONDING IT_PLANMK TO IT_MS_DOC_OBJ.

    LV_PMETHOD = '########'.
    CONCATENATE IT_MS_DOC_OBJ-PMETHODE LV_PMETHOD  INTO LV_PMETHOD.

    CONCATENATE IT_MS_DOC_OBJ-QMTB_WERKS
                LV_PMETHOD
                IT_MS_DOC_OBJ-PMTVERSION
                           INTO IT_MS_DOC_OBJ-OBJKY.

*-     Replace '#' to SPACE for OBJKY.
    PERFORM REPLACE_OF_SYMBOL_TO_SPACE USING IT_MS_DOC_OBJ-OBJKY
                                             '#'.

    APPEND IT_MS_DOC_OBJ.
  ENDLOOP.

  CHECK NOT IT_MS_DOC_OBJ[] IS INITIAL.

*-- Get Document No.(DRAD-DOKNR) Using OBJKY.
  SELECT DISTINCT  DOKNR OBJKY
    INTO CORRESPONDING FIELDS OF TABLE   IT_MS_DOC
      FROM DRAD
       FOR ALL ENTRIES IN IT_MS_DOC_OBJ
         WHERE OBJKY = IT_MS_DOC_OBJ-OBJKY.

ENDFORM.                    " GET_MS_NUMBER
*&------------------------------------------------------------------*
*&      Form  REPLACE_OF_SYMBOL_TO_SPACE
*&------------------------------------------------------------------*
FORM REPLACE_OF_SYMBOL_TO_SPACE USING    P_STRING
                                      VALUE(P_CHSTR).

  DO.
    SEARCH P_STRING FOR P_CHSTR .
    IF SY-SUBRC NE 0. EXIT. ENDIF.
    REPLACE P_CHSTR WITH SPACE INTO P_STRING.

  ENDDO.

ENDFORM.                    " REPLACE_OF_SYMBOL_TO_SPACE
*&---------------------------------------------------------------------*
*&      Form  GET_USER_STATUS_OF_OBJNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PS_CALI_OBJNR  text
*      <--P_PS_CALI_TXT30  text
*----------------------------------------------------------------------*
FORM GET_USER_STATUS_OF_OBJNR USING    P_OBJNR
                              CHANGING P_TXT30.

  SELECT SINGLE TXT30  INTO P_TXT30
      FROM ZVQM_OBJ_STATUS
        WHERE OBJNR = P_OBJNR.


ENDFORM.                    " GET_USER_STATUS_OF_OBJNR
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_FROM_DB2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZSQM_CALI_CERTI_HEADER  text
*      -->P_P_PRUEFL  text
*----------------------------------------------------------------------*
FORM GET_DATA_FROM_DB2 USING  PS_CALI  STRUCTURE ZSQM_CALI_CERTI_HEADER
                              VALUE(P_PRUEFLOS) TYPE QPLOS.

  MOVE : P_PRUEFLOS TO ZSQM_CALI_CERTI_HEADER-PRUEFLOS.
*** Date
  SELECT SINGLE VDATUM INTO ZSQM_CALI_CERTI_HEADER-VDATUM
                FROM QAVE
                WHERE PRUEFLOS = P_PRUEFLOS.
*** Equip. Number
  SELECT SINGLE A~AUFNR B~EQUNR
                INTO (ZSQM_CALI_CERTI_HEADER-AUFNR,
                      ZSQM_CALI_CERTI_HEADER-EQUNR)
                FROM QALS AS A
                    INNER JOIN AFIH AS B
                    ON A~AUFNR = B~AUFNR
                WHERE A~PRUEFLOS = P_PRUEFLOS.
*** Equip. Description
  SELECT SINGLE A~OBJNR B~EQKTX A~HERST A~TYPBZ
                A~BRGEW A~GEWEI A~GROES
               INTO (ZSQM_CALI_CERTI_HEADER-OBJNR,
                     ZSQM_CALI_CERTI_HEADER-EQKTX,
                     ZSQM_CALI_CERTI_HEADER-HERST,
                     ZSQM_CALI_CERTI_HEADER-TYPBZ,
                     ZSQM_CALI_CERTI_HEADER-BRGEW,
                     ZSQM_CALI_CERTI_HEADER-GEWEI,
                     ZSQM_CALI_CERTI_HEADER-GROES)
               FROM EQUI AS A
                    INNER JOIN EQKT AS B
                    ON A~EQUNR = B~EQUNR
               WHERE A~EQUNR = ZSQM_CALI_CERTI_HEADER-EQUNR
               AND   B~SPRAS = SY-LANGU.
**** Department
  SELECT SINGLE A~PARNR B~ORGTX
               INTO (ZSQM_CALI_CERTI_HEADER-PARNR_DEP,
                     ZSQM_CALI_CERTI_HEADER-ORGTX_DEP)
               FROM IHPA AS A
                   INNER JOIN T527X  AS B
                   ON B~ORGEH = A~PARNR
               WHERE A~OBJNR = ZSQM_CALI_CERTI_HEADER-OBJNR
               AND   A~PARVW = 'AB'
               AND   B~SPRSL = SY-LANGU
               AND   B~ENDDA = '99991231'.
**** Charge
  SELECT SINGLE B~NAME_TEXTC
              INTO ZSQM_CALI_CERTI_HEADER-NAME_CHG
              FROM IHPA AS A
                  INNER JOIN USER_ADDR  AS B
                  ON B~BNAME = A~PARNR
              WHERE A~OBJNR = ZSQM_CALI_CERTI_HEADER-OBJNR
              AND   A~PARVW = 'VB'.
*** Order Type
  SELECT SINGLE B~TXT INTO ZSQM_CALI_CERTI_HEADER-TXT
                FROM AUFK AS A
                    INNER JOIN T003P AS B
                    ON A~AUART = B~AUART
                WHERE A~AUFNR = ZSQM_CALI_CERTI_HEADER-AUFNR
                AND   B~SPRAS = SY-LANGU.

ENDFORM.                    " GET_DATA_FROM_DB2

************************************************************************
* Program Name      : ZSQM01U_NOTI_IMPROVE_FORM_q1
* Author            : Chris Li
* Creation Date     : 12/17/2004
* Specifications By : RASHMIT
* Pattern           : 1.1.2 General
* Development Request No : UD1K913539
* Addl Documentation:
* Description       : Improvement Report for Notification Using Samrt
*                     Forms for notification type Q1
*                     COPY AND CHANGED FROM ZSQM01U_NOTI_IMPROVE_FORM
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT  ZSQM01U_NOTI_IMPROVE_FORM_Q1     .

*&&& Data Declaration.  &&&*
TYPE-POOLS VRM.     "//Value Request Manager: Types & Constants
*TYPE-POOLS CXTAB .  "//Table_control Object type pool
TABLES : FELD.      "//Screen Object Structure

*-- SAP Scripts Object Interface
TABLES : THEAD. "/SAPscript: Text Header

*//Tables;(TABLES : Table_Name  "//Table Description)
TABLES : QMEL.   "/Quality Notification
TABLES : QMSM.

*//Structures Declaration(TABLES : Structure Name."/Description)
TABLES : ZSQM_NOTI_IMPROV_HEADER.   "/Improvement Report for Noti. -
*                                     Header Data Structure
TABLES : ZSQM_NOTI_IMPROV_DEFECT.   "/Defect type Structure
TABLES : ZSQM_NOTI_IMPROV_DEF_LOC.  "/Defect Location Structure
TABLES : ZSQM_NOTI_IMPROV_REASON.   "/Reason of Defect Data Structure
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

*--
DATA : WA_RETURN     LIKE	BAPIRETURN1.   "Return Values

DATA : WA_FORMNAME TYPE TDSFNAME  VALUE 'ZSQM01_NOTI_IMPRV_V1'.
*                                             "/Smart Form Name
DATA : WA_CONTACT_NR LIKE QMSM-PARNR.  "CONTACT PERSON
DATA : WA_CONTACT_NAME TYPE AD_NAMTEXT.
*-- User Confirm for pop-up Message
DATA : WA_ANSWER TYPE C.
DATA : WA_STATUS LIKE RIWO00-STTXT.
*//Data(Work Area or (Internal) Structures);(WA_ )(ST_)?



*//Internal Tables and Index Fields;(IT_), (I_)
DATA : IT_DEFECT LIKE ZSQM_NOTI_IMPROV_DEFECT   "/Defect type
                    OCCURS 0  WITH HEADER LINE.
DATA : IT_DEF_LOC LIKE ZSQM_NOTI_IMPROV_DEF_LOC  "/Defect Location
                    OCCURS 0 WITH HEADER LINE.
DATA : IT_REASON LIKE ZSQM_NOTI_IMPROV_REASON  "/ Reason of Defect Data
                    OCCURS 0  WITH HEADER LINE.
DATA : IT_REPORT LIKE ZSQM_INSP_RESULT_04       "/Report(Document) No
                    OCCURS 0  WITH HEADER LINE.

*-- Internal Tables for Long Text of Notification
DATA : IT_QMEL_LTQM  LIKE TLINE OCCURS 0,  "/Content of Request/Comment
       IT_ZQMEL_ZQM1 LIKE TLINE OCCURS 0,  "/Description of problem
       IT_ZQMEL_ZQM2 LIKE TLINE OCCURS 0,  "/Content of first resolution
       IT_ZQMEL_ZQM3 LIKE TLINE OCCURS 0.  "content of second resolution
DATA : WA_TXTNAME    LIKE THEAD-TDNAME.    " TEXT NAME

DATA : BEGIN OF FIELDS OCCURS 0.
        INCLUDE STRUCTURE HELP_VALUE.
DATA : END OF FIELDS.
DATA  SELECT_INDEX LIKE SY-TABIX.
DATA : SELECT_VALUE(10).
DATA : BEGIN OF SELECT_VALUES OCCURS 0.
        INCLUDE STRUCTURE HELP_VTAB.
DATA : END OF SELECT_VALUES.
DATA : BEGIN OF VALUETAB OCCURS 0,
          VALUE(30).
DATA : END OF VALUETAB.


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
PARAMETERS : P_QMNUM  LIKE QMEL-QMNUM OBLIGATORY.  "/Notification Number

SELECTION-SCREEN END OF BLOCK BLK .

*-- Seclection Screen Flow Logic Control Event Handling
*AT SELECTION-SCREEN ON ( ON END OF, ON VALUE-REQUEST FOR,
* ON HELP-REQUEST FOR, ON RADIOBUTTON GROUP, ON BLOCK OUTPUT,
* ON EXIT-COMMAND )
AT SELECTION-SCREEN OUTPUT.
  SET TITLEBAR 'NOTI'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_QMNUM.
  PERFORM MAKE_NOTIF_LIST.


AT SELECTION-SCREEN ON P_QMNUM.
  CHECK NOT P_QMNUM IS INITIAL.
  SELECT SINGLE *
     FROM QMEL
       WHERE QMNUM = P_QMNUM.
  CHECK SY-SUBRC NE 0.
  MESSAGE E000(ZMQM)
              WITH 'Not Found Notification Number. '(E01) P_QMNUM.

*-- Selection for Selection Screen
START-OF-SELECTION.

END-OF-SELECTION.

*-- Get Header Data Using Notification Which was entered.
  PERFORM GET_HEADER_DATA_FROM_QMNUM  USING ZSQM_NOTI_IMPROV_HEADER
                                            P_QMNUM.

*-- Get other data which are not in QMEL
  PERFORM GET_ETC_DATA_FOR_HEADER   USING ZSQM_NOTI_IMPROV_HEADER.

*-- Get Defect Type List  -> IT_DEFECT
  PERFORM GET_DEFECT_TYPE_LIST  TABLES IT_DEFECT
                                USING P_QMNUM.

*-- Get Defect Location List -> IT_DEF_LOC
  PERFORM GET_DEFECT_LOCATION_LIST TABLES IT_DEF_LOC
                                   USING P_QMNUM.

*-- Get Reason of Defect List -> IT_REASON
  PERFORM GET_REASON_OF_DEFECT_LIST  TABLES IT_REASON
                                     USING  P_QMNUM.

*-- Get Report Number List  -> IT_REPORT.
  PERFORM GET_REPORT_NO_LIST    TABLES IT_REPORT
                                USING  P_QMNUM.

*-- /Get Long Text for Notification
*  - Content of Request/Comment
*  PERFORM GET_LONGTEXT    TABLES IT_QMEL_LTQM  "/Text Table
*                          USING 'QMEL'         "/Text Object
*                                'LTQM'         "/Text ID
*                                 P_QMNUM.      "/Text ID Name
*  - Description of problem
  WA_TXTNAME = P_QMNUM.
  PERFORM GET_LONGTEXT    TABLES IT_ZQMEL_ZQM1
                          USING 'QMEL'
                                'LTQM'
                                 WA_TXTNAME.
*  - Content o first resolution
  DATA : L_TASK(4) TYPE C.
  DATA : S_NO_EXIST.
  CLEAR: WA_TXTNAME, S_NO_EXIST.
* CHECK IF THE RESOLUTION ITEM EXIST
  L_TASK = '01'.
  PERFORM CHECK_TASK USING P_QMNUM L_TASK S_NO_EXIST.
  IF S_NO_EXIST IS INITIAL.
    CONCATENATE P_QMNUM L_TASK INTO WA_TXTNAME.
    PERFORM GET_LONGTEXT    TABLES IT_ZQMEL_ZQM2
                            USING 'QMSM'
                                  'LTQM'
                                   WA_TXTNAME.
  ENDIF.
*  - Content of second resolution
  CLEAR: WA_TXTNAME, S_NO_EXIST.
* CHECK IF THE RESOLUTION ITEM EXIST, IF NOT,
* IT'S NOT NECCESSORY TO READ THE TEXT.
  L_TASK = '02'.
  PERFORM CHECK_TASK USING P_QMNUM L_TASK S_NO_EXIST.
  IF S_NO_EXIST IS INITIAL.
    CONCATENATE P_QMNUM L_TASK INTO WA_TXTNAME.
    PERFORM GET_LONGTEXT    TABLES IT_ZQMEL_ZQM3
                            USING 'QMSM'
                                  'LTQM'
                                   WA_TXTNAME.
  ENDIF.
*  -Contact person
  PERFORM GET_CONTACTPERSON USING P_QMNUM.
* SYSTEM STATUS.
  PERFORM GET_STATUS.

*-- Print out Form
  PERFORM PRINT_FORMS  USING WA_FORMNAME.


*// Event Handling(Except Selection Screen (Flow)event)
*Load of Program.

*Initialization.

***//Macro Definitions
*DEFINE RANGE_MACRO.
*END-OF-DEFINITION.

**<<<<<<<<< Program Main / Subroutine / Flow Logic >>>>>>>>>>>>**
*&-----------------------------------------------------------------*
*&      Form  GET_HEADER_DATA_FROM_QMNUM
*&-----------------------------------------------------------------*
FORM GET_HEADER_DATA_FROM_QMNUM
             USING  P_NOTI_HDR STRUCTURE ZSQM_NOTI_IMPROV_HEADER
                    P_QMNUM    TYPE QMNUM.

  SELECT SINGLE *
     INTO CORRESPONDING FIELDS OF P_NOTI_HDR
       FROM QMEL
         WHERE QMNUM = P_QMNUM.


ENDFORM.                    " GET_HEADER_DATA_FROM_QMNUM

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

** "/Dev-'/1BCDWB/SF00000028'
  CALL FUNCTION LW_FUNCNAME
       EXPORTING
            I_IMPRV_HDR      = ZSQM_NOTI_IMPROV_HEADER
            WA_CONTACT       = WA_CONTACT_NR
            WA_STATUS        = WA_STATUS
       TABLES
            T_DEFECT         = IT_DEFECT
            T_DEF_LOC        = IT_DEF_LOC
            T_REASON         = IT_REASON
            T_REPORT         = IT_REPORT
            T_QMEL_LTQM      = IT_QMEL_LTQM
            T_ZQMEL_ZQM1     = IT_ZQMEL_ZQM1
            T_ZQMEL_ZQM2     = IT_ZQMEL_ZQM2
            T_ZQMEL_ZQM3     = IT_ZQMEL_ZQM3
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
*&      Form  GET_ETC_DATA_FOR_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZSQM_NOTI_IMPROV_HEADER  text
*----------------------------------------------------------------------*
FORM GET_ETC_DATA_FOR_HEADER
                USING  P_NOTI_HDR    STRUCTURE ZSQM_NOTI_IMPROV_HEADER.

**-- GET REQUESTOR and Name.
*  PERFORM GET_REQ_AUTHOR_DATA  USING P_NOTI_HDR-OBJNR
*                                     P_NOTI_HDR-PARNR_REQ
*                                     P_NOTI_HDR-NAME_REQ
*                                     'KU'.          "/Partner Function

**-- GET Manager and Name. => PARNR_REQ : Manager (12/23:sllee)
  PERFORM GET_REQ_AUTHOR_DATA  USING P_NOTI_HDR-OBJNR
                                     P_NOTI_HDR-PARNR_REQ
                                     P_NOTI_HDR-NAME_REQ
                                     'Z2'.   "/Partner Function(Manager)

*-- GET Author and Name.
  PERFORM GET_REQ_AUTHOR_DATA  USING P_NOTI_HDR-OBJNR
                                     P_NOTI_HDR-PARNR_ATH
                                     P_NOTI_HDR-NAME_ATH
                                     'Z1'.          "/Partner Function

*-- Get Resp. Department and Text "/Related Human Resource(HR)
  PERFORM GET_DEPARTMENT_DATA  USING P_NOTI_HDR-OBJNR     "/Noti Object
                                     P_NOTI_HDR-PARNR_DEP "/Depart. No
                                     P_NOTI_HDR-ORGTX_DEP "/Department T
                                     'Z3'.  "'AB'.  "/Partner Function


*-- Get Priority Data.
  PERFORM GET_PRIORITY_DATA    USING P_NOTI_HDR-ARTPR  "/Priority Type
                                     P_NOTI_HDR-PRIOK  "/Priority
                            CHANGING P_NOTI_HDR-PRIOKX. "/Priority Text

*-- Get Plant Name
  PERFORM GET_PLANT_DATA       USING P_NOTI_HDR-MAWERK     "/Plant
                            CHANGING P_NOTI_HDR-NAME_WERK. "/Plant Name

*-- Get Vehicle Data
*  PERFORM GET_MAT_VEHICLE_D   USING P_NOTI_HDR-VEHICLE  "/Vehicle
*                           CHANGING P_NOTI_HDR-MAKTX_VH."/Vehicle Name
  PERFORM GET_VEHICLE_NAME_AND_CHECK USING  P_NOTI_HDR-KATART_VH
                                            P_NOTI_HDR-CODEGRP_VH
                                            P_NOTI_HDR-CODE_VH
                                CHANGING    P_NOTI_HDR-KURZTEXT_VH.


*-- Get Part Name
  PERFORM GET_MAT_VEHICLE_D   USING P_NOTI_HDR-MATNR     "/material
                           CHANGING P_NOTI_HDR-MAKTX_MAT."/Description

*-- Get System Type
  PERFORM GET_TEXT_OF_TWEWT USING    P_NOTI_HDR-EXTWG
                            CHANGING P_NOTI_HDR-EWBEZ.

*-- Get Supplier(Venodr) Data
  PERFORM GET_SUPPLIER_DATA    USING P_NOTI_HDR-OBJNR    "/Noti Object
                                     P_NOTI_HDR-PARNR_SP "/Supplier Code
                                     P_NOTI_HDR-NAME_SP  "/Supplier Name
                                     'Z5'.  "'LF'.  "/Partner Function

*-- Get Occurrence Date
  PERFORM GET_OCCURRENCE_DATE USING P_NOTI_HDR-QMNUM "/Noti. No
                                    P_NOTI_HDR-AUSVN."/Occurrence Date

*-- Get Defect Location, Activity Type by writer and Disposal Type
*- Short Text for Code of Defect(Occurrence) Location
  PERFORM GET_TEXT_OF_CODE_QPCT      USING P_NOTI_HDR-KATART_OC
                                           P_NOTI_HDR-CODEGRP_OC
                                           P_NOTI_HDR-CODE_OC
                                  CHANGING P_NOTI_HDR-KURZTEXT_OC.
*- Short Text for Code of Activity(Action) Type
  PERFORM GET_TEXT_OF_CODE_QPCT      USING P_NOTI_HDR-KATART_AT
                                           P_NOTI_HDR-CODEGRP_AT
                                           P_NOTI_HDR-CODE_AT
                                  CHANGING P_NOTI_HDR-KURZTEXT_AT.


**- Short Text for Code of Disposal Type
*  PERFORM GET_TEXT_OF_CODE_QPCT      USING P_NOTI_HDR-KATART_DT
*                                           P_NOTI_HDR-CODEGRP_DT
*                                           P_NOTI_HDR-CODE_DT
*                                  CHANGING P_NOTI_HDR-KURZTEXT_DT.


ENDFORM.                    " GET_ETC_DATA_FOR_HEADER
*&------------------------------------------------------------------*
*&      Form  GET_REQ_AUTHOR_DATA
*&------------------------------------------------------------------*
FORM GET_REQ_AUTHOR_DATA USING    P_OBJNR        "/Noti. Object No.
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

ENDFORM.                    " GET_REQ_AUTHOR_DATA
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
*&      Form  GET_PRIORITY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_NOTI_HDR_ARTPR  text
*      -->P_P_NOTI_HDR_PRIOK  text
*      <--P_P_NOTI_HDR_PRIOKX  text
*----------------------------------------------------------------------*
FORM GET_PRIORITY_DATA USING    P_ARTPR
                                P_PRIOK
                       CHANGING P_PRIOKX.
  CHECK NOT P_ARTPR IS INITIAL AND
        NOT P_PRIOK IS INITIAL.

  SELECT SINGLE PRIOKX   INTO P_PRIOKX
    FROM T356_T
      WHERE  SPRAS = SY-LANGU
        AND  ARTPR = P_ARTPR
        AND  PRIOK = P_PRIOK.

  CHECK SY-SUBRC NE 0.
  CLEAR P_PRIOKX.

ENDFORM.                    " GET_PRIORITY_DATA
*&------------------------------------------------------------------*
*&      Form  GET_PLANT_DATA
*&-------------------------------------------------------------------*
FORM GET_PLANT_DATA USING    P_WERKS
                    CHANGING P_WERKS_NAME.

  CHECK NOT P_WERKS IS INITIAL.
  CLEAR P_WERKS_NAME.

  SELECT SINGLE NAME1   INTO P_WERKS_NAME
     FROM T001W
       WHERE WERKS = P_WERKS.


ENDFORM.                    " GET_PLANT_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_MAT_VEHICLE_D
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_NOTI_HDR_VEHICLE  text
*      <--P_P_NOTI_HDT_MAKTX_VH  text
*----------------------------------------------------------------------*
FORM GET_MAT_VEHICLE_D USING    P_MATNR
                       CHANGING P_MAKTX.

  CHECK NOT P_MATNR IS INITIAL.
  SELECT SINGLE MAKTX INTO P_MAKTX
    FROM MAKT
      WHERE MATNR = P_MATNR
        AND SPRAS = SY-LANGU.

ENDFORM.                    " GET_MAT_VEHICLE_D
*&---------------------------------------------------------------------*
*&      Form  GET_TEXT_OF_TWEWT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EXTWG  text
*      <--P_EWBEZ  text
*----------------------------------------------------------------------*
FORM GET_TEXT_OF_TWEWT USING    P_EXTWG
                       CHANGING P_EWBEZ.
  CHECK NOT P_EXTWG IS INITIAL.

  SELECT SINGLE EWBEZ INTO P_EWBEZ
      FROM ZVQM_TWEWT
        WHERE EXTWG = P_EXTWG.

ENDFORM.                    " GET_TEXT_OF_TWEWT
*&---------------------------------------------------------------------*
*&      Form  GET_SUPPLIER_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_NOTI_HDR_OBJNR  text
*      -->P_P_NOTI_HDR_PARNR_SP  text
*      -->P_P_NOTI_HDR_NAME_SP  text
*      -->P_0231   text
*----------------------------------------------------------------------*
FORM GET_SUPPLIER_DATA USING    P_OBJNR
                                P_PARNR
                                P_NAME
                           VALUE(P_PARVW).

  CHECK NOT P_OBJNR IS INITIAL.

  SELECT SINGLE A~NAME1 B~PARNR
                     INTO (P_NAME , P_PARNR)
     FROM LFA1 AS A INNER JOIN IHPA AS B
       ON A~LIFNR = B~PARNR
       WHERE B~OBJNR = P_OBJNR
         AND B~PARVW = P_PARVW.

  CHECK SY-SUBRC NE 0.
  CLEAR : P_NAME,  P_PARNR.

ENDFORM.                    " GET_SUPPLIER_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_OCCURRENCE_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_NOTI_HDR_QMNUM  text
*      -->P_P_NOTI_HDR_AUSVN  text
*----------------------------------------------------------------------*
FORM GET_OCCURRENCE_DATE USING    P_QMNUM
                                  P_AUSVN.

  SELECT SINGLE AUSVN  INTO P_AUSVN
    FROM QMIH
      WHERE QMNUM = P_QMNUM.

ENDFORM.                    " GET_OCCURRENCE_DATE
*&---------------------------------------------------------------------*
*&      Form  GET_TEXT_OF_CODE_QPCT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_NOTI_HDR_KATART_DT  text
*      -->P_P_NOTI_HDR_CODEGRP_DT  text
*      -->P_P_NOTI_HDR_CODE_DT  text
*      <--P_P_NOTI_HDR_KURZTEXT_DT  text
*----------------------------------------------------------------------*
FORM GET_TEXT_OF_CODE_QPCT USING    P_KATART
                                    P_CODEGRP
                                    P_CODE
                           CHANGING P_KURZTEXT.

  SELECT SINGLE KURZTEXT_C INTO P_KURZTEXT
     FROM ZVQM_OCCR_LOC
       WHERE KATALOGART  = P_KATART
         AND CODEGRUPPE  = P_CODEGRP
         AND CODE        = P_CODE.

ENDFORM.                    " GET_TEXT_OF_CODE_QPCT
*&---------------------------------------------------------------------*
*&      Form  GET_DEFECT_TYPE_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DEFECT  text
*      -->P_P_QMNUM  text
*----------------------------------------------------------------------*
FORM GET_DEFECT_TYPE_LIST
                   TABLES   PT_DEFECT STRUCTURE ZSQM_NOTI_IMPROV_DEFECT
                   USING    P_QMNUM  TYPE QMNUM.

  REFRESH PT_DEFECT.

  SELECT B~FENUM B~FEKAT
         B~FEGRP B~FECOD
         C~KURZTEXT AS KURZTEXT_FE
        INTO CORRESPONDING FIELDS OF TABLE PT_DEFECT
           FROM ( QMEL AS A INNER JOIN QMFE AS B
             ON  A~QMNUM = B~QMNUM      ) INNER JOIN QPCT AS C
             ON  B~FEKAT = C~KATALOGART
             AND B~FEGRP = C~CODEGRUPPE
             AND B~FECOD = C~CODE
          WHERE A~QMNUM = P_QMNUM
            AND C~SPRACHE = SY-LANGU
            AND C~VERSION = '000001'.

ENDFORM.                    " GET_DEFECT_TYPE_LIST
*&---------------------------------------------------------------------*
*&      Form  GET_REASON_OF_DEFECT_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_REASON  text
*      -->P_P_QMNUM  text
*----------------------------------------------------------------------*
FORM GET_REASON_OF_DEFECT_LIST
                 TABLES   PT_REASON STRUCTURE ZSQM_NOTI_IMPROV_REASON
                 USING    P_QMNUM.

  REFRESH PT_REASON.

  SELECT B~FENUM D~URNUM D~URKAT D~URGRP D~URCOD D~URVER
         C~KURZTEXT AS KURZTEXT_UR
        INTO CORRESPONDING FIELDS OF TABLE PT_REASON
           FROM ( ( QMEL AS A INNER JOIN QMFE AS B
             ON  A~QMNUM = B~QMNUM    ) INNER JOIN QMUR AS D
             ON  B~QMNUM = D~QMNUM
             AND B~FENUM = D~FENUM    ) INNER JOIN QPCT AS C
             ON  D~URKAT = C~KATALOGART
             AND D~URGRP = C~CODEGRUPPE
             AND D~URCOD = C~CODE
             AND D~URVER = C~VERSION
          WHERE A~QMNUM = P_QMNUM
            AND C~SPRACHE = SY-LANGU.

ENDFORM.                    " GET_REASON_OF_DEFECT_LIST
*&---------------------------------------------------------------------*
*&      Form  GET_REPORT_NO_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_REPORT  text
*      -->P_P_QMNUM  text
*----------------------------------------------------------------------*
FORM GET_REPORT_NO_LIST TABLES  PT_REPORT STRUCTURE ZSQM_INSP_RESULT_04
                        USING   P_QMNUM.

  DATA : LW_OBJKY LIKE DRAD-OBJKY.

  MOVE: P_QMNUM TO LW_OBJKY.

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
                           P_QMNUM            LIKE THEAD-TDNAME.
  REFRESH PT_TLINE.
  CLEAR THEAD.

  MOVE : P_TDOBJECT TO THEAD-TDOBJECT,
         P_QMNUM    TO THEAD-TDNAME,
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
*&      Form  GET_VEHICLE_NAME_AND_CHECK
*&------------------------------------------------------------------*
FORM GET_VEHICLE_NAME_AND_CHECK USING    P_KATART_VH
                                         P_CODEGRP_VH
                                         P_CODE_VH
                              CHANGING   P_KURZTEXT_VH.
  CLEAR  P_KURZTEXT_VH.

  SELECT SINGLE KURZTEXT_C INTO P_KURZTEXT_VH
     FROM ZVQM_VEHICLE
       WHERE KATALOGART = P_KATART_VH
         AND CODEGRUPPE = P_CODEGRP_VH
         AND CODE       = P_CODE_VH.

  CHECK SY-SUBRC NE 0.
  MESSAGE E000(ZMPM) WITH P_CODEGRP_VH
                          P_CODE_VH
                          ',Not Exist Vehicle/Engine Code.'(EM3).

ENDFORM.                    " GET_VEHICLE_NAME_AND_CHECK
*&------------------------------------------------------------------*
*&      Form  GET_DEFECT_LOCATION_LIST
*&------------------------------------------------------------------*
FORM GET_DEFECT_LOCATION_LIST
                   TABLES PT_DEF_LOC STRUCTURE ZSQM_NOTI_IMPROV_DEF_LOC
                   USING  P_QMNUM  TYPE QMNUM.

  REFRESH PT_DEF_LOC.

  SELECT B~FENUM B~OTKAT
         B~OTGRP B~OTEIL
         C~KURZTEXT AS KURZTEXT_OT
        INTO CORRESPONDING FIELDS OF TABLE PT_DEF_LOC
           FROM ( QMEL AS A INNER JOIN QMFE AS B
             ON  A~QMNUM = B~QMNUM      ) INNER JOIN QPCT AS C
             ON  B~OTKAT = C~KATALOGART
             AND B~OTGRP = C~CODEGRUPPE
             AND B~OTEIL = C~CODE
          WHERE A~QMNUM = P_QMNUM
            AND C~SPRACHE = SY-LANGU
            AND C~VERSION = '000001'.

ENDFORM.                    " GET_DEFECT_LOCATION_LIST
*&---------------------------------------------------------------------*
*&      Form  GET_CONTACTPERSON
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_QMNUM  text
*----------------------------------------------------------------------*
form GET_CONTACTPERSON using  p_qmnum LIKE QMEL-QMNUM.
  DATA : LT_QMSM LIKE QMSM OCCURS 0 WITH HEADER LINE.

  SELECT QMNUM MANUM MNCOD PARNR
   INTO CORRESPONDING FIELDS OF TABLE LT_QMSM
   FROM QMSM
   WHERE QMNUM    = P_QMNUM AND
         KZLOESCH NE 'X'
   ORDER BY MANUM.

  IF SY-SUBRC = 0.
   READ TABLE LT_QMSM WITH KEY MNCOD = '02'.
   IF SY-SUBRC = 0.
     WA_CONTACT_NR = LT_QMSM-PARNR.
   ELSE.
     READ TABLE LT_QMSM WITH KEY MNCOD = '01'.
     WA_CONTACT_NR = LT_QMSM-PARNR.
   ENDIF.

  ENDIF.


endform.                    " GET_CONTACTPERSON
*&---------------------------------------------------------------------*
*&      Form  MAKE_NOTIF_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form MAKE_NOTIF_LIST.
   DATA : BEGIN OF IT_NOTIF OCCURS 0,
          QMNUM    LIKE  QMEL-QMNUM,
          QMART    LIKE  QMEL-QMART,
          QMTXT    LIKE  QMEL-QMTXT,
          ERNAM    LIKE  QMEL-ERNAM,
          ERDAT    LIKE  QMEL-ERDAT.
  DATA : END OF IT_NOTIF.

   CLEAR VALUETAB. REFRESH VALUETAB.
   CLEAR FIELDS.   REFRESH FIELDS.
   SELECT QMNUM QMART QMTXT ERNAM ERDAT
       INTO TABLE IT_NOTIF
       FROM QMEL
       WHERE QMART = 'Q1'.

    LOOP AT IT_NOTIF.
      VALUETAB-VALUE = IT_NOTIF-QMNUM.
      APPEND VALUETAB.
      CLEAR VALUETAB.
      VALUETAB-VALUE = IT_NOTIF-QMART.
      APPEND VALUETAB.
      CLEAR VALUETAB.
      VALUETAB-VALUE = IT_NOTIF-QMTXT.
      APPEND VALUETAB.
      CLEAR VALUETAB.
      VALUETAB-VALUE = IT_NOTIF-ERNAM.
      APPEND VALUETAB.
      CLEAR VALUETAB.
      VALUETAB-VALUE = IT_NOTIF-ERDAT.
      APPEND VALUETAB.
      CLEAR VALUETAB.

    ENDLOOP.

  PERFORM ADD_FIELDS USING: 'QMEL' 'QMNUM' 'X',
                            'QMEL' 'QMART' ' ',
                            'QMEL' 'QMTXT' ' ',
                            'QMEL' 'ERNAM' ' ',
                            'QMEL' 'ERDAT' ' '.

  PERFORM HELP_VALUES_GET.
  P_QMNUM = SELECT_VALUE.

endform.                    " MAKE_NOTIF_LIST

*&---------------------------------------------------------------------*
*&      Form  HELP_VALUES_GET
*&---------------------------------------------------------------------*
FORM HELP_VALUES_GET.
  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
       EXPORTING
            DISPLAY                   = ' '
       IMPORTING
            INDEX                     = SELECT_INDEX
            SELECT_VALUE              = SELECT_VALUE
       TABLES
            FIELDS                    = FIELDS
            SELECT_VALUES             = SELECT_VALUES
            VALUETAB                  = VALUETAB
       EXCEPTIONS
            FIELD_NOT_IN_DDIC         = 1
            MORE_THEN_ONE_SELECTFIELD = 2
            NO_SELECTFIELD            = 3
            OTHERS                    = 4.
ENDFORM.                    " HELP_VALUES_GET

*&---------------------------------------------------------------------*
*&      Form  ADD_FIELDS
*&---------------------------------------------------------------------*
FORM ADD_FIELDS USING  P_TABNAME P_FIELDNAME P_FLAG.
  FIELDS-TABNAME = P_TABNAME.
  FIELDS-FIELDNAME = P_FIELDNAME.
  FIELDS-SELECTFLAG = P_FLAG.
  APPEND FIELDS.      CLEAR FIELDS.
ENDFORM.                    " ADD_FIELDS
*&---------------------------------------------------------------------*
*&      Form  CHECK_TASK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_QMNUM  text
*      -->P_L_TASK  text
*      -->P_P_NO_EXIST  text
*----------------------------------------------------------------------*
form CHECK_TASK using    p_qmnum
                         p_task
                         p_no_exist.
   SELECT SINGLE * FROM QMSM
      WHERE QMNUM    = P_QMNUM AND
            MNCOD    = P_TASK  AND
            KZLOESCH NE 'X'.
   IF SY-SUBRC NE 0.
     P_NO_EXIST = 'X'.
   ELSE.
     L_TASK = QMSM-MANUM.
   ENDIF.


endform.                    " CHECK_TASK
*&---------------------------------------------------------------------*
*&      Form  GET_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form GET_STATUS.
   DATA: L_OBJ    LIKE JEST-OBJNR.
DATA: L_TEXT   LIKE TJ02T-TXT30.
DATA: L_ETJ02T LIKE TJ02T.
DATA: LT_JEST  LIKE JEST OCCURS 0 WITH HEADER LINE.
DATA: L_NUM(10).

CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
   EXPORTING
      INPUT   = ZSQM_NOTI_IMPROV_HEADER-QMNUM
   IMPORTING
      OUTPUT  = L_NUM.

CONCATENATE 'QM00000' L_NUM
      INTO L_OBJ.

SELECT * INTO TABLE LT_JEST
  FROM JEST
  WHERE OBJNR = L_OBJ AND
        INACT NE 'X'.

IF SY-SUBRC NE 0.
  EXIT.
ENDIF.

LOOP AT LT_JEST.
  CLEAR: L_TEXT.
  CLEAR: L_ETJ02T.
  CALL FUNCTION 'AIA_TOOL_READ_STATUS_TEXT'
    EXPORTING
       I_STATUS    = LT_JEST-STAT
    IMPORTING
       E_TEXT      = L_TEXT
       E_TJ02T      = L_ETJ02T
    EXCEPTIONS
       NO_FOUND    = 1.

  CONCATENATE WA_STATUS L_ETJ02T-TXT04
      INTO WA_STATUS SEPARATED BY SPACE.
  CONDENSE WA_STATUS.

ENDLOOP.

endform.                    " GET_STATUS

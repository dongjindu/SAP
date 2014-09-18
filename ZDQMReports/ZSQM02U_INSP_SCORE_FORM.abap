************************************************************************
* Program Name      : ZSQM02U_INSP_SCORE_FORM
* Author            : SeungLyong, Lee
* Creation Date     : 2003.08.14.
* Specifications By : SeungLyong, Lee
* Development Request No :
* Addl Documentation:
* Description       : Inspection Scorecard Form Printing Using Smartform
*
*
* Related Table List:
*   QALS - Inspection lot record
*   QAVE - Inspection processing: Usage decision
*   MARA - Material Master
*   LFA1 - Vendor Master (General Section)
*   USER_ADDR - Users by address data
*   ADRP - Persons (central address administration)
*   DRAD - Document-Object Link
*   PLMK - Inspection plan characteristics
*   QPMK - Inspection characteristic master
*   QASE - Results table for the sample unit
*   QPCT - Code texts
*   QPCD - Inspection catalog codes
*   QAMR - Characteristic results during inspection processing
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************


REPORT  ZSQM02U_INSP_SCORE_FORM       .

*&&& Data Declaration.  &&&*
TYPE-POOLS VRM.     "//Value Request Manager: Types & Constants
TYPE-POOLS CXTAB .  "//Table_control Object type pool
TABLES : FELD.      "//Screen Object Structure

*//Tables;(TABLES : Table_Name/Structures  "//Table Description)
TABLES : ZSQM_INSP_RESULT_01, "/QM-Inspection Scorecard(Inspection lot)
                              " for Forms
         ZSQM_INSP_RESULT_02, "/QM-Inspection Scorecard(Inspection lot)
                              " Char. Result Table
         ZSQM_INSP_RESULT_03, "/QM-Inspection lot: MS Number IT Str.
                              " for Document OBJ Keys
         ZSQM_INSP_RESULT_04. "/Document NO Str for MS Number ITAB
"- by Object - DRAD
TABLES : ZSQM_INSP_QASE,   "/Inspection Result-QASE Data for
                           " Quantitative char Result
         ZSQM_INSP_QPAM_LT. "/Long Text Str for Quali. Spec.
" Collect for Form Interface

TABLES : THEAD. "/SAPscript: Text Header

*//InfoType;()
*//Cluster or Import Parameter;(Parameter Name)

*//Controls(for only Screen Control Element);(TC_ , or TS_)
*-- TABLE CONTROL
*CONTROLS: TC_9100  TYPE TABLEVIEW USING SCREEN 9100.

*//Type (Table Structure);(TY_ )- Table or Structure


*//Constants ;(C_) ==> True:'X' or '1' False:Space or '0'
CONSTANTS : C_MARK   VALUE 'X'.

*//Data(Global Fileds) ;(GL_)  ==> Local Fields ; (LO_)
*                 Flag ;(FL_), Counter;(CT_), Temp;(TMP_)

*DATA :GV_FORMNAME TYPE TDSFNAME VALUE 'ZSQM02_INSP_RESULT'.
DATA :GV_FORMNAME TYPE TDSFNAME VALUE 'ZSQM02_INSP_RESULT_V2'. "02/09/04
*                                             "/Smart Form Name

*-- Function Control
DATA : OK_CODE LIKE SY-UCOMM.

*-- User Confirm for pop-up Message
DATA : GL_ANSWER TYPE C.

*-- Global : used Variable just in this Program
DATA : WA_GENERAL_DATA	LIKE	BAPI2045D_IL0,	       "General data
       WA_TASK_LIST_DATA	LIKE	BAPI2045D_IL1, "Task List Assign
       WA_STOCK_DATA	LIKE	BAPI2045D_IL2,	       "Stock Data
       WA_ACCOUNTING	LIKE	BAPI2045D_IL3,         "Account Assign
       WA_USAGE_DECISION	LIKE	BAPI2045D_IL4, "Usage Decision
       WA_CUSTOMER_DATA	LIKE	BAPI2045CI,            "Cust. CI_QALS
       WA_RETURN	        LIKE	BAPIRETURN1.   "Return Values

*//Data(Work Area or (Internal) Structures);(WA_ )(ST_)?



*//Internal Tables and Index Fields;(IT_), (I_)
DATA : IT_SYS_STATUS  LIKE BAPI2045SS OCCURS 5 WITH HEADER LINE,
                                           "Inspection Lot System Status
       IT_USER_STATUS LIKE BAPI2045US OCCURS 5 WITH HEADER LINE,
                                     "User Status for the Inspection Lot
       IT_MAT_DOC     LIKE BAPI2045MD OCCURS 2 WITH HEADER LINE.
"References Between Inspection Lots and Material Documents

DATA : IT_PLANMK LIKE PLMKB OCCURS 10 WITH HEADER LINE.
"Characteristic document table

*- Internal Table : MS Number  Document OBJ Keys
DATA : IT_MS_DOC_OBJ LIKE ZSQM_INSP_RESULT_03 OCCURS 10
                                                   WITH HEADER LINE.
*- MS Document No Table
DATA : IT_MS_DOC     LIKE ZSQM_INSP_RESULT_04 OCCURS 10
                                                   WITH HEADER LINE.

*-- Inspection Result Table
DATA : IT_INSP_R  LIKE ZSQM_INSP_RESULT_02 OCCURS 10
                                                 WITH HEADER LINE.
*-- Inspection Result- QASE-MESSWERT.
DATA : IT_ZSQM_INSP_QASE LIKE ZSQM_INSP_QASE OCCURS 10 WITH HEADER LINE.

*-- Long Text internal Table for Inspector Opinion
DATA : IT_INSP_OPINION  LIKE TLINE OCCURS 5 WITH HEADER LINE.

*-- Long Text Collected Using key(AUSWMENGE)
DATA : IT_ZSQM_INSP_QPAM_LT LIKE ZSQM_INSP_QPAM_LT OCCURS 0
                                                  WITH HEADER LINE.

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
PARAMETERS : P_PRUEFL  TYPE QPLOS.     "/Inspection Lot Number

SELECTION-SCREEN END OF BLOCK BLK .
*-- Seclection Screen Flow Logic Control Event Handling
*AT SELECTION-SCREEN ON ( ON END OF, ON VALUE-REQUEST FOR,
* ON HELP-REQUEST FOR, ON RADIOBUTTON GROUP, ON BLOCK OUTPUT,
* ON EXIT-COMMAND )

AT SELECTION-SCREEN OUTPUT.
  SET TITLEBAR 'INSP'.

*-- Selection for Selection Screen
START-OF-SELECTION.

END-OF-SELECTION.
  CLEAR ZSQM_INSP_RESULT_01.

*-- Get Data for Inspection Scorecard  Header Data.
  PERFORM GET_DATA_FROM_DB  USING ZSQM_INSP_RESULT_01
                                  P_PRUEFL .     "/Inspec Lot No.

*-- Get Text for Inspection Scorecard Header Data.
  PERFORM GET_ETC_N_TEXT_DATA  USING ZSQM_INSP_RESULT_01
                                  P_PRUEFL.

**-- Get Long Text for Inspector Opinion "/02/09/2004 - Remarked
*  IF ZSQM_INSP_RESULT_01-LTEXTKZ  = C_MARK.
*    CLEAR THEAD.
*    MOVE : 'QPRUEFLOS' TO THEAD-TDOBJECT,
*           'QAVE'      TO THEAD-TDID.
*    CONCATENATE SY-MANDT
*                ZSQM_INSP_RESULT_01-PRUEFLOS
*                'L'
*                       INTO THEAD-TDNAME.
*
*    PERFORM READ_LONGTEXT  TABLES  IT_INSP_OPINION
*                           USING   THEAD   .
*
*  ENDIF.

*-- Get Document Data Using Function Module 'CQ_DB_PLMK_READ'.
  PERFORM GET_MS_NUMBER.

*-- Get Inspection Result for Quantitative and Qualitative Insp.
  PERFORM GET_INSP_RESULT  TABLES IT_PLANMK
                                  IT_INSP_R
                           USING ZSQM_INSP_RESULT_01.

*-- Get Text for Result Code or Characteristic.
  PERFORM GET_TEXT_FOR_RESULT TABLES IT_INSP_R.

*-- Get Results  Data for Quantitative using Inspection lot No.
*--  from QASE.
  PERFORM GET_RESULT_OF_QASE_FOR_QUANTIT  TABLES IT_ZSQM_INSP_QASE
                                          USING  P_PRUEFL.

*-- Print out Form
  PERFORM PRINT_FORMS.

*// Event Handling(Except Selection Screen (Flow)event)
*Load of Program.

*Initialization.

***//Macro Definitions
*DEFINE RANGE_MACRO.
*END-OF-DEFINITION.

**<<<<<<<<< Program Main / Subroutine / Flow Logic >>>>>>>>>>>>**
*&------------------------------------------------------------------*
*&      Form  GET_DATA_FROM_DB
*&------------------------------------------------------------------*
FORM GET_DATA_FROM_DB USING   P_INSP  STRUCTURE ZSQM_INSP_RESULT_01
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


*-- Mapping fields of BAPI Interface Parameters and P_INSP

  MOVE : WA_GENERAL_DATA-INSPLOT          TO P_INSP-PRUEFLOS,
         WA_GENERAL_DATA-PLANT            TO P_INSP-WERKS,
         WA_GENERAL_DATA-MATERIAL         TO P_INSP-MATNR,
         WA_GENERAL_DATA-VENDOR           TO P_INSP-LIFNR,
         WA_GENERAL_DATA-CREATED_BY_USER  TO P_INSP-ERSTELLER,
         WA_GENERAL_DATA-SHORT_TEXT       TO P_INSP-KTEXTLOS,


         WA_TASK_LIST_DATA-TASK_LIST_TYPE    TO P_INSP-PLNTY,
         WA_TASK_LIST_DATA-TASK_LIST_NUMBER  TO P_INSP-PLNNR,


         WA_USAGE_DECISION-UD_RECORDED_ON_DATE   TO P_INSP-VDATUM,
         WA_USAGE_DECISION-UD_CATALOG_TYPE       TO P_INSP-VKATART,
         WA_USAGE_DECISION-UD_CODE_GROUP         TO P_INSP-VCODEGRP,
         WA_USAGE_DECISION-UD_CODE               TO P_INSP-VCODE,
          WA_USAGE_DECISION-IND_UD_LONG_TEXT     TO P_INSP-LTEXTKZ,

*         WA_CUSTOMER_DATA-VEHICLE        TO P_INSP-VEHICLE,
         WA_CUSTOMER_DATA-KATART_VH      TO P_INSP-KATART_VH,
         WA_CUSTOMER_DATA-CODEGRP_VH     TO P_INSP-CODEGRP_VH,
         WA_CUSTOMER_DATA-CODE_VH        TO P_INSP-CODE_VH,

         WA_CUSTOMER_DATA-KATALOGART     TO P_INSP-KATALOGART,
         WA_CUSTOMER_DATA-CODEGRUPPE     TO P_INSP-CODEGRUPPE,
         WA_CUSTOMER_DATA-CODE           TO P_INSP-CODE.

ENDFORM.                    " GET_DATA_FROM_DB
*&-----------------------------------------------------------------*
*&      Form  GET_ETC_N_TEXT_DATA
*&-----------------------------------------------------------------*
FORM GET_ETC_N_TEXT_DATA USING  P_INSP  STRUCTURE ZSQM_INSP_RESULT_01
                          VALUE(P_PRUEFLOS) TYPE QPLOS.

*-- Material Text.
  PERFORM GET_MATERIAL_TEXT  USING P_INSP-MATNR
                                   P_INSP-MAKTX.

*-- Get Ext.material Group and Text
  PERFORM GET_EXT_MAT_GROUP   USING   P_INSP-MATNR
                             CHANGING P_INSP-EXTWG
                                      P_INSP-EWBEZ.

*-- Supplier Text
  PERFORM GET_VENDOR_NAME    USING P_INSP-LIFNR
                                   P_INSP-NAME1.

*-- Inspection lot Creater(Requester) Name
  PERFORM GET_INSPECTION_R_I_NAME  USING P_INSP-ERSTELLER
                                         P_INSP-NAME_TEXT.


*-- Usage Decision Code Text
  PERFORM GET_CODE_TEXTS   USING    P_INSP-VKATART
                                    P_INSP-VCODEGRP
                                    P_INSP-VCODE
                                    ''
                           CHANGING P_INSP-KURZTEXT_V.

*-- Inspection Purpose Text
  PERFORM GET_CODE_TEXTS   USING    P_INSP-KATALOGART
                                    P_INSP-CODEGRUPPE
                                    P_INSP-CODE
                                    ''
                           CHANGING P_INSP-KURZTEXT_C.

**-- Vehicle Text
*  PERFORM GET_MATERIAL_TEXT  USING P_INSP-VEHICLE
*                                   P_INSP-VEHICLE_N.

*-- Vehicle/Engine type Text
  PERFORM GET_CODE_TEXTS   USING    P_INSP-KATART_VH
                                    P_INSP-CODEGRP_VH
                                    P_INSP-CODE_VH
                                    ''
                           CHANGING P_INSP-KURZTEXT_VH.


ENDFORM.                    " GET_ETC_N_TEXT_DATA
*&------------------------------------------------------------------*
*&      Form  PRINT_FORMS
*&------------------------------------------------------------------*
FORM PRINT_FORMS.
  DATA :LV_FUNCNAME TYPE RS38L_FNAM .
  CLEAR :LV_FUNCNAME.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
       EXPORTING
            FORMNAME           = GV_FORMNAME
*            VARIANT            = ' '
*            DIRECT_CALL        = ' '
       IMPORTING
            FM_NAME            = LV_FUNCNAME
       EXCEPTIONS
            NO_FORM            = 1
            NO_FUNCTION_MODULE = 2
            OTHERS             = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CHECK SY-SUBRC = 0.

** "/Dev-'/1BCDWB/SF00000024'
  CALL FUNCTION LV_FUNCNAME
       EXPORTING
            I_INSP           = ZSQM_INSP_RESULT_01
       TABLES
            T_INSP_QASE      = IT_ZSQM_INSP_QASE
            T_INSP_RESULT    = IT_INSP_R
            T_MS_DOC_OBJK    = IT_MS_DOC_OBJ
            T_MS_DOC_DOKNR   = IT_MS_DOC
            T_INSP_OPN       = IT_INSP_OPINION
            T_SPEC_LTEXT     = IT_ZSQM_INSP_QPAM_LT
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
*&-----------------------------------------------------------------*
*&      Form  GET_MATERIAL_TEXT
*&-----------------------------------------------------------------*
FORM GET_MATERIAL_TEXT USING   VALUE(P_MATNR)
                                     P_MAKTX.

*  CALL FUNCTION 'HAZMAT_MATNR_GET_TEXT'
*       EXPORTING
*            I_MATNR         = P_MATNR
*            I_LANGU         = SY-LANGU
*       IMPORTING
*            E_MAKTX         = P_MAKTX
*       EXCEPTIONS
*            NO_RECORD_FOUND = 1
*            OTHERS          = 2.

  SELECT SINGLE MAKTX INTO P_MAKTX
     FROM MAKT
       WHERE SPRAS = SY-LANGU
         AND MATNR = P_MATNR.

  IF SY-SUBRC <> 0.
    CLEAR P_MAKTX.
  ENDIF.

ENDFORM.                    " GET_MATERIAL_TEXT
*&------------------------------------------------------------------*
*&      Form  GET_MS_NUMBER
*&------------------------------------------------------------------*
FORM GET_MS_NUMBER.

  CALL FUNCTION 'CQ_DB_PLMK_READ'
       EXPORTING
*            DATUB      = 0
*            DATUV      = 0
*            PLNAL      = ' '
            PLNNR      = ZSQM_INSP_RESULT_01-PLNNR
            PLNTY      = ZSQM_INSP_RESULT_01-PLNTY
*            SERNB      = 0
*            SERNRKZ    = ' '
*            SERNV      = 0
*     IMPORTING
*           FLG_NOT_FOUND        =
*           ZAEHL_MAX_PLMK       =
       TABLES
            PLANMK     = IT_PLANMK
       EXCEPTIONS
            NO_RECORDS = 1
            OTHERS     = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*--- <<< Begin of Remark - sllee
**- Delete records, Its LOEKZ(Deletion Flag) is 'X' or Inspection Method
**-  (PMETHODE) is Blank.
*  DELETE IT_PLANMK WHERE LOEKZ = C_MARK
*                      OR PMETHODE IS INITIAL.
*--- <<< End of Remark - sllee


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
*&-----------------------------------------------------------------*
*&      Form  Get_Insp_Result
*&-----------------------------------------------------------------*
FORM GET_INSP_RESULT   TABLES PT_PLANMK STRUCTURE PLMKB
                              PT_RESULT STRUCTURE ZSQM_INSP_RESULT_02
                       USING  P_INSP    STRUCTURE ZSQM_INSP_RESULT_01.
  DATA : LW_RESULT_INDEX LIKE SY-TABIX..

  REFRESH PT_RESULT.

*-- Get Inspection Result Data from View ZVQM_INSP_RESULT

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE PT_RESULT
      FROM ZVQM_INSP_RESULT
        WHERE PRUEFLOS = P_INSP-PRUEFLOS.

  IF SY-SUBRC NE 0.
    MESSAGE E000(ZMQM)
               WITH 'There is no Inspection Result Data of '(E01)
                     P_INSP-PRUEFLOS.

  ENDIF.

*-- Fill Quantitative Characteristic Flag Field of Result table
*--  Using PT_PLANMK-STEUERKZ+0(1)
  LOOP AT PT_PLANMK.
    CLEAR PT_RESULT.
    READ TABLE PT_RESULT WITH KEY PLNTY  = PT_PLANMK-PLNTY
                                  PLNNR  = PT_PLANMK-PLNNR
                                  PLNKN  = PT_PLANMK-PLNKN
                                  MERKNR = PT_PLANMK-MERKNR
                                  ZAEHL  = PT_PLANMK-ZAEHL.
    IF SY-SUBRC = 0.
      LW_RESULT_INDEX = SY-TABIX.
      MOVE : PT_PLANMK-STEUERKZ+0(1) TO PT_RESULT-QUANTITAT,
             PT_PLANMK-KURZTEXT      TO PT_RESULT-KURZTEXT.

      MOVE : PT_PLANMK-STELLEN      TO PT_RESULT-STELLEN,
             PT_PLANMK-MASSEINHSW   TO PT_RESULT-MASSEINHSW,
             PT_PLANMK-TOLERANZOB   TO PT_RESULT-TOLERANZOB,
             PT_PLANMK-TOLERANZUN   TO PT_RESULT-TOLERANZUN.

      IF PT_RESULT-QUANTITAT IS INITIAL.
        MOVE : PT_PLANMK-KATAB1        TO PT_RESULT-KATAB,
               PT_PLANMK-KATALGART1    TO PT_RESULT-KATALGART,
               PT_PLANMK-AUSWMENGE1    TO PT_RESULT-AUSWMENGE,
               PT_PLANMK-AUSWMGWRK1    TO PT_RESULT-AUSWMGWRK.
      ENDIF.

      MODIFY PT_RESULT INDEX LW_RESULT_INDEX .
    ENDIF.

  ENDLOOP.


ENDFORM.                    " Get_Insp_Result
*&------------------------------------------------------------------*
*&      Form  GET_VENDOR_NAME
*&------------------------------------------------------------------*
FORM GET_VENDOR_NAME USING    P_LIFNR
                              P_NAME.

  SELECT SINGLE NAME1 INTO P_NAME
    FROM LFA1
      WHERE LIFNR = P_LIFNR.
  IF SY-SUBRC NE 0.
    CLEAR P_NAME.
  ENDIF.

ENDFORM.                    " GET_VENDOR_NAME
*&----------------------------------------------------------------*
*&      Form  GET_Inspection_R_I_NAME
*&----------------------------------------------------------------*
FORM GET_INSPECTION_R_I_NAME USING    P_BNAME
                                      P_NAME_TEXT.

  SELECT SINGLE NAME_TEXT
       INTO P_NAME_TEXT
         FROM USR21 AS A INNER JOIN ADRP AS B
           ON  A~PERSNUMBER = B~PERSNUMBER
          WHERE A~BNAME = P_BNAME.

  CHECK SY-SUBRC NE 0.
  CLEAR P_NAME_TEXT.


ENDFORM.                    " GET_Inspection_R_I_NAME
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
*&-----------------------------------------------------------------*
*&      Form  GET_TEXT_FOR_RESULT
*&-----------------------------------------------------------------*
FORM GET_TEXT_FOR_RESULT TABLES   PT_RESULT STRUCTURE IT_INSP_R.
  DATA : LWA_PR_RESULT_INDEX LIKE SY-TABIX.
  DATA : LW_AUSWMENGE(8).

  LOOP AT PT_RESULT.
    LWA_PR_RESULT_INDEX  = SY-TABIX.
*   -- Specification Short Text for Qualit.
    IF PT_RESULT-QUANTITAT  IS INITIAL.
      PERFORM GET_CATALOG_TEXT   USING    PT_RESULT-KATALGART
                                          PT_RESULT-AUSWMENGE
                                          PT_RESULT-AUSWMGWRK
                                 CHANGING PT_RESULT-KTX01
                                          PT_RESULT-LTEXTV.

      IF PT_RESULT-LTEXTV = C_MARK.   "/ When There is Long Text.

*-- Check which there is exist Qualititative Inspection Characteristic
*-- Specification LongText
        READ TABLE  IT_ZSQM_INSP_QPAM_LT
                  WITH KEY WERKS      = ZSQM_INSP_RESULT_01-WERKS
                           KATALOGART = PT_RESULT-KATALGART
                           AUSWMENGE  = PT_RESULT-AUSWMENGE.
        IF SY-SUBRC NE 0.
*--AUSWMENGE field data will be moved to LW_AUSWMENGE,which is filled
*--with '#', and AUSWMENGE will be concatenated Using LW_AUSWMENGE.
*--Then  Character '#' will be convert to SPACE.
          LW_AUSWMENGE = '########'.
          CONCATENATE PT_RESULT-AUSWMENGE
                      LW_AUSWMENGE  INTO LW_AUSWMENGE.

          CLEAR THEAD.
          MOVE : 'QKATALOG' TO THEAD-TDOBJECT,
                 'QPAM'     TO THEAD-TDID.
          CONCATENATE SY-MANDT
                      ZSQM_INSP_RESULT_01-WERKS
                      PT_RESULT-KATALGART
                      LW_AUSWMENGE
                      'E'
                INTO THEAD-TDNAME.

*-     Replace '#' to SPACE for THEAD-TDNAME.
          PERFORM REPLACE_OF_SYMBOL_TO_SPACE USING THEAD-TDNAME
                                                   '#'.

          PERFORM COLLECT_LONG_TEXT  TABLES IT_ZSQM_INSP_QPAM_LT
                                     USING  THEAD
                                            ZSQM_INSP_RESULT_01-WERKS
                                            PT_RESULT-KATALGART
                                            PT_RESULT-AUSWMENGE.

        ENDIF.

      ENDIF.

    ENDIF.

*   -- Results :QAMR-CODE  Text
    IF NOT PT_RESULT-CODE_Q IS INITIAL.
      PERFORM GET_CODE_TEXTS   USING    PT_RESULT-KATALGART_Q
                                        PT_RESULT-GRUPPE_Q
                                        PT_RESULT-CODE_Q
                                        PT_RESULT-VERSION_Q
                               CHANGING PT_RESULT-KURZTEXT_Q.

    ENDIF.

*   -- Valuation Domain Fixed Value Text
    IF NOT PT_RESULT-MBEWERTG IS INITIAL.
      PERFORM GET_VALUATION_TEXT   USING    PT_RESULT-MBEWERTG
                                   CHANGING PT_RESULT-DTEXT_MBEWERTG.

    ENDIF.

*-- Inspection lot Inspector Name
    PERFORM GET_INSPECTION_R_I_NAME  USING PT_RESULT-PRUEFER
                                           PT_RESULT-NAME_TEXT_Q.


    MODIFY PT_RESULT INDEX LWA_PR_RESULT_INDEX.

  ENDLOOP.

ENDFORM.                    " GET_TEXT_FOR_RESULT
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
*&------------------------------------------------------------------*
*&      Form  GET_RESULT_OF_QASE_FOR_QUANTIT
*&------------------------------------------------------------------*
FORM GET_RESULT_OF_QASE_FOR_QUANTIT
                TABLES   PT_ZSQM_INSP_QASE STRUCTURE ZSQM_INSP_QASE
                USING    P_PRUEFLOS.

  REFRESH PT_ZSQM_INSP_QASE.

  SELECT PRUEFLOS VORGLFNR MERKNR DETAILERG MESSWERT
     INTO CORRESPONDING FIELDS OF TABLE PT_ZSQM_INSP_QASE
       FROM QASE
         WHERE PRUEFLOS = P_PRUEFLOS.

ENDFORM.                    " GET_RESULT_OF_QASE_FOR_QUANTIT
*&------------------------------------------------------------------*
*&      Form  READ_LONGTEXT
*&------------------------------------------------------------------*
FORM READ_LONGTEXT TABLES   PT_TXTABLE STRUCTURE TLINE
                   USING    P_THEAD    STRUCTURE THEAD.

  CALL FUNCTION 'READ_TEXT'
       EXPORTING
            CLIENT                  = SY-MANDT
            ID                      = P_THEAD-TDID
            LANGUAGE                = SY-LANGU
            NAME                    = P_THEAD-TDNAME
            OBJECT                  = P_THEAD-TDOBJECT
            ARCHIVE_HANDLE          = 0
            LOCAL_CAT               = ' '
*       IMPORTING
*            HEADER                  =
       TABLES
            LINES                   = PT_TXTABLE
       EXCEPTIONS
            ID                      = 1
            LANGUAGE                = 2
            NAME                    = 3
            NOT_FOUND               = 4
            OBJECT                  = 5
            REFERENCE_CHECK         = 6
            WRONG_ACCESS_TO_ARCHIVE = 7
            OTHERS                  = 8.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " READ_LONGTEXT
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
*&      Form  Collect_loNG_TEXT
*&---------------------------------------------------------------------*
FORM COLLECT_LONG_TEXT TABLES   PT_QPAM_LT STRUCTURE ZSQM_INSP_QPAM_LT
                       USING    P_THEAD
                                P_WERKS
                                P_KATALGART
                                P_AUSWMENGE.

  DATA : LT_TLINE LIKE TLINE OCCURS 5 WITH HEADER LINE.
  REFRESH LT_TLINE.

  PERFORM READ_LONGTEXT  TABLES  LT_TLINE
                         USING   P_THEAD .

  CHECK NOT LT_TLINE[] IS INITIAL.

  LOOP AT LT_TLINE.   "//Collect Long Text Data.
    CLEAR PT_QPAM_LT.
    MOVE : P_AUSWMENGE TO PT_QPAM_LT-AUSWMENGE,   "/Long Text Key Values
           P_WERKS     TO PT_QPAM_LT-WERKS,
           P_KATALGART TO PT_QPAM_LT-KATALOGART.
    MOVE : LT_TLINE-TDFORMAT  TO PT_QPAM_LT-TDFORMAT,
           LT_TLINE-TDLINE    TO PT_QPAM_LT-TDLINE.
    APPEND PT_QPAM_LT.
  ENDLOOP.

ENDFORM.                    " Collect_loNG_TEXT
*&------------------------------------------------------------------*
*&      Form  GET_EXT_MAT_GROUP
*&------------------------------------------------------------------*
FORM GET_EXT_MAT_GROUP USING    P_MATNR  TYPE MATNR
                       CHANGING P_EXTWG  TYPE EXTWG
                                P_EWBEZ  TYPE EWBEZ.

  CLEAR : P_EXTWG, P_EWBEZ.
  CHECK NOT P_MATNR IS INITIAL.

  SELECT SINGLE A~EXTWG B~EWBEZ
     INTO (P_EXTWG, P_EWBEZ)
       FROM MARA AS A INNER JOIN TWEWT AS B
          ON A~EXTWG = B~EXTWG
         WHERE A~MATNR = P_MATNR
           AND B~SPRAS = SY-LANGU.


ENDFORM.                    " GET_EXT_MAT_GROUP

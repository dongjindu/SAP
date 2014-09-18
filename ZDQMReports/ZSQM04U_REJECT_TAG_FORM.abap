************************************************************************
* Program Name      : ZSQM04U_REJECT_TAG_FORM
* Author            : SeungLyong, Lee
* Creation Date     : 2003.09.02.
* Specifications By : SeungLyong, Lee
* Pattern           : 1.1.3 General
* Development Request No : UD1K901760
* Addl Documentation:
* Description       : Reject Tag for Notification Using Samrt
*                     Forms
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT  ZSQM04U_REJECT_TAG_FORM     .

*&&& Data Declaration.  &&&*
TYPE-POOLS VRM.     "//Value Request Manager: Types & Constants
*TYPE-POOLS CXTAB .  "//Table_control Object type pool
TABLES : FELD.      "//Screen Object Structure

*-- SAP Scripts Object Interface
TABLES : THEAD. "/SAPscript: Text Header

*//Tables;(TABLES : Table_Name  "//Table Description)
TABLES : QMEL.   "/Quality Notification
TABLES : QALS.   "/Inspection Lot

*//Structures Declaration(TABLES : Structure Name."/Description)
TABLES : ZSQM_REJECT_TAG.   "/Reject Tag Data Structure
TABLES : ZSQM_REJECT_TAG_DEFT. "/Reject Tag Data Structure- Item List
TABLES : ZSQM_MIC.           "/MIC data/result data of Inspection Lot

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

DATA : WA_FORMNAME TYPE TDSFNAME  VALUE 'ZSQM04_REJECT_TAG'.
*                                             "/Smart Form Name

*-- User Confirm for pop-up Message
DATA : WA_ANSWER TYPE C.

*//Data(Work Area or (Internal) Structures);(WA_ )(ST_)?



*//Internal Tables and Index Fields;(IT_), (I_)
DATA : IT_DEFECT LIKE ZSQM_REJECT_TAG_DEFT  "/Defect type
                    OCCURS 0  WITH HEADER LINE.

*-- Internal Tables for Long Text of Notification
DATA : IT_QMEL_LTQM  LIKE TLINE OCCURS 0.  "/Description Longtext


*-- Internal table for MIC and Inspector
DATA : IT_ZSQM_MIC  LIKE ZSQM_MIC OCCURS 3 WITH HEADER LINE.

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
*PARAMETERS : P_QMNUM  TYPE QMNUM OBLIGATORY  "/Notification Number
*                               MATCHCODE OBJECT ZQM_DEFECT_H.
PARAMETERS : P_QPLOS  TYPE QPLOS OBLIGATORY.  "/Inspection Lot Number
*                               MATCHCODE OBJECT ZQM_REJT_H.
SELECTION-SCREEN END OF BLOCK BLK .

*-- Seclection Screen Flow Logic Control Event Handling
*AT SELECTION-SCREEN ON ( ON END OF, ON VALUE-REQUEST FOR,
* ON HELP-REQUEST FOR, ON RADIOBUTTON GROUP, ON BLOCK OUTPUT,
* ON EXIT-COMMAND )
AT SELECTION-SCREEN OUTPUT.
  SET TITLEBAR 'DEFECT'.

*AT SELECTION-SCREEN ON P_QMNUM.
*  CHECK NOT P_QMNUM IS INITIAL.
*  SELECT SINGLE *
*     FROM QMEL
*       WHERE QMNUM = P_QMNUM.
*  CHECK SY-SUBRC NE 0.
*  MESSAGE E000(ZMQM)
*              WITH 'Not Found Notification Number. '(E01) P_QMNUM.

AT SELECTION-SCREEN ON P_QPLOS.
  CHECK NOT P_QPLOS IS INITIAL.
  SELECT SINGLE *
     FROM QALS
       WHERE PRUEFLOS = P_QPLOS.
  CHECK SY-SUBRC NE 0.
  MESSAGE E000(ZMQM)
              WITH 'Not Found Inspection Lot Number. '(E02) P_QPLOS.


*-- Selection for Selection Screen
START-OF-SELECTION.

END-OF-SELECTION.

*-- Get Header Data Using Notification Which was entered.
  PERFORM GET_HEADER_DATA_FROM_QMNUM  USING ZSQM_REJECT_TAG
*                                            P_QMNUM.
                                            P_QPLOS.

*-- Get other data which are not in QMEL
  PERFORM GET_ETC_DATA_FOR_HEADER   USING ZSQM_REJECT_TAG.


**-- Get Defect Type List  -> IT_DEFECT
*  PERFORM GET_DEFECT_TYPE_LIST  TABLES IT_DEFECT
**                                USING  P_QMNUM.
*                                USING  P_QPLOS.

*-- Get MIC result data of inspection lot
  PERFORM GET_RESULT_DATA_MIC_ISPECTOR TABLES IT_ZSQM_MIC
                                       USING  P_QPLOS.

*-- /Get Long Text for Notification
*  - Content of Request/Comment
*  PERFORM GET_LONGTEXT    TABLES IT_QMEL_LTQM  "/Text Table
*                          USING 'QMEL'         "/Text Object
*                                'LTQM'         "/Text ID
*                                 P_QMNUM.      "/Text ID Name

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
             USING  P_REJECT_HDR STRUCTURE ZSQM_REJECT_TAG
*                    P_QMNUM      TYPE QMNUM.
                    P_QPLOS      TYPE QPLOS.

*-// Change SQL heaer data : request by dokim. - Start.
*  SELECT SINGLE   A~QMNUM A~MATNR A~PRUEFLOS            "A~VEHICLE
**                  A~KATART_VH A~CODEGRP_VH A~CODE_VH
*                  C~KATART_VH C~CODEGRP_VH C~CODE_VH
*                  A~OBJNR A~RKMNG A~MGEIN
*                  B~AUSVN
*     INTO CORRESPONDING FIELDS OF P_REJECT_HDR
*       FROM ( QMEL AS A INNER JOIN QMIH AS B
*         ON A~QMNUM    = B~QMNUM ) INNER JOIN QALS AS C
*         ON A~PRUEFLOS = C~PRUEFLOS
**         WHERE A~QMNUM = P_QMNUM.
*          WHERE A~PRUEFLOS = P_QPLOS.


  SELECT SINGLE A~KATART_VH A~CODEGRP_VH A~CODE_VH
                A~PRUEFLOS A~MATNR A~LIFNR A~LOSMENGE AS RKMNG
                A~MENGENEINH AS MGEIN
      INTO CORRESPONDING FIELDS OF P_REJECT_HDR
        FROM QALS AS A
          WHERE A~PRUEFLOS = P_QPLOS.

*-// Change SQL heaer data : request by dokim. - End
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

*** "/Dev-'/1BCDWB/SF00000030'
  CALL FUNCTION LW_FUNCNAME
       EXPORTING
            I_REJECT         = ZSQM_REJECT_TAG
       TABLES
            T_TLINE          = IT_QMEL_LTQM
            T_DEFECT         = IT_DEFECT
            T_MIC            = IT_ZSQM_MIC
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
                USING  P_REJECT_HDR STRUCTURE ZSQM_REJECT_TAG.

*-- Get Part(Material) Name
  PERFORM GET_MAT_VEHICLE_D   USING P_REJECT_HDR-MATNR
                           CHANGING P_REJECT_HDR-MAKTX.

*-- Get Vehicle Name
*  PERFORM GET_MAT_VEHICLE_D   USING P_REJECT_HDR-VEHICLE
*                           CHANGING P_REJECT_HDR-VEHICLE_N.
  PERFORM GET_VEHICLE_NAME_AND_CHECK USING  P_REJECT_HDR-KATART_VH
                                            P_REJECT_HDR-CODEGRP_VH
                                            P_REJECT_HDR-CODE_VH
                                 CHANGING   P_REJECT_HDR-KURZTEXT_VH.

*-- Get Supplier(Vendor) Data
*  PERFORM GET_SUPPLIER_DATA USING P_REJECT_HDR-OBJNR    "/Noti Object
*                                P_REJECT_HDR-PARNR_VND "/SupplierCode
*                                  P_REJECT_HDR-NAME1  "/Supplier Name
*                                 'Z5'.       "'LF' "/Partner Function
   PERFORM GET_VENDOR_DATA  USING P_REJECT_HDR-LIFNR
                         CHANGING P_REJECT_HDR-NAME1.

ENDFORM.                    " GET_ETC_DATA_FOR_HEADER
*&-----------------------------------------------------------------*
*&      Form  GET_MAT_VEHICLE_D
*&-----------------------------------------------------------------*
FORM GET_MAT_VEHICLE_D  USING   VALUE(P_MATNR)
                                      P_MAKTX.
  CHECK NOT P_MATNR IS INITIAL.

  CALL FUNCTION 'HAZMAT_MATNR_GET_TEXT'
       EXPORTING
            I_MATNR         = P_MATNR
            I_LANGU         = SY-LANGU
       IMPORTING
            E_MAKTX         = P_MAKTX
       EXCEPTIONS
            NO_RECORD_FOUND = 1
            OTHERS          = 2.

  IF SY-SUBRC <> 0.
    CLEAR P_MAKTX.
  ENDIF.

ENDFORM.                    " GET_MAT_VEHICLE_D
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
*&      Form  GET_DEFECT_TYPE_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DEFECT  text
*      -->P_P_QMNUM  text
*----------------------------------------------------------------------*
FORM GET_DEFECT_TYPE_LIST
                   TABLES   PT_DEFECT STRUCTURE ZSQM_REJECT_TAG_DEFT
*                   USING    P_QMNUM  TYPE QMNUM.
                   USING    P_QPLOS  TYPE QPLOS.

  REFRESH PT_DEFECT.

  SELECT B~FENUM B~FEKAT
         B~FEGRP B~FECOD B~FEVER
         C~KURZTEXT AS KURZTEXT
        INTO CORRESPONDING FIELDS OF TABLE PT_DEFECT
           FROM ( QMEL AS A INNER JOIN QMFE AS B
             ON A~QMNUM = B~QMNUM ) INNER JOIN QPCT AS C
             ON  B~FEKAT = C~KATALOGART
             AND B~FEGRP = C~CODEGRUPPE
             AND B~FECOD = C~CODE
             AND B~FEVER = C~VERSION
          WHERE "B~QMNUM = P_QMNUM
                A~PRUEFLOS = P_QPLOS
            AND C~SPRACHE = SY-LANGU.
*            AND C~VERSION = '000001'.

ENDFORM.                    " GET_DEFECT_TYPE_LIST
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
                           P_QMNUM            TYPE QMNUM.
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

  CHECK NOT P_KATART_VH  IS INITIAL     AND
        NOT P_CODEGRP_VH IS INITIAL     AND
        NOT P_CODE_VH    IS INITIAL.

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
*&      Form  GET_RESULT_DATA_MIC_ISPECTOR
*&------------------------------------------------------------------*
FORM GET_RESULT_DATA_MIC_ISPECTOR TABLES PT_ZSQM_MIC STRUCTURE ZSQM_MIC
                                  USING  P_QPLOS     TYPE QPLOS.

  REFRESH PT_ZSQM_MIC.

  SELECT A~PRUEFLOS A~MERKNR A~SATZSTATUS
         A~DBEWERTG A~MBEWERTG B~VERWMERKM
         B~KURZTEXT A~PRUEFER C~NAME_TEXTC
       INTO CORRESPONDING FIELDS OF TABLE PT_ZSQM_MIC
         FROM ( QAMR AS A INNER JOIN QAMV AS B
           ON   A~PRUEFLOS  = B~PRUEFLOS
            AND A~VORGLFNR  = B~VORGLFNR
            AND A~MERKNR    = B~MERKNR ) INNER JOIN USER_ADDR AS C
           ON   A~PRUEFER   = C~BNAME
              WHERE  A~PRUEFLOS = P_QPLOS
                AND  A~MBEWERTG = 'R'.    "/Rejected ('Not OK')


ENDFORM.                    " GET_RESULT_DATA_MIC_ISPECTOR
*&------------------------------------------------------------------*
*&      Form  GET_VENDOR_DATA
*&------------------------------------------------------------------*
FORM GET_VENDOR_DATA USING    P_LIFNR
                     CHANGING P_NAME.

  CLEAR P_NAME.

  CHECK NOT P_LIFNR IS INITIAL.

  SELECT SINGLE NAME1  INTO P_NAME
    FROM LFA1
      WHERE LIFNR = P_LIFNR.

ENDFORM.                    " GET_VENDOR_DATA

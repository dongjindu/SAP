************************************************************************
* Program Name      : ZAQM12A_REG_MAT_ASSIGN
* Author            : SeungLyong, Lee
* Creation Date     : 2004.03.04.
* Specifications By : SeungLyong, Lee
* Pattern           : 1.2.4 Call Screen + 3.1 General
* Development Request No : UD1K908042
* Addl Documentation:
* Description       : Regular Inspection : Assign part to EXTWG and
*                                          Input data
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT  ZAQM12A_REG_MAT_ASSIGN  NO STANDARD PAGE HEADING
                                   LINE-SIZE 109 .

*&&& Data Declaration.  &&&*
TYPE-POOLS : VRM.     "//Value Request Manager: Types & Constants

TYPE-POOLS ZQMT1 . "/QM-Type group for inspection

TYPE-POOLS SLIS.  "/Globale Typen f? generische Listbausteine

TYPE-POOLS CXTAB .  "//Table_control Object type pool
TABLES : FELD.      "//Screen Object Structure

*-- Include Program ( Include Constants or etc)
INCLUDE <ICON>.

*-- SAP Scripts Object Interface
*TABLES : THEAD. "/SAPscript: Text Header

*//Tables;(TABLES : Table_Name /View "//Table Description)
TABLES : ZTQM_MAT_REG. "/Regular Insp. Material Master

*//Structures Declaration(TABLES : Structure Name."/Description)
TABLES : ZSQM_MAT_SEL, "/Material Selection Str for Scheduling
         ZSQM_MAT_EXT. "/Assing EXTWG to Material Str.

TABLES : ZSQM_MAT_CNF. "/Assign Resp. person and Input confirm data

*/ structure for Excel download : Inspection scheduling.
TABLES : ZSQM_QNS_EX_REG.  "/Insp. Scheduling (Regular): Excel Layout

*//InfoType;()
*//Cluster or Import Parameter;(Parameter Name)

*//Controls(for only Screen Control Element);(TC_ , or TS_)
*-- TABLE CONTROL
CONTROLS: TC_9100  TYPE TABLEVIEW USING SCREEN 9100.
CONTROLS: TC_9200  TYPE TABLEVIEW USING SCREEN 9200.

*//Type (Table Structure);(TY_ )- Table or Structure

*-- PF-Status : Excluding Function Code table
TYPES: BEGIN OF TY_FCODE,
        FCODE LIKE RSMPE-FUNC,
      END OF TY_FCODE.

DATA: IT_EX_FUNC TYPE STANDARD TABLE OF TY_FCODE WITH
                       NON-UNIQUE DEFAULT KEY INITIAL SIZE 5,
      WA_EX_FUNC TYPE TY_FCODE.


*//Constants ;(C_) ==> True:'X' or '1' False:Space or '0'
CONSTANTS : C_MARK   VALUE 'X'.

**/-- Screen Control Mode
*CONSTANTS :

*-- Screen control Tcode Constants.
CONSTANTS : C_MANAGER_TCODE  LIKE SY-TCODE  VALUE 'ZQMA05M',
            C_RESP_PER_TCODE LIKE SY-TCODE  VALUE 'ZQMA05R'.

**-- Process Status

**//-- Global : used Variable just in this Program
*-- Function Control
DATA : OK_CODE LIKE SY-UCOMM.
DATA : WA_MODE(7) TYPE C,
       WA_STATUS(8) TYPE C.

DATA : WA_SUBRC TYPE C.

DATA : WA_VALID_TO_DATE TYPE  CP_STTAG VALUE '20040101'. "Key date



*-  BDC Mode
DATA : WA_BDCMODE   TYPE TB_BDCMODE VALUE 'A'.



*-- Screnn field cursor control
DATA : WA_FLDTXT    LIKE FELD-NAME,  "Field Name Variable
       WA_CUR_LINE  LIKE FELD-LINE.  "Field Line Variable

*-- Table Control Variables
DATA : WA_CON_LINES LIKE SY-LOOPC.  " LINES OF TABLECONTROL
DATA : WA_COUNT     TYPE   I.       " COUNT Variables
DATA : WA_LINES     LIKE SY-TABIX.
DATA : WA_TABIX     LIKE SY-TABIX.
DATA : WA_SEL_LINE  LIKE SY-TABIX.  "Select Line of T/C
DATA : WA_TCNAME    LIKE FELD-NAME. "table control Field Name

*--
DATA : WA_RETURN     LIKE	BAPIRETURN1.   "Return Values

*-- User Confirm for pop-up Message
DATA : WA_ANSWER TYPE C.
DATA : WA_REPID LIKE SY-REPID.

*-- Work area Variables in Program.
*DATA :

*//Data(Work Area or (Internal) Structures);(WA_ )(ST_)?

**-- List box variables
DATA: WA_NAME  TYPE VRM_ID,
      IT_LIST  TYPE VRM_VALUES,
      WA_VALUE LIKE LINE OF IT_LIST.

*//Internal Tables and Index Fields;(IT_), (I_)
DATA : IT_BDCMSG_COL  LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       IT_RET_MSG_COL LIKE BAPIRETURN OCCURS 0 WITH HEADER LINE.


DATA : IT_FIELDCAT_ALV  TYPE SLIS_T_FIELDCAT_ALV.
DATA : IT_EVENTS        TYPE	SLIS_T_EVENT.

**/-- Internale Tables with structure as sama as DB
*- Assing EXTWG to Material Str.
DATA : IT_ZSQM_MAT_EXT LIKE ZSQM_MAT_EXT OCCURS 0
                                          WITH HEADER LINE.
*- Assign Resp. person and Input confirm data and Usage
DATA : IT_ZSQM_MAT_CNF LIKE ZSQM_MAT_CNF OCCURS 0 WITH HEADER LINE.

*- Regular material information table for access to DB
DATA : IT_ZTQM_MAT_REG   LIKE ZTQM_MAT_REG OCCURS 0 WITH HEADER LINE,
       IT_ZTQM_MAT_REG_B LIKE ZTQM_MAT_REG OCCURS 0 WITH HEADER LINE.



*//Field Symbols; <FS_>
*-- TABLE CONTROLS VARIABLE(field-symbols)
FIELD-SYMBOLS: <TC>  TYPE CXTAB_CONTROL. "table control
"                              Table_control Object(CXTAB)

FIELD-SYMBOLS : <FS>.
*//Field Group;

***//Macro Definitions
*-- macro : RANGE_SET &1 &2 &3
*--           &1 - Range Variable
*--           &2 - Low Variable
*--           &3 - high Variable
DEFINE RANGE_SET.
  IF   NOT &2 IS INITIAL OR
       NOT &3 IS INITIAL.
    MOVE : 'I' TO &1-SIGN.
    IF NOT &2 IS INITIAL AND
           &3 IS INITIAL.
      MOVE :'EQ' TO &1-OPTION.
      MOVE : &2  TO &1-LOW.
    ELSEIF NOT &2 IS INITIAL AND
           NOT &3 IS INITIAL.
      MOVE : 'BT' TO &1-OPTION.
      MOVE : &2   TO &1-LOW,
             &3   TO &1-HIGH.
    ELSEIF  &2 IS INITIAL AND
           NOT &3 IS INITIAL.
      MOVE : 'EQ' TO &1-OPTION.
      MOVE : &3   TO &1-LOW.
    ENDIF.
    APPEND &1.
  ENDIF.
END-OF-DEFINITION.

*-- macro : EXCLUDE_FUNC &1
*--           &1 - F.code for excluding
DEFINE EXCLUDE_FUNC.
  WA_EX_FUNC = &1.
  APPEND WA_EX_FUNC TO IT_EX_FUNC.
END-OF-DEFINITION.




*//Ranges; (R_)
*-- Selection variables
RANGES : R_EXTWG FOR MARA-EXTWG,
         R_RESPP FOR USER_ADDR-BNAME.

****//& Selection Screen Definition(Parameters Select-Option)
**-- Paramerters : (P_), Select-Options : (S_)
*SELECTION-SCREEN BEGIN OF BLOCK BLK WITH FRAME  TITLE TEXT-T01.
*SELECTION-SCREEN END OF BLOCK BLK .

*-- Seclection Screen Flow Logic Control Event Handling
*AT SELECTION-SCREEN ON ( ON END OF, ON VALUE-REQUEST FOR,
* ON HELP-REQUEST FOR, ON RADIOBUTTON GROUP, ON BLOCK OUTPUT,
* ON EXIT-COMMAND )
*AT SELECTION-SCREEN OUTPUT.
*  SET TITLEBAR '1000'.

*AT SELECTION-SCREEN.
*  CHECK SY-UCOMM = 'ONLI'.


*-- Selection for Selection Screen
*START-OF-SELECTION.
**-- End of Selection.
*END-OF-SELECTION.



*// Event Handling(Except Selection Screen (Flow)event)
LOAD-OF-PROGRAM.
*- Set Default process control
  ZSQM_MAT_SEL-TASK = C_MARK.

INITIALIZATION.





**<<<<<<<<< Program Main / Subroutine / Flow Logic >>>>>>>>>>>>**
*&------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS '9000'.

  IF SY-TCODE+6(1) = 'M'.   "/Manager Transaction code.
    SET TITLEBAR  '9000' WITH TEXT-TM1.
  ELSEIF SY-TCODE+6(1) = 'R'.  "/Resp. person Ttansaction code
    SET TITLEBAR  '9000' WITH TEXT-TR1.
  ENDIF.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------*
*&      Module  SET_CURSOR_FIELD  OUTPUT
*&---------------------------------------------------------------*
MODULE SET_CURSOR_FIELD OUTPUT.
  SET CURSOR FIELD WA_FLDTXT LINE WA_CUR_LINE.
ENDMODULE.                 " SET_CURSOR_FIELD  OUTPUT
*&----------------------------------------------------------------*
*&      Module  GET_CURSOR_FIELD  INPUT
*&----------------------------------------------------------------*
MODULE GET_CURSOR_FIELD INPUT.
  CLEAR: WA_FLDTXT, WA_CUR_LINE.
  GET CURSOR FIELD WA_FLDTXT LINE WA_CUR_LINE.
ENDMODULE.                 " GET_CURSOR_FIELD  INPUT
*&-----------------------------------------------------------------*
*&      Module  EXIT_9000  INPUT
*&-----------------------------------------------------------------*
MODULE EXIT_9000 INPUT.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.

  CASE OK_CODE.
    WHEN 'EXIT'.

      LEAVE TO SCREEN 0.

    WHEN 'RW'.

      LEAVE TO SCREEN 0.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " EXIT_9000  INPUT
*&-----------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&-----------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.
  CASE OK_CODE.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'ONLI'.
*-- Check Authority by T.Code for manager and Responsible person


*-- Check Required field value
      IF ZSQM_MAT_SEL-WERKS IS INITIAL OR
         ZSQM_MAT_SEL-IYEAR IS INITIAL.
        MESSAGE E000(ZMQM) WITH
               'Plant and Inspection year are required.'(E01).
        EXIT.
      ENDIF.

      REFRESH : IT_ZSQM_MAT_CNF.
      REFRESH : R_EXTWG, R_RESPP.

      PERFORM RANGE_SET_FOR_SELECT.

**/Task and Information
      CASE ZSQM_MAT_SEL-TASK.
** - Task
        WHEN C_MARK.

*-        get data by user selection process
*      PERFORM GET_MATERIAL_DATA_FOR_REG  TABLES IT_ZSQM_MAT_CNF
*                                         USING  ZSQM_MAT_SEL.

          PERFORM GET_MAT_DATA_FOR_REG_V2  TABLES IT_ZSQM_MAT_CNF
                                           USING  ZSQM_MAT_SEL.

          IF IT_ZSQM_MAT_CNF[] IS INITIAL.

            MESSAGE W000(ZMQM) WITH 'Data not founded'(E03).
            EXIT.
          ENDIF.

*       Sort internal table :
* changed by 100565 07/26/04 - start
*          SORT IT_ZSQM_MAT_CNF BY EXTWG MATNR MTART WERKS ASCENDING.
          SORT IT_ZSQM_MAT_CNF BY MATNR EXTWG  MTART WERKS ASCENDING.
* changed by 100565 07/26/04 - end

*-   get backup data for check and compare with
*-   changed data (for timestamp - changing information)
*      PERFORM GET_MATERIAL_DATA_FOR_HISTORY TABLES IT_ZTQM_MAT_REG_B
*                                                 USING  ZSQM_MAT_SEL.
          PERFORM GET_MAT_DATA_FOR_HISTORY_V2 TABLES IT_ZTQM_MAT_REG_B
                                               USING  ZSQM_MAT_SEL.

** - Information
        WHEN ' '.  "/Information
*-        get data by user selection process

          PERFORM GET_MAT_DATA_FOR_REG_V3  TABLES IT_ZSQM_MAT_CNF
                                           USING  ZSQM_MAT_SEL.

          IF IT_ZSQM_MAT_CNF[] IS INITIAL.

            MESSAGE W000(ZMQM) WITH 'Data not founded'(E03).
            EXIT.
          ENDIF.

*       Sort internal table :
*          SORT IT_ZSQM_MAT_CNF BY EXTWG MATNR MTART WERKS ASCENDING.
* changed by 100565 07/26/04 - start
*          SORT IT_ZSQM_MAT_CNF BY EXTWG MATNR MTART WERKS ASCENDING.
          SORT IT_ZSQM_MAT_CNF BY MATNR EXTWG  MTART WERKS ASCENDING.
* changed by 100565 07/26/04 - end


*-   get backup data for check and compare with changed data

          PERFORM GET_MAT_DATA_FOR_HISTORY_V3 TABLES IT_ZTQM_MAT_REG_B
                                               USING  ZSQM_MAT_SEL.

      ENDCASE.

*--   Excluding need not function code on Toolbar
      PERFORM SET_EXCLUDING_FUNC.

      TC_9200-TOP_LINE = 1.
      CALL SCREEN 9200.

    WHEN OTHERS.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&-----------------------------------------------------------------*
*&      Module  F4_HELP_RESP_PERSON  INPUT
*&------------------------------------------------------------------*
MODULE F4_HELP_RESP_PERSON INPUT.
  PERFORM GET_PARTNER_VALUE   USING ZQMT1_PARVW_RESP_PERSON  "/respons.
                                    'ZSQM_MAT_SEL-RESPP'
                                    'ZSQM_MAT_SEL-NAME_TEXT'.
ENDMODULE.                 " F4_HELP_RESP_PERSON  INPUT
*&-----------------------------------------------------------------*
*&      Form  GET_PARTNER_VALUE
*&-----------------------------------------------------------------*
FORM GET_PARTNER_VALUE USING    VALUE(P_PARVW)
                                VALUE(P_PARNR_FIELD)
                                VALUE(P_NAME_FIELD).
*// TPAR : Business Partner: Functions Table
  DATA : LW_NRART LIKE TPAR-NRART. "/Type of partner number
  DATA : LW_PARNR LIKE IHPA-PARNR. "/Selected Partner
  DATA : LW_NAME_LIST LIKE USER_ADDR-NAME_TEXTC. "User name


  DATA: BEGIN OF LW_DYNPFIELDS OCCURS 100.
          INCLUDE STRUCTURE DYNPREAD.
  DATA: END OF LW_DYNPFIELDS.
  DATA: LW_DYNAME LIKE D020S-PROG.
  DATA: LW_DYNUMB LIKE D020S-DNUM.

  CHECK NOT P_PARVW IS INITIAL.

*-- get partner type of partner number(partner function)
  SELECT SINGLE NRART INTO LW_NRART
    FROM TPAR
      WHERE PARVW = P_PARVW.

  CHECK SY-SUBRC = 0.
*--Search Help using partner type(LW_NRART)-HR organization or SAP User
  CALL FUNCTION 'SEARCH_OM_PARTNER'
       EXPORTING
            ACT_NRART         = LW_NRART
*            ACTIVE_PLVAR      = ' '
            SEARCH_STRING     = ' '
*            RESTRICTION_OTYPE = ' '
*            RESTRICTION_OBJID = '00000000'
       IMPORTING
            SEL_PARNR         = LW_PARNR
       EXCEPTIONS
            NO_ACTIVE_PLVAR   = 1
            NOT_SELECTED      = 2
            NO_OM_OTYPE       = 3
            OTHERS            = 4.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CHECK NOT LW_PARNR IS INITIAL.


  CASE P_PARVW.
    WHEN 'VW'.                            "/Person respons.
      SELECT SINGLE NAME_TEXTC     INTO LW_NAME_LIST
        FROM USER_ADDR
           WHERE BNAME = LW_PARNR.
    WHEN OTHERS.

  ENDCASE.

*-- Update screen field data using FUNCTION 'DYNP_VALUES_UPDATE'

  LW_DYNAME = SY-REPID.
  LW_DYNUMB = SY-DYNNR.

  LW_DYNPFIELDS-FIELDNAME  = P_PARNR_FIELD.
  LW_DYNPFIELDS-FIELDVALUE = LW_PARNR.
  APPEND LW_DYNPFIELDS.
  LW_DYNPFIELDS-FIELDNAME  = P_NAME_FIELD.
  LW_DYNPFIELDS-FIELDVALUE = LW_NAME_LIST.
  APPEND LW_DYNPFIELDS.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
       EXPORTING
            DYNAME     = LW_DYNAME
            DYNUMB     = LW_DYNUMB
       TABLES
            DYNPFIELDS = LW_DYNPFIELDS.

ENDFORM.                    " GET_PARTNER_VALUE
*&------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_9000  OUTPUT
*&------------------------------------------------------------------*
MODULE MODIFY_SCREEN_9000 OUTPUT.

  CASE SY-TCODE.
    WHEN C_MANAGER_TCODE.                 "/For Manager

      CASE ZSQM_MAT_SEL-TASK.  "/Check.
        WHEN C_MARK.
          LOOP AT SCREEN.
            IF SCREEN-GROUP1 = 'DSP'.
              SCREEN-INPUT = 0.
              MODIFY SCREEN.
            ENDIF.
          ENDLOOP.
        WHEN OTHERS.
          LOOP AT SCREEN.
            IF SCREEN-GROUP1 = 'TSK'.
              SCREEN-INPUT = 0.
              MODIFY SCREEN.
            ENDIF.
          ENDLOOP.
      ENDCASE.

    WHEN C_RESP_PER_TCODE.                "/For Responsible person
      CASE ZSQM_MAT_SEL-TASK.  "/Check.
        WHEN C_MARK.
          LOOP AT SCREEN.
            IF SCREEN-GROUP1 = 'DSP' OR
               SCREEN-GROUP2 = 'MAN'.
              SCREEN-INPUT = 0.
              MODIFY SCREEN.
            ENDIF.
          ENDLOOP.
        WHEN OTHERS.
          LOOP AT SCREEN.
            IF SCREEN-GROUP1 = 'TSK'.
              SCREEN-INPUT = 0.
              MODIFY SCREEN.
            ENDIF.
          ENDLOOP.
      ENDCASE.
  ENDCASE.

ENDMODULE.                 " MODIFY_SCREEN_9000  OUTPUT
*&------------------------------------------------------------------*
*&      Form  GET_NO_SETUP_MATERIAL
*&------------------------------------------------------------------*
FORM GET_NO_SETUP_MATERIAL TABLES PT_MAT_EXT STRUCTURE ZSQM_MAT_EXT.

  IF ZSQM_MAT_SEL-WERKS IS INITIAL.
    SELECT DISTINCT A~MATNR A~MTART B~PLNT AS WERKS A~EXTWG C~MAKTX
      INTO CORRESPONDING FIELDS OF TABLE PT_MAT_EXT
        FROM (  MARA AS A INNER JOIN ZTBM_ABXMMCDT AS B
           ON  A~MATNR = B~MTNO ) INNER JOIN MAKT AS C
           ON  A~MATNR = C~MATNR
          WHERE A~EXTWG = ' '   "/Not assigned : Ext. mat. group
            AND ( (  A~MTART = ZQMT1_MTART_ROH
                AND A~MSTAE = ZQMT1_MSTAE_USING  "/Status'Using'
                 AND B~SOUR  IN ('V', 'K') )
              OR
                  (  A~MTART = ZQMT1_MTART_HALB
                 AND B~SOUR  = 'M' )  )
            AND C~SPRAS = SY-LANGU.

  ELSE.
    SELECT DISTINCT A~MATNR A~MTART B~PLNT AS WERKS A~EXTWG C~MAKTX
       INTO CORRESPONDING FIELDS OF TABLE PT_MAT_EXT
        FROM (  MARA AS A INNER JOIN ZTBM_ABXMMCDT AS B
           ON  A~MATNR = B~MTNO ) INNER JOIN MAKT AS C
           ON  A~MATNR = C~MATNR
          WHERE A~EXTWG = ' '   "/Not assigned : Ext. mat. group
            AND ( (  A~MTART = ZQMT1_MTART_ROH
                AND A~MSTAE = ZQMT1_MSTAE_USING  "/Status 'Using'
                 AND B~SOUR  IN ('V', 'K') )
              OR
                  (  A~MTART = ZQMT1_MTART_HALB
                 AND B~SOUR  = 'M' )  )
            AND B~PLNT  = ZSQM_MAT_SEL-WERKS
            AND C~SPRAS = SY-LANGU.
  ENDIF.

ENDFORM.                    " GET_NO_SETUP_MATERIAL
*&------------------------------------------------------------------*
*&      Module  STATUS_9100  OUTPUT
*&------------------------------------------------------------------*
MODULE STATUS_9100 OUTPUT.
  SET PF-STATUS '9100'.
  SET TITLEBAR '9100'.

ENDMODULE.                 " STATUS_9100  OUTPUT
*&------------------------------------------------------------------*
*&      Module  TABLE_CONTROL_OUTPUT_9100  OUTPUT
*&------------------------------------------------------------------*
MODULE TABLE_CONTROL_OUTPUT_9100 OUTPUT.
  CONCATENATE 'TC_' SY-DYNNR INTO WA_TCNAME.
  ASSIGN (WA_TCNAME) TO <TC>.

  READ TABLE IT_ZSQM_MAT_EXT INDEX <TC>-CURRENT_LINE .
  IF SY-SUBRC EQ 0.
    MOVE-CORRESPONDING IT_ZSQM_MAT_EXT TO ZSQM_MAT_EXT.
  ELSE.
    CLEAR ZSQM_MAT_EXT.
  ENDIF.

  WA_CON_LINES = SY-LOOPC.
ENDMODULE.                 " TABLE_CONTROL_OUTPUT_9100  OUTPUT
*&------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_TABLE_9100  OUTPUT
*&------------------------------------------------------------------*
MODULE MODIFY_SCREEN_TABLE_9100 OUTPUT.
  IF ZSQM_MAT_EXT IS INITIAL.
    LOOP AT SCREEN.
      SCREEN-ACTIVE = 0.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " MODIFY_SCREEN_TABLE_9100  OUTPUT
*&-----------------------------------------------------------------*
*&      Module  TABLE_CONTROL_LINES_9100  OUTPUT
*&------------------------------------------------------------------*
MODULE TABLE_CONTROL_LINES_9100 OUTPUT.
*--- Move the number of Internal Table Records to TABLE CONTROL-LINES
  CONCATENATE 'TC_' SY-DYNNR INTO WA_TCNAME.
  ASSIGN (WA_TCNAME) TO <TC>.                "not headerline

  DESCRIBE TABLE IT_ZSQM_MAT_EXT LINES <TC>-LINES.
ENDMODULE.                 " TABLE_CONTROL_LINES_9100  OUTPUT
*&------------------------------------------------------------------*
*&      Module  TABLE_CONTROL_INPUT_9100  INPUT
*&------------------------------------------------------------------*
MODULE TABLE_CONTROL_INPUT_9100 INPUT.
*---MOVE data to Internal Table from TABLE CONTROL.
  CONCATENATE 'TC_' SY-DYNNR INTO WA_TCNAME.
  ASSIGN (WA_TCNAME) TO <TC>.

  CLEAR IT_ZSQM_MAT_EXT.

  READ TABLE IT_ZSQM_MAT_EXT INDEX <TC>-CURRENT_LINE.

  IF SY-SUBRC NE 0 AND  NOT ZSQM_MAT_EXT IS INITIAL.
    MOVE-CORRESPONDING ZSQM_MAT_EXT TO IT_ZSQM_MAT_EXT.
*    MOVE : SY-DATUM  TO IT_ZSQM_MAT_EXT-ERDAT,
*           SY-UZEIT  TO IT_ZSQM_MAT_EXT-ERZET,
*           SY-UNAME  TO IT_ZSQM_MAT_EXT-ERNAM.

    APPEND IT_ZSQM_MAT_EXT.
    EXIT.
  ELSE.

    MOVE-CORRESPONDING ZSQM_MAT_EXT TO IT_ZSQM_MAT_EXT.

    MODIFY IT_ZSQM_MAT_EXT INDEX <TC>-CURRENT_LINE.

  ENDIF.

**-- move Selected Table Control Index to Global Variable(WA_SEL_LINE)
*  IF ZSQM_MAT_EXT-MARK = C_MARK.
*    WA_SEL_LINE = <TC>-CURRENT_LINE.
*  ENDIF.
*

ENDMODULE.                 " TABLE_CONTROL_INPUT_9100  INPUT
*&------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_9100  OUTPUT
*&------------------------------------------------------------------*
MODULE MODIFY_SCREEN_9100 OUTPUT.
*  CASE ZSQM_MAT_SEL-NO_SETUP.
*    WHEN C_MARK.
*      LOOP AT SCREEN.
*        IF SCREEN-GROUP3 = 'NOT'.
*          SCREEN-ACTIVE = 0.
*          MODIFY SCREEN.
*        ENDIF.
*      ENDLOOP.
*    WHEN OTHERS.
**      LOOP AT SCREEN.
**        IF SCREEN-GROUP2 = 'REQ'.
**          SCREEN-REQUIRED = 1.
**          MODIFY SCREEN.
**        ENDIF.
**      ENDLOOP.
*  ENDCASE.
ENDMODULE.                 " MODIFY_SCREEN_9100  OUTPUT
*&------------------------------------------------------------------*
*&      Module  EXIT_9100  INPUT
*&------------------------------------------------------------------*
MODULE EXIT_9100 INPUT.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.

  CASE OK_CODE.
    WHEN 'EXIT'.

      LEAVE TO SCREEN 0.

    WHEN 'RW'.

      LEAVE TO SCREEN 0.

    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " EXIT_9100  INPUT
*&------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100  INPUT
*&------------------------------------------------------------------*
MODULE USER_COMMAND_9100 INPUT.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.
  CASE OK_CODE.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'SAVE'.
*   - Save, change and Set-up inspection

*      CASE ZSQM_MAT_SEL-NO_SETUP.
*        WHEN C_MARK.  "/No inspection set-up
*
**        Delete item which is not assigned to EXTWG
*          DATA : LT_MAT_EXT LIKE ZSQM_MAT_EXT OCCURS 0 WITH HEADER LINE
      .
*          LT_MAT_EXT[] = IT_ZSQM_MAT_EXT[].
*
*          DELETE LT_MAT_EXT WHERE EXTWG = ' '.
*
*          CHECK NOT LT_MAT_EXT[] IS INITIAL.
*
*          PERFORM SET_MAT_MASTER_AND_PLAN TABLES LT_MAT_EXT.
*
***--   Save assigned parts to ext. mat. group to CBOtable(ZTQM_MAT_REG)
**          CHECK NOT LT_MAT_EXT[] IS INITIAL.
**          PERFORM SAVE_MAT_DATA_TO_DB_9100  TABLES LT_MAT_EXT.
*
**-      Display Return message.
*          PERFORM DISPLAY_BDC_MESSAGE TABLES IT_BDCMSG_COL
*                                             IT_RET_MSG_COL.
*
*
*        WHEN OTHERS.  "/
*
*      ENDCASE.
*
*      LEAVE TO SCREEN 0.

*-  Sorting
    WHEN 'SORTA' OR 'SORTD'.

      PERFORM SORT_TABLE   USING 'IT_ZSQM_MAT_EXT'
                                 WA_FLDTXT
                                 OK_CODE.


    WHEN OTHERS.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9100  INPUT
*&-----------------------------------------------------------------*
*&      Module  GET_TEXT_SCREEN_9000  INPUT
*&-----------------------------------------------------------------*
MODULE GET_TEXT_SCREEN_9000 INPUT.
  CLEAR    : ZSQM_MAT_SEL-NAME1,
             ZSQM_MAT_SEL-NAME_TEXT,
             ZSQM_MAT_SEL-EWBEZ.

  IF NOT ZSQM_MAT_SEL-WERKS IS INITIAL.
    SELECT SINGLE NAME1 INTO ZSQM_MAT_SEL-NAME1
        FROM T001W
          WHERE WERKS = ZSQM_MAT_SEL-WERKS.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZMQM) WITH ZSQM_MAT_SEL-WERKS
                              'is not exist!'(E10).
    ENDIF.

  ENDIF.

  IF NOT ZSQM_MAT_SEL-RESPP IS INITIAL.
    SELECT SINGLE NAME_TEXTC     INTO ZSQM_MAT_SEL-NAME_TEXT
        FROM USER_ADDR
           WHERE BNAME = ZSQM_MAT_SEL-RESPP.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZMQM) WITH ZSQM_MAT_SEL-RESPP
                              TEXT-E10.
    ENDIF.
  ENDIF.

  IF NOT ZSQM_MAT_SEL-EXTWG IS INITIAL.

    PERFORM CHECK_AND_GET_TEXT_EXTWG  USING ZSQM_MAT_SEL-EXTWG
                                            ZSQM_MAT_SEL-EWBEZ.

  ENDIF.

ENDMODULE.                 " GET_TEXT_SCREEN_9000  INPUT
*&-----------------------------------------------------------------*
*&      Module  CHECK_AND_GET_TEXT_EXTWG  INPUT
*&-----------------------------------------------------------------*
MODULE CHECK_AND_GET_TEXT_EXTWG INPUT.

  CHECK NOT ZSQM_MAT_EXT-EXTWG IS INITIAL.

  PERFORM CHECK_AND_GET_TEXT_EXTWG  USING ZSQM_MAT_EXT-EXTWG
                                          ZSQM_MAT_EXT-EWBEZ.

ENDMODULE.                 " CHECK_AND_GET_TEXT_EXTWG  INPUT
*&---------------------------------------------------------------------*
*&      Form  CHECK_AND_GET_TEXT_EXTWG
*&---------------------------------------------------------------------*
FORM CHECK_AND_GET_TEXT_EXTWG USING    P_EXTWG
                                       P_EWBEZ.

  SELECT SINGLE EWBEZ INTO P_EWBEZ
     FROM TWEWT
       WHERE EXTWG = P_EXTWG
         AND SPRAS = SY-LANGU.

  IF SY-SUBRC NE 0.
    MESSAGE E000(ZMQM) WITH P_EXTWG
                            ','
                            ''
                            TEXT-E10.
  ENDIF.
ENDFORM.                    " CHECK_AND_GET_TEXT_EXTWG
*&------------------------------------------------------------------*
*&      Form  SET_MAT_MASTER_AND_PLAN
*&------------------------------------------------------------------*
FORM SET_MAT_MASTER_AND_PLAN TABLES  PT_MAT_EXT STRUCTURE ZSQM_MAT_EXT.

  DATA : LT_ART LIKE ZSQM_QPART OCCURS 3 WITH HEADER LINE.
  DATA : LT_MSG LIKE BDCMSGCOLL OCCURS 5 WITH HEADER LINE.

  DATA : LW_RETURN TYPE BAPIRETURN.

  DATA : LW_INDEX LIKE SY-TABIX.

  REFRESH : IT_BDCMSG_COL , "/Collect BDC MSG
            IT_RET_MSG_COL. "/Collect Function Return message

*-- make inspection type table for inspectio setup for material
*-   Regular and MS type are appended to Inspection type table
  CLEAR LT_ART.
  MOVE : ZQMT1_INSP_TYPE_REGULAR TO LT_ART-ART. APPEND LT_ART.
  MOVE : ZQMT1_INSP_TYPE_MS      TO LT_ART-ART. APPEND LT_ART.


  LOOP AT PT_MAT_EXT.
    LW_INDEX = SY-TABIX.
*-- Change the material Master:EXTWG and QM-VIEW

    CALL FUNCTION 'Z_FQM_SET_MAT_EXTWG_N_QM_VIEW'
         EXPORTING
              I_MATNR           = PT_MAT_EXT-MATNR
              I_WERKS           = PT_MAT_EXT-WERKS
              I_EXTWG           = PT_MAT_EXT-EXTWG
              I_BDCMODE         = WA_BDCMODE
         IMPORTING
              E_RETURN          = LW_RETURN
         TABLES
              T_ART             = LT_ART
              T_MSG             = LT_MSG
         EXCEPTIONS
              BDC_ERROR_FOUNDED = 1
              OTHERS            = 2.

*    IF SY-SUBRC <> 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.

*-   Collect message from Function to internal message tables
    APPEND LINES OF LT_MSG TO IT_BDCMSG_COL.
    APPEND LW_RETURN       TO IT_RET_MSG_COL.


*-   check change Material master QM View
    IF LW_RETURN-TYPE NE 'S'. "/
*      DELETE PT_MAT_EXT INDEX LW_INDEX.
      CONTINUE.
    ENDIF.

**--  Change the inspection plan for material
*-    Material assignment for inspection plan group(same as EXTWG)
    CLEAR LW_RETURN.
    REFRESH LT_MSG.


    CALL FUNCTION 'Z_FQM_ASSIGN_MAT_TO_INSP_PLAN'
         EXPORTING
              I_MATNR           = PT_MAT_EXT-MATNR
              I_WERKS           = PT_MAT_EXT-WERKS
              I_EXTWG           = PT_MAT_EXT-EXTWG
              I_BDCMODE         = WA_BDCMODE
              I_VALID_DATE      = WA_VALID_TO_DATE
         IMPORTING
              E_RETURN          = LW_RETURN
         TABLES
              T_ART             = LT_ART
              T_MSG             = LT_MSG
         EXCEPTIONS
              BDC_ERROR_FOUNDED = 1
              OTHERS            = 2.
*    IF SY-SUBRC <> 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.

*-   Collect message from Function to internal message tables
    APPEND LINES OF LT_MSG TO IT_BDCMSG_COL.
    APPEND LW_RETURN       TO IT_RET_MSG_COL.

  ENDLOOP.

ENDFORM.                    " SET_MAT_MASTER_AND_PLAN
*&------------------------------------------------------------------*
*&      Form  DISPLAY_BDC_MESSAGE
*&------------------------------------------------------------------*
FORM DISPLAY_BDC_MESSAGE TABLES   PT_BDCMSG      STRUCTURE BDCMSGCOLL
                                  PT_RET_MSG_COL STRUCTURE BAPIRETURN.

  DATA : LW_REPID           LIKE SY-REPID,
         LW_STRUCTURE_NAME  LIKE DD02L-TABNAME VALUE 'BAPIRETURN'.

  DATA : LW_LAYOUT  TYPE SLIS_LAYOUT_ALV.
  DATA : LT_FIELDCAT TYPE    SLIS_T_FIELDCAT_ALV  WITH HEADER LINE.
  DATA : LT_EVENTS   TYPE    SLIS_T_EVENT WITH HEADER LINE.

  LW_REPID = SY-REPID.

  PERFORM SET_ALV_LAYOUT USING LW_LAYOUT.

*- generate the field catalog automatically or semi-automatically by
*- calling function module 'REUSE_ALV_FIELDCATALOG_MERGE'
  PERFORM GET_FIELDCATEGORY  TABLES LT_FIELDCAT
                              USING LW_STRUCTURE_NAME.

*-- Get Event list and set events for use : List type(0~4).
  PERFORM SET_EVENTS_FOR_ALV   TABLES LT_EVENTS
                                USING '0'.  "/simple list


  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM = LW_REPID
            I_STRUCTURE_NAME   = LW_STRUCTURE_NAME
            IS_LAYOUT          = LW_LAYOUT
            IT_FIELDCAT        = LT_FIELDCAT[]
*            I_SAVE             = 'A'
            IT_EVENTS          = LT_EVENTS[]
       TABLES
            T_OUTTAB           = PT_RET_MSG_COL
       EXCEPTIONS
            PROGRAM_ERROR      = 1
            OTHERS             = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " DISPLAY_BDC_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  GET_FIELDCATEGORY
*&---------------------------------------------------------------------*
FORM GET_FIELDCATEGORY  TABLES PT_FIELDCAT       LIKE  IT_FIELDCAT_ALV
                        USING  P_STRUCTURE_NAME  LIKE  DD02L-TABNAME.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            I_STRUCTURE_NAME       = P_STRUCTURE_NAME
       CHANGING
            CT_FIELDCAT            = PT_FIELDCAT[]
       EXCEPTIONS
            INCONSISTENT_INTERFACE = 1
            PROGRAM_ERROR          = 2
            OTHERS                 = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT PT_FIELDCAT.

    CASE PT_FIELDCAT-FIELDNAME.

      WHEN 'TYPE'.
        PT_FIELDCAT-KEY = C_MARK.
      WHEN 'CODE'.
        PT_FIELDCAT-KEY = C_MARK.
      WHEN 'MESSAGE'.

*      WHEN 'LOG_NO'.
*      WHEN 'LOG_MSG_NO'.
*      WHEN 'MESSAGE_V1'.
*      WHEN 'MESSAGE_V2'.
*      WHEN 'MESSAGE_V3'.
*      WHEN 'MESSAGE_V4'.
      WHEN OTHERS.
        PT_FIELDCAT-NO_OUT = C_MARK.

    ENDCASE.
    MODIFY PT_FIELDCAT.
  ENDLOOP.


ENDFORM.                    " GET_FIELDCATEGORY
*&------------------------------------------------------------------*
*&      Form  SET_ALV_LAYOUT
*&------------------------------------------------------------------*
FORM SET_ALV_LAYOUT USING    PW_LAYOUT TYPE	SLIS_LAYOUT_ALV.

  PW_LAYOUT-COLWIDTH_OPTIMIZE = C_MARK.

ENDFORM.                    " SET_ALV_LAYOUT
*&-------------------------------------------------------------------*
*&      Form  SET_EVENTS_FOR_ALV
*&-------------------------------------------------------------------*
FORM SET_EVENTS_FOR_ALV TABLES   PT_EVENTS LIKE	IT_EVENTS
                        USING    VALUE(P_LIST_TYPE) TYPE N.

  DATA : BEGIN OF LT_EVENT_NAMES OCCURS 10,
          EVENT(30) TYPE C,
         END OF LT_EVENT_NAMES.
  DATA : LW_EVENTS LIKE LINE OF PT_EVENTS.

  FIELD-SYMBOLS : <LW_FS>.

  CALL FUNCTION 'REUSE_ALV_EVENT_NAMES_GET'
       EXPORTING
            I_LIST_TYPE     = P_LIST_TYPE
       TABLES
            T_EVENT_NAMES   = LT_EVENT_NAMES
       EXCEPTIONS
            LIST_TYPE_WRONG = 1
            OTHERS          = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

* Events

  LOOP AT LT_EVENT_NAMES.
    CLEAR LW_EVENTS.
    ASSIGN (LT_EVENT_NAMES-EVENT) TO <LW_FS>.
    MOVE : <LW_FS>  TO LW_EVENTS-NAME.
    CASE <LW_FS>.
      WHEN SLIS_EV_ITEM_DATA_EXPAND   . "/ 'ITEM_DATA_EXPAND',
      WHEN SLIS_EV_REPREP_SEL_MODIFY  . "/ 'REPREP_SEL_MODIFY',
      WHEN SLIS_EV_CALLER_EXIT_AT_START . "/ 'CALLER_EXIT',
      WHEN SLIS_EV_USER_COMMAND       . "/ 'USER_COMMAND',
      WHEN SLIS_EV_TOP_OF_PAGE        . "/ 'TOP_OF_PAGE',
        LW_EVENTS-FORM = 'DISPLAY_LOG_HEADER'.
      WHEN SLIS_EV_DATA_CHANGED       . "/ 'DATA_CHANGED',
      WHEN SLIS_EV_TOP_OF_COVERPAGE   . "/ 'TOP_OF_COVERPAGE',
      WHEN SLIS_EV_END_OF_COVERPAGE   . "/ 'END_OF_COVERPAGE',
      WHEN SLIS_EV_FOREIGN_TOP_OF_PAGE . "/ 'FOREIGN_TOP_OF_PAGE',
      WHEN SLIS_EV_FOREIGN_END_OF_PAGE . "/ 'FOREIGN_END_OF_PAGE',
      WHEN SLIS_EV_PF_STATUS_SET      . "/ 'PF_STATUS_SET',
*        PT_EVENTS-FORM = 'SET_PF_STATUS'.
      WHEN SLIS_EV_LIST_MODIFY        . "/ 'LIST_MODIFY',
      WHEN SLIS_EV_TOP_OF_LIST        . "/ 'TOP_OF_LIST',
      WHEN SLIS_EV_END_OF_PAGE        . "/ 'END_OF_PAGE',
      WHEN SLIS_EV_END_OF_LIST        . "/ 'END_OF_LIST',
      WHEN SLIS_EV_AFTER_LINE_OUTPUT  . "/ 'AFTER_LINE_OUTPUT',
      WHEN SLIS_EV_BEFORE_LINE_OUTPUT . "/'BEFORE_LINE_OUTPUT',
      WHEN SLIS_EV_SUBTOTAL_TEXT      . "/'SUBTOTAL_TEXT'.
      WHEN OTHERS.
    ENDCASE.
    APPEND LW_EVENTS TO PT_EVENTS.
  ENDLOOP.

ENDFORM.                    " SET_EVENTS_FOR_ALV
*&------------------------------------------------------------------*
*&      Form  GET_MATERIAL_DATA_FOR_REG
*&------------------------------------------------------------------*
FORM GET_MATERIAL_DATA_FOR_REG TABLES PT_MAT_CNF STRUCTURE ZSQM_MAT_CNF
                               USING  PS_MAT_SEL LIKE ZSQM_MAT_SEL.

*  CASE C_MARK.
*
*    WHEN PS_MAT_SEL-ALL.
*
*      SELECT A~MATNR C~MAKTX A~MTART B~WERKS D~LIFNR A~EXTWG
*        INTO CORRESPONDING FIELDS OF TABLE PT_MAT_CNF
*          FROM ( (  MARA AS A INNER JOIN MARC AS B
*            ON  A~MATNR = B~MATNR ) INNER JOIN MAKT AS C
*            ON  A~MATNR = C~MATNR ) INNER JOIN EINA AS D
*            ON  A~MATNR = D~MATNR
*            WHERE A~EXTWG  NE ' '    "/Not assigned : Ext. mat. group
*              AND B~LVORM = ' '      "/Not Deleted at Plant Level
*              AND ( (  A~MTART = ZQMT1_MTART_ROH
*                   AND A~MSTAE = ZQMT1_MSTAE_USING )"/Status:'Using'
*                OR
*                  (    A~MTART = ZQMT1_MTART_HALB  ) )
*              AND B~QMATV  = C_MARK
*              AND B~WERKS  = PS_MAT_SEL-WERKS
*              AND C~SPRAS = SY-LANGU
*              AND D~LOEKZ = ' '
*              AND A~EXTWG IN R_EXTWG
*              AND A~MATNR NOT IN ( SELECT MATNR
*                                    FROM ZTQM_MAT_REG
*                                     WHERE IYEAR = PS_MAT_SEL-IYEAR
*                                       AND WERKS = PS_MAT_SEL-WERKS ).
**                                       AND EXTWG IN R_EXTWG
**                                       AND RESPP IN R_RESPP ).
*
***-     Set Yes field to 'X' for radio button setting. only not exist
***      material in ZTQM_MAT_REG
**      IF SY-SUBRC = 0.
**        MOVE : C_MARK TO PT_MAT_CNF-ZYES.
**        MODIFY PT_MAT_CNF TRANSPORTING ZYES
**                     WHERE MATNR NE ''.
**      ENDIF.
*
**-    Get material exist in ZTQM_MAT_REG
*      SELECT A~MATNR B~MAKTX A~WERKS C~LIFNR A~EXTWG
*             A~RESPP A~ZYES A~ZNO A~CONFM
*        APPENDING CORRESPONDING FIELDS OF TABLE PT_MAT_CNF
*          FROM ( ZTQM_MAT_REG AS A INNER JOIN MAKT AS B
*             ON A~MATNR = B~MATNR ) INNER JOIN EINA AS C
*             ON A~MATNR = C~MATNR
*            WHERE A~IYEAR = PS_MAT_SEL-IYEAR
*              AND A~WERKS = PS_MAT_SEL-WERKS
*              AND A~EXTWG IN R_EXTWG
*              AND A~RESPP IN R_RESPP
*              AND B~SPRAS = SY-LANGU
*              AND C~LOEKZ = ' '.
*
*    WHEN PS_MAT_SEL-NOT_ASSIGN. " Not assigned resp. person
*
*      SELECT A~MATNR C~MAKTX B~WERKS D~LIFNR A~EXTWG
*        INTO CORRESPONDING FIELDS OF TABLE PT_MAT_CNF
*          FROM ( (  MARA AS A INNER JOIN MARC AS B
*            ON  A~MATNR = B~MATNR ) INNER JOIN MAKT AS C
*            ON  A~MATNR = C~MATNR ) INNER JOIN EINA AS D
*            ON  A~MATNR = D~MATNR
*            WHERE A~EXTWG  NE ' '    "/Not assigned : Ext. mat. group
*              AND B~LVORM = ' '      "/Not Deleted at Plant Level
*              AND ( (  A~MTART = ZQMT1_MTART_ROH
*                   AND A~MSTAE = ZQMT1_MSTAE_USING )"/Status:'Using'
*                OR
*                  (    A~MTART = ZQMT1_MTART_HALB  ) )
*              AND B~QMATV  = C_MARK
*              AND B~WERKS  = PS_MAT_SEL-WERKS
*              AND C~SPRAS = SY-LANGU
*              AND D~LOEKZ = ' '
*              AND A~EXTWG IN R_EXTWG
*              AND A~MATNR NOT IN ( SELECT MATNR
*                                    FROM ZTQM_MAT_REG
*                                     WHERE IYEAR = PS_MAT_SEL-IYEAR
*                                       AND WERKS = PS_MAT_SEL-WERKS ).
**                                       AND EXTWG IN R_EXTWG
**                                       AND RESPP IN R_RESPP ).
*
***-     Set Yes field to 'X' for radio button setting. only not exist
***      material in ZTQM_MAT_REG
**      IF SY-SUBRC = 0.
**        MOVE : C_MARK TO PT_MAT_CNF-ZYES.
**        MODIFY PT_MAT_CNF TRANSPORTING ZYES
**                     WHERE MATNR NE ''.
**      ENDIF.
*
*      SELECT A~MATNR A~WERKS D~MTART A~EXTWG
*             A~RESPP A~ZYES A~ZNO A~CONFM
*             B~LIFNR C~MAKTX
*        APPENDING CORRESPONDING FIELDS OF TABLE PT_MAT_CNF
*           FROM ( ( ZTQM_MAT_REG AS A INNER JOIN EINA AS B
*              ON A~MATNR = B~MATNR ) INNER JOIN MAKT AS C
*              ON A~MATNR = C~MATNR ) INNER JOIN MARA AS D
*              ON A~MATNR = D~MATNR
*             WHERE A~IYEAR = PS_MAT_SEL-IYEAR
*               AND A~WERKS = PS_MAT_SEL-WERKS
*               AND A~EXTWG IN R_EXTWG
**               AND A~ZYES  = 'X'
**               AND A~ZNO   = ' '
*               AND B~LOEKZ = ' '
**               AND A~RESPP IN R_RESPP
*               AND A~RESPP = ' '
*               AND C~SPRAS = SY-LANGU.
*
*    WHEN PS_MAT_SEL-YES.        "/ Need to inspection material
*
*      SELECT A~MATNR A~WERKS D~MTART A~EXTWG
*             A~RESPP A~ZYES A~ZNO A~CONFM
*             B~LIFNR C~MAKTX
*        INTO CORRESPONDING FIELDS OF TABLE PT_MAT_CNF
*           FROM ( ( ZTQM_MAT_REG AS A INNER JOIN EINA AS B
*              ON A~MATNR = B~MATNR ) INNER JOIN MAKT AS C
*              ON A~MATNR = C~MATNR ) INNER JOIN MARA AS D
*              ON A~MATNR = D~MATNR
*             WHERE A~IYEAR = PS_MAT_SEL-IYEAR
*               AND A~WERKS = PS_MAT_SEL-WERKS
*               AND A~EXTWG IN R_EXTWG
*               AND A~RESPP IN R_RESPP
*               AND A~ZYES  = 'X'
*               AND A~ZNO   = ' '
*               AND B~LOEKZ = ' '
*               AND C~SPRAS = SY-LANGU.
*
*    WHEN PS_MAT_SEL-NO.         "/Don't need inspection material
*
*      SELECT A~MATNR A~WERKS D~MTART A~EXTWG
*             A~RESPP A~ZYES A~ZNO A~CONFM
*             B~LIFNR C~MAKTX
*        INTO CORRESPONDING FIELDS OF TABLE PT_MAT_CNF
*           FROM ( ( ZTQM_MAT_REG AS A INNER JOIN EINA AS B
*              ON A~MATNR = B~MATNR ) INNER JOIN MAKT AS C
*              ON A~MATNR = C~MATNR ) INNER JOIN MARA AS D
*              ON A~MATNR = D~MATNR
*             WHERE A~IYEAR = PS_MAT_SEL-IYEAR
*               AND A~WERKS = PS_MAT_SEL-WERKS
*               AND A~EXTWG IN R_EXTWG
*               AND A~RESPP IN R_RESPP
*               AND A~ZYES  = ' '
*               AND A~ZNO   = 'X'
*               AND B~LOEKZ = ' '
*               AND C~SPRAS = SY-LANGU.
*
*    WHEN PS_MAT_SEL-YES_CNF.    "/inspection material and confirmed
*
*      SELECT A~MATNR A~WERKS A~EXTWG  D~MTART
*             A~RESPP A~ZYES A~ZNO A~CONFM
*             B~LIFNR C~MAKTX
*        INTO CORRESPONDING FIELDS OF TABLE PT_MAT_CNF
*           FROM ( ( ZTQM_MAT_REG AS A INNER JOIN EINA AS B
*              ON A~MATNR = B~MATNR ) INNER JOIN MAKT AS C
*              ON A~MATNR = C~MATNR ) INNER JOIN MARA AS D
*              ON A~MATNR = D~MATNR
*             WHERE A~IYEAR = PS_MAT_SEL-IYEAR
*               AND A~WERKS = PS_MAT_SEL-WERKS
*               AND A~EXTWG IN R_EXTWG
*               AND A~RESPP IN R_RESPP
*               AND A~ZYES  = 'X'
*               AND A~ZNO   = ' '
*               AND A~CONFM = 'X'
*               AND B~LOEKZ = ' '
*               AND C~SPRAS = SY-LANGU.
*
*    WHEN OTHERS.
*  ENDCASE.
*
ENDFORM.                    " GET_MATERIAL_DATA_FOR_REG
*&------------------------------------------------------------------*
*&      Module  STATUS_9200  OUTPUT
*&------------------------------------------------------------------*
MODULE STATUS_9200 OUTPUT.
  SET PF-STATUS '9200' EXCLUDING IT_EX_FUNC.

  IF SY-TCODE+6(1) = 'M'.   "/Manager Transaction code.
    SET TITLEBAR  '9200' WITH TEXT-TM1.
  ELSEIF SY-TCODE+6(1) = 'R'.  "/Resp. person Ttansaction code
    SET TITLEBAR  '9200' WITH TEXT-TR1.
  ENDIF.

ENDMODULE.                 " STATUS_9200  OUTPUT
*&------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_9200  OUTPUT
*&------------------------------------------------------------------*
MODULE MODIFY_SCREEN_9200 OUTPUT.

  IF ZSQM_MAT_SEL-TASK = C_MARK.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = 'DSP'.
        SCREEN-ACTIVE = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ELSE.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = 'TSK'.
        SCREEN-ACTIVE = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDMODULE.                 " MODIFY_SCREEN_9200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TABLE_CONTROL_OUTPUT_9200  OUTPUT
*&---------------------------------------------------------------------*
MODULE TABLE_CONTROL_OUTPUT_9200 OUTPUT.
  CONCATENATE 'TC_' SY-DYNNR INTO WA_TCNAME.
  ASSIGN (WA_TCNAME) TO <TC>.


  READ TABLE IT_ZSQM_MAT_CNF INDEX <TC>-CURRENT_LINE .
  IF SY-SUBRC EQ 0.
    MOVE-CORRESPONDING IT_ZSQM_MAT_CNF TO ZSQM_MAT_CNF.
  ELSE.
    CLEAR ZSQM_MAT_CNF.
  ENDIF.

  WA_CON_LINES = SY-LOOPC.
ENDMODULE.                 " TABLE_CONTROL_OUTPUT_9200  OUTPUT
*&------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_TABLE_9200  OUTPUT
*&-------------------------------------------------------------------*
MODULE MODIFY_SCREEN_TABLE_9200 OUTPUT.
  IF ZSQM_MAT_CNF IS INITIAL.
    LOOP AT SCREEN.
      SCREEN-ACTIVE = 0.
      MODIFY SCREEN.
    ENDLOOP.
    EXIT.
  ENDIF.

  CASE SY-TCODE.

**-  "/For manager
    WHEN C_MANAGER_TCODE.            "/For manager

*-- Modify screen by task. by T.Code.

      IF ZSQM_MAT_SEL-TASK = ' '.  "/Information and modify
*<<<    - changed by sllee : 06/25/2004 - req. by Mr Kim - Start
* changes made by 100565 07/16/04 req by Mr Moon start
* code commented
*        CASE C_MARK.
*          WHEN ZSQM_MAT_SEL-ALL. "/Manager can chage data when 'All'
* changes made by 100565 07/16/04 req by Mr Moon End

* changes made by 100565 07/16/04 req by Mr Moon start

 LOOP AT SCREEN.
          IF SCREEN-NAME = 'ZSQM_MAT_CNF-ZYES' or

                 SCREEN-NAME = 'ZSQM_MAT_CNF-ZNO' or
                 SCREEN-NAME = 'ZSQM_MAT_CNF-CONFM'.
                SCREEN-INPUT = 0.
                MODIFY SCREEN.
              ENDIF.

        ENDLOOP.

* changes made by 100565 07/16/04 req by Mr Moon End

* changes made by 100565 07/16/04 req by Mr Moon start
* code commented

*          WHEN OTHERS.
**            LOOP AT SCREEN.
**              SCREEN-INPUT = 0.
**              MODIFY SCREEN.
**            ENDLOOP.
*        ENDCASE.
* changes made by 100565 07/16/04 req by Mr Moon End

*<<<    - changed by sllee : 06/25/2004 - req. by Mr Kim - End

      ELSE.     "/Task

        IF ZSQM_MAT_CNF-CONFM = C_MARK.
          LOOP AT SCREEN.
            IF SCREEN-NAME NE 'ZSQM_MAT_CNF-CONFM'.
              SCREEN-INPUT = 0.
              MODIFY SCREEN.
            ENDIF.
          ENDLOOP.
        ENDIF.

      ENDIF.

**-   "/For Responsible person
    WHEN C_RESP_PER_TCODE.      "/For Responsible person

      IF ZSQM_MAT_SEL-TASK = C_MARK.
        CASE C_MARK.
          WHEN ZSQM_MAT_SEL-P_AS_REP.
            LOOP AT SCREEN.
              IF SCREEN-NAME NE 'ZSQM_MAT_CNF-RESPP'.
                SCREEN-INPUT = 0.
                MODIFY SCREEN.
              ENDIF.
            ENDLOOP.
          WHEN ZSQM_MAT_SEL-P_SEL_MAT.
            LOOP AT SCREEN.
              IF SCREEN-NAME NE 'ZSQM_MAT_CNF-ZYES' AND
                 SCREEN-NAME NE 'ZSQM_MAT_CNF-ZNO'.
                SCREEN-INPUT = 0.
                MODIFY SCREEN.
              ENDIF.
            ENDLOOP.
          WHEN ZSQM_MAT_SEL-P_CONFIRM.
            LOOP AT SCREEN.
              IF SCREEN-NAME NE 'ZSQM_MAT_CNF-CONFM'.
                SCREEN-INPUT = 0.
                MODIFY SCREEN.
              ENDIF.
            ENDLOOP.
          WHEN ZSQM_MAT_SEL-P_CONFYES.
            LOOP AT SCREEN.
              SCREEN-INPUT = 0.
              MODIFY SCREEN.
            ENDLOOP.
        ENDCASE.

      ELSE.    "/Information and modify

        LOOP AT SCREEN.
          SCREEN-INPUT = 0.
          MODIFY SCREEN.
        ENDLOOP.
      ENDIF.

  ENDCASE.

ENDMODULE.                 " MODIFY_SCREEN_TABLE_9200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TABLE_CONTROL_LINES_9200  OUTPUT
*&---------------------------------------------------------------------*
MODULE TABLE_CONTROL_LINES_9200 OUTPUT.
*--- Move the number of Internal Table Records to TABLE CONTROL-LINES
  CONCATENATE 'TC_' SY-DYNNR INTO WA_TCNAME.
  ASSIGN (WA_TCNAME) TO <TC>.                "not headerline

  DESCRIBE TABLE IT_ZSQM_MAT_CNF LINES <TC>-LINES.
ENDMODULE.                 " TABLE_CONTROL_LINES_9200  OUTPUT
*&-----------------------------------------------------------------*
*&      Module  EXIT_9200  INPUT
*&-----------------------------------------------------------------*
MODULE EXIT_9200 INPUT.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.

  CASE OK_CODE.
    WHEN 'EXIT'.

      LEAVE TO SCREEN 0.

    WHEN 'RW'.

      LEAVE TO SCREEN 0.

    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " EXIT_9200  INPUT
*&---------------------------------------------------------------------*
*&      Module  TABLE_CONTROL_INPUT_9200  INPUT
*&---------------------------------------------------------------------*
MODULE TABLE_CONTROL_INPUT_9200 INPUT.
*---MOVE data to Internal Table from TABLE CONTROL.
  CONCATENATE 'TC_' SY-DYNNR INTO WA_TCNAME.
  ASSIGN (WA_TCNAME) TO <TC>.

  CLEAR IT_ZSQM_MAT_CNF.

  READ TABLE IT_ZSQM_MAT_CNF INDEX <TC>-CURRENT_LINE.

  IF SY-SUBRC NE 0 AND  NOT ZSQM_MAT_CNF IS INITIAL.
    MOVE-CORRESPONDING ZSQM_MAT_CNF TO IT_ZSQM_MAT_CNF.
*    MOVE : SY-DATUM  TO IT_ZSQM_MAT_CNF-ERDAT,
*           SY-UZEIT  TO IT_ZSQM_MAT_CNF-ERZET,
*           SY-UNAME  TO IT_ZSQM_MAT_CNF-ERNAM.

    APPEND IT_ZSQM_MAT_CNF.
    EXIT.
  ELSE.

    MOVE-CORRESPONDING ZSQM_MAT_CNF TO IT_ZSQM_MAT_CNF.

    MODIFY IT_ZSQM_MAT_CNF INDEX <TC>-CURRENT_LINE.

  ENDIF.

**-- move Selected Table Control Index to Global Variable(WA_SEL_LINE)
*  IF ZSQM_MAT_CNF-MARK = C_MARK.
*    WA_SEL_LINE = <TC>-CURRENT_LINE.
*  ENDIF.
*

ENDMODULE.                 " TABLE_CONTROL_INPUT_9200  INPUT
*&------------------------------------------------------------------*
*&      Module  USER_COMMAND_9200  INPUT
*&------------------------------------------------------------------*
MODULE USER_COMMAND_9200 INPUT.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.
  CASE OK_CODE.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'SAVE'.

      PERFORM PREPARE_FOR_SAVING.

      PERFORM SAVE_DATA_REGULAR  USING WA_SUBRC.

      CHECK WA_SUBRC = C_MARK.

      LEAVE TO SCREEN 0.

    WHEN 'SEND_MAIL'.  "/Send mail to responsible person

      PERFORM SEND_MAIL_SAP_OFFICE.

    WHEN 'YESNO'.  "/Select inspection material

      PERFORM CHECK_AND_SET_YES_OR_NO.

    WHEN 'CONFIRM'. "/Click confirm box
      PERFORM CHECK_CONFIRM_STATUS.


    WHEN 'CONFIRM_AL'. "/Confirm All item

      PERFORM CONFIRM_ALL_ITEM.

    WHEN 'UNCONF_AL'.  "/Unconfirm All confirmed item

      PERFORM UNCONFIRM_CONFIRMED_ITEM.

*-  Sorting
    WHEN 'SORTA' OR 'SORTD'.

      PERFORM SORT_TABLE   USING 'IT_ZSQM_MAT_CNF'
                                 WA_FLDTXT
                                 OK_CODE.

*-  Download to local excel file for scheduling form
    WHEN 'DOWNLOAD'.

*      PERFORM DOWNLOAD_TO_EXCEL.

      PERFORM DOWNLOAD_TO_EXCEL_NEW. "/05/04/2004 - SLLEE

    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9200  INPUT
*&------------------------------------------------------------------*
*&      Form  RANGE_SET_FOR_SELECT
*&------------------------------------------------------------------*
FORM RANGE_SET_FOR_SELECT.
  RANGE_SET  R_EXTWG  ZSQM_MAT_SEL-EXTWG   ''.
  RANGE_SET  R_RESPP  ZSQM_MAT_SEL-RESPP   ''.
ENDFORM.                    " RANGE_SET_FOR_SELECT
*&-----------------------------------------------------------------*
*&      Module  F4_HELP_RESP_PERSON_9200  INPUT
*&-----------------------------------------------------------------*
MODULE F4_HELP_RESP_PERSON_9200 INPUT.
  PERFORM GET_PARTNER_VALUE   USING ZQMT1_PARVW_RESP_PERSON "/respons.
                                    'ZSQM_MAT_CNF-RESPP'
                                    'ZSQM_MAT_CNF-NAME_TEXT'.
ENDMODULE.                 " F4_HELP_RESP_PERSON_9200  INPUT
*&------------------------------------------------------------------*
*&      Form  CHECK_CONFIRM_STATUS
*&------------------------------------------------------------------*
FORM CHECK_CONFIRM_STATUS.

  DATA : LW_INDEX LIKE SY-TABIX.
* Find clicked confirm material item index

  LW_INDEX = WA_CUR_LINE + TC_9200-TOP_LINE - 1.

  READ TABLE IT_ZSQM_MAT_CNF INDEX LW_INDEX.

  CHECK SY-SUBRC = 0.

  IF IT_ZSQM_MAT_CNF-CONFM = C_MARK AND
     IT_ZSQM_MAT_CNF-ZYES  = C_MARK AND
     IT_ZSQM_MAT_CNF-RESPP IS INITIAL.
    CLEAR IT_ZSQM_MAT_CNF-CONFM.
    MODIFY IT_ZSQM_MAT_CNF INDEX LW_INDEX.
    MESSAGE W000(ZMQM)
         WITH 'Assign resp. person before confirmation'(W01).
  ENDIF.

ENDFORM.                    " CHECK_CONFIRM_STATUS
*&-----------------------------------------------------------------*
*&      Form  SORT_TABLE
*&-----------------------------------------------------------------*
FORM SORT_TABLE USING    VALUE(P_TABLENAME)
                         PW_FLDTXT
                         P_OK_CODE.
  FIELD-SYMBOLS : <TABLE> TYPE STANDARD TABLE..
  DATA : LW_FIELD_STR(20),
         LW_FIELD(15).

  DATA : LW_TABLENAME(30) TYPE C.

  CONCATENATE P_TABLENAME '[]'  INTO LW_TABLENAME.

  ASSIGN (LW_TABLENAME) TO <TABLE>.

  SPLIT PW_FLDTXT AT '-' INTO LW_FIELD_STR   LW_FIELD.

  CASE P_OK_CODE.
    WHEN 'SORTA'.
      SORT  <TABLE> BY (LW_FIELD) ASCENDING.
    WHEN 'SORTD'.
      SORT  <TABLE> BY (LW_FIELD) DESCENDING.
  ENDCASE.
ENDFORM.                    " SORT_TABLE
*&------------------------------------------------------------------*
*&      Form  SAVE_DATA_REGULAR
*&------------------------------------------------------------------*
FORM SAVE_DATA_REGULAR  USING PW_SUBRC.
  CLEAR PW_SUBRC.

  PERFORM ENQUEUE_DB.

  MODIFY ZTQM_MAT_REG FROM TABLE IT_ZTQM_MAT_REG.

  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    PERFORM DEQUEUE_DB.
    MESSAGE E000(ZMQM) WITH 'Error founded saving data'(E02).
    EXIT.
  ENDIF.

  COMMIT WORK.

  PERFORM DEQUEUE_DB.
  MESSAGE S000(ZMQM) WITH 'Successfully save data'(S01).

  PW_SUBRC = C_MARK.

ENDFORM.                    " SAVE_DATA_REGULAR
*&------------------------------------------------------------------*
*&      Form  ENQUEUE_DB
*&------------------------------------------------------------------*
FORM ENQUEUE_DB.

  CALL FUNCTION 'ENQUEUE_EZ_ZTQM_MAT_REG'
       EXPORTING
            MODE_ZTQM_MAT_REG = 'X'
            MANDT             = SY-MANDT
            IYEAR             = ZSQM_MAT_SEL-IYEAR
*            MATNR             =
       EXCEPTIONS
            FOREIGN_LOCK      = 1
            SYSTEM_FAILURE    = 2
            OTHERS            = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " ENQUEUE_DB
*&------------------------------------------------------------------*
*&      Form  DEQUEUE_DB
*&------------------------------------------------------------------*
FORM DEQUEUE_DB.

  CALL FUNCTION 'DEQUEUE_EZ_ZTQM_MAT_REG'
       EXPORTING
            MODE_ZTQM_MAT_REG = 'X'
            MANDT             = SY-MANDT
            IYEAR             = ZSQM_MAT_SEL-IYEAR.
*            MATNR             = .


ENDFORM.                    " DEQUEUE_DB
*&------------------------------------------------------------------*
*&      Form  PREPARE_FOR_SAVING
*&------------------------------------------------------------------*
FORM PREPARE_FOR_SAVING.
*  Preparation for saving
  DATA LW_TIME_STAMP LIKE ZSCA_TIME_STAMP.

  DATA : BEGIN OF LW_CONFIRM_DATA,
           RESPP  TYPE  ZQM_RESP_P,
           ZYES   TYPE  ZQM_YES_FLAG,
           ZNO    TYPE  ZQM_NO_FLAG,
           CONFM  TYPE  ZQM_CONFIRMED,
         END OF LW_CONFIRM_DATA.

  DATA : LW_CONFIRM_DATA_N LIKE LW_CONFIRM_DATA.

  REFRESH IT_ZTQM_MAT_REG.

  LOOP AT IT_ZSQM_MAT_CNF.
    CLEAR IT_ZTQM_MAT_REG.
    MOVE-CORRESPONDING IT_ZSQM_MAT_CNF TO IT_ZTQM_MAT_REG.
    MOVE : ZSQM_MAT_SEL-IYEAR TO IT_ZTQM_MAT_REG-IYEAR.

*    Check and compare confirm data
    READ TABLE IT_ZTQM_MAT_REG_B WITH KEY IYEAR = IT_ZTQM_MAT_REG-IYEAR
                                          MATNR = IT_ZTQM_MAT_REG-MATNR.
    IF SY-SUBRC = 0. "/change material
      MOVE-CORRESPONDING IT_ZTQM_MAT_REG_B TO LW_TIME_STAMP.

*-    Check whether data is changed and set time stamp/confirm date
      MOVE-CORRESPONDING : IT_ZTQM_MAT_REG_B TO LW_CONFIRM_DATA,
                           IT_ZTQM_MAT_REG   TO LW_CONFIRM_DATA_N.

      IF LW_CONFIRM_DATA NE LW_CONFIRM_DATA_N.
        MOVE : SY-UNAME TO LW_TIME_STAMP-AENAM,
               SY-DATUM TO LW_TIME_STAMP-AEDAT,
               SY-UZEIT TO LW_TIME_STAMP-AEZET.
      ENDIF.

      MOVE-CORRESPONDING LW_TIME_STAMP TO IT_ZTQM_MAT_REG.

      IF IT_ZTQM_MAT_REG-CONFM = C_MARK AND
         IT_ZTQM_MAT_REG-CNFMD IS INITIAL.

        IT_ZTQM_MAT_REG-CNFMD = SY-DATUM.

      ELSEIF     IT_ZTQM_MAT_REG-CONFM = ' ' AND
             NOT IT_ZTQM_MAT_REG-CNFMD IS INITIAL.
        CLEAR IT_ZTQM_MAT_REG-CNFMD.
      ENDIF.

    ELSE.  "/New material.
      MOVE : SY-UNAME TO IT_ZTQM_MAT_REG-ERNAM,
             SY-DATUM TO IT_ZTQM_MAT_REG-ERDAT,
             SY-UZEIT TO IT_ZTQM_MAT_REG-ERZET.
      IF IT_ZTQM_MAT_REG-CONFM = C_MARK.
        IT_ZTQM_MAT_REG-CNFMD = SY-DATUM.
      ELSE.
        CLEAR IT_ZTQM_MAT_REG-CNFMD.
      ENDIF.
    ENDIF.

    APPEND IT_ZTQM_MAT_REG.
  ENDLOOP.
ENDFORM.                    " PREPARE_FOR_SAVING
*&------------------------------------------------------------------*
*&      Form  GET_MATERIAL_DATA_FOR_HISTORY
*&-------------------------------------------------------------------*
FORM GET_MATERIAL_DATA_FOR_HISTORY
                            TABLES PT_MAT_REG_B STRUCTURE ZTQM_MAT_REG
                            USING  PS_MAT_SEL LIKE ZSQM_MAT_SEL.

*  REFRESH PT_MAT_REG_B.
*
*  CASE C_MARK.
*
*    WHEN PS_MAT_SEL-ALL         OR
*         PS_MAT_SEL-NOT_ASSIGN.
*
*      SELECT *
*       APPENDING CORRESPONDING FIELDS OF TABLE PT_MAT_REG_B
*          FROM ZTQM_MAT_REG
*            WHERE IYEAR = PS_MAT_SEL-IYEAR
*              AND WERKS = PS_MAT_SEL-WERKS
*              AND EXTWG IN R_EXTWG
*              AND RESPP IN R_RESPP.
*
*
*    WHEN PS_MAT_SEL-YES.        "/inspection material
*      SELECT *
*        INTO CORRESPONDING FIELDS OF TABLE PT_MAT_REG_B
*           FROM ZTQM_MAT_REG
*             WHERE IYEAR = PS_MAT_SEL-IYEAR
*               AND WERKS = PS_MAT_SEL-WERKS
*               AND EXTWG IN R_EXTWG
*               AND RESPP IN R_RESPP
*               AND ZYES  = 'X'
*               AND ZNO   = ' '.
*
*    WHEN PS_MAT_SEL-NO.         "/No inspection material
*      SELECT *
*        INTO CORRESPONDING FIELDS OF TABLE PT_MAT_REG_B
*           FROM ZTQM_MAT_REG
*             WHERE IYEAR = PS_MAT_SEL-IYEAR
*               AND WERKS = PS_MAT_SEL-WERKS
*               AND EXTWG IN R_EXTWG
*               AND RESPP IN R_RESPP
*               AND ZYES  = ' '
*               AND ZNO   = 'X'.
*
*    WHEN PS_MAT_SEL-YES_CNF.    "/inspection material and confirmed
*      SELECT *
*        INTO CORRESPONDING FIELDS OF TABLE PT_MAT_REG_B
*           FROM ZTQM_MAT_REG
*             WHERE IYEAR = PS_MAT_SEL-IYEAR
*               AND WERKS = PS_MAT_SEL-WERKS
*               AND EXTWG IN R_EXTWG
*               AND RESPP IN R_RESPP
*               AND ZYES  = 'X'
*               AND ZNO   = ' '
*               AND CONFM = 'X'.
*    WHEN OTHERS.
*  ENDCASE.
*

ENDFORM.                    " GET_MATERIAL_DATA_FOR_HISTORY
*&------------------------------------------------------------------*
*&      Form  CONFIRM_ALL_ITEM
*&------------------------------------------------------------------*
FORM CONFIRM_ALL_ITEM.
  DATA : LW_INDEX LIKE SY-TABIX.

  LOOP AT IT_ZSQM_MAT_CNF.
    LW_INDEX = SY-TABIX.

    IF IT_ZSQM_MAT_CNF-CONFM = C_MARK AND
       IT_ZSQM_MAT_CNF-ZYES  = C_MARK AND
       IT_ZSQM_MAT_CNF-RESPP IS INITIAL.
      CONTINUE.
    ELSEIF  (     IT_ZSQM_MAT_CNF-CONFM NE C_MARK   AND
                  IT_ZSQM_MAT_CNF-ZYES  = C_MARK    AND
              NOT IT_ZSQM_MAT_CNF-RESPP IS INITIAL      ) OR
            (     IT_ZSQM_MAT_CNF-CONFM NE C_MARK   AND
                  IT_ZSQM_MAT_CNF-ZNO   = C_MARK    AND
                  IT_ZSQM_MAT_CNF-RESPP IS INITIAL      ).
      IT_ZSQM_MAT_CNF-CONFM = C_MARK.
      MODIFY IT_ZSQM_MAT_CNF INDEX LW_INDEX.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " CONFIRM_ALL_ITEM
*&------------------------------------------------------------------*
*&      Form  UNCONFIRM_CONFIRMED_ITEM
*&------------------------------------------------------------------*
FORM UNCONFIRM_CONFIRMED_ITEM.
  CLEAR IT_ZSQM_MAT_CNF.
  MODIFY IT_ZSQM_MAT_CNF TRANSPORTING CONFM
                       WHERE MATNR NE ''.

ENDFORM.                    " UNCONFIRM_CONFIRMED_ITEM
*&-----------------------------------------------------------------*
*&      Form  DOWNLOAD_TO_EXCEL
*&-----------------------------------------------------------------*
FORM DOWNLOAD_TO_EXCEL.

  DATA : LW_FILENAME LIKE  RLGRAP-FILENAME.

*-- Make filename
  CONCATENATE ZSQM_MAT_SEL-IYEAR
             'Regular'
              SY-UNAME
              SY-DATUM
                    INTO LW_FILENAME SEPARATED BY '_'.
  CONCATENATE  LW_FILENAME '.xls' INTO LW_FILENAME.


  CALL FUNCTION 'RH_START_EXCEL_WITH_DATA'
       EXPORTING
*            CHECK_VERSION       = ' '
            DATA_NAME           = LW_FILENAME
*            DATA_PATH_FLAG      = 'W'
*            DATA_TYPE           = 'DAT'
*            DATA_BIN_FILE_SIZE  =
*            MACRO_NAME          = ' '
*            MACRO_PATH_FLAG     = ' '
*            FORCE_START         = ' '
            WAIT                = ' '
*       IMPORTING
*            WINID               =
       TABLES
            DATA_TAB            = IT_ZSQM_MAT_CNF
       EXCEPTIONS
            NO_BATCH            = 1
            EXCEL_NOT_INSTALLED = 2
            WRONG_VERSION       = 3
            INTERNAL_ERROR      = 4
            INVALID_TYPE        = 5
            CANCELLED           = 6
            DOWNLOAD_ERROR      = 7
            OTHERS              = 8.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " DOWNLOAD_TO_EXCEL
*&-----------------------------------------------------------------*
*&      Form  SEND_MAIL_SAP_OFFICE
*&-----------------------------------------------------------------*
FORM SEND_MAIL_SAP_OFFICE.
  DATA : LW_LIST_INDEX LIKE SY-LSIND.

**-- Display Table item using ALV_LIST_DISPLAY for send mail
*-   Using ALV List Display
*  PERFORM DISPLAY_ALV_LIST_DISPLAY  TABLES IT_ZSQM_MAT_CNF.

*-- Write item list using Leave to List processing

  PERFORM DISPLAY_LIST_DISPLAY  TABLES IT_ZSQM_MAT_CNF.

  LW_LIST_INDEX = SY-LSIND.

  PERFORM SEND_MAIL_TO_RECIPENT USING SY-LSIND
                                      'SEND'.
  SET USER-COMMAND 'BACK'.

ENDFORM.                    " SEND_MAIL_SAP_OFFICE
*&------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_LIST_DISPLAY
*&------------------------------------------------------------------*
FORM DISPLAY_ALV_LIST_DISPLAY TABLES  PT_MAT_CNF STRUCTURE ZSQM_MAT_CNF.

  DATA : LW_REPID           LIKE SY-REPID,
         LW_STRUCTURE_NAME  LIKE DD02L-TABNAME VALUE 'ZSQM_MAT_CNF'.

  DATA : LW_LAYOUT  TYPE SLIS_LAYOUT_ALV.
  DATA : LT_FIELDCAT TYPE    SLIS_T_FIELDCAT_ALV  WITH HEADER LINE.
  DATA : LT_EVENTS   TYPE    SLIS_T_EVENT WITH HEADER LINE.

  LW_REPID = SY-REPID.

  PERFORM SET_ALV_LAYOUT USING LW_LAYOUT.

*- generate the field catalog automatically or semi-automatically by
*- calling function module 'REUSE_ALV_FIELDCATALOG_MERGE'
  PERFORM GET_FIELDCATEGORY_MAIL  TABLES LT_FIELDCAT
                                  USING LW_STRUCTURE_NAME.

  PERFORM SET_FIELDCATEGORY_MAIL  TABLES LT_FIELDCAT.

*-- Get Event list and set events for use : List type(0~4).
  PERFORM SET_EVENTS_FOR_ALV   TABLES LT_EVENTS
                                USING '0'.  "/simple list


  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM = LW_REPID
            I_STRUCTURE_NAME   = LW_STRUCTURE_NAME
            IS_LAYOUT          = LW_LAYOUT
            IT_FIELDCAT        = LT_FIELDCAT[]
*            I_SAVE             = 'A'
            IT_EVENTS          = LT_EVENTS[]
       TABLES
            T_OUTTAB           = PT_MAT_CNF
       EXCEPTIONS
            PROGRAM_ERROR      = 1
            OTHERS             = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " DISPLAY_ALV_LIST_DISPLAY
*&--------------------------------------------------------------------*
*&      Form  GET_FIELDCATEGORY_MAIL
*&--------------------------------------------------------------------*
FORM GET_FIELDCATEGORY_MAIL TABLES PT_FIELDCAT     LIKE IT_FIELDCAT_ALV
                          USING  P_STRUCTURE_NAME  LIKE DD02L-TABNAME.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            I_STRUCTURE_NAME       = P_STRUCTURE_NAME
       CHANGING
            CT_FIELDCAT            = PT_FIELDCAT[]
       EXCEPTIONS
            INCONSISTENT_INTERFACE = 1
            PROGRAM_ERROR          = 2
            OTHERS                 = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " GET_FIELDCATEGORY_MAIL
*&------------------------------------------------------------------*
*&      Form  SET_FIELDCATEGORY_MAIL
*&------------------------------------------------------------------*
FORM SET_FIELDCATEGORY_MAIL TABLES PT_FIELDCAT  LIKE  IT_FIELDCAT_ALV.

  LOOP AT PT_FIELDCAT.

    CASE PT_FIELDCAT-FIELDNAME.

      WHEN 'MATNR' OR  'MAKTX'.
        PT_FIELDCAT-KEY = C_MARK.
      WHEN 'WERKS'.
      WHEN 'LIFNR'.
      WHEN 'EXTWG'.
      WHEN 'RESPP'.
*      WHEN 'NAME_TEXT'.
      WHEN 'ZYES'.
      WHEN 'ZNO'.
      WHEN 'CONFM'.
      WHEN OTHERS.
        PT_FIELDCAT-NO_OUT = C_MARK.
    ENDCASE.
    MODIFY PT_FIELDCAT.
  ENDLOOP.
ENDFORM.                    " SET_FIELDCATEGORY_MAIL
*&------------------------------------------------------------------*
*&      Form  SEND_MAIL_TO_RECIPENT
*&------------------------------------------------------------------*
FORM SEND_MAIL_TO_RECIPENT USING    P_LSIND   LIKE SY-LSIND
                                    VALUE(P_METHOD).

  DATA : LT_LISTOBJECT LIKE ABAPLIST OCCURS 0 WITH HEADER LINE.

*-- Call mail function
  CALL FUNCTION 'LIST_TO_OFFICE'
       EXPORTING
            LIST_INDEX         = P_LSIND
            METHOD             = P_METHOD  "/Office Methode(send,move,.)
       TABLES
            LISTOBJECT         = LT_LISTOBJECT
       EXCEPTIONS
            LIST_INDEX_INVALID = 1
            EMPTY_LIST         = 2
            OTHERS             = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " SEND_MAIL_TO_RECIPENT
*&------------------------------------------------------------------*
*&      Form  DISPLAY_LIST_DISPLAY
*&------------------------------------------------------------------*
FORM DISPLAY_LIST_DISPLAY TABLES  PT_MAT_CNF STRUCTURE ZSQM_MAT_CNF.

  SUPPRESS DIALOG.

  LEAVE TO LIST-PROCESSING.

  SET PF-STATUS 'MAIL_LIST'.
  SET TITLEBAR  'MAIL_LIST'.


  LOOP AT PT_MAT_CNF.
    AT FIRST.
      FORMAT COLOR COL_KEY.
      ULINE.
      WRITE TEXT-H01.
      ULINE.
      FORMAT RESET.
    ENDAT.

    WRITE : '|' NO-GAP, AT (18) PT_MAT_CNF-MATNR NO-GAP,
            '|' NO-GAP, AT (40) PT_MAT_CNF-MAKTX NO-GAP,
            '|' NO-GAP, AT (4)  PT_MAT_CNF-WERKS NO-GAP,
            '|' NO-GAP, AT (6)  PT_MAT_CNF-LIFNR NO-GAP,
            '|' NO-GAP, AT (10) PT_MAT_CNF-EXTWG NO-GAP,
            '|' NO-GAP, AT (8)  PT_MAT_CNF-RESPP NO-GAP,
            '|' NO-GAP, AT (3)  PT_MAT_CNF-ZYES  NO-GAP,
            '|' NO-GAP, AT (3)  PT_MAT_CNF-ZNO   NO-GAP,
            '|' NO-GAP, AT (7)  PT_MAT_CNF-CONFM NO-GAP,
            '|'.

    AT LAST.
      ULINE.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " DISPLAY_LIST_DISPLAY
*&-----------------------------------------------------------------*
*&      Form  CHECK_AND_SET_YES_OR_NO
*&-----------------------------------------------------------------*
FORM CHECK_AND_SET_YES_OR_NO.
  DATA : LW_INDEX LIKE SY-TABIX.
  DATA : LW_FIELD_STR(20),
         LW_FIELD(15).
* Find Checkbox material item index

  LW_INDEX = WA_CUR_LINE + TC_9200-TOP_LINE - 1.

  READ TABLE IT_ZSQM_MAT_CNF INDEX LW_INDEX.

  CHECK SY-SUBRC = 0.

  SPLIT WA_FLDTXT AT '-' INTO LW_FIELD_STR   LW_FIELD.

***/-- Check status

*-   Confirmed material
  IF IT_ZSQM_MAT_CNF-CONFM = C_MARK .  "/Return to previous status
    CASE   LW_FIELD.
      WHEN 'ZYES'.
        IF IT_ZSQM_MAT_CNF-ZYES  = C_MARK.
          CLEAR IT_ZSQM_MAT_CNF-ZYES.
        ELSE.
          IT_ZSQM_MAT_CNF-ZYES  = C_MARK.
        ENDIF.

      WHEN 'ZNO'.
        IF IT_ZSQM_MAT_CNF-ZNO  = C_MARK.
          CLEAR IT_ZSQM_MAT_CNF-ZNO.
        ELSE.
          IT_ZSQM_MAT_CNF-ZNO  = C_MARK.
        ENDIF.
    ENDCASE.

    MODIFY IT_ZSQM_MAT_CNF INDEX LW_INDEX.

    MESSAGE W000(ZMQM)
         WITH 'It was already confirmed!'(W11).

*-  Unconfirmed material
  ELSEIF IT_ZSQM_MAT_CNF-CONFM = ' '. "/Set & clear Yes/No field value
    CASE   LW_FIELD.
      WHEN 'ZYES'.
        IF IT_ZSQM_MAT_CNF-ZYES  = C_MARK.
          CLEAR IT_ZSQM_MAT_CNF-ZNO.
        ELSE.
          IT_ZSQM_MAT_CNF-ZNO  = C_MARK.
        ENDIF.

      WHEN 'ZNO'.
        IF IT_ZSQM_MAT_CNF-ZNO  = C_MARK.
          CLEAR IT_ZSQM_MAT_CNF-ZYES.
        ELSE.
          IT_ZSQM_MAT_CNF-ZYES  = C_MARK.
        ENDIF.

    ENDCASE.

    MODIFY IT_ZSQM_MAT_CNF INDEX LW_INDEX.

  ENDIF.

ENDFORM.                    " CHECK_AND_SET_YES_OR_NO
*&------------------------------------------------------------------*
*&      Form  GET_MAT_DATA_FOR_REG_V2
*&------------------------------------------------------------------*
FORM GET_MAT_DATA_FOR_REG_V2  TABLES PT_MAT_CNF STRUCTURE ZSQM_MAT_CNF
                              USING  PS_MAT_SEL LIKE ZSQM_MAT_SEL.

*-/ Internal table for 'HALB' material
*   - 'HALB' materials may have vendor code and it can be deleted, so
*      'EINA' table can't be joined with mara by inner join.
  DATA : BEGIN OF LT_MAT_CNF OCCURS 0.
  INCLUDE  STRUCTURE ZSQM_MAT_CNF.
  DATA :   LOEKZ TYPE ILOEA,        "/Deletion flag for Purchasing info.
         END OF LT_MAT_CNF.


*-- Get data by user selected task
  CASE C_MARK.

*-  Assign Resp. Person
    WHEN PS_MAT_SEL-P_AS_REP.

**-- 'ROH' Material -
*       - join Purchasing Info Record Table(EINA)
      SELECT A~MATNR C~MAKTX A~MTART B~WERKS D~LIFNR A~EXTWG
        INTO CORRESPONDING FIELDS OF TABLE PT_MAT_CNF
          FROM ( (  MARA AS A INNER JOIN MARC AS B
            ON  A~MATNR = B~MATNR ) INNER JOIN MAKT AS C
            ON  A~MATNR = C~MATNR ) INNER JOIN EINA AS D
            ON  A~MATNR = D~MATNR
            WHERE A~EXTWG  NE ' '    "/Not assigned : Ext. mat. group
              AND B~LVORM = ' '      "/Not Deleted at Plant Level
              AND A~MTART = ZQMT1_MTART_ROH
              AND A~MSTAE = ZQMT1_MSTAE_USING  "/Status:'Using Material'
              AND B~QMATV  = C_MARK
              AND B~WERKS  = PS_MAT_SEL-WERKS
              AND C~SPRAS = SY-LANGU
              AND D~LOEKZ = ' '
              AND ( A~EXTWG IN R_EXTWG AND A~EXTWG NE ' ' )
              AND NOT EXISTS ( SELECT *
                                FROM ZTQM_MAT_REG
                                  WHERE IYEAR = PS_MAT_SEL-IYEAR
                                    AND MATNR = A~MATNR
                                    AND LIFNR = D~LIFNR
                                    AND WERKS = PS_MAT_SEL-WERKS )
              AND NOT EXISTS
                      ( SELECT  *
                          FROM ZTQM_QNS_ITEM
                            WHERE IYEAR = PS_MAT_SEL-IYEAR
                              AND MATNR = A~MATNR
                              AND LIFNR = D~LIFNR
                              AND ART   = ZQMT1_INSP_TYPE_REGULAR ).

*-- 'HALB' Material
*<<< Modified by sllee - 06/25/2004 : req. by Mr. Kim : Start
* - HALB Material can not have Purchasing info Record
      SELECT A~MATNR C~MAKTX A~MTART B~WERKS  A~EXTWG
             D~LIFNR   D~LOEKZ
        INTO CORRESPONDING FIELDS OF TABLE LT_MAT_CNF
          FROM ( ( MARA AS A INNER JOIN MARC AS B
            ON  A~MATNR = B~MATNR ) INNER JOIN MAKT AS C
            ON  A~MATNR = C~MATNR ) LEFT OUTER JOIN EINA AS D
            ON  A~MATNR = D~MATNR
            WHERE A~EXTWG  NE ' '    "/Not assigned : Ext. mat. group
              AND B~LVORM = ' '      "/Not Deleted at Plant Level
              AND A~MTART = ZQMT1_MTART_HALB
              AND B~QMATV  = C_MARK
              AND B~WERKS  = PS_MAT_SEL-WERKS
              AND C~SPRAS = SY-LANGU
*              AND D~LOEKZ = ' '
              AND ( A~EXTWG IN R_EXTWG AND A~EXTWG NE ' ' )
              AND NOT EXISTS ( SELECT *
                                FROM ZTQM_MAT_REG
                                  WHERE IYEAR = PS_MAT_SEL-IYEAR
                                    AND MATNR = A~MATNR
                                    AND LIFNR = D~LIFNR
                                    AND WERKS = PS_MAT_SEL-WERKS )
              AND NOT EXISTS
                      ( SELECT  *
                          FROM ZTQM_QNS_ITEM
                            WHERE IYEAR = PS_MAT_SEL-IYEAR
                              AND MATNR = A~MATNR
                              AND LIFNR = D~LIFNR
                              AND ART   = ZQMT1_INSP_TYPE_REGULAR ).

*     - filtering :  'LOEKZ' = ' ' - not deleted HALB vendor.
      IF SY-SUBRC = 0.
        LOOP AT LT_MAT_CNF WHERE LOEKZ = ' '.
          CLEAR PT_MAT_CNF.
          MOVE-CORRESPONDING LT_MAT_CNF TO PT_MAT_CNF.
          APPEND PT_MAT_CNF.
        ENDLOOP.
      ENDIF.


*<<< Modified by sllee - 06/25/2004 : req. by Mr. Kim : End

*-    Get material exist in ZTQM_MAT_REG
      SELECT A~MATNR B~MAKTX A~WERKS C~LIFNR H~EXTWG
             A~RESPP A~ZYES A~ZNO A~CONFM
        APPENDING CORRESPONDING FIELDS OF TABLE PT_MAT_CNF
          FROM ( ( ZTQM_MAT_REG AS A INNER JOIN MARA AS H
             ON   A~MATNR = H~MATNR ) INNER JOIN MAKT AS B
             ON   A~MATNR = B~MATNR ) INNER JOIN EINA AS C
             ON   A~MATNR = C~MATNR
              AND A~LIFNR = C~LIFNR
            WHERE A~IYEAR = PS_MAT_SEL-IYEAR
              AND A~WERKS = PS_MAT_SEL-WERKS
              AND ( H~EXTWG IN R_EXTWG AND H~EXTWG NE ' ' )
              AND A~RESPP = ' '
              AND A~CONFM = ' '
              AND B~SPRAS = SY-LANGU
              AND C~LOEKZ = ' '
              AND NOT EXISTS
                      ( SELECT  *
                          FROM ZTQM_QNS_ITEM
                            WHERE IYEAR = A~IYEAR
                              AND MATNR = A~MATNR
                              AND LIFNR = A~LIFNR
                              AND ART   = ZQMT1_INSP_TYPE_REGULAR ).

*-   Select Insp. Mat. :only assigned resp. person, Not checked Yes/No
    WHEN PS_MAT_SEL-P_SEL_MAT.

**--Change : HALB Material can not have Purchasing info Record - Start
*-                                                       04/08/2004

      SELECT A~MATNR A~WERKS D~MTART D~EXTWG
             A~RESPP A~ZYES A~ZNO A~CONFM
             A~LIFNR C~MAKTX
        APPENDING CORRESPONDING FIELDS OF TABLE PT_MAT_CNF
           FROM ( ZTQM_MAT_REG AS A  INNER JOIN MAKT AS C
              ON   A~MATNR = C~MATNR ) INNER JOIN MARA AS D
              ON   A~MATNR = D~MATNR
             WHERE A~IYEAR = PS_MAT_SEL-IYEAR
               AND A~WERKS = PS_MAT_SEL-WERKS
               AND D~EXTWG IN R_EXTWG
               AND A~ZYES  = ' '
               AND A~ZNO   = ' '
               AND A~CONFM = ' '
               AND ( A~RESPP IN R_RESPP AND A~RESPP NE ' ' )
               AND C~SPRAS = SY-LANGU.

**--Change : HALB Material can not have Purchasing info Record - End

*-   Confirm : need to confirm item. which has resp. person and
*              check Yes/No.
    WHEN PS_MAT_SEL-P_CONFIRM.

      SELECT A~MATNR A~WERKS D~MTART D~EXTWG
             A~RESPP A~ZYES A~ZNO A~CONFM
             A~LIFNR C~MAKTX
        INTO CORRESPONDING FIELDS OF TABLE PT_MAT_CNF
           FROM ( ZTQM_MAT_REG AS A  INNER JOIN MAKT AS C
              ON   A~MATNR = C~MATNR ) INNER JOIN MARA AS D
              ON   A~MATNR = D~MATNR
             WHERE A~IYEAR = PS_MAT_SEL-IYEAR
               AND A~WERKS = PS_MAT_SEL-WERKS
               AND D~EXTWG IN R_EXTWG
               AND A~RESPP IN R_RESPP
               AND ( A~ZYES  = 'X' OR  A~ZNO   = 'X' )
               AND A~CONFM = ' '
               AND C~SPRAS = SY-LANGU.

*-  Confrim & Yes : for Download by resp. person.
    WHEN PS_MAT_SEL-P_CONFYES.

      SELECT A~MATNR A~WERKS H~EXTWG  H~MTART
             A~RESPP A~ZYES A~ZNO A~CONFM
             A~LIFNR C~MAKTX
        INTO CORRESPONDING FIELDS OF TABLE PT_MAT_CNF
           FROM (  ZTQM_MAT_REG AS A   INNER JOIN MARA AS H
              ON   A~MATNR = H~MATNR ) INNER JOIN MAKT AS C
              ON   A~MATNR = C~MATNR
             WHERE A~IYEAR = PS_MAT_SEL-IYEAR
               AND A~WERKS = PS_MAT_SEL-WERKS
               AND H~EXTWG IN R_EXTWG
               AND ( A~RESPP IN R_RESPP AND A~RESPP NE ' ' )
               AND A~ZYES  = 'X'
               AND A~ZNO   = ' '
               AND A~CONFM = 'X'
               AND C~SPRAS = SY-LANGU
               AND NOT EXISTS
                      ( SELECT  *
                          FROM ZTQM_QNS_ITEM
                            WHERE IYEAR = PS_MAT_SEL-IYEAR
                              AND MATNR = A~MATNR
                              AND LIFNR = A~LIFNR
                              AND ART   = ZQMT1_INSP_TYPE_REGULAR ).

    WHEN OTHERS.
  ENDCASE.


ENDFORM.                    " GET_MAT_DATA_FOR_REG_V2
*&------------------------------------------------------------------*
*&      Form  GET_MAT_DATA_FOR_HISTORY_v2
*&------------------------------------------------------------------*
FORM GET_MAT_DATA_FOR_HISTORY_V2
                            TABLES PT_MAT_REG_B STRUCTURE ZTQM_MAT_REG
                            USING  PS_MAT_SEL LIKE ZSQM_MAT_SEL.

  REFRESH PT_MAT_REG_B.

*-- Get data by user selected task
  CASE C_MARK.

*-  Assign Resp. Person
    WHEN PS_MAT_SEL-P_AS_REP.

*-    Get material exist in ZTQM_MAT_REG
      SELECT A~MATNR A~IYEAR A~MATNR A~LIFNR A~WERKS A~RESPP
             A~ZYES  A~ZNO   A~CONFM A~CNFMD
             A~ERDAT A~ERZET A~ERNAM A~AEDAT A~AEZET A~AENAM
             H~EXTWG
        INTO CORRESPONDING FIELDS OF TABLE PT_MAT_REG_B
          FROM  ZTQM_MAT_REG AS A INNER JOIN MARA AS H
            ON  A~MATNR = H~MATNR
            WHERE A~IYEAR = PS_MAT_SEL-IYEAR
              AND A~WERKS = PS_MAT_SEL-WERKS
              AND ( H~EXTWG IN R_EXTWG AND H~EXTWG NE ' ' )
              AND A~RESPP = ' '
              AND NOT EXISTS
                      ( SELECT  *
                          FROM ZTQM_QNS_ITEM
                            WHERE IYEAR = A~IYEAR
                              AND MATNR = A~MATNR
                              AND LIFNR = A~LIFNR
                              AND ART   = ZQMT1_INSP_TYPE_REGULAR ).



*-   Select Insp. Mat. :only assigned resp. person, Not checked Yes/No
    WHEN PS_MAT_SEL-P_SEL_MAT.

      SELECT A~MATNR A~IYEAR A~MATNR A~LIFNR A~WERKS A~RESPP
             A~ZYES  A~ZNO   A~CONFM A~CNFMD
             A~ERDAT A~ERZET A~ERNAM A~AEDAT A~AEZET A~AENAM
             H~EXTWG
        INTO CORRESPONDING FIELDS OF TABLE PT_MAT_REG_B
           FROM  ZTQM_MAT_REG AS A INNER JOIN MARA AS H
             ON  A~MATNR = H~MATNR
            WHERE A~IYEAR = PS_MAT_SEL-IYEAR
              AND A~WERKS = PS_MAT_SEL-WERKS
              AND H~EXTWG IN R_EXTWG
               AND A~ZYES  = ' '
               AND A~ZNO   = ' '
               AND A~CONFM = ' '
               AND ( A~RESPP IN R_RESPP AND A~RESPP NE ' ' ).

*-   Confirm : need to confirm item. which has resp. person and
*              check Yes/No.
    WHEN PS_MAT_SEL-P_CONFIRM.

      SELECT A~MATNR A~IYEAR A~MATNR A~LIFNR A~WERKS A~RESPP
             A~ZYES  A~ZNO   A~CONFM A~CNFMD
             A~ERDAT A~ERZET A~ERNAM A~AEDAT A~AEZET A~AENAM
             H~EXTWG
        INTO CORRESPONDING FIELDS OF TABLE PT_MAT_REG_B
           FROM ZTQM_MAT_REG AS A INNER JOIN MARA AS H
             ON  A~MATNR = H~MATNR
            WHERE A~IYEAR = PS_MAT_SEL-IYEAR
              AND A~WERKS = PS_MAT_SEL-WERKS
              AND H~EXTWG IN R_EXTWG
               AND ( A~RESPP IN R_RESPP AND A~RESPP NE ' ' )
               AND ( A~ZYES  = 'X' OR  A~ZNO   = 'X' )
               AND A~CONFM = ' '.

*-  Confrim & Yes : for Download by resp. person.
    WHEN PS_MAT_SEL-P_CONFYES.

      SELECT A~MATNR A~IYEAR A~LIFNR A~WERKS A~RESPP
             A~ZYES  A~ZNO   A~CONFM A~CNFMD
             A~ERDAT A~ERZET A~ERNAM A~AEDAT A~AEZET A~AENAM
             H~EXTWG
        INTO CORRESPONDING FIELDS OF TABLE PT_MAT_REG_B
           FROM  ZTQM_MAT_REG AS A INNER JOIN MARA AS H
             ON  A~MATNR = H~MATNR
            WHERE A~IYEAR = PS_MAT_SEL-IYEAR
              AND A~WERKS = PS_MAT_SEL-WERKS
              AND H~EXTWG IN R_EXTWG
               AND ( A~RESPP IN R_RESPP AND A~RESPP NE ' ' )
               AND A~ZYES  = 'X'
               AND A~ZNO   = ' '
               AND A~CONFM = 'X'
               AND NOT EXISTS
                      ( SELECT  *
                          FROM ZTQM_QNS_ITEM
                            WHERE IYEAR = A~IYEAR
                              AND MATNR = A~MATNR
                              AND LIFNR = A~LIFNR
                              AND ART   = ZQMT1_INSP_TYPE_REGULAR ).


    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " GET_MAT_DATA_FOR_HISTORY_v2
*&------------------------------------------------------------------*
*&      Form  GET_MAT_DATA_FOR_REG_V3
*&------------------------------------------------------------------*
FORM GET_MAT_DATA_FOR_REG_V3 TABLES PT_MAT_CNF STRUCTURE ZSQM_MAT_CNF
                              USING  PS_MAT_SEL LIKE ZSQM_MAT_SEL.

*-/ Internal table for 'HALB' material
*   - 'HALB' materials may have vendor code and it can be deleted, so
*      'EINA' table can't be joined with mara by inner join.
  DATA : BEGIN OF LT_MAT_CNF OCCURS 0.
  INCLUDE  STRUCTURE ZSQM_MAT_CNF.
  DATA :   LOEKZ TYPE ILOEA,        "/Deletion flag for Purchasing info.
         END OF LT_MAT_CNF.

*-- Get data
  CASE C_MARK.

*-  All
    WHEN PS_MAT_SEL-ALL.

*-- ROH Material
      SELECT A~MATNR C~MAKTX A~MTART B~WERKS D~LIFNR A~EXTWG
        INTO CORRESPONDING FIELDS OF TABLE PT_MAT_CNF
          FROM ( (  MARA AS A INNER JOIN MARC AS B
            ON  A~MATNR = B~MATNR ) INNER JOIN MAKT AS C
            ON  A~MATNR = C~MATNR ) INNER JOIN EINA AS D
            ON  A~MATNR = D~MATNR
            WHERE A~EXTWG  NE ' '    "/Not assigned : Ext. mat. group
              AND B~LVORM = ' '      "/Not Deleted at Plant Level
              AND A~MTART = ZQMT1_MTART_ROH
              AND A~MSTAE = ZQMT1_MSTAE_USING  "/Status:'Using Material'
              AND B~QMATV = C_MARK
              AND B~WERKS = PS_MAT_SEL-WERKS
              AND C~SPRAS = SY-LANGU
              AND D~LOEKZ = ' '
              AND ( A~EXTWG IN R_EXTWG AND A~EXTWG NE ' ' )
              AND NOT EXISTS ( SELECT *
                                FROM ZTQM_MAT_REG
                                  WHERE IYEAR = PS_MAT_SEL-IYEAR
                                    AND MATNR = A~MATNR
                                    AND LIFNR = D~LIFNR
                                    AND WERKS = PS_MAT_SEL-WERKS ).

*- HALB Material

*<<< Modified by sllee - 06/25/2004 : req. by Mr. Kim : Start
* - HALB Material can not have Purchasing info Record

      SELECT A~MATNR C~MAKTX A~MTART B~WERKS  A~EXTWG
             D~LIFNR D~LOEKZ
        INTO CORRESPONDING FIELDS OF TABLE LT_MAT_CNF
          FROM ( (  MARA AS A INNER JOIN MARC AS B
            ON  A~MATNR = B~MATNR ) INNER JOIN MAKT AS C
            ON  A~MATNR = C~MATNR ) LEFT OUTER JOIN EINA AS D
            ON  A~MATNR = D~MATNR
            WHERE A~EXTWG  NE ' '    "/Not assigned : Ext. mat. group
              AND B~LVORM = ' '      "/Not Deleted at Plant Level
              AND A~MTART = ZQMT1_MTART_HALB
              AND B~QMATV  = C_MARK
              AND B~WERKS  = PS_MAT_SEL-WERKS
              AND C~SPRAS = SY-LANGU
              AND ( A~EXTWG IN R_EXTWG AND A~EXTWG NE ' ' )
              AND NOT EXISTS ( SELECT *
                                FROM ZTQM_MAT_REG
                                  WHERE IYEAR = PS_MAT_SEL-IYEAR
                                    AND MATNR = A~MATNR
                                    AND LIFNR = D~MATNR
                                    AND WERKS = PS_MAT_SEL-WERKS ).

*     - filtering :  'LOEKZ' = ' ' - not deleted HALB mat. vendor item.
      IF SY-SUBRC = 0.
        LOOP AT LT_MAT_CNF WHERE LOEKZ = ' '.
          CLEAR PT_MAT_CNF.
          MOVE-CORRESPONDING LT_MAT_CNF TO PT_MAT_CNF.
          APPEND PT_MAT_CNF.
        ENDLOOP.
      ENDIF.

*<<< Modified by sllee - 06/25/2004 : req. by Mr. Kim : End

*-    Get material exist in ZTQM_MAT_REG
      SELECT A~MATNR B~MAKTX A~WERKS A~LIFNR H~EXTWG
             A~RESPP A~ZYES A~ZNO A~CONFM
        APPENDING CORRESPONDING FIELDS OF TABLE PT_MAT_CNF
          FROM (  ZTQM_MAT_REG AS A INNER JOIN MARA AS H
             ON   A~MATNR = H~MATNR ) INNER JOIN MAKT AS B
             ON   A~MATNR = B~MATNR
            WHERE A~IYEAR = PS_MAT_SEL-IYEAR
              AND A~WERKS = PS_MAT_SEL-WERKS
              AND H~EXTWG IN R_EXTWG
              AND A~RESPP IN R_RESPP
              AND B~SPRAS = SY-LANGU.

*-   Select Insp. Mat. confirmed
    WHEN PS_MAT_SEL-CONFIRM.

      SELECT A~MATNR A~WERKS H~MTART H~EXTWG
             A~RESPP A~ZYES A~ZNO A~CONFM
             A~LIFNR C~MAKTX
        APPENDING CORRESPONDING FIELDS OF TABLE PT_MAT_CNF
           FROM (  ZTQM_MAT_REG AS A INNER JOIN MARA AS H
              ON   A~MATNR = H~MATNR ) INNER JOIN MAKT AS C
              ON   A~MATNR = C~MATNR
             WHERE A~IYEAR = PS_MAT_SEL-IYEAR
               AND A~WERKS = PS_MAT_SEL-WERKS
               AND H~EXTWG IN R_EXTWG
               AND A~CONFM = C_MARK
*               AND A~ZYES  = ' '
*               AND A~ZNO   = ' '
               AND A~RESPP IN R_RESPP
               AND C~SPRAS = SY-LANGU.

*-    Need Inspection
    WHEN PS_MAT_SEL-NEED_INSP.

      SELECT A~MATNR A~WERKS H~MTART H~EXTWG
             A~RESPP A~ZYES A~ZNO A~CONFM
             A~LIFNR C~MAKTX
        INTO CORRESPONDING FIELDS OF TABLE PT_MAT_CNF
           FROM (  ZTQM_MAT_REG AS A INNER JOIN MARA AS H
              ON   A~MATNR = H~MATNR ) INNER JOIN MAKT AS C
              ON   A~MATNR = C~MATNR
             WHERE A~IYEAR = PS_MAT_SEL-IYEAR
               AND A~WERKS = PS_MAT_SEL-WERKS
               AND H~EXTWG IN R_EXTWG
               AND A~RESPP IN R_RESPP
               AND A~ZYES  = 'X'
               AND C~SPRAS = SY-LANGU.

*-  Don't need inspection
    WHEN PS_MAT_SEL-DONT_INSP.

      SELECT A~MATNR A~WERKS H~EXTWG  H~MTART
             A~RESPP A~ZYES A~ZNO A~CONFM
             A~LIFNR C~MAKTX
        INTO CORRESPONDING FIELDS OF TABLE PT_MAT_CNF
           FROM (  ZTQM_MAT_REG AS A INNER JOIN MARA AS H
              ON   A~MATNR = H~MATNR ) INNER JOIN MAKT AS C
              ON   A~MATNR = C~MATNR
             WHERE A~IYEAR = PS_MAT_SEL-IYEAR
               AND A~WERKS = PS_MAT_SEL-WERKS
               AND H~EXTWG IN R_EXTWG
               AND A~RESPP IN R_RESPP
*               AND A~ZYES  = 'X'
               AND A~ZNO   = 'X'
*               AND A~CONFM = 'X'
               AND C~SPRAS = SY-LANGU.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " GET_MAT_DATA_FOR_REG_V3
*&-----------------------------------------------------------------*
*&      Form  GET_MAT_DATA_FOR_HISTORY_V3
*&------------------------------------------------------------------*
FORM GET_MAT_DATA_FOR_HISTORY_V3
                            TABLES PT_MAT_REG_B STRUCTURE ZTQM_MAT_REG
                            USING  PS_MAT_SEL LIKE ZSQM_MAT_SEL.

  REFRESH PT_MAT_REG_B.

*-- Get data by user selected task
  CASE C_MARK.


    WHEN PS_MAT_SEL-ALL.

*-    Get material exist in ZTQM_MAT_REG
      SELECT A~MATNR A~IYEAR A~MATNR A~LIFNR A~WERKS A~RESPP
             A~ZYES  A~ZNO   A~CONFM A~CNFMD
             A~ERDAT A~ERZET A~ERNAM A~AEDAT A~AEZET A~AENAM
             H~EXTWG
        INTO CORRESPONDING FIELDS OF TABLE PT_MAT_REG_B
          FROM  ZTQM_MAT_REG AS A INNER JOIN MARA AS H
            ON  A~MATNR = H~MATNR
            WHERE A~IYEAR = PS_MAT_SEL-IYEAR
              AND A~WERKS = PS_MAT_SEL-WERKS
              AND H~EXTWG IN R_EXTWG
              AND A~RESPP IN R_RESPP.


    WHEN PS_MAT_SEL-CONFIRM.

      SELECT A~MATNR A~IYEAR A~MATNR A~LIFNR A~WERKS A~RESPP
             A~ZYES  A~ZNO   A~CONFM A~CNFMD
             A~ERDAT A~ERZET A~ERNAM A~AEDAT A~AEZET A~AENAM
             H~EXTWG
        INTO CORRESPONDING FIELDS OF TABLE PT_MAT_REG_B
          FROM  ZTQM_MAT_REG AS A INNER JOIN MARA AS H
            ON  A~MATNR = H~MATNR
            WHERE A~IYEAR = PS_MAT_SEL-IYEAR
              AND A~WERKS = PS_MAT_SEL-WERKS
              AND H~EXTWG IN R_EXTWG
*               AND A~ZYES  = ' '
*               AND A~ZNO   = ' '
               AND A~CONFM = 'X'
               AND A~RESPP IN R_RESPP.

    WHEN PS_MAT_SEL-NEED_INSP.

      SELECT A~MATNR A~IYEAR A~MATNR A~LIFNR A~WERKS A~RESPP
             A~ZYES  A~ZNO   A~CONFM A~CNFMD
             A~ERDAT A~ERZET A~ERNAM A~AEDAT A~AEZET A~AENAM
             H~EXTWG
        INTO CORRESPONDING FIELDS OF TABLE PT_MAT_REG_B
          FROM  ZTQM_MAT_REG AS A INNER JOIN MARA AS H
            ON  A~MATNR = H~MATNR
            WHERE A~IYEAR = PS_MAT_SEL-IYEAR
              AND A~WERKS = PS_MAT_SEL-WERKS
              AND H~EXTWG IN R_EXTWG
               AND A~RESPP IN R_RESPP
               AND A~ZYES  = 'X'.
*               AND A~ZNO   = ' '.


    WHEN PS_MAT_SEL-DONT_INSP.

      SELECT A~MATNR A~IYEAR A~MATNR A~LIFNR A~WERKS A~RESPP
             A~ZYES  A~ZNO   A~CONFM A~CNFMD
             A~ERDAT A~ERZET A~ERNAM A~AEDAT A~AEZET A~AENAM
             H~EXTWG
        INTO CORRESPONDING FIELDS OF TABLE PT_MAT_REG_B
          FROM  ZTQM_MAT_REG AS A INNER JOIN MARA AS H
            ON  A~MATNR = H~MATNR
            WHERE A~IYEAR = PS_MAT_SEL-IYEAR
              AND A~WERKS = PS_MAT_SEL-WERKS
              AND H~EXTWG IN R_EXTWG
               AND A~RESPP IN R_RESPP
*               AND A~ZYES  = ' '
               AND A~ZNO   = 'X'.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " GET_MAT_DATA_FOR_HISTORY_V3
*&------------------------------------------------------------------*
*&      Form  SET_EXCLUDING_FUNC
*&------------------------------------------------------------------*
FORM SET_EXCLUDING_FUNC.

  REFRESH IT_EX_FUNC.

  IF ZSQM_MAT_SEL-TASK = C_MARK.

    CASE C_MARK.
      WHEN ZSQM_MAT_SEL-P_AS_REP.
        EXCLUDE_FUNC : 'CONFIRM_AL',
                       'UNCONF_AL',
                       'DOWNLOAD'.
      WHEN ZSQM_MAT_SEL-P_SEL_MAT.
        EXCLUDE_FUNC : 'CONFIRM_AL',
                       'UNCONF_AL',
                       'DOWNLOAD'.

      WHEN ZSQM_MAT_SEL-P_CONFIRM.

      WHEN ZSQM_MAT_SEL-P_CONFYES .
        EXCLUDE_FUNC : 'CONFIRM_AL',
                       'UNCONF_AL'.
    ENDCASE.

  ELSE.

    CASE C_MARK.
      WHEN ZSQM_MAT_SEL-ALL.
* changes made by 100565 07/16/04 req by Mr Moon start
       EXCLUDE_FUNC : 'CONFIRM_AL',
                       'UNCONF_AL',
                       'DOWNLOAD',
                       'DOWN_MAT'.
* changes made by 100565 07/16/04 req by Mr Moon end

      WHEN ZSQM_MAT_SEL-CONFIRM.
* changes made by 100565 07/16/04 req by Mr Moon start
       EXCLUDE_FUNC : 'CONFIRM_AL',
                       'UNCONF_AL',
                       'DOWNLOAD',
                       'DOWN_MAT'.
* changes made by 100565 07/16/04 req by Mr Moon end

      WHEN ZSQM_MAT_SEL-NEED_INSP.
* changes made by 100565 07/16/04 req by Mr Moon start
       EXCLUDE_FUNC : 'CONFIRM_AL',
                       'UNCONF_AL',
                       'DOWNLOAD',
                       'DOWN_MAT'.
* changes made by 100565 07/16/04 req by Mr Moon end

      WHEN ZSQM_MAT_SEL-DONT_INSP.
* changes made by 100565 07/16/04 req by Mr Moon start
       EXCLUDE_FUNC : 'CONFIRM_AL',
                       'UNCONF_AL',
                       'DOWNLOAD',
                       'DOWN_MAT'.
* changes made by 100565 07/16/04 req by Mr Moon end

    ENDCASE.

  ENDIF.


ENDFORM.                    " SET_EXCLUDING_FUNC
*&------------------------------------------------------------------*
*&      Form  DOWNLOAD_TO_EXCEL_NEW
*&------------------------------------------------------------------*
FORM DOWNLOAD_TO_EXCEL_NEW.
  DATA : LW_FILENAME LIKE  RLGRAP-FILENAME.


  DATA : LT_CNFRM_ITEM LIKE ZSQM_QNS_EXD_RE OCCURS 0 WITH HEADER LINE.

  REFRESH LT_CNFRM_ITEM.

*--  Fill Header line (3 Lines for Excel Header)-05/04/2004 : sllee
  CLEAR LT_CNFRM_ITEM.
*    "/Fill Insp. type
  MOVE : ZQMT1_INSP_TYPE_REGULAR TO LT_CNFRM_ITEM-MATNR.
  APPEND LT_CNFRM_ITEM.
  CLEAR LT_CNFRM_ITEM.
  APPEND LT_CNFRM_ITEM.
  APPEND LT_CNFRM_ITEM.

*-- Filtering confirmed item from IT_ZSQM_MAT_E_CNF.

  LOOP AT IT_ZSQM_MAT_CNF  WHERE ZYES  = C_MARK
                             AND CONFM = C_MARK.
    CLEAR LT_CNFRM_ITEM.
    MOVE-CORRESPONDING IT_ZSQM_MAT_CNF TO LT_CNFRM_ITEM.
    APPEND LT_CNFRM_ITEM.
  ENDLOOP.

  CHECK SY-SUBRC = 0 AND NOT LT_CNFRM_ITEM[] IS INITIAL.

*-- Make filename
  CONCATENATE ZSQM_MAT_SEL-IYEAR
             'Regular'
              SY-UNAME
              SY-DATUM
                    INTO LW_FILENAME SEPARATED BY '_'.
  CONCATENATE  LW_FILENAME '.xls' INTO LW_FILENAME.


  CALL FUNCTION 'RH_START_EXCEL_WITH_DATA'
       EXPORTING
*            CHECK_VERSION       = ' '
            DATA_NAME           = LW_FILENAME
*            DATA_PATH_FLAG      = 'W'
            DATA_TYPE           = 'DAT'
*            DATA_BIN_FILE_SIZE  =
*            MACRO_NAME          = ' '
*            MACRO_PATH_FLAG     = ' '
*            FORCE_START         = ' '
            WAIT                = ' '
*       IMPORTING
*            WINID               =
       TABLES
            DATA_TAB            = LT_CNFRM_ITEM
       EXCEPTIONS
            NO_BATCH            = 1
            EXCEL_NOT_INSTALLED = 2
            WRONG_VERSION       = 3
            INTERNAL_ERROR      = 4
            INVALID_TYPE        = 5
            CANCELLED           = 6
            DOWNLOAD_ERROR      = 7
            OTHERS              = 8.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " DOWNLOAD_TO_EXCEL_NEW

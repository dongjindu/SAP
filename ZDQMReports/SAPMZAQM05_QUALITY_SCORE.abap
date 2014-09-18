************************************************************************
* Program Name      : SAPMZAQM05_QUALITY_SCORE
* Author            : SeungLyong, Lee
* Creation Date     : 2003.09.26.
* Specifications By : SeungLyong, Lee
* Development Request No :
* Addl Documentation:
* Description       : Quality Score-Input Raw Data
*             Input the Quality evaluation's criterion data
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT  SAPMZAQM05_QUALITY_SCORE      .

*&&& Data Declaration.  &&&*
TYPE-POOLS VRM.     "//Value Request Manager: Types & Constants
TYPE-POOLS CXTAB .  "//Table_control Object type pool
TABLES : FELD.      "//Screen Object Structure

*-- Include Program ( Include Constants or etc)
*INCLUDE :

*-- SAP Scripts Object Interface
*TABLES : THEAD. "/SAPscript: Text Header

*//Tables;(TABLES : Table_Name  "//Table Description)
TABLES : ZTQM_Q_SCORE.  "/Quality Score Table - AQM05

*//Structures Declaration(TABLES : Structure Name."/Description)
TABLES : ZSQM_QUAL_SCORE.   "/Quality Score Structure T/C

TABLES : ZSCA_TIME_STAMP.   "/Time Stamp Structre.


*//InfoType;()
*//Cluster or Import Parameter;(Parameter Name)

*//Controls(for only Screen Control Element);(TC_ , or TS_)
*-- TABLE CONTROL
CONTROLS: TC_9000  TYPE TABLEVIEW USING SCREEN 9000.

*//Type (Table Structure);(TY_ )- Table or Structure

TYPES: BEGIN OF TY_FCODE,
        FCODE LIKE RSMPE-FUNC,
      END OF TY_FCODE.

DATA: IT_EX_FUNC TYPE STANDARD TABLE OF TY_FCODE WITH
                       NON-UNIQUE DEFAULT KEY INITIAL SIZE 5,
      WA_EX_FUNC TYPE TY_FCODE.


*//Constants ;(C_) ==> True:'X' or '1' False:Space or '0'
CONSTANTS : C_MARK   VALUE 'X'.

*-- Screen Control Mode
CONSTANTS : C_CREATE(7)  TYPE C VALUE 'CREATE',
            C_CHANGE(7)  TYPE C VALUE 'CHANGE',
            C_DISPLAY(7) TYPE C VALUE 'DISPLAY'.

*-- Process Status
CONSTANTS : " C_UPLOADED(8)  TYPE C VALUE 'UPLOADED',
            C_SAVED(8)     TYPE C VALUE 'SAVED'.


**//-- Global : used Variable just in this Program
*-- Function Control
DATA : OK_CODE LIKE SY-UCOMM.
DATA : WA_MODE(7) TYPE C,
       WA_STATUS(8) TYPE C.

*--
DATA : WA_RETURN     LIKE	BAPIRETURN1.   "Return Values

*-- User Confirm for pop-up Message
DATA : WA_ANSWER TYPE C.


*-- Table Control Field Variables
DATA : WA_FLDTXT    LIKE FELD-NAME,  "Field Name Variable
       WA_CUR_LINE  LIKE FELD-LINE.  "Field Line Variable

*-- Table Control Variables
DATA : WA_CON_LINES LIKE SY-LOOPC.  " LINES OF TABLECONTROL
DATA : WA_COUNT     TYPE   I.       " COUNT Variables
DATA : WA_LINES     LIKE SY-TABIX.
DATA : WA_TABIX     LIKE SY-TABIX.
DATA : WA_SEL_LINE  LIKE SY-TABIX.  "Select Line of T/C
DATA : WA_TCNAME    LIKE FELD-NAME. "table control Field Name

*-- Variables for default values of Select-options
DATA : WA_FROM_DATE TYPE DATUM,
       WA_TO_DATE   TYPE DATUM.

*-- Work area Variables in Program.
DATA : BEGIN OF WA_ZSCA_TIME_STAMP.
        INCLUDE STRUCTURE ZSCA_TIME_STAMP.
DATA :  MANDT TYPE MANDT,
       END OF WA_ZSCA_TIME_STAMP.

DATA : BEGIN OF WA_CHANGE_CHECK,
        LINESTOP   TYPE   ZLINE_STOP,
        MEINS      TYPE   MEINS,
        QNT_CAMP   TYPE   ZQUANTITY_CAMP,
        QNT_SALV   TYPE   ZQUANTITY_SALV,
        QNT_REPR   TYPE   ZQUANTITY_REPR,
      END OF WA_CHANGE_CHECK.

*//Data(Work Area or (Internal) Structures);(WA_ )(ST_)?




*//Internal Tables and Index Fields;(IT_), (I_)
DATA : IT_ZSQM_QUAL_SCORE LIKE ZSQM_QUAL_SCORE OCCURS 0
                                              WITH HEADER LINE.

*-- Internale Tables with structure as sama as DB
DATA : IT_ZTQM_Q_SCORE   LIKE ZTQM_Q_SCORE OCCURS 0 WITH HEADER LINE,
       IT_ZTQM_Q_SCORE_B LIKE ZTQM_Q_SCORE OCCURS 0 WITH HEADER LINE,
       IT_ZTQM_Q_SCORE_D LIKE ZTQM_Q_SCORE OCCURS 0 WITH HEADER LINE.

*//Ranges; (R_)
RANGES : R_COND_DATE FOR ZTQM_Q_SCORE-ISSUEDAT. "/Select Condition.

*//Field Symbols; <FS_>
*-- TABLE CONTROLS VARIABLE(field-symbols)
FIELD-SYMBOLS: <TC>  TYPE CXTAB_CONTROL. "table control
"                              Table_control Object(CXTAB)

*//Field Group;


***//Macro Definitions
*-- macro : EXCLUDING_FUNCTION_CODE &1 &2
*--           &1 - Excluding Function Code Table Name for pf-status
*--           &2 - Excluding Function code
DEFINE EXCLUDING_FUNCTION_CODE.
  MOVE &2 TO WA_EX_FUNC-FCODE.
  APPEND WA_EX_FUNC TO &1.
END-OF-DEFINITION.


***//& Selection Screen Definition(Parameters Select-Option)
*-- Paramerters : (P_), Select-Options : (S_)
SELECTION-SCREEN BEGIN OF BLOCK BLK WITH FRAME  TITLE TEXT-T01.
*- Create
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS : P_CREAT  TYPE C RADIOBUTTON GROUP SEL.
SELECTION-SCREEN COMMENT 4(10) TEXT-P01   FOR FIELD P_CREAT.
SELECTION-SCREEN END OF LINE.

*- Change
SELECTION-SCREEN BEGIN OF LINE.

PARAMETERS : P_CHANG  TYPE C RADIOBUTTON GROUP SEL.
SELECTION-SCREEN COMMENT 4(10) TEXT-P02   FOR FIELD P_CHANG.

SELECT-OPTIONS S_DAT_C FOR ZTQM_Q_SCORE-ISSUEDAT
                      DEFAULT WA_FROM_DATE TO WA_TO_DATE.

SELECTION-SCREEN END OF LINE.

*- Display
SELECTION-SCREEN BEGIN OF LINE.

PARAMETERS : P_DISPL  TYPE C RADIOBUTTON GROUP SEL DEFAULT 'X'.
SELECTION-SCREEN COMMENT 4(10) TEXT-P03   FOR FIELD P_DISPL.

SELECT-OPTIONS S_DAT_D FOR ZTQM_Q_SCORE-ISSUEDAT
                      DEFAULT WA_FROM_DATE TO WA_TO_DATE.

SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK BLK .

*-- Seclection Screen Flow Logic Control Event Handling
*AT SELECTION-SCREEN ON ( ON END OF, ON VALUE-REQUEST FOR,
* ON HELP-REQUEST FOR, ON RADIOBUTTON GROUP, ON BLOCK OUTPUT,
* ON EXIT-COMMAND )

AT SELECTION-SCREEN OUTPUT.
  SET TITLEBAR '1000'.

AT SELECTION-SCREEN.
  CHECK SY-UCOMM = 'ONLI'.
  CASE C_MARK.
    WHEN P_CREAT.
      WA_MODE = C_CREATE.
    WHEN P_CHANG.
      WA_MODE = C_CHANGE.
      R_COND_DATE = S_DAT_C.
      R_COND_DATE[] = S_DAT_C[].
    WHEN P_DISPL.
      WA_MODE = C_DISPLAY.
      R_COND_DATE = S_DAT_D.
      R_COND_DATE[] = S_DAT_D[].
    WHEN OTHERS.
      STOP.
  ENDCASE.

*- Check Required Field
  IF R_COND_DATE[] IS INITIAL AND
     P_CREAT       IS INITIAL.
    MESSAGE E000(ZMQM) WITH 'Fill Required field : From/to Date'(E01).
  ENDIF.

  REFRESH : IT_ZSQM_QUAL_SCORE, IT_ZTQM_Q_SCORE.

  CASE WA_MODE.
    WHEN C_CREATE.

    WHEN C_CHANGE OR C_DISPLAY.
      PERFORM GET_N_CHECK_DATA.
      IF IT_ZSQM_QUAL_SCORE[] IS INITIAL.
        MESSAGE E000(ZMQM) WITH 'No data founded!.'(W01).
*        STOP.
      ENDIF.

    WHEN OTHERS.
  ENDCASE.

*-- Selection for Selection Screen
*START-OF-SELECTION.


*-- End of Selection.
END-OF-SELECTION.
  CLEAR WA_STATUS.
  CASE WA_MODE.
    WHEN C_CREATE OR C_CHANGE.
      PERFORM SET_EXCLUDING_FCODE_CHANGE.
    WHEN OTHERS.
      PERFORM SET_EXCLUDING_FCODE_DISPLAY.
  ENDCASE.

  CALL SCREEN 9000.

*// Event Handling(Except Selection Screen (Flow)event)
LOAD-OF-PROGRAM.
*-- Get default date for Select-option :FROM date.
  WA_FROM_DATE = SY-DATUM.
  WA_FROM_DATE+6(2) = '01'. "/First Date of this month
  WA_TO_DATE = SY-DATUM.

*INITIALIZATION.



**<<<<<<<<< Program Main / Subroutine / Flow Logic >>>>>>>>>>>>**
*&------------------------------------------------------------------*
*&      Form  GET_N_CHECK_DATA
*&------------------------------------------------------------------*
FORM GET_N_CHECK_DATA.
*-- SQL For Selection Condition
  SELECT A~MANDT A~LIFNR  A~LINESTOP A~MEINH
         A~MATNR    A~ISSUEDAT  A~MEINS
         A~QNT_CAMP A~QNT_SALV A~QNT_REPR
         B~NAME1
         C~MAKTX
         A~ERDAT A~ERZET A~ERNAM A~AEDAT A~AEZET A~AENAM
    INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_QUAL_SCORE
      FROM ( ZTQM_Q_SCORE AS A INNER JOIN LFA1 AS B
         ON   A~LIFNR = B~LIFNR  ) INNER JOIN MAKT AS C
         ON   A~MATNR = C~MATNR
       WHERE  C~SPRAS = SY-LANGU
         AND  A~ISSUEDAT IN  R_COND_DATE.

  CHECK SY-SUBRC = 0.
  SORT IT_ZSQM_QUAL_SCORE BY LIFNR MATNR ISSUEDAT ASCENDING.

*-- Back up to IT_ZTQM_Q_SCORE
  REFRESH IT_ZTQM_Q_SCORE_B.
  LOOP AT  IT_ZSQM_QUAL_SCORE.
    CLEAR IT_ZTQM_Q_SCORE_B.
    MOVE-CORRESPONDING : IT_ZSQM_QUAL_SCORE TO  IT_ZTQM_Q_SCORE_B.
    APPEND IT_ZTQM_Q_SCORE_B.
  ENDLOOP.

ENDFORM.                    " GET_N_CHECK_DATA
*&------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS '9000'  EXCLUDING IT_EX_FUNC.
  SET TITLEBAR  '9000' WITH WA_MODE.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.
  CASE OK_CODE.
    WHEN 'BACK'.
      IF WA_STATUS NE C_SAVED AND ( WA_MODE = C_CHANGE OR
                                    WA_MODE = C_CREATE   ) AND
         NOT IT_ZSQM_QUAL_SCORE[] IS INITIAL.
        CLEAR WA_ANSWER.
        PERFORM POPUP_TO_CONFIRM_LOSS_OF_DATA  USING WA_ANSWER.
        IF WA_ANSWER = 'N'. STOP. ENDIF.
      ENDIF.
      LEAVE TO SCREEN 0.

    WHEN 'SAVE'.
      CHECK NOT IT_ZSQM_QUAL_SCORE[] IS INITIAL.
      CLEAR WA_STATUS.
      CASE WA_MODE.
        WHEN C_CREATE.
          PERFORM PREPARE_FOR_SAVE_CREATE.
          PERFORM SAVE_DATA_TO_DB_CREATE.
        WHEN C_CHANGE.
          PERFORM PREPARE_FOR_SAVE_CHANGE.
          PERFORM SAVE_DATA_TO_DB_CHANGE.
        WHEN OTHERS.
      ENDCASE.
      CHECK WA_STATUS = C_SAVED.
      LEAVE TO SCREEN 0.

    WHEN 'DEL_ROW'.
*    IT_ZTQM_Q_SCORE_D
      PERFORM DELETE_ROW.

    WHEN 'SEL_ALL'.
      CLEAR IT_ZSQM_QUAL_SCORE.
      MOVE C_MARK TO IT_ZSQM_QUAL_SCORE-MARK.
      MODIFY IT_ZSQM_QUAL_SCORE TRANSPORTING MARK
                        WHERE LIFNR NE ''.

    WHEN 'DES_ALL'.
      CLEAR IT_ZSQM_QUAL_SCORE.
      MODIFY IT_ZSQM_QUAL_SCORE TRANSPORTING MARK
                        WHERE LIFNR NE ''.
    WHEN 'CHG_DISP'.
      CASE WA_MODE.
        WHEN C_CHANGE.
          WA_MODE = C_DISPLAY.
          PERFORM SET_EXCLUDING_FCODE_DISPLAY.
        WHEN C_DISPLAY.
          WA_MODE = C_CHANGE.
          PERFORM SET_EXCLUDING_FCODE_CHANGE.
      ENDCASE.

    WHEN OTHERS.

  ENDCASE.


ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&-----------------------------------------------------------------*
*&      Form  POPUP_TO_CONFIRM_LOSS_OF_DATA
*&-----------------------------------------------------------------*
FORM POPUP_TO_CONFIRM_LOSS_OF_DATA USING    P_ANSWER.

  CALL FUNCTION 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
       EXPORTING
            TEXTLINE1     = 'Unsaved data will be lost!'(T51)
            TEXTLINE2     = 'Do you want continue? '(T52)
            TITEL         = 'Leave Current Processing '(T50)
            START_COLUMN  = 25
            START_ROW     = 6
            DEFAULTOPTION = 'N'
       IMPORTING
            ANSWER        = P_ANSWER.


ENDFORM.                    " POPUP_TO_CONFIRM_LOSS_OF_DATA
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.

  CLEAR WA_ANSWER.
  IF     WA_STATUS NE C_SAVED               AND
     NOT IT_ZSQM_QUAL_SCORE[] IS INITIAL.
    CASE WA_MODE.
      WHEN C_CREATE.
        PERFORM POPUP_TO_CONFIRM_LOSS_OF_DATA  USING WA_ANSWER.
        IF WA_ANSWER = 'N'. STOP. ENDIF.

      WHEN C_CHANGE .
        PERFORM POPUP_TO_CONFIRM_LOSS_OF_DATA  USING WA_ANSWER.
        IF WA_ANSWER = 'N'. STOP. ENDIF.

      WHEN C_DISPLAY.

    ENDCASE.
  ENDIF.

  CASE OK_CODE.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'RW'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT
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
*&      Module  MODIFY_SCREEN_9000  OUTPUT
*&-----------------------------------------------------------------*
MODULE MODIFY_SCREEN_9000 OUTPUT.
* To hide Conditions field in Create screen
  CHECK WA_MODE = C_CREATE.
  LOOP AT SCREEN.
    CHECK SCREEN-GROUP1 = 'CRT'.
    SCREEN-INVISIBLE = 1.
    MODIFY SCREEN.

  ENDLOOP.
ENDMODULE.                 " MODIFY_SCREEN_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TABLE_CONTROL_OUTPUT_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TABLE_CONTROL_OUTPUT_9000 OUTPUT.

* Table control is TC_9000
* Read table using current index.
  CONCATENATE 'TC_' SY-DYNNR INTO WA_TCNAME.
  ASSIGN (WA_TCNAME) TO <TC>.

  READ TABLE IT_ZSQM_QUAL_SCORE INDEX <TC>-CURRENT_LINE .
  IF SY-SUBRC EQ 0.
    MOVE-CORRESPONDING IT_ZSQM_QUAL_SCORE TO ZSQM_QUAL_SCORE.
  ELSE.
    CLEAR ZSQM_QUAL_SCORE.
  ENDIF.

  WA_CON_LINES = SY-LOOPC.

ENDMODULE.                 " TABLE_CONTROL_OUTPUT_9000  OUTPUT
*&-----------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_TABLE_9000  OUTPUT
*&-----------------------------------------------------------------*
MODULE MODIFY_SCREEN_TABLE_9000 OUTPUT.

* When the mode is Display or status is Saved -no value input allowed
  IF WA_STATUS = C_SAVED OR WA_MODE = C_DISPLAY.
    LOOP AT SCREEN.
      SCREEN-INPUT = 0.
      IF ZSQM_QUAL_SCORE IS INITIAL.
        SCREEN-ACTIVE = 0.
      ELSEIF SCREEN-GROUP4 = 'MAK' AND NOT ZSQM_QUAL_SCORE IS INITIAL.
        SCREEN-INPUT = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.
* When quality score is initial and mode is Create - screen Active
  IF ZSQM_QUAL_SCORE IS INITIAL AND WA_MODE NE C_CREATE.
    LOOP AT SCREEN.
      SCREEN-ACTIVE = 0.
      MODIFY SCREEN.
    ENDLOOP.
    EXIT.
  ENDIF.
* On change Mode Key fields cannot be changed, change only
* line stop, salvage, campaign, Repair
  IF WA_MODE = C_CHANGE.
    LOOP AT SCREEN.
      IF SCREEN-GROUP2 = 'CHG'.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " MODIFY_SCREEN_TABLE_9000  OUTPUT
*&-----------------------------------------------------------------*
*&      Module  TABLE_CONTROL_LINES_9000  OUTPUT
*&-----------------------------------------------------------------*
MODULE TABLE_CONTROL_LINES_9000 OUTPUT.
*--- Move the number of Internal Table Records to TABLE CONTROL-LINES
  CONCATENATE 'TC_' SY-DYNNR INTO WA_TCNAME.
  ASSIGN (WA_TCNAME) TO <TC>.                "not headerline

  DESCRIBE TABLE IT_ZSQM_QUAL_SCORE LINES <TC>-LINES.

ENDMODULE.                 " TABLE_CONTROL_LINES_9000  OUTPUT
*&-----------------------------------------------------------------*
*&      Module  TABLE_CONTROL_INPUT_9000  INPUT
*&-----------------------------------------------------------------*
MODULE TABLE_CONTROL_INPUT_9000 INPUT.
*---MOVE data to Internal Table from TABLE CONTROL.
  CONCATENATE 'TC_' SY-DYNNR INTO WA_TCNAME.
  ASSIGN (WA_TCNAME) TO <TC>.

  CLEAR IT_ZSQM_QUAL_SCORE.

  READ TABLE IT_ZSQM_QUAL_SCORE INDEX <TC>-CURRENT_LINE.

  IF SY-SUBRC NE 0 AND  NOT ZSQM_QUAL_SCORE IS INITIAL.
    MOVE-CORRESPONDING ZSQM_QUAL_SCORE TO IT_ZSQM_QUAL_SCORE.
    MOVE : SY-DATUM  TO IT_ZSQM_QUAL_SCORE-ERDAT,
           SY-UZEIT  TO IT_ZSQM_QUAL_SCORE-ERZET,
           SY-UNAME  TO IT_ZSQM_QUAL_SCORE-ERNAM.

*    IF IT_ZSQM_QUAL_SCORE-MATNR NE ZSQM_QUAL_SCORE-MATNR OR
*       ZSQM_QUAL_SCORE-MEINS    IS INITIAL.

* Get Unit of Measure and Description of Material
    PERFORM GET_MEINS_OF_MATNR  USING IT_ZSQM_QUAL_SCORE-MATNR
                                      IT_ZSQM_QUAL_SCORE-MEINS
                                      IT_ZSQM_QUAL_SCORE-MAKTX.
*    ENDIF.

* Get Vendor text
    PERFORM GET_VENDOR_TEXT     USING IT_ZSQM_QUAL_SCORE-LIFNR
                                      IT_ZSQM_QUAL_SCORE-NAME1.

* Line stop field is measured in Minutes. If Qual score is available add
* Unit at MIN
    IF NOT ZSQM_QUAL_SCORE IS INITIAL.
      MOVE : 'MIN' TO IT_ZSQM_QUAL_SCORE-MEINH. "/Default unit 'MIN'
    ENDIF.

    APPEND IT_ZSQM_QUAL_SCORE.
    EXIT.
  ENDIF.

  MOVE-CORRESPONDING : IT_ZSQM_QUAL_SCORE TO WA_ZSCA_TIME_STAMP.

  IF WA_MODE = C_CHANGE AND NOT ZSQM_QUAL_SCORE IS INITIAL.

    MOVE-CORRESPONDING IT_ZSQM_QUAL_SCORE TO  WA_CHANGE_CHECK.
* Check to record changes made to data.
* If changes are made then update current timestamp, date and username
    IF WA_CHANGE_CHECK-LINESTOP NE ZSQM_QUAL_SCORE-LINESTOP  OR
       WA_CHANGE_CHECK-QNT_CAMP NE ZSQM_QUAL_SCORE-QNT_CAMP  OR
       WA_CHANGE_CHECK-QNT_SALV NE ZSQM_QUAL_SCORE-QNT_SALV  OR
       WA_CHANGE_CHECK-QNT_REPR NE ZSQM_QUAL_SCORE-QNT_REPR.
      MOVE : SY-DATUM  TO WA_ZSCA_TIME_STAMP-AEDAT,
             SY-UZEIT  TO WA_ZSCA_TIME_STAMP-AEZET,
             SY-UNAME  TO WA_ZSCA_TIME_STAMP-AENAM.
    ENDIF.
  ENDIF.
* Pick up the Unit of Measure for the Material

  IF IT_ZSQM_QUAL_SCORE-MATNR NE ZSQM_QUAL_SCORE-MATNR OR
     ZSQM_QUAL_SCORE-MEINS IS INITIAL.
    PERFORM GET_MEINS_OF_MATNR  USING ZSQM_QUAL_SCORE-MATNR
                                      ZSQM_QUAL_SCORE-MEINS
                                      ZSQM_QUAL_SCORE-MAKTX.
  ENDIF.

* Get Vendor text
  IF IT_ZSQM_QUAL_SCORE-LIFNR NE ZSQM_QUAL_SCORE-LIFNR.
    PERFORM GET_VENDOR_TEXT     USING  ZSQM_QUAL_SCORE-LIFNR
                                       ZSQM_QUAL_SCORE-NAME1.
  ENDIF.

  IF NOT ZSQM_QUAL_SCORE IS INITIAL.
    MOVE : 'MIN' TO ZSQM_QUAL_SCORE-MEINH. "/Default unit 'MIN'
  ENDIF.

  MOVE-CORRESPONDING ZSQM_QUAL_SCORE TO IT_ZSQM_QUAL_SCORE.
  MOVE-CORRESPONDING WA_ZSCA_TIME_STAMP   TO IT_ZSQM_QUAL_SCORE.

  MODIFY IT_ZSQM_QUAL_SCORE INDEX <TC>-CURRENT_LINE.

**-- move Selected Table Control Index to Global Variable(WA_SEL_LINE)
*  IF ZSQM_QUAL_SCORE-MARK = C_MARK.
*    WA_SEL_LINE = <TC>-CURRENT_LINE.
*  ENDIF.
*

ENDMODULE.                 " TABLE_CONTROL_INPUT_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  GET_MEINS_OF_MATNR
*&---------------------------------------------------------------------*
FORM GET_MEINS_OF_MATNR USING    P_MATNR
                                 P_MEINS
                                 P_MAKTX.

  IF P_MATNR IS INITIAL.
    CLEAR : P_MEINS, P_MAKTX.
    EXIT.
  ENDIF.

  SELECT SINGLE A~MEINS B~MAKTX INTO (P_MEINS, P_MAKTX)
    FROM MARA AS A INNER JOIN MAKT AS B
      ON A~MATNR = B~MATNR
      WHERE A~MATNR = P_MATNR
        AND B~SPRAS = SY-LANGU.

  CHECK SY-SUBRC NE 0.
  MESSAGE E000(ZMQM) WITH P_MATNR ', No Existence material'(E03).

ENDFORM.                    " GET_MEINS_OF_MATNR
*&-----------------------------------------------------------------*
*&      Form  PREPARE_FOR_SAVE_CREATE
*&-----------------------------------------------------------------*
FORM PREPARE_FOR_SAVE_CREATE.

  REFRESH IT_ZTQM_Q_SCORE.
  LOOP AT IT_ZSQM_QUAL_SCORE.
    CLEAR IT_ZTQM_Q_SCORE.
    MOVE-CORRESPONDING : IT_ZSQM_QUAL_SCORE TO IT_ZTQM_Q_SCORE.
    APPEND IT_ZTQM_Q_SCORE.
  ENDLOOP.

ENDFORM.                    " PREPARE_FOR_SAVE_CREATE
*&------------------------------------------------------------------*
*&      Form  SAVE_DATA_TO_DB_CREATE
*&------------------------------------------------------------------*
FORM SAVE_DATA_TO_DB_CREATE.

  PERFORM ENQUEUE_DB.

  INSERT ZTQM_Q_SCORE FROM TABLE IT_ZTQM_Q_SCORE.

  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    MESSAGE E000(ZMQM) WITH 'Error founded durin savind data!'(E03).
    EXIT.
  ENDIF.

  COMMIT WORK.
  PERFORM DEQUEUE_DB.
  MESSAGE S000(ZMQM) WITH 'Successfully saved!'(S01).
  WA_STATUS = C_SAVED.

ENDFORM.                    " SAVE_DATA_TO_DB_CREATE
*&------------------------------------------------------------------*
*&      Form  ENQUEUE_DB
*&------------------------------------------------------------------*
FORM ENQUEUE_DB.
  CALL FUNCTION 'ENQUEUE_EZ_ZTQM_Q_SCORE'
       EXPORTING
            MODE_ZTQM_Q_SCORE = 'X'
            MANDT             = SY-MANDT
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
  CALL FUNCTION 'DEQUEUE_EZ_ZTQM_Q_SCORE'
       EXPORTING
            MODE_ZTQM_Q_SCORE = 'X'
            MANDT             = SY-MANDT.

ENDFORM.                    " DEQUEUE_DB
*&---------------------------------------------------------------------*
*&      Form  DELETE_ROW
*&---------------------------------------------------------------------*
FORM DELETE_ROW.
  DATA : LW_INDEX_SCORE LIKE SY-TABIX.
  CASE WA_MODE.
    WHEN C_CREATE.
      DELETE IT_ZSQM_QUAL_SCORE WHERE MARK = C_MARK.

    WHEN C_CHANGE.
      LOOP AT IT_ZSQM_QUAL_SCORE WHERE MARK = C_MARK.
        LW_INDEX_SCORE = SY-TABIX.
        READ TABLE IT_ZTQM_Q_SCORE_B
                    WITH KEY  LIFNR    = IT_ZSQM_QUAL_SCORE-LIFNR
                              MATNR    = IT_ZSQM_QUAL_SCORE-MATNR
                              ISSUEDAT = IT_ZSQM_QUAL_SCORE-ISSUEDAT.

        IF SY-SUBRC = 0.
          MOVE-CORRESPONDING : IT_ZTQM_Q_SCORE_B TO IT_ZTQM_Q_SCORE_D.
          APPEND IT_ZTQM_Q_SCORE_D.
        ENDIF.

        DELETE IT_ZSQM_QUAL_SCORE INDEX LW_INDEX_SCORE.
      ENDLOOP.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " DELETE_ROW
*&---------------------------------------------------------------------*
*&      Form  PREPARE_FOR_SAVE_CHANGE
*&---------------------------------------------------------------------*
FORM PREPARE_FOR_SAVE_CHANGE.
  REFRESH IT_ZTQM_Q_SCORE.
  LOOP AT IT_ZSQM_QUAL_SCORE.
    CLEAR IT_ZTQM_Q_SCORE.
    MOVE-CORRESPONDING : IT_ZSQM_QUAL_SCORE TO IT_ZTQM_Q_SCORE.
    APPEND IT_ZTQM_Q_SCORE.
  ENDLOOP.
ENDFORM.                    " PREPARE_FOR_SAVE_CHANGE
*&------------------------------------------------------------------*
*&      Form  SAVE_DATA_TO_DB_CHANGE
*&------------------------------------------------------------------*
FORM SAVE_DATA_TO_DB_CHANGE.
  PERFORM ENQUEUE_DB.

  IF NOT IT_ZTQM_Q_SCORE_D[] IS INITIAL.

    DELETE  ZTQM_Q_SCORE FROM TABLE IT_ZTQM_Q_SCORE_D.

    IF SY-SUBRC NE 0.
      ROLLBACK WORK.
      MESSAGE E000(ZMQM) WITH TEXT-E03.
      EXIT.
    ENDIF.
  ENDIF.

  UPDATE ZTQM_Q_SCORE FROM TABLE IT_ZTQM_Q_SCORE.

  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    MESSAGE E000(ZMQM) WITH 'Error founded durin savind data!'(E03).
    EXIT.
  ENDIF.

  COMMIT WORK.
  PERFORM DEQUEUE_DB.
  MESSAGE S000(ZMQM) WITH 'Successfully saved!'(S01).
  WA_STATUS = C_SAVED.
ENDFORM.                    " SAVE_DATA_TO_DB_CHANGE
*&---------------------------------------------------------------------*
*&      Form  GET_VENDOR_TEXT
*&---------------------------------------------------------------------*
FORM GET_VENDOR_TEXT USING    P_LIFNR
                              P_NAME1.

  IF P_LIFNR IS INITIAL.  CLEAR P_NAME1. ENDIF.

  SELECT SINGLE NAME1 INTO P_NAME1
    FROM LFA1
      WHERE LIFNR = P_LIFNR.

  CHECK SY-SUBRC NE 0.
  MESSAGE E000(ZMQM) WITH P_LIFNR 'is not exist vendor.'(E04).


ENDFORM.                    " GET_VENDOR_TEXT
*&-----------------------------------------------------------------*
*&      Form  SET_EXCLUDING_FCODE_DISPLAY
*&-----------------------------------------------------------------*
FORM SET_EXCLUDING_FCODE_DISPLAY.
  REFRESH IT_EX_FUNC.
  EXCLUDING_FUNCTION_CODE IT_EX_FUNC : 'DEL_ROW',
*                                       'SEL_ALL',
*                                       'DES_ALL',
                                       'ADD_ROW',
                                       'SAVE'.

ENDFORM.                    " SET_EXCLUDING_FCODE_DISPLAY
*&------------------------------------------------------------------*
*&      Form  SET_EXCLUDING_FCODE_CHANGE
*&------------------------------------------------------------------*
FORM SET_EXCLUDING_FCODE_CHANGE.
  REFRESH IT_EX_FUNC.
*EXCLUDING_FUNCTION_CODE IT_EX_FUNC : ''.,
ENDFORM.                    " SET_EXCLUDING_FCODE_CHANGE

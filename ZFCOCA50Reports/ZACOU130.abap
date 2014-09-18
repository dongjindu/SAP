* Program ID        : ZACOU130
* Title             : [CO] PIR Management
* Created on        : 10/04/2007
* Created by        : IG.MOON
*            ( BDC logic has been copied from ZAPP103U_PIR_CO(Annual),
*                                        ZAPP104U_PIR_CO(Quarterly)
* Specifications By : Andy
* Description       : PIR Management
*----------------------------------------------------------------------
*  Modification Log
*  Date        Developer Issue No    Description
*======================================================================
*  12/22/2011  Valerian  UD1K953619  Activate 'insert line'
*                                    functionalities
*----------------------------------------------------------------------
*RM60RR20   Reorganizing Indep.Reqmts
*RM60RR30   Indep. Requirements Reorganization - Delete
*RM60RR40   Reorg. Indep. Reqmts - Delete History and PIR
REPORT ZACOU130 MESSAGE-ID ZMCO.

INCLUDE ZACOUI00.
INCLUDE ZACOU130_TOP.

*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-001.
PARAMETERS : P_BUKRS  TYPE BUKRS OBLIGATORY MEMORY ID BUK,
             P_PLSCN  LIKE PLAF-PLSCN DEFAULT '901' OBLIGATORY.
SELECT-OPTIONS S_MATNR FOR PLAF-MATNR.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BL1S WITH FRAME TITLE TEXT-01S.
PARAMETERS     : P_QUAR  RADIOBUTTON GROUP
                          SELM DEFAULT 'X'  USER-COMMAND UCOM,
                 P_ANNU  RADIOBUTTON GROUP SELM.
SELECTION-SCREEN END OF BLOCK BL1S.
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 2(24) PERTXT.
PARAMETERS  P_HORY LIKE SY-DATUM+0(4) DEFAULT SY-DATUM+0(4).
SELECTION-SCREEN COMMENT 44(31) QUATXT MODIF ID QUA.
PARAMETERS  P_HORM LIKE SY-DATUM+4(2) DEFAULT SY-DATUM+4(2)
                MODIF ID QUA.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK BL1.
SELECTION-SCREEN BEGIN OF BLOCK SR1 WITH FRAME TITLE TEXT-002.
PARAMETERS     : P_SR_E  RADIOBUTTON GROUP SRCS USER-COMMAND UCOM,
                 P_SR_S  RADIOBUTTON GROUP SRCS  DEFAULT 'X'.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 10(21) SCNTXT MODIF ID PLN.
PARAMETERS P_PLSC  LIKE PLAF-PLSCN DEFAULT '900' MODIF ID PLN.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK SR1.

* block 3 {
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-005.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 35(12)  TEXT-X00 FOR FIELD P_EXL
                                 MODIF ID EXL.
PARAMETERS P_EXL   RADIOBUTTON GROUP RADI DEFAULT 'X'
                                 MODIF ID EXL.
SELECTION-SCREEN COMMENT 55(21) TEXT-X01
                                 MODIF ID EXL.
PARAMETERS P_TXT     RADIOBUTTON GROUP RADI
                                 MODIF ID EXL.
SELECTION-SCREEN END   OF LINE.

PARAMETERS: P_FILE  LIKE RLGRAP-FILENAME OBLIGATORY
                    DEFAULT 'c:\temp\PIR_Upload.xls'
                    MODIF ID EXL.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS P_HEAD AS CHECKBOX MODIF ID EXL DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(22) TEXT-T07 MODIF ID EXL.
SELECTION-SCREEN COMMENT 33(38) TEXT-T04 MODIF ID EXL.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END   OF BLOCK B3.
* }

* Layout
SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-010.
PARAMETER P_VARI TYPE SLIS_VARI.
SELECTION-SCREEN END OF BLOCK B4.

PARAMETERS   C_MODE    DEFAULT 'N' NO-DISPLAY.   "BDC MODE

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  SY-TITLE = '[CO] PIR Management '.
  PERFORM DEFAULT_.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.
  PERFORM ALV_VARIANT_F4 CHANGING P_VARI.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_HORM.
  PERFORM MONTH_INPUT_HELP CHANGING P_HORM.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_HORY.
  PERFORM YEAR_INPUT_HELP CHANGING P_HORY.

AT SELECTION-SCREEN OUTPUT.
  PERFORM MODIFY_SCREEN.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM BROWSER CHANGING P_FILE.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*

  __PROCESS 'Initialize...' '10'.
  PERFORM   INITIALIZE.

  __PROCESS 'Validate...' '20'.
  PERFORM   VALIDATE.
  CASE 'X'.
    WHEN P_SR_S.       " source is SAP
      __PROCESS 'Get plan data from PLAF...' '30'.
      PERFORM GET_PLANT.
      PERFORM SELECT_PLAN.
      __PROCESS 'Preparation...' '30'.
      PERFORM MAKE_IT_FILE.
    WHEN P_SR_E.       " source is Excel
      __PROCESS 'Upload...' '30'.
      PERFORM UPLOAD_FILE USING P_FILE.
  ENDCASE.

  __PROCESS 'Fill date...' '40'.
  PERFORM SET_WORKING_DATE_MONTHLY.

  PERFORM WORKING_DATE.

  __PROCESS 'Finalizing...' '60'.
  IF ERROR_IX EQ 0.
    __CLS IT_ROW_TAB.
    LOOP AT IT_FILE.
      MOVE-CORRESPONDING IT_FILE TO IT_ROW_TAB.
      APPEND IT_ROW_TAB.
    ENDLOOP.
    PERFORM MOVE_OUT.
  ELSE.
    G_ERROR = TRUE.
  ENDIF.

END-OF-SELECTION.
  CHECK G_ERROR EQ SPACE .
  PERFORM INFO_TEXT_SET USING FALSE.
  __PROCESS 'Finalizing...' '90'.
  PERFORM SET_OUTPUT .

*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_S01  text
*      -->P_&1  text
*----------------------------------------------------------------------*
FORM SHOW_PROGRESS USING    PF_TEXT
                            VALUE(PF_VAL).

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            PERCENTAGE = PF_VAL
            TEXT       = PF_TEXT.

ENDFORM.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INITIALIZE.
  CLEAR G_ERROR.
ENDFORM.                    " INITIALIZE_
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VALIDATE.

  DATA : L_YYYYMM     TYPE   SPMON,
         L_YYYY(4)    TYPE   N,
         L_MM(2)      TYPE   N.

  PERFORM GET_VERSION_NUMBER  USING P_PLSCN
                              CHANGING C_VERSB.
  IF C_VERSB IS INITIAL.
    MESSAGE S000 WITH
    'Could not find the Version number for independent'
    'requirements with planning scenario :' P_PLSC.
    G_ERROR = TRUE.
  ENDIF.

  CHECK G_ERROR EQ SPACE.
  CASE 'X'.
    WHEN P_QUAR.
      IF P_HORM IS INITIAL.
        SET CURSOR FIELD 'P_HORM'.
        MESSAGE S000 WITH 'Enter the Start Month of Plan Horizon.'.
        G_ERROR = TRUE.
      ENDIF.
  ENDCASE.

  __CLS R_PDATU.
  R_PDATU-SIGN = 'I'.
  R_PDATU-OPTION = 'BT'.
  CASE 'X'.
    WHEN P_QUAR.
      IF P_HORM GT '00' AND P_HORM LE '12'.
        CONCATENATE P_HORY P_HORM '01' INTO R_PDATU-LOW.
        CONCATENATE P_HORY P_HORM INTO L_YYYYMM.

        DO 3 TIMES.
          L_YYYY = L_YYYYMM(4).
          L_MM   = L_YYYYMM+4(2).
          IF L_MM EQ '12'.
            L_YYYY = L_YYYY + 1.
            CONCATENATE L_YYYY '01' INTO L_YYYYMM.
          ELSE.
            L_YYYYMM = L_YYYYMM + 1.
          ENDIF.
        ENDDO.
        CONCATENATE L_YYYYMM '01' INTO R_PDATU-HIGH.
        R_PDATU-HIGH = R_PDATU-HIGH - 1.
        APPEND R_PDATU.
      ELSE.
        SET CURSOR FIELD 'P_HORM'.
        MESSAGE S000 WITH 'Check the Month you entered.'.
        G_ERROR = TRUE.
      ENDIF.
    WHEN P_ANNU.
      CONCATENATE P_HORY '0101' INTO R_PDATU-LOW.
      CONCATENATE P_HORY '1231' INTO R_PDATU-HIGH.
      APPEND R_PDATU.
  ENDCASE.

  MOVE C_VERSB  TO  WA_PBDNR.

ENDFORM.                    " VALIDATE_
*&---------------------------------------------------------------------*
*&      Form  default_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DEFAULT_.

*  S_HKONT-SIGN = 'I'.
*  S_HKONT-OPTION = 'EQ'.
*  S_HKONT-LOW = '0000530180'.
*  APPEND S_HKONT.
*
*  S_HKONT-SIGN = 'I'.
*  S_HKONT-OPTION = 'EQ'.
*  S_HKONT-LOW = '0000532100'.
*  APPEND S_HKONT.
*
*  S_BKLAS-SIGN = 'I'.
*  S_BKLAS-OPTION = 'BT'.
*  S_BKLAS-LOW  = '3000'.
*  S_BKLAS-HIGH = '3005'.
*  APPEND S_BKLAS.

  WRITE: ICON_PERIOD AS ICON TO PERTXT,
        'Year of Plan Horizon' TO PERTXT+4(20).
  WRITE: ICON_EFFECTIVITY_PERIOD AS ICON TO QUATXT,
        'Start Month of Plan Horizon' TO QUATXT+4(27).
  WRITE: ICON_BW_RULES_SAP AS ICON TO SCNTXT,
        'Planning scenario' TO SCNTXT+4(17).

ENDFORM.                    " default_
*&---------------------------------------------------------------------*
*&      Form  MOVE_OUT_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MOVE_OUT.
  CHECK G_ERROR EQ SPACE.

  __PROCESS 'Preparing output...' '95'.

  __CLS : GT_OUT.

  LOOP AT IT_ROW_TAB.
    MOVE-CORRESPONDING IT_ROW_TAB TO GT_OUT.
    APPEND GT_OUT.
  ENDLOOP.

ENDFORM.                    " MOVE_OUT_
*&---------------------------------------------------------------------*
*&      Form  SAVE_z_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_Z_TABLE.

  CHECK : G_ERROR EQ FALSE.

  __PROCESS 'Saving data...' '90'.

ENDFORM.                    " SAVE_z_TABLE
*&---------------------------------------------------------------------*
*&      Form  POP_UP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0811   text
*      -->P_0812   text
*      -->P_0813   text
*      <--P_L_ANSWER  text
*----------------------------------------------------------------------*
FORM POP_UP USING    P_TEXT P_TEXT2 P_CANC
            CHANGING P_ANSWER.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            TEXTLINE1      = P_TEXT
            TEXTLINE2      = P_TEXT2
            TITEL          = 'Check!'
            CANCEL_DISPLAY = P_CANC
       IMPORTING
            ANSWER         = P_ANSWER.


ENDFORM.                    " POP_UP

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET TITLEBAR '100'.
*   Exclude toolbar
  PERFORM EXCLUDE_FUNCTIONS.
  PERFORM STATUS_CHANGE.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ER_DATA_CHANGED  text
*----------------------------------------------------------------------*
FORM DATA_CHANGED USING RR_DATA_CHANGED
                        TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.

  FLAG_DATA_CHANGED = TRUE.

  DATA: LS_MOD_CELLS TYPE LVC_S_MODI,
        LS_CELLS     TYPE LVC_S_MODI,
        LT_VALUES TYPE TABLE OF BAPI_CHAR_VALUES WITH HEADER LINE.

  LOOP AT RR_DATA_CHANGED->MT_GOOD_CELLS INTO LS_MOD_CELLS.
    READ TABLE GT_OUT INDEX LS_MOD_CELLS-ROW_ID.
    IF SY-SUBRC = 0.
      CALL METHOD RR_DATA_CHANGED->MODIFY_CELL
                EXPORTING I_ROW_ID    = LS_MOD_CELLS-ROW_ID
                          I_FIELDNAME = LS_MOD_CELLS-FIELDNAME
                          I_VALUE     = LS_MOD_CELLS-VALUE.
      GT_OUT-CHANGED = TRUE.
      MODIFY GT_OUT INDEX LS_MOD_CELLS-ROW_ID.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_ALV_100 OUTPUT.
  IF G_CUSTOM_CONTAINER IS INITIAL.
    PERFORM CREATE_AND_INIT_ALV.
*   Display alv grid
    CALL METHOD G_GRID->SET_TABLE_FOR_FIRST_DISPLAY
         EXPORTING IS_LAYOUT            = GS_LAYO
                   IT_TOOLBAR_EXCLUDING = GT_EXCLUDE
                   I_SAVE               = GC_VAR_SAVE
                   IS_VARIANT           = GS_VARIANT
         CHANGING  IT_OUTTAB            = GT_OUT[]
                   IT_FIELDCATALOG      = GT_FCAT[]
                   IT_SORT              = GT_SORT[].
  ENDIF.
  __FOCUS G_GRID.

ENDMODULE.                 " DISPLAY_ALV_100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_AND_INIT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_AND_INIT_ALV.
*   Create object
  PERFORM CREATE_OBJECT.

*  Create Object to verify input values.
  CREATE OBJECT G_EVENT_RECEIVER.
  SET HANDLER : G_EVENT_RECEIVER->HANDLE_DATA_CHANGED FOR G_GRID.

*   Create field category
  PERFORM CREATE_FIELD_CATEGORY USING FALSE.

  CALL METHOD G_GRID->REGISTER_EDIT_EVENT
       EXPORTING I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

  CALL METHOD G_GRID->SET_READY_FOR_INPUT
     EXPORTING
            I_READY_FOR_INPUT = 0.

  PERFORM SORT_BUILD USING GT_SORT[].

*   Setting for layout
  PERFORM SET_LVC_LAYOUT.

*   Set colors
  PERFORM SET_COLOR.

*   Set variant
  GV_REPID = GS_VARIANT-REPORT = SY-REPID.
  GS_VARIANT-VARIANT = P_VARI.

ENDFORM.                    " CREATE_AND_INIT_ALV
*&---------------------------------------------------------------------*
*&      Form  CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FALSE  text
*----------------------------------------------------------------------*
FORM CREATE_FIELD_CATEGORY USING MODE_EDIT.
  DATA: L_POS       TYPE I.
  DEFINE __CATALOG.
    L_POS = L_POS + 1.
    CLEAR GS_FCAT.
    GS_FCAT-COL_POS       = L_POS.
    GS_FCAT-KEY           = &1.
    GS_FCAT-FIELDNAME     = &2.
    GS_FCAT-COLTEXT       = &3.     " Column heading
    GS_FCAT-OUTPUTLEN     = &4.     " Column width
    GS_FCAT-DATATYPE      = &5.     " Data type
    GS_FCAT-EMPHASIZE     = &6.
    GS_FCAT-EDIT     = &7.
    APPEND GS_FCAT TO GT_FCAT.
  END-OF-DEFINITION.

  DATA : $IX(2) TYPE N,
         $Q_START(2) TYPE N,
         $Q_END(2) TYPE N,
         $MTXT(3).

  IF P_QUAR EQ TRUE.
    $Q_START = P_HORM.
    $Q_END = P_HORM + 3.
  ELSE.
    $Q_START = '01'.
    $Q_END = '13'.
  ENDIF.

  __CATALOG :
* BEGIN OF UD1K953619
    'X'  'WERKS'    'Plant'             4  'CHAR' '' 'X',
    'X'  'MATNR'    'MI/FSC'           18  'CHAR' '' 'X'.

*   'X'  'WERKS'    'Plant'             4  'CHAR' '' '',
*   'X'  'MATNR'    'MI/FSC'           18  'CHAR' '' ''.
* END OF UD1K953619

  DO 12 TIMES.
    $IX = SY-INDEX.
    CHECK : $IX >= $Q_START, $IX < $Q_END.

    CONCATENATE 'M' $IX INTO $MTXT.
    __CATALOG :
      ' '  $MTXT    $IX                15  'QUAN' '' 'X'.
  ENDDO.

  __CATALOG :
    ' '  'ICON'    'I'                  3  'ICON' '' '',
    ' '  'MSG'     'remarks'           30  'CHAR' '' ''.

  LOOP AT GT_FCAT INTO GS_FCAT.
    GS_FCAT-REF_TABLE = 'ZSCOU130'.
    GS_FCAT-REF_FIELD = GS_FIELDCAT-FIELDNAME.
    MODIFY GT_FCAT FROM GS_FCAT.
  ENDLOOP.

ENDFORM.                    " CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXCLUDE_FUNCTIONS.
  PERFORM APPEND_EXCLUDE_FUNCTIONS
           TABLES GT_EXCLUDE[]
           USING: CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO,
                  CL_GUI_ALV_GRID=>MC_FC_AVERAGE,
                  CL_GUI_ALV_GRID=>MC_FC_GRAPH,
                  CL_GUI_ALV_GRID=>MC_FC_INFO,
                  CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW,
                  CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW,
                  CL_GUI_ALV_GRID=>MC_FC_LOC_CUT,
*                 CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW,    "UD1K953619
                  CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW,
                  CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.

ENDFORM.                    " EXCLUDE_FUNCTIONS

*&---------------------------------------------------------------------*
*&      Form  SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_LVC_LAYOUT.
  CLEAR GS_LAYO.

  GS_LAYO-ZEBRA      = 'X'.
  GS_LAYO-SEL_MODE   = 'A'.       " Column and row selection
  GS_LAYO-CWIDTH_OPT = 'X'.
  GS_LAYO-CTAB_FNAME = 'TABCOLOR'.

ENDFORM.                    " SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  SET_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_COLOR.
  CLEAR: GS_SPECIALCOL, GT_SPECIALCOL[], GT_OUT-TABCOLOR[].

  DEFINE __COLOR.
    GS_SPECIALCOL-FIELDNAME = &1 .
    GS_SPECIALCOL-COLOR-COL = &2 .
    GS_SPECIALCOL-COLOR-INT = &3 .
    APPEND GS_SPECIALCOL TO GT_SPECIALCOL .
  END-OF-DEFINITION.

  DATA : $IX(2) TYPE N,
         $MTXT(3).

  __COLOR :
    'WERKS'        '1' 0,
    'MATNR'        '1' 0,
    'ICON'         '2' 0,
    'MSG'          '2' 0.

  DO 12 TIMES.
    $IX = SY-INDEX.
    CONCATENATE 'M' $IX INTO $MTXT.
    __COLOR :
      $MTXT          '2' 0.
  ENDDO.


  GT_OUT-TABCOLOR[] = GT_SPECIALCOL[].
  MODIFY GT_OUT TRANSPORTING TABCOLOR WHERE TABCOLOR IS INITIAL.

ENDFORM.                    " SET_COLOR
*&---------------------------------------------------------------------*
*&      Form  SORT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SORT[]  text
*----------------------------------------------------------------------*
FORM SORT_BUILD USING FT_SORT TYPE LVC_T_SORT.
  DEFINE SORT_TAB.
    CLEAR GS_SORT.
    GS_SORT-FIELDNAME = &1.
    GS_SORT-SPOS      = &2.
    GS_SORT-UP        = &3.
    GS_SORT-GROUP     = &4.
    GS_SORT-SUBTOT    = &5.
    GS_SORT-COMP      = &6.
    APPEND GS_SORT TO FT_SORT.
  END-OF-DEFINITION.

  SORT_TAB :
             'WERKS'    '1' 'X' '' 'X' '',
             'MATNR'    '2' 'X' '' 'X' ''.

ENDFORM.                    " SORT_BUILD
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CLEAR : G_ERROR.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.
  CASE OK_CODE.
    WHEN 'BACK' OR 'CANC'.
      PERFORM FREE_CONTAINER.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'SAVE'.
      PERFORM : SAVING,
                REFRESH_ALV.
    WHEN 'PROC'.
*      PERFORM REALLY?.
      CHECK G_ERROR NE TRUE.
      PERFORM : SET_MODE,
                PROC,
                REFRESH_ALV.
    WHEN 'SWITCH'.
      CHECK SY-DYNNR EQ '0100'.
      PERFORM SWITCH_EDIT_MODE.
      __FOCUS G_GRID.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  REFRESH_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REFRESH_ALV.
  PERFORM SET_COLOR.
  __SET_REFRESH_MODE TRUE.
  CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY
       EXPORTING IS_STABLE = STABLE.
ENDFORM.                    " REFRESH_ALV
*&---------------------------------------------------------------------*
*&      Form  REALLY?
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REALLY?.
  DATA $EXISTS(1).
  DATA L_ANSWER(1).

  PERFORM POP_UP USING
      'The existing data will be changed!'
      'Do you really want to apply?' ' '
                 CHANGING L_ANSWER.

  IF L_ANSWER NE 'J'.
    G_ERROR = TRUE.
    MESSAGE S000 WITH 'Processing was canceled by user.'.
  ENDIF.

ENDFORM.                    " REALLY?
*&---------------------------------------------------------------------*
*&      Form  SELECT_ROW_BY_SUBTOTAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_ROWS  text
*----------------------------------------------------------------------*
FORM SELECT_ROW_BY_SUBTOTAL TABLES P_PT_ROWS
                                   STRUCTURE LVC_S_ROW.

  DATA: TMPGRP TYPE LVC_T_GRPL, " For subtotal Selection .
       $TMPGRP TYPE LVC_S_GRPL.

  CALL METHOD G_GRID->GET_SUBTOTALS
          IMPORTING
            ET_GROUPLEVELS = TMPGRP.

* Selected Row by row selection ( Sub total )
  LOOP AT P_PT_ROWS WHERE NOT ROWTYPE IS INITIAL.
    READ TABLE TMPGRP INDEX P_PT_ROWS-INDEX INTO $TMPGRP.
    CHECK SY-SUBRC EQ 0 .

    LOOP AT GT_OUT FROM $TMPGRP-INDEX_FROM
                     TO $TMPGRP-INDEX_TO.
      GT_OUT-CHK = TRUE .
      MODIFY GT_OUT.
    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " SELECT_ROW_BY_SUBTOTAL
*&---------------------------------------------------------------------*
*&      Form  MONTH_INPUT_HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_HORM  text
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  MONTH_INPUT_HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM MONTH_INPUT_HELP CHANGING PF_MONTH.
  CALL FUNCTION 'Z_CO_MONTH_VIEW_BY_IG'
       IMPORTING
            SELECT_VALUE = PF_MONTH
       EXCEPTIONS
            OTHERS       = 1.
ENDFORM.                    " MONTH_INPUT_HELP
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MODIFY_SCREEN.

  LOOP AT SCREEN.
    CASE 'X'.
      WHEN P_ANNU.
        IF SCREEN-GROUP1 = 'ANU'.
          SCREEN-INPUT = 1.
          SCREEN-INVISIBLE  = 0.
        ENDIF.
        IF SCREEN-GROUP1 = 'QUA'.
          SCREEN-INPUT = 0.
          SCREEN-INVISIBLE  = 1.
        ENDIF.
      WHEN P_QUAR.
        IF SCREEN-GROUP1 = 'ANU'.
          SCREEN-INPUT = 0.
          SCREEN-INVISIBLE  = 1.
        ENDIF.
        IF SCREEN-GROUP1 = 'QUA'.
          SCREEN-INPUT = 1.
          SCREEN-INVISIBLE  = 0.
        ENDIF.
    ENDCASE.

    IF SCREEN-GROUP1 = 'EXL'.
      IF P_SR_E EQ 'X'.
        SCREEN-INPUT = 1.
        SCREEN-INVISIBLE  = 0.
      ELSE.
        SCREEN-INPUT = 0.
        SCREEN-INVISIBLE  = 1.
      ENDIF.
    ENDIF.
    IF SCREEN-GROUP1 = 'PLN'.
      IF P_SR_E EQ 'X'.
        SCREEN-INPUT = 0.
        SCREEN-INVISIBLE  = 1.
      ELSE.
        SCREEN-INPUT = 1.
        SCREEN-INVISIBLE  = 0.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.                    " MODIFY_SCREEN
*&---------------------------------------------------------------------*
*&      Form  YEAR_INPUT_HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_HORY  text
*----------------------------------------------------------------------*
FORM YEAR_INPUT_HELP CHANGING PF_YEAR.

  PERFORM GET_CUR_FIELD_VALUE USING P_HORY.
  CALL FUNCTION 'Z_CO_YEAR_VIEW_BY_IG'
       EXPORTING
            START_YEAR   = P_HORY
       IMPORTING
            SELECT_VALUE = PF_YEAR.

ENDFORM.                    " YEAR_INPUT_HELP
*&---------------------------------------------------------------------*
*&      Form  get_screen_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_CUR_FIELD_VALUE CHANGING PF_VALUE.
  DATA: BEGIN OF MF_DYNPFIELDS OCCURS 1.
          INCLUDE STRUCTURE DYNPREAD.
  DATA: END   OF MF_DYNPFIELDS.
  DATA: MF_RETURNCODE   LIKE SY-SUBRC,
        MF_MONAT        LIKE ISELLIST-MONTH,
        MF_HLP_REPID    LIKE SY-REPID.
  FIELD-SYMBOLS: <MF_FELD>.

  GET CURSOR FIELD MF_DYNPFIELDS-FIELDNAME.
  APPEND MF_DYNPFIELDS.
  MF_HLP_REPID = SY-REPID.
  DO 2 TIMES.
    CALL FUNCTION 'DYNP_VALUES_READ'
         EXPORTING
              DYNAME               = MF_HLP_REPID
              DYNUMB               = SY-DYNNR
         TABLES
              DYNPFIELDS           = MF_DYNPFIELDS
         EXCEPTIONS
              INVALID_ABAPWORKAREA = 01
              INVALID_DYNPROFIELD  = 02
              INVALID_DYNPRONAME   = 03
              INVALID_DYNPRONUMMER = 04
              INVALID_REQUEST      = 05
              NO_FIELDDESCRIPTION  = 06
              UNDEFIND_ERROR       = 07.
    IF SY-SUBRC = 3.
      MF_HLP_REPID = 'SAPLALDB'.
    ELSE.
      READ TABLE MF_DYNPFIELDS INDEX 1.
      TRANSLATE MF_DYNPFIELDS-FIELDVALUE USING '_ '.
      EXIT.
    ENDIF.
  ENDDO.

  IF SY-SUBRC = 0.
    PF_VALUE = MF_DYNPFIELDS-FIELDVALUE.
  ENDIF.

ENDFORM.                    " get_screen_value
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_FILE  text
*----------------------------------------------------------------------*
FORM UPLOAD_FILE USING FILENAME.

  IF P_FILE EQ SPACE.
    G_ERROR = TRUE.
    EXIT.
  ENDIF.

  DATA: IT_ITAB LIKE STANDARD TABLE OF ALSMEX_TABLINE WITH HEADER LINE.
  FIELD-SYMBOLS : <FS>.
  DATA : V_INDEX TYPE I.
  DATA : BEGIN_ROW TYPE I VALUE 1.

  __PROCESS ' ' '10'.
  IF P_HEAD = TRUE.
    ADD 1 TO BEGIN_ROW.
  ENDIF.

  IF P_TXT NE TRUE.
    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
         EXPORTING
              FILENAME                = FILENAME
              I_BEGIN_COL             = 1
              I_BEGIN_ROW             = BEGIN_ROW
              I_END_COL               = 14
              I_END_ROW               = 65535
         TABLES
              INTERN                  = IT_ITAB
         EXCEPTIONS
              INCONSISTENT_PARAMETERS = 1
              UPLOAD_OLE              = 2
              OTHERS                  = 3.

    IF SY-SUBRC NE 0.
      MESSAGE S000 WITH 'Could not find the file.'.
      STOP.
    ENDIF.

    __PROCESS ' ' '20'.

    IF IT_ITAB[] IS INITIAL.
      MESSAGE S003(ZZ) WITH 'No Data was uploaded'.
      G_ERROR = TRUE .
      EXIT.
    ELSE.
      SORT IT_ITAB BY ROW COL.
      LOOP AT IT_ITAB.
        MOVE : IT_ITAB-COL TO V_INDEX.
        ASSIGN COMPONENT V_INDEX OF STRUCTURE IT_FILE TO <FS>.
        MOVE : IT_ITAB-VALUE TO <FS>.
        AT END OF ROW.
          APPEND IT_FILE.
        ENDAT.
      ENDLOOP.
    ENDIF.
  ELSE.
    DATA CANCEL.
    CALL FUNCTION 'UPLOAD'
         EXPORTING
              FILENAME            = FILENAME
              FILETYPE            = 'DAT'
         IMPORTING
              CANCEL              = CANCEL
         TABLES
              DATA_TAB            = IT_FILE
         EXCEPTIONS
              CONVERSION_ERRO     = 1
              INVALID_TABLE_WIDTH = 2
              INVALID_TYPE        = 3.

    IF NOT CANCEL IS INITIAL OR SY-SUBRC NE 0.
      MESSAGE S003(ZZ) WITH 'No Data was uploaded'.
      STOP.
    ENDIF.

  ENDIF.

  DATA : L_FILENAME(40),
         L_NO(2)    TYPE  N,
         L_SPMON    TYPE  SPMON.

  LOOP AT IT_FILE.
    IF NOT IT_FILE-MATNR IN S_MATNR.
      DELETE IT_FILE.
      CONTINUE.
    ENDIF.
    DO 12 TIMES.
      L_NO = SY-INDEX.

      CONCATENATE 'IT_FILE-M' L_NO INTO L_FILENAME.
      ASSIGN (L_FILENAME) TO <MONTH>.
      PERFORM CHECK_NUM CHANGING <MONTH>.
    ENDDO.

    MODIFY IT_FILE.
  ENDLOOP.

ENDFORM.                    " upload_file
*&---------------------------------------------------------------------*
*&      Form  CHECK_NUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_<MF_FELD>  text
*----------------------------------------------------------------------*
FORM CHECK_NUM CHANGING N_VALUE.
  DATA NUM(12) VALUE ' 0123456789.'.

  REPLACE : '"' WITH '' INTO N_VALUE,
            '"' WITH '' INTO N_VALUE,
            ',' WITH '' INTO N_VALUE,
            ',' WITH '' INTO N_VALUE,
            ',' WITH '' INTO N_VALUE,
            ',' WITH '' INTO N_VALUE,
            ',' WITH '' INTO N_VALUE,
            ',' WITH '' INTO N_VALUE,
            ',' WITH '' INTO N_VALUE.
  CONDENSE N_VALUE NO-GAPS.
  IF N_VALUE CN NUM. N_VALUE = 0. ENDIF.
ENDFORM.                    " CHECK_NUM
*&---------------------------------------------------------------------*
*&      Form  GET_PLANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_PLANT.
  TYPES: BEGIN OF TY_BWKEY,
           BWKEY TYPE BWKEY,
         END OF TY_BWKEY.

  DATA   LT_BWKEY TYPE TABLE OF TY_BWKEY WITH HEADER LINE.

  __CLS : LT_BWKEY, R_BWKEY.

  SELECT BWKEY INTO TABLE LT_BWKEY
    FROM T001K
   WHERE BUKRS = P_BUKRS.

  R_BWKEY-SIGN   = 'I'.
  R_BWKEY-OPTION = 'EQ'.

  LOOP AT LT_BWKEY.
    R_BWKEY-LOW = LT_BWKEY-BWKEY.
    APPEND R_BWKEY.
  ENDLOOP.

  CLEAR R_BWKEY.
*
ENDFORM.                    " GET_PLANT
*&---------------------------------------------------------------------*
*&      Form  SET_WORKING_DATE_MONTHLY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_WORKING_DATE_MONTHLY.

  DATA : L_YYYYMM     TYPE   SPMON,
         L_YYYY(4)    TYPE   N,
         L_MM(2)      TYPE   N.

  PERFORM MAKE_IT_WERKS.

  __CLS IT_WORKDATE.

  LOOP AT IT_WERKS.
    MOVE IT_WERKS-WERKS TO IT_WORKDATE-WERKS.
    CONCATENATE P_HORY '01' INTO L_YYYYMM.
    IT_WORKDATE-SPMON = L_YYYYMM.
    APPEND IT_WORKDATE.

    DO 12 TIMES.
      L_YYYY = L_YYYYMM(4).
      L_MM   = L_YYYYMM+4(2).
      IF L_MM EQ '12'.
        L_YYYY = L_YYYY + 1.
        L_MM   = '00'.
        CONCATENATE L_YYYY L_MM INTO L_YYYYMM.
      ENDIF.
      L_YYYYMM = L_YYYYMM + 1.
      IF SY-INDEX NE 12.
        IT_WORKDATE-SPMON = L_YYYYMM.
        APPEND IT_WORKDATE.
      ENDIF.
    ENDDO.
    CLEAR IT_WORKDATE.
  ENDLOOP.


ENDFORM.                    " SET_WORKING_DATE_MONTHLY
*&---------------------------------------------------------------------*
*&      Form  MAKE_IT_WERKS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_IT_WERKS.
  __CLS IT_WERKS.

  SORT IT_FILE BY WERKS MATNR.
  DELETE IT_FILE WHERE WERKS EQ SPACE.
  LOOP AT IT_FILE.
    SELECT SINGLE *
                  FROM T001W
                  WHERE WERKS EQ IT_FILE-WERKS.
    IF SY-SUBRC EQ 0.
      MOVE IT_FILE-WERKS TO IT_WERKS-WERKS.
      APPEND IT_WERKS.
    ENDIF.
  ENDLOOP.
  SORT IT_WERKS.
  DELETE ADJACENT DUPLICATES FROM IT_WERKS.

ENDFORM.                    " MAKE_IT_WERKS
*&---------------------------------------------------------------------*
*&      Form  WORKING_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WORKING_DATE.
  DATA : L_TABIX     TYPE  SY-TABIX,
         L_DD(2)     TYPE  N.

  DATA : WA_FABKL          LIKE  T001W-FABKL. "Factory calendar key

  SORT IT_WORKDATE BY WERKS SPMON.

  LOOP AT IT_WORKDATE.
    L_TABIX = SY-TABIX.
    AT NEW WERKS.
      CLEAR WA_FABKL.
      SELECT SINGLE FABKL
                 INTO WA_FABKL
                 FROM T001W
                 WHERE WERKS EQ IT_WORKDATE-WERKS.
    ENDAT.
*    DO 31 TIMES.
*      L_DD = SY-INDEX.
*      CONCATENATE IT_WORKDATE-SPMON L_DD INTO IT_WORKDATE-DATUM.
*      CALL FUNCTION 'DATE_CHECK_WORKINGDAY'
*           EXPORTING
*                DATE                       = IT_WORKDATE-DATUM
*                FACTORY_CALENDAR_ID        = WA_FABKL
*                MESSAGE_TYPE               = 'E'
*           EXCEPTIONS
*                DATE_AFTER_RANGE           = 1
*                DATE_BEFORE_RANGE          = 2
*                DATE_INVALID               = 3
*                DATE_NO_WORKINGDAY         = 4
*                FACTORY_CALENDAR_NOT_FOUND = 5
*                MESSAGE_TYPE_INVALID       = 6
*                OTHERS                     = 7.
*      IF SY-SUBRC EQ 0.
*        EXIT.
*      ENDIF.
*    ENDDO.

    CONCATENATE IT_WORKDATE-SPMON '15' INTO IT_WORKDATE-DATUM.
    MODIFY IT_WORKDATE INDEX L_TABIX.
  ENDLOOP.

ENDFORM.                    " WORKING_DATE
*&---------------------------------------------------------------------*
*&      Form  GATHERING_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GATHERING_DATA.
  CHECK G_ERROR EQ SPACE.
  PERFORM BAPI_FORMATING.
ENDFORM.                    " GATHERING_DATA
*&---------------------------------------------------------------------*
*&      Form  UPLOADED_FORMATING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REFINE_DATA.
  DATA : L_FILENAME(40),
         L_NO(2)    TYPE  N,
         L_SPMON    TYPE  SPMON.

  DATA : $Q_START(2) TYPE N,
         $Q_END(2) TYPE N.

  IF P_QUAR EQ TRUE.
    $Q_START = P_HORM.
    $Q_END = P_HORM + 3.
  ELSE.
    $Q_START = '01'.
    $Q_END = '13'.
  ENDIF.

  CLEAR : IT_ITEM, IT_ITEM[].
  CLEAR : ERROR_IX.

  SORT IT_WORKDATE BY WERKS SPMON.

  LOOP AT IT_FILE.
*-----> Check configure Variant
    CLEAR MARA.
    SELECT SINGLE *
               FROM MARA
               WHERE MATNR EQ IT_FILE-MATNR
                 AND KZKFG EQ 'X'
                 AND MTART EQ 'FERT'.
    IF SY-SUBRC EQ 0.
      PERFORM CHECK_CONFIGURE_VARI USING IT_FILE-MATNR.
    ENDIF.

*-----> BAPI FORMATTING
    MOVE-CORRESPONDING IT_FILE TO IT_ITEM.
    MOVE : C_VERSB      TO  IT_ITEM-VERSB,   "Version
           WA_PBDNR     TO  IT_ITEM-PBDNR.   "Requirment plang No

    DO 12 TIMES.
      L_NO = SY-INDEX.
      CHECK : L_NO >= $Q_START, L_NO < $Q_END.

*-----> Planning date
      CONCATENATE  P_HORY L_NO INTO L_SPMON.
      READ TABLE IT_WORKDATE WITH KEY WERKS = IT_FILE-WERKS
                                      SPMON = L_SPMON
                                      BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        MOVE IT_WORKDATE-DATUM  TO  IT_ITEM-PDATU.
      ENDIF.

*-----> Planning Qtty
      CONCATENATE 'IT_FILE-M' L_NO INTO L_FILENAME.
      ASSIGN (L_FILENAME) TO <MONTH>.
      IT_ITEM-PLNMG = <MONTH>.
      APPEND IT_ITEM.
    ENDDO.
    CLEAR IT_ITEM.
  ENDLOOP.
ENDFORM.                    " UPLOADED_FORMATING
*&---------------------------------------------------------------------*
*&      Form  CHECK_CONFIGURE_VARI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FILE_MATNR  text
*----------------------------------------------------------------------*
FORM CHECK_CONFIGURE_VARI USING P_MATNR.
  DATA : L_ATNAM        TYPE   CABN-ATNAM,
         L_SYVAL_TABIX  TYPE   SY-TABIX.
  DATA : L_DATETIME(14) TYPE   N,
         L_VALFR        TYPE   V_IBINR-VALFR.
  DATA : L_CUOBJ        TYPE   MARC-CUOBJ.
  DATA : L_CNT          TYPE   SY-INDEX .

  CLEAR : IT_SYVAL, IT_SYVAL[], WA_IN_RECNO, WA_OBJNR.

  CONCATENATE 'MA' P_MATNR INTO WA_OBJNR.
  CONCATENATE SY-DATUM SY-UZEIT INTO L_DATETIME.
  L_VALFR = L_DATETIME.

  SELECT SINGLE CUOBJ
             INTO L_CUOBJ
             FROM MARC
             WHERE MATNR EQ P_MATNR
               AND WERKS EQ IT_FILE-WERKS.

  SELECT SINGLE IN_RECNO
            INTO WA_IN_RECNO
            FROM IBIN
            WHERE INSTANCE EQ L_CUOBJ
              AND VALFR <= L_VALFR
              AND VALTO > L_VALFR.

  IF SY-SUBRC EQ 0.
* IB: View from symbol and value
    SELECT ATINN
           ATWRT
           INTO TABLE IT_SYVAL
           FROM V_IBIN_SYVAL
           WHERE IN_RECNO EQ WA_IN_RECNO.

    IF SY-SUBRC EQ 0.
      CLEAR : IT_KSML, IT_KSML[], INOB, KSSK.
* Link between Internal Number and Object
      SELECT SINGLE *
                   FROM INOB
                   WHERE OBJEK EQ P_MATNR.

* Allocation Table: Object to Class
      SELECT SINGLE *
                   FROM KSSK
                   WHERE OBJEK EQ INOB-CUOBJ.

* Characteristics of a Class
      SELECT *
             INTO TABLE IT_KSML
             FROM KSML
             WHERE CLINT EQ KSSK-CLINT.

      IF SY-SUBRC EQ 0.
        SORT IT_KSML BY POSNR ASCENDING ADZHL DESCENDING.
        DATA : L_IMERK  LIKE  KSML-IMERK.
        LOOP AT IT_KSML.
          MOVE IT_KSML-IMERK   TO  L_IMERK.
          AT NEW POSNR.
            L_CNT = L_CNT + 1.
            IF L_CNT LE 7.
              READ TABLE IT_SYVAL WITH KEY ATINN = L_IMERK.
              IF SY-SUBRC NE 0.
                ERROR_IX = ERROR_IX + 1.
                MOVE WA_PBDNR      TO IT_ERROR-PBDNR.
                MOVE P_MATNR       TO IT_ERROR-MATNR.
                MOVE 'E'           TO IT_ERROR-MSGTY.
                MOVE TEXT-301      TO IT_ERROR-MSG.
                APPEND IT_ERROR.
                CLEAR IT_ERROR.
                EXIT.
              ELSE.
                L_SYVAL_TABIX = SY-TABIX.
                CLEAR L_ATNAM.
                SELECT SINGLE ATNAM
                            INTO L_ATNAM
                            FROM CABN
                            WHERE ATINN EQ IT_SYVAL-ATINN.
             CONCATENATE L_ATNAM '/' IT_SYVAL-ATWRT INTO IT_SYVAL-ATWRT.
                MODIFY IT_SYVAL INDEX L_SYVAL_TABIX.
              ENDIF.
            ENDIF.
          ENDAT.
        ENDLOOP.
      ELSE.
        ERROR_IX = ERROR_IX + 1.
        MOVE WA_PBDNR      TO IT_ERROR-PBDNR.
        MOVE P_MATNR       TO IT_ERROR-MATNR.
        MOVE 'E'           TO IT_ERROR-MSGTY.
        MOVE TEXT-302      TO IT_ERROR-MSG.
        APPEND IT_ERROR.
        CLEAR IT_ERROR.
      ENDIF.
    ELSE.
      ERROR_IX = ERROR_IX + 1.
      MOVE WA_PBDNR      TO IT_ERROR-PBDNR.
      MOVE P_MATNR       TO IT_ERROR-MATNR.
      MOVE 'E'           TO IT_ERROR-MSGTY.
      MOVE TEXT-302      TO IT_ERROR-MSG.
      APPEND IT_ERROR.
      CLEAR IT_ERROR.
    ENDIF.
  ELSE.
    ERROR_IX = ERROR_IX + 1.
    MOVE WA_PBDNR      TO IT_ERROR-PBDNR.
    MOVE P_MATNR       TO IT_ERROR-MATNR.
    MOVE 'E'           TO IT_ERROR-MSGTY.
    MOVE TEXT-301      TO IT_ERROR-MSG.
    APPEND IT_ERROR.
    CLEAR IT_ERROR.
  ENDIF.

ENDFORM.                    " CHECK_CONFIGURE_VARI
*&---------------------------------------------------------------------*
*&      Form  BAPI_FORMATING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BAPI_FORMATING.
  DATA : L_FLG.

  CLEAR : IT_HEADMATNR, IT_HEADMATNR[],
          IT_HEADITEM,  IT_HEADITEM[],
          IT_ERROR,     IT_ERROR[],
          ERROR_IX,  WA_SUCCESS_IX.

* get production version {

  DATA : BEGIN OF $VERID OCCURS 0,
           WERKS     TYPE WERKS_D,
           MATNR     TYPE MATNR,
           BDATU     TYPE BDATM,
           VERID     TYPE VERID,
         END   OF $VERID.

  CHECK NOT IT_ITEM[] IS INITIAL.

  SELECT WERKS MATNR BDATU VERID  INTO TABLE $VERID
  FROM MKAL
  FOR ALL ENTRIES IN IT_ITEM
  WHERE MATNR EQ IT_ITEM-MATNR
    AND WERKS EQ IT_ITEM-WERKS
    AND BDATU GT SY-DATUM
    AND ADATU LT SY-DATUM.

  SORT $VERID BY WERKS MATNR BDATU ASCENDING
                 VERID DESCENDING.

  DELETE ADJACENT DUPLICATES FROM $VERID COMPARING WERKS MATNR.

* }

  LOOP AT IT_ITEM .
    MOVE-CORRESPONDING IT_ITEM TO IT_HEADMATNR.
    MOVE-CORRESPONDING IT_ITEM TO IT_HEADITEM.
*    CONCATENATE : IT_ITEM-PDATU(6) '01' INTO IT_HEADITEM-PDATU.
*---> First working date in Month
    READ TABLE IT_WORKDATE WITH KEY SPMON = IT_ITEM-PDATU(6).
    IF SY-SUBRC EQ 0.
      MOVE IT_WORKDATE-DATUM TO IT_HEADITEM-PDATU.
    ENDIF.

*---> Req.plan number
*    MOVE : WA_PBDNR TO  IT_HEADMATNR-PBDNR,
*           WA_PBDNR TO  IT_HEADITEM-PBDNR.
*

    READ TABLE $VERID WITH KEY WERKS = IT_ITEM-WERKS
                               MATNR = IT_ITEM-MATNR
                               BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_HEADITEM-PVER = $VERID-VERID.
    ENDIF.

*---> Version
    MOVE C_VERSB  TO  : IT_HEADMATNR-VERSB,
                        IT_HEADITEM-VERSB.

    MOVE  P_PLSCN TO  :  IT_HEADMATNR-PBDNR,IT_HEADITEM-PBDNR.
*    CLEAR :  IT_HEADMATNR-PBDNR,IT_HEADITEM-PBDNR.

    COLLECT : IT_HEADMATNR,
              IT_HEADITEM.
    CLEAR : IT_HEADMATNR, IT_HEADITEM.
  ENDLOOP.

  SORT IT_HEADMATNR  BY WERKS VERSB PBDNR MATNR.
  SORT IT_HEADITEM   BY WERKS VERSB PBDNR MATNR PDATU.

  LOOP AT IT_HEADMATNR.
    CLEAR : MARC, MAST, MAPL, PLKO.
*---> Check existence of material in plant
    SELECT SINGLE *
                  FROM MARC
                  WHERE WERKS EQ IT_HEADMATNR-WERKS
                    AND MATNR EQ IT_HEADMATNR-MATNR.
    IF SY-SUBRC EQ 0.
*---> Check existence of material's BOM in plant
      SELECT SINGLE *
                    FROM MAST
                    WHERE MATNR EQ IT_HEADMATNR-MATNR
                      AND WERKS EQ IT_HEADMATNR-WERKS
                      AND STLAN EQ '6'.   "Usage
      IF SY-SUBRC EQ 0.
*----> Check existence of material's Rate routing in plant
        SELECT *
               FROM MAPL
               WHERE MATNR EQ IT_HEADMATNR-MATNR
                 AND WERKS EQ IT_HEADMATNR-WERKS.
          SELECT SINGLE *
                   FROM PLKO
                   WHERE  PLNNR  EQ  MAPL-PLNNR
                     AND  PLNAL  EQ  MAPL-PLNAL
                     AND  VERWE  EQ  '10'.  "costing routing
          IF SY-SUBRC EQ 0.
            L_FLG = 'X'.
            EXIT.
          ENDIF.
        ENDSELECT.

        IF L_FLG EQ SPACE.
          ERROR_IX = ERROR_IX + 1.
          MOVE-CORRESPONDING IT_HEADMATNR TO IT_ERROR.
          MOVE 'E'  TO  IT_ERROR-MSGTY.
          MOVE TEXT-303 TO IT_ERROR-MSG.
          APPEND IT_ERROR.
          CLEAR IT_ERROR.
        ENDIF.

      ELSE.
        ERROR_IX = ERROR_IX + 1.
        MOVE-CORRESPONDING IT_HEADMATNR TO IT_ERROR.
        MOVE 'E'  TO  IT_ERROR-MSGTY.
        MOVE TEXT-304 TO IT_ERROR-MSG.
        APPEND IT_ERROR.
        CLEAR IT_ERROR.
      ENDIF.
    ELSE.
      ERROR_IX = ERROR_IX + 1.
      MOVE-CORRESPONDING IT_HEADMATNR TO IT_ERROR.
      MOVE 'E'  TO  IT_ERROR-MSGTY.
      MOVE TEXT-305 TO IT_ERROR-MSG.
      APPEND IT_ERROR.
      CLEAR IT_ERROR.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " BAPI_FORMATING
*&---------------------------------------------------------------------*
*&      Form  BDC_EXECUTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BDC_EXECUTION.

* Existent all datas cannot be erased by t-code :MD74, 75, 76
* but can be erased by BDC (T-CODE:MD62)
* Only, delete processing by BDC does hard coding of PLANT
* because is processed to plant

  DATA : BEGIN OF IT_WERKS OCCURS 0,
          WERKS TYPE WERKS_D,
         END OF IT_WERKS.

  DATA L_BDZEI   TYPE   PBED-BDZEI.
  DATA $WERKS TYPE WERKS_D.
  DATA L_MSG  LIKE CFGNL-MSGLIN.

  LOOP AT IT_FILE.
    IT_WERKS-WERKS = IT_FILE-WERKS.
    COLLECT IT_WERKS.
  ENDLOOP.

*  LOOP AT IT_WERKS.
*    SELECT SINGLE A~BDZEI
*                 INTO L_BDZEI
*                 FROM PBIM AS A INNER JOIN PBED AS B
*                   ON A~BDZEI EQ B~BDZEI
*                 WHERE A~WERKS EQ IT_WERKS-WERKS
*                   AND A~VERSB EQ C_VERSB.
*
*    IF SY-SUBRC EQ 0.
*      __PROCESS 'Delete existing data...' '70'.
*      PERFORM GENERATE_BDC_MD62 USING IT_WERKS-WERKS.
*      CALL TRANSACTION 'MD62' USING IT_BDCDATA
*                              OPTIONS FROM WA_OPTION_DS.
*      __CLS IT_BDCDATA.
*
*      CALL FUNCTION 'RKC_MSG_STRING'
*           EXPORTING
*                ID      = SY-MSGID
*                MTYPE   = SY-MSGTY
*                NUMBER  = SY-MSGNO
*                PAR1    = SY-MSGV1
*                PAR2    = SY-MSGV2
*                PAR3    = SY-MSGV3
*                PAR4    = SY-MSGV4
*           IMPORTING
*                MSG_LIN = L_MSG
*           EXCEPTIONS
*                OTHERS  = 1.
*
*      CASE SY-MSGTY.
*        WHEN 'E' OR 'A' OR 'X' OR 'W'.
*          ERROR_IX = ERROR_IX + 1.
*          IT_ERROR-MSGTY = 'E'.
*          IT_ERROR-MSG   = L_MSG.
*          APPEND IT_ERROR. CLEAR IT_ERROR.
*      ENDCASE.
*
*    ENDIF.
*  ENDLOOP.

  PERFORM CALL_BAPI.

ENDFORM.                    " BDC_EXECUTION
*&---------------------------------------------------------------------*
*&      Form  GENERATE_BDC_MD62
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2690   text
*----------------------------------------------------------------------*
FORM GENERATE_BDC_MD62 USING P_WERKS.
  PERFORM BDC_DYNPRO  USING 'SAPMM60X'	'0106'.
  PERFORM BDC_FIELD   USING :
	    'BDC_CURSOR'	'RM60X-DATVE',
	    'BDC_OKCODE'	'/00',
	    'AM60X-PBDAW'	C_MARK,
	    'AM60X-PBDNR'	C_VERSB,   "Y1
	    'RM60X-BERID'	P_WERKS,
	    'AM60X-WERKS'	P_WERKS,
	    'AM60X-VERAW'    C_MARK,
	    'RM60X-VERSB'	C_VERSB,   "Y1
	    'RM60X-ENTLU'	C_MONTH.

  PERFORM BDC_DYNPRO  USING 'SAPLM60E'	'0200'.
  PERFORM BDC_FIELD   USING :
	    'BDC_OKCODE'	'=ALMK'.

  PERFORM BDC_DYNPRO  USING 'SAPLM60E'	'0200'.
  PERFORM BDC_FIELD   USING :
	    'BDC_OKCODE'	'=POLO'.

  PERFORM BDC_DYNPRO  USING 'SAPLSPO1'	'0500'.
  PERFORM BDC_FIELD   USING :
	    'BDC_OKCODE'	'=OPT1'.

  PERFORM BDC_DYNPRO  USING 'SAPLM60E'	'0200'.
  PERFORM BDC_FIELD   USING :       		
	    'BDC_OKCODE'	'=SICH'.

ENDFORM.                    " GENERATE_BDC_MD62
*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2757   text
*      -->P_2758   text
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING  P_PROGRAM
                       P_DYNPRO.
  CLEAR IT_BDCDATA.
  IT_BDCDATA-PROGRAM  = P_PROGRAM.
  IT_BDCDATA-DYNPRO   = P_DYNPRO.
  IT_BDCDATA-DYNBEGIN = 'X'.
  APPEND IT_BDCDATA.

ENDFORM.                    " BDC_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2762   text
*      -->P_2763   text
*----------------------------------------------------------------------*
FORM BDC_FIELD USING    P_FNAM
                        P_FVAL.
  CLEAR IT_BDCDATA.
  IT_BDCDATA-FNAM = P_FNAM.
  IT_BDCDATA-FVAL = P_FVAL.
  APPEND IT_BDCDATA.

ENDFORM.                    " BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  ERROR_TEXT_MD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_IND  text
*----------------------------------------------------------------------*
FORM ERROR_TEXT_MD USING P_IND.
  DATA L_MSG  LIKE CFGNL-MSGLIN.
  __CLS IT_BDCDATA.

  CALL FUNCTION 'RKC_MSG_STRING'
       EXPORTING
            ID      = SY-MSGID
            MTYPE   = SY-MSGTY
            NUMBER  = SY-MSGNO
            PAR1    = SY-MSGV1
            PAR2    = SY-MSGV2
            PAR3    = SY-MSGV3
            PAR4    = SY-MSGV4
       IMPORTING
            MSG_LIN = L_MSG
       EXCEPTIONS
            OTHERS  = 1.

  CASE SY-MSGTY.
    WHEN 'E' OR 'A' OR 'X' OR 'W'.
      ERROR_IX = ERROR_IX + 1.
      IT_ERROR-MSGTY = 'E'.
      IT_ERROR-MSG   = L_MSG.
      APPEND IT_ERROR. CLEAR IT_ERROR.
    WHEN OTHERS.   " 'I', 'S' :SUCCESS
      CASE P_IND.
        WHEN 'MD74'.
* Delete Old Requirements Records - transaction ID 'MD75'
          PERFORM GENERATE_BDC_DATA USING 'MD75'.
          PERFORM CALL_TRANSACTION USING 'MD75'.
        WHEN 'MD75'.
* Delete History and Independent Requirements - transaction ID 'MD76'
          PERFORM GENERATE_BDC_DATA USING 'MD76'.
          PERFORM CALL_TRANSACTION USING 'MD76'.
        WHEN 'MD76'.
          CLEAR ERROR_IX.
          PERFORM CALL_BAPI.
*        WHEN 'P001'.
*          DATA L_BDZEI   TYPE   PBED-BDZEI.
*          SELECT SINGLE A~BDZEI
*                       INTO L_BDZEI
*                       FROM PBIM AS A INNER JOIN PBED AS B
*                         ON A~BDZEI EQ B~BDZEI
*                       WHERE A~WERKS EQ 'E001'    "PLANT
*                         AND A~VERSB EQ C_VERSB   "VERSION 'Q1'
*                         AND A~PBDNR EQ C_VERSB.  "REQ PLAN No
*          IF SY-SUBRC EQ 0.
*            PERFORM GENERATE_BDC_MD62 USING 'E001'.
*            PERFORM CALL_TRANSACTION_MD62 USING 'E001'.
*          ELSE.
*            PERFORM CALL_BAPI.
*          ENDIF.
*        WHEN 'E001'.
*          PERFORM CALL_BAPI.
      ENDCASE.
  ENDCASE.
  CLEAR IT_ERROR.
ENDFORM.                    " ERROR_TEXT_MD
*&---------------------------------------------------------------------*
*&      Form  GENERATE_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2974   text
*----------------------------------------------------------------------*
FORM GENERATE_BDC_DATA USING P_IND.
  DATA : L_KEYDATE(10).

  WRITE C_KEYDATE TO L_KEYDATE.
  CASE P_IND.
    WHEN 'MD74'.
* Adjusting Requirements (Plnd Ind Req) - transaction ID 'MD74'
      CLEAR : IT_BDCDATA, IT_BDCDATA[].
      PERFORM BDC_DYNPRO  USING 'RM60RR20' '1000'.
      PERFORM BDC_FIELD   USING :
          'BDC_CURSOR'      'PBDNR-LOW',
          'BDC_OKCODE'      '=ONLI',
*          'WERKS-LOW'	    P_WERKS,      "PLANT
*          'BEDAE-LOW'	    C_REQTY,      "Requirements type (VSF)
          'VERSB-LOW'	    C_VERSB,      "VERSION
*          'PBDNR-LOW'	    WA_PBDNR,     "Requirements plan number
          'HISTFLAG'	    C_MARK,       "
          'INACFLAG'	    C_MARK,       "
          'DATE1'           L_KEYDATE,    "Key date
          'TESTFLAG'	    SPACE.
    WHEN 'MD75'.
* Delete Old Requirements Records - transaction ID 'MD75'
      PERFORM BDC_DYNPRO  USING 'RM60RR30' '1000'.
      PERFORM BDC_FIELD   USING :
    	    'BDC_CURSOR'	    'PBDNR-LOW',
    	    'BDC_OKCODE'	    '=ONLI',
*    	    'WERKS-LOW'	    P_WERKS,   "Plant
*    	    'BEDAE-LOW'	    C_REQTY,   "Requirements type(VSF)
    	    'VERSB-LOW'	    C_VERSB,   "VERSION
*    	    'PBDNR-LOW'	    WA_PBDNR,  "Requirements plan number
          'DATE1'           L_KEYDATE, "Key date
    	    'TESTFLAG'	    SPACE.
    WHEN 'MD76'.
* Delete History and Independent Requirements - transaction ID 'MD76'
      PERFORM BDC_DYNPRO  USING 'RM60RR40' '1000'.
      PERFORM BDC_FIELD   USING :
    	    'BDC_CURSOR'	   'PBDNR-LOW',
    	    'BDC_OKCODE'	   '=ONLI',
*    	    'WERKS-LOW'	   P_WERKS,    "Plant
*    	    'BEDAE-LOW'  	   C_REQTY,    "Requirements type(VSF)
    	    'VERSB-LOW'	   C_VERSB,    "VERSION
*    	    'PBDNR-LOW'	   WA_PBDNR,   "Requirements plan number
          'HDATE'          L_KEYDATE,  "Key date
    	    'TESTFLAG'       SPACE.
  ENDCASE.
ENDFORM.                    " GENERATE_BDC_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2978   text
*----------------------------------------------------------------------*
FORM CALL_TRANSACTION USING  P_IND.
  CASE P_IND.
    WHEN 'MD74'.
* Adjusting Requirements (Plnd Ind Req) - transaction ID 'MD74'
      CALL TRANSACTION 'MD74' USING IT_BDCDATA
                          OPTIONS FROM WA_OPTION_DS.
      PERFORM ERROR_TEXT_MD USING P_IND.

    WHEN 'MD75'.
* Delete Old Requirements Records - transaction ID 'MD75'
      CALL TRANSACTION 'MD75' USING IT_BDCDATA
                          OPTIONS FROM WA_OPTION_DS.
      PERFORM ERROR_TEXT_MD USING P_IND.

    WHEN 'MD76'.
* Delete History and Independent Requirements - transaction ID 'MD76'
      CALL TRANSACTION 'MD76' USING IT_BDCDATA
                          OPTIONS FROM WA_OPTION_DS.
      PERFORM ERROR_TEXT_MD USING P_IND.
  ENDCASE.
ENDFORM.                    " CALL_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  GENERATE_BAPI_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GENERATE_BAPI_DATA.

  CLEAR : BAPISITEMR, CM60R, MARA,
          IT_BAPISSHDIN, IT_BAPISSHDIN[],
          IT_BAPISCHARR, IT_BAPISCHARR[],
          IT_BAPIRETURN, IT_BAPIRETURN[].

*----> Check Fert or others
  CLEAR MARA.
  SELECT SINGLE *
               FROM MARA
               WHERE MATNR EQ IT_HEADMATNR-MATNR
                 AND KZKFG EQ 'X'
                 AND MTART EQ 'FERT'.
  IF SY-SUBRC EQ 0.
*-----> CHECK COLOR
    PERFORM CHECK_COLOR_FOR_MATNR.

*-----> Check configure variant
    PERFORM CHECK_CONFIGURE_VARI USING IT_HEADMATNR-MATNR.

  ENDIF.

*----> GENERATE BAPISITEMR
  BAPISITEMR-MATERIAL   = IT_HEADMATNR-MATNR. "FSC
  BAPISITEMR-PLANT      = IT_HEADMATNR-WERKS. "PLANT
  CLEAR : MARC, WA_BEDAE.
*  SELECT SINGLE *
*                FROM MARC
*                WHERE WERKS EQ IT_HEADMATNR-WERKS
*                  AND MATNR EQ IT_HEADMATNR-MATNR.
*  IF MARC-STRGR EQ '56'.
*    WA_BEDAE = C_REQTY56.
*    BAPISITEMR-REQU_TYPE  = C_REQTY56.            "VSE
*  ELSE.
*    WA_BEDAE = C_REQTYOT.
*    BAPISITEMR-REQU_TYPE  = C_REQTYOT.            "VSF
*  ENDIF.
  BAPISITEMR-VERSION    = IT_HEADMATNR-VERSB. "VERSION
  BAPISITEMR-VERS_ACTIV = SPACE.              "ACTIVE Yes/No
  BAPISITEMR-REQ_NUMBER = IT_HEADMATNR-PBDNR. "Req plan No

  LOOP AT IT_HEADITEM WHERE WERKS EQ IT_HEADMATNR-WERKS "PLANT
                        AND VERSB EQ IT_HEADMATNR-VERSB "VERSION
                        AND PBDNR EQ IT_HEADMATNR-PBDNR "REQ PLAN No
                        AND MATNR EQ IT_HEADMATNR-MATNR. "Material


*----> GENERATE SCHEDULE LINE
    IT_BAPISSHDIN-DATE_TYPE  = '2'. "DATE TYPE ( '2':WEEK)
    IT_BAPISSHDIN-REQ_DATE   = IT_HEADITEM-PDATU.   "DATE
    IT_BAPISSHDIN-REQ_QTY    = IT_HEADITEM-PLNMG.   "QTY
    IT_BAPISSHDIN-UNIT       = MARA-MEINS.          "UNIT
*   IT_BAPISSHDIN-PROD_VES   = IT_HEADITEM-PVER.    "PROD VERSION
    APPEND IT_BAPISSHDIN.
    CLEAR IT_BAPISSHDIN.

*----> GENERATE COLOR CHARACTERISTICS
    IF MARA-KZKFG EQ 'X'.
      LOOP AT IT_SYVAL.
        READ TABLE IT_COLOR  WITH KEY VALC = IT_SYVAL-ATWRT.
        IT_BAPISCHARR-REQU_DATE  = IT_HEADITEM-PDATU.
        IT_BAPISCHARR-INT_CHAR   = IT_COLOR-VTINT.
        IT_BAPISCHARR-CHAR_VALUE = IT_COLOR-SLNID.
        IT_BAPISCHARR-CH_QTY     = IT_HEADITEM-PLNMG.
        IT_BAPISCHARR-FIXING     = 'X'.
        IT_BAPISCHARR-COPY_FRMED = 'X'.
        IT_BAPISCHARR-FLAG_USAGE = 'X'.
        APPEND IT_BAPISCHARR.
        CLEAR IT_BAPISCHARR.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  SORT : IT_BAPISSHDIN BY DATE_TYPE REQ_DATE,
         IT_BAPISCHARR BY REQU_DATE INT_CHAR CHAR_VALUE.

  DATA L_BDZEI LIKE  PBIM-BDZEI.
  CLEAR L_BDZEI.
  SELECT SINGLE A~BDZEI
               INTO L_BDZEI
               FROM PBIM AS A INNER JOIN PBED AS B
                 ON A~BDZEI EQ B~BDZEI
               WHERE A~WERKS EQ IT_HEADMATNR-WERKS  "PLANT
                 AND A~MATNR EQ IT_HEADMATNR-MATNR  "Material
                 AND A~VERSB EQ IT_HEADMATNR-VERSB.  "VERSION WA_VERSB

  IF SY-SUBRC EQ 0.
    CALL FUNCTION 'BAPI_REQUIREMENTS_CHANGE'
         EXPORTING
              MATERIAL                 = BAPISITEMR-MATERIAL
              PLANT                    = BAPISITEMR-PLANT
              REQUIREMENTSTYPE         = BAPISITEMR-REQU_TYPE
              VERSION                  = BAPISITEMR-VERSION
              REQMTSPLANNUMBER         = BAPISITEMR-REQ_NUMBER
              VERS_ACTIV               = BAPISITEMR-VERS_ACTIV
         TABLES
              REQUIREMENTS_SCHEDULE_IN = IT_BAPISSHDIN
              REQUIREMENTS_CHAR_IN     = IT_BAPISCHARR
              RETURN                   = IT_BAPIRETURN.
  ELSE.
    CALL FUNCTION 'BAPI_REQUIREMENTS_CREATE'
         EXPORTING
              REQUIREMENTS_ITEM        = BAPISITEMR
         TABLES
              REQUIREMENTS_SCHEDULE_IN = IT_BAPISSHDIN
              REQUIREMENTS_CHAR_IN     = IT_BAPISCHARR
              RETURN                   = IT_BAPIRETURN.

  ENDIF.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

  IF IT_BAPIRETURN[] IS INITIAL.
    WA_SUCCESS_IX = WA_SUCCESS_IX + 1.
  ELSE.
    LOOP AT IT_BAPIRETURN WHERE TYPE NE 'S'.
      ERROR_IX = ERROR_IX + 1.
      MOVE-CORRESPONDING IT_HEADITEM TO IT_ERROR.
      MOVE IT_BAPIRETURN-TYPE        TO IT_ERROR-MSGTY.
      MOVE IT_BAPIRETURN-MESSAGE     TO IT_ERROR-MSG.
      APPEND IT_ERROR.
      CLEAR IT_ERROR.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " GENERATE_BAPI_DATA
*&---------------------------------------------------------------------*
*&      Form  CHECK_COLOR_FOR_MATNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_COLOR_FOR_MATNR.

  DATA: WA_OBJEK    LIKE    INOB-OBJEK.
  DATA: WA_VALUES   TYPE    M60VT_PROFIL.

  DATA: IT_TOTAL    TYPE  TY_TOTAL OCCURS 0 WITH HEADER LINE.
  DATA: IT_RETURN   LIKE  TABLE OF RM60CUVT WITH HEADER LINE.
  DATA: IT_PHWA     LIKE  TPHVP.
  DATA: IT_PLWA     LIKE  TPLVP.
  DATA: IT_PSWA     LIKE  TPSVP.

  DATA: BEGIN OF MAINT_CHAR_REL OCCURS 0.
          INCLUDE STRUCTURE RM60REL.
  DATA: END OF   MAINT_CHAR_REL.

  DATA: BEGIN OF MAINT_PROFIL OCCURS   0.
          INCLUDE STRUCTURE RM60PHVP.
  DATA:   END OF MAINT_PROFIL.

  DATA: L_PL_REL    TYPE  TPSVP-PL_REL.

*  CLEAR: WA_VALUES.
  CLEAR : WA_OBJEK.
  WA_OBJEK = IT_HEADMATNR-MATNR.
  CALL FUNCTION 'M60V_PROFIL_FOR_PLAN'
       EXPORTING
            OBJEKT      =   WA_OBJEK  "WORK_INOB-ROBJEK
*           PROFILID    =
            I_PL_REL    = ' '
            BUFFER_FREE = 'X'
*            KEY_DATE    = SY-DATUM
       IMPORTING
            EXP_VALUE   = WA_VALUES
*      tables
*           tab_phvp    = tplvp_value
       EXCEPTIONS
            NOT_FOUND   = 1
            OTHERS      = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT WA_VALUES-HEADR INTO IT_PHWA.
    MOVE-CORRESPONDING IT_PHWA TO MAINT_PROFIL.
    MOVE 'X' TO MAINT_PROFIL-DBVKZ.
    APPEND MAINT_PROFIL.
  ENDLOOP.

  LOOP AT WA_VALUES-GROUP INTO IT_PLWA.
    MOVE-CORRESPONDING IT_PLWA TO IT_TOTAL.
    IF NOT IT_PLWA-LKENZ IS INITIAL.
      MOVE 'D' TO IT_TOTAL-UPDKZ.
    ELSE.
      MOVE SPACE TO IT_TOTAL-UPDKZ.
    ENDIF.
    MOVE 'X' TO IT_TOTAL-DBVKZ.
    APPEND IT_TOTAL.
  ENDLOOP.

  REFRESH: IT_COLOR, IT_RETURN.
  LOOP AT IT_TOTAL.
    REFRESH : IT_RETURN.
    CALL FUNCTION 'M60V_COMBINATION_DISPLAY'
         EXPORTING
              TABLE_LINE   = '00000'
              TABLE_NUMBER = IT_TOTAL-CLINT
         TABLES
              TAB_VAR      = IT_RETURN.
    LOOP AT IT_RETURN.
      CLEAR L_PL_REL.
      SELECT SINGLE PL_REL
                    INTO L_PL_REL
                    FROM TPSVP
                    WHERE PROFILID   EQ IT_TOTAL-PROFILID
                      AND PHCOUNTER  EQ IT_TOTAL-PHCOUNTER
                      AND CLINT      EQ IT_TOTAL-CLINT
                      AND LNPOS      EQ IT_RETURN-SLNID.
      IF SY-SUBRC EQ 0 AND L_PL_REL EQ C_MARK.
        MOVE-CORRESPONDING IT_RETURN  TO  IT_COLOR.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_OUTPUT'
             EXPORTING
                  INPUT  = IT_COLOR-ATINN
             IMPORTING
                  OUTPUT = IT_COLOR-VTNAM.

        APPEND IT_COLOR.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " CHECK_COLOR_FOR_MATNR
*&---------------------------------------------------------------------*
*&      Form  SET_MODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_MODE.
*----> SET BDC MODE OPTION
  CLEAR : WA_OPTION_DS.
  WA_OPTION_DS-DISMODE = C_MODE.
  WA_OPTION_DS-DEFSIZE = 'X'.
  WA_OPTION_DS-UPDMODE = 'S'.

ENDFORM.                    " SET_MODE
*&---------------------------------------------------------------------*
*&      Form  SET_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_OUTPUT.
  CHECK G_ERROR EQ SPACE .
  CALL SCREEN 100.

ENDFORM.                    " SET_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SWITCH_EDIT_MODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SWITCH_EDIT_MODE.

  DATA ANSWER.
  IF G_GRID->IS_READY_FOR_INPUT( ) EQ 0.
    CALL METHOD G_GRID->SET_READY_FOR_INPUT
                     EXPORTING I_READY_FOR_INPUT = 1.
    PERFORM STATUS_CHANGE.
    PERFORM INFO_TEXT_SET USING TRUE.
  ELSE.
    IF FLAG_DATA_CHANGED EQ TRUE.
      CALL FUNCTION 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
           EXPORTING
                TEXTLINE1     = 'Data has not been saved yet.'
                TEXTLINE2     = 'Do you want to continue anyway? '
                TITEL         = 'Confirmation'
                DEFAULTOPTION = 'N'
           IMPORTING
                ANSWER        = ANSWER.
      CHECK ANSWER EQ 'J'.
    ENDIF.
    CLEAR FLAG_DATA_CHANGED.
    CALL METHOD G_GRID->SET_READY_FOR_INPUT
                     EXPORTING I_READY_FOR_INPUT = 0.
    PERFORM STATUS_CHANGE.
    PERFORM INFO_TEXT_SET USING FALSE.
  ENDIF.

ENDFORM.                    " SWITCH_EDIT_MODE
*&---------------------------------------------------------------------*
*&      Form  INFO_TEXT_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TRUE  text
*----------------------------------------------------------------------*
FORM INFO_TEXT_SET USING P_TRUE.

  IF P_TRUE EQ TRUE.
    INFO = TEXT-015.
  ELSE.
    INFO = TEXT-015.
  ENDIF.

ENDFORM.                    " info_text_set
*&---------------------------------------------------------------------*
*&      Form  saving
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROC.

  DATA: LT_ROWS TYPE LVC_T_ROW WITH HEADER LINE,
        LT_ROW_NO TYPE LVC_T_ROID. "Numeric IDs of Selected Rows
  DATA: L_LINE TYPE I.

  CALL METHOD G_GRID->GET_SELECTED_ROWS
           IMPORTING ET_INDEX_ROWS = LT_ROWS[]
                     ET_ROW_NO     = LT_ROW_NO.

  CALL METHOD CL_GUI_CFW=>FLUSH.

  IF SY-SUBRC NE 0.
    MESSAGE E000
    WITH 'Error founded during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  READ TABLE LT_ROWS INDEX 1.
  IF SY-SUBRC NE 0.
    MESSAGE E000 WITH 'Please select a data.'.
    EXIT.
  ENDIF.

  PERFORM GET_SELECTED_DATA TABLES LT_ROWS
                                   LT_ROW_NO .

  READ TABLE IT_SEL INDEX 1.

  IF SY-SUBRC EQ 0.
    __CLS IT_FILE.
    LOOP AT IT_SEL.
      MOVE-CORRESPONDING IT_SEL TO IT_FILE .
      APPEND IT_FILE.
    ENDLOOP.
    PERFORM REFINE_DATA.
    PERFORM GATHERING_DATA.
    PERFORM CALL_BAPI.
*
*    PERFORM BDC_EXECUTION.
  ENDIF.


ENDFORM.                    " saving
*&---------------------------------------------------------------------*
*&      Form  GET_SELECTED_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ROWS  text
*      -->P_LT_ROW_NO  text
*----------------------------------------------------------------------*
FORM GET_SELECTED_DATA TABLES PT_ROWS STRUCTURE LVC_S_ROW
                             PT_ROW_NO STRUCTURE LVC_S_ROID.

  __CLS IT_SEL .

  CLEAR GT_OUT-CHK.
  MODIFY GT_OUT TRANSPORTING CHK WHERE CHK = 'X'.

* Selected Row by row selection
  LOOP AT PT_ROWS WHERE ROWTYPE IS INITIAL.
    READ TABLE GT_OUT INDEX PT_ROWS-INDEX.
    GT_OUT-CHK = TRUE .
    MODIFY GT_OUT INDEX PT_ROWS-INDEX .
  ENDLOOP.

  PERFORM SELECT_ROW_BY_SUBTOTAL TABLES PT_ROWS .

  LOOP AT GT_OUT WHERE CHK EQ TRUE.
    MOVE-CORRESPONDING GT_OUT TO IT_SEL.
    MOVE SY-TABIX TO IT_SEL-LINE_NO.
    APPEND IT_SEL. CLEAR IT_SEL.
  ENDLOOP.

ENDFORM.                    " GET_POSTING_DATA
*&---------------------------------------------------------------------*
*&      Form  status_change
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM STATUS_CHANGE.
  __CLS FTAB.

  IF G_CUSTOM_CONTAINER IS INITIAL.
    FTAB-FCODE = 'SAVE'.
    APPEND FTAB. CLEAR FTAB.
  ELSE.
    IF G_GRID->IS_READY_FOR_INPUT( ) EQ 0.
      FTAB-FCODE = 'SAVE'.
      APPEND FTAB. CLEAR FTAB.
    ELSE.
      FTAB-FCODE = 'PROC'.
      APPEND FTAB. CLEAR FTAB.
    ENDIF.
  ENDIF.
  SET PF-STATUS '100' EXCLUDING FTAB.

ENDFORM.                    " status_change
*&---------------------------------------------------------------------*
*&      Form  saving
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVING.
  __CLS IT_FILE.
  LOOP AT GT_OUT.
    MOVE-CORRESPONDING GT_OUT TO IT_FILE .
    APPEND IT_FILE.
  ENDLOOP.
  CLEAR FLAG_DATA_CHANGED.
ENDFORM.                    " saving
*&---------------------------------------------------------------------*
*&      Form  SELECT_PMT07JB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_PLAN.
  DATA : L_DATUM-LOW    TYPE   SY-DATUM,
         L_DATUM-HIGH   TYPE   SY-DATUM.

  __CLS : IT_ITEM, IT_PLAF.

  READ TABLE R_PDATU INDEX 1.

  DATA L_WEEK   TYPE  SCAL-WEEK.
* Week of Input date
  CALL FUNCTION 'DATE_GET_WEEK'
       EXPORTING
            DATE         = R_PDATU-LOW
       IMPORTING
            WEEK         = L_WEEK
       EXCEPTIONS
            DATE_INVALID = 1
            OTHERS       = 2.

  CALL FUNCTION 'WEEK_GET_FIRST_DAY'
       EXPORTING
            WEEK         = L_WEEK
       IMPORTING
            DATE         = R_PDATU-LOW
       EXCEPTIONS
            WEEK_INVALID = 1
            OTHERS       = 2.

  MODIFY R_PDATU INDEX 1.

  SELECT
         B~WERKS
         B~MATNR
         C~PDATU AS PEDTR
         C~PLNMG AS GSMNG
         A~VERSB AS VERID
    INTO CORRESPONDING FIELDS OF TABLE IT_PLAF
    FROM PLPB AS A
    JOIN PBIM AS B
     ON B~VERSB EQ A~VERSB
    JOIN PBED AS C
     ON C~BDZEI EQ B~BDZEI
    WHERE A~PLSCN EQ P_PLSC
     AND  B~MATNR IN S_MATNR
     AND  B~WERKS IN R_BWKEY
     AND  C~PDATU >= R_PDATU-LOW
     AND  C~PDATU <= R_PDATU-HIGH
     AND  B~PBDNR NE SPACE.

*----> Gathering Source data at Daily, Weekly , Monthly
  LOOP AT IT_PLAF.
    MOVE : IT_PLAF-WERKS      TO  IT_ITEM-WERKS,    "PLANT
           IT_PLAF-PEDTR      TO  IT_ITEM-PDATU,    "DATE
           IT_PLAF-GSMNG      TO  IT_ITEM-PLNMG,    "QTY
           IT_PLAF-VERID      TO  IT_ITEM-PVER,     "PROD VERSION
           IT_PLAF-MATNR      TO  IT_ITEM-MATNR .
    MOVE 'B'           TO  IT_ITEM-GUBB.

    WRITE '15' TO IT_ITEM-PDATU+6(2).

    COLLECT IT_ITEM.
    CLEAR IT_ITEM.
  ENDLOOP.
  SORT IT_ITEM BY WERKS VERSB PBDNR MATNR PDATU .

ENDFORM.                    " SELECT_PMT07JB
*&---------------------------------------------------------------------*
*&      Form  MAKE_IT_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_IT_FILE.

  DATA : L_FILENAME(40),
         L_NO(2)    TYPE  N,
         L_SPMON    TYPE  SPMON.

  DATA : BEGIN OF $MATNR OCCURS 0,
           WERKS     TYPE WERKS_D,
           MATNR     TYPE MATNR,
         END   OF $MATNR.

  LOOP AT IT_ITEM.
    $MATNR-WERKS = IT_ITEM-WERKS.
    $MATNR-MATNR = IT_ITEM-MATNR.
    COLLECT $MATNR.
  ENDLOOP.

  __CLS IT_FILE.

  LOOP AT $MATNR.

    IT_FILE-WERKS = $MATNR-WERKS.
    IT_FILE-MATNR = $MATNR-MATNR.

    LOOP AT IT_ITEM WHERE WERKS = $MATNR-WERKS
                      AND MATNR = $MATNR-MATNR.
      L_NO = IT_ITEM-PDATU+4(2).
      CHECK NOT L_NO IS INITIAL.
      CONCATENATE 'IT_FILE-M' L_NO INTO L_FILENAME.
      ASSIGN (L_FILENAME) TO <MONTH>.
      <MONTH> = IT_ITEM-PLNMG.
    ENDLOOP.
    APPEND IT_FILE.
    CLEAR IT_FILE.
  ENDLOOP.

ENDFORM.                    " MAKE_IT_FILE
*&---------------------------------------------------------------------*
*&      Form  get_Version_number
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_PLSC  text
*      <--P_C_VERSB  text
*----------------------------------------------------------------------*
FORM GET_VERSION_NUMBER USING    P_PLSCN
                        CHANGING P_VERSB.

  CLEAR P_VERSB.
  SELECT SINGLE VERSB INTO P_VERSB
  FROM PLPB WHERE PLSCN EQ P_PLSCN.

ENDFORM.                    " get_Version_number
*&---------------------------------------------------------------------*
*&      Form  call_bapi
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_BAPI.

* Total Doc. Count to be created.
  DATA  : TOTAL_DOC_CNT TYPE I,
          CURRENT_DOC_CNT TYPE I.
  DATA : PERCENTAGE TYPE P,$MOD TYPE I,
         $PROG_TEXT(50),$CURRENT_CNT(10),$TOTAL_CNT(10),$TEXT(30) .

  DESCRIBE TABLE IT_HEADMATNR LINES TOTAL_DOC_CNT.
  $TOTAL_CNT = TOTAL_DOC_CNT.
  CLEAR CURRENT_DOC_CNT.

  CLEAR ERROR_IX.
  LOOP AT IT_HEADMATNR.
    ADD 1 TO CURRENT_DOC_CNT.

    $CURRENT_CNT = CURRENT_DOC_CNT.
    CONCATENATE $CURRENT_CNT '/' $TOTAL_CNT
    INTO $TEXT.
    CONDENSE $TEXT.
    CONCATENATE 'posting...' $TEXT INTO $PROG_TEXT.
    PERCENTAGE = CURRENT_DOC_CNT / TOTAL_DOC_CNT * 100.
    PERFORM SHOW_PROGRESS USING $PROG_TEXT PERCENTAGE.

    PERFORM GENERATE_BAPI_DATA.

    IF IT_BAPIRETURN[] IS INITIAL.
      GT_OUT-ICON = ICON_LED_GREEN.
      GT_OUT-MSG = 'ok'.
      CLEAR GT_OUT-FLAG.
    ELSE.
      LOOP AT IT_BAPIRETURN WHERE TYPE NE 'S'.
        MOVE IT_BAPIRETURN-MESSAGE     TO GT_OUT-MSG.
      ENDLOOP.
      GT_OUT-ICON = ICON_LED_YELLOW.
      GT_OUT-FLAG = 'E'.
    ENDIF.
    MODIFY GT_OUT TRANSPORTING ICON MSG FLAG
                  WHERE WERKS = IT_HEADMATNR-WERKS
                    AND MATNR = IT_HEADMATNR-MATNR.
    CLEAR GT_OUT.
  ENDLOOP.

ENDFORM.                    " call_bapi
*&---------------------------------------------------------------------*
*&      Form  BROWSER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_FILE  text
*----------------------------------------------------------------------*
FORM BROWSER CHANGING FILENAME.
  DATA: IT_TFILE TYPE FILETABLE ,
        GD_SUBRC TYPE I.

  CALL  METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
        EXPORTING
          WINDOW_TITLE = 'Select File Name'
          DEFAULT_EXTENSION = '*.*'
          DEFAULT_FILENAME = '*.*'
          FILE_FILTER = '*.*'
          INITIAL_DIRECTORY = 'c:\temp\'
*         MULTISELECTION =
*         WITH_ENCODING =
        CHANGING
          FILE_TABLE = IT_TFILE
          RC = GD_SUBRC.
*         USER_ACTION =
*         FILE_ENCODING =
*         EXCEPTIONS
*         FILE_OPEN_DIALOG_FAILED = 1
*         CNTL_ERROR = 2
*         ERROR_NO_GUI = 3
*         NOT_SUPPORTED_BY_GUI = 4
*         others = 5
  .
  IF SY-SUBRC <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    READ TABLE IT_TFILE INTO FILENAME INDEX 1.
  ENDIF.

ENDFORM.                    " BROWSER
*&---------------------------------------------------------------------*
*&      Form  FREE_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FREE_CONTAINER.
  IF NOT G_EVENT_RECEIVER IS INITIAL.
    FREE G_EVENT_RECEIVER.
  ENDIF.

  IF NOT G_GRID IS INITIAL.
    CALL METHOD G_GRID->FREE.
  ENDIF.

  IF NOT G_CUSTOM_CONTAINER IS INITIAL.
    CALL METHOD G_CUSTOM_CONTAINER->FREE.
  ENDIF.

  FREE : G_GRID,G_CUSTOM_CONTAINER.

  CLEAR :  GS_LAYO,GT_EXCLUDE,GT_OUT[],GT_FCAT[],GT_SORT[].

ENDFORM.                    " FREE_CONTAINER

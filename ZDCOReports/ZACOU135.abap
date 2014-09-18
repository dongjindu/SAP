*----------------------------------------------------------------------
* Program ID        : ZACOU135
* Title             : [CO] ABP Revision - BOM scrap % change
* Created on        : 09/13/2007
* Created by        : I.G.MOON
* Specifications By : Andy Choi
* Description       : BOM scrap % change
*----------------------------------------------------------------------
REPORT ZACOU135 MESSAGE-ID ZMCO.

INCLUDE ZACOU135_TOP.

*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-001.
* Planing Year
PARAMETERS : P_KOKRS LIKE KEKO-KOKRS MEMORY ID CAC OBLIGATORY,
             P_BDATJ LIKE KEKO-BDATJ MEMORY ID ZPYR OBLIGATORY.
SELECT-OPTIONS S_FEVOR FOR MARC-FEVOR MEMORY ID CFV.
SELECT-OPTIONS S_MATNR FOR MARC-MATNR .
PARAMETERS: P_USG LIKE MARC-STLAN DEFAULT '6'." NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK BL1.

SELECTION-SCREEN BEGIN OF BLOCK VIEW-RESULT WITH FRAME.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN PUSHBUTTON  1(30) VSLT USER-COMMAND VSLT.
SELECTION-SCREEN END OF BLOCK VIEW-RESULT.

SELECTION-SCREEN BEGIN OF BLOCK BDC-REQ WITH FRAME TITLE TEXT-T04.
PARAMETERS : P_MODE(1) DEFAULT 'E' NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK BDC-REQ.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-010.
PARAMETER P_VARI TYPE SLIS_VARI.
SELECTION-SCREEN END OF BLOCK B4.

PARAMETERS: P_BOM LIKE MARC-STLAN DEFAULT '6' NO-DISPLAY.
PARAMETERS: P_PLSCN LIKE PLAF-PLSCN DEFAULT '901' NO-DISPLAY.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  SY-TITLE = '[CO] ABP Revision - Change scrap % with BOM'.
  PERFORM DEFAULT_.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.
  PERFORM ALV_VARIANT_F4 CHANGING P_VARI.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
  CLEAR G_PROC_TYPE. " not use but just in case of future

  PERFORM INITIALIZE.
  PERFORM GET_ROW_DATA.
  PERFORM GET_TEXT.
  PERFORM MOVE_OUT.
  PERFORM SET_OUTPUT .

END-OF-SELECTION.

*----------------------------------------------------------------------*

AT SELECTION-SCREEN.
  CLEAR G_ERROR.

  CASE SSCRFIELDS-UCOMM.
    WHEN 'VSLT'.
      CLEAR G_PROC_TYPE.
      PERFORM INITIALIZE.
      PERFORM GET_ROW_DATA.
      PERFORM GET_TEXT.
      PERFORM MOVE_OUT.
      PERFORM SET_OUTPUT .
  ENDCASE.

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
*&      Form  SET_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_OUTPUT.

  CHECK : G_ERROR IS INITIAL.
  CLEAR FLAG_DATA_CHANGED.
  CALL SCREEN 100.

ENDFORM.                    " SET_OUTPUT
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
             'WERKS'    ' '  'X' '' 'X' '',
             'FEVOR'    ' '  'X' '' 'X' '',
             'TXT'      ' '  'X' '' 'X' ''.

ENDFORM.                    " SORT_BUILD

*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INITIALIZE.
  CLEAR : G_ERROR.
  __CLS : IT_ROW_TAB.

ENDFORM.                    " INITIALIZE_
*&---------------------------------------------------------------------*
*&      Form  default_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DEFAULT_.

  WRITE:
          ICON_BIW_REPORT_VIEW AS ICON TO VSLT,
         'View Saved Data' TO VSLT+4(24).

ENDFORM.                    " default_
*&---------------------------------------------------------------------*
*&      Form  refine_row_itab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REFINE_ROW_ITAB.
  CHECK G_ERROR EQ SPACE.
  __PROCESS 'Refining data' '70'.

ENDFORM.                    " refine_row_itab
*&---------------------------------------------------------------------*
*&      Form  MOVE_OUT_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MOVE_OUT.
*  CHECK P_DSP EQ TRUE.
  __PROCESS 'Preparing output...' '95'.

  __CLS GT_OUT.

  LOOP AT IT_ROW_TAB.
    CLEAR GT_OUT.

    MOVE-CORRESPONDING IT_ROW_TAB TO GT_OUT.
    READ TABLE IT_FEVOR_TXT  WITH KEY WERKS = GT_OUT-WERKS
                                      FEVOR = GT_OUT-FEVOR
                                      BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GT_OUT-TXT = IT_FEVOR_TXT-TXT.
    ENDIF.

    SELECT SINGLE
    A_SCRAP AUSCH AVOAU NETAU R_SCRAP
    INTO (GT_OUT-A_SCRAP,
          GT_OUT-AUSCH,
          GT_OUT-AVOAU,
          GT_OUT-NETAU,
          GT_OUT-R_SCRAP)
    FROM ZTCOU135 WHERE WERKS EQ IT_ROW_TAB-WERKS
                                    AND FEVOR EQ IT_ROW_TAB-FEVOR.

    APPEND GT_OUT.CLEAR GT_OUT.
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
FORM APPLY_SCRAP_TO_BOM.

  __CLS $GT_OUT.

  DATA: LT_ROW   TYPE LVC_T_ROW,
        LS_ROW   TYPE LVC_S_ROW,
        LT_ROID  TYPE LVC_T_ROID,
        LV_CNT(5),
        LV_DCNT(5),
        LV_MSG(200).                 " Message

  DATA  LT_MESSAGES      LIKE MESSAGES       OCCURS 0 WITH HEADER LINE.
  DATA  $MESSAGES      LIKE MESSAGES       OCCURS 0 WITH HEADER LINE.


* Save seleted data to table ZTCOU135
  CLEAR: LV_CNT, LT_ROW[], LT_ROID[].

  PERFORM GET_SELECTED_ROWS TABLES $GT_OUT.

  DATA  : I_ZTCOU135 LIKE ZTCOU135 OCCURS 0 WITH HEADER LINE,
          LS_ZTCOU135 LIKE ZTCOU135,
          LT_DEL_ROWS TYPE TABLE OF ZTCOU135.

* start logic for BoM

  DATA $IT_BOM_ROOT LIKE  IT_BOM_ROOT OCCURS 0 WITH HEADER LINE.
  $IT_BOM_ROOT[] = IT_BOM_ROOT[].

  SORT $IT_BOM_ROOT BY  WERKS FEVOR MATNR.

  DATA  : $FLAG(1),$SUBRC(1).
  DATA: FLG_WARNING LIKE CAPIFLAG-FLWARNING,
        $VALID_FROM TYPE DATUV_BI.
  DATA  $IX LIKE SY-TABIX.


  PERFORM GET_INPUT_DATE CHANGING $VALID_FROM.
  __CLS GT_MESSAGES.

* { not important
* Total Doc. Count to be created.
  DATA  : TOTAL_DOC_CNT TYPE I,
          CURRENT_DOC_CNT TYPE I.
  DATA : PERCENTAGE TYPE P,$MOD TYPE I,
         $PROG_TEXT(50),$CURRENT_CNT(10),$TOTAL_CNT(10),$TEXT(30) .
* }

  LOOP AT $GT_OUT.
    $IX = SY-TABIX.
    AT NEW FEVOR.
      $FLAG = TRUE.
    ENDAT.
    CHECK $FLAG EQ TRUE.
    CLEAR $FLAG.

    CLEAR : TOTAL_DOC_CNT,CURRENT_DOC_CNT.
    LOOP AT $IT_BOM_ROOT WHERE FEVOR EQ $GT_OUT-FEVOR.
      ADD 1 TO TOTAL_DOC_CNT.
    ENDLOOP.
    $TOTAL_CNT = TOTAL_DOC_CNT.

    LOOP AT $IT_BOM_ROOT WHERE FEVOR EQ $GT_OUT-FEVOR.
      ADD 1 TO CURRENT_DOC_CNT.
      $CURRENT_CNT = CURRENT_DOC_CNT.
      CONCATENATE $CURRENT_CNT '/' $TOTAL_CNT
      INTO $TEXT.
      CONDENSE $TEXT.
      CONCATENATE 'Change SCRAP% for ' $GT_OUT-FEVOR
        $TEXT ':' $IT_BOM_ROOT-MATNR INTO $PROG_TEXT SEPARATED BY SPACE.
      PERCENTAGE = CURRENT_DOC_CNT / TOTAL_DOC_CNT * 100.
      PERFORM SHOW_PROGRESS USING $PROG_TEXT PERCENTAGE.

      CLEAR $SUBRC.

      __CLS : IT_TSTK2, "Header
              IT_TSTP3. "Item

      PERFORM READ_BOM  USING $IT_BOM_ROOT-WERKS
                              $IT_BOM_ROOT-MATNR
                              $VALID_FROM
                     CHANGING $SUBRC.

      IF $SUBRC IS INITIAL.
        CALL FUNCTION 'CALO_INIT_API'
             EXPORTING
                  DATA_RESET_SIGN          = '!'
             EXCEPTIONS
                  LOG_OBJECT_NOT_FOUND     = 1
                  LOG_SUB_OBJECT_NOT_FOUND = 2
                  OTHER_ERROR              = 3
                  OTHERS                   = 4.

        PERFORM MAINTAIN_BOM USING $IT_BOM_ROOT-WERKS
                                   $IT_BOM_ROOT-MATNR
                                  $GT_OUT-A_SCRAP
                                  $GT_OUT-AUSCH
                                  $GT_OUT-AVOAU
                                  $GT_OUT-NETAU
                                  $GT_OUT-R_SCRAP
                                   $VALID_FROM
                       CHANGING $SUBRC.

        __CLS  $MESSAGES.

        CALL FUNCTION 'CALO_LOG_READ_MESSAGES'
             TABLES:   MESSAGES_AND_PARAMETERS = $MESSAGES
             EXCEPTIONS: WARNING = 01
                         ERROR   = 02.
        IF SY-SUBRC <> 0.

        ELSE.
          APPEND LINES OF $MESSAGES TO LT_MESSAGES.
        ENDIF.
      ELSE.
        CONCATENATE ' skip >>> BoM not found for '
                    $IT_BOM_ROOT-WERKS '/'
                    $IT_BOM_ROOT-MATNR
                    INTO LT_MESSAGES-MSG_TXT SEPARATED BY SPACE.
        APPEND LT_MESSAGES.
      ENDIF.

    ENDLOOP.

    READ TABLE LT_MESSAGES WITH KEY MSG_TYPE = 'E'.
    IF SY-SUBRC EQ 0.
      $GT_OUT-OK? = 'E'.
      $GT_OUT-REMARKS = 'Please refer to the log!'.
    ELSE.
      $GT_OUT-OK? = 'S'.
      $GT_OUT-REMARKS = 'ok'.
    ENDIF.
    APPEND LINES OF LT_MESSAGES TO GT_MESSAGES.
    MODIFY $GT_OUT INDEX $IX TRANSPORTING OK? REMARKS.

    __CLS LT_MESSAGES.

  ENDLOOP.

  LOOP AT $GT_OUT.
    READ TABLE GT_OUT WITH KEY WERKS = $GT_OUT-WERKS
                               FEVOR = $GT_OUT-FEVOR.
    IF SY-SUBRC EQ 0.
      GT_OUT = $GT_OUT.
      MODIFY GT_OUT INDEX SY-TABIX.
    ENDIF.
  ENDLOOP.

  GT_OUT-CHK = FALSE .
  MODIFY GT_OUT TRANSPORTING CHK WHERE CHK EQ TRUE.

ENDFORM.                    " SAVE_z_TABLE
*&---------------------------------------------------------------------*
*&      Form  VIEW_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VIEW_.
  CLEAR G_ERROR.
  __CLS : IT_ROW_TAB,GT_OUT.

ENDFORM.                    " VIEW_
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
*&      Form  DATA_DELETE_CONFIRM
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
      'Each Item data for BoM will be changed!'
      'Do you really want to proceed?' ' '
                 CHANGING L_ANSWER.

  IF L_ANSWER NE 'J'.
    G_ERROR = TRUE.
    MESSAGE S000 WITH 'Processing was canceled by user.'.
  ENDIF.


ENDFORM.                    " DATA_DELETE_CONFIRM
*&---------------------------------------------------------------------*
*&      Form  convert_period
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_PLAF_PERIO  text
*      <--P_IT_ROW_TAB_PERIO  text
*----------------------------------------------------------------------*
FORM CONVERT_PERIOD USING    P_PERIO
                    CHANGING P_JAHRPER.

  DATA : $YEAR(4) TYPE N,
         $PERIOD(3) TYPE N.

  $YEAR = P_PERIO DIV 100.
  $PERIOD = P_PERIO - ( $YEAR * 100 ).

  CONCATENATE $YEAR $PERIOD INTO P_JAHRPER.

ENDFORM.                    " convert_period
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET TITLEBAR '100'.
*   Exclude toolbar
  PERFORM EXCLUDE_FUNCTIONS.
  __CLS FTAB.
  SET PF-STATUS '100' EXCLUDING FTAB.

ENDMODULE.                 " STATUS_0100  OUTPUT
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
  ELSE.
    CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY.
  ENDIF.
  __FOCUS G_GRID.
ENDMODULE.                 " DISPLAY_ALV_100  OUTPUT
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
      CHECK SY-DYNNR EQ '0100'.
      PERFORM REALLY?.
      CHECK G_ERROR NE TRUE.

      PERFORM : APPLY_SCRAP_TO_BOM,
                APPLY_ICON,
                SAVE_TO_ZTABLE,
                REFRESH_ALV.
      __FOCUS G_GRID.

    WHEN 'SWITCH'.
      IF SY-DYNNR EQ '0100'.
        PERFORM SWITCH_EDIT_MODE.
      ENDIF.
      __FOCUS G_GRID.

    WHEN 'LOGV'.
      CALL SCREEN '300'.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
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

*   Define cell attribute
  PERFORM BUILD_CELL_ATTR.

ENDFORM.                    " CREATE_AND_INIT_ALV
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
                  CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW,
                  CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW,
                  CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.

ENDFORM.                    " EXCLUDE_FUNCTIONS
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
    APPEND GS_FCAT TO GT_FCAT.
  END-OF-DEFINITION.

  __CATALOG :
    'X'  'WERKS'           'Plant'              4  'CHAR' '',
    'X'  'FEVOR'           'Schdr'              3  'CHAR' '',
    'X'  'TXT'             'Description'       30  'CHAR' '',
    ' '  'A_SCRAP'         'Asmbly.scrp(%)'     6  'DEC'  '',
    ' '  'AUSCH'           'BoM ComScrp(%)'     6  'DEC'  '',
    ' '  'AVOAU'           'BoM OprScrp(%)'     6  'DEC'  '',
    ' '  'NETAU'           'BoM Net.Ind'        1  'CHAR' '',
    ' '  'R_SCRAP'         'Scrp(%) in Rt.'     6  'DEC'  '',
    ' '  'ICON'            'flg'                3  'ICON' '',
    ' '  'REMARKS'         'Remarks.'          30  'CHAR' ''.

  LOOP AT GT_FCAT INTO GS_FCAT.
    CASE GS_FCAT-FIELDNAME.
      WHEN 'A_SCRAP' OR 'AUSCH' OR 'AVOAU'
        OR 'R_SCRAP'.
        GS_FCAT-JUST = 'R'.
*        GS_FCAT-NO_ZERO = 'X'.
      WHEN 'NETAU'.
        GS_FCAT-CHECKBOX = 'X'.
    ENDCASE.

    GS_FCAT-REF_TABLE = 'ZTCOU135'.
    GS_FCAT-REF_FIELD = GS_FIELDCAT-FIELDNAME.

    MODIFY GT_FCAT FROM GS_FCAT.
  ENDLOOP.

ENDFORM.                    " CREATE_FIELD_CATEGORY
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
  GS_LAYO-EDIT       = 'X'.
  GS_LAYO-ZEBRA      = 'X'.
  GS_LAYO-SEL_MODE   = 'A'.       " Column and row selection
  GS_LAYO-CWIDTH_OPT = 'X'.
  GS_LAYO-CTAB_FNAME = 'TABCOLOR'.
  GS_LAYO-STYLEFNAME = 'CELLTAB'.
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

  __COLOR :
            'WERKS'     '1' 0,
            'FEVOR'     '1' 0,
            'TXT'       '1' 0,
            'A_SCRAP'   '2' 0,
            'AUSCH'     '3' 0,
            'AVOAU'     '3' 0,
            'NETAU'     '3' 0,
            'R_SCRAP'   '2' 0.

  GT_OUT-TABCOLOR[] = GT_SPECIALCOL[].
  MODIFY GT_OUT TRANSPORTING TABCOLOR WHERE TABCOLOR IS INITIAL.

ENDFORM.                    " SET_COLOR
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
    ENDIF.
  ENDLOOP.

  __SET_REFRESH_MODE TRUE.
  CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY
       EXPORTING IS_STABLE = STABLE.

ENDFORM.                    " DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  INFO_TEXT_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FALSE  text
*----------------------------------------------------------------------*
FORM INFO_TEXT_SET USING P_TRUE.

  IF P_TRUE EQ TRUE.
    INFO = TEXT-015.
  ELSE.
    INFO = TEXT-015.
  ENDIF.

ENDFORM.                    " info_text_set
*&---------------------------------------------------------------------*
*&      Form  REFRESH_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REFRESH_ALV.
  __SET_REFRESH_MODE TRUE.
  CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY
       EXPORTING IS_STABLE = STABLE.
ENDFORM.                    " REFRESH_ALV
*&---------------------------------------------------------------------*
*&      Form  get_selected_rows
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$GT_OUT  text
*----------------------------------------------------------------------*
FORM GET_SELECTED_ROWS TABLES $GT_OUT STRUCTURE GT_OUT.

  DATA: LT_ROWS TYPE LVC_T_ROW WITH HEADER LINE,
        LT_ROW_NO TYPE LVC_T_ROID. "Numeric IDs of Selected Rows

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
    $GT_OUT[] = GT_OUT[].
  ELSE.
    LOOP AT LT_ROWS WHERE ROWTYPE IS INITIAL.
      READ TABLE GT_OUT INDEX LT_ROWS-INDEX.
      GT_OUT-CHK = TRUE .
      MODIFY GT_OUT INDEX LT_ROWS-INDEX .
    ENDLOOP.
    LOOP AT GT_OUT.
      CHECK GT_OUT-CHK EQ TRUE.
      $GT_OUT = GT_OUT.
      APPEND $GT_OUT.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " get_selected_rows
*&---------------------------------------------------------------------*
*&      Form  UPDATE_SELECTED_ROWS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_SELECTED_ROWS USING P_FLAG.

  DATA: LT_ROWS TYPE LVC_T_ROW WITH HEADER LINE,
        LT_ROW_NO TYPE LVC_T_ROID. "Numeric IDs of Selected Rows

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


  ELSE.
    LOOP AT LT_ROWS WHERE ROWTYPE IS INITIAL.

    ENDLOOP.
  ENDIF.

ENDFORM.                    " UPDATE_SELECTED_ROWS
*&---------------------------------------------------------------------*
*&      Form  BUILD_CELL_ATTR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_CELL_ATTR.
  DATA: LT_CELLTAB TYPE LVC_T_STYL,
        LS_CELLTAB TYPE LVC_S_STYL.

  CLEAR LT_CELLTAB.
  REFRESH LT_CELLTAB.

  CLEAR GS_FCAT.

  LOOP AT GT_FCAT INTO GS_FCAT.
    LS_CELLTAB-FIELDNAME = GS_FCAT-FIELDNAME.
    IF   LS_CELLTAB-FIELDNAME = 'A_SCRAP'
      OR LS_CELLTAB-FIELDNAME = 'AUSCH'
      OR LS_CELLTAB-FIELDNAME = 'AVOAU'
      OR LS_CELLTAB-FIELDNAME = 'NETAU'
      OR LS_CELLTAB-FIELDNAME = 'R_SCRAP'.
      LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
    ELSE.
      LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    ENDIF.
    INSERT LS_CELLTAB INTO TABLE LT_CELLTAB.
  ENDLOOP.

  CLEAR GT_OUT-CELLTAB.
  INSERT LINES OF LT_CELLTAB INTO TABLE GT_OUT-CELLTAB.
  MODIFY GT_OUT TRANSPORTING CELLTAB WHERE CELLTAB IS INITIAL.
  PERFORM BUILD_CELL_ATTR1_LOCK.

ENDFORM.                    " BUILD_CELL_ATTR
*&---------------------------------------------------------------------*
*&      Form  BUILD_CELL_ATTR1_LOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_CELL_ATTR1_LOCK.

  DATA: LT_CELLTAB TYPE LVC_T_STYL,
        LS_CELLTAB TYPE LVC_S_STYL.

  CLEAR LT_CELLTAB.
  REFRESH LT_CELLTAB.

  __CLS GT_OUT-CELLTAB.
  MODIFY GT_OUT TRANSPORTING CELLTAB WHERE FEVOR = SPACE.

  CLEAR GS_FCAT.

  LOOP AT GT_FCAT INTO GS_FCAT.
    LS_CELLTAB-FIELDNAME = GS_FCAT1-FIELDNAME.
    LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    INSERT LS_CELLTAB INTO TABLE LT_CELLTAB.
  ENDLOOP.

  INSERT LINES OF LT_CELLTAB INTO TABLE GT_OUT-CELLTAB.
  MODIFY GT_OUT TRANSPORTING CELLTAB WHERE FEVOR = SPACE.


ENDFORM.                    " BUILD_CELL_ATTR1_LOCK
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
*&---------------------------------------------------------------------*
*&      Form  view_result
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VIEW_RESULT.
*  PERFORM SET_OUTPUT .

ENDFORM.                    " view_result

*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0300 OUTPUT.

  SET PF-STATUS 'ZLOG'.
  SY-TITLE = 'Error log...'.
  SUPPRESS DIALOG.
  LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
  PERFORM ERROR_LIST.

ENDMODULE.                 " STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  error_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ERROR_LIST.

  LOOP AT GT_MESSAGES WHERE MSG_TYPE NE 'W' AND MSG_TYPE NE 'I'.

    WRITE: /
*         GT_MESSAGES-LOG_NO,
*         GT_MESSAGES-MSG_LINENO,
*         GT_MESSAGES-MSG_TYPE,
*         GT_MESSAGES-MSG_ID,
*         GT_MESSAGES-MSG_NO,
*         GT_MESSAGES-MSG_V1,
*         GT_MESSAGES-MSG_V2,
*         GT_MESSAGES-MSG_V3,
*         GT_MESSAGES-MSG_V4,
           GT_MESSAGES-MSG_TXT.

  ENDLOOP.

ENDFORM.                    " error_list
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
    SET PF-STATUS '100'.
    PERFORM INFO_TEXT_SET USING TRUE.
  ELSE.
**    IF FLAG_DATA_CHANGED EQ TRUE.
**      CALL FUNCTION 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
**           EXPORTING
**                TEXTLINE1     = 'Data has not been saved yet.'
**                TEXTLINE2     = 'Do you want to continue anyway? '
**                TITEL         = 'Confirmation'
**                DEFAULTOPTION = 'N'
**           IMPORTING
**                ANSWER        = ANSWER.
**      CHECK ANSWER EQ 'J'.
**    ENDIF.
**    CLEAR FLAG_DATA_CHANGED.
    CALL METHOD G_GRID->SET_READY_FOR_INPUT
                     EXPORTING I_READY_FOR_INPUT = 0.
    SET PF-STATUS '100' EXCLUDING 'SAVE'.
    PERFORM INFO_TEXT_SET USING FALSE.
  ENDIF.

  PERFORM BUILD_CELL_ATTR.

ENDFORM.                    " SWITCH_EDIT_MODE
*&---------------------------------------------------------------------*
*&      Form  get_row_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_ROW_DATA.

  DATA : $PSTTR TYPE PSTTR,
         $PEDTR TYPE PEDTR.

  __PROCESS 'Read Plan...' '20'.

  __CLS : IT_PLAF_MARC, IT_PLAF, IT_ROW_TAB, IT_BOM_ROOT.

  CONCATENATE P_BDATJ : '0101' INTO $PSTTR,
                        '1231' INTO $PEDTR.

  SELECT A~PLNUM A~PLWRK AS WERKS
         A~MATNR
         A~GSMNG A~PEDTR B~FEVOR
         B~SAUFT C~MTART

   INTO CORRESPONDING FIELDS OF TABLE IT_PLAF_MARC
     FROM PLAF AS A
     INNER JOIN MARC AS B
     ON  B~MATNR EQ A~MATNR
     AND B~WERKS EQ A~PWWRK
     AND B~FEVOR NE SPACE
     INNER JOIN MARA AS C
     ON C~MATNR EQ B~MATNR
     WHERE A~PLSCN = P_PLSCN              "Scenario
       AND A~SOBES = 'E'                  "Special procurement
       AND A~MATNR IN S_MATNR
       AND B~FEVOR IN S_FEVOR
       AND A~PSTTR >= $PSTTR
       AND A~PEDTR <= $PEDTR
       AND C~MTART EQ 'HALB'.

  LOOP AT IT_PLAF_MARC.
    MOVE-CORRESPONDING IT_PLAF_MARC TO :
              IT_PLAF,
              IT_ROW_TAB,
              IT_BOM_ROOT.
    IT_PLAF-PERIO(6) = IT_PLAF_MARC-PEDTR.
    COLLECT : IT_PLAF, IT_ROW_TAB, IT_BOM_ROOT.
    CLEAR   : IT_PLAF, IT_ROW_TAB, IT_BOM_ROOT.
  ENDLOOP.

ENDFORM.                    " get_row_data
*&---------------------------------------------------------------------*
*&      Form  MAKE_MI_ROUTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_MI_ROUTING.
* MI Rounting (FERT)
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_PLPO_TEMP
     FROM PLKO AS A
    INNER JOIN PLAS AS B
       ON A~PLNTY = B~PLNTY
      AND A~PLNNR = B~PLNNR
      AND A~PLNAL = B~PLNAL
    INNER JOIN PLPO AS C
       ON B~PLNTY = C~PLNTY
      AND B~PLNNR = C~PLNNR
      AND B~PLNKN = C~PLNKN
      FOR ALL ENTRIES IN IT_MI
     WHERE A~PLNTY = 'M'
       AND A~PLNNR = IT_MI-PLNNR
       AND A~VERWE = '1'             "Usage
       AND A~STATU IN ('3', '4')     "Status
       AND A~DATUV <= G_STARTDT      "Valid from
       AND A~DELKZ = ''              "Delete indicator
       AND B~LOEKZ = ''              "Delete indicator
       AND C~LOEKZ = '' .            "Delete indicator

* delete old data; change number
  SORT IT_PLPO_TEMP BY PLNNR ARBID ASCENDING
                       DATUV       DESCENDING.

*  DATA: W_PLPO_TEMP LIKE IT_PLPO_TEMP.
*  LOOP AT IT_PLPO_TEMP.
*    IF  IT_PLPO_TEMP-PLNNR = W_PLPO_TEMP-PLNNR
*    AND IT_PLPO_TEMP-ARBID = W_PLPO_TEMP-ARBID.
*      DELETE IT_PLPO_TEMP.
*    ENDIF.
*    W_PLPO_TEMP = IT_PLPO_TEMP.
*  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM IT_PLPO_TEMP
      COMPARING PLNNR ARBID.

  LOOP AT IT_MI.
    LOOP AT IT_PLPO_TEMP WHERE PLNNR = IT_MI-PLNNR.
      MOVE-CORRESPONDING IT_PLPO_TEMP TO IT_PLPO.
      IT_PLPO-MATNR = IT_MI-MATNR.
      IT_PLPO-TYPE = 'F'.
      COLLECT IT_PLPO.  CLEAR IT_PLPO.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " MAKE_MI_ROUTING
*&---------------------------------------------------------------------*
*&      Form  MAKE_MIP_ROUTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_RATE  text
*      -->P_1319   text
*----------------------------------------------------------------------*
FORM MAKE_MIP_ROUTING TABLES   P_TAB  STRUCTURE IT_RATE
                       USING   P_PLNTY.

  DATA : BEGIN OF IT_PLPO_TEMP2 OCCURS 0,
          WERKS   LIKE PLPO-WERKS,
          MATNR   LIKE MARA-MATNR,
          PLNTY   LIKE PLPO-PLNTY,
          PLNNR   LIKE PLPO-PLNNR,
          PLNKN   LIKE PLPO-PLNKN,
          ARBID   LIKE PLPO-ARBID,
          ZAEHL   LIKE PLPO-ZAEHL,
          DATUV   LIKE PLPO-DATUV,

          VGW01   LIKE PLPO-VGW01,    "Set
          VGW02   LIKE PLPO-VGW02,    "Machine
          VGW03   LIKE PLPO-VGW03,    "Labor

          VGE01   LIKE PLPO-VGE01,    "Set
          VGE02   LIKE PLPO-VGE02,    "Machine
          VGE03   LIKE PLPO-VGE03,    "Labor
          AENNR   LIKE PLPO-AENNR,
          PLNAL   LIKE MAPL-PLNAL,
          AUFAK   LIKE PLPO-AUFAK,
        END OF IT_PLPO_TEMP2 .

  DATA $VERWE TYPE PLN_VERWE.
  $VERWE = '10'.

  SELECT
        D~WERKS
        A~MATNR
        D~PLNTY
        D~PLNNR
        D~PLNKN
        D~ARBID
        D~ZAEHL
        D~DATUV

        D~VGW01
        D~VGW02
        D~VGW03

        D~VGE01
        D~VGE02
        D~VGE03
        D~AENNR
        C~PLNAL
        D~AUFAK
   INTO CORRESPONDING FIELDS OF TABLE IT_PLPO_TEMP2
     FROM MAPL AS A
    INNER JOIN PLKO AS B
      ON  A~PLNTY = B~PLNTY
     AND  A~PLNNR = B~PLNNR
     AND  A~PLNAL = B~PLNAL
    INNER JOIN PLAS AS C
       ON B~PLNTY = C~PLNTY
      AND B~PLNNR = C~PLNNR
     AND  B~PLNAL = C~PLNAL
    INNER JOIN PLPO AS D
       ON C~PLNTY = D~PLNTY
      AND C~PLNNR = D~PLNNR
      AND C~PLNKN = D~PLNKN
    FOR ALL ENTRIES IN P_TAB
    WHERE A~PLNTY = P_PLNTY         "R:Rate routing N:Product
      AND A~MATNR = P_TAB-MATNR
      AND A~LOEKZ = ''
      AND B~VERWE = $VERWE          "Usage
      AND B~DATUV <= G_STARTDT      "Valid from

      AND C~LOEKZ = ''              "Delete indicator
      AND D~LOEKZ = ''.


  SORT IT_PLPO_TEMP2 BY PLNNR ARBID MATNR ASCENDING DATUV DESCENDING.

  DELETE ADJACENT DUPLICATES FROM IT_PLPO_TEMP2
      COMPARING PLNNR ARBID MATNR.

  LOOP AT IT_PLPO_TEMP2.

    MOVE-CORRESPONDING IT_PLPO_TEMP2 TO IT_PLPO.
    IF P_PLNTY = 'R'.
      IT_PLPO-TYPE = 'R'.
    ELSE.
      IT_PLPO-TYPE = 'N'.
    ENDIF.

    COLLECT IT_PLPO. CLEAR IT_PLPO.

  ENDLOOP.

ENDFORM.                    " make_mip_routing
*&---------------------------------------------------------------------*
*&      Form  get_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_TEXT.
  __CLS  IT_FEVOR_TXT.

  DATA : BEGIN OF $FEVOR  OCCURS 0,
          WERKS      LIKE MARC-WERKS,
          FEVOR      LIKE MARC-FEVOR,
        END OF $FEVOR.

  LOOP AT IT_ROW_TAB.
    $FEVOR-WERKS = IT_ROW_TAB-WERKS.
    $FEVOR-FEVOR = IT_ROW_TAB-FEVOR.
    COLLECT $FEVOR.
  ENDLOOP.

  SORT $FEVOR.

  CHECK NOT $FEVOR[] IS INITIAL.

  SELECT * FROM T024F
     INTO CORRESPONDING FIELDS OF TABLE IT_FEVOR_TXT
     FOR ALL ENTRIES IN $FEVOR
     WHERE WERKS EQ $FEVOR-WERKS
       AND FEVOR EQ $FEVOR-FEVOR.
  SORT IT_FEVOR_TXT BY WERKS FEVOR.

ENDFORM.                    " get_text
*&---------------------------------------------------------------------*
*&      Form  APPLY_BOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$ITAB_MATNR  text
*      <--P_$SUBRC  text
*----------------------------------------------------------------------*
FORM READ_BOM     USING P_WERKS
                        P_MATNR
                        P_VALID_FROM
               CHANGING P_SUBRC.

  CALL FUNCTION 'CSAP_MAT_BOM_READ'
       EXPORTING
            MATERIAL   = P_MATNR
            PLANT      = P_WERKS
            BOM_USAGE  = P_BOM
            VALID_FROM = P_VALID_FROM
       TABLES
            T_STPO     = IT_TSTP3
            T_STKO     = IT_TSTK2
       EXCEPTIONS
            ERROR      = 1
            OTHERS     = 2.
  IF SY-SUBRC <> 0.
    P_SUBRC = 'R'.
  ENDIF.

ENDFORM.                    " APPLY_BOM

*&---------------------------------------------------------------------*
*&      Form  get_itab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_ITAB.

  __CLS : IT_MIP,IT_MI,IT_PLPO_TEMP,IT_PLPO,IT_CRHD,ITAB.
  CHECK NOT IT_PLAF[] IS INITIAL.

  LOOP AT IT_PLAF .
    IF  IT_PLAF-MTART = 'FERT'.
      IT_MI-MATNR = IT_PLAF-MATNR.
      IT_MI-PLNNR = IT_PLAF-MATNR+6(7) .
      APPEND IT_MI. CLEAR IT_MI.
    ELSE.
      IT_MIP-MATNR = IT_PLAF-MATNR.
      IT_MIP-SAUFT = IT_PLAF-SAUFT.
      APPEND IT_MIP.
      CLEAR IT_MIP.
    ENDIF.
  ENDLOOP.

  CLEAR G_STARTDT.
  CONCATENATE P_BDATJ '0101' INTO G_STARTDT.

* 1) MI Rounting (FERT)
  IF NOT IT_MI[]  IS INITIAL.
    PERFORM MAKE_MI_ROUTING.
  ENDIF.

* 2) MIP Rounting (HALB)
  LOOP AT IT_MIP.
    IF IT_MIP-SAUFT = 'X'.
      IT_RATE-MATNR = IT_MIP-MATNR.
      APPEND IT_RATE. CLEAR IT_RATE.
    ELSE.
      IT_PRODUCT-MATNR = IT_MIP-MATNR.
      APPEND IT_PRODUCT. CLEAR IT_PRODUCT.
    ENDIF.
  ENDLOOP.

* 2-1) MIP :Rate routing
  IF NOT IT_RATE[] IS INITIAL.
    PERFORM MAKE_MIP_ROUTING TABLES IT_RATE
                             USING  'R'.
  ENDIF.
* 2-2) MIP:Product routing
  IF NOT IT_PRODUCT[] IS INITIAL.
    PERFORM MAKE_MIP_ROUTING TABLES IT_PRODUCT
                             USING  'N'.
  ENDIF.

* For get work center mapping
  SELECT OBJID ARBPL INTO CORRESPONDING FIELDS OF TABLE IT_CRHD
     FROM CRHD
     FOR ALL ENTRIES IN IT_PLPO
     WHERE OBJTY = 'A'
       AND OBJID = IT_PLPO-ARBID.

  SORT : IT_CRHD BY OBJID,
         IT_PLPO BY MATNR.

  DATA : L_CNT(3) TYPE N,
         L_FIELD(40).

  __CLS TMPT.


  DATA $IX LIKE SY-TABIX.
  DATA $PLPO_IX LIKE SY-TABIX.
  DATA $DATE_FROM LIKE SY-DATUM.
  DATA $PLNTY(1).

  SORT IT_PLAF.
  LOOP AT IT_PLAF.
    $IX = SY-TABIX.

    READ TABLE IT_PLPO WITH KEY MATNR = IT_PLAF-MATNR BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      $PLPO_IX = SY-TABIX.
    ELSE.
      CONTINUE.
    ENDIF.

* {
    IF IT_PLAF-SAUFT EQ 'X'.
      $PLNTY = 'R'.
    ELSE.
      $PLNTY = 'N'.
    ENDIF.
    CONCATENATE IT_PLAF-PERIO '01' INTO $DATE_FROM.

    IF  G_PROC_TYPE IS INITIAL.    " Not for BDC

      CALL FUNCTION 'CP_CC_S_TSK_EXISTENCE_CHECK'
           EXPORTING
                I_DATE_FROM    = $DATE_FROM
                I_PLNTY        = $PLNTY
                I_PLNNR        = IT_PLPO-PLNNR
                I_PLNAL        = IT_PLPO-PLNAL
           EXCEPTIONS
                TASK_NOT_FOUND = 1
                OTHERS         = 2.
      IF SY-SUBRC <> 0.
*    MESSAGE S000 WITH 'Error(s) was(were) occured when data gathering.'
*                                          'Please check the error log!'
*.
        CONTINUE.
      ENDIF.

      LOOP AT IT_PLPO FROM $PLPO_IX.
        IF IT_PLPO-MATNR NE IT_PLAF-MATNR .
          EXIT.
        ENDIF.
        TMPT-PERIO   = IT_PLAF-PERIO.
        CLEAR IT_CRHD .
        READ TABLE IT_CRHD WITH KEY OBJID = IT_PLPO-ARBID BINARY SEARCH.
        TMPT-KOSTL   = IT_CRHD-ARBPL.
        TMPT-ARBPL   = IT_PLPO-ARBID.
        TMPT-ENG_MH  = IT_PLAF-GSMNG * IT_PLPO-VGW03.
        CHECK TMPT-ENG_MH <>  0 .
        COLLECT TMPT. CLEAR TMPT.
        IT_PLAF-$FLAG = TRUE.
        MODIFY IT_PLAF INDEX $IX TRANSPORTING $FLAG.
      ENDLOOP.


    ENDIF.
* }

    LOOP AT IT_PLPO FROM $PLPO_IX.
      IF IT_PLPO-MATNR NE IT_PLAF-MATNR .
        EXIT.
      ENDIF.
      TMPT-PERIO   = IT_PLAF-PERIO.
      CLEAR IT_CRHD .
      READ TABLE IT_CRHD WITH KEY OBJID = IT_PLPO-ARBID BINARY SEARCH.
      TMPT-KOSTL   = IT_CRHD-ARBPL.
      TMPT-ARBPL   = IT_PLPO-ARBID.
      TMPT-ENG_MH  = IT_PLAF-GSMNG * IT_PLPO-VGW03.
      CHECK TMPT-ENG_MH <>  0 .
      COLLECT TMPT. CLEAR TMPT.
      IT_PLAF-$FLAG = TRUE.
      MODIFY IT_PLAF INDEX $IX TRANSPORTING $FLAG.
    ENDLOOP.

  ENDLOOP.

  SORT TMPT BY ARBPL PERIO .

  __CLS ITAB.

  LOOP AT IT_PLAF WHERE MATNR IN S_MATNR.

    READ TABLE IT_PLPO WITH KEY MATNR = IT_PLAF-MATNR BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      $PLPO_IX = SY-TABIX.
    ELSE.
      CONTINUE.
    ENDIF.

    LOOP AT IT_PLPO FROM $PLPO_IX.
      IF IT_PLPO-MATNR NE IT_PLAF-MATNR .
        EXIT.
      ENDIF.
      ITAB-WERKS   = IT_PLAF-WERKS.
      ITAB-FEVOR   = IT_PLAF-FEVOR.
      ITAB-MATNR   = IT_PLPO-MATNR.
      ITAB-PERIO   = IT_PLAF-PERIO.
      ITAB-TYPE    = IT_PLPO-TYPE.
      ITAB-VGW01   = IT_PLPO-VGW01.    "Set
      ITAB-VGW02   = IT_PLPO-VGW02.    "Machine

      ITAB-VGE01   = IT_PLPO-VGE01.    "unit Set
      ITAB-VGE02   = IT_PLPO-VGE02.    "unit Machine
      ITAB-VGE03   = IT_PLPO-VGE03.    "unit MH
      ITAB-AUFAK   = IT_PLPO-AUFAK.

      CLEAR TMPT.
      READ TABLE TMPT WITH KEY ARBPL = IT_PLPO-ARBID
                               PERIO = IT_PLAF-PERIO
                               BINARY SEARCH.
      CHECK SY-SUBRC = 0 .
      ITAB-KOSTL = TMPT-KOSTL.
      ITAB-ABP_MH = IT_PLPO-VGW03.
      ITAB-MTART = IT_PLAF-MTART.
      CHECK NOT ITAB-ABP_MH IS INITIAL.
      COLLECT ITAB. CLEAR ITAB.
    ENDLOOP.
  ENDLOOP.


ENDFORM.                    " get_itab
*&---------------------------------------------------------------------*
*&      Form  get_input_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_$VALID_FROM  text
*----------------------------------------------------------------------*
FORM GET_INPUT_DATE CHANGING P_FROM.

  DATA $FROM LIKE SY-DATUM.

  CONCATENATE P_BDATJ '0101' INTO $FROM.

  WRITE $FROM TO P_FROM DD/MM/YYYY.

ENDFORM.                    " get_input_date
*&---------------------------------------------------------------------*
*&      Form  MAINTAIN_bom
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$IT_BOM_ROOT_WERKS  text
*      -->P_$IT_BOM_ROOT_MATNR  text
*      -->P_$VALID_FROM  text
*      <--P_$SUBRC  text
*----------------------------------------------------------------------*
FORM MAINTAIN_BOM USING    P_WERKS
                           P_MATNR
                           P_A_SCRAP
                           P_AUSCH         " Component Scrap
                           P_AVOAU         " Operation Scrap
                           P_NETAU         " Net Ind.
                           P_R_SCRAP
                           P_VALID_FROM
                  CHANGING P_$SUBRC.

  DATA: BEGIN OF $TSTK2 OCCURS 0.
          INCLUDE STRUCTURE STKO_API02.
  DATA: END OF $TSTK2.

  DATA: BEGIN OF TSTKO OCCURS 0.
          INCLUDE STRUCTURE  STKO_API01.
  DATA: END OF TSTKO.

  DATA: FLG_WARNING LIKE CAPIFLAG-FLWARNING.

  DATA : $AUSCH TYPE KAUSF,
         $AVOAU TYPE AVOAU,
         $NETAU TYPE NETAU.

  $AUSCH = P_AUSCH.
  $AVOAU = P_AVOAU.
  $NETAU = P_NETAU.

  IF $AUSCH IS INITIAL.
    IT_TSTP3-COMP_SCRAP = '!'.
  ELSE.
    IT_TSTP3-COMP_SCRAP = P_AUSCH.
  ENDIF.

  IF $AVOAU IS INITIAL.
    IT_TSTP3-OP_SCRAP = '!'.
  ELSE.
    IT_TSTP3-OP_SCRAP   = P_AVOAU.
  ENDIF.

  IF $NETAU IS INITIAL.
    IT_TSTP3-OP_NET_IND = '!'.
  ELSE.
    IT_TSTP3-OP_NET_IND = P_NETAU.
  ENDIF.

  MODIFY IT_TSTP3 TRANSPORTING COMP_SCRAP OP_SCRAP OP_NET_IND
                         WHERE COMPONENT NE SPACE.

  CALL FUNCTION 'CSAP_MAT_BOM_MAINTAIN'
       EXPORTING
            MATERIAL      = P_MATNR
            PLANT         = P_WERKS
            BOM_USAGE     = P_USG
            VALID_FROM    = P_VALID_FROM
            FL_BOM_CREATE = ' '
            FL_NEW_ITEM   = ' '
            I_STKO        = TSTKO
       IMPORTING
            FL_WARNING    = FLG_WARNING
            O_STKO        = $TSTK2
       TABLES
            T_STPO        = IT_TSTP3
       EXCEPTIONS
            OTHERS        = 1.

  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " MAINTAIN_bom
*&---------------------------------------------------------------------*
*&      Form  APPLY_ICON
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM APPLY_ICON.

  DATA $IX LIKE SY-TABIX.

*ICON_LED_GREEN
*ICON_LED_RED
*ICON_LED_YELLOW

  LOOP AT GT_OUT.
    $IX = SY-TABIX.
    IF GT_OUT-OK? EQ 'S'.
      GT_OUT-ICON = ICON_LED_GREEN.
    ELSEIF GT_OUT-OK? EQ 'E'..
      GT_OUT-ICON = ICON_LED_YELLOW.
    ENDIF.
    MODIFY GT_OUT INDEX $IX TRANSPORTING ICON.
  ENDLOOP.

ENDFORM.                    " APPLY_ICON
*&---------------------------------------------------------------------*
*&      Form  save_to_ztable
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_TO_ZTABLE.

  DATA I_ZTCOU135 LIKE ZTCOU135 OCCURS 0 WITH HEADER LINE.

  LOOP AT $GT_OUT.
    MOVE-CORRESPONDING $GT_OUT TO I_ZTCOU135.
    I_ZTCOU135-AEDAT = SY-DATUM.
    I_ZTCOU135-AENAM = SY-UNAME.
    APPEND I_ZTCOU135.
  ENDLOOP.

  READ TABLE I_ZTCOU135 INDEX 1.

  CHECK SY-SUBRC EQ 0.

  MODIFY ZTCOU135 FROM TABLE I_ZTCOU135.

  IF SY-SUBRC EQ 0.
    MESSAGE S000 WITH 'Data has been saved successfully.'.
  ELSE.
    MESSAGE S000 WITH ' Error was occured when data saving.'.
  ENDIF.
ENDFORM.                    " save_to_ztable

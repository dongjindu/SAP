*----------------------------------------------------------------------
* Program ID        : ZM_IF_YM_1001_MATERIAL_MASTER
* Title             : [MM] GCCS Interface - Material Master
* Created on        : 10/11/2007
* Created by        : I.G.MOON
* Specifications By : Crossley, Ron
* Description       : GCCS Interface - Material Master
*----------------------------------------------------------------------
REPORT ZM_IF_YM_1001_MATERIAL_MASTER MESSAGE-ID ZMCO.

INCLUDE : Z_MOON_ALV_TOP,
          Z_MOON_ALV_FNC.

INCLUDE ZM_IF_YM_1001_TOP.
*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BL3 WITH FRAME TITLE TEXT-003.
SELECT-OPTIONS S_WERKS FOR MARC-WERKS MEMORY ID  WRK.
SELECT-OPTIONS S_MATNR FOR MARA-MATNR MEMORY ID  MAT.
SELECT-OPTIONS S_LGNUM FOR MLGT-LGNUM.
SELECTION-SCREEN END OF BLOCK BL3.

SELECTION-SCREEN BEGIN OF BLOCK BL4 WITH FRAME TITLE TEXT-004.
PARAMETER: P_UPD AS CHECKBOX DEFAULT FALSE,
           P_DSP DEFAULT TRUE NO-DISPLAY.
PARAMETERS P_DATUM LIKE SY-DATUM DEFAULT SY-DATUM OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BL4.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-010.
PARAMETER P_VARI TYPE SLIS_VARI.
SELECTION-SCREEN END OF BLOCK B4.

SELECTION-SCREEN BEGIN OF BLOCK VIEW-RESULT WITH FRAME TITLE TEXT-T03.
SELECTION-SCREEN PUSHBUTTON  1(24) VSLT USER-COMMAND VSLT.
SELECTION-SCREEN END OF BLOCK VIEW-RESULT.

PARAMETERS P_DEST LIKE RFCDES-RFCDEST
                       DEFAULT 'WMRM01'." NO-DISPLAY.

DATA: BEGIN OF IT_ROW_TAB1 OCCURS 0                ,
        EPART_NO    LIKE MARA-MATNR                , " Material number
        EBIN_TYPE   LIKE ZTMM_EAI_MAT_MST-EBIN_TYPE, " Fixed BIN Type
        EBIN_LOC    LIKE ZTMM_EAI_MAT_MST-EBIN_LOC , " BIN Location
        EWHN(4)     TYPE C                       , " Warehouse Number
        ERND_QTY    LIKE MLGT-RDMNG              , " Rounding qty
        EFEEDER(5)  TYPE C                       , " Feeder
        EWORK_S     LIKE ZTMM_EAI_MAT_MST-EWORK_S, " Workstation
        ERHLH(2)    TYPE C                       , " RH/LH
        ESTOP       LIKE ZTMM_MAST-ZZFSTP        , " Feeder stop
        EHRDAY      LIKE ZTMM_EAI_MAT_MST-EHRDAY , " Supply to Line
        EFEED_CYCLE LIKE ZTMM_MAST-FEED_CYCLE    , " Feed Cycle
        EPLANT      LIKE T001W-WERKS             , " Plant
        E_BUN       LIKE MARA-MEINS              , " Base UOM
        EMGRP       LIKE MARA-MATKL              , " Material group
        ENET_WGT    LIKE MARA-NTGEW              , " Net weight
        E_WUN       LIKE MARA-GEWEI              , " Weight Unit
        ELKM        LIKE MARA-PROFL              , " LP/KD/MIP
        ERND_VALUE  LIKE MARC-BSTRF              , " Rnd val for pur.qty
        EMSTS       LIKE ZTMM_EAI_MAT_MST-EMSTS  , " Plant-Spec Mat Stat
        EPART_NAME  LIKE MAKT-MAKTX              , " Material descr
      END OF IT_ROW_TAB1.
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  SY-TITLE = '[MM] GCCS Interface - Material Master'.
  PERFORM DEFAULT_.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.
  PERFORM ALV_VARIANT_F4 CHANGING P_VARI.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
  DATA $SUBRC(1).

  PERFORM :
            INITIALIZE            ,
            GATHER_DATA           ,
            MOVE_OUT              .
  CHECK G_ERROR EQ SPACE .

  IF  P_UPD EQ TRUE OR SY-BATCH EQ TRUE .
    PERFORM SAVE_Z_TABLE TABLES GT_OUT CHANGING $SUBRC.
    PERFORM SEND_EAI TABLES GT_OUT
                       USING SPACE
                       CHANGING $SUBRC.
    IF $SUBRC EQ SPACE.
      LOOP AT GT_OUT.
        GT_OUT-TAIT_TARG_RSLT = 'S'.
        MODIFY GT_OUT INDEX SY-TABIX TRANSPORTING TAIT_TARG_RSLT.
      ENDLOOP.
    ELSE.
      LOOP AT GT_OUT.
        GT_OUT-TAIT_TARG_RSLT = 'E'.
        MODIFY GT_OUT INDEX SY-TABIX TRANSPORTING TAIT_TARG_RSLT.
      ENDLOOP.
    ENDIF.

  ELSE.
    PERFORM GET_DATA_FROM_Z TABLES $IT_ROW_TAB.
    PERFORM CHK_SENT_DATA.
  ENDIF.

  IF  P_DSP EQ TRUE.
    PERFORM APPLY_ICON.
  ENDIF.

*----------------------------------------------------------------------*

AT SELECTION-SCREEN.
  CASE SSCRFIELDS-UCOMM.
* screen
    WHEN 'VSLT'.
      PERFORM VIEW_.
  ENDCASE.

END-OF-SELECTION.
  CHECK G_ERROR EQ SPACE .
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
*&      Form  SET_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_OUTPUT.

  CHECK : P_DSP EQ TRUE,
          G_ERROR IS INITIAL.

  PERFORM INIT_ALV_PARM.

***   Initialization fieldcatalog   ***
  PERFORM FIELDCAT_INIT      USING GT_FIELDCAT[].
  PERFORM SORT_BUILD         USING GT_SORT[].
  PERFORM EXCLUDE_STATUS.
  PERFORM ALV_EVENTS_GET     USING:  'P'.", 'T'.
  PERFORM ALV_GRID_DISPLAY_S TABLES  GT_OUT USING ''.

ENDFORM.                    " SET_OUTPUT

*---------------------------------------------------------------------*
*       FORM ALV_GRID_DISPLAY_S                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FT_OUTTAB                                                     *
*  -->  PF_EDIT_SET                                                   *
*---------------------------------------------------------------------*
FORM ALV_GRID_DISPLAY_S TABLES FT_OUTTAB
                         USING  PF_EDIT_SET.

  G_PROGRAM = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM       = G_PROGRAM
            I_CALLBACK_PF_STATUS_SET = 'PF_STATUS_SET'
            I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
            IS_LAYOUT                = GS_LAYOUT
            IT_EXCLUDING             = GT_EXCLUDING
            IT_FIELDCAT              = GT_FIELDCAT
            IT_SPECIAL_GROUPS        = GT_SP_GROUP
            IT_SORT                  = GT_SORT
            I_SAVE                   = G_SAVE
            IS_VARIANT               = GS_VARIANT
            IT_EVENTS                = GT_EVENTS
       TABLES
            T_OUTTAB                 = FT_OUTTAB
       EXCEPTIONS
            PROGRAM_ERROR            = 1
            OTHERS                   = 2.

** refresh
  IF GS_EXIT_CAUSED_BY_USER CS 'X'.
    SET SCREEN 0.
  ENDIF.

ENDFORM.                    " ALV_GRID_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  INIT_ALV_PARM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_ALV_PARM.

  __CLS   :  GT_FIELDCAT, GT_SORT, GT_EVENTS, GT_LISTHEADER,
             GT_SP_GROUP.

  CLEAR   :  GS_LAYOUT.
  PERFORM SET_LAYOUT CHANGING GS_LAYOUT.

  GV_REPID = GS_VARIANT-REPORT = SY-REPID.
  GS_VARIANT-VARIANT = P_VARI.


ENDFORM.                    " INIT_ALV_PARM

*---------------------------------------------------------------------*
*       FORM SET_LAYOUT                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  CS_LAYO                                                       *
*---------------------------------------------------------------------*
FORM SET_LAYOUT CHANGING CS_LAYO TYPE SLIS_LAYOUT_ALV.
  CS_LAYO-COLWIDTH_OPTIMIZE      = 'X'.
  CS_LAYO-BOX_FIELDNAME          = 'CHKBOX'.
  CS_LAYO-COLTAB_FIELDNAME       = 'TABCOLOR'.
ENDFORM.                    " set_layout

*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM FIELDCAT_INIT USING FT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV .

  DATA: L_POS       TYPE I.

  __CLS FT_FIELDCAT.

  DEFINE __CATALOG.
    L_POS = L_POS + 1.
    CLEAR GS_FIELDCAT.
    GS_FIELDCAT-COL_POS       = L_POS.
    GS_FIELDCAT-KEY           = &1.
    GS_FIELDCAT-FIELDNAME     = &2.
    GS_FIELDCAT-SELTEXT_M     = &3.        " Column heading
    GS_FIELDCAT-OUTPUTLEN     = &4.        " Column width
    GS_FIELDCAT-DATATYPE      = &5.        " Data type
    GS_FIELDCAT-QFIELDNAME    = &6.
    APPEND GS_FIELDCAT TO  FT_FIELDCAT.
  END-OF-DEFINITION.

  __CATALOG :

          'X'  'EPART_NO'    'PART NO'     18 'CHAR' ' ',
          'X'  'EPART_NAME'  'PART_NAME'   40 'CHAR' ' ',
          'X'  'SEQ_NO'      'Seq.'         6 'NUMC' ' ',
          ' '  'EBIN_TYPE'   'F.BIN'        3 'CHAR' ' ',
          ' '  'EBIN_LOC'    'BIN Loc.'    10 'CHAR' ' ',
          ' '  'ECON_TYPE'   'Ctr.Cycl'     3 'CHAR' ' ',
          ' '  'ECON_LOC'    'Ctr.BN.Loc.' 10 'CHAR' ' ',
          ' '  'ESUP_AREA'   'Sply Area'   10 'CHAR' ' ',
          ' '  'EFEEDER'     'F.Cd'         3 'CHAR' ' ',
          ' '  'EWORK_S'     'W/S'          5 'CHAR' ' ',
          ' '  'ERHLH'       'R/L'          1 'CHAR' ' ',
          ' '  'ESTOP'       'sgn'          3 'CHAR' ' ',
          ' '  'EHRDAY'      'S'            1 'CHAR' ' ',
          ' '  'EFEED_CYCLE' 'Fd.c'         4 'CHAR' ' ',
          ' '  'EWHN'        'WH#'          4 'CHAR' ' ',
          ' '  'EPLANT'      'Plnt'         4 'CHAR' ' ',
          ' '  'ESLOC'       'SLoc'         4 'CHAR' ' ',
          ' '  'ELKM'        'LKM'          1 'CHAR' ' ',
          ' '  'ERND_QTY'    'R.Qty'       11 'NUMC' ' ',
          ' '  'ERND_VALUE'  'R.VALUE'     11 'NUMC' ' ',
          ' '  'EMSTS'       'St'           2 'CHAR' ' ',
          ' '  'EMGRP'       'MGrp'        10 'CHAR' ' ',
          ' '  'ENET_WGT'    'N/W'          8 'DEC ' ' ',
          ' '  'E_WUN'       'W/U'          2 'CHAR' ' ',
          ' '  'E_BUN'       'B/U'          2 'CHAR' ' ',
          ' '  'ICON'        'flg'          3 'ICON' ' '.

  PERFORM CHANGE_FIELDCAT USING FT_FIELDCAT[] .

ENDFORM.                    " fieldcat_init
*&---------------------------------------------------------------------*
*&      Form  SORT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SORT[]  text
*----------------------------------------------------------------------*
FORM SORT_BUILD USING    FT_SORT TYPE SLIS_T_SORTINFO_ALV.

  DEFINE SORT_TAB.
    CLEAR GS_SORT.
    GS_SORT-FIELDNAME = &1.
    GS_SORT-SPOS      = &2.
    GS_SORT-UP        = &3.
    GS_SORT-GROUP     = &4.
    GS_SORT-COMP      = &5.
    APPEND GS_SORT TO FT_SORT.
  END-OF-DEFINITION.

  SORT_TAB :
             'EPART_NO'        '1' 'X' 'X' 'X',
             'EPART_NAME'      '2' 'X' 'X' 'X'.

ENDFORM.                    " SORT_BUILD

*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM TOP_OF_PAGE.
  DATA L_TEXT(60).
  DATA S_TEXT(60).

  REFRESH GT_LISTHEADER.

ENDFORM.                    "top_of_page
*---------------------------------------------------------------------*
*       FORM PF_STATUS_SET
*---------------------------------------------------------------------*
FORM PF_STATUS_SET USING  FT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS '100' EXCLUDING FT_EXTAB.
ENDFORM.                    "PF_STATUS_SET
*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
FORM USER_COMMAND USING FP_UCOMM LIKE SY-UCOMM
                        FS       TYPE SLIS_SELFIELD.
  DATA L_ANSWER(1).
  DATA $SUBRC(1).

  CLEAR : G_ERROR.

  CASE FP_UCOMM.
    WHEN 'SAVE'.

      __CLS GT_SEL.

      LOOP AT GT_OUT WHERE CHKBOX = 'X'.
        MOVE GT_OUT TO GT_SEL.
        APPEND GT_SEL.
      ENDLOOP.
      READ TABLE GT_SEL INDEX 1.
      IF SY-SUBRC NE 0.
        MESSAGE E000 WITH 'Please select a data to save.'.
        EXIT.
      ENDIF.

      PERFORM POP_UP USING
          'Data will be refreshed !'
          'Do you really want to process ?' ' '
          CHANGING L_ANSWER.

      IF L_ANSWER NE 'J'.
        MESSAGE S000 WITH 'Processing was canceled by user.'.
        EXIT.
      ENDIF.

      PERFORM SAVE_Z_TABLE TABLES GT_SEL
                         CHANGING $SUBRC.

      IF $SUBRC EQ SPACE.
        LOOP AT GT_OUT WHERE CHKBOX = 'X'.
          GT_OUT-TAIT_TARG_RSLT = ' '.
          MODIFY GT_OUT INDEX SY-TABIX TRANSPORTING TAIT_TARG_RSLT.
        ENDLOOP.
      ENDIF.

    WHEN 'IEAI' OR 'REAI'.

      __CLS GT_SEL.

      LOOP AT GT_OUT WHERE CHKBOX = 'X'.
        MOVE GT_OUT TO GT_SEL.
        APPEND GT_SEL.
      ENDLOOP.
      READ TABLE GT_SEL INDEX 1.
      IF SY-SUBRC NE 0.
        MESSAGE E000 WITH 'Please select a data to transport.'.
        EXIT.
      ENDIF.

      PERFORM SEND_EAI TABLES GT_SEL
                         USING FP_UCOMM(1)
                         CHANGING $SUBRC.
      IF $SUBRC EQ SPACE.
        LOOP AT GT_OUT WHERE CHKBOX = 'X'.
          GT_OUT-TAIT_TARG_RSLT = 'S'.
          MODIFY GT_OUT INDEX SY-TABIX TRANSPORTING TAIT_TARG_RSLT.
        ENDLOOP.
      ENDIF.

    WHEN 'SSSS'.

      __CLS GT_SEL.

      LOOP AT GT_OUT WHERE CHKBOX = 'X'.
        MOVE GT_OUT TO GT_SEL.
        APPEND GT_SEL.
      ENDLOOP.
      READ TABLE GT_SEL INDEX 1.
      IF SY-SUBRC NE 0.
        MESSAGE E000 WITH 'Please select a data to save.'.
        EXIT.
      ENDIF.

      PERFORM POP_UP USING
          'Data will be refreshed !'
          'Do you really want to process ?' ' '
          CHANGING L_ANSWER.

      IF L_ANSWER NE 'J'.
        MESSAGE S000 WITH 'Processing was canceled by user.'.
        EXIT.
      ENDIF.

      PERFORM SAVE_Z_TABLE TABLES GT_SEL
                         CHANGING $SUBRC.

      PERFORM SEND_EAI TABLES GT_SEL
                         USING FP_UCOMM(1)
                         CHANGING $SUBRC.
      IF $SUBRC EQ SPACE.
        LOOP AT GT_OUT WHERE CHKBOX = 'X'.
          GT_OUT-TAIT_TARG_RSLT = 'S'.
          MODIFY GT_OUT INDEX SY-TABIX TRANSPORTING TAIT_TARG_RSLT.
        ENDLOOP.
      ENDIF.

  ENDCASE.

  PERFORM APPLY_ICON.

  CALL FUNCTION 'REUSE_ALV_GRID_LAYOUT_INFO_SET'
       EXPORTING
            IS_LAYOUT   = GS_LAYOUT
            IT_FIELDCAT = GT_FIELDCAT
            IT_SORT     = GT_SORT.

  FS-REFRESH    = 'X'.
  FS-ROW_STABLE = 'X'.
  FS-COL_STABLE = 'X'.


ENDFORM.                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  CHANGE_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FT_FIELDCAT[]  text
*      -->P_ENDFORM  text
*----------------------------------------------------------------------*
FORM CHANGE_FIELDCAT USING    PT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.

  LOOP AT PT_FIELDCAT INTO GS_FIELDCAT.
    CASE GS_FIELDCAT-FIELDNAME.
      WHEN 'ERND_QTY' OR 'ERND_VALUE' OR 'ENET_WGT'.
        GS_FIELDCAT-JUST = 'R'.
    ENDCASE.
    GS_FIELDCAT-REF_TABNAME = 'ZTMM_EAI_MAT_MST'.
    GS_FIELDCAT-REF_FIELDNAME = GS_FIELDCAT-FIELDNAME.
    MODIFY PT_FIELDCAT FROM GS_FIELDCAT.
  ENDLOOP.

ENDFORM.                    " CHANGE_FIELDCAT
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
  PERFORM GET_PLANT.
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
         'View saved data' TO VSLT+4(21).

ENDFORM.                    " default_
*&---------------------------------------------------------------------*
*&      Form  get_r_bwkey
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_PLANT.

  IF NOT S_WERKS[] IS INITIAL.
    R_PLANT[] = S_WERKS[].
    EXIT.
  ENDIF.

  TYPES: BEGIN OF TY_PLANT,
           BWKEY TYPE BWKEY,
         END OF TY_PLANT.

  DATA   LT_PLANT TYPE TABLE OF TY_PLANT WITH HEADER LINE.

  __CLS : LT_PLANT, R_PLANT.

  SELECT BWKEY INTO TABLE LT_PLANT
    FROM T001K.

  R_PLANT-SIGN   = 'I'.
  R_PLANT-OPTION = 'EQ'.

  LOOP AT LT_PLANT.
    R_PLANT-LOW = LT_PLANT-BWKEY.
    APPEND R_PLANT.
  ENDLOOP.

  CLEAR R_PLANT.

ENDFORM.                    " get_r_bwkey
*&---------------------------------------------------------------------*
*&      Form  MOVE_OUT_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MOVE_OUT.
  CHECK : G_ERROR EQ SPACE,
          P_DSP EQ TRUE.
  __PROCESS 'Preparing output...' '95'.

  __CLS GT_OUT.

  LOOP AT IT_ROW_TAB.
    MOVE-CORRESPONDING IT_ROW_TAB TO GT_OUT.
    TRANSLATE GT_OUT-EPART_NO TO UPPER CASE.
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
FORM SAVE_Z_TABLE TABLES LT_SEL STRUCTURE GT_OUT
                                CHANGING P_SUBRC.

  CHECK : G_ERROR EQ FALSE.

  __PROCESS 'refreshing data...' '10'.

  DELETE FROM ZTMM_EAI_MAT_MST
    WHERE EPART_NO IN S_MATNR
      AND TAIT_TARG_D EQ P_DATUM
      AND EPLANT IN S_WERKS.

  __PROCESS 'Saving data...' '50'.

  DATA : $CNT TYPE I,
         $CNT_E TYPE I.

  LOOP AT LT_SEL.
    CLEAR *ZTMM_EAI_MAT_MST.
    MOVE-CORRESPONDING LT_SEL TO *ZTMM_EAI_MAT_MST.
     *ZTMM_EAI_MAT_MST-TAIT_TARG_D    = SY-DATUM.
     *ZTMM_EAI_MAT_MST-TAIT_TARG_T    = SY-UZEIT.
     *ZTMM_EAI_MAT_MST-TAIT_TARG_RSLT = SPACE.
     *ZTMM_EAI_MAT_MST-TAIT_TARG_DESC = SPACE.
    INSERT ZTMM_EAI_MAT_MST FROM *ZTMM_EAI_MAT_MST.
    IF SY-SUBRC EQ 0.
      ADD 1 TO $CNT.
    ELSE.
      ADD 1 TO $CNT_E.
    ENDIF.
  ENDLOOP.

  IF $CNT > 0.
    COMMIT WORK.
    IF $CNT EQ 1.
      MESSAGE S000 WITH $CNT ' record was saved successfully!'.
    ELSE.
      MESSAGE S000 WITH $CNT ' records were saved successfully!'.
    ENDIF.
  ELSE.
    ROLLBACK WORK.
    MESSAGE S000 WITH '(Error) I/F data has not been created!'.
    P_SUBRC = 'E'.
  ENDIF.

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

  __CLS IT_ROW_TAB.

  PERFORM : INITIALIZE            ,
            GET_DATA_FROM_Z TABLES IT_ROW_TAB,
            MOVE_OUT              .

  PERFORM APPLY_ICON.

  CHECK G_ERROR EQ SPACE .

  DATA : $P_UPD LIKE P_UPD,
         $P_DSP LIKE P_DSP.

  $P_UPD = P_UPD.
  $P_DSP = P_DSP.

  P_UPD = TRUE.
  P_DSP = TRUE.

  PERFORM SET_OUTPUT .

  P_UPD = $P_UPD.
  P_DSP = $P_DSP.

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
*&      Form  GATHER_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GATHER_DATA.

  DATA : $FLAG(1),
         $SEQ_NO(6) TYPE N,
         $IX LIKE SY-TABIX.

  __CLS : IT_ROW_TAB, IT_MATNR, IT_MARD.

** Changed by Furong on 05/01/08
*  SELECT
** (MLGT)
*        a~matnr  AS epart_no          " Material number
*        a~lgtyp  AS ebin_type         " Fixed BIN Type
*        a~lgpla  AS ebin_loc          " BIN Location
*        a~lgnum  AS ewhn              " Warehouse Number
*        a~rdmng  AS ernd_qty          " Rounding qty
** (ZTMM_MAST)
*        b~feedr  AS efeeder           " Feeder
*        b~works  AS ework_s           " Workstation
*        b~rh_lh  AS erhlh             " RH/LH
*        b~zzfstp AS estop             " Feeder stop
*        b~spptl  AS ehrday            " Supply to Line
*        b~feed_cycle AS efeed_cycle   " Feed Cycle
*        b~werks  AS eplant            " Plant
*        b~meins  AS e_bun             " Base unit of measure
** (MARA)
*        c~matkl  AS emgrp             " Material group
*        c~ntgew  AS enet_wgt          " Net weight
*        c~gewei  AS e_wun             " Weight Unit
*        c~profl  AS elkm              " LP/KD/MIP
** (MARC)
*        d~bstrf  AS ernd_value        " Rounding value for pur.qty
*        d~mmsta  AS emsts             " Plant-Specific Material Status
** (MAKT)
*        t~maktx  AS epart_name        " Material description
*
**       INTO CORRESPONDING FIELDS OF TABLE it_row_tab
*       INTO CORRESPONDING FIELDS OF TABLE it_row_tab1
*
*   FROM (
*         mlgt AS a
*           JOIN mara AS c
*             ON c~matnr EQ a~matnr
*            JOIN makt AS t
*           ON t~matnr EQ c~matnr
*           AND t~spras EQ sy-langu
*           )
*           LEFT OUTER JOIN
*           ztmm_mast AS b
*            ON b~matnr EQ a~matnr
*            INNER JOIN marc AS d
*              ON d~matnr EQ b~matnr
*             AND d~werks EQ b~werks
*
*       WHERE a~matnr IN s_matnr
*         AND a~lgtyp IN ('523','422')
*         AND a~lvorm EQ space
*         AND c~profl IN ('V','K','M')
*         AND c~tempb NE '11'             " except JIS mat.
*         AND d~mmsta IN ('11','12','13','14')
*         AND d~werks IN r_plant
*        %_HINTS oracle 'FIRST_ROWS(10)'.

**Changed by Furong on 07/28/08

*  SELECT
** (MLGT)
*        a~matnr  AS epart_no          " Material number
*        a~lgtyp  AS ebin_type         " Fixed BIN Type
*        a~lgpla  AS ebin_loc          " BIN Location
*        a~lgnum  AS ewhn              " Warehouse Number
*        a~rdmng  AS ernd_qty          " Rounding qty
** (ZTMM_MAST)
*        b~feedr  AS efeeder           " Feeder
*        b~works  AS ework_s           " Workstation
*        b~rh_lh  AS erhlh             " RH/LH
*        b~zzfstp AS estop             " Feeder stop
*        b~spptl  AS ehrday            " Supply to Line
*        b~feed_cycle AS efeed_cycle   " Feed Cycle
*        d~werks  AS eplant            " Plant
*        c~meins  AS e_bun             " Base unit of measure
** (MARA)
*        c~matkl  AS emgrp             " Material group
*        c~ntgew  AS enet_wgt          " Net weight
*        c~gewei  AS e_wun             " Weight Unit
*        c~profl  AS elkm              " LP/KD/MIP
** (MARC)
*        d~bstrf  AS ernd_value        " Rounding value for pur.qty
*        d~mmsta  AS emsts             " Plant-Specific Material Status
** (MAKT)
*        t~maktx  AS epart_name        " Material description
*
**       INTO CORRESPONDING FIELDS OF TABLE it_row_tab
*       INTO CORRESPONDING FIELDS OF TABLE it_row_tab1
*
*   FROM (
*         mlgt AS a
*           JOIN mara AS c
*             ON c~matnr EQ a~matnr
*            JOIN makt AS t
*           ON t~matnr EQ c~matnr
*           AND t~spras EQ sy-langu
*           )
*         INNER JOIN marc AS d
*              ON d~matnr EQ c~matnr
*           LEFT OUTER JOIN
*           ztmm_mast AS b
*            ON b~matnr EQ d~matnr
*           AND d~werks EQ b~werks
*
*       WHERE a~matnr IN s_matnr
*         AND a~lgtyp IN ('523','422')
*         AND a~lvorm EQ space
*         AND c~profl IN ('V','K','M')
*         AND c~tempb NE '11'             " except JIS mat.
*         AND d~mmsta IN ('11','12','13','14')
*         AND d~werks IN r_plant
*        %_HINTS oracle 'FIRST_ROWS(10)'.

  SELECT
* (MLGT)
         C~MATNR  AS EPART_NO          " Material number
         A~LGTYP  AS EBIN_TYPE         " Fixed BIN Type
         A~LGPLA  AS EBIN_LOC          " BIN Location
         A~LGNUM  AS EWHN              " Warehouse Number
         A~RDMNG  AS ERND_QTY          " Rounding qty
* (ZTMM_MAST)
         B~FEEDR  AS EFEEDER           " Feeder
         B~WORKS  AS EWORK_S           " Workstation
         B~RH_LH  AS ERHLH             " RH/LH
         B~ZZFSTP AS ESTOP             " Feeder stop
         B~SPPTL  AS EHRDAY            " Supply to Line
         B~FEED_CYCLE AS EFEED_CYCLE   " Feed Cycle
         D~WERKS  AS EPLANT            " Plant
         C~MEINS  AS E_BUN             " Base unit of measure
* (MARA)
         C~MATKL  AS EMGRP             " Material group
         C~NTGEW  AS ENET_WGT          " Net weight
         C~GEWEI  AS E_WUN             " Weight Unit
         C~PROFL  AS ELKM              " LP/KD/MIP
* (MARC)
         D~BSTRF  AS ERND_VALUE        " Rounding value for pur.qty
         D~MMSTA  AS EMSTS             " Plant-Specific Material Status
* (MAKT)
         T~MAKTX  AS EPART_NAME        " Material description

*       INTO CORRESPONDING FIELDS OF TABLE it_row_tab
        INTO CORRESPONDING FIELDS OF TABLE IT_ROW_TAB1

    FROM (
           MARA AS C
            LEFT OUTER JOIN MLGT AS A
            ON C~MATNR EQ A~MATNR
             JOIN MAKT AS T
            ON T~MATNR EQ C~MATNR
            AND T~SPRAS EQ SY-LANGU
            )
          INNER JOIN MARC AS D
               ON D~MATNR EQ C~MATNR
            LEFT OUTER JOIN
            ZTMM_MAST AS B
             ON B~MATNR EQ D~MATNR
            AND D~WERKS EQ B~WERKS

        WHERE C~MATNR IN S_MATNR
*         AND a~lgtyp IN ('523','422')
*         AND a~lvorm EQ space
          AND C~PROFL IN ('V','K','M')
*         AND c~tempb NE '11'             " except JIS mat.
*         AND d~mmsta IN ('11','12','13','14')
          AND D~WERKS IN R_PLANT
         %_HINTS ORACLE 'FIRST_ROWS(10)'.


** End of change on 07/28/08
** End of change

  IF SY-SUBRC NE 0.
    MESSAGE S000 WITH 'No data was found.'.
    G_ERROR = TRUE.
    EXIT.
  ENDIF.

  LOOP AT IT_ROW_TAB1.
    IF IT_ROW_TAB1-EWHN IN S_LGNUM.
      MOVE-CORRESPONDING IT_ROW_TAB1 TO IT_ROW_TAB.
      APPEND IT_ROW_TAB.
    ENDIF.
    CLEAR  IT_ROW_TAB.
  ENDLOOP.

  SORT IT_ROW_TAB BY EPART_NO.
  LOOP AT IT_ROW_TAB.
    $IX = SY-TABIX.
    AT NEW EPART_NO.
      $FLAG = TRUE.
    ENDAT.
    IF $FLAG = TRUE.
      CLEAR : $SEQ_NO,$FLAG.
    ENDIF.

    ADD 1 TO $SEQ_NO.
    IT_ROW_TAB-SEQ_NO = $SEQ_NO.
    MODIFY IT_ROW_TAB INDEX $IX TRANSPORTING SEQ_NO.

    IT_MATNR-MATNR = IT_ROW_TAB-EPART_NO.
    IT_MATNR-WERKS = IT_ROW_TAB-EPLANT.
    APPEND IT_MATNR.
  ENDLOOP.

  SORT IT_MATNR.
  DELETE ADJACENT DUPLICATES FROM IT_MATNR.

  SELECT MATNR WERKS LGORT INTO TABLE IT_MARD
  FROM MARD
  FOR ALL ENTRIES IN IT_MATNR
  WHERE MATNR EQ IT_MATNR-MATNR
    AND WERKS EQ IT_MATNR-WERKS
    AND LGORT NE '9999'.

  SELECT PKNUM MATNR WERKS LGTYP LGPLA PRVBE INTO TABLE IT_PKHD
  FROM PKHD
  FOR ALL ENTRIES IN IT_MATNR
  WHERE MATNR EQ IT_MATNR-MATNR
    AND WERKS EQ IT_MATNR-WERKS.

  SORT : IT_MARD BY MATNR WERKS,
         IT_PKHD BY MATNR WERKS.

  LOOP AT IT_ROW_TAB.
    $IX = SY-TABIX.
    READ TABLE IT_MARD WITH KEY MATNR = IT_ROW_TAB-EPART_NO
                                WERKS = IT_ROW_TAB-EPLANT
                                BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_ROW_TAB-ESLOC = IT_MARD-LGORT.
    ENDIF.

    READ TABLE IT_PKHD WITH KEY MATNR = IT_ROW_TAB-EPART_NO
                                WERKS = IT_ROW_TAB-EPLANT
                                BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_ROW_TAB-ECON_TYPE =  IT_PKHD-LGTYP.
      IT_ROW_TAB-ECON_LOC  =  IT_PKHD-LGPLA.
      IT_ROW_TAB-ESUP_AREA =  IT_PKHD-PRVBE.
    ENDIF.
    MODIFY IT_ROW_TAB INDEX $IX TRANSPORTING ESLOC ECON_TYPE
                                             ECON_LOC ESUP_AREA.
  ENDLOOP.

ENDFORM.                    " GATHER_DATA
*&---------------------------------------------------------------------*
*&      Form  SEND_EAI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEND_EAI TABLES LT_SEL STRUCTURE GT_OUT
                             USING    P_FORCE
                             CHANGING P_SUBRC.

  DATA $MATNR LIKE ZSMM_EAI_EPART_NO OCCURS 0 WITH HEADER LINE.
  DATA ITAB LIKE ZSMM_EAI_MAT_MST OCCURS 0 WITH HEADER LINE.
  DATA LW_DATE TYPE DATUM.
  DATA $CNT TYPE I.

  CLEAR P_SUBRC.

  LOOP AT LT_SEL.
    $MATNR-EPART_NO = LT_SEL-EPART_NO.
    APPEND $MATNR.
  ENDLOOP.

  SORT $MATNR.
  DELETE ADJACENT DUPLICATES FROM $MATNR.

  IF P_DATUM IS INITIAL.
    LW_DATE = SY-DATUM.
  ELSE.
    LW_DATE = P_DATUM.
  ENDIF.

  PERFORM GET_ITAB TABLES ITAB $MATNR
                    USING LW_DATE
                          P_FORCE.

  DESCRIBE TABLE ITAB LINES $CNT .

  IF $CNT > 0.

    CALL FUNCTION 'Z_GCS_EAI_MATERIAL_MASTER'
    DESTINATION P_DEST
         TABLES
              EAI_MATERIAL_MST  = ITAB
         EXCEPTIONS
              NO_DATA_WAS_FOUND = 1
              OTHERS            = 2.

    IF SY-SUBRC <> 0.
*      IF sy-subrc EQ 1.
*      MESSAGE s000 WITH 'Error was occured when connect the data!'.
      MESSAGE S000 WITH 'Error while sending date to EAI!'.
*      ENDIF.
      P_SUBRC = 'E'.
*    ENDIF.
      ITAB-TAIT_TARG_RSLT = 'E'.
      MODIFY ITAB TRANSPORTING TAIT_TARG_RSLT WHERE
                                    TAIT_TARG_D EQ LW_DATE.

      MODIFY ZTMM_EAI_MAT_MST FROM TABLE ITAB.

    ELSE.
      IF $CNT EQ 1.
        MESSAGE S000 WITH $CNT
                        ' record has been sent successfully!'.
      ELSE.
        MESSAGE S000 WITH $CNT
                        ' records were have been sent successfully!'.
      ENDIF.

      ITAB-TAIT_TARG_RSLT = 'S'.
      MODIFY ITAB TRANSPORTING TAIT_TARG_RSLT WHERE
                                    TAIT_TARG_D EQ LW_DATE.

      MODIFY ZTMM_EAI_MAT_MST FROM TABLE ITAB.
    ENDIF.

  ENDIF.
ENDFORM.                    " SEND_EAI
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_FROM_Z
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA_FROM_Z TABLES LT_TAB STRUCTURE IT_ROW_TAB.

  SELECT *
      INTO CORRESPONDING FIELDS OF TABLE LT_TAB
        FROM ZTMM_EAI_MAT_MST
          WHERE TAIT_TARG_D EQ P_DATUM
            AND EPART_NO IN S_MATNR
            AND EPLANT IN S_WERKS.

ENDFORM.                    " GET_DATA_FROM_Z
*&---------------------------------------------------------------------*
*&      Form  apply_icon
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM APPLY_ICON.
  LOOP AT GT_OUT.
    CASE GT_OUT-TAIT_TARG_RSLT.
      WHEN SPACE.
      WHEN 'S'.
        GT_OUT-ICON = ICON_LED_GREEN.
      WHEN 'E'.
        GT_OUT-ICON = ICON_LED_RED.
    ENDCASE.
    MODIFY GT_OUT INDEX SY-TABIX TRANSPORTING ICON.
  ENDLOOP.
ENDFORM.                    " apply_icon
*&---------------------------------------------------------------------*
*&      Form  CHK_SENT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHK_SENT_DATA.
  DATA $IX LIKE SY-TABIX.

  SORT $IT_ROW_TAB BY TAIT_TARG_D EPART_NO SEQ_NO.
  LOOP AT GT_OUT.
    $IX = SY-TABIX.
    READ TABLE $IT_ROW_TAB WITH KEY TAIT_TARG_D = P_DATUM
                                    EPART_NO = GT_OUT-EPART_NO
                                    SEQ_NO   = GT_OUT-SEQ_NO
                                    BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GT_OUT-TAIT_TARG_RSLT = $IT_ROW_TAB-TAIT_TARG_RSLT.
      MODIFY GT_OUT INDEX $IX TRANSPORTING TAIT_TARG_RSLT.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " CHK_SENT_DATA
*&---------------------------------------------------------------------*
*&      Form  get_itab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ITAB  text
*      -->P_P_DATUM  text
*      -->P_P_FORCE  text
*----------------------------------------------------------------------*
FORM GET_ITAB TABLES   P_ITAB STRUCTURE ZSMM_EAI_MAT_MST
                       IT_EPART_NO STRUCTURE ZSMM_EAI_EPART_NO
              USING    P_DATE
                       P_FORCE_IF.

  DATA LOGIC(50) OCCURS 10 WITH HEADER LINE.       " select condition

  __CLS P_ITAB.

  IF P_FORCE_IF NE 'R'.
    CONCATENATE 'TAIT_TARG_RSLT' ' = '' '' ' INTO LOGIC.
    APPEND LOGIC.
  ENDIF.

  IF IT_EPART_NO[] IS INITIAL.
    SELECT *
        INTO CORRESPONDING FIELDS OF TABLE P_ITAB
          FROM ZTMM_EAI_MAT_MST
            WHERE TAIT_TARG_D EQ P_DATE
              AND (LOGIC).
  ELSE.
    SORT IT_EPART_NO.
    DELETE ADJACENT DUPLICATES FROM IT_EPART_NO.

    SELECT *
        INTO CORRESPONDING FIELDS OF TABLE P_ITAB
          FROM ZTMM_EAI_MAT_MST
          FOR ALL ENTRIES IN IT_EPART_NO
            WHERE TAIT_TARG_D EQ P_DATE
              AND EPART_NO EQ IT_EPART_NO-EPART_NO
              AND (LOGIC).
  ENDIF.

ENDFORM.                    " get_itab
*&---------------------------------------------------------------------*
*&      Form  exclude_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXCLUDE_STATUS.

* Removing unnecessary menus and standard tools.
  REFRESH GT_EXCLUDING.
  L_EXCLUDING-FCODE = '&UMC'.
  APPEND L_EXCLUDING TO GT_EXCLUDING.
  L_EXCLUDING-FCODE = '&AVR'.
  APPEND L_EXCLUDING TO GT_EXCLUDING.
  L_EXCLUDING-FCODE = '&MIN'.
  APPEND L_EXCLUDING TO GT_EXCLUDING.
  L_EXCLUDING-FCODE = '&MAX'.
  APPEND L_EXCLUDING TO GT_EXCLUDING.
  L_EXCLUDING-FCODE = '&GRAPH'.
  APPEND L_EXCLUDING TO GT_EXCLUDING.
  L_EXCLUDING-FCODE = 'SAVE'.
  APPEND L_EXCLUDING TO GT_EXCLUDING.
  L_EXCLUDING-FCODE = 'IEAI'.
  APPEND L_EXCLUDING TO GT_EXCLUDING.
  L_EXCLUDING-FCODE = 'REAI'.
  APPEND L_EXCLUDING TO GT_EXCLUDING.

ENDFORM.                    " exclude_status

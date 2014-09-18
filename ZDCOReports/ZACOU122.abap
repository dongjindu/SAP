*----------------------------------------------------------------------
* Program ID        : ZACOU122
* Title             : [CO] Following Part Link
* Created on        : 04/27/2007
* Created by        : I.G.MOON
* Specifications By : Andy Choi
* Description       : Following Part Link
*----------------------------------------------------------------------
REPORT ZACOU122 MESSAGE-ID ZMCO.
INCLUDE ZACOUI00.

*----------------------------------------------------------------------*
*  Define Variant & tables & local class
*----------------------------------------------------------------------*
TABLES : ZTCOU105,       " [CO] Following Part Information
         ZTCOU106.       " [CO] Calculate Variances

*----------------------------------------------------------------------*
TYPES: BEGIN OF TY_106,
         KOKRS  TYPE KOKRS,          " Cotrolling Area
         BDATJ  TYPE BDATJ,          " Fiscal Year
         ID     TYPE ZID1,           " ID
         UPGVC  TYPE ZUPGVC,         " UPG-VC
         KZUST  TYPE KZUST,          " Reason Code
         COMPN  TYPE IDNRK,          " BOM component
         PMENGE TYPE ZPMENGE,        " Prev. Qty
         MENGE  TYPE MENGE_POS,      " Current Qty
         DMENGE TYPE ZDMENGE,        " Diff.Qty
         MEEHT  TYPE MEINS,          " Base unit of measure
         LIFNR  TYPE LIFNR,
         TMATNR TYPE MATNR,          " Following Part
         DATUV  TYPE DATUV,          " Valid-from date
       END OF TY_106.

TYPES: BEGIN OF TY_ITAB,
         KOKRS  TYPE KOKRS,          " Cotrolling Area
         BDATJ  TYPE BDATJ,          " Fiscal Year
         ID     TYPE ZID1,           " ID
         UPGVC  TYPE ZUPGVC,         " UPG-VC
         UPGTX  TYPE MAKTX,          " TEXT
         KZUST  TYPE KZUST,          " Reason Code
         COMPN  TYPE IDNRK,          " BOM component
         COMTX  TYPE MAKTX,          " TEXT
         PMENGE TYPE ZPMENGE,        " Prev. Qty
         MENGE  TYPE MENGE_POS,      " Current Qty
         DMENGE TYPE ZDMENGE,        " Diff.Qty
         MEEHT  TYPE MEINS,          " Base unit of measure
         LIFNR  TYPE LIFNR,
         TMATNR TYPE MATNR,          " Following Part
         DATUV  TYPE DATUV,          " Valid-from date
        END OF TY_ITAB.

TYPES: BEGIN OF TY_OUT.
INCLUDE  TYPE TY_ITAB.
TYPES   CELLTAB  TYPE LVC_T_STYL. "not use right now.
TYPES   TABCOLOR TYPE SLIS_T_SPECIALCOL_ALV.
TYPES: END OF TY_OUT.

DATA: GT_106        TYPE TABLE OF TY_106   WITH HEADER LINE,
      GT_OUT        TYPE TABLE OF TY_OUT   WITH HEADER LINE,
      GT_ZTCOU105   TYPE TABLE OF ZTCOU105 WITH HEADER LINE .

TYPES DDSHRETVAL_TABLE TYPE TABLE OF DDSHRETVAL.
*----------------------------------------------------------------------*
DATA: BEZEI       TYPE BEZEI,
      GV_CNT      TYPE I,
      INFO(80),
      GV_INDEX    TYPE I.
DATA  FLAG_DATA_CHANGED.

RANGES R_KZUST FOR ZTCOU106-KZUST.

*----------------------------------------------------------------------*
* Define local class
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF ZTCOU105_KEY.
    TYPES:   KOKRS TYPE KOKRS.
    TYPES:   TMATNR TYPE MATNR.
    TYPES:   FMATNR TYPE MATNR.
    TYPES: END OF ZTCOU105_KEY.

    TYPES: ZTCOU105_KEYS TYPE STANDARD TABLE OF ZTCOU105_KEY,
           ZTCOU105_TABLE TYPE STANDARD TABLE OF ZTCOU105.

    METHODS:
      HANDLE_DATA_CHANGED
         FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
             IMPORTING ER_DATA_CHANGED,
                       GET_DELETED_ROWS
             EXPORTING
                       DELETED_ROWS TYPE ZTCOU105_TABLE,

      REFRESH_DELTA_TABLES,

      HANDLE_DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
             IMPORTING E_ROW
                       E_COLUMN
                       ES_ROW_NO,

      ON_F4 FOR EVENT ONF4 OF CL_GUI_ALV_GRID
                IMPORTING SENDER
                          E_FIELDNAME
                          E_FIELDVALUE
                          ES_ROW_NO
                          ER_EVENT_DATA
                          ET_BAD_CELLS
                          E_DISPLAY,

      MY_F4 IMPORTING SENDER        TYPE REF TO CL_GUI_ALV_GRID
                      ET_BAD_CELLS  TYPE LVC_T_MODI
                      ES_ROW_NO     TYPE LVC_S_ROID
                      ER_EVENT_DATA TYPE REF TO CL_ALV_EVENT_DATA
                      E_DISPLAY     TYPE C
                      E_FIELDNAME   TYPE LVC_FNAME
            EXPORTING LT_F4         TYPE DDSHRETVAL_TABLE.

  PRIVATE SECTION.
    DATA DELETED_ROWS TYPE STANDARD TABLE OF ZTCOU105.

* This flag is set if any error occured in one of the
* following methods:
    DATA: ERROR_IN_DATA TYPE C.
    METHODS:
      UPDATE_DELTA_TABLES
         IMPORTING
            PR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.

ENDCLASS.                   " LCL_EVENT_RECEIVER Definition

*----------------------------------------------------------------------*
* Implementation local class
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

* Setting for Change data
  METHOD HANDLE_DATA_CHANGED.

* remember deleted lines for saving
    CALL METHOD UPDATE_DELTA_TABLES( ER_DATA_CHANGED ).

    PERFORM DATA_CHANGED USING ER_DATA_CHANGED.
  ENDMETHOD.                    " handle_data_changed

  METHOD GET_DELETED_ROWS.
    DELETED_ROWS = ME->DELETED_ROWS.
  ENDMETHOD.

  METHOD REFRESH_DELTA_TABLES.
    CLEAR ME->DELETED_ROWS[].
  ENDMETHOD.

  METHOD UPDATE_DELTA_TABLES.
    DATA: L_DEL_ROW TYPE LVC_S_MOCE,
          LS_KEY TYPE ZTCOU105_KEY,
          LS_ZTCOU105 TYPE ZTCOU105,
          LS_OUTTAB LIKE LINE OF GT_OUT.

    LOOP AT PR_DATA_CHANGED->MT_DELETED_ROWS INTO L_DEL_ROW.
      READ TABLE GT_OUT INTO LS_OUTTAB INDEX L_DEL_ROW-ROW_ID.
      IF SY-SUBRC NE 0.
        MESSAGE I000(0K) WITH TEXT-E01. "Internal error
      ELSE.
        MOVE-CORRESPONDING LS_OUTTAB TO LS_ZTCOU105.
        LS_ZTCOU105-FMATNR = LS_OUTTAB-COMPN.
        APPEND LS_ZTCOU105 TO DELETED_ROWS.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

* Double Click
  METHOD HANDLE_DOUBLE_CLICK.
    PERFORM DOUBLE_CLICK USING E_ROW
                               E_COLUMN
                               ES_ROW_NO.
  ENDMETHOD.                    " handle_data_changed

* Get values of possible entries
  METHOD ON_F4.
    PERFORM ON_F4 USING SENDER
                        E_FIELDNAME
                        E_FIELDVALUE
                        ES_ROW_NO
                        ER_EVENT_DATA
                        ET_BAD_CELLS
                        E_DISPLAY
                        'GT_OUT'.
  ENDMETHOD.                                                " ON_F4

  METHOD MY_F4.
    PERFORM MY_F4 TABLES LT_F4
                  USING  SENDER
                         ET_BAD_CELLS
                         ES_ROW_NO
                         ER_EVENT_DATA
                         E_DISPLAY
                         E_FIELDNAME
                         'GT_OUT'.
  ENDMETHOD.

ENDCLASS.                   " LCL_EVENT_RECEIVER Implementation

DATA G_EVENT_RECEIVER  TYPE REF TO LCL_EVENT_RECEIVER.

****************************** Macros **********************************
DEFINE __CLS.                          " clear & refresh
  CLEAR &1.REFRESH &1.
END-OF-DEFINITION.

DEFINE __SET_REFRESH_MODE.
  STABLE-ROW = &1.
  STABLE-COL = &1.
END-OF-DEFINITION.

DEFINE __FOCUS.
  CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
      EXPORTING
        CONTROL = &1 .
END-OF-DEFINITION.

DEFINE __PROCESS.
  PERFORM SHOW_PROGRESS USING TEXT-S01 &1.
END-OF-DEFINITION.

****************************** constants *******************************
CONSTANTS:  FALSE VALUE ' ',
            TRUE  VALUE 'X'.

*----------------------------------------------------------------------
* Selection-Screen
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B0 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_KOKRS LIKE ZTCOU106-KOKRS OBLIGATORY
                                        MEMORY ID CAC
                                        MATCHCODE OBJECT FC_KOKRS,
            P_YEAR  LIKE ZTCOU106-BDATJ OBLIGATORY MEMORY ID BDTJ.
SELECT-OPTIONS: S_POPER FOR ZTCOU106-POPER OBLIGATORY,
                S_ID    FOR ZTCOU106-ID MATCHCODE OBJECT ZID,
                S_UPGVC FOR ZTCOU106-UPGVC,
                S_KZUST FOR ZTCOU106-KZUST DEFAULT 'E4',
                S_COMPN FOR ZTCOU106-COMPN,
                S_LIFNR FOR ZTCOU106-LIFNR.
SELECTION-SCREEN END OF BLOCK B0.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-011.
PARAMETER P_CROS AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B2.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-010.
PARAMETER P_VARI TYPE SLIS_VARI.
SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.
  PERFORM ALV_VARIANT_F4 CHANGING P_VARI.

* Possible entries for Reason
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_KZUST-LOW.
  PERFORM POPUP_RSN USING S_KZUST-LOW 'S_KZUST-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_KZUST-HIGH.
  PERFORM POPUP_RSN USING S_KZUST-HIGH 'S_KZUST-HIGH'.

*----------------------------------------------------------------------*
* Start of selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM GET_DATA.
  CLEAR FLAG_DATA_CHANGED.

  CALL SCREEN 100.

END-OF-SELECTION.
* nothing

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET TITLEBAR '100'.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.
  CASE OK_CODE.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'SAVE'.
      PERFORM SAVE_DATA.
    WHEN 'SWITCH'.
      PERFORM SWITCH_EDIT_MODE.
      __FOCUS G_GRID.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA.

  PERFORM CONVERT_REASON_FOR_SELECTION.

  IF R_KZUST[] IS INITIAL.
    R_KZUST[] = S_KZUST[].
  ENDIF.

  __CLS GT_106.

  __PROCESS '10'.
  SELECT A~KOKRS A~BDATJ A~ID A~UPGVC A~KZUST A~COMPN
         A~PMENGE A~MENGE A~DMENGE A~MEEHT A~LIFNR
         B~TMATNR B~DATUV
         INTO TABLE GT_106
      FROM ZTCOU106 AS A
      LEFT OUTER JOIN ZTCOU105 AS B
        ON B~KOKRS  = A~KOKRS
       AND B~FMATNR = A~COMPN
     WHERE A~KOKRS = P_KOKRS
       AND A~BDATJ = P_YEAR
       AND A~ID IN S_ID
       AND A~UPGVC IN S_UPGVC
       AND A~POPER IN S_POPER
       AND A~COMPN IN S_COMPN
       AND A~LIFNR IN S_LIFNR
       AND KZUST1  IN R_KZUST.
  IF SY-SUBRC NE 0.
    MESSAGE S000 WITH 'Can not find material to register.'.
    EXIT.
  ENDIF.

  SORT GT_106 BY KOKRS BDATJ ID UPGVC KZUST COMPN.

  PERFORM GET_GT_OUT.
  PERFORM INFO_TEXT_SET USING FALSE.

  __PROCESS '90'. "90%

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
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

  __CATALOG :
          'X'  'ID'      'ID'               10  'CHAR' '' '',
          'X'  'UPGVC'   'UPG'              18  'CHAR' '' '',
          'X'  'UPGTX'   'Description'      20  'CHAR' '' '',
          'X'  'KZUST'   'Reason'           10  'CHAR' '' '',
          'X'  'COMPN'   'Part'             18  'CHAR' '' '',
          'X'  'COMTX'   'Description'      20  'CHAR' '' '',
          ' '  'PMENGE'  'Prev.Qty'         18  'QUAN' '' '',
          ' '  'MENGE'   'Curr.Qty'         18  'QUAN' '' '',
          ' '  'DMENGE'  'Diff.Qty'         18  'QUAN' '' '',
          ' '  'MEEHT'   'UoM'               3  'UNIT' '' '',
          ' '  'LIFNR'   'Vendor'           10  'CHAR' '' '',
          ' '  'TMATNR'  'Following Part'   18  'CHAR' '' 'X',
          ' '  'DATUV'   'Valid-from date'  8   'DATS' '' 'X'.

  LOOP AT GT_FCAT INTO GS_FCAT.
    CASE GS_FCAT-FIELDNAME.
      WHEN 'ID'.
        GS_FCAT-REF_FIELD = GS_FCAT-FIELDNAME.
        GS_FCAT-REF_TABLE = 'ZTCOU106'.
        MODIFY GT_FCAT FROM GS_FCAT.
      WHEN 'UPGVC'.
        GS_FCAT-REF_FIELD = 'MATNR'.
        GS_FCAT-REF_TABLE = 'MARA'.
        MODIFY GT_FCAT FROM GS_FCAT.
      WHEN 'COMPN'.
        GS_FCAT-REF_FIELD = GS_FCAT-FIELDNAME.
        GS_FCAT-REF_TABLE = 'ZTCOU106'.
        MODIFY GT_FCAT FROM GS_FCAT.
      WHEN 'DMENGE' OR 'PMENGE' OR 'MENGE'.
        GS_FCAT-QFIELDNAME = 'MEEHT'.
        GS_FCAT-REF_FIELD = GS_FCAT-FIELDNAME.
        GS_FCAT-REF_TABLE = 'ZTCOU106'.
        GS_FCAT-JUST = 'R'.
        MODIFY GT_FCAT FROM GS_FCAT.
      WHEN 'MEEHT' .
        GS_FCAT-NO_OUT = 'X'.
*        gs_fcat-just = 'C'.
*        gs_fcat-convexit = 'CUNIT'.
*        gs_fcat-ref_field = gs_fcat-fieldname.
*        gs_fcat-ref_table = 'ZTCOU106'.
        MODIFY GT_FCAT FROM GS_FCAT.
      WHEN 'LIFNR' .
        GS_FCAT-REF_FIELD = GS_FCAT-FIELDNAME.
        GS_FCAT-REF_TABLE = 'LFA1'.
        MODIFY GT_FCAT FROM GS_FCAT.
      WHEN 'TMATNR' .
        GS_FCAT-REF_FIELD = GS_FCAT-FIELDNAME.
        GS_FCAT-REF_TABLE = 'ZTCOU105'.
        MODIFY GT_FCAT FROM GS_FCAT.
      WHEN 'DATUV' .
        GS_FCAT-REF_FIELD = GS_FCAT-FIELDNAME.
        GS_FCAT-REF_TABLE = 'ZTCOU105'.
        MODIFY GT_FCAT FROM GS_FCAT.
      WHEN 'KZUST'.
        GS_FCAT-F4AVAILABL = 'X'.
        MODIFY GT_FCAT FROM GS_FCAT.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " CREATE_FIELD_CATEGORY

*&---------------------------------------------------------------------*
*&      Form  SORT_BUILD
*&---------------------------------------------------------------------*
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

  SORT_TAB : 'ID'       '1' 'X' 'X' '' 'X',
             'UPGVC'    '2' 'X' 'X' '' 'X',
             'UPGTX'    ' ' 'X' 'X' '' 'X',
             'KZUST'    '3' 'X' 'X' '' 'X',
             'COMPN'    '4' 'X' 'X' '' 'X',
             'COMTX'    ' ' 'X' 'X' '' 'X',
             'LIFNR'    ' ' 'X' 'X' '' 'X',
             'TMATNR'   ' ' 'X' 'X' '' 'X'.

ENDFORM.                    " SORT_BUILD
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED
*&---------------------------------------------------------------------*
*       Event of changed data
*----------------------------------------------------------------------*
*      -->RR_DATA_CHANGED  Log is Visible
*----------------------------------------------------------------------*
FORM DATA_CHANGED USING RR_DATA_CHANGED
                        TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.

  FLAG_DATA_CHANGED = TRUE.

  DATA: LS_MOD_CELLS TYPE LVC_S_MODI,
        LS_CELLS     TYPE LVC_S_MODI,
        LT_VALUES TYPE TABLE OF BAPI_CHAR_VALUES WITH HEADER LINE.

  DATA: $COMPN LIKE GT_OUT-COMPN,
        $DATUV LIKE GT_OUT-DATUV,
        $CRS_COMP  LIKE GT_OUT-COMPN.

  LOOP AT RR_DATA_CHANGED->MT_GOOD_CELLS INTO LS_MOD_CELLS.
    READ TABLE GT_OUT INDEX LS_MOD_CELLS-ROW_ID.
    IF SY-SUBRC = 0.
      $COMPN = GT_OUT-COMPN .
      CALL METHOD RR_DATA_CHANGED->MODIFY_CELL
                EXPORTING I_ROW_ID    = LS_MOD_CELLS-ROW_ID
                          I_FIELDNAME = LS_MOD_CELLS-FIELDNAME
                          I_VALUE     = LS_MOD_CELLS-VALUE.

      $CRS_COMP = LS_MOD_CELLS-VALUE.
      MODIFY GT_OUT INDEX LS_MOD_CELLS-ROW_ID.
    ENDIF.
  ENDLOOP.

* change other tmatnr, if it has same 'BOM component'

  CHECK NOT $COMPN IS INITIAL .

  IF LS_MOD_CELLS-FIELDNAME EQ 'TMATNR'.
    GT_OUT-TMATNR = LS_MOD_CELLS-VALUE.
    MODIFY GT_OUT TRANSPORTING TMATNR WHERE COMPN = $CRS_COMP. "$COMPN .
  ENDIF.

  IF P_CROS EQ TRUE.
    IF NOT $CRS_COMP IS INITIAL.
      IF LS_MOD_CELLS-FIELDNAME EQ 'TMATNR'.
        GT_OUT-TMATNR = $COMPN .
        MODIFY GT_OUT TRANSPORTING TMATNR WHERE COMPN = $CRS_COMP .
      ENDIF.
    ENDIF.
  ENDIF.

  IF LS_MOD_CELLS-FIELDNAME EQ 'DATUV'.
    GT_OUT-DATUV = LS_MOD_CELLS-VALUE.
    MODIFY GT_OUT TRANSPORTING DATUV WHERE COMPN = $COMPN .
  ENDIF.

*  __SET_REFRESH_MODE TRUE.
*  CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY
*       EXPORTING IS_STABLE = STABLE.

ENDFORM.                    " DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       Save data to table ZTCOU105
*----------------------------------------------------------------------*
FORM SAVE_DATA.
  DATA ANSWER.
  IF FLAG_DATA_CHANGED NE TRUE.

    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
         EXPORTING
              DEFAULTOPTION  = 'Y'
              TEXTLINE1      = 'Data has not been changed.'
              TEXTLINE2      = 'Do you want to save anyway?'
              TITEL          = 'Save'
              CANCEL_DISPLAY = 'X'
         IMPORTING
              ANSWER         = ANSWER
         EXCEPTIONS
              OTHERS         = 1.

    CHECK ANSWER EQ 'J'.
  ENDIF.
  PERFORM UPDATE_DATABASE.
  PERFORM REFRESH_ALV.
  __FOCUS G_GRID.

ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
FORM SET_LVC_LAYOUT.
  CLEAR GS_LAYO.

  GS_LAYO-ZEBRA      = 'X'.
  GS_LAYO-SEL_MODE   = 'A'.       " Column and row selection
  GS_LAYO-CWIDTH_OPT = 'X'.
  GS_LAYO-CTAB_FNAME = 'TABCOLOR'.
*  gs_layo-stylefname = 'CELLTAB'.

ENDFORM.                    " SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
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
*&      Form  GET_GT_OUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_GT_OUT.

  DATA L_RESP TYPE ZCORSP.
  DATA LT_MAKT  LIKE MAKT OCCURS 0 WITH HEADER LINE.
  DATA  : BEGIN OF LT_MATNR OCCURS 0,
           MATNR LIKE MAKT-MATNR,
          END OF LT_MATNR.

  __PROCESS '40'.
* Get Material info.
  LOOP AT GT_106.
    LT_MATNR-MATNR = GT_106-UPGVC .
    APPEND LT_MATNR.
    LT_MATNR-MATNR = GT_106-COMPN .
    APPEND LT_MATNR.
  ENDLOOP.

  __PROCESS '50'.
  DELETE ADJACENT DUPLICATES FROM LT_MATNR.

  SELECT * FROM MAKT INTO TABLE LT_MAKT
     FOR ALL ENTRIES IN LT_MATNR
   WHERE MATNR = LT_MATNR-MATNR AND SPRAS = SY-LANGU.

  SORT LT_MAKT BY MATNR.

  __CLS GT_OUT.

  __PROCESS '60'.
  LOOP AT GT_106.
    MOVE-CORRESPONDING GT_106 TO GT_OUT.
    CONCATENATE GT_106-KZUST+0(1) GT_106-KZUST+2(1) INTO GT_OUT-KZUST.
    READ TABLE LT_MAKT WITH KEY MATNR = GT_106-UPGVC
                                BINARY SEARCH.
    IF SY-SUBRC EQ 0 .
      GT_OUT-UPGTX = LT_MAKT-MAKTX.
    ENDIF.
    READ TABLE LT_MAKT WITH KEY MATNR = GT_106-COMPN
                                BINARY SEARCH.
    IF SY-SUBRC EQ 0 .
      GT_OUT-COMTX = LT_MAKT-MAKTX.
    ENDIF.
    APPEND GT_OUT.
  ENDLOOP.
  __PROCESS '70'.

  DESCRIBE TABLE GT_OUT LINES GV_CNT.

ENDFORM.                    " GET_GT_OUT
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

  __COLOR : 'ID'     '1' 0,
            'UPGVC'  '2' 0,
            'UPGTX'  '2' 1,
            'KZUST'  '4' 0,
            'COMPN'  '2' 0,
            'COMTX'  '2' 1,
            'LIFNR'  '5' 0,
            'PMENGE' '7' 0,
            'MENGE'  '3' 0,
            'DMENGE' '6' 0.
  GT_OUT-TABCOLOR[] = GT_SPECIALCOL[].
  MODIFY GT_OUT TRANSPORTING TABCOLOR WHERE TABCOLOR IS INITIAL.
ENDFORM.                    " SET_COLOR
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
*&      Form  CREATE_F4_FIELDS
*&---------------------------------------------------------------------*
*       Define possible entry fields
*----------------------------------------------------------------------*
FORM CREATE_F4_FIELDS USING P_GRID.
  CLEAR: GS_F4, GT_F4, GT_F4[].

* F4 FIELD
  GS_F4-FIELDNAME  = 'KZUST'.
  GS_F4-REGISTER   = 'X'.
  APPEND GS_F4 TO GT_F4.

  CALL METHOD G_GRID->REGISTER_F4_FOR_FIELDS
         EXPORTING IT_F4 = GT_F4.

ENDFORM.                    " CREATE_F4_FIELDS
*&---------------------------------------------------------------------*
*&      Form  ON_F4
*&---------------------------------------------------------------------*
*       Define possible entries
*----------------------------------------------------------------------*
FORM ON_F4 USING SENDER         TYPE REF TO CL_GUI_ALV_GRID
                 E_FIELDNAME    TYPE LVC_FNAME
                 E_FIELDVALUE   TYPE LVC_VALUE
                 ES_ROW_NO      TYPE LVC_S_ROID
                 ER_EVENT_DATA  TYPE REF TO CL_ALV_EVENT_DATA
                 ET_BAD_CELLS   TYPE LVC_T_MODI
                 E_DISPLAY      TYPE CHAR01
                 P_TAB.

  DATA LT_F4 TYPE TABLE OF DDSHRETVAL.

* Call my personal f4-help
  CLEAR: LT_F4, LT_F4[].

  CALL METHOD G_EVENT_RECEIVER->MY_F4
    EXPORTING
      SENDER        = SENDER
      ES_ROW_NO     = ES_ROW_NO
      ER_EVENT_DATA = ER_EVENT_DATA
      ET_BAD_CELLS  = ET_BAD_CELLS
      E_DISPLAY     = E_DISPLAY
      E_FIELDNAME   = E_FIELDNAME
    IMPORTING
      LT_F4         = LT_F4.

* Assign the cell table fieldsymbol to the dereferenced data table and
* fill the table.
  ASSIGN ER_EVENT_DATA->M_DATA->* TO <F4TAB>.

  READ TABLE LT_F4 INTO LS_F4 INDEX 1.

  CHECK NOT LS_F4 IS INITIAL.

  PERFORM F4_APLY USING ES_ROW_NO-ROW_ID
                        LS_F4-FIELDNAME
                        P_TAB.

* To avoid standard f4-help.
  ER_EVENT_DATA->M_EVENT_HANDLED = 'X'.

ENDFORM.                                                    " ON_F4
*&---------------------------------------------------------------------*
*&      Form  F4_APLY
*&---------------------------------------------------------------------*
FORM F4_APLY USING  ES_ROW_NO_ROW_ID
                    E_FIELDNAME TYPE FIELDNAME
                    P_TAB.
  LS_MODI-ROW_ID    = ES_ROW_NO_ROW_ID.
  LS_MODI-FIELDNAME = E_FIELDNAME.
  LS_MODI-VALUE     = LS_F4-FIELDVAL.
  APPEND LS_MODI TO <F4TAB>.
  READ TABLE GT_OUT INDEX ES_ROW_NO_ROW_ID.

ENDFORM.                                                    " F4_APLY
*&---------------------------------------------------------------------*
*&      Form  MY_F4
*&---------------------------------------------------------------------*
FORM MY_F4 TABLES ET_F4         STRUCTURE DDSHRETVAL
           USING  SENDER        TYPE REF TO CL_GUI_ALV_GRID
                  ET_BAD_CELLS  TYPE LVC_T_MODI
                  ES_ROW_NO     TYPE LVC_S_ROID
                  ER_EVENT_DATA TYPE REF TO CL_ALV_EVENT_DATA
                  E_DISPLAY     TYPE C
                  E_FIELDNAME   TYPE LVC_FNAME
                  P_TAB.

  DATA : LS_OUT        LIKE LINE OF GT_OUT,
         LT_FCAT       TYPE LVC_T_FCAT,
         LS_FIELDCAT   TYPE LVC_S_FCAT,
         LV_TABNAME    TYPE DD03V-TABNAME,
         LV_FIELDNAME  TYPE DD03V-FIELDNAME,
         LV_HELP_VALUE TYPE HELP_INFO-FLDVALUE,
         LT_BAD_CELL   TYPE LVC_T_MODI,
         L_WA          TYPE REF TO DATA.

  FIELD-SYMBOLS : <L_FIELD_VALUE> TYPE ANY,
                  <LS_WA>         TYPE ANY.

  CALL METHOD SENDER->GET_FRONTEND_FIELDCATALOG
    IMPORTING
      ET_FIELDCATALOG = LT_FCAT.

  READ TABLE GT_OUT INDEX ES_ROW_NO-ROW_ID INTO LS_OUT.

  IF SY-SUBRC = 0.
    CREATE DATA L_WA LIKE LINE OF GT_OUT.
    ASSIGN L_WA->* TO <LS_WA>.
    <LS_WA> = LS_OUT.
  ENDIF.

  READ TABLE LT_FCAT WITH KEY FIELDNAME = E_FIELDNAME
                     INTO LS_FIELDCAT.
  IF SY-SUBRC = 0.
    LV_TABNAME = LS_FIELDCAT-REF_TABLE.
    LV_FIELDNAME = LS_FIELDCAT-FIELDNAME.

    ASSIGN COMPONENT LS_FIELDCAT-FIELDNAME
                  OF STRUCTURE LS_OUT TO <L_FIELD_VALUE>.

    WRITE <L_FIELD_VALUE> TO LV_HELP_VALUE.
  ENDIF.

  PERFORM F4_SET IN PROGRAM BCALV_F4
                 USING SENDER
                       LT_FCAT
                       LT_BAD_CELL
                       ES_ROW_NO-ROW_ID
                       <LS_WA>.

  IF E_FIELDNAME = 'KZUST'.
    PERFORM F4_REASON USING E_FIELDNAME.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            RETFIELD        = E_FIELDNAME
       TABLES
            FIELD_TAB       = GT_FIELDS
            VALUE_TAB       = GT_VALUES
            RETURN_TAB      = ET_F4[]
       EXCEPTIONS
            PARAMETER_ERROR = 1
            NO_VALUES_FOUND = 2
            OTHERS          = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                                                    " MY_F4
*&---------------------------------------------------------------------*
*&      Form  DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*      -->P_E_COLUMN  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM DOUBLE_CLICK USING  E_ROW     TYPE LVC_S_ROW
                         E_COLUMN  TYPE LVC_S_COL
                         ES_ROW_NO TYPE LVC_S_ROID.
  CLEAR GV_INDEX.
  GV_INDEX = E_ROW-INDEX.

  READ TABLE GT_OUT INDEX GV_INDEX.
  IF SY-SUBRC = 0.
    IF E_COLUMN = 'UPGVC'.
      CHECK GT_OUT-UPGVC NE SPACE.
      SET PARAMETER ID 'MAT'  FIELD GT_OUT-UPGVC.
      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
    ENDIF.
    IF E_COLUMN = 'COMPN'.
      CHECK GT_OUT-COMPN NE SPACE.
      SET PARAMETER ID 'MAT'  FIELD GT_OUT-COMPN.
      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
    ENDIF.
    IF E_COLUMN = 'TMATNR'.
      CHECK GT_OUT-TMATNR NE SPACE.
      SET PARAMETER ID 'MAT'  FIELD GT_OUT-TMATNR.
      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
    ENDIF.
  ENDIF.

ENDFORM.                    " DOUBLE_CLICK
*&---------------------------------------------------------------------*
*&      Form  switch_edit_mode
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
    SET PF-STATUS '100' EXCLUDING 'SAVE'.
    PERFORM INFO_TEXT_SET USING FALSE.
  ENDIF.

ENDFORM.                    " switch_edit_mode
*&---------------------------------------------------------------------*
*&      Form  default_screen_mode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DEFAULT_SCREEN_MODE USING EDIT_MODE.
  DATA: LT_CELLTAB TYPE LVC_T_STYL,
        L_INDEX TYPE I.

  LOOP AT GT_OUT.
    L_INDEX = SY-TABIX.
    __CLS : LT_CELLTAB,GT_OUT-CELLTAB .

    PERFORM FILL_CELLTAB USING EDIT_MODE
                         CHANGING LT_CELLTAB.
    INSERT LINES OF LT_CELLTAB INTO TABLE GT_OUT-CELLTAB.
    MODIFY GT_OUT INDEX L_INDEX.
  ENDLOOP .

  SET PF-STATUS '100' EXCLUDING 'SAVE'.

ENDFORM.                    " default_screen_mode
*&---------------------------------------------------------------------*
*&      Form  fill_celltab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EDIT_MODE  text
*      <--P_LT_CELLTAB  text
*----------------------------------------------------------------------*
FORM FILL_CELLTAB USING VALUE(P_MODE)
                  CHANGING PT_CELLTAB TYPE LVC_T_STYL.
  DATA: LS_CELLTAB TYPE LVC_S_STYL.

  DEFINE __SET_MODE.
    LS_CELLTAB-FIELDNAME = &1.
    LS_CELLTAB-STYLE = &2.
    INSERT LS_CELLTAB INTO TABLE PT_CELLTAB.
  END-OF-DEFINITION.

  __SET_MODE :
          'ID' CL_GUI_ALV_GRID=>MC_STYLE_DISABLED,
          'UPG' CL_GUI_ALV_GRID=>MC_STYLE_DISABLED,
          'Description' CL_GUI_ALV_GRID=>MC_STYLE_DISABLED,
          'Reason' CL_GUI_ALV_GRID=>MC_STYLE_DISABLED,
          'Part' CL_GUI_ALV_GRID=>MC_STYLE_DISABLED,
          'Description' CL_GUI_ALV_GRID=>MC_STYLE_DISABLED,
          'Prev.Qty' CL_GUI_ALV_GRID=>MC_STYLE_DISABLED,
          'Curr.Qty' CL_GUI_ALV_GRID=>MC_STYLE_DISABLED,
          'Diff.Qty' CL_GUI_ALV_GRID=>MC_STYLE_DISABLED,
          'Vendor' CL_GUI_ALV_GRID=>MC_STYLE_DISABLED .

  IF P_MODE EQ TRUE.
    __SET_MODE :
            'TMATNR' CL_GUI_ALV_GRID=>MC_STYLE_ENABLED,
            'DATUV' CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
  ELSE.
    __SET_MODE :
            'TMATNR' CL_GUI_ALV_GRID=>MC_STYLE_DISABLED,
            'DATUV' CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
  ENDIF.

ENDFORM.                               " FILL_CELLTAB
*&---------------------------------------------------------------------*
*&      Form  update_database
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_DATABASE.
  DATA: LV_MATNR TYPE MATNR,
        LT_ROW   TYPE LVC_T_ROW,
        LS_ROW   TYPE LVC_S_ROW,
        LT_ROID  TYPE LVC_T_ROID,
        LV_CNT(5),
        LV_DCNT(5),
        LV_MSG(200),                 " Message
        LS_ZTCOU105 LIKE ZTCOU105,
        LT_DEL_ROWS TYPE TABLE OF ZTCOU105.

  DATA  : $GT_OUT LIKE GT_OUT OCCURS 0 WITH HEADER LINE,
          $GJAHR LIKE ZTCOU105-GJAHR,
          $MONAT LIKE ZTCOU105-MONAT.

* Delete Lines
  CALL METHOD G_EVENT_RECEIVER->GET_DELETED_ROWS
            IMPORTING DELETED_ROWS = LT_DEL_ROWS.

  DELETE ZTCOU105 FROM TABLE LT_DEL_ROWS.

  CALL METHOD G_EVENT_RECEIVER->REFRESH_DELTA_TABLES.

* Save seleted data to table ZTCOU105
  CLEAR: LV_CNT, LV_DCNT, LT_ROW[], LT_ROID[].

  $GT_OUT[] = GT_OUT[].

  DELETE $GT_OUT WHERE TMATNR IS INITIAL.

  LOOP AT $GT_OUT.
    CLEAR : $GT_OUT-BDATJ,$GT_OUT-ID, $GT_OUT-UPGVC, $GT_OUT-KZUST,
            $GT_OUT-DMENGE,
            $GT_OUT-LIFNR,$GT_OUT-LIFNR.
    MODIFY $GT_OUT.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM $GT_OUT.

  LOOP AT $GT_OUT.
    CLEAR LV_MATNR.
    SELECT SINGLE FMATNR INTO LV_MATNR
      FROM ZTCOU105
     WHERE KOKRS = P_KOKRS
       AND FMATNR = $GT_OUT-COMPN
       AND TMATNR = $GT_OUT-TMATNR.

    IF SY-SUBRC = 0.
      $GJAHR = GT_OUT-DATUV(4).
      $MONAT = GT_OUT-DATUV+4(2).

      UPDATE ZTCOU105 SET KOKRS = P_KOKRS
                          FMATNR = $GT_OUT-COMPN
                          TMATNR = $GT_OUT-TMATNR
                          DATUV  = $GT_OUT-DATUV
                          GJAHR = $GJAHR
                          MONAT = $MONAT
                          AEDAT = SY-DATUM
                          AENAM = SY-UNAME
                    WHERE KOKRS = P_KOKRS
                      AND FMATNR = $GT_OUT-COMPN
                      AND TMATNR = $GT_OUT-TMATNR.
    ELSE.
      CLEAR LS_ZTCOU105.
      MOVE-CORRESPONDING $GT_OUT TO LS_ZTCOU105.
      LS_ZTCOU105-KOKRS = P_KOKRS.
      LS_ZTCOU105-FMATNR = $GT_OUT-COMPN.
      LS_ZTCOU105-GJAHR = LS_ZTCOU105-DATUV(4).
      LS_ZTCOU105-MONAT = LS_ZTCOU105-DATUV+4(2).
      LS_ZTCOU105-AEDAT = SY-DATUM.
      LS_ZTCOU105-AENAM = SY-UNAME.

      INSERT INTO ZTCOU105 VALUES LS_ZTCOU105.
    ENDIF.

    IF SY-SUBRC = 0.
      LV_CNT = LV_CNT + 1.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE LT_DEL_ROWS LINES LV_DCNT.
  IF LV_DCNT > 0.
    IF LV_CNT > 0.
      CONCATENATE 'Data has been deleted' LV_DCNT  'records,'
                  'saved' LV_CNT 'records.'
             INTO LV_MSG SEPARATED BY SPACE.
    ELSE.
      CONCATENATE 'Data has been deleted' LV_DCNT  'records.'
             INTO LV_MSG SEPARATED BY SPACE.
    ENDIF.

  ELSE.
    CONCATENATE 'Data has been saved;'
                 LV_CNT  'records.'
            INTO LV_MSG SEPARATED BY SPACE.
  ENDIF.
  IF LV_DCNT > 0 OR LV_CNT > 0.
    MESSAGE S000 WITH LV_MSG.
  ENDIF.

  CLEAR FLAG_DATA_CHANGED.

ENDFORM.                    " update_database
*&---------------------------------------------------------------------*
*&      Form  create_and_init_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_AND_INIT_ALV.
*   Create object
  PERFORM CREATE_OBJECT.

*   Exclude toolbar
  PERFORM EXCLUDE_FUNCTIONS.
  SET PF-STATUS '100' EXCLUDING 'SAVE'.

*  Create Object to verify input values.
  CREATE OBJECT G_EVENT_RECEIVER.
  SET HANDLER : G_EVENT_RECEIVER->HANDLE_DATA_CHANGED FOR G_GRID,
                G_EVENT_RECEIVER->ON_F4 FOR G_GRID,
                G_EVENT_RECEIVER->HANDLE_DOUBLE_CLICK FOR G_GRID.

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

*   Define possible entry fields
  PERFORM CREATE_F4_FIELDS USING 'G_GRID'.

*   Set variant
  GV_REPID = GS_VARIANT-REPORT = SY-REPID.
  GS_VARIANT-VARIANT = P_VARI.

ENDFORM.                    " create_and_init_alv
*&---------------------------------------------------------------------*
*&      Form  info_text_set
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TRUE  text
*----------------------------------------------------------------------*
FORM INFO_TEXT_SET USING P_TRUE.
  IF P_TRUE EQ TRUE.
    INFO = TEXT-015.
  ELSE.
    INFO = TEXT-016.
  ENDIF.
ENDFORM.                    " info_text_set

*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SHOW_PROGRESS USING    PF_TEXT
                            VALUE(PF_VAL).
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            PERCENTAGE = PF_VAL
            TEXT       = PF_TEXT.

ENDFORM.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  CONVERT_REASON_FOR_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CONVERT_REASON_FOR_SELECTION.

  __CLS R_KZUST.

  LOOP AT S_KZUST.
    IF S_KZUST-LOW+0(1) <> 'X'.
      IF S_KZUST-HIGH IS INITIAL.
        R_KZUST-SIGN = 'I'.
        R_KZUST-OPTION = 'EQ'.
        CONCATENATE
          S_KZUST-LOW+0(1) 'U' S_KZUST-LOW+1(1) INTO R_KZUST-LOW.
        APPEND R_KZUST.

        CONCATENATE
          S_KZUST-LOW+0(1) 'D' S_KZUST-LOW+1(1) INTO R_KZUST-LOW.
        APPEND R_KZUST.

        CONCATENATE
        S_KZUST-LOW+0(1) 'E' S_KZUST-LOW+1(1) INTO R_KZUST-LOW.
        APPEND R_KZUST.

      ELSE.
        R_KZUST-SIGN = 'I'.
        R_KZUST-OPTION = 'BT'.
        CONCATENATE
        S_KZUST-LOW+0(1) 'U' S_KZUST-LOW+1(1) INTO R_KZUST-LOW.
        CONCATENATE
        S_KZUST-HIGH+0(1) 'U' S_KZUST-HIGH+1(1) INTO S_KZUST-HIGH.
        APPEND R_KZUST.

        CONCATENATE S_KZUST-LOW+0(1) 'D' S_KZUST-LOW+1(1) INTO
R_KZUST-LOW.
        CONCATENATE S_KZUST-HIGH+0(1) 'D' S_KZUST-HIGH+1(1) INTO
S_KZUST-HIGH.
        APPEND R_KZUST.

        CONCATENATE S_KZUST-LOW+0(1) 'E' S_KZUST-LOW+1(1) INTO
R_KZUST-LOW.
        CONCATENATE S_KZUST-HIGH+0(1) 'E' S_KZUST-HIGH+1(1) INTO
S_KZUST-HIGH.
        APPEND R_KZUST.

      ENDIF.
    ENDIF.

  ENDLOOP.


ENDFORM.                    " CONVERT_REASON_FOR_SELECTION

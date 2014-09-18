*----------------------------------------------------------------------
* Program ID        : ZACOU125
* Title             : [CO] LDC Recreation
* Description       : LDC Recreation
*----------------------------------------------------------------------
REPORT ZACOU125 MESSAGE-ID ZMCO.
INCLUDE : ZACOUI00, ZACOU125_TOP.

*----------------------------------------------------------------------
* Selection-Screen
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B9 WITH FRAME TITLE TEXT-T01.
PARAMETERS     : P_IF  RADIOBUTTON GROUP SELS
                          DEFAULT 'X'  USER-COMMAND UCOM,
                 P_XL  RADIOBUTTON GROUP SELS.
SELECTION-SCREEN END OF BLOCK B9.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS S_IFDAT FOR SY-DATUM OBLIGATORY.
SELECT-OPTIONS S_LIFNR FOR ZTMM_IF_PRICE-LIFNR.
SELECT-OPTIONS S_MATNR FOR ZTMM_IF_PRICE-MATNR.
SELECTION-SCREEN END OF BLOCK B1.

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
                    DEFAULT 'c:\temp\ldc_update.xls'
                    MODIF ID EXL.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS P_HEAD AS CHECKBOX MODIF ID EXL DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(25) TEXT-T07 MODIF ID EXL.
*SELECTION-SCREEN COMMENT 33(42) TEXT-T04 MODIF ID EXL.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END   OF BLOCK B3.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-010.
PARAMETER P_VARI TYPE SLIS_VARI.
SELECTION-SCREEN END OF BLOCK B4.
INCLUDE ZCOI_BDC_INC_125.
DATA : MESSTABAGE LIKE MESSTAB OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF GT_LOG OCCURS 0,
         LIFNR LIKE A018-LIFNR,
         MATNR LIKE A018-MATNR,
         DATAB LIKE A018-DATAB,
         DATBI LIKE A018-DATBI,
         MESSA(80),
         LINECOLOR(4),     " ALV Color
       END OF GT_LOG.


INITIALIZATION.
  PERFORM INITIALIZATION_RTN.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN .
  CASE SSCRFIELDS-UCOMM.
    WHEN 'UCOM'.
      PERFORM MODIFY_SCREEN.
  ENDCASE.

AT SELECTION-SCREEN OUTPUT.
  PERFORM MODIFY_SCREEN.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM BROWSER CHANGING P_FILE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.
  PERFORM ALV_VARIANT_F4 CHANGING P_VARI.

*----------------------------------------------------------------------*
* Start of selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  CASE 'X'.
    WHEN P_XL.
*     Upload file
      PERFORM UPLOAD_FILE USING P_FILE.
      PERFORM MAKE_ROW_TAB.
    WHEN P_IF.
      PERFORM GET_IF_TABLE.
  ENDCASE.

  PERFORM REFINE_IF_TABLE.

  CHECK G_ERROR EQ SPACE .

  PERFORM GET_GT_OUT.
*     Call ALV screen
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
    WHEN 'CREA'.
      PERFORM POST_DATA.
      PERFORM SAVE_DATA.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA.
  CLEAR G_ERROR.
  __PROCESS '40'.

  PERFORM GET_ROW_DATA.
  PERFORM CALC_DATA.
  PERFORM GET_GT_OUT.
  __PROCESS '98'. "98%

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
    APPEND GS_FCAT TO GT_FCAT.
  END-OF-DEFINITION.
  DATA $EXCEL_HEADER(13).

  __CATALOG :
          'X'  'LIFNR'   'Vendor'           10  'CHAR' '',
          'X'  'MATNR'   'Material'         18  'CHAR' '',
          ' '  'DATE_F'  'Valid From'       13  'DATS' '',
          ' '  'DATE_T'  'Valid To'         13  'DATS' '',
          ' '  'PUM_N'   'Appr#'             8  'CHAR' '',
          ' '  'RESN_C'  'RC'                3  'CHAR' '',
          ' '  'PRICE_UNIT'  'Price/U'       5  'DEC'  '',
          ' '  'PRICE'   'Price'       15  'DEC'  '',
          ' '  'FRA1'    'FRA1'       15  'DEC'  '',
          ' '  'ZOTH'    'ZOTH'       15  'DEC'  '',
          ' '  'ZOTI'  'ZOTI'       15  'DEC'  '',
          ' '  'ZP16'  'ZP16'       15  'DEC'  '',
          ' '  'ZP17'  'ZP17'       15  'DEC'  '',
          ' '  'ZP18'  'ZP18'       15  'DEC'  '',
          ' '  'WAERS'    'Curr.'           5   'CUKY' '',
          ' '  'MSG'  'Remarks                             .'
          30  'CHAR' ''.

*  LOOP AT GT_FCAT INTO GS_FCAT.
*    CASE GS_FCAT-FIELDNAME.
*      WHEN 'XMENGES'.
*        GS_FCAT-JUST = 'R'.
*        MODIFY GT_FCAT FROM GS_FCAT.
*      WHEN 'BMENGE' OR 'RMENGE' OR 'PMENGE' OR 'AMENGE'
*                    OR 'GMENGE' .
*        GS_FCAT-QFIELDNAME = 'MEINS'.
*        GS_FCAT-REF_FIELD = GS_FCAT-FIELDNAME.
*        GS_FCAT-REF_TABLE = 'EKPO'.
*        GS_FCAT-JUST = 'R'.
*        MODIFY GT_FCAT FROM GS_FCAT.
*      WHEN 'DMBTR' OR 'UPRICE' OR 'DMBTR' .
*        GS_FCAT-QFIELDNAME = 'WAERS'.
*        GS_FCAT-REF_FIELD = GS_FCAT-FIELDNAME.
*        GS_FCAT-REF_TABLE = 'EKBZ'.
*        GS_FCAT-JUST = 'R'.
*        MODIFY GT_FCAT FROM GS_FCAT.
*    ENDCASE.
*  ENDLOOP.
*
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
    GS_SORT-COMP      = &5.
    APPEND GS_SORT TO FT_SORT.
  END-OF-DEFINITION.

  SORT_TAB :
             'LIFNR'    '1' 'X' 'X' 'X',
             'MATNR'    '2' 'X' 'X' 'X'.

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

  DATA: LS_MOD_CELLS TYPE LVC_S_MODI,
        LS_CELLS     TYPE LVC_S_MODI,
        LT_VALUES TYPE TABLE OF BAPI_CHAR_VALUES WITH HEADER LINE.

  __SET_REFRESH_MODE TRUE.
  CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY
       EXPORTING IS_STABLE = STABLE.

ENDFORM.                    " DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       Save data to table ZTCOU105
*----------------------------------------------------------------------*
FORM SAVE_DATA.

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
  __CLS GT_OUT .
  LOOP AT GT_CALC.
    MOVE-CORRESPONDING GT_CALC TO GT_OUT.
    APPEND GT_OUT.
  ENDLOOP.
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
ENDFORM.                    " DOUBLE_CLICK
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

  SET PF-STATUS '100'.
*  Create Object to verify input values.
  CREATE OBJECT G_EVENT_RECEIVER.
  SET :
    HANDLER G_EVENT_RECEIVER->HANDLE_DATA_CHANGED FOR G_GRID,
    HANDLER G_EVENT_RECEIVER->HANDLE_DOUBLE_CLICK FOR G_GRID.

*   Create field category
  PERFORM :
      CREATE_FIELD_CATEGORY USING FALSE,
      SORT_BUILD USING GT_SORT[],
      SET_LVC_LAYOUT,
      SET_COLOR.

*   Set variant
  GV_REPID = GS_VARIANT-REPORT = SY-REPID.
  GS_VARIANT-VARIANT = P_VARI.

ENDFORM.                    " create_and_init_alv
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
*&      Form  modify_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MODIFY_SCREEN.

ENDFORM.                    " modify_screen
*&---------------------------------------------------------------------*
*&      Form  upload_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
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

  __PROCESS '10'.
  IF P_HEAD = TRUE.
    ADD 1 TO BEGIN_ROW.
  ENDIF.

  IF P_TXT NE TRUE.
    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
         EXPORTING
              FILENAME                = FILENAME
              I_BEGIN_COL             = 1
              I_BEGIN_ROW             = BEGIN_ROW
              I_END_COL               = 13
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

    __PROCESS '20'.

    IF IT_ITAB[] IS INITIAL.
      MESSAGE S003(ZZ) WITH 'No Data was uploaded'.
      G_ERROR = TRUE .
      EXIT.
    ELSE.
      SORT IT_ITAB BY ROW COL.
      LOOP AT IT_ITAB.
        MOVE : IT_ITAB-COL TO V_INDEX.
        ASSIGN COMPONENT V_INDEX OF STRUCTURE I_ARTICLES TO <FS>.
        MOVE : IT_ITAB-VALUE TO <FS>.
        AT END OF ROW.
          APPEND I_ARTICLES.
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
              DATA_TAB            = I_ARTICLES
         EXCEPTIONS
              CONVERSION_ERRO     = 1
              INVALID_TABLE_WIDTH = 2
              INVALID_TYPE        = 3.

    IF NOT CANCEL IS INITIAL OR SY-SUBRC NE 0.
      MESSAGE S003(ZZ) WITH 'No Data was uploaded'.
      STOP.
    ENDIF.

  ENDIF.


  __PROCESS '30'.

  LOOP AT I_ARTICLES.
    PERFORM CHECK_NUM CHANGING :
            I_ARTICLES-PRICE,
            I_ARTICLES-PRICE_UNIT,
            I_ARTICLES-ZP16,
            I_ARTICLES-ZP17,
            I_ARTICLES-ZP18,
            I_ARTICLES-FRA1,
            I_ARTICLES-ZOTH,
            I_ARTICLES-ZOTI.

    MODIFY I_ARTICLES.
  ENDLOOP.

ENDFORM.                    " upload_file
*&---------------------------------------------------------------------*
*&      Form  get_data_from_dkbz_dkpo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_ROW_DATA.

ENDFORM.                    " get_data_from_dkbz_dkpo
*&---------------------------------------------------------------------*
*&      Form  BROWSER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
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
*&      Form  CHECK_NUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IT_UPLOAD_MENGE  text
*----------------------------------------------------------------------*
FORM CHECK_NUM CHANGING N_VALUE.
  REPLACE : '"' WITH '' INTO N_VALUE,
            '"' WITH '' INTO N_VALUE,
            ',' WITH '' INTO N_VALUE,
            '$' WITH '' INTO N_VALUE.
  CONDENSE N_VALUE NO-GAPS.
  IF N_VALUE CN NUM. N_VALUE = 0. ENDIF.
ENDFORM.                    " CHECK_NUM
*&---------------------------------------------------------------------*
*&      Form  POST_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM POST_DATA.
  DATA: LT_ROWS TYPE LVC_T_ROW WITH HEADER LINE,
        LT_ROW_NO TYPE LVC_T_ROID. "Numeric IDs of Selected Rows
  DATA: L_LINE TYPE I,
        W_REPID LIKE SY-REPID.

****************************> No Important
  DATA : TOTAL_LINES TYPE I,
         T_LINES1 TYPE I,
         T_LINES2 TYPE I,
         COUNT TYPE I.
****************************> end

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
    MESSAGE E000 WITH 'Please select a data to post.'.
    EXIT.
  ENDIF.

  PERFORM GET_POSTING_DATA TABLES LT_ROWS
                                  LT_ROW_NO .

*
*  DESCRIBE TABLE :
*          IT_INV LINES T_LINES1,
*          IT_CRE LINES T_LINES2.
*  TOTAL_LINES = T_LINES1 + T_LINES2.
*

  IF IT_ITAB[] IS INITIAL.
    MESSAGE S999 WITH TEXT-M01.
  ELSE.
    PERFORM CREATE_INFO_RECORD.

  ENDIF.

ENDFORM.                    " POST_DATA
*&---------------------------------------------------------------------*
*&      Form  CALC_UNIT_PRICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_lt_itab_FOR_UNITPRICE[]  text
*----------------------------------------------------------------------*
FORM REFINE_ITAB.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CALC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALC_DATA.

ENDFORM.                    " CALC_DATA
*&---------------------------------------------------------------------*
*&      Form  initialization_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INITIALIZATION_RTN.

  __CLS S_IFDAT.

  MOVE: 'I'                       TO S_IFDAT-SIGN,
        'BT'                      TO S_IFDAT-OPTION.

*  MOVE SY-DATUM TO S_IFDAT-LOW.
*  MOVE SY-DATUM TO S_IFDAT-HIGH.

  MOVE '20070110' TO S_IFDAT-LOW.
  MOVE '20070120' TO S_IFDAT-HIGH.

  APPEND S_IFDAT.

  MOVE 'LDC INPUT' TO GROUP.
ENDFORM.                    " initialization_rtn
*&---------------------------------------------------------------------*
*&      Form  get_if_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_IF_TABLE.

  __CLS IT_ROW_TAB.

  SELECT MATNR LIFNR ZSEQ INTF_D MAX( INTF_TIME ) APP_D PRICE
         RESN_C PUM_N PRICE_UNIT ZP16 ZP17 ZP18 FRA1 ZOTH ZOTI
      FROM ZTMM_IF_PRICE
      INTO CORRESPONDING FIELDS OF TABLE IT_ROW_TAB
      WHERE APP_D IN S_IFDAT
      AND LIFNR IN S_LIFNR
      AND MATNR IN S_MATNR
      GROUP BY MATNR LIFNR ZSEQ INTF_D APP_D PRICE
               RESN_C PUM_N PRICE_UNIT ZP16 ZP17 ZP18 FRA1 ZOTH
               ZOTI.

ENDFORM.                    " get_if_table
*&---------------------------------------------------------------------*
*&      Form  REFINE_IF_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REFINE_IF_TABLE.

  DATA $ITAB LIKE IT_ROW_TAB OCCURS 0 WITH HEADER LINE.

  $ITAB[] = IT_ROW_TAB[].

  SORT $ITAB[] BY MATNR LIFNR APP_D.

  __CLS GT_CALC.

  LOOP AT IT_ROW_TAB.
    MOVE-CORRESPONDING IT_ROW_TAB TO GT_CALC.
    GT_CALC-DATE_F = GT_CALC-APP_D.

    DO.
      READ TABLE $ITAB WITH KEY
                          MATNR = GT_CALC-MATNR
                          LIFNR = GT_CALC-LIFNR
                          BINARY SEARCH.

      IF SY-SUBRC EQ 0.
        IF $ITAB-APP_D > GT_CALC-DATE_F.
          GT_CALC-DATE_T = $ITAB-APP_D - 1.
          EXIT.
        ELSE.
          DELETE $ITAB INDEX SY-TABIX.
        ENDIF.
      ELSE.
        GT_CALC-DATE_T = '99991231'.
        EXIT.
      ENDIF.
    ENDDO.

    APPEND GT_CALC.CLEAR GT_CALC.
  ENDLOOP.

ENDFORM.                    " REFINE_IF_TABLE
*&---------------------------------------------------------------------*
*&      Form  MAKE_BDC_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_BDC_TABLE.

ENDFORM.                    " MAKE_BDC_TABLE
*&---------------------------------------------------------------------*
*&      Form  GET_POSTING_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ROWS  text
*      -->P_LT_ROW_NO  text
*----------------------------------------------------------------------*
FORM GET_POSTING_DATA TABLES PT_ROWS STRUCTURE LVC_S_ROW
                             PT_ROW_NO STRUCTURE LVC_S_ROID.

  __CLS: IT_ITAB .

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
    MOVE GT_OUT TO IT_ITAB.
    IT_ITAB-$IX = SY-TABIX.
    APPEND IT_ITAB.
  ENDLOOP.

  TOTAL_DOC_CNT = 0.
  CURRENT_DOC_CNT = 0.

ENDFORM.                    " GET_POSTING_DATA
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
      MODIFY GT_OUT .
    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " SELECT_ROW_BY_SUBTOTAL
*&---------------------------------------------------------------------*
*&      Form  create_info_record
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_INFO_RECORD.
*---
  CLEAR : MESSTABAGE, MESSTABAGE[], GT_LOG, GT_LOG[].
*         it_ztmm_ldc_log, it_ztmm_ldc_log[].
  DATA : L_CNT TYPE I, $IX LIKE SY-TABIX.

  LOOP AT IT_ITAB.
    $IX = SY-TABIX.
    AT FIRST.
      PERFORM OPEN_GROUP.
    ENDAT.
    ADD 1 TO L_CNT.

    PERFORM CHANGE_INFO_CONDITION.

    IF L_CNT = P_SCOUNT.
      PERFORM CLOSE_GROUP.
      PERFORM OPEN_GROUP.
      CLEAR L_CNT.
    ENDIF.
    IF SESSION NE 'X'.
      READ TABLE GT_OUT INDEX IT_ITAB-$IX.
      IF SY-SUBRC EQ 0.
        GT_OUT-MSG = GT_LOG-MESSA.
        MODIFY GT_OUT INDEX IT_ITAB-$IX TRANSPORTING MSG.
      ENDIF.
    ENDIF.
  ENDLOOP.

  PERFORM CLOSE_GROUP.


ENDFORM.                    " create_info_record
*&---------------------------------------------------------------------*
*&      Form  change_info_condition
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHANGE_INFO_CONDITION.
*---
  DATA : L_FIELD01(20),
         L_DATAB(10),
         L_DATBI(10),
         L_KBETR(13),
         L_KPEIN(05),
         L_KZUST LIKE KONH-KZUST,
         L_KBETR_TEMP LIKE KONP-KBETR,
         LV_LDC(1)   TYPE C,
         W_LINEID(2) TYPE N,
         W_FIELD1(14) TYPE C,
         W_FIELD2(14) TYPE C,
         W_FIELD3(14) TYPE C,
         LINE_COUNT TYPE I.

  REFRESH : BDCDATA.
  CLEAR : BDCDATA, MESSTAB, MESSTAB[], L_FIELD01,
          L_DATAB, L_DATBI, L_KBETR.

*---
  PERFORM BDC_FIELD USING : 'X'  'SAPMM06I'        '0100',
                            ' '  'EINA-LIFNR'      IT_ITAB-LIFNR,
                            ' '  'EINA-MATNR'      IT_ITAB-MATNR,
                            ' '  'EINE-EKORG'      'PU01',
                            ' '  'EINE-WERKS'      SPACE,
                            ' '  'EINA-INFNR'      SPACE,
                            ' '  'BDC_OKCODE'      '/00'.

  PERFORM BDC_FIELD USING : 'X'  'SAPMM06I'        '0101',
                            ' '  'BDC_OKCODE'      '=KO'.

  PERFORM BDC_FIELD USING : 'X'  'SAPLV14A'        '0102',
                            ' '  'BDC_OKCODE'      '=NEWD'.


  WRITE : IT_ITAB-DATE_F   TO L_DATAB.

  WRITE : IT_ITAB-DATE_T  TO L_DATBI.

*-basic price condition
  PERFORM BDC_FIELD USING : 'X'  'SAPMV13A'        '0201',
                            ' '  'RV13A-DATAB'     L_DATAB,
                            ' '  'RV13A-DATBI'     L_DATBI.

  WRITE : IT_ITAB-PRICE TO L_KBETR CURRENCY 'USD'.
  MOVE  : IT_ITAB-PRICE_UNIT TO L_KPEIN.


  PERFORM BDC_FIELD    USING :
                     'X' 'SAPMV13A'   '0201',
                     ' ' 'BDC_CURSOR' 'KONP-KSCHL(1)',
                     ' ' 'BDC_OKCODE' '/00',
*                     ' ' 'KONP-KSCHL(1)'  'PB00',
                     ' ' 'KONP-KBETR(1)'  L_KBETR,
                     ' ' 'KONP-KPEIN(1)'  L_KPEIN.

  WRITE : IT_ITAB-FRA1 TO L_KBETR.

  PERFORM BDC_FIELD    USING :
                     'X' 'SAPMV13A'   '0201',
                     ' ' 'BDC_CURSOR' 'KONP-KSCHL(2)',
                     ' ' 'BDC_OKCODE' '/00',
                     ' ' 'KONP-KSCHL(2)'  'FRA1',
                     ' ' 'KONP-KBETR(2)'  L_KBETR.

  WRITE : IT_ITAB-ZOTH TO L_KBETR.

  PERFORM BDC_FIELD    USING :
                     'X' 'SAPMV13A'   '0201',
                     ' ' 'BDC_CURSOR' 'KONP-KSCHL(3)',
                     ' ' 'BDC_OKCODE' '/00',
                     ' ' 'KONP-KSCHL(3)'  'ZOTH',
                     ' ' 'KONP-KBETR(3)'  L_KBETR.
*
  WRITE : IT_ITAB-ZOTI TO L_KBETR.

  PERFORM BDC_FIELD    USING :
                     'X' 'SAPMV13A'   '0201',
                     ' ' 'BDC_CURSOR' 'KONP-KSCHL(4)',
                     ' ' 'BDC_OKCODE' '/00',
                     ' ' 'KONP-KSCHL(4)' 'ZOTI',
                     ' ' 'KONP-KBETR(4)'  L_KBETR.

  WRITE : IT_ITAB-ZP16 TO L_KBETR." DECIMALS 0.
  PERFORM BDC_FIELD    USING :
                     'X' 'SAPMV13A'   '0201',
                     ' ' 'BDC_CURSOR' 'KONP-KSCHL(5)',
                     ' ' 'BDC_OKCODE' '/00',
                     ' ' 'KONP-KSCHL(5)'  'ZP16',
                     ' ' 'KONP-KBETR(5)'  L_KBETR.
*                     ' ' 'KONP-KONWA(5)'  'USD'.

  WRITE : IT_ITAB-ZP17 TO L_KBETR." DECIMALS 0.

  PERFORM BDC_FIELD    USING :
                     'X' 'SAPMV13A'   '0201',
                     ' ' 'BDC_CURSOR' 'KONP-KSCHL(6)',
                     ' ' 'BDC_OKCODE' '/00',
                     ' ' 'KONP-KSCHL(6)'  'ZP17',
                     ' ' 'KONP-KBETR(6)'  L_KBETR.
*                     ' ' 'KONP-KONWA(6)'  'USD'.

  WRITE : IT_ITAB-ZP18 TO L_KBETR." DECIMALS 0.
  PERFORM BDC_FIELD    USING :
                     'X' 'SAPMV13A'   '0201',
                     ' ' 'BDC_CURSOR' 'KONP-KSCHL(7)',
                     ' ' 'BDC_OKCODE' '/00',
                     ' ' 'KONP-KSCHL(7)'  'ZP18',
                     ' ' 'KONP-KBETR(7)'  L_KBETR.
*                     ' ' 'KONP-KONWA(7)'  'USD'.

  PERFORM BDC_FIELD USING : 'X'  'SAPMV13A'        '0201',
                            ' '  'BDC_OKCODE'      '=KDAT'.

  PERFORM BDC_FIELD USING : 'X'  'SAPMV13A'        '0200',
                            ' '  'KONH-KZUST'      IT_ITAB-RESN_C,
                            ' '  'KONH-KOSRT'      IT_ITAB-PUM_N,
                            ' '  'BDC_OKCODE'      '=SICH'.

*---
  PERFORM BDC_TRANSACTION USING 'ME12'.
*---
  APPEND LINES OF MESSTAB TO MESSTABAGE.

  DATA : L_MESSA(80).

  CLEAR : MESSTAB, L_MESSA.

  READ TABLE MESSTAB WITH KEY MSGTYP = 'E'.

  IF SY-SUBRC EQ 0.
    PERFORM GET_MESSAGE USING    MESSTAB-MSGID
                                 MESSTAB-MSGNR
                                 MESSTAB-MSGV1
                                 MESSTAB-MSGV2
                                 MESSTAB-MSGV3
                                 MESSTAB-MSGV4
                        CHANGING L_MESSA.
  ENDIF.

  READ TABLE MESSTAB WITH KEY MSGTYP = 'S'.

  IF SY-SUBRC EQ 0.
    PERFORM GET_MESSAGE USING    MESSTAB-MSGID
                                 MESSTAB-MSGNR
                                 MESSTAB-MSGV1
                                 MESSTAB-MSGV2
                                 MESSTAB-MSGV3
                                 MESSTAB-MSGV4
                        CHANGING L_MESSA.
  ENDIF.

  MOVE : L_MESSA TO GT_LOG-MESSA.

ENDFORM.                    " change_info_condition
*&---------------------------------------------------------------------*
*&      Form  GET_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MESSTAB_MSGID  text
*      -->P_MESSTAB_MSGNR  text
*      -->P_MESSTAB_MSGV1  text
*      -->P_MESSTAB_MSGV2  text
*      -->P_MESSTAB_MSGV3  text
*      -->P_MESSTAB_MSGV4  text
*      <--P_L_MESSA  text
*----------------------------------------------------------------------*
FORM GET_MESSAGE USING    P_MSGID
                          P_MSGNR
                          P_MSGV1
                          P_MSGV2
                          P_MSGV3
                          P_MSGV4
                 CHANGING P_L_MESSA.
*---
  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
       EXPORTING
            MSGID               = P_MSGID
            MSGNR               = P_MSGNR
            MSGV1               = P_MSGV1
            MSGV2               = P_MSGV2
            MSGV3               = P_MSGV3
            MSGV4               = P_MSGV4
       IMPORTING
            MESSAGE_TEXT_OUTPUT = P_L_MESSA.
ENDFORM.                    " get_message
*&---------------------------------------------------------------------*
*&      Form  make_row_tab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_ROW_TAB.
  __CLS IT_ROW_TAB.

  LOOP AT I_ARTICLES.
    MOVE-CORRESPONDING I_ARTICLES TO IT_ROW_TAB.
    APPEND IT_ROW_TAB.
  ENDLOOP.

ENDFORM.                    " make_row_tab

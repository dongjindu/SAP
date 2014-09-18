*&---------------------------------------------------------------------*
*&  Include           ZMM_COMMON01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  Include           ZMMCOMMON01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  Table
*&---------------------------------------------------------------------*
TABLES : MARA,           " ???????
         MARC,           " ??? ?? ??????
         T001W,          " ???/??
         T024,           " ????
         T001L,          " ????
         LFA1,           " ?????? (????)
         T159L,          " ???? ? ????? ?? ???
         T460A,          " ?????
         T001,           " ?? ??
         MAKT,           " ????
         T416T,          " BOM ?????
         EKKO,           " ??????
         EKPO,           " ??????
         MBEW,           " ????
         EORD,           " ???????
         MAST,           " BOM ????
         SSCRFIELDS,     " ????? ??
         IKPF,           " ??: ??????
         ISEG,           " ????????
         USR01,          " ????????? (??????)
         MCHB,           " ????
         MARD,           " ??? ?? ???????
         EBEW,           " ????????
         MKOL,           " ????? ????
         MSLB,           " ???? ????
         EKBE,           " ????? ??
         MKPF,           " ??: ????
         MSEG,           " ??????: ??
         KONP,           " ??(??)
         CDHDR,          " ?? ?? ??
         CDPOS,          " ?? ?? ??
         RBKP,
         BKPF,
         MSKU.

*&---------------------------------------------------------------------*
*&  Type-Group
*&---------------------------------------------------------------------*
TYPE-POOLS : SLIS,
             KCDE.

*&---------------------------------------------------------------------*
*&  constants
*&---------------------------------------------------------------------*
CONSTANTS : C_BEGIN_COL TYPE I VALUE '1',
            C_BEGIN_ROW TYPE I VALUE '1',
            C_END_COL   TYPE I VALUE '256',
            C_END_ROW   TYPE I VALUE '65536'.

CONSTANTS : C_TOP_OF_PAGE(30)        VALUE 'TOP_OF_PAGE',
            C_TOP_OF_PAGE_HIER(30)   VALUE 'TOP_OF_PAGE_HIER',
            C_TOP_OF_PAGE_DETAIL(30) VALUE 'TOP_OF_PAGE_DETAIL'.

*&---------------------------------------------------------------------*
*&  Internal Table
*&---------------------------------------------------------------------*
DATA : IT_INTERN LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE.

DATA : IT_BDC  LIKE BDCDATA OCCURS 0 WITH HEADER LINE,
       IT_MESS LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.

*&---------------------------------------------------------------------*
*&  ALV
*&---------------------------------------------------------------------*
DATA : IT_FIELDCAT    TYPE SLIS_T_FIELDCAT_ALV,
       ST_FIELDCAT    TYPE SLIS_FIELDCAT_ALV,
       IT_EVENT       TYPE SLIS_T_EVENT,
       ST_EVENT       TYPE SLIS_ALV_EVENT,
       IT_LISTHEADER  TYPE SLIS_T_LISTHEADER,
       ST_LISTHEADER  TYPE SLIS_LISTHEADER,
       IT_SORTCAT     TYPE SLIS_T_SORTINFO_ALV,
       ST_SORTCAT     TYPE SLIS_SORTINFO_ALV,
       ST_LAYOUT      TYPE SLIS_LAYOUT_ALV,
       IT_LIST_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER,
        G_VARIANT      LIKE DISVARIANT,
        G_REPID        LIKE SY-REPID,
        G_SAVE.

DATA : ST_KEYINFO_HIER         TYPE SLIS_KEYINFO_ALV,
       IT_FIELDCAT_HIER   TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORTCAT_HIER    TYPE SLIS_T_SORTINFO_ALV,
       ST_LAYOUT_HIER     TYPE SLIS_LAYOUT_ALV,
       IT_EVENT_HIER      TYPE SLIS_T_EVENT,
       IT_LISTHEADER_HIER TYPE SLIS_T_LISTHEADER.

*... detail
DATA : IT_FIELDCAT01   TYPE SLIS_T_FIELDCAT_ALV,
       IT_EVENT01      TYPE SLIS_T_EVENT,
       IT_LISTHEADER01 TYPE SLIS_T_LISTHEADER,
       IT_SORTCAT01    TYPE SLIS_T_SORTINFO_ALV,
       ST_LAYOUT01     TYPE SLIS_LAYOUT_ALV.

DATA : BEGIN OF IT_MENU OCCURS 0,
         FCODE LIKE SY-UCOMM,
       END OF IT_MENU.

DATA : IT_COLOR TYPE LVC_T_SCOL WITH HEADER LINE.



*&---------------------------------------------------------------------*
*&      Form  display_status
*&---------------------------------------------------------------------*
*       ?? ??? ??? ????
*----------------------------------------------------------------------*
FORM DISPLAY_STATUS  USING    P_TEXT.
*...
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      PERCENTAGE = 0
      TEXT       = P_TEXT
    EXCEPTIONS
      OTHERS     = 1.
ENDFORM.                    " display_status

*&---------------------------------------------------------------------*
*&      Form  init_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM INIT_ALV .

*...
  MOVE : SY-REPID TO G_REPID,
         'A'      TO G_SAVE.
  " A = U & X, U = userspecific, X = general user

ENDFORM.                    " init_alv

*&---------------------------------------------------------------------*
*&      Form  call_alv_grid_function
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CALL_ALV_GRID_FUNCTION USING P_TABNAME P_TITLE.
*...
  DATA : L_TABNAME TYPE SLIS_TABNAME,
         L_TITLE   TYPE LVC_TITLE.

  FIELD-SYMBOLS : <TABLE> TYPE STANDARD TABLE.

  MOVE : P_TABNAME TO L_TABNAME,
         P_TITLE   TO L_TITLE.

  ASSIGN (L_TABNAME) TO <TABLE>.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = G_REPID
      I_CALLBACK_PF_STATUS_SET = 'SET_STATUS'
      I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
*      I_CALLBACK_TOP_OF_PAGE   = 'TOP_OF_PAGE'
      I_GRID_TITLE             = L_TITLE
      I_SAVE                   = G_SAVE
      IS_LAYOUT                = ST_LAYOUT
      IT_FIELDCAT              = IT_FIELDCAT[]
      IT_SORT                  = IT_SORTCAT[]
      IT_EVENTS                = IT_EVENT[]
    TABLES
      T_OUTTAB                 = <TABLE>
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
ENDFORM.                    " call_function

*&---------------------------------------------------------------------*
*&      Form  call_alv_grid_function_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CALL_ALV_GRID_FUNCTION_DETAIL USING P_TABNAME P_TITLE.
*...
  DATA : L_TABNAME TYPE SLIS_TABNAME,
         L_TITLE   TYPE LVC_TITLE.

  FIELD-SYMBOLS : <TABLE> TYPE STANDARD TABLE.

  MOVE : P_TABNAME TO L_TABNAME,
         P_TITLE   TO L_TITLE.

  ASSIGN (L_TABNAME) TO <TABLE>.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = G_REPID
      I_CALLBACK_PF_STATUS_SET = 'SET_STATUS_DETAIL'
      I_CALLBACK_USER_COMMAND  = 'USER_COMMAND_DETAIL'
      I_GRID_TITLE             = L_TITLE
      I_SAVE                   = G_SAVE
      IS_LAYOUT                = ST_LAYOUT01
      IT_FIELDCAT              = IT_FIELDCAT01[]
      IT_SORT                  = IT_SORTCAT01[]
      IT_EVENTS                = IT_EVENT01[]
    TABLES
      T_OUTTAB                 = <TABLE>
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
ENDFORM.                    " call_function

*&---------------------------------------------------------------------*
*&      Form  set_toolbar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2224   text
*----------------------------------------------------------------------*
FORM SET_TOOLBAR  USING    P_FCODE.
*...
  CLEAR : IT_MENU.

  MOVE : P_FCODE TO IT_MENU-FCODE.
  APPEND IT_MENU.
ENDFORM.                    " set_toolbar

*&---------------------------------------------------------------------*
*&      Form  f4_filepath
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_PATH  text
*----------------------------------------------------------------------*
FORM F4_FILEPATH  USING    P_PATH
                           P_FILTER
                           P_TITLE.
*...
*  CALL FUNCTION 'Z_MM_GET_FILE_NAME'
*    EXPORTING
*      I_FILTER   = P_FILTER
*      I_TITLE    = P_TITLE
*      I_DEF_PATH = P_PATH
*    IMPORTING
*      E_FILENAME = P_PATH.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
*       EXPORTING
*            PROGRAM_NAME  = SYST-REPID
*            DYNPRO_NUMBER = SYST-DYNNR
*            FIELD_NAME    = ' '
*            STATIC        = ' '
*            MASK          = ' '
       CHANGING
            FILE_NAME     = P_PATH
       EXCEPTIONS
            MASK_TOO_LONG = 1
            OTHERS        = 2.
ENDFORM.                    " f4_filepath

*&---------------------------------------------------------------------*
*&      Form  upload_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_INTERN  text
*      -->P_P_PATH  text
*----------------------------------------------------------------------*
FORM UPLOAD_FILE  TABLES   P_INTERN STRUCTURE ALSMEX_TABLINE
                  USING    P_PATH.
*...
  CLEAR : P_INTERN, P_INTERN[].

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
*  CALL FUNCTION 'KCD_EXCEL_OLE_TO_INT_CONVERT'
    EXPORTING
      FILENAME                = P_PATH
      I_BEGIN_COL             = C_BEGIN_COL
      I_BEGIN_ROW             = C_BEGIN_ROW
      I_END_COL               = C_END_COL
      I_END_ROW               = C_END_ROW
    TABLES
      INTERN                  = P_INTERN
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.

  SORT P_INTERN BY ROW COL.
ENDFORM.                    " upload_file

*&---------------------------------------------------------------------*
*&      Form  read_lfa1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ITAB_LIFNR  text
*----------------------------------------------------------------------*
FORM READ_LFA1  USING    P_LIFNR.
*...
  CLEAR : LFA1.

  SELECT SINGLE * FROM LFA1
                 WHERE LIFNR EQ P_LIFNR.
ENDFORM.                                                    " read_lfa1

*&---------------------------------------------------------------------*
*&      Form  read_t024
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ITAB_EKGRP  text
*----------------------------------------------------------------------*
FORM READ_T024 USING    P_EKGRP.
*...
  CLEAR : T024.

  SELECT SINGLE * FROM T024
                 WHERE EKGRP EQ P_EKGRP.
ENDFORM.                                                    " read_t024

*&---------------------------------------------------------------------*
*&      Form  read_t001w
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ITAB_WERKS  text
*----------------------------------------------------------------------*
FORM READ_T001W  USING    P_WERKS.
*...
  CLEAR : T001W.

  SELECT SINGLE * FROM T001W
                 WHERE WERKS EQ P_WERKS.
ENDFORM.                    " read_t001w

*&---------------------------------------------------------------------*
*&      Form  conversion_matn1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ITAB_MATNR  text
*----------------------------------------------------------------------*
FORM CONVERSION_MATN1  USING    P_MATNR.
*...
  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      INPUT        = P_MATNR
    IMPORTING
      OUTPUT       = P_MATNR
    EXCEPTIONS
      LENGTH_ERROR = 1
      OTHERS       = 2.
ENDFORM.                    " conversion_matn1

*&---------------------------------------------------------------------*
*&      Form  read_mara
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ITAB_MATNR  text
*----------------------------------------------------------------------*
FORM READ_MARA  USING    P_MATNR.
*...
  CLEAR : MARA.

  SELECT SINGLE * FROM MARA
                 WHERE MATNR EQ P_MATNR.
ENDFORM.                    " read_mara

*&---------------------------------------------------------------------*
*&      Form  read_marc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ITAB_MATNR  text
*      -->P_IT_ITAB_WERKS  text
*----------------------------------------------------------------------*
FORM READ_MARC  USING    P_MATNR
                         P_WERKS.
*...
  CLEAR : MARC.

  SELECT SINGLE * FROM MARC
                 WHERE MATNR EQ P_MATNR
                   AND WERKS EQ P_WERKS.
ENDFORM.                    " read_marc

*&---------------------------------------------------------------------*
*&      Form  read_t001l
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ITAB_WERKS  text
*      -->P_IT_ITAB_LGORT  text
*----------------------------------------------------------------------*
FORM READ_T001L  USING    P_WERKS
                          P_LGORT.
*...
  CLEAR : T001L.

  SELECT SINGLE * FROM T001L
                 WHERE WERKS EQ P_WERKS
                   AND LGORT EQ P_LGORT.
ENDFORM.                    " read_t001l

*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3376   text
*      -->P_3377   text
*      -->P_3378   text
*----------------------------------------------------------------------*
FORM DYNPRO USING    DYNBEGIN
                     NAME
                     VALUE.
*---
  IF DYNBEGIN = 'X'.
    CLEAR : IT_BDC.
    MOVE : NAME  TO IT_BDC-PROGRAM,
           VALUE TO IT_BDC-DYNPRO,
           'X'   TO IT_BDC-DYNBEGIN.
    APPEND IT_BDC.
  ELSE .
    CLEAR : IT_BDC.
    MOVE : NAME  TO IT_BDC-FNAM,
           VALUE TO IT_BDC-FVAL.
    APPEND IT_BDC.
  ENDIF.
ENDFORM.                    " dynpro

*&---------------------------------------------------------------------*
*&      Form  get_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MESS_MSGID  text
*      -->P_IT_MESS_MSGNR  text
*      -->P_IT_MESS_MSGV1  text
*      -->P_IT_MESS_MSGV2  text
*      -->P_IT_MESS_MSGV3  text
*      -->P_IT_MESS_MSGV4  text
*      <--P_L_MESSAGE  text
*----------------------------------------------------------------------*
FORM GET_MESSAGE  USING    P_MSGID
                           P_MSGNR
                           P_MSGV1
                           P_MSGV2
                           P_MSGV3
                           P_MSGV4
                  CHANGING P_MESSAGE.
*...
  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
    EXPORTING
      MSGID               = P_MSGID
      MSGNR               = P_MSGNR
      MSGV1               = P_MSGV1
      MSGV2               = P_MSGV2
      MSGV3               = P_MSGV3
      MSGV4               = P_MSGV4
    IMPORTING
      MESSAGE_TEXT_OUTPUT = P_MESSAGE.
ENDFORM.                    " get_message

*&---------------------------------------------------------------------*
*&      Form  read_t159l
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_WERKS  text
*----------------------------------------------------------------------*
FORM READ_T159L  USING    P_WERKS.
*...
  CLEAR : T159L.

  SELECT SINGLE * FROM T159L
                 WHERE WERKS EQ P_WERKS.
ENDFORM.                    " read_t159l

*&---------------------------------------------------------------------*
*&      Form  read_t001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_BUKRS_1000  text
*----------------------------------------------------------------------*
FORM READ_T001  USING    P_BUKRS_1000.
*...
  CLEAR : T001.

  SELECT SINGLE * FROM T001
                 WHERE BUKRS EQ P_BUKRS_1000.
ENDFORM.                                                    " read_t001

*&---------------------------------------------------------------------*
*&      Form  read_makt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ITAB_MATNR  text
*----------------------------------------------------------------------*
FORM READ_MAKT  USING    P_MATNR.
*...
  CLEAR : MAKT.

  SELECT SINGLE * FROM MAKT
                 WHERE MATNR EQ P_MATNR
                   AND SPRAS EQ SY-LANGU.
ENDFORM.                    " read_makt

*&---------------------------------------------------------------------*
*&      Form  read_t416t
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_STLAN  text
*----------------------------------------------------------------------*
FORM READ_T416T  USING    P_STLAN.
*...
  CLEAR : T416T.

  SELECT SINGLE * FROM T416T
                 WHERE STLAN EQ P_STLAN
                   AND SPRAS EQ SY-LANGU.
ENDFORM.                    " read_t416t

*&---------------------------------------------------------------------*
*&      Form  read_ikpf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ITAB_IBLNR  text
*----------------------------------------------------------------------*
FORM READ_IKPF  USING    P_IBLNR P_GJAHR.
*...
  CLEAR : IKPF.

  SELECT SINGLE * FROM IKPF
                 WHERE IBLNR EQ P_IBLNR
                   AND GJAHR EQ P_GJAHR.
ENDFORM.                    " read_ikpf

*&---------------------------------------------------------------------*
*&      Form  conversion_exit_alpha_input
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IT_ITAB_IBLNR  text
*----------------------------------------------------------------------*
FORM CONVERSION_EXIT_ALPHA_INPUT  CHANGING P_ALPHA.
*...
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = P_ALPHA
    IMPORTING
      OUTPUT = P_ALPHA.
ENDFORM.                    " conversion_exit_alpha_input
*&---------------------------------------------------------------------*
*&      Form  conversion_exit_alpha_output
*&---------------------------------------------------------------------*
FORM CONVERSION_EXIT_ALPHA_OUTPUT  USING P_ALPHA.
*...
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      INPUT  = P_ALPHA
    IMPORTING
      OUTPUT = P_ALPHA.
ENDFORM.                    " conversion_exit_alpha_input

*&---------------------------------------------------------------------*
*&      Form  f4_iblnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_S_IBNLR_LOW  text
*----------------------------------------------------------------------*
FORM F4_IBLNR  USING    P_IBNLR.
*...
  DATA : INVFLAG(1).

  MOVE : 'X' TO INVFLAG.

  EXPORT INVFLAG TO MEMORY ID 'INVKEY'.

  CALL TRANSACTION 'MI22'.

  GET PARAMETER ID 'IBN' FIELD P_IBNLR.
*  GET PARAMETER ID 'GJR' FIELD p_gjahr.

  FREE MEMORY ID 'INVKEY'.
ENDFORM.                                                    " f4_iblnr

*&---------------------------------------------------------------------*
*&      Form  help_spmon
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM HELP_SPMON .
*... This Subroutine copied from Include program 'RMCS0F0M'

  DATA: BEGIN OF MF_DYNPFIELDS OCCURS 1.
          INCLUDE STRUCTURE DYNPREAD.
  DATA: END   OF MF_DYNPFIELDS.
  DATA: MF_RETURNCODE   LIKE SY-SUBRC,
        MF_MONAT        LIKE ISELLIST-MONTH,
        MF_HLP_REPID    LIKE SY-REPID.
  FIELD-SYMBOLS: <MF_FELD>.

* Wert von Dynpro lesen
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
*     Aktuelles Dynpro ist Wertemengenbild
      MF_HLP_REPID = 'SAPLALDB'.
    ELSE.
      READ TABLE MF_DYNPFIELDS INDEX 1.
*     Unterstriche durch Blanks ersetzen
      TRANSLATE MF_DYNPFIELDS-FIELDVALUE USING '_ '.
      EXIT.
    ENDIF.
  ENDDO.
  IF SY-SUBRC = 0.
*   Konvertierung ins interne Format
    CALL FUNCTION 'CONVERSION_EXIT_PERI_INPUT'
      EXPORTING
        INPUT         = MF_DYNPFIELDS-FIELDVALUE
      IMPORTING
        OUTPUT        = MF_MONAT
      EXCEPTIONS
        ERROR_MESSAGE = 1.
    IF MF_MONAT IS INITIAL.
*     Monat ist initial => Vorschlagswert aus akt. Datum ableiten
      MF_MONAT = SY-DATLO(6).
    ENDIF.
    CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
      EXPORTING
        ACTUAL_MONTH               = MF_MONAT
      IMPORTING
        SELECTED_MONTH             = MF_MONAT
        RETURN_CODE                = MF_RETURNCODE
      EXCEPTIONS
        FACTORY_CALENDAR_NOT_FOUND = 01
        HOLIDAY_CALENDAR_NOT_FOUND = 02
        MONTH_NOT_FOUND            = 03.
    IF SY-SUBRC = 0 AND MF_RETURNCODE = 0.
*     ASSIGN (MF_DYNPFIELDS-FIELDNAME) TO <MF_FELD>. " ==>> note 148804
*     <MF_FELD> = MF_MONAT.
      CALL FUNCTION 'CONVERSION_EXIT_PERI_OUTPUT'
        EXPORTING
          INPUT  = MF_MONAT
        IMPORTING
          OUTPUT = MF_DYNPFIELDS-FIELDVALUE.
      COLLECT MF_DYNPFIELDS.
      CALL FUNCTION 'DYNP_VALUES_UPDATE'
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
          UNDEFIND_ERROR       = 07. "<<== note 148804
    ENDIF.
  ENDIF.
ENDFORM.                    " help_spmon

*&---------------------------------------------------------------------*
*&      Form  call_alv_grid_function_hier
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1341   text
*      -->P_1342   text
*----------------------------------------------------------------------*
FORM CALL_ALV_GRID_FUNCTION_HIER  USING    P_HTABNAME   P_ITABNAME
                                           P_HHEADER    P_IHEADER.
*...
  DATA : L_HTABNAME TYPE SLIS_TABNAME,
         L_ITABNAME TYPE SLIS_TABNAME.

  FIELD-SYMBOLS : <HTABLE> TYPE STANDARD TABLE,
                  <ITABLE> TYPE STANDARD TABLE.

  MOVE : P_HTABNAME TO L_HTABNAME,
         P_ITABNAME TO L_ITABNAME.

  ASSIGN : (L_HTABNAME) TO <HTABLE>,
           (L_ITABNAME) TO <ITABLE>.

  CALL FUNCTION 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = G_REPID
      I_CALLBACK_PF_STATUS_SET = 'SET_STATUS_HIER'
      I_CALLBACK_USER_COMMAND  = 'USER_COMMAND_HIER'
      IS_LAYOUT                = ST_LAYOUT_HIER
      IT_FIELDCAT              = IT_FIELDCAT_HIER[]
      IT_SORT                  = IT_SORTCAT_HIER[]
      I_SAVE                   = G_SAVE
      IS_VARIANT               = G_VARIANT
      IT_EVENTS                = IT_EVENT_HIER[]
      I_TABNAME_HEADER         = P_HHEADER
      I_TABNAME_ITEM           = P_IHEADER
      IS_KEYINFO               = ST_KEYINFO_HIER
    TABLES
      T_OUTTAB_HEADER          = <HTABLE>
      T_OUTTAB_ITEM            = <ITABLE>.
ENDFORM.                    " call_alv_grid_function_hier

*&---------------------------------------------------------------------*
*&      Form  table_control_scroll
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_SAVE_OKCODE  text
*      -->P_G_STEP_TOP  text
*      -->P_G_STEP_TOT_LINES  text
*      -->P_G_STEP_LOOPC  text
*----------------------------------------------------------------------*
FORM TABLE_CONTROL_SCROLL  USING    P_SAVE_OKCODE
                                    P_TOP_LINE
                                    P_TOT_LINES
                                    P_LOOPC.
*---
  CALL FUNCTION 'SCROLLING_IN_TABLE'
    EXPORTING
      ENTRY_ACT             = P_TOP_LINE          "* Top Line
      ENTRY_FROM            = 1
      ENTRY_TO              = P_TOT_LINES            "* Lines
      LOOPS                 = P_LOOPC
      OK_CODE               = P_SAVE_OKCODE            "
    IMPORTING
      ENTRY_NEW             = P_TOP_LINE
    EXCEPTIONS
      NO_ENTRY_OR_PAGE_ACT  = 1
      NO_ENTRY_TO           = 2
      NO_OK_CODE_OR_PAGE_GO = 3
      OTHERS                = 4.
ENDFORM.                    " table_control_scroll

*&---------------------------------------------------------------------*
*&      Form  read_a017
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZMMS020_LIFNR  text
*      -->P_ZMMS020_MATNR  text
*      -->P_C_EKORG_1000  text
*      -->P_P_WERKS  text
*      -->P_1178   text
*----------------------------------------------------------------------*
FORM READ_A017  USING    P_LIFNR P_MATNR
                         P_EKORG P_WERKS
                         P_VALUE P_KNUMH.
*...
  CLEAR : P_KNUMH.

  SELECT SINGLE KNUMH INTO P_KNUMH
                FROM A017
               WHERE KAPPL EQ 'M'
                 AND KSCHL EQ 'PB00'
                 AND LIFNR EQ P_LIFNR
                 AND MATNR EQ P_MATNR
                 AND EKORG EQ P_EKORG
                 AND WERKS EQ P_WERKS
                 AND ESOKZ EQ P_VALUE
                 AND DATBI GE SY-DATUM
                 AND DATAB LE SY-DATUM.
ENDFORM.                                                    " read_a017

*&---------------------------------------------------------------------*
*&      Form  read_konp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_KNUMH  text
*----------------------------------------------------------------------*
FORM READ_KONP  USING    P_KNUMH.
*...
  CLEAR : KONP.

  SELECT SINGLE * FROM KONP
                 WHERE KNUMH EQ P_KNUMH
                   AND LOEVM_KO EQ SPACE.
ENDFORM.                    " read_konp

*&---------------------------------------------------------------------*
*&      Form  bapi_rollback
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BAPI_ROLLBACK .
*...
  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
ENDFORM.                    " bapi_rollback

*&---------------------------------------------------------------------*
*&      Form  bapi_commit
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BAPI_COMMIT .
*...
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      WAIT = 'X'.
ENDFORM.                    " bapi_commit

*&---------------------------------------------------------------------*
*&      Form  conversion_alpha
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ITAB_LIFNR  text
*----------------------------------------------------------------------*
FORM CONVERSION_ALPHA  USING    P_ALPHA.
*...
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = P_ALPHA
    IMPORTING
      OUTPUT = P_ALPHA.
ENDFORM.                    " conversion_alpha

*&---------------------------------------------------------------------*
*&      Form  read_eord
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_MATNR  text
*      -->P_P_WERKS  text
*----------------------------------------------------------------------*
FORM READ_EORD  USING    P_MATNR
                         P_WERKS.
*...
  CLEAR : EORD.

  SELECT SINGLE * FROM EORD
                 WHERE MATNR EQ P_MATNR
                   AND WERKS EQ P_WERKS.
ENDFORM.                    " read_eord
*&---------------------------------------------------------------------*
*&      Form  ICON_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DISPLAY_TYPE  text
*      <--P_IT_DISPLAY_ICON  text
*----------------------------------------------------------------------*
FORM ICON_CREATE  USING    P_VALUE
                  CHANGING P_ICON.

  DATA : L_ICON TYPE ICONNAME.
  CLEAR L_ICON.

  CASE P_VALUE.
    WHEN 'S' OR 'Z'.
      MOVE: 'icon_led_green'  TO L_ICON.
    WHEN 'E'.
      MOVE: 'icon_led_red'    TO L_ICON.
    WHEN 'W'.
      MOVE: 'icon_led_yellow' TO L_ICON.
    WHEN OTHERS.
      MOVE: 'icon_space'      TO L_ICON.
  ENDCASE.

  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      NAME                  = L_ICON
    IMPORTING
      RESULT                = P_ICON
    EXCEPTIONS
      ICON_NOT_FOUND        = 1
      OUTPUTFIELD_TOO_SHORT = 2
      OTHERS                = 3.

ENDFORM.                    " ICON_CREATE
*&---------------------------------------------------------------------*
*&      Form  CHECK_CONVERSION_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DISPLAY_BUDAT  text
*      <--P_IT_DISPLAY_BUDAT  text
*----------------------------------------------------------------------*
FORM CHECK_CONVERSION_DATE USING    P_INPUT
                           CHANGING P_OUTPUT.

  DATA: NUMBER(30)    ,
        NUMBER_OUT(30),
        LENGTH TYPE I ,
        LENGTH2 TYPE I,
        ONE.

  MOVE : P_INPUT TO NUMBER.
  LENGTH = STRLEN( NUMBER ).

  DO LENGTH TIMES.
    ONE = NUMBER(1).

    IF ONE = ',' OR ONE = '.' OR ONE = '-' OR ONE = '/'.
    ELSEIF ONE = ' '.
      LENGTH2 = STRLEN( NUMBER_OUT ).
      IF LENGTH2 > 0. EXIT. ENDIF.
    ELSEIF ONE BETWEEN '0' AND '9' OR
    ( ONE EQ '-' AND SY-INDEX EQ LENGTH ).
      CONCATENATE NUMBER_OUT ONE INTO NUMBER_OUT.
    ELSE.
      P_OUTPUT = 'X'.
      EXIT.
*      RAISE no_numeric.
    ENDIF.

    SHIFT NUMBER.
  ENDDO.

  P_OUTPUT = NUMBER_OUT.
ENDFORM.                    " CHECK_CONVERSION_DATE
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_AMT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_UPLOAD_SUP_AMT  text
*      <--P_IT_DISPLAY_HWBAS  text
* Conversion CHAR type to Currency type
*----------------------------------------------------------------------*
FORM CONVERSION_AMT USING    P_AMT
                             P_WAERS
                    CHANGING P_CURR.

  DATA : L_CURRENCY(17) TYPE C.

  CLEAR L_CURRENCY.

  IF NOT P_AMT IS INITIAL.

    CALL FUNCTION 'MOVE_CHAR_TO_NUM'
      EXPORTING
        CHR             = P_AMT
      IMPORTING
        NUM             = P_CURR
      EXCEPTIONS
        CONVT_NO_NUMBER = 1
        CONVT_OVERFLOW  = 2
        OTHERS          = 3.

    IF SY-SUBRC = 0.
      CALL FUNCTION 'CURRENCY_AMOUNT_IDOC_TO_SAP'
        EXPORTING
          CURRENCY    = P_WAERS
          IDOC_AMOUNT = P_CURR
        IMPORTING
          SAP_AMOUNT  = P_CURR.
    ENDIF.
  ENDIF.
ENDFORM.                    " CONVERSION_AMT

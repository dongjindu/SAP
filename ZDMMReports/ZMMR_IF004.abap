*&---------------------------------------------------------------------*
*& Report  ZMMR_IF004                                                  *
*&                                                                     *
*&---------------------------------------------------------------------*
*& Date         Developer       Request       Description
*& 08/15/06     Manju           UD1K921759    Interchange City & Postal
*&                                            code
*&---------------------------------------------------------------------*

REPORT  ZMMR_IF004 MESSAGE-ID ZMMR_IF
                   NO STANDARD PAGE HEADING.

TABLES: ZTMM_IF007, " Vendor Log table
        ZTMM_IF017, " Vendor Error Log table
        T016,T016T.

TYPE-POOLS: SLIS.

RANGES : R_TYPE FOR ZTMM_IF017-TYPE,
         R_FLAG FOR ZTMM_IF007-ZFLAG.

DATA: BEGIN OF IT_DATA OCCURS 10.
DATA: CHECK.

DATA: W_TYPE(1). " like ztmm_if007-type.

DATA: ICON(4).
        INCLUDE STRUCTURE ZTMM_IF007.
*      message LIKE ztmm_if017-message.
DATA: END OF IT_DATA.

DATA: BEGIN OF IT_ITEM_DETAIL OCCURS 10.      " Error log table
        INCLUDE STRUCTURE ZTMM_IF017.
DATA: END OF IT_ITEM_DETAIL.

DATA: BEGIN OF IT_RESULT OCCURS 10.
        INCLUDE STRUCTURE BAPIRETURN.
DATA: END OF IT_RESULT.

DATA IT_ITEM  LIKE TABLE OF ZTMM_IF007 WITH HEADER LINE.
DATA IT_ERROR LIKE TABLE OF ZTMM_IF017 WITH HEADER LINE.

DATA: BEGIN OF DYNPFIELDS OCCURS 3.
        INCLUDE STRUCTURE DYNPREAD.
DATA: END OF DYNPFIELDS.

DATA : T_RETURN TYPE STANDARD TABLE OF DDSHRETVAL WITH HEADER LINE,
       SCR_FIELDS   LIKE DYNPREAD OCCURS 0 WITH HEADER LINE,
       L_DYNAME LIKE SY-REPID,
       L_DYNUMB LIKE SY-DYNNR.

DATA IS_VENDOR LIKE ZTMM_IF007.

*---// ALV general field
DATA: G_EXIT_CAUSED_BY_CALLER  TYPE C,
      G_REPID                  TYPE SY-REPID,
      G_SAVE                   TYPE C,
      G_PROGRAM_NAME           LIKE SY-REPID,
      G_INCLNAME               LIKE TRDIR-NAME.

*---// Structures
DATA: G_ST_LAYOUT              TYPE SLIS_LAYOUT_ALV,
      G_ST_FIELDCAT            TYPE SLIS_FIELDCAT_ALV,
      G_ST_EXIT_CAUSED_BY_USER TYPE SLIS_EXIT_BY_USER,
      G_ST_VARIANT             TYPE DISVARIANT.

*---// Internal Tables
DATA: G_IT_EVENTS              TYPE SLIS_T_EVENT,
      G_IT_LIST_TOP_OF_PAGE    TYPE SLIS_T_LISTHEADER,
      G_IT_FIELDCAT            TYPE SLIS_T_FIELDCAT_ALV,
      G_IT_SORT                TYPE SLIS_T_SORTINFO_ALV.

DATA: ALV_PRINT             TYPE SLIS_PRINT_ALV.
DATA: ALV_REPID             LIKE SY-REPID,
      ALV_VARIANT           LIKE DISVARIANT.
DATA: ALV_DETAIL_FUNC(30).
*-----------------------------------------------------------------------
* Constants
*-----------------------------------------------------------------------
CONSTANTS: C_STATUS_SET     TYPE SLIS_FORMNAME VALUE 'PF_STATUS_SET',
           C_USER_COMMAND   TYPE SLIS_FORMNAME VALUE 'USER_COMMAND'.

*-----------------------------------------------------------------------
* Controls
*-----------------------------------------------------------------------
DATA: G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      ALV_GRID           TYPE REF TO CL_GUI_ALV_GRID.

DATA: G_ST_DETAIL_LAYOUT TYPE LVC_S_LAYO,
      G_ST_DETAIL_FDCAT  TYPE LVC_S_FCAT,
      G_IT_DETAIL_FDCAT  TYPE LVC_T_FCAT.
*-----------------------------------------------------------------------
* Work fields
*-----------------------------------------------------------------------
DATA: G_SUBRC LIKE SY-SUBRC.

*---// Selection-screen layout
*** Search condition
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-T01.
SELECT-OPTIONS:
  SO_LIFNR FOR ZTMM_IF007-LIFNR      MODIF ID IOG,   " Vendor code
  SO_SERNO FOR ZTMM_IF007-SERNO      NO-EXTENSION NO INTERVALS, " Serial
  SO_DATE  FOR SY-DATUM. " Date on which the record was created
SELECTION-SCREEN END OF BLOCK BOX1.

*** Division
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-T02.
PARAMETERS:
  P_CREATE RADIOBUTTON GROUP GB2,             " create vendor log
  P_CHANGE RADIOBUTTON GROUP GB2,             " change vendor log
  P_C_ALL  RADIOBUTTON GROUP GB2 DEFAULT 'X'. " all
SELECTION-SCREEN END OF BLOCK BOX2.

*** Result
SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-T03.
PARAMETERS:
  P_SUCC  RADIOBUTTON GROUP GB3,             " Success log
  P_ERROR RADIOBUTTON GROUP GB3,             " Error log
  P_REPRO RADIOBUTTON GROUP GB3,             " Re-process log
  P_R_ALL RADIOBUTTON GROUP GB3 DEFAULT 'X'. " all
SELECTION-SCREEN END OF BLOCK BOX3.

*---// Initialization event
INITIALIZATION.
*** ALV layout initialization
  PERFORM ALV_LAYOUT_INIT USING G_ST_LAYOUT.
*** ALV event initialization
  PERFORM EVENT_BUILD     USING G_IT_EVENTS[].

  G_REPID              = SY-REPID.
  G_PROGRAM_NAME       = SY-REPID.
  G_INCLNAME           = SY-REPID.
  G_ST_VARIANT-VARIANT = '/ZMMR_IF004'.

*---// Start-of-selection event
START-OF-SELECTION.

  PERFORM RANGE_INPUT.
*** data selection using search conditions
  PERFORM GET_DATA.

  READ TABLE IT_DATA INDEX 1 TRANSPORTING NO FIELDS.
  IF SY-SUBRC NE 0.
    MESSAGE S001.
    EXIT.
  ENDIF.

END-OF-SELECTION.
*** Field category define
  PERFORM BUILD_FIELDCAT.
*** ALV display execution
  PERFORM ALV_DISPLAY.
*&---------------------------------------------------------------------*
*&      Form  alv_layout_init
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_ST_LAYOUT  text
*----------------------------------------------------------------------*
FORM ALV_LAYOUT_INIT  USING    P_ST_LAYOUT TYPE SLIS_LAYOUT_ALV.
  P_ST_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  P_ST_LAYOUT-ZEBRA             = 'X'.
  P_ST_LAYOUT-BOX_FIELDNAME     = 'CHECK'.

  P_ST_LAYOUT-GET_SELINFOS      = 'X'.
  P_ST_LAYOUT-GROUP_CHANGE_EDIT = 'X'.

  ALV_PRINT-NO_PRINT_SELINFOS   = 'X'.
  ALV_PRINT-NO_COVERPAGE        = 'X'.
  ALV_PRINT-NO_PRINT_LISTINFOS  = 'X'.
ENDFORM.                    " alv_layout_init
*&---------------------------------------------------------------------*
*&      Form  event_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_g_it_events[]  text
*----------------------------------------------------------------------*
FORM EVENT_BUILD  USING    P_IT_EVENTS TYPE SLIS_T_EVENT.
  DATA: LST_EVENT TYPE SLIS_ALV_EVENT.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            I_LIST_TYPE = 0
       IMPORTING
            ET_EVENTS   = P_IT_EVENTS.
*   EXCEPTIONS
*     LIST_TYPE_WRONG       = 1
*     OTHERS                = 2

*** executed in REUSE_ALV_EVENTS_GET - form pf_status_set
  READ TABLE P_IT_EVENTS WITH KEY NAME = SLIS_EV_PF_STATUS_SET
                         INTO LST_EVENT.
  IF SY-SUBRC EQ 0.
    MOVE C_STATUS_SET  TO LST_EVENT-FORM.
    APPEND LST_EVENT   TO G_IT_EVENTS.
  ENDIF.
*** executed in REUSE_ALV_EVENTS_GET - form user_command
  READ TABLE P_IT_EVENTS WITH KEY NAME = SLIS_EV_USER_COMMAND
                         INTO LST_EVENT.
  IF SY-SUBRC EQ 0.
    MOVE C_USER_COMMAND TO LST_EVENT-FORM.
    APPEND LST_EVENT    TO G_IT_EVENTS.
  ENDIF.
ENDFORM.                    " event_build
*&---------------------------------------------------------------------*
*&      Form  RANGE_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RANGE_INPUT .
  CLEAR: R_TYPE, R_FLAG.
  CLEAR: IT_DATA.

  REFRESH: R_TYPE, R_FLAG.
  REFRESH: IT_DATA.

*  IF p_succ EQ 'X'.
*    r_type-sign   = 'I'.
*    r_type-option = 'EQ'.
*    r_type-low    = 'S'.
*    APPEND r_type.
*    CLEAR  r_type.
*  ELSEIF p_error EQ 'X'.
*    r_type-sign   = 'I'.
*    r_type-option = 'EQ'.
*    r_type-low    = 'E'.
*    APPEND r_type.
*    CLEAR  r_type.
*  ENDIF.

  IF P_CREATE EQ 'X'.
    R_FLAG-SIGN   = 'I'.
    R_FLAG-OPTION = 'EQ'.
    R_FLAG-LOW    = 'C'.
    APPEND R_FLAG.
    CLEAR  R_FLAG.
  ELSEIF P_CHANGE EQ 'X'.
    R_FLAG-SIGN   = 'I'.
    R_FLAG-OPTION = 'EQ'.
    R_TYPE-LOW    = 'R'.
    APPEND R_FLAG.
    CLEAR  R_FLAG.
  ENDIF.
ENDFORM.                     " range_input
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA .
  CLEAR: IT_DATA, IT_ITEM.
  REFRESH: IT_DATA, IT_ITEM.

*  SELECT * FROM ztmm_if007 AS dt INNER JOIN ztmm_if017 AS et
*           ON dt~serno = et~serno
*           INTO CORRESPONDING FIELDS OF TABLE it_data
*           WHERE dt~zflag IN r_flag
*           AND   et~type  IN r_type
*           AND   dt~lifnr IN so_lifnr
*           AND   dt~serno IN so_serno
*           AND   dt~erdat IN so_date.
*
*  DELETE ADJACENT DUPLICATES FROM it_data COMPARING type serno.
*  SORT it_data BY erdat DESCENDING serno DESCENDING.

  CASE 'X'.
    WHEN P_SUCC.
      SELECT * FROM ZTMM_IF007
               INTO CORRESPONDING FIELDS OF TABLE IT_ITEM
               WHERE TYPE  IN ('S', 'R')
               AND   LIFNR IN SO_LIFNR
               AND   SERNO IN SO_SERNO
               AND   ERDAT IN SO_DATE
               AND   ZFLAG IN R_FLAG.

      SORT IT_ITEM BY ERDAT DESCENDING SERNO DESCENDING.
    WHEN P_ERROR.
      SELECT * FROM ZTMM_IF007
               INTO CORRESPONDING FIELDS OF TABLE IT_ITEM
** Changed by furong on 06/19/09
*               WHERE type   EQ 'E'
               WHERE ( TYPE = 'E' OR TYPE = 'Z' )
** End of change on 06/19/09
*               AND   zr2pro NE 'S'
               AND   LIFNR  IN SO_LIFNR
               AND   SERNO  IN SO_SERNO
               AND   ERDAT  IN SO_DATE
               AND   ZFLAG  IN R_FLAG.

      DELETE IT_ITEM WHERE ZR2PRO EQ 'S'.
      SORT IT_ITEM BY ERDAT DESCENDING SERNO DESCENDING.
    WHEN P_REPRO.
      SELECT * FROM ZTMM_IF007
               INTO CORRESPONDING FIELDS OF TABLE IT_ITEM
               WHERE ( TYPE EQ 'R' OR ZR2PRO EQ 'S' )
               AND   LIFNR IN SO_LIFNR
               AND   SERNO IN SO_SERNO
               AND   ERDAT IN SO_DATE
               AND   ZFLAG IN R_FLAG.

      SORT IT_ITEM BY ERDAT DESCENDING SERNO DESCENDING.
    WHEN OTHERS.
      SELECT * FROM ZTMM_IF007
               INTO CORRESPONDING FIELDS OF TABLE IT_ITEM
               WHERE LIFNR IN SO_LIFNR
               AND   SERNO IN SO_SERNO
               AND   ERDAT IN SO_DATE
               AND   ZFLAG IN R_FLAG.
      SORT IT_ITEM BY ERDAT DESCENDING SERNO DESCENDING.
  ENDCASE.

  LOOP AT IT_ITEM.
    MOVE-CORRESPONDING IT_ITEM TO IT_DATA.
    CASE IT_ITEM-TYPE.
      WHEN 'S'.
        IT_DATA-ICON = '@08@'.
      WHEN 'E' OR 'Z'.
        IF IT_DATA-ZR2PRO = 'S'.
          IT_DATA-ICON = '@09@'.
        ELSE.
          IT_DATA-ICON = '@0A@'.
        ENDIF.
      WHEN 'R'.
        IT_DATA-ICON = '@09@'.
    ENDCASE.
    APPEND IT_DATA.
    CLEAR  IT_DATA.
  ENDLOOP.

ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_FIELDCAT .
  PERFORM FILL_FIELD_CATEGORY USING :
    'S'   'FIELDNAME'          'ICON',
    ' '   'ICON'               'X',
    'E'   'REPTEXT_DDIC'       'Status',

    'S'   'FIELDNAME'          'SERNO',
    ' '   'JUST'               'R',
*    ' '   'KEY'                'X',
    ' '   'EDIT_MASK'          '==ALPHA',
    'E'   'REPTEXT_DDIC'       'Serial No',

*    'S'   'FIELDNAME'          'SEQNO',
*    'E'   'REPTEXT_DDIC'       'SEQUENCE',

    'S'   'FIELDNAME'          'ZFLAG',
    ' '   'JUST'               'L',
*    ' '   'KEY'                'X',
    'E'   'REPTEXT_DDIC'       'Flag',

*    'S'   'FIELDNAME'          'MESSAGE',
*    'E'   'REPTEXT_DDIC'       'MESSAGE',

    'S'   'FIELDNAME'          'LIFNR',
    ' '   'JUST'               'L',
*    ' '   'KEY'                'X',
    ' '   'EMPHASIZE'          'C100',
    ' '   'HOTSPOT'            'X',
    'E'   'REPTEXT_DDIC'       'Vendor',

    'S'   'FIELDNAME'          'TYPE',
    ' '   'JUST'               'L',
*    ' '   'KEY'                'X',
    'E'   'REPTEXT_DDIC'       'Type',

    'S'   'FIELDNAME'          'ZR2PRO',
    ' '   'JUST'               'L',
    ' '   'EMPHASIZE'          'C300',
    'E'   'REPTEXT_DDIC'       'Re-processing Ind',

    'S'   'FIELDNAME'          'ZREDOC',
    ' '   'JUST'               'R',
    ' '   'EMPHASIZE'          'C300',
    'E'   'REPTEXT_DDIC'       'Re-processing Doc',

    'S'   'FIELDNAME'          'BUKRS',
    ' '   'JUST'               'L',
    'E'   'REPTEXT_DDIC'       'Company',

    'S'   'FIELDNAME'          'EKORG',
    ' '   'JUST'               'L',
    'E'   'REPTEXT_DDIC'       'Purchasing Org',

    'S'   'FIELDNAME'          'KTOKK',
    ' '   'JUST'               'L',
    'E'   'REPTEXT_DDIC'       'Account Grp',

    'S'   'FIELDNAME'          'PSOTL',
    ' '   'JUST'               'L',
    'E'   'REPTEXT_DDIC'       'Title',

    'S'   'FIELDNAME'          'NAME1',
    ' '   'JUST'               'L',
    'E'   'REPTEXT_DDIC'       'Name',

    'S'   'FIELDNAME'          'STRAS',
    ' '   'JUST'               'L',
    'E'   'REPTEXT_DDIC'       'Street',

* Begin of changes - UD1K921759
*    'S'   'FIELDNAME'          'ORT01',
*    ' '   'JUST'               'R',
*    'E'   'REPTEXT_DDIC'       'Post Code',
*    'S'   'FIELDNAME'          'CITY_CD',
*    ' '   'JUST'               'L',
*    'E'   'REPTEXT_DDIC'       'City',
    'S'   'FIELDNAME'          'ORT01',
    ' '   'JUST'               'R',
    'E'   'REPTEXT_DDIC'       'City',
*    'S'   'FIELDNAME'          'CITY_CD',
     'S'   'FIELDNAME'          'ZIP_CODE',
    ' '   'JUST'               'L',
    'E'   'REPTEXT_DDIC'       'Post Code',
* End of changes - UD1K921759

    'S'   'FIELDNAME'          'LAND1',
    ' '   'JUST'               'L',
    'E'   'REPTEXT_DDIC'       'Country code',

    'S'   'FIELDNAME'          'TIME_ZN',
    ' '   'JUST'               'L',
    'E'   'REPTEXT_DDIC'       'Time zone',

    'S'   'FIELDNAME'          'SPRAS',
    ' '   'JUST'               'L',
    'E'   'REPTEXT_DDIC'       'Language code',

    'S'   'FIELDNAME'          'TELF1',
    ' '   'JUST'               'R',
    'E'   'REPTEXT_DDIC'       'Telephone',

    'S'   'FIELDNAME'          'REPR_EM',
    ' '   'JUST'               'L',
    'E'   'REPTEXT_DDIC'       'E-mail',

    'S'   'FIELDNAME'          'STCD2',
    ' '   'JUST'               'R',
    'E'   'REPTEXT_DDIC'       'Tax number',

    'S'   'FIELDNAME'          'J_1KFREPRE',
    ' '   'JUST'               'L',
    'E'   'REPTEXT_DDIC'       'Representative',

    'S'   'FIELDNAME'          'STCEG',
    ' '   'JUST'               'R',
    'E'   'REPTEXT_DDIC'       'VAT Reg_No',

    'S'   'FIELDNAME'          'J_1KFTBUS',
    ' '   'JUST'               'L',
    'E'   'REPTEXT_DDIC'       'Business',

    'S'   'FIELDNAME'          'J_1KFTIND',
    ' '   'JUST'               'L',
    'E'   'REPTEXT_DDIC'       'Industry',

    'S'   'FIELDNAME'          'WAERS',
    ' '   'JUST'               'L',
    'E'   'REPTEXT_DDIC'       'Currency',

    'S'   'FIELDNAME'          'ZTERM',
    ' '   'JUST'               'L',
    'E'   'REPTEXT_DDIC'       'Terms of payment',

    'S'   'FIELDNAME'          'INCO1',
    ' '   'JUST'               'L',
    'E'   'REPTEXT_DDIC'       'Incoterms1',

    'S'   'FIELDNAME'          'INCO2',
    ' '   'JUST'               'L',
    'E'   'REPTEXT_DDIC'       'Incoterms2',

    'S'   'FIELDNAME'          'WEBRE',
    ' '   'JUST'               'L',
    'E'   'REPTEXT_DDIC'       'GR-Based invoice',

    'S'   'FIELDNAME'          'XERSY',
    ' '   'JUST'               'L',
    'E'   'REPTEXT_DDIC'       'ERS',

    'S'   'FIELDNAME'          'XERSR',
    ' '   'JUST'               'L',
    'E'   'REPTEXT_DDIC'       'Automatic ERS',

    'S'   'FIELDNAME'          'KZAUT',
    ' '   'JUST'               'L',
    'E'   'REPTEXT_DDIC'       'AUTO.of PO allowed',

    'S'   'FIELDNAME'          'XNBWY',
    ' '   'JUST'               'L',
    'E'   'REPTEXT_DDIC'       'Revaluation',

    'S'   'FIELDNAME'          'MEPRF',
    ' '   'JUST'               'R',
    'E'   'REPTEXT_DDIC'       'Pricing date cat.',

    'S'   'FIELDNAME'          'BSTAE',
    ' '   'JUST'               'L',
    'E'   'REPTEXT_DDIC'       'Conf. ctrl key',

    'S'   'FIELDNAME'          'ERDAT',
    ' '   'JUST'               'R',
*    ' '   'KEY'                'X',
    'E'   'REPTEXT_DDIC'       'Created Date',

    'S'   'FIELDNAME'          'ERZET',
    ' '   'JUST'               'R',
*    ' '   'KEY'                'X',
    'E'   'REPTEXT_DDIC'       'Created Time',

    'S'   'FIELDNAME'          'ERNAM',
    ' '   'JUST'               'L',
    'E'   'REPTEXT_DDIC'       'User_name'.

ENDFORM.                    " build_fieldcat
*&---------------------------------------------------------------------*
*&      Form  fill_field_category
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0497   text
*      -->P_0498   text
*      -->P_0499   text
*----------------------------------------------------------------------*
FORM FILL_FIELD_CATEGORY  USING    P_CAT P_FNAM P_CON.
  IF P_CAT = 'S'.
    CLEAR G_ST_FIELDCAT.
  ENDIF.

  DATA LV_COL(40).
  FIELD-SYMBOLS <FS>.
  CONCATENATE 'G_ST_FIELDCAT-' P_FNAM INTO LV_COL.
  ASSIGN (LV_COL)     TO <FS>.
  MOVE    P_CON       TO <FS>.

  IF P_CAT = 'E'.
    APPEND G_ST_FIELDCAT TO G_IT_FIELDCAT.
  ENDIF.
ENDFORM.                    " fill_field_category
*&---------------------------------------------------------------------*
*&      Form  alv_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_DISPLAY .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
     I_CALLBACK_PROGRAM                = G_REPID
     I_BACKGROUND_ID                   = 'ALV_BACKGROUND'
*     I_GRID_TITLE                      =
*     I_GRID_SETTINGS                   =
     IS_LAYOUT                         = G_ST_LAYOUT
     IT_FIELDCAT                       = G_IT_FIELDCAT[]
*     IT_EXCLUDING                      =
*     IT_SPECIAL_GROUPS                 =
     IT_SORT                           = G_IT_SORT
*     IT_FILTER                         =
*     IS_SEL_HIDE                       =
*     I_DEFAULT                         = 'X'
     I_SAVE                            = G_SAVE
     IS_VARIANT                        = ALV_VARIANT
     IT_EVENTS                         = G_IT_EVENTS[]
*     IT_EVENT_EXIT                     =
     IS_PRINT                          = ALV_PRINT
*     IS_REPREP_ID                      =
*     I_SCREEN_START_COLUMN             = 0
*     I_SCREEN_START_LINE               = 0
*     I_SCREEN_END_COLUMN               = 0
*     I_SCREEN_END_LINE                 = 0
*     IT_ALV_GRAPHICS                   =
*     IT_HYPERLINK                      =
*     IT_ADD_FIELDCAT                   =
*     IT_EXCEPT_QINFO                   =
*     I_HTML_HEIGHT_TOP                 =
*     I_HTML_HEIGHT_END                 =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER            =
   TABLES
     T_OUTTAB                          = IT_DATA[]
   EXCEPTIONS
     PROGRAM_ERROR                     = 1
     OTHERS                            = 2.

  IF SY-SUBRC <> 0.
    CASE SY-SUBRC.
      WHEN 1.
        MESSAGE S002.
      WHEN 2.
        MESSAGE S003.
    ENDCASE.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " alv_display
*&---------------------------------------------------------------------*
*&      Form  pf_status_set
*&---------------------------------------------------------------------*
*       This form routine is called in 'REUSE_ALV_EVENTS_GET' function
*----------------------------------------------------------------------*
*      -->P_RT_EXTAB  text
*
*----------------------------------------------------------------------*
FORM PF_STATUS_SET  USING    P_RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD'.
  SET TITLEBAR 'TITLE'.
ENDFORM.                    " pf_status_set
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_UCOMM  text
*      -->P_SELFIELD  text
*
*----------------------------------------------------------------------*
FORM USER_COMMAND  USING    R_UCOMM     LIKE SY-UCOMM
                            RS_SELFIELD TYPE SLIS_SELFIELD.
  DATA LV_ANS.
  RS_SELFIELD-REFRESH = 'X'.

  CASE R_UCOMM.
    WHEN 'REFR'.           " refresh
      PERFORM GET_DATA.
    WHEN '&IC1'.           " move to mk03 when user selects vendor code
      READ TABLE IT_DATA INDEX RS_SELFIELD-TABINDEX.
      IF SY-SUBRC = 0.
        IF RS_SELFIELD-FIELDNAME = 'LIFNR'
        AND NOT IT_DATA-LIFNR IS INITIAL.

          SET PARAMETER ID 'LIF' FIELD IT_DATA-LIFNR.
          SET PARAMETER ID 'EKO' FIELD IT_DATA-EKORG.
          CALL TRANSACTION 'MK03'.
        ENDIF.
      ENDIF.
*    WHEN 'DETA'.           " selected line detail list view
*      CLEAR: it_item_detail.
*
*      READ TABLE it_data WITH KEY check = 'X'.
*      IF sy-subrc = 0.
*        MOVE-CORRESPONDING it_data TO ztmm_if007.
*
*        SELECT * FROM ztmm_if017
*                 INTO CORRESPONDING FIELDS OF TABLE it_item_detail
*                 WHERE serno = it_data-serno.
*        CALL SCREEN '0100'.
*      ELSE.
*        MESSAGE i004.
*      ENDIF.
    WHEN 'REPV'.           " Re-processing Vendor
      CLEAR IT_ITEM_DETAIL.
      REFRESH IT_ITEM_DETAIL.

      READ TABLE IT_DATA WITH KEY CHECK = 'X'.
      IF SY-SUBRC = 0.
        MOVE-CORRESPONDING IT_DATA TO ZTMM_IF007.

        SELECT * FROM ZTMM_IF017
                 INTO CORRESPONDING FIELDS OF TABLE IT_ITEM_DETAIL
                 WHERE SERNO = IT_DATA-SERNO.
        CALL SCREEN '0100'.
      ELSE.
        MESSAGE I004.
      ENDIF.
** Added by Fuorng' UD1K930957
    WHEN 'CHTY'.           " Change status
      CLEAR IT_ITEM_DETAIL.
      REFRESH IT_ITEM_DETAIL.

      READ TABLE IT_DATA WITH KEY CHECK = 'X'.
      IF SY-SUBRC = 0.
        MOVE-CORRESPONDING IT_DATA TO ZTMM_IF007.
        ZTMM_IF007-SPRAS = ' '.
        CALL SCREEN '0200'.
      ELSE.
        MESSAGE I004.
      ENDIF.
** end of addition
  ENDCASE.
ENDFORM.                    " user_command
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  IF ZTMM_IF007-TYPE   EQ 'S'
  OR ZTMM_IF007-TYPE   EQ 'R'.
** Changed by furong on 06/19/09
*  OR ZTMM_IF007-TYPE   EQ 'Z'
*  OR ZTMM_IF007-ZR2PRO EQ 'S'.
** End of change
    SET PF-STATUS 'DETAIL' EXCLUDING 'REP'.
  ELSE.
    SET PF-STATUS 'DETAIL'.
  ENDIF.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
*  IF sy-ucomm = '&F03'.
*    CLEAR   it_item_detail.
*    REFRESH it_item_detail.
*    LEAVE TO SCREEN 0.
*  ENDIF.
  DATA ANSWER.
  CASE SY-UCOMM.
    WHEN '&F03'.
      CLEAR IT_ITEM_DETAIL.
      REFRESH IT_ITEM_DETAIL.
      LEAVE TO SCREEN 0.
    WHEN 'REP'.
      PERFORM POPUP_TO_CONFIRM USING    TEXT-P01
                                        TEXT-P02
                               CHANGING ANSWER.
      CHECK ANSWER = 'J'.

      PERFORM RE_PROCESS_VENDOR.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  create_object  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREATE_OBJECT OUTPUT.
  IF G_CUSTOM_CONTAINER IS INITIAL.
    CREATE OBJECT G_CUSTOM_CONTAINER
      EXPORTING CONTAINER_NAME = 'CONTROL_AREA'.
    CREATE OBJECT ALV_GRID
      EXPORTING I_PARENT = G_CUSTOM_CONTAINER.
  ENDIF.

ENDMODULE.                 " create_object  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  transfer_data  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TRANSFER_DATA OUTPUT.
  PERFORM DETAIL_LAYOUT.
  PERFORM DETAIL_FIELD_CAT.
  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT       = G_ST_DETAIL_LAYOUT
    CHANGING
      IT_FIELDCATALOG = G_IT_DETAIL_FDCAT
      IT_OUTTAB       = IT_ITEM_DETAIL[].

ENDMODULE.                 " transfer_data  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  detail_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DETAIL_LAYOUT .
  CLEAR G_ST_DETAIL_LAYOUT.
  G_ST_DETAIL_LAYOUT-CWIDTH_OPT = 'X'.
  G_ST_DETAIL_LAYOUT-ZEBRA      = 'X'.

ENDFORM.                    " detail_layout
*&---------------------------------------------------------------------*
*&      Form  detail_field_cat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DETAIL_FIELD_CAT .
  REFRESH G_IT_DETAIL_FDCAT.
  PERFORM DETAIL_FDCAT_BUILD USING:

    'S'    'FIELDNAME'         'SERNO',
    'E'    'COLTEXT'           'SERIAL No',

    'S'    'FIELDNAME'         'SEQNO',
    'E'    'COLTEXT'           'SEQUENCE No',

    'S'    'FIELDNAME'         'TYPE',
    'E'    'COLTEXT'           'TYPE',

    'S'    'FIELDNAME'         'MESSAGE',
    'E'    'COLTEXT'           'MESSAGE'.

ENDFORM.                    " detail_field_cat
*&---------------------------------------------------------------------*
*&      Form  detail_fdcat_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1272   text
*      -->P_1273   text
*      -->P_1274   text
*----------------------------------------------------------------------*
FORM DETAIL_FDCAT_BUILD  USING    VALUE(P_CAT)
                                  VALUE(P_FNAM)
                                  VALUE(P_CON).
  IF P_CAT = 'S'.
    CLEAR G_ST_DETAIL_FDCAT.
  ENDIF.

  DATA L_COL(40).
  FIELD-SYMBOLS <FS>.
  CONCATENATE 'G_ST_DETAIL_FDCAT-' P_FNAM INTO L_COL.
  ASSIGN (L_COL)     TO <FS>.
  MOVE    P_CON      TO <FS>.

  IF P_CAT = 'E'.
    APPEND G_ST_DETAIL_FDCAT TO G_IT_DETAIL_FDCAT.
  ENDIF.

ENDFORM.                    " detail_fdcat_build
*&---------------------------------------------------------------------*
*&      Module  screen_control  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SCREEN_CONTROL OUTPUT.
*  CHECK zsmm_if021-icon NE '@0A@'.
  CHECK ZTMM_IF007-TYPE   EQ 'S'
     OR ZTMM_IF007-TYPE   EQ 'R'.
** Changed by Furong on 06/19/09
*     OR ZTMM_IF007-TYPE   EQ 'Z'
*     OR ZTMM_IF007-ZR2PRO EQ 'S'.
** End of change
  LOOP AT SCREEN.
    IF SCREEN-GROUP1 = 'G1'.
      SCREEN-INPUT  = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                 " screen_control  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  display_alv  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_ALV OUTPUT.
  DATA: ROW_NO   TYPE LVC_S_ROID,
        ROW_INFO TYPE LVC_S_ROW,
        COL_INFO TYPE LVC_S_COL.

  IF G_CUSTOM_CONTAINER IS INITIAL.
    CREATE OBJECT G_CUSTOM_CONTAINER
      EXPORTING CONTAINER_NAME = 'CONTROL_AREA'.

    CREATE OBJECT ALV_GRID
      EXPORTING I_PARENT = G_CUSTOM_CONTAINER.

    PERFORM DETAIL_LAYOUT.

    PERFORM DETAIL_FIELD_CAT.

    CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = G_ST_DETAIL_LAYOUT
      CHANGING
        IT_FIELDCATALOG = G_IT_DETAIL_FDCAT
        IT_OUTTAB       = IT_ITEM_DETAIL[].
  ELSE.
    CALL METHOD ALV_GRID->GET_SCROLL_INFO_VIA_ID
      IMPORTING
        ES_ROW_NO   = ROW_NO
        ES_ROW_INFO = ROW_INFO
        ES_COL_INFO = COL_INFO.

    CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY.

    CALL METHOD ALV_GRID->SET_SCROLL_INFO_VIA_ID
      EXPORTING
        IS_ROW_INFO = ROW_INFO
        IS_COL_INFO = COL_INFO
        IS_ROW_NO   = ROW_NO.
  ENDIF.

ENDMODULE.                 " display_alv  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  popup_to_confirm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_P01  text
*      -->P_TEXT_P02  text
*      <--P_ANSWER  text
*----------------------------------------------------------------------*
FORM POPUP_TO_CONFIRM  USING    P_TEXT_P01
                                P_TEXT_P02
                       CHANGING P_ANSWER.
*----------------------------------------------------------------------*
*  MESSAGE POPUP
*----------------------------------------------------------------------*
  DATA: BEGIN OF POP,
        TITEL     LIKE SPOP-TITEL,
        DIAGNOSE1 LIKE SPOP-DIAGNOSE1,
        DIAGNOSE2 LIKE SPOP-DIAGNOSE2,
        DIAGNOSE3 LIKE SPOP-DIAGNOSE3,
        TEXTLINE1 LIKE SPOP-TEXTLINE1,
        TEXTLINE2 LIKE SPOP-TEXTLINE2,
        TEXTLINE3 LIKE SPOP-TEXTLINE3,
        OPTION1   LIKE SPOP-VAROPTION1,
        OPTION2   LIKE SPOP-VAROPTION2,
        DEFAULT,
        ANSWER,
        END OF POP.

  DATA: CANCEL_DISPLAY.

  MOVE: P_TEXT_P01 TO POP-TEXTLINE1,
        P_TEXT_P02 TO POP-TITEL.

  CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
    EXPORTING
*     DEFAULTOPTION        = 'Y'
      DIAGNOSETEXT1        = POP-DIAGNOSE1
*     DIAGNOSETEXT2        = ' '
*     DIAGNOSETEXT3        = ' '
      TEXTLINE1            = POP-TEXTLINE1
      TEXTLINE2            = POP-TEXTLINE2
      TITEL                = POP-TITEL
*     START_COLUMN         = 25
*     START_ROW            = 6
      CANCEL_DISPLAY       = CANCEL_DISPLAY
    IMPORTING
      ANSWER               = POP-ANSWER
    EXCEPTIONS
      OTHSERS              = 1.

  P_ANSWER = POP-ANSWER.

ENDFORM.                    " popup_to_confirm
*&---------------------------------------------------------------------*
*&      Form  re_process_vendor
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RE_PROCESS_VENDOR .

  DATA: IS_VENDOR  LIKE ZSMM_IF007,
        LS_VENDOR  LIKE ZTMM_IF007,
        LV_R_SERNO LIKE ZTMM_IF007-SERNO,
        LV_SERNO   LIKE ZTMM_IF007-SERNO,
        REP_SERNO   LIKE ZTMM_IF007-SERNO.


  DATA: IT_XMSG   LIKE TABLE OF ZTISMESSAGE WITH HEADER LINE.

  CLEAR: IS_VENDOR, IT_RESULT, IT_XMSG.
  CLEAR: LS_VENDOR, LV_R_SERNO, LV_SERNO.

  REFRESH: IT_RESULT, IT_XMSG.

  MOVE-CORRESPONDING ZTMM_IF007 TO LS_VENDOR.
  MOVE-CORRESPONDING ZTMM_IF007 TO IS_VENDOR.
  MOVE: ZTMM_IF007-SERNO        TO LV_R_SERNO.

  CLEAR ZTMM_IF007.

* Get max Serial number for Error record
  SELECT MAX( SERNO ) INTO REP_SERNO
                       FROM ZTMM_IF007
                       WHERE LIFNR = LS_VENDOR-LIFNR AND
                             TYPE  = 'E' AND
                             ZFLAG = 'R' .

  CALL FUNCTION 'ZMMF_IF_VENDOR'
       EXPORTING
            I_VENDOR = IS_VENDOR
       TABLES
            E_RETURN = IT_RESULT.

  IF SY-SUBRC EQ 0.
    READ TABLE IT_RESULT WITH KEY TYPE = 'S'.

    IF SY-SUBRC EQ 0.
      SELECT MAX( SERNO ) INTO LV_SERNO
                          FROM ZTMM_IF007
                          WHERE LIFNR = LS_VENDOR-LIFNR.
      IF SY-SUBRC EQ 0.
        UPDATE ZTMM_IF007 SET   ZR2PRO = 'S'
                                ZREDOC = LV_SERNO
                          WHERE SERNO  = LV_R_SERNO.
        COMMIT WORK AND WAIT.
        CLEAR: ZTMM_IF007.
        UPDATE ZTMM_IF007 SET   TYPE   = 'R'
                                ZREDOC = LV_R_SERNO
                          WHERE SERNO  = LV_SERNO.
        COMMIT WORK AND WAIT.
      ENDIF.
      PERFORM GET_DATA.
      MESSAGE S005.
      LEAVE TO SCREEN 0.
    ELSE.
      SELECT MAX( SERNO ) INTO LV_SERNO
                          FROM ZTMM_IF007
                          WHERE LIFNR = LS_VENDOR-LIFNR.
      IF SY-SUBRC EQ 0.
        DELETE FROM ZTMM_IF007 WHERE SERNO = LV_SERNO.
        DELETE FROM ZTMM_IF017 WHERE SERNO = LV_SERNO.

        UPDATE ZTMM_IF007 SET ZFLAG = 'R'
                                TYPE = 'E'
                              WHERE SERNO EQ REP_SERNO.
        COMMIT WORK AND WAIT.
      ENDIF.

      CLEAR IT_XMSG.
      REFRESH IT_XMSG.
      LOOP AT IT_RESULT.
        MOVE: IT_RESULT-TYPE    TO IT_XMSG-MSGTY,
              IT_RESULT-MESSAGE TO IT_XMSG-MSGTX.
        APPEND IT_XMSG.
      ENDLOOP.
      CALL FUNCTION 'ZMM_IF_POPUP_TO_ERROR_MESSAGE'
        EXPORTING
*       XLOGNO       =
          XDOCNO_SHOW = 'X'
        TABLES
          XMSG        = IT_XMSG.

      LEAVE TO SCREEN 0.
    ENDIF.
  ENDIF.
ENDFORM.                    " re_process_vendor
*&---------------------------------------------------------------------*
*&      Module  HELP_LFB1-ZTERM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HELP_LFB1-ZTERM INPUT.
  PERFORM HELP_ZTERM USING 'ZTMM_IF007-ZTERM' '*' ZTMM_IF007-ZTERM.
ENDMODULE.                 " HELP_LFB1-ZTERM  INPUT
*&---------------------------------------------------------------------*
*&      Form  HELP_ZTERM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2298   text
*      -->P_2299   text
*      -->P_ZTMM_IF007_ZTERM  text
*----------------------------------------------------------------------*
FORM HELP_ZTERM USING
                      H01_FNAME LIKE RFCU3-FNAME
                      H01_FAUSW TYPE C
                      H01_ZTERM LIKE T052-ZTERM.
  DATA:  DYNNR LIKE SY-DYNNR,          " aktuelles Dynpro
         ZTERM LIKE T052-ZTERM,
         CHAR1(1)       TYPE C.

  DATA:    BEGIN OF DYNPFIELDS OCCURS 1.
          INCLUDE STRUCTURE DYNPREAD.
  DATA:    END   OF DYNPFIELDS.
*------ Inhalt des Feldes H01_FNAME vom Dynpro besorgen ----------------
  CLEAR   DYNPFIELDS.
  REFRESH DYNPFIELDS.
  DYNPFIELDS-FIELDNAME = H01_FNAME.
  APPEND DYNPFIELDS.
  DYNNR = SY-DYNNR.
  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            DYNAME     = 'ZMMR_IF004'
            DYNUMB     = DYNNR
       TABLES
            DYNPFIELDS = DYNPFIELDS
       EXCEPTIONS
            OTHERS     = 4.
  IF SY-SUBRC = 0.
    READ TABLE DYNPFIELDS INDEX 1.
    ZTERM = DYNPFIELDS-FIELDVALUE.
    TRANSLATE ZTERM TO UPPER CASE.                       "#EC TRANSLANG
  ENDIF.

*------ Feld H01_FNAME Eingabe- oder Anzeigefeld? ----------------------
  PERFORM FELDSTATUS_ERMITTELN USING H01_FNAME CHAR1.

*------ Zahlungsbedingungen anzeigen -----------------------------------
  CALL FUNCTION 'FI_F4_ZTERM'
       EXPORTING
            I_KOART = 'K'
            I_ZTERM = ZTERM
            I_XSHOW = CHAR1
       IMPORTING
            E_ZTERM = ZTMM_IF007-ZTERM.
  IF NOT ZTMM_IF007-ZTERM IS INITIAL.
    H01_ZTERM = ZTMM_IF007-ZTERM.
  ENDIF.



ENDFORM.                    " HELP_ZTERM
*&---------------------------------------------------------------------*
*&      Form  FELDSTATUS_ERMITTELN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_H01_FNAME  text
*      -->P_CHAR1  text
*----------------------------------------------------------------------*
FORM FELDSTATUS_ERMITTELN USING    FNAME
                                   XSHOW.
  LOOP AT SCREEN.
    CHECK SCREEN-NAME = FNAME.
    IF SCREEN-INPUT = 0.
      XSHOW = 'X'.
    ELSE.
      XSHOW = SPACE.
    ENDIF.
    EXIT.
  ENDLOOP.


ENDFORM.                    " FELDSTATUS_ERMITTELN
*&---------------------------------------------------------------------*
*&      Module  HELP_BRSCH  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HELP_BRSCH INPUT.
  PERFORM GET_BRSCH.
ENDMODULE.                 " HELP_BRSCH  INPUT
*&---------------------------------------------------------------------*
*&      Form  get_BRSCH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_BRSCH.
  DATA : BEGIN OF VALUE_TAB OCCURS 0,
           BRSCH  LIKE T016T-BRSCH,
           BRTXT  LIKE T016T-BRTXT,
          END OF VALUE_TAB.


  L_DYNAME = SY-REPID.
  L_DYNUMB = SY-DYNNR.

  REFRESH VALUE_TAB. CLEAR VALUE_TAB.
  SELECT BRSCH BRTXT INTO TABLE VALUE_TAB
         FROM  T016T
         WHERE SPRAS = SY-LANGU.

* Set F4 values for Industry Type
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            RETFIELD        = 'BRSCH'
            DYNPPROG        = L_DYNAME
            DYNPNR          = L_DYNUMB
            DYNPROFIELD     = 'T016-BRSCH'
            WINDOW_TITLE    = 'Industry Type'
            VALUE_ORG       = 'S'
       TABLES
            VALUE_TAB       = VALUE_TAB
            RETURN_TAB      = T_RETURN
       EXCEPTIONS
            PARAMETER_ERROR = 1.
  READ TABLE T_RETURN INDEX 1.
  IF SY-SUBRC EQ 0.

* Set values for Industry Type and Description.
    SCR_FIELDS-FIELDNAME  = 'T016-BRSCH'.
    SCR_FIELDS-FIELDVALUE = T_RETURN-FIELDVAL.
    APPEND SCR_FIELDS.
    SCR_FIELDS-FIELDNAME  = 'ZTMM_IF007-J_1KFTIND'.
    READ TABLE VALUE_TAB WITH KEY BRSCH = T_RETURN-FIELDVAL.
    SCR_FIELDS-FIELDVALUE = VALUE_TAB-BRTXT.
    APPEND SCR_FIELDS.
*  Update back Screen with Values
    CALL FUNCTION 'DYNP_VALUES_UPDATE'
         EXPORTING
              DYNAME               = SY-CPROG
              DYNUMB               = SY-DYNNR
         TABLES
              DYNPFIELDS           = SCR_FIELDS
         EXCEPTIONS
              INVALID_ABAPWORKAREA = 1
              INVALID_DYNPROFIELD  = 2
              INVALID_DYNPRONAME   = 3
              INVALID_DYNPRONUMMER = 4
              INVALID_REQUEST      = 5
              NO_FIELDDESCRIPTION  = 6
              UNDEFIND_ERROR       = 7
              OTHERS               = 8.
  ENDIF.


ENDFORM.                    " get_BRSCH
*&---------------------------------------------------------------------*
*&      Module  HELP_REGION  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HELP_REGION INPUT.
  PERFORM GET_REGION.
ENDMODULE.                 " HELP_REGION  INPUT
*&---------------------------------------------------------------------*
*&      Form  get_region
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_REGION.

  DATA : BEGIN OF VALUE_TAB OCCURS 0,
             REGIO  LIKE T005U-BLAND,
             BEZEI  LIKE T005U-BEZEI,
            END OF VALUE_TAB.

  REFRESH VALUE_TAB. CLEAR VALUE_TAB.

  IF NOT ZTMM_IF007-LAND1 IS INITIAL.
    DYNPFIELDS-FIELDNAME = 'ZTMM_IF007-LAND1'.
    APPEND DYNPFIELDS.
    L_DYNAME = SY-CPROG.
    L_DYNUMB = SY-DYNNR.

    CALL FUNCTION 'DYNP_VALUES_READ'
         EXPORTING
              DYNAME     = L_DYNAME
              DYNUMB     = L_DYNUMB
         TABLES
              DYNPFIELDS = DYNPFIELDS.

    READ TABLE DYNPFIELDS INDEX 1 .

    SELECT BLAND BEZEI  INTO TABLE VALUE_TAB
           FROM  T005U
           WHERE SPRAS = SY-LANGU AND
                 LAND1 = DYNPFIELDS-FIELDVALUE.

* Set F4 values for Industry Type

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
         EXPORTING
              RETFIELD        = 'REGIO'
              DYNPPROG        = L_DYNAME
              DYNPNR          = L_DYNUMB
              DYNPROFIELD     = 'ZTMM_IF007-REGION'
              WINDOW_TITLE    = 'Industry Type'
              VALUE_ORG       = 'S'
         TABLES
              VALUE_TAB       = VALUE_TAB
              RETURN_TAB      = T_RETURN
         EXCEPTIONS
              PARAMETER_ERROR = 1.
  ENDIF.

ENDFORM.                    " get_region
*&---------------------------------------------------------------------*
*&      Module  status_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'STATUS'.
ENDMODULE.                 " status_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.

  CASE SY-UCOMM.
    WHEN 'BACK' OR 'EXIT' OR 'CANL'.
      PERFORM GET_DATA.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      PERFORM SAVE_STATUS_DATA.
  ENDCASE.

ENDMODULE.                 " user_command_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  SAVE_STATUS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_STATUS_DATA.
  DATA: LW_VENDOR  LIKE ZTMM_IF007,
        L_SERNO LIKE ZTMM_IF007-SERNO.

  MOVE-CORRESPONDING ZTMM_IF007 TO LW_VENDOR.
  LW_VENDOR-TYPE = LW_VENDOR-ZR2PRO.
  CLEAR: LW_VENDOR-ZR2PRO.
  MODIFY ZTMM_IF007 FROM LW_VENDOR.

  UPDATE ZTMM_IF017 SET   TYPE   = LW_VENDOR-TYPE
                    WHERE SERNO  = LW_VENDOR-SERNO.

*      UPDATE ztmm_if007 SET   type   = w_type
*                        WHERE serno  = lw_vendor-serno
*                          and erdat = lw_vendor-erdat
*                          and erzet = lw_vendor-erzet
*                          and lifnr = lw_vendor-lifnr.
  IF SY-SUBRC = 0.
    COMMIT WORK AND WAIT.
    MESSAGE S000 WITH 'Status changed'.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " SAVE_STATUS_DATA

*&---------------------------------------------------------------------*
*& Report  ZMMR_IF006                                                  *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  ZMMR_IF006 MESSAGE-ID ZMMR_IF
                   NO STANDARD PAGE HEADING.

TABLES: EINE,
        ZTMM_IF008, " Info Record Log table
        ZTMM_IF014. " Info Record Error Log table

TYPE-POOLS: SLIS.

RANGES: R_TYPE FOR ZTMM_IF014-TYPE,
        R_FLAG FOR ZTMM_IF008-ZFLAG.

DATA: BEGIN OF IT_DATA OCCURS 10.
DATA: CHECK.
DATA: ICON(4).
        INCLUDE STRUCTURE ZTMM_IF008.
*      message LIKE ztmm_if014-message.
DATA: END OF IT_DATA.

DATA: BEGIN OF IT_ITEM_DETAIL OCCURS 10.      " Errors log table
        INCLUDE STRUCTURE ZTMM_IF014.
DATA: END OF IT_ITEM_DETAIL.

DATA: BEGIN OF IT_RESULT OCCURS 10.
        INCLUDE STRUCTURE BAPIRETURN.
DATA: END OF IT_RESULT.

DATA IT_ITEM  LIKE TABLE OF ZTMM_IF008 WITH HEADER LINE.
DATA IT_ERROR LIKE TABLE OF ZTMM_IF014 WITH HEADER LINE.

DATA IS_INFO_RECORD LIKE ZTMM_IF008.

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

DATA: ALV_PRINT           TYPE SLIS_PRINT_ALV.
DATA: ALV_REPID           LIKE SY-REPID,
      ALV_VARIANT         LIKE DISVARIANT.
DATA: ALV_DETAIL_FUNC(30).
*-----------------------------------------------------------------------
* Constants
*-----------------------------------------------------------------------
CONSTANTS: C_STATUS_SET   TYPE SLIS_FORMNAME VALUE 'PF_STATUS_SET',
           C_USER_COMMAND TYPE SLIS_FORMNAME VALUE 'USER_COMMAND'.

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
  SO_LIFNR FOR ZTMM_IF008-LIFNR      MODIF ID IOG, " Vendor code
  SO_MATNR FOR ZTMM_IF008-MATNR,                   " Material code
  SO_SERNO FOR ZTMM_IF008-SERNO      NO-EXTENSION NO INTERVALS, " Serial
  SO_DATE  FOR SY-DATUM,          " Date on which the record was created
  SO_EKGRP FOR EINE-EKGRP.      "Purchaing group
SELECTION-SCREEN END OF BLOCK BOX1.

*** Division
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-T02.
PARAMETERS:
  P_CREATE RADIOBUTTON GROUP GB2,    " create info record log
  P_CHANGE RADIOBUTTON GROUP GB2,    " change info record log
  P_DELETE RADIOBUTTON GROUP GB2,    " delete info record log
  P_C_ALL  RADIOBUTTON GROUP GB2 DEFAULT 'X'.      " all
SELECTION-SCREEN END OF BLOCK BOX2.

*** Result
SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-T03.
PARAMETERS:
  P_SUCC  RADIOBUTTON GROUP GB3,                    " success log
  P_ERROR RADIOBUTTON GROUP GB3,                    " error log
  P_REPRO RADIOBUTTON GROUP GB3,
  P_R_ALL RADIOBUTTON GROUP GB3 DEFAULT 'X'.        " all
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
  G_ST_VARIANT-VARIANT = '/ZMMR_IF006'.

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
*** field category define
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
*      -->P_G_IT_EVENTS[]  text
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
  IF SY-SUBRC = 0.
    MOVE C_STATUS_SET TO LST_EVENT-FORM.
    APPEND LST_EVENT  TO G_IT_EVENTS.
  ENDIF.
*** executed in REUSE_ALV_EVENTS_GET - form user_command
  READ TABLE P_IT_EVENTS WITH KEY NAME = SLIS_EV_USER_COMMAND
                         INTO LST_EVENT.
  IF SY-SUBRC = 0.
    MOVE C_USER_COMMAND TO LST_EVENT-FORM.
    APPEND LST_EVENT    TO G_IT_EVENTS.
  ENDIF.
ENDFORM.                    " event_build
*&---------------------------------------------------------------------*
*&      Form  range_input
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
    R_FLAG-SIGN = 'I'.
    R_FLAG-OPTION = 'EQ'.
    R_FLAG-LOW    = 'C'.   " Create flag
    APPEND R_FLAG.
    CLEAR  R_FLAG.
  ELSEIF P_CHANGE EQ 'X'.
    R_FLAG-SIGN = 'I'.
    R_FLAG-OPTION = 'EQ'.
    R_FLAG-LOW    = 'R'.   " Change flag
    APPEND R_FLAG.
    CLEAR  R_FLAG.
  ELSEIF P_DELETE EQ 'X'.
    R_FLAG-SIGN = 'I'.
    R_FLAG-OPTION = 'EQ'.
    R_FLAG-LOW    = 'D'.   " Delete flag
    APPEND R_FLAG.
    CLEAR  R_FLAG.
  ENDIF.
ENDFORM.                    " range_input
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA .
  CLEAR: IT_ITEM, IT_DATA.
  REFRESH: IT_ITEM, IT_DATA.

*  SELECT * FROM ztmm_if008 AS dt INNER JOIN ztmm_if014 AS et
*           ON dt~serno = et~serno
*           INTO CORRESPONDING FIELDS OF TABLE it_data
*           WHERE dt~zflag IN r_flag
*           AND   et~type  IN r_type
*           AND   dt~lifnr IN so_lifnr
*           AND   dt~matnr IN so_matnr
*           AND   dt~serno IN so_serno
*           AND   dt~erdat IN so_date.
*
*  DELETE ADJACENT DUPLICATES FROM it_data COMPARING type serno.
*  SORT it_data BY erdat DESCENDING serno DESCENDING.
  CASE 'X'.
    WHEN P_SUCC.
      SELECT * FROM ZTMM_IF008
               INTO CORRESPONDING FIELDS OF TABLE IT_ITEM
               WHERE TYPE IN ('S', 'R')
               AND   LIFNR IN SO_LIFNR
               AND   MATNR IN SO_MATNR
               AND   EKGRP IN SO_EKGRP
               AND   SERNO IN SO_SERNO
               AND   ERDAT IN SO_DATE
               AND   ZFLAG IN R_FLAG.

      SORT IT_ITEM BY ERDAT DESCENDING SERNO DESCENDING.
    WHEN P_ERROR.
      SELECT * FROM ZTMM_IF008
               INTO CORRESPONDING FIELDS OF TABLE IT_ITEM
               WHERE TYPE EQ 'E'
               AND   LIFNR IN SO_LIFNR
               AND   MATNR IN SO_MATNR
               AND   EKGRP IN SO_EKGRP
               AND   SERNO IN SO_SERNO
               AND   ERDAT IN SO_DATE
               AND   ZFLAG IN R_FLAG.

      DELETE IT_ITEM WHERE ZR2PRO EQ 'S'.
      SORT IT_ITEM BY ERDAT DESCENDING SERNO DESCENDING.
    WHEN P_REPRO.
      SELECT * FROM ZTMM_IF008
               INTO CORRESPONDING FIELDS OF TABLE IT_ITEM
               WHERE ( TYPE EQ 'R' OR ZR2PRO EQ 'S' )
               AND   LIFNR IN SO_LIFNR
               AND   MATNR IN SO_MATNR
               AND   EKGRP IN SO_EKGRP
               AND   SERNO IN SO_SERNO
               AND   ERDAT IN SO_DATE
               AND   ZFLAG IN R_FLAG.

      SORT IT_ITEM BY ERDAT DESCENDING SERNO DESCENDING.
    WHEN OTHERS.
      SELECT * FROM ZTMM_IF008
               INTO CORRESPONDING FIELDS OF TABLE IT_ITEM
               WHERE LIFNR IN SO_LIFNR
               AND   MATNR IN SO_MATNR
               AND   EKGRP IN SO_EKGRP
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
      WHEN 'E'.
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
    'S'    'FIELDNAME'         'ICON',
    ' '    'ICON'              'X',
    'E'    'REPTEXT_DDIC'      'Status',

    'S'    'FIELDNAME'         'SERNO',
    ' '    'JUST'              'R',
*    ' '    'KEY'               'X',
    ' '    'EDIT_MASK'         '==ALPHA',
    'E'    'REPTEXT_DDIC'      'Serial No',

*    'S'    'FIELDNAME'         'SEQNO',
*    'E'    'REPTEXT_DDIC'      'SEQUENCE',

    'S'    'FIELDNAME'         'ZFLAG',
    ' '    'JUST'              'L',
*    ' '    'KEY'               'X',
    'E'    'REPTEXT_DDIC'      'Flag',

*    'S'    'FIELDNAME'         'MESSAGE',
*    'E'    'REPTEXT_DDIC'      'MESSAGE',

    'S'    'FIELDNAME'         'LIFNR',
    ' '    'JUST'              'L',
*    ' '    'KEY'               'X',
    ' '    'EMPHASIZE'         'C100',
    ' '    'HOTSPOT'           'X',
    'E'    'REPTEXT_DDIC'      'Vendor',

    'S'    'FIELDNAME'         'MATNR',
    ' '    'JUST'              'L',
*    ' '    'KEY'               'X',
    'E'    'REPTEXT_DDIC'      'Material',

    'S'    'FIELDNAME'         'TYPE',
    ' '    'JUST'              'L',
*    ' '    'KEY'               'X',
    'E'    'REPTEXT_DDIC'      'Type',

    'S'    'FIELDNAME'         'ZR2PRO',
    ' '    'JUST'              'L',
    ' '    'EMPHASIZE'         'C300',
    'E'    'REPTEXT_DDIC'      'Re-processing Ind',

    'S'    'FIELDNAME'         'ZREDOC',
    ' '    'JUST'              'R',
    ' '    'EMPHASIZE'         'C300',
    'E'    'REPTEXT_DDIC'      'Re-processing Doc',

    'S'    'FIELDNAME'         'EKORG',
    ' '    'JUST'              'L',
    'E'    'REPTEXT_DDIC'      'Purchasing Org',

    'S'    'FIELDNAME'         'ESOKZ',
    ' '    'JUST'              'R',
    'E'    'REPTEXT_DDIC'      'Purchasing Info Cat.',

    'S'    'FIELDNAME'         'MEINS',
    ' '    'JUST'              'L',
    'E'    'REPTEXT_DDIC'      'Order Unit',

    'S'    'FIELDNAME'         'APLFZ',
    ' '    'JUST'              'R',
    'E'    'REPTEXT_DDIC'      'Planned delivery time',

    'S'    'FIELDNAME'         'NORBM',
    ' '    'JUST'              'R',
    'E'    'REPTEXT_DDIC'      'STD Pur. order Qty',

    'S'    'FIELDNAME'         'NETPR',
    ' '    'JUST'              'R',
    'E'    'REPTEXT_DDIC'      'Net price in Pur.',

    'S'    'FIELDNAME'         'WAERS',
    ' '    'JUST'              'L',
    'E'    'REPTEXT_DDIC'      'Currency',

    'S'    'FIELDNAME'         'UNTTO',
    ' '    'JUST'              'R',
    'E'    'REPTEXT_DDIC'      'Underdelivery',

    'S'    'FIELDNAME'         'UEBTO',
    ' '    'JUST'              'R',
    'E'    'REPTEXT_DDIC'      'Overdelivery',

    'S'    'FIELDNAME'         'WEBRE',
    ' '    'JUST'              'L',
    'E'    'REPTEXT_DDIC'      'GR-Based I/V Verif.',

** Changed by Furong on 09/01/09
*    'S'    'FIELDNAME'         'BKGRP',
     'S'    'FIELDNAME'         'EKGRP',
** End of change
    ' '    'JUST'              'L',
    'E'    'REPTEXT_DDIC'      'Purchasing Grp',

    'S'    'FIELDNAME'         'MWSKZ',
    ' '    'JUST'              'L',
    'E'    'REPTEXT_DDIC'      'Tax Code',

    'S'    'FIELDNAME'         'BSTAE',
    ' '    'JUST'              'R',
    'E'    'REPTEXT_DDIC'      'Confirmation Cont Key',

** Changed by Furong on 09/01/09
*    'S'    'FIELDNAME'         'KSCHA',
    'S'    'FIELDNAME'         'KSCHL',
** End of change
    ' '    'JUST'              'R',
    'E'    'REPTEXT_DDIC'      'Condition type',

    'S'    'FIELDNAME'         'DATAB',
    ' '    'JUST'              'R',
    'E'    'REPTEXT_DDIC'      'Validity start date',

    'S'    'FIELDNAME'         'DATBI',
    ' '    'JUST'              'R',
    'E'    'REPTEXT_DDIC'      'Validity end date',

    'S'    'FIELDNAME'         'ERDAT',
    ' '    'JUST'              'R',
*    ' '    'KEY'               'X',
    'E'    'REPTEXT_DDIC'      'Created Date',

    'S'    'FIELDNAME'         'ERZET',
    ' '    'JUST'              'R',
*    ' '    'KEY'               'X',
    'E'    'REPTEXT_DDIC'      'Created Time',

    'S'    'FIELDNAME'         'ERNAM',
    ' '    'JUST'              'L',
    'E'    'REPTEXT_DDIC'      'User_name'.

ENDFORM.                    " build_fieldcat
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
*&      Form  fill_field_category
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0535   text
*      -->P_0536   text
*      -->P_0537   text
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
*&      Form  pf_status_set
*&---------------------------------------------------------------------*
*       This form routine is called in 'REUSE_ALV_EVENTS_GET' function
*----------------------------------------------------------------------*
*      -->P_RT_EXTAB  text
*----------------------------------------------------------------------*
FORM PF_STATUS_SET  USING    P_RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD'.
  SET TITLEBAR 'TITLE'.

ENDFORM.                    " pf_status_set
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       This form routine is called in 'REUSE_ALV_EVENTS_GET' function
*----------------------------------------------------------------------*
*      -->P_R_UCOMM  text
*      -->P_RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM USER_COMMAND  USING    R_UCOMM      LIKE SY-UCOMM
                            RS_SELFIELD  TYPE SLIS_SELFIELD.
  DATA LV_ANS.
  RS_SELFIELD-REFRESH = 'X'.

  CASE R_UCOMM.
    WHEN 'REFR'.           " refresh
      PERFORM GET_DATA.
    WHEN '&IC1'.           " move to me13 when user selects vendor code
      READ TABLE IT_DATA INDEX RS_SELFIELD-TABINDEX.
      IF SY-SUBRC = 0.
        IF RS_SELFIELD-FIELDNAME = 'LIFNR'
           AND NOT IT_DATA-LIFNR IS INITIAL.
          SET PARAMETER ID 'LIF' FIELD IT_DATA-LIFNR.
          SET PARAMETER ID 'MAT' FIELD IT_DATA-MATNR.
          SET PARAMETER ID 'EKO' FIELD IT_DATA-EKORG.
          CALL TRANSACTION 'ME13'.
        ENDIF.
      ENDIF.
*    WHEN 'DETA'. " selected line detail list view
*      CLEAR: it_item_detail.
*
*      READ TABLE it_data WITH KEY check = 'X'.
*      IF sy-subrc = 0.
*        MOVE-CORRESPONDING it_data TO ztmm_if008.
*
*        SELECT * FROM ztmm_if014
*                 INTO CORRESPONDING FIELDS OF TABLE it_item_detail
*                 WHERE serno = it_data-serno.
*        CALL SCREEN '0100'.
*      ELSE.
*        MESSAGE i004.
*      ENDIF.
    WHEN 'REPV'.            " Re-processing Info Record
      CLEAR IT_ITEM_DETAIL.
      REFRESH IT_ITEM_DETAIL.

      READ TABLE IT_DATA WITH KEY CHECK = 'X'.
      IF SY-SUBRC = 0.
        MOVE-CORRESPONDING IT_DATA TO ZTMM_IF008.

        SELECT * FROM ZTMM_IF014
                 INTO CORRESPONDING FIELDS OF TABLE IT_ITEM_DETAIL
                 WHERE SERNO = IT_DATA-SERNO.
        CALL SCREEN '0100'.
      ELSE.
        MESSAGE I004.
      ENDIF.
  ENDCASE.
ENDFORM.                    " user_command
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  IF ZTMM_IF008-TYPE   EQ 'S'
  OR ZTMM_IF008-TYPE   EQ 'R'
  OR ZTMM_IF008-ZR2PRO EQ 'S'.

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
*--ABAP Memory
      DATA L_FLAG.
      L_FLAG = 'X'.
      EXPORT L_FLAG TO MEMORY ID 'FLAG'.
*----------------------------------------------
      PERFORM RE_PROCESS_INFO.
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

    'S'    'FIELDNAME'        'SERNO',
    'E'    'COLTEXT'          'SERIAL No',

    'S'    'FIELDNAME'        'SEQNO',
    'E'    'COLTEXT'          'SEQUENCE No.',

    'S'    'FIELDNAME'        'TYPE',
    'E'    'COLTEXT'          'TYPE',

    'S'    'FIELDNAME'        'MESSAGE',
    'E'    'COLTEXT'          'MESSAGE'.

ENDFORM.                    " detail_field_cat
*&---------------------------------------------------------------------*
*&      Form  detail_fdcat_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1171   text
*      -->P_1172   text
*      -->P_1173   text
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
  CHECK ZTMM_IF008-TYPE   EQ 'S'
     OR ZTMM_IF008-TYPE   EQ 'R'
     OR ZTMM_IF008-ZR2PRO EQ 'S'.

  LOOP AT SCREEN.
    IF SCREEN-GROUP1 = 'G1'.
      SCREEN-INPUT = 0.
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
*&      Form  re_process_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RE_PROCESS_INFO .

  DATA: IS_INFO_RECORD LIKE ZSMM_IF008,
        LS_INFO_RECORD LIKE ZTMM_IF008,
        LV_R_SERNO     LIKE ZTMM_IF008-SERNO,
        LV_SERNO       LIKE ZTMM_IF008-SERNO.

  DATA: IT_XMSG LIKE TABLE OF ZTISMESSAGE WITH HEADER LINE.

  CLEAR: IS_INFO_RECORD, IT_RESULT, IT_XMSG.
  CLEAR: LS_INFO_RECORD, LV_R_SERNO, LV_SERNO.

  REFRESH: IT_RESULT, IT_XMSG.

  MOVE-CORRESPONDING ZTMM_IF008 TO LS_INFO_RECORD.
  MOVE-CORRESPONDING ZTMM_IF008 TO IS_INFO_RECORD.
  MOVE: ZTMM_IF008-SERNO        TO LV_R_SERNO.

  CLEAR ZTMM_IF008.

  CALL FUNCTION 'ZMMF_IF_INFO'
       EXPORTING
            I_INFO_RECORD = IS_INFO_RECORD
       TABLES
            E_RETURN      = IT_RESULT.

  IF SY-SUBRC EQ 0.
    READ TABLE IT_RESULT WITH KEY TYPE = 'S'.

    IF SY-SUBRC EQ 0.
      SELECT MAX( SERNO ) INTO LV_SERNO
                          FROM ZTMM_IF008
                          WHERE LIFNR = LS_INFO_RECORD-LIFNR
                          AND   MATNR = LS_INFO_RECORD-MATNR.
      IF SY-SUBRC EQ 0.
        UPDATE ZTMM_IF008 SET   ZR2PRO = 'S'
                                ZREDOC = LV_SERNO
                          WHERE SERNO  = LV_R_SERNO.
        COMMIT WORK AND WAIT.

        CLEAR: ZTMM_IF008.
        UPDATE ZTMM_IF008 SET   TYPE   = 'R'
                                ZREDOC = LV_R_SERNO
                          WHERE SERNO  = LV_SERNO.
        COMMIT WORK AND WAIT.
      ENDIF.
      PERFORM GET_DATA.
      MESSAGE S005.
      LEAVE TO SCREEN 0.
    ELSE.
      SELECT MAX( SERNO ) INTO LV_SERNO
                          FROM ZTMM_IF008
                          WHERE LIFNR = LS_INFO_RECORD-LIFNR
                          AND   MATNR = LS_INFO_RECORD-MATNR.
      IF SY-SUBRC EQ 0.
        DELETE FROM ZTMM_IF008 WHERE SERNO = LV_SERNO.
        DELETE FROM ZTMM_IF014 WHERE SERNO = LV_SERNO.
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
*        XLOGNO     =
          XDOCNO_SHOW = 'X'
        TABLES
          XMSG        = IT_XMSG.

      LEAVE TO SCREEN 0.
    ENDIF.
  ENDIF.
ENDFORM.                    " re_process_info

*----------------------------------------------------------------------*
*   INCLUDE ZRFIG01I_ALV                                               *
*----------------------------------------------------------------------*
CONSTANTS: C_F2CODE  LIKE SY-UCOMM  VALUE '&ETA'.

DATA: GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      GS_LAYOUT   TYPE SLIS_LAYOUT_ALV,
      GT_SP_GROUP TYPE SLIS_T_SP_GROUP_ALV,
      GT_EVENTS   TYPE SLIS_T_EVENT,
      GT_SORTS    TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
      GS_PRNT     TYPE SLIS_PRINT_ALV.

DATA: WA_REPID LIKE SY-REPID,
      WA_VAR_SAVE(1) TYPE C             VALUE  'A',
      WA_DEFAULT(1)  TYPE C,
      WA_EXIT(1) TYPE C,
      WA_VARIANT LIKE DISVARIANT,
      WA_VAR LIKE DISVARIANT,
      WA_ALV_FUNCTION_NAME(30) TYPE C VALUE 'REUSE_ALV_GRID_LIST',
      WA_ALV_GET_INFO_NAME(40) TYPE C.

*&---------------------------------------------------------------------*
*  FORM alv_event_pf_status_set
*&---------------------------------------------------------------------*
FORM ALV_EVENT_PF_STATUS_SET USING RT_EXTAB TYPE SLIS_T_EXTAB.

  SET PF-STATUS 'STANDARD' EXCLUDING RT_EXTAB.
  SET TITLEBAR  'STANDARD'.

ENDFORM.                    "alv_event_pf_status_set
*&---------------------------------------------------------------------*
*  FORM alv_event_user_command
*&---------------------------------------------------------------------*
FORM ALV_EVENT_USER_COMMAND USING R_UCOMM     LIKE SY-UCOMM
                                      RS_SELFIELD TYPE SLIS_SELFIELD.

  CASE R_UCOMM.
*   ---------------------------------- processing on double click.
    WHEN '&IC1'.
      READ TABLE IT_BKPF INDEX RS_SELFIELD-TABINDEX.
      SET PARAMETER ID 'GJR' FIELD IT_BKPF-GJAHR.
      SET PARAMETER ID 'BLN' FIELD IT_BKPF-BELNR.
      SET PARAMETER ID 'BUK' FIELD IT_BKPF-BUKRS.
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
*   ---------------------------------- Write Account Doc.s
    WHEN '&PRT'.
      LOOP AT IT_BKPF WHERE CHKBOX EQ 'X'.
      ENDLOOP.
      IF SY-SUBRC EQ 0.
        RS_SELFIELD-EXIT = 'X'.
      ELSE.
        MESSAGE I000 WITH 'First, You have to check in list'.
      ENDIF.
    WHEN 'LIST' OR 'GRID'.
      PERFORM SWITCH_LIST_OR_GRID USING R_UCOMM.
  ENDCASE.

  CHECK R_UCOMM EQ 'LIST' OR
        R_UCOMM EQ 'GRID'.

  RS_SELFIELD-EXIT = 'X'.

ENDFORM.                    "alv_event_user_command
*&---------------------------------------------------------------------*
*&      Form  set_events
*&---------------------------------------------------------------------*
FORM SET_EVENTS CHANGING CT_EVENTS TYPE SLIS_T_EVENT.

  FIELD-SYMBOLS: <LS_EVENT> TYPE SLIS_ALV_EVENT.

  DATA: L_EVENT TYPE LVC_FNAME.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            I_LIST_TYPE     = 0
       IMPORTING
            ET_EVENTS       = CT_EVENTS
       EXCEPTIONS
            LIST_TYPE_WRONG = 1
            OTHERS          = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    DELETE CT_EVENTS WHERE NAME NE 'END_OF_PAGE'
                       AND NAME NE 'TOP_OF_PAGE'
                       AND NAME NE 'TOP_OF_LIST'
                       AND NAME NE 'END_OF_LIST'.
    LOOP AT CT_EVENTS ASSIGNING <LS_EVENT>.
      CONCATENATE 'ALV_EVENT_'
                  <LS_EVENT>-NAME
                  INTO <LS_EVENT>-FORM.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " f01_set_evts
*&---------------------------------------------------------------------*
*&      Form  set_layout
*&---------------------------------------------------------------------*
FORM SET_LAYOUT CHANGING CS_LAYO TYPE SLIS_LAYOUT_ALV.

*... Display options
  CS_LAYO-COLWIDTH_OPTIMIZE      = SPACE.
  CS_LAYO-NO_COLHEAD             = SPACE.
  CS_LAYO-NO_HOTSPOT             = SPACE.
  CS_LAYO-ZEBRA                  = ' '.
  CS_LAYO-NO_VLINE               = SPACE.
  CS_LAYO-CELL_MERGE             = SPACE.
  CS_LAYO-NO_MIN_LINESIZE        = SPACE.
  CS_LAYO-MIN_LINESIZE           = SPACE.
  CS_LAYO-MAX_LINESIZE           = SPACE.
  CS_LAYO-WINDOW_TITLEBAR        = SPACE.
  CS_LAYO-NO_ULINE_HS            = SPACE.
*... Edit
  CS_LAYO-EDIT                   = ' '."space.
  CS_LAYO-EDIT_MODE              = ' '."space.
*... Exceptions
  CS_LAYO-LIGHTS_FIELDNAME       = ' '.
  "=> ??? ??? ???
  CS_LAYO-LIGHTS_TABNAME         = SPACE.
  CS_LAYO-LIGHTS_ROLLNAME        = SPACE.
  CS_LAYO-LIGHTS_CONDENSE        = SPACE.
*... Sums
  CS_LAYO-NO_SUMCHOICE           = SPACE.
  CS_LAYO-NO_TOTALLINE           = SPACE.
  CS_LAYO-TOTALS_BEFORE_ITEMS    = SPACE.
  CS_LAYO-TOTALS_ONLY            = SPACE.
  CS_LAYO-TOTALS_TEXT            = SPACE.
  CS_LAYO-NO_SUBCHOICE           = SPACE.
  CS_LAYO-NO_SUBTOTALS           = SPACE.
  CS_LAYO-SUBTOTALS_TEXT         = SPACE.
  CS_LAYO-NUMC_SUM               = 'X'.
  CS_LAYO-NO_UNIT_SPLITTING      = SPACE.
*... Interaction
  CS_LAYO-BOX_FIELDNAME          = 'CHKBOX'.
  CS_LAYO-BOX_TABNAME            = SPACE.
  CS_LAYO-BOX_ROLLNAME           = SPACE.
  CS_LAYO-EXPAND_FIELDNAME       = SPACE.
  CS_LAYO-HOTSPOT_FIELDNAME      = SPACE.
  CS_LAYO-NO_INPUT               = ' '.
  CS_LAYO-F2CODE                 = SPACE.
  CS_LAYO-CONFIRMATION_PROMPT    = SPACE.
  CS_LAYO-KEY_HOTSPOT            = SPACE.
  CS_LAYO-FLEXIBLE_KEY           = SPACE.
  CS_LAYO-REPREP                 = SPACE.
  CS_LAYO-GROUP_BUTTONS          = 'X'.
  CS_LAYO-NO_KEYFIX              = SPACE.
  CS_LAYO-GET_SELINFOS           = SPACE.
  CS_LAYO-GROUP_CHANGE_EDIT      = 'X'.
  CS_LAYO-NO_SCROLLING           = SPACE.
  CS_LAYO-EXPAND_ALL             = SPACE.
  CS_LAYO-NO_AUTHOR              = SPACE.
*... Detailed screen
  CS_LAYO-DETAIL_POPUP           = 'X'.
  CS_LAYO-DETAIL_INITIAL_LINES   = SPACE.
  CS_LAYO-DETAIL_TITLEBAR        = SPACE.
*... PF-status
  CS_LAYO-DEF_STATUS             = SPACE.
*... Display variants
  CS_LAYO-HEADER_TEXT            = SPACE.
  CS_LAYO-ITEM_TEXT              = SPACE.
  CS_LAYO-DEFAULT_ITEM           = SPACE.
*... colour
  CS_LAYO-INFO_FIELDNAME         = SPACE.
  CS_LAYO-COLTAB_FIELDNAME       = 'TABCOLOR'.
*... others
  CS_LAYO-LIST_APPEND            = SPACE.

ENDFORM.                    " set_layout
*---------------------------------------------------------------------*
*  FORM f01_alv_event_top_of_page
*---------------------------------------------------------------------*
FORM ALV_EVENT_TOP_OF_PAGE.

ENDFORM.                    "alv_event_top_of_page
*---------------------------------------------------------------------*
*       FORM alv_event_top_of_LIST                                    *
*---------------------------------------------------------------------*
FORM ALV_EVENT_TOP_OF_LIST.

ENDFORM.                    "alv_event_top_of_page
*---------------------------------------------------------------------*
*  FORM f01_alv_event_end_of_page
*---------------------------------------------------------------------*
FORM ALV_EVENT_END_OF_PAGE.
  NEW-LINE.
  ULINE.
  DATA: L_PAGE(10).
  WRITE : SY-PAGNO TO L_PAGE.
  WRITE: /(120) L_PAGE CENTERED.

ENDFORM.                    "alv_event_end_of_page
*---------------------------------------------------------------------*
*  FORM f01_alv_event_end_of_list
*---------------------------------------------------------------------*
FORM ALV_EVENT_END_OF_LIST.

ENDFORM.                    "alv_event_end_of_list
*&---------------------------------------------------------------------
*&      Form  switch_list_or_grid
*&---------------------------------------------------------------------
FORM SWITCH_LIST_OR_GRID USING R_UCOMM.

  DATA: LS_VARI      TYPE DISVARIANT,
       LS_SLIS_LAYO TYPE SLIS_LAYOUT_ALV,
       LT_SLIS_FCAT TYPE SLIS_T_FIELDCAT_ALV,
       LT_SLIS_SORT TYPE SLIS_T_SORTINFO_ALV,
       LT_SLIS_FILT TYPE SLIS_T_FILTER_ALV,
       LS_SLIS_PRNT TYPE SLIS_PRINT_ALV.

  IF R_UCOMM = 'LIST' AND
     WA_ALV_FUNCTION_NAME = 'REUSE_ALV_LIST_DISPLY'.
    EXIT.
  ENDIF.
  IF R_UCOMM = 'GRID' AND
     WA_ALV_FUNCTION_NAME = 'REUSE_ALV_GRID_DISPLAY'.
    EXIT.
  ENDIF.
  CASE WA_ALV_FUNCTION_NAME.
    WHEN 'REUSE_ALV_LIST_DISPLAY'.
      WA_ALV_GET_INFO_NAME = 'REUSE_ALV_LIST_LAYOUT_INFO_GET'.
    WHEN 'REUSE_ALV_GRID_DISPLAY'.
      WA_ALV_GET_INFO_NAME = 'REUSE_ALV_GRID_LAYOUT_INFO_GET'.
  ENDCASE.

  CALL FUNCTION WA_ALV_GET_INFO_NAME
       IMPORTING
            ES_LAYOUT     = LS_SLIS_LAYO
            ET_FIELDCAT   = LT_SLIS_FCAT
            ET_SORT       = LT_SLIS_SORT
            ET_FILTER     = LT_SLIS_FILT
            ES_VARIANT    = LS_VARI
       EXCEPTIONS
            NO_INFOS      = 1
            PROGRAM_ERROR = 2
            OTHERS        = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  IF R_UCOMM = 'LIST'.
    WA_ALV_FUNCTION_NAME = 'REUSE_ALV_LIST_DISPLAY'.
    CALL FUNCTION WA_ALV_FUNCTION_NAME
         EXPORTING
              I_CALLBACK_PROGRAM       = WA_REPID
              I_CALLBACK_PF_STATUS_SET = 'ALV_EVENT_PF_STATUS_SET'
              I_CALLBACK_USER_COMMAND  = 'ALV_EVENT_USER_COMMAND'
              IS_LAYOUT                = LS_SLIS_LAYO
              IT_FIELDCAT              = LT_SLIS_FCAT
              IT_SORT                  = LT_SLIS_SORT
              IT_FILTER                = LT_SLIS_FILT
              I_DEFAULT                = ' '  "gs_test-vari_default
              I_SAVE                   = WA_VAR_SAVE
              IS_VARIANT               = LS_VARI
              IS_PRINT                 = LS_SLIS_PRNT
              IT_EVENTS                = GT_EVENTS[]
         TABLES
              T_OUTTAB                 = IT_BKPF
         EXCEPTIONS
              PROGRAM_ERROR            = 1
              OTHERS                   = 2.
  ENDIF.
  IF R_UCOMM = 'GRID'.
    WA_ALV_FUNCTION_NAME = 'REUSE_ALV_GRID_DISPLAY'.
    CALL FUNCTION WA_ALV_FUNCTION_NAME
         EXPORTING
              I_CALLBACK_PROGRAM       = WA_REPID
              I_CALLBACK_PF_STATUS_SET = 'ALV_EVENT_PF_STATUS_SET'
              I_CALLBACK_USER_COMMAND  = 'ALV_EVENT_USER_COMMAND'
              IS_LAYOUT                = LS_SLIS_LAYO
              IT_FIELDCAT              = LT_SLIS_FCAT
              IT_SORT                  = LT_SLIS_SORT
              IT_FILTER                = LT_SLIS_FILT
              I_DEFAULT                = ' '  "gs_test-vari_default
              I_SAVE                   = WA_VAR_SAVE
              IS_VARIANT               = LS_VARI
              IS_PRINT                 = LS_SLIS_PRNT

         TABLES
              T_OUTTAB                 = IT_BKPF
         EXCEPTIONS
              PROGRAM_ERROR            = 1
              OTHERS                   = 2.
  ENDIF.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " switch_list_or_grid
*&--------------------------------------------------------------------
*&      Form  make_field_category
*&--------------------------------------------------------------------
FORM BUILD_FIELD_CATEGORY USING   P_FIELDNAME  P_TITLE
                                  P_OUTPUTLEN  P_KEY
                                  P_JUST       P_NOOUT
                                  P_EDIT       P_CFIELD
                                  P_QFIELD.

  DATA: LS_FIELDCAT TYPE SLIS_FIELDCAT_ALV.
  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = P_FIELDNAME.
  LS_FIELDCAT-SELTEXT_L = P_TITLE.
  LS_FIELDCAT-OUTPUTLEN = P_OUTPUTLEN.
  LS_FIELDCAT-KEY       = P_KEY.
  LS_FIELDCAT-JUST      = P_JUST.
  LS_FIELDCAT-EDIT      = P_EDIT.
  LS_FIELDCAT-NO_OUT     = P_NOOUT.
  LS_FIELDCAT-CFIELDNAME = P_CFIELD.
  LS_FIELDCAT-QFIELDNAME = P_QFIELD.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

ENDFORM.                    " fill_field_category
*&---------------------------------------------------------------------*
*&      Form  f4_variant
*&---------------------------------------------------------------------*
FORM F4_VARIANT CHANGING C_VARIANT TYPE DISVARIANT-VARIANT.

  DATA: LS_VARIANT TYPE DISVARIANT,
        L_EXIT     TYPE CHAR1.

  LS_VARIANT-REPORT = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
       EXPORTING
            IS_VARIANT          = LS_VARIANT
            I_SAVE              = 'A'
       IMPORTING
            E_EXIT              = L_EXIT
            ES_VARIANT          = LS_VARIANT
       EXCEPTIONS
            NOT_FOUND = 2.
  IF SY-SUBRC = 2.
    MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    IF L_EXIT EQ SPACE.
      C_VARIANT = LS_VARIANT-VARIANT.
    ENDIF.
  ENDIF.

ENDFORM.                    " f4_variant
*&---------------------------------------------------------------------*
*&      Form  build_sort_table
*&---------------------------------------------------------------------*
FORM BUILD_SORT_TABLE USING  P_SPOS
                             P_FIELDNAME
                             P_UP
                             P_SUBTOT
                             P_GROUP.
  DATA: LS_SORT TYPE SLIS_SORTINFO_ALV.

  LS_SORT-SPOS      = P_SPOS.
  LS_SORT-FIELDNAME = P_FIELDNAME.
  LS_SORT-UP        = P_UP.
  LS_SORT-SUBTOT    = P_SUBTOT.
  LS_SORT-GROUP     = P_GROUP.
  APPEND LS_SORT TO GT_SORTS.
ENDFORM.                    " build_sort_table
*&---------------------------------------------------------------------*
*&      Form  set_line_color
*&---------------------------------------------------------------------*
FORM SET_LINE_COLOR USING    P_COLOR.
  DATA: LS_FIELDCAT   TYPE SLIS_FIELDCAT_ALV,
        LT_COLOR      TYPE SLIS_T_SPECIALCOL_ALV,
        LS_COLOR      TYPE SLIS_SPECIALCOL_ALV.

  REFRESH LT_COLOR.
  CLEAR   LT_COLOR.
  LOOP AT GT_FIELDCAT INTO LS_FIELDCAT.
    LS_COLOR-FIELDNAME = LS_FIELDCAT-FIELDNAME.
    LS_COLOR-COLOR-COL = P_COLOR.
    LS_COLOR-COLOR-INT = CL_GUI_RESOURCES=>LIST_INTENSIFIED.
    LS_COLOR-COLOR-INV = 0.
    LS_COLOR-NOKEYCOL  = 'X'.
    APPEND LS_COLOR TO LT_COLOR.
    IT_BKPF-TABCOLOR = LT_COLOR.
  ENDLOOP.

ENDFORM.                    " set_line_color
*&---------------------------------------------------------------------*
*&      Form  MAKE_ALV_BKPF
*&---------------------------------------------------------------------*
FORM MAKE_ALV_BKPF.

  PERFORM BUILD_FIELD_CATEGORY
  USING :
   'BUKRS'     'CoCd'     '04' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'BELNR'     'Doc.no.'  '10' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'BLART'     'DT'       '02' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'CPUDT'     'Entry dt' '10' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'BLDAT'     'Doc.date' '10' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'BUDAT'     'Post.dte' '10' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'BKTXT'     'Doc.Head text'  '20' ' ' 'L'  ' '  ' '  '  ' '  ' .

  PERFORM SET_LAYOUT CHANGING GS_LAYOUT.

  PERFORM SET_EVENTS CHANGING GT_EVENTS.

  CALL FUNCTION WA_ALV_FUNCTION_NAME
       EXPORTING
            I_CALLBACK_PROGRAM       = WA_REPID
            I_CALLBACK_PF_STATUS_SET = 'ALV_EVENT_PF_STATUS_SET'
            I_CALLBACK_USER_COMMAND  = 'ALV_EVENT_USER_COMMAND'
            IS_LAYOUT                = GS_LAYOUT
            IT_FIELDCAT              = GT_FIELDCAT[]
            IT_SPECIAL_GROUPS        = GT_SP_GROUP[]
            IT_SORT                  = GT_SORTS[]
            I_DEFAULT                = WA_DEFAULT
            I_SAVE                   = WA_VAR_SAVE
            IS_VARIANT               = WA_VAR
            IT_EVENTS                = GT_EVENTS[]
            IS_PRINT                 = GS_PRNT
       TABLES
            T_OUTTAB                 = IT_BKPF.
ENDFORM.                    " MAKE_ALV_BKPF

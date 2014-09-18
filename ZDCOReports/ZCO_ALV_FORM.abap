*----------------------------------------------------------------------*
*   INCLUDE ZCO_ALV_FORM                                               *
*----------------------------------------------------------------------*

*&-------------------------------------------------------------------*
*&      Form  ALV_SET_DEFAULT
*&-------------------------------------------------------------------*
FORM ALV_SET_LAYOUT USING  P_LFILEDNAME P_LTABNAME
                           P_FIELDNAME  P_TABNAME.

  CLEAR : GS_LAYOUT,  C_PROGNAME, C_INCLNAME.

  C_PROGNAME  = SY-REPID.
  C_INCLNAME  = SY-REPID.

**-- Display options
  GS_LAYOUT-COLWIDTH_OPTIMIZE      = 'X'.
*  GS_LAYOUT-NO_COLHEAD             = SPACE.
*  GS_LAYOUT-NO_HOTSPOT             = SPACE.
*  GS_LAYOUT-ZEBRA                  = 'X'.
*  GS_LAYOUT-NO_VLINE               = SPACE.
** GS_LAYOUT-cell_merge             = 'X'.
*  GS_LAYOUT-NO_MIN_LINESIZE        = SPACE.
*  GS_LAYOUT-MIN_LINESIZE           = SPACE.
*  GS_LAYOUT-MAX_LINESIZE           = SPACE.
**  gS_LAYOUT-window_titlebar        = 'Program List'.
*  GS_LAYOUT-NO_ULINE_HS            = SPACE.
**... Edit
** GS_LAYOUT-edit                   = 'X'."space.
** GS_LAYOUT-edit_mode              = 'X'."space.
**... Exceptions
*  GS_LAYOUT-LIGHTS_FIELDNAME       = P_LFILEDNAME.
*  GS_LAYOUT-LIGHTS_TABNAME         = P_LTABNAME.
*  GS_LAYOUT-LIGHTS_ROLLNAME        = SPACE.
*  GS_LAYOUT-LIGHTS_CONDENSE        = SPACE.
**... Sums
*  GS_LAYOUT-NO_SUMCHOICE           = SPACE.
*  GS_LAYOUT-NO_TOTALLINE           = SPACE.
*  GS_LAYOUT-TOTALS_BEFORE_ITEMS    = SPACE.
*  GS_LAYOUT-TOTALS_ONLY            = SPACE.
*  GS_LAYOUT-TOTALS_TEXT            = SPACE.
*  GS_LAYOUT-NO_SUBCHOICE           = SPACE.
*  GS_LAYOUT-NO_SUBTOTALS           = SPACE.
*  GS_LAYOUT-SUBTOTALS_TEXT         = SPACE.
*  GS_LAYOUT-NUMC_SUM               = 'X'.
*  GS_LAYOUT-NO_UNIT_SPLITTING      = SPACE.
**... Interaction
*  GS_LAYOUT-BOX_FIELDNAME          = P_FIELDNAME.
*  GS_LAYOUT-BOX_TABNAME            = P_TABNAME.
*  GS_LAYOUT-BOX_ROLLNAME           = SPACE.
*  GS_LAYOUT-EXPAND_FIELDNAME       = SPACE.
*  GS_LAYOUT-HOTSPOT_FIELDNAME      = SPACE.
*  GS_LAYOUT-NO_INPUT               = SPACE.
*  GS_LAYOUT-F2CODE                 = 'F2KEY'.
*  GS_LAYOUT-CONFIRMATION_PROMPT    = SPACE.
*  GS_LAYOUT-KEY_HOTSPOT            = SPACE.
*  GS_LAYOUT-FLEXIBLE_KEY           = SPACE.
*  GS_LAYOUT-REPREP                 = SPACE.
** GS_LAYOUT-GROUP_BUTTONS          = 'X'."space.
*  GS_LAYOUT-NO_KEYFIX              = SPACE.
*  GS_LAYOUT-GET_SELINFOS           = SPACE.
** GS_LAYOUT-GROUP_CHANGE_EDIT      = 'X'.
*  GS_LAYOUT-NO_SCROLLING           = SPACE.
*  GS_LAYOUT-EXPAND_ALL             = SPACE.
** GS_LAYOUT-no_author             = space.
**... Detailed screen
*  GS_LAYOUT-DETAIL_POPUP           = 'X'.
*  GS_LAYOUT-DETAIL_INITIAL_LINES   = SPACE.
*  GS_LAYOUT-DETAIL_TITLEBAR        = SPACE.
**... PF-status
*  GS_LAYOUT-DEF_STATUS             = 'A'.
**... Display variants
*  GS_LAYOUT-HEADER_TEXT            = SPACE.
*  GS_LAYOUT-ITEM_TEXT              = SPACE.
*  GS_LAYOUT-DEFAULT_ITEM           = SPACE.
**... colour
*  GS_LAYOUT-INFO_FIELDNAME         = SPACE.
**  GS_LAYOUT-coltab_fieldname      = 'TABCOLOR'.
**... others
*  GS_LAYOUT-LIST_APPEND            = SPACE.

ENDFORM.                    " ALV_SET_DEFAULT
*&-------------------------------------------------------------------*
*&      Form  ALV_SET_EVENT
*&-------------------------------------------------------------------*
FORM ALV_GET_EVENT USING PT_EVENTS TYPE SLIS_T_EVENT.

  DATA: LS_EVENT TYPE SLIS_ALV_EVENT.

  CLEAR : PT_EVENTS[].

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      I_LIST_TYPE = 0
    IMPORTING
      ET_EVENTS   = PT_EVENTS.

  READ TABLE PT_EVENTS WITH KEY NAME =  SLIS_EV_TOP_OF_PAGE
                           INTO LS_EVENT.
  IF SY-SUBRC = 0.
    MOVE SLIS_EV_TOP_OF_PAGE     TO LS_EVENT-FORM.
    APPEND LS_EVENT TO PT_EVENTS.
  ENDIF.

  READ TABLE PT_EVENTS WITH KEY NAME =  SLIS_EV_USER_COMMAND
                           INTO LS_EVENT.
  IF SY-SUBRC = 0.
    MOVE SLIS_EV_USER_COMMAND    TO LS_EVENT-FORM.
    APPEND LS_EVENT TO PT_EVENTS.
  ENDIF.

ENDFORM.                    " ALV_GET_EVENT
*&-------------------------------------------------------------------*
*&      Form  ALV_SET_TOP_PAGE
*&-------------------------------------------------------------------*
FORM ALV_SET_TOP_PAGE  USING P_TYPE
                             P_KEY
                             P_TEXT
                             PT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.

  DATA : LS_LINE  TYPE SLIS_LISTHEADER.

* TYPE :  H = Header, S = Selection, A = Action

  CLEAR  LS_LINE.
  MOVE   P_TYPE    TO LS_LINE-TYP.
  MOVE   P_KEY     TO LS_LINE-KEY.
  MOVE   P_TEXT    TO LS_LINE-INFO.
  APPEND LS_LINE   TO PT_TOP_OF_PAGE.

ENDFORM.                    " ALV_SET_TOP_PAGE

*&-------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&-------------------------------------------------------------------*
*      ??(REUSE_ALV_EVENTS_GET)?? CALL? FORM ???.            *
*--------------------------------------------------------------------*
FORM TOP_OF_PAGE.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
*           I_LOGO             = 'ENJOYSAP_LOGO'
            IT_LIST_COMMENTARY = GT_LIST_TOP_OF_PAGE.
ENDFORM.                    "TOP_OF_PAGE
*&-------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE2
*&-------------------------------------------------------------------*
*      ??(REUSE_ALV_EVENTS_GET)?? CALL? FORM ???.            *
*--------------------------------------------------------------------*
FORM TOP_OF_PAGE2.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
*           I_LOGO             = 'ENJOYSAP_LOGO'
            IT_LIST_COMMENTARY = GT_LIST_TOP_OF_PAGE2.
ENDFORM.                    "TOP_OF_PAGE
*&-------------------------------------------------------------------*
*&      ALV_GET_FIELDCAT
*&-------------------------------------------------------------------*
FORM ALV_GET_FIELDCAT  TABLES PT_FIELDCAT
                       USING  P_TABNAME.

  CLEAR : PT_FIELDCAT[].

  CONCATENATE 'IT_' P_TABNAME INTO C_ITABNAME.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME         = C_PROGNAME
      I_INTERNAL_TABNAME     = C_ITABNAME
      I_INCLNAME             = C_INCLNAME
      I_CLIENT_NEVER_DISPLAY = 'X'
    CHANGING
      CT_FIELDCAT            = PT_FIELDCAT[]
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.

  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " ALV_GET_FIELDCAT


*&-------------------------------------------------------------------*
*&      Form  ALV_LIST_DISPLAY
*&-------------------------------------------------------------------*
FORM ALV_LIST_DISPLAY  TABLES P_ITAB.


  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
   EXPORTING
     I_CALLBACK_PROGRAM             = C_PROGNAME
     I_CALLBACK_PF_STATUS_SET       = C_STATUS_SET
     I_CALLBACK_USER_COMMAND        = C_USER_COMMAND
     IS_LAYOUT                      = GS_LAYOUT
     IT_FIELDCAT                    = GT_FIELDCAT[]
     IT_SORT                        = GT_ALV_SORT[]
     IT_EVENTS                      = GT_EVENTS[]
*    IS_VARIANT                     = GS_VARIANT
     I_SAVE                         = 'A'   " A = All => Standard &user
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER        = G_EXIT_CAUSED_BY_CALLER
*     ES_EXIT_CAUSED_BY_USER         = G_EXIT_CAUSED_BY_USER_S
    TABLES
      T_OUTTAB                       = P_ITAB
   EXCEPTIONS
     PROGRAM_ERROR                  = 1
     OTHERS                         = 2
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " ALV_LIST_DISPLAY
*&-------------------------------------------------------------------*
*&      Form  ALV_GRID_DISPLAY
*&-------------------------------------------------------------------*
FORM ALV_GRID_DISPLAY  TABLES P_ITAB.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = C_PROGNAME
      I_CALLBACK_PF_STATUS_SET = C_STATUS_SET
      I_CALLBACK_USER_COMMAND  = C_USER_COMMAND
      I_CALLBACK_TOP_OF_PAGE   = 'TOP_OF_PAGE'
*     I_GRID_TITLE             = P_TITLE
*     I_SAVE                   = P_SAVE
      IS_LAYOUT                = GS_LAYOUT
      IT_FIELDCAT              = GT_FIELDCAT[]
      IT_SORT                  = GT_ALV_SORT[]
      IT_EVENTS                = GT_EVENTS[]
      I_SAVE                   = 'A'      " A = All => Standard & User
    TABLES
      T_OUTTAB                 = P_ITAB
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.


ENDFORM.                    " CALL_FUNC_ALV_GRID

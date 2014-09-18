*----------------------------------------------------------------------*
*   INCLUDE Z_MOON_ALV_FNC                                             *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ALV_EVENTS_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ALV_EVENTS_GET  USING VALUE(FP_EVENT).
  DATA LS_EVENT TYPE SLIS_ALV_EVENT.

  CASE FP_EVENT.
    WHEN 'P'.
      LS_EVENT-NAME = SLIS_EV_PF_STATUS_SET.
      LS_EVENT-FORM = 'PF_STATUS_SET'.
    WHEN 'U'.
      LS_EVENT-NAME = SLIS_EV_USER_COMMAND.
      LS_EVENT-FORM = 'USER_COMMAND'.
    WHEN 'T'.
      LS_EVENT-NAME = SLIS_EV_TOP_OF_PAGE.
      LS_EVENT-FORM = 'TOP_OF_PAGE'.
    WHEN 'D'.
      LS_EVENT-NAME = SLIS_EV_DATA_CHANGED.
      LS_EVENT-FORM = 'DATA_CHANGED'.
  ENDCASE.

  IF GT_EVENTS[] IS INITIAL.
    CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
         EXPORTING
              I_LIST_TYPE = 0  "Simple List
         IMPORTING
              ET_EVENTS   = GT_EVENTS.
  ENDIF.

  READ TABLE  GT_EVENTS TRANSPORTING NO FIELDS
                        WITH KEY  NAME = LS_EVENT-NAME.
  IF SY-SUBRC = 0.
    MODIFY GT_EVENTS FROM LS_EVENT INDEX SY-TABIX.
  ELSE.
    APPEND LS_EVENT  TO GT_EVENTS.
  ENDIF.
ENDFORM.                    " ALV_EVENTS_GET
*----------------------------------------------------------------------*
*&      Form  SET_HEADER_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SET_HEADER_LINE  USING  FP_DATA
                             FP_TYPE
                             FP_KEY
                             FP_LOW
                             FP_HIGH.
  DATA  : LS_LINE  TYPE SLIS_LISTHEADER,
          L_LDATE(10),
          L_HDATE(10).

  CHECK NOT FP_LOW IS INITIAL.
  CLEAR : LS_LINE, L_LDATE, L_HDATE.

  MOVE  : FP_TYPE    TO LS_LINE-TYP,
          FP_KEY     TO LS_LINE-KEY.

  READ TABLE GT_LISTHEADER TRANSPORTING NO FIELDS
                               WITH KEY TYP = LS_LINE-TYP
                                        KEY = LS_LINE-KEY.
  IF SY-SUBRC NE 0.
    CASE FP_DATA.
      WHEN 'P'.    "Parameters'
        CONCATENATE FP_LOW FP_HIGH        INTO LS_LINE-INFO
                                          SEPARATED BY SPACE.
      WHEN 'S'.    "Select-options
        IF FP_HIGH IS INITIAL.
          LS_LINE-INFO = FP_LOW.
        ELSE.
          CONCATENATE FP_LOW '~' FP_HIGH    INTO LS_LINE-INFO
                                            SEPARATED BY SPACE.
        ENDIF.
      WHEN 'D'.    "Date
        WRITE : FP_LOW  TO L_LDATE,
                FP_HIGH TO L_HDATE.
        IF FP_HIGH IS INITIAL.
          LS_LINE-INFO = L_LDATE.
        ELSE.
          CONCATENATE L_LDATE '~' L_HDATE INTO LS_LINE-INFO
                                          SEPARATED BY SPACE.
        ENDIF.
    ENDCASE.
    APPEND LS_LINE TO GT_LISTHEADER.
  ENDIF.
ENDFORM.                    " SET_HEADER_LINE
*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ALV_GRID_DISPLAY TABLES FT_OUTTAB
                      USING  PF_EDIT_SET.

  G_PROGRAM = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM       = G_PROGRAM
            I_CALLBACK_PF_STATUS_SET = 'PF_STATUS_SET'
            I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
            IS_LAYOUT                = GS_LAYOUT
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
*&      Form  POPUP_CONFIRM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM POPUP_CONFIRM   USING    FP_TEXT
                     CHANGING FP_ANSWER.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
       EXPORTING
            TITLEBAR       = 'Confirmation'
            TEXT_QUESTION  = FP_TEXT
            TEXT_BUTTON_1  = 'Yes'
            TEXT_BUTTON_2  = 'No'
            DEFAULT_BUTTON = '2'
       IMPORTING
            ANSWER         = FP_ANSWER
       EXCEPTIONS
            TEXT_NOT_FOUND = 1
            OTHERS         = 2.
ENDFORM.                    " POPUP_CONFIRM

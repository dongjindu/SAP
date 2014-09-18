*----------------------------------------------------------------------*
*   INCLUDE ZAPP102L_PP_LOG_MANAGEMENT_O01                             *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET TITLEBAR '9000'.

  IF SY-DYNNR EQ '9000'.
    SET PF-STATUS '9000'.
*----> Read Interface Status
    PERFORM SELECT_IF_STATUS.
  ELSEIF SY-DYNNR EQ '9100'.
    CLEAR: IT_MENU, IT_MENU[].
    IT_MENU-FCODE = 'TOGL'.  APPEND IT_MENU.
    IT_MENU-FCODE = 'INST'.  APPEND IT_MENU.
    SET PF-STATUS '9000' EXCLUDING IT_MENU.
  ENDIF.
ENDMODULE.                 " STATUS_9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  SET_CURSOR_FIELD  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_CURSOR_FIELD OUTPUT.
  IF SY-DYNNR EQ '9000' .
    SET CURSOR FIELD WA_TXT9000 LINE WA_LINE9000.
  ELSEIF SY-DYNNR EQ '9100'.
    SET CURSOR FIELD WA_TXT9100 LINE WA_LINE9100.
  ENDIF.
ENDMODULE.                 " SET_CURSOR_FIELD  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_DATA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_DATA OUTPUT.
  CLEAR : ZTPP_IF_STATUS , WA_SIGNAL , WA_TOTAL,
          WA_SUCCESS, WA_ERROR , CHK .
  MOVE : IT_SCREEN-CHK      TO  CHK                    ,
         IT_SCREEN-TABNAME  TO  ZTPP_IF_STATUS-TABNAME ,
         IT_SCREEN-TOTAL    TO  WA_TOTAL               ,
         IT_SCREEN-SUCCESS  TO  WA_SUCCESS             ,
         IT_SCREEN-ERROR    TO  WA_ERROR               .

  IF IT_SCREEN-zGO IS INITIAL.   " Interface OK!!!
    MOVE ICON_GREEN_LIGHT   TO WA_SIGNAL .
  ELSE.
    MOVE ICON_RED_LIGHT     TO WA_SIGNAL .
  ENDIF.

ENDMODULE.                 " DISPLAY_DATA  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_9200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9200 OUTPUT.
  SET PF-STATUS '9200'.
  SET TITLEBAR '9200'.
  ZTPP_IF_STATUS-ERDAT = ZTPP_IF_STATUS-AEDAT = SY-DATUM.
  ZTPP_IF_STATUS-ERZET = ZTPP_IF_STATUS-AEZET = SY-UZEIT.
  ZTPP_IF_STATUS-ERNAM = ZTPP_IF_STATUS-AENAM = SY-UNAME.
ENDMODULE.                 " STATUS_9200  OUTPUT

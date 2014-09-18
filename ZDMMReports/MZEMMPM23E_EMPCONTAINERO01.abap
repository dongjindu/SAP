************************************************************************
* Program name : SAPMZEMMPM23E_EMPCONTAINER
* Created by   : Min-su Park
* Created on   : 2003.09.25.
* Pattern      :
* Description  :
*   1. Modified Welcome screen-For Inbound delivery-Putaway process    *
*   2. Empty Container management                                      *
*                                                                      *
* Modification Log                                                     *
* Date            Developer        Request No.    Description          *
* 2003.09.19.     Min-su Park      UD1K901873     Initial Coding       *
*                                                                      *
************************************************************************
*----------------------------------------------------------------------*
***INCLUDE MZEMMPM23E_EMPCONTAINERO01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  DATA : BEGIN OF IT_MENU OCCURS 0,
          FCODE LIKE RSMPE-FUNC,
         END OF IT_MENU.
  REFRESH IT_MENU.

  IF W_MODE = 'C'.
    CLEAR IT_MENU.
    IT_MENU-FCODE = 'CREATE'.
    APPEND IT_MENU.
    CLEAR IT_MENU.
    IT_MENU-FCODE = 'PRINT'.
    APPEND IT_MENU.
    SET PF-STATUS 'MENU100' EXCLUDING IT_MENU.
    SET TITLEBAR 'CREATE'.
  ELSEIF W_MODE = 'M'.
    CLEAR IT_MENU.
    IT_MENU-FCODE = 'CHANGE'.
    APPEND IT_MENU.
    CLEAR IT_MENU.
    IT_MENU-FCODE = 'PRINT'.
    APPEND IT_MENU.
    SET PF-STATUS 'MENU100' EXCLUDING IT_MENU.
    SET TITLEBAR 'CHANGE'.
  ELSEIF W_MODE = 'D'.
    CLEAR IT_MENU.
    IT_MENU-FCODE = 'CREATE'.
    APPEND IT_MENU.
    CLEAR IT_MENU.
    IT_MENU-FCODE = 'PRINT'.
    APPEND IT_MENU.
    CLEAR IT_MENU.
    IT_MENU-FCODE = 'SAVE'.
    APPEND IT_MENU.
    SET PF-STATUS 'MENU100' EXCLUDING IT_MENU.
    SET TITLEBAR 'DISPLAY'.
  ELSEIF W_MODE = 'E'.
    CLEAR IT_MENU.
    IT_MENU-FCODE = 'CREATE'.
    APPEND IT_MENU.
    CLEAR IT_MENU.
    IT_MENU-FCODE = 'SAVE'.
    APPEND IT_MENU.
    SET PF-STATUS 'MENU100' EXCLUDING IT_MENU.
    SET TITLEBAR  'CREATE'.
  ENDIF.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SCREEN_ADJUST  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SCREEN_ADJUST OUTPUT.
  LOOP AT SCREEN.
    CASE W_MODE.
      WHEN 'C'.
        IF SCREEN-GROUP1 = 'G1'.
          SCREEN-INPUT = 0.
          SCREEN-INVISIBLE = 1.
          MODIFY SCREEN.
        ENDIF.
        CASE W_CHK_POINT.
          WHEN 'GATE 1'.
            LEIN-LETYP = 'BB'.
            IF SCREEN-GROUP1 = 'G3'.
              SCREEN-INPUT = 0.
              MODIFY SCREEN.
            ENDIF.
          WHEN OTHERS  .
            CLEAR LEIN-LETYP.
            IF SCREEN-GROUP1 = 'G3'.
              SCREEN-INPUT = 1.
              MODIFY SCREEN.
            ENDIF.
        ENDCASE.
      WHEN 'M'.
        IF SCREEN-GROUP1 = 'G2'.
          SCREEN-INPUT = 0.
          SCREEN-INVISIBLE = 0.
          MODIFY SCREEN.
        ELSE.
          SCREEN-INPUT = 1.
          SCREEN-INVISIBLE = 0.
          MODIFY SCREEN.
        ENDIF.
      WHEN 'D'.
        IF SCREEN-GROUP1 = 'G1'.
        ELSE.
          SCREEN-INPUT = 0.
          MODIFY SCREEN.
        ENDIF.
      WHEN 'E'.
        IF SCREEN-GROUP1 = 'G1'.
          SCREEN-INPUT = 0.
          SCREEN-INVISIBLE = 1.
          MODIFY SCREEN.
        ENDIF.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
    ENDCASE.
  ENDLOOP.
ENDMODULE.                 " SCREEN_ADJUST  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_FIRST_CHKPOINT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_FIRST_CHKPOINT OUTPUT.
  CHECK W_COUNT_FLG IS INITIAL.
  LECI_CHKPT_DYN-CHKIN_POINT = W_CHK_POINT.
  LTAK-LGNUM = 'P01'.
  LEIN-LETYP = 'BB' .
  W_COUNT_FLG = W_COUNT_FLG + 1.
ENDMODULE.                 " GET_FIRST_CHKPOINT  OUTPUT

************************************************************************
* Program name : SAPMZEMMPM23E_EMPCONTAINER1
* Created by  : Min-su Park                                            *
* Created on  : 2003.09.25.                                            *
* Description :                                                        *
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
MODULE status_0100 OUTPUT.
  DATA : BEGIN OF it_menu OCCURS 0,
          fcode LIKE rsmpe-func,
         END OF it_menu.
  REFRESH it_menu.

  IF w_mode = 'C'.
    CLEAR it_menu.
    it_menu-fcode = 'CREATE'.
    APPEND it_menu.
    CLEAR it_menu.
    it_menu-fcode = 'PRINT'.
    APPEND it_menu.
**--- insert by stlim (2004/04/22)
    SET PF-STATUS 'MENU100' EXCLUDING it_menu.
    IF sy-tcode EQ c_tcode.
      SET TITLEBAR 'ZWME11_C'.
    ELSE.
      SET TITLEBAR 'CREATE'.
    ENDIF.
**--- end of insert
  ELSEIF w_mode = 'M'.
    CLEAR it_menu.
    it_menu-fcode = 'CHANGE'.
    APPEND it_menu.
*   CLEAR IT_MENU.
*   IT_MENU-FCODE = 'PRINT'.
*   APPEND IT_MENU.
    SET PF-STATUS 'MENU100' EXCLUDING it_menu.
**--- insert by stlim (2004/04/22)
    IF sy-tcode EQ c_tcode.
      SET TITLEBAR 'ZWME11_M'.
    ELSE.
      SET TITLEBAR 'CHANGE'.
    ENDIF.
**--- end of insert
  ELSEIF w_mode = 'D'.
    CLEAR it_menu.
    it_menu-fcode = 'CREATE'.
    APPEND it_menu.
    CLEAR it_menu.
    it_menu-fcode = 'PRINT'.
    APPEND it_menu.
    CLEAR it_menu.
    it_menu-fcode = 'SAVE'.
    APPEND it_menu.
    SET PF-STATUS 'MENU100' EXCLUDING it_menu.
**--- insert by stlim (2004/04/22)
    IF sy-tcode EQ c_tcode.
      SET TITLEBAR 'ZWME11_D'.
    ELSE.
      SET TITLEBAR 'DISPLAY'.
    ENDIF.
*--- end of insert
  ELSEIF w_mode = 'E'.
    CLEAR it_menu.
    it_menu-fcode = 'CREATE'.
    APPEND it_menu.
    CLEAR it_menu.
    it_menu-fcode = 'SAVE'.
    APPEND it_menu.
    SET PF-STATUS 'MENU100' EXCLUDING it_menu.
**--- insert by stlim (2004/04/22)
    IF sy-tcode EQ c_tcode.
      SET TITLEBAR 'ZWME11_C'.
    ELSE.
      SET TITLEBAR  'CREATE'.
    ENDIF.
**--- end of insert
  ENDIF.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SCREEN_ADJUST  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE screen_adjust OUTPUT.

  LOOP AT SCREEN.
    CASE w_mode.
      WHEN 'C'.
        IF screen-group1 = 'G1'.
          screen-input = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
        CASE w_chk_point.
          WHEN 'GATE 1'.
            lein-letyp = 'BB'.
            IF screen-group1 = 'G3'.
              screen-input = 0.
              MODIFY SCREEN.
            ENDIF.
          WHEN OTHERS  .
            CLEAR lein-letyp.
            IF screen-group1 = 'G3'.
              screen-input = 1.
              MODIFY SCREEN.
            ENDIF.
        ENDCASE.

      WHEN 'M'.
        IF screen-group1 = 'G2'.
          screen-input = 0.
          screen-invisible = 0.
          MODIFY SCREEN.
        ELSE.
          screen-input = 1.
          screen-invisible = 0.
          MODIFY SCREEN.
        ENDIF.
      WHEN 'D'.
        IF screen-group1 = 'G1'.
        ELSE.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      WHEN 'E'.
        IF screen-group1 = 'G1'.
          screen-input = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
        screen-input = 0.
        MODIFY SCREEN.
    ENDCASE.
  ENDLOOP.

**--- insert by stlim (2004/04/26) : Local Parts Welcome Screen
  CHECK sy-tcode EQ c_tcode.
  CHECK leci_chkpt_dyn-chkin_point EQ 'GATE 3'.

  LOOP AT SCREEN.
    IF screen-group3 EQ 'GR3'.
*      screen-active = '0'.
      screen-input  = '0'.
    ENDIF.
    IF screen-name EQ 'W_NLTYP' OR screen-name EQ 'W_NLBER' OR
       screen-name EQ 'W_NLPLA'.
      screen-input = '0'.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
**--- end of insert
*set cursor field LECI_TRA_DYN-NAME_DRVR.
ENDMODULE.                 " SCREEN_ADJUST  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_FIRST_CHKPOINT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_first_chkpoint OUTPUT.
  CHECK w_count_flg IS INITIAL.
  leci_chkpt_dyn-chkin_point = w_chk_point.
  ltak-lgnum = 'P01'.
  lein-letyp = 'BB' .
  w_count_flg = w_count_flg + 1.
ENDMODULE.                 " GET_FIRST_CHKPOINT  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  set_cursor  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_cursor OUTPUT.
LECI_TRA_DYN-PASS_DATE = sy-datum.
LECI_TRA_DYN-PASS_TIME = sy-uzeit.
set cursor field 'LECI_TRA_DYN-NAME_DRVR'.
ENDMODULE.                 " set_cursor  OUTPUT

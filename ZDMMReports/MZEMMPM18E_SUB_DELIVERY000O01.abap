************************************************************************
* Program name : SAPMZEMMPM18E_SUB_DELIVERY000
* Created by   : Min-su Park
* Created on   : 2003.08.29.
* Pattern      :
* Description  : Sub-Daily Delivery Schedule z-Table
*
* Modification Log                                                     *
* Date            Developer        Request No.    Description          *
* 2003.08.29.     Min-su Park      UD1K901873     Initial Coding       *
************************************************************************
***INCLUDE MZEMMPM18E_SUB_DELIVERY000O01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET TITLEBAR  'TITLE100'.
  DATA : BEGIN OF it_menu OCCURS 0,
          fcode LIKE rsmpe-func,
         END OF it_menu.
  REFRESH it_menu.
  CASE  w_status.
*Initialization mode or Creation mode
    WHEN 'N'.
      CLEAR it_menu.
      it_menu-fcode = 'CREATE'.
      APPEND it_menu.

      CLEAR it_menu.
      it_menu-fcode = 'SELALL'.
      APPEND it_menu.

      CLEAR it_menu.
      it_menu-fcode = 'DESELALL'.
      APPEND it_menu.

      CLEAR it_menu.
      it_menu-fcode = 'DELETE'.
      APPEND it_menu.

      CLEAR it_menu.
      it_menu-fcode = 'COPY'.
      APPEND it_menu.

      SET PF-STATUS 'MENU100' EXCLUDING it_menu.
*Change Mode
    WHEN 'C'. SET PF-STATUS 'MENU100' EXCLUDING 'CHANGE'.
*Display Mode
    WHEN 'D'.
      CLEAR it_menu.
      it_menu-fcode = 'DISPLAY'.
      APPEND it_menu.

      CLEAR it_menu.
      it_menu-fcode = 'SAVE'.
      APPEND it_menu.

      CLEAR it_menu.
      it_menu-fcode = 'SELALL'.
      APPEND it_menu.

      CLEAR it_menu.
      it_menu-fcode = 'DESELALL'.
      APPEND it_menu.

      CLEAR it_menu.
      it_menu-fcode = 'DELETE'.
      APPEND it_menu.

      CLEAR it_menu.
      it_menu-fcode = 'COPY'.
      APPEND it_menu.

      SET PF-STATUS 'MENU100' EXCLUDING it_menu.
  ENDCASE.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_TABLE_DATA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_table_data OUTPUT.
  DATA : w_subrc LIKE sy-subrc.
  DATA : it_ztmm_delisch_tmp
               LIKE it_ztmm_delisch OCCURS 0 WITH HEADER LINE.

*Check whether Program is executed first.
  CHECK  w_firstchk_flg IS INITIAL.

  w_firstchk_flg = 'X'.
  CLEAR it_ztmm_delisch[].

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_ztmm_delisch_tmp
    FROM ekpo AS a INNER JOIN ekko AS b
         ON a~ebeln = b~ebeln
   WHERE b~bsart = 'JIT'.

  SORT it_ztmm_delisch_tmp BY lifnr matnr.

*Compose internal table IT_ZTMM_DELISCH for screen display.
  LOOP AT it_ztmm_delisch_tmp.
    MOVE-CORRESPONDING it_ztmm_delisch_tmp TO it_ztmm_delisch.
    AT END OF matnr.
      CLEAR w_subrc.
      PERFORM matnr_existence_chk USING w_subrc.
      IF w_subrc = 5. CONTINUE. ENDIF.
      PERFORM get_description.
      APPEND it_ztmm_delisch.
    ENDAT.
    CLEAR it_ztmm_delisch.
  ENDLOOP.
  DESCRIBE TABLE it_ztmm_delisch
                 LINES tc_ztmm_delisch-lines.
  PERFORM clear_0_time.
ENDMODULE.                 " GET_TABLE_DATA  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SCREEN_ADJUSTMENT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE screen_adjustment OUTPUT.
  MOVE-CORRESPONDING it_ztmm_delisch TO ztmm_delisch.
  w_loopc = sy-loopc.
*When Status is Display, then Screen is diplayed only.
  IF  w_status = 'D'.
    LOOP AT SCREEN.
      screen-input = 0.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.
ENDMODULE.                 " SCREEN_ADJUSTMENT  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  status_0900  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0900 OUTPUT.
*---
  SET PF-STATUS '0900'.
ENDMODULE.                 " status_0900  OUTPUT

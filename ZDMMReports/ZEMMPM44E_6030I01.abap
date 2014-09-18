*----------------------------------------------------------------------*
*   INCLUDE ZEMMPM44E_6030I01                                          *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  save_ok_code = ok_code.
  CLEAR ok_code.
  CASE sy-dynnr.
    WHEN 0100.
      CASE save_ok_code.
        WHEN 'EXIT'.
          LEAVE PROGRAM.
        WHEN 'CANC'.
          LEAVE PROGRAM.
      ENDCASE.
  ENDCASE.
ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  back  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE back INPUT.
  save_ok_code = ok_code.
  CLEAR ok_code.
  CHECK save_ok_code = 'BACK'.
  CASE sy-dynnr.
    WHEN 0100.
      SET SCREEN 0.
  ENDCASE.
ENDMODULE.                 " back  INPUT
*&---------------------------------------------------------------------*
*&      Module  get_selected_rows  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_selected_rows INPUT.
  CALL METHOD crv_gui_alv_grid->get_selected_rows
     IMPORTING
        et_row_no      =  it_roid.
ENDMODULE.                 " get_selected_rows  INPUT
*&---------------------------------------------------------------------*
*&      Module  deta  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE deta INPUT.
  CHECK save_ok_code = 'DETA'.
  PERFORM detailed_selected_view.
ENDMODULE.                 " deta  INPUT

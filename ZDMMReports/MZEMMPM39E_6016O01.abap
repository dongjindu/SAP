*----------------------------------------------------------------------*
*   INCLUDE MZEMMPM39E_6016O01                                         *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  initial_data  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE initial_data OUTPUT.
  ltak-lgnum = 'P01'.                     "Warehouse number
ENDMODULE.                 " initial_data  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  status  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status OUTPUT.

  PERFORM make_it_func.

* Instanciate PF-STATUS & TITLEBAR.
  CASE sy-tcode.
*    WHEN 'ZMME88'.
*      w_title = 'Transfer Orders for each group(Pick)'.
*      w_confirmation = 'PICK'.
**/Begin of Added by Hakchin(20030211) (For Test Use)
*    WHEN 'ZMME88S'.
*      w_title = 'Transfer Orders for each group(Pick)'.
*      w_confirmation = 'PICK'.
**/End of Added by Hakchin(20030211) (For Test Use)
    WHEN 'ZMME89'.
      w_title = 'Transfer Orders for each group(Transfer)'.
      w_confirmation = 'TRANSFER'.
  ENDCASE.
  CREATE OBJECT crv_ps
    EXPORTING im_ps      = 'PS'             "PF-STATUS
              im_it_func = it_func          "Excluding func
              im_tb      = 'TB'             "TITLEBAR
              im_title   = w_title.        "TITLE
  CLEAR it_func.
ENDMODULE.                 " status  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  list  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE list OUTPUT.
  SUPPRESS DIALOG.
  LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
  SET PF-STATUS space.
*  WRITE:/ 'My name is Hakchin Kim'.

  PERFORM get_it_ltxx TABLES it_ltxx
                      USING  ltak-refnr.
*                      USING  'F01'.

  PERFORM make_col_heading2.
*  PERFORM write_it_ltxx.
  PERFORM write_it_ltxx2.

ENDMODULE.                 " list  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  pseudo_steploop  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pseudo_steploop OUTPUT.
  READ TABLE it_ltxx INTO wa_ltxx INDEX w_top_line.
ENDMODULE.                 " pseudo_steploop  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_screen  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen OUTPUT.
  CASE save_ok_code.
    WHEN 'CALL'.
      w_pb_name = 'PB_CEAC'.
    WHEN 'CEAC'.
      w_pb_name = 'PB_CALL'.
  ENDCASE.

  CASE sy-dynnr.
    WHEN 0100.
      IF sy-tcode = 'ZMME88'. "Transfer Orders for each group(Pick)
        LOOP AT SCREEN.
          IF screen-name = 'LTAP-VLTYP'.  "Source storage type
            screen-active = 0.
          ENDIF.
          MODIFY SCREEN.
        ENDLOOP.
      ENDIF.

      IF sy-tcode = 'ZMME88'. "Transfer Orders for each group(Pick)
        LOOP AT SCREEN.
          IF screen-name = 'LTAP-VLTYP'.  "Source storage type
            screen-active = 0.
          ENDIF.
          MODIFY SCREEN.
        ENDLOOP.
      ENDIF.

*/Begin of Added by Hakchin(20040211) Needed by Sunil
      IF sy-tcode = 'ZMME88' OR "Transfer Orders for each group(Pick)
         sy-tcode = 'ZMME88S'.
        LOOP AT SCREEN.
          IF screen-name = 'PB_CALL'.  "Confirm All
            screen-active = 0.
          ENDIF.
          MODIFY SCREEN.
        ENDLOOP.
      ENDIF.
*/End of Added by Hakchin(20040211)

    WHEN 0110.
      DESCRIBE TABLE it_ltxx LINES w_lines.

      LOOP AT SCREEN.
*Push Button
        IF screen-name = w_pb_name.
          screen-active = 0.
        ENDIF.

*PageUp, PageDn
        IF screen-name = 'PB_PGUP'.
          IF w_top_line = 1.
            screen-active = 0.
          ENDIF.
        ENDIF.
        IF screen-name = 'PB_PGDN'.
          IF w_top_line = w_lines.
            screen-active = 0.
          ENDIF.
        ENDIF.

        MODIFY SCREEN.
      ENDLOOP.

    WHEN 0120.
*/Begin of Added by Hakchin(200402010)
      LOOP AT SCREEN.
        IF screen-name = 'IO_CQTY' OR  "Comparing Qty
           screen-name = 'IO_CMATNR'.  "Comparing Material
          IF wa_ltxx-nsolM <= io_sqty.  "Scanned Qty
            screen-input = 0.
          ENDIF.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
*/End of Added by Hakchin(200402010)
  ENDCASE.

ENDMODULE.                 " modify_screen  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  get_io_dqty  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_io_dqty OUTPUT.
  io_dqty = wa_ltxx-nsolM - io_sqty.
ENDMODULE.                 " get_io_dqty  OUTPUT

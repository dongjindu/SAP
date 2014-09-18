*----------------------------------------------------------------------*
*   INCLUDE MZEMMPM17E_6008I01                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  save_ok_code = ok_code.
  CLEAR ok_code.
  CASE save_ok_code.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      LEAVE PROGRAM.
    WHEN 'CLEA'.
      CLEAR: lein-lenum,  "Storage unit
       ltak-refnr.  "Group
  ENDCASE.
ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_lenum  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_lenum INPUT.
  PERFORM check_lenum USING lein-lenum.     "Check Storage unit
  IF sy-subrc <> 0.
    MESSAGE e209(l3) WITH lein-lenum.
  ENDIF.
ENDMODULE.                 " check_lenum  INPUT
*&---------------------------------------------------------------------*
*&      Module  only  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE only INPUT.
  save_ok_code = ok_code.
  CLEAR ok_code.
  CHECK save_ok_code = 'ONLY'.
  PERFORM check_lenum USING lein-lenum. "Check Storage unit
  IF sy-subrc <> 0.
*    MESSAGE e209(l3) WITH lein-lenum.
    EXIT.
  ENDIF.

  SUBMIT rllt2500 WITH t5_lgnum     = ltak-lgnum  "Warehouse number
                  WITH t5_refnr-low = ltak-refnr  "Group
                  WITH t5_offta     = 'X'   "Only open TO items
                  WITH t5_quita     = space "Only confirmed TO items
                  WITH t5_allta     = space "All TO items
                  WITH t5_direk     = 'X'   "Direct
                  WITH t5_entna     = 'X'   "Pick
                  WITH t5_aufte     = 'X'   "Allocation
                  WITH gruig        = 'X'
                  AND RETURN.
  "New page for control break
ENDMODULE.                 " only  INPUT
*&---------------------------------------------------------------------*
*&      Module  allt  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE allt INPUT.
  CHECK save_ok_code = 'ALLT'.
  PERFORM check_lenum USING lein-lenum.     "Check Storage unit
  IF sy-subrc <> 0.
*    MESSAGE e209(l3) WITH lein-lenum.
    EXIT.
  ENDIF.

  SUBMIT rllt2500 WITH t5_lgnum     = ltak-lgnum  "Warehouse number
                  WITH t5_refnr-low = ltak-refnr  "Group
                  WITH t5_offta     = space "Only open TO items
                  WITH t5_quita     = space "Only confirmed TO items
                  WITH t5_allta     = 'X'   "All TO items
                  WITH t5_direk     = 'X'   "Direct
                  WITH t5_entna     = 'X'   "Pick
                  WITH t5_aufte     = 'X'   "Allocation
                  WITH gruig        = 'X'
                  AND RETURN.
  "New page for control break

ENDMODULE.                 " allt  INPUT

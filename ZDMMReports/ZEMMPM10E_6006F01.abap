*----------------------------------------------------------------------*
*   INCLUDE ZEMMPM10E_6006F01                                          *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  get_data_from_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_from_table.
  DATA: ls_ztmm_6006_01 LIKE ztmm_6006_01.
  DATA: lt_ztmm_6006_01 LIKE TABLE OF ls_ztmm_6006_01.
  DATA: lv_datum LIKE sy-datum.
  lv_datum = sy-datum - 30.
  SELECT * INTO CORRESPONDING FIELDS OF  TABLE lt_ztmm_6006_01
    FROM resb
    WHERE bdter = sy-datum.  "    lv_datum.      "Requirements date
  break hakchin.
  MODIFY ztmm_6006_01 FROM TABLE lt_ztmm_6006_01.
ENDFORM.                    " get_data_from_table

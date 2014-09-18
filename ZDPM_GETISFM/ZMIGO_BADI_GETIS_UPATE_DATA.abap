FUNCTION zmigo_badi_getis_upate_data.
*"----------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"  TABLES
*"      IT_MIGO_BADI STRUCTURE  ZHKPMS0001
*"      IT_ITEM STRUCTURE  ZHKPMS0002
*"----------------------------------------------------------------------
  DATA : lt_pmt0009 LIKE TABLE OF zhkpmt0009  WITH HEADER LINE .

* Databse update:
  LOOP AT it_migo_badi .
    LOOP AT it_item WHERE line_id = it_migo_badi-line_id .
      IF it_item-equnr IS NOT INITIAL .
        CLEAR lt_pmt0009 .
        MOVE-CORRESPONDING it_migo_badi TO lt_pmt0009 .
        MOVE-CORRESPONDING it_item TO lt_pmt0009 .
        lt_pmt0009-ERDAT = sy-datum .
        lt_pmt0009-ERZET = sy-uzeit .
        lt_pmt0009-ERNAM = sy-uname .
        lt_pmt0009-AEDAT = sy-datum .
        lt_pmt0009-AEZET = sy-uzeit .
        lt_pmt0009-AENAM = sy-uname .
        APPEND lt_pmt0009 .
      ENDIF .
    ENDLOOP .
  ENDLOOP .

  MODIFY zhkpmt0009 FROM TABLE lt_pmt0009.
  IF sy-subrc <> 0.
*    MESSAGE a398(00) WITH 'Error update MIGO_BADI_GETIS'.
  ENDIF.



ENDFUNCTION.

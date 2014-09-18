FUNCTION z_fpp_read_wo_classification.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(MATNR) TYPE  AUSP-OBJEK
*"  TABLES
*"      CLASSIFICATION STRUCTURE  ZSPP_WO_CLASS
*"  EXCEPTIONS
*"      NO_DATA
*"----------------------------------------------------------------------
  DATA: l_it_data LIKE TABLE OF zspp_wo_class WITH HEADER LINE .
  PERFORM set_record_name .
*
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_characteristic
    FROM ausp
    WHERE objek = matnr AND
          atinn IN r_atinn AND
          klart = '001' .
  IF sy-subrc NE 0.
    RAISE no_data.
    EXIT.
  ENDIF.
  LOOP AT it_characteristic .
    SELECT SINGLE atnam INTO it_characteristic-atnam
      FROM cabn
      WHERE atinn = it_characteristic-atinn .
    MODIFY it_characteristic.
  ENDLOOP.

  PERFORM set_data TABLES l_it_data
                   USING matnr.
  MOVE l_it_data[] TO classification[] .
ENDFUNCTION.

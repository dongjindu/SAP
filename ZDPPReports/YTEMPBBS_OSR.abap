*&---------------------------------------------------------------------*
*& Report  YTEMPBBS_OSR
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ytempbbs_osr.
DATA: it_osr LIKE ztsd_osr OCCURS 0 WITH HEADER LINE.

PARAMETERS: p_check AS CHECKBOX.

START-OF-SELECTION.

  SELECT *
    INTO TABLE it_osr
    FROM ztsd_osr.
*   WHERE nation EQ space.

  LOOP AT it_osr.
    IF it_osr-uord IS NOT INITIAL.
      MOVE: it_osr-uord+9(3)  TO it_osr-nation.
*            it_osr-uord+12(2) TO it_osr-dealer.
    ELSE.
      SELECT SINGLE wo_nation
        INTO it_osr-nation
        FROM ztpp_vm
       WHERE model_code = it_osr-model_code
         AND body_no    = it_osr-vin+11(6).
    ENDIF.

    MODIFY it_osr.
  ENDLOOP.

  INSERT ztsd_osr FROM TABLE it_osr ACCEPTING DUPLICATE KEYS.

*  DELETE FROM ztsd_osr
*   WHERE nation EQ space.

  WRITE:/ 'Successfully finished'.

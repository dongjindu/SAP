FUNCTION z_fpp_vin_define.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(VTNAM) LIKE  AUSP-ATWRT
*"     REFERENCE(S219) LIKE  ZTPP_KSBOHMM-S219
*"  EXPORTING
*"     REFERENCE(RETURN) TYPE  CHAR3
*"  EXCEPTIONS
*"      NO_DATA
*"----------------------------------------------------------------------
  DATA: l_ctable            LIKE TABLE OF cuvtab_valc  WITH HEADER LINE,
        l_vals              LIKE TABLE OF cuvtab_valc  WITH HEADER LINE,
        l_ntable            LIKE TABLE OF cuvtab_valn  WITH HEADER LINE,
        l_vtint             LIKE cuvtab-vtint,
        l_find              TYPE c,
        l_check             TYPE c,
        l_slnid             LIKE cuvtab_valc-slnid,
        l_name              LIKE cabn-atnam       ,
        l_value             TYPE c,
        l_no(3)             TYPE n.

  SELECT SINGLE vtint INTO l_vtint
    FROM cuvtab
   WHERE vtnam = vtnam .

  IF sy-subrc NE 0.
    RAISE no_data.
  ENDIF.

  CALL FUNCTION 'CUTQ_SELECT_CUVTAB_VALUES'
       EXPORTING
            table_number     = l_vtint
       TABLES
            entries_char     = l_ctable
            entries_non_char = l_ntable.

  READ TABLE l_ctable INDEX 1.
  l_slnid = l_ctable-slnid   .
  l_vals[] = l_ctable[]      .

  LOOP AT l_ctable .
    IF l_slnid = l_ctable-slnid   .
      CHECK l_check IS INITIAL.
      SELECT SINGLE atnam INTO l_name
        FROM cabn
       WHERE atinn = l_ctable-atinn .
      IF l_name(06) = 'P_219_' .           "<------
        l_no = l_name+6(3) - 1 .
        l_value = s219+l_no(1) .
        IF l_value = l_ctable-valc .       "<--**
          CONTINUE .
        ELSE.
          l_check = l_ctable-valc .
        ENDIF.                              "<--**
      ELSE.
        return = l_ctable-valc .
        EXIT.
      ENDIF.                                "<------
    ELSE.
      IF l_check IS INITIAL.
        return = l_ctable-valc .
        EXIT.
      ELSE.
        CLEAR: l_check.
      ENDIF.
      l_slnid = l_ctable-slnid   .
      SELECT SINGLE atnam INTO l_name
        FROM cabn
       WHERE atinn = l_ctable-atinn .
      IF l_name(06) = 'P_219_' .
        l_no = l_name+6(3) - 1 .
        l_value = s219+l_no(1) .
        IF l_value = l_ctable-valc .
          CONTINUE .
        ELSE.
          l_check = 'E'.
        ENDIF.
      ELSE.
        return = l_ctable-valc .
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFUNCTION.

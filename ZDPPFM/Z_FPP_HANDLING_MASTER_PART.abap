FUNCTION Z_FPP_HANDLING_MASTER_PART.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(OBJECT) TYPE  EQUI-EQUNR
*"     REFERENCE(MODE) TYPE  ZRW DEFAULT 'R'
*"     REFERENCE(CTYPE) LIKE  RMCLF-KLART OPTIONAL
*"     REFERENCE(DISPLAY) TYPE  ZMODE DEFAULT 'D'
*"  TABLES
*"      VAL_TABLE STRUCTURE  ZSPP_VIN_VALUE
*"  EXCEPTIONS
*"      NO_DATA
*"      ERROR_MODE
*"      ERROR_OBJECT
*"      ERROR_VALUE
*"----------------------------------------------------------------------
  DATA: l_ctype              LIKE rmclf-klart,
        l_object             LIKE equi-equnr ,
        l_flag               type c          ,
        L_TYPE               LIKE CABN-ATFOR ,
        r_code               TYPE c        .

  IF ctype IS INITIAL.
    l_ctype = '002' .          " Equipment Master Classification

*    SELECT SINGLE equnr INTO l_object
*      FROM equi
*     WHERE equnr = object.
*
*    IF sy-subrc NE 0.
*      RAISE error_object.
*    ENDIF.
  ELSE.
    l_ctype = '001' .          " Material Master Classification

    SELECT SINGLE matnr INTO l_object
      FROM mara
     WHERE matnr = object.

    IF sy-subrc NE 0.
      RAISE error_object.
    ENDIF.
  ENDIF.

  CASE mode .
    WHEN 'R' .
      PERFORM get_values     TABLES val_table  USING object  display
                                                     L_CTYPE        .
    WHEN 'W' .
      LOOP AT val_table .
        CLEAR r_code    .
        PERFORM check_values   USING  val_table-atinn  val_table-atnam
                                      object           r_code         .

        IF r_code = space .
          PERFORM change_values USING val_table-atinn  val_table-atwrt
                               object l_ctype  r_code  L_TYPE  l_flag.
          if l_flag = 'X'.
             raise ERROR_MODE .
          endif.
          val_table-zflag = r_code.
          MODIFY val_table .
        ELSE.
          val_table-zflag = r_code.
          MODIFY val_table .
          CONTINUE       .
        ENDIF.
      ENDLOOP.
    WHEN OTHERS.
      RAISE error_mode .
  ENDCASE.
ENDFUNCTION.

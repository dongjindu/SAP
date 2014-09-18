FUNCTION z_fpp_handling_master.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(OBJECT) TYPE  MARA-MATNR
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
* Date       Developer    RequestNo     Description
*2004/12/01  Shiva        UD1K913281   1. Filed ZWORK should be 18 Char.
*                                      2. Pass error flag in the table
*                                     VAL_TABLE when raising exceptions.
*                                     3. When there is no characteristic
*                                     value in the table AUSP, instead
*                                     of "MODIFY" used "INSERT".
*2004/12/03 Shiva        UD1K913322    Error in updating inetrnal table
*                                      VAL_TABLE(not in LOOP stmt).
*06/30/2005 Chris        UD1K916729   Delete the change log creation
*&--------------------------------------------------------------------*&

  DATA: l_ctype              LIKE rmclf-klart,
        l_object             LIKE equi-equnr ,
        l_flag               type c          ,
        L_TYPE               LIKE CABN-ATFOR ,
        r_code               TYPE c        .

  IF ctype IS INITIAL .
    L_ctype = '002' .          " Equipment Master Classification
  ELSE.
    L_CTYPE = CTYPE .
  ENDIF.

  CASE L_CTYPE.
    WHEN '001'  .
      SELECT SINGLE matnr INTO l_object
        FROM mara
       WHERE matnr = object.

      IF sy-subrc NE 0.
        loop at val_table.
          val_table-zflag = 'E'.
          modify val_table transporting zflag.
        endloop.
        RAISE error_object.
      ENDIF.

    WHEN '002'  .
      SELECT SINGLE EQUNR INTO l_object
        FROM EQUI
       WHERE EQUNR = object.

      IF sy-subrc NE 0.
        loop at val_table.
          val_table-zflag = 'E'.
          modify val_table transporting zflag.
        endloop.
        RAISE error_object.
      ENDIF.
  ENDCASE.

  CASE mode .
    WHEN 'R' .
      PERFORM get_values     TABLES val_table  USING object  display
                                                     L_CTYPE        .
    WHEN 'W' .
      LOOP AT val_table .
        CLEAR r_code    .
        PERFORM check_values   USING  val_table-atinn  val_table-atnam
                                      object           r_code       .

        IF r_code = space .
          PERFORM CHECK_TYPE    USING VAL_TABLE-ATINN  L_TYPE         .
          PERFORM change_values USING val_table-atinn  val_table-atwrt
                               object l_ctype  r_code  L_TYPE   l_flag.
          if l_flag = 'X'.
            loop at val_table.
              val_table-zflag = 'E'.
              modify val_table transporting zflag.
            endloop.
            raise ERROR_MODE  .
          endif.
          val_table-zflag = r_code.
          MODIFY val_table .
        ELSE.
          val_table-zflag = r_code.
          MODIFY val_table .
          CONTINUE       .
        ENDIF.
      ENDLOOP.
*      perform log_create     tables val_table using OBJECT .
    WHEN OTHERS.
      loop at val_table.
        val_table-zflag = 'E'.
        modify val_table transporting zflag.
      endloop.
      RAISE error_mode .
  ENDCASE.
ENDFUNCTION.

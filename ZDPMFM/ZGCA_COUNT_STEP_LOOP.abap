FUNCTION zgca_count_step_loop.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(I_FIELDNAME)
*"     VALUE(I_FIELDVALUE) OPTIONAL
*"     REFERENCE(I_TCODE)
*"     REFERENCE(I_MODE) TYPE  C DEFAULT 'N'
*"  EXPORTING
*"     REFERENCE(E_COUNT) TYPE  I
*"  TABLES
*"      BDCTAB STRUCTURE  BDCDATA
*"  EXCEPTIONS
*"      NOT_FOUND_COUNT
*"      WRONG_FIELDNAME
*"----------------------------------------------------------------------

  DATA : BEGIN OF bdc_tab OCCURS 0.
          INCLUDE STRUCTURE bdcdata.
  DATA : END OF bdc_tab.

  DATA : BEGIN OF messagetab OCCURS 0.
          INCLUDE STRUCTURE bdcmsgcoll.
  DATA : END OF messagetab.

  DATA : field LIKE bdcdata-fnam,
        c(2),
        flag,
        line TYPE i.

  DESCRIBE TABLE bdctab LINES line.

  IF i_fieldvalue = ' '.
    i_fieldvalue = '1'.
  ENDIF.

  DO 99 TIMES.
    bdc_tab[] = bdctab[].
    c = sy-index .
    CONCATENATE i_fieldname '(' c ')' INTO field.
    CLEAR bdc_tab.
    bdc_tab-fnam = field.
    bdc_tab-fval = i_fieldvalue.
    APPEND bdc_tab.
    CALL TRANSACTION i_tcode   USING bdc_tab
                            UPDATE 'S'
                              MODE i_mode   "
                          MESSAGES INTO messagetab.
    LOOP AT messagetab WHERE msgid = '00' AND msgnr = '349'.
      flag = 'X'.
    ENDLOOP.
    REFRESH : messagetab, bdc_tab.
    IF flag = 'X'.
      e_count = c - 1.
      EXIT.
    ENDIF.
  ENDDO.
ENDFUNCTION.

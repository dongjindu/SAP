FUNCTION Z_HKPM_EXCEL_UPLOAD.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(FILENAME) LIKE  RLGRAP-FILENAME
*"     REFERENCE(ITAB)
*"     REFERENCE(BEGIN_LINE) TYPE  I
*"  TABLES
*"      OUTAB
*"--------------------------------------------------------------------
  TYPE-POOLS : sydes.
  DATA : it_sydes  TYPE sydes_desc,
         w_types   TYPE sydes_typeinfo,
         w_types2  TYPE sydes_typeinfo,
         w_names   TYPE sydes_nameinfo,
         g_fname(20),
         w_cnt1    TYPE i,
         w_cnt2    TYPE i.

  FIELD-SYMBOLS : <f1>, <f2>.

  DATA : l_txt1(25), l_txt2(25).
  DATA : intern LIKE TABLE OF alsmex_tabline WITH HEADER LINE.
  DATA : l_index  TYPE sy-tabix.
  DATA : l_index2 TYPE sy-tabix.
  DATA : l_date   TYPE sy-datum.
  DATA : l_col    TYPE i.
  data : l_mandt  type i.

*#1. excel file upload.
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = filename
      i_begin_col             = 1
      i_begin_row             = begin_line
      i_end_col               = 100
      i_end_row               = 30000
    TABLES
      intern                  = intern
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*#2. read internal table field name.
  DESCRIBE FIELD outab INTO it_sydes.
  READ TABLE it_sydes-types INTO w_types INDEX 2.
  l_col =  w_types-idx_name - 1.
  DELETE it_sydes-names FROM 1 TO l_col.

  READ TABLE it_sydes-names INTO w_names WITH KEY name = 'MANDT'.
  IF sy-subrc = 0.
    DELETE TABLE it_sydes-names FROM w_names.
    l_mandt  = 1.
  ENDIF.

*#3. upload data into internal table.
  LOOP AT intern.
    AT NEW row.
      CLEAR : outab, l_col.
    ENDAT.
    l_col = intern-col.
    l_index = w_types-idx_name + l_col - 1 + l_mandt.
    READ TABLE it_sydes-names  INTO w_names  INDEX l_col.
    READ TABLE it_sydes-types  INTO w_types2
                                WITH KEY idx_name = l_index.
    CONCATENATE 'OUTAB-' w_names-name INTO l_txt1.
    CONDENSE l_txt1.
    ASSIGN (l_txt1) TO <f1>.
    IF w_types2-type = 'C'.
      <f1> = intern-value.
      IF <f1> CN '1234567890'.
        TRANSLATE <f1> TO UPPER CASE.
      ENDIF.

    ELSEIF w_types2-type = 'D'.
      PERFORM remove_point USING    intern-value
                           CHANGING <f1>.
    ELSEIF w_types2-type = 'P'.
      PERFORM remove_other_char USING    intern-value
                                CHANGING <f1>.
    ELSE.
      <f1> = intern-value.
    ENDIF.

    AT END OF row.
      APPEND outab.
    ENDAT.
  ENDLOOP.


ENDFUNCTION.

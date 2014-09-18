FUNCTION z_get_mblnr_from_qmnum.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(QMNUM) TYPE  QMNUM
*"  TABLES
*"      TLINE STRUCTURE  TLINE
*"----------------------------------------------------------------------

  DATA rt_lines LIKE tline OCCURS 0 WITH HEADER LINE.

  DATA:
    BEGIN OF stxl_id,
      tdobject LIKE stxl-tdobject,
      tdname   LIKE stxl-tdname,
      tdid     LIKE stxl-tdid,
      tdspras  LIKE stxl-tdspras,
    END OF stxl_id.
  DATA off TYPE i.

  stxl_id-tdobject = 'QMEL'.
  stxl_id-tdname = qmnum.
  stxl_id-tdid = 'LTQM'.
  stxl_id-tdspras = 'E'.

  IMPORT tline TO rt_lines
     FROM DATABASE stxl(tx)
          CLIENT   '300'
          ID       stxl_id
          ACCEPTING TRUNCATION                     "important for Unicode->Nonunicode
          IGNORING CONVERSION ERRORS.

  LOOP AT rt_lines.
    IF rt_lines-tdline CP '*Material DOC:*'.
      SEARCH rt_lines-tdline FOR 'Material DOC:'.
      IF sy-subrc EQ 0.
        off = sy-fdpos + 14.
        tline-tdline =  rt_lines-tdline+off.
        APPEND tline.
      ENDIF.
    ENDIF.
  ENDLOOP.



ENDFUNCTION.

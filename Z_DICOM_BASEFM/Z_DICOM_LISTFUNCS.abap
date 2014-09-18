function z_dicom_listfuncs.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(FUNCTION_GROUP) TYPE  CHAR255 DEFAULT 'SAPLZ_DICOM'
*"  TABLES
*"      FUNCTABLE STRUCTURE  ZDCM_LSTFUNCS
*"--------------------------------------------------------------------
  tables: tfdir.

  data : func_group type char255.

  concatenate 'SAPL' function_group into func_group.

  select * from tfdir
          where pname = func_group
            and fmode = 'R'.

    move tfdir-funcname to functable.

    select single stext
       into functable-stext
       from tftit
      where spras = 'EN'
        and funcname = tfdir-funcname.

    append functable.

  endselect.

endfunction.

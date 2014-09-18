*&---------------------------------------------------------------------*
*& BDC-REPORT      ZCPP001_CU60_MULTIPLE                               *
*& Function        create multi alc code data in T_Code : CU60         *
*& Programed by    Kim Gil Hyun (Tonkey)                               *
*&                 UD1K905658
*& Reviced   by          (T.    )                                      *
*&---------------------------------------------------------------------*
*& Create on                                                           *
*& Last Modified on      /  /   by                                     *
*&---------------------------------------------------------------------*
report zcpp001_cu60 message-id zmpp
                    no standard page heading line-size 225.

data: begin of it_data occurs 0,
         table type capiparms-var_tab,       " TABLE NAME
         code(04),            " Code
         date(10),            " Date
         col_01(04),          " 1st Column
         col_02(04),          " 2nd Column
         col_03(04),          " 3rd Column
         col_04(04),
         col_05(04),
         col_06(04),
         col_07(04),
         col_08(04),
         col_09(04),
         col_10(04),
         col_11(04),
         col_12(04),
         col_13(04),
         col_14(04),
         col_15(04),
         col_16(04),
         col_17(04),
         col_18(04),
         col_19(04),
         col_20(04),
      end of it_data.
*
*     Descriptions
data: begin of wa_descriptions occurs 1.
        include structure vtdescr.
data: end of wa_descriptions.
*     Characteristics
data: begin of it_column occurs 0.
        include structure vtchars.
data: end of it_column.

data: begin of it_vtentries occurs 0.
*.      Maintain Variant Table
        include structure vtentries.
data:   column type vtentries-vtlinnoint ,
        col_name type vtentries-vtcharact .
data: end of it_vtentries.
*
data: begin of it_table_header occurs 0.
        include structure cuvtab.
data: end of it_table_header.
data: begin of it_cuvtln occurs 0.
        include structure cuvtln.
data: end of it_cuvtln.
data: begin of it_lines_old occurs 0.
        include structure cuvtln.
data: end of it_lines_old.

data: wa_lines like cuvtln.
data: begin of it_lines_new occurs 0.
        include structure cuvtln.
data: end of it_lines_new.

data: begin of it_values_c_old occurs 0.
        include structure cuvtab_valc.
data: end of it_values_c_old.

data: begin of it_values_c_new occurs 0.
        include structure cuvtab_valc.
data: end of it_values_c_new.

data: begin of it_values_n_old occurs 0.
        include structure cuvtab_valn.
data: end of it_values_n_old.

data: begin of it_values_n_new occurs 0.
        include structure cuvtab_valn.
data: end of it_values_n_new.
*
data: wa_filename             like  rlgrap-filename,
      wa_filetype             like  rlgrap-filetype value 'DAT',
      wa_bdcgroup             like  sy-uname.          " APQI-GROUPID

data: it_vtable               like table of vtentries  with header line,
      it_msg                  like table of bdcmsgcoll with header line,
      it_bdcdata              like table of bdcdata    with header line.

data: wa_date                 type d,
      wa_tmp_count(3)         type n,
      wa_tmp_flg              type c,
      wa_tmp_text(225)        type c,
      wa_table                type tablstruct-var_tab,
      wa_vtint                like cuvtab-vtint,
      wa_tableid              like vtentries-vtlineno,
      wa_charact(40)          type c,
      wa_fieldname(40)        type c,
      wa_tmp_date(10)         type c,
      wa_exist                type c,
      wa_tmp_total(06)        type n,
      wa_tmp_process(06)      type n.

field-symbols: <field_vals> .

selection-screen begin of block b1 with frame.
parameters:
      p_hd_lin                type  i   default 2   no-display,
                                             " HEAD LINE FOR EXCEL FILE.
      p_tcode                 like  tstc-tcode      no-display,
      p_cmode                 type  c               no-display,
      p_pmode                 type  c   default 'N' no-display.
selection-screen end of block b1.

selection-screen begin of block b2 with frame title text-001.
parameters: opt01 radiobutton group rad1,
            opt02 radiobutton group rad1.
selection-screen end of block b2.

*********************************************************
initialization.
*********************************************************
  wa_bdcgroup = sy-uname.
*  p_tcode  = 'CU60' .
*  IF sy-uname = 'BOBBY'.
*    p_pmode = 'A'.
*  ENDIF.

*********************************************************
top-of-page.
*********************************************************
  if opt01 = 'X'.  "Display
    perform top_of_page_for_display.
  else.  "Upload

  endif.

*********************************************************
start-of-selection.
*********************************************************
  perform bdc_upload_data.
  delete it_data index 1 .
  delete it_data index 1 .
  if opt02 = 'X'.  "For Upload
    perform create_new_data.
  else.  "For Display
    set pf-status 'CPP001_MULT'.
  endif.

****************************************************
end-of-selection.
****************************************************
  if opt01 = 'X'.
    perform make_display_data.

  endif.
*  INCLUDE zcpp103_common_routine .
***************************************************
at user-command.
***************************************************
  case sy-ucomm.
    when 'UPLOAD'.
      perform create_new_data.
    when 'EXIT'.
      leave program.
    when 'BACK'.
      leave.
    when 'CANC'.
      leave screen.
  endcase.


*&---------------------------------------------------------------------*
*&      Form  CHECK_VTINT
*&---------------------------------------------------------------------*
*       Checking Code Column By The Table - CUVTAB
*----------------------------------------------------------------------*
*      -->P_SV_VTINT  text
*      -->P_IT_REC_TABLE  text
*----------------------------------------------------------------------*
form check_vtint using    p_vtint   p_table  p_tableid.
  select single vtint into p_vtint
    from cuvtab
   where vtnam = p_table .

  p_tableid = p_vtint+5(5).
endform.                    " CHECK_VTINT

*&---------------------------------------------------------------------*
*&      Form  CALL_FUNCTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM call_function.
*  CALL FUNCTION 'CAMA_TABLE_MAINTAIN_ENTRIES'
*       EXPORTING
*            var_table       = wa_table
*       TABLES
*            var_tab_entries = it_vtable
*       EXCEPTIONS
*            error           = 1
*            OTHERS          = 2.
*
*  IF sy-subrc = 0.
*    WRITE AT: /001(40)  wa_table ,
*               041(04)  '===>'   ,
*               045(10)  ' OK    '.
*  ELSE.
*    WRITE AT: /001(40)  wa_table ,
*               041(04)  '===>'   ,
*               045(42)  ' FAIL!!*****************************'.
*  ENDIF.
*ENDFORM.                    " CALL_FUNCTION

*&---------------------------------------------------------------------*
*&      Form  GET_FIELDNAME
*&---------------------------------------------------------------------*
*       Getting Data From CUVTAB_FLD and CABN
*----------------------------------------------------------------------*
*      -->P_WA_VTINT  text
*      -->P_0263   text
*----------------------------------------------------------------------*
form get_fieldname using    p_vtint  p_pos  p_charact  p_length .
  data: l_anzst               like cabn-anzst.

  select single n~atnam n~anzst into  (p_charact, l_anzst)
    from cuvtab_fld as f inner join cabn as n
      on f~atinn = n~atinn
   where f~vtint = p_vtint
     and f~vtpos = p_pos   .

  p_length = l_anzst .
endform.                    " GET_FIELDNAME

*&---------------------------------------------------------------------*
*&      Form  GET_COUNTSIZE
*&---------------------------------------------------------------------*
*       Counting Data
*----------------------------------------------------------------------*
*      -->P_WA_VTINT  text
*      -->P_L_COUNT  text
*----------------------------------------------------------------------*
form get_countsize using    p_vtint p_count.
  select count( * ) into p_count
    from cuvtab_fld
   where vtint = p_vtint  .

  p_count = p_count - 2   .
endform.                    " GET_COUNTSIZE
*&---------------------------------------------------------------------*
*&      Form  top_of_page_for_display
*&---------------------------------------------------------------------*
*       Setting TOP-OF-PAGE
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form top_of_page_for_display.
  set left scroll-boundary column 40.
  skip.
  format color col_positive intensified on.
  write: /(159) sy-uline.
  write: / '|' no-gap,
           (20) 'TABLE NAME' no-gap,
           '|' no-gap,
           (05) 'CODE' no-gap,
           '|' no-gap,
           (10) 'DATE' no-gap,
           '|' no-gap,
           (05) 'COL01' no-gap,
           '|' no-gap,
           (05) 'COL02' no-gap,
           '|' no-gap,
           (05) 'COL03' no-gap,
           '|' no-gap,
           (05) 'COL04' no-gap,
           '|' no-gap,
           (05) 'COL05' no-gap,
           '|' no-gap,
           (05) 'COL06' no-gap,
           '|' no-gap,
           (05) 'COL07' no-gap,
           '|' no-gap,
           (05) 'COL08' no-gap,
           '|' no-gap,
           (05) 'COL09' no-gap,
           '|' no-gap,
           (05) 'COL10' no-gap,
           '|' no-gap,
           (05) 'COL11' no-gap,
           '|' no-gap,
           (05) 'COL12' no-gap,
           '|' no-gap,
           (05) 'COL13' no-gap,
           '|' no-gap,
           (05) 'COL14' no-gap,
           '|' no-gap,
           (05) 'COL15' no-gap,
           '|' no-gap,
           (05) 'COL16' no-gap,
           '|' no-gap,
           (05) 'COL17' no-gap,
           '|' no-gap,
           (05) 'COL18' no-gap,
           '|' no-gap,
           (05) 'COL19' no-gap,
           '|' no-gap,
           (05) 'COL20' no-gap,
           '|' no-gap.
  write: /(159) sy-uline.

endform.                    " top_of_page_for_display
*&---------------------------------------------------------------------*
*&      Form  call_functions
*&---------------------------------------------------------------------*
*       Calling a Function to Update data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form call_function using p_table
                         p_count .
  call function 'CUVT_UPDATE_TABLE_CONTENT'
    exporting
      table_header       = it_table_header
*   ECM_NUMBER         =
    tables
      lines_old          = it_lines_old
      lines_new          = it_lines_new
      values_c_old       = it_values_c_old
      values_c_new       = it_values_c_new
      values_n_old       = it_values_n_old
      values_n_new       = it_values_n_new .

  if sy-subrc = 0.
    perform progress_indicator using
                        50 'Data was uploaded successfully'.
    write: / p_table ,
                ':'   ,
                p_count ,
               'EA Data was(were) uploaded successfully.'.
  else.
    perform progress_indicator using
                        50 'It was not uploaded.'.
    write: /  p_table ,
              ':'   ,
              'It was not uploaded.'.
  endif.

endform.                    " call_functions
*&---------------------------------------------------------------------*
*&      Form  reset_internal_tables
*&---------------------------------------------------------------------*
*       Clearing Internal Tables
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form reset_internal_tables.
  clear: it_lines_old,
         it_lines_new,
         it_values_c_old,
         it_values_c_new,
         it_values_n_old,
         it_values_n_new .
  refresh: it_lines_old,
           it_lines_new,
           it_values_c_old,
           it_values_c_new,
           it_values_n_old,
           it_values_n_new .
  clear: it_table_header.
  refresh: it_table_header.

endform.                    " reset_internal_tables
*&---------------------------------------------------------------------*
*&      Form  read_cuvtab
*&---------------------------------------------------------------------*
*       Getting Data From CUVTAB
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_cuvtab using p_code.
  data: wa_table_header type cuvtab.
  clear: wa_table_header, it_table_header.
  refresh it_table_header.

  select single *
    into corresponding fields of wa_table_header
    from cuvtab
    where vtnam = p_code and
          vtsta = '1'.
  append wa_table_header to it_table_header.

endform.                    " read_cuvtab
*&---------------------------------------------------------------------*
*&      Form  make_new_line
*&---------------------------------------------------------------------*
*       Creation of a New Line
*----------------------------------------------------------------------*
*      -->P_IT_TABLE_HEADER_VTINT  text
*      -->P_L_LINE  text
*      -->P_L_SLNID  text
*----------------------------------------------------------------------*
form make_new_line using    p_vtint
                            p_line
                            p_slnid.
  move p_vtint to it_lines_new-vtint .
  move p_slnid to it_lines_new-slnid .
  move p_line  to it_lines_new-vtlin .
  append it_lines_new.

endform.                    " make_new_line
*&---------------------------------------------------------------------*
*&      Form  make_new_data
*&---------------------------------------------------------------------*
*       The Process of Data Creation
*----------------------------------------------------------------------*
*      -->P_IT_NEW_DATA  text
*      -->P_IT_TABLE_HEADER_VTINT  text
*      -->P_L_SLNID  text
*----------------------------------------------------------------------*
form make_new_data tables p_it_data structure it_data
                   using  p_vtint
                          p_slnid .
  data: l_date_d like sy-datum,
        l_date_c(08).
  data original_date type d.

  data l_tabix like sy-tabix.
  loop at it_column.
    clear: it_values_n_new, it_values_c_new.
    l_tabix = sy-tabix.

    search it_column-charact for 'DATE'.
    if sy-subrc = 0.   " If it is date type ...
      move p_vtint to it_values_n_new-vtint.
      move p_slnid to it_values_n_new-slnid.
      perform call_function_conversion using it_column-charact
                                             it_values_n_new-atinn.
      move 1 to it_values_n_new-vlcnt.
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*     DATE CONTROL : IT_VALUES_N_NEW-VAL_FROM
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      clear: l_date_c.
      call 'DATE_CONV_EXT_TO_INT'
        id 'DATEXT' field p_it_data-date
        id 'DATINT' field l_date_d.
      move l_date_d to l_date_c.
      move l_date_c to it_values_n_new-val_from.

      move '1' to it_values_n_new-val_code.
      append it_values_n_new .

    else.
      case l_tabix.
        when 1 .  "CODE
          move p_vtint to it_values_c_new-vtint .
          move p_slnid to it_values_c_new-slnid .
          perform call_function_conversion
                                       using it_column-charact
                                             it_values_c_new-atinn.
          move 1 to it_values_c_new-vlcnt .
          move p_it_data-code to it_values_c_new-valc.
          append it_values_c_new.
*        WHEN 2 .  "DATE USUALLY
        when 3 .                                            " 1ST KEY
          move p_vtint to it_values_c_new-vtint .
          move p_slnid to it_values_c_new-slnid .
          perform call_function_conversion
                                       using it_column-charact
                                             it_values_c_new-atinn.
          move 1 to it_values_c_new-vlcnt .
          move p_it_data-col_01 to it_values_c_new-valc.
          append it_values_c_new.

        when 4 .                                            " 2ND KEY
          move p_vtint to it_values_c_new-vtint .
          move p_slnid to it_values_c_new-slnid .
          perform call_function_conversion
                                       using it_column-charact
                                             it_values_c_new-atinn.
          move 1 to it_values_c_new-vlcnt .
          move p_it_data-col_02 to it_values_c_new-valc.
          append it_values_c_new.

        when 5 .                                            " 3RD KEY
          move p_vtint to it_values_c_new-vtint .
          move p_slnid to it_values_c_new-slnid .
          perform call_function_conversion
                                       using it_column-charact
                                             it_values_c_new-atinn.
          move 1 to it_values_c_new-vlcnt .
          move p_it_data-col_03 to it_values_c_new-valc.
          append it_values_c_new.

        when 6 .
          move p_vtint to it_values_c_new-vtint .
          move p_slnid to it_values_c_new-slnid .
          perform call_function_conversion
                                       using it_column-charact
                                             it_values_c_new-atinn.
          move 1 to it_values_c_new-vlcnt .
          move p_it_data-col_04 to it_values_c_new-valc.
          append it_values_c_new.

        when 7 .
          move p_vtint to it_values_c_new-vtint .
          move p_slnid to it_values_c_new-slnid .
          perform call_function_conversion
                                       using it_column-charact
                                             it_values_c_new-atinn.
          move 1 to it_values_c_new-vlcnt .
          move p_it_data-col_05 to it_values_c_new-valc.
          append it_values_c_new.

        when 8 .
          move p_vtint to it_values_c_new-vtint .
          move p_slnid to it_values_c_new-slnid .
          perform call_function_conversion
                                       using it_column-charact
                                             it_values_c_new-atinn.
          move 1 to it_values_c_new-vlcnt .
          move p_it_data-col_06 to it_values_c_new-valc.
          append it_values_c_new.

        when 9 .
          move p_vtint to it_values_c_new-vtint .
          move p_slnid to it_values_c_new-slnid .
          perform call_function_conversion
                                       using it_column-charact
                                             it_values_c_new-atinn.
          move 1 to it_values_c_new-vlcnt .
          move p_it_data-col_07 to it_values_c_new-valc.
          append it_values_c_new.

        when 10 .
          move p_vtint to it_values_c_new-vtint .
          move p_slnid to it_values_c_new-slnid .
          perform call_function_conversion
                                       using it_column-charact
                                             it_values_c_new-atinn.
          move 1 to it_values_c_new-vlcnt .
          move p_it_data-col_08 to it_values_c_new-valc.
          append it_values_c_new.

        when 11 .
          move p_vtint to it_values_c_new-vtint .
          move p_slnid to it_values_c_new-slnid .
          perform call_function_conversion
                                       using it_column-charact
                                             it_values_c_new-atinn.
          move 1 to it_values_c_new-vlcnt .
          move p_it_data-col_09 to it_values_c_new-valc.
          append it_values_c_new.

        when 12 .
          move p_vtint to it_values_c_new-vtint .
          move p_slnid to it_values_c_new-slnid .
          perform call_function_conversion
                                       using it_column-charact
                                             it_values_c_new-atinn.
          move 1 to it_values_c_new-vlcnt .
          move p_it_data-col_10 to it_values_c_new-valc.
          append it_values_c_new.

        when 13 .
          move p_vtint to it_values_c_new-vtint .
          move p_slnid to it_values_c_new-slnid .
          perform call_function_conversion
                                       using it_column-charact
                                             it_values_c_new-atinn.
          move 1 to it_values_c_new-vlcnt .
          move p_it_data-col_11 to it_values_c_new-valc.
          append it_values_c_new.

        when 14 .
          move p_vtint to it_values_c_new-vtint .
          move p_slnid to it_values_c_new-slnid .
          perform call_function_conversion
                                       using it_column-charact
                                             it_values_c_new-atinn.
          move 1 to it_values_c_new-vlcnt .
          move p_it_data-col_12 to it_values_c_new-valc.
          append it_values_c_new.

        when 15 .
          move p_vtint to it_values_c_new-vtint .
          move p_slnid to it_values_c_new-slnid .
          perform call_function_conversion
                                       using it_column-charact
                                             it_values_c_new-atinn.
          move 1 to it_values_c_new-vlcnt .
          move p_it_data-col_13 to it_values_c_new-valc.
          append it_values_c_new.

        when 16 .
          move p_vtint to it_values_c_new-vtint .
          move p_slnid to it_values_c_new-slnid .
          perform call_function_conversion
                                       using it_column-charact
                                             it_values_c_new-atinn.
          move 1 to it_values_c_new-vlcnt .
          move p_it_data-col_14 to it_values_c_new-valc.
          append it_values_c_new.

        when 17 .
          move p_vtint to it_values_c_new-vtint .
          move p_slnid to it_values_c_new-slnid .
          perform call_function_conversion
                                       using it_column-charact
                                             it_values_c_new-atinn.
          move 1 to it_values_c_new-vlcnt .
          move p_it_data-col_15 to it_values_c_new-valc.
          append it_values_c_new.

        when 18 .
          move p_vtint to it_values_c_new-vtint .
          move p_slnid to it_values_c_new-slnid .
          perform call_function_conversion
                                       using it_column-charact
                                             it_values_c_new-atinn.
          move 1 to it_values_c_new-vlcnt .
          move p_it_data-col_16 to it_values_c_new-valc.
          append it_values_c_new.

        when 19 .
          move p_vtint to it_values_c_new-vtint .
          move p_slnid to it_values_c_new-slnid .
          perform call_function_conversion
                                       using it_column-charact
                                             it_values_c_new-atinn.
          move 1 to it_values_c_new-vlcnt .
          move p_it_data-col_17 to it_values_c_new-valc.
          append it_values_c_new.

        when 20 .
          move p_vtint to it_values_c_new-vtint .
          move p_slnid to it_values_c_new-slnid .
          perform call_function_conversion
                                       using it_column-charact
                                             it_values_c_new-atinn.
          move 1 to it_values_c_new-vlcnt .
          move p_it_data-col_18 to it_values_c_new-valc.
          append it_values_c_new.

        when 21 .
          move p_vtint to it_values_c_new-vtint .
          move p_slnid to it_values_c_new-slnid .
          perform call_function_conversion
                                       using it_column-charact
                                             it_values_c_new-atinn.
          move 1 to it_values_c_new-vlcnt .
          move p_it_data-col_19 to it_values_c_new-valc.
          append it_values_c_new.

        when 22 .
          move p_vtint to it_values_c_new-vtint .
          move p_slnid to it_values_c_new-slnid .
          perform call_function_conversion
                                       using it_column-charact
                                             it_values_c_new-atinn.
          move 1 to it_values_c_new-vlcnt .
          move p_it_data-col_20 to it_values_c_new-valc.
          append it_values_c_new.

      endcase.

    endif.

  endloop.

endform.                    " MAKE_NEW_DATA
*&---------------------------------------------------------------------*
*&      Form  call_function_conversion
*&---------------------------------------------------------------------*
*       Characteristic's Name Conversion To Char's Internal No.
*----------------------------------------------------------------------*
*      -->P_IT_COLUMN_CHARACT  text
*      -->P_IT_VALUES_N_NEW_ATINN  text
*----------------------------------------------------------------------*
form call_function_conversion using    p_char_c
                                       p_numb_n.
  call function 'CONVERSION_EXIT_ATINN_INPUT'
       exporting
            input  = p_char_c
       importing
            output = p_numb_n.

endform.                    " call_function_conversion
*&---------------------------------------------------------------------*
*&      Form  read_variant_table_header
*&---------------------------------------------------------------------*
*       Searching Data By a Function
*----------------------------------------------------------------------*
*      -->P_IT_REC_TABLE  text
*----------------------------------------------------------------------*
form read_variant_table_header using    p_full_code.
  clear: it_column, it_column[].
  call function 'CARD_TABLE_READ_STRUCTURE'
    exporting
      var_tab                    = p_full_code
*   CHANGE_NO                  =
*   DATE                       =
      language                   = sy-langu
* IMPORTING
*   BASIC_DATA                 =
*   RETURN                     =
    tables
      descriptions               = wa_descriptions
      characteristics            = it_column
*   VALUE_ASSIGNMENT_ALT       =
    exceptions
      error                      = 1
      others                     = 2 .
endform.                    " read_variant_table_header
*&---------------------------------------------------------------------*
*&      Form  DELETE_VARIANT_TABLE
*&---------------------------------------------------------------------*
*       Updating Data By a Function
*----------------------------------------------------------------------*
*      -->P_IT_DATA_TABLE  text
*----------------------------------------------------------------------*
form delete_variant_table using    p_table.
  perform progress_indicator using 50 p_table.
  write: / 'It is the time to proccess table ', p_table, '.'.
  data: l_var_tab_entries like vtentries occurs 0.
  call function 'CAMA_TABLE_MAINTAIN_ENTRIES'
    exporting
      var_table             = p_table
     fldelete              = 'X'
*   CHANGE_NO             =
*   DATE                  =
    tables
      var_tab_entries       = l_var_tab_entries
* EXCEPTIONS
*   ERROR                 = 1
*   OTHERS                = 2
            .
  if sy-subrc <> 0.
    perform progress_indicator using
              50 'There is ERROR during deleting data...'.
    write: / p_table, ' : THERE IS ERROR DURING DELETING DATA.'.
  else.
    perform progress_indicator using
              50 'The entries were deleted successfully.'.
    write: / p_table,
           ' : The whole entries were deleted successfully. '.
  endif.

endform.                    " DELETE_VARIANT_TABLE
*&---------------------------------------------------------------------*
*&      Form  bdc_upload_data
*&---------------------------------------------------------------------*
*       Data Upload into a Internal Table
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form bdc_upload_data.
  call function 'UPLOAD'
       exporting
            codepage                = 'DAT'
            filename                = wa_filename
            filetype                = wa_filetype
       tables
            data_tab                = it_data
       exceptions
            conversion_error        = 1
            invalid_table_width     = 2
            invalid_type            = 3
            no_batch                = 4
            unknown_error           = 5
            gui_refuse_filetransfer = 6
            others                  = 7.

  if sy-subrc <> 0.
    write: /, ' Error Opening File: ', wa_filename,
          /, ' Return Code: ', sy-subrc.
    exit.
  endif.
endform.                    " BDC_UPLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  CREATE_NEW_DATA
*&---------------------------------------------------------------------*
*       Data Modification For Creating New Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_new_data.
  data: l_line  type vtentries-vtlineno,
        l_slnid type cuvtln-slnid,
        l_count type i,
        l_tabix like sy-tabix.
  data: l_flag.

  sort it_data by table
                  col_01 col_02 col_03 col_04 col_05
                  col_06 col_07 col_08 col_09 col_10
                  col_11 col_12 col_13 col_14 col_15
                  col_16 col_17 col_18 col_19 col_20.

  loop at it_data.
    l_tabix = sy-tabix.
    at new table.
      perform reset_internal_tables.
      perform delete_variant_table      using it_data-table.
      perform read_variant_table_header using it_data-table.
      perform read_cuvtab using it_data-table.  " Variant
      read table it_table_header index 1.
    endat.
    clear l_flag.
    if l_tabix <> 1.
      perform check_unique_code using l_tabix
                                      l_flag.
    endif.
    if l_flag = 'X'.
      perform write_error_code.
      continue.
    endif.
    l_line  = l_line  + 1.
    l_slnid = l_slnid + 1.
    l_count = l_count + 1.
    perform make_new_line using it_table_header-vtint
                                l_line
                                l_slnid .
    perform make_new_data tables it_data
                          using  it_table_header-vtint
                                 l_slnid .
    at end of table.
      perform call_function using it_data-table
                                  l_count.
      clear: l_line, l_slnid, l_count.
    endat.

  endloop.

endform.                    " CREATE_NEW_DATA
*&---------------------------------------------------------------------*
*&      Form  check_unique_code
*&---------------------------------------------------------------------*
*       Checking Unique Data
*----------------------------------------------------------------------*
*      -->P_L_FLAG  text
*----------------------------------------------------------------------*
form check_unique_code using p_tabix
                             p_flag.
  data: l_current_data like line of it_data.
  data: l_before_data like line of it_data.
  clear: l_current_data, l_before_data.
  p_tabix = p_tabix - 1.
  read table it_data index p_tabix.
  move-corresponding it_data to l_before_data.   "Before Row
  p_tabix = p_tabix + 1.
  read table it_data index p_tabix.
  move-corresponding it_data to l_current_data.  "Current Row
  if l_current_data-table <> l_before_data-table.
    exit.
  endif.
* check codes from 01 to 20.
  if l_current_data-col_01 <> space.
    if l_current_data-col_01 = l_before_data-col_01.
      p_flag = 'X'.
    else.
      clear p_flag.
      exit.
    endif.
  endif.
  if l_current_data-col_02 <> space.
    if l_current_data-col_02 = l_before_data-col_02.
      p_flag = 'X'.
    else.
      clear p_flag.
      exit.
    endif.
  endif.
  if l_current_data-col_03 <> space.
    if l_current_data-col_03 = l_before_data-col_03.
      p_flag = 'X'.
    else.
      clear p_flag.
      exit.
    endif.
  endif.
  if l_current_data-col_04 <> space.
    if l_current_data-col_04 = l_before_data-col_04.
      p_flag = 'X'.
    else.
      clear p_flag.
      exit.
    endif.
  endif.
  if l_current_data-col_05 <> space.
    if l_current_data-col_05 = l_before_data-col_05.
      p_flag = 'X'.
    else.
      clear p_flag.
      exit.
    endif.
  endif.
  if l_current_data-col_06 <> space.
    if l_current_data-col_06 = l_before_data-col_06.
      p_flag = 'X'.
    else.
      clear p_flag.
      exit.
    endif.
  endif.
  if l_current_data-col_07 <> space.
    if l_current_data-col_07 = l_before_data-col_07.
      p_flag = 'X'.
    else.
      clear p_flag.
      exit.
    endif.
  endif.
  if l_current_data-col_08 <> space.
    if l_current_data-col_08 = l_before_data-col_08.
      p_flag = 'X'.
    else.
      clear p_flag.
      exit.
    endif.
  endif.
  if l_current_data-col_09 <> space.
    if l_current_data-col_09 = l_before_data-col_09.
      p_flag = 'X'.
    else.
      clear p_flag.
      exit.
    endif.
  endif.
  if l_current_data-col_10 <> space.
    if l_current_data-col_10 = l_before_data-col_10.
      p_flag = 'X'.
    else.
      clear p_flag.
      exit.
    endif.
  endif.
  if l_current_data-col_11 <> space.
    if l_current_data-col_11 = l_before_data-col_11.
      p_flag = 'X'.
    else.
      clear p_flag.
      exit.
    endif.
  endif.
  if l_current_data-col_12 <> space.
    if l_current_data-col_12 = l_before_data-col_12.
      p_flag = 'X'.
    else.
      clear p_flag.
      exit.
    endif.
  endif.
  if l_current_data-col_13 <> space.
    if l_current_data-col_13 = l_before_data-col_13.
      p_flag = 'X'.
    else.
      clear p_flag.
      exit.
    endif.
  endif.
  if l_current_data-col_14 <> space.
    if l_current_data-col_14 = l_before_data-col_14.
      p_flag = 'X'.
    else.
      clear p_flag.
      exit.
    endif.
  endif.
  if l_current_data-col_15 <> space.
    if l_current_data-col_15 = l_before_data-col_15.
      p_flag = 'X'.
    else.
      clear p_flag.
      exit.
    endif.
  endif.
  if l_current_data-col_16 <> space.
    if l_current_data-col_16 = l_before_data-col_16.
      p_flag = 'X'.
    else.
      clear p_flag.
      exit.
    endif.
  endif.
  if l_current_data-col_17 <> space.
    if l_current_data-col_17 = l_before_data-col_17.
      p_flag = 'X'.
    else.
      clear p_flag.
      exit.
    endif.
  endif.
  if l_current_data-col_18 <> space.
    if l_current_data-col_18 = l_before_data-col_18.
      p_flag = 'X'.
    else.
      clear p_flag.
      exit.
    endif.
  endif.
  if l_current_data-col_19 <> space.
    if l_current_data-col_19 = l_before_data-col_19.
      p_flag = 'X'.
    else.
      clear p_flag.
      exit.
    endif.
  endif.
  if l_current_data-col_20 <> space.
    if l_current_data-col_20 = l_before_data-col_20.
      p_flag = 'X'.
    else.
      clear p_flag.
      exit.
    endif.
  endif.

endform.                    " check_unique_code
*&---------------------------------------------------------------------*
*&      Form  write_error_code
*&---------------------------------------------------------------------*
*       Display of The Process Result - Errors
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form write_error_code.
  perform progress_indicator using
                        50 'There is a unique code error...'.
  write:/ 'Table : ',
          it_data-table,
          ',',
          'No Unique Code : ',
           it_data-col_01,
           it_data-col_02,
           it_data-col_03,
           it_data-col_04,
           it_data-col_05,
           it_data-col_06,
           it_data-col_07,
           it_data-col_08,
           it_data-col_09,
           it_data-col_10,
           it_data-col_11,
           it_data-col_12,
           it_data-col_13,
           it_data-col_14,
           it_data-col_15,
           it_data-col_16,
           it_data-col_17,
           it_data-col_18,
           it_data-col_19,
           it_data-col_20.

endform.                    " write_error_code
*&---------------------------------------------------------------------*
*&      Form  make_display_data
*&---------------------------------------------------------------------*
*       Display of The Process Result
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_display_data.
  data flag.
  loop at it_data.
    at new table.
      if flag <> 'X'.
        format color col_positive intensified off.
        flag = 'X'.
      else.
        format color col_positive intensified on.
        flag = ' '.
      endif.
      perform setting_column_name.
    endat.
    if flag <> 'X'.
      format color col_positive intensified off.
      flag = 'X'.
    else.
      format color col_positive intensified on.
      flag = ' '.
    endif.
    perform write_body .
    at end of table.
      write: /(159) sy-uline.
    endat.
  endloop.
endform.                    " make_display_data
*&---------------------------------------------------------------------*
*&      Form  PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*       Progress Indicator.
*----------------------------------------------------------------------*
*  -->  PA_SZBAR  Size of Bar.
*  <--  PA_DISPL  Text to be displayed.
*----------------------------------------------------------------------*
form progress_indicator using pa_szbar
                              pa_displ.
  call function 'SAPGUI_PROGRESS_INDICATOR'
       exporting
            percentage = pa_szbar
            text       = pa_displ.
endform.                               " PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*&      Form  SETTING_COLUMN_NAME
*&---------------------------------------------------------------------*
*       Setting a Form For Display
*----------------------------------------------------------------------*
*      -->P_L_COUNT  text
*----------------------------------------------------------------------*
form setting_column_name.
  data: l_count type i.
  perform read_variant_table_header using it_data-table.
  describe table it_column lines l_count.
  write:/ '|' no-gap,
          (20) it_data-table no-gap, '|' no-gap,
          (05) '*****' no-gap, '|' no-gap,
          (10) '**********' no-gap, '|' no-gap.
  if l_count > 2.  "FIRST KEY
    read table it_column index 3.
    search it_column-charact for '219'.
    if sy-subrc = 0.
      write: (05) it_column-charact+06 no-gap,
             '|' no-gap.
    else.
      write: (05) it_column-charact+02 no-gap,
             '|' no-gap.
    endif.
  endif.
  if l_count > 3.  "SECOND KEY
    read table it_column  index 4.
    search it_column-charact for '219'.
    if sy-subrc = 0.
      write: (05) it_column-charact+06 no-gap,
             '|' no-gap.
    else.
      write: (05) it_column-charact+02 no-gap,
             '|' no-gap.
    endif.
  endif.
  if l_count > 4.  "THIRD KEY
    read table it_column  index 5.
    search it_column-charact for '219'.
    if sy-subrc = 0.
      write: (05) it_column-charact+06 no-gap,
             '|' no-gap.
    else.
      write: (05) it_column-charact+02 no-gap,
             '|' no-gap.
    endif.
  endif.
  if l_count > 5.
    read table  it_column index 6.
    search it_column-charact for '219'.
    if sy-subrc = 0.
      write: (05) it_column-charact+06 no-gap,
             '|' no-gap.
    else.
      write: (05) it_column-charact+02 no-gap,
             '|' no-gap.
    endif.
  endif.
  if l_count > 6.
    read table it_column  index 7.
    search it_column-charact for '219'.
    if sy-subrc = 0.
      write: (05) it_column-charact+06 no-gap,
             '|' no-gap.
    else.
      write: (05) it_column-charact+02 no-gap,
             '|' no-gap.
    endif.
  endif.
  if l_count > 7.
    read table it_column  index 8.
    search it_column-charact for '219'.
    if sy-subrc = 0.
      write: (05) it_column-charact+06 no-gap,
             '|' no-gap.
    else.
      write: (05) it_column-charact+02 no-gap,
             '|' no-gap.
    endif.
  endif.
  if l_count > 8.
    read table  it_column index 9.
    search it_column-charact for '219'.
    if sy-subrc = 0.
      write: (05) it_column-charact+06 no-gap,
             '|' no-gap.
    else.
      write: (05) it_column-charact+02 no-gap,
             '|' no-gap.
    endif.

  endif.
  if l_count > 9.
    read table  it_column index 10.
    search it_column-charact for '219'.
    if sy-subrc = 0.
      write: (05) it_column-charact+06 no-gap,
             '|' no-gap.
    else.
      write: (05) it_column-charact+02 no-gap,
             '|' no-gap.
    endif.
  endif.
  if l_count > 10.
    read table  it_column index 11.
    search it_column-charact for '219'.
    if sy-subrc = 0.
      write: (05) it_column-charact+06 no-gap,
             '|' no-gap.
    else.
      write: (05) it_column-charact+02 no-gap,
             '|' no-gap.
    endif.
  endif.
  if l_count > 11.
    read table  it_column index 12.
    search it_column-charact for '219'.
    if sy-subrc = 0.
      write: (05) it_column-charact+06 no-gap,
             '|' no-gap.
    else.
      write: (05) it_column-charact+02 no-gap,
             '|' no-gap.
    endif.
  endif.
  if l_count > 12.
    read table it_column  index 13.
    search it_column-charact for '219'.
    if sy-subrc = 0.
      write: (05) it_column-charact+06 no-gap,
             '|' no-gap.
    else.
      write: (05) it_column-charact+02 no-gap,
             '|' no-gap.
    endif.
  endif.
  if l_count > 13.
    read table  it_column index 14.
    search it_column-charact for '219'.
    if sy-subrc = 0.
      write: (05) it_column-charact+06 no-gap,
             '|' no-gap.
    else.
      write: (05) it_column-charact+02 no-gap,
             '|' no-gap.
    endif.
  endif.
  if l_count > 14.
    read table  it_column index 15.
    search it_column-charact for '219'.
    if sy-subrc = 0.
      write: (05) it_column-charact+06 no-gap,
             '|' no-gap.
    else.
      write: (05) it_column-charact+02 no-gap,
             '|' no-gap.
    endif.
  endif.
  if l_count > 15.
    read table  it_column index 16.
    search it_column-charact for '219'.
    if sy-subrc = 0.
      write: (05) it_column-charact+06 no-gap,
             '|' no-gap.
    else.
      write: (05) it_column-charact+02 no-gap,
             '|' no-gap.
    endif.
  endif.
  if l_count > 16.
    read table  it_column index 17.
    search it_column-charact for '219'.
    if sy-subrc = 0.
      write: (05) it_column-charact+06 no-gap,
             '|' no-gap.
    else.
      write: (05) it_column-charact+02 no-gap,
             '|' no-gap.
    endif.
  endif.
  if l_count > 17.
    read table  it_column index 18.
    search it_column-charact for '219'.
    if sy-subrc = 0.
      write: (05) it_column-charact+06 no-gap,
             '|' no-gap.
    else.
      write: (05) it_column-charact+02 no-gap,
             '|' no-gap.
    endif.
  endif.
  if l_count > 18.
    read table it_column  index 19.
    search it_column-charact for '219'.
    if sy-subrc = 0.
      write: (05) it_column-charact+06 no-gap,
             '|' no-gap.
    else.
      write: (05) it_column-charact+02 no-gap,
             '|' no-gap.
    endif.
  endif.
  if l_count > 19.
    read table  it_column index 20.
    search it_column-charact for '219'.
    if sy-subrc = 0.
      write: (05) it_column-charact+06 no-gap,
             '|' no-gap.
    else.
      write: (05) it_column-charact+02 no-gap,
             '|' no-gap.
    endif.
  endif.
  if l_count > 20.
    read table  it_column index 21.
    search it_column-charact for '219'.
    if sy-subrc = 0.
      write: (05) it_column-charact+06 no-gap,
             '|' no-gap.
    else.
      write: (05) it_column-charact+02 no-gap,
             '|' no-gap.
    endif.
  endif.
  if l_count > 21.
    read table  it_column index 22.
    search it_column-charact for '219'.
    if sy-subrc = 0.
      write: (05) it_column-charact+06 no-gap,
             '|' no-gap.
    else.
      write: (05) it_column-charact+02 no-gap,
             '|' no-gap.
    endif.
  endif.
  if l_count > 22.
    read table  it_column index 23.
    search it_column-charact for '219'.
    if sy-subrc = 0.
      write: (05) it_column-charact+06 no-gap,
             '|' no-gap.
    else.
      write: (05) it_column-charact+02 no-gap,
             '|' no-gap.
    endif.
  endif.
  if l_count > 23.
    read table  it_column index 24.
    search it_column-charact for '219'.
    if sy-subrc = 0.
      write: (05) it_column-charact+06 no-gap,
             '|' no-gap.
    else.
      write: (05) it_column-charact+02 no-gap,
             '|' no-gap.
    endif.
  endif.
  write: 159 '|' no-gap.
  write: /(159) sy-uline no-gap.
endform.                    " SETTING_COLUMN_NAME
*&---------------------------------------------------------------------*
*&      Form  WRITE_BODY
*&---------------------------------------------------------------------*
*       Setting A Form's Body Part
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form write_body.
  write: / '|' no-gap,
           (20) ' ' no-gap,
           '|' no-gap,
           (05) it_data-code no-gap,
           '|' no-gap,
           (10) it_data-date no-gap,
           '|' no-gap,
           (05) it_data-col_01 no-gap,
           '|' no-gap,
           (05) it_data-col_02 no-gap,
           '|' no-gap,
           (05) it_data-col_03 no-gap,
           '|' no-gap,
           (05) it_data-col_04 no-gap,
           '|' no-gap,
           (05) it_data-col_05 no-gap,
           '|' no-gap,
           (05) it_data-col_06 no-gap,
           '|' no-gap,
           (05) it_data-col_07 no-gap,
           '|' no-gap,
           (05) it_data-col_08 no-gap,
           '|' no-gap,
           (05) it_data-col_09 no-gap,
           '|' no-gap,
           (05) it_data-col_10 no-gap,
           '|' no-gap,
           (05) it_data-col_11 no-gap,
           '|' no-gap,
           (05) it_data-col_12 no-gap,
           '|' no-gap,
           (05) it_data-col_13 no-gap,
           '|' no-gap,
           (05) it_data-col_14 no-gap,
           '|' no-gap,
           (05) it_data-col_15 no-gap,
           '|' no-gap,
           (05) it_data-col_16 no-gap,
           '|' no-gap,
           (05) it_data-col_17 no-gap,
           '|' no-gap,
           (05) it_data-col_18 no-gap,
           '|' no-gap,
           (05) it_data-col_19 no-gap,
           '|' no-gap,
           (05) it_data-col_20 no-gap,
           '|' no-gap.

endform.                    " WRITE_BODY

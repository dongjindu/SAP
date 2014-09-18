************************************************************************
* Program Name      : ZCPP001R_CU60_MULTIPLE
* Author            : Kim Gil Hyun (Tonkey)
* Creation Date     : 2003.11.10.
* Specifications By : Bobby
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       : Create multi ALC Code data in T_Code : CU60
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT ZCPP001R_CU60_MULTIPLE  MESSAGE-ID zmpp
                               NO STANDARD PAGE HEADING LINE-SIZE 225.

DATA: BEGIN OF it_data OCCURS 0,
         table TYPE capiparms-var_tab,       " TABLE NAME
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
      END OF it_data.
*
*     Descriptions
DATA: BEGIN OF wa_descriptions OCCURS 1.
        INCLUDE STRUCTURE vtdescr.
DATA: END OF wa_descriptions.
*     Characteristics
DATA: BEGIN OF it_column OCCURS 0.
        INCLUDE STRUCTURE vtchars.
DATA: END OF it_column.

DATA: BEGIN OF it_vtentries OCCURS 0.
*.      Maintain Variant Table
        INCLUDE STRUCTURE vtentries.
DATA:   column TYPE vtentries-vtlinnoint ,
        col_name TYPE vtentries-vtcharact .
DATA: END OF it_vtentries.
*
DATA: BEGIN OF it_table_header OCCURS 0.
        INCLUDE STRUCTURE cuvtab.
DATA: END OF it_table_header.
DATA: BEGIN OF it_cuvtln OCCURS 0.
        INCLUDE STRUCTURE cuvtln.
DATA: END OF it_cuvtln.
DATA: BEGIN OF it_lines_old OCCURS 0.
        INCLUDE STRUCTURE cuvtln.
DATA: END OF it_lines_old.

DATA: wa_lines LIKE cuvtln.
DATA: BEGIN OF it_lines_new OCCURS 0.
        INCLUDE STRUCTURE cuvtln.
DATA: END OF it_lines_new.

DATA: BEGIN OF it_values_c_old OCCURS 0.
        INCLUDE STRUCTURE cuvtab_valc.
DATA: END OF it_values_c_old.

DATA: BEGIN OF it_values_c_new OCCURS 0.
        INCLUDE STRUCTURE cuvtab_valc.
DATA: END OF it_values_c_new.

DATA: BEGIN OF it_values_n_old OCCURS 0.
        INCLUDE STRUCTURE cuvtab_valn.
DATA: END OF it_values_n_old.

DATA: BEGIN OF it_values_n_new OCCURS 0.
        INCLUDE STRUCTURE cuvtab_valn.
DATA: END OF it_values_n_new.
*
DATA: wa_filename             LIKE  rlgrap-filename,
      wa_filetype             LIKE  rlgrap-filetype VALUE 'DAT',
      wa_bdcgroup             LIKE  sy-uname.          " APQI-GROUPID

DATA: it_vtable               LIKE TABLE OF vtentries  WITH HEADER LINE,
      it_msg                  LIKE TABLE OF bdcmsgcoll WITH HEADER LINE,
      it_bdcdata              LIKE TABLE OF bdcdata    WITH HEADER LINE.

DATA: wa_date                 TYPE d,
      wa_tmp_count(3)         TYPE n,
      wa_tmp_flg              TYPE c,
      wa_tmp_text(225)        TYPE c,
      wa_table                TYPE tablstruct-var_tab,
      wa_vtint                LIKE cuvtab-vtint,
      wa_tableid              LIKE vtentries-vtlineno,
      wa_charact(40)          TYPE c,
      wa_fieldname(40)        TYPE c,
      wa_tmp_date(10)         TYPE c,
      wa_exist                TYPE c,
      wa_tmp_total(06)        TYPE n,
      wa_tmp_process(06)      TYPE n.

FIELD-SYMBOLS: <field_vals> .

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS:
      p_hd_lin                TYPE  i   DEFAULT 2   NO-DISPLAY,
                                             " HEAD LINE FOR EXCEL FILE.
      p_tcode                 LIKE  tstc-tcode      NO-DISPLAY,
      p_cmode                 TYPE  c               NO-DISPLAY,
      p_pmode                 TYPE  c   DEFAULT 'N' NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-001.
PARAMETERS: opt01 RADIOBUTTON GROUP rad1,
            opt02 RADIOBUTTON GROUP rad1.
SELECTION-SCREEN END OF BLOCK b2.

*********************************************************
INITIALIZATION.
*********************************************************
  wa_bdcgroup = sy-uname.
*  p_tcode  = 'CU60' .
*  IF sy-uname = 'BOBBY'.
*    p_pmode = 'A'.
*  ENDIF.

*********************************************************
TOP-OF-PAGE.
*********************************************************
  IF opt01 = 'X'.  "Display
    PERFORM top_of_page_for_display.
  ELSE.  "Upload

  ENDIF.

*********************************************************
START-OF-SELECTION.
*********************************************************
  PERFORM bdc_upload_data.
  DELETE it_data INDEX 1 .
  DELETE it_data INDEX 1 .
  IF opt02 = 'X'.  "For Upload
    PERFORM create_new_data.
  ELSE.  "For Display
    SET PF-STATUS 'CPP001_MULT'.
  ENDIF.

****************************************************
END-OF-SELECTION.
****************************************************
  IF opt01 = 'X'.
    PERFORM make_display_data.

  ENDIF.
*  INCLUDE zcpp103_common_routine .
***************************************************
AT USER-COMMAND.
***************************************************
  CASE sy-ucomm.
    WHEN 'UPLOAD'.
      PERFORM create_new_data.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      LEAVE.
    WHEN 'CANC'.
      LEAVE SCREEN.
  ENDCASE.


*&---------------------------------------------------------------------*
*&      Form  CHECK_VTINT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SV_VTINT  text
*      -->P_IT_REC_TABLE  text
*----------------------------------------------------------------------*
FORM check_vtint USING    p_vtint   p_table  p_tableid.
  SELECT SINGLE vtint INTO p_vtint
    FROM cuvtab
   WHERE vtnam = p_table .

  p_tableid = p_vtint+5(5).
ENDFORM.                    " CHECK_VTINT

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
*       text
*----------------------------------------------------------------------*
*      -->P_WA_VTINT  text
*      -->P_0263   text
*----------------------------------------------------------------------*
FORM get_fieldname USING    p_vtint  p_pos  p_charact  p_length .
  DATA: l_anzst               LIKE cabn-anzst.

  SELECT SINGLE n~atnam n~anzst INTO  (p_charact, l_anzst)
    FROM cuvtab_fld AS f INNER JOIN cabn AS n
      ON f~atinn = n~atinn
   WHERE f~vtint = p_vtint
     AND f~vtpos = p_pos   .

  p_length = l_anzst .
ENDFORM.                    " GET_FIELDNAME

*&---------------------------------------------------------------------*
*&      Form  GET_COUNTSIZE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_VTINT  text
*      -->P_L_COUNT  text
*----------------------------------------------------------------------*
FORM get_countsize USING    p_vtint p_count.
  SELECT COUNT( * ) INTO p_count
    FROM cuvtab_fld
   WHERE vtint = p_vtint  .

  p_count = p_count - 2   .
ENDFORM.                    " GET_COUNTSIZE
*&---------------------------------------------------------------------*
*&      Form  top_of_page_for_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM top_of_page_for_display.
  SET LEFT SCROLL-BOUNDARY COLUMN 40.
  SKIP.
  FORMAT COLOR COL_POSITIVE INTENSIFIED ON.
  WRITE: /(159) sy-uline.
  WRITE: / '|' NO-GAP,
           (20) 'TABLE NAME' NO-GAP,
           '|' NO-GAP,
           (05) 'CODE' NO-GAP,
           '|' NO-GAP,
           (10) 'DATE' NO-GAP,
           '|' NO-GAP,
           (05) 'COL01' NO-GAP,
           '|' NO-GAP,
           (05) 'COL02' NO-GAP,
           '|' NO-GAP,
           (05) 'COL03' NO-GAP,
           '|' NO-GAP,
           (05) 'COL04' NO-GAP,
           '|' NO-GAP,
           (05) 'COL05' NO-GAP,
           '|' NO-GAP,
           (05) 'COL06' NO-GAP,
           '|' NO-GAP,
           (05) 'COL07' NO-GAP,
           '|' NO-GAP,
           (05) 'COL08' NO-GAP,
           '|' NO-GAP,
           (05) 'COL09' NO-GAP,
           '|' NO-GAP,
           (05) 'COL10' NO-GAP,
           '|' NO-GAP,
           (05) 'COL11' NO-GAP,
           '|' NO-GAP,
           (05) 'COL12' NO-GAP,
           '|' NO-GAP,
           (05) 'COL13' NO-GAP,
           '|' NO-GAP,
           (05) 'COL14' NO-GAP,
           '|' NO-GAP,
           (05) 'COL15' NO-GAP,
           '|' NO-GAP,
           (05) 'COL16' NO-GAP,
           '|' NO-GAP,
           (05) 'COL17' NO-GAP,
           '|' NO-GAP,
           (05) 'COL18' NO-GAP,
           '|' NO-GAP,
           (05) 'COL19' NO-GAP,
           '|' NO-GAP,
           (05) 'COL20' NO-GAP,
           '|' NO-GAP.
  WRITE: /(159) sy-uline.

ENDFORM.                    " top_of_page_for_display
*&---------------------------------------------------------------------*
*&      Form  call_functions
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_function USING p_table
                         p_count .
  CALL FUNCTION 'CUVT_UPDATE_TABLE_CONTENT'
    EXPORTING
      table_header       = it_table_header
*   ECM_NUMBER         =
    TABLES
      lines_old          = it_lines_old
      lines_new          = it_lines_new
      values_c_old       = it_values_c_old
      values_c_new       = it_values_c_new
      values_n_old       = it_values_n_old
      values_n_new       = it_values_n_new .

  IF sy-subrc = 0.
    PERFORM progress_indicator USING
                        50 'Data was uploaded successfully'.
    WRITE: / p_table ,
                ':'   ,
                p_count ,
               'EA Data was(were) uploaded successfully.'.
  ELSE.
    PERFORM progress_indicator USING
                        50 'It was not uploaded.'.
    WRITE: /  p_table ,
              ':'   ,
              'It was not uploaded.'.
  ENDIF.

ENDFORM.                    " call_functions
*&---------------------------------------------------------------------*
*&      Form  reset_internal_tables
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reset_internal_tables.
  CLEAR: it_lines_old,
         it_lines_new,
         it_values_c_old,
         it_values_c_new,
         it_values_n_old,
         it_values_n_new .
  REFRESH: it_lines_old,
           it_lines_new,
           it_values_c_old,
           it_values_c_new,
           it_values_n_old,
           it_values_n_new .
  CLEAR: it_table_header.
  REFRESH: it_table_header.

ENDFORM.                    " reset_internal_tables
*&---------------------------------------------------------------------*
*&      Form  read_cuvtab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_cuvtab USING p_code.
  DATA: wa_table_header TYPE cuvtab.
  CLEAR: wa_table_header, it_table_header.
  REFRESH it_table_header.

  SELECT SINGLE *
    INTO CORRESPONDING FIELDS OF wa_table_header
    FROM cuvtab
    WHERE vtnam = p_code AND
          vtsta = '1'.
  APPEND wa_table_header TO it_table_header.

ENDFORM.                    " read_cuvtab
*&---------------------------------------------------------------------*
*&      Form  make_new_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TABLE_HEADER_VTINT  text
*      -->P_L_LINE  text
*      -->P_L_SLNID  text
*----------------------------------------------------------------------*
FORM make_new_line USING    p_vtint
                            p_line
                            p_slnid.
  MOVE p_vtint TO it_lines_new-vtint .
  MOVE p_slnid TO it_lines_new-slnid .
  MOVE p_line  TO it_lines_new-vtlin .
  APPEND it_lines_new.

ENDFORM.                    " make_new_line
*&---------------------------------------------------------------------*
*&      Form  make_new_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_NEW_DATA  text
*      -->P_IT_TABLE_HEADER_VTINT  text
*      -->P_L_SLNID  text
*----------------------------------------------------------------------*
FORM make_new_data TABLES p_it_data STRUCTURE it_data
                   USING  p_vtint
                          p_slnid .
  DATA: l_date_d LIKE sy-datum,
        l_date_c(08).
  DATA original_date TYPE d.

  DATA l_tabix LIKE sy-tabix.
  LOOP AT it_column.
    CLEAR: it_values_n_new, it_values_c_new.
    l_tabix = sy-tabix.

    SEARCH it_column-charact FOR 'DATE'.
    IF sy-subrc = 0.   " If it is date type ...
      MOVE p_vtint TO it_values_n_new-vtint.
      MOVE p_slnid TO it_values_n_new-slnid.
      PERFORM call_function_conversion USING it_column-charact
                                             it_values_n_new-atinn.
      MOVE 1 TO it_values_n_new-vlcnt.
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*     DATE CONTROL : IT_VALUES_N_NEW-VAL_FROM
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      CLEAR: l_date_c.
      CALL 'DATE_CONV_EXT_TO_INT'
        ID 'DATEXT' FIELD p_it_data-date
        ID 'DATINT' FIELD l_date_d.
      MOVE l_date_d TO l_date_c.
      MOVE l_date_c TO it_values_n_new-val_from.

      MOVE '1' TO it_values_n_new-val_code.
      APPEND it_values_n_new .

    ELSE.
      CASE l_tabix.
        WHEN 1 .  "CODE
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_data-code TO it_values_c_new-valc.
          APPEND it_values_c_new.
*        WHEN 2 .  "DATE USUALLY
        WHEN 3 .                                            " 1ST KEY
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_data-col_01 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 4 .                                            " 2ND KEY
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_data-col_02 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 5 .                                            " 3RD KEY
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_data-col_03 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 6 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_data-col_04 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 7 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_data-col_05 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 8 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_data-col_06 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 9 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_data-col_07 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 10 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_data-col_08 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 11 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_data-col_09 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 12 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_data-col_10 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 13 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_data-col_11 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 14 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_data-col_12 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 15 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_data-col_13 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 16 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_data-col_14 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 17 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_data-col_15 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 18 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_data-col_16 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 19 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_data-col_17 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 20 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_data-col_18 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 21 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_data-col_19 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 22 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_data-col_20 TO it_values_c_new-valc.
          APPEND it_values_c_new.

      ENDCASE.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " MAKE_NEW_DATA
*&---------------------------------------------------------------------*
*&      Form  call_function_conversion
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_COLUMN_CHARACT  text
*      -->P_IT_VALUES_N_NEW_ATINN  text
*----------------------------------------------------------------------*
FORM call_function_conversion USING    p_char_c
                                       p_numb_n.
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            input  = p_char_c
       IMPORTING
            output = p_numb_n.

ENDFORM.                    " call_function_conversion
*&---------------------------------------------------------------------*
*&      Form  read_variant_table_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_REC_TABLE  text
*----------------------------------------------------------------------*
FORM read_variant_table_header USING    p_full_code.
  CLEAR: it_column, it_column[].
  CALL FUNCTION 'CARD_TABLE_READ_STRUCTURE'
    EXPORTING
      var_tab                    = p_full_code
*   CHANGE_NO                  =
*   DATE                       =
      language                   = sy-langu
* IMPORTING
*   BASIC_DATA                 =
*   RETURN                     =
    TABLES
      descriptions               = wa_descriptions
      characteristics            = it_column
*   VALUE_ASSIGNMENT_ALT       =
    EXCEPTIONS
      error                      = 1
      OTHERS                     = 2 .
ENDFORM.                    " read_variant_table_header
*&---------------------------------------------------------------------*
*&      Form  DELETE_VARIANT_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DATA_TABLE  text
*----------------------------------------------------------------------*
FORM delete_variant_table USING    p_table.
  perform progress_indicator using 50 p_table.
  WRITE: / 'It is the time to proccess table ', p_table, '.'.
  DATA: l_var_tab_entries LIKE vtentries OCCURS 0.
  CALL FUNCTION 'CAMA_TABLE_MAINTAIN_ENTRIES'
    EXPORTING
      var_table             = p_table
     fldelete              = 'X'
*   CHANGE_NO             =
*   DATE                  =
    TABLES
      var_tab_entries       = l_var_tab_entries
* EXCEPTIONS
*   ERROR                 = 1
*   OTHERS                = 2
            .
  IF sy-subrc <> 0.
    PERFORM progress_indicator USING
              50 'There is ERROR during deleting data...'.
    WRITE: / p_table, ' : THERE IS ERROR DURING DELETING DATA.'.
  ELSE.
    PERFORM progress_indicator USING
              50 'The entries were deleted successfully.'.
    WRITE: / p_table,
           ' : The whole entries were deleted successfully. '.
  ENDIF.

ENDFORM.                    " DELETE_VARIANT_TABLE
*&---------------------------------------------------------------------*
*&      Form  bdc_upload_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_upload_data.
  CALL FUNCTION 'UPLOAD'
       EXPORTING
            codepage                = 'DAT'
            filename                = wa_filename
            filetype                = wa_filetype
       TABLES
            data_tab                = it_data
       EXCEPTIONS
            conversion_error        = 1
            invalid_table_width     = 2
            invalid_type            = 3
            no_batch                = 4
            unknown_error           = 5
            gui_refuse_filetransfer = 6
            OTHERS                  = 7.

  IF sy-subrc <> 0.
    WRITE: /, ' Error Opening File: ', wa_filename,
          /, ' Return Code: ', sy-subrc.
    EXIT.
  ENDIF.
ENDFORM.                    " BDC_UPLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  CREATE_NEW_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_new_data.
  DATA: l_line  TYPE vtentries-vtlineno,
        l_slnid TYPE cuvtln-slnid,
        l_count TYPE i,
        l_tabix LIKE sy-tabix.
  DATA: l_flag.

  SORT it_data BY table
                  col_01 col_02 col_03 col_04 col_05
                  col_06 col_07 col_08 col_09 col_10
                  col_11 col_12 col_13 col_14 col_15
                  col_16 col_17 col_18 col_19 col_20.

  LOOP AT it_data.
    l_tabix = sy-tabix.
    AT NEW table.
      PERFORM reset_internal_tables.
      PERFORM delete_variant_table      USING it_data-table.
      PERFORM read_variant_table_header USING it_data-table.
      PERFORM read_cuvtab USING it_data-table.  " Variant
      READ TABLE it_table_header INDEX 1.
    ENDAT.
    CLEAR l_flag.
    IF l_tabix <> 1.
      PERFORM check_unique_code USING l_tabix
                                      l_flag.
    ENDIF.
    IF l_flag = 'X'.
      PERFORM write_error_code.
      CONTINUE.
    ENDIF.
    l_line  = l_line  + 1.
    l_slnid = l_slnid + 1.
    l_count = l_count + 1.
    PERFORM make_new_line USING it_table_header-vtint
                                l_line
                                l_slnid .
    PERFORM make_new_data TABLES it_data
                          USING  it_table_header-vtint
                                 l_slnid .
    AT END OF table.
      PERFORM call_function USING it_data-table
                                  l_count.
      CLEAR: l_line, l_slnid, l_count.
    ENDAT.

  ENDLOOP.

ENDFORM.                    " CREATE_NEW_DATA
*&---------------------------------------------------------------------*
*&      Form  check_unique_code
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_FLAG  text
*----------------------------------------------------------------------*
FORM check_unique_code USING p_tabix
                             p_flag.
  DATA: l_current_data LIKE LINE OF it_data.
  DATA: l_before_data LIKE LINE OF it_data.
  CLEAR: l_current_data, l_before_data.
  p_tabix = p_tabix - 1.
  READ TABLE it_data INDEX p_tabix.
  MOVE-CORRESPONDING it_data TO l_before_data.   "Before Row
  p_tabix = p_tabix + 1.
  READ TABLE it_data INDEX p_tabix.
  MOVE-CORRESPONDING it_data TO l_current_data.  "Current Row
  IF l_current_data-table <> l_before_data-table.
    EXIT.
  ENDIF.
* check codes from 01 to 20.
  IF l_current_data-col_01 <> space.
    IF l_current_data-col_01 = l_before_data-col_01.
      p_flag = 'X'.
    ELSE.
      CLEAR p_flag.
      EXIT.
    ENDIF.
  ENDIF.
  IF l_current_data-col_02 <> space.
    IF l_current_data-col_02 = l_before_data-col_02.
      p_flag = 'X'.
    ELSE.
      CLEAR p_flag.
      EXIT.
    ENDIF.
  ENDIF.
  IF l_current_data-col_03 <> space.
    IF l_current_data-col_03 = l_before_data-col_03.
      p_flag = 'X'.
    ELSE.
      CLEAR p_flag.
      EXIT.
    ENDIF.
  ENDIF.
  IF l_current_data-col_04 <> space.
    IF l_current_data-col_04 = l_before_data-col_04.
      p_flag = 'X'.
    ELSE.
      CLEAR p_flag.
      EXIT.
    ENDIF.
  ENDIF.
  IF l_current_data-col_05 <> space.
    IF l_current_data-col_05 = l_before_data-col_05.
      p_flag = 'X'.
    ELSE.
      CLEAR p_flag.
      EXIT.
    ENDIF.
  ENDIF.
  IF l_current_data-col_06 <> space.
    IF l_current_data-col_06 = l_before_data-col_06.
      p_flag = 'X'.
    ELSE.
      CLEAR p_flag.
      EXIT.
    ENDIF.
  ENDIF.
  IF l_current_data-col_07 <> space.
    IF l_current_data-col_07 = l_before_data-col_07.
      p_flag = 'X'.
    ELSE.
      CLEAR p_flag.
      EXIT.
    ENDIF.
  ENDIF.
  IF l_current_data-col_08 <> space.
    IF l_current_data-col_08 = l_before_data-col_08.
      p_flag = 'X'.
    ELSE.
      CLEAR p_flag.
      EXIT.
    ENDIF.
  ENDIF.
  IF l_current_data-col_09 <> space.
    IF l_current_data-col_09 = l_before_data-col_09.
      p_flag = 'X'.
    ELSE.
      CLEAR p_flag.
      EXIT.
    ENDIF.
  ENDIF.
  IF l_current_data-col_10 <> space.
    IF l_current_data-col_10 = l_before_data-col_10.
      p_flag = 'X'.
    ELSE.
      CLEAR p_flag.
      EXIT.
    ENDIF.
  ENDIF.
  IF l_current_data-col_11 <> space.
    IF l_current_data-col_11 = l_before_data-col_11.
      p_flag = 'X'.
    ELSE.
      CLEAR p_flag.
      EXIT.
    ENDIF.
  ENDIF.
  IF l_current_data-col_12 <> space.
    IF l_current_data-col_12 = l_before_data-col_12.
      p_flag = 'X'.
    ELSE.
      CLEAR p_flag.
      EXIT.
    ENDIF.
  ENDIF.
  IF l_current_data-col_13 <> space.
    IF l_current_data-col_13 = l_before_data-col_13.
      p_flag = 'X'.
    ELSE.
      CLEAR p_flag.
      EXIT.
    ENDIF.
  ENDIF.
  IF l_current_data-col_14 <> space.
    IF l_current_data-col_14 = l_before_data-col_14.
      p_flag = 'X'.
    ELSE.
      CLEAR p_flag.
      EXIT.
    ENDIF.
  ENDIF.
  IF l_current_data-col_15 <> space.
    IF l_current_data-col_15 = l_before_data-col_15.
      p_flag = 'X'.
    ELSE.
      CLEAR p_flag.
      EXIT.
    ENDIF.
  ENDIF.
  IF l_current_data-col_16 <> space.
    IF l_current_data-col_16 = l_before_data-col_16.
      p_flag = 'X'.
    ELSE.
      CLEAR p_flag.
      EXIT.
    ENDIF.
  ENDIF.
  IF l_current_data-col_17 <> space.
    IF l_current_data-col_17 = l_before_data-col_17.
      p_flag = 'X'.
    ELSE.
      CLEAR p_flag.
      EXIT.
    ENDIF.
  ENDIF.
  IF l_current_data-col_18 <> space.
    IF l_current_data-col_18 = l_before_data-col_18.
      p_flag = 'X'.
    ELSE.
      CLEAR p_flag.
      EXIT.
    ENDIF.
  ENDIF.
  IF l_current_data-col_19 <> space.
    IF l_current_data-col_19 = l_before_data-col_19.
      p_flag = 'X'.
    ELSE.
      CLEAR p_flag.
      EXIT.
    ENDIF.
  ENDIF.
  IF l_current_data-col_20 <> space.
    IF l_current_data-col_20 = l_before_data-col_20.
      p_flag = 'X'.
    ELSE.
      CLEAR p_flag.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.                    " check_unique_code
*&---------------------------------------------------------------------*
*&      Form  write_error_code
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_error_code.
  PERFORM progress_indicator USING
                        50 'There is a unique code error...'.
  WRITE:/ 'Table : ',
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

ENDFORM.                    " write_error_code
*&---------------------------------------------------------------------*
*&      Form  make_display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_display_data.
  DATA flag.
  LOOP AT it_data.
    AT NEW table.
      IF flag <> 'X'.
        FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
        flag = 'X'.
      ELSE.
        FORMAT COLOR COL_POSITIVE INTENSIFIED ON.
        flag = ' '.
      ENDIF.
      PERFORM setting_column_name.
    ENDAT.
    IF flag <> 'X'.
      FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
      flag = 'X'.
    ELSE.
      FORMAT COLOR COL_POSITIVE INTENSIFIED ON.
      flag = ' '.
    ENDIF.
    PERFORM write_body .
    AT END OF table.
      WRITE: /(159) sy-uline.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " make_display_data
*&---------------------------------------------------------------------*
*&      Form  PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*       Progress Indicator.
*----------------------------------------------------------------------*
*  -->  PA_SZBAR  Size of Bar.
*  <--  PA_DISPL  Text to be displayed.
*----------------------------------------------------------------------*
FORM progress_indicator USING pa_szbar
                              pa_displ.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = pa_szbar
            text       = pa_displ.
ENDFORM.                               " PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*&      Form  SETTING_COLUMN_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_COUNT  text
*----------------------------------------------------------------------*
FORM setting_column_name.
  DATA: l_count TYPE i.
  PERFORM read_variant_table_header USING it_data-table.
  DESCRIBE TABLE it_column LINES l_count.
  WRITE:/ '|' NO-GAP,
          (20) it_data-table NO-GAP, '|' NO-GAP,
          (05) '*****' NO-GAP, '|' NO-GAP,
          (10) '**********' NO-GAP, '|' NO-GAP.
  IF l_count > 2.  "FIRST KEY
    READ TABLE it_column INDEX 3.
    SEARCH it_column-charact FOR '219'.
    IF sy-subrc = 0.
      WRITE: (05) it_column-charact+06 NO-GAP,
             '|' NO-GAP.
    ELSE.
      WRITE: (05) it_column-charact+02 NO-GAP,
             '|' NO-GAP.
    ENDIF.
  ENDIF.
  IF l_count > 3.  "SECOND KEY
    READ TABLE it_column  INDEX 4.
    SEARCH it_column-charact FOR '219'.
    IF sy-subrc = 0.
      WRITE: (05) it_column-charact+06 NO-GAP,
             '|' NO-GAP.
    ELSE.
      WRITE: (05) it_column-charact+02 NO-GAP,
             '|' NO-GAP.
    ENDIF.
  ENDIF.
  IF l_count > 4.  "THIRD KEY
    READ TABLE it_column  INDEX 5.
    SEARCH it_column-charact FOR '219'.
    IF sy-subrc = 0.
      WRITE: (05) it_column-charact+06 NO-GAP,
             '|' NO-GAP.
    ELSE.
      WRITE: (05) it_column-charact+02 NO-GAP,
             '|' NO-GAP.
    ENDIF.
  ENDIF.
  IF l_count > 5.
    READ TABLE  it_column INDEX 6.
    SEARCH it_column-charact FOR '219'.
    IF sy-subrc = 0.
      WRITE: (05) it_column-charact+06 NO-GAP,
             '|' NO-GAP.
    ELSE.
      WRITE: (05) it_column-charact+02 NO-GAP,
             '|' NO-GAP.
    ENDIF.
  ENDIF.
  IF l_count > 6.
    READ TABLE it_column  INDEX 7.
    SEARCH it_column-charact FOR '219'.
    IF sy-subrc = 0.
      WRITE: (05) it_column-charact+06 NO-GAP,
             '|' NO-GAP.
    ELSE.
      WRITE: (05) it_column-charact+02 NO-GAP,
             '|' NO-GAP.
    ENDIF.
  ENDIF.
  IF l_count > 7.
    READ TABLE it_column  INDEX 8.
    SEARCH it_column-charact FOR '219'.
    IF sy-subrc = 0.
      WRITE: (05) it_column-charact+06 NO-GAP,
             '|' NO-GAP.
    ELSE.
      WRITE: (05) it_column-charact+02 NO-GAP,
             '|' NO-GAP.
    ENDIF.
  ENDIF.
  IF l_count > 8.
    READ TABLE  it_column INDEX 9.
    SEARCH it_column-charact FOR '219'.
    IF sy-subrc = 0.
      WRITE: (05) it_column-charact+06 NO-GAP,
             '|' NO-GAP.
    ELSE.
      WRITE: (05) it_column-charact+02 NO-GAP,
             '|' NO-GAP.
    ENDIF.

  ENDIF.
  IF l_count > 9.
    READ TABLE  it_column INDEX 10.
    SEARCH it_column-charact FOR '219'.
    IF sy-subrc = 0.
      WRITE: (05) it_column-charact+06 NO-GAP,
             '|' NO-GAP.
    ELSE.
      WRITE: (05) it_column-charact+02 NO-GAP,
             '|' NO-GAP.
    ENDIF.
  ENDIF.
  IF l_count > 10.
    READ TABLE  it_column INDEX 11.
    SEARCH it_column-charact FOR '219'.
    IF sy-subrc = 0.
      WRITE: (05) it_column-charact+06 NO-GAP,
             '|' NO-GAP.
    ELSE.
      WRITE: (05) it_column-charact+02 NO-GAP,
             '|' NO-GAP.
    ENDIF.
  ENDIF.
  IF l_count > 11.
    READ TABLE  it_column INDEX 12.
    SEARCH it_column-charact FOR '219'.
    IF sy-subrc = 0.
      WRITE: (05) it_column-charact+06 NO-GAP,
             '|' NO-GAP.
    ELSE.
      WRITE: (05) it_column-charact+02 NO-GAP,
             '|' NO-GAP.
    ENDIF.
  ENDIF.
  IF l_count > 12.
    READ TABLE it_column  INDEX 13.
    SEARCH it_column-charact FOR '219'.
    IF sy-subrc = 0.
      WRITE: (05) it_column-charact+06 NO-GAP,
             '|' NO-GAP.
    ELSE.
      WRITE: (05) it_column-charact+02 NO-GAP,
             '|' NO-GAP.
    ENDIF.
  ENDIF.
  IF l_count > 13.
    READ TABLE  it_column INDEX 14.
    SEARCH it_column-charact FOR '219'.
    IF sy-subrc = 0.
      WRITE: (05) it_column-charact+06 NO-GAP,
             '|' NO-GAP.
    ELSE.
      WRITE: (05) it_column-charact+02 NO-GAP,
             '|' NO-GAP.
    ENDIF.
  ENDIF.
  IF l_count > 14.
    READ TABLE  it_column INDEX 15.
    SEARCH it_column-charact FOR '219'.
    IF sy-subrc = 0.
      WRITE: (05) it_column-charact+06 NO-GAP,
             '|' NO-GAP.
    ELSE.
      WRITE: (05) it_column-charact+02 NO-GAP,
             '|' NO-GAP.
    ENDIF.
  ENDIF.
  IF l_count > 15.
    READ TABLE  it_column INDEX 16.
    SEARCH it_column-charact FOR '219'.
    IF sy-subrc = 0.
      WRITE: (05) it_column-charact+06 NO-GAP,
             '|' NO-GAP.
    ELSE.
      WRITE: (05) it_column-charact+02 NO-GAP,
             '|' NO-GAP.
    ENDIF.
  ENDIF.
  IF l_count > 16.
    READ TABLE  it_column INDEX 17.
    SEARCH it_column-charact FOR '219'.
    IF sy-subrc = 0.
      WRITE: (05) it_column-charact+06 NO-GAP,
             '|' NO-GAP.
    ELSE.
      WRITE: (05) it_column-charact+02 NO-GAP,
             '|' NO-GAP.
    ENDIF.
  ENDIF.
  IF l_count > 17.
    READ TABLE  it_column INDEX 18.
    SEARCH it_column-charact FOR '219'.
    IF sy-subrc = 0.
      WRITE: (05) it_column-charact+06 NO-GAP,
             '|' NO-GAP.
    ELSE.
      WRITE: (05) it_column-charact+02 NO-GAP,
             '|' NO-GAP.
    ENDIF.
  ENDIF.
  IF l_count > 18.
    READ TABLE it_column  INDEX 19.
    SEARCH it_column-charact FOR '219'.
    IF sy-subrc = 0.
      WRITE: (05) it_column-charact+06 NO-GAP,
             '|' NO-GAP.
    ELSE.
      WRITE: (05) it_column-charact+02 NO-GAP,
             '|' NO-GAP.
    ENDIF.
  ENDIF.
  IF l_count > 19.
    READ TABLE  it_column INDEX 20.
    SEARCH it_column-charact FOR '219'.
    IF sy-subrc = 0.
      WRITE: (05) it_column-charact+06 NO-GAP,
             '|' NO-GAP.
    ELSE.
      WRITE: (05) it_column-charact+02 NO-GAP,
             '|' NO-GAP.
    ENDIF.
  ENDIF.
  IF l_count > 20.
    READ TABLE  it_column INDEX 21.
    SEARCH it_column-charact FOR '219'.
    IF sy-subrc = 0.
      WRITE: (05) it_column-charact+06 NO-GAP,
             '|' NO-GAP.
    ELSE.
      WRITE: (05) it_column-charact+02 NO-GAP,
             '|' NO-GAP.
    ENDIF.
  ENDIF.
  IF l_count > 21.
    READ TABLE  it_column INDEX 22.
    SEARCH it_column-charact FOR '219'.
    IF sy-subrc = 0.
      WRITE: (05) it_column-charact+06 NO-GAP,
             '|' NO-GAP.
    ELSE.
      WRITE: (05) it_column-charact+02 NO-GAP,
             '|' NO-GAP.
    ENDIF.
  ENDIF.
  IF l_count > 22.
    READ TABLE  it_column INDEX 23.
    SEARCH it_column-charact FOR '219'.
    IF sy-subrc = 0.
      WRITE: (05) it_column-charact+06 NO-GAP,
             '|' NO-GAP.
    ELSE.
      WRITE: (05) it_column-charact+02 NO-GAP,
             '|' NO-GAP.
    ENDIF.
  ENDIF.
  IF l_count > 23.
    READ TABLE  it_column INDEX 24.
    SEARCH it_column-charact FOR '219'.
    IF sy-subrc = 0.
      WRITE: (05) it_column-charact+06 NO-GAP,
             '|' NO-GAP.
    ELSE.
      WRITE: (05) it_column-charact+02 NO-GAP,
             '|' NO-GAP.
    ENDIF.
  ENDIF.
  WRITE: 159 '|' NO-GAP.
  WRITE: /(159) sy-uline NO-GAP.
ENDFORM.                    " SETTING_COLUMN_NAME
*&---------------------------------------------------------------------*
*&      Form  WRITE_BODY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_body.
  WRITE: / '|' NO-GAP,
           (20) ' ' NO-GAP,
           '|' NO-GAP,
           (05) it_data-code NO-GAP,
           '|' NO-GAP,
           (10) it_data-date NO-GAP,
           '|' NO-GAP,
           (05) it_data-col_01 NO-GAP,
           '|' NO-GAP,
           (05) it_data-col_02 NO-GAP,
           '|' NO-GAP,
           (05) it_data-col_03 NO-GAP,
           '|' NO-GAP,
           (05) it_data-col_04 NO-GAP,
           '|' NO-GAP,
           (05) it_data-col_05 NO-GAP,
           '|' NO-GAP,
           (05) it_data-col_06 NO-GAP,
           '|' NO-GAP,
           (05) it_data-col_07 NO-GAP,
           '|' NO-GAP,
           (05) it_data-col_08 NO-GAP,
           '|' NO-GAP,
           (05) it_data-col_09 NO-GAP,
           '|' NO-GAP,
           (05) it_data-col_10 NO-GAP,
           '|' NO-GAP,
           (05) it_data-col_11 NO-GAP,
           '|' NO-GAP,
           (05) it_data-col_12 NO-GAP,
           '|' NO-GAP,
           (05) it_data-col_13 NO-GAP,
           '|' NO-GAP,
           (05) it_data-col_14 NO-GAP,
           '|' NO-GAP,
           (05) it_data-col_15 NO-GAP,
           '|' NO-GAP,
           (05) it_data-col_16 NO-GAP,
           '|' NO-GAP,
           (05) it_data-col_17 NO-GAP,
           '|' NO-GAP,
           (05) it_data-col_18 NO-GAP,
           '|' NO-GAP,
           (05) it_data-col_19 NO-GAP,
           '|' NO-GAP,
           (05) it_data-col_20 NO-GAP,
           '|' NO-GAP.

ENDFORM.                    " WRITE_BODY

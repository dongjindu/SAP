*----------------------------------------------------------------------
* Program ID        : ZCOE_TBL_ZTCO_MAT_MNT
* Title             : Program to maintain table ZTCO_MAT
* Created on        : 07/15/2011
* Created by        : Valerian Utama
* Specifications By : Michael Yoon
* Description       : The program will be used to maintain table
*                     ZTCO_MAT. This is required because of the limited
*                     functionalities of Tcode SM30.
*
*----------------------------------------------------------------------
*  Modification Log
*  Date        Developer Issue No    Description
*======================================================================
*  07/15/2011  Valerian  UD1K952463  Intial Program Development
*  07/26/2011  Valerian  UD1K952584  Validate Material Number before
*                                    saving the data
*  08/22/2011  Valerian  UD1K952799  Add special selection logic for
*                                    material start with '0'.
*  10/07/2011  Valerian  UD1K953131  Fix creating new entries logic
*                                    Use correct conversion exit for
*                                    field 'MATNR'
*  02/25/2013  Valerian  UD1K956523  Add additional field "Responsible
*                                    Team" in table ZTCO_MAT
*----------------------------------------------------------------------

REPORT ztable_maintenance .
TABLES: ztco_mat.

SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_matnr FOR ztco_mat-matnr,
                s_mtart FOR ztco_mat-mtart,
                s_matkl FOR ztco_mat-matkl.
SELECTION-SCREEN END OF BLOCK blk.

LOOP AT s_matnr.
  PERFORM conv_matnr CHANGING s_matnr-low.
  PERFORM conv_matnr CHANGING s_matnr-high.
  MODIFY s_matnr.
ENDLOOP.

CALL SCREEN 100.

***&spwizard: data declaration for tablecontrol 'TC1'
*&spwizard: definition of ddic-table

*&spwizard: type for the data of tablecontrol 'TC1'
TYPES: BEGIN OF t_tc1,
         matnr LIKE ztco_mat-matnr,
         maktx LIKE ztco_mat-maktx,
         mtart LIKE ztco_mat-mtart,
         matkl LIKE ztco_mat-matkl,
         zgrp1 LIKE ztco_mat-zgrp1,
         zgrp2 LIKE ztco_mat-zgrp2,
         zgrp3 LIKE ztco_mat-zgrp3,
         zgrp4 LIKE ztco_mat-zgrp4,
         zgrp5 LIKE ztco_mat-zgrp5,
         zgrp6 LIKE ztco_mat-zgrp6,
         zgrp7 LIKE ztco_mat-zgrp7,
         zgrp8 LIKE ztco_mat-zgrp8,
         zgrp9 LIKE ztco_mat-zgrp9,
         zgrp10 LIKE ztco_mat-zgrp10,
         zresp LIKE ztco_mat-zresp,                 "UD1K956523
         zch_date LIKE ztco_mat-zch_Date,
         zch_time LIKE ztco_mat-zch_time,
         zch_user LIKE ztco_mat-zch_user,
         flag,       "flag for mark column
       END OF t_tc1.

*&spwizard: internal table for tablecontrol 'TC1'
DATA:     g_tc1_itab   TYPE t_tc1 OCCURS 0,
          g_tc1_wa     TYPE t_tc1. "work area
DATA:     g_tc1_copied.           "copy flag

DATA:     gt_delrec    TYPE t_tc1 OCCURS 0 WITH HEADER LINE, "UD1K952463
          gt_orgrec    TYPE t_tc1 OCCURS 0,                 "UD1K952463
          gt_tmprec    TYPE t_tc1 OCCURS 0,                 "UD1K952463
          g_rec        TYPE i.                              "UD1K952463

* BEGIN OF UD1K952463
DATA: tab LIKE sval OCCURS 0 WITH HEADER LINE,
      g_matnr TYPE matnr,
      g_error(1) TYPE c,
      g_fieldname(30) TYPE c,
      g_tabix TYPE sy-tabix.

FIELD-SYMBOLS: <fs> TYPE any.
* END OF UD1K952463

*&spwizard: declaration of tablecontrol 'TC1' itself
CONTROLS: tc1 TYPE TABLEVIEW USING SCREEN 0100.

*&spwizard: lines of tablecontrol 'TC1'
DATA:     g_tc1_lines  LIKE sy-loopc.

DATA:     ok_code LIKE sy-ucomm.

*&spwizard: output module for tc 'TC1'. do not change this line!
*&spwizard: copy ddic-table to itab
MODULE tc1_init OUTPUT.
  IF g_tc1_copied IS INITIAL.
*&spwizard: copy ddic-table 'ZTCO_MAT'
*&spwizard: into internal table 'g_TC1_itab'
    SELECT * FROM ztco_mat
       INTO CORRESPONDING FIELDS
       OF TABLE g_tc1_itab
       WHERE matnr IN s_matnr
         AND mtart IN s_mtart
         AND matkl IN s_matkl.

    SORT g_tc1_itab.                                        "UD1K952463
    gt_orgrec[] = g_tc1_itab[].                             "UD1K952463
    DESCRIBE TABLE g_tc1_itab LINES g_rec.                  "UD1K952463
    MESSAGE s000(zmco) WITH g_rec 'record(s) selected'.     "UD1K952463
    g_tc1_copied = 'X'.
    REFRESH CONTROL 'TC1' FROM SCREEN '0100'.
  ENDIF.
ENDMODULE.                    "tc1_init OUTPUT

*&spwizard: output module for tc 'TC1'. do not change this line!
*&spwizard: move itab to dynpro
MODULE tc1_move OUTPUT.
  MOVE-CORRESPONDING g_tc1_wa TO ztco_mat.
ENDMODULE.                    "tc1_move OUTPUT

*&spwizard: output module for tc 'TC1'. do not change this line!
*&spwizard: get lines of tablecontrol
MODULE tc1_get_lines OUTPUT.
  g_tc1_lines = sy-loopc.
ENDMODULE.                    "tc1_get_lines OUTPUT

*&spwizard: input module for tc 'TC1'. do not change this line!
*&spwizard: modify table
MODULE tc1_modify INPUT.
  MOVE-CORRESPONDING ztco_mat TO g_tc1_wa.
  IF g_tc1_itab[] IS INITIAL.                               "UD1K953131
    APPEND g_tc1_wa TO g_tc1_itab.                          "UD1K953131
  ELSE.                                                     "UD1K953131
    MODIFY g_tc1_itab
      FROM g_tc1_wa
      INDEX tc1-current_line.
  ENDIF.                                                    "UD1K953131
ENDMODULE.                    "tc1_modify INPUT

*&spwizard: input module for tc 'TC1'. do not change this line!
*&spwizard: mark table
MODULE tc1_mark INPUT.
  IF tc1-line_sel_mode = 1 AND
     g_tc1_wa-flag = 'X'.
    LOOP AT g_tc1_itab INTO g_tc1_wa
      WHERE flag = 'X'.
      g_tc1_wa-flag = ''.
      MODIFY g_tc1_itab
        FROM g_tc1_wa
        TRANSPORTING flag.
    ENDLOOP.
    g_tc1_wa-flag = 'X'.
  ENDIF.
  MODIFY g_tc1_itab
    FROM g_tc1_wa
    INDEX tc1-current_line
    TRANSPORTING flag.
ENDMODULE.                    "tc1_mark INPUT

*&spwizard: input module for tc 'TC1'. do not change this line!
*&spwizard: process user command
MODULE tc1_user_command INPUT.
  ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'TC1'
                              'G_TC1_ITAB'
                              'FLAG'
                     CHANGING ok_code.
  sy-ucomm = ok_code.
ENDMODULE.                    "tc1_user_command INPUT

*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
FORM user_ok_tc USING    p_tc_name TYPE dynfnam
                         p_table_name
                         p_mark_name
                CHANGING p_ok      LIKE sy-ucomm.

*-BEGIN OF LOCAL DATA--------------------------------------------------*
  DATA: l_ok              TYPE sy-ucomm,
        l_offset          TYPE i.
*-END OF LOCAL DATA----------------------------------------------------*

* Table control specific operations                                    *
*   evaluate TC name and operations                                    *
  SEARCH p_ok FOR p_tc_name.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.
  l_offset = strlen( p_tc_name ) + 1.
  l_ok = p_ok+l_offset.
* execute general and TC specific operations                           *
  CASE l_ok.
    WHEN 'INSR'.                      "insert row
      PERFORM fcode_insert_row USING    p_tc_name
                                        p_table_name.
      CLEAR p_ok.

    WHEN 'DELE'.                      "delete row
      PERFORM fcode_delete_row USING    p_tc_name
                                        p_table_name
                                        p_mark_name.
      CLEAR p_ok.

    WHEN 'P--' OR                     "top of list
         'P-'  OR                     "previous page
         'P+'  OR                     "next page
         'P++'.                       "bottom of list
      PERFORM compute_scrolling_in_tc USING p_tc_name
                                            l_ok.
      CLEAR p_ok.

    WHEN 'MARK'.                      "mark all filled lines
      PERFORM fcode_tc_mark_lines USING p_tc_name
                                        p_table_name
                                        p_mark_name   .
      CLEAR p_ok.

    WHEN 'DMRK'.                      "demark all filled lines
      PERFORM fcode_tc_demark_lines USING p_tc_name
                                          p_table_name
                                          p_mark_name .
      CLEAR p_ok.

  ENDCASE.

ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_insert_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name             .

*-BEGIN OF LOCAL DATA--------------------------------------------------*
  DATA l_lines_name       LIKE feld-name.
  DATA l_selline          LIKE sy-stepl.
  DATA l_lastline         TYPE i.
  DATA l_line             TYPE i.
  DATA l_table_name       LIKE feld-name.
  FIELD-SYMBOLS <tc>                 TYPE cxtab_control.
  FIELD-SYMBOLS <table>              TYPE STANDARD TABLE.
  FIELD-SYMBOLS <lines>              TYPE i.
*-END OF LOCAL DATA----------------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

* get the table, which belongs to the tc                               *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

* get looplines of TableControl
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_lines_name.
  ASSIGN (l_lines_name) TO <lines>.

* get current line
  GET CURSOR LINE l_selline.
  IF sy-subrc <> 0.                   " append line to table
    l_selline = <tc>-lines + 1.
*&SPWIZARD: set top line and new cursor line                           *
    IF l_selline > <lines>.
      <tc>-top_line = l_selline - <lines> + 1 .
    ELSE.
      <tc>-top_line = 1.
    ENDIF.
  ELSE.                               " insert line into table
    l_selline = <tc>-top_line + l_selline - 1.
    l_lastline = <tc>-top_line + <lines> - 1.
  ENDIF.
*&SPWIZARD: set new cursor line                                        *
  l_line = l_selline - <tc>-top_line + 1.
* insert initial line
  INSERT INITIAL LINE INTO <table> INDEX l_selline.
  <tc>-lines = <tc>-lines + 1.
* set cursor
  SET CURSOR LINE l_line.

ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_delete_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name
                       p_mark_name   .

*-BEGIN OF LOCAL DATA--------------------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*-END OF LOCAL DATA----------------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

* get the table, which belongs to the tc                               *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

* delete marked lines                                                  *
  DESCRIBE TABLE <table> LINES <tc>-lines.

  LOOP AT <table> ASSIGNING <wa>.

*   access to the component 'FLAG' of the table header                 *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    IF <mark_field> = 'X'.
      READ TABLE <table> INDEX syst-tabix INTO gt_delrec.   "UD1K952463
      DELETE <table> INDEX syst-tabix.
      IF sy-subrc = 0.
        <tc>-lines = <tc>-lines - 1.
        APPEND gt_delrec.                                   "UD1K952463
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
FORM compute_scrolling_in_tc USING    p_tc_name
                                      p_ok.
*-BEGIN OF LOCAL DATA--------------------------------------------------*
  DATA l_tc_new_top_line     TYPE i.
  DATA l_tc_name             LIKE feld-name.
  DATA l_tc_lines_name       LIKE feld-name.
  DATA l_tc_field_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <lines>      TYPE i.
*-END OF LOCAL DATA----------------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.
* get looplines of TableControl
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_tc_lines_name.
  ASSIGN (l_tc_lines_name) TO <lines>.


* is no line filled?                                                   *
  IF <tc>-lines = 0.
*   yes, ...                                                           *
    l_tc_new_top_line = 1.
  ELSE.
*   no, ...                                                            *
    CALL FUNCTION 'SCROLLING_IN_TABLE'
      EXPORTING
        entry_act             = <tc>-top_line
        entry_from            = 1
        entry_to              = <tc>-lines
        last_page_full        = 'X'
        loops                 = <lines>
        ok_code               = p_ok
        overlapping           = 'X'
      IMPORTING
        entry_new             = l_tc_new_top_line
      EXCEPTIONS
*       NO_ENTRY_OR_PAGE_ACT  = 01
*       NO_ENTRY_TO           = 02
*       NO_OK_CODE_OR_PAGE_GO = 03
        OTHERS                = 0.
  ENDIF.

* get actual tc and column                                             *
  GET CURSOR FIELD l_tc_field_name
             AREA  l_tc_name.

  IF syst-subrc = 0.
    IF l_tc_name = p_tc_name.
*     set actual column                                                *
      SET CURSOR FIELD l_tc_field_name LINE 1.
    ENDIF.
  ENDIF.

* set the new top line                                                 *
  <tc>-top_line = l_tc_new_top_line.


ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_mark_lines USING p_tc_name
                               p_table_name
                               p_mark_name.
*-BEGIN OF LOCAL DATA--------------------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*-END OF LOCAL DATA----------------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

* get the table, which belongs to the tc                               *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

* mark all filled lines                                                *
  LOOP AT <table> ASSIGNING <wa>.

*   access to the component 'FLAG' of the table header                 *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = 'X'.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_demark_lines USING p_tc_name
                                 p_table_name
                                 p_mark_name .
*-BEGIN OF LOCAL DATA--------------------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*-END OF LOCAL DATA----------------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

* get the table, which belongs to the tc                               *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

* demark all filled lines                                              *
  LOOP AT <table> ASSIGNING <wa>.

*   access to the component 'FLAG' of the table header                 *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = space.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

* BEGIN OF UD1K952463
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'S100'.
  SET TITLEBAR 'T100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MENU_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE menu_exit INPUT.
  DATA: l_ans(1) TYPE c.

  LOOP AT g_tc1_itab INTO g_tc1_wa.
    CLEAR g_tc1_wa-flag.
    APPEND g_tc1_wa TO gt_tmprec.
  ENDLOOP.

  IF gt_orgrec[] <> gt_tmprec[].
    CLEAR: gt_tmprec, gt_tmprec[].
    CALL FUNCTION 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
      EXPORTING
        textline1     = 'Do you want to exit?'
*       TEXTLINE2     = ' '
        titel         = 'Confirm Exit'
*       START_COLUMN  = 25
*       START_ROW     = 6
*       DEFAULTOPTION = 'N'
      IMPORTING
        answer        = l_ans.
    CHECK l_ans = 'J'.
  ENDIF.
  SET SCREEN 0.
ENDMODULE.                 " MENU_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Form  CONV_MATNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_S_MATNR_LOW  text
*----------------------------------------------------------------------*
FORM conv_matnr CHANGING p_matnr.
  CHECK p_matnr(1) <> '0'.                                  "UD1K952799
* CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'               "UD1K953131
  CALL FUNCTION 'CONVERSION_EXIT_MATN2_INPUT'               "UD1K953131
       EXPORTING
            input  = p_matnr
       IMPORTING
            output = p_matnr.

ENDFORM.                    " CONV_MATNR
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA: lw_deleted LIKE ztco_mat_deleted.
  CASE sy-ucomm.
    WHEN 'SAVE'.
      SORT g_tc1_itab BY matnr.
      DELETE ADJACENT DUPLICATES FROM g_tc1_itab COMPARING matnr.
      DELETE g_tc1_itab WHERE matnr = space.
      DESCRIBE TABLE g_tc1_itab LINES tc1-lines.

      LOOP AT g_tc1_itab INTO g_tc1_wa.
        CLEAR g_tc1_wa-flag.
        APPEND g_tc1_wa TO gt_tmprec.
      ENDLOOP.

      IF gt_orgrec[] = gt_tmprec[].
        MESSAGE i208(00) WITH 'Data Not Changed - Saving Not Necessary'.
      ELSE.
        LOOP AT g_tc1_itab INTO g_tc1_wa WHERE flag = 'X'.
          CLEAR g_tc1_wa-flag.
          MODIFY g_tc1_itab FROM g_tc1_wa
           INDEX sy-tabix TRANSPORTING flag.

          SELECT SINGLE matnr INTO g_tc1_wa-matnr
            FROM mara
           WHERE matnr = g_tc1_wa-matnr.

          IF sy-subrc <> 0.
            ROLLBACK WORK.
            CLEAR sy-ucomm.
            MODIFY g_tc1_itab FROM g_tc1_wa TRANSPORTING flag
                              WHERE flag = 'X'.
            MESSAGE e305(m3) WITH g_tc1_wa-matnr.
          ENDIF.

          MOVE-CORRESPONDING g_tc1_wa TO ztco_mat.
          ztco_mat-zch_date = sy-datum.
          ztco_mat-zch_time = sy-uzeit.
          ztco_mat-zch_user = sy-uname.
          MODIFY ztco_mat.
        ENDLOOP.

        IF sy-subrc <> 0.
          CALL FUNCTION 'POPUP_CONTINUE_YES_NO'
            EXPORTING
              textline1 = 'All data will be saved'
              textline2 = 'Continue?'
              titel     = 'No selection(s) made'
            IMPORTING
              answer    = l_ans.

          CHECK l_ans = 'J'.

          LOOP AT g_tc1_itab INTO g_tc1_wa.

            SELECT SINGLE matnr INTO g_tc1_wa-matnr
              FROM mara
             WHERE matnr = g_tc1_wa-matnr.

            IF sy-subrc <> 0.
              ROLLBACK WORK.
              CLEAR sy-ucomm.
              MESSAGE e305(m3) WITH g_tc1_wa-matnr.
            ENDIF.

            MOVE-CORRESPONDING g_tc1_wa TO ztco_mat.
            ztco_mat-zch_date = sy-datum.
            ztco_mat-zch_time = sy-uzeit.
            ztco_mat-zch_user = sy-uname.
            MODIFY ztco_mat.
          ENDLOOP.
        ENDIF.

        LOOP AT gt_delrec.
          MOVE-CORRESPONDING gt_delrec TO ztco_mat.
          DELETE ztco_mat.
** By Furong on 02/19/14 (
          IF sy-subrc = 0.
            MOVE-CORRESPONDING ztco_mat TO lw_deleted.
            lw_deleted-zdel_user = sy-uname.
            lw_deleted-zdel_date = sy-datum.
            lw_deleted-zdel_time = sy-uzeit.
            modify ztco_mat_deleted FROM lw_deleted.
            CLEAR lw_deleted.
          ENDIF.
** ) End
          DELETE gt_delrec.
        ENDLOOP.

        gt_orgrec[] = g_tc1_itab[].

        MESSAGE i208(00) WITH 'Data Saved'.
      ENDIF.
      CLEAR: gt_tmprec, gt_tmprec[].

    WHEN 'PRNT'.
      PERFORM print_table_control_data.
    WHEN 'FIND'.
      PERFORM find_entry.

    WHEN 'NEXT'.
      PERFORM find_next.
  ENDCASE.

  CLEAR: sy-ucomm.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  print_table_control_data
*&---------------------------------------------------------------------*
*       Prints the Table control data
*----------------------------------------------------------------------*
FORM print_table_control_data.
  DATA: l_callback TYPE sy-repid.
  l_callback = sy-repid.

  CALL FUNCTION 'FITRV_PRINT_TABLE_CONTROL_DATA'
    EXPORTING
      table_control                  = tc1
      callback_program               = l_callback
      callback_top_of_list           = 'TOP_OF_PAGE'
*     CALLBACK_TOP_OF_PAGE           =
*     CALLBACK_END_OF_PAGE           =
*     CALLBACK_END_OF_LIST           =
      optimize_column_width          = 'X'
      get_curr_quan_fields_from_ddic = 'X'
*     WINDOW_TITLE                   =
*     PRINT_IMMEDIATELY              = ' '
    TABLES
      print_data                     = g_tc1_itab
    EXCEPTIONS
      column_information_missing     = 1
      printing_not_possible          = 2
      OTHERS                         = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " print_table_control_data
*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
*       Generate Top of Page
*----------------------------------------------------------------------*
FORM top_of_page.
  WRITE: / 'Print Date/Time:', sy-datum, '/', sy-uzeit.
ENDFORM.                    " top_of_page
*&---------------------------------------------------------------------*
*&      Form  find_entry
*&---------------------------------------------------------------------*
*       Find Entry
*----------------------------------------------------------------------*
FORM find_entry.
  DATA: l_retcode(1) TYPE c.

  CLEAR: tab[], tab.
  tab-tabname = 'MARA'.
  tab-fieldname = 'MATNR'.
  APPEND tab.

  tab-tabname = 'MARA'.
  tab-fieldname = 'MTART'.
  APPEND tab.

  tab-tabname = 'MARA'.
  tab-fieldname = 'MATKL'.
  APPEND tab.

  CLEAR g_tabix.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title  = 'FIND'
      start_column = '5'
      start_row    = '5'
    IMPORTING
      returncode   = l_retcode
    TABLES
      fields       = tab.

  CHECK l_retcode IS INITIAL.

  LOOP AT tab WHERE NOT value IS INITIAL.
    EXIT.
  ENDLOOP.

  IF sy-subrc NE 0.
    MESSAGE i208(00) WITH 'Please specify at least one search criteria'.
    EXIT.
  ENDIF.

  LOOP AT g_tc1_itab INTO g_tc1_wa.
    g_tabix = sy-tabix.

    CLEAR g_error.
    LOOP AT tab WHERE NOT value IS INITIAL.
      IF tab-fieldname = 'MATNR'.
        g_matnr = tab-value.
        PERFORM conv_matnr CHANGING g_matnr.
        tab-value = g_matnr.
      ENDIF.

      CONCATENATE 'G_TC1_WA' '-' tab-fieldname INTO g_fieldname.
      ASSIGN (g_fieldname) TO <fs>.

      IF <fs> <> tab-value.
        g_error = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF g_error IS INITIAL.
      tc1-top_line = g_tabix.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF NOT g_error IS INITIAL.
    CLEAR g_tabix.
    MESSAGE i208(00) WITH 'No entry found'.
  ENDIF.

ENDFORM.                    " find_entry
*&---------------------------------------------------------------------*
*&      Form  find_next
*&---------------------------------------------------------------------*
*       find next entry
*----------------------------------------------------------------------*
FORM find_next.
  CHECK NOT g_tabix IS INITIAL.

  g_tabix = g_tabix + 1.

  LOOP AT g_tc1_itab INTO g_tc1_wa FROM g_tabix.
    g_tabix = sy-tabix.

    CLEAR g_error.
    LOOP AT tab WHERE NOT value IS INITIAL.
      IF tab-fieldname = 'MATNR'.
        g_matnr = tab-value.
        PERFORM conv_matnr CHANGING g_matnr.
        tab-value = g_matnr.
      ENDIF.

      CONCATENATE 'G_TC1_WA' '-' tab-fieldname INTO g_fieldname.
      ASSIGN (g_fieldname) TO <fs>.

      IF <fs> <> tab-value.
        g_error = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF g_error IS INITIAL.
      tc1-top_line = g_tabix.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF NOT g_error IS INITIAL OR sy-subrc <> 0.
    CLEAR g_tabix.
    MESSAGE i208(00) WITH 'No entry found'.
  ENDIF.

ENDFORM.                    " find_next
* END OF UD1K952463

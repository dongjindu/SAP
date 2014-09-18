*----------------------------------------------------------------------*
***INCLUDE ZASD03L_DMR_CREATE_F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
FORM read_data.
  SELECT *
         INTO CORRESPONDING FIELDS OF TABLE g_tc_9000_itab
         FROM ztsd_acl_l
        WHERE zrmfg EQ 'Q'
        AND   zrcfg EQ p_zrcfg
        AND   zcmrd IN s_zcmrd.
ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN
*&---------------------------------------------------------------------*
FORM call_screen.
  DESCRIBE TABLE g_tc_9000_itab LINES w_cnt.
  IF w_cnt = 0.
    MESSAGE i000 WITH text-m01.
    STOP.
  ENDIF.

  CALL SCREEN 9000.
ENDFORM.                    " CALL_SCREEN
*&---------------------------------------------------------------------*
*&      Form  REFRESH
*&---------------------------------------------------------------------*
FORM refresh.
*  IMPORT IT_LIST FROM DATABASE INDX(ZR) ID VARIANT.

  LOOP AT   g_tc_9000_itab
       INTO g_tc_9000_wa
       WHERE flag = 'X'
       AND   ( zrcfg = ' ' OR zrcfg = '1' OR zrcfg = '2' ).
    w_index = sy-tabix.

    READ TABLE it_list WITH KEY zacln = g_tc_9000_wa-zacln
                                zcdst = g_tc_9000_wa-zcdst.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING it_list TO g_tc_9000_wa.
      MODIFY g_tc_9000_itab FROM g_tc_9000_wa INDEX w_index.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " REFRESH
*&---------------------------------------------------------------------*
*&      Form  PROCESS_CALCULATE
*&---------------------------------------------------------------------*
FORM process_calculate_BK.
  DATA: wa_jobname LIKE tbtcjob-jobname VALUE 'ZASDA05_CAL',
        wa_jobcount LIKE tbtcjob-jobcount,
        valid TYPE C,
        user_print_params type PRI_PARAMS occurs 0 with header line.

  CLEAR: w_err.
  PERFORM check_issue_no USING save_ok_code.
  IF w_err = 'X'.
    EXIT.
  ENDIF.

  REFRESH it_list. CLEAR it_list.
  LOOP AT   g_tc_9000_itab
       INTO g_tc_9000_wa
       WHERE flag = 'X'
       AND   zrcfg = ' '.
    MOVE-CORRESPONDING g_tc_9000_wa TO it_list.
    it_list-zissn = w_zissn.
    it_list-okcode = 'CALC'.
    APPEND it_list. CLEAR it_list.
  ENDLOOP.

  EXPORT it_list p_one TO   DATABASE indx(zs) ID variant.

*  MESSAGE I000 WITH 'STARTING BATCH JOB'.
*
*  EVENTID = 'ZASD03_01'.
*
*  CALL FUNCTION 'BP_EVENT_RAISE'
*    EXPORTING
*      EVENTID                      = EVENTID
**     EVENTPARM                    = ' '
**     TARGET_INSTANCE              = ' '
*   EXCEPTIONS
*     BAD_EVENTID                  = 1
*     EVENTID_DOES_NOT_EXIST       = 2
*     EVENTID_MISSING              = 3
*     RAISE_FAILED                 = 4
*     OTHERS                       = 5.
  SET PARAMETER ID 'PONE' FIELD p_one.

** changed by Furong on 11/01/2006
  CALL FUNCTION 'JOB_OPEN'
       EXPORTING
            jobname  = wa_jobname
       IMPORTING
            jobcount = wa_jobcount.
 if SY-SUBRC <>  0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

CALL FUNCTION 'GET_PRINT_PARAMETERS'
     EXPORTING
          mode                   = 'BATCH'
          destination            = 'LOCL'
          report                 = 'zasd03m_rec_edit_bg'
          user                   = sy-uname
          no_dialog              = 'X'
     IMPORTING
          out_parameters         = user_print_params
          valid                  = valid
     EXCEPTIONS
          archive_info_not_found = 1
          invalid_print_params   = 2
          invalid_archive_params = 3
          OTHERS                 = 4.

IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.
CLEAR user_print_params-primm .
MODIFY user_print_params TRANSPORTING primm WHERE primm EQ 'X'.

SUBMIT zasd03m_rec_edit_bg
             USER sy-uname
             VIA JOB wa_jobname
             NUMBER wa_jobcount
             TO SAP-SPOOL
             SPOOL PARAMETERS user_print_params
             WITHOUT SPOOL DYNPRO
             AND RETURN.

CALL FUNCTION 'JOB_CLOSE'
         EXPORTING
              JOBCOUNT  = WA_JOBcount
              JOBNAME   = wa_jobname
              STRTIMMED = 'X'
         EXCEPTIONS
              OTHERS    = 4.


CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
  EXPORTING
    TITEL              = 'Warning '
    textline1          = 'Background Job is running'
    TEXTLINE2          = 'Please do not run the same record again!!'
    START_COLUMN       = 25
    START_ROW          = 6
 .
*  SUBMIT zasd03m_rec_edit_bg AND RETURN.
** end of change

  IMPORT it_list it_rec_l it_rec_h it_rec_i
    FROM DATABASE indx(zr)
    ID variant.

  PERFORM refresh.

ENDFORM.                    " PROCESS_CALCULATE
*&---------------------------------------------------------------------*
*&      Form  PROCESS_RECALCULATE
*&---------------------------------------------------------------------*
FORM process_recalculate_BK.
  DATA: flag(1).
  DATA: wa_jobname LIKE tbtcjob-jobname VALUE 'ZASDA05_CAL',
        wa_jobcount LIKE tbtcjob-jobcount,
        valid TYPE C,
        user_print_params type PRI_PARAMS occurs 0 with header line.
  CLEAR: w_err.
  PERFORM check_issue_no USING save_ok_code.
  IF w_err = 'X'.
    EXIT.
  ENDIF.

  REFRESH it_list. CLEAR it_list.
  LOOP AT   g_tc_9000_itab
       INTO g_tc_9000_wa
       WHERE flag = 'X'
       AND   ( zrcfg = '1' OR zrcfg = '2' ).
    MOVE-CORRESPONDING g_tc_9000_wa TO it_list.
    it_list-zissn = w_zissn.
    it_list-okcode = 'RECA'.
    APPEND it_list. CLEAR it_list.
  ENDLOOP.

  EXPORT it_list p_one TO   DATABASE indx(zs) ID variant.

*  set parameter id 'PONE' FIELD P_ONE.
** changed by Furong on 11/01/2006
  CALL FUNCTION 'JOB_OPEN'
       EXPORTING
            jobname  = wa_jobname
       IMPORTING
            jobcount = wa_jobcount.
 if SY-SUBRC <>  0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

CALL FUNCTION 'GET_PRINT_PARAMETERS'
     EXPORTING
          mode                   = 'BATCH'
          destination            = 'LOCL'
          report                 = 'zasd03m_rec_edit_bg'
          user                   = sy-uname
          no_dialog              = 'X'
     IMPORTING
          out_parameters         = user_print_params
          valid                  = valid
     EXCEPTIONS
          archive_info_not_found = 1
          invalid_print_params   = 2
          invalid_archive_params = 3
          OTHERS                 = 4.

IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.
CLEAR user_print_params-primm .
MODIFY user_print_params TRANSPORTING primm WHERE primm EQ 'X'.

SUBMIT zasd03m_rec_edit_bg
             USER sy-uname
             VIA JOB wa_jobname
             NUMBER wa_jobcount
             TO SAP-SPOOL
             SPOOL PARAMETERS user_print_params
             WITHOUT SPOOL DYNPRO
             AND RETURN.

CALL FUNCTION 'JOB_CLOSE'
         EXPORTING
              JOBCOUNT  = WA_JOBcount
              JOBNAME   = wa_jobname
              STRTIMMED = 'X'
         EXCEPTIONS
              OTHERS    = 4.


CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
  EXPORTING
    TITEL              = 'Warning '
    textline1          = 'Background Job is running'
    TEXTLINE2          = 'Please do not run the same record again!!'
    START_COLUMN       = 25
    START_ROW          = 6
          .

*  SUBMIT zasd03m_rec_edit_bg AND RETURN.
** end of change
IMPORT it_list it_rec_l it_rec_h it_rec_i
  FROM DATABASE indx(zr)
  ID variant.

PERFORM refresh.

ENDFORM.                    " PROCESS_RECALCULATE
*&---------------------------------------------------------------------*
*&      Form  CONFIRM_DMR
*&---------------------------------------------------------------------*
FORM confirm_dmr.
  LOOP AT   g_tc_9000_itab
       INTO g_tc_9000_wa
       WHERE flag = 'X'
       AND   ( zrcfg = '1' OR zrcfg = '2' ).
    g_tc_9000_wa-zrcfg = 'Q'.

    MODIFY g_tc_9000_itab FROM g_tc_9000_wa INDEX sy-tabix.

    UPDATE ztsd_acl_l SET : zrcfg = g_tc_9000_wa-zrcfg
                      WHERE zacln = g_tc_9000_wa-zacln
                      AND   zcdst = g_tc_9000_wa-zcdst.

  ENDLOOP.

  IF sy-subrc = 0.
    MESSAGE i000 WITH text-m08.
  ELSE.
    MESSAGE i000 WITH text-m09.
  ENDIF.
ENDFORM.                    " CONFIRM_DMR















************************************************************************
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
    WHEN 'P--' OR                     "top of list
         'P-'  OR                     "previous page
         'P+'  OR                     "next page
         'P++'.                       "bottom of list
      PERFORM compute_scrolling_in_tc USING p_tc_name
                                            l_ok.
      CLEAR p_ok.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
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

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

  ENDCASE.

ENDFORM.                              " USER_OK_TC

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
*              NO_ENTRY_OR_PAGE_ACT  = 01
*              NO_ENTRY_TO           = 02
*              NO_OK_CODE_OR_PAGE_GO = 03
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
                                                            " REFRESH_1
*&---------------------------------------------------------------------*
*&      Form  CHECK_ISSUE_NO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_issue_no USING p_calmode.
  DATA: l_st1,
        l_st2.
  DATA: no_select.
  DATA: l_text(15).
  DATA: l_issn LIKE ztsd_acl_l-zissn.
  DATA: lt_itab LIKE g_tc_9000_wa OCCURS 0
                     WITH HEADER LINE.


  IF p_calmode = 'CALC'.
    l_st1 = ' '.
    l_st2 = ' '.
  ELSE.
    l_st1 = '1'.
    l_st2 = '2'.
  ENDIF.

  LOOP AT g_tc_9000_itab INTO g_tc_9000_wa
             WHERE flag = 'X'      AND
                   ( zrcfg = l_st1  OR
                     zrcfg = l_st2 ).
    no_select = 'N'.
    APPEND g_tc_9000_wa TO lt_itab.

  ENDLOOP.

* check if item selected
  IF no_select  IS INITIAL .
    w_err = 'X'.
    CONCATENATE 'with status' l_st1 ',' l_st2
                INTO l_text.
    MESSAGE i000 WITH text-m10 l_text.
    EXIT.
  ENDIF.

* check if selected items has the same issue number.
  CLEAR g_tc_9000_wa.
  READ TABLE lt_itab INDEX 1.
  l_issn = lt_itab-zissn.
  LOOP AT lt_itab.
    IF lt_itab-zissn NE l_issn.
      w_err = 'X'.
      MESSAGE i000 WITH
      'You need to select items with same issue no'.
      EXIT.
    ENDIF.
  ENDLOOP .
  CHECK w_err NE 'X'.

* check if issue entered for item without issue number
  IF l_issn  EQ space  AND
     w_zissn EQ space.
    w_err = 'X'.
    MESSAGE i000 WITH 'Please assign an issue number'.
  ENDIF.

* if a new issue no assigned check if the number exist already.
  IF l_issn  EQ space AND
     w_zissn NE space.
    CLEAR: no_select.
    PERFORM check_no_exist USING no_select w_zissn.
    IF no_select = 'Y'.
      w_err = 'X'.
      MESSAGE i000 WITH 'The issue no already exists '.
      EXIT.
    ENDIF.
  ENDIF.
* check if user enter an issue number.
* if issue number endered, it must be the same with selected
* issue number or the selected number is empty.
  IF NOT w_zissn IS INITIAL AND
     l_issn NE space        AND
     l_issn NE w_zissn.
    w_err = 'X'.
    MESSAGE i000 WITH 'The entered issue no must be the sa'
                      'me with selected item issue no'.
    EXIT.
  ENDIF.
* assign the issue no
  IF l_issn NE space AND
     w_zissn IS INITIAL .
    w_zissn = l_issn.
  ENDIF.


ENDFORM.                    " CHECK_ISSUE_NO
*&---------------------------------------------------------------------*
*&      Form  check_no_exist
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NO_SELECT  text
*      -->P_W_ZISSN  text
*----------------------------------------------------------------------*
FORM check_no_exist USING    p_exist
                             p_zissn.
  DATA: l_issn LIKE it_list-zissn.

* check if the no exist in internal table it_acl_l
  LOOP AT it_list.
    IF it_list-zissn = p_zissn.
      p_exist = 'Y'.
      EXIT.
    ENDIF.
  ENDLOOP.

  CHECK p_exist NE 'Y'.

* check if the no exist in database table ztsd_acl_l.
  SELECT zissn INTO l_issn
    FROM ztsd_acl_l
    WHERE zissn = p_zissn.
    IF sy-subrc EQ 0.
      p_exist = 'Y'.
      EXIT.
    ENDIF.
  ENDSELECT.
ENDFORM.                    " check_no_exist

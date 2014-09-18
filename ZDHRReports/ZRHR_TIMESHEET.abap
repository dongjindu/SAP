************************************************************************
* Program Name      : ZRHR_TIMESHEET
* Creation Date     : 07/12/2007
* Development Request No :
* Addl Documentation:
* Description       : Timesheet report
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT zrhr_timesheet NO STANDARD PAGE HEADING
                     LINE-SIZE 255
                     LINE-COUNT 46
                     MESSAGE-ID zmhr.
TYPE-POOLS slis.
TABLES: pa0001, t526.
CONSTANTS: c_werks(4) VALUE 'HMMA'.

* by ig.moon 7/31/2008 { " emp. status
DATA: BEGIN OF it_status OCCURS 0,
        pernr LIKE pa0000-pernr,
        begda LIKE pa0000-begda,
        massn LIKE pa0000-massn,
        massg LIKE pa0000-massg,
        stat2 LIKE pa0000-stat2,
      END OF it_status           .
* }

DATA: BEGIN OF it_tab OCCURS 0,
      sachz LIKE pa0001-sachz,
      pernr LIKE catsdb-pernr,
      sname LIKE pa0001-sname,
      schkz LIKE pa0007-schkz,

* by ig.moon 3/6/2009 {
      anzshtxt TYPE zanzshtxt,
* }
      awart LIKE catsdb-awart,
      atext LIKE t554t-atext,
      total LIKE catsdb-catshours,
      catshours_01 LIKE catsdb-catshours,
      catshours_02 LIKE catsdb-catshours,
      catshours_03 LIKE catsdb-catshours,
      catshours_04 LIKE catsdb-catshours,
      catshours_05 LIKE catsdb-catshours,
      catshours_06 LIKE catsdb-catshours,
      catshours_07 LIKE catsdb-catshours,
      remark(40),
      END OF it_tab.

DATA: BEGIN OF it_day OCCURS 21,
        seq(2)  TYPE n,
        datum   LIKE   sy-datum,
      END   OF it_day.

DATA: w_frdate LIKE sy-datum,
      w_todate LIKE sy-datum.

DATA: ok_code      LIKE sy-ucomm,
      w_repid  LIKE sy-repid,
      w_cnt       TYPE   i,
      w_flag(1).

*DATA:  L_KALID LIKE KAKO-KALID.

DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldcat_fi  TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldcat_co  TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE,
       it_fieldcat_det TYPE lvc_t_fcat WITH HEADER LINE. "/Detail

DATA : wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
       wa_is_print TYPE lvc_s_prnt,
*      it_color type LVC_T_SCOL,
*      wa_color like line of it_color,
      w_fieldname    LIKE LINE OF it_fieldcat.

DATA: wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
      wa_variant TYPE disvariant.      "for parameter IS_VARIANT

DATA: wa_custom_control TYPE        scrfname VALUE 'ALV_CONTAINER',
      alv_grid          TYPE REF TO cl_gui_alv_grid,
      grid_container    TYPE REF TO cl_gui_custom_container.

DATA: wa_custom_control_po TYPE scrfname VALUE 'ALV_CONTAINER_210',
      alv_grid_po TYPE REF TO cl_gui_alv_grid,
      grid_container_po TYPE REF TO cl_gui_custom_container.

*FIELD-SYMBOLS : <FS01>, <FS02>, <FS-QTY>.
FIELD-SYMBOLS : <fs01>.
*DATA: dsn(4).

**--- Constants

DATA:  w_refresh(1),
       w_new(1) VALUE 'X'.

DATA  g_flag.

DATA : BEGIN OF value_tab OCCURS 0,
        sachx LIKE t526-sachx,
        sachn LIKE t526-sachn,
        END OF value_tab.

DATA:  l_agr_name LIKE agr_users-agr_name,
        l_uname(12),
        l_auth(1),
        l_mess(40),
        l_to_dat LIKE sy-datum.

DATA  super_role.

SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
PARAMETERS : p_pernr LIKE t526-usrid OBLIGATORY.
SELECTION-SCREEN COMMENT 60(20) text-t06.
PARAMETERS : p_fdate LIKE sy-datum OBLIGATORY.
PARAMETERS : p_tdate LIKE sy-datum OBLIGATORY.
** Changed by Furong on 02/12/09
*PARAMETERS : p_sachx LIKE t526-sachx OBLIGATORY.
SELECT-OPTIONS: s_sachx FOR t526-sachx NO INTERVALS OBLIGATORY .
** End of change
PARAMETERS : p_schkz LIKE pa0007-schkz.
SELECTION-SCREEN END OF BLOCK block1.

SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME.
SELECTION-SCREEN COMMENT 1(50) text-t03.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(50) text-t04.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(50) text-t05.
SELECTION-SCREEN END OF BLOCK block2.

*select-options: s_sachx for t526-sachx OBLIGATORY.

** Changed by Furong on 02/12/09
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_sachx.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_sachx-low.
** End of change


  SELECT sachx sachn FROM t526
         INTO TABLE value_tab.

  SORT value_tab BY sachx.
  DELETE ADJACENT DUPLICATES FROM value_tab.
  w_repid = sy-repid.

* Set F4 values for Module Code

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            retfield        = 'SACHX'
            dynpprog        = w_repid
            dynpnr          = '1000'
            dynprofield     = 'SACHX'
            window_title    = 'Administrator Codes'
            value_org       = 'S'
       TABLES
            value_tab       = value_tab
       EXCEPTIONS
            parameter_error = 1.

START-OF-SELECTION.

  IF sy-uname = '100206' OR sy-uname = '100295' OR sy-uname = '103569'.
  ELSE.

** Changed by Furong on 02/12/09
*    CONCATENATE 'Z:HR_TIME_CATS_' p_sachx INTO l_agr_name.
*    SELECT SINGLE uname to_dat INTO (l_uname,l_to_dat)
*      FROM agr_users
*      WHERE agr_name = l_agr_name
*        AND uname = p_pernr.
*
*    IF sy-subrc <> 0.
*      MESSAGE i001 WITH 'No authorization'.
*      EXIT.
*    ENDIF.
*    IF l_to_dat < sy-datum.
*      MESSAGE e000 WITH 'Adminstrator not valid '.
*      EXIT.
*    ENDIF.
    CLEAR: l_auth.

* by ig.moon 2/20/2009 {
    CLEAR super_role.
    CONCATENATE 'Z:HR_TIME_CATS_' 'ALL' INTO l_agr_name.
    SELECT SINGLE uname to_dat INTO (l_uname,l_to_dat)
      FROM agr_users
      WHERE agr_name = l_agr_name
        AND uname = p_pernr.

    IF sy-subrc EQ 0 AND l_to_dat >= sy-datum.
      super_role = 'X'.
    ELSE.
* }
      LOOP AT s_sachx.

        CONCATENATE 'Z:HR_TIME_CATS_' s_sachx-low INTO l_agr_name.

        SELECT SINGLE uname to_dat INTO (l_uname,l_to_dat)
          FROM agr_users
          WHERE agr_name = l_agr_name
            AND uname = p_pernr.

        IF sy-subrc <> 0.
          l_auth = 'X'.
          CONCATENATE 'No authorization for' l_agr_name INTO l_mess
                SEPARATED BY space.
          MESSAGE i001 WITH l_mess.
          EXIT.
        ENDIF.
        IF l_to_dat < sy-datum.
          MESSAGE e000 WITH 'Adminstrator not valid '.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF l_auth = 'X'.
        EXIT.
      ENDIF.

    ENDIF.

** End of change
  ENDIF.
  PERFORM set_days.

** Changed by furong on 02/12/09
*  WHILE w_refresh = 'X' OR w_new = 'X'.
*
*    PERFORM get_data.
*    IF w_flag = 'X'.
*      CLEAR: w_flag.
*      EXIT.
*    ENDIF.
*
*    PERFORM display_data.
*  ENDWHILE.

  PERFORM get_data.
  IF w_flag = 'X'.
    CLEAR: w_flag.
    EXIT.
  ENDIF.
  PERFORM print_data.
*  PERFORM set_print.

** End of change on 02/12/09

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  DATA: l_sachx LIKE t526-sachx,
        l_line TYPE i,
        l_text(30).

  DATA: lt_t526 LIKE TABLE OF t526 WITH HEADER LINE.

  DATA: BEGIN OF lt_itab OCCURS 0,
        sachz LIKE pa0001-sachz,
        pernr LIKE pa0001-pernr,
        sname LIKE pa0001-sname,
        schkz LIKE pa0007-schkz,
        awart LIKE catsdb-awart,
        workdate LIKE catsdb-workdate,
        catshours LIKE catsdb-catshours,
        END OF lt_itab.

  DATA: BEGIN OF lt_temp OCCURS 0,
        pernr LIKE pa0001-pernr,
        awart LIKE catsdb-awart,
        workdate LIKE catsdb-workdate,
        catshours LIKE catsdb-catshours,
        sachz LIKE pa0001-sachz,
        END OF lt_temp.

  DATA: BEGIN OF lt_pernr OCCURS 0,
        pernr LIKE pa0001-pernr,
        sname LIKE pa0001-sname,
        schkz LIKE pa0007-schkz,
        sachz LIKE pa0001-sachz,
        awart LIKE catsdb-awart,
        END OF lt_pernr.

  DATA: lw_itab LIKE lt_itab.
  REFRESH: it_tab.

** Changed by Furong on 02/12/09
*  IF p_sachx IS INITIAL.
*    SELECT * INTO TABLE lt_t526
*      FROM t526
*     WHERE usrid = p_pernr.
*    IF sy-subrc = 0.
*      DESCRIBE TABLE lt_t526 LINES l_line.
*      IF l_line > 1.
*        w_flag = 'X'.
*        MESSAGE s001 WITH 'Multiple Admin codes exist, Please
*                              specify one only'.
*        EXIT.
*      ELSE.
*        READ TABLE lt_t526 INDEX 1.
*        l_sachx = lt_t526-sachx.
*      ENDIF.
*    ELSE.
*      MESSAGE s001 WITH 'Adminstrator No. not found'.
*      w_flag = 'X'.
*      EXIT.
*    ENDIF.
*  ELSE.
*    SELECT SINGLE sachx INTO l_sachx
*      FROM t526
*       WHERE werks = c_werks
*         AND sachx = p_sachx.
*    IF sy-subrc IS INITIAL.
*    ELSE.
*      w_flag = 'X'.
*      MESSAGE s001 WITH 'Adminstrator ID not found'.
*      EXIT.
*    ENDIF.
*  ENDIF.

  SELECT * INTO TABLE lt_t526
       FROM t526
      WHERE sachx IN s_sachx.
  IF sy-subrc <> 0.
    MESSAGE s001 WITH 'Adminstrator No. not found'.
    w_flag = 'X'.
    EXIT.
  ENDIF.

** Changed by Furong on 06/17/08
*  IF p_schkz IS INITIAL.
*    SELECT a~pernr sname schkz INTO TABLE lt_pernr
*     FROM pa0001 AS a
*     INNER JOIN pa0007 AS a1
*     ON a~pernr = a1~pernr
**     INNER JOIN CATSDB AS B
**     ON A~PERNR = B~PERNR
*     WHERE a~sachz = l_sachx
*       AND a~endda >= sy-datum
*       AND a1~endda >= w_frdate
** by ig.moon 7/31/2008 {
*       and a1~begda <= w_frdate
*       AND abkrs NE '13'.
** }
*
**     AND A1~SCHKZ = P_SCHKZ
**       AND B~WORKDATE BETWEEN W_FRDATE AND W_TODATE
**       AND B~STATUS <> '60'.
*
*  ELSE.
*    SELECT a~pernr sname schkz INTO TABLE lt_pernr
*    FROM pa0001 AS a
*    INNER JOIN pa0007 AS a1
*    ON a~pernr = a1~pernr
**    INNER JOIN CATSDB AS B
**    ON A~PERNR = B~PERNR
*    WHERE a~sachz = l_sachx
*      AND a~endda >= sy-datum
*      AND a1~schkz = p_schkz
*      AND a1~endda >= w_frdate
** by ig.moon 7/31/2008 {
*       and a1~begda <= w_frdate
*       AND abkrs NE '13'.
** }
**      AND B~WORKDATE BETWEEN W_FRDATE AND W_TODATE
**      AND B~STATUS <> '60'.
*  ENDIF.
*
* by ig.moon 8/19/2008 {


** Changed by Furong on 06/17/08
  IF p_schkz IS INITIAL.
    SELECT a~pernr sname schkz sachz INTO TABLE lt_pernr
     FROM pa0001 AS a
     INNER JOIN pa0007 AS a1
     ON a~pernr = a1~pernr
*     INNER JOIN CATSDB AS B
*     ON A~PERNR = B~PERNR
     FOR ALL ENTRIES IN lt_t526
     WHERE a~sachz = lt_t526-sachx
       AND a~endda >= sy-datum
       AND a1~endda >= w_frdate
* by ig.moon 7/31/2008 {
       AND a1~begda <= w_frdate
       AND abkrs NE '13'.
* }

*     AND A1~SCHKZ = P_SCHKZ
*       AND B~WORKDATE BETWEEN W_FRDATE AND W_TODATE
*       AND B~STATUS <> '60'.

  ELSE.
    SELECT a~pernr sname schkz sachz INTO TABLE lt_pernr
    FROM pa0001 AS a
    INNER JOIN pa0007 AS a1
    ON a~pernr = a1~pernr
*    INNER JOIN CATSDB AS B
*    ON A~PERNR = B~PERNR
     FOR ALL ENTRIES IN lt_t526
     WHERE a~sachz = lt_t526-sachx
      AND a1~schkz = p_schkz
      AND a1~endda >= w_frdate
* by ig.moon 7/31/2008 {
       AND a1~begda <= w_frdate
       AND abkrs NE '13'.
* }
*      AND B~WORKDATE BETWEEN W_FRDATE AND W_TODATE
*      AND B~STATUS <> '60'.
  ENDIF.

* by ig.moon 8/19/2008 {

** End of change on 02/12/09

  SORT lt_pernr BY pernr.
  DELETE ADJACENT DUPLICATES FROM lt_pernr
      COMPARING pernr.
* }

  IF NOT lt_pernr[] IS INITIAL.

* by ig.moon 7/31/2008 { " It has better performance than table join
    SELECT pernr begda massn massg stat2 INTO TABLE it_status
    FROM pa0000
    FOR ALL ENTRIES IN lt_pernr
    WHERE pernr EQ lt_pernr-pernr
*    AND begda <= w_frdate. " by ig.moon 9/18/2008
      AND endda >= w_todate
      AND begda <= w_todate
      AND stat2 EQ '3'.

    SORT it_status BY pernr ASCENDING
                      begda DESCENDING .

    DELETE ADJACENT DUPLICATES FROM it_status
        COMPARING pernr.

  ENDIF.

* }

  LOOP AT lt_pernr.

* by ig.moon 7/31/2008 {
    READ TABLE it_status WITH KEY pernr = lt_pernr-pernr
    BINARY SEARCH.
    IF sy-subrc EQ 0 AND it_status-stat2 EQ '3'.
    ELSE.
      CONTINUE.
    ENDIF.
* }

    REFRESH: lt_temp.
    SELECT pernr awart workdate catshours INTO TABLE lt_temp
      FROM catsdb
      WHERE pernr = lt_pernr-pernr
        AND workdate BETWEEN w_frdate AND w_todate
        AND status <> '60'.
    IF sy-subrc = 0.
      LOOP AT lt_temp.
        MOVE-CORRESPONDING lt_temp TO lt_itab.
        lt_itab-sname = lt_pernr-sname.
        lt_itab-schkz = lt_pernr-schkz.
        lt_itab-sachz = lt_pernr-sachz.
        APPEND lt_itab.
      ENDLOOP.
    ELSE.
      lt_itab-pernr = lt_pernr-pernr.
      lt_itab-sname = lt_pernr-sname.
      lt_itab-schkz = lt_pernr-schkz.
      lt_itab-sachz = lt_pernr-sachz.
      APPEND lt_itab.
    ENDIF.
    CLEAR: lt_itab.
  ENDLOOP.

*  IF P_SCHKZ IS INITIAL.
*    SELECT A~PERNR SNAME AWART WORKDATE CATSHOURS INTO TABLE LT_ITAB
*     FROM PA0001 AS A
*     INNER JOIN PA0007 AS A1
*     ON A~PERNR = A1~PERNR
*     INNER JOIN CATSDB AS B
*     ON A~PERNR = B~PERNR
*     WHERE A~SACHZ = L_SACHX
*       AND A~ENDDA >= SY-DATUM
*       AND A1~ENDDA >= W_FRDATE
**     AND A1~SCHKZ = P_SCHKZ
*       AND B~WORKDATE BETWEEN W_FRDATE AND W_TODATE
*       AND B~STATUS <> '60'.
*
*  ELSE.
*    SELECT A~PERNR SNAME AWART WORKDATE CATSHOURS INTO TABLE LT_ITAB
*    FROM PA0001 AS A
*    INNER JOIN PA0007 AS A1
*    ON A~PERNR = A1~PERNR
*    INNER JOIN CATSDB AS B
*    ON A~PERNR = B~PERNR
*    WHERE A~SACHZ = L_SACHX
*      AND A~ENDDA >= SY-DATUM
*      AND A1~SCHKZ = P_SCHKZ
*      AND A1~ENDDA >= W_FRDATE
*      AND B~WORKDATE BETWEEN W_FRDATE AND W_TODATE
*      AND B~STATUS <> '60'.
*  ENDIF.

** End of change on 06/17/08

  IF lt_itab[] IS INITIAL.
    MESSAGE s001 WITH 'No data in timesheet'.
    w_flag = 'X'.
    EXIT.
  ELSE.
    SORT lt_itab BY pernr awart catshours.
    LOOP AT lt_itab INTO lw_itab.
      AT NEW awart.
        it_tab-pernr = lw_itab-pernr.
        it_tab-sname = lw_itab-sname.
        it_tab-awart = lw_itab-awart.
        it_tab-schkz = lw_itab-schkz.
        it_tab-sachz = lw_itab-sachz.
        SELECT SINGLE atext INTO it_tab-atext
          FROM t554t
          WHERE awart = it_tab-awart.
        IF sy-subrc = 0.
        ELSE.
          CLEAR: it_tab-atext.
        ENDIF.
      ENDAT.
      READ TABLE it_day WITH KEY datum = lw_itab-workdate.
      IF sy-subrc = 0.
        CONCATENATE 'IT_TAB-CATSHOURS_' it_day-seq INTO l_text.
        ASSIGN (l_text) TO <fs01>.
        IF sy-subrc = 0.
          <fs01> = <fs01> + lw_itab-catshours.
        ENDIF.
        UNASSIGN <fs01>.
      ENDIF.
      AT END OF awart.
        SUM.
        it_tab-total = lw_itab-catshours.
        APPEND it_tab.
        CLEAR: it_tab.
      ENDAT.
    ENDLOOP.
  ENDIF.
** Changed by Furong on 12/08/08
*  DELETE it_tab WHERE ( awart = '1055' OR awart = '1056' ).
** End of change
* rechanged by ig.moon 1/12/2009 {
  LOOP AT it_tab.
    IF it_tab-awart = '1055' OR it_tab-awart = '1056'.
      CLEAR : it_tab-awart,
              it_tab-atext,
              it_tab-total,
              it_tab-catshours_01,
              it_tab-catshours_02,
              it_tab-catshours_03,
              it_tab-catshours_04,
              it_tab-catshours_05,
              it_tab-catshours_06,
              it_tab-catshours_07,
              it_tab-remark.
      MODIFY it_tab.
    ENDIF.
  ENDLOOP.
* }
ENDFORM.
*---------------------------------------------------------------------*
*       FORM get_req_data                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
*       FORM set_days                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM set_days.


  DATA: l_count TYPE i.
  DATA: l_date LIKE sy-datum.

  l_count = p_tdate - p_fdate + 1.

  IF l_count > 7 and sy-uname ne '103569'. " Fix bug purpose by ig.moon

    MESSAGE i001 WITH 'Only allow 7 days'.

    EXIT.
  ENDIF.

  it_day-seq = 1.
  it_day-datum = p_fdate.
  APPEND it_day.
  l_count = '01'.
  l_date = p_fdate .
  WHILE l_date < p_tdate.
*  DO 6 TIMES.
    l_count  = l_count + 1.
    l_date   = l_date  + 1.
*    PERFORM read_working_date USING '+' l_kalid  l_date.
    it_day-seq     = l_count.
    it_day-datum   = l_date .
    APPEND it_day.  CLEAR: it_day.
*  ENDDO.
  ENDWHILE.
  w_frdate = p_fdate.
  w_todate = p_tdate.
ENDFORM.                    " set_DAYS
*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*---------------------------------------------------------------------*
*       FORM read_working_date                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PA_TYPE                                                       *
*  -->  PA_KALID                                                      *
*  -->  PA_WDATE                                                      *
*---------------------------------------------------------------------*
FORM read_working_date USING  pa_type  pa_kalid  pa_wdate.
  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
       EXPORTING
            correct_option               = pa_type
            date                         = pa_wdate
            factory_calendar_id          = pa_kalid
       IMPORTING
            date                         = pa_wdate
       EXCEPTIONS
            calendar_buffer_not_loadable = 1
            correct_option_invalid       = 2
            date_after_range             = 3
            date_before_range            = 4
            date_invalid                 = 5
            factory_calendar_not_found   = 6
            OTHERS                       = 7.
ENDFORM.                    " READ_WORKING_DATE
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  CALL SCREEN 200.
ENDFORM.                    " display_data
INCLUDE zrhr_timesheet_pbo.
*INCLUDE zrmm_requirement_plan_pbo.

INCLUDE zrhr_timesheet_pai.
*&---------------------------------------------------------------------*
*&      Form  write_title
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_title.
  DATA: l_date LIKE sy-datum,
        l_day(20),
        l_weekday  LIKE dtresr-weekday,
        l_cn TYPE i,
        l_line(1),
        l_pernr LIKE pa0007-pernr.


*** Print title
  FORMAT COLOR OFF.
  WRITE : /(23) 'TIME SHEET REPORT: From'.
  WRITE:  (8) w_frdate MM/DD/YY.
  WRITE:  (2) 'to'.
  WRITE:  (8) w_todate MM/DD/YY.
*  WRITE : (80) 'Page : ', W_PAGE.

** Changed by Furong on 02/12/09
*  WRITE : /(26) 'Time Admin Code:', p_sachx.
  WRITE : /(26) 'Time Admin Code:', it_tab-sachz.
** End of Change
  SKIP.

  WRITE : / sy-uline.
*  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / sy-vline NO-GAP, (8) 'Per No'             NO-GAP,
            sy-vline NO-GAP, (25) '        Name'      NO-GAP,
            sy-vline NO-GAP, (8) 'Shift '             NO-GAP,
            sy-vline NO-GAP, (3) '   '             NO-GAP,
            sy-vline NO-GAP, (6) 'A/A TY'             NO-GAP,
            sy-vline NO-GAP, (25) '      Description' NO-GAP.

  l_date = w_frdate.
  WRITE: sy-vline NO-GAP, (10) l_date  NO-GAP.
  l_date = l_date + 1.
  WRITE: sy-vline NO-GAP, (10) l_date  NO-GAP.
  l_date = l_date + 1.
  WRITE: sy-vline NO-GAP, (10) l_date  NO-GAP.
  l_date = l_date + 1.
  WRITE: sy-vline NO-GAP, (10) l_date  NO-GAP.
  l_date = l_date + 1.
  WRITE: sy-vline NO-GAP, (10) l_date  NO-GAP.
  l_date = l_date + 1.
  WRITE: sy-vline NO-GAP, (10) l_date  NO-GAP.
  l_date = l_date + 1.
  WRITE: sy-vline NO-GAP, (10) l_date  NO-GAP.

  WRITE: sy-vline NO-GAP, (10) '  Total'              NO-GAP.
  WRITE: sy-vline NO-GAP,(40) '       Team Member Signature' NO-GAP,
  sy-vline NO-GAP.

  WRITE : / sy-vline NO-GAP, (8)  ' '     NO-GAP,
              sy-vline NO-GAP, (25) ' '   NO-GAP,
              sy-vline NO-GAP, (8)  ' '   NO-GAP,
              sy-vline NO-GAP, (3)  ' '   NO-GAP,
              sy-vline NO-GAP, (6)  ' '   NO-GAP,
              sy-vline NO-GAP, (25) ' '   NO-GAP.

  l_date = w_frdate.
  CALL FUNCTION 'DATE_TO_DAY'
       EXPORTING
            date    = l_date
       IMPORTING
            weekday = l_weekday.

  WRITE: sy-vline NO-GAP, (10) l_weekday  NO-GAP.
  l_date = l_date + 1.
  CALL FUNCTION 'DATE_TO_DAY'
       EXPORTING
            date    = l_date
       IMPORTING
            weekday = l_weekday.
  WRITE: sy-vline NO-GAP, (10) l_weekday  NO-GAP.
  l_date = l_date + 1.
  CALL FUNCTION 'DATE_TO_DAY'
       EXPORTING
            date    = l_date
       IMPORTING
            weekday = l_weekday.
  WRITE: sy-vline NO-GAP, (10) l_weekday  NO-GAP.
  l_date = l_date + 1.
  CALL FUNCTION 'DATE_TO_DAY'
       EXPORTING
            date    = l_date
       IMPORTING
            weekday = l_weekday.
  WRITE: sy-vline NO-GAP, (10) l_weekday  NO-GAP.
  l_date = l_date + 1.
  CALL FUNCTION 'DATE_TO_DAY'
       EXPORTING
            date    = l_date
       IMPORTING
            weekday = l_weekday.
  WRITE: sy-vline NO-GAP, (10) l_weekday  NO-GAP.
  l_date = l_date + 1.
  CALL FUNCTION 'DATE_TO_DAY'
       EXPORTING
            date    = l_date
       IMPORTING
            weekday = l_weekday.
  WRITE: sy-vline NO-GAP, (10) l_weekday  NO-GAP.
  l_date = l_date + 1.
  CALL FUNCTION 'DATE_TO_DAY'
       EXPORTING
            date    = l_date
       IMPORTING
            weekday = l_weekday.
  WRITE: sy-vline NO-GAP, (10) l_weekday  NO-GAP.

  WRITE: sy-vline NO-GAP, (10) ' '   NO-GAP.

  WRITE: sy-vline NO-GAP,(40) '      ' NO-GAP,
         sy-vline NO-GAP.

ENDFORM.                    " write_title
*&---------------------------------------------------------------------*
*&      Form  PRINT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_data.
  DATA: l_cn TYPE i,
       l_line(1),
       l_pernr LIKE pa0007-pernr,
       l_line_int.

  DATA $ix TYPE i.
  DATA $tprog TYPE  tprog.

  LOOP AT it_tab.
    $ix = sy-tabix.
    CALL FUNCTION 'Z_CO_GET_DWS_IG'
         EXPORTING
              schkz                          = it_tab-schkz
              datum                          = p_fdate
         IMPORTING
              tprog                          = $tprog
         EXCEPTIONS
              not_found_work_schedule_rules  = 1
              invalid_date                   = 2
              not_found_period_work_schedule = 3
              OTHERS                         = 4.
    IF sy-subrc <> 0.
      $tprog = it_tab-schkz.
    ENDIF.


    CASE $tprog.
      WHEN '0002' OR '1003' OR '1002'.
        it_tab-anzshtxt = '2nd'.
*      WHEN '0001' OR '1000' OR '1001'.
*        it_tab-anzshtxt = '1st'.
      WHEN OTHERS.
        it_tab-anzshtxt = '1st'.
    ENDCASE.

    MODIFY it_tab INDEX $ix TRANSPORTING anzshtxt.
  ENDLOOP.


*  new-page line-size 198 print on.
*  print-control FONT 8.

** print content
  SORT it_tab BY sachz pernr.

  LOOP AT it_tab.
    AT NEW sachz.
*      NEW-PAGE print on LINE-SIZE 207.
      NEW-PAGE LINE-SIZE 211.
      l_pernr = '*'.
      l_cn = 0.
      PERFORM write_title.
    ENDAT.

    IF sy-linno > 43.
      WRITE : / sy-uline.
      NEW-PAGE LINE-SIZE 211.
      l_pernr = '*'.
      l_cn = 0.
      PERFORM write_title.
*      WRITE : SY-ULINE.
    ENDIF.

    IF it_tab-pernr <> l_pernr.
      IF l_cn = 0.
*        FORMAT COLOR 2.
*        FORMAT COLOR OFF.
        l_cn = 1.
        l_line_int = 'X'.
      ELSE.
*        FORMAT COLOR OFF.
        l_cn = 0.
        l_line_int = space.
      ENDIF.

      WRITE : / sy-uline.
      WRITE : / sy-vline NO-GAP, (8) it_tab-pernr    NO-GAP,
                  sy-vline NO-GAP, (25) it_tab-sname    NO-GAP.
      l_pernr = it_tab-pernr.
      l_line = ' '.
    ELSE.
      WRITE: / sy-vline NO-GAP, (8) ' '  NO-GAP,
               sy-vline NO-GAP, (25) ' ' NO-GAP.
      ULINE 36(135).
      WRITE: 211 sy-vline NO-GAP.

      WRITE: / sy-vline NO-GAP, (8) ' '  NO-GAP,
                sy-vline NO-GAP, (25) ' ' NO-GAP.
      l_line = 'X'.
    ENDIF.
    WRITE: sy-vline NO-GAP, (8) it_tab-schkz NO-GAP.
    WRITE: sy-vline NO-GAP, (3) it_tab-anzshtxt NO-GAP.
    WRITE: sy-vline NO-GAP, (6) it_tab-awart NO-GAP,
           sy-vline NO-GAP, (25) it_tab-atext NO-GAP,
           sy-vline NO-GAP, (10) it_tab-catshours_01 NO-GAP NO-ZERO,
           sy-vline NO-GAP, (10) it_tab-catshours_02 NO-GAP NO-ZERO,
           sy-vline NO-GAP, (10) it_tab-catshours_03 NO-GAP NO-ZERO,
           sy-vline NO-GAP, (10) it_tab-catshours_04 NO-GAP NO-ZERO,
           sy-vline NO-GAP, (10) it_tab-catshours_05 NO-GAP NO-ZERO,
           sy-vline NO-GAP, (10) it_tab-catshours_06 NO-GAP NO-ZERO,
           sy-vline NO-GAP, (10) it_tab-catshours_07 NO-GAP NO-ZERO,
           sy-vline NO-GAP, (10) it_tab-total NO-GAP NO-ZERO,
           sy-vline NO-GAP.
    IF l_line = ' '.
      IF l_line_int EQ 'X'.
        WRITE: 171(34) it_tab-sname NO-GAP COLOR 2.
      ELSE.
        WRITE: 171(34) it_tab-sname NO-GAP.
      ENDIF.
    ELSE.
      WRITE: 171(38) space NO-GAP.
    ENDIF.
    WRITE: 211 sy-vline NO-GAP.

    AT END OF sachz.
      WRITE : / sy-uline.
      FORMAT COLOR OFF.
      WRITE : / sy-vline NO-GAP, (80) 'Supervisor Signature:' NO-GAP,
                sy-vline NO-GAP, (10) space NO-GAP,
                sy-vline NO-GAP, (10) space NO-GAP,
                sy-vline NO-GAP, (10) space NO-GAP,
                sy-vline NO-GAP, (10) space NO-GAP,
                sy-vline NO-GAP, (10) space NO-GAP,
                sy-vline NO-GAP, (10) space NO-GAP,
                sy-vline NO-GAP, (10) space NO-GAP,
                sy-vline NO-GAP, (10) space NO-GAP,
                sy-vline NO-GAP, (40) space NO-GAP,
                sy-vline NO-GAP.
      WRITE : / sy-uline.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " PRINT_DATA
*&---------------------------------------------------------------------*
*&      Form  set_print
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_print.

  CALL FUNCTION 'SET_PRINT_PARAMETERS'
       EXPORTING
            immediately    = 'X'
            layout         = 'X_65_200'  "'ZX_50_210'
            sap_cover_page = ' '.
ENDFORM.                    " set_print


************************************************************************
* Program Name      : ZAHR_CAT2_M
* Author            : Furong, Wang
* Creation Date     : 08/2007
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       : Weekly tiemsheet update
*
* Modification Logs
* Date       Developer    RequestNo    Description
*01/18/08    Furong Wang  UD1K942710   New Excel sheet layout
*
*
************************************************************************

REPORT zahr_cat2_m NO STANDARD PAGE HEADING
                LINE-SIZE  100
                LINE-COUNT 65
                MESSAGE-ID zmhr.
*TABLES: PA0001.
DATA: BEGIN OF it_file OCCURS 0,
      name(20),
      pernr(8),
      shift(11),
      ot1(8),
      day11(8),
      day12(8),
      day13(8),
      day14(8),
      ot2(8),
      day21(8),
      day22(8),
      day23(8),
      day24(8),
      ot3(8),
      day31(8),
      day32(8),
      day33(8),
      day34(8),
      ot4(8),
      day41(8),
      day42(8),
      day43(8),
      day44(8),
      ot5(8),
      day51(8),
      day52(8),
      day53(8),
      day54(8),
      ot6(8),
      day61(8),
      day62(8),
      day63(8),
      day64(8),
      ot7(8),
      day71(8),
      day72(8),
      day73(8),
      day74(8),
      total(8),
      remarks(40),
      END OF it_file.

DATA: BEGIN OF it_cat2         OCCURS 0,
      pernr LIKE catsd-pernr,
      awart LIKE catsd-awart,
      day1 LIKE catsdb-catshours,
      day2 LIKE catsdb-catshours,
      day3 LIKE catsdb-catshours,
      day4 LIKE catsdb-catshours,
      day5 LIKE catsdb-catshours,
      day6 LIKE catsdb-catshours,
      day7 LIKE catsdb-catshours,
      total LIKE catsdb-catshours,
      name(20),
      remarks(40),
      END OF it_cat2.

DATA: it_output LIKE TABLE OF it_cat2 WITH HEADER LINE,
      it_output_err LIKE TABLE OF it_cat2 WITH HEADER LINE.

DATA: w_cn(3) TYPE n,
      w_flag(1).
DATA: it_excl             LIKE TABLE OF it_file       WITH HEADER LINE,
      it_msg              LIKE TABLE OF bdcmsgcoll    WITH HEADER LINE.

DATA : BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA : END OF it_bdc.
DATA : BEGIN OF it_mess OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA : END OF it_mess.

DATA : w_mode LIKE ctu_params-dismode VALUE 'E', "'E'. "A-display 'N'
       w_fr_date LIKE sy-datum,
       w_filety LIKE rlgrap-filetype VALUE 'DAT',
       w_admin LIKE catsfields-sachz.

*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-101.
PARAMETERS:
  p_file  LIKE rlgrap-filename OBLIGATORY.
PARAMETERS:  p_trun(1).
SELECTION-SCREEN END   OF BLOCK b1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM at_sel_screen_on_value_request USING p_file 'O'.

START-OF-SELECTION.
** On 04/03/14
  MESSAGE e001 with 'The program is obsolete'.
** End

  PERFORM upload_process.
  IF it_excl[] IS INITIAL.
    EXIT.
  ENDIF.
  PERFORM read_excel.
  IF w_flag = 'X'.
    CLEAR:  w_flag.
    EXIT.
  ENDIF.

  PERFORM data_process_bapi.
  PERFORM display_data.

*&---------------------------------------------------------------------*
*&      Form  AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
FORM at_sel_screen_on_value_request USING def_path LIKE rlgrap-filename
                                          mode     TYPE c.

  DATA: tmp_filename LIKE rlgrap-filename.
  DATA: tmp_mask(80).                  " LIKE GLOBAL_FILEMASK_ALL.
  DATA: fieldln TYPE i.
  FIELD-SYMBOLS: <tmp_sym>.

  tmp_mask = ',*.*,*.*.'.
  fieldln = strlen( def_path ) - 1.
  ASSIGN def_path+fieldln(1) TO <tmp_sym>.
  IF <tmp_sym> = '/' OR <tmp_sym> = '\'.
    CLEAR <tmp_sym>.
  ENDIF.

  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            def_filename     = p_file
            def_path         = def_path
*           MASK             = ',*.*,*.*.'
            mask             = tmp_mask
            mode             = mode
*           TITLE            = ' '
       IMPORTING
            filename         = tmp_filename
*         RC               =
       EXCEPTIONS
            inv_winsys       = 01
            no_batch         = 02
            selection_cancel = 03
            selection_error  = 04.

  IF sy-subrc = 0.
    p_file = tmp_filename.
  ENDIF.

ENDFORM.                               " AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_PROCESS
*&---------------------------------------------------------------------*
FORM upload_process.

  CALL FUNCTION 'UPLOAD'
   EXPORTING
*   CODEPAGE                      = ' '
     filename                      = p_file
     filetype                      = w_filety
*   ITEM                          = ' '
*   FILEMASK_MASK                 = ' '
*   FILEMASK_TEXT                 = ' '
*   FILETYPE_NO_CHANGE            = ' '
*   FILEMASK_ALL                  = ' '
*   FILETYPE_NO_SHOW              = ' '
*   LINE_EXIT                     = ' '
*   USER_FORM                     = ' '
*   USER_PROG                     = ' '
*   SILENT                        = 'S'
* IMPORTING
*   FILESIZE                      =
*   CANCEL                        =
*   ACT_FILENAME                  =
*   ACT_FILETYPE                  =
    TABLES
      data_tab                      = it_excl

* EXCEPTIONS
*   CONVERSION_ERROR              = 1
*   INVALID_TABLE_WIDTH           = 2
*   INVALID_TYPE                  = 3
*   NO_BATCH                      = 4
*   UNKNOWN_ERROR                 = 5
*   GUI_REFUSE_FILETRANSFER       = 6
*   OTHERS                        = 7
            .

  CASE sy-subrc.
    WHEN 0.
*      DATA L_TEXT(132).
*      CONCATENATE P_FILE ' successfully uploaded'
*                  INTO L_TEXT.
*      WRITE: / L_TEXT.
*      SKIP.
    WHEN 2.
      MESSAGE e000 WITH 'File open error'.
    WHEN 3.
      MESSAGE e000 WITH 'File read error'.
    WHEN OTHERS.
      MESSAGE e000 WITH 'Upload error'.
  ENDCASE.

ENDFORM.                               " UPLOAD_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_excel.
  DATA: l_endda LIKE pa0001-endda,
        l_date_c(8),
        l_to_dat LIKE sy-datum,
        w_pernr LIKE it_excl-pernr,
        w_name LIKE it_excl-name,
        w_total LIKE it_cat2-day1,
        l_sachz LIKE pa0001-sachz,
        l_agr_name LIKE agr_users-agr_name,
        l_dw(1),
        l_to_weekday LIKE dtresr-weekday,
        l_ans(1) TYPE c,
        l_no(1) TYPE n,
        l_text(30),
        l_first_line(1),
        l_otcode(4),
        l_shift(1).

  DATA: l_awart11 LIKE it_excl-day11,
        l_awart12 LIKE it_excl-day11,
        l_awart13 LIKE it_excl-day11,
        l_awart14 LIKE it_excl-day11,
        l_awart21 LIKE it_excl-day11,
        l_awart22 LIKE it_excl-day11,
        l_awart23 LIKE it_excl-day11,
        l_awart24 LIKE it_excl-day11,
        l_awart31 LIKE it_excl-day11,
        l_awart32 LIKE it_excl-day11,
        l_awart33 LIKE it_excl-day11,
        l_awart34 LIKE it_excl-day11,
        l_awart41 LIKE it_excl-day11,
        l_awart42 LIKE it_excl-day11,
        l_awart43 LIKE it_excl-day11,
        l_awart44 LIKE it_excl-day11,
        l_awart51 LIKE it_excl-day11,
        l_awart52 LIKE it_excl-day11,
        l_awart53 LIKE it_excl-day11,
        l_awart54 LIKE it_excl-day11,
        l_awart61 LIKE it_excl-day11,
        l_awart62 LIKE it_excl-day11,
        l_awart63 LIKE it_excl-day11,
        l_awart64 LIKE it_excl-day11,
        l_awart71 LIKE it_excl-day11,
        l_awart72 LIKE it_excl-day11,
        l_awart73 LIKE it_excl-day11,
        l_awart74 LIKE it_excl-day11.

  DATA: BEGIN OF lt_type OCCURS 0,
        awart LIKE it_cat2-awart,
        day(1),
        value LIKE it_excl-day11,
        END OF lt_type.

  FIELD-SYMBOLS: <fs>.
  DATA: lt_pa0001 LIKE TABLE OF pa0001 WITH HEADER LINE.

  READ TABLE it_excl INDEX 2.
*  L_SHIFT = IT_EXCL-OT6+0(1).
*  CASE L_SHIFT.
*    WHEN '1'.
*      L_OTCODE = '1001'.
*    WHEN '2'.
*      L_OTCODE = '1003'.
*    WHEN '3'.
*      L_OTCODE = '1004'.
*  ENDCASE.

* by ig.moon 2/9/2009 {

*  l_sachz = it_excl-day72+0(4).
  DATA tmp_4(4).
  tmp_4 = it_excl-day72+0(4).
  REPLACE '"' WITH '' INTO tmp_4.
  CONDENSE tmp_4.
  l_sachz = tmp_4(3).

* }


*  READ TABLE IT_EXCL INDEX 5.
  CONCATENATE '20' it_excl-day22+6(2) it_excl-day22+0(2)
              it_excl-day22+3(2) INTO l_date_c.

  WRITE: l_date_c TO w_fr_date.

  CALL FUNCTION 'HR_GB_DAY_RELATIVE_TO_DATE'
   EXPORTING
     date                  = sy-datum
     day_in_week           = '7'
     before_or_after       = '-'
   IMPORTING
     new_date              = l_to_dat
*   DAY_TEXT              =
            .
  l_to_dat = l_to_dat - 7.

  IF w_fr_date <= l_to_dat.
    MESSAGE i001 WITH 'Date entry error'.
    w_flag = 'X'.
    EXIT.
  ENDIF.

  l_to_dat = sy-datum - 7.
  IF w_fr_date <> l_to_dat.
    CALL FUNCTION 'POPUP_TO_DECIDE'
      EXPORTING
        textline1               = 'Entry date should be 7 days ago'
        text_option1            = 'Continue to process'
        text_option2            = 'Back'
        titel                   = 'Warning'
*   START_COLUMN            = 25
*   START_ROW               = 6
*   CANCEL_DISPLAY          = 'X'
        IMPORTING
        answer                  = l_ans
            .
    IF l_ans <> '1'.
      w_flag = 'X'.
      EXIT.
    ENDIF.
  ENDIF.

  IF sy-uname = '101457' OR sy-uname = '100553'.
** for testing only
  ELSE.

    CONCATENATE 'Z:HR_TIME_CATS_' l_sachz INTO l_agr_name.
    CONDENSE l_agr_name.

    SELECT SINGLE to_dat INTO l_to_dat
      FROM agr_users
      WHERE agr_name = l_agr_name.
    IF sy-subrc <> 0.

* by ig.moon 2/9/2009 {
      SELECT SINGLE to_dat INTO l_to_dat
        FROM agr_users
        WHERE agr_name = 'Z:HR_TIME_CATS_ALL'
          AND uname EQ sy-uname.
      IF sy-subrc EQ 0.
      ELSE.
* }
        MESSAGE e000 WITH 'No authorization to upload data'.
        EXIT.
      ENDIF.
    ENDIF.
    IF l_to_dat < sy-datum.
      MESSAGE e000 WITH 'Adminstrator not valid '.
      EXIT.
    ENDIF.
    SELECT * INTO TABLE lt_pa0001
      FROM pa0001
      WHERE sachz = l_sachz
        AND endda >= w_fr_date. "sy-datum.
  ENDIF.

  READ TABLE it_excl INDEX 8.
  w_pernr = it_excl-pernr.
  w_name = it_excl-name.
  l_first_line = 'X'.

  DATA $tprog TYPE  tprog.
  DATA $schkz LIKE pa0007-schkz.
  DATA $pos TYPE i.

  LOOP AT it_excl FROM 8.
    IF it_excl-pernr IS INITIAL AND l_first_line = 'X'.
      CONTINUE.
    ENDIF.
    IF l_first_line = 'X'.
      it_cat2-pernr = it_excl-pernr.
      it_cat2-name = it_excl-name.
      w_pernr = it_excl-pernr.
      w_name = it_excl-name.
      CLEAR: l_first_line.

* by ig.moon 3/16/2009 {

      SEARCH it_excl-shift FOR '#'.

      IF sy-subrc EQ 0.
        $pos = sy-fdpos + 1.
        l_shift = it_excl-shift+$pos(1).
      ELSE.

        $schkz = it_excl-shift.

        CALL FUNCTION 'Z_CO_GET_DWS_IG'
             EXPORTING
                  schkz                          = $schkz
                  datum                          = w_fr_date
             IMPORTING
                  tprog                          = $tprog
             EXCEPTIONS
                  not_found_work_schedule_rules  = 1
                  invalid_date                   = 2
                  not_found_period_work_schedule = 3
                  OTHERS                         = 4.
        IF sy-subrc <> 0.
          $tprog = $schkz.
        ENDIF.

        CASE $tprog.
          WHEN '0002' OR '1003' OR '1002'.
            l_shift = '2'.
          WHEN OTHERS.
            l_shift = '1'.
        ENDCASE.

      ENDIF.

* }
*      l_shift = it_excl-shift+6(1).
      CASE l_shift.
        WHEN '1'.
          l_otcode = '1001'.
        WHEN '2'.
          l_otcode = '1003'.
        WHEN '3'.
          l_otcode = '1004'.
      ENDCASE.

      READ TABLE lt_pa0001 WITH KEY pernr = it_cat2-pernr.
      IF sy-subrc = 0 OR sy-uname = '101457' OR sy-uname = '100553'.
        IF it_excl-ot1 >= 0.
          it_cat2-awart = l_otcode.
          it_cat2-day1 = it_excl-ot1.
        ENDIF.
        IF it_excl-ot2 >= 0.
          it_cat2-awart = l_otcode.
          it_cat2-day2 = it_excl-ot2.
        ENDIF.
        IF it_excl-ot3 >= 0.
          it_cat2-awart = l_otcode.
          it_cat2-day3 = it_excl-ot3.
        ENDIF.
        IF it_excl-ot4 >= 0.
          it_cat2-awart = l_otcode.
          it_cat2-day4 = it_excl-ot4.
        ENDIF.
        IF it_excl-ot5 >= 0.
          it_cat2-awart = l_otcode.
          it_cat2-day5 = it_excl-ot5.
        ENDIF.
        IF it_excl-ot6 >= 0.
          it_cat2-awart = l_otcode.
          it_cat2-day6 = it_excl-ot6.
        ENDIF.
        IF it_excl-ot7 >= 0.
          it_cat2-awart = l_otcode.
          it_cat2-day7 = it_excl-ot7.
        ENDIF.

        IF NOT it_cat2-awart IS INITIAL.
          APPEND it_cat2.
          CLEAR: it_cat2.
          it_cat2-pernr = it_excl-pernr.
          it_cat2-name = it_excl-name.
        ENDIF.

        IF NOT it_excl-day11 IS INITIAL.
          l_awart11 = it_excl-day11.
        ENDIF.
        IF NOT it_excl-day12 IS INITIAL.
          l_awart12 = it_excl-day12.
        ENDIF.
        IF NOT it_excl-day13 IS INITIAL.
          l_awart13 = it_excl-day13.
        ENDIF.
        IF NOT it_excl-day14 IS INITIAL.
          l_awart14 = it_excl-day14.
        ENDIF.
        IF NOT it_excl-day21 IS INITIAL.
          l_awart21 = it_excl-day21.
        ENDIF.
        IF NOT it_excl-day22 IS INITIAL.
          l_awart22 = it_excl-day22.
        ENDIF.
        IF NOT it_excl-day23 IS INITIAL.
          l_awart23 = it_excl-day23.
        ENDIF.
        IF NOT it_excl-day24 IS INITIAL.
          l_awart24 = it_excl-day24.
        ENDIF.
        IF NOT it_excl-day31 IS INITIAL.
          l_awart31 = it_excl-day31.
        ENDIF.
        IF NOT it_excl-day32 IS INITIAL.
          l_awart32 = it_excl-day32.
        ENDIF.
        IF NOT it_excl-day33 IS INITIAL.
          l_awart33 = it_excl-day33.
        ENDIF.
        IF NOT it_excl-day34 IS INITIAL.
          l_awart34 = it_excl-day34.
        ENDIF.

        IF NOT it_excl-day41 IS INITIAL.
          l_awart41 = it_excl-day41.
        ENDIF.
        IF NOT it_excl-day42 IS INITIAL.
          l_awart42 = it_excl-day42.
        ENDIF.
        IF NOT it_excl-day43 IS INITIAL.
          l_awart43 = it_excl-day43.
        ENDIF.
        IF NOT it_excl-day44 IS INITIAL.
          l_awart44 = it_excl-day44.
        ENDIF.

        IF NOT it_excl-day51 IS INITIAL.
          l_awart51 = it_excl-day51.
        ENDIF.
        IF NOT it_excl-day52 IS INITIAL.
          l_awart52 = it_excl-day52.
        ENDIF.
        IF NOT it_excl-day53 IS INITIAL.
          l_awart53 = it_excl-day53.
        ENDIF.
        IF NOT it_excl-day54 IS INITIAL.
          l_awart54 = it_excl-day54.
        ENDIF.

        IF NOT it_excl-day61 IS INITIAL.
          l_awart61 = it_excl-day61.
        ENDIF.
        IF NOT it_excl-day62 IS INITIAL.
          l_awart62 = it_excl-day62.
        ENDIF.
        IF NOT it_excl-day63 IS INITIAL.
          l_awart63 = it_excl-day63.
        ENDIF.
        IF NOT it_excl-day64 IS INITIAL.
          l_awart64 = it_excl-day64.
        ENDIF.

        IF NOT it_excl-day71 IS INITIAL.
          l_awart71 = it_excl-day71.
        ENDIF.
        IF NOT it_excl-day72 IS INITIAL.
          l_awart72 = it_excl-day72.
        ENDIF.
        IF NOT it_excl-day73 IS INITIAL.
          l_awart73 = it_excl-day73.
        ENDIF.
        IF NOT it_excl-day74 IS INITIAL.
          l_awart74 = it_excl-day74.
        ENDIF.

      ELSE.
        CALL FUNCTION 'POPUP_TO_INFORM'
           EXPORTING
             titel = 'Time Sheet'
             txt1  = 'The employee does not report to the administrator'
             txt2  = it_cat2-pernr
             txt3  = l_sachz
             txt4  = it_excl-ot1.
        w_flag = 'X'.
        EXIT.
      ENDIF.
    ELSE.
      it_cat2-pernr = w_pernr.
      it_cat2-name = w_name.
      l_first_line = 'X'.
      IF NOT it_excl-day11 IS INITIAL.
        lt_type-awart = l_awart11.
        lt_type-day = '1'.
        lt_type-value = it_excl-day11.
        APPEND lt_type.
        CLEAR: lt_type.
      ENDIF.
      IF NOT it_excl-day12 IS INITIAL.
        lt_type-awart = l_awart12.
        lt_type-day = '1'.
        lt_type-value = it_excl-day12.
        APPEND lt_type.
        CLEAR: lt_type.
      ENDIF.
      IF NOT it_excl-day13 IS INITIAL.
        lt_type-awart = l_awart13.
        lt_type-day = '1'.
        lt_type-value = it_excl-day13.
        APPEND lt_type.
        CLEAR: lt_type.
      ENDIF.
      IF NOT it_excl-day14 IS INITIAL.
        lt_type-awart = l_awart14.
        lt_type-day = '1'.
        lt_type-value = it_excl-day14.
        APPEND lt_type.
        CLEAR: lt_type.
      ENDIF.

      IF NOT it_excl-day21 IS INITIAL.
        lt_type-awart = l_awart21.
        lt_type-day = '2'.
        lt_type-value = it_excl-day21.
        APPEND lt_type.
        CLEAR: lt_type.
      ENDIF.
      IF NOT it_excl-day22 IS INITIAL.
        lt_type-awart = l_awart22.
        lt_type-day = '2'.
        lt_type-value = it_excl-day22.
        APPEND lt_type.
        CLEAR: lt_type.
      ENDIF.
      IF NOT it_excl-day23 IS INITIAL.
        lt_type-awart = l_awart23.
        lt_type-day = '2'.
        lt_type-value = it_excl-day23.
        APPEND lt_type.
        CLEAR: lt_type.
      ENDIF.
      IF NOT it_excl-day24 IS INITIAL.
        lt_type-awart = l_awart24.
        lt_type-day = '2'.
        lt_type-value = it_excl-day24.
        APPEND lt_type.
        CLEAR: lt_type.
      ENDIF.

      IF NOT it_excl-day31 IS INITIAL.
        lt_type-awart = l_awart31.
        lt_type-day = '3'.
        lt_type-value = it_excl-day31.
        APPEND lt_type.
        CLEAR: lt_type.
      ENDIF.
      IF NOT it_excl-day32 IS INITIAL.
        lt_type-awart = l_awart32.
        lt_type-day = '3'.
        lt_type-value = it_excl-day32.
        APPEND lt_type.
        CLEAR: lt_type.
      ENDIF.
      IF NOT it_excl-day33 IS INITIAL.
        lt_type-awart = l_awart33.
        lt_type-day = '3'.
        lt_type-value = it_excl-day33.
        APPEND lt_type.
        CLEAR: lt_type.
      ENDIF.
      IF NOT it_excl-day34 IS INITIAL.
        lt_type-awart = l_awart34.
        lt_type-day = '3'.
        lt_type-value = it_excl-day34.
        APPEND lt_type.
        CLEAR: lt_type.
      ENDIF.

      IF NOT it_excl-day41 IS INITIAL.
        lt_type-awart = l_awart41.
        lt_type-day = '4'.
        lt_type-value = it_excl-day41.
        APPEND lt_type.
        CLEAR: lt_type.
      ENDIF.
      IF NOT it_excl-day42 IS INITIAL.
        lt_type-awart = l_awart42.
        lt_type-day = '4'.
        lt_type-value = it_excl-day42.
        APPEND lt_type.
        CLEAR: lt_type.
      ENDIF.
      IF NOT it_excl-day43 IS INITIAL.
        lt_type-awart = l_awart43.
        lt_type-day = '4'.
        lt_type-value = it_excl-day43.
        APPEND lt_type.
        CLEAR: lt_type.
      ENDIF.
      IF NOT it_excl-day44 IS INITIAL.
        lt_type-awart = l_awart44.
        lt_type-day = '4'.
        lt_type-value = it_excl-day44.
        APPEND lt_type.
        CLEAR: lt_type.
      ENDIF.

      IF NOT it_excl-day51 IS INITIAL.
        lt_type-awart = l_awart51.
        lt_type-day = '5'.
        lt_type-value = it_excl-day51.
        APPEND lt_type.
        CLEAR: lt_type.
      ENDIF.
      IF NOT it_excl-day52 IS INITIAL.
        lt_type-awart = l_awart52.
        lt_type-day = '5'.
        lt_type-value = it_excl-day52.
        APPEND lt_type.
        CLEAR: lt_type.
      ENDIF.
      IF NOT it_excl-day53 IS INITIAL.
        lt_type-awart = l_awart53.
        lt_type-day = '5'.
        lt_type-value = it_excl-day53.
        APPEND lt_type.
        CLEAR: lt_type.
      ENDIF.
      IF NOT it_excl-day54 IS INITIAL.
        lt_type-awart = l_awart54.
        lt_type-day = '5'.
        lt_type-value = it_excl-day54.
        APPEND lt_type.
        CLEAR: lt_type.
      ENDIF.

      IF NOT it_excl-day61 IS INITIAL.
        lt_type-awart = l_awart61.
        lt_type-day = '6'.
        lt_type-value = it_excl-day61.
        APPEND lt_type.
        CLEAR: lt_type.
      ENDIF.
      IF NOT it_excl-day62 IS INITIAL.
        lt_type-awart = l_awart62.
        lt_type-day = '6'.
        lt_type-value = it_excl-day62.
        APPEND lt_type.
        CLEAR: lt_type.
      ENDIF.
      IF NOT it_excl-day63 IS INITIAL.
        lt_type-awart = l_awart63.
        lt_type-day = '6'.
        lt_type-value = it_excl-day63.
        APPEND lt_type.
        CLEAR: lt_type.
      ENDIF.
      IF NOT it_excl-day64 IS INITIAL.
        lt_type-awart = l_awart64.
        lt_type-day = '6'.
        lt_type-value = it_excl-day64.
        APPEND lt_type.
        CLEAR: lt_type.
      ENDIF.

      IF NOT it_excl-day71 IS INITIAL.
        lt_type-awart = l_awart71.
        lt_type-day = '7'.
        lt_type-value = it_excl-day71.
        APPEND lt_type.
        CLEAR: lt_type.
      ENDIF.
      IF NOT it_excl-day72 IS INITIAL.
        lt_type-awart = l_awart72.
        lt_type-day = '7'.
        lt_type-value = it_excl-day72.
        APPEND lt_type.
        CLEAR: lt_type.
      ENDIF.
      IF NOT it_excl-day73 IS INITIAL.
        lt_type-awart = l_awart73.
        lt_type-day = '7'.
        lt_type-value = it_excl-day73.
        APPEND lt_type.
        CLEAR: lt_type.
      ENDIF.
      IF NOT it_excl-day74 IS INITIAL.
        lt_type-awart = l_awart74.
        lt_type-day = '7'.
        lt_type-value = it_excl-day74.
        APPEND lt_type.
        CLEAR: lt_type.
      ENDIF.

      SORT lt_type BY awart day.
      LOOP AT lt_type.
        it_cat2-awart = lt_type-awart.
        CASE lt_type-day.
          WHEN '1'.
            it_cat2-day1 = lt_type-value.
          WHEN '2'.
            it_cat2-day2 = lt_type-value.
          WHEN '3'.
            it_cat2-day3 = lt_type-value.
          WHEN '4'.
            it_cat2-day4 = lt_type-value.
          WHEN '5'.
            it_cat2-day5 = lt_type-value.
          WHEN '6'.
            it_cat2-day6 = lt_type-value.
          WHEN '7'.
            it_cat2-day7 = lt_type-value.
        ENDCASE.
        AT END OF awart.
          APPEND it_cat2.
          CLEAR: it_cat2-day1,it_cat2-day2,it_cat2-day3,it_cat2-day4,
                it_cat2-day5,it_cat2-day6,it_cat2-day7.
        ENDAT.
      ENDLOOP.
    ENDIF.
    CLEAR: l_endda, it_cat2, it_excl, w_total, lt_type[].
  ENDLOOP.
ENDFORM.     "read_excel
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
*FORM DATA_PROCESS_DBC.
*  DATA: W_DATE(10).
*
*  CONSTANTS: C_VARANT LIKE TCATST-VARIANT VALUE 'HMMA'.
**  WRITE: P_PDATE TO W_DATE.
*
*  PERFORM BDC_DYNPRO      USING 'SAPLCATS' '1000'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                'CATSFIELDS-INPUTDATE'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                '=VARI'.
*  PERFORM BDC_FIELD       USING 'TCATST-VARIANT'
*                                C_VARANT.
*  PERFORM BDC_FIELD       USING 'CATSFIELDS-INPUTDATE'
*                                W_DATE.
*
*  PERFORM BDC_DYNPRO      USING 'SAPLCATS' '1000'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                'TCATST-VARIANT'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                '=VARI'.
*  PERFORM BDC_FIELD       USING 'TCATST-VARIANT'
*                                C_VARANT.
*
*  PERFORM BDC_DYNPRO      USING 'SAPLCATS' '3001'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                '=PRPE'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                'TCATS-CATSCELL_SIZE'.
*
*  PERFORM BDC_FIELD       USING 'TCATS-ADDINFOBRI'
*                                'X'.
*  PERFORM BDC_FIELD       USING 'TCATS-DAYTEXT'
*                                 'X'.
*  PERFORM BDC_FIELD       USING 'TCATS-CATSCELL_SIZE'
*                                '10'.
*
*  PERFORM BDC_DYNPRO      USING 'SAPLCATS' '3001'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                '=ENTE'.
*  PERFORM BDC_DYNPRO      USING 'SAPLCATS' '3001'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                'CATSFIELDS-SACHZ'.
*
*  PERFORM BDC_FIELD       USING 'TCATS-TIMECLERK'
*                                 'X'.
*  PERFORM BDC_FIELD       USING 'CATSFIELDS-SBMOD'
*                                 C_VARANT.
*  PERFORM BDC_FIELD       USING 'CATSFIELDS-SACHZ'
*                                W_ADMIN.
*
*  PERFORM BDC_DYNPRO      USING 'SAPLCATS' '1000'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                'TCATST-VARIANT'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                '=PMAL'.
*  PERFORM BDC_FIELD       USING 'TCATST-VARIANT'
*                                C_VARANT.
*
*
*  PERFORM BDC_DYNPRO      USING 'SAPLCATS' '1000'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                'TCATST-VARIANT'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                '=TIME'.
*  PERFORM BDC_FIELD       USING 'TCATST-VARIANT'
*                                C_VARANT.
*
*
*  PERFORM BDC_DYNPRO      USING 'SAPLCATS' '2002'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                               'CATSD-DAY2(02)'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                '/00'.
*  LOOP AT IT_CAT2.
*
*    PERFORM BDC_FIELD       USING 'CATSD-PERNR(01)'
*                                  IT_CAT2-PERNR.
*    PERFORM BDC_FIELD       USING 'CATSD-AWART(01)'
*                                  IT_CAT2-AWART.
*    PERFORM BDC_FIELD       USING 'CATSD-DAY1(01)'
*                                  IT_CAT2-DAY1.
*    PERFORM BDC_FIELD       USING 'CATSD-DAY2(01)'
*                                  IT_CAT2-DAY2.
*    PERFORM BDC_FIELD       USING 'CATSD-DAY3(01)'
*                                  IT_CAT2-DAY3.
*    PERFORM BDC_FIELD       USING 'CATSD-DAY4(01)'
*                                  IT_CAT2-DAY4.
*    PERFORM BDC_FIELD       USING 'CATSD-DAY5(01)'
*                                  IT_CAT2-DAY5.
*    PERFORM BDC_FIELD       USING 'CATSD-DAY6(01)'
*                                  IT_CAT2-DAY6.
*    PERFORM BDC_FIELD       USING 'CATSD-DAY7(01)'
*                                  IT_CAT2-DAY7.
*  ENDLOOP.
*
*
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                '=SAVE'.
*
*  CALL TRANSACTION 'CAT2' USING IT_BDC
*                              MODE W_MODE
*                              UPDATE 'S'
*                              MESSAGES INTO IT_MESS.
*
*  READ TABLE IT_MESS WITH KEY MSGTYP = 'E'.
*  IF SY-SUBRC EQ 0.
*
*    CALL FUNCTION 'POPUP_TO_INFORM'
*         EXPORTING
*              TITEL = 'Time Sheet'
*              TXT1  = 'Error for time sheet entry'
*              TXT2  = IT_MESS-MSGV1
*              TXT3  = IT_MESS-MSGV2
*              TXT4  = IT_MESS-MSGV3.
*  ELSE.
*    MESSAGE S001 WITH 'Successfully processed'.
*  ENDIF.
*
*ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  display_progress_bar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_MESS  text
*----------------------------------------------------------------------*
FORM display_progress_bar USING    p_text.
  DATA: lw_text(50).

  MOVE: p_text TO lw_text.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            text = lw_text.

ENDFORM.                    " display_progress_bar

*---------------------------------------------------------------------*
*       FORM BDC_DYNPRO                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PROGRAM                                                       *
*  -->  DYNPRO                                                        *
*---------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR it_bdc.
  it_bdc-program  = program.
  it_bdc-dynpro   = dynpro.
  it_bdc-dynbegin = 'X'.
  APPEND it_bdc.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM BDC_FIELD                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FNAM                                                          *
*  -->  FVAL                                                          *
*---------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR it_bdc.
  it_bdc-fnam = fnam.
  it_bdc-fval = fval.
  APPEND it_bdc.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_process_bapi.
*  IF R_B1 = 'X'.
  PERFORM data_process_bapi_insert.
*  ELSE.
*    PERFORM DATA_PROCESS_BAPI_CHANGE.
*  ENDIF.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM DATA_PROCESS_BAPI_INSERT                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM data_process_bapi_insert.
  DATA: profile LIKE bapicats6-profile VALUE 'HMMA',
        w_catsrecords_in LIKE bapicats1,
        l_no(1) TYPE n,
        l_date LIKE sy-datum.
  DATA: l_msg(128).
  DATA: l_excel LIKE rlgrap-filename,
        l_fname_len TYPE i.
  DATA: bapiret2 LIKE bapiret2.
  DATA: catsrecords_in LIKE bapicats1.
  DATA: it_catsrecords_in LIKE TABLE OF catsrecords_in WITH HEADER LINE,
        it_catsrecords_out LIKE TABLE OF bapicats2 WITH HEADER LINE,
        it_bapiret2  LIKE TABLE OF bapiret2 WITH HEADER LINE.

  LOOP AT it_cat2.
    l_date = w_fr_date.
    w_catsrecords_in-employeenumber = it_cat2-pernr.
    w_catsrecords_in-abs_att_type = it_cat2-awart.
    w_catsrecords_in-all_day_flag = 'X'.
    IF it_cat2-day1 > 0.
      w_catsrecords_in-workdate = l_date.
      w_catsrecords_in-catshours = it_cat2-day1.
      APPEND w_catsrecords_in TO it_catsrecords_in.
    ENDIF.
    l_date = l_date + 1.
    IF it_cat2-day2 > 0.
      w_catsrecords_in-workdate = l_date.
      w_catsrecords_in-catshours = it_cat2-day2.
      APPEND w_catsrecords_in TO it_catsrecords_in.
    ENDIF.
    l_date = l_date + 1.
    IF it_cat2-day3 > 0.
      w_catsrecords_in-workdate = l_date.
      w_catsrecords_in-catshours = it_cat2-day3.
      APPEND w_catsrecords_in TO it_catsrecords_in.
    ENDIF.
    l_date = l_date + 1.
    IF it_cat2-day4 > 0.
      w_catsrecords_in-workdate = l_date.
      w_catsrecords_in-catshours = it_cat2-day4.
      APPEND w_catsrecords_in TO it_catsrecords_in.
    ENDIF.
    l_date = l_date + 1.
    IF it_cat2-day5 > 0.
      w_catsrecords_in-workdate = l_date.
      w_catsrecords_in-catshours = it_cat2-day5.
      APPEND w_catsrecords_in TO it_catsrecords_in.
    ENDIF.
    l_date = l_date + 1.
    IF it_cat2-day6 > 0.
      w_catsrecords_in-workdate = l_date.
      w_catsrecords_in-catshours = it_cat2-day6.
      APPEND w_catsrecords_in TO it_catsrecords_in.
    ENDIF.
    l_date = l_date + 1.
    IF it_cat2-day7 > 0.
      w_catsrecords_in-workdate = l_date.
      w_catsrecords_in-catshours = it_cat2-day7.
      APPEND w_catsrecords_in TO it_catsrecords_in.
    ENDIF.
*---> CATS: Insert Data Records
    IF NOT it_catsrecords_in[] IS INITIAL.
      CALL FUNCTION 'BAPI_CATIMESHEETMGR_INSERT'
               EXPORTING
                   profile = profile
                   testrun = p_trun
*                  RELEASE_DATA = 'X'
               TABLES
                  catsrecords_in = it_catsrecords_in
                   catsrecords_out = it_catsrecords_out
               return = it_bapiret2.


      IF it_bapiret2[] IS INITIAL.
        APPEND it_cat2 TO it_output.
        w_cn  = w_cn + 1.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
             EXPORTING
                  wait = 'X'.
*      MESSAGE S000 WITH 'Successfully processed' IT_CAT2-PERNR.
      ELSE.

        LOOP AT it_bapiret2 WHERE type = 'E' OR type = 'A'.
          APPEND it_cat2 TO it_output_err.
          CALL FUNCTION 'POPUP_TO_INFORM'
               EXPORTING
                    titel = 'Time Sheet'
                    txt1  = 'Error for time sheet entry'
                    txt2  = it_cat2-pernr
                    txt3  = it_bapiret2-message
                    txt4  = it_bapiret2-message_v1.
          ROLLBACK WORK.
          EXIT.
        ENDLOOP.
        IF sy-subrc = 0.
        ELSE.
          APPEND it_cat2 TO it_output.
          w_cn  = w_cn + 1.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
               EXPORTING
                    wait = 'X'.
        ENDIF.
      ENDIF.
*      READ TABLE IT_BAPIRET2 WITH KEY TYPE = 'S'.
*      IF SY-SUBRC = 0.
*        APPEND IT_CAT2 TO IT_OUTPUT.
*        W_CN  = W_CN + 1.
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*             EXPORTING
*                  WAIT = 'X'.
*
**      MESSAGE S000 WITH 'Successfully processed' IT_CAT2-PERNR.
*      ELSE.
*        LOOP AT IT_BAPIRET2 WHERE TYPE = 'E'
*                              OR TYPE = 'A'
*                              OR TYPE = 'W'.
*          APPEND IT_CAT2 TO IT_OUTPUT_ERR.
*          CALL FUNCTION 'POPUP_TO_INFORM'
*               EXPORTING
*                    TITEL = 'Time Sheet'
*                    TXT1  = 'Error for time sheet entry'
*                    TXT2  = IT_CAT2-PERNR
*                    TXT3  = IT_BAPIRET2-MESSAGE
*                    TXT4  = IT_BAPIRET2-MESSAGE_V1.
*          ROLLBACK WORK.
*          EXIT.
*        ENDLOOP.
*      ENDIF.
    ENDIF.
    REFRESH : it_catsrecords_in, it_bapiret2.
    CLEAR: w_catsrecords_in, it_output, it_output_err.
  ENDLOOP.

  IF w_cn > 0 AND p_trun IS INITIAL.
    CALL FUNCTION 'GUI_DELETE_FILE'
         EXPORTING
              file_name = p_file
         EXCEPTIONS
              failed    = 1
              OTHERS    = 2.
    IF sy-subrc <> 0.

* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    l_fname_len = strlen( p_file ).
    l_fname_len = l_fname_len - 4.
    CONCATENATE p_file+0(l_fname_len) '.xls' INTO l_excel.
    CALL FUNCTION 'GUI_DELETE_FILE'
         EXPORTING
              file_name = l_excel
         EXCEPTIONS
              failed    = 1
              OTHERS    = 2.
    IF sy-subrc <> 0.

* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.
  IF p_trun IS INITIAL.
    CONCATENATE 'Successfully processed record(s):' w_cn INTO l_msg
                SEPARATED BY space.
  ELSE.
    CONCATENATE 'Record(s) will be uploaded successfully:' w_cn
                INTO l_msg SEPARATED BY space.
  ENDIF.
  MESSAGE s001 WITH l_msg.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM DATA_PROCESS_BAPI_CHANGE                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM data_process_bapi_change.
  DATA: profile LIKE bapicats6-profile VALUE 'HMMA',
        w_catsrecords_in LIKE bapicats3,
        l_no(1) TYPE n,
        l_date LIKE sy-datum.

  DATA: bapiret2 LIKE bapiret2.

  DATA: catsrecords_in LIKE bapicats3.

  DATA: it_catsrecords_in LIKE TABLE OF catsrecords_in WITH HEADER LINE,
        it_catsrecords_out LIKE TABLE OF bapicats2 WITH HEADER LINE,
        it_bapiret2  LIKE TABLE OF bapiret2 WITH HEADER LINE.

  LOOP AT it_cat2.
    l_date = w_fr_date.
    w_catsrecords_in-employeenumber = it_cat2-pernr.
    w_catsrecords_in-abs_att_type = it_cat2-awart.
    w_catsrecords_in-all_day_flag = 'X'.
    IF it_cat2-day1 > 0.
      w_catsrecords_in-workdate = l_date.
      w_catsrecords_in-catshours = it_cat2-day1.
      SELECT SINGLE counter INTO w_catsrecords_in-counter
        FROM catsdb
        WHERE pernr = it_cat2-pernr
          AND awart = it_cat2-awart
          AND workdate = l_date
                     AND status <> '60'..
      IF sy-subrc = 0.
        APPEND w_catsrecords_in TO it_catsrecords_in.
      ENDIF.
    ENDIF.
    l_date = l_date + 1.
    IF it_cat2-day2 > 0.
      w_catsrecords_in-workdate = l_date.
      w_catsrecords_in-catshours = it_cat2-day2.
      SELECT SINGLE counter INTO w_catsrecords_in-counter
        FROM catsdb
        WHERE pernr = it_cat2-pernr
          AND awart = it_cat2-awart
          AND workdate = l_date
                     AND status <> '60'.
      IF sy-subrc = 0.
        APPEND w_catsrecords_in TO it_catsrecords_in.
      ENDIF.
    ENDIF.
    l_date = l_date + 1.
    IF it_cat2-day3 > 0.
      w_catsrecords_in-workdate = l_date.
      w_catsrecords_in-catshours = it_cat2-day3.
      SELECT SINGLE counter INTO w_catsrecords_in-counter
         FROM catsdb
         WHERE pernr = it_cat2-pernr
           AND awart = it_cat2-awart
           AND workdate = l_date
                      AND status <> '60'..
      IF sy-subrc = 0.
        APPEND w_catsrecords_in TO it_catsrecords_in.
      ENDIF.
    ENDIF.
    l_date = l_date + 1.
    IF it_cat2-day4 > 0.
      w_catsrecords_in-workdate = l_date.
      w_catsrecords_in-catshours = it_cat2-day4.
      SELECT SINGLE counter INTO w_catsrecords_in-counter
         FROM catsdb
         WHERE pernr = it_cat2-pernr
           AND awart = it_cat2-awart
           AND workdate = l_date
                      AND status <> '60'..
      IF sy-subrc = 0.
        APPEND w_catsrecords_in TO it_catsrecords_in.
      ENDIF.
    ENDIF.
    l_date = l_date + 1.
    IF it_cat2-day5 > 0.
      w_catsrecords_in-workdate = l_date.
      w_catsrecords_in-catshours = it_cat2-day5.
      SELECT SINGLE counter INTO w_catsrecords_in-counter
         FROM catsdb
         WHERE pernr = it_cat2-pernr
           AND awart = it_cat2-awart
           AND workdate = l_date
                      AND status <> '60'..
      IF sy-subrc = 0.
        APPEND w_catsrecords_in TO it_catsrecords_in.
      ENDIF.
    ENDIF.
    l_date = l_date + 1.
    IF it_cat2-day6 > 0.
      w_catsrecords_in-workdate = l_date.
      w_catsrecords_in-catshours = it_cat2-day6.
      SELECT SINGLE counter INTO w_catsrecords_in-counter
         FROM catsdb
         WHERE pernr = it_cat2-pernr
           AND awart = it_cat2-awart
           AND workdate = l_date
           AND status <> '60'..
      IF sy-subrc = 0.
        APPEND w_catsrecords_in TO it_catsrecords_in.
      ENDIF.
    ENDIF.
    l_date = l_date + 1.
    IF it_cat2-day7 > 0.
      w_catsrecords_in-workdate = l_date.
      w_catsrecords_in-catshours = it_cat2-day7.
      SELECT SINGLE counter INTO w_catsrecords_in-counter
         FROM catsdb
         WHERE pernr = it_cat2-pernr
           AND awart = it_cat2-awart
           AND workdate = l_date
           AND status <> '60'.
      IF sy-subrc = 0.
        APPEND w_catsrecords_in TO it_catsrecords_in.
      ENDIF.
    ENDIF.
*---> CATS: Change Data Records

    CALL FUNCTION 'BAPI_CATIMESHEETMGR_CHANGE'
            EXPORTING
               profile = profile
                  testrun = p_trun
*                  RELEASE_DATA          =
            TABLES
                  catsrecords_in  = it_catsrecords_in
                  catsrecords_out = it_catsrecords_out
                  return = it_bapiret2.

    IF it_bapiret2[] IS INITIAL.
      APPEND it_cat2 TO it_output.
      w_cn  = w_cn + 1.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
                wait = 'X'.

*      MESSAGE S000 WITH 'Successfully processed' IT_CAT2-PERNR.
    ELSE.

      LOOP AT it_bapiret2 WHERE type = 'E' OR type = 'A'.
        APPEND it_cat2 TO it_output_err.
        CALL FUNCTION 'POPUP_TO_INFORM'
             EXPORTING
                  titel = 'Time Sheet'
                  txt1  = 'Error for time sheet entry'
                  txt2  = it_cat2-pernr
                  txt3  = it_bapiret2-message
                  txt4  = it_bapiret2-message_v1.
        ROLLBACK WORK.
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
      ELSE.
        APPEND it_cat2 TO it_output.
        w_cn  = w_cn + 1.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
             EXPORTING
                  wait = 'X'.
      ENDIF.

    ENDIF.

    REFRESH : it_catsrecords_in, it_bapiret2.
    CLEAR: w_catsrecords_in, it_output, it_output_err.
  ENDLOOP.
  MESSAGE s000 WITH 'Successfully processed records: ' w_cn.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  DATA: l_atext LIKE t554t-atext.
  NEW-PAGE LINE-SIZE 198.
*  new-page line-size 198 print on.
*  print-control FONT 8.
*** Print title
  IF p_trun IS INITIAL.
    WRITE : /(35) 'Total Records uploaded: '.
  ELSE.
    WRITE : /(45) 'Total Records will be uploaded: '.
  ENDIF.
  WRITE:  (8) w_cn.
  SKIP.

  WRITE : /(142) sy-uline.
*  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / sy-vline NO-GAP, (8) 'Per No'         NO-GAP,
              sy-vline NO-GAP, (25) '   Name'     NO-GAP,
              sy-vline NO-GAP, (10) 'Type'       NO-GAP,
              sy-vline NO-GAP, (30) 'Description' NO-GAP,
              sy-vline NO-GAP, (7) ' DAY1'        NO-GAP,
              sy-vline NO-GAP, (7) ' DAY2'        NO-GAP,
              sy-vline NO-GAP, (7) ' DAY3'        NO-GAP,
              sy-vline NO-GAP, (7) ' DAY4'        NO-GAP,
              sy-vline NO-GAP, (7) ' DAY5'        NO-GAP,
              sy-vline NO-GAP, (7) ' DAY6'        NO-GAP,
              sy-vline NO-GAP, (7) ' DAY7'        NO-GAP,
              sy-vline NO-GAP, (7) 'Total'       NO-GAP,
              sy-vline NO-GAP.

  WRITE : /(142) sy-uline.
** print content
  LOOP AT it_output.
    SELECT SINGLE atext INTO l_atext
          FROM t554t
          WHERE awart = it_output-awart.

    it_output-total = it_output-day1 + it_output-day2 + it_output-day3
                    + it_output-day4 + it_output-day5 + it_output-day6
                    + it_output-day7.
    WRITE : / sy-vline NO-GAP, (8) it_output-pernr    NO-GAP,
                sy-vline NO-GAP, (25) it_output-name    NO-GAP,
                sy-vline NO-GAP, (10) it_output-awart    NO-GAP,
                sy-vline NO-GAP, (30) l_atext    NO-GAP,

                sy-vline NO-GAP, (7) it_output-day1 NO-GAP,
                sy-vline NO-GAP, (7) it_output-day2 NO-GAP,
                sy-vline NO-GAP, (7) it_output-day3 NO-GAP,
                sy-vline NO-GAP, (7) it_output-day4 NO-GAP,
                sy-vline NO-GAP, (7) it_output-day5 NO-GAP,
                sy-vline NO-GAP, (7) it_output-day6 NO-GAP,
                sy-vline NO-GAP, (7) it_output-day7 NO-GAP,
                sy-vline NO-GAP, (7) it_output-total  NO-GAP,
                sy-vline NO-GAP.
    WRITE : /(142) sy-uline.
  ENDLOOP.
  IF it_output_err[] IS INITIAL.
  ELSE.
    WRITE : /.
    WRITE : /.
    WRITE : /(33) 'Records with error'.
    WRITE : /(142) sy-uline.
    LOOP AT it_output_err.
      SELECT SINGLE atext INTO l_atext
           FROM t554t
           WHERE awart = it_output_err-awart.

      it_output_err-total = it_output_err-day1 + it_output_err-day2
                          + it_output_err-day3 + it_output_err-day4
                          + it_output_err-day5 + it_output_err-day6
                          + it_output_err-day7.
      WRITE : / sy-vline NO-GAP, (8) it_output_err-pernr    NO-GAP,
                  sy-vline NO-GAP, (25) it_output_err-name    NO-GAP,
                  sy-vline NO-GAP, (10) it_output_err-awart    NO-GAP,
                  sy-vline NO-GAP, (30) l_atext    NO-GAP,
                  sy-vline NO-GAP, (7) it_output_err-day1 NO-GAP,
                  sy-vline NO-GAP, (7) it_output_err-day2 NO-GAP,
                  sy-vline NO-GAP, (7) it_output_err-day3 NO-GAP,
                  sy-vline NO-GAP, (7) it_output_err-day4 NO-GAP,
                  sy-vline NO-GAP, (7) it_output_err-day5 NO-GAP,
                  sy-vline NO-GAP, (7) it_output_err-day6 NO-GAP,
                  sy-vline NO-GAP, (7) it_output_err-day7 NO-GAP,
                  sy-vline NO-GAP, (7) it_output_err-total  NO-GAP,
                  sy-vline NO-GAP.
      WRITE : /(142) sy-uline.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " display_data

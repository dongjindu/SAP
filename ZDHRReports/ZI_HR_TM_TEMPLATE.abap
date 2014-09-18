*----------------------------------------------------------------------
* Program ID        : ZI_HR_TE_DOWNLOAD
* Title             : HR Timesheet Template Download
* Created on        : 01/07/2008
* Created by        : Rakesh Gandhi
* Specifications By : Hu,J.T.
* Description       : Program to populate timesheet entry template for
*                     HR Time Entry tool.
*----------------------------------------------------------------------
* CHANGE LOG
*-----------------------------------------------------------------------
* DATE      |  NAME       |      DESC
*-----------------------------------------------------------------------
*01/22/08      IG.MOON      Merge weekly,daily into one template
*05/01/08      IG.MOON      Fix Bug regarding clearing buffer.

REPORT zi_hr_template_download NO STANDARD PAGE HEADING
                               LINE-SIZE  100
                               LINE-COUNT 65
                               MESSAGE-ID zmhr.

INCLUDE ole2incl.
INCLUDE <icon>.                        " icon

TABLES: t526,t549q,sscrfields.

*--------------------------------------------------------------------*
* DATA DECLARATION
*--------------------------------------------------------------------*
DATA g_kokrs TYPE kokrs VALUE 'H201'.
DATA: BEGIN OF it_ws OCCURS 0,
        kokrs TYPE kokrs,
        mosid TYPE mosid,
        schkz	TYPE schkn,
        rtext	TYPE retext,
        anzsh TYPE anzschicht,
      END OF it_ws           .

DATA: BEGIN OF t_t526 OCCURS 0,
        sachx LIKE t526-sachx ,
        sachn LIKE t526-sachn ,
        admncode(100) TYPE c  ,
      END OF t_t526           ,

      BEGIN OF it_pa0001 OCCURS 0,
        sachz LIKE pa0001-sachz  ,
        pernr LIKE pa0001-pernr  ,
        sname LIKE pa0001-sname  ,
        kostl  TYPE kostl,
        schkz  TYPE schkn,
      END OF it_pa0001           ,

      BEGIN OF it_pa0001_1 OCCURS 0,
        pernr LIKE pa0001-pernr    ,
        sname LIKE pa0001-sname    ,
      END OF it_pa0001_1           ,

      BEGIN OF it_pa0000 OCCURS 0,
        pernr LIKE pa0001-pernr  ,
      END OF it_pa0000           ,

      BEGIN OF it_t554t OCCURS 0 ,
        awart LIKE t554t-awart   ,
        atext LIKE t554t-atext   ,
        excp_code(30) TYPE c     ,
      END OF it_t554t            ,

      BEGIN OF it_t554t_1 OCCURS 0,
        excp_code(30) TYPE c      ,
      END OF it_t554t_1           .

DATA: BEGIN OF con_list OCCURS 0,
          werks LIKE t526-werks,
          sachx LIKE t526-sachx,
          sachn LIKE t526-sachn,
      END OF con_list.

DATA: BEGIN OF help_field OCCURS 0.
        INCLUDE STRUCTURE help_value.
DATA: END OF help_field.

DATA: BEGIN OF help_vtab OCCURS 0.
        INCLUDE STRUCTURE help_vtab.
DATA: END OF help_vtab.

DATA: BEGIN OF help_value OCCURS 0,
      value LIKE help_vtab-value,
      END OF help_value.

DATA: BEGIN OF dynpfields OCCURS 3.
        INCLUDE STRUCTURE dynpread.
DATA: END OF dynpfields.

DATA: w_sachn      LIKE t526-sachn            ,
      w_enddate(8) TYPE c VALUE '99991231'    ,
      w_sprsl      LIKE t554t-sprsl VALUE 'E' ,
      w_moabw      LIKE t554t-moabw VALUE '09',
      w_stat2      LIKE pa0000-stat2 VALUE '3',
      w_file       LIKE rlgrap-filename       ,
      w_row        TYPE i                     ,
      w_col        TYPE i                     .

*-Excel download related variables
DATA: t_t526_1 LIKE t_t526 OCCURS 0 WITH HEADER LINE,
      excel    TYPE ole2_object,        " Excel object
      books    TYPE ole2_object,        " workbook
      sheet    TYPE ole2_object,        " worksheet
      cells    TYPE ole2_object.        " Cell

DATA  g_pabrp TYPE  pabrp.

DATA:
  folder TYPE string,
  retval LIKE TABLE OF ddshretval WITH HEADER LINE,
  fldvalue LIKE help_info-fldvalue,
  transdir TYPE text255,
  filename(255),
  trfile(20) TYPE c,
  datatab TYPE TABLE OF text8192 WITH HEADER LINE,
  len TYPE i,
  flen TYPE i.

DATA g_d_w.
DATA $kostl TYPE kostl.

*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECTION-SCREEN SKIP 1.
PARAMETERS: p_start LIKE sy-datum MODIF ID wk..

PARAMETERS: p_weekly LIKE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 33(41) text-x01
                            MODIF ID exl.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP 1.
PARAMETERS: p_oweek  LIKE rlgrap-filename.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 33(41) text-x02
                            MODIF ID exl.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END   OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
SELECT-OPTIONS : s_tmcode  FOR t526-sachx.
SELECTION-SCREEN END   OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b12 WITH FRAME TITLE text-015.
PARAMETERS: p_mnday LIKE sy-datum MODIF ID dl..

SELECTION-SCREEN BEGIN OF BLOCK b12x  WITH FRAME.
PARAMETERS p_not RADIOBUTTON GROUP rax MODIF ID br  USER-COMMAND ucom.
PARAMETERS p_all RADIOBUTTON GROUP rax MODIF ID br.
SELECTION-SCREEN END   OF BLOCK b12x.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT : 05(6) t_day_1 MODIF ID br,
                           15(6) t_day_2 MODIF ID br,
                           25(6) t_day_3 MODIF ID br,
                           35(6) t_day_4 MODIF ID br,
                           45(6) t_day_5 MODIF ID br,
                           55(6) t_day_6 MODIF ID br,
                           65(6) t_day_7 MODIF ID br.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 07.PARAMETERS p1   AS CHECKBOX MODIF ID dl
DEFAULT 'X'.
SELECTION-SCREEN POSITION 17.PARAMETERS p2   AS CHECKBOX MODIF ID dl.
SELECTION-SCREEN POSITION 27.PARAMETERS p3   AS CHECKBOX MODIF ID dl.
SELECTION-SCREEN POSITION 37.PARAMETERS p4   AS CHECKBOX MODIF ID dl.
SELECTION-SCREEN POSITION 47.PARAMETERS p5   AS CHECKBOX MODIF ID dl.
SELECTION-SCREEN POSITION 57.PARAMETERS p6   AS CHECKBOX MODIF ID dl.
SELECTION-SCREEN POSITION 67.PARAMETERS p7   AS CHECKBOX MODIF ID dl.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT : 06(6) t_date_1 MODIF ID br,
                           16(6) t_date_2 MODIF ID br,
                           26(6) t_date_3 MODIF ID br,
                           36(6) t_date_4 MODIF ID br,
                           46(6) t_date_5 MODIF ID br,
                           56(6) t_date_6 MODIF ID br,
                           66(6) t_date_7 MODIF ID br.
SELECTION-SCREEN END OF LINE.
*selection-screen end   of block b1x.

SELECTION-SCREEN END   OF BLOCK b12.

* by ig.moon 01/22/08 {
*selection-screen begin of block b1 with frame title text-001.
*parameters: p_weekly like rlgrap-filename obligatory,
*            p_daily  like rlgrap-filename obligatory,
*            p_oweek  like rlgrap-filename obligatory,
*            p_odaily like rlgrap-filename obligatory.
*selection-screen end   of block b1.
*

PARAMETERS p_debug NO-DISPLAY DEFAULT space.


*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.

  DATA : $date LIKE sy-datum,
         $sat  LIKE sy-datum.

  IF sy-tcode EQ 'ZHR_TM_CREATE_DAILY'.
    g_d_w = 'D'.
    sy-title = 'HR Timesheet Template Download - Daily'.
    $date = sy-datum.
    CONCATENATE
'R:\HR Dept - Public\Time Sheets\Master\New'
'Master Timesheet Daily.xls' INTO p_weekly SEPARATED BY space.

  ELSE.

    CONCATENATE
 'R:\HR Dept - Public\Time Sheets\Master\New'
 'Master Timesheet Weekly.xls' INTO p_weekly SEPARATED BY space.

    g_d_w = 'W'.
    sy-title = 'HR Timesheet Template Download - Weekly'.
    $date = sy-datum + 7.
  ENDIF.

  CALL FUNCTION 'GET_WEEK_INFO_BASED_ON_DATE'
       EXPORTING
            date     = $date
       IMPORTING
            monday   = p_start
            saturday = $sat.

  p_mnday = p_start .
  PERFORM set_all_week.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN 'UCOM'.
      IF p_all EQ 'X'.
        p1 = p2 = p3 = p4 = p5 = p6 = p7 = 'X'.
      ENDIF.
      IF p_not EQ 'X'.
        p1 = p2 = p3 = p4 = p5 = p6 = p7 = space.
      ENDIF.
  ENDCASE.

  PERFORM validate_input.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_weekly.
  PERFORM f4_p_upfile USING p_weekly.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_oweek.
  PERFORM f4_p_dir USING p_oweek.

AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen.

* by ig.moon 01/30/08 {
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_tmcode-low.
  PERFORM tmcode_input_help CHANGING s_tmcode-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_tmcode-high.
  PERFORM tmcode_input_help CHANGING s_tmcode-high.
* }

AT SELECTION-SCREEN ON p_mnday .

  DATA : $date LIKE sy-datum,
         $sat  LIKE sy-datum.

  CALL FUNCTION 'GET_WEEK_INFO_BASED_ON_DATE'
       EXPORTING
            date     = p_mnday
       IMPORTING
            monday   = $date
            saturday = $sat.

  IF $date NE p_mnday.
    MESSAGE e001 WITH 'This date is not for Monday !'.
  ENDIF.

  PERFORM set_all_week.

* by ig.moon 01/22/08 {
*at selection-screen on value-request for p_daily.
*  perform f4_p_upfile using p_daily.
* }

*----------------------------------------------------------------------*
* START OF SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.

  IF p_oweek IS INITIAL.
    MESSAGE s001 WITH 'Please enter the target directory.'.
    EXIT.
  ENDIF.

  IF p1 EQ space AND p2 EQ space AND p3 EQ space AND
     p4 EQ space AND p5 EQ space AND p6 EQ space AND p7 EQ space.
    MESSAGE s001 WITH 'Please select at least one day.'.
    EXIT.
  ENDIF.

  PERFORM get_data.

  IF NOT t_t526[] IS INITIAL.
    PERFORM update_template.
  ELSE.
    MESSAGE e001 WITH text-010.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  f4_p_upfile
*&---------------------------------------------------------------------*
*       Subroutine to provide F4 help for file
*----------------------------------------------------------------------*
*      -->P_FILE File name
*----------------------------------------------------------------------*
FORM f4_p_upfile USING    p_file.

  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            def_path         = p_file  "* File Name
            mask             = ',*.*,*.*.'
            mode             = 'O'
       IMPORTING
            filename         = p_file
       EXCEPTIONS
            inv_winsys       = 1
            no_batch         = 2
            selection_cancel = 3
            selection_error  = 4
            OTHERS           = 5.

ENDFORM.                    " f4_p_upfile
*&---------------------------------------------------------------------*
*&      Form  validate_input
*&---------------------------------------------------------------------*
*       Subroutine to validate user entered values
*----------------------------------------------------------------------*
FORM validate_input.
  DATA: l_len TYPE i.

**-Validation on directory name for Daily timesheet templates
*  clear l_len.
*  l_len = strlen( p_oweek ).
*  l_len = l_len - 1.
*  if p_oweek+l_len(1) <> '\'.
*    message e001 with text-003.
*  endif.

ENDFORM.                    " validate_input
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       Subroutine to get data from DB tables
*----------------------------------------------------------------------*
FORM get_data.

  DATA end_date LIKE p_start.

  CLEAR : it_ws[],it_ws.

*  SELECT b~kokrs  a~mosid a~schkz a~rtext b~anzsh INTO TABLE it_ws
*    FROM t508s AS a
*    INNER JOIN ztco_mh_ws AS b
*    ON  b~kokrs EQ g_kokrs
*    AND b~mosid EQ a~mosid
*    AND b~schkz EQ a~schkz
*  WHERE a~sprsl EQ sy-langu.
*
*  SORT it_ws BY schkz.

  IF g_d_w EQ 'D'.
    end_date = p_start = p_mnday.
  ELSE.
    end_date = p_start + 6.
  ENDIF.

  w_sachn = '%NO LONGER VALID%'.
  SELECT sachx sachn
               FROM t526
               INTO TABLE t_t526
               WHERE sachx IN s_tmcode AND
                     sachn NOT LIKE w_sachn.

  IF NOT t_t526[] IS INITIAL.
    LOOP AT t_t526.
      IF t_t526-sachx IS INITIAL AND t_t526-sachn IS INITIAL.
        DELETE t_t526.
        CONTINUE.
      ENDIF.

      CONCATENATE t_t526-sachx t_t526-sachn INTO t_t526-admncode
                                             SEPARATED BY space.
      MODIFY t_t526 INDEX sy-tabix TRANSPORTING admncode.
    ENDLOOP.

    SELECT a~sachz a~pernr a~sname a~kostl b~schkz
                 FROM pa0001 AS a
                 INNER JOIN pa0007 AS b
                 ON b~pernr EQ a~pernr
*                 AND b~endda EQ a~endda
                 INTO TABLE it_pa0001
                 FOR ALL ENTRIES IN t_t526
                 WHERE a~sachz = t_t526-sachx
* by ig.moon 8/13/2008 {
*                  AND a~endda = w_enddate
                   AND a~begda <= end_date "p_start
                   AND a~endda >= end_date "p_start
                   AND b~begda <= end_date "p_start
                   AND b~endda >= end_date "p_start
* }
* by ig.moon 07/18/2008 {
                       AND abkrs NE '13'.
* }
    SORT it_pa0001 BY sachz pernr.

    CHECK NOT it_pa0001[] IS INITIAL.
    SELECT pernr FROM pa0000
                 INTO TABLE it_pa0000
                 FOR ALL ENTRIES IN it_pa0001
                 WHERE pernr = it_pa0001-pernr
* by ig.moon 8/13/2008 {
*                  AND endda = w_enddate
                   AND begda <= end_date "p_start
                   AND endda >= end_date "p_start
* }
                 AND   stat2 = w_stat2.

    SORT it_pa0000 BY pernr.
    DELETE ADJACENT DUPLICATES FROM it_pa0000 COMPARING pernr.

    LOOP AT it_pa0001.
    READ TABLE it_pa0000 WITH KEY pernr = it_pa0001-pernr BINARY SEARCH.
      IF sy-subrc <> 0.
        DELETE it_pa0001.
      ENDIF.
    ENDLOOP.

    SELECT awart atext FROM t554t
                       INTO TABLE it_t554t
                       WHERE sprsl = w_sprsl.

    SORT it_t554t BY awart atext.
    DELETE ADJACENT DUPLICATES FROM it_t554t COMPARING awart atext.
    LOOP AT it_t554t.
      IF it_t554t-awart IS INITIAL AND it_t554t-atext IS INITIAL.
        DELETE it_t554t.
        CONTINUE.
      ENDIF.

      CONCATENATE it_t554t-awart it_t554t-atext INTO it_t554t-excp_code
                                             SEPARATED BY space.
      MODIFY it_t554t INDEX sy-tabix TRANSPORTING excp_code.
    ENDLOOP.
  ENDIF.      " IF NOT t_t526[] IS INITIAL

* by ig.moon {
  PERFORM get_pay_period.
* }
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  update_template
*&---------------------------------------------------------------------*
*       Subroutine to update Admin Code in Excel Template
*----------------------------------------------------------------------*
FORM update_template.

  DATA :  $today(10),
          $todate LIKE sy-datum.

  DATA end_date LIKE p_start.

  IF g_d_w EQ 'D'.
    end_date = p_start.
  ELSE.
    end_date = p_start + 6.
  ENDIF.

  FIELD-SYMBOLS: <field>.
  DATA  : l_n(1), l_text(10), date_cnt TYPE i.
  t_t526_1[] = t_t526[].

  CLEAR g_pabrp.

  SELECT pabrp FROM  t549q INTO g_pabrp
         WHERE  permo       = '04'
         AND    begda      <= end_date "p_start
         AND    endda      >= end_date. "p_start.
    EXIT.
  ENDSELECT.

*-Start Excel
  PERFORM start_excel USING p_weekly.

  IF g_d_w  EQ 'W'.
    PERFORM update_weekly_template.
  ELSE.

    DO 7 TIMES.
      l_n = sy-index.
      date_cnt = sy-index - 1.
      CONCATENATE 'P' l_n INTO l_text.
      ASSIGN (l_text) TO <field>.
      IF <field> EQ 'X'.
        $todate = p_start + date_cnt.
        PERFORM get_today CHANGING $today $todate.
        TRANSLATE $today TO UPPER CASE.
        PERFORM update_daily_template_new USING $today $todate.
      ENDIF.
    ENDDO.

    CALL METHOD OF excel 'QUIT'.

  ENDIF.

  FREE OBJECT cells.
  FREE OBJECT books.
  FREE OBJECT sheet.
  FREE OBJECT excel.
  IF sy-subrc = 0.
    WRITE: text-014.
  ELSE.
    PERFORM err_hdl.
  ENDIF.


* by ig.moon 01/22/08 {
*  perform update_daily_template.
* }
ENDFORM.                    " update_template
*&---------------------------------------------------------------------*
*&      Form  err_hdl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM err_hdl.
  IF sy-subrc <> 0.
    WRITE: / text-012, sy-subrc.
    EXIT.
  ENDIF.

ENDFORM.                    " err_hdl
*&---------------------------------------------------------------------*
*&      Form  fill_cell
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_row  text
*      -->P_W_COL  text
*      -->P_0      text
*      -->P_T_T526_SACHX  text
*----------------------------------------------------------------------*
FORM fill_cell USING    w_row
                        w_col
                        bold
                        p_char.
  CALL METHOD OF excel 'Cells' = cells EXPORTING #1 = w_row #2 = w_col.
  SET PROPERTY OF cells 'Value' = p_char.
ENDFORM.                    " fill_cell
*&---------------------------------------------------------------------*
*&      Form  file_delete
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM file_delete.
  DATA: l_subrc LIKE sy-subrc      .

  DO 100 TIMES.
    CALL FUNCTION 'WS_QUERY'
         EXPORTING
              filename       = w_file
              query          = 'FE'
         IMPORTING
              return         = l_subrc
         EXCEPTIONS
              inv_query      = 1
              no_batch       = 2
              frontend_error = 3
              OTHERS         = 4.
    IF l_subrc NE 0.
      EXIT.
    ENDIF.
    CALL FUNCTION 'WS_FILE_DELETE'
         EXPORTING
              file = w_file.
  ENDDO.

ENDFORM.                    " file_delete
*&---------------------------------------------------------------------*
*&      Form  update_weekly_template
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM update_weekly_template.

  DATA :$times TYPE i,
        $ix TYPE i,
        $ix_n(2) TYPE n,
        $cnt TYPE i,
        $from TYPE i,
        $clr TYPE i.
  DATA  sft_text(10).

  DATA : $admin_text(100),$dept_text(100).

  LOOP AT t_t526_1.

    PERFORM get_times USING t_t526_1-sachx
                      CHANGING $times.
    $from = 0.

    DO $times TIMES.
      CLEAR w_file.

      $ix = sy-index.

      IF $ix EQ 1.
        CONCATENATE p_oweek '\' 'W_TM_' t_t526_1-sachx INTO w_file.
      ELSE.
        $ix_n = $ix - 1.
        CONCATENATE p_oweek '\' 'W_TM_' t_t526_1-sachx '-' $ix_n INTO
w_file.
      ENDIF.

      PERFORM file_delete.

*-Administrator Code

* Disabled by ig.moon 01/30/2008 {
*      call method of excel 'Worksheets' = sheet exporting #1 = 3.
*      if sy-subrc <> 0.
*        message e001 with text-011.
*      endif.
*      call method of sheet 'Activate'.
*      w_row = 1.

      CLEAR $admin_text.
      LOOP AT t_t526.

*        w_col = 1.
*        perform fill_cell using w_row w_col 0 t_t526-admncode.
*        w_row = w_row + 1.

* by ig.moon 01/30/2008 {
        IF t_t526-sachx EQ t_t526_1-sachx.
          $admin_text = t_t526-admncode.
          $dept_text = t_t526-sachn.
          EXIT.
        ENDIF.
* }
      ENDLOOP.
* }

*-Exception Code
      CALL METHOD OF excel 'Worksheets' = sheet EXPORTING #1 = 2.
      IF sy-subrc <> 0.
        MESSAGE e001 WITH text-011.
      ENDIF.
      CALL METHOD OF sheet 'Activate'.
      w_row = 1.
      LOOP AT it_t554t.
        w_col = 4.
        PERFORM fill_cell USING w_row w_col 0 it_t554t-excp_code.
        w_row = w_row + 1.
      ENDLOOP.      " LOOP AT it_t554t.

*-Employee Name and Number
      CALL METHOD OF excel 'Worksheets' = sheet EXPORTING #1 = 1.
      IF sy-subrc <> 0.
        MESSAGE e001 WITH text-011.
      ENDIF.
      CALL METHOD OF sheet 'Activate'.

* by ig.moon 01/30/2008 {
      PERFORM fill_cell USING : 2 11 0 p_start,
                                2 36 0 $admin_text,
                                3 11 0 $dept_text,
                                3 36 0 g_pabrp.
* }
      ADD 1 TO $from.

      READ TABLE it_pa0001 INDEX $from.
      IF sy-subrc EQ 0.
* by ig.moon 7/10/2008 {
        $kostl = it_pa0001-kostl.
        PERFORM get_kostl_admncode USING t_t526-admncode
                                CHANGING $kostl.
* }
        PERFORM fill_cell USING 3  29 0 $kostl..
      ENDIF.

      w_row = 7.
      $cnt = 0.
      LOOP AT it_pa0001 FROM $from WHERE sachz = t_t526_1-sachx.
        $from = sy-tabix.

        w_col = 1.
        PERFORM fill_cell USING w_row w_col 0 it_pa0001-sname.
        w_col = w_col + 1.
        PERFORM fill_cell USING w_row w_col 0 it_pa0001-pernr.
        w_col = w_col + 1.

* logic for sft text {
        CLEAR sft_text.
        PERFORM get_shift USING it_pa0001-schkz p_start
                       CHANGING it_ws-anzsh .
        IF NOT it_ws-anzsh IS INITIAL.
*   READ TABLE it_ws WITH KEY schkz = it_pa0001-schkz BINARY SEARCH.
*        IF sy-subrc EQ 0.
          CONCATENATE it_pa0001-schkz '^#' it_ws-anzsh  INTO sft_text.
        ELSE.
          MOVE it_pa0001-schkz TO sft_text.
        ENDIF.

        CONDENSE sft_text.
        REPLACE '^' WITH ' ' INTO sft_text .
* }

        PERFORM fill_cell USING w_row w_col 0 sft_text.
        w_row = w_row + 2.

        ADD 1 TO $cnt.
        IF $cnt EQ 40.
          EXIT.
        ENDIF.

      ENDLOOP.      " LOOP AT it_pa0001

      IF $cnt < 40. " AND $times > 1.
        $clr = 40 - $cnt.

        DO $clr TIMES.

          w_col = 1.
          PERFORM fill_cell USING w_row w_col 0 space.
          w_col = w_col + 1.
          PERFORM fill_cell USING w_row w_col 0 space.
          w_col = w_col + 1.
          PERFORM fill_cell USING w_row w_col 0 space.

          w_row = w_row + 2.

        ENDDO.

      ENDIF.

*-Save Excel Sheet
      CALL METHOD OF sheet 'SAVEAS' EXPORTING #1 = w_file.
    ENDDO.

  ENDLOOP.      " LOOP AT t_t526_1

  CALL METHOD OF excel 'QUIT'.
ENDFORM.                    " update_weekly_template
*&---------------------------------------------------------------------*
*&      Form  update_daily_template
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM update_daily_template.

* by ig.moon 01/22/08 {
*-Start Excel
*  perform start_excel using p_daily.
*
*  loop at t_t526_1.
*    clear w_file.
*
*    concatenate p_odaily 'D_' t_t526_1-sachx into w_file.
**-Delete already existing file
*    perform file_delete.
*
**-Administrator Code
*    call method of excel 'Worksheets' = sheet exporting #1 = 3.
*    if sy-subrc <> 0.
*      message e001 with text-011.
*    endif.
*    call method of sheet 'Activate'.
*    w_row = 1.
*    loop at t_t526.
*      w_col = 1.
*      perform fill_cell using w_row w_col 0 t_t526-admncode.
*      w_row = w_row + 1.
*    endloop.
*
**-Exception Code
*    call method of excel 'Worksheets' = sheet exporting #1 = 2.
*    if sy-subrc <> 0.
*      message e001 with text-011.
*    endif.
*    call method of sheet 'Activate'.
*    w_row = 1.
*    loop at it_t554t.
*      w_col = 1.
*      perform fill_cell using w_row w_col 0 it_t554t-excp_code.
*      w_row = w_row + 1.
*    endloop.      " LOOP AT it_t554t.
*
**-Employee Name and Number
*    call method of excel 'Worksheets' = sheet exporting #1 = 1.
*    if sy-subrc <> 0.
*      message e001 with text-011.
*    endif.
*    call method of sheet 'Activate'.
*    w_row = 8.
*    loop at it_pa0001 where sachz = t_t526_1-sachx.
*      w_col = 1.
*      perform fill_cell using w_row w_col 0 it_pa0001-sname.
*      w_col = w_col + 1.
*      perform fill_cell using w_row w_col 0 it_pa0001-pernr.
*      w_row = w_row + 1.
*    endloop.      " LOOP AT it_pa0001
*
**-Save Excel Sheet
*    call method of sheet 'SAVEAS'
*                     exporting #1 = w_file.
*  endloop.      " LOOP AT t_t526_1
*
*  call method of excel 'QUIT'.
*  free object cells.
*  free object books.
*  free object sheet.
*  free object excel.
*  if sy-subrc = 0.
*    write: text-013.
*  else.
*    perform err_hdl.
*  endif.
* }
ENDFORM.                    " update_daily_template
*&---------------------------------------------------------------------*
*&      Form  start_excel
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FILE  text
*----------------------------------------------------------------------*
FORM start_excel USING    p_file.
  DATA $visible TYPE i.

  CREATE OBJECT excel 'EXCEL.APPLICATION'.
  IF sy-subrc <> 0.
    MESSAGE e001 WITH text-008.
  ENDIF.

  $visible = 0.

  SET PROPERTY OF excel  'Visible' = $visible.
  CALL METHOD OF excel 'Workbooks' = books.
  CALL METHOD  OF  books 'OPEN' EXPORTING #1 = p_file.

* by ig.moon 01/22/08 {
  IF sy-subrc NE 0.
    MESSAGE s001 WITH 'Template could not be openned'.
    STOP.
  ENDIF.
* }

ENDFORM.                    " start_excel
*&---------------------------------------------------------------------*
*&      Form  modify_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_screen.

  IF g_d_w EQ 'W'.
    LOOP AT SCREEN.
      IF screen-group1 = 'BR'.
        screen-invisible = 1.
      ENDIF.
      IF screen-group1 = 'WK'.
        screen-invisible = 0.
      ENDIF.
      IF screen-group1 = 'DL'.
        screen-invisible = 1.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF screen-group1 = 'DL'.
        screen-invisible = 0.
      ENDIF.
      IF screen-group1 = 'WK'.
        screen-invisible = 1.
        screen-active = 0.
      ENDIF.
      IF screen-group1 = 'BR'.
        screen-invisible = 0.
        screen-intensified = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " modify_screen
*&---------------------------------------------------------------------*
*&      Form  get_times
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_$TIMES  text
*----------------------------------------------------------------------*
FORM get_times  USING p_sachx
                CHANGING $times.

  DATA : $cnt TYPE i,
         $mod TYPE i.

  LOOP AT it_pa0001 WHERE sachz = p_sachx.
    ADD 1 TO $cnt.
  ENDLOOP.

  $times = $cnt / 40.
  $mod   = $cnt MOD 40.

  IF $mod > 0 AND $mod < 20.
    ADD 1 TO $times.
  ENDIF.

ENDFORM.                    " get_times
*&---------------------------------------------------------------------*
*&      Form  tmcode_input_help
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_S_TMCODE_LOW  text
*----------------------------------------------------------------------*
FORM tmcode_input_help CHANGING p_tmcode.

  DATA j LIKE sy-index.
  CLEAR : con_list.

  SELECT
          werks
          sachx
          sachn
  INTO TABLE con_list
  FROM t526.

  SORT con_list BY werks sachx .
  DELETE con_list WHERE sachx EQ space.
  LOOP AT con_list.
    help_value-value = con_list-sachx.
    APPEND help_value.
    help_value-value = con_list-sachn.
    APPEND help_value.
    help_value-value = con_list-werks.
    APPEND help_value.
  ENDLOOP.

  PERFORM add_fields USING: 'T526'  'SACHX' 'X',
                            'T526'  'SACHN' ' ',
                            'T526'  'WERKS' ' '.

  PERFORM value_help CHANGING j.

  IF j > 0.
    READ TABLE con_list INDEX j.
    p_tmcode = con_list-sachx.
  ENDIF.

*  dynpfields-fieldname  = 'SACHX'.
*  dynpfields-fieldvalue = con_list-sachx.
*  append dynpfields.
*
*  call function 'DYNP_VALUES_UPDATE'
*       exporting
*            dyname     = sy-cprog
*            dynumb     = sy-dynnr
*       tables
*            dynpfields = dynpfields.
*
  CLEAR: dynpfields.
  REFRESH: con_list, help_field, help_vtab, help_value, dynpfields.

ENDFORM.                    " tmcode_input_help
*&---------------------------------------------------------------------*
*&      Form  value_help
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_J  text
*----------------------------------------------------------------------*
FORM value_help CHANGING p_j.

  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
       EXPORTING
            display              = ' '
            title_in_values_list = 'Administrator Code'
       IMPORTING
            index                = p_j
       TABLES
            fields               = help_field
            select_values        = help_vtab
            valuetab             = help_value.

ENDFORM.                    " value_help
*&---------------------------------------------------------------------*
*&      Form  get_pay_period
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_pay_period.

  CLEAR t549q.
  SELECT SINGLE * FROM t549q WHERE begda <= p_start
                        AND endda >= p_start.


ENDFORM.                    " get_pay_period

*---------------------------------------------------------------------*
*       FORM add_fields                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_TABNAME                                                     *
*  -->  P_FIELDNAME                                                   *
*  -->  P_FLAG                                                        *
*---------------------------------------------------------------------*
FORM add_fields USING  p_tabname p_fieldname p_flag.
  help_field-tabname = p_tabname.
  help_field-fieldname = p_fieldname.
  help_field-selectflag = p_flag.
  APPEND help_field.
  CLEAR help_field.
ENDFORM.                    " add_fields
*&---------------------------------------------------------------------*
*&      Form  f4_p_dir
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_OWEEK  text
*----------------------------------------------------------------------*
FORM f4_p_dir USING  p_folder.

  DATA: title TYPE string.

  title = 'Select target folder'(005).
  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title    = title
    CHANGING
      selected_folder = folder
    EXCEPTIONS
      cntl_error      = 1
      error_no_gui    = 2
      OTHERS          = 3.

  CALL FUNCTION 'CONTROL_FLUSH'
       EXCEPTIONS
            cntl_system_error = 1
            cntl_error        = 2
            OTHERS            = 3.

  p_folder = folder.

ENDFORM.                                                    " f4_p_dir
*&---------------------------------------------------------------------*
*&      Form  set_all_week
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_all_week.

  DATA t_day_x LIKE t_day_1.
  DATA t_date_x LIKE t_date_1.
  DATA $day_x(3).
  DATA : $day_1(3) VALUE 'MON',
         $day_2(3) VALUE 'TUE',
         $day_3(3) VALUE 'WED',
         $day_4(3) VALUE 'THU',
         $day_5(3) VALUE 'FRI',
         $day_6(3) VALUE 'SAT',
         $day_7(3) VALUE 'SUN'.

  IF g_d_w EQ 'D'.
    $date = p_mnday.
    DO 7 TIMES VARYING t_day_x FROM t_day_1 NEXT t_day_2
               VARYING t_date_x FROM t_date_1 NEXT t_date_2
               VARYING $day_x FROM $day_1 NEXT $day_2 .
      WRITE: $date TO t_day_x(5) MM/DD/YYYY.
      t_date_x = $day_x.
      ADD 1 TO $date.
    ENDDO.
  ENDIF.

ENDFORM.                    " set_all_week
*&---------------------------------------------------------------------*
*&      Form  update_daily_template_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_daily_template_new USING $today $todate.

  DATA :$times TYPE i,
        $ix TYPE i,
        $ix_n(2) TYPE n,
        $cnt TYPE i,
        $from TYPE i,
        $clr TYPE i,
        $format_day(10).
  DATA  sft_text(10) .

  WRITE $todate TO $format_day MMDDYY.
  CONCATENATE $format_day(4) '20' $format_day+4(2) INTO $format_day.
  DATA : $admin_text(100),$dept_text(100).

  LOOP AT t_t526_1.

    PERFORM get_times USING t_t526_1-sachx
                      CHANGING $times.
    $from = 0.

    DO $times TIMES.
      CLEAR w_file.

      $ix = sy-index.

      IF $ix EQ 1.
        CONCATENATE p_oweek '\' t_t526_1-sachx '_' $today(3) '_'
        $format_day INTO w_file.
      ELSE.
        $ix_n = $ix - 1.
        CONCATENATE p_oweek '\' t_t526_1-sachx  '_' $today(3) '_'
        $format_day '-' $ix_n INTO w_file.
      ENDIF.

      PERFORM file_delete.

      CLEAR $admin_text.
      LOOP AT t_t526.

        IF t_t526-sachx EQ t_t526_1-sachx.
          $admin_text = t_t526-admncode.
          $dept_text =  t_t526-sachn.
          EXIT.
        ENDIF.
      ENDLOOP.

*-Exception Code
      CALL METHOD OF excel 'Worksheets' = sheet EXPORTING #1 = 2.
      IF sy-subrc <> 0.
        MESSAGE e001 WITH text-011.
      ENDIF.
      CALL METHOD OF sheet 'Activate'.
      w_row = 1.
      LOOP AT it_t554t.
        w_col = 4.
        PERFORM fill_cell USING w_row w_col 0 it_t554t-excp_code.
        w_row = w_row + 1.
      ENDLOOP.      " LOOP AT it_t554t.

*-Employee Name and Number
      CALL METHOD OF excel 'Worksheets' = sheet EXPORTING #1 = 1.
      IF sy-subrc <> 0.
        MESSAGE e001 WITH text-011.
      ENDIF.
      CALL METHOD OF sheet 'Activate'.

      PERFORM fill_cell USING : 2 3  0 p_start,
                                3 3  0 $dept_text,
                                5 3  0 $admin_text,
                                6 3  0 g_pabrp,
                                7 4  0 $today,
                                8 4  0 $todate.
      ADD 1 TO $from.

      READ TABLE it_pa0001 INDEX $from.
      IF sy-subrc EQ 0.

* by ig.moon 7/10/2008 {
        $kostl = it_pa0001-kostl.
        PERFORM get_kostl_admncode USING t_t526-admncode
                                CHANGING $kostl.
* }

        PERFORM fill_cell USING 4  3 0 $kostl.
      ENDIF.

      w_row = 10.
      $cnt = 0.
      LOOP AT it_pa0001 FROM $from WHERE sachz = t_t526_1-sachx.
        $from = sy-tabix.

        w_col = 1.
        PERFORM fill_cell USING w_row w_col 0 it_pa0001-sname.
        w_col = w_col + 1.
        PERFORM fill_cell USING w_row w_col 0 it_pa0001-pernr.
        w_col = w_col + 1.


* logic for sft text {
        CLEAR sft_text.
        PERFORM get_shift USING it_pa0001-schkz p_start
                       CHANGING it_ws-anzsh .
        IF NOT it_ws-anzsh IS INITIAL.
*   READ TABLE it_ws WITH KEY schkz = it_pa0001-schkz BINARY SEARCH.
*        IF sy-subrc EQ 0.
          CONCATENATE it_pa0001-schkz '^#' it_ws-anzsh  INTO sft_text.
        ELSE.
          MOVE it_pa0001-schkz TO sft_text.
        ENDIF.
        CONDENSE sft_text.
        REPLACE '^' WITH ' ' INTO sft_text .
* }
        PERFORM fill_cell USING w_row w_col 0 sft_text.
        w_row = w_row + 2.

        ADD 1 TO $cnt.
        IF $cnt EQ 40.
          EXIT.
        ENDIF.

      ENDLOOP.      " LOOP AT it_pa0001

      IF $cnt < 40. " and $times > 1.
        $clr = 40 - $cnt.

        DO $clr TIMES.

          w_col = 1.
          PERFORM fill_cell USING w_row w_col 0 space.
          w_col = w_col + 1.
          PERFORM fill_cell USING w_row w_col 0 space.
          w_col = w_col + 1.
          PERFORM fill_cell USING w_row w_col 0 space.

          w_row = w_row + 2.

        ENDDO.

      ENDIF.

*-Save Excel Sheet
      CALL METHOD OF sheet 'SAVEAS' EXPORTING #1 = w_file.
    ENDDO.

  ENDLOOP.      " LOOP AT t_t526_1

ENDFORM.                    " update_daily_template_new
*&---------------------------------------------------------------------*
*&      Form  get_today
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_today CHANGING today todate.

  CALL FUNCTION 'DATE_TO_DAY'
       EXPORTING
            date    = todate
       IMPORTING
            weekday = today.

  TRANSLATE today TO UPPER CASE.

ENDFORM.                    " get_today
*&---------------------------------------------------------------------*
*&      Form  get_kostl_admncode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_T526_ADMNCODE  text
*      <--P_$KOSTL  text
*----------------------------------------------------------------------*
FORM get_kostl_admncode USING    p_admncode
                        CHANGING p_kostl.


ENDFORM.                    " get_kostl_admncode
*&---------------------------------------------------------------------*
*&      Form  get_shift
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_PA0001_SCHKZ  text
*      -->P_P_START  text
*      <--P_IT_WS_ANZSH  text
*----------------------------------------------------------------------*
FORM get_shift USING    p_schkz
                        p_date
               CHANGING p_anzsh.

  DATA $tprog TYPE  tprog.

  CALL FUNCTION 'Z_CO_GET_DWS_IG'
       EXPORTING
            schkz                          = p_schkz
            datum                          = p_date
       IMPORTING
            tprog                          = $tprog
       EXCEPTIONS
            not_found_work_schedule_rules  = 1
            invalid_date                   = 2
            not_found_period_work_schedule = 3
            OTHERS                         = 4.

  IF sy-subrc <> 0.
    $tprog = p_schkz.
  ENDIF.

  CASE $tprog.
    WHEN '0002' OR '1003' OR '1002'.
      p_anzsh = '2'.
    WHEN OTHERS.
      p_anzsh = '1'.
  ENDCASE.

ENDFORM.                    " get_shift

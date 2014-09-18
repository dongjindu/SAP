
************************************************************************
* Program Name      : ZAHR_CAT2_D
* Author            : Furong, Wang
* Creation Date     : 03/2008
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       : Upload timesheet daily basis
*
* Modification Logs
* Date       Developer    RequestNo    Description
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
      shift(7),
      ot1(8),
      day11(8),
      day12(8),
      day13(8),
      day14(8),
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
        l_awart14 LIKE it_excl-day11.

  DATA: BEGIN OF lt_type OCCURS 0,
        awart LIKE it_cat2-awart,
        day(1),
        value LIKE it_excl-day11,
        END OF lt_type.

  FIELD-SYMBOLS: <fs>.
  DATA: lt_pa0001 LIKE TABLE OF pa0001 WITH HEADER LINE.

*  READ TABLE IT_EXCL INDEX 4.
*  L_SHIFT = IT_EXCL-OT1+0(1).
*  CASE L_SHIFT.
*    WHEN '1'.
*      L_OTCODE = '1001'.
*    WHEN '2'.
*      L_OTCODE = '1003'.
*    WHEN '3'.
*      L_OTCODE = '1004'.
*  ENDCASE.

  READ TABLE it_excl INDEX 5.

* by ig.moon 2/9/2009 {

*  l_sachz = it_excl-shift+0(3).
  data tmp_4(4).
  tmp_4 = it_excl-shift+0(4).
  replace '"' with '' into tmp_4.
  condense tmp_4.
  l_sachz = tmp_4(3).

* }

  READ TABLE it_excl INDEX 8.
  CONCATENATE '20' it_excl-ot1+6(2) it_excl-ot1+0(2)
              it_excl-ot1+3(2) INTO l_date_c.

  WRITE: l_date_c TO w_fr_date.

*  CALL FUNCTION 'HR_GB_DAY_RELATIVE_TO_DATE'
*   EXPORTING
*     DATE                  = SY-DATUM
*     DAY_IN_WEEK           = '7'
*     BEFORE_OR_AFTER       = '-'
*   IMPORTING
*     NEW_DATE              = L_TO_DAT
**   DAY_TEXT              =
*            .
*  L_TO_DAT = L_TO_DAT - 7.
*
*  IF W_FR_DATE <= L_TO_DAT.
*    MESSAGE I001 WITH 'Date entry error'.
*    W_FLAG = 'X'.
*    EXIT.
*  ENDIF.
*
*  L_TO_DAT = SY-DATUM - 7.
*  IF W_FR_DATE <> L_TO_DAT.
*    CALL FUNCTION 'POPUP_TO_DECIDE'
*      EXPORTING
*        TEXTLINE1               = 'Entry date should be 7 days ago'
*        TEXT_OPTION1            = 'Continue to process'
*        TEXT_OPTION2            = 'Back'
*        TITEL                   = 'Warning'
**   START_COLUMN            = 25
**   START_ROW               = 6
**   CANCEL_DISPLAY          = 'X'
*        IMPORTING
*        ANSWER                  = L_ANS
*            .
*    IF L_ANS <> '1'.
*      W_FLAG = 'X'.
*      EXIT.
*    ENDIF.
*  ENDIF.

  IF sy-uname = '101457'. " or SY-UNAME = '100553'.
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
        AND endda >= sy-datum.
  ENDIF.

  READ TABLE it_excl INDEX 11.
  w_pernr = it_excl-pernr.
  w_name = it_excl-name.
  l_first_line = 'X'.
  LOOP AT it_excl FROM 11.
    IF it_excl-pernr IS INITIAL AND l_first_line = 'X'.
      CONTINUE.
    ENDIF.
    IF l_first_line = 'X'.
      it_cat2-pernr = it_excl-pernr.
      it_cat2-name = it_excl-name.
      w_pernr = it_excl-pernr.
      w_name = it_excl-name.
      CLEAR: l_first_line.

      l_shift = it_excl-shift+6(1).
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

      SORT lt_type BY awart.
      LOOP AT lt_type.
        it_cat2-awart = lt_type-awart.
*        CASE LT_TYPE-DAY.
*          WHEN '1'.
        it_cat2-day1 = lt_type-value.
*          WHEN '2'.
*            IT_CAT2-DAY2 = LT_TYPE-VALUE.
*          WHEN '3'.
*            IT_CAT2-DAY3 = LT_TYPE-VALUE.
*          WHEN '4'.
*            IT_CAT2-DAY4 = LT_TYPE-VALUE.
*          WHEN '5'.
*            IT_CAT2-DAY5 = LT_TYPE-VALUE.
*          WHEN '6'.
*            IT_CAT2-DAY6 = LT_TYPE-VALUE.
*          WHEN '7'.
*            IT_CAT2-DAY7 = LT_TYPE-VALUE.
*        ENDCASE.
        AT END OF awart.
          APPEND it_cat2.
          CLEAR: it_cat2-day1.
        ENDAT.
      ENDLOOP.
    ENDIF.
    CLEAR: l_endda, it_cat2, it_excl, w_total, lt_type[].
  ENDLOOP.
ENDFORM.     "read_excel
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
  DATA: bapiret2 LIKE bapiret2.
  DATA: l_excel LIKE rlgrap-filename,
        l_fname_len TYPE i.
  DATA: catsrecords_in LIKE bapicats1.
  DATA: it_catsrecords_in LIKE TABLE OF catsrecords_in WITH HEADER LINE,
        it_catsrecords_out LIKE TABLE OF bapicats2 WITH HEADER LINE,
        it_bapiret2  LIKE TABLE OF bapiret2 WITH HEADER LINE.

  l_date = w_fr_date.
  LOOP AT it_cat2.
    w_catsrecords_in-employeenumber = it_cat2-pernr.
    w_catsrecords_in-abs_att_type = it_cat2-awart.
    w_catsrecords_in-all_day_flag = 'X'.
    IF it_cat2-day1 > 0.
      w_catsrecords_in-workdate = l_date.
      w_catsrecords_in-catshours = it_cat2-day1.
      APPEND w_catsrecords_in TO it_catsrecords_in.
*    ENDIF.
*---> CATS: Insert Data Records
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

  NEW-PAGE LINE-SIZE 156.
*  new-page line-size 198 print on.
*  print-control FONT 8.
*** Print title
  IF p_trun IS INITIAL.
    WRITE : /(35) 'Total Records uploaded '.
    WRITE :(70) w_fr_date MM/DD/YY.
  ELSE.
    WRITE : /(35) 'Total Records will be uploaded: '.
    WRITE : (75) w_fr_date MM/DD/YY.
  ENDIF.
  WRITE:  (8) w_cn.
  SKIP.

  WRITE : /(94) sy-uline.
*  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / sy-vline NO-GAP, (8) 'Per No'         NO-GAP,
              sy-vline NO-GAP, (25) '   Name'     NO-GAP,
              sy-vline NO-GAP, (10) 'Type'       NO-GAP,
              sy-vline NO-GAP, (30) 'Description' NO-GAP,

              sy-vline NO-GAP, (7) ' DAY'        NO-GAP,
*              SY-VLINE NO-GAP, (7) ' DAY2'        NO-GAP,
*              SY-VLINE NO-GAP, (7) ' DAY3'        NO-GAP,
*              SY-VLINE NO-GAP, (7) ' DAY4'        NO-GAP,
*              SY-VLINE NO-GAP, (7) ' DAY5'        NO-GAP,
*              SY-VLINE NO-GAP, (7) ' DAY6'        NO-GAP,
*              SY-VLINE NO-GAP, (7) ' DAY7'        NO-GAP,
              sy-vline NO-GAP, (7) 'Total'       NO-GAP,
              sy-vline NO-GAP.

  WRITE : /(94) sy-uline.
** print content
  LOOP AT it_output.
    SELECT SINGLE atext INTO l_atext
        FROM t554t
        WHERE awart = it_output-awart.

    it_output-total = it_output-day1.
*    IT_OUTPUT-TOTAL = IT_OUTPUT-DAY1 + IT_OUTPUT-DAY2 + IT_OUTPUT-DAY3
*                    + IT_OUTPUT-DAY4 + IT_OUTPUT-DAY5 + IT_OUTPUT-DAY6
*                    + IT_OUTPUT-DAY7.
    WRITE : / sy-vline NO-GAP, (8) it_output-pernr    NO-GAP,
                sy-vline NO-GAP, (25) it_output-name    NO-GAP,
                sy-vline NO-GAP, (10) it_output-awart    NO-GAP,
                sy-vline NO-GAP, (30) l_atext    NO-GAP,

                sy-vline NO-GAP, (7) it_output-day1 NO-GAP,
*                SY-VLINE NO-GAP, (7) IT_OUTPUT-DAY2 NO-GAP,
*                SY-VLINE NO-GAP, (7) IT_OUTPUT-DAY3 NO-GAP,
*                SY-VLINE NO-GAP, (7) IT_OUTPUT-DAY4 NO-GAP,
*                SY-VLINE NO-GAP, (7) IT_OUTPUT-DAY5 NO-GAP,
*                SY-VLINE NO-GAP, (7) IT_OUTPUT-DAY6 NO-GAP,
*                SY-VLINE NO-GAP, (7) IT_OUTPUT-DAY7 NO-GAP,
                sy-vline NO-GAP, (7) it_output-total  NO-GAP,
                sy-vline NO-GAP.
    WRITE : /(94) sy-uline.
  ENDLOOP.
  IF it_output_err[] IS INITIAL.
  ELSE.
    WRITE : /.
    WRITE : /.
    WRITE : /(35) 'Records with error'.
    WRITE : (70) w_fr_date MM/DD/YY.
    WRITE : /(94) sy-uline.
    LOOP AT it_output_err.
      it_output_err-total = it_output_err-day1.
*      IT_OUTPUT_ERR-TOTAL = IT_OUTPUT_ERR-DAY1 + IT_OUTPUT_ERR-DAY2
*                          + IT_OUTPUT_ERR-DAY3 + IT_OUTPUT_ERR-DAY4
*                          + IT_OUTPUT_ERR-DAY5 + IT_OUTPUT_ERR-DAY6
*                          + IT_OUTPUT_ERR-DAY7.
      WRITE : / sy-vline NO-GAP, (8) it_output_err-pernr    NO-GAP,
                  sy-vline NO-GAP, (25) it_output_err-name    NO-GAP,
                  sy-vline NO-GAP, (10) it_output_err-awart    NO-GAP,
                  sy-vline NO-GAP, (30) l_atext    NO-GAP,
                  sy-vline NO-GAP, (7) it_output_err-day1 NO-GAP,
*                  SY-VLINE NO-GAP, (7) IT_OUTPUT_ERR-DAY2 NO-GAP,
*                  SY-VLINE NO-GAP, (7) IT_OUTPUT_ERR-DAY3 NO-GAP,
*                  SY-VLINE NO-GAP, (7) IT_OUTPUT_ERR-DAY4 NO-GAP,
*                  SY-VLINE NO-GAP, (7) IT_OUTPUT_ERR-DAY5 NO-GAP,
*                  SY-VLINE NO-GAP, (7) IT_OUTPUT_ERR-DAY6 NO-GAP,
*                  SY-VLINE NO-GAP, (7) IT_OUTPUT_ERR-DAY7 NO-GAP,
                  sy-vline NO-GAP, (7) it_output_err-total  NO-GAP,
                  sy-vline NO-GAP.
      WRITE : /(94) sy-uline.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " display_data

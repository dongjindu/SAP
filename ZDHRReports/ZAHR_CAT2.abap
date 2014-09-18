************************************************************************
* Program Name      : ZAHR_CAT2
* Author            : Furong, Wang
* Creation Date     : 08/2007
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       : Module Cost Update
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT zahr_cat2 NO STANDARD PAGE HEADING
                LINE-SIZE  100
                LINE-COUNT 65
                MESSAGE-ID zmhr.
*TABLES: PA0001.
DATA: BEGIN OF it_file OCCURS 0,
      name(20),
*      INIT(8),
      pernr(8),
      awart(10),
      day1(8),
      day2(8),
      day3(8),
      day4(8),
      day5(8),
      day6(8),
      day7(8),
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

DATA: w_cn TYPE i,
      w_flag(1).
DATA: it_excl             LIKE TABLE OF it_file       WITH HEADER LINE,
      it_msg              LIKE TABLE OF bdcmsgcoll    WITH HEADER LINE.

DATA : BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA : END OF it_bdc.
DATA : BEGIN OF it_mess OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA : END OF it_mess.
DATA : w_mode LIKE ctu_params-dismode VALUE 'E'. "'E'. "A-display 'N'
DATA: w_fr_date LIKE sy-datum.
*      W_DATE LIKE SY-DATUM.
DATA: w_filety LIKE rlgrap-filetype VALUE 'DAT'.
DATA: w_admin LIKE catsfields-sachz.

*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
*SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-100.
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN POSITION 1.
*PARAMETERS: r_b1 RADIOBUTTON GROUP grp DEFAULT 'X' USER-COMMAND ucom.
*SELECTION-SCREEN COMMENT 3(20) text-201.
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN POSITION 1.
*PARAMETERS: r_b2 RADIOBUTTON GROUP grp.
*SELECTION-SCREEN COMMENT 3(20) text-202.
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN END   OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-101.
PARAMETERS:
  p_file  LIKE rlgrap-filename OBLIGATORY.
*  P_FILETY LIKE RLGRAP-FILETYPE DEFAULT 'DAT', " MODIF ID md1,
*  P_PDATE LIKE SY-DATUM  OBLIGATORY,
PARAMETERS:  p_trun(1).
*  SELECTION-SCREEN BEGIN OF LINE.
*  SELECTION-SCREEN COMMENT (40) TEXT-002. "for field P_TRUN.
**   SELECTION-SCREEN POSITION POS_LOW.

*  SELECTION-SCREEN end OF LINE.
SELECTION-SCREEN END   OF BLOCK b1.

*SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-100.
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN POSITION 1.
*PARAMETERS: R_B1 RADIOBUTTON GROUP GRP DEFAULT 'X' USER-COMMAND UCOM.
*SELECTION-SCREEN COMMENT 5(30) TEXT-201.
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN POSITION 1.
*PARAMETERS: R_B2 RADIOBUTTON GROUP GRP.
*SELECTION-SCREEN COMMENT 5(30) TEXT-202.
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN END   OF BLOCK B2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM at_sel_screen_on_value_request USING p_file 'O'.

** Furong on 08/31/12 for deactivate the unused program
Initialization.
Leave program.
** End on 08/31/12

START-OF-SELECTION.
  PERFORM upload_process.
  IF it_excl[] IS INITIAL.
    EXIT.
  ENDIF.
  PERFORM read_excel.
  IF w_flag = 'X'.
    CLEAR:  w_flag.
    EXIT.
  ENDIF.
*  PERFORM DATA_PROCESS_DBC.
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
  ELSE.
* IF SY-SUBRC = 01.    "// Does not work, why ???
*   MESSAGELINE = 'Not supported'.
* ENDIF.
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
  IF sy-subrc <> 0.

* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


*  CALL FUNCTION 'WS_UPLOAD'
*       EXPORTING
*            codepage                = ' '
*            filename                = p_file
*            filetype                = p_filety
**           HEADLEN                 = ' '
**           LINE_EXIT               = ' '
**           TRUNCLEN                = ' '
**           USER_FORM               = ' '
**           USER_PROG               = ' '
**      IMPORTING
**           FILELENGTH              =
*       TABLES
*            data_tab                = it_excl
*      EXCEPTIONS
*           conversion_error        = 1
*           file_open_error         = 2
*           file_read_error         = 3
*           invalid_table_width     = 4
*           invalid_type            = 5
*           no_batch                = 6
*           unknown_error           = 7
*           gui_refuse_filetransfer = 8
*           customer_error          = 9
*           OTHERS                  = 10
*            .
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
        l_text(30).
  FIELD-SYMBOLS: <fs>.
  DATA: lt_pa0001 LIKE TABLE OF pa0001 WITH HEADER LINE.

  READ TABLE it_excl INDEX 2.
* CONCATENATE '20' IT_EXCL-INIT+6(2) IT_EXCL-INIT+0(2) IT_EXCL-INIT+3(2)
*                          INTO L_DATE_C.
  CONCATENATE '20' it_excl-pernr+6(2) it_excl-pernr+0(2)
 it_excl-pernr+3(2)
                           INTO l_date_c.


  WRITE: l_date_c TO w_fr_date.

*  CONCATENATE '20' IT_EXCL-DAY1+6(2) IT_EXCL-DAY1+0(2)
*  IT_EXCL-DAY1+3(2)
*                            INTO L_DATE_C.
*
*  WRITE: L_DATE_C TO W_DATE.


* by ig.moon 2/9/2009 {

*  l_sachz = it_excl-remarks+0(3).
  DATA tmp_4(4).
  tmp_4 = it_excl-remarks+0(4).
  REPLACE '"' WITH '' INTO tmp_4.
  CONDENSE tmp_4.
  l_sachz = tmp_4(3).

* }


  l_dw = it_excl-day1+0(1).

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

  IF l_dw = 'W'.
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
  ELSE.
    CALL FUNCTION 'DATE_TO_DAY'
         EXPORTING
              date    = sy-datum
         IMPORTING
              weekday = l_to_weekday.

    IF l_to_weekday = 'Monday'.
      l_to_dat = sy-datum - 3.
      IF w_fr_date <> l_to_dat.
        CALL FUNCTION 'POPUP_TO_DECIDE'
           EXPORTING
*   DEFAULTOPTION           = '1'
           textline1               = 'Enter date should be Friday data'
*   TEXTLINE2               = ' '
*   TEXTLINE3               = ' '
           text_option1            = 'Continue to process'
           text_option2            = 'Back'
*   ICON_TEXT_OPTION1       = ' '
*   ICON_TEXT_OPTION2       = ' '
           titel                   = 'Warning'
*   START_COLUMN            = 25
*   START_ROW               = 6
*   CANCEL_DISPLAY          = 'X'
           IMPORTING
           answer                  = l_ans.
        IF l_ans <> '1'.
          w_flag = 'X'.
          EXIT.
        ENDIF.
      ENDIF.
    ELSE.
      l_to_dat = sy-datum - 1.
      IF w_fr_date <> l_to_dat.
        CALL FUNCTION 'POPUP_TO_DECIDE'
            EXPORTING
*   DEFAULTOPTION           = '1'
            textline1               = 'Entry date should be yesterday'
*   TEXTLINE2               = ' '
*   TEXTLINE3               = ' '
            text_option1            = 'Continue to process'
            text_option2            = 'Back'
*   ICON_TEXT_OPTION1       = ' '
*   ICON_TEXT_OPTION2       = ' '
            titel                   = 'Warning'
*   START_COLUMN            = 25
*   START_ROW               = 6
*   CANCEL_DISPLAY          = 'X'
            IMPORTING
            answer                  = l_ans.
        IF l_ans <> '1'.
          w_flag = 'X'.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
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
      MESSAGE e001 WITH 'No authorization to upload data'.
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

  READ TABLE it_excl INDEX 8.
  w_pernr = it_excl-pernr.
  w_name = it_excl-name.
  LOOP AT it_excl FROM 8.
    IF it_excl-awart IS INITIAL.
      CONTINUE.
    ENDIF.
    IF it_excl-pernr IS INITIAL.
      it_cat2-pernr = w_pernr.
      it_cat2-name = w_name.
    ELSE.
      it_cat2-pernr = it_excl-pernr.
      it_cat2-name = it_excl-name.
      w_pernr = it_excl-pernr.
      w_name = it_excl-name.
    ENDIF.
    READ TABLE lt_pa0001 WITH KEY pernr = it_cat2-pernr.
    IF sy-subrc = 0.
      it_cat2-awart = it_excl-awart+0(4).
*      IF W_DATE = W_FR_DATE OR L_DW = 'W' or R_B1 = 'X'.
      it_cat2-day1 = it_excl-day1.
      it_cat2-day2 = it_excl-day2.
      it_cat2-day3 = it_excl-day3.
      it_cat2-day4 = it_excl-day4.
      it_cat2-day5 = it_excl-day5.
      it_cat2-day6 = it_excl-day6.
      it_cat2-day7 = it_excl-day7.
      it_cat2-total = it_excl-total.
      w_total = it_cat2-day1 + it_cat2-day2 + it_cat2-day3 +
                it_cat2-day4 + it_cat2-day5 + it_cat2-day6 +
                it_cat2-day7.
      IF w_total > 0.
        APPEND it_cat2.
      ENDIF.
*      ELSE.
*        SELECT SINGLE CATSHOURS INTO IT_CAT2-DAY1
*        FROM CATSDB
*        WHERE PERNR = IT_CAT2-PERNR
*          AND AWART = IT_CAT2-AWART
*          AND WORKDATE = W_FR_DATE
*                     AND STATUS <> '60'..
*        L_NO = W_DATE - W_FR_DATE + 1.
*        CONCATENATE 'IT_CAT2-DAY' L_NO INTO L_TEXT.
*        ASSIGN (L_TEXT) TO <FS>.
*        <FS> = IT_EXCL-DAY1.
*        IT_CAT2-TOTAL = IT_EXCL-TOTAL.
*        W_TOTAL = IT_EXCL-DAY1.
*        IF W_TOTAL > 0.
*          APPEND IT_CAT2.
*        ENDIF.
*      ENDIF.
    ELSE.
      CALL FUNCTION 'POPUP_TO_INFORM'
           EXPORTING
               titel = 'Time Sheet'
        txt1  = 'The employee does not report to the administrator'
               txt2  = it_cat2-pernr
               txt3  = l_sachz
               txt4  = it_excl-awart.
    ENDIF.
    CLEAR: l_endda, it_cat2, it_excl, w_total.
  ENDLOOP.
ENDFORM.     "read_excel
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM data_process_dbc.
  DATA: w_date(10).

  CONSTANTS: c_varant LIKE tcatst-variant VALUE 'HMMA'.

*  WRITE: P_PDATE TO W_DATE.

  PERFORM bdc_dynpro      USING 'SAPLCATS' '1000'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'CATSFIELDS-INPUTDATE'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=VARI'.
  PERFORM bdc_field       USING 'TCATST-VARIANT'
                                c_varant.
  PERFORM bdc_field       USING 'CATSFIELDS-INPUTDATE'
                                w_date.

  PERFORM bdc_dynpro      USING 'SAPLCATS' '1000'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'TCATST-VARIANT'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=VARI'.
  PERFORM bdc_field       USING 'TCATST-VARIANT'
                                c_varant.

  PERFORM bdc_dynpro      USING 'SAPLCATS' '3001'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=PRPE'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'TCATS-CATSCELL_SIZE'.

  PERFORM bdc_field       USING 'TCATS-ADDINFOBRI'
                                'X'.
  PERFORM bdc_field       USING 'TCATS-DAYTEXT'
                                 'X'.
  PERFORM bdc_field       USING 'TCATS-CATSCELL_SIZE'
                                '10'.

  PERFORM bdc_dynpro      USING 'SAPLCATS' '3001'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=ENTE'.
  PERFORM bdc_dynpro      USING 'SAPLCATS' '3001'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'CATSFIELDS-SACHZ'.

  PERFORM bdc_field       USING 'TCATS-TIMECLERK'
                                 'X'.
  PERFORM bdc_field       USING 'CATSFIELDS-SBMOD'
                                 c_varant.
  PERFORM bdc_field       USING 'CATSFIELDS-SACHZ'
                                w_admin.

  PERFORM bdc_dynpro      USING 'SAPLCATS' '1000'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'TCATST-VARIANT'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=PMAL'.
  PERFORM bdc_field       USING 'TCATST-VARIANT'
                                c_varant.


  PERFORM bdc_dynpro      USING 'SAPLCATS' '1000'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'TCATST-VARIANT'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=TIME'.
  PERFORM bdc_field       USING 'TCATST-VARIANT'
                                c_varant.


  PERFORM bdc_dynpro      USING 'SAPLCATS' '2002'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                               'CATSD-DAY2(02)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  LOOP AT it_cat2.

    PERFORM bdc_field       USING 'CATSD-PERNR(01)'
                                  it_cat2-pernr.
    PERFORM bdc_field       USING 'CATSD-AWART(01)'
                                  it_cat2-awart.
    PERFORM bdc_field       USING 'CATSD-DAY1(01)'
                                  it_cat2-day1.
    PERFORM bdc_field       USING 'CATSD-DAY2(01)'
                                  it_cat2-day2.
    PERFORM bdc_field       USING 'CATSD-DAY3(01)'
                                  it_cat2-day3.
    PERFORM bdc_field       USING 'CATSD-DAY4(01)'
                                  it_cat2-day4.
    PERFORM bdc_field       USING 'CATSD-DAY5(01)'
                                  it_cat2-day5.
    PERFORM bdc_field       USING 'CATSD-DAY6(01)'
                                  it_cat2-day6.
    PERFORM bdc_field       USING 'CATSD-DAY7(01)'
                                  it_cat2-day7.
  ENDLOOP.


  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=SAVE'.

  CALL TRANSACTION 'CAT2' USING it_bdc
                              MODE w_mode
                              UPDATE 'S'
                              MESSAGES INTO it_mess.

  READ TABLE it_mess WITH KEY msgtyp = 'E'.
  IF sy-subrc EQ 0.

    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = 'Time Sheet'
              txt1  = 'Error for time sheet entry'
              txt2  = it_mess-msgv1
              txt3  = it_mess-msgv2
              txt4  = it_mess-msgv3.
  ELSE.
    MESSAGE s001 WITH 'Successfully processed'.
  ENDIF.

ENDFORM.                    " DATA_PROCESS
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
  ENDIF.
  MESSAGE s000 WITH 'Successfully processed records' w_cn.
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
  MESSAGE s000 WITH 'Successfully processed records' w_cn.
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
  NEW-PAGE LINE-SIZE 198.
*  new-page line-size 198 print on.
*  print-control FONT 8.
*** Print title
  WRITE : /(23) 'Total Records uploaded'.
  WRITE:  (8) w_cn.
  SKIP.

  WRITE : /(111) sy-uline.
*  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / sy-vline NO-GAP, (8) 'Per No'         NO-GAP,
              sy-vline NO-GAP, (25) '   Name'     NO-GAP,
              sy-vline NO-GAP, (10) 'Type'       NO-GAP,
              sy-vline NO-GAP, (7) ' DAY1'        NO-GAP,
              sy-vline NO-GAP, (7) ' DAY2'        NO-GAP,
              sy-vline NO-GAP, (7) ' DAY3'        NO-GAP,
              sy-vline NO-GAP, (7) ' DAY4'        NO-GAP,
              sy-vline NO-GAP, (7) ' DAY5'        NO-GAP,
              sy-vline NO-GAP, (7) ' DAY6'        NO-GAP,
              sy-vline NO-GAP, (7) ' DAY7'        NO-GAP,
              sy-vline NO-GAP, (7) 'Total'       NO-GAP,
              sy-vline NO-GAP.

  WRITE : /(111) sy-uline.
** print content
  LOOP AT it_output.
    WRITE : / sy-vline NO-GAP, (8) it_output-pernr    NO-GAP,
                sy-vline NO-GAP, (25) it_output-name    NO-GAP,
                sy-vline NO-GAP, (10) it_output-awart    NO-GAP,
                sy-vline NO-GAP, (7) it_output-day1 NO-GAP,
                sy-vline NO-GAP, (7) it_output-day2 NO-GAP,
                sy-vline NO-GAP, (7) it_output-day3 NO-GAP,
                sy-vline NO-GAP, (7) it_output-day4 NO-GAP,
                sy-vline NO-GAP, (7) it_output-day5 NO-GAP,
                sy-vline NO-GAP, (7) it_output-day6 NO-GAP,
                sy-vline NO-GAP, (7) it_output-day7 NO-GAP,
                sy-vline NO-GAP, (7) it_output-total  NO-GAP,
                sy-vline NO-GAP.
    WRITE : /(111) sy-uline.
  ENDLOOP.
  IF it_output_err[] IS INITIAL.
  ELSE.
    WRITE : /.
    WRITE : /.
    WRITE : /(23) 'Records with error'.
    WRITE : /(111) sy-uline.
    LOOP AT it_output_err.
      WRITE : / sy-vline NO-GAP, (8) it_output_err-pernr    NO-GAP,
                  sy-vline NO-GAP, (25) it_output_err-name    NO-GAP,
                  sy-vline NO-GAP, (10) it_output_err-awart    NO-GAP,
                  sy-vline NO-GAP, (7) it_output_err-day1 NO-GAP,
                  sy-vline NO-GAP, (7) it_output_err-day2 NO-GAP,
                  sy-vline NO-GAP, (7) it_output_err-day3 NO-GAP,
                  sy-vline NO-GAP, (7) it_output_err-day4 NO-GAP,
                  sy-vline NO-GAP, (7) it_output_err-day5 NO-GAP,
                  sy-vline NO-GAP, (7) it_output_err-day6 NO-GAP,
                  sy-vline NO-GAP, (7) it_output_err-day7 NO-GAP,
                  sy-vline NO-GAP, (7) it_output_err-total  NO-GAP,
                  sy-vline NO-GAP.
      WRITE : /(111) sy-uline.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " display_data

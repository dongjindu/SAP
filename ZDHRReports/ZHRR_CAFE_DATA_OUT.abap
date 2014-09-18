*&---------------------------------------------------------------------*
*& Report  ZHRR_CAFE_DATA_OUT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zhrr_cafe_data_out MESSAGE-ID zmfi.

TABLES: pa9100, p9100, *pa9100, pa0000, pa0001.

DATA: lt_9100             LIKE TABLE OF p9100 WITH HEADER LINE
    , wt_9100             LIKE TABLE OF p9100 WITH HEADER LINE
    , ls_9100             TYPE p9100
    , ws_9100             TYPE p9100
    , lt_0000             LIKE TABLE OF p9100 WITH HEADER LINE
    , ls_0000             TYPE p9100
    , gs_pay_result       TYPE pay99_result
    , wa_rt               LIKE pc207 OCCURS 0 WITH HEADER LINE
    , ddntk               LIKE pc23e OCCURS 0 WITH HEADER LINE
    , arrrs               LIKE pc22z OCCURS 0 WITH HEADER LINE
    , wa_9100             TYPE p9100
    , wa_begda            TYPE dats
    , wa_endda            TYPE dats
    .
DATA: g_molga             TYPE molga VALUE '10'.        "USA
DATA: gt_rgdir            LIKE pc261 OCCURS 0 WITH HEADER LINE.
DATA: gs_rgdir            LIKE pc261.
DATA: gs_500l             TYPE t500l.

DATA: BEGIN OF gt_pay_result OCCURS 0,
         evp   TYPE pc261,
         inter TYPE pay99_international,
         nat  ,
      END OF gt_pay_result.
DATA: ls_return           LIKE bapireturn1,
      lv_key              LIKE bapipakey,
      return              LIKE TABLE OF bapireturn WITH HEADER LINE.

DATA: g_return            TYPE zmms0053.
DATA: it_data             LIKE TABLE OF zshr_cafe_out WITH HEADER LINE.

DATA: it_periods          LIKE t549q OCCURS 1
    , is_periods          LIKE LINE OF it_periods
    , get_pabrj           LIKE t549q-pabrj
    , get_pabrp           LIKE t549q-pabrp
    , lv_last_day         TYPE dats
    , gv_pp               LIKE t549q-pabrj
    .
TYPE-POOLS: slis.

DATA: it_file             LIKE TABLE OF zshr_cafe_out  WITH HEADER LINE.

DATA: l_dfile             TYPE string VALUE 'HMMA_MONTHLY_PAYMENT_'.

DATA: v_file              TYPE string,
      v_ans               TYPE i.

DATA: gt_fieldcat         TYPE slis_t_fieldcat_alv,
      gs_fieldcat         TYPE slis_fieldcat_alv.

DATA: BEGIN OF it_file_dlmtd OCCURS 0,
        record(500)       TYPE c,
      END   OF it_file_dlmtd.

DATA: gv_msg(80)          TYPE c
    , gv_dial             TYPE c
    , gv_flag             TYPE c
    , gv_balance          TYPE c
    , gv_balance2         TYPE c
    .
DATA: gv_total_cnt        TYPE string
    , gv_error_cnt        TYPE string
    .

DATA: BEGIN OF it_file_email OCCURS 0,
        pernr             TYPE pernr_d,
      END   OF it_file_email.

DATA: gs_text             TYPE LINE OF bcsy_text
    , gt_e_text           TYPE bcsy_text
    , gv_fdate(8)
    .

CONSTANTS: change TYPE pspar-actio VALUE 'MOD',
  c_dlmtd(1)        TYPE c VALUE ',',
  c_dpath           TYPE string VALUE 'C:\TEMP\GEMPAY\EE_DEMOGRAPHIC',
  c_dextn           TYPE string VALUE '.CSV',
  c_unix_pre(30)    TYPE c VALUE '/sapmnt/',
  c_unix_suf(30)    TYPE c VALUE '/kronos/kronosftp',
  c_2               TYPE c VALUE '2',
  c_3               TYPE c VALUE '3',
  gc_textne         TYPE string VALUE 'TEXT_NE'.

CONSTANTS: c_active(1)    TYPE c VALUE 1.

FIELD-SYMBOLS: <fs>       TYPE any.



SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_pernr   FOR pa0000-pernr NO INTERVALS
              , s_stat2   FOR pa0000-stat2 NO INTERVALS
              , s_bukrs   FOR pa0001-bukrs NO INTERVALS
              , s_werks   FOR pa0001-werks NO INTERVALS
              , s_btrtl   FOR pa0001-btrtl NO INTERVALS
              , s_persg   FOR pa0001-persg NO INTERVALS
              , s_persk   FOR pa0001-persk NO INTERVALS
              , s_abkrs   FOR pa0001-abkrs NO INTERVALS
              , s_kostl   FOR pa0001-kostl NO INTERVALS
              , s_sachz   FOR pa0001-sachz NO INTERVALS
              .

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE text-t02.
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS p_aes AS CHECKBOX.
SELECTION-SCREEN COMMENT 3(30) text-003 FOR FIELD p_aes.

SELECTION-SCREEN POSITION 36.
PARAMETERS p_apr AS CHECKBOX.
SELECTION-SCREEN COMMENT 38(30) text-004 FOR FIELD p_apr.
SELECTION-SCREEN:   END OF LINE.
SELECTION-SCREEN END OF BLOCK blk2.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK blk3 WITH FRAME TITLE text-t03.
PARAMETERS: p_down AS CHECKBOX.
SELECTION-SCREEN SKIP.
PARAMETERS: p_pres RADIOBUTTON GROUP serv USER-COMMAND rad
                                        MODIF ID chk DEFAULT 'X'.
PARAMETERS: p_file(1024) TYPE c LOWER CASE MODIF ID chk
                                VISIBLE LENGTH 45.
SELECTION-SCREEN SKIP.
PARAMETERS: p_eai  RADIOBUTTON GROUP serv MODIF ID chk,
            p_dest TYPE rfcdest MODIF ID chk.
SELECTION-SCREEN END OF BLOCK blk3.

SELECTION-SCREEN END OF BLOCK blk1.



INITIALIZATION.
  CLEAR gv_dial.
  p_dest = 'WMHR01'.
  WRITE sy-datum TO gv_fdate.
  CONCATENATE c_dpath '_' gv_fdate c_dextn INTO p_file.
***  CONCATENATE c_dpath '_' gv_fdate sy-uzeit c_dextn INTO p_file.



AT SELECTION-SCREEN OUTPUT.
  IF sy-tcode = 'ZHRR00013'. " OR sy-uname = 'T00326'.
    gv_dial = 'X'.
  ENDIF.




AT SELECTION-SCREEN.
  IF p_down IS NOT INITIAL.
    IF p_file IS INITIAL.
      MESSAGE e000 WITH 'Please Enter Filename'.
    ENDIF.
  ENDIF.




AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
***  IF NOT p_pres IS INITIAL.
  PERFORM browser CHANGING p_file v_ans.



START-OF-SELECTION.

*- Process 2.
** update eligibility status according to employment status
  IF p_aes IS NOT INITIAL.
    CLEAR: lt_9100[], gv_flag, it_file_email[].
    PERFORM hr_read_infotype_9100.
*
    IF gv_flag IS NOT INITIAL.
      MESSAGE s032. " WITH '(aes)'.
      EXIT.
    ENDIF.
    CLEAR: ls_9100, gv_flag, it_file_email[].
    .
    LOOP AT lt_9100 INTO ls_9100.
      PERFORM read_current_employee_status USING ls_9100-pernr.
      CHECK gv_flag IS INITIAL.
      PERFORM upd_eligibility_status.
    ENDLOOP.

    IF it_file_email[] IS INITIAL.
      MESSAGE s032. " WITH '(aes)'.
    ELSE.
      MESSAGE s000 WITH 'Successful.'.
    ENDIF.

    gv_flag = 'S'.
    IF it_file_email[] IS NOT INITIAL.
      DESCRIBE TABLE it_file_email LINES gv_total_cnt.
      PERFORM send_email using 'E'.
    ENDIF.
  ENDIF.

*- Process 3.
** update eligibility status according to payroll result
  IF p_apr IS NOT INITIAL.
    CLEAR: lt_9100[], gv_flag, it_file_email[].
    PERFORM hr_read_infotype_9100.
    IF gv_flag IS NOT INITIAL.
      MESSAGE s032. " WITH '(apr)'.
      EXIT.
    ENDIF.
    CHECK lt_9100[] IS NOT INITIAL.

    LOOP AT lt_9100 INTO ls_9100.
      CLEAR: gv_balance, gv_pp.
*    "read payroll results
      PERFORM payroll_balance_new_record USING ls_9100-pernr.
      CASE ls_9100-eligi.
        WHEN 'E'.
          PERFORM eligibility_status_e_process.
*          CLEAR it_file_email.
*          it_file_email-pernr = ls_9100-pernr.
*          APPEND it_file_email.
        WHEN 'S1'. PERFORM eligibility_status_s1_process.
*        WHEN 'S2'. PERFORM eligibility_status_s2_process.
        WHEN 'S2'.
          PERFORM eligibility_status_s2_process
                  USING ls_9100-pernr.
      ENDCASE.
    ENDLOOP.
    IF it_file_email[] IS INITIAL.
      MESSAGE s032. " WITH '(apr)'.
    ELSE.
      MESSAGE s000 WITH 'Successful.'.
    ENDIF.

    gv_flag = 'R'.
    IF it_file_email[] IS NOT INITIAL.
      DESCRIBE TABLE it_file_email LINES gv_total_cnt.
      PERFORM send_email USING 'P'.
    ENDIF.
  ENDIF.

  IF p_eai = 'X'.
    IF it_file_email[] IS NOT INITIAL.
      DESCRIBE TABLE it_file_email LINES gv_total_cnt.
      PERFORM send_email_eai.
    ENDIF.
  ENDIF.

*- Process 4
  IF p_down = 'X'.
    PERFORM hr_read_infotype_9100.
    PERFORM output_data2file.
    IF it_file[] IS INITIAL.
      MESSAGE s032.
      STOP.
    ELSE.
      PERFORM transfer_data.
    ENDIF.
  ENDIF.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  BROWSER
*&---------------------------------------------------------------------*
*       Browse Desktop/PC Directory
*----------------------------------------------------------------------*
*      <--filename  filename
*      <--answer    return code
*----------------------------------------------------------------------*
FORM browser              CHANGING filename TYPE c answer TYPE i.

  DATA: $filename         TYPE string,
        l_path            TYPE string,
        l_fpath           TYPE string,
        l_dfile           TYPE string.

  CONCATENATE l_dfile sy-datum sy-uzeit c_dextn INTO l_dfile.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title      = 'Select File Name'
***   default_extension = 'txt'
      default_extension = 'csv'
      default_file_name = l_dfile
***   file_filter       = 'TXT (*.txt)|*.txt| All (*.*)|*.*'
      file_filter       = 'CSV (*.csv)|*.csv| All (*.*)|*.*'
      initial_directory = c_dpath
    CHANGING
      filename          = $filename
      path              = l_path
      fullpath          = l_fpath
      user_action       = answer
    EXCEPTIONS
      cntl_error        = 1
      error_no_gui      = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSEIF answer = 0.
    filename = l_fpath.
  ELSE.
    MESSAGE s118(ba).
  ENDIF.
ENDFORM.                    " BROWSER
*&---------------------------------------------------------------------*
*&      Form  display_unix
*&---------------------------------------------------------------------*
*       Display UNIX Directory
*----------------------------------------------------------------------*
*      <--filename  filename
*      <--answer    return code
*----------------------------------------------------------------------*
FORM display_unix CHANGING filename TYPE c answer TYPE i.

  DATA: BEGIN OF it_filename OCCURS 0,
          path(1024) TYPE c,
        END OF it_filename.

  CONCATENATE c_unix_pre sy-sysid c_unix_suf INTO it_filename-path.
  APPEND it_filename.

  CALL 'C_SAPGPARAM' ID 'NAME'  FIELD 'DIR_TEMP'
                     ID 'VALUE' FIELD it_filename-path.
  APPEND it_filename.

  CALL FUNCTION 'POPUP_WITH_TABLE'
    EXPORTING
      endpos_col   = '100'
      endpos_row   = '10'
      startpos_col = '1'
      startpos_row = '1'
      titletext    = 'Select UNIX Directory'
    IMPORTING
      choice       = filename
    TABLES
      valuetab     = it_filename
    EXCEPTIONS
      break_off    = 1
      OTHERS       = 2.

  answer = sy-subrc.

  IF sy-subrc = 0.
    CONCATENATE filename '/' l_dfile sy-datum sy-uzeit c_dextn
           INTO filename.
  ELSE.
    MESSAGE s549(fibl).
  ENDIF.

ENDFORM.                    " display_unix



*&---------------------------------------------------------------------*
*&      Form  UPD_ELIGIBILITY_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0298   text
*----------------------------------------------------------------------*
FORM upd_eligibility_status.

  CLEAR: gv_flag.
  PERFORM create_infotype_9100 USING ls_9100-pernr gc_textne.

ENDFORM.                    " UPD_ELIGIBILITY_STATUS
*&---------------------------------------------------------------------*
*&      Form  HR_READ_INFOTYPE_0000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM hr_read_infotype_0000.

  DATA: p_pernr  TYPE pernr_d.

* HR History read
  p_pernr = s_pernr-low.
  CALL FUNCTION 'HR_READ_INFOTYPE'
    EXPORTING
*      TCLAS                 = 'A'
      pernr                 = p_pernr
      infty                 = '0000'
*     BEGDA                 = '18000101'
*     ENDDA                 = '99991231'
*     BYPASS_BUFFER         = ' '
*     LEGACY_MODE           = ' '
*   IMPORTING
*     SUBRC                 =
     TABLES
       infty_tab             = lt_0000
     EXCEPTIONS
       infty_not_found       = 1
       OTHERS                = 2
            .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  CHECK lt_0000[] IS NOT INITIAL.
  SORT lt_0000 BY begda endda.
  DELETE lt_0000 WHERE begda > sy-datum OR endda < sy-datum.

ENDFORM.                    " HR_READ_INFOTYPE_0000



*&---------------------------------------------------------------------*
*&      Form  HR_READ_INFOTYPE_9100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM hr_read_infotype_9100.

  DATA: lt_9100r            LIKE TABLE OF p9100 WITH HEADER LINE
      , ls_9100r            TYPE p9100
      , lv_flag             TYPE c
      .

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_9100r
    FROM pa9100
   WHERE pernr IN s_pernr
     AND begda <= sy-datum AND endda >= sy-datum.

  IF lt_9100r[] IS INITIAL.
    gv_flag = 'X'.
    EXIT.
  ENDIF.

  SORT lt_9100r BY pernr endda DESCENDING begda.

  CLEAR: ls_9100, lt_9100[].
  LOOP AT lt_9100r INTO ls_9100r.
    ls_9100 = ls_9100r.
    CLEAR lv_flag.
    PERFORM personnel_group_check USING lv_flag.
    IF lv_flag IS INITIAL.
      APPEND ls_9100 TO lt_9100.
      CLEAR: ls_9100, ls_9100r.
    ENDIF.
  ENDLOOP.

  IF lt_9100[] IS INITIAL.
    gv_flag = 'X'.
  ENDIF.

ENDFORM.                    " HR_READ_INFOTYPE_9100



*&---------------------------------------------------------------------*
*&      Form  READ_CURRENT_EMPLOYEE_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_9100_PERNR  text
*----------------------------------------------------------------------*
FORM read_current_employee_status  USING    p_pernr TYPE pernr_d.

  RANGES: r_stat2         FOR  p0000-stat2.
  clear: gv_flag.
  r_stat2-option = 'EQ'.
  r_stat2-sign   = 'I'.
  r_stat2-low    = '1'.
  APPEND r_stat2.

  r_stat2-low    = '3'.
  APPEND r_stat2.

  CLEAR pa0000.
  SELECT SINGLE * FROM pa0000
   WHERE pernr   = p_pernr
     AND ( begda =< sy-datum AND endda >= sy-datum )
     AND stat2   IN r_stat2. "s_stat2.

  IF sy-subrc = 0.
    gv_flag = 'N'.
  ENDIF.

ENDFORM.                    " READ_CURRENT_EMPLOYEE_STATUS



*&---------------------------------------------------------------------*
*&      Form  CREATE_INFOTYPE_9100
*&---------------------------------------------------------------------*
*       text : Delimit current it9100 record and append text
*----------------------------------------------------------------------*
*      -->P_GC_TEXTNE  text
*----------------------------------------------------------------------*
FORM create_infotype_9100  USING    p_pernr TYPE pernr_d
                                    p_textne.

  DATA : is_9100          TYPE p9100
       , s_p9100          LIKE p9100
       , lv_term_date(10) TYPE c
       , w_begda          LIKE pa9100-begda
       , w_endda          LIKE pa9100-endda
       , lv_tdate(11)     TYPE c
       .
  CONSTANTS:
         lc_text          TYPE string VALUE
        'Employment termination updated on'
       .

  SELECT SINGLE * FROM pa9100
    INTO CORRESPONDING FIELDS OF s_p9100
   WHERE pernr = p_pernr
     AND ( begda <= sy-datum AND endda >= sy-datum ).

  "This code is requred and locks the record ready for modification
  CALL FUNCTION 'HR_EMPLOYEE_ENQUEUE'
    EXPORTING
      number = p_pernr.

  s_p9100-infty = '9100'.
  w_begda = s_p9100-begda.
  w_endda = s_p9100-endda.
  MOVE-CORRESPONDING s_p9100 TO is_9100.
  is_9100-pernr = p_pernr.
  is_9100-endda = sy-datum - 1.
  s_p9100-aedtm = sy-datum.
  s_p9100-uname = sy-uname.

  WRITE: is_9100-endda USING EDIT MASK '__/__/____' TO lv_term_date.
  CONCATENATE lv_term_date '.' INTO lv_tdate.
  IF ls_9100-text1 IS NOT INITIAL.
    CONCATENATE ls_9100-text1 lc_text
                lv_tdate INTO is_9100-text1 SEPARATED BY space.
  ELSE.
    CONCATENATE lc_text
                lv_tdate INTO is_9100-text1 SEPARATED BY space.
  ENDIF.

  is_9100-uname = sy-uname.

  "plus populate any other fields you need to update
  CALL FUNCTION 'HR_INFOTYPE_OPERATION'
    EXPORTING
      infty            = '9100'
      number           = s_p9100-pernr
      subtype          = s_p9100-subty
      objectid         = s_p9100-objps
      lockindicator    = s_p9100-sprps
      validityend      = w_endda
      validitybegin    = w_begda
      recordnumber     = s_p9100-seqnr
      record           = is_9100
      operation        = change
      nocommit         = ' '
*     tclas            = 'A'
*     DIALOG_MODE      = '0'
*                                                           "20140610
*     VIEW_IDENTIFIER  =
*     SECONDARY_RECORD =
    IMPORTING
      return           = ls_return
      key              = lv_key.


  IF ls_return IS NOT INITIAL.
    return-type = 'E'.
    CONCATENATE ls_return-id ls_return-number INTO ls_return-id.
    CONDENSE ls_return-id NO-GAPS.
*        return-code = ls_return-id.
    return-message = ls_return-message.
    APPEND return.
  ELSE.
    COMMIT WORK.

    return-type = 'S'.
    CONCATENATE 'Success :' lv_key INTO return-message
                SEPARATED BY space.
*        return-message = 'Success!'.
    APPEND return.

    CLEAR it_file_email.
    it_file_email-pernr = ls_9100-pernr.
    APPEND it_file_email.
  ENDIF.

  "unlock record after modification
  CALL FUNCTION 'HR_EMPLOYEE_DEQUEUE'
    EXPORTING
      number = p_pernr.

ENDFORM.                    " CREATE_INFOTYPE_9100



*&---------------------------------------------------------------------*
*&      Form  PAYROLL_BALANCE_NEW_RECORD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM payroll_balance_new_record USING p_pernr TYPE pernr_d.

  DATA : lv_relid         LIKE pcl2-relid
       , lv_molga         TYPE molga
       .
  RANGES: s_begda         FOR  p0001-begda
        , s_fpper         FOR  pc261-fpper
        .
  CHECK p_pernr IS NOT INITIAL.



*- Read last payroll results  start
  CLEAR : gt_rgdir, gt_rgdir[].
  CALL FUNCTION 'CU_READ_RGDIR'
    EXPORTING
      persnr             = p_pernr
*     BUFFER             =
*     NO_AUTHORITY_CHECK = ' '
    IMPORTING
      molga              = g_molga
    TABLES
      in_rgdir           = gt_rgdir[]
    EXCEPTIONS
      no_record_found    = 1
      OTHERS             = 2.

* Delete payroll control records where payroll period is 000000
  DELETE gt_rgdir WHERE fpper EQ '000000'.

** Delete voided payroll data.
  DELETE gt_rgdir WHERE voidr NE space.

  IF NOT s_begda[] IS INITIAL.
    DELETE gt_rgdir WHERE NOT paydt IN s_begda. "System Year
  ENDIF.
*** Delete payroll control records based on selection input
  IF  NOT  s_fpper[]  IS INITIAL.
    DELETE gt_rgdir WHERE NOT fpper IN s_fpper. "Payroll Period
  ENDIF.

* Cluster id for US
* Personnel Country Grouping
  CLEAR lv_relid.
  SELECT SINGLE relid INTO lv_relid
                FROM t500l
                WHERE molga = g_molga.
  IF   lv_relid IS INITIAL.
    lv_relid = 'RU'.
  ENDIF.

  CLEAR: gs_pay_result, gs_rgdir, gv_pp.
*  sort last payroll period
  SORT gt_rgdir BY seqnr DESCENDING fpbeg fpend DESCENDING.
  READ TABLE gt_rgdir INTO gs_rgdir INDEX 1.
  gv_pp = gs_rgdir-fpend.

  CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
    EXPORTING
*     CLUSTERID                          =
      employeenumber                   = p_pernr
      sequencenumber                   = gs_rgdir-seqnr
*     READ_ONLY_BUFFER                 = ' '
      read_only_international          = 'X'
*     ARC_GROUP                        = ' '
*     CHECK_READ_AUTHORITY             = 'X'
*     FILTER_CUMULATIONS               = 'X'
*     CLIENT                           =
*   IMPORTING
*     VERSION_NUMBER_PAYVN             =
*     VERSION_NUMBER_PCL2              =
    CHANGING
      payroll_result                   = gs_pay_result
*   EXCEPTIONS
*     ILLEGAL_ISOCODE_OR_CLUSTERID     = 1
*     ERROR_GENERATING_IMPORT          = 2
*     IMPORT_MISMATCH_ERROR            = 3
*     SUBPOOL_DIR_FULL                 = 4
*     NO_READ_AUTHORITY                = 5
*     NO_RECORD_FOUND                  = 6
*     VERSIONS_DO_NOT_MATCH            = 7
*     ERROR_READING_ARCHIVE            = 8
*     ERROR_READING_RELID              = 9
*     OTHERS                           = 10
            .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

*- Read last payroll results  end
  LOOP AT gs_pay_result-inter-arrrs INTO arrrs.

    IF arrrs-lgart = '3050' AND arrrs-betrg <> 0.
      gv_balance = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.

* 20140616 s Request by Grace.
  CHECK gv_balance IS INITIAL.
  LOOP AT gs_pay_result-inter-ddntk INTO ddntk.

    IF ddntk-lgart = '3050' AND ddntk-betrg <> 0.
      gv_balance = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.
* 20140616 e
ENDFORM.                    " PAYROLL_BALANCE_NEW_RECORD

*&---------------------------------------------------------------------*
*&      Form  OUTPUT_DATA2FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM output_data2file .

  CLEAR: it_file[].
  LOOP AT lt_9100 INTO ls_9100.
    CLEAR: it_file.
    SELECT SINGLE nachn vorna INTO (it_file-nachn, it_file-vorna)
      FROM pa0002
     WHERE pernr = ls_9100-pernr.
    IF sy-subrc = 0.
      it_file-pernr = ls_9100-pernr+2(6).
      .
      SELECT SINGLE zausw INTO it_file-zausw
        FROM pa0050
       WHERE pernr = ls_9100-pernr
         AND endda = '99991231'
         AND zausw > 0.

      APPEND it_file.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " OUTPUT_DATA2FILE

*&---------------------------------------------------------------------*
*&      Form  TRANSFER_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM transfer_data .

  TYPE-POOLS: truxs.

  DATA: l_totrec          TYPE i
      , l_filter(50)      TYPE c VALUE 'des -e -k abc123forme'
      , l_return          TYPE zmms0053
      , it_file_dlmtd1    TYPE truxs_t_text_data
      .

  DATA: l_message         TYPE char80.

  DESCRIBE TABLE it_file LINES l_totrec.
  IF NOT p_eai IS INITIAL.
    CALL FUNCTION 'ZFHR_CAFE_OUT'
      DESTINATION p_dest
      TABLES
        OT_DATA1A                      = it_file
      EXCEPTIONS
        call_function_destination_no_t = 1
        call_function_no_dest          = 2
        call_function_remote_error     = 3
        rfc_no_authority               = 4
        OTHERS                         = 5.
    IF sy-subrc <> 0.
      MESSAGE e000 WITH 'Error when calling RFC destination:'
                         p_dest.
    ELSE.
      CONCATENATE 'Data has been sent to' p_dest
             INTO l_message SEPARATED BY space.
      MESSAGE s000 WITH l_message l_totrec 'Record(s)' .
    ENDIF.
  ELSE.
    LOOP AT it_file.
      DO.
        ASSIGN COMPONENT sy-index OF STRUCTURE it_file TO <fs>.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        IF NOT it_file_dlmtd IS INITIAL.
          CONCATENATE it_file_dlmtd-record c_dlmtd <fs>
                 INTO it_file_dlmtd-record.
        ELSE.
          it_file_dlmtd-record = <fs>.
        ENDIF.
      ENDDO.

      APPEND it_file_dlmtd. CLEAR it_file_dlmtd.
    ENDLOOP.

    DESCRIBE TABLE it_file_dlmtd LINES l_totrec.
    v_file = p_file.
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = v_file
      TABLES
        data_tab                = it_file_dlmtd
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.
*    CALL FUNCTION 'GUI_DOWNLOAD'
*      EXPORTING
*        filename                = v_file
*      TABLES
*        data_tab                = it_file_dlmtd
*      EXCEPTIONS
*        file_write_error        = 1
*        no_batch                = 2
*        gui_refuse_filetransfer = 3
*        invalid_type            = 4
*        no_authority            = 5
*        unknown_error           = 6.

    IF sy-subrc <> 0.
      IF sy-subrc = 15.
        MESSAGE s011 WITH 'Access Denied'.
      ELSEIF NOT sy-msgid IS INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        MESSAGE s011 WITH 'Error when creating the file'.
      ENDIF.
    ELSE.
      MESSAGE s000 WITH 'File is written to:' v_file l_totrec
                'Record(s)'.
    ENDIF.
  ENDIF.

ENDFORM.                    " TRANSFER_DATA



*&---------------------------------------------------------------------*
*&      Form  ELIGIBILITY_STATUS_E_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM eligibility_status_e_process .

  DATA: lv_s12  TYPE nused2.

  IF gv_balance IS NOT INITIAL.
    CLEAR gv_flag.
    PERFORM read_9100_ststus_s1 USING gv_flag.
    IF gv_flag IS NOT INITIAL.
      lv_s12 = 'S1'.
    ELSE.
      lv_s12 = 'S2'.
    ENDIF.
    PERFORM s12_record_create_proc USING lv_s12.
  ENDIF.

ENDFORM.                    " ELIGIBILITY_STATUS_E_PROCESS



*&---------------------------------------------------------------------*
*&      Form  ELIGIBILITY_STATUS_S1_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM eligibility_status_s1_process .

  DATA : lv_flag          TYPE c
       , lv_s12           TYPE c
       .
***  CHECK gv_balance IS NOT INITIAL.   20140616
  CHECK gv_balance IS INITIAL.
***
  CLEAR: gv_balance2.
  PERFORM 2nd_read_before_last_payperiod USING ls_9100-pernr.
  CHECK gv_balance2 IS INITIAL.

* 20140616 Change s
  lv_s12 = 'E'.
  PERFORM e_record_create_proc USING lv_s12.
****** Check Current S1/S2 record
*****  CLEAR: wt_9100, wt_9100[].
*****  SELECT * INTO CORRESPONDING FIELDS OF TABLE wt_9100
*****    FROM pa9100
*****   WHERE pernr EQ ls_9100-pernr
*****     AND begda <= sy-datum AND endda >= sy-datum
*****     AND ( eligi = 'S1' OR eligi = 'S2').
*****
*****  IF wt_9100[] IS NOT INITIAL.
*****
*****    SORT wt_9100 BY begda endda.
*****    LOOP AT wt_9100 INTO ws_9100.
*****      lv_flag = 'E'.
*****      PERFORM table_lock USING lv_flag.  "Enqueue
*****
*****      CLEAR wa_9100.
*****      wa_9100       = ws_9100.
*****      wa_9100-begda = sy-datum.
*****      wa_9100-endda = '99991231'.
*****      wa_begda      = ws_9100-begda.
*****      wa_endda      = ws_9100-endda.
*****
*****      lv_s12 = 'E'.
*****      PERFORM e_record_create_proc USING lv_s12.
*****    ENDLOOP.
*****
*****  ENDIF.
* 20140616 Change e

ENDFORM.                    " ELIGIBILITY_STATUS_S1_PROCESS

*&---------------------------------------------------------------------*
*&      Form  ELIGIBILITY_STATUS_S2_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM eligibility_status_s2_process USING p_pernr .

  DATA : lv_date          TYPE dats
       , lv_flag          TYPE c
       .

*  lv_date = sy-datum - 1.
*  PERFORM get_pay_period USING lv_date.
*  CHECK it_periods[] IS NOT INITIAL.
*  CHECK get_pabrj = gv_pp.
  PERFORM get_pay_period USING p_pernr.
  check get_pabrp = '01'.
  PERFORM eligibility_status_s1_process.

ENDFORM.                    " ELIGIBILITY_STATUS_S2_PROCESS

*&---------------------------------------------------------------------*
*&      Form  READ_9100_STSTUS_S1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_FLAG  text
*----------------------------------------------------------------------*
FORM read_9100_ststus_s1  USING    p_flag.

  DATA: ls_9100s1         TYPE p9100
      , lv_begda          TYPE dats
      , lv_endda          TYPE dats
      .

  lv_begda = lv_endda = sy-datum.
  lv_begda+4(4) = '0101'.
  lv_endda+4(4) = '1231'.

  SELECT SINGLE * FROM pa9100
   WHERE pernr EQ ls_9100-pernr
***     AND begda GE lv_begda        "20140616 change
                                                            "20140610 a
***     AND endda LE lv_endda
                                                            "20140610 a
     AND endda GE lv_begda                                  "20140610 a
     AND begda LE lv_endda                                  "20140610 a
     AND eligi EQ 'S1'.
  IF sy-subrc NE 0.
    p_flag = 'X'.
  ENDIF.

ENDFORM.                    " READ_9100_STSTUS_S1

*&---------------------------------------------------------------------*
*&      Form  E_RECORD_DELIMIT_PROC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM e_record_delimit_proc .

  DATA : lv_flag          TYPE c
       , lv_tdate(11)     TYPE c
       .
  CONSTANTS:
         lc_text          TYPE string VALUE
        'Employment termination update on'
       .

  lv_flag = 'E'.
  PERFORM table_lock USING lv_flag.  "Enqueue

  CLEAR wa_9100.
  wa_9100       = ls_9100.
  wa_9100-endda = sy-datum - 1.
  wa_begda = ls_9100-begda.
  wa_endda = ls_9100-endda.

**  CONCATENATE ',,' ls_9100-text1 'Employment terminated on'
**              wa_9100-endda INTO wa_9100-text1 SEPARATED BY space.
  WRITE: ls_9100-endda USING EDIT MASK '__/__/____' TO lv_tdate.
  CONCATENATE lv_tdate '.' INTO lv_tdate.
  IF ls_9100-text1 IS NOT INITIAL.
    CONCATENATE ls_9100-text1 lc_text
                lv_tdate INTO wa_9100-text1 SEPARATED BY space.
  ELSE.
    CONCATENATE lc_text
                lv_tdate INTO wa_9100-text1 SEPARATED BY space.
  ENDIF.

  "plus populate any other fields you need to update
  CALL FUNCTION 'HR_INFOTYPE_OPERATION'
    EXPORTING
      infty         = '9100'
      number        = ls_9100-pernr
      subtype       = ls_9100-subty
      objectid      = ls_9100-objps
      lockindicator = ls_9100-sprps
      validityend   = wa_endda
      validitybegin = wa_begda
      recordnumber  = ls_9100-seqnr
      record        = wa_9100
      operation     = 'MOD'
      nocommit      = ' '
    IMPORTING
      return        = ls_return
      key           = lv_key.

  IF ls_return IS NOT INITIAL.
    return-type = 'E'.
    CONCATENATE ls_return-id ls_return-number INTO ls_return-id.
    CONDENSE ls_return-id NO-GAPS.
*        return-code = ls_return-id.
    return-message = ls_return-message.
    APPEND return.
  ELSE.
    COMMIT WORK.

    return-type = 'S'.
    CONCATENATE 'Success :' lv_key INTO return-message
                SEPARATED BY space.
*        return-message = 'Success!'.
    APPEND return.

  ENDIF.
  lv_flag = 'D'.
  PERFORM table_lock USING lv_flag.  "Dequeue

ENDFORM.                    " E_RECORD_DELIMIT_PROC



*&---------------------------------------------------------------------*
*&      Form  S12_RECORD_CREATE_PROC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_S12  text
*----------------------------------------------------------------------*
FORM s12_record_create_proc  USING    p_s12.

  DATA : wa_9100          TYPE p9100
       , wa_begda         TYPE dats
       , wa_endda         TYPE dats
       , lv_date(10)      TYPE c
       , lv_flag          TYPE c
       .
  lv_flag = 'E'.
  PERFORM table_lock USING lv_flag.  "Enqueue

  CLEAR wa_9100.
  wa_9100       = ls_9100.
  wa_9100-begda = sy-datum.
  wa_9100-endda = '99991231'.
***  wa_begda      = sy-datum.
***  wa_endda      = wa_9100-endda.

  WRITE: sy-datum USING EDIT MASK '__/__/____' TO lv_date.
  CONCATENATE 'Suspended on ' lv_date
         INTO wa_9100-text1 SEPARATED BY space.

  CONCATENATE wa_9100-text1
             'due to unrecovered balance in payroll results.'
         INTO wa_9100-text1 SEPARATED BY space.

  wa_9100-uname = sy-uname. "'RFCESS'.
  wa_9100-seqnr = '000'.
  wa_9100-eligi = p_s12.

  "plus populate any other fields you need to update
  CALL FUNCTION 'HR_INFOTYPE_OPERATION'
    EXPORTING
      infty            = '9100'
      number           = ls_9100-pernr
      subtype          = ls_9100-subty
      objectid         = ls_9100-objps
*     LOCKINDICATOR    =
      validityend      = wa_9100-endda
      validitybegin    = wa_9100-begda
*     RECORDNUMBER     =
      record           = wa_9100
      operation        = 'INS'
      tclas            = 'A'
*     DIALOG_MODE      = '0'
      nocommit         = 'X'
*     VIEW_IDENTIFIER  =
*     SECONDARY_RECORD =
    IMPORTING
      return           = ls_return
      key              = lv_key.

  IF ls_return IS NOT INITIAL.
    return-type = 'E'.
    CONCATENATE ls_return-id ls_return-number INTO ls_return-id.
    CONDENSE ls_return-id NO-GAPS.
*        return-code = ls_return-id.
    return-message = ls_return-message.
    APPEND return.
  ELSE.
    COMMIT WORK.

    return-type = 'S'.
    CONCATENATE 'Success :' lv_key INTO return-message
                SEPARATED BY space.
*        return-message = 'Success!'.
    APPEND return.

    CLEAR it_file_email.
    it_file_email-pernr = ls_9100-pernr.
    APPEND it_file_email.

  ENDIF.
  lv_flag = 'D'.
  PERFORM table_lock USING lv_flag.  "Dequeue

ENDFORM.                    " S12_RECORD_CREATE_PROC

*&---------------------------------------------------------------------*
*&      Form  TABLE_LOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_FLAG  text
*----------------------------------------------------------------------*
FORM table_lock  USING    p_flag.

  CASE p_flag.
    WHEN 'E'.
      "This code is requred and locks the record ready for modification
      CALL FUNCTION 'HR_EMPLOYEE_ENQUEUE'
        EXPORTING
          number = ls_9100-pernr.
    WHEN 'D'.
      "unlock record after modification
      CALL FUNCTION 'HR_EMPLOYEE_DEQUEUE'
        EXPORTING
          number = ls_9100-pernr.
  ENDCASE.

ENDFORM.                    " TABLE_LOCK



*&---------------------------------------------------------------------*
*&      Form  2ND_READ_BEFORE_LAST_PAYPERIOD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_9100_PERNR  text
*----------------------------------------------------------------------*
FORM 2nd_read_before_last_payperiod  USING    p_pernr TYPE pernr_d.

  DATA : lv_relid         LIKE pcl2-relid
       , lv_molga         TYPE molga
       , ws_pay_result    TYPE pay99_result
       , ws_rt            LIKE pc207 OCCURS 0 WITH HEADER LINE
       , ddntk            LIKE pc23e OCCURS 0 WITH HEADER LINE
       , wt_rgdir         LIKE pc261 OCCURS 0 WITH HEADER LINE
       , ws_rgdir         LIKE pc261
       , ws_500l          TYPE t500l
       .
  RANGES
       : s_begda          FOR  p0001-begda
       , s_fpper          FOR  pc261-fpper
       .
  CHECK p_pernr IS NOT INITIAL.

*- Read last payroll results  start
  CLEAR : gt_rgdir, gt_rgdir[].
  CALL FUNCTION 'CU_READ_RGDIR'
    EXPORTING
      persnr          = p_pernr
    IMPORTING
      molga           = g_molga
    TABLES
      in_rgdir        = wt_rgdir[]
    EXCEPTIONS
      no_record_found = 1
      OTHERS          = 2.

* Delete payroll control records where payroll period is 000000
  DELETE wt_rgdir WHERE fpper EQ '000000'.

** Delete voided payroll data.
  DELETE wt_rgdir WHERE voidr NE space.

  IF NOT s_begda[] IS INITIAL.
    DELETE wt_rgdir WHERE NOT paydt IN s_begda. "System Year
  ENDIF.
*** Delete payroll control records based on selection input
  IF  NOT  s_fpper[]  IS INITIAL.
    DELETE wt_rgdir WHERE NOT fpper IN s_fpper. "Payroll Period
  ENDIF.

* Cluster id for US
* Personnel Country Grouping
  CLEAR lv_relid.
  SELECT SINGLE relid INTO lv_relid
                FROM t500l
                WHERE molga = g_molga.
  IF   lv_relid IS INITIAL.
    lv_relid = 'RU'.
  ENDIF.

  CLEAR: gs_pay_result, gs_rgdir.
*  sort last payroll period
  SORT wt_rgdir BY seqnr DESCENDING fpbeg fpend DESCENDING.
  READ TABLE wt_rgdir INTO ws_rgdir INDEX 2.

  CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
    EXPORTING
*     CLUSTERID                          =
      employeenumber                     = p_pernr
      sequencenumber                     = ws_rgdir-seqnr
*     READ_ONLY_BUFFER                   = ' '
      read_only_international            = 'X'
*     ARC_GROUP                          = ' '
*     CHECK_READ_AUTHORITY               = 'X'
*     FILTER_CUMULATIONS                 = 'X'
*     CLIENT                             =
*   IMPORTING
*     VERSION_NUMBER_PAYVN               =
*     VERSION_NUMBER_PCL2                =
    CHANGING
      payroll_result                     = ws_pay_result
*   EXCEPTIONS
*     ILLEGAL_ISOCODE_OR_CLUSTERID       = 1
*     ERROR_GENERATING_IMPORT            = 2
*     IMPORT_MISMATCH_ERROR              = 3
*     SUBPOOL_DIR_FULL                   = 4
*     NO_READ_AUTHORITY                  = 5
*     NO_RECORD_FOUND                    = 6
*     VERSIONS_DO_NOT_MATCH              = 7
*     ERROR_READING_ARCHIVE              = 8
*     ERROR_READING_RELID                = 9
*     OTHERS                             = 10
            .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

*- Read 2nd_last payroll results  end
  LOOP AT ws_pay_result-inter-arrrs INTO arrrs.
    IF arrrs-lgart = '3050' AND arrrs-betrg <> 0.
      gv_balance2 = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.

* 20140616 s Request by Grace.
  CHECK gv_balance IS INITIAL.
  LOOP AT gs_pay_result-inter-ddntk INTO ddntk.

    IF ddntk-lgart = '3050' AND ddntk-betrg <> 0.
      gv_balance2 = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.
* 20140616 e
ENDFORM.                    " 2ND_READ_BEFORE_LAST_PAYPERIOD
*&---------------------------------------------------------------------*
*&      Form  E_RECORD_CREATE_PROC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_S12  text
*----------------------------------------------------------------------*
FORM e_record_create_proc  USING    p_s12.

  DATA : wa_9100          TYPE p9100
*       , wa_begda         TYPE dats
*       , wa_endda         TYPE dats
       , lv_flag          TYPE c.

  data: lv_date(10).

  lv_flag = 'E'.
  PERFORM table_lock USING lv_flag.  "Enqueue

  CLEAR wa_9100.
*  wa_9100       = ws_9100.
  MOVE-CORRESPONDING ls_9100 to wa_9100.
  wa_9100-begda = sy-datum.
  wa_9100-endda = '99991231'.
*  wa_begda      = sy-datum.
*  wa_endda      = wa_9100-endda.

  WRITE: sy-datum USING EDIT MASK '__/__/____' TO lv_date.
  CONCATENATE 'Previous suspension lifted on'
              lv_date INTO wa_9100-text1 SEPARATED BY space.

  wa_9100-uname = 'RFCESS'.
  wa_9100-seqnr = '000'.
  wa_9100-eligi = p_s12.

  "plus populate any other fields you need to update
  CALL FUNCTION 'HR_INFOTYPE_OPERATION'
    EXPORTING
      infty            = '9100'
      number           = ws_9100-pernr
      subtype          = ws_9100-subty
      objectid         = ws_9100-objps
*     LOCKINDICATOR    =
      validityend      = wa_9100-endda
      validitybegin    = wa_9100-begda
*     RECORDNUMBER     =
      record           = wa_9100
      operation        = 'INS'
      tclas            = 'A'
*     DIALOG_MODE      = '0'
      nocommit         = 'X'
*     VIEW_IDENTIFIER  =
*     SECONDARY_RECORD =
    IMPORTING
      return           = ls_return
      key              = lv_key.

  IF ls_return IS NOT INITIAL.
    return-type = 'E'.
    CONCATENATE ls_return-id ls_return-number INTO ls_return-id.
    CONDENSE ls_return-id NO-GAPS.
*        return-code = ls_return-id.
    return-message = ls_return-message.
    APPEND return.
  ELSE.
    COMMIT WORK.

    return-type = 'S'.
    CONCATENATE 'Success :' lv_key INTO return-message
                SEPARATED BY space.
*        return-message = 'Success!'.
    APPEND return.

  ENDIF.
  lv_flag = 'D'.
  PERFORM table_lock USING lv_flag.  "Dequeue

ENDFORM.                    " E_RECORD_CREATE_PROC



*&---------------------------------------------------------------------*
*&      Form  GET_PAY_PERIOD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_DATE  text
*----------------------------------------------------------------------*
*FORM get_pay_period  USING    p_date TYPE dats.
*
*  CALL FUNCTION 'HR_PAYROLL_PERIODS_GET'
*    EXPORTING
*      get_begda       = p_date  "validitybegin
*      get_permo       = '04'    "RPTIME_PERIOD
*    IMPORTING
*      get_pabrj       = get_pabrj
*      get_pabrp       = get_pabrp
*    TABLES
*      get_periods     = it_periods
*    EXCEPTIONS
*      no_period_found = 1
*      no_valid_permo  = 2
*      OTHERS          = 3.
*  IF sy-subrc <> 0.
** Implement suitable error handling here
*  ENDIF.
*
*ENDFORM.                    " GET_PAY_PERIOD

FORM get_pay_period using p_pernr.

  data:lv_abkrs like pa0001-abkrs,
       l_state like T569V-STATE,
       l_curr_period like T569V-PABRP,
       l_foll_period like T569V-PABRP.

  select single abkrs into lv_abkrs
    from pa0001
   where pernr = p_pernr
     and endda = '99991231'.

  CALL FUNCTION 'PA03_PCR_READ'
    EXPORTING
      F_ABKRS                     = lv_abkrs
     ERROR_NO_ACCOUNTING         = 'X'
   IMPORTING
     F_STATE                     = l_state
     F_CURRENT_PERIOD            = l_curr_period
*     F_CURRENT_YEAR              =
*     F_PERMO                     =
*     F_DATMO                     =
*      F_CURRENT_BEGDA             =
*     F_CURRENT_ENDDA             =
     F_FOLLOWING_PERIOD          = l_foll_period
*     F_FOLLOWING_YEAR            =
*     F_FOLLOWING_BEGDA           =
*     F_FOLLOWING_ENDDA           =
*     F_TWM_DATE                  =
*     F_ABKRS_TEXT                =
*     F_STATE_TEXT                =
*   EXCEPTIONS
*     ABKRS_NO_ACCOUNTING         = 1
*     PCR_DOES_NOT_EXIST          = 2
*     ABKRS_DOES_NOT_EXIST        = 3
*     PERIOD_DOES_NOT_EXIST       = 4
*     OTHERS                      = 5
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
  IF l_STATE = 3.
    get_pabrp =  l_foll_period.
  ELSE.
    get_pabrp = l_curr_period.
  ENDIF.

ENDFORM.                    " GET_PAY_PERIOD

*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_email USING P_EMPST.

  DATA: send_request       TYPE REF TO cl_bcs.
  DATA: text               TYPE bcsy_text.
  DATA: document           TYPE REF TO cl_document_bcs.
  DATA: sender             TYPE REF TO cl_sapuser_bcs.
  DATA: recipient          TYPE REF TO if_recipient_bcs.
  DATA: bcs_exception      TYPE REF TO cx_bcs.
  DATA: sent_to_all        TYPE os_boolean.

  DATA: l_date(10),
        l_time(8).

  DATA: l_subject          TYPE string,
        l_subject_char     TYPE char50,
        l_text             TYPE LINE OF bcsy_text,
        l_nachn            TYPE p0002-nachn,
        l_vorna            TYPE p0002-vorna,
        l_fname(100)       TYPE c,
        l_pernr(10)        TYPE c.

  DATA: l_desc(100).

  DATA:  lc_upd            TYPE string VALUE
        'CAFE Payroll Deduction Eligibility Status Update -',
*        lc_upd            TYPE string VALUE
*        'CAFE Payroll Deduction Eligibility Status Update -'
         lv_eai            TYPE string VALUE
        'CAFE Payroll Outbound EE demographic file -'
      .
  IF P_EMPST = 'E'.
    CONCATENATE lc_upd '(Employment)' INTO lc_upd SEPARATED BY space.
  ELSEIF P_EMPST = 'P'.
    CONCATENATE lc_upd '(Payroll)' INTO lc_upd SEPARATED BY space.
  ENDIF.
  l_subject = lc_upd.
*
  WRITE: sy-datum USING EDIT MASK '__/__/____' TO l_subject_char.
  WRITE: sy-uzeit USING EDIT MASK '__:__:__'   TO l_subject_char+11.
*
  CONCATENATE l_subject l_subject_char INTO l_subject
    SEPARATED BY space.

  TRY.
*     -------- create persistent send request ------------------------
      send_request = cl_bcs=>create_persistent( ).

*     -------- create and set document -------------------------------
*     create document from internal table with text

      WRITE sy-datum MM/DD/YYYY TO l_date.
      WRITE sy-uzeit TO l_time USING EDIT MASK '__:__:__'.
      APPEND l_text TO text. CLEAR l_text.

      CONCATENATE l_date l_time
      '- CAFE Payroll Deduction Eligibility Status Update.'
                                    INTO l_text SEPARATED BY space.
      APPEND l_text TO text. CLEAR l_text.
      APPEND l_text TO text. CLEAR l_text.

      CONCATENATE 'Total :' gv_total_cnt 'records.'
                                   INTO l_text SEPARATED BY space.
      APPEND l_text TO text. CLEAR l_text.
      APPEND l_text TO text. CLEAR l_text.
*
      LOOP AT it_file_email.
        CLEAR l_text.
        l_text = it_file_email-pernr.
        APPEND l_text TO text.
      ENDLOOP.


      LOOP AT gt_e_text INTO l_text.
        APPEND l_text TO text.
      ENDLOOP.

      document = cl_document_bcs=>create_document(
                      i_type    = 'RAW'
                      i_text    = text
                      i_length  = '12'
                      i_subject = 'Subject' ).

*     add document to send request
      CALL METHOD send_request->set_document( document ).

*     change subject line using more than 50 characters
      CALL METHOD send_request->set_message_subject( l_subject ).

*     No delivery status
      CALL METHOD send_request->set_status_attributes( 'N' ).

*     --------- set sender -------------------------------------------
*     note: this is necessary only if you want to set the sender
*           different from actual user (SY-UNAME). Otherwise sender is
*           set automatically with actual user.

      sender = cl_sapuser_bcs=>create( sy-uname ).
      CALL METHOD send_request->set_sender
        EXPORTING
          i_sender = sender.

*     --------- add recipient (e-mail address) -----------------------

      recipient = cl_distributionlist_bcs=>getu_persistent( i_dliname =
      'HR_BN_CAFE'
                                                            i_private =
                                                            space ).
      send_request->set_send_immediately('X').


*     add recipient with its respective attributes to send request
      CALL METHOD send_request->add_recipient
        EXPORTING
          i_recipient = recipient
          i_express   = 'X'.

*     ---------- send document ---------------------------------------
      CALL METHOD send_request->send(
        EXPORTING
          i_with_error_screen = 'X'
        RECEIVING
          result              = sent_to_all ).

      COMMIT WORK.

* -----------------------------------------------------------
* *                     exception handling
* -----------------------------------------------------------
* * replace this very rudimentary exception handling
* * with your own one !!!
* -----------------------------------------------------------
    CATCH cx_bcs INTO bcs_exception.
*      write: / text-001, p_email.
      WRITE:   text-002, bcs_exception->error_type.
      EXIT.

  ENDTRY.

ENDFORM.                    " SEND_EMAIL



*&---------------------------------------------------------------------*
*&      Form  PERSONNEL_GROUP_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_FLAG  text
*----------------------------------------------------------------------*
FORM personnel_group_check  USING    p_flag.

  CLEAR pa0001.

  SELECT SINGLE * INTO CORRESPONDING FIELDS OF pa0001
    FROM pa0001
   WHERE pernr EQ ls_9100-pernr
     AND bukrs IN s_bukrs
     AND werks IN s_werks
     AND btrtl IN s_btrtl
     AND persg IN s_persg
     AND persk IN s_persk
     AND abkrs IN s_abkrs
     AND kostl IN s_kostl
     AND sachz IN s_sachz.

  IF sy-subrc NE 0.
    p_flag = 'N'.
  ENDIF.

ENDFORM.                    " PERSONNEL_GROUP_CHECK



*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL_EAI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_email_eai .

  DATA: send_request       TYPE REF TO cl_bcs.
  DATA: text               TYPE bcsy_text.
  DATA: document           TYPE REF TO cl_document_bcs.
  DATA: sender             TYPE REF TO cl_sapuser_bcs.
  DATA: recipient          TYPE REF TO if_recipient_bcs.
  DATA: bcs_exception      TYPE REF TO cx_bcs.
  DATA: sent_to_all        TYPE os_boolean.

  DATA: l_date(10),
        l_time(8).

  DATA: l_subject          TYPE string,
        l_subject_char     TYPE char50,
        l_text             TYPE LINE OF bcsy_text,
        l_nachn            TYPE p0002-nachn,
        l_vorna            TYPE p0002-vorna,
        l_fname(100)       TYPE c,
        l_pernr(10)        TYPE c.

  DATA: l_desc(100)
      , lv_char(20)        TYPE c
      .
  CONSTANTS:
        lc_subject         TYPE string VALUE
        'Cafe Payroll Deduction Outbound EE demographic file - '
        .

* Set Subject
  l_subject_char = '[STATUS]:AFLAC Inbound file import log - '.
  WRITE: sy-datum USING EDIT MASK '__/__/____' TO lv_char.
  WRITE: sy-datum USING EDIT MASK '__:__:__'   TO lv_char+11.
  l_subject = lc_subject.
  CONCATENATE l_subject lv_char INTO l_subject
    SEPARATED BY space.

  TRY.
*     -------- create persistent send request ------------------------
      send_request = cl_bcs=>create_persistent( ).

*     -------- create and set document -------------------------------
*     create document from internal table with text

      WRITE sy-datum MM/DD/YYYY TO l_date.
      WRITE sy-uzeit TO l_time USING EDIT MASK '__:__:__'.
      APPEND l_text TO text. CLEAR l_text.

      CONCATENATE l_date l_time
      '- Cafe Payroll Deduction Outbound EE demographic file.'
                                    INTO l_text SEPARATED BY space.
      APPEND l_text TO text. CLEAR l_text.
      APPEND l_text TO text. CLEAR l_text.

      CONCATENATE 'Total :' gv_total_cnt 'records.'
                                   INTO l_text SEPARATED BY space.
      APPEND l_text TO text. CLEAR l_text.
      APPEND l_text TO text. CLEAR l_text.


      LOOP AT gt_e_text INTO l_text.
        APPEND l_text TO text.
      ENDLOOP.

      document = cl_document_bcs=>create_document(
                      i_type    = 'RAW'
                      i_text    = text
                      i_length  = '12'
                      i_subject = 'Subject' ).

*     add document to send request
      CALL METHOD send_request->set_document( document ).

*     change subject line using more than 50 characters
      CALL METHOD send_request->set_message_subject( l_subject ).

*     No delivery status
      CALL METHOD send_request->set_status_attributes( 'N' ).

*     --------- set sender -------------------------------------------
*     note: this is necessary only if you want to set the sender
*           different from actual user (SY-UNAME). Otherwise sender is
*           set automatically with actual user.

      sender = cl_sapuser_bcs=>create( sy-uname ).
      CALL METHOD send_request->set_sender
        EXPORTING
          i_sender = sender.

*     --------- add recipient (e-mail address) -----------------------

      recipient = cl_distributionlist_bcs=>getu_persistent( i_dliname =
      'HR_BN_CAFE'
                                                            i_private =
                                                            space ).
      send_request->set_send_immediately('X').


*     add recipient with its respective attributes to send request
      CALL METHOD send_request->add_recipient
        EXPORTING
          i_recipient = recipient
          i_express   = 'X'.

*     ---------- send document ---------------------------------------
      CALL METHOD send_request->send(
        EXPORTING
          i_with_error_screen = 'X'
        RECEIVING
          result              = sent_to_all ).

      COMMIT WORK.

* -----------------------------------------------------------
* *                     exception handling
* -----------------------------------------------------------
* * replace this very rudimentary exception handling
* * with your own one !!!
* -----------------------------------------------------------
    CATCH cx_bcs INTO bcs_exception.
*      write: / text-001, p_email.
      WRITE:   text-002, bcs_exception->error_type.
      EXIT.
  ENDTRY.

ENDFORM.                    " SEND_EMAIL_EAI

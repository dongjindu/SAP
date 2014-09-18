************************************************************************
* Program Name      : ZCA_IF_BATCH_LOG_MGT
* Author            : ByungSung, Bae
* Creation Date     : 2003.09.26.
* Specifications By : ByungSung, Bae
* Development Request No :
* Addl Documentation:
* Description       : Interface & Bauchjob Log Management
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT  zca_if_batch_log_mgt.
*----- Type
TYPE-POOLS : slis, sp01r.

*----- Tables, Views & Structures
TABLES: usr01,
        tbtco,
        tbtcp,
        tsp01,
        ztca_if_section.

*----- Define internal tables
DATA: BEGIN OF it_log OCCURS 0,
        uname     LIKE   ztca_if_section-uname,   "User name
        sflag     LIKE   ztca_if_log-sflag,       "S:Success,F:Failed
        sftxt(7),                                 "Status text
        iftyp     LIKE   ztca_if_section-iftyp,   "I/F Type
        iftxt(10),                                "Text
        tcode     LIKE   ztca_if_section-tcode,   "Transaction code
        zdesc     LIKE   ztca_if_section-zdesc,   "Description
        ifdoc     LIKE   ztca_if_log-ifdoc,       "I/F document
        jobname   LIKE   ztca_if_section-jobname, "Batchjob name
        jobcount  LIKE   ztca_if_log-jobcount,    "Batchjob Count
        total     LIKE   ztca_if_log-total,       "Total count
        zsucc     LIKE   ztca_if_log-zsucc,       "Success count
        error     LIKE   ztca_if_log-error,       "Error count
        erdat     LIKE   ztca_if_log-erdat,       "Processing date
        erzet     LIKE   ztca_if_log-erzet,       "Processing time
        zslno     LIKE   ztca_if_log-zslno,       "I/F Serial No
        zztar     LIKE   ztca_if_section-zztar,   "Target System
        tartxt(15),                               "Text
        zzsrc     LIKE   ztca_if_section-zzsrc,   "Source System
        srctxt(15),                               "Text
        retry     LIKE   ztca_if_section-retry,   "Retry T-code
        distc     LIKE   ztca_if_section-distc,   "Display T-code
      END   OF it_log.

DATA: it_joblist   LIKE tbtcjob_bk OCCURS 0 WITH HEADER LINE.

*----- Define global variable
DATA: w_jobname   LIKE   ztca_if_section-jobname,"For other batch
      w_uname     LIKE   sy-uname,               "For other batch
      w_datum_fr  LIKE   sy-datum,               "For other batch
      w_datum_to  LIKE   sy-datum,               "For other batch
      w_sflag     LIKE   ztca_if_log-sflag.      "Success/Fail

*----- Define range variable
RANGES: r_sflag   FOR   ztca_if_log-sflag.

*----- Define variable for ALV
DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat TYPE slis_t_event WITH HEADER LINE,
       w_selfield TYPE slis_selfield,
       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       w_col_pos  TYPE i,
       w_program  LIKE sy-repid,
       w_top_of_page TYPE slis_t_listheader.

*----- Constants
CONSTANTS : c_formname_top_of_page TYPE slis_formname
                                        VALUE 'TOP_OF_PAGE'.

*----- Macro
DEFINE append_fieldcat.
  &1 = &1 + 1.
  w_fieldcat-col_pos    = &1.
  w_fieldcat-fieldname  = &2.
  w_fieldcat-ref_fieldname   = &3.
  w_fieldcat-key        = &4.
  w_fieldcat-qfieldname = &5.
  w_fieldcat-cfieldname = &6.
  w_fieldcat-seltext_l  = &7.
  w_fieldcat-seltext_m  = &7.
  w_fieldcat-seltext_s  = &7.
  w_fieldcat-outputlen  = &8.
  w_fieldcat-no_out     = &9.
  append w_fieldcat.
  clear : w_fieldcat.
END-OF-DEFINITION.

*----- Define selection screen
SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_datum FOR sy-datum OBLIGATORY DEFAULT sy-datum
                        NO-EXTENSION,
                s_uname FOR ztca_if_section-uname
                        NO INTERVALS NO-EXTENSION,
                s_iftyp FOR ztca_if_section-iftyp NO INTERVALS,
                s_tcode FOR ztca_if_section-tcode,
                s_jobnm FOR ztca_if_section-jobname
                        NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK block2.

SELECTION-SCREEN BEGIN OF BLOCK block3 WITH FRAME TITLE text-t02.
PARAMETERS: r_fail RADIOBUTTON GROUP rd1 DEFAULT 'X',
            r_succ RADIOBUTTON GROUP rd1,
            r_all  RADIOBUTTON GROUP rd1.
SELECTION-SCREEN END OF BLOCK block3.

*----- Initialization
INITIALIZATION.
  PERFORM event_build USING w_eventcat[].

*----- Top-of-page
TOP-OF-PAGE.
  PERFORM top_of_page.

*----- Check input value & Select data
AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM check_input_value.
  PERFORM read_data.

START-OF-SELECTION.
  PERFORM display_data.

*&---------------------------------------------------------------------*
*&      Form  event_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_EVENTCAT[]  text
*----------------------------------------------------------------------*
FORM event_build USING p_w_eventcat TYPE slis_t_event.
  DATA : l_event TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            i_list_type = 0
       IMPORTING
            et_events   = p_w_eventcat.

  READ TABLE p_w_eventcat WITH KEY name = slis_ev_top_of_page
                          INTO l_event.

  IF sy-subrc EQ 0.
    MOVE c_formname_top_of_page TO l_event-form.
    APPEND l_event TO p_w_eventcat.
  ENDIF.
ENDFORM.                    " event_build
*&---------------------------------------------------------------------*
*&      Form  CHECK_INPUT_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_input_value.
  PERFORM check_uname.
  PERFORM check_tcode.
  PERFORM check_jobname.
  PERFORM check_datum.
  PERFORM check_sflag.
  PERFORM check_iftyp.
ENDFORM.                    " CHECK_INPUT_VALUE
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data.
  PERFORM read_if_log.
  PERFORM read_batchjob_log.
  PERFORM read_fixed_value_4_if_log.
ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  check_uname
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_uname.
  SELECT SINGLE * FROM usr01 WHERE bname IN s_uname.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m01.
  ENDIF.

  READ TABLE s_uname INDEX 1.

  IF s_uname-low EQ ''.
    MOVE: '*' TO w_uname.
  ELSE.
    MOVE: s_uname-low TO w_uname.
  ENDIF.
ENDFORM.                    " check_uname
*&---------------------------------------------------------------------*
*&      Form  check_tcode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_tcode.
  SELECT SINGLE * FROM ztca_if_section
                 WHERE tcode IN s_tcode.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m02.
  ENDIF.
ENDFORM.                    " check_tcode
*&---------------------------------------------------------------------*
*&      Form  check_jobname
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_jobname.
  SELECT SINGLE * FROM tbtco WHERE jobname IN s_jobnm.
  IF sy-subrc NE 0.
*    MESSAGE w000(ZZ) WITH TEXT-M03.
  ENDIF.

  READ TABLE s_jobnm INDEX 1.

  IF s_jobnm-low EQ ''.
    MOVE: '*' TO w_jobname.
  ELSE.
    MOVE: s_jobnm-low TO w_jobname.
  ENDIF.
ENDFORM.                    " check_jobname
*&---------------------------------------------------------------------*
*&      Form  READ_IF_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_if_log.
  SELECT a~tcode a~iftyp a~jobname a~zztar a~zzsrc a~retry a~distc
         a~zdesc b~ifdoc b~total   b~zsucc b~error b~erdat b~erzet
         b~zslno b~jobcount        a~uname b~sflag
    INTO CORRESPONDING FIELDS OF TABLE it_log
    FROM ztca_if_section AS a INNER JOIN ztca_if_log AS b
      ON a~tcode = b~tcode
   WHERE a~uname   IN s_uname
     AND a~tcode   IN s_tcode
     AND a~jobname IN s_jobnm
     AND a~iftyp   IN s_iftyp
     AND b~sflag   IN r_sflag
     AND b~erdat   IN s_datum.

  PERFORM get_job_count_number.
ENDFORM.                    " READ_IF_LOG
*&---------------------------------------------------------------------*
*&      Form  READ_DOMAIN_FIXED_VALUE_4_IF_L
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_fixed_value_4_if_log.
*----- Fill I/F type, Target, Source Text
  DATA: lw_dd07v   LIKE   dd07v.

  LOOP AT it_log.
    " Status
    IF     it_log-sflag EQ 'S'.
      it_log-sftxt = 'Success'.
    ELSEIF it_log-sflag EQ 'F'.
      it_log-sftxt = 'Failed'.
    ENDIF.

    " Interface Type
    CLEAR: lw_dd07v.
    MOVE: 'ZIFTYP'     TO lw_dd07v-domname,
          sy-langu     TO lw_dd07v-ddlanguage,
          it_log-iftyp TO lw_dd07v-domvalue_l.

    CALL FUNCTION 'C_DIC_DOMAIN_VALUE_TEXT_READ'
         EXPORTING
              name      = lw_dd07v-domname
              spras     = lw_dd07v-ddlanguage
              value     = lw_dd07v-domvalue_l
         IMPORTING
              text      = lw_dd07v-ddtext
         EXCEPTIONS
              not_found = 1
              OTHERS    = 2.
    it_log-iftxt = lw_dd07v-ddtext.

    " Target System
    CLEAR: lw_dd07v.
    MOVE: 'ZZTAR'      TO lw_dd07v-domname,
          sy-langu     TO lw_dd07v-ddlanguage,
          it_log-zztar TO lw_dd07v-domvalue_l.

    CALL FUNCTION 'C_DIC_DOMAIN_VALUE_TEXT_READ'
         EXPORTING
              name      = lw_dd07v-domname
              spras     = lw_dd07v-ddlanguage
              value     = lw_dd07v-domvalue_l
         IMPORTING
              text      = lw_dd07v-ddtext
         EXCEPTIONS
              not_found = 1
              OTHERS    = 2.
    it_log-tartxt = lw_dd07v-ddtext.

    " Source System
    CLEAR: lw_dd07v.
    MOVE: 'ZZSRC'      TO lw_dd07v-domname,
          sy-langu     TO lw_dd07v-ddlanguage,
          it_log-zzsrc TO lw_dd07v-domvalue_l.

    CALL FUNCTION 'C_DIC_DOMAIN_VALUE_TEXT_READ'
         EXPORTING
              name      = lw_dd07v-domname
              spras     = lw_dd07v-ddlanguage
              value     = lw_dd07v-domvalue_l
         IMPORTING
              text      = lw_dd07v-ddtext
         EXCEPTIONS
              not_found = 1
              OTHERS    = 2.
    it_log-srctxt = lw_dd07v-ddtext.

    MODIFY it_log.
  ENDLOOP.
ENDFORM.                    " READ_DOMAIN_FIXED_VALUE_4_IF_L
*&---------------------------------------------------------------------*
*&      Form  READ_BATCHJOB_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_batchjob_log.
  PERFORM read_batchjob_log_data.
  PERFORM get_registed_entry.
ENDFORM.                    " READ_BATCHJOB_LOG
*&---------------------------------------------------------------------*
*&      Form  check_datum
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_datum.
  READ TABLE s_datum INDEX 1.

  IF      s_datum-low EQ '00000000' AND s_datum-high EQ '00000000'.
    CLEAR: w_datum_fr, w_datum_to.
  ELSEIF  s_datum-low EQ '00000000' AND s_datum-high NE '00000000'.
    CLEAR: w_datum_fr.
    MOVE: s_datum-high TO w_datum_to.
  ELSEIF  s_datum-low NE '00000000' AND s_datum-high EQ '00000000'.
    MOVE s_datum-low TO: w_datum_fr, w_datum_to.
  ELSEIF  s_datum-low NE '00000000' AND s_datum-high NE '00000000'.
    MOVE: s_datum-low  TO w_datum_fr,
          s_datum-high TO w_datum_to.
  ENDIF.
ENDFORM.                    " check_datum
*&---------------------------------------------------------------------*
*&      Form  check_sflag
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_sflag.
  IF     r_fail EQ 'X'.
    MOVE: 'F' TO w_sflag.
    r_sflag-sign = 'I'. r_sflag-option = 'EQ'.
    r_sflag-low = 'F'.
    APPEND r_sflag.
  ELSEIF r_succ EQ 'X'.
    MOVE: 'S' TO w_sflag.
    r_sflag-sign = 'I'. r_sflag-option = 'EQ'.
    r_sflag-low = 'S'.
    APPEND r_sflag.
  ELSEIF r_all EQ 'X'.
*    READ TABLE s_datum INDEX 1.
*    IF sy-subrc NE 0.
*      MESSAGE e000(zz) WITH text-m04 text-m05.
*    ENDIF.
  ENDIF.
ENDFORM.                    " check_sflag
*&---------------------------------------------------------------------*
*&      Form  read_batchjob_log_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_batchjob_log_data.
  DATA: lw_btcselect LIKE btcselect.              "Select condition

  CLEAR: it_joblist, it_joblist[].

  MOVE: w_jobname  TO lw_btcselect-jobname,
        w_uname    TO lw_btcselect-username,
        w_datum_fr TO lw_btcselect-from_date,
        w_datum_to TO lw_btcselect-to_date.

  IF     w_sflag EQ 'F'.
    MOVE: 'X'         TO lw_btcselect-aborted.
  ELSEIF w_sflag EQ 'S'.
    MOVE: 'X'         TO lw_btcselect-finished.
  ELSEIF w_sflag EQ ' '.
    MOVE: 'X'         TO lw_btcselect-aborted,
          'X'         TO lw_btcselect-finished.
  ENDIF.

  CALL FUNCTION 'BP_JOB_SELECT_SM37B'
       EXPORTING
            jobselect_dialog    = 'N'
            jobsel_param_in     = lw_btcselect
       TABLES
            jobselect_joblist_b = it_joblist
       EXCEPTIONS
            invalid_dialog_type = 1
            jobname_missing     = 2
            no_jobs_found       = 3
            selection_canceled  = 4
            username_missing    = 5
            OTHERS              = 6.
ENDFORM.                    " read_batchjob_log_data
*&---------------------------------------------------------------------*
*&      Form  get_registed_entry
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_registed_entry.
  DATA: lt_ztca_if_section LIKE ztca_if_section
                                OCCURS 0 WITH HEADER LINE.

  SELECT * INTO TABLE lt_ztca_if_section
    FROM ztca_if_section
   WHERE tcode IN s_tcode
     AND uname IN s_uname
     AND jobname IN s_jobnm
     AND iftyp EQ 'B'.

  LOOP AT it_joblist.
    READ TABLE lt_ztca_if_section WITH KEY jobname = it_joblist-jobname.
    IF sy-subrc EQ 0.
      CLEAR: it_log.

      MOVE-CORRESPONDING lt_ztca_if_section TO it_log.
      MOVE: it_joblist-jobcount  TO it_log-jobcount,
            it_joblist-strtdate TO it_log-erdat,
            it_joblist-strttime TO it_log-erzet.

      IF     it_joblist-status EQ 'F'.
        MOVE: 'S' TO it_log-sflag.
      ELSEIF it_joblist-status EQ 'A'.
        MOVE: 'F' TO it_log-sflag.
      ENDIF.

      APPEND it_log.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " get_registed_entry
*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM top_of_page.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
*           i_logo             = 'Z_HYUNDAI_LOGO'
*           i_logo             = 'ENJOYSAP_LOGO'
            it_list_commentary = w_top_of_page.
ENDFORM.                    " top_of_page
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  PERFORM build_fieldcat.
  PERFORM build_event.
  PERFORM build_sort.
  PERFORM comment_build USING  w_top_of_page[].
  PERFORM alv_function.
ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcat.
  append_fieldcat :
*   postion    field    ref field     key q-ref   c-ref
*   Text                leg

    w_col_pos 'UNAME'    'UNAME'        'X'  ''      ''
    'Owner'                             '12' '',
    w_col_pos 'ERDAT'    'ERDAT'        'X'  ''      ''
    'Date'                              '10' '',
    w_col_pos 'ERZET'    'ERZET'        'X'  ''      ''
    'Time'                              '8'  '',
    w_col_pos 'IFTXT'    'IFTXT'        'X'  ''      ''
    'Type'                              '10' '',
    w_col_pos 'SFTXT'    'SFTXT'        'X'  ''      ''
    'Status'                            '7' '',
    w_col_pos 'TCODE'    'TCODE'        'X'  ''      ''
    'T-code'                            '20' '',
    w_col_pos 'ZDESC'    'ZDESC'        ''   ''      ''
    'Description'                       '40' '',
    w_col_pos 'TOTAL'    'TOTAL'        ''   ''      ''
    'Total Cnt.'                        '10' '',
    w_col_pos 'ZSUCC'    'ZSUCC'        ''   ''      ''
    'Success'                           '10' '',
    w_col_pos 'ERROR'    'ERROR'        ''   ''      ''
    'Fail'                              '10' '',
    w_col_pos 'TARTXT'   'TARTXT'       ''   ''      ''
    'Target'                            '15' '',
    w_col_pos 'SRCTXT'   'SRCTXT'       ''   ''      ''
    'Source'                            '15' '',
    w_col_pos 'IFDOC'    'IFDOC'        ''   ''      ''
    'IFDOC'                             '20' '',
    w_col_pos 'JOBNAME'  'JOBNAME'      ''   ''      ''
    'Job Name'                          '20' '',
    w_col_pos 'JOBCOUNT' 'JOBCOUNT'     ''   ''      ''
    'Job Count'                         '8'  '',
    w_col_pos 'ZSLNO'    'ZSLNO'        ''   ''      ''
    'I/F Serial No'                     '15' ''.
ENDFORM.                    " build_fieldcat
*&---------------------------------------------------------------------*
*&      Form  build_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_event.
  w_eventcat-name = 'TOP_OF_PAGE'.
  w_eventcat-form = 'TOP_OF_PAGE'.

  APPEND w_eventcat.
ENDFORM.                    " build_event
*&---------------------------------------------------------------------*
*&      Form  build_sort
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sort.
  w_sortcat-spos           = 1.
  w_sortcat-fieldname      = 'UNAME'.
  w_sortcat-tabname        = 'IT_LOG'.
  w_sortcat-up             = 'X'.
  APPEND w_sortcat.

  w_sortcat-spos           = 2.
  w_sortcat-fieldname      = 'ERDAT'.
  w_sortcat-tabname        = 'IT_LOG'.
  w_sortcat-up             = 'X'.
  APPEND w_sortcat.

  w_sortcat-spos           = 3.
  w_sortcat-fieldname      = 'ERZET'.
  w_sortcat-tabname        = 'IT_LOG'.
  w_sortcat-up             = 'X'.
  APPEND w_sortcat.

  w_sortcat-spos           = 4.
  w_sortcat-fieldname      = 'IFTXT'.
  w_sortcat-tabname        = 'IT_LOG'.
  w_sortcat-up             = 'X'.
  APPEND w_sortcat.

  w_sortcat-spos           = 5.
  w_sortcat-fieldname      = 'SFTXT'.
  w_sortcat-tabname        = 'IT_LOG'.
  w_sortcat-up             = 'X'.
  APPEND w_sortcat.

  w_sortcat-spos           = 6.
  w_sortcat-fieldname      = 'TCODE'.
  w_sortcat-tabname        = 'IT_LOG'.
  w_sortcat-up             = 'X'.
  APPEND w_sortcat.
ENDFORM.                    " build_sort
*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_TOP_OF_PAGE[]  text
*----------------------------------------------------------------------*
FORM comment_build USING  lt_top_of_page TYPE slis_t_listheader.
  DATA: ls_line TYPE slis_listheader,
        l_manager(50),
        l_date(50),
        l_list(50),
        l_dsnam LIKE t024d-dsnam,
        l_h_dsnam LIKE t024d-dsnam,
        l_ldate(10),
        l_hdate(10).

*----- Title
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = text-b01.
  APPEND ls_line TO lt_top_of_page.

*----- User
  READ TABLE s_uname INDEX 1.
  IF sy-subrc EQ 0.
    ls_line-typ  = 'S'.
    ls_line-key  = 'Owner: '.
    ls_line-info = s_uname-low.
    APPEND ls_line TO lt_top_of_page.
  ENDIF.

*----- Type
  READ TABLE s_iftyp INDEX 1.
  IF sy-subrc EQ 0.
    ls_line-typ  = 'S'.
    ls_line-key  = 'Interface Type: '.

    CALL FUNCTION 'C_DIC_DOMAIN_VALUE_TEXT_READ'
         EXPORTING
              name      = 'ZIFTYP'
              spras     = sy-langu
              value     = s_iftyp-low
         IMPORTING
              text      = l_list
         EXCEPTIONS
              not_found = 1
              OTHERS    = 2.
    IF sy-subrc <> 0. ENDIF.

    ls_line-info = l_list.
    APPEND ls_line TO lt_top_of_page.
  ELSE.
    ls_line-typ  = 'S'.
    ls_line-key  = 'Interface Type: '.
    ls_line-info = 'All'.
    APPEND ls_line TO lt_top_of_page.
  ENDIF.

*----- T-code
  READ TABLE s_tcode INDEX 1.
  IF sy-subrc EQ 0.
    ls_line-typ  = 'S'.
    ls_line-key  = 'T-code: '.
    CONCATENATE 'FROM: ' s_tcode-low '  TO: ' s_tcode-high INTO l_list.
    ls_line-info = l_list.
    APPEND ls_line TO lt_top_of_page.
  ENDIF.

*----- Jobname
  READ TABLE s_jobnm INDEX 1.
  IF sy-subrc EQ 0.
    ls_line-typ  = 'S'.
    ls_line-key  = 'Jobname: '.
    ls_line-info = s_jobnm-low.
    APPEND ls_line TO lt_top_of_page.
  ENDIF.

*----- Date
  READ TABLE s_datum INDEX 1.
  IF sy-subrc EQ 0.
    ls_line-typ  = 'S'.
    ls_line-key  = 'Date: '.
    CONCATENATE 'FROM: ' s_datum-low '  TO: ' s_datum-high INTO l_list.
    ls_line-info = l_list.
    APPEND ls_line TO lt_top_of_page.
  ENDIF.

*----- Status
  ls_line-typ  = 'S'.
  ls_line-key  = 'Status: '.

  IF     r_succ EQ 'X'.
    ls_line-info = 'Success'.
  ELSEIF r_fail EQ 'X'.
    ls_line-info = 'Fail'.
  ELSEIF r_all EQ 'X'.
    ls_line-info = 'All'.
  ENDIF.

  APPEND ls_line TO lt_top_of_page.
ENDFORM.                    " comment_build
*&---------------------------------------------------------------------*
*&      Form  alv_function
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_function.
  DATA:   l_print_p TYPE slis_print_alv.  " print setting

  CLEAR : w_program.

  MOVE : sy-repid TO w_program.

*** print paramter   ****************************************
  l_print_p-no_coverpage = 'X'.
  l_print_p-no_print_listinfos = 'X'.
  l_print_p-no_change_print_params = 'X'.
  l_print_p-no_print_selinfos = 'X'.
*************************************************************

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*   I_INTERFACE_CHECK                 = ' '
     i_bypassing_buffer                = 'X'
*   I_BUFFER_ACTIVE                   = ' '
     i_callback_program                = w_program
   i_callback_pf_status_set          = 'SET_STATUS'
     i_callback_user_command           = 'USER_COMMAND'
*   I_CALLBACK_TOP_OF_PAGE            = ' '
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
*   I_STRUCTURE_NAME                  =
*   I_BACKGROUND_ID                   = ' '
*  I_GRID_TITLE                      = text-001
*   I_GRID_SETTINGS                   =
*   IS_LAYOUT                         =
     it_fieldcat                       = w_fieldcat[]
*   IT_EXCLUDING                      =
*   IT_SPECIAL_GROUPS                 =
     it_sort                           = w_sortcat[]
*   IT_FILTER                         =
*   IS_SEL_HIDE                       =
*   I_DEFAULT                         = 'X'
      i_save                            = 'A'
*   IS_VARIANT                        =
     it_events                         = w_eventcat[]
*   IT_EVENT_EXIT                     =
     is_print                          = l_print_p
*   IS_REPREP_ID                      =
*   I_SCREEN_START_COLUMN             = 0
*   I_SCREEN_START_LINE               = 0
*   I_SCREEN_END_COLUMN               = 0
*   I_SCREEN_END_LINE                 = 0
*   IT_ALV_GRAPHICS                   =
*   IT_ADD_FIELDCAT                   =
*   IT_HYPERLINK                      =
*   I_HTML_HEIGHT_TOP                 =
*   I_HTML_HEIGHT_END                 =
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER           =
*   ES_EXIT_CAUSED_BY_USER            =
    TABLES
      t_outtab                          = it_log
   EXCEPTIONS
     program_error                     = 1
     OTHERS                            = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " alv_function
*&---------------------------------------------------------------------*
*&      Form  check_iftyp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_iftyp.
  READ TABLE s_iftyp INDEX 1.
  CHECK sy-subrc EQ 0.

  CALL FUNCTION 'C_DIC_DOMAIN_VALUE_TEXT_READ'
       EXPORTING
            name      = 'ZIFTYP'
            spras     = sy-langu
            value     = s_iftyp-low
       EXCEPTIONS
            not_found = 1
            OTHERS    = 2.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m10.
  ENDIF.
ENDFORM.                    " check_iftyp

*---------------------------------------------------------------------*
*       FORM SET_STATUS                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  RT_EXTAB                                                      *
*---------------------------------------------------------------------*
FORM set_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'BASE'.
ENDFORM.                    "

*---------------------------------------------------------------------*
*       FORM user_command                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  UCOMM                                                         *
*  -->  SELFIELD                                                      *
*---------------------------------------------------------------------*
FORM user_command USING ucomm LIKE sy-ucomm
                       selfield TYPE slis_selfield.
  READ TABLE it_log INDEX selfield-tabindex.
  CASE ucomm.
    WHEN '&DISPLAY'.
      PERFORM display_log_rtn.
    WHEN '&RETRY'.
      PERFORM retry_transaction_rtn.
  ENDCASE.
ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LOG_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_log_rtn.

  CASE it_log-iftyp.
    WHEN 'I'.      "Inbound
      PERFORM display_inbound_log.
    WHEN 'O'.      "Outbound
      PERFORM display_outbound_log.
    WHEN 'B'.      "Other Batchjob
      PERFORM display_batchjob_log.
  ENDCASE.
ENDFORM.                    " DISPLAY_LOG_RTN
*&---------------------------------------------------------------------*
*&      Form  display_inbound_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_inbound_log.
  DATA: lw_okay_flg.

  IF it_log-jobname NE '' AND it_log-jobcount NE 0.
    PERFORM display_report_log USING lw_okay_flg.
    CHECK lw_okay_flg IS INITIAL.

    PERFORM display_std_batchjob_log USING lw_okay_flg.
    CHECK lw_okay_flg IS INITIAL.
  ENDIF.

  PERFORM display_report_program.
ENDFORM.                    " display_inbound_log
*&---------------------------------------------------------------------*
*&      Form  display_report_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_report_log USING p_okay_flg.
*----- Step 1 : Batchjob Report Display

  SELECT SINGLE * FROM tbtcp WHERE jobname   = it_log-jobname
                               AND jobcount  = it_log-jobcount
                               AND stepcount = 1.
  IF sy-subrc EQ 0.
    SELECT SINGLE * FROM tsp01 WHERE rqident = tbtcp-listident.
    IF sy-subrc NE 0.
      CHECK 1 = 0.
    ENDIF.

    DATA: lw_id_list TYPE sp01r_id_list WITH HEADER LINE.

    MOVE: sy-sysid        TO lw_id_list-sysid,
          tbtcp-listident TO lw_id_list-id.
    APPEND lw_id_list.

    CALL FUNCTION 'RSPO_RID_SPOOLREQ_LIST'
         EXPORTING
              id_list = lw_id_list[]
              summary = ' '
         EXCEPTIONS
              error   = 1
              OTHERS  = 2.
    IF sy-subrc EQ 0.
      p_okay_flg = 'X'.
    ENDIF.

  ENDIF.
ENDFORM.                    " display_report_log
*&---------------------------------------------------------------------*
*&      Form  display_std_batchjob_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_std_batchjob_log USING p_okay_flg.
*----- Step 2 : Batchjob Log
  CALL FUNCTION 'BP_JOBLOG_SHOW'
       EXPORTING
            client                    = sy-mandt
            jobcount                  = it_log-jobcount
            joblogid                  = ' '
            jobname                   = it_log-jobname
       EXCEPTIONS
            error_reading_jobdata     = 1
            error_reading_joblog_data = 2
            jobcount_missing          = 3
            joblog_does_not_exist     = 4
            joblog_is_empty           = 5
            joblog_show_canceled      = 6
            jobname_missing           = 7
            job_does_not_exist        = 8
            no_joblog_there_yet       = 9
            no_show_privilege_given   = 10
            OTHERS                    = 11.

  IF sy-subrc EQ 0.
    p_okay_flg = 'X'.
  ENDIF.
ENDFORM.                    " display_std_batchjob_log
*&---------------------------------------------------------------------*
*&      Form  display_report_program
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_report_program.
*----- Step 3 : Display Transaction

  IF it_log-distc IS INITIAL.
    MESSAGE e000(zz) WITH text-m06.
  ENDIF.

  CALL TRANSACTION it_log-distc.
ENDFORM.                    " display_report_program
*&---------------------------------------------------------------------*
*&      Form  display_outbound_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_outbound_log.
  DATA: lw_okay_flg.

  IF it_log-jobname NE '' AND it_log-jobcount NE 0.
    PERFORM display_report_log USING lw_okay_flg.
    CHECK lw_okay_flg IS INITIAL.

    PERFORM display_std_batchjob_log USING lw_okay_flg.
    CHECK lw_okay_flg IS INITIAL.
  ENDIF.

  PERFORM display_report_program.
ENDFORM.                    " display_outbound_log
*&---------------------------------------------------------------------*
*&      Form  display_batchjob_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_batchjob_log.
  DATA: lw_okay_flg.

  IF it_log-jobname NE '' AND it_log-jobcount NE 0.
    PERFORM display_report_log USING lw_okay_flg.
    CHECK lw_okay_flg IS INITIAL.

    PERFORM display_std_batchjob_log USING lw_okay_flg.
    CHECK lw_okay_flg IS INITIAL.
  ENDIF.

  PERFORM display_report_program.
ENDFORM.                    " display_batchjob_log
*&---------------------------------------------------------------------*
*&      Form  retry_Transaction_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM retry_transaction_rtn.
  IF it_log-distc IS INITIAL.
    MESSAGE e000(zz) WITH text-m06.
  ENDIF.

  CALL TRANSACTION it_log-retry.
ENDFORM.                    " retry_Transaction_rtn
*&---------------------------------------------------------------------*
*&      Form  GET_JOB_COUNT_NUMBER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_job_count_number.
  DATA: lw_to_time   LIKE sy-uzeit.
  DATA: lw_btcselect LIKE btcselect.              "Select condition
  DATA: lt_joblist   LIKE tbtcjob_bk OCCURS 0 WITH HEADER LINE.


  LOOP AT it_log WHERE iftyp NE 'B'
                   AND jobcount NE ''.
    MOVE: it_log-jobname  TO lw_btcselect-jobname,
          it_log-erdat    TO lw_btcselect-from_date,
          it_log-erzet    TO lw_btcselect-from_time.

    lw_to_time = it_log-erzet + 1.
    IF lw_to_time = '000000'.
      lw_btcselect-to_date = it_log-erdat + 1.
      lw_btcselect-to_time = it_log-erzet + 5.
    ELSE.
      lw_btcselect-to_date = it_log-erdat.
      lw_btcselect-to_time = it_log-erzet + 5.
    ENDIF.

    IF     w_sflag EQ 'F'.
      MOVE: 'X'         TO lw_btcselect-aborted.
    ELSEIF w_sflag EQ 'S'.
      MOVE: 'X'         TO lw_btcselect-finished.
    ELSEIF w_sflag EQ ' '.
      MOVE: 'X'         TO lw_btcselect-aborted,
            'X'         TO lw_btcselect-finished.
    ENDIF.


    CALL FUNCTION 'BP_JOB_SELECT_SM37B'
         EXPORTING
              jobselect_dialog    = 'N'
              jobsel_param_in     = lw_btcselect
         TABLES
              jobselect_joblist_b = lt_joblist
         EXCEPTIONS
              invalid_dialog_type = 1
              jobname_missing     = 2
              no_jobs_found       = 3
              selection_canceled  = 4
              username_missing    = 5
              OTHERS              = 6.
    IF sy-subrc EQ 0.
      READ TABLE lt_joblist INDEX 1.
      IF sy-subrc NE 0.
        MOVE: it_joblist-jobcount  TO it_log-jobcount.
      ENDIF.

      MODIFY it_log.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " GET_JOB_COUNT_NUMBER

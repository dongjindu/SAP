************************************************************************
**                                                                    **
**            C O M P A N Y   C O N F I D E N T I A L                 **
**                                                                    **
**      This program is the property of  HMMA LLC                     **
**      Care should be taken to prevent its unauthorized use.         **
**                                                                    **
************************************************************************
*&---------------------------------------------------------------------*
*& Program: ZAPP703C_WORKORD_MAINT_SHARE_C
*& Type   : Interface / Report ( Copy of ZAPP703C_WORKORD_MAINT_SHARE)
*& Author : Manjunath
*& Title  : Interface Work Order from Legacy System                    *
*&---------------------------------------------------------------------*
* Help Desk Request No:-                                               *
* Issue Log No:-                                                       *
*                                                                      *
*   Requested by:      Daniel Kim                                      *
*   Assigned to:                                                       *
*   Original Request #:                                                *
*   ABAP Analyst:      Manjunath Venkatesh                             *
*                                                                      *
* Business Users:                                           *
*                                                                      *
* Business Requirement Description:-                                   *
*                                                                      *
* Processing Logic:                                                    *
*     < Outline the flow of the main processing logic >
*  Program Execution Logic:-
* Configuration Requirements:                                          *
*     < Document any special config requirements that must exist for   *
*       this program to work correctly >                               *
*                                                                      *
* Program Inputs:                                                      *
*     < Input File Path & Name >                                       *
*     < Any variants program would be typically run with >             *
*                                                                      *
* Program Outputs:
*   Program  submits background job by calling ZAEBPP703C_JOBPROGRAM
*   for every new Work Order.
* Authorization Checks:                                                *
*     < Document if any authorization objects are checked >            *
*                                                                      *
* Direct Update Database Tables:                                       *
*   < No direct updates to SAP tables are allowed.List custom tables > *
*                                                                      *
* Outstanding Issues:                                                  *
*     < If the program is being delivered with any known open issues   *
*       document them here; they could be planned for next release >   *
*                                                                      *
* Instructions on how to test this program:                            *
*     < If this program needs any special inputs like an inbound file  *
*       from an EDI subsystem for testing, document it here >          *
*                                                                      *
* Instructions on how to re-start this program:                        *
*                                                                      *
* Volume Estimates:                                                    *
*                                                                      *
* Frequency of Execution:                                              *
*   o On demand   / Periodically                                       *
*                                                                      *
* Execution Mode:                                                      *
*   o Online      - Transaction Code -                                 *
*                                                                      *
* Other Comments:                                                      *
*                                                                      *
*&----------------------------------------------------------------------
* Modification Logs
************************************************************************
* Date        Developer    RequestNo    Description
* 02/27/07    Manju        UD1K930891   Initial Coding
************************************************************************
REPORT  ZAPP703C_WORKORD_MAINT_SHARE_C   LINE-SIZE  700
                                   MESSAGE-ID zmpp.

TABLES: ztpp_pp_log_head,       " Table of the Interface Log(Header)
        ztpp_pp_log_deta,       " Table of the Interface Log(Detail)
        ztpp_common_vals,       " Table of the last Working Information
        ztpp_wosum    ,         " Table of the WorkOrder Summary
        ztpp_ksbohmm,
        ZTEBPP_DEAL_CONV,
         sscrfields.

INCLUDE <icon>.


DATA: BEGIN OF it_data       OCCURS 0.
        INCLUDE STRUCTURE    ztpp_ksbohmm .
DATA:   p_flag               TYPE c,
        wo                   TYPE c,
        h_c                  TYPE c,
        exist                TYPE c,
        material             LIKE mara-matnr,
      END OF it_data.

DATA: it_msg                 LIKE TABLE OF bdcmsgcoll  WITH HEADER LINE,
      it_bdcdata             LIKE TABLE OF bdcdata     WITH HEADER LINE,
      it_error               LIKE TABLE OF it_data     WITH HEADER LINE,
      it_sum                 LIKE TABLE OF ztpp_wosum  WITH HEADER LINE,
      wa_mode                TYPE c VALUE 'N',
      wa_date                LIKE sy-datum   ,
      wa_date2               LIKE sy-datum   ,
      wa_check(4)            TYPE c       ,
      wa_cnt                 TYPE i       ,
      wa_month               LIKE ztpp_wosum-wo_ser,
      wa_hdcnt               TYPE i       ,
      wa_clcnt               TYPE i       ,
      wa_flg                 TYPE c       ,
      wa_error               TYPE c       ,
      wa_number              LIKE ztpp_pp_log_head-logkey,
      wa_data                LIKE it_data ,
      wa_seq                 LIKE ztpp_pp_log_deta-sequence,
      wa_wkdate              LIKE sy-datum,
      wa_tabix               LIKE sy-tabix.

DATA: global_job LIKE tbtcjob OCCURS 0 WITH HEADER LINE.
DATA: it_joblist LIKE tbtcjob OCCURS 0 WITH HEADER LINE.
DATA: global_start_date LIKE tbtcstrt OCCURS 0 WITH HEADER LINE.
DATA: global_step_tbl LIKE tbtcstep OCCURS 0 WITH HEADER LINE.

DATA: jobc LIKE tbtcjob-jobcount ,
      jobn LIKE  tbtcjob-jobname ,
      immediate LIKE btch0000-char1 VALUE 'X'.
DATA: wa_count(4) TYPE n,
      c_prog LIKE sy-repid.

data : begin of it_tab occurs 0,
       OLD_DEALER like ZTEBPP_DEAL_CONV-OLD_DEALER,
       NEW_DEALER like ZTEBPP_DEAL_CONV-NEW_DEALER,
      end of it_tab.

data :GRID1  TYPE REF TO CL_GUI_ALV_GRID,
      G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER ,
       GT_SFLIGHT TYPE TABLE OF SFLIGHT.

DATA: WA_EMPTY  TYPE C.                            "  UD1K913223

*  ALV Decalaration
*---// ALV general field
data : it_fieldcat     type lvc_t_fcat with header line,
       it_fieldcat_fi  type lvc_t_fcat with header line,
       it_fieldcat_co  type lvc_t_fcat with header line,
       it_fieldname    type slis_t_fieldcat_alv,
       it_sort         type lvc_t_sort with header line,
       it_fieldcat_det type lvc_t_fcat with header line. "/Detail


data : wa_is_layout type lvc_s_layo, "/The Layout Structure
       w_fieldname  like line of it_fieldcat.

data : wa_variant type disvariant. "for parameter IS_VARIANT

data: wa_save    type c   value 'A'.   "for Parameter I_SAVE
DATA:  grid_container    type ref to cl_gui_custom_container,
       alv_grid          type ref to cl_gui_alv_grid,
       w_repid like sy-repid.


RANGES: s_jobnam FOR tbtcp-jobname,
        s_pronam FOR tbtcp-progname,
        s_date FOR tbtcp-sdldate,
        s_time FOR tbtcp-sdltime.

*SELECTION-SCREEN BEGIN OF SCREEN 1100 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS: p_coment          TYPE c  AS CHECKBOX   DEFAULT 'X',
            p_prod            TYPE c  AS CHECKBOX   DEFAULT ' '.
SELECTION-SCREEN END OF BLOCK b1.
*SELECTION-SCREEN END OF SCREEN 1100.

*at selection-output.
*call screen 9000.

*************** DO NOT USE!!!! *****************************************
DATA: it_rec                  LIKE TABLE OF mara ,
      p_tcode                 LIKE  tstc-tcode                ,
      p_cmode                 TYPE  c                         ,
      p_pmode                 TYPE  c   VALUE   'N'           ,
      wa_filename             LIKE  rlgrap-filename,
      wa_filetype             LIKE  rlgrap-filetype VALUE 'DAT',
      wa_bdcgroup             LIKE  sy-uname.          " APQI-GROUPID
************************************************************************

INITIALIZATION.

  sscrfields-functxt_01 = 'DEALER CONVERSION TABLE DISPLAY'.
  SELECTION-SCREEN FUNCTION KEY 1.

LOAD-OF-PROGRAM.

AT SELECTION-SCREEN.
  IF sscrfields-ucomm = 'FC01'.
    CALL SCREEN 0100 STARTING AT 10  1.
*                     ENDING   AT 50 5     .
  ENDIF.

START-OF-SELECTION.
* call screen 100.
  CHECK p_coment = 'X' .
  PERFORM get_data.
  PERFORM write_start      .
  PERFORM pre_checking_data.               " Sequence Quantity Check..
  PERFORM pre_checking_data2.              " OLD Monthly MOD-Qty Check.
  PERFORM bdc_processing   .
* PERFORM create_finishlog .           " Finish Log ....
  PERFORM WRITE_VIN_SPEC.

END-OF-SELECTION.
  DATA: variant              LIKE indx-srtfd VALUE 'ZISD03_01' ,
        eventid              LIKE tbtcjob-eventid.

  CHECK p_coment = 'X' .
  PERFORM write_success.
  PERFORM write_end.
  IF WA_EMPTY IS INITIAL.                                   "UD1K913223
    UPDATE ztpp_common_vals SET: dates = wa_date
                               item1 = wa_date2
                         WHERE jobs  = 'ZAPP703C_WORKORDER_MAINT' .
  ENDIF.                                                    "UD1K913223

*  CHECK wa_error = space .
*  WA_DATE = WA_DATE - 1  .
*  EXPORT p_date = wa_date TO DATABASE indx(zz) ID variant.
*  eventid = 'ZISD03_01' .
*
*  CALL FUNCTION 'BP_EVENT_RAISE'
*       EXPORTING
*            eventid                = eventid
*       EXCEPTIONS
*            bad_eventid            = 1
*            eventid_does_not_exist = 2
*            eventid_missing        = 3
*            raise_failed           = 4
*            OTHERS                 = 5.
*
*  IF sy-subrc <> 0.
*    WRITE AT: /001(50) ' Event Call Process was failed.. ',
*              /001(50) '    *** Event ID: ZISD03_01 ***  '.
*  ENDIF.

  PERFORM delete_joblist.
** changed by Fuorng on 11/02/05
*  PERFORM delete_data   .
** end of change

  INCLUDE zcpp103_common_routine .


*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  " Set the Check month Value...
  CLEAR WA_EMPTY.                                           "UD1K913223
  wa_check =  sy-datum+2(4).

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_data
    FROM ztpp_ksbohmm .
  wa_tabix = sy-tabix.

  IF sy-subrc NE 0.
    WA_EMPTY = 'X'.                                         "UD1K913223
    MESSAGE i001 WITH text-015.
    STOP.
  ENDIF.

  SELECT MIN( chg_date ) MAX( chg_date ) INTO (wa_date, wa_date2)
    FROM ztpp_ksbohmm .

*  SELECT SINGLE *  FROM ztpp_common_vals
*   WHERE jobs = 'ZAPP703C_WORKORDER_MAINT' .
*
*  IF sy-subrc = 0 AND ztpp_common_vals-item1(8) < wa_date2 .
*  ELSE.
*    MESSAGE i001 WITH text-015.
*    DELETE FROM ztpp_ksbohmm CLIENT SPECIFIED WHERE mandt = sy-mandt.
*    p_coment = ' '            .
*    STOP.
*  ENDIF.
ENDFORM.                    " GET_DATA

*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_processing.
  DATA: l_modsum             LIKE ztpp_ksbohmm-modqty,
        l_cont               TYPE i        .

  CLEAR:  l_cont.

  CHECK p_coment = 'X' .
  SORT it_data BY wo_ser nation dealer extc intc .
  PERFORM get_logserial.
  LOOP AT it_data.
    AT NEW wo_ser.
*      c_prog = 'ZAPP703C_JOBPROGRAM'.
      c_prog = 'ZAEBPP703C_JOBPROGRAM'.
      PERFORM job_create.
    ENDAT.
    l_cont = l_cont + 1.
  ENDLOOP.

  PERFORM job_check.
ENDFORM.                    " BDC_PROCESSING

*&---------------------------------------------------------------------*
*&      Form  check_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_COLOR  text
*----------------------------------------------------------------------*
FORM check_color USING    pa_color.
  pa_color = 'N'  .
  IF it_data-extc = '***' AND it_data-intc = '***'.
    pa_color = 'Y' .
  ENDIF.
ENDFORM.                    " check_color

*&---------------------------------------------------------------------*
*&      Form  CHECK_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_EXIST  text
*----------------------------------------------------------------------*
FORM check_material USING    pa_exist  pa_material .
  SELECT SINGLE matnr INTO pa_material
    FROM mara
   WHERE matnr = pa_material .

  IF sy-subrc = 0.
    pa_exist = 'Y' .
  ELSE.
    pa_exist = 'N' .
  ENDIF.
ENDFORM.                    " CHECK_MATERIAL

*&---------------------------------------------------------------------*
*&      Form  create_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0274   text
*      -->P_TXT_001  text
*----------------------------------------------------------------------*
FORM create_log USING    pa_type  pa_step  pa_text  pa_key .
  wa_seq = wa_seq + 1 .
  IF wa_seq = 1       .
    PERFORM get_logserial.               " Log Number Generation........
    ztpp_pp_log_head-logkey   = wa_number   .
    ztpp_pp_log_head-programm = 'ZAPP703C_REPROCESS' .
    ztpp_pp_log_head-logtype  = pa_type     .
    ztpp_pp_log_head-jobtype  = sy-batch    .
    ztpp_pp_log_head-logstep  = pa_step     .
    ztpp_pp_log_head-msg      = pa_text     .
    ztpp_pp_log_head-ldate    = sy-datum    .
    ztpp_pp_log_head-ltime    = sy-uzeit    .
    ztpp_pp_log_head-luser    = sy-uname    .
    INSERT INTO ztpp_pp_log_head VALUES ztpp_pp_log_head .
  ENDIF.

  " Log Detail Creation
  ztpp_pp_log_deta-logkey   = wa_number    .
  ztpp_pp_log_deta-sequence = wa_seq      .
  ztpp_pp_log_deta-logtype  = pa_type     .
  ztpp_pp_log_deta-keydata  = pa_key      .
  INSERT INTO ztpp_pp_log_deta VALUES ztpp_pp_log_deta .
ENDFORM.                    " create_log

*&---------------------------------------------------------------------*
*&      Form  GET_LOGSERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_logserial.
  " Log Head Creation..
  CALL FUNCTION 'NUMBER_GET_NEXT'
       EXPORTING
            nr_range_nr             = '01'
            object                  = 'ZLOG'
       IMPORTING
            number                  = wa_number
       EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            buffer_overflow         = 7
            OTHERS                  = 8.
ENDFORM.                    " GET_LOGSERIAL

*&---------------------------------------------------------------------*
*&      Form  create_finishlog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_finishlog.
  " Step 12: Finished creation process.....
  PERFORM create_log USING 'E' 12 text-013  space .
ENDFORM.                    " create_finishlog

*&---------------------------------------------------------------------*
*&      Form  create_steplog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_steplog.
  IF wa_error = space.
    " Step 11: Successfully creation...
    PERFORM create_log USING 'S' 11 text-011  it_data.
  ELSE.
    " Step 11: Error creation...
    PERFORM create_log USING 'E' 11 text-012  it_data.
  ENDIF.
ENDFORM.                    " create_steplog


*&---------------------------------------------------------------------*
*&      Form  check_workorder
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_workorder   USING pa_flag.
  DATA: l_worder                 LIKE TABLE OF it_data WITH HEADER LINE,
        l_cqty                   LIKE ztpp_ksbohmm-initqty,  " Color Qty
        l_conf                   LIKE TABLE OF zspp_vin_value
                                                 WITH HEADER LINE.

  l_worder[] = it_data[].
  CLEAR: l_conf, l_conf[].
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            input  = 'P_MOD_QTY'
       IMPORTING
            output = l_conf-atinn.
  APPEND l_conf.

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            input  = 'P_SEQ_QTY'
       IMPORTING
            output = l_conf-atinn.
  APPEND l_conf.

  LOOP AT l_worder WHERE wo_ser = it_data-wo_ser  AND
                         nation = it_data-nation  AND
                         dealer = it_data-dealer  AND
                         extc   NE '***'          .
    CONCATENATE it_data-wo_ser it_data-nation it_data-dealer
                it_data-extc   it_data-intc   INTO l_worder-material.


    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              object       = l_worder-material
              ctype        = '001'
         TABLES
              val_table    = l_conf
         EXCEPTIONS
              no_data      = 1
              error_mode   = 2
              error_object = 3
              error_value  = 4
              OTHERS       = 5.

    " Compare the Qty....
    READ TABLE l_conf WITH KEY atnam = 'P_SEQ_QTY' .
    l_cqty = l_conf-atwrt.
    IF l_cqty > l_worder-modqty.
      " Error.....
      pa_flag = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " check_workorder
*&---------------------------------------------------------------------*
*&      Form  JOB_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM job_create.
  REFRESH: global_job,
           global_step_tbl.
  CLEAR: global_job,
         global_step_tbl.
  wa_count = wa_count + 1.
  CONCATENATE 'WO_CRE_'         wa_count INTO jobn.

  global_job-jobname = jobn.             "S_PROG.
  global_job-jobclass = 'B'.             "<====== 레벨 C로 지정
  global_job-newflag = 'O'.
  global_step_tbl-program = 'RSBTCPT3'.  "dummy step
  global_step_tbl-typ = 'A'.             "프로그램실행단계식별
  global_step_tbl-status = 'P'.          "scheduled
  global_step_tbl-authcknam = sy-uname.
  APPEND global_step_tbl.
  APPEND global_job.

  CALL FUNCTION 'BP_JOB_CREATE'
     EXPORTING
       job_cr_dialog             = 'N'
       job_cr_head_inp           = global_job
*       ADK_MODE                  = FALSE
    IMPORTING
      job_cr_head_out           = global_job
*       JOB_CR_STDT_OUT           = GLOBAL_START_DATE
     TABLES
       job_cr_steplist           = global_step_tbl
    EXCEPTIONS
        cant_create_job           = 1
        invalid_dialog_type       = 2
        invalid_job_data          = 3
        job_create_canceled       = 4
        OTHERS                    = 5.
  IF sy-subrc <> 0.
  ENDIF.

  jobc = global_job-jobcount.
  jobn = global_job-jobname.

*  SUBMIT zapp703c_jobprogram AND RETURN
  SUBMIT ZAEBPP703C_JOBPROGRAM AND RETURN
         WITH p_woser EQ it_data-wo_ser
         WITH p_log   EQ wa_number
         VIA JOB jobn NUMBER jobc .

  CONCATENATE 'IEQ' jobn     INTO s_jobnam.  APPEND s_jobnam.
*  CALL FUNCTION 'JOB_SUBMIT'
*       EXPORTING
*            authcknam = sy-uname
*            jobcount  = jobc
*            jobname   = jobn
*            report    = c_prog
*       EXCEPTIONS
*            OTHERS    = 4.

  CALL FUNCTION 'JOB_CLOSE'
       EXPORTING
            jobcount  = jobc
            jobname   = jobn
            strtimmed = immediate
       EXCEPTIONS
            OTHERS    = 4.

ENDFORM.                    " JOB_PROCESS
*&---------------------------------------------------------------------*
*&      Form  JOB_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM job_check.
  DO.
    CLEAR it_joblist.
    REFRESH it_joblist.
    CALL FUNCTION 'BP_FIND_JOBS_WITH_PROGRAM'
         EXPORTING
              abap_program_name             = c_prog  "'YIPP_TEST16'
              dialog                        = 'N'
         TABLES
              joblist                       = it_joblist
         EXCEPTIONS
              no_jobs_found                 = 1
              program_specification_missing = 2
              invalid_dialog_type           = 3
              job_find_canceled             = 4
              OTHERS                        = 5.

    IF sy-subrc <> 0.     EXIT.     ENDIF.

    READ TABLE it_joblist WITH KEY jobname(6) = 'WO_CRE'
                                   status = 'S'.
    IF sy-subrc = 0.
      CONTINUE.
    ELSE.
      READ TABLE it_joblist WITH KEY jobname(6) = 'WO_CRE'
                                     status = 'R'.
      IF sy-subrc = 0.
        CONTINUE.
      ELSE.
        EXIT.
      ENDIF.
    ENDIF.
  ENDDO.
ENDFORM.                    " JOB_CHECK

*&---------------------------------------------------------------------*
*&      Form  delete_joblist
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_joblist.
*  s_pronam = 'IEQZAPP703C_JOBPROGRAM'.  APPEND s_pronam.
  s_pronam = 'IEQZAEBPP703C_JOBPROGRAM'.  APPEND s_pronam.


  SUBMIT zapp703c_jobdelete AND RETURN
         WITH s_jobnam   IN s_jobnam
         WITH s_pronam   IN s_pronam .
ENDFORM.                    " delete_joblist

*&---------------------------------------------------------------------*
*&      Form  PRE_CHECKING_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pre_checking_data.
  DATA: l_matnr              LIKE mara-matnr,
        l_qty(5)             TYPE n         ,
        l_line               TYPE i         ,
        l_vals               LIKE TABLE OF zspp_vin_value
                                                       WITH HEADER LINE,
        l_data               LIKE TABLE OF it_data     WITH HEADER LINE,
        L_dealer(2) type c,
        l_deal(1) type c.

  SORT it_data BY wo_ser nation dealer extc intc .
  l_data[] = it_data[].
  DELETE l_data WHERE extc = '***' .

  LOOP AT l_data .

    CLEAR: l_vals, l_vals[].
    CLEAR: l_data-s219,  l_vals-atwrt, l_vals-atinn, l_vals-atbez,
           l_vals-atwtb, l_vals-zflag, l_data-moye, L_dealer .

* Begin of changes - UD1K930891
     clear l_deal.
     l_deal =  l_data-dealer.
    CALL FUNCTION 'ZFEB_GET_NEW_DEALER_CODE'
         EXPORTING
              new_DEALER = l_deal
         IMPORTING
              old_DEALER = L_dealer.

    if not l_dealer is initial.
      l_data-dealer =  L_dealer.
    else.
     message  e001 with 'Dealer conversion error'.
    endif.


* End of changes -   UD1K930891

    CONCATENATE l_data-wo_ser l_data-nation l_data-dealer
                l_data-extc   l_data-intc   INTO l_data-material.

    l_vals-atnam = 'P_SEQ_QTY'.          APPEND  l_vals.
    l_vals-atnam = 'P_MOD_QTY'.          APPEND  l_vals.

    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              object       = l_data-material
              ctype        = '001'
         TABLES
              val_table    = l_vals
         EXCEPTIONS
              no_data      = 1
              error_mode   = 2
              error_object = 3
              error_value  = 4
              OTHERS       = 5.

    IF sy-subrc = 0.
      READ TABLE  l_vals WITH KEY atnam = 'P_SEQ_QTY' .     "INDEX  1 .
      l_qty = l_vals-atwrt.
      IF l_qty > l_data-modqty .
        l_data-s219 = text-003.
        l_data-initqty = l_qty.
        l_data-moye = 'E'     .
      ENDIF.
    ENDIF.
    MODIFY l_data.
  ENDLOOP.

  DELETE l_data WHERE moye = ' '.
  DESCRIBE TABLE l_data LINES l_line.
  CHECK l_line > 0.
*  CLEAR: p_coment .
  WRITE  / text-111.
  SKIP 2 .
  ULINE AT (118) .
  WRITE AT: /001(024) text-102     ,
             025(001) sy-vline     ,
             026(015) text-103     ,
             041(001) sy-vline     ,
             042(014) text-104     ,
             056(001) sy-vline     ,
             057(061) text-105     ,
             118(001) sy-vline     .
  ULINE AT: /(118) .

  LOOP AT l_data .
    WRITE AT: /001(009) l_data-wo_ser,
               010(001) sy-vline     ,
               011(003) l_data-nation,
               014(001) sy-vline     ,
               015(002) l_data-dealer,
               017(001) sy-vline     ,
               018(003) l_data-extc  ,
               021(001) sy-vline     ,
               022(003) l_data-intc  ,
               025(001) sy-vline     ,
               026(015) l_data-modqty,
               041(001) sy-vline     ,
               042(014) l_data-initqty,
               056(001) sy-vline     ,
               057(061) l_data-s219  ,
               118(001) sy-vline     .
  ENDLOOP.
  ULINE AT: /(118) .
*  PERFORM write_end.
ENDFORM.                    " PRE_CHECKING_DATA

*&---------------------------------------------------------------------*
*&      Form  PRE_CHECKING_DATA2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pre_checking_data2.
  DATA: l_matnr              LIKE mara-matnr,
        l_qty(5)             TYPE n         ,
        l_line               TYPE i         ,
        l_flag               TYPE c         ,
        l_vals               LIKE TABLE OF zspp_vin_value
                                                       WITH HEADER LINE,
        l_data               LIKE TABLE OF it_data     WITH HEADER LINE.

  SORT it_data BY wo_ser nation dealer extc intc .
  l_data[] = it_data[].
  DELETE l_data WHERE extc = '***' .

  l_vals-atnam = 'P_MOD_QTY'.          APPEND  l_vals.

  CHECK p_prod IS INITIAL   .
  LOOP AT l_data .  " where wo_ser < wa_check .   " p_month.
    CHECK l_data-wo_ser+1(4) <= wa_check .         " Monthly Pack Check
    CLEAR: l_data-s219,  l_vals-atwrt, l_vals-atinn, l_vals-atbez,
           l_vals-atwtb, l_vals-zflag, l_data-moye .

    CONCATENATE l_data-wo_ser l_data-nation l_data-dealer
                l_data-extc   l_data-intc   INTO l_data-material.

    SELECT SINGLE matnr INTO l_data-material
      FROM mara
     WHERE matnr = l_data-material
       AND mtart = 'WOCL'          .

    IF sy-subrc NE 0 AND sy-datum >= '20040601' .
      l_data-s219 = text-016.
      l_data-initqty = l_qty.
      l_data-moye = 'E'     .
      MODIFY l_data.
      CONTINUE.
    ENDIF.

    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              object       = l_data-material
              ctype        = '001'
         TABLES
              val_table    = l_vals
         EXCEPTIONS
              no_data      = 1
              error_mode   = 2
              error_object = 3
              error_value  = 4
              OTHERS       = 5.

    IF sy-subrc NE 0.
      l_flag = 'X' .
    ELSE.
      CLEAR: l_flag.
    ENDIF.
    READ TABLE  l_vals  INDEX  1 .
    l_qty = l_vals-atwrt.   CLEAR: l_vals-atwrt.
    IF l_qty < l_data-modqty  OR l_flag = 'X'  .
      l_data-s219 = text-016.
      l_data-initqty = l_qty.
      l_data-moye = 'E'     .
    ENDIF.
    MODIFY l_data.
  ENDLOOP.

  DELETE l_data WHERE moye NE 'E'.
  DESCRIBE TABLE l_data LINES l_line.
  CHECK l_line > 0.
*  CLEAR: p_coment .
  WRITE  / text-101.
  SKIP 2 .
  ULINE AT (118) .
  WRITE AT: /001(024) text-102     ,
             025(001) sy-vline     ,
             026(015) text-103     ,
             041(001) sy-vline     ,
             042(014) text-104     ,
             056(001) sy-vline     ,
             057(061) text-105     ,
             118(001) sy-vline     .
  ULINE AT: /(118) .

  LOOP AT l_data .
    WRITE AT: /001(009) l_data-wo_ser,
               010(001) sy-vline     ,
               011(003) l_data-nation,
               014(001) sy-vline     ,
               015(002) l_data-dealer,
               017(001) sy-vline     ,
               018(003) l_data-extc  ,
               021(001) sy-vline     ,
               022(003) l_data-intc  ,
               025(001) sy-vline     ,
               026(015) l_data-modqty,
               041(001) sy-vline     ,
               042(014) l_data-initqty,
               056(001) sy-vline     ,
               057(061) l_data-s219  ,
               118(001) sy-vline     .
  ENDLOOP.
  ULINE AT: /(118) .
  PERFORM write_end.
** changed by furong on 11/02/05
*  DELETE FROM ztpp_ksbohmm CLIENT SPECIFIED WHERE mandt = sy-mandt.
** end of change
ENDFORM.                    " PRE_CHECKING_DATA2

*&---------------------------------------------------------------------*
*&      Form  write_start
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_start.
  WRITE :/ text-201 .
  WRITE AT: /001(030) text-202                    ,
             031(010) sy-datum                    ,
             042(010) sy-uzeit                    .
  WRITE :/ text-201 .
  SKIP 1.
ENDFORM.                    " write_start

*&---------------------------------------------------------------------*
*&      Form  WRITE_END
*&---------------------------------------------------------------------*
FORM write_end.
  GET TIME.
  SKIP 2.
  WRITE :/ text-203 .
  WRITE AT: /001(030) text-204                    ,
             031(010) sy-datum                    ,
             042(010) sy-uzeit                    .
  WRITE :/ text-203 .
ENDFORM.                    " WRITE_END

*&---------------------------------------------------------------------*
*&      Form  write_success
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_success.
  DATA: l_data               LIKE TABLE OF it_data     WITH HEADER LINE,
        l_woser              LIKE ztpp_wosum-wo_ser ,
        l_nation(5)          TYPE c  ,
        l_sum_hd             TYPE i  ,
        l_sum_cl             TYPE i  ,
        BEGIN OF l_check     OCCURS 0,
          nation(5)          TYPE c  ,
          hd_cnt             TYPE i  ,
          cl_cnt             TYPE i  ,
        END OF l_check.

  WRITE : / text-205.
  l_data[] = it_data[].
  LOOP AT l_data.
    CONCATENATE l_data-nation l_data-dealer INTO l_data-material .
    MODIFY l_data.
  ENDLOOP.

  SORT l_data BY material  extc .
  READ TABLE l_data INDEX 1     .
  l_check-nation = l_nation = l_data-material    .

  LOOP AT l_data.
    IF l_data-material = l_nation.
      IF l_data-extc  = '***'   .
        l_check-hd_cnt = l_check-hd_cnt + 1.
      ELSE.
        l_check-cl_cnt = l_check-cl_cnt + 1.
      ENDIF.
    ELSE.
      APPEND l_check.
      CLEAR: l_check.
      l_check-nation = l_nation = l_data-material.
      IF l_data-extc  = '***'   .
        l_check-hd_cnt = 1 .
      ELSE.
        l_check-cl_cnt = 1.
      ENDIF.
    ENDIF.
  ENDLOOP.
  APPEND l_check.

  SKIP 2 .
  WRITE AT: /001(017) text-401     .
  ULINE AT  /(043) .
  WRITE AT: /001(017) text-302     ,
             018(001) sy-vline     ,
             019(012) text-303     ,
             031(001) sy-vline     ,
             032(011) text-304     ,
             043(001) sy-vline     .
  ULINE AT: /(043) .

  LOOP AT l_check.
    l_sum_hd = l_sum_hd + l_check-hd_cnt .
    l_sum_cl = l_sum_cl + l_check-cl_cnt .
    WRITE AT: /001(017) l_check-nation,
               018(001) sy-vline      ,
               019(012) l_check-hd_cnt,
               031(001) sy-vline      ,
               032(011) l_check-cl_cnt,
               043(001) sy-vline      .
  ENDLOOP.
  ULINE AT: /(043) .
  WRITE AT: /001(017) text-305      ,
             018(001) sy-vline      ,
             019(012) l_sum_hd      ,
             031(001) sy-vline      ,
             032(011) l_sum_cl      ,
             043(001) sy-vline      .
  ULINE AT: /(043) .

  " Work Order Summary data Check...
*  READ TABLE it_data INDEX 1.
*  clear: l_data, l_data[], l_check-hd_cnt, l_check-cl_cnt,
*         l_sum_hd, l_sum_cl, L_CHECK, L_CHECK[] .
*
*  CONCATENATE it_data-wo_ser(5) '%'  INTO l_woser.
*  SELECT * INTO TABLE it_sum
*    FROM ztpp_wosum
*   WHERE wo_ser LIKE l_woser .
*
*  LOOP AT it_sum .
*    move-corresponding it_sum to l_data.
*    CONCATENATE l_data-nation l_data-dealer INTO l_data-material .
*    append l_data.
*  ENDLOOP.

*  SORT l_data BY material  extc .
*  READ TABLE l_data INDEX 1     .
*  l_check-nation = l_nation = l_data-material    .
*
*  LOOP AT l_data.
*    IF l_data-material = l_nation.
*       l_check-cl_cnt = l_check-cl_cnt + 1.
*    ELSE.
*      APPEND l_check.
*      CLEAR: l_check.
*      l_check-nation = l_nation = l_data-material.
*      l_check-cl_cnt = 1.
*    ENDIF.
*  ENDLOOP.
*  APPEND l_check.

*  SKIP 2 .
*  WRITE AT: /001(023) text-402     .
*  ULINE AT  /(043) .
*  WRITE AT: /001(017) text-302     ,
*             018(001) sy-vline     ,
*             019(012) text-303     ,
*             031(001) sy-vline     ,
*             032(011) text-304     ,
*             043(001) sy-vline     .
*  ULINE AT: /(043) .
*
*  LOOP AT l_check.
*    l_sum_hd = l_sum_hd + l_check-hd_cnt .
*    l_sum_cl = l_sum_cl + l_check-cl_cnt .
*    WRITE AT: /001(017) l_check-nation,
*               018(001) sy-vline      ,
*               019(012) l_check-hd_cnt,
*               031(001) sy-vline      ,
*               032(011) l_check-cl_cnt,
*               043(001) sy-vline      .
*  ENDLOOP.
*  ULINE AT: /(043) .
*  WRITE AT: /001(017) text-305      ,
*             018(001) sy-vline      ,
*             019(012) l_sum_hd      ,
*             031(001) sy-vline      ,
*             032(011) l_sum_cl      ,
*             043(001) sy-vline      .
*  ULINE AT: /(043) .
ENDFORM.                    " write_success

*&---------------------------------------------------------------------*
*&      Form  DELETE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_data.
  DELETE FROM ztpp_ksbohmm CLIENT SPECIFIED WHERE mandt = sy-mandt .
ENDFORM.                    " DELETE_DATA

*---------------------------------------------------------------------*
*       FORM WRITE_VIN_SPEC                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM WRITE_VIN_SPEC.
  DATA: BEGIN OF LT_DATA OCCURS 0,
        WO_SER LIKE ZTPP_KSBOHMM-WO_sER,
        NATION LIKE ZTPP_KSBOHMM-NATION,
        DEALER LIKE ZTPP_KSBOHMM-DEALER,
        bmdl like ZTPP_KSBOHMM-bmdl,
        ocnn like ZTPP_KSBOHMM-ocnn,
        work_order like ztpp_input_plan-work_order,
        END OF LT_DATA.

  DATA: L_ATINN LIKE CABN-ATINN,
        LT_AUSP LIKE TABLE OF AUSP WITH HEADER LINE.

  RANGES: S_WORKORDER FOR ZTPP_INPUT_PLAN-WORK_ORDER.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE Lt_data
   FROM ztpp_ksbohmm
    WHERE EXTC = '***'.

  LOOP AT LT_DATA.
    CONCATENATE LT_DATA-WO_sER LT_DATA-NATION LT_DATA-DEALER INTO
             LT_DATA-work_order.
    modify lt_data.

    S_WORKORDER-LOW = LT_DATA-work_order.
    S_WORKORDER-SIGN = 'I'.
    S_WORKORDER-OPTION = 'EQ'.
    APPEND S_WORKORDER.
    CLEAR: S_WORKORDER, lt_data.
  ENDLOOP.

  SELECT SINGLE ATINN INTO L_ATINN
    FROM CABN
    WHERE ATNAM = 'P_VIN_SPEC'.

  SELECT * INTO TABLE LT_AUSP
    FROM AUSP
    WHERE OBJEK IN S_WORKORDER
      AND ATINN = L_ATINN
      AND KLART = '001'.

  LOOP AT LT_data.
    read table lt_ausp with key objek = lt_data-work_order.
    WRITE: / LT_data-work_order , LT_AUSP-ATWRT,
         lt_data-bmdl, lt_data-ocnn.
  ENDLOOP.
ENDFORM.                    " WRITE_VIN_SPEC
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

*&      Module  move_data  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PAI INPUT.

ENDMODULE.                 " PAI  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
 CASE sy-ucomm.

 when 'CANC'.
  leave to screen 0.

 endcase.
ENDMODULE.                 " user_command_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.
leave to screen 0.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  PBO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PBO OUTPUT.
SET pf-status 'M100'.
set titlebar 'S1'.
select OLD_DEALER NEW_DEALER into table it_tab
       from ZTEBPP_DEAL_CONV.

 if grid_container is initial.
    perform create_container.
    perform set_attributes.
*    perform build_field_catalog.
    perform create_grid.
  endif.

  PERFORM build_fieldcat.

ENDMODULE.                 " PBO  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcat.
*PERFORM fill_field_category USING :
*    'S'   'FIELDNAME'          'OLD_DEALER',
*    ' '   'JUST'               'R',
*    'E'   'REPTEXT_DDIC'       'OLD DEALER',
*    'S'   'FIELDNAME'          'NEW_DEALER',
*    ' '   'JUST'               'L',
*    'E'   'REPTEXT_DDIC'       'NEW DEALER'.

ENDFORM.                    " build_fieldcat
*&---------------------------------------------------------------------*
*&      Form  alv_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_display.
*g_repid = sy-repid.
*CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*   EXPORTING
*     i_callback_program                = g_repid
*     i_background_id                   = 'ALV_BACKGROUND'
**     I_GRID_TITLE                      =
**     I_GRID_SETTINGS                   =
*     is_layout                         = g_st_layout
*     it_fieldcat                       = g_it_fieldcat[]
**     IT_EXCLUDING                      =
**     IT_SPECIAL_GROUPS                 =
*     it_sort                           = g_it_sort
**     IT_FILTER                         =
**     IS_SEL_HIDE                       =
**     I_DEFAULT                         = 'X'
*     i_save                            = g_save
*     is_variant                        = alv_variant
*     it_events                         = g_it_events[]
**     IT_EVENT_EXIT                     =
*     is_print                          = alv_print
*   TABLES
*     t_outtab                          = it_data[]
*   EXCEPTIONS
*     program_error                     = 1
*     OTHERS                            = 2.
*
*  IF sy-subrc <> 0.
*    CASE sy-subrc.
*      WHEN 1.
*        MESSAGE s002.
*      WHEN 2.
*        MESSAGE s003.
*    ENDCASE.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.

ENDFORM.                    " alv_display
*&---------------------------------------------------------------------*
*&      Form  fill_field_category
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2200   text
*      -->P_2201   text
*      -->P_2202   text
*----------------------------------------------------------------------*


*FORM fill_field_category  USING    p_cat p_fnam p_con.
*  IF p_cat = 'S'.
*    CLEAR g_st_fieldcat.
*  ENDIF.
*
*  DATA lv_col(40).
*  FIELD-SYMBOLS <fs>.
*  CONCATENATE 'G_ST_FIELDCAT-' p_fnam INTO lv_col.
*  ASSIGN (lv_col)     TO <fs>.
*  MOVE    p_con       TO <fs>.
*
*  IF p_cat = 'E'.
*    APPEND g_st_fieldcat TO g_it_fieldcat.
*  ENDIF.
*
*
*ENDFORM.                    " fill_field_category
*&---------------------------------------------------------------------*
*&      Form  create_container
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_container.
create object grid_container
            exporting container_name = 'MY_CONT'
            exceptions
             cntl_error = 1
             cntl_system_error = 2
             create_error = 3
             lifetime_error = 4
             lifetime_dynpro_dynpro_link = 5.

  if sy-subrc ne 0.
    w_repid = sy-repid.
    call function 'POPUP_TO_INFORM'
         exporting
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'The control can not be created'.
  endif.

  create object alv_grid
         exporting i_parent = grid_container
                   i_appl_events = 'X'.

ENDFORM.                    " create_container
*&---------------------------------------------------------------------*
*&      Form  set_attributes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes.
it_fieldcat-fieldname = 'OLD_DEALER'.
it_fieldcat-REPTEXT = 'OLD DEALER'.
append it_fieldcat.

it_fieldcat-fieldname = 'NEW_DEALER'.
it_fieldcat-REPTEXT = 'NEW DEALER'.
append it_fieldcat.

ENDFORM.                    " set_attributes
*&---------------------------------------------------------------------*
*&      Form  create_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_grid.

  wa_is_layout-detailtitl  = 'Update One Time Vendor Data'.



  call method alv_grid->set_table_for_first_display
         exporting
                   is_layout        = wa_is_layout
                   i_save           = wa_save
                   is_variant       = wa_variant
                   i_default        = space
         changing  it_fieldcatalog  = it_fieldcat[]
                   it_outtab        = it_tab[].

ENDFORM.                    " create_grid

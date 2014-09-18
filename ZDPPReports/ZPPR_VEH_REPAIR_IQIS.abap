************************************************************************
* Program Name      : ZPPR_VEH_REPAIR_IQIS
* Author            : Furong Wang
* Creation Date     : 11/2012
* Specifications By : BS Bae
* Pattern           :
* Development Request
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT zppr_veh_repair_iqis MESSAGE-ID zmmm.

TYPE-POOLS: slis .

CONSTANTS: c_error VALUE 'E',
            c_succ VALUE 'S'.
*            c_no VALUE 'No',
*            c_yes VALUE 'Yes'.,


DATA: it_email LIKE TABLE OF ztpp_rep_veh WITH HEADER LINE.
DATA: w_dest(30).

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS: s_date FOR sy-datum NO-EXTENSION OBLIGATORY.
PARAMETERS:     p_days(3).
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------
INITIALIZATION.
  PERFORM init_data.

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------

*  PERFORM check_multi_run.

  PERFORM locking_rtn USING sy-repid.

  PERFORM get_data.

  PERFORM dequeue_prg.

*&---------------------------------------------------------------------*
*&      Form  init_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_data.
  s_date-option = 'BT'.
  s_date-sign = 'I'.
  s_date-low = sy-datum - 7.
  s_date-high = sy-datum.
  APPEND s_date.
  p_days = 90.
ENDFORM.                    " init_data

*&---------------------------------------------------------------------*
*&      Form  CHECK_MULTI_RUN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_multi_run .
  DATA: l_flag(1).

  DO 30 TIMES.
    CALL FUNCTION 'ENQUEUE_EPROG'
      EXPORTING
        mode_trdir     = 'E'
        programm       = sy-cprog
*       X_PROGRAMM     = ' '
        _scope         = '1'
*       _WAIT          = ' '
*       _COLLECT       = ' '
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc = 0.
      CLEAR: l_flag.
      EXIT.
    ELSE.
      l_flag = 'X'.
      WAIT UP TO 6 SECONDS.
    ENDIF.
  ENDDO.
  IF l_flag = 'X'.
    MESSAGE e001 WITH 'The Program is running by other'.
  ENDIF.
ENDFORM.                    " CHECK_MULTI_RUN
*&---------------------------------------------------------------------*
*&      Form  GET_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  DATA: l_date LIKE sy-datum.
  DATA: BEGIN OF lt_date OCCURS 90,
        date LIKE sy-datum,
        END OF lt_date.

  l_date = sy-datum - p_days.
  DELETE FROM ztpp_rep_veh_h WHERE ref_date <= l_date.

  WRITE: /1(10) 'Repair Date'.
  WRITE:15(10) 'Count'.
  WRITE:25(15) 'New MH Count'.
  WRITE:40(10) 'Email'.
  WRITE:50(5) 'Flag'.
  WRITE:55(40) 'Message'.

  l_date = s_date-low.
  WHILE l_date <= s_date-high.
    lt_date-date = l_date.
    APPEND lt_date.
    l_date = l_date + 1.
  ENDWHILE.

  LOOP AT lt_date.
    l_date = lt_date-date.
    PERFORM call_rfc USING l_date.
  ENDLOOP.
ENDFORM.                    "GET_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_RFC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_rfc USING p_date.

* DATA: lt_part TYPE STANDARD TABLE OF ztpp_repair_part,
*       lw_part TYPE ztpp_repair_part.

  DATA: lt_veh  TYPE STANDARD TABLE OF ztpp_rep_veh WITH HEADER LINE,
        lt_vehh LIKE ztpp_rep_veh_h OCCURS 0 WITH HEADER LINE,
        lt_mh LIKE TABLE OF ztpp_rep_mh WITH HEADER LINE,
        lw_veh TYPE ztpp_rep_veh,
        lw_mh TYPE ztpp_rep_mh.

  DATA: l_date LIKE sy-datum,
        l_date_dis(10),
        l_cn_veh(5) TYPE n,
        l_cn_mh(5) TYPE n,
*        l_body(9),
        l_flag_email(1),
        l_meg(40),
        w_result,
        w_mes TYPE zmsg,
        l_msgtxt(60)..

  l_date = p_date.
  CLEAR: w_result, w_mes.

  w_dest = 'WMPP01'.
  CALL FUNCTION 'ZFPP_VEH_REPAIR_IQIS'
    DESTINATION w_dest
    EXPORTING
      i_date                = l_date
    IMPORTING
      o_result              = w_result
      o_mes                 = w_mes
    TABLES
      t_data                = lt_veh
    EXCEPTIONS
      communication_failure = 1  MESSAGE l_msgtxt
      system_failure        = 2  MESSAGE l_msgtxt.

  IF sy-subrc = 0 AND w_result = 'S'.
    READ TABLE lt_veh INTO lw_veh INDEX 1.
    lw_veh-zresult = 'S'.
    lw_veh-zedat = sy-datum.
    lw_veh-zetim = sy-uzeit.
    lw_veh-zuser = sy-uname.
    SORT lt_veh BY model body_serial plant_no shop_cd ref_main
                   ref_sub part_cd ref_cd ref_loc ref_date ref_time.

    LOOP AT lt_veh.
      AT NEW ref_time.
        CLEAR: lw_veh-zseqno.
      ENDAT.

      lw_veh-zseqno = lw_veh-zseqno + 1.

      MODIFY lt_veh FROM lw_veh TRANSPORTING zseqno zedat zetim
                                             zuser zresult.

      CLEAR: lt_vehh.
      MOVE-CORRESPONDING lt_veh TO lt_vehh.
      MOVE: lw_veh-zseqno  TO lt_vehh-zseqno,
            lw_veh-zresult TO lt_vehh-zresult,
            lw_veh-zedat   TO lt_vehh-zedat,
            lw_veh-zetim   TO lt_vehh-zetim,
            lw_veh-zuser   TO lt_vehh-zuser.
      APPEND lt_vehh.
    ENDLOOP.

    DESCRIBE TABLE lt_veh LINES l_cn_veh.

    INSERT ztpp_rep_veh_h FROM TABLE lt_vehh
           ACCEPTING DUPLICATE KEYS.
    IF sy-subrc <> 0.
      l_meg = 'Vehicle repair history table update error'.
      WRITE l_date TO l_date_dis MM/DD/YY.
      WRITE: /1(10) l_date_dis.
      WRITE:15(10) l_cn_veh  NO-ZERO.
      WRITE:25(15) l_cn_mh  NO-ZERO.
      WRITE:40(10) l_flag_email.
      WRITE:50(5) c_error.
      WRITE:55(40) l_meg.
      ROLLBACK WORK.
      EXIT.
    ENDIF.

    COMMIT WORK AND WAIT.

    DELETE FROM ztpp_rep_veh WHERE ref_date = l_date.
*    IF sy-subrc <> 0.
*      l_meg = 'Vehicle repair table deletion error'.
*      WRITE l_date TO l_date_dis MM/DD/YY.
*      WRITE: /1(10) l_date_dis.
*      WRITE:15(10) l_cn_veh  NO-ZERO.
*      WRITE:25(15) l_cn_mh  NO-ZERO.
*      WRITE:40(10) l_flag_email.
*      WRITE:50(5) c_error.
*      WRITE:55(40) l_meg.
*      ROLLBACK WORK.
*      EXIT.
*    ENDIF.


    INSERT ztpp_rep_veh FROM TABLE lt_veh
           ACCEPTING DUPLICATE KEYS.
    IF sy-subrc <> 0.
      l_meg = 'Vehicle repair table update error'.
      WRITE l_date TO l_date_dis MM/DD/YY.
      WRITE: /1(10) l_date_dis.
      WRITE:15(10) l_cn_veh  NO-ZERO.
      WRITE:25(15) l_cn_mh  NO-ZERO.
      WRITE:40(10) l_flag_email.
*    WRITE:50(5) w_result.
      WRITE:50(5) c_error.
      WRITE:55(40) l_meg.
      ROLLBACK WORK.
      EXIT.
    ENDIF.

    IF sy-subrc = 0.
      WRITE l_date TO l_date_dis MM/DD/YY.
      WRITE: /1(10) l_date_dis.
      WRITE:15(10) l_cn_veh  NO-ZERO.
** Insert to MH table
      REFRESH: lt_mh.
      LOOP AT lt_veh.
        SELECT SINGLE * INTO lw_mh
         FROM ztpp_rep_mh
         WHERE model     = lt_veh-model
           AND plant_no  = lt_veh-plant_no
*           and shop      = lt_veh-shop_cd
           AND part_cd   = lt_veh-part_cd
           AND def_cd    = lt_veh-ref_cd.
        IF sy-subrc <> 0.
          it_email = lt_veh.
          COLLECT it_email.
          MOVE-CORRESPONDING lt_veh TO lw_mh.
          lw_mh-def_cd = lt_veh-ref_cd.
          lw_mh-erdat  = lt_veh-zedat.
          lw_mh-erzet  = lt_veh-zetim.
          lw_mh-ernam  = sy-uname.
          COLLECT lw_mh INTO lt_mh.
        ENDIF.
        CLEAR: lw_mh, lt_veh.
      ENDLOOP.
      DESCRIBE TABLE lt_mh LINES l_cn_mh.
      IF l_cn_mh > 0.
        INSERT ztpp_rep_mh FROM TABLE lt_mh
               ACCEPTING DUPLICATE KEYS.
        IF sy-subrc = 0.
          PERFORM email USING l_flag_email.
          WRITE:25(15) l_cn_mh  NO-ZERO.
          WRITE:40(10) l_flag_email.
          WRITE:50(5) c_succ.
          COMMIT WORK.
        ELSE.
          l_meg = 'Manhour table update error'.
          WRITE:25(15) l_cn_mh NO-ZERO.
          WRITE:40(10) l_flag_email.
          WRITE:50(5) c_error.
          WRITE:55(40) l_meg.
          ROLLBACK WORK.
        ENDIF.
      ELSE.
        WRITE l_date TO l_date_dis MM/DD/YY.
        WRITE: /1(10) l_date_dis.
        WRITE:15(10) l_cn_veh.
        WRITE:25(15) l_cn_mh.
        WRITE:40(10) l_flag_email.
        WRITE:50(5) c_succ.
        COMMIT WORK.
      ENDIF.
*** Insert to parts table
*    REFRESH: lt_part.
*    LOOP AT t_data.
*      IF t_data-rep_part_ind = 'Y'.
*        MOVE-CORRESPONDING t_data TO lw_part.
*        lw_part-erdat = sy-datum.
*        lw_part-erzet = sy-uzeit.
*        lw_part-ernam = sy-uname.
*        lw_part-seq = '01'.
*        APPEND lw_part TO lt_part.
*      ENDIF.
*      CLEAR: lw_part, t_data.
*    ENDLOOP.
*    IF lt_part[] IS NOT INITIAL.
*      MODIFY ztpp_repair_part FROM TABLE lt_part.
*      IF sy-subrc = 0.
*        o_result = 'S'.
*        o_mes = 'Successfully updated'.
*        COMMIT WORK.
*      ELSE.
*        o_result = 'E'.
*        o_mes = 'Parts table update error'.
*        ROLLBACK WORK.
*        EXIT.
*      ENDIF.
*    ELSE.
*      o_result = 'S'.
*      o_mes = 'Successfully updated'.
*      COMMIT WORK.
*    ENDIF.
    ELSE.
      l_meg = 'Vehicle Repair table update error'.
      WRITE l_date TO l_date_dis MM/DD/YY.
      WRITE: /1(10) l_date_dis.
      WRITE:15(10) l_cn_veh  NO-ZERO.
      WRITE:25(15) l_cn_mh  NO-ZERO.
      WRITE:40(10) l_flag_email.
      WRITE:50(5) c_error.
      WRITE:55(40) l_meg.
      ROLLBACK WORK.
    ENDIF.
  ELSE.
    WRITE l_date TO l_date_dis MM/DD/YY.
    WRITE: /1(10) l_date_dis.
    WRITE:15(10) l_cn_veh  NO-ZERO.
    WRITE:25(15) l_cn_mh  NO-ZERO.
    WRITE:40(10) l_flag_email.
    WRITE:50(5) c_error.
    IF w_mes IS INITIAL.
      w_mes = l_msgtxt.
    ENDIF.
    WRITE:55(40) w_mes.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " CALL_RFC
*&---------------------------------------------------------------------*
*&      Form  EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM email USING p_result.
  DATA: lt_body TYPE ccrctt_text_tab,
         lw_body TYPE solisti1.
  DATA: l_subject TYPE p15_text150,
        l_p_rec_type  LIKE  somlreci1-rec_type.
  DATA: l_rver LIKE somlreci1-receiver VALUE 'ZPP_REPAIR'.
  DATA: l_result TYPE zresult.

  MOVE 'The Missing Repair Master' TO lw_body.
  APPEND lw_body TO lt_body.
  CLEAR: lw_body.
  MOVE '=========================' TO lw_body.
  APPEND lw_body TO lt_body.
  CLEAR: lw_body.

  MOVE: 'Mod' TO lw_body+0(5),
         'Body Ser' TO lw_body+5(8),
         'Plant no' TO lw_body+13(5),
         'Rep Date' TO lw_body+20(10),
         'Part Code' TO lw_body+30(15),
         'Part Content' TO lw_body+45(40),
         'Def Code' TO lw_body+85(10),
         'Defect Description' TO lw_body+95(40),
         'Ind' TO lw_body+135(5),
         'Rep Part' TO lw_body+140(20),
         'Rep Dept' TO lw_body+160(10),
         'Shift' TO lw_body+170(5).

  APPEND lw_body TO lt_body.
  CLEAR: lw_body.

  MOVE:    '-----' TO lw_body+0(5),
           '--------' TO lw_body+5(8),
           '--------' TO lw_body+13(5),
           '----------' TO lw_body+20(10),
           '---------------' TO lw_body+30(15),
           '--------------------' TO lw_body+45(20),
           '--------------------' TO lw_body+65(20),
           '----------' TO lw_body+85(10),
           '--------------------' TO lw_body+95(20),
           '--------------------' TO lw_body+115(20),
           '---' TO lw_body+135(5),
           '--------------------' TO lw_body+140(20),
           '----------' TO lw_body+160(10),
           '-----' TO lw_body+170(5).

  APPEND lw_body TO lt_body.
  CLEAR: lw_body.

  LOOP AT it_email.
    MOVE: it_email-model TO lw_body+0(5),
          it_email-body_serial TO lw_body+5(8),
          it_email-plant_no TO lw_body+13(5),
          it_email-ref_date TO lw_body+20(10),
          it_email-part_cd TO lw_body+30(15),
          it_email-part_cont TO lw_body+45(40),
          it_email-ref_cd TO lw_body+85(10),
          it_email-ref_desc TO lw_body+95(40),
          it_email-rep_part_ind TO lw_body+135(5),
          it_email-rep_part TO lw_body+140(20),
          it_email-rep_dept TO lw_body+160(10),
          it_email-shift TO lw_body+170(5).

    APPEND lw_body TO lt_body.
    CLEAR: lw_body.
  ENDLOOP.

*  CALL METHOD zcit_cl_email=>send(
*    i_subject = 'The Missing Repair Maste'
*    i_rec_type = 'C'
*    i_receiver = l_rver
*    t_body = lt_body
*    o_result = l_result ).

  CALL METHOD zcit_cl_email=>send
    EXPORTING
      i_subject  = 'The Missing Repair Master'
      i_rec_type = 'C'
      i_receiver = l_rver
      t_body     = lt_body
    IMPORTING
      o_result   = l_result.

  p_result = l_result.

ENDFORM.                    " EMAIL
*&---------------------------------------------------------------------*
*&      Form  DEQUEUE_PRG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dequeue_prg .
  CALL FUNCTION 'DEQUEUE_EPROG'
    EXPORTING
      mode_trdir     = 'E'
      programm       = sy-cprog
*     X_PROGRAMM     = ' '
      _scope         = '1'
*     _WAIT          = ' '
*     _COLLECT       = ' '
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
ENDFORM.                    " DEQUEUE_PRG
*&---------------------------------------------------------------------*
*&      Form  LOCKING_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_REPID  text
*----------------------------------------------------------------------*
FORM locking_rtn USING p_repid.
  PERFORM check_lock_object  USING p_repid.
  PERFORM check_enqueue_read USING p_repid.
  PERFORM check_batchjob     USING p_repid.
ENDFORM.                    " LOCKING_RTN
*&---------------------------------------------------------------------*
*&      Form  CHECK_LOCK_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_REPID  text
*----------------------------------------------------------------------*
FORM check_lock_object USING p_repid.
  CALL FUNCTION 'ENQUEUE_EPROG'
    EXPORTING
      mode_trdir     = 'E'
      programm       = p_repid
      x_programm     = ' '
      _scope         = '1'
      _wait          = ' '
      _collect       = ' '
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    IF sy-batch EQ 'X'.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      LEAVE PROGRAM.
    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_LOCK_OBJECT
*&---------------------------------------------------------------------*
*&      Form  CHECK_ENQUEUE_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_REPID  text
*----------------------------------------------------------------------*
FORM check_enqueue_read USING p_repid.
  DATA: l_garg        LIKE seqg3-garg,
        l_gname       LIKE seqg3-gname,
        l_lock_number LIKE sy-tabix.

  DATA: lt_lock TYPE TABLE OF seqg3 WITH HEADER LINE.

  MOVE: sy-mandt     TO l_garg(3),
        p_repid      TO l_garg+3,
        p_repid      TO l_gname.

  CALL FUNCTION 'ENQUEUE_READ'
    EXPORTING
      gclient               = sy-mandt
      gname                 = l_gname
      garg                  = l_garg
      guname                = ' '
      local                 = ' '
      fast                  = ' '
    IMPORTING
      number                = l_lock_number
    TABLES
      enq                   = lt_lock
    EXCEPTIONS
      communication_failure = 1
      system_failure        = 2
      OTHERS                = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF l_lock_number > 1.
    IF sy-batch EQ 'X'.
      MESSAGE i601(mc) WITH sy-msgv1.
      LEAVE PROGRAM.
    ELSE.
      MESSAGE e601(mc) WITH sy-msgv1.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_ENQUEUE_READ
*&---------------------------------------------------------------------*
*&      Form  CHECK_BATCHJOB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_batchjob USING p_repid.
  DATA: lt_joblist LIKE tbtcjob OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'BP_FIND_JOBS_WITH_PROGRAM'
    EXPORTING
      abap_program_name             = p_repid
      dialog                        = 'N'
      status                        = 'R'
    TABLES
      joblist                       = lt_joblist
    EXCEPTIONS
      no_jobs_found                 = 1
      program_specification_missing = 2
      invalid_dialog_type           = 3
      job_find_canceled             = 4
      OTHERS                        = 5.

  IF sy-batch EQ 'X'.
    READ TABLE lt_joblist INDEX 2.
    IF sy-subrc EQ 0.
      MESSAGE i601(mc) WITH lt_joblist-sdluname.
      LEAVE PROGRAM.
    ENDIF.
  ELSE.
    READ TABLE lt_joblist INDEX 1.
    IF sy-subrc EQ 0.
      MESSAGE e601(mc) WITH lt_joblist-sdluname.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_BATCHJOB

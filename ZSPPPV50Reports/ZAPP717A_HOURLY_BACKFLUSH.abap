************************************************************************
* Program Name      : ZAPP717A_HOURLY_BACKFLUSH
* Author            : Won-seob Kim
* Creation Date     : 2003.12.10.
* Specifications By :
* Pattern           : Report 1-1
* Development Request No :
* Addl Documentation:
* Description       : Hourly BackFlush Reporting Point
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 04.01.04   WSKIM                     Change to Parallel Mode.
* 02/01/2005 WSKIM        UD1K914114   BIP/BIW BF Point logic *
*# 1, 02/18/2005 wskim        UD1K914497   Spec change backflush error
*modification
************************************************************************

INCLUDE  zapp717a_hourly_backflush_top.
INCLUDE  zapp717a_hourly_backflush_para.


AT SELECTION-SCREEN.
  PERFORM screen_input_check.

INITIALIZATION.
  PERFORM initialization.

START-OF-SELECTION.
*It hasn't to run two program of backflush at the same time
  PERFORM running_program_check USING flag.
  CHECK flag <> 'E'.
*Run Preparation
  PERFORM get_data.

END-OF-SELECTION.
  IF flag EQ 'E'.
    WRITE : / 'The program is running!!!!'.
    PERFORM dequeue_object USING flag.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  return_01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM return_01 USING taskname.
  REFRESH : rt_bfst.
  RECEIVE RESULTS FROM FUNCTION 'Z_FPP_BF_PRE_PARALLEL'
          TABLES
           re_bfst                   = rt_bfst
*          re_bfst                   = tot_bfst
          EXCEPTIONS
          communication_failure       = 1
          system_failure              = 2
          RESOURCE_FAILURE            = 3
          OTHERS                      = 4.

  CHECK sy-subrc = 0.
  READ TABLE rt_bfst INDEX 1.
  MOVE rt_bfst TO tot_bfst.
  APPEND tot_bfst.
*  APPEND LINES OF rt_bfst  TO tot_bfst.

  rcv_jobs  = rcv_jobs + 1.
ENDFORM.                                                    " return_01
*&---------------------------------------------------------------------*
*&      Form  screen_input_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_input_check.
  CHECK NOT p_a IS INITIAL.
  IF s_plnum-low IS INITIAL.
    MESSAGE e001 WITH 'Plan order is a required field'.
  ENDIF.

ENDFORM.                    " screen_input_check
*&---------------------------------------------------------------------*
*&      Form  write_results
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_results TABLES tot_bfst STRUCTURE it_bfst
                          rt_bfst09 STRUCTURE it_bfst
                          rt_bfst18 STRUCTURE it_bfst.
*Check
  DATA : z_time(18),z_dat(18),z_tim(18).
  FIELD-SYMBOLS : <date> TYPE ANY,
                  <time> TYPE ANY.
  DATA : c_time LIKE sy-timlo,
         c_date LIKE sy-datum,  "Current day
         num(2) TYPE n.
  CLEAR : c_time,c_date,num.
  GET TIME.
  c_time = sy-timlo - 1800.
  IF c_time  > sy-timlo.
    c_date = sy-datum - 1.
  ELSE.
    c_date = sy-datum.
  ENDIF.
  WRITE : / '********************************************************'.
  WRITE : / 'Results through RP Point(01~17)'.
  LOOP AT tot_bfst.
    num = 01.
    DO 18 TIMES.
      CONCATENATE 'TOT_BFST' '-' 'BFP' num '_TIM' INTO z_time.
      ASSIGN (z_time) TO <time>.
      CONCATENATE 'TOT_BFST' '-' 'BFP' num '_DAT' INTO z_dat.
      ASSIGN (z_dat) TO  <date> .

      IF <date> > c_date . "Current day > Checkday
        PERFORM write_paralle TABLES tot_bfst.
        EXIT.
      ELSEIF <date> = c_date AND <time> > c_time.
        PERFORM write_paralle TABLES tot_bfst.
        EXIT.
*      ELSEIF <date> = '00000000' . "Current day > Checkday
*        EXIT.
      ELSE.
        num = num + 1.
      ENDIF.
    ENDDO.
    CLEAR : z_time,z_dat.
  ENDLOOP.

  WRITE : / '********************************************************'.
  WRITE : / 'Reversal :   '.
  LOOP AT rt_bfst09.
    PERFORM write_reversal TABLES rt_bfst09.
  ENDLOOP.

  WRITE : / '********************************************************'.
  WRITE : / 'Sgin off : RP18 '.
  LOOP AT rt_bfst18.
    WRITE : / sy-tabix,
              rt_bfst18-plant,rt_bfst18-model,rt_bfst18-body_ser,
              rt_bfst18-plan_ord,rt_bfst18-bfp18_flg.
  ENDLOOP.

*Parallel processing : Trigger SD GOOD Issue program :
*by Swkim 02.14.2004
  IF  NOT p_d IS INITIAL.
    PERFORM sd_trigger_event.
  ENDIF.

ENDFORM.                    " write_results
*&---------------------------------------------------------------------*
*&      Form  RP18_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BFST18  text
*----------------------------------------------------------------------*
FORM rp18_process TABLES   p_bfst18 STRUCTURE it_bfst
                           p_rt     STRUCTURE it_bfst
                  USING   lp_budat.

  DATA : p_field(18),num(2) TYPE n,che_num(2) TYPE n.
  FIELD-SYMBOLS : <pfield> TYPE ANY,<ffield> TYPE ANY.
  CLEAR : num,che_num.

  CLEAR wa_bfst.

  PERFORM get_data_signoff TABLES   p_bfst18.

*Check status of Each point at ZTPP_BFST table
  LOOP AT p_bfst18.
    num = 1.
    DO 17 TIMES.
      CONCATENATE 'P_BFST18' '-' 'BFP' num '_FLG' INTO p_field.
      ASSIGN (p_field) TO <pfield>.
      IF <pfield> EQ '00' OR <pfield> EQ 'Y'.
        num = num + 1.
      ELSE.
        DELETE TABLE p_bfst18 FROM p_bfst18.
        EXIT.
      ENDIF.
    ENDDO.
  ENDLOOP.

  LOOP AT p_bfst18.
*    READ TABLE p_rt WITH KEY plant    = p_bfst18-plant
*                             model    = p_bfst18-model
*                             body_ser = p_bfst18-body_ser
*                             plan_ord = p_bfst18-plan_ord
*                             bfp17_flg = '01'.
*    CHECK sy-subrc <> 0.
    MOVE-CORRESPONDING p_bfst18 TO wa_bfst.
    CALL FUNCTION 'Z_FPP_BF_PRE_PARALLEL'
*    STARTING NEW TASK taskname DESTINATION IN GROUP 'PG_BF'
*      PERFORMING return_01 ON END OF TASK
      EXPORTING
        lp_bfst                     = wa_bfst
        lp_num                      = rp18
        lp_budat                    = lp_budat
      TABLES
        re_bfst                     = rt_bfst18
        re_bfst_r                   = rt_bfst09
      EXCEPTIONS
        communication_failure       = 1
        system_failure              = 2
        RESOURCE_FAILURE            = 3 .
*          OTHERS                      = 4.
  ENDLOOP.
  COMMIT WORK .
ENDFORM.                    " RP18_PROCESS
*&---------------------------------------------------------------------*
*&      Form  processing_single_mode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BFST  text
*----------------------------------------------------------------------*
FORM processing_single_mode USING    p_bfst STRUCTURE ztpp_bfst
                                     lp_budat.
  MOVE-CORRESPONDING p_bfst TO wa_bfst.
  CALL FUNCTION 'Z_FPP_BF_PRE_PARALLEL'
       EXPORTING
            lp_bfst               = wa_bfst
            lp_num                = ' '
            lp_budat              = lp_budat
       TABLES
            re_bfst               = rt_bfst
            re_bfst_r             = rt_bfst09
       EXCEPTIONS
            communication_failure = 1
            system_failure        = 2
            RESOURCE_FAILURE      = 3.

ENDFORM.                    " processing_single_mode
*&---------------------------------------------------------------------*
*&      Form  sd_trigger_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sd_trigger_event.
  CALL FUNCTION 'BP_EVENT_RAISE'
           EXPORTING
                eventid                = 'ZESD02_01'
*                eventparm              = 'SD_DEL01'
           EXCEPTIONS
                bad_eventid            = 1
                eventid_does_not_exist = 2
                eventid_missing        = 3
                raise_failed           = 4
                OTHERS                 = 99.

  CASE sy-subrc.
    WHEN 0.
      MESSAGE s001 WITH 'SD Call Success' 'ZESD02_01'.
    WHEN OTHERS.
      MESSAGE e001 WITH 'SD Call fail' 'ZESD02_01'.
  ENDCASE.
ENDFORM.                    " sd_trigger_event
*&---------------------------------------------------------------------*
*&      Form  initialization
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization.
  CLEAR: it_bfst[],it_bfst,snd_jobs,rcv_jobs,rt_bfst[],it_bfst18[],
         rt_bfst18[],flag.

ENDFORM.                    " initialization
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  DATA : r_field(17),f_field(17),num(2) TYPE n,che_num(2) TYPE n.
  CLEAR : num,che_num.
  FIELD-SYMBOLS : <rfield> TYPE ANY,<ffield> TYPE ANY.
*Get data
  PERFORM get_data_source.

  DESCRIBE TABLE it_bfst LINES w_int.
  IF w_int <> 0.
**-----start #1
    READ TABLE p_budat INDEX 1.
*-----end
    IF sy-uname  EQ  '101457'.
      LOOP AT it_bfst.
        PERFORM processing_single_mode USING it_bfst p_budat-low.
      ENDLOOP.
    ELSE.
***Seperate  RP18(sign off) Point  :03.28.2004
***when RP18(Sign off) is synchronous working, lock errors occured
**      LOOP AT it_bfst WHERE bfp18_flg = '01'.
**        num = 01.che_num = 0.
**        DO 17 TIMES.
**          CONCATENATE 'IT_BFST' '-' 'BFP' num '_FLG' INTO f_field.
**          ASSIGN (f_field) TO <ffield>.
**          IF <ffield> EQ '00' OR <ffield> EQ 'Y'.
**            MOVE-CORRESPONDING it_bfst TO it_bfst18.
**            che_num = che_num + 1.
**          ELSEIF <ffield> EQ '01' OR <ffield> EQ 'EB'
**                 OR <ffield> EQ 'ER' OR <ffield> EQ space.
**            EXIT.
**          ENDIF.
**          num = num + 1.
**        ENDDO.
**        IF che_num EQ '17'.
**          MOVE-CORRESPONDING it_bfst TO it_bfst18.
**          APPEND it_bfst18.
**        ENDIF.
**      ENDLOOP.
*Reversal is synchronous working, problems of lock  will occure
*To prevent from doing error, operating is separated reversal from
*normal case
      LOOP AT it_bfst .
        num = 01.
        DO 18 TIMES.
          CONCATENATE 'IT_BFST' '-' 'BFP' num '_FLG' INTO r_field.
          ASSIGN (r_field) TO <rfield>.
          IF <rfield> EQ '09'.
            MOVE-CORRESPONDING it_bfst TO it_bfst09.
            APPEND it_bfst09.
            EXIT.
          ELSE.
            num = num + 1.
          ENDIF.
        ENDDO.
      ENDLOOP.

      REFRESH : rt_bfst,tot_bfst.
      DO.
        READ TABLE it_bfst INDEX 1.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        MOVE-CORRESPONDING it_bfst TO wa_bfst.

        CALL FUNCTION 'Z_FPP_BF_PRE_PARALLEL'
        STARTING NEW TASK taskname DESTINATION IN GROUP 'PG_BF'
          PERFORMING return_01 ON END OF TASK
          EXPORTING
            lp_bfst                     = wa_bfst
            lp_num                      = ' '
            lp_budat                    = p_budat-low
          TABLES
            re_bfst                     = rt_bfst
            re_bfst_r                   = rt_bfst09
          EXCEPTIONS
            communication_failure       = 1
            system_failure              = 2
            RESOURCE_FAILURE            = 3 .

        CASE sy-subrc.
          WHEN 0.
            taskname = taskname + 1.
            snd_jobs = snd_jobs  + 1.
            DELETE it_bfst INDEX 1.
          WHEN 1 OR 2.
            excp_flag = 'X'.
            EXIT.
          WHEN 3.
*Receive reply to asynchronous RFC calls
            IF excp_flag = space.
              excp_flag = 'X'.
*First attempt for RESOURCE_Failure handling
              WAIT UNTIL rcv_jobs >= snd_jobs UP TO '0.01' SECONDS.
            ELSE.
*Second attempt for RESOURCE_Failure handling
              WAIT UNTIL rcv_jobs >= snd_jobs UP TO '0.1' SECONDS.
            ENDIF.
            IF sy-subrc = 0.
              CLEAR excp_flag. " Reset flag
            ENDIF.
        ENDCASE.
        CLEAR : wa_bfst,it_bfst.
      ENDDO.
      WAIT UNTIL rcv_jobs >= snd_jobs.
      IF sy-subrc = 0.
        COMMIT WORK AND WAIT.
*Issue Num : PP-20040714-001 Request by bhkim  : 2004.07.08
*Start : Reverse backflush locking Problem
        REFRESH : rt_bfst09,rt_bfst18.
        PERFORM reverse_process TABLES it_bfst09
                                USING p_budat-low.
*End
*Process RP18 Point
        REFRESH it_bfst18.
        PERFORM rp18_process TABLES it_bfst18 tot_bfst
                             USING p_budat-low.
        WRITE : / 'SEND NUM :', snd_jobs,'REV.NUM:',rcv_jobs,
                  'Total num:' , w_int .
        PERFORM write_results TABLES tot_bfst  rt_bfst09 rt_bfst18.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE i005. "WITH text-002.
    EXIT.
  ENDIF.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  running_program_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FLAG  text
*----------------------------------------------------------------------*
FORM running_program_check USING    p_flag.
*Job check
  CALL FUNCTION 'BP_FIND_JOBS_WITH_PROGRAM'
   EXPORTING
     abap_program_name                   = sy-cprog
*     ABAP_VARIANT_NAME                   = ' '
*     EXTERNAL_PROGRAM_NAME               = ' '
     dialog                              = 'N'
    TABLES
      joblist                             = it_joblist
   EXCEPTIONS
     no_jobs_found                       = 1
     program_specification_missing       = 2
     invalid_dialog_type                 = 3
     job_find_canceled                   = 4
     OTHERS                              = 5 .

  READ TABLE it_joblist WITH KEY jobname(3) = p_job
                                 status = 'A'.

  IF   sy-subrc = 0.
    MESSAGE s001 WITH 'The program is running!!!!'.
    p_flag = 'E'.
    EXIT.
  ENDIF.

  CHECK p_flag <> 'E'.
*Enqueue Lock object
  CALL FUNCTION 'ENQUEUE_EZ_BFST'
       EXPORTING
            mode_zspp_bfst_l = 'E'
            mandt            = sy-mandt
            status           = ' '
       EXCEPTIONS
            foreign_lock     = 1
            system_failure   = 2
            OTHERS           = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    p_flag = 'E'.
    EXIT.
  ENDIF.

ENDFORM.                    " running_program_check
*&---------------------------------------------------------------------*
*&      Form  dequeue_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FLAG  text
*----------------------------------------------------------------------*
FORM dequeue_object USING    p_flag.
*Dequeue Lock object
  CALL FUNCTION 'DEQUEUE_EZ_BFST'
       EXPORTING
            mode_zspp_bfst_l = 'E'
            mandt            = sy-mandt
            status           = ' '
       EXCEPTIONS
*           foreign_lock     = 1
            system_failure   = 2
            OTHERS           = 3.
  IF sy-subrc = 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    CLEAR p_flag .
  ENDIF.

ENDFORM.                    " dequeue_object
*&---------------------------------------------------------------------*
*&      Form  REVERSE_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BFST18  text
*----------------------------------------------------------------------*
FORM reverse_process TABLES   p_bfst09 STRUCTURE it_bfst
                     USING   lp_budat.
  DATA : w_int9 TYPE i.
  CLEAR : wa_bfst,rt_bfst09[],w_int9.
  DESCRIBE TABLE  p_bfst09 LINES w_int9.
  IF w_int9 <> 0.
    LOOP AT p_bfst09.
      MOVE-CORRESPONDING p_bfst09 TO wa_bfst.
      CALL FUNCTION 'Z_FPP_BF_PRE_PARALLEL'
*    STARTING NEW TASK taskname DESTINATION IN GROUP 'PG_BF'
*      PERFORMING return_01 ON END OF TASK
        EXPORTING
          lp_bfst                     = wa_bfst
          lp_num                      = rev
          lp_budat                    = lp_budat
        TABLES
           re_bfst                     = rt_bfst18
           re_bfst_r                   = rt_bfst09
        EXCEPTIONS
          communication_failure       = 1
          system_failure              = 2
          RESOURCE_FAILURE            = 3 .
*          OTHERS                      = 4.
    ENDLOOP.
    COMMIT WORK AND WAIT.
  ENDIF.
ENDFORM.                    " REVERSE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  write_paralle
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TOT_BFST  text
*----------------------------------------------------------------------*
FORM write_paralle TABLES   tot_bfst STRUCTURE ztpp_bfst.
  WRITE : / tot_bfst-plant,tot_bfst-model,tot_bfst-body_ser,
        tot_bfst-plan_ord,
        tot_bfst-bfp01_flg,tot_bfst-bfp02_flg,tot_bfst-bfp03_flg,
        tot_bfst-bfp04_flg,tot_bfst-bfp05_flg,tot_bfst-bfp06_flg,
        tot_bfst-bfp07_flg,tot_bfst-bfp08_flg,tot_bfst-bfp09_flg,
        tot_bfst-bfp10_flg,tot_bfst-bfp11_flg,tot_bfst-bfp12_flg,
        tot_bfst-bfp13_flg,tot_bfst-bfp14_flg,tot_bfst-bfp15_flg,
        tot_bfst-bfp16_flg,tot_bfst-bfp17_flg,tot_bfst-bfp18_flg.

ENDFORM.                    " write_paralle
*&---------------------------------------------------------------------*
*&      Form  write_reversal
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RT_BFST09  text
*----------------------------------------------------------------------*
FORM write_reversal TABLES  rt_bfst09 STRUCTURE ztpp_bfst.
  WRITE : / sy-tabix,
            rt_bfst09-plant,rt_bfst09-model,rt_bfst09-body_ser,
            rt_bfst09-plan_ord,rt_bfst09-bfp01_flg,
            rt_bfst09-bfp02_flg,rt_bfst09-bfp03_flg,
            rt_bfst09-bfp04_flg,rt_bfst09-bfp05_flg,
            rt_bfst09-bfp06_flg,rt_bfst09-bfp07_flg,
            rt_bfst09-bfp08_flg,rt_bfst09-bfp09_flg,
            rt_bfst09-bfp10_flg,rt_bfst09-bfp11_flg,
            rt_bfst09-bfp12_flg,rt_bfst09-bfp13_flg,
            rt_bfst09-bfp14_flg,rt_bfst09-bfp15_flg,
            rt_bfst09-bfp16_flg,rt_bfst09-bfp17_flg,
            rt_bfst09-bfp18_flg.

ENDFORM.                    " write_reversal
*&---------------------------------------------------------------------*
*&      Form  get_data_source
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_source.
  IF p_a EQ 'X'.
    SELECT * INTO TABLE it_bfst FROM ztpp_bfst
            WHERE plant EQ p_plant
              AND plan_ord IN s_plnum
              AND fin_bf_flg NE 'Y'
              AND bfp18_flg <> 'Y' ORDER BY model body_ser plan_ord.
  ELSE.
    SELECT * INTO TABLE it_bfst FROM ztpp_bfst
            WHERE plant EQ p_plant
              AND fin_bf_flg NE 'Y'
              AND bfp18_flg <> 'Y' ORDER BY model body_ser plan_ord.
  ENDIF.

ENDFORM.                    " get_data_source
*&---------------------------------------------------------------------*
*&      Form  get_data_signoff
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_signoff TABLES   p_bfst18 STRUCTURE it_bfst.

  IF p_a EQ 'X'.
    SELECT * INTO TABLE p_bfst18 FROM ztpp_bfst
            WHERE plant EQ p_plant
              AND plan_ord IN s_plnum
              AND plan_del_flg NE 'X'
              AND fin_bf_flg NE 'Y'
              AND bfp18_flg <> 'Y' ORDER BY model body_ser plan_ord.
  ELSE.
    SELECT * INTO TABLE p_bfst18 FROM ztpp_bfst
            WHERE plant EQ p_plant
              AND plan_del_flg NE 'X'
              AND fin_bf_flg NE 'Y'
              AND bfp18_flg <> 'Y' ORDER BY model body_ser plan_ord.
  ENDIF.
ENDFORM.                    " get_data_signoff

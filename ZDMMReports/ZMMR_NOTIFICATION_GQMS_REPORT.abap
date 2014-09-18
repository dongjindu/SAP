************************************************************************
* Program Name      : ZMMR_NOTIFICATION_GQMS_REPORT
* Author            : Furong Wang
* Creation Date     : 07/2014
* Specifications By :
* Pattern           :
* Development Request
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT zmmr_notification_gqms_report MESSAGE-ID zmmm.

TABLES : qmel.

TYPE-POOLS: slis .

CONSTANTS: c_error VALUE 'E',
           c_succ VALUE 'S'.

DATA: it_log LIKE TABLE OF ztmm_qmnoti_gqms WITH HEADER LINE.
DATA: it_send LIKE TABLE OF zsmm_qmnoti_gqms WITH HEADER LINE.

DATA: BEGIN OF it_disp OCCURS 0.
        INCLUDE STRUCTURE zsmm_qmnoti_gqms.
DATA: loca_desc LIKE qpgt-kurztext,
      END OF it_disp.

DATA: BEGIN OF it_data OCCURS 0.
        INCLUDE STRUCTURE ztmm_qmnoti_gqms.
DATA: objnr LIKE jest-objnr,
      qmdat LIKE qmel-qmdat,   "05.29.2014 Victor
      qmart LIKE qmel-qmart,
      code_vh LIKE qmel-code_vh,
*      qcodegrp_loc LIKE qmel-qcodegrp_loc,
      END OF it_data.

*DATA: it_email LIKE TABLE OF ztpp_rep_veh WITH HEADER LINE.
DATA: w_dest(30),
      ok_code LIKE sy-ucomm,
      w_repid LIKE sy-repid,
      w_cnt TYPE i.

DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE,
       it_fieldcat_det TYPE lvc_t_fcat WITH HEADER LINE. "/Detail

DATA : wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
       w_fieldname    LIKE LINE OF it_fieldcat.

DATA: wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
      wa_variant TYPE disvariant.      "for parameter IS_VARIANT

DATA: wa_custom_control TYPE        scrfname VALUE 'ALV_CONTAINER',
      alv_grid          TYPE REF TO cl_gui_alv_grid,
      g_docking_container    TYPE REF TO cl_gui_docking_container.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS: s_date FOR sy-datum NO-EXTENSION OBLIGATORY,
                s_qmnum FOR qmel-qmnum,
                s_lifnum FOR qmel-lifnum,
                s_matnr FOR qmel-matnr,
                s_mawerk FOR qmel-mawerk,
                s_matkl FOR qmel-matkl.
*PARAMETERS:     p_days(3).
*SELECT-OPTIONS : s_lifnum FOR qmel-lifnum.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------
INITIALIZATION.
  PERFORM init_data.

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------

  PERFORM get_data.

  PERFORM display_data.


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
  s_date-low = sy-datum - 10.
  s_date-high = sy-datum.
  APPEND s_date.
*  p_days = 90.
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
    MESSAGE e009 WITH 'The Program is running by other'.
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
  DATA:
*        l_matkl1 LIKE mara-matkl,
*        l_matkl2 LIKE mara-matkl,
        l_fegrp LIKE qmfe-fegrp,
        l_fecod LIKE qmfe-fecod.

  DATA: lt_jest LIKE TABLE OF jest WITH HEADER LINE,
        lt_eng LIKE TABLE OF ztmm_engmod_gqms WITH HEADER LINE.

*  l_matkl1 =  'UD*'.
*  l_matkl2 =  'YF*'.

*-< new
*-Q3
  SELECT a~qmnum a~lifnum a~matnr a~mawerk a~matkl a~qmgrp a~erdat
         a~rkmng b~urtxt a~objnr a~qmdat a~qmart  a~code_vh
             qcodegrp_loc
    INTO CORRESPONDING FIELDS OF TABLE it_data
      FROM qmel AS a
       INNER JOIN qmur AS b
    ON a~qmnum = b~qmnum
       INNER JOIN qmfe AS f
      ON b~qmnum = f~qmnum
     AND b~fenum = f~fenum
      INNER JOIN qmih AS g
     ON a~qmnum = g~qmnum
      WHERE a~qmart = 'Q3'
** furong on 07/07/14
       AND a~qmnum IN s_qmnum
       AND lifnum IN s_lifnum
       AND a~matnr IN s_matnr
       AND mawerk IN s_mawerk
       AND matkl IN s_matkl
** End on 07/07/14
*        AND a~erdat IN s_date
        AND g~ausvn IN s_date
*        AND ( a~erdat IN s_date OR g~ausvn IN s_date )
*        AND a~qmkat = 'X'
        AND a~qmgrp <> 'MXGXS1'
*        AND a~lifnum IN s_lifnum    "06.26.2014
        AND f~fekat = 'W'
        AND f~fegrp = '0'
        AND f~fecod >= '0001'
        AND f~fecod <= '0014'
        AND NOT EXISTS ( SELECT *   FROM jest AS c   "Delete & E008
                         WHERE c~objnr = a~objnr
                           AND c~inact = ' '
                           AND ( c~stat  =  'I0076'
                                   OR c~stat =  'E0008' )
                        )
       AND EXISTS ( SELECT * FROM jest AS d          "only completed
                    WHERE d~objnr = a~objnr
                      AND d~inact = ' '
                      AND d~stat  =  'I0072'
                  )
       AND EXISTS ( SELECT * FROM jest AS e        "only Vendor Issue
                    WHERE e~objnr = a~objnr
                      AND e~inact = ' '
                      AND e~stat  =  'E0004'
                  ).

  SELECT a~qmnum a~lifnum a~matnr a~mawerk a~matkl a~qmgrp a~erdat
         a~rkmng b~urtxt a~objnr a~qmdat a~qmart  a~code_vh
             qcodegrp_loc
    APPENDING CORRESPONDING FIELDS OF TABLE it_data
      FROM qmel AS a
       INNER JOIN qmur AS b
    ON a~qmnum = b~qmnum
       INNER JOIN qmfe AS f
      ON b~qmnum = f~qmnum
     AND b~fenum = f~fenum
      INNER JOIN qmih AS g
     ON a~qmnum = g~qmnum
      WHERE a~qmart = 'Q3'
        AND a~erdat IN s_date
** furong on 07/07/14
       AND a~qmnum IN s_qmnum
       AND lifnum IN s_lifnum
       AND a~matnr IN s_matnr
       AND mawerk IN s_mawerk
       AND matkl IN s_matkl
** End on 07/07/14
        AND g~ausvn = '00000000'
*        AND ( a~erdat IN s_date OR g~ausvn IN s_date )
*        AND a~qmkat = 'X'
        AND a~qmgrp <> 'MXGXS1'
*        AND a~lifnum IN s_lifnum    "06.26.2014
        AND f~fekat = 'W'
        AND f~fegrp = '0'
        AND f~fecod >= '0001'
        AND f~fecod <= '0014'
        AND NOT EXISTS ( SELECT *   FROM jest AS c   "Delete & E008
                         WHERE c~objnr = a~objnr
                           AND c~inact = ' '
                           AND ( c~stat  =  'I0076'
                                   OR c~stat =  'E0008' )
                        )
       AND EXISTS ( SELECT * FROM jest AS d          "only completed
                    WHERE d~objnr = a~objnr
                      AND d~inact = ' '
                      AND d~stat  =  'I0072'
                  )
       AND EXISTS ( SELECT * FROM jest AS e        "only Vendor Issue
                    WHERE e~objnr = a~objnr
                      AND e~inact = ' '
                      AND e~stat  =  'E0004'
                  ).

*-Q4
  SELECT a~qmnum a~lifnum a~matnr a~mawerk a~matkl a~qmgrp a~erdat
         a~rkmng b~urtxt a~objnr a~qmdat  a~qmart a~code_vh
             qcodegrp_loc
    APPENDING CORRESPONDING FIELDS OF TABLE it_data
      FROM qmel AS a
       INNER JOIN qmur AS b
    ON a~qmnum = b~qmnum
       INNER JOIN qmfe AS f
      ON b~qmnum = f~qmnum
     AND b~fenum = f~fenum
      INNER JOIN qmih AS g
     ON a~qmnum = g~qmnum
      WHERE a~qmart = 'Q4'
*        AND a~erdat IN s_date
        AND g~ausvn IN s_date
** furong on 07/07/14
       AND a~qmnum IN s_qmnum
       AND lifnum IN s_lifnum
       AND a~matnr IN s_matnr
       AND mawerk IN s_mawerk
       AND matkl IN s_matkl
** End on 07/07/14
*        AND ( a~erdat IN s_date OR g~ausvn IN s_date )
*        AND a~qmkat = 'X'
        AND a~qmgrp <> 'MXGXS1'
*        AND a~lifnum IN s_lifnum    "06.26.2014
        AND f~fekat = 'W'
        AND f~fegrp = '0'
        AND f~fecod >= '0001'
        AND f~fecod <= '0014'
** Furong on 07/31/14 (
     AND EXISTS ( SELECT * FROM jest AS d          "only completed
                    WHERE d~objnr = a~objnr
                      AND d~inact = ' '
                      AND d~stat  =  'I0072'
                  )
** )
        AND NOT EXISTS ( SELECT *   FROM jest AS c
                         WHERE c~objnr = a~objnr
                           AND c~inact = ' '
                           AND ( c~stat  =  'I0076'
                                   OR c~stat =  'E0001' )
                        ).

  SELECT a~qmnum a~lifnum a~matnr a~mawerk a~matkl a~qmgrp a~erdat
         a~rkmng b~urtxt a~objnr a~qmdat  a~qmart a~code_vh
         qcodegrp_loc
    APPENDING CORRESPONDING FIELDS OF TABLE it_data
      FROM qmel AS a
       INNER JOIN qmur AS b
    ON a~qmnum = b~qmnum
       INNER JOIN qmfe AS f
      ON b~qmnum = f~qmnum
     AND b~fenum = f~fenum
      INNER JOIN qmih AS g
     ON a~qmnum = g~qmnum
      WHERE a~qmart = 'Q4'
        AND a~erdat IN s_date
** furong on 07/07/14
       AND a~qmnum IN s_qmnum
       AND lifnum IN s_lifnum
       AND a~matnr IN s_matnr
       AND mawerk IN s_mawerk
       AND matkl IN s_matkl
** End on 07/07/14
        AND g~ausvn = '00000000'
*        AND ( a~erdat IN s_date OR g~ausvn IN s_date )
*        AND a~qmkat = 'X'
        AND a~qmgrp <> 'MXGXS1'
*        AND a~lifnum IN s_lifnum    "06.26.2014
        AND f~fekat = 'W'
        AND f~fegrp = '0'
        AND f~fecod >= '0001'
        AND f~fecod <= '0014'
** Furong on 07/31/14 (
     AND EXISTS ( SELECT * FROM jest AS d          "only completed
                    WHERE d~objnr = a~objnr
                      AND d~inact = ' '
                      AND d~stat  =  'I0072'
                  )
** )
        AND NOT EXISTS ( SELECT *   FROM jest AS c
                         WHERE c~objnr = a~objnr
                           AND c~inact = ' '
                           AND ( c~stat  =  'I0076'
                                   OR c~stat =  'E0001' )
                        ).
*->
*  SELECT a~qmnum a~lifnum a~matnr a~mawerk a~matkl a~qmgrp a~erdat
*         a~rkmng b~urtxt a~objnr a~qmdat
*    INTO CORRESPONDING FIELDS OF TABLE it_data
*      FROM qmel AS a
*       INNER JOIN qmur AS b
*    ON a~qmnum = b~qmnum
*        INNER JOIN jest AS c
*      ON a~objnr = c~objnr
**       INNER JOIN jest AS d
**      ON c~objnr = d~objnr
**       INNER JOIN jest AS e
**      ON d~objnr = e~objnr
*       INNER JOIN qmfe AS f
*      ON b~qmnum = f~qmnum
*     AND b~fenum = f~fenum
*      WHERE ( ( a~qmart = 'Q3' AND c~stat <>  'E0008' )
*                OR ( a~qmart = 'Q4' AND c~stat <>  'E0001' ) )
*        AND a~erdat IN s_date
*        AND a~qmkat = 'X'
*        AND a~qmgrp <> 'MXGXS1'
**        AND c~stat =  'E0004'
*        AND c~inact = ' '
**        AND d~stat  =  'I0160'
*        AND c~stat  <>  'I0160'
**        AND d~inact = ' '
**        AND e~stat =  'I0072'
**        AND e~inact = ' '.
*        AND f~fekat = 'W'
*        AND f~fegrp = '0'
*        AND f~fecod >= '0001'
*        AND f~fecod <= '0013'.
**        AND lifnum <> 'SEF9'. "Temp

*  SELECT a~qmnum a~lifnum a~matnr a~mawerk a~matkl a~qmgrp a~erdat
*         a~rkmng b~urtxt a~objnr a~qmdat
*    INTO CORRESPONDING FIELDS OF TABLE it_data
*      FROM qmel AS a
*       INNER JOIN qmur AS b
*    ON a~qmnum = b~qmnum
*        INNER JOIN jest AS c
*      ON a~objnr = c~objnr
*       INNER JOIN jest AS d
*      ON c~objnr = d~objnr
*       INNER JOIN jest AS e
*      ON d~objnr = e~objnr
**       INNER JOIN qmfe AS f
**      ON b~qmnum = f~qmnum
**     AND b~fenum = f~fenum
*      WHERE ( ( a~qmart = 'Q3' AND c~stat <>  'E0008' )
*                OR ( a~qmart = 'Q4' AND c~stat <>  'E0001' ) )
*        AND a~erdat IN s_date
*        AND a~qmkat = 'X'
*        AND a~qmgrp <> 'MXGXS1'
**        AND c~stat =  'E0004'
*        AND c~inact = ' '
**        AND d~stat  =  'I0160'
*        AND d~stat  <>  'I0160'
*        AND d~inact = ' '
*        AND e~stat =  'I0072'
*        AND e~inact = ' '.
**        AND f~fekat = 'W'
**        AND f~fegrp = '0'
**        AND f~fecod >= '0001'
**        AND f~fecod <= '0013'.
**        AND lifnum <> 'SEF9'. "Temp

  IF it_data[] IS INITIAL.
    PERFORM dequeue_prg.
    MESSAGE e009 WITH 'No data found'.
  ELSE.
*    SELECT * INTO TABLE lt_jest
*      FROM jest
*      FOR ALL ENTRIES IN it_data
*      WHERE objnr = it_data-objnr
*        AND stat =  'E0008'    " PPM
*        AND inact = ' '.
*    IF sy-subrc = 0.
*      SORT lt_jest BY objnr.
*    ENDIF.

*-  delete duplicate one
    SORT it_data BY qmnum lifnum matnr mawerk.
    DELETE ADJACENT DUPLICATES FROM it_data
                  COMPARING qmnum lifnum matnr mawerk.

    SELECT * INTO TABLE lt_eng
      FROM ztmm_engmod_gqms.
  ENDIF.

  LOOP AT it_data.
    "commented on 05.29.2014
*    READ TABLE lt_jest WITH KEY objnr = it_data-objnr.
*    IF sy-subrc = 0.
*      CONTINUE.
*    ENDIF.

    IF it_data-matkl = 'INIT'.
      it_data-matkl = ''.
    ENDIF.

    IF it_data-matkl IS INITIAL AND it_data-qmart = 'Q4'. "06.04.2014
      it_data-matkl = it_data-code_vh.
    ENDIF.

    CASE it_data-matkl+0(2).
      WHEN 'UD'.
        it_data-matkl = 'MDA'.
      WHEN 'YF'.
        it_data-matkl = 'YFA'.
** oN 11/08/13 by Furong
      WHEN 'LF'.
        it_data-matkl = 'LFA'.
** End
      WHEN OTHERS.
        IF it_data-matkl+0(1) = '1' OR
           it_data-matkl+0(1) = '2' OR
          it_data-matkl+0(1) = 'E'.
          READ TABLE lt_eng WITH KEY matkl = it_data-matkl.
          it_data-matkl = lt_eng-engine_model.
        ELSE.
*          CONTINUE.      "comment by Victor 06.04.2014
        ENDIF.
    ENDCASE.

    it_data-qmgrp = it_data-qmgrp+2(1).

    IF it_data-lifnum IS INITIAL.
*-    added on 05.29.2014 Victor
      SELECT SINGLE lifnr INTO it_data-lifnum
      FROM ztcou137
      WHERE bukrs = 'H201'
        AND matnr = it_data-matnr
        AND zdtfr <= it_data-qmdat
        AND zdtto >= it_data-qmdat.
    ELSE.
*      CONTINUE.            "commented for TEMP.......

*      SELECT SINGLE lifnr INTO it_data-lifnum
*        FROM eord
*        WHERE matnr = it_data-matnr
*         AND werks = it_data-mawerk
*          AND autet = '2'.
*      IF sy-subrc = 0 AND it_data-lifnum  = 'SEF9'.
*        CONTINUE.
*      ENDIF.
    ENDIF.
*    CLEAR: l_fegrp, l_fecod.
*    SELECT SINGLE fegrp fecod INTO (l_fegrp, l_fecod)
*       FROM qmfe
*      WHERE qmnum = it_data-qmnum.
*    IF l_fegrp = 0 AND l_fecod = 14.
*      CONTINUE.
*    ENDIF.

    CHECK it_data-lifnum <> 'SEF9'.
    CHECK it_data-lifnum <> 'SSTX'.
    CHECK it_data-lifnum <> 'SBC3'.

    it_log = it_data.
    APPEND it_log.

  ENDLOOP.

  PERFORM check_duplication.

  LOOP AT it_log.

    MOVE-CORRESPONDING it_log TO it_send.
    APPEND it_send.
    CLEAR: it_send.

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
FORM call_rfc.
*  DATA:   l_result,
*          l_msg LIKE bapireturn-message,
*          l_date LIKE sy-datum,
*          l_time LIKE sy-uzeit.
*
*  w_dest = 'WMHR01'.
*  CALL FUNCTION 'ZFMM_QMNOTI_GQMS'
*    DESTINATION w_dest
*    IMPORTING
*      e_result              = l_result
*      e_msg                 = l_msg
*    TABLES
*      t_data                = it_send
*    EXCEPTIONS
*      communication_failure = 1
*      system_failure        = 2.
*
*  IF sy-subrc = 0.
*    IF l_result IS INITIAL.
*      l_result = 'E'.
*      l_msg = 'Retunn flag is empty'.
*      MESSAGE s999 WITH 'I/F result : Fail'.
*    ELSE.
*      MESSAGE s999 WITH 'I/F result : Sucess'.
*    ENDIF.
*
*  ELSE.
*    l_result = 'E'.
*    MESSAGE s999 WITH 'I/F result : Fail'.
*  ENDIF.
*
*  l_time = sy-uzeit.
*  LOOP AT it_log.
*    it_log-run_date = sy-datum.
*    it_log-run_time = l_time.
*    it_log-msg = l_msg.
*    it_log-zresult = l_result.
*    it_log-run_user = sy-uname.
*    MODIFY it_log TRANSPORTING run_date run_time zresult msg run_user.
*  ENDLOOP.
*
*  l_date = sy-datum - p_days.
*  DELETE FROM ztmm_qmnoti_gqms WHERE run_date < l_date.
*
*  INSERT ztmm_qmnoti_gqms FROM TABLE it_log
*        ACCEPTING DUPLICATE KEYS.
**  IF sy-subrc <> 0.
**    MESSAGE e009 WITH 'LOG table update error'.
*  COMMIT WORK AND WAIT.
**  ENDIF.

ENDFORM.                    " CALL_RFC
*
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
*&---------------------------------------------------------------------*
*&      Form  CHECK_REPLICATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_duplication.
  DATA: lt_dupl LIKE TABLE OF ztmm_qmnoti_gqms WITH HEADER LINE.

  LOOP AT it_log.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = it_log-qmnum
      IMPORTING
        output = it_log-qmnum.
    MODIFY it_log TRANSPORTING qmnum.
  ENDLOOP.

*  SELECT * INTO TABLE lt_dupl
*     FROM ztmm_qmnoti_gqms
*     FOR ALL ENTRIES IN it_log
*     WHERE qmnum = it_log-qmnum
*       AND matnr = it_log-matnr
*       AND zresult = 'S'.
*  SORT lt_dupl BY qmnum matnr.
*
*  IF sy-subrc = 0.
*    LOOP AT it_log.
*      READ TABLE lt_dupl WITH KEY qmnum = it_log-qmnum
*                                  matnr = it_log-matnr
*                                  BINARY SEARCH.
*      IF sy-subrc = 0.
*        DELETE it_log.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.

ENDFORM.                    " CHECK_REPLICATE
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_container_n_object.
  DATA:   l_repid LIKE sy-repid,
          l_dynnr LIKE sy-dynnr.

  l_repid = sy-repid.
  l_dynnr = sy-dynnr.
  CREATE OBJECT g_docking_container
    EXPORTING
      repid     = l_repid
      dynnr     = l_dynnr
      side      = cl_gui_docking_container=>dock_at_bottom
*     RATIO     = 90
      extension = 2000.

  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = l_repid
        txt2  = sy-subrc
        txt1  = 'The control can not be created'.
  ENDIF.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  CREATE OBJECT alv_grid
*         EXPORTING i_parent = grid_container
         EXPORTING i_parent = g_docking_container
                   i_appl_events = 'X'.

ENDFORM.                    " create_container_n_object
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_grid.
  DATA : lw_s_dragdrop TYPE lvc_s_dd01. "/ Drag&Drop control settings

  CLEAR : wa_is_layout, wa_variant.

*//-- Set Layout Structure
  wa_is_layout-edit       = ' '.      "/Edit Mode Enable
  wa_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  wa_is_layout-language   = sy-langu. "/Language Key
  wa_is_layout-cwidth_opt = 'X'.   "/optimizes the column width
*  wa_is_layout-info_fname = 'IF'.
*  wa_is_layout-ctab_fname = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

*//-- Set Variant Structure
  wa_variant-report       = sy-repid.
  wa_variant-username     = sy-uname.

ENDFORM.                    " set_attributes_alv_grid
*&---------------------------------------------------------------------*
*&      Form  build_sortcat_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sortcat_display.

*  it_sort-spos           = 1.
*  it_sort-fieldname      = 'MATNR'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.



ENDFORM.                    " build_sortcat_display
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0027   text
*----------------------------------------------------------------------*
FORM build_field_catalog USING p_itab.

  DATA: lw_itab TYPE slis_tabname,
        lw_waers LIKE t001-waers,
        l_rqty(9),
        l_datum(8),
        l_cn(2) TYPE n.

  CLEAR: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].
  CLEAR: w_repid.

  lw_itab = p_itab.

  w_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = w_repid
*      i_structure_name   = 'ZSMM_QMNOTI_GQMS'
     i_internal_tabname = lw_itab
     i_inclname         = w_repid
    CHANGING
      ct_fieldcat        = it_fieldname.

  PERFORM setting_fieldcat TABLES it_fieldcat USING :


                                'S' 'QMNUM'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Notification No',
                                  'E' 'OUTPUTLEN'   '14',

                                   'S' 'LIFNUM'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Vendor',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'MAWERK'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Plant',
                                  'E' 'OUTPUTLEN'   '6',

                                  'S' 'MATNR'         ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Material',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'MATKL'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Material Group',
                                  'E' 'OUTPUTLEN'   '40',

                                  'S' 'ERDAT'         ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Cr Date',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'RKMNG'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Complaint Quantity'
                                  ,
                                  ' ' 'DECIMALS_O'  '0',
                                  ' ' 'NO_ZERO'     'X',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'URTXT'         ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Leave name',
                                  'E' 'OUTPUTLEN'   '20',

                                  'S' 'QCODEGRP_LOC' ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Location',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'LOCA_DESC'   ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Location Desc.',
                                  'E' 'OUTPUTLEN'   '30'.
  .


ENDFORM.                    " build_field_catalog

*---------------------------------------------------------------------*
*       FORM setting_fieldcat                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_FIELDCAT                                                    *
*  -->  P_GUBUN                                                       *
*  -->  P_FIELD                                                       *
*  -->  P_VALUE                                                       *
*---------------------------------------------------------------------*
FORM setting_fieldcat TABLES   p_fieldcat STRUCTURE it_fieldcat
                      USING    p_gubun
                               p_field
                               p_value.
  DATA : l_col(40).

  FIELD-SYMBOLS <fs>.

* START - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'S'.
    CLEAR: p_fieldcat.

    READ TABLE it_fieldname INTO w_fieldname
                            WITH KEY fieldname  = p_field.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH 'Check field catalog'.
    ENDIF.

    MOVE: w_fieldname-fieldname TO p_fieldcat-fieldname.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' p_field  INTO l_col.
  ASSIGN (l_col) TO <fs>.
  MOVE   p_value TO <fs>.

* END - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'E'.
    ADD 1 TO w_cnt.
    p_fieldcat-col_pos = w_cnt.
    APPEND p_fieldcat.
  ENDIF.
ENDFORM.                    " setting_fieldcat

*---------------------------------------------------------------------*
*       FORM assign_itab_to_alv                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM assign_itab_to_alv.

  CALL METHOD alv_grid->set_table_for_first_display
    EXPORTING
      is_layout            = wa_is_layout
      i_save               = wa_save
      is_variant           = wa_variant
*     i_default            = space
*     it_toolbar_excluding = it_toolbar_excluding[]
    CHANGING
      it_fieldcatalog      = it_fieldcat[]
      it_outtab            = it_disp[].
*               it_sort          = it_sort[].

ENDFORM.                    " assign_itab_to_alv
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE ok_code.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'ST200'.
  SET TITLEBAR 'T200'.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv_200 OUTPUT.

  IF g_docking_container IS INITIAL.

    PERFORM create_container_n_object.
    PERFORM set_attributes_alv_grid.
    PERFORM build_sortcat_display.
    PERFORM build_field_catalog USING 'IT_DISP'.
    PERFORM assign_itab_to_alv.
*    PERFORM sssign_event_9000.  ELSE.
    CALL METHOD alv_grid->refresh_table_display.
  ENDIF.

ENDMODULE.                 " DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data .
  PERFORM get_display.
  CALL SCREEN 0200.
ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_display .
  LOOP AT it_send.
    MOVE-CORRESPONDING it_send TO it_disp.
    SELECT SINGLE kurztext INTO it_disp-loca_desc
      FROM qpgt
     WHERE katalogart = '#'
       AND codegruppe = it_send-qcodegrp_loc.
    APPEND it_disp.
    CLEAR: it_disp.
  ENDLOOP.
ENDFORM.                    " GET_DISPLAY

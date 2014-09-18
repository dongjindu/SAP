************************************************************************
* Program Name      : ZMMR_NOTIFICATION_GQMS
* Author            : Furong Wang
* Creation Date     : 03/2013
* Specifications By :
* Pattern           :
* Development Request
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT zmmr_notification_gqms MESSAGE-ID zmmm.

TABLES : qmel.

TYPE-POOLS: slis .

CONSTANTS: c_error VALUE 'E',
           c_succ VALUE 'S'.

DATA: it_log LIKE TABLE OF ztmm_qmnoti_gqms WITH HEADER LINE.
DATA: it_send LIKE TABLE OF zsmm_qmnoti_gqms WITH HEADER LINE.

DATA: BEGIN OF it_data OCCURS 0.
        INCLUDE STRUCTURE ztmm_qmnoti_gqms.
DATA: objnr LIKE jest-objnr,
      qmdat LIKE qmel-qmdat,   "05.29.2014 Victor
      qmart LIKE qmel-qmart,
      code_vh LIKE qmel-code_vh,
*      QCODEGRP_LOC LIKE QMEL-QCODEGRP_LOC,
      END OF it_data.

DATA: it_email LIKE TABLE OF ztpp_rep_veh WITH HEADER LINE.
DATA: w_dest(30).

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS: s_date FOR sy-datum NO-EXTENSION OBLIGATORY.
PARAMETERS:     p_days(3).
*SELECT-OPTIONS : s_lifnum FOR qmel-lifnum.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------
INITIALIZATION.
  PERFORM init_data.

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------

  PERFORM locking_rtn USING sy-repid.

  PERFORM get_data.

  PERFORM call_rfc.

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
  s_date-low = sy-datum - 10.
  s_date-high = sy-datum.
  APPEND s_date.
  p_days = 720.
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
         QCODEGRP_LOC
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
         QCODEGRP_LOC
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
         QCODEGRP_LOC
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
         QCODEGRP_LOC
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
  DATA:   l_result,
          l_msg LIKE bapireturn-message,
          l_date LIKE sy-datum,
          l_time LIKE sy-uzeit.

  w_dest = 'WMHR01'.
  CALL FUNCTION 'ZFMM_QMNOTI_GQMS'
    DESTINATION w_dest
    IMPORTING
      e_result              = l_result
      e_msg                 = l_msg
    TABLES
      t_data                = it_send
    EXCEPTIONS
      communication_failure = 1
      system_failure        = 2.

  IF sy-subrc = 0.
    IF l_result IS INITIAL.
      l_result = 'E'.
      l_msg = 'Retunn flag is empty'.
      MESSAGE s999 WITH 'I/F result : Fail'.
    ELSE.
      MESSAGE s999 WITH 'I/F result : Sucess'.
    ENDIF.

  ELSE.
    l_result = 'E'.
    MESSAGE s999 WITH 'I/F result : Fail'.
  ENDIF.

  l_time = sy-uzeit.
  LOOP AT it_log.
    it_log-run_date = sy-datum.
    it_log-run_time = l_time.
    it_log-msg = l_msg.
    it_log-zresult = l_result.
    it_log-run_user = sy-uname.
    MODIFY it_log TRANSPORTING run_date run_time zresult msg run_user.
  ENDLOOP.

  l_date = sy-datum - p_days.
  DELETE FROM ztmm_qmnoti_gqms WHERE run_date < l_date.

  INSERT ztmm_qmnoti_gqms FROM TABLE it_log
        ACCEPTING DUPLICATE KEYS.
*  IF sy-subrc <> 0.
*    MESSAGE e009 WITH 'LOG table update error'.
  COMMIT WORK AND WAIT.
*  ENDIF.

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

  SELECT * INTO TABLE lt_dupl
     FROM ztmm_qmnoti_gqms
     FOR ALL ENTRIES IN it_log
     WHERE qmnum = it_log-qmnum
       AND matnr = it_log-matnr
       AND zresult = 'S'.
  SORT lt_dupl BY qmnum matnr.

  IF sy-subrc = 0.
    LOOP AT it_log.
      READ TABLE lt_dupl WITH KEY qmnum = it_log-qmnum
                                  matnr = it_log-matnr
                                  BINARY SEARCH.
      IF sy-subrc = 0.
        DELETE it_log.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " CHECK_REPLICATE

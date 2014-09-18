************************************************************************
* Program Name      : ZPPR_DEALER_ALLOCATION_UPD
* Author            : Furong Wang
* Creation Date     : 03/2013
* Specifications By : BS Bae
* Pattern           :
* Development Request
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT zppr_dealer_allocation_upd MESSAGE-ID zmmm.

DATA: BEGIN OF it_um OCCURS 0,
      model_code LIKE ztsd_um-model_code,
      body_no LIKE ztsd_um-body_no,
      wo_nation LIKE ztsd_um-wo_nation,
      wo_dealer1 LIKE ztsd_um-wo_dealer1,
      dealer_dt LIKE ztsd_um-dealer_dt,
      objek LIKE ausp-objek,
      END OF it_um.

DATA: BEGIN OF it_vmas OCCURS 0,
      objek         LIKE   ausp-objek,
      rp_status     LIKE   ausp-atwrt,
      dealer_no     LIKE   ausp-atwrt,
      allo_dt       LIKE   ausp-atflv,    "Allocation Date
      rp23_vpc      LIKE   ausp-atflv,
      rp25_sout     LIKE   ausp-atflv,
      rp27_sout     LIKE   ausp-atflv,
      END   OF it_vmas.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS: s_date FOR sy-datum NO-EXTENSION OBLIGATORY.

SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------
INITIALIZATION.
  PERFORM init_data.

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------

  PERFORM locking_rtn USING sy-repid.

  PERFORM get_data.

  PERFORM process_data.

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
  s_date-low = sy-datum - 2.
  s_date-high = sy-datum.
  APPEND s_date.
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

  DATA: BEGIN OF lt_temp OCCURS 0,
        model_code LIKE ztsd_um-model_code,
        body_no LIKE ztsd_um-body_no,
        wo_nation LIKE ztsd_um-wo_nation,
        wo_dealer1 LIKE ztsd_um-wo_dealer1,
        dealer_dt LIKE ztsd_um-dealer_dt,
        ship_out LIKE ausp-atflv,
        vpc_out LIKE ausp-atflv,
        objek LIKE ausp-objek,
        END OF lt_temp.

  DATA: lt_vmaster LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
        lt_vmas_r LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
        l_objek(18),
        l_flag(1).

  DATA: l_rp_status      LIKE cabn-atinn,
        l_dealer         LIKE cabn-atinn,
        l_rp21_allo_date LIKE cabn-atinn,
        l_rp23_vpc_date  LIKE cabn-atinn,
        l_rp25_shipout   LIKE cabn-atinn,
        l_rp27_shipout   LIKE cabn-atinn.

  SELECT SINGLE atinn INTO l_rp_status
  FROM cabn WHERE atnam = 'P_RP_STATUS'.

  SELECT SINGLE atinn INTO l_dealer
    FROM cabn WHERE atnam = 'P_DEALER_NO'.

  SELECT SINGLE atinn INTO l_rp21_allo_date
    FROM cabn WHERE atnam = 'P_RP21_SHOP_DATE'.

  SELECT SINGLE atinn INTO l_rp23_vpc_date
    FROM cabn WHERE atnam = 'P_RP23_SHOP_DATE'.

  SELECT SINGLE atinn INTO l_rp25_shipout
    FROM cabn WHERE atnam = 'P_RP25_SHOP_DATE'.

  SELECT SINGLE atinn INTO l_rp27_shipout
    FROM cabn WHERE atnam = 'P_RP27_SHOP_DATE'.


  SELECT model_code body_no wo_nation wo_dealer1
         dealer_dt INTO TABLE it_um
    FROM ztsd_um
   WHERE ( status = ''  OR
         ( status = 'F' AND ship_out IN s_date ) )
     AND body_no <> ' '.
  IF sy-subrc = 0.
    LOOP AT it_um.
      CONCATENATE it_um-model_code it_um-body_no INTO it_um-objek.
      MODIFY it_um TRANSPORTING objek.
    ENDLOOP.

    SELECT a~objek a~atwrt AS rp_status
           b~atwrt AS dealer_no
           c~atflv AS allo_dt
           d~atflv AS rp23_vpc
           e~atflv AS rp25_sout
           f~atflv AS rp27_sout
      INTO CORRESPONDING FIELDS OF TABLE it_vmas
      FROM ausp AS a LEFT OUTER JOIN ausp AS b
                          ON  b~objek = a~objek
                          AND b~atinn = l_dealer
                          AND b~mafid = a~mafid
                          AND b~klart = a~klart
                     LEFT OUTER JOIN ausp AS c
                          ON  c~objek = a~objek
                          AND c~atinn = l_rp21_allo_date
                          AND c~mafid = a~mafid
                          AND c~klart = a~klart
                     LEFT OUTER JOIN ausp AS d
                          ON  d~objek = a~objek
                          AND d~atinn = l_rp23_vpc_date
                          AND d~mafid = a~mafid
                          AND d~klart = a~klart
                     LEFT OUTER JOIN ausp AS e
                          ON  e~objek = a~objek
                          AND e~atinn = l_rp25_shipout
                          AND e~mafid = a~mafid
                          AND e~klart = a~klart
                     LEFT OUTER JOIN ausp AS f
                          ON  f~objek = a~objek
                          AND f~atinn = l_rp27_shipout
                          AND f~mafid = a~mafid
                          AND f~klart = a~klart
               FOR ALL ENTRIES IN it_um
          WHERE a~klart EQ '002'
            AND a~objek EQ it_um-objek
            AND a~atinn EQ l_rp_status.
  ENDIF.
ENDFORM.                    "GET_DATA

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
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data.
  DATA: lt_vmaster LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
        lt_vmas_r LIKE TABLE OF zspp_vin_value WITH HEADER LINE.

  DATA: l_date LIKE sy-datum,
        l_time LIKE sy-uzeit.

  DATA: l_count TYPE i,
        l_allo_dt LIKE sy-datum,
        l_num(8) TYPE n,
        l_objek(18),
        l_flag(1),
        l_null   LIKE sy-datum.

  l_date = sy-datum.
  l_time = sy-uzeit.

  SORT it_vmas BY objek.

  LOOP AT it_um.
    CLEAR: lt_vmaster, lt_vmaster[].
    CLEAR: lt_vmas_r, lt_vmas_r[],l_flag .

    READ TABLE it_vmas WITH KEY objek = it_um-objek BINARY SEARCH.
    CHECK sy-subrc = 0.

    IF it_um-dealer_dt EQ space.
      CLEAR: it_um-dealer_dt.
    ENDIF.

    IF it_um-dealer_dt IS INITIAL.
      CASE it_um-wo_nation.
        WHEN 'B28'.      "HMA
          IF it_vmas-rp_status >= '23'.
            it_um-dealer_dt = l_num = it_vmas-rp23_vpc.
          ENDIF.
        WHEN OTHERS.     "not HMA
          CASE it_vmas-rp_status.
            WHEN '25'.
              it_um-dealer_dt = l_num = it_vmas-rp25_sout.
            WHEN '27'.
              it_um-dealer_dt = l_num = it_vmas-rp27_sout.
          ENDCASE.
      ENDCASE.
    ENDIF.

    IF it_um-dealer_dt EQ space.
      CLEAR: it_um-dealer_dt.
    ENDIF.

    l_allo_dt = l_num = it_vmas-allo_dt.

    CHECK l_allo_dt         <> it_um-dealer_dt OR
          it_vmas-dealer_no <> it_um-wo_dealer1.

    IF it_um-dealer_dt IS INITIAL.
      lt_vmaster-atnam = 'P_RP21_SHOP_DATE'.
      CLEAR: lt_vmaster-atwrt.
      APPEND lt_vmaster.
      lt_vmaster-atnam = 'P_DEALER_NO'.
      lt_vmaster-atwrt = it_um-wo_dealer1.
      APPEND lt_vmaster.
      lt_vmaster-atnam = 'P_RP21_ACTUAL_DATE'.
      CLEAR: lt_vmaster-atwrt.
      APPEND lt_vmaster.
    ELSE.
      lt_vmaster-atnam = 'P_RP21_SHOP_DATE'.
      lt_vmaster-atwrt = it_um-dealer_dt.
      APPEND lt_vmaster.
      lt_vmaster-atnam = 'P_DEALER_NO'.
      lt_vmaster-atwrt = it_um-wo_dealer1.
      APPEND lt_vmaster.
      lt_vmaster-atnam = 'P_RP21_ACTUAL_DATE'.
      CONCATENATE l_date l_time INTO lt_vmaster-atwrt.
      APPEND lt_vmaster.
    ENDIF.

    l_objek = it_vmas-objek.

    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
      EXPORTING
        object       = l_objek
        mode         = 'W'
      TABLES
        val_table    = lt_vmaster
      EXCEPTIONS
        no_data      = 1
        error_mode   = 2
        error_object = 3
        error_value  = 4
        OTHERS       = 5.
    IF sy-subrc = 0.
      l_count = l_count + 1.
    ENDIF.
  ENDLOOP.

  WRITE: / 'Total updated records: ', l_count.
ENDFORM.                    " PROCESS_DATA

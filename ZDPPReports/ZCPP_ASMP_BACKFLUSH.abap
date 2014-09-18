*&----------------------------------------------------------------------
*& Development ID :
*& Program ID     : ZCPP_ASMP_BACKFLUSH
*& Program Name   : AS/MP Backflush
*& Created by     : Victor Park
*& Created on     : 07.07.2014
*& Issue Doc No.  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*&======================================================================
*& Desc.
*&
*&----------------------------------------------------------------------

REPORT  zcpp_asmp_backflush NO STANDARD PAGE HEADING LINE-SIZE 132
                                             MESSAGE-ID zmpp.

TABLES : mara, marc,  ztpp_asmppr, afko, mapl, msta.

DATA : BEGIN OF it_data OCCURS 0,
        werks LIKE ztpp_asmppr-werks,
        matnr LIKE ztpp_asmppr-matnr,
        rdatu LIKE ztpp_asmppr-rdatu,
*        pqty type ztpp_asmppr-pqty,
        pqty TYPE i,
        aufnr LIKE ztpp_asmppr-aufnr,
*        porel LIKE ztpp_asmppr-porel,
        pocon LIKE ztpp_asmppr-pocon,
        mblnr_h LIKE ztpp_asmppr-mblnr_h,
        mjahr LIKE ztpp_asmppr-mjahr,
*        zmode LIKE ztpp_asmppr-zmode,
        zresult LIKE ztpp_asmppr-zresult,
        zmsg  LIKE ztpp_asmppr-zmsg,
      END OF it_data.

DATA : it_asmppr     LIKE ztpp_asmppr OCCURS 0 WITH HEADER LINE,
       it_save       LIKE ztpp_asmppr OCCURS 0 WITH HEADER LINE.

*-BDC Objects
DATA: BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF it_bdc.

DATA: BEGIN OF it_opt OCCURS 0.
        INCLUDE STRUCTURE ctu_params.
DATA: END   OF it_opt.

DATA : BEGIN OF mess_tab OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA : END OF mess_tab.

DATA : disp_mode TYPE ctu_params-dismode VALUE 'N'.
DATA: lv_msg LIKE cfgnl-msglin.

DATA : w_flag(1).

*DATA : it_asmppp   LIKE ztpp_asmppp OCCURS 0 WITH HEADER LINE,
*       it_asmppp_h LIKE ztpp_asmppp_h OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------*
* SELECTION-SCREEN.
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
SELECT-OPTIONS : s_werks  FOR   marc-werks NO-EXTENSION
                                 OBLIGATORY MEMORY ID wrk.
SELECT-OPTIONS : s_matnr FOR  mara-matnr.
*SELECT-OPTIONS : s_date  FOR ztpp_asmppr-zedat OBLIGATORY.
SELECTION-SCREEN SKIP.
PARAMETERS     : p_error AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* INITIALIZATION.
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM init.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen.

*----------------------------------------------------------------------*
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.
  CLEAR: w_flag.
  PERFORM locking_rtn USING sy-repid w_flag.

  PERFORM check_data.
  PERFORM select_data.
  PERFORM process_data.
  PERFORM write_log.



*----------------------------------------------------------------------*
* END-OF-SELECTION.
*----------------------------------------------------------------------*
END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
FORM process_data .
  DATA: it_orders    LIKE bapi_order_key    OCCURS 0 WITH HEADER LINE,
        it_ordrtn    LIKE bapi_order_return OCCURS 0 WITH HEADER LINE.
  DATA: st_orderdata LIKE bapi_pp_order_create,
        st_return2    LIKE bapiret2,
        st_return1    LIKE bapiret1.

  DATA: it_header LIKE bapi_pp_hdrlevel      OCCURS 0 WITH HEADER LINE,
        it_item LIKE bapi2017_gm_item_create OCCURS 0 WITH HEADER LINE,
        it_return LIKE bapi_coru_return      OCCURS 0 WITH HEADER LINE.

  DATA : BEGIN OF it_mseg OCCURS 0,
          mblnr LIKE mseg-mblnr,
          mjahr LIKE mseg-mjahr,
          bwart LIKE mseg-bwart,
        END OF it_mseg.

  DATA : l_budat TYPE sy-datum,
         l_menge(10).

  DATA: v_aufnr TYPE aufnr.

  DATA : e_result TYPE  zresult,
         e_msg    TYPE  bapi_msg,
         l_msgtxt(200).

  LOOP AT it_data.

    PERFORM check_basic_data CHANGING it_data-zresult it_data-zmsg.

    IF it_data-zresult IS NOT INITIAL.
      MODIFY it_data.
      PERFORM update_asmppr USING 'A'.
      CONTINUE.
    ENDIF.

*-Production Order and Release
    IF it_data-aufnr IS INITIAL.
      CLEAR : mess_tab[], it_bdc[], mess_tab, it_bdc.

      WRITE : it_data-pqty  TO l_menge,
              it_data-rdatu TO l_budat.

      PERFORM bdc_dynpro USING :
              'X' 'SAPLCOKO1'             '0100',
              ' ' 'BDC_CURSOR'           'CAUFVD-MATNR',
              ' ' 'BDC_OKCODE'           '/00',
              ' ' 'CAUFVD-MATNR'         it_data-matnr,
              ' ' 'CAUFVD-WERKS'         it_data-werks,
              ' ' 'AUFPAR-PP_AUFART'     'PP01'.

      PERFORM bdc_dynpro USING :
              'X' 'SAPLCOKO1'             '0115',
              ' ' 'BDC_CURSOR'           'CAUFVD-GSTRP',
              ' ' 'BDC_OKCODE'           '=FREI',
              ' ' 'CAUFVD-GAMNG'         l_menge,
              ' ' 'CAUFVD-GMEIN'         'EA',
              ' ' 'CAUFVD-GLTRP'          l_budat,
              ' ' 'CAUFVD-GSTRP'          l_budat,
              ' ' 'CAUFVD-TERKZ'          '3',
              ' ' 'CAUFVD-FHORI'         '000'.

      PERFORM bdc_dynpro USING :
              'X' 'SAPLCOKO1'             '0115',
              ' ' 'BDC_CURSOR'           'CAUFVD-GAMNG',
              ' ' 'BDC_OKCODE'           '=BU',
              ' ' 'CAUFVD-GAMNG'         l_menge,
*            ' ' 'CAUFVD-GMEIN'         'EA',
              ' ' 'CAUFVD-GLTRP'          l_budat,
              ' ' 'CAUFVD-GSTRP'          l_budat,
              ' ' 'CAUFVD-TERKZ'          '3',
              ' ' 'CAUFVD-FHORI'         '000'.

      CALL TRANSACTION 'CO01' USING it_bdc MODE disp_mode
                                    UPDATE 'S'
                                    MESSAGES INTO mess_tab.

      READ TABLE mess_tab WITH KEY msgtyp = 'S'
                                   msgnr  = '100'.
      IF sy-subrc = 0.
        it_data-zresult = 'E'.
        it_data-zmsg    = ''.
        it_data-aufnr   = mess_tab-msgv1.
        PERFORM call_alpha_conversion CHANGING it_data-aufnr.

        DO 1000 TIMES.
          SELECT SINGLE * FROM afko WHERE aufnr = it_data-aufnr.
          IF sy-subrc EQ 0.
            EXIT.
          ENDIF.
        ENDDO.

      ELSE.
        READ TABLE mess_tab WITH KEY msgtyp = 'E'.
        IF sy-subrc = 0.
          CALL FUNCTION 'MESSAGE_TEXT_BUILD'
            EXPORTING
              msgid               = mess_tab-msgid
              msgnr               = mess_tab-msgnr
              msgv1               = mess_tab-msgv1
              msgv2               = mess_tab-msgv2
              msgv3               = mess_tab-msgv3
              msgv4               = mess_tab-msgv4
            IMPORTING
              message_text_output = lv_msg.

          it_data-zresult = 'E'.
          it_data-zmsg    = lv_msg.
        ENDIF.
      ENDIF.

      PERFORM update_asmppr USING 'A'.
    ENDIF.

**-1.Create Production Order
*    IF it_data-aufnr IS INITIAL.
*      CLEAR: st_orderdata, st_return2.
*
*      st_orderdata-material         = it_data-matnr.
*      st_orderdata-plant            = it_data-werks.
*      st_orderdata-order_type       = 'PP01'.
*      st_orderdata-basic_start_date = it_data-rdatu.
*      st_orderdata-basic_end_date   = it_data-rdatu.
*      st_orderdata-quantity         = it_data-pqty.
*      st_orderdata-quantity_uom     = 'EA'.
*
*      CALL FUNCTION 'BAPI_PRODORD_CREATE'
*        EXPORTING
*          orderdata    = st_orderdata
*        IMPORTING
*          return       = st_return2
*          order_number = v_aufnr.
*
*      IF v_aufnr IS INITIAL OR
*         st_return2-type EQ 'E' OR
*         st_return2-type EQ 'A' OR
*         st_return2-type EQ 'X'.
*        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*
*        it_data-zresult = 'P'.
*        it_data-zmsg    = st_return2-message.
*
*      ELSE.
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*          EXPORTING
*            wait = 'X'.
*        DO 1000 TIMES.
*          SELECT SINGLE * FROM afko WHERE aufnr = v_aufnr.
*          IF sy-subrc EQ 0.
*            EXIT.
*          ENDIF.
*        ENDDO.
*        it_data-zresult = 'P'.
*        it_data-zmsg    = ''.
*        it_data-aufnr   = v_aufnr.
*      ENDIF.
*
*    ENDIF.
*
**-2.Release Production Order
*    IF it_data-porel  IS INITIAL AND it_data-aufnr IS NOT INITIAL.
*      CLEAR : st_return2, it_orders[], it_orders.
*
*      MOVE: v_aufnr TO it_orders.
*      APPEND it_orders.
*
*      CALL FUNCTION 'BAPI_PRODORD_RELEASE'
*        EXPORTING
*          release_control    = '1'
*          work_process_group = 'PG_BF'
*          work_process_max   = 10
*        IMPORTING
*          return             = st_return2
*        TABLES
*          orders             = it_orders
*          detail_return      = it_ordrtn.
*
*      IF st_return2-type EQ 'E' OR
*         st_return2-type EQ 'A' OR
*         st_return2-type EQ 'X'.
*
*        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*
*        it_data-porel   = ''.
*        it_data-zresult = 'P'.
*        it_data-zmsg    = st_return2-message.
*
*      ELSE.
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*          EXPORTING
*            wait = 'X'.
*
*        it_data-porel   = 'X'.
*        it_data-zresult = 'P'.
*        it_data-zmsg    = ''.
*
*      ENDIF.
*
*    ENDIF.

*-.Production Order Confirmation
    IF  it_data-aufnr IS NOT INITIAL AND it_data-pocon   IS INITIAL.
      CLEAR: it_header[], it_header, st_return1,
             it_return[], it_return.

      it_header-orderid         = it_data-aufnr.
      it_header-fin_conf        = 'X'.
      it_header-postg_date      = it_data-rdatu.
      it_header-conf_quan_unit  = 'EA'.
      it_header-yield           = it_data-pqty.
      it_header-exec_start_date = sy-datum.
      it_header-exec_start_time = sy-uzeit.
      it_header-exec_fin_date   = sy-datum.
      it_header-exec_fin_time   = sy-uzeit.
*      it_header-conf_text       = 'test'.
      APPEND it_header.

      CALL FUNCTION 'BAPI_PRODORDCONF_CREATE_HDR'
        IMPORTING
          return        = st_return1
        TABLES
          athdrlevels   = it_header
          detail_return = it_return.

      READ TABLE it_return WITH KEY id = 'RU'
                                    number = '100'.
      IF sy-subrc = 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        DO 10 TIMES.
          SELECT single mblnr MJAHR
            into (it_data-mblnr_h, IT_DATA-MJAHR)
          FROM mseg
          WHERE werks   = it_data-werks
            AND zbudat  =  it_data-rdatu
            AND bwart   = '101'
            AND aufnr = it_data-aufnr.
          IF sy-subrc eq 0.
            EXIT.
          ENDIF.
          WAIT UP TO 1 SECONDS.
        ENDDO.
*
*        READ TABLE it_mseg WITH KEY bwart = '101'.
*        IF sy-subrc = 0.
*          it_data-mblnr_h = it_mseg-mblnr.
*        ENDIF.
*        READ TABLE it_mseg WITH KEY bwart = '261'.
*        IF sy-subrc = 0.
*          it_data-mblnr_i = it_mseg-mblnr.
*        ENDIF.

*        it_data-mjahr   = it_mseg-mjahr.
        it_data-zresult = 'S'.
        it_data-pocon   = 'X'.
        it_data-zmsg    = ''.

      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        read table it_return with key type = 'E'.
        it_data-pocon   = ''.
        it_data-zresult = 'E'.
        it_data-zmsg    = it_return-message.

      ENDIF.

      PERFORM update_asmppr USING 'B'.
    ENDIF.

    MODIFY it_data.
  ENDLOOP.


**-apply process result to the internal table
*  SORT it_data BY werks matnr rdatu aufnr.
*  LOOP AT it_asmppr.
*    IF it_asmppr-aufnr IS INITIAL.
*      READ TABLE it_data WITH KEY werks = it_asmppr-werks
*                                  matnr = it_asmppr-matnr
*                                  rdatu = it_asmppr-rdatu
*                                  BINARY SEARCH.
*      IF sy-subrc = 0.
*        it_asmppr-aufnr   = it_data-aufnr.
**        it_asmppr-porel   = it_data-porel.
*        it_asmppr-poCON   = it_data-poCON.
*        it_asmppr-mblnr_h = it_data-mblnr_h.
*        it_asmppr-mblnr_i = it_data-mblnr_i.
*        it_asmppr-mjahr   = it_data-mjahr.
*        it_asmppr-zresult = it_data-zresult.
*        it_asmppr-zmsg    = it_data-zmsg.
*
*      ENDIF.
*    ELSE.
*      READ TABLE it_data WITH KEY werks = it_asmppr-werks
*                                  matnr = it_asmppr-matnr
*                                  rdatu = it_asmppr-rdatu
*                                  aufnr = it_asmppr-aufnr
*                                  BINARY SEARCH.
*      IF sy-subrc = 0.
*        it_asmppr-aufnr   = it_data-aufnr.
**        it_asmppr-porel   = it_data-porel.
*        it_asmppr-poCON   = it_data-poCON.
*        it_asmppr-mblnr_h = it_data-mblnr_h.
*        it_asmppr-mblnr_i = it_data-mblnr_i.
*        it_asmppr-mjahr   = it_data-mjahr.
*        it_asmppr-zresult = it_data-zresult.
*        it_asmppr-zmsg    = it_data-zmsg.
*      ENDIF.
*
*
*    ENDIF.
*
*
*    it_asmppr-zbdat = sy-datum.
*    it_asmppr-zbtim = sy-uzeit.
*    it_asmppr-zbnam = sy-uname.
*
*    MODIFY it_asmppr.
*  ENDLOOP.
*
**-table update
*  MODIFY ztpp_asmppr FROM TABLE it_asmppr.
*  IF sy-subrc = 0.
*    COMMIT WORK AND WAIT.
*  ELSE.
*    ROLLBACK WORK.
*  ENDIF.

ENDFORM.                    " PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  CHECK_DATA
*&---------------------------------------------------------------------*
FORM check_data .
  LOOP AT s_werks.
    IF s_werks-low+0(1) <> 'P'.
      MESSAGE s000 WITH 'Engine plant is not acceptable'.
      STOP.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " CHECK_DATA
*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
FORM select_data .
  CLEAR : it_data[], it_data.

  IF p_error = 'X'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_asmppr
    FROM ztpp_asmppr AS a
    WHERE a~matnr IN s_matnr
      AND a~werks IN s_werks
*      AND a~zedat IN s_date
      AND a~zresult IN ('', 'I', 'E').
  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_asmppr
    FROM ztpp_asmppr AS a
    WHERE a~matnr IN s_matnr
      AND a~werks IN s_werks
*      AND a~zedat IN s_date
      AND a~zresult IN ('', 'I').
  ENDIF.

  LOOP AT it_asmppr.
    MOVE-CORRESPONDING it_asmppr TO it_data.
    CLEAR : it_data-zresult, it_data-zmsg.

    COLLECT it_data.
  ENDLOOP.


  IF it_data[] IS INITIAL.
    MESSAGE s000 WITH 'There is No data'.
    STOP.
  ENDIF.
ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*
FORM modify_screen .
  LOOP AT SCREEN.
    IF screen-name = 'S_WERKS-HIGH'.
      screen-invisible = 1.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " MODIFY_SCREEN
*&---------------------------------------------------------------------*
*&      Form  WRITE_LOG
*&---------------------------------------------------------------------*
FORM write_log  .
  WRITE:/ '--------------------------------'.
  WRITE:/        'Backflush Result'.
  WRITE:/ '--------------------------------'.
  SKIP.


  WRITE AT : 1(5) 'Plant', 10(20) 'Material No', 30(10) 'quantity', 40(15) 'Prd.Order', 60(15) 'Confirm', 80(6)'Result', 90(100) 'Message text'.
  ULINE.

  LOOP AT it_data.
    WRITE AT : /1(5) it_data-werks, 10(20) it_data-matnr, 30(10) it_data-pqty, 40(15) it_data-aufnr, 60(15) it_data-pocon,
               80(6) it_data-zresult, 90(100) it_data-zmsg.
  ENDLOOP.
ENDFORM.                    " WRITE_LOG
*&---------------------------------------------------------------------*
*&      Form  INIT
*&---------------------------------------------------------------------*
FORM init .
*  CLEAR : s_date[], s_date.
*  s_date-sign = 'I'.
*  s_date-option = 'BT'.
*  s_date-low  = sy-datum - 7.
*  s_date-high = sy-datum.
*  APPEND s_date.

ENDFORM.                    " INIT

*&---------------------------------------------------------------------*
*&      Form  LOCKING_RTN
*&---------------------------------------------------------------------*
FORM locking_rtn  USING p_repid p_result.
  PERFORM check_lock_object  USING p_repid p_result.
  PERFORM check_enqueue_read USING p_repid p_result.
  PERFORM check_batchjob     USING p_repid p_result.

ENDFORM.                    " LOCKING_RTN
*&---------------------------------------------------------------------*
*&      Form  CHECK_LOCK_OBJECT
*&---------------------------------------------------------------------*
FORM check_lock_object  USING p_repid p_result.
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
    MOVE: 'E' TO p_result.
  ENDIF.
ENDFORM.                    " CHECK_LOCK_OBJECT
*&---------------------------------------------------------------------*
*&      Form  CHECK_ENQUEUE_READ
*&---------------------------------------------------------------------*
FORM check_enqueue_read  USING p_repid  p_result.
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
    p_result = 'E'.
    EXIT.
  ENDIF.

  IF l_lock_number > 1.
    p_result = 'E'.
  ENDIF.
ENDFORM.                    " CHECK_ENQUEUE_READ
*&---------------------------------------------------------------------*
*&      Form  CHECK_BATCHJOB
*&---------------------------------------------------------------------*
FORM check_batchjob  USING p_repid  p_result.
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
      p_result = 'E'.
    ENDIF.
  ELSE.
    READ TABLE lt_joblist INDEX 1.
    IF sy-subrc EQ 0.
      p_result = 'E'.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_BATCHJOB
*&---------------------------------------------------------------------*
*&      Form  CHECK_BASIC_DATA
*&---------------------------------------------------------------------*
FORM check_basic_data  CHANGING p_zresult
                                p_zmsg.
  SELECT SINGLE *
  FROM mapl
  WHERE matnr = it_data-matnr
    AND werks = it_data-werks.
  IF sy-subrc <> 0.
    p_zresult = 'E'.
    p_zmsg    = 'No Routing Information'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
  FROM msta
  WHERE matnr =  it_data-matnr
    AND statm = 'B'.
  IF sy-subrc <> 0.
    p_zresult = 'E'.
    p_zmsg    = 'Material View Missing: Accounting'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
  FROM msta
  WHERE matnr =  it_data-matnr
    AND statm = 'D'.
  IF sy-subrc <> 0.
    p_zresult = 'E'.
    p_zmsg    = 'Material View Missing: MRP'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
  FROM msta
  WHERE matnr =  it_data-matnr
    AND statm = 'A'.
  IF sy-subrc <> 0.
    p_zresult = 'E'.
    p_zmsg    = 'Material View Missing: Work scheduling'.
    EXIT.
  ENDIF.

ENDFORM.                    " CHECK_BASIC_DATA
*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
FORM bdc_dynpro   USING dynbegin name value.

  IF dynbegin = 'X'.
    CLEAR it_bdc.
    MOVE: dynbegin TO it_bdc-dynbegin,
          name TO it_bdc-program,
          value TO it_bdc-dynpro.
    APPEND it_bdc.
  ELSE.
    CLEAR it_bdc.
    MOVE: name TO it_bdc-fnam,
          value TO it_bdc-fval.
    APPEND it_bdc.
  ENDIF.

ENDFORM.                    " BDC_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  CALL_ALPHA_CONVERSION
*&---------------------------------------------------------------------*
FORM call_alpha_conversion   CHANGING p_para.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_para
    IMPORTING
      output = p_para.
ENDFORM.                    " CALL_ALPHA_CONVERSION
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ASMPPR
*&---------------------------------------------------------------------*
FORM update_asmppr USING p_flag.
  CLEAR : it_save[], it_save.

  IF p_flag = 'A'.
    LOOP AT it_asmppr WHERE werks = it_data-werks
                        AND matnr = it_data-matnr
                        AND rdatu = it_data-rdatu
                        AND aufnr = ''.

      it_asmppr-aufnr   = it_data-aufnr.
      it_asmppr-pocon   = it_data-pocon.
      it_asmppr-mblnr_h = it_data-mblnr_h.
      IT_ASMPPR-MJAHR   = IT_DATA-MJAHR.
      it_asmppr-zresult = it_data-zresult.
      it_asmppr-zmsg    = it_data-zmsg.
      it_asmppr-zbdat = sy-datum.
      it_asmppr-zbtim = sy-uzeit.
      it_asmppr-zbnam = sy-uname.

      MOVE-CORRESPONDING it_asmppr TO it_save.

      APPEND it_save. CLEAR it_save.
      MODIFY it_asmppr.
    ENDLOOP.
  ELSE.
    LOOP AT it_asmppr WHERE werks = it_data-werks
                        AND matnr = it_data-matnr
                        AND rdatu = it_data-rdatu
                        AND aufnr = it_data-aufnr.
*        it_asmppr-aufnr   = it_data-aufnr.
      it_asmppr-pocon   = it_data-pocon.
      it_asmppr-mblnr_h = it_data-mblnr_h.
      IT_ASMPPR-MJAHR   = IT_DATA-MJAHR.
      it_asmppr-zresult = it_data-zresult.
      it_asmppr-zmsg    = it_data-zmsg.
      it_asmppr-zbdat = sy-datum.
      it_asmppr-zbtim = sy-uzeit.
      it_asmppr-zbnam = sy-uname.
      MOVE-CORRESPONDING it_asmppr TO it_save.

      APPEND it_save. CLEAR it_save.
      MODIFY it_asmppr.
    ENDLOOP.

  ENDIF.

*-table update
  MODIFY ztpp_asmppr FROM TABLE it_save.
  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " UPDATE_ASMPPR

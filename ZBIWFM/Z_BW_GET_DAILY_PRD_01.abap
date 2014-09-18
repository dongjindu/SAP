FUNCTION z_bw_get_daily_prd_01.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_REQUNR) TYPE  SRSC_S_IF_SIMPLE-REQUNR
*"     VALUE(I_DSOURCE) TYPE  SRSC_S_IF_SIMPLE-DSOURCE OPTIONAL
*"     VALUE(I_MAXSIZE) TYPE  SRSC_S_IF_SIMPLE-MAXSIZE OPTIONAL
*"     VALUE(I_INITFLAG) TYPE  SRSC_S_IF_SIMPLE-INITFLAG OPTIONAL
*"     VALUE(I_READ_ONLY) TYPE  SRSC_S_IF_SIMPLE-READONLY OPTIONAL
*"     VALUE(I_REMOTE_CALL) TYPE  SBIWA_FLAG OPTIONAL
*"  TABLES
*"      I_T_SELECT TYPE  SRSC_S_IF_SIMPLE-T_SELECT OPTIONAL
*"      I_T_FIELDS TYPE  SRSC_S_IF_SIMPLE-T_FIELDS OPTIONAL
*"      E_T_DATA STRUCTURE  ZBWS0001 OPTIONAL
*"  EXCEPTIONS
*"      NO_MORE_DATA
*"      ERROR_PASSED_TO_MESS_HANDLER
*"----------------------------------------------------------------------

* Example: DataSource for table SFLIGHT
  TABLES: ztppvr, ztpp_rpid, ztpp_vm.

* Auxiliary Selection criteria structure
  DATA: l_s_select TYPE srsc_s_select.

* Maximum number of lines for DB table
  STATICS: s_s_if TYPE srsc_s_if_simple,

* counter
          s_counter_datapakid LIKE sy-tabix,

* cursor
          s_cursor TYPE cursor,

* shift range
          l_r_datum TYPE RANGE OF datum,
          l_r_t_1st TYPE RANGE OF zrp_actdate,
          l_r_t_2nd TYPE RANGE OF zrp_actdate,
          l_r_t_3rd TYPE RANGE OF zrp_actdate.

* Select ranges
  RANGES: l_r_carrid  FOR sflight-carrid,
          l_r_connid  FOR sflight-connid.

*... ranges for time stamp.
  RANGES: l_r_t_stamp FOR ztpp_rpid-rp01_adate.
*          l_r_t_1st   FOR ztpp_rpid-rp01_adate,
*          l_r_t_2nd   FOR ztpp_rpid-rp01_adate,
*          l_r_t_3rd   FOR ztpp_rpid-rp01_adate.

*  RANGES: lc_r_t_1st   FOR ztpp_rpid-rp01_adate,
*          lc_r_t_2nd   FOR ztpp_rpid-rp01_adate,
*          lc_r_t_3rd   FOR ztpp_rpid-rp01_adate.

*... ranges for base date.
  DATA: l_s_datum LIKE LINE OF l_r_datum,
        l_r_s_1st LIKE LINE OF l_r_t_1st ,
        l_r_s_2nd LIKE LINE OF l_r_t_2nd ,
        l_r_s_3rd LIKE LINE OF l_r_t_3rd .

  DATA: lt_ztppvr TYPE TABLE OF ztppvr,
        ls_ztppvr TYPE ztppvr,
        lt_ztpp_rpid TYPE TABLE OF ztpp_rpid,
        ls_ztpp_rpid TYPE ztpp_rpid.

  DATA: BEGIN OF ls_vm_info,
         model_code TYPE zmodelcode,
         body_no    TYPE zsd_body,
         wo_nation  TYPE znation,
         rp_cstatus TYPE zrp_cstatus,
         dealer_dt  TYPE datum,
         status	    TYPE zustatus,
        END OF ls_vm_info.

  DATA: lt_vm_info LIKE TABLE OF ls_vm_info.

  DATA: BEGIN OF ls_rp_tab,
         l_rpxx TYPE zrp_shopdate,
         l_name(20),
        END OF ls_rp_tab.

  DATA: lt_rp_tab LIKE TABLE OF ls_rp_tab.


  DATA: BEGIN OF ls_body_info,
         model_code TYPE  zmodelcode,
         body_no    TYPE zsd_body,
        END OF ls_body_info.

  DATA: lt_body_info LIKE TABLE OF ls_body_info.

  DATA: l_idx(2) TYPE n,
        l_datum TYPE datum.

  DATA: l_fname1(50) TYPE c,
        l_fname2(50) TYPE c.

  FIELD-SYMBOLS: <fs1>, <fs2>.

* Initialization mode (first call by SAPI) or data transfer mode
* (following calls) ?
  IF i_initflag = sbiwa_c_flag_on.

************************************************************************
* Initialization: check input parameters
*                 buffer input parameters
*                 prepare data selection
************************************************************************

* Check DataSource validity
    CASE i_dsource.
      WHEN 'ZBW_PP_DAILY_PRD_01_FUNC'.
      WHEN OTHERS.
        IF 1 = 2. MESSAGE e009(r3). ENDIF.
* this is a typical log call. Please write every error message like this
        log_write 'E'                  "message type
                  'R3'                 "message class
                  '009'                "message number
                  i_dsource   "message variable 1
                  ' '.                 "message variable 2
        RAISE error_passed_to_mess_handler.
    ENDCASE.

    APPEND LINES OF i_t_select TO s_s_if-t_select.

* Fill parameter buffer for data extraction calls
    s_s_if-requnr    = i_requnr.
    s_s_if-dsource = i_dsource.
    s_s_if-maxsize   = i_maxsize.

* Fill field list table for an optimized select statement
* (in case that there is no 1:1 relation between InfoSource fields
* and database table fields this may be far from beeing trivial)
    APPEND LINES OF i_t_fields TO s_s_if-t_fields.

  ELSE.                 "Initialization mode or data extraction ?

************************************************************************
* Data transfer: First Call      OPEN CURSOR + FETCH
*                Following Calls FETCH only
************************************************************************

* First data package -> OPEN CURSOR
    IF s_counter_datapakid = 0.

* Fill range tables BW will only pass down simple selection criteria
* of the type SIGN = 'I' and OPTION = 'EQ' or OPTION = 'BT'.

      LOOP AT s_s_if-t_select INTO l_s_select WHERE fieldnm = 'H_STRPDT'.
        MOVE-CORRESPONDING l_s_select TO l_r_t_stamp.
        APPEND l_r_t_stamp.
      ENDLOOP.

      SORT l_r_t_stamp BY low high.

      LOOP AT l_r_t_stamp.
*        l_r_t_stamp-low  = '20111121063000'.
        l_s_datum-sign   = l_r_t_stamp-sign.
        l_s_datum-option = l_r_t_stamp-option.
        l_s_datum-low    = l_r_t_stamp-low+0(8).

        IF l_r_t_stamp-high+0(8) EQ ''.
          l_datum = l_s_datum-low.
          l_s_datum-high = l_datum + 1.
        ELSE.
          l_s_datum-high   = l_r_t_stamp-high+0(8).
        ENDIF.

        APPEND l_s_datum TO l_r_datum.
*        modify l_r_t_stamp.
      ENDLOOP.

***... read shift infomation.
**      DATA: ls_shift_info TYPE ztbw_shift_info,
**            ls_day_attr   TYPE casdayattr,
**            lt_day_attr   TYPE TABLE OF casdayattr.
**
**      READ TABLE l_r_datum INTO l_s_datum INDEX 1.
**
**      l_datum = l_s_datum-low+0(8).
**
**      IF l_datum IS INITIAL.
**        l_datum = sy-datum.
**      ENDIF.
**
**      SELECT SINGLE * INTO ls_shift_info
**        FROM ztbw_shift_info
**       WHERE bdate EQ l_datum.
**
**      IF sy-subrc EQ 0.
**
**        l_r_s_1st-sign   = l_r_s_2nd-sign   = l_r_s_3rd-sign   = 'I'.
**        l_r_s_1st-option = l_r_s_2nd-option = l_r_s_3rd-option = 'BT'.
**        l_r_s_1st-low    = l_r_s_2nd-low    = l_r_s_3rd-low    = l_datum.
**        l_r_s_1st-high   = l_r_s_2nd-high   = l_datum.
**        l_datum = l_datum + 1.
**        l_r_s_3rd-high   = l_datum.
**
**        CONCATENATE: l_r_s_1st-low ls_shift_info-begin_1st INTO l_r_s_1st-low,
**                     l_r_s_2nd-low ls_shift_info-begin_2nd INTO l_r_s_2nd-low,
**                     l_r_s_3rd-low ls_shift_info-begin_3rd INTO l_r_s_3rd-low.
**
**        CONCATENATE: l_r_s_1st-high ls_shift_info-end_1st INTO l_r_s_1st-high,
**                     l_r_s_2nd-high ls_shift_info-end_2nd INTO l_r_s_2nd-high,
**                     l_r_s_3rd-high ls_shift_info-end_3rd INTO l_r_s_3rd-high.
**
**        APPEND: l_r_s_1st TO l_r_t_1st,
**                l_r_s_2nd TO l_r_t_2nd,
**                l_r_s_3rd TO l_r_t_3rd.
**
**      ELSE.
***... get day attribute.
**        CALL FUNCTION 'DAY_ATTRIBUTES_GET'
**          EXPORTING
**            date_from                  = l_datum
**            date_to                    = l_datum
**          TABLES
**            day_attributes             = lt_day_attr
**          EXCEPTIONS
**            factory_calendar_not_found = 1
**            holiday_calendar_not_found = 2
**            date_has_invalid_format    = 3
**            date_inconsistency         = 4
**            OTHERS                     = 5.
**
**        READ TABLE lt_day_attr INTO ls_day_attr INDEX 1.
**
**        CLEAR ls_shift_info.
**
**        SELECT SINGLE * INTO ls_shift_info
**          FROM ztbw_shift_info
**         WHERE wotnr EQ ls_day_attr-weekday.
**
**        IF l_datum <> ''.
**
**          l_r_s_1st-sign   = l_r_s_2nd-sign   = l_r_s_3rd-sign   = 'I'.
**          l_r_s_1st-option = l_r_s_2nd-option = l_r_s_3rd-option = 'BT'.
**          l_r_s_1st-low    = l_r_s_2nd-low    = l_r_s_3rd-low    = l_datum.
**          l_r_s_1st-high   = l_r_s_2nd-high   = l_datum.
**          l_datum = l_datum + 1.
**          l_r_s_3rd-high   = l_datum.
**
**          CONCATENATE: l_r_s_1st-low ls_shift_info-begin_1st INTO l_r_s_1st-low,
**                       l_r_s_2nd-low ls_shift_info-begin_2nd INTO l_r_s_2nd-low,
**                       l_r_s_3rd-low ls_shift_info-begin_3rd INTO l_r_s_3rd-low.
**
**          CONCATENATE: l_r_s_1st-high ls_shift_info-end_1st INTO l_r_s_1st-high,
**                       l_r_s_2nd-high ls_shift_info-end_2nd INTO l_r_s_2nd-high,
**                       l_r_s_3rd-high ls_shift_info-end_3rd INTO l_r_s_3rd-high.
**
**          APPEND: l_r_s_1st TO l_r_t_1st,
**                  l_r_s_2nd TO l_r_t_2nd,
**                  l_r_s_3rd TO l_r_t_3rd.
**        ENDIF.
**      ENDIF.

*      LOOP AT l_r_t_stamp.
*        l_s_datum-sign   = l_r_t_stamp-sign.
*        l_s_datum-option = l_r_t_stamp-option.
*        l_s_datum-low    = l_r_t_stamp-low+0(8).
*
*        IF l_r_t_stamp-high+0(8) EQ ''.
*          l_datum = l_s_datum-low.
*          l_s_datum-high = l_datum + 1.
*        ELSE.
*          l_s_datum-high   = l_r_t_stamp-high+0(8).
*        ENDIF.
*
*        APPEND l_s_datum TO l_r_datum.
*      ENDLOOP.

      IF l_r_t_1st[] IS INITIAL.
        READ TABLE l_r_t_stamp INDEX 1.
        IF sy-subrc EQ 0.
          l_datum = l_r_t_stamp-low+0(8).
          l_r_s_1st-sign   = 'I'.
          l_r_s_1st-option = 'BT'.
          l_r_s_1st-low    = l_r_t_stamp-low.

          CONCATENATE l_datum `144459` INTO l_r_s_1st-high.

          APPEND l_r_s_1st TO l_r_t_1st.
        ENDIF.
      ENDIF.

      IF l_r_t_2nd[] IS INITIAL.

        READ TABLE l_r_t_stamp INDEX 1.
        IF sy-subrc EQ 0.
          l_datum = l_r_t_stamp-low+0(8).


          l_r_s_2nd-sign   = 'I'.
          l_r_s_2nd-option = 'BT'.
          CONCATENATE l_datum `144500` INTO l_r_s_2nd-low.
          CONCATENATE l_datum `224459` INTO l_r_s_2nd-high.

          APPEND l_r_s_2nd TO l_r_t_2nd.
        ENDIF.
      ENDIF.

      IF l_r_t_3rd[] IS INITIAL.

        READ TABLE l_r_t_stamp INDEX 1.
        IF sy-subrc EQ 0.
          l_datum = l_r_t_stamp-low+0(8).
          l_r_s_3rd-sign   = 'I'.
          l_r_s_3rd-option = 'BT'.

          CONCATENATE l_datum `224500` INTO l_r_s_3rd-low.

          l_datum = l_datum + 1.

          CONCATENATE l_datum l_r_t_stamp-high+8(6) INTO l_r_s_3rd-high.

*          CONCATENATE l_datum `064459` INTO l_r_s_3rd-high.

          APPEND l_r_s_3rd TO l_r_t_3rd.
        ENDIF.
      ENDIF.

* Determine number of database records to be read per FETCH statement
* from input parameter I_MAXSIZE. If there is a one to one relation
* between DataSource table lines and database entries, this is trivial.
* In other cases, it may be impossible and some estimated value has to
* be determined.
      IF l_r_datum[] IS NOT INITIAL.
        OPEN CURSOR WITH HOLD s_cursor FOR
        SELECT * FROM ztppvr
                WHERE zbdat IN l_r_datum.
      ENDIF.
    ENDIF.                             "First data package ?

    CHECK l_r_datum[] IS NOT INITIAL.
* Fetch records into interface table.
*   named E_T_'Name of extract structure'.
    FETCH NEXT CURSOR s_cursor
               APPENDING CORRESPONDING FIELDS
               OF TABLE lt_ztppvr
               PACKAGE SIZE s_s_if-maxsize.

    IF sy-subrc <> 0.
      CLOSE CURSOR s_cursor.
      RAISE no_more_data.
    ENDIF.

*... Fill the data.
    LOOP AT lt_ztppvr INTO ls_ztppvr.
      ls_body_info-model_code = ls_ztppvr-p_model.
      ls_body_info-body_no    = ls_ztppvr-p_body_serial.
      APPEND ls_body_info TO lt_body_info.
    ENDLOOP.

    SORT lt_body_info BY model_code body_no.

    IF lt_body_info[] IS NOT INITIAL.

      SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_ztpp_rpid
        FROM ztpp_rpid
         FOR ALL ENTRIES IN  lt_body_info
       WHERE model_code EQ lt_body_info-model_code
         AND body_no    EQ lt_body_info-body_no.


      LOOP AT lt_ztpp_rpid INTO ls_ztpp_rpid.

        l_idx = 1.

*wono
        DATA: l_serial LIKE ztpp_vm-wo_serial,
              l_nation LIKE ztpp_vm-wo_nation,
              l_dealer LIKE ztpp_vm-wo_dealer.

        CLEAR: l_serial, l_nation, l_dealer.

        SELECT SINGLE wo_serial wo_nation wo_dealer
          INTO (l_serial, l_nation, l_dealer)
          FROM ztpp_vm
         WHERE model_code EQ ls_ztpp_rpid-model_code
           AND body_no    EQ ls_ztpp_rpid-body_no.

* RP01 ~ RP27
        DO 27 TIMES.
          CLEAR: l_fname1, l_fname2.

          CONCATENATE `LS_ZTPP_RPID-RP` l_idx `_ADATE` INTO l_fname1.
          CONCATENATE `E_T_DATA-RP`     l_idx INTO l_fname2.

          CONDENSE: l_fname1, l_fname2.

          ASSIGN: (l_fname1) TO <fs1>, (l_fname2) TO <fs2>.

          CONCATENATE ls_ztpp_rpid-model_code ls_ztpp_rpid-body_no INTO e_t_data-body_no.
          CONCATENATE l_serial l_nation l_dealer INTO e_t_data-h_wono.

          IF <fs1> IN l_r_t_1st.
            e_t_data-shift = 1.

            <fs2> = 1.

            READ TABLE l_r_t_1st INTO l_r_s_1st INDEX 1.

            e_t_data-bdate = l_r_s_1st-low+0(8).

            COLLECT e_t_data.

            CLEAR: e_t_data.

          ELSEIF <fs1> IN l_r_t_2nd.
            e_t_data-shift = 2.

            <fs2> = 1.

            READ TABLE l_r_t_2nd INTO l_r_s_2nd INDEX 1.

            e_t_data-bdate = l_r_s_2nd-low+0(8).

            COLLECT e_t_data.

            CLEAR: e_t_data.

          ELSEIF <fs1> IN l_r_t_3rd.
            e_t_data-shift = 3.

            <fs2> = 1.

            READ TABLE l_r_t_3rd INTO l_r_s_3rd INDEX 1.

            e_t_data-bdate = l_r_s_3rd-low+0(8).

            COLLECT e_t_data.

            CLEAR: e_t_data.

          ENDIF.

          l_idx = l_idx + 1.
        ENDDO.
      ENDLOOP.

      SORT lt_body_info.

      DELETE ADJACENT DUPLICATES FROM lt_body_info COMPARING ALL FIELDS.

* get dealer alloc date from UM
      SELECT a~model_code a~body_no a~wo_nation a~rp_cstatus
             b~dealer_dt b~status
        INTO CORRESPONDING FIELDS OF TABLE lt_vm_info
        FROM ztpp_vm AS a INNER JOIN ztsd_um AS b
          ON a~model_code = b~model_code
         AND a~body_no    = b~body_no
         FOR ALL ENTRIES IN  lt_body_info
       WHERE a~model_code EQ lt_body_info-model_code
         AND a~body_no    EQ lt_body_info-body_no
         AND ( b~status = ' ' OR b~status EQ 'F').

      SORT lt_vm_info BY model_code body_no.

      SORT e_t_data BY body_no.

      LOOP AT e_t_data.

        CASE e_t_data-h_wono+9(3).

          WHEN 'B28'. "HMA

            READ TABLE lt_vm_info INTO ls_vm_info WITH KEY model_code = e_t_data-body_no(3)
                                                           body_no    = e_t_data-body_no+3(6).
*check if dealer_dt eq prod. date
            IF sy-subrc EQ 0 AND ( ls_vm_info-dealer_dt EQ e_t_data-bdate ).
              ADD 1 TO e_t_data-alloc.
              MODIFY e_t_data.
            ENDIF.

          WHEN OTHERS.

            DATA: l_rp21 TYPE ztpp_rpid-rp21_sdate,
                  l_rp22 TYPE ztpp_rpid-rp21_sdate,
                  l_rp23 TYPE ztpp_rpid-rp21_sdate,
                  l_rp24 TYPE ztpp_rpid-rp21_sdate,
                  l_rp25 TYPE ztpp_rpid-rp21_sdate,
                  l_rp26 TYPE ztpp_rpid-rp21_sdate,
                  l_rp27 TYPE ztpp_rpid-rp21_sdate.

            DATA : tmp_rp_c(1) TYPE n, tmp_str(30), l_rpxx TYPE zrp_shopdate.
            DATA : l_rpst TYPE ztpp_vm-rp_cstatus.

            CLEAR: l_rp21, l_rpst, ls_rp_tab, lt_rp_tab[].

            SELECT SINGLE a~rp_cstatus b~rp21_sdate b~rp22_sdate
                          b~rp23_sdate b~rp24_sdate b~rp25_sdate
                          b~rp26_sdate b~rp27_sdate
              INTO (l_rpst , l_rp21, l_rp22, l_rp23, l_rp24, l_rp25, l_rp26, l_rp27)
              FROM ztpp_vm AS a INNER JOIN ztpp_rpid AS b
                ON a~model_code = b~model_code
               AND a~body_no    = b~body_no
             WHERE a~model_code EQ e_t_data-body_no(3)
               AND a~body_no    EQ e_t_data-body_no+3(6).

            IF l_rpst >= '21'.                              "<> B28

              DO 7 TIMES VARYING l_rpxx FROM l_rp21 NEXT l_rp22.

                IF l_rpxx IS INITIAL.
                  CONTINUE.
                ENDIF.

                ls_rp_tab-l_rpxx = l_rpxx.
                tmp_rp_c = sy-index.
                CONCATENATE 'l_rp2' tmp_rp_c INTO tmp_str.
                ls_rp_tab-l_name = tmp_str.

                IF ls_rp_tab-l_rpxx <> ' '.
                  APPEND ls_rp_tab TO lt_rp_tab.
                ENDIF.

              ENDDO.

              SORT lt_rp_tab BY l_rpxx.
              READ TABLE lt_rp_tab INTO ls_rp_tab INDEX 1.

              IF sy-subrc EQ 0 AND ( ls_rp_tab-l_rpxx EQ e_t_data-bdate ).
                " Earliest date. --> ls_rp_tab-l_rpxx
                ADD 1 TO e_t_data-alloc.
                MODIFY e_t_data.
              ENDIF.

            ENDIF.

        ENDCASE.

*        CLEAR lt_vm_info.
*        READ TABLE lt_vm_info INTO ls_vm_info WITH KEY model_code = e_t_data-body_no(3)
*                                                       body_no    = e_t_data-body_no+3(6).
*
*        IF sy-subrc EQ 0.
*          IF ls_vm_info-wo_nation EQ 'B28'.
**            IF ls_vm_info-dealer_dt IS NOT INITIAL.
*            IF ls_vm_info-dealer_dt EQ e_t_data-bdate.
*              ADD 1 TO e_t_data-alloc.
*            ENDIF.
*          ELSE.
**
*            DATA: l_rp21 TYPE sy-datum, l_rpst(2).
*
*            CLEAR: l_rp21, l_rpst.
*
*            SELECT SINGLE a~rp_cstatus b~rp21_adate
*              INTO (l_rpst , l_rp21)
*              FROM ztpp_vm AS a INNER JOIN ztpp_rpid AS b
*                ON a~model_code = b~model_code
*               AND a~body_no    = b~body_no
**         FOR ALL ENTRIES IN  lt_body_info
*             WHERE a~model_code EQ e_t_data-body_no(3)
*               AND a~body_no    EQ e_t_data-body_no+3(6).
*
*            IF l_rpst >= '21' AND l_rp21 = e_t_data-bdate.
*              ADD 1 TO e_t_data-alloc.
*            ENDIF.
*
*          ENDIF.
*          MODIFY e_t_data.
*        ENDIF.

      ENDLOOP.

      LOOP AT s_s_if-t_select INTO l_s_select WHERE fieldnm = 'H_STRPDT'.
        MOVE-CORRESPONDING l_s_select TO l_r_t_stamp.
        APPEND l_r_t_stamp.
      ENDLOOP.

      READ TABLE l_r_t_stamp INDEX 1.

      CLEAR: e_t_data.

      e_t_data-h_strpdt = l_r_t_stamp-low.

      MODIFY e_t_data TRANSPORTING  h_strpdt WHERE h_strpdt EQ ''.

    ENDIF.
    s_counter_datapakid = s_counter_datapakid + 1.
  ENDIF.              "Initialization mode or data extraction ?
ENDFUNCTION.

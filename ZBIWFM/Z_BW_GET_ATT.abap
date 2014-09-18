FUNCTION Z_BW_GET_ATT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_REQUNR) TYPE  SRSC_S_IF_SIMPLE-REQUNR
*"     VALUE(I_DSOURCE) TYPE  SRSC_S_IF_SIMPLE-DSOURCE OPTIONAL
*"     VALUE(I_MAXSIZE) TYPE  SRSC_S_IF_SIMPLE-MAXSIZE OPTIONAL
*"     VALUE(I_INITFLAG) TYPE  SRSC_S_IF_SIMPLE-INITFLAG OPTIONAL
*"     VALUE(I_READ_ONLY) TYPE  SRSC_S_IF_SIMPLE-READONLY OPTIONAL
*"  TABLES
*"      I_T_SELECT TYPE  SRSC_S_IF_SIMPLE-T_SELECT OPTIONAL
*"      I_T_FIELDS TYPE  SRSC_S_IF_SIMPLE-T_FIELDS OPTIONAL
*"      E_T_DATA STRUCTURE  ZBWST_ATT OPTIONAL
*"  EXCEPTIONS
*"      NO_MORE_DATA
*"      ERROR_PASSED_TO_MESS_HANDLER
*"----------------------------------------------------------------------

* Example: DataSource for table SFLIGHT
  TABLES: t001p, ztco_mh_dws.

* Auxiliary Selection criteria structure
  DATA: l_s_select TYPE srsc_s_select.

* Maximum number of lines for DB table
  STATICS: s_s_if TYPE srsc_s_if_simple,

* counter
          s_counter_datapakid LIKE sy-tabix,

* cursor
          s_cursor TYPE cursor.
* Select ranges
  RANGES: l_r_carrid  FOR sflight-carrid,
          l_r_connid  FOR sflight-connid.


*  DATA: BEGIN OF lt_eng OCCURS 0,
*          matnr   LIKE mara-matnr,
*        END OF lt_eng.

  DATA : l_sign(1), l_option(2).
  DATA : l_sday LIKE sy-datum, "start date
         l_eday like sy-datum. "end date
  DATA : l_tabix LIKE sy-tabix.
  DATA : l_tabix1 LIKE sy-tabix.
  DATA : l_tabname LIKE dd02d-tabname.
  DATA : l_mosid LIKE t001p-mosid.
  DATA : lt_att LIKE zbwst_att OCCURS 0 WITH HEADER LINE.
  DATA : $tprog(4).

*  DATA : BEGIN OF l_stb OCCURS 0,
*          matnr LIKE mara-matnr.
*          INCLUDE STRUCTURE stpox.
*  DATA END OF l_stb.

  DATA : l_subrc LIKE sy-subrc.
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
      WHEN 'ZBW_HR_ATT_FUNC'.
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
    s_s_if-dsource   = i_dsource.
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
* for user exit
      REFRESH: lt_att.

***********************
      l_eday = sy-datum. l_sday = l_eday - 2.
*      l_eday = '20091106'. l_sday = '20091106'.

      select * into corresponding fields of table lt_att
      from ZTHRATTNCOR
      where RDATE >= l_sday
        and RDATE <= l_eday.

      OPEN CURSOR WITH HOLD s_cursor FOR
      SELECT * FROM t157e
                               WHERE spras EQ 'EN'
                                 AND bwart EQ '551'.
    ENDIF.                             "First data package ?

* Fetch records into interface table.
*   named E_T_'Name of extract structure'.

    FETCH NEXT CURSOR s_cursor
               APPENDING CORRESPONDING FIELDS
               OF TABLE e_t_data
               PACKAGE SIZE s_s_if-maxsize.


    IF sy-subrc <> 0.
      CLOSE CURSOR s_cursor.
      RAISE no_more_data.
    ENDIF.

    s_counter_datapakid = s_counter_datapakid + 1.

    REFRESH e_t_data. CLEAR lt_att.

    LOOP AT lt_att.

      MOVE-CORRESPONDING lt_att TO e_t_data.

* to get Personnel Subarea Grouping for Work Schedules
      SELECT SINGLE mosid INTO l_mosid
       FROM t001p
      WHERE werks = lt_att-WERKS
        AND btrtl = lt_att-BTRTL.
*
* to get shift information
* new DWS
      CALL FUNCTION 'Z_CO_GET_DWS_IG'
           EXPORTING
                schkz                          = lt_att-schkz
                datum                          = lt_att-rdate
           IMPORTING
                tprog                          = $tprog
           EXCEPTIONS
                not_found_work_schedule_rules  = 1
                invalid_date                   = 2
                not_found_period_work_schedule = 3
                OTHERS                         = 4.


      lt_att-schkz = $tprog.

      CLEAR ztco_mh_dws.
      SELECT SINGLE *
        FROM ztco_mh_dws
       WHERE mosid = l_mosid
         AND schkz = lt_att-schkz
         AND zsdat <= lt_att-rdate
         AND zedat >= lt_att-rdate.

      IF sy-subrc EQ 0.
        e_t_data-anzsh = ztco_mh_dws-zshif.
      ENDIF.

      APPEND e_t_data.

    ENDLOOP.


  ENDIF.              "Initialization mode or data extraction ?



ENDFUNCTION.

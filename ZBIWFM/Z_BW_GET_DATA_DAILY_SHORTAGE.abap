FUNCTION z_bw_get_data_daily_shortage.
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
*"      E_T_DATA STRUCTURE  ZOXUD10135 OPTIONAL
*"  EXCEPTIONS
*"      NO_MORE_DATA
*"      ERROR_PASSED_TO_MESS_HANDLER
*"----------------------------------------------------------------------

* Example: DataSource for table SFLIGHT
  TABLES: ztmm_daily_short.

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


  DATA: BEGIN OF lt_remark OCCURS 0,
          matnr   LIKE ztmm_h_short_eis-matnr,
          remark  LIKE ztmm_h_short_eis-remark,
          to_date LIKE ztmm_h_short_eis-to_date,
          to_time LIKE ztmm_h_short_eis-to_time,
        END OF lt_remark.

  DATA: BEGIN OF lt_stg_range OCCURS 0,
          fieldname LIKE ztbw_stg_ranges-fieldname,
          low       LIKE ztbw_stg_ranges-low,
          high      LIKE ztbw_stg_ranges-high,
        END OF lt_stg_range.

  RANGES: r_matnr FOR ztmm_hour_short-matnr,
          r_mtart FOR ztmm_hour_short-mtart,
          r_matkl FOR ztmm_hour_short-matkl,
          r_dispo FOR ztmm_hour_short-dispo,
          r_lifnr FOR ztmm_hour_short-lifnr,
          r_fname FOR dd03l-fieldname.

  DATA : l_sign(1), l_option(2).
  DATA : l_tabix LIKE sy-tabix.
  DATA : l_tabix1 LIKE sy-tabix.
  DATA : l_tabname LIKE dd02d-tabname.

DATA : l_zoxud10134 LIKE zoxud10134 OCCURS 0 WITH HEADER LINE, "Hr Short
       l_zoxud10135 LIKE zoxud10135 OCCURS 0 WITH HEADER LINE. "Dy Short

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
      WHEN 'ZBW_MM_DAILY_SHORT_FUNC'.
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

    SELECT fieldname INTO r_fname-low
      FROM dd03l
     WHERE tabname = 'ZTMM_DAILY_SHORT'.
      r_fname-sign   = 'E'.
      r_fname-option = 'EQ'.
      r_fname-high   = space.
      APPEND r_fname.
    ENDSELECT.

    DELETE s_s_if-t_fields WHERE fieldnm IN r_fname.

  ELSE.                 "Initialization mode or data extraction ?

************************************************************************
* Data transfer: First Call      OPEN CURSOR + FETCH
*                Following Calls FETCH only
************************************************************************

* First data package -> OPEN CURSOR
    IF s_counter_datapakid = 0.
* for user exit
      REFRESH: lt_remark, r_matnr, r_mtart, r_matkl, r_dispo, r_lifnr.

      l_tabname = 'ZTMM_D_SHORT_EIS'.
      SELECT matnr remark to_date to_time
        INTO TABLE lt_remark
        FROM (l_tabname)
       WHERE to_date >= sy-datum
          OR to_date EQ '00000000'.

      LOOP AT lt_remark.
        l_tabix = sy-tabix.
        IF lt_remark-to_date EQ sy-datum.
          IF lt_remark-to_time EQ space.
            lt_remark-to_time = '240000'.
          ENDIF.
          IF lt_remark-to_time < sy-uzeit.
            DELETE lt_remark INDEX l_tabix.
          ENDIF.
        ENDIF.
      ENDLOOP.
      SORT lt_remark.

* Get Excluding Condition
      l_tabname = 'ZTBW_STG_RANGESD'.
      SELECT fieldname low high INTO TABLE lt_stg_range
        FROM (l_tabname).

      LOOP AT lt_stg_range.

        TRANSLATE lt_stg_range-low USING '*^'.
        SEARCH lt_stg_range-low FOR '^'.
        IF sy-subrc EQ 0.
          l_option = 'CP'.
          TRANSLATE lt_stg_range-low USING '^*'.
        ELSEIF lt_stg_range-high IS INITIAL.
          l_option = 'EQ'.
        ELSE.
          l_option = 'BT'.
        ENDIF.
        l_sign = 'E'.

        CASE lt_stg_range-fieldname.
          WHEN 'MATNR'.
            r_matnr-sign   = l_sign.
            r_matnr-option = l_option.
            r_matnr-low    = lt_stg_range-low.
            r_matnr-high   = lt_stg_range-high.
            APPEND r_matnr.
          WHEN 'MTART'.
            r_mtart-sign   = l_sign.
            r_mtart-option = l_option.
            r_mtart-low    = lt_stg_range-low.
            r_mtart-high   = lt_stg_range-high.
            APPEND r_mtart.
          WHEN 'MATKL'.
            r_matkl-sign   = l_sign.
            r_matkl-option = l_option.
            r_matkl-low    = lt_stg_range-low.
            r_matkl-high   = lt_stg_range-high.
            APPEND r_matkl.
          WHEN 'DISPO'.
            r_dispo-sign   = l_sign.
            r_dispo-option = l_option.
            r_dispo-low    = lt_stg_range-low.
            r_dispo-high   = lt_stg_range-high.
            APPEND r_dispo.
          WHEN 'LIFNR'.
            r_lifnr-sign   = l_sign.
            r_lifnr-option = l_option.
            r_lifnr-low    = lt_stg_range-low.
            r_lifnr-high   = lt_stg_range-high.
            APPEND r_lifnr.
        ENDCASE.
      ENDLOOP.

*      r_matkl-sign   = 'I'.
*      r_matkl-option = 'EQ'.
*      r_matkl-low    = '|'.
*      r_matkl-high   = space.
*      APPEND r_matkl.
*
*      r_matnr = r_matkl.
*      APPEND r_matnr.
*
*      r_dispo = r_matkl.
*      APPEND r_dispo.
*
*      r_lifnr = r_matkl.
*      APPEND r_lifnr.
*
*      r_mtart = r_matkl.
*      APPEND r_mtart.


      OPEN CURSOR WITH HOLD s_cursor FOR
      SELECT (s_s_if-t_fields) FROM ztmm_daily_short
       WHERE matnr IN r_matnr
         AND mtart IN r_mtart
         AND matkl IN r_matkl
         AND dispo IN r_dispo
         AND lifnr IN r_lifnr.
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

    LOOP AT e_t_data INTO l_zoxud10135.
      l_tabix = sy-tabix.

      READ TABLE lt_remark WITH KEY matnr = l_zoxud10135-matnr
                             BINARY SEARCH.
      IF sy-subrc EQ 0.
        l_zoxud10135-remark = lt_remark-remark.
      ENDIF.

      MODIFY e_t_data FROM l_zoxud10135 INDEX l_tabix.

    ENDLOOP.

  ENDIF.              "Initialization mode or data extraction ?



ENDFUNCTION.

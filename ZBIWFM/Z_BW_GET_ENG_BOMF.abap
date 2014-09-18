FUNCTION z_bw_get_eng_bomf.
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
*"      E_T_DATA STRUCTURE  ZBWSTBF OPTIONAL
*"  EXCEPTIONS
*"      NO_MORE_DATA
*"      ERROR_PASSED_TO_MESS_HANDLER
*"----------------------------------------------------------------------

* Example: DataSource for table SFLIGHT
*  TABLES: mara.

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


  DATA: BEGIN OF lt_eng OCCURS 0,
          WERKS like marc-WERKS,
          matnr   LIKE mara-matnr,
        END OF lt_eng.

  DATA : l_sign(1), l_option(2).
  DATA : l_tday LIKE sy-datum. "today
  DATA : l_tabix LIKE sy-tabix.
  DATA : l_tabix1 LIKE sy-tabix.
  DATA : l_tabname LIKE dd02d-tabname.

  DATA : lt_stb LIKE stpox OCCURS 0 WITH HEADER LINE.

  DATA : BEGIN OF l_stb OCCURS 0,
          matnr LIKE mara-matnr.
          INCLUDE STRUCTURE stpox.
  DATA END OF l_stb.

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
      WHEN 'Z_BW_GET_ENG_BOMF'.
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
      REFRESH: l_stb.

***********************
* get engine *
***********************
      REFRESH: lt_eng.

* get engine & 3C code
      SELECT WERKS matnr INTO CORRESPONDING FIELDS OF TABLE lt_eng
      FROM marc
      WHERE ( werks = 'E001' or werks = 'E002' )
*        AND mmsta = '12'
        AND sfepr = 'ENGI'
        AND ( fevor = 'SEA' OR fevor = 'SEC' ).

      SORT lt_eng BY matnr.

***begin of testing
*refresh lt_eng.
*lt_eng-matnr = 'AU52'.
*append lt_eng.
*** end of testing

***begin of need?
*      DATA: $ix LIKE sy-tabix,
*            $amt LIKE mard-labst.
*
*      CLEAR lt_eng.
*      LOOP AT lt_eng.
*        $ix = sy-tabix. $amt = 0.
*
*        SELECT SUM( labst ) INTO $amt
*        FROM mard
*        WHERE matnr = lt_eng-matnr
*        GROUP by matnr.
*        ENDSELECT.
** delete if stock = 0 or no records
*        IF sy-subrc EQ 0.
*          IF $amt EQ 0.
*            DELETE lt_eng INDEX $ix.
*          ENDIF.
*        ELSE.
*          DELETE lt_eng INDEX $ix.
*        ENDIF.
*
*      ENDLOOP.
***end of need?

***********************
***********************
      l_tday = sy-datum.

      LOOP AT lt_eng.

        CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
          EXPORTING
            capid                 = 'PP01'
            datuv                 = l_tday
            mtnrv                 = lt_eng-matnr
            werks                 = lt_eng-WERKS
            stlan                 = '1'
            stlal                 = '01'
            mmory                 = '1'  "Memory use On(1)
            sanka                 = ' '
                                    "Only Costing Relevency(inc.Phantom)
            ftrel                 = ' '
                              "stop explosion not relevant to production
            aumgb                 = ' '  "calculate scrap
            mdmps                 = 'X'
                                     "Limited multi-lvl- explode phantom
            mehrs                 = 'X'
                                "Multi-level explosion "Notes 729663 !!!
            rndkz                 = ' '
                                      "Round off: ' '=always, '1'=never,
            emeng                 = 1  "Required quantity
          TABLES
            stb                   = lt_stb
          EXCEPTIONS
            alt_not_found         = 1
            call_invalid          = 2
            material_not_found    = 3
            missing_authorization = 4
            no_bom_found          = 5
            no_plant_data         = 6
            no_suitable_bom_found = 7
            conversion_error      = 8
            OTHERS                = 9.

        IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.


*        clear l_stb.
        LOOP AT lt_stb.
          l_stb-matnr = lt_eng-matnr.
          MOVE-CORRESPONDING lt_stb TO l_stb.
          APPEND l_stb.
          CLEAR l_stb.
        ENDLOOP.

        REFRESH lt_stb.

      ENDLOOP.

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

    REFRESH e_t_data. CLEAR l_stb.

    LOOP AT l_stb WHERE dumps NE 'x'.
*      l_tabix = sy-tabix.
      MOVE-CORRESPONDING l_stb TO e_t_data.
* to fix qty issue
      e_t_data-menge = l_stb-mnglg.
*      APPEND e_t_data.
      COLLECT e_t_data.
    ENDLOOP.

*    LOOP AT e_t_data.
**      l_tabix = sy-tabix.
*      MOVE-CORRESPONDING e_t_data TO e_t_dataf.
**      APPEND e_t_dataf.
*      COLLECT e_t_dataf.
*    ENDLOOP.


  ENDIF.              "Initialization mode or data extraction ?



ENDFUNCTION.

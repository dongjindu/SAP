FUNCTION Z_MM_READ_MRP_LIST_PARALLEL_T.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_CM61X) LIKE  CM61X STRUCTURE  CM61X
*"     VALUE(IS_SFILT) LIKE  SFILT STRUCTURE  SFILT OPTIONAL
*"     VALUE(IV_INPER) LIKE  MDST-INPER OPTIONAL
*"     VALUE(IV_SVGRP) LIKE  PARUSER-SVGRP
*"     VALUE(IV_MAXTASK) LIKE  PARUSER-MAXTASK OPTIONAL
*"     VALUE(IV_PACKSIZE) LIKE  PARUSER-PACKSIZE OPTIONAL
*"     VALUE(I_START) TYPE  INT4
*"     VALUE(I_END) TYPE  INT4
*"  TABLES
*"      IT_MARA STRUCTURE  ZMMS219
*"      T_DATA STRUCTURE  ZTMM_EAI_DEMAND
*"--------------------------------------------------------------------



  DATA:
        lv_psize_default       TYPE i VALUE '10',
        lv_tasks_default       TYPE i VALUE '10'.

  DATA: ls_cm61x               TYPE cm61x,
        lv_group               TYPE rzlli_apcl,
        lv_max_tasks           TYPE i,
        lv_max_psize           TYPE i,
        lt_mdmwx               TYPE TABLE OF mdmw,
        ls_material            TYPE mds_parallel_read_result,
        lt_materials           TYPE TABLE OF mds_parallel_read_result,
        ls_mdmw                TYPE mdmw.
*        lt_period              TYPE TABLE OF zmms0028.
  DATA: lv_material_count      TYPE i,
    lv_count2              TYPE i,
    lv_index               TYPE i,
    lv_offset              TYPE i,
    lv_curr_line           TYPE i,
    lv_psize               TYPE i,
    lv_cur_jobs            TYPE i,
    lv_snd_jobs            TYPE i,
    lv_wp_total            TYPE i,
    lv_wp_avail            TYPE i,
    lv_msg(80)             TYPE c VALUE space,
    lv_taskname(10)        TYPE n VALUE '0000000000',
    lv_server_name         LIKE pbtresourc-servername,
    lv_text(132)           TYPE c,
    lv_rtime               TYPE i.    "Zeitspanne in msecs

  DATA:   lv_parallel_failed     TYPE c.

*---<
  DATA : lv_lines LIKE sy-index.

  DATA : it_daily  LIKE mdsu OCCURS 0 WITH HEADER LINE,
         it_weekly LIKE mdsu OCCURS 0 WITH HEADER LINE,
         wa_mdsu   LIKE mdsu OCCURS 0 WITH HEADER LINE.
*  DATA : it_pp0001 LIKE ztpp0001  OCCURS 0 WITH HEADER LINE.
  DATA : it_data  LIKE ZTMM_EAI_DEMAND OCCURS 0 WITH HEADER LINE.
  DATA : l_tabix(2) TYPE n.

  FIELD-SYMBOLS : <date>,
                  <receipt>,
                  <demand>.

  DATA : r_field(30).
  DATA : d_field(30).
  DATA : q_field(30).
  DATA : i_date TYPE d.
  DATA : l_week(8).

  data: l_ptype like it_data-ptype.
  DATA: T_MARA LIKE TABLE OF IT_MARA WITH HEADER LINE,
        WS_TEMP_MARA LIKE LINE OF IT_MARA.
  DATA: BEGIN OF LT_TEMP OCCURS 4,
        WERKS LIKE MARC-WERKS,
        END OF LT_TEMP.

*  CLEAR :  it_pp0001[].

*  SELECT *
*    INTO CORRESPONDING FIELDS OF TABLE it_pp0001
*    FROM ztpp0001
*    WHERE code  = '07'.
*
*  SORT it_pp0001 BY key1.

  LOOP AT It_mara.
    CLEAR: T_MARA[], T_MARA, LT_TEMP[], LT_TEMP.


*    if sy-tabix >= i_start and sy-tabix <= l_end.
    IF sy-tabix < i_start OR  sy-tabix > i_end.
      CONTINUE.
    ENDIF.

*    READ TABLE it_pp0001 WITH KEY key1  =  t_mara-vspvb
*                                             BINARY SEARCH.
*    IF sy-subrc = 0.
*      it_data-ptype = it_pp0001-key2+0(1).
*    ENDIF.

*** to get PTYPE
    case It_mara-vspvb+0(1).
      When  'P'.
        l_ptype = 'P'.
      When  'B'.
        l_ptype = 'B'.
      When Others.
        l_ptype = 'T'.
    Endcase.

** Get multi-plant
    SELECT WERKS INTO TABLE LT_TEMP
      FROM MARC
      WHERE MATNR = It_mara-MATNR
        AND LVORM = ' '.

    WS_TEMP_MARA = IT_MARA.
    LOOP AT LT_TEMP.
      WS_TEMP_MARA-WERKS = LT_TEMP-WERKS.
      APPEND WS_TEMP_MARA TO T_MARA.
      CLEAR: WS_TEMP_MARA-WERKS.
    ENDLOOP.

    LOOP AT T_MARA.

      CLEAR : it_daily[], it_weekly[], it_data, l_tabix.    "it_pp0001.
*---daily
      CALL FUNCTION 'MD_MRP_LIST_API'
        EXPORTING
          matnr                    = t_mara-matnr
          werks                    = t_mara-werks
          inper                    = 'T'
        TABLES
          mdsux                    = it_daily[]
        EXCEPTIONS
          mrp_list_not_found       = 1
          material_plant_not_found = 2
          error                    = 3
          OTHERS                   = 4.
      IF sy-subrc = 0.
        CLEAR : wa_mdsu.
        READ TABLE it_daily INDEX 1  INTO wa_mdsu.

        DELETE it_daily WHERE sort0 < sy-datum.

        LOOP AT it_daily.

          CHECK it_daily-perkz  = 'T'.

          IF l_tabix > 22.
            EXIT.
          ENDIF.

          IF l_tabix  = 0.
            it_data-plnt    = 'P'.
            it_data-line    = '1'.
            it_data-pdate   = wa_mdsu-dat00.
*          it_data-idnrk   = t_mara-matnr.
            it_data-beskz   = t_mara-beskz.
            it_data-meins   = t_mara-meins.
            it_data-init_qty  = wa_mdsu-mng04.
            it_data-EDMD_TYPE   = 'D'.
            it_data-etnam   = sy-uname.
            it_data-etdat   = sy-datum.
            it_data-ettim   = sy-uzeit.
            it_data-ptype =  l_ptype.
            IT_DATA-EPART_NO = IT_MARA-MATNR.
          ENDIF.

*-date
          CONCATENATE 'IT_DATA-DAT_D' l_tabix INTO d_field.
          ASSIGN (d_field) TO <date>.
          <date>  = it_daily-sort0.

*-receipt
          CONCATENATE 'IT_DATA-RET_D' l_tabix INTO r_field.
          ASSIGN (r_field) TO <receipt>.
          <receipt>  = it_daily-tgpro.

**-demand
*        CONCATENATE 'IT_DATA-DPLN' l_tabix INTO q_field.
*        ASSIGN (q_field) TO <demand>.
*        <demand>  = it_daily-mng01 + it_daily-mng02.
*        IF <demand> < 0.   "send positive qty
*          <demand> = -1 * <demand>.
*        ENDIF.
          l_tabix = l_tabix + 1.

        ENDLOOP.

        IF it_daily[] IS NOT INITIAL.
** to merge plants
*        APPEND it_data.
          COLLECT it_data.
        ENDIF.
        CLEAR : it_data.
      ENDIF.


      i_date  =  sy-datum - 6. "to delete old week
**---weekly
      CALL FUNCTION 'MD_MRP_LIST_API'
        EXPORTING
          matnr                    = t_mara-matnr
          werks                    = t_mara-werks
          inper                    = 'W'
        TABLES
          mdsux                    = it_weekly[]
        EXCEPTIONS
          mrp_list_not_found       = 1
          material_plant_not_found = 2
          error                    = 3
          OTHERS                   = 4.

      IF sy-subrc = 0.
        CLEAR : wa_mdsu, l_tabix.

        READ TABLE it_weekly INDEX 1  INTO wa_mdsu.

        DELETE it_weekly WHERE sort0 < i_date.

        LOOP AT it_weekly.

          CHECK it_weekly-perkz  = 'W'.

          IF l_tabix > 22.
            EXIT.
          ENDIF.

          IF l_tabix  = 0.
            it_data-plnt    = 'P'.
            it_data-line    = '1'.
*          it_data-ptype   = it_pp0001-key2+0(1).
            it_data-pdate   = wa_mdsu-dat00.
*          it_data-idnrk   = t_mara-matnr.
            it_data-beskz   = t_mara-beskz.
            it_data-meins   = t_mara-meins.
            it_data-init_qty  = wa_mdsu-mng04.
            it_data-EDMD_TYPE   = it_weekly-perkz.
            it_data-etnam   = sy-uname.
            it_data-etdat   = sy-datum.
            it_data-ettim   = sy-uzeit.
            it_data-ptype =  l_ptype.
            IT_DATA-EPART_NO = IT_MARA-MATNR.
          ENDIF.

*-date
          CONCATENATE 'IT_DATA-DAT_D' l_tabix INTO d_field.
          ASSIGN (d_field) TO <date>.
          l_week  = it_weekly-extsu+0(4).
          CONDENSE l_week NO-GAPS.
          <date>  = l_week.

*-receipt
          CONCATENATE 'IT_DATA-RET_D' l_tabix INTO r_field.
          ASSIGN (r_field) TO <receipt>.
          <receipt>  = it_weekly-mng03.

**-demand
*        CONCATENATE 'IT_DATA-DPLN' l_tabix INTO q_field.
*        ASSIGN (q_field) TO <demand>.
*        <demand>  = it_weekly-mng01 + it_weekly-mng02.
*        IF <demand> < 0.   "send positive qty
*          <demand> = -1 * <demand>.
*        ENDIF.
          l_tabix = l_tabix + 1.
        ENDLOOP.

        IF it_weekly[] IS NOT INITIAL.
** to merge plant
*        APPEND it_data.
          COLLECT it_data.
        ENDIF.

        CLEAR : it_data.
      ENDIF.

    ENDLOOP.

  ENDLOOP.

  t_data[]  = it_data[].

ENDFUNCTION.

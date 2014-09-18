FUNCTION z_eis_get_prd_summary_by_hour.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(CHECK_DATE) TYPE  SY-DATUM DEFAULT SY-DATUM
*"  TABLES
*"      ZPRDQTYHOUR STRUCTURE  ZPRDQTYHOUR
*"----------------------------------------------------------------------

  DATA: lt_ztppvr TYPE TABLE OF ztppvr,
        lt_ztppvr_ex TYPE TABLE OF ztppvr,
        ls_ztppvr TYPE ztppvr.

  DATA: l_btime    TYPE uzeit,
        l_etime    TYPE uzeit,
        l_bdatum   TYPE datum,
        l_edatum   TYPE datum,
        l_dflag(1) TYPE c,
        l_sbtime   TYPE uzeit,
        l_setime   TYPE uzeit,
        c_addtime TYPE uzeit VALUE '010000',
        l_datum    TYPE datum,
        l_check_date TYPE datum,
        l_seq(2) TYPE n,
        f_name(30).

  __cls  gt_zprdqtyhour.

  CLEAR :  p_status,
           qty_01,         qty_02,         qty_03,
           qty_04,         qty_05,         qty_06,
           qty_07,         qty_08,         qty_09,
           qty_10,         qty_11,         qty_12,
           qty_13,         qty_14,         qty_15,
           qty_16,         qty_17,         qty_18,
           qty_19,         qty_20,         qty_21,
           qty_22,         qty_23,         qty_24.

  CLEAR :  p_status,
           qty_01,         qty_02,         qty_03,
           qty_04,         qty_05,         qty_06,
           qty_07,         qty_08,         qty_09,
           qty_10,         qty_11,         qty_12,
           qty_13,         qty_14,         qty_15,
           qty_16,         qty_17,         qty_18,
           qty_19,         qty_20,         qty_21,
           qty_22,         qty_23,         qty_24.


  SELECT * INTO TABLE lt_ztppvr
    FROM ztppvr
    WHERE k04pdat EQ check_date
      AND p_status IN ('B01','P02','T01','T24','T25','T26')
       AND zresult  = 'S'
       AND NOT ( p_dest_code LIKE '%XX%' OR p_dest_code LIKE '%XY%' ).

  SORT lt_ztppvr BY p_status.

  l_btime  = '063000'.
  l_etime  = '072959'.
  l_bdatum = check_date.
  l_seq = 1.
  l_check_date  = check_date + 1.
  DO 24 TIMES.
    LOOP AT lt_ztppvr INTO ls_ztppvr.

      IF l_dflag EQ 'X'.
        CLEAR: l_sbtime, l_setime.

        l_setime = l_btime.
        l_setime+2(4) = '5959'.

        l_sbtime = l_etime.
        l_sbtime+2(4) = '0000'.

        IF NOT ( ( ls_ztppvr-p_rp_actual_time BETWEEN l_btime  AND l_setime OR
                 ls_ztppvr-p_rp_actual_time BETWEEN l_sbtime AND l_etime ) ).
          CONTINUE.
        ENDIF.
      ELSE.
        IF NOT ls_ztppvr-p_rp_actual_time BETWEEN l_btime AND l_etime.
          CONTINUE.
        ENDIF.
      ENDIF.

      CASE ls_ztppvr-p_status.
        WHEN 'B01'.
          f_name = 'ZPRDQTYHOUR-BI_QTY'.
        WHEN 'P02'.
          f_name = 'ZPRDQTYHOUR-PI_QTY'.
        WHEN 'T01'.
          f_name = 'ZPRDQTYHOUR-TR_QTY'.
        WHEN OTHERS.
          f_name = 'ZPRDQTYHOUR-SF_QTY'.
      ENDCASE.

      ASSIGN (f_name) TO <_qty>.

      IF ( ls_ztppvr-p_rp_actual_date EQ l_check_date AND
         ls_ztppvr-p_rp_actual_time > '063000' ).
        zprdqtyhour-worktime = 'WORKTIME_24'.
      ELSE.
        IF ( ls_ztppvr-p_rp_actual_date EQ check_date AND ls_ztppvr-p_rp_actual_time < '063000' ).
          CONTINUE.
        ELSE.
          CONCATENATE 'WORKTIME_' l_seq INTO zprdqtyhour-worktime.
        ENDIF.
      ENDIF.

      <_qty> = 1.

      COLLECT zprdqtyhour.
      CLEAR: zprdqtyhour.
    ENDLOOP.

    l_seq = l_seq + 1.

    CALL FUNCTION 'C14B_ADD_TIME'
      EXPORTING
        i_starttime = l_btime
        i_startdate = l_bdatum
        i_addtime   = c_addtime
      IMPORTING
        e_endtime   = l_btime
        e_enddate   = l_edatum.

    CALL FUNCTION 'C14B_ADD_TIME'
      EXPORTING
        i_starttime = l_etime
        i_startdate = l_bdatum
        i_addtime   = c_addtime
      IMPORTING
        e_endtime   = l_etime
        e_enddate   = l_edatum.

    IF l_bdatum <> l_edatum.
      l_dflag = 'X'.
    ELSE.
      CLEAR l_dflag.
    ENDIF.
  ENDDO.

  CONSTANTS :
      worktime_00(20) VALUE 'Total',
      worktime_01(20) VALUE '06:30~07:30',
      worktime_02(20) VALUE '07:30~08:30',
      worktime_03(20) VALUE '08:30~09:30',
      worktime_04(20) VALUE '09:30~10:30',
      worktime_05(20) VALUE '10:30~11:30',
      worktime_06(20) VALUE '11:30~12:30',
      worktime_07(20) VALUE '12:30~13:30',
      worktime_08(20) VALUE '13:30~14:30',
      worktime_09(20) VALUE '14:30~15:30',
      worktime_10(20) VALUE '15:30~16:30',
      worktime_11(20) VALUE '16:30~17:30',
      worktime_12(20) VALUE '17:30~18:30',
      worktime_13(20) VALUE '18:30~19:30',
      worktime_14(20) VALUE '19:30~20:30',
      worktime_15(20) VALUE '20:30~21:30',
      worktime_16(20) VALUE '21:30~22:30',
      worktime_17(20) VALUE '22:30~23:30',
      worktime_18(20) VALUE '23:30~00:30',
      worktime_19(20) VALUE '00:30~01:30',
      worktime_20(20) VALUE '01:30~02:30',
      worktime_21(20) VALUE '02:30~03:30',
      worktime_22(20) VALUE '03:30~04:30',
      worktime_23(20) VALUE '04:30~05:30',
      worktime_24(20) VALUE '05:30~06:30',
      worktime_25(20) VALUE '1st Total',
      worktime_26(20) VALUE '2nd Total',
      worktime_27(20) VALUE '3rd Total'.

  SORT zprdqtyhour BY worktime.

  DATA: zprdqtyhour1 LIKE zprdqtyhour OCCURS 8 WITH HEADER LINE,
        zprdqtyhour2 LIKE zprdqtyhour OCCURS 8 WITH HEADER LINE,
        zprdqtyhour3 LIKE zprdqtyhour OCCURS 8 WITH HEADER LINE.

  LOOP AT zprdqtyhour FROM 1 TO 8.
    zprdqtyhour1 = zprdqtyhour.
    APPEND zprdqtyhour1.
  ENDLOOP.

  LOOP AT zprdqtyhour FROM 9 TO 16.
    zprdqtyhour2 = zprdqtyhour.
    APPEND zprdqtyhour2.
  ENDLOOP.

  LOOP AT zprdqtyhour FROM 17.
    zprdqtyhour3 = zprdqtyhour.
    APPEND zprdqtyhour3.
  ENDLOOP.

  LOOP AT zprdqtyhour1.
    AT LAST.
      SUM.
      zprdqtyhour = zprdqtyhour1.
      zprdqtyhour-worktime = 'WORKTIME_25'.
      APPEND zprdqtyhour.
    ENDAT.
  ENDLOOP.

  LOOP AT zprdqtyhour2.
    AT LAST.
      SUM.
      zprdqtyhour = zprdqtyhour2.
      zprdqtyhour-worktime = 'WORKTIME_26'.
      APPEND zprdqtyhour.
    ENDAT.
  ENDLOOP.

  LOOP AT zprdqtyhour3.
    AT LAST.
      SUM.
      zprdqtyhour = zprdqtyhour3.
      zprdqtyhour-worktime = 'WORKTIME_27'.
      APPEND zprdqtyhour.
    ENDAT.
  ENDLOOP.

  FIELD-SYMBOLS : <_txt>.

  LOOP AT zprdqtyhour.
    ASSIGN (zprdqtyhour-worktime) TO <_txt>.
    zprdqtyhour-worktime = <_txt>.
    MODIFY zprdqtyhour INDEX sy-tabix.
  ENDLOOP.

*... Begin{ 3Shift, Commented original Source 06/26/2012
*  EXEC SQL PERFORMING GET_VALUE.
*    SELECT
*    P_STATUS,
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'063000'),
*                  least(P_RP_ACTUAL_TIME,'072959'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'073000'),
*                  least(P_RP_ACTUAL_TIME,'082959'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'083000'),
*                  least(P_RP_ACTUAL_TIME,'092959'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'093000'),
*                  least(P_RP_ACTUAL_TIME,'102959'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'103000'),
*                  least(P_RP_ACTUAL_TIME,'112959'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'113000'),
*                  least(P_RP_ACTUAL_TIME,'121459'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'121500'),
*                  least(P_RP_ACTUAL_TIME,'131459'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'131500'),
*                  least(P_RP_ACTUAL_TIME,'141459'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'141500'),
*                  least(P_RP_ACTUAL_TIME,'151459'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'151500'),
*                  least(P_RP_ACTUAL_TIME,'161459'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'161500'),
*                  least(P_RP_ACTUAL_TIME,'171459'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'171500'),
*                  least(P_RP_ACTUAL_TIME,'181459'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'181500'),
*                  least(P_RP_ACTUAL_TIME,'191459'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'191500'),
*                  least(P_RP_ACTUAL_TIME,'201459'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'201500'),
*                  least(P_RP_ACTUAL_TIME,'211459'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'211500'),
*                  least(P_RP_ACTUAL_TIME,'221459'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'221500'),
*                  least(P_RP_ACTUAL_TIME,'231459'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'231500'),
*                  least(P_RP_ACTUAL_TIME,'235959'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'000000'),
*                  least(P_RP_ACTUAL_TIME,'005959'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'010000'),
*                  least(P_RP_ACTUAL_TIME,'015959'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'020000'),
*                  least(P_RP_ACTUAL_TIME,'025959'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'030000'),
*                  least(P_RP_ACTUAL_TIME,'035959'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'040000'),
*                  least(P_RP_ACTUAL_TIME,'045959'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'050000'),
*                  least(P_RP_ACTUAL_TIME,'062959'), 1, 0))
*    INTO :p_status,
*         :qty_01,:qty_02,:qty_03,:qty_04,:qty_05,:qty_06,
*         :qty_07,:qty_08,:qty_09,:qty_10,:qty_11,:qty_12,
*         :qty_13,:qty_14,:qty_15,:qty_16,:qty_17,:qty_18,
*         :qty_19,:qty_20,:qty_21,:qty_22,:qty_23,:qty_24
*
*    FROM ztppvr
*   WHERE MANDT    = :SY-MANDT
*     AND K04PDAT  = :CHECK_DATE
*     AND P_STATUS IN ('B01','P02','T01','T24','T25','T26')
*     AND ZRESULT  = 'S'
*     AND NOT ( p_dest_code LIKE '%XX%' OR p_dest_code LIKE '%XY%' )
*     GROUP by P_STATUS
*
*  ENDEXEC.
*
*  CONSTANTS :
*            worktime_01(20) VALUE '06:30~07:30',
*            worktime_02(20) VALUE '07:30~08:30',
*            worktime_03(20) VALUE '08:30~09:30',
*            worktime_04(20) VALUE '09:30~10:30',
*            worktime_05(20) VALUE '10:30~11:30',
*            worktime_06(20) VALUE '11:30~12:15',
*            worktime_07(20) VALUE '12:15~13:15',
*            worktime_08(20) VALUE '13:15~14:15',
*            worktime_09(20) VALUE '14:15~15:15',
*            worktime_10(20) VALUE '15:15~16:15',
*            worktime_11(20) VALUE '16:15~17:15',
*            worktime_12(20) VALUE '17:15~18:15',
*            worktime_13(20) VALUE '18:15~19:15',
*            worktime_14(20) VALUE '19:15~20:15',
*            worktime_15(20) VALUE '20:15~21:15',
*            worktime_16(20) VALUE '21:15~22:15',
*            worktime_17(20) VALUE '22:15~23:15',
*            worktime_18(20) VALUE '23:15~00:00',
*            worktime_19(20) VALUE '00:00~01:00',
*            worktime_20(20) VALUE '01:00~02:00',
*            worktime_21(20) VALUE '02:00~03:00',
*            worktime_22(20) VALUE '03:00~04:00',
*            worktime_23(20) VALUE '04:00~05:00',
*            worktime_24(20) VALUE '05:00~06:30',
*            worktime_25(20) VALUE '1st Total',
*            worktime_26(20) VALUE '2nd Total'.
*
*  LOOP AT gt_zprdqtyhour.
*    zprdqtyhour = gt_zprdqtyhour.
*    COLLECT zprdqtyhour.
*    CLEAR zprdqtyhour.
*  ENDLOOP.
*
*  SORT zprdqtyhour BY worktime.
*  DATA zprdqtyhour1 LIKE zprdqtyhour OCCURS 12 WITH HEADER LINE.
*  DATA zprdqtyhour2 LIKE zprdqtyhour OCCURS 12 WITH HEADER LINE.
*
*  LOOP AT zprdqtyhour FROM 1 TO 12.
*    zprdqtyhour1 = zprdqtyhour.
*    APPEND zprdqtyhour1.
*  ENDLOOP.
*
*  LOOP AT zprdqtyhour FROM 13.
*    zprdqtyhour2 = zprdqtyhour.
*    APPEND zprdqtyhour2.
*  ENDLOOP.
*
*  LOOP AT zprdqtyhour1.
*    AT LAST.
*      SUM.
*      zprdqtyhour = zprdqtyhour1.
*      zprdqtyhour-worktime = 'WORKTIME_25'.
*      APPEND zprdqtyhour.
*    ENDAT.
*  ENDLOOP.
*
*  LOOP AT zprdqtyhour2.
*    AT LAST.
*      SUM.
*      zprdqtyhour = zprdqtyhour2.
*      zprdqtyhour-worktime = 'WORKTIME_26'.
*      APPEND zprdqtyhour.
*    ENDAT.
*  ENDLOOP.
*
*  FIELD-SYMBOLS : <_txt>.
*
*  LOOP AT zprdqtyhour.
*    ASSIGN (zprdqtyhour-worktime) TO <_txt>.
*    zprdqtyhour-worktime = <_txt>.
*    MODIFY zprdqtyhour INDEX sy-tabix.
*  ENDLOOP.
*...}End
ENDFUNCTION.

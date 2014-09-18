*----------------------------------------------------------------------*
*   INCLUDE ZRMMGM06_COIL_REQ_SUMF01                                   *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
* set date
  PERFORM set_date.
* set period date
  PERFORM get_period_6month_week.
* select basic data
  PERFORM select_basicdata.
* modify data
  IF     p_perkz  EQ  'W'.
    PERFORM modify_data_week.
  ELSEIF p_perkz  EQ  'M'.
    PERFORM modify_data_month.
  ELSE.
    EXIT.
  ENDIF.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  set_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_date.

  CLEAR : w_date_f, w_date_e.

  CASE p_perkz.
    WHEN 'W'.   "weeks
* return first day on week
      CALL FUNCTION 'DATE_GET_WEEK'
           EXPORTING
                date         = p_date
           IMPORTING
                week         = w_week
           EXCEPTIONS
                date_invalid = 1
                OTHERS       = 2.
      CALL FUNCTION 'WEEK_GET_FIRST_DAY'
           EXPORTING
                week         = w_week
           IMPORTING
                date         = w_date_f
           EXCEPTIONS
                week_invalid = 1
                OTHERS       = 2.
      CHECK sy-subrc EQ 0.
* get last day of the week after 5 weeks
      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
           EXPORTING
                date      = w_date_f
                days      = '42'  "Changed by Hakchin 35 --> 42
                months    = '0'
                signum    = '+'
                years     = '0'
           IMPORTING
                calc_date = w_date_e.

    WHEN 'M'.   "months
* return first day on month
      CALL FUNCTION 'CK_F_GET_FIRST_DAY_OF_DATE'
           EXPORTING
                p_date              = p_date
                p_periv             = 'K1'
           IMPORTING
                first_day_in_period = w_date_f
           EXCEPTIONS
                error_occured       = 1
                OTHERS              = 2.
      CHECK sy-subrc EQ 0.
* get last day of the month after 5 months
      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
           EXPORTING
                date      = w_date_f
                days      = '0'
                months    = '6'  "Changed by Hakchin 5 --> 6
                signum    = '+'
                years     = '0'
           IMPORTING
                calc_date = w_date_e.

    WHEN OTHERS.
  ENDCASE.

*/Begin of Added by Hakchin(20040414)
  w_date_e = w_date_e - 1.
*/End of Added by Hakchin(20040414)

ENDFORM.                    " set_date
*&---------------------------------------------------------------------*
*&      Form  get_period_6month_week
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_period_6month_week.

  DATA : lw_index TYPE sy-index. CLEAR : lw_index.
  CLEAR: w_week00, w_week01, w_week02,
         w_week03, w_week04, w_week05,
         w_mon00,  w_mon01,  w_mon02,
         w_mon03,  w_mon04,  w_mon05.

  DATA: lv_date LIKE w_date_f.
  lv_date = w_date_f.

* weeks

  IF    p_perkz  EQ 'W'.
    DO 6 TIMES.
      ADD 1 TO lw_index.
      CLEAR w_week.
      CALL FUNCTION 'DATE_GET_WEEK'
           EXPORTING
                date         = lv_date
           IMPORTING
                week         = w_week
           EXCEPTIONS
                date_invalid = 1
                OTHERS       = 2.
      CASE lw_index.
        WHEN 1.  w_week00  =  w_week.
        WHEN 2.  w_week01  =  w_week.
        WHEN 3.  w_week02  =  w_week.
        WHEN 4.  w_week03  =  w_week.
        WHEN 5.  w_week04  =  w_week.
        WHEN 6.  w_week05  =  w_week.
      ENDCASE.

      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
           EXPORTING
                date      = lv_date
                days      = '7'
                months    = '0'
                signum    = '+'
                years     = '0'
           IMPORTING
                calc_date = lv_date.
    ENDDO.
* months
  ELSEIF p_perkz  EQ 'M'.
    DO 6 TIMES.
      ADD 1 TO lw_index.
      CLEAR w_mon.
      CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
           EXPORTING
                day_in            = lv_date
           IMPORTING
                last_day_of_month = w_mon
           EXCEPTIONS
                day_in_no_date    = 1
                OTHERS            = 2.
      CASE lw_index.
        WHEN 1.  w_mon00  =  w_mon.
        WHEN 2.  w_mon01  =  w_mon.
        WHEN 3.  w_mon02  =  w_mon.
        WHEN 4.  w_mon03  =  w_mon.
        WHEN 5.  w_mon04  =  w_mon.
        WHEN 6.  w_mon05  =  w_mon.
      ENDCASE.
      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
           EXPORTING
                date      = lv_date
                days      = '0'
                months    = '1'
                signum    = '+'
                years     = '0'
           IMPORTING
                calc_date = lv_date.
    ENDDO.
* others
  ELSE.
    EXIT.
  ENDIF.

ENDFORM.                    " get_period_6month_week
*&---------------------------------------------------------------------*
*&      Form  select_basicdata
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_basicdata.

  CLEAR : it_plan[], it_plan,
          it_pr[],   it_pr,
          it_po[],   it_po.

* plan order
  SELECT matnr  gsmng  meins  pedtr
         INTO TABLE it_plan
        FROM plaf
       WHERE dispo  EQ      'P01'
       AND   pedtr  BETWEEN w_date_f  and w_date_e
       and   MATNR  ne       ''.
* open pr
  SELECT a~matnr
         a~menge
         a~bsmng
         a~meins
         a~lfdat
    INTO TABLE it_pr
    FROM eban AS a
      INNER JOIN marc AS b
      ON a~mandt  EQ  b~mandt AND
         a~matnr  EQ  b~matnr AND
         a~werks  EQ  b~werks
    WHERE a~lfdat BETWEEN w_date_f and w_date_e and
*/Begin of Changed by Hakchin(20040318)
          B~DISPO eq  'P01'                     AND
*/End of Changed by Hakchin(20040318)
          a~matnr NE ''.
* open po
  SELECT b~matnr
         c~menge
         c~wemng
         b~meins
         c~eindt
         INTO TABLE it_po
         FROM ekko AS a INNER JOIN ekpo AS b
           ON a~mandt  EQ  b~mandt
          AND a~ebeln  EQ  b~ebeln
            INNER JOIN eket AS c
               ON b~mandt  EQ  c~mandt
              AND b~ebeln  EQ  c~ebeln
              AND b~ebelp  EQ  c~ebelp
                INNER JOIN marc AS d
                   ON b~mandt  EQ  d~mandt
                  AND b~matnr  EQ  d~matnr
                  AND b~werks  EQ  d~werks
        WHERE b~loekz  EQ      ''
        AND   c~eindt  BETWEEN w_date_f  and w_date_e
*/Begin of Changed by Hakchin(20040318)
        and   D~DISPO  eq      'P01'
*/End of Changed by Hakchin(20040318)
        AND   b~matnr  NE      ''
        AND   c~menge  <>      c~wemng.

  SORT it_plan BY matnr.
  SORT it_pr   BY matnr.
  SORT it_po   BY matnr.

ENDFORM.                    " select_basicdata
*&---------------------------------------------------------------------*
*&      Form  modify_data_week
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_data_week.

  DATA : lw_menge  LIKE  mseg-menge.
  CLEAR: it_list_p[], it_list_p,
         it_list_r[], it_list_r,
         it_list_o[], it_list_o,
         it_list[],   it_list.

* plan order
  LOOP AT it_plan.
    it_list_p-matnr  =  it_plan-matnr.
    it_list_p-meins  =  it_plan-meins.

    CLEAR w_week.
    CALL FUNCTION 'DATE_GET_WEEK'
         EXPORTING
              date         = it_plan-pedtr
         IMPORTING
              week         = w_week
         EXCEPTIONS
              date_invalid = 1
              OTHERS       = 2.
    CASE  w_week.
      WHEN w_week00.
        it_list_p-menge00  =  it_list_p-menge00 + it_plan-gsmng.
      WHEN w_week01.
        it_list_p-menge01  =  it_list_p-menge01 + it_plan-gsmng.
      WHEN w_week02.
        it_list_p-menge02  =  it_list_p-menge02 + it_plan-gsmng.
      WHEN w_week03.
        it_list_p-menge03  =  it_list_p-menge03 + it_plan-gsmng.
      WHEN w_week04.
        it_list_p-menge04  =  it_list_p-menge04 + it_plan-gsmng.
      WHEN w_week05.
        it_list_p-menge05  =  it_list_p-menge05 + it_plan-gsmng.
    ENDCASE.
    it_list_p-menget  =
      it_list_p-menge00 + it_list_p-menge01 + it_list_p-menge02 +
      it_list_p-menge03 + it_list_p-menge04 + it_list_p-menge05.
    COLLECT it_list_p. CLEAR it_list_p.
  ENDLOOP.


* open pr
  LOOP AT it_pr.
    it_list_r-matnr  =  it_pr-matnr.
    it_list_r-meins  =  it_pr-meins.

    CLEAR lw_menge.
    lw_menge  =  it_pr-menge - it_pr-bsmng.
    IF lw_menge  > 0.
      CLEAR w_week.
      CALL FUNCTION 'DATE_GET_WEEK'
           EXPORTING
                date         = it_pr-lfdat
           IMPORTING
                week         = w_week
           EXCEPTIONS
                date_invalid = 1
                OTHERS       = 2.
      CASE  w_week.
        WHEN w_week00.
          it_list_r-menge00  =  it_list_r-menge00 + lw_menge.
        WHEN w_week01.
          it_list_r-menge01  =  it_list_r-menge01 + lw_menge.
        WHEN w_week02.
          it_list_r-menge02  =  it_list_r-menge02 + lw_menge.
        WHEN w_week03.
          it_list_r-menge03  =  it_list_r-menge03 + lw_menge.
        WHEN w_week04.
          it_list_r-menge04  =  it_list_r-menge04 + lw_menge.
        WHEN w_week05.
          it_list_r-menge05  =  it_list_r-menge05 + lw_menge.
      ENDCASE.
      it_list_r-menget  =
           it_list_r-menge00 + it_list_r-menge01 + it_list_r-menge02 +
           it_list_r-menge03 + it_list_r-menge04 + it_list_r-menge05.
      COLLECT it_list_r. CLEAR it_list_r.
    ENDIF.
  ENDLOOP.


* open po
  LOOP AT it_po.
    it_list_o-matnr  =  it_po-matnr.
    it_list_o-meins  =  it_po-meins.

    CLEAR lw_menge.
    lw_menge  =  it_po-menge - it_po-wemng.
    CLEAR w_week.
    CALL FUNCTION 'DATE_GET_WEEK'
         EXPORTING
              date         = it_po-eindt
         IMPORTING
              week         = w_week
         EXCEPTIONS
              date_invalid = 1
              OTHERS       = 2.
    CASE  w_week.
      WHEN w_week00.
        it_list_o-menge00  =  it_list_o-menge00 + lw_menge.
      WHEN w_week01.
        it_list_o-menge01  =  it_list_o-menge01 + lw_menge.
      WHEN w_week02.
        it_list_o-menge02  =  it_list_o-menge02 + lw_menge.
      WHEN w_week03.
        it_list_o-menge03  =  it_list_o-menge03 + lw_menge.
      WHEN w_week04.
        it_list_o-menge04  =  it_list_o-menge04 + lw_menge.
      WHEN w_week05.
        it_list_o-menge05  =  it_list_o-menge05 + lw_menge.
    ENDCASE.
    it_list_o-menget  =
        it_list_o-menge00 + it_list_o-menge01 + it_list_o-menge02 +
        it_list_o-menge03 + it_list_o-menge04 + it_list_o-menge05.
    COLLECT it_list_o. CLEAR it_list_o.
  ENDLOOP.

*/Begin of Commented By Hakchin(20040413)
*  LOOP AT it_list_p.
*    MOVE-CORRESPONDING it_list_p TO it_list.
*
*    READ TABLE it_list_r WITH KEY matnr  =  it_list_p-matnr.
*    IF sy-subrc EQ 0.
*      it_list-menge00 = it_list-menge00 + it_list_r-menge00.
*      it_list-menge01 = it_list-menge01 + it_list_r-menge01.
*      it_list-menge02 = it_list-menge02 + it_list_r-menge02.
*      it_list-menge03 = it_list-menge03 + it_list_r-menge03.
*      it_list-menge04 = it_list-menge04 + it_list_r-menge04.
*      it_list-menge05 = it_list-menge05 + it_list_r-menge05.
*      it_list-menget  = it_list-menget  + it_list_r-menget.
*    ENDIF.
*
*    READ TABLE it_list_o WITH KEY matnr  =  it_list_p-matnr.
*    IF sy-subrc EQ 0.
*      it_list-menge00 = it_list-menge00 + it_list_o-menge00.
*      it_list-menge01 = it_list-menge01 + it_list_o-menge01.
*      it_list-menge02 = it_list-menge02 + it_list_o-menge02.
*      it_list-menge03 = it_list-menge03 + it_list_o-menge03.
*      it_list-menge04 = it_list-menge04 + it_list_o-menge04.
*      it_list-menge05 = it_list-menge05 + it_list_o-menge05.
*      it_list-menget  = it_list-menget  + it_list_o-menget.
*    ENDIF.
*
*    COLLECT it_list. CLEAR it_list.
*  ENDLOOP.
*/End of Commented By Hakchin(20040413)

*/Begin of Added by Hakchin(20040413)
  APPEND LINES OF it_list_p TO it_list.
  APPEND LINES OF it_list_r TO it_list.
  APPEND LINES OF it_list_o TO it_list.

  DELETE it_list WHERE menget IS initial.

*Group by matnr in it_list.
  PERFORM itable_group_by_matnr
               USING it_list[].
*/End of Added by Hakchin(20040413)
ENDFORM.                    " modify_data_week
*&---------------------------------------------------------------------*
*&      Form  modify_data_month
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_data_month.
  DATA : lw_menge  LIKE  mseg-menge.
  CLEAR: it_list_p[], it_list_p,
         it_list_r[], it_list_r,
         it_list_o[], it_list_o,
         it_list[],   it_list.

* plan order
  LOOP AT it_plan.
    it_list_p-matnr  =  it_plan-matnr.
    it_list_p-meins  =  it_plan-meins.

    CLEAR w_mon.
    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
         EXPORTING
              day_in            = it_plan-pedtr
         IMPORTING
              last_day_of_month = w_mon
         EXCEPTIONS
              day_in_no_date    = 1
              OTHERS            = 2.
    CASE  w_mon.
      WHEN w_mon00.
        it_list_p-menge00  =  it_list_p-menge00 + it_plan-gsmng.
      WHEN w_mon01.
        it_list_p-menge01  =  it_list_p-menge01 + it_plan-gsmng.
      WHEN w_mon02.
        it_list_p-menge02  =  it_list_p-menge02 + it_plan-gsmng.
      WHEN w_mon03.
        it_list_p-menge03  =  it_list_p-menge03 + it_plan-gsmng.
      WHEN w_mon04.
        it_list_p-menge04  =  it_list_p-menge04 + it_plan-gsmng.
      WHEN w_mon05.
        it_list_p-menge05  =  it_list_p-menge05 + it_plan-gsmng.
    ENDCASE.
    it_list_p-menget  =
          it_list_p-menge00 + it_list_p-menge01 + it_list_p-menge02 +
          it_list_p-menge03 + it_list_p-menge04 + it_list_p-menge05.
    COLLECT it_list_p. CLEAR it_list_p.
  ENDLOOP.


* open pr
  LOOP AT it_pr.
    it_list_r-matnr  =  it_pr-matnr.
    it_list_r-meins  =  it_pr-meins.

    CLEAR lw_menge.
    lw_menge  =  it_pr-menge - it_pr-bsmng.
    IF lw_menge  > 0.
      CLEAR w_mon.
      CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
           EXPORTING
                day_in            = it_pr-lfdat
           IMPORTING
                last_day_of_month = w_mon
           EXCEPTIONS
                day_in_no_date    = 1
                OTHERS            = 2.
      CASE  w_mon.
        WHEN w_mon00.
          it_list_r-menge00  =  it_list_r-menge00 + lw_menge.
        WHEN w_mon01.
          it_list_r-menge01  =  it_list_r-menge01 + lw_menge.
        WHEN w_mon02.
          it_list_r-menge02  =  it_list_r-menge02 + lw_menge.
        WHEN w_mon03.
          it_list_r-menge03  =  it_list_r-menge03 + lw_menge.
        WHEN w_mon04.
          it_list_r-menge04  =  it_list_r-menge04 + lw_menge.
        WHEN w_mon05.
          it_list_r-menge05  =  it_list_r-menge05 + lw_menge.
      ENDCASE.
    ENDIF.
    it_list_r-menget  =
          it_list_r-menge00 + it_list_r-menge01 + it_list_r-menge02 +
          it_list_r-menge03 + it_list_r-menge04 + it_list_r-menge05.
    COLLECT it_list_r. CLEAR it_list_r.
  ENDLOOP.



* open po
  LOOP AT it_po.
    it_list_o-matnr  =  it_po-matnr.
    it_list_o-meins  =  it_po-meins.

    CLEAR lw_menge.
    lw_menge  =  it_po-menge - it_po-wemng.
    CLEAR w_mon.
    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
         EXPORTING
              day_in            = it_po-eindt
         IMPORTING
              last_day_of_month = w_mon
         EXCEPTIONS
              day_in_no_date    = 1
              OTHERS            = 2.
    CASE  w_mon.
      WHEN w_mon00.
        it_list_o-menge00  =  it_list_o-menge00 + lw_menge.
      WHEN w_mon01.
        it_list_o-menge01  =  it_list_o-menge01 + lw_menge.
      WHEN w_mon02.
        it_list_o-menge02  =  it_list_o-menge02 + lw_menge.
      WHEN w_mon03.
        it_list_o-menge03  =  it_list_o-menge03 + lw_menge.
      WHEN w_mon04.
        it_list_o-menge04  =  it_list_o-menge04 + lw_menge.
      WHEN w_mon05.
        it_list_o-menge05  =  it_list_o-menge05 + lw_menge.
    ENDCASE.
    it_list_o-menget  =
          it_list_o-menge00 + it_list_o-menge01 + it_list_o-menge02 +
          it_list_o-menge03 + it_list_o-menge04 + it_list_o-menge05.
    COLLECT it_list_o. CLEAR it_list_o.
  ENDLOOP.

*/Begin of Commented By Hakchin(20040413)
*  LOOP AT it_list_p.
*    MOVE-CORRESPONDING it_list_p TO it_list.
*
*    READ TABLE it_list_r WITH KEY matnr  =  it_list_p-matnr.
*    IF sy-subrc EQ 0.
*      it_list-menge00 = it_list-menge00 + it_list_r-menge00.
*      it_list-menge01 = it_list-menge01 + it_list_r-menge01.
*      it_list-menge02 = it_list-menge02 + it_list_r-menge02.
*      it_list-menge03 = it_list-menge03 + it_list_r-menge03.
*      it_list-menge04 = it_list-menge04 + it_list_r-menge04.
*      it_list-menge05 = it_list-menge05 + it_list_r-menge05.
*      it_list-menget  = it_list-menget  + it_list_r-menget.
*    ENDIF.
*
*    READ TABLE it_list_o WITH KEY matnr  =  it_list_p-matnr.
*    IF sy-subrc EQ 0.
*      it_list-menge00 = it_list-menge00 + it_list_o-menge00.
*      it_list-menge01 = it_list-menge01 + it_list_o-menge01.
*      it_list-menge02 = it_list-menge02 + it_list_o-menge02.
*      it_list-menge03 = it_list-menge03 + it_list_o-menge03.
*      it_list-menge04 = it_list-menge04 + it_list_o-menge04.
*      it_list-menge05 = it_list-menge05 + it_list_o-menge05.
*      it_list-menget  = it_list-menget  + it_list_o-menget.
*    ENDIF.
*
*    COLLECT it_list. CLEAR it_list.
*  ENDLOOP.
*/End of Commented By Hakchin(20040413)

*/Begin of Added by Hakchin(20040413)
  APPEND LINES OF it_list_p TO it_list.
  APPEND LINES OF it_list_r TO it_list.
  APPEND LINES OF it_list_o TO it_list.

  DELETE it_list WHERE menget IS initial.

*Group by matnr in it_list.
  PERFORM itable_group_by_matnr
               USING it_list[].
*/End of Added by Hakchin(20040413)
ENDFORM.                    " modify_data_month
*&---------------------------------------------------------------------*
*&      Form  alv_field_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_field_build.
  w_repid = sy-repid.
  CLEAR : it_fieldcat[], wa_events[],
          it_list_top_of_page[]     .
* set fields
  PERFORM fieldcat_init.
* set event
  PERFORM eventtab_build USING wa_events[].
* set list heading
  PERFORM comment_build  USING it_list_top_of_page[].
ENDFORM.                    " alv_field_build
*&---------------------------------------------------------------------*
*&      Form  fieldcat_init
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fieldcat_init.

  build_fieldcat  'MATNR'  'MATNR'  'X'  space  space  'Component'
                  'Component'  'Component'  '18'.
  build_fieldcat  'MEINS'  'MEINS'  ''  space  space  'UOM'
                  'UOM'  'UOM'  '3'.
  build_fieldcat  'MENGE00'  'MEINS'  ''  ''  ''  'M_W+0'
                  'M'  'M'  '13'.
  build_fieldcat  'MENGE01'  'MEINS'  ''  ''  ''  'M_W+1'
                  'M+1'  'M+1'  '13'.
  build_fieldcat  'MENGE02'  'MEINS'  ''  ''  ''  'M_W+2'
                  'M+2'  'M+2'  '13'.
  build_fieldcat  'MENGE03'  'MEINS'  ''  ''  ''  'M_W+3'
                  'M+3'  'M+3'  '13'.
  build_fieldcat  'MENGE04'  'MEINS'  ''  ''  ''  'M_W+4'
                  'M+4'  'M+4'  '13'.
  build_fieldcat  'MENGE05'  'MEINS'  ''  ''  ''  'M_W+5'
                  'M+5'  'M+5'  '13'.
  build_fieldcat  'MENGET'   'MEINS'  ''  ''  ''  'Total'
                  'Total'  'Total'  '13'.

ENDFORM.                    " fieldcat_init
*&---------------------------------------------------------------------*
*&      Form  eventtab_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_EVENTS[]  text
*----------------------------------------------------------------------*
FORM eventtab_build
                   USING e03_lt_events TYPE slis_t_event.

  DATA: ls_event TYPE slis_alv_event.
*
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            i_list_type = 0
       IMPORTING
            et_events   = e03_lt_events.
  READ TABLE e03_lt_events WITH KEY name =  slis_ev_top_of_page
                           INTO ls_event.
  IF sy-subrc = 0.
    MOVE c_formname_top_of_page TO ls_event-form.
    APPEND ls_event TO e03_lt_events.
  ENDIF.

ENDFORM.                    " EVENTTAB_BUILD
*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_LIST_TOP_OF_PAGE[]  text
*----------------------------------------------------------------------*
FORM comment_build
               USING lt_top_of_page TYPE slis_t_listheader.

  DATA: ls_line  TYPE slis_listheader.
  DATA: lw_info  TYPE slis_entry,
        lw_name1 LIKE t001w-name1,
        lw_maktx LIKE makt-maktx,
        lw_ekotx LIKE t024e-ekotx,
        lw_eknam LIKE t024-eknam.
*/Begin of Added by Hakchin(20040408)
* title
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  CONCATENATE text-h01 'by' p_perkz
            INTO ls_line-info
            SEPARATED BY space.
  APPEND ls_line TO lt_top_of_page.
  APPEND INITIAL LINE TO lt_top_of_page.
  ls_line-typ = 'S'.
  CONCATENATE 'Month/Week:'
              p_perkz
          INTO ls_line-info
          SEPARATED BY space.
  APPEND ls_line TO lt_top_of_page.

  ls_line-typ = 'S'.
  CONCATENATE 'Requirement Date:'
              w_reqdate
          INTO ls_line-info
          SEPARATED BY space.
  APPEND ls_line TO lt_top_of_page.

*/End of Added by Hakchin(20040408)


ENDFORM.                    " COMMENT_BUILD
*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM top_of_page.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
*           i_logo             = 'HTMLCNTL_TESTHTM2_SAPLOGO'
*           I_LOGO             = 'ENJOYSAP_LOGO'
            it_list_commentary = it_list_top_of_page.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  display_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_list.
  CHECK NOT it_list[] IS INITIAL.
  SORT it_list BY matnr.

*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
   EXPORTING
*   I_INTERFACE_CHECK              = ' '
*   I_BYPASSING_BUFFER             =
*   I_BUFFER_ACTIVE                = ' '
     i_callback_program             = w_repid
     i_callback_pf_status_set       = 'PS_TB'
*     i_callback_user_command        = 'USER_COMMAND'
*   I_STRUCTURE_NAME               =
*   IS_LAYOUT                      =
     it_fieldcat                    = it_fieldcat[]
*   IT_EXCLUDING                   =
*   IT_SPECIAL_GROUPS              =
*   IT_SORT                        = it_sort[]
*   IT_FILTER                      =
*   IS_SEL_HIDE                    =
*   I_DEFAULT                      = 'X'
   i_save                         = 'A'
*   IS_VARIANT                     =
     it_events                      = wa_events[]
*   IT_EVENT_EXIT                  =
*   IS_PRINT                       =
*   IS_REPREP_ID                   =
*   I_SCREEN_START_COLUMN          = 0
*   I_SCREEN_START_LINE            = 0
*   I_SCREEN_END_COLUMN            = 0
*   I_SCREEN_END_LINE              = 0
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER        =
*   ES_EXIT_CAUSED_BY_USER         =
    TABLES
      t_outtab                       = it_list
   EXCEPTIONS
     program_error                  = 1
     OTHERS                         = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " display_list

*---------------------------------------------------------------------*
*       FORM ps_tb                                                    *
*---------------------------------------------------------------------*
FORM ps_tb USING rt_extab TYPE slis_t_extab.
*  CLEAR: rt_extab[], rt_extab.
  DATA: lv_numbering       TYPE i.
  DATA: lv_numbering_c(10) TYPE c.
  DESCRIBE TABLE it_list LINES lv_numbering.
  lv_numbering_c = lv_numbering.

* Instanciate PF-STATUS & TITLEBAR.
  DATA: lv_title(80).         " Title
  CONCATENATE 'RMMGM06 MIP Steel Requirement Summary '
              'Select Entries'
              lv_numbering_c
    INTO lv_title
    SEPARATED BY space.
  SET TITLEBAR 'TB' WITH lv_title.

*  SET PF-STATUS 'PS' EXCLUDING 'TRAN'.  "Transfer
  SET PF-STATUS 'PS'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  itable_group_by_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_LIST[]  text
*----------------------------------------------------------------------*
FORM itable_group_by_matnr
      USING iet_itable LIKE it_list[].
  DATA: lv_serno TYPE i.
*/For Summary WA & Itab
  FIELD-SYMBOLS: <lf_summary> LIKE LINE OF iet_itable.
  DATA: lt_summary LIKE TABLE OF <lf_summary>.
*/Field Symbol for iet_itable
  FIELD-SYMBOLS: <lf_itable> LIKE LINE OF iet_itable.

*/1.Make unique line by MATNR
  lt_summary = iet_itable.
  SORT lt_summary BY matnr.
  DELETE ADJACENT DUPLICATES FROM lt_summary
                   COMPARING matnr.

*/2.Grouping by MATNR
  SORT lt_summary BY matnr.
  LOOP AT lt_summary ASSIGNING <lf_summary>.
    CLEAR: lv_serno,
           <lf_summary>-menge00,
           <lf_summary>-menge01,
           <lf_summary>-menge02,
           <lf_summary>-menge03,
           <lf_summary>-menge04,
           <lf_summary>-menge05,
           <lf_summary>-menget.
    LOOP AT iet_itable ASSIGNING <lf_itable>
                  WHERE matnr = <lf_summary>-matnr.
      lv_serno = lv_serno + 1.  "Occurrence by MATNR

      <lf_summary>-menge00 =
            <lf_summary>-menge00 + <lf_itable>-menge00.
      <lf_summary>-menge01 =
            <lf_summary>-menge01 + <lf_itable>-menge01.
      <lf_summary>-menge02 =
            <lf_summary>-menge02 + <lf_itable>-menge02.
      <lf_summary>-menge03 =
            <lf_summary>-menge03 + <lf_itable>-menge03.
      <lf_summary>-menge04 =
            <lf_summary>-menge04 + <lf_itable>-menge04.
      <lf_summary>-menge05 =
            <lf_summary>-menge05 + <lf_itable>-menge05.
      <lf_summary>-menget  =
            <lf_summary>-menget  + <lf_itable>-menget.

    ENDLOOP.
  ENDLOOP.
  iet_itable = lt_summary.
ENDFORM.

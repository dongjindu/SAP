*----------------------------------------------------------------------*
*   INCLUDE ZIMMGM15I_6013F01
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
  CASE p_perkz.    "Period indicator(Month/Week)
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

* get day after 5 weeks
      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
           EXPORTING
                date      = w_date_f
                days      = '35'
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
* get day after 5 months
      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
           EXPORTING
                date      = w_date_f
                days      = '0'
                months    = '5'
                signum    = '+'
                years     = '0'
           IMPORTING
                calc_date = w_date_e.
    WHEN OTHERS.
      EXIT.
  ENDCASE.
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

* weeks
  IF    p_perkz  EQ 'W'.
    DO 6 TIMES.
      ADD 1 TO lw_index.
      CLEAR w_week.
      CALL FUNCTION 'DATE_GET_WEEK'
           EXPORTING
                date         = p_date
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
                date      = p_date
                days      = '7'
                months    = '0'
                signum    = '+'
                years     = '0'
           IMPORTING
                calc_date = p_date.
    ENDDO.
* months
  ELSEIF p_perkz  EQ 'M'.
    DO 6 TIMES.
      ADD 1 TO lw_index.
      CLEAR w_mon.
      CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
           EXPORTING
                day_in            = p_date
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
                date      = p_date
                days      = '0'
                months    = '1'
                signum    = '+'
                years     = '0'
           IMPORTING
                calc_date = p_date.
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
  SELECT matnr
         gsmng
         meins
         pedtr
         plwrk AS plant
    INTO CORRESPONDING FIELDS OF TABLE it_plan
    FROM plaf
    WHERE dispo EQ 'P01' AND
          pedtr BETWEEN w_date_f  and w_date_e and
          MATNR ne ''.
* open pr
  SELECT a~matnr
         a~menge
         a~bsmng
         a~meins
         a~lfdat
         a~werks AS plant
    INTO CORRESPONDING FIELDS OF TABLE it_pr
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
         b~werks AS plant
         INTO CORRESPONDING FIELDS OF TABLE it_po
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
    it_list_p-plant  =  it_plan-plant.
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
    it_list_r-plant  =  it_pr-plant.
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
    it_list_o-plant  =  it_po-plant.
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


  LOOP AT it_list_p.
    MOVE-CORRESPONDING it_list_p TO it_list.

    READ TABLE it_list_r WITH KEY matnr  =  it_list_p-matnr.
    IF sy-subrc EQ 0.
      it_list-menge00 = it_list-menge00 + it_list_r-menge00.
      it_list-menge01 = it_list-menge01 + it_list_r-menge01.
      it_list-menge02 = it_list-menge02 + it_list_r-menge02.
      it_list-menge03 = it_list-menge03 + it_list_r-menge03.
      it_list-menge04 = it_list-menge04 + it_list_r-menge04.
      it_list-menge05 = it_list-menge05 + it_list_r-menge05.
      it_list-menget  = it_list-menget  + it_list_r-menget.
    ENDIF.

    READ TABLE it_list_o WITH KEY matnr  =  it_list_p-matnr.
    IF sy-subrc EQ 0.
      it_list-menge00 = it_list-menge00 + it_list_o-menge00.
      it_list-menge01 = it_list-menge01 + it_list_o-menge01.
      it_list-menge02 = it_list-menge02 + it_list_o-menge02.
      it_list-menge03 = it_list-menge03 + it_list_o-menge03.
      it_list-menge04 = it_list-menge04 + it_list_o-menge04.
      it_list-menge05 = it_list-menge05 + it_list_o-menge05.
      it_list-menget  = it_list-menget  + it_list_o-menget.
    ENDIF.

    COLLECT it_list. CLEAR it_list.
  ENDLOOP.

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
    it_list_p-plant  =  it_plan-plant.
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
    it_list_r-plant  =  it_pr-plant.
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
    it_list_o-plant  =  it_po-plant.
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


  LOOP AT it_list_p.
    MOVE-CORRESPONDING it_list_p TO it_list.

    READ TABLE it_list_r WITH KEY matnr  =  it_list_p-matnr.
    IF sy-subrc EQ 0.
      it_list-menge00 = it_list-menge00 + it_list_r-menge00.
      it_list-menge01 = it_list-menge01 + it_list_r-menge01.
      it_list-menge02 = it_list-menge02 + it_list_r-menge02.
      it_list-menge03 = it_list-menge03 + it_list_r-menge03.
      it_list-menge04 = it_list-menge04 + it_list_r-menge04.
      it_list-menge05 = it_list-menge05 + it_list_r-menge05.
      it_list-menget  = it_list-menget  + it_list_r-menget.
    ENDIF.

    READ TABLE it_list_o WITH KEY matnr  =  it_list_p-matnr.
    IF sy-subrc EQ 0.
      it_list-menge00 = it_list-menge00 + it_list_o-menge00.
      it_list-menge01 = it_list-menge01 + it_list_o-menge01.
      it_list-menge02 = it_list-menge02 + it_list_o-menge02.
      it_list-menge03 = it_list-menge03 + it_list_o-menge03.
      it_list-menge04 = it_list-menge04 + it_list_o-menge04.
      it_list-menge05 = it_list-menge05 + it_list_o-menge05.
      it_list-menget  = it_list-menget  + it_list_o-menget.
    ENDIF.

    COLLECT it_list. CLEAR it_list.
  ENDLOOP.

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

  PERFORM catalog
           TABLES it_fieldcat
           USING
                  'X'             'ZDOCNO'     'IT_ZTMM_6013_01'
                  'ZTMM_6013_01'  'ZDOCNO'     ''
                  ''              ''           ''
                  ''              ''           ''
                  ''              ''           ''
                  ''              ''           ''
                  'App.DocNo.'.

  PERFORM catalog
           TABLES it_fieldcat
           USING
                  'X'             'LOGNO_H'    'IT_ZTMM_6013_01'
                  'ZTMM_6013_01'  'LOGNO_H'    ''
                  ''              ''           ''
                  ''              ''           ''
                  ''              ''           ''
                  ''              ''           ''
                  'Log no.'.

  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'MARA_MATNR' 'IT_ZTMM_6013_01'
                  'ZTMM_6013_01'  'MARA_MATNR' ''
                  ''              ''           '18'
                  ''              ''           ''
                  ''              ''           ''
                  ''              ''           ''
                  'Component'.

  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'DOCTYPE'    'IT_ZTMM_6013_01'
                  'ZTMM_6013_01'  'DOCTYPE'    ''
                  ''              ''           ''
                  ''              ''           ''
                  ''              ''           ''
                  ''              ''           ''
                  'Doc. Type'.

  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'MATL'         'IT_ZTMM_6013_01'
                  'ZTMM_6013_01'  'MATL'         ''
                  ''              ''             ''
                  ''              ''             ''
                  ''              ''             ''
                  ''              ''             ''
                  'Mat.Property'.

  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'COAT_QTY'     'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'COAT_QTY'     ''
                  ''              ''             ''
                  ''              ''             ''
                  ''              ''             ''
                  ''              ''             ''
                  'Coating'.

  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'THICK'        'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'THICK'        ''
                  ''              ''             ''
                  ''              ''             ''
                  ''              ''             ''
                  ''              ''             ''
                  'Thick'.


  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'WIDTH'        'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'WIDTH'        ''
                  ''              ''             ''
                  ''              ''             ''
                  ''              ''             ''
                  ''              ''             ''
                  'Width'.


  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'LENGTH'       'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'LENGTH'       ''
                  ''              ''             ''
                  ''              ''             ''
                  ''              ''             ''
                  ''              ''             ''
                  'Length'.


  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'IN_OUT'        'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'IN_OUT'        ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  'Inner/Outside'.


  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'M'             'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'M'             ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  'M'.


  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'CM'            'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'CM'            ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  'M-Confirmed QTY'.


  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'M_W'           'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'M_W'           ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  'M_W'.


  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'M_W1'          'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'M_W1'          ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  'M W+1'.


  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'M_W2'          'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'M_W2'          ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  'M W+2'.


  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'M_W3'          'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'M_W3'          ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  'M W+3'.


  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'M_W4'          'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'M_W4'          ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  'M W+4'.


  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'M1'            'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'M1'            ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  'M+1'.


  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'CM1'           'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'CM1'           ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  'M+1:Confirmed QTY'.


  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'M2'            'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'M2'            ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  'M+2'.


  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'CM2'           'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'CM2'           ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  'M+2:Confirmed QTY'.


  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'M3'            'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'M3'            ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  'M+3'.


  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'CM3'           'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'CM3'           ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  'M+3:Confirmed QTY'.


  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'M4'            'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'M4'            ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  'M+4'.


  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'CM4'           'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'CM4'           ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  'M+4:Confirmed QTY'.


  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'M5'            'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'M5'            ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  'M+5'.


  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'CM5'           'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'CM5'           ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  'M+5:Confirmed QTY'.



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

  READ TABLE e03_lt_events INTO ls_event
               WITH KEY name =  slis_ev_top_of_page.

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
* title
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = text-h01.
  APPEND ls_line TO lt_top_of_page.

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
  CONCATENATE 'MIP Coil Requirement Summary with'
              'Items'
              lv_numbering_c
    INTO lv_title
    SEPARATED BY space.
  SET TITLEBAR 'TB' WITH lv_title.

*  SET PF-STATUS 'PS' EXCLUDING 'TRAN'.  "Transfer
  SET PF-STATUS 'PS'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data.

*/
  CLEAR: it_ztmm_6013_01.
  LOOP AT it_list.
    PERFORM get_characteristics.
    MOVE:
              'LS'            TO wa_ztmm_6013_01-doctype,
              'H201'          TO wa_ztmm_6013_01-bukrs,
*              it_list-plant   TO wa_ztmm_6013_01-werks,
              'P001'          TO wa_ztmm_6013_01-werks, "Needed by BHHAN

              p_date(6)       TO wa_ztmm_6013_01-stand_yyyymm,
              p_date          TO wa_ztmm_6013_01-stand_yyyymmdd,

              it_list-matnr   TO wa_ztmm_6013_01-mara_matnr,
              it_list-meins   TO wa_ztmm_6013_01-meins,
              it_list-menge00 TO wa_ztmm_6013_01-cm,
              it_list-menge01 TO wa_ztmm_6013_01-cm1,
              it_list-menge02 TO wa_ztmm_6013_01-cm2,
              it_list-menge03 TO wa_ztmm_6013_01-cm3,
              it_list-menge04 TO wa_ztmm_6013_01-cm4,
              it_list-menge05 TO wa_ztmm_6013_01-cm5.
*          menget   LIKE  mseg-menge,
    APPEND wa_ztmm_6013_01 TO it_ztmm_6013_01.
  ENDLOOP.

*/App. Doc. No.
  PERFORM number_get_next USING    c_nro_nr_09
                                   'ZMMNRO0002'
                          CHANGING w_zdocno.
  COMMIT WORK.

*/Interface with External System
  STATICS: lv_logno_h TYPE num10.
  DATA:    lv_zresult LIKE zsca_if_time_stamp_out-zresult.
  DATA:    lv_message TYPE bapi_msg. "Message text (220)
  CONSTANTS : c_dest(10) VALUE 'WMRM01'.

  CALL FUNCTION 'Z_FMM_6013_OUT_REQINFO'
    DESTINATION              c_dest
    TABLES
      ta_ztmm_6013_01      = it_ztmm_6013_01
    EXCEPTIONS
      communication_failure = 1 MESSAGE lv_message
      system_failure        = 2 MESSAGE lv_message.
  IF sy-subrc NE 0.
    lv_zresult = 'E'.  "Result of the Processing
    MESSAGE s999(zmmm) WITH lv_message.
  ELSE.
    lv_zresult = 'S'.  "Result of the Processing
    lv_message = 'Outbound RFC FM Connected!'(002).
    MESSAGE s999(zmmm) WITH lv_message.
  ENDIF.

*/ Modify it_ZTMM_6013_01
  LOOP AT it_ztmm_6013_01 ASSIGNING <fs_ztmm_6013_01>.
    lv_logno_h = lv_logno_h + 1.

    <fs_ztmm_6013_01>-zdocno  = w_zdocno.  "App. Doc. No.
    <fs_ztmm_6013_01>-logno_h = lv_logno_h."Logno Header

    <fs_ztmm_6013_01>-zuser   = sy-uname.  "User name
*    <fs_ZTMM_6013_01>-zsdat   = .  "Send File Created Date
*    <fs_ZTMM_6013_01>-zstim   = .  "Send file Created Time
    <fs_ztmm_6013_01>-zedat   = sy-datum.  "SAP Interface Date
    <fs_ztmm_6013_01>-zetim   = sy-uzeit.  "SAP Interface Time
    <fs_ztmm_6013_01>-zmode   = 'C'.       "Data Characteristic Flag
    <fs_ztmm_6013_01>-zresult = lv_zresult."Result of the Processing
    <fs_ztmm_6013_01>-zmsg    = lv_message."Message text
*    <fs_ZTMM_6013_01>-zzret   = .  "Inerface Return Value
  ENDLOOP.

*/ Logging to it_ZTMM_6013_01.
  INSERT ztmm_6013_01 FROM TABLE it_ztmm_6013_01.

ENDFORM.                    " process_data
*&---------------------------------------------------------------------*
*&      Form  number_get_next
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_NRO_NR_09  text
*      -->P_1714   text
*      <--P_W_ZDOCNO  text
*----------------------------------------------------------------------*
FORM number_get_next
           USING    value(p_nro_interval) LIKE inri-nrrangenr
                    value(p_nro_object)   LIKE inri-object
           CHANGING value(p_nro_next).
  CLEAR: p_nro_next.
  CALL FUNCTION 'NUMBER_GET_NEXT'
       EXPORTING
            nr_range_nr             = p_nro_interval
            object                  = p_nro_object
       IMPORTING
            number                  = p_nro_next
       EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            OTHERS                  = 7.
  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    "number_get_next
*&---------------------------------------------------------------------*
*&      Form  z_fca_eai_interface_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_fca_eai_interface_log.
*/ Function Module for Interface Log
*
*Where to be inserted:
* 1. Inbound: When interface table is updated after Standard BDC/BAPI
*             executed.
* 2. Outbound: After calling EAI
*
*====================================================================
*
*Function name : Z_FCA_EAI_INTERFACE_LOG
*
*Import/Export Parameter Structure : ZTCA_IF_LOG
*
*IFDOC   <= Serial No. for Log. Leave as empty
*TCODE   <= Present Transaction Code
*TOTAL   <= Total Execution number
*ZSUCC   <= Successful occurrences(number) for BDC/BAPI Processing
*ERROR   <= Failed occurrences(number) for BDC/BAPI Processing
*ERDAT   <= Created on.
*ERZET   <= Created time.
*ERNAM   <= Creator.
*AEDAT   <= Changed on.
*AEZET   <= Changed time
*AENAM   <= the person who change

  DATA: lv_total TYPE i.
  DESCRIBE TABLE it_ztmm_6013_01 LINES lv_total.

  CHECK NOT lv_total IS INITIAL.
  CLEAR: wa_ztca_if_log.
  LOOP AT it_ztmm_6013_01 ASSIGNING <fs_ztmm_6013_01>.
    IF <fs_ztmm_6013_01>-zzret = 'S'.
      wa_ztca_if_log-zsucc = wa_ztca_if_log-zsucc + 1.
    ELSEIF <fs_ztmm_6013_01>-zzret = 'E'.
      wa_ztca_if_log-error = wa_ztca_if_log-error + 1.
    ENDIF.
  ENDLOOP.

  wa_ztca_if_log-tcode = 'ZMMI69'. "Present Transaction Code
  wa_ztca_if_log-total = lv_total. "Total Execution number
  wa_ztca_if_log-erdat = sy-datum. "Created on.
  wa_ztca_if_log-erzet = sy-uname. "Created time.
  wa_ztca_if_log-ernam = sy-uname. "Created by.
  CALL FUNCTION 'Z_FCA_EAI_INTERFACE_LOG'
    EXPORTING
      i_ztca_if_log     = wa_ztca_if_log
* IMPORTING
*   E_ZTCA_IF_LOG              =
   EXCEPTIONS
     update_failed              = 1
     number_range_error         = 2
     tcode_does_not_exist       = 3
     OTHERS                     = 4.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " z_fca_eai_interface_log
*&---------------------------------------------------------------------*
*&      Form  display_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_log.

* set alv parameters
  PERFORM alv_field_build.


*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
   EXPORTING
*   I_INTERFACE_CHECK              = ' '
*   I_BYPASSING_BUFFER             =
*   I_BUFFER_ACTIVE                = ' '
     i_callback_program             = w_repid
     i_callback_pf_status_set       = 'PS_TB'
*     i_callback_user_command        = 'USER_COMMAND'
      i_structure_name               = 'ZTMM_6013_01'
"You can ALV w/o Structure. At this time it_fieldcat[] have to be used.
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
      t_outtab                       = it_ztmm_6013_01
   EXCEPTIONS
     program_error                  = 1
     OTHERS                         = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " display_log
*&---------------------------------------------------------------------*
*&      Form  catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM catalog  TABLES   ta_fieldcat
                         STRUCTURE wa_fieldcat
              USING    value(p_key)
                       value(p_fieldname)
                       value(p_tabname)
                       value(p_ref_tabname)
                       value(p_ref_fieldname)
                       value(p_reptext_ddic)
                       value(p_do_sum)
                       value(p_no_zero)
                       value(p_outputlen)
                       value(p_just)
                       value(p_tech)
                       value(p_no_out)
                       value(p_fix_column)
                       value(p_emphasize)
                       value(p_qfieldname)
                       value(p_qtabname)
                       value(p_ctabname)
                       value(p_cfieldname)
                       value(p_lavel).

  TYPE-POOLS: slis.
  DATA :  lt_fieldcat TYPE slis_t_fieldcat_alv,
          ls_fieldcat TYPE slis_fieldcat_alv,
          l_pos TYPE i.

*  l_pos = l_pos + 1.
  ls_fieldcat-col_pos       = l_pos.
  ls_fieldcat-key           = p_key.
  ls_fieldcat-fieldname     = p_fieldname.
  ls_fieldcat-tabname       = p_tabname.
  ls_fieldcat-ref_tabname   = p_ref_tabname.
  ls_fieldcat-ref_fieldname = p_ref_fieldname.
  ls_fieldcat-reptext_ddic  = p_reptext_ddic.
  ls_fieldcat-do_sum        = p_do_sum.
  ls_fieldcat-no_zero       = p_no_zero.
  ls_fieldcat-outputlen     = p_outputlen.
* --> Sort
  ls_fieldcat-just          = p_just.
  ls_fieldcat-tech          = p_tech.
* --> Hiding FIELD
  ls_fieldcat-no_out        = p_no_out.
  ls_fieldcat-fix_column    = p_fix_column.
* --> Intensify
  ls_fieldcat-emphasize     = p_emphasize.
* --> Quantity field
  ls_fieldcat-qfieldname    = p_qfieldname.
  ls_fieldcat-qtabname      = p_qtabname.
* --> Currency field
  ls_fieldcat-ctabname      = p_ctabname.
  ls_fieldcat-cfieldname    = p_cfieldname.
* --> field label
  ls_fieldcat-seltext_m     = p_lavel.
  ls_fieldcat-seltext_l     = p_lavel.
  ls_fieldcat-seltext_s     = p_lavel.
  ls_fieldcat-ddictxt       = 'M'.

*  APPEND ls_fieldcat TO  lt_fieldcat.
  APPEND ls_fieldcat TO  ta_fieldcat.
ENDFORM.                    " catalog
*&---------------------------------------------------------------------*
*&      Form  get_characteristics
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_characteristics.
* For bapi_objcl_getclasses and bapi_objcl_getdetail.
  DATA: lt_alloclist   LIKE TABLE OF bapi1003_alloc_list,
        ls_alloclist   LIKE LINE OF lt_alloclist,
        lt_bapiret2    LIKE TABLE OF bapiret2.
  DATA:
    lt_allocvaluesnum  LIKE TABLE OF bapi1003_alloc_values_num,
    lt_allocvalueschar LIKE TABLE OF bapi1003_alloc_values_char,
    lt_allocvaluescurr LIKE TABLE OF bapi1003_alloc_values_curr,
    ls_allocvalueschar LIKE LINE OF lt_allocvalueschar,
    lv_objectkey       LIKE bapi1003_key-object,
    lv_objecttable     LIKE bapi1003_key-objecttable,
    lv_classnum        LIKE bapi1003_key-classnum,
    lv_classtype       LIKE bapi1003_key-classtype.
*
  lv_objectkey   = it_list-matnr.
  lv_objecttable = 'MARA'.
  lv_classtype   = '001'.

* Get Class number
  PERFORM bapi_objcl_getclasses
        TABLES lt_alloclist
               lt_bapiret2
        USING  lv_objectkey    "it_list-matnr
               lv_objecttable  "MARA
               lv_classtype.   "001
  READ TABLE lt_alloclist INTO ls_alloclist
                          INDEX 1.
  CHECK sy-subrc = 0.
  lv_classnum    = ls_alloclist-classnum.
* Get Characteristic Value by Class
  PERFORM bapi_objcl_getdetail
       TABLES lt_allocvaluesnum
              lt_allocvalueschar  "Characteristic Data
              lt_allocvaluescurr
              lt_bapiret2
       USING  lv_objectkey   "Blank mat number
              lv_objecttable "Table
              lv_classnum    "Class number
              lv_classtype.  "Class type

*ZSTEEL_MATPROPERTY
  CLEAR: ls_allocvalueschar.
  READ TABLE lt_allocvalueschar INTO ls_allocvalueschar
             WITH KEY charact = 'ZSTEEL_MATPROPERTY'.
  wa_ztmm_6013_01-matl = ls_allocvalueschar-value_char.

*COAT_QTY : I can't find this.

*ZSPEC_THICK
  CLEAR: ls_allocvalueschar.
  READ TABLE lt_allocvalueschar INTO ls_allocvalueschar
             WITH KEY charact = 'ZSPEC_THICK'.
  wa_ztmm_6013_01-thick = ls_allocvalueschar-value_char.

*ZSPEC_WIDTH
  CLEAR: ls_allocvalueschar.
  READ TABLE lt_allocvalueschar INTO ls_allocvalueschar
             WITH KEY charact = 'ZSPEC_WIDTH'.
  wa_ztmm_6013_01-width = ls_allocvalueschar-value_char.

*ZSPEC_LENGTH
  CLEAR: ls_allocvalueschar.
  READ TABLE lt_allocvalueschar INTO ls_allocvalueschar
             WITH KEY charact = 'ZSPEC_LENGTH'.
  wa_ztmm_6013_01-length = ls_allocvalueschar-value_char.

*ZIN_ OR_OUT
  CLEAR: ls_allocvalueschar.
  READ TABLE lt_allocvalueschar INTO ls_allocvalueschar
             WITH KEY charact = 'ZIN_ OR_OUT'.
  wa_ztmm_6013_01-in_out = ls_allocvalueschar-value_char.

ENDFORM.                    " get_characteristics
*&---------------------------------------------------------------------*
*&      Form  bapi_objcl_getclasses
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ALLOCLIST  text
*      -->P_LT_BAPIRET2  text
*      -->P_LV_OBJECTKEY  text
*      -->P_LV_OBJECTTABLE  text
*      -->P_LV_CLASSTYPE  text
*----------------------------------------------------------------------*
FORM bapi_objcl_getclasses
          TABLES ext_alloclist
                   STRUCTURE bapi1003_alloc_list
                 ext_bapiret2
                   STRUCTURE bapiret2
          USING value(im_objectkey_imp)   LIKE bapi1003_key-object
                value(im_objecttable_imp) LIKE bapi1003_key-objecttable
                value(im_classtype_imp)   LIKE bapi1003_key-classtype.
  CLEAR: ext_alloclist, ext_bapiret2, ext_alloclist[], ext_bapiret2[].
  CALL FUNCTION 'BAPI_OBJCL_GETCLASSES'
    EXPORTING
      objectkey_imp         = im_objectkey_imp
      objecttable_imp       = im_objecttable_imp
      classtype_imp         = im_classtype_imp
*   READ_VALUATIONS       =
*   KEYDATE               = SY-DATUM
*   LANGUAGE              = SY-LANGU
    TABLES
      alloclist             = ext_alloclist
*   ALLOCVALUESCHAR       =
*   ALLOCVALUESCURR       =
*   ALLOCVALUESNUM        =
      return                = ext_bapiret2.
ENDFORM.                    "bapi_objcl_getclasses
*&---------------------------------------------------------------------*
*&      Form  bapi_objcl_getdetail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM bapi_objcl_getdetail
             TABLES ext_allocvaluesnum
                      STRUCTURE bapi1003_alloc_values_num
                    ext_allocvalueschar
                      STRUCTURE bapi1003_alloc_values_char
                    ext_allocvaluescurr
                      STRUCTURE bapi1003_alloc_values_curr
                    ext_bapiret2
                      STRUCTURE bapiret2
             USING  value(im_objectkey)   LIKE bapi1003_key-object
                    value(im_objecttable) LIKE bapi1003_key-objecttable
                    value(im_classnum)    LIKE bapi1003_key-classnum
                    value(im_classtype)   LIKE bapi1003_key-classtype.
  CLEAR: ext_allocvaluesnum,    ext_allocvalueschar,
         ext_allocvaluescurr,   ext_bapiret2,
         ext_allocvaluesnum[],  ext_allocvalueschar[],
         ext_allocvaluescurr[], ext_bapiret2[].
  CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
    EXPORTING
      objectkey              = im_objectkey
      objecttable            = im_objecttable
      classnum               = im_classnum
      classtype              = im_classtype
*   KEYDATE                = SY-DATUM
*   UNVALUATED_CHARS       = ' '
*   LANGUAGE               = SY-LANGU
* IMPORTING
*   STATUS                 =
*   STANDARDCLASS          =
    TABLES
      allocvaluesnum         = ext_allocvaluesnum
      allocvalueschar        = ext_allocvalueschar
      allocvaluescurr        = ext_allocvaluescurr
      return                 = ext_bapiret2.
ENDFORM.                    "bapi_objcl_getdetail

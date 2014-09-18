************************************************************************
* Program Name      : ZEMMPM18_DAILY_DELIVERY                          *
* Author            : Jaesung-LEE                                      *
* Creation Date     : 2003.09.18.                                      *
* Specifications By : Jaesung-LEE                                      *
* Development Request No :                                             *
* Addl Documentation:                                                  *
* Description       : Sub-Daily Delivery Schedule user-exit            *
*                                                                      *
* Modification Logs                                                    *
* Date            Developer          RequestNo    Description          *
* 2003.09.18     Jaesung-LEE                     Initial Coding        *
* 2005.02.24     Furong              UD1K914657  index for it_sort is
**                                          not work for certain case
**                                   UD1K914659
*                                    UD1K914771  refresh error
************************************************************************

REPORT zemmpm18_daily_delivery_n0224.
************************************************************************
* Data
************************************************************************

* Schedule agreement data .
DATA : BEGIN OF it_eket OCCURS 100.
        INCLUDE STRUCTURE eket.
DATA : END   OF it_eket.
DATA : BEGIN OF it_sort OCCURS 100.
        INCLUDE STRUCTURE eket.
DATA : END   OF it_sort.
DATA : BEGIN OF it_cal OCCURS 100.
        INCLUDE STRUCTURE eket.
DATA : END   OF it_cal.
* po item
DATA : BEGIN OF it_ekpo OCCURS 100.
        INCLUDE STRUCTURE ekpo.
DATA : END OF it_ekpo.
* BDC data
DATA: BEGIN OF bdc_tab OCCURS 100.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF bdc_tab.
DATA : BEGIN OF it_ekko OCCURS 10,
       ebeln LIKE ekko-ebeln,
       lifnr LIKE ekko-lifnr,
       END OF it_ekko.
*
DATA : BEGIN OF it_time .
        INCLUDE STRUCTURE ztmm_delisch.
DATA : END OF it_time.
DATA:   messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
DATA : w_flag.

*--- insert by stlim (2004/07/16)
CONSTANTS : c_calid LIKE scal-fcalid VALUE 'HM'.
*--- end of insert

************************************************************************
* SELECTION SCREEN
************************************************************************

SELECT-OPTIONS : s_ebeln FOR it_ekko-ebeln.
PARAMETERS : p_curday LIKE sy-datum DEFAULT sy-datum OBLIGATORY,
             p_cmode LIKE ctu_params-dismode DEFAULT 'A' OBLIGATORY.

"A: show all dynpros
"E: show dynpro on error only
"N: do not display dynpro

************************************************************************
* START-OF-SELECTION
************************************************************************

START-OF-SELECTION.
  PERFORM select_ekko.
  CLEAR w_flag.
  LOOP AT it_ekko.
    PERFORM sa_selection .
    CHECK w_flag = space.
    PERFORM data_calculation .
    CHECK w_flag = space .
    PERFORM bdc_generation.
  ENDLOOP.


*&---------------------------------------------------------------------*
*&      Form  sa_selection
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sa_selection .

  CLEAR w_flag .

  SELECT * INTO TABLE it_eket
    FROM   eket
    WHERE  ebeln  = it_ekko-ebeln
      AND  menge > 0 .

  IF sy-subrc NE 0.
    w_flag = 'X'.
  ENDIF.

  SELECT * INTO TABLE it_ekpo
      FROM ekpo
      WHERE ebeln  = it_ekko-ebeln
        AND loekz  = ' '

**--- insert by stlim (2004/04/13)
        AND elikz  EQ space.
**--- end of insert

  IF sy-subrc NE 0.
    w_flag = 'X'.
  ENDIF.

**--- insert by stlim (2004/04/13)
  LOOP AT it_eket.
    READ TABLE it_ekpo WITH KEY ebeln = it_eket-ebeln
                                ebelp = it_eket-ebelp.
    IF sy-subrc NE 0.
      DELETE it_eket.
    ENDIF.
  ENDLOOP.
**--- end of insert

  CLEAR : it_sort, it_sort[].

** changed by Furong
*  it_sort[] = it_eket[].
** end of changed

** insert by Furong
 SELECT * INTO TABLE it_sort
    FROM   eket
    WHERE  ebeln  = it_ekko-ebeln.

 LOOP AT it_sort.
    READ TABLE it_ekpo WITH KEY ebeln = it_sort-ebeln
                                ebelp = it_sort-ebelp.
    IF sy-subrc NE 0.
      DELETE it_sort.
    ENDIF.
  ENDLOOP.

** end of insert


ENDFORM.                    " sa_selection
*&---------------------------------------------------------------------*
*&      Form  bdc_generation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_generation.

  DATA : l_sec_item,
         l_count(4) ,"TYPE I.
         l_menge(16),
         l_ebeln LIKE eket-ebeln,
         l_ebelp LIKE eket-ebelp.


  REFRESH:  bdc_tab.
  LOOP AT it_ekpo.
    CLEAR  :  bdc_tab, l_ebelp.

    it_eket-ebeln = 'Xxxx'.  " on change for value change
*    it_eket-ebelp = '99999'.

*    BREAK-POINT.


*  SORT ett BY ebelp eindt uzeit etenr.

    SORT it_eket BY ebeln ebelp eindt etenr.

    LOOP AT it_eket WHERE ebeln  =  it_ekpo-ebeln
                      AND ebelp  =  it_ekpo-ebelp
                      AND eindt  =  it_ekpo-aedat.
*    BREAK-POINT.
*    position search

      CLEAR l_menge .

      WRITE it_eket-menge TO l_menge  UNIT it_ekpo-meins.
      SORT it_sort BY ebelp eindt uzeit etenr .
      READ TABLE it_sort WITH KEY ebeln = it_eket-ebeln
                                  ebelp = it_eket-ebelp
                                  etenr = it_eket-etenr .
      IF sy-subrc EQ 0.
        it_sort-uzeit = it_eket-uzeit.
        MOVE : sy-tabix TO l_count. " L_COUNT = SY-TABIX.

        IF it_eket-menge NE 0.
          MODIFY it_sort INDEX sy-tabix.
        ELSE.
          DELETE it_sort INDEX sy-tabix.   " synchronous item line
        ENDIF.

      ENDIF.

*      IF it_eket-ebeln NE l_ebeln.
      ON CHANGE OF  it_eket-ebeln .
        PERFORM bdc_data USING : 'X' 'SAPMM06E'      '0205',
                                 ' ' 'RM06E-EVRTN'    it_eket-ebeln,
                                 ' ' 'BDC_OKCODE'    '/00'.
        l_sec_item = ' '.
*        l_ebeln = it_eket-ebeln.
      ENDON.
*      Endif.
*      ON CHANGE OF  it_eket-ebelp.
      IF it_eket-ebelp NE l_ebelp.

        l_ebelp = it_eket-ebelp.

        IF l_sec_item = 'X'.
          PERFORM bdc_data USING : 'X' 'SAPMM06E'    '1117',
*                                   ' ' 'RM06E-EBELP'  it_eket-ebelp ,
                                   ' ' 'BDC_OKCODE'  'BACK'.
        ELSE.
          l_sec_item = 'X'.
        ENDIF.

        PERFORM bdc_data USING : 'X' 'SAPMM06E'    '0222',
                                 ' ' 'RM06E-EBELP'  it_eket-ebelp ,
                                 ' ' 'BDC_OKCODE'  '/00'.

        PERFORM bdc_data USING : 'X' 'SAPMM06E'    '0222',
                                 ' ' 'RM06E-TCSELFLAG(01)'  'X' ,
                                 ' ' 'BDC_OKCODE'  '=ET'.
      ENDIF.

*      ENDON.

      PERFORM bdc_data USING : 'X' 'SAPMM06E'    '1117',
                               ' ' 'RM06E-ETNR1'  l_count,
                               ' ' 'BDC_OKCODE'  '/00'.

      PERFORM bdc_data USING : 'X' 'SAPMM06E'    '1117',
                               ' ' 'EKET-MENGE(01)'  l_menge ,
                               ' ' 'EKET-UZEIT(01)'  it_eket-uzeit(4),
                               ' ' 'BDC_OKCODE'  '/00'.


    ENDLOOP.


    AT END OF ebeln .

      IF NOT bdc_tab[] IS INITIAL.
        PERFORM bdc_data USING : 'X' 'SAPMM06E'    '1117',
                                 ' ' 'BDC_OKCODE'  '=BU'.
        PERFORM bdc_excution.
      ENDIF.

    ENDAT.


  ENDLOOP.

ENDFORM.                    " bdc_generation

*&---------------------------------------------------------------------*
*&      Form  BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0357   text
*      -->P_0358   text
*      -->P_0359   text
*----------------------------------------------------------------------*
FORM bdc_data USING dynbegin name value.

  IF dynbegin = 'X'.
    CLEAR bdc_tab.
    MOVE: name TO bdc_tab-program,
          value TO bdc_tab-dynpro,
          'X'   TO bdc_tab-dynbegin.
    APPEND bdc_tab.
  ELSE.
    CLEAR bdc_tab.
    MOVE: name TO bdc_tab-fnam,
          value TO bdc_tab-fval.
    APPEND bdc_tab.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  data_calculation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_calculation.

  DATA : l_index TYPE i.
  DATA : l_time  TYPE i.

  LOOP AT it_ekpo .
    SELECT SINGLE  * INTO  it_time
            FROM ztmm_delisch
            WHERE matnr  = it_ekpo-matnr
              AND lifnr  = it_ekko-lifnr.

    IF sy-subrc NE 0.
      DELETE it_ekpo.
      CONTINUE.
    ENDIF.

    PERFORM time_caculation CHANGING l_time   .
* time not defintion exit
    IF l_time = 0.
      w_flag = 'X'.
      EXIT .
    ENDIF.

    PERFORM sub-daily_menge_cacul USING l_time .

*--- insert by stlim (2004/07/07)
    IF w_flag NE space.
      EXIT.
    ENDIF.
*--- end of insert

    PERFORM change_eket_menge USING l_time.

  ENDLOOP.


ENDFORM.                    " data_calculation
*&---------------------------------------------------------------------*
*&      Form  bdc_excution
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_excution.

  TABLES : t100.
  DATA: l_mstring(480).

  CALL TRANSACTION 'ME38'
                 USING    bdc_tab
                 MODE     p_cmode
                 UPDATE  'A'
                 MESSAGES INTO messtab.

  CHECK sy-subrc NE 0.


  LOOP AT messtab.
    SELECT SINGLE * FROM t100 WHERE sprsl = messtab-msgspra
                              AND   arbgb = messtab-msgid
                              AND   msgnr = messtab-msgnr.
    IF sy-subrc = 0.
      l_mstring = t100-text.
      IF l_mstring CS '&1'.
        REPLACE '&1' WITH messtab-msgv1 INTO l_mstring.
        REPLACE '&2' WITH messtab-msgv2 INTO l_mstring.
        REPLACE '&3' WITH messtab-msgv3 INTO l_mstring.
        REPLACE '&4' WITH messtab-msgv4 INTO l_mstring.
      ELSE.
        REPLACE '&' WITH messtab-msgv1 INTO l_mstring.
        REPLACE '&' WITH messtab-msgv2 INTO l_mstring.
        REPLACE '&' WITH messtab-msgv3 INTO l_mstring.
        REPLACE '&' WITH messtab-msgv4 INTO l_mstring.
      ENDIF.
      CONDENSE l_mstring.
      WRITE: / messtab-msgtyp, l_mstring(250).
    ELSE.
      WRITE: / messtab.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " bdc_excution
*&---------------------------------------------------------------------*
*&      Form  select_ekko
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_ekko.

  SELECT ebeln lifnr INTO TABLE it_ekko
      FROM ekko
      WHERE bstyp = 'L'    " s/a
        AND loekz = ' '    " deletion flag
        AND ebeln IN s_ebeln.




ENDFORM.                    " select_ekko
*&---------------------------------------------------------------------*
*&      Form  calulation_rate
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_SUM  text
*      -->P_L_RATE  text
*----------------------------------------------------------------------*
FORM calulation_rate USING    p_max
                              p_min
                              p_rate
                              p_sum  LIKE it_eket-menge
                              p_mlot LIKE marc-bstma.
  DATA: l_input TYPE f,
        l_output TYPE f.

  CHECK p_min > 0.
  CHECK p_mlot > 0.
*  L_INPUT = P_MAX / P_MIN .
  l_input = ( ( p_sum / p_min ) / p_mlot ) .

  CALL FUNCTION 'FIMA_NUMERICAL_VALUE_ROUND'
       EXPORTING
            i_rtype     = '+'
            i_runit     = '1'
            i_value     = l_input
       IMPORTING
            e_value_rnd = l_output.

  MOVE :  l_output TO p_rate.


ENDFORM.                    " calulation_rate
*&---------------------------------------------------------------------*
*&      Form  TIME_CACULATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DAY  text
*      -->P_L_TIME  text
*----------------------------------------------------------------------*
FORM time_caculation CHANGING p_time TYPE i .
  DATA : l_co(2).
  DATA : l_fname(20) .
  DATA : l_day   LIKE t5a4a-dlydy.
  FIELD-SYMBOLS: <t1> TYPE ANY.

*--- insert by stlim (2004/07/16)
  DATA : l_factorydate LIKE scal-facdate.
*--- end of insert

  MOVE : it_ekpo-etfz1 TO l_day.

*--- blocked by stlim (2004/07/16)
*  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
*       EXPORTING
*            date      = p_curday
*            days      = l_day
*            months    = '00'
*            signum    = '+'
*            years     = '00'
*       IMPORTING
*            calc_date = it_ekpo-aedat.
*--- end of block

*--- insert by stlim (2004/07/16)
  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
       EXPORTING
            date                         = p_curday
            factory_calendar_id          = c_calid
       IMPORTING
            factorydate                  = l_factorydate
       EXCEPTIONS
            calendar_buffer_not_loadable = 1
            correct_option_invalid       = 2
            date_after_range             = 3
            date_before_range            = 4
            date_invalid                 = 5
            factory_calendar_not_found   = 6
            OTHERS                       = 7.
  l_factorydate = l_factorydate + l_day.
  CALL FUNCTION 'FACTORYDATE_CONVERT_TO_DATE'
       EXPORTING
            factorydate                  = l_factorydate
            factory_calendar_id          = c_calid
       IMPORTING
            date                         = it_ekpo-aedat
       EXCEPTIONS
            calendar_buffer_not_loadable = 1
            factorydate_after_range      = 2
            factorydate_before_range     = 3
            factorydate_invalid          = 4
            factory_calendar_id_missing  = 5
            factory_calendar_not_found   = 6
            OTHERS                       = 7.
*--- end of insert

  MODIFY it_ekpo.
  CLEAR : l_co, p_time.

  DO 10 TIMES .
    l_co = l_co + 1.
    CONCATENATE 'IT_TIME-TIME' l_co INTO l_fname.
    ASSIGN (l_fname) TO <t1>.
    IF NOT <t1> IS INITIAL.
      p_time = p_time + 1.
    ENDIF.
  ENDDO.

  CLEAR : it_cal, it_cal[].

  DO p_time TIMES.
    APPEND it_cal.
  ENDDO.

  CLEAR l_co.

ENDFORM.                    " TIME_CACULATION
*&---------------------------------------------------------------------*
*&      Form  Sub-Daily_MENGE_CACUL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub-daily_menge_cacul USING p_time.

  DATA : l_sum LIKE it_eket-menge.
  DATA : l_mlot LIKE marc-bstma.
  DATA : l_clot LIKE marc-bstma.
  DATA : l_rate TYPE i.
  DATA : l_index TYPE i.
  DATA : l_co(2).
  DATA : l_fname(20) .

  FIELD-SYMBOLS: <t1> TYPE ANY.

** 2004 01 13 insert by jslee
  SORT  it_eket BY ebeln ebelp eindt menge DESCENDING.
** end of insert.

*<<< insert by jslee 2004/03/24
  LOOP AT it_eket WHERE eindt = it_ekpo-aedat
                    AND ebelp = it_ekpo-ebelp
                    AND ebeln = it_ekpo-ebeln.


    l_sum = l_sum + it_eket-menge.
    l_index = l_index + 1.
  ENDLOOP.

  SELECT SINGLE bstma INTO l_mlot
     FROM marc
     WHERE matnr = it_ekpo-matnr
       AND werks = it_ekpo-werks.

*--- insert by stlim (2004/07/07)
  IF l_mlot EQ space AND p_time GE 2.
    w_flag = 'X'.
    EXIT .
  ENDIF.
*--- end of insert

*  IF SY-SUBRC EQ 0.
*    L_CLOT = L_MLOT * L_RATE.
*  ENDIF.

  PERFORM calulation_rate USING l_index p_time l_rate
                                l_sum   l_mlot.

  l_clot = l_mlot * l_rate.

  CLEAR : l_index .

* CACULATION MENGE
  LOOP AT it_cal.
    IF l_sum > l_clot.
      it_cal-menge =  l_clot.
      l_sum = l_sum - l_clot.
    ELSE.

      IF l_sum > l_mlot .
        IF sy-tabix = p_time.
          it_cal-menge = l_sum.
          l_sum = 0.
        ELSE.
          it_cal-menge = l_mlot.
          l_sum = l_sum - l_mlot.
        ENDIF.
      ELSE.
        it_cal-menge =  l_sum.
        l_sum = 0.
      ENDIF.
    ENDIF.


    l_co = l_co + 1.
    IF p_time  >= l_co.
      CONCATENATE 'IT_TIME-TIME' l_co INTO l_fname.
      ASSIGN (l_fname) TO <t1>.
      it_cal-uzeit = <t1>.
    ENDIF.
    MODIFY  it_cal .

  ENDLOOP.
*  * ERROR HANDING..
  IF l_sum <> 0.
    READ TABLE it_cal INDEX p_time.
    IF sy-subrc EQ 0.
      it_cal-menge = it_cal-menge + l_sum .
      MODIFY it_cal INDEX p_time.
    ENDIF.
  ENDIF.

*>>>>====== insert by jslee 2004/03/24



ENDFORM.                    " Sub-Daily_MENGE_CACUL
*&---------------------------------------------------------------------*
*&      Form  CHANGE_EKET_MENGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_eket_menge USING p_time .
  DATA : l_index TYPE i.

*   clear data .
** 2004 01 13 insert by jslee
  SORT it_eket BY ebeln ebelp eindt etenr.
** end of insert.

  LOOP AT it_eket WHERE eindt = it_ekpo-aedat
                    AND ebelp = it_ekpo-ebelp
                    AND ebeln = it_ekpo-ebeln.
    l_index = l_index + 1.

    IF l_index <=  p_time .
      READ TABLE it_cal INDEX  l_index.
      IF sy-subrc EQ 0.
        it_eket-menge =  it_cal-menge.
        it_eket-uzeit =  it_cal-uzeit.
      ENDIF.
** 2004 01 13  delete  by jslee
*        it_eket-sernr =  l_index.
*  end of delete.
    ELSE.
      it_eket-menge =  0.
    ENDIF.
    MODIFY it_eket.
  ENDLOOP.

ENDFORM.                    " CHANGE_EKET_MENGE

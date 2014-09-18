*----------------------------------------------------------------------*
*   INCLUDE ZXM06U65                                                   *
*----------------------------------------------------------------------*
*"      I_EKPO STRUCTURE  UEKPO
*"      I_EKET STRUCTURE  EKET

DATA : BEGIN OF it_time .
        INCLUDE STRUCTURE ztmm_delisch.
DATA : END OF it_time.

DATA : l_eindt LIKE sy-datum.
DATA : l_day   LIKE t5a4a-dlydy.
*DATA : l_menge LIKE i_eket-menge.

DATA : l_count TYPE i.
DATA : l_time  TYPE i.

FIELD-SYMBOLS: <t1> TYPE ANY.

DATA : l_fname(20) .
DATA : l_co(2).

DATA : BEGIN OF st_div ,
       bstma   LIKE marc-bstma ,
       div    LIKE eket-menge ,
       mod    LIKE eket-menge ,
       menge  LIKE eket-menge ,
       END OF st_div .


* CHECK sy-tcode NE 'ME33L'.
IF sy-uname = 'JSLEE72'.
*

  LOOP AT i_ekpo.

* if i_ekpo-ebeln = '4600000001'.
  break-point.
* endif.
*
* vendor ??

    SELECT SINGLE  * INTO  it_time
           FROM ztmm_delisch
           WHERE matnr  = i_ekpo-matnr.

    IF sy-subrc EQ 0.
* firm zone move.

      MOVE : i_ekpo-etfz1 TO l_day.

* count time
      CLEAR : l_count , l_time.

      DO 10 TIMES .
        l_co = l_co + 1.
        CONCATENATE 'IT_TIME-TIME' l_co INTO l_fname.

        ASSIGN (l_fname) TO <t1>.

        IF NOT <t1> IS INITIAL.
          l_time = l_time + 1.
        ENDIF.

      ENDDO.

      CLEAR : l_eindt, l_co.
      CLEAR: st_div, l_count.

*      CHECK NOT i_ekpo-etfz1 IS INITIAL.
*

      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
           EXPORTING
                date      = sy-datum
                days      = l_day
                months    = '00'
                signum    = '+'
                years     = '00'
           IMPORTING
                calc_date = l_eindt.

* sum



      LOOP AT i_eket WHERE eindt = l_eindt
                       AND ebelp = i_ekpo-ebelp.

        l_count = l_count +  1.
        st_div-menge = st_div-menge +  i_eket-menge.

      ENDLOOP.

* lot size selection

      SELECT SINGLE  bstma INTO st_div-bstma
             FROM marc
             WHERE matnr = i_ekpo-matnr
               AND werks = i_ekpo-werks .

*     day per lot size        =  ( quntity / lot size  ) / time
*     day per lot size  rest  =  ( quntity / lot size  ) / time


      IF st_div-bstma NE 0 AND l_time NE 0.
*    BREAK-POINT.

        st_div-div = ( st_div-menge / st_div-bstma ) DIV l_time.
        st_div-mod = ( st_div-menge / st_div-bstma ) MOD l_time.
      ENDIF.


      CLEAR : l_co.

      LOOP AT i_eket WHERE eindt = l_eindt
                       AND ebelp = i_ekpo-ebelp.

        l_co = l_co + 1.
        CONCATENATE 'IT_TIME-TIME' l_co INTO l_fname.

        ASSIGN (l_fname) TO <t1>.
        i_eket-uzeit = <t1>.
        i_EKET-FIXKZ = 'X'.
        IF i_eket-menge NE 0.
*         break-point.

*          i_eket-menge = i_eket-menge + 1.
        ENDIF.
        MODIFY i_eket.

      ENDLOOP.


*      DO l_count TIMES .
*
*      ENDDO.

    ENDIF.


  ENDLOOP.

ENDIF.

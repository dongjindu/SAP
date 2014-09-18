FUNCTION z_fmm_stl_overtime.
*"----------------------------------------------------------------------
*"*"Global interface:
*"  IMPORTING
*"     VALUE(I_DATUM) LIKE  SY-DATUM
*"     VALUE(I_UZEIT) LIKE  SY-UZEIT
*"  TABLES
*"      T_WORKTIME STRUCTURE  ZSMM_WORKTIME OPTIONAL
*"  CHANGING
*"     VALUE(E_1SHIFT_OVERTIME) TYPE  CHAR1 OPTIONAL
*"     VALUE(E_2SHIFT_OVERTIME) TYPE  CHAR1 OPTIONAL
*"----------------------------------------------------------------------

*---
*  TABLES : " crhd,
*           " kako,
*           " kazy,
*           " kapa,
*           " tc37a.


  DATA : BEGIN OF it_worktime OCCURS 0,
           tagnr LIKE kapa-tagnr,
           schnr LIKE kapa-schnr,
           kaptprog LIKE tc37a-kaptprog,
           begda LIKE tc37a-begda,
           endda LIKE tc37a-endda,
           begzt LIKE tc37a-begzt,
           endzt LIKE tc37a-endzt,
         END OF it_worktime.

*---
  DATA : l_daynr LIKE hrvsched-daynr,
         l_daytxt LIKE hrvsched-daytxt,
         l_dayfree LIKE hrvsched-noday,
         l_current_date TYPE d.

  CONSTANTS : c_uzeit_000000 TYPE t VALUE '000000',
              c_time_020000 TYPE t VALUE '020000',
              c_uzeit_035959 TYPE t VALUE '035959'.

  CLEAR : crhd, kako, kazy, it_worktime, it_worktime[], l_daynr,
          l_daytxt, l_dayfree.


*---
  IF i_uzeit GE c_uzeit_000000 AND i_uzeit LE c_uzeit_035959.
    l_current_date = i_datum - 1.
  ELSE.
    l_current_date = i_datum.
  ENDIF.

  SELECT SINGLE kapid INTO crhd-kapid
                      FROM crhd
                     WHERE objty EQ 'A'
                       AND arbpl EQ 'T'
                       AND werks EQ 'P001'.

  SELECT SINGLE mosid INTO kako-mosid
                      FROM kako
                     WHERE kapid EQ crhd-kapid.

  SELECT SINGLE versn INTO kazy-versn
                      FROM kazy
                     WHERE kapid EQ crhd-kapid
                       AND datub GE l_current_date.

  CALL FUNCTION 'RH_GET_DATE_DAYNAME'
       EXPORTING
            langu               = sy-langu
            date                = l_current_date
       IMPORTING
            daynr               = l_daynr
            daytxt              = l_daytxt
            dayfree             = l_dayfree
       EXCEPTIONS
            no_langu            = 1
            no_date             = 2
            no_daytxt_for_langu = 3
            invalid_date        = 4
            OTHERS              = 5.

  CLEAR : it_worktime, it_worktime[], t_worktime, t_worktime[].

  SELECT tagnr
         schnr
         kaptprog
         b~begda
         b~endda
         b~begzt
         b~endzt
               INTO CORRESPONDING FIELDS OF TABLE it_worktime
               FROM kapa AS a INNER JOIN tc37a AS b
                 ON a~mandt EQ b~mandt
                AND a~tprog EQ b~kaptprog
              WHERE schgrup EQ kako-mosid
                AND kapid   EQ crhd-kapid
                AND versn   EQ kazy-versn
                AND begda   LE l_current_date
                AND endda   GE l_current_date
                AND tagnr   EQ l_daynr.

  SORT it_worktime BY tagnr schnr.

  t_worktime[] = it_worktime[].

*---
  DATA : l_endzt LIKE it_worktime-endzt,
         l_begzt LIKE it_worktime-begzt.

*---
  CLEAR : e_1shift_overtime, e_2shift_overtime.

  CLEAR : it_worktime, l_endzt.
  READ TABLE it_worktime WITH KEY schnr = 1.
  MOVE : it_worktime-endzt TO l_endzt.

  CHECK sy-subrc EQ 0.

  CLEAR : it_worktime, l_begzt.
  READ TABLE it_worktime WITH KEY schnr = 2.
  MOVE : it_worktime-begzt TO l_begzt.

  CHECK sy-subrc EQ 0.

*--- 1 shift overtime
  IF l_endzt EQ l_begzt.
    MOVE : 'X' TO e_1shift_overtime.
  ENDIF.

  CLEAR : it_worktime, l_endzt.
  READ TABLE it_worktime WITH KEY schnr = 2.
  MOVE : it_worktime-endzt TO l_endzt.

*--- 2 shift overtime
  IF l_endzt NE c_time_020000.
    MOVE : 'X' TO e_2shift_overtime.
  ENDIF.


ENDFUNCTION.

FUNCTION y_fpp_prod_result_for_dash.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_SDATE) TYPE  ZSDATE OPTIONAL
*"  TABLES
*"      T_RESULT STRUCTURE  YSPP_PROD_RESULT
*"----------------------------------------------------------------------


  SELECT SINGLE atinn INTO v_worder_atn
    FROM cabn WHERE atnam = 'P_WORK_ORDER'.

  SELECT SINGLE atinn INTO v_usage_atn
    FROM cabn WHERE atnam = 'P_USAGE_CAR'.

  SELECT SINGLE atinn INTO v_model_atn
    FROM cabn WHERE atnam = 'P_MODEL'.

  SELECT SINGLE atinn INTO v_soff
    FROM cabn WHERE atnam = 'P_RP18_SHOP_DATE'.

  SELECT SINGLE atinn INTO v_soff_act
    FROM cabn WHERE atnam = 'P_RP18_ACTUAL_DATE'.

  v_d1dat = i_sdate - 1.
  CONCATENATE i_sdate(6) '01'   INTO v_mon_1st.
  CONCATENATE i_sdate(4) '0101' INTO v_year_1st.
  CONCATENATE i_sdate(4) '1231' INTO v_year_end.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = i_sdate
    IMPORTING
      last_day_of_month = v_mon_end
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.
*  IF sy-subrc <> 0. EXIT. ENDIF.


  REFRESH it_result.
  CLEAR: v_mjahr, v_spmon, v_sdate, v_model, v_prodqty,
         v_dealer, v_usage.

*  EXEC SQL PERFORMING append_prod_result.
*    SELECT SUBSTR(TO_NCHAR(A.ATFLV),1,4),SUBSTR(TO_NCHAR(A.ATFLV),1,6),
*           A.ATFLV, B.ATWRT, COUNT(*)
*      INTO :V_MJAHR,:V_SPMON,:V_SDATE, :V_MODEL, :V_PRODQTY
*      FROM AUSP B, AUSP C, AUSP D, AUSP A
*     WHERE A.MANDT    = :sy-mandt
*       AND A.ATINN    = :V_SOFF
*       AND A.ATFLV    BETWEEN :V_D1DAT AND :I_SDATE
*       AND A.KLART    = '002'
*       AND B.MANDT(+) =  A.MANDT
*       AND B.OBJEK(+) =  A.OBJEK
*       AND B.ATINN(+) = :V_MODEL_ATN
*       AND B.MAFID(+) =  A.MAFID
*       AND B.KLART(+) =  A.KLART
*       AND C.MANDT =  A.MANDT
*       AND C.OBJEK =  A.OBJEK
*       AND C.ATINN = :v_worder_ATN
*       AND C.MAFID =  A.MAFID
*       AND C.KLART =  A.KLART
*       AND NOT (C.ATWRT LIKE '%XX%' OR C.ATWRT LIKE '%XY%')
*       AND D.MANDT =  A.MANDT
*       AND D.OBJEK =  A.OBJEK
*       AND D.ATINN = :V_USAGE_ATN
*       AND D.MAFID =  A.MAFID
*       AND D.KLART =  A.KLART
*       AND D.ATWRT NOT IN ('S','D')
*     GROUP BY SUBSTR(TO_NCHAR(A.ATFLV),1,4),
*              SUBSTR(TO_NCHAR(A.ATFLV),1,6),
*              A.ATFLV, B.ATWRT
*  ENDEXEC.

  CLEAR: v_mjahr, v_spmon, v_sdate, v_model, v_prodqty,
         v_dealer, v_usage.

*  EXEC SQL PERFORMING append_prod_result.
*    SELECT
*           SUBSTR(TO_NCHAR(A.ATFLV),1,4),
*           SUBSTR(TO_NCHAR(A.ATFLV),1,6),
*           B.ATWRT, SUBSTR(C.ATWRT,12,2), D.ATWRT, COUNT(*)
*      INTO :V_MJAHR,:V_SPMON,:V_MODEL, :V_DEALER, :V_USAGE, :V_PRODQTY
**      INTO :V_MODEL, :V_DEALER, :V_USAGE, :V_PRODQTY
*      FROM AUSP B, AUSP C, AUSP D, AUSP A
*     WHERE A.MANDT    = :sy-mandt
*       AND A.ATINN    = :V_SOFF
*       AND A.ATFLV    BETWEEN :V_MON_1ST AND :V_MON_END
*       AND A.KLART    = '002'
*       AND B.MANDT =  A.MANDT
*       AND B.OBJEK =  A.OBJEK
*       AND B.ATINN = :V_MODEL_ATN
*       AND B.MAFID =  A.MAFID
*       AND B.KLART =  A.KLART
*       AND C.MANDT =  A.MANDT
*       AND C.OBJEK =  A.OBJEK
*       AND C.ATINN = :v_worder_ATN
*       AND C.MAFID =  A.MAFID
*       AND C.KLART =  A.KLART
*       AND D.MANDT =  A.MANDT
*       AND D.OBJEK =  A.OBJEK
*       AND D.ATINN = :V_USAGE_ATN
*       AND D.MAFID =  A.MAFID
*       AND D.KLART =  A.KLART
*     GROUP BY SUBSTR(TO_NCHAR(A.ATFLV),1,4),
*              SUBSTR(TO_NCHAR(A.ATFLV),1,6),
*              B.ATWRT,  SUBSTR(C.ATWRT,12,2), D.ATWRT

*    SELECT SUBSTR(TO_NCHAR(A.ATFLV),1,4),SUBSTR(TO_NCHAR(A.ATFLV),1,6),
*           B.ATWRT, COUNT(*)
*      INTO :V_MJAHR,:V_SPMON,:V_MODEL,:V_PRODQTY
*      FROM AUSP B, AUSP C, AUSP D, AUSP A
*     WHERE A.MANDT    = :sy-mandt
*       AND A.ATINN    = :V_SOFF
*       AND A.ATFLV    BETWEEN :V_MON_1ST AND :V_MON_END
*       AND A.KLART    = '002'
*       AND B.MANDT(+) =  A.MANDT
*       AND B.OBJEK(+) =  A.OBJEK
*       AND B.ATINN(+) = :V_MODEL_ATN
*       AND B.MAFID(+) =  A.MAFID
*       AND B.KLART(+) =  A.KLART
*       AND C.MANDT =  A.MANDT
*       AND C.OBJEK =  A.OBJEK
*       AND C.ATINN = :v_worder_ATN
*       AND C.MAFID =  A.MAFID
*       AND C.KLART =  A.KLART
*       AND NOT (C.ATWRT LIKE '%XX%' OR C.ATWRT LIKE '%XY%')
*       AND D.MANDT =  A.MANDT
*       AND D.OBJEK =  A.OBJEK
*       AND D.ATINN = :V_USAGE_ATN
*       AND D.MAFID =  A.MAFID
*       AND D.KLART =  A.KLART
*       AND D.ATWRT NOT IN ('S','D')
*     GROUP BY SUBSTR(TO_NCHAR(A.ATFLV),1,4),
*              SUBSTR(TO_NCHAR(A.ATFLV),1,6),
*              B.ATWRT
*  ENDEXEC.
*
*
*  DO 12 TIMES.
*    CLEAR: v_mjahr, v_spmon, v_sdate, v_model, v_prodqty,
*           v_dealer, v_usage.
*
*    IF sy-index EQ 1.
*      v_mon_1st = v_year_1st.
*    ELSE.
*      v_mon_1st = v_mon_end + 1.
*    ENDIF.
*
*    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
*      EXPORTING
*        day_in            = v_mon_1st
*      IMPORTING
*        last_day_of_month = v_mon_end
*      EXCEPTIONS
*        day_in_no_date    = 1
*        OTHERS            = 2.
*    IF sy-subrc <> 0. EXIT. ENDIF.
*
*
*    EXEC SQL PERFORMING append_prod_result.
*      SELECT SUBSTR(TO_NCHAR(A.ATFLV),1,4),
*             B.ATWRT, SUBSTR(C.ATWRT,12,2), D.ATWRT, COUNT(*)
*        INTO :V_MJAHR,:V_MODEL, :V_DEALER, :V_USAGE, :V_PRODQTY
*        FROM AUSP B, AUSP C, AUSP D, AUSP A
**      FROM AUSP B, AUSP C,AUSP A
*       WHERE A.MANDT    = :sy-mandt
*         AND A.ATINN    = :V_SOFF
*         AND A.ATFLV    BETWEEN :V_MON_1ST AND :V_MON_END
*         AND A.KLART    = '002'
*         AND B.MANDT =  A.MANDT
*         AND B.OBJEK =  A.OBJEK
*         AND B.ATINN = :V_MODEL_ATN
*         AND B.MAFID =  A.MAFID
*         AND B.KLART =  A.KLART
*         AND C.MANDT =  A.MANDT
*         AND C.OBJEK =  A.OBJEK
*         AND C.ATINN = :v_worder_ATN
*         AND C.MAFID =  A.MAFID
*         AND C.KLART =  A.KLART
*         AND D.MANDT =  A.MANDT
*         AND D.OBJEK =  A.OBJEK
*         AND D.ATINN = :V_USAGE_ATN
*         AND D.MAFID =  A.MAFID
*         AND D.KLART =  A.KLART
*       GROUP BY SUBSTR(TO_NCHAR(A.ATFLV),1,4),
*                B.ATWRT,  SUBSTR(C.ATWRT,12,2), D.ATWRT
*    ENDEXEC.
*  ENDDO.

  CLEAR: v_mjahr, v_spmon, v_sdate, v_model, v_prodqty,
         v_dealer, v_usage.

*  EXEC SQL PERFORMING append_prod_result.
*    SELECT SUBSTR(TO_NCHAR(A.ATFLV),1,4),
*           B.ATWRT, SUBSTR(C.ATWRT,12,2), D.ATWRT, COUNT(*)
*      INTO :V_MJAHR,:V_MODEL, :V_DEALER, :V_USAGE, :V_PRODQTY
*      FROM AUSP B, AUSP C, AUSP D, AUSP A
**      FROM AUSP B, AUSP C,AUSP A
*     WHERE A.MANDT    = :sy-mandt
*       AND A.ATINN    = :V_SOFF
*       AND A.ATFLV    BETWEEN :V_YEAR_1ST AND :V_YEAR_END
*       AND A.KLART    = '002'
*       AND B.MANDT =  A.MANDT
*       AND B.OBJEK =  A.OBJEK
*       AND B.ATINN = :V_MODEL_ATN
*       AND B.MAFID =  A.MAFID
*       AND B.KLART =  A.KLART
*       AND C.MANDT =  B.MANDT
*       AND C.OBJEK =  B.OBJEK
*       AND C.ATINN = :v_worder_ATN
*       AND C.MAFID =  B.MAFID
*       AND C.KLART =  B.KLART
*       AND D.MANDT =  C.MANDT
*       AND D.OBJEK =  C.OBJEK
*       AND D.ATINN = :V_USAGE_ATN
*       AND D.MAFID =  C.MAFID
*       AND D.KLART =  C.KLART
**     GROUP BY SUBSTR(TO_NCHAR(A.ATFLV),1,4),
**              B.ATWRT,  SUBSTR(C.ATWRT,12,2), D.ATWRT
*  ENDEXEC.

*  EXEC SQL PERFORMING append_prod_result.
*    SELECT B.ATWRT, COUNT(*)
*      INTO :V_DEALER,:V_PRODQTY
**      FROM AUSP B, AUSP C, AUSP D, AUSP A
*      FROM AUSP B, AUSP A
*     WHERE A.MANDT    = :sy-mandt
*       AND A.ATINN    = :V_SOFF
*       AND A.ATFLV    BETWEEN :V_YEAR_1ST AND :V_YEAR_END
*       AND A.KLART    = '002'
*       AND B.MANDT =  A.MANDT
*       AND B.OBJEK =  A.OBJEK
*       AND B.ATINN = :v_worder_ATN
*       AND B.MAFID =  A.MAFID
*       AND B.KLART =  A.KLART
*
*
*
**       AND B.MANDT =  A.MANDT
**       AND B.OBJEK =  A.OBJEK
**       AND B.ATINN = :V_MODEL_ATN
**       AND B.MAFID =  A.MAFID
**       AND B.KLART =  A.KLART
**       AND D.MANDT =  C.MANDT
**       AND D.OBJEK =  C.OBJEK
**       AND D.ATINN = :V_USAGE_ATN
**       AND D.MAFID =  C.MAFID
**       AND D.KLART =  C.KLART
*     GROUP BY B.ATWRT
*  ENDEXEC.

  DATA: L_ATFLV_F LIKE AUSP-ATFLV,
        L_ATFLV_T LIKE AUSP-ATFLV.

  L_ATFLV_F = V_YEAR_1ST.
  L_ATFLV_T = V_YEAR_END.

*  SELECT COUNT(*)
*    INTO V_PRODQTY
*    FROM AUSP AS A INNER JOIN AUSP AS B
*                      ON B~MANDT = A~MANDT
*                     AND B~OBJEK = A~OBJEK
*                     AND B~ATINN = V_WORDER_ATN
*                     AND B~MAFID = A~MAFID
*                     AND B~KLART = A~KLART
*     WHERE A~ATINN    = V_SOFF
*       AND A~ATFLV    BETWEEN L_ATFLV_F AND L_ATFLV_T
*       AND A~KLART    = '002'.

  EXEC SQL PERFORMING append_prod_result.
    SELECT COUNT(*)
      INTO :V_PRODQTY
      FROM AUSP B, ( SELECT OBJEK, ATZHL, MAFID, KLART
                       FROM AUSP
                      WHERE ATINN    = :V_SOFF
                        AND KLART    = '002'
                        AND ATFLV    BETWEEN :L_ATFLV_F AND :L_ATFLV_T)
                    A
     WHERE B.MANDT =  :SY-MANDT
       AND B.OBJEK =  A.OBJEK
       AND B.ATINN = :v_worder_ATN
       AND B.MAFID =  A.MAFID
       AND B.KLART =  A.KLART

  ENDEXEC.

  EXEC SQL PERFORMING append_prod_result.
    SELECT COUNT(*)
      INTO :V_PRODQTY
*      FROM AUSP B, AUSP C, AUSP D, AUSP A
      FROM AUSP B, AUSP A
     WHERE A.MANDT    = :sy-mandt
       AND A.ATINN    = :V_SOFF
       AND A.KLART    = '002'
*       AND A.ATFLV    LIKE '2010%'
       AND A.ATFLV    BETWEEN :L_ATFLV_F AND :L_ATFLV_T
       AND B.MANDT =  A.MANDT
       AND B.OBJEK =  A.OBJEK
       AND B.ATINN = :v_worder_ATN
       AND B.ATZHL =  A.ATZHL
       AND B.MAFID =  A.MAFID
       AND B.KLART =  A.KLART
       AND B.ADZHL =  A.ADZHL



*       AND B.MANDT =  A.MANDT
*       AND B.OBJEK =  A.OBJEK
*       AND B.ATINN = :V_MODEL_ATN
*       AND B.MAFID =  A.MAFID
*       AND B.KLART =  A.KLART
*       AND D.MANDT =  C.MANDT
*       AND D.OBJEK =  C.OBJEK
*       AND D.ATINN = :V_USAGE_ATN
*       AND D.MAFID =  C.MAFID
*       AND D.KLART =  C.KLART
*     GROUP BY B.ATWRT
  ENDEXEC.
  t_result[] = it_result[].

ENDFUNCTION.

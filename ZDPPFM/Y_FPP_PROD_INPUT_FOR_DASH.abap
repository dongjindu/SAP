FUNCTION y_fpp_prod_input_for_dash.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_SDATE) TYPE  ZSDATE
*"  TABLES
*"      T_INPUT STRUCTURE  YSPP_PROD_INPUT
*"----------------------------------------------------------------------


  SELECT SINGLE atinn INTO v_worder_atn
    FROM cabn WHERE atnam = 'P_WORK_ORDER'.

  SELECT SINGLE atinn INTO v_usage_atn
    FROM cabn WHERE atnam = 'P_USAGE_CAR'.

  SELECT SINGLE atinn INTO v_model_atn
    FROM cabn WHERE atnam = 'P_MODEL'.

  SELECT SINGLE atinn INTO v_bin
    FROM cabn WHERE atnam = 'P_RP01_SHOP_DATE'.

  SELECT SINGLE atinn INTO v_bin_act
    FROM cabn WHERE atnam = 'P_RP01_ACTUAL_DATE'.

  SELECT SINGLE atinn INTO v_pin
    FROM cabn WHERE atnam = 'P_RP02_SHOP_DATE'.

  SELECT SINGLE atinn INTO v_pin_act
    FROM cabn WHERE atnam = 'P_RP02_ACTUAL_DATE'.

  SELECT SINGLE atinn INTO v_pout
    FROM cabn WHERE atnam = 'P_RP06_SHOP_DATE'.

  SELECT SINGLE atinn INTO v_pout_act
    FROM cabn WHERE atnam = 'P_RP06_ACTUAL_DATE'.

  SELECT SINGLE atinn INTO v_tin
    FROM cabn WHERE atnam = 'P_RP07_SHOP_DATE'.

  SELECT SINGLE atinn INTO v_tin_act
    FROM cabn WHERE atnam = 'P_RP07_ACTUAL_DATE'.

  SELECT SINGLE atinn INTO v_lout
    FROM cabn WHERE atnam = 'P_RP17_SHOP_DATE'.

  SELECT SINGLE atinn INTO v_lout_act
    FROM cabn WHERE atnam = 'P_RP17_ACTUAL_DATE'.

  SELECT SINGLE atinn INTO v_soff
    FROM cabn WHERE atnam = 'P_RP18_SHOP_DATE'.

  SELECT SINGLE atinn INTO v_soff_act
    FROM cabn WHERE atnam = 'P_RP18_ACTUAL_DATE'.

  v_d1dat = i_sdate - 1.

  REFRESH: it_input.

  EXEC SQL PERFORMING append_prod_input.
*---// Body-in
    SELECT A.ATFLV, E.ATWRT, '01', '1', COUNT(*)
      INTO :V_SDATE, :V_MODEL, :V_ZRPID, :V_SHIFT, :V_INQTY
      FROM AUSP B, AUSP C, AUSP D, AUSP E, AUSP A
     WHERE A.MANDT    = :sy-mandt
       AND A.ATINN    = :V_BIN
       AND A.ATFLV    BETWEEN :V_D1DAT AND :I_SDATE
       AND A.KLART    = '002'
       AND B.MANDT(+) =  A.MANDT
       AND B.OBJEK(+) =  A.OBJEK
       AND B.ATINN(+) = :V_BIN_ACT
       AND B.MAFID(+) =  A.MAFID
       AND B.KLART(+) =  A.KLART
       AND B.ATWRT <  TO_NCHAR(A.ATFLV) || :C_1ST_SHIFT
       AND C.MANDT =  A.MANDT
       AND C.OBJEK =  A.OBJEK
       AND C.ATINN = :v_worder_ATN
       AND C.MAFID =  A.MAFID
       AND C.KLART =  A.KLART
       AND NOT (C.ATWRT LIKE '%XX%' OR C.ATWRT LIKE '%XY%')
       AND D.MANDT =  A.MANDT
       AND D.OBJEK =  A.OBJEK
       AND D.ATINN = :V_USAGE_ATN
       AND D.MAFID =  A.MAFID
       AND D.KLART =  A.KLART
       AND D.ATWRT NOT IN ('S','D')
       AND E.MANDT =  A.MANDT
       AND E.OBJEK =  A.OBJEK
       AND E.ATINN = :V_MODEL_ATN
       AND E.MAFID =  A.MAFID
       AND E.KLART =  A.KLART
     GROUP BY A.ATFLV, E.ATWRT
     UNION
    SELECT A.ATFLV, E.ATWRT, '01', '2', COUNT(*)
      FROM AUSP B, AUSP C, AUSP D, AUSP E, AUSP A
     WHERE A.MANDT    = :sy-mandt
       AND A.ATINN    = :V_BIN
       AND A.ATFLV    BETWEEN :V_D1DAT AND :I_SDATE
       AND A.KLART    = '002'
       AND B.MANDT(+) =  A.MANDT
       AND B.OBJEK(+) =  A.OBJEK
       AND B.ATINN(+) =  :V_BIN_ACT
       AND B.MAFID(+) =  A.MAFID
       AND B.KLART(+) =  A.KLART
       AND B.ATWRT    >= TO_NCHAR(A.ATFLV) || :C_1ST_SHIFT
       AND B.ATWRT    <  TO_NCHAR(A.ATFLV) || :C_2ND_SHIFT
       AND C.MANDT =  A.MANDT
       AND C.OBJEK =  A.OBJEK
       AND C.ATINN = :v_worder_ATN
       AND C.MAFID =  A.MAFID
       AND C.KLART =  A.KLART
       AND NOT (C.ATWRT LIKE '%XX%' OR C.ATWRT LIKE '%XY%')
       AND D.MANDT =  A.MANDT
       AND D.OBJEK =  A.OBJEK
       AND D.ATINN = :V_USAGE_ATN
       AND D.MAFID =  A.MAFID
       AND D.KLART =  A.KLART
       AND D.ATWRT NOT IN ('S','D')
       AND E.MANDT =  A.MANDT
       AND E.OBJEK =  A.OBJEK
       AND E.ATINN = :V_MODEL_ATN
       AND E.MAFID =  A.MAFID
       AND E.KLART =  A.KLART
     GROUP BY A.ATFLV, E.ATWRT
     UNION
    SELECT A.ATFLV, E.ATWRT, '01', '3', COUNT(*)
      FROM AUSP B, AUSP C, AUSP D, AUSP E, AUSP A
     WHERE A.MANDT    = :sy-mandt
       AND A.ATINN    = :V_BIN
       AND A.ATFLV    BETWEEN :V_D1DAT AND :I_SDATE
       AND A.KLART    = '002'
       AND B.MANDT(+) =  A.MANDT
       AND B.OBJEK(+) =  A.OBJEK
       AND B.ATINN(+) =  :V_BIN_ACT
       AND B.MAFID(+) =  A.MAFID
       AND B.KLART(+) =  A.KLART
       AND B.ATWRT    >= TO_NCHAR(A.ATFLV) || :C_2ND_SHIFT
       AND C.MANDT =  A.MANDT
       AND C.OBJEK =  A.OBJEK
       AND C.ATINN = :v_worder_ATN
       AND C.MAFID =  A.MAFID
       AND C.KLART =  A.KLART
       AND NOT (C.ATWRT LIKE '%XX%' OR C.ATWRT LIKE '%XY%')
       AND D.MANDT =  A.MANDT
       AND D.OBJEK =  A.OBJEK
       AND D.ATINN = :V_USAGE_ATN
       AND D.MAFID =  A.MAFID
       AND D.KLART =  A.KLART
       AND D.ATWRT NOT IN ('S','D')
       AND E.MANDT =  A.MANDT
       AND E.OBJEK =  A.OBJEK
       AND E.ATINN = :V_MODEL_ATN
       AND E.MAFID =  A.MAFID
       AND E.KLART =  A.KLART
     GROUP BY A.ATFLV, E.ATWRT

*---// Paint-in
    UNION
    SELECT A.ATFLV, E.ATWRT, '02', '1', COUNT(*)
      FROM AUSP B, AUSP C, AUSP D, AUSP E, AUSP A
     WHERE A.MANDT    = :sy-mandt
       AND A.ATINN    = :V_PIN
       AND A.ATFLV    BETWEEN :V_D1DAT AND :I_SDATE
       AND A.KLART    = '002'
       AND B.MANDT(+) =  A.MANDT
       AND B.OBJEK(+) =  A.OBJEK
       AND B.ATINN(+) = :V_PIN_ACT
       AND B.MAFID(+) =  A.MAFID
       AND B.KLART(+) =  A.KLART
       AND B.ATWRT <  TO_NCHAR(A.ATFLV) || :C_1ST_SHIFT
       AND C.MANDT =  A.MANDT
       AND C.OBJEK =  A.OBJEK
       AND C.ATINN = :v_worder_ATN
       AND C.MAFID =  A.MAFID
       AND C.KLART =  A.KLART
       AND NOT (C.ATWRT LIKE '%XX%' OR C.ATWRT LIKE '%XY%')
       AND D.MANDT =  A.MANDT
       AND D.OBJEK =  A.OBJEK
       AND D.ATINN = :V_USAGE_ATN
       AND D.MAFID =  A.MAFID
       AND D.KLART =  A.KLART
       AND D.ATWRT NOT IN ('S','D')
       AND E.MANDT =  A.MANDT
       AND E.OBJEK =  A.OBJEK
       AND E.ATINN = :V_MODEL_ATN
       AND E.MAFID =  A.MAFID
       AND E.KLART =  A.KLART
     GROUP BY A.ATFLV, E.ATWRT
     UNION
    SELECT A.ATFLV, E.ATWRT, '02', '2', COUNT(*)
      FROM AUSP B, AUSP C, AUSP D, AUSP E, AUSP A
     WHERE A.MANDT    = :sy-mandt
       AND A.ATINN    = :V_PIN
       AND A.ATFLV    BETWEEN :V_D1DAT AND :I_SDATE
       AND A.KLART    = '002'
       AND B.MANDT(+) =  A.MANDT
       AND B.OBJEK(+) =  A.OBJEK
       AND B.ATINN(+) =  :V_PIN_ACT
       AND B.MAFID(+) =  A.MAFID
       AND B.KLART(+) =  A.KLART
       AND B.ATWRT    >= TO_NCHAR(A.ATFLV) || :C_1ST_SHIFT
       AND B.ATWRT    <  TO_NCHAR(A.ATFLV) || :C_2ND_SHIFT
       AND C.MANDT =  A.MANDT
       AND C.OBJEK =  A.OBJEK
       AND C.ATINN = :v_worder_ATN
       AND C.MAFID =  A.MAFID
       AND C.KLART =  A.KLART
       AND NOT (C.ATWRT LIKE '%XX%' OR C.ATWRT LIKE '%XY%')
       AND D.MANDT =  A.MANDT
       AND D.OBJEK =  A.OBJEK
       AND D.ATINN = :V_USAGE_ATN
       AND D.MAFID =  A.MAFID
       AND D.KLART =  A.KLART
       AND D.ATWRT NOT IN ('S','D')
       AND E.MANDT =  A.MANDT
       AND E.OBJEK =  A.OBJEK
       AND E.ATINN = :V_MODEL_ATN
       AND E.MAFID =  A.MAFID
       AND E.KLART =  A.KLART
     GROUP BY A.ATFLV, E.ATWRT
     UNION
    SELECT A.ATFLV, E.ATWRT, '02', '3', COUNT(*)
      FROM AUSP B, AUSP C, AUSP D, AUSP E, AUSP A
     WHERE A.MANDT    = :sy-mandt
       AND A.ATINN    = :V_PIN
       AND A.ATFLV    BETWEEN :V_D1DAT AND :I_SDATE
       AND A.KLART    = '002'
       AND B.MANDT(+) =  A.MANDT
       AND B.OBJEK(+) =  A.OBJEK
       AND B.ATINN(+) =  :V_PIN_ACT
       AND B.MAFID(+) =  A.MAFID
       AND B.KLART(+) =  A.KLART
       AND B.ATWRT    >= TO_NCHAR(A.ATFLV) || :C_2ND_SHIFT
       AND C.MANDT =  A.MANDT
       AND C.OBJEK =  A.OBJEK
       AND C.ATINN = :v_worder_ATN
       AND C.MAFID =  A.MAFID
       AND C.KLART =  A.KLART
       AND NOT (C.ATWRT LIKE '%XX%' OR C.ATWRT LIKE '%XY%')
       AND D.MANDT =  A.MANDT
       AND D.OBJEK =  A.OBJEK
       AND D.ATINN = :V_USAGE_ATN
       AND D.MAFID =  A.MAFID
       AND D.KLART =  A.KLART
       AND D.ATWRT NOT IN ('S','D')
       AND E.MANDT =  A.MANDT
       AND E.OBJEK =  A.OBJEK
       AND E.ATINN = :V_MODEL_ATN
       AND E.MAFID =  A.MAFID
       AND E.KLART =  A.KLART
     GROUP BY A.ATFLV, E.ATWRT

*---// PBS-out
    UNION
    SELECT A.ATFLV, E.ATWRT, '06', '1', COUNT(*)
      FROM AUSP B, AUSP C, AUSP D, AUSP E, AUSP A
     WHERE A.MANDT    = :sy-mandt
       AND A.ATINN    = :V_POUT
       AND A.ATFLV    BETWEEN :V_D1DAT AND :I_SDATE
       AND A.KLART    = '002'
       AND B.MANDT(+) =  A.MANDT
       AND B.OBJEK(+) =  A.OBJEK
       AND B.ATINN(+) = :V_POUT_ACT
       AND B.MAFID(+) =  A.MAFID
       AND B.KLART(+) =  A.KLART
       AND B.ATWRT <  TO_NCHAR(A.ATFLV) || :C_1ST_SHIFT
       AND C.MANDT =  A.MANDT
       AND C.OBJEK =  A.OBJEK
       AND C.ATINN = :v_worder_ATN
       AND C.MAFID =  A.MAFID
       AND C.KLART =  A.KLART
       AND NOT (C.ATWRT LIKE '%XX%' OR C.ATWRT LIKE '%XY%')
       AND D.MANDT =  A.MANDT
       AND D.OBJEK =  A.OBJEK
       AND D.ATINN = :V_USAGE_ATN
       AND D.MAFID =  A.MAFID
       AND D.KLART =  A.KLART
       AND D.ATWRT NOT IN ('S','D')
       AND E.MANDT =  A.MANDT
       AND E.OBJEK =  A.OBJEK
       AND E.ATINN = :V_MODEL_ATN
       AND E.MAFID =  A.MAFID
       AND E.KLART =  A.KLART
     GROUP BY A.ATFLV, E.ATWRT
     UNION
    SELECT A.ATFLV, E.ATWRT, '06', '2', COUNT(*)
      FROM AUSP B, AUSP C, AUSP D, AUSP E, AUSP A
     WHERE A.MANDT    = :sy-mandt
       AND A.ATINN    = :V_POUT
       AND A.ATFLV    BETWEEN :V_D1DAT AND :I_SDATE
       AND A.KLART    = '002'
       AND B.MANDT(+) =  A.MANDT
       AND B.OBJEK(+) =  A.OBJEK
       AND B.ATINN(+) =  :V_POUT_ACT
       AND B.MAFID(+) =  A.MAFID
       AND B.KLART(+) =  A.KLART
       AND B.ATWRT    >= TO_NCHAR(A.ATFLV) || :C_1ST_SHIFT
       AND B.ATWRT    <  TO_NCHAR(A.ATFLV) || :C_2ND_SHIFT
       AND C.MANDT =  A.MANDT
       AND C.OBJEK =  A.OBJEK
       AND C.ATINN = :v_worder_ATN
       AND C.MAFID =  A.MAFID
       AND C.KLART =  A.KLART
       AND NOT (C.ATWRT LIKE '%XX%' OR C.ATWRT LIKE '%XY%')
       AND D.MANDT =  A.MANDT
       AND D.OBJEK =  A.OBJEK
       AND D.ATINN = :V_USAGE_ATN
       AND D.MAFID =  A.MAFID
       AND D.KLART =  A.KLART
       AND D.ATWRT NOT IN ('S','D')
       AND E.MANDT =  A.MANDT
       AND E.OBJEK =  A.OBJEK
       AND E.ATINN = :V_MODEL_ATN
       AND E.MAFID =  A.MAFID
       AND E.KLART =  A.KLART
     GROUP BY A.ATFLV, E.ATWRT
     UNION
    SELECT A.ATFLV, E.ATWRT, '06', '3', COUNT(*)
      FROM AUSP B, AUSP C, AUSP D, AUSP E, AUSP A
     WHERE A.MANDT    = :sy-mandt
       AND A.ATINN    = :V_POUT
       AND A.ATFLV    BETWEEN :V_D1DAT AND :I_SDATE
       AND A.KLART    = '002'
       AND B.MANDT(+) =  A.MANDT
       AND B.OBJEK(+) =  A.OBJEK
       AND B.ATINN(+) =  :V_POUT_ACT
       AND B.MAFID(+) =  A.MAFID
       AND B.KLART(+) =  A.KLART
       AND B.ATWRT    >= TO_NCHAR(A.ATFLV) || :C_2ND_SHIFT
       AND C.MANDT =  A.MANDT
       AND C.OBJEK =  A.OBJEK
       AND C.ATINN = :v_worder_ATN
       AND C.MAFID =  A.MAFID
       AND C.KLART =  A.KLART
       AND NOT (C.ATWRT LIKE '%XX%' OR C.ATWRT LIKE '%XY%')
       AND D.MANDT =  A.MANDT
       AND D.OBJEK =  A.OBJEK
       AND D.ATINN = :V_USAGE_ATN
       AND D.MAFID =  A.MAFID
       AND D.KLART =  A.KLART
       AND D.ATWRT NOT IN ('S','D')
       AND E.MANDT =  A.MANDT
       AND E.OBJEK =  A.OBJEK
       AND E.ATINN = :V_MODEL_ATN
       AND E.MAFID =  A.MAFID
       AND E.KLART =  A.KLART
     GROUP BY A.ATFLV, E.ATWRT

*---// Trim-in
    UNION
    SELECT A.ATFLV, E.ATWRT, '07', '1', COUNT(*)
      FROM AUSP B, AUSP C, AUSP D, AUSP E, AUSP A
     WHERE A.MANDT    = :sy-mandt
       AND A.ATINN    = :V_TIN
       AND A.ATFLV    BETWEEN :V_D1DAT AND :I_SDATE
       AND A.KLART    = '002'
       AND B.MANDT(+) =  A.MANDT
       AND B.OBJEK(+) =  A.OBJEK
       AND B.ATINN(+) = :V_TIN_ACT
       AND B.MAFID(+) =  A.MAFID
       AND B.KLART(+) =  A.KLART
       AND B.ATWRT <  TO_NCHAR(A.ATFLV) || :C_1ST_SHIFT
       AND C.MANDT =  A.MANDT
       AND C.OBJEK =  A.OBJEK
       AND C.ATINN = :v_worder_ATN
       AND C.MAFID =  A.MAFID
       AND C.KLART =  A.KLART
       AND NOT (C.ATWRT LIKE '%XX%' OR C.ATWRT LIKE '%XY%')
       AND D.MANDT =  A.MANDT
       AND D.OBJEK =  A.OBJEK
       AND D.ATINN = :V_USAGE_ATN
       AND D.MAFID =  A.MAFID
       AND D.KLART =  A.KLART
       AND D.ATWRT NOT IN ('S','D')
       AND E.MANDT =  A.MANDT
       AND E.OBJEK =  A.OBJEK
       AND E.ATINN = :V_MODEL_ATN
       AND E.MAFID =  A.MAFID
       AND E.KLART =  A.KLART
     GROUP BY A.ATFLV, E.ATWRT
     UNION
    SELECT A.ATFLV, E.ATWRT, '07', '2', COUNT(*)
      FROM AUSP B, AUSP C, AUSP D, AUSP E, AUSP A
     WHERE A.MANDT    = :sy-mandt
       AND A.ATINN    = :V_TIN
       AND A.ATFLV    BETWEEN :V_D1DAT AND :I_SDATE
       AND A.KLART    = '002'
       AND B.MANDT(+) =  A.MANDT
       AND B.OBJEK(+) =  A.OBJEK
       AND B.ATINN(+) =  :V_TIN_ACT
       AND B.MAFID(+) =  A.MAFID
       AND B.KLART(+) =  A.KLART
       AND B.ATWRT    >= TO_NCHAR(A.ATFLV) || :C_1ST_SHIFT
       AND B.ATWRT    <  TO_NCHAR(A.ATFLV) || :C_2ND_SHIFT
       AND C.MANDT =  A.MANDT
       AND C.OBJEK =  A.OBJEK
       AND C.ATINN = :v_worder_ATN
       AND C.MAFID =  A.MAFID
       AND C.KLART =  A.KLART
       AND NOT (C.ATWRT LIKE '%XX%' OR C.ATWRT LIKE '%XY%')
       AND D.MANDT =  A.MANDT
       AND D.OBJEK =  A.OBJEK
       AND D.ATINN = :V_USAGE_ATN
       AND D.MAFID =  A.MAFID
       AND D.KLART =  A.KLART
       AND D.ATWRT NOT IN ('S','D')
       AND E.MANDT =  A.MANDT
       AND E.OBJEK =  A.OBJEK
       AND E.ATINN = :V_MODEL_ATN
       AND E.MAFID =  A.MAFID
       AND E.KLART =  A.KLART
     GROUP BY A.ATFLV, E.ATWRT
     UNION
    SELECT A.ATFLV, E.ATWRT, '07', '3', COUNT(*)
      FROM AUSP B, AUSP C, AUSP D, AUSP E, AUSP A
     WHERE A.MANDT    = :sy-mandt
       AND A.ATINN    = :V_TIN
       AND A.ATFLV    BETWEEN :V_D1DAT AND :I_SDATE
       AND A.KLART    = '002'
       AND B.MANDT(+) =  A.MANDT
       AND B.OBJEK(+) =  A.OBJEK
       AND B.ATINN(+) =  :V_TIN_ACT
       AND B.MAFID(+) =  A.MAFID
       AND B.KLART(+) =  A.KLART
       AND B.ATWRT    >= TO_NCHAR(A.ATFLV) || :C_2ND_SHIFT
       AND C.MANDT =  A.MANDT
       AND C.OBJEK =  A.OBJEK
       AND C.ATINN = :v_worder_ATN
       AND C.MAFID =  A.MAFID
       AND C.KLART =  A.KLART
       AND NOT (C.ATWRT LIKE '%XX%' OR C.ATWRT LIKE '%XY%')
       AND D.MANDT =  A.MANDT
       AND D.OBJEK =  A.OBJEK
       AND D.ATINN = :V_USAGE_ATN
       AND D.MAFID =  A.MAFID
       AND D.KLART =  A.KLART
       AND D.ATWRT NOT IN ('S','D')
       AND E.MANDT =  A.MANDT
       AND E.OBJEK =  A.OBJEK
       AND E.ATINN = :V_MODEL_ATN
       AND E.MAFID =  A.MAFID
       AND E.KLART =  A.KLART
     GROUP BY A.ATFLV, E.ATWRT

*---// Line-out
    UNION
    SELECT A.ATFLV, E.ATWRT, '17', '1', COUNT(*)
      FROM AUSP B, AUSP C, AUSP D, AUSP E, AUSP A
     WHERE A.MANDT    = :sy-mandt
       AND A.ATINN    = :V_LOUT
       AND A.ATFLV    BETWEEN :V_D1DAT AND :I_SDATE
       AND A.KLART    = '002'
       AND B.MANDT(+) =  A.MANDT
       AND B.OBJEK(+) =  A.OBJEK
       AND B.ATINN(+) = :V_LOUT_ACT
       AND B.MAFID(+) =  A.MAFID
       AND B.KLART(+) =  A.KLART
       AND B.ATWRT <  TO_NCHAR(A.ATFLV) || :C_1ST_SHIFT
       AND C.MANDT =  A.MANDT
       AND C.OBJEK =  A.OBJEK
       AND C.ATINN = :v_worder_ATN
       AND C.MAFID =  A.MAFID
       AND C.KLART =  A.KLART
       AND NOT (C.ATWRT LIKE '%XX%' OR C.ATWRT LIKE '%XY%')
       AND D.MANDT =  A.MANDT
       AND D.OBJEK =  A.OBJEK
       AND D.ATINN = :V_USAGE_ATN
       AND D.MAFID =  A.MAFID
       AND D.KLART =  A.KLART
       AND D.ATWRT NOT IN ('S','D')
       AND E.MANDT =  A.MANDT
       AND E.OBJEK =  A.OBJEK
       AND E.ATINN = :V_MODEL_ATN
       AND E.MAFID =  A.MAFID
       AND E.KLART =  A.KLART
     GROUP BY A.ATFLV, E.ATWRT
     UNION
    SELECT A.ATFLV, E.ATWRT, '17', '2', COUNT(*)
      FROM AUSP B, AUSP C, AUSP D, AUSP E, AUSP A
     WHERE A.MANDT    = :sy-mandt
       AND A.ATINN    = :V_LOUT
       AND A.ATFLV    BETWEEN :V_D1DAT AND :I_SDATE
       AND A.KLART    = '002'
       AND B.MANDT(+) =  A.MANDT
       AND B.OBJEK(+) =  A.OBJEK
       AND B.ATINN(+) =  :V_LOUT_ACT
       AND B.MAFID(+) =  A.MAFID
       AND B.KLART(+) =  A.KLART
       AND B.ATWRT    >= TO_NCHAR(A.ATFLV) || :C_1ST_SHIFT
       AND B.ATWRT    <  TO_NCHAR(A.ATFLV) || :C_2ND_SHIFT
       AND C.MANDT =  A.MANDT
       AND C.OBJEK =  A.OBJEK
       AND C.ATINN = :v_worder_ATN
       AND C.MAFID =  A.MAFID
       AND C.KLART =  A.KLART
       AND NOT (C.ATWRT LIKE '%XX%' OR C.ATWRT LIKE '%XY%')
       AND D.MANDT =  A.MANDT
       AND D.OBJEK =  A.OBJEK
       AND D.ATINN = :V_USAGE_ATN
       AND D.MAFID =  A.MAFID
       AND D.KLART =  A.KLART
       AND D.ATWRT NOT IN ('S','D')
       AND E.MANDT =  A.MANDT
       AND E.OBJEK =  A.OBJEK
       AND E.ATINN = :V_MODEL_ATN
       AND E.MAFID =  A.MAFID
       AND E.KLART =  A.KLART
     GROUP BY A.ATFLV, E.ATWRT
     UNION
    SELECT A.ATFLV, E.ATWRT, '17', '3', COUNT(*)
      FROM AUSP B, AUSP C, AUSP D, AUSP E, AUSP A
     WHERE A.MANDT    = :sy-mandt
       AND A.ATINN    = :V_LOUT
       AND A.ATFLV    BETWEEN :V_D1DAT AND :I_SDATE
       AND A.KLART    = '002'
       AND B.MANDT(+) =  A.MANDT
       AND B.OBJEK(+) =  A.OBJEK
       AND B.ATINN(+) =  :V_LOUT_ACT
       AND B.MAFID(+) =  A.MAFID
       AND B.KLART(+) =  A.KLART
       AND B.ATWRT    >= TO_NCHAR(A.ATFLV) || :C_2ND_SHIFT
       AND C.MANDT =  A.MANDT
       AND C.OBJEK =  A.OBJEK
       AND C.ATINN = :v_worder_ATN
       AND C.MAFID =  A.MAFID
       AND C.KLART =  A.KLART
       AND NOT (C.ATWRT LIKE '%XX%' OR C.ATWRT LIKE '%XY%')
       AND D.MANDT =  A.MANDT
       AND D.OBJEK =  A.OBJEK
       AND D.ATINN = :V_USAGE_ATN
       AND D.MAFID =  A.MAFID
       AND D.KLART =  A.KLART
       AND D.ATWRT NOT IN ('S','D')
       AND E.MANDT =  A.MANDT
       AND E.OBJEK =  A.OBJEK
       AND E.ATINN = :V_MODEL_ATN
       AND E.MAFID =  A.MAFID
       AND E.KLART =  A.KLART
     GROUP BY A.ATFLV, E.ATWRT

*---// Sign-off
    UNION
    SELECT A.ATFLV, E.ATWRT, '18', '1', COUNT(*)
      FROM AUSP B, AUSP C, AUSP D, AUSP E, AUSP A
     WHERE A.MANDT    = :sy-mandt
       AND A.ATINN    = :V_SOFF
       AND A.ATFLV    BETWEEN :V_D1DAT AND :I_SDATE
       AND A.KLART    = '002'
       AND B.MANDT(+) =  A.MANDT
       AND B.OBJEK(+) =  A.OBJEK
       AND B.ATINN(+) = :V_SOFF_ACT
       AND B.MAFID(+) =  A.MAFID
       AND B.KLART(+) =  A.KLART
       AND B.ATWRT <  TO_NCHAR(A.ATFLV) || :C_1ST_SHIFT
       AND C.MANDT =  A.MANDT
       AND C.OBJEK =  A.OBJEK
       AND C.ATINN = :v_worder_ATN
       AND C.MAFID =  A.MAFID
       AND C.KLART =  A.KLART
       AND NOT (C.ATWRT LIKE '%XX%' OR C.ATWRT LIKE '%XY%')
       AND D.MANDT =  A.MANDT
       AND D.OBJEK =  A.OBJEK
       AND D.ATINN = :V_USAGE_ATN
       AND D.MAFID =  A.MAFID
       AND D.KLART =  A.KLART
       AND D.ATWRT NOT IN ('S','D')
       AND E.MANDT =  A.MANDT
       AND E.OBJEK =  A.OBJEK
       AND E.ATINN = :V_MODEL_ATN
       AND E.MAFID =  A.MAFID
       AND E.KLART =  A.KLART
     GROUP BY A.ATFLV, E.ATWRT
     UNION
    SELECT A.ATFLV, E.ATWRT, '18', '2', COUNT(*)
      FROM AUSP B, AUSP C, AUSP D, AUSP E, AUSP A
     WHERE A.MANDT    = :sy-mandt
       AND A.ATINN    = :V_SOFF
       AND A.ATFLV    BETWEEN :V_D1DAT AND :I_SDATE
       AND A.KLART    = '002'
       AND B.MANDT(+) =  A.MANDT
       AND B.OBJEK(+) =  A.OBJEK
       AND B.ATINN(+) =  :V_SOFF_ACT
       AND B.MAFID(+) =  A.MAFID
       AND B.KLART(+) =  A.KLART
       AND B.ATWRT    >= TO_NCHAR(A.ATFLV) || :C_1ST_SHIFT
       AND B.ATWRT    <  TO_NCHAR(A.ATFLV) || :C_2ND_SHIFT
       AND C.MANDT =  A.MANDT
       AND C.OBJEK =  A.OBJEK
       AND C.ATINN = :v_worder_ATN
       AND C.MAFID =  A.MAFID
       AND C.KLART =  A.KLART
       AND NOT (C.ATWRT LIKE '%XX%' OR C.ATWRT LIKE '%XY%')
       AND D.MANDT =  A.MANDT
       AND D.OBJEK =  A.OBJEK
       AND D.ATINN = :V_USAGE_ATN
       AND D.MAFID =  A.MAFID
       AND D.KLART =  A.KLART
       AND D.ATWRT NOT IN ('S','D')
       AND E.MANDT =  A.MANDT
       AND E.OBJEK =  A.OBJEK
       AND E.ATINN = :V_MODEL_ATN
       AND E.MAFID =  A.MAFID
       AND E.KLART =  A.KLART
     GROUP BY A.ATFLV, E.ATWRT
     UNION
    SELECT A.ATFLV, E.ATWRT, '18', '3', COUNT(*)
      FROM AUSP B, AUSP C, AUSP D, AUSP E, AUSP A
     WHERE A.MANDT    = :sy-mandt
       AND A.ATINN    = :V_SOFF
       AND A.ATFLV    BETWEEN :V_D1DAT AND :I_SDATE
       AND A.KLART    = '002'
       AND B.MANDT(+) =  A.MANDT
       AND B.OBJEK(+) =  A.OBJEK
       AND B.ATINN(+) =  :V_SOFF_ACT
       AND B.MAFID(+) =  A.MAFID
       AND B.KLART(+) =  A.KLART
       AND B.ATWRT    >= TO_NCHAR(A.ATFLV) || :C_2ND_SHIFT
       AND C.MANDT =  A.MANDT
       AND C.OBJEK =  A.OBJEK
       AND C.ATINN = :v_worder_ATN
       AND C.MAFID =  A.MAFID
       AND C.KLART =  A.KLART
       AND NOT (C.ATWRT LIKE '%XX%' OR C.ATWRT LIKE '%XY%')
       AND D.MANDT =  A.MANDT
       AND D.OBJEK =  A.OBJEK
       AND D.ATINN = :V_USAGE_ATN
       AND D.MAFID =  A.MAFID
       AND D.KLART =  A.KLART
       AND D.ATWRT NOT IN ('S','D')
       AND E.MANDT =  A.MANDT
       AND E.OBJEK =  A.OBJEK
       AND E.ATINN = :V_MODEL_ATN
       AND E.MAFID =  A.MAFID
       AND E.KLART =  A.KLART
     GROUP BY A.ATFLV, E.ATWRT
  ENDEXEC.

  select *
    into CORRESPONDING FIELDS OF TABLE it_plan
    from ZTPP_PLAN_DAY
   where prdt_date BETWEEN V_D1DAT AND I_SDATE.

  LOOP AT IT_PLAN.
    CLEAR: IT_INPUT.

    MOVE: IT_PLAN-PRDT_DATE TO IT_INPUT-SDATE,
          IT_PLAN-MODEL     TO IT_INPUT-MODEL,
          IT_PLAN-QTY_PLAN  TO IT_INPUT-PLANQTY.

    APPEND IT_INPUT.
  ENDLOOP.

  t_input[] = it_input[].

ENDFUNCTION.

FUNCTION ZH_HR_GET_GHRSINFO_CHECK.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_BRACD) TYPE  NUMC08
*"     VALUE(IV_BRAAV) TYPE  CHAR20
*"     VALUE(IV_BUKRS) TYPE  BUKRS
*"     VALUE(IV_BEGDA) TYPE  BEGDA
*"     VALUE(IV_ENDDA) TYPE  ENDDA
*"  EXPORTING
*"     VALUE(EV_CNT) TYPE  I
*"----------------------------------------------------------------------

  DATA:
            lt_pa0000     TYPE TABLE OF pa0000 WITH HEADER LINE,
            lt_pa9883     TYPE TABLE OF pa9883 WITH HEADER LINE.


  SELECT DISTINCT m~pernr FROM pa0000 AS m
    INNER JOIN pa0001 AS r
      ON m~pernr = r~pernr
      AND r~bukrs = iv_bukrs
    INTO CORRESPONDING FIELDS OF TABLE lt_pa0000
    WHERE   m~stat2 <> 0
          AND   m~begda <= iv_endda
          AND   m~endda >= iv_begda
          AND   r~begda <= iv_endda
          AND   r~endda >= iv_begda.

  IF sy-subrc EQ 0.

    SELECT DISTINCT * FROM pa9883
      INTO TABLE lt_pa9883
      FOR ALL ENTRIES IN lt_pa0000
      WHERE   pernr = lt_pa0000-pernr
            AND   begda <= iv_endda
            AND   endda >= iv_begda.

    SORT lt_pa9883 BY pernr.

    LOOP AT lt_pa0000.

      READ TABLE lt_pa9883 WITH KEY pernr = lt_pa0000-pernr
      BINARY SEARCH.
      IF sy-subrc NE 0 OR lt_pa9883-zzcgsjbgrp IS INITIAL
          OR  lt_pa9883-zzcgsjikun IS INITIAL OR lt_pa9883-zzcgsjikub IS INITIAL.

        ADD 1 TO ev_cnt.
      ENDIF.
    ENDLOOP.

  ENDIF.



ENDFUNCTION.

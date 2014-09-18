FUNCTION z_co_get_map_ig.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(MATNR) TYPE  MATNR
*"     VALUE(POPER) TYPE  POPER
*"     VALUE(BDATJ) TYPE  BDATJ
*"     VALUE(I_LOOK) TYPE  CHAR1 OPTIONAL
*"  EXPORTING
*"     VALUE(VERPR) TYPE  VERPR
*"     VALUE(PEINH) TYPE  PEINH
*"     VALUE(RETRO) TYPE  RETRO
*"----------------------------------------------------------------------
* Spec by Andy Choi
* To get MAP from multiple plant (valuation)
  DATA: BEGIN OF i_ck OCCURS 0,
         preis TYPE preis,
         peinh TYPE peinh,
         ab    TYPE ck_abkum,
         um    TYPE ck_umkum,
         zu    TYPE ck_zukum,
         xabrech TYPE ck_xabrech,
        END OF i_ck.
  DATA: w_ck LIKE i_ck.
  DATA: l_bdatj TYPE bdatj,
        l_poper TYPE poper.

  DATA $peinh LIKE peinh.

  DATA: $abkum TYPE ck_abkum.
  DATA $matnr TYPE  matnr.
  CONCATENATE matnr '%' INTO $matnr.

  REFRESH: i_ck.
  CLEAR: w_ck.

  IF matnr CP '*%'.

    SELECT pvprs peinh abkumo umkumo zukumo xabrech
      INTO TABLE i_ck
      FROM ckmlhd
           INNER JOIN ckmlcr
                     ON ckmlcr~kalnr  = ckmlhd~kalnr
           INNER JOIN ckmlpp
                     ON ckmlcr~kalnr  = ckmlpp~kalnr
                    AND ckmlcr~bdatj  = ckmlpp~bdatj
                    AND ckmlcr~poper  = ckmlpp~poper
                    AND ckmlcr~untper = ckmlpp~untper

      WHERE ckmlhd~matnr  LIKE matnr
        AND ckmlcr~bdatj  = bdatj
        AND ckmlcr~poper  = poper
        AND ckmlcr~untper = space
        AND ckmlcr~curtp  = '10'
      %_HINTS ORACLE 'FIRST_ROWS(10)'.
  ELSE.

    SELECT pvprs peinh abkumo umkumo zukumo xabrech
      INTO TABLE i_ck
      FROM ckmlhd
           INNER JOIN ckmlcr
                     ON ckmlcr~kalnr  = ckmlhd~kalnr
           INNER JOIN ckmlpp
                     ON ckmlcr~kalnr  = ckmlpp~kalnr
                    AND ckmlcr~bdatj  = ckmlpp~bdatj
                    AND ckmlcr~poper  = ckmlpp~poper
                    AND ckmlcr~untper = ckmlpp~untper

      WHERE ckmlhd~matnr  = matnr
        AND ckmlcr~bdatj  = bdatj
        AND ckmlcr~poper  = poper
        AND ckmlcr~untper = space
        AND ckmlcr~curtp  = '10'
      %_HINTS ORACLE 'FIRST_ROWS(10)'.

  ENDIF.

  IF sy-subrc EQ 0.
  ELSE.
    IF matnr CP '*%'.
    ELSE.
      SELECT pvprs peinh abkumo umkumo zukumo xabrech
  INTO TABLE i_ck
  FROM ckmlhd
       INNER JOIN ckmlcr
                 ON ckmlcr~kalnr  = ckmlhd~kalnr
       INNER JOIN ckmlpp
                 ON ckmlcr~kalnr  = ckmlpp~kalnr
                AND ckmlcr~bdatj  = ckmlpp~bdatj
                AND ckmlcr~poper  = ckmlpp~poper
                AND ckmlcr~untper = ckmlpp~untper

  WHERE ckmlhd~matnr  LIKE $matnr
    AND ckmlcr~bdatj  = bdatj
    AND ckmlcr~poper  = poper
    AND ckmlcr~untper = space
    AND ckmlcr~curtp  = '10'
  %_HINTS ORACLE 'FIRST_ROWS(10)'.
    ENDIF.
  ENDIF.

*..weighted average
  LOOP AT i_ck.
    SUM. w_ck = i_ck.
  ENDLOOP.

  $abkum = w_ck-ab + w_ck-um + w_ck-zu .

  IF $abkum IS INITIAL .
    SORT i_ck BY xabrech DESCENDING.
    CLEAR i_ck.
    READ TABLE i_ck INDEX 1.
    verpr = i_ck-preis.
    peinh = i_ck-peinh.
  ELSE.
    LOOP AT i_ck.
      verpr = verpr +
                i_ck-preis * ( i_ck-ab + i_ck-um + i_ck-zu )
                           / $abkum.
      peinh = i_ck-peinh.
    ENDLOOP.
  ENDIF.

  IF NOT peinh IS INITIAL.
    $peinh = peinh.
  ENDIF.
*-- if zero price... look up another period
  IF i_look = 'X' AND verpr IS INITIAL.

    IF matnr CP '*%'.
      SELECT ckmlpp~bdatj ckmlpp~poper
        INTO (l_bdatj, l_poper)
        FROM ckmlhd
             INNER JOIN ckmlpp
                       ON ckmlpp~kalnr  = ckmlhd~kalnr
        WHERE ckmlhd~matnr  LIKE matnr
          AND ckmlpp~bdatj  >= bdatj
          AND ckmlpp~untper = space
          AND ( ckmlpp~abkumo NE 0  OR ckmlpp~zukumo NE 0 )
        ORDER BY ckmlpp~bdatj ckmlpp~poper.
        EXIT.
      ENDSELECT.
    ELSE.
      SELECT ckmlpp~bdatj ckmlpp~poper
        INTO (l_bdatj, l_poper)
        FROM ckmlhd
             INNER JOIN ckmlpp
                       ON ckmlpp~kalnr  = ckmlhd~kalnr
        WHERE ckmlhd~matnr  = matnr
          AND ckmlpp~bdatj  >= bdatj
          AND ckmlpp~untper = space
          AND ( ckmlpp~abkumo NE 0  OR ckmlpp~zukumo NE 0 )
        ORDER BY ckmlpp~bdatj ckmlpp~poper.
        EXIT.
      ENDSELECT.
    ENDIF.
    IF sy-subrc EQ 0.
    ELSE.
      IF matnr CP '*%'.
      ELSE.
        SELECT ckmlpp~bdatj ckmlpp~poper
          INTO (l_bdatj, l_poper)
          FROM ckmlhd
               INNER JOIN ckmlpp
                         ON ckmlpp~kalnr  = ckmlhd~kalnr
          WHERE ckmlhd~matnr  LIKE $matnr
            AND ckmlpp~bdatj  >= bdatj
            AND ckmlpp~untper = space
            AND ( ckmlpp~abkumo NE 0  OR ckmlpp~zukumo NE 0 )
          ORDER BY ckmlpp~bdatj ckmlpp~poper.
          EXIT.
        ENDSELECT.
      ENDIF.
    ENDIF.

    IF matnr CP '*%'.
      SELECT pvprs peinh abkumo umkumo zukumo xabrech
        INTO TABLE i_ck  "(exp_preis, exp_peinh, l_ab, l_zu)
        FROM ckmlhd
             INNER JOIN ckmlcr
                       ON ckmlcr~kalnr  = ckmlhd~kalnr
             INNER JOIN ckmlpp
                       ON ckmlcr~kalnr  = ckmlpp~kalnr
                      AND ckmlcr~bdatj  = ckmlpp~bdatj
                      AND ckmlcr~poper  = ckmlpp~poper
                      AND ckmlcr~untper = ckmlpp~untper
        WHERE ckmlhd~matnr  LIKE matnr
          AND ckmlcr~bdatj  = bdatj
          AND ckmlcr~poper  = poper
          AND ckmlcr~untper = space
          AND ckmlcr~curtp  = '10'
      %_HINTS ORACLE 'FIRST_ROWS(10)'.

    ELSE.

      SELECT pvprs peinh abkumo umkumo zukumo xabrech
        INTO TABLE i_ck  "(exp_preis, exp_peinh, l_ab, l_zu)
        FROM ckmlhd
             INNER JOIN ckmlcr
                       ON ckmlcr~kalnr  = ckmlhd~kalnr
             INNER JOIN ckmlpp
                       ON ckmlcr~kalnr  = ckmlpp~kalnr
                      AND ckmlcr~bdatj  = ckmlpp~bdatj
                      AND ckmlcr~poper  = ckmlpp~poper
                      AND ckmlcr~untper = ckmlpp~untper
        WHERE ckmlhd~matnr  = matnr
          AND ckmlcr~bdatj  = l_bdatj
          AND ckmlcr~poper  = l_poper
          AND ckmlcr~untper = space
          AND ckmlcr~curtp  = '10'
      %_HINTS ORACLE 'FIRST_ROWS(10)'.

    ENDIF.

    IF sy-subrc EQ 0.
    ELSE.
      IF matnr CP '*%'.
      ELSE.
        SELECT pvprs peinh abkumo umkumo zukumo xabrech
          INTO TABLE i_ck  "(exp_preis, exp_peinh, l_ab, l_zu)
          FROM ckmlhd
               INNER JOIN ckmlcr
                         ON ckmlcr~kalnr  = ckmlhd~kalnr
               INNER JOIN ckmlpp
                         ON ckmlcr~kalnr  = ckmlpp~kalnr
                        AND ckmlcr~bdatj  = ckmlpp~bdatj
                        AND ckmlcr~poper  = ckmlpp~poper
                        AND ckmlcr~untper = ckmlpp~untper
          WHERE ckmlhd~matnr  LIKE $matnr
            AND ckmlcr~bdatj  = bdatj
            AND ckmlcr~poper  = poper
            AND ckmlcr~untper = space
            AND ckmlcr~curtp  = '10'
        %_HINTS ORACLE 'FIRST_ROWS(10)'.
      ENDIF.
    ENDIF.

*..weighted average
    LOOP AT i_ck.
      SUM. w_ck = i_ck.
    ENDLOOP.

    $abkum = w_ck-ab + w_ck-um + w_ck-zu .

    IF $abkum IS INITIAL.
      SORT i_ck BY xabrech DESCENDING.
      CLEAR i_ck.
      READ TABLE i_ck INDEX 1.
      verpr = i_ck-preis.
      peinh = i_ck-peinh.
    ELSE.
      LOOP AT i_ck.
        verpr = verpr +
                  i_ck-preis * ( i_ck-ab + i_ck-um + i_ck-zu )
                             / $abkum.
        peinh = i_ck-peinh.
      ENDLOOP.
    ENDIF.

    IF NOT verpr IS INITIAL.
      retro = 'X'.
    ENDIF.

  ENDIF.

** if initial, return default value...
*  if verpr is initial.
*    peinh = '9999'.
*    verpr = '0.01'.
*  endif.

  IF peinh IS INITIAL.
    peinh = $peinh.
  ENDIF.

ENDFUNCTION.

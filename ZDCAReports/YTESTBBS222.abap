REPORT ytestbbs222 .
TABLES: mara, eina,zvmm_info_condi.

DATA: BEGIN OF it_matnr OCCURS 0,
        matnr LIKE mara-matnr,
      END   OF it_matnr.

DATA: BEGIN OF IT_INFO OCCURS 0,
        MATNR LIKE MARA-MATNR,
        LIFNR LIKE LFA1-LIFNR,
        DATAB LIKE A018-DATAB,
        DATBI LIKE A018-DATBI,
        NETPR LIKE EKPO-NETPR,
        PEINH LIKE EKPO-PEINH,
        MEINS LIKE EKPO-MEINS,
        WAERS LIKE EKKO-WAERS,
      END   OF IT_INFO.

DATA: BEGIN OF it_knumh OCCURS 0,
        knumh LIKE konh-knumh,
        datab LIKE konh-datab,
        datbi LIKE konh-datbi,
        lifnr LIKE lfa1-lifnr,
      END   OF it_knumh.

SELECT-OPTIONS s_matnr FOR mara-matnr.
PARAMETERS: p_datum LIKE sy-datum OBLIGATORY.

SELECT matnr INTO CORRESPONDING FIELDS OF TABLE it_matnr
  FROM mara
 WHERE matnr IN s_matnr
   AND mtart     IN ('ROH', 'ROH1')
   AND profl     IN ('K',   'V')
   AND lvorm     <> 'X'.
IF sy-subrc NE 0.
  MESSAGE e000(zz) WITH 'No data founded'.
ENDIF.

LOOP AT it_matnr.
  CLEAR: it_knumh, it_knumh[].

  SELECT knumh datab matnr lifnr
    INTO CORRESPONDING FIELDS OF TABLE it_knumh
    FROM a018
   WHERE kappl =  'M'
     AND kschl =  'PB00'
     AND matnr =  it_matnr-matnr
     AND ekorg =  'PU01'
     AND esokz =  '0'
     AND datab <= p_datum
     AND datbi >= p_datum.
  IF sy-subrc NE 0.
    CONTINUE.
  ENDIF.

  LOOP AT it_knumh.
    SELECT SINGLE matnr
      INTO eina-matnr
      FROM eina AS a INNER JOIN eine AS b
        ON a~infnr = b~infnr
     WHERE a~matnr = it_matnr-matnr
       AND a~lifnr = it_knumh-lifnr
       AND a~loekz = ' '
       AND b~werks = ' '
       AND b~ekorg = 'PU01'
       AND b~loekz = ' '.
    IF sy-subrc EQ 0.
      SELECT SINGLE *
        FROM zvmm_info_condi
       WHERE knumh    = it_knumh-knumh
         AND kschl    = 'PB00'
         AND loevm_ko = ' '.
      IF sy-subrc EQ 0.
        DELETE it_matnr.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDLOOP.

LOOP AT it_matnr.
  CLEAR: it_knumh, it_knumh[].

  SELECT datab knumh matnr lifnr
    INTO CORRESPONDING FIELDS OF TABLE it_knumh
    FROM a018
   WHERE kappl =  'M'
     AND kschl =  'PB00'
     AND matnr =  it_matnr-matnr
     AND ekorg =  'PU01'
     AND esokz =  '0'
     AND datbi < p_datum.
  IF sy-subrc NE 0.
    CONTINUE.
  ENDIF.

  LOOP AT it_knumh.
    SELECT SINGLE *
      FROM zvmm_info_condi
     WHERE knumh    = it_knumh-knumh
       AND kschl    = 'PB00'
       AND loevm_ko = ' '.
    IF sy-subrc EQ 0.
      MOVE: IT_MATNR-MATNR TO IT_INFO-MATNR,
            IT_KNUMH-LIFNR TO IT_INFO-LIFNR,
            ZVMM_INFO_CONDI-DATAB TO IT_INFO-DATAB,
            ZVMM_INFO_CONDI-DATBI TO IT_INFO-DATBI,
            ZVMM_INFO_CONDI-KBETR TO IT_INFO-NETPR,
            ZVMM_INFO_CONDI-KPEIN TO IT_INFO-PEINH,
            ZVMM_INFO_CONDI-KMEIN TO IT_INFO-MEINS,
            ZVMM_INFO_CONDI-KONWA TO IT_INFO-WAERS.

      APPEND IT_INFO.
    ENDIF.
  ENDLOOP.
ENDLOOP.

SORT IT_INFO BY MATNR LIFNR DATBI DATAB.
LOOP AT IT_INFO.
  WRITE:/ IT_INFO-MATNR,
          IT_INFO-LIFNR,
          IT_INFO-DATAB,
          IT_INFO-DATBI,
          IT_INFO-NETPR,
          IT_INFO-PEINH,
          IT_INFO-MEINS,
          IT_INFO-WAERS.
ENDLOOP.

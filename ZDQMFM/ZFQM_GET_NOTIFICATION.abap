FUNCTION zfqm_get_notification .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_QMGRP) TYPE  QMGRP
*"     VALUE(I_QMCOD) TYPE  QMCOD OPTIONAL
*"     VALUE(I_DATE_FROM) TYPE  SY-DATUM
*"     VALUE(I_DATE_TO) TYPE  SY-DATUM
*"  EXPORTING
*"     VALUE(ZRESULT) TYPE  ZRESULT
*"     VALUE(ZMESS) TYPE  ZMESS
*"  TABLES
*"      ET_DATA STRUCTURE  ZSQM_SCRAP_PDI
*"----------------------------------------------------------------------
  DATA: l_txt04(20).
  DATA: BEGIN OF lt_temp OCCURS 0,
        qmnum LIKE qmel-qmnum,
        matnr LIKE zsqm_scrap_pdi-matnr,
        maktx LIKE zsqm_scrap_pdi-maktx,
        qmcod LIKE zsqm_scrap_pdi-qmcod,
        rkmng LIKE zsqm_scrap_pdi-rkmng,
        erdat LIKE zsqm_scrap_pdi-erdat,
        mzeit LIKE zsqm_scrap_pdi-mzeit,
        mawerk LIKE qmel-mawerk,
        END OF lt_temp.

  IF i_date_from > i_date_to.
    zresult = 'E'.
    zmess = 'Starting date cannot be late Ending date'.
  ELSE.
    SELECT qmnum a~matnr maktx qmcod rkmng
      erdat mzeit mawerk                  "ABLAD
      INTO CORRESPONDING FIELDS OF TABLE lt_temp
      FROM qmel AS a
      INNER JOIN makt AS b
      ON a~matnr = b~matnr
*      INNER JOIN PKHD AS C
*      ON A~MAWERK = C~WERKS
*      AND A~MATNR = C~MATNR
      WHERE qmart = 'Q3'
        AND erdat BETWEEN i_date_from AND i_date_to
        AND qmgrp = i_qmgrp
*        AND QMCOD = I_QMCOD
        AND kzdkz = ' '
        AND qmnam = ' '
        AND b~spras = 'EN'.

*    SORT ET_DATA BY QMNUM MATNR.
*    DELETE ADJACENT DUPLICATES FROM ET_DATA
*     COMPARING QMNUM MATNR.

    LOOP AT lt_temp.
      MOVE-CORRESPONDING lt_temp TO et_data.
      et_data-qmnum = lt_temp-qmnum+2(10).

      SELECT SINGLE txt04 INTO l_txt04
      FROM qmel AS a
      INNER JOIN jest AS d
      ON a~objnr = d~objnr
      INNER JOIN tj02t AS e
      ON d~stat = e~istat
      WHERE a~qmnum = et_data-qmnum
         AND spras = 'EN'
        AND inact = ' '
        AND txt04 = 'DLFL'.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.
      SELECT SINGLE ablad INTO et_data-ablad
        FROM pkhd
        WHERE werks = lt_temp-mawerk
          AND matnr = lt_temp-matnr.

      APPEND et_data.
      CLEAR: et_data.
    ENDLOOP.
    sort et_data by qmnum.

    IF et_data[] IS NOT INITIAL.
      DELETE ADJACENT DUPLICATES FROM et_data.
      zresult = 'S'.
    ELSE.
      zresult = 'E'.
      zmess = 'No data found'.
    ENDIF.
  ENDIF.
ENDFUNCTION.

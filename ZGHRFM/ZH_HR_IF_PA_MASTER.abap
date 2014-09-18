FUNCTION zh_hr_if_pa_master.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_BRACD) TYPE  HROBJID
*"     VALUE(IV_BEGDA) TYPE  BEGDA
*"     VALUE(IV_ENDDA) TYPE  ENDDA
*"  TABLES
*"      ET_PA0000 STRUCTURE  ZGHRS0000 OPTIONAL
*"      ET_PA0302 STRUCTURE  ZGHRS0302 OPTIONAL
*"      ET_PA0001 STRUCTURE  ZGHRS0001 OPTIONAL
*"      ET_PA0001ENH STRUCTURE  ZGHRS0001ENH OPTIONAL
*"      ET_PA0002 STRUCTURE  ZGHRS0002 OPTIONAL
*"      ET_PA0007 STRUCTURE  ZGHRS0007 OPTIONAL
*"      ET_PA0008 STRUCTURE  ZGHRS0008 OPTIONAL
*"      ET_PA0022 STRUCTURE  ZGHRS0022 OPTIONAL
*"      ET_PA0022ENH STRUCTURE  ZGHRS0022ENH OPTIONAL
*"      ET_99001 STRUCTURE  ZGHRSS0007 OPTIONAL
*"      ET_99002 STRUCTURE  ZGHRSS0011 OPTIONAL
*"      ET_99003 STRUCTURE  ZGHRSS0012 OPTIONAL
*"      ET_PA9882 STRUCTURE  ZGHRS9882 OPTIONAL
*"      ET_PA9880 STRUCTURE  ZGHRS9880 OPTIONAL
*"      ET_PA9880ENH STRUCTURE  ZGHRS9880ENH OPTIONAL
*"      ET_PA9881 STRUCTURE  ZGHRS9881 OPTIONAL
*"      ET_PA9881ENH1 STRUCTURE  ZGHRS9881ENH1 OPTIONAL
*"      ET_PA9881ENH2 STRUCTURE  ZGHRS9881ENH2 OPTIONAL
*"----------------------------------------------------------------------
  DATA: lt_pa0023       TYPE TABLE OF pa0023 WITH HEADER LINE.
  DATA: lt_t513c        TYPE TABLE OF t513c WITH HEADER LINE.
  DATA: lt_99001        LIKE TABLE OF et_99001,
        ls_99001        LIKE et_99001.

  CLEAR: et_pa0000[], et_pa0302[], et_pa0001[], et_pa0001enh[], et_pa0002[], et_pa0022[], et_pa0022enh[].
  CLEAR: et_99001[], et_99002[], et_pa9880[], et_pa9880enh[], et_pa9881[], et_pa9881enh1[], et_pa9881enh2[].

**********Infotype 0000/0302**********
  SELECT * INTO CORRESPONDING FIELDS OF TABLE et_pa0000 FROM pa0000
    WHERE pernr IN ( SELECT DISTINCT pernr FROM pa0000 WHERE aedtm BETWEEN iv_begda AND iv_endda ).
  IF sy-subrc = 0.
    PERFORM fill_bracd_code USING iv_bracd 3 CHANGING et_pa0000[].
  ENDIF.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE et_pa0302 FROM pa0302
    WHERE pernr IN ( SELECT DISTINCT pernr FROM pa0302 WHERE aedtm BETWEEN iv_begda AND iv_endda ).
  IF sy-subrc = 0.
    PERFORM fill_bracd_code USING iv_bracd 3 CHANGING et_pa0302[].
  ENDIF.

**********Infotype 0001**********
  CLEAR: gt_pa0001[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_pa0001 FROM pa0001
    WHERE pernr IN ( SELECT DISTINCT pernr FROM pa0001 WHERE aedtm BETWEEN iv_begda AND iv_endda ).
  IF sy-subrc = 0.
    LOOP AT gt_pa0001.
      MOVE-CORRESPONDING gt_pa0001 TO et_pa0001.
      MOVE-CORRESPONDING gt_pa0001 TO et_pa0001enh.

* Local work area
      et_pa0001-zclwrkar = gt_pa0001-werks.

      APPEND et_pa0001.
      APPEND et_pa0001enh.
    ENDLOOP.

    PERFORM fill_bracd_code USING iv_bracd 3 CHANGING et_pa0001[].
    PERFORM fill_bracd_code USING iv_bracd 3 CHANGING et_pa0001enh[].
    PERFORM fill_additional_data TABLES et_pa0001[] gt_pa0001[].
  ENDIF.

**********Infotype 0002**********
  SELECT * INTO CORRESPONDING FIELDS OF TABLE et_pa0002 FROM pa0002
    WHERE pernr IN ( SELECT DISTINCT pernr FROM pa0002 WHERE aedtm BETWEEN iv_begda AND iv_endda ).
  IF sy-subrc = 0.
    PERFORM fill_bracd_code USING iv_bracd 3 CHANGING et_pa0002[].
  ENDIF.

**********Infotype 0007**********
  SELECT * INTO CORRESPONDING FIELDS OF TABLE et_pa0007 FROM pa0007
    WHERE pernr IN ( SELECT DISTINCT pernr FROM pa0007 WHERE aedtm BETWEEN iv_begda AND iv_endda ).
  IF sy-subrc = 0.
    PERFORM fill_bracd_code USING iv_bracd 3 CHANGING et_pa0007[].
  ENDIF.

**********Infotype 0008**********
  SELECT * INTO CORRESPONDING FIELDS OF TABLE et_pa0008 FROM pa0008
    WHERE pernr IN ( SELECT DISTINCT pernr FROM pa0008 WHERE aedtm BETWEEN iv_begda AND iv_endda )..
  IF sy-subrc = 0.
    PERFORM fill_bracd_code USING iv_bracd 3 CHANGING et_pa0008[].
  ENDIF.

**********Infotype 0022**********
  CLEAR: gt_pa0022[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_pa0022 FROM pa0022
    WHERE pernr IN ( SELECT DISTINCT pernr FROM pa0022 WHERE aedtm BETWEEN iv_begda AND iv_endda ).
  IF sy-subrc = 0.
    LOOP AT gt_pa0022.
      MOVE-CORRESPONDING gt_pa0022 TO et_pa0022.
      MOVE-CORRESPONDING gt_pa0022 TO et_pa0022enh.
      APPEND et_pa0022.
      APPEND et_pa0022enh.
    ENDLOOP.

    PERFORM fill_bracd_code USING iv_bracd 3 CHANGING et_pa0022[].
    PERFORM fill_bracd_code USING iv_bracd 3 CHANGING et_pa0022enh[].
    PERFORM fill_extra_0022  TABLES et_pa0022[].
  ENDIF.

***********Infotype 0025(Appraisal)*********
  CLEAR gt_pa0025[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_pa0025 FROM pa0025
    WHERE pernr IN ( SELECT DISTINCT pernr FROM pa0025 WHERE aedtm BETWEEN iv_begda AND iv_endda )
    AND krt01 IN ('01', '02').

  CLEAR: gt_pa0025, et_99001[].
  LOOP AT gt_pa0025.
*    CLEAR: ls_99001.
*    MOVE-CORRESPONDING gt_pa0025 TO ls_99001.

    CLEAR: lt_99001[].
    CALL FUNCTION 'ZH_HR_APPRAISAL_SCORE_SUM'
      EXPORTING
        is_0025  = gt_pa0025
*       iv_pernr = gt_pa0025-pernr
      TABLES
        et_99001 = lt_99001.
    APPEND LINES OF lt_99001 TO et_99001.
  ENDLOOP.
  IF lines( et_99001 ) > 0.
    PERFORM fill_bracd_code USING iv_bracd 3 CHANGING et_99001[].
  ENDIF.

***********Infotype Other/Previous Employers**********
* PA0023
  SELECT * FROM pa0023 INTO TABLE lt_pa0023
    WHERE pernr IN ( SELECT DISTINCT pernr FROM pa0023 WHERE aedtm BETWEEN iv_begda AND iv_endda ).

  SELECT * FROM t513c INTO TABLE lt_t513c
    WHERE spras = 'E'.
  SORT lt_t513c BY taete.

  LOOP AT lt_pa0023.
    CLEAR et_99002.
    MOVE-CORRESPONDING lt_pa0023 TO et_99002.
    et_99002-zcbracd = iv_bracd.
    et_99002-wpbeg = lt_pa0023-begda.
    et_99002-wpend = lt_pa0023-endda.
    et_99002-arbgb = lt_pa0023-arbgb.
*    et_99002-ORGTX
*    et_99002-JIKWI
    CLEAR lt_t513c.
    READ TABLE lt_t513c WITH KEY taete = lt_pa0023-taete BINARY SEARCH.
    et_99002-resdx = lt_t513c-ltext.

    et_99002-locat = lt_pa0023-ort01.
    APPEND et_99002.
  ENDLOOP.

**********Infotype 9882(Succesion Planning)**********
  SELECT * INTO CORRESPONDING FIELDS OF TABLE et_pa9882 FROM pa9882
    WHERE pernr IN ( SELECT DISTINCT pernr FROM pa9882 WHERE aedtm BETWEEN iv_begda AND iv_endda ).

  IF sy-subrc = 0.
    PERFORM fill_bracd_code USING iv_bracd 3 CHANGING et_pa9882[].
  ENDIF.

**********Infotype 9880(Multi-Language)**********
  SELECT * INTO CORRESPONDING FIELDS OF TABLE et_pa9880 FROM pa9880
    WHERE pernr IN ( SELECT DISTINCT pernr FROM pa9880 WHERE aedtm BETWEEN iv_begda AND iv_endda ).
  .
  IF sy-subrc = 0.
    PERFORM fill_bracd_code USING iv_bracd 3 CHANGING et_pa9880[].
  ENDIF.

  IF lines( et_pa9880 ) > 0.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE et_pa9880enh FROM zghrlt0001
      FOR ALL ENTRIES IN et_pa9880
      WHERE adatanr = et_pa9880-adatanr.
    IF sy-subrc = 0.
      PERFORM fill_bracd_code USING iv_bracd 3 CHANGING et_pa9880enh[].
    ENDIF.
  ENDIF.

**********Infotype 9881(Talent Mgmt)**********
  SELECT * INTO CORRESPONDING FIELDS OF TABLE et_pa9881 FROM pa9881
    WHERE pernr IN ( SELECT DISTINCT pernr FROM pa9881 WHERE aedtm BETWEEN iv_begda AND iv_endda ).
  IF sy-subrc = 0.
    PERFORM fill_bracd_code USING iv_bracd 3 CHANGING et_pa9881[].
  ENDIF.

  IF lines( et_pa9881 ) > 0.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE et_pa9881enh1 FROM zghrlt0002
      FOR ALL ENTRIES IN et_pa9881
      WHERE tabnr = et_pa9881-thist.
    IF sy-subrc = 0.
      PERFORM fill_bracd_code USING iv_bracd 3 CHANGING et_pa9881enh1[].
    ENDIF.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE et_pa9881enh2 FROM zghrlt0003
      FOR ALL ENTRIES IN et_pa9881
      WHERE tabnr = et_pa9881-tplan.
    IF sy-subrc = 0.
      PERFORM fill_bracd_code USING iv_bracd 3 CHANGING et_pa9881enh2[].
    ENDIF.
  ENDIF.

ENDFUNCTION.

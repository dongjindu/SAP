FUNCTION zh_hr_if_local_cd_master.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_BRACD) TYPE  HROBJID
*"  TABLES
*"      ET_ZHHRT2910 STRUCTURE  ZGHRSS0003 OPTIONAL
*"      ET_ZHHRT2910T STRUCTURE  ZGHRSS0004 OPTIONAL
*"      ET_ZHHRT2916 STRUCTURE  ZGHRSS0009 OPTIONAL
*"      ET_ZHHRT2916T STRUCTURE  ZGHRSS0010 OPTIONAL
*"      ET_ZHHRT2920 STRUCTURE  ZGHRSS0013 OPTIONAL
*"      ET_ZHHRT2920T STRUCTURE  ZGHRSS0014 OPTIONAL
*"----------------------------------------------------------------------

  DATA: lt_hrp1000 TYPE TABLE OF hrp1000 WITH HEADER LINE.
  DATA: lt_t500p    TYPE TABLE OF t500p WITH HEADER LINE.

* Local Title
  CLEAR: et_zhhrt2910[], et_zhhrt2910t[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_hrp1000
  FROM hrp1000
  WHERE plvar = '01'
  AND   otype = 'C'
  AND   istat = '1'.

  LOOP AT lt_hrp1000.

    et_zhhrt2910t-zcbracd  = iv_bracd.
    et_zhhrt2910t-zcljikub = lt_hrp1000-objid.
    et_zhhrt2910t-sprsl    = lt_hrp1000-langu.
    et_zhhrt2910t-endda    = lt_hrp1000-endda.
    et_zhhrt2910t-begda    = lt_hrp1000-begda.
    et_zhhrt2910t-text     = lt_hrp1000-stext.

    MOVE-CORRESPONDING et_zhhrt2910t TO et_zhhrt2910.
    APPEND et_zhhrt2910t.
    APPEND et_zhhrt2910.
  ENDLOOP.

  SORT et_zhhrt2910 BY zcljikub.
  DELETE ADJACENT DUPLICATES FROM et_zhhrt2910 COMPARING  zcljikub.

** Local Talent Mgmt Training Code
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE et_zghrlt0004
*    FROM   zghrlt0004.
*  IF sy-subrc = 0.
*    PERFORM fill_bracd_code USING iv_bracd 3 CHANGING et_zghrlt0004[].
*  ENDIF.

* Local Work area
  SELECT * FROM t500p INTO TABLE lt_t500p.
  LOOP AT lt_t500p.
    CLEAR: et_zhhrt2916, et_zhhrt2916t.
    et_zhhrt2916t-zcbracd = et_zhhrt2916-zcbracd = iv_bracd.
    et_zhhrt2916t-zclwrkar = et_zhhrt2916-zclwrkar = lt_t500p-persa.

    et_zhhrt2916t-sprsl = sy-langu.
    et_zhhrt2916t-endda = '99991231'.
    et_zhhrt2916t-begda = '19800101'.
    et_zhhrt2916t-text  = lt_t500p-name1.

    APPEND: et_zhhrt2916, et_zhhrt2916t.
  ENDLOOP.

ENDFUNCTION.

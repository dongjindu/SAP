FUNCTION ZH_HR_IF_PD_MASTER_2ND.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_BRACD) TYPE  HROBJID
*"     VALUE(IV_BEGDA) TYPE  BEGDA
*"     VALUE(IV_ENDDA) TYPE  ENDDA
*"  TABLES
*"      ET_HRP1000 STRUCTURE  ZGHRS1000 OPTIONAL
*"      ET_HRP1001 STRUCTURE  ZGHRS1001 OPTIONAL
*"      ET_ORGLEVEL STRUCTURE  ZGHRS9890 OPTIONAL
*"      ET_HRP9891 STRUCTURE  ZGHRS9891 OPTIONAL
*"----------------------------------------------------------------------

  CLEAR: et_hrp1000[], et_hrp1001[], et_orglevel[], et_hrp9891[].

*  DATA: it_hrp9890 TYPE TABLE OF hrp9890 WITH HEADER LINE.
  DATA: it_hrp9891 TYPE TABLE OF hrp9891 WITH HEADER LINE.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE et_hrp1000 FROM hrp1000
   WHERE otype IN ('O','S','P','C','91','92')
      and plvar = '01'
     AND aedtm => iv_begda
     AND aedtm <= iv_endda.
  IF sy-subrc = 0.
    PERFORM fill_bracd_code USING iv_bracd 3 CHANGING et_hrp1000[].
  ENDIF.

* 5) O-003-S add. 2014-05-08

  SELECT * INTO CORRESPONDING FIELDS OF TABLE et_hrp1001 FROM hrp1001
   WHERE otype IN ('O','S','91','92')
         AND relat IN ('002','003','007','008','012')
         AND plvar = '01'
         AND sclas IN ('O','S','P','91','92')
         AND aedtm => iv_begda
         AND aedtm <= iv_endda.
  IF sy-subrc = 0.
    PERFORM fill_bracd_code USING iv_bracd 3 CHANGING et_hrp1001[].
  ENDIF.

*  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_hrp9890
*   FROM  hrp9890
*   WHERE plvar = '01'
*   AND   otype = 'O'
*   AND   istat = '1'
*   AND   aedtm => iv_begda
*   AND   aedtm <= iv_endda.
*  IF sy-subrc = 0.
*    PERFORM convert_it_hrp9890_et_orglevel TABLES it_hrp9890[]
*et_orglevel[].
  PERFORM make_et_orglevel TABLES et_hrp1000[]
                                  et_hrp1001[]
                                  et_orglevel[].
  PERFORM fill_bracd_code USING iv_bracd 3 CHANGING et_orglevel[].
*  ENDIF.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_hrp9891
   FROM  hrp9891
   WHERE plvar = '01'
   AND   otype = 'O'
   AND   istat = '1'
   AND   aedtm => iv_begda
   AND   aedtm <= iv_endda.
  IF sy-subrc = 0.
    PERFORM move_9891_to_et_orgkey TABLES it_hrp9891[] et_hrp9891[].
    PERFORM fill_bracd_code USING iv_bracd 3 CHANGING et_hrp9891[].
  ENDIF.



ENDFUNCTION.

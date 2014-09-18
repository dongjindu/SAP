FUNCTION zh_hr_appraisal_score_sum.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IS_0025) TYPE  PA0025
*"  TABLES
*"      ET_99001 STRUCTURE  ZGHRSS0007 OPTIONAL
*"----------------------------------------------------------------------


  TABLES: q0025, hrcm_empinfo..

  INCLUDE fhcmpd04.

*  DATA: i0025 TYPE p0025 OCCURS 0 WITH HEADER LINE.
*
*  SELECT * INTO TABLE i0025
*    FROM pa0025
*    WHERE pernr = is_99001-pernr
*    AND   subty = is_99001-subty
*    AND   objps = is_99001-objps
*    AND   sprps = is_99001-sprps
*    AND   endda = is_99001-endda
*    AND   begda = is_99001-begda
*    AND   seqnr = is_99001-seqnr.
*
*  SORT i0025 BY begda DESCENDING endda.
*
*  LOOP AT i0025.
*    et_99001-evyear = i0025-begda+0(4).
*    et_99001-subty  = et_99001-evtype = '03'. "Overall
*
*    SELECT SINGLE * INTO CORRESPONDING FIELDS OF hrcm_empinfo FROM pa0001 WHERE pernr = i0025-pernr
*                                                          AND endda => i0025-begda
*                                                          AND begda <= i0025-endda.
*
*    PERFORM criteria_weights_get USING i0025
*                                       hrcm_empinfo
*                              CHANGING q0025.
*
*    IF q0025-summe > 0.
*      et_99001-evscor = q0025-summe.
*
*      APPEND et_99001.
*    ENDIF.
*    .
**    WRITE:/ i0025-pernr, i0025-begda, i0025-endda, q0025-summe.
*  ENDLOOP.

  MOVE-CORRESPONDING is_0025 TO et_99001.

  et_99001-evyear = is_0025-begda+0(4).
  et_99001-evtype = '03'. "Overall
  et_99001-subty  = is_0025-krt01.

  SELECT SINGLE * INTO CORRESPONDING FIELDS OF hrcm_empinfo FROM pa0001 WHERE pernr = is_0025-pernr
                                                        AND endda => is_0025-begda
                                                        AND begda <= is_0025-endda.

  PERFORM criteria_weights_get USING is_0025
                                     hrcm_empinfo
                            CHANGING q0025.

  IF q0025-summe > 0.
    et_99001-evscor = q0025-summe.

    APPEND et_99001.
  ENDIF.



ENDFUNCTION.

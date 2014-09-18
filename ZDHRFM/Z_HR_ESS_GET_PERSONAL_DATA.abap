FUNCTION z_hr_ess_get_personal_data.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(EMPLOYEE_NUMBER) LIKE  BAPI7004-PERNR
*"  TABLES
*"      ZESS_PERSONAL STRUCTURE  ZESS_PERSONAL
*"      RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------
* Modification Logs
* Date       Developer  Request ID  Description
*-----------------------------------------------------------------------
* 02/19/2013 Valerian   UD1K956440  Fix Hire Date for Personal Data on
*                                   MyHMMA
*-----------------------------------------------------------------------

  SELECT SINGLE * FROM pa0000
  WHERE pernr EQ employee_number
    AND endda EQ '99991231'.

  IF sy-subrc EQ 0 OR pa0000-stat2 EQ '1'
          OR pa0000-stat2 EQ '3'.
  ELSE.
    return-type = 'E'.
    return-message = 'Invalid Employee Number or Withdrawn'.
    APPEND return.
    EXIT.
  ENDIF.

  SELECT SINGLE ename INTO zess_personal-name
        FROM pa0001 WHERE pernr EQ employee_number
                             AND begda <= sy-datum
                             AND endda >= sy-datum.
  IF sy-subrc NE 0.
    return-type = 'E'.
    return-message = 'Invalid Employee Number'.
    APPEND return.
    EXIT.
  ENDIF.

  REPLACE :
      'Mrs ' WITH '' INTO zess_personal-name,
      'Mrs.' WITH '' INTO zess_personal-name,
      'Mr ' WITH '' INTO zess_personal-name,
      'Mr.' WITH '' INTO zess_personal-name,
      'Miss ' WITH '' INTO zess_personal-name,
      'Ms ' WITH '' INTO zess_personal-name,
      'Ms.' WITH '' INTO zess_personal-name,
      'Dr.' WITH '' INTO zess_personal-name,
      'Dr ' WITH '' INTO zess_personal-name.

  SHIFT zess_personal-name
           LEFT  DELETING LEADING space.

  SELECT SINGLE  * FROM pa0002 WHERE pernr EQ employee_number
                             AND begda <= sy-datum
                             AND endda >= sy-datum.

  IF sy-subrc EQ 0.
    zess_personal-ssn = pa0002-perid.
    zess_personal-birth_date = pa0002-gbdat.
*    ZESS_PERSONAL-GENDER = PA0002-GESCH .

    SELECT SINGLE sptxt INTO zess_personal-comm_lang
          FROM t002t WHERE spras EQ 'E'
                       AND sprsl EQ pa0002-sprsl.


    SELECT SINGLE natio INTO zess_personal-nationality
    FROM t005t WHERE land1 = pa0002-natio
                 AND spras = 'E'.

    SELECT SINGLE ftext INTO zess_personal-mar_status
    FROM t502t WHERE famst = pa0002-famst
                 AND sprsl = 'E'.

    zess_personal-dependents = pa0002-anzkd.
  ENDIF.

  zess_personal-comm_lang_key = pa0002-sprsl.
  zess_personal-nationality_key = pa0002-natio.
  zess_personal-mar_status_key = pa0002-famst.

  DATA : $datar TYPE datar,
         $dardt TYPE dardt.

  SELECT SINGLE * FROM pa0041
             WHERE pernr EQ employee_number
               AND endda EQ '99991231'.

  DO 12 TIMES VARYING $datar FROM pa0041-dar01 NEXT pa0041-dar02
              VARYING $dardt FROM pa0041-dat01 NEXT pa0041-dat02.
    IF $datar EQ 'ZC'.                                      "UD1K956440
*   IF $datar EQ 'Z1'.                                      "UD1K956440
      zess_personal-hire_date = $dardt.
      EXIT.
    ENDIF.
  ENDDO.

*  ZESS_PERSONAL-HIRE_DATE = pa0000-begda.

*  select single * from PA0041
*  WHERE pernr EQ employee_number
*    AND endda EQ '99991231'.
*  if sy-subrc eq 0.
*    ZESS_PERSONAL-hire_date = pa0041-begda.
*  endif.

  zess_personal-stat2 = pa0000-stat2.

  CASE pa0000-stat2.
    WHEN '1'.
      zess_personal-stat2_descript = 'Inactive'.
    WHEN '3'.
      zess_personal-stat2_descript = 'Active'.
    WHEN OTHERS.
  ENDCASE.

  APPEND zess_personal.

  return-type = 'S'.
  return-message = 'Success!'.
  APPEND return.

ENDFUNCTION.

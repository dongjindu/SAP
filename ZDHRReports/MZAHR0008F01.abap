*----------------------------------------------------------------------*
*   INCLUDE MZAHR0008F01                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
FORM SELECT_DATA.
  CLEAR IT_PCP04. REFRESH IT_PCP04.
*
  CLEAR ZTHR_PCP04.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_PCP04
    FROM ZTHR_PCP04 WHERE ZYEAR = W_ZYEAR
                      AND ZVERS = W_ZVERS.
*
  IF SY-SUBRC = 0.
    LOOP AT IT_PCP04.
      IT_PCP04-TOTA1 = IT_PCP04-DAY01 + IT_PCP04-DAY02 + IT_PCP04-DAY03
                     + IT_PCP04-DAY04 + IT_PCP04-DAY05 + IT_PCP04-DAY06
                     + IT_PCP04-DAY07 + IT_PCP04-DAY08 + IT_PCP04-DAY09
                     + IT_PCP04-DAY10 + IT_PCP04-DAY11 + IT_PCP04-DAY12.
      IT_PCP04-TOTA2 = IT_PCP04-SAT01 + IT_PCP04-SAT02 + IT_PCP04-SAT03
                     + IT_PCP04-SAT04 + IT_PCP04-SAT05 + IT_PCP04-SAT06
                     + IT_PCP04-SAT07 + IT_PCP04-SAT08 + IT_PCP04-SAT09
                     + IT_PCP04-SAT10 + IT_PCP04-SAT11 + IT_PCP04-SAT12.
      IT_PCP04-TOTA3 = IT_PCP04-SUN01 + IT_PCP04-SUN02 + IT_PCP04-SUN03
                     + IT_PCP04-SUN04 + IT_PCP04-SUN05 + IT_PCP04-SUN06
                     + IT_PCP04-SUN07 + IT_PCP04-SUN08 + IT_PCP04-SUN09
                     + IT_PCP04-SUN10 + IT_PCP04-SUN11 + IT_PCP04-SUN12.
      IT_PCP04-TOTA4 = IT_PCP04-HOL01 + IT_PCP04-HOL02 + IT_PCP04-HOL03
                     + IT_PCP04-HOL04 + IT_PCP04-HOL05 + IT_PCP04-HOL06
                     + IT_PCP04-HOL07 + IT_PCP04-HOL08 + IT_PCP04-HOL09
                     + IT_PCP04-HOL10 + IT_PCP04-HOL11 + IT_PCP04-HOL12.
      MODIFY IT_PCP04.
    ENDLOOP.
  ELSE.
    IT_PCP04-ZYEAR = W_ZYEAR.
    IT_PCP04-ZVERS = W_ZVERS.
    APPEND IT_PCP04.
    MESSAGE W001 WITH 'NO DATA SELECTED'.
  ENDIF.
ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
FORM SAVE_DATA.
  DATA: L_COUNT(2) TYPE N,
        L_DVALU    LIKE DD07V-DOMVALUE_L,
        L_DTEXT    LIKE DD07V-DDTEXT.
*
  L_COUNT = 0.
*
  DO 12 TIMES.
    CLEAR W_DAYTO.
    L_COUNT = L_COUNT + 1.
    CLEAR: W_DAYFN, W_SATFN, W_SUNFN, W_HOLFN.
    CONCATENATE: 'IT_PCP04-DAY' L_COUNT INTO W_DAYFN,
                 'IT_PCP04-SAT' L_COUNT INTO W_SATFN,
                 'IT_PCP04-SUN' L_COUNT INTO W_SUNFN,
                 'IT_PCP04-HOL' L_COUNT INTO W_HOLFN.
    ASSIGN: (W_DAYFN) TO <DA>,
            (W_SATFN) TO <SA>,
            (W_SUNFN) TO <SU>,
            (W_HOLFN) TO <HO>.
    W_DAYTO = <DA> + <SA> + <SU> + <HO>.
    IF W_DAYTO = 0.
      L_DVALU = L_COUNT.
      CALL FUNCTION 'DOMAIN_VALUE_GET'
           EXPORTING I_DOMNAME  = 'MONTH'
                     I_DOMVALUE = L_DVALU
           IMPORTING E_DDTEXT   = L_DTEXT.
      MESSAGE W012 WITH L_DTEXT.
      EXIT.
    ENDIF.
  ENDDO.
*
  CHECK W_DAYTO > 0.
  W_FLAGS = SPACE.
*
  IT_PCP04-ZYEAR = W_ZYEAR.
  IT_PCP04-ZVERS = W_ZVERS.
  MODIFY IT_PCP04 INDEX 1.
*
  CLEAR ZTHR_PCP04.
  MODIFY ZTHR_PCP04 FROM TABLE IT_PCP04.
  IF SY-SUBRC = 0.
    MESSAGE S001 WITH 'DATA SAVED'.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " SAVE_DATA

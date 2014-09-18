*----------------------------------------------------------------------*
*   INCLUDE MZAHR0008I01                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  BACK_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE BACK_EXIT INPUT.
  SET SCREEN 0. LEAVE SCREEN.
ENDMODULE.                 " BACK_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_ZYEAR_VALUE  INPUT
*&---------------------------------------------------------------------*
MODULE GET_ZYEAR_VALUE INPUT.
  CLEAR IT_YEARS. REFRESH IT_YEARS.
*
  CLEAR ZTHR_PCP02.
  SELECT ZVAL1 INTO ZTHR_PCP02-ZVAL1
    FROM ZTHR_PCP02 WHERE ZMODL = '02'
                      AND ZGRUP = '1020'.
    IT_YEARS-ZYEAR = ZTHR_PCP02-ZVAL1+(4).
    APPEND IT_YEARS. CLEAR IT_YEARS.
  ENDSELECT.
*
  CLEAR IT_FIELD. REFRESH IT_FIELD.
*
  IT_FIELD-TABNAME    = 'ZTHR_PCP03'.
  IT_FIELD-FIELDNAME  = 'ZYEAR'.
  IT_FIELD-SELECTFLAG = 'X'.
  APPEND IT_FIELD. CLEAR IT_FIELD.
*
  CLEAR: W_FNAME, W_TABIX, W_FLDVL.
*
  CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
       EXPORTING
            SELECTFIELD      = W_FNAME
       IMPORTING
            IND              = W_TABIX
            SELECT_VALUE     = W_FLDVL
       TABLES
            FIELDS           = IT_FIELD
            FULL_TABLE       = IT_YEARS.
*
  W_ZYEAR = W_FLDVL.
ENDMODULE.                 " GET_ZYEAR_VALUE  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_ZVERS_VALUE  INPUT
*&---------------------------------------------------------------------*
MODULE GET_ZVERS_VALUE INPUT.
  CLEAR IT_VERSN. REFRESH IT_VERSN.
*
  CLEAR ZTHR_PCP02.
  SELECT ZVAL1 ZVAL3 INTO (ZTHR_PCP02-ZVAL1, ZTHR_PCP02-ZVAL3)
    FROM ZTHR_PCP02 WHERE ZMODL = '02'
                      AND ZGRUP = '1030'
                      AND ZVAL2 = W_ZYEAR.
    IT_VERSN-ZVERS = ZTHR_PCP02-ZVAL1+(2).
    APPEND IT_VERSN. CLEAR IT_VERSN.
  ENDSELECT.
*
  IF IT_VERSN[] IS INITIAL.
    EXIT.
  ENDIF.
*
  CLEAR IT_FIELD. REFRESH IT_FIELD.
*
  IT_FIELD-TABNAME    = 'ZTHR_PCP03'.
  IT_FIELD-FIELDNAME  = 'ZVERS'.
  IT_FIELD-SELECTFLAG = 'X'.
  APPEND IT_FIELD. CLEAR IT_FIELD.
*
  CLEAR: W_FNAME, W_TABIX, W_FLDVL.
*
  CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
       EXPORTING
            SELECTFIELD      = W_FNAME
       IMPORTING
            IND              = W_TABIX
            SELECT_VALUE     = W_FLDVL
       TABLES
            FIELDS           = IT_FIELD
            FULL_TABLE       = IT_VERSN.
*
  W_ZVERS = W_FLDVL.
ENDMODULE.                 " GET_ZVERS_VALUE  INPUT
*&---------------------------------------------------------------------*
*&      Module  CALCU_TOTAL_DAYWEEK  INPUT
*&---------------------------------------------------------------------*
MODULE CALCU_TOTAL_DAYWEEK INPUT.
  IT_PCP04-TOTA1 = IT_PCP04-DAY01 + IT_PCP04-DAY02 + IT_PCP04-DAY03
                 + IT_PCP04-DAY04 + IT_PCP04-DAY05 + IT_PCP04-DAY06
                 + IT_PCP04-DAY07 + IT_PCP04-DAY08 + IT_PCP04-DAY09
                 + IT_PCP04-DAY10 + IT_PCP04-DAY11 + IT_PCP04-DAY12.
ENDMODULE.                 " CALCU_TOTAL_DAYWEEK  INPUT
*&---------------------------------------------------------------------*
*&      Module  CALCU_TOTAL_SATURDAY  INPUT
*&---------------------------------------------------------------------*
MODULE CALCU_TOTAL_SATURDAY INPUT.
  IT_PCP04-TOTA2 = IT_PCP04-SAT01 + IT_PCP04-SAT02 + IT_PCP04-SAT03
                 + IT_PCP04-SAT04 + IT_PCP04-SAT05 + IT_PCP04-SAT06
                 + IT_PCP04-SAT07 + IT_PCP04-SAT08 + IT_PCP04-SAT09
                 + IT_PCP04-SAT10 + IT_PCP04-SAT11 + IT_PCP04-SAT12.
ENDMODULE.                 " CALCU_TOTAL_SATURDAY  INPUT
*&---------------------------------------------------------------------*
*&      Module  CALCU_TOTAL_SUNDAY  INPUT
*&---------------------------------------------------------------------*
MODULE CALCU_TOTAL_SUNDAY INPUT.
  IT_PCP04-TOTA3 = IT_PCP04-SUN01 + IT_PCP04-SUN02 + IT_PCP04-SUN03
                 + IT_PCP04-SUN04 + IT_PCP04-SUN05 + IT_PCP04-SUN06
                 + IT_PCP04-SUN07 + IT_PCP04-SUN08 + IT_PCP04-SUN09
                 + IT_PCP04-SUN10 + IT_PCP04-SUN11 + IT_PCP04-SUN12.
ENDMODULE.                 " CALCU_TOTAL_SUNDAY  INPUT
*&---------------------------------------------------------------------*
*&      Module  CALCU_TOTAL_HOLIDAY  INPUT
*&---------------------------------------------------------------------*
MODULE CALCU_TOTAL_HOLIDAY INPUT.
  IT_PCP04-TOTA4 = IT_PCP04-HOL01 + IT_PCP04-HOL02 + IT_PCP04-HOL03
                 + IT_PCP04-HOL04 + IT_PCP04-HOL05 + IT_PCP04-HOL06
                 + IT_PCP04-HOL07 + IT_PCP04-HOL08 + IT_PCP04-HOL09
                 + IT_PCP04-HOL10 + IT_PCP04-HOL11 + IT_PCP04-HOL12.
ENDMODULE.                 " CALCU_TOTAL_HOLIDAY  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  CASE SY-UCOMM.
    WHEN 'EXEC'.
      W_FLAGS = 'X'.
      PERFORM SELECT_DATA.
    WHEN 'SAVE'.
      PERFORM SAVE_DATA.
*   WHEN OTHERS. W_FLAGS = SPACE.
  ENDCASE.
*
  CLEAR SY-UCOMM.
ENDMODULE.                 " USER_COMMAND_9000  INPUT

*----------------------------------------------------------------------*
*   INCLUDE MZAHR0002I01                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  BACK_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE BACK_EXIT INPUT.
  SET SCREEN 0. LEAVE SCREEN.
ENDMODULE.                 " BACK_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_PER_AREA_NAME  INPUT
*&---------------------------------------------------------------------*
*       possible entry for personal area and get name
*----------------------------------------------------------------------*
MODULE GET_PER_AREA_NAME INPUT.
  GET CURSOR LINE W_INDEX.
  PERFORM P_ENTRY_PERSONAL_AREA.
  PERFORM GET_PERSONAL_AREA_NAME.
ENDMODULE.                 " GET_PER_AREA_NAME  INPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_INTAB  INPUT
*&---------------------------------------------------------------------*
MODULE MODIFY_INTAB INPUT.
  IT_S9000-ZVERS = W_ZVERS.
  IT_S9000-ZYEAR = SY-DATUM+(4) + 1.
*
  CLEAR T500P.
  SELECT SINGLE PERSA INTO T500P-PERSA
    FROM T500P WHERE MOLGA = '10'
                 AND NAME1 = IT_S9000-NAME1.
  IT_S9000-ZPERA = T500P-PERSA.
*
  CLEAR TP_HCP01.
  READ TABLE TP_HCP01 WITH KEY ZPERA = IT_S9000-ZPERA
                               ZCOST = IT_S9000-ZCOST
                               ZJOBK = IT_S9000-ZJOBK
                               ZPERG = IT_S9000-ZPERG
                               ZSUBG = IT_S9000-ZSUBG
                               ZSENR = IT_S9000-ZSENR.
  IF SY-SUBRC = 0.
    IT_S9000-ZCURC = TP_HCP01-ZCURC.
    IT_S9000-ZHEDC = IT_S9000-ZCURC + IT_S9000-ZPLAN.
  ELSE.
    IT_S9000-ZCURC = 0.
    IT_S9000-ZHEDC = IT_S9000-ZPLAN.
  ENDIF.
*
  MODIFY IT_S9000 INDEX TC9000-CURRENT_LINE.
ENDMODULE.                 " MODIFY_INTAB  INPUT
*&---------------------------------------------------------------------*
*&      Module  CALCULATE_MAN  INPUT
*&---------------------------------------------------------------------*
MODULE CALCULATE_MAN INPUT.
  IF IT_S9000-ZPLAN < 0.
    IF IT_S9000-ZSENR = 1.
      MESSAGE E004.
    ENDIF.

    CLEAR TP_HCP01.
    READ TABLE TP_HCP01 WITH KEY ZPERA = IT_S9000-ZPERA
                                 ZCOST = IT_S9000-ZCOST
                                 ZJOBK = IT_S9000-ZJOBK
                                 ZPERG = IT_S9000-ZPERG
                                 ZSUBG = IT_S9000-ZSUBG
                                 ZSENR = IT_S9000-ZSENR.
    IF SY-SUBRC = 0.
      W_ABSVA = ABS( IT_S9000-ZPLAN ).
      IF TP_HCP01-ZCURC < W_ABSVA.
        MESSAGE E006.
      ENDIF.
    ELSE.
      MESSAGE E005.
    ENDIF.
  ENDIF.
  IT_S9000-ZHEDC = IT_S9000-ZCURC + IT_S9000-ZPLAN.
*
  MODIFY IT_S9000 INDEX TC9000-CURRENT_LINE.
ENDMODULE.                 " CALCULATE_MAN  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  CASE SY-UCOMM.
    WHEN 'EXEC'.
*     PERFORM GET_PERSONNEL_COUNT.
      PERFORM GET_MONTHLY_COUNT.
      PERFORM CALCULATE_PERSONAL_COUNT.
    WHEN 'DELE'.
      DELETE IT_S9000 WHERE CHKBX = 'X'.
      REFRESH CONTROL 'TC9000' FROM SCREEN 9000.
      DESCRIBE TABLE IT_S9000 LINES TC9000-LINES.
  ENDCASE.
  CLEAR SY-UCOMM.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_JOB_NAME  INPUT
*&---------------------------------------------------------------------*
MODULE GET_JOB_NAME INPUT.
  GET CURSOR LINE W_INDEX.
  PERFORM P_ENTRY_JOB.
  PERFORM GET_JOB_NAME.
ENDMODULE.                 " GET_JOB_NAME  INPUT

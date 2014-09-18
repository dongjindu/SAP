*&---------------------------------------------------------------------*
*& Report  ZTRR00600
*&
*&---------------------------------------------------------------------*
*&  Cash Planning By Yearly
*&  You must create the table!!!.
*&---------------------------------------------------------------------*

REPORT  ZTRR00600  MESSAGE-ID ZMFI.

INCLUDE ZTRR00600TOP.
INCLUDE ZTRR00600CLS.
INCLUDE ZTRR00600F01.

INITIALIZATION.

  GET PARAMETER ID 'BUK' FIELD P_BUKRS.
  IF P_BUKRS IS INITIAL.
    P_BUKRS = 'H201'.
  ENDIF.

START-OF-SELECTION.

  IF  P_R2 = 'X' AND P_SEQNO IS INITIAL.
    MESSAGE S031 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  PERFORM SET_DATE.
  PERFORM GET_TREE_DATA.
  PERFORM SELECT_CO_DATA.
  PERFORM SELECT_PLAN_DATA.
  PERFORM GET_BALANCE_DATA.

  PERFORM MAKE_TREE_DATA.

  CALL SCREEN 9000.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND INPUT.


  CASE OK_CODE.
    WHEN 'EXPAND'.
      PERFORM EXPAND_NODE.

    WHEN 'COLLAPSE'.
      PERFORM COLLAPSE_NODE.

    WHEN 'SAVE'.

      PERFORM SELECT_CO_DATA.
      PERFORM SELECT_PLAN_DATA.
      IF G_PA = 'X'.
        PERFORM APPEND_PA_DATA.
      ENDIF.
      IF G_CCA = 'X'.
        PERFORM APPEND_CCA_DATA.
      ENDIF.
      IF G_IM = 'X'.
        PERFORM APPEND_IM_DATA.
      ENDIF.
      PERFORM APPEND_MANUAL.

      PERFORM SAVE_TO_ZTTR0008.

      PERFORM GET_BALANCE_DATA.
      PERFORM REFRESH_DATA.
    WHEN 'SINGLE'.
      CALL TRANSACTION 'FF63'.

      PERFORM SELECT_CO_DATA.
      PERFORM SELECT_PLAN_DATA.
      IF G_PA = 'X'.
        PERFORM APPEND_PA_DATA.
      ENDIF.
      IF G_CCA = 'X'.
        PERFORM APPEND_CCA_DATA.
      ENDIF.
      IF G_IM = 'X'.
        PERFORM APPEND_IM_DATA.
      ENDIF.
      PERFORM APPEND_MANUAL.

      PERFORM GET_BALANCE_DATA.
      PERFORM REFRESH_DATA.

    WHEN 'NEWDISPLAY'.
      CALL SCREEN 8000 STARTING AT 20   5
                       ENDING   AT 80   8.

    WHEN 'MA'.
      ICLEAR GT_7000.

      IF GT_MANUAL[] IS INITIAL.
        DO 10 TIMES.
          APPEND GT_7000.
        ENDDO.
      ELSE.
        GT_7000[] = GT_MANUAL[].
      ENDIF.

      CALL SCREEN 7000 STARTING AT 10   1
                       ENDING   AT 130  20.

    WHEN 'OK'.
      PERFORM SELECT_CO_DATA.
      PERFORM SELECT_PLAN_DATA.
      IF G_PA = 'X'.
        PERFORM APPEND_PA_DATA.
      ENDIF.
      IF G_CCA = 'X'.
        PERFORM APPEND_CCA_DATA.
      ENDIF.
      IF G_IM = 'X'.
        PERFORM APPEND_IM_DATA.
      ENDIF.
      PERFORM APPEND_MANUAL.
      PERFORM GET_BALANCE_DATA.
      PERFORM REFRESH_DATA.
      LEAVE TO SCREEN 0.

    WHEN 'PA'.
      IF G_PA = SPACE.
        PERFORM SELECT_PLAN_DATA.
        PERFORM APPEND_PA_DATA.
        IF G_CCA = 'X'.
          PERFORM APPEND_CCA_DATA.
        ENDIF.
        IF G_IM = 'X'.
          PERFORM APPEND_IM_DATA.
        ENDIF.
        PERFORM APPEND_MANUAL.
        PERFORM GET_BALANCE_DATA.
        PERFORM REFRESH_DATA.
      ENDIF.

    WHEN 'CCA'.
      IF G_CCA = SPACE.
        PERFORM SELECT_PLAN_DATA.
        IF G_PA = 'X'.
          PERFORM APPEND_PA_DATA.
        ENDIF.
        IF G_IM = 'X'.
          PERFORM APPEND_IM_DATA.
        ENDIF.
        PERFORM APPEND_CCA_DATA.
        PERFORM APPEND_MANUAL.
        PERFORM GET_BALANCE_DATA.
        PERFORM REFRESH_DATA.
      ENDIF.

    WHEN 'IM'.
      IF G_IM = SPACE.
        PERFORM SELECT_PLAN_DATA.
        IF G_PA = 'X'.
          PERFORM APPEND_PA_DATA.
        ENDIF.
        IF G_CCA = 'X'.
          PERFORM APPEND_CCA_DATA.
        ENDIF.
        PERFORM APPEND_IM_DATA.
        PERFORM APPEND_MANUAL.
        PERFORM GET_BALANCE_DATA.
        PERFORM REFRESH_DATA.
      ENDIF.
    WHEN 'REFRESH'.
      CLEAR  : G_PA, G_CCA, G_IM.
      ICLEAR : GT_MANUAL, GT_7000.
      PERFORM SELECT_CO_DATA.
      PERFORM SELECT_PLAN_DATA.
      PERFORM GET_BALANCE_DATA.
      PERFORM REFRESH_DATA.

    WHEN 'PRINT'.
      PERFORM DATA_PRINT.

    WHEN 'EXCEL'.
      PERFORM EXCEL_DOWN.

  ENDCASE.

  CLEAR OK_CODE.

ENDMODULE.                 " USER_COMMAND  INPUT

INCLUDE ZTRR00600F02.

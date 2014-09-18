*&---------------------------------------------------------------------*
*& Module pool       ZRMM_BDP                            *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

INCLUDE ZRMM_BDPTOP.
*&---------------------------------------------------------------------*
*&      Module  user_command_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9001 INPUT.
  OK_CODE = SY-UCOMM.
  CASE OK_CODE.
    WHEN 'UPLOAD'.
      CALL TRANSACTION 'ZRMM_BDP01'.
    WHEN 'CREATE'.
      CALL SCREEN '9120'.
*      CALL TRANSACTION 'ZRMM_BDP02'.
    WHEN 'EDIT'.
      CALL TRANSACTION 'ZRMM_BDP03'.
    when 'DISP'.
      CALL TRANSACTION 'ZRMM_BDP04'.
*    WHEN 'S2L'.
*      CALL TRANSACTION 'ZRMM_BDP05'.
    WHEN 'BACK' OR 'EXIT'.
      CLEAR OK_CODE.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " user_command_9001  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9001 OUTPUT.
  SET PF-STATUS 'ST9100'.
  SET TITLEBAR 'ST9100'.
ENDMODULE.                 " STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  leave_scr  INPUT
*&---------------------------------------------------------------------*
*       Module to exit from screen processing
*----------------------------------------------------------------------*
MODULE LEAVE_SCR INPUT.
  OK_CODE = SY-UCOMM.
  CASE OK_CODE.
    WHEN 'BACK' OR 'EXIT'.
      CLEAR OK_CODE.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " leave_scr  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9120  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9120 OUTPUT.
  SET PF-STATUS 'ST9120'.
  SET TITLEBAR 'ST9120'.
  if w_new is initial.
  else.
    perform initial_data.
  endif.
ENDMODULE.                 " STATUS_9120  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9120  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9120 INPUT.
  OK_CODE = SY-UCOMM.
  CASE OK_CODE.
    WHEN 'PROC'.
      PERFORM INSERT_DATA.
    WHEN 'BACK' OR 'EXIT'.
      CLEAR OK_CODE.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9120  INPUT
*&---------------------------------------------------------------------*
*&      Form  INSERT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INSERT_DATA.
  TABLES: ZTMM_BDP.
  DATA: L_MATNR LIKE MARD-MATNR,
        L_LGPLA LIKE ZTMM_BDP-LGPLA.

  SELECT SINGLE MATNR INTO L_MATNR
  FROM MARD
  WHERE MATNR = ZTMM_BDP-MATNR
    AND WERKS = ZTMM_BDP-WERKS
    AND LGORT = ZTMM_BDP-LGORT.
  IF SY-SUBRC <> 0.
    clear: w_new.
    MESSAGE W009(ZMMM) WITH 'No Data in Material Master(MARD)'.
    EXIT.
  ENDIF.

  SELECT SINGLE LGPLA INTO L_LGPLA
  FROM LAGP
  WHERE LGNUM = 'P01'
    AND LGPLA = ZTMM_BDP-LGPLA.
  IF SY-SUBRC <> 0.
      clear: w_new.
    MESSAGE W009(ZMMM) WITH 'BIN not found'.
    EXIT.
  ENDIF.
  SELECT SINGLE MATNR INTO L_MATNR
  FROM ZTMM_BDP
  WHERE MATNR = ZTMM_BDP-MATNR
    AND WERKS = ZTMM_BDP-WERKS
    AND LGORT = ZTMM_BDP-LGORT
    AND LGPLA = ZTMM_BDP-LGPLA.
  IF SY-SUBRC = 0.
      clear: w_new.
    MESSAGE W009(ZMMM) WITH 'Data has existed already'.
  ELSE.
     ZTMM_BDP-AEDAT = SY-DATUM.
    ZTMM_BDP-AEZET = SY-UZEIT.
    ZTMM_BDP-AENAM = SY-UNAME.

    INSERT ZTMM_BDP.
    IF SY-SUBRC = 0.
      COMMIT WORK.

      MESSAGE s009(ZMMM) WITH 'Data was saved'.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDIF.

ENDFORM.                    " INSERT_DATA
*&---------------------------------------------------------------------*
*&      Form  initial_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initial_data.
 clear: ZTMM_BDP.
* clear:  ZTMM_BDP-WERKS, ZTMM_BDP-LGORT,
*             ZTMM_BDP-LGPLA, ZTMM_BDP-FEEDER, ZTMM_BDP-DEL_METHOD,
*             ZTMM_BDP-ZUSAGE, ZTMM_BDP-ZOPTION, ZTMM_BDP-ZVARIANT,
*             ZTMM_BDP-APPS, ZTMM_BDP-ROUTE_CYCLE.

ENDFORM.                    " initial_data

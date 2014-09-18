*&---------------------------------------------------------------------*
*& Module pool       ZR_MM_VEHICLE_TRACKING                            *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*


INCLUDE ztop                                    .                      "

* INCLUDE ZO01                                    .                    *
* INCLUDE ZI01                                    .                    *
* INCLUDE ZF01                                    .                    *

*&---------------------------------------------------------------------*
*&      Module  user_command_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN 'CREATE'.
      CALL TRANSACTION 'ZTVT01'.
    WHEN 'CHANGE'.
      CALL TRANSACTION 'ZTVT02'.
    WHEN 'TRANSFER'.
      CALL TRANSACTION 'ZTVT03'.
    WHEN 'SCRAP'.
      CALL TRANSACTION 'ZTVT04'.
    WHEN 'VIEW'.
      CALL TRANSACTION 'ZTVT05'.

  ENDCASE.

ENDMODULE.                 " user_command_9001  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  SET PF-STATUS 'GUI_STATUS'.
  SET TITLEBAR 'TITLE'.

ENDMODULE.                 " STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  leave_scr  INPUT
*&---------------------------------------------------------------------*
*       Module to exit from screen processing
*----------------------------------------------------------------------*
MODULE leave_scr INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN 'E' OR 'ENDE' OR 'ECAN'.
      CLEAR ok_code.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " leave_scr  INPUT

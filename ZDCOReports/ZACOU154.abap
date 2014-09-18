*&---------------------------------------------------------------------*
*& Report  ZACOU154
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
*&--------------------------------------------------------------------*
* Program name  : ZACOU154
* Description   : Outbound D/O Auto Creation Program
* Developer     : T00281
* Creation date : 06/27/2012
*&--------------------------------------------------------------------*
* Change date |  Name     | Comment
* 1/28/13   Deactivate the program requested by MIT
*&--------------------------------------------------------------------*
REPORT  zacou154 MESSAGE-ID zmco.

INCLUDE zacou154_top.
INCLUDE zacou154_f01.
INCLUDE zacou154_o01.
INCLUDE zacou154_i01.

*=====================================================================*
* INITIALIZATION
*=====================================================================*
INITIALIZATION.
  PERFORM set_initial.

*=====================================================================*
* AT SELECTION-SCREEN
*=====================================================================*
AT SELECTION-SCREEN.

*=====================================================================*
* AT SELECTION-SCREEN OUTPUT
*=====================================================================*
AT SELECTION-SCREEN OUTPUT.

*=====================================================================*
* START-OF-SELECTION
*=====================================================================*
START-OF-SELECTION.
*1/28/13   Deactivate the program requested by MIT
  MESSAGE e000 with 'The program deactivated'.
  PERFORM get_data.
*=====================================================================*
* END-OF-SELECTION
*=====================================================================*
END-OF-SELECTION.

  CHECK g_sflag <> 'X'.
  IF lines( gt_record ) > 0.
    CALL SCREEN 100.
  ELSE.
    MESSAGE s026 DISPLAY LIKE 'E'.
  ENDIF.

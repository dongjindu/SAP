************************************************************************
* Program Name      : ZEMMGM17E_6024
* Author            : Hakchin Kim
* Creation Date     : 2003.12.10.
* Specifications By : Hakchin Kim
* Pattern           : Report 1-1
* Development Request No : EMMGM17
* Addl Documentation: F/S - EMMGM17 Class Upload
* Description       : SAP MM Class Upload
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
************************************************************************
REPORT zemmgm17e_6024
                 MESSAGE-ID zmmm
                 NO STANDARD PAGE HEADING
                 LINE-SIZE 400.

INCLUDE zemmgm17e_6024top.   "Data Declaration
INCLUDE zemmgm17e_6024cla.   "Class Part
INCLUDE zemmgm17e_6024o01.   "PBO Part
INCLUDE zemmgm17e_6024i01.   "PAI Part
INCLUDE zemmgm17e_6024f01.   "Perform Library
*--------- Report Transactin Execution --------------------------------*
INITIALIZATION.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f4_ws_filename_get
               CHANGING p_file.


START-OF-SELECTION.
  PERFORM ps_tb.  "PF-STATUS & TITLEBAR
  PERFORM get_data.    "get data

  IF it_class_data IS INITIAL.
    MESSAGE s999(zmmm) WITH 'There is no data!'(001).
    EXIT.
  ENDIF.
  PERFORM display_data.

*---------- List Procession Events ------------------------------------*
TOP-OF-PAGE.
*  PERFORM make_col_heading.

AT USER-COMMAND.
  IF sy-ucomm = 'CREA'.  "Create Characteristic
    IF sy-lsind > 1. sy-lsind = sy-lsind - 1. ENDIF.
    PERFORM process_data.   "Process Data
    PERFORM display_log.    "Display Data Log
  ENDIF.

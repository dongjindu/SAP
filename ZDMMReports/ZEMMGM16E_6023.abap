************************************************************************
* Program Name      : ZEMMGM16E_6023
* Author            : Hakchin Kim
* Creation Date     : 2003.12.10.
* Specifications By : Hakchin Kim
* Pattern           : Report 1-1
* Development Request No : EMMGM16
* Addl Documentation: F/S - EMMGM16 Characteristic Upload
* Description       : SAP MM Characteristic Upload
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
************************************************************************
REPORT zemmgm16e_6023
                 MESSAGE-ID zmmm
                 NO STANDARD PAGE HEADING
                 LINE-SIZE 400.

INCLUDE zemmgm16e_6023top.   "Data Declaration
INCLUDE zemmgm16e_6023cla.   "Class Part
INCLUDE zemmgm16e_6023o01.   "PBO Part
INCLUDE zemmgm16e_6023i01.   "PAI Part
INCLUDE zemmgm16e_6023f01.   "Perform Library
*--------- Report Transactin Execution --------------------------------*
INITIALIZATION.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f4_ws_filename_get
               CHANGING p_file.

START-OF-SELECTION.
  PERFORM ps_tb.  "PF-STATUS & TITLEBAR
  PERFORM get_data.    "get data

  IF it_charact_data IS INITIAL.
    MESSAGE s999(zmmm) WITH 'There is no data!'(001).
    EXIT.
  ENDIF.
  PERFORM display_data.   "Display File Data

*---------- List Procession Events ------------------------------------*
TOP-OF-PAGE.
*  PERFORM make_col_heading.

AT USER-COMMAND.
  IF sy-ucomm = 'CREA'.  "Create Characteristic
    IF sy-lsind > 1. sy-lsind = sy-lsind - 1. ENDIF.
    PERFORM process_data.   "Process Data
    PERFORM display_log.    "Display Data Log
  ENDIF.

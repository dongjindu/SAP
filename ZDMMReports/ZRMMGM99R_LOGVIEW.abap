REPORT zrmmgm99r_logview
             NO STANDARD PAGE HEADING
             LINE-SIZE  120
             LINE-COUNT 90    "(7)
             MESSAGE-ID zmmm. "MessageClassDeclaration
INCLUDE zrmmgm99r_logviewtop. "Data Declaration
INCLUDE zrmmgm99r_logviewcla. "Class
INCLUDE zrmmgm99r_logviewf01. "Library
INCLUDE zrmmgm99r_logviewo01. "PBO
INCLUDE zrmmgm99r_logviewi01. "PAI

*--------- Report Transactin Execution --------------------------------*
INITIALIZATION.

*/ For User
  MOVE: 'I'      TO s_zuname-sign,
        'BT'     TO s_zuname-option,
        sy-uname TO s_zuname-low.
*        sy-uname TO s_zuname-high.
  APPEND s_zuname.

*/ For Log Date
  fr_logdate = sy-datum - 1.
  to_logdate = sy-datum.
  MOVE: 'I'        TO s_zdatum-sign,
        'BT'       TO s_zdatum-option,
        fr_logdate TO s_zdatum-low,
        to_logdate TO s_zdatum-high.
  APPEND s_zdatum.


AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN.

START-OF-SELECTION.  "event block for creating lists
  PERFORM get_it_ztlog.   "Get Log Data
  IF it_ztlog IS INITIAL.
    MESSAGE s999(zmmm) WITH 'There is no data!'.
  ELSE.
    CALL FUNCTION 'Z_FMM_6001_02_DISPLAY_LOG'
         TABLES
              imt_ztlog = it_ztlog.
  ENDIF.

END-OF-SELECTION.

*---------- List Procession Events ------------------------------------*
TOP-OF-PAGE.

END-OF-PAGE.

AT LINE-SELECTION.

AT USER-COMMAND.

TOP-OF-PAGE DURING LINE-SELECTION.

*&------------------------------------------------------------------
*& Program ID     : ZMMR80300T
*& Program Name   : ERSA Min / Max Maintain
*& Created by     : Yang
*& Created on     : 04.29.2009
*& Development ID : MM-0XX
*& Reference Pgm. :
*& Description    : ERSA Min / Max Maintain
*&
*& Modification Log
*&====================================================================
*& Date        Developer      Request ID      Description
*& 04.29.2009   Yang                       First development
*&--------------------------------------------------------------------
REPORT zmmr80300t   LINE-SIZE 154 LINE-COUNT 58 MESSAGE-ID zmmm
                    NO STANDARD PAGE HEADING.
*----------------------------------------------------------------------*
*   includes
*----------------------------------------------------------------------*
INCLUDE zmmr80300_top.
INCLUDE zmmr00000_com.
INCLUDE zmmr80300_cls.
INCLUDE zmmr80300_frm.
INCLUDE zmmr80300_pai.
INCLUDE zmmr80300_pbo.

*--------------------------------------------------------------------*
*   INITIALIZATION                                                   *
*--------------------------------------------------------------------*
INITIALIZATION.
*  PERFORM init_data.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_LGORT-LOW.
  PERFORM HELP_REQUEST_S_LGORT.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN Event                                            *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM screen_modify.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR fname.
  PERFORM selection_screen USING fname.


*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
  PERFORM p0000_indicator.
  PERFORM p1000_initial_data.

  CASE 'X'.
    WHEN ra.
      PERFORM p1000_select_data.
    WHEN ra_1.
      PERFORM get_upload_file .
  ENDCASE.
      PERFORM p2000_process_data.

  CALL SCREEN 100.


*----------------------------------------------------------------------*
END-OF-SELECTION.
*----------------------------------------------------------------------*

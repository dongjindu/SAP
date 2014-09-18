*&---------------------------------------------------------------------*
*& Report  ZRHR_COMPE_RESULT
*&
*&---------------------------------------------------------------------*
*& Program name      : ZRHR_COMPE_RESULT
*& Creation date     : 03/16/2013
*& Writer            : T00289
*&---------------------------------------------------------------------*
*& Description       :
*& 1. Annual Competency Result Report
*&---------------------------------------------------------------------*
*& Modified date     :
*& Modified user     :
*&---------------------------------------------------------------------*

report  zrhr_compe_result message-id zmhrpms.

include zrhr_compe_resulttop.
include zrhr_compe_resultf01.
include zrhr_compe_resulto01.
include zrhr_compe_resulti01.


*----------------------------------------------------------------------*
*   INITIALIZATION
*----------------------------------------------------------------------*
initialization.
  perform set_init_value.       " set initial value

*----------------------------------------------------------------------*
*   AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
at selection-screen output.
  perform set_droplist_year.    " set droplist Year
  perform set_droplist_rating.  " set droplist Rating
  perform set_droplist_compg.   " set droplist Competency Group
*   07/22/2013 - T00306 Start
  perform set_droplist_status.  " set droplist Status

  case sy-ucomm.
    when 'COMPG'.
      perform set_droplist_compt.   " set droplist Competency
    when 'COMPT'.
      perform set_droplist_beatt.   " set droplist Behavioral Attribute
  endcase.
*   07/26/2013 - T00306 End

*----------------------------------------------------------------------*
*   AT SELECTION-SCREEN ON VALUE-REQUEST FOR
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*   AT SELECTION-SCREEN
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*   START-OF-SELECTION
*----------------------------------------------------------------------*
*START-OF-SELECTION.
*  PERFORM get_data.

*----------------------------------------------------------------------*
*   END-OF-SELECTION
*----------------------------------------------------------------------*
end-of-selection.
  call screen 100.

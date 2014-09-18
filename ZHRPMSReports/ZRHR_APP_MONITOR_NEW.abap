
*& Report  ZRHR_APP_MONITOR
*&
*&---------------------------------------------------------------------*
*& Program name      : ZRHR_APP_MONITOR
*& Creation date     : 03/14/2013
*& Writer            : T00289
*&---------------------------------------------------------------------*
*& Description       :
*& 1. Monitoring of Appraisal Progress
*&---------------------------------------------------------------------*
*& Modified date     :
*& Modified user     :
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------
*  Modification Log
*  Date        Developer Issue No    Description
*======================================================================
*  03/27/2013  Valerian  UD1K956832  Select approver based on current
*                                    date.
*  04/30/2013  Valerian  UD1K957000  Remove prefix (Mr/Mrs/Miss/Ms)
*                                    Add col org.unit and description
*                                    Add email notification function
*----------------------------------------------------------------------

report  zrhr_app_monitor_new message-id zmhrpms.

include zrhr_app_monitortop_new.
include zrhr_app_monitorf01_new.
include zrhr_app_monitoro01_new.
include zrhr_app_monitori01_new.

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
  perform set_droplist_year2.
  perform set_droplist_status.  " set droplist Status
  perform set_droplist_substatus2 using p_st2.

*** 07/08/2013 T00306 Start
  loop at screen.
    if screen-group1 = 'ALV'.
      screen-input = 0.
      modify screen.
    endif.
  endloop.

*----------------------------------------------------------------------*
*   AT SELECTION-SCREEN ON VALUE-REQUEST FOR
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*   AT SELECTION-SCREEN
*----------------------------------------------------------------------*
at selection-screen.
*  if sy-ucomm = 'ST'.
*    perform set_droplist_substatus2 using p_st2.
*  endif.
  p_year = p_year2.
  p_st = p_st2.
  p_subst = p_subst2.

*----------------------------------------------------------------------*
*   START-OF-SELECTION
*----------------------------------------------------------------------*
start-of-selection.
  perform get_data.

  if not r_disp is initial.
    call screen 100.
  elseif not p_subst2 is initial.
    perform email_processing.
  else.
    message s011 with text-m02 display like 'E'.
    stop.
  endif.
*----------------------------------------------------------------------*
*   END-OF-SELECTION
*----------------------------------------------------------------------*
end-of-selection.

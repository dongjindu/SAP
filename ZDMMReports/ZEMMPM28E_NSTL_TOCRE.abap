************************************************************************
* Program Name      : ZEMMPM28E_NSTL_TOCRE
* Author            : Hakchin Kim
* Creation Date     : 2003.11.24.
* Specifications By : Hakchin Kim
* Pattern           : Report 1-1
* Development Request No : UD1K901598
* Addl Documentation:
* Description       : Daily Supply to Line (Non Supply to Line)
*                     Transfer Order Creation & Header Change
* Modification Logs
* Date         Developer       RequestNo      Description
* 2003.11.25.  Hakchin Kim     UD1K901864     Initial Coding
*
************************************************************************
REPORT zemmpm28e_nstl_tocre
              MESSAGE-ID zmmm
              NO STANDARD PAGE HEADING
              LINE-SIZE 400.

**--- insert by stlim (2004/04/29)
INCLUDE : zrmmpmxxr_incl.
**--- end of insert

INCLUDE zemmpm28e_nstl_tocretop.   "Data Declaration
INCLUDE zemmpm28e_nstl_tocref01.   "Perform Library

**--- insert by stlim (2004/04/29)
INITIALIZATION.
  PERFORM event_build USING w_eventcat[].
**--- end of insert


*--------- Report Transactin Execution --------------------------------*
START-OF-SELECTION.  "event block for creating lists
  PERFORM get_it_ztmm_nstl.       "Raw Data
  PERFORM get_it_matnr_date_time. "Raw Data by time
  PERFORM get_it_data_for_to.     "Raw Data for T/O Creation

  IF it_data_for_to IS INITIAL.
    MESSAGE s999(zmmm) WITH 'There is no data!'(001).
  ELSE.
    PERFORM process_it_data_for_to.      "Process Data in Forground.
*    PERFORM process_it_data_for_to_bgd.  "Process Data in Background.
  ENDIF.

END-OF-SELECTION.

**--- insert by stlim (2004/04/29)
  PERFORM update_table.
  PERFORM comment_build.     " USING w_top_of_page[].
  PERFORM make_alv_grid.
**--- end of insert

*---------- List Procession Events ------------------------------------*
TOP-OF-PAGE.

**--- insert by stlim (2004/04/29)
  PERFORM top_of_page.
**--- end of insert

*  PERFORM make_col_heading.

END-OF-PAGE.

AT LINE-SELECTION.

AT USER-COMMAND.

TOP-OF-PAGE DURING LINE-SELECTION.

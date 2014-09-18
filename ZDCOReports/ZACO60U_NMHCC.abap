************************************************************************
* Program Name      : ZACO05U_MHCC
* Author            : Hyung Jin Youn
* Creation Date     : 2003.10.14
* Specifications By : Dong-Hwan Kim
* Pattern           : Report 1-1
* Development Request No : UD1K902723
* Addl Documentation:
* Description       : This program will allocate variance M/Hs
*                     which are calculated from timesheet, B/F data
*                     and PCC data
*                     This program will generate data for posting,
*                     and post them using MFBF(REM Backflush) and
*                     Production order time ticket
* Modification Logs
*   Date       Developer    RequestNo    Description
*#1 03/09/2005 WSKIM        UD1K914876   WIP ERROR
* 18/06/2013  T00303   UD1K957381   U1: Apply archiving
************************************************************************

* For TOP include
INCLUDE zaco60u_1top.
*INCLUDE ZACO05U_1TOP.
* For Sub-routine
INCLUDE zaco60l_f001.
*INCLUDE ZACO05L_F001.
* For Sub-routine - Posting
INCLUDE zaco60l_f002.
*INCLUDE ZACO05L_F002.


*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
* 'Fert' 'Halb' are default.
  PERFORM default_value_s_mtart.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
* Check period range
  PERFORM check_period_range.
* Searching for CCtr group
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_ncoal.
  PERFORM read_cctr_group.


*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Calculating Period Count
  PERFORM cal_per_count.
* Controlling Area Information
  PERFORM read_tka01.
* Read CCtrs
  PERFORM read_cctr.
* Enqueue ZTCO_NMHHRTRANS
  PERFORM enqueue_ztco_nmhhrtrans.
* Enqueue ZTCO_NMHHRTRANS
  PERFORM enqueue_ztco_nmhpcpost.
* Read resource_information
  PERFORM read_resource_data.
* Set MFBF Variant
  PERFORM set_mfbf_init.

*  CASE p_revs.
* posting
*    WHEN space.
  PERFORM post_process.
* Reversing
*    WHEN 'X'.
*      PERFORM reverse_process.
*  ENDCASE.


*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Print Result
  PERFORM result_list.
*






*

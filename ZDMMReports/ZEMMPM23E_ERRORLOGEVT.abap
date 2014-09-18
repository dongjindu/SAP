************************************************************************
* Program name : ZEMMPM23E_ERRORLOG
* Created by   : Min-su Park
* Created on   : 2003.11.03.
* Pattern      :
* Description  :
*   1. Modified Welcome screen-For Inbound delivery-Putaway process    *
*   2. Empty Container management                                      *
*                                                                      *
* Modification Log                                                     *
* Date            Developer        Request No.    Description          *
* 2003.11.03.     Min-su Park      UD1K901873     Initial Coding       *
*                                                                      *
************************************************************************

*----------------------------------------------------------------------*
*   INCLUDE ZEMMPM23E_ERRORLOGEVT                                      *
*----------------------------------------------------------------------*

AT SELECTION-SCREEN.
  PERFORM make_basic_data.
  PERFORM alv_field_build.


START-OF-SELECTION.

  w_layout-colwidth_optimize = 'X'.
  w_layout-info_fieldname    = 'LINECOLOR'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_bypassing_buffer       = 'X'
            i_callback_program       = w_repid
            i_callback_pf_status_set = 'SET_STATUS'
            i_callback_user_command  = 'USER_COMMAND'
            is_layout                = w_layout
            i_save                   = 'A'
            it_events                = wa_events[]
            it_fieldcat              = it_fieldcat[]
       TABLES
            t_outtab                 = it_error.

*----------------------------------------------------------------------
* Program ID        : ZRCO_BULK_MATERIAL
* Title             : [CO] Bulk Material Variance Report
* Created on        : 07/30/2013
* Created by        : Sunho Jeong
* Specifications By : Reddy Bhimanapati
* Description       :

*----------------------------------------------------------------------
*----------------------------------------------------------------------

report  zrpp_bulk_material message-id zmco.

include zrpp_bulk_materialtop.

*----------------------------------------------------------------------*
* SELECTION SCREEN
*----------------------------------------------------------------------*
*& PROGRAM-LOGIC : #0001 Selection Screen &*
*& PROGRAM-DESC :  Selection Screen for receiving data condition &*
selection-screen begin of block bl1 with frame.
parameters: p_bdatj like zsco_bulk_material-bdatj obligatory,
            p_poper like zsco_bulk_material-poper obligatory.
parameters: p_zresp type char01 default 'V' as listbox user-command re
                                visible length 20 obligatory .
select-options: s_zgrp3 for zsco_bulk_material-zgrp3 no intervals
                                                     modif id mod,
                s_zgrp1 for zsco_bulk_material-zgrp1 no intervals,
                s_matnr for zsco_bulk_material-matnr no intervals.
selection-screen begin of line.
selection-screen comment 1(4) text-001 for field p_save.
parameters: p_save type char01 as checkbox.
selection-screen end of line.
selection-screen end of block bl1.

selection-screen begin of block bl2 with frame.
parameters: p_varia like disvariant-variant.
selection-screen end of block bl2.
*& END-PROGRAM-LOGIC : #0001 &*

include zhmmacommon.
include zrpp_bulk_materialf01.


*----------------------------------------------------------------------*
* INITIALIZAITON
*----------------------------------------------------------------------*
initialization.
  perform initialization.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
at selection-screen on value-request for p_varia.
  perform f4_layout_variant.

at selection-screen output.
  perform set_droplist_shop.

  loop at screen.
    if screen-group1 eq 'MOD'.
      case p_zresp.
        when 'V'.
          screen-active = 1.
        when 'E'.
          screen-active = 0.
      endcase.
    endif.
    modify screen.
  endloop.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
start-of-selection.

  if sy-batch eq 'X'.
    p_save = sy-batch.
  endif.

  if p_save eq 'X'.
    perform get_data.
  else.
    perform get_data_from_bw.
  endif.

end-of-selection.
  if it_list[] is initial.
    message s000 with 'No data'.
  else.
    if p_save eq 'X'.
      perform save_data.
    else.
      call screen 0100.
    endif.
  endif.

*----------------------------------------------------------------------*
*  MODULE STATUS_0100 OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
module status_0100 output.

  set pf-status 'G_0100'.
  set titlebar  'T_0100'.

endmodule.                    "STATUS_0100 OUTPUT
*----------------------------------------------------------------------*
*  MODULE CREATE_ALV_GRID OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
module create_alv_grid output.

  perform create_alv_grid.

endmodule.                    "CREATE_ALV_GRID OUTPUT
*----------------------------------------------------------------------*
*  MODULE USER_COMMAND INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
module user_command input.

  g_save_ok = ok_code.
  clear ok_code.

  case g_save_ok.
    when 'SAVE'.
      perform save_data.
  endcase.

endmodule.                    "USER_COMMAND_0100 INPUT
*----------------------------------------------------------------------*
*  MODULE EXIT INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
module exit input.

  g_save_ok = ok_code.
  clear ok_code.

  perform free_object.

  if g_save_ok = 'EXIT'.
    leave program.
  else.
    leave to screen 0.
  endif.

endmodule.                    "EXIT INPUT
*----------------------------------------------------------------------*
*  MODULE GET_CURSOR_WITH_TAB INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
module get_cursor_with_tab input.

  if g_cursor is initial.
    get cursor field g_cursor.
  endif.

endmodule.                    "GET_CURSOR_WITH_TAB INPUT

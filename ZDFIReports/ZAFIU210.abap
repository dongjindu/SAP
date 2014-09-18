*&---------------------------------------------------------------------*
*& Report  ZAFIU210
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report  zafiu210 message-id zmfi.
tables: ztfi_imfm, sscrfields,
        imavz, imak, zsfiu20.

data  : gv_waers  type waers.

data :  l_sel_button type smp_dyntxt.
include : <icon>.

data itab type table of sy-ucomm.

*SE16N
data: gt_se16stab   like se16n_seltab occurs 0 with header line.

include zafiu210top.
*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
selection-screen begin of block bl1 with frame.
parameters: p_bukrs like bkpf-bukrs memory id buk obligatory.
parameters:
  p_ayear like ztfi_imfm-ayear memory id gjr obligatory,
  p_gubun like ztfi_imfm-gubun default '1' modif id gub.
select-options:
  s_posid for ztfi_imfm-posid.
parameters:p_versi like imavz-versi DEFAULT '0'.
selection-screen begin of line.
parameters: p_stflg like imak-stratflg default 'X'.
selection-screen comment 2(31) text-007 for field p_stflg.
selection-screen end of line.
selection-screen skip 1.
parameters: p_max type sytabix default 5000 modif id gub.
selection-screen end of block bl1.

selection-screen skip 1.
parameters: p_copy as checkbox user-command chk.
selection-screen begin of line.
parameters: p_plan type char01 radiobutton group rad1 default 'X'
                   user-command ch1.
selection-screen comment 5(30) text-001 for field p_plan.
parameters: p_bugt type char01 radiobutton group rad1.
selection-screen comment 38(32) text-002 for field p_bugt.
selection-screen end of line.

*  call function 'AUTHORITY_CHECK_TCODE'
*    exporting
*      tcode  = 'SE16N'
*    exceptions
*      ok     = 0
*      not_ok = 1.
*  if sy-subrc ne 0.
*    message e059(eu) with 'SE16N'.  " no authority
*  endif.

include zhmmacommon.
include zafiu210f01.


initialization.
  p_bukrs = 'H201'.

at selection-screen.

  if sy-ucomm eq 'CHK'.
    if p_copy eq 'X'.
      p_versi = '000'.
    endif.
  endif.


at selection-screen output.

  loop at screen.
    case screen-name.
      when 'P_BUKRS'.
        screen-input = 0.
        screen-intensified = 1.
        modify screen.
      when 'P_PLAN' or 'P_BUGT' or 'P_STFLG'.
        if p_copy eq 'X'.
          screen-active = 1.
        else.
          screen-active = 0.
        endif.
        modify screen.
      when 'P_VERSI'.
        if p_copy eq 'X' and p_bugt eq 'X'.
          screen-input = 0.
          screen-intensified = 1.
        else.
          screen-input = 1.
          screen-intensified = 0.
        endif.
        modify screen.
    endcase.

    case screen-group1.
      when 'GUB'.
        if p_copy eq 'X'.
          screen-active = 0.
        else.
          screen-active = 1.
        endif.
        modify screen.
    endcase.

  endloop.


*----------------------------------------------------------------------*
start-of-selection.
*----------------------------------------------------------------------*

  select single waers into gv_waers from t001
         where bukrs = p_bukrs.
  check sy-subrc = 0.

  if p_copy eq 'X'.
    if p_versi is initial.
      message s000 with 'Version is required' display like 'E'.
      exit.
    endif.
    perform get_ar_monthly_amount.
    if it_list[] is initial.
      message s000 with 'No data' display like 'E'.
      stop.
    else.
      call screen '0100'.
    endif.
  else.
*show ALV editing screen
    perform show_se16n.
  endif.

*----------------------------------------------------------------------*
*  MODULE user_command INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
module user_command input.

  g_save_ok = ok_code.
  clear ok_code.

  case g_save_ok.
    when 'EXEC'.
      perform copy_to_pi.
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

endmodule.                    "exit INPUT
*----------------------------------------------------------------------*
*  MODULE status_0100 OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
module status_0100 output.

  set pf-status 'G_0100'.

  if p_plan eq 'X'.
    set titlebar  'T_0100' with text-001.
  else.
    set titlebar  'T_0100' with text-002.
  endif.

endmodule.                    "status_0100 OUTPUT
*----------------------------------------------------------------------*
*  MODULE create_alv_grid OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
module create_alv_grid output.

  perform create_alv_grid.

endmodule.                    "create_alv_grid OUTPUT

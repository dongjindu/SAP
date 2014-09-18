*----------------------------------------------------------------------
* Program ID        : ZRFI_OPEN_PO
* Title             : [FI] Open Purchase Orders Report
* Created on        : 08/26/2013
* Created by        : Sunho Jeong
* Specifications By : Ravikumar, Sunkara Zensar
* Description       :

*----------------------------------------------------------------------
*----------------------------------------------------------------------
report  zrfi_open_po message-id zmco.

include zrfi_open_potop.

*----------------------------------------------------------------------*
* SELECTION SCREEN
*----------------------------------------------------------------------*
*& PROGRAM-LOGIC : #0001 Selection Screen &*
*& PROGRAM-DESC :  Selection Screen for receiving data condition &*
selection-screen begin of block bl1 with frame.
parameters: p_bukrs like zsfi_open_po-bukrs modif id man.
select-options: s_ebeln for zsfi_open_po-ebeln,
                s_ebelp for zsfi_open_po-ebelp,
                s_aedat for zsfi_open_po-aedat,
                s_bstyp for zsfi_open_po-bstyp modif id man
                                  no intervals no-extension,
                s_knttp for ekpo-knttp.
*PARAMETERS: p_grate type p DECIMALS 2 DEFAULT 1,
PARAMETERS:     p_irate type p DECIMALS 2 DEFAULT 1.

selection-screen end of block bl1.

selection-screen begin of block bl2 with frame.
parameters: p_varia like disvariant-variant.
selection-screen end of block bl2.
*& END-PROGRAM-LOGIC : #0001 &*

selection-screen begin of block bl3 with frame.
SELECTION-SCREEN COMMENT /1(70) TEXT-002.
SELECTION-SCREEN COMMENT /5(70) TEXT-003.
SELECTION-SCREEN COMMENT /5(70) TEXT-004.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT /1(70) TEXT-005.
SELECTION-SCREEN COMMENT /1(70) TEXT-006.
selection-screen end of block bl3.

include zhmmacommon.
include zrfi_open_pof01.


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
  perform set_selection_screen.


*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
start-of-selection.

  perform get_data.

end-of-selection.
  if it_list[] is initial.
    message s000 with 'No data'.
  else.
    call screen 0100.
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

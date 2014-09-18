*----------------------------------------------------------------------*
*   INCLUDE MZAPP272_HPC_ORDER_PLANO01                                 *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_APP272 output.
  set pf-status 'PF_9000'.
  case okcode_02.
    when 'ORDER'.
      p_maker_app272 = 'HMC'.
      sv_prog_app272    =  sy-repid.
      sv_dynnr_app272   = '3110'.
      set titlebar '9000' with 'HPC CODE ORDER PLAN'.
    when 'ALC_1'.
      p_maker_app272 = 'HMC'.
      sv_prog_app272    =  sy-repid.
      sv_dynnr_app272   = '3111'.
      set titlebar '9000' with
          'HPC To ALC Comparison'.
    when 'ALC_2'.
      p_maker_app272 = 'HMC'.
      sv_prog_app272    =  sy-repid.
      sv_dynnr_app272   = '3112'.
      set titlebar '9000' with
          'HPC To ALC Comparison item administration'.
    when others.
      p_maker_app272 = 'HMC'.
      sv_prog_app272    =  sy-repid.
      sv_dynnr_app272   = '3110'.
      set titlebar '9000' with 'HPC CODE ORDER PLAN'.
  endcase.
endmodule.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_LISTBOX  OUTPUT
*&---------------------------------------------------------------------*
module set_listbox_APP272 output.
  perform set_listbox_APP272.
endmodule.                 " SET_LISTBOX  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INITIALIZATION  OUTPUT
*&---------------------------------------------------------------------*
module initialization_APP272 output.
*  CASE OKCODE_02.
*    WHEN 'ORDER'.
*      P_MAKER_APP272 = 'HMC'.
*      SV_PROG_APP272    =  SY-REPID.
*      SV_DYNNR_APP272   = '9001'.
*    WHEN 'ALC_1'.
*
*    WHEN 'ALC_2'.
*
*    WHEN OTHERS.
*      P_MAKER_APP272 = 'HMC'.
*      SV_PROG_APP272    =  SY-REPID.
*      SV_DYNNR_APP272   = '9001'.
*  ENDCASE.
endmodule.                 " INITIALIZATION  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
module display_APP272 output.
  perform display_APP272.
endmodule.                 " DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_9002  OUTPUT
*&---------------------------------------------------------------------*
module display_APP272_02 output.
  perform display_APP272_02.
endmodule.                 " DISPLAY_9002  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_9003  OUTPUT
*&---------------------------------------------------------------------*
module display_APP272_03 output.
  perform display_APP272_03.
endmodule.                 " DISPLAY_9003  OUTPUT

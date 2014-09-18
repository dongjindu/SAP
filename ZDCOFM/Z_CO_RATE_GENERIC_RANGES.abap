function z_co_rate_generic_ranges .
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_CC) LIKE  ZCO140_GEN-CC DEFAULT SPACE
*"     VALUE(I_CE) LIKE  ZCO140_GEN-CE DEFAULT SPACE
*"  TABLES
*"      T_R_CC
*"      T_R_CE
*"----------------------------------------------------------------------

  if not i_CC is initial.

    perform fill_r_CC using i_CC.

    refresh t_r_CC.

    loop at g_r_CC.
      move g_r_CC to t_r_CC.
      append t_r_CC.
    endloop.

  endif.

  if not i_CE is initial.

    perform fill_r_CE using i_CE.

    refresh t_r_CE.

    loop at g_r_CE.
      move g_r_CE to t_r_CE.
      append t_r_CE.
    endloop.

  endif.


endfunction.

*---------------------------------------------------------------------*
*       FORM FILL_R_CE                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_CE                                                     *
*---------------------------------------------------------------------*
form fill_r_CE using p_CE.

  data: l_CE like zco140_gen-CE,
        l_lines like sy-tabix,
        l_fdpos like sy-fdpos.

  describe table g_r_CE lines l_lines.
  if not l_lines is initial.
    read table g_r_CE index 1.
    check g_r_CE-low <> p_CE.
  endif.

  refresh g_r_CE.
  move p_CE to l_CE.

  if l_CE ca ' '.
  endif.

  move sy-fdpos to l_fdpos.

  move: space   to g_r_CE,
        'I'     to g_r_CE-sign,
        'EQ'    to g_r_CE-option,
        l_CE to g_r_CE-low.

  append g_r_CE.

  while l_fdpos => 0.
    write '*' to l_CE+l_fdpos(1).
    move l_CE to g_r_CE-low.
    append g_r_CE.
    write space to l_CE+l_fdpos(1).
    subtract 1 from l_fdpos.
  endwhile.

endform.                               " FILL_R_CE

*---------------------------------------------------------------------*
*       FORM FILL_R_CC                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_CC                                                     *
*---------------------------------------------------------------------*
form fill_r_CC using p_CC.

  data: l_CC like zco140_gen-CC,
        l_lines like sy-tabix,
        l_fdpos like sy-fdpos.

  describe table g_r_CC lines l_lines.
  if not l_lines is initial.
    read table g_r_CC index 1.
    check g_r_CC-low <> p_CC.
  endif.

  refresh g_r_CC.
  move p_CC to l_CC.

  if l_CC ca ' '.
  endif.

  move sy-fdpos to l_fdpos.

  move: space   to g_r_CC,
        'I'     to g_r_CC-sign,
        'EQ'    to g_r_CC-option,
        l_CC to g_r_CC-low.

  append g_r_CC.

  while l_fdpos => 0.
    write '*' to l_CC+l_fdpos(1).
    move l_CC to g_r_CC-low.
    append g_r_CC.
    write space to l_CC+l_fdpos(1).
    subtract 1 from l_fdpos.
  endwhile.

endform.                               " FILL_R_CC

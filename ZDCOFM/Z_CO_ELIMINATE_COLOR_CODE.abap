function z_co_eliminate_color_code.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      P_T_NAFTA STRUCTURE  ZSCO_NAFTA_CK11
*"      GT_COLOR STRUCTURE  ZSCO_COLOR
*"----------------------------------------------------------------------
  data: $strlen type i.
  data: $ix like sy-tabix.
  data :
        $verpr   type verpr,
        $gpreis  type zgpreis,
        $peinh   type ck_kpeinh,
        $losgr   type ck_losgr,
        $meins   type meins,
        $l_qty   type menge_pos,
        $l_amt   type zetotam.

  data $p_t_nafta like zsco_nafta_ck11 occurs 0 with header line.

  loop at p_t_nafta.
    $ix = sy-tabix.
    $strlen = strlen( p_t_nafta-compn ).
    if p_t_nafta-compn+5(1) eq 'M'.
      gt_color-matnr_i = p_t_nafta-compn.
      p_t_nafta-compn = p_t_nafta-compn+(12).
      gt_color-matnr_o = p_t_nafta-compn.
      gt_color-t_qty = p_t_nafta-reqqt.
      gt_color-matnr = p_t_nafta-artnr.
      collect gt_color.
    else.
      if $strlen > 11.
        case $strlen.
          when 12 or 13.
            gt_color-matnr_i = p_t_nafta-compn.
            p_t_nafta-compn = p_t_nafta-compn+(10).
            gt_color-matnr_o = p_t_nafta-compn.
            gt_color-t_qty = p_t_nafta-reqqt.
            gt_color-matnr = p_t_nafta-artnr.
            collect gt_color.
          when 14 or 15.
            gt_color-matnr_i = p_t_nafta-compn.
            p_t_nafta-compn = p_t_nafta-compn+(12).
            gt_color-matnr_o = p_t_nafta-compn.
            gt_color-t_qty = p_t_nafta-reqqt.
            gt_color-matnr = p_t_nafta-artnr.
            collect gt_color.
        endcase.
      else.
        p_t_nafta-compn = p_t_nafta-compn.
      endif.
    endif.
    modify p_t_nafta index $ix.
  endloop.

  loop at p_t_nafta.
    $p_t_nafta = p_t_nafta.

    $verpr   = p_t_nafta-verpr.
    $gpreis  = p_t_nafta-gpreis.
    $peinh   = p_t_nafta-peinh.
    $losgr   = p_t_nafta-losgr.
    $l_qty   = p_t_nafta-l_qty.
    $l_amt   = p_t_nafta-l_amt.

    collect $p_t_nafta.
    move :
      $verpr   to $p_t_nafta-verpr,
      $gpreis  to $p_t_nafta-gpreis,
      $peinh   to $p_t_nafta-peinh,
      $losgr   to $p_t_nafta-losgr,
      $l_qty   to $p_t_nafta-l_qty,
      $l_amt   to $p_t_nafta-l_amt.

    modify $p_t_nafta index sy-tabix transporting
                              verpr gpreis peinh losgr l_qty l_amt.

  endloop.
  __cls p_t_nafta.
  p_t_nafta[] = $p_t_nafta[].

endfunction.

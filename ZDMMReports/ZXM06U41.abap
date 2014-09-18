*----------------------------------------------------------------------*
*   INCLUDE ZXM06F41                                                   *
*----------------------------------------------------------------------*
data: ls_ref_ekpo like line of gt_ref_ekpo_tab.

*
* store transaction type for later modification of screen attributes
*
gl_aktyp = i_aktyp.

gl_no_screen = i_no_screen.

*
*store current state of customer data in ekko_ci (structure for screen)
*
ekpo_ci  = i_ci_ekpo.

gl_ekpo = i_ekpo.

if not i_rekpo-ebeln is initial and
   not i_ekpo-ebelp is initial.

  clear ls_ref_ekpo.

  read table gt_ref_ekpo_tab into ls_ref_ekpo
      with key ebelp = i_ekpo-ebelp.

  if ls_ref_ekpo-ebeln ne i_rekpo-ebeln and
     ls_ref_ekpo-ebelp ne i_rekpo-ebelp.

    ls_ref_ekpo-ebelp = i_ekpo-ebelp.
    ls_ref_ekpo-ekpo = i_rekpo.

    if sy-subrc is initial.
      modify gt_ref_ekpo_tab from ls_ref_ekpo index sy-tabix.
    else.
      append ls_ref_ekpo to gt_ref_ekpo_tab.
    endif.

  endif.

endif.

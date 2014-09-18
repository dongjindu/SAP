*&---------------------------------------------------------------------*
*& Report  ZMDF_DICONF_CREATE                                          *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

report  zmdf_diconf_create.

selection-screen begin of block b1 with frame title text-sl1.
parameters:
  p_plnum  like ppc_comp_conf-plnum obligatory,
  p_repnt like ppc_comp_conf-stsor obligatory.
selection-screen end of block b1.
selection-screen begin of block b2 with frame title text-sl2.
parameters:
  p_erfmg  like ppc_comp_conf-erfmg obligatory,
  p_erfme  like ppc_comp_conf-erfme obligatory,
  p_budat  like ppc_comp_conf-budat obligatory default sy-datum,
  p_bldat  like ppc_comp_conf-bldat obligatory default sy-datum,
  p_gr_ind like ppc_comp_conf-gr_ind,
  p_gi_ind like ppc_comp_conf-gi_ind,
  p_alort  like ppc_comp_conf-alort.
selection-screen end of block b2.
selection-screen begin of block b3 with frame title text-sl3.
parameters:
  p_write type c radiobutton group conf,
  p_rev   type c radiobutton group conf.
selection-screen comment /40(30) comm1.
selection-screen end of block b3.

data: ls_ppc_comp_conf type zppc_comp_conf.
data: lt_mdpmx like mdpm occurs 0 with header line.
data: lt_act like ZPPC_ACT_CONF occurs 0 with header line.


start-of-selection.

  perform fill_data.

  if p_write = 'X'.
    call function 'ZPPC1TP_COMP_CONF_DATA_WRITE'
      exporting
        is_ppc_comp_conf = ls_ppc_comp_conf
      tables
        it_mdpmx         = lt_mdpmx
        IT_CONF_ACTS     = lt_ACT
      exceptions
        order_error      = 1
        line_error       = 2
        bapi_error       = 3
        others           = 4.
  endif.
  if p_rev = 'X'.
    call function 'ZPPC1TP_COMP_CONF_DATA_REVERSE'
      exporting
        is_ppc_comp_conf = ls_ppc_comp_conf
      tables
        it_mdpmx         = lt_mdpmx
        IT_CONF_ACTS     = lt_ACT
      exceptions
        order_error      = 1
        line_error       = 2
        bapi_error       = 3
        others           = 4.
  endif.

  if sy-subrc is initial.
    message s008(ppc1pr).
  else.
    message id sy-msgid type 'I' number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.


*&--------------------------------------------------------------------*
*&      Form  fill_data
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
form fill_data.

  ls_ppc_comp_conf-plnum  = p_plnum.
  ls_ppc_comp_conf-budat  = p_budat.
  ls_ppc_comp_conf-bldat  = p_bldat.
  ls_ppc_comp_conf-erfmg  = p_erfmg.
  ls_ppc_comp_conf-erfme  = p_erfme.
  ls_ppc_comp_conf-alort  = p_alort.
  ls_ppc_comp_conf-gr_ind = p_gr_ind.
  ls_ppc_comp_conf-gi_ind = p_gi_ind.
  ls_ppc_comp_conf-stsor  =  p_repnt.

endform.                    " fill_data

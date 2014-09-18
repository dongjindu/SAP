function zppc1tp_update_delete_plaf_res.
*"----------------------------------------------------------------------
*"*"Update function module:
*"
*"*"Local interface:
*"  TABLES
*"      I_PPC1TP_PLAF_UPD STRUCTURE  PPC1TP_PLAF OPTIONAL
*"      I_PPC1TP_PLAF_DEL STRUCTURE  PPC1TP_PLAF OPTIONAL
*"      I_PPC1TP_RESB_UPD STRUCTURE  PPC1TP_RESB OPTIONAL
*"      I_PPC1TP_RESB_DEL STRUCTURE  PPC1TP_RESB OPTIONAL
*"----------------------------------------------------------------------



  if not i_ppc1tp_plaf_del[] is initial.
    delete ppc1tp_plaf from table i_ppc1tp_plaf_del.
  endif.

  if not i_ppc1tp_plaf_upd[] is initial.
    update ppc1tp_plaf from table i_ppc1tp_plaf_upd.
  endif.


  if not i_ppc1tp_resb_del[] is initial.
    delete ppc1tp_resb from table i_ppc1tp_resb_del.
  endif.

  if not i_ppc1tp_resb_upd[] is initial.
    update ppc1tp_resb from table i_ppc1tp_resb_upd.
  endif.


endfunction.

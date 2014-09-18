*----------------------------------------------------------------------*
*   INCLUDE ZXFMYU11                                                   *
*----------------------------------------------------------------------*
*"  IMPORTING
*"     VALUE(I_ITEM) LIKE  FMIDATA STRUCTURE  FMIDATA
*"     VALUE(I_ITEM_NEW) TYPE  FMFI_ITEM
*"     REFERENCE(I_F_ACCIT) TYPE  ACCIT
*"  CHANGING
*"     VALUE(C_STATS) LIKE  FMIT-RSTATS

*SAPLFMFA
*C_STATS = 'X' : The document line is updated as statistical
*C_STATS = ' ' : The document line is updated as "real"

case i_f_accit-AWTYP.
  when 'AUAK'.   "order settlement (auc)
     c_stats = 'X'.

  when others.

endcase.

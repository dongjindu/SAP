*----------------------------------------------------------------------*
*   INCLUDE ZCO_ZXM06U52_1                                             *
*----------------------------------------------------------------------*
*Determine INFO (lowest price)
TABLES: ztfi_ctl, ztco_mail.
DATA: l_flg_low(1) TYPE c,
      l_categ(8)   type c.
CLEAR l_flg_low.

*Determine Newest Valid From?
SELECT SINGLE flag INTO l_flg_low FROM ztfi_ctl
  WHERE categ = 'ZXM06U99' AND flag  = 'X'.

*Send Notice
l_send_notice = 0.
concatenate sy-tcode(2) l_klvaf into l_categ.
REFRESH r_receiv.
r_receiv-option = 'EQ'.  r_receiv-sign = 'I'.
SELECT * FROM ztco_mail WHERE categ = 'PU' and flag = 'X'.
  r_receiv-low = ztco_mail-uname. APPEND r_receiv.
  l_send_notice = 1.
ENDSELECT.

*for Debugging------------------------
SELECT SINGLE * FROM ztfi_ctl
  WHERE categ = 'ZXM06U52' AND flag  = 'X'.
IF  sy-subrc = 0 AND ztfi_ctl-zuonr = i_bqpim-matnr.
  BREAK-POINT.
ENDIF.
*for Debugging------------------------

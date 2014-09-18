*&---------------------------------------------------------------------*
*&  Include           ZXM08U20
*&---------------------------------------------------------------------*
DATA lv_rbkpv     TYPE mrm_rbkpv.
DATA lt_selwenr   LIKE ek08erswe OCCURS 0 WITH HEADER LINE.
DATA lt_settle_dc LIKE ek08ersdc OCCURS 0 WITH HEADER LINE.

DATA lv_xblnr     TYPE xblnr.

lv_rbkpv = i_rbkpv.
lt_selwenr[]   = t_selwenr[].
lt_settle_dc[] = t_settle_dc[].

READ TABLE lt_selwenr INDEX 1.

SELECT SINGLE xblnr
  INTO lv_xblnr
  FROM mkpf
 WHERE mblnr = lt_selwenr-lfbnr.

MOVE-CORRESPONDING i_rbkpv TO e_rbkpv_ers_change.
e_rbkpv_ers_change-xblnr = lv_xblnr.
e_change = 'X'.

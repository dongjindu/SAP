*----------------------------------------------------------------------*
*   INCLUDE ZXCSAU02                                                   *
*----------------------------------------------------------------------*

  zeitm = userdata-eitm.
  zstgb = userdata-stgb.
  zsuff = userdata-suff.
  zsequ = userdata-sequ.
  zupgn = userdata-upgn.
  zinfo = userdata-zinfo.

*  IF userdata-eitm EQ 'K'.
*    MOVE: 'X' TO itemdata-sanfe,
*          'X' TO itemdata-sanka.
*  ELSE.
*    MOVE: space TO itemdata-sanfe,
*          space TO itemdata-sanka.
*  ENDIF.

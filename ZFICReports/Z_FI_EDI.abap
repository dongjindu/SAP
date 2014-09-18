REPORT Z_FIED01 .

tables reguh.

parameters: p_LAUFD like reguh-laufd,
            p_LAUFI like reguh-LAUFI,
            p_xvorl like reguh-xvorl,
*           p_bukrs like reguh-zbukr,
*           p_lifnr like reguh-lifnr,
            p_edi   like reguh-EDIAV default 'X',
            p_order like reguh-EDIBN.

parameters: p_run as checkbox.


SELECT        * FROM  REGUH
       WHERE  LAUFD  = p_laufd
       AND    LAUFI  = p_laufi
       AND    XVORL  = p_xvorl.
*      AND    ZBUKR  = p_bukrs
*      AND    LIFNR  = p_lifnr.
*      AND    VBLNR  = ___.


  if p_run = 'X'.
    reguh-EDIAV = p_edi.
    reguh-EDIBN = p_order.

    update reguh.
    write:/ REGUH-VBLNR,  'Updated...'.
  else.
    write:/ REGUH-VBLNR,  '...', reguh-EDIAV.
  endif.

ENDSELECT.

*&---------------------------------------------------------------------*
*&  Include           ZXMBCU10
*&---------------------------------------------------------------------*

If i_MKPF-XBLNR is not initial and I_MSEG-LIFNR eq 'AG7N'.
    select single LICHN into E_CHARG from LIPS
                    where vbeln eq i_MKPF-XBLNR.
endif.

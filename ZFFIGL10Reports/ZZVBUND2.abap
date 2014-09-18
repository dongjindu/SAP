report zzvbund1.
tables: bseg, bkpf, t003, kna1, lfa1, bsis, bsas, bsik, bsak, bsid,bsad.
tables: vf_debi, vf_kred.


*TYPES: BKPF_TYPE LIKE BKPF,
*       BSEG_TYPE LIKE BSEG,
*       BSIS_TYPE LIKE BSIS,
*       BSAS_TYPE LIKE BSAS,
*       BSIK_TYPE LIKE BSIK,
*       BSAK_TYPE LIKE BSAK,
*       BSID_TYPE LIKE BSID,
*       BSAD_TYPE LIKE BSAD.
*
*DATA: XBKPF TYPE BKPF_TYPE OCCURS 0 WITH HEADER LINE,
*      EBKPF TYPE BKPF_TYPE OCCURS 0 WITH HEADER LINE,
*      XBSEG TYPE BSEG_TYPE OCCURS 0 WITH HEADER LINE,
*      YBSEG TYPE BSEG_TYPE OCCURS 0 WITH HEADER LINE,
*      XBSIS TYPE BSIS_TYPE OCCURS 0 WITH HEADER LINE,
*      XBSAS TYPE BSAS_TYPE OCCURS 0 WITH HEADER LINE,
*      XBSIK TYPE BSIK_TYPE OCCURS 0 WITH HEADER LINE,
*      XBSAK TYPE BSAK_TYPE OCCURS 0 WITH HEADER LINE,
*      XBSID TYPE BSID_TYPE OCCURS 0 WITH HEADER LINE,
*      XBSAD TYPE BSAD_TYPE OCCURS 0 WITH HEADER LINE,
*      XVBUND LIKE LFA1-VBUND,
*      V_ERROR TYPE C,
*      V_FLAG  TYPE C.

*DATA: IT_BSIK LIKE BSIK OCCURS 0.
data: begin of it_bsik occurs 0,
        lifnr like bseg-lifnr,
        bukrs like bseg-bukrs,
        gjahr like bsik-gjahr,
        belnr like bseg-belnr,
        buzei like bseg-buzei,
        vbund like bsik-vbund,
        hkont like bseg-hkont,
      end of it_bsik.
data: w_bsik like it_bsik.
data: it_bsid like it_bsik occurs 0.

data: begin of it_bseg occurs 0,
        bukrs like bseg-bukrs,
        belnr like bseg-belnr,
        buzei like bseg-buzei,
        hkont like bseg-hkont,
        koart like bseg-koart,
        augdt like bseg-augdt,
        augbl like bseg-augbl,
      end of it_bseg.
*----------------------------------------------------------------
select-options:
          s_lifnr     for  bsik-lifnr.

select-options:
          s_bukrs     for  bkpf-bukrs memory id buk,
          s_gjahr     for  bkpf-gjahr,
          s_belnr     for  bkpf-belnr,
          s_cpudt     for  bkpf-cpudt.

parameters: update default space as checkbox,
            awtyp like ttyp-awtyp. " DEFAULT 'MKPF' .
parameters: p_vend as checkbox,
            p_cust as checkbox.

*----------------------------------------------------------------
*
* COep, coej,
* cooi
* bsad, bsak, bsid, bsik
* glfunct
* glt1
* anla, anek
* regup
*

*TABLES: BSIK, LFA1, BSEG.

start-of-selection.

  if p_vend = 'X'.
* vendor
    select lifnr bukrs gjahr belnr buzei vbund hkont
      into table it_bsik
      from bsik
      where bukrs in s_bukrs
        and lifnr in s_lifnr
        and gjahr in s_gjahr
        and belnr in s_belnr.
    loop at it_bsik into w_bsik.
      select single * from lfa1
** Changed by Furong on 02/18/10
*        where lifnr = w_bsik-lifnr.
        where kunnr = w_bsik-lifnr.
** End of change on 02/18/10
      if lfa1-vbund = w_bsik-vbund.
        continue.
      endif.
      perform update_vbund using 'K' w_bsik lfa1-vbund.
    endloop.
  endif.

  if p_cust = 'X'.
*customer
    select kunnr bukrs gjahr belnr buzei vbund hkont
      into table it_bsid
      from bsid
      where bukrs in s_bukrs
        and kunnr in s_lifnr
        and gjahr in s_gjahr
        and belnr in s_belnr.
    loop at it_bsid into w_bsik.
      select single * from kna1
** Changed by Furong on 02/18/10
*        where lifnr = w_bsik-lifnr.
        where kunnr = w_bsik-lifnr.
** End of change on 02/18/10
      if kna1-vbund = w_bsik-vbund.
        continue.
      endif.
      perform update_vbund using 'D' w_bsik kna1-vbund.
    endloop.
  endif.
*&---------------------------------------------------------------------*
*&      Form  update_vbund
*&---------------------------------------------------------------------*
form update_vbund using    p_koart
                           p_w_bsik structure it_bsik
                           p_vbund.


  if update = 'X'.
    update bseg set vbund = p_vbund
        where bukrs = p_w_bsik-bukrs
          and gjahr = p_w_bsik-gjahr
          and belnr = p_w_bsik-belnr.

    case p_koart.
      when 'K'.
        update bsik set vbund = p_vbund
            where bukrs = p_w_bsik-bukrs
              and lifnr = p_w_bsik-lifnr
              and gjahr = p_w_bsik-gjahr
              and belnr = p_w_bsik-belnr.
      when 'D'.
        update bsid set vbund = p_vbund
            where bukrs = p_w_bsik-bukrs
              and kunnr = p_w_bsik-lifnr
              and gjahr = p_w_bsik-gjahr
              and belnr = p_w_bsik-belnr.
    endcase.

*-- BSAS,BSIS
    refresh it_bseg.
    select bukrs belnr buzei hkont koart augdt augbl

       into table it_bseg
       from bseg
        where bukrs = p_w_bsik-bukrs
          and gjahr = p_w_bsik-gjahr
          and belnr = p_w_bsik-belnr.

    loop at it_bseg where koart = 'S'.

      update bsas set vbund = p_vbund
         where bukrs = p_w_bsik-bukrs
           and hkont = it_bseg-hkont
           and augdt eq it_bseg-augdt
           and augbl eq  it_bseg-augbl
           and gjahr = p_w_bsik-gjahr
           and belnr = p_w_bsik-belnr.

      update bsis set vbund = p_vbund
         where bukrs = p_w_bsik-bukrs
           and hkont = it_bseg-hkont
           and gjahr = p_w_bsik-gjahr
           and belnr = p_w_bsik-belnr.
    endloop.
    write:/ 'UPDATED', p_w_bsik-belnr.
  endif.

endform.                    " update_vbund

FUNCTION z_fbm_abxc23dt.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_ABXC23DT STRUCTURE  ZTBM_ABXC23DT
*"----------------------------------------------------------------------

  DATA: it_abxc23dt TYPE ztbm_abxc23dt OCCURS 0 WITH HEADER LINE.
  DATA : w_int TYPE i,
         w_dsn(90).
  DATA : BEGIN OF it_c23 OCCURS 0,
         record(134),
         END OF it_c23.
  DATA : d_date LIKE sy-datum.
  CLEAR : d_date.

  d_date = sy-datum - 1.

  REFRESH it_abxc23dt.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_abxc23dt
     FROM ztbm_abxc23dt
      WHERE zsdat EQ d_date.

  DELETE ztbm_abxc23dt FROM TABLE it_abxc23dt.
  REFRESH it_abxc23dt.
  GET TIME.
  LOOP AT t_abxc23dt.
    MOVE-CORRESPONDING t_abxc23dt TO it_abxc23dt.
    MOVE : sy-uname TO it_abxc23dt-zuser,
           sy-datum TO it_abxc23dt-zsdat,
           sy-uzeit TO it_abxc23dt-zstim.
    APPEND it_abxc23dt.
  ENDLOOP.

  MODIFY  ztbm_abxc23dt FROM TABLE it_abxc23dt.
  IF sy-subrc EQ 0.
    t_abxc23dt-zresult = 'S'.
    MODIFY t_abxc23dt TRANSPORTING  zresult
      WHERE zc23_key1_c0000 <> space.
**file download
*    LOOP AT  it_abxc23dt.
*      it_c23-record+0(4)    = it_abxc23dt-zc23_key1_c0000.
*      it_c23-record+4(8)    = it_abxc23dt-zc23_key2_c0000.
*      it_c23-record+12(20)  = it_abxc23dt-zc23_optn_c0000.
*      it_c23-record+32(3)   = it_abxc23dt-zc23_natn_c0000.
*      it_c23-record+35(3)   = it_abxc23dt-zc23_ccno_n0000.
*      it_c23-record+38(4)   = it_abxc23dt-zc23_keyo_ccolr.
*      it_c23-record+41(5)   = it_abxc23dt-zc23_clsf_nsequ.
*      it_c23-record+44(6)   = it_abxc23dt-zc23_part_ccolr.
*      it_c23-record+47(9)   = it_abxc23dt-zc23_e000_nstrt.
*      it_c23-record+56(9)   = it_abxc23dt-zc23_e000_nfini.
*      it_c23-record+65(8)   = it_abxc23dt-zc23_dsgn_dstrt.
*      it_c23-record+73(8)   = it_abxc23dt-zc23_dsgn_dfini.
*      it_c23-record+81(8)   = it_abxc23dt-zc23_prod_dstrt.
*      it_c23-record+89(8)   = it_abxc23dt-zc23_prod_dfini.
*      it_c23-record+97(1)   = it_abxc23dt-zc23_prod_cdsgn.
*      it_c23-record+98(2)   = it_abxc23dt-zc23_plnt_cstrt.
*      it_c23-record+100(3)  = it_abxc23dt-zc23_epin_cstrt.
*      it_c23-record+103(2)  = it_abxc23dt-zc23_plnt_cfini.
*      it_c23-record+105(3)  = it_abxc23dt-zc23_epin_cfini.
*      it_c23-record+108(3)  = it_abxc23dt-zc23_prev_cpart.
*      it_c23-record+111(8)  = it_abxc23dt-zc23_rgst_d0000.
*      it_c23-record+119(8)  = it_abxc23dt-zc23_updt_d0000.
*      it_c23-record+127(7)  = it_abxc23dt-zc23_auth_n0000.
*      APPEND it_c23.
*    ENDLOOP.
*    CLEAR w_int.
*    DESCRIBE TABLE it_c23 LINES w_int.
*    IF w_int <> 0.
*      DATA : d_datum LIKE sy-datum.
*      d_datum = sy-datum - 1.
*      CONCATENATE  p_mobis_imt
*                     d_datum '_ccc.txt'
*                 INTO w_dsn.
*      DELETE DATASET w_dsn.
*
*      CONCATENATE  p_mobis_imt
*                   sy-datum '_ccc.txt'
*               INTO w_dsn.
*
*      DELETE DATASET w_dsn.
*
*      OPEN DATASET w_dsn IN TEXT MODE FOR APPENDING.
*
*      LOOP AT it_c23.
*        TRANSFER it_c23 TO w_dsn.
*      ENDLOOP.
*
*      CLOSE DATASET w_dsn.
*      IF sy-subrc = 0.
*        MESSAGE i000 WITH text-001 '(C23)'.
*      ELSE.
*        MESSAGE i000 WITH text-002 '(C23)'.
*      ENDIF.
*
*    ENDIF.
  ELSE.
    t_abxc23dt-zresult = 'E'.
    MODIFY t_abxc23dt TRANSPORTING zresult
       WHERE zc23_key1_c0000 <> space.
  ENDIF.
ENDFUNCTION.

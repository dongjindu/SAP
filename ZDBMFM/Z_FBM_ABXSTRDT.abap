FUNCTION z_fbm_abxstrdt.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_ABXSTRDT STRUCTURE  ZTBM_ABXSTRDT
*"----------------------------------------------------------------------
  DATA: it_abxstrdt TYPE ztbm_abxstrdt OCCURS 0 WITH HEADER LINE.
  DATA : w_int TYPE i,
         w_dsn(90).
  DATA : BEGIN OF it_str OCCURS 0,
         record(194),
         END OF it_str.
  DATA : d_date LIKE sy-datum.
  CLEAR : d_date.

  d_date = sy-datum - 1.

  REFRESH it_abxstrdt.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_abxstrdt
     FROM ztbm_abxstrdt
      WHERE zsdat EQ d_date.

  DELETE ztbm_abxstrdt FROM TABLE it_abxstrdt.
  REFRESH it_abxstrdt.
  GET TIME.
  LOOP AT t_abxstrdt.
    MOVE-CORRESPONDING t_abxstrdt TO it_abxstrdt.
    MOVE : sy-uname TO it_abxstrdt-zuser,
           sy-datum TO it_abxstrdt-zsdat,
           sy-uzeit TO it_abxstrdt-zstim.
    APPEND it_abxstrdt.
  ENDLOOP.

  MODIFY  ztbm_abxstrdt FROM TABLE it_abxstrdt.
  IF sy-subrc EQ 0.
    t_abxstrdt-zresult = 'S'.
    MODIFY t_abxstrdt TRANSPORTING  zresult
      WHERE zstr_vend_c <> space.
**file download
*    LOOP AT  it_abxstrdt.
*      it_str-record+0(4)    = it_abxstrdt-zstr_vend_c.
*      it_str-record+4(3)    = it_abxstrdt-zstr_carx_c.
*      it_str-record+7(1)    = it_abxstrdt-zstr_modu_g.
*      it_str-record+8(23)   = it_abxstrdt-zstr_assy_part.
*      it_str-record+31(4)   = it_abxstrdt-zstr_comp_pref.
*      it_str-record+35(15)  = it_abxstrdt-zstr_comp_part.
*      it_str-record+50(4)   = it_abxstrdt-zstr_comp_suff.
*      it_str-record+54(10)  = it_abxstrdt-zstr_qtyo_n.
*      it_str-record+64(1)   = it_abxstrdt-zstr_eitm_engn.
*      it_str-record+65(1)   = it_abxstrdt-zstr_eitm_prot.
*      it_str-record+66(1)   = it_abxstrdt-zstr_eitm_ewon.
*      it_str-record+67(1)   = it_abxstrdt-zstr_eitm_ewgt.
*      it_str-record+68(1)   = it_abxstrdt-zstr_eitm_matl.
*      it_str-record+69(1)   = it_abxstrdt-zstr_eitm_wons.
*      it_str-record+70(1)   = it_abxstrdt-zstr_eitm_prod.
*      it_str-record+71(1)   = it_abxstrdt-zstr_eitm_pacs.
*      it_str-record+72(1)   = it_abxstrdt-zstr_gong_no1.
*      it_str-record+73(9)   = it_abxstrdt-zstr_gong_no2.
*      it_str-record+82(9)   = it_abxstrdt-zstr_addo_eono.
*      it_str-record+91(9)   = it_abxstrdt-zstr_delo_eono.
*      it_str-record+100(8)  = it_abxstrdt-zstr_addo_dengn.
*      it_str-record+108(8)  = it_abxstrdt-zstr_delo_dengn.
*      it_str-record+116(3)  = it_abxstrdt-zstr_addo_suff.
*      it_str-record+119(3)  = it_abxstrdt-zstr_delo_suff.
*      it_str-record+122(8)  = it_abxstrdt-zstr_addo_dprod.
*      it_str-record+130(8)  = it_abxstrdt-zstr_delo_dprod.
*      it_str-record+138(8)  = it_abxstrdt-zstr_init_date.
*      it_str-record+146(30) = it_abxstrdt-zstr_remk_c.
*      it_str-record+176(3)  = it_abxstrdt-zstr_ccno_c.
*      it_str-record+179(3)  = it_abxstrdt-zstr_rate_n.
*      it_str-record+182(3)  = it_abxstrdt-zstr_crap_n.
*      it_str-record+185(8)  = it_abxstrdt-zstr_last_date.
*      it_str-record+193(1)  = it_abxstrdt-zstr_gubn_g.
*      APPEND it_str.
*    ENDLOOP.
*    CLEAR w_int.
*    DESCRIBE TABLE it_str LINES w_int.
*    IF w_int <> 0.
*      DATA : d_datum LIKE sy-datum.
*      d_datum = sy-datum - 1.
*      CONCATENATE  p_mobis_imt
*                    d_datum '_prdstr.txt'
*                 INTO w_dsn.
*      DELETE DATASET w_dsn.
*      CONCATENATE  p_mobis_imt
*                   sy-datum '_prdstr.txt'
*               INTO w_dsn.
*
*      DELETE DATASET w_dsn.
*
*      OPEN DATASET w_dsn IN TEXT MODE FOR APPENDING.
*
*      LOOP AT it_str.
*        TRANSFER it_str TO w_dsn.
*      ENDLOOP.
*
*      CLOSE DATASET w_dsn.
*      IF sy-subrc = 0.
*        MESSAGE i000 WITH text-001 '(STR)'.
*      ELSE.
*        MESSAGE i000 WITH text-002 '(STR)'.
*      ENDIF.
*
*    ENDIF.
  ELSE.
    t_abxstrdt-zresult = 'E'.
    MODIFY t_abxstrdt TRANSPORTING zresult
       WHERE zstr_vend_c <> space.
  ENDIF.

ENDFUNCTION.

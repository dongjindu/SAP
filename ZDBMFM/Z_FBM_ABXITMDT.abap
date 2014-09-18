FUNCTION z_fbm_abxitmdt.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_ABXITMDT STRUCTURE  ZTBM_ABXITMDT
*"----------------------------------------------------------------------
  DATA: it_abxitmdt TYPE ztbm_abxitmdt OCCURS 0 WITH HEADER LINE.
  DATA : w_int TYPE i,
         w_dsn(90).
  DATA : BEGIN OF it_imt OCCURS 0,
         record(150),
         END OF it_imt.
  DATA : d_date LIKE sy-datum.
  CLEAR: d_date.

  d_date = sy-datum - 1.


  REFRESH it_abxitmdt.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_abxitmdt
     FROM ztbm_abxitmdt
      WHERE ZSDAT EQ D_DATE.

  DELETE ztbm_abxitmdt FROM TABLE it_abxitmdt.
  REFRESH it_abxitmdt.
  GET TIME.
  LOOP AT t_abxitmdt.
    MOVE-CORRESPONDING t_abxitmdt TO it_abxitmdt.
    MOVE : sy-uname TO it_abxitmdt-zuser,
           sy-datum TO it_abxitmdt-zsdat,
           sy-uzeit TO it_abxitmdt-zstim.
    APPEND it_abxitmdt.
  ENDLOOP.

  MODIFY  ztbm_abxitmdt FROM TABLE it_abxitmdt.
  IF sy-subrc EQ 0.
    t_abxitmdt-zresult = 'S'.
    MODIFY t_abxitmdt TRANSPORTING  zresult
      WHERE zitm_vend_c <> space.
**file download
*    LOOP AT  it_abxitmdt.
*      it_imt-record+0(4)   = it_abxitmdt-zitm_vend_c.
*      it_imt-record+4(3)   = it_abxitmdt-zitm_carx_c.
*      it_imt-record+7(1)   = it_abxitmdt-zitm_modu_g.
*      it_imt-record+8(15)  = it_abxitmdt-zitm_part_no.
*      it_imt-record+23(30) = it_abxitmdt-zitm_part_desc.
*      it_imt-record+53(3)  = it_abxitmdt-zitm_llco_d.
*      it_imt-record+56(10) = it_abxitmdt-zitm_pnc_c.
*      it_imt-record+66(3)  = it_abxitmdt-zitm_ccno_no.
*      it_imt-record+69(1)  = it_abxitmdt-zitm_sour_engn.
*      it_imt-record+70(1)  = it_abxitmdt-zitm_sour_deve.
*      it_imt-record+71(1)  = it_abxitmdt-zitm_sour_prod.
*      it_imt-record+72(2)  = it_abxitmdt-zitm_grup_c.
*      it_imt-record+74(2)  = it_abxitmdt-zitm_unit_c.
*      it_imt-record+76(2)  = it_abxitmdt-zitm_valo_c.
*      it_imt-record+78(1)  = it_abxitmdt-zitm_colr_id.
*      it_imt-record+79(1)  = it_abxitmdt-zitm_gubn_c.
*      it_imt-record+80(9)  = it_abxitmdt-zitm_last_eono.
*      it_imt-record+89(2)  = it_abxitmdt-zitm_last_rev.
*      it_imt-record+91(8)  = it_abxitmdt-zitm_last_date.
*      it_imt-record+99(8)  = it_abxitmdt-zitm_init_date.
*      it_imt-record+107(7) = it_abxitmdt-zitm_wono_n.
*      it_imt-record+114(8) = it_abxitmdt-zitm_wono_date.
*      it_imt-record+122(1) = it_abxitmdt-zitm_type_c.
*      it_imt-record+123(1) = it_abxitmdt-zitm_emis_id.
*      it_imt-record+124(1) = it_abxitmdt-zitm_prot_c.
*      it_imt-record+125(1) = it_abxitmdt-zitm_init_c.
*      it_imt-record+126(4) = it_abxitmdt-zitm_rlno_c.
*      it_imt-record+130(1) = it_abxitmdt-zitm_dimo_c.
*      it_imt-record+131(1) = it_abxitmdt-zitm_alto_c.
*      it_imt-record+132(1) = it_abxitmdt-zitm_subs_mark.
*      it_imt-record+133(1) = it_abxitmdt-zitm_sour_main.
*      it_imt-record+134(1) = it_abxitmdt-zitm_stat_c.
*      it_imt-record+135(9) = it_abxitmdt-zitm_cbmo_n.
*      it_imt-record+144(1) = it_abxitmdt-zitm_didi_c.
*      it_imt-record+145(1) = it_abxitmdt-zitm_asas_c.
*      it_imt-record+146(3) = it_abxitmdt-zitm_ccne_no.
*      it_imt-record+149(1) = it_abxitmdt-zitm_pass_c.
*      APPEND it_imt.
*    ENDLOOP.
*    CLEAR w_int.
*    DESCRIBE TABLE it_imt LINES w_int.
*    IF w_int <> 0.
*      DATA : d_datum LIKE sy-datum.
*      d_datum = sy-datum - 1.
*      CONCATENATE  p_mobis_imt
*                     d_datum '_item.txt'
*                 INTO w_dsn.
*      DELETE DATASET w_dsn.
*      CONCATENATE  p_mobis_imt
*                   sy-datum '_item.txt'
*               INTO w_dsn.
*
*      DELETE DATASET w_dsn.
*
*      OPEN DATASET w_dsn IN TEXT MODE FOR APPENDING.
*
*      LOOP AT it_imt.
*        TRANSFER it_imt TO w_dsn.
*      ENDLOOP.
*
*      CLOSE DATASET w_dsn.
*      IF sy-subrc = 0.
*        MESSAGE i000 WITH text-001 '(ITM)'.
*      ELSE.
*        MESSAGE i000 WITH text-002 '(ITM)'.
*      ENDIF.
*
*    ENDIF.
  ELSE.
    t_abxitmdt-zresult = 'E'.
    MODIFY t_abxitmdt TRANSPORTING zresult
       WHERE zitm_vend_c <> space.
  ENDIF.

ENDFUNCTION.

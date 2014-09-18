*&---------------------------------------------------------------------*
*& Report  ZAPP724U_DOWNLAOD_M
*
*& Author                 : WSKIM
*& Creation Date          : 11/17/2004
*& Specification By       :
*& Pattern                : Report 1-1
*& Development Request No :
*& Addl documentation     :
*& Description  : AR  Upload
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*&--------------------------------------------------------------------
REPORT  zapp724u_downlaod_m   MESSAGE-ID zmbm.

TABLES : ztbm_abxstrdt,ztbm_abxitmdt,ztbm_abxocndt,ztbm_abxc23dt,
         ztbm_mobis_log.

DATA: it_abxstrdt TYPE ztbm_abxstrdt OCCURS 0 WITH HEADER LINE,
      it_abxitmdt TYPE ztbm_abxitmdt OCCURS 0 WITH HEADER LINE,
      it_abxocndt LIKE ztbm_abxocndt OCCURS 0 WITH HEADER LINE,
      it_abxc23dt TYPE ztbm_abxc23dt OCCURS 0 WITH HEADER LINE.

DATA : p_mobis_imt LIKE rlgrap-filename VALUE '/usr/sap/EDI_SAP/BOM/',
       w_int TYPE i,
       w_dsn(90).

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.

PARAMETERS: p_run TYPE c  DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK b1.


INITIALIZATION.
  PERFORM itialization.

START-OF-SELECTION.

  CHECK p_run EQ 'X'.
  PERFORM download_text_str.
  PERFORM download_text_itm.
  PERFORM download_text_ocn.
  PERFORM download_text_c23.

*&---------------------------------------------------------------------*
*&      Form  itialization
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM itialization.
  REFRESH : it_abxstrdt,it_abxitmdt,it_abxocndt,it_abxc23dt.
ENDFORM.                    " itialization
*&---------------------------------------------------------------------*
*&      Form  download_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_text_str.
  DATA : BEGIN OF it_str OCCURS 0,
         record(194),
         END OF it_str.
  CLEAR : w_int,w_dsn,it_str[].
*1st : ZTBM_ABXSTRDT
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_abxstrdt
    FROM ztbm_abxstrdt
     WHERE zsdat EQ sy-datum.

*file download
  LOOP AT  it_abxstrdt.
    it_str-record+0(4)    = it_abxstrdt-zstr_vend_c.
    it_str-record+4(3)    = it_abxstrdt-zstr_carx_c.
    it_str-record+7(1)    = it_abxstrdt-zstr_modu_g.
    it_str-record+8(23)   = it_abxstrdt-zstr_assy_part.
    it_str-record+31(4)   = it_abxstrdt-zstr_comp_pref.
    it_str-record+35(15)  = it_abxstrdt-zstr_comp_part.
    it_str-record+50(4)   = it_abxstrdt-zstr_comp_suff.
    it_str-record+54(10)  = it_abxstrdt-zstr_qtyo_n.
    it_str-record+64(1)   = it_abxstrdt-zstr_eitm_engn.
    it_str-record+65(1)   = it_abxstrdt-zstr_eitm_prot.
    it_str-record+66(1)   = it_abxstrdt-zstr_eitm_ewon.
    it_str-record+67(1)   = it_abxstrdt-zstr_eitm_ewgt.
    it_str-record+68(1)   = it_abxstrdt-zstr_eitm_matl.
    it_str-record+69(1)   = it_abxstrdt-zstr_eitm_wons.
    it_str-record+70(1)   = it_abxstrdt-zstr_eitm_prod.
    it_str-record+71(1)   = it_abxstrdt-zstr_eitm_pacs.
    it_str-record+72(1)   = it_abxstrdt-zstr_gong_no1.
    it_str-record+73(9)   = it_abxstrdt-zstr_gong_no2.
    it_str-record+82(9)   = it_abxstrdt-zstr_addo_eono.
    it_str-record+91(9)   = it_abxstrdt-zstr_delo_eono.
    it_str-record+100(8)  = it_abxstrdt-zstr_addo_dengn.
    it_str-record+108(8)  = it_abxstrdt-zstr_delo_dengn.
    it_str-record+116(3)  = it_abxstrdt-zstr_addo_suff.
    it_str-record+119(3)  = it_abxstrdt-zstr_delo_suff.
    it_str-record+122(8)  = it_abxstrdt-zstr_addo_dprod.
    it_str-record+130(8)  = it_abxstrdt-zstr_delo_dprod.
    it_str-record+138(8)  = it_abxstrdt-zstr_init_date.
    it_str-record+146(30) = it_abxstrdt-zstr_remk_c.
    it_str-record+176(3)  = it_abxstrdt-zstr_ccno_c.
    it_str-record+179(3)  = it_abxstrdt-zstr_rate_n.
    it_str-record+182(3)  = it_abxstrdt-zstr_crap_n.
    it_str-record+185(8)  = it_abxstrdt-zstr_last_date.
    it_str-record+193(1)  = it_abxstrdt-zstr_gubn_g.
    APPEND it_str.
  ENDLOOP.
  CLEAR w_int.
  DESCRIBE TABLE it_str LINES w_int.
  IF w_int <> 0.
    DATA : d_datum LIKE sy-datum.

    d_datum = sy-datum - 1.
    CONCATENATE  p_mobis_imt
                  d_datum '_prdstr.txt'
               INTO w_dsn.
    DELETE DATASET w_dsn.


    CONCATENATE  p_mobis_imt
                 sy-datum '_prdstr.txt'
             INTO w_dsn.

    DELETE DATASET w_dsn.

    OPEN DATASET w_dsn IN TEXT MODE FOR APPENDING.

    LOOP AT it_str.
      TRANSFER it_str TO w_dsn.
    ENDLOOP.

    CLOSE DATASET w_dsn.
    IF sy-subrc = 0.
      MESSAGE i000 WITH text-001 '(STR)'.
      CLEAR ztbm_mobis_log.
      MOVE : 'ZTBM_ABXSTRDT' TO ztbm_mobis_log-ztable,
              'H'            TO ztbm_mobis_log-gubun,
              sy-datum       TO ztbm_mobis_log-infdate,
              w_int          TO ztbm_mobis_log-zcount.
      MODIFY ztbm_mobis_log.
    ELSE.
      MESSAGE i000 WITH text-002 '(STR)'.
    ENDIF.

  ENDIF.

ENDFORM.                    " download_text
*&---------------------------------------------------------------------*
*&      Form  download_text_itm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_text_itm.
  DATA : BEGIN OF it_imt OCCURS 0,
         record(150),
         END OF it_imt.

  CLEAR : w_int,w_dsn,it_imt[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_abxitmdt
   FROM ztbm_abxitmdt
    WHERE zsdat EQ sy-datum.

*file download
  LOOP AT  it_abxitmdt.
    it_imt-record+0(4)   = it_abxitmdt-zitm_vend_c.
    it_imt-record+4(3)   = it_abxitmdt-zitm_carx_c.
    it_imt-record+7(1)   = it_abxitmdt-zitm_modu_g.
    it_imt-record+8(15)  = it_abxitmdt-zitm_part_no.
    it_imt-record+23(30) = it_abxitmdt-zitm_part_desc.
    it_imt-record+53(3)  = it_abxitmdt-zitm_llco_d.
    it_imt-record+56(10) = it_abxitmdt-zitm_pnc_c.
    it_imt-record+66(3)  = it_abxitmdt-zitm_ccno_no.
    it_imt-record+69(1)  = it_abxitmdt-zitm_sour_engn.
    it_imt-record+70(1)  = it_abxitmdt-zitm_sour_deve.
    it_imt-record+71(1)  = it_abxitmdt-zitm_sour_prod.
    it_imt-record+72(2)  = it_abxitmdt-zitm_grup_c.
    it_imt-record+74(2)  = it_abxitmdt-zitm_unit_c.
    it_imt-record+76(2)  = it_abxitmdt-zitm_valo_c.
    it_imt-record+78(1)  = it_abxitmdt-zitm_colr_id.
    it_imt-record+79(1)  = it_abxitmdt-zitm_gubn_c.
    it_imt-record+80(9)  = it_abxitmdt-zitm_last_eono.
    it_imt-record+89(2)  = it_abxitmdt-zitm_last_rev.
    it_imt-record+91(8)  = it_abxitmdt-zitm_last_date.
    it_imt-record+99(8)  = it_abxitmdt-zitm_init_date.
    it_imt-record+107(7) = it_abxitmdt-zitm_wono_n.
    it_imt-record+114(8) = it_abxitmdt-zitm_wono_date.
    it_imt-record+122(1) = it_abxitmdt-zitm_type_c.
    it_imt-record+123(1) = it_abxitmdt-zitm_emis_id.
    it_imt-record+124(1) = it_abxitmdt-zitm_prot_c.
    it_imt-record+125(1) = it_abxitmdt-zitm_init_c.
    it_imt-record+126(4) = it_abxitmdt-zitm_rlno_c.
    it_imt-record+130(1) = it_abxitmdt-zitm_dimo_c.
    it_imt-record+131(1) = it_abxitmdt-zitm_alto_c.
    it_imt-record+132(1) = it_abxitmdt-zitm_subs_mark.
    it_imt-record+133(1) = it_abxitmdt-zitm_sour_main.
    it_imt-record+134(1) = it_abxitmdt-zitm_stat_c.
    it_imt-record+135(9) = it_abxitmdt-zitm_cbmo_n.
    it_imt-record+144(1) = it_abxitmdt-zitm_didi_c.
    it_imt-record+145(1) = it_abxitmdt-zitm_asas_c.
    it_imt-record+146(3) = it_abxitmdt-zitm_ccne_no.
    it_imt-record+149(1) = it_abxitmdt-zitm_pass_c.
    APPEND it_imt.
  ENDLOOP.
  CLEAR w_int.
  DESCRIBE TABLE it_imt LINES w_int.
  IF w_int <> 0.
    DATA : d_datum LIKE sy-datum.
    d_datum = sy-datum - 1.
    CONCATENATE  p_mobis_imt
                   d_datum '_item.txt'
               INTO w_dsn.
    DELETE DATASET w_dsn.
    CONCATENATE  p_mobis_imt
                 sy-datum '_item.txt'
             INTO w_dsn.

    DELETE DATASET w_dsn.

    OPEN DATASET w_dsn IN TEXT MODE FOR APPENDING.

    LOOP AT it_imt.
      TRANSFER it_imt TO w_dsn.
    ENDLOOP.

    CLOSE DATASET w_dsn.
    IF sy-subrc = 0.
      MESSAGE i000 WITH text-001 '(ITM)'.
      CLEAR ztbm_mobis_log.
      MOVE : 'ZTBM_ABXITMDT' TO ztbm_mobis_log-ztable,
              'H'            TO ztbm_mobis_log-gubun,
              sy-datum       TO ztbm_mobis_log-infdate,
              w_int          TO ztbm_mobis_log-zcount.
      MODIFY ztbm_mobis_log.

    ELSE.
      MESSAGE i000 WITH text-002 '(ITM)'.
    ENDIF.
  ENDIF.

ENDFORM.                    " download_text_itm
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_TEXT_OCN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_text_ocn.
  DATA : BEGIN OF it_ocn OCCURS 0,
      record(310),
      END OF it_ocn.
  DATA : f_num(3) TYPE n,
         s_num(2) TYPE n,
         field(16).
  CLEAR: f_num,s_num,it_ocn.
  FIELD-SYMBOLS <fs> TYPE ANY.
  REFRESH it_abxocndt.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_abxocndt
        FROM ztbm_abxocndt.

  LOOP AT  it_abxocndt.
    it_ocn-record+0(1)    = it_abxocndt-zyear.
    it_ocn-record+1(5)    = it_abxocndt-natn.
    it_ocn-record+6(10)   = it_abxocndt-base.
    it_ocn-record+16(4)   = it_abxocndt-ocno.
    it_ocn-record+20(2)   = it_abxocndt-vers.
    it_ocn-record+22(4)   = it_abxocndt-occn.
    f_num = 26. s_num = 1.
    DO 60 TIMES.
      CONCATENATE 'IT_ABXOCNDT-OP' s_num INTO field.
      ASSIGN (field) TO <fs>.
      it_ocn-record+f_num(4) = <fs>.
      f_num = f_num + 4.s_num = s_num + 1.
    ENDDO.
    it_ocn-record+266(12)   = it_abxocndt-zuser.
    it_ocn-record+278(6)   = '      '.
    it_ocn-record+282(8)   = it_abxocndt-zsdat.
    it_ocn-record+290(6)   = it_abxocndt-zstim.
    it_ocn-record+296(8)   = it_abxocndt-zedat.
    it_ocn-record+304(6)   = it_abxocndt-zetim.

    APPEND it_ocn.

  ENDLOOP.
  CLEAR w_int.
  DESCRIBE TABLE it_ocn LINES w_int.
  IF w_int <> 0.
    DATA : d_datum LIKE sy-datum.
    d_datum = sy-datum - 1.
    CONCATENATE  p_mobis_imt
                   d_datum '_ocn.txt'
               INTO w_dsn.
    DELETE DATASET w_dsn.
    CONCATENATE  p_mobis_imt
                 sy-datum '_ocn.txt'
             INTO w_dsn.

    DELETE DATASET w_dsn.

    OPEN DATASET w_dsn IN TEXT MODE FOR APPENDING.

    LOOP AT it_ocn.
      TRANSFER it_ocn TO w_dsn.
    ENDLOOP.

    CLOSE DATASET w_dsn.
    IF sy-subrc = 0.
      MESSAGE i000 WITH text-001 '(OCN)'.
      CLEAR ztbm_mobis_log.
      MOVE : 'ZTBM_ABXOCNDT' TO ztbm_mobis_log-ztable,
              'H'            TO ztbm_mobis_log-gubun,
              sy-datum       TO ztbm_mobis_log-infdate,
              w_int          TO ztbm_mobis_log-zcount.
      MODIFY ztbm_mobis_log.
    ELSE.
      MESSAGE i000 WITH text-002 '(OCN)'.
    ENDIF.
  ENDIF.

ENDFORM.                    " DOWNLOAD_TEXT_OCN
*&---------------------------------------------------------------------*
*&      Form  download_text_c23
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_text_c23.
  DATA : BEGIN OF it_c23 OCCURS 0,
         record(134),
         END OF it_c23.
  CLEAR it_c23[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_abxc23dt
     FROM ztbm_abxc23dt
      WHERE zsdat EQ sy-datum.
*file download
  LOOP AT  it_abxc23dt.
    it_c23-record+0(4)    = it_abxc23dt-zc23_key1_c0000.
    it_c23-record+4(8)    = it_abxc23dt-zc23_key2_c0000.
    it_c23-record+12(20)  = it_abxc23dt-zc23_optn_c0000.
    it_c23-record+32(3)   = it_abxc23dt-zc23_natn_c0000.
    it_c23-record+35(3)   = it_abxc23dt-zc23_ccno_n0000.
    it_c23-record+38(4)   = it_abxc23dt-zc23_keyo_ccolr.
    it_c23-record+41(5)   = it_abxc23dt-zc23_clsf_nsequ.
    it_c23-record+44(6)   = it_abxc23dt-zc23_part_ccolr.
    it_c23-record+47(9)   = it_abxc23dt-zc23_e000_nstrt.
    it_c23-record+56(9)   = it_abxc23dt-zc23_e000_nfini.
    it_c23-record+65(8)   = it_abxc23dt-zc23_dsgn_dstrt.
    it_c23-record+73(8)   = it_abxc23dt-zc23_dsgn_dfini.
    it_c23-record+81(8)   = it_abxc23dt-zc23_prod_dstrt.
    it_c23-record+89(8)   = it_abxc23dt-zc23_prod_dfini.
    it_c23-record+97(1)   = it_abxc23dt-zc23_prod_cdsgn.
    it_c23-record+98(2)   = it_abxc23dt-zc23_plnt_cstrt.
    it_c23-record+100(3)  = it_abxc23dt-zc23_epin_cstrt.
    it_c23-record+103(2)  = it_abxc23dt-zc23_plnt_cfini.
    it_c23-record+105(3)  = it_abxc23dt-zc23_epin_cfini.
    it_c23-record+108(3)  = it_abxc23dt-zc23_prev_cpart.
    it_c23-record+111(8)  = it_abxc23dt-zc23_rgst_d0000.
    it_c23-record+119(8)  = it_abxc23dt-zc23_updt_d0000.
    it_c23-record+127(7)  = it_abxc23dt-zc23_auth_n0000.
    APPEND it_c23.
  ENDLOOP.
  CLEAR w_int.
  DESCRIBE TABLE it_c23 LINES w_int.
  IF w_int <> 0.
    DATA : d_datum LIKE sy-datum.
    d_datum = sy-datum - 1.
    CONCATENATE  p_mobis_imt
                   d_datum '_ccc.txt'
               INTO w_dsn.
    DELETE DATASET w_dsn.

    CONCATENATE  p_mobis_imt
                 sy-datum '_ccc.txt'
             INTO w_dsn.

    DELETE DATASET w_dsn.

    OPEN DATASET w_dsn IN TEXT MODE FOR APPENDING.

    LOOP AT it_c23.
      TRANSFER it_c23 TO w_dsn.
    ENDLOOP.

    CLOSE DATASET w_dsn.
    IF sy-subrc = 0.
      MESSAGE i000 WITH text-001 '(C23)'.
      CLEAR ztbm_mobis_log.
      MOVE : 'ZTBM_ABXC23DT' TO ztbm_mobis_log-ztable,
              'H'            TO ztbm_mobis_log-gubun,
              sy-datum       TO ztbm_mobis_log-infdate,
              w_int          TO ztbm_mobis_log-zcount.
      MODIFY ztbm_mobis_log.
    ELSE.
      MESSAGE i000 WITH text-002 '(C23)'.
    ENDIF.

  ENDIF.

ENDFORM.                    " download_text_c23

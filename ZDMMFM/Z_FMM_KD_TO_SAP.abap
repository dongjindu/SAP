FUNCTION z_fmm_kd_to_sap.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_CKD_ASN STRUCTURE  ZSMM_KD_ASN
*"----------------------------------------------------------------------

*--- data declaration
  DATA : it_ckd_asn TYPE ztmm_kd_asn OCCURS 0 WITH HEADER LINE,
         ws_ckd_asn  TYPE ztmm_kd_asn,

********  HASEEB Mohammad modifications start  UD1K949694
         count type I,
         flag type c,
         IT_TAB TYPE ztmm_kd_asn OCCURS 0 WITH HEADER LINE.
********  HASEEB Mohammad modifications complete  UD1K949694

*--- tables parameter into internal table
  LOOP AT t_ckd_asn.
    MOVE-CORRESPONDING t_ckd_asn TO it_ckd_asn.
    APPEND it_ckd_asn.
  ENDLOOP.

*--- table deletion
  DELETE ztmm_kd_asn FROM TABLE it_ckd_asn.

*--- table insert

*  INSERT ztmm_kd_asn FROM TABLE it_ckd_asn ACCEPTING DUPLICATE KEYS.

  MOVE: 'S' TO t_ckd_asn-zzret.
  loop at it_ckd_asn into ws_ckd_asn.
    modify ztmm_kd_asn from ws_ckd_asn.

    IF sy-subrc <> 0.
      MOVE : 'E' TO t_ckd_asn-zzret.
      flag = 'X'.
*  ELSE.
*     MOVE : 'S' TO t_ckd_asn-zzret.
    endif.
*   MODIFY t_ckd_asn TRANSPORTING zzret WHERE  TRAID = it_ckd_asn-TRAID
*                                 AND EMATN = it_ckd_asn-EMATN
*                                 AND MATNR = it_ckd_asn-MATNR
*                                 AND EBELN = it_ckd_asn-EBELN
*                                 AND traid GE space.
*
  endloop.


********  HASEEB Mohammad modifications start  UD1K949694
  if flag = 'X'.
    MODIFY t_ckd_asn TRANSPORTING zzret WHERE traid GE space.
  else.

    select count(*) into count from ztmm_kd_asn.

    CALL FUNCTION 'Z_FMM_KD_TO_SAP_EXECUTE'
         EXPORTING
              I_COUNT = count.

    data tmP_ZETIM type ZETIM.
    data tmp_ix type i.

    LOOP AT t_ckd_asn.

      write t_ckd_asn-ZETIM to tmP_ZETIM.
      tmp_ix = sy-tabix.
      SELECT * INTO table IT_TAB
             FROM ZTMM_KD_ASN_main  where
             TRAID = t_ckd_asn-TRAID and
           EMATN = t_ckd_asn-EMATN and
           MATNR = t_ckd_asn-MATNR and
           EBELN = t_ckd_asn-EBELN and
           ZEDAT = t_ckd_asn-ZEDAT and
           ZETIM = tmP_ZETIM.


      IF SY-SUBRC = 0.
        read table IT_TAB index 1.
        MOVE-CORRESPONDING IT_TAB TO T_CKD_ASN.
        MODIFY T_CKD_ASN index tmp_ix.
      ENDIF.
      clear IT_TAB.

    ENDLOOP.
  endif.
*MODIFY t_ckd_asn TRANSPORTING zzret WHERE traid GE space.

******** HASEEB Mohammad modifications  complete UD1K949694


*--- result return
*  IF sy-subrc EQ 0.
*    MOVE : 'S' TO t_ckd_asn-zzret.
*    MODIFY t_ckd_asn TRANSPORTING zzret WHERE traid GE space.
*  ELSE.
*    MOVE : 'E' TO t_ckd_asn-zzret.
*    MODIFY t_ckd_asn TRANSPORTING zzret WHERE traid GE space.
*  ENDIF.

*---
ENDFUNCTION.

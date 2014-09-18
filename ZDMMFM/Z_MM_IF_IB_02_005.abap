FUNCTION Z_MM_IF_IB_02_005.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(E_RETURN) LIKE  ZMMS0053 STRUCTURE  ZMMS0053
*"  TABLES
*"      IT_BODY STRUCTURE  ZMMT0034
*"----------------------------------------------------------------------


  DATA : l_mtyp   TYPE bwart,
         l_wbstk  LIKE vbuk-wbstk,
         l_mblnr  LIKE mkpf-mblnr,
         l_return LIKE zmms0053,
         ls_body  LIKE zmmt0034,
         lt_m034_d   LIKE  zmmt0034 OCCURS 0 WITH HEADER LINE,
         lt_m034     LIKE  zmmt0034 OCCURS 0 WITH HEADER LINE.

  CLEAR : gt_body_34,   it_body_541,   it_body_961,   lt_m034, lt_m034_d
,
          gt_body_34[], it_body_541[], it_body_961[], lt_m034[],
lt_m034_d[].

  IF it_body[] IS INITIAL.
    e_return-type    = 'E'.
    e_return-message = text-m01.
    EXIT.
  ENDIF.

  MODIFY zmmt0034 FROM TABLE it_body.

  IF sy-subrc EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

  gt_body_34[] = it_body[].

  PERFORM bapi_clear.

  SORT gt_body_34 BY budat.

  LOOP AT gt_body_34.
    g_tabix = sy-tabix.
    IF gt_body_34-bwart EQ '311'.
      MOVE-CORRESPONDING gt_body_34 TO it_body_541.
      APPEND it_body_541. CLEAR it_body_541.
      DELETE gt_body_34 INDEX g_tabix.
    ELSE.
      IF gt_body_34-matnr(5) EQ '20100' OR
         gt_body_34-matnr(5) EQ '45000' OR
         gt_body_34-matnr(5) EQ '43000'.
        MOVE-CORRESPONDING gt_body_34 TO it_body_541.
        it_body_541-bwart = '541'.
        APPEND it_body_541. CLEAR it_body_541.
        DELETE gt_body_34 INDEX g_tabix.
      ELSE.
        MOVE-CORRESPONDING gt_body_34 TO it_body_961.
        it_body_961-bwart = '961'.
        APPEND it_body_961. CLEAR it_body_961.
        DELETE gt_body_34 INDEX g_tabix.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF NOT it_body_961[] IS INITIAL.
    LOOP AT it_body_961.
      CONCATENATE it_body_961-zcrtdt it_body_961-zcrtim
                  it_body_961-lifnr
                  INTO it_body_961-zkey.
      MODIFY it_body_961.
    ENDLOOP.

    SORT it_body_961 BY zkey.

    LOOP AT it_body_961.
      MOVE-CORRESPONDING it_body_961 TO lt_m034.
      APPEND lt_m034. CLEAR lt_m034.

      AT END OF zkey.
        READ TABLE lt_m034 INDEX 1.

        IF lt_m034-vbeln_vl IS INITIAL.
          CALL FUNCTION 'Z_MM_CREATE_DELIVERY_VL01N'
            IMPORTING
              e_return = e_return
            TABLES
              it_body  = lt_m034.
        ENDIF.

        READ TABLE lt_m034 INDEX 1.
        IF NOT lt_m034-vbeln_vl IS INITIAL.
          CLEAR : l_wbstk.
          SELECT SINGLE wbstk
                 INTO l_wbstk
                 FROM vbuk
                 WHERE vbeln = lt_m034-vbeln_vl.
          IF l_wbstk NE 'C'.
            CALL FUNCTION 'Z_MM_BAPI_HU_CREATE'
              EXPORTING
                i_vbeln  = lt_m034-vbeln_vl
                i_budat  = lt_m034-budat
              IMPORTING
                e_return = e_return
              TABLES
                it_body  = lt_m034.

            IF e_return-type NE 'E'.
              CALL FUNCTION 'Z_MM_UNLOADING_CT_PACK_GI'
                EXPORTING
                  i_vbeln  = lt_m034-vbeln_vl
                  i_budat  = lt_m034-budat
                IMPORTING
                  e_return = e_return
                TABLES
                  it_body  = lt_m034.
            ENDIF.
          ENDIF.
        ENDIF.

        lt_m034-type = e_return-type.
        IF e_return-type EQ 'S'.
          lt_m034-message = e_return-message = lt_m034-vbeln_vl.
        ELSE.
          lt_m034-message = e_return-message.
        ENDIF.
        MODIFY lt_m034 TRANSPORTING type
                                    message
                          WHERE NOT vbeln_vl IS INITIAL.
        LOOP AT lt_m034.
          MOVE-CORRESPONDING lt_m034 TO lt_m034_d.
          APPEND lt_m034_d. CLEAR lt_m034_d.
        ENDLOOP.
        CLEAR : lt_m034, lt_m034[].
      ENDAT.
    ENDLOOP.
  ENDIF.

  IF NOT it_body_541[] IS INITIAL.
    LOOP AT it_body_541.
      CONCATENATE it_body_541-zcrtdt it_body_541-zcrtim
                  it_body_541-bktxt  it_body_541-budat
                  INTO it_body_541-zkey.
      MODIFY it_body_541.
    ENDLOOP.

    LOOP AT it_body_541 WHERE bwart = '541'.
      g_tabix = sy-tabix.
      CLEAR : l_mblnr.
      SELECT SINGLE mblnr INTO l_mblnr
             FROM mkpf
             WHERE bktxt = it_body_541-bktxt.
      IF sy-subrc EQ 0.
        DELETE it_body_541 INDEX g_tabix.
      ENDIF.
    ENDLOOP.

    SORT it_body_541 BY zkey.

    LOOP AT it_body_541.
      g_tabix = sy-tabix.
      MOVE-CORRESPONDING it_body_541 TO ls_body.

      PERFORM bapi_clear.
      PERFORM bapi_header_34 USING '04'
                                   ls_body-budat
                                   ls_body-bktxt.

      PERFORM bapi_item_34 USING it_body_541-bwart
                                 ' '.
      PERFORM bapi_gr.
      PERFORM bapi_return CHANGING l_return.

      it_body_541-mblnr   = materialdocument.
      it_body_541-mjahr   = matdocumentyear.
      IF l_return-type = 'S'.
        e_return-type    = it_body_541-type    = l_return-type.
        e_return-message = it_body_541-message = materialdocument.
      ELSE.
        e_return-type    = it_body_541-type    = l_return-type.
        e_return-message = it_body_541-message = l_return-message.
      ENDIF.
      MODIFY it_body_541  INDEX g_tabix
                           TRANSPORTING type
                                     message
                                     mblnr
                                     mjahr.
    ENDLOOP.
  ENDIF.

  LOOP AT lt_m034_d.
    MOVE-CORRESPONDING lt_m034_d TO gt_body_34.
    APPEND gt_body_34. CLEAR gt_body_34.
  ENDLOOP.

  LOOP AT it_body_541.
    MOVE-CORRESPONDING it_body_541 TO gt_body_34.
    APPEND gt_body_34. CLEAR gt_body_34.
  ENDLOOP.

**** LOGIC
  LOOP AT gt_body_34.
    g_tabix = sy-tabix.

    g_return = gt_body_34-type.
    gt_body_34-etnam = sy-uname.
    gt_body_34-etdat = sy-datum.
    gt_body_34-ettim = sy-uzeit.

    CALL METHOD zmmc_cl_if=>if_set_key(   ifkey = 'MMIF205_ECC_IB'
                              modle = 'GCS'       " 'MM', 'SD', 'FI'
                              centy = 'US'       "
                              dirct = 'I' " 'O':Outbound, 'I':Inbound
                              logox = ' '
                              ttype = 'S'
                              cparm = '7'
                           ).

    CALL METHOD zmmc_cl_if=>if_set_messg( type    = gt_body_34-type
                              id      = ' '    "gt_retmsg-id
                              message = gt_body_34-message
                            ).

    CALL METHOD zmmc_cl_if=>if_set_param( istat = g_return
                              ifp01 = gt_body_34-zcrtdt
                              ifp02 = gt_body_34-zcrtim
                              ifp03 = gt_body_34-lifnr
                              ifp04 = gt_body_34-bktxt
                              ifp05 = gt_body_34-budat
                              ifp06 = gt_body_34-matnr
                              ifp07 = gt_body_34-lgort
                              ifp08 = gt_body_34-umlgo
                              ifp09 = gt_body_34-bwart
                              ifp10 = gt_body_34-lifnr
*                              ifp11 = gt_body_34-mblnr
*                              ifp12 = gt_body_34-vbeln_vl
                            ).
    CALL METHOD zmmc_cl_if=>if_save_data( ).

    MODIFY gt_body_34 INDEX g_tabix.
  ENDLOOP.

  MODIFY zmmt0034 FROM TABLE gt_body_34.

  IF sy-subrc EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

  CLEAR : it_body, it_body[].

  it_body[] = gt_body_34[].




ENDFUNCTION.

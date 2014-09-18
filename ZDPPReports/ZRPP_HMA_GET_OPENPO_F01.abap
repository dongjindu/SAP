*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_F01                                        *
*----------------------------------------------------------------------*

FORM p2000_get_data.
*&#**********************************************************
* Open E-Order Search Condition
*     1. ZTPP_WOSUM
*          1) Sales Order <> space
*          2) Mod Qty > Seq Qty
*          3) W/Order = E*
*          3) Nation = B28
*          4) Dealer = A*
*     2. Vehicle Master
*          1) Current RP < 23 /X
*          2) W/Order = E*
*          2) Nation = B28
*          3) Dealer = A*
*          4) Usage = P
*#&*

  DATA : lt_wosum LIKE TABLE OF ztpp_wosum WITH HEADER LINE,
         lt_vm    LIKE TABLE OF ztpp_vm    WITH HEADER LINE.

  RANGES : r_woser FOR ztpp_wosum-wo_ser,
           r_dist  FOR ztpp_wosum-dealer.

  r_woser-sign   = r_dist-sign    = 'I'.
  r_woser-option = 'CP'.
  r_woser-low = 'E*'.
  r_dist-low  = 'A*'.
  APPEND : r_woser .
  IF NOT p_dealer IS INITIAL.
    r_dist-option = 'EQ'.
    r_dist-low  = p_dealer.
    APPEND r_dist.
  ELSE.
    r_dist-option  = 'CP'.
    r_dist-low  = 'A*'.
    APPEND r_dist.
  ENDIF.


  PERFORM p1000_start_progressbar USING '10'.

  IF p_extc <> '' AND p_intc <> ''.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_wosum
    FROM ztpp_wosum
    WHERE wo_ser IN s_woser " R_WOSER
      AND nation EQ p_nation                                "'B28'
      AND dealer IN r_dist
      AND extc EQ p_extc
      AND intc EQ p_intc
      AND sales NE space.
  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_wosum
    FROM ztpp_wosum
    WHERE wo_ser IN s_woser " R_WOSER
      AND nation EQ p_nation                                "'B28'
*    AND DEALER EQ P_DEALER
      AND dealer IN r_dist
      AND sales NE space.
  ENDIF.


  PERFORM p1000_start_progressbar USING '30'.
  LOOP AT lt_wosum .
    IF lt_wosum-modqty EQ lt_wosum-seqqty.
      IF lt_wosum-rp15tq EQ lt_wosum-seqqty.
        DELETE lt_wosum.
      ENDIF.
    ENDIF.
  ENDLOOP.

  PERFORM p1000_start_progressbar USING '40'.
  CHECK NOT lt_wosum[] IS INITIAL.

  SORT lt_wosum BY  wo_ser
                 nation
                 dealer
                 extc
                 intc   .
  PERFORM p1000_start_progressbar USING '50'.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_vm
    FROM ztpp_vm
    FOR ALL ENTRIES IN lt_wosum
    WHERE wo_serial = lt_wosum-wo_ser
      AND wo_nation = lt_wosum-nation
      AND wo_dealer = lt_wosum-dealer
      AND extc      = lt_wosum-extc
      AND intc      = lt_wosum-intc
      AND usg_car   = 'P'
      AND rp_cstatus <= p_rp
  %_HINTS ORACLE 'index("ZTPP_VM","ZTPP_VM~N1")'.

  PERFORM p1000_start_progressbar USING '70'.
  SORT lt_vm BY  wo_serial
                 wo_nation
                 wo_dealer
                 extc
                 intc   .

  DATA : lv_intno LIKE ztsd_um-intno,
         lv_uintno LIKE ztsd_um-intno,
         lv_zvin LIKE ztsd_um-zvin,
** Furong on 06/15/12
*         LV_ZVINCNT(9) TYPE N.
         lv_zvincnt(8) TYPE n.
** End on 06/15/12
  DATA : lv_plnqty LIKE ztpp_wosum-planqty.
  DATA : lv_objek LIKE ausp-objek,
         lv_bmdl  LIKE ausp-atwrt.

  DATA: l_var LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
        l_model_year(1),
        l_model(3),
        l_matnr LIKE mara-matnr.

  DATA : lt_ausp LIKE TABLE OF ausp WITH HEADER LINE.
  CLEAR : lv_intno, lv_plnqty.

  SORT : lt_ausp BY objek,
         lt_wosum BY wo_ser
                     nation
                     dealer
                     extc
                     intc,
         lt_vm BY model_code
                  body_no
                  wo_serial
                  wo_nation
                  wo_dealer
                  extc
                  intc.
  RANGES : r_zvin FOR ztsd_um-zvin.
  DATA   : lv_max LIKE ztsd_um-zvin.
  r_zvin-sign = 'I'.
  r_zvin-option = 'CP'.
  r_zvin-low    = 'Y*'.

** Furong on 06/15/12
*  SELECT MAX( ZVIN ) INTO  LV_MAX FROM ZTSD_UM
*  WHERE ZVIN IN R_ZVIN.
  SELECT MAX( zvin ) INTO  lv_max
  FROM ztsd_um
  WHERE zvin IN r_zvin
  AND wo_nation = p_nation.

** End

  LOOP AT lt_wosum .
    AT FIRST.
      IF  lv_max IS INITIAL .
        lv_zvincnt = 1.
      ELSE .
        lv_zvincnt = lv_max+1(17) .
        lv_zvincnt = lv_zvincnt + 1.
      ENDIF.
    ENDAT.

    CONCATENATE lt_wosum-wo_ser
                lt_wosum-nation
                lt_wosum-dealer
                lt_wosum-extc
                lt_wosum-intc INTO lv_objek.

*
*
*    READ TABLE LT_AUSP WITH KEY OBJEK = LV_OBJEK BINARY SEARCH.
*    IF SY-SUBRC = 0 .
*      CONTINUE.
*    ENDIF.

    IF lt_wosum-modqty EQ lt_wosum-seqqty.
      IF lt_wosum-rp15tq EQ lt_wosum-seqqty.
        CONTINUE.
      ENDIF.
    ENDIF.
    MOVE-CORRESPONDING lt_wosum TO gt_data.
    gt_data-wo_serial    = lt_wosum-wo_ser.
    gt_data-wo_nation    = lt_wosum-nation.
    gt_data-wo_dealer    = lt_wosum-dealer.
    gt_data-wo_extc      = lt_wosum-extc  .
    gt_data-wo_intc      = lt_wosum-intc  .
    gt_data-fsc          = lt_wosum-fsc   .
    gt_data-zvin         = lv_zvin.

    AT NEW wo_ser.
      SELECT MAX( intno ) INTO lv_intno
        FROM ztsd_um
       WHERE wo_serial = lt_wosum-wo_ser AND
** Furong on 06/13/12
*             WO_NATION = 'B28' AND
** End
             wo_dealer = 'AA'.

      IF sy-subrc <> 0.
        lv_intno = 1.
      ELSE.
        lv_intno = lv_intno + 1.
      ENDIF.
    ENDAT.

    LOOP AT lt_vm WHERE   wo_serial = lt_wosum-wo_ser
                      AND wo_nation = lt_wosum-nation
                      AND wo_dealer = lt_wosum-dealer
                      AND extc      = lt_wosum-extc
                      AND intc      = lt_wosum-intc .
** Furong on 06/15/12
*      CONCATENATE 'Y' LV_ZVINCNT INTO LV_ZVIN.
      CONCATENATE 'Y1' lv_zvincnt INTO lv_zvin.
** End on 06/15/12
      CONCATENATE lt_vm-model_code lt_vm-body_no INTO lv_objek.


      gt_data-intno = lv_intno.
      gt_data-zvin  = lv_zvin.
      gt_data-model_code = lt_vm-model_code.
      gt_data-body_no    = lt_vm-body_no.

      CONCATENATE '20' lt_wosum-wo_ser+1(4) INTO gt_data-pack.
      PERFORM get_ausp_value USING lv_objek  'P_VIN'  '002'
                             CHANGING gt_data-vin.
      PERFORM get_ausp_value USING lv_objek  'P_ENGINE_NO'  '002'
                             CHANGING gt_data-eng_no.
      PERFORM get_ausp_value USING lv_objek  'P_KEY_NO'  '002'
                             CHANGING gt_data-key_no.
      PERFORM get_ausp_value USING lv_objek  'P_AIRBAG_NO10'  '002'
                             CHANGING gt_data-esn_no.

      gt_data-rp_cstatus = lt_vm-rp_cstatus.
      PERFORM get_rpstatus USING gt_data-rp_cstatus
                                 gt_data-zvin_status.
      lv_zvincnt = lv_zvincnt + 1.
      lv_intno   = lv_intno + 1.

** Furong on 06/11/12
      REFRESH l_var.
      l_var-atnam = 'P_MODEL_YEAR'.
      APPEND l_var.
      l_matnr = lv_objek.
      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
        EXPORTING
          object       = l_matnr
          ctype        = '001'
        TABLES
          val_table    = l_var
        EXCEPTIONS
          no_data      = 1
          error_mode   = 2
          error_object = 3
          error_value  = 4
          OTHERS       = 5.

      IF sy-subrc = 0 .
        READ TABLE l_var INDEX 1.
        l_model_year = l_var-atwrt.
        l_model = lt_wosum-fsc+5(2).

        CALL FUNCTION 'Z_FPP_CONVERT_COLOR'
          EXPORTING
            i_model = l_model
            i_year  = l_model_year
            i_gubn  = ''            "HMMA -> HAC    " NULL
            i_extc  = gt_data-wo_extc
            i_intc  = gt_data-wo_intc
          IMPORTING
            e_extc  = gt_data-hacc_extc
            e_intc  = gt_data-hacc_intc.
      ENDIF.
** End on 06/11/12

      APPEND gt_data.

    ENDLOOP.

    CLEAR : gt_data-model_code, gt_data-body_no, gt_data-rp_cstatus,
            gt_data-zvin_status, gt_data-vin, gt_data-eng_no,
            gt_data-key_no, gt_data-esn_no.

    lv_plnqty = lt_wosum-forecastqty + lt_wosum-planqty.

    DO lv_plnqty TIMES.
** Furong on 06/15/12
*      CONCATENATE 'Y' LV_ZVINCNT INTO LV_ZVIN.
      CONCATENATE 'Y1' lv_zvincnt INTO lv_zvin.
** End
      gt_data-intno = lv_intno.
      gt_data-zvin  = lv_zvin.

** Furong on 06/11/12
      REFRESH l_var.
      l_var-atnam = 'P_MODEL_YEAR'.
      APPEND l_var.
      l_matnr = lv_objek.
      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
        EXPORTING
          object       = l_matnr
          ctype        = '001'
        TABLES
          val_table    = l_var
        EXCEPTIONS
          no_data      = 1
          error_mode   = 2
          error_object = 3
          error_value  = 4
          OTHERS       = 5.

      IF sy-subrc = 0 .
        READ TABLE l_var INDEX 1.
        l_model_year = l_var-atwrt.
        l_model = lt_wosum-fsc+5(2).

        CALL FUNCTION 'Z_FPP_CONVERT_COLOR'
          EXPORTING
            i_model = l_model
            i_year  = l_model_year
            i_gubn  = ''            "HMMA -> HAC    " NULL
            i_extc  = gt_data-wo_extc
            i_intc  = gt_data-wo_intc
          IMPORTING
            e_extc  = gt_data-hacc_extc
            e_intc  = gt_data-hacc_intc.
      ENDIF.
** End on 06/11/12

      APPEND gt_data.

      lv_intno = lv_intno + 1.
      lv_zvincnt = lv_zvincnt + 1.

    ENDDO.
  ENDLOOP.

  PERFORM p1000_start_progressbar USING '90'.

ENDFORM.                    "P2000_GET_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_START_PROGRESSBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p1000_start_progressbar USING percent.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = percent
      text       = text-001
    EXCEPTIONS
      OTHERS     = 1.
ENDFORM.                    " P1000_START_PROGRESSBAR

*&---------------------------------------------------------------------*
*&      Form  GET_RPSTATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_VM_RP_CSTATUS  text
*      -->P_LV_OSR_FILED  text
*----------------------------------------------------------------------*
FORM get_rpstatus USING    p_cstatus
                           p_field.


*# CS_RPSTATUS : HMA ZVIN STATUS
* ST06 : no body zvin
* ST07 : body seq
* ST08 : rp01
* ST09 : rp02, rp03, rp04, rp05, rp06
* ST10 : rp07, rp08, rp09, rp10,
*        rp11, rp12, rp13, rp14, rp15, rp16, rp17
* ST11 : rp18, rp19, rp20, rp21
* ST12 : rp22
* ST13 : rp23, rp24, 2p26
* ST15 : rp25, rp27.


  IF p_cstatus EQ '' OR p_cstatus IS INITIAL . EXIT. ENDIF.

  IF  p_cstatus EQ '27'
   OR p_cstatus EQ '25'.
    p_field = 'ST15' .EXIT.
  ENDIF.

  IF p_cstatus EQ '26'
   OR p_cstatus EQ '24'
   OR p_cstatus GE '23'.
    p_field = 'ST13' .EXIT.
  ENDIF.

  IF p_cstatus GE '22'. p_field = 'ST12' .EXIT.  ENDIF.

  IF p_cstatus GE '18'. p_field = 'ST11' .EXIT.  ENDIF.

  IF p_cstatus GE '07'. p_field = 'ST10' .EXIT.  ENDIF.

  IF p_cstatus GE '02'. p_field = 'ST09' .EXIT.  ENDIF.

  IF p_cstatus GE '01'. p_field = 'ST08' .EXIT.  ENDIF.

  IF p_cstatus GE '00'. p_field = 'ST07' .EXIT.  ENDIF.

  IF p_field IS INITIAL. p_field = 'ST01'.ENDIF.


ENDFORM.                    " GET_RPSTATUS
*&---------------------------------------------------------------------*
*&      Form  GET_AUSP_ENDPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_AUSP  text
*----------------------------------------------------------------------*
FORM get_ausp_endpo  TABLES p_ausp STRUCTURE ausp
                            p_wosum STRUCTURE ztpp_wosum.

  DATA : lv_atinn LIKE ausp-atinn,
         l_wosum LIKE ztpp_wosum.
  RANGES : r_objek FOR ausp-objek.

  r_objek-option = 'EQ'.
  r_objek-sign   = 'I'.
  LOOP AT p_wosum INTO l_wosum.
    CONCATENATE l_wosum-wo_ser
                l_wosum-nation
                l_wosum-dealer
                l_wosum-extc
                l_wosum-intc
                INTO r_objek-low .
*  R_OBJEK-OPTION = 'CP'.
*  R_OBJEK-SIGN   = 'I'.
*  R_OBJEK-LOW    = 'E*'.
    APPEND r_objek.

  ENDLOOP.


  PERFORM p4000_conversion_atinn USING 'P_PERF_YN' lv_atinn.

  SELECT objek atwrt INTO CORRESPONDING FIELDS OF TABLE p_ausp
    FROM ausp
    WHERE objek IN r_objek
      AND klart EQ '001'
      AND atinn EQ lv_atinn
      AND atwrt EQ 'Y'.
*  %_HINTS ORACLE 'index("AUSP","AUSP~N3")'.

ENDFORM.                    " GET_AUSP_ENDPO

*---------------------------------------------------------------------*
*       FORM P4000_CONVERSION_ATINN                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_VALUE                                                       *
*  -->  P_ATINN                                                       *
*---------------------------------------------------------------------*
FORM p4000_conversion_atinn USING p_value p_atinn .

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = p_value
    IMPORTING
      output = p_atinn.


ENDFORM.                    "P4000_CONVERSION_ATINN
*&---------------------------------------------------------------------*
*&      Form  P3000_SAVE_UM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p3000_save_um.
  DATA : lt_um LIKE TABLE OF ztsd_um WITH HEADER LINE,
         iline TYPE i,
         lv_objek LIKE ausp-objek,
         lv_bmdl  LIKE ausp-atwrt.

  CLEAR : lt_um[], lt_um.

  LOOP AT gt_data.
    CLEAR : lv_bmdl, lv_objek.
    MOVE-CORRESPONDING gt_data TO lt_um.

    CONCATENATE lt_um-wo_serial
                lt_um-wo_nation
                lt_um-wo_dealer
                lt_um-wo_extc
                lt_um-wo_intc INTO lv_objek.

    lt_um-ernam = sy-uname.
    lt_um-erdat = sy-datum.
    lt_um-erzet = sy-uzeit.
    lt_um-aenam = sy-uname.
    lt_um-aedat = sy-datum.
    lt_um-aezet = sy-uzeit.
    PERFORM p2210_getsingle_atwrt USING lv_objek lv_bmdl  'P_MI'.

    SELECT SINGLE model INTO lt_um-model_code
    FROM ztpp_model_conv WHERE bmdl EQ lv_bmdl .

    APPEND lt_um.
  ENDLOOP.

  DESCRIBE TABLE lt_um LINES iline.
*  DELETE FROM ZTSD_UM
*    WHERE ZVIN LIKE 'Y%'
*      AND WO_SERIAL LIKE 'E%'.
*  COMMIT WORK.
  INSERT ztsd_um FROM TABLE lt_um.
  IF sy-subrc = 0 .
    MESSAGE s001 WITH iline 'counts Complete.' .
    COMMIT WORK.
  ELSE.
    MESSAGE e001 WITH iline 'counts failed' .
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " P3000_SAVE_UM
*&---------------------------------------------------------------------*
*&      Form  P2210_GETSINGLE_ATWRT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_OBJEK  text
*      -->P_GT_DATA_BMDL  text
*      -->P_0310   text
*----------------------------------------------------------------------*
FORM p2210_getsingle_atwrt USING p_objek p_atwrt p_atnam.

  DATA : lv_atinn LIKE ausp-atinn.
  PERFORM p4000_conversion_atinn USING p_atnam lv_atinn.

  SELECT SINGLE atwrt INTO p_atwrt
  FROM ausp
  WHERE klart = '001'
    AND atinn = lv_atinn
    AND objek = p_objek
    AND mafid EQ 'O' .
* %_HINTS ORACLE 'index("AUSP","AUSP~N1")'

ENDFORM.                    " P2210_GETSINGLE_ATWRT
*&---------------------------------------------------------------------*
*&      Form  GET_AUSP_VALUE
*&---------------------------------------------------------------------*
FORM get_ausp_value  USING    p_objek
                              p_atinn
                              p_klart
                     CHANGING p_atwrt.

  DATA : lv_atinn LIKE ausp-atinn.

  PERFORM conversion_atinn USING p_atinn lv_atinn.

  SELECT SINGLE atwrt INTO p_atwrt
  FROM ausp
  WHERE klart = p_klart
    AND atinn = lv_atinn
    AND objek = p_objek
    AND mafid EQ 'O'.

ENDFORM.                    " GET_AUSP_VALUE
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_ATINN
*&---------------------------------------------------------------------*
FORM conversion_atinn  USING p_value p_atinn .

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = p_value
    IMPORTING
      output = p_atinn.

ENDFORM.                    " CONVERSION_ATINN

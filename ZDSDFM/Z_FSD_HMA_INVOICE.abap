FUNCTION z_fsd_hma_invoice.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(VBELN) LIKE  VBRK-VBELN
*"     VALUE(MESTYP) LIKE  EDP13-MESTYP DEFAULT 'ZINVOICE_MST'
*"     VALUE(SEGNAM_HD) LIKE  EDIDD-SEGNAM DEFAULT 'ZINVSEG1'
*"     VALUE(SEGNAM_IT) LIKE  EDIDD-SEGNAM DEFAULT 'ZINVSEG2'
*"     VALUE(ERDAT_ST) LIKE  SY-DATUM OPTIONAL
*"     VALUE(ERDAT_EN) LIKE  SY-DATUM OPTIONAL
*"  EXPORTING
*"     REFERENCE(RETURN) LIKE  BAPIRETURN STRUCTURE  BAPIRETURN
*"  TABLES
*"      HEAD STRUCTURE  ZINVSEG1 OPTIONAL
*"      ITEM STRUCTURE  ZINVSEG2 OPTIONAL
*"----------------------------------------------------------------------
*# define val

  DATA : xvbeln   LIKE vbeln,
         xobjek   LIKE mara-matnr,
         xwoser   LIKE ausp-atwrt ,
         xsdate   LIKE ausp-atwrt .

  DATA : xhead   LIKE TABLE OF zinvseg1 WITH HEADER LINE,
         xitem   LIKE TABLE OF zinvseg2 WITH HEADER LINE.

  DATA : xausp    LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
         xvbrk    LIKE TABLE OF vbrk WITH HEADER LINE,
         xvbpa    LIKE TABLE OF vbpa WITH HEADER LINE.


  DATA : BEGIN OF xvbrp OCCURS 0 .
          INCLUDE STRUCTURE vbrp.
  DATA :  bdno  LIKE ztsd_um-body_no ,
          mdcd LIKE ztsd_um-model_code,
         END OF xvbrp.
  DATA : wvbrp LIKE LINE OF xvbrp.

  DATA : xsdum LIKE TABLE OF ztsd_um WITH HEADER LINE.

  DATA : wtab(72) OCCURS 100 WITH HEADER LINE.

  DATA : lv_model(3).
  DATA : lv_model_year LIKE ztpp_vm-model_year.

  RANGES : r_erdat FOR sy-datum.



********
*For IDOC
********
  DATA : lt_edidd LIKE TABLE OF edidd WITH HEADER LINE,
         lt_edidc LIKE TABLE OF edidc WITH HEADER LINE,
         xmestyp  LIKE edp13-mestyp.

  CONSTANTS : c_mestyp(20) VALUE 'ZINVOICE_MST'.

  IF mestyp IS INITIAL .
    xmestyp = c_mestyp.
  ELSE.
    xmestyp = mestyp.
  ENDIF.
********

*# initialize

  CLEAR : xhead, xitem, xitem[], xobjek.
  MOVE  : vbeln TO xvbeln .

  CHECK NOT vbeln IS INITIAL OR
        NOT erdat_st IS INITIAL.

  IF  NOT erdat_st IS INITIAL.
    r_erdat-sign = 'I'.
    r_erdat-low = erdat_st.
    r_erdat-option = 'EQ'.
    APPEND r_erdat.
  ENDIF.

  IF NOT erdat_en IS INITIAL.
    r_erdat-high = erdat_en.
    r_erdat-option = 'BT'.
    MODIFY TABLE r_erdat.
  ENDIF.


  IF vbeln IS INITIAL.
    CONCATENATE 'ERDAT' 'IN' 'R_ERDAT'
    INTO wtab SEPARATED BY space.
    APPEND wtab.
  ELSE.
    CONCATENATE 'VBELN '  ' EQ '  ' '''xvbeln''' '
        INTO wtab .
    APPEND wtab.
  ENDIF.

  SELECT * INTO  CORRESPONDING FIELDS OF TABLE xvbrk
   FROM vbrk
   WHERE (wtab) .

  CHECK NOT xvbrk[] IS INITIAL.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE xvbpa
   FROM vbpa
   FOR ALL ENTRIES IN xvbrk
   WHERE vbeln EQ xvbrk-vbeln
     AND parvw EQ 'WE'.

*# make HEAD
  SORT :  xvbrk BY vbeln,
          xvbpa BY vbeln.

  LOOP AT xvbrk.
    READ TABLE xvbpa WITH KEY vbeln = xvbrk-vbeln.
    IF sy-subrc <> 0 .
      CLEAR xvbpa.
    ENDIF.
    xhead-ivnb = xvbrk-vbeln.
    xhead-ivdt = xvbrk-fkdat.
    xhead-dist = xvbpa-kunnr.
    xhead-hkmc = 'HMG'.

    CASE xvbrk-vbtyp.
      WHEN 'M'.
        xhead-ivtp = 'A'.
      WHEN 'N'.
        xhead-ivtp = 'B'.
* Creadit Memo , Debit Memo Invoice
      WHEN 'P'.
        xhead-ivtp = 'C'.
      WHEN 'O'.
        xhead-ivtp = 'D'.
    ENDCASE.

    APPEND xhead.

*****
* For IDOC
*****
    lt_edidd-segnam = segnam_hd.
    lt_edidd-sdata  = xhead    .

    APPEND lt_edidd .
  ENDLOOP.

*# make Item

  SELECT * INTO CORRESPONDING FIELDS OF xvbrp
   FROM vbrp
   FOR ALL ENTRIES IN xvbrk
   WHERE vbeln EQ xvbrk-vbeln .

    IF xvbrk-vbtyp EQ 'P' OR
       xvbrk-vbtyp EQ 'O'.

      SELECT SINGLE * INTO CORRESPONDING FIELDS OF wvbrp
        FROM vbrp
       WHERE vbeln EQ xvbrp-vgbel
         AND posnr EQ xvbrp-vgpos.

      xvbrp-vgbel = wvbrp-vgbel.
      xvbrp-mdcd  = wvbrp-vgbel+0(3).
      xvbrp-bdno  = wvbrp-vgbel+3(6).
    ELSE.
      xvbrp-mdcd  = xvbrp-vgbel+0(3).
      xvbrp-bdno  = xvbrp-vgbel+3(6).
    ENDIF.
    APPEND xvbrp.
  ENDSELECT.

  CHECK NOT xvbrp[] IS INITIAL.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE xsdum
    FROM ztsd_um
    FOR ALL ENTRIES IN xvbrp
    WHERE body_no = xvbrp-bdno
      AND model_code = xvbrp-mdcd
      AND status    NE 'S'
      AND status    NE 'D'.

  SORT : xvbrk BY vbeln ,
         xvbrp BY vbeln vgbel bdno mdcd,
         xhead BY ivnb  ,
         xsdum BY body_no model_code .

  LOOP AT xvbrp.

    READ TABLE xvbrk WITH KEY vbeln = xvbrp-vbeln.
    IF sy-subrc <> 0 . CLEAR xvbrk. ENDIF.

    READ TABLE xhead WITH KEY ivnb = xvbrp-vbeln.
    IF sy-subrc <> 0 . CLEAR xhead . ENDIF.

    READ TABLE xsdum WITH KEY body_no    = xvbrp-bdno
                              model_code = xvbrp-mdcd .
    IF sy-subrc <> 0 . CLEAR xsdum . ENDIF.

    xitem-hkmc          = 'HMG'.
    xitem-invtyp        = xhead-ivtp.
    xitem-ivnb          = xvbrp-vbeln.
    xitem-zfsc          = xvbrp-matnr.
    xitem-ivdt      = xvbrk-fkdat.
    xitem-xinvb	    = xvbrk-xblnr.
    xitem-uord      = xsdum-intno.
    xitem-zvin      = xsdum-zvin .
    xitem-curr      = xvbrk-stwae.
    xitem-foba      = xvbrp-netwr.
    xitem-zcif      = xvbrp-netwr.
    xitem-kgss      = xvbrp-ntgew.
    xitem-crdt      = xvbrp-erdat.


    PERFORM getsingle_atwrt USING :

    xvbrp-vgbel xitem-vinn          'P_VIN',
    xvbrp-vgbel xitem-mimi          'P_MI',
    xvbrp-vgbel xitem-occn          'P_OCN',
    xvbrp-vgbel xwoser          'P_WORK_ORDER',
    xvbrp-vgbel xitem-excl          'P_EXT_COLOR',
    xvbrp-vgbel xitem-incl          'P_INT_COLOR',
    xvbrp-vgbel xitem-prdt          'P_RP18_SHOP_DATE',
    xvbrp-vgbel xitem-msdate       'P_RP23_SHOP_DATE',
    xvbrp-vgbel xsdate         'P_RP23_ACTUAL_DATE'.


    xitem-wkno = xwoser+0(9).
    xitem-dist = xwoser+9(5).

    xitem-mdate = xsdate+0(8).
    xitem-mzeit = xsdate+8(6).

    xitem-sndt = sy-datum.


* Furong on 08/30/12 color conversion
    IF  xitem-dist+0(3)  <> 'B28'.
      CLEAR: lv_model, lv_model_year.
      lv_model  =  xvbrp-mdcd+0(2).
      SELECT SINGLE model_year INTO lv_model_year
      FROM ztpp_vm
      WHERE model_code   = xvbrp-mdcd
        AND body_no      = xvbrp-bdno.

      CALL FUNCTION 'Z_FPP_CONVERT_COLOR'
        EXPORTING
          i_model = lv_model
          i_year  = lv_model_year
          i_gubn  = ''            "HMMA -> HAC/HMM
          i_extc  = xitem-excl
          i_intc  = xitem-incl
        IMPORTING
          e_extc  = xitem-excl
          e_intc  = xitem-incl.
    ENDIF.
** endl
    APPEND xitem.
*****
* For IDOC
*****
    lt_edidd-segnam = segnam_it.
    lt_edidd-sdata  = xitem   .

    APPEND lt_edidd .

  ENDLOOP.

  head[] = xhead[].
  item[] = xitem[].


***********************************************************
*# Send IDOC
***********************************************************
  PERFORM send_idoc TABLES lt_edidd
                           lt_edidc USING xmestyp  xitem-dist.

  READ TABLE lt_edidc INDEX 1.
  IF sy-subrc = 0 .
    return-type = 'S'.
    return-code = lt_edidc-status.
    return-message = lt_edidc-docnum.
    return-message_v1 = vbeln.
  ELSE.
    return-type = 'E'.
    return-message = 'DOCNUM was not created'.
  ENDIF.

ENDFUNCTION.

FUNCTION z_fsd_hma_signoff.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(BODYNO) LIKE  AUSP-OBJEK
*"     REFERENCE(MESTYP) LIKE  EDP13-MESTYP DEFAULT 'ZSIGNOFF_MST'
*"     REFERENCE(SEGNAM) LIKE  EDIDD-SEGNAM DEFAULT 'ZSOFFSEG'
*"  EXPORTING
*"     REFERENCE(RETURN) LIKE  BAPIRETURN STRUCTURE  BAPIRETURN
*"     VALUE(OUTPUT) LIKE  ZSOFFSEG STRUCTURE  ZSOFFSEG
*"----------------------------------------------------------------------

  DATA : xmdcd    LIKE ztsd_um-model_code,
         xbdno    LIKE ztsd_um-body_no,
         xobjek   LIKE mara-matnr,
         xzsseg   LIKE zsoffseg,
         xwoser   LIKE ausp-atwrt ,
         xsdate   LIKE ausp-atwrt .

  DATA : xausp    LIKE TABLE OF zspp_vin_value WITH HEADER LINE.
  DATA : lv_model(3).
  DATA : lv_model_year LIKE ztpp_vm-model_year.

  CLEAR : xzsseg , xobjek.

  MOVE : bodyno TO xobjek .

  xmdcd = xobjek+0(3).
  xbdno = xobjek+3(6).

  DATA : lt_um LIKE TABLE OF ztsd_um WITH HEADER LINE.


  SELECT SINGLE zvin INTO xzsseg-zvin
*      INTO CORRESPONDING FIELDS OF TABLE LT_UM "XZSSEG-ZVIN
     FROM ztsd_um
    WHERE body_no    = xbdno
      AND model_code = xmdcd
      AND status     <> 'S'
      AND status     <> 'D'.

  PERFORM getsingle_atwrt USING :
    xobjek xzsseg-vin        'P_VIN',
    xobjek xzsseg-model_code 'P_MI' ,
    xobjek xzsseg-occn       'P_OCN' ,
    xobjek xwoser            'P_WORK_ORDER' ,
    xobjek xzsseg-wo_extc    'P_EXT_COLOR' ,
    xobjek xzsseg-wo_intc    'P_INT_COLOR' ,
    xobjek xzsseg-eng_no     'P_ENGINE_NO'   ,
    xobjek xzsseg-keyn       'P_KEY_NO'  ,
    xobjek xzsseg-shop_date  'P_RP18_SHOP_DATE'  ,
    xobjek xsdate            'P_RP18_ACTUAL_DATE',
    xobjek xzsseg-xmid       'P_AIRBAG_NO10',
    xobjek xzsseg-tm_no      'P_TM_NO',  "Victor 10.04.2011  T/M Serial
* by Daniel on 05/18/11 {
    xobjek xzsseg-telid       'P_AIRBAG_NO11',
* }
    xobjek xzsseg-immoid     'P_AIRBAG_NO16'.

  xzsseg-hkmc   = 'HMG'.
  xzsseg-wkno   = xwoser+0(9) .
  xzsseg-distco = xwoser+9(5) .
  xzsseg-act_date = xsdate+0(8).
  xzsseg-act_time = xsdate+8(14).

** Furong on 08/30/12 color conversion
IF  xzsseg-distco+0(3)  <> 'B28'.
    lv_model  =  xmdcd+0(2).
    SELECT SINGLE model_year INTO lv_model_year
    FROM ztpp_vm
    WHERE model_code   = xmdcd
      AND body_no      = xbdno.

    CALL FUNCTION 'Z_FPP_CONVERT_COLOR'
      EXPORTING
        i_model = lv_model
        i_year  = lv_model_year
        i_gubn  = ''            "HMMA -> HAC/HMM
        i_extc  = xzsseg-wo_extc
        i_intc  = xzsseg-wo_intc
      IMPORTING
        e_extc  = xzsseg-wo_extc
        e_intc  = xzsseg-wo_intc.
  ENDIF.
** end
  output = xzsseg.
***********************************************************
*# Create IDOC
***********************************************************
  DATA : lt_edidd LIKE TABLE OF edidd WITH HEADER LINE,
         lt_edidc LIKE TABLE OF edidc WITH HEADER LINE,
         xmestyp  LIKE edp13-mestyp.

  CONSTANTS : c_mestyp(20) VALUE 'ZSIGNOFF_MST'.

  IF mestyp IS INITIAL .
    xmestyp = c_mestyp.
  ELSE.
    xmestyp = mestyp.
  ENDIF.

  lt_edidd-segnam = segnam . "'ZSOFFSEG'.
  lt_edidd-sdata  = xzsseg .

  APPEND lt_edidd .

** Furong on 08/30/12 - destination by nation code
*  PERFORM send_idoc TABLES lt_edidd
*                           lt_edidc USING xmestyp.
 PERFORM send_idoc TABLES lt_edidd lt_edidc
                   USING xmestyp xzsseg-distco.
** End

  READ TABLE lt_edidc INDEX 1.
  IF sy-subrc = 0 .
    return-type = 'S'.
    return-code = lt_edidc-status.
    return-message = lt_edidc-docnum.
    return-message_v1 = bodyno.
  ELSE.
    return-type = 'E'.
    return-message = 'DOCNUM was not created'.
  ENDIF.

*  CHECK RETURN-TYPE EQ 'S' .
*
*  PERFORM SEND_GLOVIS USING OUTPUT.

ENDFUNCTION.

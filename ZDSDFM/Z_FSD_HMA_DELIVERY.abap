FUNCTION z_fsd_hma_delivery.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(BODYNO) LIKE  AUSP-OBJEK
*"     REFERENCE(MESTYP) LIKE  EDP13-MESTYP DEFAULT 'ZDELIVERY_MST'
*"     REFERENCE(SEGNAM) LIKE  EDIDD-SEGNAM DEFAULT 'ZDEVSEG'
*"  EXPORTING
*"     REFERENCE(RETURN) LIKE  BAPIRETURN STRUCTURE  BAPIRETURN
*"     VALUE(OUTPUT) LIKE  ZDEVSEG STRUCTURE  ZDEVSEG
*"----------------------------------------------------------------------

  DATA : xobjek   LIKE mara-matnr,
         xzdveg   LIKE zdevseg,
         xwoser   LIKE ausp-atwrt ,
         xsdate   LIKE ausp-atwrt .

  DATA : xausp    LIKE TABLE OF zspp_vin_value WITH HEADER LINE.
  DATA : lv_model(3).
  DATA : lv_model_year LIKE ztpp_vm-model_year.
  DATA : xmdcd    LIKE ztsd_um-model_code,
         xbdno    LIKE ztsd_um-body_no.

  CLEAR : xzdveg , xobjek.

  MOVE : bodyno TO xobjek .
  xmdcd = xobjek+0(3).
  xbdno = xobjek+3(6).

  PERFORM getsingle_atwrt USING :
    xobjek  xzdveg-vin         'P_VIN' ,
    xobjek  xzdveg-model_code  'P_MI'   ,
    xobjek  xzdveg-occn        'P_OCN' ,
    xobjek  xwoser             'P_WORK_ORDER' ,
    xobjek  xzdveg-wo_extc     'P_EXT_COLOR' ,
    xobjek  xzdveg-wo_intc     'P_INT_COLOR' ,
    xobjek  xzdveg-shop_date   'P_RP18_SHOP_DATE' ,
    xobjek  xsdate             'P_RP18_ACTUAL_DATE'.

  xzdveg-hkmc   = 'HMG'.
  xzdveg-wkno   = xwoser+0(9) .
  xzdveg-distco = xwoser+9(5) .
  xzdveg-act_date = xsdate+0(8).
  xzdveg-act_time = xsdate+8(14).

** Furong on 08/30/12 color conversion
  IF   xzdveg-distco+0(3)  <> 'B28'.
    lv_model  =  xmdcd+0(2).
    SELECT SINGLE model_year INTO lv_model_year
    FROM ztpp_vm
    WHERE model_code   = xmdcd
      AND body_no      = xbdno.

    CALL FUNCTION 'Z_FPP_CONVERT_COLOR'
      EXPORTING
        i_model = lv_model
        i_year  = lv_model_year
        i_gubn  = ''            "HMMA -> HAC
        i_extc  = xzdveg-wo_extc
        i_intc  = xzdveg-wo_intc
      IMPORTING
        e_extc  = xzdveg-wo_extc
        e_intc  = xzdveg-wo_intc.
  ENDIF.
** end

  output = xzdveg.
***********************************************************
*# Create IDOC
***********************************************************
  DATA : lt_edidd LIKE TABLE OF edidd WITH HEADER LINE,
         lt_edidc LIKE TABLE OF edidc WITH HEADER LINE,
         xmestyp  LIKE edp13-mestyp.

  CONSTANTS : c_mestyp(20) VALUE 'ZDELIVERY_MST'.

  IF mestyp IS INITIAL .
    xmestyp = c_mestyp.
  ELSE.
    xmestyp = mestyp.
  ENDIF.

  lt_edidd-segnam = segnam."'ZDEVSEG'.
  lt_edidd-sdata  = xzdveg .

  APPEND lt_edidd .

  PERFORM send_idoc TABLES lt_edidd
                           lt_edidc USING xmestyp xzdveg-distco.

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

ENDFUNCTION.

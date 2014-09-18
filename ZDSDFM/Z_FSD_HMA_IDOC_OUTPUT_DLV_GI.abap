FUNCTION z_fsd_hma_idoc_output_dlv_gi.
*"----------------------------------------------------------------------
*"*"Global Interface:
*"  IMPORTING
*"     VALUE(OBJECT) LIKE  NAST STRUCTURE  NAST
*"     VALUE(CONTROL_RECORD_IN) LIKE  EDIDC STRUCTURE  EDIDC
*"  EXPORTING
*"     VALUE(OBJECT_TYPE) LIKE  WFAS1-ASGTP
*"     VALUE(CONTROL_RECORD_OUT) LIKE  EDIDC STRUCTURE  EDIDC
*"  TABLES
*"      INT_EDIDD STRUCTURE  EDIDD
*"  EXCEPTIONS
*"      ERROR_MESSAGE_RECEIVED
*"      NO_UNIT_ORDER_FOUND
*"----------------------------------------------------------------------
  DATA : xmdcd    LIKE ztsd_um-model_code,
         xbdno    LIKE ztsd_um-body_no,
         xobjek   LIKE mara-matnr,

         xzdveg   LIKE zdevseg,
         xwoser   LIKE ausp-atwrt ,
         xsdate   LIKE ausp-atwrt ,
         rp23_shop   LIKE ausp-atwrt ,
         rp23_actual   LIKE ausp-atwrt ,
         rp25_shop   LIKE ausp-atwrt ,
         rp25_actual   LIKE ausp-atwrt ,
         rp27_shop   LIKE ausp-atwrt ,
         rp27_actual   LIKE ausp-atwrt .

  DATA : xedidd LIKE int_edidd.
  DATA : lv_model_year LIKE ztpp_vm-model_year.
  DATA : lv_model(3).

*-------MODEL FUNCTION: IDOC_OUTPUT_DELVRY
*# Step 1. prepare control_record_out

  DATA: h_logsys LIKE t000-logsys.

  CHECK object-nacha = '6' OR                           "EDI
        object-nacha = 'A'.                             "ALE
  CLEAR control_record_out.

  MOVE control_record_in TO control_record_out.

  CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET' "
       IMPORTING
            own_logical_system             = h_logsys
       EXCEPTIONS
            own_logical_system_not_defined = 1
            OTHERS                         = 2.

  IF ( sy-subrc IS INITIAL ).
    control_record_out-sndprt = 'LS'.
    control_record_out-sndprn = h_logsys.
  ELSE.
    MESSAGE ID 'B2' TYPE 'E' NUMBER '001'
            RAISING error_message_received.                 "#EC *
  ENDIF.

  control_record_out-serial = sy-datum.
  control_record_out-serial+8 = sy-uzeit.

*# Step 2.
* fill data in idoc table
  CLEAR : xzdveg .
* Key document
  xobjek = object-objky.  "BODY #
  xmdcd = xobjek+0(3).
  xbdno = xobjek+3(6).


  PERFORM getsingle_atwrt USING :
    xobjek  xzdveg-vin         'P_VIN' ,
    xobjek  xzdveg-model_code  'P_MI'   ,
    xobjek  xzdveg-occn        'P_OCN' ,
    xobjek  xwoser             'P_WORK_ORDER' ,
    xobjek  xzdveg-wo_extc     'P_EXT_COLOR' ,
    xobjek  xzdveg-wo_intc     'P_INT_COLOR' ,
*    XOBJEK  XZDVEG-SHOP_DATE   'P_RP23_SHOP_DATE' ,
    xobjek  rp23_shop          'P_RP23_SHOP_DATE' ,
    xobjek  rp23_actual        'P_RP23_ACTUAL_DATE',
    xobjek  rp25_shop          'P_RP25_SHOP_DATE' ,
    xobjek  rp25_actual        'P_RP25_ACTUAL_DATE',
    xobjek  rp27_shop          'P_RP27_SHOP_DATE' ,
    xobjek  rp27_actual        'P_RP27_ACTUAL_DATE'.


  xzdveg-hkmc   = 'HMG'.
  xzdveg-wkno   = xwoser+0(9) .
  xzdveg-distco = xwoser+9(5) .

*--<Money Gate D:rp22, E: rp24 or rp25   10.26.2011 Victor
  IF xzdveg-distco+0(3)  =  'B28'.
    xzdveg-shop_date  = rp23_shop.
    xzdveg-act_date   = rp23_actual+0(8).
    xzdveg-act_time   = rp23_actual+8(14).
  ELSE.
    IF rp25_shop IS INITIAL.
      xzdveg-shop_date  = rp27_shop.
      xzdveg-act_date   = rp27_actual+0(8).
      xzdveg-act_time   = rp27_actual+8(14).
    ELSE.
      xzdveg-shop_date  = rp25_shop.
      xzdveg-act_date   = rp25_actual+0(8).
      xzdveg-act_time   = rp25_actual+8(14).
    ENDIF.

*   Color conversion  2 -> 3 digit Victor 02.17.2012
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
*-->

  xedidd-segnam = 'ZDEVSEG'. "CONTROL_RECORD_IN-IDOCTP.
  xedidd-sdata  = xzdveg .
  APPEND xedidd TO int_edidd.

*--Victor 02.08.2012
  PERFORM modify_control_record USING  xzdveg-distco
                                CHANGING control_record_out.

*# Step 3. Send IDOC
*{ALE Begin} generation http://intranet.sap.com/materialversion
*Do not change coding between begin and end comments. PA8 20050511
  CALL FUNCTION 'MGV_ALE_ADD_EXTERNAL_MATNR'
    TABLES
      idoc_data   = int_edidd
    CHANGING
      idoc_header = control_record_out.
*{ALE End} generation
ENDFUNCTION.

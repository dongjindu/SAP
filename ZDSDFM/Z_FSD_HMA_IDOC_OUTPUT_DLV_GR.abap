FUNCTION z_fsd_hma_idoc_output_dlv_gr.
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
         xzsseg   LIKE zsoffseg,
         xwoser   LIKE ausp-atwrt ,
         xsdate   LIKE ausp-atwrt .
  DATA : lv_model_year LIKE ztpp_vm-model_year.
  DATA : lv_model(3).

* fill data in idoc table
  CLEAR : xzsseg .

*****************************************
*# Get Date change logic seq. 02032011
*****************************************

*-------MODEL FUNCTION: IDOC_OUTPUT_DELVRY
*# Step 1.
* Key document
  xobjek = object-objky.  "BODY #

  xmdcd = xobjek+0(3).
  xbdno = xobjek+3(6).

  SELECT  SINGLE zvin INTO xzsseg-zvin
     FROM ztsd_um
    WHERE body_no    = xbdno
      AND model_code = xmdcd
      AND status     <> 'S'
      AND status     <> 'D'.

  IF sy-subrc NE 0.
    MESSAGE e029(e0) WITH control_record_in-idoctp ''
          'ZTSD_UM data does not exist ' RAISING no_unit_order_found.
  ENDIF.
  SELECT SINGLE model_year INTO lv_model_year
  FROM ztpp_vm
  WHERE model_code   = xmdcd
    AND body_no      = xbdno.

  PERFORM getsingle_atwrt USING :
    xobjek xzsseg-vin        'P_VIN',
    xobjek xzsseg-model_code 'P_MI' ,
    xobjek xzsseg-occn       'P_OCN' ,
    xobjek xwoser            'P_WORK_ORDER' ,
    xobjek xzsseg-wo_extc    'P_EXT_COLOR' ,
    xobjek xzsseg-wo_intc    'P_INT_COLOR' ,
    xobjek xzsseg-eng_no     'P_ENGINE_NO'   ,
    xobjek xzsseg-keyn       'P_KEY_NO'  ,
** Furong on 08/28/12 for database update delay
*    xobjek xzsseg-shop_date  'P_RP18_SHOP_DATE'  ,
*    xobjek xsdate            'P_RP18_ACTUAL_DATE',
** End on 08/28/12
    xobjek xzsseg-xmid       'P_AIRBAG_NO10',
    xobjek xzsseg-tm_no      'P_TM_NO',  "Victor 10.04.2011  T/M Serial
* by Daniel on 05/18/11 {
    xobjek xzsseg-telid       'P_AIRBAG_NO11',
* }
    xobjek xzsseg-immoid     'P_AIRBAG_NO16'.
** Furong on 08/28/12 for database update delay
  DO 3 TIMES.
    PERFORM getsingle_atwrt USING :
       xobjek xzsseg-shop_date  'P_RP18_SHOP_DATE'  ,
       xobjek xsdate            'P_RP18_ACTUAL_DATE'.
    IF xzsseg-shop_date = '00000000'.
      WAIT UP TO 1 SECONDS.
      CONTINUE.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.
** End on 08/28/12

  xzsseg-hkmc     = 'HMG'.
  xzsseg-wkno     = xwoser+0(9) .
  xzsseg-distco   = xwoser+9(5) .
  xzsseg-act_date = xsdate+0(8).
  xzsseg-act_time = xsdate+8(14).

*-<      Color conversion  2 -> 3 digit Victor 02.17.2012
  IF  xzsseg-distco+0(3)  <> 'B28'.
    lv_model  =  xmdcd+0(2).
    CALL FUNCTION 'Z_FPP_CONVERT_COLOR'
      EXPORTING
        i_model = lv_model
        i_year  = lv_model_year
        i_gubn  = ''            "HMMA -> HAC
        i_extc  = xzsseg-wo_extc
        i_intc  = xzsseg-wo_intc
      IMPORTING
        e_extc  = xzsseg-wo_extc
        e_intc  = xzsseg-wo_intc.
  ENDIF.
*->
* prepare control_record_out
  DATA: h_logsys LIKE t000-logsys.

  CHECK object-nacha = '6' OR                           "EDI
        object-nacha = 'A'.                             "ALE
  CLEAR control_record_out.
  MOVE control_record_in TO control_record_out.

  CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
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

  DATA : xedidd LIKE int_edidd.

  xedidd-segnam = 'ZSOFFSEG'. "CONTROL_RECORD_IN-IDOCTP.
  xedidd-sdata  = xzsseg .
  APPEND xedidd TO int_edidd.

*--Victor 02.08.2012
  PERFORM modify_control_record USING xzsseg-distco
                                CHANGING control_record_out.

*{ALE Begin} generation http://intranet.sap.com/materialversion
*Do not change coding between begin and end comments. PA8 20050511
  CALL FUNCTION 'MGV_ALE_ADD_EXTERNAL_MATNR'
    TABLES
      idoc_data   = int_edidd
    CHANGING
      idoc_header = control_record_out.
*{ALE End} generation

*Send sign off data to glovis
*  PERFORM SEND_GLOVIS USING XZSSEG .

ENDFUNCTION.

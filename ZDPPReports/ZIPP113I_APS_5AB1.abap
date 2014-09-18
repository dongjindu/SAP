************************************************************************
* Program Name           : ZIPP113I_APS_5AB1
* Author                 : Furong Wang
* Creation Date          : 03/28/2006
* Specifications By      : Mr. Hur
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       : APS Reschedule Order Data Creation.
*----------------------------------------------------------------------
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
REPORT  zipp113i_aps_5ab1 MESSAGE-ID zmpp NO STANDARD PAGE HEADING .

TABLES: ztpp_pmt05ab.    "Reschedule's Order Detail(Before UPC Out)

DATA: it_pmt05ab LIKE table of ztpp_pmt05ab WITH HEADER LINE.
DATA: it_input_plan LIKE table of ztpp_input_plan WITH HEADER LINE.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME .
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_run          TYPE c AS CHECKBOX  DEFAULT 'X'.
SELECTION-SCREEN COMMENT  (55) text-001 FOR FIELD p_run.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b1.

CHECK p_run = 'X'  .
DELETE FROM ztpp_pmt05ab WHERE plnt <> space.
PERFORM read_data.
IF it_input_plan[] IS INITIAL.
  MESSAGE i000 WITH text-001.
ELSE.
  PERFORM process_data.
ENDIF.
*********************************************
END-OF-SELECTION.
*********************************************

*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data.
  SELECT * INTO TABLE it_input_plan
    FROM ztpp_input_plan.
ENDFORM.                    " read_data



*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data.
  DATA: l_ser TYPE i,
        l_tot type i,
        l_text(40),
        l_work_order like ztpp_input_plan-work_order.

  SORT it_input_plan BY work_order.
  REFRESH it_pmt05ab.
  CLEAR: it_pmt05ab, it_input_plan.
  l_work_order = '*'.
  LOOP AT it_input_plan.
    l_tot = l_tot + 1.
    if l_work_order <> it_input_plan-work_order.
       l_work_order = it_input_plan-work_order.
       clear: l_Ser.
    endif.
    l_ser = l_ser + 1.
    it_pmt05ab-plnt = '1'.
    it_pmt05ab-line = '1'.
    it_pmt05ab-modl = it_input_plan-modl.
    it_pmt05ab-usee = it_input_plan-work_order+0(1).
    it_pmt05ab-pack = it_input_plan-work_order+1(4).
    it_pmt05ab-regn = it_input_plan-work_order+5(1).
    it_pmt05ab-serl = it_input_plan-work_order+6(3).
    it_pmt05ab-dist = it_input_plan-work_order+9(5).
    it_pmt05ab-extc = it_input_plan-extc.
    it_pmt05ab-intc = it_input_plan-intc.
    it_pmt05ab-wser = l_ser.
    it_pmt05ab-bmdl = it_input_plan-mi.
    it_pmt05ab-ocnn = it_input_plan-ocnn.
    it_pmt05ab-vers = it_input_plan-vers.
    it_pmt05ab-zproc = it_input_plan-status.
    it_pmt05ab-dttm = it_input_plan-rp05.
    it_pmt05ab-sdat = it_input_plan-seq_date.
    it_pmt05ab-sser = it_input_plan-seq_serial.
    it_pmt05ab-sfdt = it_input_plan-rp18.
    it_pmt05ab-tidt = it_input_plan-rp06.
    it_pmt05ab-bidt = it_input_plan-rp01.
    it_pmt05ab-plgu = it_input_plan-rs18.
    it_pmt05ab-bdno = it_input_plan-body_ser.
    it_pmt05ab-zdate = sy-datum.
    it_pmt05ab-user_01 = sy-uname.
    append it_pmt05ab.
    CLEAR: it_pmt05ab, it_input_plan.
  ENDLOOP.

  INSERT ztpp_pmt05ab FROM TABLE it_pmt05ab .
  IF sy-subrc <> 0.
    ROLLBACK WORK .
    IF sy-batch = 'X'.
      MESSAGE w001 WITH text-102.
    ENDIF.
  ELSE.
    COMMIT WORK AND WAIT.
    WRITE l_tot TO l_text LEFT-JUSTIFIED .
    CONCATENATE 'Created Record Count :' l_text
      INTO l_text.
    MESSAGE s001 WITH l_text .
    IF sy-batch = 'X'.
      MESSAGE s001 WITH text-101.
    ENDIF.
  ENDIF.
ENDFORM.                    " process_data

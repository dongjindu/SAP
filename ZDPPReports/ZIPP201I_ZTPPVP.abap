************************************************************************
* Program Name      : ZIPP201I_ZTPPVP
* Author            : Jong Oh, Kim
* Creation Date     : 2003.09.16
* Specifications By : JongOh, Kim
* Pattern           : 5.2.2
* Development Request No : UD1K901977
* Addl Documentation:
* Description       : Vehicle Planned Order from PP to ALC
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT zipp201i_ztppvp  NO STANDARD PAGE HEADING
                          MESSAGE-ID zmpp.

TYPE-POOLS: slis.
*----------------------------------------------------------------------*
*  EXTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
TABLES : ztppvp,
         zsppvp,
         ztpp_pmt07jb_a,
         equi.
*VAL_TABLE	LIKE	ZSPP_VIN_VALUE
*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
DATA : it_pmt07jb_ir  LIKE TABLE OF ztpp_pmt07jb_a WITH HEADER LINE,
       it_pmt07jb_rp  LIKE TABLE OF ztpp_pmt07jb_a WITH HEADER LINE.

DATA : it_zsppvp_ir   LIKE TABLE OF zsppvp  WITH HEADER LINE,
       it_zsppvp_rp   LIKE TABLE OF zsppvp  WITH HEADER LINE,
       it_zsppvp_dl   LIKE TABLE OF zsppvp  WITH HEADER LINE,
       it_zsppvp_ba   LIKE TABLE OF zsppvp  WITH HEADER LINE,
       it_ztppvp      LIKE TABLE OF ztppvp  WITH HEADER LINE.

DATA : it_zsppvp      LIKE TABLE OF zsppvp  WITH HEADER LINE.
DATA : it_equi        LIKE TABLE OF equi  WITH HEADER LINE.
DATA : it_vm	    LIKE TABLE OF zspp_vin_value WITH HEADER LINE.

*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
DATA : wa_fname_tx(40),
       wa_saveline_ix     LIKE  sy-index.
DATA : wa_ir         LIKE  sy-tabix,   "Count IR
       wa_rp         LIKE  sy-tabix,   "Count RP
       wa_dl         LIKE  sy-tabix,   "Count DL
       wa_ba         LIKE  sy-tabix.   "Count BA

DATA : wa_atinn      TYPE  cabn-atinn,
       wa_objek      TYPE  ausp-objek,
       wa_atwrt_s    TYPE  ausp-atwrt,
       wa_atwrt_e    TYPE  ausp-atwrt,
       wa_atflv_s    TYPE  ausp-atflv,
       wa_atflv_e    TYPE  ausp-atflv.

DATA: w_answer(1).

RANGES: r_atinn      FOR  cabn-atinn,
        r_atnam      FOR  cabn-atnam.

*----------------------------------------------------------------------*
* ALV DECLARATION.
*----------------------------------------------------------------------*
DATA : ok_code       LIKE  sy-ucomm,
       save_ok_code  LIKE  sy-ucomm.
DATA : alv_grid               TYPE REF TO cl_gui_alv_grid,
       gs_custom_container    TYPE REF TO cl_gui_custom_container,
       wa_container           TYPE scrfname VALUE 'CONTAINER'.
*       GS_APPLICATION         TYPE REF TO LCL_APPLICATION,
DATA : gs_variant        TYPE disvariant ,  "Display Variant
       gs_layout         TYPE lvc_s_layo ,  "Layout
       gs_print          TYPE lvc_s_prnt ,  "Print control
       gt_special_groups TYPE lvc_t_sgrp ,  "Field groups
       gt_toolbar_excluding TYPE ui_functions , "Exclu Toolbar Std FUNC
       gt_header         TYPE TABLE OF slis_listheader WITH HEADER LINE,
       gt_fieldcat       TYPE lvc_t_fcat ,  "Field Catalog
       gt_sort           TYPE lvc_t_sort ,  "Sort criteria
       gt_filter         TYPE lvc_t_filt .  "Filter criteria
DATA : wa_fieldcat     TYPE lvc_s_fcat.
*----------------------------------------------------------------------*
*  CONSTANTS DECLARATION
*----------------------------------------------------------------------*
CONSTANTS: c_mark     VALUE  'X',
           c_gubb     VALUE  '*',
           c_mitu     VALUE  'M',
           c_job1(30) VALUE  'ZIPP201I_ZTPPVP',
** Changed by Furong On 05/19/2006, Requested by Mr Hur
*           C_KEY2(30) VALUE  'P_VM_DATE'      ,
           c_key2(30) VALUE  'P_SEQUENCE_DATE'      ,
** end of change
           c_dest(10) VALUE 'WMPP01'.   "Outbound Interface Destination
*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS : p_datum      LIKE   sy-datum   OBLIGATORY .
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  (20) text-100.
PARAMETERS: p_ir RADIOBUTTON GROUP r . "AS CHECKBOX.
SELECTION-SCREEN COMMENT  (12) text-101 FOR FIELD p_ir.   "INSERT
PARAMETERS: p_rp RADIOBUTTON GROUP r . "AS CHECKBOX.
SELECTION-SCREEN COMMENT  (12) text-102 FOR FIELD p_rp.   "REPLACE
PARAMETERS: p_dl RADIOBUTTON GROUP r . "AS CHECKBOX.
SELECTION-SCREEN COMMENT  (12) text-103 FOR FIELD p_dl.   "DELETE
PARAMETERS: p_ba RADIOBUTTON GROUP r . "AS CHECKBOX.
SELECTION-SCREEN COMMENT  (12) text-104 FOR FIELD p_ba.   "BACK
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  (5) text-200.
PARAMETERS: p_rew  AS CHECKBOX .
SELECTION-SCREEN COMMENT  (15) text-201 FOR FIELD p_rew.   " RE-WORK
SELECTION-SCREEN END OF LINE.

*----------------------------------------------------------------------*
*  MACRO DECLARATION
*----------------------------------------------------------------------*
DEFINE fieldcat_compose.
  it_fieldcat-fieldname = &1.
  it_fieldcat-reptext   = &2.
  it_fieldcat-outputlen = 20.
  append it_fieldcat to gt_fieldcat.

END-OF-DEFINITION.

DEFINE set_fieldcat.
  l_fieldcat-reptext   = &1.
  l_fieldcat-scrtext_l = &1.
  l_fieldcat-scrtext_m = &1.
  l_fieldcat-scrtext_s = &1.
  l_fieldcat-colddictxt = 'L'.
  l_fieldcat-outputlen = &2.

END-OF-DEFINITION.

************************************************************************
* INITIALIZAION
************************************************************************
INITIALIZATION.
*  PERFORM INITIALIZATION.

************************************************************************
* AT SELECTION-SCREEN
************************************************************************
AT SELECTION-SCREEN.
  PERFORM at_selection-screen.

************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
** Added by Furong on 02/04/11
  IF p_rew IS INITIAL.
    PERFORM check_data_exist.
  ELSE.
    w_answer = '1'.
  ENDIF.
  IF w_answer <> '1'.
    EXIT.
  ENDIF.
** End of change
  PERFORM call_emission    .
  PERFORM execute_process.
*  IF IT_ZTPPVP[] IS INITIAL.
*    MESSAGE E001 WITH 'No valid Data exist for selected condition'.
*  ENDIF.
************************************************************************
* END-OF-SELECTION
************************************************************************
END-OF-SELECTION.
  CALL SCREEN 9000.
*  PERFORM LIST_PROCESS.

*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM initialization.
ENDFORM.                    " INITIALIZATION

*&---------------------------------------------------------------------*
*&      Form  AT_SELECTION-SCREEN
*&---------------------------------------------------------------------*
FORM at_selection-screen.
 IF p_ir EQ space AND p_rp EQ space AND p_dl EQ space AND p_ba EQ space.
    MESSAGE e001 WITH text-211.
  ENDIF.
ENDFORM.                    " AT_SELECTION-SCREEN

*&---------------------------------------------------------------------*
*&      Form  EXECUTE_PROCESS
*&---------------------------------------------------------------------*
FORM execute_process.
  CLEAR :   it_pmt07jb_ir,  it_pmt07jb_rp,
            it_zsppvp_ir, it_zsppvp_rp,
            it_zsppvp_dl, it_zsppvp_ba,
            it_zsppvp,
            it_equi.
  REFRESH : it_pmt07jb_ir,  it_pmt07jb_rp,
            it_zsppvp_ir, it_zsppvp_rp,
            it_zsppvp_dl, it_zsppvp_ba,
            it_zsppvp,
            it_equi.
  CLEAR : wa_ir, wa_rp, wa_dl, wa_ba.
*-----> IR(INSERT)
  PERFORM append_ir.

*-----> RP(REPLACE)
  PERFORM append_rp.

*-----> READ VEHICLE MASTER FOR DL AND BA
  PERFORM read_vehicle_master.

* Get PIN Code
** Changed by Furong on 06/17/08
  PERFORM get_pin_code.
** end of change

ENDFORM.                    " EXECUTE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  READ_VEHICLE_MASTER
*&---------------------------------------------------------------------*
FORM read_vehicle_master.
  IF p_dl EQ c_mark.
    PERFORM read_dlba_from_vehicle.
  ENDIF.

  IF p_ba EQ c_mark.
    PERFORM read_dlba_from_vehicle.
*    CASE P_REW.
*      WHEN SPACE.
*        PERFORM READ_DLBA_FROM_VEHICLE.

*      WHEN C_MARK.
*        PERFORM REWORK_DL.
*        PERFORM REWORK_BA.
*    ENDCASE.
  ENDIF.
ENDFORM.                    " READ_VEHICLE_MASTER

*&---------------------------------------------------------------------*
*&      Form  APPEND_IR
*&---------------------------------------------------------------------*
FORM append_ir.
  CLEAR : wa_ir.
  IF p_ir EQ c_mark.
    CASE p_rew.
      WHEN  space.
        PERFORM select_ir_from_pmt07jb.

      WHEN c_mark.      "RE-WORK
        PERFORM rework_ir.

    ENDCASE.
    DESCRIBE TABLE it_zsppvp_ir LINES wa_ir.
  ENDIF.
ENDFORM.                    " APPEND_IR
*&---------------------------------------------------------------------*
*&      Form  APPEND_RP
*&---------------------------------------------------------------------*
FORM append_rp.
  CLEAR wa_rp.
  IF p_rp EQ c_mark.
    CASE p_rew.
      WHEN space.
        PERFORM select_rp_from_pmt07jb.

      WHEN c_mark.    "RE-WORK
        PERFORM rework_rp.

    ENDCASE.
    DESCRIBE TABLE it_zsppvp_rp LINES wa_rp.
  ENDIF.
ENDFORM.                    " APPEND_RP

*&---------------------------------------------------------------------*
*&      Form  SELECT_IR_FROM_PMT07JB
*&---------------------------------------------------------------------*
FORM select_ir_from_pmt07jb.
*  SELECT *
*         INTO TABLE IT_PMT07JB_IR
*         FROM ZTPP_PMT07JB_A
**         WHERE VHNO NE SPACE     " Serial of VIN
*          WHERE MTGU EQ SPACE     " ' '
*            AND GUBB EQ C_GUBB    " '*'
*            AND SQDT IN R_DATUM.  " Sequence Date
  " Read Vehicle Master for the P_VM_DATE = P_DATUM ...
  PERFORM get_vehicle    USING ' '  'IR' .
  IF sy-subrc EQ 0.
    LOOP AT it_pmt07jb_ir.
      IF it_pmt07jb_ir-vinn NE space.
        MOVE-CORRESPONDING it_pmt07jb_ir TO it_zsppvp_ir.
        MOVE it_pmt07jb_ir-ernam+0(6)    TO it_zsppvp_ir-k01pno.
        MOVE it_pmt07jb_ir-vinn+11(6)    TO it_zsppvp_ir-vhno.
        MOVE it_pmt07jb_ir-gubb          TO it_zsppvp_ir-p_emmission.
        it_zsppvp_ir-flg = 'IR'.
        APPEND it_zsppvp_ir.
        MOVE-CORRESPONDING it_zsppvp_ir TO it_zsppvp.
        APPEND it_zsppvp.
        CLEAR : it_zsppvp_ir, it_zsppvp.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " SELECT_IR_FROM_PMT07JB


*&---------------------------------------------------------------------*
*&      Form  REWORK_IR
*&---------------------------------------------------------------------*
FORM rework_ir.
  SELECT *
         INTO CORRESPONDING FIELDS OF TABLE it_zsppvp_ir
         FROM ztppvp
         WHERE flg  EQ 'IR'
           AND sqdt = p_datum.       " IN R_DATUM.
  LOOP AT it_zsppvp_ir.
    MOVE-CORRESPONDING it_zsppvp_ir TO it_zsppvp.
    APPEND it_zsppvp.
    CLEAR it_zsppvp.
  ENDLOOP.
ENDFORM.                    " REWORK_IR

*&---------------------------------------------------------------------*
*&      Form  SELECT_RP_FROM_PMT07JB
*&---------------------------------------------------------------------*
FORM select_rp_from_pmt07jb.
*  SELECT *
*         INTO TABLE IT_PMT07JB_RP
*         FROM ZTPP_PMT07JB_A
**         WHERE VHNO NE SPACE      " Serial of VIN
*          WHERE MTGU EQ C_MITU     " 'M'
*            AND GUBB EQ C_GUBB     " '*'
*            AND SQDT IN R_DATUM.   " Sequence Date
  " Read Vehicle Master for the P_VM_DATE = P_DATUM ...
  PERFORM get_vehicle    USING 'M'  'RP' .
  IF sy-subrc EQ 0.
    LOOP AT it_pmt07jb_rp.
      IF it_pmt07jb_rp-vinn NE space.
        MOVE-CORRESPONDING it_pmt07jb_rp TO it_zsppvp_rp.
        MOVE it_pmt07jb_rp-ernam+0(6)    TO it_zsppvp_rp-k01pno.
        MOVE it_pmt07jb_rp-vinn+11(6)    TO it_zsppvp_rp-vhno.
        it_zsppvp_rp-flg = 'RP'.
        APPEND it_zsppvp_rp.
        MOVE-CORRESPONDING it_zsppvp_rp TO it_zsppvp.
        APPEND it_zsppvp.
        CLEAR : it_zsppvp_rp, it_zsppvp.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " SELECT_RP_FROM_PMT07JB

*&---------------------------------------------------------------------*
*&      Form  REWORK_RP
*&---------------------------------------------------------------------*
FORM rework_rp.
  SELECT *
     INTO CORRESPONDING FIELDS OF TABLE it_zsppvp_rp
     FROM ztppvp
     WHERE flg  EQ 'RP'
       AND sqdt = p_datum.          " IN R_DATUM.
  LOOP AT it_zsppvp_rp.
    MOVE-CORRESPONDING it_zsppvp_rp TO it_zsppvp.
    APPEND it_zsppvp.
    CLEAR it_zsppvp.
  ENDLOOP.
ENDFORM.                    " REWORK_RP

*&---------------------------------------------------------------------*
*&      Form  SELECT_DL_FROM_VEHICLE
*&---------------------------------------------------------------------*
FORM select_dl_from_vehicle.
  DATA : l_atinn    LIKE  cabn-atinn,
*         L_ATNAM  LIKE  CABN-ATNAM,
         l_status   LIKE  ausp-atwrt,
         l_usage    LIKE  ausp-atwrt,
         l_atwrt18  LIKE  ausp-atwrt,
         l_datum    LIKE  sy-datum.

  PERFORM classification_value USING 'P_RP18_ACTUAL_DATE'
                               CHANGING l_atwrt18.
  MOVE l_atwrt18(8)   TO       l_datum.
  IF l_datum   LE  p_datum.
    PERFORM classification_value USING 'P_STATUS'
                                 CHANGING l_status.
    PERFORM classification_value USING 'P_USAGE_CAR'
                                 CHANGING l_usage.
    IF l_status EQ 'T24' OR l_usage EQ 'S'.
      PERFORM append_dl.
    ENDIF.
  ENDIF.
*  DATA:    L_STATUS      LIKE  ZSPP_VIN_VALUE-ATWRT,
*           L_USAGE_CAR   LIKE  ZSPP_VIN_VALUE-ATWRT,
*           L_RP18        LIKE  SY-DATUM . "ZSPP_VIN_VALUE-ATWRT.
*  DATA:  WA_FLG.
*  IF P_DL EQ C_MARK.
*    CLEAR : L_RP18, L_STATUS, L_USAGE_CAR.
*
**----> P_RP18_SHOP_DATE (Sign/Off On)
*    READ TABLE IT_VM WITH KEY ATNAM = 'P_RP18_ACTUAL_DATE'.
*    IF SY-SUBRC EQ 0.
*      L_RP18 = IT_VM-ATWRT(8).
*    ENDIF.
*
*    IF R_DATUM-HIGH IS INITIAL.
*      IF L_RP18 EQ S_DATUM-LOW.
*        WA_FLG = 'X'.
*      ELSE.
*        CLEAR WA_FLG.
*      ENDIF.
*    ELSE.
*      IF L_RP18 GE R_DATUM-LOW AND L_RP18 LE R_DATUM-HIGH.
*        WA_FLG = 'X'.
*      ELSE.
*        CLEAR WA_FLG.
*      ENDIF.
*    ENDIF.
*
*    IF WA_FLG EQ 'X'.
**----> P_STATUS
*      READ TABLE IT_VM WITH KEY ATNAM = 'P_STATUS'.
*      IF SY-SUBRC EQ 0 AND IT_VM-ATWRT EQ 'T24'.
*        L_STATUS = IT_VM-ATWRT.
*      ENDIF.
**----> P_USAGE_CAR
*      READ TABLE IT_VM WITH KEY ATNAM = 'P_USAGE_CAR'.
*      IF SY-SUBRC EQ 0 AND IT_VM-ATWRT EQ 'S'.
*        L_USAGE_CAR = IT_VM-ATWRT.
*      ENDIF.
*
*      IF L_STATUS EQ 'T24' OR L_USAGE_CAR EQ 'S'.
*        IT_ZSPPVP_DL-FLG   = 'DL'.
**----> P_MODEL
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_MODEL'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_DL-MODL  = IT_VM-ATWRT.
*        ENDIF.
**----> P_BODY_SERIAL
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_BODY_SERIAL'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_DL-VHNO  = IT_VM-ATWRT.
*        ENDIF.
*
*        APPEND IT_ZSPPVP_DL.
*        MOVE-CORRESPONDING IT_ZSPPVP_DL TO IT_ZSPPVP.
*        APPEND IT_ZSPPVP.
*        CLEAR : IT_ZSPPVP_DL, IT_ZSPPVP.
*      ENDIF.
*    ENDIF.
*  ENDIF.
ENDFORM.                    " SELECT_DL_FROM_VEHICLE

*&---------------------------------------------------------------------*
*&      Form  REWORK_DL
*&---------------------------------------------------------------------*
FORM rework_dl.
  SELECT *
     INTO CORRESPONDING FIELDS OF TABLE it_zsppvp_dl
     FROM ztppvp
     WHERE flg  EQ 'DL'
       AND sqdt =  p_datum.      " IN S_DATUM.

  LOOP AT it_zsppvp_dl.
    MOVE-CORRESPONDING it_zsppvp_dl TO it_zsppvp.
    APPEND it_zsppvp.
    CLEAR it_zsppvp.
  ENDLOOP.
ENDFORM.                    " REWORK_DL

*&---------------------------------------------------------------------*
*&      Form  SELECT_BA_FROM_VEHICLE
*&---------------------------------------------------------------------*
FORM select_ba_from_vehicle.
  DATA : l_atinn  LIKE  cabn-atinn,
         l_atwrt  LIKE  ausp-atwrt.

  PERFORM classification_value USING 'P_USAGE_CAR'
                               CHANGING l_atwrt.
  IF sy-subrc EQ 0 AND l_atwrt EQ 'R'.
    MOVE  sy-mandt TO    it_zsppvp_ba-mandt.
    MOVE  'BA'     TO    it_zsppvp_ba-flg.
    PERFORM classification_value USING 'P_MODEL'
                                 CHANGING it_zsppvp_ba-modl.
    PERFORM classification_value USING 'P_BODY_SERIAL'
                                 CHANGING it_zsppvp_ba-vhno.
    PERFORM classification_value USING 'P_WORKORDER'
                                 CHANGING it_zsppvp_ba-ordr.
    PERFORM classification_value USING 'P_DESTINATION_CODE'
                                 CHANGING it_zsppvp_ba-dist.
    PERFORM classification_value USING 'P_INT_COLOR'
                                 CHANGING it_zsppvp_ba-intc.
    PERFORM classification_value USING 'P_EXT_COLOR'
                                 CHANGING it_zsppvp_ba-extc.
    PERFORM classification_value USING 'P_VIN'
                                 CHANGING it_zsppvp_ba-vinn.
    PERFORM classification_value USING 'P_BODY_PLANT_NO'
                                 CHANGING it_zsppvp_ba-plnt.
    PERFORM classification_value USING 'P_BODY_LINE_NO'
                                 CHANGING it_zsppvp_ba-line.
    PERFORM classification_value USING 'P_SEQUENCE_SERIAL'
                                 CHANGING it_zsppvp_ba-ssr1.
    PERFORM classification_value USING 'P_EMMISSION'
                                 CHANGING it_zsppvp_ba-p_emmission.
    PERFORM classification_value USING 'P_SEQUENCE_DATE'
                                 CHANGING it_zsppvp_ba-sqdt.
    PERFORM classification_value USING 'P_ENGINE_NO'
                                 CHANGING it_zsppvp_ba-p_engine_no.
    PERFORM classification_value USING 'P_KEY_NO'
                                 CHANGING it_zsppvp_ba-p_key_no.
    PERFORM classification_value USING 'P_TM_NO'
                                 CHANGING it_zsppvp_ba-p_tm_no.
    APPEND it_zsppvp_ba.
    wa_ba = wa_ba + 1.
    MOVE-CORRESPONDING it_zsppvp_ba TO it_zsppvp.
    APPEND it_zsppvp.
    CLEAR : it_zsppvp_ba, it_zsppvp.
  ENDIF.

*  DATA : L_USAGE_CAR   LIKE  ZSPP_VIN_VALUE-ATWRT,
*         L_RETURN_DT   LIKE  SY-DATUM,
*         WA_FLG.
*
*  IF P_BA EQ C_MARK.
*    CLEAR : L_USAGE_CAR.
**----> RETURN DATE
*    READ TABLE IT_VM WITH KEY ATNAM = 'P_RETURN_DATE'.
*    IF SY-SUBRC EQ 0.
*      L_RETURN_DT = IT_VM-ATWRT(8).
*    ENDIF.
*
*    IF R_DATUM-HIGH IS INITIAL.
*      IF L_RETURN_DT EQ R_DATUM-LOW.
*        WA_FLG = 'X'.
*      ELSE.
*        CLEAR WA_FLG.
*      ENDIF.
*    ELSE.
*      IF L_RETURN_DT GE R_DATUM-LOW AND L_RETURN_DT LE R_DATUM-HIGH.
*        WA_FLG = 'X'.
*      ELSE.
*        CLEAR WA_FLG.
*      ENDIF.
*    ENDIF.
*
*    IF WA_FLG EQ 'X'.
**---> P_USAGE_CAR
*      READ TABLE IT_VM WITH KEY ATNAM = 'P_USAGE_CAR'.
*      IF SY-SUBRC EQ 0 AND IT_VM-ATWRT EQ 'R'.
*        L_USAGE_CAR = IT_VM-ATWRT.
*      ENDIF.
*
*      IF L_USAGE_CAR EQ 'R'.
**---> P_MODEL
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_MODEL'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_BA-MODL  = IT_VM-ATWRT.
*        ENDIF.
**---> P_BODY_SERIAL
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_BODY_SERIAL'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_BA-VHNO  = IT_VM-ATWRT.
*        ENDIF.
*
**---> P_WORK_ORDER
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_WORK_ORDER'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_BA-ORDR  = IT_VM-ATWRT.
*        ENDIF.
*
**---> P_DESTINATION_CODE
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_DESTINATION_CODE'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_BA-DIST  = IT_VM-ATWRT.
*        ENDIF.
*
**---> P_INT_COLOR
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_INT_COLOR'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_BA-INTC  = IT_VM-ATWRT.
*        ENDIF.
*
**---> P_EXT_COLOR
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_EXT_COLOR'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_BA-EXTC  = IT_VM-ATWRT.
*        ENDIF.
*
**---> P_VIN
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_VIN'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_BA-VINN  = IT_VM-ATWRT.
*        ENDIF.
*
**---> P_VIN
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_VIN'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_BA-VINN  = IT_VM-ATWRT.
*        ENDIF.
*
**---> P_BODY_PLANT_NO
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_BODY_PLANT_NO'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_BA-PLNT  = IT_VM-ATWRT.
*        ENDIF.
*
**---> P_BODY_LINE_NO
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_BODY_LINE_NO'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_BA-LINE  = IT_VM-ATWRT.
*        ENDIF.
*
**---> P_SEQUENCE_SERIAL
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_SEQUENCE_SERIAL'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_BA-SSR1  = IT_VM-ATWRT.
*        ENDIF.
*
**---> P_EMISSION
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_EMISSION'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_BA-P_EMMISSION  = IT_VM-ATWRT.
*        ENDIF.
*
**---> P_SEQUENCE_DATE
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_SEQUENCE_DATE'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_BA-SQDT  = IT_VM-ATWRT.
*        ENDIF.
*
*        IT_ZSPPVP_BA-FLG   = 'BA'.
**---> P_ENGINE_NO
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_ENGINE_NO'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_BA-P_ENGINE_NO  = IT_VM-ATWRT.
*        ENDIF.
**---> P_KEY_NO
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_KEY_NO'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_BA-P_KEY_NO  = IT_VM-ATWRT.
*        ENDIF.
**---> P_TM_NO
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_TM_NO'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_BA-P_TM_NO  = IT_VM-ATWRT.
*        ENDIF.
*        APPEND IT_ZSPPVP_BA.
*        MOVE-CORRESPONDING IT_ZSPPVP_BA TO IT_ZSPPVP.
*        APPEND IT_ZSPPVP.
*        CLEAR : IT_ZSPPVP_BA, IT_ZSPPVP.
*      ENDIF.
*    ENDIF.
*  ENDIF.
ENDFORM.                    " SELECT_BA_FROM_VEHICLE

*&---------------------------------------------------------------------*
*&      Form  REWORK_BA
*&---------------------------------------------------------------------*
FORM rework_ba.
  SELECT *
     INTO CORRESPONDING FIELDS OF TABLE it_zsppvp_ba
     FROM ztppvp
     WHERE flg  EQ 'BA'
       AND sqdt =  p_datum .     " IN S_DATUM.

  LOOP AT it_zsppvp_ba.
    MOVE-CORRESPONDING it_zsppvp_ba TO it_zsppvp.
    APPEND it_zsppvp.
    CLEAR it_zsppvp.
  ENDLOOP.
ENDFORM.                    " REWORK_BA

*&---------------------------------------------------------------------*
*&      Form  READ_DLBA_FROM_VEHICLE
*&---------------------------------------------------------------------*
FORM read_dlba_from_vehicle.
  DATA : l_atfor   TYPE  cabn-atfor,
         l_num(8) TYPE  n,
         l_int     TYPE  i.

  IF p_dl EQ c_mark.
    CLEAR : wa_dl, wa_atinn, l_atfor.
    SELECT SINGLE atinn
                  atfor
                INTO (wa_atinn, l_atfor)
                FROM cabn
                WHERE atnam EQ 'P_RP18_ACTUAL_DATE'.
  ELSEIF p_ba EQ c_mark.
    CLEAR : wa_ba, wa_atinn, l_atfor.
    SELECT SINGLE atinn
                  atfor
                INTO (wa_atinn, l_atfor)
                FROM cabn
                WHERE atnam EQ 'P_RETURN_DATE'.
  ENDIF.

  IF l_atfor EQ 'CHAR'.
    CONCATENATE p_datum '9999999'        INTO wa_atwrt_e .
*    CONCATENATE P_DATUM '%'              INTO WA_ATWRT_E .
    EXEC SQL PERFORMING SELECT_DLBA_FROM_VEHICLE.
      SELECT A.OBJEK
        INTO :WA_OBJEK
        FROM AUSP A, EQUI B
       WHERE B.MANDT = :SY-MANDT
         AND B.EQTYP = 'V'
         AND A.MANDT = :SY-MANDT
         AND A.OBJEK = B.EQUNR
         AND A.ATINN = :WA_ATINN
         AND A.ATWRT <= :WA_ATWRT_E
*                                    BETWEEN :WA_ATWRT_S AND :WA_ATWRT_E
         AND A.KLART = '002'
    ENDEXEC.
  ELSE.
    MOVE : p_datum        TO    l_num.
    MOVE : l_num          TO    wa_atflv_e.
    EXEC SQL PERFORMING SELECT_DLBA_FROM_VEHICLE.
      SELECT A.OBJEK
        INTO :WA_OBJEK
        FROM AUSP A, EQUI B
       WHERE B.MANDT = :SY-MANDT
         AND B.EQTYP = 'V'
         AND A.MANDT = :SY-MANDT
         AND A.OBJEK = B.EQUNR
         AND A.ATINN = :WA_ATINN
         AND A.ATFLV <= :WA_ATFLV_E
         AND A.KLART = '002'
    ENDEXEC.
  ENDIF.


*  SELECT *
*         INTO TABLE IT_EQUI
*         FROM EQUI
*         WHERE EQTYP  EQ 'V'.
**           AND LVORM  EQ SPACE.
*  LOOP AT IT_EQUI.
*    CLEAR : IT_VM, IT_VM[].
**-----> READ IT_VM FOR VEHICLE MASTER
*    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
*      EXPORTING
*        OBJECT           = IT_EQUI-EQUNR
**             MODE             = 'R'
*      TABLES
*        VAL_TABLE        = IT_VM
*      EXCEPTIONS
*       NO_DATA          = 1
*       ERROR_MODE       = 2
*       OTHERS           = 3.
*    IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ELSE.
*      PERFORM SELECT_DL_FROM_VEHICLE.
*      PERFORM SELECT_BA_FROM_VEHICLE.
*      DESCRIBE TABLE IT_ZSPPVP_DL LINES WA_DL.
*      DESCRIBE TABLE IT_ZSPPVP_BA LINES WA_BA.
*    ENDIF.
*  ENDLOOP.

ENDFORM.                    " READ_DLBA_FROM_VEHICLE


*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET TITLEBAR '9000'.
  IF it_zsppvp[] IS INITIAL.
    SET PF-STATUS 'MAIN' EXCLUDING 'ZTRAN'.
  ELSE.
    SET PF-STATUS 'MAIN'.
  ENDIF.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_ALV_GRID  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_alv_grid OUTPUT.
  IF p_datum IS INITIAL.
    SELECT SINGLE dates INTO p_datum
      FROM ztpp_common_vals
     WHERE jobs = c_job1
       AND key2 = c_key2   .
  ENDIF.

  IF gs_custom_container IS INITIAL.
*-----> CREATE OBJECT
*    CREATE OBJECT GS_APPLICATION.

    CREATE OBJECT gs_custom_container
        EXPORTING container_name = wa_container.

    CREATE OBJECT alv_grid
        EXPORTING i_parent = gs_custom_container.

    PERFORM  build_variant.
    PERFORM  build_layout.
    PERFORM  build_fieldcat.

*-----> SET OBJECT
    CALL METHOD alv_grid->set_table_for_first_display
      EXPORTING
*        I_BYPASSING_BUFFER            =
*        I_BUFFER_ACTIVE               =
*        I_CONSISTENCY_CHECK           =
        i_structure_name              = 'ZSPPVP_S'
        is_variant                    = gs_variant
        i_save                        = 'A'
*        I_DEFAULT                     = 'X'
        is_layout                     = gs_layout
*        IS_PRINT                      =
*        IT_SPECIAL_GROUPS             =
*        IT_TOOLBAR_EXCLUDING          =
*        IT_HYPERLINK                  =
*        IT_ALV_GRAPHICS               =
      CHANGING
        it_outtab                     = it_zsppvp[]
        it_fieldcatalog               = gt_fieldcat[]
*        IT_SORT                       =
*        IT_FILTER                     =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4
            .
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.
ENDMODULE.                 " CREATE_ALV_GRID  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  SET_CURSOR_FIELD  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_cursor_field OUTPUT.
  SET CURSOR FIELD wa_fname_tx LINE wa_saveline_ix.
ENDMODULE.                 " SET_CURSOR_FIELD  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  CASE ok_code.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  READ_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE read_data INPUT.
  PERFORM check_and_read_data.
ENDMODULE.                 " READ_DATA  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_CURSOR_FIELD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_cursor_field INPUT.
  CLEAR: wa_fname_tx, wa_saveline_ix.
  GET CURSOR FIELD wa_fname_tx LINE wa_saveline_ix.
ENDMODULE.                 " GET_CURSOR_FIELD  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  save_ok_code = ok_code.
  CLEAR ok_code.
  CASE save_ok_code.
    WHEN 'ZTRAN'.   "ALC TRANSFER
      " FCR: M.Y.Hur Request...  08/02/2004...
      PERFORM save_and_transfer.
    WHEN OTHERS.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT

*&---------------------------------------------------------------------*
*&      Form  BUILD_VARIANT
*&---------------------------------------------------------------------*
FORM build_variant.
  gs_variant-report = sy-repid.
ENDFORM.                    " BUILD_VARIANT

*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
FORM build_layout.
  gs_layout-zebra  = 'X'.       "ZEBRA
  gs_layout-cwidth_opt = 'X'.   "OPTIMIZE COLUMN WIDTH
  gs_layout-detailinit = 'X'.   "DISPLAY INITIAL VALUES ON DETAIL SCREEN

ENDFORM.                    " BUILD_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
FORM build_fieldcat.

  DATA: l_struct    LIKE dd02l-tabname.


  DATA: zero_fname1(20),
        zero_fname2(20),
        zero_cnt TYPE i.

  l_struct = 'ZSPPVP_S'.
  CLEAR : wa_fieldcat, gt_fieldcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            i_buffer_active        = 'X'
            i_structure_name       = l_struct
       CHANGING
            ct_fieldcat            = gt_fieldcat[]
       EXCEPTIONS
            inconsistent_interface = 1
            program_error          = 2
            OTHERS                 = 3.

  LOOP AT gt_fieldcat INTO wa_fieldcat.
    PERFORM set_field_info USING wa_fieldcat.
    MODIFY gt_fieldcat FROM wa_fieldcat.
    CLEAR wa_fieldcat.
  ENDLOOP.
ENDFORM.                    " BUILD_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_INFO
*&---------------------------------------------------------------------*
FORM set_field_info USING l_fieldcat STRUCTURE lvc_s_fcat.

  CASE l_fieldcat-fieldname.
    WHEN 'FLG'.
      set_fieldcat  'FLG' 3.
      l_fieldcat-key = 'X'.
    WHEN 'MODL'.
      set_fieldcat 'MODEL' 3.
      l_fieldcat-key = 'X'.
    WHEN 'VHNO'.
      set_fieldcat 'BODY NO.' 7.
      l_fieldcat-key = 'X'.
    WHEN 'ORDR'.
      set_fieldcat 'WORK ORDER' 9.
    WHEN 'DIST'.
      set_fieldcat 'DIST.' 5.
    WHEN 'INTC'.
*      SET_FIELDCAT 'INT'  3.
    WHEN 'EXTC'.
*      SET_FIELDCAT 'EXT'  3.
    WHEN 'VINN'.
      set_fieldcat 'VIN' 17.
    WHEN 'K01PNO'.
      set_fieldcat 'Imm. Pin' 8.
    WHEN 'PLNT'.
*      SET_FIELDCAT 'Plant' 2.
    WHEN 'LINE'.
*      SET_FIELDCAT 'Line'  2.
    WHEN 'SSR1'.
      set_fieldcat 'SEQ' 4.
    WHEN 'P_EMMISSION'.
      set_fieldcat 'Emission' 2.
    WHEN 'EVL1'.
      set_fieldcat 'VAL1' 4.
    WHEN 'EVL2'.
      set_fieldcat 'VAL2' 4.
    WHEN 'EVL3'.
      set_fieldcat 'VAL3' 4.
    WHEN 'EVL4'.
      set_fieldcat 'VAL4' 4.
    WHEN 'EVL5'.
      set_fieldcat 'VAL5' 4.
*   WHEN 'P_SEQ_DATE'.                " Sequence Date
*     SET_FIELDCAT 'SEQ Date' 10.
    WHEN 'P_ENGINE_NO'.
      set_fieldcat 'ENGINE ASSY ID' 15.
    WHEN 'P_KEY_NO'.
      set_fieldcat 'KEY' 6.
    WHEN 'P_TM_NO'.
      set_fieldcat 'KEY' 15.
    WHEN 'CDAT'.
      set_fieldcat 'CREATE DATE' 10.
    WHEN 'CTIM'.
      set_fieldcat 'CREATE TIME' 10.
    WHEN 'ZEDAT'.
      set_fieldcat 'I/F DATE' 10.
  ENDCASE.
ENDFORM.                    " SET_FIELD_INFO
*&---------------------------------------------------------------------*
*&      Form  CHECK_AND_READ_DATA
*&---------------------------------------------------------------------*
FORM check_and_read_data.
  IF p_ir EQ space AND p_rp EQ space
     AND p_dl EQ space AND p_ba EQ space.
    MESSAGE e001 WITH text-211.
  ELSE.
*   IF NOT R_DATUM-HIGH IS INITIAL.
    IF NOT p_datum      IS INITIAL.
*      IF R_DATUM-LOW GT R_DATUM-HIGH.
*        MESSAGE E001 WITH TEXT-212.
*      ELSE.
      PERFORM read_data.
    ENDIF.
*    ELSE.
*      PERFORM READ_DATA.
*    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_AND_READ_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
FORM read_data.
* CLEAR : R_DATUM[].
*  IF R_DATUM-HIGH IS INITIAL.
*    R_DATUM-SIGN = 'I'.
*    R_DATUM-OPTION = 'EQ'.
*    APPEND R_DATUM.
*  ELSE.
*    R_DATUM-SIGN = 'I'.
*    R_DATUM-OPTION = 'BT'.
*    APPEND R_DATUM.
*  ENDIF.
  PERFORM execute_process.

  CALL  METHOD gs_custom_container->free.
  FREE  gs_custom_container.

ENDFORM.                    " READ_DATA

*&---------------------------------------------------------------------*
*&      Form  SAVE_AND_TRANSFER
*&---------------------------------------------------------------------*
FORM save_and_transfer.
  DATA : l_msgtxt(100),
         l_linestext(10),
         l_lines      LIKE  sy-tabix.

*---> Interface date update
  it_zsppvp-zuser = sy-uname.
  it_zsppvp-zsdat = sy-datum.
  it_zsppvp-zstim = sy-uzeit.
  it_zsppvp-cdat  = sy-datum.
  it_zsppvp-ctim  = sy-uzeit.

  MODIFY it_zsppvp TRANSPORTING zuser zsdat zstim
                                WHERE mandt EQ sy-mandt.

  IF sy-subrc EQ 0.
    CALL FUNCTION 'Z_FPP_SET_ZTPPVP'
      DESTINATION c_dest
      EXPORTING
          ir             =  wa_ir  "IR QTTY
          rp             =  wa_rp  "RP QTTY
          dl             =  wa_dl  "DL QTTY
          ba             =  wa_ba  "BA QTTY
      TABLES
           i_zsppvp              = it_zsppvp
      EXCEPTIONS
           communication_failure = 1 MESSAGE l_msgtxt
           system_failure        = 2 MESSAGE l_msgtxt.
    IF sy-subrc NE 0.
      MESSAGE i001 WITH l_msgtxt.
    ELSE.
      it_zsppvp-zedat = sy-datum.
      it_zsppvp-zetim = sy-uzeit.
      MODIFY it_zsppvp TRANSPORTING zuser zsdat zstim
                                WHERE mandt EQ sy-mandt.
      CLEAR : it_ztppvp, it_ztppvp[], l_lines.
      LOOP AT it_zsppvp WHERE zzret EQ 'S'.
        MOVE-CORRESPONDING it_zsppvp TO it_ztppvp.
        APPEND it_ztppvp.
        l_lines = l_lines + 1.
        CLEAR it_zsppvp.
      ENDLOOP.
      DESCRIBE TABLE it_ztppvp LINES l_lines.
      WRITE l_lines TO l_linestext.
      MODIFY ztppvp FROM TABLE it_ztppvp.
      MESSAGE i002 WITH l_linestext
                       text-301.
      PERFORM save_common_vals .
    ENDIF.
  ENDIF.
ENDFORM.                    " SAVE_AND_TRANSFER
*&---------------------------------------------------------------------*
*&      Form  SELECT_DLBA_FROM_VEHICLE
*&---------------------------------------------------------------------*
FORM select_dlba_from_vehicle.
  CASE c_mark.
    WHEN p_dl.
      PERFORM select_dl_from_vehicle.
    WHEN p_ba.
      PERFORM select_ba_from_vehicle.
  ENDCASE.
ENDFORM.                    " SELECT_DLBA_FROM_VEHICLE
*&---------------------------------------------------------------------*
*&      Form  CLASSIFICATION_VALUE
*&---------------------------------------------------------------------*
FORM classification_value USING p_atnam
                          CHANGING p_atwrt.

  DATA : l_atinn    TYPE   cabn-atinn,
         l_atfor    TYPE   cabn-atfor,
         l_atwrt    TYPE   ausp-atwrt,
         l_atflv    TYPE   ausp-atflv,
         l_integer  TYPE   i.

  SELECT SINGLE atinn
                atfor
         INTO (l_atinn, l_atfor)
         FROM cabn
         WHERE atnam EQ p_atnam.

  CLEAR p_atwrt.
  SELECT SINGLE atwrt
                atflv
         INTO (l_atwrt, l_atflv)
         FROM ausp
         WHERE objek = wa_objek
           AND klart = '002'
           AND atinn = l_atinn.
  CASE l_atfor.
    WHEN 'CHAR'.
      p_atwrt = l_atwrt.
    WHEN OTHERS.
      l_integer = l_atflv.
      WRITE l_integer TO p_atwrt LEFT-JUSTIFIED NO-GROUPING.
  ENDCASE.
ENDFORM.                    " CLASSIFICATION_VALUE
*&---------------------------------------------------------------------*
*&      Form  APPEND_DL
*&---------------------------------------------------------------------*
FORM append_dl.
  wa_dl = wa_dl + 1.
  MOVE  sy-mandt TO      it_zsppvp_dl-mandt.
  MOVE   'DL'    TO      it_zsppvp_dl-flg.
  PERFORM classification_value USING 'P_MODEL'
                               CHANGING it_zsppvp_dl-modl.
  PERFORM classification_value USING 'P_BODY_SERIAL'
                               CHANGING it_zsppvp_dl-vhno.
  APPEND it_zsppvp_dl.
  MOVE-CORRESPONDING it_zsppvp_dl TO it_zsppvp.
  APPEND it_zsppvp.
  CLEAR : it_zsppvp_dl, it_zsppvp.
ENDFORM.                    " APPEND_DL

*&---------------------------------------------------------------------*
*&      Form  GET_VEHICLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_vehicle   USING pa_mitu  pa_type.
  " Reading Value: MODEL, VHNO(If it is MITU..), WORKORDER(ORDR,DIST)
  "                COLOR(EXT,INT), VIN, PLANT(BODY), LINE(BODY),
  "                SEQ_SERIAL, P_EPI_CODE(4 DIGIT - SEPARATED 1 DIGIT),
  "                SEQ_DATE  .
  DATA: l_datum     LIKE ausp-atwrt,
        l_datum_seq LIKE ausp-atflv,
        l_datum_char(10),
        l_atinn     LIKE ausp-atinn,
        l_mitu      LIKE ausp-atwrt,
        l_7jb       LIKE ztpp_pmt07jb_a,
        l_cnt       TYPE i             ,
        l_idx       TYPE n             ,
        l_date      TYPE d             ,
        l_vals      LIKE TABLE OF zspp_vin_value       WITH HEADER LINE,
        l_ausp      LIKE TABLE OF ausp                 WITH HEADER LINE.

  CONCATENATE p_datum '%'  INTO l_datum.
  l_datum_seq = l_datum_char = p_datum.

 CLEAR: it_pmt07jb_ir,  it_pmt07jb_ir[], it_pmt07jb_rp, it_pmt07jb_rp[].

  SELECT SINGLE atinn INTO l_atinn
    FROM cabn
*   WHERE ATNAM = 'P_VM_DATE' .
   WHERE atnam = 'P_SEQUENCE_DATE'.

  SELECT * INTO TABLE l_ausp
    FROM ausp
   WHERE atinn = l_atinn
     AND klart = '002'
     AND atflv = l_datum_seq.
*     AND ATWRT LIKE L_DATUM .

  DESCRIBE TABLE l_ausp  LINES l_cnt.
  IF l_cnt = 0.
    SELECT mandt equnr INTO TABLE l_ausp
      FROM equi
     WHERE erdat = p_datum
       AND eqtyp = 'V'     .
  ENDIF.

  LOOP AT l_ausp.
    CLEAR: l_vals, l_vals[].
    PERFORM append_char    TABLES l_vals .
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              object       = l_ausp-objek(18)
*             DISPLAY      = 'X'
         TABLES
              val_table    = l_vals
         EXCEPTIONS
              no_data      = 1
              error_mode   = 2
              error_object = 3
              OTHERS       = 4.

    " Data Assign to the IT_PMT07JB_*R(Internal Table)
    CLEAR: l_7jb, l_vals.
    READ TABLE l_vals INDEX 1 .  " WITH KEY ATNAM = 'P_MODEL'.
    l_7jb-modl = l_vals-atwrt .      CLEAR: l_vals.
    READ TABLE l_vals INDEX 2 .  " WITH KEY ATNAM = 'P_MITU' .
    l_7jb-mtgu = l_vals-atwrt   .    CLEAR: l_vals.
    READ TABLE l_vals INDEX 3 .  " WITH KEY ATNAM = 'P_WORK_ORDER'.
    l_7jb-ordr = l_vals-atwrt(9).    CLEAR: l_vals.
   READ TABLE l_vals INDEX 4 .  " WITH KEY ATNAM = 'P_DESTINATION_CODE'.
    l_7jb-dist = l_vals-atwrt   .    CLEAR: l_vals.
    READ TABLE l_vals INDEX 5 .  " WITH KEY ATNAM = 'P_EXT_COLOR'.
    l_7jb-extc = l_vals-atwrt   .    CLEAR: l_vals.
    READ TABLE l_vals INDEX 6 .  " WITH KEY ATNAM = 'P_INT_COLOR'.
    l_7jb-intc = l_vals-atwrt   .    CLEAR: l_vals.
    READ TABLE l_vals INDEX 7 .  " WITH KEY ATNAM = 'P_VIN'  .
    l_7jb-vinn = l_vals-atwrt   .    CLEAR: l_vals.
    READ TABLE l_vals INDEX 8 .  " WITH KEY ATNAM = 'P_BODY_PLANT_NO'.
    l_7jb-plnt = l_vals-atwrt   .    CLEAR: l_vals.
    READ TABLE l_vals INDEX 9 .  " WITH KEY ATNAM = 'P_BODY_LINE_NO'.
    l_7jb-line = l_vals-atwrt   .    CLEAR: l_vals.
    READ TABLE l_vals INDEX 10.  " WITH KEY ATNAM = 'P_SEQUENCE_SERIAL'.
    l_7jb-ssr1 = l_vals-atwrt   .    CLEAR: l_vals.
    READ TABLE l_vals INDEX 11.  " WITH KEY ATNAM = 'P_EPI_CODE'.
    l_cnt = strlen( l_vals-atwrt ) .
    DO l_cnt TIMES.
      l_idx = l_idx + 1.
      CASE l_idx.
        WHEN 1.
          l_7jb-evl1 = l_vals-atwrt(1)      .
        WHEN 2.
          l_7jb-evl2 = l_vals-atwrt+01(1)   .
        WHEN 3.
          l_7jb-evl3 = l_vals-atwrt+02(1)   .
        WHEN 4.
          l_7jb-evl4 = l_vals-atwrt+03(1)   .
      ENDCASE.
    ENDDO.
*  L_7JB-MODL = L_VALS-ATWRT(2).    CLEAR: L_VALS.
    READ TABLE l_vals INDEX 12.  " WITH KEY ATNAM = 'P_VM_DATE' .
    l_7jb-cdat = l_vals-atwrt(8).
    l_7jb-ctim = l_vals-atwrt+8(6).  CLEAR: l_vals.
    IF l_vals-atwrt = space.
      SELECT SINGLE erdat INTO l_date
        FROM equi
       WHERE equnr = l_ausp-objek(18) .
      l_7jb-sqdt   = l_date           .
    ENDIF.
    READ TABLE l_vals INDEX 13.  " WITH KEY ATNAM = 'P_MI'      .
    l_7jb-bmdl = l_vals-atwrt   .    CLEAR: l_vals.
    READ TABLE l_vals INDEX 14.  " WITH KEY ATNAM = 'P_OCN'     .
    l_7jb-ocnn = l_vals-atwrt   .    CLEAR: l_vals.
    READ TABLE l_vals INDEX 15.  " WITH KEY ATNAM = 'P_VERSION' .
    l_7jb-vers = l_vals-atwrt   .    CLEAR: l_vals.
    READ TABLE l_vals INDEX 16.  " WITH KEY ATNAM = 'P_MODEL_YEAR'.
    l_7jb-moye = l_vals-atwrt   .    CLEAR: l_vals.
    READ TABLE l_vals INDEX 17.  " WITH KEY ATNAM = 'P_SEQUENCE_DATE'.
    CALL FUNCTION 'CONVERSION_EXIT_MODAT_INPUT'
         EXPORTING
              input  = l_vals-atwrt(10)
         IMPORTING
              output = l_7jb-sqdt.
    CLEAR: l_vals.

    l_7jb-vhno = l_ausp-objek+3(6).  CLEAR: l_vals.
    READ TABLE l_vals INDEX 18.  " WITH KEY ATNAM = 'P_EMISSION' .
    IF l_vals-zflag IS INITIAL.
      l_7jb-gubb = l_vals-atwrt   .    CLEAR: l_vals.
    ENDIF.
    l_7jb-mandt = sy-mandt.

** Changed by Furong on 06/17/08
    READ TABLE l_vals INDEX 19.  " WITH KEY ATNAM = 'P_AIRBAG_NO16'.
    l_7jb-ernam = l_vals-atwrt   .    CLEAR: l_vals.
** End of change

    IF pa_type = 'IR'.
      MOVE-CORRESPONDING l_7jb  TO it_pmt07jb_ir .
      APPEND it_pmt07jb_ir.
    ENDIF.

    IF pa_type = 'RP'.
      MOVE-CORRESPONDING l_7jb  TO it_pmt07jb_rp .
      APPEND it_pmt07jb_rp.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " GET_VEHICLE

*&---------------------------------------------------------------------*
*&      Form  CALL_EMISSION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_emission.
  SUBMIT zrpp804r_vehicle_emission   AND RETURN
    WITH p_vdate = p_datum                      .
ENDFORM.                    " CALL_EMISSION

*&---------------------------------------------------------------------*
*&      Form  SAVE_COMMON_VALS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_common_vals.
  DATA: lw_vals       LIKE ztpp_common_vals.

  CLEAR: lw_vals.
  SELECT SINGLE * INTO CORRESPONDING FIELDS OF lw_vals
    FROM ztpp_common_vals
   WHERE jobs = c_job1
     AND key2 = c_key2.

  IF sy-subrc = 0.
    lw_vals-dates = p_datum.
    MODIFY ztpp_common_vals FROM lw_vals.
  ELSE.
    lw_vals-jobs  = c_job1 .
    lw_vals-key2  = c_key2 .
    lw_vals-dates = p_datum.
    INSERT INTO ztpp_common_vals VALUES lw_vals.
  ENDIF.
ENDFORM.                    " SAVE_COMMON_VALS
*&---------------------------------------------------------------------*
*&      Form  APPEND_CHAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_VALS  text
*----------------------------------------------------------------------*
FORM append_char TABLES   l_vals STRUCTURE zspp_vin_value .
  l_vals-atnam = 'P_MODEL'.              APPEND l_vals.
  l_vals-atnam = 'P_MITU' .              APPEND l_vals.
  l_vals-atnam = 'P_WORK_ORDER'.         APPEND l_vals.
  l_vals-atnam = 'P_DESTINATION_CODE'.   APPEND l_vals.
  l_vals-atnam = 'P_EXT_COLOR'.          APPEND l_vals.
  l_vals-atnam = 'P_INT_COLOR'.          APPEND l_vals.
  l_vals-atnam = 'P_VIN'  .              APPEND l_vals.
  l_vals-atnam = 'P_BODY_PLANT_NO'.      APPEND l_vals.
  l_vals-atnam = 'P_BODY_LINE_NO'.       APPEND l_vals.
  l_vals-atnam = 'P_SEQUENCE_SERIAL'.    APPEND l_vals.
  l_vals-atnam = 'P_EPI_CODE'.           APPEND l_vals.
  l_vals-atnam = 'P_VM_DATE' .           APPEND l_vals.
  l_vals-atnam = 'P_MI'      .           APPEND l_vals.
  l_vals-atnam = 'P_OCN'     .           APPEND l_vals.
  l_vals-atnam = 'P_VERSION' .           APPEND l_vals.
  l_vals-atnam = 'P_MODEL_YEAR'.         APPEND l_vals.
  l_vals-atnam = 'P_SEQUENCE_DATE'.      APPEND l_vals.
  l_vals-atnam = 'P_EMISSION'     .      APPEND l_vals.
** Changed by Furong on 06/17/08
  l_vals-atnam = 'P_AIRBAG_NO16'  .      APPEND l_vals.
** End of change
ENDFORM.                    " APPEND_CHAR
*&---------------------------------------------------------------------*
*&      Form  GET_PIN_CODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_pin_code.

  DATA: lt_zsppvp LIKE TABLE OF it_zsppvp WITH HEADER LINE.
  DATA: l_equnr LIKE equi-equnr.
  DATA: l_ran_int LIKE qf00-ran_int,
        l_ran_6(6) TYPE n,
        it_vmaster LIKE TABLE OF zspp_vin_value WITH HEADER LINE.

  LOOP AT it_zsppvp.
    IF it_zsppvp-k01pno IS INITIAL.
      CLEAR: l_ran_int.

      CONCATENATE it_zsppvp-modl it_zsppvp-vhno INTO l_equnr.
      CALL FUNCTION 'QF05_RANDOM_INTEGER'
           EXPORTING
                ran_int_max   = 999999
                ran_int_min   = 000001
           IMPORTING
                ran_int       = l_ran_int
           EXCEPTIONS
                invalid_input = 1
                OTHERS        = 2.
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      l_ran_6 = l_ran_int.
      CLEAR it_vmaster.
      REFRESH:it_vmaster.
      it_vmaster-atnam = 'P_AIRBAG_NO16'.
      it_vmaster-atwrt = l_ran_6.
      APPEND it_vmaster.

      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
           EXPORTING
                object     = l_equnr
                mode       = 'W'
                cmode      = '002'
           TABLES
                val_table  = it_vmaster
           EXCEPTIONS
                no_data    = 1
                error_mode = 2
                OTHERS     = 3.

      IF sy-subrc NE 0.
        MESSAGE i001 WITH 'Error: Updating PIN Code'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' .
        it_zsppvp-k01pno = l_ran_6.
        MODIFY it_zsppvp TRANSPORTING k01pno.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_PIN_CODE
*&---------------------------------------------------------------------*
*&      Form  check_data_exist
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_data_exist.

  SELECT SINGLE * FROM ztppvp
  WHERE sqdt = p_datum.
  IF sy-subrc = 0.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
       EXPORTING
     text_question  = 'Sequence Data existed. Do you want to process?'
       IMPORTING
           answer         = w_answer
       EXCEPTIONS
            text_not_found = 1
           OTHERS         = 2.

  ELSE.
    w_answer = '1'.
  ENDIF.
ENDFORM.                    " check_data_exist

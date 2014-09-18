*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_TOP                                        *
*----------------------------------------------------------------------*

**-------------------------------------------------------------------*
**  TABLE DEFINE
**-------------------------------------------------------------------*
TABLES : ztpp_wosum, ausp, cabn, edp13, ztsd_um, ztpp_vm.
**-------------------------------------------------------------------*
**  TYPE DEFINE
**-------------------------------------------------------------------*


**-------------------------------------------------------------------*
**  DATA DEFINE
**-------------------------------------------------------------------*
*
DATA : BEGIN OF gt_data OCCURS 0 ,
    wo_serial  LIKE ztsd_um-wo_serial  ,
    wo_nation  LIKE ztsd_um-wo_nation  ,
    wo_dealer  LIKE ztsd_um-wo_dealer  ,
    wo_extc    LIKE ztsd_um-wo_extc    ,
    wo_intc    LIKE ztsd_um-wo_intc    ,
    fsc        LIKE ztpp_wosum-fsc     ,
    intno      LIKE ztsd_um-intno      ,
    zvin       LIKE ztsd_um-zvin       ,
    pack(6),
    vin        LIKE ztpp_vm-vin,
    eng_no     LIKE ztpp_vm-eng_no,
    key_no     LIKE ztpp_vm-key_no,
    esn_no     LIKE ztpp_vm-esn_no,
    model_code LIKE ztsd_um-model_code ,
    body_no    LIKE ztsd_um-body_no    ,
    rp_cstatus LIKE ztpp_vm-rp_cstatus,
** Furong on 06/11/12
    hacc_extc  LIKE ztsd_um-wo_extc    ,
    hacc_intc  LIKE ztsd_um-wo_intc    ,
** End
    zvin_status(4) .
DATA : END OF gt_data,
       gt_ksbohmm LIKE TABLE OF ztpp_ksbohmm_if WITH HEADER LINE.

*&---------------------------------------------------------------------*
*& VARIABLES
*&---------------------------------------------------------------------*

DATA: ok_code LIKE sy-ucomm.

DATA : gv_repid LIKE sy-repid ,
       gv_new(1),
       gv_docnum LIKE edidc-docnum.

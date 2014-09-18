*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_TOP                                        *
*----------------------------------------------------------------------*

**-------------------------------------------------------------------*
**  TABLE DEFINE
**-------------------------------------------------------------------*
TABLES : ztsd_um, mara,
         ztsd_spec,
         ztpp_rpid,
         zvpp_vm,
         ztpp_vm,
         ztpp_wosum,
         ztpp_input_osr,
         ztpp_pmt07jb_a,
         ztsd_osr_log,
         ztsd_osr,
         ztsd_ale_dest.
*         ZTPP_VEHICLE_PLN,
*         ZTPP_INPUTPL_OSR,
*         ZTSD_KSBOPRC, ZTSD_GLOVIS_LOAD, ZTSD_LCCON,
*         ZTSD_OSR_UMG_L, ZTSD_OSR_SUMK_L.

**-------------------------------------------------------------------*
**  DATA DEFINE
**-------------------------------------------------------------------*
TYPES : BEGIN OF t_osr.
TYPES :   wo_nation  LIKE ztsd_um-wo_nation,
          wo_dealer  LIKE ztsd_um-wo_dealer.
        INCLUDE STRUCTURE zosrseg.
*-< added by Victor 05/18/2010
*TYPES : wo_nation  LIKE ztsd_um-wo_nation,
*       wo_dealer  LIKE ztsd_um-wo_dealer,
TYPES :  intno      LIKE ztsd_um-intno,
         body_no    LIKE ztsd_um-body_no,
         plan_date  LIKE ztsd_um-plan_date,
         urgency    LIKE ztsd_um-urgency,
         urgseq     LIKE ztsd_um-urgency,
         urgcdate   LIKE ztsd_um-urgcdate,
         dealer_dt  LIKE ztsd_um-dealer_dt,
         dealer_yn(1),                       "Dealer Exist? : X, ''
       status     LIKE ztsd_um-status,
       lpdd_d(1).
*->
TYPES : END OF t_osr.
*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*

*# View
DATA : BEGIN OF gt_item OCCURS 0 .
        INCLUDE STRUCTURE zosrseg.

DATA :  dt01_a LIKE sy-datum,
        dt06_a LIKE sy-datum,
        dt07_a LIKE sy-datum,
        dt08_a LIKE sy-datum,
        dt09_a LIKE sy-datum,
        dt10_a LIKE sy-datum,
        dt11_a LIKE sy-datum,
        dt12_a LIKE sy-datum,
        dt13_a LIKE sy-datum,
        dt14_a LIKE sy-datum,
        dt15_a LIKE sy-datum,
        crdt_a LIKE sy-datum,
        ipdd_a LIKE sy-datum,
        lpdd_a LIKE sy-datum,
        chdt_a LIKE sy-datum,
        status LIKE icon-id,

        END OF gt_item.

*# Processing
DATA : BEGIN OF it_input_osr OCCURS 0.
        INCLUDE STRUCTURE ztpp_input_osr.
DATA : conf(1).   "reservation :X
DATA : END OF   it_input_osr.

DATA : it_um     LIKE TABLE OF ztsd_um    WITH HEADER LINE,
*       it_osr_if LIKE TABLE OF zosrseg   WITH HEADER LINE,
       it_body   LIKE TABLE OF it_input_osr WITH HEADER LINE,
       it_body_n LIKE TABLE OF it_input_osr WITH HEADER LINE.

*       IT_HDUM   LIKE TABLE OF ZTPP_INPUT_OSR WITH HEADER LINE,
*       IT_VM     LIKE TABLE OF ZTPP_VM    WITH HEADER LINE.

DATA : BEGIN OF it_hdum OCCURS 0 , " > IT_COL_UM
         wohd      LIKE mara-matnr,
         wocl      LIKE mara-matnr,
         wo_serial LIKE ztsd_um-wo_serial,
         wo_nation LIKE ztsd_um-wo_nation,
         wo_dealer LIKE ztsd_um-wo_dealer,
         wo_extc   LIKE ztsd_um-wo_extc,
         wo_intc   LIKE ztsd_um-wo_intc,
         intno     LIKE ztsd_um-intno,
       END OF it_hdum.


DATA : it_osr TYPE TABLE OF t_osr WITH HEADER LINE,
       it_osr_total TYPE TABLE OF t_osr WITH HEADER LINE.

DATA : lt_b28   TYPE TABLE  OF t_osr WITH HEADER LINE, "06.17.2014
       lt_oth_natn TYPE TABLE OF t_osr  WITH HEADER LINE.

*DATA : it_osr_if_hma TYPE TABLE OF t_osr WITH  HEADER LINE,
*       it_osr_if_hac TYPE TABLE OF t_osr WITH  HEADER LINE,
DATA : it_osr_if     TYPE TABLE OF t_osr WITH  HEADER LINE,
       it_osr_if_glv TYPE TABLE OF zosrseg WITH  HEADER LINE.

DATA:  it_ztsd_osr_log TYPE TABLE OF ztsd_osr_log WITH  HEADER LINE.
DATA : BEGIN OF g_cond OCCURS 0,
              text(72),
       END OF g_cond.

DATA : lv_model(3),
       lv_model_year LIKE ztpp_vm-model_year,
       lv_objek      LIKE ausp-objek.
*
*DATA : BEGIN OF IT_OSR OCCURS 0.
*        INCLUDE STRUCTURE ZOSRSEG.
**-< added by Victor 05/18/2010
*DATA : WO_NATION  LIKE ZTSD_UM-WO_NATION,
*       WO_DEALER  LIKE ZTSD_UM-WO_DEALER,
*       INTNO      LIKE ZTSD_UM-INTNO,
*       BODY_NO    LIKE ZTSD_UM-BODY_NO,
*       PLAN_DATE  LIKE ZTSD_UM-PLAN_DATE,
*       URGENCY    LIKE ZTSD_UM-URGENCY,
*       URGSEQ     LIKE ZTSD_UM-URGENCY,
*       URGCDATE   LIKE ZTSD_UM-URGCDATE,
*       DEALER_DT  LIKE ZTSD_UM-DEALER_DT,
*       STATUS     LIKE ZTSD_UM-STATUS.
**->
*DATA : END OF IT_OSR.

DATA : it_ale_dest LIKE ztsd_ale_dest OCCURS 0 WITH HEADER LINE.

*-     Partial sending 09.20.2010
*DATA : it_osr_partial_h LIKE it_osr_if OCCURS 0 WITH HEADER LINE. "Hyun
*DATA : g_index LIKE sy-index.
*DATA : g_company(3).              "   KMA, HMA, KCI, HAC
*DATA : BEGIN OF IT_COL_UM OCCURS 0,
*  WO_SERIAL LIKE ZTSD_UM-WO_SERIAL,
*  WO_NATION LIKE ZTSD_UM-WO_NATION,
*  WO_DEALER LIKE ZTSD_UM-WO_DEALER,
*  WO_EXTC LIKE ZTSD_UM-WO_EXTC,
*  WO_INTC LIKE ZTSD_UM-WO_INTC,
**** only changed data
*  INTNO LIKE ZTSD_UM-INTNO,
***
*END OF IT_COL_UM.


*DATA : IT_OSR_MAIN LIKE ZTSD_OSR_MAIN OCCURS 0 WITH HEADER LINE.



**-added by Victor
*DATA :IT_VEHICLE_PLN LIKE ZTPP_VEHICLE_PLN OCCURS 0 WITH HEADER LINE.
*DATA : GT_DAY       LIKE ZSPP0098 OCCURS 0 WITH HEADER LINE.
*DATA : GT_DAY_TEMP  LIKE ZSPP0098 OCCURS 0 WITH HEADER LINE.
*DATA : BEGIN OF GT_DAY_SEQ  OCCURS 0.
*        INCLUDE STRUCTURE ZSPP0098.
*DATA : CONF(1).   "reservation :X
*DATA : END OF GT_DAY_SEQ.

*DATA : BEGIN OF it_count OCCURS 0,
*  ordr LIKE ztpp_pmt07jb_a-ordr,
*  dist LIKE ztpp_pmt07jb_a-dist,
*  extc LIKE ztpp_pmt07jb_a-extc,
*  intc LIKE ztpp_pmt07jb_a-intc,
*END OF it_count.

*DATA : BEGIN OF IT_VM OCCURS 0,
*        MODEL_CODE  LIKE ZVPP_VM-MODEL_CODE,
*        BODY_NO     LIKE ZVPP_VM-BODY_NO,
*        VIN         LIKE ZVPP_VM-VIN,
*        ENG_NO      LIKE ZVPP_VM-ENG_NO,
*        KEY_NO      LIKE ZVPP_VM-KEY_NO,
*        SEQ_DATE    LIKE ZVPP_VM-SEQ_DATE,
*        RP_CSTATUS  LIKE ZVPP_VM-RP_CSTATUS,
*        RP01_SDATE  LIKE ZVPP_VM-RP01_SDATE,
*        RP02_SDATE  LIKE ZVPP_VM-RP02_SDATE,
*        RP03_SDATE  LIKE ZVPP_VM-RP03_SDATE,
*        RP04_SDATE  LIKE ZVPP_VM-RP04_SDATE,
*        RP05_SDATE  LIKE ZVPP_VM-RP05_SDATE,
*        RP06_SDATE  LIKE ZVPP_VM-RP06_SDATE,
*        RP07_SDATE  LIKE ZVPP_VM-RP07_SDATE,
*        RP08_SDATE  LIKE ZVPP_VM-RP08_SDATE,
*        RP09_SDATE  LIKE ZVPP_VM-RP09_SDATE,
*        RP10_SDATE  LIKE ZVPP_VM-RP10_SDATE,
*        RP11_SDATE  LIKE ZVPP_VM-RP11_SDATE,
*        RP12_SDATE  LIKE ZVPP_VM-RP12_SDATE,
*        RP13_SDATE  LIKE ZVPP_VM-RP13_SDATE,
*        RP14_SDATE  LIKE ZVPP_VM-RP14_SDATE,
*        RP15_SDATE  LIKE ZVPP_VM-RP15_SDATE,
*        RP16_SDATE  LIKE ZVPP_VM-RP16_SDATE,
*        RP17_SDATE  LIKE ZVPP_VM-RP17_SDATE,
*        RP18_SDATE  LIKE ZVPP_VM-RP18_SDATE,
*        RP19_SDATE  LIKE ZVPP_VM-RP19_SDATE,
*        RP20_SDATE  LIKE ZVPP_VM-RP20_SDATE,
*        RP21_SDATE  LIKE ZVPP_VM-RP21_SDATE,
*        RP22_SDATE  LIKE ZVPP_VM-RP22_SDATE,
*        RP23_SDATE  LIKE ZVPP_VM-RP23_SDATE,
*        RP24_SDATE  LIKE ZVPP_VM-RP24_SDATE,
*        RP25_SDATE  LIKE ZVPP_VM-RP25_SDATE,
*        DEST_CODE   LIKE ZVPP_VM-DEST_CODE,
*        WO_SERIAL   LIKE ZVPP_VM-WO_SERIAL,
*        WO_NATION   LIKE ZVPP_VM-WO_NATION,
*        WO_DEALER   LIKE ZVPP_VM-WO_DEALER,
*        EXTC        LIKE ZVPP_VM-EXTC,
*        INTC        LIKE ZVPP_VM-INTC,
*  END OF IT_VM.

*DATA: ST_CRITICAL_RP LIKE ZSPP1010.

*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
DATA: wa_error        TYPE c               ,
      wa_dest(20)     TYPE c               ,
      i_dat_1         LIKE sy-datum,
      i_dat_3         LIKE sy-datum,
      i_dat_0         LIKE sy-datum,
      l_wo_serial     LIKE ztsd_um-wo_serial,
      l_wo_nation     LIKE ztsd_um-wo_nation,
      l_wo_dealer     LIKE ztsd_um-wo_dealer,
      l_wo_extc       LIKE ztsd_um-wo_extc,
      l_wo_intc       LIKE ztsd_um-wo_intc,
      gv_model         LIKE zvpp_vm-model_code,
      gv_body          LIKE zvpp_vm-body_no,
      gv_cstatus       LIKE zvpp_vm-rp_cstatus,
*      s_rp(26) VALUE '01,03,08,17,19,20,  ,21,22',
      s_rp(26) VALUE '01,04,09,18,21,22,  ,23,24',
*      C_INIT_QTY      LIKE ZSSD_OSR_SUM-INIT_QTY,
*      C_MOD_QTY       LIKE ZSSD_OSR_SUM-MOD_QTY,
*      ZHR             LIKE ZSSD_OSR_UMG-CRDT,
      w_dsn(90).

DATA: l_msgtxt(100),
      l_size      TYPE i,        " Total Count
      l_idx       TYPE num9,     " Current record point.
      l_start     TYPE c   ,
      l_end       TYPE c   ,
      lt_osr  LIKE TABLE OF it_osr WITH HEADER LINE.

DATA: result TYPE n.
*DATA: START_RESULT TYPE C LENGTH 1.
DATA: total_size TYPE i.

DATA : g_subrc         LIKE sy-subrc,
       v_sout_month(6) TYPE n.

FIELD-SYMBOLS: <fs> TYPE any.
FIELD-SYMBOLS: <rp> TYPE any.

*----------------------------------------------------------------------*
*  CONSTANS DECLARATION
*----------------------------------------------------------------------*
CONSTANTS: c_size     TYPE i VALUE 500,
           c_zosrseg LIKE edidd-segnam VALUE 'ZOSRSEG',
            c_mestyp LIKE edidc-mestyp VALUE 'ZOSR_MST',
            "Message Type
            c_rcvprt LIKE edidc-rcvprt VALUE 'LS',
            "Partner type of receiver
            c_logsys LIKE edidc-rcvprn VALUE 'NDECLNT850',
            c_rcvpor LIKE edidc-rcvpor VALUE 'A000000014',
            c_sndprn LIKE edidc-sndprn VALUE 'UD1300',
            c_sndprt LIKE edidc-sndprt VALUE 'LS',
            c_idoctp LIKE edidc-idoctp VALUE 'ZOSR'.
*&---------------------------------------------------------------------*
*& VARIABLES
*&---------------------------------------------------------------------*

DATA: ok_code LIKE sy-ucomm.

DATA : gv_repid LIKE sy-repid , gv_docnum LIKE edidc-docnum,
       gv_new(1).

DATA : w_del_date TYPE sy-datum. "for delete log

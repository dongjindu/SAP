*----------------------------------------------------------------------*
*   INCLUDE ZEMMGM13_VENDOR_STO_TOP                                    *
*----------------------------------------------------------------------*
TYPE-POOLS : slis, sp01r.

*** table
TABLES : ztmm_vendor_sto,
         ztca_if_log.

* itab
DATA   : it_ztmm_vendor_sto  LIKE TABLE OF ztmm_vendor_sto
                                  WITH HEADER LINE,
         wa_ztmm_vendor_sto  LIKE it_ztmm_vendor_sto,

         it_goodsmvt_item    LIKE TABLE OF bapi2017_gm_item_create
                                  WITH HEADER LINE,
         wa_goodsmvt_item    LIKE it_goodsmvt_item,

         wa_goodsmvt_header  LIKE bapi2017_gm_head_01,

         wa_goodsmvt_code    LIKE bapi2017_gm_code,

         wa_goodsmvt_headret LIKE bapi2017_gm_head_ret,

         it_bapiret2         LIKE TABLE OF bapiret2
                                  WITH HEADER LINE,
         wa_bapiret2         LIKE it_bapiret2,

         wa_ztca_if_log      LIKE ztca_if_log,

         it_ta_zsmm_6014_01  LIKE TABLE OF zsmm_6014_01
                                  WITH HEADER LINE.


*** selection screen
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS    :    p_count  LIKE  ztmm_vendor_sto-jobcount.
SELECT-OPTIONS:    s_zsdat  FOR   ztmm_vendor_sto-zsdat.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-005.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETERS       r1 RADIOBUTTON GROUP radi DEFAULT 'X'.
SELECTION-SCREEN POSITION 7.
SELECTION-SCREEN COMMENT 8(10) text-002 FOR FIELD r1.
SELECTION-SCREEN POSITION 25.
PARAMETERS       r2 RADIOBUTTON GROUP radi.
SELECTION-SCREEN POSITION 26.
SELECTION-SCREEN COMMENT 27(25) text-003 FOR FIELD r1.
SELECTION-SCREEN POSITION 55.
PARAMETERS       r3 RADIOBUTTON GROUP radi.
SELECTION-SCREEN POSITION 56.
SELECTION-SCREEN COMMENT 57(10) text-004 FOR FIELD r1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.
*PARAMETERS : P_MODE LIKE CTU_PARAMS-DISMODE DEFAULT 'N'.
*PARAMETERS : P_ASSYID LIKE ZTPPER-EASSYID.
*SELECT-OPTIONS: S_PDATE FOR ZTPPEP-PDATE OBLIGATORY MODIF ID AA.
**                                         NO-EXTENSION
**                                         NO INTERVALS

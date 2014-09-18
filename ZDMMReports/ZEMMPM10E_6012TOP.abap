*----------------------------------------------------------------------*
*   INCLUDE ZEMMGM12E_6006TOP                                          *
*----------------------------------------------------------------------*
* For Return code
DATA: w_subrc LIKE sy-subrc.

* For BDC message
DATA: it_bdcmsgcoll LIKE TABLE OF bdcmsgcoll.
DATA: wa_bdcmsgcoll LIKE LINE OF it_bdcmsgcoll.

* Variable for date & time
DATA: w_present_date   TYPE sy-datum.
DATA: w_present_time   TYPE sy-uzeit.
DATA: w_cal_time       TYPE t.  "Calculated time from minutes
CONSTANTS: c_onehour   TYPE t VALUE '010000'.


* Itab & WA for zvmm_6012_02 (PLAF+RESB+ZTMM_MAST)
DATA:          wa_zvmm_6012_02   LIKE          zvmm_6012_02.
FIELD-SYMBOLS: <fs_zvmm_6012_02> LIKE          wa_zvmm_6012_02.
DATA:          it_zvmm_6012_02   LIKE TABLE OF wa_zvmm_6012_02.


* Itab & WA for Create TO(/nLT01)
DATA: BEGIN OF wa_toline,
       matnr        LIKE ztmm_mast-matnr,
       bdmng        LIKE resb-bdmng,
       meins        LIKE resb-meins,
       feedr        LIKE ztmm_mast-feedr,
       works        LIKE ztmm_mast-works,
       rh_lh        LIKE ztmm_mast-rh_lh,
       sdate        TYPE d,        "Start Date
       feeding_time TYPE sy-uzeit, "Start Time
       edate        TYPE d,        "End Date
       etime        TYPE t,        "End Time
       stock_check  LIKE ztmm_mast-stock_check,
       feed_cycle   LIKE ztmm_mast-feed_cycle,
       ztime        LIKE ztmm_mast-ztime, "Time for STL
       verme        LIKE lqua-verme,      "Available stock
       lpmin        LIKE mlgt-lpmin,      "Min. Storage bin qty
       open_to      LIKE ltap-vsola,      "Open TO
       bferrorqty   LIKE affw-erfmg,      "Backkflush Error Qty
       rdmng        LIKE mlgt-rdmng,      "Rounding Qty
       src_lgtyp    like mlgt-lgtyp,      "Source Storage type
       src_lgpla    like mlgt-lgpla,      "Source Storage bin
       des_lgtyp    like pkhd-lgtyp,      "Destination Storage type
       des_lgpla    like pkhd-lgpla,      "Destination Storage bin
       tqty         LIKE resb-bdmng,      "Target Qty
      END OF wa_toline.
DATA: it_toline LIKE TABLE OF wa_toline.
FIELD-SYMBOLS: <fs_toline> LIKE LINE OF it_toline.

* Odd Even Flag
DATA: w_odd_even TYPE i.

**** Constants&Vars for Number range object ****************************
CONSTANTS: c_nro_nr_00   VALUE '00' LIKE inri-nrrangenr. "Header Part
CONSTANTS: c_nro_nr_01   VALUE '01' LIKE inri-nrrangenr. "Item Part
CONSTANTS: c_nro_nr_09   VALUE '09' LIKE inri-nrrangenr. "App. Doc no.
* Number range object
DATA:      w_nro_object  VALUE 'ZMMNRO0002'
                               LIKE inri-object. "NRO for ZTLOG
* Number_Get_Next
DATA:      w_nro_number  TYPE num10.      " Same type of w_nro_object

DATA: w_zdocno TYPE num10.       "App. Doc. No.

***Selection Screen*****************************************************
TABLES: zvmm_6012_02.    "EMMGM12 PLAF+RESB+ZTMM_MAST

SELECTION-SCREEN BEGIN OF BLOCK bl_1 WITH FRAME TITLE text-bl1.
SELECT-OPTIONS: s_plnum FOR zvmm_6012_02-plnum  "Planned order number
                        OBLIGATORY.
*                        DEFAULT '0000390048' TO '0000516700'.
SELECTION-SCREEN END OF BLOCK bl_1.

*----------------------------------------------------------------------*
*   INCLUDE ZEMMGM12E_6006TOP                                          *
*----------------------------------------------------------------------*
*/ Begin of For test
DATA: BEGIN OF wa_rp06,
       rp06 LIKE ztpp_dvrt1-rp06,
      END OF wa_rp06.
DATA: it_rp06 LIKE TABLE OF wa_rp06.

* For Returned Value from Search Help
DATA BEGIN OF wa_ddshretval.
        INCLUDE STRUCTURE ddshretval.
DATA END OF wa_ddshretval.
DATA it_ddshretval LIKE TABLE OF wa_ddshretval.


**** Dynamic Where Clause
DATA: w_where_condition(72).
DATA: it_where_condition LIKE TABLE OF w_where_condition.


*/ End of For test




* For Return code
DATA: w_subrc LIKE sy-subrc.

* For BDC message
DATA: it_bdcmsgcoll LIKE TABLE OF bdcmsgcoll.
DATA: wa_bdcmsgcoll LIKE LINE OF it_bdcmsgcoll.

* Variable for date & time
DATA: w_current_date   TYPE sy-datum.
DATA: w_current_time   TYPE sy-uzeit.
DATA: w_screen_date    TYPE sy-datum.
DATA: w_screen_time    TYPE sy-uzeit.
data: w_dummy_date     type sy-datum.  "Dummy date for temp use
data: w_dummy_minutes  type num03.   "Dummy minutes for Cal of Start D&T

*DATA: w_cal_time       TYPE t.  "Calculated time from minutes
CONSTANTS: c_onehour   TYPE t VALUE '010000'.

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
       src_lgtyp    LIKE mlgt-lgtyp,      "Source Storage type
       src_lgpla    LIKE mlgt-lgpla,      "Source Storage bin
       des_lgtyp    LIKE pkhd-lgtyp,      "Destination Storage type
       des_lgpla    LIKE pkhd-lgpla,      "Destination Storage bin
       tqty         LIKE resb-bdmng,      "Target Qty
      END OF wa_toline.
DATA: it_toline LIKE SORTED TABLE OF wa_toline
                WITH UNIQUE KEY matnr.
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


**** Selection Screen **************************************************
SELECTION-SCREEN BEGIN OF BLOCK bl_1 WITH FRAME TITLE text-bl1.
*PARAMETER: p_cdate LIKE sy-datum  "Current date
*                   OBLIGATORY.
*PARAMETER: p_ctime LIKE sy-uzeit  "Current time
*                   OBLIGATORY.
PARAMETER: p_rp06  LIKE ztpp_dvrt1-rp06  "For Test
                   OBLIGATORY.
SELECTION-SCREEN END OF BLOCK bl_1.

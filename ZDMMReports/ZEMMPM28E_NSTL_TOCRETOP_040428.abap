*----------------------------------------------------------------------*
*   INCLUDE ZEMMPM28E_NSTL_TOCRETOP                                    *
*----------------------------------------------------------------------*
**** Constants&Vars for Number range object ****************************
CONSTANTS: c_nro_nr_00   VALUE '00' LIKE inri-nrrangenr. "Header Part
CONSTANTS: c_nro_nr_01   VALUE '01' LIKE inri-nrrangenr. "Item Part
CONSTANTS: c_nro_nr_09   VALUE '09' LIKE inri-nrrangenr. "App. Doc no.
* Number range object
DATA:      w_nro_object  VALUE 'ZMMNRO0002'
                               LIKE inri-object. "NRO for ZBDCMSGCOLL
* Number_Get_Next
DATA:      w_nro_number  TYPE num10.      " Same type of nro_object

DATA: w_zdocno TYPE num10.       "App. Doc. No.
************************************************************************
* For BDC message
DATA: it_bdcmsgcoll LIKE TABLE OF bdcmsgcoll.
DATA: wa_bdcmsgcoll LIKE LINE OF it_bdcmsgcoll.

* For Return code
DATA: w_subrc LIKE sy-subrc.

**** Itab & WA for Non Supply to Line
DATA: BEGIN OF wa_matnr_date_time,
       matnr      LIKE ztmm_nstl-matnr,  "Material
       sdate      TYPE d,                "Start Date
       stime      TYPE t,                "Start Time
       edate      TYPE d,                "End Date
       etime      TYPE t,                "End Time
       qty        LIKE resb-bdmng,       "Quantity
       unit       LIKE resb-meins,       "Unit
       rdmng      LIKE mlgt-rdmng,       "Rounding Qunatity
       tqty       LIKE resb-bdmng,       "Target Quantity
       src_lgtyp  LIKE mlgt-lgtyp,       "Source Storage type
       src_lgpla  LIKE mlgt-lgpla,       "Source Storage bin
       des_lgtyp  LIKE pkhd-lgtyp,       "Destination Storage type
       des_lgpla  LIKE pkhd-lgpla,       "Destination Storage bin
       feedr      LIKE ztmm_mast-feedr,  "Feeder
       feed_cycle LIKE ztmm_mast-feed_cycle,"Feed Cycle
      END OF wa_matnr_date_time.
DATA: it_matnr_date_time LIKE TABLE OF wa_matnr_date_time.
FIELD-SYMBOLS: <fs_matnr_date_time> LIKE LINE OF it_matnr_date_time.


DATA: it_data_for_to LIKE it_matnr_date_time.
DATA: wa_data_for_to LIKE LINE OF it_data_for_to.
FIELD-SYMBOLS: <fs_data_for_to> LIKE LINE OF it_data_for_to.

**** Itab & WA for ZTMM_NSTL
*DATA: wa_ztmm_nstl LIKE ztmm_nstl.

DATA: BEGIN OF wa_ztmm_nstl.
        INCLUDE STRUCTURE ztmm_nstl.
DATA:  feedr      LIKE ztmm_mast-feedr,   "Feeder
       feed_cycle LIKE ztmm_mast-feed_cycle,"Feed Cycle
      END OF wa_ztmm_nstl.

DATA: it_ztmm_nstl LIKE TABLE OF wa_ztmm_nstl.
FIELD-SYMBOLS: <fs_ztmm_nstl> LIKE LINE OF it_ztmm_nstl.

*/ Selection Screen
SELECTION-SCREEN BEGIN OF BLOCK bl_1 WITH FRAME TITLE text-bl1.
PARAMETERS : p_tocred LIKE ztmm_nstl-datum
                  OBLIGATORY. "NSTL TO Creation Date
SELECTION-SCREEN END OF BLOCK bl_1.

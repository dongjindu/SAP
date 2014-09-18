*----------------------------------------------------------------------*
*   INCLUDE ZEMMPM28E_NSTL_TOCRETOP                                    *
*----------------------------------------------------------------------*
*/ Constnats for Time
  CONSTANTS: c_begintime TYPE t VALUE '063000'.
  CONSTANTS: c_onehour   TYPE t VALUE '010000'.

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

**--- blocked by stlim (2004/04/29)
*  DATA: it_bdcmsgcoll LIKE TABLE OF bdcmsgcoll.
**--- end of block

**--- insert by stlim (2004/04/29)
  DATA: it_bdcmsgcoll LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
**--- end of insert

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
         ztime      LIKE ztmm_mast-ztime,  "Time from PBS out to W/S
**--- insert by stlim (2004/04/29)
         ZLINE      like ztmm_mast-zline,
         WORKS      like ztmm_mast-WORKS,
         RH_LH      like ztmm_mast-RH_LH,
         DISPO      like ztmm_mast-DISPO,
**--- end of insert
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
  DATA:  feedr      LIKE ztmm_mast-feedr,      "Feeder
         feed_cycle LIKE ztmm_mast-feed_cycle, "Feed Cycle
         ztime      LIKE ztmm_mast-ztime,      "Time from PBS out to W/S
        END OF wa_ztmm_nstl.

  DATA: it_ztmm_nstl LIKE TABLE OF wa_ztmm_nstl.
  FIELD-SYMBOLS: <fs_ztmm_nstl> LIKE LINE OF it_ztmm_nstl.

**--- insert by stlim (2004/04/29)
  DATA : BEGIN OF it_itab OCCURS 0.
          INCLUDE STRUCTURE wa_matnr_date_time.
  DATA :   tanum LIKE ltap-tanum,     " TO number
           w_docno TYPE num10,
           linecolor(4),     " ALV Color
           messa(80),
           msgty LIKE ztmm_stl_log-msgty,
         END OF it_itab.

**--- Macro
  DEFINE append_fieldcat.
    &1 = &1 + 1.
    w_fieldcat-col_pos    = &1.
    w_fieldcat-fieldname  = &2.
    w_fieldcat-outputlen  = &3.
    w_fieldcat-seltext_l  = &4.
    w_fieldcat-seltext_m  = &4.
    w_fieldcat-seltext_s  = &4.
    w_fieldcat-datatype   = &5.
    w_fieldcat-key        = &6.
    w_fieldcat-qfieldname = &7.
    w_fieldcat-cfieldname = &8.
    append w_fieldcat.
    clear : w_fieldcat.
  END-OF-DEFINITION.

  DEFINE append_sortcat.
    w_sortcat-spos      = &1.
    w_sortcat-fieldname = &2.
    w_sortcat-tabname   = &3.
    w_sortcat-up        = &4.
    w_sortcat-subtot    = &5.
    append w_sortcat.
    clear : w_sortcat.
  END-OF-DEFINITION.
**--- end of insert


*/ Selection Screen
  SELECTION-SCREEN BEGIN OF BLOCK bl_1 WITH FRAME TITLE text-bl1.
  PARAMETERS : p_tocred LIKE ztmm_nstl-datum
                    OBLIGATORY. "NSTL TO Creation Date
  SELECTION-SCREEN END OF BLOCK bl_1.

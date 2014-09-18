************************************************************************
* Program Name      : ZEMMPM10E_STL_07
* Author            : Sung-Tae, Lim
* Creation Date     : 2004.04.23.
* Specifications By : Sung-Tae, Lim
* Development Request No : UD1K909855
* Addl Documentation:
* Description       : Supply to Line - RP07
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2004.04.23.     Sung-Tae Lim     UD1K909855     Initial Coding
*
*
************************************************************************

REPORT zemmpm10e_stl_07 NO STANDARD PAGE HEADING
                        LINE-SIZE 400
                        MESSAGE-ID zmmm.

**---
INCLUDE : zrmmpmxxr_incl.

**---
DATA : w_subrc LIKE sy-subrc.

*--- For BDC message
DATA : it_bdcmsgcoll LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE,
       wa_bdcmsgcoll LIKE LINE OF it_bdcmsgcoll.

* Variable for date & time
DATA : w_current_date   TYPE sy-datum.
DATA : w_current_time   TYPE sy-uzeit.
DATA : w_screen_date    TYPE sy-datum.
DATA : w_screen_time    TYPE sy-uzeit.
DATA : w_dummy_date     TYPE sy-datum.  "Dummy date for temp use
DATA : w_dummy_minutes  TYPE num03.  "Dummy minutes for Cal of Start D&T


**---
* Itab & WA for Create TO(/nLT01)
DATA : BEGIN OF wa_toline,
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
         lpmin        LIKE ztmm_mast-lpmin,     " safety stock
         open_to      LIKE ltap-vsola,      "Open TO
         bferrorqty   LIKE affw-erfmg,      "Backkflush Error Qty
         bf_to_qty    LIKE ppc1_all-komp_quant,   "to Backflush Qty.
         rdmng        LIKE mlgt-rdmng,      "Rounding Qty
         src_lgtyp    LIKE mlgt-lgtyp,      "Source Storage type
         src_lgpla    LIKE mlgt-lgpla,      "Source Storage bin
         des_lgtyp    LIKE pkhd-lgtyp,      "Destination Storage type
         des_lgpla    LIKE pkhd-lgpla,      "Destination Storage bin
         tqty         LIKE resb-bdmng,      "Target Qty
         dispo        LIKE ztmm_mast-dispo,
       END OF wa_toline.

DATA : it_toline LIKE wa_toline OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_itab OCCURS 0.
        INCLUDE STRUCTURE it_toline.
DATA :   tanum LIKE ltap-tanum,     " TO number
         w_docno TYPE num10,
         linecolor(4),     " ALV Color
         messa(80),
         msgty LIKE ztmm_stl_log-msgty,
       END OF it_itab.


* Odd Even Flag
DATA : w_odd_even TYPE i.

**---
**** Constants&Vars for Number range object ****************************
CONSTANTS : c_nro_nr_00   VALUE '00' LIKE inri-nrrangenr, "Header Part
            c_nro_nr_01   VALUE '01' LIKE inri-nrrangenr, "Item Part
            c_nro_nr_09   VALUE '09' LIKE inri-nrrangenr, "App. Doc no.
            c_onehour   TYPE t VALUE '010000'.

*--- Number range object
DATA : w_nro_object  VALUE 'ZMMNRO0002' LIKE inri-object. "NRO for ZTLOG

*--- Number_Get_Next
DATA :      w_nro_number  TYPE num10.      " Same type of w_nro_object

DATA : w_zdocno TYPE num10.       "App. Doc. No.

*--- scheduled backflush quantity
DATA : BEGIN OF it_bfqty_temp OCCURS 0,
         matnr LIKE ztmm_ppc1_all-matnr,
         werks LIKE ztmm_ppc1_all-werks,
         lgort LIKE ztmm_ppc1_all-lgort,
         komp_quant LIKE ztmm_ppc1_all-komp_quant,
         flg_asynch LIKE ztmm_ppc1_all-flg_asynch,
         sync_ind LIKE ztmm_ppc1_all-sync_ind,
       END OF it_bfqty_temp.

DATA : it_back LIKE it_bfqty_temp OCCURS 0 WITH HEADER LINE.

*--- current stock
DATA : BEGIN OF it_stock_temp OCCURS 0,
         matnr LIKE lqua-matnr,
         gesme LIKE lqua-gesme,
         lgtyp LIKE pkhd-lgtyp,
         lgpla LIKE pkhd-lgpla,
       END OF it_stock_temp.

DATA : it_stock LIKE it_stock_temp OCCURS 0 WITH HEADER LINE.

*--- open TO quantity
DATA : BEGIN OF it_open_temp OCCURS 0,
         matnr LIKE ltap-matnr,
         vsola LIKE ltap-vsola,
       END OF it_open_temp.

DATA : it_open LIKE it_open_temp OCCURS 0 WITH HEADER LINE.

*--- backflush error quantity
DATA : BEGIN OF it_bferror_temp OCCURS 0,
         matnr LIKE affw-matnr,
         erfmg LIKE affw-erfmg,
         bwart LIKE affw-bwart,
         werks LIKE affw-werks,
         lgort LIKE affw-lgort,
       END OF it_bferror_temp.

DATA : it_bferror LIKE it_bferror_temp OCCURS 0 WITH HEADER LINE.

*--- rounding quantity
DATA : BEGIN OF it_round_temp OCCURS 0,
         matnr LIKE mlgt-matnr,
         rdmng LIKE mlgt-rdmng,
       END OF it_round_temp.

DATA : it_round LIKE it_round_temp OCCURS 0 WITH HEADER LINE.

*--- source storage bin/type
DATA : BEGIN OF it_source OCCURS 0,
         matnr LIKE mlgt-matnr,
         lgtyp LIKE mlgt-lgtyp,
         lgpla LIKE mlgt-lgpla,
       END OF it_source.

*--- destination storage bin/type
DATA : BEGIN OF it_dest OCCURS 0,
         matnr LIKE pkhd-matnr,
         lgtyp LIKE pkhd-lgtyp,
         lgpla LIKE pkhd-lgpla,
       END OF it_dest.

*--- MRP controller
DATA : BEGIN OF it_dispo OCCURS 0,
         dispo LIKE marc-dispo,
       END OF it_dispo.

DATA : w_minus_stock_check(1).

DATA : it_ztmm_stl_time LIKE ztmm_stl_time OCCURS 0 WITH HEADER LINE,
       w_tabix LIKE sy-tabix,
       w_lines TYPE i.


**---
CONSTANTS : c_time_073000 TYPE t VALUE '073000',
            c_time_083000 TYPE t VALUE '083000',
            c_time_093000 TYPE t VALUE '093000',
            c_time_111500 TYPE t VALUE '111500',
            c_time_121500 TYPE t VALUE '121500',
            c_time_131500 TYPE t VALUE '131500',
            c_time_141500 TYPE t VALUE '141500',
            c_time_151500 TYPE t VALUE '151500',
            c_time_161500 TYPE t VALUE '161500',
            c_time_171500 TYPE t VALUE '171500',
            c_time_181500 TYPE t VALUE '181500',
            c_time_191500 TYPE t VALUE '191500',
            c_time_201500 TYPE t VALUE '201500',
            c_time_220000 TYPE t VALUE '220000',
            c_time_230000 TYPE t VALUE '230000',
            c_time_000000 TYPE t VALUE '000000',
            c_time_010000 TYPE t VALUE '010000',
            c_time_020000 TYPE t VALUE '020000',
            c_time_030000 TYPE t VALUE '030000',
            c_time_040000 TYPE t VALUE '040000',
            c_time_063000 TYPE t VALUE '063000'.

DATA : w_max_exec_datetime LIKE ztmm_stl_exec-dtime,
       w_max_exec_time TYPE t.
DATA : w_starting_time LIKE ztmm_stl_time-stime,
       w_ending_time   LIKE ztmm_stl_time-etime.
DATA : w_1shift_overtime(1),
       w_2shift_overtime(1).

DATA : it_worktime LIKE zsmm_worktime OCCURS 0 WITH HEADER LINE.

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


**---
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-003.
PARAMETERS : p_cdate LIKE sy-datum OBLIGATORY,     "Current date
             p_ctime LIKE sy-uzeit OBLIGATORY.     "Current time
SELECTION-SCREEN END OF BLOCK block1.

SELECT-OPTIONS : s_matnr FOR mara-matnr.


**---
INITIALIZATION.
  p_cdate = sy-datum.
  p_ctime = sy-uzeit.
  PERFORM event_build USING w_eventcat[].


**---
TOP-OF-PAGE.
  PERFORM top_of_page.
*  PERFORM make_col_heading.


**---
START-OF-SELECTION.
  PERFORM get_data.


**---
END-OF-SELECTION.
  IF it_toline[] IS INITIAL.
    MESSAGE s999(zmmm) WITH 'There is no data!'(001).
    EXIT.
  ELSE.
    PERFORM execute_process.
    PERFORM update_table.
    PERFORM comment_build.     " USING w_top_of_page[].
    PERFORM make_alv_grid.
*    PERFORM send_mail.
  ENDIF.








*&----------------------------------------------------------------------
FORM time_calculation USING    value(im_date) TYPE d
                               value(im_time) TYPE t
                               value(im_minutes)
                      CHANGING value(ex_date) TYPE d
                               value(ex_time) TYPE t.
*---
  CLEAR : ex_date, ex_time.
  DATA : lv_time    TYPE t.
  DATA : lv_hoursum TYPE p.

  PERFORM get_time_from_minutes USING    im_minutes
                                CHANGING lv_time.

  lv_hoursum = im_time(2) + lv_time(2).
  ex_date = im_date.
  ex_time = im_time + lv_time.
  IF lv_hoursum >= 24.
    ex_date = ex_date + 1.
  ENDIF.

**---
  MOVE : '00' TO ex_time+4(2).
ENDFORM.                    " time_calculation

*&---------------------------------------------------------------------*
*&      Form  get_verme_lpmin
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_verme_lpmin.
*---
  CLEAR : it_stock_temp, it_stock_temp[], it_stock, it_stock[].

  SELECT a~matnr
         gesme     " quantity
         a~lgtyp
         a~lgpla
               INTO CORRESPONDING FIELDS OF TABLE it_stock_temp
               FROM pkhd AS a INNER JOIN lqua AS b
                 ON a~mandt EQ b~mandt
                AND a~matnr EQ b~matnr
                AND a~lgtyp EQ b~lgtyp
                AND a~lgpla EQ b~lgpla
                FOR ALL ENTRIES IN it_toline
              WHERE a~matnr EQ it_toline-matnr.

  LOOP AT it_stock_temp.
    MOVE : it_stock_temp-matnr TO it_stock-matnr,
           it_stock_temp-gesme TO it_stock-gesme.
    COLLECT it_stock.
    CLEAR : it_stock_temp, it_stock.
  ENDLOOP.
ENDFORM.                    " get_verme_lpmin

*&---------------------------------------------------------------------*
*&      Form  get_open_to
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_open_to.
*---
  CLEAR : it_open_temp, it_open_temp[], it_open, it_open[].

  SELECT matnr
         vsola     "Source target quantity in alternate unit
               INTO CORRESPONDING FIELDS OF TABLE it_open_temp
               FROM ltap
                FOR ALL ENTRIES IN it_toline
              WHERE matnr EQ it_toline-matnr
                AND pquit EQ space
                AND lgnum EQ 'P01'.
  "Open TO(Indicator: confirmation complete)

  LOOP AT it_open_temp.
    MOVE-CORRESPONDING it_open_temp TO it_open.
    COLLECT it_open.
    CLEAR : it_open_temp, it_open.
  ENDLOOP.
ENDFORM.                    " get_open_to

*&---------------------------------------------------------------------*
*&      Form  get_bf_error_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_bf_error_qty.
*---
  CLEAR : it_bferror_temp, it_bferror_temp[], it_bferror, it_bferror[].

  CLEAR : affw.

  SELECT matnr
         bwart
         werks
         lgort
         erfmg
               INTO CORRESPONDING FIELDS OF TABLE it_bferror_temp
               FROM affw
                FOR ALL ENTRIES IN it_toline
              WHERE matnr EQ it_toline-matnr
*                AND werks EQ 'P001'
*                AND lgort EQ 'P400'
                AND bwart EQ '261'.

  DELETE it_bferror_temp WHERE NOT ( werks EQ 'P001'
                                 AND lgort EQ 'P400' ).

  LOOP AT it_bferror_temp.
    MOVE : it_bferror_temp-matnr TO it_bferror-matnr,
           it_bferror_temp-erfmg TO it_bferror-erfmg.
    COLLECT it_bferror.
    CLEAR : it_bferror_temp, it_bferror.
  ENDLOOP.
ENDFORM.                    " get_bf_error_qty

*&---------------------------------------------------------------------*
*&      Form  get_rdmng
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_rdmng.
*---
  CLEAR : it_round, it_round[], it_round_temp, it_round_temp[].

  SELECT matnr
         rdmng
               INTO CORRESPONDING FIELDS OF TABLE it_round_temp
               FROM mlgt
                FOR ALL ENTRIES IN it_toline
              WHERE matnr EQ it_toline-matnr
                AND lvorm EQ space.

  LOOP AT it_round_temp.
    MOVE : it_round_temp-matnr TO it_round-matnr,
           it_round_temp-rdmng TO it_round-rdmng.
    COLLECT it_round.
    CLEAR : it_round_temp, it_round.
  ENDLOOP.
ENDFORM.                    " get_rdmng

*&---------------------------------------------------------------------*
*&      Form  get_tqty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_tqty.
**--- logic which get quantity :
*     if supply to line master table 'STOCK_CHECK' = 'X'.
*  TO Qty. =    RESB-BDMNG(requirement qty)
*     ¨ç      - LQUA-VERME(current stock(GESME))
*     ¨è      + Safety Stock(Supply to Line master table field)
*     ¨é      - open quantity
*     ¨ê      + backflush error quantity
*     ¨ë      + scheduled backflush quantity
**---

*---
  DATA : lv_tqty LIKE resb-bdmng.  "Target Quantity.

  lv_tqty = it_toline-bdmng.

*--- check current stock     ¨ç
  IF it_toline-stock_check = 'X'.
    CLEAR : it_stock.
    READ TABLE it_stock WITH KEY matnr = it_toline-matnr.
    MOVE : it_stock-gesme TO it_toline-verme.
    lv_tqty = lv_tqty - it_toline-verme.
  ENDIF.

*--- check safety stock     ¨è
  IF it_toline-stock_check = 'X'.
    lv_tqty = lv_tqty + it_toline-lpmin.
  ENDIF.

*--- check Open TO quantity     ¨é
  IF it_toline-stock_check = 'X'.
    CLEAR : it_open.
    READ TABLE it_open WITH KEY matnr = it_toline-matnr.
    MOVE : it_open-vsola TO it_toline-open_to.
    lv_tqty = lv_tqty - it_toline-open_to.
  ENDIF.

*--- check backflush error quantity     ¨ê
  IF it_toline-stock_check = 'X'.
    CLEAR : it_bferror.
    READ TABLE it_bferror WITH KEY matnr = it_toline-matnr.
    MOVE : it_bferror-erfmg TO it_toline-bferrorqty.
    lv_tqty = lv_tqty + it_toline-bferrorqty.
  ENDIF.

*--- check scheduled backflush quantity     ¨ë
  IF it_toline-stock_check = 'X'.
    CLEAR : it_back.
    READ TABLE it_back WITH KEY matnr = it_toline-matnr.
    MOVE : it_back-komp_quant TO it_toline-bf_to_qty.
    lv_tqty = lv_tqty + it_toline-bf_to_qty.
  ENDIF.

  IF lv_tqty LT 1.
    MOVE : 'X' TO w_minus_stock_check.
  ENDIF.


  IF w_minus_stock_check EQ space.
*--- rounding value check
*A. Get Remainder  :mod
*B. Get quotient   :div
    DATA : lv_tqtymodrdmng TYPE p,
           lv_tqtydivrdmng TYPE p.

    CLEAR : it_round.
    READ TABLE it_round WITH KEY matnr = it_toline-matnr.
    MOVE : it_round-rdmng TO it_toline-rdmng.

    IF NOT it_toline-rdmng IS INITIAL.
      lv_tqtymodrdmng = lv_tqty MOD it_toline-rdmng.
      lv_tqtydivrdmng = lv_tqty DIV it_toline-rdmng.
    ENDIF.

    IF NOT lv_tqtymodrdmng IS INITIAL.
      lv_tqtydivrdmng = lv_tqtydivrdmng + 1.
      lv_tqty = lv_tqtydivrdmng * it_toline-rdmng.
    ENDIF.
    it_toline-tqty = lv_tqty.   "Target Qty
  ELSE.
    it_toline-tqty = 0.
  ENDIF.
ENDFORM.                    " get_tqty

*&---------------------------------------------------------------------*
*&      Form  number_get_next
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_NRO_NR_09  text
*      -->P_W_NRO_OBJECT  text
*      <--P_W_ZDOCNO  text
*----------------------------------------------------------------------*
FORM number_get_next
        USING    value(p_w_nro_interval) LIKE inri-nrrangenr
                 value(p_w_nro_object)   LIKE inri-object
        CHANGING value(p_w_nro_next).
*---
  CLEAR: p_w_nro_next.

  CALL FUNCTION 'NUMBER_GET_NEXT'
       EXPORTING
            nr_range_nr             = p_w_nro_interval
            object                  = p_w_nro_object
       IMPORTING
            number                  = p_w_nro_next
       EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            OTHERS                  = 7.
ENDFORM.                    " number_get_next

*&---------------------------------------------------------------------*
*&      Form  get_sorce_storage_type_bin
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_sorce_storage_type_bin.
*---
  CLEAR : it_source, it_source[].

  SELECT matnr
         lgtyp
         lgpla
               INTO CORRESPONDING FIELDS OF TABLE it_source
               FROM mlgt
                FOR ALL ENTRIES IN it_toline
              WHERE matnr EQ it_toline-matnr
                AND lvorm EQ space.
ENDFORM.                    " get_sorce_storage_type_bin

*&---------------------------------------------------------------------*
*&      Form  get_des_storage_type_bin
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_des_storage_type_bin.
*---
  CLEAR : it_dest, it_dest[].

  SELECT matnr
         lgtyp
         lgpla
               INTO CORRESPONDING FIELDS OF TABLE it_dest
               FROM pkhd
                FOR ALL ENTRIES IN it_toline
              WHERE matnr EQ it_toline-matnr.
ENDFORM.                    " get_des_storage_type_bin

*&---------------------------------------------------------------------*
*&      Form  bdc_processing_lt01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BDCMSGCOLL  text
*      -->P_W_ZDOCNO  text
*      <--P_W_SUBRC  text
*----------------------------------------------------------------------*
FORM bdc_processing_lt01 TABLES   ext_bdcmsgcoll STRUCTURE bdcmsgcoll
                         USING    value(p_zdocno)
                         CHANGING value(p_subrc).
*---
  CLEAR : ext_bdcmsgcoll, ext_bdcmsgcoll[], p_subrc.

  DATA : lv_bwlvs_002     TYPE bdcdata-fval,   "Movement type
         lv_matnr_003     TYPE bdcdata-fval,
         lv_anfme_004     TYPE bdcdata-fval,
         lv_anfme_007     TYPE bdcdata-fval,
         lv_altme_008     TYPE bdcdata-fval,
         lv_vltyp_009     TYPE bdcdata-fval,
         lv_vlpla_010     TYPE bdcdata-fval,
         lv_nltyp_011     TYPE bdcdata-fval,
         lv_nlpla_012     TYPE bdcdata-fval,
         lv_refnr_013     TYPE bdcdata-fval.   "Group(Feeder)

  lv_bwlvs_002 = '850'.  "Movement type
  lv_refnr_013 = it_toline-feedr. "Group(Feeder)
  lv_matnr_003  = it_toline-matnr. "Material '327003K100'
  lv_anfme_004  = it_toline-tqty.
  lv_anfme_007  = it_toline-tqty.
  lv_altme_008  = it_toline-meins.
  lv_vltyp_009  = it_toline-src_lgtyp.  "Src Storage Type '434'
  lv_vlpla_010  = it_toline-src_lgpla.  "Src Storage Bin  'AA-01-11'
  lv_nltyp_011  = it_toline-des_lgtyp.  "Des Storage Type '443'
  lv_nlpla_012  = it_toline-des_lgpla.  "Des Storage Bin  'TS-01'

  CONDENSE : lv_bwlvs_002,  "Movement type
             lv_matnr_003,
             lv_anfme_004,
             lv_anfme_007,
             lv_altme_008,
             lv_vltyp_009,
             lv_vlpla_010,
             lv_nltyp_011,
             lv_nlpla_012,
             lv_refnr_013.

*--- BDC for LT01(Create TO)
  CALL FUNCTION 'Z_FMM_6012_01'
       EXPORTING
            lgnum_001 = 'P01'  "Warehouse number
            refnr_013 = lv_refnr_013  "Group(Feeder)
            bwlvs_002 = lv_bwlvs_002  "Movement type '850'
            matnr_003 = lv_matnr_003  "Material '327003K100'
            anfme_004 = lv_anfme_004
            werks_005 = 'P001'  "Plant
            lgort_006 = 'P400'  "Storage Location
            anfme_007 = lv_anfme_007
            altme_008 = lv_altme_008
            vltyp_009 = lv_vltyp_009  "Src Storage Type '434'
            vlpla_010 = lv_vlpla_010  "Src Storage Bin 'AA-01-11'
            nltyp_011 = lv_nltyp_011  "Des Storage Type '443'
            nlpla_012 = lv_nlpla_012  "Des Storage Bin 'TS-01'
       IMPORTING
            subrc     = p_subrc
       TABLES
            messtab   = ext_bdcmsgcoll[].

*--- One More Try
  IF p_subrc NE 0.
    CALL FUNCTION 'Z_FMM_6012_01'
         EXPORTING
              lgnum_001 = 'P01'  "Warehouse number
              refnr_013 = lv_refnr_013  "Group(Feeder)
              bwlvs_002 = lv_bwlvs_002  "Movement type '850'
              matnr_003 = lv_matnr_003  "Material '327003K100'
              anfme_004 = lv_anfme_004  " Quantity
              werks_005 = 'P001'  "Plant
              lgort_006 = 'P400'  "Storage Location
              anfme_007 = lv_anfme_007  " Quantity
              altme_008 = lv_altme_008  " Unit of Measure
              vltyp_009 = lv_vltyp_009  "Src Storage Type '434'
              vlpla_010 = lv_vlpla_010  "Src Storage Bin 'AA-01-11'
              nltyp_011 = lv_nltyp_011  "Des Storage Type '443'
              nlpla_012 = lv_nlpla_012  "Des Storage Bin 'TS-01'
         IMPORTING
              subrc     = p_subrc
         TABLES
              messtab   = ext_bdcmsgcoll[].
  ENDIF.
ENDFORM.                    " bdc_processing_lt01

*&---------------------------------------------------------------------*
*&      Form  bdc_processing_lta1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BDCMSGCOLL  text
*      -->P_W_ZDOCNO  text
*      -->P_WA_BDCMSGCOLL_MSGV1  text
*      <--P_W_SUBRC  text
*----------------------------------------------------------------------*
FORM bdc_processing_lta1 TABLES   ext_bdcmsgcoll STRUCTURE bdcmsgcoll
                         USING    value(p_zdocno)
                                  value(p_msgv1)
                         CHANGING value(p_subrc).
*---
  CLEAR : ext_bdcmsgcoll, ext_bdcmsgcoll[], p_subrc.

  DATA : lv_tanum_001     TYPE bdcdata-fval,  "TO number
         lv_lgnum_002     TYPE bdcdata-fval,  "Warehouse number
         lv_stdat_003     TYPE bdcdata-fval,  "Start date
         lv_stuzt_004     TYPE bdcdata-fval,  "Start time
         lv_endat_005     TYPE bdcdata-fval,  "End date
         lv_enuzt_006     TYPE bdcdata-fval.  "End time

  DATA : lv_date(8).

  CLEAR : lv_date.

  PERFORM user_date_format USING    sy-uname
                                    it_toline-sdate
                           CHANGING lv_date.

  lv_stdat_003 = lv_date.        "Start date
  lv_tanum_001 = p_msgv1.                              "TO number  '813'
  lv_lgnum_002 = 'P01'.                                "Warehouse number
  lv_stuzt_004 = it_toline-feeding_time.             "Start time

  MOVE : w_ending_time TO it_toline-etime.

  PERFORM time_calculation USING    it_toline-sdate
                                    it_toline-feeding_time
                                    60   "Minutes
                           CHANGING it_toline-edate
                                    it_toline-etime.

*  MOVE : w_ending_time TO it_toline-etime.

  lv_enuzt_006 = it_toline-etime. "End time

  CLEAR : lv_date.
  PERFORM user_date_format USING    sy-uname
                                    it_toline-edate
                           CHANGING lv_date.

  lv_endat_005 = lv_date.      "End date

  CONDENSE : lv_tanum_001,
             lv_lgnum_002,
             lv_stdat_003,
             lv_stuzt_004,
             lv_endat_005,
             lv_enuzt_006.

*--- BDC for LTA1(Change TO Header)
  CALL FUNCTION 'Z_FMM_6012_02'
       EXPORTING
            tanum_001 = lv_tanum_001  " TO number '813'
            lgnum_002 = lv_lgnum_002
            stdat_003 = lv_stdat_003  "Start date
            stuzt_004 = lv_stuzt_004  "Start time "'10:36:48'
            endat_005 = lv_endat_005  "End date
            enuzt_006 = lv_enuzt_006  "End time
       IMPORTING
            subrc     = p_subrc
       TABLES
            messtab   = ext_bdcmsgcoll[].

*--- BDC Log to the table ZTLOG
  IF ext_bdcmsgcoll[] IS INITIAL.  "SUCCESS
    CLEAR : wa_bdcmsgcoll.
    wa_bdcmsgcoll-tcode   = 'LT1A'.
    wa_bdcmsgcoll-msgtyp  = 'S'.  "SUCCESS
    wa_bdcmsgcoll-msgspra = 'E'.
    wa_bdcmsgcoll-msgid   = 'ZMMM'.
    wa_bdcmsgcoll-msgnr   = '999'.
    wa_bdcmsgcoll-msgv1   = 'Transfer order'.
    wa_bdcmsgcoll-msgv2   = lv_tanum_001.
    wa_bdcmsgcoll-msgv3   = 'Start/End Date/Time'.
    wa_bdcmsgcoll-msgv4   = 'is changed.'.
    APPEND wa_bdcmsgcoll TO ext_bdcmsgcoll[].
  ENDIF.
ENDFORM.                    " bdc_processing_lta1

*&---------------------------------------------------------------------*
*&      Form  write_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_list.
**---
*  WRITE : /1(18)  <fs_toline>-matnr RIGHT-JUSTIFIED,
*          20(10)  <fs_toline>-bdmng UNIT <fs_toline>-meins,
*          31      <fs_toline>-meins,
*          36      <fs_toline>-works,
*          50      <fs_toline>-rh_lh,
*          60      <fs_toline>-feeding_time,  "Start time(Feeding Time)
*          75      <fs_toline>-stock_check,
*          90      <fs_toline>-feed_cycle,
*         115      <fs_toline>-ztime,
*         155      <fs_toline>-verme UNIT <fs_toline>-meins,
*         180      <fs_toline>-lpmin UNIT <fs_toline>-meins,
*         220      <fs_toline>-open_to UNIT <fs_toline>-meins,
*         240      <fs_toline>-bferrorqty UNIT <fs_toline>-meins,
*         270      <fs_toline>-bf_to_qty UNIT <fs_toline>-meins,
*         300      <fs_toline>-rdmng UNIT <fs_toline>-meins,
*         325      <fs_toline>-tqty UNIT <fs_toline>-meins,
*         355      <fs_toline>-feedr,      "Feeder
*         365      wa_bdcmsgcoll-msgv1 COLOR COL_POSITIVE.
ENDFORM.                    " write_list

*&---------------------------------------------------------------------*
*&      Form  make_col_heading
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_col_heading.
*---
  WRITE: /1   'Material',
          20(9) 'Quantity' RIGHT-JUSTIFIED,
          31  'Unit',
          36  'Workstation',
          50  'RH/LH',
          60  'Start Time',  "Feeding Time
          75  'Stock Check',
          90  'Feed Cycle',
          115  'Time for STL',
          155 'Available stock(GESME)',
          180 'Safety Stock(LPMIN)',
*         155  'Available stock(VERME)',
*         180 'Minimum storage bin quantity(LPMIN)',
          220 'Open TO',
          240 'Backflush Error Check',
          270 'Scheduled Backflush Qty',
          300 'Rounding Quantity Check',
          325 'Target Quantity(TO)',
          355 'Feeder'.
ENDFORM.                    " make_col_heading

*&---------------------------------------------------------------------*
*&      Form  get_time_from_minutes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IM_MINUTES  text
*      <--P_LV_TIME  text
*----------------------------------------------------------------------*
FORM get_time_from_minutes USING    value(im_minutes)
                           CHANGING value(ex_time) TYPE t.
*---
  CLEAR : ex_time.
  DATA : BEGIN OF ls_time,
           hour(2) TYPE n,
           minute(2) TYPE n,
           second(2) TYPE n,
         END OF ls_time.

  ls_time-minute = im_minutes MOD 60.
  ls_time-hour   = im_minutes DIV 60.

  MOVE ls_time TO ex_time.
ENDFORM.                    " get_time_from_minutes

*&---------------------------------------------------------------------*
*&      Form  user_date_format
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_UNAME  text
*      -->P_<FS_TOLINE>_SDATE  text
*      <--P_LV_DATE  text
*----------------------------------------------------------------------*
FORM user_date_format USING    value(p_user)     LIKE sy-uname
                               value(p_date)     LIKE sy-datum
                      CHANGING value(p_userdate) TYPE char8.
*---
  CLEAR : p_userdate.

  DATA : yyyy(4).  "year
  DATA : mm(2).    "day
  DATA : dd(2).    "month
  DATA : datfm LIKE usr01-datfm.  "date format

  SELECT SINGLE datfm INTO datfm
                      FROM usr01
                     WHERE bname = p_user.

** datfm
*1 DD.MM.YYYY
*2 MM/DD/YYYY
*3 MM-DD-YYYY
*4 YYYY.MM.DD
*5 YYYY/MM/DD
*6 YYYY-MM-DD
  yyyy = p_date+0(4).
  mm   = p_date+4(2).
  dd   = p_date+6(2).

  CASE datfm.
    WHEN 1.
      p_userdate+0(2) = dd.
      p_userdate+2(2) = mm.
      p_userdate+4(4) = yyyy.
    WHEN 2 OR 3.
      p_userdate+0(2) = mm.
      p_userdate+2(2) = dd.
      p_userdate+4(4) = yyyy.
    WHEN 4 OR 5 OR 6.
      p_userdate+0(4) = yyyy.
      p_userdate+4(2) = mm.
      p_userdate+6(2) = dd.
  ENDCASE.
ENDFORM.                    " user_date_format

*&---------------------------------------------------------------------*
*&      Form  get_scheduled_bf_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_scheduled_bf_qty.
*---
  CLEAR : it_bfqty_temp, it_bfqty_temp[], it_back, it_back[].

  SELECT matnr
         werks
         lgort
         komp_quant     " quantity
         flg_asynch
         sync_ind
                    INTO CORRESPONDING FIELDS OF TABLE it_bfqty_temp
                    FROM ztmm_ppc1_all
                   WHERE flg_asynch NE 'X'.

*---
  DELETE it_bfqty_temp WHERE sync_ind EQ 'X'.

*---
  LOOP AT it_bfqty_temp.
    MOVE-CORRESPONDING it_bfqty_temp TO it_back.
    COLLECT it_back.
    CLEAR : it_bfqty_temp, it_back.
  ENDLOOP.
ENDFORM.                    " get_scheduled_bf_qty

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
*---
  CLEAR : w_screen_date, w_screen_time, w_current_date, w_current_time.

  MOVE : p_cdate TO w_screen_date,
         p_ctime TO w_screen_time,
         w_screen_date TO w_current_date,
         w_screen_time TO w_current_time.

*---
  PERFORM decide_times.

*---
  PERFORM get_vehicle_master.
ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  get_vehicle_master
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_vehicle_master.
*---
  DATA : lv_rp06_timestamp(11).      "'YYYYMMDDHHMISSX'

  DATA : BEGIN OF ls_rsnum,
           rsnum LIKE ztpp_dvrt1-rsnum,  "Reservation number
         END OF ls_rsnum.

  DATA : lt_rsnum LIKE ls_rsnum OCCURS 0 WITH HEADER LINE.

  DATA : lt_supply_info LIKE zspp_vin_info_for_stl OCCURS 0
                                                   WITH HEADER LINE.

*---
  CLEAR : lt_supply_info, lt_supply_info[].

  CALL FUNCTION 'Z_FPP_GET_SUPPLY_TO_LINE'
       EXPORTING
            i_date                   = w_current_date
            i_time                   = w_current_time
            i_rp                     = '07'
       TABLES
            t_supply_info            = lt_supply_info
       EXCEPTIONS
            no_data_founded          = 1
            line_info_does_not_exist = 2
            etc_exception            = 3
            OTHERS                   = 4.

  LOOP AT lt_supply_info.
    MOVE : lt_supply_info-rsnum TO ls_rsnum.
    APPEND ls_rsnum TO lt_rsnum.
    CLEAR : lt_supply_info, ls_rsnum, lt_rsnum.
  ENDLOOP.

* Unique reservation numbers
  DELETE lt_rsnum WHERE rsnum = '0000000000'.

  DELETE ADJACENT DUPLICATES FROM lt_rsnum
                             COMPARING rsnum.

*/ Get Quantity Raw Data(LT_TOLINE) from RESB and ZTMM_MAST
  DATA : BEGIN OF ls_toline.
          INCLUDE STRUCTURE wa_toline.
  DATA :   rsnum LIKE resb-rsnum,
           rspos LIKE resb-rspos,
         END OF ls_toline.

  DATA : lt_toline LIKE ls_toline OCCURS 0 WITH HEADER LINE.

  CHECK NOT lt_rsnum[] IS INITIAL.

  SELECT a~rsnum            "Reservation number
         a~rspos            "Item number of reservation
         a~matnr            "Material
         a~bdmng            "Quantity
         a~meins            "Unit
         b~feedr            "Feeder
         b~works            "Workstation
         b~rh_lh            "RH/LH
         b~stock_check      "STOCK CHECK
         b~feed_cycle       "FEEDING CYCLE
         b~ztime            "TIME
         b~lpmin            " safety stock
         b~dispo            " person of contact
                 INTO CORRESPONDING FIELDS OF TABLE lt_toline
                 FROM resb AS a INNER JOIN ztmm_mast AS b
                   ON a~mandt EQ b~mandt
                  AND a~matnr EQ b~matnr
                  FOR ALL ENTRIES IN lt_rsnum
                WHERE a~rsnum EQ lt_rsnum-rsnum    "Reservation number
                  AND b~werks EQ 'P001'
                  AND b~spptl EQ 'S'
                  AND zline IN ('F1', 'F2', 'F3')
                  AND b~matnr IN s_matnr.

*/Get draft data(IT_TOLINE) for TO Creation
  DATA : lv_bdmng     LIKE wa_toline-bdmng.  "For Sum of qty

  SORT lt_toline BY matnr.

  LOOP AT lt_toline.
    lv_bdmng = lv_bdmng + lt_toline-bdmng.
    MOVE-CORRESPONDING lt_toline TO wa_toline.
    AT END OF matnr.
      MOVE : lv_bdmng TO wa_toline-bdmng.
      APPEND wa_toline TO it_toline.
      CLEAR : lv_bdmng.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " get_vehicle_master

*&---------------------------------------------------------------------*
*&      Form  execute_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM execute_process.
*--- get scheduled backflush quantity
  PERFORM get_scheduled_bf_qty.
*--- get current stock
  PERFORM get_verme_lpmin.
*--- get open TO quantity
  PERFORM get_open_to.
*--- get BackFlush error quantity(/nCOGI)
  PERFORM get_bf_error_qty.
*--- get rounding value
  PERFORM get_rdmng.
*--- get source storage type/bin
  PERFORM get_sorce_storage_type_bin.
*--- get destination storage type/bin
  PERFORM get_des_storage_type_bin.

*---
  PERFORM create_transfer_order.
ENDFORM.                    " execute_process

*&---------------------------------------------------------------------*
*&      Form  create_transfer_order
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_transfer_order.
*---
  DATA : l_tabix LIKE sy-tabix,
         l_messa(80).

  CLEAR : it_itab, it_itab[], it_dispo, it_dispo[].

  LOOP AT it_toline.
    MOVE : sy-tabix TO l_tabix.
*--- TO GET Start Date(Feeding Date) and Start Time
    MOVE : w_screen_date   TO it_toline-sdate,
           w_starting_time TO it_toline-feeding_time.
*    PERFORM time_calculation USING    w_screen_date
*                                      w_screen_time
*                                      0
*                             CHANGING it_toline-sdate
*                                      it_toline-feeding_time.

*--- consider feeding cycle
    IF it_toline-feed_cycle EQ '120'.
      CLEAR : w_odd_even, it_ztmm_stl_time.
      READ TABLE it_ztmm_stl_time WITH KEY rtime = w_current_time.
      w_odd_even = it_ztmm_stl_time-zindx MOD 2.
*      w_odd_even = it_toline-feeding_time(2) MOD 2.
      IF w_odd_even EQ 0.
*      IF w_odd_even NE 1.
        CLEAR : it_ztmm_stl_time, w_tabix.
        READ TABLE it_ztmm_stl_time WITH KEY rtime = w_current_time.
        MOVE : sy-tabix TO w_tabix.
        w_tabix = w_tabix + 1.
        CLEAR : it_ztmm_stl_time.
        READ TABLE it_ztmm_stl_time INDEX w_tabix.
        MOVE : it_ztmm_stl_time-stime TO it_toline-feeding_time.
      ENDIF.
    ENDIF.
*---

    CLEAR : w_minus_stock_check.

*--- calculate quantity
    PERFORM get_tqty.

*--- App Doc No
    PERFORM number_get_next USING    c_nro_nr_09     "NRO Interval
                                     w_nro_object    "NRO Object
                            CHANGING w_zdocno.     "App Doc No
    COMMIT WORK.

    IF w_minus_stock_check EQ space.
*--- Get Source Storage type/bin
      CLEAR : it_source.
      READ TABLE it_source WITH KEY matnr = it_toline-matnr.
      MOVE : it_source-lgtyp TO it_toline-src_lgtyp,
             it_source-lgpla TO it_toline-src_lgpla.

*--- Get Destination Storage type/bin
      CLEAR : it_dest.
      READ TABLE it_dest WITH KEY matnr = it_toline-matnr.
      IF sy-subrc EQ 0.
        MOVE : it_dest-lgtyp TO it_toline-des_lgtyp,
               it_dest-lgpla TO it_toline-des_lgpla.
      ELSE.
        MOVE : 'XXX'         TO it_toline-des_lgtyp,
               'XXXXXXXXXX'  TO it_toline-des_lgpla.
      ENDIF.

*--- BDC Processing of /nLT01
      PERFORM bdc_processing_lt01 TABLES   it_bdcmsgcoll
                                  USING    w_zdocno
                                  CHANGING w_subrc.

      IF w_subrc EQ 0.
*--- Change TO Header (/nLT1A)
        CLEAR : wa_bdcmsgcoll.
        READ TABLE it_bdcmsgcoll INTO wa_bdcmsgcoll
                                 WITH KEY msgtyp = 'S'.
        CHECK sy-subrc = 0.
        PERFORM bdc_processing_lta1 TABLES   it_bdcmsgcoll
                                    USING    w_zdocno
                                             wa_bdcmsgcoll-msgv1
                                    CHANGING w_subrc.
      ENDIF.
    ENDIF.

    MODIFY it_toline INDEX l_tabix.

    MOVE-CORRESPONDING it_toline TO it_itab.
    MOVE : w_zdocno              TO it_itab-w_docno.
    IF w_minus_stock_check EQ space.
      IF w_subrc EQ 0.
        MOVE : c_green             TO it_itab-linecolor,
               'S'                 TO it_itab-msgty.
        CLEAR : it_bdcmsgcoll.
        READ TABLE it_bdcmsgcoll INDEX 1.
        MOVE : it_bdcmsgcoll-msgv2 TO it_itab-tanum.
      ELSE.
        MOVE : c_red               TO it_itab-linecolor,
               'E'                 TO it_itab-msgty.
      ENDIF.
    ELSE.
      CLEAR : it_bdcmsgcoll, it_bdcmsgcoll[].
    ENDIF.
*--- message
    CLEAR : it_bdcmsgcoll, l_messa.
    READ TABLE it_bdcmsgcoll WITH KEY msgtyp = 'E'.
    IF sy-subrc EQ 0.
      PERFORM get_message USING    it_bdcmsgcoll-msgid
                                   it_bdcmsgcoll-msgnr
                                   it_bdcmsgcoll-msgv1
                                   it_bdcmsgcoll-msgv2
                                   it_bdcmsgcoll-msgv3
                                   it_bdcmsgcoll-msgv4
                          CHANGING l_messa.
    ELSE.
      READ TABLE it_bdcmsgcoll WITH KEY msgtyp = 'S'.
      IF sy-subrc EQ 0.
        PERFORM get_message USING    it_bdcmsgcoll-msgid
                                     it_bdcmsgcoll-msgnr
                                     it_bdcmsgcoll-msgv1
                                     it_bdcmsgcoll-msgv2
                                     it_bdcmsgcoll-msgv3
                                     it_bdcmsgcoll-msgv4
                            CHANGING l_messa.
      ENDIF.
    ENDIF.
    MOVE : l_messa         TO it_itab-messa.
    APPEND it_itab.
*---
    MOVE : it_toline-dispo TO it_dispo-dispo.
    COLLECT it_dispo.
    CLEAR : it_toline, it_itab, it_dispo.
  ENDLOOP.
ENDFORM.                    " create_transfer_order

*&---------------------------------------------------------------------*
*&      Form  update_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_table.
*---
  DATA : it_ztmm_stl_log LIKE ztmm_stl_log OCCURS 0 WITH HEADER LINE.

  CLEAR : it_ztmm_stl_log, it_ztmm_stl_log[].

  LOOP AT it_itab.
    CLEAR : ztmm_stl_log.
    MOVE-CORRESPONDING it_itab TO it_ztmm_stl_log.
    MOVE : it_itab-w_docno     TO it_ztmm_stl_log-logno_h,
           sy-tcode            TO it_ztmm_stl_log-ztcode,
           sy-repid            TO it_ztmm_stl_log-zprogramm.
    it_ztmm_stl_log-ernam = it_ztmm_stl_log-aenam = sy-uname.
    it_ztmm_stl_log-erdat = it_ztmm_stl_log-aedat = sy-datum.
    it_ztmm_stl_log-erzet = it_ztmm_stl_log-aezet = sy-uzeit.
    APPEND it_ztmm_stl_log.
  ENDLOOP.

  MODIFY ztmm_stl_log FROM TABLE it_ztmm_stl_log.
ENDFORM.                    " update_table

*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM comment_build.
*---
  CLEAR : w_line.
  w_line-typ  = 'H'.
  w_line-info = text-006.
  APPEND w_line TO w_top_of_page.

  CLEAR : w_line.
  APPEND INITIAL LINE TO w_top_of_page.
ENDFORM.                    " comment_build

*&---------------------------------------------------------------------*
*&      Form  make_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_alv_grid.
*---
  MOVE : 'LINECOLOR' TO w_layout-info_fieldname.
  w_layout-colwidth_optimize = 'X'.

  PERFORM build_fieldcat.
  PERFORM build_sortcat.

  CLEAR : w_program.

  MOVE : sy-repid TO w_program.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            i_callback_program = w_program
            is_layout          = w_layout
            it_fieldcat        = w_fieldcat[]
            it_events          = w_eventcat[]
            it_sort            = w_sortcat[]
            i_save             = 'A'
       TABLES
            t_outtab           = it_itab
       EXCEPTIONS
            program_error      = 1
            OTHERS             = 2.
ENDFORM.                    " make_alv_grid

*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcat.
**--- &1 : position       &2 : field name       &3 : field length
**--- &4 : description    &5 : field type       &6 : key
**--- &7 : qty field      &8 : color
  append_fieldcat :
    w_col_pos 'MATNR'     18 'Material'       'CHAR' 'X' ''      '',
    w_col_pos 'BDMNG'     12 'Quantity'       'QUAN' ''  'MEINS' '',
    w_col_pos 'MEINS'     03 'UoM'            'UNIT' ''  ''      '',
    w_col_pos 'WORKS'     05 'Workstation'    'CHAR' ''  ''      '',
    w_col_pos 'RH_LH'     02 'RH/LH'          'CHAR' ''  ''      '',
    w_col_pos 'FEEDING_TIME' 10 'Starting Time'
                                              'TIMS' ''  ''      '',
    w_col_pos 'STOCK_CHECK'  01 'Stock Check'
                                              'CHAR' ''  ''      '',
    w_col_pos 'FEED_CYCLE'   03 'Feed Cycle'
                                              'NUMC' ''  'MEINS' '',
    w_col_pos 'ZTIME'     03 'Time for STL'   'NUMC' ''  ''      '',
    w_col_pos 'VERME'     12 'Current Stock'  'QUAN' ''  'MEINS' '',
    w_col_pos 'LPMIN'     12 'Safety Stock'   'QUAN' ''  'MEINS' '',
    w_col_pos 'OPEN_TO'   12 'Open TO'        'QUAN' ''  'MEINS' '',
    w_col_pos 'BFERRORQTY' 12 'B/F Error'     'QUAN' ''  'MEINS' '',
    w_col_pos 'BF_TO_QTY' 12 'to B/F Qty'     'QUAN' ''  'MEINS' '',
    w_col_pos 'RDMNG'     12 'Rounding Qty'   'QUAN' ''  'MEINS' '',
    w_col_pos 'TQTY'      12 'TO Qty'         'QUAN' ''  'MEINS' '',
    w_col_pos 'FEEDR'     05 'Feeder'         'CHAR' ''  ''      '',
    w_col_pos 'TANUM'     10 'TO Number'      'CHAR' ''  ''      '',
    w_col_pos 'SRC_LGTYP' 03 'Src S/Type'     'CHAR' ''  ''      '',
    w_col_pos 'SRC_LGPLA' 10 'Src S/Bin'      'CHAR' ''  ''      '',
    w_col_pos 'DES_LGTYP' 03 'Des S/Type'     'CHAR' ''  ''      '',
    w_col_pos 'DES_LGPLA' 10 'Des S/Bin'      'CHAR' ''  ''      '',
    w_col_pos 'MESSA'     80 'Message'        'CHAR' ''  ''      ''.
ENDFORM.                    " build_fieldcat

*&---------------------------------------------------------------------*
*&      Form  build_sortcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sortcat.
**--- &1 : position     &2 : field name     &3 : tab name
**--- &4 : up           &5 : sub total
  append_sortcat : '1' 'MATNR' 'IT_ITAB' 'X' ''.
ENDFORM.                    " build_sortcat

*&---------------------------------------------------------------------*
*&      Form  get_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BDCMSGCOLL_MSGID  text
*      -->P_IT_BDCMSGCOLL_MSGNR  text
*      -->P_IT_BDCMSGCOLL_MSGV1  text
*      -->P_IT_BDCMSGCOLL_MSGV2  text
*      -->P_IT_BDCMSGCOLL_MSGV3  text
*      -->P_IT_BDCMSGCOLL_MSGV4  text
*      <--P_L_MESSA  text
*----------------------------------------------------------------------*
FORM get_message USING    p_msgid
                          p_msgnr
                          p_msgv1
                          p_msgv2
                          p_msgv3
                          p_msgv4
                 CHANGING p_l_messa.
*---
  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
       EXPORTING
            msgid               = p_msgid
            msgnr               = p_msgnr
            msgv1               = p_msgv1
            msgv2               = p_msgv2
            msgv3               = p_msgv3
            msgv4               = p_msgv4
       IMPORTING
            message_text_output = p_l_messa.
ENDFORM.                    " get_message

*&---------------------------------------------------------------------*
*&      Form  send_mail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_mail.
*---
  DATA : BEGIN OF as_abaplist OCCURS 0.
          INCLUDE STRUCTURE abaplist.
  DATA : END OF as_abaplist.

  DATA : BEGIN OF as_ali_cont OCCURS 0,
           line(255),
         END OF as_ali_cont.

  DATA : BEGIN OF ms_mailobject_cont OCCURS 0.
          INCLUDE STRUCTURE mcmailobj.
  DATA : END OF ms_mailobject_cont.

  DATA : l_okcode LIKE sy-ucomm.

  CLEAR : as_abaplist, as_ali_cont, ms_mailobject_cont.
  REFRESH : as_abaplist, as_ali_cont, ms_mailobject_cont.

  CALL FUNCTION 'SAVE_LIST'
       EXPORTING
            list_index         = '0'
       TABLES
            listobject         = as_abaplist
       EXCEPTIONS
            list_index_invalid = 1
            OTHERS             = 2.

  CALL FUNCTION 'TABLE_COMPRESS'
       TABLES
            in             = as_abaplist
            out            = as_ali_cont
       EXCEPTIONS
            compress_error = 1
            OTHERS         = 2.

  LOOP AT as_ali_cont.
    MOVE : '1' TO ms_mailobject_cont-objnr,
           '1' TO ms_mailobject_cont-objlevel,
           'ALI' TO ms_mailobject_cont-objtype,
           'Supply to Line' TO ms_mailobject_cont-objnam,
           'Supply to Line' TO ms_mailobject_cont-objdes,
           as_ali_cont-line TO ms_mailobject_cont-objline.
    APPEND ms_mailobject_cont.
  ENDLOOP.

  CALL FUNCTION 'MC_SEND_MAIL'
       EXPORTING
            ms_mail_sendmode          = 'O'
            ms_mail_title             = 'Notification'
            ms_mail_description       = 'Supply to Line Result'
            ms_mail_receiver          = 'STLIMSIS'
*           MS_MAIL_EXPRESS           =
*           MS_MAIL_DLINAME           =
*           MS_MAIL_LANGU             = SY-LANGU
*           MS_MAIL_FUNKOBJ_TYP       = 'R'
*           MS_MAIL_FUNKOBJ_NAME      = 'RMCSMAIL'
       IMPORTING
            ms_ok_code                = l_okcode
       TABLES
            ms_mail_cont              = ms_mailobject_cont
*           MS_MAIL_FUNKOBJ_PARM      =
*           MS_MAIL_SETGET_PARM       =
*           MS_MAIL_RECEIVERS         =
       EXCEPTIONS
            send_error                = 1
            canceled                  = 2
            no_title                  = 3
            no_description            = 4
            no_receiver_or_dli        = 5
            invalid_sendmode          = 6
            no_content                = 7
            invalid_functional_params = 8
            OTHERS                    = 9.
ENDFORM.                    " send_mail

*&---------------------------------------------------------------------*
*&      Form  get_working_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_working_time CHANGING p_1shift_overtime
                               p_2shift_overtime.
*---
  DATA : l_daynr LIKE hrvsched-daynr,
         l_daytxt LIKE hrvsched-daytxt,
         l_dayfree LIKE hrvsched-noday,
         l_current_date TYPE d.

  CONSTANTS : c_uzeit_000000 TYPE t VALUE '000000',
              c_uzeit_035959 TYPE t VALUE '035959'.

  CLEAR : crhd, kako, kazy, it_worktime, it_worktime[], l_daynr,
          l_daytxt, l_dayfree.

*---
  IF sy-uzeit GE c_uzeit_000000 AND sy-uzeit LE c_uzeit_035959.
    l_current_date = sy-datum - 1.
  ELSE.
    l_current_date = sy-datum.
  ENDIF.

  SELECT SINGLE kapid INTO crhd-kapid
                      FROM crhd
                     WHERE objty EQ 'A'
                       AND arbpl EQ 'T'
                       AND werks EQ 'P001'.

  SELECT SINGLE mosid INTO kako-mosid
                      FROM kako
                     WHERE kapid EQ crhd-kapid.

  SELECT SINGLE versn INTO kazy-versn
                      FROM kazy
                     WHERE kapid EQ crhd-kapid
                       AND datub GE l_current_date.

  CALL FUNCTION 'RH_GET_DATE_DAYNAME'
       EXPORTING
            langu               = sy-langu
            date                = l_current_date
       IMPORTING
            daynr               = l_daynr
            daytxt              = l_daytxt
            dayfree             = l_dayfree
       EXCEPTIONS
            no_langu            = 1
            no_date             = 2
            no_daytxt_for_langu = 3
            invalid_date        = 4
            OTHERS              = 5.

  SELECT tagnr
         schnr
         kaptprog
         b~begda
         b~endda
         b~begzt
         b~endzt
               INTO CORRESPONDING FIELDS OF TABLE it_worktime
               FROM kapa AS a INNER JOIN tc37a AS b
                 ON a~mandt EQ b~mandt
                AND a~tprog EQ b~kaptprog
              WHERE schgrup EQ kako-mosid
                AND kapid   EQ crhd-kapid
                AND versn   EQ kazy-versn
                AND begda   LE l_current_date
                AND endda   GE l_current_date
                AND tagnr   EQ l_daynr.

  SORT it_worktime BY tagnr schnr.

*---
  DATA : l_endzt LIKE it_worktime-endzt,
         l_begzt LIKE it_worktime-begzt.

  CLEAR : it_worktime, l_endzt.
  READ TABLE it_worktime WITH KEY schnr = 1.
  MOVE : it_worktime-endzt TO l_endzt.

  CLEAR : it_worktime, l_begzt.
  READ TABLE it_worktime WITH KEY schnr = 2.
  MOVE : it_worktime-begzt TO l_begzt.

  CLEAR : p_1shift_overtime, p_2shift_overtime.

*--- 1 shift overtime
  IF l_endzt EQ l_begzt.
    MOVE : 'X' TO p_1shift_overtime.
  ENDIF.

  CLEAR : it_worktime, l_endzt.
  READ TABLE it_worktime WITH KEY schnr = 2.
  MOVE : it_worktime-endzt TO l_endzt.

*--- 2 shift overtime
  IF l_endzt NE c_time_020000.
    MOVE : 'X' TO p_2shift_overtime.
  ENDIF.
ENDFORM.                    " get_working_time

*&---------------------------------------------------------------------*
*&      Form  decide_times
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM decide_times.
*---
  DATA : it_1shift LIKE it_ztmm_stl_time OCCURS 0 WITH HEADER LINE,
         it_2shift LIKE it_ztmm_stl_time OCCURS 0 WITH HEADER LINE.

  CLEAR : ztmm_stl_exec, ztmm_stl_time, it_ztmm_stl_time, w_tabix,
          it_ztmm_stl_time[], w_max_exec_datetime, w_max_exec_time,
          w_lines,
          it_1shift, it_1shift[], it_2shift, it_2shift[].

  SELECT MAX( dtime ) INTO w_max_exec_datetime
                      FROM ztmm_stl_exec
                     WHERE repid EQ sy-repid.

  IF sy-subrc NE 0.
    MESSAGE e999 WITH text-m01.
  ENDIF.

  MOVE : w_max_exec_datetime+8(6) TO w_max_exec_time.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ztmm_stl_time
           FROM ztmm_stl_time.

  IF sy-subrc NE 0.
    MESSAGE e999 WITH text-m02.
  ENDIF.

  LOOP AT it_ztmm_stl_time.
    MOVE-CORRESPONDING it_ztmm_stl_time TO it_1shift.
    MOVE-CORRESPONDING it_ztmm_stl_time TO it_2shift.
    IF it_ztmm_stl_time-overt EQ '1'.
      MOVE : '0' TO it_1shift-overt.
      APPEND it_1shift.
    ELSEIF it_ztmm_stl_time-overt EQ '2'.
      MOVE : '0' TO it_2shift-overt.
      APPEND it_2shift.
    ENDIF.
    CLEAR : it_ztmm_stl_time, it_1shift, it_2shift.
  ENDLOOP.

  SORT it_ztmm_stl_time BY zindx rtime.

*--- overtime check
  CLEAR : it_worktime, it_worktime[].

  CALL FUNCTION 'Z_FMM_STL_OVERTIME'
       EXPORTING
            i_datum           = sy-datum
            i_uzeit           = sy-uzeit
       TABLES
            t_worktime        = it_worktime
       CHANGING
            e_1shift_overtime = w_1shift_overtime
            e_2shift_overtime = w_2shift_overtime.

  IF w_1shift_overtime EQ space.     " no overtime
    DELETE it_ztmm_stl_time WHERE overt EQ '1'.
  ELSE.     " 1 shift overtime
    LOOP AT it_ztmm_stl_time.
      READ TABLE it_1shift WITH KEY zindx = it_ztmm_stl_time-zindx
                                    rtime = it_ztmm_stl_time-rtime
                                    overt = it_ztmm_stl_time-overt.
      IF sy-subrc EQ 0.
        DELETE it_ztmm_stl_time.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF w_2shift_overtime EQ space.     " no overtime
    DELETE it_ztmm_stl_time WHERE overt EQ '2'.
  ELSE.     " 2 shift overtime
    LOOP AT it_ztmm_stl_time.
      READ TABLE it_2shift WITH KEY zindx = it_ztmm_stl_time-zindx
                                    rtime = it_ztmm_stl_time-rtime
                                    overt = it_ztmm_stl_time-overt.
      IF sy-subrc EQ 0.
        DELETE it_ztmm_stl_time.
      ENDIF.
    ENDLOOP.
  ENDIF.

  DESCRIBE TABLE it_ztmm_stl_time LINES w_lines.

  READ TABLE it_ztmm_stl_time WITH KEY rtime = w_max_exec_time.
  MOVE : sy-tabix TO w_tabix.

  w_tabix = w_tabix + 1.

  IF w_tabix GT w_lines.
    w_tabix = 1.
  ENDIF.

  READ TABLE it_ztmm_stl_time INDEX w_tabix.

  CLEAR : ztmm_stl_exec, w_starting_time, w_ending_time.
  MOVE : it_ztmm_stl_time-stime TO w_starting_time,
         it_ztmm_stl_time-etime TO w_ending_time.

  MOVE : sy-repid TO ztmm_stl_exec-repid.
  CONCATENATE w_current_date it_ztmm_stl_time-rtime
                                              INTO ztmm_stl_exec-dtime.
  ztmm_stl_exec-erdat = ztmm_stl_exec-aedat = sy-datum.
  ztmm_stl_exec-erzet = ztmm_stl_exec-aezet = sy-uzeit.
  ztmm_stl_exec-ernam = ztmm_stl_exec-aenam = sy-uname.

  INSERT ztmm_stl_exec.

*---
*  MOVE : w_current_time TO w_max_exec_time.
  MOVE : it_ztmm_stl_time-rtime TO w_current_time.

*--- overtime check
*  PERFORM get_working_time CHANGING w_1shift_overtime
*                                    w_2shift_overtime.
ENDFORM.                    " decide_times

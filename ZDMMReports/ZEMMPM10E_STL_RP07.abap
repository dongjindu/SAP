************************************************************************
* Program Name      : ZEMMPM10E_STL_RP07
* Author            : Sung-Tae, Lim
* Creation Date     : 2004.04.23.
* Specifications By : Sung-Tae, Lim
* Development Request No : D1K9098555
* Addl Documentation:
* Description       : Supply to Line - RP07
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2004.04.23.     Sung-Tae Lim     D1K9098555     Initial Coding
*
*
************************************************************************

REPORT zemmpm10e_stl_rp07 NO STANDARD PAGE HEADING
                          LINE-SIZE 400
*                          LINE-COUNT 64(1)
                          MESSAGE-ID zmmm.

**---
INCLUDE : zrmmpmxxr_incl.

**---
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
DATA: w_dummy_date     TYPE sy-datum.  "Dummy date for temp use
DATA: w_dummy_minutes  TYPE num03.   "Dummy minutes for Cal of Start D&T

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
       lpmin        LIKE ztmm_mast-lpmin,     " safety stock
*       lpmin        LIKE mlgt-lpmin,      "Min. Storage bin qty
       open_to      LIKE ltap-vsola,      "Open TO
       bferrorqty   LIKE affw-erfmg,      "Backkflush Error Qty
**--- insert bys tlim (2004/04/19)
       bf_to_qty    LIKE ppc1_all-komp_quant,   "to Backflush Qty.
**--- end of insert
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


**--- insert by stlim (2004/04/22)
DATA : BEGIN OF it_bfqty_temp OCCURS 0,
         matnr LIKE ztmm_ppc1_all-matnr,
         werks LIKE ztmm_ppc1_all-werks,
         lgort LIKE ztmm_ppc1_all-lgort,
         komp_quant LIKE ztmm_ppc1_all-komp_quant,
         flg_asynch LIKE ztmm_ppc1_all-flg_asynch,
         sync_ind LIKE ztmm_ppc1_all-sync_ind,
       END OF it_bfqty_temp.

DATA : it_back LIKE it_bfqty_temp OCCURS 0 WITH HEADER LINE.

DATA : w_minus_stock_check(1).
**--- end of insert


**** Selection Screen **************************************************
SELECTION-SCREEN BEGIN OF BLOCK bl_1 WITH FRAME TITLE text-bl1.
PARAMETER: p_cdate LIKE sy-datum  "Current date
                   OBLIGATORY.
PARAMETER: p_ctime LIKE sy-uzeit  "Current time
                   OBLIGATORY.

*PARAMETER: p_rp06  LIKE ztpp_dvrt1-rp06  "For Test
*                   OBLIGATORY.
SELECTION-SCREEN END OF BLOCK bl_1.


**---
INITIALIZATION.
  p_cdate = sy-datum.
  p_ctime = sy-uzeit.

**---
START-OF-SELECTION.
*1. Prepare Data
*2. Create Transfer Order
*3. Change T/O Header Data

* Get Screen Date
  PERFORM get_w_screen_date.
* Get Screen Time
  PERFORM get_w_screen_time.

* Get Current Date & Time
  PERFORM get_w_current_date_time.

* Get data supply to line
  PERFORM get_data_from_table.            "get data

  IF it_toline IS INITIAL.
    MESSAGE s999(zmmm) WITH 'There is no data!'(001).
    EXIT.
  ENDIF.

**--- insert by stlim (2004/04/22)
  PERFORM get_scheduled_bf_qty.
**--- end of insert

  LOOP AT it_toline ASSIGNING <fs_toline>.

*/ Begin OF TO GET Start Date(Feeding Date) and Start Time
    PERFORM time_calculation USING    w_screen_date
                                      w_screen_time
                                      0
*                                      <fs_toline>-ztime
                             CHANGING <fs_toline>-sdate
                                      <fs_toline>-feeding_time.

*/ End OF TO GET Start Date and Start Time

**** 1.5. For Stock Level Check
* Get Available stock(verme) & min. storage bin qty(lpmin).
    PERFORM get_verme_lpmin.

**** 1.6. For Open TO Check (LTAP-VSOLA)(/nLT23)
    PERFORM get_open_to.

**** 1.7. For Backflush Error Qty Check (/nCOGI)
    PERFORM get_bf_error_qty.

**--- insert by stlim (2004/04/19)
**** 1.7.1 check for quantity to backflush
*    PERFORM get_to_backflush.
**--- end of insert

**** 1.8. Rounding Quantity Check
    PERFORM get_rdmng.

**** 1.9. Calculate Target Quantity from the aboves
    CLEAR : w_minus_stock_check.

    PERFORM get_tqty.


    IF w_minus_stock_check EQ space.
***** 1.9.1 Calculate rounding value
*    PERFORM check_rounding_value.

**** 2. Create TO
* App Doc No
      PERFORM number_get_next USING    c_nro_nr_09     "NRO Interval
                                       w_nro_object    "NRO Object
                              CHANGING w_zdocno.     "App Doc No
      COMMIT WORK.

**** Begin of Create TO (/nLT01)
* Get Source Storage type/bin
      PERFORM get_sorce_storage_type_bin.

* Get Destination Storage type/bin
      PERFORM get_des_storage_type_bin.

* BDC Processing of /nLT01
      PERFORM bdc_processing_lt01 TABLES   it_bdcmsgcoll
                                  USING    w_zdocno
                                  CHANGING w_subrc.

**** End of Create TO (/nLT01)
      IF w_subrc = 0.

* 3. Begin of Change TO Header (/nLT1A)
        CLEAR: wa_bdcmsgcoll.
        READ TABLE it_bdcmsgcoll INTO wa_bdcmsgcoll
                                 WITH KEY msgtyp = 'S'.
        CHECK sy-subrc = 0.
        PERFORM bdc_processing_lta1
                         TABLES   it_bdcmsgcoll
                         USING    w_zdocno
                                  wa_bdcmsgcoll-msgv1  "TO number
                         CHANGING w_subrc.
* End of Change TO Header (/nLT1A)
      ENDIF.
    ENDIF.

**** Write List
    PERFORM write_list.
  ENDLOOP.

*---------- List Procession Events ------------------------------------*
TOP-OF-PAGE.
  PERFORM make_col_heading.





*&---------------------------------------------------------------------*
*&      Form  get_w_screen_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_w_screen_date.
  CLEAR: w_screen_date.
  w_screen_date = p_cdate.
ENDFORM.                    " get_w_screen_date

*&---------------------------------------------------------------------*
*&      Form  get_w_screen_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_w_screen_time.
  CLEAR: w_screen_time.
  w_screen_time      = p_ctime.
ENDFORM.                    " get_w_screen_time

*&---------------------------------------------------------------------*
*&      Form  get_w_current_date_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_w_current_date_time.
  CLEAR: w_current_date, w_current_time.
  w_current_date = w_screen_date.
  w_current_time = w_screen_time.
ENDFORM.                    " get_w_current_date_time

*&---------------------------------------------------------------------*
*&      Form  get_data_from_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_from_table.
*---
  DATA: lv_rp06_timestamp(11). "'YYYYMMDDHHMISSX'

  DATA: BEGIN OF ls_rsnum,
         rsnum LIKE ztpp_dvrt1-rsnum,  "Reservation number
        END OF ls_rsnum.

  DATA: lt_rsnum LIKE SORTED TABLE OF ls_rsnum
                 WITH NON-UNIQUE KEY rsnum.

*/ Begin of Added by Hakchin(20040127)
* Get Reservation Numbers
  DATA: lt_supply_info LIKE TABLE OF zspp_vin_info_for_stl.

  CALL FUNCTION 'Z_FPP_GET_SUPPLY_TO_LINE'
*  CALL FUNCTION 'Z_FPP_GET_SUPPLY_TO_LINE_RP'
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
*/ End of Added by Hakchin(20040127)

  FIELD-SYMBOLS: <fs_supply_info> LIKE LINE OF lt_supply_info.

  LOOP AT lt_supply_info ASSIGNING <fs_supply_info>.
    MOVE <fs_supply_info>-rsnum TO ls_rsnum.
    APPEND ls_rsnum TO lt_rsnum.
  ENDLOOP.

* Unique reservation numbers
  DELETE lt_rsnum WHERE rsnum = '0000000000'.

  DELETE ADJACENT DUPLICATES FROM lt_rsnum
                             COMPARING rsnum.

*/ Get Quantity Raw Data(LT_TOLINE) from RESB and ZTMM_MAST
  DATA: BEGIN OF ls_toline.
          INCLUDE STRUCTURE wa_toline.
  DATA:  rsnum LIKE resb-rsnum,
         rspos LIKE resb-rspos,
        END OF ls_toline.

  DATA: lt_toline LIKE SORTED TABLE OF ls_toline
                  WITH NON-UNIQUE KEY matnr.

  FIELD-SYMBOLS: <fs_toline> LIKE LINE OF lt_toline.

  CHECK NOT lt_rsnum IS INITIAL.

  SELECT
         resb~rsnum            "Reservation number
         resb~rspos            "Item number of reservation
         resb~matnr            "Material
         resb~bdmng            "Quantity
         resb~meins            "Unit
         ztmm_mast~feedr       "Feeder
         ztmm_mast~works       "Workstation
         ztmm_mast~rh_lh       "RH/LH
         ztmm_mast~stock_check "STOCK CHECK
         ztmm_mast~feed_cycle  "FEEDING CYCLE
         ztmm_mast~ztime       "TIME
         ztmm_mast~lpmin       " safety stock
              INTO CORRESPONDING FIELDS OF TABLE lt_toline
              FROM resb INNER JOIN ztmm_mast
                ON ztmm_mast~werks = 'P001'       "Plant
               AND ztmm_mast~matnr = resb~matnr   "Material
               AND ztmm_mast~spptl = 'S'             "Supply To Line
               FOR ALL entries IN lt_rsnum
             WHERE resb~rsnum = lt_rsnum-rsnum    "Reservation number
               AND zline IN ('F1', 'F2', 'F3').

*/Get draft data(IT_TOLINE) for TO Creation
  DATA: lv_bdmng     LIKE wa_toline-bdmng.  "For Sum of qty

  LOOP AT lt_toline ASSIGNING <fs_toline>.
    lv_bdmng = lv_bdmng + <fs_toline>-bdmng.
    AT END OF matnr.
      MOVE-CORRESPONDING <fs_toline> TO wa_toline.
      MOVE lv_bdmng TO wa_toline-bdmng.
      APPEND wa_toline TO it_toline.
      CLEAR: lv_bdmng.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " get_data_from_table

*&---------------------------------------------------------------------*
*&      Form  time_calculation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_SCREEN_DATE  text
*      -->P_W_SCREEN_TIME  text
*      -->P_<FS_TOLINE>_ZTIME  text
*      <--P_<FS_TOLINE>_SDATE  text
*      <--P_<FS_TOLINE>_FEEDING_TIME  text
*----------------------------------------------------------------------*
FORM time_calculation USING    value(im_date) TYPE d
                               value(im_time) TYPE t
                               value(im_minutes)
                      CHANGING value(ex_date) TYPE d
                               value(ex_time) TYPE t.
  CLEAR: ex_date, ex_time.
  DATA: lv_time    TYPE t.
  DATA: lv_hoursum TYPE p.

  PERFORM get_time_from_minutes
                   USING    im_minutes
                   CHANGING lv_time.

  lv_hoursum = im_time(2) + lv_time(2).
  ex_date = im_date.
  ex_time = im_time + lv_time.
  IF lv_hoursum >= 24.
    ex_date = ex_date + 1.
  ENDIF.
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
* Get Storage type, Storage bin
  DATA: lv_lgtyp LIKE pkhd-lgtyp. "Dest Storage Type
  DATA: lv_lgpla LIKE pkhd-lgpla. "Dest Storage Bin

  SELECT SINGLE lgtyp
                lgpla INTO (lv_lgtyp, lv_lgpla)
                      FROM pkhd
                     WHERE matnr =  <fs_toline>-matnr.

* Get verme (Available stock)
**--- blocked by stlim (2004/04/15)
*  SELECT SUM( verme ) AS verme INTO <fs_toline>-verme
*                               FROM lqua
*                              WHERE matnr =  <fs_toline>-matnr
*                                AND lgtyp = lv_lgtyp   " storage type
*                                AND lgpla = lv_lgpla   " Storage Bin
*                              GROUP by matnr.
*  ENDSELECT.
**--- end of block

**--- insert by stlim (2004/04/15)
  SELECT SUM( gesme ) AS verme INTO <fs_toline>-verme
                               FROM lqua
                              WHERE matnr =  <fs_toline>-matnr
                                AND lgtyp = lv_lgtyp   " storage type
                                AND lgpla = lv_lgpla   " Storage Bin
                              GROUP by matnr.
  ENDSELECT.
**--- end of insert

**--- blocked by stlim (2004/04/22)
** Select MLGT-LPMIN  "Minimum storage bin quantity
*  SELECT SINGLE lpmin   "Minimum storage bin quantity
*                      INTO <fs_toline>-lpmin
*                      FROM mlgt
*                     WHERE matnr = <fs_toline>-matnr
*                       AND lvorm = space.     " Deletion flag
**--- end of block
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
  SELECT SUM( vsola ) AS vsola
                             "Source target quantity in alternate unit
                      INTO <fs_toline>-open_to
                      FROM ltap  "Transfer order item
                     WHERE pquit = space
                             "Open TO(Indicator: confirmation complete)
                       AND matnr = <fs_toline>-matnr       "Material
                     GROUP by matnr.
  ENDSELECT.
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
  DATA: lv_err_qty     LIKE affw-erfmg.  "Error Qty
  DATA: lv_err_qty_tmp LIKE affw-erfmg.  "Tmp Error Qty
  DATA: lv_weblpos     LIKE affw-weblpos."Counter
  DATA: lv_erfmg       LIKE affw-erfmg.  "Quantity in unit of entry

  CLEAR: lv_err_qty_tmp, lv_err_qty.
  CLEAR: lv_weblpos, lv_erfmg.

*--- blocked by stlim (2004/04/15)
*  SELECT weblpos
*         erfmg   INTO (lv_weblpos, lv_erfmg)
*                 FROM affw
*                        "Goods movements with errors from confirmations
*                WHERE matnr = <fs_toline>-matnr
*                  AND bwart = '261'.     " Error case
*    lv_err_qty_tmp = lv_weblpos * lv_erfmg.
*    lv_err_qty = lv_err_qty + lv_err_qty_tmp.
*  ENDSELECT.
*  <fs_toline>-bferrorqty = lv_err_qty.  "Backflush Error Qty
*  " This time, Error Quantity = Counter * Quantity in UnE
*  " i.e. Error Quantity = WEBLPOS * ERFMG.
**--- end of block

**--- insert by stlim (2004/04/15)
  CLEAR : affw.
  SELECT SUM( erfmg ) INTO <fs_toline>-bferrorqty
                      FROM affw
                     WHERE werks EQ 'P001'
                       AND lgort EQ 'P400'
                       AND matnr EQ <fs_toline>-matnr
                       AND bwart EQ '261'.
**--- end of insert
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
  SELECT SINGLE rdmng INTO <fs_toline>-rdmng
                      FROM mlgt   "Material Data for Each Storage Type
                     WHERE matnr = <fs_toline>-matnr
                       AND lvorm = space.
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
  DATA: lv_tqty LIKE resb-bdmng.  "Target Quantity.

  lv_tqty = <fs_toline>-bdmng.

**--- insert by stlim (2004/04/19) : current stock     ¨ç
**--- insert by stlim (2004/04/22) : stock level check
  IF <fs_toline>-stock_check = 'X'.
    lv_tqty = lv_tqty - <fs_toline>-verme.
  ENDIF.
**--- end of insert

**--- blocked by stlim (2004/04/22)
**/( 1.5. Stock Level Check )
** If Available stock < min. storage bin qty
** Add min. storage bin qty to planned qty
*  IF <fs_toline>-stock_check = 'X'.
*    IF <fs_toline>-verme < <fs_toline>-lpmin.
*      lv_tqty = lv_tqty + <fs_toline>-lpmin.
*    ENDIF.
*  ENDIF.
**--- end of block

**--- insert by stlim (2004/04/22) : safety stock     ¨è
  IF <fs_toline>-stock_check = 'X'.
    lv_tqty = lv_tqty + <fs_toline>-lpmin.
  ENDIF.
**--- end of insert

*/( 1.6. Open T/O Check )
* If there are Open T/O, add Open T/O to planned qty
**--- blocked by stlim (2004/04/19)
*  lv_tqty = lv_tqty + <fs_toline>-open_to.
**--- end of block

**--- insert by stlim (2004/04/19) : Open quantity     ¨é
**--- insert by stlim (2004/04/22) : stock level check
  IF <fs_toline>-stock_check = 'X'.
    lv_tqty = lv_tqty - <fs_toline>-open_to.
  ENDIF.
**--- end of insert

*/( 1.7. Backflush Error Qty Check (/nCOGI) )
* If there are Backflush Error Qty,
* subtract Backflush Error Qty from planned qty
**--- blocked by stlim (2004/04/19)
*  lv_tqty = lv_tqty - <fs_toline>-bferrorqty.
**--- end of block

**--- insert by stlim (2004/04/19) : backflush error quantity     ¨ê
**--- insert by stlim (2004/04/22) : stock level check
  IF <fs_toline>-stock_check = 'X'.
    lv_tqty = lv_tqty + <fs_toline>-bferrorqty.
  ENDIF.
**--- end of insert

**--- insert by stlim (2004/04/19) : scheduled backflush quantity     ¨ë
**--- insert by stlim (2004/04/22) : stock level check
  IF <fs_toline>-stock_check = 'X'.
    CLEAR : it_back.
    READ TABLE it_back WITH KEY matnr = <fs_toline>-matnr.
    MOVE : it_back-komp_quant TO <fs_toline>-bf_to_qty.
    lv_tqty = lv_tqty + <fs_toline>-bf_to_qty.
  ENDIF.

  IF lv_tqty LT 1.
    MOVE : 'X' TO w_minus_stock_check.
  ENDIF.
**--- end of insert

  IF w_minus_stock_check EQ space.
*/( 1.8. Rounding Quantity Check )
* If there is Rounding Quantity,
* planned order qty is to be least multiple of Rounding Quantity.

*A. Get Remainder  :mod
*B. Get quotient   :div
    DATA: lv_tqtymodrdmng TYPE p.
    DATA: lv_tqtydivrdmng TYPE p.

    IF NOT <fs_toline>-rdmng IS INITIAL.
      lv_tqtymodrdmng = lv_tqty MOD <fs_toline>-rdmng.
      lv_tqtydivrdmng = lv_tqty DIV <fs_toline>-rdmng.
    ENDIF.

    IF NOT lv_tqtymodrdmng IS INITIAL.
      lv_tqtydivrdmng = lv_tqtydivrdmng + 1.
      lv_tqty = lv_tqtydivrdmng * <fs_toline>-rdmng.
    ENDIF.
    <fs_toline>-tqty = lv_tqty.   "Target Qty

  ELSE.

    <fs_toline>-tqty = 0.

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
  "Storage Type
  "Storage bin
  SELECT SINGLE lgtyp lgpla
    INTO (<fs_toline>-src_lgtyp, <fs_toline>-src_lgpla)
    FROM mlgt
    WHERE matnr = <fs_toline>-matnr AND
          lvorm = space.
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
  SELECT SINGLE lgtyp lgpla
    INTO (<fs_toline>-des_lgtyp, <fs_toline>-des_lgpla)
    FROM pkhd
    WHERE matnr =  <fs_toline>-matnr.

* To enable manual error occurrence in /nLT01.
  IF sy-subrc <> 0.
    <fs_toline>-des_lgtyp = 'XXX'.
    <fs_toline>-des_lgpla = 'XXXXXXXXXX'.
  ENDIF.
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
  CLEAR: ext_bdcmsgcoll, ext_bdcmsgcoll[], p_subrc.

  DATA:
       lv_bwlvs_002     TYPE bdcdata-fval,   "Movement type
       lv_matnr_003     TYPE bdcdata-fval,
       lv_anfme_004     TYPE bdcdata-fval,
       lv_anfme_007     TYPE bdcdata-fval,
       lv_altme_008     TYPE bdcdata-fval,
       lv_vltyp_009     TYPE bdcdata-fval,
       lv_vlpla_010     TYPE bdcdata-fval,
       lv_nltyp_011     TYPE bdcdata-fval,
       lv_nlpla_012     TYPE bdcdata-fval,
       lv_refnr_013     TYPE bdcdata-fval.   "Group(Feeder)

*&<<<<-----------------------------------------------------------------*
* block by stlim - 2004/02/05 - begin
*&<<<<-----------------------------------------------------------------*
*  IF <fs_toline>-src_lgpla = '422'. "Source Storage type
*    lv_bwlvs_002 = '850'.  "Movement type
*  ELSE.
*    lv_bwlvs_002 = '999'.  "Movement type
*  ENDIF.
*&<<<<-----------------------------------------------------------------*
* block by stlim - 2004/02/05 - end
*&<<<<-----------------------------------------------------------------*

*&<<<<-----------------------------------------------------------------*
* insert by stlim - 2004/02/05 - begin
*&<<<<-----------------------------------------------------------------*
*  IF <fs_toline>-src_lgpla = '422'. "Source Storage type
  lv_bwlvs_002 = '850'.  "Movement type
*  ELSE.
*    lv_bwlvs_002 = '999'.  "Movement type
*  ENDIF.
*&<<<<-----------------------------------------------------------------*
* insert by stlim - 2004/02/05 - end
*&<<<<-----------------------------------------------------------------*

  lv_refnr_013 = <fs_toline>-feedr. "Group(Feeder)

  lv_matnr_003  = <fs_toline>-matnr. "Material '327003K100'

  lv_anfme_004  = <fs_toline>-tqty.
  lv_anfme_007  = <fs_toline>-tqty.
  lv_altme_008  = <fs_toline>-meins.
  lv_vltyp_009  = <fs_toline>-src_lgtyp.  "Src Storage Type '434'
  lv_vlpla_010  = <fs_toline>-src_lgpla.  "Src Storage Bin  'AA-01-11'
  lv_nltyp_011  = <fs_toline>-des_lgtyp.  "Des Storage Type '443'
  lv_nlpla_012  = <fs_toline>-des_lgpla.  "Des Storage Bin  'TS-01'


  CONDENSE:
            lv_bwlvs_002,  "Movement type
            lv_matnr_003,
            lv_anfme_004,
            lv_anfme_007,
            lv_altme_008,
            lv_vltyp_009,
            lv_vlpla_010,
            lv_nltyp_011,
            lv_nlpla_012,
            lv_refnr_013.

*BDC for LT01(Create TO)
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

**--- Insert Log Table
*  CALL FUNCTION 'Z_FMM_STL_LOG' IN UPDATE TASK
*       EXPORTING
*            im_zdocno      = p_zdocno
*            im_ztcode      = sy-tcode
*            im_zprogramm   = sy-cprog
*            im_matnr       = lv_matnr_003
*            im_werks       = 'P001'
*            im_bdmng       = lv_anfme_007
*            im_meins       = lv_altme_008
*       TABLES
*            imt_bdcmsgcoll = ext_bdcmsgcoll[].

**** (Begin)BDC Log to the table ZTLOG
  CALL FUNCTION 'Z_FMM_6001_03_LOG_TO_ZTABLE' IN UPDATE TASK
       EXPORTING
            im_zdocno      = p_zdocno
            im_ztcode      = sy-tcode
            im_zprogramm   = sy-cprog
       TABLES
            imt_bdcmsgcoll = ext_bdcmsgcoll[].

  COMMIT WORK.
**** (End)BDC Log to the table ZTLOG
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
  CLEAR: ext_bdcmsgcoll, ext_bdcmsgcoll[], p_subrc.

  DATA:
     lv_tanum_001     TYPE bdcdata-fval,  "TO number
     lv_lgnum_002     TYPE bdcdata-fval,  "Warehouse number
     lv_stdat_003     TYPE bdcdata-fval,  "Start date
     lv_stuzt_004     TYPE bdcdata-fval,  "Start time
     lv_endat_005     TYPE bdcdata-fval,  "End date
     lv_enuzt_006     TYPE bdcdata-fval.  "End time

**** Begin of Adjust Date format in user by user
  DATA: lv_date(8). CLEAR: lv_date.

  PERFORM user_date_format
             USING    sy-uname
                      <fs_toline>-sdate                     "'99991231'
             CHANGING lv_date.

  lv_stdat_003 = lv_date.        "Start date
**** End of Adjust Date format in user by user
  lv_tanum_001 = p_msgv1.                              "TO number  '813'
  lv_lgnum_002 = 'P01'.                                "Warehouse number
  lv_stuzt_004 = <fs_toline>-feeding_time.             "Start time

  PERFORM time_calculation USING    <fs_toline>-sdate
                                    <fs_toline>-feeding_time
                                    60   "Minutes
                           CHANGING <fs_toline>-edate
                                    <fs_toline>-etime.

  lv_enuzt_006 = <fs_toline>-etime. "End time

**** Begin of Adjust Date format in user by user
  CLEAR: lv_date.
  PERFORM user_date_format
             USING    sy-uname
                      <fs_toline>-edate                     "'99991231'
             CHANGING lv_date.
  lv_endat_005 = lv_date.      "End date
**** End of Adjust Date format in user by user

  CONDENSE:
     lv_tanum_001,
     lv_lgnum_002,
     lv_stdat_003,
     lv_stuzt_004,
     lv_endat_005,
     lv_enuzt_006.

*BDC for LTA1(Change TO Header)
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

**** (Begin)BDC Log to the table ZTLOG
  IF ext_bdcmsgcoll[] IS INITIAL.  "SUCCESS
    CLEAR: wa_bdcmsgcoll.
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

    MESSAGE s999(zmmm)
                WITH wa_bdcmsgcoll-msgv1
                     wa_bdcmsgcoll-msgv2
                     wa_bdcmsgcoll-msgv3
                     wa_bdcmsgcoll-msgv4.
  ENDIF.

**--- Insert Log Table
*  CALL FUNCTION 'Z_FMM_STL_LOG' IN UPDATE TASK
*       EXPORTING
*            im_zdocno      = p_zdocno
*            im_ztcode      = sy-tcode
*            im_zprogramm   = sy-cprog
*       TABLES
*            imt_bdcmsgcoll = ext_bdcmsgcoll[].

  CALL FUNCTION 'Z_FMM_6001_03_LOG_TO_ZTABLE' IN UPDATE TASK
       EXPORTING
            im_zdocno      = p_zdocno
            im_ztcode      = sy-tcode
            im_zprogramm   = sy-cprog
       TABLES
            imt_bdcmsgcoll = ext_bdcmsgcoll[].

  COMMIT WORK.
**** (End)BDC Log to the table ZTLOG
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
*---
  WRITE:
    /1(18)  <fs_toline>-matnr RIGHT-JUSTIFIED,
     20(10) <fs_toline>-bdmng
                             UNIT <fs_toline>-meins,
     31     <fs_toline>-meins,
     36     <fs_toline>-works,
     50     <fs_toline>-rh_lh,
     60     <fs_toline>-feeding_time,  "Start time(Feeding Time)
     75     <fs_toline>-stock_check,
     90     <fs_toline>-feed_cycle,
     115    <fs_toline>-ztime,
     155    <fs_toline>-verme       "Available stock
                             UNIT <fs_toline>-meins,
     180    <fs_toline>-lpmin       "Minimum storage bin quantity
                             UNIT <fs_toline>-meins,
     220    <fs_toline>-open_to     "Open TO
                             UNIT <fs_toline>-meins,
     240    <fs_toline>-bferrorqty  "Error Quantity
                             UNIT <fs_toline>-meins,
     270    <fs_toline>-bf_to_qty   "backflush qty
                             UNIT <fs_toline>-meins,
     300    <fs_toline>-rdmng       "Rounding Quantity
                             UNIT <fs_toline>-meins,
     325    <fs_toline>-tqty        "Last Quanty
                             UNIT <fs_toline>-meins,
     355    <fs_toline>-feedr,      "Feeder
     365    wa_bdcmsgcoll-msgv1 COLOR COL_POSITIVE.
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
  CLEAR: ex_time.
  DATA: BEGIN OF ls_time,
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
  CLEAR: p_userdate.
  DATA: yyyy(4).  "year
  DATA: mm(2).    "day
  DATA: dd(2).    "month
  DATA: datfm LIKE usr01-datfm.  "date format

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
*&      Form  get_to_backflush
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_to_backflush.
**---
*  CLEAR : ppc1_all.
*
*  SELECT SUM( komp_quant ) INTO <fs_toline>-bf_to_qty
*                           FROM ppc1_all
*                          WHERE matnr EQ <fs_toline>-matnr
*                            AND werks EQ 'P001'
*                            AND lgort EQ 'P400'
*                            AND flg_asynch EQ space.
ENDFORM.                    " get_to_backflush

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
         komp_quant
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
*&      Form  check_rounding_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_rounding_value.
*---

ENDFORM.                    " check_rounding_value

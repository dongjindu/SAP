*----------------------------------------------------------------------*
*   INCLUDE ZEMMGM12E_6006F01                                          *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  get_data_from_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_from_table.
  SELECT matnr
         SUM( bdmng )       AS bdmng
         MAX( meins )       AS meins
         MAX( feedr )       AS feedr
         MAX( works )       AS works
         MAX( rh_lh )       AS rh_lh
         MAX( stock_check ) AS stock_check
         MAX( feed_cycle )  AS feed_cycle
         MAX( ztime )       AS ztime
*    INTO CORRESPONDING FIELDS OF TABLE it_zvmm_6012_02
    INTO CORRESPONDING FIELDS OF TABLE it_toline
    FROM zvmm_6012_02
    WHERE spptl = 'S'      AND  "Supply To Line
          plnum IN s_plnum   "Planned order number
    GROUP BY MATNR.
ENDFORM.                    " get_data_from_table
*&---------------------------------------------------------------------*
*&      Form  make_col_heading
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_col_heading.
  WRITE: /1   'Material',
          20(9) 'Quantity' RIGHT-JUSTIFIED,
          31  'Unit',
          36  'Workstation',
          50  'RH/LH',
          60  'Feeding Time',
          75  'Stock Check',
          90  'Feed Cycle',
          115  'Time for STL',
          155  'Available stock(VERME)',
          180 'Minimum storage bin quantity(LPMIN)',
          220 'Open TO',
          240 'Backflush Error Check',
          270 'Rounding Quantity Check',
          300 'Target Quantity',
          325 'Feeder'.
ENDFORM.                    " make_col_heading
*&---------------------------------------------------------------------*
*&      Form  get_present_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_w_present_time.
  CLEAR: w_present_time.
  w_present_time = sy-uzeit.
  CLEAR: w_present_time+2(4).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  get_verme_lpmin
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_verme_lpmin.
* Get Storage type, Storage bin
  DATA: lv_lgtyp LIKE pkhd-lgtyp. "Dest Storage Type
  DATA: lv_lgpla LIKE pkhd-lgpla. "Dest Storage Bin
  SELECT SINGLE lgtyp lgpla
    INTO (lv_lgtyp, lv_lgpla)
    FROM pkhd
    WHERE matnr =  <fs_toline>-matnr.
* Get verme (Available stock)
  SELECT
         SUM( verme ) AS verme
    INTO <fs_toline>-verme
    FROM lqua
    WHERE matnr =  <fs_toline>-matnr AND
          lgtyp = lv_lgtyp           AND "Storage Type
          lgpla = lv_lgpla               "Storage Bin
    GROUP by matnr.
  ENDSELECT.

* Select MLGT-LPMIN  "Minimum storage bin quantity
  SELECT SINGLE lpmin   "Minimum storage bin quantity
    INTO <fs_toline>-lpmin
    FROM mlgt
    WHERE matnr = <fs_toline>-matnr AND
          lvorm = space. "Deletion flag
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
  SELECT
      SUM( vsola ) AS vsola  "Source target quantity in alternate unit
    INTO <fs_toline>-open_to
    FROM ltap  "Transfer order item
    WHERE pquit = space AND "Open TO(Indicator: confirmation complete)
          matnr = <fs_toline>-matnr  "Material
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
  DATA: lv_err_qty     LIKE affw-erfmg.  "Error Qty
  DATA: lv_err_qty_tmp LIKE affw-erfmg.  "Tmp Error Qty
  DATA: lv_weblpos     LIKE affw-weblpos."Counter
  DATA: lv_erfmg       LIKE affw-erfmg.  "Quantity in unit of entry

  CLEAR: lv_err_qty_tmp, lv_err_qty.
  CLEAR: lv_weblpos, lv_erfmg.

  SELECT weblpos erfmg INTO (lv_weblpos, lv_erfmg)
    FROM affw        "Goods movements with errors from confirmations
    WHERE matnr = <fs_toline>-matnr AND
          bwart = '261'.  "Error case

    lv_err_qty_tmp = lv_weblpos * lv_erfmg.
    lv_err_qty = lv_err_qty + lv_err_qty_tmp.
  ENDSELECT.
  <fs_toline>-bferrorqty = lv_err_qty.  "Backflush Error Qty
  " This time, Error Quantity = Counter * Quantity in UnE
  " i.e. Error Quantity = WEBLPOS * ERFMG.
ENDFORM.                    " get_bf_error_qty
*&---------------------------------------------------------------------*
*&      Form  get_RDMNG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_rdmng.
  SELECT SINGLE rdmng INTO <fs_toline>-rdmng
    FROM mlgt   "Material Data for Each Storage Type
    WHERE matnr = <fs_toline>-matnr AND
          lvorm = space.
ENDFORM.                    " get_RDMNG
*&---------------------------------------------------------------------*
*&      Form  get_tqty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_tqty.
  DATA: lv_tqty LIKE resb-bdmng.  "Target Quantity.
  lv_tqty = <fs_toline>-bdmng.

*/( 1.5. Stock Level Check )
* If Available stock < min. storage bin qty
* Add min. storage bin qty to planned qty
  IF <fs_toline>-stock_check = 'X'.
    IF <fs_toline>-verme < <fs_toline>-lpmin.
      lv_tqty = lv_tqty + <fs_toline>-lpmin.
    ENDIF.
  ENDIF.

*/( 1.6. Open T/O Check )
* If there are Open T/O, add Open T/O to planned qty
  lv_tqty = lv_tqty + <fs_toline>-open_to.

*/( 1.7. Backflush Error Qty Check (/nCOGI) )
* If there are Backflush Error Qty,
* subtract Backflush Error Qty from planned qty
  lv_tqty = lv_tqty - <fs_toline>-bferrorqty.

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
ENDFORM.                    " get_tqty
*&---------------------------------------------------------------------*
*&      Form  write_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_list.
  WRITE:
    /1(18)  <fs_toline>-matnr RIGHT-JUSTIFIED,
     20(10) <fs_toline>-bdmng
                             UNIT <fs_toline>-meins,
     31     <fs_toline>-meins,
     36     <fs_toline>-works,
     50     <fs_toline>-rh_lh,
     60     <fs_toline>-feeding_time,
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
     270    <fs_toline>-rdmng       "Rounding Quantity
                             UNIT <fs_toline>-meins,
     300    <fs_toline>-tqty        "Last Quanty
                             UNIT <fs_toline>-meins,
     325    <fs_toline>-feedr.      "Feeder
ENDFORM.                    " write_list
*&---------------------------------------------------------------------*
*&      Form  number_get_next
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_c_nro_nr_09  text
*      -->P_w_nro_OBJECT  text
*      <--P_W_ZDOCNO  text
*----------------------------------------------------------------------*
FORM number_get_next
           USING    value(p_w_nro_interval) LIKE inri-nrrangenr
                    value(p_w_nro_object)   LIKE inri-object
           CHANGING value(p_w_nro_next).
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
  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    "number_get_next
*&---------------------------------------------------------------------*
*&      Form  bdc_processing_lt01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BDCMSGCOLL  text
*      -->P_W_ZDOCNO  text
*      <--P_W_SUBRC  text
*----------------------------------------------------------------------*
FORM bdc_processing_lt01
                        TABLES ext_bdcmsgcoll
                                STRUCTURE bdcmsgcoll
                        USING    value(p_zdocno)
                        CHANGING value(p_subrc).
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

  IF <fs_toline>-src_lgpla = '422'. "Source Storage type
    lv_bwlvs_002 = '850'.
  ELSE.
**--- blocked by stlim (2004/04/13)
*    lv_bwlvs_002 = '999'.
**--- end of block
**--- insert by stlim (2004/04/13)
    lv_bwlvs_002 = '977'.
**--- end of insert
  ENDIF.

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
*   CTU           = 'X'
*   MODE          = 'N'
*   UPDATE        = 'L'
*   GROUP         =
*   USER          =
*   KEEP          =
*   HOLDDATE      =
*   NODATA        = '/'
    lgnum_001     = 'P01'             "Warehouse number
    refnr_013     = lv_refnr_013      "Group(Feeder)
    bwlvs_002     = lv_bwlvs_002      "Movement type '999' -> '977'
    matnr_003     = lv_matnr_003      "Material '327003K100'
*     anfme_004     = '1'
    anfme_004     = lv_anfme_004
    werks_005     = 'P001'            "Plant
    lgort_006     = 'P400'            "Storage Location
*    anfme_007     = '1'
     anfme_007     = lv_anfme_007
*     altme_008     = 'EA'
    altme_008     = lv_altme_008
    vltyp_009     = lv_vltyp_009  "Src Storage Type '434'
    vlpla_010     = lv_vlpla_010  "Src Storage Bin  'AA-01-11'
    nltyp_011     = lv_nltyp_011  "Des Storage Type '443'
    nlpla_012     = lv_nlpla_012  "Des Storage Bin  'TS-01'
    IMPORTING
      subrc         = p_subrc
    TABLES
      messtab       = ext_bdcmsgcoll[].

**** (Begin)BDC Log to the table ZTLOG
  CALL FUNCTION 'Z_FMM_6001_03_LOG_TO_ZTABLE'
    IN UPDATE TASK
    EXPORTING
      im_zdocno            = p_zdocno
      im_ztcode            = sy-tcode
      im_zprogramm         = sy-cprog
*            IM_TCODE             =
*            IM_FM_NAME           =
   TABLES
     imt_bdcmsgcoll       = ext_bdcmsgcoll[]
*           IMT_BAPIRET2         =
            .
  COMMIT WORK.
**** (End)BDC Log to the table ZTLOG

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  get_sorce_storage_type_bin
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_sorce_storage_type_bin.
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
  SELECT SINGLE lgtyp lgpla
    INTO (<fs_toline>-des_lgtyp, <fs_toline>-des_lgpla)
    FROM pkhd
    WHERE matnr =  <fs_toline>-matnr.
ENDFORM.                    " get_des_storage_type_bin
*&---------------------------------------------------------------------*
*&      Form  bdc_processing_lta1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BDCMSGCOLL  text
*      -->P_W_ZDOCNO  text
*      <--P_W_SUBRC  text
*----------------------------------------------------------------------*
FORM bdc_processing_lta1
                        TABLES ext_bdcmsgcoll
                                STRUCTURE bdcmsgcoll
                        USING    value(p_zdocno)
                                 value(p_msgv1)
                        CHANGING value(p_subrc).
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
*  lv_time      = <fs_toline>-feeding_time + c_onehour. "End time

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
*   CTU             = 'X'
*   MODE            = 'N'
*   UPDATE          = 'L'
*   GROUP           =
*   USER            =
*   KEEP            =
*   HOLDDATE        =
*   NODATA          = '/'
     tanum_001       = lv_tanum_001    " TO number  '813'
     lgnum_002       = lv_lgnum_002
     stdat_003       = lv_stdat_003    "Start date
     stuzt_004       = lv_stuzt_004    "Start time  "'10:36:48'
     endat_005       = lv_endat_005    "End date
     enuzt_006       = lv_enuzt_006    "End time
      IMPORTING
        subrc         = p_subrc
      TABLES
        messtab       = ext_bdcmsgcoll[].

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


  CALL FUNCTION 'Z_FMM_6001_03_LOG_TO_ZTABLE'
    IN UPDATE TASK
    EXPORTING
      im_zdocno            = p_zdocno
      im_ztcode            = sy-tcode
      im_zprogramm         = sy-cprog
*            IM_TCODE             =
*            IM_FM_NAME           =
   TABLES
     imt_bdcmsgcoll       = ext_bdcmsgcoll[]
*           IMT_BAPIRET2         =
            .
  COMMIT WORK.
**** (End)BDC Log to the table ZTLOG

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  user_date_format
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_UNAME  text
*      -->P_SY_DATUM  text
*      <--P_LV_C_TODAY  text
*----------------------------------------------------------------------*
FORM user_date_format USING    value(p_user)     LIKE sy-uname
                               value(p_date)     LIKE sy-datum
                      CHANGING value(p_userdate) TYPE char8.
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
*&      Form  get_w_present_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_w_present_date.
  CLEAR: w_present_date.
  w_present_date = sy-datum.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  get_time_from_minutes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_TOLINE>_FEED_CYCLE  text
*      <--P_W_CAL_TIME  text
*----------------------------------------------------------------------*
FORM get_time_from_minutes
                 USING    value(im_minutes)
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
ENDFORM.                    "get_time_from_minutes
*&---------------------------------------------------------------------*
*&      Form  time_calculation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_PRESENT_DATE  text
*      -->P_WA_PRESENT_TIME  text
*      -->P_<FS_TOLINE>_FEED_CYCLE  text
*      <--P_<FS_TOLINE>_SDATE  text
*      <--P_<FS_TOLINE>_FEEDING_TIME  text
*----------------------------------------------------------------------*
FORM time_calculation USING    value(im_date) TYPE d
                               value(im_time) TYPE t
                               value(im_minutes)
                      CHANGING value(ex_date) TYPE d
                               value(ex_time) TYPE t.

  CLEAR: ex_date, ex_time.
  DATA: lv_time TYPE t.
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
ENDFORM.                    "time_calculation

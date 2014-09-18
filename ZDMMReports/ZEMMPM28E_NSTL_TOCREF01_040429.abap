*----------------------------------------------------------------------*
*   INCLUDE ZEMMPM28E_NSTL_TOCREF01                                    *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  get_it_ztmm_nstl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_it_ztmm_nstl.
  DATA: lv_nstl_date TYPE sy-datum.

*  lv_nstl_date = sy-datum.
  lv_nstl_date = p_tocred.
* NSTL TO Creation Day 06:00:00 ~ Next Day 03:59:59
  SELECT ztmm_nstl~matnr  AS matnr
         ztmm_nstl~time01 AS time01
         ztmm_nstl~time02 AS time02
         ztmm_nstl~time03 AS time03
         ztmm_nstl~time04 AS time04
         ztmm_nstl~time05 AS time05
         ztmm_nstl~time06 AS time06
         ztmm_nstl~time07 AS time07
         ztmm_nstl~time08 AS time08
         ztmm_nstl~time09 AS time09
         ztmm_nstl~time10 AS time10
         ztmm_nstl~time11 AS time11
         ztmm_nstl~time12 AS time12
         ztmm_nstl~time13 AS time13
         ztmm_nstl~time14 AS time14
         ztmm_nstl~time15 AS time15
         ztmm_nstl~time16 AS time16
         ztmm_nstl~time17 AS time17
         ztmm_nstl~time18 AS time18
         ztmm_nstl~time19 AS time19
         ztmm_nstl~time20 AS time20
*/Begin of Added by Hakchin(20040131)
         ztmm_nstl~time20 AS time21
         ztmm_nstl~time20 AS time22
*         ztmm_nstl~time20 AS time23
*         ztmm_nstl~time20 AS time24
*/End of Added by Hakchin(20040131)
         ztmm_nstl~meins  AS meins
         ztmm_mast~feedr  AS feedr
         ztmm_mast~feed_cycle AS feed_cycle
    INTO CORRESPONDING FIELDS OF TABLE it_ztmm_nstl
    FROM ztmm_nstl
    INNER JOIN ztmm_mast
      ON ztmm_mast~matnr = ztmm_nstl~matnr AND  "Material
         ztmm_mast~spptl = 'N' "Non Supply To Line
*/Begin of Added by Hakchin(20040131)
    WHERE datum = lv_nstl_date.
*/End of Added by Hakchin(20040131)
ENDFORM.                             " get_it_ztmm_nstl
*&---------------------------------------------------------------------*
*&      Form  get_rdmng
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_rdmng USING    value(im_matnr)
               CHANGING value(ex_rdmng) TYPE mlgt-rdmng. "Rounding qty
  CLEAR: ex_rdmng.

  SELECT SINGLE rdmng INTO ex_rdmng
    FROM mlgt   "Material Data for Each Storage Type
    WHERE matnr = im_matnr AND
          lvorm = space.
  "Deletion flag for all material data of a storage type
ENDFORM.                    " get_rdmng
*&---------------------------------------------------------------------*
*&      Form  get_sorce_storage_type_bin
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_sorce_storage_type_bin
                 USING    value(im_matnr)
                 CHANGING value(ex_src_lgtyp)  "Storage Type
                          value(ex_src_lgpla). "Storage bin
  CLEAR: ex_src_lgtyp, ex_src_lgpla.
  SELECT SINGLE lgtyp lgpla
    INTO (ex_src_lgtyp, ex_src_lgpla)
    FROM mlgt
    WHERE matnr = im_matnr AND
          lvorm = space.
ENDFORM.                    " get_sorce_storage_type_bin
*&---------------------------------------------------------------------*
*&      Form  get_des_storage_type_bin
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_MATNR_DATE_TIME_MATNR  text
*      <--P_WA_MATNR_DATE_TIME_DES_LGTYP  text
*      <--P_WA_MATNR_DATE_TIME_DES_LGPLA  text
*----------------------------------------------------------------------*
FORM get_des_storage_type_bin USING    value(im_matnr)
                              CHANGING value(ex_des_lgtyp)
                                       value(ex_des_lgpla).
  CLEAR: ex_des_lgtyp, ex_des_lgpla.
  SELECT SINGLE lgtyp lgpla
    INTO (ex_des_lgtyp, ex_des_lgpla)
    FROM pkhd
    WHERE matnr = im_matnr.
ENDFORM.                    " get_des_storage_type_bin
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
           USING    value(p_nro_interval) LIKE inri-nrrangenr
                    value(p_nro_object)   LIKE inri-object
           CHANGING value(p_nro_next).
  CLEAR: p_nro_next.
  CALL FUNCTION 'NUMBER_GET_NEXT'
       EXPORTING
            nr_range_nr             = p_nro_interval
            object                  = p_nro_object
       IMPORTING
            number                  = p_nro_next
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

*&<<<<-----------------------------------------------------------------*
* block by stlim - 2004/02/05 - begin
*&<<<<-----------------------------------------------------------------*
*  IF <fs_data_for_to>-src_lgpla = '422'. "Source Storage type
*    lv_bwlvs_002 = '850'.
*  ELSE.
*    lv_bwlvs_002 = '999'.
*  ENDIF.
*&<<<<-----------------------------------------------------------------*
* block by stlim - 2004/02/05 - end
*&<<<<-----------------------------------------------------------------*

*&<<<<-----------------------------------------------------------------*
* insert by stlim - 2004/02/05 - begin
*&<<<<-----------------------------------------------------------------*
*  IF <fs_data_for_to>-src_lgpla = '422'. "Source Storage type
  lv_bwlvs_002 = '850'.
*  ELSE.
*    lv_bwlvs_002 = '999'.
*  ENDIF.
*&<<<<-----------------------------------------------------------------*
* insert by stlim - 2004/02/05 - end
*&<<<<-----------------------------------------------------------------*


  lv_refnr_013 =  <fs_data_for_to>-feedr. "Group(Feeder)

  lv_matnr_003  = <fs_data_for_to>-matnr. "Material '327003K100'

  lv_anfme_004  = <fs_data_for_to>-tqty.
  lv_anfme_007  = <fs_data_for_to>-tqty.
  lv_altme_008  = <fs_data_for_to>-unit.
  lv_vltyp_009  = <fs_data_for_to>-src_lgtyp.
  "Src Storage Type '434'
  lv_vlpla_010  = <fs_data_for_to>-src_lgpla.
  "Src Storage Bin  'AA-01-11'
  lv_nltyp_011  = <fs_data_for_to>-des_lgtyp.
  "Des Storage Type '443'
  lv_nlpla_012  = <fs_data_for_to>-des_lgpla.
  "Des Storage Bin  'TS-01'


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
    bwlvs_002     = lv_bwlvs_002      "Movement type '999'
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
*&      Form  bdc_processing_lta1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BDCMSGCOLL  text
*      -->P_W_ZDOCNO  text
*      -->P_WA_BDCMSGCOLL_MSGV1  text
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

**** (Begin)Adjust Date format in user by user
  DATA: lv_date(8). CLEAR: lv_date.
  PERFORM user_date_format
             USING    sy-uname
                      <fs_data_for_to>-sdate
             CHANGING lv_date.
  lv_stdat_003 = lv_date.     "Start date
****
  PERFORM user_date_format
             USING    sy-uname
                      <fs_data_for_to>-edate
             CHANGING lv_date.
  lv_endat_005 = lv_date.  "End date
**** (End)Adjust Date format in user by user
  lv_tanum_001 = p_msgv1.                       "TO number  '813'
  lv_lgnum_002 = 'P01'.                         "Warehouse number
  lv_stuzt_004 = <fs_data_for_to>-stime.        "Start time
  lv_enuzt_006 = <fs_data_for_to>-etime.        "End time

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
     stdat_003       = lv_stdat_003
     stuzt_004       = lv_stuzt_004                         "'10:36:48'
     endat_005       = lv_endat_005
     enuzt_006       = lv_enuzt_006
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
ENDFORM.                    " bdc_processing_lta1
*&---------------------------------------------------------------------*
*&      Form  user_date_format
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_UNAME  text
*      -->P_WA_MATNR_DATE_TIME_SDATE  text
*      <--P_LV_DATE  text
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
*&      Form  get_time_from_minutes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_ZTMM_NSTL>_FEED_CYCLE  text
*      <--P_LV_TIMEFORCAL  text
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
*&      Form  get_time_from_hours
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_lv_TIME_idx  text
*      <--P_LV_TIMEFORCAL  text
*----------------------------------------------------------------------*
FORM get_time_from_hours
                 USING    value(im_hours)
                 CHANGING value(ex_time) TYPE t.
  CLEAR: ex_time.
  DATA: BEGIN OF ls_time,
         hour(2) TYPE n,
         minute(2) TYPE n,
         second(2) TYPE n,
        END OF ls_time.

  ls_time-hour   = im_hours.

  MOVE ls_time TO ex_time.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  make_wa_data_for_to
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_wa_data_for_to
                   USING value(im_qty)
                         value(im_sdate)
                         value(im_stime).

  CONSTANTS: c_onehour   TYPE t VALUE '010000'.
  DATA: lv_remainder TYPE p. "Remainder
  DATA: lv_quotient  TYPE p.  "Quotient



  MOVE <fs_matnr_date_time> TO wa_data_for_to.
  MOVE im_qty               TO wa_data_for_to-qty.
  MOVE im_sdate             TO wa_data_for_to-sdate.
  MOVE im_stime             TO wa_data_for_to-stime.



**** Begin of Rounding Quantity Check
  PERFORM get_rdmng USING    wa_data_for_to-matnr
                    CHANGING wa_data_for_to-rdmng.

* If there is Rounding Quantity,
* qty is to be least multiple of Rounding Quantity.

*A. Get Remainder  :mod
*B. Get quotient   :div
  CLEAR: lv_remainder, lv_quotient.

  IF NOT wa_data_for_to-rdmng IS INITIAL.
    lv_remainder = wa_data_for_to-qty MOD wa_data_for_to-rdmng.
    lv_quotient  = wa_data_for_to-qty DIV wa_data_for_to-rdmng.
  ENDIF.
  wa_data_for_to-tqty = wa_data_for_to-qty.

  IF NOT lv_remainder IS INITIAL.
    lv_quotient          = lv_quotient + 1.
    wa_data_for_to-tqty  = lv_quotient * wa_data_for_to-rdmng.
  ENDIF.
**** End of Rounding Quantity Check
* Get Source Storage type/bin
  PERFORM get_sorce_storage_type_bin
          USING    wa_data_for_to-matnr
          CHANGING wa_data_for_to-src_lgtyp
                   wa_data_for_to-src_lgpla.

* Get Destination Storage type/bin
  PERFORM get_des_storage_type_bin
          USING    wa_data_for_to-matnr
          CHANGING wa_data_for_to-des_lgtyp
                   wa_data_for_to-des_lgpla.


*/Begin of Added by Hakchin(20040429)
* End Time Adjust
  wa_data_for_to-etime = wa_data_for_to-stime + c_onehour.

  IF wa_data_for_to-etime = '000000'.
    wa_data_for_to-edate = wa_data_for_to-sdate + 1.
  ELSE.
    wa_data_for_to-edate = wa_data_for_to-sdate.
  ENDIF.
*/End of Added by Hakchin(20040429)
ENDFORM.                    " make_wa_data_for_to
*&---------------------------------------------------------------------*
*&      Form  get_it_data_for_to
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_it_data_for_to.
  DATA lv_sdate        TYPE d.
  DATA lv_stime        TYPE t.
  DATA lv_timeforcal   TYPE t.
  DATA lv_hour_idx(2)  TYPE n.
  DATA lv_hour_idx_tmp LIKE lv_hour_idx.
  DATA lv_qty          LIKE wa_matnr_date_time-qty.
  LOOP AT it_ztmm_nstl ASSIGNING <fs_ztmm_nstl>.
    PERFORM get_time_from_minutes
                     USING    <fs_ztmm_nstl>-feed_cycle
                     CHANGING lv_timeforcal.

* Making IT_DATA_FOR_TO.
    CLEAR: lv_hour_idx.
    LOOP AT it_matnr_date_time ASSIGNING <fs_matnr_date_time>
      WHERE matnr = <fs_ztmm_nstl>-matnr.

      lv_hour_idx     = lv_hour_idx + 1.   "Hour Increase
      lv_hour_idx_tmp = lv_hour_idx.       "Hour Increase

      IF lv_hour_idx <= lv_timeforcal(2).  "For Sum of DuringHour Qty
        lv_qty = lv_qty + <fs_matnr_date_time>-qty.
      ENDIF.

      IF lv_hour_idx = 1.
        lv_sdate = <fs_matnr_date_time>-sdate.   "Start Date
        lv_stime = <fs_matnr_date_time>-stime.   "Start Time
      ENDIF.

      IF lv_hour_idx = lv_timeforcal(2).
        PERFORM make_wa_data_for_to USING lv_qty
                                          lv_sdate
                                          lv_stime.
        APPEND wa_data_for_to TO it_data_for_to.
        CLEAR: lv_hour_idx, lv_qty.
      ENDIF.

      AT END OF matnr.
        IF lv_hour_idx_tmp = lv_timeforcal(2).
          CONTINUE.
        ENDIF.
        PERFORM make_wa_data_for_to USING lv_qty
                                          lv_sdate
                                          lv_stime.

        APPEND wa_data_for_to TO it_data_for_to.
        CLEAR: lv_hour_idx_tmp, lv_qty.
      ENDAT.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " get_it_data_for_to
*&---------------------------------------------------------------------*
*&      Form  get_it_matnr_date_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_it_matnr_date_time.
*/Begin of Changed by Hakchin(040131)
*  CONSTANTS: c_begintime TYPE t VALUE '080000'.
*  CONSTANTS: c_begintime TYPE t VALUE '060000'.
  CONSTANTS: c_begintime TYPE t VALUE '063000'.
*/End of Changed by Hakchin(040131)
  CONSTANTS: c_onehour   TYPE t VALUE '010000'.

  DATA lv_sdate TYPE d.
  DATA lv_stime TYPE t.
  DATA lv_edate TYPE d.
  DATA lv_etime TYPE t.

  DATA: lv_time_qty(19).
  "Time Quantity Field (TIME01,TIME02,TIME03,... )

  FIELD-SYMBOLS: <fs_qty>.
  DATA: lv_time_idx(2) TYPE n.

  LOOP AT it_ztmm_nstl INTO wa_ztmm_nstl.

**/Part1

*DATE & Start Time
*    lv_sdate = sy-datum.   "Commented by Hakchin(20040428)
    lv_sdate = p_tocred.
    lv_stime = c_begintime.

    CLEAR: lv_time_idx.

*/Begin of Changed by Hakchin(20040131)
*    DO 20 TIMES.
*    DO 24 TIMES.  "(06:30:00 ~ 05:59:59)
    DO 4 TIMES.  "(06:30:00 ~ 10:30:00)
*/End of Changed by Hakchin(20040131)
      lv_time_idx = lv_time_idx + 1.

      IF lv_stime = '000000'.
        lv_sdate = lv_sdate + 1.
      ENDIF.

      lv_edate = lv_sdate.
      lv_etime = lv_stime + c_onehour.  "END TIME
      IF lv_etime = '000000'.
        lv_edate = lv_edate + 1.
      ENDIF.

      CONCATENATE 'WA_ZTMM_NSTL-TIME' lv_time_idx INTO lv_time_qty.

      MOVE: wa_ztmm_nstl-matnr      TO wa_matnr_date_time-matnr.
      MOVE: wa_ztmm_nstl-feedr      TO wa_matnr_date_time-feedr."Feeder
      MOVE: wa_ztmm_nstl-feed_cycle TO wa_matnr_date_time-feed_cycle.
      MOVE: lv_sdate                TO wa_matnr_date_time-sdate.
      MOVE: lv_stime                TO wa_matnr_date_time-stime.
      MOVE: lv_edate                TO wa_matnr_date_time-edate.
      MOVE: lv_etime                TO wa_matnr_date_time-etime.
      ASSIGN: (lv_time_qty)         TO <fs_qty>.
      MOVE: <fs_qty>                TO wa_matnr_date_time-qty.
      MOVE: wa_ztmm_nstl-meins      TO wa_matnr_date_time-unit.
      MOVE: wa_ztmm_nstl-feedr      TO wa_matnr_date_time-feedr."Feeder
      MOVE: wa_ztmm_nstl-feed_cycle TO wa_matnr_date_time-feed_cycle.

      IF NOT wa_matnr_date_time-qty IS INITIAL.
        APPEND wa_matnr_date_time TO it_matnr_date_time.
      ENDIF.
* TIME
      lv_stime  = lv_stime + c_onehour.
    ENDDO.

*/Begin og Added by Hakchin(20040428)
**/Part2

*DATE & Start Time
    lv_stime = '111500'.

    CLEAR: lv_time_idx. lv_time_idx = '04'.

    DO 10 TIMES.  "(11:15:00 ~ 21:15:00)
      lv_time_idx = lv_time_idx + 1.

      IF lv_stime = '000000'.
        lv_sdate = lv_sdate + 1.
      ENDIF.

      lv_edate = lv_sdate.
      lv_etime = lv_stime + c_onehour.  "END TIME
      IF lv_etime = '000000'.
        lv_edate = lv_edate + 1.
      ENDIF.

      CONCATENATE 'WA_ZTMM_NSTL-TIME' lv_time_idx INTO lv_time_qty.

      MOVE: wa_ztmm_nstl-matnr      TO wa_matnr_date_time-matnr.
      MOVE: wa_ztmm_nstl-feedr      TO wa_matnr_date_time-feedr."Feeder
      MOVE: wa_ztmm_nstl-feed_cycle TO wa_matnr_date_time-feed_cycle.
      MOVE: lv_sdate                TO wa_matnr_date_time-sdate.
      MOVE: lv_stime                TO wa_matnr_date_time-stime.
      MOVE: lv_edate                TO wa_matnr_date_time-edate.
      MOVE: lv_etime                TO wa_matnr_date_time-etime.
      ASSIGN: (lv_time_qty)         TO <fs_qty>.
      MOVE: <fs_qty>                TO wa_matnr_date_time-qty.
      MOVE: wa_ztmm_nstl-meins      TO wa_matnr_date_time-unit.
      MOVE: wa_ztmm_nstl-feedr      TO wa_matnr_date_time-feedr."Feeder
      MOVE: wa_ztmm_nstl-feed_cycle TO wa_matnr_date_time-feed_cycle.

      IF NOT wa_matnr_date_time-qty IS INITIAL.
        APPEND wa_matnr_date_time TO it_matnr_date_time.
      ENDIF.
* TIME
      lv_stime  = lv_stime + c_onehour.
    ENDDO.

**/Part3

*DATE & Start Time
    lv_stime = '220000'.

    CLEAR: lv_time_idx. lv_time_idx = '14'.

    DO 6 TIMES.  "(22:00:00 ~ 04:00:00)
      lv_time_idx = lv_time_idx + 1.

      IF lv_stime = '000000'.
        lv_sdate = lv_sdate + 1.
      ENDIF.

      lv_edate = lv_sdate.
      lv_etime = lv_stime + c_onehour.  "END TIME
      IF lv_etime = '000000'.
        lv_edate = lv_edate + 1.
      ENDIF.

      CONCATENATE 'WA_ZTMM_NSTL-TIME' lv_time_idx INTO lv_time_qty.

      MOVE: wa_ztmm_nstl-matnr      TO wa_matnr_date_time-matnr.
      MOVE: wa_ztmm_nstl-feedr      TO wa_matnr_date_time-feedr."Feeder
      MOVE: wa_ztmm_nstl-feed_cycle TO wa_matnr_date_time-feed_cycle.
      MOVE: lv_sdate                TO wa_matnr_date_time-sdate.
      MOVE: lv_stime                TO wa_matnr_date_time-stime.
      MOVE: lv_edate                TO wa_matnr_date_time-edate.
      MOVE: lv_etime                TO wa_matnr_date_time-etime.
      ASSIGN: (lv_time_qty)         TO <fs_qty>.
      MOVE: <fs_qty>                TO wa_matnr_date_time-qty.
      MOVE: wa_ztmm_nstl-meins      TO wa_matnr_date_time-unit.
      MOVE: wa_ztmm_nstl-feedr      TO wa_matnr_date_time-feedr."Feeder
      MOVE: wa_ztmm_nstl-feed_cycle TO wa_matnr_date_time-feed_cycle.

      IF NOT wa_matnr_date_time-qty IS INITIAL.
        APPEND wa_matnr_date_time TO it_matnr_date_time.
      ENDIF.
* TIME
      lv_stime  = lv_stime + c_onehour.
    ENDDO.
*/End of Added by Hakchin(20040428)

  ENDLOOP.
ENDFORM.                    " get_it_matnr_date_time
*&---------------------------------------------------------------------*
*&      Form  process_it_data_for_to
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_it_data_for_to.
* App Doc No
  PERFORM number_get_next USING    c_nro_nr_09   "NRO Interval
                                   w_nro_object  "NRO Object
                          CHANGING w_zdocno.     "App Doc No
  COMMIT WORK.
  LOOP AT it_data_for_to ASSIGNING <fs_data_for_to>.
**** Begin of Create TO (/nLT01)
* BDC Processing of /nLT01
    PERFORM bdc_processing_lt01 TABLES   it_bdcmsgcoll
                                USING    w_zdocno
                                CHANGING w_subrc.
* Begin of Change TO Header (/nLT1A)
    IF w_subrc = 0.
      CLEAR: wa_bdcmsgcoll.
      READ TABLE it_bdcmsgcoll INTO wa_bdcmsgcoll
                               WITH KEY msgtyp = 'S'.
      CHECK sy-subrc = 0.
      PERFORM bdc_processing_lta1
                       TABLES   it_bdcmsgcoll
                       USING    w_zdocno
                                wa_bdcmsgcoll-msgv1  "TO number
                       CHANGING w_subrc.
    ENDIF.
* End of Change TO Header (/nLT1A)
  ENDLOOP.
ENDFORM.                    " process_it_data_for_to
*&---------------------------------------------------------------------*
*&      Form  process_it_data_for_to_bgd
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_it_data_for_to_bgd.
* App Doc No
  PERFORM number_get_next USING    c_nro_nr_09   "NRO Interval
                                   w_nro_object  "NRO Object
                          CHANGING w_zdocno.     "App Doc No
  COMMIT WORK.
************************************************************************
  DATA: lv_lines     TYPE i.  "Line No of it_data_for_to
  DATA: lv_quotient  TYPE i.
  DATA: lv_remainder TYPE i.
  DATA: lt_data_for_to LIKE it_data_for_to.
  "For Sectional Use of it_data_for_to
  DATA: lc_cutlines TYPE i VALUE 4000. "Section Size
  DATA: lv_do_idx TYPE i.  "Index for Do Loop
  DATA: lv_frline TYPE i.  "From Line
  DATA: lv_toline TYPE i.  "To Line

*/Get Line no. of it_data_for_to
  DESCRIBE TABLE it_data_for_to LINES lv_lines.
  "I estimate lv_lines = 18000.
  lv_quotient  = lv_lines DIV lc_cutlines.
  lv_remainder = lv_lines MOD lc_cutlines.

*/Transfer Order Manipulation
  DO lv_quotient TIMES.
    lv_do_idx = lv_do_idx + 1.
    lv_frline  = ( lv_do_idx - 1 ) * lc_cutlines + 1.
    lv_toline  = lv_do_idx * lc_cutlines.
    CLEAR: lt_data_for_to.
    APPEND LINES OF it_data_for_to
      FROM lv_frline TO lv_toline TO lt_data_for_to.
    CALL FUNCTION 'Z_FMM_NSTL_TOCRE'
      IN BACKGROUND TASK
      EXPORTING
        im_zdocno       = w_zdocno
      TABLES
        imt_data_for_to = lt_data_for_to.
    COMMIT WORK.
  ENDDO.
  IF NOT lv_remainder IS INITIAL.
    lv_frline  = lv_quotient * lc_cutlines + 1.
    lv_toline  = lv_lines. "(=lv_quotient * lc_cutlines + lv_remainder)
    CLEAR: lt_data_for_to.
    APPEND LINES OF it_data_for_to
      FROM lv_frline TO lv_toline TO lt_data_for_to.
    CALL FUNCTION 'Z_FMM_NSTL_TOCRE'
      IN BACKGROUND TASK
      EXPORTING
        im_zdocno       = w_zdocno
      TABLES
        imt_data_for_to = lt_data_for_to.
    COMMIT WORK.

  ENDIF.
ENDFORM.                    " process_it_data_for_to_bgd

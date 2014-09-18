
************************************************************************
* Program Name      : ZMMR_GAPS
* Creation Date     : 07/2012
* Development Request No :
* Addl Documentation:
* Description       : GAPS interface (Daily, Weekly, Monthly)
*
* Modification Logs
* Date       Developer RequestNo      Description
************************************************************************

REPORT zmmr_gaps NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmmm.

TABLES: mara.
TYPE-POOLS slis.
DATA: it_daily LIKE TABLE OF ztmm_gaps_daily WITH HEADER LINE,
      it_weekly LIKE TABLE OF ztmm_gaps_weekly WITH HEADER LINE,
      it_month LIKE TABLE OF ztmm_gaps_month WITH HEADER LINE.

DATA : BEGIN OF it_tab_temp OCCURS 0,
      werks LIKE marc-werks,
      matnr LIKE mara-matnr,
      lifnr LIKE lfa1-lifnr,
      matkl LIKE mara-matkl,
      bklas LIKE mbew-bklas,

      END OF it_tab_temp.

DATA: BEGIN OF it_tab OCCURS 0,
      werks LIKE marc-werks,
      matnr LIKE mara-matnr,
      lifnr LIKE lfa1-lifnr,
      days LIKE mdsm-bdmng,
      labst LIKE mard-labst,
      seq(1),
      rqty_p LIKE mdsm-bdmng,
      rqty_01 LIKE mdsm-bdmng,
      rqty_02 LIKE mdsm-bdmng,
      rqty_03 LIKE mdsm-bdmng,
      rqty_04 LIKE mdsm-bdmng,
      rqty_05 LIKE mdsm-bdmng,
      rqty_06 LIKE mdsm-bdmng,
      rqty_07 LIKE mdsm-bdmng,
      rqty_08 LIKE mdsm-bdmng,
      rqty_09 LIKE mdsm-bdmng,
      rqty_10 LIKE mdsm-bdmng,
      rqty_11 LIKE mdsm-bdmng,
      rqty_12 LIKE mdsm-bdmng,
      rqty_13 LIKE mdsm-bdmng,
      rqty_14 LIKE mdsm-bdmng,
      rqty_15 LIKE mdsm-bdmng,
      rqty_16 LIKE mdsm-bdmng,
      rqty_17 LIKE mdsm-bdmng,
      rqty_18 LIKE mdsm-bdmng,
      rqty_19 LIKE mdsm-bdmng,
      rqty_20 LIKE mdsm-bdmng,
      rqty_21 LIKE mdsm-bdmng,
      rqty_22 LIKE mdsm-bdmng,
      rqty_23 LIKE mdsm-bdmng,
      rqty_24 LIKE mdsm-bdmng,
      rqty_25 LIKE mdsm-bdmng,
      rqty_26 LIKE mdsm-bdmng,
      rqty_27 LIKE mdsm-bdmng,
      rqty_28 LIKE mdsm-bdmng,
      rqty_29 LIKE mdsm-bdmng,
      rqty_30 LIKE mdsm-bdmng,
      rqty_31 LIKE mdsm-bdmng,
      rqty_32 LIKE mdsm-bdmng,
      rqty_33 LIKE mdsm-bdmng,
      rqty_34 LIKE mdsm-bdmng,
      rqty_35 LIKE mdsm-bdmng,
      rqty_36 LIKE mdsm-bdmng,
      rqty_37 LIKE mdsm-bdmng,
      rqty_38 LIKE mdsm-bdmng,
      rqty_39 LIKE mdsm-bdmng,
      rqty_40 LIKE mdsm-bdmng,
      rqty_41 LIKE mdsm-bdmng,
      rqty_42 LIKE mdsm-bdmng,
      rqty_43 LIKE mdsm-bdmng,
      rqty_44 LIKE mdsm-bdmng,
      rqty_45 LIKE mdsm-bdmng,
      total LIKE mdsm-bdmng,
*      EISBE LIKE MARC-EISBE,
*      MAKTX LIKE MAKT-MAKTX,
      wmstk LIKE mard-labst,
      cogi LIKE affw-erfmg,
      matkl LIKE mara-matkl,
      bklas LIKE mbew-bklas,
      END OF it_tab.

DATA: BEGIN OF it_output OCCURS 0,
      werks LIKE marc-werks,
      matnr LIKE mara-matnr,
      lifnr LIKE lfa1-lifnr,
      days LIKE mdsm-bdmng,
      labst LIKE mard-labst,
      seq(1),
      rqty_p LIKE mdsm-bdmng,
      rqty_01 LIKE mdsm-bdmng,
      rqty_02 LIKE mdsm-bdmng,
      rqty_03 LIKE mdsm-bdmng,
      rqty_04 LIKE mdsm-bdmng,
      rqty_05 LIKE mdsm-bdmng,
      rqty_06 LIKE mdsm-bdmng,
      rqty_07 LIKE mdsm-bdmng,
      rqty_08 LIKE mdsm-bdmng,
      rqty_09 LIKE mdsm-bdmng,
      rqty_10 LIKE mdsm-bdmng,
      rqty_11 LIKE mdsm-bdmng,
      rqty_12 LIKE mdsm-bdmng,
      rqty_13 LIKE mdsm-bdmng,
      rqty_14 LIKE mdsm-bdmng,
      rqty_15 LIKE mdsm-bdmng,
      rqty_16 LIKE mdsm-bdmng,
      rqty_17 LIKE mdsm-bdmng,
      rqty_18 LIKE mdsm-bdmng,
      rqty_19 LIKE mdsm-bdmng,
      rqty_20 LIKE mdsm-bdmng,
      rqty_21 LIKE mdsm-bdmng,
      rqty_22 LIKE mdsm-bdmng,
      rqty_23 LIKE mdsm-bdmng,
      rqty_24 LIKE mdsm-bdmng,
      rqty_25 LIKE mdsm-bdmng,
      rqty_26 LIKE mdsm-bdmng,
      rqty_27 LIKE mdsm-bdmng,
      rqty_28 LIKE mdsm-bdmng,
      rqty_29 LIKE mdsm-bdmng,
      rqty_30 LIKE mdsm-bdmng,
      rqty_31 LIKE mdsm-bdmng,
      rqty_32 LIKE mdsm-bdmng,
      rqty_33 LIKE mdsm-bdmng,
      rqty_34 LIKE mdsm-bdmng,
      rqty_35 LIKE mdsm-bdmng,
      rqty_36 LIKE mdsm-bdmng,
      rqty_37 LIKE mdsm-bdmng,
      rqty_38 LIKE mdsm-bdmng,
      rqty_39 LIKE mdsm-bdmng,
      rqty_40 LIKE mdsm-bdmng,
      rqty_41 LIKE mdsm-bdmng,
      rqty_42 LIKE mdsm-bdmng,
      rqty_43 LIKE mdsm-bdmng,
      rqty_44 LIKE mdsm-bdmng,
      rqty_45 LIKE mdsm-bdmng,
*      EISBE LIKE MARC-EISBE,
*      MAKTX LIKE MAKT-MAKTX,
      wmstk LIKE mard-labst,
      matkl LIKE mara-matkl,
      bklas LIKE mbew-bklas,
      END OF it_output.

DATA: BEGIN OF it_lips OCCURS 0,
      werks LIKE marc-werks,
      matnr LIKE mara-matnr,
      lifnr LIKE lfa1-lifnr,
      dispo LIKE marc-dispo,
      days LIKE mdsm-bdmng,
      labst LIKE mard-labst,
      seq(1),
      rqty_p LIKE mdsm-bdmng,
      rqty_01 LIKE mdsm-bdmng,
      rqty_02 LIKE mdsm-bdmng,
      rqty_03 LIKE mdsm-bdmng,
      rqty_04 LIKE mdsm-bdmng,
      rqty_05 LIKE mdsm-bdmng,
      rqty_06 LIKE mdsm-bdmng,
      rqty_07 LIKE mdsm-bdmng,
      rqty_08 LIKE mdsm-bdmng,
      rqty_09 LIKE mdsm-bdmng,
      rqty_10 LIKE mdsm-bdmng,
      rqty_11 LIKE mdsm-bdmng,
      rqty_12 LIKE mdsm-bdmng,
      rqty_13 LIKE mdsm-bdmng,
      rqty_14 LIKE mdsm-bdmng,
      rqty_15 LIKE mdsm-bdmng,
      rqty_16 LIKE mdsm-bdmng,
      rqty_17 LIKE mdsm-bdmng,
      rqty_18 LIKE mdsm-bdmng,
      rqty_19 LIKE mdsm-bdmng,
      rqty_20 LIKE mdsm-bdmng,
      rqty_21 LIKE mdsm-bdmng,
      rqty_22 LIKE mdsm-bdmng,
      rqty_23 LIKE mdsm-bdmng,
      rqty_24 LIKE mdsm-bdmng,
      rqty_25 LIKE mdsm-bdmng,
      rqty_26 LIKE mdsm-bdmng,
      rqty_27 LIKE mdsm-bdmng,
      rqty_28 LIKE mdsm-bdmng,
      rqty_29 LIKE mdsm-bdmng,
      rqty_30 LIKE mdsm-bdmng,
      rqty_31 LIKE mdsm-bdmng,
      rqty_32 LIKE mdsm-bdmng,
      rqty_33 LIKE mdsm-bdmng,
      rqty_34 LIKE mdsm-bdmng,
      rqty_35 LIKE mdsm-bdmng,
      rqty_36 LIKE mdsm-bdmng,
      rqty_37 LIKE mdsm-bdmng,
      rqty_38 LIKE mdsm-bdmng,
      rqty_39 LIKE mdsm-bdmng,
      rqty_40 LIKE mdsm-bdmng,
      rqty_41 LIKE mdsm-bdmng,
      rqty_42 LIKE mdsm-bdmng,
      rqty_43 LIKE mdsm-bdmng,
      rqty_44 LIKE mdsm-bdmng,
      rqty_45 LIKE mdsm-bdmng,
      eisbe LIKE marc-eisbe,
      matkl LIKE mara-matkl,
      bklas LIKE mbew-bklas,
      END OF it_lips.

DATA : BEGIN OF it_mard OCCURS 0,
       matnr LIKE mard-matnr,
       labst LIKE mard-labst,
       END OF it_mard.

DATA:  BEGIN OF it_mdsm OCCURS 0,
         werks LIKE marc-werks,
         matnr LIKE mara-matnr,
         bdter LIKE mdsm-bdter,
         bdmng LIKE mdsm-bdmng,
         sbnum LIKE mdsm-sbnum,
         sbpos LIKE mdsm-sbpos,
       END OF it_mdsm.

DATA : BEGIN OF it_mard_temp OCCURS 0,
         werks LIKE mard-werks,
         matnr LIKE mard-matnr,
         labst LIKE mard-labst,
       END OF it_mard_temp.

DATA: BEGIN OF it_day OCCURS 45,
        seq(2)  TYPE n,
        datum   LIKE   sy-datum,
      END   OF it_day.

DATA: BEGIN OF it_day_lips OCCURS 45,
        seq(2)  TYPE n,
        datum   LIKE   sy-datum,
      END   OF it_day_lips.

DATA: BEGIN OF it_week OCCURS 45,
        seq(2)  TYPE n,
        datum   LIKE   sy-datum,
      END   OF it_week.

DATA: w_prd_date LIKE sy-datum.

DATA: w_no_data(1).

DATA:  l_kalid LIKE kako-kalid.

DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE.

FIELD-SYMBOLS : <fs01>, <fs02>, <fs-qty>.

**--- Constants
CONSTANTS : c_plscn LIKE plaf-plscn VALUE '900'.

DATA: w_dest(10).
DATA:  BEGIN OF it_error OCCURS 0,
      crtn_ymd(10),
      part_cd(20),
      item(1),
      END OF it_error.

SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.

SELECT-OPTIONS : s_matnr FOR mara-matnr,
                 s_matkl FOR mara-matkl.
PARAMETERS : p_werks LIKE t001w-werks OBLIGATORY DEFAULT 'P001'.
PARAMETERS : p_day TYPE p DECIMALS 1
    DEFAULT '45.0' NO-DISPLAY.  " OBLIGATORY '21.0'
PARAMETERS : p_pwerks LIKE t001w-werks DEFAULT 'P001' NO-DISPLAY..
SELECTION-SCREEN END OF BLOCK block1.

SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-002.

SELECTION-SCREEN ULINE.
PARAMETERS : r01 RADIOBUTTON GROUP rb1 DEFAULT 'X'.
PARAMETERS : r02 RADIOBUTTON GROUP rb1.
PARAMETERS : r03 RADIOBUTTON GROUP rb1.

SELECTION-SCREEN ULINE.
PARAMETERS : p_eai AS CHECKBOX DEFAULT 'X'.
PARAMETERS: p_rver LIKE somlreci1-receiver OBLIGATORY.

SELECTION-SCREEN END OF BLOCK block2.

START-OF-SELECTION.
  IF sy-uzeit >= '000000' AND sy-uzeit <= '064500'.
    w_prd_date = sy-datum - 1.
  ELSE.
    w_prd_date = sy-datum.
  ENDIF.

  PERFORM get_data.
  IF w_no_data = 'X'.
    MESSAGE e009 WITH 'NO Data'.
  ENDIF.
  PERFORM process_data.
  PERFORM make_interface_data.
  IF p_eai = 'X'.
    PERFORM send_data.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  PERFORM get_req_data.
  IF w_no_data IS INITIAL.
    PERFORM get_lips_data.
  ENDIF.
ENDFORM.                    "GET_DATA
*---------------------------------------------------------------------*
*       FORM get_req_data                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM get_req_data.
**---
  DATA : BEGIN OF lt_mbew OCCURS 0,
       matnr LIKE mara-matnr,
       matkl LIKE mara-matkl,
       bklas LIKE mbew-bklas,
       END OF lt_mbew.

  DATA:    l_date LIKE sy-datum,
           l_week_f LIKE sy-datum,
           l_cn(2) TYPE n,
           l_in_cn(2) TYPE n,
           l_cn_week(2) TYPE n,
           l_text(30),
           l_text_21t(30),
           l_date_1 LIKE sy-datum,
           l_date_2 LIKE sy-datum,
           l_date_3 LIKE sy-datum,
           l_in_date LIKE sy-datum,
           l_index LIKE sy-tabix.

  DATA: it_mrp LIKE TABLE OF ztmm_parts_21day WITH HEADER LINE,
       it_mdsm_temp LIKE TABLE OF it_mdsm WITH HEADER LINE.

  DATA: l_mrp_inx LIKE sy-index.

  CLEAR : it_tab, it_tab[], it_day, it_day[], it_mdsm,
          it_mdsm[].
  CLEAR: it_tab_temp, it_tab_temp[], it_mard_temp, it_mard_temp[],
         it_mard, it_mard[]. " IT_LQUA, IT_LQUA[].

  SELECT * INTO TABLE it_mrp
     FROM ztmm_parts_21day
     WHERE matnr IN s_matnr.
** On 12/02/13 by Furong - Request by Pascal
*       AND datum = w_prd_date.
** End on 12/02/13

** GEt ltp from w4-w21
  SELECT werks matnr bdter bdmng sbnum sbpos
    INTO CORRESPONDING FIELDS OF TABLE it_mdsm_temp
  FROM mdsm
   WHERE werks = p_pwerks
     AND matnr IN s_matnr
     AND plscn = '900'
     AND bdter >= l_date_3.

  LOOP AT it_mdsm_temp.
    it_mdsm = it_mdsm_temp.
    it_mdsm-sbnum = ''.
    it_mdsm-sbpos = ''.
    COLLECT it_mdsm.
    CLEAR: it_mdsm, it_mdsm_temp.
  ENDLOOP.

  LOOP AT it_mrp.
    MOVE-CORRESPONDING it_mrp TO it_tab_temp.
    APPEND it_tab_temp.
    CLEAR: it_tab_temp.
  ENDLOOP.

  SORT it_mrp BY matnr.
  LOOP AT it_mdsm.
    READ TABLE it_mrp WITH KEY matnr = it_mdsm-matnr.
    IF sy-subrc = 0.
    ELSE.
      MOVE-CORRESPONDING it_mdsm TO it_tab_temp.
      PERFORM get_vendor USING it_tab_temp-lifnr.
      COLLECT it_tab_temp.
    ENDIF.
    CLEAR: it_tab_temp.
  ENDLOOP.

  IF NOT it_tab_temp[] IS INITIAL.

    SELECT a~matnr matkl bklas INTO TABLE lt_mbew
      FROM mbew AS a
      INNER JOIN mara AS b
      ON a~matnr = b~matnr
      FOR ALL ENTRIES IN it_tab_temp
      WHERE bwkey = it_tab_temp-werks
        AND a~matnr = it_tab_temp-matnr.
    SORT lt_mbew BY matnr.

    SELECT werks matnr labst INTO TABLE it_mard_temp
       FROM mard
       FOR ALL ENTRIES IN it_tab_temp
       WHERE matnr = it_tab_temp-matnr
         AND werks = it_tab_temp-werks
         AND lvorm = space
         AND diskz <> '1'.

    LOOP AT it_mard_temp.
      MOVE-CORRESPONDING it_mard_temp TO it_mard.
      COLLECT it_mard.
      CLEAR: it_mard_temp, it_mard.
    ENDLOOP.

    LOOP AT it_tab_temp.
      MOVE-CORRESPONDING it_tab_temp TO it_tab.
      READ TABLE lt_mbew WITH KEY matnr = it_tab-matnr.
      IF lt_mbew-bklas = '3000' OR
         lt_mbew-bklas = '3001' OR
         lt_mbew-bklas = '3005'.
        it_tab-bklas = lt_mbew-bklas.
        it_tab-matkl = lt_mbew-matkl.
        READ TABLE it_mard WITH KEY matnr = it_tab-matnr.
        IF sy-subrc = 0.
          it_tab-labst = it_mard-labst.
        ENDIF.
        COLLECT it_tab.
      ENDIF.
      CLEAR: it_tab_temp, it_tab, lt_mbew, it_mard.
    ENDLOOP.

    CLEAR: it_mard_temp[], it_tab_temp[].

* reading working calendar
    SELECT SINGLE kalid INTO l_kalid
      FROM zvpp_capacity
     WHERE arbpl = 'T'   .

    PERFORM read_shop_calid  USING l_kalid.
    PERFORM set_days.

** Set week date
    CLEAR: it_week, it_week[].
    l_date = w_prd_date.
    CALL FUNCTION 'HR_GBSSP_GET_WEEK_DATES'
      EXPORTING
        p_pdate       = l_date
      IMPORTING
        p_sunday      = l_date
*       P_SATURDAY    =
*       P_DAY_IN_WEEK =
*       P_WEEK_NO     =
      .
    l_date = l_date + 1.                                    "03/13

    l_cn = '01'.
    DO 45 TIMES.
      it_week-seq = l_cn.
      l_date_1 = l_date.
      l_date = l_date + 7.
      IF l_date_1 > l_date.
        l_date = l_date + 7.
      ELSE.
        IF l_date = l_date_1.
          l_date_1 = l_date_1 - 7.
        ENDIF.
      ENDIF.
       if l_date_1+4(2) <> w_prd_date+4(2)
          and l_cn = 1.
        CONCATENATE w_prd_date+0(6) '01' into l_date_1.
      endif.
      it_week-datum = l_date_1.
      APPEND it_week.
      l_cn = l_cn + 1.
    ENDDO.

    READ TABLE it_week INDEX 3.
    l_date_2 = it_week-datum.
    READ TABLE it_week INDEX 4.
    l_date_3 = it_week-datum.

*-- MRP
    IF r01 = 'X'.
      LOOP AT it_tab.
        l_cn = '01'.
        l_date = w_prd_date.
        READ TABLE it_mrp WITH KEY werks = p_pwerks
                                   matnr = it_tab-matnr
                                   lifnr = it_tab-lifnr.
        IF sy-subrc = 0.
*          IT_TAB-RQTY_P = IT_TAB-RQTY_P + IT_MRP-BFD.
          WHILE l_date < l_date_3.
*          DO 21 TIMES.
            CONCATENATE 'IT_TAB-RQTY_' l_cn INTO l_text.
            ASSIGN (l_text) TO <fs-qty>.
*            READ TABLE IT_DAY WITH KEY SEQ = L_CN.
            READ TABLE it_day WITH KEY datum = l_date.
            IF sy-subrc = 0.
              CONCATENATE 'IT_MRP-D' it_day-seq INTO  l_text_21t.
              ASSIGN (l_text_21t) TO <fs01>.
              <fs-qty> = <fs01>.
            ELSE.
              <fs-qty> = 0.
            ENDIF.
            l_cn = l_cn + 1.
            l_date = l_date + 1.
*          ENDDO.
          ENDWHILE.
        ELSE.
          l_cn = l_date_3 - w_prd_date + 1.
        ENDIF.
        l_cn_week = '03'.

        WHILE l_cn_week < 42.
          l_cn_week = l_cn_week + 1.
          READ TABLE it_week WITH KEY seq = l_cn_week.
          l_date = it_week-datum.
          IF l_cn_week = 45.
            l_in_date = l_date + 6.
          ELSE.
            l_in_cn = l_cn_week + 1.
            READ TABLE it_week WITH KEY seq = l_in_cn.
            l_in_date = it_week-datum - 1.
          ENDIF.
          CONCATENATE 'IT_TAB-RQTY_' l_cn INTO l_text.
          ASSIGN (l_text) TO <fs-qty>.
          IF sy-subrc = 0.
            LOOP AT it_mdsm WHERE werks = it_tab-werks
                              AND matnr = it_tab-matnr
                           AND ( bdter BETWEEN l_date AND l_in_date )
.
              <fs-qty> = <fs-qty> + it_mdsm-bdmng.
            ENDLOOP.
          ENDIF.
          l_cn = l_cn + 7.
        ENDWHILE.

        it_tab-total = it_tab-rqty_p + it_tab-rqty_01 + it_tab-rqty_02
                    + it_tab-rqty_03 + it_tab-rqty_04 + it_tab-rqty_05
                    + it_tab-rqty_06 + it_tab-rqty_07 + it_tab-rqty_08
                    + it_tab-rqty_09 + it_tab-rqty_10 + it_tab-rqty_11
                    + it_tab-rqty_12 + it_tab-rqty_13 + it_tab-rqty_14
                    + it_tab-rqty_15 + it_tab-rqty_16 + it_tab-rqty_17
                    + it_tab-rqty_18 + it_tab-rqty_19 + it_tab-rqty_20
                    + it_tab-rqty_21 + it_tab-rqty_22 + it_tab-rqty_23
                    + it_tab-rqty_24 + it_tab-rqty_25 + it_tab-rqty_26
                    + it_tab-rqty_27 + it_tab-rqty_28 + it_tab-rqty_29
                    + it_tab-rqty_30 + it_tab-rqty_31 + it_tab-rqty_32
                    + it_tab-rqty_33 + it_tab-rqty_34 + it_tab-rqty_35
                    + it_tab-rqty_36 + it_tab-rqty_37 + it_tab-rqty_38
                    + it_tab-rqty_39 + it_tab-rqty_40 + it_tab-rqty_41
                    + it_tab-rqty_42 + it_tab-rqty_43 + it_tab-rqty_44
                    + it_tab-rqty_45.
        it_tab-seq = '1'.
        MODIFY it_tab.
        CLEAR: it_tab.
      ENDLOOP.
      DELETE it_tab WHERE total = 0.
    ENDIF.

*-- long term planning
    IF r02 = 'X' OR r03 = 'X'.

      LOOP AT it_tab.
        l_index = sy-tabix.

        READ TABLE it_week INDEX 1.
        l_week_f = it_week-datum.

        l_mrp_inx = 2.

        READ TABLE it_mrp WITH KEY werks = p_pwerks
                                   matnr = it_tab-matnr
                                   lifnr = it_tab-lifnr.
        IF sy-subrc = 0.
*          IT_TAB-RQTY_P = IT_TAB-RQTY_P + IT_MRP-BFD + IT_TAB-COGI.

          l_text = 'IT_TAB-RQTY_01'.
          ASSIGN (l_text) TO <fs-qty>.
          READ TABLE it_week INDEX l_mrp_inx.

          LOOP AT it_day WHERE ( datum >= l_week_f
                             AND datum < it_week-datum ).
            CONCATENATE 'it_mrp-D' it_day-seq INTO  l_text_21t.
            ASSIGN (l_text_21t) TO <fs01>.
            IF sy-subrc = 0.
              <fs-qty> = <fs-qty> + <fs01>.
            ELSE.
              <fs-qty> = 0.
            ENDIF.
            UNASSIGN <fs01>.
          ENDLOOP.

          l_text = 'IT_TAB-RQTY_02'.
          ASSIGN (l_text) TO <fs-qty>.
          l_mrp_inx = l_mrp_inx + 1.
          l_week_f = it_week-datum.
          READ TABLE it_week INDEX l_mrp_inx.

          LOOP AT it_day WHERE ( datum >= l_week_f
                           AND datum < it_week-datum ).
            CONCATENATE 'it_mrp-D' it_day-seq INTO  l_text_21t.
            ASSIGN (l_text_21t) TO <fs01>.
            IF sy-subrc = 0.
              <fs-qty> = <fs-qty> + <fs01>.
            ELSE.
              <fs-qty> = 0.
            ENDIF.
            UNASSIGN <fs01>.
          ENDLOOP.

          l_text = 'IT_TAB-RQTY_03'.
          ASSIGN (l_text) TO <fs-qty>.
          l_mrp_inx = l_mrp_inx + 1.
          l_week_f = it_week-datum.
          READ TABLE it_week INDEX l_mrp_inx.

          LOOP AT it_day WHERE ( datum >= l_week_f
                          AND datum < it_week-datum ).
            CONCATENATE 'it_mrp-D' it_day-seq INTO  l_text_21t.
            ASSIGN (l_text_21t) TO <fs01>.
            IF sy-subrc = 0.
              <fs-qty> = <fs-qty> + <fs01>.
            ELSE.
              <fs-qty> = 0.
            ENDIF.
            UNASSIGN <fs01>.
          ENDLOOP.
        ENDIF.

        l_cn = '03'.

        DO 42 TIMES.  "18
          l_cn = l_cn + 1.
          READ TABLE it_week WITH KEY seq = l_cn.
          l_date = it_week-datum.
          IF l_cn = 45.
            l_in_date = l_date + 6.
          ELSE.
            l_in_cn = l_cn + 1.
            READ TABLE it_week WITH KEY seq = l_in_cn.
            l_in_date = it_week-datum - 1.
          ENDIF.
          CONCATENATE 'IT_TAB-RQTY_' l_cn INTO l_text.
          ASSIGN (l_text) TO <fs-qty>.
          IF sy-subrc = 0.
            LOOP AT it_mdsm WHERE werks = it_tab-werks
                              AND matnr = it_tab-matnr
                              AND ( bdter BETWEEN l_date AND l_in_date )
.
              <fs-qty> = <fs-qty> + it_mdsm-bdmng.
            ENDLOOP.
          ENDIF.
        ENDDO.

        it_tab-total = it_tab-rqty_p + it_tab-rqty_01 + it_tab-rqty_02
                     + it_tab-rqty_03 + it_tab-rqty_04 + it_tab-rqty_05
                     + it_tab-rqty_06 + it_tab-rqty_07 + it_tab-rqty_08
                     + it_tab-rqty_09 + it_tab-rqty_10 + it_tab-rqty_11
                     + it_tab-rqty_12 + it_tab-rqty_13 + it_tab-rqty_14
                     + it_tab-rqty_15 + it_tab-rqty_16 + it_tab-rqty_17
                     + it_tab-rqty_18 + it_tab-rqty_19 + it_tab-rqty_20
                     + it_tab-rqty_21 + it_tab-rqty_22 + it_tab-rqty_23
                     + it_tab-rqty_24 + it_tab-rqty_25 + it_tab-rqty_26
                     + it_tab-rqty_27 + it_tab-rqty_28 + it_tab-rqty_29
                     + it_tab-rqty_30 + it_tab-rqty_31 + it_tab-rqty_32
                     + it_tab-rqty_33 + it_tab-rqty_34 + it_tab-rqty_35
                     + it_tab-rqty_36 + it_tab-rqty_37 + it_tab-rqty_38
                     + it_tab-rqty_39 + it_tab-rqty_40 + it_tab-rqty_41
                     + it_tab-rqty_42 + it_tab-rqty_43 + it_tab-rqty_44
                     + it_tab-rqty_45.
        MODIFY it_tab INDEX l_index.
      ENDLOOP.
    ENDIF.
    DELETE it_tab WHERE total = 0.
  ELSE.
    w_no_data = 'X'.
    MESSAGE i000(zz) WITH  'No data found'.
  ENDIF.
ENDFORM.                    " get_req_data

*---------------------------------------------------------------------*
*       FORM set_days                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM set_days.
  DATA: l_count TYPE i.
  DATA: l_date LIKE sy-datum.

  l_count = '01'.
  l_date = w_prd_date.

  DO 45 TIMES.
    PERFORM read_working_date USING '+' l_kalid  l_date.
    it_day-seq     = l_count.
    it_day-datum   = l_date .
    APPEND it_day.  CLEAR: it_day.
    l_count  = l_count + 1.
    l_date   = l_date  + 1.
  ENDDO.

  l_count = '01'.
  l_date = w_prd_date.

  DO 45 TIMES.
    it_day_lips-seq     = l_count.
    it_day_lips-datum   = l_date .
    APPEND it_day_lips.  CLEAR: it_day_lips.
    l_count  = l_count + 1.
    l_date   = l_date  + 1.
  ENDDO.

ENDFORM.                    " set_DAYS
*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data.
  DATA: l_days LIKE it_tab-days,
        l_qty LIKE it_tab-rqty_p,
        l_cn(2) TYPE n,
        l_text_01(30),
        l_text_02(30),
        l_text_03(30),
        l_flag(1),
        l_line TYPE i.

  DATA: wa_bal LIKE LINE OF it_output.

  CLEAR: it_output, it_output[].

  LOOP AT it_tab.
    CLEAR: wa_bal, l_flag.
    MOVE-CORRESPONDING it_tab TO it_output.

** ASN
    READ TABLE it_lips WITH KEY matnr = it_tab-matnr.
*                                  LIFNR  = IT_TAB-LIFNR.
    IF sy-subrc NE 0.
      it_lips-matnr = it_tab-matnr.
      it_lips-werks = it_tab-werks.
    ENDIF.
*    IT_LIPS-DISPO = IT_TAB-DISPO.
*    IT_LIPS-EISBE = IT_TAB-EISBE.
    it_lips-lifnr = it_tab-lifnr.
    it_lips-matkl = it_tab-matkl.
    it_lips-bklas = it_tab-bklas.
    it_lips-seq = '2'.
    l_qty = it_tab-labst + it_lips-rqty_p - it_tab-rqty_p.
    it_output-days = 0.

    wa_bal-seq = '3'.
    wa_bal-rqty_p = l_qty.
    l_cn = '00'.
    DO 45 TIMES.
      l_cn = l_cn + 1.
      CONCATENATE 'IT_TAB-RQTY_' l_cn INTO l_text_01.
      ASSIGN (l_text_01) TO <fs01>.
      CONCATENATE 'IT_LIPS-RQTY_' l_cn INTO l_text_02.
      ASSIGN (l_text_02) TO <fs02>.
      CONCATENATE 'WA_BAL-RQTY_' l_cn INTO l_text_03.
      ASSIGN (l_text_03) TO <fs-qty>.
      IF sy-subrc EQ 0.
        <fs-qty> = l_qty + <fs02> - <fs01>.
        l_qty = <fs-qty>.
      ENDIF.
      IF l_flag IS INITIAL.
        IF l_qty >= 0.
          it_output-days = it_output-days + 1.
        ELSE.
          l_flag = 'X'.
          IF <fs01> <> 0 AND l_cn <> '01'.
            it_output-days = it_output-days
               + ( <fs01> + l_qty ) / <fs01>.
          ENDIF.
        ENDIF.
      ENDIF.
      UNASSIGN: <fs-qty>, <fs02>, <fs01>.
    ENDDO.
    IF it_output-days <= p_day.
      it_output-seq = '1'.
      APPEND it_output.
      it_lips-seq = '2'.
      CLEAR: it_output.
      MOVE-CORRESPONDING it_lips TO it_output.
    ELSE.
      CLEAR: wa_bal, it_lips, it_tab, it_output.
      CONTINUE.
    ENDIF.
    APPEND it_output.
    CLEAR: it_output.
    wa_bal-matkl = it_tab-matkl.
    wa_bal-lifnr = it_tab-lifnr.
    wa_bal-matnr = it_tab-matnr.
    wa_bal-werks = it_tab-werks.
    wa_bal-bklas = it_tab-bklas.
    APPEND wa_bal TO it_output.
    IF l_line = 0.
      l_line = 1.
    ELSE.
      l_line = 0.
    ENDIF.

    CLEAR: wa_bal, it_lips, it_tab, it_output.
    UNASSIGN: <fs01>, <fs02>, <fs-qty>.
  ENDLOOP.

ENDFORM.                    " process_data
*&---------------------------------------------------------------------*
*&      Form  read_shop_calid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_KALID  text
*----------------------------------------------------------------------*
FORM read_shop_calid USING p_l_kalid.
  SELECT SINGLE kalid INTO p_l_kalid
  FROM zvpp_capacity
 WHERE arbpl = 'T'   .
ENDFORM.                    " read_shop_calid
*---------------------------------------------------------------------*
*       FORM read_working_date                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PA_TYPE                                                       *
*  -->  PA_KALID                                                      *
*  -->  PA_WDATE                                                      *
*---------------------------------------------------------------------*
FORM read_working_date USING  pa_type  pa_kalid  pa_wdate.
  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
    EXPORTING
      correct_option               = pa_type
      date                         = pa_wdate
      factory_calendar_id          = pa_kalid
    IMPORTING
      date                         = pa_wdate
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      correct_option_invalid       = 2
      date_after_range             = 3
      date_before_range            = 4
      date_invalid                 = 5
      factory_calendar_not_found   = 6
      OTHERS                       = 7.

  IF sy-subrc <> 0.

  ENDIF.
ENDFORM.                    " READ_WORKING_DATE
*
*&---------------------------------------------------------------------*
*&      Form  get_lips_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_lips_data.
  DATA: BEGIN OF lt_lips_temp OCCURS 0,
        lifnr LIKE likp-lifnr,
        werks LIKE marc-werks,
        matnr LIKE mara-matnr,
        lfdat LIKE likp-lfdat,
        lfimg LIKE lips-lfimg,
        dabmg LIKE ekes-dabmg,
        END OF lt_lips_temp.

  DATA: lt_lips LIKE TABLE OF lt_lips_temp WITH HEADER LINE.

  DATA: l_st_date LIKE sy-datum,
        l_end_date LIKE sy-datum,
        l_text(30),
        l_index LIKE sy-tabix,
        l_cn(2) TYPE n.

  DATA: l_lifnr LIKE likp-lifnr,
        l_matnr LIKE mara-matnr,
       l_werks LIKE marc-werks,
       l_weekday LIKE dtresr-weekday.

  CLEAR: it_lips, it_lips[].

  IF r01 = 'X'.
    READ TABLE it_day INDEX 1.
    l_st_date = it_day-datum.
    READ TABLE it_day INDEX 45.
    l_end_date = it_day-datum.
  ELSE.
    READ TABLE it_week INDEX 1.
    l_st_date = it_week-datum.
    READ TABLE it_week INDEX 45.
    l_end_date = it_week-datum.
  ENDIF.

  SELECT a~lifnr b~werks b~matnr d~eindt AS lfdat
         d~menge AS lfimg d~dabmg
  INTO CORRESPONDING FIELDS OF TABLE lt_lips_temp
  FROM ekko AS a INNER JOIN ekpo AS b
  ON a~ebeln = b~ebeln
  INNER JOIN ekes AS d
  ON d~ebeln = b~ebeln
  AND d~ebelp = b~ebelp
  INNER JOIN vbuk AS c
  ON d~vbeln = c~vbeln
  WHERE matnr IN s_matnr
    AND b~werks = p_werks
    AND c~wbstk = 'A'
    AND c~vbtyp = '7'
    AND d~eindt BETWEEN l_st_date AND l_end_date
 %_HINTS ORACLE
   'LEADING(T_03) USE_NL(T_00 T_01 T_02) INDEX (T_03 "VBUK~Z01")'.
  "Addition

  LOOP AT lt_lips_temp.
    lt_lips = lt_lips_temp.
    lt_lips-lfimg = lt_lips-lfimg - lt_lips-dabmg.
    COLLECT lt_lips.
    CLEAR: lt_lips_temp.
  ENDLOOP.

  CLEAR: lt_lips_temp[].

  IF NOT lt_lips[] IS INITIAL.
    SORT lt_lips BY matnr.
    READ TABLE lt_lips INDEX 1.
    l_lifnr = lt_lips-lifnr.
    l_matnr = lt_lips-matnr.
    l_werks = lt_lips-werks.

    LOOP AT lt_lips.

*      IF L_LIFNR <> LT_LIPS-LIFNR
*         OR L_MATNR <> LT_LIPS-MATNR.
      IF  l_matnr <> lt_lips-matnr.

*        IT_LIPS-LIFNR = L_LIFNR.
        it_lips-matnr = l_matnr.
        it_lips-werks = l_werks.

        APPEND it_lips.
        CLEAR: it_lips.

        l_lifnr = lt_lips-lifnr.
        l_matnr = lt_lips-matnr.
        l_werks = lt_lips-werks.

      ENDIF.
      IF lt_lips-lfdat < l_st_date.
        it_lips-rqty_p = it_lips-rqty_p + lt_lips-lfimg.
      ELSE.
        IF r01 = 'X'.
          READ TABLE it_day_lips WITH KEY datum = lt_lips-lfdat.
          IF sy-subrc = 0.
            CONCATENATE 'IT_LIPS-RQTY_' it_day_lips-seq INTO l_text.
            ASSIGN (l_text) TO <fs01>.

            IF sy-subrc = 0.
              <fs01> = <fs01> + lt_lips-lfimg.
            ENDIF.

          ENDIF.
        ELSE.
          CLEAR: l_index.

          LOOP AT it_week.
            IF it_week-datum > lt_lips-lfdat.
              l_index = sy-tabix - 1.
              EXIT.
            ENDIF.
          ENDLOOP.

          IF l_index IS INITIAL.
            l_cn = '45'.
          ELSE.
            l_cn = l_index.
          ENDIF.
          CONCATENATE 'IT_LIPS-RQTY_' l_cn INTO l_text.
          ASSIGN (l_text) TO <fs01>.

          IF sy-subrc = 0.
            <fs01> = <fs01> + lt_lips-lfimg.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
*    IT_LIPS-LIFNR = L_LIFNR.
    it_lips-matnr = l_matnr.
    it_lips-werks = l_werks.
    APPEND it_lips.
    CLEAR: it_lips.
  ENDIF.
ENDFORM.                    " get_lips_data

*&---------------------------------------------------------------------*
*&      Form  MAKE_INTERFACE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_interface_data .
  DATA: l_cn(2) TYPE n,
        l_m_inc(2) TYPE n,
        l_m_cn(2) TYPE n,
        l_text_01(40),
        l_text_02(40),
        l_labst LIKE it_output-labst.
  DATA: l_date_from LIKE sy-datum,
        l_date_to LIKE sy-datum,
        l_yymm(6) TYPE n.

  IF r01 = 'X'.
    LOOP AT it_output.
      IF it_output-bklas = '3000'.
        it_daily-src_cd = 'K'.
        CASE it_output-lifnr.
          WHEN 'SEF9'.
            it_daily-vend_cd = 'G'.
          WHEN 'SSTX'.
            it_daily-vend_cd = 'H'.
          WHEN OTHERS.
            it_daily-vend_cd = 'ZZZZ'.
        ENDCASE.

        l_cn = '00'.
        DO 45 TIMES.
          l_cn = l_cn + 1.
          CONCATENATE 'IT_OUTPUT-RQTY_' l_cn INTO l_text_01.
          ASSIGN (l_text_01) TO <fs01>.
          CONCATENATE 'IT_DAILY-D' l_cn '_QTY' INTO l_text_02.
          ASSIGN (l_text_02) TO <fs02>.
          IF sy-subrc EQ 0.
            <fs02> = <fs01>.
          ENDIF.
        ENDDO.
        it_daily-cmpy_cd = 'HMMA'.
        it_daily-crtn_ymd = sy-datum.
        it_daily-plnt_cd = 'HVA1'.
        it_daily-part_cd = it_output-matnr.
        it_daily-part_grp =  it_output-matkl.
        CASE it_output-seq.
          WHEN '1'.
            it_daily-item = 'A'.
            l_labst = it_output-labst.
          WHEN '2'.
            it_daily-item = 'B'.
          WHEN '3'.
            it_daily-item = 'C'.
            it_daily-init_qty = l_labst.
            CLEAR: l_labst.
        ENDCASE.
        it_daily-inp_ymd = sy-datum.
        it_daily-ipe_eeno = sy-uname.
        it_daily-createtime = sy-uzeit.
        APPEND it_daily.
        CLEAR: it_daily.
        UNASSIGN: <fs02>, <fs01>.
      ENDIF.
    ENDLOOP.
    DELETE FROM ztmm_gaps_daily WHERE crtn_ymd = w_prd_date.
    INSERT ztmm_gaps_daily FROM TABLE it_daily
             ACCEPTING DUPLICATE KEYS.
    IF sy-subrc <> 0.
      MESSAGE i009 WITH 'Update table error'.
    ENDIF.
  ELSE.
** Weekly
    LOOP AT it_output.

      IF r02 = 'X' AND it_output-bklas <> '3000'.
        CONTINUE.
      ENDIF.
      it_weekly-src_cd = 'K'.
      IF it_output-lifnr = 'SEF9'.
        it_weekly-vend_cd = 'G'.
      ELSE.
        it_weekly-vend_cd = 'H'.
      ENDIF.
      IF r03 = 'X' AND it_output-bklas <> '3000'.
        it_weekly-src_cd = 'V'.
** On 01/22/13
        if it_output-lifnr is INITIAL.
           it_weekly-vend_cd = 'ZZZZ'.
        else.
** End on 01/22/13
            it_weekly-vend_cd = it_output-lifnr .
        ENDIF.
      ENDIF.

      l_cn = '00'.
      DO 45 TIMES.
        l_cn = l_cn + 1.
        CONCATENATE 'IT_OUTPUT-RQTY_' l_cn INTO l_text_01.
        ASSIGN (l_text_01) TO <fs01>.
        CONCATENATE 'IT_WEEKLY-W' l_cn '_QTY' INTO l_text_02.
        ASSIGN (l_text_02) TO <fs02>.
        IF sy-subrc EQ 0.
          <fs02> = <fs01>.
        ENDIF.
      ENDDO.
      it_weekly-cmpy_cd = 'HMMA'.
      it_weekly-crtn_ymd = w_prd_date.
      it_weekly-plnt_cd = 'HVA1'.
      it_weekly-part_cd = it_output-matnr.
      it_weekly-part_grp =  it_output-matkl.
      CASE it_output-seq.
        WHEN '1'.
          it_weekly-item = 'A'.
          l_labst = it_output-labst.
        WHEN '2'.
          it_weekly-item = 'B'.
        WHEN '3'.
          it_weekly-item = 'C'.
          it_weekly-init_qty = l_labst.
          CLEAR: l_labst.
      ENDCASE.
      it_weekly-inp_ymd = sy-datum.
      it_weekly-ipe_eeno = sy-uname.
      it_weekly-createtime = sy-uzeit.
      APPEND it_weekly.
      CLEAR: it_weekly.
      UNASSIGN: <fs02>, <fs01>.
    ENDLOOP.

    IF r02 = 'X'.
      DELETE FROM ztmm_gaps_weekly WHERE crtn_ymd = w_prd_date.
      INSERT ztmm_gaps_weekly FROM TABLE it_weekly
        ACCEPTING DUPLICATE KEYS.
      IF sy-subrc <> 0.
        MESSAGE i009 WITH 'Update table error'.
      ENDIF.
    ENDIF.

** monthly
    IF r03 = 'X'.
      LOOP AT it_weekly.
        IF  it_weekly-item = 'A'.
          MOVE-CORRESPONDING it_weekly TO it_month.
          CLEAR: it_month-pln_m01, it_month-pln_m02,
          it_month-pln_m03,it_month-pln_m04,it_month-pln_m05.
          l_cn = '01'.
          l_m_cn = '01'.
          l_yymm = w_prd_date+0(6).
          CONCATENATE l_yymm '01' INTO l_date_from.
          CALL FUNCTION 'JVA_LAST_DATE_OF_MONTH'
            EXPORTING
              year_month         = l_yymm
            IMPORTING
              last_date_of_month = l_date_to
            EXCEPTIONS
              invalide_month     = 1
              OTHERS             = 2.
          IF sy-subrc <> 0.
          ENDIF.
          DO 5 TIMES.
            CONCATENATE 'IT_MONTH-PLN_M' l_m_cn INTO l_text_02.
            ASSIGN (l_text_02) TO <fs02>.
            READ TABLE it_week INDEX l_cn.

            LOOP AT it_week WHERE seq >= l_cn.
              IF it_week-datum BETWEEN l_date_from AND l_date_to.
                CONCATENATE 'IT_WEEKLY-W' it_week-seq '_QTY'
                       INTO l_text_01.
                ASSIGN (l_text_01) TO <fs01>.
                IF sy-subrc EQ 0.
                  <fs02> = <fs02> + <fs01>.
                ENDIF.
              ELSE.
                EXIT.
              ENDIF.
            ENDLOOP.

            l_cn = it_week-seq.
            l_m_cn = l_m_cn + 1.

            UNASSIGN: <fs02>, <fs01>.
            l_date_from = l_date_to + 1.
            l_yymm = l_date_from+0(6).
            CALL FUNCTION 'JVA_LAST_DATE_OF_MONTH'
              EXPORTING
                year_month         = l_yymm
              IMPORTING
                last_date_of_month = l_date_to
              EXCEPTIONS
                invalide_month     = 1
                OTHERS             = 2.
            IF sy-subrc <> 0.

            ENDIF.
          ENDDO.
          IF it_month-pln_m01 + it_month-pln_m02 + it_month-pln_m03
              + it_month-pln_m04 + it_month-pln_m05 > 0.
            it_month-crtn_ymd = w_prd_date+0(6).
            APPEND it_month.
            CLEAR: it_month.
          ENDIF.
        ENDIF.
      ENDLOOP.
      l_yymm = w_prd_date+0(6).
      DELETE FROM ztmm_gaps_month WHERE crtn_ymd = l_yymm.
      INSERT ztmm_gaps_month FROM TABLE it_month
       ACCEPTING DUPLICATE KEYS.
      IF sy-subrc <> 0.
        MESSAGE i009 WITH 'Update table error'.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " MAKE_INTERFACE_DATA
*&---------------------------------------------------------------------*
*&      Form  SEND_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_data.
  DATA: l_result(1),
        zmsg(255),
        l_totrec TYPE i,
        l_srec TYPE i,
        l_frec TYPE i,
        l_msgtxt(60),
        l_type(40).

  w_dest = 'WMHR01'.

  CASE 'X'.
    WHEN r01.
      DESCRIBE TABLE it_daily LINES l_totrec.

      CALL FUNCTION 'ZMMIO_GAPS_DAILY'
        DESTINATION w_dest
        IMPORTING
          flag   = l_result
          msg    = zmsg
        TABLES
          t_item = it_daily.
*    EXCEPTIONS
*            communication_failure = 1 MESSAGE l_msgtxt
*            system_failure        = 2 MESSAGE l_msgtxt.

      IF l_result = 'S' OR l_result = 's'.
        WRITE: / 'Successfully sent'.
      ELSE.
        WRITE: / 'EAI Error'.
      ENDIF.
      LOOP AT it_daily.
        it_daily-ifresult = l_result.
        it_daily-iffailmsg = zmsg.
        MODIFY it_daily TRANSPORTING ifresult iffailmsg.
      ENDLOOP.

      MODIFY ztmm_gaps_daily FROM TABLE it_daily.

    WHEN r02.
      DESCRIBE TABLE it_weekly LINES l_totrec.

      CALL FUNCTION 'ZMMIO_GAPS_WEEKLY'
        DESTINATION w_dest
        IMPORTING
          flag   = l_result
          msg    = zmsg
        TABLES
          t_item = it_weekly.

      IF l_result = 'S' OR l_result = 's'.
        WRITE: / 'Successfully sent'.
      ELSE.
        WRITE: / 'EAI Error'.
      ENDIF.
      LOOP AT it_weekly.
        it_weekly-ifresult = l_result.
        it_weekly-iffailmsg = zmsg.
        MODIFY it_weekly TRANSPORTING ifresult iffailmsg.
      ENDLOOP.

      MODIFY ztmm_gaps_weekly FROM TABLE it_weekly .
    WHEN r03.
      DESCRIBE TABLE it_month LINES l_totrec.

      CALL FUNCTION 'ZMMIO_GAPS_MONTHLY'
        DESTINATION w_dest
        IMPORTING
          flag   = l_result
          msg    = zmsg
        TABLES
          t_item = it_month.

      IF l_result = 'S' OR l_result = 's'.
        WRITE: / 'Successfully sent'.
      ELSE.
        WRITE: / 'EAI Error'.
      ENDIF.
      LOOP AT it_month.
        it_month-ifresult = l_result.
        it_month-iffailmsg = zmsg.
        MODIFY it_month TRANSPORTING ifresult iffailmsg.
      ENDLOOP.

*      DELETE ztmm_gaps_month FROM TABLE it_month.
      MODIFY ztmm_gaps_month FROM TABLE it_month.
       IF sy-subrc <> 0.
        MESSAGE i009 WITH 'Error: table update'.
      ENDIF.
  ENDCASE.

  COMMIT WORK.
*  IF sy-subrc = 0.
*    COMMIT WORK.
*  ELSE.
*    ROLLBACK WORK.
*  ENDIF.
  IF NOT it_error[] IS INITIAL.
    PERFORM send_email USING l_type .
  ENDIF.
ENDFORM.                    " SEND_DATA
**&---------------------------------------------------------------------
*
*&      Form  SEND_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_email USING p_par.
  DATA: lt_body LIKE TABLE OF solisti1 WITH HEADER LINE.

  DATA: l_subject TYPE p15_text150,
        l_p_rec_type  LIKE  somlreci1-rec_type.

  MOVE 'Following items with EAI errors:' TO lt_body.
  APPEND lt_body.
  CLEAR: lt_body.
  MOVE '================================' TO lt_body.
  APPEND lt_body.
  CLEAR: lt_body.
  MOVE: p_par TO lt_body.
  APPEND lt_body.
  CLEAR: lt_body.

  MOVE: 'Date' TO lt_body+0(10),
        'Part No' TO lt_body+10(20),
        'Item' TO lt_body+30(5).

  APPEND lt_body.
  CLEAR: lt_body.

  MOVE: '----------' TO  lt_body+0(10),
        '--------------------' TO  lt_body+10(20),
        '-----' TO  lt_body+30(5).
  APPEND lt_body.
  CLEAR: lt_body.

  LOOP AT it_error.
    MOVE: it_error-crtn_ymd TO lt_body+0(10),
           it_error-part_cd TO lt_body+10(20),
          it_error-item TO lt_body+30(5).
    APPEND lt_body.
  ENDLOOP.

  CALL FUNCTION 'ZCAF_SEND_EMAIL'
    EXPORTING
      p_subject  = 'V-Steel interface error - BLANK'
      p_rec_type = 'C'
      p_receiver = p_rver
    TABLES
      pt_body    = lt_body.

ENDFORM.                    " SEND_EMAIL
*&---------------------------------------------------------------------*
*&      Form  GET_VENDOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TAB_TEMP_LIFNR  text
*----------------------------------------------------------------------*
FORM get_vendor USING p_lifnr.
  DATA: l_lifnr LIKE it_tab_temp-lifnr.
  DATA: lt_a018 LIKE a018 OCCURS 0 WITH HEADER LINE.

  SELECT SINGLE lifnr INTO l_lifnr
    FROM ekko AS a INNER JOIN ekpo AS b
                      ON a~mandt EQ b~mandt
                     AND a~ebeln EQ b~ebeln
   WHERE matnr   EQ it_tab_temp-matnr
*     AND werks   EQ p_werks "COMMENT BY CHRIS ON 06/28/2005
     AND a~loekz EQ space
     AND b~loekz EQ space
     AND kdatb   <= w_prd_date
     AND kdate   >= w_prd_date.

  p_lifnr = l_lifnr.
  IF p_lifnr IS INITIAL.

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE lt_a018
      FROM a018
     WHERE kappl =  'M'
       AND kschl =  'PB00'
       AND matnr =  it_tab_temp-matnr
       AND ekorg =  'PU01'
       AND esokz =  '0'
       AND datab <= w_prd_date
       AND datbi >= w_prd_date.

    LOOP AT lt_a018.
      SELECT SINGLE a~lifnr INTO l_lifnr
        FROM eina AS a INNER JOIN eine AS b
          ON a~infnr = b~infnr
       WHERE a~matnr = it_tab_temp-matnr
         AND a~lifnr = lt_a018-lifnr
         AND a~loekz = ' '
         AND b~werks = ' '
         AND b~ekorg = 'PU01'
         AND b~loekz = ' '.
      IF sy-subrc NE 0.
        DELETE lt_a018.
      ENDIF.
    ENDLOOP.

    SORT lt_a018 BY datab DESCENDING.

    READ TABLE lt_a018 INDEX 1.
    IF sy-subrc = 0.
      MOVE: lt_a018-lifnr TO p_lifnr.
    ELSE.
      p_lifnr = 'ZZZZ'.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_VENDOR

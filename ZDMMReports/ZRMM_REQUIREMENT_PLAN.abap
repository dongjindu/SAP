************************************************************************
* Program Name      : ZRMM_REQUIREMENT_PLAN
* Creation Date     : 03/10/2006
* Development Request No :
* Addl Documentation:
* Description       : Dash Board
*
* Modification Logs
* Date       Developer RequestNo      Description
* 02.21.2012 J.C.MUN   UD1K954006     Date check(Saturday, Sunday).
************************************************************************

REPORT zrmm_requirement_plan NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmmm.

TABLES: lfa1,mara,mard,marc,ekko,ekpo.
TYPE-POOLS slis.
DATA : BEGIN OF it_tab_temp OCCURS 0,
      lifnr LIKE lfa1-lifnr,
      werks LIKE marc-werks,
      matnr LIKE mara-matnr,
      dispo LIKE marc-dispo,
      eisbe LIKE marc-eisbe,
      maktx LIKE makt-maktx,
      matkl LIKE mara-matkl,
      END OF it_tab_temp.

DATA: BEGIN OF it_tab OCCURS 0,
      lifnr LIKE lfa1-lifnr,
      werks LIKE marc-werks,
      matnr LIKE mara-matnr,
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
      total LIKE mdsm-bdmng,
      eisbe LIKE marc-eisbe,
      maktx LIKE makt-maktx,
      wmstk LIKE mard-labst,
      cogi LIKE affw-erfmg,
      matkl LIKE mara-matkl,
      END OF it_tab.

DATA : BEGIN OF it_affw OCCURS 0,
      matnr LIKE mara-matnr,
      shkzg LIKE affw-shkzg,
      erfmg LIKE affw-erfmg,
      END OF it_affw.

DATA: BEGIN OF it_output OCCURS 0,
      lifnr LIKE lfa1-lifnr,
      werks LIKE marc-werks,
      matnr LIKE mara-matnr,
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
      if(4) TYPE c,
      eisbe LIKE marc-eisbe,
      maktx LIKE makt-maktx,
      wmstk LIKE mard-labst,
      cogi LIKE affw-erfmg,
      ct TYPE lvc_t_scol,
      matkl LIKE mara-matkl,
      END OF it_output.

DATA: BEGIN OF it_lips OCCURS 0,
      lifnr LIKE lfa1-lifnr,
      werks LIKE marc-werks,
      matnr LIKE mara-matnr,
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
      eisbe LIKE marc-eisbe,
      END OF it_lips.

DATA: BEGIN OF it_po OCCURS 0,
      lifnr LIKE lfa1-lifnr,
      werks LIKE marc-werks,
      matnr LIKE mara-matnr,
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
      eisbe LIKE marc-eisbe,
      END OF it_po.

DATA : BEGIN OF it_mard OCCURS 0,
       matnr LIKE mard-matnr,
       labst LIKE mard-labst,
       END OF it_mard.

DATA : BEGIN OF it_marc OCCURS 0,
         werks LIKE marc-werks,
         matnr LIKE marc-matnr,
         sobsl LIKE marc-sobsl,
       END OF it_marc.

DATA:  BEGIN OF it_mdsm OCCURS 0,
         werks LIKE marc-werks,
         matnr LIKE mara-matnr,
         bdter LIKE mdsm-bdter,
         bdmng LIKE mdsm-bdmng,
         sbnum LIKE mdsm-sbnum,
         sbpos LIKE mdsm-sbpos,
       END OF it_mdsm.

DATA : BEGIN OF it_mard_temp OCCURS 0,
*         werks LIKE marc-werks,
         matnr LIKE mard-matnr,
         labst LIKE mard-labst,
       END OF it_mard_temp.

DATA : BEGIN OF it_lqua OCCURS 0,
         werks LIKE marc-werks,
         matnr LIKE mara-matnr,
         gesme LIKE lqua-gesme,
       END OF it_lqua.

DATA : BEGIN OF it_lqua_cc OCCURS 0,
         werks LIKE marc-werks,
         matnr LIKE mara-matnr,
         gesme LIKE lqua-gesme,
       END OF it_lqua_cc.

DATA : BEGIN OF it_lqua_al OCCURS 0,
         werks LIKE marc-werks,
         matnr LIKE mara-matnr,
         gesme LIKE lqua-gesme,
       END OF it_lqua_al.

DATA: BEGIN OF it_day OCCURS 21,
        seq(2)  TYPE n,
        datum   LIKE   sy-datum,
      END   OF it_day.

DATA: BEGIN OF it_week OCCURS 21,
        seq(2)  TYPE n,
        datum   LIKE   sy-datum,
      END   OF it_week.

DATA: BEGIN OF it_po_data OCCURS 0,
       vendor LIKE ekko-lifnr,
       matl_group LIKE mara-matkl,
       material LIKE mara-matnr,
       plant LIKE marc-werks,
       stge_loc LIKE mard-lgort,
       po_unit LIKE mara-bstme,
       quantity LIKE ekpo-menge,
       round_qty LIKE ekpo-menge,
       order_qty LIKE ekpo-menge,
       desc LIKE makt-maktx,
       po_no LIKE ekko-ebeln,
      END OF it_po_data.

DATA: p_del_date LIKE sy-datum.
** Added by Furong on 03/17/10
DATA: w_prd_date LIKE sy-datum.
** end of addition

DATA: ok_code      LIKE sy-ucomm,
      w_repid  LIKE sy-repid,
      w_cnt       TYPE   i,
      w_no_data(1).

DATA:  l_kalid LIKE kako-kalid.

DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldcat_fi  TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldcat_co  TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE,
       it_fieldcat_det TYPE lvc_t_fcat WITH HEADER LINE. "/Detail

DATA : wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
       w_fieldname    LIKE LINE OF it_fieldcat.

DATA: wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
      wa_variant TYPE disvariant.      "for parameter IS_VARIANT

DATA: wa_custom_control TYPE        scrfname VALUE 'ALV_CONTAINER',
      alv_grid          TYPE REF TO cl_gui_alv_grid,
*      grid_container    TYPE REF TO cl_gui_custom_container.
      g_docking_container    TYPE REF TO cl_gui_docking_container.

DATA: wa_custom_control_po TYPE scrfname VALUE 'ALV_CONTAINER_210',
      alv_grid_po TYPE REF TO cl_gui_alv_grid,
      grid_container_po TYPE REF TO cl_gui_custom_container.

FIELD-SYMBOLS : <fs01>, <fs02>, <fs-qty>.

**--- Ranges
RANGES : r_lgort FOR mard-lgort,
         r_lgtyp FOR lqua-lgtyp,
         r_pedtr FOR plaf-pedtr,
         r_pedtr3 FOR plaf-pedtr.

DATA: dsn(4).

**--- Constants
CONSTANTS : c_profl LIKE mara-profl VALUE 'K',
            c_lgnum LIKE t300-lgnum VALUE 'P01',
            c_plscn LIKE plaf-plscn VALUE '900'.

DATA:  w_refresh(1),
       w_new(1) VALUE 'X'.

SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.

SELECT-OPTIONS : s_lifnr FOR lfa1-lifnr OBLIGATORY,
                 s_matnr FOR mara-matnr,
                 s_matkl FOR mara-matkl,
                 s_dispo FOR marc-dispo,
                 s_profl FOR mara-profl.
PARAMETERS : p_werks LIKE t001w-werks OBLIGATORY DEFAULT 'P001'.
PARAMETERS : p_day TYPE p DECIMALS 1 OBLIGATORY DEFAULT '21.0'.
PARAMETERS : p_pwerks LIKE t001w-werks OBLIGATORY DEFAULT 'P001'.
SELECTION-SCREEN END OF BLOCK block1.

SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-002.
PARAMETERS : r01 RADIOBUTTON GROUP rb1 DEFAULT 'X'.
PARAMETERS : r02 RADIOBUTTON GROUP rb1.
*SELECTION-SCREEN SKIP.
SELECTION-SCREEN ULINE.
PARAMETERS : inb RADIOBUTTON GROUP rb2 DEFAULT 'X'.
PARAMETERS : opo RADIOBUTTON GROUP rb2.
PARAMETERS : no RADIOBUTTON GROUP rb2.
*SELECTION-SCREEN SKIP.
SELECTION-SCREEN ULINE.
PARAMETERS : inv AS CHECKBOX.
PARAMETERS : p_sft AS CHECKBOX.
SELECTION-SCREEN ULINE.
PARAMETERS: stt RADIOBUTTON GROUP rb3.
PARAMETERS: stw RADIOBUTTON GROUP rb3 DEFAULT 'X'.
PARAMETERS: stc RADIOBUTTON GROUP rb3.
PARAMETERS: sta RADIOBUTTON GROUP rb3.
PARAMETERS: stp RADIOBUTTON GROUP rb3.
SELECTION-SCREEN ULINE.

SELECTION-SCREEN END OF BLOCK block2.

START-OF-SELECTION.
** Furong on 07/19/12 for 3 shift
*  IF SY-UZEIT >= '000000' AND SY-UZEIT <= '060000'.
  IF sy-uzeit >= '000000' AND sy-uzeit <= '062959'.
** End on 07/19/12
    w_prd_date = sy-datum - 1.
  ELSE.
    w_prd_date = sy-datum.
  ENDIF.
  WHILE w_refresh = 'X' OR w_new = 'X'.
    PERFORM get_data.
    IF w_no_data = 'X'.
      CLEAR: w_no_data.
      EXIT.
    ENDIF.
    PERFORM process_data.
    PERFORM display_data.
  ENDWHILE.

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
  IF w_no_data = 'X'.
    EXIT.
  ENDIF.
  IF inb = 'X'.
    PERFORM get_lips_data.
  ELSEIF opo = 'X'.
    PERFORM get_po_data.
  ENDIF.
ENDFORM.                    "GET_DATA
*---------------------------------------------------------------------*
*       FORM get_req_data                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM get_req_data.
**---
  DATA : l_qty LIKE mdsm-bdmng,
         l_date LIKE sy-datum,
         l_week_f LIKE sy-datum,
         l_cn(2) TYPE n,
         l_in_cn(2)  TYPE n,
         l_text(30),
         l_text_21t(30),
         l_date_curr LIKE sy-datum,
         l_date_1 LIKE sy-datum,
         l_date_2 LIKE sy-datum,
         l_date_3 LIKE sy-datum,
         l_in_date LIKE sy-datum,
         l_cn_week TYPE i.

  DATA : BEGIN OF lt_lqua_temp OCCURS 0,
         werks LIKE marc-werks,
         matnr LIKE mara-matnr,
         lqnum LIKE lqua-lqnum,
         gesme LIKE lqua-gesme,
         lgort LIKE mard-lgort,
       END OF lt_lqua_temp.

  DATA : BEGIN OF lt_affw_temp OCCURS 0,
      weblnr LIKE affw-weblnr,
      weblpos LIKE affw-weblpos,
      matnr LIKE mara-matnr,
      shkzg LIKE affw-shkzg,
      erfmg LIKE affw-erfmg,
      END OF lt_affw_temp.

  DATA: it_mrp LIKE TABLE OF ztmm_parts_21day WITH HEADER LINE,
       it_mdsm_temp LIKE TABLE OF it_mdsm WITH HEADER LINE.

  DATA: it_mard_2 LIKE TABLE OF it_mard WITH HEADER LINE.

  DATA: l_mrp_inx LIKE sy-index.

  RANGES: r_lgort FOR mard-lgort.

  CLEAR : it_tab, it_tab[], it_day, it_day[], it_mdsm,
          it_mdsm[].
  CLEAR: it_tab_temp, it_tab_temp[], it_mard_temp, it_mard_temp[],
         it_mard, it_mard[], it_lqua, it_lqua[].
  CLEAR: lt_affw_temp, lt_affw_temp[], it_affw, it_affw[].

  SELECT a~lifnr a~werks a~matnr c~dispo c~eisbe e~maktx
   b~matkl
   INTO TABLE it_tab_temp
   FROM eord AS a INNER JOIN marc AS c
     ON a~matnr = c~matnr
        AND a~werks = c~werks
     INNER JOIN mara AS b
     ON a~matnr = b~matnr
     INNER JOIN makt AS e
     ON b~matnr = e~matnr
     WHERE a~lifnr IN s_lifnr
       AND a~matnr IN s_matnr
       AND b~matkl IN s_matkl
       AND c~dispo IN s_dispo
       AND b~profl IN s_profl
       AND c~werks = p_werks
       AND a~notkz = space
    ORDER BY a~lifnr a~werks a~matnr.

  IF sy-subrc = 0.

    DELETE ADJACENT DUPLICATES FROM it_tab_temp.

    SELECT matnr labst INTO TABLE it_mard_temp
     FROM mard
     FOR ALL ENTRIES IN it_tab_temp
     WHERE matnr = it_tab_temp-matnr
       AND werks = it_tab_temp-werks
       AND lvorm = space
       AND ( lgort <> '9999' AND
             lgort <> 'P499' ).

    LOOP AT it_mard_temp.
      MOVE it_mard_temp TO it_mard.
      COLLECT it_mard.
      CLEAR: it_mard_temp, it_mard.
    ENDLOOP.

    CLEAR: it_mard_temp[].

    SELECT weblnr weblpos matnr shkzg erfmg INTO TABLE lt_affw_temp
     FROM affw
     FOR ALL ENTRIES IN it_tab_temp
     WHERE matnr = it_tab_temp-matnr.

    LOOP AT lt_affw_temp.
      MOVE-CORRESPONDING lt_affw_temp TO it_affw.
      COLLECT it_affw.
      CLEAR: lt_affw_temp, it_affw.
    ENDLOOP.

** for e002
*    IF P_WERKS = 'E001'.
    IF p_werks = 'E001' OR p_werks = 'E002'.
** End
      SELECT werks matnr sobsl INTO TABLE it_marc FROM marc
       FOR ALL ENTRIES IN it_mard
       WHERE matnr = it_mard-matnr
** for e002
*         AND WERKS = 'E001'.
         AND werks = p_werks.
** end e002
      SELECT matnr labst INTO TABLE it_mard_temp
       FROM mard
       FOR ALL ENTRIES IN it_tab_temp
       WHERE matnr = it_tab_temp-matnr
         AND werks = 'P001'
         AND lvorm = space
         AND ( lgort <> '9999' AND
               lgort <> 'P499' ).

      LOOP AT it_mard_temp.
        MOVE it_mard_temp TO it_mard_2.
        COLLECT it_mard_2.
        CLEAR: it_mard_temp, it_mard_2.
      ENDLOOP.

      LOOP AT it_mard.
        READ TABLE it_marc WITH KEY matnr = it_mard-matnr
** for e002
*                                    WERKS = 'E001'.
                                    werks = p_werks.
** end e002
        IF it_marc-sobsl = '40'.
          READ TABLE it_mard_2 WITH KEY matnr = it_mard-matnr.
          IF sy-subrc = 0.
            it_mard-labst = it_mard-labst + it_mard_2-labst.
            MODIFY it_mard.
          ENDIF.
        ENDIF.
      ENDLOOP.

      IF stw = 'X'.
        SELECT werks
               matnr
               lgort
               labst AS gesme
          INTO CORRESPONDING FIELDS OF TABLE lt_lqua_temp
          FROM mard

          FOR ALL ENTRIES IN it_mard

        WHERE matnr EQ it_mard-matnr
          AND werks EQ 'P001'
          AND NOT ( lgort LIKE 'E99%' AND
                lgort LIKE 'G99%' AND
                lgort LIKE 'P99%' ).

        SORT lt_lqua_temp BY werks matnr.
        LOOP AT lt_lqua_temp.
          MOVE-CORRESPONDING lt_lqua_temp TO it_lqua.
          COLLECT it_lqua.
          CLEAR: lt_lqua_temp, it_lqua.
        ENDLOOP.

        LOOP AT it_mard.
          READ TABLE it_marc WITH KEY matnr = it_mard-matnr
                                      werks = 'E001'.
          IF it_marc-sobsl = '40'.
            READ TABLE it_lqua WITH KEY matnr = it_mard-matnr.
            IF sy-subrc = 0.
              it_mard-labst = it_mard-labst - it_lqua-gesme.
              MODIFY it_mard.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ELSE.
      IF stw = 'X'.
        SELECT werks
               matnr
               lgort
               labst AS gesme
          INTO CORRESPONDING FIELDS OF TABLE lt_lqua_temp
          FROM mard

          FOR ALL ENTRIES IN it_mard

        WHERE matnr EQ it_mard-matnr
          AND werks EQ 'P001'
          AND NOT ( lgort LIKE '9999' OR
                lgort LIKE 'G99%' OR
                lgort LIKE 'P99%' OR
                lgort LIKE 'P499' ).

        SORT lt_lqua_temp BY werks matnr.
        LOOP AT lt_lqua_temp.
          MOVE-CORRESPONDING lt_lqua_temp TO it_lqua.
          COLLECT it_lqua.
          CLEAR: lt_lqua_temp, it_lqua.
        ENDLOOP.

        LOOP AT it_mard.

          READ TABLE it_lqua WITH KEY matnr = it_mard-matnr.
          IF sy-subrc = 0.
            it_mard-labst = it_lqua-gesme.
            MODIFY it_mard.
          ENDIF.

        ENDLOOP.
      ENDIF.

    ENDIF.

    CLEAR:lt_lqua_temp, lt_lqua_temp[].
    IF stc = 'X'.
**c___ Paul chnage 06/20/11
      SELECT werks
             matnr
             lgort
             labst AS gesme
        INTO CORRESPONDING FIELDS OF TABLE lt_lqua_temp
        FROM mard

        FOR ALL ENTRIES IN it_mard

      WHERE matnr EQ it_mard-matnr
*          AND WERKS EQ 'P001'
        AND lgort IN ('G100', 'G150').

      SORT lt_lqua_temp BY werks matnr.
      LOOP AT lt_lqua_temp.
        MOVE-CORRESPONDING lt_lqua_temp TO it_lqua_cc.
        COLLECT it_lqua_cc.
        CLEAR: lt_lqua_temp, it_lqua_cc.
      ENDLOOP.

      LOOP AT it_mard.
*      READ TABLE it_lqua WITH KEY werks = it_mard-werks
        READ TABLE it_lqua_cc WITH KEY matnr = it_mard-matnr.
        IF sy-subrc = 0.
          it_mard-labst = it_lqua_cc-gesme.
        ELSE.
          it_mard-labst = 0.
        ENDIF.
        MODIFY it_mard.
      ENDLOOP.

    ENDIF.

** Added on 10/12/12
    IF stp = 'X'.
      SELECT werks
             matnr
             lgort
             labst AS gesme
        INTO CORRESPONDING FIELDS OF TABLE lt_lqua_temp
        FROM mard
        FOR ALL ENTRIES IN it_mard
      WHERE matnr EQ it_mard-matnr
        AND lgort IN ('P400', 'G100').

      SORT lt_lqua_temp BY werks matnr.
      LOOP AT lt_lqua_temp.
        MOVE-CORRESPONDING lt_lqua_temp TO it_lqua_cc.
        COLLECT it_lqua_cc.
        CLEAR: lt_lqua_temp, it_lqua_cc.
      ENDLOOP.

      LOOP AT it_mard.
        READ TABLE it_lqua_cc WITH KEY matnr = it_mard-matnr.
        IF sy-subrc = 0.
          it_mard-labst = it_lqua_cc-gesme.
        ELSE.
          it_mard-labst = 0.
        ENDIF.
        MODIFY it_mard.
      ENDLOOP.

    ENDIF.
** End

    IF sta = 'X'.
**c___ Paul chnage 06/20/11
      SELECT werks
             matnr
             lgort
             labst AS gesme
        INTO CORRESPONDING FIELDS OF TABLE lt_lqua_temp
        FROM mard

        FOR ALL ENTRIES IN it_mard

      WHERE matnr EQ it_mard-matnr
        AND werks EQ 'P001'
        AND lgort EQ 'P400' .

      SORT lt_lqua_temp BY werks matnr.
      LOOP AT lt_lqua_temp.
        MOVE-CORRESPONDING lt_lqua_temp TO it_lqua_al.
        COLLECT it_lqua_al.
        CLEAR: lt_lqua_temp, it_lqua_al.
      ENDLOOP.

      LOOP AT it_mard.
        READ TABLE it_lqua_al WITH KEY matnr = it_mard-matnr.
        IF sy-subrc = 0.
          it_mard-labst = it_lqua_al-gesme.
        ELSE.
          it_mard-labst = 0.
        ENDIF.
        MODIFY it_mard.
      ENDLOOP.

** FOR E002
      REFRESH:  r_lgort.
      IF p_werks = 'E001'.
        r_lgort-option = 'EQ'.
        r_lgort-sign = 'I'.
        r_lgort-low = 'E100'.
        APPEND r_lgort.
        r_lgort-low = 'E200'.
        APPEND r_lgort.
        r_lgort-low = 'E300'.
        APPEND r_lgort.
      ELSEIF p_werks = 'E002'.
        r_lgort-option = 'EQ'.
        r_lgort-sign = 'I'.
        r_lgort-low = 'N100'.
        APPEND r_lgort.
        r_lgort-low = 'N200'.
        APPEND r_lgort.
        r_lgort-low = 'N300'.
        APPEND r_lgort.
      ENDIF.

*      IF P_WERKS = 'E001'.
      IF p_werks = 'E001' OR p_werks = 'E002'.
** end e002
        CLEAR: lt_lqua_temp, lt_lqua_temp[].
        CLEAR: it_lqua_al, it_lqua_al[].
        SELECT werks
               matnr
               lgort
               labst AS gesme
          INTO CORRESPONDING FIELDS OF TABLE lt_lqua_temp
          FROM mard

          FOR ALL ENTRIES IN it_mard

** FOR E002
*        WHERE MATNR EQ IT_MARD-MATNR
*          AND WERKS EQ 'E001'
*          AND LGORT IN ('E100', 'E200', 'E300').
        WHERE matnr EQ it_mard-matnr
          AND werks EQ p_werks
          AND lgort IN r_lgort.
** end e002

        SORT lt_lqua_temp BY werks matnr.
        LOOP AT lt_lqua_temp.
          MOVE-CORRESPONDING lt_lqua_temp TO it_lqua_al.
          COLLECT it_lqua_al.
          CLEAR: lt_lqua_temp, it_lqua_al.
        ENDLOOP.

        LOOP AT it_mard.
          READ TABLE it_lqua_al WITH KEY matnr = it_mard-matnr.
          IF sy-subrc = 0.
            it_mard-labst = it_mard-labst + it_lqua_al-gesme.
            MODIFY it_mard.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

    LOOP AT it_tab_temp.
      MOVE-CORRESPONDING it_tab_temp TO it_tab.
      READ TABLE it_mard WITH KEY matnr = it_tab-matnr.
      IF sy-subrc = 0.
        it_tab-labst = it_mard-labst.
      ENDIF.
      READ TABLE it_lqua WITH KEY matnr = it_tab-matnr.
      IF sy-subrc = 0.
        it_tab-wmstk = it_lqua-gesme.
      ENDIF.
      READ TABLE it_affw WITH KEY matnr = it_tab-matnr
                                  shkzg = 'H'.
      IF sy-subrc = 0.
        it_tab-cogi = it_affw-erfmg.
      ENDIF.
      READ TABLE it_affw WITH KEY matnr = it_tab-matnr
                                  shkzg = 'S'.
      IF sy-subrc = 0.
        it_tab-cogi = it_tab-cogi - it_affw-erfmg.
      ENDIF.
      it_tab-rqty_p = it_tab-cogi.
      IF p_sft = 'X'.
        it_tab-rqty_p = it_tab-rqty_p + it_tab-eisbe.
      ENDIF.
      COLLECT it_tab.
      CLEAR: it_tab_temp, it_tab.
    ENDLOOP.

* reading working calendar
    SELECT SINGLE kalid INTO l_kalid
      FROM zvpp_capacity
     WHERE arbpl = 'T'   .

    PERFORM read_shop_calid  USING l_kalid.

    SELECT * INTO TABLE it_mrp
     FROM ztmm_parts_21day
     FOR ALL ENTRIES IN it_tab_temp
     WHERE matnr = it_tab_temp-matnr
       AND lifnr = it_tab_temp-lifnr
** Changed by Furong on 03/21/12 for e002
*       AND werks = it_tab_temp-werks
       AND werks = p_pwerks
** End of change on 03/21/12
        AND datum = w_prd_date.
    PERFORM set_days.

*-- MRP

    IF r01 = 'X'.

      LOOP AT it_tab.
        l_cn = '00'.
        l_date = w_prd_date.

** Changed by Furong on 03/21/12 for E002 requested by Prasad
*        READ TABLE it_mrp WITH KEY werks = it_tab-werks
*                                   matnr = it_tab-matnr
*                                   lifnr = it_tab-lifnr.
        READ TABLE it_mrp WITH KEY werks = p_pwerks
                                   matnr = it_tab-matnr
                                   lifnr = it_tab-lifnr.
** end on 03/21/12
        IF sy-subrc = 0.
          it_tab-rqty_p = it_tab-rqty_p + it_mrp-bfd.
          DO 21 TIMES.
            l_cn = l_cn + 1.
            CONCATENATE 'IT_TAB-RQTY_' l_cn INTO l_text.
            ASSIGN (l_text) TO <fs-qty>.
            READ TABLE it_day WITH KEY seq = l_cn.
            IF sy-subrc = 0.
              CONCATENATE 'IT_MRP-D' it_day-seq INTO  l_text_21t.
              ASSIGN (l_text_21t) TO <fs01>.
              <fs-qty> = <fs01>.
            ENDIF.
            l_date = l_date + 1.
          ENDDO.
         it_tab-total = it_tab-rqty_p + it_tab-rqty_01 + it_tab-rqty_02
                     + it_tab-rqty_03 + it_tab-rqty_04 + it_tab-rqty_05
                     + it_tab-rqty_06 + it_tab-rqty_07 + it_tab-rqty_08
                     + it_tab-rqty_09 + it_tab-rqty_10 + it_tab-rqty_11
                     + it_tab-rqty_12 + it_tab-rqty_13 + it_tab-rqty_14
                     + it_tab-rqty_15 + it_tab-rqty_16 + it_tab-rqty_17
                     + it_tab-rqty_18 + it_tab-rqty_19 + it_tab-rqty_20
                          + it_tab-rqty_21.
        ENDIF.
        it_tab-seq = '1'.
        MODIFY it_tab.
      ENDLOOP.
    ENDIF.

*-- long term planning
    IF r02 = 'X'.
      CLEAR: it_week, it_week[].
      l_date = w_prd_date.

      CALL FUNCTION 'HR_GBSSP_GET_WEEK_DATES'
        EXPORTING
          p_pdate       = l_date
        IMPORTING
          p_sunday      = l_date
*         P_SATURDAY    =
*         P_DAY_IN_WEEK =
*         P_WEEK_NO     =
        .
      l_date = l_date + 1.                                  "03/13
      l_cn = '01'.
      CLEAR: l_cn_week.
      DO 21 TIMES.

        it_week-seq = l_cn.
        l_date_1 = l_date.
        PERFORM read_working_date USING '+'  l_kalid  l_date_1.
        l_date = l_date + 7.
        IF l_date_1 > l_date.
          l_date = l_date + 7.
          l_cn_week = l_cn_week + 1.
        ELSE.
          IF l_date = l_date_1.
            l_date_1 = l_date_1 - 7.
          ENDIF.
        ENDIF.
        it_week-datum = l_date_1.
        APPEND it_week.
        l_cn = l_cn + 1.

      ENDDO.

      READ TABLE it_week INDEX 3.
      l_date_2 = it_week-datum.
      READ TABLE it_week INDEX 4.
      l_date_3 = it_week-datum.

*** On 12/17/12
      IF l_cn_week = 1.
        l_date_3 =  l_date_2.
      ELSE.
        IF l_cn_week = 2.
          l_date_3 =  l_date_1.
          l_date_2 =  l_date_1.
        ENDIF.
      ENDIF.
** End

      SELECT  matnr werks bdter bdmng sbnum sbpos
        INTO CORRESPONDING FIELDS OF TABLE it_mdsm_temp
      FROM mdsm
      FOR ALL ENTRIES IN it_tab
       WHERE matnr = it_tab-matnr
** on 03/21/12 for E002
*         AND werks = it_tab-werks
         AND werks = p_pwerks
** End on 03/21/12
         AND plscn = '900'
         AND bdter >= l_date_3.

      LOOP AT it_mdsm_temp.
        it_mdsm = it_mdsm_temp.
        it_mdsm-sbnum = ''.
        it_mdsm-sbpos = ''.
        COLLECT it_mdsm.
        CLEAR: it_mdsm, it_mdsm_temp.
      ENDLOOP.
      SORT it_mdsm BY werks matnr bdter.
      LOOP AT it_tab.

        READ TABLE it_week INDEX 1.
        l_week_f = it_week-datum.

        l_mrp_inx = 2.

** Changed by Furong on 03/21/12 for E002 requested by Prasad
*        READ TABLE it_mrp WITH KEY werks = it_tab-werks
*                                   matnr = it_tab-matnr
**                                 datum = l_date
*                                   lifnr = it_tab-lifnr.
        READ TABLE it_mrp WITH KEY werks = p_pwerks
                                   matnr = it_tab-matnr
                                   lifnr = it_tab-lifnr.
** End on 03/21/12

        IF sy-subrc = 0.
          it_tab-rqty_p = it_tab-rqty_p + it_mrp-bfd + it_tab-cogi.

          l_text = 'IT_TAB-RQTY_01'.
          ASSIGN (l_text) TO <fs-qty>.
          READ TABLE it_week INDEX l_mrp_inx.

          LOOP AT it_day WHERE ( datum >= l_week_f
                             AND datum < it_week-datum ).
            CONCATENATE 'it_mrp-D' it_day-seq INTO  l_text_21t.
            ASSIGN (l_text_21t) TO <fs01>.
            IF sy-subrc = 0.
              <fs-qty> = <fs-qty> + <fs01>.
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
            ENDIF.
            UNASSIGN <fs01>.
          ENDLOOP.
        ENDIF.

*** On 12/17/12
        CASE l_cn_week.
          WHEN 0.
            l_cn = '03'.
          WHEN 1.
            l_cn = '02'.
          WHEN 2.
            l_cn = '01'.
        ENDCASE.
*    L_CN = '03'.
** End
*** On 12/21/12
        IF it_tab-rqty_03 IS INITIAL.
          l_cn = '02'.
        ELSE.
          l_cn = '03'.
        ENDIF.
        UNASSIGN <fs-qty>.
** END
        DO 18 TIMES.
          l_cn = l_cn + 1.
          READ TABLE it_week WITH KEY seq = l_cn.
          l_date = it_week-datum.
          IF l_cn = 21.
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
                          + it_tab-rqty_21.
        MODIFY it_tab.
      ENDLOOP.
    ENDIF.
    IF inv = 'X'.
      DELETE it_tab WHERE total = 0.
    ELSE.
      DELETE it_tab WHERE labst = 0 AND total = 0.
    ENDIF.
  ELSE.
    w_no_data = 'X'.
    MESSAGE i000(zz) WITH  'No data found'.
  ENDIF.
ENDFORM.                    " get_req_data

*&---------------------------------------------------------------------*
*&      Form  check_input_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_input_value.

** added by furong check s_matnr
  IF s_matnr-low IS INITIAL AND s_matnr-high IS INITIAL.
    s_matnr-high = 'ZZZZZZZ'.
  ELSEIF s_matnr-high IS INITIAL.
    s_matnr-high = s_matnr-low.
  ENDIF.

  IF s_dispo-low IS INITIAL AND s_dispo-high IS INITIAL.
    s_dispo-high = 'ZZZ'.
  ELSEIF s_dispo-high IS INITIAL.
    s_dispo-high = s_dispo-low.
  ENDIF.
** end of addition
ENDFORM.                    " check_input_value
*&---------------------------------------------------------------------*
*&      Form  check_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_screen.
  LOOP AT SCREEN.
    IF screen-name = 'P_EXCEL'.
      screen-input = 0.
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " check_screen
*---------------------------------------------------------------------*
*       FORM set_days                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM set_days.
  DATA: l_count TYPE i.
  DATA: l_date LIKE sy-datum.

  it_day-seq = 1.
  it_day-datum = w_prd_date.
  APPEND it_day.
  l_count = '01'.
  l_date = w_prd_date.

  DO 20 TIMES.
    l_count  = l_count + 1.
    l_date   = l_date  + 1.
    PERFORM read_working_date USING '+' l_kalid  l_date.
    it_day-seq     = l_count.
    it_day-datum   = l_date .
    APPEND it_day.  CLEAR: it_day.
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
  DATA: it_color TYPE lvc_t_scol,
        wa_color LIKE LINE OF it_color.

  CLEAR: it_output, it_output[].

  SORT it_tab BY lifnr werks matnr dispo.
  LOOP AT it_tab.
    CLEAR: wa_bal, l_flag.
    MOVE-CORRESPONDING it_tab TO it_output.

    IF inb = 'X'.
      READ TABLE it_lips WITH KEY matnr = it_tab-matnr
                                  lifnr  = it_tab-lifnr.
      IF sy-subrc NE 0.
        it_lips-lifnr = it_tab-lifnr.
        it_lips-matnr = it_tab-matnr.
        it_lips-werks = it_tab-werks.
      ENDIF.
      it_lips-dispo = it_tab-dispo.
      it_lips-eisbe = it_tab-eisbe.
      it_lips-seq = '2'.
      l_qty = it_tab-labst + it_lips-rqty_p - it_tab-rqty_p.
      it_output-days = 0.

      wa_bal-seq = '3'.
      wa_bal-rqty_p = l_qty.
      l_cn = '00'.
      DO 21 TIMES.
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
        IF it_output-days < p_day.
          wa_color-color-col = 6.
          wa_color-color-int = 1.
          wa_color-fname = 'DAYS'.
          APPEND wa_color TO it_color.
          it_output-ct = it_color.
          CLEAR: wa_color, it_color, it_color[].
        ENDIF.

        IF l_line = 1.
          it_output-if = 'C210'.
        ENDIF.

        it_output-seq = '1'.
        APPEND it_output.
        it_lips-seq = '2'.
        CLEAR: it_output.
        CLEAR: it_lips-lifnr, it_lips-matnr,it_lips-werks,
               it_lips-dispo, it_lips-eisbe.
        MOVE-CORRESPONDING it_lips TO it_output.
      ELSE.
        CLEAR: wa_bal, it_lips, it_tab, it_output.
        CONTINUE.
      ENDIF.
    ELSE.
**Open PO
      READ TABLE it_po WITH KEY matnr = it_tab-matnr
                                lifnr  = it_tab-lifnr.
      IF sy-subrc NE 0.
        it_po-lifnr = it_tab-lifnr.
        it_po-matnr = it_tab-matnr.
        it_po-werks = it_tab-werks.
      ENDIF.
      it_po-dispo = it_tab-dispo.
      it_po-eisbe = it_tab-eisbe.
      it_po-seq = '2'.
      l_qty = it_tab-labst + it_po-rqty_p - it_tab-rqty_p.
      it_output-days = 0.

      wa_bal-seq = '3'.
      wa_bal-rqty_p = l_qty.
      l_cn = '00'.
      DO 21 TIMES.
        l_cn = l_cn + 1.
        CONCATENATE 'IT_TAB-RQTY_' l_cn INTO l_text_01.
        ASSIGN (l_text_01) TO <fs01>.
        CONCATENATE 'IT_PO-RQTY_' l_cn INTO l_text_02.
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
              it_output-days = it_output-days +
              ( <fs01> + l_qty ) / <fs01>.
            ENDIF.
          ENDIF.
        ENDIF.
        UNASSIGN: <fs-qty>, <fs02>, <fs01>.
      ENDDO.
      IF it_output-days <= p_day.
        IF it_output-days < p_day.
          wa_color-color-col = 6.
          wa_color-color-int = 1.
          wa_color-fname = 'DAYS'.
          APPEND wa_color TO it_color.
          it_output-ct = it_color.
          CLEAR: wa_color, it_color, it_color[].
        ENDIF.

        IF l_line = 1.
          it_output-if = 'C210'.
        ENDIF.

        it_output-seq = '1'.
        APPEND it_output.
        it_po-seq = '2'.
        CLEAR: it_output.
        CLEAR: it_po-lifnr, it_po-matnr,it_po-werks,
               it_po-dispo, it_po-eisbe.
        MOVE-CORRESPONDING it_po TO it_output.
      ELSE.
        CLEAR: wa_bal, it_po, it_tab, it_output.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF l_line = 1.
      it_output-if = 'C210'.
    ENDIF.
    APPEND it_output.
    CLEAR: it_output.
    IF l_line = 1.
      wa_bal-if = 'C210'.
    ENDIF.
    APPEND wa_bal TO it_output.
    IF l_line = 0.
      l_line = 1.
    ELSE.
      l_line = 0.
    ENDIF.

    CLEAR: wa_bal, it_lips, it_po, it_tab, it_output.
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
ENDFORM.                    " READ_WORKING_DATE
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  CALL SCREEN 200.
ENDFORM.                    " display_data
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
  DATA: lt_lips_2 LIKE TABLE OF lt_lips_temp WITH HEADER LINE.

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
    READ TABLE it_day INDEX 21.
    l_end_date = it_day-datum.
  ENDIF.
  IF r02 = 'X'.
    READ TABLE it_week INDEX 1.
    l_st_date = it_week-datum.
    READ TABLE it_week INDEX 21.
    l_end_date = it_week-datum.

*    CALL FUNCTION 'HR_GBSSP_GET_WEEK_DATES'
*         EXPORTING
*              p_pdate    = l_end_date
*         IMPORTING
*              p_saturday = l_end_date.
*    l_end_date = l_end_date + 1.
  ENDIF.

*  SELECT a~lifnr b~matnr a~lfdat lfimg
*   INTO CORRESPONDING FIELDS OF TABLE lt_lips_temp
*   FROM likp AS a INNER JOIN lips AS b
*   ON a~vbeln = b~vbeln
*   INNER JOIN vbuk AS c
*   ON a~vbeln = c~vbeln
*   WHERE lifnr IN s_lifnr
*     AND matnr IN s_matnr
*     AND b~werks = p_werks
*     AND wbstk = 'A'
*     AND lfdat <= l_end_date.

  SELECT a~lifnr b~matnr d~eindt AS lfdat d~menge AS lfimg d~dabmg
   INTO CORRESPONDING FIELDS OF TABLE lt_lips_temp
   FROM ekko AS a INNER JOIN ekpo AS b
   ON a~ebeln = b~ebeln
   INNER JOIN ekes AS d
   ON d~ebeln = b~ebeln
   AND d~ebelp = b~ebelp
   INNER JOIN vbuk AS c
   ON d~vbeln = c~vbeln
   WHERE lifnr IN s_lifnr
     AND matnr IN s_matnr
     AND b~werks = p_werks
     AND c~wbstk = 'A'
     AND c~vbtyp = '7'
     AND d~eindt <= l_end_date
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
** FOR E002
*  IF P_WERKS = 'E001'.
  IF p_werks = 'E001' OR p_werks = 'E002'.
** END E002
    SELECT a~lifnr b~matnr d~eindt AS lfdat d~menge AS lfimg d~dabmg
     INTO CORRESPONDING FIELDS OF TABLE lt_lips_temp
     FROM ekko AS a INNER JOIN ekpo AS b
     ON a~ebeln = b~ebeln
     INNER JOIN ekes AS d
     ON d~ebeln = b~ebeln
     AND d~ebelp = b~ebelp
     INNER JOIN vbuk AS c
     ON d~vbeln = c~vbeln
     WHERE lifnr IN s_lifnr
       AND matnr IN s_matnr
       AND b~werks = 'P001'
       AND c~wbstk = 'A'
       AND c~vbtyp = '7'
       AND d~eindt <= l_end_date
    %_HINTS ORACLE
       'LEADING(T_03) USE_NL(T_00 T_01 T_02) INDEX (T_03 "VBUK~Z01")'.
    "Addition

    LOOP AT lt_lips_temp.
      lt_lips = lt_lips_temp.
      lt_lips-lfimg = lt_lips-lfimg - lt_lips-dabmg.
      COLLECT lt_lips.
      CLEAR: lt_lips_temp.
    ENDLOOP.


    LOOP AT lt_lips_2.
      READ TABLE lt_lips WITH KEY lifnr = lt_lips_2-lifnr
                                  matnr = lt_lips_2-matnr
                                  lfdat = lt_lips_2-lfdat.
      IF sy-subrc NE 0.
        lt_lips_2-lfimg = 0.
        APPEND lt_lips_2 TO lt_lips.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_lips.
      READ TABLE it_marc WITH KEY matnr = it_lips-matnr
** FOR E002
*                                   WERKS = 'E001'.
                                   werks = p_werks.
** END E002
      IF it_marc-sobsl = '40'.
        READ TABLE lt_lips_2 WITH KEY lifnr = lt_lips-lifnr
                                      matnr = lt_lips-matnr
                                      lfdat = lt_lips-lfdat.
        IF sy-subrc = 0.
          lt_lips-lfimg = lt_lips-lfimg + lt_lips_2-lfimg.
          MODIFY lt_lips.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF NOT lt_lips[] IS INITIAL.
    SORT lt_lips BY lifnr werks matnr.
    READ TABLE lt_lips INDEX 1.
    l_lifnr = lt_lips-lifnr.
*    l_werks = lt_lips-werks.
    l_matnr = lt_lips-matnr.

    LOOP AT lt_lips.

      IF l_lifnr <> lt_lips-lifnr
         OR l_matnr <> lt_lips-matnr.
*         OR l_werks <> lt_lips-werks.

        it_lips-lifnr = l_lifnr.
        it_lips-matnr = l_matnr.
*        it_lips-werks = l_werks.
        APPEND it_lips.
        CLEAR: it_lips.
        l_lifnr = lt_lips-lifnr.
*        l_werks = lt_lips-werks.
        l_matnr = lt_lips-matnr.
      ENDIF.
      IF lt_lips-lfdat < l_st_date.
        it_lips-rqty_p = it_lips-rqty_p + lt_lips-lfimg.
      ELSE.
        IF r01 = 'X'.
          READ TABLE it_day WITH KEY datum = lt_lips-lfdat.
          IF sy-subrc = 0.
            CONCATENATE 'IT_LIPS-RQTY_' it_day-seq INTO l_text.
            ASSIGN (l_text) TO <fs01>.
*     <fs01> = lt_lips-lfimg.
            IF sy-subrc = 0.
              <fs01> = <fs01> + lt_lips-lfimg.
            ENDIF.
          ELSE.
*            CLEAR: l_weekday.
*            CALL FUNCTION 'DATE_TO_DAY'
*                 EXPORTING
*                      date    = lt_lips-lfdat
*                 IMPORTING
*                      weekday = l_weekday.
*            IF l_weekday = 'Saturday' OR l_weekday = 'Sunday'.

* 02.20.2012(delete start), mjc
*            PERFORM read_working_date USING '-' l_kalid lt_lips-lfdat.
* 02.20.2012(delete end), mjc

* 02.20.2012(insert start), mjc
            CLEAR: mara-profl.
            SELECT SINGLE profl INTO mara-profl
             FROM  mara
             WHERE matnr = lt_lips-matnr.
            IF mara-profl = 'K'.
              PERFORM read_working_date USING '+' l_kalid lt_lips-lfdat.
            ELSE.
              PERFORM read_working_date USING '-' l_kalid lt_lips-lfdat.
            ENDIF.
* 02.20.2012(insert end), mjc

            READ TABLE it_day WITH KEY datum = lt_lips-lfdat.
            IF sy-subrc = 0.
              CONCATENATE 'IT_LIPS-RQTY_' it_day-seq INTO l_text.
              ASSIGN (l_text) TO <fs01>.
              IF sy-subrc = 0.
                <fs01> = <fs01> + lt_lips-lfimg.
              ENDIF.
*              ELSE.
*                lt_lips-lfdat = lt_lips-lfdat - 1.
*              PERFORM read_working_date USING '-' l_kalid lt_lips-lfdat
              .
*                IF sy-subrc = 0.
*                  CONCATENATE 'IT_LIPS-RQTY_' it_day-seq INTO l_text.
*                  ASSIGN (l_text) TO <fs01>.
*                  IF sy-subrc = 0.
*                    <fs01> = <fs01> + lt_lips-lfimg.
*                  ENDIF.
*                ENDIF.
*              ENDIF.
            ENDIF.
          ENDIF.
        ELSE.
          CLEAR: l_index.

* 02.20.2012(insert start), mjc
          CLEAR: mara-profl.
          SELECT SINGLE profl INTO mara-profl
           FROM  mara
           WHERE matnr = lt_lips-matnr.
          IF mara-profl = 'K'.
            PERFORM read_working_date USING '+' l_kalid lt_lips-lfdat .
            LOOP AT it_week.
              IF it_week-datum > lt_lips-lfdat.
                l_index = sy-tabix - 1.
                EXIT.
              ENDIF.
            ENDLOOP.
          ELSE.
* 02.20.2012(insert end), mjc

            LOOP AT it_week.
              IF it_week-datum > lt_lips-lfdat.
                l_index = sy-tabix - 1.
                EXIT.
              ENDIF.
            ENDLOOP.
* 02.20.2012(insert start), mjc
          ENDIF.
* 02.20.2012(insert end), mjc
          IF l_index IS INITIAL.
            l_cn = '21'.
          ELSE.
            l_cn = l_index.
          ENDIF.
          CONCATENATE 'IT_LIPS-RQTY_' l_cn INTO l_text.
          ASSIGN (l_text) TO <fs01>.
*     <fs01> = lt_lips-lfimg.
          IF sy-subrc = 0.
            <fs01> = <fs01> + lt_lips-lfimg.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    it_lips-lifnr = l_lifnr.
    it_lips-matnr = l_matnr.
*    it_lips-werks = l_werks.
    APPEND it_lips.
    CLEAR: it_lips.
  ENDIF.
ENDFORM.                    " get_lips_data

INCLUDE zrmm_requirement_plan_pbo.

INCLUDE zrmm_requirement_plan_paio.
*&---------------------------------------------------------------------*
*&      Form  BUILD_COLOR_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_LINE  text
*      -->P_1971   text
*----------------------------------------------------------------------*
*FORM BUILD_COLOR_ALL USING p_line p_fname.
*  if p_line  = 1.
*    wa_color-color-col = 6.
*    wa_color-color-int = 1.
*    wa_color-fname = p_fname.
*    append wa_color to it_color.
*    clear wa_color.
*  endif.
*endform.                    " BUILD_COLOR_ALL
*&---------------------------------------------------------------------*
*&      Form  call_COGI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_cogi.
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
         lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i.
  RANGES: s_werks FOR it_output-werks,
          s_matnr FOR it_output-matnr.

  CALL METHOD alv_grid->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows[]
      et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = w_repid
        txt2  = sy-subrc
        txt1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.
  READ TABLE it_output INDEX lt_rows-index.
  IF it_output-matnr = ' '.
    MESSAGE e000(zz) WITH text-m13.
  ENDIF.

  s_werks-sign = 'I'.
  s_werks-option = 'EQ'.
  s_werks-low = it_output-werks.
  APPEND s_werks.

  LOOP AT lt_rows.
    READ TABLE it_output INDEX lt_rows-index.
    IF NOT it_output-matnr IS INITIAL.
      s_matnr-sign = 'I'.
      s_matnr-option = 'EQ'.
      s_matnr-low = it_output-matnr.
      APPEND s_matnr.
    ENDIF.
    CLEAR: s_matnr.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM s_matnr.

  SUBMIT coruaffw WITH s_werks IN s_werks
                  WITH s_matnr IN s_matnr
                  AND RETURN.

ENDFORM.                    " call_COGI
*&---------------------------------------------------------------------*
*&      Form  get_po_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_po_data.
  DATA: BEGIN OF lt_po_temp OCCURS 0,
        lifnr LIKE ekko-lifnr,
        werks LIKE marc-werks,
        matnr LIKE mara-matnr,
        eindt LIKE eket-eindt,
        menge LIKE eket-menge,
        wemng LIKE eket-wemng,
        END OF lt_po_temp.

  DATA: BEGIN OF lt_po OCCURS 0,
         lifnr LIKE ekko-lifnr,
         werks LIKE marc-werks,
         matnr LIKE mara-matnr,
         eindt LIKE eket-eindt,
         menge LIKE eket-menge,
         END OF lt_po.

  DATA: lt_po_2 LIKE TABLE OF lt_po WITH HEADER LINE.

  DATA: l_st_date LIKE sy-datum,
        l_end_date LIKE sy-datum,
        l_text(30),
        l_index LIKE sy-tabix,
        l_cn(2) TYPE n.

  DATA: l_lifnr LIKE ekko-lifnr,
        l_matnr LIKE mara-matnr,
        l_werks LIKE marc-werks,
        l_weekday LIKE dtresr-weekday.
  CLEAR: it_po, it_po[].

  IF r01 = 'X'.
    READ TABLE it_day INDEX 1.
    l_st_date = it_day-datum.
    READ TABLE it_day INDEX 21.
    l_end_date = it_day-datum.
  ENDIF.
  IF r02 = 'X'.
    READ TABLE it_week INDEX 1.
    l_st_date = it_week-datum.
    READ TABLE it_week INDEX 21.
    l_end_date = it_week-datum.

*    CALL FUNCTION 'HR_GBSSP_GET_WEEK_DATES'
*         EXPORTING
*              p_pdate    = l_end_date
*         IMPORTING
*              p_saturday = l_end_date.
*    l_end_date = l_end_date + 1.
  ENDIF.

  SELECT a~lifnr b~matnr c~eindt c~menge c~wemng
   INTO CORRESPONDING FIELDS OF TABLE lt_po_temp
   FROM ekko AS a INNER JOIN ekpo AS b
   ON a~ebeln = b~ebeln
   INNER JOIN eket AS c
   ON b~ebeln = c~ebeln
   AND b~ebelp = c~ebelp
   WHERE a~bstyp = 'F'
     AND lifnr IN s_lifnr
     AND matnr IN s_matnr
     AND b~werks = p_werks
     AND b~elikz <> 'X'
     AND b~loekz <> 'L'
     AND c~eindt <= l_end_date.

  LOOP AT lt_po_temp.
    MOVE-CORRESPONDING lt_po_temp TO lt_po .
    lt_po-menge = lt_po_temp-menge - lt_po_temp-wemng.
    COLLECT lt_po.
    CLEAR: lt_po_temp, lt_po.
  ENDLOOP.
  CLEAR: lt_po_temp[].

** FOR E002
*  IF P_WERKS = 'E001'.
  IF p_werks = 'E001' OR p_werks = 'E002'.
** END E002

    SELECT a~lifnr b~matnr c~eindt c~menge c~wemng
     INTO CORRESPONDING FIELDS OF TABLE lt_po_temp
     FROM ekko AS a INNER JOIN ekpo AS b
     ON a~ebeln = b~ebeln
     INNER JOIN eket AS c
     ON b~ebeln = c~ebeln
     AND b~ebelp = c~ebelp
     WHERE a~bstyp = 'F'
       AND lifnr IN s_lifnr
       AND matnr IN s_matnr
       AND b~werks = 'P001'
       AND b~elikz <> 'X'
       AND b~loekz <> 'L'
       AND c~eindt <= l_end_date.

    LOOP AT lt_po_temp.
      MOVE-CORRESPONDING lt_po_temp TO lt_po_2.
      lt_po_2-menge = lt_po_temp-menge - lt_po_temp-wemng.
      COLLECT lt_po_2.
      CLEAR: lt_po_temp, lt_po_2.
    ENDLOOP.

    LOOP AT lt_po_2.
      READ TABLE lt_po WITH KEY lifnr = lt_po_2-lifnr
                                  matnr = lt_po_2-matnr
                                  eindt = lt_po_2-eindt.
      IF sy-subrc NE 0.
        lt_po_2-menge = 0.
        APPEND lt_po_2 TO lt_po.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_po.
      READ TABLE it_marc WITH KEY matnr = it_po-matnr
** FOR E002
*                                   WERKS = 'E001'.
                                   werks = p_werks.
** END E002
      IF it_marc-sobsl = '40'.
        READ TABLE lt_po_2 WITH KEY lifnr = lt_po-lifnr
                                      matnr = lt_po-matnr
                                      eindt = lt_po-eindt.
        IF sy-subrc = 0.
          lt_po-menge = lt_po-menge + lt_po_2-menge.
          MODIFY lt_po.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF NOT lt_po[] IS INITIAL.
    SORT lt_po BY lifnr werks matnr.
    READ TABLE lt_po INDEX 1.
    l_lifnr = lt_po-lifnr.
    l_matnr = lt_po-matnr.

    LOOP AT lt_po.

      IF l_lifnr <> lt_po-lifnr
         OR l_matnr <> lt_po-matnr.
        it_po-lifnr = l_lifnr.
        it_po-matnr = l_matnr.
        APPEND it_po.
        CLEAR: it_po.
        l_lifnr = lt_po-lifnr.
*        l_werks = lt_lips-werks.
        l_matnr = lt_po-matnr.
      ENDIF.
      IF lt_po-eindt < l_st_date.
        it_po-rqty_p = it_po-rqty_p + lt_po-menge.
      ELSE.
        IF r01 = 'X'.
          READ TABLE it_day WITH KEY datum = lt_po-eindt.
          IF sy-subrc = 0.
            CONCATENATE 'IT_PO-RQTY_' it_day-seq INTO l_text.
            ASSIGN (l_text) TO <fs01>.
            IF sy-subrc = 0.
              <fs01> = <fs01> + lt_po-menge.
            ENDIF.
          ELSE.
            CLEAR: l_weekday.
            CALL FUNCTION 'DATE_TO_DAY'
              EXPORTING
                date    = lt_po-eindt
              IMPORTING
                weekday = l_weekday.
*20.02.2012(delete start)
*            IF l_weekday = 'Saturday' OR l_weekday = 'Sunday'.
*20.02.2012(delete end)
*20.02.2012(insert start)
            IF l_weekday+0(3) = 'Sat' OR l_weekday+0(3) = 'Sun'.
              CLEAR: mara-profl.
              SELECT SINGLE profl INTO mara-profl
               FROM  mara
               WHERE matnr = lt_po-matnr.
              IF mara-profl = 'K'.
                PERFORM read_working_date USING '+' l_kalid lt_po-eindt.
              ELSE.
                PERFORM read_working_date USING '-' l_kalid lt_po-eindt.
              ENDIF.
*20.02.2012(insert end)

              READ TABLE it_day WITH KEY datum = lt_po-eindt.
              IF sy-subrc = 0.
                CONCATENATE 'IT_PO-RQTY_' it_day-seq INTO l_text.
                ASSIGN (l_text) TO <fs01>.
                IF sy-subrc = 0.
                  <fs01> = <fs01> + lt_po-menge.
                ENDIF.
              ELSE.
                lt_po-eindt = lt_po-eindt - 1.
                PERFORM read_working_date USING '-' l_kalid lt_po-eindt.
                IF sy-subrc = 0.
                  CONCATENATE 'IT_PO-RQTY_' it_day-seq INTO l_text.
                  ASSIGN (l_text) TO <fs01>.
                  IF sy-subrc = 0.
                    <fs01> = <fs01> + lt_po-menge.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ELSE.
          CLEAR: l_index.
* 02.20.2012(delete start), mjc
*          LOOP AT it_week.
*            IF it_week-datum > lt_po-eindt.
*              l_index = sy-tabix - 1.
*              EXIT.
*            ENDIF.
*          ENDLOOP.
* 02.20.2012(delete end), mjc

* 02.20.2012(insert start), mjc
          CLEAR: mara-profl.
          SELECT SINGLE profl INTO mara-profl
           FROM  mara
           WHERE matnr = lt_po-matnr.
          IF mara-profl = 'K'.
            PERFORM read_working_date USING '+' l_kalid lt_po-eindt.
          ELSE.
            PERFORM read_working_date USING '-' l_kalid lt_po-eindt.
          ENDIF.
          LOOP AT it_week.
            IF it_week-datum > lt_po-eindt.
              l_index = sy-tabix - 1.
              EXIT.
            ENDIF.
          ENDLOOP.
* 02.20.2012(insert end), mjc
          IF l_index IS INITIAL.
            l_cn = '21'.
          ELSE.
            l_cn = l_index.
          ENDIF.
          CONCATENATE 'IT_PO-RQTY_' l_cn INTO l_text.
          ASSIGN (l_text) TO <fs01>.
*     <fs01> = lt_lips-lfimg.
          IF sy-subrc = 0.
            <fs01> = <fs01> + lt_po-menge.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    it_po-lifnr = l_lifnr.
    it_po-matnr = l_matnr.
*    it_lips-werks = l_werks.
    APPEND it_po.
    CLEAR: it_po.
  ENDIF.
ENDFORM.                    " get_po_data

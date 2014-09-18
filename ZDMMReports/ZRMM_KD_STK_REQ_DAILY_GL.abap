************************************************************************
* Program Name      : ZRMM_KD_STK_REQ_DAILY
* Creation Date     : 08/02/2005.
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 08/02/2005      Furong Wang
************************************************************************

REPORT  zrmm_kd_stk_req_dayly NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmmm.
**---
INCLUDE : zrmmpmxxr_incl.

DATA: it_itab LIKE ztmm_trim_planr OCCURS 0 WITH HEADER LINE.
DATA : it_temp LIKE it_itab OCCURS 0 WITH HEADER LINE.

** PARALLEL DATA
DATA: wa_taskname(4)      TYPE n VALUE '0001' ,
      wa_excp_flag        TYPE c,
      wa_snd_jobs         TYPE i,
      wa_rcv_jobs         TYPE i.

** download to excel
DATA : dsn(90),
       wa_date like sy-datuM,
       WA_ERROR(1),
       wa_kalid  LIKE kako-kalid,
       l_index(2) TYPE n.

DATA : BEGIN OF it_mard OCCURS 0,
         matnr LIKE it_itab-matnr,
         werks LIKE mard-werks,
         lgort LIKE mard-lgort,
         maktx LIKE it_itab-maktx,
         dispo LIKE it_itab-dispo,
         matkl LIKE it_itab-matkl,
         plifz LIKE it_itab-plifz,     " Planned Delivery Time
         webaz LIKE it_itab-webaz,     " GR processing time
         eisbe LIKE it_itab-eisbe,     " safety stock
         bstrf LIKE it_itab-bstrf,     " rouding value
         labst LIKE mard-labst,  " Valuated stock with unrestricted use
         speme LIKE mard-speme,  " blocked stock
         meins LIKE mara-meins,
       END OF it_mard.

DATA : BEGIN OF it_matnr OCCURS 0,
         matnr LIKE it_itab-matnr,
         maktx LIKE it_itab-maktx,
         matkl LIKE it_itab-matkl,
         labst LIKE it_mard-labst,
       END OF it_matnr.

DATA : BEGIN OF it_lqua OCCURS 0,
         matnr LIKE it_itab-matnr,
         lgtyp LIKE lqua-lgtyp,
         gesme LIKE lqua-gesme,
         lqnum LIKE lqua-lqnum,
       END OF it_lqua.

DATA : BEGIN OF it_id_temp OCCURS 0,
         vbeln LIKE lips-vbeln,
         posnr LIKE lips-posnr,
         matnr LIKE lips-matnr,
         lfimg LIKE lips-lfimg,
       END OF it_id_temp.

DATA : BEGIN OF it_id_sum_sea OCCURS 0,
         matnr LIKE it_id_temp-matnr,
         lfimg LIKE it_id_temp-lfimg,
       END OF it_id_sum_sea.

DATA : BEGIN OF it_id_sum_mobil OCCURS 0,
         matnr LIKE it_id_temp-matnr,
         lfimg LIKE it_id_temp-lfimg,
       END OF it_id_sum_mobil.

DATA: BEGIN OF it_likp_ztbl OCCURS 0,
        vbeln LIKE ztblit-ebeln,
        matnr LIKE mara-matnr,
        traid LIKE likp-traid,
        zfreta LIKE ztbl-zfreta,
      END OF it_likp_ztbl.

DATA : BEGIN OF it_mobile OCCURS 0,
         ebeln LIKE ztblit-ebeln,
         ebelp LIKE ztblit-ebelp,
         matnr LIKE ztblit-matnr,
         blmenge LIKE ztblit-blmenge,
       END OF it_mobile.

DATA : it_mobile_copy LIKE it_mobile OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_mobile_sto OCCURS 0,
         matnr LIKE it_mobile-matnr,
         blmenge LIKE it_mobile-blmenge,
       END OF it_mobile_sto.

DATA : BEGIN OF it_plaf OCCURS 0,
         matnr LIKE plaf-matnr,
         pedtr LIKE plaf-pedtr,
         gsmng LIKE plaf-gsmng,
       END OF it_plaf.

DATA : it_mdsux_temp LIKE mdsu OCCURS 0 WITH HEADER LINE,
       it_mdsux LIKE it_mdsux_temp OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_week OCCURS 0,
         matnr LIKE it_matnr-matnr,
         week  LIKE scal-week,
         mng02 LIKE it_mdsux_temp-mng02,
       END OF it_week.

DATA : BEGIN OF it_month OCCURS 0,
         matnr LIKE it_matnr-matnr,
         month LIKE s031-spmon,
         mng02 LIKE it_mdsux_temp-mng02,
       END OF it_month.

DATA : BEGIN OF it_week_text OCCURS 0,
         index(2) TYPE n,     " LIKE sy-index,
         week  LIKE scal-week,
       END OF it_week_text.

DATA: it_mrp_866 TYPE STANDARD TABLE OF zsmm_display_plan_21day_9000
                                     WITH HEADER LINE.
DATA: BEGIN OF it_day OCCURS 0,
        seq(3)    TYPE c,
        datum  LIKE sy-datum,
      END OF it_day.

*DATA : BEGIN OF it_day OCCURS 0,
*         matnr LIKE it_matnr-matnr,
*         day(4)  TYPE n,
*         mng02 LIKE it_mdsux_temp-mng02,
*       END OF it_day.
*
*DATA : BEGIN OF it_day_text OCCURS 0,
*         index(2) TYPE n,     " LIKE sy-index,
*         day(4) TYPE n,
*       END OF it_day_text.
*
*
*DATA : BEGIN OF it_month_text OCCURS 0,
*         index(2) TYPE n,     " LIKE sy-index,
*         spmon LIKE s031-spmon,
*       END OF it_month_text.

DATA : BEGIN OF it_mard_stock OCCURS 0,
         matnr LIKE mard-matnr,
         labst LIKE mard-labst,
       END OF it_mard_stock.

DATA : BEGIN OF it_plaf_sum OCCURS 0,
         matnr LIKE plaf-matnr,
         bdw01 LIKE resb-bdmng,     " W1
         bdw02 LIKE resb-bdmng,     " W2
         bdw03 LIKE resb-bdmng,     " W3
         bdw04 LIKE resb-bdmng,     " W4
         bdw05 LIKE resb-bdmng,     " W5
         bdw06 LIKE resb-bdmng,     " W6
         bdw07 LIKE resb-bdmng,     " W7
         bdw08 LIKE resb-bdmng,     " W8
         bdw09 LIKE resb-bdmng,     " W9
         bdw10 LIKE resb-bdmng,                             " W10
         bdw11 LIKE resb-bdmng,                             " W11
         bdw12 LIKE resb-bdmng,                             " W12
         bdw13 LIKE resb-bdmng,     " M1
         bdw14 LIKE resb-bdmng,     " M2
         bdw15 LIKE resb-bdmng,     " M3
         bdw16 LIKE resb-bdmng,     " M4
         bdw17 LIKE resb-bdmng,     " M5
       END OF it_plaf_sum.

DATA : BEGIN OF it_po_temp OCCURS 0,
         ebeln LIKE mdbs-ebeln,
         vgpos LIKE lips-vgpos,
         matnr LIKE mdbs-matnr,
         menge LIKE mdbs-menge,
         wemng LIKE mdbs-wemng,
       END OF it_po_temp.

DATA : BEGIN OF it_po_sum OCCURS 0,
         matnr LIKE mdbs-matnr,
         meng3 LIKE it_itab-meng3,
         wemng LIKE it_po_temp-wemng,
       END OF it_po_sum.

DATA : BEGIN OF it_import_temp OCCURS 0,
         matnr LIKE it_itab-matnr,
         zfreta LIKE ztbl-zfreta,
         blmenge LIKE ztblit-blmenge,
       END OF it_import_temp.

DATA : BEGIN OF it_import_sum OCCURS 0,
         matnr LIKE it_itab-matnr,
         blmenge LIKE it_itab-meng3,
       END OF it_import_sum.

DATA : BEGIN OF it_import_sum01 OCCURS 0,
         matnr LIKE it_itab-matnr,
         blmenge LIKE it_itab-meng3,
       END OF it_import_sum01.

DATA : BEGIN OF it_import_sum02 OCCURS 0,
         matnr LIKE it_itab-matnr,
         blmenge LIKE it_itab-meng3,
       END OF it_import_sum02.

**--- Ranges
RANGES : r_lgort FOR mard-lgort,
         r_lgtyp FOR lqua-lgtyp,
         r_pedtr FOR plaf-pedtr,
         r_pedtr3 FOR plaf-pedtr.

RANGES : r_bdw01 FOR plaf-pedtr,                            " week 01
         r_bdw02 FOR plaf-pedtr,
         r_bdw03 FOR plaf-pedtr,
         r_bdw04 FOR plaf-pedtr,
         r_bdw05 FOR plaf-pedtr,
         r_bdw06 FOR plaf-pedtr,
         r_bdw07 FOR plaf-pedtr,
         r_bdw08 FOR plaf-pedtr,
         r_bdw09 FOR plaf-pedtr,
         r_bdw10 FOR plaf-pedtr,
         r_bdw11 FOR plaf-pedtr,
         r_bdw12 FOR plaf-pedtr,
         r_bdw13 FOR plaf-pedtr,
         r_bdw14 FOR plaf-pedtr,                            " week 14
         r_bdw15 FOR plaf-pedtr,                            " month 3
         r_bdw16 FOR plaf-pedtr,                            " month 4
         r_bdw17 FOR plaf-pedtr.                            " month 5

** for excel **
DATA: col_text LIKE gxxlt_v OCCURS 0 WITH HEADER LINE.
DATA: online_text LIKE gxxlt_o OCCURS 0,
      print_text LIKE gxxlt_p OCCURS 0.

**--- Constants
CONSTANTS : c_profl LIKE mara-profl VALUE 'K',
            c_lgnum LIKE t300-lgnum VALUE 'P01',
            c_plscn LIKE plaf-plscn VALUE '900',
            c_e001 LIKE t001w-werks VALUE 'E001'.


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
*  w_fieldcat-do_sum     = &6.
  w_fieldcat-qfieldname = &7.
  w_fieldcat-cfieldname = &8.
  w_fieldcat-no_out     = &9.
  append w_fieldcat.
  clear : w_fieldcat.
END-OF-DEFINITION.

DEFINE append_top.
  clear : w_line.
  if not &3 is initial or not &4 is initial.
    w_line-typ   = &1.
    w_line-key   = &2.
    concatenate &3 '~' &4 into w_line-info separated by space.
    append w_line to w_top_of_page.
  endif.
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

DEFINE append_date.
  clear : &3.
  move : 'I'     to &3-sign,
         'BT'    to &3-option,
         &1      to &3-low,
         &2      to &3-high.
  append &3.
END-OF-DEFINITION.

**---
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
PARAMETERS : p_werks LIKE t001w-werks OBLIGATORY DEFAULT 'P001'.
SELECT-OPTIONS : "s_werks FOR t001w-werks NO-EXTENSION NO INTERVALS
                 "                        OBLIGATORY DEFAULT 'P001',
                 s_dispo FOR marc-dispo, " OBLIGATORY,
                 s_matnr FOR mara-matnr OBLIGATORY,
*                 s_PROFL for mara-PROFL,
                 s_mtart FOR mara-mtart NO-EXTENSION NO INTERVALS
                                        DEFAULT 'ROH' OBLIGATORY.
SELECTION-SCREEN END OF BLOCK block1.

SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-007.
PARAMETERS : p_ztable TYPE c AS CHECKBOX DEFAULT 'X'.
PARAMETERS : p_excel TYPE c  AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF BLOCK block2.

*SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-007.
*PARAMETERS : p_excel type c  as checkbox default ' '.
*SELECTION-SCREEN END OF BLOCK block2.
*
**---
AT SELECTION-SCREEN OUTPUT.
  PERFORM check_screen.

AT SELECTION-SCREEN.
  PERFORM check_input_value.

**---
INITIALIZATION.
  PERFORM event_build USING w_eventcat[].

**---
TOP-OF-PAGE.
  PERFORM top_of_page.

**---
START-OF-SELECTION.
  CLEAR: WA_ERROR.
  IF p_ztable = 'X'.
    PERFORM get_data_from_ztable.
  ELSE.
    PERFORM get_data.
  ENDIF.
*  IF WA_ERROR = 'X'.
*     MESSAGE e999 WITH 'There is no MRP data for today'.
*     EXIT.
*  ENDIF.
**---
END-OF-SELECTION.
  IF it_itab[] IS INITIAL.
    MESSAGE s999 WITH text-m01.
  ELSE.
    PERFORM comment_build.     " USING w_top_of_page[].
    PERFORM make_alv_grid.
    IF p_excel = 'X'.
      PERFORM display_excel.
    ENDIF.
    PERFORM save_data_to_ztable.
*    perform download_to_excel.
  ENDIF.

**---

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
**---
  DATA : l_wemng LIKE mdbs-wemng,
         l_dmenge LIKE mdbs-menge,
         l_dwemng LIKE mdbs-wemng.

*  PERFORM plant_condition.
*  PERFORM storage_location_setting.
  PERFORM storage_type_setting.

  CLEAR : it_itab, it_itab[], it_temp, it_temp[], it_mard, it_mard[],
          it_matnr, it_matnr[], it_lqua, it_lqua[], it_mobile,
          it_mobile[], it_mobile_copy, it_mobile_copy[], it_mobile_sto,
          it_mobile_sto[],it_day,it_day[].

*---
  SELECT a~matnr     " material
         c~werks
         c~lgort
         d~maktx     " material desc.
         b~dispo     " MRP Controller : person of contact
         a~matkl     " material group
         b~plifz     " planned delivery time
         b~webaz     " GR processing time
         b~eisbe     " safety stock
         b~bstrf     " rouding value
         c~labst     " Valuated stock with unrestricted use
         c~speme     " blocked stock
         a~meins     " unit of measure
                 INTO CORRESPONDING FIELDS OF TABLE it_mard
                 FROM mara AS a INNER JOIN marc AS b
                   ON a~mandt EQ b~mandt
                  AND a~matnr EQ b~matnr
                      INNER JOIN mard AS c
                         ON b~mandt EQ c~mandt
                        AND b~matnr EQ c~matnr
                        AND b~werks EQ c~werks
                            INNER JOIN makt AS d
                               ON a~mandt EQ d~mandt
                              AND a~matnr EQ d~matnr
                WHERE b~werks EQ p_werks
*                      b~werks IN s_werks
                  AND b~dispo IN s_dispo
                  AND a~matnr IN s_matnr
                  AND a~mtart IN s_mtart
                  AND a~profl EQ c_profl    " MIP/LP/KD -> KD
                  AND a~lvorm EQ space      " Deletion Indicator
*                  AND c~lgort IN r_lgort
                  AND d~spras EQ sy-langu.

*--- collect material(distinct)
  LOOP AT it_mard.
    MOVE-CORRESPONDING it_mard TO it_matnr.
    COLLECT it_matnr.
    CLEAR : it_mard, it_matnr.
  ENDLOOP.

*--- read LQUA
*  SELECT matnr
*         lgtyp
*         gesme
*               INTO CORRESPONDING FIELDS OF TABLE it_lqua
*               FROM lqua
*                FOR ALL ENTRIES IN it_matnr
*              WHERE matnr EQ it_matnr-matnr.
*                AND lgtyp IN r_lgtyp
*                AND werks EQ p_werks.
**                AND werks IN s_werks
**                AND lgort IN r_lgort.

  SELECT matnr
           lgtyp
           gesme
           lqnum
                 INTO CORRESPONDING FIELDS OF TABLE it_lqua
                 FROM lqua
                  FOR ALL ENTRIES IN it_matnr
                  WHERE matnr EQ it_matnr-matnr
*                AND lgtyp IN r_lgtyp
                 AND werks EQ p_werks.

  SORT it_lqua BY matnr lgtyp.

*--- read Mobile Port Stock & Open PO Quantity
  CLEAR : it_po_temp, it_po_temp[], it_po_sum, it_po_sum[].
*          it_import_temp, it_import_temp[], it_import_sum,
*          it_import_sum[].

  SELECT ebeln
         ebelp AS vgpos
         matnr
         menge
         wemng INTO CORRESPONDING FIELDS OF TABLE it_po_temp
               FROM mdbs
                FOR ALL ENTRIES IN it_matnr
              WHERE matnr EQ it_matnr-matnr
                AND werks = p_werks
                AND loekz EQ space
                AND elikz EQ space.
*  SELECT a~ebeln
*         a~ebelp AS vgpos
*         matnr
*         b~menge
*         wemng INTO CORRESPONDING FIELDS OF TABLE it_po_temp
*               FROM ekpo as a inner join eket as b
*                on a~ebeln = b~ebeln
*                 and a~ebelp = b~ebelp
*                FOR ALL ENTRIES IN it_matnr
*              WHERE a~matnr EQ it_matnr-matnr
*                and werks = p_werks
*                AND loekz EQ space
*                AND elikz EQ space.

  IF NOT it_po_temp[] IS INITIAL.
    LOOP AT it_po_temp.
      MOVE : it_po_temp-matnr TO it_po_sum-matnr,
             it_po_temp-wemng TO it_po_sum-wemng.
      it_po_sum-meng3 = it_po_temp-menge - it_po_temp-wemng .
      COLLECT it_po_sum.
      CLEAR : it_po_temp, it_po_sum.
    ENDLOOP.

    SELECT a~vbeln
           a~posnr
           a~matnr
           lfimg INTO CORRESPONDING FIELDS OF TABLE it_id_temp
                 FROM lips AS a INNER JOIN vbuk AS b
                  ON a~vbeln EQ b~vbeln
                  FOR ALL ENTRIES IN it_po_temp
                WHERE a~mandt = sy-mandt
                  AND vgbel EQ it_po_temp-ebeln
                  AND vgpos EQ it_po_temp-vgpos
                  AND matnr EQ it_po_temp-matnr
                  AND b~wbstk = 'A'.

    SELECT d~vbeln
           b~matnr
           a~zfreta
            INTO CORRESPONDING FIELDS OF TABLE it_likp_ztbl
                     FROM ztbl AS a INNER JOIN ztblit AS b
                          ON a~mandt EQ b~mandt
                         AND a~zfblno EQ b~zfblno
                          INNER JOIN ztblit_inf AS c
                          ON a~zfhblno EQ c~zfhblno
                          INNER JOIN likp AS d
                          ON c~zfcont EQ d~traid
                      FOR ALL ENTRIES IN it_po_temp
                    WHERE b~matnr EQ it_po_temp-matnr
                      AND b~ebeln EQ it_po_temp-ebeln
                      AND b~ebelp EQ it_po_temp-vgpos+1(5)
                      AND a~zfreta NE '00000000'
                      AND a~zfreta LE sy-datum.

    LOOP AT it_id_temp.
      READ TABLE it_likp_ztbl WITH KEY vbeln = it_id_temp-vbeln
                                       matnr = it_id_temp-matnr.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING it_id_temp TO it_id_sum_mobil.
        COLLECT it_id_sum_mobil.
        CLEAR : it_id_temp, it_id_sum_mobil.
      ELSE.
        MOVE-CORRESPONDING it_id_temp TO it_id_sum_sea.
        COLLECT it_id_sum_sea.
        CLEAR : it_id_temp, it_id_sum_sea.
      ENDIF.
    ENDLOOP.
  ENDIF.

  clear wa_date.
  PERFORM get_requirement_quantity changing wa_date.

*  IF wa_DATE <> SY-DATUM.
*     wa_error = 'X'.
*     exit.
*  ENDIF.


  IF p_werks EQ c_e001.
    PERFORM get_storage_location_stock.
  ENDIF.

*--- main internal table calculation
  DATA : l_field01(14),
         l_field02(14).

  FIELD-SYMBOLS : <fs01>,
                  <FS02>.

  LOOP AT it_matnr.
*---
    CLEAR : it_mard.
    READ TABLE it_mard WITH KEY matnr = it_matnr-matnr.
    MOVE-CORRESPONDING it_mard TO it_temp.
*--- get Usage
    CLEAR : stpo.
    SELECT SINGLE menge INTO it_temp-meng1
                        FROM stpo
                       WHERE idnrk EQ it_matnr-matnr.
*--- get Storage Type Stock
    LOOP AT it_lqua WHERE matnr EQ it_matnr-matnr.
      IF it_lqua-lgtyp EQ '411'.     " CY Stock
        it_temp-gesm1 = it_temp-gesm1 + it_lqua-gesme.
      ELSEIF it_lqua-lgtyp EQ '421'.   " CC Rack Stock
        it_temp-gesm2 = it_temp-gesm2 + it_lqua-gesme.
      ELSEIF it_lqua-lgtyp EQ '422'.   " CC Bin Stock
        it_temp-gesm3 = it_temp-gesm3 + it_lqua-gesme.
      ELSEIF it_lqua-lgtyp BETWEEN '430' AND '439'.  " W/H Stock
*      ELSEIF it_lqua-lgtyp BETWEEN '431' AND '437'.  " W/H Stock
        it_temp-gesm4 = it_temp-gesm4 + it_lqua-gesme.
      ELSEIF it_lqua-lgtyp BETWEEN '440' AND '449'.  " Line Stock
*      ELSEIF it_lqua-lgtyp BETWEEN '441' AND '445'.  " Line Stock
        it_temp-gesm5 = it_temp-gesm5 + it_lqua-gesme.
*       else.
*        it_temp-gesm6 = it_temp-gesm6 + it_lqua-gesme.
      ENDIF.
    ENDLOOP.

    IF p_werks EQ c_e001.
      CLEAR : it_mard_stock.
      LOOP AT it_mard_stock WHERE matnr EQ it_matnr-matnr.
        it_temp-gesm4 = it_temp-gesm4 + it_mard_stock-labst.
      ENDLOOP.
    ENDIF.

*    IF it_temp-gesm4 EQ 0.
*      MOVE : it_matnr-labst TO it_temp-gesm4.
*    ENDIF.

*--- get total stock
    it_temp-total = 0.
    it_temp-blocked = 0.
    LOOP AT it_mard WHERE matnr = it_matnr-matnr
                     AND werks = p_werks.
      IF it_mard-lgort <> '9999'.
        it_temp-total = it_temp-total + it_mard-labst.
        it_temp-blocked = it_temp-blocked + it_mard-speme.
      ENDIF.
    ENDLOOP.

    it_temp-gesm6 = it_temp-total - it_temp-gesm1 - it_temp-gesm2
                    - it_temp-gesm3 - it_temp-gesm4 - it_temp-gesm5.


** furong
*    it_temp-total = it_temp-gesm1 + it_temp-gesm2 + it_temp-gesm3 +
*                    it_temp-gesm4 + it_temp-gesm5.

*--- get Mobile Port Stock
*    CLEAR : it_mobile_sto.
*    READ TABLE it_mobile_sto WITH KEY matnr = it_matnr-matnr.
*    MOVE : it_mobile_sto-blmenge TO it_temp-meng2.

*--- get Open PO Qty. and Mobile Port Stock
    CLEAR : it_po_sum, it_id_sum_mobil,it_id_sum_sea.
    READ TABLE it_po_sum WITH KEY matnr = it_matnr-matnr.
    MOVE : it_po_sum-meng3 TO it_temp-meng3.

*    READ TABLE it_import_sum WITH KEY matnr = it_matnr-matnr.
*    it_temp-meng2 = it_import_sum-blmenge - it_po_sum-wemng.
**
*    READ TABLE it_import_sum02 WITH KEY matnr = it_matnr-matnr.
*    it_temp-mengsea = it_import_sum02-blmenge.


    READ TABLE it_id_sum_mobil WITH KEY matnr = it_temp-matnr.
    it_temp-meng2 = it_id_sum_mobil-lfimg.                     "mobile
    READ TABLE it_id_sum_sea WITH KEY matnr = it_temp-matnr.
    it_temp-mengsea = it_id_sum_sea-lfimg.                     "  sea

*    CLEAR : mdbs, l_dmenge, l_dwemng.
*    SELECT SUM( menge )
*           SUM( wemng ) INTO (l_dmenge, l_dwemng)
*                        FROM mdbs
*                       WHERE matnr EQ it_matnr-matnr
*                         AND werks EQ p_werks
**                         AND werks in s_werks
**                         AND lgort IN r_lgort
*                         AND loekz EQ space     " Delete Ind.
*                         AND elikz EQ space.    " Delivery Com. Ind.
*    it_temp-meng3 = l_dmenge - l_dwemng.

**  for daily

    CLEAR: IT_MRP_866.
    READ TABLE IT_MRP_866 WITH KEY MATNR = it_matnr-matnr.
    CLEAR: l_index.
    DO 21 TIMES.
       CLEAR: l_field01, l_field02.
       l_index  = l_index + 1.
       CONCATENATE 'IT_TEMP-BDD' l_index INTO l_field01.
       ASSIGN (l_field01) TO <fs01>.
       CONCATENATE 'IT_MRP_866-D' l_index INTO l_field02.
       ASSIGN (l_field02) TO <fs02>.
       MOVE : <FS02> TO <fs01>.
    ENDDO.

*    LOOP AT it_day WHERE matnr EQ it_matnr-matnr.
*      CLEAR : it_day_text, l_index, l_field01.
*      READ TABLE it_day_text WITH KEY day = it_day-day.
*      IF sy-subrc EQ 0.
*        MOVE : it_day_text-index(2) TO l_index.
*        CONCATENATE 'IT_TEMP-BDD' l_index INTO l_field01.
*        ASSIGN (l_field01) TO <fs01>.
*        MOVE : it_day-mng02 TO <fs01>.
*      ENDIF.
*    ENDLOOP.

*--- for week
*    LOOP AT it_week where matnr EQ it_matnr-matnr.
*      CLEAR : it_week_text, l_index, l_field01.
*      READ TABLE it_week_text WITH KEY week = it_week-week.
*      IF sy-subrc EQ 0.
*        MOVE : it_week_text-index(2) TO l_index.
*        CONCATENATE 'IT_TEMP-BDW' l_index INTO l_field01.
*        ASSIGN (l_field01) TO <fs01>.
*        MOVE : it_week-mng02 TO <fs01>.
*      ENDIF.
*    ENDLOOP.
*
*--- for month
*    LOOP AT it_month WHERE matnr EQ it_matnr-matnr.
*      CLEAR : it_month_text, l_index, l_field01.
*      READ TABLE it_month_text WITH KEY spmon = it_month-month.
*      IF sy-subrc EQ 0.
*        MOVE : it_month_text-index(2) TO l_index.
*        CONCATENATE 'IT_TEMP-BDM' l_index INTO l_field01.
*        ASSIGN (l_field01) TO <fs01>.
*        MOVE : it_month-mng02 TO <fs01>.
*      ENDIF.
*    ENDLOOP.

*    CLEAR : it_plaf_sum.
*    READ TABLE it_plaf_sum WITH KEY matnr = it_matnr-matnr.
*    MOVE : it_plaf_sum-bdw01 TO it_temp-bdw01,
*           it_plaf_sum-bdw02 TO it_temp-bdw02,
*           it_plaf_sum-bdw03 TO it_temp-bdw03,
*           it_plaf_sum-bdw04 TO it_temp-bdw04,
*           it_plaf_sum-bdw05 TO it_temp-bdw05,
*           it_plaf_sum-bdw06 TO it_temp-bdw06,
*           it_plaf_sum-bdw07 TO it_temp-bdw07,
*           it_plaf_sum-bdw08 TO it_temp-bdw08,
*           it_plaf_sum-bdw09 TO it_temp-bdw09,
*           it_plaf_sum-bdw10 TO it_temp-bdw10,
*           it_plaf_sum-bdw11 TO it_temp-bdw11,
*           it_plaf_sum-bdw12 TO it_temp-bdw12,
*           it_plaf_sum-bdw13 TO it_temp-bdw13,
*           it_plaf_sum-bdw14 TO it_temp-bdw14,
*           it_plaf_sum-bdw15 TO it_temp-bdw15,
*           it_plaf_sum-bdw16 TO it_temp-bdw16,
*           it_plaf_sum-bdw17 TO it_temp-bdw17.
*---
    APPEND it_temp.
    CLEAR : it_temp.
  ENDLOOP.

*---
  it_itab[] = it_temp[].
ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM comment_build.
**---
  CLEAR : w_line.
  w_line-typ  = 'H'.
  w_line-info = text-002.
  APPEND w_line TO w_top_of_page.

  CLEAR : w_line.
  APPEND INITIAL LINE TO w_top_of_page.

  append_top :
      'S' text-003 p_werks     ' ',
      'S' text-004 s_dispo-low s_dispo-high,
      'S' text-005 s_matnr-low s_matnr-high,
      'S' text-006 s_mtart-low s_mtart-high.
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
**---
  PERFORM build_fieldcat.
  PERFORM build_sortcat.

  CLEAR : w_program.

  MOVE : sy-repid TO w_program.

  MOVE : 'X' TO w_layout-colwidth_optimize.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program       = w_program
            i_callback_pf_status_set = 'SET_STATUS'
            i_callback_user_command  = 'USER_COMMAND'
            is_layout                = w_layout
            it_fieldcat              = w_fieldcat[]
            it_events                = w_eventcat[]
            it_sort                  = w_sortcat[]
            i_save                   = 'A'
       TABLES
            t_outtab                 = it_itab
       EXCEPTIONS
            program_error            = 1
            OTHERS                   = 2.
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
**--- &7 : qty field      &8 : cur field        &9 : no out
  append_fieldcat :
   w_col_pos 'MATNR' 18 'Material'       'CHAR' 'X'  ''      '' '',
   w_col_pos 'MAKTX' 30 'Material Desc.' 'CHAR' ''   ''      '' '',
   w_col_pos 'MENG1' 10 'Usage'          'QUAN' ''   'MEINS' '' 'X',
*   w_col_pos 'NETPR' 10 'Unit Price'     'CURR' ''   '' 'WAERS' '',
*   w_col_pos 'LIFNR' 10 'Vendor'         'CHAR' ''   ''      '' '',
   w_col_pos 'DISPO'  3 'POC'            'CHAR' ''   ''      '' '',
   w_col_pos 'MATKL'  9 'Mat Group'      'CHAR' ''   ''      '' '',
    w_col_pos 'EISBE' 10 'Safety Stock'   'QUAN' ''   'MEINS' '' '',
    w_col_pos 'BSTRF' 10 'Rounding Value' 'QUAN' ''   'MEINS' '' '',
*    w_col_pos 'MENG4' 10 'PR Qty'         'QUAN' ''   'MEINS' '' '',
*   w_col_pos 'MENG5' 10 'Actual PO Qty'  'QUAN' ''   'MEINS' '' '',
*   w_col_pos 'NETWR' 10 'Actual PO Amt'  'CURR' ''   '' 'WAERS' '',
    w_col_pos 'MEINS'  3 'UoM'            'UNIT' ''   ''      '' '',
    w_col_pos 'WAERS'  3 'Currency'       'CURK' ''   ''      '' 'X',
   w_col_pos 'PLIFZ'  3 'Plan Del. Time' 'DEC'  ''   ''      '' '',
   w_col_pos 'WEBAZ'  3 'GR Proc. Time'  'DEC'  ''   ''      '' '',
   w_col_pos 'GESM1' 10 'CY Stock'       'QUAN' ''   'MEINS' '' '',
   w_col_pos 'GESM2' 10 'CC Rack Stock'  'QUAN' ''   'MEINS' '' '',
   w_col_pos 'GESM3' 10 'CC Bin Stock'   'QUAN' ''   'MEINS' '' '',
   w_col_pos 'GESM4' 10 'HMMA W/H Stock' 'QUAN' ''   'MEINS' '' '',
   w_col_pos 'GESM5' 10 'Line Stock'     'QUAN' ''   'MEINS' '' '',
   w_col_pos 'GESM6' 10 'Others'         'QUAN' ''   'MEINS' '' '',
   w_col_pos 'TOTAL' 10 'Total Aval'     'QUAN' ''   'MEINS' '' '',
   w_col_pos 'BLOCKED' 10 'Blocked'      'QUAN' ''   'MEINS' '' '',
   w_col_pos 'MENG2' 10 'Mob Port Stock' 'QUAN' ''   'MEINS' '' '',
   w_col_pos 'MENGSEA' 10 'On the sea'   'QUAN' ''   'MEINS' '' '',
   w_col_pos 'MENG3' 10 'Open PO'        'QUAN' ''   'MEINS' '' ''.

  DATA : l_field01(05),
         l_title(08),
         l_text(10).

  CLEAR: l_title, l_text, l_index.
  DO 21 TIMES.
    READ TABLE it_day INDEX sy-index.
    MOVE : it_day-SEQ+1(2) TO l_index.
    CONCATENATE : 'BDD' l_index INTO l_field01.
    WRITE it_day-datum TO l_text USING EDIT MASK '__/__/____'.
    l_title = l_text(5).
    append_fieldcat :
    w_col_pos l_field01 10 l_title  'QUAN' '' 'MEINS' '' ''.
  ENDDO.

*  DO 12 TIMES.
*    READ TABLE it_week_text INDEX sy-index.
*    MOVE : it_week_text-index(2) TO l_index.
*    CONCATENATE : 'BDW' l_index INTO l_field01,
*                  it_week_text-week(4) '/' it_week_text-week+4(2)
*                                       'W' INTO l_title.
*    append_fieldcat :
*    w_col_pos l_field01 10 l_title  'QUAN' '' 'MEINS' '' ''.
*  ENDDO.

*   w_col_pos 'BDW01' 10 'Week 01'        'QUAN' ''   'MEINS' '' '',
*   w_col_pos 'BDW02' 10 'Week 02'        'QUAN' ''   'MEINS' '' '',
*   w_col_pos 'BDW03' 10 'Week 03'        'QUAN' ''   'MEINS' '' '',
*   w_col_pos 'BDW04' 10 'Week 04'        'QUAN' ''   'MEINS' '' '',
*   w_col_pos 'BDW05' 10 'Week 05'        'QUAN' ''   'MEINS' '' '',
*   w_col_pos 'BDW06' 10 'Week 06'        'QUAN' ''   'MEINS' '' '',
*   w_col_pos 'BDW07' 10 'Week 07'        'QUAN' ''   'MEINS' '' '',
*   w_col_pos 'BDW08' 10 'Week 08'        'QUAN' ''   'MEINS' '' '',
*   w_col_pos 'BDW09' 10 'Week 09'        'QUAN' ''   'MEINS' '' '',
*   w_col_pos 'BDW10' 10 'Week 10'        'QUAN' ''   'MEINS' '' '',
*   w_col_pos 'BDW11' 10 'Week 11'        'QUAN' ''   'MEINS' '' '',
*   w_col_pos 'BDW12' 10 'Week 12'        'QUAN' ''   'MEINS' '' '',

*  DO 05 TIMES.
*    READ TABLE it_month_text INDEX sy-index.
*    MOVE : it_month_text-index(2) TO l_index.
*    CONCATENATE : 'BDM' l_index INTO l_field01,
*                  it_month_text-spmon(4) '/' it_month_text-spmon+4(2)
*                                         'M' INTO l_title.
*    append_fieldcat :
*    w_col_pos l_field01 10 l_title  'QUAN' '' 'MEINS' '' ''.
*  ENDDO.

*  w_col_pos 'BDM01' 10 'Month 01'       'QUAN' ''   'MEINS' '' '',
*  w_col_pos 'BDM02' 10 'Month 02'       'QUAN' ''   'MEINS' '' '',
*  w_col_pos 'BDM03' 10 'Month 03'       'QUAN' ''   'MEINS' '' '',
*  w_col_pos 'BDM04' 10 'Month 04'       'QUAN' ''   'MEINS' '' '',
*  w_col_pos 'BDM05' 10 'Month 05'       'QUAN' ''   'MEINS' '' '',


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
**--- &1 : position       &2 : field name       &3 : tab name
**--- &4 : up
  append_sortcat : '1' 'DISPO' 'IT_ITAB' 'X' '',
                   '2' 'MATNR' 'IT_ITAB' 'X' ''.
ENDFORM.                    " build_sortcat

*&---------------------------------------------------------------------*
*&      Form  plant_condition
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM plant_condition.
**--- if input 'E001', include 'P001'
*  if p_werks eq 'E001'.
*    MOVE : 'I'    TO s_werks-sign,
*           'EQ'   TO s_werks-option,
*           'P001' TO s_werks-low.
*    APPEND s_werks.
*  ELSE.
*    READ TABLE s_werks WITH KEY high = 'E001'.
*    IF sy-subrc EQ 0.
*      MOVE : 'I'    TO s_werks-sign,
*             'EQ'   TO s_werks-option,
*             'P001' TO s_werks-low.
*      APPEND s_werks.
*    ENDIF.
*  ENDIF.
ENDFORM.                    " plant_condition

*&---------------------------------------------------------------------*
*&      Form  check_input_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_input_value.
*--- if material type(S_MTART) is filled with 'HALB',
*---    Person of Contact field(S_DISPO) is mandatory.
  CHECK s_dispo[] IS INITIAL.

  IF s_mtart[] IS INITIAL.
    SET CURSOR FIELD 'S_DISPO-LOW'.
    MESSAGE e999 WITH text-m02.
  ENDIF.

*---
  READ TABLE s_mtart WITH KEY low = 'HALB'.

  IF sy-subrc EQ 0.
    SET CURSOR FIELD 'S_DISPO-LOW'.
    MESSAGE e999 WITH text-m02.
  ELSE.
    READ TABLE s_mtart WITH KEY high = 'HALB'.
    IF sy-subrc EQ 0.
      SET CURSOR FIELD 'S_DISPO-LOW'.
      MESSAGE e999 WITH text-m02.
    ENDIF.
  ENDIF.
** added by furong check s_matnr
*  IF s_matnr-low IS INITIAL AND s_matnr-high IS INITIAL.
*    s_matnr-high = 'ZZZZZZZ'.
*  ELSEIF s_matnr-high IS INITIAL.
*    s_matnr-high = s_matnr-low.
*  ENDIF.

  IF s_dispo-low IS INITIAL AND s_dispo-high IS INITIAL.
    s_dispo-high = 'ZZZ'.
  ELSEIF s_dispo-high IS INITIAL.
    s_dispo-high = s_dispo-low.
  ENDIF.

** end of addition
ENDFORM.                    " check_input_value

*&---------------------------------------------------------------------*
*&      Form  storage_location_setting
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM storage_location_setting.
**---
*  CLEAR : r_lgort, r_lgort[].
*
*  MOVE : 'I'       TO r_lgort-sign,
*         'EQ'      TO r_lgort-option,
*         'P400'    TO r_lgort-low.
*  APPEND r_lgort.
*
*  MOVE : 'I'       TO r_lgort-sign,
*         'EQ'      TO r_lgort-option,
*         'P500'    TO r_lgort-low.
*  APPEND r_lgort.
*
*  MOVE : 'I'       TO r_lgort-sign,
*         'EQ'      TO r_lgort-option,
*         'E100'    TO r_lgort-low.
*  APPEND r_lgort.
*
*  MOVE : 'I'       TO r_lgort-sign,
*         'EQ'      TO r_lgort-option,
*         'E110'    TO r_lgort-low.
*  APPEND r_lgort.
ENDFORM.                    " storage_location_setting

*&---------------------------------------------------------------------*
*&      Form  storage_type_setting
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM storage_type_setting.
*---
  CLEAR : r_lgtyp, r_lgtyp[].

*--- CY Stock
  MOVE : 'I'     TO r_lgtyp-sign,
         'EQ'    TO r_lgtyp-option,
         '411'   TO r_lgtyp-low.
  APPEND r_lgtyp.
  CLEAR : r_lgtyp.

*--- CC Rack Stock
  MOVE : 'I'     TO r_lgtyp-sign,
         'EQ'    TO r_lgtyp-option,
         '421'   TO r_lgtyp-low.
  APPEND r_lgtyp.
  CLEAR : r_lgtyp.

*--- CC Bin Stock
  MOVE : 'I'     TO r_lgtyp-sign,
         'EQ'    TO r_lgtyp-option,
         '422'   TO r_lgtyp-low.
  APPEND r_lgtyp.
  CLEAR : r_lgtyp.

*--- W/H Stock
  MOVE : 'I'     TO r_lgtyp-sign,
         'BT'    TO r_lgtyp-option,
         '430'   TO r_lgtyp-low,
         '439'   TO r_lgtyp-high.
*         '431'   TO r_lgtyp-low,
*         '437'   TO r_lgtyp-high.
  APPEND r_lgtyp.
  CLEAR : r_lgtyp.

*--- Line Stock
  MOVE : 'I'     TO r_lgtyp-sign,
         'BT'    TO r_lgtyp-option,
         '440'   TO r_lgtyp-low,
         '449'   TO r_lgtyp-high.
*         '441'   TO r_lgtyp-low,
*         '445'   TO r_lgtyp-high.
  APPEND r_lgtyp.
  CLEAR : r_lgtyp.
ENDFORM.                    " storage_type_setting

*&---------------------------------------------------------------------*
*&      Form  date_calcu
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM date_calcu.
*---
  DATA : l_start_week LIKE scal-week,
         l_end_week LIKE scal-week,
         l_start_date TYPE d,
         l_end_date TYPE d,
         l_start_month LIKE scal-week,
         l_end_month LIKE scal-week,
         l_end_week_3 LIKE scal-week,
         l_start_date_3 TYPE d,
         l_end_date_3 TYPE d.

*--- get start week
  CALL FUNCTION 'DATE_GET_WEEK'
       EXPORTING
            date         = sy-datum
       IMPORTING
            week         = l_start_week
       EXCEPTIONS
            date_invalid = 1
            OTHERS       = 2.

*--- get end week
  l_end_week = l_start_week + 13.

*--- get end of 3 week
  l_end_week_3 = l_start_week + 2.

  CALL FUNCTION 'WEEK_GET_FIRST_DAY'
       EXPORTING
            week         = l_end_week_3
       IMPORTING
            date         = l_start_date_3
       EXCEPTIONS
            week_invalid = 1
            OTHERS       = 2.

  l_end_date_3 = l_start_date_3 + 6.

*--- get start date
  CALL FUNCTION 'WEEK_GET_FIRST_DAY'
       EXPORTING
            week         = l_start_week
       IMPORTING
            date         = l_start_date
       EXCEPTIONS
            week_invalid = 1
            OTHERS       = 2.

*--- get start month
  l_start_month = sy-datum(6) + 4.

*--- get end month
  l_end_month = l_start_month + 2.

*--- get end date
  DATA : l_temp_date TYPE d.

  CONCATENATE l_end_month '01' INTO l_temp_date.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
       EXPORTING
            day_in            = l_temp_date
       IMPORTING
            last_day_of_month = l_end_date
       EXCEPTIONS
            day_in_no_date    = 1
            OTHERS            = 2.

*--- calculate select condition date
  CLEAR : r_pedtr3, r_pedtr3[].

  MOVE : 'I'          TO r_pedtr3-sign,
         'BT'         TO r_pedtr3-option,
         l_start_date TO r_pedtr3-low,
         l_end_date_3 TO r_pedtr3-high.
  APPEND r_pedtr3.

  CLEAR : r_pedtr, r_pedtr[].

*  l_start_date = l_end_date_3 + 1.

  MOVE : 'I'          TO r_pedtr-sign,
         'BT'         TO r_pedtr-option,
         l_start_date TO r_pedtr-low,
         l_end_date   TO r_pedtr-high.
  APPEND r_pedtr.

*--- week calculation
  DATA : l_start_date_6 TYPE d.

  DO 14 TIMES.
    CLEAR : l_start_date.
    CALL FUNCTION 'WEEK_GET_FIRST_DAY'
         EXPORTING
              week         = l_start_week
         IMPORTING
              date         = l_start_date
         EXCEPTIONS
              week_invalid = 1
              OTHERS       = 2.
    l_start_date_6 = l_start_date + 6.
    CASE sy-index.
      WHEN 1.
        append_date : l_start_date l_start_date_6 r_bdw01.
      WHEN 2.
        append_date : l_start_date l_start_date_6 r_bdw02.
      WHEN 3.
        append_date : l_start_date l_start_date_6 r_bdw03.
      WHEN 4.
        append_date : l_start_date l_start_date_6 r_bdw04.
      WHEN 5.
        append_date : l_start_date l_start_date_6 r_bdw05.
      WHEN 6.
        append_date : l_start_date l_start_date_6 r_bdw06.
      WHEN 7.
        append_date : l_start_date l_start_date_6 r_bdw07.
      WHEN 8.
        append_date : l_start_date l_start_date_6 r_bdw08.
      WHEN 9.
        append_date : l_start_date l_start_date_6 r_bdw09.
      WHEN 10.
        append_date : l_start_date l_start_date_6 r_bdw10.
      WHEN 11.
        append_date : l_start_date l_start_date_6 r_bdw11.
      WHEN 12.
        append_date : l_start_date l_start_date_6 r_bdw12.
      WHEN 13.
        append_date : l_start_date l_start_date_6 r_bdw13.
      WHEN 14.
        append_date : l_start_date l_start_date_6 r_bdw14.
    ENDCASE.
    l_start_week = l_start_week + 1.
  ENDDO.

*--- +3 month
  MOVE : 'I'     TO r_bdw15-sign,
         'BT'    TO r_bdw15-option.
  CONCATENATE l_start_month '01' INTO r_bdw15-low.
  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
       EXPORTING
            day_in            = r_bdw15-low
       IMPORTING
            last_day_of_month = r_bdw15-high
       EXCEPTIONS
            day_in_no_date    = 1
            OTHERS            = 2.
  APPEND r_bdw15.

*--- +4 month
  MOVE : 'I'     TO r_bdw16-sign,
         'BT'    TO r_bdw16-option.
  r_bdw16-low = r_bdw15-high + 1.
  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
       EXPORTING
            day_in            = r_bdw16-low
       IMPORTING
            last_day_of_month = r_bdw16-high
       EXCEPTIONS
            day_in_no_date    = 1
            OTHERS            = 2.
  APPEND r_bdw16.

*--- +5 month
  MOVE : 'I'     TO r_bdw17-sign,
         'BT'    TO r_bdw17-option.
  r_bdw17-low = r_bdw16-high + 1.
  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
       EXPORTING
            day_in            = r_bdw17-low
       IMPORTING
            last_day_of_month = r_bdw17-high
       EXCEPTIONS
            day_in_no_date    = 1
            OTHERS            = 2.
  APPEND r_bdw17.
ENDFORM. " date_calcu

*&---------------------------------------------------------------------*
*&      Form  get_planned_order
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_planned_order.
*---
  CLEAR : plaf, it_plaf, it_plaf[], it_plaf_sum, it_plaf_sum[].

  PERFORM date_calcu.

**--- read RESB
*  SELECT matnr
*         bdter AS pedtr
*         bdmng AS gsmng
*               INTO CORRESPONDING FIELDS OF TABLE it_plaf
*               FROM resb
*                FOR ALL ENTRIES IN it_matnr
*              WHERE matnr EQ it_matnr-matnr
*                AND werks IN s_werks
*                AND bdter IN r_pedtr3.

*--- read PLAF
  SELECT matnr
         pedtr
         gsmng
               APPENDING CORRESPONDING FIELDS OF TABLE it_plaf
               FROM plaf
                FOR ALL ENTRIES IN it_matnr
              WHERE matnr EQ it_matnr-matnr
                AND plwrk EQ p_werks
*                AND plwrk IN s_werks
                AND paart EQ 'NB'
                AND pedtr IN r_pedtr
*                AND plscn EQ '999'.
                AND plscn EQ '900'.

  LOOP AT it_plaf.
    MOVE : it_plaf-matnr TO it_plaf_sum-matnr.
    IF it_plaf-pedtr IN r_bdw01.
      MOVE : it_plaf-gsmng TO it_plaf_sum-bdw01.
    ELSEIF it_plaf-pedtr IN r_bdw02.
      MOVE : it_plaf-gsmng TO it_plaf_sum-bdw02.
    ELSEIF it_plaf-pedtr IN r_bdw03.
      MOVE : it_plaf-gsmng TO it_plaf_sum-bdw03.
    ELSEIF it_plaf-pedtr IN r_bdw04.
      MOVE : it_plaf-gsmng TO it_plaf_sum-bdw04.
    ELSEIF it_plaf-pedtr IN r_bdw05.
      MOVE : it_plaf-gsmng TO it_plaf_sum-bdw05.
    ELSEIF it_plaf-pedtr IN r_bdw06.
      MOVE : it_plaf-gsmng TO it_plaf_sum-bdw06.
    ELSEIF it_plaf-pedtr IN r_bdw07.
      MOVE : it_plaf-gsmng TO it_plaf_sum-bdw07.
    ELSEIF it_plaf-pedtr IN r_bdw08.
      MOVE : it_plaf-gsmng TO it_plaf_sum-bdw08.
    ELSEIF it_plaf-pedtr IN r_bdw09.
      MOVE : it_plaf-gsmng TO it_plaf_sum-bdw09.
    ELSEIF it_plaf-pedtr IN r_bdw10.
      MOVE : it_plaf-gsmng TO it_plaf_sum-bdw10.
    ELSEIF it_plaf-pedtr IN r_bdw11.
      MOVE : it_plaf-gsmng TO it_plaf_sum-bdw11.
    ELSEIF it_plaf-pedtr IN r_bdw12.
      MOVE : it_plaf-gsmng TO it_plaf_sum-bdw12.
    ELSEIF it_plaf-pedtr IN r_bdw13.
      MOVE : it_plaf-gsmng TO it_plaf_sum-bdw13.
    ELSEIF it_plaf-pedtr IN r_bdw14.
      MOVE : it_plaf-gsmng TO it_plaf_sum-bdw14.
    ELSEIF it_plaf-pedtr IN r_bdw15.
      MOVE : it_plaf-gsmng TO it_plaf_sum-bdw15.
    ELSEIF it_plaf-pedtr IN r_bdw16.
      MOVE : it_plaf-gsmng TO it_plaf_sum-bdw16.
    ELSEIF it_plaf-pedtr IN r_bdw17.
      MOVE : it_plaf-gsmng TO it_plaf_sum-bdw17.
    ENDIF.
    COLLECT it_plaf_sum.
    CLEAR : it_plaf_sum, it_plaf.
  ENDLOOP.
ENDFORM.                    " get_planned_order

*&---------------------------------------------------------------------*
*&      Form  get_requirement_quantity
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_requirement_quantity changing p_date like sy-datum.
  DATA: l_date   LIKE sy-datum,
        l_count(2) TYPE n.

  CLEAR: it_mrp_866, it_mrp_866[].
  SELECT SINGLE DATUM INTO l_date
   FROM ztmm_parts_21day
   WHERE erdat NE '00000000'.
  IF sy-subrc NE 0.
    MESSAGE e999 WITH 'No data generated in MRP table'.
    exit.
  ENDIF.
  p_date = l_date.
* reading working calendar
  PERFORM read_shop_calid  USING wa_kalid.
* first is current inputed date
  l_count = '01'.
  CONCATENATE 'D' l_count INTO it_day-seq.
  it_day-datum = l_date.
  APPEND it_day.

  DO 20 TIMES.
    l_count  = l_count + 1.
    l_date   = l_date  + 1.
    PERFORM read_working_date USING '+'  wa_kalid  l_date.
    CONCATENATE 'D' l_count INTO it_day-seq.
    it_day-datum   = l_date .
    APPEND it_day.  CLEAR: it_day.
  ENDDO.

 SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_mrp_866
    FROM ztmm_parts_21day AS a INNER JOIN makt AS b
                                  ON a~matnr = b~matnr
                                 AND b~spras = sy-langu
   WHERE a~werks EQ p_werks
*     AND a~shop  IN r_shop
*     AND a~prvbe IN s_vspvb
*     AND a~lgort IN r_lgort
     AND a~profl = c_profl  "IN s_profl
*     AND a~lifnr IN s_lifnr
     AND a~matnr IN s_matnr
*     AND b~maktx IN s_maktx
     AND a~dispo IN S_dispo.
*     and mtart

  IF sy-subrc NE 0.
    MESSAGE e999 WITH text-m01.
  ENDIF.

  LOOP AT it_mrp_866.
    it_mrp_866-sum = it_mrp_866-d01 + it_mrp_866-d02 + it_mrp_866-d03 +
                     it_mrp_866-d04 + it_mrp_866-d05 + it_mrp_866-d06 +
                     it_mrp_866-d07 + it_mrp_866-d08 + it_mrp_866-d09 +
                     it_mrp_866-d10 + it_mrp_866-d11 + it_mrp_866-d12 +
                     it_mrp_866-d13 + it_mrp_866-d14 + it_mrp_866-d15 +
                     it_mrp_866-d16 + it_mrp_866-d17 + it_mrp_866-d18 +
                     it_mrp_866-d19 + it_mrp_866-d20 + it_mrp_866-d21.
    MODIFY it_mrp_866.
  ENDLOOP.

ENDFORM.                    " get_requirement_quantity

*&---------------------------------------------------------------------*
*&      Form  get_week_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_week_text.
*---
  DATA : l_week LIKE scal-week.

  CLEAR : it_week_text, it_week_text[], l_week.

  CALL FUNCTION 'DATE_GET_WEEK'
       EXPORTING
            date         = sy-datum
       IMPORTING
            week         = l_week
       EXCEPTIONS
            date_invalid = 1
            OTHERS       = 2.

  l_week = l_week - 1.

  DO 12 TIMES.
    MOVE : sy-index TO it_week_text-index.
    it_week_text-week = l_week + sy-index.
    APPEND it_week_text.
  ENDDO.
ENDFORM.                    " get_week_text

*&---------------------------------------------------------------------*
*&      Form  get_month_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM get_month_text.
**---
*  DATA : l_spmon LIKE s031-spmon.
*
*  CLEAR : it_month_text, it_month_text[], l_spmon.
*
*  MOVE : sy-datum(6) TO l_spmon.
*
*  l_spmon = l_spmon - 1.
*
*  DO 5 TIMES.
*    MOVE : sy-index TO it_month_text-index.
*    it_month_text-spmon = l_spmon + sy-index.
*    APPEND it_month_text.
*  ENDDO.
*ENDFORM.                    " get_month_text

*---------------------------------------------------------------------*
*       FORM get_date_text                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*FORM get_date_text USING pv_date LIKE sy-datum.
**---
*  DATA: l_date LIKE sy-datum,
*        l_wdate LIKE sy-datum,
*        l_day(4) TYPE n.
*
*  DATA: nday TYPE i VALUE 1.
*
*  CLEAR : it_day_text, it_day_text[].
*  l_wdate = pv_date - 1.
*  DO 21 TIMES.
*    l_date = l_wdate + 1.
*    CLEAR: l_wdate.
*    CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
*      EXPORTING
*        correct_option                     = '+'
*        date                               = l_date
*        factory_calendar_id                = 'HM'
*     IMPORTING
*       date                                = l_wdate
**           FACTORYDATE                        =
**           WORKINGDAY_INDICATOR               =
**         EXCEPTIONS
**           CALENDAR_BUFFER_NOT_LOADABLE       = 1
**           CORRECT_OPTION_INVALID             = 2
**           DATE_AFTER_RANGE                   = 3
**           DATE_BEFORE_RANGE                  = 4
**           DATE_INVALID                       = 5
**           FACTORY_CALENDAR_NOT_FOUND         = 6
**           OTHERS                             = 7
*              .
**        IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
**        ENDIF.
*
*    MOVE : sy-index TO it_day_text-index.
*    l_day = l_wdate+4(4).
*    it_day_text-day = l_day.
*    APPEND it_day_text.
*  ENDDO.
*
*ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  get_storage_location_stock
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_storage_location_stock.
*---
  CLEAR : it_mard_stock, it_mard_stock[].

  SELECT matnr
         labst
              INTO CORRESPONDING FIELDS OF TABLE it_mard_stock
              FROM mard
               FOR ALL ENTRIES IN it_matnr
             WHERE matnr EQ it_matnr-matnr
               AND werks EQ c_e001
               AND lgort IN ('E100', 'E110').
ENDFORM.                    " get_storage_location_stock

*---------------------------------------------------------------------*
*       FORM set_status                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  EXTAB                                                         *
*---------------------------------------------------------------------*
FORM set_status USING extab TYPE slis_t_extab.
*---
  SET PF-STATUS 'BASE'.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM user_command                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  UCOMM                                                         *
*  -->  SELFIELD                                                      *
*---------------------------------------------------------------------*
FORM user_command USING ucomm LIKE sy-ucomm
                       selfield TYPE slis_selfield.
*---
  CASE ucomm.
    WHEN 'SHOW'.
      PERFORM show_calendar_mdsu.
  ENDCASE.
ENDFORM.                    "user_command

*&---------------------------------------------------------------------*
*&      Form  SHOW_CALENDAR_MDSU
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM show_calendar_mdsu.
*---
*  READ TABLE it_mdsux_temp INDEX 1.

  PERFORM show_calendar USING 'X' 'HM' sy-datum.
*                                       it_mdsux_temp-dat00.
ENDFORM.                    " SHOW_CALENDAR_MDSU

*&---------------------------------------------------------------------*
*&      Form  show_calendar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2878   text
*      -->P_2879   text
*      -->P_IT_MDSUX_TEMP_DAT00  text
*----------------------------------------------------------------------*
FORM show_calendar USING    dplay calid datum.
*---
  DATA : lv_repid LIKE sy-repid.
  lv_repid = sy-repid.
*
  CALL FUNCTION 'F4_DATE'
       EXPORTING
            display                  = dplay
            factory_calendar_id      = calid
            date_for_first_month     = datum
            progname_for_first_month = lv_repid
       IMPORTING
            select_date              = datum.
ENDFORM.                    " show_calendar

*---------------------------------------------------------------------*
*       FORM fill_cell                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_COL_NO                                                      *
*  -->  P_COL_NAME                                                    *
*---------------------------------------------------------------------*
FORM fill_cell USING p_col_no LIKE gxxlt_v-col_no
                    p_col_name LIKE gxxlt_v-col_name.
  col_text-col_no = p_col_no.
  col_text-col_name = p_col_name.
  APPEND col_text.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_TEXT_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_excel.
  DATA: w_col TYPE n,
        w_col_char LIKE gxxlt_v-col_no.

  REFRESH col_text.
  CLEAR col_text.
*
  w_col = 1.
  w_col = w_col + 1.
  PERFORM fill_cell USING: w_col_char 'Material'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM fill_cell USING: w_col_char 'Material Desc.'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM fill_cell USING: w_col_char 'Usage'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM fill_cell USING: w_col_char 'Uint Price'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM fill_cell USING: w_col_char 'Vendor'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM fill_cell USING: w_col_char 'POC'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM fill_cell USING: w_col_char 'Mat Group'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM fill_cell USING: w_col_char 'Safety Stock'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM fill_cell USING: w_col_char 'Rounding Value'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM fill_cell USING: w_col_char 'PR QTY'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM fill_cell USING: w_col_char 'Act PO QTY'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM fill_cell USING: w_col_char 'Act PO Amount'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM fill_cell USING: w_col_char 'UoM'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM fill_cell USING: w_col_char 'Currency'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM fill_cell USING: w_col_char 'Plan Del. Time'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM fill_cell USING: w_col_char 'GR Proc. Time'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM fill_cell USING: w_col_char 'CY Stock'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM fill_cell USING: w_col_char 'CC Rack Stock'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM fill_cell USING: w_col_char 'CC Bin Stock'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM fill_cell USING: w_col_char 'HMMA W/H Stock'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM fill_cell USING: w_col_char 'Line Stock'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM fill_cell USING: w_col_char 'Others'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM fill_cell USING: w_col_char 'Total Aval'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM fill_cell USING: w_col_char 'Blocked'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM fill_cell USING: w_col_char 'Mob Port Stock'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM fill_cell USING: w_col_char 'On the sea'.
  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM fill_cell USING: w_col_char 'Open PO'.

  DATA : l_field01(05),
         L_TEXT(10),
         l_title LIKE gxxlt_v-col_name.

  clear: l_index.
  DO 21 TIMES.
    READ TABLE it_day INDEX sy-index.
    MOVE : it_day-SEQ+1(2) TO l_index.
    CONCATENATE : 'BDD' l_index INTO l_field01.
    WRITE it_day-datum TO l_text USING EDIT MASK '__/__/____'.
    l_title = l_text(5).
    w_col_char = w_col.
    w_col_char = w_col.
    PERFORM fill_cell USING: w_col_char l_title.
  ENDDO.

  w_col = w_col + 1.
  w_col_char = w_col.
  PERFORM fill_cell USING: w_col_char 'Date'.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
     EXPORTING
*           PERCENTAGE = 0
           text       = 'Downloading to Excel, Please Wait....'
       EXCEPTIONS
            OTHERS     = 1.

  CALL FUNCTION 'XXL_CHECK_API'
       EXPORTING
            question = 'STARTABLE'
*    IMPORTING
*         RETURN_CODE  =
     EXCEPTIONS
          inv_question = 1
          OTHERS       = 2.

  IF sy-subrc <> 0.
    MESSAGE e000 WITH 'API Error'.
  ELSE.
    CALL FUNCTION 'XXL_SIMPLE_API'
      EXPORTING
*   FILENAME                = 'ULO     '
        header                  = 'Downloding to Excel'
        n_key_cols              = 1
      TABLES
        col_text                = col_text
        data                    = it_itab
        online_text             = online_text
        print_text              = print_text
      EXCEPTIONS
        dim_mismatch_data       = 1
        file_open_error         = 2
        file_write_error        = 3
        inv_winsys              = 4
        inv_xxl                 = 5
        OTHERS                  = 6.
  ENDIF.

ENDFORM.                    " FILL_TEXT_EXCEL
*&---------------------------------------------------------------------*
*&      Form  get_data_from_ztable
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_from_ztable.
  data: l_date like sy-datum.
  IF s_matnr-high IS INITIAL.
    IF s_matnr-low IS INITIAL.
      s_matnr-high = 'ZZZZZZ'.
    ELSE.
      s_matnr-high = s_matnr-low.
    ENDIF.
    APPEND s_matnr.
  ENDIF.
  IF s_dispo-high IS INITIAL.
    IF s_dispo-low IS INITIAL.
      s_dispo-high = 'zzzzzz'.
    ELSE.
      s_dispo-high = s_dispo-low.
    ENDIF.
    APPEND s_dispo.
  ENDIF.

  SELECT * INTO TABLE it_itab
                 FROM ztmm_trim_planr
                WHERE werks EQ p_werks
                  AND dispo IN s_dispo
                  AND matnr IN s_matnr.
  IF it_itab[] IS INITIAL.
    MESSAGE s999 WITH text-m01.
  ENDIF.
  READ TABLE it_itab INDEX 1.

** get it_day
  clear: l_index.
  PERFORM read_shop_calid  USING wa_kalid.
  l_index = '01'.
  CONCATENATE 'D' l_index INTO it_day-seq.
  it_day-datum = it_itab-cdate.
  APPEND it_day.

  DO 20 TIMES.
    l_index  = l_index + 1.
    l_date   = l_date  + 1.
    PERFORM read_working_date USING '+'  wa_kalid  l_date.
    CONCATENATE 'D' l_index INTO it_day-seq.
    it_day-datum   = l_date .
    APPEND it_day.  CLEAR: it_day.
  ENDDO.



ENDFORM.                    " get_data_from_ztable
*&---------------------------------------------------------------------*
*&      Form  save_date_to_ztable
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_data_to_ztable.
  IF p_ztable IS INITIAL.
    LOOP AT it_itab.
      it_itab-cdate = sy-datum.
      MODIFY it_itab TRANSPORTING cdate.
    ENDLOOP.
    delete from ZTMM_TRIM_PLANR where cdate <= sy-datum. "from table tab
.
    MODIFY ztmm_trim_planr FROM TABLE it_itab.
  ENDIF.
  IF sy-subrc EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " save_date_to_ztable
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
*&---------------------------------------------------------------------*
*&      Form  RETURN_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_MDSUX  text
*      -->P_=  text
*      -->P_IT_MDSUX_TEMP  text
*      -->P_EXCEPTIONS  text
*      -->P_COMMUNICATION_FAILURE  text
*      -->P_=  text
*      -->P_1  text
*      -->P_SYSTEM_FAILURE  text
*      -->P_=  text
*      -->P_2  text
*      -->P_RESOURCE_FAILURE  text
*      -->P_=  text
*      -->P_3  text
*      -->P_OTHERS  text
*      -->P_=  text
*      -->P_4  text
*&---------------------------------------------------------------------*
*&      Form  download_to_excel
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_to_excel.
  CONCATENATE  '/usr/sap/trans/'
               'triminput'
               '.txt'
               INTO dsn.

  OPEN DATASET dsn IN TEXT MODE FOR OUTPUT.
  LOOP AT it_itab.
    OPEN DATASET dsn IN TEXT MODE FOR APPENDING.
    TRANSFER it_itab TO dsn.
  ENDLOOP.

  CLOSE DATASET dsn.

ENDFORM.                    " download_to_excel
*&---------------------------------------------------------------------*
*&      Form  read_shop_calid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_KALID  text
*----------------------------------------------------------------------*
FORM read_shop_calid USING    pa_kalid.
  SELECT SINGLE kalid INTO pa_kalid
    FROM zvpp_capacity
   WHERE arbpl = 'T'   .
ENDFORM.                    " READ_SHOP_CALID
*&---------------------------------------------------------------------*
*&      Form  read_working_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2998   text
*      -->P_WA_KALID  text
*      -->P_L_DATE  text
*----------------------------------------------------------------------*
form read_working_date using pa_type  pa_kalid  pa_wdate.
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
endform.                    " read_working_date

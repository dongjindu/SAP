************************************************************************
* Program Name      : ZRMMPM17_LINE_INVENTORY
* Author            : Jaesung, Lee
* Creation Date     : 2003.08.25.
* Specifications By : Jaesung, Lee
* Pattern           : Report 1-1
* Development Request No : UD1K901849
* Addl Documentation:
* Description       : Line Inventory Report
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 2003.10.16 Hakchin Kim  UD1K901849
*   Change History
*   1. KD Part Added.
*   2. Some code Changed by Hakchin Kim
*   3. Some code Changed by Sung-Tae Lim

************************************************************************

REPORT zrmmpm17r_line_inventory NO STANDARD PAGE HEADING
                                LINE-SIZE 132
                                LINE-COUNT 64(1)
                                MESSAGE-ID zmmm.

**---
INCLUDE : zrmmpmxxr_incl.


**---
*--- main internal table
DATA : BEGIN OF it_itab OCCURS 0,
         matnr  LIKE eban-matnr,     " Material number
         werks  LIKE mard-werks,     " Plant
         maktx  LIKE makt-maktx,     " Material description
         bismt  LIKE mara-bismt,     " Old material number(ALC code)
         lgort  LIKE mard-lgort,      "Storage location
         lgbzo  LIKE ekpo-lgbzo, "group gate(Automotive) Unloading Point
         feedr  LIKE ztmm_mast-feedr, " Feeder
         zline  LIKE ztmm_mast-zline, " Line
         works  LIKE ztmm_mast-works, " WorkStation
         rh_lh  LIKE ztmm_mast-rh_lh, " RH/LH
         menge  LIKE stpo-menge,   " Component quantity - Usage
         meins  LIKE stpo-meins,   " unit
         profl  LIKE mara-profl,   " surce (LP/KD)
         bstmi  LIKE marc-bstmi,   " Qty/con
         plan   LIKE mard-labst,   " plan
         diff   LIKE mard-labst,   " differce
         labst  LIKE mard-labst,   " inventory
         gesme  LIKE lqua-gesme,   " line stock
         wstok  LIKE lqua-gesme,   " Warehouse stock
         ccbin  LIKE lqua-gesme,   " cc-bin stock
         ccrack LIKE lqua-gesme,   " cc-rack stock
         cystok LIKE lqua-gesme,   " cy stock
         notyetgr LIKE lqua-gesme,   " Not Yet GR stock
         arriv  LIKE lqua-gesme,   " Arrival
         shipm  LIKE lqua-gesme,   " Shipment
         duin   LIKE eket-menge,   " Due in
         dispo  LIKE marc-dispo,   " mrp controler
       END OF it_itab.

DATA : it_temp LIKE it_itab OCCURS 0 WITH HEADER LINE.

*--- For Inbound Delivery LP
DATA : BEGIN OF it_lp OCCURS 10,
         vbeln LIKE likp-vbeln,   "Inbound Delivery
         posnr LIKE lips-posnr,   "Delivery item
         zzdepdt LIKE likp-zzdepdt,     " ETD
         zzdeptm LIKE likp-zzdeptm,
         zzarrdt LIKE likp-zzarrdt,     " ETA
         zzarrtm LIKE likp-zzarrtm,
         lifex LIKE likp-lifex,
*         traid LIKE likp-traid,   "KD Container Number
         lgbzo LIKE lips-lgbzo,
         lfimg LIKE lips-lfimg,
*         duin  LIKE eket-menge,
         meins LIKE mara-meins,
         matnr LIKE lips-matnr,
       END OF it_lp.

*--- For Inbound Delivery KD
DATA : BEGIN OF wa_kd,
         vgbel LIKE lips-vgbel,   "PO No
         vgpos LIKE lips-vgpos,
              "PO Item (Length is different from EBELP)
         traid LIKE likp-traid,   "Container number
         kdmat LIKE lips-kdmat,   " Case Number
         vrkme LIKE lips-vrkme,   "Unit
         lfimg LIKE lips-lfimg,   "Actual quantity delivered
         zzdepdt LIKE likp-zzdepdt,     " ETD
         zzarrdt LIKE likp-zzarrdt,     " ETA
       END OF wa_kd.

DATA : it_kd LIKE TABLE OF wa_kd WITH HEADER LINE.

*--- Storage Type Stock
DATA : BEGIN OF it_stype_stock OCCURS 0,
         werks LIKE lqua-werks,
         matnr LIKE lqua-matnr,
         lgort LIKE lqua-lgort,
         lgtyp LIKE lqua-lgtyp,
         gesme LIKE lqua-gesme,
       END OF it_stype_stock.

DATA : BEGIN OF it_stype_stock_sum OCCURS 0,
         werks LIKE lqua-werks,
         matnr LIKE lqua-matnr,
         lgort LIKE lqua-lgort,
         gesme1 LIKE lqua-gesme,     " Line Stock
         gesme2 LIKE lqua-gesme,     " W/H Stock
         gesme3 LIKE lqua-gesme,     " CC-Bin Stock
         gesme4 LIKE lqua-gesme,     " CC-Rack Stock
         gesme5 LIKE lqua-gesme,     " CY Stock
       END OF it_stype_stock_sum.

*--- plan data (ztmm_nstl)
DATA : it_ztmm_nstl LIKE ztmm_nstl OCCURS 0 WITH HEADER LINE.

DATA : it_ztmm_nstl_sum LIKE it_ztmm_nstl OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_plan OCCURS 0,
         matnr LIKE it_ztmm_nstl_sum-matnr,
         time01 LIKE it_ztmm_nstl_sum-time01,
       END OF it_plan.

*--- bom usage (STPO)
DATA : BEGIN OF it_stpo OCCURS 0,
         idnrk LIKE stpo-idnrk,
         menge LIKE stpo-menge,
       END OF it_stpo.

*---
DATA : BEGIN OF it_po OCCURS 10,
         matnr LIKE mara-matnr,
         werks LIKE mard-werks,
         lgort LIKE mard-lgort,  "Storage location
         lgbzo LIKE ekpo-lgbzo,  "(Automotive) Unloading Point
       END OF it_po.

DATA : BEGIN OF it_mast OCCURS 10,
         matnr  LIKE ztmm_mast-matnr,
         feedr  LIKE ztmm_mast-feedr, " Feeder
         zline  LIKE ztmm_mast-zline, " Line
         works  LIKE ztmm_mast-works, " WorkStation
         rh_lh  LIKE ztmm_mast-rh_lh, " RH/LH
       END OF it_mast.

DATA : BEGIN OF ls_open ,   "For Open Quantity: = menge-wemng
         ebeln LIKE mdbs-ebeln,  "PO no
         ebelp LIKE mdbs-ebelp,  "PO Item no
         etenr LIKE mdbs-etenr,  "Delivery schedule line counter
         eindt LIKE mdbs-eindt,  "Item delivery date
         etfz1 LIKE mdbs-etfz1,     " ekpo-etfz1,
         "Date Quantity for LP Open Item Selection
         menge LIKE mdbs-menge,  "Scheduled quantity
         wemng LIKE mdbs-wemng,  "Quantity of goods received
       END OF ls_open.

DATA : lt_open LIKE TABLE OF ls_open.

DATA : BEGIN OF it_worktime OCCURS 0.
        INCLUDE STRUCTURE zsmm_nstl_worktime.
DATA : END OF it_worktime.

DATA : BEGIN OF it_po_temp OCCURS 0,
         ebeln LIKE eket-ebeln,
         vgpos LIKE lips-vgpos,
*         ebelp LIKE eket-ebelp,
         matnr LIKE ekpo-matnr,
         menge LIKE eket-menge,
         eindt LIKE eket-eindt,
         uzeit LIKE eket-uzeit,
         etfz1 LIKE ekpo-etfz1,
       END OF it_po_temp.

DATA : BEGIN OF it_po_sum OCCURS 0,
         matnr LIKE it_po_temp-matnr,
         menge LIKE it_po_temp-menge,
       END OF it_po_sum.

DATA : BEGIN OF it_id_temp OCCURS 0,
         vbeln LIKE lips-vbeln,
         posnr LIKE lips-posnr,
         matnr LIKE lips-matnr,
         lfimg LIKE lips-lfimg,
       END OF it_id_temp.

DATA : BEGIN OF it_id_sum OCCURS 0,
         matnr LIKE it_id_temp-matnr,
         lfimg LIKE it_id_temp-lfimg,
       END OF it_id_sum.

DATA : BEGIN OF it_import_temp OCCURS 0,
         matnr   LIKE ztblit-matnr,
         zfreta  LIKE ztbl-zfreta,
         blmenge LIKE ztblit-blmenge,
       END OF it_import_temp.

DATA : BEGIN OF it_import_sum01 OCCURS 0,
         matnr   LIKE it_import_temp-matnr,
         blmenge LIKE it_import_temp-blmenge,
       END OF it_import_sum01.

DATA : it_import_sum02 LIKE it_import_sum01 OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_gr_temp OCCURS 0,
         matnr LIKE ekbe-matnr,
         bwart LIKE ekbe-bwart,
         shkzg LIKE ekbe-shkzg,
         menge LIKE ekbe-menge,
       END OF it_gr_temp.

DATA : BEGIN OF it_gr_sum OCCURS 0,
         matnr LIKE it_gr_temp-matnr,
         menge LIKE it_gr_temp-menge,
       END OF it_gr_sum.

DATA : BEGIN OF it_vbfa_temp OCCURS 0,
         matnr LIKE vbfa-matnr,
         plmin LIKE vbfa-plmin,
         rfmng LIKE vbfa-rfmng,
       END OF it_vbfa_temp.

DATA : BEGIN OF it_vbfa_sum OCCURS 0,
         matnr LIKE vbfa-matnr,
         rfmng LIKE vbfa-rfmng,
       END OF it_vbfa_sum.


*--- Storage Type
RANGES : r_lgtyp  FOR lqua-lgtyp,     " Condition Summary
         r_lgtyp1 FOR lqua-lgtyp,     " Line Stock
         r_lgtyp2 FOR lqua-lgtyp,     " W/H Stock
         r_lgtyp3 FOR lqua-lgtyp,     " CC-Bin Stock
         r_lgtyp4 FOR lqua-lgtyp,     " CC-Rack Stock
         r_lgtyp5 FOR lqua-lgtyp.     " CY Stock

RANGES : r_werks FOR t001w-werks.

CONSTANTS : c_p001 LIKE t001w-werks VALUE 'P001',
            c_e001 LIKE t001w-werks VALUE 'E001',
            c_lp   LIKE it_itab-profl VALUE 'V',
            c_kd   LIKE it_itab-profl VALUE 'K',
*            c_tempb LIKE mara-tempb VALUE '11',
            c_bsart_kd LIKE ekko-bsart VALUE 'KD',
            c_bsart_jit LIKE ekko-bsart VALUE 'JIT',
            c_bsart_em LIKE ekko-bsart VALUE 'EM',
            c_calid_p LIKE scal-fcalid VALUE 'HM',
            c_calid_e LIKE scal-fcalid VALUE 'HE'.

DATA:             c_calid LIKE scal-fcalid.
DATA : w_current_date TYPE d.

**--- macro
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
  w_fieldcat-no_out     = &9.
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

DEFINE append_top.
  clear : w_line.
  if not &3 is initial or not &4 is initial.
    w_line-typ   = &1.
    w_line-key   = &2.
    if &4 is initial.
      concatenate &3 &4 into w_line-info separated by space.
    else.
      concatenate &3 '~' &4 into w_line-info separated by space.
    endif.
    append w_line to w_top_of_page.
  endif.
END-OF-DEFINITION.

DEFINE append_storage_type.
  clear : &2.
  move : 'I'     to &2-sign,
         'EQ'    to &2-option,
         &1      to &2-low.
  append &2.
END-OF-DEFINITION.


**---
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.

PARAMETERS : p_werks LIKE mard-werks OBLIGATORY DEFAULT 'P001'.  " plant

SELECT-OPTIONS : s_matnr FOR mard-matnr.        " Material

SELECT-OPTIONS : s_lgort FOR mard-lgort,        " Storage Location
                 s_lgtyp FOR lqua-lgtyp MATCHCODE OBJECT zsh_mm_type,
                                                " storage type
                 s_feedr FOR ztmm_mast-feedr,   " feeder
                 s_zline FOR ztmm_mast-zline,   " line
                 s_works FOR ztmm_mast-works,   " Workstation
                 s_profl FOR mara-profl.        " Source

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) text-t10.
SELECTION-SCREEN POSITION 33.
PARAMETERS : p_plan TYPE btcstime DEFAULT sy-uzeit MODIF ID gr1.
*SELECTION-SCREEN POSITION 58.
PARAMETERS : p_inter(2) TYPE n DEFAULT '02'.
SELECTION-SCREEN END OF LINE.

PARAMETERS : p_inven(1) TYPE n  DEFAULT '8'." Inventory Type
SELECTION-SCREEN END OF BLOCK block1.

SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-002
                                       NO INTERVALS.
SELECTION-SCREEN COMMENT /2(20) text-t01.
SELECTION-SCREEN COMMENT /2(20) text-t02.
SELECTION-SCREEN COMMENT /2(20) text-t03.
SELECTION-SCREEN COMMENT /2(20) text-t04.
SELECTION-SCREEN COMMENT /2(20) text-t05.
*SELECTION-SCREEN COMMENT /2(20) text-t06.
SELECTION-SCREEN COMMENT /2(20) text-t07.
SELECTION-SCREEN COMMENT /2(20) text-t08.
SELECTION-SCREEN COMMENT /2(20) text-t09.

SELECTION-SCREEN END OF BLOCK block2.


**---
INITIALIZATION.
  PERFORM event_build USING w_eventcat[].
  PERFORM init.


**---
TOP-OF-PAGE.
  PERFORM top_of_page.


**---
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_inven.
  PERFORM f4_help.


**---
AT SELECTION-SCREEN OUTPUT.
  PERFORM control_screen.


**---
START-OF-SELECTION.
  PERFORM set_plant.
  PERFORM read_data.


**---
END-OF-SELECTION.
  IF it_itab[] IS INITIAL.
    MESSAGE s999 WITH text-m01.
  ELSE.
    PERFORM comment_build.     " USING w_top_of_page[].
    PERFORM make_alv_grid.
  ENDIF.






*&---------------------------------------------------------------------*
*&      Form  init
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init.
*---  set storage number
  SET PARAMETER ID 'LGN' FIELD 'P01'.
ENDFORM.                    " init

*&---------------------------------------------------------------------*
*&      Form  f4_help
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_help.
*---
  DATA : BEGIN OF it_dynpfields OCCURS 0.
          INCLUDE STRUCTURE dynpread.
  DATA : END OF it_dynpfields.

  DATA : BEGIN OF value_tab OCCURS 0,
           inven TYPE aankz,
           name TYPE zdesc50,   "Description - Size 50
         END OF value_tab.

  CLEAR : it_dynpfields, it_dynpfields[], value_tab, value_tab[].

  value_tab-inven = '1'.
  value_tab-name  = '1)'.
  APPEND value_tab.

  value_tab-inven = '2'.
  value_tab-name  = '1) + 2)'.
  APPEND value_tab.

  value_tab-inven = '3'.
  value_tab-name  = '1) + 2) + 3)'.
  APPEND value_tab.


  value_tab-inven = '4'.
  value_tab-name  = '1) + 2) + 3) + 4)'.
  APPEND value_tab.

  value_tab-inven = '5'.
  value_tab-name  = '1) + 2) + 3) + 4) + 5)'.
  APPEND value_tab.

  value_tab-inven = '6'.
  value_tab-name  = '1) + 2) + 3) + 4) + 5) + 6)'.
  APPEND value_tab.

  value_tab-inven = '7'.
  value_tab-name  = '1) + 2) + 3) + 4) + 5) + 6) + 7)'.
  APPEND value_tab.

  value_tab-inven = '8'.
  value_tab-name  = '1) + 2) + 3) + 4) + 5) + 6) + 7) + 8)'.
  APPEND value_tab.

*  value_tab-inven = '9'.
*  value_tab-name  = '1) + 2) + 3) + 4) + 5) + 6) + 7) + 8) + 9)'.
*  APPEND value_tab.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'INVEN'
      dynpprog        = sy-repid  "<- Report Name
      dynpnr          = '1000'  "<- Screen Number
      dynprofield     = 'P_INVEN'  "<- Input Field Name
      window_title    = 'Line Stock'  "<- Description
      value_org       = 'S'
    TABLES
      value_tab       = value_tab
    EXCEPTIONS
      parameter_error = 1.
ENDFORM.                                                    " f4_help

*&---------------------------------------------------------------------*
*&      Form  set_plant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_plant.
*--- if 'E001', include 'P001' stock
  CLEAR : r_werks, r_werks[].

  MOVE : 'I'     TO r_werks-sign,
         'EQ'    TO r_werks-option,
         'P001'  TO r_werks-low.
  APPEND r_werks.

  IF p_werks EQ 'E001'.
    MOVE : 'I'     TO r_werks-sign,
           'EQ'    TO r_werks-option,
           'E001'  TO r_werks-low.
    APPEND r_werks.
  ENDIF.

** FURONG ON 01/31/12
   IF p_werks EQ 'E002'.
    MOVE : 'I'     TO r_werks-sign,
           'EQ'    TO r_werks-option,
           'E002'  TO r_werks-low.
    APPEND r_werks.
  ENDIF.
** END ON 01/31/12
ENDFORM.                    " set_plant

*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data.
*---
  CLEAR : it_itab, it_itab[], it_temp, it_temp[].

*--- get material master
  PERFORM get_material_master.
*--- unloading point (group gate)
  PERFORM get_unloading_point.
*--- supply to line master (Feeder & Line & W/S & RH/LH)
  PERFORM get_stl_master.
*--- get daily supply to line data (plan data)
  PERFORM get_plan_data.
*--- storage type setting
  PERFORM storage_type_setting.
*--- get stock in storage type
  PERFORM get_storage_type_stock.

*---
  PERFORM make_itab.
ENDFORM.                    " read_data

*&---------------------------------------------------------------------*
*&      Form  get_material_master
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_material_master.
*---
  SELECT d~matnr t~maktx a~bismt a~profl
         c~bstmi a~meins d~labst c~dispo
         d~werks d~lgort
                 INTO CORRESPONDING FIELDS OF TABLE it_temp
                 FROM mard AS d INNER JOIN mara AS a
                   ON d~mandt EQ a~mandt
                  AND d~matnr EQ a~matnr
                      INNER JOIN makt AS t
                         ON t~mandt EQ d~mandt
                        AND t~matnr EQ d~matnr
                        AND t~spras EQ sy-langu
                            INNER JOIN marc AS c
                               ON d~mandt EQ c~mandt
                              AND d~matnr EQ c~matnr
                              AND d~werks EQ c~werks
                            WHERE d~werks EQ p_werks
                              AND d~matnr IN s_matnr    " Material
                              AND d~lgort IN s_lgort    " S.Loc
                              AND d~lgort NE '9999'
                              AND a~profl IN s_profl    " Source
  " K : Knock Down Parts, V : Local Parts
*S__BY PAUL
*                              AND a~tempb NE c_tempb. " except JIS mat.
                              AND a~tempb NOT IN (1,2).
ENDFORM.                    " get_material_master

*&---------------------------------------------------------------------*
*&      Form  get_unloading_point
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_unloading_point.
*---
  CLEAR : it_po, it_po[].
  SELECT p~matnr p~werks p~lgort p~lgbzo
                 INTO CORRESPONDING FIELDS OF TABLE it_po
                 FROM ekko AS k INNER JOIN ekpo AS p
                   ON k~mandt EQ p~mandt
                  AND k~ebeln EQ p~ebeln
                      FOR ALL ENTRIES IN it_temp
                WHERE k~bstyp EQ 'L'     " Scheduling Agreement
                  AND p~matnr EQ it_temp-matnr
                  AND p~loekz EQ space
                  AND k~loekz EQ space
                  AND p~elikz EQ space.
ENDFORM.                    " get_unloading_point

*&---------------------------------------------------------------------*
*&      Form  get_stl_master
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_stl_master.
*---
  CLEAR : it_mast, it_mast[].
  SELECT matnr feedr
         zline works
         rh_lh
                     INTO TABLE it_mast
                     FROM ztmm_mast
                          FOR ALL ENTRIES IN it_temp
                    WHERE matnr EQ it_temp-matnr
                      AND werks EQ it_temp-werks.
ENDFORM.                    " get_stl_master

*&---------------------------------------------------------------------*
*&      Form  get_plan_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_plan_data.
*---
  CLEAR : it_ztmm_nstl, it_ztmm_nstl[], it_ztmm_nstl_sum,
          it_ztmm_nstl_sum[], w_current_date.

  MOVE : sy-datum TO w_current_date.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ztmm_nstl
           FROM ztmm_nstl
                FOR ALL ENTRIES IN it_temp
          WHERE datum EQ w_current_date
            AND matnr EQ it_temp-matnr.

  SORT it_ztmm_nstl BY matnr.

*---
  CLEAR : it_worktime, it_worktime[].
  CALL FUNCTION 'Z_FMM_NSTL_WORKTIME'
    EXPORTING
      i_datum         = w_current_date
    TABLES
      t_nstl_worktime = it_worktime.

  DELETE it_worktime WHERE datum NE w_current_date.

*---
  CHECK NOT it_worktime[] IS INITIAL.

  DATA : BEGIN OF it_shift OCCURS 0,
           tprog LIKE it_worktime-tprog,
           count TYPE i,
         END OF it_shift.

  DATA : l_s_index(2) TYPE n,
         l_index(2) TYPE n,
         l_count(2) TYPE n,
         l_field01(19),
         l_field02(23),
         l_field03(23).

  FIELD-SYMBOLS : <fs01>, <fs02>, <fs03>.

  CLEAR : it_shift, it_shift[], l_s_index, l_index, l_count.

  LOOP AT it_worktime.
    MOVE-CORRESPONDING it_worktime TO it_shift.
    MOVE : 1                       TO it_shift-count.
    COLLECT it_shift.
    CLEAR : it_shift, it_worktime.
  ENDLOOP.

  LOOP AT it_shift.
    LOOP AT it_ztmm_nstl WHERE kaptprog EQ it_shift-tprog.
      MOVE : it_ztmm_nstl-matnr TO it_ztmm_nstl_sum-matnr.
      DO it_shift-count TIMES.
        l_s_index = l_count + sy-index.
        MOVE : sy-index TO l_index.
        CONCATENATE : 'IT_ZTMM_NSTL-TIME'     l_index   INTO l_field01,
                      'IT_ZTMM_NSTL_SUM-TIME' l_s_index INTO l_field02.
        ASSIGN : (l_field01) TO <fs01>,
                 (l_field02) TO <fs02>.
        MOVE : <fs01> TO <fs02>.
      ENDDO.
      COLLECT it_ztmm_nstl_sum.
      CLEAR : l_s_index.
    ENDLOOP.
    l_count = l_count + it_shift-count.
  ENDLOOP.

*---
  DATA : l_tabix(2) TYPE n.

  CLEAR : l_tabix.

  LOOP AT it_worktime WHERE begzt+8(6) <= p_plan
                        AND endzt+8(6) >= p_plan.
  ENDLOOP.

  CHECK sy-subrc EQ 0.
  CHECK NOT p_inter IS INITIAL.

  DATA : l_field_num(2) TYPE n.

  CLEAR : it_plan, it_plan[].

  MOVE : sy-tabix TO l_tabix.

  LOOP AT it_ztmm_nstl_sum.
    MOVE : l_tabix TO l_field_num.
    DO p_inter TIMES.
      CONCATENATE 'IT_ZTMM_NSTL_SUM-TIME' l_field_num INTO l_field03.
      ASSIGN : (l_field03) TO <fs03>.
      it_plan-time01 = it_plan-time01 + <fs03>.
      CLEAR : <fs03>.
      l_field_num = l_tabix + sy-index.
    ENDDO.
    MOVE : it_ztmm_nstl_sum-matnr TO it_plan-matnr.
    APPEND it_plan.
    CLEAR : it_plan, l_field_num.
  ENDLOOP.
ENDFORM.                    " get_plan_data

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
  CLEAR : r_lgtyp1, r_lgtyp1[], r_lgtyp2, r_lgtyp2[], r_lgtyp3,
          r_lgtyp3[], r_lgtyp4, r_lgtyp4[], r_lgtyp5, r_lgtyp5[],
          r_lgtyp, r_lgtyp[].

*--- Line Stock
  append_storage_type : '441' r_lgtyp1,
                        '442' r_lgtyp1,
                        '443' r_lgtyp1,
                        '444' r_lgtyp1,
                        '445' r_lgtyp1.
*--- W/H Stock
  append_storage_type : '431' r_lgtyp2,
                        '432' r_lgtyp2,
                        '433' r_lgtyp2,
                        '434' r_lgtyp2,
                        '435' r_lgtyp2,
                        '436' r_lgtyp2,
                        '437' r_lgtyp2.
*--- CC-Bin Stock
  append_storage_type : '422' r_lgtyp3.
*--- CC-Rack Stock
  append_storage_type : '421' r_lgtyp4.
*--- CY Stock
  append_storage_type : '411' r_lgtyp5.

*---
  APPEND LINES OF r_lgtyp1 TO r_lgtyp.

*  IF p_inven GE 2.
  APPEND LINES OF r_lgtyp2 TO r_lgtyp.
*  ENDIF.

*  IF p_inven GE 3.
  APPEND LINES OF r_lgtyp3 TO r_lgtyp.
*  ENDIF.

*  IF p_inven GE 4.
  APPEND LINES OF r_lgtyp4 TO r_lgtyp.
*  ENDIF.

*  IF p_inven GE 5.
  APPEND LINES OF r_lgtyp5 TO r_lgtyp.
*  ENDIF.
ENDFORM.                    " storage_type_setting

*&---------------------------------------------------------------------*
*&      Form  get_storage_type_stock
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_storage_type_stock.
*---
  CLEAR : it_stype_stock, it_stype_stock[],
          it_stype_stock_sum, it_stype_stock_sum[].

*---
  SELECT werks lgort
         matnr lgtyp
         gesme
                     INTO CORRESPONDING FIELDS OF TABLE it_stype_stock
                     FROM lqua
                      FOR ALL ENTRIES IN it_temp
                    WHERE matnr EQ it_temp-matnr
                      AND werks IN r_werks
*                      AND lgort IN s_lgort
                      AND lgort NE '9999'
                      AND lgtyp IN r_lgtyp.

  LOOP AT it_stype_stock.
    MOVE-CORRESPONDING it_stype_stock TO it_stype_stock_sum.
    IF it_stype_stock-lgtyp IN r_lgtyp1.
      MOVE : it_stype_stock-gesme TO it_stype_stock_sum-gesme1.
    ELSEIF it_stype_stock-lgtyp IN r_lgtyp2.
      MOVE : it_stype_stock-gesme TO it_stype_stock_sum-gesme2.
    ELSEIF it_stype_stock-lgtyp IN r_lgtyp3.
      MOVE : it_stype_stock-gesme TO it_stype_stock_sum-gesme3.
    ELSEIF it_stype_stock-lgtyp IN r_lgtyp4.
      MOVE : it_stype_stock-gesme TO it_stype_stock_sum-gesme4.
    ELSEIF it_stype_stock-lgtyp IN r_lgtyp5.
      MOVE : it_stype_stock-gesme TO it_stype_stock_sum-gesme5.
    ENDIF.
    COLLECT it_stype_stock_sum.
    CLEAR : it_stype_stock_sum, it_stype_stock.
  ENDLOOP.

  SORT it_stype_stock_sum BY werks matnr lgort.
ENDFORM.                    " get_storage_type_stock

*&---------------------------------------------------------------------*
*&      Form  make_itab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_itab.
*---
  SORT it_temp BY matnr werks.

  LOOP AT it_temp.
    MOVE-CORRESPONDING it_temp TO it_itab.
*--- check and get supply to line master information
    CLEAR : it_mast.
    READ TABLE it_mast WITH KEY matnr = it_temp-matnr.
    CHECK it_mast-feedr IN s_feedr.
    CHECK it_mast-zline IN s_zline.
    CHECK it_mast-works IN s_works.
    MOVE : it_mast-feedr TO it_itab-feedr,
           it_mast-zline TO it_itab-zline,
           it_mast-works TO it_itab-works,
           it_mast-rh_lh TO it_itab-rh_lh.
*--- get Arrival / Shipment / Due-In Quantity
    PERFORM get_import_data.
*--- get unloading point (group gate)
    CLEAR : it_po.
    READ TABLE it_po WITH KEY werks = it_temp-werks
                              matnr = it_temp-matnr.
    IF sy-subrc EQ 0.
      MOVE : it_po-lgbzo TO it_itab-lgbzo.
    ENDIF.
*--- get usage
    CLEAR : stpo.
    SELECT SINGLE menge INTO it_itab-menge
                        FROM stpo
                       WHERE idnrk EQ it_itab-matnr.
*--- get storage bin stock
    PERFORM get_stock.
*--- get plan quantity
    CLEAR : it_plan.
    READ TABLE it_plan WITH KEY matnr = it_temp-matnr.
    MOVE : it_plan-time01 TO it_itab-plan.
*--- get difference (Inventory - Plan)
    it_itab-diff = it_itab-labst - it_itab-plan.
*---
    APPEND it_itab.
    CLEAR : it_itab, it_temp.
  ENDLOOP.
ENDFORM.                    " make_itab

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
  w_line-info = text-100.
  APPEND w_line TO w_top_of_page.

  CLEAR : w_line.
  APPEND INITIAL LINE TO w_top_of_page.

  append_top :
      'S' text-003 p_werks ' ',
      'S' text-006 s_matnr-low s_matnr-high,
      'S' text-007 s_lgort-low s_lgort-high,
      'S' text-008 s_lgtyp-low s_lgtyp-high,
      'S' text-009 s_feedr-low s_feedr-high,
      'S' text-010 s_zline-low s_zline-high,
      'S' text-011 s_works-low s_works-high,
      'S' text-012 s_profl-low s_profl-high,
      'S' text-013 p_plan '',
      'S' text-014 p_inven ''.
ENDFORM.                    " comment_build

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
    w_col_pos 'MAKTX' 30 'Material Desc'  'CHAR' ' '  ''      '' '',
    w_col_pos 'BISMT' 18 'ALC Code'       'CHAR' ' '  ''      '' '',
    w_col_pos 'WERKS' 04 'Plant'          'CHAR' ' '  ''      '' '',
    w_col_pos 'LGORT' 04 'SLoc'           'CHAR' ' '  ''      '' '',
    w_col_pos 'LGBZO' 10 'Unloading Point' 'CHAR' ' '  ''      '' '',
    w_col_pos 'FEEDR' 05 'Feeder'         'CHAR' ' '  ''      '' '',
    w_col_pos 'ZLINE' 01 'Line'           'CHAR' ' '  ''      '' '',
    w_col_pos 'WORKS' 05 'W/S'            'CHAR' ' '  ''      '' '',
    w_col_pos 'RH_LH' 02 'RH/LH'          'CHAR' ' '  ''      '' '',
    w_col_pos 'MEINS' 03 'UoM'            'UNIT' ' '  ''      '' '',
    w_col_pos 'MENGE' 12 'Usage'          'QUAN' ' '  'MEINS' '' '',
    w_col_pos 'PROFL' 03 'Source'         'CHAR' ' '  ''      '' '',
    w_col_pos 'BSTMI' 12 'Qty/con'        'QUAN' ' '  'MEINS' '' '',
    w_col_pos 'PLAN'  12 'Plan'           'QUAN' ' '  'MEINS' '' '',
    w_col_pos 'DIFF'  12 'Diffrence'      'QUAN' ' '  'MEINS' '' '',
    w_col_pos 'LABST' 12 'Inventory'      'QUAN' ' '  'MEINS' '' '',
    w_col_pos 'GESME' 12 'Line Stock'     'QUAN' ' '  'MEINS' '' '',
    w_col_pos 'WSTOK' 12 'W/H Stock'      'QUAN' ' '  'MEINS' '' '',
    w_col_pos 'CCBIN' 12 'CC-Bin Stock'   'QUAN' ' '  'MEINS' '' '',
    w_col_pos 'CCRACK' 12 'CC-Rack Stock'  'QUAN' ' '  'MEINS' '' '',
    w_col_pos 'CYSTOK' 12 'CY Stock'       'QUAN' ' '  'MEINS' '' '',
*    w_col_pos 'NOTYETGR' 12 'Not Yet GR'   'QUAN' ' '  'MEINS' '' '',
    w_col_pos 'ARRIV' 12 'Arrival'        'QUAN' ' '  'MEINS' '' '',
    w_col_pos 'SHIPM' 12 'Shipment'       'QUAN' ' '  'MEINS' '' '',
    w_col_pos 'DUIN'  12 'Due-In'         'QUAN' ' '  'MEINS' '' '',
    w_col_pos 'DISPO' 12 'Manager'        'CHAR' ' '  ''      '' ''.
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
  append_sortcat : '1' 'MATNR' 'IT_ITAB' 'X' '',
*                  '2' 'MAKTX' 'IT_ITAB' 'X' '',
*                  '3' 'BISMT' 'IT_ITAB' 'X' '',
                   '4' 'WERKS' 'IT_ITAB' 'X' '',
*                  '5' 'LGBZO' 'IT_ITAB' 'X' '',
*                  '6' 'FEEDR' 'IT_ITAB' 'X' '',
*                  '7' 'ZLINE' 'IT_ITAB' 'X' '',
*                  '8' 'WORKS' 'IT_ITAB' 'X' '',
*                  '9' 'RH_LH' 'IT_ITAB' 'X' '',
                  '10' 'PROFL' 'IT_ITAB' 'X' '',
*                 '11' 'BSTMI' 'IT_ITAB' 'X' '',
*                  '12' 'ARRIV' 'IT_ITAB' 'X' '',
*                  '13' 'SHIPM' 'IT_ITAB' 'X' '',
*                  '14' 'DUIN'  'IT_ITAB' 'X' '',
                  '15' 'DISPO' 'IT_ITAB' 'X' ''.
ENDFORM.                    " build_sortcat

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
  PERFORM build_fieldcat.
  PERFORM build_sortcat.

  CLEAR : w_program.

  MOVE : sy-repid TO w_program.

  w_layout-colwidth_optimize = 'X'.

  DATA :  l_print_p TYPE slis_print_alv.  " print setting

  l_print_p-no_coverpage = 'X'.
  l_print_p-no_print_listinfos = 'X'.
  l_print_p-no_change_print_params = 'X'.
  l_print_p-no_print_selinfos = 'X'.

*---
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_bypassing_buffer      = 'X'
      i_callback_program      = w_program
      i_callback_user_command = 'USER_COMMAND'
      is_layout               = w_layout
      it_fieldcat             = w_fieldcat[]
      it_events               = w_eventcat[]
      it_sort                 = w_sortcat[]
      i_save                  = 'A'
      is_print                = l_print_p
    TABLES
      t_outtab                = it_itab
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
ENDFORM.                    " make_alv_grid

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
    WHEN 'REGIST'.
      CALL FUNCTION 'REUSE_ALV_LIST_LAYOUT_INFO_SET'
        EXPORTING
          it_fieldcat = w_fieldcat[].
    WHEN '&IC1'. " Double Click
      PERFORM detail_list USING selfield. "Executed with double click
  ENDCASE.
ENDFORM.                    "user_command

*&---------------------------------------------------------------------*
*&      Form  detail_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SELFIELD  text
*----------------------------------------------------------------------*
FORM detail_list USING    selfield TYPE slis_selfield.
*---
  DATA : wa_it_kd LIKE it_kd.

  READ TABLE it_itab INDEX selfield-tabindex.

  CHECK sy-subrc EQ 0.

*---
  IF it_itab-profl EQ 'K'.   "Knock Down Parts
    SELECT a~vgbel a~vgpos
           b~traid a~kdmat
           a~vrkme a~lfimg
           b~zzdepdt b~zzarrdt
                   INTO CORRESPONDING FIELDS OF TABLE it_kd
                   FROM lips AS a INNER JOIN likp AS b
                     ON a~mandt EQ b~mandt
                    AND a~vbeln EQ b~vbeln
                  WHERE b~lfart EQ 'EL'
                    AND matnr EQ it_itab-matnr
                    AND a~werks EQ it_itab-werks
                    AND lgort EQ it_itab-lgort.
*--- GR Check
    LOOP AT it_kd.
      CLEAR : ekbe.
      SELECT SINGLE * FROM ekbe
                     WHERE ebeln EQ it_kd-vgbel
                       AND ebelp EQ it_kd-vgpos+1(5)
                       AND vgabe EQ '1'.
      IF sy-subrc EQ 0.
        DELETE it_kd.
      ENDIF.
    ENDLOOP.
  ELSEIF it_itab-profl EQ 'V'.  "Local Parts
    SELECT b~vbeln a~posnr
           b~zzdepdt b~zzdeptm
           b~zzarrdt b~zzarrtm
           b~lifex
*           b~traid
           b~lgbzo
           a~lfimg
                   INTO TABLE it_lp
                   FROM lips AS a INNER JOIN likp AS b
                     ON a~mandt EQ b~mandt
                    AND a~vbeln EQ b~vbeln
                  WHERE b~lfart EQ 'EL'
                    AND matnr EQ it_itab-matnr
                    AND a~werks EQ it_itab-werks
                    AND lgort EQ it_itab-lgort.
*--- GR Check
    LOOP AT it_lp.
      CLEAR : vbfa.
      SELECT SINGLE * FROM vbfa
                     WHERE vbelv EQ it_lp-vbeln
                       AND posnv EQ it_lp-posnr
                       AND vbtyp_n EQ 'R'.
      IF sy-subrc EQ 0.
        DELETE it_lp.
      ENDIF.
    ENDLOOP.
*---
*    it_lp-duin = it_itab-duin.
    it_lp-meins = it_itab-meins.
    it_lp-matnr = it_itab-matnr.
    MODIFY it_lp TRANSPORTING meins matnr
                        WHERE matnr EQ space.
  ENDIF.

  PERFORM detail_list_write.
ENDFORM.                    " detail_list

*&---------------------------------------------------------------------*
*&      Form  detail_list_write
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM detail_list_write.
*---
  IF it_itab-profl = 'K'.   "Knock Down Parts
    PERFORM build_fieldcat_detail_kd.
    PERFORM list_write_detail_kd.
  ELSEIF it_itab-profl = 'V'.  "Local Parts
    PERFORM build_fieldcat_detail_lp.
    PERFORM list_write_detail_lp.
  ENDIF.
ENDFORM.                    " detail_list_write

*&---------------------------------------------------------------------*
*&      Form  build_fieldcat_detail_kd
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcat_detail_kd.
*---
  CLEAR : w_fieldcat[].

**--- &1 : position       &2 : field name       &3 : field length
**--- &4 : description    &5 : field type       &6 : key
**--- &7 : qty field      &8 : cur field        &9 : no out
  append_fieldcat :
    w_col_pos 'VGBEL' 10 'PO Number'      'CHAR' 'X'  ''      '' '',
    w_col_pos 'VGPOS' 06 'Item'           'NUMC' ' '  ''      '' '',
    w_col_pos 'TRAID' 20 'Container No'   'CHAR' ' '  ''      '' '',
    w_col_pos 'KDMAT' 20 'Case Number'    'CHAR' ' '  ''      '' '',
    w_col_pos 'VRKME' 03 'UoM'            'UNIT' ' '  ''      '' '',
    w_col_pos 'LFIMG' 12 'Quantity'       'QUAN' ' '  'VRKME' '' '',
    w_col_pos 'ZZDEPDT' 10 'ETD'            'DATS' ' '  ''      '' '',
    w_col_pos 'ZZARRDT' 10 'ETA'            'DATS' ' '  ''      '' ''.
ENDFORM.                    " build_fieldcat_detail_kd

*&---------------------------------------------------------------------*
*&      Form  list_write_detail_kd
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM list_write_detail_kd.
*---
  DATA : l_print_p TYPE slis_print_alv.  " print setting

  CLEAR : w_program.

  MOVE : sy-repid TO w_program.

  l_print_p-no_coverpage = 'X'.
  l_print_p-no_print_listinfos = 'X'.
  l_print_p-no_change_print_params = 'X'.
  l_print_p-no_print_selinfos = 'X'.

  MOVE : 'X' TO w_layout-colwidth_optimize.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_bypassing_buffer    = 'X'
      i_callback_program    = w_program
      i_grid_title          = text-005  "Sub list(KD)
      is_layout             = w_layout
      it_fieldcat           = w_fieldcat[]
      i_save                = 'A'
      is_print              = l_print_p
      i_screen_start_column = 10
      i_screen_start_line   = 5
      i_screen_end_column   = 120
      i_screen_end_line     = 15
    TABLES
      t_outtab              = it_kd
    EXCEPTIONS
      program_error         = 1
      OTHERS                = 2.
ENDFORM.                    " list_write_detail_kd

*&---------------------------------------------------------------------*
*&      Form  build_fieldcat_detail_lp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcat_detail_lp.
*---
  CLEAR : w_fieldcat[].

**--- &1 : position       &2 : field name       &3 : field length
**--- &4 : description    &5 : field type       &6 : key
**--- &7 : qty field      &8 : cur field        &9 : no out
  append_fieldcat :
    w_col_pos 'VBELN' 10 'ASN Number'     'CHAR' 'X'  ''      '' '',
    w_col_pos 'POSNR' 06 'Item'           'NUMC' ' '  ''      '' '',
    w_col_pos 'ZZDEPDT' 10 'ETD'            'DATS' ' '  ''      '' '',
    w_col_pos 'ZZDEPTM' 08 'ETD'            'TIMS' ' '  ''      '' '',
    w_col_pos 'ZZARRDT' 10 'ETA'            'DATS' ' '  ''      '' '',
    w_col_pos 'ZZARRTM' 08 'ETA'            'TIMS' ' '  ''      '' '',
    w_col_pos 'LIFEX' 20 'Trailer No'     'CHAR' ' '  ''      '' '',
*    w_col_pos 'TRAID' 20 'Trailer No'     'CHAR' ' '  ''      '' '',
    w_col_pos 'LGBZO' 10 'Gate Number'    'CHAR' ' '  ''      '' '',
    w_col_pos 'MATNR' 18 'Material No'    'CHAR' ' '  ''      '' '',
    w_col_pos 'MEINS' 03 'UoM'            'UNIT' ' '  ''      '' '',
    w_col_pos 'LFIMG' 12 'Quantity'       'QUAN' ' '  'MEINS' '' ''.
*    w_col_pos 'DUIN'  12 'Due-In'         'QUAN' ' '  'MEINS' '' ''.
ENDFORM.                    " build_fieldcat_detail_lp

*&---------------------------------------------------------------------*
*&      Form  list_write_detail_lp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM list_write_detail_lp.
*---
  DATA : l_print_p TYPE slis_print_alv.  " print setting

  CLEAR : w_program.

  MOVE : sy-repid TO w_program.

  l_print_p-no_coverpage = 'X'.
  l_print_p-no_print_listinfos = 'X'.
  l_print_p-no_change_print_params = 'X'.
  l_print_p-no_print_selinfos = 'X'.

  MOVE : 'X' TO w_layout-colwidth_optimize.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_bypassing_buffer    = 'X'
      i_callback_program    = w_program
      i_grid_title          = text-004  "Sub list(LP)
      is_layout             = w_layout
      it_fieldcat           = w_fieldcat[]
      i_save                = 'A'
      is_print              = l_print_p
      i_screen_start_column = 10
      i_screen_start_line   = 5
      i_screen_end_column   = 120
      i_screen_end_line     = 15
    TABLES
      t_outtab              = it_lp
    EXCEPTIONS
      program_error         = 1
      OTHERS                = 2.
ENDFORM.                    " list_write_detail_lp

*&---------------------------------------------------------------------*
*&      Form  get_stock
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_stock.
*---
  CLEAR : it_stype_stock_sum.

  IF p_werks EQ c_p001.
    READ TABLE it_stype_stock_sum WITH KEY werks = it_itab-werks
                                           lgort = it_itab-lgort
                                           matnr = it_itab-matnr.
    IF sy-subrc EQ 0.
      MOVE : it_stype_stock_sum-gesme1 TO it_itab-gesme,
             it_stype_stock_sum-gesme2 TO it_itab-wstok,
             it_stype_stock_sum-gesme3 TO it_itab-ccbin,
             it_stype_stock_sum-gesme4 TO it_itab-ccrack,
             it_stype_stock_sum-gesme5 TO it_itab-cystok.
    ENDIF.
  ELSEIF p_werks EQ c_e001.
    LOOP AT it_stype_stock_sum WHERE matnr EQ it_itab-matnr.
      it_itab-gesme  = it_itab-gesme  + it_stype_stock_sum-gesme1.
      it_itab-wstok  = it_itab-wstok  + it_stype_stock_sum-gesme2.
      it_itab-ccbin  = it_itab-ccbin  + it_stype_stock_sum-gesme3.
      it_itab-ccrack = it_itab-ccrack + it_stype_stock_sum-gesme4.
      it_itab-cystok = it_itab-cystok + it_stype_stock_sum-gesme5.
    ENDLOOP.
  ENDIF.

*--- get Import Information, when Inventory Type equal than '7'.
  IF p_inven GE '7'.
  ENDIF.

*---
  PERFORM line_inventory_cal.
ENDFORM.                    " get_stock

*&---------------------------------------------------------------------*
*&      Form  line_inventory_cal
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM line_inventory_cal.
*---
  CLEAR : it_itab-labst.

  CASE p_inven.
    WHEN '1'.
      it_itab-labst = it_itab-gesme.
    WHEN '2'.
      it_itab-labst = it_itab-gesme + it_itab-wstok .
    WHEN '3'.
      it_itab-labst = it_itab-gesme + it_itab-wstok + it_itab-ccbin.
    WHEN '4'.
      it_itab-labst = it_itab-gesme + it_itab-wstok + it_itab-ccbin +
                      it_itab-ccrack.
    WHEN '5'.
      it_itab-labst = it_itab-gesme  + it_itab-wstok + it_itab-ccbin +
                      it_itab-ccrack + it_itab-cystok.
    WHEN '6'.   "Not Yet GR
      it_itab-labst = it_itab-gesme  + it_itab-wstok   + it_itab-ccbin +
                         it_itab-ccrack + it_itab-cystok + it_itab-arriv.
    WHEN '7'.
      it_itab-labst = it_itab-gesme  + it_itab-wstok   + it_itab-ccbin +
                         it_itab-ccrack + it_itab-cystok + it_itab-arriv
                                           + it_itab-shipm.
    WHEN '8'.
      it_itab-labst = it_itab-gesme  + it_itab-wstok   + it_itab-ccbin +
                         it_itab-ccrack + it_itab-cystok + it_itab-arriv
                                         + it_itab-shipm  + it_itab-duin.
  ENDCASE.
ENDFORM.                    " line_inventory_cal

*&---------------------------------------------------------------------*
*&      Form  control_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM control_screen.
*---
  LOOP AT SCREEN.
    IF screen-group1 EQ 'GR1'.
      screen-input = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.                    " control_screen

*&---------------------------------------------------------------------*
*&      Form  get_import_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_import_data.
*--- in case of KD material
  IF it_temp-profl EQ c_kd.
    PERFORM read_po_info USING c_bsart_kd.
    PERFORM read_id_info.
    PERFORM read_import_info.
    PERFORM read_gr_info.
    CLEAR : it_po_sum, it_id_sum, it_import_sum01, it_gr_sum.
    READ TABLE it_po_sum WITH KEY matnr = it_temp-matnr.
    READ TABLE it_id_sum WITH KEY matnr = it_temp-matnr.
    READ TABLE it_import_sum01 WITH KEY matnr = it_temp-matnr.
    READ TABLE it_gr_sum WITH KEY matnr = it_temp-matnr.
*--- get arrival quantity
    it_itab-arriv = it_import_sum01-blmenge - it_gr_sum-menge.
*--- get shipment quantity
    it_itab-shipm = it_id_sum-lfimg - it_import_sum01-blmenge.
*--- get due-in quantity
    it_itab-duin  = it_po_sum-menge - it_id_sum-lfimg.

*--- in case of LP material
  ELSEIF it_temp-profl EQ c_lp.
*--- for JIT
    PERFORM read_po_info_lp.
    PERFORM read_id_info_lp.
    PERFORM read_gr_info_lp.
    CLEAR : it_po_sum, it_id_sum, it_vbfa_sum.
    READ TABLE it_po_sum WITH KEY matnr = it_temp-matnr.
    READ TABLE it_id_sum WITH KEY matnr = it_temp-matnr.
    READ TABLE it_vbfa_sum WITH KEY matnr = it_temp-matnr.
*--- get due-in quantity
    it_itab-duin  = it_po_sum-menge - it_id_sum-lfimg.
*--- get shipment quantity
    it_itab-shipm = it_id_sum-lfimg - it_vbfa_sum-rfmng.

*--- for EM
    PERFORM read_po_info USING c_bsart_em.
    PERFORM read_id_info.
    PERFORM read_gr_info.
    CLEAR : it_po_sum, it_id_sum, it_gr_sum.
    READ TABLE it_po_sum WITH KEY matnr = it_temp-matnr.
    READ TABLE it_id_sum WITH KEY matnr = it_temp-matnr.
    READ TABLE it_gr_sum WITH KEY matnr = it_temp-matnr.
*--- get due-in quantity
    it_itab-duin = it_itab-duin + it_po_sum-menge - it_id_sum-lfimg.
*--- get shipment quantity
    it_itab-shipm = it_itab-shipm + it_id_sum-lfimg - it_gr_sum-menge.
  ENDIF.
ENDFORM.                    " get_import_data

*&---------------------------------------------------------------------*
*&      Form  read_po_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_po_info USING p_bsart.
*---
  CLEAR : it_po_temp, it_po_temp[], it_po_sum, it_po_sum[].

  SELECT b~ebeln
         b~ebelp AS vgpos
         b~matnr
         b~menge INTO CORRESPONDING FIELDS OF TABLE it_po_temp
                 FROM ekko AS a INNER JOIN ekpo AS b
                   ON a~mandt EQ b~mandt
                  AND a~ebeln EQ b~ebeln
                WHERE a~bsart EQ p_bsart
                  AND a~loekz EQ space
                  AND b~loekz EQ space
                  AND b~elikz EQ space
                  AND b~matnr EQ it_temp-matnr
                  AND b~werks EQ it_temp-werks
                  AND b~lgort EQ it_temp-lgort.

  LOOP AT it_po_temp.
    MOVE-CORRESPONDING it_po_temp TO it_po_sum.
    COLLECT it_po_sum.
    CLEAR : it_po_temp, it_po_sum.
  ENDLOOP.
ENDFORM.                    " read_po_info

*&---------------------------------------------------------------------*
*&      Form  read_id_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_id_info.
*---
  CHECK NOT it_po_temp[] IS INITIAL.

  CLEAR : it_id_temp, it_id_temp[], it_id_sum, it_id_sum[].

  SELECT vbeln
         posnr
         matnr
         lfimg INTO CORRESPONDING FIELDS OF TABLE it_id_temp
               FROM lips
                FOR ALL ENTRIES IN it_po_temp
              WHERE matnr EQ it_po_temp-matnr
                AND vgbel EQ it_po_temp-ebeln
                AND vgpos EQ it_po_temp-vgpos.

  LOOP AT it_id_temp.
    MOVE-CORRESPONDING it_id_temp TO it_id_sum.
    COLLECT it_id_sum.
    CLEAR : it_id_temp, it_id_sum.
  ENDLOOP.
ENDFORM.                    " read_id_info

*&---------------------------------------------------------------------*
*&      Form  read_import_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_import_info.
*---
  CHECK NOT it_po_temp[] IS INITIAL.

  CLEAR : it_import_temp, it_import_temp[], it_import_sum01,
          it_import_sum01[], it_import_sum02, it_import_sum02[].

  SELECT b~matnr
         a~zfreta
         b~blmenge
                   INTO CORRESPONDING FIELDS OF TABLE it_import_temp
                   FROM ztbl AS a INNER JOIN ztblit AS b
                        ON a~mandt EQ b~mandt
                       AND a~zfblno EQ b~zfblno
                    FOR ALL ENTRIES IN it_po_temp
                  WHERE b~matnr EQ it_po_temp-matnr
                    AND b~ebeln EQ it_po_temp-ebeln
                    AND b~ebelp EQ it_po_temp-vgpos+1(5).

  LOOP AT it_import_temp.
    IF NOT it_import_temp-zfreta IS INITIAL
       AND it_import_temp-zfreta LE sy-datum.
      MOVE-CORRESPONDING it_import_temp TO it_import_sum01.
    ELSE.     " IF it_import_temp-zfreta IS INITIAL.
      MOVE-CORRESPONDING it_import_temp TO it_import_sum02.
    ENDIF.
    COLLECT : it_import_sum01, it_import_sum02.
    CLEAR : it_import_temp, it_import_sum01, it_import_sum02.
  ENDLOOP.
ENDFORM.                    " read_import_info

*&---------------------------------------------------------------------*
*&      Form  read_gr_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_gr_info.
*---
  CHECK NOT it_po_temp[] IS INITIAL.

  CLEAR : it_gr_temp, it_gr_temp[], it_gr_sum, it_gr_sum[].

  SELECT matnr
         menge
         bwart
         shkzg INTO CORRESPONDING FIELDS OF TABLE it_gr_temp
               FROM ekbe
                FOR ALL ENTRIES IN it_po_temp
              WHERE matnr EQ it_po_temp-matnr
                AND ebeln EQ it_po_temp-ebeln
                AND ebelp EQ it_po_temp-vgpos+1(5)
                AND vgabe EQ '1'
                AND bewtp EQ 'E'.

  LOOP AT it_gr_temp.
    MOVE-CORRESPONDING it_gr_temp TO it_gr_sum.
    IF it_gr_temp-shkzg EQ 'H'.
      it_gr_temp-menge = it_gr_temp-menge * - 1.
    ENDIF.
    COLLECT it_gr_sum.
    CLEAR : it_gr_temp, it_gr_sum.
  ENDLOOP.
ENDFORM.                    " read_gr_info

*&---------------------------------------------------------------------*
*&      Form  read_po_info_lp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_po_info_lp.
*---
  CLEAR : it_po_temp, it_po_temp[].

** FURONG ON 01/31/12
  IF p_werks = 'P001'.
    c_calid =  c_calid_p.
  ELSE.
    c_calid =  c_calid_e.
  ENDIF.
** END ON 01/31/12

  SELECT b~ebeln
         b~ebelp AS vgpos
         b~matnr
         c~menge
*         b~menge
         b~etfz1
         c~eindt
         c~uzeit
                 INTO CORRESPONDING FIELDS OF TABLE it_po_temp
                 FROM ekko AS a INNER JOIN ekpo AS b
                   ON a~mandt EQ b~mandt
                  AND a~ebeln EQ b~ebeln
                      INNER JOIN eket AS c
                         ON b~mandt EQ c~mandt
                        AND b~ebeln EQ c~ebeln
                        AND b~ebelp EQ c~ebelp
                WHERE a~bsart EQ c_bsart_jit
                  AND a~loekz EQ space
                  AND b~loekz EQ space
                  AND b~elikz EQ space
                  AND b~matnr EQ it_temp-matnr
                  AND b~werks EQ it_temp-werks
                  AND b~lgort EQ it_temp-lgort.

  DATA : l_date TYPE d,
         l_factorydate LIKE scal-facdate.

** FURONG ON 02/1/12
  DATA: W_factorydate LIKE scal-facdate.
 CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
      EXPORTING
        date                         = sy-datum
        factory_calendar_id          = c_calid
      IMPORTING
        factorydate                  = W_factorydate
      EXCEPTIONS
        calendar_buffer_not_loadable = 1
        correct_option_invalid       = 2
        date_after_range             = 3
        date_before_range            = 4
        date_invalid                 = 5
        factory_calendar_not_found   = 6
        OTHERS                       = 7.
** END ON 02/01/12

  LOOP AT it_po_temp.
    CLEAR : l_date, l_factorydate.
*    CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
*      EXPORTING
*        date                         = sy-datum
*        factory_calendar_id          = c_calid
*      IMPORTING
*        factorydate                  = l_factorydate
*      EXCEPTIONS
*        calendar_buffer_not_loadable = 1
*        correct_option_invalid       = 2
*        date_after_range             = 3
*        date_before_range            = 4
*        date_invalid                 = 5
*        factory_calendar_not_found   = 6
*        OTHERS                       = 7.
    l_factorydate = W_factorydate + it_po_temp-etfz1.
    CALL FUNCTION 'FACTORYDATE_CONVERT_TO_DATE'
      EXPORTING
        factorydate                  = l_factorydate
        factory_calendar_id          = c_calid
      IMPORTING
        date                         = l_date
      EXCEPTIONS
        calendar_buffer_not_loadable = 1
        factorydate_after_range      = 2
        factorydate_before_range     = 3
        factorydate_invalid          = 4
        factory_calendar_id_missing  = 5
        factory_calendar_not_found   = 6
        OTHERS                       = 7.
    IF it_po_temp-eindt GT l_date.
      DELETE it_po_temp.
    ELSE.
      MOVE-CORRESPONDING it_po_temp TO it_po_sum.
      COLLECT it_po_sum.
      CLEAR : it_po_temp, it_po_sum.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " read_po_info_lp

*&---------------------------------------------------------------------*
*&      Form  read_id_info_lp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_id_info_lp.
*---
  CHECK NOT it_po_temp[] IS INITIAL.

  CLEAR : it_id_temp, it_id_temp[], it_id_sum, it_id_sum[].

  SELECT vbeln
         posnr
         matnr
         lfimg INTO CORRESPONDING FIELDS OF TABLE it_id_temp
               FROM zvmm_delv
                FOR ALL ENTRIES IN it_po_temp
              WHERE matnr EQ it_po_temp-matnr
                AND vgbel EQ it_po_temp-ebeln
                AND vgpos EQ it_po_temp-vgpos
                AND lfdat EQ it_po_temp-eindt
                AND lfuhr EQ it_po_temp-uzeit.

  LOOP AT it_id_temp.
    MOVE-CORRESPONDING it_id_temp TO it_id_sum.
    COLLECT it_id_sum.
    CLEAR : it_id_temp, it_id_sum.
  ENDLOOP.
ENDFORM.                    " read_id_info_lp

*&---------------------------------------------------------------------*
*&      Form  read_gr_info_lp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_gr_info_lp.
*---
  CHECK NOT it_id_temp[] IS INITIAL.

  CLEAR : it_vbfa_temp, it_vbfa_temp[], it_vbfa_sum, it_vbfa_sum[].

  SELECT matnr
         plmin
         rfmng INTO CORRESPONDING FIELDS OF TABLE it_vbfa_temp
               FROM vbfa
                FOR ALL ENTRIES IN it_id_temp
              WHERE vbelv EQ it_id_temp-vbeln
                AND posnv EQ it_id_temp-posnr
                AND matnr EQ it_id_temp-matnr
                AND vbtyp_n EQ 'R'
                AND vbtyp_v EQ '7'.

  LOOP AT it_vbfa_temp.
    MOVE-CORRESPONDING it_vbfa_temp TO it_vbfa_sum.
    IF it_vbfa_temp-plmin EQ '-'.
      it_vbfa_temp-rfmng = it_vbfa_temp-rfmng * - 1.
    ENDIF.
    COLLECT it_vbfa_sum.
    CLEAR : it_vbfa_temp, it_vbfa_sum.
  ENDLOOP.
ENDFORM.                    " read_gr_info_lp

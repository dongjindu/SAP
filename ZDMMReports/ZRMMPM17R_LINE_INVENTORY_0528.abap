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

REPORT  zrmmpm16_line_inventory.

************************************************************************
* Data
************************************************************************
TYPE-POOLS : slis.

TABLES : mara,       "General Material Data
         marc,       "Plant Data for Material
         mard,       "Storage Location Data for Material
         ekpo,       "Purchasing Document Item
         ztmm_mast,  "Supply to Line Master Table
         stpo,       "BOM item
         lqua,       "Quants
         eket,       "Scheduling Agreement Schedule Lines
         ztmm_nstl,  "Non Supply to Line
         ekbe,       "History per Purchasing Document
         vbfa.       "Sales Document Flow


DATA: BEGIN OF it_tab OCCURS 10,
       matnr  LIKE eban-matnr,     " Material number
       maktx  LIKE makt-maktx,     " Material description
       bismt  LIKE mara-bismt,     " Old material number(ALC code)
       werks  LIKE mard-werks,     " Plant
       lgort  LIKE mard-lgort,      "Storage location
       lgbzo  LIKE ekpo-lgbzo, " group gate(Automotive) Unloading Point
       feedr  LIKE ztmm_mast-feedr, " Feeder
       zline  LIKE ztmm_mast-zline, " Line
       works  LIKE ztmm_mast-works, " WorkStation
       rh_lh  LIKE ztmm_mast-rh_lh, " RH/LH

       menge  LIKE stpo-menge,   " Component quantity - Usage
       meins  LIKE stpo-meins,   " unit
       profl  LIKE mara-profl,   " surce (LP/KD)
       bstmi  LIKE marc-bstmi,   " Qty/con
       plan     LIKE mard-labst,   " plan
       diff     LIKE mard-labst,   " differce
       labst    LIKE mard-labst,   " inventory
       gesme    LIKE lqua-gesme,   " line stock
       wstok    LIKE lqua-gesme,   " Warehouse stock
       ccbin    LIKE lqua-gesme,   " cc-bin stock
       ccrack   LIKE lqua-gesme,   " cc-rack stock
       cystok   LIKE lqua-gesme,   " cy stock
       notyetgr LIKE lqua-gesme,   " Not Yet GR stock
*       mobile LIKE lqua-gesme,   " mobile stock
       arriv    LIKE lqua-gesme,   " Arrival
       shipm    LIKE lqua-gesme,   " Shipment
       duin     LIKE eket-menge, " Due in
       dispo    LIKE marc-dispo, " mrp controler
      END OF it_tab.

*For Inbound Delivery LP
DATA: BEGIN OF it_lp OCCURS 10,
       vbeln LIKE likp-vbeln,   "Inbound Delivery
       posnr LIKE lips-posnr,   "Delivery item
*       zfetd LIKE ztbl-zfetd,   "E.T.D.  "Added by Hakchin
*       zfeta LIKE ztbl-zfeta,   "E.T.A.  "Added by Hakchin
       tduhr LIKE likp-tduhr,
       lfuhr LIKE likp-lfuhr,
       traid LIKE likp-traid,   "KD Container Number
*       lgtor LIKE likp-lgtor,   "Door for warehouse number
       lgbzo LIKE lips-lgbzo,
       duin  LIKE eket-menge,
       meins LIKE mara-meins,
       matnr LIKE lips-matnr,
      END OF it_lp.

*For Inbound Delivery KD
DATA: BEGIN OF wa_kd,
       vgbel LIKE lips-vgbel,   "PO No
       vgpos LIKE lips-vgpos,
              "PO Item no  (Length is different from EBELP)
       traid LIKE likp-traid,   "Container number
*       lichn LIKE lips-lichn,   "Case number
       kdmat LIKE lips-kdmat,   " Case Number
       vrkme LIKE lips-vrkme,   "Unit
       lfimg LIKE lips-lfimg,   "Actual quantity delivered
*       zfetd like ztbl-zfetd,   "ETD
*       zfeta like ztbl-zfeta,   "ETA
       tduhr LIKE likp-tduhr,
       lfuhr LIKE likp-lfuhr,
*       ?
*       ?
*       ?
      END OF wa_kd.

DATA: it_kd LIKE TABLE OF wa_kd WITH HEADER LINE.

* Storage Type Stock
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

*--- ZTMM_NSTL
DATA : BEGIN OF it_ztmm_nstl OCCURS 0.
        INCLUDE STRUCTURE ztmm_nstl.
DATA : END OF it_ztmm_nstl.

*--- STPO
DATA : BEGIN OF it_stpo OCCURS 0,
         idnrk LIKE stpo-idnrk,
         menge LIKE stpo-menge,
       END OF it_stpo.

*---
DATA : w_layout TYPE slis_layout_alv.

*--- Storage Type
RANGES : r_lgtyp  FOR lqua-lgtyp,     " Condition Summary
         r_lgtyp1 FOR lqua-lgtyp,     " Line Stock
         r_lgtyp2 FOR lqua-lgtyp,     " W/H Stock
         r_lgtyp3 FOR lqua-lgtyp,     " CC-Bin Stock
         r_lgtyp4 FOR lqua-lgtyp,     " CC-Rack Stock
         r_lgtyp5 FOR lqua-lgtyp.     " CY Stock

RANGES : r_werks FOR t001w-werks.

***********
DATA : w_fieldcat    TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat    TYPE slis_t_event        WITH HEADER LINE,
       w_selfield    TYPE slis_selfield,
       w_sortcat     TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       w_col_pos     TYPE i,
       w_program     LIKE sy-repid,  "Name of the calling program
       w_top_of_page TYPE slis_t_listheader,
       w_line        TYPE slis_listheader.



************************************************************************
* macro
************************************************************************
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
*  &1 = &1 + 1.
*  w_fieldcat-col_pos       = &1.
*  w_fieldcat-fieldname     = &2.
*  w_fieldcat-ref_fieldname = &3.
*  w_fieldcat-key           = &4.
*  w_fieldcat-qfieldname    = &5.
*  w_fieldcat-cfieldname    = &6.
*  w_fieldcat-seltext_l     = &7.
*  w_fieldcat-seltext_m     = &7.
*  w_fieldcat-seltext_s     = &7.
*  w_fieldcat-outputlen     = &8.
**  w_fieldcat-no_out        = &9.
*  w_fieldcat-datatype      = &9.
*  append w_fieldcat.
*  clear : w_fieldcat.
END-OF-DEFINITION.

DEFINE append_storage_type.
  clear : &2.
  move : 'I'     to &2-sign,
         'EQ'    to &2-option,
         &1      to &2-low.
  append &2.
END-OF-DEFINITION.

************************************************************************
* SELECTION SCREEN
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
PARAMETERS     : p_werks LIKE mard-werks OBLIGATORY
                                         DEFAULT 'P001'.  " plant

SELECT-OPTIONS : s_matnr FOR mard-matnr.        " Material

SELECT-OPTIONS : s_lgort FOR mard-lgort,        " Storage Location
                 s_lgtyp FOR lqua-lgtyp MATCHCODE OBJECT zsh_mm_type,
                                                " storage type
                 s_feedr FOR ztmm_mast-feedr,   " feeder
                 s_zline FOR ztmm_mast-zline,   " line
                 s_works FOR ztmm_mast-works,   " Workstation.
                 s_profl FOR mara-profl.        " Source

PARAMETERS     : p_plan TYPE btcstime DEFAULT sy-uzeit." Plan
PARAMETERS     : p_inven(1) TYPE n  DEFAULT '9'." Inventory Type
SELECTION-SCREEN END OF BLOCK block1.


SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-002
                                       NO INTERVALS.
SELECTION-SCREEN COMMENT /6(20) text-t01.
SELECTION-SCREEN COMMENT /6(20) text-t02.
SELECTION-SCREEN COMMENT /6(20) text-t03.
SELECTION-SCREEN COMMENT /6(20) text-t04.
SELECTION-SCREEN COMMENT /6(20) text-t05.
SELECTION-SCREEN COMMENT /6(20) text-t06.
SELECTION-SCREEN COMMENT /6(20) text-t07.
SELECTION-SCREEN COMMENT /6(20) text-t08.
SELECTION-SCREEN COMMENT /6(20) text-t09.

SELECTION-SCREEN END OF BLOCK block2.

************************************************************************
* INITIALIZATION
************************************************************************
INITIALIZATION.
  PERFORM init.

************************************************************************
* AT SELECTION-SCREEN
************************************************************************

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_inven.
  PERFORM f4_help.

************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.

  PERFORM set_plant.
  PERFORM data_selection.
  PERFORM data_calculation.

  PERFORM list_write.





*&---------------------------------------------------------------------*
*&      Form  data_selection
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
* data selection mard,mara,ztmm_mast, etc
*----------------------------------------------------------------------*
FORM data_selection.
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
*** (Begin) Added by Hakchin 20031021
          ebeln LIKE mdbs-ebeln,  "PO no
          ebelp LIKE mdbs-ebelp,  "PO Item no
          etenr LIKE mdbs-etenr,  "Delivery schedule line counter
          eindt LIKE mdbs-eindt,  "Item delivery date
          etfz1 LIKE mdbs-etfz1,     " ekpo-etfz1,
           "Date Quantity for LP Open Item Selection
*** (End) Added by Hakchin 20031021

*          matnr LIKE mdbs-matnr,
*          werks LIKE mdbs-werks,
*          lgort LIKE mdbs-lgort,
          menge LIKE mdbs-menge,  "Scheduled quantity
          wemng LIKE mdbs-wemng,  "Quantity of goods received
         END OF ls_open.

  DATA: lt_open LIKE TABLE OF ls_open.


* Get material, plant, storage unit from the table MARD.
* (MARD : Storage Location Data for Material)
  SELECT d~matnr t~maktx
         a~bismt a~profl
         c~bstmi a~meins
         d~labst c~dispo
         d~werks d~lgort
                 INTO CORRESPONDING FIELDS OF TABLE it_tab
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
                            WHERE d~werks IN r_werks
*                                  d~werks =  p_werks    " Plant
                              AND d~matnr IN s_matnr    " Material
                              AND d~lgort IN s_lgort    " S.Loc
                              AND d~lgort NE '9999'
                              AND a~profl IN s_profl.   " Source
  " K : Knock Down Parts, V : Local Parts
  " There is a profl in a Basic View2 in Material Master(/nMM03)

*--- Group gate read
  SELECT p~matnr p~werks
         p~lgort p~lgbzo
                 INTO CORRESPONDING FIELDS OF TABLE it_po
                 FROM ekko AS k INNER JOIN ekpo AS p
                   ON k~mandt EQ p~mandt
                  AND k~ebeln EQ p~ebeln
                      FOR ALL ENTRIES IN it_tab
                WHERE k~bstyp EQ 'L'     " Scheduling Agreement
                  AND p~matnr EQ it_tab-matnr.


*--- Supply to Line Master Table read
* (Feeder & Line & W/S & RH/LH)
  SELECT matnr feedr
         zline works
         rh_lh
               INTO TABLE it_mast
               FROM ztmm_mast
                    FOR ALL ENTRIES IN it_tab
              WHERE matnr EQ it_tab-matnr
                AND werks EQ it_tab-werks.

*--- get ZTMM_NSTL
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ztmm_nstl
           FROM ztmm_nstl
            FOR ALL ENTRIES IN it_tab
          WHERE datum EQ sy-datum
            AND matnr EQ it_tab-matnr.

**--- get STPO
*  SELECT idnrk
*         menge INTO CORRESPONDING FIELDS OF TABLE it_stpo
*               FROM stpo
*                FOR ALL ENTRIES IN it_tab
*              WHERE idnrk EQ it_tab-matnr.


*--- storage type setting
  PERFORM storage_type_setting.

*--- get stock in storage type
  PERFORM get_storage_type_stock.


*--- line stock
  LOOP AT it_tab.

*Get DUIN
    IF it_tab-profl EQ 'K'.     "Knock Down Parts

      SELECT SUM( menge ) AS menge
             SUM( wemng ) AS wemng
                          INTO CORRESPONDING FIELDS OF ls_open
                          FROM mdbs
                         WHERE matnr EQ it_tab-matnr
                           AND werks EQ it_tab-werks
*                           AND lgort = it_tab-lgort
                           AND lgort NE '9999'
                           AND loekz = ' '
                           AND elikz = ' '.
*                         GROUP by matnr werks.
      IF sy-subrc EQ 0.
        it_tab-duin = ls_open-menge - ls_open-wemng. "Open Qantity
        CLEAR ls_open.
      ENDIF.

    ELSEIF it_tab-profl EQ 'V'. "Local Parts
* Added by Hakchin
      SELECT ebeln ebelp
             etenr eindt
             etfz1 menge
             wemng
                   INTO CORRESPONDING FIELDS OF TABLE lt_open
                   FROM mdbs  "Material View of Order Item/Schedule Line
                  WHERE matnr EQ it_tab-matnr
                    AND werks EQ it_tab-werks
*                    AND lgort EQ it_tab-lgort
                    AND lgort NE '9999'
                    AND loekz = ' '
                    AND elikz = ' '.
*                  GROUP by matnr werks.
      IF sy-subrc EQ 0.
        DATA: criterior_date LIKE sy-datum.
        LOOP AT lt_open INTO ls_open.
** Firm Zone Check - Get Date Quantity for LP Open Item Selection
*          PERFORM get_etfz1 USING    ls_open-ebeln  "PO
*                                     ls_open-ebelp  "PO Item
*                            CHANGING ls_open-etfz1. "Date Quantity

          CLEAR : criterior_date.
          criterior_date = sy-datum + ls_open-etfz1.
          IF ls_open-eindt LE criterior_date.
            it_tab-duin = it_tab-duin +
                   ( ls_open-menge - ls_open-wemng ). "Open Qantity
          ENDIF.
        ENDLOOP.
        CLEAR : ls_open.
      ENDIF.
    ENDIF.

*****
*--- get Group Gate
    READ TABLE it_po WITH KEY werks = it_tab-werks  "Plant
                              matnr = it_tab-matnr. "Material
    IF sy-subrc EQ 0.
      it_tab-lgbzo = it_po-lgbzo.   "Group gate
    ENDIF.

*--- get Supply to Line Master Info.
    READ TABLE it_mast WITH KEY matnr = it_tab-matnr.
    IF sy-subrc EQ 0.
      MOVE : it_mast-feedr TO it_tab-feedr,
             it_mast-zline TO it_tab-zline,
             it_mast-works TO it_tab-works,
             it_mast-rh_lh TO it_tab-rh_lh.
    ENDIF.

*--- get usage
    CLEAR : stpo.
    SELECT SINGLE menge INTO it_tab-menge
                        FROM stpo
                       WHERE idnrk EQ it_tab-matnr.

*    CLEAR : it_stpo.
*    READ TABLE it_stpo WITH KEY idnrk = it_tab-matnr.
*    MOVE : it_stpo-menge TO it_tab-menge.

    IF it_tab-lgort EQ 'P400'.   "Storage location

*      PERFORM not_used_logic.

      CLEAR : it_stype_stock_sum.
      READ TABLE it_stype_stock_sum WITH KEY werks = it_tab-werks
*                                             werks = p_werks
                                             lgort = it_tab-lgort
                                             matnr = it_tab-matnr.

      IF sy-subrc EQ 0.
        MOVE : it_stype_stock_sum-gesme1 TO it_tab-gesme,
               it_stype_stock_sum-gesme2 TO it_tab-wstok,
               it_stype_stock_sum-gesme3 TO it_tab-ccbin,
               it_stype_stock_sum-gesme4 TO it_tab-ccrack,
               it_stype_stock_sum-gesme5 TO it_tab-cystok.
      ENDIF.

*--- get Import Information, when Inventory Type equal than '7'.
      IF p_inven GE '7'.
      ENDIF.
*---

** Stock: Not Yet GR
*      SELECT SUM( gesme ) INTO it_tab-notyetgr  "Not Yet GR
*        FROM lqua
*        WHERE matnr = it_tab-matnr AND
*              werks = p_werks      AND
*              lgort IN s_lgort     AND "Storage location
*              lgtyp IN ('100').        "Storage Type


      PERFORM line_inventory_cal.

    ENDIF.

*--- get Plan Quantity
    PERFORM get_plan_quantity.

*--- get Diffence (Inventory - Plan)
    it_tab-diff = it_tab-labst - it_tab-plan.

*if p_int

    MODIFY it_tab.

  ENDLOOP.

ENDFORM.                    " data_selection
*&---------------------------------------------------------------------*
*&      Form  data_calculation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_calculation.

ENDFORM.                    " data_calculation
*&---------------------------------------------------------------------*
*&      Form  f4_help
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_help.

  DATA : BEGIN OF it_dynpfields OCCURS 0.
          INCLUDE STRUCTURE dynpread.
  DATA : END OF it_dynpfields.

  DATA : BEGIN OF value_tab OCCURS 0,
           inven TYPE aankz,
* Changed by Hakchin(20031016)(Begin)
*           name LIKE lfa1-name1,
           name TYPE zdesc50,   "Description - Size50
* Changed by Hakchin(20031016)(End)
         END OF value_tab.

  CLEAR : it_dynpfields, value_tab.
  REFRESH : it_dynpfields, value_tab.


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

* Changed by Hakchin(20031016)(Begin)
  value_tab-inven = '8'.
  value_tab-name  = '1) + 2) + 3) + 4) + 5) + 6) + 7) + 8)'.
  APPEND value_tab.

  value_tab-inven = '9'.
  value_tab-name  = '1) + 2) + 3) + 4) + 5) + 6) + 7) + 8) + 9)'.
  APPEND value_tab.
* Changed by Hakchin(20031016)(End)

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            retfield        = 'INVEN'
            dynpprog =
               'ZRMMPM16R_LINE_INVENTORY' "<- Report Name
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
*&      Form  line_inventory_cal
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM line_inventory_cal.
  CLEAR : it_tab-labst.

  CASE p_inven.
    WHEN '1'.
      it_tab-labst = it_tab-gesme.
    WHEN '2'.
      it_tab-labst = it_tab-gesme + it_tab-wstok .
    WHEN '3'.
      it_tab-labst = it_tab-gesme + it_tab-wstok + it_tab-ccbin.
    WHEN '4'.
      it_tab-labst = it_tab-gesme + it_tab-wstok + it_tab-ccbin +
                     it_tab-ccrack.
    WHEN '5'.
      it_tab-labst = it_tab-gesme  + it_tab-wstok + it_tab-ccbin +
                     it_tab-ccrack + it_tab-cystok.
    WHEN '6'.   "Not Yet GR
      it_tab-labst = it_tab-gesme  + it_tab-wstok   + it_tab-ccbin +
                     it_tab-ccrack + it_tab-cystok + it_tab-notyetgr.
    WHEN '9'.   "Due-In
      it_tab-labst = it_tab-gesme  + it_tab-wstok   + it_tab-ccbin +
                     it_tab-ccrack + it_tab-cystok + it_tab-notyetgr
                      + it_tab-duin.
  ENDCASE.
ENDFORM.                    " line_inventory_cal
*&---------------------------------------------------------------------*
*&      Form  list_write
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM list_write.
*---
  DATA: l_count TYPE i.

  CLEAR l_count.

  DESCRIBE TABLE it_tab LINES l_count .

  IF l_count = 0.
    MESSAGE s420(me).  " with text-002.
    EXIT.
  ENDIF.

  PERFORM build_fieldcat.
  PERFORM build_event.
*  PERFORM build_sort.
  PERFORM comment_build USING  w_top_of_page[].
  PERFORM alv_function.

ENDFORM.                    " list_write
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
    w_col_pos 'LGBZO' 10 'Group Gate'     'CHAR' ' '  ''      '' '',
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
    w_col_pos 'NOTYETGR' 12 'Not Yet GR'   'QUAN' ' '  'MEINS' '' '',
*    w_col_pos 'NOTYETGR' 12 'Not Yet GR'   'QUAN' ' '  'MEINS' '' '',
    w_col_pos 'ARRIV' 12 'Arrival'        'QUAN' ' '  'MEINS' '' '',
    w_col_pos 'SHIPM' 12 'Shipment'       'QUAN' ' '  'MEINS' '' '',
    w_col_pos 'DUIN'  12 'Due-In'         'QUAN' ' '  'MEINS' '' '',
    w_col_pos 'DISPO' 12 'Manager'        'CHAR' ' '  ''      '' ''.
ENDFORM.                    " build_fieldcat
*&---------------------------------------------------------------------*
*&      Form  build_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_event.
  w_eventcat-name = 'TOP_OF_PAGE'.
  w_eventcat-form = 'TOP_OF_PAGE'.

  APPEND w_eventcat.
ENDFORM.                    " build_event
*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_TOP_OF_PAGE[]  text
*----------------------------------------------------------------------*
FORM comment_build USING lt_top_of_page TYPE slis_t_listheader.
  DATA: ls_line TYPE slis_listheader,
        l_manager(50),
        l_date(50),
        l_list(50),
        l_dsnam LIKE t024d-dsnam,
        l_h_dsnam LIKE t024d-dsnam,
        l_ldate(10),
        l_hdate(10),
        l_name1 LIKE t001w-name1.

*
* Listen?erschrift: Typ H
  CLEAR ls_line.
  ls_line-typ  = 'H'.
* LS_LINE-KEY:  not used for this type
  ls_line-info = text-100.   "Line inventory
  APPEND ls_line TO lt_top_of_page.

* Kopfinfo: Typ S



* Plant
  ls_line-key  = 'Plant: '.
  ls_line-typ  = 'S'.
  SELECT SINGLE name1 INTO l_name1 FROM t001w
         WHERE werks = p_werks.

  WRITE : p_werks   TO l_list(6),
          l_name1    TO l_list+7(30).
  ls_line-info = l_list.
  APPEND ls_line TO lt_top_of_page.

* Material
  ls_line-key  = 'Material: '.
  READ TABLE s_matnr INDEX 1.
  IF sy-subrc EQ 0.
    ls_line-typ  = 'S'.
    CONCATENATE 'FROM: ' s_matnr-low '  TO: ' s_matnr-high INTO l_list.
    ls_line-info = l_list.
    APPEND ls_line TO lt_top_of_page.
  ENDIF.

* Storage Location
  ls_line-key  = 'Storage Location: '.
  READ TABLE s_lgort INDEX 1.
  IF sy-subrc EQ 0.
    ls_line-typ  = 'S'.

    CONCATENATE 'FROM: ' s_lgort-low '  TO: ' s_lgort-high INTO l_list.
    ls_line-info = l_list.
    APPEND ls_line TO lt_top_of_page.
  ENDIF.

* storage type
  ls_line-key  = 'Storage type: '.
  READ TABLE s_lgtyp INDEX 1.
  IF sy-subrc EQ 0.
    ls_line-typ  = 'S'.

    CONCATENATE 'FROM: ' s_lgtyp-low '  TO: ' s_lgtyp-high INTO l_list.
    ls_line-info = l_list.
    APPEND ls_line TO lt_top_of_page.
  ENDIF.

* storage feeder
  ls_line-key  = 'Feeder   : '.
  READ TABLE s_feedr INDEX 1.
  IF sy-subrc EQ 0.
    ls_line-typ  = 'S'.

    CONCATENATE 'FROM: ' s_feedr-low '  TO: ' s_feedr-high INTO l_list.
    ls_line-info = l_list.
    APPEND ls_line TO lt_top_of_page.
  ENDIF.

* line
  ls_line-key  = 'Line     : '.
  READ TABLE s_zline INDEX 1.
  IF sy-subrc EQ 0.
    ls_line-typ  = 'S'.

    CONCATENATE 'FROM: ' s_zline-low '  TO: ' s_zline-high INTO l_list.
    ls_line-info = l_list.
    APPEND ls_line TO lt_top_of_page.
  ENDIF.

* workstation
  ls_line-key  = 'W/S       : '.
  READ TABLE s_works INDEX 1.
  IF sy-subrc EQ 0.
    ls_line-typ  = 'S'.

    CONCATENATE 'FROM: ' s_works-low '  TO: ' s_works-high INTO l_list.
    ls_line-info = l_list.
    APPEND ls_line TO lt_top_of_page.
  ENDIF.

* source
  ls_line-key  = 'Source    : '.
  READ TABLE s_profl INDEX 1.
  IF sy-subrc EQ 0.
    ls_line-typ  = 'S'.

    CONCATENATE 'FROM: ' s_profl-low '  TO: ' s_profl-high INTO l_list.
    ls_line-info = l_list.
    APPEND ls_line TO lt_top_of_page.
  ENDIF.

* Plan
  ls_line-key = 'Plan'.
  IF NOT p_plan IS INITIAL.
    ls_line-typ = 'S'.
    ls_line-info = p_plan.
    APPEND ls_line TO lt_top_of_page.
  ENDIF.

* inventory type
  ls_line-key  = 'Inventory Type: '.
  IF NOT p_inven IS INITIAL.
    ls_line-typ  = 'S'.

    ls_line-info = p_inven .
    APPEND ls_line TO lt_top_of_page.
  ENDIF.

  CLEAR ls_line.
*
ENDFORM.                    " comment_build
*&---------------------------------------------------------------------*
*&      Form  alv_function
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_function.

  DATA:  l_print_p TYPE slis_print_alv.  " print setting
  CLEAR: w_program.
  MOVE:  sy-repid TO w_program.   "Name of the calling program

*** print paramter   ****************************************
  l_print_p-no_coverpage = 'X'.
  l_print_p-no_print_listinfos = 'X'.
  l_print_p-no_change_print_params = 'X'.
  l_print_p-no_print_selinfos = 'X'.
*************************************************************

  w_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_bypassing_buffer      = 'X'
            i_callback_program      = w_program
            i_callback_user_command = 'USER_COMMAND'
            is_layout               = w_layout
            it_fieldcat             = w_fieldcat[]
            it_sort                 = w_sortcat[]
            i_save                  = 'A'
            it_events               = w_eventcat[]
            is_print                = l_print_p
       TABLES
            t_outtab                = it_tab
       EXCEPTIONS
            program_error           = 1
            OTHERS                  = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " alv_function

*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM top_of_page.
*
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
*           i_logo             = 'Z_HYUNDAI_LOGO'
*            i_logo             = 'ENJOYSAP_LOGO'
            it_list_commentary = w_top_of_page.
ENDFORM.
*&---------------------------------------------------------------------*
*&      user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM user_command USING ucomm LIKE sy-ucomm
                       selfield TYPE slis_selfield.
  CASE ucomm.
    WHEN 'REGIST'.
      CALL FUNCTION 'REUSE_ALV_LIST_LAYOUT_INFO_SET'
        EXPORTING
*         IS_LAYOUT            =
          it_fieldcat          = w_fieldcat[]
*         IT_SORT              =
*         IT_FILTER            =
*         IS_LIST_SCROLL       =
                .
    WHEN '&IC1'. " Double Click

      PERFORM detail_list USING selfield. "Executed with double click
*      CALL TRANSACTION 'ME21N' .
*       CALL DIALOG 'ME_CREATE_PO'

  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  detail_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SELFIELD  text
*----------------------------------------------------------------------*
FORM detail_list USING  selfield TYPE slis_selfield.
*---
  DATA : wa_it_kd LIKE it_kd.

  READ TABLE it_tab INDEX selfield-tabindex.

  CHECK sy-subrc EQ 0.

  IF it_tab-profl EQ 'K'.   "Knock Down Parts

*    select lips~vgbel lips~vgpos
*           likp~traid lips~lichn
*           lips~vrkme lips~lfimg
*           ztbl~zfetd ztbl~zfeta
*                into corresponding fields of table it_kd
*                from likp inner join lips
*                  on lips~mandt eq likp~mandt
*                 and lips~vbeln eq likp~vbeln
**                 and lips~matnr eq it_tab-matnr
*                     left outer join ztbl
**                     INNER JOIN ztbl
*                        on ztbl~mandt eq likp~mandt
*                       and ztbl~zfblno eq likp~bolnr   "BL no
*               where likp~lfart eq 'EL' "Del. type:Inbound Delivery
*                 and matnr eq it_tab-matnr
*                 and lips~werks eq it_tab-werks
*                 and lgort eq it_tab-lgort.

    SELECT a~vgbel a~vgpos
           b~traid a~kdmat     " a~lichn
           a~vrkme a~lfimg
           b~tduhr b~lfuhr
                   INTO CORRESPONDING FIELDS OF TABLE it_kd
                   FROM lips AS a INNER JOIN likp AS b
                     ON a~mandt EQ b~mandt
                    AND a~vbeln EQ b~vbeln
                  WHERE b~lfart EQ 'EL'
                    AND matnr EQ it_tab-matnr
                    AND a~werks EQ it_tab-werks
                    AND lgort EQ it_tab-lgort.

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
  ELSEIF it_tab-profl EQ 'V'.  "Local Parts

*    SELECT likp~vbeln lips~posnr
*           ztbl~zfetd ztbl~zfeta
*           likp~traid likp~lgtor
*                INTO TABLE it_lp
*                FROM likp INNER JOIN lips
*                  ON lips~mandt EQ likp~mandt
*                 AND lips~vbeln = likp~vbeln
**                 AND lips~matnr = it_tab-matnr
*                     LEFT OUTER JOIN ztbl
**                     INNER JOIN ztbl
*                        ON ztbl~mandt EQ likp~mandt
*                       AND ztbl~zfblno = likp~bolnr   "BL no
*               WHERE likp~lfart = 'EL' "Delivery type: Inbound Delivery
*                 AND matnr EQ it_tab-matnr
*                 AND lips~werks EQ it_tab-werks
*                 AND lgort EQ it_tab-lgort.

    SELECT b~vbeln a~posnr
           b~tduhr b~lfuhr
           b~traid b~lgbzo     " b~lgtor
                   INTO TABLE it_lp
                   FROM lips AS a INNER JOIN likp AS b
                     ON a~mandt EQ b~mandt
                    AND a~vbeln EQ b~vbeln
                  WHERE b~lfart EQ 'EL'
                    AND matnr EQ it_tab-matnr
                    AND a~werks EQ it_tab-werks
                    AND lgort EQ it_tab-lgort.

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
    it_lp-duin = it_tab-duin.
    it_lp-meins = it_tab-meins.
    it_lp-matnr = it_tab-matnr.
*    MODIFY TABLE it_lp TRANSPORTING duin meins matnr werks lgort.
    MODIFY it_lp TRANSPORTING duin meins matnr
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
  IF it_tab-profl = 'K'.   "Knock Down Parts
    PERFORM build_fieldcat_detail_kd.
    PERFORM list_write_detail_kd.
  ELSEIF it_tab-profl = 'V'.  "Local Parts
    PERFORM build_fieldcat_detail_lp.
    PERFORM list_write_detail_lp.
  ENDIF.
ENDFORM.                    " detail_list_write
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat_detail
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
*    w_col_pos 'ZFETD'  'ZFETD'          '' ''    ''
*    'ETD'                             '10' '',
*    w_col_pos 'ZFETA'  'ZFETA'          '' ''    ''
*    'ETA'                             '10' '',
    w_col_pos 'TDUHR' 08 'ETD'            'TIMS' ' '  ''      '' '',
    w_col_pos 'LFUHR' 08 'ETA'            'TIMS' ' '  ''      '' '',
    w_col_pos 'TRAID' 20 'Trailer No'     'CHAR' ' '  ''      '' '',
*    w_col_pos 'LGTOR' 03 'Gate Number'    'CHAR' ' '  ''      '' '',
    w_col_pos 'LGBZO' 10 'Gate Number'    'CHAR' ' '  ''      '' '',
    w_col_pos 'MATNR' 18 'Material No'    'CHAR' ' '  ''      '' '',
    w_col_pos 'MEINS' 03 'UoM'            'UNIT' ' '  ''      '' '',
    w_col_pos 'DUIN'  12 'Due-In'         'QUAN' ' '  'MEINS' '' ''.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  list_write_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM list_write_detail_lp.
  DATA:   l_print_p TYPE slis_print_alv.  " print setting

  CLEAR : w_program.

  MOVE : sy-repid TO w_program.

*** print paramter   ****************************************
  l_print_p-no_coverpage = 'X'.
  l_print_p-no_print_listinfos = 'X'.
  l_print_p-no_change_print_params = 'X'.
  l_print_p-no_print_selinfos = 'X'.
*************************************************************

  MOVE : 'X' TO w_layout-colwidth_optimize.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_bypassing_buffer    = 'X'
            i_callback_program    = w_program
            i_grid_title          = text-004  "Sub list(LP)
            is_layout             = w_layout
            it_fieldcat           = w_fieldcat[]
            i_save                = 'A'
*            it_events             = w_eventcat[]
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
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " list_write_detail
*&---------------------------------------------------------------------*
*&      Form  get_etfz1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_OPEN_EBELN  text
*      -->P_LS_OPEN_EBELP  text
*      <--P_LS_OPEN_ETFZ1  text
*----------------------------------------------------------------------*
FORM get_etfz1 USING    value(im_ebeln)
                        value(im_ebelp)
               CHANGING value(ex_etfz1) LIKE ekpo-etfz1.
  CLEAR : ex_etfz1.

  SELECT SINGLE etfz1     " Firm zone (go-ahead for production)
                     INTO ex_etfz1
                     FROM ekpo
                    WHERE ebeln EQ im_ebeln
                      AND ebelp EQ im_ebelp.
ENDFORM.
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
*    w_col_pos 'LICHN'  'LICHN'        '' ''      ''
    w_col_pos 'KDMAT' 20 'Case Number'    'CHAR' ' '  ''      '' '',
    w_col_pos 'VRKME' 03 'UoM'            'UNIT' ' '  ''      '' '',
    w_col_pos 'LFIMG' 12 'Quantity'       'QUAN' ' '  'VRKME' '' '',
*    w_col_pos 'ZFETD'  'ZFETD'          '' ''      ''
*    'ETD'                        '10' '',
*    w_col_pos 'ZFETA'  'ZFETA'          '' ''      ''
*    'ETA'                        '10' ''.
    w_col_pos 'TDUHR' 08 'ETD'            'TIMS' ' '  ''      '' '',
    w_col_pos 'LFUHR' 08 'ETA'            'TIMS' ' '  ''      '' ''.
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
  DATA:   l_print_p TYPE slis_print_alv.  " print setting

  CLEAR : w_program.

  MOVE : sy-repid TO w_program.

*** print paramter   ****************************************
  l_print_p-no_coverpage = 'X'.
  l_print_p-no_print_listinfos = 'X'.
  l_print_p-no_change_print_params = 'X'.
  l_print_p-no_print_selinfos = 'X'.
*************************************************************

  MOVE : 'X' TO w_layout-colwidth_optimize.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_bypassing_buffer    = 'X'
            i_callback_program    = w_program
            i_grid_title          = text-005  "Sub list(KD)
            is_layout             = w_layout
            it_fieldcat           = w_fieldcat[]
            i_save                = 'A'
*            it_events             = w_eventcat[]
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

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " list_write_detail_kd

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

  IF p_inven GE 2.
    APPEND LINES OF r_lgtyp2 TO r_lgtyp.
  ENDIF.

  IF p_inven GE 3.
    APPEND LINES OF r_lgtyp3 TO r_lgtyp.
  ENDIF.

  IF p_inven GE 4.
    APPEND LINES OF r_lgtyp4 TO r_lgtyp.
  ENDIF.

  IF p_inven GE 5.
    APPEND LINES OF r_lgtyp5 TO r_lgtyp.
  ENDIF.
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

*--- read table
  SELECT werks lgort
         matnr lgtyp
         gesme
                     INTO CORRESPONDING FIELDS OF TABLE it_stype_stock
                     FROM lqua
                      FOR ALL ENTRIES IN it_tab
                    WHERE matnr EQ it_tab-matnr
                      AND werks IN r_werks
*                          werks EQ p_werks
                      AND lgort IN s_lgort
                      AND lgort NE '9999'
                      AND lgtyp IN r_lgtyp.
*                      AND ( lgtyp IN r_lgtyp1 OR lgtyp IN r_lgtyp2
*                        OR  lgtyp IN r_lgtyp3 OR lgtyp IN r_lgtyp4
*                        OR  lgtyp IN r_lgtyp5 ).

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
*&      Form  not_used_logic
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM not_used_logic.
*---
  SELECT SUM( gesme ) INTO it_tab-gesme   "Line Stock
                      FROM lqua
                     WHERE matnr EQ it_tab-matnr
                       AND werks EQ p_werks
                       AND lgort IN s_lgort
                           " Only P400 exists in LQUA
                       AND lgtyp IN ('441','442','443',
                                     '444','445').  "Storage Type
*      " Inserted by Hakchin (20031020Mon)

  SELECT SUM( gesme ) INTO it_tab-wstok   "Warehouse stock
                      FROM lqua
                     WHERE matnr EQ it_tab-matnr
                       AND werks EQ p_werks
                       AND lgort IN s_lgort
                           " Only P400 exists in LQUA
*                           and lgtyp IN ('500','510','520').
                       AND lgtyp IN ('431','432','433','434','435',
                                     '436','437').  "Storage Type
  " Changed by Hakchin (20031020Mon)

  SELECT SUM( gesme ) INTO it_tab-ccbin "cc-bin stock
                      FROM lqua
                     WHERE matnr EQ it_tab-matnr
                       AND werks EQ p_werks
                       AND lgort IN s_lgort
                       AND lgtyp IN ('422').         "Storage Type

  SELECT SUM( gesme ) INTO it_tab-ccrack "cc-rack stock
                      FROM lqua
                     WHERE matnr EQ it_tab-matnr
                       AND werks EQ p_werks
                       AND lgort IN s_lgort
                       AND lgtyp IN ('421').        "Storage Type

  SELECT SUM( gesme ) INTO it_tab-cystok  "cy stock
                      FROM lqua
                     WHERE matnr EQ it_tab-matnr
                       AND werks EQ p_werks
                       AND lgort IN s_lgort
                       AND lgtyp IN ('411').        "Storage Type
ENDFORM.                    " not_used_logic

*&---------------------------------------------------------------------*
*&      Form  get_plan_quantity
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_plan_quantity.
**---
*  DATA : wa_ztmm_nstl LIKE ztmm_nstl,
**         l_field(2) TYPE n,
*         l_field TYPE i,
*         l_count(2) TYPE n.
*
*  FIELD-SYMBOLS : <f1> TYPE ANY.
*
**  CLEAR : ztmm_nstl.
**
**  SELECT SINGLE * INTO wa_ztmm_nstl
**                  FROM ztmm_nstl
**                 WHERE matnr EQ it_tab-matnr.
*
*  CLEAR : it_ztmm_nstl, wa_ztmm_nstl.
*  READ TABLE it_ztmm_nstl WITH KEY matnr = it_tab-matnr.
*
*  CHECK sy-subrc EQ 0.
*
*  MOVE : it_ztmm_nstl TO wa_ztmm_nstl.
*
*  l_field = p_plan(2) - 5 + 3.     " from l_field to time24
**  l_count = 32 - l_field + 1.
*
*  IF l_field LE 3.
*    l_field = l_field + 24.
*  ENDIF.
*
*  DO.
*    IF l_field EQ 28.
*      EXIT.
*    ELSE.
*      ASSIGN COMPONENT l_field OF STRUCTURE wa_ztmm_nstl TO <f1>.
*      it_tab-plan = it_tab-plan + <f1>.
*      l_field = l_field + 1.
*    ENDIF.
*  ENDDO.
ENDFORM.                    " get_plan_quantity

*&---------------------------------------------------------------------*
*&      Form  set_plant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_plant.
*---
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
ENDFORM.                    " set_plant

************************************************************************
* Program Name      : ZRMMPM21R_CONTAINER_CALL
* Created by        : Min-su Park
* Created on        : 2003.08.25.
* Pattern           :
* Description       : Container Call List
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 2003.10.16 Min-su Park  UD1K901849
*   Change History
*   1. KD Part Added.
*   2. Some code Changed.
*   3. Container call added


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
         eket.       "Scheduling Agreement Schedule Lines


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

       menge  LIKE stpo-menge,   " Component quantity
       meins  LIKE stpo-meins,   " unit
       profl  LIKE mara-profl,   " surce (LP/KD)
       bstmi  LIKE marc-bstmi,   " Qty/con
       plan(10)              ,   " plan
       diff(10)              ,   " differce
       labst    LIKE mard-labst,   " inventory
       gesme    LIKE lqua-gesme,   " line stock
       wstok    LIKE lqua-gesme,   " Warehouse stock
       ccbin    LIKE lqua-gesme,   " cc-bin stock
       ccrack   LIKE lqua-gesme,   " cc-rack stock
       cystok   LIKE lqua-gesme,   " cy stock
       notyetgr LIKE lqua-gesme,   " Not Yet GR stock
*       mobile LIKE lqua-gesme,   " mobile stock
       duin     LIKE eket-menge, " Due in
       dispo    LIKE marc-dispo, " mrp controler
*mspark add
       TRAID    LIKE LIKP-TRAID,    "Container
       ZFRETA LIKE ZTBL-ZFRETA,     "Arrival Date
       ERDAT    LIKE LIKP-ERDAT,
*mspark
     END OF it_tab.
*mspark add
*ALV Definition.
DATA:   WA_EVENTS      TYPE SLIS_T_EVENT,
        W_REPID LIKE SY-REPID                           ,
        WA_FIELDCAT TYPE slis_t_fieldcat_alv WITH HEADER LINE,
        W_FORMNAME_TOP_OF_PAGE TYPE SLIS_FORMNAME VALUE 'TOP_OF_PAGE1',
        WA_LIST_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
*Containers Picking Order
DATA: BEGIN OF IT_PICK OCCURS 0,
          ERDAT LIKE LIKP-ERDAT,
                 SEQ(03) TYPE N,
          TRAID LIKE LIKP-TRAID,      "CONT. NO.
          VGBEL LIKE LIPS-VGBEL,      "Order No.
        ZFSUMDT LIKE ZTIDSUS-ZFSUMDT, "C/CLEAR
          LFIMG LIKE LIPS-LFIMG,      "Material QTY.
          MEINS LIKE LIPS-MEINS,
         ZFRETA LIKE ZTBL-ZFRETA,     "Arrival Date
                     TOPPRT(03),      "TOP Priority
      END OF IT_PICK.
*mspark
*For Inbound Delivery LP
DATA: BEGIN OF it_lp OCCURS 10,
       vbeln LIKE likp-vbeln,   "Inbound Delivery
       posnr LIKE lips-posnr,   "Delivery item
       zfetd LIKE ztbl-zfetd,   "E.T.D.  "Added by Hakchin
       zfeta LIKE ztbl-zfeta,   "E.T.A.  "Added by Hakchin
       traid LIKE likp-traid,   "KD Container Number
       lgtor LIKE likp-lgtor,   "Door for warehouse number
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
       lichn LIKE lips-lichn,   "Case number
       vrkme LIKE lips-vrkme,   "Unit
       lfimg LIKE lips-lfimg,   "Actual quantity delivered
       zfetd LIKE ztbl-zfetd,   "ETD
       zfeta LIKE ztbl-zfeta,   "ETA
      END OF wa_kd.
DATA: it_kd LIKE TABLE OF wa_kd.

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
  w_fieldcat-col_pos       = &1.
  w_fieldcat-fieldname     = &2.
  w_fieldcat-ref_fieldname = &3.
  w_fieldcat-key           = &4.
  w_fieldcat-qfieldname    = &5.
  w_fieldcat-cfieldname    = &6.
  w_fieldcat-seltext_l     = &7.
  w_fieldcat-seltext_m     = &7.
  w_fieldcat-seltext_s     = &7.
  w_fieldcat-outputlen     = &8.
  w_fieldcat-no_out        = &9.
  append w_fieldcat.
  clear : w_fieldcat.
END-OF-DEFINITION.


************************************************************************
* SELECTION SCREEN
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
PARAMETERS     : p_werks LIKE mard-werks OBLIGATORY
                                         DEFAULT 'P001'.  " plant

SELECT-OPTIONS : s_matnr FOR mard-matnr.        " Material

SELECT-OPTIONS : s_lgort FOR mard-lgort,        " Storage Location
                 s_lgtyp FOR lqua-lgtyp,        " storage type
                 s_feedr FOR ztmm_mast-feedr,   " feeder
                 s_zline FOR ztmm_mast-zline,   " line
                 s_works FOR ztmm_mast-works,   " Workstation.
                 s_profl FOR mara-profl.        " Source

PARAMETERS     : p_inven(1) TYPE c  DEFAULT '9'."Inventory Type
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

************************************************************************
* AT SELECTION-SCREEN
************************************************************************

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_inven.
  PERFORM f4_help.

************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.

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
          etfz1 LIKE ekpo-etfz1,
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
* (MARD:Storage Location Data for Material)
  SELECT d~matnr t~maktx a~bismt a~profl c~bstmi a~meins d~labst
         c~dispo d~werks d~lgort
    INTO CORRESPONDING FIELDS OF TABLE it_tab
    FROM mard AS d
           INNER JOIN mara AS a
             ON d~matnr = a~matnr
           INNER JOIN makt AS t
             ON t~matnr = d~matnr AND
                t~spras = sy-langu
           INNER JOIN marc AS c
             ON d~matnr = c~matnr AND
                d~werks = c~werks
    WHERE d~werks =  p_werks AND   "Plant
          d~matnr IN s_matnr AND   "Material
          d~lgort IN s_lgort AND   "Storage Location
          a~profl IN s_profl.      "Source
  "K: Knock Down Parts, V: Local Parts
  "There is a profl in a Basic View2 in Material Master(/nMM03)

*--- Group gate read
  SELECT p~matnr p~werks p~lgort p~lgbzo
    INTO CORRESPONDING FIELDS OF TABLE it_po
    FROM ekko AS k
      INNER JOIN ekpo AS p
        ON k~ebeln = p~ebeln
    FOR ALL ENTRIES IN it_tab
    WHERE k~bstyp = 'L' AND  "Purchasing document category:LP type
          p~matnr = it_tab-matnr.


*--- Supply to Line Master Table read
* (Feeder|Line|W/S|RH/LH)
  SELECT matnr feedr zline works rh_lh
    INTO TABLE it_mast
    FROM ztmm_mast
    FOR ALL ENTRIES IN it_tab
    WHERE matnr = it_tab-matnr AND
          werks = it_tab-werks.

*--- line stock
  LOOP AT it_tab.

*Get DUIN
    IF it_tab-profl = 'K'. "Knock Down Parts
      SELECT SUM( menge ) AS menge SUM( wemng ) AS wemng
        INTO CORRESPONDING FIELDS OF ls_open
        FROM mdbs  "Material View of Order Item/Schedule Line
        WHERE matnr = it_tab-matnr AND   "Material
              werks = it_tab-werks AND   "Plant
*            lgort = it_tab-lgort AND
              loekz = ' '          AND   "Deletion Indicator
              elikz = ' '                ""Delivery completed" indicator
*      GROUP by matnr werks
             .
      IF sy-subrc EQ 0.
        it_tab-duin = ls_open-menge - ls_open-wemng. "Open Qantity
        CLEAR ls_open.
      ENDIF.

    ELSEIF it_tab-profl = 'V'. "Local Parts
* Added by Hakchin
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE lt_open
        FROM mdbs  "Material View of Order Item/Schedule Line
        WHERE matnr = it_tab-matnr AND   "Material
              werks = it_tab-werks AND   "Plant
*            lgort = it_tab-lgort AND    "Storage location
              loekz = ' '          AND   "Deletion Indicator
              elikz = ' '                ""Delivery completed" indicator
*      GROUP by matnr werks
             .
      IF sy-subrc EQ 0.
        DATA: criterior_date LIKE sy-datum.
        LOOP AT lt_open INTO ls_open.
* Get Date Quantity for LP Open Item Selection
          PERFORM get_etfz1 USING    ls_open-ebeln  "PO
                                     ls_open-ebelp  "PO Item
                            CHANGING ls_open-etfz1. "Date Quantity

          CLEAR: criterior_date.
          criterior_date = sy-datum + ls_open-etfz1.
          IF ls_open-eindt LE criterior_date.
            it_tab-duin = it_tab-duin +
                   ( ls_open-menge - ls_open-wemng ). "Open Qantity
          ENDIF.
        ENDLOOP.
        CLEAR ls_open.
      ENDIF.
    ENDIF.

****
    READ TABLE it_po WITH KEY werks = it_tab-werks  "Plant
                              matnr = it_tab-matnr. "Material

    IF sy-subrc EQ 0.
      it_tab-lgbzo = it_po-lgbzo.   "Group gate
    ENDIF.

    READ TABLE it_mast WITH KEY matnr = it_tab-matnr.

    IF sy-subrc EQ 0.
      MOVE : it_mast-feedr TO it_tab-feedr,
             it_mast-zline TO it_tab-zline,
             it_mast-works TO it_tab-works,
             it_mast-rh_lh TO it_tab-rh_lh.
    ENDIF.


    IF it_tab-lgort = 'P400'.   "Storage location

      SELECT SUM( gesme ) INTO it_tab-gesme   "Line Stock
        FROM lqua
        WHERE matnr = it_tab-matnr AND
              werks = p_werks      AND
              lgort IN s_lgort     AND   "Storage location
              " Only P400 exists in LQUA
              lgtyp IN ('441','442','443',
                        '444','445').  "Storage Type
*      " Inserted by Hakchin (20031020Mon)

      SELECT SUM( gesme ) INTO it_tab-wstok   "Warehouse stock
        FROM lqua
        WHERE matnr = it_tab-matnr AND
              werks = p_werks      AND
              lgort IN s_lgort     AND   "Storage location
              " Only P400 exists in LQUA
*              lgtyp IN ('500','510','520').  "Storage Type
              lgtyp IN ('431','432','433','434','435',
                        '436','437').  "Storage Type
      " Changed by Hakchin (20031020Mon)

      SELECT SUM( gesme ) INTO it_tab-ccbin "cc-bin stock
        FROM lqua
        WHERE matnr =  it_tab-matnr AND
              werks =  p_werks      AND
              lgort IN s_lgort      AND "Storage location
              lgtyp IN ('422').         "Storage Type


      SELECT SUM( gesme ) INTO it_tab-ccrack "cc-rack stock
        FROM lqua
        WHERE matnr = it_tab-matnr AND
              werks = p_werks      AND
              lgort IN s_lgort     AND "Storage location
              lgtyp IN ('421').        "Storage Type


      SELECT SUM( gesme ) INTO it_tab-cystok  "cy stock
        FROM lqua
        WHERE matnr = it_tab-matnr AND
              werks = p_werks      AND
              lgort IN s_lgort     AND "Storage location
              lgtyp IN ('411').        "Storage Type

** Stock: Not Yet GR
*      SELECT SUM( gesme ) INTO it_tab-notyetgr  "Not Yet GR
*        FROM lqua
*        WHERE matnr = it_tab-matnr AND
*              werks = p_werks      AND
*              lgort IN s_lgort     AND "Storage location
*              lgtyp IN ('100').        "Storage Type


      PERFORM line_inventory_cal.

    ENDIF.
*mspark add
*inbound delivery status is open and
*mobile arrival date is received.
    DATA : BEGIN OF IT_TMP OCCURS 0 ,
             ERDAT  LIKE LIKP-ERDAT ,
             TRAID  LIKE LIKP-TRAID ,
             ZFRETA LIKE ZTBL-ZFRETA,
           END OF IT_TMP.
    SELECT   LK~TRAID   "Container No.
             LK~ERDAT
             ZT~ZFRETA  "Arrival Date
             INTO CORRESPONDING FIELDS OF TABLE IT_TMP
*             "(IT_TAB-TRADI, IT_TAB-ERDAT, IT_TAB-ZFRETA)
             FROM LIPS AS LP INNER JOIN LIKP AS LK
               ON LP~VBELN = LK~VBELN
                  INNER JOIN VBUK AS VB
               ON LK~VBELN = VB~VBELN
                  INNER JOIN ZTMM_CONTAINER AS CONT
               ON LK~TRAID = CONT~CONT_REG_NUMB1
                  INNER JOIN ZTBL AS ZT
               ON LK~BOLNR = ZT~ZFHBLNO
            WHERE LP~MATNR = IT_TAB-MATNR
              AND ZT~ZFRETA IS NOT NULL
              AND VB~GBSTK = 'A'
              AND LK~TRATY = '0005'.
     SORT IT_TMP BY ERDAT.
     READ TABLE IT_TMP INDEX 1.
     IT_TAB-TRAID  = IT_TMP-TRAID.
     IT_TAB-ZFRETA = IT_TMP-ZFRETA.
*mspark
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

  append_fieldcat :
*   postion    field    ref field     key q-ref   c-ref
*   Text                leg

    w_col_pos 'MATNR'  'MATNR'        'X' ''      ''
    'Material'                        '18' '',
    w_col_pos 'MAKTX'  'MAKTX'        '' ''      ''
    'Material description'            '38' '',
    w_col_pos 'BISMT'  'BISMT'        '' ''      ''
    'ALC code'                        '18' '',
    w_col_pos 'WERKS'  'WERKS_D'      '' ''      ''
    'Plant'                           '4' '',
    w_col_pos 'LGORT'  'LGORT_D'      '' ''      ''
    'SLoc'                            '4' '',
    w_col_pos 'LGBZO'  'UNLPT'        '' ''      ''
    'Group gate'                      '10' '',
    w_col_pos 'FEEDR'  'ZFEEDER'      '' ''      ''
    'Feeder'                          '5' '',

    w_col_pos 'ZLINE'  'ZDLINE'       '' ''      ''
    'Line'                            '1' '',
    w_col_pos 'WORKS'  'ZWORKST'      '' ''      ''
    'W/S'                             '5' '',
    w_col_pos 'RH_LH'  'ZRH_LH'       '' ''      ''
    'RH/LH'                           '2' '',
    w_col_pos 'MEINS'  'MEINS'        '' ''      ''
    'Un'                              '3' '',
    w_col_pos 'MENGE'  'MENGE'        '' 'MEINS'      ''
    'Usage'                           '3' '',
    w_col_pos 'PROFL'  'ADGE_PROFL'   '' 'MEINS '      ''
    'Source'                          '3' '',
    w_col_pos 'BSTMI'  'BSTMI'        '' 'MEINS'      ''
    'Qty/con'                         '18' '',
    w_col_pos 'LABST'  'LABST'        '' 'MEINS'      ''
    'Inventory'                       '18' '',
    w_col_pos 'GESME'  'GESME'        '' 'MEINS'      ''
    'Line Stock'                      '18' '',
    w_col_pos 'WSTOK'  'GESME'        '' 'MEINS'      ''
    'Warehouse stock'                 '18' '',
    w_col_pos 'CCBIN'  'GESME'        '' 'MEINS'      ''
    'CC-bin stock'                    '18' '',
    w_col_pos 'CCRACK'  'GESME'       '' 'MEINS'      ''
    'CC-rack stock'                   '18' '',
    w_col_pos 'CYSTOK'  'GESME'       '' 'MEINS'      ''
    'CY stock'                        '18' '',
    w_col_pos 'MOBILE'  'GESME'       '' 'MEINS'      ''
    'Not Yet GR'                      '18' '',
*    w_col_pos 'MOBILE'  'GESME'       '' 'MEINS'      ''
*    'Mobile stock'                    '18' '',
    w_col_pos 'DUIN'  'GESME'         '' 'MEINS'      ''
    'Due in'                          '18' '',
    w_col_pos 'DISPO'  'GESME'        '' 'MEINS'      ''
    'Manager'                         '8' '',
*mspark add
    w_col_pos 'TRAID'  ''             ''  ''          ''
    'Cont.NO'                         '20' '',
    w_col_pos 'ZFRETA'  ''             ''  ''          ''
    'Dated Arrival Mobile'            '10' ''.
*mspark
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



* Material Type
  ls_line-key  = 'Plant: '.
  ls_line-typ  = 'S'.
  SELECT SINGLE name1 INTO l_name1 FROM t001w
         WHERE werks = p_werks.

  WRITE : p_werks   TO l_list(6),
          l_name1    TO l_list+7(30).
  ls_line-info = l_list.
  APPEND ls_line TO lt_top_of_page.

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

* workstation
  ls_line-key  = 'Source    : '.
  READ TABLE s_profl INDEX 1.
  IF sy-subrc EQ 0.
    ls_line-typ  = 'S'.

    CONCATENATE 'FROM: ' s_profl-low '  TO: ' s_profl-high INTO l_list.
    ls_line-info = l_list.
    APPEND ls_line TO lt_top_of_page.
  ENDIF.

  ls_line-key  = 'Storage Type: '.
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
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*   I_INTERFACE_CHECK                 = ' '
     i_bypassing_buffer                = 'X'
*   I_BUFFER_ACTIVE                   = ' '
     i_callback_program                = w_program
*   I_CALLBACK_PF_STATUS_SET          = ' '
     i_callback_user_command           = 'USER_COMMAND'
*   I_CALLBACK_TOP_OF_PAGE            = ' '
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
*   I_STRUCTURE_NAME                  =
*   I_BACKGROUND_ID                   = ' '
*  I_GRID_TITLE                      = text-001
*   I_GRID_SETTINGS                   =
*   IS_LAYOUT                         =
     it_fieldcat                       = w_fieldcat[]
*   IT_EXCLUDING                      =
*   IT_SPECIAL_GROUPS                 =
     it_sort                           = w_sortcat[]
*   IT_FILTER                         =
*   IS_SEL_HIDE                       =
*   I_DEFAULT                         = 'X'
      i_save                            = 'A'
*   IS_VARIANT                        =
     it_events                         = w_eventcat[]
*   IT_EVENT_EXIT                     =
     is_print                          = l_print_p
*   IS_REPREP_ID                      =
*   I_SCREEN_START_COLUMN             = 0
*   I_SCREEN_START_LINE               = 0
*   I_SCREEN_END_COLUMN               = 0
*   I_SCREEN_END_LINE                 = 0
*   IT_ALV_GRAPHICS                   =
*   IT_ADD_FIELDCAT                   =
*   IT_HYPERLINK                      =
*   I_HTML_HEIGHT_TOP                 =
*   I_HTML_HEIGHT_END                 =
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER           =
*   ES_EXIT_CAUSED_BY_USER            =
    TABLES
      t_outtab                          = it_tab
   EXCEPTIONS
     program_error                     = 1
     OTHERS                            = 2
            .
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
*mspark add
      IF SELFIELD-FIELDNAME = 'TRAID'.
       PERFORM PICKING_ORDER USING selfield.
       PERFORM ALV_FIELD_BUILD.
      ELSE.
*mspark
       PERFORM detail_list USING selfield. "Executed with double click
      ENDIF.
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

  READ TABLE it_tab INDEX selfield-tabindex.
  CHECK sy-subrc EQ 0.
  IF it_tab-profl = 'K'.   "Knock Down Parts

    SELECT lips~vgbel lips~vgpos likp~traid lips~lichn
           lips~vrkme lips~lfimg ztbl~zfetd ztbl~zfeta
          INTO CORRESPONDING FIELDS OF TABLE it_kd
          FROM likp   "Delivery Header Data
            INNER JOIN lips   "Delivery Item Data
              ON lips~vbeln = likp~vbeln   AND "Inbound Delivery
                 lips~matnr = it_tab-matnr     "Material
            INNER JOIN ztbl
              ON ztbl~zfblno = likp~bolnr   "BL no
          WHERE likp~lfart = 'EL'.  "Delivery type: Inbound Delivery

  ELSEIF it_tab-profl = 'V'.  "Local Parts
    SELECT likp~vbeln lips~posnr ztbl~zfetd ztbl~zfeta
           likp~traid likp~lgtor
      INTO TABLE it_lp
      FROM likp   "Delivery Header Data
        INNER JOIN lips   "Delivery Item Data
          ON lips~vbeln = likp~vbeln   AND
             lips~matnr = it_tab-matnr
        INNER JOIN ztbl
          ON ztbl~zfblno = likp~bolnr   "BL no
      WHERE likp~lfart = 'EL'.    "Delivery type: Inbound Delivery

    it_lp-duin = it_tab-duin.
    MODIFY TABLE it_lp  TRANSPORTING duin meins matnr.
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

  CLEAR : w_fieldcat[].

  append_fieldcat :
*   postion    field    ref field     key q-ref   c-ref
*   Text                leg

    w_col_pos 'VBELN'  'VBELN'        'X' ''      ''
    'ASN num'                         '10' '',
    w_col_pos 'POSNR'  'POSNR'        '' ''      ''
    'Item'                            '6' '',
    w_col_pos 'ETD'    'ETD'          '' ''      ''
    'ETD'                             '10' '',
    w_col_pos 'ETA'    'ETA'          '' ''      ''
    'ETA'                             '10' '',

    w_col_pos 'TRAID'  'TRAID'        '' ''      ''
    'Trailer No'                      '20' '',
    w_col_pos 'LGTOR'  'LGTOR'        '' ''      ''
    'Gate No'                         '3' '',
    w_col_pos 'MATNR'  'MATNR'        '' ''      ''
    'Material NO'                     '18' '',
    w_col_pos 'MEINS'  'MEINS'        '' ''      ''
    'Um'                          '3' '',
    w_col_pos 'DUEIN'  'MANGE'        '' 'MEINS'      ''
    'Due-in'                          '20' ''.

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
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_bypassing_buffer = 'X'
            i_callback_program = w_program
            i_grid_title       = text-004  "Sub list(LP)
            it_fieldcat        = w_fieldcat[]
            i_save             = 'A'
            it_events          = w_eventcat[]
            is_print           = l_print_p
       TABLES
            t_outtab           = it_lp
       EXCEPTIONS
            program_error      = 1
            OTHERS             = 2.
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
  CLEAR ex_etfz1.
  SELECT SINGLE etfz1
    INTO ex_etfz1
    FROM ekpo
    WHERE ebeln = im_ebeln AND
          ebelp = im_ebelp.
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
  CLEAR : w_fieldcat[].

  append_fieldcat :
*   postion    field    ref field     key q-ref   c-ref
*   Text                leg

    w_col_pos 'VGBEL'  'VGBEL'        'X' ''      ''
    'PO Number'                  '10' '',
    w_col_pos 'VGPOS'  'VGPOS'        '' ''      ''
    'Item'                       '6' '',
    w_col_pos 'TRAID'  'TRAID'        '' ''      ''
    'Container No'               '20' '',
    w_col_pos 'LICHN'  'LICHN'        '' ''      ''
    'Case No'                    '20' '',
    w_col_pos 'VRKME'  'VRKME'        '' ''      ''
    'Um'                         '3' '',
    w_col_pos 'LFIMG'  'LFIMG'        '' 'VRKME'      ''
    'Quantity'                   '20' '',
    w_col_pos 'ZFETD'  'ZFETD'          '' ''      ''
    'ETD'                        '10' '',
    w_col_pos 'ZFETA'  'ZFETA'          '' ''      ''
    'ETA'                        '10' ''.

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
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_bypassing_buffer = 'X'
            i_callback_program = w_program
            i_grid_title       = text-005  "Sub list(KD)
            it_fieldcat        = w_fieldcat[]
            i_save             = 'A'
            it_events          = w_eventcat[]
            is_print           = l_print_p
       TABLES
            t_outtab           = it_kd
       EXCEPTIONS
            program_error      = 1
            OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " list_write_detail_kd
*&---------------------------------------------------------------------*
*&      Form  PICKING_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PICKING_ORDER USING  selfield TYPE slis_selfield.
DATA: BEGIN OF IT_TMP OCCURS 0 ,
          TRAID LIKE LIKP-TRAID,      "CONT. NO.
          ERDAT LIKE LIKP-ERDAT,
                 SEQ(03) TYPE N,
          VGBEL LIKE LIPS-VGBEL,      "Order No.
        ZFSUMDT LIKE ZTIDSUS-ZFSUMDT, "C/CLEAR
          LFIMG LIKE LIPS-LFIMG,      "Material QTY.
          MEINS LIKE LIPS-MEINS,
         ZFRETA LIKE ZTBL-ZFRETA,     "Arrival Date
                     TOPPRT(03),      "TOP Priority
      END OF IT_TMP.
DATA: WA_TMP LIKE IT_TMP, CNT(03) TYPE N.
READ TABLE it_tab INDEX selfield-tabindex.
CHECK SY-SUBRC = 0.

*Get containers picking order by material
    SELECT   LK~TRAID   "Container No.
             LK~ERDAT
             LP~VGBEL   "Order No.
            ZTI~ZFSUMDT "C/CLEAR
             LP~LFIMG  "Material QTY.
             LP~MEINS
             ZT~ZFRETA  "Arrival Date
             INTO CORRESPONDING FIELDS OF TABLE IT_TMP
*             "(IT_TAB-TRADI, IT_TAB-ERDAT, IT_TAB-ZFRETA)
             FROM LIPS AS LP INNER JOIN LIKP AS LK
               ON LP~VBELN = LK~VBELN
                  INNER JOIN VBUK AS VB
               ON LK~VBELN = VB~VBELN
                  INNER JOIN ZTMM_CONTAINER AS CONT
               ON LK~TRAID = CONT~CONT_REG_NUMB1
                  INNER JOIN ZTBL AS ZT
               ON LK~BOLNR = ZT~ZFHBLNO
                  LEFT OUTER JOIN  ZTIDSUS AS ZTI
               ON LK~BOLNR = ZTI~ZFHBLNO
            WHERE LP~MATNR = IT_TAB-MATNR
              AND ZT~ZFRETA IS NOT NULL
              AND VB~GBSTK = 'A'
              AND LK~TRATY = '0005'.
     SORT IT_TMP BY TRAID.
*Sum quantity by Cont.no and get seq no.
     CLEAR : IT_PICK, IT_PICK[].
     LOOP AT IT_TMP INTO WA_TMP.
       AT END OF TRAID.
         SUM.
         CLEAR IT_PICK.
         CNT = CNT + 1.
         WA_TMP-SEQ = CNT.
         MOVE-CORRESPONDING WA_TMP TO IT_PICK.
         APPEND IT_PICK.
       ENDAT.
     ENDLOOP.
     SORT IT_PICK BY ERDAT.
     IT_PICK-TOPPRT = ' * '.
     MODIFY IT_PICK INDEX 1 TRANSPORTING TOPPRT.
ENDFORM.                    " PICKING_ORDER
*&---------------------------------------------------------------------*
*&      Form  ALV_FIELD_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_FIELD_BUILD.
  W_REPID = SY-REPID.
  CLEAR : WA_FIELDCAT[], WA_EVENTS[],
          WA_LIST_TOP_OF_PAGE[]     .
  PERFORM FIELDCAT_INIT  USING WA_FIELDCAT[].
  PERFORM EVENTTAB_BUILD USING WA_EVENTS[].
  PERFORM COMMENT_BUILD1  USING WA_LIST_TOP_OF_PAGE[].
  PERFORM CALL_ALV_FUNCTION.
ENDFORM.                    " ALV_FIELD_BUILD
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM FIELDCAT_INIT USING RT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.
  DATA: LS_FIELDCAT TYPE SLIS_FIELDCAT_ALV.
  DATA: POS TYPE I VALUE 1.

*Seq.
  clear ls_fieldcat.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'SEQ'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Seq.'.
  LS_FIELDCAT-SELTEXT_M     = 'Seq.'.
  LS_FIELDCAT-SELTEXT_S     = 'Seq.'.
  LS_FIELDCAT-OUTPUTLEN     = '3'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*Cont. No
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'TRAID'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'CONT.NO'.
  LS_FIELDCAT-SELTEXT_M     = 'CONT.NO'.
  LS_FIELDCAT-SELTEXT_S     = 'CONT.NO'.
  LS_FIELDCAT-OUTPUTLEN     = '20'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*ORDER NO
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'VGBEL'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'ORDER NO'.
  LS_FIELDCAT-SELTEXT_M     = 'ORDER NO'.
  LS_FIELDCAT-SELTEXT_S     = 'ORDER NO'.
  LS_FIELDCAT-OUTPUTLEN     = '10'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*C/CLEAR
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'ZFSUMDT'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'C/CLEAR'.
  LS_FIELDCAT-SELTEXT_M     = 'C/CLEAR'.
  LS_FIELDCAT-SELTEXT_S     = 'C/CLEAR'.
  LS_FIELDCAT-OUTPUTLEN     = '10'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*Material QTY
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'LFIMG'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = 'MEINS'.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Material QTY'.
  LS_FIELDCAT-SELTEXT_M     = 'Material QTY'.
  LS_FIELDCAT-SELTEXT_S     = 'Material QTY'.
  LS_FIELDCAT-OUTPUTLEN     = '15'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*Arrival in Mobile
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'ZFRETA'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Arrival in Mobile'.
  LS_FIELDCAT-SELTEXT_M     = 'Arrival in Mobile'.
  LS_FIELDCAT-SELTEXT_S     = 'Arrival in Mobile'.
  LS_FIELDCAT-OUTPUTLEN     = '10'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*TOP PRIORITY
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'TOPPRT'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'TOP PRIORITY'.
  LS_FIELDCAT-SELTEXT_M     = 'TOP PRIORITY'.
  LS_FIELDCAT-SELTEXT_S     = 'TOP PRIORITY'.
  LS_FIELDCAT-OUTPUTLEN     = '3'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
ENDFORM.                    " FIELDCAT_INIT
*&---------------------------------------------------------------------*
*&      Form  EVENTTAB_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_EVENTS[]  text
*----------------------------------------------------------------------*
FORM EVENTTAB_BUILD  USING E03_LT_EVENTS TYPE SLIS_T_EVENT.
  DATA: LS_EVENT TYPE SLIS_ALV_EVENT.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            I_LIST_TYPE = 0
       IMPORTING
            ET_EVENTS   = E03_LT_EVENTS.
  READ TABLE E03_LT_EVENTS WITH KEY NAME =  SLIS_EV_TOP_OF_PAGE
                           INTO LS_EVENT.
  IF SY-SUBRC = 0.
    MOVE W_FORMNAME_TOP_OF_PAGE TO LS_EVENT-FORM.
    APPEND LS_EVENT TO E03_LT_EVENTS.
  ENDIF.
ENDFORM.                    " EVENTTAB_BUILD
*&---------------------------------------------------------------------*
*&      Form  COMMENT_BUILD1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_LIST_TOP_OF_PAGE[]  text
*----------------------------------------------------------------------*
FORM COMMENT_BUILD1 USING LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
DATA: LS_LINE TYPE SLIS_LISTHEADER.
DATA: INFO_TXT(50).

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
* LS_LINE-KEY:  not used for this type
  LS_LINE-INFO = TEXT-101.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*DATE
  CLEAR INFO_TXT.
   WRITE : SY-DATUM TO INFO_TXT.
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = 'DATE:'.
  LS_LINE-INFO = INFO_TXT.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

ENDFORM.                    " COMMENT_BUILD1
*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM TOP_OF_PAGE1.
 CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
      EXPORTING
*           i_logo             = 'HTMLCNTL_TESTHTM2_SAPLOGO'
*           I_LOGO             = 'ENJOYSAP_LOGO'
           IT_LIST_COMMENTARY = WA_LIST_TOP_OF_PAGE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CALL_ALV_FUNCTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_ALV_FUNCTION.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM           = W_REPID
      IT_EVENTS                    = WA_EVENTS[]
      IT_FIELDCAT                  = WA_FIELDCAT[]
*     I_CALLBACK_USER_COMMAND      = 'USER_COMMAND1'
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER        =
*   ES_EXIT_CAUSED_BY_USER         =
    TABLES
      T_OUTTAB                     = IT_PICK
* EXCEPTIONS
*   PROGRAM_ERROR                  = 1
*   OTHERS                         = 2
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " CALL_ALV_FUNCTION

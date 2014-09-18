************************************************************************
* Program Name      : ZRMMPM09_EMG_PO                                  *
* Author            : Jaesung-LEE                                      *
* Creation Date     : 2003.08.14.                                      *
* Specifications By : Jaesung-LEE                                      *
* Pattern           " Report 1 - 1
* Development Request No :                                             *
* Addl Documentation:                                                  *
* Description       : Emergency PO List                                *
*                                                                      *
* Modification Logs                                                    *
* Date            Developer          RequestNo    Description          *
* 2003.08.14.     Jaesung-LEE                     Initial Coding       *
*                                                                      *
************************************************************************


REPORT zrmmpm09_emg_po .



************************************************************************
* Data
************************************************************************


TYPE-POOLS : slis.

TABLES : mara,
         marc,
         mseg,
         ekko,
         eban,
         mkpf.

DATA: BEGIN OF it_tab OCCURS 10,
      matnr  LIKE eban-matnr,
      txz01  LIKE eban-txz01,
      lifnr  LIKE ekko-lifnr,
      banfn  LIKE eban-banfn, " Purchase requisition number
      bnfpo  LIKE eban-bnfpo, " Item number of purchase requisition
      badat  LIKE eban-badat, " Requisition (request) date
      menge  LIKE eban-menge, " Purchase requisition quantity
      meins  LIKE eban-meins, " unit
      ebeln  LIKE ekko-ebeln, " Purchase order
      ebelp  LIKE ekpo-ebelp, " item
      bedat  LIKE ekko-bedat, " Purchase date
      eindt  LIKE eket-eindt, " Delivery Date
      mqant  LIKE ekpo-menge, " Purchase order quantity
      openq  LIKE ekpo-menge, " Open Qty
      dmdne  LIKE ekpo-menge, " net req.
      labst  LIKE mard-labst, " Current Stock
      appro  TYPE syuname,    " Approved by
      webre  LIKE ekpo-webre,
      werks  LIKE ekpo-werks, " PLANT
      lgort  LIKE ekpo-lgort, " STORAGE LOCATION
      name1  LIKE lfa1-name1,
      END OF it_tab.

*-- pr table
DATA: BEGIN OF it_pr OCCURS 10,
      matnr  LIKE eban-matnr,
      txz01  LIKE eban-txz01,
      lifnr  LIKE ekko-lifnr,
      banfn  LIKE eban-banfn, " Purchase requisition number
      bnfpo  LIKE eban-bnfpo, " Item number of purchase requisition
      badat  LIKE eban-badat, " Requisition (request) date
      menge  LIKE eban-menge, " Purchase requisition quantity
      meins  LIKE eban-meins, " unit
      appro  LIKE eban-ernam,
      werks  LIKE ekpo-werks, " PLANT
      lgort  LIKE ekpo-lgort, " STORAGE LOCATION
      lfdat  LIKE eban-lfdat, " Delivery date
      END OF it_pr.

*--- po table
DATA: BEGIN OF it_po OCCURS 10,
      mark,
      matnr  LIKE eban-matnr,
      txz01  LIKE eban-txz01,
      lifnr  LIKE ekko-lifnr,
      banfn  LIKE eban-banfn, " Purchase requisition number
      bnfpo  LIKE eban-bnfpo, " Item number of purchase requisition
      badat  LIKE eban-badat, " Requisition (request) date
      menge  LIKE eban-menge, " Purchase requisition quantity
      meins  LIKE eban-meins, " unit
      ebeln  LIKE ekko-ebeln, " Purchase order
      ebelp  LIKE ekpo-ebelp, " item
      bedat  LIKE ekko-bedat, " Purchase date
      eindt  LIKE eket-eindt, " Delivery Date
      mqant  LIKE ekpo-menge, " Purchase order quantity
      openq  LIKE ekpo-menge, " Open Qty
      labst  LIKE mard-labst, " Current Stock
      appro  LIKE ekko-ernam,
      webre  LIKE ekpo-webre,
      werks  LIKE ekpo-werks, " PLANT
      lgort  LIKE ekpo-lgort, " STORAGE LOCATION

      END OF it_po.

*--- ALV
DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat TYPE slis_t_event WITH HEADER LINE,
       w_selfield TYPE slis_selfield,
       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       w_col_pos  TYPE i,
       w_program  LIKE sy-repid,
       w_top_of_page TYPE slis_t_listheader,
       w_line TYPE slis_listheader,
       w_layout   TYPE slis_layout_alv.

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


************************************************************************
* SELECTION SCREEN
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_matnr FOR mara-matnr,  " Material
                 s_mtart FOR mara-mtart,  " Material Type
                 s_werks FOR mseg-werks  DEFAULT 'P001'
                                         OBLIGATORY, " Plant
                 s_lgort FOR mseg-lgort  OBLIGATORY, " Storage Location
                 s_lifnr FOR mseg-lifnr,  " Vendor
                 s_dispo FOR marc-dispo, "OBLIGATORY," Manager
                 s_ebeln FOR ekko-ebeln,  " PO No.
                 s_banfn FOR eban-banfn,  " PR No.
                 s_budat FOR mkpf-budat.
*                 s_spmon FOR s031-spmon NO-EXTENSION.
PARAMETERS : p_ok AS CHECKBOX ,
             p_no AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK block1.

************************************************************************
* INITIALIZATION
************************************************************************

INITIALIZATION.
  s_budat-high = sy-datum.
  s_budat-low  = sy-datum - 1.
  s_budat-option = 'BT'.
  s_budat-sign = 'I'.
  APPEND s_budat.

  s_mtart-low = 'ROH'.
  s_mtart-sign = 'I'.
  s_mtart-option = 'EQ'.
  APPEND s_mtart.

************************************************************************
* AT SELECTION-SCREEN
************************************************************************
AT SELECTION-SCREEN.

  PERFORM check_selection_screen.

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
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_selection.

  SELECT matnr txz01 banfn bnfpo badat menge meins ernam AS appro
         lifnr werks lgort lfdat


         INTO CORRESPONDING FIELDS OF TABLE it_pr
         FROM  eban


         WHERE bsart = 'EM'
           AND banfn IN s_banfn
           AND werks IN s_werks
           AND lifnr IN s_lifnr
           AND lgort IN s_lgort
           AND dispo IN s_dispo
           AND badat IN s_budat
           AND loekz = ' ' ..

  PERFORM mtart_deltion.



  SELECT p~matnr p~txz01 k~lifnr p~banfn p~bnfpo p~menge
         p~meins p~ebeln p~ebelp k~bedat e~eindt k~ernam AS appro
         p~werks p~lgort
         INTO CORRESPONDING FIELDS OF TABLE it_po

         FROM ( ( ekko AS k INNER JOIN ekpo AS p
                      ON k~ebeln =  p~ebeln ) INNER JOIN eket AS e
                      ON p~ebeln = e~ebeln AND
                         p~ebelp = e~ebelp )

         WHERE k~bsart = 'EM'
           AND p~matnr IN s_matnr
           AND p~werks IN s_werks
           AND p~lgort IN s_lgort
           AND k~bedat IN s_budat
           AND p~ebeln IN s_ebeln
           AND p~banfn IN s_banfn
           AND p~mtart IN s_mtart
           AND k~lifnr IN s_lifnr
           AND e~etenr = '0001'
           AND p~loekz = ' '.

  PERFORM dispo_delet.


ENDFORM.                    " data_selection
*&---------------------------------------------------------------------*
*&      Form  data_caculation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_calculation.

  PERFORM po_list_selection.
  PERFORM confirm_list_check.
  PERFORM current_stock.



ENDFORM.                    " data_caculation
*&---------------------------------------------------------------------*
*&      Form  po_list_selection
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM po_list_selection.

  DATA: l_index LIKE sy-tabix.

  DATA : it_ekbes LIKE ekbes OCCURS 0 WITH HEADER LINE.

  DATA : l_menge LIKE ekpo-menge.


*-- PO LIST
  LOOP AT it_po .

    MOVE-CORRESPONDING it_po TO it_tab.
*--PO Qty
    CLEAR : it_tab-menge .
    MOVE : it_po-menge TO it_tab-mqant.

* READ PR LIST .
    READ TABLE it_pr WITH KEY banfn = it_po-banfn
                              bnfpo = it_po-bnfpo.

    IF sy-subrc EQ 0.

      l_index = sy-tabix.

      MOVE : it_pr-badat TO it_tab-badat,
             it_pr-menge TO it_tab-menge.

* DELETE READ PR LIST .

      DELETE it_pr INDEX l_index.

    ENDIF.

* OPEN Qty

*clear : it_ekbes[].
*
*    CALL FUNCTION 'ME_READ_HISTORY'
*      EXPORTING
*        ebeln                    = it_po-ebeln
*        ebelp                    = it_po-ebelp
*        webre                    = it_po-webre
**   I_BYPASSING_BUFFER       =
**   I_REFRESH_BUFFER         =
*     TABLES
**   XEKBE                    =
*       xekbes                   = it_ekbes
**   XEKBEZ                   =
**   XEKBNK                   =
**   XEKBZ                    =
*              .
*
*    LOOP AT it_ekbes.
*      l_menge = l_menge + it_ekbes-wemng.
*
*    ENDLOOP.
*
*
*    it_tab-openq = it_tab-mqant - l_menge.
*
*    CLEAR : l_menge.

    APPEND it_tab.
    CLEAR it_tab.


  ENDLOOP.

* NOT ASSIGN PR LIST APPEND

  LOOP AT it_pr.

    MOVE-CORRESPONDING it_pr TO it_tab.
    MOVE : it_pr-lfdat TO it_tab-eindt.
    APPEND it_tab.
    CLEAR it_tab.

  ENDLOOP.

ENDFORM.                    " po_list_selection
*&---------------------------------------------------------------------*
*&      Form  CONFIRM_LIST_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM confirm_list_check.

*  CONFIRM
* PO existence

  IF p_ok = 'X'.
    DELETE it_tab WHERE ebelp = space .
  ENDIF.

* NOT CONFIRM
* PO DON'T existence

  IF p_no = 'X'.
    DELETE it_tab WHERE ebelp NE space.
  ENDIF.

ENDFORM.                    " CONFIRM_LIST_CHECK
*&---------------------------------------------------------------------*
*&      Form  MTART_DELTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM mtart_deltion.
  DATA : l_mt TYPE i,         " SELECT-OPTION COUNT
         l_mtart TYPE mtart.  " MATERIAL TYPE

  DESCRIBE TABLE s_mtart LINES l_mt.

  CHECK l_mt > 0.

  LOOP AT it_pr.
    SELECT SINGLE mtart INTO l_mtart
           FROM mara
           WHERE matnr = it_pr-matnr.

    IF sy-subrc EQ 0.
      IF l_mtart IN s_mtart.
      ELSE.
        DELETE it_pr.
      ENDIF.

    ENDIF.
    CLEAR l_mtart.
  ENDLOOP.


ENDFORM.                    " MTART_DELTION
*&---------------------------------------------------------------------*
*&      Form  DISPO_DELET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dispo_delet.
  DATA : l_di TYPE i,
         l_dispo TYPE dispo.


  LOOP AT it_po.
    SELECT SINGLE dispo INTO l_dispo
           FROM marc
           WHERE matnr = it_po-matnr
             AND werks = it_po-werks.

    IF sy-subrc EQ 0.
      IF l_dispo IN s_dispo.
      ELSE.
        DELETE it_po.
      ENDIF.


    ENDIF.

    CLEAR l_dispo.

  ENDLOOP.

ENDFORM.                    " DISPO_DELET
*&---------------------------------------------------------------------*
*&      Form  STOCK_AND_OPEN_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM current_stock.
  DATA : l_shkzg LIKE resb-shkzg,
         l_bdmng LIKE resb-bdmng.

  DATA : BEGIN OF it_stock OCCURS 0,
* * include structure mard.
         matnr LIKE ekpo-matnr,
         bwkey LIKE mbew-bwkey,
         lbkum LIKE mbew-lbkum,
         END OF it_stock.

  DATA : BEGIN OF st_open ,
*         matnr like MDBS-matnr,
          menge LIKE mdbs-menge,
          wemng LIKE mdbs-wemng,
          END OF st_open.

  DATA : BEGIN OF it_vname OCCURS 1,
         lifnr LIKE lfa1-lifnr,
         name1 LIKE lfa1-name1,
         END OF it_vname.

  SELECT matnr bwkey lbkum INTO TABLE it_stock
*   select * into table it_stock
         FROM mbew
         FOR ALL ENTRIES IN it_tab
         WHERE matnr = it_tab-matnr
           AND bwkey = it_tab-werks .
*           AND lgort = it_tab-lgort.



  SELECT lifnr name1 INTO TABLE it_vname
         FROM lfa1
         FOR ALL ENTRIES IN it_tab
         WHERE lifnr = it_tab-lifnr .

*  CHECK sy-subrc EQ 0.

*CALL FUNCTION 'MARD_EXTEND'
* EXPORTING
**   KZRFB              = ' '
**   MAXTZ              = ' '
*   XVPER              = 'X'
** IMPORTING
**   RMARDH             =
*  TABLES
*    MARD_TAB           = it_stock
**   MARD_EXT_TAB       =
  .

  LOOP AT it_tab.
*- open po qty

    SELECT SINGLE  SUM( menge ) AS menge SUM( wemng ) AS wemng
         INTO  st_open
         FROM mdbs
         WHERE matnr = it_tab-matnr
           AND werks = it_tab-werks
           AND loekz = ' '
           AND elikz = ' '
           GROUP by matnr .
    IF sy-subrc EQ 0.
      it_tab-openq = st_open-menge - st_open-wemng.
      CLEAR st_open.
    ENDIF.

* Net Requirment .
    SELECT shkzg bdmng   INTO (l_shkzg, l_bdmng)

    FROM resb

    WHERE xloek = ' '
      AND matnr = it_tab-matnr
      AND werks = it_tab-werks
      AND bdter = it_tab-eindt.

      IF sy-subrc EQ 0.
        IF l_shkzg = 'H'.
          it_tab-dmdne = l_bdmng + it_tab-dmdne.
        ELSE.
          it_tab-dmdne = l_bdmng + it_tab-dmdne.
        ENDIF.
      ENDIF.

    ENDSELECT.
* read current stock
    READ TABLE it_stock WITH KEY matnr = it_tab-matnr
                                 bwkey = it_tab-werks .

    IF sy-subrc EQ 0.
      MOVE : it_stock-lbkum TO it_tab-labst.

    ENDIF.
* read vendor name
    READ TABLE it_vname WITH KEY lifnr = it_tab-lifnr.
    IF sy-subrc EQ 0.
      MOVE : it_vname-name1 TO it_tab-name1.
    ENDIF.

    MODIFY it_tab.

  ENDLOOP.

ENDFORM.                    " STOCK_AND_OPEN_ORDER
*&---------------------------------------------------------------------*
*&      Form  LIST_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM list_write.
*---
  DATA : l_count TYPE i.

  CLEAR l_count.

  DESCRIBE TABLE it_tab LINES l_count .

  IF l_count = 0.
    MESSAGE s420(me) ." with text-002.
    STOP.
  ENDIF.

  SORT it_tab BY badat matnr.

  PERFORM build_fieldcat.
  PERFORM build_event.
  PERFORM build_sort.
  PERFORM comment_build USING  w_top_of_page[].
  PERFORM alv_function.
ENDFORM.                    " LIST_WRITE
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
    w_col_pos 'TXZ01' 40 'Material Desc'  'CHAR' ' '  ''      '' '',
    w_col_pos 'LIFNR' 10 'Vendor'         'CHAR' ' '  ''      '' '',
    w_col_pos 'NAME1' 20 'Vendor Desc'    'CHAR' ' '  ''      '' '',
    w_col_pos 'BANFN' 10 'PR No'          'CHAR' ' '  ''      '' '',
    w_col_pos 'BNFPO' 06 'PR Item'        'NUMC' ' '  ''      '' '',
    w_col_pos 'BADAT' 10 'PR Date'        'DATS' ' '  ''      '' '',
    w_col_pos 'MENGE' 12 'Qty Requested'  'QUAN' ' '  'MEINS' '' '',
    w_col_pos 'MEINS' 03 'UoM'            'UNIT' ' '  ''      '' '',
    w_col_pos 'EBELN' 10 'PO Number'      'CHAR' ' '  ''      '' '',
    w_col_pos 'EBELP' 05 'PO Item'        'NUMC' ' '  ''      '' '',
    w_col_pos 'BEDAT' 10 'PO Date'        'DATS' ' '  ''      '' '',
    w_col_pos 'EINDT' 10 'Delivery Date'  'DATS' ' '  ''      '' '',
    w_col_pos 'MQANT' 12 'PO Qty'         'QUAN' ' '  'MEINS' '' '',
    w_col_pos 'OPENQ' 12 'Open Qty'       'QUAN' ' '  'MEINS' '' '',
    w_col_pos 'DMDNE' 12 'Net Req'        'QUAN' ' '  'MEINS' '' '',
    w_col_pos 'LABST' 12 'Current Stock'  'QUAN' ' '  'MEINS' '' '',
    w_col_pos 'APPRO' 12 'Approved by'    'CHAR' ' '  ''      '' ''.
ENDFORM.                    " build_fieldcat
*&---------------------------------------------------------------------*
*&      Form  alv_function
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_function.
*---
  DATA:   l_print_p TYPE slis_print_alv.  " print setting

  CLEAR : w_program.

  MOVE : sy-repid TO w_program.

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
*&---------------------------------------------------------------------*
*&      user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM user_command USING ucomm LIKE sy-ucomm
                       selfield TYPE slis_selfield.
  CASE ucomm.

    WHEN '&IC1'. " Double Click
      CALL TRANSACTION 'ME21N' .

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      E04_COMMENT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM comment_build USING lt_top_of_page TYPE slis_t_listheader.
*---
  DATA: ls_line TYPE slis_listheader,
        l_manager(50),
        l_date(50),
        l_list(50),
        l_dsnam LIKE t024d-dsnam,
        l_h_dsnam LIKE t024d-dsnam,
        l_ldate(10),
        l_hdate(10).

*---
  CLEAR : w_line.
  w_line-typ  = 'H'.
  w_line-info = text-100.
  APPEND w_line TO w_top_of_page.

  append_top :
      'S' text-003 s_matnr-low s_matnr-high,
      'S' text-004 s_mtart-low s_mtart-high,
      'S' text-005 s_werks-low s_werks-high,
      'S' text-006 s_lgort-low s_lgort-high,
      'S' text-007 s_lifnr-low s_lifnr-high,
      'S' text-008 s_ebeln-low s_ebeln-high,
      'S' text-009 s_banfn-low s_banfn-high,
      'S' text-010 s_dispo-low s_dispo-high,
      'S' text-011 s_budat-low s_budat-high.

  IF p_ok = 'X'.
    w_line-typ  = 'A'.
    w_line-info = text-106.
    APPEND w_line TO  w_top_of_page.
  ENDIF.

  IF p_no = 'X'.
    w_line-typ  = 'A'.
    w_line-info = text-105.
    APPEND w_line TO  w_top_of_page.
  ENDIF.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM top_of_page.
*---
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            it_list_commentary = w_top_of_page.
ENDFORM.
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
*&      Form  CHECK_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_selection_screen.

  IF p_ok = 'X' AND p_no = 'X'.
    MESSAGE e096(me)." WITH 'ONLY ONE CHOICE CONFIRM STATUS'.
  ENDIF.
ENDFORM.                    " CHECK_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*&      Form  BUILD_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sort.

* LIST SORT SEQENCE
  w_sortcat-spos           = 6.
  w_sortcat-fieldname      = 'BADAT'.
  w_sortcat-tabname        = 'IT_TAB'.
  w_sortcat-up             = 'X'.
  APPEND w_sortcat.

  w_sortcat-spos           = 1.
  w_sortcat-fieldname      = 'MATNR'.
  w_sortcat-tabname        = 'IT_TAB'.
  w_sortcat-up             = 'X'.
  APPEND w_sortcat.

  w_sortcat-spos           = 2.
  w_sortcat-fieldname      = 'TXZ01'.
  w_sortcat-tabname        = 'IT_TAB'.
  w_sortcat-up             = 'X'.
  APPEND w_sortcat.

ENDFORM.                    " BUILD_SORT

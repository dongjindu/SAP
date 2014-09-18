************************************************************************
* Program Name      : ZRMMPM08R_PO_QTY_CHG
* Author            : Sung-Tae, Lim
* Creation Date     : 2003.08.18.
* Specifications By : Sung-Tae, Lim
* Pattern           : Report 1-1
* Development Request No : UD1K901864
* Addl Documentation:
* Description       : PO Qty Change Status List
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.08.18.     Sung-Tae Lim     UD1K901864     Initial Coding
*
*
************************************************************************

REPORT zrmmpm08r_po_qty_chg NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmmm.

INCLUDE : zrmmpmxxr_incl.

**--- Tables, Views & Structures

**--- Internal Tables
DATA : BEGIN OF it_itab OCCURS 0,
         ebeln LIKE ekpo-ebeln,     " PO No.
         ebelp LIKE ekpo-ebelp,     " PO Item
         matnr LIKE ekpo-matnr,     " Material No.
         txz01 LIKE ekpo-txz01,     " Material Desc.
         lifnr LIKE lfa1-lifnr,     " Vendor
         name1 LIKE lfa1-name1,     " Vendor Name
         aedat LIKE ekko-aedat,     " PO Date
         eindt LIKE eket-eindt,     " Delivery Date
         menge LIKE eket-menge,     " Scheduled Qty.
*         grqty LIKE ekpo-menge,     " GR Cumulated
         wemng LIKE eket-wemng,     " Quantity of goods received
         opqty LIKE ekpo-menge,     " Open PO Qty
         meins LIKE ekpo-meins,     " UoM
         budat LIKE mkpf-budat,     " Latest GR Date
         bsgru LIKE ekpo-bsgru,     " PO Reason
         char1(1),                  " Flag
         erdat LIKE likp-erdat,     " I/D Creation Date
         dispo LIKE marc-dispo,     " Manater(MRP Controller)
       END OF it_itab.

DATA : BEGIN OF it_temp OCCURS 0.
        INCLUDE STRUCTURE it_itab.
DATA :   werks LIKE ekpo-werks,     " Plant
         loekz LIKE ekpo-loekz,     " Del. Indicator
         elikz LIKE ekpo-elikz,     " Delivery Comp. Indicator
*         wemng LIKE eket-wamng,
       END OF it_temp.

**--- Variables
RANGES : r_elikz FOR ekpo-elikz.

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
  w_fieldcat-qfieldname = &7.
  w_fieldcat-cfieldname = &8.
*  w_fieldcat-do_sum     = &8.
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

**---
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.

SELECT-OPTIONS : s_matnr FOR mara-matnr OBLIGATORY,
                 s_mtart FOR mara-mtart DEFAULT 'ROH',
                 s_werks FOR ekpo-werks,
                 s_lgort FOR ekpo-lgort,
                 s_dispo FOR marc-dispo,
                 s_lifnr FOR ekko-lifnr,
                 s_bsgru FOR ekpo-bsgru,
                 s_eindt FOR eket-eindt OBLIGATORY.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK block11 WITH FRAME TITLE text-011.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 10.
PARAMETERS p_close AS CHECKBOX DEFAULT 'X'.              " Completed PO
SELECTION-SCREEN COMMENT (14) text-012 FOR FIELD p_close.
SELECTION-SCREEN POSITION 44.
PARAMETERS p_open  AS CHECKBOX DEFAULT 'X'.              " Open PO
SELECTION-SCREEN COMMENT (14) text-013 FOR FIELD p_open.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK block11.

SELECTION-SCREEN END OF BLOCK block1.

**---
INITIALIZATION.
  PERFORM event_build USING w_eventcat[].

**---
TOP-OF-PAGE.
  PERFORM top_of_page.

**---
START-OF-SELECTION.
  PERFORM get_data.

**---
END-OF-SELECTION.
  IF it_itab[] IS INITIAL.
    MESSAGE s999 WITH text-m01.
  ELSE.
    PERFORM comment_build.     " using w_top_of_page[].
    PERFORM make_alv_grid.
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
  DATA : l_zbudat LIKE mseg-zbudat,
         l_ebelp LIKE lips-vgpos.

  CLEAR : r_elikz, r_elikz[].

  IF p_close NE space.
    MOVE : 'I'  TO r_elikz-sign,
           'EQ' TO r_elikz-option,
           'X'  TO r_elikz-low.
    APPEND r_elikz.
  ENDIF.

  IF p_open NE space.
    MOVE : 'I'  TO r_elikz-sign,
           'EQ' TO r_elikz-option,
           ' '  TO r_elikz-low.
    APPEND r_elikz.
  ENDIF.

**---
  CLEAR : it_itab, it_itab[], it_temp, it_temp[].

  SELECT a~ebeln  a~lifnr  b~ebelp  b~werks
         b~txz01  b~aedat  b~meins  b~bsgru
         b~loekz  b~elikz  b~matnr  c~eindt
         c~menge  c~wemng  d~dispo  e~name1
                  INTO CORRESPONDING FIELDS OF TABLE it_temp
                  FROM ekko AS a INNER JOIN ekpo AS b
                    ON a~mandt EQ b~mandt
                   AND a~ebeln EQ b~ebeln
                       INNER JOIN eket AS c
                          ON b~mandt EQ c~mandt
                         AND b~ebeln EQ c~ebeln
                         AND b~ebelp EQ c~ebelp
                             INNER JOIN marc AS d
                                ON b~mandt EQ d~mandt
                               AND b~matnr EQ d~matnr
                               AND b~werks EQ d~werks
                                   INNER JOIN lfa1 AS e
                                      ON a~mandt EQ e~mandt
                                     AND a~lifnr EQ e~lifnr
                 WHERE d~matnr IN s_matnr
                   AND b~werks IN s_werks
                   AND b~lgort IN s_lgort
                   AND a~lifnr IN s_lifnr
                   AND bsgru IN s_bsgru
                   AND eindt IN s_eindt
                   AND elikz IN r_elikz
                   AND mtart IN s_mtart
                   AND dispo IN s_dispo.

  SORT it_temp BY ebeln ebelp.

  LOOP AT it_temp.
    MOVE-CORRESPONDING it_temp TO it_itab.
    IF it_temp-loekz NE space OR it_temp-elikz NE space.
      MOVE : 'X'               TO it_itab-char1.
    ENDIF.
*--- open PO Qty.
    it_itab-opqty = it_temp-menge - it_temp-wemng.
*--- latest GR
    AT NEW ebelp.
      CLEAR : mseg, l_zbudat.
      SELECT * FROM mseg
              WHERE ebeln EQ it_temp-ebeln
                AND ebelp EQ it_temp-ebelp
           ORDER BY zbudat DESCENDING.
        MOVE : mseg-zbudat TO l_zbudat.
        EXIT.
      ENDSELECT.
    ENDAT.
    MOVE : l_zbudat TO it_itab-budat.
*--- inbound delivery creation date
    CLEAR : lips, l_ebelp.
    CONCATENATE '0' it_itab-ebelp INTO l_ebelp.
    SELECT SINGLE erdat INTO it_itab-erdat
                        FROM lips
                       WHERE vgbel EQ it_itab-ebeln
                         AND vgpos EQ l_ebelp.
*--- append it_itab
    APPEND it_itab.
    CLEAR : it_temp, it_itab.
  ENDLOOP.
ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_TOP_OF_PAGE[]  text
*----------------------------------------------------------------------*
FORM comment_build.     " USING p_w_top_of_page type slis_t_listheader.
**---
  CLEAR : w_line.
  w_line-typ  = 'H'.
  w_line-info = text-002.
  APPEND w_line TO w_top_of_page.

  CLEAR : w_line.
  APPEND INITIAL LINE TO w_top_of_page.

  append_top :
      'S' text-003 s_matnr-low s_matnr-high,
      'S' text-004 s_mtart-low s_mtart-high,
      'S' text-005 s_werks-low s_werks-high,
      'S' text-006 s_lgort-low s_lgort-high,
      'S' text-007 s_dispo-low s_dispo-high,
      'S' text-008 s_lifnr-low s_lifnr-high,
      'S' text-009 s_bsgru-low s_bsgru-high,
      'S' text-010 s_eindt-low s_eindt-high.

*  clear : w_line.
*  w_line-typ  = 'S'.
*  w_line-key  = text-003.
*  move : s_matnr-low  to w_line-info(18),
*         'to'         to w_line-info+19(2),
*         s_matnr-high to w_line-info+23(18).
*  append w_line to p_w_top_of_page.
*
*  clear : w_line.
*  w_line-typ  = 'S'.
*  w_line-key  = text-004.
*  move : s_mtart-low  to w_line-info(18),
*         'to'         to w_line-info+19(2),
*         s_mtart-high to w_line-info+23(18).
*  append w_line to p_w_top_of_page.
*
*  clear : w_line.
*  w_line-typ  = 'S'.
*  w_line-key  = text-005.
*  move : s_werks-low  to w_line-info(18),
*         'to'         to w_line-info+19(2),
*         s_werks-high to w_line-info+23(18).
*  append w_line to p_w_top_of_page.
*
*  clear : w_line.
*  w_line-typ  = 'S'.
*  w_line-key  = text-006.
*  move : s_lgort-low  to w_line-info(18),
*         'to'         to w_line-info+19(2),
*         s_lgort-high to w_line-info+23(18).
*  append w_line to p_w_top_of_page.
*
*  clear : w_line.
*  w_line-typ  = 'S'.
*  w_line-key  = text-007.
*  move : s_dispo-low  to w_line-info(18),
*         'to'         to w_line-info+19(2),
*         s_dispo-high to w_line-info+23(18).
*  append w_line to p_w_top_of_page.
*
*  clear : w_line.
*  w_line-typ  = 'S'.
*  w_line-key  = text-008.
*  move : s_lifnr-low  to w_line-info(18),
*         'to'         to w_line-info+19(2),
*         s_lifnr-high to w_line-info+23(18).
*  append w_line to p_w_top_of_page.
*
*  clear : w_line.
*  w_line-typ  = 'S'.
*  w_line-key  = text-009.
*  move : s_bsgru-low  to w_line-info(18),
*         'to'         to w_line-info+19(2),
*         s_bsgru-high to w_line-info+23(18).
*  append w_line to p_w_top_of_page.
*
*  clear : w_line.
*  w_line-typ  = 'S'.
*  w_line-key  = text-010.
*  move : s_eindt-low  to w_line-info(18),
*         'to'         to w_line-info+19(2),
*         s_eindt-high to w_line-info+23(18).
*  append w_line to p_w_top_of_page.
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

  w_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program = w_program
            is_layout          = w_layout
            it_fieldcat        = w_fieldcat[]
            it_events          = w_eventcat[]
            it_sort            = w_sortcat[]
            i_save             = 'A'
       TABLES
            t_outtab           = it_itab
       EXCEPTIONS
            program_error      = 1
            OTHERS             = 2.
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
**--- &7 : qty field      &8 :
  append_fieldcat :
    w_col_pos 'EBELN' 10 'PO No.'         'CHAR' 'X' ''      '',
    w_col_pos 'EBELP'  5 'Item'           'NUMC' 'X' ''      '',
    w_col_pos 'MATNR' 18 'Material'       'CHAR' ''  ''      '',
    w_col_pos 'TXZ01' 30 'Material Desc.' 'CHAR' ''  ''      '',
    w_col_pos 'LIFNR' 10 'Vendor'         'CHAR' ''  ''      '',
    w_col_pos 'NAME1' 20 'Vendor Desc.'   'CHAR' ''  ''      '',
    w_col_pos 'AEDAT' 10 'PO Date'        'DATS' ''  ''      '',
    w_col_pos 'EINDT' 10 'Del. Date'      'DATS' ''  ''      '',
    w_col_pos 'MENGE' 15 'Scheduled Qty.' 'QUAN' ''  'MEINS' '',
*    w_col_pos 'GRQTY' 15 'GR Cumulated'   'QUAN' ''  'MEINS' '',
    w_col_pos 'WEMNG' 15 'GR Qty.'        'QUAN' ''  'MEINS' '',
    w_col_pos 'OPQTY' 15 'Open PO Qty.'   'QUAN' ''  'MEINS' '',
    w_col_pos 'MEINS'  3 'UoM'            'UNIT' ''  ''      '',
    w_col_pos 'BUDAT' 10 'Latest GR Date' 'DATS' ''  ''      '',
    w_col_pos 'BSGRU'  3 'PO Reason'      'CHAR' ''  ''      '',
    w_col_pos 'CHAR1'  1 'Flag'           'CHAR' ''  ''      '',
    w_col_pos 'ERDAT' 10 'I/D Cr. Date'   'DATS' ''  ''      ''.
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
**--- &4 : up             &5 : Sub-Tot
  append_sortcat : '1' 'EBELN' 'IT_ITAB' 'X' '',
                   '2' 'EBELP' 'IT_ITAB' 'X' '',
                   '3' 'MATNR' 'IT_ITAB' 'X' '',
                   '4' 'TXZ01' 'IT_ITAB' 'X' '',
                   '5' 'LIFNR' 'IT_ITAB' 'X' '',
                   '6' 'NAME1' 'IT_ITAB' 'X' ''.
ENDFORM.                    " build_sortcat

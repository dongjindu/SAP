************************************************************************
* Program Name      : ZRMMPM01R_POGR
* Author            : Sung-Tae, Lim
* Creation Date     : 2003.08.06.
* Specifications By : Sung-Tae, Lim
* Development Request No : UD1K901864
* Addl Documentation:
* Description       : PO to GR Status List
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.08.06.     Sung-Tae Lim     UD1K901864     Initial Coding
* 2003.10.16.     Sung-Tae Lim     UD1K901864     New Coding
*
*
************************************************************************

REPORT zrmmpm01_pogr NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmmm.

**---
INCLUDE : zrmmpmxxr_incl.

**--- Internal Tables
DATA : BEGIN OF it_itab OCCURS 0,
        lifnr LIKE ekko-lifnr,     " Vendor
        ebeln LIKE eket-ebeln,     " PO Number
        bsart LIKE ekko-bsart,     " PO Document Type
        bedat LIKE ekko-bedat,     " PO Date
        name1 LIKE lfa1-name1,     " Vendor Description
        ebelp LIKE eket-ebelp,     " PO Item No.
        matnr LIKE ekpo-matnr,     " Material
*        maktx like makt-maktx,     " Material Description
        txz01 LIKE ekpo-txz01,     " Description
        dispo LIKE marc-dispo,     " Manager (MRP Controller)
        etenr LIKE eket-etenr,     " Delivery Schedule Line No.
        eindt LIKE eket-eindt,     " Delivery Date
                                   " Delivery Time
        menge LIKE eket-menge,     " Quantity
        meins LIKE ekpo-meins,     " Unit of Measure
        openq LIKE eket-menge,     " Open Quantity
        vbeln LIKE lips-vbeln,     " Inbound Delivery Number
        ormng LIKE lips-ormng,     " Inbound Delivery Quantity
        wadat_ist LIKE likp-wadat_ist,     " Inbound Delivery Date
        mblnr LIKE mseg-mblnr,     " GR Document Number
        grund LIKE mseg-grund,     " Reason for Movement
        menge_gr LIKE mseg-menge,     " GR Quantity
        budat LIKE mkpf-budat,     " GR Date (Posting Date)
        sakto LIKE mseg-sakto,     " Account
        bwart LIKE mseg-bwart,     " Movement Type
        dmbtr LIKE mseg-dmbtr,     " Amount
        waers LIKE mseg-waers,     " Currency Key
        usnam LIKE mkpf-usnam,     " User Name

        wemng LIKE eket-wemng,     " Quantity of goods received
        posnn LIKE vbfa-posnn,     " Subsequent item of an SD document
       END OF it_itab.

DATA : it_temp LIKE it_itab OCCURS 0 WITH HEADER LINE.

**--- Variables
DATA : w_matnr_s LIKE mara-matnr,
       w_matnr_e LIKE mara-matnr,
       w_mtart_s LIKE mara-mtart,
       w_mtart_e LIKE mara-mtart,
       w_dispo_s LIKE marc-dispo,
       w_dispo_e LIKE marc-dispo,
       w_lifnr_s LIKE lfa1-lifnr,
       w_lifnr_e LIKE lfa1-lifnr,
       w_werks_s LIKE ekpo-werks,
       w_werks_e LIKE ekpo-werks,
       w_lgort_s LIKE ekpo-lgort,
       w_lgort_e LIKE ekpo-lgort,
       w_ebeln_s LIKE ekpo-ebeln,
       w_ebeln_e LIKE ekpo-ebeln,
       w_bsart_s LIKE ekko-bsart,
       w_bsart_e LIKE ekko-bsart,
       w_vbeln_s LIKE lips-vbeln,
       w_vbeln_e LIKE lips-vbeln,
       w_mblnr_s LIKE mseg-mblnr,
       w_mblnr_e LIKE mseg-mblnr,
       w_budat_s LIKE mkpf-budat,
       w_budat_e LIKE mkpf-budat,
       w_cputm_s LIKE mkpf-cputm,
       w_cputm_e LIKE mkpf-cputm.

**--- Constants

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
*  w_fieldcat-key        = &6.
  w_fieldcat-do_sum     = &6.
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

**---
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_matnr FOR ekpo-matnr NO-EXTENSION,     " Material No.
                 s_mtart FOR mara-mtart DEFAULT 'ROH' NO-EXTENSION,
                 " Material Type
                 s_dispo FOR marc-dispo OBLIGATORY NO-EXTENSION,
                 " Manager
                 s_lifnr FOR lfa1-lifnr OBLIGATORY NO-EXTENSION,
                 " Vendor
                 s_werks FOR ekpo-werks NO-EXTENSION,     " Plant
                 s_lgort FOR ekpo-lgort NO-EXTENSION,
                 " Storage Location
                 s_ebeln FOR ekpo-ebeln NO-EXTENSION,     " PO Number
                 s_bsart FOR ekko-bsart NO-EXTENSION,     " PO Doc. Type
                 s_vbeln FOR lips-vbeln NO-EXTENSION,
                 " Inbound Delivery
                 s_mblnr FOR mkpf-mblnr NO-EXTENSION,     " GR Doc. No.
                 s_budat FOR mkpf-budat NO-EXTENSION,     " GR Date
                 s_cputm FOR mkpf-cputm NO-EXTENSION.     " GR Time
SELECTION-SCREEN END OF BLOCK block1.

**---
INITIALIZATION.
  PERFORM event_build USING w_eventcat[].

**---
TOP-OF-PAGE.
  PERFORM top_of_page.

**---
START-OF-SELECTION.
*  IF p_open NE space.
*    PERFORM get_data.
*  ELSEIF p_natv NE space.
  PERFORM get_data_native.
*  ENDIF.

**---
END-OF-SELECTION.
  IF it_itab[] IS INITIAL.
    MESSAGE s999 WITH text-m01.
  ELSE.
    PERFORM comment_build.
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
  CLEAR : it_itab, it_itab[], it_temp, it_temp[].

**--- join with ekko/ekpo/eket/marc/likp/lips
  SELECT a~ebeln a~bsart a~bedat a~lifnr
         b~ebelp b~matnr b~txz01
         c~etenr c~eindt c~menge
         b~meins
         d~dispo
         e~vbeln e~ormng
         f~wadat_ist
         c~wemng
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
                                  INNER JOIN lips AS e
                                     ON b~mandt EQ e~mandt
                                    AND b~ebeln EQ e~vgbel
                                    AND b~ebelp EQ e~vgpos
                                        INNER JOIN likp AS f
                                           ON e~mandt EQ f~mandt
                                          AND e~vbeln EQ f~vbeln
                WHERE ( a~bstyp EQ 'F' OR     " PO
                        a~bstyp EQ 'L' )      " SA
                  AND a~bsart IN s_bsart     " PO Doc. Type
                  AND a~lifnr IN s_lifnr     " Vendor
                  AND a~ebeln IN s_ebeln     " PO Number
                  AND b~matnr IN s_matnr     " Material Number
                  AND b~werks IN s_werks     " Plant
                  AND b~lgort IN s_lgort     " Storage Location
                  AND b~mtart IN s_mtart     " Material Type
                  AND b~loekz EQ space       " Delete Indicator
                  AND d~dispo IN s_dispo     " Manager (MRP Controller)
                  AND f~vbeln IN s_vbeln.    " Inbound Delivery

  LOOP AT it_temp.
*--
    MOVE-CORRESPONDING it_temp TO it_itab.
*--
    it_itab-openq = it_itab-menge - it_itab-wemng.
*-- get Vendor Desc.
    PERFORM get_vendor_desc USING it_temp-lifnr.
    MOVE : lfa1-name1          TO it_itab-name1.
*-- get Mat'l Desc. if it_temp-txz01 eq space
    IF it_temp-txz01 EQ space.
      PERFORM get_material_desc USING it_temp-matnr.
      MOVE : makt-maktx        TO it_itab-txz01.
    ENDIF.
*-- get I/D information

*--
    APPEND it_itab.
    CLEAR : it_itab, it_temp.
  ENDLOOP.
ENDFORM.                    " get_data

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
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_TOP_OF_PAGE[]  text
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
      'S' text-003 s_matnr-low s_matnr-high,
      'S' text-004 s_mtart-low s_mtart-high,
      'S' text-005 s_dispo-low s_dispo-high,
      'S' text-006 s_lifnr-low s_lifnr-high,
      'S' text-007 s_werks-low s_werks-high,
      'S' text-008 s_lgort-low s_lgort-high,
      'S' text-009 s_ebeln-low s_ebeln-high,
      'S' text-010 s_bsart-low s_bsart-high,
      'S' text-011 s_vbeln-low s_vbeln-high,
      'S' text-012 s_mblnr-low s_mblnr-high,
      'S' text-013 s_budat-low s_budat-high.
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
**--- position / fieldname / tabname / length
**--- long text
**--- middle text
**--- short text
**--- data type / key / do sum / no out / quantity / currency

*---
  PERFORM append_fieldcat USING : w_col_pos  'EBELN'  'IT_ITAB'  '10'
                                  'PO No.'
                                  'PO No.'
                                  'PO No.'
                                  'CHAR'  'X'  ''  ''  ''  ''.
*---
  PERFORM append_fieldcat USING : w_col_pos  'BSART'  'IT_ITAB'  '2'
                                  'PO Document Type'
                                  'PO Doc. Type'
                                  'Tp'
                                  'CHAR'  ''  ''  ''  ''  ''.
*---
  PERFORM append_fieldcat USING : w_col_pos  'BEDAT'  'IT_ITAB'  '10'
                                  'PO Date'
                                  'PO Date'
                                  'PO Date'
                                  'DATS'  ''  ''  ''  ''  ''.
*---
  PERFORM append_fieldcat USING : w_col_pos  'LIFNR'  'IT_ITAB'  '10'
                                  'Vendor'
                                  'Vendor'
                                  'Vendor'
                                  'CHAR'  ''  ''  ''  ''  ''.
*---
  PERFORM append_fieldcat USING : w_col_pos  'NAME1'  'IT_ITAB'  '20'
                                  'Vendor Description'
                                  'Vendor Description'
                                  'Vendor Desc.'
                                  'CHAR'  ''  ''  ''  ''  ''.
*---
*  PERFORM append_fieldcat USING : w_col_pos  'EBELP'  'IT_ITAB'  '5'
*                                  'PO Item'
*                                  'Item'
*                                  'Item'
*                                  'NUMC'  ''  ''  ''  ''  ''.
*---
  PERFORM append_fieldcat USING : w_col_pos  'MATNR'  'IT_ITAB'  '18'
                                  'Material Number'
                                  'Material Number'
                                  'Material No.'
                                  'CHAR'  ''  ''  ''  ''  ''.
*---
  PERFORM append_fieldcat USING : w_col_pos  'TXZ01'  'IT_ITAB'  '30'
                                  'Material Description'
                                  'Material Description'
                                  'Material Desc.'
                                  'CHAR'  ''  ''  ''  ''  ''.
*---
  PERFORM append_fieldcat USING : w_col_pos  'DISPO'  'IT_ITAB'  '3'
                                  'Manager'
                                  'Manager'
                                  'Manager'
                                  'CHAR'  ''  ''  ''  ''  ''.
*---
*  PERFORM append_fieldcat USING : w_col_pos  'ETENR'  'IT_ITAB'  '4'
*                                  'Delivery Item'
*                                  'Del. Item'
*                                  'Item'
*                                  'NUMC'  ''  ''  ''  ''  ''.
*---
  PERFORM append_fieldcat USING : w_col_pos  'EINDT'  'IT_ITAB'  '10'
                                  'Delivery Date'
                                  'Delivery Date'
                                  'Del. Date'
                                  'DATS'  ''  ''  ''  ''  ''.
*---
  PERFORM append_fieldcat USING : w_col_pos  'MENGE'  'IT_ITAB'  '12'
                                  'Delivery Quantity'
                                  'Del. Qty.'
                                  'Quantity'
                                  'QUAN'  ''  ''  ''  'MEINS'  ''.
*---
  PERFORM append_fieldcat USING : w_col_pos  'MEINS'  'IT_ITAB'  '3'
                                  'UoM'
                                  'UoM'
                                  'UoM'
                                  'UNIT'  ''  ''  ''  ''  ''.
*---
  PERFORM append_fieldcat USING : w_col_pos  'OPENQ'  'IT_ITAB'  '12'
                                  'Open Quantity'
                                  'Open Qty.'
                                  'Open Qty.'
                                  'QUAN'  ''  ''  ''  'MEINS'  ''.
*---
  PERFORM append_fieldcat USING : w_col_pos  'VBELN'  'IT_ITAB'  '10'
                                  'Inbound Delivery'
                                  'Inb. Del.'
                                  'I/D'
                                  'CHAR'  ''  ''  ''  ''  ''.
*---
  PERFORM append_fieldcat USING : w_col_pos  'ORMNG'  'IT_ITAB'  '12'
                                  'Inbound Delivery Quantity'
                                  'Inb. Del. Quantity'
                                  'I/D Qty.'
                                  'QUAN'  ''  ''  ''  'MEINS'  ''.
*---
  PERFORM append_fieldcat USING : w_col_pos  'WADAT_IST' 'IT_ITAB' '10'
                                  'Inbound Delivery Date'
                                  'Inb. Del. Date'
                                  'I/D Date'
                                  'DATS'  ''  ''  ''  ''  ''.
*---
  PERFORM append_fieldcat USING : w_col_pos  'MBLNR'  'IT_ITAB'  '10'
                                  'GR Document Number'
                                  'GR Doc. Number'
                                  'GR Doc.No.'
                                  'CHAR'  ''  ''  ''  ''  ''.
*---
  PERFORM append_fieldcat USING : w_col_pos  'GRUND'  'IT_ITAB'  '4'
                                  'Reason for Movement'
                                  'Reason'
                                  'Reason'
                                  'NUMC'  ''  ''  ''  ''  ''.
*---
 PERFORM append_fieldcat USING : w_col_pos  'MENGE_GR'  'IT_ITAB'  '12'
                                                          'GR Quantity'
                                                          'GR Quantity'
                                                          'GR Qty.'
                                        'QUAN'  ''  ''  ''  'MEINS'  ''.
*---
  PERFORM append_fieldcat USING : w_col_pos  'BUDAT'  'IT_ITAB'  '10'
                                  'GR Date'
                                  'GR Date'
                                  'GR Date'
                                  'DATS'  ''  ''  ''  ''  ''.
*---
  PERFORM append_fieldcat USING : w_col_pos  'SAKTO'  'IT_ITAB'  '10'
                                  'Account'
                                  'Account'
                                  'Account'
                                  'CHAR'  ''  ''  ''  ''  ''.
*---
  PERFORM append_fieldcat USING : w_col_pos  'BWART'  'IT_ITAB'  '3'
                                  'Movement Type'
                                  'Mvt.Type.'
                                  'MvT'
                                  'CHAR'  ''  ''  ''  ''  ''.
*---
  PERFORM append_fieldcat USING : w_col_pos  'DMBTR'  'IT_ITAB'  '12'
                                  'Amount'
                                  'Amount'
                                  'Amount'
                                  'CURR'  ''  ''  ''  ''  'WAERS'.
*---
  PERFORM append_fieldcat USING : w_col_pos  'WAERS'  'IT_ITAB'  '5'
                                  'Currency'
                                  'Curr.'
                                  'Curr.'
                                  'CURK'  ''  ''  ''  ''  ''.
*---
  PERFORM append_fieldcat USING : w_col_pos  'USNAM'  'IT_ITAB'  '10'
                                  'User Name'
                                  'User Name'
                                  'User Name'
                                  'CHAR'  ''  ''  ''  ''  ''.
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
  append_sortcat : '1' 'EBELN' 'IT_ITAB' 'X' ''.
*                   '2' 'BSART' 'IT_ITAB' 'X' '',
*                   '3' 'BEDAT' 'IT_ITAB' 'X' '',
*                   '4' 'LIFNR' 'IT_ITAB' 'X' '',
*                   '5' 'NAME1' 'IT_ITAB' 'X' '',
*                   '6' 'EBELP' 'IT_ITAB' 'X' '',
*                   '7' 'MATNR' 'IT_ITAB' 'X' '',
*                   '8' 'TXZ01' 'IT_ITAB' 'X' '',
*                   '9' 'DISPO' 'IT_ITAB' 'X' ''.
*                  '10' 'ETENR' 'IT_ITAB' 'X' ''.
ENDFORM.                    " build_sortcat

*&---------------------------------------------------------------------*
*&      Form  append_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM append_fieldcat USING    p_col_pos p_fieldname p_tabname p_length
                              p_long p_middle p_short
                              p_type p_key p_sum p_noout p_quan p_curr.
*---
  p_col_pos = p_col_pos + 1.
  w_fieldcat-col_pos    = p_col_pos.
  w_fieldcat-fieldname  = p_fieldname.
  w_fieldcat-tabname    = p_tabname.
  w_fieldcat-outputlen  = p_length.
  w_fieldcat-seltext_l  = p_long.
  w_fieldcat-seltext_m  = p_middle.
  w_fieldcat-seltext_s  = p_short.
  w_fieldcat-datatype   = p_type.
  w_fieldcat-key        = p_key.
  w_fieldcat-do_sum     = p_sum.
  w_fieldcat-qfieldname = p_quan.
  w_fieldcat-cfieldname = p_curr.
  w_fieldcat-no_out     = p_noout.
  APPEND w_fieldcat.
  CLEAR : w_fieldcat.
ENDFORM.                    " append_fieldcat

*&---------------------------------------------------------------------*
*&      Form  get_data_native
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_native.
**---
  CLEAR : it_itab, it_itab[], it_temp, it_temp[].

**---
  PERFORM make_select_options.

***--- join with ekko/ekpo/eket/marc/lips/likp/vbfa/mseg/mkpf
*  EXEC sql performing append_it_temp.
*    SELECT A.LIFNR, A.EBELN,     A.BSART, A.BEDAT, J.NAME1,
*           B.EBELP, B.MATNR,     B.TXZ01, D.DISPO, C.ETENR,
*           C.EINDT, C.MENGE,     B.MEINS, (C.MENGE - C.WEMNG),
*           E.VBELN,
*           E.ORMNG, F.WADAT_IST, I.VBELN, G.GRUND, G.MENGE,
*           H.BUDAT, G.SAKTO,     G.BWART, G.DMBTR, G.WAERS,
*           H.USNAM, C.WEMNG,     I.POSNN
*                   INTO :IT_ITAB
*                   FROM EKPO B, EKET C, MARC D, LIPS E, LIKP F,
*                        VBFA I, MSEG G, MKPF H, LFA1 J, EKKO A
**--- EKKO : Selection Condition
*                  WHERE A.MANDT = :SY-MANDT
*                    AND A.BSTYP IN ( 'F', 'L' )
*                    AND A.BSART BETWEEN :W_BSART_S AND :W_BSART_E
*                    AND A.LIFNR BETWEEN :W_LIFNR_S AND :W_LIFNR_E
*                    AND A.EBELN BETWEEN :W_EBELN_S AND :W_EBELN_E
**--- EKPO & EKKO
*                    AND B.MANDT = A.MANDT
*                    AND B.EBELN = A.EBELN
**--- EKPO : Selection Condition
*                    AND B.MATNR BETWEEN :W_MATNR_S AND :W_MATNR_E
*                    AND B.MTART BETWEEN :W_MTART_S AND :W_MTART_E
*                    AND B.WERKS BETWEEN :W_WERKS_S AND :W_WERKS_E
*                    AND B.LGORT BETWEEN :W_LGORT_S AND :W_LGORT_E
**--- EKET & EKPO
*                    AND C.MANDT = B.MANDT
*                    AND C.EBELN = B.EBELN
*                    AND C.EBELP = B.EBELP
***--- EKET : Selection Condition
**                    AND C.EINDT BETWEEN :W_BUDAT_S AND :W_BUDAT_E
**--- MARC & EKPO
*                    AND D.MANDT = B.MANDT
*                    AND D.MATNR = B.MATNR
*                    AND D.WERKS = B.WERKS
**--- MARC : Selection Condition
*                    AND D.DISPO BETWEEN :W_DISPO_S AND :W_DISPO_E
**--- LIPS & EKPO
*                    AND E.MANDT(+) = B.MANDT
*                    AND E.VGBEL(+) = B.EBELN
*                    AND E.VGPOS(+) = CONCAT('0',B.EBELP)
**--- LIKP & LIPS
*                    AND F.MANDT = E.MANDT
*                    AND F.VBELN = E.VBELN
**--- LIKP : Selection Condition
*                    AND F.VBELN BETWEEN :W_VBELN_S AND :W_VBELN_E
**--- VBFA & LIPS
*                    AND I.MANDT(+) = E.MANDT
*                    AND I.VBELV(+) = E.VBELN
*                    AND I.POSNV(+) = E.POSNR
*                    AND I.VBTYP_N(+) = 'R'
**--- MSEG & VBFA
*                    AND G.MANDT(+) = I.MANDT
*                    AND G.MBLNR(+) = I.VBELN
*                    AND G.ZEILE(+) = SUBSTR(I.POSNN,3,4)
**--- MKPF & MSEG
*                    AND H.MANDT(+) = G.MANDT
*                    AND H.MJAHR(+) = G.MJAHR
*                    AND H.MBLNR(+) = G.MBLNR
**--- MKPF : Selection Condition
*                    AND H.BUDAT(+) BETWEEN :W_BUDAT_S AND :W_BUDAT_E
*                    AND H.CPUTM(+) BETWEEN :W_CPUTM_S AND :W_CPUTM_E
*                    AND H.MBLNR(+) BETWEEN :W_MBLNR_S AND :W_MBLNR_E
**--- LFA1 & EKKO
*                    AND J.MANDT = A.MANDT
*                    AND J.LIFNR = A.LIFNR
*  ENDEXEC.


***--- join with ekko/ekpo/eket/ekes/marc/lips/likp/vbfa/mseg/mkpf
*  EXEC sql performing append_it_temp.
*    SELECT A.LIFNR, A.EBELN,     A.BSART, A.BEDAT, J.NAME1,
*           B.EBELP, B.MATNR,     B.TXZ01, D.DISPO, C.ETENR,
*           C.EINDT, C.MENGE,     B.MEINS, (C.MENGE - C.WEMNG),
*           E.VBELN,
*           E.ORMNG, F.WADAT_IST, I.VBELN, G.GRUND, G.MENGE,
*           H.BUDAT, G.SAKTO,     G.BWART, G.DMBTR, G.WAERS,
*           H.USNAM, C.WEMNG,     I.POSNN
*                   INTO :IT_ITAB
*                   FROM EKPO B, EKET C, EKES K, MARC D, LIPS E, LIKP F,
*                        VBFA I, MSEG G, MKPF H, LFA1 J, EKKO A
**--- EKKO : Selection Condition
*                  WHERE A.MANDT = :SY-MANDT
*                    AND A.BSTYP IN ( 'F', 'L' )
*                    AND A.BSART BETWEEN :W_BSART_S AND :W_BSART_E
*                    AND A.LIFNR BETWEEN :W_LIFNR_S AND :W_LIFNR_E
*                    AND A.EBELN BETWEEN :W_EBELN_S AND :W_EBELN_E
**--- EKPO & EKKO
*                    AND B.MANDT = A.MANDT
*                    AND B.EBELN = A.EBELN
**--- EKPO : Selection Condition
*                    AND B.MATNR BETWEEN :W_MATNR_S AND :W_MATNR_E
*                    AND B.MTART BETWEEN :W_MTART_S AND :W_MTART_E
*                    AND B.WERKS BETWEEN :W_WERKS_S AND :W_WERKS_E
*                    AND B.LGORT BETWEEN :W_LGORT_S AND :W_LGORT_E
**--- EKET & EKPO
*                    AND C.MANDT = B.MANDT
*                    AND C.EBELN = B.EBELN
*                    AND C.EBELP = B.EBELP
***--- EKET : Selection Condition
**                    AND C.EINDT BETWEEN :W_BUDAT_S AND :W_BUDAT_E
**--- EKES & EKET
*                    AND K.MANDT = C.MANDT
*                    AND K.EBELN = C.EBELN
*                    AND K.EBELP = C.EBELP
*                    AND K.EINDT = C.EINDT
**--- MARC & EKPO
*                    AND D.MANDT = B.MANDT
*                    AND D.MATNR = B.MATNR
*                    AND D.WERKS = B.WERKS
**--- MARC : Selection Condition
*                    AND D.DISPO BETWEEN :W_DISPO_S AND :W_DISPO_E
***--- LIPS & EKPO
**                    AND E.MANDT(+) = B.MANDT
**                    AND E.VGBEL(+) = B.EBELN
**                    AND E.VGPOS(+) = CONCAT('0',B.EBELP)
**--- LIPS & EKES
*                    AND E.MANDT = K.MANDT
*                    AND E.VBELN = K.VBELN
*                    AND E.POSNR = K.VBELP
**--- LIKP & LIPS
*                    AND F.MANDT = E.MANDT
*                    AND F.VBELN = E.VBELN
**--- LIKP : Selection Condition
*                    AND F.VBELN BETWEEN :W_VBELN_S AND :W_VBELN_E
**--- VBFA & LIPS
*                    AND I.MANDT(+) = E.MANDT
*                    AND I.VBELV(+) = E.VBELN
*                    AND I.POSNV(+) = E.POSNR
*                    AND I.VBTYP_N(+) = 'R'
**--- MSEG & VBFA
*                    AND G.MANDT(+) = I.MANDT
*                    AND G.MBLNR(+) = I.VBELN
*                    AND G.ZEILE(+) = SUBSTR(I.POSNN,3,4)
**--- MKPF & MSEG
*                    AND H.MANDT(+) = G.MANDT
*                    AND H.MJAHR(+) = G.MJAHR
*                    AND H.MBLNR(+) = G.MBLNR
**--- MKPF : Selection Condition
*                    AND H.BUDAT(+) BETWEEN :W_BUDAT_S AND :W_BUDAT_E
*                    AND H.CPUTM(+) BETWEEN :W_CPUTM_S AND :W_CPUTM_E
*                    AND H.MBLNR(+) BETWEEN :W_MBLNR_S AND :W_MBLNR_E
**--- LFA1 & EKKO
*                    AND J.MANDT = A.MANDT
*                    AND J.LIFNR = A.LIFNR
*  ENDEXEC.

**--- join with ekko/ekpo/eket/marc/zvmm_delv/vbfa/mseg/mkpf
  EXEC sql performing append_it_temp.
    SELECT A.LIFNR,  A.EBELN,  A.BSART,  A.BEDAT,
           J.NAME1,
           B.EBELP,  B.MATNR,  B.TXZ01,
           D.DISPO,
           C.ETENR,  C.EINDT,  C.MENGE,
           B.MEINS,
           (C.MENGE - G.MENGE),
           E.VBELN,  E.ORMNG,  E.WADAT_IST,
           F.VBELN,
           G.GRUND,  G.MENGE,
           H.BUDAT,
           G.SAKTO,  G.BWART,  G.DMBTR,  G.WAERS,
           H.USNAM,
           C.WEMNG,
           F.POSNN
                   INTO :IT_ITAB
                   FROM EKPO B, EKET C, MARC D, ZVMM_DELV E,
                        VBFA F, MSEG G, MKPF H, LFA1 J, EKKO A
*--- EKKO : Selection Condition
                  WHERE A.MANDT = :SY-MANDT
                    AND A.BSTYP IN ( 'F', 'L' )
                    AND A.BSART BETWEEN :W_BSART_S AND :W_BSART_E
                    AND A.LIFNR BETWEEN :W_LIFNR_S AND :W_LIFNR_E
                    AND A.EBELN BETWEEN :W_EBELN_S AND :W_EBELN_E
*--- EKPO & EKKO
                    AND B.MANDT = A.MANDT
                    AND B.EBELN = A.EBELN
*--- EKPO : Selection Condition
                    AND B.MATNR BETWEEN :W_MATNR_S AND :W_MATNR_E
                    AND B.MTART BETWEEN :W_MTART_S AND :W_MTART_E
                    AND B.WERKS BETWEEN :W_WERKS_S AND :W_WERKS_E
                    AND B.LGORT BETWEEN :W_LGORT_S AND :W_LGORT_E
*--- EKET & EKPO
                    AND C.MANDT = B.MANDT
                    AND C.EBELN = B.EBELN
                    AND C.EBELP = B.EBELP
*--- MARC & EKPO
                    AND D.MANDT = B.MANDT
                    AND D.MATNR = B.MATNR
                    AND D.WERKS = B.WERKS
*--- MARC : Selection Condition
                    AND D.DISPO BETWEEN :W_DISPO_S AND :W_DISPO_E
*--- ZVMM_DELV & EKET
                    AND E.MANDT(+) = C.MANDT
                    AND E.VGBEL(+) = C.EBELN
                    AND E.VGPOS(+) = CONCAT('0',C.EBELP)
                    AND E.LFDAT(+) = C.EINDT
                    AND E.LFUHR(+) = C.UZEIT
*--- Addition
*                    AND E.LFUHR <> '000000'
                    AND C.UZEIT <> '000000'
*--- ZVMM_DELV : Selection Condition
                    AND F.VBELN BETWEEN :W_VBELN_S AND :W_VBELN_E
*--- VBFA & ZVMM_DELV
                    AND F.MANDT(+) = E.MANDT
                    AND F.VBELV(+) = E.VBELN
                    AND F.POSNV(+) = E.POSNR
                    AND F.VBTYP_N(+) = 'R'
*--- MSEG & VBFA
                    AND G.MANDT(+) = F.MANDT
                    AND G.MBLNR(+) = F.VBELN
                    AND G.ZEILE(+) = SUBSTR(F.POSNN,3,4)
*--- MKPF & MSEG
                    AND H.MANDT(+) = G.MANDT
                    AND H.MJAHR(+) = G.MJAHR
                    AND H.MBLNR(+) = G.MBLNR
*--- MKPF : Selection Condition
                    AND H.BUDAT(+) BETWEEN :W_BUDAT_S AND :W_BUDAT_E
                    AND H.CPUTM(+) BETWEEN :W_CPUTM_S AND :W_CPUTM_E
                    AND H.MBLNR(+) BETWEEN :W_MBLNR_S AND :W_MBLNR_E
*--- LFA1 & EKKO
                    AND J.MANDT = A.MANDT
                    AND J.LIFNR = A.LIFNR
  ENDEXEC.
ENDFORM.                    " get_data_native

*&---------------------------------------------------------------------*
*&      Form  append_it_temp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_it_temp.
**---
  APPEND it_itab.
  CLEAR : it_itab.
ENDFORM.                    " append_it_temp

*&---------------------------------------------------------------------*
*&      Form  make_select_options
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_select_options.
*--- s_matnr
  CALL FUNCTION 'Z_FMM_SELECT_OPTIONS'
       EXPORTING
            i_low  = s_matnr-low
            i_high = s_matnr-high
       IMPORTING
            e_low  = w_matnr_s
            e_high = w_matnr_e.

*--- s_mtart
  CALL FUNCTION 'Z_FMM_SELECT_OPTIONS'
       EXPORTING
            i_low  = s_mtart-low
            i_high = s_mtart-high
       IMPORTING
            e_low  = w_mtart_s
            e_high = w_mtart_e.

*--- s_dispo
  CALL FUNCTION 'Z_FMM_SELECT_OPTIONS'
       EXPORTING
            i_low  = s_dispo-low
            i_high = s_dispo-high
       IMPORTING
            e_low  = w_dispo_s
            e_high = w_dispo_e.

*--- s_lifnr
  CALL FUNCTION 'Z_FMM_SELECT_OPTIONS'
       EXPORTING
            i_low  = s_lifnr-low
            i_high = s_lifnr-high
       IMPORTING
            e_low  = w_lifnr_s
            e_high = w_lifnr_e.

*--- s_werks
  CALL FUNCTION 'Z_FMM_SELECT_OPTIONS'
       EXPORTING
            i_low  = s_werks-low
            i_high = s_werks-high
       IMPORTING
            e_low  = w_werks_s
            e_high = w_werks_e.
*--- s_lgort
  CALL FUNCTION 'Z_FMM_SELECT_OPTIONS'
       EXPORTING
            i_low  = s_lgort-low
            i_high = s_lgort-high
       IMPORTING
            e_low  = w_lgort_s
            e_high = w_lgort_e.

*--- s_ebeln
  CALL FUNCTION 'Z_FMM_SELECT_OPTIONS'
       EXPORTING
            i_low  = s_ebeln-low
            i_high = s_ebeln-high
       IMPORTING
            e_low  = w_ebeln_s
            e_high = w_ebeln_e.

*--- s_bsart
  CALL FUNCTION 'Z_FMM_SELECT_OPTIONS'
       EXPORTING
            i_low  = s_bsart-low
            i_high = s_bsart-high
       IMPORTING
            e_low  = w_bsart_s
            e_high = w_bsart_e.

*--- s_vbeln
  CALL FUNCTION 'Z_FMM_SELECT_OPTIONS'
       EXPORTING
            i_low  = s_vbeln-low
            i_high = s_vbeln-high
       IMPORTING
            e_low  = w_vbeln_s
            e_high = w_vbeln_e.

*--- s_mblnr
  CALL FUNCTION 'Z_FMM_SELECT_OPTIONS'
       EXPORTING
            i_low  = s_mblnr-low
            i_high = s_mblnr-high
       IMPORTING
            e_low  = w_mblnr_s
            e_high = w_mblnr_e.

*--- s_budat
  IF s_budat-low IS INITIAL AND s_budat-high IS INITIAL.
    MOVE : '00000000'   TO w_budat_s,
           '99999999'   TO w_budat_e.
  ELSEIF NOT s_budat-low IS INITIAL AND s_budat-high IS INITIAL.
    MOVE : s_budat-low  TO w_budat_s,
           s_budat-low  TO w_budat_e.
  ELSEIF s_budat-low IS INITIAL AND NOT s_budat-high IS INITIAL.
    MOVE : '00000000'   TO w_budat_s,
           s_budat-high TO w_budat_e.
  ELSEIF NOT s_budat-low IS INITIAL AND NOT s_budat-high IS INITIAL.
    MOVE : s_budat-low  TO w_budat_s,
           s_budat-high TO w_budat_e.
  ENDIF.

*--- s_cputm
  IF s_cputm-low IS INITIAL AND s_cputm-high IS INITIAL.
    MOVE : '000000'     TO w_cputm_s,
           '235959'     TO w_cputm_e.
  ELSEIF NOT s_cputm-low IS INITIAL AND s_cputm-high IS INITIAL.
    MOVE : s_cputm-low  TO w_cputm_s,
           s_cputm-low  TO w_cputm_e.
  ELSEIF s_cputm-low IS INITIAL AND NOT s_cputm-high IS INITIAL.
    MOVE : '000000'     TO w_cputm_s,
           s_cputm-high TO w_cputm_e.
  ELSEIF NOT s_cputm-low IS INITIAL AND NOT s_cputm-high IS INITIAL.
    MOVE : s_cputm-low  TO w_cputm_s,
           s_cputm-high TO w_cputm_e.
  ENDIF.
ENDFORM.                    " make_select_options

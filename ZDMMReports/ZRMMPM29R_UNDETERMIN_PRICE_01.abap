************************************************************************
* Program Name : ZRMMPM29R_UNDETERMIN_PRICE_01
* Created by   : Min-su Park
* Created on   : 2003.11.17.
* Pattern      :
* Description  : Undeterminated Price by Material
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.11.17.     Min-su Park    UD1K901873     Initial Coding
************************************************************************

*&---------------------------------------------------------------------*
*& Report  ZRMMPM29R_UNDETERMIN_PRICE_01                               *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  zrmmpm29r_undetermin_price_01 MESSAGE-ID zmmm.

*Tables
TABLES : marc, mara, lfa1, eine.

*ALV Definition.
TYPE-POOLS: slis.
DATA:   wa_events      TYPE slis_t_event                              ,
        w_repid LIKE sy-repid                                         ,
        wa_sort     TYPE slis_t_sortinfo_alv                          ,
        it_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE         ,
        w_formname_top_of_page TYPE slis_formname VALUE 'TOP_OF_PAGE' ,
        wa_list_top_of_page TYPE slis_t_listheader                    ,
        w_print_p TYPE slis_print_alv                                 .

DATA : w_layout   TYPE slis_layout_alv.

*Internal Table
DATA : BEGIN OF it_price OCCURS 0,
            werks LIKE marc-werks, "Plant
            matnr LIKE mara-matnr, "Material No.
            lifnr LIKE eord-lifnr, "Vendor
            ekgrp LIKE marc-ekgrp, "Purchasing Group
            maktx LIKE makt-maktx, "Material Description
            stprs LIKE mbew-stprs, "STD Price
            kbetr_z LIKE konp-kbetr, "Zero Price
            kbetr LIKE konp-kbetr, "Current Price
            konwa LIKE konp-konwa, "Currency
            kzust LIKE konh-kzust,
            erfmg LIKE mseg-erfmg, "GR QTY
            meins LIKE mseg-meins,
            name1 LIKE lfa1-name1, "Vendor Description
            eknam LIKE t024-eknam, "Person
            datab LIKE a018-datab, "Valid on Date
            erdat LIKE konh-erdat, "Created Date
        END OF it_price.

RANGES : r_bwart_plus FOR mseg-bwart,
         r_bwart_minus FOR mseg-bwart.

DATA : txt1(40).

*---
*Select-options
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_werks FOR marc-werks,  "Plant
                 s_ekorg FOR eine-ekorg,  "Purchase Organization
*                 S_MTART FOR MARA-MTART,  "Material Type
                 s_ekgrp FOR marc-ekgrp.  "Purchase Group
PARAMETERS : p_lifnr LIKE lfa1-lifnr OBLIGATORY. "Vendor
SELECT-OPTIONS : s_matnr FOR mara-matnr . "Material Number
SELECTION-SCREEN END OF BLOCK block1.


INITIALIZATION.
  s_ekorg-sign = 'I'. s_ekorg-option = 'EQ'. s_ekorg-low = 'PU01'.
  APPEND s_ekorg.

AT SELECTION-SCREEN.
  SELECT SINGLE name1 INTO txt1
                      FROM lfa1
                     WHERE lifnr = p_lifnr
                       AND loevm <> 'X'.

START-OF-SELECTION.
  PERFORM get_data.
  PERFORM alv_field_build.
  PERFORM alv_function.





*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
*---
  TABLES : eord.

  DATA : erdat LIKE konh-erdat,
         kzust LIKE konh-kzust,
         w_in  LIKE mseg-erfmg,
         w_out LIKE mseg-erfmg,
         w_ok                 .

  DATA : it_a017 LIKE a017 OCCURS 0 WITH HEADER LINE.

  SELECT * FROM zvmm_new_sprice
           INTO CORRESPONDING FIELDS OF TABLE it_price
          WHERE werks IN s_werks
*           AND EKORG IN S_EKORG
            AND mtart = 'ROH'
            AND ekgrp IN s_ekgrp
*           AND LIFNR = P_LIFNR
            AND matnr IN s_matnr
            AND lvorm  <> 'X'
            AND lvorm1 <> 'X'
            AND lvorm2 <> 'X'.

  SORT it_price.

*---
  PERFORM set_movement_type.

*---
  LOOP AT it_price .
    CLEAR w_ok.
*Check Source List
    SELECT SINGLE *
             FROM eord
            WHERE matnr = it_price-matnr
              AND werks = it_price-werks
              AND lifnr = p_lifnr.
    IF sy-subrc <> 0.
      DELETE it_price. CONTINUE.
    ENDIF.
*Get Vendor Name
    it_price-lifnr = p_lifnr.
*    SELECT SINGLE NAME1
*             INTO IT_PRICE-NAME1
*             FROM LFA1
*            WHERE LIFNR = P_LIFNR
*              AND LOEVM <> 'X'.
*Check Info Record
*    SELECT * FROM a017
*             INTO CORRESPONDING FIELDS OF TABLE it_a017
*            WHERE kschl = 'PB00'
*              AND lifnr = p_lifnr
*              AND matnr = it_price-matnr
*              AND ekorg IN s_ekorg
*              AND werks = it_price-werks
*              AND datab <= sy-datum.
*    IF sy-subrc <> 0.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_a017
             FROM a018
            WHERE kappl = 'M'
              AND kschl = 'PB00'
              AND lifnr = p_lifnr
              AND matnr = it_price-matnr
              AND ekorg IN s_ekorg
              AND esokz EQ '0'
              AND datab <= sy-datum.
*    ENDIF.

    SORT it_a017 BY datab DESCENDING.

    LOOP AT it_a017.
      SELECT SINGLE erdat kzust
               INTO (erdat, kzust)
               FROM konh
              WHERE knumh = it_a017-knumh.
      IF sy-subrc = 0 AND kzust+0(1) = 'X'.
        w_ok = 'X'.
        EXIT.
      ELSE.
        CLEAR it_a017.
      ENDIF.
    ENDLOOP.

    IF w_ok <> 'X'.
      DELETE it_price. CONTINUE.
    ENDIF.

*Purchasing Group
    SELECT SINGLE ekgrp INTO it_price-ekgrp
                        FROM zvmm_inforecord
                       WHERE matnr = it_a017-matnr.
*Person
    SELECT SINGLE eknam INTO it_price-eknam
                        FROM t024
                       WHERE ekgrp = it_price-ekgrp.
*Current Price
    SELECT SINGLE kbetr
                  konwa
                  kmein
                        INTO (it_price-kbetr, it_price-konwa,
                              it_price-meins)
                        FROM konp
                       WHERE knumh = it_a017-knumh
                         AND kappl = 'M'
                         AND kschl = 'PB00'.
*Valid on date, Created Date
    it_price-kzust = kzust.
    it_price-datab = it_a017-datab.
    it_price-erdat = erdat.
*GR Qty
    CLEAR : w_in, w_out.
    SELECT SUM( erfmg ) INTO w_in
                        FROM mseg AS msg INNER JOIN mkpf AS mkp
                          ON msg~mblnr = mkp~mblnr
                         AND msg~mjahr = mkp~mjahr
                       WHERE msg~matnr = it_price-matnr
                         AND msg~bwart IN r_bwart_plus
*                         AND msg~bwart = '101'
                         AND msg~lifnr = p_lifnr
                         AND mkp~budat >= it_a017-datab.

    SELECT SUM( erfmg ) INTO w_out
                        FROM mseg AS msg INNER JOIN mkpf AS mkp
                          ON msg~mblnr = mkp~mblnr
                         AND msg~mjahr = mkp~mjahr
                       WHERE msg~matnr = it_price-matnr
                         AND msg~bwart IN r_bwart_minus
*                         AND msg~bwart = '102'
                         AND msg~lifnr = p_lifnr
                         AND mkp~budat >= it_a017-datab.

    it_price-erfmg = w_in - w_out.
    MOVE : txt1 TO it_price-name1.
    MODIFY it_price.
  ENDLOOP.

  SORT it_price BY werks matnr lifnr ekgrp.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  ALV_FIELD_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_field_build.
  w_repid = sy-repid.
  CLEAR : it_fieldcat[], wa_events[], wa_list_top_of_page[].
  PERFORM fieldcat_init  USING it_fieldcat[].
  PERFORM eventtab_build USING wa_events[].
  PERFORM comment_build  USING wa_list_top_of_page[].
ENDFORM.                    " ALV_FIELD_BUILD
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM fieldcat_init USING rt_fieldcat TYPE slis_t_fieldcat_alv.
  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  DATA: pos TYPE i.
*--- Plant
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'WERKS'.
  ls_fieldcat-ref_fieldname = ''.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Plant'.
  ls_fieldcat-seltext_m     = 'Plant'.
  ls_fieldcat-seltext_s     = 'Plant'.
  ls_fieldcat-outputlen     = '4'.
  ls_fieldcat-no_out        = ''.
  ls_fieldcat-datatype      = 'CHAR'.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Material Number
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'MATNR'.
  ls_fieldcat-ref_fieldname = ''.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Material No'.
  ls_fieldcat-seltext_m     = 'Material No'.
  ls_fieldcat-seltext_s     = 'Material No'.
  ls_fieldcat-outputlen     = '18'.
  ls_fieldcat-no_out        = ''.
  ls_fieldcat-datatype      = 'CHAR'.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Material Description
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'MAKTX'.
  ls_fieldcat-ref_fieldname = ''.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Material Description'.
  ls_fieldcat-seltext_m     = 'Material Description'.
  ls_fieldcat-seltext_s     = 'Material Description'.
  ls_fieldcat-outputlen     = '30'.
  ls_fieldcat-no_out        = ''.
  ls_fieldcat-datatype      = 'CHAR'.
  APPEND ls_fieldcat TO  rt_fieldcat.

*STD Price
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'STPRS'.
  ls_fieldcat-ref_fieldname = ''.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = 'KONWA'.
  ls_fieldcat-seltext_l     = 'STD Price'.
  ls_fieldcat-seltext_m     = 'STD Price'.
  ls_fieldcat-seltext_s     = 'STD Price'.
  ls_fieldcat-outputlen     = '11'.
  ls_fieldcat-no_out        = ''.
  ls_fieldcat-datatype      = 'CURR'.
  APPEND ls_fieldcat TO  rt_fieldcat.

**Zero Price
*  clear ls_fieldcat.
*  POS = POS + 1.
*  LS_FIELDCAT-COL_POS       = POS.
*  LS_FIELDCAT-FIELDNAME     = 'KBETR_Z'.
*  LS_FIELDCAT-REF_FIELDNAME = ''.
*  LS_FIELDCAT-KEY           = ''.
*  LS_FIELDCAT-QFIELDNAME    = ''.
*  LS_FIELDCAT-CFIELDNAME    = 'KONWA'.
*  LS_FIELDCAT-SELTEXT_L     = 'Zero Price'.
*  LS_FIELDCAT-SELTEXT_M     = 'Zero Price'.
*  LS_FIELDCAT-SELTEXT_S     = 'Zero Price'.
*  LS_FIELDCAT-OUTPUTLEN     = '11'.
*  LS_FIELDCAT-NO_OUT        = ''.
*  ls_fieldcat-datatype      = 'CURR'.
*  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*Current Price
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'KBETR'.
  ls_fieldcat-ref_fieldname = ''.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = 'KONWA'.
  ls_fieldcat-seltext_l     = 'Current Price'.
  ls_fieldcat-seltext_m     = 'Current Price'.
  ls_fieldcat-seltext_s     = 'Current Price'.
  ls_fieldcat-outputlen     = '11'.
  ls_fieldcat-no_out        = ''.
  ls_fieldcat-datatype      = 'CURR'.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Currency
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'KONWA'.
  ls_fieldcat-ref_fieldname = ''.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Currency'.
  ls_fieldcat-seltext_m     = 'Currency'.
  ls_fieldcat-seltext_s     = 'Currency'.
  ls_fieldcat-outputlen     = '5'.
  ls_fieldcat-no_out        = ''.
  ls_fieldcat-datatype      = 'CUKY'.
  APPEND ls_fieldcat TO  rt_fieldcat.

*GR Quantity
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'ERFMG'.
  ls_fieldcat-ref_fieldname = ''.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = 'MEINS'.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'GR Quantity'.
  ls_fieldcat-seltext_m     = 'GR Quantity'.
  ls_fieldcat-seltext_s     = 'GR Quantity'.
  ls_fieldcat-outputlen     = '15'.
  ls_fieldcat-no_out        = ''.
  ls_fieldcat-datatype      = 'QUAN'.
  APPEND ls_fieldcat TO  rt_fieldcat.

*UM
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'MEINS'.
  ls_fieldcat-ref_fieldname = ''.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'UOM'.
  ls_fieldcat-seltext_m     = 'UOM'.
  ls_fieldcat-seltext_s     = 'UOM'.
  ls_fieldcat-outputlen     = '3'.
  ls_fieldcat-no_out        = ''.
  ls_fieldcat-datatype      = 'UNIT'.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Vendor
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'LIFNR'.
  ls_fieldcat-ref_fieldname = ''.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Vendor'.
  ls_fieldcat-seltext_m     = 'Vendor'.
  ls_fieldcat-seltext_s     = 'Vendor'.
  ls_fieldcat-outputlen     = '10'.
  ls_fieldcat-no_out        = ''.
  ls_fieldcat-datatype      = 'CHAR'.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Vendor Description
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'NAME1'.
  ls_fieldcat-ref_fieldname = ''.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Vendor Description'.
  ls_fieldcat-seltext_m     = 'Vendor Description'.
  ls_fieldcat-seltext_s     = 'Vendor Description'.
  ls_fieldcat-outputlen     = '35'.
  ls_fieldcat-no_out        = ''.
  ls_fieldcat-datatype      = 'CHAR'.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Reason Code
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'KZUST'.
  ls_fieldcat-ref_fieldname = ''.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Reason Code'.
  ls_fieldcat-seltext_m     = 'Reason Code'.
  ls_fieldcat-seltext_s     = 'Reason Code'.
  ls_fieldcat-outputlen     = '4'.
  ls_fieldcat-no_out        = ''.
  ls_fieldcat-datatype      = 'CHAR'.
  APPEND ls_fieldcat TO  rt_fieldcat.


*Purchasing Group
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'EKGRP'.
  ls_fieldcat-ref_fieldname = ''.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Purchasing Group'.
  ls_fieldcat-seltext_m     = 'Purchasing Group'.
  ls_fieldcat-seltext_s     = 'Purchasing Group'.
  ls_fieldcat-outputlen     = '3'.
  ls_fieldcat-no_out        = ''.
  ls_fieldcat-datatype      = 'CHAR'.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Person
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'EKNAM'.
  ls_fieldcat-ref_fieldname = ''.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Person'.
  ls_fieldcat-seltext_m     = 'Person'.
  ls_fieldcat-seltext_s     = 'Person'.
  ls_fieldcat-outputlen     = '18'.
  ls_fieldcat-no_out        = ''.
  ls_fieldcat-datatype      = 'CHAR'.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Valid on Date
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'DATAB'.
  ls_fieldcat-ref_fieldname = ''.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Valid on Date'.
  ls_fieldcat-seltext_m     = 'Valid on Date'.
  ls_fieldcat-seltext_s     = 'Valid on Date'.
  ls_fieldcat-outputlen     = '10'.
  ls_fieldcat-no_out        = ''.
  ls_fieldcat-datatype      = 'DATS'.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Created Date
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'ERDAT'.
  ls_fieldcat-ref_fieldname = ''.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Created Date'.
  ls_fieldcat-seltext_m     = 'Created Date'.
  ls_fieldcat-seltext_s     = 'Created Date'.
  ls_fieldcat-outputlen     = '10'.
  ls_fieldcat-no_out        = ''.
  ls_fieldcat-datatype      = 'DATS'.
  APPEND ls_fieldcat TO  rt_fieldcat.
ENDFORM.                    " FIELDCAT_INIT
*&---------------------------------------------------------------------*
*&      Form  EVENTTAB_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_EVENTS[]  text
*----------------------------------------------------------------------*
FORM eventtab_build USING lt_events TYPE slis_t_event.
  DATA: ls_event TYPE slis_alv_event.
*
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            i_list_type = 0
       IMPORTING
            et_events   = lt_events.
  READ TABLE lt_events WITH KEY name =  slis_ev_top_of_page
                           INTO ls_event.
  IF sy-subrc = 0.
    MOVE w_formname_top_of_page TO ls_event-form.
    APPEND ls_event TO lt_events.
  ENDIF.
ENDFORM.                    " EVENTTAB_BUILD
*&---------------------------------------------------------------------*
*&      Form  COMMENT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_LIST_TOP_OF_PAGE[]  text
*----------------------------------------------------------------------*
FORM comment_build USING lt_top_of_page TYPE slis_t_listheader.
  DATA: ls_line TYPE slis_listheader.
  DATA: info_txt(50).

  CLEAR ls_line.
  ls_line-typ  = 'H'.
* LS_LINE-KEY:  not used for this type
  ls_line-info = text-100.
  APPEND ls_line TO lt_top_of_page.

*Plant Selection Range Display
  CLEAR info_txt.
  info_txt+0(4)  = 'From'    .
  info_txt+5(4)  = s_werks-low .
  info_txt+10(2) = 'To'      .
  info_txt+13(4) = s_werks-high.
  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = 'Plant:'.
  ls_line-info = info_txt.
  APPEND ls_line TO lt_top_of_page.

*Purchase Organization Selection Range Display
  CLEAR info_txt.
  info_txt+0(4)  = 'From'    .
  info_txt+5(4)  = s_ekorg-low .
  info_txt+10(2) = 'To'      .
  info_txt+13(4) = s_ekorg-high.
  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = 'Pur. Organization:'.
  ls_line-info = info_txt.
  APPEND ls_line TO lt_top_of_page.

**Material type Selection Range Display
*  CLEAR INFO_TXT.
*  INFO_TXT+0(4)  = 'From'    .
*  INFO_TXT+5(4)  = S_MTART-LOW .
*  INFO_TXT+10(2) = 'To'      .
*  INFO_TXT+13(4) = S_MTART-HIGH.
*  CLEAR LS_LINE.
*  LS_LINE-TYP  = 'S'.
*  LS_LINE-KEY  = 'Material Type:'.
*  LS_LINE-INFO = INFO_TXT.
*  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*Purchase group Selection Range Display
  CLEAR info_txt.
  info_txt+0(4)  = 'From'      .
  info_txt+5(4)  = s_ekgrp-low .
  info_txt+10(2) = 'To'        .
  info_txt+13(4) = s_ekgrp-high.
  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = 'Purchase Group:'.
  ls_line-info = info_txt.
  APPEND ls_line TO lt_top_of_page.

*Vendor Selection Range Display
  CLEAR info_txt.
*  INFO_TXT+0(4)  = 'From'      .
  info_txt+5(4)  = p_lifnr      .
*  INFO_TXT+10(2) = 'To'        .
*  INFO_TXT+13(4) = S_EKGRP-HIGH.
  CLEAR ls_line                 .
  ls_line-typ  = 'S'            .
  ls_line-key  = 'Vendor:'      .
  ls_line-info = info_txt       .
  APPEND ls_line TO lt_top_of_page.

*Material Selection Range Display
  CLEAR info_txt.
  info_txt+0(4)  = 'From'      .
  info_txt+5(18) = s_matnr-low .
  info_txt+24(2) = 'To'        .
  info_txt+27(18) = s_matnr-high.
  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = 'Material:'.
  ls_line-info = info_txt.
  APPEND ls_line TO lt_top_of_page.

ENDFORM.                    " COMMENT_BUILD
*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM top_of_page.
*
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
*           i_logo             = 'HTMLCNTL_TESTHTM2_SAPLOGO'
*           I_LOGO             = 'ENJOYSAP_LOGO'
            it_list_commentary = wa_list_top_of_page.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_FUNCTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_function.
*---
  w_repid = sy-repid.

  w_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program = w_repid
            is_layout          = w_layout
            it_events          = wa_events[]
            it_fieldcat        = it_fieldcat[]
       TABLES
            t_outtab           = it_price.
ENDFORM.                    " ALV_FUNCTION

*&---------------------------------------------------------------------*
*&      Form  set_movement_type
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_movement_type.
*---
  CLEAR : r_bwart_plus, r_bwart_plus[], r_bwart_minus, r_bwart_minus[].

  PERFORM append_mvt_type_plus USING : '101',
                                       '511'.

  PERFORM append_mvt_type_minus USING : '102',
                                        '122',
                                        '932',
                                        '512'.
ENDFORM.                    " set_movement_type

*&---------------------------------------------------------------------*
*&      Form  append_mvt_type_plus
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1544   text
*----------------------------------------------------------------------*
FORM append_mvt_type_plus USING    p_value.
*---
  MOVE : 'I'     TO r_bwart_plus-sign,
         'EQ'    TO r_bwart_plus-option,
         p_value TO r_bwart_plus-low.
  APPEND r_bwart_plus.
ENDFORM.                    " append_mvt_type_plus

*&---------------------------------------------------------------------*
*&      Form  append_mvt_type_minus
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1552   text
*----------------------------------------------------------------------*
FORM append_mvt_type_minus USING    p_value.
*---
  MOVE : 'I'     TO r_bwart_minus-sign,
         'EQ'    TO r_bwart_minus-option,
         p_value TO r_bwart_minus-low.
  APPEND r_bwart_minus.
ENDFORM.                    " append_mvt_type_minus

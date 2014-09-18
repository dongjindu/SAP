************************************************************************
* Program name : ZEMMPM23E_ERRORLOG
* Created by   : Min-su Park
* Created on   : 2003.11.03.
* Pattern      :
* Description  :
*   1. Modified Welcome screen-For Inbound delivery-Putaway process    *
*   2. Empty Container management                                      *
*                                                                      *
* Modification Log                                                     *
* Date            Developer        Request No.    Description          *
* 2003.11.03.     Min-su Park      UD1K901873     Initial Coding       *
*                                                                      *
************************************************************************

*&---------------------------------------------------------------------*
*& Include ZEMMPM23E_ERRORLOGTOP                                       *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM  zemmpm23e_errorlog            .

TABLES : ztmm_ct_errlog,
         vbuk,
         likp.

*ALV Definition.
TYPE-POOLS: slis.
DATA:   wa_events   TYPE slis_t_event       ,
        w_repid LIKE sy-repid               ,
        wa_sort     TYPE slis_t_sortinfo_alv,
        it_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
        w_formname_top_of_page TYPE slis_formname VALUE 'TOP_OF_PAGE',
        wa_list_top_of_page TYPE slis_t_listheader.

DATA : w_layout   TYPE slis_layout_alv.

DATA : c_green(4) VALUE 'C510',
       c_red(4) VALUE 'C610'.


*Internal Table.
DATA : BEGIN OF it_error OCCURS 0.
        INCLUDE STRUCTURE ztmm_ct_errlog.
DATA :   linecolor(4),     " ALV Color
        END OF it_error.

DATA : it_ztmm_error LIKE ztmm_ct_errlog OCCURS 0 WITH HEADER LINE.

*---
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_cntain FOR ztmm_ct_errlog-zcontainer,
                 s_erdat FOR ztmm_ct_errlog-erdat,
                 s_ernam FOR ztmm_ct_errlog-ernam,
                 s_vbeln FOR ztmm_ct_errlog-vbeln,
                 s_ebeln FOR ztmm_ct_errlog-ebeln.
SELECTION-SCREEN ULINE.
PARAMETERS : p_error AS CHECKBOX DEFAULT 'X',
             p_fixed AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK block1.

*---

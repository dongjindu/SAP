*----------------------------------------------------------------------*
*   INCLUDE ZRMMGM06_COIL_REQ_SUM_TOP                                  *
*----------------------------------------------------------------------*
TYPE-POOLS: slis.
* macro
DEFINE build_fieldcat.
  add 1 to col_pos.
  wa_fieldcat-col_pos           = col_pos.
  wa_fieldcat-fieldname         = &1.
  wa_fieldcat-ref_fieldname     = &2.
  wa_fieldcat-key               = &3.
  wa_fieldcat-qfieldname        = &4.
  wa_fieldcat-cfieldname        = &5.
  wa_fieldcat-seltext_l         = &6.
  wa_fieldcat-seltext_m         = &7.
  wa_fieldcat-seltext_s         = &8.
  wa_fieldcat-outputlen         = &9.
  append wa_fieldcat to it_fieldcat.
  clear  wa_fieldcat.
END-OF-DEFINITION.

DEFINE build_fieldcat_qty.
  add 1 to col_pos.
  wa_fieldcat-col_pos           = col_pos.
  wa_fieldcat-fieldname         = &1.
  wa_fieldcat-ref_fieldname     = &2.
  wa_fieldcat-key               = &3.
  wa_fieldcat-qfieldname        = &4.
  wa_fieldcat-cfieldname        = &5.
  wa_fieldcat-seltext_l         = &6.
  wa_fieldcat-seltext_m         = &7.
  wa_fieldcat-seltext_s         = &8.
  wa_fieldcat-outputlen         = &9.
  wa_fieldcat-decimals_out      = '0'.
  append wa_fieldcat to it_fieldcat.
  clear  wa_fieldcat.
END-OF-DEFINITION.
*/Requirement date
DATA: w_reqdate TYPE sy-datum.

* tables
TABLES : t001w,
         lfa1,
         plaf,
         eban,
         ekko,
         ekpo,
         mara,
         marc,
         makt.


* itab
DATA :
       BEGIN  OF it_plan OCCURS 0,
          matnr  LIKE  plaf-matnr,
          gsmng  LIKE  plaf-gsmng,
          meins  LIKE  plaf-meins,
          pedtr  LIKE  plaf-pedtr,
       END    OF it_plan,

       BEGIN  OF it_pr  OCCURS 0,
          matnr  LIKE  eban-matnr,
          menge  LIKE  eban-menge,
          bsmng  LIKE  eban-bsmng,
          meins  LIKE  eban-meins,
          lfdat  LIKE  eban-lfdat,
       END    OF it_pr,

       BEGIN  OF it_po  OCCURS 0,
          matnr  LIKE  ekpo-matnr,
          menge  LIKE  eket-menge,
          wemng  LIKE  eket-wemng,
          meins  LIKE  ekpo-meins,
          eindt  LIKE  eket-eindt,
       END    OF it_po,

       BEGIN  OF it_list  OCCURS 0,
          matnr    LIKE  mara-matnr,
          meins    LIKE  mseg-meins,
          menge00  LIKE  mseg-menge,
          menge01  LIKE  mseg-menge,
          menge02  LIKE  mseg-menge,
          menge03  LIKE  mseg-menge,
          menge04  LIKE  mseg-menge,
          menge05  LIKE  mseg-menge,
          menget   LIKE  mseg-menge,
       END    OF it_list,
       it_list_p  LIKE TABLE OF it_list WITH HEADER LINE,
       it_list_r  LIKE TABLE OF it_list WITH HEADER LINE,
       it_list_o  LIKE TABLE OF it_list WITH HEADER LINE.

* gv data
DATA :
       w_date_f       LIKE  sy-datum,
       w_date_e       LIKE  sy-datum,

       w_week         LIKE  scal-week,
       w_week00       LIKE  scal-week,
       w_week01       LIKE  scal-week,
       w_week02       LIKE  scal-week,
       w_week03       LIKE  scal-week,
       w_week04       LIKE  scal-week,
       w_week05       LIKE  scal-week,

       w_mon          LIKE  sy-datum,
       w_mon00        LIKE  sy-datum,
       w_mon01        LIKE  sy-datum,
       w_mon02        LIKE  sy-datum,
       w_mon03        LIKE  sy-datum,
       w_mon04        LIKE  sy-datum,
       w_mon05        LIKE  sy-datum,

* alv Definition
       w_fieldcat     TYPE   slis_t_fieldcat_alv,
       wa_fieldcat    LIKE   LINE OF w_fieldcat,
       wa_events      TYPE   slis_t_event,
       it_fieldcat    TYPE   slis_t_fieldcat_alv
                             WITH HEADER LINE,
       it_list_top_of_page
                      TYPE   slis_t_listheader,
       c_formname_top_of_page
                      TYPE   slis_formname VALUE 'TOP_OF_PAGE',
       w_repid        LIKE   sy-repid,
       col_pos        TYPE   i.


*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME.
PARAMETERS     :
*                 p_perkz   LIKE marc-perkz
*                                DEFAULT 'M' OBLIGATORY,
                 p_perkz   TYPE zperkz
                                DEFAULT 'M' OBLIGATORY,
                 p_date    LIKE sy-datum
                                DEFAULT sy-datum OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK bl1.

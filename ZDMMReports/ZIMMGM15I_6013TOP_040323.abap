*----------------------------------------------------------------------*
*   INCLUDE ZIMMGM15I_6013_TOP
*----------------------------------------------------------------------*
TYPE-POOLS: slis.
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
          plant  LIKE  plaf-plwrk,
          gsmng  LIKE  plaf-gsmng,
          meins  LIKE  plaf-meins,
          pedtr  LIKE  plaf-pedtr,
       END    OF it_plan,

       BEGIN  OF it_pr  OCCURS 0,
          matnr  LIKE  eban-matnr,
          plant  LIKE  eban-werks,
          menge  LIKE  eban-menge,
          bsmng  LIKE  eban-bsmng,
          meins  LIKE  eban-meins,
          lfdat  LIKE  eban-lfdat,
       END    OF it_pr,

       BEGIN  OF it_po  OCCURS 0,
          matnr  LIKE  ekpo-matnr,
          plant  LIKE  ekpo-werks,
          menge  LIKE  eket-menge,
          wemng  LIKE  eket-wemng,
          meins  LIKE  ekpo-meins,
          eindt  LIKE  eket-eindt,
       END    OF it_po,

       BEGIN  OF it_list  OCCURS 0,
          matnr    LIKE  mara-matnr,
          plant    LIKE  marc-werks,
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
*       w_weekdate_f   LIKE  sy-datum,  "Week  Date First
*       w_weekdate_e   LIKE  sy-datum,  "Week  Date End
*       w_monthdate_f  LIKE  sy-datum,  "Month Date First
*       w_monthdate_e  LIKE  sy-datum,  "Month Date End

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
       w_repid        LIKE   sy-repid.
*       col_pos        TYPE   i.

*/ Itab & wa for ZTMM_6013_01
DATA: BEGIN OF wa_ztmm_6013_01.
        INCLUDE STRUCTURE ztmm_6013_01.
DATA: END OF wa_ztmm_6013_01.

DATA: it_ztmm_6013_01 LIKE TABLE OF wa_ztmm_6013_01.
FIELD-SYMBOLS: <fs_ztmm_6013_01> LIKE LINE OF it_ztmm_6013_01.

**** Constants&Vars for Number range object ****************************
CONSTANTS: c_nro_nr_00   VALUE '00' LIKE inri-nrrangenr. "Header Part
CONSTANTS: c_nro_nr_01   VALUE '01' LIKE inri-nrrangenr. "Item Part
CONSTANTS: c_nro_nr_09   VALUE '09' LIKE inri-nrrangenr. "App. Doc no.
* Number range object
DATA:      w_nro_object  VALUE 'ZMMNRO0002'
                               LIKE inri-object. "NRO for ZBDCMSGCOLL
* Number_Get_Next
DATA:      w_nro_number  TYPE num10.      " Same type of nro_object

DATA: w_zdocno TYPE num10.       "App. Doc. No.

*/For interface log
DATA: wa_ztca_if_log LIKE ztca_if_log.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME.
PARAMETERS     : p_perkz   LIKE marc-perkz
                                DEFAULT 'M' OBLIGATORY,
                 p_date    LIKE sy-datum
                                DEFAULT sy-datum OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK bl1.

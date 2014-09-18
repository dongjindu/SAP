*----------------------------------------------------------------------*
*   INCLUDE ZIMMGM09_HMMA_STOCK_TOP                                    *
*----------------------------------------------------------------------*
TYPE-POOLS: slis.


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

TABLES :
         mkpf,
         mseg,
         ztmm_roh_stock,
         ztca_if_log.

DATA  :
        BEGIN OF it_basic OCCURS 0,
           werks  LIKE  mseg-werks,
           lgort  LIKE  mseg-lgort,
           bwart  LIKE  mseg-bwart,
           matnr  LIKE  mseg-matnr,
           lifnr  LIKE  mseg-lifnr,
           charg  LIKE  mseg-charg,
           erfmg  LIKE  mseg-erfmg,
           erfme  LIKE  mseg-erfme,
        END   OF it_basic,

        BEGIN OF it_stock OCCURS 0,
           werks  LIKE  mseg-werks,
           lgort  LIKE  mseg-lgort,
           bwart  LIKE  mseg-bwart,
           matnr  LIKE  mseg-matnr,
           clabs  LIKE  mchb-clabs,
           cinsm  LIKE  mchb-cinsm,
           bdmng  LIKE  mseg-erfmg,
        END   OF it_stock,

        it_list     LIKE  TABLE OF ztmm_roh_stock
                                 WITH HEADER LINE,

        it_return   LIKE  TABLE OF bapiret2
                                 WITH HEADER LINE.

DATA : wa_zsmm_class  LIKE  zsmm_class.

DATA:   wa_events      TYPE  slis_t_event,
        it_fieldcat    TYPE  slis_t_fieldcat_alv WITH HEADER LINE,
        wa_list_top_of_page
                       TYPE  slis_t_listheader,
        wa_fieldcat    LIKE  LINE OF it_fieldcat,
        c_formname_top_of_page
                       TYPE  slis_formname VALUE 'TOP_OF_PAGE',
        w_repid        LIKE  sy-repid,
        col_pos        TYPE  i.

DATA : w_total        LIKE ztca_if_log-total,  "total cnt in interface
       w_succ         LIKE ztca_if_log-zsucc,  "success cnt in interface
       wa_ztca_if_log LIKE ztca_if_log.


CONSTANTS: c_objecttable_imp   LIKE  bapi1003_key-objecttable
                                  VALUE 'MARA',
           c_classtype_imp     LIKE  bapi1003_key-classtype
                                  VALUE '001',
           c_dest(10)          VALUE 'WMRM01'. "Outbound Destination


* input condition
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS  :  s_budat  FOR mkpf-budat,
                   s_bldat  FOR mkpf-bldat.
SELECTION-SCREEN END OF BLOCK b1.
* type
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-004.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETERS       rv1 RADIOBUTTON GROUP rad1 DEFAULT 'X'.
SELECTION-SCREEN POSITION 7.
SELECTION-SCREEN COMMENT 8(15) text-005 FOR FIELD rv1.
SELECTION-SCREEN POSITION 40.
PARAMETERS       rv2 RADIOBUTTON GROUP rad1.
SELECTION-SCREEN POSITION 41.
SELECTION-SCREEN COMMENT 42(25) text-006 FOR FIELD rv2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b3.
* processing
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETERS       r1 RADIOBUTTON GROUP rad2 DEFAULT 'X'.
SELECTION-SCREEN POSITION 7.
SELECTION-SCREEN COMMENT 8(10) text-002 FOR FIELD r1.
SELECTION-SCREEN POSITION 40.
PARAMETERS       r2 RADIOBUTTON GROUP rad2.
SELECTION-SCREEN POSITION 41.
SELECTION-SCREEN COMMENT 42(25) text-003 FOR FIELD r2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.

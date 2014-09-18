*----------------------------------------------------------------------*
*   INCLUDE ZRMMPM27_MATERIAL_PRICE_TOP                                *
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

* tables
TABLES : t001w,
         t024e,
         a017,
         a018,
         konh,
         konp,
         eord,
         lfa1,
         mara,
         makt,
         marc,
         t686d,
         zvmm_gr_detail.

* itab
DATA :
       it_a018  LIKE  TABLE OF a018  WITH HEADER LINE,
       wa_a018  LIKE  it_a018,

       BEGIN  OF it_list  OCCURS 0,
         index(10),
         matnr   LIKE  mara-matnr,
         werks   LIKE  t001w-werks,
         datab   LIKE  a017-datab,     "Validity start date
         kosrt   LIKE  konh-kosrt,     "approval no
         kzust   LIKE  konh-kzust,     "Reason Code
         vtext   LIKE  t686d-vtext,    "Reason Code text
         kbetr   LIKE  konp-kbetr,     "Rate
         konwa   LIKE  konp-konwa,     "unit of Rate
         erdat   LIKE  konh-erdat,     "create date
         erfme   LIKE  mseg-erfme,
         erfmg_v LIKE  mseg-erfmg,    "LP gr
         erfmg_k LIKE  mseg-erfmg,    "KD gr
         lifnr   LIKE  lfa1-lifnr,     "vendor
         name1   LIKE  lfa1-name1,
       END    OF it_list,

       BEGIN  OF it_mseg OCCURS 0,
         werks    LIKE  mseg-werks,
         matnr    LIKE  mseg-matnr,
         datab    LIKE  a017-datab,
         erfme    LIKE  mseg-erfme,
         erfmg_v  LIKE  mseg-erfmg,
         erfmg_k  LIKE  mseg-erfmg,
       END    OF it_mseg,

       wa_mseg  LIKE  it_mseg,

       BEGIN  OF it_nextvd OCCURS 0,  "next vendor
         lifnr  LIKE  lfa1-lifnr,
         name1  LIKE  lfa1-name1,
       END    OF it_nextvd,
       wa_nextvd LIKE it_nextvd.


DATA : BEGIN  OF it_eord OCCURS 0.
        INCLUDE STRUCTURE eord.
DATA :   name1  LIKE  lfa1-name1,
       END    OF it_eord.

* gv data
DATA :
       w_mtart_flag(1),
       w_fname TYPE fname,
       w_fname1 TYPE fname,
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

* field symbols
FIELD-SYMBOLS: <field>, <field1>.

* ALV set
DATA :ok_code      LIKE sy-ucomm,
      g_container  TYPE scrfname VALUE 'ALV_GRID',
      grid1        TYPE REF TO cl_gui_alv_grid,
      g_custom_container
                   TYPE REF TO cl_gui_custom_container.

* selection screen
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME.
PARAMETERS     :
*                 p_werks   LIKE t001w-werks
*                                DEFAULT 'P001' OBLIGATORY,
                 p_ekorg   LIKE t024e-ekorg
                                DEFAULT 'PU01' OBLIGATORY,
                 p_matnr   LIKE mara-matnr OBLIGATORY.
SELECT-OPTIONS : s_lifnr   FOR  lfa1-lifnr.
SELECTION-SCREEN END   OF BLOCK bl1.

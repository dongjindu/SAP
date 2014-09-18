*----------------------------------------------------------------------*
*   INCLUDE ZACO92A_CREAT_ROUTING_TOP                                  *
*----------------------------------------------------------------------*
TABLES :keko, cosl, plaf, mara.
FIELD-SYMBOLS: <f_field>, <f_field2>.

DATA : it_cosl_temp LIKE cosl OCCURS 0 WITH HEADER LINE.
DATA : BEGIN OF it_cosl OCCURS 0 ,
        lednr    LIKE cosl-lednr,
        objnr    LIKE cosl-objnr,
        kostl    TYPE kostl,
        gjahr    LIKE cosl-gjahr,
        wrttp    LIKE cosl-wrttp,
        versn    LIKE cosl-versn,
        vrgng    LIKE cosl-vrgng,
        perbl    LIKE cosl-perbl,
        kap001   LIKE cosl-kap001,
        kap002   LIKE cosl-kap002,
        kap003   LIKE cosl-kap003,
        kap004   LIKE cosl-kap004,
        kap005   LIKE cosl-kap005,
        kap006   LIKE cosl-kap006,
        kap007   LIKE cosl-kap007,
        kap008   LIKE cosl-kap008,
        kap009   LIKE cosl-kap009,
        kap010   LIKE cosl-kap010,
        kap011   LIKE cosl-kap011,
        kap012   LIKE cosl-kap012,
        mh001    LIKE cosl-kap001,
        mh002    LIKE cosl-kap002,
        mh003    LIKE cosl-kap003,
        mh004    LIKE cosl-kap004,
        mh005    LIKE cosl-kap005,
        mh006    LIKE cosl-kap006,
        mh007    LIKE cosl-kap007,
        mh008    LIKE cosl-kap008,
        mh009    LIKE cosl-kap009,
        mh010    LIKE cosl-kap010,
        mh011    LIKE cosl-kap011,
        mh012    LIKE cosl-kap012,
       END OF it_cosl .


DATA : BEGIN OF it_mara OCCURS 0,
       mtart LIKE mara-mtart,
       matnr LIKE mara-matnr,
       END OF it_mara.

DATA : BEGIN OF it_marc OCCURS 0,
       werks LIKE marc-werks,
       matnr LIKE marc-matnr,
       sauft LIKE marc-sauft,
       END OF it_marc.


DATA : BEGIN OF it_rate OCCURS 0,
       matnr LIKE mara-matnr,
       END OF it_rate.

DATA : BEGIN OF it_product OCCURS 0,
       matnr LIKE mara-matnr,
       END OF it_product.



DATA : BEGIN OF it_mi OCCURS 0,
       plnnr LIKE plko-plnnr,
       matnr LIKE mara-matnr,
       END OF it_mi.

DATA : BEGIN OF it_mip OCCURS 0,
       matnr LIKE mara-matnr,
       END OF it_mip.

DATA : begin of it_plpo_temp occurs 0,
       plnty  like plpo-plnty,
       plnnr  like plpo-plnnr,
       plnkn  like plpo-plnkn,
       zaehl  like plpo-zaehl,
       datuv  like plpo-datuv,
       arbid  like plpo-arbid,
       AENNR  like plpo-AENNR,
       VORNR  like plpo-VORNR,
       WERKS  like plpo-WERKS,
       LAR01  like plpo-LAR01,
       VGE01  like plpo-VGE01,
       VGW01  like plpo-VGW01,
       LAR02  like plpo-LAR02,
       VGE02  like plpo-VGE02,
       VGW02  like plpo-VGW02,
       LAR03  like plpo-LAR03,
       VGE03  like plpo-VGE03,
       VGW03  like plpo-VGW03,
       ZGR03  like plpo-zgr03,
       end of it_plpo_temp.
*      LIKE plpo OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_plpo OCCURS 0,
        werks   like plpo-werks,
        matnr   LIKE mara-matnr,
        plnty   LIKE plpo-plnty,
        type(1),
*        plnnr   LIKE plpo-plnnr,
*        plnkn   LIKE plpo-plnkn,
*        zaehl   LIKE plpo-zaehl,
        arbid   LIKE plpo-arbid,
        vgw01   LIKE plpo-vgw01,    "Set
        vgw02   LIKE plpo-vgw02,    "Machine
        vgw03   LIKE plpo-vgw03,    "Labor
        vge01   LIKE plpo-vge01,    "Unit Set
        vge02   LIKE plpo-vge02,    "Unit Machine
        vge03   LIKE plpo-vge03,    "Unit Labor
        ZGR03   like plpo-ZGR03,
      END OF it_plpo.



DATA : BEGIN OF it_crhd OCCURS 0,
       objid LIKE crhd-objid,
       arbpl LIKE crhd-arbpl,
       END OF it_crhd.


DATA : BEGIN OF it_plaf OCCURS 0,
       perio(6),
       plwrk LIKE plaf-plwrk,
       matnr LIKE mara-matnr,
       gsmng LIKE plaf-gsmng,
       END OF it_plaf.


DATA : it_plaf_temp LIKE plaf OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF itab OCCURS 0,
       type(1),
       werks LIKE plaf-plwrk,
       matnr LIKE mara-matnr,
       kostl TYPE kostl,
       perio(6),
       eng_mh LIKE it_plaf-gsmng,
       cc_rate TYPE p DECIMALS 6,
       abp_mh LIKE it_plaf-gsmng,
       vgw01   LIKE plpo-vgw01,    "Set
       vgw02   LIKE plpo-vgw02,    "Machine
       vgE01   LIKE plpo-vgE01,    "Unit set
       vgE02  LIKE plpo-vgE02,     "Unit machine
       vgE03   LIKE plpo-vgE03,    "Unit Mh
       END OF itab .

DATA : BEGIN OF it_rout OCCURS 0,
       werks LIKE plaf-plwrk,
       matnr LIKE mara-matnr,
       type(1),
       bdc_type(1),
       kostl  TYPE kostl,
       abptot LIKE cosl-kap001,
       abp001 LIKE cosl-kap001,
       abp002 LIKE cosl-kap002,
       abp003 LIKE cosl-kap003,
       abp004 LIKE cosl-kap004,
       abp005 LIKE cosl-kap005,
       abp006 LIKE cosl-kap006,
       abp007 LIKE cosl-kap007,
       abp008 LIKE cosl-kap008,
       abp009 LIKE cosl-kap009,
       abp010 LIKE cosl-kap010,
       abp011 LIKE cosl-kap011,
       abp012 LIKE cosl-kap012,
       set001 LIKE cosl-kap001,
       set002 LIKE cosl-kap002,
       set003 LIKE cosl-kap003,
       set004 LIKE cosl-kap004,
       set005 LIKE cosl-kap005,
       set006 LIKE cosl-kap006,
       set007 LIKE cosl-kap007,
       set008 LIKE cosl-kap008,
       set009 LIKE cosl-kap009,
       set010 LIKE cosl-kap010,
       set011 LIKE cosl-kap011,
       set012 LIKE cosl-kap012,
       mch001 LIKE cosl-kap001,
       mch002 LIKE cosl-kap002,
       mch003 LIKE cosl-kap003,
       mch004 LIKE cosl-kap004,
       mch005 LIKE cosl-kap005,
       mch006 LIKE cosl-kap006,
       mch007 LIKE cosl-kap007,
       mch008 LIKE cosl-kap008,
       mch009 LIKE cosl-kap009,
       mch010 LIKE cosl-kap010,
       mch011 LIKE cosl-kap011,
       mch012 LIKE cosl-kap012,
       VGE01  LIKE PLPOD-VGE01,
       VGE02  LIKE PLPOD-VGE02,
       VGE03  LIKE PLPOD-VGE03,
       END OF it_rout .

data : it_bdc_rout like it_rout occurs 0 with header line.
DATA : BEGIN OF IT_CHG_ROUT OCCURS 0,
        matnr  LIKE mapl-matnr,
        werks  type werks_d,
        PLNTY  like plko-PLNTY,
        plnnr  LIKE plko-plnnr,
        plnal  LIKE plko-plnal,
        AENNR  LIKE PLKO-AENNR,
        DATUV  LIKE PLKO-DATUV,
       END OF IT_CHG_ROUT .


DATA : BEGIN OF it_display OCCURS 0,
       werks LIKE plaf-plwrk,
       matnr LIKE mara-matnr,
       type(1),
       kostl  TYPE kostl,
       abptot LIKE cosl-kap001,
       abp001 LIKE cosl-kap001,
       abp002 LIKE cosl-kap002,
       abp003 LIKE cosl-kap003,
       abp004 LIKE cosl-kap004,
       abp005 LIKE cosl-kap005,
       abp006 LIKE cosl-kap006,
       abp007 LIKE cosl-kap007,
       abp008 LIKE cosl-kap008,
       abp009 LIKE cosl-kap009,
       abp010 LIKE cosl-kap010,
       abp011 LIKE cosl-kap011,
       abp012 LIKE cosl-kap012,
       CHK,
       END OF it_display .

DATA : G_VAL_CNT(2) TYPE N.
data : g_screen(4) ,
       g_code(4).       "Transaction code

DATA: G_startdt LIKE sy-datum.

* FOR bdc
DATA: BEGIN OF wa_result,
        matnr LIKE mara-matnr,
        werks LIKE marc-werks,
        plnnr LIKE plkod-plnnr,          "Group
        plnal LIKE plkod-plnal,          "Group counter
        datuv(10) TYPE c,                "Valid from date
        messa(100) TYPE c,
      END OF wa_result.

DATA: wa_bdcdata LIKE bdcdata,
      wa_opt     LIKE ctu_params,
      wa_msg LIKE bdcmsgcoll,
      it_msg LIKE TABLE OF wa_msg,
      it_result LIKE TABLE OF wa_result,
      it_bdcdata LIKE TABLE OF wa_bdcdata.

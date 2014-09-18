*----------------------------------------------------------------------*
*   INCLUDE ZAPP719_BF_ENGINE_TOP                                      *
*----------------------------------------------------------------------*
REPORT  zapp719_backflush_engine_tr MESSAGE-ID zmpp.

TABLES : ppc1_all,marc,t460a,mkal.

*internal table declation
DATA : BEGIN OF it_ppc1_all OCCURS 0,
        matnr LIKE ppc1_all-matnr,
        plant LIKE ppc1_all-plant,       "target plant
        prvbe LIKE ppc1_all-prvbe,
        confunit LIKE ppc1_all-confunit,
        postdate LIKE ppc1_all-postdate,
** on 03/15/12
        plant_s LIKE mkal-alort,           "source plant
** on 03/15/12
        lgort LIKE ppc1_all-lgort,       "target stl
        alort LIKE mkal-alort,           "source stl
        komp_quant LIKE ppc1_all-komp_quant,
       END OF it_ppc1_all,
       st_ppc1_all LIKE it_ppc1_all,
*Target quantity
       BEGIN OF it_quantity OCCURS 0,
        matnr LIKE ppc1_all-matnr,
        plant LIKE ppc1_all-plant,
* on 03/15/12
        plant_s LIKE mkal-alort,           "source plant
** on 03/15/12
        lgort LIKE ppc1_all-lgort,        "target  stl
        alort LIKE mkal-alort,            "
        komp_quant LIKE ppc1_all-komp_quant,
        type(1),
        text(40),
       END OF it_quantity.

DATA: itab TYPE TABLE OF bdcmsgcoll.

*data :  p_PRVBE like ppc1_all-PRVBE value 'ENG'.
DATA : w_int TYPE i,
       p_plant LIKE ppc1_all-plant VALUE 'P001',
       p_sobsl LIKE marc-sobsl VALUE '40',
*       p_wrk02 LIKE t460a-wrk02,
       gm_code LIKE bapi2017_gm_code VALUE '04',
       p_mtype(3) TYPE c VALUE '301',
       p_prvbe LIKE ppc1_all-prvbe VALUE 'ENG'.

DATA: gv_bdate like sy-datum. "begin

*&---------------------------------------------------------------------*
*& Include MZAHR0007TOP                                                *
*&                                                                     *
*&---------------------------------------------------------------------*
REPORT sapmzahr0007 MESSAGE-ID zmhr
       NO STANDARD PAGE HEADING.
*...
TABLES: zthr_ahc01,
        zthr_pcp00,
        zthr_pcp01,
        zthr_pcp02,
        zthr_pcp04,
        zthr_pcp05,
        t500p,
        cskt,
        hrp1000,
        pa0000,
        pa0001.

CONTROLS: tc9100 TYPE TABLEVIEW USING SCREEN 9100.

*... internal tables
DATA: BEGIN OF it_ahc01 OCCURS 0.
        INCLUDE STRUCTURE zthr_ahc01.
DATA: chkbx,
      ktext    LIKE cskt-ktext,
      stats,
      objid    LIKE hrp1000-objid,         " job code
      zval1    TYPE i,
      END OF it_ahc01.

DATA: it_copyt LIKE it_ahc01 OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_pcp05 OCCURS 0.
        INCLUDE STRUCTURE zthr_pcp05.
DATA: stats,
      END OF it_pcp05.

DATA: it_cpy05 LIKE it_pcp05 OCCURS 0 WITH HEADER LINE,
      it_pcp00 LIKE zthr_pcp00 OCCURS 0 WITH HEADER LINE.

DATA: it_perda LIKE person.

DATA: BEGIN OF it_persa OCCURS 1,
      werks    LIKE t500p-persa,
      name1    LIKE t500p-name1.
DATA: END OF it_persa.

DATA: BEGIN OF it_years OCCURS 1,
      zyear    LIKE zthr_pcp03-zyear.
DATA: END OF it_years.

DATA: BEGIN OF it_versn OCCURS 1,
      zvers    LIKE zthr_pcp03-zvers.
DATA: END OF it_versn.

DATA: BEGIN OF it_zscst OCCURS 1,
      zscst    LIKE zthr_ahc01-zscst,
      kname    LIKE cskt-ktext.
DATA: END OF it_zscst.

DATA: BEGIN OF it_group OCCURS 1,
      zgrup    LIKE zthr_pcp01-zgrup,
      zgtxt    LIKE zthr_pcp01-zgtxt.
DATA: END OF it_group.

DATA: BEGIN OF it_cost OCCURS 0,
      zcost    LIKE pa0001-kostl,
      END  OF it_cost.


RANGES: zjobc FOR zthr_ahc01-zjobc.

DATA: it_field     LIKE help_value OCCURS 1 WITH HEADER LINE,
      dynpfields   LIKE STANDARD TABLE OF dynpread WITH HEADER LINE.

DATA: it_units     LIKE rhldapo OCCURS 0 WITH HEADER LINE,
      it_persn     LIKE rhldapp OCCURS 0 WITH HEADER LINE,
      it_orgpn     LIKE rhldapop OCCURS 0 WITH HEADER LINE.

*... variants
DATA: w_chos1(50),
      w_chos2(50),
      w_ptext(50),
      w_flags,
      w_zdays      LIKE zthr_pcp04-day01,
      w_count      LIKE sy-tabix,
      p_flag .

DATA: w_kostl      LIKE pa0001-kostl,      " parameter ; cost center
      w_ktext      LIKE rpcyerkx-ktext,    " parameter ; cost name
      w_werks      LIKE pa0001-werks,      " parameter ; personnel area
      w_name1      LIKE t500p-name1,       " parameter ; p. area name
      w_zvers      LIKE zthr_ahc01-zvers,  " parameter ; version
      w_zyear      LIKE zthr_ahc01-zyear,  " parameter ; year
      w_zmons      LIKE zthr_ahc01-zmons,  " parameter ; month
      w_cmons      LIKE zthr_ahc01-zmons.  " parameter ; copy month

DATA: w_zscst      LIKE zthr_ahc01-zscst,  " parameter ; subcost center
      w_kname      LIKE cskt-ktext,        " cost center name
      w_zgrup      LIKE zthr_pcp01-zgrup,  " parameter ; work type
      w_zgtxt      LIKE zthr_pcp01-zgtxt.  " parameter ; work type name

DATA: w_pernr      LIKE pa0001-pernr,
      w_orgeh      LIKE t527x-orgeh,
      w_master     LIKE sy-uname.

DATA: w_fname      LIKE  help_info-fieldname,
      w_tabix      LIKE  sy-tabix,
      w_fldvl      LIKE  help_info-fldvalue.

DATA: w_topln      TYPE i VALUE 1,
      w_lstln      TYPE i VALUE 1,
      w_lopln      LIKE sy-loopc,
      w_index      LIKE sy-tabix.

DATA: w_dvalu      LIKE dd07v-domvalue_l,
      w_dtext      LIKE dd07v-ddtext,
      w_close      LIKE sy-datum.

*... define MACRO
DEFINE read_pcp04.
  clear zthr_pcp04.
  select single &1 into w_zdays
    from zthr_pcp04 where zyear = w_zyear
                      and zvers = w_zvers.
END-OF-DEFINITION.

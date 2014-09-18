*&---------------------------------------------------------------------*
*& Include MZAHR0010TOP                                                *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM sapmzahr0010 MESSAGE-ID zmhr.
*...
TABLES: pa0001,
        hrp1000,
        zthr_pcp00,
        zthr_pcp02.

CONTROLS: tc9000 TYPE TABLEVIEW USING SCREEN 9000.

*... internal tables
DATA: BEGIN OF it_pcp00 OCCURS 0,
      chkbx,
      zcost    LIKE zthr_pcp00-zcost,
      zpera    LIKE zthr_pcp00-zpera,
      zperg    LIKE zthr_pcp00-zperg,
      zsubg    LIKE zthr_pcp00-zsubg,
      zobjc    LIKE zthr_pcp00-zobjc,
      zjobk    LIKE zthr_ahc01-zjobk,
      zsenr    LIKE zthr_pcp00-zsenr,
      zhedc    LIKE zthr_pcp00-zhedc,
      zsaly    LIKE zthr_pcp00-zsaly,
      osaly    LIKE zthr_pcp00-omthly,
      zsalc    LIKE zthr_pcp00-zsaly,
      act01    LIKE zthr_pcp00-act01,
      act02    LIKE zthr_pcp00-act02,
      act03    LIKE zthr_pcp00-act03,
      ancur    LIKE zthr_pcp00-ancur,
      ktext    LIKE cskt-ktext,
      zval1    TYPE i.
DATA: END OF it_pcp00.

DATA: it_field     LIKE help_value OCCURS 1 WITH HEADER LINE.

DATA: BEGIN OF it_years OCCURS 1,
      zyear    LIKE zthr_pcp03-zyear.
DATA: END OF it_years.

DATA: BEGIN OF it_versn OCCURS 1,
      zvers    LIKE zthr_pcp03-zvers.
DATA: END OF it_versn.

DATA: BEGIN OF it_mons OCCURS 1,
        zmons LIKE zthr_month-zmons,
        ztext LIKE zthr_month-ztext,
      END OF it_mons.

*... variants
DATA: w_zyear      LIKE zthr_ahc01-zyear,  " parameter ; year
      w_zmons      LIKE zthr_ahc01-zmons,  " parameter ; month
      w_zvers      LIKE zthr_ahc01-zvers.  " parameter ; version

DATA: w_fname      LIKE  help_info-fieldname,
      w_tabix      LIKE  sy-tabix,
      w_fldvl      LIKE  help_info-fldvalue.

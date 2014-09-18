*&---------------------------------------------------------------------*
*& Include MZAHR0014TOP                                                *
*&                                                                     *
*&---------------------------------------------------------------------*
PROGRAM  sapmzahr0014                  .

TABLES: pa0001,
        pa0008,
        hrp1000,
        t500p,
        cskt,
        zthr_pcpxx,
        zthr_pcp00,
        zthr_pcp01,
        zthr_pcp02,
        zthr_pcp05.

*... internal tables
DATA: BEGIN OF it_9000 OCCURS 0,
      zcost    LIKE zthr_pcp00-zcost,  " Cost Center
      zobjc    LIKE zthr_pcpxx-zobjc,  " JOB
      zsenr    LIKE zthr_pcpxx-zsenr,
      divi(10) ,                       " Division production
      zhedc    LIKE zthr_pcp00-zhedc,  " Head Count
      zhouy    LIKE zthr_pcpxx-zhouy,  "
      annul(5) TYPE p DECIMALS 2,"    TYPE i ,
      rate1(5) TYPE p DECIMALS 2,
      amunt    LIKE zthr_pcp00-act03,
      zjobk    LIKE zthr_pcp03-zjobk,
      ktext    LIKE cskt-ktext,
      zperg    LIKE zthr_pcp00-zperg,
      zval1    TYPE i,
      zinc,
     END OF it_9000.

DATA : it_pcp00 LIKE zthr_pcp00 OCCURS 1 WITH HEADER LINE,
       it_pcp07 LIKE zthr_pcp07 OCCURS 1 WITH HEADER LINE,
       it_ahc01 LIKE zthr_ahc01 OCCURS 1 WITH HEADER LINE.

RANGES :         r_jobcode  FOR pa0001-stell.

DATA: BEGIN OF it_val OCCURS 1,
      zcode    LIKE zthr_pcp02-zcode,
      zctxt    LIKE zthr_pcp02-zctxt,
      zval1    LIKE zthr_pcp02-zval1,
      zval2    LIKE zthr_pcp02-zval2.
DATA: END OF it_val.

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

DATA: BEGIN OF it_pcpxx OCCURS 1.
        INCLUDE STRUCTURE zthr_pcpxx.
DATA: zhedc LIKE zthr_pcp00-zhedc,
      END OF it_pcpxx.

DATA: BEGIN OF it_persa OCCURS 1,
      werks    LIKE t500p-persa,
      name1    LIKE t500p-name1.
DATA: END OF it_persa.

DATA: it_field     LIKE help_value OCCURS 1 WITH HEADER LINE,
      dynpfields   LIKE STANDARD TABLE OF dynpread WITH HEADER LINE.

CONTROLS: tc_9000 TYPE TABLEVIEW USING SCREEN 9000.

*... variants
DATA: w_zyear      LIKE zthr_ahc01-zyear,  " parameter ; year
      w_zvers      LIKE zthr_ahc01-zvers,  " parameter ; version
      w_zmons      LIKE zthr_ahc01-zmons,  " parameter ; month
      w_werks      LIKE pa0001-werks,
      w_name1      LIKE t500p-name1,
      WA_SEL .

DATA: w_fname      LIKE  help_info-fieldname,
      w_tabix      LIKE  sy-tabix,
      w_fldvl      LIKE  help_info-fldvalue,
      w_save   ,
      w_input      .
DATA : s_people LIKE zthr_ahc01-zhedc.

DATA : BEGIN OF it_count OCCURS 0,
         zscst LIKE zthr_ahc01-zscst,
         zjobc LIKE zthr_ahc01-zjobc,
         zmons LIKE zthr_ahc01-zmons,
         zhedc LIKE zthr_ahc01-zhedc,
         znewc LIKE zthr_ahc01-znewc,
         zsenr LIKE zthr_ahc01-zsenr,
         zct  TYPE i,
        END OF it_count,
        it_count1 LIKE it_count OCCURS 0 WITH HEADER LINE.
DATA : BEGIN OF it_man OCCURS 0 ,
       zmons LIKE  zthr_ahc01-zmons,
       zscst LIKE  zthr_ahc01-zscst,
       zjobk LIKE  zthr_ahc01-zjobk,
       zperg LIKE  zthr_ahc01-zperg,
       znewc LIKE  zthr_ahc01-znewc,
 END OF it_man.

DATA : l_zhouy LIKE zthr_pcpxx-zhouy,
       l_ansal LIKE zthr_pcpxx-ansal,
       l_zmons LIKE zthr_pcp00-zmons.

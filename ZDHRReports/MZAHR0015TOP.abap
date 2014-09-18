*&---------------------------------------------------------------------*
*& Include MZAHR0015TOP                                                *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM  sapmzahr0015                  .

tABLES: pa0001,pa0008,hrp1000,t500p,csks,cskt,zthr_pcpxx,
        zthr_pcp00,zthr_pcp01,zthr_pcp02,zthr_pcp05.

*... internal tables
DATA: BEGIN OF it_9000 OCCURS 0,
      zcost        LIKE zthr_pcp00-zcost,  " Cost Center
      zobjc        LIKE zthr_pcpxx-zobjc,  " JOB
      zsenr        LIKE zthr_pcp00-zsenr,
      zhedc        LIKE zthr_pcp00-zhedc,  " Head Count
      zjobk        LIKE zthr_ahc01-zjobk,
      a_q_hce      LIKE zthr_pcp00-zhedc,   " e
      a_q_nhce     LIKE zthr_pcp00-zhedc,   " f
      a_q_noq      LIKE zthr_pcp00-zhedc,   " g
      a_b_hce      LIKE zthr_pcp00-zhedc,   " h
      a_b_nhce     LIKE zthr_pcp00-zhedc,   " i
      a_r_hce      TYPE p DECIMALS 2,       " j
      a_r_nhce     TYPE p DECIMALS 2,       " k
      a_a_hce      LIKE zthr_pcp00-act10,   " l
      a_a_nhce     LIKE zthr_pcp00-act10,   " m
      a_p_hce      LIKE zthr_pcp00-act10,   " n
      a_p_nhce     LIKE zthr_pcp00-act10,   " o
      a_ar_hce     TYPE p DECIMALS 2,       " p
      a_ar_nhce    TYPE p DECIMALS 2,       " q
      p_q_hce      LIKE zthr_pcp00-zhedc,   " r
      p_q_nhce     LIKE zthr_pcp00-zhedc,   " s
      p_q_noq      LIKE zthr_pcp00-zhedc,   " t
      p_b_hce      TYPE p DECIMALS 2,        " u
      p_b_nhce     TYPE p DECIMALS 2,       " v
      p_r_hce      TYPE p DECIMALS 2,       " w
      p_r_nhce     TYPE p DECIMALS 2,       " x
      p_a_hce      LIKE zthr_pcp00-act10,   " y
      p_a_nhce     LIKE zthr_pcp00-act10,   " z
      p_p_hce      LIKE zthr_pcp00-act10,   " a`
      p_p_nhce     LIKE zthr_pcp00-act10,   " b`
      p_ar_hce     TYPE p DECIMALS 2,       " c
      p_ar_nhce    TYPE p DECIMALS 2,
      ktext    LIKE cskt-ktext,
      zval1    TYPE i ,
     END OF it_9000.

RANGES :         r_jobcode  FOR pa0001-stell.

DATA: BEGIN OF it_years OCCURS 1,
      zyear    LIKE zthr_pcp03-zyear.
DATA: END OF it_years.

DATA: BEGIN OF it_versn OCCURS 1,
      zvers    LIKE zthr_pcp03-zvers.
DATA: END OF it_versn.

DATA: BEGIN OF it_pcp00 OCCURS 1.
        INCLUDE STRUCTURE zthr_pcp00.
DATA: END OF it_pcp00.

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
      w_zmons      LIKE zthr_ahc01-zmons,
      w_zvers      LIKE zthr_ahc01-zvers,  " parameter ; version
      w_werks      LIKE pa0001-werks,
      w_name1      LIKE t500p-name1.



DATA: w_fname      LIKE  help_info-fieldname,
      w_tabix      LIKE  sy-tabix,
      w_fldvl      LIKE  help_info-fldvalue,
      w_save   ,
      w_input      .

*&---------------------------------------------------------------------*
*& Include MZAHR0007TOP                                                *
*&                                                                     *
*&---------------------------------------------------------------------*
REPORT sapmzahr0018 MESSAGE-ID zmhr
       NO STANDARD PAGE HEADING.
*...
TABLES: zthr_et01, zthr_et02, zthr_et03,
        t500p, cskt, hrp1000, pa0000,pa0001.

CONTROLS: tc9100 TYPE TABLEVIEW USING SCREEN 9100,
          tc9200 TYPE TABLEVIEW USING SCREEN 9200.
*... internal tables
DATA: BEGIN OF it_et03 OCCURS 0,
        chkbx,
        tabix    LIKE sy-tabix,
        zetmh1(8) TYPE n,
        zetext(40).
        INCLUDE STRUCTURE zthr_et03.
DATA : END OF it_et03.

DATA: it_perda LIKE person.
DATA : wa_et03 LIKE it_et03.

DATA: BEGIN OF it_persa OCCURS 1,
      werks    LIKE t500p-persa,
      name1    LIKE t500p-name1.
DATA: END OF it_persa.

DATA: BEGIN OF it_cost OCCURS 0,
      zcost    LIKE pa0001-kostl,
      END  OF it_cost.

DATA: it_field     LIKE help_value OCCURS 1 WITH HEADER LINE,
      dynpfields   LIKE STANDARD TABLE OF dynpread WITH HEADER LINE.

DATA: it_units     LIKE rhldapo OCCURS 0 WITH HEADER LINE,
      it_persn     LIKE rhldapp OCCURS 0 WITH HEADER LINE,
      it_orgpn     LIKE rhldapop OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF value_cost OCCURS 0,
         kostl LIKE pa0001-kostl,
         zctxt LIKE p1000-stext,
       END OF value_cost.
*... variants
DATA: w_chos1(50),
      w_chos2(50),
      w_ptext(50).
DATA: w_flags,
      w_falg_new,
      w_count      LIKE sy-tabix,
      w_int TYPE i,
      p_flag,
      w_kostl      LIKE pa0001-kostl,      " parameter ; cost center
      w_kostlt     LIKE pa0001-kostl,      " parameter ; cost center
      w_ktext      LIKE rpcyerkx-ktext,    " parameter ; cost name
      w_zyear      LIKE zthr_et03-zyear,  " parameter ; year
      w_zmons      LIKE zthr_et03-zmons,  " parameter ; month
      w_zmonst     LIKE zthr_et03-zmons,
      w_sachz      LIKE zthr_et03-sachz,
      w_zhedc      LIKE zthr_et03-zhedc,
      w_rc_head    LIKE zthr_et03-zhedc,
      w_pernr      LIKE pa0001-pernr,
      f_flag,
      w_orgeh      LIKE t527x-orgeh,
      w_master     LIKE sy-uname,
      w_status,
      ok_code LIKE sy-ucomm,
      r_a TYPE c ,
      r_b TYPE c VALUE 'X',
      save_flag ,
      wa_title(50) TYPE c,
      wa_answer TYPE c.

RANGES: r_mons FOR zthr_et03-zmons,
        r_cost FOR zthr_et03-zcost.

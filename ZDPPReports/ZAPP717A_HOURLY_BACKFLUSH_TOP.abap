*----------------------------------------------------------------------*
*   INCLUDE ZAPP717A_HOURLY_BACKFLUSH_TOP                              *
*----------------------------------------------------------------------*
REPORT  zapp717a_hourly_backflush MESSAGE-ID zmpp  .
TABLES : marc,plaf,ztpp_bfst, wpinfo.

DATA: snd_jobs TYPE i VALUE 1,
      rcv_jobs TYPE i VALUE 1,
      width TYPE i,
      excp_flag(1) TYPE c,
      taskname(4) TYPE n VALUE '0001',
      err_chk,
      z_num(2) TYPE n,
      rp18(2) TYPE c VALUE '18',
      rev(2)  TYPE c VALUE '09',
      w_int TYPE i,
      flag,
      with_cpu TYPE x VALUE 0,
      z_report(40) TYPE c VALUE '%PPC%',
      c_prog  LIKE sy-repid ,
      p_plant LIKE marc-werks VALUE 'P001',
      zval1(7) TYPE c VALUE 'LSA-NEW',
      zva12(7) TYPE c VALUE 'LSA'.

DATA  : it_bfst    LIKE ztpp_bfst OCCURS 0 WITH HEADER LINE,
        wa_bfst    LIKE ztpp_bfst.  "OCCURS 0 WITH HEADER LINE,
DATA :it_bfst18  LIKE ztpp_bfst OCCURS 0 WITH HEADER LINE,
      it_bfst09  LIKE ztpp_bfst OCCURS 0 WITH HEADER LINE,
      wa_bfst18  LIKE ztpp_bfst,  "OCCURS 0 WITH HEADER LINE,
      rt_bfst    LIKE ztpp_bfst OCCURS 0 WITH HEADER LINE,
      rt_bfst09  LIKE ztpp_bfst OCCURS 0 WITH HEADER LINE,
      wt_bfst09  LIKE ztpp_bfst OCCURS 0 WITH HEADER LINE,
      rt_bfst18  LIKE ztpp_bfst OCCURS 0 WITH HEADER LINE,
      tot_bfst   LIKE ztpp_bfst OCCURS 0 WITH HEADER LINE,
      it_joblist LIKE TABLE OF tbtcjob WITH HEADER LINE.

DATA: BEGIN OF COMMON PART wp.
DATA: BEGIN OF wp_tabl OCCURS 10.
        INCLUDE STRUCTURE wpinfo.
DATA: END OF wp_tabl.
DATA: END OF COMMON PART wp.

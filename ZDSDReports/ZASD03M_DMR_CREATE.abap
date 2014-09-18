************************************************************************
* Program Name      : ZASD03M_DMR_CREATE
* Author            : jun ho choi
* Creation Date     : 2003.07.21.
* Specifications By : jun ho choi
* Pattern           : 3-3
* Development Request No : UD1K904910
* Addl Documentation:
* Description       : Create Debit Memo Request based on Reclaim
*                          result in SAP Costum Tables.
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 12/16/2004  CHRIS        UD1K913539  ADD ONE MORE FIELD ZCSEQ TO THE
*                                      OUTPUT STRUCTURE AND CHANGE THE
*                                      DATA POSTING LOGIC TO SKIP THE
*                                      ITEM LEVEL IF THE CLAIM TYPE IS
*                                      'C'.
* 05/15/2004  CHRIS                    USING TABLE ZTSD_VIN_CONV FOR
*                                      CONDTION TYPE FOR EACH MODEL
* 06/06/2005  CHRIS                    USING DIFFERENT MATERIAL NUMBER
*                                      FOR DIFFERENT CLAIM TYPE
*                                      MATERIAL NUMBER COMES FROM TABLE
*                                      ZTSD_VIN_CONV.
* 08.19.2014      Victor     T-code has been deleted for APM         *
************************************************************************
REPORT zasd03m_dmr_create NO STANDARD PAGE HEADING
                          MESSAGE-ID zmsd
                          LINE-SIZE 120
                          LINE-COUNT 90.


*
TABLES : ztsd_acl_l,
         ztsd_rec_h,
         ztsd_rec_i,
         ztsd_rec_l,
         usr01,
         vbrp,
         vbfa,
         kna1,
         knvv,
         knvk.


*
DATA : BEGIN OF bdc_tab OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA : END OF bdc_tab.

DATA : BEGIN OF mess_tab OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA : END OF mess_tab.

DATA : BEGIN OF mess_tab_multi OCCURS 0,
       msgv1 LIKE bdcmsgcoll-msgv1,
       END OF mess_tab_multi.

DATA : BEGIN OF bdc_list OCCURS 0,
       zdmno LIKE ztsd_acl_l-zdmno,
       END OF bdc_list.

DATA : BEGIN OF it_rec_noti OCCURS 0.
        INCLUDE STRUCTURE zssd_rec_noti_i.
*       ZISSN LIKE ZTSD_REC_L-ZISSN,
*       ZRCQT LIKE ZTSD_REC_L-ZRCQT,
*       ZRCTT LIKE ZTSD_REC_L-ZRCTT,
*       ZPYCR LIKE ZTSD_REC_L-ZPYCR,
DATA : END OF it_rec_noti.

DATA : BEGIN OF it_rec_noti_h OCCURS 0.
        INCLUDE STRUCTURE zssd_rec_noti_h.
DATA : END OF it_rec_noti_h.

DATA : BEGIN OF mail_addr OCCURS 0.
        INCLUDE STRUCTURE zssd_mail_addr.
DATA : END OF mail_addr.


* BAPI
DATA : BEGIN OF order_header_inx.
        INCLUDE STRUCTURE bapisdh1x.
DATA : END OF order_header_inx.

DATA : BEGIN OF return OCCURS 0.
        INCLUDE STRUCTURE bapiret2.
DATA : END OF return.
* BAPI

DATA : save_ok_code(4).

DATA : w_cnt TYPE i,
       w_result(1),
       w_result_msg(100),
       w_gubun(1), "1-FAX 2-REMI
       w_attn(20),
       w_cc(20).

DATA : w_char_11(11),
       w_numc_5(5) TYPE n.

DATA : w_zrcqt LIKE ztsd_rec_l-zrcqt,
       w_zrctt LIKE ztsd_rec_l-zrctt.

DATA : w_return LIKE sy-subrc.


DATA : BEGIN OF attafile OCCURS 0.
        INCLUDE STRUCTURE zssd_mail_atta.
DATA : END OF attafile.
DATA : BEGIN OF smtp_addr OCCURS 0.
        INCLUDE STRUCTURE zssd_mail_addr.
DATA : END OF smtp_addr.

*
TABLES : ztsd_acm_h, ztsd_acm_i,
         ztsd_vin_conv, ztsd_part_inf.

DATA : BEGIN OF rec_txt_dat OCCURS 0,
       zvend(04), " LIKE ZTSD_REC_H-ZVEND,
       zissn(07), " LIKE ZTSD_REC_H-ZISSN,
       zacln(07), " LIKE ZTSD_REC_H-ZACLN,
       zcdst(05), " LIKE ZTSD_REC_H-ZCDST,
       zcdlr(05), " LIKE ZTSD_REC_H-ZCDLR,
       zcser(06), " LIKE ZTSD_REC_H-ZCSER,
       zcseq(04), " LIKE ZTSD_REC_H-ZCSEQ,  "UD1K913539
       zcpis(08), " LIKE ZTSD_ACM_H-ZCPIS,
       zctyp(04), " LIKE ZTSD_ACM_H-ZCTYP,
       zvin(17),  " LIKE ZTSD_ACM_H-ZVIN,
       zscod(04), " LIKE ZTSD_ACM_H-ZSCOD,
       zspar(15), " LIKE ZTSD_VIN_CONV-ZSPAR,
       zprdt(08), " LIKE ZTSD_ACM_H-ZPRDT,
       zdlvy(08), " LIKE ZTSD_ACM_H-ZDLVY,
       zrpdt(08), " LIKE ZTSD_ACM_H-ZRPDT,
       zodrd(06), " LIKE ZTSD_ACM_H-ZODRD,
       zronm(10), " LIKE ZTSD_ACM_H-ZRONM,
       zpidt(08), " LIKE ZTSD_ACM_H-ZPIDT,
       zpodr(06), " LIKE ZTSD_ACM_H-ZPODR,
       zpron(10), " LIKE ZTSD_ACM_H-ZPRON,
       zcptn(15), " LIKE ZTSD_ACM_H-ZCPTN,
       zptna(30), " LIKE ZTSD_PART_INF-ZPTNA,
       zmnop(08), " LIKE ZTSD_ACM_H-ZMNOP,
       znatr(04), " LIKE ZTSD_ACM_H-ZNATR,
       zcaus(04), " LIKE ZTSD_ACM_H-ZCAUS,
       zsbla(04), " LIKE ZTSD_ACM_H-ZSBLA,
       zsblb(04), " LIKE ZTSD_ACM_H-ZSBLB,
       zrcpp(09), " LIKE ZTSD_REC_H-ZRCPP,
       zrcll(09), " LIKE ZTSD_REC_H-ZRCLL,
       zrcss(09), " LIKE ZTSD_REC_H-ZRCSS,
       zsprt(05), " LIKE ZTSD_REC_H-ZSPRT,
       zshar(05), " LIKE ZTSD_REC_H-ZSHAR,
       zline(04), " LIKE ZTSD_REC_I-ZLINE,
       zrppn(15), " LIKE ZTSD_ACM_I-ZRPPN,
       zptn2(30), " LIKE ZTSD_PART_INF-ZPTNA,
       zrmpq(04), " LIKE ZTSD_ACM_I-ZRMPQ,
       zvprc(09), " LIKE ZTSD_REC_I-ZVPRC,
       zvmup(04), " LIKE ZTSD_REC_I-ZVMUP,
       zrcp2(09), " LIKE ZTSD_REC_I-ZRCPP,
       zoper(08), " LIKE ZTSD_ACM_I-ZOPER,
       zrmlq(04), " LIKE ZTSD_ACM_I-ZRMLQ,
       zrmlt(04), " LIKE ZTSD_ACM_I-ZRMLT,
       zlrat(05), " LIKE ZTSD_ACM_I-ZLRAT,
       zrcl2(09), " LIKE ZTSD_REC_I-ZRCLL,
       END OF rec_txt_dat.

DATA : BEGIN OF rec_txt_asc OCCURS 0,
       zvend(04), " LIKE ZTSD_REC_H-ZVEND,
       zissn(07), " LIKE ZTSD_REC_H-ZISSN,
       zacln(07), " LIKE ZTSD_REC_H-ZACLN,
       zcdst(05), " LIKE ZTSD_REC_H-ZCDST,
       zcdlr(05), " LIKE ZTSD_REC_H-ZCDLR,
       zcser(06), " LIKE ZTSD_REC_H-ZCSER,
       zcseq(02), " LIKE ZTSD_REC_H-ZCSEQ,   "UD1K913539
       zcpis(08), " LIKE ZTSD_ACM_H-ZCPIS,
       zctyp(01), " LIKE ZTSD_ACM_H-ZCTYP,
       zvin(17),  " LIKE ZTSD_ACM_H-ZVIN,
       zscod(03), " LIKE ZTSD_ACM_H-ZSCOD,
       zspar(15), " LIKE ZTSD_VIN_CONV-ZSPAR,
       zprdt(08), " LIKE ZTSD_ACM_H-ZPRDT,
       zdlvy(08), " LIKE ZTSD_ACM_H-ZDLVY,
       zrpdt(08), " LIKE ZTSD_ACM_H-ZRPDT,
       zodrd(06), " LIKE ZTSD_ACM_H-ZODRD,
       zronm(10), " LIKE ZTSD_ACM_H-ZRONM,
       zpidt(08), " LIKE ZTSD_ACM_H-ZPIDT,
       zpodr(06), " LIKE ZTSD_ACM_H-ZPODR,
       zpron(10), " LIKE ZTSD_ACM_H-ZPRON,
       zcptn(15), " LIKE ZTSD_ACM_H-ZCPTN,
       zptna(30), " LIKE ZTSD_PART_INF-ZPTNA,
       zmnop(08), " LIKE ZTSD_ACM_H-ZMNOP,
       znatr(03), " LIKE ZTSD_ACM_H-ZNATR,
       zcaus(03), " LIKE ZTSD_ACM_H-ZCAUS,
       zsbla(01), " LIKE ZTSD_ACM_H-ZSBLA,
       zsblb(01), " LIKE ZTSD_ACM_H-ZSBLB,
       zrcpp(09), " LIKE ZTSD_REC_H-ZRCPP,
       zrcll(09), " LIKE ZTSD_REC_H-ZRCLL,
       zrcss(09), " LIKE ZTSD_REC_H-ZRCSS,
       zsprt(05), " LIKE ZTSD_REC_H-ZSPRT,
       zshar(05), " LIKE ZTSD_REC_H-ZSHAR,
       zline(02), " LIKE ZTSD_REC_I-ZLINE,
       zrppn(15), " LIKE ZTSD_ACM_I-ZRPPN,
       zptn2(30), " LIKE ZTSD_PART_INF-ZPTNA,
       zrmpq(02), " LIKE ZTSD_ACM_I-ZRMPQ,
       zvprc(09), " LIKE ZTSD_REC_I-ZVPRC,
       zvmup(03), " LIKE ZTSD_REC_I-ZVMUP,
       zrcp2(09), " LIKE ZTSD_REC_I-ZRCPP,
       zoper(08), " LIKE ZTSD_ACM_I-ZOPER,
       zrmlq(01), " LIKE ZTSD_ACM_I-ZRMLQ,
       zrmlt(03), " LIKE ZTSD_ACM_I-ZRMLT,
       zlrat(05), " LIKE ZTSD_ACM_I-ZLRAT,
       zrcl2(09), " LIKE ZTSD_REC_I-ZRCLL,
       END OF rec_txt_asc.

DATA : BEGIN OF it_rec_h OCCURS 0.
        INCLUDE STRUCTURE ztsd_rec_h.
DATA : END OF it_rec_h.

DATA : BEGIN OF it_rec_i OCCURS 0.
        INCLUDE STRUCTURE ztsd_rec_i.
DATA : END OF it_rec_i.

DATA : it_rec  LIKE ztsd_rec_h OCCURS 0 WITH HEADER LINE.
DATA : BEGIN OF it_recitem OCCURS 0,
         zissn   LIKE ztsd_rec_h-zissn,
         zvend   LIKE ztsd_rec_h-zvend,
         zctyp   LIKE ztsd_rec_h-zctyp,
         zmodl   LIKE ztsd_rec_h-zmodl,
         zconw   LIKE ztsd_vin_conv-zconw,
         zconc   LIKE ztsd_vin_conv-zconc,
         zpycr   LIKE ztsd_rec_h-zpycr,
         zrcpp   LIKE ztsd_rec_h-zrcpp,
         zrcll   LIKE ztsd_rec_h-zrcll,
         zrcss   LIKE ztsd_rec_h-zrcss,
         matnr   LIKE mara-matnr,
       END OF it_recitem.
DATA : it_head  LIKE it_recitem OCCURS 0 WITH HEADER LINE.
DATA : w_file LIKE rlgrap-filename.

*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_zissn FOR ztsd_rec_l-zissn NO-EXTENSION.
PARAMETERS : p_zdmfg LIKE ztsd_rec_l-zdmfg.
SELECTION-SCREEN END OF BLOCK b1.


*
AT USER-COMMAND.
  CASE sy-ucomm.
    WHEN 'EMAIL'.
      PERFORM send_word.
  ENDCASE.


*
START-OF-SELECTION.
  PERFORM read_data.


*
END-OF-SELECTION.
  PERFORM call_screen.






************************************************************************
* TYPE FOR THE DATA OF TABLECONTROL 'TC_9000'
  TYPES: BEGIN OF t_tc_9000.
          INCLUDE STRUCTURE ztsd_rec_l.
  TYPES:   flag,       "flag for mark column
         END OF t_tc_9000.

* INTERNAL TABLE FOR TABLECONTROL 'TC_9000'
  DATA:     g_tc_9000_itab   TYPE t_tc_9000 OCCURS 0,
            g_tc_9000_wa     TYPE t_tc_9000, "work area
            g_tc_9000_copied.           "copy flag

* DECLARATION OF TABLECONTROL 'TC_9000' ITSELF
  CONTROLS: tc_9000 TYPE TABLEVIEW USING SCREEN 9000.

* LINES OF TABLECONTROL 'TC_9000'
  DATA:     g_tc_9000_lines  LIKE sy-loopc.

  DATA:     ok_code LIKE sy-ucomm.

* Includes inserted by Screen Painter Wizard. DO NOT CHANGE THIS LINE!
  INCLUDE zasd03l_dmr_create_f01.
  INCLUDE zasd03l_dmr_create_pbo.
  INCLUDE zasd03l_dmr_create_pai.

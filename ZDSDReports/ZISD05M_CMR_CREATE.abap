************************************************************************
* Program Name      : ZISD05M_CMR_CREATE
* Author            : jun ho choi
* Creation Date     : 2003.07.09.
* Specifications By : jun ho choi
* Pattern           : 3-3
* Development Request No : UD1K904910
* Addl Documentation:
* Description       : Create Credit Memo Request based on ACM Data
*                          stored in SAP Costum Tables.
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 06/07/2005 chris                     credit memo created by using dif-
*                                      frent material number for each
*                                      model
* 05.19.2014 Victor
*
************************************************************************
report zisd05m_cmr_create no standard page heading
                          message-id zmsd
                          line-size 120
                          line-count 90.


*
tables : ztsd_acm_h,
         ztsd_acm_i,
         ztsd_acl_l,
         ztsd_vin_conv,
         usr01,
         vbak,
         vbrp,
         konv,
         kna1,
         knvv,
         knvk.


*
data : begin of bdc_tab occurs 0.
       include structure bdcdata.
data : end of bdc_tab.

data : begin of mess_tab occurs 0.
       include structure bdcmsgcoll.
data : end of mess_tab.

data : begin of bdc_item occurs 0,
       zacln like ztsd_acm_h-zacln,
       zcdst like ztsd_acm_h-zcdst,
       zmodl like ztsd_acm_h-zmodl,
       zscod like ztsd_acm_h-zscod,
       zpycr like ztsd_acm_h-zpycr,
       zctyp like ztsd_acm_h-zctyp,
       zcon  like ztsd_vin_conv-zconw,
       zrm   like ztsd_acm_h-zrmss,
       zrmc(11),
       matnr like ztsd_vin_conv-matnr,
       end of bdc_item.
data : it_head like bdc_item occurs 0 with header line.
data : begin of bdc_list occurs 0,
       zcmno like ztsd_acl_l-zcmno,
       end of bdc_list.

data : begin of it_acm_fax occurs 0.
       include structure zssd_acm_fax_i.
**       ZACLN LIKE ZTSD_ACL_L-ZACLN,
**       ZACDT LIKE ZTSD_ACL_L-ZACDT,
**       ZSCAA LIKE ZTSD_ACL_L-ZSCAA,
**       ZCMRN LIKE ZTSD_ACL_L-ZCMRN,
**       ZACAA LIKE ZTSD_ACL_L-ZACAA,
**       ZCBAA LIKE ZTSD_ACL_L-ZACAA,
data : end of it_acm_fax.

data : begin of it_acm_fax_h occurs 0.
       include structure zssd_acm_fax_h.
data : end of it_acm_fax_h.

data : begin of it_acm_remi occurs 0.
       include structure zssd_acm_remi_i.
**       ZACLN LIKE ZTSD_ACL_L-ZACLN,
**       ZACDT LIKE ZTSD_ACL_L-ZACDT,
**       ZCMRN LIKE ZTSD_ACL_L-ZCMRN,
**       ZACAA LIKE ZTSD_ACL_L-ZACAA,
**       ZCBAA LIKE ZTSD_ACL_L-ZACAA,
**       ZREMI LIKE ZTSD_aCL_L-ZACAA,
data : end of it_acm_remi.

data : begin of it_acm_remi_h occurs 0.
       include structure zssd_acm_remi_h.
data : end of it_acm_remi_h.

data : begin of mail_addr occurs 0.
       include structure zssd_mail_addr.
data : end of mail_addr.


* BAPI
data : begin of order_header_inx.
       include structure bapisdh1x.
data : end of order_header_inx.

data : begin of return occurs 0.
       include structure bapiret2.
data : end of return.
* BAPI

data : save_ok_code(4).

data : w_cnt type i,
       w_answer(1),
       w_result(1),
       w_result_msg(100),
       w_gubun(1), "1-FAX 2-REMI
       w_attn(20),
       w_cc(20).

data : w_zscaa like ztsd_acl_l-zscaa,
       w_zacaa like ztsd_acl_l-zacaa,
       w_zcbaa like ztsd_acl_l-zacaa,
       w_zremi like ztsd_acl_l-zacaa.

data : w_return like sy-subrc.


*
selection-screen begin of block b1 with frame title text-001.
select-options : s_zacln for ztsd_acl_l-zacln no-extension.
parameters : p_zcdst like ztsd_acl_l-zcdst,
             p_zrmfg like ztsd_acl_l-zrmfg.
selection-screen end of block b1.


*
at user-command.
  case sy-ucomm.
  when 'EMAIL'.
    perform send_word.
  endcase.


*
start-of-selection.
  perform read_data.


*
end-of-selection.
  perform call_screen.







************************************************************************
* TYPE FOR THE DATA OF TABLECONTROL 'TC_9000'
types: begin of t_tc_9000.
         include structure ztsd_acl_l.
types:   flag,       "flag for mark column
       end of t_tc_9000.

* INTERNAL TABLE FOR TABLECONTROL 'TC_9000'
data:     g_tc_9000_itab   type t_tc_9000 occurs 0,
          g_tc_9000_wa     type t_tc_9000, "work area
          g_tc_9000_copied.           "copy flag

* DECLARATION OF TABLECONTROL 'TC_9000' ITSELF
controls: tc_9000 type tableview using screen 9000.

* LINES OF TABLECONTROL 'TC_9000'
data:     g_tc_9000_lines  like sy-loopc.

data:     ok_code like sy-ucomm.

* Includes inserted by Screen Painter Wizard. DO NOT CHANGE THIS LINE!
include zisd05l_cmr_create_f01.
include zisd05l_cmr_create_pbo.
include zisd05l_cmr_create_pai.

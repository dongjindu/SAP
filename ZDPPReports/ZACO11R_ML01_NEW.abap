************************************************************************
* Program Name      : ZACO11R_ML01
* Author            :
* Creation Date     : 01/22/2004
* Specifications By : Andy Choi
* Pattern           : Report 1-1
* Development Request No: UD1K906275
* Add documentation :
* Description       : Summary report for T-Code "CKM3"
*   Change....
*
* Modifications Log
* Date       Developer   Request ID    Description
* 04/27/2011 Valerian    UD1K951500    Adjust logic for JIS Material
* 04/28/2011 Valerian    UD1K951515    Fix bug in the program
* 05/23/2011 Valerian    UD1K951772    Add MARA-ZEINR in report layout
* 06/16/2011 Valerian    UD1K952048    Get MARA-ZEINR from tab ZTCO_MAT
* 08/24/2011 Valerian    UD1K952838    Get ZGRP2, ZGRP3, ZGRP4 from
*                                      table ZTCO_MAT
* 08/31/2011 Valerian    UD1K952897    Get ZGRP5 from table ZTCO_MAT
* 11/29/2011 Valerian    UQ1K900343    Include requested fields from
*                                      table ZTCO_MAT
* 06/14/2013 T00303      UD1K957366    U1 - Apply archiving
************************************************************************
report zaco11r_ml01_new no standard page heading message-id zmco.

*----------------------------------------------------------------------*
*   Macro
*----------------------------------------------------------------------*
define def_val.
  data :
          &1_lbkum like mlcd-lbkum,
          &1_salk3 like mlcd-salk3,
          &1_rd    like mlcd-salk3.
end-of-definition .

*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
** Type-Pools
type-pools : ckmv0, slis.

** Tables
tables : ckmlpp, ckmlcr, ckmlhd, ckmlct.
tables : macku, marc, mara.
tables : mlcd_key, mlcd, mlcr, mlpp,
         mlhd, mlit.
tables : t001w, mbew,marv.
data: gs_variant  like disvariant.

data: g_msg(100) type c.

* For MLCD
data : it_kalnr	  type ckmv0_matobj_tbl with header line.
constants: c_dailyjob type char12 value 'DAILYJOB'.

data : begin of it_mlcd occurs 1000,
        kalnr   like mlcd-kalnr ,
        bdatj   like mlcd-bdatj ,
        poper   like mlcd-poper ,
        categ   like mlcd-categ ,
        ptyp    like mlcd-ptyp  ,
        bvalt   like mlcd-bvalt ,
        curtp   like mlcd-curtp ,
        object  like mara-matnr,  " partner material
        lbkum   like mlcd-lbkum ,
        meins   like mlcd-meins ,
        salk3   like mlcd-salk3 ,
        estprd  like mlcd-estprd,
        estkdm  like mlcd-estkdm,
        mstprd  like mlcd-mstprd,
        mstkdm  like mlcd-mstkdm,
        waers   like mlcd-waers ,
        tpprd   like mlcd-tpprd ,
       end of   it_mlcd.

* For Display
data : begin of it_mlsum occurs 500,
        bdatj like mlcd-bdatj,
        poper like mlcd-poper,
        mtart like mara-mtart,
        matnr like ckmlhd-matnr,
        bwkey like ckmlhd-bwkey,
        bwtar like ckmlhd-bwtar,
        meins like mlcd-meins,
        waers like mlcd-waers.

* Value Part
def_val ab.  "AB     Beginning inventory
def_val pc.  "PC     Price changes
def_val zn.  "ZN     Receipts on PO/Subcont.
def_val zf.  "ZF     Production
def_val zo.  "ZO     Other receipts
def_val zu.  "ZU     stock transfer
def_val zm.                                                 "ZM     M2M
def_val vp.  "VP     debit/credit
def_val nd.  "ND  Not distributed
def_val kb.  "KB  Cumulative inventory
def_val vn.  "VN  Normal Consumption
def_val vx.  "VX     consumption cc
def_val vk.  "VK     consumption cc
def_val vo.  "VO  consumption order
def_val vu.  "VU     stock transfer
def_val vm.                                                 "VM     M2M
def_val ni.  "NC  Not allocated/Included
def_val eb.  "EB  Ending inventory

data :   bklas like mbew-bklas,  "MBEW/H
         vklas like mbew-bklas,                             "CKMLMV011
         maktg like makt-maktg,
         kalnr like mlcd-kalnr,
         prctr type prctr,
         zeinr type mara-zeinr,                             "UD1K951772
         zgrp2 type ztco_mat-zgrp2,                         "UD1K952838
         zgrp3 type ztco_mat-zgrp3,                         "UD1K952838
         zgrp4 type ztco_mat-zgrp4,                         "UD1K952838
         zgrp5 type ztco_mat-zgrp5,                         "UD1K952897
         zgrp6 type ztco_mat-zgrp6,                         "UQ1K900343
         zgrp7 type ztco_mat-zgrp7,                         "UQ1K900343
         zgrp8 type ztco_mat-zgrp8,                         "UQ1K900343
         zgrp9 type ztco_mat-zgrp9,                         "UQ1K900343
         zgrp10 type ztco_mat-zgrp10,                       "UQ1K900343
       end of it_mlsum.

data : gv_col_pos type i,
       it_sort    type slis_t_sortinfo_alv with header line.

data : begin of it_t001w occurs 0.
        include structure t001w.
data : end of   it_t001w.

* For Summary
data : it_ml_summary  like standard table of ztco_ml_summary
                           with header line
                           initial size 3000.


* Find BADY BOY
types: begin of s_mats,
        kalnr type ckmlhd-kalnr,
        bklas type mbew-bklas,   "val class from MBEW/MBEWH- correct
        vklas type mbew-bklas,   "val class from CKMLMV011 - incorrect

        matnr type ckmlhd-matnr,
        bwkey type ckmlhd-bwkey,
        bwtar type ckmlhd-bwtar,
        mtart type mara-mtart,
        matkl type mara-matkl,
        meins type mara-meins,
        maktg type makt-maktg,

        stprs type mbew-stprs,
        verpr type mbew-verpr,

     salk3    type mbew-salk3,
     status   like ckmlpp-status,    "ML status
     abkumo   like ckmlpp-abkumo,    "Begin
     umkumo   like ckmlpp-umkumo,    "Prev Posting
     zukumo   like ckmlpp-zukumo,    "GR
     vnkumo   like ckmlpp-vnkumo,    "GI
     lbkum    like ckmlpp-lbkum ,    "End
     ekkumo   like ckmlpp-ekkumo,    "PO GR

     peinh    like ckmlcr-peinh,
     absalk3  like ckmlcr-absalk3,
     abprd_o  like ckmlcr-abprd_o,
     abkdm_o  like ckmlcr-abkdm_o,
     abprd_mo like ckmlcr-abprd_mo,
     abkdm_mo like ckmlcr-abkdm_mo,

     vpprd_o  like ckmlcr-vpprd_o,
     zuprd_o  like ckmlcr-zuprd_o,
     zukdm_o  like ckmlcr-zukdm_o,
     vpkdm_o  like ckmlcr-vpkdm_o,

     zuprd_mo  like ckmlcr-zuprd_mo,
     zukdm_mo  like ckmlcr-zukdm_mo,

     vnprd_ea  like ckmlcr-vnprd_ea,
     vnkdm_ea  like ckmlcr-vnkdm_ea,
     ebprd_ea  like ckmlcr-ebprd_ea,
     ebkdm_ea  like ckmlcr-ebkdm_ea,
     vnprd_ma  like ckmlcr-vnprd_ma,
     vnkdm_ma  like ckmlcr-vnkdm_ma,
     ebprd_ma  like ckmlcr-ebprd_ma,
     ebkdm_ma  like ckmlcr-ebkdm_ma,
     prctr type prctr,
     zeinr     like mara-zeinr,                             "UD1K951772
     zgrp2     like ztco_mat-zgrp2,                         "UD1K952838
     zgrp3     like ztco_mat-zgrp3,                         "UD1K952838
     zgrp4     like ztco_mat-zgrp4,                         "UD1K952838
     zgrp5     like ztco_mat-zgrp5,                         "UD1K952897
     zgrp6     like ztco_mat-zgrp6,                         "UQ1K900343
     zgrp7     like ztco_mat-zgrp7,                         "UQ1K900343
     zgrp8     like ztco_mat-zgrp8,                         "UQ1K900343
     zgrp9     like ztco_mat-zgrp9,                         "UQ1K900343
     zgrp10    like ztco_mat-zgrp10,                        "UQ1K900343
   end of s_mats,

  ty_mats type standard table of s_mats with key kalnr,

    begin of s_ndi,
       kalnr type ckmlhd-kalnr,
       bdatj type ckmlpp-bdatj,
       poper type ckmlpp-poper,
       untper type ckmlpp-untper,
       curtp type ckmlcr-curtp,
       matnr type ckmlhd-matnr,
       bwkey type ckmlhd-bwkey,
       bwtar type ckmlhd-bwtar,
       vbeln type ckmlhd-vbeln,
       posnr type ckmlhd-posnr,
       pspnr type ckmlhd-pspnr,
       pos_type(3),                 "NDI, NIN
       bklas type mbew-bklas,
       mtart type mara-mtart,
       matkl type mara-matkl,
       prctr type marc-prctr,
       meins type ckmlpp-meins,
       status type ckmlpp-status,
       lbkum type ckmlpp-lbkum,
       menge type kkb_ml_menge,
       pbpopo type ckmlpp-pbpopo,
       salk3 type ckmlcr-salk3,
       wert type kkb_ml_bewer,
       stprs type ckmlcr-stprs,
       pvprs type ckmlcr-pvprs,
       peinh type ckmlcr-peinh,
       waers type ckmlcr-waers,
       pbprd_o type ckmlcr-pbprd_o,
       pbkdm_o type ckmlcr-pbkdm_o,
       estprd type ckml_estprd,
       estkdm type ckml_estkdm,
       mstprd type ckml_mstprd,
       mstkdm type ckml_mstkdm,
       estdif type ck_singlelevel_dif,
       mstdif type ck_multilevel_dif,
       prdif type ck_sum_prdif,
       krdif type ck_sum_krdif,
       sumdif type ck_sum_dif,
       color(3) type c,
     end of s_ndi,

     ty_out type standard table of s_ndi with key kalnr.

data: t_mats type ty_mats  with header line.


data: t_ckmlpp type standard table of ckmlpp
               with key kalnr bdatj poper
               with header line,
      t_ckmlcr type standard table of ckmlcr
               with key kalnr bdatj poper curtp
               with header line,
      t_mlcd type standard table of mlcd
               with key kalnr bdatj poper untper categ ptyp bvalt curtp
               with header line,
      t_mlcd_not_alloc type standard table of mlcd
               with key kalnr bdatj poper untper categ ptyp bvalt curtp
               with header line,
      t_bad  type ty_out   with header line.

data: begin of i_proc_kalnr occurs 0,
        werks like ckmlmv001-werks,
        matnr like ckmlmv001-matnr,
        bwtar like ckmlmv001-bwtar,
        prock like ckmlmv001-proc_kalnr,
        btyp  like ckmlmv001-btyp,  "bf-production, bb-procurement
        kalnr like ckmlhd-kalnr,
      end of i_proc_kalnr.

*--- ALV
type-pools: slis.
data : w_fieldcat type slis_t_fieldcat_alv with header line,
       w_eventcat type slis_t_event with header line,
       w_selfield type slis_selfield,
       w_sortcat  type slis_t_sortinfo_alv with header line,
       w_col_pos  type i,
       w_program  like sy-repid,
       w_top_of_page type slis_t_listheader,
       w_line1 type slis_listheader.

data: gt_fieldcat type slis_t_fieldcat_alv,
      gs_layout   type slis_layout_alv,
      gt_sp_group type slis_t_sp_group_alv,
      gt_events   type slis_t_event,
      gt_sorts    type slis_t_sortinfo_alv with header line,
      gs_prnt     type slis_print_alv,
      g_repid     like sy-repid,
      g_user_command  type slis_formname value 'USER_COMMAND'.

*---- ALV

*- U1 Start
data: begin of gt_mlcrf occurs 1000,
      kalnr like mlit-kalnr,
      prdif like mlcrf-prdif,
      krdif like mlcrf-krdif,
      end of gt_mlcrf.
*- U1 End

*** 08/05/2013 - T00306 Start
data: l_bulk type c.
data: gt_bulk_um type table of zsco_bulk_per_um
                      with header line.
*** 08/05/2013 - T00306 End

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
selection-screen begin of block bl1 with frame title text-001.
parameters : p_kokrs like csks-kokrs     memory id cac  obligatory
                                         value check,
             p_bdatj like mlcd_key-bdatj memory id bdtj obligatory.
select-options :  s_poper for mlcd_key-poper no-extension
                          memory id popr.
parameters : p_curtp like mlcd-curtp     default '10'
                                         modif id dis.
parameters : p_db     as checkbox default ' '.
selection-screen end of block bl1.

selection-screen begin of block bl2 with frame title text-002.
select-options : s_matnr for macku-matnr memory id mat,
                 s_mtart for macku-mtart.
select-options : s_bwkey for mbew-bwkey,
                 s_bklas for mbew-bklas,
                 s_vklas for mbew-bklas  modif id dis, "VC in ML
                 s_prctr for marc-prctr.
selection-screen end of block bl2.

selection-screen begin of block bl3 with frame title text-003.
parameters: p_up     as checkbox             modif id dis.
parameters: p_ml(1)  type c default 'X'      modif id dis.
selection-screen end of block bl3.
* Layout
selection-screen begin of block b4 with frame title text-010.
parameter p_vari type slis_vari.
selection-screen end of block b4.

*- U1 Start
*INCLUDE ziarch_comm01.
*- U1 End

data: p_poper like mlcd_key-poper.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
at selection-screen output.
* Check For Update-Option
  perform chk_up_opt.

at selection-screen.
* Check Future Date
  perform chk_ft_date.

at selection-screen on value-request for p_vari.
  perform alv_variant_f4 changing p_vari.

*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
start-of-selection.
* Read Current fiscal Period
  perform fiscal_year.

  if p_db = 'X'.
    perform get_data_from_buffer.

  else.
* Get Buffer data from ZTCO_ML_DETAIL table
    read table s_poper index 1.
    p_poper = s_poper-low.

    perform get_materials.

    if 1 = 2.
      perform get_material_periods using t_mats[]
                                         t_ckmlpp[]
                                         t_ckmlcr[].
    endif.

    if t_mats[] is initial.
      exit.
    endif.

    perform get_proc_kalnr.

* Read MLCD
    perform read_mlcd.
    perform find_bad_boys_new.

* Merge to IT_MLCD
    perform merge_mlcd_data.

* Read Material Data / Unit / Curr.
    perform delete_zero_record.
    perform fill_material_info.

* Update
    perform update_record.
  endif.

*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
end-of-selection.

  import bulk = l_bulk from memory id 'BULK'.
  free memory id 'BULK'.

  if l_bulk eq 'X'.
    loop at it_mlsum.
      move-corresponding it_mlsum to gt_bulk_um.
      append gt_bulk_um.
    endloop.
    export tab = gt_bulk_um to memory id 'BULK_PER_UM'.
  else.
    if p_up = space.
      perform call_alv_list.
    endif.
  endif.

*----------------------------------------------------------------------*
* Sub-Routine
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  READ_MLCD
*&---------------------------------------------------------------------*
*       Read MLCD data
*----------------------------------------------------------------------*
form read_mlcd.
  data: lt_kalnr type ckmv0_matobj_tbl,
        ls_mats  type s_mats,
        ls_kalnr type ckmv0_matobj_str.

  refresh lt_kalnr.

  loop at t_mats into ls_mats.
    clear ls_kalnr.
    ls_kalnr-kalnr = ls_mats-kalnr.
    ls_kalnr-bwkey = ls_mats-bwkey.
    append ls_kalnr to lt_kalnr.
  endloop.

* Read data
  call function 'CKMCD_MLCD_READ'
    exporting
      i_from_bdatj      = p_bdatj
      i_from_poper      = p_poper
      i_refresh_buffer  = 'X'
      i_online          = 'X'
    tables
      it_kalnr          = lt_kalnr
      ot_mlcd           = t_mlcd
      ot_mlcd_not_alloc = t_mlcd_not_alloc
    exceptions
      data_error        = 1
      others            = 2.

  if sy-subrc <> 0.
    message id   sy-msgid type sy-msgty number sy-msgno
                          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

  sort t_mlcd_not_alloc by kalnr bdatj poper.

endform.                    " READ_MLCD

*&---------------------------------------------------------------------*
*&      Form  MERGE_MLCD_DATA
*&---------------------------------------------------------------------*
*       Merge to IT_MLCD
*----------------------------------------------------------------------*
form merge_mlcd_data.
  clear : it_mlcd,  it_mlcd[].
  clear : it_mlsum, it_mlsum[].

* Ignore Process category for procurement alt. or consuption alt.
* Ignore Procurement alternative/process
* MLCD-PTYP MLCD-BVALT
* t_mlcd
* t_mlcd_not_alloc

* For Allocated data / Unallocated Data
  perform allocated_data_mlcd.

* Not distributed
  perform fill_ndi_nin.

* For Beginning Inv./Amt.
  perform beginning_inv_amt.

  perform cal_space_cat.

* After PC, goods movement -> use new STD price.
  if p_ml = 'X'.
*   PERFORM read_mlcrf_prev_pst_var.  "Too Slow!!!

*-- use this logic
    perform read_prev_pst_var2.
  endif.

* Calculate Cumulative / Ending
  perform cal_cumulative_ending.                            "UD1K918935

endform.                    " MERGE_MLCD_DATA

*&---------------------------------------------------------------------*
*&      Form  TRANS_VAL_TO_DIS
*&---------------------------------------------------------------------*
*       Transfering Value to Dis. Tab.
*----------------------------------------------------------------------*
form trans_val_to_dis.
  field-symbols <fsval> type any.
  data lv_fname(60).

* Key Part
  clear it_mlsum.
  it_mlsum-kalnr = it_mlcd-kalnr.

* Qty.
  clear lv_fname.
  concatenate 'it_mlsum' '-' it_mlcd-categ '_LBKUM'
         into lv_fname.
  assign (lv_fname) to <fsval>.

  <fsval> = it_mlcd-lbkum.

* Amt. / Valuated stock
  clear lv_fname.
  concatenate 'it_mlsum' '-' it_mlcd-categ '_SALK3'
         into lv_fname.
  assign (lv_fname) to <fsval>.

  <fsval> = it_mlcd-salk3.

* Amt. / Price Difference (ERD+PRD In Category)
  clear lv_fname.
  concatenate 'it_mlsum' '-' it_mlcd-categ '_RD'
         into lv_fname.
  assign (lv_fname) to <fsval>.

  <fsval> = it_mlcd-estprd
          + it_mlcd-estkdm
          + it_mlcd-mstprd
          + it_mlcd-mstkdm
          + it_mlcd-tpprd.

***ANDY
*  IF it_mlcd-categ = 'PC'.
*    it_mlsum-ab_salk3 = - it_mlcd-salk3.
*    it_mlsum-ab_rd    = + it_mlcd-salk3.
*    it_mlsum-pc_salk3 = + it_mlcd-salk3.
*    it_mlsum-pc_rd    = - it_mlcd-salk3.
*  ENDIF.

* Append
  collect it_mlsum.
  clear  it_mlsum.

endform.                    " TRANS_VAL_TO_DIS
*&---------------------------------------------------------------------*
*&      Form  ALLOCATED_DATA_MLCD
*&---------------------------------------------------------------------*
*       For Allocated data / Unallocated Data
*----------------------------------------------------------------------*
form allocated_data_mlcd.
*  DATA: l_object LIKE mara-matnr.

** For Allocated Data
* Ending/Beginning Inv/Amt will be calculated later part
  loop at t_mlcd where categ ne 'AB'
                   and categ ne 'EB'.

** Find object for GI
*    CLEAR: it_mlcd, l_object.
*    IF ( t_mlcd-categ = 'VN' AND t_mlcd-ptyp = 'VF' )
*    OR t_mlcd-ptyp = 'BU' OR t_mlcd-ptyp = 'VO'.
*      READ TABLE i_proc_kalnr WITH KEY prock = t_mlcd-bvalt
*           BINARY SEARCH.
*      IF sy-subrc = 0.
*        MOVE: i_proc_kalnr-matnr TO it_mlcd-object,
*              i_proc_kalnr-matnr TO l_object.
*      ENDIF.
*    ENDIF.
*
**Allow Price Change
*    IF t_mlcd-categ = space AND t_mlcd-ptyp <> 'PC'.
*      CONTINUE.
*    ENDIF.
*    IF t_mlcd-ptyp = 'PC'.
*      t_mlcd-categ = 'PC'.
*    ENDIF.
*
**Transfer; change category
*    IF t_mlcd-ptyp(2) = 'VU' OR t_mlcd-ptyp(2) = 'BU'.
**     CONCATENATE t_mlcd-ptyp(1) 'S' INTO t_mlcd-categ.
*      t_mlcd-categ = t_mlcd-ptyp(2).
*
*      READ TABLE t_mats WITH KEY kalnr = t_mlcd-kalnr BINARY SEARCH.
*      IF l_object <> space AND t_mats-matnr <> l_object.  "M2M
*        IF t_mlcd-ptyp(2) = 'BU'.
*          t_mlcd-categ = 'BM'.
*        ELSE.
*          t_mlcd-categ = 'VM'.
*        ENDIF.
*      ELSEIF t_mlcd-ptyp+2(2) = 'BU'.
*        CONTINUE.  "SKIP (sales order stock transfer)
*      ENDIF.
*
**Debit/Credit
*    ELSEIF t_mlcd-categ = 'VP'.
*      t_mlcd-categ = 'VP'.
*    ELSEIF t_mlcd-categ = 'VN'.
*      CASE t_mlcd-ptyp.
*        WHEN 'VEAU'. t_mlcd-categ = 'VO'.   "GI-order
*        WHEN 'VK'.   t_mlcd-categ = 'VK'.   "GI-CC
*      ENDCASE.
*
*    ENDIF.
*
**physical inventory
*   if t_mlcd-categ = 'ZU' and t_mlcd-ptyp = 'B+'.
*      t_mlcd-categ = 'ZV'.
*   elseif t_mlcd-categ = 'VN' and t_mlcd-ptyp = 'V+'.
*      t_mlcd-categ = 'VV'.
*   endif.
*
** Transfer values
** Clear the values which are not useful.
*    CLEAR : t_mlcd-meins, t_mlcd-waers.
*    CLEAR : t_mlcd-bvalt, t_mlcd-ptyp.
*    MOVE-CORRESPONDING t_mlcd TO it_mlcd.
*
*
*    COLLECT it_mlcd.
*    IF t_mlcd-ptyp = 'BU' OR t_mlcd-ptyp = 'VU'.
*    ENDIF.

    case t_mlcd-categ.
      when 'ZU'.
        case t_mlcd-ptyp.
          when 'BB' or 'BBK' or 'BL'.
            t_mlcd-categ = 'ZN'.

          when 'BF'.
            t_mlcd-categ = 'ZF'.

          when 'B+' or 'BKA'.
            t_mlcd-categ = 'ZO'.

          when 'BU'.
            read table t_mats with key kalnr = t_mlcd-kalnr
                 binary search.

            read table i_proc_kalnr with key prock = t_mlcd-bvalt
                 binary search.

            if sy-subrc <> 0.
              select single werks matnr bwtar proc_kalnr btyp kalnr
                into i_proc_kalnr
                from ckmlmv001
              where proc_kalnr = t_mlcd-bvalt.
            endif.

            if sy-subrc = 0 and t_mats-matnr <> i_proc_kalnr-matnr.
              move: i_proc_kalnr-matnr to it_mlcd-object.
              t_mlcd-categ = 'ZM'.
            else.
              t_mlcd-categ = 'ZU'.
            endif.

          when 'BUBM'.
            t_mlcd-categ = 'ZM'.

          when 'BUBS'.
            clear t_mlcd-categ.

          when others.
            clear t_mlcd-categ.
        endcase.
*
      when  'VN'.
        t_mlcd-lbkum = - t_mlcd-lbkum.
        t_mlcd-salk3 = - t_mlcd-salk3.
        t_mlcd-estprd  = - t_mlcd-estprd.
        t_mlcd-estkdm  = - t_mlcd-estkdm.
        t_mlcd-mstprd  = - t_mlcd-mstprd.
        t_mlcd-mstkdm  = - t_mlcd-mstkdm.

        case t_mlcd-ptyp.
          when 'VKA' or 'VF' or 'VL'.
            t_mlcd-categ = 'VN'.

          when 'V+' or 'VA' or 'VP'.
            t_mlcd-categ = 'VX'.

          when 'VK'.
            t_mlcd-categ = 'VK'.

          when 'VHP' or 'VEAU'.
            t_mlcd-categ = 'VO'.

          when 'VU'.
            read table t_mats with key kalnr = t_mlcd-kalnr
                 binary search.

            read table i_proc_kalnr with key prock = t_mlcd-bvalt
                 binary search.

            if sy-subrc <> 0.
              select single werks matnr bwtar proc_kalnr btyp kalnr
                into i_proc_kalnr
                from ckmlmv001
              where proc_kalnr = t_mlcd-bvalt.
            endif.

            if sy-subrc = 0 and t_mats-matnr <> i_proc_kalnr-matnr.
              move: i_proc_kalnr-matnr to it_mlcd-object.
              t_mlcd-categ = 'VM'.
            else.
              t_mlcd-categ = 'VU'.
            endif.

          when 'VUBM'.
            t_mlcd-categ = 'VM'.

          when 'VUBS'.
            clear t_mlcd-categ.

          when others.
            clear t_mlcd-categ.
        endcase.

      when 'VP'.
        t_mlcd-categ = 'VP'.

      when others.
        clear t_mlcd-categ.
    endcase.

* Transfer values
* Clear the values which are not useful.
    clear: t_mlcd-meins, t_mlcd-waers, t_mlcd-bvalt, t_mlcd-ptyp.

    if t_mlcd-categ <> space.
      move-corresponding t_mlcd to it_mlcd.
      collect it_mlcd.clear it_mlcd.
    endif.

  endloop.

** Trasfer data to Display Tab.
  loop at it_mlcd.
* AB  Beginning inventory
* EB  Ending inventory
* SPACE Value
    if it_mlcd-categ <> 'AB' and
       it_mlcd-categ <> 'EB' and
       it_mlcd-categ <> space.
      perform trans_val_to_dis.
    endif.
  endloop.

  clear: it_mlsum, it_mlcd.

endform.                    " ALLOCATED_DATA_MLCD

*&---------------------------------------------------------------------*
*&      Form  fill_material_info
*&---------------------------------------------------------------------*
*       Read Material Data / Unit / Curr.
*----------------------------------------------------------------------*
form fill_material_info.
* Read Material Information with Cost. Est. Number.

  loop at it_mlsum.
    clear t_mats.
    read table t_mats with key
                      kalnr = it_mlsum-kalnr
                      binary search.
    move : t_mats-matnr to it_mlsum-matnr,
           t_mats-bklas to it_mlsum-bklas,
           t_mats-vklas to it_mlsum-vklas,
           t_mats-bwkey to it_mlsum-bwkey,
           t_mats-bwtar to it_mlsum-bwtar,
           t_mats-mtart to it_mlsum-mtart,
           t_mats-bklas to it_mlsum-bklas,
           t_mats-meins to it_mlsum-meins,
           t_mats-maktg to it_mlsum-maktg,
           t_mats-prctr to it_mlsum-prctr,
           t_mats-zeinr to it_mlsum-zeinr,                  "UD1K951772
           t_mats-zgrp2 to it_mlsum-zgrp2,                  "UD1K952838
           t_mats-zgrp3 to it_mlsum-zgrp3,                  "UD1K952838
           t_mats-zgrp4 to it_mlsum-zgrp4,                  "UD1K952838
           t_mats-zgrp5 to it_mlsum-zgrp5,                  "UD1K952897
           t_mats-zgrp6 to it_mlsum-zgrp6,                  "UQ1K900343
           t_mats-zgrp7 to it_mlsum-zgrp7,                  "UQ1K900343
           t_mats-zgrp8 to it_mlsum-zgrp8,                  "UQ1K900343
           t_mats-zgrp9 to it_mlsum-zgrp9,                  "UQ1K900343
           t_mats-zgrp10 to it_mlsum-zgrp10.                "UQ1K900343

    modify it_mlsum.
  endloop.

endform.                    " fill_material_info

*&---------------------------------------------------------------------*
*&      Form  BEGINNING_INV_AMT
*&---------------------------------------------------------------------*
*       For Beginning Inv./Amt.
*----------------------------------------------------------------------*
form beginning_inv_amt.
  loop at  t_mats.
    clear   it_mlsum.

    it_mlsum-kalnr = t_mats-kalnr.

*   Beginning Qty
    it_mlsum-ab_lbkum = t_mats-abkumo. " t_mats-UMKUMO.

*   Previous Posting (UMKUMO)-> consider in MLCD ? NO!

*   Beginning Amt.
    it_mlsum-ab_salk3 = t_mats-absalk3.

*   Beginning RD.
    it_mlsum-ab_rd = t_mats-abprd_o  + t_mats-abkdm_o
                   + t_mats-abprd_mo + t_mats-abkdm_mo.

    collect it_mlsum.clear it_mlsum.
  endloop.

endform.                    " BEGINNING_INV_AMT
*&---------------------------------------------------------------------*
*&      Form  CAL_SPACE_CAT
*&---------------------------------------------------------------------*
*       Calculate Values in the Category ' ' (SPACE)
*----------------------------------------------------------------------*
form cal_space_cat.

**SAPCE - Price Change
* AB - Post to Previous Period
  refresh it_mlcd.

  loop at t_mlcd where categ eq space
                    or categ eq 'AB'.

    clear it_mlcd.
    move-corresponding t_mlcd to it_mlcd.
    clear: it_mlcd-ptyp, it_mlcd-bvalt.

    it_mlcd-categ = 'AB'.

    collect it_mlcd.clear it_mlcd.
  endloop.

** Trasfer data to Display Tab.
  loop at it_mlcd.
    perform trans_val_to_dis.
  endloop.

endform.                    " CAL_SPACE_CAT
*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
*       User Command                                                  *
*---------------------------------------------------------------------*
form user_command using r_ucomm like sy-ucomm
                  rs_selfield type slis_selfield.

  case r_ucomm.
    when '&IC1'.                             "dobbleclick
* CKM3
      if  it_mlsum-matnr <> space.  "rs_selfield-value NE space.
        set parameter id 'MAT'  field  it_mlsum-matnr.
        set parameter id 'WRK'  field  it_mlsum-bwkey.
        set parameter id 'POPR' field  it_mlsum-poper.
        set parameter id 'BDTJ' field  it_mlsum-bdatj.

        call transaction 'CKM3' and skip first screen.
        clear r_ucomm.
      endif.
** PCC Order
*      IF  rs_selfield-sel_tab_field = 'it_mlsum-MATNR'
*      AND rs_selfield-value NE space.
*        READ TABLE it_mlsum INTO it_mlsum INDEX rs_selfield-tabindex.
*        CLEAR t001w.
*        SELECT SINGLE * FROM t001w
*                       WHERE bwkey = it_mlsum-bwkey.
*        PERFORM show_pcc_order.
*      ENDIF.
  endcase.
endform.                    "user_command

*&---------------------------------------------------------------------*
*&      Form  SHOW_PCC_ORDER
*&---------------------------------------------------------------------*
*       SHow Pcc order
*----------------------------------------------------------------------*
form show_pcc_order.
* it_mlsum
  data : it_l_vkks0 like standard table of vkks0
                    with header line,
         it_l_pkosa like standard table of pkosa
                    with header line.

  call function 'KK_F_PKOSA_FIND'
    exporting
      i_matnr               = it_mlsum-matnr
      i_werks               = t001w-werks
    tables
      e_vkks0               = it_l_vkks0
      e_pkosa               = it_l_pkosa
    exceptions
      none_found            = 1
      wrong_input           = 2
      none_picked           = 3
      wrong_rule            = 4
      rsh_not_valid         = 5
      wrong_characteristics = 6
      no_rule               = 7
      version_not_valid     = 8
      others                = 9.

  if sy-subrc <> 0.
    message i000 with text-101.
  else.

    call function 'STC1_POPUP_WITH_TABLE_CONTROL'
      exporting
        header            = text-101
        tabname           = 'VKKS0'
        display_only      = 'X'
        no_button         = space
      tables
        table             = it_l_vkks0
      exceptions
        no_more_tables    = 1
        too_many_fields   = 2
        nametab_not_valid = 3
        handle_not_valid  = 4
        others            = 5.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.
  endif.
endform.                    " SHOW_PCC_ORDER
*&---------------------------------------------------------------------*
*&      Form  CHK_FT_DATE
*&---------------------------------------------------------------------*
*       Check Future Period
*----------------------------------------------------------------------*
form chk_ft_date.
  if  p_bdatj > sy-datum(4).
    message e000 with text-301 p_bdatj p_poper.
  endif.
endform.                    " CHK_FT_DATE

*&---------------------------------------------------------------------*
*&      Form  chk_up_opt
*&---------------------------------------------------------------------*
form chk_up_opt.

*  data: l_num(3) type n.

  if sy-tcode = 'ZCOR07'.
    loop at screen.
      if screen-group1 = 'DIS'.
        screen-invisible = '1'.
        screen-active    = '0'.
        modify screen.
      endif.
    endloop.

  endif.

  if sy-slset eq c_dailyjob.
    p_bdatj = sy-datum+0(4).
    clear: s_poper, s_poper[].
    s_poper = 'IEQ'.
    s_poper-low = sy-datum+4(2).
    s_poper-high = sy-datum+4(2).
    append s_poper.
  endif.

endform.                    " chk_up_opt
*&---------------------------------------------------------------------*
*&      Form  fiscal_year
*&---------------------------------------------------------------------*
form fiscal_year.
  select single * from marv
    where bukrs = p_kokrs.

endform.                    " fiscal_year
*&---------------------------------------------------------------------*
*&      Form  cal_cumulative_ending
*&---------------------------------------------------------------------*
form cal_cumulative_ending.
* Cumulative Inventory = Sum of Begining inventory + Receipts +
*                        not distributed.
  loop at it_mlsum.
    it_mlsum-bdatj = p_bdatj.
    it_mlsum-poper = p_poper.

*   Cumulative Qty
    it_mlsum-kb_lbkum = it_mlsum-ab_lbkum
                        + it_mlsum-zn_lbkum + it_mlsum-zf_lbkum
                        + it_mlsum-zo_lbkum
                        + it_mlsum-zu_lbkum + it_mlsum-zm_lbkum.

*   Cumulative Value (PC -> AB)
    it_mlsum-kb_salk3 =   it_mlsum-ab_salk3
                        + it_mlsum-pc_salk3
                        + it_mlsum-zn_salk3
                        + it_mlsum-zf_salk3
                        + it_mlsum-zo_salk3
                        + it_mlsum-zu_salk3
                        + it_mlsum-zm_salk3.

*   Cumulative Price Difference (PC -> AB)
    it_mlsum-kb_rd =   it_mlsum-ab_rd
                     + it_mlsum-pc_rd
                     + it_mlsum-zn_rd
                     + it_mlsum-zf_rd
                     + it_mlsum-zo_rd
                     + it_mlsum-zu_rd
                     + it_mlsum-zm_rd
                     + it_mlsum-vp_rd
                     + it_mlsum-nd_rd.

*   Ending.
    it_mlsum-eb_lbkum = it_mlsum-kb_lbkum
                        + it_mlsum-vn_lbkum
                        + it_mlsum-vo_lbkum
                        + it_mlsum-vk_lbkum
                        + it_mlsum-vx_lbkum
                        + it_mlsum-vu_lbkum
                        + it_mlsum-vm_lbkum.
*   Ending Amt.
    it_mlsum-eb_salk3 =  it_mlsum-kb_salk3
                        + it_mlsum-vn_salk3
                        + it_mlsum-vo_salk3
                        + it_mlsum-vk_salk3
                        + it_mlsum-vx_salk3
                        + it_mlsum-vu_salk3
                        + it_mlsum-vm_salk3.

* not closed...
    read table t_mats with key kalnr = it_mlsum-kalnr.
    if t_mats-status <> '70'.
      it_mlsum-nd_rd    = it_mlsum-nd_rd - it_mlsum-kb_rd.
    else.
*   Ending - RD
      it_mlsum-eb_rd    = it_mlsum-kb_rd
                          + it_mlsum-ni_rd
                          + it_mlsum-vn_rd
                          + it_mlsum-vo_rd
                          + it_mlsum-vk_rd
                          + it_mlsum-vx_rd
                          + it_mlsum-vu_rd
                          + it_mlsum-vm_rd.
    endif.

    modify it_mlsum.
  endloop.

endform.                    " cal_cumulative_ending
*&---------------------------------------------------------------------*
*&      Form  get_materials
*&---------------------------------------------------------------------*
form get_materials.
  types: begin of ty_bwkey,
           bwkey type bwkey,
         end of ty_bwkey.

  data   lt_bwkey type table of ty_bwkey with header line.
  ranges r_bwkey for t001k-bwkey.

  refresh: lt_bwkey, r_bwkey, t_mats.
  clear  : lt_bwkey, r_bwkey, t_mats.
*
  select bwkey into table lt_bwkey
    from t001k
   where bukrs = p_kokrs.

  r_bwkey-sign   = 'I'.
  r_bwkey-option = 'EQ'.

  if s_bwkey[] is initial.
    loop at lt_bwkey.
      r_bwkey-low = lt_bwkey-bwkey.
      append r_bwkey.
    endloop.

  else.
    loop at lt_bwkey where bwkey in s_bwkey.
      r_bwkey-low = lt_bwkey-bwkey.
      append r_bwkey.
    endloop.
  endif.

  clear r_bwkey.
*
  data: l_abrechdat like ckmlhd-abrechdat.

  data: l_date like sy-datum.
  select single last_day into l_date
      from ckmlrunperiod as p
      inner join ckmlmv011 as m
         on  m~laufid = p~run_id
*      inner join marc as c
*         on c~matnr = m~matnr
*        and c~werks = m~WERKS
      where p~gjahr = p_bdatj
        and p~poper = p_poper
        and m~bwkey in r_bwkey.
*        and c~prctr in s_prctr.

  if sy-subrc <> 0.

    select mbew~bwkey mbew~matnr mbew~bwtar
           makt~maktg
           ckmlhd~kalnr
           mara~mtart mara~matkl marc~prctr
*          mara~zeinr                                       "UD1K951772
                                                            "UD1K952048
           mbew~bklas
           ckmlcr~stprs
           ckmlcr~pvprs as verpr
           ckmlpp~meins
*          mbew~lfgja mbew~lfmon
           ckmlpp~status
           ckmlpp~abkumo   ckmlpp~umkumo   ckmlpp~zukumo
           ckmlpp~vnkumo   ckmlpp~lbkum    ckmlpp~ekkumo
           ckmlcr~peinh
           ckmlcr~absalk3  ckmlcr~abprd_o  ckmlcr~abkdm_o
           ckmlcr~abprd_mo ckmlcr~abkdm_mo
           ckmlcr~vpprd_o  ckmlcr~zuprd_o
           ckmlcr~zukdm_o  ckmlcr~vpkdm_o
           ckmlcr~zuprd_mo ckmlcr~zukdm_mo
           ckmlcr~vnprd_ea ckmlcr~vnkdm_ea
           ckmlcr~ebprd_ea ckmlcr~ebkdm_ea
           ckmlcr~vnprd_ma ckmlcr~vnkdm_ma
           ckmlcr~ebprd_ma ckmlcr~ebkdm_ma
       into corresponding fields of table t_mats
             from ( mbew
                    inner join mara
                      on  mbew~matnr = mara~matnr
                    inner join makt
                      on  makt~matnr = mara~matnr
                     and  makt~spras = sy-langu
                    inner join marc
                      on  mbew~matnr = marc~matnr
                      and mbew~bwkey = marc~werks
                    inner join ckmlcr
                      on  ckmlcr~kalnr = mbew~kaln1
                     and  ckmlcr~bdatj = p_bdatj
                     and  ckmlcr~poper = p_poper
                     and  ckmlcr~curtp = p_curtp
                     and  ckmlcr~untper = space
                    inner join ckmlpp
                      on  ckmlpp~kalnr  = ckmlcr~kalnr
                     and  ckmlpp~bdatj  = ckmlcr~bdatj
                     and  ckmlpp~poper  = ckmlcr~poper
                     and  ckmlpp~untper = space
                    inner join ckmlhd
                      on  ckmlhd~kalnr = mbew~kaln1 )
             where mbew~bwkey in r_bwkey
               and mbew~bklas in s_bklas
               and mbew~bklas <> '3005'                     "UD1K951500
               and mara~mtart in s_mtart
               and mbew~matnr in s_matnr
*               AND ckmlhd~abrechdat <> l_abrechdat
               and marc~prctr in s_prctr
               and (   ckmlpp~zukumo <> 0 or ckmlpp~vnkumo <> 0
                    or ckmlpp~abkumo <> 0 or ckmlpp~umkumo <> 0
                    or ckmlpp~lbkum <> 0 )
      order by ckmlhd~kalnr.

* by ig.moon 4/21/2011 {
*    if sy-subrc <> 0.                                      "UD1K951500
    if '3005' in s_bklas.                                   "UD1K951515
      select mbew~bwkey mbew~matnr mbew~bwtar
             makt~maktg
             ckmlhd~kalnr
             mara~mtart mara~matkl marc~prctr
*            mara~zeinr                                     "UD1K951772
                                                            "UD1K952048
             mbew~bklas
             ckmlcr~stprs
             ckmlcr~pvprs as verpr
             ckmlpp~meins
             ckmlpp~status
             ckmlpp~abkumo   ckmlpp~umkumo   ckmlpp~zukumo
             ckmlpp~vnkumo   ckmlpp~lbkum    ckmlpp~ekkumo
             ckmlcr~peinh
             ckmlcr~absalk3  ckmlcr~abprd_o  ckmlcr~abkdm_o
             ckmlcr~abprd_mo ckmlcr~abkdm_mo
             ckmlcr~vpprd_o  ckmlcr~zuprd_o
             ckmlcr~zukdm_o  ckmlcr~vpkdm_o
             ckmlcr~zuprd_mo ckmlcr~zukdm_mo
             ckmlcr~vnprd_ea ckmlcr~vnkdm_ea
             ckmlcr~ebprd_ea ckmlcr~ebkdm_ea
             ckmlcr~vnprd_ma ckmlcr~vnkdm_ma
             ckmlcr~ebprd_ma ckmlcr~ebkdm_ma
             appending corresponding fields of table t_mats "UD1K951500
               from ( mbew
                      inner join mara
                        on  mbew~matnr = mara~matnr
                      inner join makt
                        on  makt~matnr = mara~matnr
                       and  makt~spras = sy-langu
                      inner join marc
                        on  mbew~matnr = marc~matnr
                        and mbew~bwkey = marc~werks
                      inner join ckmlcr
                        on  ckmlcr~kalnr = mbew~kaln1
                       and  ckmlcr~bdatj = p_bdatj
                       and  ckmlcr~poper = p_poper
                       and  ckmlcr~curtp = p_curtp
                       and  ckmlcr~untper = space
                      inner join ckmlpp
                        on  ckmlpp~kalnr  = ckmlcr~kalnr
                       and  ckmlpp~bdatj  = ckmlcr~bdatj
                       and  ckmlpp~poper  = ckmlcr~poper
                       and  ckmlpp~untper = space
                      inner join ckmlhd
                        on  ckmlhd~kalnr = mbew~kaln1 )
               where mbew~bwkey in r_bwkey
                 and mbew~bklas eq '3005'
                 and mara~mtart in s_mtart
                 and mbew~matnr in s_matnr
                 and marc~prctr in s_prctr
                 and (   ckmlpp~zukumo <> 0 or ckmlpp~vnkumo <> 0
                      or ckmlpp~abkumo <> 0 or ckmlpp~umkumo <> 0
                      or ckmlpp~lbkum <> 0 )
        order by ckmlhd~kalnr.
    endif.                                                  "UD1K951515
* }
*    endif.                                                 "UD1K951500

    sort t_mats by kalnr.
    perform get_valuation_class.

  else.
    select mbew~bwkey mbew~matnr mbew~bwtar
           makt~maktg
           ckmlhd~kalnr
*          mara~zeinr                                       "UD1K951772
                                                            "UD1K952048
           ckmlmv011~mtart ckmlmv011~matkl marc~prctr
           ckmlmv011~bklas as vklas     "INCORRECT!!! - ANDY
           ckmlcr~stprs
           ckmlcr~pvprs as verpr
           ckmlpp~meins
           ckmlpp~status
           ckmlpp~abkumo   ckmlpp~umkumo   ckmlpp~zukumo
           ckmlpp~vnkumo   ckmlpp~lbkum    ckmlpp~ekkumo
           ckmlcr~peinh
           ckmlcr~absalk3  ckmlcr~abprd_o  ckmlcr~abkdm_o
           ckmlcr~abprd_mo ckmlcr~abkdm_mo
           ckmlcr~vpprd_o  ckmlcr~zuprd_o
           ckmlcr~zukdm_o  ckmlcr~vpkdm_o
           ckmlcr~zuprd_mo ckmlcr~zukdm_mo
           ckmlcr~vnprd_ea ckmlcr~vnkdm_ea
           ckmlcr~ebprd_ea ckmlcr~ebkdm_ea
           ckmlcr~vnprd_ma ckmlcr~vnkdm_ma
           ckmlcr~ebprd_ma ckmlcr~ebkdm_ma
       into corresponding fields of table t_mats
             from ( mbew
                    inner join mara
                      on  mbew~matnr = mara~matnr
                    inner join makt
                      on  makt~matnr = mara~matnr
                     and  makt~spras = sy-langu
                    inner join marc
                      on  mbew~matnr = marc~matnr
                      and mbew~bwkey = marc~werks
                    inner join ckmlmv011
                      on  ckmlmv011~matnr = mbew~matnr
                      and ckmlmv011~bwkey = mbew~bwkey
                    inner join ckmlrunperiod
                      on  ckmlmv011~laufid = ckmlrunperiod~run_id
                    inner join ckmlcr
                      on  ckmlcr~kalnr  = ckmlmv011~kalnr
                     and  ckmlcr~bdatj  = ckmlrunperiod~gjahr
                     and  ckmlcr~poper  = ckmlrunperiod~poper
                     and  ckmlcr~curtp  = '10'
                     and  ckmlcr~untper = space
                    inner join ckmlpp
                      on  ckmlpp~kalnr  = ckmlcr~kalnr
                     and  ckmlpp~bdatj  = ckmlcr~bdatj
                     and  ckmlpp~poper  = ckmlcr~poper
                     and  ckmlpp~untper = space
                    inner join ckmlhd
                      on  ckmlmv011~kalnr = ckmlhd~kalnr )
             where ckmlrunperiod~gjahr = p_bdatj
               and ckmlrunperiod~poper = p_poper
               and ckmlmv011~bwkey in r_bwkey
               and ckmlmv011~mtart in s_mtart
               and ckmlmv011~matnr in s_matnr
               and ckmlhd~abrechdat <> l_abrechdat
               and marc~prctr in s_prctr
               and (   ckmlpp~zukumo <> 0 or ckmlpp~vnkumo <> 0
                    or ckmlpp~abkumo <> 0 or ckmlpp~umkumo <> 0
                    or ckmlpp~lbkum <> 0 )
               and mbew~bklas <> '3005'                     "UD1K951500
      order by ckmlhd~kalnr.

* by ig.moon 4/21/2011 {
*    if sy-subrc <> 0.                                      "UD1K951500
    if '3005' in s_bklas.                                   "UD1K951515

      select mbew~bwkey mbew~matnr mbew~bwtar
             makt~maktg
             ckmlhd~kalnr
*            mara~zeinr                                     "UD1K951772
                                                            "UD1K952048
             ckmlmv011~mtart ckmlmv011~matkl marc~prctr
             ckmlmv011~bklas as vklas     "INCORRECT!!! - ANDY
             ckmlcr~stprs
             ckmlcr~pvprs as verpr
             ckmlpp~meins
             ckmlpp~status
             ckmlpp~abkumo   ckmlpp~umkumo   ckmlpp~zukumo
             ckmlpp~vnkumo   ckmlpp~lbkum    ckmlpp~ekkumo
             ckmlcr~peinh
             ckmlcr~absalk3  ckmlcr~abprd_o  ckmlcr~abkdm_o
             ckmlcr~abprd_mo ckmlcr~abkdm_mo
             ckmlcr~vpprd_o  ckmlcr~zuprd_o
             ckmlcr~zukdm_o  ckmlcr~vpkdm_o
             ckmlcr~zuprd_mo ckmlcr~zukdm_mo
             ckmlcr~vnprd_ea ckmlcr~vnkdm_ea
             ckmlcr~ebprd_ea ckmlcr~ebkdm_ea
             ckmlcr~vnprd_ma ckmlcr~vnkdm_ma
             ckmlcr~ebprd_ma ckmlcr~ebkdm_ma
             appending corresponding fields of table t_mats "UD1K951500
               from ( mbew
                      inner join mara
                        on  mbew~matnr = mara~matnr
                      inner join makt
                        on  makt~matnr = mara~matnr
                       and  makt~spras = sy-langu
                      inner join marc
                        on  mbew~matnr = marc~matnr
                        and mbew~bwkey = marc~werks
                      inner join ckmlmv011
                        on  ckmlmv011~matnr = mbew~matnr
                        and ckmlmv011~bwkey = mbew~bwkey
                      inner join ckmlrunperiod
                        on  ckmlmv011~laufid = ckmlrunperiod~run_id
                      inner join ckmlcr
                        on  ckmlcr~kalnr  = ckmlmv011~kalnr
                       and  ckmlcr~bdatj  = ckmlrunperiod~gjahr
                       and  ckmlcr~poper  = ckmlrunperiod~poper
                       and  ckmlcr~curtp  = '10'
                       and  ckmlcr~untper = space
                      inner join ckmlpp
                        on  ckmlpp~kalnr  = ckmlcr~kalnr
                       and  ckmlpp~bdatj  = ckmlcr~bdatj
                       and  ckmlpp~poper  = ckmlcr~poper
                       and  ckmlpp~untper = space
                      inner join ckmlhd
                        on  ckmlmv011~kalnr = ckmlhd~kalnr )
               where ckmlrunperiod~gjahr = p_bdatj
                 and ckmlrunperiod~poper = p_poper
                 and ckmlmv011~bwkey in r_bwkey
                 and ckmlmv011~mtart in s_mtart
                 and ckmlmv011~matnr in s_matnr
                 and marc~prctr in s_prctr
                 and (   ckmlpp~zukumo <> 0 or ckmlpp~vnkumo <> 0
                      or ckmlpp~abkumo <> 0 or ckmlpp~umkumo <> 0
                      or ckmlpp~lbkum <> 0 )
                 and mbew~bklas = '3005'
        order by ckmlhd~kalnr.
    endif.                                                  "UD1K951515
*    endif.                                                 "UD1K951500
* }

    perform get_extra_data_mbew.

    sort t_mats by kalnr.
    delete adjacent duplicates from t_mats comparing kalnr.

*    s_bklas-option = 'EQ'.
*    s_bklas-sign   = 'E'.
*    s_bklas-low    = '3040'.
*    append s_bklas.

    sort t_mats by kalnr.

    perform get_valuation_class.
  endif.

endform.                    " get_materials
*&---------------------------------------------------------------------*
*&      Form  get_extra_data_mbew
*&---------------------------------------------------------------------*
form get_extra_data_mbew.
  types: begin of ty_t134,
           kkref type kkref,
         end of ty_t134.

  types: begin of ty_bklas,
           bklas type bklas,
         end of ty_bklas.

  data: lt_t134   type table of ty_t134  with header line,
        lt_bklas  type table of ty_bklas with header line.
*
  if s_bklas[] is initial.
    refresh: lt_bklas, lt_t134.
    clear  : lt_bklas, lt_t134.

    select kkref into table lt_t134
      from t134
     where mtart in s_mtart
       and kkref <> space.

    select bklas into table lt_bklas
      from t025
      for all entries in lt_t134
     where bklas in s_bklas
       and kkref = lt_t134-kkref.

    s_bklas-option = 'EQ'. s_bklas-sign = 'I'.

    loop at lt_bklas.
      s_bklas-low = lt_bklas-bklas.
      append s_bklas.
    endloop.
    clear s_bklas.
  endif.
*
  data: t_mats2 like t_mats occurs 0 with header line.
  data: t_mats3 like t_mats occurs 0 with header line.

  refresh: t_mats2, t_mats3.

  select
*        MBEWH~BKLAS
         ckmlhd~bwkey ckmlhd~matnr ckmlhd~bwtar
         ckmlhd~kalnr
         makt~maktg
         mara~mtart mara~matkl
         ckmlcr~stprs
         ckmlcr~pvprs as verpr
         ckmlpp~meins
         ckmlpp~status
         ckmlpp~abkumo ckmlpp~umkumo ckmlpp~zukumo
         ckmlpp~vnkumo ckmlpp~lbkum  ckmlpp~ekkumo
         ckmlcr~peinh
         ckmlcr~absalk3  ckmlcr~abprd_o  ckmlcr~abkdm_o
         ckmlcr~abprd_mo ckmlcr~abkdm_mo
         ckmlcr~vpprd_o  ckmlcr~zuprd_o
         ckmlcr~zukdm_o  ckmlcr~vpkdm_o
         ckmlcr~zuprd_mo ckmlcr~zukdm_mo
         ckmlcr~vnprd_ea ckmlcr~vnkdm_ea
         ckmlcr~ebprd_ea ckmlcr~ebkdm_ea
         ckmlcr~vnprd_ma ckmlcr~vnkdm_ma
         ckmlcr~ebprd_ma ckmlcr~ebkdm_ma  marc~prctr

     into corresponding fields of table t_mats2
           from   ckmlpp
                  inner join ckmlhd
                    on  ckmlpp~kalnr = ckmlhd~kalnr
                  inner join mara
                    on  mara~matnr = ckmlhd~matnr
                  inner join makt
                    on  makt~matnr = ckmlhd~matnr
                   and  makt~spras = sy-langu
                  inner join marc
                    on  ckmlhd~matnr = marc~matnr
                    and ckmlhd~bwkey = marc~werks
                  inner join ckmlcr
                    on  ckmlcr~kalnr  = ckmlhd~kalnr
                   and  ckmlcr~bdatj  = p_bdatj
                   and  ckmlcr~poper  = p_poper
                   and  ckmlcr~curtp  = '10'
                   and  ckmlcr~untper = space
             where ckmlpp~bdatj  = p_bdatj
               and ckmlpp~poper  = p_poper
               and ckmlpp~kalnr  = ckmlhd~kalnr
               and ckmlhd~matnr in s_matnr
               and ckmlhd~bwkey in s_bwkey
               and marc~prctr   in s_prctr
*               AND MBEWH~LFGJA = P_BDATJ
*               AND MBEWH~LFMON = P_POPER
*               AND MBEWH~BKLAS IN S_BKLAS
*               and ckmlhd~kalnr <> t_mats-kalnr
               and (   ckmlpp~zukumo <> 0 or ckmlpp~vnkumo <> 0
                    or ckmlpp~abkumo <> 0 or ckmlpp~umkumo <> 0
                    or ckmlpp~lbkum <> 0 ).
*   ORDER BY CKMLHD~KALNR.

  sort t_mats by kalnr.
  loop at t_mats2.
    read table t_mats with key kalnr = t_mats2-kalnr binary search.
    if sy-subrc <> 0 and t_mats2-mtart in s_mtart.
      append t_mats2 to t_mats3.
    endif.
  endloop.

  append lines of t_mats3 to t_mats.

endform.                    " get_extra_data_mbew
*&---------------------------------------------------------------------*
*&      Form  get_material_periods
*&---------------------------------------------------------------------*
form get_material_periods using pt_mats like t_mats[]
                                pt_ckmlpp like t_ckmlpp[]
                                pt_ckmlcr like t_ckmlcr[].

  data: lt_kalnr type ckmv0_matobj_tbl,
        ls_mats type s_mats,
        ls_kalnr type ckmv0_matobj_str.

  if pt_mats[] is initial.
    refresh: pt_ckmlpp, pt_ckmlcr.
    exit.
  endif.
* Periodens?ze lesen
  refresh: lt_kalnr.
  loop at pt_mats into ls_mats.
    clear: ls_kalnr.
    ls_kalnr-kalnr = ls_mats-kalnr.
    ls_kalnr-bwkey = ls_mats-bwkey.
    append ls_kalnr to lt_kalnr.
  endloop.
  call function 'CKMS_PERIOD_READ_WITH_ITAB'
    exporting
*     I_REFRESH_BUFFER          =
*     I_READ_ONLY_BUFFER        = ' '
*     I_USE_BUFFER              = 'X'
*     I_BUILD_SMBEW             =
      i_bdatj_1                 = p_bdatj
      i_poper_1                 = p_poper
*     I_BDATJ_2                 =
*     I_POPER_2                 =
*     I_BDATJ_3                 =
*     I_POPER_3                 =
*     I_BETWEEN_1_AND_2         =
*     i_untper                  =
      i_call_by_reporting       = 'X'
      i_no_chk_periods_complete = 'X'
    tables
      t_kalnr                   = lt_kalnr
      t_ckmlpp                  = pt_ckmlpp
      t_ckmlcr                  = pt_ckmlcr
*     T_MISS_CKMLPP             =
*     T_MISS_CKMLCR             =
    exceptions
      no_data_found             = 1
      input_data_inconsistent   = 2
      buffer_inconsistent       = 3
      others                    = 4.
  if sy-subrc <> 0 and
     not ( sy-subrc = 1 and
           not ( pt_ckmlpp[] is initial and pt_ckmlpp[] is initial ) ).
*   Probleme
    refresh: pt_mats, pt_ckmlpp, pt_ckmlcr.
    exit.
  endif.
  sort: pt_ckmlpp, pt_ckmlcr.


endform.                    " get_material_periods
*&---------------------------------------------------------------------*
*&      Form  find_bad_boys
*&---------------------------------------------------------------------*
form find_bad_boys.
  data: ls_ndi_ndi type s_ndi,
        ls_ndi_cum type s_ndi,
        ls_ndi_nin type s_ndi,
        ls_mats type s_mats,
        ls_ckmlpp type ckmlpp,
        ls_ckmlcr type ckmlcr,
        ls_mlcd type mlcd,
        ls_mlcd_not_alloc type mlcd,
        l_color(3) type c,
        l_ab_menge like mlcd-lbkum,
        l_nin type boole_d,
        l_kalnr_old like mlcd-kalnr.

  clear t_mats.

  loop at t_mats into ls_mats.
    read table t_ckmlpp into ls_ckmlpp
                         with key kalnr = ls_mats-kalnr
                                  bdatj = p_bdatj
                                  poper = p_poper
                                  untper = '000'
                                  binary search.
    check sy-subrc = 0.
    clear: ls_ndi_ndi, ls_ndi_cum, ls_ndi_nin.

    move-corresponding ls_ckmlpp to ls_ndi_ndi.
    move-corresponding ls_ckmlpp to ls_ndi_cum.
    move-corresponding ls_ckmlpp to ls_ndi_nin.

    if ls_ckmlpp-status >= '40'.  "y_einstufig_abgerechnet.
      read table t_ckmlcr with key kalnr = ls_mats-kalnr
                                    bdatj = p_bdatj
                                    poper = p_poper
                                    untper = '000'
                                    binary search
                                    transporting no fields.

      loop at t_ckmlcr into ls_ckmlcr from sy-tabix.
        if ls_ckmlcr-kalnr <> ls_ckmlpp-kalnr or
           ls_ckmlcr-bdatj <> ls_ckmlpp-bdatj or
           ls_ckmlcr-poper <> ls_ckmlpp-poper or
           ls_ckmlcr-untper <> ls_ckmlpp-untper.
          exit.
        endif.

*       Kumulierter Bestand
        move-corresponding ls_ckmlcr to ls_ndi_ndi.
        move-corresponding ls_ckmlcr to ls_ndi_cum.
        move-corresponding ls_ckmlcr to ls_ndi_nin.
        ls_ndi_cum-estprd = ls_ckmlcr-abprd_o + ls_ckmlcr-zuprd_o +
                            ls_ckmlcr-vpprd_o.
        ls_ndi_cum-estkdm = ls_ckmlcr-abkdm_o + ls_ckmlcr-zukdm_o +
                            ls_ckmlcr-vpkdm_o.
*
        if ls_ckmlpp-status >= '50'.  "y_mehrstufig_abgerechnet.
          ls_ndi_cum-mstprd = ls_ckmlcr-abprd_mo + ls_ckmlcr-zuprd_mo.
          ls_ndi_cum-mstkdm = ls_ckmlcr-abkdm_mo + ls_ckmlcr-zukdm_mo.
        else.
          ls_ndi_cum-mstprd = ls_ckmlcr-abprd_mo.
          ls_ndi_cum-mstkdm = ls_ckmlcr-abkdm_mo.
        endif.
*
        ls_ndi_ndi-estprd = ls_ndi_cum-estprd.
        ls_ndi_ndi-estkdm = ls_ndi_cum-estkdm.
        ls_ndi_ndi-mstprd = ls_ndi_cum-mstprd.
        ls_ndi_ndi-mstkdm = ls_ndi_cum-mstkdm.
        ls_ndi_ndi-estprd = ls_ndi_ndi-estprd -
                            ( ls_ckmlcr-vnprd_ea + ls_ckmlcr-ebprd_ea ).
        ls_ndi_ndi-estkdm = ls_ndi_ndi-estkdm -
                            ( ls_ckmlcr-vnkdm_ea + ls_ckmlcr-ebkdm_ea ).
        ls_ndi_ndi-mstprd = ls_ndi_ndi-mstprd -
                            ( ls_ckmlcr-vnprd_ma + ls_ckmlcr-ebprd_ma ).
        ls_ndi_ndi-mstkdm = ls_ndi_ndi-mstkdm -
                            ( ls_ckmlcr-vnkdm_ma + ls_ckmlcr-ebkdm_ma ).
        ls_ndi_ndi-sumdif = ls_ndi_ndi-estprd + ls_ndi_ndi-estkdm +
                            ls_ndi_ndi-mstprd + ls_ndi_ndi-mstkdm.
*       Gibt's eine 'Nicht verrechnet'-Zeile?
        read table t_mlcd_not_alloc into ls_mlcd_not_alloc
                                     with key kalnr = ls_ckmlcr-kalnr
                                              bdatj = ls_ckmlcr-bdatj
                                              poper = ls_ckmlcr-poper
*                                             untper = ls_ckmlcr-untper
*                                             curtp = ls_ckmlcr-curtp.
                                             binary search.
        if sy-subrc = 0.
          l_nin = 'X'.
        else.
          clear: l_nin.
        endif.
        if not ls_ndi_ndi-sumdif is initial or
           not l_nin is initial.
          if ls_ndi_ndi-kalnr <> l_kalnr_old.
            l_kalnr_old = ls_ndi_ndi-kalnr.
            if l_color = 'C21'.
              l_color = 'C20'.
            else.
              l_color = 'C21'.
            endif.
          endif.
          read table t_mats into ls_mats
                             with key kalnr = ls_ndi_ndi-kalnr.
          if sy-subrc = 0.
            move-corresponding ls_mats to ls_ndi_cum.
            move-corresponding ls_mats to ls_ndi_ndi.
            move-corresponding ls_mats to ls_ndi_nin.
          endif.
*Not distributed
          if not ls_ndi_ndi-sumdif is initial.
            ls_ndi_ndi-pos_type = 'NDI'.
            ls_ndi_ndi-color = l_color.
*           ls_ndi_ndi-pos_type_text = text-006.
            clear: ls_ndi_ndi-menge, ls_ndi_ndi-wert.
            ls_ndi_ndi-prdif = ls_ndi_ndi-estprd + ls_ndi_ndi-mstprd.
            ls_ndi_ndi-krdif = ls_ndi_ndi-estkdm + ls_ndi_ndi-mstkdm.
            ls_ndi_ndi-estdif = ls_ndi_ndi-estprd + ls_ndi_ndi-estkdm.
            ls_ndi_ndi-mstdif = ls_ndi_ndi-mstprd + ls_ndi_ndi-mstkdm.
            append ls_ndi_ndi to t_bad.
          endif.
*Not included
          if not l_nin is initial.
            ls_ndi_nin-pos_type = 'NIN'.
            ls_ndi_nin-color = l_color.
*           ls_ndi_nin-pos_type_text = text-007.
            clear: ls_ndi_nin-menge, ls_ndi_nin-wert.
            ls_ndi_nin-estprd = ls_mlcd_not_alloc-estprd.
            ls_ndi_nin-estkdm = ls_mlcd_not_alloc-estkdm.
            ls_ndi_nin-mstprd = ls_mlcd_not_alloc-mstprd.
            ls_ndi_nin-mstkdm = ls_mlcd_not_alloc-mstkdm.
            ls_ndi_nin-prdif = ls_ndi_nin-estprd + ls_ndi_nin-mstprd.
            ls_ndi_nin-krdif = ls_ndi_nin-estkdm + ls_ndi_nin-mstkdm.
            ls_ndi_nin-estdif = ls_ndi_nin-estprd + ls_ndi_nin-estkdm.
            ls_ndi_nin-mstdif = ls_ndi_nin-mstprd + ls_ndi_nin-mstkdm.
            ls_ndi_nin-sumdif = ls_ndi_nin-estprd + ls_ndi_nin-estkdm +
                                  ls_ndi_nin-mstprd + ls_ndi_nin-mstkdm.
            append ls_ndi_nin to t_bad.
          endif.

        endif.
      endloop.
    else.
*   Da kommt noch was!
    endif.
  endloop.


endform.                    " find_bad_boys
*&---------------------------------------------------------------------*
*&      Form  find_bad_boys
*&---------------------------------------------------------------------*
form find_bad_boys_new.
* refer MLHELP_VALUE_FLOW_ANALYZER program
  data: ls_ndi_ndi type s_ndi,
        ls_ndi_cum type s_ndi,
        ls_ndi_nin type s_ndi,
        ls_mats type s_mats,
        ls_ckmlpp type ckmlpp,
        ls_ckmlcr type ckmlcr,
        ls_mlcd type mlcd,
        ls_mlcd_not_alloc type mlcd,
        l_color(3) type c,
        l_ab_menge like mlcd-lbkum,
        l_nin type boole_d,
        l_kalnr_old like mlcd-kalnr.

  clear t_mats.
  loop at t_mats into ls_mats.
    move-corresponding ls_mats to ls_ckmlpp.
    move-corresponding ls_mats to ls_ckmlcr.

    clear: ls_ndi_ndi, ls_ndi_cum, ls_ndi_nin.
    move-corresponding ls_ckmlpp  to ls_ndi_ndi.
    move-corresponding ls_ckmlpp  to ls_ndi_cum.
    move-corresponding ls_ckmlpp  to ls_ndi_nin.

    if ls_ckmlpp-status >= '40'.  "y_einstufig_abgerechnet.
      move-corresponding ls_ckmlcr  to ls_ndi_ndi.
      move-corresponding ls_ckmlcr  to ls_ndi_cum.
      move-corresponding ls_ckmlcr  to ls_ndi_nin.

      ls_ndi_cum-estprd = ls_mats-abprd_o + ls_mats-zuprd_o +
                          ls_mats-vpprd_o.
      ls_ndi_cum-estkdm = ls_mats-abkdm_o + ls_mats-zukdm_o +
                          ls_mats-vpkdm_o.
*
      if ls_ckmlpp-status >= '50'.  "y_mehrstufig_abgerechnet.
        ls_ndi_cum-mstprd = ls_mats-abprd_mo + ls_mats-zuprd_mo.
        ls_ndi_cum-mstkdm = ls_mats-abkdm_mo + ls_mats-zukdm_mo.
      else.
        ls_ndi_cum-mstprd = ls_mats-abprd_mo.
        ls_ndi_cum-mstkdm = ls_mats-abkdm_mo.
      endif.
*
      ls_ndi_ndi-estprd = ls_ndi_cum-estprd.
      ls_ndi_ndi-estkdm = ls_ndi_cum-estkdm.
      ls_ndi_ndi-mstprd = ls_ndi_cum-mstprd.
      ls_ndi_ndi-mstkdm = ls_ndi_cum-mstkdm.
      ls_ndi_ndi-estprd = ls_ndi_ndi-estprd -
                          ( ls_mats-vnprd_ea + ls_mats-ebprd_ea ).
      ls_ndi_ndi-estkdm = ls_ndi_ndi-estkdm -
                          ( ls_mats-vnkdm_ea + ls_mats-ebkdm_ea ).
      ls_ndi_ndi-mstprd = ls_ndi_ndi-mstprd -
                          ( ls_mats-vnprd_ma + ls_mats-ebprd_ma ).
      ls_ndi_ndi-mstkdm = ls_ndi_ndi-mstkdm -
                          ( ls_mats-vnkdm_ma + ls_mats-ebkdm_ma ).
      ls_ndi_ndi-sumdif = ls_ndi_ndi-estprd + ls_ndi_ndi-estkdm +
                          ls_ndi_ndi-mstprd + ls_ndi_ndi-mstkdm.
*       Gibt's eine 'Nicht verrechnet'-Zeile?
      read table t_mlcd_not_alloc into ls_mlcd_not_alloc
                                   with key kalnr = ls_mats-kalnr
                                            bdatj = p_bdatj
                                            poper = p_poper
*                                             untper = ls_ckmlcr-untper
*                                             curtp = ls_ckmlcr-curtp.
                                           binary search.
      if sy-subrc = 0.
        l_nin = 'X'.
      else.
        clear: l_nin.
      endif.

      if not ls_ndi_ndi-sumdif is initial or
         not l_nin is initial.
        if ls_ndi_ndi-kalnr <> l_kalnr_old.
          l_kalnr_old = ls_ndi_ndi-kalnr.
          if l_color = 'C21'.
            l_color = 'C20'.
          else.
            l_color = 'C21'.
          endif.
        endif.
*        READ TABLE t_mats INTO ls_mats
*                           WITH KEY kalnr = ls_ndi_ndi-kalnr
*                           BINARY SEARCH.
*        IF sy-subrc = 0.
        move-corresponding ls_mats to ls_ndi_cum.
        move-corresponding ls_mats to ls_ndi_ndi.
        move-corresponding ls_mats to ls_ndi_nin.
*        ENDIF.
*Not distributed
        if not ls_ndi_ndi-sumdif is initial.
          ls_ndi_ndi-pos_type = 'NDI'.
          ls_ndi_ndi-color = l_color.
*           ls_ndi_ndi-pos_type_text = text-006.
          clear: ls_ndi_ndi-menge, ls_ndi_ndi-wert.
          ls_ndi_ndi-prdif = ls_ndi_ndi-estprd + ls_ndi_ndi-mstprd.
          ls_ndi_ndi-krdif = ls_ndi_ndi-estkdm + ls_ndi_ndi-mstkdm.
          ls_ndi_ndi-estdif = ls_ndi_ndi-estprd + ls_ndi_ndi-estkdm.
          ls_ndi_ndi-mstdif = ls_ndi_ndi-mstprd + ls_ndi_ndi-mstkdm.
          append ls_ndi_ndi to t_bad.
        endif.
*Not included
        if not l_nin is initial.
          ls_ndi_nin-pos_type = 'NIN'.
          ls_ndi_nin-color = l_color.
*           ls_ndi_nin-pos_type_text = text-007.
          clear: ls_ndi_nin-menge, ls_ndi_nin-wert.
          ls_ndi_nin-estprd = ls_mlcd_not_alloc-estprd.
          ls_ndi_nin-estkdm = ls_mlcd_not_alloc-estkdm.
          ls_ndi_nin-mstprd = ls_mlcd_not_alloc-mstprd.
          ls_ndi_nin-mstkdm = ls_mlcd_not_alloc-mstkdm.
          ls_ndi_nin-prdif = ls_ndi_nin-estprd + ls_ndi_nin-mstprd.
          ls_ndi_nin-krdif = ls_ndi_nin-estkdm + ls_ndi_nin-mstkdm.
          ls_ndi_nin-estdif = ls_ndi_nin-estprd + ls_ndi_nin-estkdm.
          ls_ndi_nin-mstdif = ls_ndi_nin-mstprd + ls_ndi_nin-mstkdm.
          ls_ndi_nin-sumdif = ls_ndi_nin-estprd + ls_ndi_nin-estkdm +
                                ls_ndi_nin-mstprd + ls_ndi_nin-mstkdm.
          append ls_ndi_nin to t_bad.
        endif.

      endif.
    else.
*   Da kommt noch was!
    endif.
  endloop.


endform.                    " find_bad_boys
*&---------------------------------------------------------------------*
*&      Form  fill_ndi_nin
*&---------------------------------------------------------------------*
form fill_ndi_nin.
* Not Include --> Consumption Diff.
  loop at t_bad.
    clear it_mlcd.

    it_mlcd-kalnr  = t_bad-kalnr.
    it_mlcd-bdatj  = t_bad-bdatj.
    it_mlcd-poper  = t_bad-poper.
*   it_mlcd-UNTPER = t_bad-UNTPER.
    it_mlcd-curtp  = t_bad-curtp.
*    it_mlcd-MATNR  = t_bad-MATNR.
*    it_mlcd-BWKEY  = t_bad-BWKEY.
*    it_mlcd-BWTAR  = t_bad-BWTAR.

*different sign!!! ??? --> NO by ANDY CHOI
    if t_bad-pos_type = 'NDI'.
      it_mlcd-categ = 'ND'.
      it_mlcd-estprd = - t_bad-estprd.
      it_mlcd-estkdm = - t_bad-estkdm.
      it_mlcd-mstprd = - t_bad-mstprd.
      it_mlcd-mstkdm = - t_bad-mstkdm.
    else.
      it_mlcd-categ = 'NI'.
      it_mlcd-estprd = - t_bad-estprd.
      it_mlcd-estkdm = - t_bad-estkdm.
      it_mlcd-mstprd = - t_bad-mstprd.
      it_mlcd-mstkdm = - t_bad-mstkdm.
    endif.

    perform trans_val_to_dis.
  endloop.

endform.                    " fill_ndi_nin
*&---------------------------------------------------------------------*
*&      Form  delete_zero_record
*&---------------------------------------------------------------------*
form delete_zero_record.
* Exclude records which have zero values
  delete it_mlsum where ab_lbkum eq 0 and
                        ab_salk3 eq 0 and
                        ab_rd    eq 0 and
                        pc_lbkum eq 0 and
                        pc_salk3 eq 0 and
                        pc_rd eq 0    and
                        zn_lbkum eq 0 and
                        zn_salk3 eq 0 and
                        zn_rd eq 0    and
                        zu_lbkum eq 0 and
                        zu_salk3 eq 0 and
                        zu_rd eq 0    and
                        zo_lbkum eq 0 and
                        zo_salk3 eq 0 and
                        zo_rd eq 0    and
                        zm_lbkum eq 0 and
                        zm_salk3 eq 0 and
                        zm_rd eq 0    and
                        vp_lbkum eq 0 and
                        vp_salk3 eq 0 and
                        vp_rd eq 0    and
                        nd_rd eq 0    and
                        kb_lbkum eq 0 and
                        kb_salk3 eq 0 and
                        kb_rd eq 0    and
                        kb_lbkum eq 0 and
                        kb_salk3 eq 0 and
                        kb_rd eq 0    and
                        vn_lbkum eq 0 and
                        vn_salk3 eq 0 and
                        vn_rd eq 0    and
                        vx_lbkum eq 0 and
                        vx_salk3 eq 0 and
                        vx_rd eq 0    and
                        vk_lbkum eq 0 and
                        vk_salk3 eq 0 and
                        vk_rd eq 0    and
                        vo_lbkum eq 0 and
                        vo_salk3 eq 0 and
                        vo_rd eq 0    and

                          vu_lbkum eq 0 and
                          vu_salk3 eq 0 and
                          vu_rd eq 0    and

                          vm_lbkum eq 0 and
                          vm_salk3 eq 0 and
                          vm_rd eq 0    and

                          ni_rd eq 0    and

                          eb_lbkum eq 0 and
                          eb_salk3 eq 0.


endform.                    " delete_zero_record
*&---------------------------------------------------------------------*
*&      Form  update_record
*&---------------------------------------------------------------------*
form update_record.
  check p_up = 'X'.

*delete old data in the table
  delete from ztco_ml_summary
         where bdatj = p_bdatj
           and poper = p_poper
           and matnr in s_matnr
           and mtart in s_mtart
           and bwkey in s_bwkey
           and bklas in s_bklas.

* No Check Subrc
  commit work.

  data: lw_tka01 like tka01.
  select single * into lw_tka01 from tka01 where kokrs = p_kokrs.

  loop at it_mlsum.
    move-corresponding  it_mlsum to it_ml_summary.
    it_ml_summary-mandt = sy-mandt.
    it_ml_summary-kokrs = p_kokrs.
    it_ml_summary-bdatj = p_bdatj.
    it_ml_summary-poper = p_poper.
    it_ml_summary-waers = lw_tka01-waers.
    append it_ml_summary.
  endloop.

  insert ztco_ml_summary from table it_ml_summary.

  if sy-subrc <> 0.
    call function 'FI_PROGRESS_INDICATOR'
      exporting
        text = 'ERROR DB INSERT'.
  else.
    message s000 with 'Buffer saved!'.
  endif.
endform.                    " update_record
*&---------------------------------------------------------------------*
*&      Form  get_data_from_buffer
*&---------------------------------------------------------------------*
form get_data_from_buffer.
  read table s_poper index 1.
  if s_poper-high is initial.
    s_poper-high = s_poper-low.
  endif.

  select * into corresponding fields of it_mlsum
       from ztco_ml_summary as a
       inner join makt as k
          on k~matnr = a~matnr
         and k~spras = sy-langu
       inner join marc as c
          on a~matnr = c~matnr
         and a~bwkey = c~werks
     where a~kokrs = p_kokrs
       and a~bdatj = p_bdatj
       and a~poper in s_poper
       and a~matnr in s_matnr
       and a~bklas in s_bklas
       and a~mtart in s_mtart
       and a~vklas in s_vklas
       and c~prctr in s_prctr.

    if s_poper-low <> s_poper-high.
      if it_mlsum-poper <> s_poper-low.
        clear: it_mlsum-ab_salk3,
               it_mlsum-ab_rd,
               it_mlsum-ab_lbkum.
      endif.
      if it_mlsum-poper <> s_poper-high.
        clear: it_mlsum-eb_salk3,
               it_mlsum-eb_rd,
               it_mlsum-eb_lbkum.
      endif.
    endif.

    clear: it_mlsum-poper.

    collect it_mlsum.clear it_mlsum.
  endselect.

endform.                    " get_data_from_buffer
*&---------------------------------------------------------------------*
*&      Form  read_mlcrf_prev_pst_var
*&---------------------------------------------------------------------*
form read_mlcrf_prev_pst_var.
  data: begin of lt_mlcrf occurs 1000,
          kalnr like mlit-kalnr   ,
          prdif like mlcrf-prdif  ,
          krdif like mlcrf-krdif  ,
        end of lt_mlcrf.

  data l_idx like sy-tabix.

* price change; MLIT-PSART = PC, PTYP = SPACE
* posting prev period ...; PTYP = VKA, ...
  loop at t_mats.
    select mlit~kalnr sum( mlcrf~prdif ) sum( mlcrf~krdif )
    appending table  lt_mlcrf
              from mlcrf
                inner join mlit
                   on mlcrf~belnr =  mlit~belnr
                  and mlcrf~kjahr =  mlit~kjahr
                  and mlcrf~posnr =  mlit~posnr
              where mlcrf~bdatj = p_bdatj
                and mlcrf~poper = p_poper
                and mlcrf~curtp = p_curtp
                and mlcrf~feldg = 'UMO'
                and mlit~kalnr  = t_mats-kalnr
                and mlit~ptyp   ne space
              group by mlit~kalnr.

*- U1 Start
*    IF p_arch EQ 'X'.
*      PERFORM archive_read_mlcrf.
*
*      LOOP AT gt_mlcrf.
*        CLEAR lt_mlcrf.
*        MOVE-CORRESPONDING gt_mlcrf TO lt_mlcrf.
*        COLLECT lt_mlcrf.
*      ENDLOOP.
*    ENDIF.
*- U1 End

  endloop.

  sort it_mlsum by kalnr.

  loop at lt_mlcrf.
    clear l_idx.

    read table it_mlsum with key kalnr = lt_mlcrf-kalnr
                        binary search.
    l_idx = sy-tabix.

    if sy-subrc = 0.
      it_mlsum-ab_salk3 = it_mlsum-ab_salk3
                          + lt_mlcrf-prdif + lt_mlcrf-krdif.

      it_mlsum-ab_rd    = it_mlsum-ab_rd
                          - lt_mlcrf-prdif - lt_mlcrf-krdif.

      it_mlsum-pc_salk3 = it_mlsum-pc_salk3
                          - lt_mlcrf-prdif - lt_mlcrf-krdif.

      it_mlsum-pc_rd    = it_mlsum-pc_rd
                          + lt_mlcrf-prdif + lt_mlcrf-krdif.

      modify it_mlsum index l_idx.
    endif.

  endloop.

endform.                    " read_mlcrf_prev_pst_var
*&---------------------------------------------------------------------*
*&      Form  call_alv_list
*&---------------------------------------------------------------------*
form call_alv_list.
  perform field_setting(zcogsrev) tables gt_fieldcat using :
 'BDATJ'     'Year'        '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'POPER'     'Mon'         '03' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'MTART'     'Mtyp'        '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'BKLAS'     'V.CL'        '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'MATNR'     'Material'    '18' 'X' 'L'  ' '  ' '  '  ' ' '  ' ',
 'MAKTG'     'Name'        '25' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'ZEINR'     'Document'    '22' ' ' 'L'  ' '  ' '  '  ' ' '  ' ', "VAL
* BEGIN OF UD1K952838
 'ZGRP2'     'Color Code'  '22' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'ZGRP3'     'Model'       '22' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'ZGRP4'     'Sub Part Group' '22' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
* END OF UD1K952838
* BEGIN OF UD1K952897
 'ZGRP5'     'Field-5'     '22' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
* END OF UD1K952897
* BEGIN OF UQ1K900343
  'ZGRP6'    'Engine Capacity' '22' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'ZGRP7'    'Engine Type'     '22' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'ZGRP8'    'Transmission'    '22' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'ZGRP9'    'Model Year'      '22' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'ZGRP10'   'Field-10'        '22' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
* END OF UQ1K900343
 'BWKEY'     'Plant'       '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'BWTAR'     'V.TYP'       '06' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'KALNR'     'CostNo'      '12' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'MEINS'     'UoM'         '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',

'AB_LBKUM'    ' BIG   '    '16' ' ' 'R' ' ' ' ' ' ' 'MEINS' ' ',
'AB_SALK3'    '$BIG  S'    '16' ' ' 'R' ' ' ' ' ' ' '     '  ' ',
'AB_RD'       '$BIG  V'    '16' ' ' 'R' ' ' ' ' ' ' '     '  ' ',

'PC_LBKUM'    ' PChg'      '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
'PC_SALK3'    '$Pchg S'    '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
'PC_RD'       '$Pchg V'    '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',

'ZN_LBKUM'    ' GR MM '    '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
'ZN_SALK3'    '$GR MMS'    '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
'ZN_RD'       '$GR MMV'    '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',

'ZF_LBKUM'    ' GR PP '    '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
'ZF_SALK3'    '$GR PPS'    '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
'ZF_RD'       '$GR PPV'    '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',

'ZO_LBKUM'    ' GR OT'    '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
'ZO_SALK3'    '$GR OTS'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
'ZO_RD'       '$GR OTV'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',

'ZU_LBKUM'    ' GR TRF'    '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
'ZU_SALK3'    '$GR TRFS'    '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
'ZU_RD'       '$GR TRFV'    '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',

'ZM_LBKUM'    ' GR M2M'    '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
'ZM_SALK3'    '$GR M2MS'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
'ZM_RD'       '$GR M2MV'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',

'VP_LBKUM'    ' GR D/C'    '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
'VP_SALK3'    '$GR D/CS'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
'VP_RD'       '$GR D/CV'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',

'ND_RD'       '$GR NoDst'  '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',

'KB_LBKUM'   ' Cumulative'  '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
'KB_SALK3'   '$CumulativeS' '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
'KB_RD'      '$CumulativeV' '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',

'VN_LBKUM'    ' GI MM  '   '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
'VN_SALK3'    '$GI MMS '   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
'VN_RD'       '$GI MMV '   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',

'VX_LBKUM'    ' GI CC2  '   '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
'VX_SALK3'    '$GI CC2 S'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
'VX_RD'       '$GI CC2 V'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',


'VK_LBKUM'    ' GI CC  '   '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
'VK_SALK3'    '$GI CC S'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
'VK_RD'       '$GI CC V'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',

*'ZV_LBKUM'    ' GR Phy'   '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
*'ZV_SALK3'    '$GR PhyS'  '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'ZV_RD'       '$GR PhyV'  '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*
*'BU_LBKUM'    ' GR TRF '   '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
*'BU_SALK3'    '$GR TRFS'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'BU_RD'       '$GR TRFV'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'BM_LBKUM'    ' GR M2M'    '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
*'BM_SALK3'    '$GR M2MS'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'BM_RD'       '$GR M2MV'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*

'VO_LBKUM'    ' GI OR  '   '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
'VO_SALK3'    '$GI OR S'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
'VO_RD'       '$GI OR V'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',

*'VV_LBKUM'    ' GI Phy '   '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
*'VV_SALK3'    '$GI PhyS'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'VV_RD'       '$GI PhyV'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
'VU_LBKUM'  ' GI TRF'    '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
'VU_SALK3'  '$GI TRFS'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
'VU_RD'     '$GI TRFV'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',

'VM_LBKUM'  ' GI M2M '   '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
'VM_SALK3'  '$GI M2M S'  '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
'VM_RD'     '$GI M2M V'  '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',

'NI_RD'       '$GI NoInc'  '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',

'EB_LBKUM'       ' End Q'     '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
'EB_SALK3'       '$End S'     '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
'EB_RD'          '$End V'     '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
'PRCTR'          'PRCTR'      '10' ' ' 'L' ' ' ' ' ' ' '     '  'X'.


  perform init_alv_parm.


  g_repid = sy-repid.

  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_callback_program      = g_repid
      it_fieldcat             = gt_fieldcat
      i_callback_user_command = g_user_command
      i_save                  = 'A'
      is_variant              = gs_variant
    tables
      t_outtab                = it_mlsum
    exceptions
      program_error           = 1
      others                  = 2.

endform.                    " call_alv_list
*&---------------------------------------------------------------------*
*&      Form  get_proc_kalnr
*&---------------------------------------------------------------------*
form get_proc_kalnr.
  select werks matnr bwtar proc_kalnr btyp kalnr
     into table i_proc_kalnr
     from ckmlmv001
     for all entries in t_mats
     where bwkey = t_mats-bwkey
       and matnr = t_mats-matnr
       and bwtar = t_mats-bwtar.

  sort i_proc_kalnr by prock.

endform.                    " get_proc_kalnr
*&---------------------------------------------------------------------*
*&      Form  read_prev_pst_var2
*&---------------------------------------------------------------------*
form read_prev_pst_var2.
  data: begin of lt_mats_prev occurs 0,
          kalnr like ckmlcr-kalnr,
          stprs like ckmlcr-stprs,
          peinh like ckmlcr-peinh,
          salk3 like ckmlcr-salk3,
        end of lt_mats_prev.

  data: l_pryr like mlcd_key-bdatj,
        l_prmn like mlcd_key-poper,
        l_diff  like ckmlcr-salk3,
        w_mlsum like it_mlsum.

  clear: l_prmn, l_pryr.

  l_prmn = p_poper - 1.

  if l_prmn = 0.
    l_prmn = 12.
    l_pryr = p_bdatj - 1.
  else.
    l_pryr = p_bdatj.
  endif.

  if not t_mats[] is initial.
    select * into corresponding fields of table lt_mats_prev
        from ckmlcr
        for all entries in t_mats
        where kalnr = t_mats-kalnr
          and bdatj = l_pryr
          and poper = l_prmn.
    sort lt_mats_prev by kalnr.
  endif.

*only price changed...
  loop at it_mlsum into w_mlsum.
    read table t_mats with key kalnr = w_mlsum-kalnr
         binary search.
    read table lt_mats_prev with key kalnr = w_mlsum-kalnr
         binary search.

    if sy-subrc = 0.
      clear l_diff.
      l_diff = w_mlsum-ab_lbkum * ( t_mats-stprs / t_mats-peinh -
                              lt_mats_prev-stprs / lt_mats_prev-peinh ).

      it_mlsum-kalnr = w_mlsum-kalnr.
      it_mlsum-ab_salk3 = - l_diff.
      it_mlsum-ab_rd    = + l_diff.
      it_mlsum-pc_salk3 = + l_diff.
      it_mlsum-pc_rd    = - l_diff.
      collect it_mlsum. clear it_mlsum.
    endif.
  endloop.

*zero inventory... clear (simplified method)
  loop at it_mlsum where ab_lbkum = 0.
    it_mlsum-ab_salk3 = it_mlsum-ab_salk3 + it_mlsum-pc_salk3.
    it_mlsum-pc_salk3 = 0.
    modify it_mlsum index sy-tabix.
  endloop.

endform.                    " read_prev_pst_var2
*&---------------------------------------------------------------------*
*&      Form  get_valuation_class
*&---------------------------------------------------------------------*
form get_valuation_class.
* CKMLMV011 - valuation class ; incorrect
* MBEWH, MBEW only...
  tables: mbewh.
  data: lt_ztco_mat like ztco_mat occurs 0 with header line. "UD1K952048
  data: lw_mbewh like mbewh.
  data: lt_mbewh like mbewh occurs 0 with header line.
  refresh: lt_mbewh.

** Added by Furong on 10/28/11 for performance
  if t_mats[] is not initial.
** End on 10/28/11

*   BEGIN OF UD1K952048
    select * into table lt_ztco_mat
         from ztco_mat
         for all entries in t_mats
         where matnr = t_mats-matnr.
    sort lt_ztco_mat by matnr.
*   END OF UD1K952048

    select * into table lt_mbewh
         from mbewh
         for all entries in t_mats
         where matnr = t_mats-matnr
           and bwkey = t_mats-bwkey
           and lfgja = p_bdatj
           and lfmon = p_poper.
    select * appending corresponding fields of table lt_mbewh
         from mbew
         for all entries in t_mats
         where matnr = t_mats-matnr
           and bwkey = t_mats-bwkey
           and lfgja = p_bdatj
           and lfmon <= p_poper.
    select * appending corresponding fields of table lt_mbewh
         from mbew
         for all entries in t_mats
         where matnr = t_mats-matnr
           and bwkey = t_mats-bwkey
           and lfgja < p_bdatj.

    sort lt_mbewh by matnr bwkey bwtar.
  endif.

  data: l_idx type i.
  loop at t_mats.
    l_idx = sy-tabix.

* BEGIN OF UD1K952048
    read table lt_ztco_mat with key matnr = t_mats-matnr
                                binary search.
    if sy-subrc = 0.
      t_mats-zeinr = lt_ztco_mat-zgrp1.
      t_mats-zgrp2 = lt_ztco_mat-zgrp2.                     "UD1K952838
      t_mats-zgrp3 = lt_ztco_mat-zgrp3.                     "UD1K952838
      t_mats-zgrp4 = lt_ztco_mat-zgrp4.                     "UD1K952838
      t_mats-zgrp5 = lt_ztco_mat-zgrp5.                     "UD1K952897
      t_mats-zgrp6 = lt_ztco_mat-zgrp6.                     "UQ1K900343
      t_mats-zgrp7 = lt_ztco_mat-zgrp7.                     "UQ1K900343
      t_mats-zgrp8 = lt_ztco_mat-zgrp8.                     "UQ1K900343
      t_mats-zgrp9 = lt_ztco_mat-zgrp9.                     "UQ1K900343
      t_mats-zgrp10 = lt_ztco_mat-zgrp10.                   "UQ1K900343
    endif.
* END OF UD1K952048

    clear: lw_mbewh.
    read table lt_mbewh into lw_mbewh
                        with key matnr = t_mats-matnr
                                 bwkey = t_mats-bwkey
                                 bwtar = t_mats-bwtar
                             binary search.
    if sy-subrc <> 0.
      select * from mbewh
           where lfgja = p_bdatj
             and lfmon < p_poper
             and matnr = t_mats-matnr
             and bwkey = t_mats-bwkey
             order by lfmon descending.
        lw_mbewh = mbewh. exit.
      endselect.
      if sy-subrc <> 0.
        select * from mbewh
             where lfgja < p_bdatj
               and matnr = t_mats-matnr
               and bwkey = t_mats-bwkey
               order by lfgja lfmon descending.
          lw_mbewh = mbewh. exit.
        endselect.
      endif.
    endif.

    if sy-subrc <> 0.
      break-point.
    endif.

    check lw_mbewh-bklas in s_bklas.
    t_mats-bklas = lw_mbewh-bklas.
    modify t_mats index l_idx transporting bklas zeinr      "UD1K952838
                              zgrp2 zgrp3 zgrp4 zgrp5       "UD1K952897
                        zgrp6 zgrp7 zgrp8 zgrp9 zgrp10.     "UQ1K900343
*                             zgrp2 zgrp3 zgrp4.            "UD1K952838
*   MODIFY t_mats INDEX l_idx TRANSPORTING bklas zeinr.     "UD1K952048
*   MODIFY t_mats INDEX l_idx TRANSPORTING bklas.           "UD1K952048
  endloop.

  sort t_mats by bklas.
  delete t_mats where bklas is initial.

  sort t_mats by kalnr.
endform.                    " get_valuation_class
*&---------------------------------------------------------------------*
*&      Form  ALV_VARIANT_F4
*&---------------------------------------------------------------------*
form alv_variant_f4 changing p_vari.
  data: rs_variant like disvariant,
        lv_nof4 type c.

  clear lv_nof4.
  loop at screen.
    if screen-name = 'PA_VARI'.
      if screen-input = 0.
        lv_nof4 = 'X'.
      endif.
    endif.
  endloop.

  clear rs_variant.
  rs_variant-report   = sy-repid.
  rs_variant-username = sy-uname.

  call function 'REUSE_ALV_VARIANT_F4'
    exporting
      is_variant = rs_variant
      i_save     = 'A'
    importing
      es_variant = rs_variant
    exceptions
      others     = 1.

  if sy-subrc = 0 and lv_nof4 = space.
    p_vari = rs_variant-variant.
  endif.

endform.                    " ALV_VARIANT_F4
*&---------------------------------------------------------------------*
*&      Form  INIT_ALV_PARM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form init_alv_parm.

  clear   :  gs_layout.

  gs_layout-colwidth_optimize = 'X'.

*   Set variant
  gs_variant-report = sy-repid.
  gs_variant-variant = p_vari.


endform.                    " INIT_ALV_PARM
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_MLCRF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM archive_read_mlcrf .
*
*  TYPES: BEGIN OF ty_mlcrf,
*         bdatj TYPE bdatj,
*         poper TYPE poper,
*         curtp TYPE curtp,
*         feldg TYPE ck_feldg,
*         kalnr TYPE ck_kalnr,
*         ptyp  TYPE ck_ptyp_org,
*           archivekey TYPE arkey,
*           archiveofs TYPE admi_offst.
*  TYPES: END OF ty_mlcrf.
*
*  DATA: l_handle    TYPE sytabix,
*        lt_mlcrf    TYPE TABLE OF mlcrf WITH HEADER LINE,
*        lt_mlit     TYPE TABLE OF mlit  WITH HEADER LINE,
*        l_archindex LIKE aind_str2-archindex,
*        l_gentab    LIKE aind_str2-gentab.
*
*  DATA: lt_inx_mlcrf TYPE TABLE OF ty_mlcrf,
*        ls_inx_mlcrf TYPE ty_mlcrf.
*
** 1. Input the archive infostructure name
*  CLEAR l_archindex.
*  l_archindex = 'ZMLCRF_001'.
*
** 2. Get the structure table using infostructure
*  CLEAR l_gentab.
*  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
*   WHERE archindex = l_archindex.
*
*  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.
*
** 3. Get the archived data from structure table
*  CLEAR lt_inx_mlcrf[].
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_mlcrf
*    FROM (l_gentab)
*   WHERE bdatj = p_bdatj
*     AND poper = p_poper
*     AND curtp = p_curtp
*     AND feldg = 'UMO'
*     AND kalnr = t_mats-kalnr
*     AND ptyp  NE space.
*
*  CHECK NOT lt_inx_mlcrf[] IS INITIAL.
*
** 4. Get more archived data looping structure table
*  CLEAR: gt_mlcrf[], gt_mlcrf[].
*  LOOP AT lt_inx_mlcrf INTO ls_inx_mlcrf.
**  4.1 Read information from archivekey & offset
*    CLEAR l_handle.
*    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
*      EXPORTING
*        object                    = 'CO_ML_BEL'
*        archivkey                 = ls_inx_mlcrf-archivekey
*        offset                    = ls_inx_mlcrf-archiveofs
*      IMPORTING
*        archive_handle            = l_handle
*      EXCEPTIONS
*        no_record_found           = 1
*        file_io_error             = 2
*        internal_error            = 3
*        open_error                = 4
*        cancelled_by_user         = 5
*        archivelink_error         = 6
*        object_not_found          = 7
*        filename_creation_failure = 8
*        file_already_open         = 9
*        not_authorized            = 10
*        file_not_found            = 11
*        error_message             = 12
*        OTHERS                    = 13.
*
*    CHECK sy-subrc = 0.
*
**  4.2 Read table from information
*    CLEAR: lt_mlcrf, lt_mlcrf[].
*    CALL FUNCTION 'ARCHIVE_GET_TABLE'
*      EXPORTING
*        archive_handle          = l_handle
*        record_structure        = 'MLCRF'
*        all_records_of_object   = 'X'
*      TABLES
*        table                   = lt_mlcrf
*      EXCEPTIONS
*        end_of_object           = 1
*        internal_error          = 2
*        wrong_access_to_archive = 3
*        OTHERS                  = 4.
*
*    CHECK sy-subrc = 0 AND NOT lt_mlcrf[] IS INITIAL.
*
*    CLEAR: lt_mlit, lt_mlit[].
*    CALL FUNCTION 'ARCHIVE_GET_TABLE'
*      EXPORTING
*        archive_handle          = l_handle
*        record_structure        = 'MLIT'
*        all_records_of_object   = 'X'
*      TABLES
*        table                   = lt_mlit
*      EXCEPTIONS
*        end_of_object           = 1
*        internal_error          = 2
*        wrong_access_to_archive = 3
*        OTHERS                  = 4.
*
*    CHECK sy-subrc = 0 AND NOT lt_mlit[] IS INITIAL.
*
** 5. Append archived data table to finally interal table
*    LOOP AT lt_mlcrf.
*      CLEAR lt_mlit.
*      READ TABLE lt_mlit WITH KEY belnr = lt_mlcrf-belnr
*                                  kjahr = lt_mlcrf-kjahr
*                                  posnr = lt_mlcrf-posnr.
*      CHECK sy-subrc = 0.
*
*      gt_mlcrf-kalnr = lt_mlit-kalnr.
*      gt_mlcrf-prdif = lt_mlcrf-prdif.
*      gt_mlcrf-krdif = lt_mlcrf-prdif.
*      COLLECT gt_mlcrf.  CLEAR gt_mlcrf.
*    ENDLOOP.
*  ENDLOOP.
*
*ENDFORM.                    " ARCHIVE_READ_MLCRF

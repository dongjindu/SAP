*----------------------------------------------------------------------*
*   INCLUDE ZSTKBTOP                                                   *
*----------------------------------------------------------------------*

type-pools: slis.
data: begin of wa_mara,
        matnr like mara-matnr,
        mtart like mara-mtart,
        matkl like mara-matkl,
        profl like mara-profl,
      end of wa_mara.
data: begin of wa_marc,
        matnr like mara-matnr,
        werks like marc-werks,
        ekgrp like marc-ekgrp,
        dispo like marc-dispo,
      end of wa_marc.
data: begin of wa_mard,
        matnr like mard-matnr,
        werks like mard-werks,
        labst like mard-labst,
        insme like mard-insme,
        speme like mard-speme,
        einme like mard-einme,
        retme like mard-retme,
      end of wa_mard.
data: begin of wa_mbew,
        matnr like mbew-matnr,
        bwkey like mbew-bwkey,
        lbkum like mbew-lbkum,
        stprs like mbew-stprs,
        peinh like mbew-peinh,
     end of wa_mbew.
data: begin of wa_stk_info,
        matnr like mard-matnr,
        werks like mard-werks,
        menge like mard-labst,
       end of wa_stk_info.
data: begin of wa_mseg,
        matnr like mseg-matnr,
        werks like mseg-werks,
        bwart like mseg-bwart,
        shkzg like mseg-shkzg,
        menge like mseg-menge,
        mkdel type c,
      end of wa_mseg.
data: begin of wa_matl,
        matnr like mara-matnr,
        werks like marc-werks,
        mtart like mara-mtart,
        matkl like mara-matkl,
        profl like mara-profl,
        ekgrp like marc-ekgrp,
        dispo like marc-dispo,
      end of wa_matl.
data: begin of wa_grp_mat,
        matnr like mseg-matnr,
        werks like mseg-werks,
        mvgrp(3) type c,
        menge like mseg-menge,
      end of wa_grp_mat.
data: begin of wa_sum_mat,
       matnr like mseg-matnr,
       werks like mseg-werks,
       shkzg like mseg-shkzg,
       menge like mseg-menge,
      end of wa_sum_mat.
data: begin of wa_result,
        werks like mseg-werks,
        matnr like mseg-matnr,
        stprs like mbew-stprs,
        bs_menge like mseg-menge,
        gr_menge like mseg-menge,
        gip_menge like mseg-menge,
        gis_menge like mseg-menge,
        gim_menge like mseg-menge,
        cct_menge like mseg-menge,
        stk_menge like mseg-menge,
        cs_menge  like mseg-menge,
        bs_netpr like ekpo-netpr,
        gr_netpr like ekpo-netpr,
        gip_netpr like ekpo-netpr,
        gis_netpr like ekpo-netpr,
        gim_netpr like ekpo-netpr,
        cct_netpr like ekpo-netpr,
        stk_netpr like ekpo-netpr,
        cs_netpr  like ekpo-netpr,
     end of wa_result.
data: it_mara like table of wa_mara,
      it_marc like table of wa_marc,
      it_mard like table of wa_mard,
      it_mbew like table of wa_mbew,
      it_stk_info like table of wa_stk_info,
      it_matl like table of wa_matl,
      it_mseg like table of wa_mseg,
      it_grp_mat like table of wa_grp_mat,
      it_sum_mat like table of wa_sum_mat,
      it_result like table of wa_result.

ranges: r_gr_bwart for mseg-bwart,           "Goods Receipt
        r_gip_bwart for mseg-bwart,          "Goods Issue for production
        r_gis_bwart for mseg-bwart,          "Goods Issue for scrap
        r_mis_bwart for mseg-bwart,          "Miscellaneous
        r_cct_bwart for mseg-bwart,          "Cycle count
        r_stk_bwart for mseg-bwart.          "Stock transfer

data: w_cdate like sy-datum,
      w_3date like sy-datum,
      w_pdate like sy-datum,
      w_tdate like sy-datum,
      w_ybdat like sy-datum,
      w_yedat like sy-datum,
      w_cyear like mkpf-mjahr,
      w_selflg type c,
      w_yrdif  type i,
      w_idx type i,
      w_lines type i.
field-symbols <fs_tst> like line of it_mseg.
*&---------------Class for ALV Grid.
data: wa_layout type lvc_s_layo,
      wa_fieldcat like lvc_s_fcat,
      it_fieldcat  type slis_t_fieldcat_alv,
      it_fieldsort type slis_t_sortinfo_alv.
data: w_custom_container type ref to cl_gui_custom_container,
      w_alv_grid         type ref to cl_gui_alv_grid.

*&--------------------------------

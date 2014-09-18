*----------------------------------------------------------------------*
*   INCLUDE ZMMCONTP                                                   *
*----------------------------------------------------------------------*

data: wa_return like bapiret1,
      wa_header like bapiplaf_e1,
      wa_components like bapi_pldordcomp_e1.
data: it_components like table of wa_components.
data: begin of wa_cabn,
        atinn like cabn-atinn,
        adzhl like cabn-adzhl,
        atnam like cabn-atnam,
      end of wa_cabn.
data: begin of wa_ausp,
        objek like ausp-objek,
        atinn like ausp-atinn,
        atwrt like ausp-atwrt,
        atflv like ausp-atflv,
      end of wa_ausp.
data: begin of wa_equi_info,
        equnr like equi-equnr,
        plnum like plaf-plnum,
        fscod like mara-matnr,
        sdate like sy-datum,
        codes(5) type c,
        versn(3) type c,
        salod like vbak-vbeln,
        mark     type c,
      end of wa_equi_info.
data: begin of wa_marc,
        matnr like marc-matnr,
        werks like marc-werks,
      end of wa_marc.
data: begin of wa_fsc_info,
        plnum like plaf-plnum,
        matnr like mara-matnr,
        sdate like sy-datum,
        versn(3) type c,
        salod like vbak-vbeln,
      end of wa_fsc_info.

data: begin of wa_planord_info,
        plnum  like plaf-plnum,
        vbeln  like vbak-vbeln,
        pmatnr like mara-matnr,
        cmatnr like mara-matnr,
        plant  like marc-werks,
        reqqty like wa_components-req_quan,
        menge  like wa_components-entry_qty,
        meins  like mara-meins,
      end of wa_planord_info.
data: begin of wa_parent_mat,
       plnum like bapiplaf_e1-plannedorder_num,
       vbeln like vbak-vbeln,
       matnr like bapiplaf_e1-material,
       plant like bapiplaf_e1-prod_plant,
       menge like bapiplaf_e1-req_quan,
       meins like mara-meins,
      end of wa_parent_mat.
data: begin of wa_temp,
        matcom like wa_components,
        plnum  like bapiplaf_e1-plannedorder_num,
        pmatnr like mara-matnr,
        vbeln  like bapiplaf_e1-sales_ord,
      end of wa_temp.
data: begin of wa_plaf,
        plnum like plaf-plnum,
      end of wa_plaf.
data: begin of wa_mat_type,
        matnr like mara-matnr,
        mtart like mara-mtart,
        meins like mara-meins,
      end of wa_mat_type.
data: begin of wa_color_parts,
        matnr like mara-matnr,
      end of wa_color_parts.
data: begin of wa_halb,
        plnum like bapiplaf_e1-plannedorder_num,
        vbeln like bapiplaf_e1-sales_ord,
        matnr like mara-matnr,
        pmatnr like mara-matnr,
        werks like marc-werks,
        menge like plaf-bdmng,
        stlal like stko-stlal,
        datuv like stko-datuv,
      end of wa_halb.
data: begin of wa_bom_com,
        plnum like bapiplaf_e1-plannedorder_num,
        vbeln like bapiplaf_e1-sales_ord,
        matnr like mara-matnr,
        stlal like stko-stlal,
        datuv like stko-datuv,
        idnrk like stpox-idnrk,
        werks like marc-werks,
        menge like plaf-bdmng,
        mngko like plaf-bdmng,
        meins like mara-meins,
     end of wa_bom_com.

data: it_ausp like table of wa_ausp,
      it_ausp1 like table of wa_ausp,
      it_ausp2 like table of wa_ausp,
      it_color_parts like table of wa_color_parts,
      it_equi_info like table of wa_equi_info,
      it_planord_info like table of wa_planord_info,
      it_parent_mat like table of wa_parent_mat,
      it_temp like table of wa_temp,
      it_mat_type like hashed table of wa_mat_type
                       with unique key matnr mtart,
      it_halb like table of wa_halb,
      it_bom_com like table of wa_bom_com with header line,
*&---new.
      it_cabn like table of wa_cabn,
      it_plaf like table of wa_plaf,
      it_marc like table of wa_marc,
      it_fsc_info like table of wa_fsc_info,
      it_equi_plan_info like table of wa_equi_info with header line..

data: w_jobs     type i ,
      w_idx type i,
      w_flag type c,
      w_matnr_nc like mara-matnr,
      w_color(3) type c,
      w_c_matnr like mara-matnr.

data: w_key_date(10) type c.
*&--new
data: w_plnum like plaf-plnum,
      w_salord like vbak-vbeln,
      w_modyr type c,
      w_destn(5) type c,
      w_modidx(9) type c,
      w_ocn(4) type c,
      w_fsc like mara-matnr,
      w_pack type p,
      w_sdate type d,
      w_versn(3) type c,
      w_frm type i,
      w_to  type i,
      w_max type i,
      w_free type i,
      w_no_times type i,
      w_i type i,
      w_rem type i,
      V_dest(5).

data: w_len type i,
      w_odealer(2),
      w_ndealer(1).

ranges: r_scrap for wa_ausp-objek.
                                  .
field-symbols: <fs_ausp> like line of it_ausp,
               <fs_equi> like line of it_equi_info,
               <fs_comp> like line of it_temp ,
               <fs_planord> like line of it_planord_info.

************************************************************************
* Program Name      : ZRPP310R_NEW_MAT_NOTIFICATION
* Author            : KIM GIL-HYUN (Tonkey)
* Creation Date     : 2003.11.25.
* Specifications By : B. Choi
* Development Request No : UD1K902277
* Addl Documentation:
* Description       : New Material Notification.
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
report zrpp310r_new_mat_notification no standard page heading
                                     line-size 180 .
tables: mara,  "General Material Data
        makt,  "Material Descriptions
        marc,  "Plant Data for Material
        mard,  "Storage Location Data
        mbew,  "Material Valuation
        mvke,  "Sales Data for Material
        mkal,  "Production Versions of Material
        mlgt,  "Material Data for Each Storage Type
        mlan,  "Tax Classification for Material
        tstl,  "Taxes: Valid Tax Categories for Each Country
        vbap,  "Sales Document: Item Data
        plpo,  "Task list - operation/activity
        qmat,  "Inspection type - material parameters
        inob,  "Link between Internal Number and Object
        kssk,  "Allocation Table: Object to Class
        klah,  "Class Header Data
        ausp.  "Characteristic Values

data: begin of it_qm occurs 0,
        matnr     type marc-matnr,  "material No
        maktx     type makt-maktx,  "Material description
        meins     type mara-meins,  "Base Unit of Measure
        werks     type marc-werks,  "Plant
        mtart     type mara-mtart,  "Material Type
        ersda     type mara-ersda,  "Creation Date
        ernam     type mara-ernam,  "Name of Person who Created Object
*
        qmatv     type marc-qmatv,  "Inspection Setup Flag
        art1      type qmat-art,  "Inspection Type
        art2      type qmat-art,
        art3      type qmat-art,
        act1      type qmat-aktiv,  "Active Flag
        act2      type qmat-aktiv,
        act3      type qmat-aktiv,
        lgort     type mard-lgort,  "Storage location
        profl     type mara-profl,  "MIP/LP/KD(Source)
        dismm     type marc-dismm,  "MRP type
        beskz     type marc-beskz,  "Procurement Type
      end of it_qm.


data: begin of it_pm occurs 0,
        matnr     type mara-matnr,  "material No
        maktx     type makt-maktx,  "Material description
        meins     type mara-meins,  "Base Unit of Measure
        werks     type marc-werks,  "Plant
        mtart     type mara-mtart,  "Material Type
        ersda     type mara-ersda,  "Creation Date
        ernam     type mara-ernam,  "Name of Person who Created Object
*
        disls     type marc-disls,  "Lot size (materials planning)
        matkl     type mara-matkl,  "Material group
        ekgrp     type marc-ekgrp,  "Purchasing group
        bklas     type mbew-bklas,  "Valuation class
        vprsv     type mbew-vprsv,  "Price control indicator
        lgort     type mard-lgort,  "Storage location
        profl     type mara-profl,  "MIP/LP/KD(Source)
        dismm     type marc-dismm,  "MRP type
        beskz     type marc-beskz,  "Procurement Type
        disgr     type marc-disgr,  "MRP Group
        dispo     type marc-dispo,  "MRP controller
        zplp1     type mbew-zplp1,  "Future Planned Price 1
        zpld1     type mbew-zpld1,  "Date For Price 1
        stprs     type mbew-stprs,  "Standard price
*       Reference...
        bwkey     type mbew-bwkey,  "Valuation area
*
      end of it_pm.


data: begin of it_co occurs 0,
        matnr     type mard-matnr,  "material No
        maktx     type makt-maktx,  "Material description
        meins     type mara-meins,  "Base Unit of Measure
        werks     type marc-werks,  "Plant
        mtart     type mara-mtart,  "Material Type
        ersda     type mara-ersda,  "Creation Date
        ernam     type mara-ernam,  "Name of Person who Created Object
*
        bklas     type mbew-bklas,  "Valuation class
        vprsv     type mbew-vprsv,  "Price control indicator
        zplp1     type mbew-zplp1,  "Future Planned Price 1
        zpld1     type mbew-zpld1,  "Date For Price 1
        zplp3     type mbew-zplp3,
        zpld3     type mbew-zpld3,
        stprs     type mbew-stprs,  "Standard price
        lgort     type mard-lgort,  "Storage location
        prdha     type mara-prdha,  "Product hierarchy
        mvgr3     type vbap-mvgr3,  "Material group 3
        mvgr4     type vbap-mvgr4,  "Material group 4
        mvgr5     type vbap-mvgr5,  "Material group 5
*
      end of it_co.


data: begin of it_sd occurs 0,
        matnr     type mara-matnr,  "material No
        maktx     type makt-maktx,  "Material description
        meins     type mara-meins,  "Base Unit of Measure
        werks     type marc-werks,  "Plant
        mtart     type mara-mtart,  "Material Type
        ersda     type mara-ersda,  "Creation Date
        ernam     type mara-ernam,  "Name of Person who Created Object
*
        vkorg     type mvke-vkorg,  "Sales organization
        vtweg     type mvke-vtweg,  "Distribution channel
        spart     type mara-spart,  "Division
        versg     type mvke-versg,  "Material statistics group
        ktgrm     type mvke-ktgrm,  "Account assignment group
        tragr     type mara-tragr,  "Transportation group
        ladgr     type marc-ladgr,  "Loading group
        lgort     type mard-lgort,  "Storage location
        prdha     type mara-prdha,  "Product hierarchy
        mtpos_mara type mara-mtpos_mara,  "General item category group
        mtpos     type mvke-mtpos,  "Item category group
        kzkfg     type mara-kzkfg,  "Configurable Material Flag
        dwerk     type mvke-dwerk,  "Delivering plant
        tatyp1    type tskmt-tatyp,  "Tax category
        tatyp2    type tskmt-tatyp,
        tatyp3    type tskmt-tatyp,
        taxm1     type mlan-taxm1,  "Tax classification
        taxm2     type mlan-taxm2,
        taxm3     type mlan-taxm3,
*     reference
        verid     type mkal-verid,  "Production version
        aland     type mlan-aland,  "Departure country
      end of it_sd.


data: begin of it_pp occurs 0,
        matnr     type mara-matnr,  "material No
        maktx     type makt-maktx,  "Material description
        meins     type mara-meins,  "Base Unit of Measure
        werks     type marc-werks,  "Plant
        mtart     type mara-mtart,  "Material Type
        ersda     type mara-ersda,  "Creation Date
        ernam     type mara-ernam,  "Name of Person who Created Object
*
        disls     type marc-disls,  "Lot size (materials planning)
        rgekz     type marc-rgekz,  "Indicator: Backflush
        strgr     type marc-strgr,  "Planning strategy group
        verid     type mkal-verid,  "Production version
        stlan     type mkal-stlan,  "BOM usage
        stlal     type mkal-stlal,  "Alternative BOM
        plnng     type mkal-plnng,  "Key for task list group(Routing)
        aplal     type marc-aplal,  "Group counter
        pltyg     type mkal-pltyg,  "Task list type
        lgort     type mard-lgort,  "Storage location
        profl     type mara-profl,  "MIP/LP/KD(Source)
        dismm     type marc-dismm,  "MRP type
        beskz     type marc-beskz,  "Procurement Type
        disgr     type marc-disgr,  "MRP Group
        dispo     type marc-dispo,  "MRP controller
        lgpro     type marc-lgpro,  "Issue Storage Location
        sobsl     type marc-sobsl,  "Special procurement type
        fevor     type marc-fevor,  "Production scheduler
        sfepr     type marc-sfepr,  "Repetitive manufacturing profile
        class     type klah-class,  "Class number
*
      end of it_pp.


data: begin of it_mm occurs 0,
        matnr     type mard-matnr,  "material No
        maktx     type makt-maktx,  "Material description
        meins     type mara-meins,  "Base Unit of Measure
        werks     type mard-werks,  "Plant
        mtart     type mara-mtart,  "Material Type
        ersda     type mara-ersda,  "Creation Date
        ernam     type mara-ernam,  "Name of Person who Created Object
*
        disls     type marc-disls,  "Lot size (materials planning)
        rgekz     type marc-rgekz,  "Indicator: Backflush
        matkl     type mara-matkl,  "Material group
        ekgrp     type marc-ekgrp,  "Purchasing group
        kautb     type marc-kautb,  "Indicator: Automatic Po
        kordb     type marc-kordb,  "Indicator: Source list requirement
        fabkz     type marc-fabkz,  "JIT sched. indicator
        abcin     type marc-abcin,  "Physical inventory indicator
        lgort     type mard-lgort,  "Storage location
        profl     type mara-profl,  "MIP/LP/KD(Source)
        dismm     type marc-dismm,  "MRP type
        beskz     type marc-beskz,  "Procurement Type
        disgr     type marc-disgr,  "MRP Group
        dispo     type marc-dispo,  "MRP controller
        lgpro     type marc-lgpro,  "Issue Storage Location
        vspvb     type marc-vspvb,  "Proposed Supply Area
        bstmi     type marc-bstmi,  "Minimum lot size
        bstrf     type marc-bstrf,  "Rounding value
        tempb     type mara-tempb,  "Backflush Cycle
        raube     type mara-raube,  "Shop
*
      end of it_mm.


data: begin of it_wm occurs 0,
        matnr     type mara-matnr,  "material No
        maktx     type makt-maktx,  "Material description
        meins     type mara-meins,  "Base Unit of Measure
        werks     type marc-werks,  "Plant
        mtart     type mara-mtart,  "Material Type
        ersda     type mara-ersda,  "Creation Date
        ernam     type mara-ernam,  "Name of Person who Created Object
*
        lgnum     type mlgt-lgnum,  "Warehouse Number
        lgtyp     type mlgt-lgtyp,  "Storage Type
        lgpla     type mlgt-lgpla,  "Storage bin
        lpmax     type mlgt-lpmax,  "Maximum storage bin quantity
        lpmin     type mlgt-lpmin,  "Minimum storage bin quantity
        rdmng     type mlgt-rdmng,  "Rounding qty
        lgort     type mard-lgort,  "Storage location
        profl     type mara-profl,  "MIP/LP/KD(Source)
        dismm     type marc-dismm,  "MRP type
        beskz     type marc-beskz,  "Procurement Type
*
      end of it_wm.


data: begin of it_rs occurs 0,
        matnr     type marc-matnr,  "material No
        maktx     type makt-maktx,  "Material description
        meins     type mara-meins,  "Base Unit of Measure
        werks     type marc-werks,  "Plant
        mtart     type mara-mtart,  "Material Type
        ersda     type mara-ersda,  "Creation Date
        ernam     type mara-ernam,  "Name of Person who Created Object
*
        disls     type marc-disls,  "Lot size (materials planning)
        rgekz     type marc-rgekz,  "Indicator: Backflush
        matkl     type mara-matkl,  "Material group
        ekgrp     type marc-ekgrp,  "Purchasing group
        kautb     type marc-kautb,  "Indicator: Automatic Po
        kordb     type marc-kordb,  "Indicator: Source list requirement
        xchpf     type marc-xchpf,  "Batch Flag
        fxhor     type marc-fxhor,  "Planning time fence
        lgort     type mard-lgort,  "Storage location
        profl     type mara-profl,  "MIP/LP/KD(Source)
        dismm     type marc-dismm,  "MRP type
        beskz     type marc-beskz,  "Procurement Type
        disgr     type marc-disgr,  "MRP Group
        dispo     type marc-dispo,  "MRP controller
        lgpro     type marc-lgpro,  "Issue Storage Location
        vspvb     type marc-vspvb,  "Proposed Supply Area
        bwkey     type mbew-bwkey,  "Valuation area
        vprsv     type mbew-vprsv,  "Price control indicator
        zplp1     type mbew-zplp1,  "Future Planned Price 1
        zpld1     type mbew-zpld1,  "Date For Price 1
*
      end of it_rs.


data: begin of it_wo occurs 0,
        matnr     type marc-matnr,  "material No
        maktx     type makt-maktx,  "Material description
        meins     type mara-meins,  "Base Unit of Measure
        werks     type marc-werks,  "Plant
        mtart     type mara-mtart,  "Material Type
        ersda     type mara-ersda,  "Creation Date
        ernam     type mara-ernam,  "Name of Person who Created Object
*
        p_219_1(01) ,                                       "219-1
        p_219_101(01) ,                                     "219-101
        p_alc_u_1(04) ,                                     "ALC-001
        p_alc_u_101(04) ,                                   "ALC-101
        p_h_gen_date type d ,       "HPC gen date
        p_hpc_status(01) ,          "HPC status
        class     type klah-class , "CLASS
        p_wo_ser(09) ,              "WO ser
        p_init_qty(05) type n ,     "Init QTY
        p_vin_spec(11) ,            "VIN spec
        p_wo_hpc_b001(04) ,                                 "HPC-B-001
        p_wo_hpc_p001(04) ,                                 "HPC-P-001
        p_tech_spec_001(20) ,                               "Tech S 001
        p_perf_yn(01) ,             "Completeness of Work order
*
      end of it_wo.


data: begin of it_st occurs 0,
        matnr     type marc-matnr,  "material No
        maktx     type makt-maktx,  "Material description
        meins     type mara-meins,  "Base Unit of Measure
        werks     type marc-werks,  "Plant
        "Common & PM
        mtart     type mara-mtart,  "Material Type
        ersda     type mara-ersda,  "Creation Date
        ernam     type mara-ernam,  "Name of Person who Created Object
*   For PP.
        strgr     type marc-strgr,  "Planning strategy group
        verid     type mkal-verid,  "Production version
        stlan     type mkal-stlan,  "BOM usage
        stlal     type mkal-stlal,  "Alternative BOM
        plnng     type mkal-plnng,  "Key for task list group(Routing)
        aplal     type marc-aplal,  "Group counter
        pltyg     type mkal-pltyg,  "Task list type
        "PP & MM & PM & RS
        lgort     type mard-lgort,  "Storage location
        "PP & MM
        dispo     type marc-dispo,  "MRP controller
        sfepr     type marc-sfepr,  "Repetitive manufacturing profile
*   For MM.
        rgekz     type marc-rgekz,  "Indicator: Backflush
        "MM & PM & RS
        ekgrp     type marc-ekgrp,  "Purchasing group
        "MM & PM & RS
        beskz     type marc-beskz,  "Procurement Type
        lgpro     type marc-lgpro,  "Issue Storage Location
        vspvb     type marc-vspvb,  "Proposed Supply Area
*   For SD.
        vkorg     type mvke-vkorg,  "Sales organization
        vtweg     type mvke-vtweg,  "Distribution channel
        spart     type mara-spart,  "Division
        ktgrm     type mvke-ktgrm,  "Account assignment group
        mtpos_mara type mara-mtpos_mara,  "General item category group
        mtpos     type mvke-mtpos,  "Item category group
*   For CO/FI.
        bklas     type mbew-bklas,  "Valuation class
        "CO/FI & RS
        zplp1     type mbew-zplp1,  "Future Planned Price 1
        "CO/FI & RS
        zpld1     type mbew-zpld1,  "Date For Price 1
        zplp3     type mbew-zplp3,  "Future Planned Price 3
        zpld3     type mbew-zpld3,  "Date For Price 3
        prdha     type mara-prdha,  "Product hierarchy
        mvgr3     type vbap-mvgr3,  "Material group 3
        mvgr4     type vbap-mvgr4,  "Material group 4
        mvgr5     type vbap-mvgr5,  "Material group 5
*   For PM
        "PM & RS
        matkl     type mara-matkl,  "Material group
*   For QM.
        qmatv     type marc-qmatv,  "Inspection Setup Flag
        art1      type qmat-art,    "Inspection Type
        art2      type qmat-art,
        art3      type qmat-art,
        act1      type qmat-aktiv,  "Active Flag
        act2      type qmat-aktiv,
        act3      type qmat-aktiv,
*   For WM.
        lgnum     type mlgt-lgnum,  "Warehouse Number
        lgtyp     type mlgt-lgtyp,  "Storage Type
        lgpla     type mlgt-lgpla,  "Storage bin
        lpmax     type mlgt-lpmax,  "Maximum storage bin quantity
        lpmin     type mlgt-lpmin,  "Minimum storage bin quantity
*   For Raw & Sub Material
        bwkey     type mbew-bwkey,  "Valuation area
*   For Work Order
        p_perf_yn(01) ,             "Completeness of Work order
*
        pp_flg(01),  "PP
        mm_flg(01),  "MM
        sd_flg(01),  "SD
        co_flg(01),  "CO/FI
        pm_flg(01),  "PM
        qm_flg(01),  "QM
        wm_flg(01),  "WM
        rs_flg(01),  "Raw/Sub Material
        wo_flg(01),  "Work order
        st_flg(01),  "Status Flag
*
      end of it_st.

include <icon>.
field-symbols: <field_vals> .

selection-screen begin of block b1 with frame.
select-options: p_matnr for mard-matnr,
                p_werks for mard-werks.
selection-screen end of block b1.

select-options: p_mtart for mara-mtart,  "Material type
                p_ersda for mara-ersda,  "Creation Date
                p_ernam for mara-ernam.  "Name of Person who Created

*selection-screen begin of block b2 with frame title text-001.
parameters: optpp radiobutton group rad1,
            optmm radiobutton group rad1,
            optsd radiobutton group rad1,
            optco radiobutton group rad1,
            optpm radiobutton group rad1,
            optqm radiobutton group rad1,
            optwm radiobutton group rad1,
            optrs radiobutton group rad1,
            optwo radiobutton group rad1.
*            optst radiobutton group rad1.
*selection-screen end of block b2.


***********************************************
initialization.
***********************************************
  move: sy-datum to p_ersda-low,
        sy-datum to p_ersda-high.
  append p_ersda.

***********************************************
start-of-selection.
***********************************************
  if optpp = 'X'.
*  "PP(Production Planning and Control)
    perform make_data_for_pp .
  elseif optmm = 'X'.
*  "MM(Materials Management)
    perform make_data_for_mm .
  elseif optsd = 'X'.
*  "SD(Sales and Distribution)
    perform make_data_for_sd .
  elseif optco = 'X'.
*  "CO(Controlling), FI(Fi.Accounting)
    perform make_data_for_co .
  elseif optpm = 'X'.
*  "PM(Plant Maintenance)
    perform make_data_for_pm .
  elseif optqm = 'X'.
*  "QM(Quality Management)
    perform make_data_for_qm .
  elseif optwm = 'X'.
*  "WM(Warehouse Management)
    perform make_data_for_wm .
  elseif optrs = 'X'.
*  "Raw & Sub Material
    perform make_data_for_rs .
  elseif optwo = 'X'.
*  "Work Order
    perform make_data_for_wo .
*  elseif optst = 'X'.
**  "Summary Status
*    perform make_data_for_st .
*    perform set_flag_for_st .
  endif.

***********************************************
end-of-selection.
***********************************************
  if optpp = 'X'.
    sort it_pp by matnr werks mtart ersda ernam .
    perform write_body_for_pp .
  elseif optmm = 'X'.
*  "MM(Materials Management)
    sort it_mm by matnr werks mtart ersda ernam .
    perform write_body_for_mm .
  elseif optsd = 'X'.
*  "SD(Sales and Distribution)
    sort it_sd by matnr werks mtart ersda ernam .
    perform write_body_for_sd .
  elseif optco = 'X'.
*  "CO(Controlling), FI(Fi.Accounting)
    sort it_co by matnr werks mtart ersda ernam .
    perform write_body_for_co .
  elseif optpm = 'X'.
*  "PM(Plant Maintenance)
    sort it_pm by matnr werks mtart ersda ernam .
    perform write_body_for_pm .
  elseif optqm = 'X'.
*  "QM(Quality Management)
    sort it_qm by matnr werks mtart ersda ernam .
    perform write_body_for_qm .
  elseif optwm = 'X'.
*  "WM(Warehouse Management)
    sort it_wm by matnr werks mtart ersda ernam .
    perform write_body_for_wm .
  elseif optrs = 'X'.
*  "Raw & Sub Material
    sort it_rs by matnr werks mtart ersda ernam .
    perform write_body_for_rs .
  elseif optwo = 'X'.
*  "Work Order
    sort it_wo by matnr werks mtart ersda ernam .
    perform write_body_for_wo .
*  elseif optst = 'X'.
**  "Summary Status
*    sort it_st by matnr werks mtart ersda ernam .
*    perform write_body_for_st .
  endif.

***********************************************
at line-selection.
***********************************************
  data: l_field_name01(20).
  get cursor field l_field_name01.

  if sy-lsind < 2 and l_field_name01(12) = 'ICON_RED_LIG'.
    perform detail_data using it_st-matnr .
  endif.

***********************************************
top-of-page during line-selection  .
***********************************************
  perform top_of_page_during.


***********************************************
top-of-page.
***********************************************
  if optpp = 'X'.
*  "PP(Production Planning and Control)
    perform top_of_page_for_pp .
  elseif optmm = 'X'.
*  "MM(Materials Management)
    perform top_of_page_for_mm .
  elseif optsd = 'X'.
*  "SD(Sales and Distribution)
    perform top_of_page_for_sd .
  elseif optco = 'X'.
*  "CO(Controlling), FI(Fi.Accounting)
    perform top_of_page_for_co .
  elseif optpm = 'X'.
*  "PM(Plant Maintenance)
    perform top_of_page_for_pm .
  elseif optqm = 'X'.
*  "QM(Quality Management)
    perform top_of_page_for_qm .
  elseif optwm = 'X'.
*  "WM(Warehouse Management)
    perform top_of_page_for_wm .
  elseif optrs = 'X'.
*  "Raw & Sub Material
    perform top_of_page_for_rs .
  elseif optwo = 'X'.
*  "Work Order
    perform top_of_page_for_wo .
*  elseif optst = 'X'.
**  "Summary Status
*    perform top_of_page_for_st .
  endif.


*&---------------------------------------------------------------------*
*&      Form  make_data_for_pp
*&---------------------------------------------------------------------*
*       Getting Data of PP
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_data_for_pp.
  select *
    into corresponding fields of it_pp
    from ( ( mara as ma
      inner join marc as mc on ma~matnr = mc~matnr )
      inner join makt as mt on ma~matnr = mt~matnr )
    where ma~matnr in p_matnr and
          mc~werks in p_werks and
          ma~mtart in p_mtart and
          ma~ersda in p_ersda and
          ma~ernam in p_ernam and
          "Added Condition.
          ( ma~mtart = 'FERT' or
            ma~mtart = 'HALB' or
            ma~mtart = 'ROH'  or
            ma~mtart = 'ROH1' ) and
          mt~spras = sy-langu .
*   Search MKAL.
    clear: it_pp-verid, it_pp-plnng,
           it_pp-pltyg, it_pp-stlan,
           it_pp-stlal .
    select single verid plnng pltyg stlan stlal
      into (it_pp-verid, it_pp-plnng,
            it_pp-pltyg, it_pp-stlan,
            it_pp-stlal)
      from mkal
      where verid = ( select max( verid )
                       from mkal
                       where matnr = it_pp-matnr and
                             werks = it_pp-werks    ) .

*   Storage location
    clear it_pp-lgort .
    select single lgort
      into it_pp-lgort
      from mard
      where matnr = it_pp-matnr and
            werks = it_pp-werks .
*   CLASS
    clear it_pp-class .
    select single kl~class
      into it_pp-class
      from ( ( inob as ib
        inner join kssk as ks on ib~cuobj = ks~objek )
        inner join klah as kl on ks~clint = kl~clint )
      where ib~objek = it_pp-matnr and
            ib~obtab = 'MARA'      and
            ( ib~klart = '001' or
              ib~klart = '002' or
              ib~klart = '300' ) .
*
    append it_pp.
*
  endselect.

endform.                    " make_data_for_pp

*&---------------------------------------------------------------------*
*&      Form  make_data_for_mm
*&---------------------------------------------------------------------*
*       Getting Data of MM
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_data_for_mm.
  select *
    into corresponding fields of it_mm
    from ( ( mara as ma
      inner join marc as mc on ma~matnr = mc~matnr )
      inner join makt as mt on ma~matnr = mt~matnr )
    where ma~matnr in p_matnr and
          mc~werks in p_werks and
          ma~mtart in p_mtart and
          ma~ersda in p_ersda and
          ma~ernam in p_ernam and
          "Added Condition.
          ( ma~mtart = 'FERT' or
            ma~mtart = 'HALB' or
            ma~mtart = 'ROH'  or
            ma~mtart = 'ROH1' ) and
          mt~spras = sy-langu .
*   Storage location
    clear it_mm-lgort.
    select single lgort
      into it_mm-lgort
      from mard
      where matnr = it_mm-matnr and
            werks = it_mm-werks .
*
    append it_mm.
*
  endselect.
*
endform.                    " make_data_for_mm
*&---------------------------------------------------------------------*
*&      Form  make_data_for_sd
*&---------------------------------------------------------------------*
*       Getting Data of SD
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_data_for_sd.
  data: l_tatyp type tstl-tatyp.
  select *
    into corresponding fields of it_sd
    from ( ( mara as ma
      inner join marc as mc on ma~matnr = mc~matnr )
      inner join makt as mt on ma~matnr = mt~matnr )
    where ma~matnr in p_matnr and
          mc~werks in p_werks and
          ma~mtart in p_mtart and
          ma~ersda in p_ersda and
          ma~ernam in p_ernam and
          "Added Condition.
          ( ma~mtart = 'FERT' or
            ma~mtart = 'HALB' ) and
          mt~spras = sy-langu .
*   Search MKAL.
    clear it_sd-verid.
    select single verid
      into it_sd-verid
      from mkal
      where verid = ( select max( verid )
                       from mkal
                       where matnr = it_sd-matnr and
                             werks = it_sd-werks    ) .
*   Storage location
    clear it_sd-lgort.
    select single lgort
      into it_sd-lgort
      from mard
      where matnr = it_sd-matnr and
            werks = it_sd-werks .
*   Sales Information and Tax Classification
    clear: it_sd-vkorg, it_sd-vtweg, it_sd-versg,
           it_sd-ktgrm, it_sd-mtpos, it_sd-dwerk,
           it_sd-taxm1, it_sd-taxm2, it_sd-taxm3,
           it_sd-aland.
    select single *
      into corresponding fields of it_sd
      from ( mvke as mv
        inner join mlan as ml on mv~matnr = ml~matnr )
      where ml~matnr = it_sd-matnr .
*   Tax category  <---  Check more...
    clear: it_sd-tatyp1, it_sd-tatyp2, it_sd-tatyp3 .
    select tatyp
      into l_tatyp
      from tstl
      where talnd = it_sd-aland  .
      if sy-dbcnt = 1.
        move l_tatyp to it_sd-tatyp1.
      elseif sy-dbcnt = 2.
        move l_tatyp to it_sd-tatyp2.
      elseif sy-dbcnt = 3.
        move l_tatyp to it_sd-tatyp3.
      endif.
    endselect.
*
    append it_sd.
*
  endselect.

endform.                    " make_data_for_sd
*&---------------------------------------------------------------------*
*&      Form  make_data_for_co
*&---------------------------------------------------------------------*
*       Getting Data of CO
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_data_for_co.
  select *
    into corresponding fields of it_co
    from ( ( ( mara as ma
      inner join marc as mc on ma~matnr = mc~matnr )
      inner join mbew as mb on ma~matnr = mb~matnr and
                               mc~werks = mb~bwkey )
      inner join makt as mt on ma~matnr = mt~matnr )
    where ma~matnr in p_matnr and
          mc~werks in p_werks and
          ma~mtart in p_mtart and
          ma~ersda in p_ersda and
          ma~ernam in p_ernam and
          "Added Condition.
          ( ma~mtart = 'FERT' or
            ma~mtart = 'HALB' or
            ma~mtart = 'ROH'  or
            ma~mtart = 'ROH1' or
            ma~mtart = 'ERSA' ) and
          mt~spras = sy-langu .
*   Storage location
    clear it_co-lgort.
    select single lgort
      into it_co-lgort
      from mard
      where matnr = it_co-matnr and
            werks = it_co-werks .
*   MATERIAL GROUP3, 4, 5.
    clear: it_co-mvgr3, it_co-mvgr4, it_co-mvgr5.
    select single mvgr3 mvgr4 mvgr5
      into (it_co-mvgr3, it_co-mvgr4, it_co-mvgr5)
      from mvke
      where matnr = it_co-matnr .
*
    append it_co.
*
  endselect.

endform.                    " make_data_for_co
*&---------------------------------------------------------------------*
*&      Form  make_data_for_pm
*&---------------------------------------------------------------------*
*       Getting Data of PM
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_data_for_pm.
  select *
    into corresponding fields of it_pm
    from ( ( ( mara as ma
      inner join marc as mc on ma~matnr = mc~matnr )
      inner join mbew as mb on ma~matnr = mb~matnr and
                               mc~werks = mb~bwkey )
      inner join makt as mt on ma~matnr = mt~matnr )
    where ma~matnr in p_matnr and
          mc~werks in p_werks and
          ma~mtart in p_mtart and
          ma~ersda in p_ersda and
          ma~ernam in p_ernam and
          "Added Condition.
          ma~mtart = 'ERSA' and
          mt~spras = sy-langu .
*   Storage location
    clear it_pm-lgort.
    select single lgort
      into it_pm-lgort
      from mard
      where matnr = it_pm-matnr and
            werks = it_pm-werks .
*
    append it_pm.
*
  endselect.
endform.                    " make_data_for_pm
*&---------------------------------------------------------------------*
*&      Form  make_data_for_qm
*&---------------------------------------------------------------------*
*       Getting Data of QM
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_data_for_qm.
  data: l_art type qmat-art,
        l_aktiv type qmat-aktiv.
  select *
    into corresponding fields of it_qm
    from ( ( mara as ma
      inner join marc as mc on ma~matnr = mc~matnr )
      inner join makt as mt on ma~matnr = mt~matnr )
    where ma~matnr in p_matnr and
          mc~werks in p_werks and
          ma~mtart in p_mtart and
          ma~ersda in p_ersda and
          ma~ernam in p_ernam and
          ( ma~mtart = 'ROH' or
            ma~mtart = 'ROH1' ) and
          mt~spras = sy-langu .
*   Storage location
    clear it_qm-lgort.
    select single lgort
      into it_qm-lgort
      from mard
      where matnr = it_qm-matnr and
            werks = it_qm-werks .
*   Inspection Type
    clear: l_art, l_aktiv,
           it_qm-art1, it_qm-art2, it_qm-art3,
           it_qm-act1, it_qm-act2, it_qm-act3.
    select art aktiv
      into (l_art, l_aktiv)
      from qmat
      where matnr = it_qm-matnr and
            werks = it_qm-werks .
      if sy-dbcnt = 1.
        move l_art to it_qm-art1.
        move l_aktiv to it_qm-act1.
      elseif sy-dbcnt = 2.
        move l_art to it_qm-art2.
        move l_aktiv to it_qm-act2.
      elseif sy-dbcnt = 3.
        move l_art to it_qm-art3.
        move l_aktiv to it_qm-act3.
      endif.
    endselect.
*
    append it_qm.
*
  endselect.
endform.                    " make_data_for_qm
*&---------------------------------------------------------------------*
*&      Form  make_data_for_wm
*&---------------------------------------------------------------------*
*       Getting Data of WM
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_data_for_wm.
  select *
    into corresponding fields of it_wm
    from ( ( mara as ma
      inner join marc as mc on ma~matnr = mc~matnr )
      inner join makt as mt on ma~matnr = mt~matnr )
    where ma~matnr in p_matnr and
          mc~werks in p_werks and
          ma~mtart in p_mtart and
          ma~ersda in p_ersda and
          ma~ernam in p_ernam and
          "Added Condition.
          ( ma~mtart = 'ROH' or
            ma~mtart = 'ROH1' ) and
          mt~spras = sy-langu .
*   Storage location
    clear it_wm-lgort .
    select single lgort
      into it_wm-lgort
      from mard
      where matnr = it_wm-matnr and
            werks = it_wm-werks .
*   Material Data for Each Storage Type
    clear: it_wm-lgnum, it_wm-lgtyp, it_wm-lgpla,
           it_wm-lpmax, it_wm-lpmin, it_wm-rdmng.
    select single *
      into corresponding fields of it_wm
      from mlgt
      where matnr = it_wm-matnr .
*
    append it_wm.
*
  endselect.
endform.                    " make_data_for_wm
*&---------------------------------------------------------------------*
*&      Form  make_data_for_rs
*&---------------------------------------------------------------------*
*       Getting Data of RS
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_data_for_rs.
  select *
    into corresponding fields of it_rs
    from ( ( mara as ma
      inner join marc as mc on ma~matnr = mc~matnr )
      inner join makt as mt on ma~matnr = mt~matnr )
    where ma~matnr in p_matnr and
          mc~werks in p_werks and
          ma~mtart in p_mtart and
          ma~ersda in p_ersda and
          ma~ernam in p_ernam and
          "Added Condition
          ma~mtart = 'ROH1' and
          mt~spras = sy-langu .
*   Search MBEW.
    clear: it_rs-bwkey, it_rs-vprsv,
           it_rs-zplp1, it_rs-zpld1.
    select single bwkey vprsv zplp1 zpld1
      into (it_rs-bwkey, it_rs-vprsv,
            it_rs-zplp1, it_rs-zpld1)
      from mbew
      where matnr = it_rs-matnr and
            bwkey = it_rs-werks .
*   Storage location
    clear it_rs-lgort .
    select single lgort
      into it_rs-lgort
      from mard
      where matnr = it_rs-matnr and
            werks = it_rs-werks .
*
    append it_rs.
*
  endselect.

endform.                    " make_data_for_rs
*&---------------------------------------------------------------------*
*&      Form  make_data_for_wo
*&---------------------------------------------------------------------*
*       Getting Data of W/O
*----------------------------------------------------------------------*
*  -->  p1        text

*  <--  p2        text
*----------------------------------------------------------------------*
form make_data_for_wo.
  data: begin of l_it_class occurs 0.
          include structure zspp_vin_value.
  data: end of l_it_class.
  data: l_date_n(08) type n.

  select *
    into corresponding fields of it_wo
    from ( mara as ma
      inner join makt as mt on ma~matnr = mt~matnr )
    where ma~matnr in p_matnr and
*          mc~werks IN p_werks AND
          ma~mtart in p_mtart and
          ma~ersda in p_ersda and
          ma~ernam in p_ernam and
          "Added Condition
          ( ma~mtart = 'WOCL' or
            ma~mtart = 'WOHD'   ) and
          mt~spras = sy-langu .
*   PLANT
    clear it_wo-werks.
    select single werks
      into it_wo-werks
      from marc
      where matnr = it_wo-matnr and
            werks in p_werks .
*   CLASS
    clear: it_wo-class.
    select single kl~class
      into it_wo-class
      from ( kssk as ks
        inner join klah as kl on ks~clint = kl~clint )
      where ks~objek = it_wo-matnr and
            ( ks~klart = '001' or   "W/O
              ks~klart = '002' or   "V/M
              ks~klart = '300' ) .  "Variants
*   Searching Classifications.
*        p_219_1(01) ,                                       "219-1
    clear it_wo-p_219_1.
    perform search_class using it_wo-matnr
                               'P_219_1'
                         changing it_wo-p_219_1 .
*        p_219_101(01) ,                                     "219-101
    clear it_wo-p_219_101.
    perform search_class using it_wo-matnr
                               'P_219_101'
                         changing it_wo-p_219_101 .
*        p_alc_u_1(04) ,                                     "ALC-001
    clear it_wo-p_alc_u_1.
    perform search_class using it_wo-matnr
                               'P_ALC_U_1'
                         changing it_wo-p_alc_u_1 .
*        p_alc_u_101(04) ,                                   "ALC-101
    clear it_wo-p_alc_u_101.
    perform search_class using it_wo-matnr
                               'P_ALC_U_101'
                         changing it_wo-p_alc_u_101 .
*        p_h_gen_date TYPE d ,       "HPC gen date
    clear: l_date_n, it_wo-p_h_gen_date.
    perform search_class_n using it_wo-matnr
                               'P_H_GEN_DATE'
                         changing l_date_n .
    move l_date_n to it_wo-p_h_gen_date.
*        p_hpc_status(01) ,          "HPC status
    clear it_wo-p_hpc_status.
    perform search_class using it_wo-matnr
                               'P_HPC_STATUS'
                         changing it_wo-p_hpc_status .
*        p_wo_ser(09) ,              "WO ser
    clear it_wo-p_wo_ser.
    perform search_class using it_wo-matnr
                               'P_WO_SER'
                         changing it_wo-p_wo_ser .
*        p_init_qty(05) TYPE n ,     "Init QTY
    clear it_wo-p_init_qty.
    perform search_class_n using it_wo-matnr
                               'P_INIT_QTY'
                         changing it_wo-p_init_qty .
*        p_vin_spec(11) ,            "VIN spec
    clear it_wo-p_vin_spec.
    perform search_class using it_wo-matnr
                               'P_VIN_SPEC'
                         changing it_wo-p_vin_spec .
*        p_wo_hpc_b001(04) ,                                 "HPC-B-001
    clear it_wo-p_wo_hpc_b001.
    perform search_class using it_wo-matnr
                               'P_WO_HPC_B001'
                         changing it_wo-p_wo_hpc_b001 .
*        p_wo_hpc_p001(04) ,                                 "HPC-P-001
    clear it_wo-p_wo_hpc_p001.
    perform search_class using it_wo-matnr
                               'P_WO_HPC_P001'
                         changing it_wo-p_wo_hpc_p001 .
*        p_tech_spec_001(20) ,                               "Tech S 001
    clear it_wo-p_tech_spec_001.
    perform search_class using it_wo-matnr
                               'P_TECH_SPEC_001'
                         changing it_wo-p_tech_spec_001 .
*        p_perf_yn(01) ,             "Completeness of Work order
    clear it_wo-p_perf_yn.
    perform search_class using it_wo-matnr
                               'P_PERF_YN'
                         changing it_wo-p_perf_yn .
*
    append it_wo.
*
  endselect.

endform.                    " make_data_for_wo
*&---------------------------------------------------------------------*
*&      Form  make_data_for_st
*&---------------------------------------------------------------------*
*       Getting Data of ST
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_data_for_st.
* mara "General Material Data
* makt "Material Descriptions
* marc "Plant Data for Material
* mard "Storage Location Data
* mkal "Production Versions of Material
* mvke "Sales Data for Material
* mbew "Material Valuation
* qmat "Inspection type - material parameters
* mlgt "Material Data for Each Storage Type
  data: l_art type qmat-art,
        l_aktiv type qmat-aktiv,
        l_atinn type ausp-atinn.
* Search MARA.
  select *
    into corresponding fields of it_st
    from ( mara as ma
      inner join makt as mt on ma~matnr = mt~matnr )
    where ma~matnr in p_matnr and
          ma~mtart in p_mtart and
          ma~ersda in p_ersda and
          ma~ernam in p_ernam and
          mt~spras = sy-langu .
*   Search MARC.
    clear: it_st-werks, it_st-strgr, it_st-aplal,
           it_st-dispo, it_st-sfepr, it_st-rgekz,
           it_st-ekgrp, it_st-beskz, it_st-lgpro,
           it_st-vspvb, it_st-qmatv .
    select single *
      into corresponding fields of it_st
      from marc
      where matnr = it_st-matnr and
            werks in p_werks .
*   Search MARD.
    clear it_st-lgort .
    select single lgort
      into it_st-lgort
      from mard
      where matnr = it_st-matnr and
            werks = it_st-werks and
            lgort <> space .
    clear: it_st-verid, it_st-plnng,
           it_st-pltyg, it_st-stlan,
           it_st-stlal .
    select single verid plnng pltyg stlan stlal
      into (it_st-verid, it_st-plnng,
            it_st-pltyg, it_st-stlan,
            it_st-stlal )
      from mkal
      where verid = ( select max( verid )
                       from mkal
                       where matnr = it_st-matnr and
                             werks = it_st-werks    ) .
*   Search MVKE.
    clear: it_st-vkorg, it_st-vtweg, it_st-ktgrm, it_st-mtpos.
    select single vkorg vtweg ktgrm mtpos
      into (it_st-vkorg, it_st-vtweg, it_st-ktgrm, it_st-mtpos)
      from mvke
      where matnr = it_st-matnr and
            vkorg <> space .
*   Search MBEW.
    select single *
      into corresponding fields of it_st
      from mbew
      where matnr = it_st-matnr and
            bwkey = it_st-werks  .
*   Search QMAT.
    clear: l_art, l_aktiv .
    select art aktiv
      into (l_art, l_aktiv)
      from qmat
      where matnr = it_st-matnr and
            werks = it_st-werks .
      if sy-dbcnt = 1.
        move l_art to it_st-art1.
        move l_aktiv to it_st-act1.
      elseif sy-dbcnt = 2.
        move l_art to it_st-art2.
        move l_aktiv to it_st-act2.
      elseif sy-dbcnt = 3.
        move l_art to it_st-art3.
        move l_aktiv to it_st-act3.
      endif.
    endselect.
*   Search MLGT.
    select single *
      into corresponding fields of it_st
      from mlgt
      where matnr = it_st-matnr .
*   Search AUSP.
    clear it_st-p_perf_yn.
    call function 'CONVERSION_EXIT_ATINN_INPUT'
         exporting
              input  = 'P_PERF_YN'
         importing
              output = l_atinn.
    select single atwrt
      into it_st-p_perf_yn
      from ausp
      where objek = it_st-matnr and
            atinn = l_atinn .
*
    append it_st.
*
  endselect.

endform.                    " make_data_for_st
*&---------------------------------------------------------------------*
*&      Form  top_of_page_for_pp
*&---------------------------------------------------------------------*
form top_of_page_for_pp.
  skip.
  set left scroll-boundary column 33.

  write:/ 'From : ', p_ersda-low, '~', p_ersda-high.
  write:/ 'Materials List', '(PP)'.
  format color col_positive intensified on.
  write:/(154) sy-uline.
  write:/ sy-vline no-gap,
          (30) 'Material' no-gap, '|' no-gap,
          (06) 'UoM' no-gap, ' ' no-gap,
          (06) 'Plant' no-gap, ' ' no-gap,
          (08) 'M.type' no-gap, ' ' no-gap,
          (12) 'Created Date' no-gap, ' ' no-gap,
          (10) 'Created by' no-gap, ' ' no-gap,
          (08) 'B/F flag' no-gap, ' ' no-gap,
          (09) 'Lot size' no-gap, ' ' no-gap,
          (10) 'Stratg grp' no-gap, ' ' no-gap,
          (08) 'Prod.Ver' no-gap, ' ' no-gap,
          (07) 'BOM usg' no-gap, ' ' no-gap,
          (06) 'Alt.No' no-gap, ' ' no-gap,
          (08) 'Routing' no-gap, ' ' no-gap,
          (06) 'gr.cnt' no-gap, ' ' no-gap,
          (04) 'Type' no-gap, sy-vline no-gap.
  write:/ sy-vline no-gap,
          (30) 'Description' no-gap, '|' no-gap,
          (06) 'St.loc' no-gap, ' ' no-gap,
          (06) 'Source' no-gap, ' ' no-gap,
          (08) 'MRP type' no-gap, ' ' no-gap,
          (08) 'Pro.type' no-gap, ' ' no-gap,
          (10) 'MRP group' no-gap, ' ' no-gap,
          (14) 'MRP controller' no-gap, ' ' no-gap,
          (13) 'issue st.loc.' no-gap, ' ' no-gap,
          (07) 'Sp.Pro.' no-gap, ' ' no-gap,
          (14) 'Prod.scheduler' no-gap, ' ' no-gap,
          (11) 'REM.profile' no-gap, ' ' no-gap,
          (14) 'classification' no-gap, sy-vline no-gap.
  write:/(154) sy-uline.

endform.                    " top_of_page_for_pp
*&---------------------------------------------------------------------*
*&      Form  top_of_page_for_mm
*&---------------------------------------------------------------------*
form top_of_page_for_mm.
  skip.
  set left scroll-boundary column 33.
  write:/ 'From : ', p_ersda-low, '~', p_ersda-high.
  write:/ 'Materials List', '(MM)'.
  format color col_total intensified on.
  write:/(155) sy-uline.
  write:/ '|' no-gap,
          (30) 'Material' no-gap, '|' no-gap,
          (06) 'UoM' no-gap, ' ' no-gap,
          (06) 'Plant' no-gap, ' ' no-gap,
          (08) 'M.type' no-gap, ' ' no-gap,
          (12) 'Created Date' no-gap, ' ' no-gap,
          (10) 'Created by' no-gap, ' ' no-gap,
          (08) 'Lot size' no-gap, ' ' no-gap,
          (09) 'B/F Flag' no-gap, ' ' no-gap,
          (09) 'M.group' no-gap, ' ' no-gap,
          (09) 'Pur.Group' no-gap, ' ' no-gap,
          (07) 'Auto Po' no-gap, ' ' no-gap,
          (06) 'source' no-gap, ' ' no-gap,
          (08) 'Jit Del.' no-gap, ' ' no-gap,
          (09) 'P.Inv.ind' no-gap,
          155 '|' no-gap.
  write:/ '|' no-gap,
          (30) 'Description' no-gap, '|' no-gap,
          (06) 'St.loc' no-gap, ' ' no-gap,
          (06) 'Source' no-gap, ' ' no-gap,
          (08) 'MRP type' no-gap, ' ' no-gap,
          (08) 'Pro.type' no-gap, ' ' no-gap,
          (10) 'MRP group' no-gap, ' ' no-gap,
          (14) 'MRP controller' no-gap, ' ' no-gap,
          (13) 'issue st.loc.' no-gap, ' ' no-gap,
          (11) 'Supply area' no-gap, ' ' no-gap,
          (12) 'Min lot size' no-gap, ' ' no-gap,
          (09) 'Rod.value' no-gap, ' ' no-gap,
          (10) 'back.cylce' no-gap, ' ' no-gap,
          (04) 'shop' no-gap, '|' no-gap.
  write:/(155) sy-uline.
endform.                    " top_of_page_for_mm
*&---------------------------------------------------------------------*
*&      Form  top_of_page_for_sd
*&---------------------------------------------------------------------*
form top_of_page_for_sd.
  skip.
  set left scroll-boundary column 33.
  write:/ 'From : ', p_ersda-low, '~', p_ersda-high.
  write:/ 'Materials List', '(SD)'.
  format color col_heading intensified on.
  write:/(164) sy-uline.
  write:/ '|' no-gap,
          (30) 'Material' no-gap, '|' no-gap,
          (06) 'UoM' no-gap, ' ' no-gap,
          (06) 'Plant' no-gap, ' ' no-gap,
          (08) 'M.type' no-gap, ' ' no-gap,
          (12) 'Created Date' no-gap, ' ' no-gap,
          (10) 'Created by' no-gap, ' ' no-gap,
          (10) 'Sales area' no-gap, ' ' no-gap,
          (12) 'Dist.channel' no-gap, ' ' no-gap,
          (08) 'Division' no-gap, ' ' no-gap,
          (10) 'Mat.status' no-gap, ' ' no-gap,
          (19) 'Acct assignment grp' no-gap, ' ' no-gap,
          (09) 'Trans.grp' no-gap, ' ' no-gap,
          (10) 'LoadingGrp' no-gap, '|' no-gap.
  write:/ '|' no-gap,
          (30) 'Description' no-gap, '|' no-gap,
          (06) 'St.loc' no-gap, ' ' no-gap,
          (14) 'Prod.hierarchy' no-gap, ' ' no-gap,
          (15) 'GenItemCatGroup' no-gap, ' ' no-gap,
          (19) 'Item category group' no-gap, ' ' no-gap,
          (12) 'mat.is.conf.' no-gap, ' ' no-gap,
          (13) 'Delivery plant' no-gap, ' ' no-gap,
          (04) 'Tax1' no-gap, '/' no-gap,
          (05) 'class' no-gap, ' ' no-gap,
          (04) 'Tax2' no-gap, '/' no-gap,
          (05) 'class' no-gap, ' ' no-gap,
          (04) 'Tax3' no-gap, '/' no-gap,
          (05) 'class' no-gap,
          164 '|' no-gap.
  write:/(164) sy-uline.

endform.                    " top_of_page_for_sd
*&---------------------------------------------------------------------*
*&      Form  top_of_page_for_co
*&---------------------------------------------------------------------*
form top_of_page_for_co.
  skip.
  set left scroll-boundary column 33.
  write:/ 'From : ', p_ersda-low, '~', p_ersda-high.
  write:/ 'Materials List', '(CO)'.
  format color col_positive intensified on.
  write:/(156) sy-uline.
  write:/ '|' no-gap,
          (30) 'Material' no-gap, '|' no-gap,
          (06) 'UoM' no-gap, ' ' no-gap,
          (06) 'Plant' no-gap, ' ' no-gap,
          (08) 'M.type' no-gap, ' ' no-gap,
          (12) 'Created Date' no-gap, ' ' no-gap,
          (10) 'Created by' no-gap, ' ' no-gap,
          (15) 'valuation class' no-gap, ' ' no-gap,
          (13) 'Price control' no-gap, ' ' no-gap,
          (20) 'planned price1(USD)' no-gap, '/' no-gap,
          (10) 'Date' no-gap, ' ' no-gap,
          (14) 'standard price' no-gap, '|' no-gap.
  write:/ '|' no-gap,
          (30) 'Description' no-gap, '|' no-gap,
          (06) 'St.loc' no-gap, ' ' no-gap,
          (14) 'Prod.hierarchy' no-gap, ' ' no-gap,
          (08) 'M.group3' no-gap, ' ' no-gap,
          (08) 'M.group4' no-gap, ' ' no-gap,
          (08) 'M.group5' no-gap, ' ' no-gap,
          (20) 'planned price3(USD)' no-gap, '/' no-gap,
          (10) 'Date' no-gap,
          156 '|' no-gap.
  write:/(156) sy-uline.

endform.                    " top_of_page_for_co
*&---------------------------------------------------------------------*
*&      Form  top_of_page_for_pm
*&---------------------------------------------------------------------*
form top_of_page_for_pm.
  skip.
  set left scroll-boundary column 33.
  write:/ 'From : ', p_ersda-low, '~', p_ersda-high.
  write:/ 'Materials List', '(PM)'.
  format color col_total intensified on.
  write:/(141) sy-uline.
  write:/ '|' no-gap,
          (30) 'Material' no-gap, '|' no-gap,
          (06) 'UoM' no-gap, ' ' no-gap,
          (06) 'Plant' no-gap, ' ' no-gap,
          (08) 'M.type' no-gap, ' ' no-gap,
          (12) 'Created Date' no-gap, ' ' no-gap,
          (10) 'Created by' no-gap, ' ' no-gap,
          (08) 'Lot size' no-gap, ' ' no-gap,
          (07) 'M.group' no-gap, ' ' no-gap,
          (14) 'Purchase group' no-gap, ' ' no-gap,
          (15) 'Valuation class' no-gap, ' ' no-gap,
          (13) 'price control' no-gap, '|' no-gap.
  write:/ '|' no-gap,
          (30) 'Description' no-gap, '|' no-gap,
          (06) 'St.loc' no-gap, ' ' no-gap,
          (06) 'Source' no-gap, ' ' no-gap,
          (08) 'MRP type' no-gap, ' ' no-gap,
          (08) 'Pro.type' no-gap, ' ' no-gap,
          (09) 'MRP group' no-gap, ' ' no-gap,
          (14) 'MRP controller' no-gap, ' ' no-gap,
          (20) 'planned price1(USD)' no-gap, '/' no-gap,
          (10) 'Date' no-gap, ' ' no-gap,
          (14) 'standard price' no-gap,
          141 '|' no-gap.
  write:/(141) sy-uline.
endform.                    " top_of_page_for_pm
*&---------------------------------------------------------------------*
*&      Form  top_of_page_for_qm
*&---------------------------------------------------------------------*
form top_of_page_for_qm.
  skip.
  set left scroll-boundary column 33.
  write:/ 'From : ', p_ersda-low, '~', p_ersda-high.
  write:/ 'Materials List', '(QM)'.
  format color col_heading intensified on.
  write:/(147) sy-uline.
  write:/ '|' no-gap,
          (30) 'Material' no-gap, '|' no-gap,
          (06) 'UoM' no-gap, ' ' no-gap,
          (06) 'Plant' no-gap, ' ' no-gap,
          (08) 'M.type' no-gap, ' ' no-gap,
          (12) 'Created Date' no-gap, ' ' no-gap,
          (10) 'Created by' no-gap, ' ' no-gap,
          (16) 'Inspection setup' no-gap, ' ' no-gap,
          (16) 'Inspection type1' no-gap, ' ' no-gap,
          (16) 'Inspection type2' no-gap, ' ' no-gap,
          (16) 'Inspection type3' no-gap, '|' no-gap.
  write:/ '|' no-gap,
          (30) 'Description' no-gap, '|' no-gap,
          (06) 'St.loc' no-gap, ' ' no-gap,
          (06) 'Source' no-gap, ' ' no-gap,
          (08) 'MRP type' no-gap, ' ' no-gap,
          (08) 'Pro.type' no-gap, ' ' no-gap,
          97(07) 'Active1' no-gap, ' ' no-gap,
          114(07) 'Active2' no-gap, ' ' no-gap,
          131(07) 'Active3' no-gap,
          147 '|' no-gap.
  write:/(147) sy-uline.
endform.                    " top_of_page_for_qm
*&---------------------------------------------------------------------*
*&      Form  top_of_page_for_wm
*&---------------------------------------------------------------------*
form top_of_page_for_wm.
  skip.
  set left scroll-boundary column 33.
  write:/ 'From : ', p_ersda-low, '~', p_ersda-high.
  write:/ 'Materials List', '(WM)'.
  format color col_positive intensified on.
  write:/(161) sy-uline.
  write:/ '|' no-gap,
          (30) 'Material' no-gap, '|' no-gap,
          (06) 'UoM' no-gap, ' ' no-gap,
          (06) 'Plant' no-gap, ' ' no-gap,
          (08) 'M.type' no-gap, ' ' no-gap,
          (12) 'Created Date' no-gap, ' ' no-gap,
          (10) 'Created by' no-gap, ' ' no-gap,
          (13) 'warehouse no.' no-gap, ' ' no-gap,
          (12) 'Storage type' no-gap, ' ' no-gap,
          (11) 'Storage bin' no-gap, ' ' no-gap,
          (16) 'Max bin quantity' no-gap, ' ' no-gap,
          (16) 'Min bin quantity' no-gap, ' ' no-gap,
          (08) 'Rounding' no-gap, '|' no-gap.
  write:/ '|' no-gap,
          (30) 'Description' no-gap, '|' no-gap,
          (06) 'St.loc' no-gap, ' ' no-gap,
          (06) 'Source' no-gap, ' ' no-gap,
          (08) 'MRP type' no-gap, ' ' no-gap,
          (08) 'Pro.type' no-gap,
          161 '|' no-gap.
  write:/(161) sy-uline.
endform.                    " top_of_page_for_wm
*&---------------------------------------------------------------------*
*&      Form  top_of_page_for_rs
*&---------------------------------------------------------------------*
form top_of_page_for_rs.
  skip.
  set left scroll-boundary column 33.
  write:/ 'From : ', p_ersda-low, '~', p_ersda-high.
  write:/ 'Materials List', '(Raw & Sub material)'.
  format color col_total intensified on.
  write:/(168) sy-uline.
  write:/ '|' no-gap,
          (30) 'Material' no-gap, '|' no-gap,
          (06) 'UoM' no-gap, ' ' no-gap,
          (06) 'Plant' no-gap, ' ' no-gap,
          (08) 'M.type' no-gap, ' ' no-gap,
          (12) 'Created Date' no-gap, ' ' no-gap,
          (10) 'Created by' no-gap, ' ' no-gap,
          (08) 'Lot size' no-gap, ' ' no-gap,
          (09) 'backflush' no-gap, ' ' no-gap,
          (07) 'M.group' no-gap, ' ' no-gap,
          (09) 'Pur.Group' no-gap, ' ' no-gap,
          (07) 'Auto Po' no-gap, ' ' no-gap,
          (11) 'source list' no-gap, ' ' no-gap,
          (05) 'Batch' no-gap, ' ' no-gap,
          (08) 'pln.time' no-gap,
          168 '|' no-gap.
  write:/ '|' no-gap,
          (30) 'Description' no-gap, '|' no-gap,
          (06) 'St.loc' no-gap, ' ' no-gap,
          (06) 'Source' no-gap, ' ' no-gap,
          (08) 'MRP type' no-gap, ' ' no-gap,
          (08) 'Pro.type' no-gap, ' ' no-gap,
          (09) 'MRP group' no-gap, ' ' no-gap,
          (14) 'MRP controller' no-gap, ' ' no-gap,
          (17) 'issue st.location' no-gap, ' ' no-gap,
          (11) 'Supply area' no-gap, ' ' no-gap,
          (08) 'Val.Area' no-gap, ' ' no-gap,
          (07) 'Pr.ctrl' no-gap, ' ' no-gap,
          (20) 'pld price1(USD)' no-gap, '/' no-gap,
          (10) 'Date' no-gap, '|' no-gap.
  write:/(168) sy-uline.
endform.                    " top_of_page_for_rs
*&---------------------------------------------------------------------*
*&      Form  top_of_page_for_wo
*&---------------------------------------------------------------------*
form top_of_page_for_wo.
  skip.
  set left scroll-boundary column 33.
  write:/ 'From : ', p_ersda-low, '~', p_ersda-high.
  write:/ 'Materials List', '(W/Order)'.
  format color col_heading intensified on.
  write:/(126) sy-uline.
  write:/ '|' no-gap,
          (30) 'Material' no-gap, '|' no-gap,
          (06) 'UoM' no-gap, ' ' no-gap,
*          (06) 'Plant' no-gap, ' ' no-gap,
          (08) 'M.type' no-gap, ' ' no-gap,
          (12) 'Created Date' no-gap, ' ' no-gap,
          (10) 'Created by' no-gap, ' ' no-gap,
          (05) '219-1' no-gap, ' ' no-gap,
          (07) '219-101' no-gap, ' ' no-gap,
          (07) 'ALC-001' no-gap, ' ' no-gap,
          (07) 'ALC-101' no-gap, ' ' no-gap,
          (12) 'HPC-gen-date' no-gap, ' ' no-gap,
          (10) 'HPC-status' no-gap, '|' no-gap.
  write:/ '|' no-gap,
          (30) 'Description' no-gap, '|' no-gap,
          (10) 'Class' no-gap, ' ' no-gap,
          (08) 'WO-ser' no-gap, ' ' no-gap,
          (08) 'Init-qty' no-gap, ' ' no-gap,
          (10) 'VIN-spec' no-gap, ' ' no-gap,
          (10) 'HPC-B001' no-gap, ' ' no-gap,
          (10) 'HPC-P001' no-gap, ' ' no-gap,
          (10) 'Tech-S-001' no-gap, ' ' no-gap,
          (07) 'PERF_YN' no-gap,
          126 '|' no-gap.
  write:/(126) sy-uline.
endform.                    " top_of_page_for_wo
*&---------------------------------------------------------------------*
*&      Form  top_of_page_for_st
*&---------------------------------------------------------------------*
form top_of_page_for_st.
  skip.
  set left scroll-boundary column 42.
  write:/ 'From : ', p_ersda-low, '~', p_ersda-high.
  write:/ 'Materials List', '(Summary Status)'.
  format color col_group intensified on.
  write:/(142) sy-uline.
  write:/ '|' no-gap,
          (18) 'Material' no-gap, ' ' no-gap,
          (20) 'Description' no-gap, '|' no-gap,
          (04) 'UoM' no-gap, ' ' no-gap,
          (05) 'Plant' no-gap, ' ' no-gap,
          (06) 'M.type' no-gap, ' ' no-gap,
          (12) 'Created Date' no-gap, ' ' no-gap,
          (10) 'Created by' no-gap, ' ' no-gap,
          (04) 'PP' no-gap, ' ' no-gap,
          (04) 'MM' no-gap, ' ' no-gap,
          (04) 'SD' no-gap, ' ' no-gap,
          (05) 'CO/FI' no-gap, ' ' no-gap,
          (04) 'PM' no-gap, ' ' no-gap,
          (04) 'QM' no-gap, ' ' no-gap,
          (04) 'WM' no-gap, ' ' no-gap,
          (07) 'Raw/Sub' no-gap, ' ' no-gap,
          (07) 'W/order' no-gap, ' ' no-gap,
          (06) 'Status' no-gap, '|' no-gap.
  write:/(142) sy-uline.

endform.                    " top_of_page_for_st
*&---------------------------------------------------------------------*
*&      Form  write_body_for_pp
*&---------------------------------------------------------------------*
form write_body_for_pp.
  data: l_flag.
  loop at it_pp.
    if l_flag <> 'X'.
      format color col_positive intensified off.
      l_flag = 'X'.
    else.
      format color col_positive intensified on.
      l_flag = ' '.
    endif.

    write:/ sy-vline no-gap,
            (30) it_pp-matnr no-gap, '|' no-gap,
            (06) it_pp-meins no-gap, ' ' no-gap,
            (06) it_pp-werks no-gap, ' ' no-gap,
            (08) it_pp-mtart no-gap, ' ' no-gap,
            (12) it_pp-ersda no-gap, ' ' no-gap,
            (10) it_pp-ernam no-gap, ' ' no-gap,
            (08) it_pp-rgekz no-gap, ' ' no-gap,
            (09) it_pp-disls no-gap, ' ' no-gap,
            (10) it_pp-strgr no-gap, ' ' no-gap,
            (08) it_pp-verid no-gap, ' ' no-gap,
            (07) it_pp-stlan no-gap, ' ' no-gap,
            (06) it_pp-stlal no-gap, ' ' no-gap,
            (08) it_pp-plnng no-gap, ' ' no-gap,
            (06) it_pp-aplal no-gap, ' ' no-gap,
            (04) it_pp-pltyg no-gap, '|' no-gap.
    write:/ '|' no-gap,
            (30) it_pp-maktx no-gap, '|' no-gap,
            (06) it_pp-lgort no-gap, ' ' no-gap,
            (06) it_pp-profl no-gap, ' ' no-gap,
            (08) it_pp-dismm no-gap, ' ' no-gap,
            (08) it_pp-beskz no-gap, ' ' no-gap,
            (10) it_pp-disgr no-gap, ' ' no-gap,
            (14) it_pp-dispo no-gap, ' ' no-gap,
            (13) it_pp-lgpro no-gap, ' ' no-gap,
            (07) it_pp-sobsl no-gap, ' ' no-gap,
            (14) it_pp-fevor no-gap, ' ' no-gap,
            (11) it_pp-sfepr no-gap, ' ' no-gap,
            (14) it_pp-class no-gap, sy-vline no-gap.
  endloop.
  write:/(154) sy-uline.
endform.                    " write_body_for_pp
*&---------------------------------------------------------------------*
*&      Form  write_body_for_mm
*&---------------------------------------------------------------------*
form write_body_for_mm.
  data: l_flag.
  loop at it_mm.
    if l_flag <> 'X'.
      format color col_total intensified off.
      l_flag = 'X'.
    else.
      format color col_total intensified on.
      l_flag = ' '.
    endif.
    write:/ '|' no-gap,
            (30) it_mm-matnr no-gap, '|' no-gap,
            (06) it_mm-meins no-gap, ' ' no-gap,
            (06) it_mm-werks no-gap, ' ' no-gap,
            (08) it_mm-mtart no-gap, ' ' no-gap,
            (12) it_mm-ersda no-gap, ' ' no-gap,
            (10) it_mm-ernam no-gap, ' ' no-gap,
            (08) it_mm-disls no-gap, ' ' no-gap,
            (09) it_mm-rgekz no-gap, ' ' no-gap,
            (09) it_mm-matkl no-gap, ' ' no-gap,
            (09) it_mm-ekgrp no-gap, ' ' no-gap,
            (07) it_mm-kautb no-gap, ' ' no-gap,
            (06) it_mm-profl no-gap, ' ' no-gap,
            (08) it_mm-fabkz no-gap, ' ' no-gap,
            (09) it_mm-abcin no-gap,
            155 '|' no-gap.
    write:/ '|' no-gap,
            (30) it_mm-maktx no-gap, '|' no-gap,
            (06) it_mm-lgort no-gap, ' ' no-gap,
            (06) it_mm-profl no-gap, ' ' no-gap,
            (08) it_mm-dismm no-gap, ' ' no-gap,
            (08) it_mm-beskz no-gap, ' ' no-gap,
            (10) it_mm-disgr no-gap, ' ' no-gap,
            (14) it_mm-dispo no-gap, ' ' no-gap,
            (13) it_mm-lgpro no-gap, ' ' no-gap,
            (11) it_mm-vspvb no-gap, ' ' no-gap,
            (12) it_mm-bstmi no-gap, ' ' no-gap,
            (09) it_mm-bstrf no-gap, ' ' no-gap,
            (10) it_mm-tempb no-gap, ' ' no-gap,
            (04) it_mm-raube no-gap, '|' no-gap.
  endloop.
  write:/(155) sy-uline.
endform.                    " write_body_for_mm
*&---------------------------------------------------------------------*
*&      Form  write_body_for_sd
*&---------------------------------------------------------------------*
form write_body_for_sd.
  data: l_flag.
  loop at it_sd.
    if l_flag <> 'X'.
      format color col_heading intensified off.
      l_flag = 'X'.
    else.
      format color col_heading intensified on.
      l_flag = ' '.
    endif.
    write:/ '|' no-gap,
            (30) it_sd-matnr no-gap, '|' no-gap,
            (06) it_sd-meins no-gap, ' ' no-gap,
            (06) it_sd-werks no-gap, ' ' no-gap,
            (08) it_sd-mtart no-gap, ' ' no-gap,
            (12) it_sd-ersda no-gap, ' ' no-gap,
            (10) it_sd-ernam no-gap, ' ' no-gap,
            (10) it_sd-vkorg no-gap, ' ' no-gap,
            (12) it_sd-vtweg no-gap, ' ' no-gap,
            (08) it_sd-spart no-gap, ' ' no-gap,
            (10) it_sd-versg no-gap, ' ' no-gap,
            (19) it_sd-ktgrm no-gap, ' ' no-gap,
            (09) it_sd-tragr no-gap, ' ' no-gap,
            (10) it_sd-ladgr no-gap, '|' no-gap.
    write:/ '|' no-gap,
            (30) it_sd-maktx no-gap, '|' no-gap,
            (06) it_sd-lgort no-gap, ' ' no-gap,
            (14) it_sd-prdha no-gap, ' ' no-gap,
            (15) it_sd-mtpos_mara no-gap, ' ' no-gap,
            (19) it_sd-mtpos no-gap, ' ' no-gap,
            (12) it_sd-kzkfg no-gap, ' ' no-gap,
            (13) it_sd-dwerk no-gap, ' ' no-gap,
            (04) it_sd-tatyp1 no-gap, '/' no-gap,
            (05) it_sd-taxm1 no-gap, ' ' no-gap,
            (04) it_sd-tatyp2 no-gap, '/' no-gap,
            (05) it_sd-taxm2 no-gap, ' ' no-gap,
            (04) it_sd-tatyp3 no-gap, '/' no-gap,
            (05) it_sd-taxm3 no-gap,
            164 '|' no-gap,
*           reference
            it_sd-verid.

  endloop.
  write:/(164) sy-uline.
endform.                    " write_body_for_sd
*&---------------------------------------------------------------------*
*&      Form  write_body_for_co
*&---------------------------------------------------------------------*
form write_body_for_co.
  data l_flag.
  loop at it_co.
    if l_flag <> 'X'.
      format color col_positive intensified off.
      l_flag = 'X'.
    else.
      format color col_positive intensified on.
      l_flag = ' '.
    endif.
    write:/ '|' no-gap,
            (30) it_co-matnr no-gap, '|' no-gap,
            (06) it_co-meins no-gap, ' ' no-gap,
            (06) it_co-werks no-gap, ' ' no-gap,
            (08) it_co-mtart no-gap, ' ' no-gap,
            (12) it_co-ersda no-gap, ' ' no-gap,
            (10) it_co-ernam no-gap, ' ' no-gap,
            (15) it_co-bklas no-gap, ' ' no-gap,
            (13) it_co-vprsv no-gap, ' ' no-gap,
            (20) it_co-zplp1 no-gap, '/' no-gap,
            (10) it_co-zpld1 no-gap, ' ' no-gap,
            (14) it_co-stprs no-gap, '|' no-gap.
    write:/ '|' no-gap,
            (30) it_co-maktx no-gap, '|' no-gap,
            (06) it_co-lgort no-gap, ' ' no-gap,
            (14) it_co-prdha no-gap, ' ' no-gap,
            (08) it_co-mvgr3 no-gap, ' ' no-gap,
            (08) it_co-mvgr4 no-gap, ' ' no-gap,
            (08) it_co-mvgr5 no-gap, ' ' no-gap,
            (20) it_co-zplp3 no-gap, '/' no-gap,
            (10) it_co-zpld1 no-gap,
            156 '|' no-gap.
  endloop.
  write:/(156) sy-uline.
endform.                    " write_body_for_co
*&---------------------------------------------------------------------*
*&      Form  write_body_for_pm
*&---------------------------------------------------------------------*
form write_body_for_pm.
  data: l_flag.
  loop at it_pm.
    if l_flag <> 'X'.
      format color col_total intensified off.
      l_flag = 'X'.
    else.
      format color col_total intensified on.
      l_flag = ' '.
    endif.
    write:/ '|' no-gap,
            (30) it_pm-matnr no-gap, '|' no-gap,
            (06) it_pm-meins no-gap, ' ' no-gap,
            (06) it_pm-werks no-gap, ' ' no-gap,
            (08) it_pm-mtart no-gap, ' ' no-gap,
            (12) it_pm-ersda no-gap, ' ' no-gap,
            (10) it_pm-ernam no-gap, ' ' no-gap,
            (08) it_pm-disls no-gap, ' ' no-gap,
            (07) it_pm-matkl no-gap, ' ' no-gap,
            (14) it_pm-ekgrp no-gap, ' ' no-gap,
            (15) it_pm-bklas no-gap, ' ' no-gap,
            (13) it_pm-vprsv no-gap, '|' no-gap.
    write:/ '|' no-gap,
            (30) it_pm-maktx no-gap, '|' no-gap,
            (06) it_pm-lgort no-gap, ' ' no-gap,
            (06) it_pm-profl no-gap, ' ' no-gap,
            (08) it_pm-dismm no-gap, ' ' no-gap,
            (08) it_pm-beskz no-gap, ' ' no-gap,
            (09) it_pm-disgr no-gap, ' ' no-gap,
            (14) it_pm-dispo no-gap, ' ' no-gap,
            (20) it_pm-zplp1 no-gap, '/' no-gap,
            (10) it_pm-zpld1 no-gap, ' ' no-gap,
            (14) it_pm-stprs no-gap,
            141 '|' no-gap.
*   reference
    write: it_pm-bwkey .

  endloop.
  write:/(141) sy-uline.
endform.                    " write_body_for_pm
*&---------------------------------------------------------------------*
*&      Form  write_body_for_qm
*&---------------------------------------------------------------------*
form write_body_for_qm.
  data: l_flag.
  loop at it_qm.
    if l_flag <> 'X'.
      format color col_heading intensified off.
      l_flag = 'X'.
    else.
      format color col_heading intensified on.
      l_flag = ' '.
    endif.
    write:/ '|' no-gap,
            (30) it_qm-matnr no-gap, '|' no-gap,
            (06) it_qm-meins no-gap, ' ' no-gap,
            (06) it_qm-werks no-gap, ' ' no-gap,
            (08) it_qm-mtart no-gap, ' ' no-gap,
            (12) it_qm-ersda no-gap, ' ' no-gap,
            (10) it_qm-ernam no-gap, ' ' no-gap,
            (16) it_qm-qmatv no-gap, ' ' no-gap,
            (16) it_qm-art1  no-gap, ' ' no-gap,
            (16) it_qm-art2  no-gap, ' ' no-gap,
            (16) it_qm-art3  no-gap, '|' no-gap.
    write:/ '|' no-gap,
            (30) it_qm-maktx no-gap, '|' no-gap,
            (06) it_qm-lgort no-gap, ' ' no-gap,
            (06) it_qm-profl no-gap, ' ' no-gap,
            (08) it_qm-dismm no-gap, ' ' no-gap,
            (08) it_qm-beskz no-gap, ' ' no-gap,
            97(07) it_qm-act1  no-gap, ' ' no-gap,
            114(07) it_qm-act2  no-gap, ' ' no-gap,
            131(07) it_qm-act3  no-gap,
            147 '|' no-gap.

  endloop.
  write:/(147) sy-uline.
endform.                    " write_body_for_qm
*&---------------------------------------------------------------------*
*&      Form  write_body_for_wm
*&---------------------------------------------------------------------*
form write_body_for_wm.
  data l_flag.
  loop at it_wm.
    if l_flag <> 'X'.
      format color col_positive intensified off.
      l_flag = 'X'.
    else.
      format color col_positive intensified on.
      l_flag = ' '.
    endif.
    write:/ '|' no-gap,
            (30) it_wm-matnr no-gap, '|' no-gap,
            (06) it_wm-meins no-gap, ' ' no-gap,
            (06) it_wm-werks no-gap, ' ' no-gap,
            (08) it_wm-mtart no-gap, ' ' no-gap,
            (12) it_wm-ersda no-gap, ' ' no-gap,
            (10) it_wm-ernam no-gap, ' ' no-gap,
            (13) it_wm-lgnum no-gap, ' ' no-gap,
            (12) it_wm-lgtyp no-gap, ' ' no-gap,
            (11) it_wm-lgpla no-gap, ' ' no-gap,
            (16) it_wm-lpmax no-gap, ' ' no-gap,
            (16) it_wm-lpmin no-gap, ' ' no-gap,
            (08) it_wm-rdmng no-gap, '|' no-gap.
    write:/ '|' no-gap,
            (30) it_wm-maktx no-gap, '|' no-gap,
            (06) it_wm-lgort no-gap, ' ' no-gap,
            (06) it_wm-profl no-gap, ' ' no-gap,
            (08) it_wm-dismm no-gap, ' ' no-gap,
            (08) it_wm-beskz no-gap,
            161 '|' no-gap.
  endloop.
  write:/(161) sy-uline.
endform.                    " write_body_for_wm
*&---------------------------------------------------------------------*
*&      Form  write_body_for_rs
*&---------------------------------------------------------------------*
form write_body_for_rs.
  data l_flag.
  loop at it_rs.
    if l_flag <> 'X'.
      format color col_total intensified off.
      l_flag = 'X'.
    else.
      format color col_total intensified on.
      l_flag = ' '.
    endif.
    write:/ '|' no-gap,
            (30) it_rs-matnr no-gap, '|' no-gap,
            (06) it_rs-meins no-gap, ' ' no-gap,
            (06) it_rs-werks no-gap, ' ' no-gap,
            (08) it_rs-mtart no-gap, ' ' no-gap,
            (12) it_rs-ersda no-gap, ' ' no-gap,
            (10) it_rs-ernam no-gap, ' ' no-gap,
            (08) it_rs-disls no-gap, ' ' no-gap,
            (09) it_rs-rgekz no-gap, ' ' no-gap,
            (07) it_rs-matkl no-gap, ' ' no-gap,
            (09) it_rs-ekgrp no-gap, ' ' no-gap,
            (07) it_rs-kautb no-gap, ' ' no-gap,
            (11) it_rs-kordb no-gap, ' ' no-gap,
            (05) it_rs-xchpf no-gap, ' ' no-gap,
            (08) it_rs-fxhor no-gap,
            168 '|' no-gap.
    write:/ '|' no-gap,
            (30) it_rs-maktx no-gap, '|' no-gap,
            (06) it_rs-lgort no-gap, ' ' no-gap,
            (06) it_rs-profl no-gap, ' ' no-gap,
            (08) it_rs-dismm no-gap, ' ' no-gap,
            (08) it_rs-beskz no-gap, ' ' no-gap,
            (09) it_rs-disgr no-gap, ' ' no-gap,
            (14) it_rs-dispo no-gap, ' ' no-gap,
            (17) it_rs-lgpro no-gap, ' ' no-gap,
            (11) it_rs-vspvb no-gap, ' ' no-gap,
            (08) it_rs-bwkey no-gap, ' ' no-gap,
            (07) it_rs-vprsv no-gap, ' ' no-gap,
            (20) it_rs-zplp1 no-gap, '/' no-gap,
            (10) it_rs-zpld1 no-gap, '|' no-gap.
  endloop.
  write:/(168) sy-uline.
endform.                    " write_body_for_rs
*&---------------------------------------------------------------------*
*&      Form  write_body_for_wo
*&---------------------------------------------------------------------*
form write_body_for_wo.
  data l_flag.
  loop at it_wo.
    if l_flag <> 'X'.
      format color col_heading intensified off.
      l_flag = 'X'.
    else.
      format color col_heading intensified on.
      l_flag = ' '.
    endif.
    write:/ '|' no-gap,
            (30) it_wo-matnr no-gap, '|' no-gap,
            (06) it_wo-meins no-gap, ' ' no-gap,
*            (06) it_wo-werks no-gap, ' ' no-gap,
            (08) it_wo-mtart no-gap, ' ' no-gap,
            (12) it_wo-ersda no-gap, ' ' no-gap,
            (10) it_wo-ernam no-gap, ' ' no-gap,
            (05) it_wo-p_219_1 no-gap, ' ' no-gap,
            (07) it_wo-p_219_101 no-gap, ' ' no-gap,
            (07) it_wo-p_alc_u_1 no-gap, ' ' no-gap,
            (07) it_wo-p_alc_u_101 no-gap, ' ' no-gap,
            (12) it_wo-p_h_gen_date no-gap, ' ' no-gap,
            (10) it_wo-p_hpc_status no-gap, '|' no-gap.
    write:/ '|' no-gap,
            (30) it_wo-maktx no-gap, '|' no-gap,
            (10) it_wo-class no-gap, ' ' no-gap,
            (08) it_wo-p_wo_ser no-gap, ' ' no-gap,
            (08) it_wo-p_init_qty no-gap, ' ' no-gap,
            (10) it_wo-p_vin_spec no-gap, ' ' no-gap,
            (10) it_wo-p_wo_hpc_b001 no-gap, ' ' no-gap,
            (10) it_wo-p_wo_hpc_p001 no-gap, ' ' no-gap,
            (10) it_wo-p_tech_spec_001 no-gap, ' ' no-gap,
            (07) it_wo-p_perf_yn no-gap,
            126 '|' no-gap.
  endloop.
  write:/(126) sy-uline.
endform.                    " write_body_for_wo
*&---------------------------------------------------------------------*
*&      Form  write_body_for_st
*&---------------------------------------------------------------------*
form write_body_for_st.
  data l_flag.
  loop at it_st.
    if l_flag <> 'X'.
      format color col_group intensified off.
      l_flag = 'X'.
    else.
      format color col_group intensified on.
      l_flag = ' '.
    endif.
    write:/ '|' no-gap,
            (18) it_st-matnr no-gap, ' ' no-gap,
            (20) it_st-maktx no-gap, '|' no-gap,
            (04) it_st-meins no-gap, ' ' no-gap,
            (05) it_st-werks no-gap, ' ' no-gap,
            (06) it_st-mtart no-gap, ' ' no-gap,
            (12) it_st-ersda no-gap, ' ' no-gap,
            (10) it_st-ernam no-gap, ' ' no-gap,
            (04) it_st-pp_flg no-gap, ' ' no-gap,
            (04) it_st-mm_flg no-gap, ' ' no-gap,
            (04) it_st-sd_flg no-gap, ' ' no-gap,
            (05) it_st-co_flg no-gap, ' ' no-gap,
            (04) it_st-pm_flg no-gap, ' ' no-gap,
            (04) it_st-qm_flg no-gap, ' ' no-gap,
            (04) it_st-wm_flg no-gap, ' ' no-gap,
            (07) it_st-rs_flg no-gap, ' ' no-gap,
            (07) it_st-wo_flg no-gap, ' ' no-gap.
    if it_st-pp_flg <> 'X' and
       it_st-mm_flg <> 'X' and
       it_st-sd_flg <> 'X' and
       it_st-co_flg <> 'X' and
       it_st-pm_flg <> 'X' and
       it_st-qm_flg <> 'X' and
       it_st-wm_flg <> 'X' and
       it_st-rs_flg <> 'X' and
       it_st-rs_flg <> 'X' .
      write: (06) icon_green_light as icon no-gap, '|' no-gap.
    else.
      write: (06) icon_red_light as icon no-gap, '|' no-gap.
    endif.
    hide: it_st-matnr.
  endloop.
  write:/(142) sy-uline.
endform.                    " write_body_for_st
*&---------------------------------------------------------------------*
*&      Form  top_of_page_during
*&---------------------------------------------------------------------*
form top_of_page_during.
  skip.
  write:/ 'From : ', p_ersda-low, '~', p_ersda-high.
  write:/ 'Summary Status', '(Detail)'.
  read table it_st with key matnr = it_st-matnr .
  write:/(30) 'Material No :', ' ',
              it_st-matnr, ' ',
        /(30) 'Material Description :', ' ',
              it_st-maktx, ' ',
        /(30) 'Base Unit of Measure :', ' ',
              it_st-meins  , ' ',
        /(30) 'Plant :', ' ',
              it_st-werks  , ' ',
        /(30) 'Material Type :', ' ',
              it_st-mtart  , ' ',
        /(30) 'Creation Date :', ' ',
              it_st-ersda  , ' ',
        /(30) 'Created By :', ' ',
              it_st-ernam  , ' '.
  skip.
endform.                    " top_of_page_during
*&---------------------------------------------------------------------*
*&      Form  detail_data
*&---------------------------------------------------------------------*
form detail_data using    p_matnr.
  read table it_st with key matnr = p_matnr .

  skip.
  write:/ 'For Production Planning' color col_positive,
        /(30) 'Planning strategy group :', ' ',
              it_st-strgr  , ' ',
        /(30) 'Production version :', ' ',
              it_st-verid  , ' ',
        /(30) 'BOM usage :', ' ',
              it_st-stlan  , ' ',
        /(30) 'Alternative BOM :', ' ',
              it_st-stlal  , ' ',
        /(30) 'Routing :', ' ',
              it_st-plnng  , ' ',
        /(30) 'Group counter :', ' ',
              it_st-aplal  , ' ',
        /(30) 'Task list type :', ' ',
              it_st-pltyg  , ' ',
        /(30) 'Storage location :', ' ',
              it_st-lgort  , ' ',
        /(30) 'MRP controller :', ' ',
              it_st-dispo  , ' ',
        /(30) 'Re. manu. profile :', ' ',
              it_st-sfepr  , ' '.

  skip.
  write:/ 'For Materials Management' color col_positive,
        /(30) 'Storage location :', ' ',
              it_st-lgort  , ' ',
        /(30) 'MRP controller :', ' ',
              it_st-dispo  , ' ',
        /(30) 'Backflush Flag :', ' ',
              it_st-rgekz  , ' ',
        /(30) 'Issue Storage Location :', ' ',
              it_st-lgpro  , ' ',
        /(30) 'Proposed Supply Area :', ' ',
              it_st-vspvb  , ' ',
        /(30) 'Purchasing group :', ' ',
              it_st-ekgrp  , ' ',
        /(30) 'Procurement Type :', ' ',
              it_st-beskz  , ' '.

  skip.
  write:/ 'For Sales and Distribution' color col_positive,
        /(30) 'Sales organization :', ' ',
              it_st-vkorg  , ' ',
        /(30) 'Distribution channel :', ' ',
              it_st-vtweg  , ' ',
        /(30) 'Division :', ' ',
              it_st-spart  , ' ',
        /(30) 'Account assignment group :', ' ',
              it_st-ktgrm  , ' ',
        /(30) 'General item category group :', ' ',
              it_st-mtpos_mara  , ' ',
        /(30) 'Item category group :', ' ',
              it_st-mtpos  , ' '.

  skip.
  write:/ 'For CO and FI' color col_positive,
        /(30) 'Valuation class :', ' ',
              it_st-bklas  , ' ',
        /(30) 'Future Planned Price 1 :', ' ',
              it_st-zplp1  , ' ',
        /(30) 'Date For Price 1 :', ' ',
              it_st-zpld1  , ' ',
        /(30) 'Future Planned Price 3 :', ' ',
              it_st-zplp1  , ' ',
        /(30) 'Date For Price 3 :', ' ',
              it_st-zpld3  , ' ',
        /(30) 'Product hierarchy :', ' ',
              it_st-prdha  , ' ',
        /(30) 'Material group 3 :', ' ',
              it_st-mvgr3  , ' ',
        /(30) 'Material group 4 :', ' ',
              it_st-mvgr4  , ' ',
        /(30) 'Material group 5 :', ' ',
              it_st-mvgr5  , ' '.

  skip.
  write:/ 'For Plant Maintenace' color col_positive,
        /(30) 'Material Type :', ' ',
              it_st-mtart  , ' ',
        /(30) 'Storage location :', ' ',
              it_st-lgort  , ' ',
        /(30) 'Purchasing group :', ' ',
              it_st-ekgrp  , ' ',
        /(30) 'Procurement Type :', ' ',
              it_st-beskz  , ' ',
        /(30) 'Material group :', ' ',
              it_st-matkl  , ' '.

  skip.
  write:/ 'For Quality Management' color col_positive,
        /(30) 'Inspection Setup Flag :', ' ',
              it_st-qmatv  , ' ',
        /(30) 'Inspection Type1 :', ' ',
              it_st-art1  , ' ',
        /(30) 'Inspection Type2 :', ' ',
              it_st-art2  , ' ',
        /(30) 'Inspection Type3 :', ' ',
              it_st-art3  , ' ',
        /(30) 'Active Flag1 :', ' ',
              it_st-act1  , ' ',
        /(30) 'Active Flag2 :', ' ',
              it_st-act2  , ' ',
        /(30) 'Active Flag3 :', ' ',
              it_st-act3  , ' '.

  skip.
  write:/ 'For Warehouse Management' color col_positive,
        /(30) 'Warehouse Number :', ' ',
              it_st-lgnum  , ' ',
        /(30) 'Storage Type :', ' ',
              it_st-lgtyp  , ' ',
        /(30) 'Storage bin :', ' ',
              it_st-lgpla  , ' ',
        /(30) 'Maximum storage bin quantity :', ' ',
              it_st-lpmax  , ' ',
        /(30) 'Minimum storage bin quantity :', ' ',
              it_st-lpmin  , ' '.

  skip.
  write:/ 'For Raw & Sub Material' color col_positive,
        /(30) 'Storage location :', ' ',
              it_st-lgort  , ' ',
        /(30) 'Purchasing group :', ' ',
              it_st-ekgrp  , ' ',
        /(30) 'Procurement Type :', ' ',
              it_st-beskz  , ' ',
        /(30) 'Future Planned Price 1 :', ' ',
              it_st-zplp1  , ' ',
        /(30) 'Date For Price 1 :', ' ',
              it_st-zpld1  , ' ',
        /(30) 'Material group :', ' ',
              it_st-matkl  , ' ',
        /(30) 'Valuation area :', ' ',
              it_st-bwkey  , ' '.

  skip.
  write:/ 'For Work Order' color col_positive,
        /(30) 'Completeness of Work order :', ' ',
              it_st-p_perf_yn  , ' '.

endform.                    " detail_data
*&---------------------------------------------------------------------*
*&      Form  set_flag_for_st
*&---------------------------------------------------------------------*
form set_flag_for_st.
  sort it_st by werks matnr .
  loop at it_st.
    if it_st-mtart = space .
      it_st-pm_flg = 'X'.
    endif.
    if it_st-strgr = space or
       it_st-verid = space or
       it_st-stlan = space or
       it_st-stlal = space or
       it_st-plnng = space or
       it_st-aplal = space or
       it_st-pltyg = space or
       it_st-sfepr = space  .
      it_st-pp_flg = 'X'.
    endif.
    if it_st-lgort = space .
      it_st-pp_flg = 'X'.
      it_st-mm_flg = 'X'.
      it_st-pm_flg = 'X'.
      it_st-rs_flg = 'X'.
    endif.
    if it_st-dispo = space.
      it_st-pp_flg = 'X'.
      it_st-mm_flg = 'X'.
    endif.
    if it_st-rgekz = space or
       it_st-lgpro = space or
       it_st-vspvb = space   .
      it_st-mm_flg = 'X'.
    endif.
    if it_st-ekgrp = space or
       it_st-beskz = space  .
      it_st-mm_flg = 'X'.
      it_st-pm_flg = 'X'.
      it_st-rs_flg = 'X'.
    endif.
    if it_st-vkorg = space or
       it_st-vtweg = space or
       it_st-spart = space or
       it_st-ktgrm = space or
       it_st-mtpos_mara = space or
       it_st-mtpos = space   .
      it_st-sd_flg = 'X'.
    endif.
    if it_st-zplp1 = space or
       it_st-zpld1 = space   .
      it_st-co_flg = 'X'.
      it_st-rs_flg = 'X'.
    endif.
    if it_st-bklas = space or
       it_st-zplp3 = space or
       it_st-zpld3 = space or
       it_st-prdha = space or
       it_st-mvgr3 = space or
       it_st-mvgr4 = space or
       it_st-mvgr5 = space  .
      it_st-co_flg = 'X'.
    endif.
    if it_st-matkl = space .
      it_st-pm_flg = 'X'.
      it_st-rs_flg = 'X'.
    endif.
    if it_st-qmatv = space or
       it_st-art1  = space or
       it_st-art2  = space or
       it_st-art3  = space or
       it_st-act1  = space or
       it_st-act2  = space or
       it_st-act3  = space  .
      it_st-qm_flg = 'X'.
    endif.
    if it_st-lgnum = space or
       it_st-lgtyp = space or
       it_st-lgpla = space or
       it_st-lpmax = space or
       it_st-lpmin = space   .
      it_st-wm_flg = 'X'.
    endif.
    if it_st-bwkey = space .
      it_st-rs_flg = 'X'.
    endif.
    if it_st-p_perf_yn = 'N'.
      it_st-wo_flg = 'X'.
    endif.
    if it_st-pp_flg <> 'X'.
      it_st-pp_flg = 'O'.
    endif.
    if it_st-mm_flg <> 'X'.
      it_st-mm_flg = 'O'.
    endif.
    if it_st-sd_flg <> 'X'.
      it_st-sd_flg = 'O'.
    endif.
    if it_st-co_flg <> 'X'.
      it_st-co_flg = 'O'.
    endif.
    if it_st-pm_flg <> 'X'.
      it_st-pm_flg = 'O'.
    endif.
    if it_st-qm_flg <> 'X'.
      it_st-qm_flg = 'O'.
    endif.
    if it_st-wm_flg <> 'X'.
      it_st-wm_flg = 'O'.
    endif.
    if it_st-rs_flg <> 'X'.
      it_st-rs_flg = 'O'.
    endif.
    if it_st-wo_flg <> 'X'.
      it_st-wo_flg = 'O'.
    endif.

*   FERT(Check Only PP, MM, SD and CO/FI)
    if it_st-mtart = 'FERT'.
      clear: it_st-pm_flg,
             it_st-qm_flg,
             it_st-wm_flg,
             it_st-rs_flg,
             it_st-wo_flg.
    endif.
*   HALB(Check Only PP, MM, SD and CO/FI)
    if it_st-mtart = 'HALB'.
      clear: it_st-pm_flg,
             it_st-qm_flg,
             it_st-wm_flg,
             it_st-rs_flg,
             it_st-wo_flg.
    endif.
*   ROH (Check Only PP, MM, SD, CO/FI, WM and QM)
    if it_st-mtart = 'ROH'.
      clear: it_st-pm_flg,
             it_st-rs_flg,
             it_st-wo_flg.
    endif.
*   ROH1(Check Only RS)
    if it_st-mtart = 'ROH1'.
      clear: it_st-pp_flg,
             it_st-mm_flg,
             it_st-sd_flg,
             it_st-co_flg,
             it_st-pm_flg,
             it_st-qm_flg,
             it_st-wm_flg,
             it_st-wo_flg.
    endif.
*   ERSA(Check Only PM)
    if it_st-mtart = 'ERSA'.
      clear: it_st-pp_flg,
             it_st-mm_flg,
             it_st-sd_flg,
             it_st-co_flg,
             it_st-qm_flg,
             it_st-wm_flg,
             it_st-rs_flg,
             it_st-wo_flg.
    endif.
*   WOHD,WOCL(Check Only WO)
    if it_st-mtart = 'WOHD' or
       it_st-mtart = 'WOCL'  .
      clear: it_st-pp_flg,
             it_st-mm_flg,
             it_st-sd_flg,
             it_st-co_flg,
             it_st-pm_flg,
             it_st-qm_flg,
             it_st-wm_flg,
             it_st-rs_flg.
    endif.
    modify it_st index sy-tabix.
  endloop.
endform.                    " set_flag_for_st
*&---------------------------------------------------------------------*
*&      Form  search_class
*&---------------------------------------------------------------------*
form search_class using    p_matnr
                           p_atnam
                  changing p_data .
  data: l_atinn type ausp-atinn .

  call function 'CONVERSION_EXIT_ATINN_INPUT'
       exporting
            input  = p_atnam
       importing
            output = l_atinn.
  select single atwrt
    into p_data
    from ausp
    where objek = p_matnr and
          atinn = l_atinn .

endform.                    " search_class
*&---------------------------------------------------------------------*
*&      Form  search_class_n
*&---------------------------------------------------------------------*
form search_class_n using    p_matnr
                             p_atnam
                  changing p_data .
  data: l_atinn type ausp-atinn .
  data: l_atflv type ausp-atflv .
  data: l_number(08) type n.
  call function 'CONVERSION_EXIT_ATINN_INPUT'
       exporting
            input  = p_atnam
       importing
            output = l_atinn.
  select single atflv
    into l_atflv
    from ausp
    where objek = p_matnr and
          atinn = l_atinn .
  move l_atflv to p_data.

endform.                    " search_class

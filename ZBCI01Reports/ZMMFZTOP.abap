*----------------------------------------------------------------------*
*   INCLUDE ZMMFZTOP                                                *
*----------------------------------------------------------------------*
tables: mkpf.

data: begin of wa_mseg,
        matnr like mara-matnr,
        lifnr like mseg-lifnr,
        mtart like mara-mtart,
        meins like mara-meins,
        profl like mara-profl,
        ntgew like mara-ntgew,
        gewei like mara-gewei,
        xblnr(10) type c,
        xabln like mkpf-xabln,
        bwart like mseg-bwart,
        shkzg like mseg-shkzg,
        werks like mseg-werks,
        kdauf like mseg-kdauf,
        menge like mseg-menge,
        erfme like mseg-erfme,
        ummat like mseg-ummat,
        ebeln like mseg-ebeln,
        ebelp like mseg-ebelp,
        maktx like makt-maktx,
* Begin of changes by Matthew Cupples on 08/20/2010
        sakto like mseg-sakto,
* End changes on 08/20/2010
** Furong on 02/03/12
*        MBLNR LIKE MSEG-MBLNR,
        ZEILE LIKE MSEG-ZEILE,
        POSNR LIKE LIPS-POSNR,
        LICHN LIKE LIPS-LICHN,
** End on 02/03/12
        mngko like stpox-mngko,           "Not part of select statement
        pmatnr like mara-matnr,           "Not part of select statement
        pprofl like mara-profl,           "Not part of select statement
      end of wa_mseg.


DATA: BEGIN OF wa_mseg1,
        matnr LIKE mara-matnr,
        lifnr LIKE mseg-lifnr,
        mtart LIKE mara-mtart,
        meins LIKE mara-meins,
        profl LIKE mara-profl,
        ntgew LIKE mara-ntgew,
        gewei LIKE mara-gewei,
        xblnr(10) TYPE c,
        xabln LIKE mkpf-xabln,
        bwart LIKE mseg-bwart,
        shkzg LIKE mseg-shkzg,
        werks LIKE mseg-werks,
        kdauf LIKE mseg-kdauf,
        menge LIKE mseg-menge,
        erfme LIKE mseg-erfme,
        ummat LIKE mseg-ummat,
        ebeln LIKE mseg-ebeln,
        ebelp LIKE mseg-ebelp,
        maktx LIKE makt-maktx,
        LGORT LIKE MSEG-LGORT,
* Begin of changes by Matthew Cupples on 08/20/2010
        sakto LIKE mseg-sakto,
* End changes on 08/20/2010
** Furong on 02/03/12
*        MBLNR LIKE MSEG-MBLNR,
        zeile LIKE mseg-zeile,
        posnr LIKE lips-posnr,
        lichn LIKE lips-lichn,
** End on 02/03/12
        mngko LIKE stpox-mngko,           "Not part of select statement
        pmatnr LIKE mara-matnr,           "Not part of select statement
        pprofl LIKE mara-profl,           "Not part of select statement
      END OF wa_mseg1.

data: begin of wa_ztmm_6026_01.
        include structure ztmm_6026_01.
data: end of wa_ztmm_6026_01.
data: begin of wa_991_mat,
        matnr like mseg-matnr,
      end of wa_991_mat.
data: begin of it_ausp_status occurs 0,
        objek like ausp-objek,
      end of it_ausp_status.
data: begin of it_ausp_status_1 occurs 0,
        objek like ausp-objek,
      end of it_ausp_status_1.

data : begin of wa_mat_info,
         matnr like mara-matnr,
         werks like marc-werks,
         profl like mara-profl,
         ntgew like mara-ntgew,
         gewei like mara-gewei,
         stawn like marc-stawn,
         stprs like mbew-stprs,
         peinh like mbew-peinh,
         maktx like makt-maktx,
       end of wa_mat_info.
data : begin of wa_mat_info1,
         matnr like mara-matnr,
         werks like marc-werks,
         profl like mara-profl,
         ntgew like mara-ntgew,
         gewei like mara-gewei,
         stawn like marc-stawn,
         maktx like makt-maktx,
       end of wa_mat_info1.

data: begin of wa_eina,
        matnr like eina-matnr,
        lifnr like eina-lifnr,
        werks like eine-werks,
        ekorg like eine-ekorg,
        waers like eine-waers,
        peinh like eine-peinh,
        bpumz like eine-bpumz,
        bpumn like eine-bpumn,
        effpr like eine-effpr,
        land1 like lfa1-land1,
        kbetr like konp-kbetr,
     end of wa_eina.

** Added by furong on 08/11/08
data: begin of wa_eina_po,
        matnr like eina-matnr,
        lifnr like eina-lifnr,
        werks like eine-werks,
        ekorg like eine-ekorg,
        waers like eine-waers,
        peinh like eine-peinh,
        bpumz like eine-bpumz,
        bpumn like eine-bpumn,
        effpr like eine-effpr,
        land1 like lfa1-land1,
        ebeln like mseg-ebeln,
        ebelp like mseg-ebelp,
        kbetr like konp-kbetr,
        UMREN like ekpo-UMREN,
        UMREz like ekpo-UMREz,
     end of wa_eina_po.
** End of addition

data: begin of wa_lfa1,
        lifnr like lfa1-lifnr,
        land1 like lfa1-land1,
       end of wa_lfa1.
data : begin of wa_ztbl,
         ebeln like ekko-ebeln,
         ebelp like ekpo-ebelp,
         zfhblno like ztbl-zfhblno,
         zfrptty like ztbl-zfrptty,
         zfvia like ztbl-zfvia,
         XBLNr like LIKP-VBELN,
       end of wa_ztbl.
data: begin of wa_po_bol,
        matnr like mseg-matnr,
        ebeln like ekko-ebeln,
        zfhblno like ztbl-zfhblno,
      end of wa_po_bol.
data: begin of wa_ftztcode,
        bwart like ztftz_tcode-bwart,
        zzcod like ztftz_tcode-zzcod,
        zzflg like ztftz_tcode-zzflg,
        shkzg like ztftz_tcode-shkzg,
** Changed by Matthew Cupples on 08/19/2010
        sakto like ztftz_tcode-sakto,
** End of change on 08/19/2010

      end of wa_ftztcode.
data: begin of wa_vbak,
         vbeln like vbak-vbeln,
      end of wa_vbak.
data: begin of wa_vbpa,
        vbeln like vbpa-vbeln,
        land1 like vbpa-land1,
      end of wa_vbpa.
data: begin of wa_stpox1.
        include structure stpox.
data: matnr like mara-matnr,
*      bwart like mseg-bwart,
      end of wa_stpox1.
data: begin of wa_fsc_stpox1.
        include structure stpox.
data:  pmatnr like mara-matnr,
       stlal1 like stko-stlal,
       datuv1  like stko-datuv,
       plnum like bapiplaf_e1-plannedorder_num,
       vbeln like bapiplaf_e1-sales_ord,
      end of wa_fsc_stpox1.

data: begin of wa_fsc_stpox2,
       ojtxb like stpox-ojtxb,
       stlal like stpox-stlal,
       loekz like stpox-loekz,
       ojtxp like stpox-ojtxp,
       mtart like stpox-mtart,
       werks like stpox-werks,
       mmein like stpox-mmein,
       mnglg like stpox-mnglg,
       mngko like stpox-mngko,
       msign like stpox-msign,
       sobsl like stpox-sobsl,
       kzaus like stpox-kzaus,
       ausdt like stpox-ausdt,
       nfmat like stpox-nfmat,
       stawn like stpox-stawn,
       matmk like stpox-matmk,
       datuv like stpox-datuv,
       idnrk like stpox-idnrk,
       sortf like stpox-sortf,
       meins like stpox-meins,
       menge like stpox-menge,
       datub like stpox-datub,
       dumps like stpox-dumps,
       sobmx like stpox-sobmx,
       matkl like stpox-matkl,
       pmatnr like mara-matnr,
       stlal1 like stko-stlal,
       datuv1  like stko-datuv,
       plnum like bapiplaf_e1-plannedorder_num,
       vbeln like bapiplaf_e1-sales_ord,
      end of wa_fsc_stpox2.

data: begin of wa_adjust,
        matnr like mara-matnr,
        txncode(4) type c,
        menge like mseg-menge,
      end of wa_adjust.
data: begin of wa_halb_exp,
        matnr like mara-matnr,
        werks like marc-werks,
        menge like plaf-bdmng,
        stlal like stko-stlal,
        datuv like stko-datuv,
      end of wa_halb_exp.

DATA: BEGIN OF wa_halb_eng,
         matnr LIKE mara-matnr,
         werks LIKE marc-werks,
         datuv LIKE sy-datum,
         bdmng LIKE plaf-bdmng,
         stlal like stko-stlal,
         pmatnr like mara-matnr,
         pdatuv LIKE sy-datum,
       END OF wa_halb_eng.

data: BEGIN OF wa_EKPO,
      EBELN like mseg-ebeln,
      EBELP like mseg-ebelp,
      MATNR like mseg-matnr,
      WERKS like mseg-werks,
      NETPR like ekpo-netpr,
      bpumz like EKPO-bpumz,
      bpumn like EKPO-bpumn,
      peinh LIKE ekpo-peinh,
             UMREN like ekpo-UMREN,
        UMREz like ekpo-UMREz,
      END OF wa_ekpo.

DATA: it_halb_eng LIKE TABLE OF wa_halb_eng with header line.

data: it_mseg like table of wa_mseg,
      it_991_mat like table of wa_991_mat,
      it_halb_exp like table of wa_halb_exp,
      it_ztmm_6026_01 like table of wa_ztmm_6026_01,
      it_ztmm_6026_01_tmp LIKE TABLE OF wa_ztmm_6026_01,
      it_mat_info like hashed table of wa_mat_info
                       with unique key matnr werks,
      it_mat_info1 like hashed table of wa_mat_info1
                       with unique key matnr werks,
      it_eina like table of wa_eina,
      it_eina_po like table of wa_eina_po,
      it_ekpo like table of wa_ekpo,
      it_lfa1 like table of wa_lfa1,
      it_ztbl like table of wa_ztbl,
      it_ftztcode like table of wa_ftztcode,
      it_vbak like table of wa_vbak,
      it_vbpa like table of wa_vbpa,
      it_stpox1 like table of wa_stpox1,
** changed by Furong
*      it_fsc_stpox1 like table of wa_fsc_stpox1,
      it_fsc_stpox1 like table of wa_fsc_stpox2,
** end of change
      it_adjust like table of wa_adjust,
      it_po_bol like table of wa_po_bol.
data: wa_ztca_if_log like ztca_if_log.
data: w_date like sy-datum,
      w_time like sy-uzeit,
      w_ftz_tcode(4) type c,
      w_matnr like mara-matnr,
      w_anpc_qty like mseg-menge,
      w_appc_qty like mseg-menge,
      w_adj_netqty like mseg-menge,
      w_adj_txcode(4) type c.
constants: c_erp_source type c value space,
           c_ftzlink_source type c value 'I'.

field-symbols: <fs_mseg> like line of it_mseg,
               <fs_ztmm_6026_01> like line of it_ztmm_6026_01.

ranges: r_bwart for mseg-bwart,
        r_mtart for mara-mtart.
**** Constants&Vars for Number range object ****************************
CONSTANTS: c_nro_nr_00   VALUE '00' LIKE inri-nrrangenr. "Header Part
CONSTANTS: c_nro_nr_01   VALUE '01' LIKE inri-nrrangenr. "Item Part
CONSTANTS: c_nro_nr_09   VALUE '09' LIKE inri-nrrangenr. "App. Doc no.
* Number range object
DATA:      w_nro_object  VALUE 'ZMMNRO0002'
                               LIKE inri-object. "NRO for ZBDCMSGCOLL
* Number_Get_Next
DATA:      w_nro_number  TYPE num10.      " Same type of nro_object

DATA:      w_zdocno      TYPE num10.       "App. Doc. No.

* For OK code
DATA: ok_code TYPE sy-ucomm,  save_ok_code LIKE ok_code.


* For PF-STATUS and Titlebar
CLASS lcl_ps DEFINITION DEFERRED.
DATA: crv_ps TYPE REF TO lcl_ps.


****/ Begin of Menu
* Itab & WA For FOR Dynamic Menu
DATA:  it_func TYPE STANDARD TABLE OF rsmpe-func.
DATA:  wa_func LIKE LINE OF it_func.
* Title
DATA: w_title(80).         " Title
****/ End of Menu

**** Custom Control
*Custom Control Name Declaration (FOR ALV GRID)
DATA: cc_name TYPE scrfname VALUE 'CC_0100'.

*Reference Variable Declaration
DATA: crv_custom_container TYPE REF TO cl_gui_custom_container,
      crv_alv_grid         TYPE REF TO cl_gui_alv_grid.

* Variables for ALV
DATA: wa_layout   TYPE lvc_s_layo.
DATA: it_fieldcat TYPE lvc_t_fcat WITH HEADER LINE.
DATA: wa_toolbar  TYPE stb_button.
DATA: wa_sort     TYPE lvc_s_sort.  "For sort
DATA: it_sort     LIKE TABLE OF wa_sort. "For sort
DATA: it_roid TYPE lvc_t_roid.         "For Selected Row ID
DATA: wa_roid LIKE LINE OF it_roid.    "For Selected Row ID
DATA: it_row TYPE lvc_t_row.           "For Selected Row ID:Before 620
DATA: wa_row LIKE LINE OF it_row.      "For Selected Row ID:Before 620

*/Miscellaneous
DATA: w_lines TYPE i.  "For the number of Itab Lines
*  DATA: w_date  TYPE d.  "Present Date
*  DATA: w_time  TYPE t.  "Present time


*.........................................................
DATA: w_save,                          "for Parameter I_SAVE
      wa_disvariant TYPE disvariant.   "for parameter IS_VARIANT

*&------------Added by Shiva.
data: w_atinn1 like cabn-atinn,     "Destination code
      w_atinn2 like cabn-atinn,     "Model Index
      w_atinn3 like cabn-atinn,     "Model year
      w_atinn4 like cabn-atinn,     "OCN
      w_atinn5 like cabn-atinn,     "Plan order
      w_atinn6 like cabn-atinn,     "actural date/time 25
      w_atinn7 like cabn-atinn,     "actural date/time 27
      w_atinn8 like cabn-atinn,     "RP status
      w_atinn9 like cabn-atinn,     "Sales order
      w_atinn10 like cabn-atinn,    "Seq. date
      w_atinn11 like cabn-atinn,    "Usage car
      w_atinn12 like cabn-atinn,    "Version
      w_adzhl1 like cabn-adzhl,     "Destination code counter
      w_adzhl2 like cabn-adzhl,     "Model Index counter
      w_adzhl3 like cabn-adzhl,     "Model year counter
      w_adzhl4 like cabn-adzhl,     "OCN counter
      w_adzhl5 like cabn-adzhl,     "Plan order counter
      w_adzhl6 like cabn-adzhl,     "Rep. point 25 counter
      w_adzhl7 like cabn-adzhl,     "Rep. point 27 counter
      w_adzhl8 like cabn-adzhl,     "RP status counter
      w_adzhl9 like cabn-adzhl,     "Sales order
      w_adzhl10 like cabn-adzhl,    "Seq. date counter
      w_adzhl11 like cabn-adzhl,    "Usage car counter
      w_adzhl12 like cabn-adzhl.    "Version counter


data: w_taskname(4) type n value '0001',
      w_excep_flag type c,
      w_snd_jobs type i value 1,
      w_rcv_jobs type i value 1.
data:  w_date_time(14) type c,
       w_date_time_shop(8) type n,
       W_DEST(3) .

data: it_ztpperm like table of ztpperm with header line.

ranges: r_dt for w_date_time,
        r_dt_shop for ausp-atflv.

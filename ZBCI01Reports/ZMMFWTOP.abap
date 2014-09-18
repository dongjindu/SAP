*----------------------------------------------------------------------*
*   INCLUDE ZMMFWTOP                                                   *
*----------------------------------------------------------------------*

data: begin of wa_cabn,
        atinn like cabn-atinn,
        adzhl like cabn-adzhl,
        atnam like cabn-atnam,
      end of wa_cabn.
data: begin of wa_ausp,
        objek like ausp-objek,
        atnam like cabn-atnam,
        atwrt like ausp-atwrt,
        atflv like ausp-atflv,
      end of wa_ausp.
data: begin of wa_fsc_info,
        equnr like equi-equnr,
        plnum like plaf-plnum,
        fscod(18) type c,
        versn(3)  type n,
        vdate     type d,
      end of  wa_fsc_info.
data: begin of wa_ausp_wip,
        objek like ausp-objek,
        atinn like ausp-atinn,
        atwrt like ausp-atwrt,
      end of wa_ausp_wip.
data: begin of wa_key_wip,
        objek like ausp-objek,
        plnum like plaf-plnum,
        usage type c,
        statu(2) type c,
      end of wa_key_wip.
data: begin of it_ausp_status occurs 0,
        objek like ausp-objek,
      end of it_ausp_status.
data: begin of wa_mat_info,
        matnr like mara-matnr,
        mtart like mara-mtart,
        profl like mara-profl,
        meins like mara-meins,
        lbkum like mbew-lbkum,
        maktx like makt-maktx,
      end of wa_mat_info.

data: begin of IT_MAT_INV OCCURS 0,
        matnr like mara-matnr,
        BWKEY like MBEW-BWKEY,
        lbkum like mbew-lbkum,
      end of IT_MAT_INV.

  DATA: BEGIN OF wa_halb,
           matnr LIKE mara-matnr,
           werks LIKE marc-werks,
           datuv LIKE sy-datum,
           bdmng LIKE plaf-bdmng,
           stlal like stko-stlal,
        END OF wa_halb.

  DATA: it_halb LIKE TABLE OF wa_halb,
        it_halb_eng LIKE TABLE OF wa_halb with header line.

data: it_plaf like table of plaf with header line.
data: wa_return like bapiret1,
      wa_header like bapiplaf_e1,
      wa_components like bapi_pldordcomp_e1,
      wa_6022_01 like ztmm_6022_01,
      wa_stpox like stpox.
data: it_cabn like table of wa_cabn,
      it_ausp like table of wa_ausp,
      it_fsc_info like table of wa_fsc_info,
      it_ausp_wip like table of wa_ausp_wip,
      it_key_wip like table of wa_key_wip,
      it_key_wip1 like table of wa_key_wip,
      it_components like table of wa_components,
      it_components1 like table of wa_components,
      it_mat_info like table of wa_mat_info,
      it_ztmm_6022_01 like table of wa_6022_01,
      it_main_stpox like table of wa_stpox,
      it_filter_stpox like table of wa_stpox.
data: w_plnum like plaf-plnum,
      w_fsc(18)  type c,
      w_destn(5)  type c,
      w_modidx(9) type c,
      w_modyr   type c,
      w_ocn(4)  type c,
      w_versn(3) type c,
      w_pack type p,
      w_date type d,
      w_date_time(14) type c,
      w_objek like ausp-objek,
      w_atinn1 like cabn-atinn,     "Destination code
      w_atinn2 like cabn-atinn,     "Plan order
      w_atinn3 like cabn-atinn,     "RP status
      w_atinn4 like cabn-atinn,     "Seq. date
      w_atinn5 like cabn-atinn,     "Usage car
      w_atinn6 like cabn-atinn,     "Version
      w_atinn11 like cabn-atinn,     "
      w_atinn12 like cabn-atinn,     "
      w_atinn13 like cabn-atinn,     "
      w_adzhl1 like cabn-atinn,     "Destn.code counter
      w_adzhl2 like cabn-atinn,     "Plan order counter
      w_adzhl3 like cabn-atinn,     "RP status counter
      w_adzhl4 like cabn-atinn,     "Seq. date counter
      w_adzhl5 like cabn-atinn,     "Usage car counter
      w_adzhl6 like cabn-atinn,     "Version counter
      w_adzhl11 like cabn-atinn,     "
      w_adzhl12 like cabn-atinn,     "
      w_adzhl13 like cabn-atinn.     "

data: w_max type i,
      w_free type i,
      w_lines type i,
      w_rem type i,
      w_no_times type i,
      w_i type i,
      w_frm type i,
      w_to type i.
data: w_taskname(4) type n value '0001',
      w_excep_flag type c,
      w_snd_jobs type i value 1,
      w_rcv_jobs type i value 1.

data: w_len type i,
      w_odealer(2),
      w_ndealer(1).

DATA: wa_6022_tmp LIKE wa_6022_01.
field-symbols: <fs_wip> like line of it_key_wip,
               <fs_ztmm_6022_01> like line of it_ztmm_6022_01.
ranges: r_dt for w_date_time,
        r_mtart for mara-mtart.

*&-------For interface log & ALV display-------------------------------&

**** Constants&Vars for Number range object ****************************
  CONSTANTS: c_nro_nr_00   VALUE '00' LIKE inri-nrrangenr. "Header Part
  CONSTANTS: c_nro_nr_01   VALUE '01' LIKE inri-nrrangenr. "Item Part
  CONSTANTS: c_nro_nr_09   VALUE '09' LIKE inri-nrrangenr. "App. Doc no.
* Number range object
  DATA:      w_nro_object  VALUE 'ZMMNRO0002'
                                 LIKE inri-object. "NRO for ZBDCMSGCOLL
* Number_Get_Next
  DATA:      w_nro_number  TYPE num10.      " Same type of nro_object

  DATA: w_zdocno TYPE num10.       "App. Doc. No.

* For interface log
  DATA: wa_ztca_if_log LIKE ztca_if_log.

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

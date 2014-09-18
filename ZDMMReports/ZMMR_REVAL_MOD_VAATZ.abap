***********************************************************************
* Program Name      : ZMMR_REVAL_MOD_VAATZL
* Author            : Furong Wang
* Creation Date     : 03/2014
* Specifications By : Mike S
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
***********************************************************************
REPORT zmmr_reval_mod_vaatz NO STANDARD PAGE HEADING
                             LINE-SIZE 132
                             LINE-COUNT 64(1)
                             MESSAGE-ID zmmm.
TYPE-POOLS : slis, icon.

TABLES: ekko, ekpo, ztmm_mod_reval, ztmm_mod_rev_res.

DATA : it_comp LIKE TABLE OF ztmm_mod_reval WITH HEADER LINE,
       it_error LIKE TABLE OF ztmm_mod_rev_res WITH HEADER LINE,
       it_pri_selected LIKE TABLE OF ztmm_mod_reval WITH HEADER LINE.

DATA : BEGIN OF it_pri OCCURS 0.
        INCLUDE STRUCTURE zsmm_mod_reval.
DATA:   celltab TYPE lvc_t_styl.
DATA: END OF it_pri.

DATA: it_mod_pri LIKE TABLE OF ztmm_mod_pri_bk WITH HEADER LINE.
DATA: BEGIN OF it_mod_info OCCURS 0,
      vtype LIKE ztmm_assy_cost1-vtype,
      mcode LIKE ztmm_assy_cost1-mcode,
*      lifnr LIKE ztmm_assy_cost1-lifnr,
      matnr LIKE it_pri-matnr,
*      kbetr LIKE konp-kbetr,
*      date_from LIKE sy-datum,
*      date_to LIKE sy-datum,
      celltab TYPE lvc_t_styl,
   END OF it_mod_info.

DATA: BEGIN OF it_mcode OCCURS 0,
      seq(2) TYPE n,
      mcode LIKE ztmm_assy_cost1-mcode,
      END OF it_mcode.

DATA: BEGIN OF it_data OCCURS 0.
        INCLUDE STRUCTURE ztmm_mod_rev_res.
DATA:  comp LIKE ztmm_mod_reval-matnr,
       qty_per LIKE ztmm_mod_pri_bk-qnty,
       upgvc LIKE	ztmm_mod_pri_bk-upgvc.
DATA: END OF it_data.

DATA: it_reval_base LIKE TABLE OF it_data WITH HEADER LINE.

DATA: BEGIN OF it_reval OCCURS 0.
        INCLUDE STRUCTURE ztmm_mod_rev_res.
DATA:   celltab TYPE lvc_t_styl.
DATA: END OF it_reval.

DATA: it_reval_selected LIKE TABLE OF ztmm_mod_rev_res
       WITH HEADER LINE.

DATA : BEGIN OF it_itab OCCURS 0,
       pum_n LIKE ztmm_mod_reval-pum_n,
       matnr LIKE ekpo-matnr,
*      lifnr LIKE ekko-lifnr,
       comp LIKE  ztmm_mod_reval-matnr,
       upgvc  LIKE ztmm_mod_pri_bk-upgvc,
       ztseq LIKE ztmm_mod_reval-ztseq,
       comp_lifnr LIKE ekko-lifnr,
       mod_lifnr LIKE ekko-lifnr,
       sa_update LIKE ztmm_mod_reval-sa_update,
       reval_update LIKE ztmm_mod_reval-reval_update,
       excprice LIKE konp-kbetr,  "(13),
       excfdate LIKE ekko-kdatb,
       exctdate LIKE ekko-kdatb,
       ebeln LIKE ekpo-ebeln,
       ebelp LIKE ekpo-ebelp,
       safdate LIKE ekko-kdatb,
       satdate LIKE ekko-kdatb,
       saprice  LIKE konp-kbetr,
       deleted(1),
       grprice LIKE konp-kbetr,
       grqty LIKE ekpo-menge,
       gramt LIKE konp-kbetr,
       cal_amt LIKE konp-kbetr,
       error_amt LIKE konp-kbetr,
       newamt LIKE konp-kbetr,
       diff  LIKE  konp-kbetr,
       peinh(5),
       comp_diff LIKE konp-kbetr,
       qty_per(10),
       rsn LIKE t686c-kzust,
       multi_sa(3) TYPE n,
       gr_info(1),
       msg(100),
       if(4) TYPE c,
       END OF it_itab.

DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldcat_100  TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_fieldname_100 TYPE slis_t_fieldcat_alv,
       it_fieldcat_110  TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname_110 TYPE slis_t_fieldcat_alv,
       it_fieldcat_210  TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname_210 TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE,
       it_sort_100     TYPE lvc_t_sort WITH HEADER LINE,
       it_sort_210     TYPE lvc_t_sort WITH HEADER LINE.

DATA : wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
       w_fieldname    LIKE LINE OF it_fieldcat,
       w_fieldname_100  LIKE LINE OF it_fieldcat_100,
              w_fieldname_210  LIKE LINE OF it_fieldcat_210,
       wa_is_layout_100 TYPE lvc_s_layo, "/The Layout Structure
       wa_is_layout_110 TYPE lvc_s_layo,
       w_fieldname_110  LIKE LINE OF it_fieldcat_110.

DATA: wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
      wa_variant TYPE disvariant,      "for parameter IS_VARIANT
      wa_variant_100 TYPE disvariant,      "for parameter IS_VARIANT
      wa_variant_110 TYPE disvariant.

DATA: it_exclude TYPE ui_functions.

*DATA: wa_custom_control TYPE        scrfname VALUE 'ALV_CONTAINER',
DATA: alv_grid          TYPE REF TO cl_gui_alv_grid,
      grid_container    TYPE REF TO cl_gui_custom_container.

DATA: alv_grid_100          TYPE REF TO cl_gui_alv_grid,
      alv_grid_110          TYPE REF TO cl_gui_alv_grid,
      alv_grid_210          TYPE REF TO cl_gui_alv_grid,
      g_docking_container_210 TYPE REF TO cl_gui_docking_container,
      g_docking_container_100 TYPE REF TO cl_gui_docking_container,
      g_docking_container_110 TYPE REF TO cl_gui_docking_container,
      g_docking_container TYPE REF TO cl_gui_docking_container.

DATA: ok_code      LIKE sy-ucomm,
      ok_code_110  LIKE sy-ucomm,
      w_repid  LIKE sy-repid,
      w_cnt       TYPE   i,
*      w_no_data(1),
      w_code      LIKE sy-ucomm,
      w_index LIKE sy-tabix,
      w_index_rev LIKE sy-tabix,
      w_index_sa LIKE sy-tabix,
      w_multi_sa(3) TYPE n,
      w_update_flag(1).

*DATA: w_vename LIKE lfa1-name1.

DATA : BEGIN OF it_sa_all OCCURS 0,
         matnr LIKE ekpo-matnr,
         lifnr LIKE ekko-lifnr,
         ebeln LIKE ekpo-ebeln,
         ebelp LIKE ekpo-ebelp,
         werks LIKE ekpo-werks,
         lgort LIKE ekpo-lgort,
         peinh LIKE ekpo-peinh,
         bstyp LIKE ekko-bstyp,
         bukrs LIKE ekko-bukrs,
         bsart LIKE ekko-bsart,
         ekorg LIKE ekko-ekorg,
         ekgrp LIKE ekko-ekgrp,
         kdatb LIKE ekko-kdatb,
         kdate LIKE ekko-kdate,
         loekz LIKE ekpo-loekz,
       END OF it_sa_all.
DATA: it_sa_all_onestep LIKE TABLE OF it_sa_all WITH HEADER LINE.

DATA : w_error(1).
** bdc
DATA : BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA : END OF it_bdc.

DATA : BEGIN OF it_mess OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA : END OF it_mess.

DATA : it_message LIKE it_mess OCCURS 0 WITH HEADER LINE.

DATA : w_mode LIKE ctu_params-dismode VALUE 'N',  "'A'
       l_datab(10),
       l_datbi(10),
       w_rundate LIKE sy-datum,
       w_runtime LIKE sy-uzeit.

DATA: it_log LIKE TABLE OF ztmm_mod_simulog WITH HEADER LINE.
DATA: it_sa_log LIKE TABLE OF ztmm_revalsa_log WITH HEADER LINE.

DATA: bdcdata LIKE bdcdata    OCCURS 0 WITH HEADER LINE.
DATA: messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
DATA: ctumode LIKE ctu_params-dismode VALUE 'E'.
*                                      "A: show all dynpros
*                                      "E: show dynpro on error only
*                                      "N: do not display dynpro,
DATA: cupdate LIKE ctu_params-updmode VALUE  'L'.
*                                      "S: synchronously
*                                      "A: asynchronously
*                                      "L: local.

DATA: BEGIN OF alv_tab OCCURS 1000, " WITH HEADER LINE,
        matnr LIKE ekbe-matnr,
        ebeln LIKE ekbe-ebeln,
        ebelp LIKE ekbe-ebelp,
        bewtp LIKE ekbe-bewtp,  "PO history category

        gjahr LIKE ekbe-gjahr,
        belnr LIKE ekbe-belnr,
        budat LIKE ekbe-budat,
        cpudt LIKE ekbe-cpudt,
        shkzg LIKE ekbe-shkzg,
        menge LIKE ekbe-menge,
        dmbtr LIKE ekbe-dmbtr,
        reewr LIKE ekbe-reewr,   "Inv.Value

        lfgja LIKE ekbe-lfgja,
        lfbnr LIKE ekbe-lfbnr,

        xblnr LIKE ekbe-xblnr,

        bsart LIKE ekko-bsart,   "PO type
        bstyp LIKE ekko-bstyp,   "PO category
        ekgrp LIKE ekko-ekgrp,
        lifnr LIKE ekko-lifnr,

        peinh LIKE ekpo-peinh,
        uebtk LIKE ekpo-uebtk,
        elikz LIKE ekpo-elikz,
        erekz LIKE ekpo-erekz,
        loekz LIKE ekpo-loekz,

        mtart LIKE mara-mtart,
        profl LIKE mara-profl,
        maktx LIKE makt-maktx,
        infnr LIKE eina-infnr,

        refdt     LIKE ekbe-budat,
        yyyymm(6) TYPE c,
        zvbeln    LIKE likp-vbeln,
        asn       LIKE likp-borgr_grp,
        zbelnr    LIKE bsis-belnr,
        duedt     LIKE bsis-zfbdt,
        clrdt     LIKE bsis-augdt,
        augbl     LIKE bsis-augbl,
        blart     LIKE bsis-blart,

        zmenge LIKE ekbe-menge,  "sign
        zdmbtr LIKE ekbe-dmbtr,  "sign
        ivprc  LIKE ekbe-dmbtr,  "IV price

        saprc  LIKE ekbe-dmbtr,  "SA $
        saunt  LIKE konp-kpein,  "SA unit
        saval  LIKE ekbe-dmbtr,  "SA value
        sano(1) TYPE c,          "No SA price

        ifprc  LIKE ekbe-dmbtr,  "info $
        ifunt  LIKE konp-kpein,  "info unit
        ifval  LIKE ekbe-dmbtr,  "info value

        diffa  LIKE ekbe-dmbtr,  "SA-IV
        diffb  LIKE ekbe-dmbtr,  "Info-SA
END OF alv_tab.

RANGES: r_lifnr FOR ekko-lifnr,
        r_matnr FOR ekpo-matnr,
        r_refdt FOR sy-datum,
        r_ebeln FOR ekko-ebeln,
        r_ebelp FOR ekpo-ebelp,
        r_xblnr FOR bkpf-xblnr,
        r_gjahr FOR bkpf-gjahr,
        r_cpudt FOR bkpf-cpudt.

*---------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
handle_button_click FOR EVENT button_click OF cl_gui_alv_grid
*              IMPORTING e_oject e_ucomm,
          IMPORTING es_col_id es_row_no,

handle_hotspot_click FOR  EVENT hotspot_click OF cl_gui_alv_grid
             IMPORTING e_row_id
                       e_column_id
                       es_row_no.
ENDCLASS.                    "lcl_event_receiver DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_button_click.
    PERFORM handle_button_click USING es_col_id es_row_no.
  ENDMETHOD .                    "handle_button_click
  METHOD handle_hotspot_click.
    PERFORM hotspot_click USING e_row_id
                                  e_column_id
                                  es_row_no.

  ENDMETHOD.                    "handle_hotspot_click1
ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

DATA g_event_receiver  TYPE REF TO lcl_event_receiver.
DATA g_event_receiver_110  TYPE REF TO lcl_event_receiver.
DATA g_event_receiver_210  TYPE REF TO lcl_event_receiver.

SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
SELECT-OPTIONS:
   s_pum_n FOR ztmm_mod_reval-pum_n,
   s_lifnr FOR ekko-lifnr,
   s_matnr FOR ekpo-matnr.
PARAMETERS: p_sub LIKE ztmm_mod_reval-sub DEFAULT 'SUB'.
SELECT-OPTIONS: s_ekgrp FOR ekko-ekgrp,
   s_zbdat FOR sy-datum DEFAULT sy-datum.

PARAMETERS: p_status(1) DEFAULT 'A'.

SELECTION-SCREEN END OF BLOCK block1.

SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-007.
PARAMETERS: p_pdate LIKE sy-datum DEFAULT sy-datum OBLIGATORY.
PARAMETERS: p_rver  LIKE somlreci1-receiver." OBLIGATORY.

SELECTION-SCREEN END OF BLOCK block2.

AT SELECTION-SCREEN OUTPUT.
  PERFORM set_screen.

START-OF-SELECTION.
  PERFORM get_data_comp.
  IF it_pri[] IS INITIAL.
    MESSAGE s999 WITH text-m01.
  ELSE.
    CHECK w_error IS INITIAL.
*    PERFORM refresh.
    PERFORM display_data.
  ENDIF.

*&--------------------------------------------------------------------*
*&      Form  get_scheduling_agreement
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM get_scheduling_agreement.

  CLEAR: it_sa_all, it_sa_all[].
  SELECT a~ebeln ebelp matnr werks lgort lifnr
         peinh a~bstyp a~bukrs bsart ekorg
         ekgrp kdatb kdate b~loekz
               INTO CORRESPONDING FIELDS OF TABLE it_sa_all
               FROM ekko AS a INNER JOIN ekpo AS b
                 ON a~mandt EQ b~mandt
                AND a~ebeln EQ b~ebeln
               FOR ALL ENTRIES IN it_reval
               WHERE a~bstyp EQ 'L'
                 AND b~matnr = it_reval-matnr
*                 AND a~lifnr = it_reval-lifnr
                 AND b~loekz = ' '
                 AND b~elikz = ' '.
ENDFORM.                    " get_scheduling_agreement


*&--------------------------------------------------------------------*
*&      Form  dynpro
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->P_0955   text
*      -->P_0956   text
*      -->P_0957   text
*---------------------------------------------------------------------*
FORM dynpro USING    dynbegin
                     name
                     value.
  IF dynbegin = 'X'.
    CLEAR : it_bdc.
    MOVE : name  TO it_bdc-program,
           value TO it_bdc-dynpro,
           'X'   TO it_bdc-dynbegin.
    APPEND it_bdc.
  ELSE .
    CLEAR : it_bdc.
    MOVE : name  TO it_bdc-fnam,
           value TO it_bdc-fval.
    APPEND it_bdc.
  ENDIF.
ENDFORM.                    " dynpro

*&--------------------------------------------------------------------*
*&      Form  get_message
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->P_IT_MESS_MSGID  text
*      -->P_IT_MESS_MSGNR  text
*      -->P_IT_MESS_MSGV1  text
*      -->P_IT_MESS_MSGV2  text
*      -->P_IT_MESS_MSGV3  text
*      -->P_IT_MESS_MSGV4  text
*      <--P_L_MESSA  text
*--------------------------------------------------------------------*
FORM get_message USING    p_msgid
                          p_msgnr
                          p_msgv1
                          p_msgv2
                          p_msgv3
                          p_msgv4
                 CHANGING p_l_messa.
*---
  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
    EXPORTING
      msgid               = p_msgid
      msgnr               = p_msgnr
      msgv1               = p_msgv1
      msgv2               = p_msgv2
      msgv3               = p_msgv3
      msgv4               = p_msgv4
    IMPORTING
      message_text_output = p_l_messa.
ENDFORM.                    " get_message


*
*&--------------------------------------------------------------------*
*&      Form  make_data
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text*  <--  p2        text
*---------------------------------------------------------------------*
FORM make_data.
  DATA: l_flag(1),
         l_multi_sa(3) TYPE n.

  DATA: l_datab LIKE a018-datab,
         l_grqty LIKE ekbe-menge,
         l_gramt LIKE ekbe-dmbtr,
         l_price LIKE ekbe-dmbtr,
         l_newamt LIKE ekbe-dmbtr,
         l_diff LIKE ekbe-dmbtr,
         l_recno TYPE i,
         l_index LIKE sy-tabix,
         l_reval_index LIKE sy-tabix,
         l_belnr LIKE ekbe-belnr,
         l_ebeln LIKE it_itab-ebeln,
         l_matnr LIKE it_itab-matnr,
         l_from LIKE sy-datum,
         l_to LIKE sy-datum.

  DATA: lt_a016 LIKE TABLE OF a016 WITH HEADER LINE,
        lt_itab LIKE TABLE OF it_itab WITH HEADER LINE,
        lt_ekbe LIKE TABLE OF ekbe WITH HEADER LINE.

  SORT it_sa_all BY matnr lifnr.
  SORT it_reval_base BY matnr comp datab datbi.
  SORT it_reval BY pum_n matnr.

  LOOP AT it_reval.
    CLEAR l_flag.
    l_reval_index = sy-tabix.

    lt_itab-matnr = it_reval-matnr.
*    lt_itab-lifnr = it_reval-lifnr.
    lt_itab-pum_n = it_reval-pum_n.
*    lt_itab-ztseq = it_reval-ztseq.
    lt_itab-sa_update = it_reval-sa_update.
    lt_itab-reval_update = it_reval-reval_update.
    lt_itab-excprice = it_reval-price_diff.
    lt_itab-excfdate = it_reval-datab.
    lt_itab-exctdate =  it_reval-datbi.
*    lt_itab-comp_diff = it_REVAL-price_diff.

*    lt_itab-rsn = it_reval-resn_c.
    lt_itab-if = 'C210'.

*    READ TABLE it_sa_all WITH KEY matnr = lt_itab-matnr
*                                  lifnr  = lt_itab-lifnr
*                                  BINARY SEARCH.

    LOOP AT it_sa_all WHERE matnr = lt_itab-matnr.
*                       AND lifnr  = lt_itab-lifnr.
      SELECT * INTO TABLE lt_a016
                     FROM a016
                    WHERE kappl EQ 'M'
                      AND kschl EQ 'PB00'
                      AND evrtn EQ it_sa_all-ebeln
                      AND evrtp EQ it_sa_all-ebelp
                      AND datab <= lt_itab-exctdate
                      AND datbi >= lt_itab-excfdate.

      IF sy-subrc = 0.
        lt_itab-ebeln = it_sa_all-ebeln.
        lt_itab-ebelp = it_sa_all-ebelp.
        lt_itab-peinh = it_sa_all-peinh.
        lt_itab-deleted = it_sa_all-loekz.
        l_index = sy-tabix.

        lt_itab-mod_lifnr = it_sa_all-lifnr.
        it_reval-mod_lifnr = it_sa_all-lifnr.
      ENDIF.
      CLEAR: l_multi_sa.
      DESCRIBE TABLE lt_a016 LINES l_multi_sa.

      LOOP AT lt_a016.
        SELECT SINGLE kbetr INTO lt_itab-saprice
          FROM konp
          WHERE knumh = lt_a016-knumh
            AND kschl = 'PB00'.
*        lt_itab-ebeln = it_sa_all-ebeln.
*        lt_itab-ebelp = it_sa_all-ebelp.
*        lt_itab-peinh = it_sa_all-peinh.
        lt_itab-safdate = lt_a016-datab.
        lt_itab-satdate = lt_a016-datbi.
*        lt_itab-deleted = it_sa_all-loekz.

        l_flag = 'X'.
*        if sy-tabix = 1.

        lt_itab-multi_sa = l_multi_sa.

        APPEND lt_itab.
*        endif.
      ENDLOOP.

      READ TABLE it_reval_base WITH KEY pum_n = it_reval-pum_n
                                       matnr = it_reval-matnr
                                       datab = it_reval-datab
                                       datbi = it_reval-datbi.

      CHECK sy-subrc = 0.
      l_index = sy-tabix.

      LOOP AT it_reval_base FROM l_index WHERE pum_n = it_reval-pum_n
                                     AND matnr = it_reval-matnr
                                     AND datab = it_reval-datab
                                     AND datbi = it_reval-datbi.
        CLEAR: lt_itab.
        lt_itab-matnr = it_reval-matnr.
*        lt_itab-lifnr = it_reval-lifnr.
        lt_itab-pum_n = it_reval-pum_n.
        lt_itab-ztseq = it_reval-ztseq.

*        lt_itab-comp_diff =
*        it_reval_base-price_diff / it_reval_base-qty_per.

*          lt_itab-safdate = lt_a016-datab.
*          lt_itab-satdate = lt_a016-datbi.
*          lt_itab-excfdate = it_reval-datab.
*          lt_itab-exctdate =  it_reval-datbi.
        lt_itab-comp = it_reval_base-comp.
        lt_itab-upgvc = it_reval_base-upgvc.
        lt_itab-qty_per = it_reval_base-qty_per.

        READ TABLE it_pri WITH KEY pum_n = it_reval-pum_n
                                   matnr = lt_itab-comp.
*                                   ztseq = lt_itab-ztseq.

        lt_itab-comp_lifnr = it_pri-lifnr.
        lt_itab-ztseq = it_pri-ztseq.

*          lt_itab-qty_per = it_data-qty_per.
        APPEND lt_itab.
*          CLEAR lt_itab.
      ENDLOOP.
      MODIFY it_reval INDEX l_reval_index TRANSPORTING mod_lifnr.

      CLEAR: it_sa_all, lt_a016[], lt_a016.
    ENDLOOP.
    IF l_flag IS INITIAL.
      DELETE it_reval INDEX   l_reval_index.
    ENDIF.

    CLEAR: lt_itab.
  ENDLOOP.

** make SA update table
  REFRESH it_itab.
  LOOP AT lt_itab.
    REFRESH: r_matnr, r_refdt, r_ebeln, r_ebelp,r_lifnr.

    it_itab = lt_itab.
    IF NOT lt_itab-ebeln IS INITIAL
         AND lt_itab-comp IS INITIAL.

      r_matnr-sign  = 'I'.
      r_matnr-option  = 'EQ'.
      r_matnr-low  = lt_itab-matnr.
      APPEND r_matnr.

      r_refdt-sign  = 'I'.
      r_refdt-option  = 'BT'.

      IF it_itab-safdate = it_itab-excfdate.
        MOVE : it_itab-excfdate TO r_refdt-low.
        IF it_itab-satdate < it_itab-exctdate.
          MOVE it_itab-satdate TO r_refdt-high.
        ELSE.
          MOVE it_itab-exctdate TO r_refdt-high.
        ENDIF.
      ENDIF.

      IF it_itab-excfdate > it_itab-safdate AND
         it_itab-excfdate <= it_itab-satdate.
        MOVE it_itab-excfdate TO r_refdt-low.
        IF it_itab-exctdate > it_itab-satdate.
          MOVE it_itab-satdate TO r_refdt-high.
        ELSE.
          MOVE it_itab-exctdate TO r_refdt-high.
        ENDIF.
      ENDIF.

      IF it_itab-excfdate < it_itab-safdate.
        MOVE it_itab-safdate TO r_refdt-low.
        IF it_itab-exctdate > it_itab-satdate.
          MOVE it_itab-satdate TO r_refdt-high.
        ELSE.
          MOVE it_itab-exctdate TO r_refdt-high.
        ENDIF.
      ENDIF.
*      MOVE it_itab-safdate TO r_refdt-low.
*      MOVE it_itab-satdate TO r_refdt-high.

      APPEND r_refdt.

      r_lifnr-sign  = 'I'.
      r_lifnr-option  = 'EQ'.
      r_lifnr-low  = lt_itab-mod_lifnr.
      APPEND r_lifnr.

      r_ebeln-sign  = 'I'.
      r_ebeln-option  = 'EQ'.
      r_ebeln-low  = lt_itab-ebeln.
      APPEND r_ebeln.

      r_ebelp-sign  = 'I'.
      r_ebelp-option  = 'EQ'.
      r_ebelp-low  = lt_itab-ebelp.
      APPEND r_ebelp.

      FREE MEMORY ID 'SUBMFE'.

      SUBMIT zmmr_sa_reval_get_inv        "ZRFI013
                WITH p_bukrs  = 'H201'
                WITH s_lifnr IN r_lifnr
                WITH s_matnr IN r_matnr
                WITH s_refdt IN r_refdt
                WITH s_ebeln IN r_ebeln
                WITH s_ebelp IN r_ebelp
                WITH p_alv = ' '
                EXPORTING LIST TO MEMORY
                AND RETURN.

      CLEAR: alv_tab, alv_tab[].
      CLEAR: l_grqty, l_gramt, l_price, l_recno.

      IMPORT alv_tab FROM MEMORY ID 'SUBMFE'.

      SORT alv_tab BY matnr.

      LOOP AT alv_tab.
*      if LT_ITAB-EXCPRICE = ALV_TAB-IVPRC.
        l_price = l_price + alv_tab-ivprc.
        l_grqty = l_grqty + alv_tab-zmenge.
        l_gramt = l_gramt + alv_tab-zdmbtr.
        IF alv_tab-zmenge <> 0.
          l_recno = l_recno + 1.
        ENDIF.
        CLEAR: alv_tab.
      ENDLOOP.
*      IF l_recno = 0.
*        CONTINUE.
*      ENDIF.
      it_itab-grqty = l_grqty.
      it_itab-gramt =  l_gramt.

      IF l_grqty <> 0.
        l_price = l_gramt / l_grqty.
      ENDIF.
      it_itab-grprice = l_price.

      it_itab-excprice = it_itab-saprice + it_itab-excprice.
      l_newamt = it_itab-excprice * l_grqty.
      it_itab-newamt = l_newamt.
      l_diff =  l_newamt - l_gramt.
      it_itab-diff = l_diff.
    ELSE.
*      it_itab-grqty = l_grqty.
*      it_itab-diff = it_itab-grqty * it_itab-comp_diff.
    ENDIF.

*-< 08.22.2014 Victor added 2 fields
    it_itab-cal_amt  =  ( it_itab-excprice - it_itab-saprice )
                         * it_itab-grqty.
    it_itab-error_amt  = it_itab-cal_amt - it_itab-diff.
*->
** Furong on 09/04/15 - GR with info no SA (
    IF it_itab-excfdate < it_itab-safdate AND
       it_itab-ebelp <> '0000'.
      IF  it_itab-matnr <> l_matnr.  " it_itab-ebeln <> l_ebeln OR
        l_from = it_itab-excfdate.
        l_to = it_itab-safdate - 1.
        SELECT SINGLE belnr INTO l_belnr
          FROM ekbe
          WHERE ebeln = it_itab-ebeln
*          AND ebelp = it_itab-ebelp
            AND budat BETWEEN l_from AND l_to
            AND vgabe = 1.
        IF sy-subrc = 0.
          it_itab-gr_info = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.
*    l_ebeln = it_itab-ebeln.
    l_matnr = it_itab-matnr.
** )
    APPEND it_itab.
    CLEAR: it_itab.
  ENDLOOP.

ENDFORM.                    " make_data
*&--------------------------------------------------------------------*
*&      Form  display_data
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM display_data.
  CALL SCREEN 100.
ENDFORM.                    " display_data
*&--------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  IF sy-dynnr = '0200'.
    SET PF-STATUS 'ST200'.
    SET TITLEBAR 'T200'.

  ENDIF.

ENDMODULE.                 " STATUS_0200  OUTPUT
*&--------------------------------------------------------------------*
*&      Form  create_container_n_object_9000
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM create_container_n_object.
  DATA: l_repid LIKE sy-repid,
        l_dynnr LIKE sy-dynnr.

  l_repid = sy-repid.
  l_dynnr = sy-dynnr.
  CREATE OBJECT g_docking_container
    EXPORTING
      repid     = l_repid
      dynnr     = l_dynnr
      side      = cl_gui_docking_container=>dock_at_bottom
*     RATIO     = 90
      extension = 2000.

  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = l_repid
        txt2  = sy-subrc
        txt1  = 'The control can not be created'.
  ENDIF.
  CREATE OBJECT alv_grid
    EXPORTING
      i_parent      = g_docking_container
      i_appl_events = 'X'.

ENDFORM.                    " create_container_n_object
*&--------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM set_attributes_alv_grid.
  DATA : lw_s_dragdrop TYPE lvc_s_dd01. "/ Drag&Drop control settings

  CLEAR : wa_is_layout, wa_variant.

*//-- Set Layout Structure
  wa_is_layout-edit       = ' '.      "/Edit Mode Enable
  wa_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  wa_is_layout-language   = sy-langu. "/Language Key
  wa_is_layout-cwidth_opt = 'X'.   "/optimizes the column width
  wa_is_layout-info_fname = 'IF'.
  wa_is_layout-ctab_fname = 'CT'.
  wa_is_layout-stylefname = 'CELLTAB'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

*//-- Set Variant Structure
  wa_variant-report       = sy-repid.
  wa_variant-username     = sy-uname.

ENDFORM.                    " set_attributes_alv_grid
*&--------------------------------------------------------------------*
*&      Form  build_sortcat_display
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM build_sortcat_display.
  REFRESH it_sort.
  it_sort-spos           = 1.
  it_sort-fieldname      = 'PUM_N'.
  it_sort-up             = 'X'.
  it_sort-subtot         = 'X'.
  APPEND it_sort.

  it_sort-spos           = 2.
  it_sort-fieldname      = 'MATNR'.
  it_sort-up             = 'X'.
  it_sort-subtot         = 'X'.
  APPEND it_sort.
ENDFORM.                    " build_sortcat_display

*--------------------------------------------------------------------*
*       FORM setting_fieldcat                                        *
*--------------------------------------------------------------------*
*       ........                                                     *
*--------------------------------------------------------------------*
*  -->  P_FIELDCAT                                                   *
*  -->  P_GUBUN                                                      *
*  -->  P_FIELD                                                      *
*  -->  P_VALUE                                                      *
*--------------------------------------------------------------------*
FORM setting_fieldcat TABLES   p_fieldcat STRUCTURE it_fieldcat
                      USING    p_gubun
                               p_field
                               p_value.
  DATA : l_col(40).

  FIELD-SYMBOLS <fs>.

* START - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'S'.
    CLEAR: p_fieldcat.

    READ TABLE it_fieldname INTO w_fieldname
                            WITH KEY fieldname  = p_field.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH 'Check field catalog'.
    ENDIF.

    MOVE: w_fieldname-fieldname TO p_fieldcat-fieldname.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' p_field  INTO l_col.
  ASSIGN (l_col) TO <fs>.
  MOVE   p_value TO <fs>.

* END - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'E'.
    ADD 1 TO w_cnt.
    p_fieldcat-col_pos = w_cnt.
    APPEND p_fieldcat.
  ENDIF.
ENDFORM.                    " setting_fieldcat

*--------------------------------------------------------------------*
*       FORM assign_itab_to_alv                                      *
*--------------------------------------------------------------------*
*       ........                                                     *
*--------------------------------------------------------------------*
FORM assign_itab_to_alv.

  CALL METHOD alv_grid->set_table_for_first_display
    EXPORTING
      is_layout            = wa_is_layout
*     i_save               = wa_save
*     is_variant           = wa_variant
*     i_default            = space
*     it_toolbar_excluding = it_exclude[]
    CHANGING
      it_fieldcatalog      = it_fieldcat[]
      it_outtab            = it_itab[]
      it_sort              = it_sort[].

ENDFORM.                    "assign_itab_to_alv
*&--------------------------------------------------------------------*
*&      Form  build_field_catalog
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->P_0027   text
*---------------------------------------------------------------------*
FORM build_field_catalog USING p_itab.

  DATA: lw_itab TYPE slis_tabname,
        lw_waers LIKE t001-waers,
        l_rqty(9),
        l_datum(8),
        l_cn(2) TYPE n.

  CLEAR: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].
  CLEAR: w_cnt,w_repid.

  lw_itab = p_itab.

  w_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = w_repid
      i_internal_tabname = lw_itab
      i_inclname         = w_repid
    CHANGING
      ct_fieldcat        = it_fieldname.

  PERFORM setting_fieldcat TABLES it_fieldcat USING :

                               'S' 'PUM_N'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Approval No',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'MATNR'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Module',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'COMP'       ' ',
                                  ' ' 'COLTEXT'     'Part',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'UPGVC'       ' ',
                                  ' ' 'COLTEXT'     'UPGVC',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'COMP_LIFNR'       ' ',
                                  ' ' 'KEY '         ' ',
                                  ' ' 'COLTEXT'     'Comp Vendor',
                                  'E' 'OUTPUTLEN'   '10',

                                'S' 'SAFDATE'    ' ',
                                  ' ' 'COLTEXT'     'SA Date From',
                                  ' ' 'DATATYPE'    'DATS',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'SATDATE'    ' ',
                                  ' ' 'COLTEXT'     'SA Date To',
                                  ' ' 'DATATYPE'    'DATS',
                                  'E' 'OUTPUTLEN'   '10',

                                 'S' 'QTY_PER'       ' ',
                                  ' ' 'COLTEXT'     'Qty Per',
                                  ' ' 'DECIMALS_O'  '0',
                                  ' ' 'NO_ZERO'     'X',
                                  ' ' 'DATATYPE'    'CURR',
                                  'E' 'OUTPUTLEN'   '10',

                                 'S' 'EXCPRICE'       ' ',
                                  ' ' 'COLTEXT'     'New Price',
                                  ' ' 'DECIMALS_O'  '2',
                                  ' ' 'NO_ZERO'     'X',
                                  ' ' 'DATATYPE'    'CURR',
                                  'E' 'OUTPUTLEN'   '13',

                                  'S' 'SAPRICE'       ' ',
                                  ' ' 'COLTEXT'     'Old Price',
                                  ' ' 'DECIMALS_O'  '2',
                                  ' ' 'NO_ZERO'     'X',
                                  ' ' 'DATATYPE'    'CURR',
                                  'E' 'OUTPUTLEN'   '13',

                                  'S' 'GRQTY'       ' ',
                                  ' ' 'COLTEXT'     'GR Qty',
                                  ' ' 'DECIMALS_O'  '0',
                                  ' ' 'NO_ZERO'     'X',
                                  ' ' 'DATATYPE'    'DEC',
                                  ' ' 'DO_SUM'      'X',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'CAL_AMT'       ' ',
                                  ' ' 'COLTEXT'    'Cal.Amt',
                                  ' ' 'DECIMALS_O'  '2',
                                  ' ' 'NO_ZERO'     'X',
                                  ' ' 'DATATYPE'    'CURR',
                                  ' ' 'DO_SUM'      'X',
                                  'E' 'OUTPUTLEN'   '14',

                                  'S' 'DIFF'       ' ',
                                  ' ' 'COLTEXT'    'Reval.Amt',
                                  ' ' 'DECIMALS_O'  '2',
                                  ' ' 'NO_ZERO'     'X',
                                  ' ' 'DATATYPE'    'CURR',
                                  ' ' 'DO_SUM'      'X',
                                  'E' 'OUTPUTLEN'   '14',

                                  'S' 'ERROR_AMT'   ' ',
                                  ' ' 'COLTEXT'     '(Cal-Reval) Amt',
                                  ' ' 'DECIMALS_O'  '2',
                                  ' ' 'NO_ZERO'     'X',
                                  ' ' 'DATATYPE'    'CURR',
                                  ' ' 'DO_SUM'      'X',
                                  'E' 'OUTPUTLEN'   '14',

                                 'S' 'NEWAMT'       ' ',
                                  ' ' 'COLTEXT'     'New Payment',
                                  ' ' 'DECIMALS_O'  '2',
                                  ' ' 'NO_ZERO'     'X',
                                  ' ' 'DATATYPE'    'CURR',
                                  ' ' 'DO_SUM'      'X',
                                  'E' 'OUTPUTLEN'   '13',


                                 'S' 'GRAMT'       ' ',
                                  ' ' 'COLTEXT'     'Old Payment',
                                  ' ' 'DECIMALS_O'  '2',
                                  ' ' 'NO_ZERO'     'X',
                                  ' ' 'DATATYPE'    'CURR',
                                  ' ' 'DO_SUM'      'X',
                                  'E' 'OUTPUTLEN'   '13',

*                                 'S' 'GRPRICE'       ' ',
*                                  ' ' 'COLTEXT'     'Old Avg Price',
*                                  ' ' 'DECIMALS_O'  '2',
*                                  ' ' 'NO_ZERO'     'X',
*                                  ' ' 'DATATYPE'    'CURR',
*                                  'E' 'OUTPUTLEN'   '13',

*                                  'S' 'COMP_DIFF'       ' ',
*                                  ' ' 'COLTEXT'     'Diff (comp)',
*                                  ' ' 'DECIMALS_O'  '2',
*                                  ' ' 'NO_ZERO'     'X',
*                                  ' ' 'DATATYPE'    'CURR',
*                                  'E' 'OUTPUTLEN'   '13',

                                  'S' 'EBELN'       ' ',
                                  ' ' 'COLTEXT'     'SA Number',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'EBELP'       ' ',
                                  ' ' 'COLTEXT'     'I/No',
                                  'E' 'OUTPUTLEN'   '3',


                                  'S' 'EXCFDATE'    ' ',
                                  ' ' 'COLTEXT'     'Date From',
                                  ' ' 'DATATYPE'    'DATS',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'EXCTDATE'    ' ',
                                  ' ' 'COLTEXT'     'Date To',
                                  ' ' 'DATATYPE'    'DATS',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'GR_INFO'   ' ',
                                  ' ' 'COLTEXT'     'GR W Info',
                                  'E' 'OUTPUTLEN'   '1',

                                  'S' 'DELETED'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Del',
                                  'E' 'OUTPUTLEN'   '3',

                                  'S' 'MSG'       ' ',
                                  ' ' 'COLTEXT'     'Message',
                                  'E' 'OUTPUTLEN'   '100'.

ENDFORM.                    " build_field_catalog
*&--------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CLEAR: w_error.
  w_code = ok_code.
  CLEAR : ok_code.

  CASE w_code.
    WHEN 'EXIT'.
*      LEAVE PROGRAM.
    WHEN 'BACK'.
      PERFORM unlock_prog.
      LEAVE TO SCREEN 0.
    WHEN 'REEVAL'.
      PERFORM revaluation.
      PERFORM update_status.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0200  INPUT

*&--------------------------------------------------------------------*
*&      Form  RE_EVAL
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM re_eval.

  DATA: l_answer(1),
         l_error(1),
         l_lines TYPE i,
         l_index LIKE sy-tabix.

  DATA: l_gramt LIKE it_itab-gramt,
        l_diff LIKE it_itab-diff.

  REFRESH it_error.

  PERFORM get_selected_reval.
  DESCRIBE TABLE it_reval_selected LINES l_lines.
  IF l_lines = 0.
    MESSAGE s999 WITH 'Must select at least one module'.
    EXIT.
  ENDIF.

  LOOP AT it_reval_selected.
    AT NEW pum_n.
      SELECT SINGLE *
      FROM ztmm_mod_rev_res
      WHERE pum_n =  it_reval_selected-pum_n.
      IF sy-subrc <> 0.
        MESSAGE e999 WITH 'Please use Revaluation'.
      ENDIF.
    ENDAT.
  ENDLOOP.

  w_update_flag = 'X'.
  PERFORM get_simulation .
  LOOP AT it_reval_selected.
    CLEAR: w_index, w_index_rev, w_index_sa, w_update_flag, it_reval.
    w_index_rev = sy-tabix.

    MESSAGE s999 WITH 'Processing' it_reval_selected-matnr
      ' ' it_reval_selected-pum_n.

    CLEAR: l_error, l_gramt.
    CLEAR: it_itab.
    READ TABLE it_itab WITH KEY  pum_n = it_reval_selected-pum_n
                                 matnr = it_reval_selected-matnr
                                 excfdate = it_reval_selected-datab
                                 exctdate = it_reval_selected-datbi.
*                                BINARY SEARCH.
    IF sy-subrc = 0.
      w_index_sa = sy-tabix.
      w_index =  w_index_sa.
      IF it_itab-multi_sa > 1.
        LOOP AT it_itab FROM w_index
                              WHERE pum_n = it_reval_selected-pum_n
                                AND matnr = it_reval_selected-matnr
                                AND excfdate = it_reval_selected-datab
                                AND exctdate = it_reval_selected-datbi.
          l_gramt = l_gramt + it_itab-gramt.
          l_diff = l_diff + it_itab-diff.
        ENDLOOP.
        w_index =  w_index_sa.
        READ TABLE it_itab INDEX w_index.
      ELSE.
        l_gramt = it_itab-gramt.
        l_diff = it_itab-diff.
      ENDIF.
      IF l_diff IS NOT INITIAL.
        PERFORM re_eval_auto.

      ELSE.
        READ TABLE it_reval WITH KEY pum_n = it_reval_selected-pum_n
                        matnr = it_reval_selected-matnr
                        lifnr = it_reval_selected-lifnr
                        datab = it_reval_selected-datab
                        datbi = it_reval_selected-datbi.
        l_index = sy-tabix.

        w_update_flag = 'X'.

        it_reval-reval_update = 'C'.
        it_reval-reval_date = sy-datum.
        it_reval-reval_user = sy-uname.
        it_reval-zmsg = 'No Invoice found'.

        MODIFY it_reval INDEX l_index TRANSPORTING reval_update
        reval_date reval_user zmsg.

        PERFORM update_mod_log USING 'C'.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF it_error[] IS NOT INITIAL AND p_rver IS NOT INITIAL.
    PERFORM send_email.
  ENDIF.

ENDFORM.                    " RE_EVAL
*&--------------------------------------------------------------------*
*&      Form  UPDATE_SA
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM update_sa USING p_error.

  DATA:   l_field01(20),
          l_kbetr(13),
          l_kpein(5),
          l_kzust LIKE konh-kzust,
          l_kbetr_temp LIKE konp-kbetr,
          l_flag(1).
  DATA: l_99991231 TYPE d VALUE '99991231',
        l_sa_index LIKE sy-tabix.

  DATA: l_ebeln LIKE it_itab-ebeln,
        l_index LIKE sy-tabix.
  DATA: wa_itab LIKE it_itab,
        l_text(90).

  DATA: l_result(1),
        l_messa(255).

  DATA: lw_rev_res LIKE ztmm_mod_rev_res.

  l_index = sy-tabix.

  CLEAR : it_bdc, it_bdc[], it_mess, it_mess[], l_field01,
         l_datab, l_datbi, l_kbetr,l_flag.

  IF it_itab-ebeln IS INITIAL.
    CLEAR: it_itab.
    p_error = 'X'.
    CONCATENATE 'Please select correct data for'
     it_itab-matnr INTO l_text
     SEPARATED BY space.
    MESSAGE w999 WITH l_text.
    EXIT.
  ENDIF.

  IF it_itab-safdate = it_itab-excfdate.
    WRITE : it_itab-excfdate TO l_datab.
    IF it_itab-satdate < it_itab-exctdate.
      WRITE: it_itab-satdate TO l_datbi.
    ELSE.
      WRITE: it_itab-exctdate TO l_datbi.
    ENDIF.
  ENDIF.

  IF it_itab-excfdate > it_itab-safdate AND
     it_itab-excfdate <= it_itab-satdate.
    WRITE : it_itab-excfdate TO l_datab.
    IF it_itab-exctdate > it_itab-satdate.
      WRITE: it_itab-satdate TO l_datbi.
    ELSE.
      WRITE: it_itab-exctdate TO l_datbi.
    ENDIF.
  ENDIF.

  IF it_itab-excfdate < it_itab-safdate.
    WRITE : it_itab-safdate TO l_datab.
    IF it_itab-exctdate > it_itab-satdate.
      WRITE: it_itab-satdate TO l_datbi.
    ELSE.
      WRITE: it_itab-exctdate TO l_datbi.
    ENDIF.
  ENDIF.

*  WRITE : it_itab-safdate TO l_datab.
*  WRITE: it_itab-satdate TO l_datbi.

  PERFORM dynpro USING : 'X'  'SAPMM06E'        '0205',
                          ' '  'RM06E-EVRTN'     it_itab-ebeln,
                          ' '  'BDC_OKCODE'      '=AB'.

  CONCATENATE 'RM06E-TCSELFLAG(' it_itab-ebelp+3(2) ')'
      INTO l_field01.

  PERFORM dynpro USING : 'X'  'SAPMM06E'        '0220',
                         ' '  l_field01         'X',
                         ' '  'BDC_OKCODE'      '=KO'.

  PERFORM dynpro USING : 'X'  'SAPLV14A'        '0102',
                         ' '  'BDC_OKCODE'      '=NEWD'.

  PERFORM dynpro USING : 'X'  'SAPMV13A'        '0201',
                         ' '  'RV13A-DATAB'     l_datab,
                         ' '  'RV13A-DATBI'     l_datbi.
*    IF SY-SUBRC EQ 0.
  CLEAR : l_kbetr, l_kpein.
  WRITE : it_itab-excprice TO l_kbetr.
*                               CURRENCY 'USD'.
  WRITE : '1' TO l_kpein.
  PERFORM dynpro USING : ' '  'KONP-KBETR(01)'  l_kbetr,
                         ' '  'BDC_CURSOR'      'KONP-KPEIN(01)',
                         ' '  'KONP-KPEIN(01)'  l_kpein.

  PERFORM dynpro USING : 'X'  'SAPMV13A'        '0201',
                              ' '  'BDC_CURSOR'   'KONP-KSCHL(02)',
                               ' '  'BDC_OKCODE'      '=EINF'.

  PERFORM dynpro USING : 'X'  'SAPMV13A'        '0201',
                         ' '  'BDC_OKCODE'      '=KDAT'.

  CLEAR : l_kzust.
*    MOVE : IT_INFO_ITEM-KZUST TO L_KZUST.

  PERFORM dynpro USING : 'X'  'SAPMV13A'        '0200',
                         ' '  'KONH-KZUST'      l_kzust,

                         ' '  'KONH-KOSRT'      it_itab-pum_n,

                         ' '  'BDC_OKCODE'      '=BACK'.

  PERFORM dynpro USING : 'X'  'SAPMM06E'        '0220',
                         ' '  'BDC_OKCODE'      '=BU'.

  PERFORM dynpro USING : 'X'  'SAPLSPO1'        '0300',
                         ' '  'BDC_OKCODE'      '=YES'.

  CALL TRANSACTION 'ME32L' USING it_bdc
                           MODE w_mode
                           UPDATE 'S'
                           MESSAGES INTO it_mess.

  APPEND LINES OF it_mess TO it_message.


  READ TABLE it_mess WITH KEY msgtyp = 'E'.

  IF sy-subrc EQ 0.
    p_error = 'X'.
    it_itab-sa_update = ' '.
    it_itab-msg = 'Error in SA Update: '.
    l_result = 'E'.
    PERFORM get_message USING    it_mess-msgid
                                    it_mess-msgnr
                                    it_mess-msgv1
                                    it_mess-msgv2
                                    it_mess-msgv3
                                    it_mess-msgv4
                           CHANGING l_messa.
    CONCATENATE  it_itab-msg l_messa INTO l_messa
      SEPARATED BY space.

  ELSE.
    READ TABLE it_mess WITH KEY msgtyp = 'S'.
    IF sy-subrc EQ 0.
      it_itab-sa_update = 'X'.
      it_itab-msg = 'Successfully updated'.
      l_result = 'S'.

    ELSE.
      it_itab-sa_update = ' '.
      PERFORM get_message USING    it_mess-msgid
                                    it_mess-msgnr
                                    it_mess-msgv1
                                    it_mess-msgv2
                                    it_mess-msgv3
                                    it_mess-msgv4
                           CHANGING it_itab-msg.
      l_result = 'E'.

    ENDIF.
  ENDIF.

  MODIFY it_itab INDEX w_index   "lt_rows-index
          TRANSPORTING sa_update msg.

  it_reval-sa_update = it_itab-sa_update.
  it_reval-zmsg = it_itab-msg.
  it_reval-sa_date = sy-datum.
  it_reval-sa_user = sy-uname.

  READ TABLE it_reval_selected INDEX w_index_rev.

  READ TABLE it_reval WITH KEY pum_n = it_reval_selected-pum_n
                       matnr = it_reval_selected-matnr
                       lifnr = it_reval_selected-lifnr
                       datab = it_reval_selected-datab
                       datbi = it_reval_selected-datbi.
  l_index = sy-tabix.

  it_reval-sa_update = it_itab-sa_update.
  it_reval-zmsg = it_itab-msg.
  it_reval-sa_date = sy-datum.
  it_reval-sa_user = sy-uname.

  MODIFY it_reval INDEX l_index
          TRANSPORTING sa_update zmsg sa_date sa_user.

  UPDATE ztmm_mod_rev_res
    SET: sa_update = it_reval-sa_update
         zmsg = it_reval-zmsg
         sa_date = sy-datum
         sa_user = sy-uname
   WHERE matnr = it_reval-matnr
     AND lifnr = it_reval-lifnr
     AND pum_n = it_reval-pum_n
     AND datab = it_reval-datab
     AND datbi = it_reval-datbi.

  IF sy-subrc = 0 OR sy-subrc = 4.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
    MESSAGE w999 WITH 'ztmm_mod_rev_res updat error'.
    p_error ='E'.
  ENDIF.

  w_update_flag = 'X'.

ENDFORM.                    " UPDATE_SA
**&--------------------------------------------------------------------*
**&      Form  double_click_rtn
**&--------------------------------------------------------------------*
**       text
**---------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**---------------------------------------------------------------------*
*FORM double_click_rtn.
*
*  DATA: lw_field(40),
*        lw_line TYPE i..
*
*  CHECK NOT it_itab IS INITIAL.
*
*  GET CURSOR FIELD lw_field.
*  GET CURSOR FIELD lw_field LINE lw_line.
*
*  IF lw_field = 'IT_ITAB-GRPRICE'.
*    READ TABLE it_itab INDEX lw_line.
*
*    r_matnr-sign  = 'I'.
*    r_matnr-option  = 'EQ'.
*    r_matnr-low  = it_itab-matnr.
*    APPEND r_matnr.
*
*    r_refdt-sign  = 'I'.
*    r_refdt-option  = 'BT'.
*    r_refdt-low  = it_itab-excfdate.
*    r_refdt-high = it_itab-exctdate.
*    APPEND r_refdt.
*
*    r_ebeln-sign  = 'I'.
*    r_ebeln-option  = 'EQ'.
*    r_ebeln-low  = it_itab-ebeln.
*    APPEND r_ebeln.
*
*    SUBMIT zmmr_sa_reval_get_inv                            "ZRFI013
*              WITH p_bukrs  = 'H201'
*              WITH s_lifnr IN r_lifnr
*              WITH s_matnr IN r_matnr
*              WITH s_budat IN r_refdt
*              WITH s_ebeln IN r_ebeln
*              EXPORTING LIST TO MEMORY
*              AND RETURN.
*  ENDIF.
*ENDFORM.                    " double_click_rtn
*&--------------------------------------------------------------------*
*&      Form  DE_AGGREgate
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM de_aggregate.
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
      lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_lines TYPE i.
  RANGES: r_ebeln FOR ekko-ebeln,
          r_werks FOR ekpo-werks.

  CALL METHOD alv_grid_210->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows[]
      et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = w_repid
        txt2  = sy-subrc
        txt1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  PERFORM get_selected_reval.
  DESCRIBE TABLE it_reval_selected LINES l_lines.
  IF l_lines = 0.
    MESSAGE s999 WITH 'Must select at least one module'.
    EXIT.
  ENDIF.

  LOOP AT it_reval_selected.
    AT NEW pum_n.
      SELECT SINGLE *
      FROM ztmm_mod_rev_res
      WHERE pum_n =  it_reval_selected-pum_n.
      IF sy-subrc <> 0.
        MESSAGE e999 WITH 'Please use RE_PROCESS function'.
      ENDIF.
    ENDAT.
  ENDLOOP.

  w_update_flag = 'X'.
  PERFORM get_simulation.
  IF it_itab[] IS INITIAL.
    MESSAGE s999 WITH 'Error in reading Simulation Log'.
    EXIT.
  ENDIF.

  LOOP AT it_reval_selected.
    CLEAR: it_itab.
    READ TABLE it_itab WITH KEY  pum_n = it_reval_selected-pum_n
                                matnr = it_reval_selected-matnr
                                excfdate = it_reval_selected-datab
                                exctdate = it_reval_selected-datbi.
    IF sy-subrc = 0 AND it_itab-matnr <> ' '.
      r_ebeln-sign = 'I'.
      r_ebeln-option = 'EQ'.
      r_ebeln-low = it_itab-ebeln.
      APPEND r_ebeln.
    ENDIF.
  ENDLOOP.

  r_werks-sign = 'I'.
  r_werks-option = 'EQ'.
  r_werks-low = 'P001'..
  APPEND r_werks.

  SUBMIT z_ekbeaufl    "via selection-screen
         WITH s_werks IN r_werks
         WITH s_ebeln IN r_ebeln
         WITH update = 'X'
         WITH p_alv = ' '
         AND RETURN.

ENDFORM.                    " DE_AGGREgate

*&--------------------------------------------------------------------*
*&      Form  de_aggregate_ONESTEP
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM de_aggregate_onestep USING p_error.

  DATA: l_cn TYPE i.
  RANGES: r_ebeln FOR ekko-ebeln,
          r_werks FOR ekpo-werks.

  CHECK it_itab-gramt <> 0.
  r_ebeln-sign = 'I'.
  r_ebeln-option = 'EQ'.
  r_ebeln-low = it_itab-ebeln.
  COLLECT r_ebeln.

  CLEAR: p_error.
  r_werks-sign = 'I'.
  r_werks-option = 'EQ'.
  r_werks-low = 'P001'..
  APPEND r_werks.

  SUBMIT z_ekbeaufl    "via selection-screen
         WITH s_werks IN r_werks
         WITH s_ebeln IN r_ebeln
         WITH update = 'X'
         WITH p_alv = ' '
         AND RETURN.
  IF sy-subrc = 0.
  ELSE.
    p_error = 'X'.
  ENDIF.
ENDFORM.                    " DE_AGGREgate_ONESTEP
*&--------------------------------------------------------------------*
*&      Form  check_reason_code
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM check_reasoncode_module.
  DATA: l_kzust LIKE t686c-kzust,
        l_seq(2) TYPE n,
        l_index LIKE sy-tabix,
        l_text(50),
        l_mcode(2).

  DATA ls_style TYPE lvc_s_styl .
  FIELD-SYMBOLS: <fs>.

  IF it_pri[] IS NOT INITIAL.
    LOOP AT it_pri.
      REFRESH: it_pri-celltab.
      MODIFY it_pri.
    ENDLOOP.
  ENDIF.

  REFRESH: it_mcode, it_mod_pri, it_pri.

  SELECT mcode INTO CORRESPONDING FIELDS OF TABLE it_mcode
    FROM ztmm_assy_cost1.
  SORT it_mcode BY mcode.
  DELETE ADJACENT DUPLICATES FROM it_mcode.

  LOOP AT it_mcode.
    l_seq = l_seq + 1.
    it_mcode-seq = l_seq.
    MODIFY it_mcode.
  ENDLOOP.

  SELECT * INTO TABLE it_mod_pri
    FROM ztmm_mod_pri_bk
    FOR ALL ENTRIES IN it_comp
    WHERE comp = it_comp-matnr.
*      AND input_date >= it_comp-term_from
*      AND input_date <= it_comp-term_to.

  SORT it_mod_pri BY comp.

  CLEAR: w_error.
  LOOP AT it_comp.
    MOVE-CORRESPONDING it_comp TO it_pri.
    SELECT SINGLE kzust INTO l_kzust
      FROM  t686c
      WHERE kzust = it_pri-resn_c.
    IF sy-subrc = 0.
    ELSE.
      it_pri-zmsg = 'Reason Code not found: '.
    ENDIF.

    READ TABLE it_mod_pri WITH KEY comp = it_pri-matnr
                          BINARY SEARCH TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      l_index = sy-tabix.
      UNASSIGN <fs>.
      LOOP AT it_mod_pri FROM l_index WHERE comp = it_pri-matnr.
        l_mcode = it_mod_pri-matnr+3(2).
        READ TABLE it_mcode WITH KEY mcode = l_mcode.
        CHECK sy-subrc = 0.
        CONCATENATE 'IT_PRI-MCODE' it_mcode-seq INTO l_text.
        ASSIGN (l_text) TO <fs>.
        <fs> = 'X'.
      ENDLOOP.
    ENDIF.

    APPEND it_pri.
  ENDLOOP.
  LOOP AT it_pri.
    it_pri-mod_icon = icon_businav_sysorgi.
    ls_style-fieldname = 'MOD_ICON'.
    ls_style-style = cl_gui_alv_grid=>mc_style_button.
    APPEND ls_style TO it_pri-celltab.
    MODIFY it_pri.
  ENDLOOP.
ENDFORM.                    " check_reason_code_module
*&--------------------------------------------------------------------*
*&      Form  DISPLAY_SA
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM display_sa.
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows

  CALL METHOD alv_grid->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows[]
      et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = w_repid
        txt2  = sy-subrc
        txt1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

*  CLEAR: w_select, w_success, w_fail.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.
  READ TABLE it_itab INDEX lt_rows-index.

  SET PARAMETER ID 'VRT' FIELD it_itab-ebeln.
  CALL TRANSACTION 'ME33' AND SKIP FIRST SCREEN.

ENDFORM.                    " DISPLAY_SA
*&--------------------------------------------------------------------*
*&      Form  DISPLAY_info
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM display_info.
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
         lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_lifnr LIKE ekko-lifnr.

  CALL METHOD alv_grid->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows[]
      et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = w_repid
        txt2  = sy-subrc
        txt1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.
  READ TABLE it_itab INDEX lt_rows-index.

  SELECT SINGLE lifnr INTO l_lifnr
    FROM ekko
    WHERE ebeln = it_itab-ebeln.

  SET PARAMETER ID 'LIF' FIELD l_lifnr.
  SET PARAMETER ID 'MAT' FIELD it_itab-matnr.
  SET PARAMETER ID 'WRK' FIELD '    '.
  CALL TRANSACTION 'ME13' AND SKIP FIRST SCREEN.

ENDFORM.                    " DISPLAY_info
*&--------------------------------------------------------------------*
*&      Form  GET_DATA
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM get_data_comp.

  w_rundate = sy-datum.
  w_runtime = sy-uzeit.
  REFRESH it_comp.
  IF p_status = 'A'.
    SELECT * INTO TABLE it_comp
    FROM ztmm_mod_reval
    WHERE matnr IN s_matnr
      AND lifnr IN s_lifnr
      AND purch_g IN s_ekgrp
      AND pum_n IN s_pum_n
      AND zbdat IN s_zbdat
      AND zresult <> 'E'.
  ELSE.
    SELECT * INTO TABLE it_comp
      FROM ztmm_mod_reval
      WHERE matnr IN s_matnr
        AND lifnr IN s_lifnr
        AND purch_g IN s_ekgrp
        AND pum_n IN s_pum_n
        AND zbdat IN s_zbdat
        AND reval_update = p_status
        AND zresult <> 'E'.
  ENDIF.
  SORT it_comp BY pum_n.
  IF sy-subrc = 0.
    PERFORM check_reasoncode_module.
  ENDIF.

ENDFORM.                    " GET_DATA
*&--------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_N_OBJECT_100
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM create_container_n_object_100 .
  DATA:   l_repid LIKE sy-repid,
         l_dynnr LIKE sy-dynnr.

  l_repid = sy-repid.
  l_dynnr = sy-dynnr.
  CREATE OBJECT g_docking_container_100
    EXPORTING
      repid     = l_repid
      dynnr     = l_dynnr
      side      = cl_gui_docking_container=>dock_at_bottom
*     RATIO     = 90
      extension = 2000.

  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = w_repid
        txt2  = sy-subrc
        txt1  = 'The control can not be created'.
  ENDIF.
  CREATE OBJECT alv_grid_100
    EXPORTING
      i_parent      = g_docking_container_100
      i_appl_events = 'X'.

  CREATE OBJECT g_event_receiver.
  SET HANDLER g_event_receiver->handle_button_click FOR alv_grid_100.

ENDFORM.                    " CREATE_CONTAINER_N_OBJECT_100

*&--------------------------------------------------------------------*
*&      Form  build_field_catalog_100
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->P_ITAB     text
*---------------------------------------------------------------------*
FORM build_field_catalog_100 USING p_itab.
  DATA: lw_itab TYPE slis_tabname,
         lw_waers LIKE t001-waers,
         l_rqty(9),
         l_datum(8),
         l_cn(2) TYPE n,
         l_mcode_seq(30).

  CLEAR: it_fieldcat_100,  it_fieldcat_100[],
         it_fieldname_100, it_fieldname_100[].
  CLEAR: w_cnt,w_repid.

  lw_itab = p_itab.

  w_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = w_repid
      i_internal_tabname = lw_itab
      i_inclname         = w_repid
    CHANGING
      ct_fieldcat        = it_fieldname_100.

  PERFORM setting_fieldcat_100 TABLES it_fieldcat_100 USING :

                                  'S' 'PUM_N'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Approval No',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'ZTSEQ'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Serial No',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'MATNR'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Material',
                                  'E' 'OUTPUTLEN'   '18',

                                 'S' 'LIFNR'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Vendor',
                                  'E' 'OUTPUTLEN'   '8',


*                                  'S' 'RESN_C'    ' ',
*                                  ' ' 'COLTEXT'     'Reason Code',
*                                  'E' 'OUTPUTLEN'   '6',

                                  'S' 'TERM_PRICE'  ' ',
                                  ' ' 'COLTEXT'     'Term Price ',
                                  ' ' 'DECIMALS_O'  '2',
                                  ' ' 'NO_ZERO'     'X',
                                  ' ' 'DATATYPE'    'CURR',
                                  'E' 'OUTPUTLEN'   '13',

                                  'S' 'TERM_FROM'    ' ',
                                  ' ' 'COLTEXT'     'Date From',
                                  ' ' 'DATATYPE'    'DATS',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'TERM_TO'    ' ',
                                  ' ' 'COLTEXT'     'Date To',
                                  ' ' 'DATATYPE'    'DATS',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'SUB'       ' ',
                                  ' ' 'COLTEXT'     'Sub',
                                  'E' 'OUTPUTLEN'   '4',

                                  'S' 'SA_UPDATE'       ' ',
                                  ' ' 'COLTEXT'     'SA Update',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'SA_DATE'       ' ',
                                  ' ' 'COLTEXT'     'SA Date',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'SA_USER'       ' ',
                                  ' ' 'COLTEXT'     'SA User',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'REVAL_UPDATE'    ' ',
                                  ' ' 'COLTEXT'     'Reval Update',
                                  'E' 'OUTPUTLEN'   '6',

                                  'S' 'REVAL_DATE'    ' ',
                                  ' ' 'COLTEXT'     'Reval Date',
                                  'E' 'OUTPUTLEN'   '6',

                                 'S' 'REVAL_USER'       ' ',
                                  ' ' 'COLTEXT'     'Reval User',
                                  'E' 'OUTPUTLEN'   '12'.

*                                 'S' 'BELNR'       ' ',
*                                  ' ' 'COLTEXT'     'Account Doc',
*                                  'E' 'OUTPUTLEN'   '12',
*
*                                 'S' 'INV_AMT'       ' ',
*                                  ' ' 'COLTEXT'     'Reval Amt',
*                                  ' ' 'NO_ZERO'     'X',
*                                  'E' 'OUTPUTLEN'   '12'.

  LOOP AT it_mcode.
    CONCATENATE 'MCODE' it_mcode-seq INTO l_mcode_seq.
    PERFORM setting_fieldcat_100 TABLES it_fieldcat_100 USING :
                                    'S' l_mcode_seq   ' ',
                                    ' ' 'COLTEXT'     it_mcode-mcode,
                                    'E' 'OUTPUTLEN'   '2'.
  ENDLOOP.

  PERFORM setting_fieldcat_100 TABLES it_fieldcat_100 USING :
                                   'S' 'MOD_ICON'       ' ',
                                   ' ' 'COLTEXT'     'Mod Item',
                                   ' ' 'ICON'       'X',
                                   'E' 'OUTPUTLEN'   '4'.

ENDFORM.                    " build_field_catalog
*&--------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ST100'.
  SET TITLEBAR 'T100'.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&--------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_0200  OUTPUT
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
MODULE display_alv_0100 OUTPUT.
  IF g_docking_container_100 IS INITIAL.
    PERFORM create_container_n_object_100.
    PERFORM set_attributes_alv_grid_100.
    PERFORM build_sortcat_display_100.
    PERFORM build_field_catalog_100 USING 'IT_PRI'.
    PERFORM assign_itab_to_alv_100.
  ELSE.
    CALL METHOD alv_grid_100->refresh_table_display.
  ENDIF.
ENDMODULE.                 " DISPLAY_ALV_0200  OUTPUT
*&--------------------------------------------------------------------*
*&      Form  ASSIGN_ITAB_TO_ALV_100
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM assign_itab_to_alv_100 .
  CALL METHOD alv_grid_100->set_table_for_first_display
    EXPORTING
      is_layout       = wa_is_layout_100
      i_save          = wa_save
      is_variant      = wa_variant_100
    CHANGING
      it_fieldcatalog = it_fieldcat_100[]
      it_sort         = it_sort_100[]
      it_outtab       = it_pri[].
ENDFORM.                    " ASSIGN_ITAB_TO_ALV_100
*&--------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  w_code = ok_code.
  CASE w_code.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'REFRESH'.
      PERFORM refresh.
    WHEN 'PREV'.
      PERFORM simulation.
    WHEN 'STATUS'.
      PERFORM re_process.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&--------------------------------------------------------------------*
*&      Form  SETTING_FIELDCAT_100
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->P_IT_FIELDCAT_100  text
*      -->P_3903   text
*      -->P_3904   text
*      -->P_3905   text
*---------------------------------------------------------------------*
FORM setting_fieldcat_100 TABLES p_fieldcat STRUCTURE it_fieldcat_100
                      USING    p_gubun
                               p_field
                               p_value.
  DATA : l_col(40).

  FIELD-SYMBOLS <fs>.

* START - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'S'.
    CLEAR: p_fieldcat.

    READ TABLE it_fieldname_100 INTO w_fieldname_100
                            WITH KEY fieldname  = p_field.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH 'Check field catalog'.
    ENDIF.

    MOVE: w_fieldname_100-fieldname TO p_fieldcat-fieldname.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' p_field  INTO l_col.
  ASSIGN (l_col) TO <fs>.
  MOVE   p_value TO <fs>.

* END - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'E'.
    ADD 1 TO w_cnt.
    p_fieldcat-col_pos = w_cnt.
    APPEND p_fieldcat.
  ENDIF.
ENDFORM.                    " setting_fieldcat
*&--------------------------------------------------------------------*
*&      Form  SEND_EMAIL
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM send_email .
  DATA: lt_body LIKE TABLE OF solisti1 WITH HEADER LINE.

  DATA: l_subject TYPE p15_text150,
        l_p_rec_type  LIKE  somlreci1-rec_type.

*  MOVE 'Following Document(s) are created' TO lt_body.
  MOVE 'Following items have error in Revaluation:' TO lt_body.
  APPEND lt_body.
  CLEAR: lt_body.
  MOVE '=================================' TO lt_body.
  APPEND lt_body.
  CLEAR: lt_body.

  MOVE: 'Approval No' TO lt_body+0(10),
        'Module No' TO lt_body+10(20),
        'Vendor' TO lt_body+30(10),
        'Compont' TO lt_body+40(20)..

  APPEND lt_body.
  CLEAR: lt_body.

  MOVE: '--------------------' TO  lt_body+0(20),
        '------------------------------' TO  lt_body+20(30).
  APPEND lt_body.
  CLEAR: lt_body.

  LOOP AT it_error.

    MOVE: it_error-pum_n TO lt_body+0(10),
          it_error-matnr TO lt_body+10(20),
          it_error-lifnr TO lt_body+30(10).
*          it_error-comp TO lt_body+40(20).

    APPEND lt_body.
  ENDLOOP.

  CALL FUNCTION 'ZCAF_SEND_EMAIL'
    EXPORTING
      p_subject  = 'Re_evaluation Error List (Module Part)'
      p_rec_type = 'C'
      p_receiver = p_rver
    TABLES
      pt_body    = lt_body.

ENDFORM.                    " SEND_EMAIL

*&--------------------------------------------------------------------*
*&      Form  UPDATE_LOG_REVAL
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->P_LT_EKBE_BELNR  text
*---------------------------------------------------------------------*
*FORM update_log_reval USING p_belnr.
*
*  UPDATE ztmm_mod_simulog
*       SET: reval_user = sy-uname
*            reval_update = 'X'
*            reval_msg = p_belnr
*            reval_date = sy-datum
*            zbdat = p_pdate
*         WHERE matnr = it_itab-matnr
*               AND lifnr = it_itab-lifnr
*               AND pum_n = it_itab-pum_n
*               AND ztseq = it_itab-ztseq
*               AND reval_msg = ' '.
*  IF sy-subrc EQ 0.
*    COMMIT WORK.
*  ELSE.
*    ROLLBACK WORK.
*    MESSAGE s999 WITH 'Error in updating log table for'
*           it_log-pum_n
*                 DISPLAY LIKE 'E'
*.
*  ENDIF.
*
*  UPDATE ztmm_revalsa_log
*       SET: reval_user = sy-uname
*            reval_update = 'X'
*            reval_msg = p_belnr
*            reval_date = sy-datum
*            zbdat = p_pdate
*         WHERE matnr = it_itab-matnr
*               AND lifnr = it_itab-lifnr
*               AND pum_n = it_itab-pum_n
*               AND ztseq = it_itab-ztseq
*               AND reval_msg = ' '.
*  IF sy-subrc EQ 0.
*    COMMIT WORK.
*  ELSE.
*    ROLLBACK WORK.
*    MESSAGE s999 WITH 'Error in updating SA log table for'
*       it_log-pum_n DISPLAY LIKE 'E'.
*  ENDIF.
*
*ENDFORM.                    " UPDATE_LOG_REVAL

*&--------------------------------------------------------------------*
*&      Form  GET_SELECT_DATA
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM get_selected_data .

  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i,
        l_index LIKE sy-tabix.
*  DATA: l_pum_n LIKE it_pri-pum_n.

  DATA: BEGIN OF lt_pum_n OCCURS 0,
        pum_n LIKE it_pri-pum_n,
        END OF lt_pum_n.

  CALL METHOD alv_grid_100->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows[]
      et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = w_repid
        txt2  = sy-subrc
        txt1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  DESCRIBE TABLE lt_rows LINES l_line.
  IF l_line = 0.
    MESSAGE e001.
  ENDIF.

  REFRESH: it_pri_selected.

*  LOOP AT lt_rows.
*    CLEAR: it_pri_selected.
*    READ TABLE it_pri INDEX lt_rows-index.
*    l_pum_n = it_pri-pum_n.
*    LOOP AT it_pri WHERE pum_n = l_pum_n.
*      MOVE-CORRESPONDING it_pri TO it_pri_selected.
*      COLLECT it_pri_selected.
*    ENDLOOP.
*  ENDLOOP.
  LOOP AT lt_rows.
    READ TABLE it_pri INDEX lt_rows-index.
    lt_pum_n-pum_n = it_pri-pum_n.
    COLLECT lt_pum_n.
  ENDLOOP.

  DESCRIBE TABLE lt_pum_n LINES l_line.
  IF l_line > 1.
    MESSAGE e999 WITH 'One Approval number is processed once'.
  ENDIF.
  SORT lt_pum_n BY pum_n.
  LOOP AT lt_pum_n.
    READ TABLE it_pri WITH KEY pum_n = lt_pum_n-pum_n.
    l_index = sy-tabix.
    LOOP AT it_pri FROM l_index WHERE pum_n = lt_pum_n-pum_n.
      CLEAR: it_pri_selected.
      MOVE-CORRESPONDING it_pri TO it_pri_selected.
      APPEND it_pri_selected.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " GET_SELECT_DATA
*&--------------------------------------------------------------------*
*&      Form  REFRESH
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM refresh .
  PERFORM reset_status.
  PERFORM  get_data_comp.
ENDFORM.                    " REFRESH
*&--------------------------------------------------------------------*
*&      Form  RE_EVALUATION
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM revaluation .

  DATA: l_answer(1),
        l_error(1),
        l_lines TYPE i,
        l_index LIKE sy-tabix.

  DATA: l_gramt LIKE it_itab-gramt,
        l_diff LIKE it_itab-diff.

  REFRESH it_error.

  LOOP AT it_itab WHERE error_amt <> 0.
    MESSAGE e999 WITH 'You can not do the revaluation until you fix '
                      'your data issue. :' it_itab-matnr
                      it_itab-error_amt.
  ENDLOOP.

  LOOP AT it_itab WHERE gr_info = 'X'.
    MESSAGE e999 WITH 'You can not do the revaluation until you fix '
                      'the SA Period Price for ' it_itab-matnr.
  ENDLOOP.

  PERFORM locking_rtn USING sy-repid  l_answer.
  IF  l_answer = 'E'.
*    PERFORM dequeue_prg.
    MESSAGE e009 WITH text-m03.
  ENDIF.

  CLEAR: l_answer.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question  = 'Are you sure to process?'
    IMPORTING
      answer         = l_answer
    EXCEPTIONS
      text_not_found = 1
      OTHERS         = 2.

  CHECK l_answer = '1'.

  PERFORM get_selected_reval.
  DESCRIBE TABLE it_reval_selected LINES l_lines.
  IF l_lines = 0.
    MESSAGE s999 WITH 'Must select at least one module'.
    EXIT.
  ENDIF.

  LOOP AT it_reval_selected.
    AT NEW pum_n.
      SELECT SINGLE *
      FROM ztmm_mod_rev_res
      WHERE pum_n =  it_reval_selected-pum_n.
      IF sy-subrc = 0.
        MESSAGE e999 WITH 'Please use RE_PROCESS function'.
      ENDIF.
    ENDAT.
  ENDLOOP.

  w_update_flag = 'X'.
  PERFORM save_simulation.
  PERFORM save_mod_result.
*  SORT it_itab BY pum_n matnr excfdate exctdate.

  LOOP AT it_reval_selected.
    CLEAR: w_index, w_index_rev, w_index_sa, w_update_flag, it_reval.
    w_index_rev = sy-tabix.

    MESSAGE s999 WITH 'Processing' it_reval_selected-matnr
      ' ' it_reval_selected-pum_n.

    CLEAR: it_itab, l_gramt, l_diff.
    READ TABLE it_itab WITH KEY  pum_n = it_reval_selected-pum_n
                                 matnr = it_reval_selected-matnr
                                 excfdate = it_reval_selected-datab
                                 exctdate = it_reval_selected-datbi.
*                                BINARY SEARCH.
    w_index_sa = sy-tabix.
    w_index = w_index_sa.
    CLEAR: l_error, w_multi_sa.
    w_multi_sa = it_itab-multi_sa.

    IF w_multi_sa > 1.
      LOOP AT it_itab FROM w_index_sa
                         WHERE pum_n = it_reval_selected-pum_n
                           AND matnr = it_reval_selected-matnr
                           AND excfdate = it_reval_selected-datab
                           AND exctdate = it_reval_selected-datbi.
        w_index = sy-tabix.
        PERFORM update_sa USING l_error.
*        MODIFY it_itab index w_index TRANSPORTING sa_update msg.
        IF l_error IS INITIAL.
          l_gramt = l_gramt + it_itab-gramt.
          l_diff = l_diff + it_itab-diff.
        ELSE.
          EXIT.
        ENDIF.
      ENDLOOP.
      w_index = w_index_sa.
      READ TABLE it_itab INDEX  w_index.
    ELSE.
      PERFORM update_sa USING l_error.
*      MODIFY it_itab INDEX w_index TRANSPORTING sa_update msg.
      l_gramt = it_itab-gramt.
      l_diff = it_itab-diff.
    ENDIF.
    IF l_error IS INITIAL.
      IF l_diff IS NOT INITIAL.
*      IF l_gramt IS NOT INITIAL.
        PERFORM de_aggregate_onestep USING l_error.
        IF l_error IS INITIAL.
          PERFORM re_eval_auto.
        ENDIF.
      ELSE.
** No INV
        READ TABLE it_reval WITH KEY pum_n = it_reval_selected-pum_n
                        matnr = it_reval_selected-matnr
                        lifnr = it_reval_selected-lifnr
                        datab = it_reval_selected-datab
                        datbi = it_reval_selected-datbi.
        l_index = sy-tabix.

        it_reval-reval_update = 'C'.
        it_reval-reval_date = sy-datum.
        it_reval-reval_user = sy-uname.
        it_reval-zmsg = 'No Invoice found'.

        MODIFY it_reval INDEX l_index TRANSPORTING reval_update
        reval_date reval_user zmsg.

        it_itab-reval_update = 'C'.
        it_itab-msg = it_reval-zmsg.
        PERFORM update_itab.

        PERFORM update_mod_log USING 'C'.
      ENDIF.
    ELSE.
      MESSAGE s999 WITH 'Error in updating SA' it_pri-matnr
         DISPLAY LIKE 'E'.
    ENDIF.
  ENDLOOP.

  PERFORM unlock_prog.

  IF it_error[] IS NOT INITIAL AND p_rver IS NOT INITIAL.
    PERFORM send_email.
  ENDIF.
  CALL SCREEN 210.
ENDFORM.                    " REVALUATION
*&--------------------------------------------------------------------*
*&      Form  RE_EVAL_AUTO
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM re_eval_auto.
  DATA: BEGIN OF lt_ekbe OCCURS 0,
    belnr LIKE ekbe-belnr,
    cpudt LIKE ekbe-cpudt,
    cputm LIKE ekbe-cputm,
*    DMBTR LIKE EKBE-DMBTR,
    END OF lt_ekbe.

  DATA: l_xrech LIKE rbkp-xrech,
        l_flag(1),
        l_index LIKE sy-tabix,
        l_diff LIKE it_itab-diff,
        l_cn TYPE i,
        l_belnr LIKE lt_ekbe-belnr.

  DATA: l_date_from(10),
        l_date_to(10),
         l_post_date(10),
         l_mess LIKE cfgnl-msglin.

  RANGES: lr_wedat FOR sy-datum.

  READ TABLE it_reval WITH KEY pum_n = it_reval_selected-pum_n
                        matnr = it_reval_selected-matnr
                        lifnr = it_reval_selected-lifnr
                        datab = it_reval_selected-datab
                        datbi = it_reval_selected-datbi.
  l_index = sy-tabix.

  IF it_reval-sa_update IS INITIAL.
    MESSAGE e000(zz) WITH text-m17.
  ENDIF.

  IF it_reval-reval_update IS NOT INITIAL.
    IF it_reval-reval_update = 'C'.
      MESSAGE s000(zz) WITH text-m18.
      EXIT.
    ELSE.
      MESSAGE s000(zz) WITH text-m14.
      EXIT.
    ENDIF.
  ENDIF.

  REFRESH it_bdc.
  WRITE: it_itab-excfdate TO l_date_from,
         it_itab-exctdate TO l_date_to,
         p_pdate TO l_post_date.

  PERFORM dynpro USING:
        'X' 'RMMR1MRB' '1000',
        ' ' 'BDC_CURSOR' 'PA_GUZTE',
        ' ' 'BDC_OKCODE' 'ONLI',
        ' ' 'PA_EBELN' it_itab-ebeln,
        ' ' 'PA_EBELP' it_itab-ebelp,
        ' ' 'SO_WEDAT-LOW' l_date_from,
        ' ' 'SO_WEDAT-HIGH' l_date_to,
        ' ' 'PA_BUDAT'  'X',
        ' ' 'PA_NBWDT'  l_post_date,
        ' ' 'PA_ZTERM'  'P030',
        ' ' 'PA_GUZTE'  'P030',
        ' ' 'PA_XTEST' ' '.

*  PERFORM dynpro USING:
*         'X' 'SAPMSSY0' '0120',
*         ' ' 'BDC_CURSOR' '21/03',
*         ' ' 'BDC_OKCODE' '=EF3'.
*
*  PERFORM dynpro USING:
*        'X' 'RMMR1MRB' '1000',
*        ' ' 'BDC_OKCODE' '/EE',
*        ' ' 'BDC_CURSOR' 'PA_EBELN'.


  CALL TRANSACTION 'MRNB'   USING it_bdc
                               MODE w_mode
                               UPDATE 'S'
                               MESSAGES INTO it_mess.
  IF sy-subrc <> 0.
    IF sy-msgty = 'S' AND sy-msgno = '344'.
      it_reval-reval_update = 'C'.
      it_reval-reval_date = sy-datum.
      it_reval-reval_user = sy-uname.
      it_reval-zmsg = 'No Price Different'.

      it_itab-reval_update = 'C'.
      it_itab-msg = 'No Price Different'.

      MODIFY it_reval INDEX l_index TRANSPORTING reval_update
      reval_date reval_user zmsg.
      PERFORM update_mod_log USING 'C'.
    ELSE.
      CLEAR: l_mess.
      CALL FUNCTION 'RKC_MSG_STRING'
        EXPORTING
          id      = sy-msgid
          mtype   = sy-msgty
          number  = sy-msgno
          par1    = sy-msgv1
          par2    = sy-msgv2
          par3    = sy-msgv3
          par4    = sy-msgv4
        IMPORTING
          msg_lin = l_mess.

      MESSAGE s000(zz) WITH l_mess
               it_itab-ebeln it_itab-excfdate it_itab-exctdate.

      it_reval-reval_update = ' '.
      it_reval-reval_date = sy-datum.
      it_reval-reval_user = sy-uname.
      it_reval-zmsg = l_mess.

      MOVE-CORRESPONDING it_reval TO it_error.
      APPEND it_error.

      it_itab-reval_update = ' '.
      it_itab-msg = 'Error in re_val'.

      MODIFY it_reval INDEX l_index TRANSPORTING reval_update
          reval_date  reval_user.
      PERFORM update_mod_log USING 'E'.
    ENDIF.
    PERFORM update_itab.
    EXIT.
  ENDIF.

  DO 10 TIMES.
    CLEAR: lt_ekbe, lt_ekbe[].
    SELECT belnr cpudt cputm INTO TABLE lt_ekbe
         FROM ekbe
         WHERE ebeln = it_itab-ebeln
           AND ebelp = it_itab-ebelp
*             AND BUDAT = P_DATE
           AND cpudt = sy-datum
           AND vgabe = '3'
           AND bewtp = 'W'
           AND bwart = '  '
           AND ernam = sy-uname.
    IF sy-subrc = 0.
      EXIT.
    ELSE.
      WAIT UP TO '3' SECONDS.
    ENDIF.
  ENDDO.

  SORT lt_ekbe DESCENDING BY belnr cpudt cputm.
  READ TABLE lt_ekbe INDEX 1.
*      it_reval_rsn-belnr = lt_ekbe-belnr.

  IF sy-subrc = 0.
    it_reval-reval_update = 'X'.
    it_reval-reval_date = sy-datum.
    it_reval-reval_user = sy-uname.
    it_reval-belnr = lt_ekbe-belnr.

    CLEAR: l_xrech.
    SELECT SINGLE rmwwr xrech
      INTO (it_reval-inv_amt, l_xrech)
        FROM rbkp
      WHERE belnr = it_reval-belnr
        AND gjahr = p_pdate+0(4).

    IF l_xrech IS INITIAL.
      it_reval-inv_amt = it_reval-inv_amt * -1.
    ENDIF.

    LOOP AT it_itab FROM w_index
                                WHERE pum_n = it_reval_selected-pum_n
                                  AND matnr = it_reval_selected-matnr
                                  AND excfdate = it_reval_selected-datab
                                  AND exctdate = it_reval_selected-datbi
                                  .
      it_reval-inv_qty =     it_reval-inv_qty + it_itab-grqty.
    ENDLOOP.

    it_itab-reval_update = 'X'.
    CONCATENATE 'Doc No:' lt_ekbe-belnr INTO it_itab-msg
       SEPARATED BY space.

    MODIFY it_reval INDEX l_index TRANSPORTING reval_update
    reval_date reval_user belnr inv_amt inv_qty.
    PERFORM update_mod_log USING 'X'.
  ELSE.
    it_reval-reval_update = ' '.
    it_reval-reval_date = sy-datum.
    it_reval-reval_user = sy-uname.
    it_reval-zmsg = 'Error in re_val'.

    MOVE-CORRESPONDING it_reval TO it_error.
    APPEND it_error.

    it_itab-reval_update = ' '.
    it_itab-msg = 'Error in re_val - No acocunt doc'.

    MODIFY it_reval INDEX l_index TRANSPORTING reval_update
        reval_date reval_user.
    PERFORM update_mod_log USING 'E'.
  ENDIF.
  PERFORM update_itab.

*** Second for Debit

  REFRESH: it_bdc, it_mess.
  CLEAR: l_xrech.
  WRITE: it_itab-excfdate TO l_date_from,
         it_itab-exctdate TO l_date_to,
         p_pdate TO l_post_date.

  PERFORM dynpro USING:
        'X' 'RMMR1MRB' '1000',
        ' ' 'BDC_CURSOR' 'PA_GUZTE',
        ' ' 'BDC_OKCODE' 'ONLI',
        ' ' 'PA_EBELN' it_itab-ebeln,
        ' ' 'PA_EBELP' it_itab-ebelp,
        ' ' 'SO_WEDAT-LOW' l_date_from,
        ' ' 'SO_WEDAT-HIGH' l_date_to,
        ' ' 'PA_BUDAT'  'X',
        ' ' 'PA_NBWDT'  l_post_date,
        ' ' 'PA_ZTERM'  'P030',
        ' ' 'PA_GUZTE'  'P030',
        ' ' 'PA_XTEST' ' '.

  CALL TRANSACTION 'MRNB'   USING it_bdc
                               MODE w_mode
                               UPDATE 'S'
                               MESSAGES INTO it_mess.
  IF sy-subrc <> 0.
    IF sy-msgty = 'S' AND sy-msgno = '344'.
    ELSE.
      CLEAR: l_mess.
      CALL FUNCTION 'RKC_MSG_STRING'
        EXPORTING
          id      = sy-msgid
          mtype   = sy-msgty
          number  = sy-msgno
          par1    = sy-msgv1
          par2    = sy-msgv2
          par3    = sy-msgv3
          par4    = sy-msgv4
        IMPORTING
          msg_lin = l_mess.

      MESSAGE s000(zz) WITH l_mess
               it_itab-ebeln it_itab-excfdate it_itab-exctdate.

      it_reval-reval_update = ' '.
      it_reval-reval_date = sy-datum.
      it_reval-reval_user = sy-uname.
      it_reval-zmsg2 = l_mess.

      MOVE-CORRESPONDING it_reval TO it_error.
      APPEND it_error.

      it_itab-reval_update = ' '.
      it_itab-msg = 'Error in re_val'.

      MODIFY it_reval INDEX l_index TRANSPORTING reval_update
          reval_date  reval_user.
      PERFORM update_mod_log_2 USING 'E'.
    ENDIF.
    PERFORM update_itab.
    EXIT.
  ENDIF.

  DO 10 TIMES.
    CLEAR: lt_ekbe, lt_ekbe[].
    SELECT belnr cpudt cputm INTO TABLE lt_ekbe
         FROM ekbe
         WHERE ebeln = it_itab-ebeln
           AND ebelp = it_itab-ebelp
*             AND BUDAT = P_DATE
           AND cpudt = sy-datum
           AND vgabe = '3'
           AND bewtp = 'W'
           AND bwart = '  '
           AND ernam = sy-uname.
    IF sy-subrc = 0.
      EXIT.
    ELSE.
      WAIT UP TO '3' SECONDS.
    ENDIF.
  ENDDO.

  SORT lt_ekbe DESCENDING BY belnr cpudt cputm.
  READ TABLE lt_ekbe INDEX 1.
*      it_reval_rsn-belnr = lt_ekbe-belnr.

  IF sy-subrc = 0.
    it_reval-reval_update = 'X'.
    it_reval-reval_date = sy-datum.
    it_reval-reval_user = sy-uname.
    it_reval-belnr2 = lt_ekbe-belnr.

    CLEAR: l_xrech.
    SELECT SINGLE rmwwr xrech
      INTO (it_reval-inv_amt2, l_xrech)
        FROM rbkp
      WHERE belnr = it_reval-belnr2
        AND gjahr = p_pdate+0(4).

    IF l_xrech IS INITIAL.
      it_reval-inv_amt2 = it_reval-inv_amt2 * -1.
    ENDIF.

*    LOOP AT it_itab FROM w_index
*                                WHERE pum_n = it_reval_selected-pum_n
*                                  AND matnr = it_reval_selected-matnr
*                                  AND excfdate =
*it_reval_selected-datab
*                                  AND exctdate =
*it_reval_selected-datbi
*                                  .
*      it_reval-inv_qty =     it_reval-inv_qty + it_itab-grqty.
*    ENDLOOP.

    it_itab-reval_update = 'X'.
    CONCATENATE it_itab-msg lt_ekbe-belnr INTO it_itab-msg
       SEPARATED BY space.

    MODIFY it_reval INDEX l_index TRANSPORTING reval_update
    reval_date reval_user belnr2 inv_amt2 inv_qty.
    PERFORM update_mod_log_2 USING 'X'.
  ELSE.
    it_reval-reval_update = ' '.
    it_reval-reval_date = sy-datum.
    it_reval-reval_user = sy-uname.
    it_reval-zmsg2 = 'Error in re_val'.

    MOVE-CORRESPONDING it_reval TO it_error.
    APPEND it_error.

    it_itab-reval_update = ' '.
    it_itab-msg = 'Error in re_val - No acocunt doc'.

    MODIFY it_reval INDEX l_index TRANSPORTING reval_update
        reval_date reval_user.
    PERFORM update_mod_log_2 USING 'E'.
  ENDIF.
  PERFORM update_itab.
ENDFORM.                    " RE_EVAL_AUTO
*&--------------------------------------------------------------------*
*&      Form  CALL_TCODE_ZRFIG02
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM call_tcode_zrfig02 USING p_bukrs p_year.

  DATA: l_data_fr(8),
        l_data_to(8),
        l_cn(2) TYPE n,
        l_text(60),
        l_lines TYPE i.

  REFRESH:  bdcdata.
  PERFORM bdc_dynpro      USING 'ZRFIG02' '1000'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'P_OWN'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=%015'.
  PERFORM bdc_field       USING 'P_BUKRS'
                                p_bukrs.
  PERFORM bdc_field       USING 'S_GJAHR-LOW'
                                p_year.
  PERFORM bdc_field       USING 'S_CPUDT-LOW'
                                l_data_fr.
  PERFORM bdc_field       USING 'S_CPUDT-HIGH'
                                l_data_to.
  PERFORM bdc_field       USING 'P_OWN'
                                ' '.

  DESCRIBE TABLE r_xblnr LINES l_lines.
  IF l_lines > 8.
    PERFORM bdc_dynpro      USING 'SAPLALDB' '3000'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=P+'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RSCSEL_255-SLOW_I(02)'.
    l_cn = '01'.
    LOOP AT r_xblnr.
      IF l_cn > 8.
        PERFORM bdc_dynpro      USING 'SAPLALDB' '3000'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                      '/00'.
        PERFORM bdc_field       USING 'BDC_CURSOR'
                                      'RSCSEL_255-SLOW_I(02)'.
        l_cn = '02'.
      ENDIF.

      CONCATENATE 'RSCSEL_255-SLOW_I(' l_cn ')' INTO l_text.

      PERFORM bdc_field       USING l_text
                                  r_xblnr-low.
      l_cn = l_cn + 1.
    ENDLOOP.
    PERFORM bdc_dynpro      USING 'SAPLALDB' '3000'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=ACPT'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RSCSEL_255-SLOW_I(02)'.
  ELSE.
    PERFORM bdc_dynpro      USING 'SAPLALDB' '3000'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=ACPT'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RSCSEL_255-SLOW_I(01)'.
    l_cn = '01'.
    LOOP AT r_xblnr.
      CONCATENATE 'RSCSEL_255-SLOW_I(' l_cn ')' INTO l_text.
      PERFORM bdc_field     USING l_text
                                  r_xblnr-low.
      l_cn = l_cn + 1.
    ENDLOOP.
  ENDIF.

  PERFORM bdc_dynpro      USING 'ZRFIG02' '1000'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'S_CPUDT-HIGH'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=ONLI'.

  PERFORM bdc_dynpro      USING 'SAPMSSY0' '0130'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=%EX'.
  PERFORM bdc_transaction USING 'ZRFIG02'.
ENDFORM.                    " CALL_TCODE_ZRFIG02
*&      Form  BDC_DYNPRO
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->P_1580   text
*      -->P_1581   text
*---------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.                    "BDC_FIELD
*---------------------------------------------------------------------*
*        Start new screen                                             *
*---------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.                    "BDC_DYNPRO
*&------------------------------------------------------------------
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->P_6558   text
*---------------------------------------------------------------------*

FORM bdc_transaction USING p_tcode.
  DATA: l_subrc LIKE sy-subrc,
        msg(255),
        l_index LIKE sy-tabix.
  DATA opt TYPE ctu_params.
  opt-dismode = 'E'.

  CALL TRANSACTION p_tcode USING bdcdata
        OPTIONS FROM opt.
*                   MODE   ctumode
*                   UPDATE cupdate.
  REFRESH: bdcdata.
ENDFORM.                    " BDC_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  SET_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_screen .
  LOOP AT SCREEN.
    IF screen-name = 'P_SUB'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " SET_SCREEN
*&---------------------------------------------------------------------*
*&      Form  HANDLE_BUTTON_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ES_COL_ID  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM handle_button_click  USING  e_column  TYPE lvc_s_col
                                 es_row_no TYPE lvc_s_roid.

  READ TABLE it_pri INDEX es_row_no-row_id.
  CHECK sy-subrc = 0.

  CASE e_column-fieldname.
    WHEN 'MOD_ICON'.
      PERFORM get_mod_inforprcie.
      CALL SCREEN 0110.
  ENDCASE.
ENDFORM.                    " HANDLE_BUTTON_CLICK
*&---------------------------------------------------------------------*
*&      Form  GET_MOD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_mod_inforprcie.
  DATA: l_index LIKE sy-tabix.
  DATA ls_style TYPE lvc_s_styl.

  REFRESH: it_mod_info.
  PERFORM clear_mod_info.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_mod_info
      FROM ztmm_mod_type
      WHERE pum_n = it_pri-pum_n
      AND comp = it_pri-matnr.
  IF sy-subrc = 0.
    LOOP AT it_mod_info.
      ls_style-fieldname = 'MATNR'.
      ls_style-style = cl_gui_alv_grid=>mc_style_hotspot.
      APPEND ls_style TO it_mod_info-celltab.
      MODIFY it_mod_info.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " GET_MOD_DATA
*&--------------------------------------------------------------------*
*&      Module  STATUS_0110  OUTPUT
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
MODULE status_0110 OUTPUT.
  SET PF-STATUS 'ST110'.
  SET TITLEBAR 'T110'.
ENDMODULE. " STATUS_0110  OUTPUT
*&--------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_0110  OUTPUT
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
MODULE display_alv_0110 OUTPUT.
  IF g_docking_container_110 IS INITIAL.
    PERFORM create_container_n_object_110.
    PERFORM set_attributes_alv_grid_110.
    PERFORM build_field_catalog_110 USING 'IT_MOD_INFO'.
    PERFORM assign_itab_to_alv_110.
*    PERFORM sssign_event_9000.
  ELSE.
    CALL METHOD alv_grid_110->refresh_table_display.
  ENDIF.
ENDMODULE.                 " DISPLAY_ALV_0110  OUTPUT
*&--------------------------------------------------------------------*
*&      Module  USER_COMMAND_0110  INPUT
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
MODULE user_command_0110 INPUT.
  w_code = ok_code_110.
  CASE w_code.
    WHEN 'EXIT'.
      PERFORM clear_mod_info.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      PERFORM clear_mod_info.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0110  INPUT
*&--------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_N_OBJECT_110
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM create_container_n_object_110 .
  DATA:   l_repid LIKE sy-repid,
          l_dynnr LIKE sy-dynnr.

  l_repid = sy-repid.
  l_dynnr = sy-dynnr.
  CREATE OBJECT g_docking_container_110
    EXPORTING
      repid     = l_repid
      dynnr     = l_dynnr
      side      = cl_gui_docking_container=>dock_at_bottom
*     RATIO     = 90
      extension = 2000.

  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = w_repid
        txt2  = sy-subrc
        txt1  = 'The control can not be created'.
  ENDIF.
  CREATE OBJECT alv_grid_110
    EXPORTING
      i_parent      = g_docking_container_110
      i_appl_events = 'X'.


  CREATE OBJECT g_event_receiver_110.
  SET HANDLER
  g_event_receiver_110->handle_hotspot_click FOR alv_grid_110.

ENDFORM.                    " CREATE_CONTAINER_N_OBJECT_110
*&--------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_GRID_110
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM set_attributes_alv_grid_110 .
  DATA : lw_s_dragdrop TYPE lvc_s_dd01. "/ Drag&Drop control settings

  CLEAR : wa_is_layout_110, wa_variant_110.

*//-- Set Layout Structure
  wa_is_layout_110-edit       = ' '.      "/Edit Mode Enable
*  wa_is_layout_110-sel_mode   = 'A'.      "/mode for select col and row
  wa_is_layout_110-language   = sy-langu. "/Language Key
*  wa_is_layout_110-cwidth_opt = 'X'.   "/optimizes the column width
  wa_is_layout_110-info_fname = 'IF'.
  wa_is_layout_110-ctab_fname = 'CT'.
  wa_is_layout_110-stylefname = 'CELLTAB'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

*//-- Set Variant Structure
  wa_variant_110-report       = sy-repid.
  wa_variant_110-username     = sy-uname.
ENDFORM.                    " SET_ATTRIBUTES_ALV_GRID_110
*&--------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG_110
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->P_7855   text
*---------------------------------------------------------------------*
FORM build_field_catalog_110  USING p_itab.
  DATA: lw_itab TYPE slis_tabname,
         lw_waers LIKE t001-waers,
         l_rqty(9),
         l_datum(8),
         l_cn(2) TYPE n,
         l_mcode_seq(30).

  CLEAR: it_fieldcat_110,  it_fieldcat_110[],
         it_fieldname_110, it_fieldname_110[].
  CLEAR: w_cnt,w_repid.

  lw_itab = p_itab.

  w_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = w_repid
      i_internal_tabname = lw_itab
      i_inclname         = w_repid
    CHANGING
      ct_fieldcat        = it_fieldname_110.

  PERFORM setting_fieldcat_110 TABLES it_fieldcat_110 USING :

                                  'S' 'VTYPE'       ' ',
                                  ' ' 'COLTEXT'     'Type',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'MCODE'       ' ',
                                  ' ' 'COLTEXT'     'Mode Code',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'MATNR'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Material',
                                  ' ' 'HOTSPOT'     'X',
                                  'E' 'OUTPUTLEN'   '18'.
*
*                                 'S' 'LIFNR'       ' ',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'COLTEXT'     'Vendor',
*                                  'E' 'OUTPUTLEN'   '10'.

ENDFORM.                    " BUILD_FIELD_CATALOG_110
*&--------------------------------------------------------------------*
*&      Form  SETTING_FIELDCAT_110
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->P_IT_FIELDCAT_110  text
*      -->P_8105   text
*      -->P_8106   text
*      -->P_8107   text
*---------------------------------------------------------------------*
FORM setting_fieldcat_110 TABLES p_fieldcat STRUCTURE it_fieldcat_110
                      USING    p_gubun
                               p_field
                               p_value.
  DATA : l_col(40).

  FIELD-SYMBOLS <fs>.

* START - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'S'.
    CLEAR: p_fieldcat.

    READ TABLE it_fieldname_110 INTO w_fieldname_110
                            WITH KEY fieldname  = p_field.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH 'Check field catalog'.
    ENDIF.

    MOVE: w_fieldname_110-fieldname TO p_fieldcat-fieldname.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' p_field  INTO l_col.
  ASSIGN (l_col) TO <fs>.
  MOVE   p_value TO <fs>.

* END - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'E'.
    ADD 1 TO w_cnt.
    p_fieldcat-col_pos = w_cnt.
    APPEND p_fieldcat.
  ENDIF.
ENDFORM.                    " SETTING_FIELDCAT_110
*&--------------------------------------------------------------------*
*&      Form  ASSIGN_ITAB_TO_ALV_110
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM assign_itab_to_alv_110 .
  CALL METHOD alv_grid_110->set_table_for_first_display
    EXPORTING
      is_layout       = wa_is_layout
*     i_save          = wa_save
*     is_variant      = wa_variant_110
    CHANGING
      it_fieldcatalog = it_fieldcat_110[]
      it_outtab       = it_mod_info[].
ENDFORM.                    " ASSIGN_ITAB_TO_ALV_110
*&--------------------------------------------------------------------*
*&      Form  HOTSPOT_CLICK
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->P_E_ROW_ID  text
*      -->P_E_COLUMN_ID  text
*      -->P_ES_ROW_NO  text
*---------------------------------------------------------------------*
FORM hotspot_click  USING p_e_row_id  TYPE  lvc_s_row
                          p_e_column_id TYPE  lvc_s_col
                          p_es_row_no	TYPE lvc_s_roid.

  CASE p_e_column_id-fieldname.
    WHEN 'MATNR'.
      READ TABLE it_mod_info INDEX p_es_row_no-row_id.
      CHECK sy-subrc = 0.
      SUBMIT zemmpm45r_dis_mod_prc_ebom_ztb
          WITH p_mcode = it_mod_info-mcode
          WITH p_matnr = it_mod_info-matnr
          AND RETURN.
    WHEN 'BELNR'.
      READ TABLE it_reval INDEX p_es_row_no-row_id.
      CHECK sy-subrc = 0 AND it_reval-belnr IS NOT INITIAL.
      SET PARAMETER ID 'RBN' FIELD it_reval-belnr.
      SET PARAMETER ID 'GJR' FIELD it_reval-reval_date+0(4).

      CALL TRANSACTION  'MIR4' AND SKIP FIRST SCREEN.
    WHEN 'BELNR2'.
      READ TABLE it_reval INDEX p_es_row_no-row_id.
      CHECK sy-subrc = 0 AND it_reval-belnr IS NOT INITIAL.
      SET PARAMETER ID 'RBN' FIELD it_reval-belnr2.
      SET PARAMETER ID 'GJR' FIELD it_reval-reval_date+0(4).

      CALL TRANSACTION  'MIR4' AND SKIP FIRST SCREEN.
  ENDCASE.
ENDFORM.                    " HOTSPOT_CLICK
*&--------------------------------------------------------------------*
*&      Form  CLEAR_MOD_INFO
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM clear_mod_info .
  LOOP AT it_mod_info.
    REFRESH it_mod_info-celltab.
    MODIFY it_mod_info.
  ENDLOOP.
  REFRESH it_mod_info.

ENDFORM.                    " CLEAR_MOD_INFO
*---------------------------------------------------------------------*
*  MODULE display_alv_0200 OUTPUT
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
MODULE display_alv_0200 OUTPUT.
  IF g_docking_container IS INITIAL.
    PERFORM create_container_n_object.
    PERFORM set_attributes_alv_grid.
    PERFORM exclude_ui_function.
    PERFORM build_sortcat_display.
    PERFORM build_field_catalog USING 'IT_ITAB'.
    PERFORM assign_itab_to_alv.
  ELSE.
    CALL METHOD alv_grid->refresh_table_display.
  ENDIF.
ENDMODULE.                    "display_alv_0200 OUTPUT
*&--------------------------------------------------------------------*
*&      Form  simulation
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM simulation.
  PERFORM get_selected_data.
  PERFORM check_valid_data.
  CHECK it_pri_selected[] IS NOT INITIAL.
  PERFORM get_module_data.
  PERFORM get_scheduling_agreement.
  PERFORM make_data.
  CALL SCREEN 200.
ENDFORM.                    "simulation
*&--------------------------------------------------------------------*
*&      Form  GET_MODULE_DATA
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM get_module_data.
  DATA: l_date TYPE sy-datum,
        l_pre_cal_date TYPE sy-datum,
        l_index LIKE sy-tabix,
          l_index_1 LIKE sy-tabix,
        l_index_comp LIKE sy-tabix,
            l_delete(1),
             l_new(1).

  DATA: BEGIN OF lt_reval_temp OCCURS 0.
          INCLUDE STRUCTURE ztmm_mod_rev_res.
  DATA: END OF lt_reval_temp.

  DATA: BEGIN OF lt_date OCCURS 0,
        module LIKE ztmm_mod_pri_bk-matnr,
        date LIKE sy-datum,
        type(10),
        END OF lt_date.

  DATA: lt_date_makeup LIKE TABLE OF lt_date WITH HEADER LINE.

  DATA: BEGIN OF lt_date_r OCCURS 0,
         module LIKE ztmm_mod_pri_bk-matnr,
         datab LIKE sy-datum,
         datbi LIKE sy-datum,
         END OF lt_date_r.

  DATA: BEGIN OF lt_mod OCCURS 0.
          INCLUDE STRUCTURE ztmm_mod_pri_bk.
  DATA: cal_date TYPE sy-datum,
        END OF lt_mod.

  DATA: lw_date LIKE lt_date,
       l_pre_end TYPE sy-datum.

  CONSTANTS: c_start(10) VALUE 'Start',
             c_end(10) VALUE 'End'.

  REFRESH: it_mod_pri, it_reval_base.
  SELECT * INTO TABLE it_mod_pri
    FROM ztmm_mod_pri_bk
    FOR ALL ENTRIES IN it_pri_selected
    WHERE comp = it_pri_selected-matnr
      AND zresult = 'S'.

** Ingore compont vendore code
  LOOP AT it_mod_pri.
    CLEAR: it_mod_pri-lifnr.
    MODIFY it_mod_pri.
  ENDLOOP.

** making todate step 2
  SORT it_mod_pri BY comp matnr input_date DESCENDING.
  LOOP AT it_mod_pri.
    MOVE-CORRESPONDING it_mod_pri TO lt_mod.

    AT NEW matnr.
      l_date = '99991231'.
    ENDAT.
    IF it_mod_pri-input_date <= l_date.
      lt_mod-cal_date = l_date.
    ELSE.
      lt_mod-cal_date = l_pre_cal_date.
    ENDIF.
    l_date = it_mod_pri-input_date - 1.
    l_pre_cal_date = lt_mod-cal_date.
    APPEND lt_mod.
  ENDLOOP.

** Step 3  delete those not in seelcted comp range
  SORT lt_mod BY comp matnr input_date.
  DELETE ADJACENT DUPLICATES FROM lt_mod.

  SORT it_pri_selected BY matnr.
  LOOP AT lt_mod.
    l_index = sy-tabix.
    READ TABLE it_pri_selected WITH KEY matnr = lt_mod-comp
         BINARY SEARCH.
    l_index_comp = sy-tabix.
    CLEAR: l_delete.
    LOOP AT it_pri_selected FROM l_index_comp
                     WHERE matnr = lt_mod-comp.
      IF it_pri_selected-term_to < lt_mod-input_date OR
         it_pri_selected-term_from > lt_mod-cal_date.
        l_delete = 'X'.
      ELSE.
        CLEAR: l_delete.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF l_delete = 'X'.
      DELETE lt_mod INDEX l_index.
    ENDIF.
  ENDLOOP.

  REFRESH it_data.
  LOOP AT it_pri_selected.
    READ TABLE lt_mod WITH KEY comp = it_pri_selected-matnr
         BINARY SEARCH.
    l_index_comp = sy-tabix.
    LOOP AT lt_mod FROM l_index_comp
                     WHERE comp = it_pri_selected-matnr.
      MOVE-CORRESPONDING lt_mod TO it_data.
      IF it_pri_selected-term_from >= lt_mod-input_date " between
           AND it_pri_selected-term_to =< lt_mod-cal_date.
        it_data-datab = it_pri_selected-term_from.
        it_data-datbi = it_pri_selected-term_to.
      ELSEIF it_pri_selected-term_from >= lt_mod-input_date " partial
            AND it_pri_selected-term_to > lt_mod-cal_date.
        it_data-datab = it_pri_selected-term_from.
        it_data-datbi = lt_mod-cal_date.
      ELSEIF it_pri_selected-term_from =< lt_mod-input_date " partial
            AND it_pri_selected-term_to =< lt_mod-cal_date
            AND it_pri_selected-term_to >= lt_mod-input_date.
        it_data-datab = lt_mod-input_date.
        it_data-datbi = it_pri_selected-term_to.
      ELSE.
        it_data-datab = lt_mod-input_date.
        it_data-datbi = lt_mod-cal_date.
      ENDIF.
      it_data-qty_per = lt_mod-qnty.
      it_data-price_diff = it_pri_selected-term_price *
                             it_data-qty_per.
      it_data-pum_n =  it_pri_selected-pum_n.
*      it_data-ztseq =  it_pri_selected-ztseq.
      APPEND it_data.
    ENDLOOP.
  ENDLOOP.

** Creating date
  SORT it_data BY comp matnr datab.
  LOOP AT it_data.
    lt_date-module = it_data-matnr.
    lt_date-date = it_data-datab.
    lt_date-type = c_start.
    COLLECT lt_date.

    lt_date-module = it_data-matnr.
    lt_date-date = it_data-datbi.
    lt_date-type = c_end.
    COLLECT lt_date.
  ENDLOOP.

  SORT lt_date BY module date type DESCENDING.

  LOOP AT lt_date.
    CLEAR: lt_date_makeup.
    AT NEW module.
      l_new = 'X'.
    ENDAT.

    IF l_new = 'X'.
      IF lt_date-type = c_start.
        lt_date_makeup = lt_date.
        APPEND lt_date_makeup.
        CLEAR l_new.
      ELSE.
        lt_date_makeup-date = l_pre_end + 1.
        lt_date_makeup-module = lt_date-module.
        lt_date_makeup-type = c_start.
        APPEND lt_date_makeup.

        lt_date_makeup = lt_date.
        APPEND lt_date_makeup.
        l_pre_end = lt_date-date.
        l_new = 'X'.
      ENDIF.

    ELSE.
      IF lt_date-type = c_end.
        lt_date_makeup = lt_date.
        APPEND lt_date_makeup.
        l_pre_end = lt_date-date.
        l_new = 'X'.
      ELSE.
** add one more end date (make 2 recoreds)
        lt_date_makeup-module = lt_date-module.
        lt_date_makeup-date =  lt_date-date - 1.
        lt_date_makeup-type = c_end.
        APPEND lt_date_makeup.

        lt_date_makeup = lt_date.
        APPEND lt_date_makeup.
        CLEAR l_new.
      ENDIF.
    ENDIF.
    AT END OF module.
      IF l_new IS INITIAL.
        lt_date_makeup-type = c_end.
        APPEND lt_date_makeup.
      ENDIF.
    ENDAT.
  ENDLOOP.

  LOOP AT lt_date_makeup.
    IF lt_date_makeup-type = c_start.
      lt_date_r-module = lt_date_makeup-module.
      lt_date_r-datab = lt_date_makeup-date.
    ELSE.
      lt_date_r-datbi = lt_date_makeup-date.
      APPEND lt_date_r.
    ENDIF.
  ENDLOOP.

** Making lt_reval_base
  SORT lt_date_r BY module datab datbi.
** Furong On 09/03/14 (
  DELETE lt_date_r WHERE datbi = '99991231'.
** )
  LOOP AT it_data.
    MOVE-CORRESPONDING it_data TO it_reval_base.
    READ TABLE lt_date_r WITH KEY module = it_data-matnr
         datab = it_data-datab.
    l_index = sy-tabix.

    LOOP AT lt_date_r FROM l_index
                      WHERE module = it_data-matnr
                        AND datbi <= it_data-datbi.
      it_reval_base-datab = lt_date_r-datab.
      it_reval_base-datbi = lt_date_r-datbi.
      APPEND it_reval_base.
    ENDLOOP.
    CLEAR: it_reval_base.
  ENDLOOP.

** create it_reval
*  sort lt_reval_base by matnr datab.
  REFRESH it_reval.
  LOOP AT it_reval_base.
    MOVE-CORRESPONDING it_reval_base TO lt_reval_temp.

    CLEAR: lt_reval_temp-zresult, lt_reval_temp-zmsg.
    COLLECT lt_reval_temp.
  ENDLOOP.

  LOOP AT lt_reval_temp.
    MOVE-CORRESPONDING lt_reval_temp TO it_reval.
    APPEND it_reval.
  ENDLOOP.
ENDFORM.                    " GET_MODULE_DATA
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_GRID_100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_grid_100 .

  DATA : lw_s_dragdrop TYPE lvc_s_dd01. "/ Drag&Drop control settings

  CLEAR : wa_is_layout_100, wa_variant_100.

*//-- Set Layout Structure
  wa_is_layout_100-edit       = ' '.      "/Edit Mode Enable
  wa_is_layout_100-sel_mode   = 'A'.      "/mode for select col and row
  wa_is_layout_100-language   = sy-langu. "/Language Key
  wa_is_layout_100-cwidth_opt = 'X'.   "/optimizes the column width
*  wa_is_layout-info_fname = 'IF'.
  wa_is_layout_100-ctab_fname = 'CT'.
  wa_is_layout_100-stylefname = 'CELLTAB'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

*//-- Set Variant Structure
  wa_variant_100-report       = sy-repid.
  wa_variant_100-username     = sy-uname.

ENDFORM.                    " SET_ATTRIBUTES_ALV_GRID_100
*&--------------------------------------------------------------------*
*&      Module  STATUS_0210  OUTPUT
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
MODULE status_0210 OUTPUT.
  SET PF-STATUS 'ST210'.
  SET TITLEBAR 'T100'.
ENDMODULE.                 " STATUS_0210  OUTPUT
*&--------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_0210  OUTPUT
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
MODULE display_alv_0210 OUTPUT.
  IF g_docking_container_210 IS INITIAL.
    PERFORM create_container_n_object_210.
    PERFORM set_attributes_alv_grid.
    PERFORM build_sortcat_display_210.
    PERFORM build_field_catalog_210 USING 'IT_REVAL'.
    PERFORM assign_itab_to_alv_210.
  ELSE.
    CALL METHOD alv_grid_210->refresh_table_display.
  ENDIF.
ENDMODULE.                 " DISPLAY_ALV_0210  OUTPUT
*&--------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_N_OBJECT_210
*&--------------------------------------------------------------------*
*       text
*------------------------------------------------------------------
*  <--  p2        text
*---------------------------------------------------------------------*
FORM create_container_n_object_210 .
  DATA: l_repid LIKE sy-repid,
        l_dynnr LIKE sy-dynnr.

  l_repid = sy-repid.
  l_dynnr = sy-dynnr.
  CREATE OBJECT g_docking_container_210
    EXPORTING
      repid     = l_repid
      dynnr     = l_dynnr
      side      = cl_gui_docking_container=>dock_at_bottom
*     RATIO     = 90
      extension = 2000.

  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = l_repid
        txt2  = sy-subrc
        txt1  = 'The control can not be created'.
  ENDIF.
  CREATE OBJECT alv_grid_210
    EXPORTING
      i_parent      = g_docking_container_210
      i_appl_events = 'X'.

  CREATE OBJECT g_event_receiver_210.
  SET HANDLER g_event_receiver_210->handle_hotspot_click
    FOR alv_grid_210.

ENDFORM.                    " CREATE_CONTAINER_N_OBJECT_210

*&--------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG_210
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->P_9036   text
*---------------------------------------------------------------------*
FORM build_field_catalog_210 USING p_itab..
  DATA: lw_itab TYPE slis_tabname,
      lw_waers LIKE t001-waers,
      l_rqty(9),
      l_datum(8),
      l_cn(2) TYPE n,
      l_mcode_seq(30).

  CLEAR: it_fieldcat_210,  it_fieldcat_210[],
         it_fieldname_210, it_fieldname_210[].
  CLEAR: w_cnt,w_repid.

  lw_itab = p_itab.

  w_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = w_repid
      i_internal_tabname = lw_itab
      i_inclname         = w_repid
    CHANGING
      ct_fieldcat        = it_fieldname_210.

  PERFORM setting_fieldcat_210 TABLES it_fieldcat_210 USING :

                                  'S' 'PUM_N'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Approval No',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'MATNR'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Material',
                                  'E' 'OUTPUTLEN'   '18',

                                 'S' 'MOD_LIFNR'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Vendor',
                                  'E' 'OUTPUTLEN'   '8',

*                                  'S' 'ZTSEQ'       ' ',
*                                  ' ' 'COLTEXT'     'Serial No',
*                                  'E' 'OUTPUTLEN'   '8',

*                                  'S' 'RESN_C'    ' ',
*                                  ' ' 'COLTEXT'     'Reason Code',
*                                  'E' 'OUTPUTLEN'   '6',

                                  'S' 'PRICE_DIFF'  ' ',
                                  ' ' 'COLTEXT'     'Price Diff',
                                  ' ' 'DECIMALS_O'  '2',
                                  ' ' 'NO_ZERO'     'X',
*                                  ' ' 'DO_SUM'      'X',
                                  ' ' 'DATATYPE'    'CURR',
                                  'E' 'OUTPUTLEN'   '13',

                                  'S' 'DATAB'    ' ',
                                  ' ' 'COLTEXT'     'Date From',
                                  ' ' 'DATATYPE'    'DATS',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'DATBI'    ' ',
                                  ' ' 'COLTEXT'     'Date To',
                                  ' ' 'DATATYPE'    'DATS',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'SUB'       ' ',
                                  ' ' 'COLTEXT'     'Sub',
                                  'E' 'OUTPUTLEN'   '4',

                                  'S' 'INV_QTY'       ' ',
                                  ' ' 'COLTEXT'     'Reval Qty',
                                  ' ' 'NO_ZERO'     'X',
                                  ' ' 'DO_SUM'      'X',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'SA_UPDATE'       ' ',
                                  ' ' 'COLTEXT'     'SA Update',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'SA_DATE'       ' ',
                                  ' ' 'COLTEXT'     'SA Date',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'SA_USER'       ' ',
                                  ' ' 'COLTEXT'     'SA User',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'REVAL_UPDATE'    ' ',
                                  ' ' 'COLTEXT'     'Reval Update',
                                  'E' 'OUTPUTLEN'   '6',

                                  'S' 'REVAL_DATE'    ' ',
                                  ' ' 'COLTEXT'     'Reval Date',
                                  'E' 'OUTPUTLEN'   '6',

                                 'S' 'REVAL_USER'       ' ',
                                  ' ' 'COLTEXT'     'Reval User',
                                  'E' 'OUTPUTLEN'   '12',

                                 'S' 'BELNR'       ' ',
                                  ' ' 'COLTEXT'     'Account Doc',
                                  ' ' 'HOTSPOT'     'X',
                                  'E' 'OUTPUTLEN'   '12',

                                 'S' 'INV_AMT'       ' ',
                                  ' ' 'COLTEXT'     'Reval Amt',
                                  ' ' 'NO_ZERO'     'X',
                                  ' ' 'DO_SUM'      'X',
                                  'E' 'OUTPUTLEN'   '12',

                                 'S' 'BELNR2'       ' ',
                                  ' ' 'COLTEXT'     '2nd Acc Doc',
                                  ' ' 'HOTSPOT'     'X',
                                  'E' 'OUTPUTLEN'   '12',

                                 'S' 'INV_AMT2'       ' ',
                                  ' ' 'COLTEXT'     'Reval Amt',
                                  ' ' 'NO_ZERO'     'X',
                                  ' ' 'DO_SUM'      'X',
                                  'E' 'OUTPUTLEN'   '12'.


ENDFORM.                    " BUILD_FIELD_CATALOG_210
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_ITAB_TO_ALV_210
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM assign_itab_to_alv_210 .
  CALL METHOD alv_grid_210->set_table_for_first_display
    EXPORTING
      is_layout       = wa_is_layout
*     i_save          = wa_save
*     is_variant      = wa_variant_100
    CHANGING
      it_fieldcatalog = it_fieldcat_210[]
      it_sort         = it_sort_210[]
      it_outtab       = it_reval[].
ENDFORM.                    " ASSIGN_ITAB_TO_ALV_210
*&---------------------------------------------------------------------*
*&      Form  SETTING_FIELDCAT_210
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT_210  text
*      -->P_9132   text
*      -->P_9133   text
*      -->P_9134   text
*----------------------------------------------------------------------*
FORM setting_fieldcat_210 TABLES p_fieldcat STRUCTURE it_fieldcat_210
                      USING    p_gubun
                               p_field
                               p_value.
  DATA : l_col(40).

  FIELD-SYMBOLS <fs>.

  IF p_gubun = 'S'.
    CLEAR: p_fieldcat.

    READ TABLE it_fieldname_210 INTO w_fieldname_210
                            WITH KEY fieldname  = p_field.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH 'Check field catalog'.
    ENDIF.

    MOVE: w_fieldname_210-fieldname TO p_fieldcat-fieldname.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' p_field  INTO l_col.
  ASSIGN (l_col) TO <fs>.
  MOVE   p_value TO <fs>.

* END - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'E'.
    ADD 1 TO w_cnt.
    p_fieldcat-col_pos = w_cnt.
    APPEND p_fieldcat.
  ENDIF.
ENDFORM.                    " SETTING_FIELDCAT_210
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0210  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0210 INPUT.
  CLEAR: w_error.
  CASE ok_code.
    WHEN 'EXIT'.
*      LEAVE PROGRAM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'REEVAL'.
      PERFORM re_eval.
      PERFORM update_status.
    WHEN 'SAUPDATE'.
      PERFORM re_update_sa.
      PERFORM update_status.
    WHEN 'DE-AGGRE'.
      PERFORM de_aggregate.
    WHEN 'PRINT'.
      PERFORM call_zrfig02.
*    WHEN 'DISP-SA'.
*      PERFORM display_sa.
*    WHEN 'INFOREC'.
*      PERFORM display_info.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0210  INPUT
*&---------------------------------------------------------------------*
*&      Form  GET_SELECTED_REVAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_selected_reval .
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
       lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i.


*  CALL METHOD alv_grid_210->get_selected_rows
*    IMPORTING
*      et_index_rows = lt_rows[]
*      et_row_no     = lt_row_no.

*  CALL METHOD cl_gui_cfw=>flush.
*
*  IF sy-subrc NE 0.
*    w_repid = sy-repid.
*    CALL FUNCTION 'POPUP_TO_INFORM'
*      EXPORTING
*        titel = w_repid
*        txt2  = sy-subrc
*        txt1  = 'Error found during flushing of ALV Grid Control'.
*    EXIT.
*  ENDIF.

  REFRESH: it_reval_selected.
*  IF lt_rows[] IS INITIAL.
*    MESSAGE e001.
*  ENDIF.

  LOOP AT it_reval.
    CLEAR: it_reval_selected.
    MOVE-CORRESPONDING it_reval TO it_reval_selected.
    APPEND it_reval_selected.
  ENDLOOP.

*  LOOP AT lt_rows.
*    CLEAR: it_reval_selected.
*    READ TABLE it_reval INDEX lt_rows-index.
*    MOVE-CORRESPONDING it_reval TO it_reval_selected.
*    APPEND it_reval_selected.
*  ENDLOOP.
ENDFORM.                    " GET_SELECTED_REVAL
*&---------------------------------------------------------------------*
*&      Form  UPDATE_COMP_IF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_comp_if USING p_flag.
  CASE p_flag.
    WHEN 'EV'.
      UPDATE ztmm_mod_reval
             SET: reval_update = 'R'
             reval_date = sy-datum
             reval_user = sy-uname
           WHERE pum_n = it_reval-pum_n
             AND matnr = it_itab-comp
             AND ztseq = it_itab-ztseq.
*           WHERE matnr = it_reval-matnr
*             AND lifnr = it_reval-lifnr
*             AND pum_n = it_reval-pum_n.
    WHEN 'SA'.
      UPDATE ztmm_mod_reval
             SET: sa_update = 'R'
             sa_date = sy-datum
             sa_user = sy-uname
           WHERE pum_n = it_reval-pum_n
             AND matnr = it_itab-comp
             AND ztseq = it_itab-ztseq.
*           WHERE matnr = it_reval-matnr
*             AND lifnr = it_reval-lifnr
*             AND pum_n = it_reval-pum_n.

  ENDCASE.
ENDFORM.                    " UPDATE_COMP_IF
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_UI_FUNCTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exclude_ui_function .

  DATA ls_exclude TYPE ui_func.

  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_move_row.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND ls_exclude TO it_exclude.

  ls_exclude = cl_gui_alv_grid=>mc_fc_sort_asc.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_sort_dsc.
  APPEND ls_exclude TO it_exclude.

ENDFORM.                    " EXCLUDE_UI_FUNCTION
*&--------------------------------------------------------------------*
*&      Form  build_sortcat_display
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM build_sortcat_display_100.
  REFRESH it_sort_100.

  it_sort_100-spos           = 1.
  it_sort_100-fieldname      = 'PUM_N'.
  it_sort_100-up             = 'X'.
  it_sort_100-subtot         = 'X'.
  APPEND it_sort_100.

  it_sort_100-spos           = 2.
  it_sort_100-fieldname      = 'ZTSEQ'.
  it_sort_100-up             = 'X'.
  it_sort_100-subtot         = 'X'.
  APPEND it_sort_100.

  it_sort_100-spos           = 3.
  it_sort_100-fieldname      = 'MATNR'.
  it_sort_100-up             = 'X'.
  it_sort_100-subtot         = 'X'.
  APPEND it_sort_100.
ENDFORM.                    " build_sortcat_display_100
*&---------------------------------------------------------------------*
*&      Form  build_sortcat_display_210
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM build_sortcat_display_210.
  REFRESH it_sort_210.
  it_sort_210-spos           = 1.
  it_sort_210-fieldname      = 'PUM_N'.
  it_sort_210-up             = 'X'.
  it_sort_210-subtot         = 'X'.
  APPEND it_sort_210.

  it_sort_210-spos           = 2.
  it_sort_210-fieldname      = 'MATNR'.
  it_sort_210-up             = 'X'.
  it_sort_210-subtot         = 'X'.
  APPEND it_sort_210.
ENDFORM.                    " build_sortcat_display_100
*&--------------------------------------------------------------------*
*&      Form  CALL_ZRFIG02
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM call_zrfig02.
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i,
        l_bukrs(4) VALUE 'H201'.
  DATA: l_xblnr LIKE bkpf-xblnr,
        l_zbdat LIKE sy-datum,
        l_year LIKE bkpf-gjahr.

  RANGES: lr_matnr FOR mara-matnr,
          lr_lifnr FOR lfa1-lifnr,
          lr_gjahr FOR ekbe-gjahr,
          lr_budat FOR sy-datum.

  CALL METHOD alv_grid_210->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows[]
      et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = w_repid
        txt2  = sy-subrc
        txt1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.

  REFRESH: r_xblnr, r_gjahr, r_cpudt.
  r_xblnr-option = 'EQ'.
  r_xblnr-sign = 'I'.

  LOOP AT lt_rows.

    READ TABLE it_reval INDEX lt_rows-index.
    IF it_reval-belnr IS NOT INITIAL.
      r_xblnr-low = it_reval-belnr.
      COLLECT r_xblnr.
      l_zbdat = it_reval-reval_date.

      IF it_reval-belnr2 IS NOT INITIAL.
        r_xblnr-low = it_reval-belnr2.
        COLLECT r_xblnr.
      ENDIF.
    ENDIF.
*    SELECT SINGLE belnr reval_date
*      INTO (l_xblnr, l_zbdat)
*      FROM ztmm_mod_rev_res
*             WHERE pum_n = it_reval-pum_n
*              AND ztseq = it_reval-ztseq
*              AND matnr = it_reval-matnr
*              AND lifnr = it_reval-lifnr
*              AND datab = it_reval-datab
*              AND datbi = it_reval-datbi.
*
*    IF sy-subrc = 0.
*      r_xblnr-low = l_xblnr.
*      APPEND r_xblnr.
*    ENDIF.
  ENDLOOP.

  l_year = l_zbdat+0(4).

*  SET PARAMETER ID 'BUK' FIELD l_bukrs.
*  SET PARAMETER ID 'REV' FIELD R_XBLNR.
*  CALL TRANSACTION 'ZRFIG02' AND SKIP FIRST SCREEN.

  r_gjahr-option = 'EQ'.
  r_gjahr-sign = 'I'.
  r_gjahr-low = l_year.
  APPEND r_gjahr.


  SUBMIT zrfig02    "via selection-screen
         WITH p_bukrs  = 'H201'
         WITH s_gjahr IN r_gjahr
         WITH s_xblnr IN r_xblnr
         WITH s_cpudt IN r_cpudt
         WITH p_own = ' '
*         WITH update = 'X'
*         WITH p_alv = ' '
         AND RETURN.

*  PERFORM call_tcode_zrfig02 USING l_bukrs l_year.
ENDFORM.                    " CALL_ZRFIG02

*&---------------------------------------------------------------------*
*&      Form  UPDATE_MOD_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_FLAG  text
*----------------------------------------------------------------------*
FORM update_mod_log  USING p_flag.
  DATA: lw_rev_res LIKE ztmm_mod_rev_res.

  IF p_flag <> 'E'.

    UPDATE ztmm_mod_rev_res
         SET: reval_update = p_flag
         reval_date = sy-datum
         reval_user = sy-uname
         belnr = it_reval-belnr
         inv_amt = it_reval-inv_amt
         inv_qty = it_reval-inv_qty
         zmsg =  it_reval-zmsg
         WHERE matnr = it_reval-matnr
*         AND lifnr = it_reval-lifnr
         AND pum_n = it_reval-pum_n
*               AND ztseq = it_itab-ztseq
         AND datab = it_reval-datab
         AND datbi = it_reval-datbi.
*           AND belnr = ' '.

  ELSE.
** Not set status
    UPDATE ztmm_mod_rev_res
      SET: reval_date = sy-datum
      reval_user = sy-uname
      zmsg =  it_reval-zmsg
      WHERE matnr = it_reval-matnr
*      AND lifnr = it_reval-lifnr
      AND pum_n = it_reval-pum_n
*               AND ztseq = it_itab-ztseq
      AND datab = it_reval-datab
      AND datbi = it_reval-datbi
      AND belnr = ' '.
  ENDIF.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
    MESSAGE s000(zz) WITH text-m16.
  ENDIF.

ENDFORM.                    " UPDATE_MOD_LOG
*&---------------------------------------------------------------------*
*&      Form  SAVE_SIMULATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_simulation.
  DATA: lt_log LIKE TABLE OF  ztmm_mod_simulog  WITH HEADER LINE,
        l_seq LIKE  lt_log-zseq.

  LOOP AT it_itab.
    AT NEW matnr.
      l_seq  = 1.
    ENDAT.
    MOVE-CORRESPONDING it_itab TO lt_log.
    lt_log-zbdat = sy-datum.
    lt_log-zuser = sy-uname.
    lt_log-zseq = l_seq.
    APPEND lt_log.
    l_seq  = l_seq + 1.
  ENDLOOP.
  INSERT ztmm_mod_simulog FROM TABLE lt_log
   ACCEPTING DUPLICATE KEYS.
  IF sy-subrc <> 0 AND sy-subrc <> 4.
    MESSAGE e999 WITH 'Data saving Error: Simplaution log'.
  ENDIF.
ENDFORM.                    " SAVE_SIMULATION
*&---------------------------------------------------------------------*
*&      Form  SAVE_MOD_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_mod_result .
  DATA: lt_temp LIKE TABLE OF ztmm_mod_rev_res WITH HEADER LINE.
  LOOP AT it_reval.
    MOVE-CORRESPONDING it_reval TO lt_temp.
    APPEND lt_temp.
  ENDLOOP.
  INSERT ztmm_mod_rev_res FROM TABLE lt_temp
     ACCEPTING DUPLICATE KEYS.
  IF sy-subrc <> 0 AND sy-subrc <> 4.
    MESSAGE e999 WITH 'Data saving Error: Module Result '.
  ENDIF.
ENDFORM.                    " SAVE_MOD_RESULT
*&---------------------------------------------------------------------*
*&      Form  RE_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM re_process .
  DATA: lt_temp LIKE TABLE OF ztmm_mod_rev_res WITH HEADER LINE.
  DATA: ls_style TYPE lvc_s_styl .

  IF it_reval[] IS NOT INITIAL.
    LOOP AT it_reval.
      REFRESH: it_reval-celltab.
      MODIFY it_reval.
    ENDLOOP.
  ENDIF.
  REFRESH: it_reval.

  PERFORM get_selected_data.
  SELECT * INTO TABLE lt_temp
    FROM ztmm_mod_rev_res
    FOR ALL ENTRIES IN it_pri_selected
    WHERE pum_n = it_pri_selected-pum_n.

  IF sy-subrc <> 0.
    MESSAGE s999 WITH 'No data'.
  ELSE.
    LOOP AT lt_temp.
      MOVE-CORRESPONDING lt_temp TO it_reval.
      APPEND it_reval.
    ENDLOOP.

    LOOP AT it_reval.
      ls_style-fieldname = 'BELNR'.
      ls_style-style = cl_gui_alv_grid=>mc_style_hotspot.
      APPEND ls_style TO it_reval-celltab.
    ENDLOOP.
    CALL SCREEN 210.
  ENDIF.
ENDFORM.                    " RE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  RE_UPDATE_SA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERROR  text
*----------------------------------------------------------------------*
FORM re_update_sa.
  DATA: l_answer(1),
       l_error(1),
       l_lines TYPE i,
          l_text(90).

  REFRESH it_error.

  PERFORM get_selected_reval.
  DESCRIBE TABLE it_reval_selected LINES l_lines.
  IF l_lines = 0.
    MESSAGE s999 WITH 'Must select at least one module'.
    EXIT.
  ENDIF.

*  LOOP AT it_reval_selected.
*    IF it_reval_selected-sa_update IS NOT INITIAL.
*      CONCATENATE 'SA Already Updated' it_itab-ebeln
*      INTO l_text
*       SEPARATED BY space.
*      MESSAGE e999 WITH l_text.
*      EXIT.
*    ENDIF.
*  ENDLOOP.

  LOOP AT it_reval_selected.
    IF it_reval_selected-sa_update IS NOT INITIAL.
      DELETE it_reval_selected..
    ENDIF.
  ENDLOOP.

  LOOP AT it_reval_selected.
    AT NEW pum_n.
      SELECT SINGLE *
      FROM ztmm_mod_rev_res
      WHERE pum_n =  it_reval_selected-pum_n.
      IF sy-subrc <> 0.
        MESSAGE e999 WITH 'Please use Revaluation function'.
      ENDIF.
    ENDAT.
  ENDLOOP.

  w_update_flag = 'X'.
  PERFORM get_simulation.

  LOOP AT it_reval_selected.
    CLEAR: w_index, w_index_rev.
    w_index_rev = sy-tabix.

    MESSAGE s999 WITH 'Processing' it_reval_selected-matnr
      ' ' it_reval_selected-pum_n.

    CLEAR: it_itab.
    READ TABLE it_itab WITH KEY  pum_n = it_reval_selected-pum_n
                                 matnr = it_reval_selected-matnr
                                 excfdate = it_reval_selected-datab
                                 exctdate = it_reval_selected-datbi.
*                                BINARY SEARCH.
    w_index_sa = sy-tabix.
    w_index = w_index_sa.
    CLEAR l_error.
    IF it_itab-multi_sa > 1.
      LOOP AT it_itab FROM w_index
                      WHERE pum_n = it_reval_selected-pum_n
                        AND matnr = it_reval_selected-matnr
                        AND excfdate = it_reval_selected-datab
                        AND exctdate = it_reval_selected-datbi.
        w_index = sy-tabix.
        PERFORM update_sa USING l_error.
        IF l_error IS NOT INITIAL.
          EXIT.
        ENDIF.
      ENDLOOP.
    ELSE.
      PERFORM update_sa USING l_error.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " RE_UPDATE_SA
*&---------------------------------------------------------------------*
*&      Form  GET_SIMULATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_simulation .
  DATA: lt_temp LIKE TABLE OF  ztmm_mod_simulog WITH HEADER LINE.
  REFRESH it_itab.
  SELECT * INTO TABLE lt_temp
    FROM ztmm_mod_simulog
    FOR ALL ENTRIES IN it_reval
    WHERE pum_n =  it_reval-pum_n
      AND matnr =  it_reval-matnr.
*      AND lifnr =  it_reval_selected-lifnr
*      AND excfdate = it_reval-datab
*      AND exctdate = it_reval-datbi.
  SORT lt_temp BY pum_n matnr zseq.
  LOOP AT lt_temp.
    MOVE-CORRESPONDING lt_temp TO it_itab.
    APPEND it_itab.
  ENDLOOP.

ENDFORM.                    " GET_SIMULATION
*&---------------------------------------------------------------------*
*&      Form  CHECK_VALID_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_valid_data.
  DATA: l_pum_n LIKE it_reval_selected-pum_n,
        l_index LIKE sy-datum.

  SORT it_reval_selected BY pum_n matnr.
  LOOP AT it_pri_selected.
*      l_index = sy-tabix.
    AT NEW pum_n.
      SELECT SINGLE pum_n INTO l_pum_n
         FROM ztmm_mod_rev_res
          WHERE pum_n =  it_pri_selected-pum_n.
      IF sy-subrc = 0.
        MESSAGE s999 WITH
         'Please use RE_PROCESS function foR' it_pri_selected-pum_n.
        DELETE it_pri_selected
         WHERE pum_n = l_pum_n.
      ENDIF.
    ENDAT.

  ENDLOOP.
ENDFORM.                    " CHECK_VALID_DATA
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ITAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_itab .
  DATA: l_index LIKE sy-tabix.

  LOOP AT it_itab FROM w_index
                                WHERE pum_n = it_reval_selected-pum_n
                                  AND matnr = it_reval_selected-matnr
                                  AND excfdate = it_reval_selected-datab
                                  AND exctdate = it_reval_selected-datbi
                                  .
    l_index = sy-tabix.
    MODIFY it_itab INDEX l_index TRANSPORTING reval_update msg.
  ENDLOOP.
ENDFORM.                    " UPDATE_ITAB
*&---------------------------------------------------------------------*
*&      Form  GET_REVAL_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_reval_result .
  DATA: lt_temp LIKE TABLE OF ztmm_mod_rev_res WITH HEADER LINE.

  IF it_reval[] IS NOT INITIAL.
    LOOP AT it_reval.
      REFRESH: it_reval-celltab.
      MODIFY it_reval.
    ENDLOOP.
  ENDIF.
  REFRESH: it_reval.

*  PERFORM get_selected_data.
  SELECT * INTO TABLE lt_temp
    FROM ztmm_mod_rev_res
    FOR ALL ENTRIES IN it_pri_selected
    WHERE pum_n = it_pri_selected-pum_n.

  LOOP AT lt_temp.
    MOVE-CORRESPONDING lt_temp TO it_reval.
    APPEND it_reval.
  ENDLOOP.

ENDFORM.                    " GET_REVAL_RESULT
*&---------------------------------------------------------------------*
*&      Form  UPDATE_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_status .
  CHECK w_update_flag = 'X'.
  PERFORM reset_status.
  PERFORM modify_existing_status.
  CLEAR: w_update_flag.
ENDFORM.                    " UPDATE_STATUS
*&---------------------------------------------------------------------*
*&      Form  RESET_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reset_status .
  DATA: l_index LIKE sy-tabix,
       l_flag_sa(1),
       l_flag_rev(1).

  DATA: BEGIN OF lt_temp OCCURS 0,
        pum_n LIKE ztmm_mod_simulog-pum_n,
        comp LIKE ztmm_mod_simulog-comp,
        matnr LIKE ztmm_mod_simulog-matnr,
        END OF lt_temp.

  REFRESH it_pri_selected.

  LOOP AT it_pri.
    MOVE-CORRESPONDING it_pri TO it_pri_selected.
    APPEND it_pri_selected.
  ENDLOOP.

  PERFORM get_reval_result.
  PERFORM get_simulation.
  SORT it_itab BY pum_n comp.

** Furong on 09/08/14
  SORT it_reval BY pum_n.
  LOOP AT it_reval.
    AT NEW pum_n.
      l_flag_sa = l_flag_rev = 'X'.
    ENDAT.
    IF it_reval-sa_update <> 'C'
       AND it_reval-sa_update <> 'X'.
      l_flag_sa = 'R'.
    ENDIF.
    IF it_reval-reval_update <> 'C'
      AND it_reval-reval_update <> 'X'.
      l_flag_rev = 'R'.
    ENDIF.
    AT END OF pum_n.
      UPDATE ztmm_mod_reval
                  SET: sa_update = l_flag_sa
                  reval_update = l_flag_rev
                  sa_date = sy-datum
                  sa_user = sy-uname
                  reval_date = sy-datum
                  reval_user = sy-uname
      WHERE pum_n = it_reval-pum_n.
      if sy-subrc = 0.
         COMMIT WORK.
      else.
        ROLLBACK work.
        MESSAGE s999 WITH 'ZTMM_MOD_REVAL update failed'.
      endif.
    ENDAT.
 ENDLOOP.

*  LOOP AT it_pri_selected.
*    READ TABLE it_itab WITH KEY pum_n = it_pri_selected-pum_n
*              comp = it_pri_selected-matnr.
*    l_index = sy-tabix.
*    LOOP AT it_itab FROM l_index
*            WHERE pum_n = it_pri_selected-pum_n
*              AND comp = it_pri_selected-matnr.
*      lt_temp-pum_n = it_itab-pum_n.
*      lt_temp-matnr = it_itab-matnr.
*      lt_temp-comp = it_itab-comp.
**              lt_temp-ztseq = it_itab-ztseq.
*      COLLECT lt_temp.
*    ENDLOOP.
*  ENDLOOP.
*
*  SORT it_reval BY pum_n matnr.
*
*  SORT lt_temp BY pum_n comp matnr.

*  LOOP AT lt_temp.
*    AT NEW comp.
*      CLEAR: l_flag_sa, l_flag_rev.
*    ENDAT.
*
*    READ TABLE it_reval WITH KEY pum_n = lt_temp-pum_n
*                              matnr = lt_temp-matnr.
*    l_index = sy-tabix.
*    LOOP AT it_reval FROM l_index
*          WHERE pum_n = lt_temp-pum_n
*            AND matnr = lt_temp-matnr.
*      IF it_reval-sa_update IS INITIAL.
*        l_flag_sa = 'X'.
*      ENDIF.
*      IF it_reval-reval_update IS INITIAL.
*        l_flag_rev = 'X'.
*      ENDIF.
*      IF l_flag_sa = 'X' OR  l_flag_rev = 'X'.
*        EXIT.
*      ENDIF.
*    ENDLOOP.
*
*    AT END OF comp.
*      IF l_flag_sa = 'X'.
*        UPDATE ztmm_mod_reval
*               SET: sa_update = 'R'
*               sa_date = sy-datum
*               sa_user = sy-uname
*         WHERE pum_n = lt_temp-pum_n
*           AND matnr = lt_temp-comp.
*        CLEAR: l_flag_sa.
*      ELSE.
*        UPDATE ztmm_mod_reval
*            SET: sa_update = 'X'
*            sa_date = sy-datum
*            sa_user = sy-uname
*         WHERE pum_n = lt_temp-pum_n
*           AND matnr = lt_temp-comp.
**          AND ztseq = l_ztseq .
*      ENDIF.
*      IF l_flag_rev = 'X'.
*        UPDATE ztmm_mod_reval
*               SET: reval_update = 'R'
*               reval_date = sy-datum
*               reval_user = sy-uname
*         WHERE pum_n = lt_temp-pum_n
*           AND matnr = lt_temp-comp.
*        CLEAR: l_flag_rev.
*      ELSE.
*        UPDATE ztmm_mod_reval
*            SET: reval_update = 'X'
*             reval_date = sy-datum
*               reval_user = sy-uname
*         WHERE pum_n = lt_temp-pum_n
*           AND matnr = lt_temp-comp.
**          AND ztseq = l_ztseq .
*      ENDIF.
*      COMMIT WORK.
*    ENDAT.
*    IF l_flag_sa = 'X' OR  l_flag_rev = 'X'.
*      CONTINUE.
*    ENDIF.
*  ENDLOOP.
  ENDFORM.                    " RESET_STATUS
*&---------------------------------------------------------------------*
*&      Form  LOCKING_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_REPID  text
*      -->P_L_ANSWER  text
*----------------------------------------------------------------------*
FORM locking_rtn  USING    p_repid p_result.
  PERFORM check_lock_object  USING p_repid p_result.
  PERFORM check_enqueue_read USING p_repid p_result.
  PERFORM check_batchjob     USING p_repid p_result.
ENDFORM.                    " LOCKING_RTN
*&---------------------------------------------------------------------*
*&      Form  CHECK_LOCK_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_REPID  text
*      -->P_P_RESULT  text
*----------------------------------------------------------------------*
FORM check_lock_object USING p_repid p_result.
  CALL FUNCTION 'ENQUEUE_EPROG'
    EXPORTING
      mode_trdir     = 'E'
      programm       = p_repid
      x_programm     = ' '
      _scope         = '1'
      _wait          = ' '
      _collect       = ' '
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    MOVE: 'E' TO p_result.
  ENDIF.
ENDFORM.                    " CHECK_LOCK_OBJECT
*&---------------------------------------------------------------------*
*&      Form  CHECK_ENQUEUE_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_REPID  text
*      -->P_P_RESULT  text
*----------------------------------------------------------------------*
FORM check_enqueue_read USING p_repid  p_result.
  DATA: l_garg        LIKE seqg3-garg,
        l_gname       LIKE seqg3-gname,
        l_lock_number LIKE sy-tabix.

  DATA: lt_lock TYPE TABLE OF seqg3 WITH HEADER LINE.

  MOVE: sy-mandt     TO l_garg(3),
        p_repid      TO l_garg+3,
        p_repid      TO l_gname.

  CALL FUNCTION 'ENQUEUE_READ'
    EXPORTING
      gclient               = sy-mandt
      gname                 = l_gname
      garg                  = l_garg
      guname                = ' '
      local                 = ' '
      fast                  = ' '
    IMPORTING
      number                = l_lock_number
    TABLES
      enq                   = lt_lock
    EXCEPTIONS
      communication_failure = 1
      system_failure        = 2
      OTHERS                = 3.
  IF sy-subrc <> 0.
    p_result = 'E'.
    EXIT.
  ENDIF.

  IF l_lock_number > 1.
    p_result = 'E'.
  ENDIF.
ENDFORM.                    " CHECK_ENQUEUE_READ
*&---------------------------------------------------------------------*
*&      Form  CHECK_BATCHJOB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_batchjob USING p_repid  p_result.
  DATA: lt_joblist LIKE tbtcjob OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'BP_FIND_JOBS_WITH_PROGRAM'
    EXPORTING
      abap_program_name             = p_repid
      dialog                        = 'N'
      status                        = 'R'
    TABLES
      joblist                       = lt_joblist
    EXCEPTIONS
      no_jobs_found                 = 1
      program_specification_missing = 2
      invalid_dialog_type           = 3
      job_find_canceled             = 4
      OTHERS                        = 5.

  IF sy-batch EQ 'X'.
    READ TABLE lt_joblist INDEX 2.
    IF sy-subrc EQ 0.
      p_result = 'E'.
    ENDIF.
  ELSE.
    READ TABLE lt_joblist INDEX 1.
    IF sy-subrc EQ 0.
      p_result = 'E'.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_BATCHJOB
*-------------------------*
*&      Form  UNLOCK_PROG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM unlock_prog .
  CALL FUNCTION 'DEQUEUE_EPROG'
    EXPORTING
      mode_trdir = 'E'
      programm   = sy-repid
      x_programm = ' '
      _scope     = '1'
*     _SYNCHRON  = ' '
*     _COLLECT   = ' '
    .
ENDFORM.                    " UNLOCK_PROG
*&---------------------------------------------------------------------*
*&      Form  MODIFY_EXISTING_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_existing_status .
  LOOP AT it_pri.
    SELECT SINGLE reval_update
      reval_date reval_user
      sa_update
      sa_date sa_user
      INTO (it_pri-reval_update, it_pri-reval_date,
            it_pri-reval_user,
            it_pri-sa_update, it_pri-sa_date,
            it_pri-sa_user)
      FROM ztmm_mod_reval
      WHERE pum_n = it_pri-pum_n
        AND matnr = it_pri-matnr.
    IF sy-subrc = 0.
      MODIFY it_pri.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " MODIFY_EXISTING_STATUS
*&---------------------------------------------------------------------*
*&      Form  UPDATE_MOD_LOG_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_6642   text
*----------------------------------------------------------------------*
FORM update_mod_log_2  USING p_flag.
  DATA: lw_rev_res LIKE ztmm_mod_rev_res.

  IF p_flag <> 'E'.

    UPDATE ztmm_mod_rev_res
      SET: belnr2 = it_reval-belnr2
           inv_amt2 = it_reval-inv_amt2
           zmsg2 =  it_reval-zmsg2
         WHERE matnr = it_reval-matnr
*         AND lifnr = it_reval-lifnr
         AND pum_n = it_reval-pum_n
*               AND ztseq = it_itab-ztseq
         AND datab = it_reval-datab
         AND datbi = it_reval-datbi.
*           AND belnr = ' '.

  ELSE.
** Not set status
    UPDATE ztmm_mod_rev_res
      SET: zmsg2 =  it_reval-zmsg2
      WHERE matnr = it_reval-matnr
*      AND lifnr = it_reval-lifnr
      AND pum_n = it_reval-pum_n
*               AND ztseq = it_itab-ztseq
      AND datab = it_reval-datab
      AND datbi = it_reval-datbi
      AND belnr2 = ' '.
  ENDIF.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
    MESSAGE s000(zz) WITH text-m16.
  ENDIF.
ENDFORM.                    " UPDATE_MOD_LOG_2

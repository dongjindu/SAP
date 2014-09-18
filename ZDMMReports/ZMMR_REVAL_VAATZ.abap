***********************************************************************
* Program Name      : ZMMR_REVAL_VAATZL
* Author            : Furong Wang
* Creation Date     : 06/2013
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 08.26.2014      Victor         added cal_amt, error_amt
***********************************************************************

REPORT zmmr_reval_vaatzl NO STANDARD PAGE HEADING
                             LINE-SIZE 132
                             LINE-COUNT 64(1)
                             MESSAGE-ID zmmm.
TYPE-POOLS : slis.

TABLES: ekko, ekpo, ztmm_pri_reval.

DATA : it_pri LIKE TABLE OF ztmm_pri_reval WITH HEADER LINE,
       it_error LIKE TABLE OF ztmm_pri_reval WITH HEADER LINE,
       it_pri_selected LIKE TABLE OF ztmm_pri_reval WITH HEADER LINE.
DATA : it_reval_log LIKE TABLE OF ztmm_reval_log WITH HEADER LINE.

DATA : BEGIN OF it_itab OCCURS 0,
       matnr LIKE ekpo-matnr,
       lifnr LIKE ekko-lifnr,
       pum_n LIKE ztmm_pri_reval-pum_n,
       ztseq LIKE ztmm_pri_reval-ztseq,
       sa_update LIKE ztmm_pri_reval-sa_update,
       reval_update LIKE ztmm_pri_reval-reval_update,
       excprice(13), " LIKE KONP-KBETR,
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
       newamt LIKE konp-kbetr,
       diff  LIKE  konp-kbetr,
       cal_amt LIKE konp-kbetr,
       error_amt LIKE konp-kbetr,
       peinh(5),
       rsn LIKE t686c-kzust,
       msg(100),
       END OF it_itab.

DATA: it_itab_onestep LIKE TABLE OF it_itab WITH HEADER LINE.

DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldcat_100  TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_fieldname_100 TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE.

DATA : wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
*      it_color type LVC_T_SCOL,
*      wa_color like line of it_color,
      w_fieldname    LIKE LINE OF it_fieldcat,
      w_fieldname_100  LIKE LINE OF it_fieldcat_100.

DATA: wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
      wa_variant TYPE disvariant.      "for parameter IS_VARIANT

DATA: wa_custom_control TYPE        scrfname VALUE 'ALV_CONTAINER',
      alv_grid          TYPE REF TO cl_gui_alv_grid,
      grid_container    TYPE REF TO cl_gui_custom_container.

DATA: wa_custom_control_100 TYPE scrfname VALUE 'ALV_CONTAINER_100',
      alv_grid_100          TYPE REF TO cl_gui_alv_grid,
      grid_container_100    TYPE REF TO cl_gui_custom_container.

DATA: ok_code      LIKE sy-ucomm,
      w_repid  LIKE sy-repid,
      w_cnt       TYPE   i,
      w_no_data(1),
      w_code      LIKE sy-ucomm.

DATA: w_vename LIKE lfa1-name1.

DATA : BEGIN OF it_sa_all OCCURS 0,
         ebeln LIKE ekpo-ebeln,
         ebelp LIKE ekpo-ebelp,
         lifnr LIKE ekko-lifnr,
         matnr LIKE ekpo-matnr,
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

DATA: it_log LIKE TABLE OF ztmm_reval_log WITH HEADER LINE.
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

SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
SELECT-OPTIONS:
   s_pum_n FOR ztmm_pri_reval-pum_n,
   s_lifnr FOR ekko-lifnr,
   s_matnr FOR ekpo-matnr,
   s_ekgrp FOR ekko-ekgrp,
   s_zbdat FOR sy-datum DEFAULT sy-datum.

PARAMETERS: p_status(1).

SELECTION-SCREEN END OF BLOCK block1.

SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-007.
PARAMETERS: p_pdate LIKE sy-datum DEFAULT sy-datum OBLIGATORY.
PARAMETERS: p_rver  LIKE somlreci1-receiver." OBLIGATORY.
*PARAMETERS: p_com RADIOBUTTON GROUP grp1 DEFAULT 'X',
*            p_mod RADIOBUTTON GROUP grp1.
SELECTION-SCREEN END OF BLOCK block2.


*SELECTION-SCREEN BEGIN OF BLOCK block3 WITH FRAME TITLE text-006.
*PARAMETERS:
*  p_test AS CHECKBOX.
*SELECTION-SCREEN END OF BLOCK block3.

INITIALIZATION.
**---
START-OF-SELECTION.
  PERFORM get_data.
  IF it_pri[] IS INITIAL.
    MESSAGE s999 WITH text-m01.
  ELSE.
    CHECK w_error IS INITIAL.
*    PERFORM get_scheduling_agreement.
*    PERFORM make_data.
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
               FOR ALL ENTRIES IN it_pri_selected
               WHERE a~bstyp EQ 'L'
                 AND b~matnr = it_pri_selected-matnr
                 AND a~lifnr = it_pri_selected-lifnr
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
  DATA: l_flag(1).
  DATA: l_datab LIKE a018-datab,
         l_grqty LIKE ekbe-menge,
         l_gramt LIKE ekbe-dmbtr,
         l_price LIKE ekbe-dmbtr,
         l_newamt LIKE ekbe-dmbtr,
         l_diff LIKE ekbe-dmbtr,
         l_recno TYPE i.

  DATA: lt_a016 LIKE TABLE OF a016 WITH HEADER LINE,
        lt_itab LIKE TABLE OF it_itab WITH HEADER LINE,
        lt_ekbe LIKE TABLE OF ekbe WITH HEADER LINE.

  REFRESH it_itab.
  SORT it_sa_all BY matnr lifnr.

  LOOP AT it_pri_selected.
    CLEAR: l_flag.

    lt_itab-matnr = it_pri_selected-matnr.
    lt_itab-lifnr = it_pri_selected-lifnr.
    lt_itab-pum_n = it_pri_selected-pum_n.
    lt_itab-ztseq = it_pri_selected-ztseq.
    lt_itab-sa_update = it_pri_selected-sa_update.
    lt_itab-reval_update = it_pri_selected-reval_update.
    lt_itab-excprice = it_pri_selected-term_price.
    lt_itab-excfdate = it_pri_selected-term_from.
    lt_itab-exctdate = it_pri_selected-term_to.
    lt_itab-rsn = it_pri_selected-resn_c.

    LOOP AT it_sa_all WHERE matnr = lt_itab-matnr
                       AND lifnr  = lt_itab-lifnr.
      SELECT * INTO TABLE lt_a016
                     FROM a016
                    WHERE kappl EQ 'M'
                      AND kschl EQ 'PB00'
                      AND evrtn EQ it_sa_all-ebeln
                      AND evrtp EQ it_sa_all-ebelp
                      AND datab <= lt_itab-exctdate
                      AND datbi >= lt_itab-excfdate.

      LOOP AT lt_a016.
        SELECT SINGLE kbetr INTO lt_itab-saprice
          FROM konp
          WHERE knumh = lt_a016-knumh
            AND kschl = 'PB00'.
        lt_itab-ebeln = it_sa_all-ebeln.
        lt_itab-ebelp = it_sa_all-ebelp.
        lt_itab-peinh = it_sa_all-peinh.
        lt_itab-safdate = lt_a016-datab.
        lt_itab-satdate = lt_a016-datbi.
        lt_itab-deleted = it_sa_all-loekz.
        APPEND lt_itab.
        l_flag = 'X'.
      ENDLOOP.
      CLEAR: it_sa_all, lt_a016[], lt_a016.
    ENDLOOP.
    IF l_flag IS INITIAL.
      APPEND lt_itab.
    ENDIF.
    CLEAR: lt_itab.
  ENDLOOP.

  LOOP AT lt_itab.
    REFRESH: r_matnr, r_refdt, r_ebeln, r_ebelp,r_lifnr.

    it_itab = lt_itab.
    IF NOT lt_itab-ebeln IS INITIAL.

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

      APPEND r_refdt.

      r_lifnr-sign  = 'I'.
      r_lifnr-option  = 'EQ'.
      r_lifnr-low  = lt_itab-lifnr.
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

      SUBMIT zmmr_sa_reval_get_inv                          "ZRFI013
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
      IF l_recno = 0.
        CONTINUE.
      ENDIF.
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
    ENDIF.


*-< 08.22.2014 Victor added 2 fields
    it_itab-cal_amt  =  ( it_itab-excprice - it_itab-saprice )
                         * it_itab-grqty.
    it_itab-error_amt  = it_itab-cal_amt - it_itab-diff.
*->

    APPEND it_itab.

  ENDLOOP.
  SORT it_itab BY pum_n lifnr matnr ebeln.

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
  ELSE.
    SET PF-STATUS 'ST300'.
    SET TITLEBAR 'T300'.
  ENDIF.

ENDMODULE.                 " STATUS_0200  OUTPUT
*&--------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_200  OUTPUT
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
MODULE display_alv_200 OUTPUT.
  IF grid_container IS INITIAL.
    PERFORM create_container_n_object.
    PERFORM set_attributes_alv_grid.
    PERFORM build_sortcat_display.
    PERFORM build_field_catalog USING 'IT_ITAB'.
    PERFORM assign_itab_to_alv.
  ELSE.
    CALL METHOD alv_grid->refresh_table_display.
  ENDIF.

ENDMODULE.                 " DISPLAY_ALV_200  OUTPUT
*&--------------------------------------------------------------------*
*&      Form  create_container_n_object_9000
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM create_container_n_object.
  DATA:   w_repid LIKE sy-repid.
  CREATE OBJECT grid_container
    EXPORTING
      container_name              = wa_custom_control
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.
  w_repid = sy-repid.
  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = w_repid
        txt2  = sy-subrc
        txt1  = 'The control can not be created'.
  ENDIF.
  CREATE OBJECT alv_grid
    EXPORTING
      i_parent      = grid_container
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
*  wa_is_layout-info_fname = 'IF'.
*  wa_is_layout-ctab_fname = 'CT'.
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
  it_sort-spos           = 1.
  it_sort-fieldname      = 'PUM_N'.
  it_sort-up             = 'X'.
  it_sort-subtot         = 'X'.
  APPEND it_sort.

  it_sort-spos           = 2.
  it_sort-fieldname      = 'LIFNR'.
  it_sort-up             = 'X'.
  it_sort-subtot         = 'X'.
  APPEND it_sort.

  it_sort-spos           = 3.
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
      i_save               = wa_save
      is_variant           = wa_variant
*     i_default            = space
*     it_toolbar_excluding = it_toolbar_excluding[]
    CHANGING
      it_fieldcatalog      = it_fieldcat[]
      it_outtab            = it_itab[]
      it_sort              = it_sort[].

ENDFORM.                    " assign_itab_to_alv

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

                                 'S' 'LIFNR'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Vendor',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'MATNR'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Material',
                                  'E' 'OUTPUTLEN'   '18',

                                 'S' 'ZTSEQ'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Seq No',
                                  'E' 'OUTPUTLEN'   '6',

                                  'S' 'EXCPRICE'       ' ',
                                  ' ' 'COLTEXT'     'New Price',
                                  ' ' 'DECIMALS_O'  '2',
                                  ' ' 'NO_ZERO'     'X',
                                  ' ' 'DATATYPE'    'CURR',
                                  'E' 'OUTPUTLEN'   '13',

                                  'S' 'EXCFDATE'    ' ',
                                  ' ' 'COLTEXT'     'Date From',
                                  ' ' 'DATATYPE'    'DATS',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'EXCTDATE'    ' ',
                                  ' ' 'COLTEXT'     'Date To',
                                  ' ' 'DATATYPE'    'DATS',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'EBELN'       ' ',
                                  ' ' 'COLTEXT'     'SA Number',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'EBELP'       ' ',
                                  ' ' 'COLTEXT'     'I/No',
                                  'E' 'OUTPUTLEN'   '3',

                                  'S' 'SAFDATE'    ' ',
                                  ' ' 'COLTEXT'     'SA Date From',
                                  ' ' 'DATATYPE'    'DATS',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'SATDATE'    ' ',
                                  ' ' 'COLTEXT'     'SA Date To',
                                  ' ' 'DATATYPE'    'DATS',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'DELETED'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Del',
                                  'E' 'OUTPUTLEN'   '3',

                                  'S' 'GRPRICE'       ' ',
                                  ' ' 'COLTEXT'     'Old Avg Price',
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

                                 'S' 'GRAMT'       ' ',
                                  ' ' 'COLTEXT'     'Old Payment',
                                  ' ' 'DECIMALS_O'  '2',
                                  ' ' 'NO_ZERO'     'X',
                                  ' ' 'DATATYPE'    'CURR',
*                                  ' ' 'DO_SUM'      'X',
                                  'E' 'OUTPUTLEN'   '13',


                                 'S' 'NEWAMT'       ' ',
                                  ' ' 'COLTEXT'     'New Payment',
                                  ' ' 'DECIMALS_O'  '2',
                                  ' ' 'NO_ZERO'     'X',
                                  ' ' 'DATATYPE'    'CURR',
*                                  ' ' 'DO_SUM'      'X',
                                  'E' 'OUTPUTLEN'   '13',

                                  'S' 'CAL_AMT'       ' ',
                                  ' ' 'COLTEXT'     'Cal.Amt',
                                  ' ' 'DECIMALS_O'  '2',
                                  ' ' 'NO_ZERO'     'X',
                                  ' ' 'DATATYPE'    'CURR',
                                  ' ' 'DO_SUM'      'X',
                                  'E' 'OUTPUTLEN'   '14',

                                  'S' 'ERROR_AMT'       ' ',
                                  ' ' 'COLTEXT'     '(Cal-Reval) Amt',
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
  CASE ok_code.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
*    WHEN 'DOWNLOAD'.
*      PERFORM .
    WHEN 'PRINT'.
      PERFORM call_zrfig02.
    WHEN 'REEVAL'.
      PERFORM re_eval.
    WHEN 'SAUPDATE'.
      PERFORM update_sa USING w_error.
    WHEN 'DE-AGGRE'.
      PERFORM de_aggregate.
    WHEN 'DISP-SA'.
      PERFORM display_sa.
    WHEN 'INFOREC'.
      PERFORM display_info.
    WHEN '%_GC'.
      PERFORM double_click_rtn.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0200  INPUT
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
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.

  REFRESH: r_xblnr.
  r_xblnr-option = 'EQ'.
  r_xblnr-sign = 'I'.

  LOOP AT lt_rows.

    READ TABLE it_pri INDEX lt_rows-index.

*    SELECT SINGLE reval_msg zbdat
*      INTO (l_xblnr, l_zbdat)
*      FROM ztmm_reval_log
*             WHERE matnr = it_pri-matnr
*                 AND lifnr = it_pri-lifnr
*                 AND pum_n = it_pri-pum_n
*                 AND ztseq = it_pri-ztseq.
*    IF sy-subrc = 0.
*      r_xblnr-low = l_xblnr.
*      APPEND r_xblnr.
*    ENDIF.

    SELECT reval_msg zbdat
        INTO (l_xblnr, l_zbdat)
        FROM ztmm_reval_log
               WHERE matnr = it_pri-matnr
                   AND lifnr = it_pri-lifnr
                   AND pum_n = it_pri-pum_n
                   AND ztseq = it_pri-ztseq.
      IF sy-subrc = 0.
        r_xblnr-low = l_xblnr.
        APPEND r_xblnr.
      ENDIF.
    ENDSELECT.

  ENDLOOP.

  l_year = l_zbdat+0(4).

*  SET PARAMETER ID 'BUK' FIELD l_bukrs.
*  SET PARAMETER ID 'REV' FIELD R_XBLNR.
*  CALL TRANSACTION 'ZRFIG02' AND SKIP FIRST SCREEN.

** Furong on 06/03/14
*  PERFORM call_tcode_zrfig02 USING l_bukrs l_year.

  REFRESH: r_gjahr, r_cpudt.

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
** End
ENDFORM.                    " CALL_ZRFIG02
*&--------------------------------------------------------------------*
*&      Form  RE_EVAL
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM re_eval.
  DATA: BEGIN OF lt_ekbe OCCURS 0,
    belnr LIKE ekbe-belnr,
    cpudt LIKE ekbe-cpudt,
    cputm LIKE ekbe-cputm,
    END OF lt_ekbe.

  DATA : BEGIN OF lt_reval OCCURS 0,
       matnr LIKE ekpo-matnr,
       ebeln LIKE ekpo-ebeln,
       ebelp LIKE ekpo-ebelp,
       excfdate LIKE it_itab-excfdate,
       exctdate LIKE it_itab-exctdate,
       END OF lt_reval.

  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i,
        l_reval_index LIKE sy-tabix,
        l_ebeln LIKE it_itab-ebeln,
        l_ebelp LIKE it_itab-ebelp,
        l_update LIKE it_itab-reval_update,
        l_msg LIKE it_itab-msg,
        l_curr_matnr LIKE it_itab-matnr,
        l_pre_matnr LIKE it_itab-matnr.

  RANGES: lr_wedat FOR sy-datum.

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

  REFRESH: lt_reval, it_reval_log.
  CLEAR: l_curr_matnr, l_pre_matnr.
  LOOP AT lt_rows.

    READ TABLE it_itab INDEX lt_rows-index.
    IF it_itab-matnr = ' '.
      MESSAGE e000(zz) WITH text-m13.
    ENDIF.

    IF it_itab-reval_update = 'X'.
      MESSAGE e000(zz) WITH text-m14.
    ENDIF.

    IF it_itab-sa_update IS INITIAL.
      MESSAGE e000(zz) WITH text-m17.
    ENDIF.

    READ TABLE lt_reval WITH KEY
               matnr = it_itab-matnr
               ebeln = it_itab-ebeln
               ebelp = it_itab-ebelp
               excfdate = it_itab-excfdate
               exctdate = it_itab-exctdate.
    .
    IF sy-subrc <> 0.
      l_pre_matnr = l_curr_matnr.
      l_curr_matnr = it_itab-matnr.

      lt_reval-matnr = it_itab-matnr.
      lt_reval-ebeln = it_itab-ebeln.
      lt_reval-ebelp = it_itab-ebelp.
      lt_reval-excfdate = it_itab-excfdate.
      lt_reval-exctdate = it_itab-exctdate.

      APPEND lt_reval.

      lr_wedat-sign = 'I'.
      lr_wedat-option = 'BT'.
      lr_wedat-low = it_itab-excfdate.
      lr_wedat-high = it_itab-exctdate.
      APPEND lr_wedat.

      SUBMIT rmmr1mrb    "via selection-screen
             WITH pa_ebeln = it_itab-ebeln
             WITH pa_ebelp = it_itab-ebelp
             WITH so_wedat IN lr_wedat
             WITH pa_nbwdt = p_pdate
             WITH pa_zterm = 'P030'
             WITH pa_guzte  = 'P030'
             WITH pa_xtest = ' '   "p_test
             AND RETURN.

      DO 60 TIMES.
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
          WAIT UP TO '1' SECONDS.
        ENDIF.
      ENDDO.

      SORT lt_ekbe DESCENDING BY cpudt cputm.
      READ TABLE lt_ekbe INDEX 1.
*      it_reval_rsn-belnr = lt_ekbe-belnr.

      IF sy-subrc = 0.
        READ TABLE it_pri WITH KEY matnr = it_itab-matnr
                                  lifnr = it_itab-lifnr
                                  pum_n = it_itab-pum_n
                                  ztseq = it_itab-ztseq.
        IF ( sy-subrc = 0 AND it_pri-reval_update IS INITIAL )
            OR ( sy-subrc = 0 AND l_pre_matnr = l_curr_matnr ).
          l_reval_index = sy-tabix.
          it_pri-reval_update = 'X'.
          it_pri-reval_date = sy-datum.
          it_pri-reval_user = sy-uname.
          it_pri-belnr = lt_ekbe-belnr.
          MODIFY it_pri INDEX l_reval_index TRANSPORTING reval_update
                reval_user reval_date belnr.
*          IF p_test IS INITIAL.
          UPDATE ztmm_pri_reval
          SET: reval_update = 'X'
               reval_date = sy-datum
               reval_user = sy-uname
               belnr = it_pri-belnr
               inv_amt = it_pri-inv_amt
               WHERE matnr = it_itab-matnr
               AND lifnr = it_itab-lifnr
               AND pum_n = it_itab-pum_n
               AND ztseq = it_itab-ztseq
               AND belnr = ' '.

          IF sy-subrc = 0.
            COMMIT WORK.
          ELSE.
            ROLLBACK WORK.
            MESSAGE s000(zz) WITH text-m16 DISPLAY LIKE 'E'..
          ENDIF.
          PERFORM update_log_reval USING lt_ekbe-belnr.
*          ENDIF.
        ENDIF.
        it_itab-reval_update = 'S'.
        CONCATENATE 'Doc No:' lt_ekbe-belnr INTO it_itab-msg
          SEPARATED BY space.
      ELSE.
        it_pri-reval_update = ' '.
        READ TABLE it_pri WITH KEY matnr = it_itab-matnr
                                  lifnr = it_itab-lifnr
                                  pum_n = it_itab-pum_n
                                  ztseq = it_itab-ztseq.
        IF sy-subrc = 0 AND  it_pri-reval_update = 'X'.
          l_reval_index = sy-tabix.
          it_pri-reval_update = ' '.
          it_pri-reval_date = '00000000'.
          it_pri-reval_user = ' '.
          MODIFY it_pri INDEX l_reval_index TRANSPORTING reval_update
                reval_user reval_date.
*          IF p_test IS INITIAL.
          UPDATE ztmm_pri_reval
          SET: reval_update = ' '
               reval_date = '00000000'
               reval_user = ' '
                 WHERE matnr = it_itab-matnr
               AND lifnr = it_itab-lifnr
               AND pum_n = it_itab-pum_n
               AND ztseq = it_itab-ztseq..
          COMMIT WORK.
          it_error = it_pri.
          APPEND it_error.
        ENDIF.
        it_itab-reval_update = 'E'.
        it_itab-msg = 'Error in re_val'.
      ENDIF.
      MODIFY it_itab INDEX lt_rows-index
            TRANSPORTING reval_update msg.
*      CLEAR: it_reval_rsn.
      REFRESH: lr_wedat.
    ENDIF.
  ENDLOOP.

  CLEAR: l_ebeln, l_ebelp, l_update,l_msg.
  LOOP AT it_itab.
    IF l_ebeln = it_itab-ebeln AND
       l_ebelp = it_itab-ebelp.
      it_itab-reval_update = l_update.
      it_itab-msg = l_msg.
      MODIFY it_itab.
    ELSE.
      l_ebeln = it_itab-ebeln.
      l_ebelp = it_itab-ebelp.
      l_update = it_itab-reval_update.
      l_msg = it_itab-msg.
    ENDIF.

  ENDLOOP.

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

  DATA: l_field01(20),
*        l_datab(10),
*        l_datbi(10),
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
        l_text(80).

  DATA: l_result(1),
        l_messa(255).

  LOOP AT it_itab.
    l_index = sy-tabix.

    CLEAR : it_bdc, it_bdc[], it_mess, it_mess[], l_field01,
           l_datab, l_datbi, l_kbetr,l_flag.

    IF it_itab-ebeln IS INITIAL.
      CLEAR: it_itab.
      CONTINUE.
    ENDIF.

*    l_ebeln = it_itab-ebeln.
*    l_index = sy-tabix.

*    LOOP AT it_itab FROM l_index WHERE ebeln = l_ebeln.

    IF it_itab-sa_update IS NOT INITIAL.
      CLEAR: it_itab.
      CONCATENATE 'SA Updated already'
        it_itab-matnr it_itab-ebeln INTO l_text
        SEPARATED BY space.
      MESSAGE w999 WITH l_text.
      CONTINUE.
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
      it_itab-msg = 'Error in SA Update'.
      l_result = 'E'.
      PERFORM update_log_sa USING l_result CHANGING l_messa.
    ELSE.
      READ TABLE it_mess WITH KEY msgtyp = 'S'.
      IF sy-subrc EQ 0.
        it_itab-sa_update = 'X'.
        it_itab-msg = 'Successfully updated'.
        l_result = 'S'.
        PERFORM update_log_sa USING l_result CHANGING l_messa.
      ELSE.
        it_itab-sa_update = ' '.
        it_itab-msg = it_mess-msgv1.
        l_result = 'E'.
        PERFORM update_log_sa USING l_result CHANGING l_messa.
      ENDIF.
    ENDIF.

    MODIFY it_itab INDEX l_index   "lt_rows-index
            TRANSPORTING sa_update msg.

  ENDLOOP.
*  IF l_flag IS INITIAL.
*    MESSAGE i999 WITH 'Successfully updated '.
*  ENDIF.

  CLEAR: l_ebeln, l_flag.
  LOOP AT it_itab.

    IF it_itab-sa_update IS INITIAL.
      l_flag = 'X'.
    ENDIF.
    IF l_ebeln = it_itab-ebeln.
      CONTINUE.
    ELSE.
      IF NOT l_ebeln IS INITIAL.
        IF l_flag IS INITIAL.
          PERFORM set_sa_update USING wa_itab.
        ENDIF.
        CLEAR: l_flag.
      ENDIF.
      l_ebeln = it_itab-ebeln.
    ENDIF.
    wa_itab = it_itab.
  ENDLOOP.
  IF l_flag IS INITIAL.
    PERFORM set_sa_update USING wa_itab.
  ENDIF.

  PERFORM update_reval_sa_log USING l_result l_messa.

ENDFORM.                    " UPDATE_SA
*&--------------------------------------------------------------------*
*&      Form  double_click_rtn
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM double_click_rtn.

  DATA: lw_field(40),
        lw_line TYPE i..

  CHECK NOT it_itab IS INITIAL.

  GET CURSOR FIELD lw_field.
  GET CURSOR FIELD lw_field LINE lw_line.

  IF lw_field = 'IT_ITAB-GRPRICE'.
    READ TABLE it_itab INDEX lw_line.

    r_matnr-sign  = 'I'.
    r_matnr-option  = 'EQ'.
    r_matnr-low  = it_itab-matnr.
    APPEND r_matnr.

    r_refdt-sign  = 'I'.
    r_refdt-option  = 'BT'.
    r_refdt-low  = it_itab-excfdate.
    r_refdt-high = it_itab-exctdate.
    APPEND r_refdt.

    r_ebeln-sign  = 'I'.
    r_ebeln-option  = 'EQ'.
    r_ebeln-low  = it_itab-ebeln.
    APPEND r_ebeln.


    SUBMIT zmmr_sa_reval_get_inv                            "ZRFI013
              WITH p_bukrs  = 'H201'
              WITH s_lifnr IN r_lifnr
              WITH s_matnr IN r_matnr
              WITH s_budat IN r_refdt
              WITH s_ebeln IN r_ebeln
              EXPORTING LIST TO MEMORY
              AND RETURN.
  ELSE.
  ENDIF.
ENDFORM.                    " double_click_rtn
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
  DATA: l_line TYPE i.
  RANGES: r_ebeln FOR ekko-ebeln,
          r_werks FOR ekpo-werks.

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

  LOOP AT lt_rows.
    READ TABLE it_itab INDEX lt_rows-index.
    IF it_itab-matnr = ' '.
      MESSAGE e000(zz) WITH text-m13.
    ENDIF.

    r_ebeln-sign = 'I'.
    r_ebeln-option = 'EQ'.
    r_ebeln-low = it_itab-ebeln.
    APPEND r_ebeln.

  ENDLOOP.

  r_werks-sign = 'I'.
  r_werks-option = 'EQ'.
  r_werks-low = 'P001'..
  APPEND r_werks.

  SUBMIT z_ekbeaufl    "via selection-screen
         WITH s_werks IN r_werks
         WITH s_ebeln IN r_ebeln
         WITH update = 'X'
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

  LOOP AT it_itab.
    CHECK it_itab-gramt <> 0.
    r_ebeln-sign = 'I'.
    r_ebeln-option = 'EQ'.
    r_ebeln-low = it_itab-ebeln.
    COLLECT r_ebeln.
  ENDLOOP.

  DESCRIBE TABLE r_ebeln LINES l_cn.
  IF l_cn = 0.
    p_error = 'X'.
  ELSE.
    CLEAR: p_error.
    r_werks-sign = 'I'.
    r_werks-option = 'EQ'.
    r_werks-low = 'P001'..
    APPEND r_werks.

    SUBMIT z_ekbeaufl    "via selection-screen
           WITH s_werks IN r_werks
           WITH s_ebeln IN r_ebeln
           WITH update = 'X'
           AND RETURN.
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
FORM check_reason_code.
  DATA: l_kzust LIKE t686c-kzust.
  CLEAR: w_error.
  LOOP AT it_pri.
    SELECT SINGLE kzust INTO l_kzust
      FROM  t686c
      WHERE kzust = it_pri-resn_c.
    IF sy-subrc = 0.
    ELSE.
      it_pri-zmsg = 'Reason Code not found: '.
      MODIFY it_pri.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " check_reason_code
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
*
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
FORM get_data .

  w_rundate = sy-datum.
  w_runtime = sy-uzeit.
  IF p_status = 'A'.
    SELECT * INTO TABLE it_pri
    FROM ztmm_pri_reval
    WHERE matnr IN s_matnr
      AND lifnr IN s_lifnr
      AND purch_g IN s_ekgrp
      AND pum_n IN s_pum_n
      AND zbdat IN s_zbdat
      AND zresult <> 'E'.
  ELSE.
    SELECT * INTO TABLE it_pri
      FROM ztmm_pri_reval
      WHERE matnr IN s_matnr
        AND lifnr IN s_lifnr
        AND purch_g IN s_ekgrp
        AND pum_n IN s_pum_n
        AND zbdat IN s_zbdat
        AND reval_update = p_status
        AND zresult <> 'E'.
  ENDIF.
  IF sy-subrc = 0.
    PERFORM check_reason_code.
  ENDIF.
ENDFORM.                    " GET_DATA
*&--------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_0100  OUTPUT
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
MODULE display_alv_0200 OUTPUT.
  IF grid_container IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM create_container_n_object.
    PERFORM set_attributes_alv_grid.
    PERFORM build_sortcat_display.
    PERFORM build_field_catalog USING 'IT_ITAB'.
    PERFORM assign_itab_to_alv.
*    PERFORM sssign_event_9000.
  ELSE.
    CALL METHOD alv_grid->refresh_table_display.
  ENDIF.
ENDMODULE.                 " DISPLAY_ALV_0100  OUTPUT
*&--------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_N_OBJECT_100
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM create_container_n_object_100 .
  DATA: w_repid LIKE sy-repid.
  CREATE OBJECT grid_container_100
    EXPORTING
      container_name              = wa_custom_control_100
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.
  w_repid = sy-repid.
  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = w_repid
        txt2  = sy-subrc
        txt1  = 'The control can not be created'.
  ENDIF.
  CREATE OBJECT alv_grid_100
    EXPORTING
      i_parent      = grid_container_100
      i_appl_events = 'X'.
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
         l_cn(2) TYPE n.

  CLEAR: it_fieldcat_100,  it_fieldcat_100[],
         it_fieldname_100, it_fieldname_100[].
  CLEAR: w_cnt,w_repid.

  lw_itab = p_itab.

  w_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = w_repid
      i_internal_tabname = lw_itab
      i_structure_name   = 'ZTMM_PRI_REVAL'
*     i_inclname         = w_repid
    CHANGING
      ct_fieldcat        = it_fieldname_100.

  PERFORM setting_fieldcat_100 TABLES it_fieldcat_100 USING :

*                                  'S' 'CHK'       ' ',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'COLTEXT'     'Chk',
**                                  ' ' 'SELTEXT'   'Checkbox',
*                                  ' ' 'CHECKBOX'  'X',
*                                  ' ' 'EDIT'      'X',
*                                  'E' 'OUTPUTLEN' '1',

                                  'S' 'PUM_N'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Approval No',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'MATNR'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Material',
                                  'E' 'OUTPUTLEN'   '18',

                                 'S' 'LIFNR'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Vendor',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'ZTSEQ'       ' ',
                                  ' ' 'COLTEXT'     'Serial No',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'RESN_C'    ' ',
                                  ' ' 'COLTEXT'     'Reason Code',
                                  'E' 'OUTPUTLEN'   '10',

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

                                  'S' 'PURCH_G'       ' ',
                                  ' ' 'COLTEXT'     'Pur Grp',
                                  'E' 'OUTPUTLEN'   '6',

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
                                  'E' 'OUTPUTLEN'   '12',

                                 'S' 'INV_AMT'       ' ',
                                  ' ' 'COLTEXT'     'Reval Amt',
                                  ' ' 'NO_ZERO'     'X',
                                  'E' 'OUTPUTLEN'   '12'.


*                                  'S' 'ZMSG'    ' ',
*                                  ' ' 'COLTEXT'     'Message',
*                                  'E' 'OUTPUTLEN'   '100'.

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
  IF grid_container_100 IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM create_container_n_object_100.
    PERFORM set_attributes_alv_grid.
    PERFORM build_field_catalog_100 USING 'IT_PRI'.
    PERFORM assign_itab_to_alv_100.
*    PERFORM sssign_event_9000.
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
      is_layout       = wa_is_layout
      i_save          = wa_save
      is_variant      = wa_variant
    CHANGING
      it_fieldcatalog = it_fieldcat_100[]
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
    WHEN 'PREV' OR  'REEVA'.
      PERFORM preview.
    WHEN 'ONESTEP'.
      PERFORM re_evaluation.
    WHEN 'PRINT'.
      PERFORM call_zrfig02.
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
  MOVE 'Following items have error in Re_val process:' TO lt_body.
  APPEND lt_body.
  CLEAR: lt_body.
  MOVE '=================================' TO lt_body.
  APPEND lt_body.
  CLEAR: lt_body.

  MOVE: 'Approval No' TO lt_body+0(10),
        'Material No' TO lt_body+10(20),
        'Vendor' TO lt_body+30(10),
        'Approval No' TO lt_body+40(10)..

  APPEND lt_body.
  CLEAR: lt_body.

  MOVE: '--------------------' TO  lt_body+0(20),
        '------------------------------' TO  lt_body+20(30).
  APPEND lt_body.
  CLEAR: lt_body.

  LOOP AT it_error.

    MOVE: it_error-pum_n TO lt_body+0(10),
          it_error-matnr TO lt_body+10(20),
          it_error-lifnr TO lt_body+30(10),
          it_error-pum_n TO lt_body+40(10).

    APPEND lt_body.
  ENDLOOP.

  CALL FUNCTION 'ZCAF_SEND_EMAIL'
    EXPORTING
      p_subject  = 'Re_evaluation Erro List'
      p_rec_type = 'C'
      p_receiver = p_rver
    TABLES
      pt_body    = lt_body.

ENDFORM.                    " SEND_EMAIL
*&--------------------------------------------------------------------*
*&      Form  UPDATE_LOG
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->P_3336   text
*---------------------------------------------------------------------*
FORM update_log_sa USING p_result CHANGING p_messa  .

  REFRESH: it_log.
  CLEAR: it_log.
  IF p_result = 'S'.
    p_messa = 'Successfully Updated'.
  ELSE.
    PERFORM get_message USING    it_mess-msgid
                                    it_mess-msgnr
                                    it_mess-msgv1
                                    it_mess-msgv2
                                    it_mess-msgv3
                                    it_mess-msgv4
                           CHANGING p_messa.
  ENDIF.

** 089/03/13
*  MOVE-CORRESPONDING it_itab TO it_log.
*  READ TABLE it_itab_onestep with KEY matnr = it_itab-matnr
*                                      lifnr = it_itab-lifnr
*                                      pum_n = it_itab-pum_n
*                                      ztseq = it_itab-ztseq
*                                      safdate = it_itab-safdate
*                                      satdate = it_itab-satdate.
*  if sy-subrc = 0.
*    MOVE-CORRESPONDING it_itab_onestep to it_log.
*    it_log-sa_to = it_itab_onestep-satdate.
*    it_log-sa_from = it_itab_onestep-safdate.
*    it_log-oldprice = it_itab_onestep-saprice.
*    it_log-newprice = it_itab_onestep-excprice.
*  ELSE.
  MOVE-CORRESPONDING it_itab TO it_log.
  it_log-sa_to = it_itab-satdate.
  it_log-sa_from = it_itab-safdate.
  it_log-oldprice = it_itab-saprice.
  it_log-newprice = it_itab-excprice.
*  endif.
* End on 09/03/13
  READ TABLE it_pri WITH KEY matnr = it_itab-matnr
                                 lifnr = it_itab-lifnr
                                 pum_n = it_itab-pum_n
                                 ztseq = it_itab-ztseq.
  IF sy-subrc = 0.
    MOVE-CORRESPONDING it_pri TO it_log.
  ENDIF.

  it_log-sa_user = sy-uname.
  it_log-sa_update = p_result.
  it_log-sa_msg = p_messa.

  it_log-run_date = w_rundate.
  it_log-run_time = w_runtime.

  APPEND it_log.

  INSERT ztmm_reval_log FROM TABLE it_log
    ACCEPTING DUPLICATE KEYS.
  IF sy-subrc EQ 0  OR sy-subrc EQ 4 .
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
    MESSAGE s999 WITH 'Error in adding to log table for'
       it_log-pum_n.
  ENDIF.
ENDFORM.                    " UPDATE_LOG
*&--------------------------------------------------------------------*
*&      Form  UPDATE_LOG_REVAL
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->P_LT_EKBE_BELNR  text
*---------------------------------------------------------------------*
FORM update_log_reval USING p_belnr.
*  CLEAR: it_log.
*  MOVE-CORRESPONDING it_pri TO it_log.
*  MOVE-CORRESPONDING it_itab TO it_log.
*  it_log-reval_user = sy-uname.
*  it_log-reval_update = 'X'.
*  it_log-reval_msg = 'Sucessfully Updated'.
*  it_log-zbdat = p_pdate.
*  it_log-reval_msg = p_belnr.
*  it_reval_log = it_log.
*  APPEND it_reval_log.

  UPDATE ztmm_reval_log
       SET: reval_user = sy-uname
            reval_update = 'X'
            reval_msg = p_belnr
            reval_date = sy-datum
            zbdat = p_pdate
         WHERE matnr = it_itab-matnr
               AND lifnr = it_itab-lifnr
               AND pum_n = it_itab-pum_n
               AND ztseq = it_itab-ztseq
               AND ebeln = it_itab-ebeln
               AND ebelp = it_itab-ebelp
               AND reval_msg = ' '.
  IF sy-subrc EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
    MESSAGE s999 WITH 'Error in updating log table for'
           it_log-pum_n
                 DISPLAY LIKE 'E'
.
  ENDIF.

  UPDATE ztmm_revalsa_log
       SET: reval_user = sy-uname
            reval_update = 'X'
            reval_msg = p_belnr
            reval_date = sy-datum
            zbdat = p_pdate
         WHERE matnr = it_itab-matnr
               AND lifnr = it_itab-lifnr
               AND pum_n = it_itab-pum_n
               AND ztseq = it_itab-ztseq
               AND ebeln = it_itab-ebeln
               AND ebelp = it_itab-ebelp
               AND reval_msg = ' '.
  IF sy-subrc EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
    MESSAGE s999 WITH 'Error in updating SA log table for'
       it_log-pum_n DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.                    " UPDATE_LOG_REVAL
*&--------------------------------------------------------------------*
*&      Form  PREVIEW
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM preview .
  PERFORM get_select_data.
  PERFORM get_scheduling_agreement.
  PERFORM make_data.
  IF  w_code = 'REEVA'.
    CALL SCREEN 200.
  ELSE.
    CALL SCREEN 300.
  ENDIF.
ENDFORM.                    " PREVIEW
*&--------------------------------------------------------------------*
*&      Form  GET_SELECT_DATA
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM get_select_data .
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i.

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

  REFRESH: it_pri_selected.
  IF lt_rows[] IS INITIAL.
    MESSAGE e001.
  ENDIF.

  LOOP AT lt_rows.
    CLEAR: it_pri_selected.
    READ TABLE it_pri INDEX lt_rows-index.
    MOVE it_pri TO it_pri_selected.
    APPEND it_pri_selected.
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
  DATA: ls_stable TYPE lvc_s_stbl.
  ls_stable-row = 'X'.
  ls_stable-col = 'X'.
  CALL METHOD alv_grid_100->refresh_table_display
    EXPORTING
      is_stable = ls_stable.
*      i_soft_refresh = 'X'.
ENDFORM.                    " REFRESH
*&--------------------------------------------------------------------*
*&      Form  SET_SA_UPDATE
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM set_sa_update USING pa_itab STRUCTURE it_itab.
  DATA:  l_sa_index LIKE sy-tabix.

  READ TABLE it_pri WITH KEY matnr = pa_itab-matnr
                               lifnr = pa_itab-lifnr
                                  pum_n = pa_itab-pum_n
                                  ztseq = pa_itab-ztseq.
  IF sy-subrc = 0 AND it_pri-sa_update IS INITIAL.
    l_sa_index = sy-tabix.
    it_pri-sa_update = 'X'.
    it_pri-sa_date = sy-datum.
    it_pri-sa_user = sy-uname.
    MODIFY it_pri INDEX l_sa_index
      TRANSPORTING sa_update sa_date sa_user.

    UPDATE ztmm_pri_reval
    SET: sa_update = 'X'
         sa_date = sy-datum
         sa_user = sy-uname
         WHERE matnr = pa_itab-matnr
           AND lifnr = pa_itab-lifnr
           AND pum_n = pa_itab-pum_n
           AND ztseq = pa_itab-ztseq.
*          IF sy-subrc = 0.
    COMMIT WORK.
*          ELSE.
*            ROLLBACK WORK.
*            it_itab-msg = 'Error in updating ztmm_pri_reval'.
*          ENDIF.
  ENDIF.


ENDFORM.                    " SET_SA_UPDATE
*&--------------------------------------------------------------------*
*&      Form  RE_EVALUATION
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM re_evaluation .
  REFRESH it_error.
  DATA: l_answer(1),
        l_error(1).

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question  = 'Are you sure to process?'
    IMPORTING
      answer         = l_answer
    EXCEPTIONS
      text_not_found = 1
      OTHERS         = 2.

  IF l_answer = '1'.
    REFRESH: it_sa_all_onestep,it_itab_onestep.

    PERFORM get_select_data.
**
    CLEAR: l_error.
    LOOP AT it_pri_selected.
      IF it_pri_selected-reval_update = 'X'.
        CALL FUNCTION 'POPUP_TO_INFORM'
          EXPORTING
            titel = 'Re_evaluation is done'
            txt1  = it_pri_selected-matnr
            txt2  = it_pri_selected-pum_n
            txt3  = it_pri_selected-ztseq
            txt4  = it_pri_selected-lifnr.
        EXIT.
        l_error = 'X'.
      ENDIF.
    ENDLOOP.
    IF l_error IS NOT INITIAL.
      EXIT.
    ENDIF.

    PERFORM get_scheduling_agreement.
    PERFORM make_data.
    it_sa_all_onestep[] = it_sa_all[].
    it_itab_onestep[] = it_itab[].

    SORT it_sa_all_onestep BY matnr lifnr.
    SORT it_itab_onestep BY  matnr lifnr pum_n ztseq.
*         safdate satdate.

    REFRESH: it_sa_all, it_itab.
*    perform get_all_selected_sa.

    LOOP AT it_pri_selected.
      PERFORM get_sa.
      IF it_sa_all[] IS INITIAL.
        MESSAGE s999 WITH 'No SA existed for '
            it_pri_selected-matnr DISPLAY LIKE 'E'.
        CONTINUE.
      ENDIF.
      CLEAR: l_error.
      PERFORM get_inv USING l_error.
      IF l_error = 'X'.
        MESSAGE s999 WITH 'No invoice existed for '
           it_pri_selected-matnr DISPLAY LIKE 'E'.
        CONTINUE.
      ENDIF.
      CLEAR: l_error.
      MESSAGE s999 WITH 'Processing' it_pri_selected-matnr
        ' ' it_pri_selected-ztseq.
      PERFORM update_sa USING l_error.
      IF l_error IS INITIAL.
        PERFORM de_aggregate_onestep USING l_error.
        IF l_error IS INITIAL.
          PERFORM re_eval_auto.
        ENDIF.
      ELSE.
        MESSAGE s999 WITH 'Error in updating SA' it_pri-matnr
           DISPLAY LIKE 'E'.
      ENDIF.
    ENDLOOP.
    IF it_error[] IS NOT INITIAL AND p_rver IS NOT INITIAL.
      PERFORM send_email.
    ENDIF.
  ENDIF.
*  CALL SCREEN 200.
ENDFORM.                    " RE_EVALUATION
*&-------------------------------------------------------------------*
*&      Form  GET_SA
*&-------------------------------------------------------------------*
*       text
*--------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*--------------------------------------------------------------------*
FORM get_sa .
  CLEAR: it_sa_all, it_sa_all[].

*  loop at it_sa_all_onestep WHERE matnr = it_pri_selected-matnr
*                              and lifnr = it_pri_selected-lifnr.
*
*    it_sa_all = it_sa_all_onestep.
*    append it_sa_all.
*  endloop.
  SELECT a~ebeln ebelp matnr werks lgort lifnr
         peinh a~bstyp a~bukrs bsart ekorg
         ekgrp kdatb kdate b~loekz
               INTO CORRESPONDING FIELDS OF TABLE it_sa_all
               FROM ekko AS a INNER JOIN ekpo AS b
                 ON a~mandt EQ b~mandt
                AND a~ebeln EQ b~ebeln
                WHERE a~bstyp EQ 'L'
                 AND b~matnr = it_pri_selected-matnr
                 AND a~lifnr = it_pri_selected-lifnr
                 AND b~loekz = ' '..
ENDFORM.                    " GET_SA
*&-------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&-------------------------------------------------------------------*
*       text
*--------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*--------------------------------------------------------------------*
FORM get_inv USING p_error..
  REFRESH it_itab.

*  loop at it_itab_onestep WHERE pum_n = it_pri_selected-pum_n
*                            and ztseq = it_pri_selected-ztseq
*                            and matnr = it_pri_selected-matnr
*                            and lifnr = it_pri_selected-lifnr.
*
*    it_itab = it_itab_onestep.
*    append it_itab.
*  endloop.

  DATA: l_flag(1).
  DATA: l_datab LIKE a018-datab,
         l_grqty LIKE ekbe-menge,
         l_gramt LIKE ekbe-dmbtr,
         l_price LIKE ekbe-dmbtr,
         l_newamt LIKE ekbe-dmbtr,
         l_diff LIKE ekbe-dmbtr,
         l_recno TYPE i.

  DATA: lt_a016 LIKE TABLE OF a016 WITH HEADER LINE,
        lt_itab LIKE TABLE OF it_itab WITH HEADER LINE,
        lt_ekbe LIKE TABLE OF ekbe WITH HEADER LINE.

  DATA: l_inv LIKE it_itab-gramt.
  REFRESH it_itab.
  SORT it_sa_all BY matnr lifnr.

  CLEAR: l_flag.

  lt_itab-matnr = it_pri_selected-matnr.
  lt_itab-lifnr = it_pri_selected-lifnr.
  lt_itab-pum_n = it_pri_selected-pum_n.
  lt_itab-ztseq = it_pri_selected-ztseq.
  lt_itab-sa_update = it_pri_selected-sa_update.
  lt_itab-reval_update = it_pri_selected-reval_update.
  lt_itab-excprice = it_pri_selected-term_price.
  lt_itab-excfdate = it_pri_selected-term_from.
  lt_itab-exctdate = it_pri_selected-term_to.
  lt_itab-rsn = it_pri-resn_c.

  LOOP AT it_sa_all.
    SELECT * INTO TABLE lt_a016
                   FROM a016
                  WHERE kappl EQ 'M'
                    AND kschl EQ 'PB00'
                    AND evrtn EQ it_sa_all-ebeln
                    AND evrtp EQ it_sa_all-ebelp
                    AND datab <= lt_itab-exctdate
                    AND datbi >= lt_itab-excfdate.

    LOOP AT lt_a016.
      SELECT SINGLE kbetr INTO lt_itab-saprice
        FROM konp
        WHERE knumh = lt_a016-knumh
          AND kschl = 'PB00'.
      lt_itab-ebeln = it_sa_all-ebeln.
      lt_itab-ebelp = it_sa_all-ebelp.
      lt_itab-peinh = it_sa_all-peinh.
      lt_itab-safdate = lt_a016-datab.
      lt_itab-satdate = lt_a016-datbi.
      lt_itab-deleted = it_sa_all-loekz.
      APPEND lt_itab.
      l_flag = 'X'.
    ENDLOOP.
    CLEAR: it_sa_all, lt_a016[], lt_a016.
  ENDLOOP.
  IF l_flag IS INITIAL.
    APPEND lt_itab.
  ENDIF.

  LOOP AT lt_itab.
    REFRESH: r_matnr, r_refdt, r_ebeln, r_ebelp,r_lifnr.

    it_itab = lt_itab.
    IF NOT lt_itab-ebeln IS INITIAL.

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

      APPEND r_refdt.

      r_lifnr-sign  = 'I'.
      r_lifnr-option  = 'EQ'.
      r_lifnr-low  = lt_itab-lifnr.
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

      SUBMIT zmmr_sa_reval_get_inv                      "ZRFI013
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
      IF l_recno = 0.
        CONTINUE.
      ENDIF.

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
    ENDIF.
    APPEND it_itab.

  ENDLOOP.
  LOOP AT it_itab.
*    AT END OF MATNR.
*       SUM.
*    ENDAT.
*    IF IT_ITAB-GRAMT = 0.
*       P_ERROR = 'X'.
*    ELSE.
*      CLEAR P_ERROR.
*    ENDIF.
    l_inv = l_inv + it_itab-gramt.
  ENDLOOP.
  IF l_inv = 0.
    p_error = 'X'.
  ELSE.
    CLEAR p_error.
  ENDIF.
  SORT it_itab BY pum_n lifnr matnr ebeln.
ENDFORM.                    " PROCESS_DATA
*&--------------------------------------------------------------------*
*&      Form  RE_EVAL_AUTO
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM re_eval_auto .
  DATA: BEGIN OF lt_ekbe OCCURS 0,
    belnr LIKE ekbe-belnr,
    cpudt LIKE ekbe-cpudt,
    cputm LIKE ekbe-cputm,
    END OF lt_ekbe.

  DATA : BEGIN OF lt_reval OCCURS 0,
       matnr LIKE ekpo-matnr,
       ebeln LIKE ekpo-ebeln,
       ebelp LIKE ekpo-ebelp,
       excfdate LIKE it_itab-excfdate,
       exctdate LIKE it_itab-exctdate,
       END OF lt_reval.

*  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
*        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i,
        l_reval_index LIKE sy-tabix,
        l_ebeln LIKE it_itab-ebeln,
        l_ebelp LIKE it_itab-ebelp,
        l_update LIKE it_itab-reval_update,
        l_msg LIKE it_itab-msg,
        l_xrech LIKE rbkp-xrech,
        l_curr_matnr LIKE it_itab-matnr,
        l_pre_matnr LIKE it_itab-matnr.

  RANGES: lr_wedat FOR sy-datum.

  REFRESH: lt_reval, it_reval_log.

  LOOP AT it_itab.

    IF it_itab-matnr = ' '.
      MESSAGE e000(zz) WITH text-m13.
    ENDIF.

*    IF it_itab-reval_update = 'X'.
*      MESSAGE e000(zz) WITH text-m14.
*    ENDIF.

    IF it_itab-sa_update IS INITIAL.
      MESSAGE e000(zz) WITH text-m17.
    ENDIF.

    READ TABLE lt_reval WITH KEY
               matnr = it_itab-matnr
               ebeln = it_itab-ebeln
               ebelp = it_itab-ebelp
               excfdate = it_itab-excfdate
               exctdate = it_itab-exctdate.

    IF sy-subrc <> 0.

      l_pre_matnr = l_curr_matnr.
      l_curr_matnr = it_itab-matnr.
      lt_reval-matnr = it_itab-matnr.
      lt_reval-ebeln = it_itab-ebeln.
      lt_reval-ebelp = it_itab-ebelp.
      lt_reval-excfdate = it_itab-excfdate.
      lt_reval-exctdate = it_itab-exctdate.

      APPEND lt_reval.

      lr_wedat-sign = 'I'.
      lr_wedat-option = 'BT'.
      lr_wedat-low = it_itab-excfdate.
      lr_wedat-high = it_itab-exctdate.
      APPEND lr_wedat.

      SUBMIT rmmr1mrb    "via selection-screen
             WITH pa_ebeln = it_itab-ebeln
             WITH pa_ebelp = it_itab-ebelp
             WITH so_wedat IN lr_wedat
             WITH pa_nbwdt = p_pdate
             WITH pa_zterm = 'P030'
             WITH pa_guzte  = 'P030'
             WITH pa_xtest = ' '   "p_test
             AND RETURN.

      DO 30 TIMES.
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
          WAIT UP TO '1' SECONDS.
        ENDIF.
      ENDDO.

      SORT lt_ekbe DESCENDING BY cpudt cputm.
      READ TABLE lt_ekbe INDEX 1.

      IF sy-subrc = 0.
        READ TABLE it_pri WITH KEY matnr = it_itab-matnr
                                  lifnr = it_itab-lifnr
                                  pum_n = it_itab-pum_n
                                  ztseq = it_itab-ztseq.
        IF ( sy-subrc = 0 AND  it_pri-reval_update IS INITIAL )
           OR ( sy-subrc = 0 AND l_curr_matnr = l_pre_matnr ).
          l_reval_index = sy-tabix.
          it_pri-reval_update = 'X'.
          it_pri-reval_date = sy-datum.
          it_pri-reval_user = sy-uname.
          it_pri-belnr = lt_ekbe-belnr.
          CLEAR: l_xrech.

** On 07/18/14  added do 6 times to wait for database updae (
          DO 60 TIMES.
            SELECT SINGLE rmwwr xrech
              INTO (it_pri-inv_amt, l_xrech)
                FROM rbkp
              WHERE belnr = it_pri-belnr
                AND gjahr = p_pdate+0(4).
            IF sy-subrc = 0.
              EXIT.
            ELSE.
              WAIT UP TO '1' SECONDS.
            ENDIF.
          ENDDO.
** )
          IF l_xrech IS INITIAL.
            it_pri-inv_amt = it_pri-inv_amt * -1.
          ENDIF.
          MODIFY it_pri INDEX l_reval_index TRANSPORTING reval_update
               reval_date  reval_user belnr inv_amt.
*          IF p_test IS INITIAL.
          UPDATE ztmm_pri_reval
          SET: reval_update = 'X'
               reval_date = sy-datum
               reval_user = sy-uname
               belnr = it_pri-belnr
               inv_amt = it_pri-inv_amt
               WHERE matnr = it_itab-matnr
               AND lifnr = it_itab-lifnr
               AND pum_n = it_itab-pum_n
               AND ztseq = it_itab-ztseq
               AND belnr = ' '.

          IF sy-subrc = 0.
            COMMIT WORK.
          ELSE.
            ROLLBACK WORK.
            MESSAGE s000(zz) WITH text-m16.
          ENDIF.
          PERFORM update_log_reval USING lt_ekbe-belnr.
*          ENDIF.
        ENDIF.
        it_itab-reval_update = 'S'.
        CONCATENATE 'Doc No:' lt_ekbe-belnr INTO it_itab-msg
          SEPARATED BY space.
      ELSE.
        it_pri-reval_update = ' '.
        READ TABLE it_pri WITH KEY matnr = it_itab-matnr
                                  lifnr = it_itab-lifnr
                                  pum_n = it_itab-pum_n
                                  ztseq = it_itab-ztseq.
        IF sy-subrc = 0 AND  it_pri-reval_update = 'X'.
          l_reval_index = sy-tabix.
          it_pri-reval_update = ' '.
          it_pri-reval_date = '00000000'.
          it_pri-reval_user = ' '.
          MODIFY it_pri INDEX l_reval_index TRANSPORTING reval_update
              reval_date reval_user.
*          IF p_test IS INITIAL.
          UPDATE ztmm_pri_reval
          SET: reval_update = ' '
               reval_date = '00000000'
               reval_user = ' '
                 WHERE matnr = it_itab-matnr
               AND lifnr = it_itab-lifnr
               AND pum_n = it_itab-pum_n
               AND ztseq = it_itab-ztseq..
          COMMIT WORK.
          it_error = it_pri.
          APPEND it_error.
        ENDIF.
        it_itab-reval_update = 'E'.
        it_itab-msg = 'Error in re_val'.
      ENDIF.
      MODIFY it_itab TRANSPORTING reval_update msg.
*      CLEAR: it_reval_rsn.
      REFRESH: lr_wedat.
    ENDIF.
  ENDLOOP.

  CLEAR: l_ebeln, l_ebelp, l_update,l_msg.
  LOOP AT it_itab.
    IF l_ebeln = it_itab-ebeln AND
       l_ebelp = it_itab-ebelp.
      it_itab-reval_update = l_update.
      it_itab-msg = l_msg.
      MODIFY it_itab.
    ELSE.
      l_ebeln = it_itab-ebeln.
      l_ebelp = it_itab-ebelp.
      l_update = it_itab-reval_update.
      l_msg = it_itab-msg.
    ENDIF.
  ENDLOOP.
*
*  IF it_error[] IS NOT INITIAL.
*    PERFORM send_email.
*  ENDIF.
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
*perform bdc_field       using 'S_XBLNR-LOW'
*                                r_xblnr-LOW.

*  PERFORM bdc_dynpro      USING 'SAPMSSY0' '0120'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'
*                                '=BACK'.
*  PERFORM bdc_dynpro      USING 'ZRFIG02' '1000'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'
*                                '/EE'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'
*                                'P_BUKRS'.
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
**&--------------------------------------------------------------------*
**&      Form  GET_ALL_SELECTED_SA
**&--------------------------------------------------------------------*
**       text
**---------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**---------------------------------------------------------------------*
*FORM GET_ALL_SELECTED_SA .
* CLEAR: it_sa_all_onestep, it_sa_all_onestep[].
*  SELECT a~ebeln ebelp matnr werks lgort lifnr
*         peinh a~bstyp a~bukrs bsart ekorg
*         ekgrp kdatb kdate b~loekz
*               INTO CORRESPONDING FIELDS OF TABLE it_sa_all_onestep
*               FROM ekko AS a INNER JOIN ekpo AS b
*                 ON a~mandt EQ b~mandt
*                AND a~ebeln EQ b~ebeln
*               FOR ALL ENTRIES IN it_pri_selected
*               WHERE a~bstyp EQ 'L'
*                 AND b~matnr = it_pri_selected-matnr
*                 AND a~lifnr = it_pri_selected-lifnr
*                 and b~LOEKZ = ' '.
*
*  sort it_sa_all_onestep by matnr lifnr.
*ENDFORM.                    " GET_ALL_SELECTED_SA
*&---------------------------------------------------------------------*
*&      Form  UPDATE_REVAL_SA_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_reval_sa_log USING p_result p_messa.
  DATA: l_index LIKE sy-tabix.
  REFRESH it_sa_log.
  READ TABLE it_itab_onestep WITH KEY matnr = it_pri_selected-matnr
                                      lifnr = it_pri_selected-lifnr
                                      pum_n = it_pri_selected-pum_n
                                      ztseq = it_pri_selected-ztseq
                                      BINARY SEARCH.
  l_index = sy-tabix.
  LOOP AT it_itab_onestep FROM l_index
                                WHERE matnr = it_pri_selected-matnr
                                  AND lifnr = it_pri_selected-lifnr
                                  AND pum_n = it_pri_selected-pum_n
                                  AND ztseq = it_pri_selected-ztseq.

    MOVE-CORRESPONDING it_itab_onestep TO it_sa_log.
    MOVE-CORRESPONDING it_pri TO it_sa_log.
    it_sa_log-sa_to = it_itab_onestep-satdate.
    it_sa_log-sa_from = it_itab_onestep-safdate.
    it_sa_log-oldprice = it_itab_onestep-saprice.
    it_sa_log-newprice = it_itab_onestep-excprice.

    it_sa_log-sa_user = sy-uname.
    it_sa_log-sa_update = p_result.
    it_sa_log-sa_msg = p_messa.
    it_sa_log-run_date = w_rundate.
    it_sa_log-run_time = w_runtime.

    APPEND it_sa_log.
  ENDLOOP.
  IF it_sa_log[] IS NOT INITIAL.
    INSERT ztmm_revalsa_log FROM TABLE it_sa_log
    ACCEPTING DUPLICATE KEYS.
    IF sy-subrc EQ 0  OR sy-subrc EQ 4 .
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
      MESSAGE s999 WITH 'Error in adding to SA log table for'
         it_pri_selected-pum_n it_pri_selected-ztseq.
    ENDIF.
  ENDIF.
ENDFORM.                    " UPDATE_REVAL_SA_LOG

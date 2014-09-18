************************************************************************
* Program Name      : ZMMR_AUTO_TAX_UPDATE
* Creation Date     : 05/30/13
* Development Request No :
* Addl Documentation:
* Description       : Auto Tax Update
*                     1) When new parts created, condition NAVS will be
*                     created with DUMMY vendor ZZZZ
*                     2) Creation mek1 if not exist of current vendor
*                      for current existed PO
*                     3) Update PO condition if NAVS not exist
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT zmmr_auto_tax_update NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmmm.

TYPE-POOLS: slis, vrmn.
TABLES: ztmm_mat_02, a951.
INCLUDE <icon>.

DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE.

DATA : wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
       w_fieldname    LIKE LINE OF it_fieldname.

DATA: wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
      wa_variant TYPE disvariant,      "for parameter IS_VARIANT
      it_exclude TYPE ui_functions.

DATA: wa_custom_control TYPE        scrfname VALUE 'ALV_CONTAINER',
      alv_grid          TYPE REF TO cl_gui_alv_grid,
*      grid_container    TYPE REF TO cl_gui_custom_container.
       g_docking_container TYPE REF TO cl_gui_docking_container.

DATA: ok_code LIKE sy-ucomm,
      w_repid LIKE sy-repid,
      w_cnt   TYPE i.

DATA: w_ebeln LIKE ekko-ebeln.

DATA: BEGIN OF it_data OCCURS 0,
      lifnr LIKE ekko-lifnr,
      matnr LIKE ekpo-matnr,
      mwskz LIKE ekpo-mwskz,
      result(1),
      message(80) TYPE c,
      END OF it_data.

DATA: BEGIN OF it_temp OCCURS 0,
       matnr LIKE ekpo-matnr,
       maktx LIKE makt-maktx,
*       mwskz LIKE ekpo-mwskz,
       END OF it_temp.

DATA: BEGIN OF it_10 OCCURS 0,
       matnr LIKE ekpo-matnr,
       mwskz LIKE ekpo-mwskz,
       lifnr LIKE ekko-lifnr,
       END OF it_10.

DATA: BEGIN OF it_po OCCURS 0,
      ebeln LIKE ekko-ebeln,
      ebelp LIKE ekpo-ebelp,
      lifnr LIKE ekko-lifnr,
      name1 LIKE lfa1-name1,
      matnr LIKE ekpo-matnr,
      desc LIKE ekpo-txz01,
      meins  LIKE ekpo-meins,
      menge LIKE ekpo-menge,
      waers LIKE ekko-waers,
      netwr LIKE ekpo-netwr,
      mwskz LIKE ekpo-mwskz,
      mwsk1 LIKE konp-mwsk1,
      kbetr LIKE konp-kbetr,
      st_po(4), " TYPE icon_d,
      st_con(4), " TYPE icon_d,
      remark(50),
      celltab TYPE lvc_t_styl,
      END OF it_po.

DATA: BEGIN OF it_po_s OCCURS 0,
      ebeln LIKE ekko-ebeln,
      ebelp LIKE ekpo-ebelp,
      lifnr LIKE ekko-lifnr,
      name1 LIKE lfa1-name1,
      matnr LIKE ekpo-matnr,
      desc LIKE ekpo-txz01,
      meins  LIKE ekpo-meins,
      menge LIKE ekpo-menge,
      waers LIKE ekko-waers,
      netwr LIKE ekpo-netwr,
      mwskz LIKE ekpo-mwskz,
      mwsk1 LIKE konp-mwsk1,
      st_po TYPE icon_d,
      st_con TYPE icon_d,
      remark(50),
      END OF it_po_s.

DATA: it_chpo LIKE TABLE OF it_po WITH HEADER LINE.

DATA: bdcdata LIKE bdcdata    OCCURS 0 WITH HEADER LINE.
DATA: messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
DATA: ctumode LIKE ctu_params-dismode VALUE 'N'.
*                                      "A: show all dynpros
*                                      "E: show dynpro on error only
*                                      "N: do not display dynpro,
DATA: cupdate LIKE ctu_params-updmode VALUE  'L'.
*                                      "S: synchronously
*                                      "A: asynchronously
*                                      "L: local.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETERS: p_date LIKE ztmm_mat_02-zbdat OBLIGATORY.
*            p_day TYPE i DEFAULT '7'.

SELECTION-SCREEN SKIP.

PARAMETERS: p_rver LIKE somlreci1-receiver OBLIGATORY.
*SELECTION-SCREEN  BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 1(16) text-u01 FOR FIELD p_all.
*PARAMETERS: p_all RADIOBUTTON GROUP grp1.
*SELECTION-SCREEN COMMENT 30(10) text-u12 FOR FIELD p_po.
*PARAMETERS: p_po RADIOBUTTON GROUP grp1.
*SELECTION-SCREEN  END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.

START-OF-SELECTION.

** Creation ZZZZ
  PERFORM get_data.
  IF it_temp[] IS INITIAL.
    MESSAGE s009 WITH 'No data (ZZZZ)'.
  ELSE.
    PERFORM create_condition USING 'ZZZZ'.
    PERFORM send_email.
  ENDIF.

** Update MEK1 with actual vendor
  PERFORM update_navs_condition.
  PERFORM update_po.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  create_condition
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIFNR    text
*----------------------------------------------------------------------*
FORM create_condition USING p_lifnr.
  DATA: l_count TYPE i,
        l_zero LIKE konp-kbetr.
  DATA: lt_a951 LIKE TABLE OF a951 WITH HEADER LINE.

  CLEAR: l_count.
  SELECT * INTO TABLE lt_a951
    FROM a951
    FOR ALL ENTRIES IN it_temp
    WHERE matnr = it_temp-matnr
     AND lifnr = 'ZZZZ'.
  SORT lt_a951 BY matnr.

  LOOP AT it_temp.
    READ TABLE lt_a951 WITH KEY matnr = it_temp-matnr
       BINARY SEARCH.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.

    l_count = l_count + 1.

    IF l_count > 10.
      PERFORM call_mek1_vendor USING p_lifnr l_zero.
      REFRESH: it_10.
      CLEAR: it_10, l_count.
    ELSE.
      it_10 = it_temp.
      it_10-lifnr = p_lifnr.
      CLEAR: it_10-mwskz.
      APPEND it_10.
    ENDIF.
  ENDLOOP.
  IF NOT it_10[] IS INITIAL.
    PERFORM call_mek1_vendor USING p_lifnr l_zero.
    REFRESH: it_10.
    CLEAR: it_10.
  ENDIF.
ENDFORM.                                                    "CALL_MEK1
*&---------------------------------------------------------------------*
*&      Form  bdc_transaction
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TCODE    text
*----------------------------------------------------------------------*
FORM bdc_transaction USING p_tcode.
  DATA: l_subrc LIKE sy-subrc,
        msg(255),
        l_index LIKE sy-tabix.

  REFRESH: messtab.

  CALL TRANSACTION p_tcode USING bdcdata
                   MODE   ctumode
                   UPDATE cupdate
                   MESSAGES INTO messtab.
  l_subrc = sy-subrc.

  READ TABLE messtab WITH KEY msgtyp = 'S'.
  IF sy-subrc = 0.
    LOOP AT it_10.
      LOOP AT it_po WHERE lifnr = it_10-lifnr
                      AND  matnr = it_10-matnr
                      AND  mwsk1 = it_10-mwskz.
        IF sy-subrc = 0.
          l_index = sy-tabix.
          it_po-st_con = '@08@'.
          MODIFY it_po INDEX l_index TRANSPORTING st_con.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
    COMMIT WORK.
  ELSE.
    READ TABLE messtab WITH KEY msgtyp = 'E'.
    IF sy-subrc = 0.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = sy-msgid
          msgnr               = sy-msgno
          msgv1               = sy-msgv1
          msgv2               = sy-msgv2
          msgv3               = sy-msgv3
          msgv4               = sy-msgv4
        IMPORTING
          message_text_output = msg.
      LOOP AT it_10.
        LOOP AT it_po WHERE lifnr = it_10-lifnr
                         AND  matnr = it_10-matnr
                         AND  mwsk1 = it_10-mwskz.
          IF sy-subrc = 0.
            l_index = sy-tabix.
            it_po-st_con = '@0A@'.
            it_po-remark = msg.
            MODIFY it_po INDEX l_index TRANSPORTING st_con remark.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ELSE.
      READ TABLE messtab WITH KEY msgtyp = 'A'.
      IF sy-subrc = 0.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            msgid               = sy-msgid
            msgnr               = sy-msgno
            msgv1               = sy-msgv1
            msgv2               = sy-msgv2
            msgv3               = sy-msgv3
            msgv4               = sy-msgv4
          IMPORTING
            message_text_output = msg.
        LOOP AT it_10.
          READ TABLE it_po WITH KEY lifnr = it_10-lifnr
                                    matnr = it_10-matnr
                                    mwsk1 = it_10-mwskz.
          IF sy-subrc = 0.
            l_index = sy-tabix.
            it_po-st_con = '@0A@'.
            it_po-remark = msg.
            MODIFY it_po INDEX l_index TRANSPORTING st_con remark.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.
  REFRESH: messtab, bdcdata.

ENDFORM.                    " BDC_TRANSACTION               " CALL_MEK1

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .
  REFRESH: it_temp.
  SELECT a~matnr maktx INTO TABLE it_temp
    FROM ztmm_mat_02 AS a
*    INNER JOIN makt AS B
*    ON A~MATNR = B~MATNR
     WHERE zbdat = p_date
       AND zmode = 'C'
       AND zresult = 'S'.
*       AND B~SPARS = 'EN'.

ENDFORM.                    " GET_DATA

*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1580   text
*      -->P_1581   text
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.                    "BDC_FIELD
*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.                    "BDC_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  CHANGE_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_po .
  REFRESH it_data.
  LOOP AT it_po WHERE mwsk1 <> ' '.
    AT NEW ebeln.
      w_ebeln = it_po-ebeln.
      REFRESH it_chpo.
      CLEAR: it_chpo.
    ENDAT.
    it_chpo = it_po.
    APPEND it_chpo.
    AT END OF ebeln.
      PERFORM change_po_bapi.
    ENDAT.
  ENDLOOP.
  IF NOT it_chpo[] IS INITIAL.
    PERFORM change_po_bapi.
  ENDIF.
ENDFORM.                    " CHANGE_PO
*&---------------------------------------------------------------------*
*&      Form  CHANGE_PO_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_po_bapi .
  DATA: i_poheader        LIKE          bapimepoheader,
        i_poheaderx       LIKE          bapimepoheaderx,
        t_poitem TYPE TABLE OF bapimepoitem WITH HEADER LINE,
        t_poitemx       TYPE TABLE OF bapimepoitemx    WITH HEADER LINE,
        t_poaccount     TYPE TABLE OF bapimepoaccount  WITH HEADER LINE,
        t_poaccountx    TYPE TABLE OF bapimepoaccountx WITH HEADER LINE,
        t_poschedule    TYPE TABLE OF bapimeposchedule WITH HEADER LINE,
        t_poschedulex   TYPE TABLE OF bapimeposchedulx WITH HEADER LINE,
        t_pocond        TYPE TABLE OF bapimepocond     WITH HEADER LINE,
        t_pocondx       TYPE TABLE OF bapimepocondx    WITH HEADER LINE,
        t_potext        TYPE TABLE OF bapimepotext     WITH HEADER LINE,
        t_polimits      TYPE TABLE OF bapiesuhc        WITH HEADER LINE,
        t_poservices    TYPE TABLE OF bapiesllc        WITH HEADER LINE.

  DATA: ret2 LIKE bapiret2 OCCURS 0 WITH HEADER LINE.
  DATA: l_index LIKE sy-tabix.

  LOOP AT it_chpo.

    t_poitem-po_item    = it_chpo-ebelp.
    t_poitem-material    = it_chpo-matnr.
    t_poitem-tax_code   = it_chpo-mwsk1.

    t_poitemx-po_item    = it_chpo-ebelp.
*    t_poitemx-po_itemx   = 'X'.
    t_poitemx-material    = 'X'.
    t_poitemx-tax_code    = 'X'.

    APPEND t_poitem.
    APPEND t_poitemx.

*    t_pocond-itm_number    = it_chpo-ebelp.
*    t_pocond-condition_no  = it_chpo-knumv.
*    t_pocond-cond_type     =  'NAVS'.
*
*    t_pocondx-itm_number   = it_chpo-ebelp.
*    t_pocondx-condition_no = 'X'.
*    t_pocondx-cond_type    = 'X'.

    APPEND t_pocond.
    APPEND t_pocondx.

    CLEAR it_chpo.

  ENDLOOP.

  CALL FUNCTION 'BAPI_PO_CHANGE'
    EXPORTING
      purchaseorder = w_ebeln
    TABLES
      return        = ret2
      poitem        = t_poitem
      poitemx       = t_poitemx
      pocond        = t_pocond
      pocondx       = t_pocondx.

  READ TABLE ret2 WITH KEY type = 'S'.

  IF sy-subrc EQ 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
*      IMPORTING
*        return = commit_return.

    LOOP AT it_chpo.
      READ TABLE it_po WITH KEY ebeln = it_chpo-ebeln
                                ebelp = it_chpo-ebelp.
      IF sy-subrc = 0.
        l_index = sy-tabix.
        it_po-st_po = '@08@'.
        MODIFY it_po INDEX l_index TRANSPORTING st_po .
      ENDIF.
    ENDLOOP.
    WRITE :/ w_ebeln.
  ELSE.
    LOOP AT ret2 WHERE type = 'E'
                    OR type = 'A'
                    OR type = 'X'.
      LOOP AT it_chpo.
        READ TABLE it_po WITH KEY ebeln = it_chpo-ebeln
                                  ebelp = it_chpo-ebelp.
        IF sy-subrc = 0.
          l_index = sy-tabix.
          it_po-st_po = '@08@'.
          it_po-remark = ret2-message_v1.
          MODIFY it_po INDEX l_index TRANSPORTING st_po remark .
        ENDIF.
      ENDLOOP.
      EXIT.
    ENDLOOP.
    WRITE :/ 'Failed for updating PO: ', w_ebeln.
  ENDIF.

  REFRESH it_chpo.
  CLEAR: it_chpo, w_ebeln.
ENDFORM.                    " CHANGE_PO_BAPI
*----------------------------------------------------------------------*
***INCLUDE ZRMM_REQUIREMENT_PLAN_PBO .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'ST200'.
  SET TITLEBAR 'T200'.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv_200 OUTPUT.

  IF g_docking_container IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM select_edit_line.
    PERFORM create_container_n_object.
    PERFORM set_attributes_alv_grid.
    PERFORM build_sortcat_display.
    PERFORM exclude_tb_functions.
    PERFORM build_field_catalog USING 'IT_PO'.
    PERFORM assign_itab_to_alv.
*    PERFORM sssign_event_9000.
  ELSE.
    CALL METHOD alv_grid->refresh_table_display
      EXPORTING
        i_soft_refresh = 'X'.
  ENDIF.

ENDMODULE.                 " DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_container_n_object.
  DATA: l_repid LIKE sy-repid,
        l_dynnr LIKE sy-dynnr.

  l_repid = sy-repid.
  l_dynnr = sy-dynnr.
  CREATE OBJECT g_docking_container
    EXPORTING
      repid                       = l_repid
      dynnr                       = l_dynnr
      side                        = cl_gui_docking_container=>dock_at_bottom
*     RATIO                       = 90
      extension                   = 2000
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.
*  CREATE OBJECT g_docking_container
*    EXPORTING
*      container_name              = wa_custom_control
*    EXCEPTIONS
*      cntl_error                  = 1
*      cntl_system_error           = 2
*      create_error                = 3
*      lifetime_error              = 4
*      lifetime_dynpro_dynpro_link = 5.
*  w_repid = sy-repid.
  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = l_repid
        txt2  = sy-subrc
        txt1  = 'The control can not be created'.
  ENDIF.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  CREATE OBJECT alv_grid
    EXPORTING
      i_parent      = g_docking_container
      i_appl_events = 'X'.

ENDFORM.                    " create_container_n_object
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_grid.
  DATA : lw_s_dragdrop TYPE lvc_s_dd01. "/ Drag&Drop control settings

  CLEAR : wa_is_layout, wa_variant.

*//-- Set Layout Structure
  wa_is_layout-edit       = 'X'.      "/Edit Mode Enable
  wa_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  wa_is_layout-language   = sy-langu. "/Language Key
  wa_is_layout-cwidth_opt = ' '.   "/optimizes the column width
  wa_is_layout-info_fname = 'IF'.
*  WA_IS_LAYOUT-CTAB_FNAME = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging
  wa_is_layout-stylefname = 'CELLTAB'.
*//-- Set Variant Structure
  wa_variant-report       = sy-repid.
  wa_variant-username     = sy-uname.

ENDFORM.                    " set_attributes_alv_grid
*&---------------------------------------------------------------------*
*&      Form  build_sortcat_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sortcat_display.

*  it_sort-spos           = 1.
*  it_sort-fieldname      = 'LIFNR'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
*
*  it_sort-spos           = 2.
*  it_sort-fieldname      = 'MATNR'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.

ENDFORM.                    " build_sortcat_display
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0027   text
*----------------------------------------------------------------------*
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

                                  'S' 'EBELN'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'PO Number',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'EBELP'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Item',
                                   'E' 'OUTPUTLEN'   '4',

                                  'S' 'LIFNR'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Vendor',
                                   'E' 'OUTPUTLEN'   '6',

                                  'S' 'NAME1'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Name',
                                  'E' 'OUTPUTLEN'   '20',

                                  'S' 'MATNR'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Material No',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'DESC'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Description',
                                  'E' 'OUTPUTLEN'   '25',

                                  'S' 'MEINS'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'UM',
                                  'E' 'OUTPUTLEN'   '4',

                                  'S' 'MENGE'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Quantity',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'WAERS'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Curr',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'NETWR'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Net Amt',
                                  'E' 'OUTPUTLEN'   '13',


                                 'S' 'MWSKZ'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Old Code',
                                  'E' 'OUTPUTLEN'   '5',


                                 'S' 'MWSK1'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'New Code',
                                  ' ' 'F4AVAILABL'  'X',
*                                  ' ' 'REF_TABLE'   'T007S',
*                                  ' ' 'REF_FIELD'   'MWSKZ',
*                                  ' ' 'DOMNAME'     'MWSKZ',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'ST_PO'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Status(PO)',
                                  ' ' 'ICON'        'X',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'ST_CON'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Status(Con)',
                                  ' ' 'ICON'        'X',
                                  'E' 'OUTPUTLEN'   '10'.

*                                  'S' 'RRMARK'       ' ',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'COLTEXT'     'Remarks',
*                                  'E' 'OUTPUTLEN'   '30'.

ENDFORM.                    " build_field_catalog

*---------------------------------------------------------------------*
*       FORM setting_fieldcat                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_FIELDCAT                                                    *
*  -->  P_GUBUN                                                       *
*  -->  P_FIELD                                                       *
*  -->  P_VALUE                                                       *
*---------------------------------------------------------------------*
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

*---------------------------------------------------------------------*
*       FORM assign_itab_to_alv                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM assign_itab_to_alv.

  CALL METHOD alv_grid->set_table_for_first_display
    EXPORTING
      is_layout            = wa_is_layout
      i_save               = wa_save
      is_variant           = wa_variant
*     i_default            = space
      it_toolbar_excluding = it_exclude
    CHANGING
      it_fieldcatalog      = it_fieldcat[]
      it_outtab            = it_po[].
*               it_sort          = it_sort[].

  CALL METHOD alv_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

* Cursor----
  CALL METHOD alv_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD cl_gui_control=>set_focus
    EXPORTING
      control = alv_grid.
ENDFORM.                    " assign_itab_to_alv
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exclude_tb_functions.
  DATA ls_exclude TYPE ui_func.

* Row manipulation
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

*  Sort buttons
  ls_exclude = cl_gui_alv_grid=>mc_fc_sort_asc.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_sort_dsc.
  APPEND ls_exclude TO it_exclude.
**  This excludes all buttons
*  LS_EXCLUDE = '&EXCLALLFC'.
*  APPEND LS_EXCLUDE TO PT_EXCLUDE.
ENDFORM.                    " EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  SELECT_EDIT_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_edit_line.
  DATA: lt_celltab TYPE lvc_t_styl,
        w_celltab TYPE lvc_s_styl,
        l_index TYPE i,
        l_mode TYPE raw4.


  REFRESH lt_celltab.

  w_celltab-fieldname = 'MWSK1'.
  l_mode = cl_gui_alv_grid=>mc_style_enabled.

  l_mode = cl_gui_alv_grid=>mc_style_disabled.
  w_celltab-fieldname = 'EBELN'.
  w_celltab-style = l_mode.
  INSERT w_celltab INTO TABLE lt_celltab.
  w_celltab-fieldname = 'EBELP'.
  w_celltab-style = l_mode.
  INSERT w_celltab INTO TABLE lt_celltab.
  w_celltab-fieldname = 'LIFNR'.
  w_celltab-style = l_mode.
  INSERT w_celltab INTO TABLE lt_celltab.

  l_mode = cl_gui_alv_grid=>mc_style_disabled.
  w_celltab-fieldname = 'NAME1'.
  w_celltab-style = l_mode.
  INSERT w_celltab INTO TABLE lt_celltab.
  w_celltab-fieldname = 'MATNR'.
  w_celltab-style = l_mode.
  INSERT w_celltab INTO TABLE lt_celltab.
  w_celltab-fieldname = 'DESC'.
  w_celltab-style = l_mode.
  INSERT w_celltab INTO TABLE lt_celltab.

  l_mode = cl_gui_alv_grid=>mc_style_disabled.
  w_celltab-fieldname = 'MEINS'.
  w_celltab-style = l_mode.
  INSERT w_celltab INTO TABLE lt_celltab.
  w_celltab-fieldname = 'MENGE'.
  w_celltab-style = l_mode.
  INSERT w_celltab INTO TABLE lt_celltab.
  w_celltab-fieldname = 'WAERS'.
  w_celltab-style = l_mode.
  INSERT w_celltab INTO TABLE lt_celltab.

  l_mode = cl_gui_alv_grid=>mc_style_disabled.
  w_celltab-fieldname = 'NETWR'.
  w_celltab-style = l_mode.
  INSERT w_celltab INTO TABLE lt_celltab.
  w_celltab-fieldname = 'MWSKZ'.
  w_celltab-style = l_mode.
  INSERT w_celltab INTO TABLE lt_celltab.
  w_celltab-fieldname = 'ST_PO'.
  w_celltab-style = l_mode.
  INSERT w_celltab INTO TABLE lt_celltab.
  w_celltab-fieldname = 'ST_CON'.
  w_celltab-style = l_mode.
  INSERT w_celltab INTO TABLE lt_celltab.
  w_celltab-fieldname = 'REMARK'.
  w_celltab-style = l_mode.
  INSERT w_celltab INTO TABLE lt_celltab.

  LOOP AT it_po.
    l_index = sy-tabix.
    INSERT LINES OF lt_celltab INTO TABLE it_po-celltab.
    MODIFY it_po INDEX l_index.
  ENDLOOP.

ENDFORM.    " SELECT_EDIT_LINE
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE ok_code.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'UPDATE'.
      PERFORM change_po.
  ENDCASE.
ENDMODULE.                    "USER_COMMAND_0200 INPUT
*&---------------------------------------------------------------------*
*&      Form  GUI_ALV_REFRESH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gui_alv_refresh .
  DATA: l_scroll TYPE lvc_s_stbl.
  CHECK NOT alv_grid IS INITIAL.

  l_scroll-row = 'X'.
  l_scroll-col = 'X'.

  CALL METHOD alv_grid->refresh_table_display
    EXPORTING
      i_soft_refresh = 'X'
      is_stable      = l_scroll.     "?? ??? ??? refresh

ENDFORM.                    " GUI_ALV_REFRESH

*&---------------------------------------------------------------------*
*&      Form  call_mek1_vendor
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_mek1_vendor USING p_lifnr p_kbetr .
  DATA: l_no(2) TYPE n,
        l_kbetr(8),
        l_matnr_t(20),
        l_kbetr_t(20),
        l_mwsk1_t(20).

  l_kbetr = p_kbetr.
  PERFORM bdc_dynpro      USING 'SAPMV13A' '0100'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RV13A-KSCHL'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'RV13A-KSCHL'
                                'NAVS'.

  PERFORM bdc_dynpro      USING 'SAPMV13A' '1951'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'KOMG-MATNR(02)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'KOMG-LIFNR'
                                p_lifnr.
*  IF p_kbetr > 0.
  l_no = '01'.
  LOOP AT it_10.
    CONCATENATE 'KOMG-MATNR(' l_no ')' INTO l_matnr_t.
    CONCATENATE 'KONP-KBETR(' l_no ')' INTO l_kbetr_t.
    CONCATENATE 'KONP-MWSK1(' l_no ')' INTO l_mwsk1_t.

    PERFORM bdc_field       USING l_matnr_t
                              it_10-matnr.
    PERFORM bdc_field       USING l_kbetr_t
                                 l_kbetr.
    PERFORM bdc_field       USING l_mwsk1_t
                              it_10-mwskz.

    l_no = l_no + 1.
*    CONCATENATE 'KOMG-MATNR(' l_no ')' INTO l_matnr_t.
*
*    perform bdc_dynpro      using 'SAPMV13A' '1951'.
*perform bdc_field       using 'BDC_CURSOR'
*                              l_matnr_t.
*perform bdc_field       using 'BDC_OKCODE'
*                              '/00'.

  ENDLOOP.

  PERFORM bdc_dynpro      USING 'SAPMV13A' '1951'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'KOMG-MATNR(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=SICH'.

  PERFORM bdc_transaction USING 'MEK1'.

ENDFORM.                    " CALL_MEK1_ZZZZ
*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_email .
  DATA: lt_body LIKE TABLE OF solisti1 WITH HEADER LINE.

  DATA: l_subject TYPE p15_text150,
        l_p_rec_type  LIKE  somlreci1-rec_type.

  MOVE 'New Parts:' TO lt_body.
  APPEND lt_body.
  CLEAR: lt_body.
  MOVE '=========' TO lt_body.
  APPEND lt_body.
  CLEAR: lt_body.

  MOVE: 'Material No' TO lt_body+0(20),
        'Steel Material' TO lt_body+20(30).

  APPEND lt_body.
  CLEAR: lt_body.

  MOVE: '--------------------' TO  lt_body+0(20),
        '------------------------------' TO  lt_body+20(30),
        '----------------------------------------'
         TO  lt_body+50(40).
  APPEND lt_body.
  CLEAR: lt_body.


  LOOP AT it_temp.
    MOVE: it_temp-matnr TO lt_body+0(20),
          p_date TO lt_body+20(30),
          it_temp-maktx TO lt_body+50(40).
    APPEND lt_body.
  ENDLOOP.

  CALL FUNCTION 'ZCAF_SEND_EMAIL'
    EXPORTING
      p_subject  = 'Tax Code Update Required'
      p_rec_type = 'C'
      p_receiver = p_rver
    TABLES
      pt_body    = lt_body.
*
ENDFORM.                    " SEND_EMAIL
*&---------------------------------------------------------------------*
*&      Form  UPDATE_NAVS_CONDITION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_navs_condition.
  DATA: l_kbetr LIKE konp-kbetr.

  REFRESH: it_po.

*  DATA: BEGIN OF lt_navs OCCURS 0,
*        matnr LIKE a951-matnr,
**        lifnr LIKE a951-lifnr,
*        kbetr LIKE konp-kbetr,
*        mwsk1 LIKE konp-mwsk1,
*        END OF lt_navs.

*  DATA: BEGIN OF lt_matnr OCCURS 0,
*        matnr LIKE a951-matnr,
**        lifnr LIKE a951-lifnr,
**        kbetr LIKE konp-kbetr,
**        mwsk1 LIKE konp-mwsk1,
*        END OF lt_matnr.


*  SELECT a~ebeln b~ebelp matnr lifnr mwskz
*    INTO CORRESPONDING FIELDS OF TABLE it_po
*    FROM ekko AS a
*    INNER JOIN ekpo AS b
*    ON a~ebeln = b~ebeln
*    FOR ALL ENTRIES IN it_temp
*    WHERE a~aedat >= l_date
*      AND a~loekz = ' '
*      AND b~matnr = it_temp-matnr.
*
*  SELECT matnr a~lifnr kbetr mwsk1
*   INTO CORRESPONDING FIELDS OF TABLE lt_navs
*   FROM a951 AS a
*   INNER JOIN konp AS b
*   ON a~knumh = b~knumh
*   FOR ALL ENTRIES IN it_po
*   WHERE matnr = it_po-matnr.
**      and lifnr = 'ZZZZ'
**      and kbtre > 0..

*  SELECT matnr COUNT( * ) INTO (l_matnr, l_count)
*    FROM a951
*    WHERE datab <= p_date
*      AND datbi >= p_date
*    GROUP BY matnr.
*    IF l_count = 1.
*      lt_matnr-matnr = l_matnr.
*      APPEND lt_matnr.
*      CLEAR: lt_matnr.
*    ENDIF.
*    CLEAR: l_matnr, l_count.
*  ENDSELECT.
*
*  CHECK lt_matnr[] IS NOT INITIAL.
*  SELECT matnr kbetr mwsk1
*  INTO CORRESPONDING FIELDS OF TABLE lt_navs
*  FROM a951 AS a
*  INNER JOIN konp AS b
*  ON a~knumh = b~knumh
*  FOR ALL ENTRIES IN lt_matnr
*  WHERE matnr = lt_matnr-matnr
*    AND a~lifnr = 'ZZZZ'
*    AND mwsk1 <> ' '.
*
*  CHECK lt_navs[] IS NOT INITIAL.
*  SELECT a~ebeln b~ebelp matnr lifnr mwskz
*    INTO CORRESPONDING FIELDS OF TABLE it_po
*    FROM ekko AS a
*    INNER JOIN ekpo AS b
*    ON a~ebeln = b~ebeln
*    FOR ALL ENTRIES IN lt_navs
**     WHERE a~aedat >= l_date
*      WHERE b~matnr = lt_navs-matnr
*        AND a~loekz = ' '.

  SELECT a~ebeln b~ebelp b~matnr a~lifnr mwskz mwsk1 kbetr
    INTO CORRESPONDING FIELDS OF TABLE it_po
    FROM ekko AS a
    INNER JOIN ekpo AS b
    ON a~ebeln = b~ebeln
    INNER JOIN a951 AS c
    ON b~matnr = c~matnr
    INNER JOIN konp AS d
     ON c~knumh = d~knumh
    WHERE c~lifnr = 'ZZZZ'
      AND mwsk1 <> ' '
      AND a~loekz = ' '
      AND b~erekz = ' '
      AND ( mwskz =  ' ' OR mwskz =  'I0' OR mwskz =  'U0' ).
*      AND a~lifnr NOT IN ( SELECT lifnr FROM a951
*                           WHERE lifnr = a~lifnr ).
  IF SY-SUBRC <> 0.
    MESSAGE I009 WITH 'No PO Data'.
    exit.
  endif.
  SORT it_po BY ebeln ebelp matnr.
  DELETE ADJACENT DUPLICATES FROM it_po COMPARING
      ebeln ebelp matnr.

  LOOP AT it_po.
    SELECT SINGLE * FROM a951
      WHERE lifnr = it_po-lifnr
        AND MATNR = IT_PO-MATNR.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.
    REFRESH it_10.
    l_kbetr = it_po-kbetr.
    it_10-matnr = it_po-matnr.
    it_10-mwskz = it_po-mwsk1.
    it_10-lifnr = it_po-lifnr.
    APPEND it_10.
    PERFORM call_mek1_vendor USING it_po-lifnr l_kbetr.
  ENDLOOP.

ENDFORM.                    " UPDATE_PO
*&---------------------------------------------------------------------*
*&      Form  UPDATE_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_po .
  REFRESH it_data.
  LOOP AT it_po.
    AT NEW ebeln.
      w_ebeln = it_po-ebeln.
      REFRESH it_chpo.
      CLEAR: it_chpo.
    ENDAT.
    it_chpo = it_po.
    APPEND it_chpo.
    AT END OF ebeln.
      PERFORM change_po_bapi.
    ENDAT.
  ENDLOOP.
  IF NOT it_chpo[] IS INITIAL.
    PERFORM change_po_bapi.
  ENDIF.

ENDFORM.                    " UPDATE_PO

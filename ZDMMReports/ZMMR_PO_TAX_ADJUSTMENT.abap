************************************************************************
* Program Name      : ZMMR_PO_TAX_ADJUSTMENTE
* Creation Date     : 10/08/12
* Development Request No :
* Addl Documentation:
* Description       : Mass Tax Maintenance
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT zmmr_po_tax_adjustmente NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmmm.

TYPE-POOLS: slis, vrmn.
TABLES: mara, ekko.
INCLUDE <icon>.


*DATA: IT_DATA LIKE TABLE OF ZTMM_PILOT_MATL WITH HEADER LINE.

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
      grid_container    TYPE REF TO cl_gui_custom_container.


*DATA: f4_mwskz  LIKE TABLE OF t007a  WITH HEADER LINE,
DATA: it_fieldcat2    TYPE slis_t_fieldcat_alv.

DATA: BEGIN OF f4_mwskz OCCURS 0,
      mwskz LIKE t007a-mwskz,
      text1 LIKE t007s-text1,
      END OF f4_mwskz.

DATA: ok_code LIKE sy-ucomm,
      w_repid LIKE sy-repid,
      w_cnt   TYPE i,
      w_mtart LIKE mara-mtart,
      w_ebeln LIKE ekko-ebeln.

DATA: v_file_table TYPE filetable,
      v_rc TYPE i.

DATA: w_lifnr LIKE ekko-lifnr.

DATA: BEGIN OF it_data OCCURS 0,
      lifnr LIKE ekko-lifnr,
      matnr LIKE ekpo-matnr,
      mwskz LIKE ekpo-mwskz,
      result(1),
      message(80) TYPE c,
      END OF it_data.

DATA: BEGIN OF it_temp OCCURS 0,
       matnr LIKE ekpo-matnr,
       mwskz LIKE ekpo-mwskz,
       END OF it_temp.

DATA: BEGIN OF it_12 OCCURS 0,
       matnr LIKE ekpo-matnr,
       mwskz LIKE ekpo-mwskz,
       END OF it_12.

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

* -------------------------------------------------------------
* EVent class
*-----------------------------------------------------------
* local class to handle semantic checks
CLASS lcl_event_receiver DEFINITION DEFERRED.

DATA: g_event_receiver TYPE REF TO lcl_event_receiver.

*************************************************************
* LOCAL CLASS Definition
**************************************************************
*§4.Define and implement event handler to handle event DATA_CHANGED.
*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS:
      handle_data_changed
         FOR EVENT data_changed OF cl_gui_alv_grid
              IMPORTING er_data_changed.

    METHODS : handle_f4 FOR EVENT onf4 OF cl_gui_alv_grid
           IMPORTING
             e_fieldname
             e_fieldvalue
             es_row_no
             er_event_data
             et_bad_cells
             e_display.
    DATA: error_in_data TYPE c.

ENDCLASS.                    "LCL_EVENT_RECEIVER DEFINITION
DATA :it_lvc  LIKE lvc_s_row.
*************************************************************
* LOCAL CLASS IMPLEMENTATION
**************************************************************
CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_data_changed.

    DATA: ls_good TYPE lvc_s_modi,
          lv_value TYPE lvc_value,
          lvc_t_row TYPE lvc_t_row.

    error_in_data = space.
    LOOP AT er_data_changed->mt_good_cells INTO ls_good.
      IF ls_good-fieldname = 'MWSK1'.
        CALL METHOD er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = ls_good-fieldname
          IMPORTING
            e_value     = lv_value.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = ls_good-fieldname
            i_value     = lv_value.
      ENDIF.
    ENDLOOP.

*§7.Display application log if an error has occured.
    IF error_in_data EQ 'X'.
      CALL METHOD er_data_changed->display_protocol.
    ENDIF.

  ENDMETHOD.                    "HANDLE_DATA_CHANGED
  METHOD handle_f4.
    PERFORM handle_f4 USING e_fieldname
                            e_fieldvalue
                            es_row_no
                            er_event_data
                            et_bad_cells
                            e_display.
  ENDMETHOD.                                                "handle_f4
ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS: s_bedat FOR ekko-bedat,
                s_bsart FOR ekko-bsart DEFAULT 'ZB',
                s_mtart FOR mara-mtart.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN  BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(16) text-u01 FOR FIELD p_all.
PARAMETERS: p_all RADIOBUTTON GROUP grp1.
SELECTION-SCREEN COMMENT 30(10) text-u12 FOR FIELD p_po.
PARAMETERS: p_po RADIOBUTTON GROUP grp1.
SELECTION-SCREEN  END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.


AT SELECTION-SCREEN OUTPUT.

START-OF-SELECTION.

  PERFORM get_po_data.
  IF it_po[] IS INITIAL.
    MESSAGE e009 WITH 'No data'.
  ENDIF.
  CALL SCREEN 0200.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data.
  LOOP AT it_po.
    IF it_po-st_po = 'S' OR  it_po-st_po = '@08@'.
      it_data-lifnr = it_po-lifnr.
      it_data-matnr = it_po-matnr.
      it_data-mwskz = it_po-mwsk1.
      COLLECT it_data.
    ENDIF.
  ENDLOOP.
  SORT it_data BY lifnr matnr.

  LOOP AT it_data.
    AT NEW lifnr.
      w_lifnr = it_data-lifnr.
      REFRESH it_temp.
      CLEAR: it_temp.
    ENDAT.
    it_temp-matnr = it_data-matnr.
    it_temp-mwskz = it_data-mwskz.
    APPEND it_temp.
    AT END OF lifnr.
      PERFORM create_condition.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  CALL_MEK1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_condition.
  DATA: l_count TYPE i.

  CLEAR: l_count.
  LOOP AT it_temp.
    l_count = l_count + 1.
    IF l_count > 12.
      PERFORM call_mek1.
      REFRESH: it_12.
      CLEAR: it_12, l_count.
    ELSE.
      it_12 = it_temp.
      APPEND it_12.
    ENDIF.
  ENDLOOP.
  IF NOT it_12[] IS INITIAL.
    PERFORM call_mek1.
    REFRESH: it_12.
    CLEAR: it_12.
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
    LOOP AT it_12.
      LOOP AT it_po WHERE lifnr = w_lifnr
                      AND  matnr = it_12-matnr
                      AND  mwsk1 = it_12-mwskz.
        IF sy-subrc = 0.
          l_index = sy-tabix.
          it_po-st_con = '@08@'.
          MODIFY it_po INDEX l_index TRANSPORTING st_con.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
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
      LOOP AT it_12.
        LOOP AT it_po WHERE lifnr = w_lifnr
                         AND  matnr = it_12-matnr
                         AND  mwsk1 = it_12-mwskz.
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
        LOOP AT it_12.
          READ TABLE it_po WITH KEY lifnr = w_lifnr
                                    matnr = it_12-matnr
                                    mwsk1 = it_12-mwskz.
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
*&      Form  GET_PO_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_po_data .
  DATA: l_index LIKE sy-tabix,
        l_index_inv LIKE sy-tabix.

  DATA: BEGIN OF lt_ekbe OCCURS 0,
      ebeln LIKE ekbe-ebeln,
      ebelp LIKE ekbe-ebelp,
      matnr LIKE ekbe-matnr,
      menge LIKE ekbe-menge,
      shkzg LIKE ekbe-shkzg,
      END OF lt_ekbe.

  SELECT a~ebeln b~ebelp a~lifnr name1 b~matnr b~txz01 AS desc
         b~meins menge waers netwr mwskz
         INTO CORRESPONDING FIELDS OF TABLE it_po
    FROM ekko AS a INNER JOIN ekpo AS b
    ON a~ebeln = b~ebeln
    INNER JOIN mara AS c
    ON b~matnr = c~matnr
    INNER JOIN lfa1 AS d
    ON a~lifnr = d~lifnr
    WHERE a~bedat IN s_bedat
      AND a~bsart IN s_bsart
      AND c~mtart IN s_mtart
      AND a~loekz = ' '
      AND b~loekz = ' '
      AND b~mwskz = ' '.

  IF sy-subrc = 0.
    SORT it_po BY ebeln ebelp.
  ENDIF.

ENDFORM.                    " GET_PO_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_MEK1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_12  text
*----------------------------------------------------------------------*
FORM call_mek1.
  DATA: l_no(2) TYPE n,
        l_matnr_t(20),
        l_kbetr_t(20),
        l_datab_t(20),
        l_datbi_t(20),
        l_mwsk1_t(20),
        l_kbetr(8),
        l_datab(10),
        l_datbi(10).

  PERFORM bdc_dynpro      USING 'SAPMV13A' '0100'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RV13A-KSCHL'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'RV13A-KSCHL'
                                'NAVS'.
  PERFORM bdc_dynpro      USING 'SAPMV13A' '1951'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'KONP-MWSK1(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'KOMG-LIFNR'
                                w_lifnr.

  l_no = '01'.
  LOOP AT it_12.
    CONCATENATE 'KOMG-MATNR(' l_no ')' INTO l_matnr_t.
    CONCATENATE 'KONP-KBETR(' l_no ')' INTO l_kbetr_t.
    CONCATENATE 'RV13A-DATAB(' l_no ')' INTO l_datab_t.
    CONCATENATE 'RV13A-DATBI(' l_no ')' INTO l_datbi_t.
    CONCATENATE 'KONP-MWSK1(' l_no ')' INTO l_mwsk1_t.

    l_datab = '01/01/2010'.
    l_datbi = '12/31/2999'.

    CASE it_12-mwskz.
      WHEN 'U1'.
        l_kbetr = 9.
      WHEN 'U3'.
        l_kbetr = 10.
      WHEN 'U5'.
        l_kbetr = '0.13'.
      WHEN 'U6'.
        l_kbetr = '0.13'.
      WHEN 'U7'.
        l_kbetr = '3.58'.
      WHEN 'U8'.
        l_kbetr = '3.58'.
    ENDCASE.

    PERFORM bdc_field       USING l_matnr_t
                            it_12-matnr.
    PERFORM bdc_field       USING l_kbetr_t
                            l_kbetr.
    PERFORM bdc_field       USING l_datab_t
                                  l_datab.
    PERFORM bdc_field       USING l_datbi_t
                                  l_datbi.
    PERFORM bdc_field       USING l_mwsk1_t
                                  it_12-mwskz.

    PERFORM bdc_dynpro      USING 'SAPMV13A' '1951'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  l_mwsk1_t.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    l_no = l_no + 1.
  ENDLOOP.

  PERFORM bdc_dynpro      USING 'SAPMV13A' '1951'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'KONP-MWSK1(12)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BNEW'.
  PERFORM bdc_dynpro      USING 'SAPMV13A' '1951'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'KOMG-LIFNR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=SICH'.
*    PERFORM bdc_field       USING 'KOMG-LIFNR'
*                                  record-lifnr_063.

  PERFORM bdc_transaction USING 'MEK1'.

ENDFORM.                                                    " CALL_MEK1
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
  IF p_all = 'X'.
    PERFORM process_data.
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
    t_poitemx-po_itemx   = 'X'.
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
*
*    APPEND t_pocond.
*    APPEND t_pocondx.

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

  IF grid_container IS INITIAL. "/Not Created Control for ALV GRID
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

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  CREATE OBJECT alv_grid
    EXPORTING
      i_parent      = grid_container
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

  CREATE OBJECT g_event_receiver.
  SET HANDLER g_event_receiver->handle_data_changed FOR alv_grid.
  SET HANDLER g_event_receiver->handle_f4 FOR alv_grid.

  PERFORM f4_field_assign.

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
*&      Form  F4_FIELD_ASSIGN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_field_assign .
  DATA:
    lt_f4 TYPE lvc_t_f4,
    ls_f4 TYPE lvc_s_f4.

  ls_f4-fieldname  = 'MWSK1'.
  ls_f4-register   = 'X'.
* ls_f4-getbefore  = 'X'.
  ls_f4-chngeafter = 'X'.
  INSERT ls_f4 INTO TABLE lt_f4.

  INSERT ls_f4 INTO TABLE lt_f4.

  CALL METHOD alv_grid->register_f4_for_fields
    EXPORTING
      it_f4 = lt_f4.

ENDFORM.                    " F4_FIELD_ASSIGN
*&---------------------------------------------------------------------*
*&      Form  HANDLE_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_FIELDNAME  text
*      -->P_E_FIELDVALUE  text
*      -->P_ES_ROW_NO  text
*      -->P_ER_EVENT_DATA  text
*      -->P_ET_BAD_CELLS  text
*      -->P_E_DISPLAY  text
*----------------------------------------------------------------------*
FORM handle_f4  USING    u_fieldname
                         u_fieldvalue
                         u_row_no
                         u_event_data
                         u_bad_cells
                         u_display.
  IF u_fieldname = 'MWSK1'.
    PERFORM f4_mwskz.
  ENDIF.
ENDFORM.                                                    "handle_f4
*&---------------------------------------------------------------------*
*&      Form  F4_MWSKZ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_mwskz .
  DATA : l_line TYPE sy-tabix,
         l_index LIKE sy-tabix.

  DATA : ct_cells    TYPE lvc_t_cell,
          l_cell     TYPE lvc_s_cell.
  DATA: l_text1 LIKE t007s-text1,
        l_mwskz LIKE f4_mwskz-mwskz.
  DATA: lw_mwskz LIKE f4_mwskz.

  REFRESH : f4_mwskz. ", field_tab.

*--> Read Dynpro Value from Screen.
  CALL METHOD alv_grid->get_selected_cells
    IMPORTING
      et_cell = ct_cells.

  READ TABLE ct_cells INTO l_cell INDEX 1.
  READ TABLE it_po INDEX l_cell-row_id-index.

  SELECT mwskz INTO CORRESPONDING FIELDS OF TABLE f4_mwskz
    FROM t007a
    WHERE kalsm = 'TAXUS'.

  LOOP AT f4_mwskz INTO lw_mwskz.
    l_index = sy-tabix.
    l_mwskz = lw_mwskz-mwskz.
    SELECT SINGLE text1 INTO l_text1
       FROM t007s
      WHERE spras = 'EN'
        AND kalsm = 'TAXUS'
        AND mwskz = l_mwskz.
    IF sy-subrc = 0.
      lw_mwskz-text1 = l_text1.
      MODIFY f4_mwskz FROM lw_mwskz INDEX l_index.
    ENDIF.
    CLEAR: f4_mwskz.
  ENDLOOP.

  IF sy-subrc = 0.
*--> pop up
    PERFORM f4_mwskz_fieldcat.
    PERFORM f4_mwskz_popup USING l_cell-row_id-index.

  ELSE.
*    MESSAGE i009(zz) WITH text-001.
  ENDIF.
ENDFORM.                                                    " F4_MWSKZ
*&---------------------------------------------------------------------*
*&      Form  F4_MWSKZ_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_mwskz_fieldcat .
  DATA : l_fieldcat TYPE slis_fieldcat_alv.

  REFRESH it_fieldcat2.

  l_fieldcat-fieldname             = 'MWSKZ'.
  l_fieldcat-ref_tabname           = 'T007A'.
  APPEND l_fieldcat TO it_fieldcat2.

  l_fieldcat-fieldname             = 'TEXT1'.
  l_fieldcat-ref_tabname           = 'T007S'.
  APPEND l_fieldcat TO it_fieldcat2.

ENDFORM.                    " F4_MWSKZ_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  F4_MWSKZ_POPUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_CELL_ROW_ID_INDEX  text
*----------------------------------------------------------------------*
FORM f4_mwskz_popup  USING l_row_id_index.
  DATA : es_selfield   TYPE slis_selfield,
         data_protocol TYPE REF TO cl_alv_changed_data_protocol.

  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      i_selection = 'X'
      i_tabname   = 'F4_MWSKZ'
      it_fieldcat = it_fieldcat2
    IMPORTING
      es_selfield = es_selfield
    TABLES
      t_outtab    = f4_mwskz.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
** selecting value in pop-up screen
    READ TABLE f4_mwskz INDEX es_selfield-tabindex.

* modify cell

    it_po-mwsk1 = f4_mwskz-mwskz.
    MODIFY it_po INDEX l_row_id_index.
    PERFORM gui_alv_refresh.
  ENDIF.
ENDFORM.                    " F4_MWSKZ_POPUP
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

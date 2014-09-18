REPORT zemm_po_gr_inv_completion MESSAGE-ID zmmm.
************************************************************************
* Program Name      : ZEMM_PO_GR_INV_COMPLETION
* Author            :
* Creation Date     : 09/26/2013
* Development Request No :
* Description       : Update Findal Delivery/Final Inv indicator
*
* Modification Logs
* Date            Developer        RequestNo      Description
************************************************************************

TABLES: ekko, ekpo, ekbe .

*DATA: BEGIN OF it_ekpo OCCURS 0,
*       ebeln LIKE ekpo-ebeln,
*       ebelp LIKE ekpo-ebelp,
**       GR_BUDAT LIKE EKBE-BUDAT,
**       IV_BUDAT LIKE EKBE-BUDAT,
*       bedat LIKE ekko-bedat,
*       lifnr LIKE ekko-lifnr,
*       matnr LIKE ekpo-matnr,
**       MSGTXT(50),
**       MSGTYP,
*       END OF it_ekpo.

DATA: BEGIN OF it_data OCCURS 0,
       ebeln LIKE ekpo-ebeln,
       ebelp LIKE ekpo-ebelp,
       budat LIKE ekbe-budat,
       bedat LIKE ekko-bedat,
       lifnr LIKE ekko-lifnr,
       matnr LIKE ekpo-matnr,
       bewtp LIKE ekbe-bewtp,
*       MSGTXT(50),
*       MSGTYP,
       END OF it_data.

DATA: BEGIN OF it_output OCCURS 0,
       ebeln LIKE ekpo-ebeln,
       ebelp LIKE ekpo-ebelp,
       bedat LIKE ekko-bedat,
       gr_budat LIKE ekbe-budat,
       iv_budat LIKE ekbe-budat,
       lifnr LIKE ekko-lifnr,
       matnr LIKE ekpo-matnr,
       msgtxt(50),
       msgtyp,
       END OF it_output.

DATA: it_sel_po LIKE TABLE OF it_output WITH HEADER LINE.

*DATA: BEGIN OF it_sel_po OCCURS 0,
*       ebeln LIKE ekpo-ebeln,
*       ebelp LIKE ekpo-ebelp,
*       bedat LIKE ekko-bedat,
*       gr_budat LIKE ekbe-budat,
*       iv_budat LIKE ekbe-budat,
*       lifnr LIKE ekko-lifnr,
*       matnr LIKE ekpo-matnr,
*       msgtxt(50),
*       msgtyp,
*       END OF it_sel_po.

DATA: BEGIN OF it_ekbe OCCURS 0,
      ebeln LIKE ekbe-ebeln,
      ebelp LIKE ekbe-ebelp,
        bewtp LIKE ekbe-bewtp,
      budat LIKE ekbe-budat,
*      MENGE LIKE EKBE-MENGE,
*      BELNR LIKE EKBE-BELNR,
      END OF it_ekbe.

DATA : it_pohead LIKE bapimepoheader,
       it_poheadx LIKE bapimepoheaderx,
       it_poitem LIKE bapimepoitem OCCURS 0 WITH HEADER LINE,
       it_poitemx LIKE bapimepoitemx OCCURS 0 WITH HEADER LINE,
       it_return  LIKE bapiret2 OCCURS 0 WITH HEADER LINE.

DATA: v_ponum LIKE bapimepoheader-po_number,
      v_item LIKE bapimepoitem-po_item,
      v_final LIKE bapimepoitem-final_inv VALUE 'X',
      v_no_more_gr LIKE bapimepoitem-final_inv VALUE 'X',
      v_finalx LIKE bapimepoitemx-final_inv VALUE 'X'.

DATA:  w_repid LIKE sy-repid,
       w_cnt TYPE i,
       ok_code LIKE sy-ucomm.

DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldcat_fi  TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldcat_co  TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE,
       it_fieldcat_det TYPE lvc_t_fcat WITH HEADER LINE. "/Detail

DATA : wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
       w_fieldname    LIKE LINE OF it_fieldcat.

DATA: wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
      wa_variant TYPE disvariant.      "for parameter IS_VARIANT


DATA: wa_custom_control TYPE        scrfname VALUE 'ALV_CONTAINER',
      alv_grid          TYPE REF TO cl_gui_alv_grid,
      g_docking_container    TYPE REF TO cl_gui_docking_container.



SELECTION-SCREEN BEGIN OF BLOCK blk
                          WITH FRAME TITLE text-005.
*SELECTION-SCREEN BEGIN OF LINE.
*PARAMETERS R1 RADIOBUTTON GROUP RAD1.
*SELECTION-SCREEN COMMENT 5(15) TEXT-002 FOR FIELD S_EBELN.
*SELECT-OPTIONS: S_EBELN FOR EKKO-EBELN.
*SELECTION-SCREEN END   OF LINE.
*
*SELECTION-SCREEN BEGIN OF LINE.
*PARAMETERS R2 RADIOBUTTON GROUP RAD1.
*SELECTION-SCREEN COMMENT 5(18) TEXT-003 FOR FIELD P_DATE.
*PARAMETERS P_DATE TYPE SY-DATUM DEFAULT SY-DATUM.
*SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: r_opo RADIOBUTTON GROUP rad1.
SELECTION-SCREEN COMMENT 5(15) text-001.
PARAMETERS: r_gr RADIOBUTTON GROUP rad1.
SELECTION-SCREEN COMMENT 25(15) text-002.
PARAMETERS: r_inv RADIOBUTTON GROUP rad1.
SELECTION-SCREEN COMMENT 45(15) text-003.
PARAMETERS: r_all RADIOBUTTON GROUP rad1.
SELECTION-SCREEN COMMENT 65(15) text-004.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN SKIP.
SELECT-OPTIONS: s_ebeln FOR ekko-ebeln,
                s_bedat FOR ekko-bedat,
                s_lifnr FOR ekko-lifnr,
                s_matnr FOR ekpo-matnr,
                s_bsart FOR ekko-bsart,
                s_knttp FOR ekpo-knttp.
SELECTION-SCREEN SKIP.
PARAMETERS: p_grdt TYPE i DEFAULT '730',
            p_invdt TYPE i DEFAULT '730'.

SELECTION-SCREEN END OF BLOCK blk.


START-OF-SELECTION.

  PERFORM get_data.

  IF it_output[] IS INITIAL.
  ELSE.
    IF sy-batch = 'X'.
      PERFORM call_update.
    ELSE.
      CALL SCREEN 0200.
    ENDIF.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  DATA: l_index LIKE sy-tabix,
        l_date LIKE sy-datum.

  REFRESH: it_data, it_ekbe.
  CASE 'X'.  " Open PO
    WHEN r_opo.
      SELECT a~ebeln ebelp bedat lifnr matnr " ELIKZ EREKZ
        INTO CORRESPONDING FIELDS OF TABLE it_data
        FROM ekko AS a
        INNER JOIN ekpo AS b
        ON a~ebeln = b~ebeln
        WHERE a~ebeln IN s_ebeln
          AND bedat IN s_bedat
          AND lifnr IN s_lifnr
          AND bsart IN s_bsart
          AND a~bstyp = 'F'
          AND matnr IN s_matnr
          AND knttp IN s_knttp
          AND elikz = ' '
          AND b~loekz = ' '.

      IF sy-subrc <> 0.
        WRITE : 'There Are No Purchase Orders' .
        EXIT.
      ENDIF.

      SORT it_data BY ebeln ebelp.
      DELETE ADJACENT DUPLICATES FROM it_data COMPARING ebeln ebelp.

      SELECT ebeln ebelp budat
        INTO CORRESPONDING FIELDS OF TABLE it_ekbe
        FROM ekbe
        FOR ALL ENTRIES IN it_data
        WHERE ebeln = it_data-ebeln
          AND ebelp = it_data-ebelp.

      SORT it_ekbe BY ebeln ebelp.

      LOOP AT it_data.
        l_index = sy-tabix.
        READ TABLE it_ekbe WITH KEY ebeln = it_data-ebeln
                                    ebelp = it_data-ebelp
                            BINARY SEARCH.
        IF sy-subrc = 0.
          DELETE it_data INDEX l_index.
        ELSE.
          MOVE-CORRESPONDING it_data TO it_output.
          APPEND it_output.
        ENDIF.
      ENDLOOP.

    WHEN r_gr.   " GR
      l_date = sy-datum - p_grdt.
      SELECT a~ebeln b~ebelp c~budat bedat lifnr
             b~matnr bewtp
      INTO CORRESPONDING FIELDS OF TABLE it_data
      FROM ekko AS a
      INNER JOIN ekpo AS b
      ON a~ebeln = b~ebeln
        INNER JOIN ekbe AS c
      ON b~ebeln = c~ebeln
        AND b~ebelp = c~ebelp
      WHERE a~ebeln IN s_ebeln
        AND bedat IN s_bedat
        AND lifnr IN s_lifnr
                  AND bsart IN s_bsart
        AND a~bstyp = 'F'
        AND b~matnr IN s_matnr
        AND knttp IN s_knttp
              AND b~elikz = ' '
          AND b~loekz = ' '
      AND c~budat <= l_date
      AND bewtp = 'E'.

      IF sy-subrc <> 0.
        WRITE : 'There Are No Purchase Orders' .
        EXIT.
      ENDIF.

      SORT it_data BY ebeln ebelp budat DESCENDING.
      DELETE ADJACENT DUPLICATES FROM it_data COMPARING
          ebeln ebelp.

      SELECT ebeln ebelp budat
        INTO CORRESPONDING FIELDS OF TABLE it_ekbe
        FROM ekbe
        FOR ALL ENTRIES IN it_data
        WHERE ebeln = it_data-ebeln
          AND ebelp = it_data-ebelp
          AND bewtp = 'Q'.

      SORT it_ekbe BY ebeln ebelp.

      LOOP AT it_data.
        l_index = sy-tabix.
        READ TABLE it_ekbe WITH KEY ebeln = it_data-ebeln
                                    ebelp = it_data-ebelp
                            BINARY SEARCH.
        IF sy-subrc = 0.
          DELETE it_data INDEX l_index.
        ELSE.
          MOVE-CORRESPONDING it_data TO it_output.
          it_output-gr_budat = it_data-budat.
          APPEND it_output.
        ENDIF.
      ENDLOOP.

    WHEN r_inv.   " Invocie
      l_date = sy-datum - p_invdt.
      SELECT a~ebeln b~ebelp c~budat bedat lifnr
             b~matnr bewtp
      INTO CORRESPONDING FIELDS OF TABLE it_data
      FROM ekko AS a
      INNER JOIN ekpo AS b
      ON a~ebeln = b~ebeln
        INNER JOIN ekbe AS c
      ON b~ebeln = c~ebeln
        AND b~ebelp = c~ebelp
      WHERE a~ebeln IN s_ebeln
        AND bedat IN s_bedat
        AND lifnr IN s_lifnr
                  AND bsart IN s_bsart
        AND a~bstyp = 'F'
        AND b~matnr IN s_matnr
                  AND knttp IN s_knttp
        AND b~erekz = ' '
          AND b~loekz = ' '
      AND c~budat <= l_date
      AND bewtp = 'Q'.

      IF sy-subrc <> 0.
        WRITE : 'There are no purchase orders' .
        EXIT.
      ENDIF.

      SORT it_data BY ebeln ebelp budat DESCENDING.
      DELETE ADJACENT DUPLICATES FROM it_data COMPARING
          ebeln ebelp.

      SELECT ebeln ebelp budat
        INTO CORRESPONDING FIELDS OF TABLE it_ekbe
        FROM ekbe
        FOR ALL ENTRIES IN it_data
        WHERE ebeln = it_data-ebeln
          AND ebelp = it_data-ebelp
          AND bewtp = 'E'.

      SORT it_ekbe BY ebeln ebelp.

      LOOP AT it_data.
        l_index = sy-tabix.
        MOVE-CORRESPONDING it_data TO it_output.
        READ TABLE it_ekbe WITH KEY ebeln = it_data-ebeln
                                    ebelp = it_data-ebelp
                            BINARY SEARCH.
        IF sy-subrc = 0.
          it_output-gr_budat = it_ekbe-budat.
        ENDIF.
        it_output-iv_budat = it_data-budat.
        APPEND it_output.
      ENDLOOP.

    WHEN r_all.
      SELECT a~ebeln b~ebelp bedat lifnr
             b~matnr " BEWTP
      INTO CORRESPONDING FIELDS OF TABLE it_data
      FROM ekko AS a
      INNER JOIN ekpo AS b
      ON a~ebeln = b~ebeln
      WHERE a~ebeln IN s_ebeln
        AND bedat IN s_bedat
        AND lifnr IN s_lifnr
                  AND bsart IN s_bsart
       AND a~bstyp = 'F'
        AND matnr IN s_matnr
                  AND knttp IN s_knttp
        AND ( b~elikz = ' ' OR  b~erekz = ' ' )
        AND b~loekz = ' '.

      IF sy-subrc <> 0.
        WRITE : 'There are no purchase order' .
        EXIT.
      ENDIF.

      SORT it_data BY ebeln ebelp.
      DELETE ADJACENT DUPLICATES FROM it_data COMPARING ebeln ebelp.

      SELECT ebeln ebelp bewtp budat
        INTO CORRESPONDING FIELDS OF TABLE it_ekbe
        FROM ekbe
        FOR ALL ENTRIES IN it_data
        WHERE ebeln = it_data-ebeln
          AND ebelp = it_data-ebelp.

      SORT it_ekbe BY ebeln ebelp bewtp budat DESCENDING.
      LOOP AT it_data.
        l_index = sy-tabix.
        MOVE-CORRESPONDING it_data TO it_output.
        READ TABLE it_ekbe WITH KEY ebeln = it_data-ebeln
                                    ebelp = it_data-ebelp
                                    bewtp = 'E'
                            BINARY SEARCH.
        IF sy-subrc = 0.
          it_output-gr_budat = it_ekbe-budat.
        ENDIF.
        READ TABLE it_ekbe WITH KEY ebeln = it_data-ebeln
                                   ebelp = it_data-ebelp
                                   bewtp = 'Q'
                           BINARY SEARCH.
        IF sy-subrc = 0.
          it_output-iv_budat = it_ekbe-budat.
        ENDIF.
        APPEND it_output.
      ENDLOOP.

  ENDCASE.

*     CALL FUNCTION 'GET_CURRENT_YEAR'
*     EXPORTING
*        BUKRS         = 'H201'
*        DATE          = P_DATE
*     IMPORTING
**      CURRM         =
*        CURRY         = V_YEAR
**      PREVM         =
**      PREVY         =
  .

ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  prepare_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data.
  DATA: l_index LIKE sy-tabix,
        l_fail TYPE i,
        l_succ TYPE i.

  REFRESH: it_return.
  SORT it_sel_po BY ebeln ebelp.

  LOOP AT it_sel_po.
    l_index = sy-tabix.
    AT NEW ebeln.
      v_ponum = it_sel_po-ebeln.
    ENDAT.
    AT NEW ebelp.
      v_item = it_sel_po-ebelp.
    ENDAT.

    AT END OF ebelp.
*       Appending data into item level.
      it_poitem-po_item   = v_item.
      it_poitem-no_more_gr = v_no_more_gr.
      it_poitem-final_inv = v_final.
      APPEND it_poitem.
      CLEAR it_poitem.

*       Appending data into X structure
      it_poitemx-po_item   = v_item.
      it_poitemx-no_more_gr = v_finalx.
      it_poitemx-final_inv = v_finalx.
      APPEND it_poitemx.
      CLEAR it_poitemx.
    ENDAT.

    AT END OF ebeln.
      IF NOT it_poitem[] IS INITIAL.

        CALL FUNCTION 'BAPI_PO_CHANGE'
          EXPORTING
            purchaseorder = v_ponum
          TABLES
            return        = it_return
            poitem        = it_poitem
            poitemx       = it_poitemx.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*  EXPORTING
*    WAIT          =
*  IMPORTING
*    RETURN        =
                  .

        READ TABLE it_return WITH KEY  type = 'E'.
        IF sy-subrc = 0.
          it_sel_po-msgtxt = it_return-message.
          it_sel_po-msgtyp = it_return-type.
          l_fail = l_fail + 1.
        ELSE.
          it_sel_po-msgtxt = 'Successfully Update'.
          it_sel_po-msgtyp = 'S'.
          l_succ = l_succ + 1.
        ENDIF.

        MODIFY it_sel_po INDEX l_index TRANSPORTING msgtxt msgtyp.
        CLEAR it_sel_po.
      ENDIF.
      CLEAR: v_ponum, it_poitem, it_poitem[], it_poitemx, it_poitemx[],
             it_return, it_return[].
    ENDAT.
  ENDLOOP.

  MESSAGE s999 WITH 'Successful updated PO: ' l_succ
                    '**** Failed PO:' l_fail.

  LOOP AT it_sel_po.
    READ TABLE it_output WITH KEY ebeln = it_sel_po-ebeln
                              ebelp = it_sel_po-ebelp
                              BINARY SEARCH.
    IF sy-subrc = 0.
      l_index = sy-tabix.
      it_output-msgtxt  = it_sel_po-msgtxt.
      it_output-msgtyp  = it_sel_po-msgtyp.
      MODIFY it_output INDEX l_index TRANSPORTING msgtxt msgtyp.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " prepare_data
*&---------------------------------------------------------------------*
*&      Form  write_outpt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_outpt.
  SORT it_output.
  LOOP AT it_output.
    ULINE AT /(75).
    IF it_output-msgtyp = 'E'.
      NEW-LINE.

      WRITE :  '|', it_output-ebeln, '|', it_output-ebelp, '|',
               it_output-msgtxt COLOR 6, '|'.

    ELSE.
      NEW-LINE.
      WRITE : / '|', it_output-ebeln, '|', it_output-ebelp, '|',
               it_output-msgtxt COLOR 5, '|'.
    ENDIF.
  ENDLOOP.
  ULINE AT /(75).
ENDFORM.                    " write_outpt
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
** On 08/26/13 by Furong
*  IF grid_container IS INITIAL. "/Not Created Control for ALV GRID
  IF g_docking_container IS INITIAL.
** End on 08/26/13
    PERFORM create_container_n_object.
    PERFORM set_attributes_alv_grid.
    PERFORM build_sortcat_display.
    PERFORM build_field_catalog USING 'IT_OUTPUT'.
    PERFORM assign_itab_to_alv.
*    PERFORM sssign_event_9000.
  ELSE.
    CALL METHOD alv_grid->refresh_table_display.
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
  DATA:   l_repid LIKE sy-repid,
          l_dynnr LIKE sy-dynnr.

** O 08/26/13 By Furong
*  CREATE OBJECT grid_container
*          EXPORTING container_name = wa_custom_control
*          EXCEPTIONS
*           cntl_error = 1
*           cntl_system_error = 2
*           create_error = 3
*           lifetime_error = 4
*           lifetime_dynpro_dynpro_link = 5.
  l_repid = sy-repid.
  l_dynnr = sy-dynnr.
  CREATE OBJECT g_docking_container
    EXPORTING
      repid     = l_repid
      dynnr     = l_dynnr
      side      = cl_gui_docking_container=>dock_at_bottom
*     RATIO     = 90
      extension = 2000.
** End on 08/26/13
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
*         EXPORTING i_parent = grid_container
         EXPORTING i_parent = g_docking_container
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
  wa_is_layout-edit       = ' '.      "/Edit Mode Enable
  wa_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  wa_is_layout-language   = sy-langu. "/Language Key
*  wa_is_layout-cwidth_opt = 'X'.   "/optimizes the column width
*  wa_is_layout-info_fname = 'IF'.
*  wa_is_layout-ctab_fname = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

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
*
*  it_sort-spos           = 3.
*  it_sort-fieldname      = 'WERKS'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
*
*  it_sort-spos           = 4.
*  it_sort-fieldname      = 'DISPO'.
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
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'PO Number',
                                  'E' 'OUTPUTLEN'   '10',

                                   'S' 'EBELP'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Plant',
                                  'E' 'OUTPUTLEN'   '4',

                                  'S' 'BEDAT'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Cr. Date',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'GR_BUDAT'         ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'GR Date',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'IV_BUDAT'        ' ',
                                    ' ' 'COLTEXT'     'Inv Date',
                                    'E' 'OUTPUTLEN'   '10',

                                  'S' 'LIFNR'       ' ',
                                  ' ' 'COLTEXT'     'Vendor',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'MATNR'         ' ',
                                  ' ' 'COLTEXT'     'Material No',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'MSGTXT'       ' ',
                                  ' ' 'COLTEXT'     'Remarks',
                                  'E' 'OUTPUTLEN'   '40'.

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
*     it_toolbar_excluding = it_toolbar_excluding[]
    CHANGING
      it_fieldcatalog      = it_fieldcat[]
      it_outtab            = it_output[].
*               it_sort          = it_sort[].

ENDFORM.                    " assign_itab_to_alv
*&----------------------------------------------------------------
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
    WHEN 'POST'.
      PERFORM call_update.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  CALL_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_update .

  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i.
  RANGES: lr_matnr FOR mara_matnr.

  REFRESH: it_sel_po.
  IF sy-batch = 'X'.
    it_sel_po[] = it_output[].
  ELSE.
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

    READ TABLE it_output INDEX lt_rows-index.
    IF sy-subrc <> 0.
      MESSAGE e000(zz) WITH text-m01.
    ENDIF.


    LOOP AT lt_rows.
      READ TABLE it_output INDEX lt_rows-index.
      MOVE-CORRESPONDING it_output TO it_sel_po.
      APPEND it_sel_po.
      CLEAR: it_sel_po.
    ENDLOOP.
  ENDIF.

  PERFORM process_data.

*  PERFORM write_outpt.
ENDFORM.                    " CALL_UPDATE

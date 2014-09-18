*&----------------------------------------------------------------------
*& Program ID        : ZFIE_CLEAR_DP_PAY_BLOCK
*& Title             : [FI] - Clear Downpayment Payment Block
*& Created by        : Valerian Utama
*& Created on        : 06/18/2012
*& Specifications By : Calvin Kong
*& Reference Pgm     : N/A
*& Description       : Clear Downpayment Payment Block
*&
*& Modification Log
*& Date        Developer Issue No    Description
*&======================================================================
*& 06/18/2012  Valerian  UD1K955174  Initial Program Development
*& 07/19/2012  Valerian  UD1K955212  Fix update mode when calling the
*&                                   BDC Data
*&                                   Use new data source and adjust the
*&                                   program logic.
*&----------------------------------------------------------------------

REPORT  zfie_clear_dp_pay_block MESSAGE-ID zmfi.

* Define data for ALV
TYPE-POOLS: slis.

DATA : gt_fieldcat TYPE slis_t_fieldcat_alv,
       gs_fieldcat TYPE slis_fieldcat_alv.

* Define internal table for BDC
DATA: gt_bdc TYPE TABLE OF bdcdata    WITH HEADER LINE,
      gt_msg TYPE TABLE OF bdcmsgcoll WITH HEADER LINE.

* Define program data
TABLES: bsik.

DATA: BEGIN OF gt_pay OCCURS 0,
        lifnr LIKE bsik-lifnr,
        gjahr LIKE bsik-gjahr,
        belnr LIKE bsik-belnr,
        buzei LIKE bsik-buzei,
        blart LIKE bsik-blart,
        ebeln LIKE bsik-ebeln,
        ebelp LIKE bsik-ebelp,
        rebzg LIKE bsik-rebzg,
        zlspr LIKE bsik-zlspr,
        sgtxt LIKE bsik-sgtxt,
        aufnr LIKE bsik-aufnr,
        budat LIKE bsik-budat,
      END OF gt_pay.

DATA: gt_inv LIKE gt_pay OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF gt_sdinv OCCURS 0,
       belnr LIKE rseg-belnr,
       gjahr LIKE rseg-gjahr,
       buzei LIKE rseg-buzei,
       ebeln LIKE rseg-ebeln,
       ebelp LIKE rseg-ebelp,
       menge LIKE rseg-menge,
      END OF gt_sdinv.

DATA: BEGIN OF gt_po OCCURS 0,
       ebeln LIKE rseg-ebeln,
       ebelp LIKE rseg-ebelp,
       menge LIKE rseg-menge,
      END OF gt_po.

DATA: BEGIN OF gt_out OCCURS 0,
        lifnr LIKE bsik-lifnr,
        gjahr LIKE rseg-gjahr,
        belnr LIKE bsik-belnr,
        buzei LIKE bsik-buzei,
        blart LIKE bsik-blart,
        ebeln LIKE bsik-ebeln,
        ebelp LIKE bsik-ebelp,
        beln1 LIKE bsik-belnr,
        beln2 LIKE bsik-belnr,
        zlspr LIKE bsik-zlspr,
        aufnr LIKE bsik-aufnr,
        budat LIKE bsik-budat,
        icon(4) TYPE c,
        msg(99) TYPE c,
      END OF gt_out.

* Exclude documents
DATA: BEGIN OF gt_excdoc OCCURS 0,
        gjahr LIKE bsik-gjahr,
        belnr LIKE bsik-belnr,
      END OF gt_excdoc.

RANGES: r_zlspr FOR bsik-zlspr.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-t01.
PARAMETERS: p_bukrs TYPE bsik-bukrs OBLIGATORY DEFAULT 'H201'.
SELECT-OPTIONS: s_gjahr FOR bsik-gjahr OBLIGATORY
                        DEFAULT sy-datum(4).

SELECTION-SCREEN SKIP.
SELECT-OPTIONS: s_lifnr FOR bsik-lifnr,
                s_belnr FOR bsik-belnr.

SELECTION-SCREEN SKIP.
PARAMETERS: p_all RADIOBUTTON GROUP pblk DEFAULT 'X',
            p_blk RADIOBUTTON GROUP pblk,
            p_clr RADIOBUTTON GROUP pblk.

SELECTION-SCREEN END OF BLOCK blk1.

SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE text-t02.
PARAMETERS: p_post AS CHECKBOX,
            p_mode DEFAULT 'N' NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK blk2.

AT SELECTION-SCREEN.
  CASE 'X'.
    WHEN p_blk.
      r_zlspr-sign   = 'I'.
      r_zlspr-option = 'EQ'.
      r_zlspr-low    = 'A'.
      APPEND r_zlspr.
    WHEN p_clr.
      r_zlspr-sign   = 'I'.
      r_zlspr-option = 'EQ'.
      APPEND r_zlspr.
  ENDCASE.

START-OF-SELECTION.
  PERFORM get_data.
  PERFORM process_data.
  PERFORM post_data.
  PERFORM display_data.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       Display Data
*----------------------------------------------------------------------*
FORM display_data.
  PERFORM fieldcat_init     USING gt_fieldcat[].
  PERFORM alv_grid_display  TABLES gt_out.
ENDFORM.                    " DISPLAY_DATA

*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       Initialize Field Catalog
*----------------------------------------------------------------------*
*      -->ft_fieldcat  Field Catalog Value
*----------------------------------------------------------------------*
FORM fieldcat_init USING ft_fieldcat TYPE slis_t_fieldcat_alv.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
      i_internal_tabname     = 'GT_OUT'
      i_inclname             = sy-repid
    CHANGING
      ct_fieldcat            = ft_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Set Key field.
  LOOP AT ft_fieldcat INTO gs_fieldcat.
    IF gs_fieldcat-fieldname = 'LIFNR'
    OR gs_fieldcat-fieldname = 'BELNR'
    OR gs_fieldcat-fieldname = 'BUZEI'.

      gs_fieldcat-key = 'X'.
    ELSE.
      CLEAR gs_fieldcat-key.
    ENDIF.

    MODIFY ft_fieldcat FROM gs_fieldcat TRANSPORTING key.
  ENDLOOP.

* Change Description
  PERFORM change_desc USING 'BELN1' 'FI Inv.Doc.'.
  PERFORM change_desc USING 'BELN2' 'SD Inv.Doc.'.
  PERFORM change_desc USING 'EBELP' 'PO Item'.
  PERFORM change_desc USING 'ICON'  'Status'.
  PERFORM change_desc USING 'MSG'   'Message'.

ENDFORM.                    " fieldcat_init
*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*       Display Data using ALV Grid
*----------------------------------------------------------------------*
FORM alv_grid_display TABLES ft_outtab TYPE table.

  DATA: l_repid TYPE sy-repid.
  DATA : gs_layout TYPE slis_layout_alv.

  gs_layout-colwidth_optimize = 'X'.
  gs_layout-zebra = 'X'.

  l_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = l_repid
      is_layout          = gs_layout
      it_fieldcat        = gt_fieldcat
    TABLES
      t_outtab           = ft_outtab
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " ALV_GRID_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  CHANGE_DESC
*&---------------------------------------------------------------------*
*       Change ALV field description
*----------------------------------------------------------------------*
*      -->P_FIELD   Field Name
*      -->P_DESC    Field Description
*----------------------------------------------------------------------*
FORM change_desc  USING    p_field TYPE c
                           p_desc  TYPE c.

  DATA: gs_fieldcat TYPE slis_fieldcat_alv.

  READ TABLE gt_fieldcat INTO gs_fieldcat
                         WITH KEY fieldname = p_field.
  IF sy-subrc = 0.
    gs_fieldcat-seltext_l    = p_desc.
    gs_fieldcat-seltext_m    = p_desc.
    gs_fieldcat-seltext_s    = p_desc.
    gs_fieldcat-reptext_ddic = p_desc.
    MODIFY gt_fieldcat FROM gs_fieldcat INDEX sy-tabix.
  ENDIF.
ENDFORM.                    " CHANGE_DESC
*---------------------------------------------------------------------*
*       Form DYNPRO                                                   *
*---------------------------------------------------------------------*
FORM dynpro USING p_dynbegin p_name p_value.
  CLEAR gt_bdc.

  IF p_dynbegin = 'X'.
    gt_bdc-program = p_name.
    gt_bdc-dynpro = p_value.
    gt_bdc-dynbegin = p_dynbegin.
  ELSE.
    gt_bdc-fnam = p_name.
    gt_bdc-fval = p_value.
  ENDIF.

  APPEND gt_bdc.

ENDFORM.                    " DYNPRO
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       Get data from database
*----------------------------------------------------------------------*
FORM get_data.
  DATA: l_tabix TYPE sy-tabix.

  SELECT lifnr gjahr belnr buzei blart ebeln ebelp rebzg zlspr sgtxt
         aufnr budat
    INTO CORRESPONDING FIELDS OF TABLE gt_pay
    FROM bsik
   WHERE bukrs = p_bukrs
     AND lifnr IN s_lifnr
     AND gjahr IN s_gjahr
     AND belnr IN s_belnr
     AND blart = 'ZP'
     AND hkont = '0000151020'
     AND zlspr IN r_zlspr.

  IF gt_pay[] IS INITIAL.
    MESSAGE s020.
    STOP.
  ENDIF.

* Get excluded documents.
  LOOP AT gt_pay WHERE sgtxt = 'MR8M'.
    gt_excdoc-gjahr = gt_pay-gjahr.
    gt_excdoc-belnr = gt_pay-belnr.
    APPEND gt_excdoc.
    gt_excdoc-belnr = gt_pay-rebzg.
    APPEND gt_excdoc.
  ENDLOOP.

* Remove excluded documents
  IF NOT gt_excdoc[] IS INITIAL.
    SORT gt_excdoc BY gjahr belnr.

    LOOP AT gt_pay.
      l_tabix = sy-tabix.

      READ TABLE gt_excdoc WITH KEY gjahr = gt_pay-gjahr
                                    belnr = gt_pay-belnr
                                    BINARY SEARCH.
      IF sy-subrc = 0.
        DELETE gt_pay INDEX l_tabix.
      ENDIF.
    ENDLOOP.
  ENDIF.

  SELECT lifnr gjahr belnr buzei blart ebeln ebelp rebzg zlspr sgtxt
         aufnr budat
    INTO CORRESPONDING FIELDS OF TABLE gt_inv
    FROM bsak
   FOR ALL ENTRIES IN gt_pay
   WHERE bukrs = p_bukrs
     AND lifnr = gt_pay-lifnr
     AND gjahr IN s_gjahr
     AND aufnr = gt_pay-aufnr
     AND ebeln = gt_pay-ebeln
     AND ebelp = gt_pay-ebelp
     AND rebzg = gt_pay-belnr
     AND xragl = ' '.

  IF gt_inv[] IS INITIAL.
    MESSAGE s020.
    STOP.
  ENDIF.

* Remove excluded documents
  IF NOT gt_excdoc[] IS INITIAL.

    LOOP AT gt_inv.
      l_tabix = sy-tabix.

      READ TABLE gt_excdoc WITH KEY gjahr = gt_inv-gjahr
                                    belnr = gt_inv-belnr
                                    BINARY SEARCH.
      IF sy-subrc = 0.
        DELETE gt_inv INDEX l_tabix.
      ENDIF.
    ENDLOOP.
  ENDIF.

  SELECT ebeln ebelp menge
    INTO CORRESPONDING FIELDS OF TABLE gt_po
    FROM ekpo
   FOR ALL ENTRIES IN gt_inv
   WHERE ebeln = gt_inv-ebeln
     AND ebelp = gt_inv-ebelp.

  SELECT belnr gjahr buzei ebeln ebelp menge
    INTO CORRESPONDING FIELDS OF TABLE gt_sdinv
    FROM rseg
   FOR ALL ENTRIES IN gt_inv
   WHERE ebeln = gt_inv-ebeln
     AND ebelp = gt_inv-ebelp.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       Process Data
*----------------------------------------------------------------------*
FORM process_data .
  SORT: gt_pay   BY lifnr belnr buzei,
        gt_inv   BY lifnr aufnr ebeln ebelp rebzg,
        gt_po    BY ebeln ebelp,
        gt_sdinv BY ebeln ebelp.

  LOOP AT gt_pay.
    READ TABLE gt_inv WITH KEY lifnr = gt_pay-lifnr
                               aufnr = gt_pay-aufnr
                               ebeln = gt_pay-ebeln
                               ebelp = gt_pay-ebelp
                               rebzg = gt_pay-belnr
                           BINARY SEARCH.
    CHECK sy-subrc = 0.
    MOVE-CORRESPONDING gt_pay TO gt_out.
    gt_out-beln1 = gt_inv-belnr.


    READ TABLE gt_po WITH KEY ebeln = gt_pay-ebeln
                              ebelp = gt_pay-ebelp
                          BINARY SEARCH.
    CHECK sy-subrc = 0.

    READ TABLE gt_sdinv WITH KEY ebeln = gt_pay-ebeln
                                 ebelp = gt_pay-ebelp
                             BINARY SEARCH.
    CHECK sy-subrc = 0.

* Check if PO QTY = SD Invoice QTY
    CHECK gt_po-menge = gt_sdinv-menge.

    gt_out-beln2 = gt_sdinv-belnr.
    APPEND gt_out. CLEAR gt_out.
  ENDLOOP.

  FREE: gt_pay, gt_inv.
ENDFORM.                    " PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  POST_DATA
*&---------------------------------------------------------------------*
*       Unblock the line item
*----------------------------------------------------------------------*
FORM post_data .
  DATA: l_tabix TYPE sy-tabix,
        l_count TYPE i.

  CHECK NOT p_post IS INITIAL.

  LOOP AT gt_out.
    l_tabix = sy-tabix.

    CLEAR: gt_msg, gt_msg[],
           gt_bdc, gt_bdc[].

    PERFORM dynpro USING:
        'X' 'SAPMF05L'        '0102',
        ' ' 'BDC_CURSOR'      'RF05L-BUZEI',
        ' ' 'BDC_OKCODE'      '/00',
        ' ' 'RF05L-BELNR'     gt_out-belnr,
        ' ' 'RF05L-BUKRS'     p_bukrs,
        ' ' 'RF05L-GJAHR'     gt_out-gjahr,
        ' ' 'RF05L-BUZEI'     gt_out-buzei.

    PERFORM dynpro USING:
        'X' 'SAPMF05L'        '0304',
        ' ' 'BDC_CURSOR'      'BSEG-ZLSPR',
        ' ' 'BDC_OKCODE'      '=AE',
        ' ' 'BSEG-ZLSPR'      ' '.

    CALL TRANSACTION 'FB09'   USING gt_bdc
                              MODE p_mode
                              UPDATE 'S'                    "UD1K955212
                              MESSAGES INTO gt_msg.

    IF sy-subrc = 0.
      gt_out-icon = icon_green_light.
    ELSE.
      gt_out-icon = icon_red_light.
    ENDIF.

    DESCRIBE TABLE gt_msg LINES l_count.
    READ TABLE gt_msg INDEX l_count.
    IF sy-subrc = 0.

      CASE gt_msg-msgtyp.
        WHEN 'S'.
          gt_out-icon = icon_green_light.
        WHEN 'E'.
          gt_out-icon = icon_red_light.
        WHEN 'W'.
          gt_out-icon = icon_yellow_light.
      ENDCASE.

      MESSAGE ID gt_msg-msgid TYPE gt_msg-msgtyp NUMBER gt_msg-msgnr
         WITH gt_msg-msgv1 gt_msg-msgv2 gt_msg-msgv3 gt_msg-msgv4
         INTO gt_out-msg.

      MODIFY gt_out INDEX l_tabix TRANSPORTING icon msg.

    ENDIF.
  ENDLOOP.
ENDFORM.                    " POST_DATA

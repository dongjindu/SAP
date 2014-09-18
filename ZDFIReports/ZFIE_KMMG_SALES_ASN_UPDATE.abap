*&----------------------------------------------------------------------
*& Program ID        : ZFIE_KMMG_SALES_ASN_UPDATE
*& Title             : [FI] - KMMG Engine Sales ASN# Update
*& Created by        : Valerian Utama
*& Created on        : 02/22/2011
*& Specifications By : Michael Yoon
*& Reference Pgm     : N/A
*& Description       : Update ASN information for Customer A/R
*&                     Reconciliation Automatically
*&
*& Modification Log
*& Date        Developer Issue No    Description
*&======================================================================
*& 02/22/11    Valerian  UD1K950936  Initial Program Development
*&
*&----------------------------------------------------------------------

REPORT zfie_kmmg_sales_asn_update MESSAGE-ID zmfi.

CONSTANTS: c_bukrs TYPE bsid-bukrs VALUE 'H201',
           c_kunnr TYPE bsid-kunnr VALUE 'AKNH'.

TYPE-POOLS: slis, icon.

DATA: g_success TYPE i,
      g_error   TYPE i.

DATA: gt_fieldcat TYPE slis_t_fieldcat_alv.

DATA: BEGIN OF bdcdata OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF bdcdata.

DATA: BEGIN OF it_message OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF it_message.

DATA: BEGIN OF it_data OCCURS 0,
         bukrs      TYPE bsid-bukrs,
         gjahr      TYPE bsid-gjahr,
         belnr      TYPE bsid-belnr,
         buzei      TYPE bsid-buzei,
         vgbel      TYPE vbrp-vgbel,
         vbeln      TYPE bsid-vbeln,
         budat      TYPE bsid-budat,
         wrbtr      TYPE bsid-wrbtr,
         waers      TYPE bsid-waers,
         posnr      TYPE vbrp-posnr,
       END OF it_data.

DATA: BEGIN OF it_out OCCURS 0.
        INCLUDE STRUCTURE it_data.
DATA:   zuonr      TYPE bsid-zuonr,
        messg(132) TYPE c,
        sel(1)     TYPE c,
        icon(4)    TYPE c,
        color TYPE lvc_t_scol,
      END OF it_out.

DATA: it_out_save LIKE it_out OCCURS 0 WITH HEADER LINE.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_budat FOR it_out-budat OBLIGATORY
                DEFAULT sy-datum.
SELECTION-SCREEN END OF BLOCK blk1.

SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE text-t02.
PARAMETERS: p_mnual RADIOBUTTON GROUP pos DEFAULT 'X',
            p_batch RADIOBUTTON GROUP pos.
SELECTION-SCREEN END OF BLOCK blk2.

AT SELECTION-SCREEN.

START-OF-SELECTION.
  PERFORM select_data.
  PERFORM modify_data.
  PERFORM output_data.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       Select Data
*----------------------------------------------------------------------*
FORM select_data.
  DATA: ls_cellcolour TYPE lvc_s_scol.

  SELECT a~bukrs a~gjahr a~belnr a~buzei b~vgbel a~vbeln a~budat
         a~wrbtr a~waers b~posnr
    INTO CORRESPONDING FIELDS OF TABLE it_data
    FROM bsid AS a JOIN vbrp AS b
         ON a~vbeln = b~vbeln
   WHERE a~bukrs = c_bukrs
     AND a~kunnr = c_kunnr
     AND a~budat IN s_budat.

* Set color for the column
  LOOP AT it_data.
    MOVE-CORRESPONDING it_data TO it_out.
    AT FIRST.
      ls_cellcolour-fname = 'BELNR'.
      ls_cellcolour-color-col = '6'.
      ls_cellcolour-color-int = '0'.
      ls_cellcolour-color-inv = '0'.
      APPEND ls_cellcolour TO it_out-color.
      ls_cellcolour-fname = 'VBELN'.
      APPEND ls_cellcolour TO it_out-color.

      ls_cellcolour-color-col = '4'.
      ls_cellcolour-fname = 'BUDAT'.
      APPEND ls_cellcolour TO it_out-color.
      ls_cellcolour-fname = 'WRBTR'.
      APPEND ls_cellcolour TO it_out-color.
      ls_cellcolour-fname = 'WAERS'.
      APPEND ls_cellcolour TO it_out-color.

      ls_cellcolour-color-col = '5'.
      ls_cellcolour-fname = 'ICON'.
      APPEND ls_cellcolour TO it_out-color.
      ls_cellcolour-fname = 'MESSG'.
      APPEND ls_cellcolour TO it_out-color.
    ENDAT.

    APPEND it_out.
  ENDLOOP.

  FREE it_data.
ENDFORM.                    " SELECT_DATA

*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
*       Process Data
*----------------------------------------------------------------------*
FORM modify_data .
  DATA: l_rec   TYPE i,
        wa_data LIKE it_out,
        wa_save LIKE it_out,
        it_save LIKE it_out OCCURS 0.

  SORT it_out BY bukrs gjahr belnr buzei vgbel.

  LOOP AT it_out.
    wa_data = it_out.

    AT NEW buzei.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
           EXPORTING
                input  = wa_data-vgbel
           IMPORTING
                output = wa_data-zuonr.
      wa_save = wa_data.
    ENDAT.

    AT END OF buzei.
      IF wa_data-vgbel <> wa_save-vgbel.
        CONCATENATE wa_save-zuonr '~' wa_data-vgbel+6(4)
               INTO wa_save-zuonr.

      ENDIF.
      APPEND wa_save TO it_save.
    ENDAT.
  ENDLOOP.

  it_out[] = it_save[].

  IF it_out[] IS INITIAL.
    MESSAGE s011 WITH text-m03.
    STOP.
  ENDIF.

  DESCRIBE TABLE it_out LINES l_rec.
  MESSAGE s000 WITH text-m04 l_rec.

  it_out_save[] = it_out[].
ENDFORM.                    " MODIFY_DATA

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       Display Data
*----------------------------------------------------------------------*
FORM display_data.
  PERFORM fieldcat_init     USING gt_fieldcat[].
  PERFORM alv_grid_display  TABLES it_out.
ENDFORM.                    " DISPLAY_DATA

*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       Initialize Field Catalog
*----------------------------------------------------------------------*
*      -->ft_fieldcat  Field Catalog Value
*----------------------------------------------------------------------*
FORM fieldcat_init USING ft_fieldcat TYPE slis_t_fieldcat_alv .

  DATA: l_pos TYPE i,
        gs_fieldcat TYPE slis_fieldcat_alv.

  DEFINE __catalog.
    l_pos = l_pos + 1.
    clear gs_fieldcat.
    gs_fieldcat-col_pos       = l_pos.
    gs_fieldcat-key           = &1.
    gs_fieldcat-fieldname     = &2.
    gs_fieldcat-seltext_s     = &3.        " Column heading
    gs_fieldcat-seltext_m     = &3.        " Column heading
    gs_fieldcat-outputlen     = &4.        " Column width
    gs_fieldcat-datatype      = &5.        " Data type
    gs_fieldcat-emphasize     = &6.
    gs_fieldcat-cfieldname    = &7.
    gs_fieldcat-lowercase     = 'X'.
    append gs_fieldcat to  ft_fieldcat.
  END-OF-DEFINITION.

  __catalog :
    'X'  'BELNR'      'Doc.Number'       10 'BELNR_D' '' '',
    'X'  'VBELN'      'Bill. Doc.'       10 'VBELN_VF' '' '',
    ' '  'BUDAT'      'Post.Date'        10 'BUDAT' '' '',
    ' '  'WRBTR'      'Amount'           16 'CURR' '' '',
    ' '  'WAERS'      'Curr.'            5  'WAERS' '' '',
    ' '  'ZUONR'      'ASN#'             18 'DZUONR' '' '',
    ' '  'ICON'       'Stat.'            4  'CHAR' '' '',
    ' '  'MESSG'      'Message'          50 'CHAR' '' ''.

* Make ASN# as editable field
  gs_fieldcat-edit = 'X'.
  MODIFY ft_fieldcat FROM gs_fieldcat TRANSPORTING edit
  WHERE fieldname = 'ZUONR'.

ENDFORM.                    " fieldcat_init

*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*       Display Data using ALV Grid
*----------------------------------------------------------------------*
FORM alv_grid_display TABLES ft_outtab.

  DATA: l_repid TYPE sy-repid.
  DATA : gs_layout TYPE slis_layout_alv.

  gs_layout-confirmation_prompt ='X'.
  gs_layout-box_fieldname = 'SEL'.
  gs_layout-box_tabname = 'FT_OUTTAB'.
  gs_layout-key_hotspot = 'X'.
  gs_layout-coltab_fieldname = 'COLOR'.

  l_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program      = l_repid
            i_callback_user_command = 'USER_COMMAND'
            i_callback_top_of_page  = 'TOP_OF_PAGE'
            is_layout               = gs_layout
            it_fieldcat             = gt_fieldcat
       TABLES
            t_outtab                = ft_outtab
       EXCEPTIONS
            program_error           = 1
            OTHERS                  = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " ALV_GRID_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  output_data
*&---------------------------------------------------------------------*
*       Display data or write data to file
*----------------------------------------------------------------------*
FORM output_data.
  CASE 'X'.
    WHEN p_batch.
      PERFORM process_data USING ' '.
  ENDCASE.

  PERFORM display_data.
ENDFORM.                    " output_data

*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
FORM user_command USING fp_ucomm LIKE sy-ucomm
                        fs       TYPE slis_selfield.
  CASE fp_ucomm.
    WHEN '&DATA_SAVE'.
      PERFORM process_data USING 'X'.

      IF sy-subrc <> 0.
        CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
             EXPORTING
                  textline1 = text-m01.
      ENDIF.

      fs-refresh = 'X'.
    WHEN '&IC1'.
      CASE fs-fieldname.
        WHEN 'BELNR'.
          READ TABLE it_out INDEX fs-tabindex.
          IF sy-subrc = 0.
            SET PARAMETER ID: 'BUK' FIELD it_out-bukrs,
                              'BLN' FIELD it_out-belnr,
                              'GJR' FIELD it_out-gjahr,
                              'BUZ' FIELD it_out-buzei.

            CALL TRANSACTION 'FB09' AND SKIP FIRST SCREEN.

          ENDIF.

        WHEN 'VBELN'.
          READ TABLE it_out INDEX fs-tabindex.
          IF sy-subrc = 0.
            SET PARAMETER ID: 'VF' FIELD it_out-vbeln.

            CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
          ENDIF.
      ENDCASE.

  ENDCASE.

ENDFORM.                    "USER_COMMAND

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.
*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  change_data
*&---------------------------------------------------------------------*
*       Change Allocation Number (ZUONR)
*----------------------------------------------------------------------*
FORM change_data.
  DATA: l_rec_msg TYPE i.

  PERFORM bdc_dynpro USING: 'SAPMF05L' '0102'.
  PERFORM bdc_field  USING: 'BDC_OKCODE' '/00',
                            'RF05L-BELNR' it_out-belnr,
                            'RF05L-BUKRS' it_out-bukrs,
                            'RF05L-GJAHR' it_out-gjahr,
                            'RF05L-BUZEI' it_out-buzei.

  PERFORM bdc_dynpro USING: 'SAPMF05L' '0301'.
  PERFORM bdc_field  USING: 'BDC_OKCODE' '=AE',
                            'BSEG-ZUONR' it_out-zuonr.

  CALL TRANSACTION 'FB09'
           USING bdcdata
           MODE 'N'
           UPDATE 'S'
       MESSAGES INTO it_message.

  IF sy-subrc <> 0.
    READ TABLE it_out_save WITH KEY belnr = it_out-belnr.
    IF sy-subrc = 0.
      it_out-zuonr = it_out_save-zuonr.
      MODIFY it_out TRANSPORTING zuonr.
    ENDIF.
    g_error = g_error + 1.
  ELSE.
    g_success = g_success + 1.
  ENDIF.

  DESCRIBE TABLE it_message LINES l_rec_msg.
  READ TABLE it_message INDEX l_rec_msg.
  IF sy-subrc = 0.
    MESSAGE ID it_message-msgid
          TYPE it_message-msgtyp
        NUMBER it_message-msgnr
          WITH it_message-msgv1 it_message-msgv2
               it_message-msgv3 it_message-msgv4
          INTO it_out-messg.

    CASE it_message-msgtyp.
      WHEN 'S'.
        it_out-icon  = icon_green_light.
      WHEN 'E'.
        it_out-icon  = icon_red_light.
      WHEN OTHERS.
        it_out-icon  = icon_yellow_light.
    ENDCASE.

    MODIFY it_out TRANSPORTING messg icon.
  ENDIF.

  REFRESH: bdcdata, it_message.

ENDFORM.                    " change_data

*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM top_of_page.
  DATA: gt_listheader TYPE slis_t_listheader,
        gs_listheader TYPE slis_listheader.

  SELECT SINGLE butxt INTO gs_listheader-info
    FROM t001
   WHERE bukrs = c_bukrs.
  gs_listheader-typ  = 'S'.
  gs_listheader-key  = 'Company Code:'.
  CONCATENATE c_bukrs '-' gs_listheader-info
         INTO gs_listheader-info SEPARATED BY space.
  APPEND gs_listheader TO gt_listheader.

  SELECT SINGLE name1 INTO gs_listheader-info
    FROM kna1
   WHERE kunnr = c_kunnr.
  gs_listheader-typ  = 'S'.
  gs_listheader-key  = 'Customer No.:'.
  CONCATENATE c_kunnr '-' gs_listheader-info
         INTO gs_listheader-info SEPARATED BY space.
  APPEND gs_listheader TO gt_listheader.

  IF sy-batch IS INITIAL OR p_batch IS INITIAL.
    gs_listheader-typ = 'A'.
    gs_listheader-key = 'DESCR'.
    gs_listheader-info = text-m02.
    APPEND gs_listheader TO gt_listheader.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            it_list_commentary = gt_listheader.

ENDFORM.                    "top_of_page

*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       Process Data for selected records
*----------------------------------------------------------------------*
*      -->P_FLAG   Processing Flag
*----------------------------------------------------------------------*
FORM process_data USING p_flag TYPE c.
  CLEAR: g_success, g_error.

  LOOP AT it_out WHERE sel = p_flag.
    PERFORM change_data.
  ENDLOOP.

  IF sy-subrc = 0.
    MESSAGE s000 WITH g_success text-m05 g_error text-m06.
  ENDIF.
ENDFORM.                    " process_data

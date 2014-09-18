*&------------------------------------------------------------------
*& Program ID     : ZMMR30200T
*& Program Name   : Create ASN with Local Text File(WEB)
*& Created by     : Yang
*& Created on     : 05.21.2009
*& Development ID : MM-000
*& Description    : Web upload and  creates with Local Text File
*&                  And the program for ASN
*& To web will count and with under upload function with text
*&and to be a restricted fact where the support does not become,is done
*& Modification Log
*&====================================================================
*& Date        Developer    Request ID    Description
*& 05.21.2009  Yang                           first dev.
*& 04.26.2011  Paul         UD1K951487    Copied
*& 03.03.2012  t00266
*& 05.08.2014  Victor   Use Standard Idoc Function
*& 05.12.2014  Victor   Created screen 100 to display check message
*&--------------------------------------------------------------------
REPORT  zmmr30200t.
TABLES:
  edp21,
  likp,
  tvsa,
  tvty,
  vekp,
  mara.

INCLUDE ole2incl.


DATA:  g_custom_container TYPE REF TO cl_gui_custom_container,
       g_con_grid      TYPE REF TO cl_gui_container,
       g_grid              TYPE REF TO cl_gui_alv_grid.
DATA : gt_exclude   TYPE ui_functions,
       g_container    TYPE scrfname VALUE 'G_CUSTOM_CONTAINER',
       it_fieldname    TYPE slis_t_fieldcat_alv,
       gs_fcat      TYPE lvc_s_fcat,
       gt_fcat      TYPE lvc_t_fcat,
       gs_layout      TYPE lvc_s_layo,
       gs_fcat1     TYPE lvc_s_fcat,
       gs_sort      TYPE lvc_s_sort,
       gt_sort      TYPE lvc_t_sort,
       gs_sort_alv  TYPE slis_sortinfo_alv,
       gt_sort_alv  TYPE slis_t_sortinfo_alv.

DATA : ok_code      TYPE sy-ucomm,
       save_ok      TYPE sy-ucomm.

*----------------------------------------------------------------------*
*    Excel Variables.
*----------------------------------------------------------------------*
DATA: BEGIN OF t_idoc_status OCCURS 0.
        INCLUDE STRUCTURE bdidocstat.
DATA: END OF t_idoc_status.

DATA : xlsdata  LIKE alsmex_tabline OCCURS 0 WITH HEADER LINE.
DATA : xlsdatab LIKE alsmex_tabline OCCURS 0 WITH HEADER LINE.
DATA:
  gf_tddat_col  TYPE i,
  gf_lfdat_col  TYPE i,
  gf_lifex_col  TYPE i,
  gf_bolnr_col  TYPE i,
  gf_traid_col  TYPE i,
  gf_traty_col  TYPE i,
  gf_knote_col  TYPE i,
  gf_exidv2_col TYPE i,
  gf_exidv_col  TYPE i,
  gf_vhilm_col  TYPE i,
  gf_matnr_col  TYPE i,
  gf_maktx_col  TYPE i,
  gf_meins_col  TYPE i,
  gf_lfimg_col  TYPE i,
  gf_ebeln_col  TYPE i,
  gf_ebelp_col  TYPE i,
  gf_pabnum_col TYPE i,
* 03.03.2012
  gf_charg_col  TYPE i,
  gf_pabpos_col TYPE i.


*----------------------------------------------------------------------*
*    IDoc Variables.
*----------------------------------------------------------------------*
DATA: w_edidc   LIKE edidc,
      t_edidc   TYPE TABLE OF edidc WITH HEADER LINE,
      t_edidd   TYPE TABLE OF edidd WITH HEADER LINE.
DATA  gl_idocno TYPE edidc-docnum.
DATA:
  fs_delivery_header  LIKE e1edl20,  "Delivery Header
  fs_delivery_item    LIKE e1edl24,  "Delivery Item
  fs_deadline         LIKE e1edt13,  "Deadline
  fs_delivery_control LIKE e1edl18,  "Delivery Control
  fs_import           LIKE e1edl33,  "Import Data Delivery Header
  fs_routes           LIKE e1edl28,  "Routes
  fs_reference        LIKE e1edl41,  "Item Reference
  fs_hu_header        LIKE e1edl37,  "HU Header
  fs_hu_item          LIKE e1edl44,  "HU Item
  fs_jit_call         LIKE e1edl52.  "JIT Call

*----------------------------------------------------------------------*
*    Packing List Variables.
*----------------------------------------------------------------------*
DATA:
  BEGIN OF xlist OCCURS 0,
    ebeln  LIKE ekko-ebeln,
    ebelp  LIKE ekpo-ebelp,
    matnr  LIKE mara-matnr,
    exidv2 LIKE vekp-exidv2,
    exidv  LIKE vekp-exidv,
    vhilm  LIKE vekp-vhilm,
    maktx  LIKE makt-maktx,
    lfimg  LIKE lips-lfimg,
    meins  LIKE lips-meins,
    inhalt LIKE vekp-inhalt,
    pabnum LIKE pabasn-pabnum,
    pabpos LIKE pabasn-pabpos,
* 03.03.2012(1)
    charg(10) TYPE c,
  END OF xlist.
DATA :
  BEGIN OF xitem OCCURS 0,
    bstnr LIKE lips-vgbel,
    posex LIKE lips-vgpos,
    kdmat LIKE lips-kdmat,
    vemeh LIKE vepo-vemeh,
    vemng LIKE vepo-vemng,
* 03.03.2012(1)
    charg LIKE lips-charg,
  END OF xitem.
DATA :
  BEGIN OF xpack OCCURS 0,
    exidv2 LIKE vekp-exidv2,
    exidv  LIKE vekp-exidv,
    vhilm  LIKE vekp-vhilm,
    posnr  LIKE lips-posnr,
    vemng  LIKE vepo-vemng,
    vemeh  LIKE vepo-vemeh,
    inhalt LIKE vekp-inhalt,
  END OF xpack.
DATA :
  BEGIN OF xjitcall OCCURS 0,
    posnr  LIKE pabasn-posnr,
    pabnum LIKE pabasn-pabnum,
    pabpos LIKE pabasn-pabpos,
    pabavm LIKE pabasn-pabavm,
  END OF xjitcall.

DATA :  BEGIN OF it_message OCCURS 0,
*          type  LIKE  bapireturn-type,
          status  LIKE icon-id,
          message LIKE bapireturn-message,
        END OF it_message.


DATA : tmp_likp TYPE likp.
DATA : g_zeile TYPE sy-index.

* local file
DATA : BEGIN OF it_local OCCURS 0,
        col01(20),
        col02(30),
        col03(20),
        col04(20),
        col05(40),
        col06(20),
        col07(20),
        col08(20),
        col09(20),
        col10(20),
        col11(20),
        col12(20),
       END OF it_local.


*----------------------------------------------------------------------*
*   Selection Criteria                                                 *
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK snd WITH FRAME
                          TITLE text-snd.
PARAMETERS:
  p_sndprt LIKE edoc_stat-sndprt DEFAULT 'LI' MODIF ID noi  OBLIGATORY,
  p_sndprn LIKE edoc_stat-sndprn OBLIGATORY,
*                                 MODIF ID noi,
  p_sndpfc LIKE edoc_stat-sndpfc DEFAULT 'LF' MODIF ID noi  OBLIGATORY.
SELECTION-SCREEN END OF BLOCK snd.
SELECTION-SCREEN BEGIN OF BLOCK rcv WITH FRAME
                          TITLE text-rcv.
PARAMETERS:
  p_rcvprt LIKE edoc_stat-rcvprt DEFAULT 'LS' MODIF ID noi  OBLIGATORY,
  p_rcvprn LIKE edoc_stat-rcvprn              MODIF ID noi  OBLIGATORY,
  p_rcvpor LIKE edoc_stat-rcvpor              MODIF ID noi.
SELECTION-SCREEN END OF BLOCK rcv.
SELECTION-SCREEN BEGIN OF BLOCK fil WITH FRAME
                          TITLE text-fil.
PARAMETERS:
  pa_fname LIKE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN END OF BLOCK fil.

INITIALIZATION.
  PERFORM get_system_info.

*----------------------------------------------------------------------*
*   AT SELECTION SCREEN ON VALUE-REQUEST                               *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_fname.   "FILE UPLOAD
  PERFORM value_local_file.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT                                           *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT .
  CASE sy-tcode.
    WHEN 'ZMMR30200T'.
      PERFORM modify_screen.
    WHEN OTHERS.

  ENDCASE.

START-OF-SELECTION.
  PERFORM read_partner_profile.
*  PERFORM read_excel_file.
  PERFORM import_local_file.
  PERFORM local_to_xls.
  PERFORM make_packing_list_from_xls.
  PERFORM check_input_data.      "05.09.2014 Victor

  PERFORM create_idoc_file.
  PERFORM show_messages.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  READ_PARTNER_PROFILE
*&---------------------------------------------------------------------*
*       Read Partner Profile.
*----------------------------------------------------------------------*
FORM read_partner_profile .
  CLEAR edp21.
  SELECT SINGLE *
  FROM edp21
  WHERE sndprn EQ p_sndprn
    AND sndprt EQ p_sndprt
    AND sndpfc EQ p_sndpfc
    AND mestyp EQ 'DESADV'.
  IF sy-subrc <> 0.
    MESSAGE e424(e0).
  ENDIF.
ENDFORM.                    " READ_PARTNER_PROFILE
*&---------------------------------------------------------------------*
*&      Form  VALUE_LOCAL_FILE
*&---------------------------------------------------------------------*
*       Value Local File.
*----------------------------------------------------------------------*
FORM value_local_file.

  CONSTANTS lc_mask(20)   TYPE c VALUE ',*.*  ,*.*.'.

  DATA: l_dynpread_t LIKE dynpread OCCURS 0 WITH HEADER LINE,
        l_dyname     LIKE d020s-prog,
        l_dynumb     LIKE d020s-dnum.

  FIELD-SYMBOLS <lfs>.

  REFRESH l_dynpread_t.
  CLEAR   l_dynpread_t.
  GET CURSOR FIELD l_dynpread_t-fieldname.
  APPEND l_dynpread_t.

  ASSIGN (l_dynpread_t-fieldname) TO <lfs>.
  MOVE    l_dynpread_t-fieldvalue TO <lfs>.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = space
      def_path         = <lfs>
      mask             = lc_mask
      mode             = 'O'
      title            = text-fil
    IMPORTING
      filename         = <lfs>
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " VALUE_LOCAL_FILE
*&---------------------------------------------------------------------*
*&      Form  READ_EXCEL_FILE
*&---------------------------------------------------------------------*
*       Read Excel File.
*----------------------------------------------------------------------*
FORM read_excel_file.
  DATA : lf_filename  LIKE rlgrap-filename.
  DATA : lf_begin_col TYPE i VALUE 1,
         lf_begin_row TYPE i VALUE 1,
         lf_end_col   TYPE i VALUE 255,
         lf_end_row   TYPE i VALUE 65536.

  MOVE pa_fname TO lf_filename.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = lf_filename
      i_begin_col             = lf_begin_col
      i_begin_row             = lf_begin_row
      i_end_col               = lf_end_col
      i_end_row               = lf_end_row
    TABLES
      intern                  = xlsdata
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2.
ENDFORM.                    " READ_EXCEL_FILE
*&---------------------------------------------------------------------*
*&      Form  MAKE_PACKING_LIST_FROM_XLS
*&---------------------------------------------------------------------*
*       Make Packing List from Excel File.
*----------------------------------------------------------------------*
FORM make_packing_list_from_xls.
  DATA :
    lf_header_line TYPE i,
    lf_item_line   TYPE i.

  DATA:
    lf_posnr LIKE lips-posnr,
    lf_new.
  REFRESH xlist.

  READ TABLE xlsdata WITH KEY value = 'TDDAT'.
  IF sy-subrc EQ 0.
    gf_tddat_col = xlsdata-col.
  ENDIF.
  READ TABLE xlsdata WITH KEY value = 'LFDAT'.
  IF sy-subrc EQ 0.
    gf_lfdat_col = xlsdata-col.
  ENDIF.
  READ TABLE xlsdata WITH KEY value = 'LIFEX'.
  IF sy-subrc EQ 0.
    gf_lifex_col = xlsdata-col.
    lf_header_line = xlsdata-row + 3.
  ENDIF.
  READ TABLE xlsdata WITH KEY value = 'BOLNR'.
  IF sy-subrc EQ 0.
    gf_bolnr_col = xlsdata-col.
  ENDIF.
  READ TABLE xlsdata WITH KEY value = 'TRAID'.
  IF sy-subrc EQ 0.
    gf_traid_col = xlsdata-col.
  ENDIF.
  READ TABLE xlsdata WITH KEY value = 'TRATY'.
  IF sy-subrc EQ 0.
    gf_traty_col = xlsdata-col.
  ENDIF.
  READ TABLE xlsdata WITH KEY value = 'KNOTE'.
  IF sy-subrc EQ 0.
    gf_knote_col = xlsdata-col.
  ENDIF.

  READ TABLE xlsdata WITH KEY value = 'EXIDV2'.
  IF sy-subrc EQ 0.
    gf_exidv2_col = xlsdata-col.
  ENDIF.
  READ TABLE xlsdata WITH KEY value = 'EXIDV'.
  IF sy-subrc EQ 0.
    gf_exidv_col = xlsdata-col.
  ENDIF.
  READ TABLE xlsdata WITH KEY value = 'VHILM'.
  IF sy-subrc EQ 0.
    gf_vhilm_col = xlsdata-col.
  ENDIF.
  READ TABLE xlsdata WITH KEY value = 'MATNR'.
  IF sy-subrc EQ 0.
    gf_matnr_col = xlsdata-col.
    lf_item_line = xlsdata-row + 3.
  ENDIF.
  READ TABLE xlsdata WITH KEY value = 'MAKTX'.
  IF sy-subrc EQ 0.
    gf_maktx_col = xlsdata-col.
  ENDIF.
  READ TABLE xlsdata WITH KEY value = 'MEINS'.
  IF sy-subrc EQ 0.
    gf_meins_col = xlsdata-col.
  ENDIF.
  READ TABLE xlsdata WITH KEY value = 'LFIMG'.
  IF sy-subrc EQ 0.
    gf_lfimg_col = xlsdata-col.
  ENDIF.
  READ TABLE xlsdata WITH KEY value = 'EBELN'.
  IF sy-subrc EQ 0.
    gf_ebeln_col = xlsdata-col.
  ENDIF.
  READ TABLE xlsdata WITH KEY value = 'EBELP'.
  IF sy-subrc EQ 0.
    gf_ebelp_col = xlsdata-col.
  ENDIF.
  READ TABLE xlsdata WITH KEY value = 'PABNUM'.
  IF sy-subrc EQ 0.
    gf_pabnum_col = xlsdata-col.
  ENDIF.
  READ TABLE xlsdata WITH KEY value = 'PABPOS'.
  IF sy-subrc EQ 0.
    gf_pabpos_col = xlsdata-col.
  ENDIF.

* 03.03.2012(insert start)
  READ TABLE xlsdata WITH KEY value = 'CHARG'.
  IF sy-subrc EQ 0.
    gf_charg_col = xlsdata-col.
  ENDIF.
* 03.03.2012(insert end)

  SORT xlsdata BY row col.
  REFRESH xlsdatab.
  APPEND LINES OF xlsdata TO xlsdatab.

  LOOP AT xlsdata.
    IF xlsdata-row EQ lf_header_line.
      PERFORM move_xlhd_fval_to_header.
    ENDIF.
    AT NEW row.
      IF xlsdata-row >= lf_item_line.
        CLEAR xlist.
      ENDIF.
    ENDAT.
    IF xlsdata-row >= lf_item_line.
      PERFORM move_xlit_fval_to_item.
    ENDIF.
    AT END OF row.
      IF xlsdata-row >= lf_item_line.
        APPEND xlist.
      ENDIF.
    ENDAT.
  ENDLOOP.

  SORT xlist BY ebeln ebelp. "Victor 05.08.2014

  LOOP AT xlist.
*---Convert Material Number
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = xlist-matnr
      IMPORTING
        output       = xlist-matnr
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.
*---Find PO number/Item for JIT Call
    IF xlist-pabnum NE '' AND
       xlist-pabpos NE ''.
      SELECT SINGLE k~ebeln k~ebelp
        INTO (xlist-ebeln,xlist-ebelp)
        FROM jitoco AS j INNER JOIN pkhd AS k
                            ON j~pknum = k~pknum
       WHERE outpo EQ xlist-pabnum
         AND jcpos EQ xlist-pabpos.
    ENDIF.
*---Find PO Line Item Number.
    IF xlist-ebelp IS INITIAL OR
       xlist-ebelp = 0.
      SELECT SINGLE ebelp
            INTO xlist-ebelp
            FROM ekpo
            WHERE ebeln EQ xlist-ebeln
              AND matnr EQ xlist-matnr
              AND loekz EQ space.
    ENDIF.
    IF xlist-matnr IS INITIAL.
      DELETE xlist.
    ELSE.
      MODIFY xlist.
    ENDIF.
  ENDLOOP.

* 03.03.2012(delete start)
*  SORT xlist BY ebeln ebelp exidv.
*  LOOP AT xlist.
*    AT NEW ebelp.
*      ADD 1 TO lf_posnr.
*    ENDAT.
*    ENDAT.
* 03.03.2012(delete end)

* 03.03.2012(insert start)
*  SORT xlist BY ebeln ebelp exidv.
  DATA: l_ebeln LIKE ekko-ebeln.
  DATA: l_ebelp LIKE ekpo-ebelp.
  DATA: l_charg(10)  TYPE c.
  LOOP AT xlist.
    IF l_ebeln <> xlist-ebeln
    OR l_ebelp <> xlist-ebelp
    OR l_charg <> xlist-charg.
      l_ebeln = xlist-ebeln.
      l_ebelp = xlist-ebelp.
      l_charg = xlist-charg.
      ADD 1 TO lf_posnr.
    ENDIF.
* 03.03.2012(insert end)

    CLEAR xpack.
    xpack-exidv2 = xlist-exidv2.
    xpack-exidv  = xlist-exidv.
    xpack-vhilm  = xlist-vhilm.
    xpack-posnr  = lf_posnr.
    xpack-vemng  = xlist-lfimg.
    xpack-vemeh  = xlist-meins.
    xpack-inhalt = xlist-inhalt.
    COLLECT xpack.

    IF NOT  xlist-pabnum IS INITIAL.
      CLEAR xjitcall.
      xjitcall-posnr  = lf_posnr.
      xjitcall-pabnum = xlist-pabnum.
      xjitcall-pabpos = xlist-pabpos.
      xjitcall-pabavm = xlist-lfimg.
      COLLECT xjitcall.
    ENDIF.

    CLEAR xitem.
*---Convert Material Number
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = xlist-matnr
      IMPORTING
        output       = xitem-kdmat
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.
    xitem-vemng = xlist-lfimg.
    xitem-vemeh = xlist-meins.
    xitem-bstnr = xlist-ebeln.
    xitem-posex = xlist-ebelp.
* 03.03.2012(1)
    xitem-charg = xlist-charg.
*---Find PO Line Item Number.
    IF xlist-ebelp IS INITIAL OR
       xlist-ebelp = 0.
      SELECT SINGLE ebelp
            INTO xitem-posex
            FROM ekpo
            WHERE ebeln EQ xitem-bstnr
              AND matnr EQ xitem-kdmat
              AND loekz EQ space.
    ENDIF.
    COLLECT xitem.
  ENDLOOP.
ENDFORM.                    " MAKE_PACKING_LIST_FROM_XLS
*&---------------------------------------------------------------------*
*&      Form  MOVE_XLHD_FVAL_TO_HEADER
*&---------------------------------------------------------------------*
*       Move Excel Header Field Value to Header.
*----------------------------------------------------------------------*
FORM move_xlhd_fval_to_header.
  CASE xlsdata-col.
    WHEN gf_tddat_col.
      PERFORM convert_date_to_internal USING    xlsdata-value
                                       CHANGING likp-tddat.
    WHEN gf_lfdat_col.
      PERFORM convert_date_to_internal USING    xlsdata-value
                                       CHANGING likp-lfdat.
    WHEN gf_lifex_col.
      likp-lifex = xlsdata-value.
    WHEN gf_bolnr_col.
      likp-bolnr = xlsdata-value.
    WHEN gf_traid_col.
      likp-traid = xlsdata-value.
    WHEN gf_traty_col.
      likp-traty = xlsdata-value.
      IF     likp-traty = 'A002'.
        likp-vsart = '04'.
      ELSEIF likp-traty = 'A003'.
        likp-vsart = '05'.
      ELSE.
        likp-vsart = '01'.
      ENDIF.
    WHEN gf_knote_col.
      CASE xlsdata-value.
        WHEN 'USMOB'.
          likp-route = 'KMMGS0'.
        WHEN 'USLAX'.
          likp-route = 'KMMGS1'.
        WHEN 'USATL'.
          likp-route = 'KMMGA0'.
        WHEN OTHERS.
          likp-route = 'KMMGDM'.
      ENDCASE.
  ENDCASE.
ENDFORM.                    " MOVE_XLHD_FVAL_TO_HEADER
*&---------------------------------------------------------------------*
*&      Form  MOVE_XLIT_FVAL_TO_ITEM
*&---------------------------------------------------------------------*
*       Move Item Value to Item.
*----------------------------------------------------------------------*
FORM move_xlit_fval_to_item.
  DATA : lf_isocode LIKE  t006-isocode,
         l_len      TYPE i,
         l_lifnr    TYPE  lfa1-lifnr.

  CASE xlsdata-col.
    WHEN gf_exidv_col.
*    xlist-exidv = xlsdata-value.
      l_len = strlen( xlsdata-value ).
      IF l_len <= 20.
        xlist-exidv = xlsdata-value.
      ELSE.
        READ TABLE xlsdatab WITH KEY row = xlsdata-row
                                     col = gf_ebeln_col.

        SELECT SINGLE lifnr
          FROM ekko
          INTO l_lifnr
         WHERE ebeln = xlsdatab-value.

        CONCATENATE l_lifnr xlsdata-value(10) INTO xlist-exidv .
        xlist-inhalt  = xlsdata-value.
      ENDIF.
    WHEN gf_exidv2_col.
      xlist-exidv2 = xlsdata-value.
    WHEN gf_vhilm_col.
      xlist-vhilm = xlsdata-value.
    WHEN gf_matnr_col.
      xlist-matnr = xlsdata-value.
    WHEN gf_maktx_col.
      xlist-maktx = xlsdata-value.
    WHEN gf_lfimg_col.
      xlist-lfimg = xlsdata-value.
    WHEN gf_meins_col.
      xlist-meins = xlsdata-value.
      CALL FUNCTION 'UNIT_OF_MEASURE_SAP_TO_ISO'
        EXPORTING
          sap_code    = xlist-meins
        IMPORTING
          iso_code    = lf_isocode
        EXCEPTIONS
          not_found   = 1
          no_iso_code = 2
          OTHERS      = 3.
      xlist-meins = lf_isocode.
    WHEN gf_ebeln_col.
      xlist-ebeln = xlsdata-value.
    WHEN gf_ebelp_col.
      xlist-ebelp = xlsdata-value.
    WHEN gf_pabnum_col.
      xlist-pabnum = xlsdata-value.
    WHEN gf_pabpos_col.
      xlist-pabpos = xlsdata-value.
* 03.03.2012
    WHEN gf_charg_col.
      xlist-charg = xlsdata-value.

  ENDCASE.
ENDFORM.                    " MOVE_XLIT_FVAL_TO_ITEM

*&---------------------------------------------------------------------*
*&      Form  INSERT_ITEM_LINE
*&---------------------------------------------------------------------*
*       Insert Item Line.
*----------------------------------------------------------------------*
*      -->P_INDEX  Index.
*----------------------------------------------------------------------*
FORM insert_item_line USING    p_index.
  CLEAR xlist.
  INSERT xlist INDEX p_index.
ENDFORM.                    " INSERT_ITEM_LINE
*&---------------------------------------------------------------------*
*&      Form  CONVERT_DATE_TO_INTERNAL
*&---------------------------------------------------------------------*
*       Convert External Date to Internal.
*----------------------------------------------------------------------*
*      -->P_EXTDT  External Date.
*      <--P_INTDT  Internal Date.
*----------------------------------------------------------------------*
FORM convert_date_to_internal USING    p_extdt
                              CHANGING p_intdt.
  DO.
    REPLACE '-' WITH space INTO p_extdt.
    IF sy-subrc NE 0.
      REPLACE '/' WITH space INTO p_extdt.
      IF sy-subrc NE 0.
        REPLACE '.' WITH space INTO p_extdt.
        IF sy-subrc NE 0.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDDO.
  CONDENSE p_extdt NO-GAPS.
  p_intdt = p_extdt.
ENDFORM.                    " CONVERT_DATE_TO_INTERNAL
*&---------------------------------------------------------------------*
*&      Form  CREATE_IDOC_FILE
*&---------------------------------------------------------------------*
*       Create IDoc File.
*----------------------------------------------------------------------*
FORM create_idoc_file .
  DATA:
    lf_posnr  LIKE lips-posnr,
    lf_count  TYPE sy-tabix,
    lf_segnum TYPE edid4-segnum,
    lf_psgnum TYPE edid4-psgnum,
    lf_hlevel TYPE edid4-hlevel.

  CLEAR g_zeile.
  CALL FUNCTION 'MESSAGES_INITIALIZE'.
  w_edidc-mestyp  = edp21-mestyp.
  w_edidc-direct  = '2'.
  w_edidc-idoctp  = 'DELVRY03'.
*S__ insert by paul
  w_edidc-cimtyp  = 'ZDLVRY03'.
*E
  w_edidc-sndpor  = 'EXCELFILE'.            "Sender Port
  w_edidc-sndprt  = edp21-sndprt.           "Sender Partner Type.
  w_edidc-sndpfc  = edp21-sndpfc.           "Sender Partner Function.
  w_edidc-sndprn  = edp21-sndprn.           "Sender Partner No.
  w_edidc-rcvpor  = p_rcvpor.               "Receiver Port
  w_edidc-rcvprt  = p_rcvprt.               "Receiver Partner Type.
  w_edidc-rcvprn  = p_rcvprn.               "Receiver Partner No.

* Fill Delivery Header.
  CLEAR t_edidd.
  t_edidd-segnam            = 'E1EDL20'.
  lf_segnum = lf_segnum + 1.
  t_edidd-segnum            = lf_segnum.
  lf_psgnum                 = lf_segnum.
  t_edidd-hlevel            = 2.
  fs_delivery_header-bolnr  = likp-bolnr.          "B/L number
  fs_delivery_header-traty  = likp-traty.          "Transport Type
  fs_delivery_header-traid  = likp-traid.          "Transportation ID
  fs_delivery_header-lifex  = likp-lifex.          "Delivery Note
  t_edidd-sdata             = fs_delivery_header.
  APPEND t_edidd.

*S__CHANGE BY PAUL 04/29/2011
  CLEAR t_edidd.
  t_edidd-segnam            = 'ZE1EDL1'.
  lf_segnum = lf_segnum + 1.
  t_edidd-segnum            = lf_segnum.
  lf_psgnum                 = 1.
  t_edidd-hlevel            = 3.

  APPEND t_edidd.

  CLEAR t_edidd.
  t_edidd-segnam            = 'E1EDL55'.
  lf_segnum = lf_segnum + 1.
  t_edidd-segnum            = lf_segnum.
  lf_psgnum                 = 1.
  t_edidd-hlevel            = 3.

  APPEND t_edidd.

  CLEAR t_edidd.
  t_edidd-segnam            = 'E1ADRM1'.
  lf_segnum = lf_segnum + 1.
  t_edidd-segnum            = lf_segnum.
  lf_psgnum                 = 1.
  t_edidd-hlevel            = 3.

  APPEND t_edidd.

  CLEAR t_edidd.
  t_edidd-segnam            = 'E1ADRM1'.
  lf_segnum = lf_segnum + 1.
  t_edidd-segnum            = lf_segnum.
  lf_psgnum                 = 1.
  t_edidd-hlevel            = 3.

  APPEND t_edidd.
**  DESCRIBE TABLE xjitcall LINES lf_count.
**  IF lf_count > 0.
*** Fill Controlling (Delivery)
**    CLEAR t_edidd.
**    t_edidd-segnam            = 'E1EDL18'.
**    lf_segnum = lf_segnum + 1.
**    t_edidd-segnum            = lf_segnum.
**    t_edidd-psgnum            = 1.
**    t_edidd-hlevel            = 3.
**    fs_delivery_control-qualf = 'JIT'.               "JIT Call
**    t_edidd-sdata             = fs_delivery_control.
**    APPEND t_edidd.
**  ENDIF.
* Fill Deadline
  CLEAR t_edidd.
  t_edidd-segnam            = 'E1EDT13'.
  lf_segnum = lf_segnum + 1.
  t_edidd-segnum            = lf_segnum.
  t_edidd-psgnum            = 1.
  t_edidd-hlevel            = 3.
  fs_deadline-qualf         = '007'.               "Est. Delivery Date
  fs_deadline-ntanf         = likp-lfdat.          "Date
  t_edidd-sdata             = fs_deadline.
  APPEND t_edidd.

* Fill Import Data Delivery Header
  CLEAR t_edidd.
  t_edidd-segnam            = 'E1EDL33'.
  lf_segnum = lf_segnum + 1.
  t_edidd-segnum            = lf_segnum.
  t_edidd-psgnum            = 1.
  t_edidd-hlevel            = 3.
  fs_import-expvz           = '1'.                 "Mode of Transport
  fs_import-iever           = '2'.                 "Domestic Mode.
  fs_import-conta           = '1'.                 "Container.
  t_edidd-sdata             = fs_import.
  APPEND t_edidd.

* Fill Routes
  CLEAR t_edidd.
  t_edidd-segnam            = 'E1EDL28'.
  lf_segnum = lf_segnum + 1.
  t_edidd-segnum            = lf_segnum.
  t_edidd-psgnum            = 1.
  t_edidd-hlevel            = 3.
  fs_routes-route           = likp-route.          "Route
  fs_routes-vsart           = likp-vsart.          "Shipping Type
  t_edidd-sdata             = fs_routes.
  APPEND t_edidd.

  SORT xitem BY bstnr posex.
  LOOP AT xitem.
    ADD 1 TO lf_posnr.
* Fill Delivery Items
    CLEAR t_edidd.
    t_edidd-segnam            = 'E1EDL24'.
    lf_segnum = lf_segnum + 1.
    t_edidd-segnum            = lf_segnum.
    t_edidd-psgnum            = 1.
    lf_psgnum = lf_segnum.
    t_edidd-hlevel            = 3.
    fs_delivery_item-posnr    = lf_posnr.          "Item # for Delivery
    fs_delivery_item-kdmat    = xitem-kdmat.       "Material Number
    fs_delivery_item-lfimg    = xitem-vemng.       "Delivered Q'ty
* 02.03.2012(1)
    fs_delivery_item-charg    = xitem-charg.       "Korus ind
    CONDENSE fs_delivery_item-lfimg NO-GAPS.
    fs_delivery_item-vrkme    = xitem-vemeh.       "Unit
    IF likp-traty = 'A002' OR
       likp-traty = 'A003'.
      fs_delivery_item-lgort    = 'C001'.            "Receiving StLoc.
    ENDIF.
    t_edidd-sdata             = fs_delivery_item.
    APPEND t_edidd.
    lf_psgnum = lf_segnum.
*S_COMMENT BY PAUL
** Fill JIT Call
*    LOOP AT xjitcall WHERE posnr EQ lf_posnr.
*      CLEAR t_edidd.
*      t_edidd-segnam            = 'E1EDL52'.
*      lf_segnum = lf_segnum + 1.
*      t_edidd-segnum            = lf_segnum.
*      t_edidd-psgnum            = lf_psgnum.
*      t_edidd-hlevel            = 4.
*      fs_jit_call-qualf         = 'PRN'. "Qualifier
*      fs_jit_call-prodn         = xjitcall-pabnum.   "JIT CallNumber
*      fs_jit_call-itmnr         = xjitcall-pabpos.   "JIT Call Item
*      fs_jit_call-squnt         = xjitcall-pabavm.   "Quantity
*      t_edidd-sdata             = fs_jit_call.
*      APPEND t_edidd.
*    ENDLOOP.
* Fill Reference Data(Purchase Order)
    CLEAR t_edidd.
    t_edidd-segnam            = 'E1EDL41'.
    lf_segnum = lf_segnum + 1.
    t_edidd-segnum            = lf_segnum.
    t_edidd-psgnum            = lf_psgnum.
    t_edidd-hlevel            = 4.
    fs_reference-quali        = '001'. "Qualifier
    fs_reference-bstnr        = xitem-bstnr.       "PO #/SA #
    fs_reference-posex        = xitem-posex.       "PO Line #
    t_edidd-sdata             = fs_reference.
    APPEND t_edidd.
  ENDLOOP.
  SORT xpack BY exidv2 exidv posnr.
  LOOP AT xpack.
    AT NEW vhilm.
* Fill Handling Unit Header
      CLEAR: t_edidd, fs_hu_header.
      t_edidd-segnam            = 'E1EDL37'.
      lf_segnum = lf_segnum + 1.
      t_edidd-segnum            = lf_segnum.
      lf_psgnum = lf_segnum.
      t_edidd-psgnum            = 1.
      t_edidd-hlevel            = 3.
      fs_hu_header-exidv        = xpack-exidv.     "Ext. HU ID.
*      fs_hu_header-INHALT       = xpack-INHALT.     "Contents.
      IF xpack-vhilm IS INITIAL.
        fs_hu_header-vhilm_ku     = 'CASE'.  "Pack.Material
      ELSE.
        fs_hu_header-vhilm_ku     = xpack-vhilm.   "Pack.Material
      ENDIF.
      t_edidd-sdata             = fs_hu_header.
      APPEND t_edidd.
    ENDAT.
* Fill Handling Unit Items
    CLEAR: t_edidd, fs_hu_item.
    t_edidd-segnam            = 'E1EDL44'.
    lf_segnum = lf_segnum + 1.
    t_edidd-segnum            = lf_segnum.
    t_edidd-psgnum            = lf_psgnum.
    t_edidd-hlevel            = 4.
    fs_hu_item-posnr          = xpack-posnr.       "Item # for Deliver
    fs_hu_item-exidv          = xpack-exidv.       "External HU ID
    fs_hu_item-vemng          = xpack-vemng.       "Packed Quantity
    CONDENSE fs_hu_item-vemng NO-GAPS.
    fs_hu_item-vemeh          = xpack-vemeh.       "Unit
    t_edidd-sdata             = fs_hu_item.
    APPEND t_edidd.
  ENDLOOP.
  LOOP AT xpack WHERE exidv2 NE ''.
    AT NEW exidv2.
* Fill Handling Unit Header - Unit Load
      CLEAR: t_edidd, fs_hu_header.
      t_edidd-segnam            = 'E1EDL37'.
      lf_segnum = lf_segnum + 1.
      t_edidd-segnum            = lf_segnum.
      t_edidd-psgnum            = 1.
      lf_psgnum = lf_segnum.
      t_edidd-hlevel            = 3.
      fs_hu_header-exidv        = xpack-exidv2.
      fs_hu_header-vhilm_ku     = 'CASE'.       "Pack.Material
      t_edidd-sdata             = fs_hu_header.
      APPEND t_edidd.
    ENDAT.
    AT NEW vhilm.
* Fill Handling Unit Items
      CLEAR: t_edidd, fs_hu_item.
      t_edidd-segnam            = 'E1EDL44'.
      lf_segnum = lf_segnum + 1.
      t_edidd-segnum            = lf_segnum.
      t_edidd-psgnum            = lf_psgnum.
      t_edidd-hlevel            = 4.
      fs_hu_item-velin          = '3'.
      fs_hu_item-exidv          = xpack-exidv.       "External HU ID
      t_edidd-sdata             = fs_hu_item.
      APPEND t_edidd.
    ENDAT.
  ENDLOOP.

*-< use Standard function 05.08.2014 Victor
  CALL FUNCTION 'IDOC_INBOUND_WRITE_TO_DB'    "Create IDoc
    IMPORTING
      pe_idoc_number    = gl_idocno
    TABLES
      t_data_records    = t_edidd
    CHANGING
      pc_control_record = w_edidc
    EXCEPTIONS
      idoc_not_saved    = 1
      OTHERS            = 2.
  IF sy-subrc EQ 0.
    COMMIT WORK AND WAIT.
    APPEND w_edidc TO t_edidc.
*   "Post Idoc -> Create Inbound Delivery in case of Success
    CALL FUNCTION 'APPLICATION_IDOC_POST_IMMEDIAT'
      EXPORTING
        do_commit                 = 'X'
      TABLES
        idoc_control              = t_edidc
        idoc_data                 = t_edidd
      EXCEPTIONS
        error_opening_idoc        = 1
        error_writing_idoc_status = 2
        no_idocs                  = 3
        OTHERS                    = 4.
    IF sy-subrc <> 0.
      ADD 1 TO g_zeile.
      CALL FUNCTION 'MESSAGE_STORE'
        EXPORTING
          arbgb                  = sy-msgid
          msgty                  = 'E'
          txtnr                  = sy-msgno
          msgv1                  = sy-msgv1
          msgv2                  = sy-msgv2
          msgv3                  = sy-msgv3
          msgv4                  = sy-msgv4
          zeile                  = g_zeile
        EXCEPTIONS
          message_type_not_valid = 1
          not_active             = 2.
    ENDIF.
    PERFORM read_idoc_result USING gl_idocno.
  ELSE.
    ROLLBACK WORK.
    ADD 1 TO g_zeile.
    CALL FUNCTION 'MESSAGE_STORE'
      EXPORTING
        arbgb                  = sy-msgid
        msgty                  = 'E'
        txtnr                  = sy-msgno
        msgv1                  = sy-msgv1
        msgv2                  = sy-msgv2
        msgv3                  = sy-msgv3
        msgv4                  = sy-msgv4
        zeile                  = g_zeile
      EXCEPTIONS
        message_type_not_valid = 1
        not_active             = 2.
  ENDIF.

*--< below comment
**-----------------------------------------------------------------
**  Write IDoc & Inbound Start
**----------------------------------------------------------------
*  CALL FUNCTION 'ZIDOC_WRITE_AND_START_INBOUND'
*    EXPORTING
*      i_edidc        = w_edidc
*    IMPORTING
*      docnum         = gl_idocno
*    TABLES
*      i_edidd        = t_edidd
*      t_idoc_status  = t_idoc_status
*    EXCEPTIONS
*      idoc_not_saved = 1
*      OTHERS         = 2.
*
*  READ TABLE t_idoc_status WITH KEY msgty = 'E'.
*  IF sy-subrc EQ 0.
*    LOOP AT t_idoc_status WHERE msgty = 'E'.
*      ADD 1 TO g_zeile.
*
*      CALL FUNCTION 'MESSAGE_STORE'
*        EXPORTING
*          arbgb                  = t_idoc_status-msgid
*          msgty                  = t_idoc_status-msgty
*          msgv1                  = t_idoc_status-msgv1
*          msgv2                  = t_idoc_status-msgv2
*          msgv3                  = t_idoc_status-msgv3
*          msgv4                  = t_idoc_status-msgv4
*          txtnr                  = t_idoc_status-msgno
*          zeile                  = g_zeile
*        EXCEPTIONS
*          message_type_not_valid = 1
*          not_active             = 2.
*    ENDLOOP.
*
*  ELSE.
*    ADD 1 TO g_zeile.
*
*    CALL FUNCTION 'MESSAGE_STORE'
*      EXPORTING
*        arbgb                  = 'E0'
*        msgty                  = 'S'
*        msgv1                  = gl_idocno
*        txtnr                  = '036'
*        zeile                  = g_zeile
*      EXCEPTIONS
*        message_type_not_valid = 1
*        not_active             = 2.
*
*    LOOP AT t_idoc_status WHERE msgty = 'S'.
*      ADD 1 TO g_zeile.
*
*      CALL FUNCTION 'MESSAGE_STORE'
*        EXPORTING
*          arbgb                  = t_idoc_status-msgid
*          msgty                  = t_idoc_status-msgty
*          msgv1                  = t_idoc_status-msgv1
*          msgv2                  = t_idoc_status-msgv2
*          msgv3                  = t_idoc_status-msgv3
*          msgv4                  = t_idoc_status-msgv4
*          txtnr                  = t_idoc_status-msgno
*          zeile                  = g_zeile
*        EXCEPTIONS
*          message_type_not_valid = 1
*          not_active             = 2.
*    ENDLOOP.
*  ENDIF.
*
***----------------------------------------------------------------
***  Write IDoc & Inbound Start
***-----------------------------------------------------------------
**  CALL FUNCTION 'IDOC_WRITE_AND_START_INBOUND'
**    EXPORTING
**      i_edidc        = w_edidc
**    IMPORTING
**      docnum         = gl_idocno
**    TABLES
**      i_edidd        = t_edidd
***     t_idoc_status  = t_idoc_status
**    EXCEPTIONS
**      idoc_not_saved = 1
**      OTHERS         = 2.
**  PERFORM read_idoc_result USING gl_idocno.
ENDFORM.                    " CREATE_IDOC_FILE

*&---------------------------------------------------------------------*
*&      Form  SHOW_MESSAGES
*&---------------------------------------------------------------------*
*       Show Messages.
*----------------------------------------------------------------------*
FORM show_messages.
  CALL FUNCTION 'MESSAGES_SHOW'
    EXPORTING
      object             = text-log
      show_linno         = space
    EXCEPTIONS
      inconsistent_range = 1
      no_messages        = 2.
  IF sy-subrc NE 0.
  ENDIF.
ENDFORM.                    " SHOW_MESSAGES
*&---------------------------------------------------------------------*
*&      Form  get_system_info
*&---------------------------------------------------------------------*
*       Get System Information.
*----------------------------------------------------------------------*
FORM get_system_info .
  CONCATENATE 'SAP' sy-sysid INTO p_rcvpor.
  CONCATENATE sy-sysid 'CLNT' sy-mandt INTO p_rcvprn.

  PERFORM read_vendor_of_userid .

ENDFORM.                    " get_system_info
*&---------------------------------------------------------------------*
*&      Form  READ_IDOC_RESULT
*&---------------------------------------------------------------------*
*       Read IDoc Status
*----------------------------------------------------------------------*
*      -->IF_IDOCNO  IDoc Number
*----------------------------------------------------------------------*
FORM read_idoc_result  USING    if_idocno.
  DATA : xedids TYPE TABLE OF edids WITH HEADER LINE,
         lf_error.

  CALL FUNCTION 'IDOC_READ_COMPLETELY'
    EXPORTING
      document_number         = gl_idocno
    TABLES
      int_edids               = xedids
    EXCEPTIONS
      document_not_exist      = 1
      document_number_invalid = 2
      OTHERS                  = 3.
  LOOP AT xedids WHERE ( statyp EQ 'S' AND status <> '64' )
                      OR statyp EQ 'E'.
    ADD 1 TO g_zeile.
    CALL FUNCTION 'MESSAGE_STORE'
      EXPORTING
        arbgb                  = xedids-stamid
        msgty                  = xedids-statyp
        msgv1                  = xedids-stapa1
        msgv2                  = xedids-stapa2
        msgv3                  = xedids-stapa3
        msgv4                  = xedids-stapa4
        txtnr                  = xedids-stamno
        zeile                  = g_zeile
      EXCEPTIONS
        message_type_not_valid = 1
        not_active             = 2.
    IF xedids-statyp = 'E'.
      lf_error = 'X'.
    ENDIF.
  ENDLOOP.

  IF lf_error EQ 'X'.
    CALL FUNCTION 'EDI_DOCUMENT_DELETE'
      EXPORTING
        document_number = gl_idocno.
    COMMIT WORK AND WAIT.
  ENDIF.
ENDFORM.                    " READ_IDOC_RESULT

*&---------------------------------------------------------------------*
*&      Form  import_local_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM import_local_file .
  DATA : l_filename TYPE string.
  l_filename = pa_fname.

  REFRESH it_local.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = l_filename
      filetype                = 'ASC'
      has_field_separator     = 'X'
    TABLES
      data_tab                = it_local
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " import_local_file
*&---------------------------------------------------------------------*
*&      Form  local_to_xls
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM local_to_xls .
  FIELD-SYMBOLS : <fs1>.
  DATA : l_field(40),
         l_colum(2)  TYPE n .
  REFRESH xlsdata.

  LOOP AT it_local.
    CLEAR : xlsdata, l_colum.
    xlsdata-row = sy-tabix.
    DO 12 TIMES.
      l_colum = l_colum + 1.
      CONCATENATE 'IT_LOCAL-COL' l_colum INTO l_field.
      ASSIGN (l_field) TO <fs1>.
      CHECK <fs1> NE space.
      xlsdata-col      = l_colum.
      xlsdata-value    = <fs1>.
      APPEND xlsdata.
    ENDDO.
  ENDLOOP.

ENDFORM.                    " local_to_xls

*&---------------------------------------------------------------------*
*&      Form  read_vendor_of_userid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_vendor_of_userid .

  SELECT SINGLE vendor
  INTO p_sndprn
  FROM isautosicvendtab
  WHERE sic_user = sy-uname.

ENDFORM.                    " read_vendor_of_userid
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*
FORM modify_screen .

  LOOP AT SCREEN .
    IF screen-group1 EQ 'NOI' .
      screen-input        = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " MODIFY_SCREEN
*&---------------------------------------------------------------------*
*&      Form  CHECK_INPUT_DATA
*&---------------------------------------------------------------------*
FORM check_input_data .
  DATA : l_length TYPE i.
  DATA : xlist_tmp LIKE xlist OCCURS 0 WITH HEADER LINE.
  data : l_exidv  LIKE vekp-exidv.

  CLEAR : it_message[], it_message.

*-Check Header data
  IF likp-lfdat < '20000101' OR likp-lfdat > '22001231'.
    MESSAGE s000(zmpp) WITH 'Invalid Date. Use Data format YYYY-MM-DD'.
    STOP.
  ENDIF.

  CLEAR : tmp_likp.
  SELECT SINGLE * INTO tmp_likp
  FROM likp
  WHERE lifex = likp-lifex.
  IF sy-subrc = 0.
*    MESSAGE s000(zmpp) WITH 'Delivery Note' likp-lifex
*                           'has already been used with' tmp_likp-vbeln.
*    STOP.
    CONCATENATE 'Delivery Note ' likp-lifex
                ' has already been used with ' tmp_likp-vbeln
                           INTO it_message-message.
    it_message-status = '@AH@'.
    APPEND it_message. CLEAR it_message.
  ENDIF.

*  CLEAR : tmp_likp.
*  SELECT SINGLE * INTO tmp_likp
*  FROM likp
*  WHERE traid = likp-traid.
*  IF sy-subrc = 0.
**    MESSAGE s000(zmpp) WITH 'Truck ID' likp-traid
**                      'has already been used with' tmp_likp-vbeln.
**    STOP.
*    CONCATENATE 'Truck ID' likp-traid
*                'has already been used with' tmp_likp-vbeln
*                              INTO it_message-message.
*    it_message-status = '@AH@'.
*    APPEND it_message. CLEAR it_message.
*  ENDIF.

  SELECT SINGLE *
  FROM tvty
  WHERE traty = likp-traty
    AND veltp = 'A'.
  IF sy-subrc <> 0.
    MESSAGE s000(zmpp) WITH 'Transportation ' likp-traty
                            'is not correct'.
    STOP.
  ENDIF.


  xlist_tmp[] = xlist[].
  SORT xlist_tmp BY exidv.
  DELETE ADJACENT DUPLICATES FROM xlist_tmp COMPARING exidv.
  IF sy-subrc = 0.
    MESSAGE s000(zmpp) WITH 'Duplicate HU is existing. Correct it'.
    STOP.
  ENDIF.

*-Item Check
  SORT xlist BY matnr.
  LOOP AT xlist.

    SELECT SINGLE *
    FROM mara
    WHERE matnr = xlist-vhilm
      AND mtart = 'VERP'.
    IF sy-subrc <> 0.
      MESSAGE s000(zmpp) WITH 'Error Packing material ' xlist-vhilm
                              'is not correct'.
      STOP.
    ENDIF.

    l_length  = strlen( xlist-exidv ).
    IF l_length > 14.
*      MESSAGE s000(zmpp) WITH 'HU Length should not be more than 14'.
*      STOP.
      CONCATENATE xlist-exidv 'HU Length should not be more than 14'
                            INTO it_message-message.
      it_message-status  = '@AH@'.
      APPEND it_message. CLEAR it_message.
    ENDIF.

CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
  EXPORTING
    input         = xlist-exidv
 IMPORTING
   OUTPUT        = l_exidv.


    SELECT SINGLE *
    FROM vekp
    WHERE  exidv = l_exidv.
    IF sy-subrc = 0.
      MESSAGE s000(zmpp) WITH 'Error ' vekp-exidv ' already existing'
                              ' with other Inbound Delivery'.
      STOP.
    ENDIF.

  ENDLOOP.

  IF it_message[] IS NOT INITIAL.
    CALL SCREEN 0100 STARTING AT 10 5
                     ENDING AT 90  18.
    IF save_ok = 'CANCEL'.
      STOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " CHECK_INPUT_DATA
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '0100'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_100  OUTPUT
*&---------------------------------------------------------------------*
MODULE display_alv_100 OUTPUT.
  IF g_custom_container IS INITIAL.
    PERFORM create_alv.
    PERFORM set_layout.
    PERFORM build_field_catalog.
    PERFORM display_alv.
  ELSE.

  ENDIF.
ENDMODULE.                 " DISPLAY_ALV_100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_ALV
*&---------------------------------------------------------------------*
FORM create_alv .
  PERFORM create_object.
ENDFORM.                    " CREATE_ALV
*&---------------------------------------------------------------------*
*&      Form  CREATE_OBJECT
*&---------------------------------------------------------------------*
FORM create_object .

  CREATE OBJECT g_custom_container
    EXPORTING
      container_name              = g_container
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.
  IF sy-subrc <> 0.
  ENDIF.

  CREATE OBJECT g_grid
    EXPORTING
      i_parent          = g_custom_container
    EXCEPTIONS
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4
      OTHERS            = 5.
  IF sy-subrc <> 0.
  ENDIF.


ENDFORM.                    " CREATE_OBJECT
*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT
*&---------------------------------------------------------------------*
FORM set_layout .

  gs_layout-no_toolbar  = 'X'.
  gs_layout-grid_title  = 'WARNING!   Will you process anyway?'.

ENDFORM.                    " SET_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG
*&---------------------------------------------------------------------*
FORM build_field_catalog .
  CLEAR : it_fieldname[], it_fieldname.

  DATA: l_datum(08).

  sy-datum = sy-datum + 1.
  MOVE sy-datum TO l_datum.
  SET PARAMETER ID 'ALVBUFFER' FIELD l_datum.

*-It doesn't work with Internal Table
* CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
      i_internal_tabname     = 'IT_MESSAGE'
      i_inclname             = sy-repid
      i_bypassing_buffer     = 'X'
*     I_BUFFER_ACTIVE        =
    CHANGING
      ct_fieldcat            = it_fieldname
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
*
  CALL FUNCTION 'LVC_TRANSFER_FROM_SLIS'
    EXPORTING
      it_fieldcat_alv = it_fieldname
    IMPORTING
      et_fieldcat_lvc = gt_fcat
    TABLES
      it_data         = it_message
    EXCEPTIONS
      it_data_missing = 1
      OTHERS          = 2.

  LOOP AT gt_fcat INTO gs_fcat.
    IF gs_fcat-fieldname  = 'STATUS'.
      gs_fcat-icon      = 'X'.
      gs_fcat-outputlen = '7'.
      gs_fcat-coltext   = 'Warning'.
      MODIFY gt_fcat FROM gs_fcat.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " BUILD_FIELD_CATALOG
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
FORM display_alv .

  CALL METHOD g_grid->set_table_for_first_display
    EXPORTING
      is_layout                     = gs_layout
    CHANGING
      it_outtab                     = it_message[]
      it_fieldcatalog               = gt_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  save_ok = ok_code.
  CLEAR ok_code.

  CASE save_ok.
    WHEN 'PROCESS' OR 'CANCEL'.
      LEAVE TO SCREEN 0.

  ENDCASE.
ENDMODULE.                 " EXIT  INPUT

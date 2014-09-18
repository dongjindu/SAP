*&--------------------------------------------------------------------&*
* Program Name      : ZFIE_BOA_PARK_DOCUMENT
* Author            : Valerian Utama
* Creation Date     : 10/29/2010
* Specifications By : Calvin Kong
* Pattern           :
* Addl Documentation:
* Description       : BOA Credit Card Automate FI Park Document
*
*&--------------------------------------------------------------------&*
* Modification Logs
* Date       Developer  Request ID  Description
* 10/29/2010 VALERIAN   UD1K950095  Initial Program Development
* 02/18/2011 VALERIAN   UD1K950901  Correct BDC Logic to Park Document
* 02/21/2011 VALERIAN   UD1K950946  Re-design the program:
*                                   Split Park Document based on TM
*                                   Change default Vendor
*                                   Provide input for document Date
*                                   Default G/L item tax code to 'I1'
*&--------------------------------------------------------------------&*

REPORT zfie_boa_park_document MESSAGE-ID zmfi.
CONSTANTS: c_bukrs TYPE bukrs VALUE 'H201',
           c_lifnr TYPE lifnr VALUE '0000501941'.

* ALV Data
TYPE-POOLS: slis, icon.

DATA : gt_fieldcat TYPE slis_t_fieldcat_alv,
       gt_sort     TYPE slis_t_sortinfo_alv,
       gs_sort     TYPE slis_sortinfo_alv.

* BDC Data
DATA: BEGIN OF bdcdata OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF bdcdata.

DATA: BEGIN OF it_message OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF it_message.

* Program Data
CONSTANTS: c_row_max TYPE i VALUE '65536'.

FIELD-SYMBOLS: <fs> TYPE ANY.

DATA: BEGIN OF t_file OCCURS 0,
        kostl(10) TYPE c,        "Cost Center
        hkont(10) TYPE c,        "G/L Account
        pernr(10) TYPE c,        "Team Member
        xblnr(16) TYPE c,        "Txn Number
        budat(10) TYPE c,        "Posting Date
        bedat(10) TYPE c,        "Purchasing Date
        dmbtr(16) TYPE c,        "Amount
        commt(50) TYPE c,        "Comment
      END OF t_file.

DATA: BEGIN OF t_data OCCURS 0.
        INCLUDE STRUCTURE t_file.
DATA: icon(4)    TYPE c,
      belnr      TYPE belnr_d,
      messg(132) TYPE c,
      dmbt1      TYPE dmbtr,
      END OF t_data.

DATA: t_excel     TYPE alsmex_tabline OCCURS 0 WITH HEADER LINE,
      i_begin_col TYPE i VALUE 1,
      i_begin_row TYPE i VALUE 2,
      i_end_col   TYPE i VALUE 8,
      i_end_row   TYPE i VALUE 2.

DATA: v_file_table TYPE filetable,
      v_rc TYPE i.

DATA: g_total TYPE dmbtr,
      g_ctotal(16) TYPE c,
      g_rec TYPE i,
      g_prk TYPE i,
      g_err TYPE i.

SELECTION-SCREEN BEGIN OF BLOCK sel WITH FRAME TITLE text-001.
PARAMETERS: p_bukrs TYPE bukrs OBLIGATORY DEFAULT c_bukrs,
            p_lifnr TYPE lifnr OBLIGATORY DEFAULT c_lifnr,
            p_sgtxt TYPE sgtxt OBLIGATORY,
            p_budat TYPE budat OBLIGATORY DEFAULT sy-datum,
            p_bldat TYPE bldat OBLIGATORY DEFAULT sy-datum.

SELECTION-SCREEN SKIP.
PARAMETERS: p_file TYPE rlgrap-filename OBLIGATORY.

SELECTION-SCREEN SKIP.
PARAMETERS: r_simu RADIOBUTTON GROUP gr1 DEFAULT 'X',
            r_park RADIOBUTTON GROUP gr1.
SELECTION-SCREEN END OF BLOCK sel.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Input File'
      initial_directory       = 'C:\'
    CHANGING
      file_table              = v_file_table
      rc                      = v_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      OTHERS                  = 4.

  READ TABLE v_file_table INDEX 1 INTO p_file.

START-OF-SELECTION.
* Read Data from excel file
  PERFORM get_data.

* Process/Update Data
  PERFORM process_data.

* Display data using ALV
  PERFORM display_data.


*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       Initialize Field Catalog
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT[]  text
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
    ' '  'PERNR'     'Team Member'       10  'CHAR' '' '',
    ' '  'KOSTL'     'Cost Center'       10  'CHAR' '' '',
    ' '  'HKONT'     'G/L Account'       10  'CHAR' '' '',
    ' '  'XBLNR'     'Txn Number'        16  'CHAR' '' '',
    ' '  'BUDAT'     'Post. Date'        10  'CHAR' '' '',
    ' '  'BEDAT'     'Purch.Date'        10  'CHAR' '' '',
    ' '  'DMBT1'     'Amount'            16  'CURR'  '' '',
    ' '  'COMMT'     'Comment'           50  'CHAR' '' '',
    ' '  'ICON'      'Status'            4   'CHAR' '' '',
    ' '  'BELNR'     'Park Doc.'         10  'CHAR' '' '',
    ' '  'MESSG'     'Message'          132  'CHAR' '' ''.

* Make Park Document as Hot Spot
  gs_fieldcat-hotspot = 'X'.
  MODIFY ft_fieldcat FROM gs_fieldcat TRANSPORTING hotspot
  WHERE fieldname = 'BELNR'.

* Total Amount by Team Member
  gs_fieldcat-do_sum = 'X'.
  MODIFY ft_fieldcat FROM gs_fieldcat TRANSPORTING do_sum
  WHERE fieldname = 'DMBT1'.

ENDFORM.                    " fieldcat_init

*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_grid_display TABLES ft_outtab.

  DATA : gs_layout TYPE slis_layout_alv,
         l_repid TYPE sy-repid.

  l_repid = sy-repid.

  gs_layout-confirmation_prompt ='X'.
  gs_layout-colwidth_optimize = 'X'.
  gs_layout-zebra = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program      = l_repid
            i_callback_user_command = 'USER_COMMAND'
            i_callback_top_of_page  = 'TOP_OF_PAGE'
            is_layout               = gs_layout
            it_fieldcat             = gt_fieldcat
            it_sort                 = gt_sort
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
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data .
  PERFORM fieldcat_init     USING gt_fieldcat[].
  PERFORM sort_build        USING gt_sort[].
  PERFORM alv_grid_display  TABLES t_data.
ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       Get data from excel file
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  DATA: l_row_inc TYPE i VALUE '1',
        l_tot_rec TYPE i.

  WHILE i_begin_row <= c_row_max.

* Optimize the reading process
    l_row_inc = l_row_inc * 10.
    IF l_row_inc GT 10000.
      l_row_inc = 10000.
    ENDIF.

    i_end_row = i_begin_row + l_row_inc - 1.

    IF i_end_row GT c_row_max.
      i_end_row = c_row_max.
    ENDIF.

    CLEAR: t_excel, l_tot_rec.
    REFRESH t_excel.

    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
         EXPORTING
              filename                = p_file
              i_begin_col             = i_begin_col
              i_begin_row             = i_begin_row
              i_end_col               = i_end_col
              i_end_row               = i_end_row
         TABLES
              intern                  = t_excel
         EXCEPTIONS
              inconsistent_parameters = 1
              upload_ole              = 2
              OTHERS                  = 3.

    IF sy-subrc <> 0.
      MESSAGE s000 WITH text-m04.
      LEAVE PROGRAM.
    ENDIF.

    SORT t_excel BY row col.
    LOOP AT t_excel.
      ASSIGN COMPONENT t_excel-col OF STRUCTURE t_file TO <fs>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF t_excel-col = 7.
        TRANSLATE: t_excel-value USING ', ',
                   t_excel-value USING '$ '.
        CONDENSE t_excel-value NO-GAPS.

* Only numeric character is allowed
        CHECK t_excel-value CO '1234567890. '.
      ENDIF.

      <fs> = t_excel-value.

      AT END OF row.
        APPEND t_file. CLEAR t_file.
        l_tot_rec = l_tot_rec + 1.
      ENDAT.
    ENDLOOP.

    IF l_tot_rec LT l_row_inc.
      EXIT.
    ENDIF.

    i_begin_row = i_begin_row + l_row_inc.
  ENDWHILE.

  FREE t_excel.

ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       Process/Update data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data.
  DATA: BEGIN OF t_file_all OCCURS 0,
          pernr(10) TYPE c,        "Team Member
          kostl(10) TYPE c,        "Cost Center
          hkont(10) TYPE c,        "G/L Account
          xblnr(16) TYPE c,        "Txn Number
          budat(10) TYPE c,        "Posting Date
          bedat(10) TYPE c,        "Purchasing Date
          dmbtr(16) TYPE c,        "Amount
          commt(50) TYPE c,        "Comment
        END OF t_file_all.

  LOOP AT t_file.
    MOVE-CORRESPONDING t_file TO t_file_all.
    APPEND t_file_all.

    CLEAR t_data.
    MOVE-CORRESPONDING t_file TO t_data.
    t_data-dmbt1 = t_file-dmbtr.
    APPEND t_data.
  ENDLOOP.

  REFRESH t_file.
  CLEAR:  t_file, g_rec, g_total, g_prk, g_err.

  SORT t_file_all BY pernr.

  LOOP AT t_file_all.
    MOVE-CORRESPONDING t_file_all TO t_file.
    APPEND t_file.

    g_total = g_total + t_file-dmbtr.
    g_rec   = g_rec + 1.

    AT END OF pernr.
      g_ctotal = g_total.

      CASE 'X'.
        WHEN r_park.
          PERFORM park_data.
        WHEN r_simu.
          PERFORM simulate_park.
      ENDCASE.

      CLEAR: g_total, g_rec.
      REFRESH t_file.
    ENDAT.

    AT LAST.
      CASE 'X'.
        WHEN r_park.
          MESSAGE s000 WITH g_prk text-m02 g_err text-m05.
        WHEN r_simu.
          DESCRIBE TABLE t_file_all LINES g_rec.
          MESSAGE s000 WITH text-m01 g_rec text-m03 p_file.
      ENDCASE.

    ENDAT.
  ENDLOOP.

  FREE: t_file.

ENDFORM.                    " process_data

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
*&      Form  park_data
*&---------------------------------------------------------------------*
*       Park Document
*----------------------------------------------------------------------*
FORM park_data.
  DATA: l_next_idx  TYPE sy-tabix,
        wa_curr_rec LIKE t_file,
        wa_next_rec LIKE t_file,
        l_date(10)  TYPE c,
        l_date1(10) TYPE c,
        l_rec_msg   TYPE i,
        l_xblnr     TYPE xblnr.

  WRITE: p_budat TO l_date,
         p_bldat TO l_date1.

  CLEAR t_data.

  LOOP AT t_file.
    l_next_idx = sy-tabix.
    l_next_idx = l_next_idx + 1.
    wa_curr_rec = t_file.

    CLEAR wa_next_rec.
    READ TABLE t_file INTO wa_next_rec
                      INDEX l_next_idx.

    AT FIRST.
      CONCATENATE wa_curr_rec-pernr '-' p_budat+4(2) p_budat+2(2)
             INTO l_xblnr.
      PERFORM bdc_dynpro USING: 'SAPLF040' '0100'.
      PERFORM bdc_field  USING: 'BDC_OKCODE' '/00',
                                'BKPF-BLDAT' l_date1,
                                'BKPF-BLART' 'KR',
                                'BKPF-BUKRS' p_bukrs,
                                'BKPF-BUDAT' l_date,
                                'BKPF-MONAT' sy-datum+4(2),
                                'BKPF-WAERS' 'USD',
                                'BKPF-XBLNR' l_xblnr,
                                'VBKPF-XBWAE' 'X',
                                'FS006-DOCID' '*',
                                'RF05V-NEWBS' '31',
                                'RF05V-NEWKO' p_lifnr.

      PERFORM bdc_dynpro USING: 'SAPLF040' '0302'.
      PERFORM bdc_field  USING: 'BDC_OKCODE' '/00',
                                'BSEG-WRBTR' g_ctotal,
                                'BSEG-MWSKZ' '**',
                                'BSEG-SGTXT' p_sgtxt,
                                'RF05V-NEWBS' '40',
                                'RF05V-NEWKO' wa_curr_rec-hkont.
    ENDAT.

    AT LAST.
      PERFORM bdc_dynpro USING: 'SAPLF040' '0300'.
      PERFORM bdc_field  USING: 'BDC_OKCODE' '=BP',
                                'BSEG-WRBTR' wa_curr_rec-dmbtr,
                                'BSEG-MWSKZ' 'I1',
                                'BSEG-SGTXT' wa_curr_rec-commt,
                                'BSEG-ZUONR' wa_curr_rec-xblnr,
                                'DKACB-FMORE' 'X'.

      PERFORM bdc_dynpro USING: 'SAPLKACB' '0002'.
      PERFORM bdc_field  USING: 'BDC_OKCODE' '=ENTE',
                                'COBL-KOSTL' wa_curr_rec-kostl.

      CALL TRANSACTION 'F-63'
               USING bdcdata
               MODE 'N'
               UPDATE 'S'
           MESSAGES INTO it_message.

      IF sy-subrc EQ 0.
        g_prk = g_prk + 1.

        DESCRIBE TABLE it_message LINES l_rec_msg.
        READ TABLE it_message INDEX l_rec_msg.

        IF sy-subrc = 0 AND it_message-msgtyp = 'S'.
          t_data-icon  = icon_green_light.
          t_data-belnr = it_message-msgv1.

          MESSAGE ID it_message-msgid
                TYPE it_message-msgtyp
              NUMBER it_message-msgnr
                WITH it_message-msgv1 it_message-msgv2
                     it_message-msgv3 it_message-msgv4
                INTO t_data-messg.

          MODIFY t_data TRANSPORTING icon belnr messg
                               WHERE pernr = wa_curr_rec-pernr.
        ENDIF.

      ELSE.
        g_err = g_err + 1.

        DESCRIBE TABLE it_message LINES l_rec_msg.
        READ TABLE it_message INDEX l_rec_msg.

        IF sy-subrc = 0.
          IF it_message-msgtyp = 'E'.
            t_data-icon  = icon_red_light.
          ELSE.
            t_data-icon  = icon_yellow_light.
          ENDIF.

          CLEAR t_data-belnr.

          MESSAGE ID it_message-msgid
                TYPE it_message-msgtyp
              NUMBER it_message-msgnr
                WITH it_message-msgv1 it_message-msgv2
                     it_message-msgv3 it_message-msgv4
                INTO t_data-messg.

          MODIFY t_data TRANSPORTING icon belnr messg
                               WHERE pernr = wa_curr_rec-pernr.
        ENDIF.

      ENDIF.
      REFRESH: bdcdata, it_message.
      EXIT.

    ENDAT.

    PERFORM bdc_dynpro USING: 'SAPLF040' '0300'.
    PERFORM bdc_field  USING: 'BDC_OKCODE' '/00',
                              'BSEG-WRBTR' wa_curr_rec-dmbtr,
                              'BSEG-MWSKZ' 'I1',
                              'BSEG-SGTXT' wa_curr_rec-commt,
                              'BSEG-ZUONR' wa_curr_rec-xblnr,
                              'RF05V-NEWBS' '40',
                              'RF05V-NEWKO' wa_next_rec-hkont,
                              'DKACB-FMORE' 'X'.
    PERFORM bdc_dynpro USING: 'SAPLKACB' '0002'.
    PERFORM bdc_field  USING: 'BDC_OKCODE' '=ENTE',
                              'COBL-KOSTL' wa_curr_rec-kostl.
  ENDLOOP.
ENDFORM.                    " park_data
*&---------------------------------------------------------------------*
*&      Form  simulate_park
*&---------------------------------------------------------------------*
*       Simulate Park for checking
*----------------------------------------------------------------------*
FORM simulate_park.
  DATA: l_next_idx  TYPE sy-tabix,
        wa_curr_rec LIKE t_file,
        wa_next_rec LIKE t_file,
        l_date(10)  TYPE c,
        l_date1(10) TYPE c,
        l_rec_msg   TYPE i,
        l_xblnr     TYPE xblnr.

  WRITE: p_budat TO l_date,
         p_bldat TO l_date1.

  CLEAR t_data.

  LOOP AT t_file.
    l_next_idx = sy-tabix.
    l_next_idx = l_next_idx + 1.
    wa_curr_rec = t_file.

    CLEAR wa_next_rec.
    READ TABLE t_file INTO wa_next_rec
                      INDEX l_next_idx.

    AT FIRST.
      CONCATENATE wa_curr_rec-pernr '-' p_budat+4(2) p_budat+2(2)
             INTO l_xblnr.
      PERFORM bdc_dynpro USING: 'SAPLF040' '0100'.
      PERFORM bdc_field  USING: 'BDC_OKCODE' '/00',
                                'BKPF-BLDAT' l_date1,
                                'BKPF-BLART' 'KR',
                                'BKPF-BUKRS' p_bukrs,
                                'BKPF-BUDAT' l_date,
                                'BKPF-MONAT' sy-datum+4(2),
                                'BKPF-WAERS' 'USD',
                                'BKPF-XBLNR' l_xblnr,
                                'VBKPF-XBWAE' 'X',
                                'FS006-DOCID' '*',
                                'RF05V-NEWBS' '31',
                                'RF05V-NEWKO' p_lifnr.

      PERFORM bdc_dynpro USING: 'SAPLF040' '0302'.
      PERFORM bdc_field  USING: 'BDC_OKCODE' '/00',
                                'BSEG-WRBTR' g_ctotal,
                                'BSEG-MWSKZ' '**',
                                'BSEG-SGTXT' p_sgtxt,
                                'RF05V-NEWBS' '40',
                                'RF05V-NEWKO' wa_curr_rec-hkont.
    ENDAT.

    AT LAST.
      PERFORM bdc_dynpro USING: 'SAPLF040' '0300'.
      PERFORM bdc_field  USING: 'BDC_OKCODE' '=AB',
                                'BSEG-WRBTR' wa_curr_rec-dmbtr,
                                'BSEG-MWSKZ' 'I1',
                                'BSEG-SGTXT' wa_curr_rec-commt,
                                'BSEG-ZUONR' wa_curr_rec-xblnr,
                                'DKACB-FMORE' 'X'.

      PERFORM bdc_dynpro USING: 'SAPLKACB' '0002'.
      PERFORM bdc_field  USING: 'BDC_OKCODE' '=ENTE',
                                'COBL-KOSTL' wa_curr_rec-kostl.

      PERFORM bdc_dynpro USING: 'SAPLF040' '0700'.
      PERFORM bdc_field  USING: 'BDC_OKCODE' '=RW'.

      PERFORM bdc_dynpro USING: 'SAPLSPO1' '0200'.
      PERFORM bdc_field  USING: 'BDC_OKCODE' '=YES'.

      CALL TRANSACTION 'F-63'
               USING bdcdata
               MODE 'N'
               UPDATE 'S'
           MESSAGES INTO it_message.

      IF sy-subrc EQ 0.
        t_data-icon  = icon_green_light.

        DESCRIBE TABLE it_message LINES l_rec_msg.
        READ TABLE it_message INDEX l_rec_msg.

        IF sy-subrc = 0.
          IF it_message-msgtyp = 'W'.
            t_data-icon  = icon_yellow_light.
          ENDIF.

          MESSAGE ID it_message-msgid
                TYPE it_message-msgtyp
              NUMBER it_message-msgnr
                WITH it_message-msgv1 it_message-msgv2
                     it_message-msgv3 it_message-msgv4
                INTO t_data-messg.
        ENDIF.

        MODIFY t_data TRANSPORTING icon messg
                             WHERE pernr = wa_curr_rec-pernr.

      ELSE.
        DESCRIBE TABLE it_message LINES l_rec_msg.
        READ TABLE it_message INDEX l_rec_msg.

        IF sy-subrc = 0.
          IF it_message-msgtyp = 'E'.
            t_data-icon  = icon_red_light.
          ELSE.
            t_data-icon  = icon_yellow_light.
          ENDIF.

          CLEAR t_data-belnr.

          MESSAGE ID it_message-msgid
                TYPE it_message-msgtyp
              NUMBER it_message-msgnr
                WITH it_message-msgv1 it_message-msgv2
                     it_message-msgv3 it_message-msgv4
                INTO t_data-messg.

          MODIFY t_data TRANSPORTING icon belnr messg
                               WHERE pernr = wa_curr_rec-pernr.
        ENDIF.
      ENDIF.

      REFRESH: bdcdata, it_message.
      EXIT.
    ENDAT.

    PERFORM bdc_dynpro USING: 'SAPLF040' '0300'.
    PERFORM bdc_field  USING: 'BDC_OKCODE' '/00',
                              'BSEG-WRBTR' wa_curr_rec-dmbtr,
                              'BSEG-MWSKZ' 'I1',
                              'BSEG-SGTXT' wa_curr_rec-commt,
                              'BSEG-ZUONR' wa_curr_rec-xblnr,
                              'RF05V-NEWBS' '40',
                              'RF05V-NEWKO' wa_next_rec-hkont,
                              'DKACB-FMORE' 'X'.
    PERFORM bdc_dynpro USING: 'SAPLKACB' '0002'.
    PERFORM bdc_field  USING: 'BDC_OKCODE' '=ENTE',
                              'COBL-KOSTL' wa_curr_rec-kostl.
  ENDLOOP.
ENDFORM.                    " simulate_park
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       Display park document
*----------------------------------------------------------------------*
FORM user_command USING r_ucomm     LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.
  CASE r_ucomm.
    WHEN '&IC1'.

      CASE rs_selfield-fieldname.
        WHEN 'BELNR'.
          READ TABLE t_data INDEX rs_selfield-tabindex.
          IF sy-subrc = 0.

            CHECK NOT t_data-belnr IS INITIAL.

            SELECT SINGLE belnr
              FROM vbkpf
              INTO t_data-belnr
             WHERE ausbk = p_bukrs
               AND bukrs = p_bukrs
               AND belnr = t_data-belnr
               AND gjahr = p_budat(4).

            IF sy-subrc = 0.
              SET PARAMETER ID: 'BUK' FIELD p_bukrs,
                                'BLP' FIELD t_data-belnr,
                                'GJR' FIELD p_budat(4).

              CALL TRANSACTION 'FBV3' AND SKIP FIRST SCREEN.
            ELSE.
              MESSAGE i076(fp) WITH p_bukrs t_data-belnr p_budat(4).
            ENDIF.
          ENDIF.
      ENDCASE.
  ENDCASE.
ENDFORM.                    " user_command

*&---------------------------------------------------------------------*
*&      Form  sort_build
*&---------------------------------------------------------------------*
*       Sort Data
*----------------------------------------------------------------------*
*      -->ft_sort[]  Sort Table
*----------------------------------------------------------------------*
FORM sort_build USING ft_sort TYPE slis_t_sortinfo_alv.

  DEFINE sort_tab.
    clear gs_sort.
    gs_sort-fieldname = &1.
    gs_sort-spos      = &2.
    gs_sort-up        = &3.
    gs_sort-group     = &4.
    gs_sort-comp      = &5.
    gs_sort-subtot    = &6.
    append gs_sort to ft_sort.
  END-OF-DEFINITION.

  sort_tab :
     'PERNR'    '01' 'X' ' ' ' ' 'X',
     'BUDAT'    '02' 'X' ' ' ' ' ' ',
     'ICON'     '03' 'X' ' ' ' ' ' ',
     'BELNR'    '04' 'X' ' ' ' ' ' ',
     'MESSG'    '05' 'X' ' ' ' ' ' '.

ENDFORM.                    " SORT_BUILD

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
  gs_listheader-key  = text-t01.
  CONCATENATE c_bukrs '-' gs_listheader-info
         INTO gs_listheader-info SEPARATED BY space.
  APPEND gs_listheader TO gt_listheader.

  SELECT SINGLE name1 INTO gs_listheader-info
    FROM lfa1
   WHERE lifnr = c_lifnr.

  gs_listheader-typ  = 'S'.
  gs_listheader-key  = text-t02.
  CONCATENATE c_lifnr '-' gs_listheader-info
         INTO gs_listheader-info SEPARATED BY space.
  SHIFT gs_listheader-info LEFT DELETING LEADING '0'.
  APPEND gs_listheader TO gt_listheader.

  IF r_park IS INITIAL.
    gs_listheader-typ = 'A'.
    gs_listheader-key = 'DESCR'.
    gs_listheader-info = text-t03.
    APPEND gs_listheader TO gt_listheader.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            it_list_commentary = gt_listheader.

ENDFORM.                    "top_of_page

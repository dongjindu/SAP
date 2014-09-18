*&----------------------------------------------------------------------
*& Program ID        : ZFIE_AUTOCLEAR_DP_ITEMS
*& Title             : [FI] - Automatic down payment line item clearing
*&                            during IR
*& Created by        : Valerian Utama
*& Created on        : 07/30/2012
*& Specifications By : Calvin Kong
*& Reference Pgm     : N/A
*& Description       : This program allows Finance to post and clear
*&                     "Automatic down payment during IR" line items
*&
*& Modification Log
*& Date        Developer Issue No    Description
*&======================================================================
*& 07/30/2012  Valerian  UD1K955269  Initial Program Development
*&
*&----------------------------------------------------------------------

REPORT  zfie_autoclear_dp_items MESSAGE-ID zmfi.

* Define data for ALV
TYPE-POOLS: slis.

DATA : gt_fieldcat TYPE slis_t_fieldcat_alv,
       gs_fieldcat TYPE slis_fieldcat_alv.

* Define internal table for BDC
DATA: gt_bdc TYPE TABLE OF bdcdata    WITH HEADER LINE,
      gt_msg TYPE TABLE OF bdcmsgcoll WITH HEADER LINE.

* Define program data
TABLES: lfa1.

DATA: BEGIN OF gt_lfa1 OCCURS 0,
        lifnr TYPE lfa1-lifnr,
      END OF gt_lfa1.

DATA: BEGIN OF gt_out OCCURS 0,
        lifnr   LIKE lfa1-lifnr,
        icon(4) TYPE c,
        msg(99) TYPE c,
END OF gt_out.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_lifnr FOR lfa1-lifnr OBLIGATORY.
SELECTION-SCREEN END OF BLOCK blk1.

SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE text-t02.
PARAMETERS: p_post AS CHECKBOX,
            p_mode DEFAULT 'N'.
SELECTION-SCREEN END OF BLOCK blk2.

AT SELECTION-SCREEN.
  IF p_mode NE 'N' AND
     p_mode NE 'A'.
    p_mode = 'N'.
  ENDIF.

START-OF-SELECTION.
  PERFORM get_data.
  PERFORM process_data.
  PERFORM post_or_simulate.
  PERFORM display_data.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  SIMU_DATA
*&---------------------------------------------------------------------*
*       Simulate Clear down payment line items
*----------------------------------------------------------------------*
FORM simu_data.
  DATA: l_tabix TYPE sy-tabix,
        l_count TYPE i.

  LOOP AT gt_out.
    l_tabix = sy-tabix.

    CLEAR: gt_msg, gt_msg[],
           gt_bdc, gt_bdc[].

    PERFORM dynpro USING:
        'X' 'SAPMF05A'        '0131',
        ' ' 'BDC_CURSOR'      'RF05A-XPOS1(12)',
        ' ' 'BDC_OKCODE'      '/00',
        ' ' 'RF05A-AGKON'     gt_out-lifnr,
        ' ' 'RF05A-AGUMS'     'K',
        ' ' 'RF05A-XPOS1(12)' 'X'.

    PERFORM dynpro USING:
        'X' 'SAPMF05A'        '0608',
        ' ' 'BDC_OKCODE'      '/ECNCL',
        ' ' 'BDC_CURSOR'      'RF05A-XPOS1(01)'.

    PERFORM dynpro USING:
        'X' 'SAPMF05A'        '0710',
        ' ' 'BDC_CURSOR'      'RF05A-XPOS1(14)',
        ' ' 'BDC_OKCODE'      '=PA',
        ' ' 'RF05A-AGKON'     gt_out-lifnr,
        ' ' 'RF05A-AGKOA'     'K',
        ' ' 'RF05A-AGUMS'     'K',
        ' ' 'RF05A-XNOPS'     'X',
        ' ' 'RF05A-XPOS1(14)' 'X'.

    PERFORM dynpro USING:
        'X' 'SAPMF05A'        '0731',
        ' ' 'BDC_CURSOR'      'RF05A-SEL02(02)',
        ' ' 'BDC_OKCODE'      '=BS',
        ' ' 'RF05A-SEL01(01)' '27',
        ' ' 'RF05A-SEL01(02)' '39',
        ' ' 'RF05A-SEL02(01)' '27',
        ' ' 'RF05A-SEL02(02)' '39'.

    PERFORM dynpro USING:
        'X' 'SAPMF05A'        '0700',
        ' ' 'BDC_OKCODE'      '/ECNC',
        ' ' 'BDC_CURSOR'      'RF05A-NEWBS'.

    PERFORM dynpro USING:
        'X' 'SAPLSPO1'        '0200',
        ' ' 'BDC_OKCODE'      '=YES'.

    CALL TRANSACTION 'F-44'   USING gt_bdc
                              MODE p_mode
                              UPDATE 'S'
                              MESSAGES INTO gt_msg.

    IF sy-subrc = 0.
      gt_out-icon = icon_green_light.
    ELSE.
      gt_out-icon = icon_red_light.
    ENDIF.

    DELETE gt_msg WHERE msgid = '00'
                    AND msgnr = '344'.

    DESCRIBE TABLE gt_msg LINES l_count.
    READ TABLE gt_msg INDEX l_count.
    IF sy-subrc = 0.

      IF gt_msg-msgtyp NE 'S' AND
         gt_msg-msgtyp NE 'E'.
        gt_out-icon = icon_yellow_light.
      ENDIF.

      MESSAGE ID gt_msg-msgid TYPE gt_msg-msgtyp NUMBER gt_msg-msgnr
         WITH gt_msg-msgv1 gt_msg-msgv2 gt_msg-msgv3 gt_msg-msgv4
         INTO gt_out-msg.

      MODIFY gt_out INDEX l_tabix TRANSPORTING icon msg.

    ENDIF.
  ENDLOOP.
ENDFORM.                    " SIMU_DATA

*&---------------------------------------------------------------------*
*&      Form  POST_DATA
*&---------------------------------------------------------------------*
*       Clear down payment line items
*----------------------------------------------------------------------*
FORM post_data .
  DATA: l_tabix TYPE sy-tabix,
        l_count TYPE i.

  LOOP AT gt_out.
    l_tabix = sy-tabix.

    CLEAR: gt_msg, gt_msg[],
           gt_bdc, gt_bdc[].

    PERFORM dynpro USING:
        'X' 'SAPMF05A'        '0131',
        ' ' 'BDC_CURSOR'      'RF05A-XPOS1(12)',
        ' ' 'BDC_OKCODE'      '/00',
        ' ' 'RF05A-AGKON'     gt_out-lifnr,
        ' ' 'RF05A-AGUMS'     'K',
        ' ' 'RF05A-XPOS1(12)' 'X'.

    PERFORM dynpro USING:
        'X' 'SAPMF05A'        '0608',
        ' ' 'BDC_OKCODE'      '/ECNCL',
        ' ' 'BDC_CURSOR'      'RF05A-XPOS1(01)'.

    PERFORM dynpro USING:
        'X' 'SAPMF05A'        '0710',
        ' ' 'BDC_CURSOR'      'RF05A-XPOS1(14)',
        ' ' 'BDC_OKCODE'      '=PA',
        ' ' 'RF05A-AGKON'     gt_out-lifnr,
        ' ' 'RF05A-AGKOA'     'K',
        ' ' 'RF05A-AGUMS'     'K',
        ' ' 'RF05A-XNOPS'     'X',
        ' ' 'RF05A-XPOS1(14)' 'X'.

    PERFORM dynpro USING:
        'X' 'SAPMF05A'        '0731',
        ' ' 'BDC_CURSOR'      'RF05A-SEL02(02)',
        ' ' 'BDC_OKCODE'      '=PA',
        ' ' 'RF05A-SEL01(01)' '27',
        ' ' 'RF05A-SEL01(02)' '39',
        ' ' 'RF05A-SEL02(01)' '27',
        ' ' 'RF05A-SEL02(02)' '39'.

    PERFORM dynpro USING:
        'X' 'SAPDF05X'        '3100',
        ' ' 'BDC_OKCODE'      '=BU',
        ' ' 'BDC_CURSOR'      'DF05B-PSSKT(01)',
        ' ' 'RF05A-ABPOS'     '1'.

    CALL TRANSACTION 'F-44'   USING gt_bdc
                              MODE p_mode
                              UPDATE 'S'
                              MESSAGES INTO gt_msg.

    IF sy-subrc = 0.
      gt_out-icon = icon_green_light.
    ELSE.
      gt_out-icon = icon_red_light.
    ENDIF.

    DELETE gt_msg WHERE msgid = '00'
                    AND msgnr = '344'.

    DESCRIBE TABLE gt_msg LINES l_count.
    READ TABLE gt_msg INDEX l_count.
    IF sy-subrc = 0.

      IF gt_msg-msgtyp NE 'S' AND
         gt_msg-msgtyp NE 'E'.
        gt_out-icon = icon_yellow_light.
      ENDIF.

      MESSAGE ID gt_msg-msgid TYPE gt_msg-msgtyp NUMBER gt_msg-msgnr
         WITH gt_msg-msgv1 gt_msg-msgv2 gt_msg-msgv3 gt_msg-msgv4
         INTO gt_out-msg.

      MODIFY gt_out INDEX l_tabix TRANSPORTING icon msg.

    ENDIF.
  ENDLOOP.
ENDFORM.                    " POST_DATA
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
  SELECT lifnr INTO CORRESPONDING FIELDS OF TABLE gt_lfa1
    FROM lfa1
   WHERE lifnr IN s_lifnr.

  IF gt_lfa1[] IS INITIAL.
    MESSAGE s020.
    STOP.
  ENDIF.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       Process Data
*----------------------------------------------------------------------*
FORM process_data .
  LOOP AT gt_lfa1.
    MOVE-CORRESPONDING gt_lfa1 TO gt_out.
    APPEND gt_out. CLEAR gt_out.
  ENDLOOP.

  FREE: gt_lfa1.
ENDFORM.                    " PROCESS_DATA
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
    IF gs_fieldcat-fieldname = 'LIFNR'.
      gs_fieldcat-key = 'X'.
    ELSE.
      CLEAR gs_fieldcat-key.
    ENDIF.

    MODIFY ft_fieldcat FROM gs_fieldcat TRANSPORTING key.
  ENDLOOP.

* Change Description
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
*&---------------------------------------------------------------------*
*&      Form  POST_OR_SIMULATE
*&---------------------------------------------------------------------*
*       Post or Simulate data
*----------------------------------------------------------------------*
FORM post_or_simulate.
  IF p_post IS INITIAL.
    PERFORM simu_data.
  ELSE.
    PERFORM post_data.
  ENDIF.
ENDFORM.                    " POST_OR_SIMULATE

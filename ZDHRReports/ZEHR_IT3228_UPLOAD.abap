*----------------------------------------------------------------------
* Program ID        :
* Title             : Mass Uploading IT3228 (Online Selection)
* Created on        : 10/08/2012
* Created by        : Valerian Utama
* Specifications By : Euna.Lee
* Description       : Create info type 3228 to make online selection
*                     possible.
*
*----------------------------------------------------------------------
*  Modification Log
*  Date        Developer Issue No    Description
*======================================================================
*  10/08/2012  Valerian  UD1K955631  Initial Program Development
*
*----------------------------------------------------------------------

REPORT  zehr_it3228_upload MESSAGE-ID zmhr.

TYPE-POOLS: slis.

DATA: g_sdat TYPE pa0000-begda,
      g_edat TYPE pa0000-endda,
      g_sdatout(10) TYPE c,
      g_edatout(10) TYPE c,
      g_ltext TYPE t5utl-ltext.

DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      wa_fieldcat TYPE slis_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv.

* Define internal table for BDC
DATA: gt_bdc TYPE TABLE OF bdcdata    WITH HEADER LINE,
      gt_msg TYPE TABLE OF bdcmsgcoll WITH HEADER LINE.

DATA: BEGIN OF it_data OCCURS 0,
        pernr LIKE pa0000-pernr,
        begda LIKE pa0000-begda,
        endda LIKE pa0000-endda,
        stat2 LIKE pa0000-stat2,
        icon(4) TYPE c,
        messg LIKE bapiret2-message,
      END OF it_data.

DATA: BEGIN OF it_pa3228 OCCURS 0,
        pernr LIKE pa3228-pernr,
      END OF it_pa3228.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_pernr FOR it_data-pernr.
PARAMETERS: p_year(4) TYPE n OBLIGATORY.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (30) text-t02.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS: p_txcmp TYPE t5utl-txcmp DEFAULT 'H201'.
SELECTION-SCREEN COMMENT (40) comm1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.
PARAMETERS: p_creat AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK blk1.

INITIALIZATION.
  p_year = sy-datum(4).

AT SELECTION-SCREEN OUTPUT.
  SELECT SINGLE ltext INTO g_ltext
    FROM t5utl
   WHERE txcmp = p_txcmp.
  comm1 = g_ltext.

  LOOP AT SCREEN.
    IF screen-name = 'P_TXCMP'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

START-OF-SELECTION.
  PERFORM get_data.
  PERFORM post_data.
  PERFORM display_data.

END-OF-SELECTION.

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
*&      Form  CHANGE_DESC
*&---------------------------------------------------------------------*
*       Change ALV field description
*----------------------------------------------------------------------*
*      -->P_FIELD   Field Name
*      -->P_DESC    Field Description
*----------------------------------------------------------------------*
FORM change_desc  USING    p_field TYPE c
                           p_desc  TYPE c.

  READ TABLE gt_fieldcat INTO wa_fieldcat
                         WITH KEY fieldname = p_field.
  IF sy-subrc = 0.
    wa_fieldcat-seltext_l    = p_desc.
    wa_fieldcat-seltext_m    = p_desc.
    wa_fieldcat-seltext_s    = p_desc.
    wa_fieldcat-reptext_ddic = p_desc.
    MODIFY gt_fieldcat FROM wa_fieldcat INDEX sy-tabix.
  ENDIF.
ENDFORM.                    " CHANGE_DESC
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       Get Data
*----------------------------------------------------------------------*
FORM get_data .
  CONCATENATE: p_year '01' '01' INTO g_sdat,
               p_year '12' '31' INTO g_edat.

  WRITE: g_sdat TO g_sdatout,
         g_edat TO g_edatout.

  SELECT pernr begda endda stat2
    INTO CORRESPONDING FIELDS OF TABLE it_data
    FROM pa0000
   WHERE pernr IN s_pernr
     AND endda >= g_sdat
     AND begda <= g_edat
     AND ( stat2 = '3' OR stat2 = '1' ).

* Terminate if no data selected
  IF it_data[] IS INITIAL.
    MESSAGE s001 WITH 'No Data Found' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  SORT it_data BY pernr endda DESCENDING.
  DELETE ADJACENT DUPLICATES FROM it_data COMPARING pernr.

  SELECT pernr
    INTO CORRESPONDING FIELDS OF TABLE it_pa3228
    FROM pa3228
    FOR ALL ENTRIES IN it_data
   WHERE pernr = it_data-pernr
     AND subty = 'W2'
     AND endda = '99991231'
     AND begda = g_sdat.

  SORT it_pa3228 BY pernr.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  POST_DATA
*&---------------------------------------------------------------------*
*       Post/Create Info Records
*----------------------------------------------------------------------*
FORM post_data .
  CHECK NOT p_creat IS INITIAL.

  DATA: l_tabix TYPE sy-tabix,
        l_lines TYPE i.

  LOOP AT it_data.
    l_tabix = sy-tabix.

    READ TABLE it_pa3228 WITH KEY pernr = it_data-pernr
                         BINARY SEARCH.
    IF sy-subrc = 0.
      it_data-icon = icon_red_light.
      it_data-messg = 'Infotype has been existed'.
      MODIFY it_data INDEX l_tabix TRANSPORTING icon messg.
      CONTINUE.
    ENDIF.

    PERFORM dynpro USING:
        'X' 'SAPMP50A'        '1000',
        ' ' 'BDC_OKCODE'      '=INS',
        ' ' 'RP50G-PERNR'     it_data-pernr,
        ' ' 'RP50G-TIMR6'     'X',
        ' ' 'RP50G-BEGDA'     g_sdatout,
        ' ' 'RP50G-ENDDA'     g_edatout,
        ' ' 'RP50G-CHOIC'     '3228',
        ' ' 'RP50G-SUBTY'     'W2'.

    PERFORM dynpro USING:
        'X' 'SAPLSPO1'        '0500',
        ' ' 'BDC_OKCODE'      '=OPT1'.

    PERFORM dynpro USING:
        'X' 'MP322800'        '2010',
        ' ' 'BDC_OKCODE'      '/00',
        ' ' 'Q3228-ELRES01'   '2'.

    PERFORM dynpro USING:
        'X' 'MP322800'        '2010',
        ' ' 'BDC_OKCODE'      '=UPD'.

    PERFORM dynpro USING:
        'X' 'SAPLSPO1'        '0500',
        ' ' 'BDC_OKCODE'      '=OPT1'.

    PERFORM dynpro USING:
        'X' 'MP322800'        '2010',
        ' ' 'Q3228-ELRES01'   '2'.

    CALL TRANSACTION 'PA30'   USING         gt_bdc
                              MODE 'N'
                              MESSAGES INTO gt_msg.

    IF sy-subrc = 0.
      it_data-icon = icon_green_light.
    ELSE.
      it_data-icon = icon_red_light.
    ENDIF.

    DESCRIBE TABLE gt_msg LINES l_lines.

    WHILE l_lines > 0.
      READ TABLE gt_msg INDEX l_lines.
      IF sy-subrc = 0.
        MESSAGE ID gt_msg-msgid TYPE gt_msg-msgtyp
         NUMBER gt_msg-msgnr
           WITH gt_msg-msgv1 gt_msg-msgv2 gt_msg-msgv3 gt_msg-msgv4
           INTO it_data-messg.

        IF gt_msg-msgnr <> '344'.
          EXIT.
        ENDIF.
      ENDIF.

      l_lines = l_lines - 1.

    ENDWHILE.

    MODIFY it_data INDEX l_tabix TRANSPORTING icon messg.

    CLEAR: gt_bdc, gt_bdc[],
           gt_msg, gt_msg[].
  ENDLOOP.
ENDFORM.                    " POST_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       Display Data
*----------------------------------------------------------------------*
FORM display_data .
  DATA: l_repid TYPE sy-repid.

  l_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = l_repid
      i_internal_tabname = 'IT_DATA'
      i_inclname         = l_repid
    CHANGING
      ct_fieldcat        = gt_fieldcat.

  gs_layout-colwidth_optimize = 'X'.
  gs_layout-zebra = 'X'.

  PERFORM change_desc USING 'ICON' 'Status'.
  PERFORM change_cat USING: 'BEGDA' 'TECH' 'X',
                            'ENDDA' 'TECH' 'X',
                            'STAT2' 'TECH' 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = l_repid
      is_layout          = gs_layout
      it_fieldcat        = gt_fieldcat
    TABLES
      t_outtab           = it_data
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  CHANGE_CAT
*&---------------------------------------------------------------------*
*       Change field catalog property
*----------------------------------------------------------------------*
*      -->P_FIELDNAME   Field Name
*      -->P_PROPERTY    Property
*      -->P_VALUE       Value of Property
*----------------------------------------------------------------------*
FORM change_cat  USING    p_fieldname TYPE c
                          p_property  TYPE c
                          p_value     TYPE any.

  FIELD-SYMBOLS: <fs> TYPE any.

  READ TABLE gt_fieldcat INTO wa_fieldcat
                     WITH KEY fieldname = p_fieldname.
  IF sy-subrc = 0.
    ASSIGN COMPONENT p_property OF STRUCTURE wa_fieldcat TO <fs>.
    IF sy-subrc = 0.
      <fs> = p_value.
      MODIFY gt_fieldcat FROM wa_fieldcat INDEX sy-tabix.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHANGE_CAT

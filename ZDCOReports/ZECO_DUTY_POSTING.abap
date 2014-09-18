*&----------------------------------------------------------------------
*& Program ID        : ZECO_DUTY_POSTING
*& Title             : [CO] - Duty Posting Report
*& Created by        : Valerian Utama
*& Created on        : 12/06/2012
*& Specifications By : An Hyung Ki
*& Reference Pgm     : N/A
*& Description       : Posting duty to Sold Material
*&
*& Modification Log
*& Date        Developer Issue No    Description
*&======================================================================
*& 12/06/2012  Valerian  UD1K955915  Initial Program Development
*&
*&----------------------------------------------------------------------

REPORT  zeco_duty_posting.

TYPE-POOLS: slis.

DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      wa_fieldcat TYPE slis_fieldcat_alv,
      gs_layout TYPE slis_layout_alv,
      gt_sort TYPE slis_t_sortinfo_alv,
      wa_sort TYPE slis_sortinfo_alv.

DATA: g_frstday TYPE sy-datum,
      g_lastday TYPE sy-datum,
      g_posted(1) TYPE c.

DATA: it_duty_dist TYPE ztco_duty_dist OCCURS 0 WITH HEADER LINE,
      it_duty_post TYPE ztco_duty_post OCCURS 0 WITH HEADER LINE,

      BEGIN OF it_data OCCURS 0,
        hkont LIKE ztco_duty_post-hkont,
        txt50 LIKE skat-txt50,
        matnr LIKE ztco_duty_post-matnr,
        wrbtr LIKE ztco_duty_post-wrbtr,
        belnr LIKE ztco_duty_post-belnr,
        messg LIKE ztco_duty_post-messg,
      END OF it_data,

      BEGIN OF it_map OCCURS 0,
        class TYPE ztco_duty_dist-class,
        hkont TYPE ztco_duty_post-hkont,
      END OF it_map,

      BEGIN OF it_skat OCCURS 0,
        saknr TYPE skat-saknr,
        txt50 TYPE skat-txt50,
      END OF it_skat.

* define internal table for BDC
DATA: gt_bdc TYPE TABLE OF bdcdata    WITH HEADER LINE,
      gt_msg TYPE TABLE OF bdcmsgcoll WITH HEADER LINE.

INITIALIZATION.
  CLEAR: it_map, it_map[].

  it_map-class = 'FG DOM'.
  it_map-hkont = '0000510007'.
  APPEND it_map.

  it_map-class = 'FG EXP'.
  it_map-hkont = '0000520007'.
  APPEND it_map.

  it_map-class = 'SFG'.
  it_map-hkont = '0000510107'.
  APPEND it_map.

  SORT it_map BY class.

  SELECT saknr txt50 INTO TABLE it_skat
    FROM skat
   FOR ALL ENTRIES IN it_map
   WHERE spras = sy-langu
     AND ktopl = 'HNA1'
     AND saknr = it_map-hkont.

  SORT it_skat BY saknr.

  SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-t01.
  PARAMETERS: p_bukrs TYPE bkpf-bukrs OBLIGATORY DEFAULT 'H201',
              p_gjahr TYPE bkpf-gjahr OBLIGATORY DEFAULT sy-datum(4),
              p_monat TYPE bkpf-monat OBLIGATORY DEFAULT sy-datum+4(2).
  SELECT-OPTIONS: s_matnr FOR it_duty_dist-matnr.
  PARAMETERS p_mode DEFAULT 'N' NO-DISPLAY.
  SELECTION-SCREEN END OF BLOCK blk1.

AT SELECTION-SCREEN.
  CONCATENATE p_gjahr p_monat '01' INTO g_frstday.

  CALL FUNCTION 'DATE_GET_MONTH_LASTDAY'
    EXPORTING
      i_date = g_frstday
    IMPORTING
      e_date = g_lastday.

START-OF-SELECTION.

  SELECT * INTO TABLE it_duty_post
    FROM ztco_duty_post
   WHERE bukrs = p_bukrs
     AND gjahr = p_gjahr
     AND monat = p_monat
     AND matnr IN s_matnr.

  IF it_duty_post[] IS INITIAL.
    PERFORM populate_data.
  ELSE.
    READ TABLE it_duty_post INDEX 1.
    IF it_duty_post-belnr IS INITIAL.
      CLEAR: it_duty_post, it_duty_post[].
      PERFORM populate_data.
    ELSE.
      g_posted = 'X'.
    ENDIF.
  ENDIF.

  IF it_duty_post[] IS INITIAL.
    MESSAGE 'No Sold Material Found' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  SORT it_duty_post BY bukrs gjahr monat hkont matnr.
  LOOP AT it_duty_post.
    MOVE-CORRESPONDING it_duty_post TO it_data.

    READ TABLE it_skat WITH KEY saknr = it_duty_post-hkont.
    IF sy-subrc = 0.
      it_data-txt50 = it_skat-txt50.
    ENDIF.

    APPEND it_data.
  ENDLOOP.

* Display the result
  PERFORM display_data.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       Display the result
*----------------------------------------------------------------------*
FORM display_data.
  DATA: l_repid TYPE sy-repid.

  l_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = l_repid
      i_internal_tabname = 'IT_DATA'
      i_inclname         = l_repid
    CHANGING
      ct_fieldcat        = gt_fieldcat.

  PERFORM change_cat USING 'BELNR' 'KEY' ' '.
  PERFORM change_cat USING 'WRBTR' 'DO_SUM' 'X'.

  PERFORM set_sorting USING: 'HKONT' 'X',
                             'TXT50' ' ',
                             'MATNR' ' '.

  gs_layout-colwidth_optimize = 'X'.
  gs_layout-zebra = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = l_repid
      i_callback_pf_status_set = 'SET_MAIN'
      i_callback_user_command  = 'USER_COMMAND'
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat
      it_sort                  = gt_sort
    TABLES
      t_outtab                 = it_data
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " DISPLAY_DATA

*&---------------------------------------------------------------------*
*&      Form  CHANGE_CAT
*&---------------------------------------------------------------------*
*       Change field catalog
*----------------------------------------------------------------------*
*      -->P_FIELDNAME   Field Name
*      -->P_PROPERTY    Property
*      -->P_VALUE       Value
*----------------------------------------------------------------------*
FORM change_cat  USING    p_fieldname
                          p_property
                          p_value.

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

*&---------------------------------------------------------------------*
*&      Form  SET_SORTING
*&---------------------------------------------------------------------*
*       Set report sorting
*----------------------------------------------------------------------*
*      -->P_FIELDNAME  Fieldname
*----------------------------------------------------------------------*
FORM set_sorting USING p_fieldname p_subtot.
  STATICS: l_spos(2) TYPE n.
  l_spos = l_spos + 1.

  wa_sort-spos = l_spos.
  wa_sort-fieldname = p_fieldname.
  wa_sort-up = 'X'.
  wa_sort-subtot = p_subtot.
  APPEND wa_sort TO gt_sort.
ENDFORM.                    " SET_SORTING

*&---------------------------------------------------------------------*
*&      Form  SET_MAIN
*&---------------------------------------------------------------------*
*       Set Main Menu
*----------------------------------------------------------------------*
FORM set_main USING rt_extab TYPE slis_t_extab.             "#EC CALLED
  CASE g_posted.
    WHEN 'X'.
      APPEND '&POST' TO rt_extab.
    WHEN OTHERS.
      APPEND '&REVS' TO rt_extab.
  ENDCASE.
  SET PF-STATUS 'MAIN' EXCLUDING rt_extab.
ENDFORM.                    "SET_MAIN

*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       User Command for Main Menu
*----------------------------------------------------------------------*
*      -->R_UCOMM      TCode
*      -->RS_SELFIELD  Status
*----------------------------------------------------------------------*
FORM user_command USING r_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.     "#EC CALLED

  CASE r_ucomm.
    WHEN '&POST'.

      IF NOT s_matnr[] IS INITIAL.
        MESSAGE 'Re-run program for entire Sold Materials'
           TYPE 'I'.
        EXIT.
      ENDIF.

      PERFORM post_data.
      rs_selfield-refresh = 'X'.

    WHEN '&REVS'.
      PERFORM reverse_data.
      rs_selfield-refresh = 'X'.

    WHEN '&IC1'.
      IF ( rs_selfield-fieldname = 'BELNR' OR
           rs_selfield-fieldname = 'BELNS' ) AND
           NOT rs_selfield-value IS INITIAL.

        SET PARAMETER ID: 'BLN' FIELD  rs_selfield-value,
                          'BUK' FIELD p_bukrs,
                          'GJR' FIELD p_gjahr.

        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
  ENDCASE.
ENDFORM.                    "user_command

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
*&      Form  POPULATE_DATA
*&---------------------------------------------------------------------*
*       Populate internal table for data to be posted
*----------------------------------------------------------------------*
FORM populate_data .
  SELECT * INTO TABLE it_duty_dist
    FROM ztco_duty_dist
   WHERE bukrs = p_bukrs
     AND gjahr = p_gjahr
     AND monat = p_monat
     AND matnr IN s_matnr.

  LOOP AT it_duty_dist.
    MOVE-CORRESPONDING it_duty_dist TO it_duty_post.

    READ TABLE it_map WITH KEY class = it_duty_dist-class
                           BINARY SEARCH.
    IF sy-subrc = 0.
      it_duty_post-hkont = it_map-hkont.
    ENDIF.

    APPEND it_duty_post.
  ENDLOOP.
ENDFORM.                    " POPULATE_DATA
*&---------------------------------------------------------------------*
*&      Form  POST_DATA
*&---------------------------------------------------------------------*
*       Post Data
*----------------------------------------------------------------------*
FORM post_data .
  DATA: l_first(1) TYPE c,
        l_postdate(10) TYPE c,
        l_amount(16) TYPE c,
        l_subrc TYPE sy-subrc,
        l_lines TYPE i.

  data: l_NEWBS like RF05A-NEWBS.

  CHECK g_posted IS INITIAL.

  CLEAR: gt_bdc, gt_bdc[], gt_msg, gt_msg[].
  WRITE g_lastday TO l_postdate.

  LOOP AT it_duty_post.
    CHECK it_duty_post-wrbtr <> 0.
    CLEAR: l_NEWBS.
** Furong on 12/20/12
    if it_duty_post-wrbtr >= 0.
      case it_duty_post-hkont.
        when '0000510007' or '0000520007' or '0000510107'.
          l_NEWBS = '40'.
        when '0000532160'.
          l_NEWBS = '50'.
        when  others.

      endcase.
    else.
      it_duty_post-wrbtR = it_duty_post-wrbtR * -1.
      case it_duty_post-hkont.
        when '0000510007' or '0000520007' or '0000510107'.
          l_NEWBS = '50'.
        when '0000532160'.
          l_NEWBS = '40'.
        when others.

      endcase.
      l_NEWBS = '50'.
    endif.
** End on 12/20/12

    WRITE it_duty_post-wrbtr TO l_amount CURRENCY 'USD'.

    AT FIRST.
      l_first = 'X'.
    ENDAT.

    IF NOT l_first IS INITIAL.
      CLEAR l_first.

      PERFORM dynpro USING:
          'X' 'SAPMF05A'        '0100',
          ' ' 'BDC_OKCODE'      '/00',
          ' ' 'BKPF-BLDAT'      l_postdate,
          ' ' 'BKPF-BLART'      'SA',
          ' ' 'BKPF-BUKRS'      p_bukrs,
          ' ' 'BKPF-BUDAT'      l_postdate,
          ' ' 'BKPF-MONAT'      p_monat,
          ' ' 'BKPF-WAERS'      'USD',
          ' ' 'BKPF-BKTXT'      'Duty Var.Posting to CO-PA',
          ' ' 'RF05A-NEWBS'      l_NEWBS,    "'40',
          ' ' 'RF05A-NEWKO'      it_duty_post-hkont.
    ELSE.
      PERFORM dynpro USING:
          'X' 'SAPMF05A'        '0330',
          ' ' 'BDC_OKCODE'      '/00',
          ' ' 'RF05A-NEWBS'      l_NEWBS,   " '40',
          ' ' 'RF05A-NEWKO'      it_duty_post-hkont.

    ENDIF.

    PERFORM dynpro USING:
        'X' 'SAPMF05A'        '0300',
        ' ' 'BDC_OKCODE'      '=ZK',
        ' ' 'BSEG-WRBTR'      l_amount.

    PERFORM dynpro USING:
        'X' 'SAPLKACB'        '0002',
        ' ' 'BDC_OKCODE'      '=COBL_XERGO',
        ' ' 'DKACB-XERGO'     'X'.

    PERFORM dynpro USING:
        'X' 'SAPLKEAK'        '0300',
        ' ' 'BDC_OKCODE'      '=WEIT',
        ' ' 'RKEAK-FIELD(02)' it_duty_post-matnr.

    PERFORM dynpro USING:
        'X' 'SAPLKACB'        '0002',
        ' ' 'BDC_OKCODE'      '=ENTE'.

    AT LAST.
      PERFORM dynpro USING:
          'X' 'SAPMF05A'        '0330',
          ' ' 'BDC_OKCODE'      '/00',
          ' ' 'RF05A-NEWBS'     '50',
          ' ' 'RF05A-NEWKO'     '0000532160'.

      PERFORM dynpro USING:
          'X' 'SAPMF05A'        '0300',
          ' ' 'BDC_OKCODE'      '=BU',
          ' ' 'BSEG-WRBTR'      '*'.

      PERFORM dynpro USING:
          'X' 'SAPLKACB'        '0002',
          ' ' 'BDC_OKCODE'      '=ENTE'.

    ENDAT.
  ENDLOOP.

  CALL TRANSACTION 'F-02'   USING         gt_bdc
                            MODE p_mode
                            UPDATE 'S'
                            MESSAGES INTO gt_msg.
  l_subrc = sy-subrc.

  it_duty_post-cpudt = sy-datum.
  it_duty_post-cputm = sy-uzeit.
  it_duty_post-usnam = sy-uname.

  DESCRIBE TABLE gt_msg LINES l_lines.
  READ TABLE gt_msg INDEX l_lines.

  MESSAGE ID gt_msg-msgid TYPE gt_msg-msgtyp
   NUMBER gt_msg-msgnr
     WITH gt_msg-msgv1 gt_msg-msgv2 gt_msg-msgv3 gt_msg-msgv4
     INTO it_duty_post-messg.

  IF l_subrc = 0.
    it_duty_post-belnr = gt_msg-msgv1.
    CLEAR it_duty_post-belns.
  ENDIF.

  MODIFY it_duty_post
  TRANSPORTING belnr belns cpudt cputm usnam messg
   WHERE bukrs <> ' '.

  DELETE FROM ztco_duty_post WHERE bukrs = p_bukrs
                               AND gjahr = p_gjahr
                               AND monat = p_monat.

  INSERT ztco_duty_post FROM TABLE it_duty_post.

  IF sy-subrc = 0.
    COMMIT WORK.
    g_posted = 'X'.
    CLEAR: it_data, it_data[].
    LOOP AT it_duty_post.
      MOVE-CORRESPONDING it_duty_post TO it_data.
      APPEND it_data. CLEAR it_data.
    ENDLOOP.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " POST_DATA
*&---------------------------------------------------------------------*
*&      Form  REVERSE_DATA
*&---------------------------------------------------------------------*
*       Reverse Posting
*----------------------------------------------------------------------*
FORM reverse_data.
  DATA: l_subrc TYPE sy-subrc,
        l_lines TYPE i,
        l_belnr TYPE bkpf-belnr.

  CHECK NOT g_posted IS INITIAL.

  CLEAR: gt_bdc, gt_bdc[], gt_msg, gt_msg[].

  READ TABLE it_duty_post INDEX 1.
  l_belnr = it_duty_post-belnr.

  PERFORM dynpro USING:
      'X' 'SAPMF05A'        '0105',
      ' ' 'BDC_OKCODE'      '=BU',
      ' ' 'RF05A-BELNS'     l_belnr,
      ' ' 'BKPF-BUKRS'      p_bukrs,
      ' ' 'RF05A-GJAHS'     p_gjahr,
      ' ' 'UF05A-STGRD'     '01'.

  CALL TRANSACTION 'FB08'   USING         gt_bdc
                            MODE p_mode
                            UPDATE 'S'
                            MESSAGES INTO gt_msg.
  l_subrc = sy-subrc.

  it_duty_post-cpudt = sy-datum.
  it_duty_post-cputm = sy-uzeit.
  it_duty_post-usnam = sy-uname.

  DESCRIBE TABLE gt_msg LINES l_lines.
  READ TABLE gt_msg INDEX l_lines.

  MESSAGE ID gt_msg-msgid TYPE gt_msg-msgtyp
   NUMBER gt_msg-msgnr
     WITH gt_msg-msgv1 gt_msg-msgv2 gt_msg-msgv3 gt_msg-msgv4
     INTO it_duty_post-messg.

  IF l_subrc = 0.
    it_duty_post-belns = gt_msg-msgv1.
    CLEAR it_duty_post-belnr.
  ENDIF.

  MODIFY it_duty_post
  TRANSPORTING belnr belns cpudt cputm usnam messg
   WHERE bukrs <> ' '.

  DELETE FROM ztco_duty_post WHERE bukrs = p_bukrs
                               AND gjahr = p_gjahr
                               AND monat = p_monat.

  INSERT ztco_duty_post FROM TABLE it_duty_post.

  IF sy-subrc = 0.
    COMMIT WORK.
    CLEAR g_posted.
    CLEAR: it_data, it_data[].
    LOOP AT it_duty_post.
      MOVE-CORRESPONDING it_duty_post TO it_data.
      APPEND it_data. CLEAR it_data.
    ENDLOOP.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " REVERSE_DATA

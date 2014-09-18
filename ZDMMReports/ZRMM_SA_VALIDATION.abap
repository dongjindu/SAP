************************************************************************
* Program Name      : ZRMM_SA_VALIDATION
* Creation Date     : 08/08/2006
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT zrmm_sa_validation NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmmm.
TABLES: ekko.

TYPE-POOLS slis.

DATA: BEGIN OF it_tab OCCURS 0,
      ebeln LIKE ekko-ebeln,
      ebelp LIKE ekpo-ebelp,
      lifnr LIKE lfa1-lifnr,
      kdate LIKE ekko-kdate,
      matnr LIKE mara-matnr,
      werks LIKE ekpo-werks,
      lgort LIKE ekpo-lgort,
      ktmng LIKE ekpo-ktmng,
      bstae LIKE ekpo-bstae,
      uebtk LIKE ekpo-uebtk,
      xersy LIKE ekpo-xersy,
      wepos LIKE ekpo-wepos,
      repos LIKE ekpo-repos,
      webre LIKE ekpo-webre,
      etfz1 LIKE ekpo-etfz1,
      abueb LIKE ekpo-abueb,
      fabkz LIKE ekpo-fabkz,
*      febel LIKE eord-febel,
*      notkz LIKE eord-notkz,
*      autet LIKE eord-autet,
      mess(40),
      END OF it_tab.

DATA: BEGIN OF it_SA_DUP OCCURS 0,
      ebeln LIKE ekko-ebeln,
      ebelp LIKE ekpo-ebelp,
      matnr LIKE mara-matnr,
      werks LIKE ekpo-werks,
      END OF it_SA_DUP.

DATA: BEGIN OF it_output OCCURS 0,
      ebeln LIKE ekko-ebeln,
*      ebelp LIKE ekpo-ebelp,
      lifnr LIKE lfa1-lifnr,
      kdate LIKE ekko-kdate,
      matnr LIKE mara-matnr,
      werks LIKE ekpo-werks,
      lgort LIKE ekpo-lgort,
      ktmng LIKE ekpo-ktmng,
      bstae LIKE ekpo-bstae,
      uebtk LIKE ekpo-uebtk,
      xersy LIKE ekpo-xersy,
      wepos LIKE ekpo-wepos,
      repos LIKE ekpo-repos,
      webre LIKE ekpo-webre,
      etfz1 LIKE ekpo-etfz1,
      abueb LIKE ekpo-abueb,
      fabkz LIKE ekpo-fabkz,
      febel LIKE eord-febel,
      notkz LIKE eord-notkz,
      autet LIKE eord-autet,
      mess(40),
      ct TYPE lvc_t_scol,
      END OF it_output.

DATA: BEGIN OF it_eord OCCURS 0,
      ebeln LIKE ekko-ebeln,
      ebelp LIKE ekpo-ebelp,
      lifnr LIKE lfa1-lifnr,
      matnr LIKE mara-matnr,
      werks LIKE ekpo-werks,
      febel LIKE eord-febel,
      notkz LIKE eord-notkz,
      autet LIKE eord-autet,
      END OF it_eord.

DATA: it_color TYPE lvc_t_scol,
       wa_color LIKE LINE OF it_color.

DATA: ok_code LIKE sy-ucomm,
      w_repid LIKE sy-repid,
      w_cnt TYPE i.

DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldcat_fi  TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldcat_co  TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE,
       it_fieldcat_det TYPE lvc_t_fcat WITH HEADER LINE. "/Detail

DATA : wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
*      it_color type LVC_T_SCOL,
*      wa_color like line of it_color,
      w_fieldname    LIKE LINE OF it_fieldcat.

DATA: wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
      wa_variant TYPE disvariant.      "for parameter IS_VARIANT

DATA: wa_custom_control TYPE        scrfname VALUE 'ALV_CONTAINER',
      alv_grid          TYPE REF TO cl_gui_alv_grid,
      grid_container    TYPE REF TO cl_gui_custom_container.

DATA: it_mail TYPE STANDARD TABLE OF solisti1 INITIAL SIZE 0
                  WITH HEADER LINE.

DATA: w_ebeln LIKE ekpo-ebeln.

CONSTANTS: c_jis LIKE ekko-bsart VALUE 'JIS',
           c_jit LIKE ekko-bsart VALUE 'JIT'.

SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.

SELECT-OPTIONS : s_lifnr FOR ekko-lifnr,   " OBLIGATORY,
                 s_ebeln FOR ekko-ebeln.

PARAMETERS: p_bstyp LIKE ekko-bstyp DEFAULT 'L',
            p_bsart LIKE ekko-bsart  OBLIGATORY.
SELECTION-SCREEN END OF BLOCK block1.

SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-002.
PARAMETERS: p_send AS CHECKBOX DEFAULT 'X' USER-COMMAND ucom.
PARAMETERS: p_email(40) DEFAULT 'SASLVAL' MODIF ID md3.
SELECTION-SCREEN END OF BLOCK block2.


SELECTION-SCREEN BEGIN OF BLOCK block3 WITH FRAME TITLE text-003.
PARAMETERS: p_unlmt AS CHECKBOX DEFAULT 'X' USER-COMMAND ucom.
PARAMETERS: p_firm as checkbox default 'X'.
SELECTION-SCREEN END OF BLOCK block3.

START-OF-SELECTION.
  PERFORM get_data.
  IF it_tab[] IS INITIAL.
    MESSAGE i999 WITH text-t01.
    EXIT.
  ENDIF.
  PERFORM process_data.
  IF p_send = 'X' and ( not it_output[] is initial ).
    PERFORM send_email.
  ENDIF.

  if not it_output[] is initial.
    PERFORM display_data.
  endif.
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  DATA: l_bsarT LIKE P_bsarT.
  IF p_bsart = c_jit.
    w_ebeln = '460%'.
    l_bsart = 'JIS'.
  ELSE.
    w_ebeln = '465%'.
    l_bsart = 'JIT'.
  ENDIF.

  SELECT
        a~ebeln b~ebelp a~lifnr a~kdate b~matnr b~werks lgort
        ktmng bstae uebtk xersy wepos repos webre etfz1 abueb fabkz
        INTO CORRESPONDING FIELDS OF TABLE it_tab
                 FROM ekko AS a INNER JOIN ekpo AS b
                   ON a~mandt EQ b~mandt
                  AND a~ebeln EQ b~ebeln
                 WHERE a~ebeln IN s_ebeln
                  and a~bstyp EQ p_bstyp
                  AND bsart = p_bsart
                  AND a~loekz EQ space
                  AND a~lifnr IN s_lifnr
                  AND b~loekz EQ space
                  AND b~elikz EQ space.

  SELECT ebeln ebelp lifnr matnr werks febel notkz autet
     INTO CORRESPONDING FIELDS OF TABLE it_eord
     FROM eord
     FOR ALL ENTRIES IN it_tab
    WHERE matnr EQ it_tab-matnr
      AND werks EQ it_tab-werks
      AND ebeln EQ it_tab-ebeln
      AND ebelp EQ it_tab-ebelp
      AND lifnr EQ it_tab-lifnr
      AND ebeln LIKE w_ebeln.

  SELECT
        a~ebeln b~ebelp b~matnr b~werks
          INTO TABLE it_sa_dup
                 FROM ekko AS a INNER JOIN ekpo AS b
                   ON a~mandt EQ b~mandt
                  AND a~ebeln EQ b~ebeln
                 WHERE a~bstyp EQ p_bstyp
                  AND bsart = L_bsart
                  AND a~loekz EQ space
                  AND b~loekz EQ space
                  AND b~elikz EQ space.
  SORT it_sa_dup BY MATNR.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data.
  DATA: l_matnr LIKE ekpo-matnr,
        l_flag(1).

  CONSTANTS: l_max_date LIKE sy-datum VALUE '29991231'.

  SORT it_tab BY matnr lifnr ebeln.
  LOOP AT it_tab.
    IF l_matnr = it_tab-matnr.
      it_output-mess = 'Multi SA'.
      l_flag = 'X'.
      PERFORM set_color_error USING 'MESS'.
    ENDIF.
    READ TABLE it_sa_dup WITH KEY MATNR = it_tab-matnr.
    IF SY-SUBRC = 0.
      it_output-mess = 'Multi SA'.
      l_flag = 'X'.
      PERFORM set_color_error USING 'MESS'.
    ENDIF.
    l_matnr = it_tab-matnr.
    it_output-matnr = it_tab-matnr.
    it_output-lifnr = it_tab-lifnr.
    it_output-ebeln = it_tab-ebeln.

    IF it_tab-kdate < l_max_date.
      it_output-kdate = it_tab-kdate.
      l_flag = 'X'.
      PERFORM set_color_error USING 'KDATE'.
    ENDIF.

*    IF it_tab-werks <> 'P001'.
*      it_output-werks = it_tab-werks.
*      l_flag = 'X'.
*      PERFORM set_color_error USING 'WERKS'.
*    ENDIF.

    IF it_tab-ktmng < 999999.
      it_output-ktmng = it_tab-ktmng.
      l_flag = 'X'.
      PERFORM set_color_error USING 'KTMNG'.
    ENDIF.

    IF p_bsart = c_jis AND it_tab-lgort <> 'P500'.
      it_output-lgort = it_tab-lgort.
      l_flag = 'X'.
      PERFORM set_color_error USING 'LGORT'.
    ENDIF.

    IF p_bsart = c_jit AND it_tab-lgort = ' '.
      it_output-lgort = it_tab-lgort.
      l_flag = 'X'.
      PERFORM set_color_error USING 'LGORT'.
    ENDIF.

    IF p_bsart = c_jis AND it_tab-bstae <> ' '.
      it_output-bstae = it_tab-bstae.
      l_flag = 'X'.
      PERFORM set_color_error USING 'BSTAE'.
    ENDIF.

    IF p_bsart = c_jit AND it_tab-bstae <> 'Z001'.
      it_output-bstae = it_tab-bstae.
      l_flag = 'X'.
      PERFORM set_color_error USING 'BSTAE'.
    ENDIF.

    if p_unlmt = 'X' AND p_bsart = c_jis AND it_tab-uebtk <> 'X'.
      it_output-uebtk = it_tab-uebtk.
      l_flag = 'X'.
      PERFORM set_color_error USING 'UEBTK'.
    ENDIF.

    IF p_unlmt = 'X' AND p_bsart = c_jit AND it_tab-uebtk <> ' '.
      it_output-uebtk = it_tab-uebtk.
      l_flag = 'X'.
      PERFORM set_color_error USING 'UEBTK'.
    ENDIF.

    IF it_tab-xersy <> 'X'.
      it_output-xersy = it_tab-xersy.
      l_flag = 'X'.
      PERFORM set_color_error USING 'XERSY'.
    ENDIF.

    IF it_tab-wepos <> 'X'.
      it_output-wepos = it_tab-wepos.
      l_flag = 'X'.
      PERFORM set_color_error USING 'WEPOS'.
    ENDIF.

    IF it_tab-repos <> 'X'.
      it_output-repos = it_tab-repos.
      l_flag = 'X'.
      PERFORM set_color_error USING 'REPOS'.
    ENDIF.

    IF it_tab-webre <> 'X'.
      it_output-webre = it_tab-webre.
      l_flag = 'X'.
      PERFORM set_color_error USING 'WEBRE'.
    ENDIF.

    IF p_FIRM = 'X' AND p_bsart = c_jis AND it_tab-etfz1 = '0'.
      it_output-etfz1 = it_tab-etfz1.
      l_flag = 'X'.
      PERFORM set_color_error USING 'ETFZ1'.
    ENDIF.

    IF p_FIRM = 'X' AND p_bsart = c_jit.
      IF NOT it_tab-etfz1 BETWEEN '1' AND '5'.
        it_output-etfz1 = it_tab-etfz1.
        l_flag = 'X'.
        PERFORM set_color_error USING 'ETFZ1'.
      ENDIF.
    ENDIF.

    IF NOT it_tab-abueb BETWEEN 'Z001' AND 'Z005'.
      it_output-abueb = it_tab-abueb.
      l_flag = 'X'.
      PERFORM set_color_error USING 'ABUEB'.
    ENDIF.

    IF p_bsart = c_jis AND it_tab-fabkz <> ' '.
      it_output-fabkz = it_tab-fabkz.
      l_flag = 'X'.
      PERFORM set_color_error USING 'FABKZ'.
    ENDIF.

    IF p_bsart = c_jit AND it_tab-fabkz <> '1'.
      it_output-fabkz = it_tab-fabkz.
      l_flag = 'X'.
      PERFORM set_color_error USING 'FABKZ'.
    ENDIF.

    READ TABLE it_eord WITH KEY matnr = it_tab-matnr
                                werks = it_tab-werks
                                ebeln = it_tab-ebeln
                                ebelp = it_tab-ebelp
                                lifnr = it_tab-lifnr.
    IF sy-subrc = 0.
      IF it_eord-febel <> 'X'.
        it_output-febel = it_eord-febel.
        l_flag = 'X'.
        PERFORM set_color_error USING 'FEBEL'.
      ENDIF.

      IF it_eord-notkz <> ' '.
        it_output-notkz = it_eord-notkz.
        l_flag = 'X'.
        PERFORM set_color_error USING 'NOTKZ'.
      ENDIF.

      IF it_eord-autet <> '2'.
        it_output-autet = it_eord-autet.
        l_flag = 'X'.
        PERFORM set_color_error USING 'AUTET'.
      ENDIF.
    ELSE.
      IF it_output-mess IS INITIAL.
        it_output-mess = 'Source List Missing'.
      ELSE.
        CONCATENATE it_output-mess ' & Source List Missing'
        INTO it_output-mess.
      ENDIF.
      l_flag = 'X'.
      PERFORM set_color_error USING 'MESS'.
    ENDIF.
    IF l_flag = 'X'.
      it_output-ct = it_color.
      APPEND it_output.
      CLEAR: l_flag, it_color[].
    ENDIF.
    CLEAR: it_output, it_tab, l_flag, it_eord.
  ENDLOOP.
ENDFORM.                    " process_data

*---------------------------------------------------------------------*
*       FORM display_data                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM display_data.
  CALL SCREEN 200.
ENDFORM.
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
    PERFORM create_container_n_object.
    PERFORM set_attributes_alv_grid.
    PERFORM build_sortcat_display.
    PERFORM build_field_catalog USING 'IT_OUTPUT'.
    PERFORM assign_itab_to_alv.
  ELSE.
    CALL METHOD alv_grid->refresh_table_display.
  ENDIF.

ENDMODULE.                 " DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object
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
*---------------------------------------------------------------------*
*       FORM set_attributes_alv_grid                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM set_attributes_alv_grid.
  DATA : lw_s_dragdrop TYPE lvc_s_dd01. "/ Drag&Drop control settings

  CLEAR : wa_is_layout, wa_variant.

*//-- Set Layout Structure
  wa_is_layout-edit       = ' '.      "/Edit Mode Enable
  wa_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  wa_is_layout-language   = sy-langu. "/Language Key
  wa_is_layout-cwidth_opt = 'X'.   "/optimizes the column width
  wa_is_layout-info_fname = 'IF'.
  wa_is_layout-ctab_fname = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging
  wa_is_layout-grid_title = 'SA Validation'.
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
                                  ' ' 'COLTEXT'     'SA No',
                                  'E' 'OUTPUTLEN'   '10',


                                  'S' 'LIFNR'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Vendor',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'MATNR'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Material',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'KDATE'     ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Valid Date',
                                  'E' 'OUTPUTLEN'   '10',

*                                  'S' 'WERKS'       ' ',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'COLTEXT'     'Plant',
*                                  'E' 'OUTPUTLEN'   '4',
*
                                  'S' 'LGORT'         ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'SLOC',
                                  'E' 'OUTPUTLEN'   '4',

                                  'S' 'KTMNG'        ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Target QTY',
                                  ' ' 'NO_ZERO'     'X',
                                  ' ' 'DECIMALS_O'  '0',
                                  'E' 'OUTPUTLEN'   '13',

                                  'S' 'BSTAE'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Confirmation',
                                  'E' 'OUTPUTLEN'   '4',

                                  'S' 'UEBTK'         ' ',
                                  ' ' 'COLTEXT'     'Unlimited',
                                  'E' 'OUTPUTLEN'   '1',

                                  'S' 'XERSY'       ' ',
                                  ' ' 'COLTEXT'     'ERS',
                                  'E' 'OUTPUTLEN'   '1',

                                  'S' 'WEPOS'       ' ',
                                  ' ' 'COLTEXT'     'GR',
                                  'E' 'OUTPUTLEN'   '1',

                                  'S' 'REPOS'       ' ',
                                  ' ' 'COLTEXT'     'IR',
                                  'E' 'OUTPUTLEN'   '1',

                                  'S' 'WEBRE'       ' ',
                                  ' ' 'COLTEXT'     'GR-IV',
                                  'E' 'OUTPUTLEN'   '1',

                                  'S' 'ETFZ1'       ' ',
                                  ' ' 'COLTEXT'     'Firm',
                                  ' ' 'NO_ZERO'     'X',
                                  'E' 'OUTPUTLEN'   '1',

                                  'S' 'ABUEB'       ' ',
                                  ' ' 'COLTEXT'     'Profile',
                                  'E' 'OUTPUTLEN'   '4',

                                  'S' 'FABKZ'       ' ',
                                  ' ' 'COLTEXT'     'JIT',
                                  'E' 'OUTPUTLEN'   '1',

                                  'S' 'FEBEL'       ' ',
                                  ' ' 'COLTEXT'     'Fixd Agreement',
                                  'E' 'OUTPUTLEN'   '1',

                                  'S' 'NOTKZ'       ' ',
                                  ' ' 'COLTEXT'     'Blocked',
                                  'E' 'OUTPUTLEN'   '1',

                                  'S' 'AUTET'       ' ',
                                  ' ' 'COLTEXT'     'Source List Usage',
                                  'E' 'OUTPUTLEN'   '1',

                                  'S' 'MESS'       ' ',
                                  ' ' 'COLTEXT'     'Error Message',
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
*&---------------------------------------------------------------------*
*&      Form  set_color_error
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_color_error USING p_fname.
  wa_color-color-col = 3.
  wa_color-color-int = 1.
  wa_color-fname = p_fname.
  APPEND wa_color TO it_color.
  CLEAR: wa_color.
ENDFORM.                    " set_color_error
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
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  send_email
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_email.
  DATA: l_subject(40) TYPE c VALUE 'SA Validation'.

  DATA:   it_packing_list LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
          it_contents LIKE solisti1 OCCURS 0 WITH HEADER LINE,
          it_receivers LIKE somlreci1 OCCURS 0 WITH HEADER LINE,
          it_attachment LIKE solisti1 OCCURS 0 WITH HEADER LINE,
          gd_cnt TYPE i,
          gd_sent_all(1) TYPE c,
          gd_doc_data LIKE sodocchgi1,
          gd_error TYPE sy-subrc,
          L_LINES TYPE I.

  PERFORM populate_data_for_output.

  DESCRIBE TABLE it_mail LINES L_LINES.

* Populate the subject/generic message attributes
  gd_doc_data-obj_langu = sy-langu.
  gd_doc_data-obj_name  = sy-repid.
  gd_doc_data-obj_descr = l_subject.
  gd_doc_data-sensitivty = 'F'.
  gd_doc_data-no_change = 'X'.
  gd_doc_data-doc_size = L_LINES * 255
                         + strlen( it_MAIL ).
*  gd_doc_data-PROC_TYPE = 'R'.
* Describe the body of the message

  CLEAR it_packing_list.
  REFRESH it_packing_list.
*  it_packing_list-transf_bin = ' '.
  it_packing_list-head_start = 1.
  it_packing_list-head_num = 0.
  it_packing_list-body_start = 1.
  it_packing_list-doc_type = 'RAW'.
  it_packing_list-body_num = L_LINES * 255.
  APPEND it_packing_list.

  CLEAR it_packing_list.
  it_packing_list-transf_bin = 'X'.
  it_packing_list-head_start = 1.
  it_packing_list-head_num = 0.
  it_packing_list-body_start = 1.
  it_packing_list-body_num = L_LINES.
  it_packing_list-OBJ_name = 'SA'.
  it_packing_list-OBJ_DESCR = 'Attached Docuemnt'.
  it_packing_list-doc_type = 'RAW'.

  APPEND it_packing_list.

* Add the recipients email address
  CLEAR it_receivers.
  REFRESH it_receivers.
  it_receivers-receiver = p_email.
*  it_receivers-rec_type = 'U'.  " internet email
  it_receivers-rec_type = 'C'.
  it_receivers-com_type = 'INT'.
  it_receivers-notif_del = 'X'.
  it_receivers-notif_ndel = 'X'.
  APPEND it_receivers.

* Call the FM to post the message to SAPMAIL
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = gd_doc_data
*            PUT_IN_OUTBOX              = 'X'
            COMMIT_WORK                = 'X'
    IMPORTING
      sent_to_all                = gd_sent_all
    TABLES
      packing_list               = it_packing_list
      contents_bin               = IT_MAIL
*     contents_txt               = it_mail
      receivers                  = it_receivers
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

* Store function module return code

  IF sy-subrc = 0.
    SUBMIT rsconn01           "Start the sending process
          WITH mode   = 'INT'
          WITH output = ' '
          AND RETURN.

  ENDIF.

  gd_error = sy-subrc.

ENDFORM.                    " send_email

*---------------------------------------------------------------------*
*       FORM populate_data_for_output                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM populate_data_for_output.
  DATA: l_mail(255),
        l_message TYPE so_text255,
        l_ktmng(14),
        l_etfz1(1).

  DATA: tab TYPE x VALUE '09'.
  DATA: BEGIN OF lt_output OCCURS 0,
      ebeln(11),
*      ebelp LIKE ekpo-ebelp,
      lifnr(5),
      kdate(10),
      matnr(14),
      werks(6),
      lgort(5),
      ktmng(13),
      bstae(5),
      uebtk(4),
      xersy(4),
      wepos(3),
      repos(3),
      webre(6),
      etfz1(3),
      abueb(4),
      fabkz(4),
      febel(5),
      notkz(5),
      autet(7),
      mess(40),
      END OF lt_output.

  DATA: lw_output LIKE LINE OF lt_output.
  CLEAR: it_mail,it_mail[].

  MOVE: 'SA Number' TO lw_output-ebeln,
         'Vndr' TO lw_output-lifnr,
          'Valid Date' TO lw_output-kdate,
          'Material      ' TO lw_output-matnr,
*         'Plant|' TO lw_output-werks,
          'Stor' TO lw_output-lgort,
         'Target Qty' TO lw_output-ktmng,
          'Conf' TO lw_output-bstae,
          'Ulm' TO lw_output-uebtk,
          'ERS' TO lw_output-xersy,
          'GR' TO lw_output-wepos,
          'IR' TO lw_output-repos,
          'GR-IV' TO lw_output-webre,
          'Fm' TO lw_output-etfz1,
          'Prf' TO lw_output-abueb,
          'JIT' TO lw_output-fabkz,
          'F.SA' TO lw_output-febel,
          'Blkd' TO lw_output-notkz,
          'Source' TO lw_output-autet,
          'Error' TO lw_output-mess.

  CONCATENATE lw_output-ebeln tab lw_output-lifnr tab
              lw_output-kdate tab lw_output-matnr
*                lw_output-werks tab
              lw_output-lgort tab
              INTO l_mail.
  CONCATENATE l_mail lw_output-ktmng tab lw_output-bstae tab
              lw_output-uebtk tab lw_output-xersy tab
              lw_output-wepos tab lw_output-repos tab
                             INTO l_mail.
  CONCATENATE l_mail lw_output-webre tab lw_output-etfz1 tab
              lw_output-abueb tab lw_output-fabkz tab
              lw_output-febel tab lw_output-notkz tab
              lw_output-autet tab lw_output-mess
                              INTO l_mail.
  APPEND l_mail TO it_mail.
  CLEAR: it_mail, l_mail.

*  MOVE: 'SA Number|' TO l_mail+0(11),
*         'Vndr|' TO l_mail+12(5),
*          'Valid Date|' TO l_mail+18(10),
*          'Material          |' TO l_mail+29(18),
*         'Plant|' TO l_mail+48(6),
*          'Stge|' TO l_mail+55(5),
*         'Target Qty  |' TO l_mail+61(13),
*          'Conf|' TO l_mail+75(5),
*          'Ulm|' TO l_mail+81(4),
*          'ERS|' TO l_mail+86(4),
*          'GR|' TO l_mail+91(3),
*          'IR|' TO l_mail+95(3),
*          'GR-IV|' TO l_mail+99(6),
*          'Fm|' TO l_mail+106(3),
*          'Prf|' TO l_mail+110(4),
*          'JIT|' TO l_mail+115(4),
*          'F.SA|' TO l_mail+120(5),
*          'Blkd|' TO l_mail+126(5),
*          'Source|' TO l_mail+132(7),
*          'Error' TO l_mail+140(35).

*  MOVE lw_output TO l_mail.
*  APPEND l_mail TO it_mail.
*  CLEAR: it_mail.
*
  LOOP AT it_output.
    MOVE-CORRESPONDING it_output TO lw_output.

    lw_output-ktmng = it_output-ktmng.
    lw_output-etfz1 = it_output-etfz1.

    CONDENSE lw_output-ktmng.
    CONDENSE lw_output-etfz1.

*    IF it_output-ktmng = 0.
*      lw_output-ktmng = '      '.
*    ENDIF.
*
*    IF it_output-etfz1 = 0.
*      lw_output-etfz1 = ' '.
*    ENDIF.
*
*    MOVE: lw_output-ebeln TO l_mail+0(11),
*          lw_output-lifnr TO l_mail+12(5),
*          lw_output-kdate TO l_mail+18(10),
*          lw_output-matnr TO l_mail+29(18),
*          lw_output-werks TO l_mail+48(6),
*          lw_output-lgort TO l_mail+55(5),
*          lw_output-ktmng TO l_mail+61(13),
*          lw_output-bstae TO l_mail+75(5),
*          lw_output-uebtk TO l_mail+81(4),
*          lw_output-xersy TO l_mail+86(4),
*          lw_output-wepos TO l_mail+91(3),
*          lw_output-repos TO l_mail+95(3),
*          lw_output-webre TO l_mail+99(6),
*          lw_output-etfz1 TO l_mail+106(3),
*          lw_output-abueb TO l_mail+110(4),
*          lw_output-fabkz TO l_mail+115(4),
*          lw_output-febel TO l_mail+120(5),
*          lw_output-notkz TO l_mail+126(5),
*          lw_output-autet TO l_mail+132(7),
*          lw_output-mess TO l_mail+140(35).
*

    CONCATENATE lw_output-ebeln tab lw_output-lifnr tab
                lw_output-kdate tab
                lw_output-matnr tab
*                lw_output-werks tab
                lw_output-lgort tab
                lw_output-ktmng tab
                INTO l_mail.

    CONCATENATE l_mail lw_output-bstae tab
                lw_output-uebtk tab lw_output-xersy tab
                lw_output-wepos tab lw_output-repos tab
                               INTO l_mail.
    CONCATENATE l_mail lw_output-webre tab lw_output-etfz1 tab
                lw_output-abueb tab lw_output-fabkz tab
                lw_output-febel tab lw_output-notkz tab
                lw_output-autet tab lw_output-mess
                                INTO l_mail.

    APPEND l_mail TO it_mail.
    CLEAR: it_mail, l_mail.
  ENDLOOP.
ENDFORM.

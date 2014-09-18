TYPE-POOLS slis.

TYPES: BEGIN OF typ_alv_item.
        INCLUDE STRUCTURE bset.        "Table BSET
TYPES:   ktonr  TYPE rfums_alv-ktonr,  "Account
         name   TYPE rfums_alv-name,   "Business Partner
         LAND1  type lfa1-LAND1,       "Country
         REGIO  type lfa1-REGIO,       "region
         curr_kbetr TYPE bkpf-hwaer,   "currency unit of BSET-KBETR
         xblnr  TYPE bkpf-xblnr,       "Referenc document
         bldat  TYPE bkpf-bldat,       "Document Date
         text1  TYPE ttxjt-text1,      "Jurisdiction
         text2  TYPE t007s-text1,                           "VAT
         mwart  TYPE t007a-mwart,      "Tax type
         waers  TYPE bkpf-waers,       "document currency
         hwaer  TYPE bkpf-hwaer,       "local currency
         hwae2  TYPE bkpf-hwae2,       "local currency 2
         hwae3  TYPE bkpf-hwae3,       "local currency 3
         lwaer  TYPE t005-waers,       "country currency
         jur_sta TYPE bset-txjcd,      "Jurisdiction: State-Level
       END OF typ_alv_item.

DATA: gt_alv_item TYPE TABLE OF typ_alv_item WITH HEADER LINE.


*---------------------------------------------------------------------*
*       FORM print_list                                               *
*---------------------------------------------------------------------*
*       Print table gt_alv_item with the ALV                          *
*---------------------------------------------------------------------*

FORM print_list.

  DATA: lt_fieldcat  TYPE slis_t_fieldcat_alv,  "Field catalog
        lt_alv_event TYPE slis_t_event,"Table of events to perform
        l_repid      TYPE sy-repid,    "Report-Name
        l_variant    TYPE disvariant,  "Display Variant
        l_layout     TYPE slis_layout_alv.  "List layout specifications

  l_repid = sy-repid.

* Create Table lt_alv_event
  PERFORM append_event USING    slis_ev_top_of_page
                                'TOP_OF_PAGE'
                       CHANGING lt_alv_event.

* Create field catalog lt_fieldcat
  PERFORM create_fieldcat CHANGING lt_fieldcat.

* Create layout for ALV
  PERFORM create_layout CHANGING l_layout.

* Define Start-Variant
*  PERFORM get_start_variant CHANGING l_variant.
   l_variant-variant = p_vari.
   l_variant-REPORT  = sy-repid.


* Print tables gt_alv_item.
  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            i_callback_program = l_repid
            is_layout          = l_layout
            it_fieldcat        = lt_fieldcat
            is_variant         = l_variant
            i_save             = 'A'
            it_events          = lt_alv_event
       TABLES
            t_outtab           = gt_alv_item.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM top_of_page                                              *
*---------------------------------------------------------------------*
*       Information output at the top of a page                       *
*---------------------------------------------------------------------*
FORM top_of_page.

* IF header = 'X'.                     "Print special header
    IF gt_alv_item-mwart = 'A'.
      bhdgd-line2 = text-011.
    ELSEIF gt_alv_item-mwart = 'V'.
      bhdgd-line2 = text-012.
    ENDIF.
    bhdgd-line1 = sy-title.
    bhdgd-inifl = '0'.
* ENDIF.
  bhdgd-lines = sy-linsz.
  IF bhdgd-lines > 250.
    bhdgd-lines = 250.
  ENDIF.
  MOVE gt_alv_item-bukrs TO bhdgd-bukrs.
  MOVE gt_alv_item-bukrs TO bhdgd-werte.

  PERFORM new-section(rsbtchh0).
  PERFORM batch-heading(rsbtchh0).
  PERFORM print_heading.

ENDFORM.


*---------------------------------------------------------------------*
*       FORM create_layout                                            *
*---------------------------------------------------------------------*
*  -->  c_layout                                                      *
*---------------------------------------------------------------------*
FORM create_layout CHANGING c_layout TYPE slis_layout_alv.
  c_layout-group_change_edit = 'X'.    "Aufbereitungsoptionen änderbar
ENDFORM.


*---------------------------------------------------------------------*
*       FORM append_event                                             *
*---------------------------------------------------------------------*
*  -->  u_name           Event-name                                   *
*  -->  u_form           Form-name                                    *
*---------------------------------------------------------------------*
*  <--  ct_event         Table of events to perform                   *
*---------------------------------------------------------------------*
FORM append_event USING    u_name   TYPE slis_alv_event-name
                           u_form   TYPE slis_alv_event-form
                  CHANGING ct_event TYPE slis_t_event.

  DATA: l_alv_event TYPE slis_alv_event.

  l_alv_event-name = u_name.
  l_alv_event-form = u_form.
  APPEND l_alv_event TO ct_event.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM create_fieldcat                                          *
*---------------------------------------------------------------------*
*       Create Field catalog with field descriptions                  *
*---------------------------------------------------------------------*
*  -->  ct_fieldcat                                                   *
*---------------------------------------------------------------------*
FORM create_fieldcat CHANGING ct_fieldcat TYPE slis_t_fieldcat_alv.

  DATA: l_repid TYPE sy-repid.         "Report name

  l_repid = sy-repid.

* Append some fields to ct_fieldcat
  PERFORM append_fieldcat TABLES ct_fieldcat USING:
    'KTONR'       'RFUMS_ALV'        space,
    'NAME'        'RFUMS_ALV'        space,
    'LAND1'       'LFA1'             space,
    'REGIO'       'LFA1'             space,
    'BLDAT'       'BKPF'             space,
    'WAERS'       'BKPF'             space,
    'HWAER'       'BKPF'             space,
    'HWAE2'       'BKPF'             space,
    'HWAE3'       'BKPF'             space,
    'XBLNR'       'BKPF'             space,
    'LWAER'       'T005'            'WAERS',
    'CURR_KBETR'  'T005'            'WAERS',
    'MWART'       'T007A'            space,
    'JUR_STA'     'RFUMS_ALV'        space,
    'TEXT1'       'TTXJT'            space,
    'TEXT2'       'T007S'           'TEXT1'.

* Insert the fields of BSET into ct_fieldcat
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name   = l_repid
            i_structure_name = 'BSET'
       CHANGING
            ct_fieldcat      = ct_fieldcat.

* Hide the most fields
  PERFORM modify_fieldcat CHANGING ct_fieldcat.

ENDFORM.


*---------------------------------------------------------------------*
*       FORM append_fieldcat                                          *
*---------------------------------------------------------------------*
*       Append one field discription to tt_fieldcat                   *
*---------------------------------------------------------------------*
*  -->  tt_fieldcat                                                   *
*  -->  u_fieldname                                                   *
*  -->  u_ref_tabname                                                 *
*  -->  u_ref_fieldname                                               *
*---------------------------------------------------------------------*
FORM append_fieldcat
      TABLES
        tt_fieldcat
      USING
        u_fieldname TYPE slis_fieldcat_alv-fieldname
        u_ref_tabname TYPE slis_fieldcat_alv-ref_tabname
        u_ref_fieldname TYPE slis_fieldcat_alv-ref_fieldname.

  DATA: l_fieldcat TYPE slis_fieldcat_alv.        "field string

  l_fieldcat-fieldname      = u_fieldname.
  l_fieldcat-ref_tabname    = u_ref_tabname.
  l_fieldcat-ref_fieldname  = u_ref_fieldname.
  APPEND l_fieldcat TO tt_fieldcat.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM modify_fieldcat                                          *
*---------------------------------------------------------------------*
*       Change tabel ct_fieldcat                                      *
*---------------------------------------------------------------------*
*  -->  ct_fieldcat                                                   *
*---------------------------------------------------------------------*
FORM modify_fieldcat CHANGING ct_fieldcat TYPE slis_t_fieldcat_alv.

  DATA l_fieldcat TYPE slis_fieldcat_alv.        "Field String

  LOOP AT ct_fieldcat INTO l_fieldcat.

    IF (     l_fieldcat-fieldname <> 'BELNR'
         AND l_fieldcat-fieldname <> 'XBLNR'
         AND l_fieldcat-fieldname <> 'BLDAT'
         AND l_fieldcat-fieldname <> 'HWBAS'
         AND l_fieldcat-fieldname <> 'HWSTE' ).
      l_fieldcat-no_out = 'X'.         "Do not display
    ENDIF.

    IF ( NOT l_fieldcat-key IS INITIAL ).
      CLEAR l_fieldcat-key.            "No key field
    ENDIF.

    CASE l_fieldcat-fieldname.         "Define Currencies
      WHEN 'KBETR'.
        l_fieldcat-cfieldname = 'CURR_KBETR'.
      WHEN 'HWBAS'.
        l_fieldcat-cfieldname = 'HWAER'.
      WHEN 'HWSTE'.
        l_fieldcat-cfieldname = 'HWAER'.
      WHEN 'FWBAS'.
        l_fieldcat-cfieldname = 'WAERS'.
      WHEN 'FWSTE'.
        l_fieldcat-cfieldname = 'WAERS'.
      WHEN 'H2STE'.
        l_fieldcat-cfieldname = 'HWAE2'.
      WHEN 'H2BAS'.
        l_fieldcat-cfieldname = 'HWAE2'.
      WHEN 'H3STE'.
        l_fieldcat-cfieldname = 'HWAE3'.
      WHEN 'H3BAS'.
        l_fieldcat-cfieldname = 'HWAE3'.
      WHEN 'LWSTE'.
        l_fieldcat-cfieldname = 'LWAER'.
      WHEN 'LWBAS'.
        l_fieldcat-cfieldname = 'LWAER'.
      WHEN 'CURR_KBETR'.
        l_fieldcat-tech = 'X'.
    ENDCASE.

    IF l_fieldcat-fieldname = 'DUEFL'. "We don´t need 'DUEFL'
      DELETE ct_fieldcat.
    ELSE.
      MODIFY ct_fieldcat FROM l_fieldcat.
    ENDIF.

  ENDLOOP.
ENDFORM.



*---------------------------------------------------------------------*
*       FORM append_data                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM append_data.

*  xtxjt = space.
*  xtxjt-spras = sy-langu.
*  xtxjt-kalsm = x005-kalsm.
*  xtxjt-txjcd = xbset-txjcd.
*  READ TABLE xtxjt.

  READ TABLE xtxjt WITH KEY
        spras = sy-langu
        kalsm = x005-kalsm
        txjcd = xbset-txjcd.

  CLEAR gt_alv_item.
  MOVE-CORRESPONDING xbset TO gt_alv_item.
  gt_alv_item-ktonr = ktonr.           "Account
  gt_alv_item-name = name.             "Business Partner
  gt_alv_item-land1 = land1.           " Country
  gt_alv_item-regio = regio.           " Region
  gt_alv_item-curr_kbetr = '3'.        "3 decimal places
  gt_alv_item-xblnr = bkpf-xblnr.      "Referenc document
  gt_alv_item-bldat = bkpf-bldat.      "Document Date
  gt_alv_item-waers = bkpf-waers.      "document currency
  gt_alv_item-hwaer = bkpf-hwaer.      "local currency
  gt_alv_item-hwae2 = bkpf-hwae2.      "local currency 2
  gt_alv_item-hwae3 = bkpf-hwae3.      "local currency 3
  gt_alv_item-lwaer = x005-waers.      "country currency
  gt_alv_item-text1 = xtxjt-text1.
  gt_alv_item-text2 = x007-text1.
  gt_alv_item-mwart = x007-mwart.

* Get Jursidictin Code on State-Level
  PERFORM get_jurisdiction_state USING    xbset-bukrs
                                          xbset-txjcd
                                 CHANGING gt_alv_item-jur_sta.
  APPEND gt_alv_item.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM print_heading                                            *
*---------------------------------------------------------------------*
*       Print a header                                                *
*---------------------------------------------------------------------*
FORM print_heading.
  IF ( NOT gt_alv_item-bukrs IS INITIAL ) AND header = 'X'.
    WRITE: /.
    ULINE AT /1(sy-linsz).
    WRITE:    sy-vline,
              text-020,
              gt_alv_item-txjcd,
              gt_alv_item-text1.
    WRITE AT sy-linsz(1) sy-vline.
    WRITE: / sy-vline,
             text-021,
             gt_alv_item-mwskz UNDER gt_alv_item-txjcd,
             gt_alv_item-text2 UNDER gt_alv_item-text1.
    WRITE AT sy-linsz(1) sy-vline.
  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  GET_JURISDICTION_STATE
*&---------------------------------------------------------------------*
*       Get Jurisdicion Code on State-Level
*----------------------------------------------------------------------*
*      <--C_JUR_STA
*      -->U_BUKRS
*      -->U_TXJCD
*----------------------------------------------------------------------*
FORM get_jurisdiction_state USING    u_bukrs type bset-bukrs
                                     u_txjcd type bset-txjcd
                            CHANGING c_jur_sta type bset-txjcd.

  DATA: BEGIN OF lt_txjcd OCCURS 4,
          txjcd LIKE ttxj-txjcd,
        END OF lt_txjcd.

  if ( not u_bukrs is initial ) and ( not u_txjcd is initial ).
  CALL FUNCTION 'FI_TAX_GET_TXJCD_LEVELS'
       EXPORTING
            i_bukrs     = u_bukrs
            i_txjcd     = u_txjcd
       TABLES
            tab_e_txjcd = lt_txjcd[].
  READ TABLE lt_txjcd.
  c_jur_sta = lt_txjcd.
  else.
    clear c_jur_sta.
  endif.

ENDFORM.                               " GET_JURISDICTION_STATE


*---------------------------------------------------------------------*
*       FORM get_start_variant                                        *
*---------------------------------------------------------------------*
*       Define the Start Variant for the ALV                          *
*---------------------------------------------------------------------*
*  -->  C_VARIANT                                                     *
*---------------------------------------------------------------------*
FORM get_start_variant CHANGING c_variant TYPE disvariant.

  c_variant-report = sy-repid.
  c_variant-username = sy-uname.

* Read User´s Start Variant
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
       EXPORTING
            i_save     = 'A'
       CHANGING
            cs_variant = c_variant
       EXCEPTIONS
            OTHERS     = 0.

* Define Start Variant, if User has no one
  IF ( c_variant-variant IS INITIAL ).
    CLEAR c_variant-username.
    c_variant-variant = '1SAP'.
  ENDIF.

* Existence-Check for Variant c_variant.
  CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
       EXPORTING
            i_save     = 'A'
       CHANGING
            cs_variant = c_variant
       EXCEPTIONS
            OTHERS     = 4.
  IF sy-subrc <> 0.
    CLEAR c_variant.
  ENDIF.

ENDFORM.

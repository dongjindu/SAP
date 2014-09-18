FUNCTION Z_FI_TAX_SHOW_BSET.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(I_BUKRS) LIKE  BKPF-BUKRS OPTIONAL
*"     REFERENCE(I_BELNR) LIKE  BKPF-BELNR OPTIONAL
*"     REFERENCE(I_GJAHR) LIKE  BKPF-GJAHR OPTIONAL
*"     REFERENCE(I_WAERS) LIKE  BKPF-WAERS OPTIONAL
*"     REFERENCE(I_SCREEN_START_COLUMN) TYPE  I DEFAULT 0
*"     REFERENCE(I_SCREEN_START_LINE) TYPE  I DEFAULT 0
*"     REFERENCE(I_SCREEN_END_COLUMN) TYPE  I DEFAULT 0
*"     REFERENCE(I_SCREEN_END_LINE) TYPE  I DEFAULT 0
*"  TABLES
*"      T_BSET STRUCTURE  BSET OPTIONAL
*"  EXCEPTIONS
*"      PARAMETER_ERROR
*"      ALV_PROBLEM
*"----------------------------------------------------------------------

  TYPE-POOLS: slis.
  TABLES: bset.

* Field string of lt_bset_am ('Local Table BSET and More')
  DATA BEGIN OF l_bset_am.
          INCLUDE STRUCTURE bset.
  DATA:   text1    TYPE t007s-text1,                        "VAT
          curr_kbe TYPE bkpf-hwaer,    "currency unit of BSET-KBETR
          text2    TYPE ttxjt-text1,   "Jurisdiction Code
          waers    TYPE bkpf-waers,    "document currency
          hwaer    TYPE bkpf-hwaer,    "local currency
          hwae2    TYPE bkpf-hwae2,    "local currency 2
          hwae3    TYPE bkpf-hwae3,    "local currency 3
          lwaer    TYPE t005-waers,    "country currency
       END OF l_bset_am.
* Lokal Table which is printed on the screen with the ALV
  DATA: lt_bset_am LIKE l_bset_am OCCURS 5.
* More Data
  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv, "Infos about LT_BSET_AM
        l_variant   TYPE disvariant,   "Display-Variant (ALV)
        l_repid     TYPE sy-repid,     "Program name (ALV)
        l_flg_jurisdiction TYPE bkpf-xusvr,   "Jurisdiction Code activ?
        l_flg_ext_tax TYPE ttxd-xextn, "External tax system activ?
        l_flg_wia     TYPE t000f-xwiaa,"Plants abroad activ?
        l_kalsm  TYPE t005-kalsm,      "Pricing Procedure
        l_waers  TYPE bkpf-waers,      "document currency
        l_hwaer1 TYPE bkpf-waers,      "local currency 1
        l_hwaer2 TYPE bkpf-waers,      "local currency 2
        l_hwaer3 TYPE bkpf-waers,      "local currency 3
        l_bukrs  TYPE bkpf-bukrs,      "company code
        l_belnr  TYPE bkpf-belnr,      "document number
        l_gjahr  TYPE bkpf-gjahr,      "fiscal year
        l_x001   TYPE x001,
        l_t007s  TYPE t007s,
        l_t005   TYPE t005,
        l_t001   TYPE t001.

* Check Parameters
  IF ( NOT i_belnr IS INITIAL ).
    IF ( i_bukrs IS INITIAL ) OR
       ( i_gjahr IS INITIAL ).
      MESSAGE e769 WITH 'FI_TAX_SHOW_BSET' '1' 'TAX2'
                   RAISING parameter_error.
    ENDIF.
  ENDIF.

* Initialization
  l_repid = sy-repid.
  l_variant-handle = 'BSET'.
  l_variant-report = l_repid.

* Create LT_BSET_AM
  IF ( NOT i_belnr IS INITIAL ).       "Select Tax Items
    SELECT * FROM bset WHERE bukrs = i_bukrs
                       AND   belnr = i_belnr
                       AND   gjahr = i_gjahr.
      CLEAR l_bset_am.
      MOVE-CORRESPONDING bset TO l_bset_am.
      APPEND l_bset_am TO lt_bset_am.
    ENDSELECT.
* Nothing found? Maybe the document is archived and BSET has
* already been read by FI_DOCUMENT_READ_SINGLE.
    IF sy-subrc NE 0 AND NOT t_bset[] IS INITIAL.          "Note 490520
      lt_bset_am[] = t_bset[].                             "Note 490520
    ENDIF.                                                 "Note 490520
  ELSE.                                "Read Tax Items
    LOOP AT t_bset.
      CLEAR l_bset_am.
      MOVE-CORRESPONDING t_bset TO l_bset_am.
      APPEND l_bset_am TO lt_bset_am.
    ENDLOOP.
  ENDIF.

* Table LT_BSET_AM is not empty?
  READ TABLE lt_bset_am INDEX 1 INTO l_bset_am.
  IF sy-subrc = 0.

    l_bukrs = l_bset_am-bukrs.
    l_gjahr = l_bset_am-gjahr.
    l_belnr = l_bset_am-belnr.

*   Check Company Code
    CALL FUNCTION 'FI_COMPANY_CODE_CHECK'
         EXPORTING
              i_bukrs = l_bukrs.

*   Plants abroad activ? (T000F)
    CALL FUNCTION 'CHECK_PLANTS_ABROAD_ACTIVE'
         EXPORTING
              i_bukrs       = l_bukrs
         IMPORTING
              e_fi_isactive = l_flg_wia.

*   Jurisdiction Code activ?
    CALL FUNCTION 'FI_TAX_CHK_JURISDICTION_ACTIVE'
         EXPORTING
              i_bukrs    = l_bukrs
         IMPORTING
              e_isactive = l_flg_jurisdiction
              e_external = l_flg_ext_tax
         EXCEPTIONS
              OTHERS     = 0.

*   Read pricing procedure (T005)
    CALL FUNCTION 'FI_TAX_GET_PRICING_PROCEDURE'
         EXPORTING
              i_bukrs = l_bukrs
         IMPORTING
              e_kalsm = l_kalsm
         EXCEPTIONS
              OTHERS  = 0.

*   Get document currency
    IF ( i_waers IS INITIAL ).         "Unkown document currency
      SELECT SINGLE waers FROM bkpf INTO l_waers
                          WHERE bukrs = l_bukrs
                          AND   belnr = l_belnr
                          AND   gjahr = l_gjahr.
    ELSE.
      l_waers = i_waers.
    ENDIF.

*   Get local currency 1 (= company code currency)
    CALL FUNCTION 'FI_COMPANY_CODE_DATA'
         EXPORTING
              i_bukrs = l_bukrs
         IMPORTING
              e_t001  = l_t001
         EXCEPTIONS
              OTHERS  = 11.
    IF sy-subrc = 0.
      l_hwaer1 = l_t001-waers.
    ENDIF.

*   Get local currency 2 and 3
    CALL FUNCTION 'FI_CURRENCY_INFORMATION'
         EXPORTING
              i_bukrs = l_bukrs
              i_land1 = l_t001-land1
         IMPORTING
              e_x001  = l_x001
         EXCEPTIONS
              OTHERS  = 10.
    IF sy-subrc = 0.
      l_hwaer2 = l_x001-hwae2.
      l_hwaer3 = l_x001-hwae3.
    ENDIF.

*   Insert further information into lt_bset_am
    LOOP AT lt_bset_am INTO l_bset_am.

      l_bset_am-curr_kbe = '3'.        "3 decimal places
      l_bset_am-waers = l_waers.
      l_bset_am-hwaer = l_hwaer1.
      l_bset_am-hwae2 = l_hwaer2.
      l_bset_am-hwae3 = l_hwaer3.

*   switch sign for tax amounts in credit line items
      IF l_bset_am-shkzg = 'H'.
        PERFORM change_sign USING l_bset_am.
      ENDIF.

      IF l_flg_wia = 'X'.              "Plants abroad activ
        IF ( NOT l_bset_am-lstml IS INITIAL ).
*         Read country currency (T005)
          CALL FUNCTION 'FI_COUNTRY_DATA'
               EXPORTING
                    i_land1       = l_bset_am-lstml
               IMPORTING
                    e_t005        = l_t005
               EXCEPTIONS
                    error_message = 2.
          IF sy-subrc = 0.
            l_bset_am-lwaer = l_t005-waers.
          ENDIF.
        ENDIF.
      ENDIF.

      IF ( NOT l_kalsm IS INITIAL ).
*       Read name of VAT (T007S)
        CALL FUNCTION 'MM_T007S_READ'
             EXPORTING
                  i_spras = sy-langu
                  i_kalsm = l_kalsm
                  i_mwskz = l_bset_am-mwskz
             IMPORTING
                  e_t007s = l_t007s
             EXCEPTIONS
                  OTHERS  = 4.
        IF sy-subrc = 0.
          l_bset_am-text1 = l_t007s-text1.
        ENDIF.
      ENDIF.

      IF ( l_flg_jurisdiction = 'X' ). "Jurisdiction Code
        IF ( NOT l_bset_am-txjcd IS INITIAL )
        AND ( l_flg_ext_tax IS INITIAL ).
*       Read name of jurisdictin code (TTXJT)
          CALL FUNCTION 'PRUEFEN_STEUER_STANDORT'
               EXPORTING
                    bukrs         = l_bset_am-bukrs
                    txjcd         = l_bset_am-txjcd
               IMPORTING
                    standort_text = l_bset_am-text2
               EXCEPTIONS
                    OTHERS        = 0.
        ENDIF.
      ENDIF.

      MODIFY lt_bset_am FROM l_bset_am.
      move-corresponding  l_bset_am to   T_BSET.
      if t_bset-kbetr > 0.
       t_bset-kbetr = t_bset-kbetr / 10.
      endif.
      append t_bset.
    ENDLOOP.

  ENDIF.

** Create all other informations for the ALV
*  PERFORM prepare_alv   USING    l_repid
*                        CHANGING lt_fieldcat.
*
** Header
*  sy-title = text-013.
*
** Print LT_BSET_AM on screen
*  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
*       EXPORTING
*            i_callback_program    = l_repid
*            it_fieldcat           = lt_fieldcat
*            i_default             = 'X'
*            i_save                = 'A'
*            is_variant            = l_variant
*            i_screen_start_column = i_screen_start_column
*            i_screen_start_line   = i_screen_start_line
*            i_screen_end_column   = i_screen_end_column
*            i_screen_end_line     = i_screen_end_line
*       TABLES
*            t_outtab              = lt_bset_am
*       EXCEPTIONS
*            OTHERS                = 8.
*  IF sy-subrc <> 0.
*    MESSAGE e769 WITH 'FI_TAX_SHOW_BSET' '8' 'TAX2'
*                 RAISING alv_problem.
*  ENDIF.


ENDFUNCTION.



*---------------------------------------------------------------------*
*       FORM prepare_alv                                              *
*---------------------------------------------------------------------*
*       To display the table LT_BSET_AM, the ALV needs some more      *
*       informations. Here they are created.                          *
*---------------------------------------------------------------------*
*  <--  lt_fieldcat                                                   *
*---------------------------------------------------------------------*
*  -->  u_repid                                                       *
*---------------------------------------------------------------------*
FORM prepare_alv USING    u_repid TYPE sy-repid
                 CHANGING lt_fieldcat TYPE slis_t_fieldcat_alv.

* Append the fields of T_BSET_AM, which are not in BSET, to LT_FIELDCAT
  PERFORM append_fieldcat TABLES lt_fieldcat
                         USING: 'TEXT1'    'T007S'  space  'X' space 30,
                                'TEXT2'    'TTXJT' 'TEXT1' 'X' space 30,
                                'WAERS'    'BKPF'   space  'X' space  0,
                                'HWAER'    'BKPF'   space  'X' space  0,
                                'HWAE2'    'BKPF'   space  'X' space  0,
                                'HWAE3'    'BKPF'   space  'X' space  0,
                                'LWAER'    'T005'  'WAERS' 'X' space  5,
                                'CURR_KBE' 'BKPF'  'HWAER' 'X' 'X'    0.

* Insert the fields of BSET into LT_FIELDCAT
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name   = u_repid
            i_structure_name = 'BSET'
       CHANGING
            ct_fieldcat      = lt_fieldcat
       EXCEPTIONS
            OTHERS           = 7.
  IF sy-subrc <> 0.
    MESSAGE e769 WITH 'FI_TAX_SHOW_BSET' '7' 'TAX2'
                 RAISING alv_problem.
  ENDIF.

  PERFORM modify_fieldcat TABLES lt_fieldcat.

ENDFORM.


*---------------------------------------------------------------------*
*       FORM append_fieldcat                                          *
*---------------------------------------------------------------------*
*       Append using-parameters to table l_lt_fieldcat                *
*---------------------------------------------------------------------*
*  -->  l_lt_fieldcat                                                 *
*  -->  u_l_fieldname                                                 *
*  -->  u_l_ref_tabname                                               *
*  -->  u_l_ref_fieldname                                             *
*  -->  u_l_no_out                                                    *
*  -->  u_l_tech                                                      *
*  -->  u_l_outputlen                                                 *
*---------------------------------------------------------------------*
FORM append_fieldcat
      TABLES
        l_lt_fieldcat
      USING
        u_l_fieldname TYPE slis_fieldcat_alv-fieldname
        u_l_ref_tabname TYPE slis_fieldcat_alv-ref_tabname
        u_l_ref_fieldname TYPE slis_fieldcat_alv-ref_fieldname
        u_l_no_out  TYPE slis_fieldcat_alv-no_out
        u_l_tech    TYPE slis_fieldcat_alv-tech
        u_l_outputlen TYPE slis_fieldcat_alv-outputlen.

  DATA: l_fieldcat TYPE slis_fieldcat_alv.        "field string

  l_fieldcat-fieldname      = u_l_fieldname.
  l_fieldcat-ref_tabname    = u_l_ref_tabname.
  l_fieldcat-ref_fieldname  = u_l_ref_fieldname.
  l_fieldcat-no_out         = u_l_no_out.
  l_fieldcat-tech           = u_l_tech.
  l_fieldcat-outputlen      = u_l_outputlen.
  APPEND l_fieldcat TO l_lt_fieldcat.

ENDFORM.



*---------------------------------------------------------------------*
*       FORM modify_fieldcat                                          *
*---------------------------------------------------------------------*
*  -->  lt_fieldcat                                                   *
*---------------------------------------------------------------------*
FORM modify_fieldcat TABLES lt_fieldcat.
  DATA l_fieldcat TYPE slis_fieldcat_alv.        "Field String

  LOOP AT lt_fieldcat INTO l_fieldcat.
    IF ( l_fieldcat-fieldname     <> 'FWBAS'     "tax basis
         AND l_fieldcat-fieldname <> 'HKONT'     "general ledger account
         AND l_fieldcat-fieldname <> 'FWSTE'     "tax amount
         AND l_fieldcat-fieldname <> 'MWSKZ'     "VAT indicator
         AND l_fieldcat-fieldname <> 'SHKZG'     "Debit/credit indicator
         AND l_fieldcat-fieldname <> 'KBETR' ).  "condition amount
      l_fieldcat-no_out = 'X'.         "Do not display
    ENDIF.

    IF ( l_fieldcat-fieldname  = 'FWSTE' ).     "tax amount
      l_fieldcat-do_sum = 'X'.         "Amounts
    ENDIF.

    IF ( l_fieldcat-fieldname  = 'FWSTE' )      "tax amount
      OR ( l_fieldcat-fieldname  = 'FWBAS' ).   "tax basis
      l_fieldcat-outputlen = 15.
    ENDIF.

* Insert the name of the internal output table field containing the
* currency unit associated with the amount field FIELDCAT-FIELDNAME
* and set the output-length
    CASE l_fieldcat-fieldname.
      WHEN 'KBETR'.
        l_fieldcat-cfieldname = 'CURR_KBE'.
        l_fieldcat-outputlen  = 8.
      WHEN 'HWBAS'.
        l_fieldcat-cfieldname = 'HWAER'.
        l_fieldcat-outputlen  = 16.
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
        l_fieldcat-outputlen  = 16.
      WHEN 'H3STE'.
        l_fieldcat-cfieldname = 'HWAE3'.
      WHEN 'H3BAS'.
        l_fieldcat-outputlen  = 16.
        l_fieldcat-cfieldname = 'HWAE3'.
      WHEN 'LWSTE'.
        l_fieldcat-cfieldname = 'LWAER'.
      WHEN 'LWBAS'.
        l_fieldcat-outputlen  = 16.
        l_fieldcat-cfieldname = 'LWAER'.
    ENDCASE.

    IF ( NOT l_fieldcat-key IS INITIAL ).
      CLEAR l_fieldcat-key.
    ENDIF.

    MODIFY lt_fieldcat FROM l_fieldcat.
  ENDLOOP.
ENDFORM.

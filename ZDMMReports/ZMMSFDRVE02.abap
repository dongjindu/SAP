*&---------------------------------------------------------------------*
*&  Include           ZMMSFDRVE02
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*   Smart Form Print Routines                                          *
*----------------------------------------------------------------------*
form entry_neu using ent_retco ent_screen.

  data: l_druvo like t166k-druvo,
        l_nast  like nast,
        l_from_memory,
        l_doc   type meein_purchase_doc_print.
  data: ls_print_data_to_read type lbbil_print_data_to_read.
  data: ls_bil_invoice type lbbil_invoice.
  data: lf_fm_name            type rs38l_fnam.
  data: ls_control_param      type ssfctrlop.
  data: ls_composer_param     type ssfcompop.
  data: ls_recipient          type swotobjid.
  data: ls_sender             type swotobjid.
  data: lf_formname           type tdsfname.
  data: ls_addr_key           like addr_key.

  xscreen = ent_screen.

  clear ent_retco.
  if nast-aende eq space.
    l_druvo = '1'.
  else.
    l_druvo = '2'.
  endif.

  call function 'ME_READ_PO_FOR_PRINTING'
       exporting
            ix_nast        = nast
            ix_screen      = ent_screen
       importing
            ex_retco       = ent_retco
            ex_nast        = l_nast
            doc            = l_doc
       changing
            cx_druvo       = l_druvo
            cx_from_memory = l_from_memory.
  check ent_retco eq 0.
  if nast-adrnr is initial.
    perform get_addr_key
                         changing ls_addr_key.
  else.
    ls_addr_key = nast-adrnr.
  endif.

*Set the print Parameters
  perform set_print_param using      ls_addr_key
                            changing ls_control_param
                                     ls_composer_param
                                     ls_recipient
                                     ls_sender
                                     ent_retco.
*Get the Smart Form name.
  if not tnapr-sform is initial.
    lf_formname = tnapr-sform.
  else.
    lf_formname = tnapr-fonam.
  endif.
* determine smartform function module for invoice
  call function 'SSF_FUNCTION_MODULE_NAME'
       exporting
            formname           = lf_formname
       importing
            fm_name            = lf_fm_name
       exceptions
            no_form            = 1
            no_function_module = 2
            others             = 3.
  if sy-subrc <> 0.
*  error handling
    ent_retco = sy-subrc.
    perform protocol_update_i.
  endif.


  call function lf_fm_name
       exporting
            archive_index      = toa_dara
            archive_parameters = arc_params
            control_parameters = ls_control_param
            mail_recipient     = ls_recipient
            mail_sender        = ls_sender
            output_options     = ls_composer_param
            user_settings      = ' '
            zxekko             = l_doc-xekko
            zxpekko            = l_doc-xpekko
       tables
            l_xekpo            = l_doc-xekpo[]
            l_xekpa            = l_doc-xekpa[]
            l_xpekpo           = l_doc-xpekpo[]
            l_xeket            = l_doc-xeket[]
            l_xtkomv           = l_doc-xtkomv[]
            l_xekkn            = l_doc-xekkn[]
            l_xekek            = l_doc-xekek[]
            l_xkomk            = l_xkomk
       exceptions
            formatting_error   = 1
            internal_error     = 2
            send_error         = 3
            user_canceled      = 4
            others             = 5.
  if sy-subrc <> 0.
    ent_retco = sy-subrc.
    perform protocol_update_i.

* get SmartForm protocoll and store it in the NAST protocoll
    perform add_smfrm_prot.
  endif.
endform.
*----------------------------------------------------------------------*
* Mahnung
*----------------------------------------------------------------------*
form entry_mahn using ent_retco ent_screen.

  data: l_druvo like t166k-druvo,
        l_nast  like nast,
        l_from_memory,
        l_doc   type meein_purchase_doc_print.

  clear ent_retco.
  l_druvo = '3'.
  call function 'ME_READ_PO_FOR_PRINTING'
       exporting
            ix_nast        = nast
            ix_screen      = ent_screen
       importing
            ex_retco       = ent_retco
            ex_nast        = l_nast
            doc            = l_doc
       changing
            cx_druvo       = l_druvo
            cx_from_memory = l_from_memory.
  check ent_retco eq 0.
  call function 'ME_PRINT_PO'
       exporting
            ix_nast        = l_nast
            ix_druvo       = l_druvo
            doc            = l_doc
            ix_screen      = ent_screen
            ix_from_memory = l_from_memory
            ix_toa_dara    = toa_dara
            ix_arc_params  = arc_params
       importing
            ex_retco       = ent_retco.
endform.

*eject
*----------------------------------------------------------------------*
* Auftragsbestatigungsmahnung
*----------------------------------------------------------------------*
form entry_aufb using ent_retco ent_screen.

  data: l_druvo like t166k-druvo,
        l_nast  like nast,
        l_from_memory,
        l_doc   type meein_purchase_doc_print.

  clear ent_retco.
  l_druvo = '7'.
  call function 'ME_READ_PO_FOR_PRINTING'
       exporting
            ix_nast        = nast
            ix_screen      = ent_screen
       importing
            ex_retco       = ent_retco
            ex_nast        = l_nast
            doc            = l_doc
       changing
            cx_druvo       = l_druvo
            cx_from_memory = l_from_memory.
  check ent_retco eq 0.
  call function 'ME_PRINT_PO'
       exporting
            ix_nast        = l_nast
            ix_druvo       = l_druvo
            doc            = l_doc
            ix_screen      = ent_screen
            ix_from_memory = l_from_memory
            ix_toa_dara    = toa_dara
            ix_arc_params  = arc_params
       importing
            ex_retco       = ent_retco.
endform.
*eject
*----------------------------------------------------------------------*
* Lieferabrufdruck fur Formular MEDRUCK mit Fortschrittszahlen
*----------------------------------------------------------------------*
form entry_lphe using ent_retco ent_screen.

  data: l_druvo like t166k-druvo,
        l_nast  like nast,
        l_from_memory,
        l_xfz,
        l_doc   type meein_purchase_doc_print.

  clear ent_retco.
  l_druvo = '9'.
  l_xfz = 'X'.
  call function 'ME_READ_PO_FOR_PRINTING'
       exporting
            ix_nast        = nast
            ix_screen      = ent_screen
       importing
            ex_retco       = ent_retco
            ex_nast        = l_nast
            doc            = l_doc
       changing
            cx_druvo       = l_druvo
            cx_from_memory = l_from_memory.
  check ent_retco eq 0.
  if nast-adrnr is initial.
    perform get_addr_key
                         changing ls_addr_key.
  else.
    ls_addr_key = nast-adrnr.
  endif.
  perform set_print_param using      ls_addr_key
                            changing ls_control_param
                                     ls_composer_param
                                     ls_recipient
                                     ls_sender
                                     ent_retco.


*Get the Smart Form name.
  if not tnapr-sform is initial.
    lf_formname = tnapr-sform.
  else.
    lf_formname = tnapr-fonam.
  endif.

* determine smartform function module for invoice
  call function 'SSF_FUNCTION_MODULE_NAME'
       exporting  formname           = lf_formname
*                 variant            = ' '
*                 direct_call        = ' '
       importing  fm_name            = lf_fm_name
       exceptions no_form            = 1
                  no_function_module = 2
                  others             = 3.
  if sy-subrc <> 0.
*   error handling
    ent_retco = sy-subrc.
    perform protocol_update_i.
  endif.

* move the value
  move-corresponding l_doc-xekko to l_zekko.
  move-corresponding l_doc-xpekko to l_xpekko.
  l_xekpo[] = l_doc-xekpo[].
  l_xekpa[] = l_doc-xekpa[].
  l_xpekpo[] = l_doc-xpekpo[].
  l_xeket[] = l_doc-xeket[].
  l_xtkomv[] = l_doc-xtkomv[].
  l_xekkn[] = l_doc-xekkn[].
*  l_xekek[] = l_doc-xekeh[].  "" KDM
  l_xekek[] = l_doc-xekek[].   "" KDM
*l_xaend[]    = l_doc-xaend[].

  call function lf_fm_name
    exporting
     archive_index              = toa_dara
*   ARCHIVE_INDEX_TAB          =
     archive_parameters         = arc_params
     control_parameters         = ls_control_param
*   MAIL_APPL_OBJ              =
     mail_recipient             = ls_recipient
     mail_sender                = ls_sender
     output_options             = ls_composer_param
     user_settings              = ' '
      zxekko                     = l_zekko
      zxpekko                    = l_xpekko
*   l_xaend                    = l_xaend
*    IMPORTING
*   DOCUMENT_OUTPUT_INFO       =
*   JOB_OUTPUT_INFO            =
*   JOB_OUTPUT_OPTIONS         =
    tables
      l_xekpo                    = l_xekpo
      l_xekpa                    = l_xekpa
      l_xpekpo                   = l_xpekpo
      l_xeket                    = l_xeket
      l_xtkomv                   = l_xtkomv
      l_xekkn                    = l_xekkn
      l_xekek                    = l_xekek
      l_xkomk                    = l_xkomk
*    l_xaend                    = l_xaend
* EXCEPTIONS
*   FORMATTING_ERROR           = 1
*   INTERNAL_ERROR             = 2
*   SEND_ERROR                 = 3
*   USER_CANCELED              = 4
*   OTHERS                     = 5
            .
  if sy-subrc <> 0.
    ent_retco = sy-subrc.
    perform protocol_update_i.
    perform add_smfrm_prot.
  endif.
endform.
*eject
*----------------------------------------------------------------------*
* Lieferabrufdruck fur Formular MEDRUCK ohne Fortschrittszahlen
*----------------------------------------------------------------------*
form entry_lphe_cd using ent_retco ent_screen.

  data: l_druvo like t166k-druvo,
        l_nast  like nast,
        l_from_memory,
        l_doc   type meein_purchase_doc_print.

  clear ent_retco.
  l_druvo = '9'.
  call function 'ME_READ_PO_FOR_PRINTING'
       exporting
            ix_nast        = nast
            ix_screen      = ent_screen
       importing
            ex_retco       = ent_retco
            ex_nast        = l_nast
            doc            = l_doc
       changing
            cx_druvo       = l_druvo
            cx_from_memory = l_from_memory.
  check ent_retco eq 0.
  if nast-adrnr is initial.
    perform get_addr_key
                         changing ls_addr_key.
  else.
    ls_addr_key = nast-adrnr.
  endif.
  perform set_print_param using      ls_addr_key
                            changing ls_control_param
                                     ls_composer_param
                                     ls_recipient
                                     ls_sender
                                     ent_retco.


  lf_formname = tnapr-fonam.
* determine smartform function module for invoice
  call function 'SSF_FUNCTION_MODULE_NAME'
       exporting  formname           = lf_formname
*                 variant            = ' '
*                 direct_call        = ' '
       importing  fm_name            = lf_fm_name
       exceptions no_form            = 1
                  no_function_module = 2
                  others             = 3.
  if sy-subrc <> 0.
*   error handling
    ent_retco = sy-subrc.
    perform protocol_update_i.
  endif.

* move the value
  move-corresponding l_doc-xekko to l_zekko.
  move-corresponding l_doc-xpekko to l_xpekko.
  l_xekpo[] = l_doc-xekpo[].
  l_xekpa[] = l_doc-xekpa[].
  l_xpekpo[] = l_doc-xpekpo[].
  l_xeket[] = l_doc-xeket[].
  l_xtkomv[] = l_doc-xtkomv[].
  l_xekkn[] = l_doc-xekkn[].
*  l_xekek[] = l_doc-xekeh[].   ""KDM
  l_xekek[] = l_doc-xekek[].   ""KDM
*  l_xaend[]    = l_doc-xaend[].

  call function lf_fm_name
    exporting
     archive_index              = toa_dara
*   ARCHIVE_INDEX_TAB          =
     archive_parameters         = arc_params
     control_parameters         = ls_control_param
*   MAIL_APPL_OBJ              =
     mail_recipient             = ls_recipient
     mail_sender                = ls_sender
     output_options             = ls_composer_param
     user_settings              = ' '
      zxekko                     = l_zekko
      zxpekko                    = l_xpekko
*   l_xaend                    = l_xaend
*    IMPORTING
*   DOCUMENT_OUTPUT_INFO       =
*   JOB_OUTPUT_INFO            =
*   JOB_OUTPUT_OPTIONS         =
    tables
      l_xekpo                    = l_xekpo
      l_xekpa                    = l_xekpa
      l_xpekpo                   = l_xpekpo
      l_xeket                    = l_xeket
      l_xtkomv                   = l_xtkomv
      l_xekkn                    = l_xekkn
      l_xekek                    = l_xekek
      l_xkomk                    = l_xkomk
*    l_xaend                    = l_xaend
* EXCEPTIONS
*   FORMATTING_ERROR           = 1
*   INTERNAL_ERROR             = 2
*   SEND_ERROR                 = 3
*   USER_CANCELED              = 4
*   OTHERS                     = 5
            .
  if sy-subrc <> 0.
    ent_retco = sy-subrc.
    perform protocol_update_i.
    perform add_smfrm_prot.
  endif.
endform.
*eject
*----------------------------------------------------------------------*
* Feinabrufdruck fur Formular MEDRUCK mit Fortschrittszahlen
*----------------------------------------------------------------------*
form entry_lpje using ent_retco ent_screen.

  data: l_druvo like t166k-druvo,
        l_nast  like nast,
        l_from_memory,
        l_xfz,
        l_doc   type meein_purchase_doc_print.

  clear ent_retco.
  l_druvo = 'A'.
  l_xfz = 'X'.
  call function 'ME_READ_PO_FOR_PRINTING'
       exporting
            ix_nast        = nast
            ix_screen      = ent_screen
       importing
            ex_retco       = ent_retco
            ex_nast        = l_nast
            doc            = l_doc
       changing
            cx_druvo       = l_druvo
            cx_from_memory = l_from_memory.
  check ent_retco eq 0.
  if nast-adrnr is initial.
    perform get_addr_key
                         changing ls_addr_key.
  else.
    ls_addr_key = nast-adrnr.
  endif.
  perform set_print_param using      ls_addr_key
                            changing ls_control_param
                                     ls_composer_param
                                     ls_recipient
                                     ls_sender
                                     ent_retco.


  lf_formname = tnapr-fonam.
* determine smartform function module for invoice
  call function 'SSF_FUNCTION_MODULE_NAME'
       exporting  formname           = lf_formname
*                 variant            = ' '
*                 direct_call        = ' '
       importing  fm_name            = lf_fm_name
       exceptions no_form            = 1
                  no_function_module = 2
                  others             = 3.
  if sy-subrc <> 0.
*   error handling
    ent_retco = sy-subrc.
    perform protocol_update_i.
  endif.

* move the value
  move-corresponding l_doc-xekko to l_zekko.
  move-corresponding l_doc-xpekko to l_xpekko.
  l_xekpo[] = l_doc-xekpo[].
  l_xekpa[] = l_doc-xekpa[].
  l_xpekpo[] = l_doc-xpekpo[].
  l_xeket[] = l_doc-xeket[].
  l_xtkomv[] = l_doc-xtkomv[].
  l_xekkn[] = l_doc-xekkn[].
*  l_xekek[] = l_doc-xekeh[].   ""KDM
  l_xekek[] = l_doc-xekek[].   ""KDM
*  l_xaend[]    = l_doc-xaend[].

  call function lf_fm_name
    exporting
     archive_index              = toa_dara
*   ARCHIVE_INDEX_TAB          =
     archive_parameters         = arc_params
     control_parameters         = ls_control_param
*   MAIL_APPL_OBJ              =
     mail_recipient             = ls_recipient
     mail_sender                = ls_sender
     output_options             = ls_composer_param
     user_settings              = ' '
      zxekko                     = l_zekko
      zxpekko                    = l_xpekko
*   l_xaend                    = l_xaend
*    IMPORTING
*   DOCUMENT_OUTPUT_INFO       =
*   JOB_OUTPUT_INFO            =
*   JOB_OUTPUT_OPTIONS         =
    tables
      l_xekpo                    = l_xekpo
      l_xekpa                    = l_xekpa
      l_xpekpo                   = l_xpekpo
      l_xeket                    = l_xeket
      l_xtkomv                   = l_xtkomv
      l_xekkn                    = l_xekkn
      l_xekek                    = l_xekek
      l_xkomk                    = l_xkomk
*    l_xaend                    = l_xaend
* EXCEPTIONS
*   FORMATTING_ERROR           = 1
*   INTERNAL_ERROR             = 2
*   SEND_ERROR                 = 3
*   USER_CANCELED              = 4
*   OTHERS                     = 5
            .
  if sy-subrc <> 0.
    ent_retco = sy-subrc.
    perform protocol_update_i.
    perform add_smfrm_prot.
  endif.
endform.
*eject
*----------------------------------------------------------------------*
* Feinabrufdruck fur Formular MEDRUCK ohne Fortschrittszahlen
*----------------------------------------------------------------------*
form entry_lpje_cd using ent_retco ent_screen.

  data: l_druvo like t166k-druvo,
        l_nast  like nast,
        l_from_memory,
        l_doc   type meein_purchase_doc_print.

  clear ent_retco.
  l_druvo = 'A'.
  call function 'ME_READ_PO_FOR_PRINTING'
       exporting
            ix_nast        = nast
            ix_screen      = ent_screen
       importing
            ex_retco       = ent_retco
            ex_nast        = l_nast
            doc            = l_doc
       changing
            cx_druvo       = l_druvo
            cx_from_memory = l_from_memory.
  check ent_retco eq 0.
  if nast-adrnr is initial.
    perform get_addr_key
                         changing ls_addr_key.
  else.
    ls_addr_key = nast-adrnr.
  endif.
  perform set_print_param using      ls_addr_key
                            changing ls_control_param
                                     ls_composer_param
                                     ls_recipient
                                     ls_sender
                                     ent_retco.


  lf_formname = tnapr-fonam.
* determine smartform function module for invoice
  call function 'SSF_FUNCTION_MODULE_NAME'
       exporting  formname           = lf_formname
*                 variant            = ' '
*                 direct_call        = ' '
       importing  fm_name            = lf_fm_name
       exceptions no_form            = 1
                  no_function_module = 2
                  others             = 3.
  if sy-subrc <> 0.
*   error handling
    ent_retco = sy-subrc.
    perform protocol_update_i.
  endif.

* move the value
  move-corresponding l_doc-xekko to l_zekko.
  move-corresponding l_doc-xpekko to l_xpekko.
  l_xekpo[] = l_doc-xekpo[].
  l_xekpa[] = l_doc-xekpa[].
  l_xpekpo[] = l_doc-xpekpo[].
  l_xeket[] = l_doc-xeket[].
  l_xtkomv[] = l_doc-xtkomv[].
  l_xekkn[] = l_doc-xekkn[].
*  l_xekek[] = l_doc-xekeh[].    ""KDM
  l_xekek[] = l_doc-xekek[].    ""KDM
*  l_xaend[]    = l_doc-xaend[].

  call function lf_fm_name
    exporting
     archive_index              = toa_dara
*   ARCHIVE_INDEX_TAB          =
     archive_parameters         = arc_params
     control_parameters         = ls_control_param
*   MAIL_APPL_OBJ              =
     mail_recipient             = ls_recipient
     mail_sender                = ls_sender
     output_options             = ls_composer_param
     user_settings              = ' '
      zxekko                     = l_zekko
      zxpekko                    = l_xpekko
*   l_xaend                    = l_xaend
*    IMPORTING
*   DOCUMENT_OUTPUT_INFO       =
*   JOB_OUTPUT_INFO            =
*   JOB_OUTPUT_OPTIONS         =
    tables
      l_xekpo                    = l_xekpo
      l_xekpa                    = l_xekpa
      l_xpekpo                   = l_xpekpo
      l_xeket                    = l_xeket
      l_xtkomv                   = l_xtkomv
      l_xekkn                    = l_xekkn
      l_xekek                    = l_xekek
      l_xkomk                    = l_xkomk
*    l_xaend                    = l_xaend
* EXCEPTIONS
*   FORMATTING_ERROR           = 1
*   INTERNAL_ERROR             = 2
*   SEND_ERROR                 = 3
*   USER_CANCELED              = 4
*   OTHERS                     = 5
            .
  if sy-subrc <> 0.
    ent_retco = sy-subrc.
    perform protocol_update_i.
    perform add_smfrm_prot.
  endif.
endform.
*eject
*----------------------------------------------------------------------*
*   INCLUDE FM06PE02                                                   *
*----------------------------------------------------------------------*
form entry_neu_matrix using ent_retco ent_screen.

  data: l_druvo like t166k-druvo,
        l_nast  like nast,
        l_from_memory,
        l_doc   type meein_purchase_doc_print.

  clear ent_retco.
  if nast-aende eq space.
    l_druvo = '1'.
  else.
    l_druvo = '2'.
  endif.

  call function 'ME_READ_PO_FOR_PRINTING'
       exporting
            ix_nast        = nast
            ix_screen      = ent_screen
       importing
            ex_retco       = ent_retco
            ex_nast        = l_nast
            doc            = l_doc
       changing
            cx_druvo       = l_druvo
            cx_from_memory = l_from_memory.
  check ent_retco eq 0.
  if nast-adrnr is initial.
    perform get_addr_key
                         changing ls_addr_key.
  else.
    ls_addr_key = nast-adrnr.
  endif.
  perform set_print_param using      ls_addr_key
                            changing ls_control_param
                                     ls_composer_param
                                     ls_recipient
                                     ls_sender
                                     ent_retco.


  lf_formname = tnapr-fonam.
* determine smartform function module for invoice
  call function 'SSF_FUNCTION_MODULE_NAME'
       exporting  formname           = lf_formname
*                 variant            = ' '
*                 direct_call        = ' '
       importing  fm_name            = lf_fm_name
       exceptions no_form            = 1
                  no_function_module = 2
                  others             = 3.
  if sy-subrc <> 0.
*   error handling
    ent_retco = sy-subrc.
    perform protocol_update_i.
  endif.

* move the value
  move-corresponding l_doc-xekko to l_zekko.
  move-corresponding l_doc-xpekko to l_xpekko.
  l_xekpo[] = l_doc-xekpo[].
  l_xekpa[] = l_doc-xekpa[].
  l_xpekpo[] = l_doc-xpekpo[].
  l_xeket[] = l_doc-xeket[].
  l_xtkomv[] = l_doc-xtkomv[].
  l_xekkn[] = l_doc-xekkn[].
*  l_xekek[] = l_doc-xekeh[].    ""KDM
  l_xekek[] = l_doc-xekek[].    ""KDM
*  l_xaend[]    = l_doc-xaend[].

  call function lf_fm_name
    exporting
     archive_index              = toa_dara
*   ARCHIVE_INDEX_TAB          =
     archive_parameters         = arc_params
     control_parameters         = ls_control_param
*   MAIL_APPL_OBJ              =
     mail_recipient             = ls_recipient
     mail_sender                = ls_sender
     output_options             = ls_composer_param
     user_settings              = ' '
      zxekko                     = l_zekko
      zxpekko                    = l_xpekko
*   l_xaend                    = l_xaend
*    IMPORTING
*   DOCUMENT_OUTPUT_INFO       =
*   JOB_OUTPUT_INFO            =
*   JOB_OUTPUT_OPTIONS         =
    tables
      l_xekpo                    = l_xekpo
      l_xekpa                    = l_xekpa
      l_xpekpo                   = l_xpekpo
      l_xeket                    = l_xeket
      l_xtkomv                   = l_xtkomv
      l_xekkn                    = l_xekkn
      l_xekek                    = l_xekek
      l_xkomk                    = l_xkomk
*    l_xaend                    = l_xaend
* EXCEPTIONS
*   FORMATTING_ERROR           = 1
*   INTERNAL_ERROR             = 2
*   SEND_ERROR                 = 3
*   USER_CANCELED              = 4
*   OTHERS                     = 5
            .
  if sy-subrc <> 0.
    ent_retco = sy-subrc.
    perform protocol_update_i.
    perform add_smfrm_prot.
  endif.
endform.
*eject
*----------------------------------------------------------------------*
* Angebotsabsage
*----------------------------------------------------------------------*
form entry_absa using ent_retco ent_screen.

  data: l_druvo like t166k-druvo,
        l_nast  like nast,
        l_from_memory,
        l_doc   type meein_purchase_doc_print.

  l_druvo = '4'.
  clear ent_retco.
  call function 'ME_READ_PO_FOR_PRINTING'
       exporting
            ix_nast        = nast
            ix_screen      = ent_screen
       importing
            ex_retco       = ent_retco
            ex_nast        = l_nast
            doc            = l_doc
       changing
            cx_druvo       = l_druvo
            cx_from_memory = l_from_memory.
  check ent_retco eq 0.
  if nast-adrnr is initial.
    perform get_addr_key
                         changing ls_addr_key.
  else.
    ls_addr_key = nast-adrnr.
  endif.
  perform set_print_param using      ls_addr_key
                            changing ls_control_param
                                     ls_composer_param
                                     ls_recipient
                                     ls_sender
                                     ent_retco.


  lf_formname = tnapr-fonam.
* determine smartform function module for invoice
  call function 'SSF_FUNCTION_MODULE_NAME'
       exporting  formname           = lf_formname
*                 variant            = ' '
*                 direct_call        = ' '
       importing  fm_name            = lf_fm_name
       exceptions no_form            = 1
                  no_function_module = 2
                  others             = 3.
  if sy-subrc <> 0.
*   error handling
    ent_retco = sy-subrc.
    perform protocol_update_i.
  endif.

* move the value
  move-corresponding l_doc-xekko to l_zekko.
  move-corresponding l_doc-xpekko to l_xpekko.
  l_xekpo[] = l_doc-xekpo[].
  l_xekpa[] = l_doc-xekpa[].
  l_xpekpo[] = l_doc-xpekpo[].
  l_xeket[] = l_doc-xeket[].
  l_xtkomv[] = l_doc-xtkomv[].
  l_xekkn[] = l_doc-xekkn[].
*  l_xekek[] = l_doc-xekeh[].    ""KDM
  l_xekek[] = l_doc-xekek[].    ""KDM
*  l_xaend[]    = l_doc-xaend[].

  call function lf_fm_name
    exporting
     archive_index              = toa_dara
*   ARCHIVE_INDEX_TAB          =
     archive_parameters         = arc_params
     control_parameters         = ls_control_param
*   MAIL_APPL_OBJ              =
     mail_recipient             = ls_recipient
     mail_sender                = ls_sender
     output_options             = ls_composer_param
     user_settings              = ' '
      zxekko                     = l_zekko
      zxpekko                    = l_xpekko
*   l_xaend                    = l_xaend
*    IMPORTING
*   DOCUMENT_OUTPUT_INFO       =
*   JOB_OUTPUT_INFO            =
*   JOB_OUTPUT_OPTIONS         =
    tables
      l_xekpo                    = l_xekpo
      l_xekpa                    = l_xekpa
      l_xpekpo                   = l_xpekpo
      l_xeket                    = l_xeket
      l_xtkomv                   = l_xtkomv
      l_xekkn                    = l_xekkn
      l_xekek                    = l_xekek
      l_xkomk                    = l_xkomk
*    l_xaend                    = l_xaend
* EXCEPTIONS
*   FORMATTING_ERROR           = 1
*   INTERNAL_ERROR             = 2
*   SEND_ERROR                 = 3
*   USER_CANCELED              = 4
*   OTHERS                     = 5
            .
  if sy-subrc <> 0.
    ent_retco = sy-subrc.
    perform protocol_update_i.
    perform add_smfrm_prot.
  endif.
endform.
*eject
*----------------------------------------------------------------------*
* Lieferplaneinteilung
*----------------------------------------------------------------------*
form entry_lpet using ent_retco ent_screen.

  data: l_druvo like t166k-druvo,
        l_nast  like nast,
        l_from_memory,
        l_doc   type meein_purchase_doc_print.
  data: l_zekko like ekko,
        l_xpekko like pekko,
        l_xekpo like table of ekpo,
        l_wa_xekpo like ekpo.

  data: l_xekpa like ekpa occurs 0,
        l_wa_xekpa like ekpa.
  data: l_xpekpo  like pekpo occurs 0,
        l_wa_xpekpo like pekpo,
        l_xeket   like table of eket with header line,
        l_xekkn  like table of ekkn with header line,
        l_xekek  like table of ekek with header line,
        l_xekeh   like table of ekeh with header line,
        l_xkomk like table of komk with header line,
        l_xtkomv  type komv occurs 0,
        l_wa_xtkomv type komv.
  data: ls_addr_key           like addr_key.
  clear ent_retco.
  if nast-aende eq space.
    l_druvo = '5'.
  else.
    l_druvo = '8'.
  endif.

  call function 'ME_READ_PO_FOR_PRINTING'
       exporting
            ix_nast        = nast
            ix_screen      = ent_screen
       importing
            ex_retco       = ent_retco
            ex_nast        = l_nast
            doc            = l_doc
       changing
            cx_druvo       = l_druvo
            cx_from_memory = l_from_memory.
  check ent_retco eq 0.
  if nast-adrnr is initial.
    perform get_addr_key
                         changing ls_addr_key.
  else.
    ls_addr_key = nast-adrnr.
  endif.
  perform set_print_param using      ls_addr_key
                            changing ls_control_param
                                     ls_composer_param
                                     ls_recipient
                                     ls_sender
                                     ent_retco.


*Get the Smart Form name.
  if not tnapr-sform is initial.
    lf_formname = tnapr-sform.
  else.
    lf_formname = tnapr-fonam.
  endif.

* determine smartform function module for invoice
  call function 'SSF_FUNCTION_MODULE_NAME'
       exporting  formname           = lf_formname
*                 variant            = ' '
*                 direct_call        = ' '
       importing  fm_name            = lf_fm_name
       exceptions no_form            = 1
                  no_function_module = 2
                  others             = 3.
  if sy-subrc <> 0.
*   error handling
    ent_retco = sy-subrc.
    perform protocol_update_i.
  endif.
                                       "break limw.
* move the value
  move-corresponding l_doc-xekko to l_zekko.
  move-corresponding l_doc-xpekko to l_xpekko.
  l_xekpo[] = l_doc-xekpo[].
  l_xekpa[] = l_doc-xekpa[].
  l_xpekpo[] = l_doc-xpekpo[].
  l_xeket[] = l_doc-xeket[].
  l_xtkomv[] = l_doc-xtkomv[].
  l_xekkn[] = l_doc-xekkn[].
*  l_xekek[] = l_doc-xekeh[].    ""KDM
  l_xekek[] = l_doc-xekek[].    ""KDM

  call function lf_fm_name
    exporting
     archive_index              = toa_dara
*   ARCHIVE_INDEX_TAB          =
     archive_parameters         = arc_params
     control_parameters         = ls_control_param
*   MAIL_APPL_OBJ              =
     mail_recipient             = ls_recipient
     mail_sender                = ls_sender
     output_options             = ls_composer_param
     user_settings              = ' '
      zxekko                     = l_zekko
      zxpekko                    = l_xpekko
*   l_xaend                    = l_xaend
*    IMPORTING
*   DOCUMENT_OUTPUT_INFO       =
*   JOB_OUTPUT_INFO            =
*   JOB_OUTPUT_OPTIONS         =
    tables
      l_xekpo                    = l_xekpo
      l_xekpa                    = l_xekpa
      l_xpekpo                   = l_xpekpo
      l_xeket                    = l_xeket
      l_xtkomv                   = l_xtkomv
      l_xekkn                    = l_xekkn
      l_xekek                    = l_xekek
      l_xkomk                    = l_xkomk
 exceptions
   formatting_error           = 1
   internal_error             = 2
   send_error                 = 3
   user_canceled              = 4
   others                     = 5
            .
  if sy-subrc <> 0.
    ent_retco = sy-subrc.
    perform protocol_update_i.
* get SmartForm protocoll and store it in the NAST protocoll
    perform add_smfrm_prot.
  endif.
endform.
*eject
*----------------------------------------------------------------------*
* Lieferplaneinteilung
*----------------------------------------------------------------------*
form entry_lpfz using ent_retco ent_screen.

  data: l_druvo like t166k-druvo,
        l_nast  like nast,
        l_from_memory,
        l_doc   type meein_purchase_doc_print.

  clear ent_retco.
  if nast-aende eq space.
    l_druvo = '5'.
  else.
    l_druvo = '8'.
  endif.

  call function 'ME_READ_PO_FOR_PRINTING'
       exporting
            ix_nast        = nast
            ix_screen      = ent_screen
       importing
            ex_retco       = ent_retco
            ex_nast        = l_nast
            doc            = l_doc
       changing
            cx_druvo       = l_druvo
            cx_from_memory = l_from_memory.
  check ent_retco eq 0.
  if nast-adrnr is initial.
    perform get_addr_key
                         changing ls_addr_key.
  else.
    ls_addr_key = nast-adrnr.
  endif.
  perform set_print_param using      ls_addr_key
                            changing ls_control_param
                                     ls_composer_param
                                     ls_recipient
                                     ls_sender
                                     ent_retco.


*Get the Smart Form name.
  if not tnapr-sform is initial.
    lf_formname = tnapr-sform.
  else.
    lf_formname = tnapr-fonam.
  endif.

* determine smartform function module for invoice
  call function 'SSF_FUNCTION_MODULE_NAME'
       exporting  formname           = lf_formname
*                 variant            = ' '
*                 direct_call        = ' '
       importing  fm_name            = lf_fm_name
       exceptions no_form            = 1
                  no_function_module = 2
                  others             = 3.
  if sy-subrc <> 0.
*   error handling
    ent_retco = sy-subrc.
    perform protocol_update_i.
  endif.

* move the value
  move-corresponding l_doc-xekko to l_zekko.
  move-corresponding l_doc-xpekko to l_xpekko.
  l_xekpo[] = l_doc-xekpo[].
  l_xekpa[] = l_doc-xekpa[].
  l_xpekpo[] = l_doc-xpekpo[].
  l_xeket[] = l_doc-xeket[].
  l_xtkomv[] = l_doc-xtkomv[].
  l_xekkn[] = l_doc-xekkn[].
*  l_xekek[] = l_doc-xekeh[].    ""KDM
  l_xekek[] = l_doc-xekek[].    ""KDM
*  l_xaend[]    = l_doc-xaend[].

  call function lf_fm_name
    exporting
     archive_index              = toa_dara
*   ARCHIVE_INDEX_TAB          =
     archive_parameters         = arc_params
     control_parameters         = ls_control_param
*   MAIL_APPL_OBJ              =
     mail_recipient             = ls_recipient
     mail_sender                = ls_sender
     output_options             = ls_composer_param
     user_settings              = ' '
      zxekko                     = l_zekko
      zxpekko                    = l_xpekko
*   l_xaend                    = l_xaend
*    IMPORTING
*   DOCUMENT_OUTPUT_INFO       =
*   JOB_OUTPUT_INFO            =
*   JOB_OUTPUT_OPTIONS         =
    tables
      l_xekpo                    = l_xekpo
      l_xekpa                    = l_xekpa
      l_xpekpo                   = l_xpekpo
      l_xeket                    = l_xeket
      l_xtkomv                   = l_xtkomv
      l_xekkn                    = l_xekkn
      l_xekek                    = l_xekek
      l_xkomk                    = l_xkomk
*    l_xaend                    = l_xaend
 exceptions
   formatting_error           = 1
   internal_error             = 2
   send_error                 = 3
   user_canceled              = 4
   others                     = 5
            .
  if sy-subrc <> 0.
    ent_retco = sy-subrc.
    perform protocol_update_i.
* get SmartForm protocoll and store it in the NAST protocoll
    perform add_smfrm_prot.
  endif.

endform.
*eject
*----------------------------------------------------------------------*
* Mahnung
*----------------------------------------------------------------------*
form entry_lpma using ent_retco ent_screen.

  data: l_druvo like t166k-druvo,
        l_nast  like nast,
        l_from_memory,
        l_doc   type meein_purchase_doc_print.

  clear ent_retco.
  l_druvo = '6'.
  call function 'ME_READ_PO_FOR_PRINTING'
       exporting
            ix_nast        = nast
            ix_screen      = ent_screen
       importing
            ex_retco       = ent_retco
            ex_nast        = l_nast
            doc            = l_doc
       changing
            cx_druvo       = l_druvo
            cx_from_memory = l_from_memory.
  check ent_retco eq 0.
  if nast-adrnr is initial.
    perform get_addr_key
                         changing ls_addr_key.
  else.
    ls_addr_key = nast-adrnr.
  endif.
  perform set_print_param using      ls_addr_key
                            changing ls_control_param
                                     ls_composer_param
                                     ls_recipient
                                     ls_sender
                                     ent_retco.


  lf_formname = tnapr-fonam.
* determine smartform function module for invoice
  call function 'SSF_FUNCTION_MODULE_NAME'
       exporting  formname           = lf_formname
*                 variant            = ' '
*                 direct_call        = ' '
       importing  fm_name            = lf_fm_name
       exceptions no_form            = 1
                  no_function_module = 2
                  others             = 3.
  if sy-subrc <> 0.
*   error handling
    ent_retco = sy-subrc.
    perform protocol_update_i.
  endif.

* move the value
  move-corresponding l_doc-xekko to l_zekko.
  move-corresponding l_doc-xpekko to l_xpekko.
  l_xekpo[] = l_doc-xekpo[].
  l_xekpa[] = l_doc-xekpa[].
  l_xpekpo[] = l_doc-xpekpo[].
  l_xeket[] = l_doc-xeket[].
  l_xtkomv[] = l_doc-xtkomv[].
  l_xekkn[] = l_doc-xekkn[].
*  l_xekek[] = l_doc-xekeh[].    ""KDM
  l_xekek[] = l_doc-xekek[].    ""KDM
* l_xaend[]    = l_doc-xaend[].

  call function lf_fm_name
    exporting
     archive_index              = toa_dara
*   ARCHIVE_INDEX_TAB          =
     archive_parameters         = arc_params
     control_parameters         = ls_control_param
*   MAIL_APPL_OBJ              =
     mail_recipient             = ls_recipient
     mail_sender                = ls_sender
     output_options             = ls_composer_param
     user_settings              = ' '
      zxekko                     = l_zekko
      zxpekko                    = l_xpekko
*   l_xaend                    = l_xaend
*    IMPORTING
*   DOCUMENT_OUTPUT_INFO       =
*   JOB_OUTPUT_INFO            =
*   JOB_OUTPUT_OPTIONS         =
    tables
      l_xekpo                    = l_xekpo
      l_xekpa                    = l_xekpa
      l_xpekpo                   = l_xpekpo
      l_xeket                    = l_xeket
      l_xtkomv                   = l_xtkomv
      l_xekkn                    = l_xekkn
      l_xekek                    = l_xekek
      l_xkomk                    = l_xkomk
*    l_xaend                    = l_xaend
* EXCEPTIONS
*   FORMATTING_ERROR           = 1
*   INTERNAL_ERROR             = 2
*   SEND_ERROR                 = 3
*   USER_CANCELED              = 4
*   OTHERS                     = 5
            .
  if sy-subrc <> 0.
    ent_retco = sy-subrc.
    perform protocol_update_i.
    perform add_smfrm_prot.
  endif.
endform.

**********************************************************
*form entry_lpf2_new for lpf2
************************************************************

form entry_lpf2_new using ent_retco ent_screen.
  data: l_druvo like t166k-druvo,
         l_nast  like nast,
         l_from_memory,
         l_doc   type meein_purchase_doc_print.

  xscreen = ent_screen.
  xlpet  = 'X'.
  if nast-aende eq space.
    xdruvo = '5'.
  else.
    xdruvo = '8'.
  endif.
  xfz    = 'X'.
  xoffen = 'X'.
  clear: xlmahn.
*- Anstoﬂ Verarbeitung ------------------------------------------------*
  clear ent_retco.
  call function 'ME_READ_PO_FOR_PRINTING'
       exporting
            ix_nast        = nast
            ix_screen      = ent_screen
       importing
            ex_retco       = ent_retco
            ex_nast        = l_nast
            doc            = l_doc
       changing
            cx_druvo       = l_druvo
            cx_from_memory = l_from_memory.
  check ent_retco eq 0.
  if nast-adrnr is initial.
    perform get_addr_key
                         changing ls_addr_key.
  else.
    ls_addr_key = nast-adrnr.
  endif.
  perform set_print_param using      ls_addr_key
                            changing ls_control_param
                                     ls_composer_param
                                     ls_recipient
                                     ls_sender
                                     ent_retco.


*Get the Smart Form name.
  if not tnapr-sform is initial.
    lf_formname = tnapr-sform.
  else.
    lf_formname = tnapr-fonam.
  endif.

* determine smartform function module for invoice
  call function 'SSF_FUNCTION_MODULE_NAME'
       exporting  formname           = lf_formname
*                 variant            = ' '
*                 direct_call        = ' '
       importing  fm_name            = lf_fm_name
       exceptions no_form            = 1
                  no_function_module = 2
                  others             = 3.
  if sy-subrc <> 0.
*   error handling
    ent_retco = sy-subrc.
    perform protocol_update_i.
  endif.

* move the value
  move-corresponding l_doc-xekko to l_zekko.
  move-corresponding l_doc-xpekko to l_xpekko.
  l_xekpo[] = l_doc-xekpo[].
  l_xekpa[] = l_doc-xekpa[].
  l_xpekpo[] = l_doc-xpekpo[].
  l_xeket[] = l_doc-xeket[].
  l_xtkomv[] = l_doc-xtkomv[].
  l_xekkn[] = l_doc-xekkn[].
*  l_xekek[] = l_doc-xekeh[].    ""KDM
  l_xekek[] = l_doc-xekek[].    ""KDM
*  l_xaend[]    = l_doc-xaend[].

  call function lf_fm_name
    exporting
     archive_index              = toa_dara
*   ARCHIVE_INDEX_TAB          =
     archive_parameters         = arc_params
     control_parameters         = ls_control_param
*   MAIL_APPL_OBJ              =
     mail_recipient             = ls_recipient
     mail_sender                = ls_sender
     output_options             = ls_composer_param
     user_settings              = ' '
      zxekko                     = l_zekko
      zxpekko                    = l_xpekko
*   l_xaend                    = l_xaend
*    IMPORTING
*   DOCUMENT_OUTPUT_INFO       =
*   JOB_OUTPUT_INFO            =
*   JOB_OUTPUT_OPTIONS         =
    tables
      l_xekpo                    = l_xekpo
      l_xekpa                    = l_xekpa
      l_xpekpo                   = l_xpekpo
      l_xeket                    = l_xeket
      l_xtkomv                   = l_xtkomv
      l_xekkn                    = l_xekkn
      l_xekek                    = l_xekek
      l_xkomk                    = l_xkomk
*    l_xaend                    = l_xaend
* EXCEPTIONS
*   FORMATTING_ERROR           = 1
*   INTERNAL_ERROR             = 2
*   SEND_ERROR                 = 3
*   USER_CANCELED              = 4
*   OTHERS                     = 5
            .
  if sy-subrc <> 0.
    ent_retco = sy-subrc.
    perform protocol_update_i.
    perform add_smfrm_prot.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  set_print_param
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_ADDR_KEY  text
*      <--P_LS_CONTROL_PARAM  text
*      <--P_LS_COMPOSER_PARAM  text
*      <--P_LS_RECIPIENT  text
*      <--P_LS_SENDER  text
*      <--P_CF_RETCODE  text
*----------------------------------------------------------------------*
form set_print_param using    is_addr_key like addr_key
                     changing cs_control_param type ssfctrlop
                              cs_composer_param type ssfcompop
                              cs_recipient type  swotobjid
                              cs_sender type  swotobjid
                              cf_retcode type sy-subrc.

  data: ls_itcpo     type itcpo.
  data: lf_repid     type sy-repid.
  data: lf_device    type tddevice.
  data: ls_recipient type swotobjid.
  data: ls_sender    type swotobjid.

  lf_repid = sy-repid.

  call function 'WFMC_PREPARE_SMART_FORM'
       exporting
            pi_nast       = nast
            pi_addr_key   = is_addr_key
            pi_repid      = lf_repid
       importing
            pe_returncode = cf_retcode
            pe_itcpo      = ls_itcpo
            pe_device     = lf_device
            pe_recipient  = cs_recipient
            pe_sender     = cs_sender.

  if cf_retcode = 0.
    move-corresponding ls_itcpo to cs_composer_param.
    cs_control_param-device      = lf_device.
    cs_control_param-no_dialog   = 'X'.
    cs_control_param-preview     = xscreen.
    cs_control_param-getotf      = ls_itcpo-tdgetotf.
    cs_control_param-langu       = nast-spras.
  endif.
endform.

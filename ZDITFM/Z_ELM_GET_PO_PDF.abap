FUNCTION Z_ELM_GET_PO_PDF.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(EBELN) TYPE  EBELN
*"     VALUE(SCREEN) TYPE  CHAR1 OPTIONAL
*"  EXPORTING
*"     VALUE(O_PDF) TYPE  XSTRING
*"  TABLES
*"      ZELM_CONTRACT STRUCTURE  ZELM_CONTRACT
*"      RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------

  data ls_addr_key           like addr_key.
  data ent_retco(1).
  data ls_control_param      type ssfctrlop.
  data ls_recipient          type swotobjid.
  data ls_sender             type swotobjid.
  data ls_composer_param     type ssfcompop.

  DATA: lt_otf      LIKE itcoo      OCCURS 0 WITH HEADER LINE,
        lt_lines    LIKE tline      OCCURS 0 WITH HEADER LINE.

  data: lv_pdf_len type i,
        ls_pdf_xstring type xstring.

  data: l_druvo type DRUVO,
        l_nast  like nast,
        l_from_memory,
        l_doc   type meein_purchase_doc_print,
        l_xkomk like table of komk with header line.

  data: lf_fm_name            type rs38l_fnam.
  data: l_zekko                  like ekko,
        l_xpekko                 like pekko,
        l_xekpo                like table of ekpo,
        l_wa_xekpo             like ekpo.

  data: l_xekpa like ekpa occurs 0,
        l_wa_xekpa like ekpa.
  data: l_xpekpo  like pekpo occurs 0,
        l_wa_xpekpo like pekpo,
        l_xeket   like table of eket with header line,
        l_xekkn  like table of ekkn with header line,
        l_xekek  like table of ekek with header line,
        l_xekeh   like table of ekeh with header line,
        l_xtkomv  type komv occurs 0,
        l_wa_xtkomv type komv.

  data: ls_job_info TYPE ssfcrescl.

  select single * from nast where kappl = 'EF'
                       and objky = ebeln.
  CALL FUNCTION 'ME_READ_PO_FOR_PRINTING'
    EXPORTING
      ix_nast   = nast
      ix_screen = 'X'
    IMPORTING
      ex_retco  = ent_retco
      ex_nast   = l_nast
      doc       = l_doc
    CHANGING
      cx_druvo  = l_druvo.

  if ent_retco eq 0.
    if nast-adrnr is initial.
      perform get_addr_key
                           changing ls_addr_key.
    else.
      ls_addr_key = nast-adrnr.
    endif.
  endif.

  move-corresponding l_doc-xekko to l_zekko.
  move-corresponding l_doc-xpekko to l_xpekko.

  l_xekpo[] = l_doc-xekpo[].
  l_xekpa[] = l_doc-xekpa[].
  l_xpekpo[] = l_doc-xpekpo[].
  l_xeket[] = l_doc-xeket[].
  l_xtkomv[] = l_doc-xtkomv[].
  l_xekkn[] = l_doc-xekkn[].
  l_xekek[] = l_doc-xekek[].    ""KDM

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZMM_PUR_ORD'
    IMPORTING
      fm_name            = lf_fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      others             = 3.

  LS_CONTROL_PARAM-NO_DIALOG = 'X'.
  if screen eq space.
    LS_CONTROL_PARAM-getotf = 'X'.
  endif.

  LS_CONTROL_PARAM-PREVIEW = 'X'.

  ls_composer_param-TDARMOD = '1'.
  ls_composer_param-TDDEST = 'LOCM'.
  ls_composer_param-tdimmed = 'X'.

  ls_composer_param-tddelete = 'X'.
  ls_composer_param-tdlifetime = '0'.
  ls_composer_param-TDCOPIES = '001'.

  ls_composer_param-tdreceiver = 'HMMABACK_MM'.

  CLEAR ls_job_info.
  CALL FUNCTION lf_fm_name
    EXPORTING
      control_parameters = ls_control_param
      output_options     = ls_composer_param
      user_settings      = ' '
      zxekko             = l_doc-xekko
      zxpekko            = l_doc-xpekko
    IMPORTING
      job_output_info    = ls_job_info
    TABLES
      l_xekpo            = l_doc-xekpo[]
      l_xekpa            = l_doc-xekpa[]
      l_xpekpo           = l_doc-xpekpo[]
      l_xeket            = l_doc-xeket[]
      l_xtkomv           = l_doc-xtkomv[]
      l_xekkn            = l_doc-xekkn[]
      l_xekek            = l_doc-xekek[]
      l_xkomk            = l_xkomk
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      others             = 5.

  IF sy-subrc <> 0.
    RETURN-TYPE = 'E'.
    RETURN-MESSAGE = 'Error!'.
    APPEND RETURN.
    exit.
  ENDIF.

  REFRESH lt_otf.
  lt_otf[] = ls_job_info-otfdata[].

*&----------------------------------------------------------------------
*& Convert to PDF
*&----------------------------------------------------------------------
  REFRESH lt_lines.
  CLEAR: ls_pdf_xstring, lv_pdf_len.

  CALL FUNCTION 'CONVERT_OTF'
    EXPORTING
      FORMAT                = 'PDF'
    IMPORTING
      BIN_FILESIZE          = lv_pdf_len
      BIN_FILE              = ls_pdf_xstring
    TABLES
      OTF                   = lt_otf
      LINES                 = lt_lines
    EXCEPTIONS
      ERR_MAX_LINEWIDTH     = 1
      ERR_FORMAT            = 2
      ERR_CONV_NOT_POSSIBLE = 3
      ERR_BAD_OTF           = 4
      OTHERS                = 5.

  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

  o_pdf = ls_pdf_xstring.

  RETURN-TYPE = 'S'.
  RETURN-MESSAGE = 'Success!'.
  APPEND RETURN.

ENDFUNCTION.

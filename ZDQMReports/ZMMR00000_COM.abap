*&---------------------------------------------------------------------*
*&  Include           ZKEMMR04000_COM
*&---------------------------------------------------------------------*

*---------------------------------------------------------------------*
*       FORM MONAT_F4                                                 *
*---------------------------------------------------------------------*
*       F4-Hilfe fur Monat                                            *
*---------------------------------------------------------------------*
FORM  monat_f4.
  DATA: BEGIN OF mf_dynpfields OCCURS 1.
          INCLUDE STRUCTURE dynpread.
  DATA: END   OF mf_dynpfields.
  DATA: mf_returncode   LIKE sy-subrc,
        mf_monat        LIKE isellist-month,
        mf_hlp_repid    LIKE sy-repid.
  FIELD-SYMBOLS: <mf_feld>.

* Wert von Dynpro lesen
  GET CURSOR FIELD mf_dynpfields-fieldname.
  APPEND mf_dynpfields.
  mf_hlp_repid = sy-repid.
  DO 2 TIMES.
    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname               = mf_hlp_repid
        dynumb               = sy-dynnr
      TABLES
        dynpfields           = mf_dynpfields
      EXCEPTIONS
        invalid_abapworkarea = 01
        invalid_dynprofield  = 02
        invalid_dynproname   = 03
        invalid_dynpronummer = 04
        invalid_request      = 05
        no_fielddescription  = 06
        undefind_error       = 07.
    IF sy-subrc = 3.
*     Aktuelles Dynpro ist Wertemengenbild
      mf_hlp_repid = 'SAPLALDB'.
    ELSE.
      READ TABLE mf_dynpfields INDEX 1.
*     Unterstriche durch Blanks ersetzen
      TRANSLATE mf_dynpfields-fieldvalue USING '_ '.
      EXIT.
    ENDIF.
  ENDDO.
  IF sy-subrc = 0.
*   Konvertierung ins interne Format
    CALL FUNCTION 'CONVERSION_EXIT_PERI_INPUT'
      EXPORTING
        input         = mf_dynpfields-fieldvalue
      IMPORTING
        output        = mf_monat
      EXCEPTIONS
        error_message = 1.
    IF mf_monat IS INITIAL.
*     Monat ist initial => Vorschlagswert aus akt. Datum ableiten
      mf_monat = sy-datlo(6).
    ENDIF.
    CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
      EXPORTING
        actual_month               = mf_monat
      IMPORTING
        selected_month             = mf_monat
        return_code                = mf_returncode
      EXCEPTIONS
        factory_calendar_not_found = 01
        holiday_calendar_not_found = 02
        month_not_found            = 03.
    IF sy-subrc = 0 AND mf_returncode = 0.
*     ASSIGN (MF_DYNPFIELDS-FIELDNAME) TO <MF_FELD>. " ==>> note 148804
*     <MF_FELD> = MF_MONAT.
      CALL FUNCTION 'CONVERSION_EXIT_PERI_OUTPUT'
        EXPORTING
          input  = mf_monat
        IMPORTING
          output = mf_dynpfields-fieldvalue.
      COLLECT mf_dynpfields.
      CALL FUNCTION 'DYNP_VALUES_UPDATE'
        EXPORTING
          dyname               = mf_hlp_repid
          dynumb               = sy-dynnr
        TABLES
          dynpfields           = mf_dynpfields
        EXCEPTIONS
          invalid_abapworkarea = 01
          invalid_dynprofield  = 02
          invalid_dynproname   = 03
          invalid_dynpronummer = 04
          invalid_request      = 05
          no_fielddescription  = 06
          undefind_error       = 07.           "<<== note 148804
    ENDIF.
  ENDIF.
ENDFORM.                                                    "MONAT_F4

*&---------------------------------------------------------------------*
*&      Form  update_screen_field
*&---------------------------------------------------------------------*
FORM update_screen_field  USING    value(p_fieldname)
      p_value LIKE dynpread-fieldvalue
      p_stepl LIKE dynpread-stepl.


  DATA: fields     LIKE dynpread OCCURS 10 WITH  HEADER LINE.

  REFRESH: fields.
  fields-fieldname   = p_fieldname.
  fields-fieldvalue  = p_value.
  fields-stepl       = p_stepl.
  APPEND fields.
*
  CALL FUNCTION 'DYNP_UPDATE_FIELDS'
    EXPORTING
      dyname               = sy-repid
      dynumb               = sy-dynnr
      request              = 'A'
    TABLES
      dynpfields           = fields
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      undefind_error       = 7
      OTHERS               = 8.

ENDFORM.                    " update_screen_field
*&---------------------------------------------------------------------*
*&      Form  select_all_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM select_all_item .

  IF g_check       = 'X'.
    g_check             = ''.
    gt_display-check    = ''.
    MODIFY gt_display TRANSPORTING check WHERE check  EQ 'X'.
  ELSE.
    gt_display-check   = 'X'.
    g_check            = 'X'.
    MODIFY gt_display TRANSPORTING check WHERE check  EQ ' '.
  ENDIF.

ENDFORM.                    " select_all_item

*&---------------------------------------------------------------------*
*&      Form  CALL_MESSAGE_SCREEN.
*&---------------------------------------------------------------------*
*       ???? ??? ???.
*----------------------------------------------------------------------*
FORM call_message_screen.
  DATA lc_message_count TYPE i.
  DESCRIBE TABLE gt_bdcmsg LINES lc_message_count.
  CHECK lc_message_count > 0.
  CALL SCREEN 2000 STARTING AT 10 10
  ENDING   AT 89 25.

ENDFORM.                    " CALL_MESSAGE_SCREEN.
*&---------------------------------------------------------------------*
*&      Module  LIST_MESSAGES  OUTPUT
*&---------------------------------------------------------------------*
*       ?? ??? ???.
*----------------------------------------------------------------------*
MODULE list_messages OUTPUT.
  PERFORM show_messages.
ENDMODULE.                 " LIST_MESSAGES  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  SHOW_MESSAGES
*&---------------------------------------------------------------------*
*       ???? ??? ???.
*----------------------------------------------------------------------*
FORM show_messages.
  SET PF-STATUS space.
  SET TITLEBAR 'TT_MESSAGE'.
  SUPPRESS DIALOG.
  LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
  PERFORM write_message_top.
  PERFORM write_message_line.
ENDFORM.                    " SHOW_MESSAGES
*&---------------------------------------------------------------------*
*&      Form  WRITE_MESSAGE_TOP
*&---------------------------------------------------------------------*
*       ??? ?? ??.
*----------------------------------------------------------------------*
FORM write_message_top.
  FORMAT COLOR COL_HEADING.
  WRITE: /03(02) text-901, "'St',
  06(20) text-902, "'Message ID',
  27(03) text-903, "'No.'
  31(50) text-904. "'Message'.
  ULINE.
ENDFORM.                    " WRITE_MESSAGE_TOP
*&---------------------------------------------------------------------*
*&      Form  WRITE_MESSAGE_LINE
*&---------------------------------------------------------------------*
*       ??? ?? ??.
*----------------------------------------------------------------------*
FORM write_message_line.
  DATA: lc_message TYPE bapiret2-message,
        lc_odd.
  LOOP AT gt_bdcmsg.
    AT NEW zrsnum.
      WRITE: /01(20) gt_bdcmsg-zrsnum COLOR COL_KEY
      INTENSIFIED ON.
      CASE gt_bdcmsg-updkz.
        WHEN 'I'.
          WRITE: 22(02) icon_create AS ICON.
        WHEN 'U'.
          WRITE: 22(02) icon_change AS ICON.
        WHEN 'D'.
          WRITE: 22(02) icon_delete AS ICON.
      ENDCASE.
    ENDAT.
    IF lc_odd IS INITIAL.
      lc_odd = 'X'.
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    ELSE.
      CLEAR lc_odd.
      FORMAT COLOR COL_NORMAL INTENSIFIED ON.
    ENDIF.
    CASE gt_bdcmsg-type.
      WHEN 'S'.
        WRITE: /03(02) icon_led_green  AS ICON.
      WHEN 'W'.
        WRITE: /03(02) icon_led_yellow AS ICON.
      WHEN 'E' OR 'A' OR 'X'.
        WRITE: /03(02) icon_led_red    AS ICON.
      WHEN OTHERS.
        WRITE: /03(02) icon_space      AS ICON.
    ENDCASE.
*    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
*      EXPORTING
*        msgid               = gt_bdcmsg-id
*        msgnr               = gt_bdcmsg-number
*        msgv1               = gt_bdcmsg-message_v1
*        msgv2               = gt_bdcmsg-message_v2
*        msgv3               = gt_bdcmsg-message_v3
*        msgv4               = gt_bdcmsg-message_v4
*      IMPORTING
*        message_text_output = lc_message.


    WRITE: 06(20)  gt_bdcmsg-id,
           27(03)  gt_bdcmsg-number,
           31(150) gt_bdcmsg-message.
  ENDLOOP.
ENDFORM.                    " WRITE_MESSAGE_LINE

*---------------------------------------------------------------------*
*       FORM f4help_layout                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  CS_LAYOUT                                                     *
*---------------------------------------------------------------------*
FORM f4help_layout CHANGING cs_layout LIKE disvariant-variant.

  DATA: ls_variant LIKE disvariant.

  ls_variant-report      = sy-repid.
  ls_variant-handle      = space.
  ls_variant-log_group   = space.
  ls_variant-username    = space.
  ls_variant-variant     = cs_layout.
  ls_variant-text        = space.
  ls_variant-dependvars  = space.

  DATA: lt_dynpfields LIKE dynpread OCCURS 0 WITH HEADER LINE.

  CLEAR lt_dynpfields.
  lt_dynpfields-fieldname = 'P_LAYOUT'.
  APPEND lt_dynpfields.

  READ TABLE lt_dynpfields WITH KEY fieldname = 'P_LAYOUT'.
  IF sy-subrc EQ 0.
    IF NOT lt_dynpfields-fieldvalue(1) CA '0123456789/' AND
    NOT lt_dynpfields-fieldvalue IS INITIAL.
      ls_variant-username = sy-uname.
    ENDIF.
    ls_variant-variant = lt_dynpfields-fieldvalue.
  ENDIF.

  DATA: lt_fcat TYPE lvc_t_fcat.
  CALL FUNCTION 'LVC_VARIANT_SAVE_LOAD'
    EXPORTING
      i_save_load = 'F'
      i_tabname   = '1'
    CHANGING
      cs_variant  = ls_variant
      ct_fieldcat = lt_fcat[].

  cs_layout = ls_variant-variant.

ENDFORM.                    "f4help_layout
*&---------------------------------------------------------------------*
*&      Form  POV_CLEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM pov_clear .

  CLEAR : g_selectfield,
  g_it_fields, g_it_fields[],
  g_select_value,
  g_ld_tabix,
  g_fields, g_fields[].

  CLEAR : gs_fields, gs_fields[],
  gs_dynpfields, gs_dynpfields[],
  gt_valuetab, gt_valuetab[],
  gs_select_values, gs_select_values[].

ENDFORM.                    " POV_CLEAR

**&---------------------------------------------------------------------
**
**&      Form  ADD_FIELDS
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*FORM add_fields USING p_tabname p_fieldname p_flag.
*
*  gs_fields-tabname    = p_tabname.
*  gs_fields-fieldname  = p_fieldname.
*  gs_fields-selectflag = p_flag.
*
*  APPEND gs_fields.  CLEAR gs_fields.
*
*ENDFORM.                    " ADD_FIELDS
*
**&---------------------------------------------------------------------
**
**&      Form  HELP_VALUES_GET
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*FORM help_values_get .
*
*  CLEAR g_lno.
*  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
*    EXPORTING
*      display                   = ' '
*    IMPORTING
*      index                     = g_lno
*    TABLES
*      fields                    = gs_fields
*      select_values             = gs_select_values
*      valuetab                  = gt_valuetab
*    EXCEPTIONS
*      field_not_in_ddic         = 1
*      more_then_one_selectfield = 2
*      no_selectfield            = 3
*      OTHERS                    = 4.
*
*ENDFORM.                    " HELP_VALUES_GET
*
**&---------------------------------------------------------------------
**
**&      Form  VALUE_UPDATE
**&---------------------------------------------------------------------
**
*FORM value_update USING p_process p_fieldname p_fieldvalue p_stepl.
*
*  gs_dynpfields-fieldname  = p_fieldname.
*  gs_dynpfields-fieldvalue = p_fieldvalue.
*
*  IF p_stepl > 0.
*    gs_dynpfields-stepl = p_stepl.
*  ENDIF.
*  APPEND gs_dynpfields.
*  CLEAR  gs_dynpfields.
*
*  IF p_process = 'X'.
*    CALL FUNCTION 'DYNP_VALUES_UPDATE'
*      EXPORTING
*        dyname               = sy-cprog
*        dynumb               = sy-dynnr
*      TABLES
*        dynpfields           = gs_dynpfields
*      EXCEPTIONS
*        invalid_abapworkarea = 1
*        invalid_dynprofield  = 2
*        invalid_dynproname   = 3
*        invalid_dynpronummer = 4
*        invalid_request      = 5
*        no_fielddescription  = 6
*        undefind_error       = 7
*        OTHERS               = 8.
*  ENDIF.
*
*ENDFORM.                    " VALUE_UPDATE

**&---------------------------------------------------------------------
**
**&      Form  VALUE_READ
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**      -->P_ENDIF  text
**----------------------------------------------------------------------
**
*FORM value_read  TABLES pt_dynpread STRUCTURE dynpread
*USING pi_name pi_dynnr .
*
*  pt_dynpread-fieldname = pi_name. APPEND pt_dynpread.
*
*  CALL FUNCTION 'DYNP_VALUES_READ'
*    EXPORTING
*      dyname     = sy-cprog
*      dynumb     = pi_dynnr
*    TABLES
*      dynpfields = pt_dynpread.
*  IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*ENDFORM.                    " VALUE_READ
*&---------------------------------------------------------------------*
*&      Form  message_popup_screen
*&---------------------------------------------------------------------*
*       text : Popup message screen
*----------------------------------------------------------------------*
*      -->P_TEXT_200  text    : Title description
*      -->P_TEXT_201  text    : Body description
*      <--P_G_CONTINUE  text  : Continue flag
*----------------------------------------------------------------------*
FORM message_popup_screen   USING    p_title p_body
CHANGING p_continue.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
  EXPORTING
    titlebar                    = p_title
*    DIAGNOSE_OBJECT             = ' '
    text_question               = p_body
    text_button_1               = 'YES'
    icon_button_1               = 'ICON_OKAY'
    text_button_2               = 'NO'
    icon_button_2               = 'ICON_CANCEL'
*    DEFAULT_BUTTON              = '1'
    display_cancel_button       = ' '
*    USERDEFINED_F1_HELP         = ' '
*    START_COLUMN                = ' '
*    START_ROW                   = 6
*   POPUP_TYPE                  =
  IMPORTING
    answer                      = p_continue.

  CASE p_continue.
    WHEN '1'.

    WHEN '2'.

  ENDCASE.
ENDFORM.                    " MESSAGE_POPUP_SCREEN

*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM dynpro USING  dynbegin fnam fval.

  CLEAR gt_bdctab.

  IF dynbegin = 'X'.

    gt_bdctab-dynbegin = 'X'.
    gt_bdctab-program = fnam.
    gt_bdctab-dynpro = fval.
    APPEND gt_bdctab.

  ELSE.

    gt_bdctab-fnam = fnam.
    gt_bdctab-fval = fval.
    APPEND gt_bdctab.

  ENDIF.

ENDFORM.                    "dynpro


*&---------------------------------------------------------------------*
*&      Form  f4_WERKS
*&---------------------------------------------------------------------*
FORM f4_werks  USING    u_field.

  PERFORM search_werks_data.
  PERFORM show_f4_data  USING 'KEY'   u_field
        'F4_HELP[]'.
ENDFORM.                                                    " f4_werks

**&---------------------------------------------------------------------
**
**&      Form  f4_lgort
**&---------------------------------------------------------------------
**
*FORM f4_lgort  USING    u_field.
*  DATA : l_werks TYPE t001w-werks.
*  PERFORM value_read TABLES lt_dynpread USING: 'S_WERKS-LOW' sy-dynnr .
*  READ TABLE lt_dynpread INDEX 1 .
*  IF sy-subrc EQ 0 .
*    l_werks = lt_dynpread-fieldvalue .
*  ENDIF.
*
*  PERFORM search_lgort_data USING l_werks.
*  PERFORM show_f4_data  USING 'KEY'   u_field
*        'F4_HELP[]'.
*ENDFORM.                                                    " f4_werks


*&---------------------------------------------------------------------*
*&      Form  show_f4_data
*&---------------------------------------------------------------------*
FORM show_f4_data  USING    u_retfield  u_dynprofield  u_f4tabname.
  ASSIGN (u_f4tabname) TO <ft1>.
  IF <ft1> IS NOT INITIAL.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield    = u_retfield
        dynpprog    = sy-repid
        dynpnr      = sy-dynnr
        dynprofield = u_dynprofield
        value_org   = 'S'
      TABLES
        value_tab   = <ft1>.
  ELSE.
    MESSAGE s801(dh).
  ENDIF.
ENDFORM.                    " show_f4_data
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_EXIT_ABPSP_INPUT
*&---------------------------------------------------------------------*
FORM conversion_exit_abpsp_input  USING     p_psp_pnr
CHANGING  p_pspnr.

  CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
    EXPORTING
      input     = p_psp_pnr
    IMPORTING
      output    = p_pspnr
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.

ENDFORM.                    " CONVERSION_EXIT_ABPSP_INPUT
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_EXIT_ABPSP_OUTPUT
*&---------------------------------------------------------------------*
FORM conversion_exit_abpsp_output  USING    p_psp_pnr
CHANGING p_pspnr.

  CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
    EXPORTING
      input  = p_psp_pnr
    IMPORTING
      output = p_pspnr.

ENDFORM.                    " CONVERSION_EXIT_ABPSP_INPUT
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_EXIT_ABPSN_INPUT
*&---------------------------------------------------------------------*
FORM conversion_exit_abpsn_input  USING     p_psp_pnr
CHANGING  p_pspnr.

  CALL FUNCTION 'CONVERSION_EXIT_ABPSN_INPUT'
    EXPORTING
      input  = p_psp_pnr
    IMPORTING
      output = p_pspnr.

ENDFORM.                    " CONVERSION_EXIT_ABPSN_INPUT
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_EXIT_ABPSN_OUTPUT
*&---------------------------------------------------------------------*
FORM conversion_exit_abpsn_output  USING    p_psp_pnr
CHANGING p_pspnr.

  CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
    EXPORTING
      input  = p_psp_pnr
    IMPORTING
      output = p_pspnr.

ENDFORM.                    " CONVERSION_EXIT_ABPSN_INPUT

*&---------------------------------------------------------------------*
*&      Form  conversion_exit_konpd_input
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PSPNR  text
*      <--P_PSPHI  text
*----------------------------------------------------------------------*
FORM conversion_exit_konpd_input  USING    p_pspnr
CHANGING p_psphi.

  CALL FUNCTION 'CONVERSION_EXIT_KONPD_INPUT'
    EXPORTING
      input     = p_pspnr
    IMPORTING
      output    = p_psphi
    EXCEPTIONS
      not_found = 1.

ENDFORM.                    " conversion_exit_konpd_input
*&---------------------------------------------------------------------*
*&      Form  conversion_exit_konpd_output
*&---------------------------------------------------------------------*
FORM conversion_exit_konpd_output  USING    p_pspnr
CHANGING p_psphi.

  CALL FUNCTION 'CONVERSION_EXIT_KONPD_OUTPUT'
    EXPORTING
      input  = p_pspnr
    IMPORTING
      output = p_psphi.

ENDFORM.                    " conversion_exit_konpd_output
*&---------------------------------------------------------------------*
*&      Form  conversion_exit_ALPHA_input
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_INPUT
*      <--P_OUTPUT
*----------------------------------------------------------------------*
FORM conversion_exit_alpha_input  USING    p_input
CHANGING p_output.


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_input
    IMPORTING
      output = p_output.

ENDFORM.                    " conversion_exit_ALPHA_input
*&---------------------------------------------------------------------*
*&      Form  conversion_exit_ALPHA_input
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_INPUT
*      <--P_OUTPUT
*----------------------------------------------------------------------*
FORM conversion_exit_alpha_output  USING    p_input
CHANGING p_output.


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_input
    IMPORTING
      output = p_output.

ENDFORM.                    " conversion_exit_ALPHA_OUTput
*&---------------------------------------------------------------------*
*&      Form  get_maktx_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MATNR  text
*      <--P_TXZ01  text
*----------------------------------------------------------------------*
FORM get_maktx_data  USING    p_matnr
                              p_spras
                     CHANGING p_txz01.

  SELECT SINGLE maktx INTO p_txz01
        FROM makt
        WHERE spras = p_spras
         AND  matnr = p_matnr.

ENDFORM.                    " get_maktx_data

*&---------------------------------------------------------------------*
*&      Form  coversion_of_currency
*&---------------------------------------------------------------------*
FORM coversion_of_currency  USING    p_money
      p_key
CHANGING p_text.
  WRITE p_money TO p_text CURRENCY p_key.

  CALL FUNCTION 'CATS_NUMERIC_INPUT_CHECK'
  EXPORTING
    input            = p_text
*   INTERNAL         = 'X'
  IMPORTING
    output           = p_text
  EXCEPTIONS
    no_numeric       = 1
    OTHERS           = 2 .

ENDFORM.                    " coversion_of_currency
*&---------------------------------------------------------------------*
*&      Form  GET_ENAME_DATA
*&---------------------------------------------------------------------*
FORM get_ename_data  USING    p_pernr
CHANGING p_ename.


  SELECT SINGLE ename INTO p_ename
  FROM pa0001
  WHERE pernr = p_pernr
  AND endda = '99991231'.


ENDFORM.                    " GET_ENAME_DATA
*&---------------------------------------------------------------------*
*&      Form  get_lifnr_name_data
*&---------------------------------------------------------------------*
FORM get_lifnr_name_data  USING    p_lifnr
CHANGING p_name1.


  SELECT SINGLE name1
  INTO p_name1
  FROM lfa1
  WHERE lifnr = p_lifnr.

ENDFORM.                    " get_lifnr_name_data
*&---------------------------------------------------------------------*
*&      Form  get_domain_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0023   text
*      -->P_GW_HEAD_STATU  text
*----------------------------------------------------------------------*
FORM get_domain_value  USING p_domain
                             p_value
                    CHANGING p_statu_tx.
  SELECT SINGLE ddtext
     INTO  p_statu_tx
    FROM dd07t
  WHERE domname    = p_domain
    AND domvalue_l = p_value
    AND ddlanguage = sy-langu.

ENDFORM.                    " get_domain_value
*&---------------------------------------------------------------------*
*&      Form  read_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0757   text
*      -->P_IT_LIST_LIFNR  text
*      <--P_IT_LIST_LIFNX  text
*----------------------------------------------------------------------*
FORM read_text  USING    u_id
                         u_value
                CHANGING c_text.
  CASE u_id.
    WHEN 'MATNR'.
      SELECT SINGLE maktx
      FROM makt
      INTO c_text
      WHERE matnr = u_value
      AND spras = sy-langu.
    WHEN 'KUNNR'.
      SELECT SINGLE name1
      INTO c_text
      FROM kna1
      WHERE kunnr = u_value.
    WHEN 'INTST'.
      SELECT SINGLE intstt
      FROM cjit04t
      INTO c_text
      WHERE intst = u_value
      AND spras = sy-langu.
  ENDCASE.

ENDFORM.                    " read_text
*&---------------------------------------------------------------------*
*&      Form  p0000_indicator
*&---------------------------------------------------------------------*
FORM p0000_indicator .

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = percentage
      text       = text.

ENDFORM.                    " p0000_indicator

*&---------------------------------------------------------------------*
*&      Form  search_locn_data
*&---------------------------------------------------------------------*
FORM search_werks_data.

  DATA : lt_t001w LIKE t001w OCCURS 0 WITH HEADER LINE.

  CLEAR : lt_t001w, lt_t001w[],
  f4_help, f4_help[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_t001w
  FROM t001w
  WHERE fabkl = 'ES'.

  LOOP AT lt_t001w.
    CLEAR f4_help.
    f4_help-key   = lt_t001w-werks.
    f4_help-text  = lt_t001w-name1.
    APPEND f4_help.
  ENDLOOP.

ENDFORM.                    " search_werks_data
*&---------------------------------------------------------------------*
*&      Form  search_lgort_data
*&---------------------------------------------------------------------*
FORM search_lgort_data USING p_werks.

  DATA : lt_t001l LIKE t001l OCCURS 0 WITH HEADER LINE.

  CLEAR : lt_t001l, lt_t001l[],
  f4_help, f4_help[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_t001l
  FROM t001l
  WHERE werks = p_werks.

  LOOP AT lt_t001l.
    CLEAR f4_help.
    f4_help-key   = lt_t001l-lgort.
    f4_help-text  = lt_t001l-lgobe.
    APPEND f4_help.
  ENDLOOP.

ENDFORM.                    " search_werks_data

*&---------------------------------------------------------------------*
*&      Form  search_MATNR_data
*&---------------------------------------------------------------------*
FORM search_matnr_data USING p_atnam.

  DATA : lt_makt LIKE makt OCCURS 0 WITH HEADER LINE,
        l_matnr LIKE makt-matnr.

  CONCATENATE p_atnam '%' INTO l_matnr.

  CLEAR : lt_makt, lt_makt[],
  f4_help_m, f4_help_m[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_makt
  FROM makt
  WHERE matnr LIKE l_matnr
  AND spras = sy-langu.

  LOOP AT lt_makt.
    CLEAR f4_help_m.
    f4_help_m-key   = lt_makt-matnr.
    f4_help_m-text  = lt_makt-maktx.
    APPEND f4_help_m.
  ENDLOOP.

ENDFORM.                    " search_MATNR_data

*&---------------------------------------------------------------------*
*&      Form  display_error_log
*&---------------------------------------------------------------------*
FORM display_error_log .

*  IF NOT gt_return[] IS INITIAL.
*    LOOP AT gt_return.
*      MOVE-CORRESPONDING gt_return TO gt_bdcmsg.
*      gt_bdcmsg-zrsnum = gt_display-zrsnum.
*      APPEND gt_bdcmsg. CLEAR gt_bdcmsg.
*    ENDLOOP.
*
*  ENDIF.

ENDFORM.                    " display_error_log
*&---------------------------------------------------------------------*
*&      Form  GET_CC_TEXT-DATA
*&---------------------------------------------------------------------*
FORM get_cc_text-data  USING    p_kostl
                       CHANGING p_ktext.

  CHECK p_kostl NE ' '.

  SELECT SINGLE ktext INTO p_ktext
                      FROM cskt
                           WHERE kostl  = p_kostl
                           AND   kokrs  = 'K201'
                           AND   spras  = 'EN'.

ENDFORM.                    " GET_CC_TEXT-DATA
*&---------------------------------------------------------------------*
*&      Form  GET_COMPANY_DATA
*&---------------------------------------------------------------------*

FORM get_company_data  USING    p_lifnr
                       CHANGING p_akont
                                p_zwels.

  SELECT SINGLE akont zwels INTO (p_akont , p_zwels)
                      FROM lfb1
                           WHERE lifnr = p_lifnr
                           AND   bukrs = 'K201'.

ENDFORM.                    " GET_COMPANY_DATA

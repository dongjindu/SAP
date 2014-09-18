************************************************************************
* Transaction MB51 (and replacing also MB59)
* List of material documemts
* Last modification: 20.10.2000 jkr
************************************************************************

* Sept 2002 MM                                              "n555246
* log function tax auditor                                  "n555246

* Sept 2002 MM                                              "n555893
* avoid runtime error DBIF_RSQL_INVALID_RSQL                "n555893
* - use SELECT command for database selection depending on  "n555893
*   the entered material number                             "n555893
* - colourize the numeric values in the list depending on   "n555893
*   the customizing settings in VIEW  V_MMIM_REP_PRINT      "n555893
* - the function module FI_CHECK_DATE of note 486477 will   "n555893
*   be processed when it exists                             "n555893

* separate time depending authorization for tax auditor     "n486477

REPORT rm07docs MESSAGE-ID m7.
*{SEL-OPT Begin} http://intranet.sap.com/materialversion
*Do not change coding between begin and end comments PA8 20030407
INITIALIZATION.
  DATA: mgv_matnr_prog LIKE rsvar-report,
        mgv_matnr_selopt_tab LIKE rsldbdfs OCCURS 0 WITH HEADER LINE.
  FIELD-SYMBOLS <mgv_matnr_selopt_conv> TYPE STANDARD TABLE.
  mgv_matnr_prog = sy-repid.
  mgv_matnr_selopt_tab-name = 'MATNR' .
  APPEND mgv_matnr_selopt_tab.
  CALL FUNCTION 'MGV_SELOP_AFTER_INITIALIZATION'
       EXPORTING
            program        = mgv_matnr_prog
       TABLES
            selop          = mgv_matnr_selopt_tab
       EXCEPTIONS
            no_programname = 1
            OTHERS         = 2.

START-OF-SELECTION.
  LOOP AT mgv_matnr_selopt_tab.
    CONCATENATE mgv_matnr_selopt_tab-name'[]' INTO
    mgv_matnr_selopt_tab-name.
    ASSIGN (mgv_matnr_selopt_tab-name) TO <mgv_matnr_selopt_conv>.
    IF sy-subrc IS INITIAL.
      CALL FUNCTION 'MGV_SELOP_AFTER_START_OF_SEL'
           EXPORTING
                selopt_name = mgv_matnr_selopt_tab-name
           TABLES
                range       = <mgv_matnr_selopt_conv>.
    ENDIF.
  ENDLOOP.
*{SEL-OPT End}

  TYPE-POOLS: mbarc, slis.
  CLASS cl_mmim_auth DEFINITION LOAD.

  TABLES: mkpf, mseg, mmim_rep_cust.

  DATA: BEGIN OF maritab OCCURS 0.
          INCLUDE STRUCTURE mari.
  DATA: END OF maritab.

  DATA: archive_documents TYPE mbarc_mkpf_tab WITH HEADER LINE,
        archive_mkpf      TYPE mbarc_mkpf,
        archive_mseg      TYPE mbarc_mseg,
        archive_messages  TYPE mbarc_message_tab WITH HEADER LINE,
        wa_mseg_tab       TYPE mbarc_mseg.

  CONSTANTS : c_i(01)          TYPE c    VALUE 'I',         "n555893
              c_eq(02)         TYPE c    VALUE 'EQ',        "n555893
              c_x(01)          TYPE c    VALUE 'X'.         "n555893

  INCLUDE zrm07docs_control.
*INCLUDE rm07docs_control.
  INCLUDE zrm07docs_generated.
*INCLUDE rm07docs_generated.

  DATA: BEGIN OF code OCCURS 0,
          line(72),
        END OF code.
  DATA: timestamp(72).

  DATA: BEGIN OF dbtab OCCURS 0.
          INCLUDE STRUCTURE mmim_rep_cust.
  DATA: END OF dbtab.

  DATA: BEGIN OF list OCCURS 0.
          INCLUDE STRUCTURE itab.
  DATA: maktx LIKE makt-maktx,
        name1 LIKE t001w-name1,
        btext LIKE t156t-btext,
        pspid LIKE prps-posid,
        vornr TYPE vornr,                                   "215929
        color_line(03)       TYPE c,                        "n555893
        color TYPE slis_t_specialcol_alv.
  DATA: END OF list.

  DATA: BEGIN OF header OCCURS 0,
          matnr LIKE makt-matnr,
          maktx LIKE makt-maktx,
          werks LIKE t001w-werks,
          name1 LIKE t001w-name1,
        END OF header.

* Commiunication structure between this report and transaction MIGO.
* Changes in the structure have to be copied to include LMIGOSR1.
  DATA: BEGIN OF export_list OCCURS 0,
          matnr TYPE mseg-matnr,
          maktx TYPE makt-maktx,
          werks TYPE mseg-werks,
          name1 TYPE t001w-name1,
          lgort TYPE mseg-lgort,
        charg TYPE mseg-charg,                              "351455
        bwtar TYPE mseg-bwtar,                              "351455
          bwart TYPE mseg-bwart,
          sobkz TYPE mseg-sobkz,
          btext TYPE t156t-btext,
          mblnr TYPE mseg-mblnr,
          mjahr TYPE mseg-mjahr,
          zeile TYPE mseg-zeile,
          budat TYPE mkpf-budat,
          erfmg TYPE mseg-erfmg,
          erfme TYPE mseg-erfme,
          lifnr TYPE mseg-lifnr,
          vgart TYPE mkpf-vgart,
        END OF export_list.

  DATA: BEGIN OF imakt_key OCCURS 0,
          matnr LIKE makt-matnr,
        END OF imakt_key.
  DATA: BEGIN OF imakt OCCURS 0.
          INCLUDE STRUCTURE imakt_key.
  DATA: maktx LIKE makt-maktx,
END OF imakt.

  DATA: BEGIN OF iekpo_key OCCURS 0,
          ebeln LIKE ekpo-ebeln,
          ebelp LIKE ekpo-ebelp,
        END OF iekpo_key.
  DATA: BEGIN OF iekpo OCCURS 0.
          INCLUDE STRUCTURE iekpo_key.
  DATA: txz01 LIKE ekpo-txz01.
  DATA: END OF iekpo.

  DATA: BEGIN OF it001w_key OCCURS 0,
          werks LIKE t001w-werks,
        END OF it001w_key.
  DATA: BEGIN OF it001w OCCURS 0.
          INCLUDE STRUCTURE it001w_key.
  DATA: name1 LIKE t001w-name1,
END OF it001w.

  DATA: BEGIN OF it156t_key OCCURS 0,
          bwart LIKE t156t-bwart,
          sobkz LIKE t156t-sobkz,
          kzbew LIKE t156t-kzbew,
          kzzug LIKE t156t-kzzug,
          kzvbr LIKE t156t-kzvbr,
        END OF it156t_key.
  DATA: BEGIN OF it156t OCCURS 0.
          INCLUDE STRUCTURE it156t_key.
  DATA: btext LIKE t156t-btext,
END OF it156t.

  DATA: BEGIN OF organ OCCURS 0,
          werks LIKE t001w-werks,
          bukrs LIKE t001k-bukrs,
        END OF organ.

  DATA: fc_flat      TYPE slis_fieldcat_alv OCCURS 0 WITH HEADER LINE.
  DATA: fc_hier      TYPE slis_fieldcat_alv OCCURS 0 WITH HEADER LINE.
  DATA: sort         TYPE slis_t_sortinfo_alv WITH HEADER LINE.
  DATA: detail,
        flag,        "Name for compatibility with MB03
        no_list.     "Do not show the list

  SELECTION-SCREEN BEGIN OF BLOCK disp WITH FRAME TITLE text-003.
  PARAMETERS: alv_def LIKE disvariant-variant.
  SELECTION-SCREEN END OF BLOCK disp.
  SELECTION-SCREEN BEGIN OF BLOCK arch WITH FRAME TITLE text-005.
  PARAMETERS: database LIKE am07m-ar_flag  AS CHECKBOX DEFAULT 'X',
              shortdoc LIKE am07m-ar_flag2 AS CHECKBOX DEFAULT ' ',
              archive  LIKE am07m-ar_flag1 AS CHECKBOX DEFAULT ' '
                                           USER-COMMAND us_archive.
  SELECTION-SCREEN END OF BLOCK arch.

  INCLUDE zrm07alvi.
*INCLUDE rm07alvi.

* separate time depending authorization for tax auditor     "n486477
  DATA : g_flag_tpcuser(01)    TYPE c,                      "n486477
*      1 = carry out the special checks for this user       "n486477
         g_flag_message(01)    TYPE c,                      "n486477
         g_f_repid             TYPE progname.               "n486477

INITIALIZATION.
  CLEAR flag.
  IF sy-calld <> space.
    IMPORT flag    FROM MEMORY ID 'MB51_FLAG'.
    IMPORT no_list FROM MEMORY ID 'MB51_NOLIST'.
  ENDIF.
  PERFORM check_version.
  PERFORM alv_init.

AT SELECTION-SCREEN OUTPUT.
  PERFORM place_cursor.
  PERFORM checkboxes_set.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR alv_def.
  PERFORM alv_f4.

AT SELECTION-SCREEN.
  CALL FUNCTION 'MMIM_ENTRYCHECK_MAIN'
       TABLES
            it_matnr = matnr
            it_werks = werks
            it_lgort = lgort
            it_lifnr = lifnr
            it_kunnr = kunnr
            it_bwart = bwart
            it_sobkz = sobkz
            it_vgart = vgart
            it_usnam = usnam.
  PERFORM alv_check.

* - the function module FI_CHECK_DATE of note 486477 will   "n555893
*   be processed when it exists                             "n555893
  CALL FUNCTION 'FUNCTION_EXISTS'                           "n555893
    EXPORTING                                               "n555893
      funcname               = 'FI_CHECK_DATE'              "n555893
    EXCEPTIONS                                              "n555893
      function_not_exist     = 1                            "n555893
      OTHERS                 = 2.                           "n555893
                                                            "n555893
  IF sy-subrc IS INITIAL.                                   "n555893
*   the function module FI_CHECK_DATE exists -> go on       "n555893

* separate time depending authorization for tax auditor     "n486477
*   check, whether the user is a tax auditor                "n486477
    MOVE  sy-repid             TO  g_f_repid.               "n486477
                                                            "n486477
    CALL FUNCTION           'FI_CHECK_DATE'    "#EC EXISTS     "n555893
        EXPORTING                                           "n486477
          i_bukrs           = space                         "n486477
          i_user            = sy-uname                      "n486477
          i_program         = g_f_repid                     "n486477
        IMPORTING                                           "n486477
          e_return          = g_flag_tpcuser                "n486477
        EXCEPTIONS                                          "n486477
          no_authority_prog = 1                             "n486477
          no_authority_date = 2                             "n486477
          wrong_parameter   = 3                             "n486477
          OTHERS            = 4.                            "n486477
                                                            "n486477
    CASE  sy-subrc.                                         "n486477
      WHEN  0.                                              "n486477
*       what kind of user : g_flag_tpcuser = 1 tax auditor  "n486477
*                           g_flag_tpcuser = 4 other other  "n486477
      WHEN  1.                                              "n486477
*       user is tax auditor, but program is not allowed     "n486477
        MESSAGE  e001(ca_check_date) WITH sy-repid.     "#EC * "n555893
      WHEN  OTHERS.                                         "n486477
*       other error                                         "n486477
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno   "n486477
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.   "n486477
    ENDCASE.                                                "n486477
  ENDIF.                                                    "n555893

START-OF-SELECTION.
  IF database = 'X'.
    PERFORM data_selection.
*----- insert by stlim - 2004/02/07 - begin
    PERFORM delete_document.
*----- insert by stlim - 2004/02/07 - end
  ENDIF.
  IF shortdoc = 'X' OR archive = 'X'.
    PERFORM shortdocument_read.
    PERFORM merge_data.
  ENDIF.

END-OF-SELECTION.
  PERFORM build_runtimetable.
  PERFORM build_fieldcatalog.
  PERFORM process_list.
  PERFORM output_list.

************************************************************************
* The data read from the database are enriched with additional
* information (material texts, color, mvttype-text, ...)
************************************************************************
FORM process_list.
  DATA: linecolor TYPE slis_specialcol_alv OCCURS 0 WITH HEADER LINE.
  DATA: fname(20).
  FIELD-SYMBOLS: <f>,
                 <itab> LIKE itab.
* First loop to fill up keytables for additional database accesses
  LOOP AT itab ASSIGNING <itab>.
    IF <itab>-matnr <> space.
      imakt_key-matnr = <itab>-matnr.
      COLLECT imakt_key.
    ELSE.
      iekpo_key-ebeln = <itab>-ebeln.
      iekpo_key-ebelp = <itab>-ebelp.
      COLLECT iekpo_key.
    ENDIF.
    it001w_key-werks = <itab>-werks.
    COLLECT it001w_key.
    it156t_key-bwart = <itab>-bwart.
    it156t_key-sobkz = <itab>-sobkz.
    it156t_key-kzbew = <itab>-kzbew.
    it156t_key-kzzug = <itab>-kzzug.
    it156t_key-kzvbr = <itab>-kzvbr.
    COLLECT it156t_key.
  ENDLOOP.
* Read the data from the additional tables
  READ TABLE imakt_key INDEX 1 TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
    SELECT matnr maktx INTO CORRESPONDING FIELDS OF TABLE imakt
           FROM makt
           FOR ALL ENTRIES IN imakt_key
           WHERE matnr = imakt_key-matnr
           AND   spras = sy-langu.
    SORT imakt BY matnr.
  ENDIF.
  READ TABLE iekpo_key INDEX 1 TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
    SELECT ebeln ebelp txz01 INTO CORRESPONDING FIELDS OF TABLE iekpo
           FROM ekpo
           FOR ALL ENTRIES IN iekpo_key
           WHERE ebeln = iekpo_key-ebeln
             AND ebelp = iekpo_key-ebelp.
    SORT iekpo BY ebeln ebelp.
  ENDIF.
  READ TABLE it001w_key INDEX 1 TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
    SELECT werks name1 INTO CORRESPONDING FIELDS OF TABLE it001w
           FROM t001w
           FOR ALL ENTRIES IN it001w_key
           WHERE werks = it001w_key-werks.
    SORT it001w BY werks.
  ENDIF.
  READ TABLE it156t_key INDEX 1 TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
    SELECT bwart sobkz kzbew kzzug kzvbr btext
           INTO CORRESPONDING FIELDS OF TABLE it156t
           FROM t156t
           FOR ALL ENTRIES IN it156t_key
           WHERE spras = sy-langu
             AND bwart = it156t_key-bwart
             AND sobkz = it156t_key-sobkz
             AND kzbew = it156t_key-kzbew
             AND kzzug = it156t_key-kzzug
             AND kzvbr = it156t_key-kzvbr.
    SORT it156t BY bwart sobkz kzbew kzzug kzvbr.
  ENDIF.
  SELECT werks bukrs INTO CORRESPONDING FIELDS OF TABLE organ
         FROM t001w INNER JOIN t001k
         ON t001k~bwkey = t001w~bwkey
         WHERE werks IN werks.
  SORT organ BY werks.
* Final loop over itab. Merge data, eliminate authorization problems,
* fill color, ...
  LOOP AT itab ASSIGNING <itab>.
    CLEAR list.
    MOVE-CORRESPONDING <itab> TO list.
* Authorization for material documents plant level
    CHECK cl_mmim_auth=>check( i_object = 'M_MSEG_WMB'
                               i_value1 = list-werks ) IS INITIAL.
* Authorization for storage location
    CHECK cl_mmim_auth=>check( i_object = 'M_MSEG_LGO'
                               i_value1 = list-werks
                               i_value2 = list-lgort
                               i_value3 = list-bwart ) IS INITIAL.


*   separate time depending authorization for tax auditor   "n486477
    IF  g_flag_tpcuser = '1'.                               "n486477
*     check the authority per item                          "n486477
      CALL FUNCTION 'MB_CHECK_USER_PG_DATE_PLANT'           "n486477
        EXPORTING                                           "n486477
          i_plant            = list-werks                   "n486477
          i_user             = sy-uname                     "n486477
          i_program          = g_f_repid                    "n486477
          i_budat            = list-budat                   "n486477
          i_write_log        = ' '   "no protocol function   "n555246
        EXCEPTIONS                                          "n486477
          no_authority_prog  = 1                            "n486477
          no_authority_date  = 2                            "n486477
          OTHERS             = 3.                           "n486477
                                                            "n486477
*     evaluate the result                                   "n486477
      CASE   sy-subrc.                                      "n486477
        WHEN  0.                                            "n486477
*         user has the required authorization               "n486477
        WHEN  2.                                            "n486477
          IF  g_flag_message IS INITIAL.                    "n486477
            MOVE  'X'        TO  g_flag_message.            "n486477
*           The list is incomplete due to lacking autho...  "n486477
            MESSAGE          s124(m7).                      "n486477
          ENDIF.                                            "n486477
        WHEN  OTHERS.                                       "n486477
*         parameter error                                   "n486477
      ENDCASE.                                              "n486477
                                                            "n486477
*     go on when the authorization is ok                    "n486477
      CHECK : sy-subrc IS INITIAL.                          "n486477
    ENDIF.                                                  "n486477


* Authorization for posting values company code level
    READ TABLE organ WITH KEY werks = list-werks BINARY SEARCH.
    IF NOT cl_mmim_auth=>check(
                      i_object = 'F_BKPF_BUK'
                      i_level  = cl_mmim_auth=>c_warning
                      i_value1 = organ-bukrs ) IS INITIAL.
      LOOP AT rtt WHERE fiauth = 'X'.
        CONCATENATE 'LIST-' rtt-fieldname INTO fname.
        ASSIGN (fname) TO <f>.
        CLEAR <f>.
      ENDLOOP.
    ENDIF.
* Merge additional data
    IF list-matnr <> space.
      READ TABLE imakt WITH KEY matnr = list-matnr BINARY SEARCH.
      IF sy-subrc = 0. list-maktx = imakt-maktx. ENDIF.
    ELSE.
      READ TABLE iekpo WITH KEY ebeln = list-ebeln
                                ebelp = list-ebelp BINARY SEARCH.
      IF sy-subrc = 0. list-maktx = iekpo-txz01. ENDIF.
    ENDIF.
    READ TABLE it001w WITH KEY werks = list-werks BINARY SEARCH.
    IF sy-subrc = 0. list-name1 = it001w-name1. ENDIF.
    READ TABLE it156t WITH KEY bwart = list-bwart
                               sobkz = list-sobkz
                               kzbew = list-kzbew
                               kzzug = list-kzzug
                               kzvbr = list-kzvbr BINARY SEARCH.
    IF sy-subrc = 0. list-btext = it156t-btext. ENDIF.
* Conversions
    IF list-ps_psp_pnr <> space.
*     WRITE list-ps_psp_pnr TO list-pspid.
*     filtering works only when converting the in following way: "428530
      CALL FUNCTION 'CJPN_INTERN_TO_EXTERN_CONV'            "428530
        EXPORTING
          edit_imp         = space
          int_num          = list-ps_psp_pnr
        IMPORTING
          ext_num          = list-pspid
        EXCEPTIONS
          not_found        = 1
          OTHERS           = 2.
      IF sy-subrc <> 0.                                     "428530
        CLEAR list-pspid.
      ENDIF.                                                "428530
    ENDIF.
* Derivation of VORNR from APLZL/AUFPL for networks
    IF NOT list-nplnr IS INITIAL.                           "215929
      CALL FUNCTION 'READ_NETWORK_NPLNR_VORNR'              "215929
        EXPORTING                                           "215929
          aplzl           = list-aplzl                      "215929
          aufpl           = list-aufpl                      "215929
        IMPORTING                                           "215929
          vornr           = list-vornr                      "215929
        EXCEPTIONS                                          "215929
          not_found       = 1                               "215929
          OTHERS          = 2.                              "215929
      IF sy-subrc <> 0.                                     "215929
        CLEAR list-vornr.                                   "215929
      ENDIF.                                                "215929
    ENDIF.                                                  "215929
* Color information and signs

    IF  alv_colourize_fields = 'X'.                         "n555893
*   customizing setting allows the colourization            "n555893
      REFRESH linecolor.
      LOOP AT rtt WHERE color <> space.
        CONCATENATE 'LIST-' rtt-fieldname INTO fname.
        linecolor-fieldname = rtt-fieldname.
        linecolor-color-int = 0.
        CASE list-shkzg.
          WHEN 'S'. linecolor-color-col = 5.
          WHEN 'H'.
            linecolor-color-col = 6.
            IF rtt-color = '-'.
              ASSIGN (fname) TO <f>.
              <f> = 0 - <f>.
            ENDIF.
        ENDCASE.
        APPEND linecolor.
      ENDLOOP.

      list-color[] = linecolor[].
    ELSE.                                                   "n555893
*     customizing setting does not colourize numeric fields "n555893
      LOOP AT rtt WHERE color <> space.                     "n555893
*       set the sign for numeric fields                     "n555893
        CONCATENATE 'LIST-' rtt-fieldname INTO fname.       "n555893
                                                            "n555893
        CASE list-shkzg.                                    "n555893
          WHEN 'H'.                                         "n555893
            IF rtt-color = '-'.                             "n555893
              ASSIGN (fname) TO <f>.                        "n555893
              <f> = 0 - <f>.                                "n555893
            ENDIF.                                          "n555893
        ENDCASE.                                            "n555893
      ENDLOOP.                                              "n555893
                                                            "n555893
*     set the color for the whole line                      "n555893
      CASE list-shkzg.                                      "n555893
        WHEN 'S'.                                           "n555893
          MOVE  'C20'   TO  list-color_line. "light grey    "n555893
        WHEN 'H'.                                           "n555893
          MOVE  'C21'   TO  list-color_line. "grey          "n555893
      ENDCASE.                                              "n555893
    ENDIF.                                                  "n555893

    APPEND list.
  ENDLOOP.

  SORT list BY matnr maktx werks budat DESCENDING.

ENDFORM.
************************************************************************
* Print the detail list.
************************************************************************
FORM detail_list.
  DATA: variant_detail      LIKE disvariant.
  DATA: lt_base_list LIKE list[].                           "401421
* The detail ALV may modify the list (sorting). If returned to the
* base list, the original list needs to be restored.
  lt_base_list[] = list[].                                  "401421
  CLEAR variant_detail.
  variant_detail-report = alv_repid.
  variant_detail-handle = 'DETA'.
  detail = 'X'.
  CALL FUNCTION alv_detail_func
       EXPORTING
            i_callback_program       = alv_repid
            i_callback_pf_status_set = 'SET_STATUS'
            i_callback_user_command  = 'USER_COMMAND'
            is_layout                = alv_layout
            it_fieldcat              = fc_flat[]
            i_default                = 'X'
            i_save                   = 'A'
            is_variant               = variant_detail
            is_print                 = alv_print
       TABLES
            t_outtab                 = list[]
       EXCEPTIONS
            program_error            = 1
            OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  CLEAR detail.                                             "401421
  list[] = lt_base_list[].                                  "401421
ENDFORM.
************************************************************************
* Print the main list (hierarchical)
************************************************************************
FORM output_list.

* consider the customizing setting for colourization        "n555893
  IF  alv_colourize_fields = 'X'.                           "n555893
    alv_layout-coltab_fieldname = 'COLOR'.                  "n555893
  ELSE.                                                     "n555893
    alv_layout-info_fieldname   = 'COLOR_LINE'.             "n555893
  ENDIF.                                                    "n555893

  alv_keyinfo-header01 = 'MATNR'.
  alv_keyinfo-header02 = 'MAKTX'.
  alv_keyinfo-header03 = 'WERKS'.
  alv_keyinfo-item01   = 'MATNR'.
  alv_keyinfo-item02   = 'MAKTX'.
  alv_keyinfo-item03   = 'WERKS'.
  LOOP AT list.
    ON CHANGE OF list-matnr OR list-maktx OR list-werks.
      header-matnr = list-matnr.
      header-maktx = list-maktx.
      header-werks = list-werks.
      header-name1 = list-name1.
      APPEND header.
    ENDON.
  ENDLOOP.

* process log function when                               "n555246
* a) the database selection was successful and            "n555246
* b) the user is a tax auditor                            "n555246
  IF  NOT list[] IS INITIAL  AND                            "n555246
      g_flag_tpcuser = '1'.                                 "n555246
    PERFORM                  tpc_write_log.                 "n555246
  ENDIF.                                                    "n555246

  CLEAR detail.
* Export the result list to memory for use in the MIGO-Tree
  IF flag <> space.
    REFRESH export_list.
    LOOP AT list.
      MOVE-CORRESPONDING list TO export_list.
      APPEND export_list.
    ENDLOOP.
    EXPORT export_list TO MEMORY ID 'MB51_EXPORT_LIST'.
*   If called just to deliver results but not to show them
    IF no_list <> space.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
       EXPORTING
*           I_INTERFACE_CHECK        = intchk
           i_callback_program       = alv_repid
           i_callback_pf_status_set = 'SET_STATUS'
           i_callback_user_command  = 'USER_COMMAND'
           is_layout                = alv_layout
           is_print                 = alv_print
           it_fieldcat              = fc_hier[]
           i_default                = 'X'
           i_save                   = 'A'
           is_variant               = alv_variant
            i_tabname_header         = 'HEADER'
            i_tabname_item           = 'LIST'
            is_keyinfo               = alv_keyinfo
       TABLES
            t_outtab_header          = header[]
            t_outtab_item            = list[]
      EXCEPTIONS
           program_error            = 1
           OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.

************************************************************************
* Generate the fieldcatalog.
* The routine uses the information stored in the RM07DOCS_GENERATED
* file created upon the arrival of new customizing data affecting
* the field selection (view V_MMIM_REP_CUST)
************************************************************************
FORM build_fieldcatalog.
  REFRESH fc_flat.
  LOOP AT rtt.
    CLEAR fc_flat.
    fc_flat-fieldname     = rtt-fieldname.
    fc_flat-ref_tabname   = rtt-tabname.
    fc_flat-ref_fieldname = rtt-fieldname.
    CASE rtt-cqindicator.
      WHEN 'C'. fc_flat-cfieldname = rtt-cqfieldname.
      WHEN 'Q'. fc_flat-qfieldname = rtt-cqfieldname.
    ENDCASE.
    fc_flat-tabname       = 'LIST'.
    IF rtt-output_position = '00'.
      fc_flat-no_out        = 'X'.
    ENDIF.
* PS_PSP_PNR need a special conversion and refers to another field
    IF rtt-fieldname = 'PS_PSP_PNR'.
      fc_flat-fieldname      = 'PSPID'.
      fc_flat-ref_tabname    = 'PRPS'.
      fc_flat-ref_fieldname  = 'POSID'.
    ENDIF.
    APPEND fc_flat.
* Some fields have apendixes (e.g. MATNR is followed by MAKTX)
    CLEAR fc_flat.
    CASE rtt-fieldname.
      WHEN 'MATNR'.
        fc_flat-fieldname   = 'MAKTX'.
        fc_flat-tabname     = 'LIST'.
        fc_flat-ref_tabname = 'MAKT'.
      WHEN 'WERKS'.
        fc_flat-fieldname   = 'NAME1'.
        fc_flat-tabname     = 'LIST'.
        fc_flat-ref_tabname = 'T001W'.
      WHEN 'BWART'.
        fc_flat-fieldname   = 'BTEXT'.
        fc_flat-tabname     = 'LIST'.
        fc_flat-ref_tabname = 'T156T'.
      WHEN 'NPLNR'.                                         "215929
        fc_flat-fieldname   = 'VORNR'.                      "215929
        fc_flat-tabname     = 'LIST'.                       "215929
        fc_flat-ref_tabname = 'RESB'.                       "215929
      WHEN OTHERS.
        CONTINUE.
    ENDCASE.
    fc_flat-no_out = 'X'.
    APPEND fc_flat.
  ENDLOOP.
* For the hierarchic ALV, the header fields are extracted
* and activated.
  REFRESH fc_hier.
  LOOP AT fc_flat.
    fc_hier = fc_flat.
    CASE fc_flat-fieldname.
      WHEN 'MATNR' OR 'MAKTX' OR 'WERKS' OR 'NAME1'.
        fc_hier-tabname = 'HEADER'.
        fc_hier-no_out = ' '.
    ENDCASE.
    APPEND fc_hier.
  ENDLOOP.
ENDFORM.

************************************************************************
* Compare the actual selection screen and data selection versions
* with the data from the customizing table. If any transport
* has occured into this table, the MAX of the date-time-group
* will differ from the last generation's value and a regeneration
* is necessary.
************************************************************************
FORM check_version.
  DATA: dtg_db(14) TYPE n.
  SELECT MAX( dtg ) INTO dtg_db FROM mmim_rep_cust
    WHERE report = 'RM07DOCS'.
  IF sy-subrc = 0.
    READ REPORT 'RM07DOCS_GENERATED' INTO code.             "215043
    READ TABLE code INDEX 1.
    CONCATENATE '*' dtg_db INTO timestamp.
    CHECK timestamp <> code-line.
  ENDIF.
  PERFORM regenerate.
ENDFORM.
************************************************************************
* Regenerate the selection screen and data selection routines.
* The data are written into the inclue RM07DOCS_GENERATED
************************************************************************
FORM regenerate.

  DATA: line(72),
        quote(1),
        number_of_lines TYPE i.
  DATA: BEGIN OF mariinfo OCCURS 30.
          INCLUDE STRUCTURE dntab.
  DATA: END OF mariinfo.

  DATA : l_f_fieldname(40)    TYPE c.                       "n555893
  DATA : l_f_where_and(12)    TYPE c.                       "n555893

  DEFINE ac.
    code-line = &1.
    append code.
  END-OF-DEFINITION.

* Call the auto-repair-coding to ensure that the database table
* MMIM_REP_CUST is consistent.
  PERFORM auto_repair.                                      "215043

  SELECT * FROM mmim_rep_cust INTO TABLE dbtab WHERE
     report = 'RM07DOCS'.
  SORT dbtab BY tabname fieldname.
  PERFORM build_controltable.

  REFRESH code.
  ac timestamp.                                             "215043
  ac '* SELECT command improved with note  555893      '.   "n555893
  ac ' '.                                                   "n555893
  ac '* Generated include for RM07DOCS'.
  ac '* Please do not change manually as any changes here will'.
  ac '* be completely overwritten when the settings in view'.
  ac '* V_MMIM_REP_CUST are changed for this report.'.
  ac ''.
* Selection screen
  ac '* Selection screen'.
  ac 'selection-screen begin of block mseg with frame title text-001.'.
  PERFORM fill_selections USING 'MSEG'.
  ac 'selection-screen end of block mseg.'.
  ac 'selection-screen begin of block mkpf with frame title text-002.'.
  PERFORM fill_selections USING 'MKPF'.
  ac 'selection-screen end of block mkpf.'.
  ac ''.
* Internal table
  ac '* Internal table for data selection'.
  ac 'data: begin of itab occurs 0.'.
  SORT dbtab BY fieldname.
  LOOP AT dbtab WHERE outpos <> space.
    CONCATENATE dbtab-tabname '-' dbtab-fieldname INTO line.
    CONCATENATE '  data:' dbtab-fieldname 'like' line '.'
      INTO line SEPARATED BY ' '.
    ac line.
  ENDLOOP.
  ac 'data: end of itab.'.
  ac ''.
* Data selection routine
  ac '* Data selection routine'.
  ac 'form data_selection.'.

* define local working fields                               "n555893
  ac ' '.                                                   "n555893
  ac '* define local working fields                    '.   "n555893
  ac 'data : l_cnt_matnr_i_eq     type i.              '.   "n555893
  ac 'data : l_cnt_matnr_total    type i.              '.   "n555893
  ac ' '.                                                   "n555893
  ac '* working table for the fields to be transported '.   "n555893
  ac 'types : begin of stype_fields,                   '.   "n555893
  ac '          fieldname         type  name_feld,     '.   "n555893
  ac '        end of stype_fields.                     '.   "n555893
  ac 'types : stab_fields         type standard table  '.   "n555893
  ac '                            of stype_fields      '.   "n555893
  ac '                            with default key.    '.   "n555893
  ac 'data : l_t_fields           type stab_fields.    '.   "n555893
  ac ' '.                                                   "n555893
                                                            "n555893
* create table with the fields to be transported            "n555893
  ac '* create table with the fields to be transported '.   "n555893
                                                            "n555893
  LOOP AT dbtab WHERE outpos <> space.                      "n555893
    CONCATENATE ''''          dbtab-tabname '~'             "n555893
                dbtab-fieldname ''''                        "n555893
                              INTO l_f_fieldname.           "n555893
    CONCATENATE 'append'  l_f_fieldname                     "n555893
                'to'      'l_t_fields.'                     "n555893
                             INTO  line                     "n555893
                             SEPARATED BY space.            "n555893
    ac                       line.                          "n555893
  ENDLOOP.                                                  "n555893
                                                            "n555893
* analyse the select-option table for material numbers      "n555893
  ac ' '.                                                   "n555893
  ac '* analyse the select-option table for material   '.   "n555893
  ac '* numbers                                        '.   "n555893
  ac '  clear : l_cnt_matnr_total, l_cnt_matnr_i_eq.   '.   "n555893
  ac ' '.                                                   "n555893
  ac '  loop at matnr.                                 '.   "n555893
  ac '    add  1             to  l_cnt_matnr_total.    '.   "n555893
  ac ' '.                                                   "n555893
  ac '    if  not matnr-low     is initial  and        '.   "n555893
  ac '            matnr-sign    =  c_i      and        '.   "n555893
  ac '            matnr-option  =  c_eq     and        '.   "n555893
  ac '            matnr-high    is initial.            '.   "n555893
  ac '*     the table contains single a material number'.   "n555893
  ac '      add  1           to  l_cnt_matnr_i_eq.     '.   "n555893
  ac '    else.                                        '.   "n555893
  ac '      exit.                                      '.   "n555893
  ac '    endif.                                       '.   "n555893
  ac '  endloop.                                       '.   "n555893
  ac ' '.                                                   "n555893
                                                            "n555893
* create two different select commands                      "n555893
  ac '* process SELECT command depending on the        '.   "n555893
  ac '* required material selection                    '.   "n555893
  ac '  if  l_cnt_matnr_total  > 0                 and '.   "n555893
  ac '      l_cnt_matnr_total  = l_cnt_matnr_i_eq.     '.   "n555893
                                                            "n555893
* create the select command with ... for all entries ...    "n555893
  ac '*   work with .. for all entries ...             '.   "n555893
  ac '    select (l_t_fields)                          '.   "n555893
  ac '    into corresponding fields of table itab      '.   "n555893
  ac '    from mkpf inner join mseg                    '.   "n555893
  ac '    on    mkpf~mandt = mseg~mandt                '.   "n555893
  ac '      and mkpf~mblnr = mseg~mblnr                '.   "n555893
  ac '      and mkpf~mjahr = mseg~mjahr                '.   "n555893
  ac '    for all entries in matnr                     '.   "n555893
  ac '    where matnr = matnr-low                      '.   "n555893
                                                            "n555893
* create the normal where ... in ... conditions             "n555893
  LOOP AT dbtab WHERE selpos <> space.                      "n555893
    CHECK dbtab-fieldname NE 'MATNR'.                       "n555893
                                                            "n555893
    CONCATENATE dbtab-tabname '~' dbtab-fieldname           "n555893
                             INTO l_f_fieldname.            "n555893
    CONCATENATE '      and'  l_f_fieldname                  "n555893
                'in'         dbtab-fieldname(8)             "n555893
                             INTO line                      "n555893
                             SEPARATED BY ' '.              "n555893
    ac                       line.                          "n555893
  ENDLOOP.                                                  "n555893
                                                            "n555893
  ac '.                                                '.   "n555893
  ac '  else.                                          '.   "n555893
                                                            "n555893
* create the normal select command                          "n555893
  ac '*   work with the select command as usual        '.   "n555893
  ac '    select (l_t_fields)                          '.   "n555893
  ac '    into corresponding fields of table itab      '.   "n555893
  ac '    from mkpf inner join mseg                    '.   "n555893
  ac '    on    mkpf~mandt = mseg~mandt                '.   "n555893
  ac '      and mkpf~mblnr = mseg~mblnr                '.   "n555893
  ac '      and mkpf~mjahr = mseg~mjahr                '.   "n555893
                                                            "n555893
  MOVE '       WHERE'        TO  l_f_where_and.             "n555893
                                                            "n555893
* create the normal where ... in ... conditions             "n555893
  LOOP AT dbtab WHERE selpos <> space.                      "n555893
    CONCATENATE dbtab-tabname '~' dbtab-fieldname           "n555893
                             INTO l_f_fieldname.            "n555893
    CONCATENATE l_f_where_and  l_f_fieldname                "n555893
                'in' dbtab-fieldname(8)                     "n555893
                             INTO line                      "n555893
                             SEPARATED BY ' '.              "n555893
    ac                       line.                          "n555893
    MOVE  '         and'     TO  l_f_where_and.             "n555893
  ENDLOOP.                                                  "n555893
                                                            "n555893
  ac '.'.                                                   "n555893
  ac '  endif.                                         '.   "n555893
  ac 'endform.                                         '.   "n555893

  ac ''.
* Data selection routine for short documents (table MARI)
* The select statement is the intersection of the fields in MARI
* (can change due to customer include) and the fields on the
* selection screen.
  ac '* Data selection routine for short documents'.
  ac 'form shortdocument_read.'.
  CALL FUNCTION 'NAMETAB_GET'
       EXPORTING
            langu   = sy-langu
            tabname = 'MARI'
       TABLES
            nametab = mariinfo
       EXCEPTIONS
            OTHERS  = 5.
  IF sy-subrc = 0.
    ac 'select * from mari'.
    ac '  into corresponding fields of table maritab where'.
    LOOP AT mariinfo.
      READ TABLE dbtab WITH KEY fieldname = mariinfo-fieldname.
      CHECK sy-subrc = 0 AND dbtab-selpos <> space.
      CONCATENATE dbtab-fieldname 'in' dbtab-fieldname INTO line
         SEPARATED BY ' '.
      ac line.
      ac 'and'.
    ENDLOOP.
    DESCRIBE TABLE code LINES number_of_lines.
    DELETE code INDEX number_of_lines.
  ENDIF.
  ac '.'.
  ac 'endform.'.
  ac ''.
* Form routine to check material documents against the selection
* criteria (necessary for archived documents)
  ac '* Checking selections for archives documents'.
  ac 'form archive_check_selections changing rc.'.
  ac 'if'.
  LOOP AT dbtab WHERE selpos <> space.
    CONCATENATE 'ITAB-' dbtab-fieldname INTO line.
    CONCATENATE line 'in' dbtab-fieldname(8) INTO line
      SEPARATED BY ' '.
    ac line.
    ac 'and'.
  ENDLOOP.
  DESCRIBE TABLE code LINES number_of_lines.
  DELETE code INDEX number_of_lines.
  ac '. rc = 0. else. rc = 4. endif.'.
  ac 'endform.'.
  ac ''.
* List of the fields that are in the fieldcatalog
  ac '* Control data for fieldcatalog, color, authorization, ...'.
  ac 'form build_runtimetable.'.
  SORT dbtab BY fieldname.
  SORT ct BY output_position.
  WRITE '''' TO quote.
  LOOP AT ct WHERE output_position <> '00'.
    CONCATENATE '  rx' quote ct quote '.' INTO line.
    ac line.
  ENDLOOP.
  LOOP AT dbtab WHERE outpos <> space.
    CLEAR ct.
    ct-selection_position = '00'.
    ct-output_position = '00'.
    MOVE-CORRESPONDING dbtab TO ct.
    READ TABLE ct WITH KEY tabname = dbtab-tabname
                           fieldname = dbtab-fieldname.
    CHECK   sy-subrc <> 0 OR
          ( sy-subrc = 0 AND ct-output_position = '00' ).
    CONCATENATE '  rx' quote ct quote '.' INTO line.
    ac line.
  ENDLOOP.
  ac 'endform.'.
  ac ''.

  INSERT REPORT 'RM07DOCS_GENERATED' FROM code.
  GENERATE REPORT sy-repid.                                 "215043

  LEAVE TO TRANSACTION sy-tcode.                            "215043
ENDFORM.
************************************************************************
* Fills the selection screen for a block (MKPF or MSEG).
************************************************************************
FORM fill_selections USING table.
  SORT ct BY tabname selection_position.
* Fields that are mandatory in presence and position according to
* the control table
  LOOP AT ct WHERE tabname = table AND selection_position <> '00'.
    READ TABLE dbtab WITH KEY fieldname = ct-fieldname.
    CHECK sy-subrc = 0.
    PERFORM fill_single_line.
  ENDLOOP.
* Remaining fields from the customizing table
  LOOP AT dbtab WHERE tabname = table AND selpos <> space.
    READ TABLE ct WITH KEY tabname = dbtab-tabname
                           fieldname = dbtab-fieldname.
    CHECK  sy-subrc <> 0 OR
         ( sy-subrc = 0 AND ct-selection_position = '00' ).
    PERFORM fill_single_line.
  ENDLOOP.
ENDFORM.
************************************************************************
* Another modularization unit below: A single line for the selection
* screen is generated.
************************************************************************
FORM fill_single_line.
  DATA: line(72).
  CONCATENATE dbtab-tabname '-' dbtab-fieldname INTO line.
  CONCATENATE '  select-options' dbtab-fieldname(8) 'for' line
      INTO line SEPARATED BY ' '.
  IF dbtab-memoryid <> space.
    CONCATENATE line 'memory id' dbtab-memoryid
      INTO line SEPARATED BY ' '.
  ENDIF.
  CONCATENATE line '.' INTO line.
  ac line.
ENDFORM.
************************************************************************
* Status for both lists
************************************************************************
FORM set_status USING rt_extab TYPE slis_t_extab.
  IF cl_mmim_auth=>level( ) IS INITIAL.
    APPEND 'AUTH' TO rt_extab.
  ENDIF.
  IF cl_mmim_auth=>level( ) = cl_mmim_auth=>c_error.
    MESSAGE s124(m7).
  ENDIF.
  IF detail = 'X'.
    APPEND 'DETAIL' TO rt_extab.
  ENDIF.
  IF shortdoc = space OR archive = 'X'.
    APPEND 'ARCHIVE' TO rt_extab.
  ENDIF.
  SET PF-STATUS 'STANDARD' EXCLUDING rt_extab.
ENDFORM.
************************************************************************
* User command for both lists
************************************************************************
FORM user_command USING rf_ucomm LIKE sy-ucomm
                          rs TYPE slis_selfield.
  DATA: awref LIKE acchd-awref,
        aworg LIKE acchd-aworg.
  DATA: BEGIN OF archive_table OCCURS 0.
          INCLUDE STRUCTURE mseg.
  DATA: END OF archive_table.
* Read tables
  IF rs-tabname = '1'.
    rs-tabname = 'LIST'.
  ENDIF.
  CASE rs-tabname.
    WHEN 'LIST'.   READ TABLE list INDEX rs-tabindex.
    WHEN 'HEADER'.
      READ TABLE header INDEX rs-tabindex.
      CLEAR list.
      MOVE-CORRESPONDING header TO list.
  ENDCASE.
  CHECK sy-subrc = 0.
  IF rf_ucomm = '&IC1'.
    CASE rs-tabname.
      WHEN 'HEADER'. rf_ucomm = 'MMBE'.
      WHEN 'LIST'  .
        CASE rs-fieldname.
          WHEN 'EBELN' OR 'EBELP'. rf_ucomm = 'PUOR'.
          WHEN OTHERS.             rf_ucomm = 'DOCU'.
        ENDCASE.
    ENDCASE.
  ENDIF.
  CASE rf_ucomm.
    WHEN 'MMBE'.
      SET PARAMETER ID 'MAT' FIELD list-matnr.
      SET PARAMETER ID 'WRK' FIELD list-werks.
      CALL TRANSACTION 'MMBE' AND SKIP FIRST SCREEN.
    WHEN 'DOCU'.
      CHECK rs-tabname = 'LIST'.
      CASE rs-fieldname.
        WHEN 'ZEILE'.
        WHEN OTHERS.
          CLEAR list-zeile.
      ENDCASE.
      SET PARAMETER ID 'MBN' FIELD list-mblnr.
      SET PARAMETER ID 'POS' FIELD list-zeile.
      SET PARAMETER ID 'MJA' FIELD list-mjahr.
* If the transaction has been called, the double click leads
* back to the caller.
      IF flag <> space.
        CLEAR list-zeile.
        SET PARAMETER ID 'POS' FIELD list-zeile.
        LEAVE PROGRAM.
      ELSE.
* Call the display transaction
        CALL FUNCTION 'MIGO_DIALOG'
             EXPORTING
                  i_action            = 'A04'
                  i_refdoc            = 'R02'
                  i_notree            = 'X'
                  i_no_auth_check     = ' '
                  i_deadend           = 'X'
                  i_skip_first_screen = 'X'
                  i_okcode            = 'OK_GO'
                  i_mblnr             = list-mblnr
                  i_mjahr             = list-mjahr
                  i_zeile             = list-zeile.
      ENDIF.
    WHEN 'PUOR'.
      CHECK rs-tabname = 'LIST'.
      CASE rs-fieldname.
        WHEN 'EBELP'.
        WHEN OTHERS.
          CLEAR list-ebelp.
      ENDCASE.
      CHECK list-ebeln <> space.
      CALL FUNCTION 'ME_DISPLAY_PURCHASE_DOCUMENT'
           EXPORTING
                i_ebeln      = list-ebeln
                i_ebelp      = list-ebelp
                i_enjoy      = 'X'
           EXCEPTIONS
                not_found    = 1
                no_authority = 2
                invalid_call = 3
                OTHERS       = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    WHEN 'AUTH'.
      CALL METHOD cl_mmim_auth=>display.
    WHEN 'SORT'.
      REFRESH sort. CLEAR sort.
      sort-fieldname = 'CPUDT'. sort-tabname = 'LIST'.
      sort-spos = '01'. sort-down = 'X'.
      APPEND sort.
      sort-fieldname = 'CPUTM'. sort-tabname = 'LIST'.
      sort-spos = '02'. sort-down = 'X'.
      APPEND sort.
      sort-fieldname = 'ZEILE'. sort-tabname = 'LIST'.
      sort-spos = '03'. sort-down = 'X'.
      APPEND sort.
      CALL FUNCTION 'REUSE_ALV_LIST_LAYOUT_INFO_SET'
           EXPORTING
                it_sort = sort[].
      rs-refresh = 'X'.
    WHEN 'FIDC'.
*{   REPLACE        KA5K014065                                        1
*\      CHECK rs-tabname = 'LIST'.
*\      awref = list-mblnr.
*\      aworg = list-mjahr.
*\      CALL FUNCTION 'AC_DOCUMENT_RECORD'
*\        EXPORTING
*\          i_awtyp      = 'MKPF'
*\          i_awref      = awref
*\          i_aworg      = aworg
*\      EXCEPTIONS
*\          no_reference = 1
*\          no_document  = 2
*\          OTHERS       = 3.
      CALL FUNCTION 'CKMD_AC_DOCUMENT_RECORD'
           EXPORTING
                i_mblnr              = list-mblnr
                i_mjahr              = list-mjahr
                i_zeile              = list-zeile
                i_dialog             = 'X'
           EXCEPTIONS
                no_document          = 1
                no_reference         = 2
                selection_not_unique = 3
                OTHERS               = 4.
*
*}   REPLACE
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    WHEN 'DETAIL'.
      PERFORM detail_list.
    WHEN 'ARCHIVE'.
      CHECK rs-tabname = 'LIST'.
      CALL FUNCTION 'MB_GET_SINGLE_MM_MATBEL'
           EXPORTING
                document_number              = list-mblnr
                document_year                = list-mjahr
           TABLES
                line_items                   = archive_table
           EXCEPTIONS
                index_not_found              = 1
                no_access_to_archive         = 2
                no_authority_to_read_archive = 3
                OTHERS                       = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      READ TABLE archive_table WITH KEY mblnr = list-mblnr
                                        mjahr = list-mjahr
                                        zeile = list-zeile.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING archive_table TO list.
        MODIFY list INDEX rs-tabindex.
        rs-refresh = 'X'.
      ENDIF.
  ENDCASE.
  CLEAR rf_ucomm.
ENDFORM.
************************************************************************
* The routine check for the data from the short documents (MARI)
* whether they are already in the database table. If not,
* data are mixed into the internal table to be displayed.
************************************************************************
FORM merge_data.
  DATA: rc TYPE i.
* Eliminate dublettes in MARITAB, fill to ITAB if no archive read
  SORT itab BY mblnr mjahr zeile.
  LOOP AT maritab.
    READ TABLE itab WITH KEY mblnr = maritab-mblnr
                             mjahr = maritab-mjahr
                             zeile = maritab-zeile
                             BINARY SEARCH.
    IF sy-subrc <> 0.
      IF archive = ' '.
        CLEAR itab.
        MOVE-CORRESPONDING maritab TO itab.
*       Insert into main table keeping the sorting order
        INSERT itab INDEX sy-tabix.                         "302603
      ENDIF.
    ELSE.
      DELETE maritab.
    ENDIF.
  ENDLOOP.
* Reading in archive only if requested
  CHECK archive = 'X'.
  READ TABLE maritab INDEX 1.
  CHECK sy-subrc = 0.
  CALL FUNCTION 'MB_GET_MM_MATBEL'
       TABLES
            data_table                   = archive_documents
            key_table                    = maritab
            message_table                = archive_messages
       EXCEPTIONS
            no_authority_to_read_archive = 1
            check_message_table          = 2
            index_not_found              = 3
            OTHERS                       = 4.
  IF sy-subrc <> 0 AND sy-subrc <> 2.
    archive_messages-msgid = sy-msgid.
    archive_messages-msgno = sy-msgno.
    archive_messages-msgv1 = sy-msgv1.
    archive_messages-msgv2 = sy-msgv2.
    archive_messages-msgv3 = sy-msgv3.
    archive_messages-msgv4 = sy-msgv4.
    APPEND archive_messages.
  ENDIF.
* Report errors
  SORT archive_messages BY msgid msgno msgv1 msgv2 msgv3 msgv4.
  READ TABLE archive_messages INDEX 1.
  IF sy-subrc = 0.
    PERFORM archive_messages_popup.
  ENDIF.
  CLEAR itab.
  LOOP AT archive_documents.
    MOVE-CORRESPONDING archive_documents TO itab.
    LOOP AT archive_documents-mseg_tab INTO wa_mseg_tab.
      MOVE-CORRESPONDING wa_mseg_tab TO itab.
* Dataselection from archive brings out all material documents whose
* characteristics were found in the short documents
* e.g. search for material A brings mat doc for materials A and B.
* This requires a check for the selection criteria afterwards.
      PERFORM archive_check_selections CHANGING rc.
      IF rc = 0.
        APPEND itab.
      ENDIF.
    ENDLOOP.
  ENDLOOP.


ENDFORM.                    " merge_data

************************************************************************
* If reading the archive delivered any messages (index not found,
* corrupt, insert more money...).
************************************************************************
FORM archive_messages_popup.
  DATA: BEGIN OF outtab OCCURS 0,
          msgid LIKE sy-msgid,
          msgno LIKE sy-msgno,
          text(80),
        END OF outtab.
  DATA: fc TYPE slis_fieldcat_alv OCCURS 0 WITH HEADER LINE.

  REFRESH outtab.
  LOOP AT archive_messages.
    MOVE-CORRESPONDING archive_messages TO outtab.
    MESSAGE ID     archive_messages-msgid
            TYPE   'E'
            NUMBER archive_messages-msgno
            WITH   archive_messages-msgv1
                   archive_messages-msgv2
                   archive_messages-msgv3
                   archive_messages-msgv4
            INTO   outtab-text.
    APPEND outtab.
  ENDLOOP.

  REFRESH fc.
  fc-fieldname = 'MSGID'.
  fc-ref_tabname = 'SYST'.
  APPEND fc.
  fc-fieldname = 'MSGNO'.
  fc-ref_tabname = 'SYST'.
  APPEND fc.
  fc-fieldname = 'TEXT'.
  fc-ref_tabname = 'T100'.
  APPEND fc.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            i_callback_program       = alv_repid
            i_callback_pf_status_set = 'SET_STATUS_ARCHIVE_POPUP'
            it_fieldcat              = fc[]
            i_default                = ' '
            i_save                   = ' '
            i_screen_start_column    = 3
            i_screen_start_line      = 3
            i_screen_end_column      = 107
            i_screen_end_line        = 20
       TABLES
            t_outtab                 = outtab.
ENDFORM.

************************************************************************
* Status for the popup with the archive messages
************************************************************************
FORM set_status_archive_popup USING rt_extab TYPE slis_t_extab.
  SET TITLEBAR '002'.
ENDFORM.
************************************************************************
* The report replaces MB51 (matdocs for material) and MB59
* (matdocs for posting date). We place the cursor into the proper field
************************************************************************
FORM place_cursor.
  STATICS done VALUE ' '.
  CHECK done IS INITIAL.
  done = 'X'.
  CASE sy-tcode.
    WHEN 'MB51'.
      SET CURSOR FIELD 'MATNR-LOW'.
    WHEN 'MB59'.
      SET CURSOR FIELD 'BUDAT-LOW'.
  ENDCASE.
ENDFORM.                    " place_cursor
************************************************************************
* Set the "short documents"-checkbox if archive is set.
************************************************************************
FORM checkboxes_set.
  IF archive = 'X'.
    shortdoc = 'X'.
*   Disable function.
    LOOP AT SCREEN.
      IF screen-name CS 'SHORTDOC'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.
************************************************************************
* Auto-repair.
* This coding ensures that MMIM_REP_CUST fulfills the minimum
* requirements as layed down in RM07DOCS_CONTROL.
* In theory, this step is never necessary as MMIM_REP_CUST has
* been delivered correctly and the customizing view prevents
* mandatory entries from beeing deleted. But we have learned
* that this trust is never justified.
************************************************************************
FORM auto_repair.                                           "215043
  DATA:
    ls_cust      TYPE mmim_rep_cust,
    lt_cust      TYPE STANDARD TABLE OF mmim_rep_cust,
    ls_x031l     TYPE x031l,
    lt_x031l     TYPE STANDARD TABLE OF x031l,
    lt_x031ltemp TYPE STANDARD TABLE OF x031l,
    l_changed,
    l_tabix      TYPE sytabix,
    l_dtg        LIKE mmim_rep_cust-dtg.
* Create tables for processing.
  PERFORM build_controltable.
  SELECT * FROM mmim_rep_cust INTO TABLE lt_cust
           WHERE report = 'RM07DOCS'.
  CLEAR lt_x031ltemp.
  CALL FUNCTION 'DDIF_NAMETAB_GET'
       EXPORTING
            tabname   = 'MKPF'
       TABLES
            x031l_tab = lt_x031ltemp.
  INSERT LINES OF lt_x031ltemp INTO TABLE lt_x031l.
  CLEAR lt_x031ltemp.
  CALL FUNCTION 'DDIF_NAMETAB_GET'
       EXPORTING
            tabname   = 'MSEG'
       TABLES
            x031l_tab = lt_x031ltemp.
  INSERT LINES OF lt_x031ltemp INTO TABLE lt_x031l.
  CONCATENATE sy-datum sy-uzeit INTO l_dtg.
* Loop over the control table and ensure that mandatory fields are OK
  LOOP AT ct.
    CLEAR ls_cust.
    CLEAR l_tabix.
*   Read the entry in the customizing table. If not present yet,
*   LS_CUST is empty and L_TABIX = 0, otherwise L_TABIX points
*   to the entry.
    READ TABLE lt_cust INTO ls_cust WITH KEY tabname   = ct-tabname
                                             fieldname = ct-fieldname.
    IF sy-subrc = 0.
      l_tabix = sy-tabix.
    ENDIF.
    IF ( ct-selection = '+' AND ls_cust-selpos = space ) OR
       ( ct-output    = '+' AND ls_cust-outpos = space ).
*     Entry needs to be added/modified, as mandatory fields are not
*     activated.
      ls_cust-report    = 'RM07DOCS'.
      ls_cust-tabname   = ct-tabname.
      ls_cust-fieldname = ct-fieldname.
      IF ct-selection = '+'.
        ls_cust-selpos = 'X'.
      ENDIF.
      IF ct-output = '+'.
        ls_cust-outpos = 'X'.
      ENDIF.
*     For entirely new entries, the memory ID is read from DDIC
      IF l_tabix IS INITIAL.
        READ TABLE lt_x031l INTO ls_x031l
                            WITH KEY tabname = ct-tabname
                                     fieldname = ct-fieldname.
        IF sy-subrc = 0.
          ls_cust-memoryid = ls_x031l-memoryid.
        ENDIF.
      ENDIF.
      ls_cust-dtg = l_dtg.
*     Insert or modify the customizing mirror table.
      IF l_tabix = 0.
        APPEND ls_cust TO lt_cust.
      ELSE.
        MODIFY lt_cust FROM ls_cust INDEX l_tabix.
      ENDIF.
*     Ensure that the data are transported back to the database.
      l_changed = 'X'.
    ENDIF.
  ENDLOOP.
* Loop over the database table (maybe enlarged by the previous step).
  LOOP AT lt_cust INTO ls_cust.
*   Fields which are disabled in the controltable
    READ TABLE ct WITH KEY tabname   = ls_cust-tabname
                           fieldname = ls_cust-fieldname.
    IF sy-subrc = 0.
      IF ct-selection = '-' AND ls_cust-selpos = 'X'.
        ls_cust-selpos = space.
        ls_cust-dtg    = l_dtg.
        MODIFY lt_cust FROM ls_cust.
        l_changed = 'X'.
      ENDIF.
      IF ct-output = '-' AND ls_cust-outpos = 'X'.
        ls_cust-outpos = space.
        ls_cust-dtg    = l_dtg.
        MODIFY lt_cust FROM ls_cust.
        l_changed = 'X'.
      ENDIF.
      IF ct-selection = '-' AND ct-output = '-'.
        DELETE lt_cust.
        l_changed = 'X'.
        CONTINUE.
      ENDIF.
    ENDIF.
*   A selection field needs to be an output field, too.
    IF ls_cust-selpos = 'X' AND ls_cust-outpos IS INITIAL.
      ls_cust-outpos = 'X'.
      ls_cust-dtg    = l_dtg.
      MODIFY lt_cust FROM ls_cust.
      l_changed      = 'X'.
    ENDIF.
*   Fields which are unknown to MKPF and MSEG are removed
    READ TABLE lt_x031l WITH KEY tabname   = ls_cust-tabname
                                 fieldname = ls_cust-fieldname
                        TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      DELETE lt_cust.
      l_changed = 'X'.
      CONTINUE.
    ENDIF.
  ENDLOOP.
* If there was a change of customizing necessary, write it to the
* database and commit. Before, roll back so that any other data
* are rejected.
  IF l_changed = 'X'.
    ROLLBACK WORK.
    DELETE FROM mmim_rep_cust WHERE report = 'RM07DOCS'.
    INSERT mmim_rep_cust FROM TABLE lt_cust.
    COMMIT WORK AND WAIT.
    MESSAGE s895(m7) WITH                                   "n555893
      'AUTO_REPAIR executed. See note 215043.'.    "#EC NOTEXT "n555893
  ENDIF.

ENDFORM.                                                    "215043

*-----------------------------------------------------------"n555246
*    tpc_write_log                                          "n555246
*-----------------------------------------------------------"n555246
                                                            "n555246
*---------------------------------------------------------------------*
*       FORM tpc_write_log                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM tpc_write_log.                                         "n555246
                                                            "n555246
* check whether the function module is available            "n555246
  CALL FUNCTION 'FUNCTION_EXISTS'                           "n555246
    EXPORTING                                               "n555246
      funcname           = 'CA_WRITE_LOG'                   "n555246
    EXCEPTIONS                                              "n555246
      function_not_exist = 1                                "n555246
      OTHERS             = 2.                               "n555246
                                                            "n555246
  CHECK : sy-subrc IS INITIAL.                              "n555246
                                                            "n555246
* write the entries of the selection screen into log file   "n555246
  CALL FUNCTION         'CA_WRITE_LOG'         "#EC EXISTS     "n555246
        EXPORTING                                           "n555246
          i_program     = g_f_repid                         "n555246
        EXCEPTIONS                                          "n555246
          write_error   = 1                                 "n555246
          OTHERS        = 2.                                "n555246
                                                            "n555246
  IF sy-subrc IS INITIAL.                                   "n555246
    COMMIT WORK.                                            "n555246
  ELSE.                                                     "n555246
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno       "n555246
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.   "n555246
  ENDIF.                                                    "n555246
                                                            "n555246
ENDFORM.                     "tpc_write_log                 "n555246
                                                            "n555246
*-----------------------------------------------------------"n555246





*&---------------------------------------------------------------------*
*&      Form  delete_document
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_document.
*--- if GR document(101) has already been canceled,
*---    it is not display...
  LOOP AT itab.
    SELECT SINGLE mblnr INTO mseg-mblnr
                        FROM mseg
                       WHERE smbln EQ itab-mblnr
                         AND sjahr EQ itab-mjahr
                         AND smblp EQ itab-zeile
                         AND bwart EQ '102'.
    IF sy-subrc EQ 0.
      DELETE itab.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " delete_document

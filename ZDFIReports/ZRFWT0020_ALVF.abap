*----------------------------------------------------------------------*
*   INCLUDE RFWT0020_ALVF                                              *
*----------------------------------------------------------------------*


*---------------------------------------------------------------------*
*       FORM print_table                                              *
*---------------------------------------------------------------------*
*       Call the ALV                                                  *
*---------------------------------------------------------------------*
*  -->  lt_outtab                     Output table                    *
*  -->  l_check_append                Display table if flg_notp = 'X'?*
*  -->  l_check_avp                   Display table if flg_notp = ' '?*
*  -->  l_variant_handle              Handle for the display variant  *
*  -->  l_variant_name                Name of the display variant     *
*  -->  l_next_form                   Form, called at the end of this *
*                                     form                            *
*  -->  l_top_of_page_form            Form, called at top of page     *
*  -->  l_create_fieldcat_form        Form, called to create the field*
*                                     catalog with field descriptions *
*---------------------------------------------------------------------*
FORM print_table
  TABLES lt_outtab
  USING
    l_variant_handle       TYPE disvariant-handle
    l_next_form            TYPE slis_alv_event-form
    l_top_of_page_form     TYPE slis_alv_event-form
    l_create_fieldcat_form TYPE slis_alv_event-form.

  DATA:    lt_fieldcat  TYPE slis_t_fieldcat_alv,     "Field catalog
           lt_alv_event TYPE slis_t_event,            "Events by ALV
           l_layout     TYPE slis_layout_alv,         "Layout info.
           l_variant    TYPE disvariant.              "Display variant

* This form is called the first time?
  STATICS: s_flg_fiti   TYPE c VALUE 'X'.

  flg_print_header = 'X'.
**  Initialization
*   Fill structure l_variant
  l_variant-handle  = l_variant_handle.
  l_variant-report  = g_repid.

*   Fill table lt_alv_event
*  PERFORM append_event   USING    slis_ev_end_of_list
*                                  l_next_form
*                         CHANGING lt_alv_event.

  IF s_flg_fiti = 'X'.                 "First List
    PERFORM append_event   USING    slis_ev_top_of_list
                                    'TOP_OF_LIST'
                           CHANGING lt_alv_event.
  ENDIF.

  PERFORM append_event     USING    slis_ev_top_of_page
                                    l_top_of_page_form
                              CHANGING lt_alv_event.

*   Fill table lt_fieldcat
  PERFORM (l_create_fieldcat_form) IN PROGRAM (g_repid)
                           CHANGING lt_fieldcat.

*   Fill structure l_layout
  PERFORM create_layout USING    s_flg_fiti
                        CHANGING l_layout.

**  Print the list
  IF s_flg_fiti = 'X'.                 "Display the first list
    CLEAR s_flg_fiti.
  ENDIF.

*     Call the ALV
  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            i_callback_program = g_repid
            is_layout          = l_layout
            it_excluding       = gt_excluding
            it_fieldcat        = lt_fieldcat
            it_events          = lt_alv_event
       TABLES
            t_outtab           = lt_outtab.
ENDFORM.                               "print_table



*---------------------------------------------------------------------*
*       FORM print_with_item                                          *
*---------------------------------------------------------------------*
*       Display table i_added_with_item                               *
*---------------------------------------------------------------------*
FORM print_with_item.
  PERFORM print_table
            TABLES
               i_added_with_item
            USING
               c_with_item             "l_variant_handle
               'PRINT_CHANGED_BSEG'    "l_next_form
               'TOP_OF_PAGE_WITH_ITEM' "l_top_of_page_form
               'CREATE_FIELDCAT_WITH_ITEM'.     "l_create_fieldcat_form
ENDFORM.                               "print_with_item

*---------------------------------------------------------------------*
*       FORM print_xqfor_with_item                                    *
*---------------------------------------------------------------------*
*       Display table i_xqfor_with_item                               *
*---------------------------------------------------------------------*
*--- not in use at the moment
FORM print_xqfor_with_item.
  PERFORM print_table
            TABLES
               i_xqfor_with_item
            USING
               c_xqfor_with_item       "l_variant_handle
               'PRINT_CHANGED_BSEG'    "l_next_form
               'TOP_OF_PAGE_XQFOR_WITH_ITEM'     "l_top_of_page_form
               'CREATE_FIELDCAT_XQFOR_ITEM'.     "l_create_fieldcat_form
ENDFORM.                               "print_xqfor_with_item


*---------------------------------------------------------------------*
*       FORM print_changed_bseg
*
*---------------------------------------------------------------------*
*       Display table i_changed_bseg
*
*---------------------------------------------------------------------*
FORM print_changed_bseg.
  PERFORM print_table
            TABLES
               i_changed_bseg
            USING
               c_changed_bseg          "l_variant_handle
               'PRINT_LOCKED_DOCUMENTS'"l_next_form
               'TOP_OF_PAGE_BSEG'      "l_top_of_page_form
               'CREATE_FIELDCAT_BSEG'. "l_create_fieldcat_form
ENDFORM.                               "print_added_bseg

*---------------------------------------------------------------------*
*       FORM print_error_bseg
*
*---------------------------------------------------------------------*
*       Display table i_error_bseg
*
*---------------------------------------------------------------------*
*--- not in use at the moment
FORM print_error_bseg.
  PERFORM print_table
            TABLES
               i_error_bseg
            USING
               c_error_bseg            "l_variant_handle
               'PRINT_LOCKED_DOCUMENTS'"l_next_form
               'TOP_OF_PAGE_E_BSEG'    "l_top_of_page_form
               'CREATE_FIELDCAT_BSEG'. "l_create_fieldcat_form

ENDFORM.                               "print_added_bseg

*---------------------------------------------------------------------*
*       FORM print_locked_documents                                   *
*---------------------------------------------------------------------*
*       Table locked_documents                                        *
*---------------------------------------------------------------------*
FORM print_locked_documents.
  PERFORM print_table
  TABLES
  locked_documents
  USING
  c_locked_documents                   "l_variant_handle
  'PRINT_LOCKED_ENTRIES'               "l_next_form
  'TOP_OF_PAGE_LOCKED_DOCUMENTS'       "l_top_of_page_form
  'CREATE_FIELDCAT_DOCUMENTS'.         "l_create_fieldcat_form
ENDFORM.


*---------------------------------------------------------------------*
*       FORM print_locked_entries                                     *
*---------------------------------------------------------------------*
*       locked_entries                                                *
*---------------------------------------------------------------------*

FORM print_locked_entries.
  PERFORM print_table
          TABLES
          locked_entries
          USING
             c_locked_entries          "l_variant_handle
             'PRINT_UNCHANGEABLE_BSEG' "l_next_form
             'TOP_OF_PAGE_LOCKED_ENTRIES'     "l_top_of_page_form
             'CREATE_FIELDCAT_ENTRIES'."l_create_fieldcat_form
ENDFORM.                               "print_locked_entries

*&---------------------------------------------------------------------*
*&      Form  print_unchangeable_bseg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM print_unchangeable_bseg.
  PERFORM print_table
          TABLES
          i_unchangeable_bseg
          USING
             c_unchangeable            "l_variant_handle
             ' '                       "l_next_form
             'TOP_OF_PAGE_UNCHANGEABLE'"l_top_of_page_form
             'CREATE_FIELDCAT_DOCUMENTS'.  "l_create_fieldcat_form

ENDFORM.                               " print_unchangeable_bseg

*---------------------------------------------------------------------*
*       FORM append_fcode                                             *
*---------------------------------------------------------------------*
FORM append_fcode USING u_fcode TYPE rsmpe-func.
  APPEND u_fcode TO gt_excluding.
ENDFORM.                               "append_fcode


*---------------------------------------------------------------------*
*       FORM append_event                                             *
*---------------------------------------------------------------------*
*  -->  u_name           Name of the event                            *
*  -->  u_form           Name of the form, called during the event    *
*---------------------------------------------------------------------*
FORM append_event USING    u_name TYPE slis_alv_event-name
                           u_form TYPE slis_alv_event-form
                  CHANGING ct_event     TYPE slis_t_event.
  DATA: l_alv_event TYPE slis_alv_event.

  l_alv_event-name = u_name.
  l_alv_event-form = u_form.
  APPEND l_alv_event TO ct_event.
ENDFORM.                               "append_event



*---------------------------------------------------------------------*
*       FORM create_layout                                            *
*---------------------------------------------------------------------*
*       Create list layout specifications for the ALV                 *
*---------------------------------------------------------------------*
*  -->  l_flg_fiti     First List?                                    *
*---------------------------------------------------------------------*
FORM create_layout USING    l_flg_fiti TYPE c
                   CHANGING l_layout TYPE slis_layout_alv.

  IF l_flg_fiti = 'X'.
    l_layout-min_linesize = 250.       "Place for the following lists
    l_layout-get_selinfos = 'X'.       "Print report selections
  ELSE.
    l_layout-list_append = 'X'.        "This list is a Append-List
  ENDIF.
  IF flg_notp = 'X'.
    l_layout-no_hotspot = 'X'.         "Headings not as a Hotspot
  ELSE.
    l_layout-group_change_edit = 'X'.  "User can enter format options
  ENDIF.
  l_layout-no_totalline = 'X'.         "No total record
ENDFORM.                               "create_layout





*---------------------------------------------------------------------*
*       FORM CREATE_FIELDCAT_WITH_ITEM                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  l_handle                                                      *
*  -->  p_lt_fieldcat                                                 *
*---------------------------------------------------------------------*
FORM create_fieldcat_with_item
           CHANGING p_lt_fieldcat TYPE slis_t_fieldcat_alv.

  DATA:i_dd03p LIKE dd03p OCCURS 1 WITH HEADER LINE,
       char_field TYPE c.

*--- get fieldnames from with_item
  CALL FUNCTION 'DDIF_TABL_GET'
       EXPORTING
            name      = 'WITH_ITEM'
       TABLES
            dd03p_tab = i_dd03p.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  LOOP AT i_dd03p.
    CASE i_dd03p-fieldname.
      WHEN 'BUKRS' OR 'BELNR' OR 'BUZEI' OR 'GJAHR' OR 'WITHT'
        OR 'WT_WITHCD' OR 'WT_ACCO' OR 'AUGBL' OR 'AUGDT' OR 'WT_QBSHB'
        OR 'WT_QSSHB'.
        DELETE i_dd03p.
    ENDCASE.
  ENDLOOP.

  i_dd03p-fieldname = 'BUKRS'. APPEND i_dd03p.
  i_dd03p-fieldname = 'WT_ACCO'. APPEND i_dd03p.
  i_dd03p-fieldname = 'BELNR'. APPEND i_dd03p.
  i_dd03p-fieldname = 'AUGBL'. APPEND i_dd03p.
  i_dd03p-fieldname = 'AUGDT'. APPEND i_dd03p.
  i_dd03p-fieldname = 'BUZEI'. APPEND i_dd03p.
  i_dd03p-fieldname = 'WT_WITH'. APPEND i_dd03p.
  i_dd03p-fieldname = 'WT_WITHCD'. APPEND i_dd03p.
  i_dd03p-fieldname = 'WT_QSSHB'. APPEND i_dd03p.
  i_dd03p-fieldname = 'WT_QBSHB'. APPEND i_dd03p.
*--- activate respective fields for alv
  LOOP AT i_dd03p.
    CASE i_dd03p-fieldname.
      WHEN 'BUKRS' OR 'BELNR' OR 'BUZEI' OR 'GJAHR' OR 'WITHT'
        OR 'WT_WITHCD' OR 'WT_ACCO' OR 'AUGBL' OR 'AUGDT' OR 'WT_QBSHB'
        OR 'WT_QSSHB'.
        char_field = space.
      WHEN OTHERS.
        char_field = 'X'.
    ENDCASE.

    PERFORM append_fieldcat
       TABLES p_lt_fieldcat
                USING:
                 i_dd03p-fieldname
                 'i_added_bsak_with_item'
                 'with_item'
                 char_field.
  ENDLOOP.

ENDFORM.                               "create_fieldcat_with_item

*---------------------------------------------------------------------*
*       FORM CREATE_FIELDCAT_XQFORWITH_ITEM                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  l_handle                                                      *
*  -->  p_lt_fieldcat                                                 *
*---------------------------------------------------------------------*
FORM create_fieldcat_xqfor_item
           CHANGING p_lt_fieldcat TYPE slis_t_fieldcat_alv.

  DATA:i_dd03p LIKE dd03p OCCURS 1 WITH HEADER LINE,
       char_field TYPE c.

*--- get fieldnames from with_item
  CALL FUNCTION 'DDIF_TABL_GET'
       EXPORTING
            name      = 'WITH_ITEM'
       TABLES
            dd03p_tab = i_dd03p.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  LOOP AT i_dd03p.
    CASE i_dd03p-fieldname.
      WHEN 'BUKRS' OR 'BELNR' OR 'BUZEI' OR 'GJAHR' OR 'WITHT'
        OR 'WT_WITHCD' OR 'WT_ACCO' OR 'AUGBL' OR 'AUGDT' OR 'WT_QBSHB'
        OR 'WT_QSSHB'.
        DELETE i_dd03p.
    ENDCASE.
  ENDLOOP.

  i_dd03p-fieldname = 'BUKRS'. APPEND i_dd03p.
  i_dd03p-fieldname = 'WT_ACCO'. APPEND i_dd03p.
  i_dd03p-fieldname = 'BELNR'. APPEND i_dd03p.
  i_dd03p-fieldname = 'AUGBL'. APPEND i_dd03p.
  i_dd03p-fieldname = 'AUGDT'. APPEND i_dd03p.
  i_dd03p-fieldname = 'BUZEI'. APPEND i_dd03p.
  i_dd03p-fieldname = 'WT_WITH'. APPEND i_dd03p.
  i_dd03p-fieldname = 'WT_WITHCD'. APPEND i_dd03p.
  i_dd03p-fieldname = 'WT_QSSHB'. APPEND i_dd03p.
  i_dd03p-fieldname = 'WT_QBSHB'. APPEND i_dd03p.

*--- activate respective fields for alv
  LOOP AT i_dd03p.
    CASE i_dd03p-fieldname.
      WHEN 'BUKRS' OR 'BELNR' OR 'BUZEI' OR 'GJAHR' OR 'WITHT'
        OR 'WT_WITHCD' OR 'WT_ACCO' OR 'AUGBL' OR 'AUGDT' OR 'WT_QBSHB'
        OR 'WT_QSSHB'.
        char_field = space.
      WHEN OTHERS.
        char_field = 'X'.
    ENDCASE.

    PERFORM append_fieldcat
       TABLES p_lt_fieldcat
                USING:
                 i_dd03p-fieldname
                 'i_added_bsak_with_item'
                 'with_item'
                 char_field.
  ENDLOOP.

ENDFORM.                               "create_fieldcat_xqfor_with_item


*---------------------------------------------------------------------*
*       FORM CREATE_FIELDCAT_BSEG                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  l_handle                                                      *
*  -->  p_lt_fieldcat                                                 *
*---------------------------------------------------------------------*
FORM create_fieldcat_bseg
           CHANGING p_lt_fieldcat TYPE slis_t_fieldcat_alv.

  DATA:i_dd03p LIKE dd03p OCCURS 1 WITH HEADER LINE,
       char_field TYPE c.

*--- get fieldnames from with_item
  CALL FUNCTION 'DDIF_TABL_GET'
       EXPORTING
            name      = 'BSEG'
       TABLES
            dd03p_tab = i_dd03p.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  LOOP AT i_dd03p.
    CASE i_dd03p-fieldname.
      WHEN 'BUKRS' OR 'BELNR' OR 'BUZEI' OR  'WITHT'
        OR 'QSSKZ' OR 'LIFNR' OR 'AUGBL' OR 'AUGDT' OR 'QSSHB'
        OR 'QSFBT'.
        DELETE i_dd03p.
    ENDCASE.
  ENDLOOP.

  i_dd03p-fieldname = 'BUKRS'. APPEND i_dd03p.
  i_dd03p-fieldname = 'LIFNR'. APPEND i_dd03p.
  i_dd03p-fieldname = 'BELNR'. APPEND i_dd03p.
  i_dd03p-fieldname = 'AUGBL'. APPEND i_dd03p.
  i_dd03p-fieldname = 'AUGDT'. APPEND i_dd03p.
  i_dd03p-fieldname = 'BUZEI'. APPEND i_dd03p.
  i_dd03p-fieldname = 'QSSKZ'. APPEND i_dd03p.
  i_dd03p-fieldname = 'QSSHB'. APPEND i_dd03p.
  i_dd03p-fieldname = 'QSFBT'. APPEND i_dd03p.
*--- activate respective fields for alv
  LOOP AT i_dd03p.
    CASE i_dd03p-fieldname.
      WHEN 'BUKRS' OR 'BELNR' OR 'BUZEI' OR 'QSSHB'
        OR 'QSSKZ' OR 'QSFBT' OR 'AUGBL' OR 'AUGDT' OR 'LIFNR'.
        char_field = space.
      WHEN OTHERS.
        char_field = 'X'.
    ENDCASE.

    PERFORM append_fieldcat
       TABLES p_lt_fieldcat
                USING:
                 i_dd03p-fieldname
                 'i_changed_bseg'
                 'bseg'
                 char_field.
  ENDLOOP.

ENDFORM.                               "create_fieldcat_with_item


*---------------------------------------------------------------------*
*       FORM CREATE_FIELDCAT_DOCUMENTS                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  l_handle                                                      *
*  -->  p_lt_fieldcat                                                 *
*---------------------------------------------------------------------*
FORM create_fieldcat_documents
CHANGING p_lt_fieldcat TYPE slis_t_fieldcat_alv.

  DATA: char_field TYPE c.
  DATA: l_fieldcat TYPE slis_fieldcat_alv. "field string
  char_field = space.

  l_fieldcat-fieldname = 'BUKRS'.
  l_fieldcat-ref_fieldname = 'BUKRS'.
  l_fieldcat-tabname = 'LOCKED_DOCUMENTS'.
  l_fieldcat-ref_tabname = 'BSEG'.
  l_fieldcat-no_out = char_field.
  l_fieldcat-seltext_s = 'BuKr'.

  APPEND l_fieldcat TO p_lt_fieldcat.

  l_fieldcat-fieldname = 'BELNR'.
  l_fieldcat-ref_fieldname = 'BELNR'.
  l_fieldcat-tabname = 'LOCKED_DOCUMENTS'.
  l_fieldcat-ref_tabname = 'BSEG'.
  l_fieldcat-no_out = char_field.
  l_fieldcat-seltext_s = 'Belnr'.

  APPEND l_fieldcat TO p_lt_fieldcat.

  l_fieldcat-fieldname = 'GJAHR'.
  l_fieldcat-ref_fieldname = 'GJAHR'.
  l_fieldcat-tabname = 'LOCKED_DOCUMENTS'.
  l_fieldcat-ref_tabname = 'BSEG'.
  l_fieldcat-no_out = char_field.
  l_fieldcat-seltext_s = 'GJahr'.

  APPEND l_fieldcat TO p_lt_fieldcat.

  l_fieldcat-fieldname = 'BUZEI'.
  l_fieldcat-ref_fieldname = 'BUZEI'.
  l_fieldcat-tabname = 'LOCKED_DOCUMENTS'.
  l_fieldcat-ref_tabname = 'BSEG'.
  l_fieldcat-no_out = char_field.
  l_fieldcat-seltext_s = 'Pos'.

  APPEND l_fieldcat TO p_lt_fieldcat.

ENDFORM.                               "create_fieldcat_documents

*---------------------------------------------------------------------*
*       FORM CREATE_FIELDCAT_ENTRIES                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  l_handle                                                      *
*  -->  p_lt_fieldcat                                                 *
*---------------------------------------------------------------------*
FORM create_fieldcat_entries
           CHANGING p_lt_fieldcat TYPE slis_t_fieldcat_alv.

  DATA:i_dd03p LIKE dd03p OCCURS 1 WITH HEADER LINE,
       char_field TYPE c.

*--- get fieldnames from with_item
  CALL FUNCTION 'DDIF_TABL_GET'
       EXPORTING
            name      = 'REGUS'
       TABLES
            dd03p_tab = i_dd03p.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*--- activate respective fields for alv
  LOOP AT i_dd03p.
    CASE i_dd03p-fieldname.
      WHEN 'BUKRS' OR 'KOART' OR 'KONKO' OR 'LAUFD' OR 'LAUFI' OR
           'UMSKL'.
        char_field = space.
      WHEN OTHERS.
        char_field = 'X'.
    ENDCASE.

    PERFORM append_fieldcat
       TABLES p_lt_fieldcat
                USING:
                 i_dd03p-fieldname
                 'locked_entries'
                 'regus'
                 char_field.
  ENDLOOP.

ENDFORM.                               "create_fieldcat_entries


**---------------------------------------------------------------------*
**       FORM append_fieldcat                                          *
**---------------------------------------------------------------------*
FORM append_fieldcat
      TABLES
        l_lt_fieldcat
      USING
        u_l_fieldname   TYPE slis_fieldcat_alv-fieldname
        u_l_tabname TYPE slis_fieldcat_alv-ref_tabname
        u_l_ref_tabname TYPE slis_fieldcat_alv-ref_tabname
        u_l_no_out       TYPE slis_fieldcat_alv-no_out.
  DATA: l_fieldcat TYPE slis_fieldcat_alv.        "field string

  l_fieldcat-fieldname      = u_l_fieldname.
  l_fieldcat-ref_tabname    = u_l_ref_tabname.
  l_fieldcat-tabname        = u_l_tabname.
  l_fieldcat-no_out         = u_l_no_out.

  APPEND l_fieldcat TO l_lt_fieldcat.

ENDFORM.                               "append_fieldcat

*---------------------------------------------------------------------*
*       FORM top_of_page_with_item                                    *
*---------------------------------------------------------------------*
FORM top_of_page_with_item.                                 "#EC CALLED
  PERFORM print_heading USING c_with_item.
ENDFORM.                               "top_of_page_with_item

*---------------------------------------------------------------------*
*       FORM top_of_page_xqfor_with_item                              *
*---------------------------------------------------------------------*
FORM top_of_page_xqfor_with_item.                           "#EC CALLED
  PERFORM print_heading USING c_xqfor_with_item.
ENDFORM.                               "top_of_page_xqfor_with_item

*---------------------------------------------------------------------*
*       FORM top_of_page_bseg                                         *
*---------------------------------------------------------------------*
FORM top_of_page_bseg.                                      "#EC CALLED
  PERFORM print_heading USING c_changed_bseg.
ENDFORM.                               "top_of_page_xqfor_with_item

*---------------------------------------------------------------------*
*       FORM top_of_page_e_bseg                                       *
*---------------------------------------------------------------------*
FORM top_of_page_e_bseg.                                    "#EC CALLED
  PERFORM print_heading USING c_error_bseg.
ENDFORM.                               "top_of_page_xqfor_with_item

*---------------------------------------------------------------------*
*       FORM top_of_page_unchangeable
*
*---------------------------------------------------------------------*
FORM top_of_page_unchangeable.                              "#EC CALLED
  PERFORM print_heading USING c_unchangeable.
ENDFORM.                               "top_of_page_xqfor_with_item

*---------------------------------------------------------------------*
*       FORM top_of_page_locked_entries                                *
*---------------------------------------------------------------------*
FORM top_of_page_locked_entries.                            "#EC CALLED
  PERFORM print_heading USING c_locked_entries.
ENDFORM.                               "top_of_page_locked_entries

*---------------------------------------------------------------------*
*       FORM top_of_page_locked_documents                             *
*---------------------------------------------------------------------*
FORM top_of_page_locked_documents.                          "#EC CALLED
  PERFORM print_heading USING c_locked_documents.
ENDFORM.                               "top_of_page_locked_documents

*---------------------------------------------------------------------*
*       FORM print_heading                                            *
*---------------------------------------------------------------------*
*       Print a header                                                *
*---------------------------------------------------------------------*
FORM print_heading USING u_handl TYPE slis_handl.
  DATA: l_text(55) TYPE c,             "Name of the list
        l_length TYPE int4.            "Width of the list

  IF NOT flg_print_header IS INITIAL.

* How wide is the list?
    IF u_handl = c_with_item.

      CALL FUNCTION 'REUSE_ALV_LIST_LAYOUT_INFO_GET'
           IMPORTING
                e_width = l_length
           EXCEPTIONS
                OTHERS  = 3.

      FORMAT COLOR COL_TOTAL INTENSIFIED ON.
      ULINE AT /(l_length).
      WRITE: / sy-vline.
      IF test = 'X'.
        WRITE text-022.
      ELSE.
        WRITE text-021.
      ENDIF.
      WRITE AT l_length sy-vline.
      ULINE AT /(l_length).
      FORMAT COLOR COL_BACKGROUND.
      WRITE /.

    ENDIF.
*    PERFORM batch-heading(rsbtchh0).   "Print standard list heading

    CALL FUNCTION 'REUSE_ALV_LIST_LAYOUT_INFO_GET'
         IMPORTING
              e_width = l_length
         EXCEPTIONS
              OTHERS  = 3.

    CASE u_handl.
      WHEN c_with_item.
        l_text = text-009.
      WHEN c_xqfor_with_item.
        l_text = text-025.
      WHEN c_locked_documents.
        l_text = text-018.
      WHEN c_locked_entries.
        l_text = text-019.
      WHEN c_changed_bseg.
        l_text = text-026.
      WHEN c_error_bseg.
        l_text = text-027.
      when c_unchangeable.
        l_text = text-030.
    ENDCASE.

    SKIP.
    ULINE AT /(l_length).
    WRITE: / sy-vline,
             l_text.
    IF l_length > 15.
      WRITE AT l_length(1) sy-vline.
    ENDIF.
  ENDIF.
ENDFORM.                               "print_heading

*---------------------------------------------------------------------*
*       FORM top_of_list                                              *
*---------------------------------------------------------------------*
*       Gibt bei Bedarf eine Zusätzliche Textzeile über den Listen aus*
*---------------------------------------------------------------------*
FORM top_of_list.                                           "#EC CALLED
*  PERFORM print_zusatztitel.
ENDFORM.                               "top_of_list

*---------------------------------------------------------------------*
*       FORM create_gt_excluding                                      *
*---------------------------------------------------------------------*
*       Fill the table gt_excluding:                                  *
*       Table of inactive function codes for the ALV                  *
*---------------------------------------------------------------------*
FORM create_gt_excluding.
  REFRESH gt_excluding.
  PERFORM append_fcode USING: '&SUM',  "Zwischensummen
                              '%SL',   "e-mail versenden
                              '&NFO',  " Selektion
                              '&CRB',  "Scrollen
                              '&CRL',  "Scrollen
                              '&CRR',  "Scrollen
                              '&CRE',  "Scrollen
                              '&AVE',  "Variante sichern
                              '&UMC',  "Summe
                              '&OMP',  "Nur Summ.
                              '&XPA',  "Sum. aufr.
                              '&ETA',  "Detail
                              '&OPT',  "Spalte op.
                              '&OLX',  "Anz. def.
                              '&ERW',  "Anz. Verw.
                              '&KOM',  "Sum. ausw.
                              '&AUF',  "Aufriss
                              '&CDF',  "Fixi. auf.
                              '&CFI',  "Spalte fi.
                              '&DAU',  "Tren. aut.
                              '&DOF',  "Tren. aus
                              '&LFO',  "Liststatus
                              '&XXL',  "Tabellenkal.
                              '&AQW',  "Textverarb.
                              '&DON',  "Tren. ein
                              '&IC1',  "Doppelklick
                              '&LIS',  "Grundliste
                              '&ABC',  "ABC Analye
                              '&ILT',  "Filter
                              '&ILD',  "Filter lö
                              '&OL0',
                              '&ODN',
                              '&OUP'.
ENDFORM.                               "create_gt_excluding

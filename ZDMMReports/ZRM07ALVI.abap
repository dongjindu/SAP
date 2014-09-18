***INCLUDE RM07ALVI .

* Dec. 2002 MM                                              "n579976
* M7 393 when user deletes the initial display variant      "n579976

* Sept 2002 MM                                              "n555893
* - colourize the numeric values in the list depending on   "n555893
*   the customizing settings in VIEW  V_MMIM_REP_PRINT      "n555893

* Type pool
type-pools: SLIS.
tables: mmim_rep_print.

* Global variables for handling ALV functionality
DATA: alv_keyinfo      TYPE slis_keyinfo_alv.
DATA: alv_variant      LIKE disvariant.
DATA: alv_layout       TYPE slis_layout_alv.
data: alv_repid        like sy-repid.
data: alv_print        type slis_print_alv.
data: alv_detail_func(30).
data: alv_default_variant    like  disvariant-variant.      "n579976
data: alv_colourize_fields   like  mmim_rep_print-color.    "n555893

form alv_init.
  clear: alv_keyinfo, alv_variant, alv_layout, alv_print.
  alv_repid = sy-repid.
  CLEAR alv_variant.
  alv_variant-report = alv_repid.
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save     = 'A'
    CHANGING
      cs_variant = alv_variant
    EXCEPTIONS
      not_found  = 2.
  IF sy-subrc = 0.
    alv_def = alv_variant-variant.
*   save the initial, e.g. default variant                  "n579976
    move alv_variant-variant to  alv_default_variant.       "n579976
  ENDIF.
* Printing settings
  alv_layout-get_selinfos = 'X'.
  alv_layout-group_change_edit = 'X'.
  select single * from mmim_rep_print where report = sy-repid.
  if sy-subrc = 0.
    if mmim_rep_print-selinfo = 'X'.
      alv_print-no_print_selinfos = ' '.
    else.
      alv_print-no_print_selinfos = 'X'.
    endif.
    if mmim_rep_print-coverpage = 'X'.
      alv_print-no_coverpage = ' '.
    else.
      alv_print-no_coverpage = 'X'.
    endif.
    if mmim_rep_print-listinfo = 'X'.
      alv_print-no_print_listinfos = ' '.
    else.
      alv_print-no_print_listinfos = 'X'.
    endif.
    if mmim_rep_print-gridcontrol = 'X'.
      alv_detail_func = 'REUSE_ALV_GRID_DISPLAY'.
    else.
      alv_detail_func = 'REUSE_ALV_LIST_DISPLAY'.
    endif.

*   consider customizing settings regarding the             "n555893
*   colourization of numeric fields                         "n555893
    if mmim_rep_print-color = 'X'.                          "n555893
*     the customizing says : use no colors                  "n555893
      clear                  alv_colourize_fields.          "n555893
    else.                                                   "n555893
*     no restrictions -> work with colours                  "n555893
      move  'X'              to  alv_colourize_fields.      "n555893
    endif.                                                  "n555893

  else.
*   MMIM_REP_PRINT entry missing / work with colours        "n555893
    move  'X'                to  alv_colourize_fields.      "n555893

    alv_print-no_print_selinfos  = 'X'.
    alv_print-no_coverpage       = ' '.
    alv_print-no_print_listinfos = 'X'.
    alv_detail_func = 'REUSE_ALV_LIST_DISPLAY'.
  endif.
endform.

form alv_f4.
  alv_variant-report = alv_repid.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = alv_variant
      i_save     = 'A'
    IMPORTING
      es_variant = alv_variant
    EXCEPTIONS
      not_found  = 2.
  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    alv_def = alv_variant-variant.
  ENDIF.
ENDFORM.

form alv_check.
  alv_variant-report = alv_repid.
  alv_variant-variant = alv_def.
  IF not alv_def is initial.
    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        i_save     = 'A'
      CHANGING
        cs_variant = alv_variant
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
      message e321(m7) with alv_def alv_repid.
    ENDIF.
  else.                                                     "n579976
*   the user wants no initial display variant               "n579976
    if  not alv_default_variant  is initial.                "n579976
*     but the SAP-LIST-VIEWER will apply the existing       "n579976
*     initial display variant / emerge warning 393 ?        "n579976
      CALL FUNCTION          'ME_CHECK_T160M'               "n579976
        EXPORTING                                           "n579976
          I_ARBGB            = 'M7'                         "n579976
          I_MSGNR            = '393'                        "n579976
        EXCEPTIONS                                          "n579976
          NOTHING            = 0                            "n579976
          OTHERS             = 1.                           "n579976
                                                            "n579976
      IF SY-SUBRC <> 0.                                     "n579976
*       list will be created using the initial layout &     "n579976
        message w393(M7)     with  alv_default_variant.     "n579976
      endif.                                                "n579976
    endif.                                                  "n579976
  endif.
endform.

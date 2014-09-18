*&---------------------------------------------------------------------*
*& Report  ZITSE16N
*&
*&---------------------------------------------------------------------*
*& for IT table maintenance purpose
*&
*&---------------------------------------------------------------------*
REPORT  zitse16n.

PARAMETERS:
  p_table TYPE se16n_tab MEMORY ID dtb OBLIGATORY.

PARAMETERS: p_max TYPE sytabix DEFAULT 500.

CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
  EXPORTING
    tcode  = 'SE16N'
  EXCEPTIONS
    ok     = 0
    not_ok = 1.
IF sy-subrc NE 0.
  MESSAGE e059(eu) WITH 'SE16N'.  " no authority
ENDIF.


DATA: gt_se16stab LIKE se16n_seltab OCCURS 0 WITH HEADER LINE.
PERFORM se16stab_init.
*perform se16stab_append
*        using 'AYEAR' 'I' 'EQ' p_ayear ''.
*loop at s_posid.
*  perform se16stab_append
*          using 'POSID' s_posid-sign s_posid-option s_posid-low s_posid-high.
*endloop.

* copied from SE16N_INTERFACE
CALL FUNCTION 'Z_SE16N_INTERFACE'
  EXPORTING
    i_tab                       = p_table
    i_edit                      = 'X'
*   I_SAPEDIT                   = ' '
*   I_NO_TXT                    = ' '
    i_max_lines                 = p_max
*   I_LINE_DET                  = ' '
*   I_DISPLAY                   = 'X'
*   I_CLNT_SPEZ                 = ' '
*   I_CLNT_DEP                  = ' '
*   I_VARIANT                   = ' '
*   I_OLD_ALV                   = ' '
*   I_CHECKKEY                  = ' '
*   I_TECH_NAMES                = ' '
*   I_CWIDTH_OPT_OFF            = ' '
*   I_SCROLL                    = ' '
*   I_NO_CONVEXIT               = ' '
*   I_LAYOUT_GET                = ' '
*   I_ADD_FIELD                 =
*   I_ADD_FIELDS_ON             =
*   I_UNAME                     =
* IMPORTING
*   E_LINE_NR                   =
*   E_DREF                      =
 TABLES
   it_selfields                = gt_se16stab
*   IT_OUTPUT_FIELDS            =
*   IT_OR_SELFIELDS             =
*   IT_CALLBACK_EVENTS          =
*   IT_ADD_UP_CURR_FIELDS       =
*   IT_ADD_UP_QUAN_FIELDS       =
* EXCEPTIONS
*   NO_VALUES                   = 1
*   OTHERS                      = 2
          .
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.

*---------------------------------------------------------------------*
*       FORM se16stab_init                                            *
*---------------------------------------------------------------------*
FORM se16stab_init.
  REFRESH gt_se16stab.
  CLEAR gt_se16stab.
ENDFORM.                    "SE16STAB_INIT
*---------------------------------------------------------------------*
*       FORM se16stab_append                                          *
*---------------------------------------------------------------------*
FORM se16stab_append
     USING i_field LIKE se16n_seltab-field
           i_sign   TYPE char1
           i_option TYPE char2
           i_low  TYPE any
           i_high TYPE any.
  CLEAR gt_se16stab.
  gt_se16stab-field = i_field.
  gt_se16stab-sign = 'I'.

  gt_se16stab-option = 'EQ'.
  gt_se16stab-low    = i_low.
  APPEND gt_se16stab.
ENDFORM.                    "SE16STAB_APPEND

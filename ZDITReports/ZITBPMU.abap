*&---------------------------------------------------------------------*
*& Report  ZITBPMU
*&
*&---------------------------------------------------------------------*
*& Transaction usage
*&
*&---------------------------------------------------------------------*
REPORT  zitbpmu.
TABLES: zthrappusge.

SELECT-OPTIONS:
  s_ldate FOR zthrappusge-ldate,
  s_tcode FOR zthrappusge-tcode,
  s_acct FOR zthrappusge-account.
PARAMETERS: p_max TYPE sytabix DEFAULT 500.

*  call function 'AUTHORITY_CHECK_TCODE'
*    exporting
*      tcode  = 'SE16N'
*    exceptions
*      ok     = 0
*      not_ok = 1.
*  if sy-subrc ne 0.
*    message e059(eu) with 'SE16N'.  " no authority
*  endif.

DATA: gt_se16stab LIKE se16n_seltab OCCURS 0 WITH HEADER LINE.
PERFORM se16stab_init.
*perform se16stab_append
*        using 'AYEAR' 'I' 'EQ' p_ayear ''.
LOOP AT s_tcode.
  PERFORM se16stab_append
          USING 'TCODE' s_tcode-sign s_tcode-option s_tcode-low s_tcode-high.
ENDLOOP.
LOOP AT s_ldate.
  PERFORM se16stab_append
          USING 'LDATE' s_ldate-sign s_ldate-option s_ldate-low s_ldate-high.
ENDLOOP.

LOOP AT s_acct.
  PERFORM se16stab_append
          USING 'ACCOUNT' s_acct-sign s_acct-option s_acct-low s_acct-high.
ENDLOOP.

* copied from SE16N_INTERFACE
CALL FUNCTION 'SE16N_INTERFACE'
  EXPORTING
    i_tab                       = 'ZTHRAPPUSGE'
    i_edit                      = ' '
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

*&---------------------------------------------------------------------*
*& Report  ZAFIU200
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZAFIU200.

*  call function 'AUTHORITY_CHECK_TCODE'
*    exporting
*      tcode  = 'SE16N'
*    exceptions
*      ok     = 0
*      not_ok = 1.
*  if sy-subrc ne 0.
*    message e059(eu) with 'SE16N'.  " no authority
*  endif.

* copied from SE16N_INTERFACE
call function 'Z_SE16N_INTERFACE'
  exporting
    i_tab                       = 'ZTIM008'
    I_EDIT                      = 'X'
*   I_SAPEDIT                   = ' '
*   I_NO_TXT                    = ' '
*   I_MAX_LINES                 = 500
*   I_LINE_DET                  = ' '
*   I_DISPLAY                   = 'X'
*   I_CLNT_SPEZ                 = ' '
    I_CLNT_DEP                  = 'X'
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
* TABLES
*   IT_SELFIELDS                =
*   IT_OUTPUT_FIELDS            =
*   IT_OR_SELFIELDS             =
*   IT_CALLBACK_EVENTS          =
*   IT_ADD_UP_CURR_FIELDS       =
*   IT_ADD_UP_QUAN_FIELDS       =
* EXCEPTIONS
*   NO_VALUES                   = 1
*   OTHERS                      = 2
          .
if sy-subrc <> 0.
* Implement suitable error handling here
endif.

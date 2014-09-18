*&---------------------------------------------------------------------*
*& Report  ZRFFM_INIT_FMVOR                                            *
*&                                                                     *
*&---------------------------------------------------------------------*
*& Modification Log
*& Date        Developer Issue No    Description
*&======================================================================
*& 08/09/2012  Valerian  UD1K955377  Implement ZRFFM_INIT_FMVOR as to
*&                                   FMJ2 (carry forward)invoices
*&                                   posted before upgrade.
*&                                   (SAP Note 852241)
*&----------------------------------------------------------------------
REPORT zrffm_init_fmvor MESSAGE-ID fi.


INCLUDE:
  lfmauequ.

TYPE-POOLS:
  slis, fmfi.

CONSTANTS:
  g_con_packsize LIKE sy-tabix VALUE '10000'.

DATA:
  g_cursor TYPE cursor,
  g_fmvor  TYPE fmifiit-fmvor,
  BEGIN OF g_f_fmifiitkey,
    mandt TYPE mandt.
        INCLUDE STRUCTURE:
          fmikeyfi,
          fmikey.
DATA:
  END OF g_f_fmifiitkey,
  BEGIN OF g_f_count,
    mandt TYPE mandt,
    count TYPE i,
  END OF g_f_count,
  g_t_count           LIKE STANDARD TABLE OF g_f_count,
  g_t_fmifiitkey      LIKE STANDARD TABLE OF g_f_fmifiitkey.


************************************************************************
************************************************************************
*----- selection screen
SELECT-OPTIONS:
  s_mandt FOR g_f_fmifiitkey-mandt DEFAULT sy-mandt.

SELECTION-SCREEN BEGIN OF BLOCK ctrl
                 WITH FRAME TITLE bltxtprc.                 "#EC NEEDED
PARAMETERS:
  p_test  TYPE testlauf DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK ctrl.

*&---------------------------------------------------------------------*
*&   Event initialization
*&---------------------------------------------------------------------*
INITIALIZATION.

  CALL FUNCTION 'FUNCTION_EXISTS'
    EXPORTING
      funcname           = 'FM_FYC_SELSCR_FRAME_TITLES_GET'
    EXCEPTIONS
      function_not_exist = 1
      OTHERS             = 2.

  IF sy-subrc IS INITIAL.

*----- fetch reuse frame titles
    CALL FUNCTION 'FM_FYC_SELSCR_FRAME_TITLES_GET'
      IMPORTING
        e_text_prctrl = bltxtprc.

  ENDIF.

*&---------------------------------------------------------------------*
*&   Event start-of-selection
*&---------------------------------------------------------------------*
START-OF-SELECTION.

*----- initialize for collect
  g_f_count-count = 1.

  OPEN CURSOR WITH HOLD g_cursor FOR
       SELECT mandt
              fmbelnr fikrs fmbuzei
              btart rldnr gjahr stunr
         FROM fmifiit CLIENT SPECIFIED
        WHERE mandt IN s_mandt
          AND fmvor IS NULL.

  DO.

*----- read relevant data by package
    FETCH NEXT CURSOR g_cursor
      INTO TABLE g_t_fmifiitkey
      PACKAGE SIZE g_con_packsize.

*----- work finished
    IF sy-subrc <> 0.
      CLOSE CURSOR g_cursor.
      EXIT.
    ENDIF.

*----- initialize fmvor
    LOOP AT g_t_fmifiitkey INTO g_f_fmifiitkey.
      g_f_count-mandt = g_f_fmifiitkey-mandt.
      COLLECT g_f_count INTO g_t_count.

*----- do not update in test mode
      CHECK p_test IS INITIAL.
      UPDATE fmifiit CLIENT SPECIFIED
         SET fmvor   = g_fmvor
       WHERE mandt   = g_f_fmifiitkey-mandt
         AND fmbelnr = g_f_fmifiitkey-fmbelnr
         AND fikrs   = g_f_fmifiitkey-fikrs
         AND fmbuzei = g_f_fmifiitkey-fmbuzei
         AND btart   = g_f_fmifiitkey-btart
         AND rldnr   = g_f_fmifiitkey-rldnr
         AND gjahr   = g_f_fmifiitkey-gjahr
         AND stunr   = g_f_fmifiitkey-stunr.
    ENDLOOP.

*----- do not update in test mode
    CHECK p_test IS INITIAL.
    CALL FUNCTION 'DB_COMMIT'.
  ENDDO.

*&---------------------------------------------------------------------*
*&   Event END-OF-SELECTION
*&---------------------------------------------------------------------*
END-OF-SELECTION.

  WRITE:
    /1 '# UPDATES BY CLIENT'.

  LOOP AT g_t_count INTO g_f_count.
    WRITE:
    /1 g_f_count-mandt, g_f_count-count.
  ENDLOOP.

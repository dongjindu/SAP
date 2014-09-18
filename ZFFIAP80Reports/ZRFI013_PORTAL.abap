************************************************************************
**                                                                    **
**            C O M P A N Y   C O N F I D E N T I A L                 **
**                                                                    **
**      This program is the property of  HMMA LLC                     **
**      Care should be taken to prevent its unauthorized use.         **
**                                                                    **
************************************************************************
REPORT ZRFI013_PORTAL LINE-SIZE 132 LINE-COUNT 65
                             NO STANDARD PAGE HEADING message-id db .

tables: ekko, ekbe, FDKUSER.
*-------------------------------------------------------------*
* Selection Screen
*--------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_bukrs LIKE ekko-bukrs  MEMORY ID buk default 'H201'.
*SELECT-OPTIONS s_lifnr FOR ekko-lifnr MEMORY ID lif obligatory
*                       no intervals no-extension.
parameters s_lifnr(10) obligatory.
*PARAMETERS: p_curr  no-display  DEFAULT 'X'.
PARAMETERS: p_past  as checkbox.
PARAMETERS: p_reval as checkbox.
*PARAMETERS: p_due   no-display default 'X'.
PARAMETERS: p_duedt like BSAK-AUGDT.
PARAMETERS: p_grno AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECT-OPTIONS s_matnr FOR ekbe-matnr MEMORY ID mat.
SELECT-OPTIONS s_refdt FOR sy-datum.
SELECT-OPTIONS s_budat FOR ekbe-budat.
SELECT-OPTIONS s_ebeln FOR ekbe-ebeln MEMORY ID bes.
SELECT-OPTIONS s_ebelp FOR ekbe-ebelp no-display.

SELECT-OPTIONS s_gjahr FOR ekbe-gjahr MEMORY ID mja.
*  default sy-datum+0(4).
SELECT-OPTIONS s_belnr FOR ekbe-belnr MEMORY ID mbn.

SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-005.
*parameter p_vari    like disvariant-variant.
SeLECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN POSITION 1.
SELECTION-SCREEN COMMENT (79) TEXT-006 VISIBLE LENGTH 160.
SELECTION-SCREEN end of line.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (79) TEXT-007 VISIBLE LENGTH 160.
SELECTION-SCREEN end of line.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (79) TEXT-008 VISIBLE LENGTH 160.
SELECTION-SCREEN end of line.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (79) TEXT-009 VISIBLE LENGTH 160.
SELECTION-SCREEN end of line.

SELECTION-SCREEN END OF BLOCK b3.

data: gv_date type sy-datum.

*------------------------------------------------------------*
*INITIALIZATION
*-------------------------------------------------------------*
INITIALIZATION.

  select single * from FDKUSER  where  XUBNAME eq  sy-uname.
  if sy-subrc eq 0.
    loop at screen.
      if screen-name = 'S_LIFNR'.
        screen-input = 0.
        modify screen.
        exit.
      endif.
    endloop.
    s_lifnr = fdkuser-account_from.
  else.
    message i000 with 'Caution:HMMA internal use'.
  endif.

  CALL FUNCTION 'MONTH_PLUS_DETERMINE'
       EXPORTING
            MONTHS  = -1
            OLDDATE = sy-datum
       IMPORTING
            NEWDATE = gv_date.
  concatenate gv_date+0(6) '01' into gv_date.
  s_budat-low =  sy-datum.
  s_budat-high = sy-datum.
  append s_budat.

*------------------------------------------------------------*
*  At Selection-Screen
*-------------------------------------------------------------*

AT SELECTION-SCREEN OUTPUT.
  select single * from FDKUSER  where  XUBNAME eq  sy-uname.
  if sy-subrc eq 0.
    loop at screen.
      if screen-name = 'S_LIFNR'.
        screen-input = 0.
        modify screen.
        exit.
      endif.
    endloop.
    s_lifnr = fdkuser-account_from.
  else.
    message i000 with 'Caution:HMMA internal use'.
  endif.
*
*AT SELECTION-SCREEN.
*
*  if s_budat-low < l_date.
*    SET CURSOR FIELD s_budat-low.
*    message id 'ZMMM' type 'E' number '009' with text-004.
*    exit.
*  endif.
*at selection-screen on value-request for p_vari.
*  data: rs_variant like disvariant,
*        nof4 type c.
*
*  clear nof4.
*  loop at screen.
*    if screen-name = 'P_VARI'.
*      if screen-input = 0.
*        nof4 = 'X'.
*      endif.
*    endif.
*  endloop.
*  rs_variant-report   = sy-repid.
*  rs_variant-username = sy-uname.
*
*  call function 'REUSE_ALV_VARIANT_F4'
*       EXPORTING
*            is_variant = rs_variant
*            i_save     = 'A'
*       IMPORTING
*            es_variant = rs_variant
*       EXCEPTIONS
*            others     = 1.
*  if sy-subrc = 0 and nof4 eq space.
*    p_vari = rs_variant-variant.
*
*  endif.

*-------------------------------------------------------------*
* Start-of-selection
*--------------------------------------------------------------*
start-of-selection.

ranges r_lifnr for ekko-lifnr.

  r_lifnr = 'IEQ'.
  r_lifnr-low = s_lifnr.
  append r_lifnr.

  SUBMIT ZRFI013 and return
          WITH P_BUKRS = p_bukrs
          WITH P_CURR   = 'X'
          WITH P_PAST   = p_past

          WITH P_DUE    = 'X'
          WITH P_DUEDT  = p_duedt

          WITH P_REVAL  = p_reval

          WITH P_GRNO  = P_GRNO

          WITH P_SHORT  = 'X'
          WITH P_VARI   = '/PORTAL'   "layout
          WITH S_LIFNR in r_lifnr

          WITH S_MATNR in s_matnr
          WITH S_REFDT in s_refdt
          WITH S_EBELN in s_ebeln
          WITH S_EBELP in s_ebelp
          WITH S_GJAHR in s_gjahr
          WITH S_BELNR in s_belnr
          WITH S_BUDAT in s_budat.

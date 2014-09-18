************************************************************************
**                                                                    **
**            C O M P A N Y   C O N F I D E N T I A L                 **
**                                                                    **
**      This program is the property of  HMMA LLC                     **
**      Care should be taken to prevent its unauthorized use.         **
**                                                                    **
************************************************************************
REPORT zrfitemap_portal LINE-SIZE 132 LINE-COUNT 65
                             NO STANDARD PAGE HEADING MESSAGE-ID db .

TABLES: ekko, ekbe, fdkuser.
TABLES: lfa1, lfb1, bsik, bsid, admi_files.
TABLES: kna1.
TABLES: t001.
TABLES: t005.

DATA: wa_pos         LIKE rfposxext,
      it_pos         LIKE rfposxext OCCURS 1 WITH HEADER LINE.

*-------------------------------------------------------------*
* Selection Screen
*--------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-00x.
PARAMETERS p_bukrs LIKE ekko-bukrs  MEMORY ID buk DEFAULT 'H201'.
*PARAMETERS p_lifnr LIKE ekko-lifnr MEMORY ID lif OBLIGATORY.
PARAMETERS p_lifnr(10) OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

* outer frame for item selection:
SELECTION-SCREEN BEGIN OF BLOCK items WITH FRAME TITLE text-007.
* inner frame 1:
SELECTION-SCREEN BEGIN OF BLOCK status WITH FRAME TITLE text-002.
*   open items:
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS x_opsel LIKE itemset-xopsel RADIOBUTTON GROUP rad1.
SELECTION-SCREEN COMMENT 3(20) text-003 FOR FIELD x_opsel.
SELECTION-SCREEN END OF LINE.
PARAMETERS pa_stida LIKE rfpdo-allgstid DEFAULT sy-datlo.
SELECTION-SCREEN SKIP.
*   cleared items:
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS x_clsel LIKE itemset-xclsel RADIOBUTTON GROUP rad1.
SELECTION-SCREEN COMMENT 3(25) text-004 FOR FIELD x_clsel.
SELECTION-SCREEN END OF LINE.
SELECT-OPTIONS so_augdt FOR bsik-augdt NO DATABASE SELECTION.
PARAMETERS pa_stid2 LIKE rfpdo-allgstid.
SELECTION-SCREEN SKIP.
*   all items:
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS x_aisel LIKE itemset-xaisel RADIOBUTTON GROUP rad1.
SELECTION-SCREEN COMMENT 3(20) text-005 FOR FIELD x_aisel.
SELECTION-SCREEN END OF LINE.
SELECT-OPTIONS so_budat FOR bsik-budat NO DATABASE SELECTION.
SELECTION-SCREEN END OF BLOCK status.
* inner frame 2:
SELECTION-SCREEN BEGIN OF BLOCK type WITH FRAME TITLE text-006.
PARAMETERS: x_norm LIKE itemset-xnorm AS CHECKBOX DEFAULT 'X',
            x_shbv LIKE itemset-xshbv AS CHECKBOX,
*            x_merk LIKE itemset-xmerk AS CHECKBOX,
*            x_park LIKE itemset-xpark AS CHECKBOX,
            x_apar LIKE itemset-xapit AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK type.
SELECTION-SCREEN END OF BLOCK items.
* end of outer frame.

SELECTION-SCREEN BEGIN OF BLOCK list WITH FRAME TITLE text-001.
PARAMETERS: pa_vari TYPE slis_vari NO-DISPLAY,
            pa_nmax LIKE itemset-nmax.
SELECTION-SCREEN END OF BLOCK list.

DATA: gv_date TYPE sy-datum.

*------------------------------------------------------------*
*INITIALIZATION
*-------------------------------------------------------------*
INITIALIZATION.
  SELECT SINGLE * FROM fdkuser  WHERE  xubname EQ  sy-uname.
  IF sy-subrc EQ 0.
    p_lifnr = fdkuser-account_from.
    LOOP AT SCREEN.
      IF screen-name = 'P_LIFNR'.
        screen-input = 0.
        MODIFY SCREEN.
        EXIT.
      ENDIF.
    ENDLOOP.
  ELSE.
    MESSAGE i000 WITH 'Caution:HMMA internal use'.
  ENDIF.

  CALL FUNCTION 'MONTH_PLUS_DETERMINE'
       EXPORTING
            months  = -1
            olddate = sy-datum
       IMPORTING
            newdate = gv_date.
  CONCATENATE gv_date+0(6) '01' INTO gv_date.
  so_budat-low =  sy-datum.
  so_budat-high = sy-datum.
  APPEND so_budat.

AT SELECTION-SCREEN OUTPUT.
  SELECT SINGLE * FROM fdkuser  WHERE  xubname EQ  sy-uname.
  IF sy-subrc EQ 0.
    p_lifnr = fdkuser-account_from.
    LOOP AT SCREEN.
      IF screen-name = 'P_LIFNR'.
        screen-input = 0.
        MODIFY SCREEN.
        EXIT.
      ENDIF.
    ENDLOOP.
  ELSE.
    MESSAGE i000 WITH 'Caution:HMMA internal use'.
  ENDIF.

*-------------------------------------------------------------*
* Start-of-selection
*--------------------------------------------------------------*
START-OF-SELECTION.

  RANGES r_lifnr FOR ekko-lifnr.

  CLEAR : r_lifnr, r_lifnr[].
  r_lifnr = 'IEQ'.
  r_lifnr-low = p_lifnr.
  r_lifnr-high = ''.
  APPEND r_lifnr.

  pa_vari = '/PORTAL'.

  SUBMIT rfitemap
          WITH p_bukrs   = p_bukrs
          WITH kd_lifnr  IN  r_lifnr

          WITH x_opsel   = x_opsel
          WITH pa_stida  = pa_stida

          WITH x_clsel  = x_clsel
          WITH so_augdt  IN so_augdt
          WITH pa_stid2  = pa_stid2

          WITH x_aisel  = x_aisel
          WITH so_budat IN so_budat

          WITH x_norm = x_norm
          WITH x_shbv = x_shbv
          WITH x_apar = x_apar

          WITH pa_vari = pa_vari
          WITH pa_nmax = pa_nmax.

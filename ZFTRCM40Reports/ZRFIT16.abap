*&--------------------------------------------------------------------
*& Author                 : JIPARK
*& Creation Date          : 26/01/2004
*& Specification By       : JIPARK
*& Pattern                : Report 1-16
*& Development Request No : UD1K906366
*& Addl documentation     :
*& Description  : transfer down payment
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*&--------------------------------------------------------------------

REPORT  zrfit16 NO STANDARD PAGE HEADING MESSAGE-ID zmfi
                LINE-SIZE 135.

************************************************************************
*     DATA DECLARATION
************************************************************************
TABLES: ztfi_fmal.

DATA : it_fmal LIKE ztfi_fmal OCCURS 0 WITH HEADER LINE.
DATA : BEGIN OF it_trns OCCURS 0,
       frdat      LIKE sy-datum,
       framt      LIKE ztfi_fmal-wrshb,
       frdoc      LIKE ztfi_fmal-belnr,
       todat      LIKE sy-datum,
       toamt(15) TYPE p DECIMALS 2, "   LIKE ztfi_fmal-wrshb,
       todoc      LIKE ztfi_fmal-belnr,
       fipos      LIKE ztfi_fmal-fipos,
       fincode    LIKE ztfi_fmal-fincode,
       fistl      LIKE ztfi_fmal-fistl,
       assign_amt LIKE ztfi_fmal-wrshb,
       residu_amt LIKE ztfi_fmal-wrshb,
       END OF it_trns.
DATA : wa_fmal LIKE it_fmal.
DATA : BEGIN OF wa_sum,
       toamt      LIKE ztfi_fmal-wrshb,
       assign_amt LIKE ztfi_fmal-wrshb,
       residu_amt LIKE ztfi_fmal-wrshb,
       END OF wa_sum.
DATA : wa_line(140) TYPE c.

DATA : g_len TYPE i VALUE 135,
       g_tab_lin TYPE sy-tabix,
       l_new.
DATA : g_idx LIKE sy-tabix,
       g_cnt LIKE sy-tabix,
       g_numc(10) VALUE '0123456789'.

*DATA : l_aamt(15),
*       l_ramt(15),        "LIKE ztfi_fmal-wrshb,
*       l_assign_sum(15) TYPE p, " LIKE ztfi_fmal-wrshb,
*       l_residu_sum(20). " LIKE ztfi_fmal-wrshb.

************************************************************************
*     SELECTION-SCREEN
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
PARAMETERS : p_frdoc LIKE ztfi_fmal-belnr OBLIGATORY,
             p_frgjr LIKE ztfi_fmal-gjahr OBLIGATORY.
SELECTION-SCREEN END OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-002.
PARAMETERS : p_todoc LIKE ztfi_fmal-belnr OBLIGATORY,
             p_togjr LIKE ztfi_fmal-gjahr OBLIGATORY.
SELECTION-SCREEN END OF BLOCK bl2.

***********************************************************************
* TOP-OF-PAGE
***********************************************************************
TOP-OF-PAGE.
  PERFORM write_title USING :
          'S'  '10'   'D/P Date',
          ' '  '15'   'D/P Amount',
          ' '  '10'   'D/P Doc.',
          ' '  '10'   'Fund',
          ' '  '06'   'FCtr',
          ' '  '07'   'Cmltm',
          ' '  '10'   'Pay Date',
          ' '  '15'   'Pay Amount',
          ' '  '15'   'Assigned',
          ' '  '15'   'Residual',
          'E'  '10'   'Pay Doc.'.

***********************************************************************
* TOP-OF-PAGE DURING LINE-SELECTION
***********************************************************************
TOP-OF-PAGE DURING LINE-SELECTION.
  PERFORM write_title USING :
          'S'  '10'   'D/P Date',
          ' '  '15'   'D/P Amount',
          ' '  '10'   'D/P Doc.',
          ' '  '10'   'Fund',
          ' '  '06'   'FCtr',
          ' '  '07'   'Cmltm',
          ' '  '10'   'Pay Date',
          ' '  '15'   'Pay Amount',
          ' '  '15'   'Assigned',
          ' '  '15'   'Residual',
          'E'  '10'   'Pay Doc.'.

************************************************************************
*     START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
  CLEAR: it_fmal[], it_fmal, wa_fmal.

  SELECT * FROM ztfi_fmal
           INTO CORRESPONDING FIELDS OF TABLE it_fmal
           WHERE ( gjahr EQ p_frgjr AND belnr EQ p_frdoc )
           OR    ( gjahr EQ p_togjr AND belnr EQ p_todoc ).

*-- D/P Document
  READ TABLE it_fmal WITH KEY gjahr = p_frgjr
                              belnr = p_frdoc.
  MOVE-CORRESPONDING it_fmal TO wa_fmal.

*-- Pay Document
  LOOP AT it_fmal WHERE gjahr EQ p_togjr AND belnr EQ p_todoc.
    MOVE : wa_fmal-datum   TO  it_trns-frdat,
           wa_fmal-wrshb   TO  it_trns-framt,
           wa_fmal-belnr   TO  it_trns-frdoc,
           it_fmal-datum   TO  it_trns-todat,
           it_fmal-wrshb   TO  it_trns-toamt,
           it_fmal-belnr   TO  it_trns-todoc,
           it_fmal-fipos   TO  it_trns-fipos,
           it_fmal-fincode TO  it_trns-fincode,
           it_fmal-fistl   TO  it_trns-fistl.
    APPEND it_trns.
  ENDLOOP.

  IF it_trns[] IS INITIAL.
    MESSAGE s001.
    EXIT.
  ELSE.
*---Sum
    LOOP AT it_trns.
      AT FIRST.
        SUM.
        MOVE-CORRESPONDING it_trns TO wa_sum.
      ENDAT.
    ENDLOOP.
  ENDIF.
************************************************************************
*     END-OF-SELECTION
************************************************************************
END-OF-SELECTION.
  SET PF-STATUS 'MENU'.
  PERFORM write_list.

************************************************************************
*   AT LINE-SELECTION
************************************************************************
AT LINE-SELECTION.
  DATA : l_field(20).

  GET CURSOR FIELD l_field.

  IF l_field EQ 'IT_TRNS-TODOC'.
    SET PARAMETER ID:'BLN' FIELD p_todoc,
                     'BUK' FIELD 'H201',
                     'GJR' FIELD p_togjr.
    CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
  ELSEIF l_field EQ 'IT_TRNS-FRDOC'.
    SET PARAMETER ID:'BLN' FIELD p_frdoc,
                     'BUK' FIELD 'H201',
                     'GJR' FIELD p_frgjr.
    CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
  ENDIF.

************************************************************************
*   AT USER-COMMAND
************************************************************************
AT USER-COMMAND.
  CASE sy-ucomm.
    WHEN 'CALC'.
      CLEAR : g_tab_lin, g_cnt, g_idx.
      DESCRIBE TABLE it_trns LINES g_tab_lin.
      g_cnt = 1.  "list line
      DO.
        CLEAR wa_line.
        READ LINE g_cnt line value INTO wa_line.
        g_cnt = g_cnt + 1.
        IF wa_line+124(1) CO g_numc.
          g_idx = g_idx + 1.  "internal table line

          DO.
            REPLACE ',' LENGTH 1 WITH '.' INTO wa_line+92(15).
            IF sy-subrc <> 0.
              EXIT.
            ENDIF.
          ENDDO.

          READ TABLE it_trns INDEX g_idx.

          MOVE wa_line+92(15) TO it_trns-assign_amt.
          it_trns-residu_amt = it_trns-toamt - it_trns-assign_amt.

          MODIFY it_trns INDEX g_idx.  CLEAR it_trns.

          IF g_tab_lin EQ g_idx. EXIT. ENDIF.
        ENDIF.
      ENDDO.

      sy-lsind = sy-lsind - 1.
*     sy-lsind = 1.   "keep screen level.
      PERFORM write_list.
    WHEN 'SAVE'.
      IF wa_fmal-wrshb <> wa_sum-assign_amt.
        MESSAGE e000 WITH 'Assigned error!'.
      ELSE.
        DELETE FROM ztfi_fmal WHERE gjahr EQ wa_fmal-gjahr
                              AND   belnr EQ wa_fmal-belnr
                              AND   fmdummy EQ 'X'.
        IF sy-subrc <> 0.
          MESSAGE e000 WITH 'Fail to saved'.
        ENDIF.
        LOOP AT it_trns.
          SELECT SINGLE * FROM ztfi_fmal
                   WHERE gjahr   EQ p_togjr
                   AND   belnr   EQ it_trns-todoc
                   AND   fipos   EQ it_trns-fipos
                   AND   fistl   EQ it_trns-fistl
                   AND   fincode EQ it_trns-fincode
                   AND   wrshb   EQ it_trns-toamt.
          IF sy-subrc EQ 0.
            ztfi_fmal-wrshb = it_trns-assign_amt.
            MODIFY ztfi_fmal.
            IF sy-subrc <> 0.
              EXIT.
            ENDIF.
          ENDIF.
        ENDLOOP.
        IF sy-subrc <> 0.
          ROLLBACK WORK.
          MESSAGE e000 WITH 'Failed to save'.
        ELSE.
          MESSAGE s007.
        ENDIF.
      ENDIF.
  ENDCASE.

*&---------------------------------------------------------------------
*&      Form  WRITE_TITLE
*&---------------------------------------------------------------------
FORM write_title USING p_new p_len p_txt.
  CASE p_new.
    WHEN 'S'.
      WRITE: AT /(g_len) sy-uline,
             AT / sy-vline NO-GAP.
    WHEN 'M'.
      WRITE: AT / sy-vline NO-GAP.
  ENDCASE.

  WRITE: AT (p_len) p_txt NO-GAP COLOR COL_HEADING CENTERED,
         AT (1) sy-vline NO-GAP.

  IF p_new = 'E'.
    WRITE: AT /(g_len) sy-uline.
  ENDIF.
ENDFORM.                                        "WRITE_TITLE
*&---------------------------------------------------------------------
*&      Form  WRITE_ITEM
*&---------------------------------------------------------------------
FORM write_item USING p_new p_len  p_item.
  IF p_new = 'S'.
    WRITE: / sy-vline NO-GAP.
  ENDIF.
*-- assigned amount is input field.
  IF p_new EQ 'C'.
    WRITE: AT (p_len) p_item NO-GAP INPUT, "COLOR COL_NORMAL,
             AT (1) sy-vline NO-GAP.
  ELSE.
    WRITE: AT (p_len) p_item NO-GAP, "COLOR COL_NORMAL,
           AT (1) sy-vline NO-GAP.
  ENDIF.
ENDFORM.                               " WRITE_ITEM
*&---------------------------------------------------------------------*
*&      Form  WRITE_ITEM_SUM
*&---------------------------------------------------------------------*
FORM write_item_sum USING p_new p_len  p_item.
  IF p_new = 'S'.
    WRITE: / sy-vline NO-GAP.
  ENDIF.
  WRITE: AT (p_len) p_item NO-GAP COLOR COL_TOTAL,
         AT (1) sy-vline NO-GAP.
ENDFORM.                               " WRITE_ITEM_SUM
*&---------------------------------------------------------------------*
*&      Form  write_list
*&---------------------------------------------------------------------*
FORM write_list. " USING p_assign_amt p_residu_amt.
*-calculate sub total.
  LOOP AT it_trns.
    AT FIRST.
      SUM.
      MOVE-CORRESPONDING it_trns TO wa_sum.
    ENDAT.
  ENDLOOP.

  LOOP AT it_trns.
    AT NEW frdoc.
      l_new = 'X'.
    ENDAT.
    IF l_new EQ 'X'.
      PERFORM write_item USING :
            'S'  '10'   it_trns-frdat,
            ' '  '15'   it_trns-framt,
            ' '  '10'   it_trns-frdoc.
      CLEAR l_new.
    ELSE.
      PERFORM write_item USING :
            'S'  '10'   '',
            ' '  '15'   '',
            ' '  '10'   ''.
    ENDIF.

    PERFORM write_item USING :
          ' '  '10'   it_trns-fincode,
          ' '  '06'   it_trns-fistl,
          ' '  '07'   it_trns-fipos,
          ' '  '10'   it_trns-todat,
          ' '  '15'   it_trns-toamt,
          'C'  '15'   it_trns-assign_amt,
          ' '  '15'   it_trns-residu_amt,
          'E'  '10'   it_trns-todoc.
  ENDLOOP.
*--write sub-total
  ULINE.
  PERFORM write_item_sum USING :
        'S'  '10'   '',
        ' '  '15'   wa_fmal-wrshb,
        ' '  '10'   '',
        ' '  '10'   '',
        ' '  '06'   '',
        ' '  '07'   '',
        ' '  '10'   '',
        ' '  '15'   wa_sum-toamt,
        ' '  '15'   wa_sum-assign_amt,
        ' '  '15'   wa_sum-residu_amt,
        'E'  '10'   ''.
  ULINE.
ENDFORM.                    " write_list

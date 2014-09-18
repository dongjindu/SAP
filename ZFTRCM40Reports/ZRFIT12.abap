*&--------------------------------------------------------------------
*& Author                 : JIPARK
*& Creation Date          : 01/09/2004
*& Specification By       : JIPARK
*& Pattern                : Report 1-12
*& Development Request No : UD1K905622
*& Addl documentation     :
*& Description  : CM actuals update clearing date (Batch)
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*&--------------------------------------------------------------------
REPORT zrfif12 MESSAGE-ID zmfi
               LINE-SIZE 200
               LINE-COUNT 65
               NO STANDARD PAGE HEADING.

************************************************************************
*     DATA DECLARATION
************************************************************************
TABLES: ztfi_cmal, ztfi_fmal, bsas, skb1.
DATA : it_cmal LIKE ztfi_cmal OCCURS 0 WITH HEADER LINE,
       it_fmal LIKE ztfi_fmal OCCURS 0 WITH HEADER LINE.

RANGES : r_bank FOR skb1-saknr.
************************************************************************
*     SELECTION-SCREEN
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS : p_bukrs LIKE glt0-bukrs OBLIGATORY MEMORY ID buk
                                     DEFAULT 'H201',
             p_gjahr LIKE bkpf-gjahr OBLIGATORY DEFAULT sy-datum(4).
SELECT-OPTIONS : s_datum FOR sy-datum,
                 s_belnr FOR ztfi_cmal-belnr.
SELECTION-SCREEN END OF BLOCK b1.

***********************************************************************
* START-OF-SELECTION
***********************************************************************
START-OF-SELECTION.
  CLEAR: it_cmal[], it_cmal, it_fmal[], it_fmal, r_bank[], r_bank.
*..get non-clearing CM Actual
  SELECT saknr INTO r_bank-low
        FROM skb1   "G/L account master (company code)
        WHERE bukrs = p_bukrs
          AND xgkon = 'X'  "Cash receipt(disbursement) account
          AND fdlev BETWEEN 'B1' and 'B9'.  "clearing bank acct.
    r_bank-sign = 'I'.  r_bank-option = 'EQ'.
    APPEND r_bank.
  ENDSELECT.

  SELECT * FROM ztfi_cmal
           INTO CORRESPONDING FIELDS OF TABLE it_cmal
           WHERE bukrs EQ p_bukrs
           AND   gjahr EQ p_gjahr
           AND   belnr IN s_belnr
           AND   budat IN s_datum
           AND   saknr IN r_bank
           AND   augdt EQ '00000000'.
*..get clearing date
  LOOP AT it_cmal.
    SELECT SINGLE augdt INTO it_cmal-augdt
                  FROM bsas
                  WHERE bukrs EQ it_cmal-bukrs
                  AND   hkont EQ it_cmal-saknr
                  AND   gjahr EQ it_cmal-gjahr
                  AND   belnr EQ it_cmal-belnr
                  AND   buzei EQ it_cmal-buzei.
    MODIFY it_cmal.  CLEAR it_cmal.
  ENDLOOP.

  DELETE it_cmal WHERE augdt IS initial.
*..get non-clearing FM Actual
  IF NOT it_cmal[] IS INITIAL.
    LOOP AT it_cmal.
      SELECT * FROM ztfi_fmal
               WHERE bukrs EQ it_cmal-bukrs
               AND   gjahr EQ it_cmal-gjahr
               AND   belnr EQ it_cmal-belnr
*               AND   grupp EQ it_cmal-grupp
*               AND   ebene EQ it_cmal-ebene
               AND   dispw EQ it_cmal-dispw
               AND   datum EQ it_cmal-budat
               AND   augdt EQ '00000000'
               AND   gsber EQ it_cmal-gsber.
        MOVE-CORRESPONDING ztfi_fmal TO it_fmal.
        it_fmal-augdt = it_cmal-augdt.
        APPEND it_fmal. CLEAR it_fmal.
      ENDSELECT.
    ENDLOOP.
*---CM actual upate
    MODIFY ztfi_cmal FROM TABLE it_cmal.
    IF sy-subrc EQ 0.
*-----FM actual update
      IF NOT it_fmal[] IS INITIAL.
*       MODIFY ztfi_fmal FROM TABLE it_fmal.
        LOOP AT it_fmal.
          MOVE-CORRESPONDING it_fmal TO ztfi_fmal.
          MODIFY ztfi_fmal.
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.
        ENDLOOP.
        IF sy-subrc <> 0.
          MESSAGE s000 WITH 'FM updating Failed.'.
          STOP.
        ENDIF.
      ENDIF.

      MESSAGE s000 WITH 'Clearing date All Updated.'.
    ELSE.
      ROLLBACK WORK.
      MESSAGE s000 WITH 'CM updating Failed.'.
    ENDIF.
  ELSE.
    MESSAGE s001.
  ENDIF.

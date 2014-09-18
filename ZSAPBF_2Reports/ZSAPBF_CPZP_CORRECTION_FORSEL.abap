*&---------------------------------------------------------------------*
*&  Include           ZSAPBF_CPZP_CORRECTION_FORSEL
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK general WITH FRAME TITLE text-041.

PARAMETERS: p_year TYPE gjahr OBLIGATORY DEFAULT sy-datlo(4)," NO-DISPLAY DEFAULT sy-datlo(4),
            p_month TYPE monat OBLIGATORY DEFAULT sy-datlo+4(2).

PARAMETERS: p_aufnr TYPE aufnr OBLIGATORY MATCHCODE OBJECT orde MODIF ID 10.
SELECTION-SCREEN END OF BLOCK general.




SELECTION-SCREEN BEGIN OF BLOCK test WITH FRAME TITLE text-037.
PARAMETERS: p_test AS CHECKBOX TYPE flag DEFAULT 'X'.
*PARAMETERS: p_error AS CHECKBOX TYPE zsapbf_cpzp_error DEFAULT ' '.
SELECTION-SCREEN END OF BLOCK test.

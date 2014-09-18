*&---------------------------------------------------------------------*
*& Include MZAHR0003TOP                                                *
*&                                                                     *
*&---------------------------------------------------------------------*
PROGRAM SAPMZAHR0003 MESSAGE-ID ZMHR.
*
TABLES: ZTHR_AHC01,                   " Head Count Plan
        HRP1000,                      " Infotype 1000 DB Table
        PA0001,                       " HR Master Record: Infotype 0001
        T500P,                        " Personnel Areas
        T527X.                        " Organizational Units

CONTROLS: TC9000 TYPE TABLEVIEW USING SCREEN 9000.

*... internal tables
DATA: BEGIN OF IT_S9000 OCCURS 0.               " screen 9000 display
      INCLUDE STRUCTURE ZTHR_AHC01.
DATA: CHKBX,
      NAME1    LIKE T500P-NAME1,
      MONTH(10),
      END OF IT_S9000.

DATA: IT_AHC01 LIKE ZTHR_AHC01 OCCURS 0 WITH HEADER LINE.

*... variants
DATA: W_ZVERS  LIKE ZTHR_HCP01-ZVERS.

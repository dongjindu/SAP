*&---------------------------------------------------------------------*
*& Include MZAHR0002TOP                                                *
*&                                                                     *
*&---------------------------------------------------------------------*
PROGRAM SAPMZAHR0002 MESSAGE-ID ZMHR.
*
TABLES: ZTHR_HCP01,                   " Head Count Plan
        HRP1000,                      " Infotype 1000 DB Table
        PA0001,                       " HR Master Record: Infotype 0001
        T500P,                        " Personnel Areas
        T527X.                        " Organizational Units

CONTROLS: TC9000 TYPE TABLEVIEW USING SCREEN 9000.

*... internal tables
DATA: BEGIN OF IT_S9000 OCCURS 0.               " screen 9000 display
      INCLUDE STRUCTURE ZTHR_HCP01.
DATA: CHKBX,
      NAME1    LIKE T500P-NAME1,
      INDEX    LIKE SY-TABIX,
      END OF IT_S9000.

DATA: IT_HCP01 LIKE ZTHR_HCP01 OCCURS 0 WITH HEADER LINE,
      TP_HCP01 LIKE ZTHR_HCP01 OCCURS 0 WITH HEADER LINE.

DATA: IT_UNITS LIKE RHLDAPO OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF IT_VALUE OCCURS 0,               " possible entry
      PERSA    LIKE T500P-PERSA,
      NAME1    LIKE T500P-NAME1.
DATA: END OF IT_VALUE.

DATA: BEGIN OF IT_JOBVL OCCURS 0,
      OBJID    LIKE HRP1000-OBJID,
      SHORT    LIKE HRP1000-SHORT,
      STEXT    LIKE HRP1000-STEXT.
DATA: END OF IT_JOBVL.

DATA: IT_FIELD     LIKE  HELP_VALUE OCCURS 1 WITH HEADER LINE,
      DYNPFIELDS   LIKE STANDARD TABLE OF DYNPREAD WITH HEADER LINE.

*... variants
DATA: W_ZVERS  LIKE ZTHR_HCP01-ZVERS.

DATA: W_FNAME  LIKE  HELP_INFO-FIELDNAME,
      W_TABIX  LIKE  SY-TABIX,
      W_FLDVL  LIKE  HELP_INFO-FLDVALUE.

DATA: W_INITF,
      W_ABSVA  TYPE I,
      W_INDEX  LIKE  SY-TABIX.

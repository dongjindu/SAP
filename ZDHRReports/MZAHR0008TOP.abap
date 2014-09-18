*&---------------------------------------------------------------------*
*& Include MZAHR0008TOP                                                *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM SAPMZAHR0008 MESSAGE-ID ZMHR.
*...
TABLES: ZTHR_PCP02,
        ZTHR_PCP04.

FIELD-SYMBOLS : <DA>   LIKE ZTHR_PCP04-DAY01,
                <SA>   LIKE ZTHR_PCP04-SAT01,
                <SU>   LIKE ZTHR_PCP04-SUN01,
                <HO>   LIKE ZTHR_PCP04-HOL01.

*... internal tables
DATA: BEGIN OF IT_PCP04 OCCURS 0.
      INCLUDE STRUCTURE ZTHR_PCP04.
DATA: TOTA1    LIKE ZTHR_PCP04-DAY01,
      TOTA2    LIKE ZTHR_PCP04-DAY01,
      TOTA3    LIKE ZTHR_PCP04-DAY01,
      TOTA4    LIKE ZTHR_PCP04-DAY01,
      END OF IT_PCP04.

DATA: BEGIN OF IT_YEARS OCCURS 1,
      ZYEAR    LIKE ZTHR_PCP03-ZYEAR.
DATA: END OF IT_YEARS.

DATA: BEGIN OF IT_VERSN OCCURS 1,
      ZVERS    LIKE ZTHR_PCP03-ZVERS.
DATA: END OF IT_VERSN.

DATA: IT_FIELD     LIKE HELP_VALUE OCCURS 1 WITH HEADER LINE,
      DYNPFIELDS   LIKE STANDARD TABLE OF DYNPREAD WITH HEADER LINE.

*... variants
DATA: W_ZYEAR  LIKE ZTHR_PCP04-ZYEAR,
      W_ZVERS  LIKE ZTHR_PCP04-ZVERS,
      W_FLAGS.

DATA: W_FNAME      LIKE  HELP_INFO-FIELDNAME,
      W_TABIX      LIKE  SY-TABIX,
      W_FLDVL      LIKE  HELP_INFO-FLDVALUE.

DATA: W_DAYFN(20),
      W_SATFN(20),
      W_SUNFN(20),
      W_HOLFN(20).

DATA: W_DAYTO         LIKE ZTHR_PCP04-DAY01.

*&---------------------------------------------------------------------*
*& Include MZAHR0013TOP                                                *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM  SAPMZAHR0013                  .

TABLES: PA0001,
        PA0008,
        HRP1000,
        T500P,
        CSKT,
        ZTHR_PCPXX,
        ZTHR_PCP00,
        ZTHR_PCP01,
        ZTHR_PCP02,
        ZTHR_PCP05.

*... internal tables
DATA: BEGIN OF IT_9000 OCCURS 0,
      ZOBJC    LIKE ZTHR_PCP00-ZOBJC,  " Job
      ZPERG    LIKE ZTHR_PCP00-ZPERG,  " Employee Group
      ZSENR    like zthr_pcp00-ZSENR,
      ZHEDC    LIKE ZTHR_PCP00-ZHEDC,  " Head Count
      ZSALY    LIKE ZTHR_PCPXX-ZSALY,
      BONUS    LIKE ZTHR_PCPXX-BONUS,
      RATE1(5) TYPE P DECIMALS 2,
      ACT01    LIKE ZTHR_PCP00-ACT01,
      RATE2(5) TYPE P DECIMALS 2,
      MOTHA    LIKE ZTHR_PCP00-ACT03,
      AMUNT    LIKE ZTHR_PCP00-ACT03,
      ZJOBK    LIKE ZTHR_PCP03-ZJOBK,
      zval1    type i,
     END OF IT_9000.
DATA : IT_PCP06 LIKE ZTHR_PCP06 OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF IT_YEARS OCCURS 1,
      ZYEAR    LIKE ZTHR_PCP03-ZYEAR.
DATA: END OF IT_YEARS.

DATA: BEGIN OF IT_VERSN OCCURS 1,
      ZVERS    LIKE ZTHR_PCP03-ZVERS.
DATA: END OF IT_VERSN.

DATA: BEGIN OF IT_PCP00 OCCURS 1.
        INCLUDE STRUCTURE ZTHR_PCP00.
DATA: END OF IT_PCP00.

DATA: BEGIN OF IT_PCPXX OCCURS 1.
        INCLUDE STRUCTURE ZTHR_PCPXX.
DATA: zhedc like zthr_pcp00-zhedc,
      END OF IT_PCPXX.



DATA: BEGIN OF IT_PERSA OCCURS 1,
      WERKS    LIKE T500P-PERSA,
      NAME1    LIKE T500P-NAME1.
DATA: END OF IT_PERSA.

DATA: IT_FIELD     LIKE HELP_VALUE OCCURS 1 WITH HEADER LINE,
      DYNPFIELDS   LIKE STANDARD TABLE OF DYNPREAD WITH HEADER LINE.

CONTROLS: TC_9000 TYPE TABLEVIEW USING SCREEN 9000.

*... variants
DATA: W_ZYEAR      LIKE ZTHR_AHC01-ZYEAR,  " parameter ; year
      W_ZVERS      LIKE ZTHR_AHC01-ZVERS,  " parameter ; version
      W_WERKS      LIKE PA0001-WERKS,
      W_NAME1      LIKE T500P-NAME1.



DATA: W_FNAME      LIKE  HELP_INFO-FIELDNAME,
      W_TABIX      LIKE  SY-TABIX,
      W_FLDVL      LIKE  HELP_INFO-FLDVALUE,
      W_SAVE   ,
      W_INPUT      .

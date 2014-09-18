*&---------------------------------------------------------------------*
*& Include MZAHR0006TOP                                                *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM  SAPMZAHR0006 MESSAGE-ID ZMHR.

*... tables
  TABLES: ZTHR_PCP01,
          ZTHR_PCP02,
          ZTHR_PCP03,
          ZTHR_HCP01,
          CSKT,
          T500P,
          T527X,
          HRP1000.

  CONTROLS: TC9000 TYPE TABLEVIEW USING SCREEN 9000.

  TYPE-POOLS VRM.

*... internal tables
  DATA: BEGIN OF IT_PCP03 OCCURS 0.
        INCLUDE STRUCTURE ZTHR_PCP03.
  DATA: KTEXT    LIKE CSKT-KTEXT,
        OBJID    LIKE HRP1000-OBJID,              " job code
        END OF IT_PCP03.

  DATA: BEGIN OF IT_YEARS OCCURS 1,
        ZYEAR    LIKE ZTHR_PCP03-ZYEAR.
  DATA: END OF IT_YEARS.

  DATA: BEGIN OF IT_VERSN OCCURS 1,
        ZVERS    LIKE ZTHR_PCP03-ZVERS.
  DATA: END OF IT_VERSN.

  DATA: BEGIN OF IT_PERSA OCCURS 1,
        WERKS    LIKE T500P-PERSA,
        NAME1    LIKE T500P-NAME1.
  DATA: END OF IT_PERSA.

  DATA: IT_FIELD     LIKE HELP_VALUE OCCURS 1 WITH HEADER LINE,
        DYNPFIELDS   LIKE STANDARD TABLE OF DYNPREAD WITH HEADER LINE.

  DATA: IT_UNITS     LIKE RHLDAPO OCCURS 0 WITH HEADER LINE,
        IT_PERSN     LIKE RHLDAPP OCCURS 0 WITH HEADER LINE,
        IT_ORGPN     LIKE RHLDAPOP OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF IT_STATS OCCURS 1,
        FCODE    LIKE RSMPE-FUNC,
        END OF IT_STATS.

  RANGES: R_ZJOBK    FOR  ZTHR_PCP03-ZJOBK.

*... variants
  DATA: W_PERNR      LIKE PA0001-PERNR,
        W_ORGEH      LIKE T527X-ORGEH,
        W_ORGTX      LIKE T527X-ORGTX,
        W_ZYEAR      LIKE ZTHR_PCP03-ZYEAR,
        W_ZVERS      LIKE ZTHR_PCP03-ZVERS,
        W_ZMONS      LIKE ZTHR_PCP03-ZMONS,
        W_ZCODE      LIKE ZTHR_PCP02-ZCODE,
        W_WERKS      LIKE PA0001-WERKS,
        W_NAME1      LIKE T500P-NAME1,
        W_MASTR.

  DATA: W_FNAME      LIKE  HELP_INFO-FIELDNAME,
        W_TABIX      LIKE  SY-TABIX,
        W_FLDVL      LIKE  HELP_INFO-FLDVALUE.

  DATA: W_FLAGS.

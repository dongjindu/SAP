*&---------------------------------------------------------------------*
*& Report  MP9871BI                                                    *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM MP9871BI.

TABLES: P9871.

*&---------------------------------------------------------------------*
*&      Form  BATCH_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VALUE      text
*      -->(BI_FCODE) text
*      -->VALUE      text
*      -->(BI_WPLOG) text
*----------------------------------------------------------------------*
FORM BATCH_INPUT USING VALUE(BI_FCODE)
                       VALUE(BI_WPLOG).

  FIELD-SYMBOLS: <BI_WPLOG> TYPE P9871.
  ASSIGN BI_WPLOG TO <BI_WPLOG> CASTING.
  P9871 = <BI_WPLOG>.
  PERFORM FILL_FIELD(RHALTD00) USING
  'P9871-IGID'
  P9871-IGID.
**PERFORM FILL_FIELD(RHALTD00) USING 'P9871-DUMMY' P9871-DUMMY.

  PERFORM FILL_OKCODE(RHALTD00) USING 'U'.

ENDFORM.                    "BATCH_INPUT

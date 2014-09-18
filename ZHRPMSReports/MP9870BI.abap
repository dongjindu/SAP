*&---------------------------------------------------------------------*
*& Report  MP9870BI                                                    *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM MP9870BI.

TABLES: P9870.

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

  FIELD-SYMBOLS: <BI_WPLOG> TYPE P9870.
  ASSIGN BI_WPLOG TO <BI_WPLOG> CASTING.
  P9870 = <BI_WPLOG>.
  PERFORM FILL_FIELD(RHALTD00) USING
  'P9870-TRACK'
  P9870-TRACK.
  PERFORM FILL_FIELD(RHALTD00) USING
  'P9870-GRADE'
  P9870-GRADE.
**PERFORM FILL_FIELD(RHALTD00) USING 'P9870-DUMMY' P9870-DUMMY.

  PERFORM FILL_OKCODE(RHALTD00) USING 'U'.

ENDFORM.                    "BATCH_INPUT

*&---------------------------------------------------------------------*
*& Report  MP9872BI                                                    *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM MP9872BI.

TABLES: P9872.

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

  FIELD-SYMBOLS: <BI_WPLOG> TYPE P9872.
  ASSIGN BI_WPLOG TO <BI_WPLOG> CASTING.
  P9872 = <BI_WPLOG>.
  PERFORM FILL_FIELD(RHALTD00) USING
  'P9872-ROLE_ID'
  P9872-ROLE_ID.
**PERFORM FILL_FIELD(RHALTD00) USING 'P9872-DUMMY' P9872-DUMMY.

  PERFORM FILL_OKCODE(RHALTD00) USING 'U'.

ENDFORM.                    "BATCH_INPUT

*&---------------------------------------------------------------------*
*& Report  MP9873BI                                                    *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM MP9873BI.

TABLES: P9873.

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

  FIELD-SYMBOLS: <BI_WPLOG> TYPE P9873.
  ASSIGN BI_WPLOG TO <BI_WPLOG> CASTING.
  P9873 = <BI_WPLOG>.
  PERFORM FILL_FIELD(RHALTD00) USING
  'P9873-MYEY'
  P9873-MYEY.
  PERFORM FILL_FIELD(RHALTD00) USING
  'P9873-ROLE_ID'
  P9873-ROLE_ID.
**PERFORM FILL_FIELD(RHALTD00) USING 'P9873-DUMMY' P9873-DUMMY.

  PERFORM FILL_OKCODE(RHALTD00) USING 'U'.

ENDFORM.                    "BATCH_INPUT

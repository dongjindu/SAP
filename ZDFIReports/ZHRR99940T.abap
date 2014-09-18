*&---------------------------------------------------------------------*
*& Subroutine Pool   ZHRR99940T
*&
*&---------------------------------------------------------------------*
*& called by SAPScript : ZF110_IN_AVIS
*&
*&---------------------------------------------------------------------*

PROGRAM  ZHRR99940T.


*&---------------------------------------------------------------------*
*&      Form  GET_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IN_PAR     text
*      -->OUT_PAR    text
*----------------------------------------------------------------------*
FORM get_email TABLES in_par  STRUCTURE itcsy
                      out_par STRUCTURE itcsy.

  DATA: adrnr(10) TYPE n,
        email     TYPE adr6-smtp_addr.

  READ TABLE in_par WITH KEY name = 'REGUH-ADRNR'.
  CHECK sy-subrc = 0.
  adrnr = in_par-value.

  SELECT smtp_addr INTO email
    FROM adr6
  UP TO 1 ROWS
    WHERE addrnumber = adrnr.
  ENDSELECT.

  CHECK sy-subrc = 0.

  READ TABLE out_par WITH KEY name = 'EMAIL'.
  CHECK sy-subrc = 0.

  out_par-value = email.

  MODIFY out_par INDEX sy-tabix.

ENDFORM.                    "GET_EMAIL

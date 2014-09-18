*----------------------------------------------------------------------*
***INCLUDE ZFIR0062O01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9100 OUTPUT.

  SET PF-STATUS '9000'.
  SET TITLEBAR '910'.

  IF NOT zfit0091-pernr IS INITIAL.
    SELECT SINGLE ename INTO pa0001-ename
                        FROM pa0001
                        WHERE pernr =  zfit0090-pernr
                        AND   endda >= sy-datum
                        AND   begda <= sy-datum.
  ENDIF.

ENDMODULE.                 " STATUS_9100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9200 OUTPUT.

  SET PF-STATUS '9000'.
  SET TITLEBAR '920'.

  IF NOT zfit0091-pernr IS INITIAL.
    SELECT SINGLE ename INTO pa0001-ename
                        FROM pa0001
                        WHERE pernr =  zfit0091-pernr
                        AND   endda >= sy-datum
                        AND   begda <= sy-datum.
  ENDIF.

ENDMODULE.                 " STATUS_9200  OUTPUT

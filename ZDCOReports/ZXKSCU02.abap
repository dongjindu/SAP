*----------------------------------------------------------------------*
*   INCLUDE ZXKSCU02                                                   *
*----------------------------------------------------------------------*
*"     VALUE(I_OHEAD) LIKE  T685-KSCHL
*"     VALUE(I_DATE) LIKE  SY-DATUM
*"     VALUE(I_COIOB) LIKE  COIOB STRUCTURE  COIOB
*"     VALUE(I_KOMK) LIKE  KOMK STRUCTURE  KOMK
*"     VALUE(I_KOMP) LIKE  KOMP STRUCTURE  KOMP
*"  EXPORTING
*"     VALUE(E_PCENT) LIKE  COSS-MEG001
*"     VALUE(E_AMONT) LIKE  KOMV-KBETR
*"     VALUE(E_CURCY) LIKE  KOMV-WAERS
*"     VALUE(E_PERQT) LIKE  KOMV-KPEIN
*"     VALUE(E_QUNIT) LIKE  KOMV-KMEIN
*"  EXCEPTIONS
*"      FEHLER

CLEAR: SY-SUBRC,
       E_PCENT,
       E_AMONT,
       E_CURCY,
       E_PERQT,
       E_QUNIT.

CHECK I_OHEAD = 'C000'.
IF I_COIOB-PRCTR = '0000000001'.
  E_PCENT = '10.000'.
ELSE.
  E_PCENT = '12.000'.
ENDIF.
CLEAR SY-SUBRC.

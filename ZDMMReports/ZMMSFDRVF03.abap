*&---------------------------------------------------------------------*
*&  Include           ZMMSFDRVF03
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Subroutines.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_PLANT_ADDRESS
*&---------------------------------------------------------------------*
FORM GET_PLANT_ADDRESS USING    P_WERKS LIKE T001W-WERKS
                       CHANGING P_ADRNR
                                P_SADR  LIKE SADR.
* parameter P_ADRNR without type since there are several address
* fields with different domains

  DATA: L_EKKO LIKE EKKO,
        L_ADDRESS LIKE ADDR1_VAL.

  CHECK NOT P_WERKS IS INITIAL.
  L_EKKO-RESWK = P_WERKS.
  L_EKKO-BSAKZ = 'T'.
  CALL FUNCTION 'MM_ADDRESS_GET'
       EXPORTING
            I_EKKO          = L_EKKO
       IMPORTING
            E_ADDRESS       = L_ADDRESS
            E_SADR          = P_SADR.
  P_ADRNR = L_ADDRESS-ADDRNUMBER.

ENDFORM.                    " GET_PLANT_ADDRESS

FUNCTION ZPP_GET_PROD_CHECKSUM.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_DATE) LIKE  SY-DATUM
*"  EXPORTING
*"     VALUE(O_QTY) TYPE  ZELOG_QTY
*"     VALUE(O_SUM) TYPE  ZELOG_SUM
*"----------------------------------------------------------------------

  DATA: L_QTY LIKE ZTPP_ACT_HMC-QTY_SIGNOFF,
        L_SUM TYPE ZTPP_ACT_HMC-QTY_SIGNOFF.

  SELECT COUNT( * ) SUM( QTY_SIGNOFF )
     INTO (L_QTY, L_SUM)
     FROM ZTPP_ACT_HMC
     WHERE PRDT_DATE = I_DATE.

  O_QTY = L_QTY.
  O_SUM = L_SUM.
  SHIFT O_QTY LEFT DELETING LEADING SPACE.
  SHIFT O_SUM LEFT DELETING LEADING SPACE.

ENDFUNCTION.

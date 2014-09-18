*&---------------------------------------------------------------------*
*&  Include           ZXFMDTU02
*&---------------------------------------------------------------------*
*"     REFERENCE(I_COBL) LIKE  COBL STRUCTURE  COBL
*"     REFERENCE(I_COBL_AD) LIKE  COBL_AD STRUCTURE  COBL_AD
*"  CHANGING
*"     REFERENCE(C_FMDERIVE) LIKE  FMDERIVE STRUCTURE  FMDERIVE
*"     VALUE(C_FLG_SKIP_BUFFER) TYPE  XFELD OPTIONAL
*{ 09/20/11 PaUL Insert Responsible Cost Center Logic..
*  IF I_COBL-AUFNR IS NOT INITIAL AND
*     I_COBL-KOSTL IS INITIAL.
*
*     SELECT SINGLE KOSTV
*       INTO C_FMDERIVE-COST_CENTER
*       FROM AUFK
*      WHERE AUFNR EQ I_COBL-AUFNR.
*
*  ENDIF.
*}

FUNCTION Z_FRF_WM_DATA.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_MATNR) TYPE  MATNR
*"     VALUE(I_WERKS) TYPE  WERKS_D
*"     VALUE(I_LGORT) TYPE  LGORT_D
*"     VALUE(I_LGTYP) TYPE  LGTYP
*"  EXPORTING
*"     VALUE(O_RESULT) TYPE  ZRESULT
*"     VALUE(O_MESS) TYPE  BAPI_MSG
*"  TABLES
*"      O_ITAB STRUCTURE  ZSMM_WM_DATA
*"----------------------------------------------------------------------

  IF I_MATNR IS INITIAL OR I_WERKS IS INITIAL OR
     I_LGORT IS INITIAL OR I_LGTYP IS INITIAL.
    O_RESULT = 'E'.
    O_MESS = 'Please provide all input data'.
  ELSE.
** Changed by Furong on 06/25/10
*    SELECT A~MATNR B~WERKS LGORT A~LGTYP A~LGPLA RDMNG VERME MEINS
    SELECT A~MATNR B~WERKS LGORT A~LGTYP A~LGPLA RDMNG GESME MEINS
** End of change
    INTO TABLE O_ITAB
    FROM MLGT AS A
    INNER JOIN LQUA AS B
    ON A~MATNR = B~MATNR
    AND A~LGTYP = B~LGTYP
    AND A~LGPLA = B~LGPLA
    WHERE A~MATNR = I_MATNR
      AND A~LGNUM = 'P01'
      AND A~LGTYP = I_LGTYP
      AND WERKS = I_WERKS
      AND LVORM = ' '.
    IF SY-SUBRC = 0.
      O_RESULT = 'S'.
      O_MESS = 'Success'.
    ELSE.
      O_RESULT = 'E'.
      O_MESS = 'No data found'.
    ENDIF.
  ENDIF.

ENDFUNCTION.

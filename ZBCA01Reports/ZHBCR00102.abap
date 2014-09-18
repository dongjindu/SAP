************************************************************************
*** Report  ID   : ZHBCR00102
*** Report  Name : SE11 TABLE USEDLIST BDC
*** Created By   : MIRALUZ Co.Ltd
*** Description.
*
*
************************************************************************

REPORT ZHBCR00102 .

TABLES: ctu_params.

DATA: BEGIN OF bdcdata OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF bdcdata.
DATA: msgtab  LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
*PARAMETER : tab_name(15) OBLIGATORY.
PARAMETER : tab_name LIKE DD02L-TABNAME OBLIGATORY MEMORY ID dtb.

START-OF-SELECTION.

  CLEAR bdcdata.
  REFRESH bdcdata.

  PERFORM bdc_dynpro      USING 'SAPMSRD0'          '0102'.
  PERFORM bdc_field       USING 'BDC_OKCODE'        '=CREF'.
  PERFORM bdc_field       USING 'RSRD1-TBMA'        'X'.
  PERFORM bdc_field       USING 'RSRD1-TBMA_VAL'    tab_name.

  PERFORM bdc_dynpro      USING 'SAPICDT_'          '0101'.
  PERFORM bdc_field       USING 'RSEUX-CP'          'X'.
  PERFORM bdc_field       USING 'RSEUX-CO'         'X'.
  PERFORM bdc_field       USING 'RSEUX-CWO'         'X'.
  PERFORM bdc_field       USING 'RSEUX-CFF'         'X'.
  PERFORM bdc_field       USING 'RSEUX-CDV'        'X'.
  PERFORM bdc_field       USING 'BDC_OKCODE'        '=AREA'.

  PERFORM bdc_dynpro      USING 'SAPLSEUA'          '0020'.
  PERFORM bdc_field       USING 'BDC_OKCODE'        '=%004'.

  PERFORM bdc_dynpro      USING 'SAPLALDB'          '3000'.
  PERFORM bdc_field       USING 'RSCSEL-SLOW_I(01)' 'Z*'.
  PERFORM bdc_field       USING 'RSCSEL-SLOW_I(02)' 'Y*'.
  PERFORM bdc_field       USING 'BDC_OKCODE'        '=ACPT'.

  PERFORM bdc_dynpro      USING 'SAPLSEUA'          '0020'.
  PERFORM bdc_field       USING 'XADIR-LOW'          'Z*'.
  PERFORM bdc_field       USING 'BDC_OKCODE'        '=%009'.

  PERFORM bdc_dynpro      USING 'SAPLALDB'          '3000'.
  PERFORM bdc_field       USING 'RSCSEL-SLOW_I(01)' 'SELECT'.
  PERFORM bdc_field       USING 'RSCSEL-SLOW_I(02)' 'UPDATE'.
  PERFORM bdc_field       USING 'RSCSEL-SLOW_I(03)' 'DELETE'.
  PERFORM bdc_field       USING 'RSCSEL-SLOW_I(04)' 'MODIFY'.
  PERFORM bdc_field       USING 'BDC_OKCODE'        '=ACPT'.

  PERFORM bdc_dynpro      USING 'SAPLSEUA'          '0020'.
  PERFORM bdc_field       USING 'XADIR-LOW'          'Z*'.
  PERFORM bdc_field       USING '$KEYWORD-LOW'      'SELECT'.
  PERFORM bdc_field       USING 'BDC_OKCODE'        '=CRET'.


  PERFORM bdc_dynpro      USING 'SAPICDT_'          '0101'.
  PERFORM bdc_field       USING 'RSEUX-CP'          'X'.
  PERFORM bdc_field       USING 'RSEUX-CO'         'X'.
  PERFORM bdc_field       USING 'RSEUX-CWO'         'X'.
  PERFORM bdc_field       USING 'RSEUX-CFF'         'X'.
  PERFORM bdc_field       USING 'RSEUX-CDV'        'X'.
  PERFORM bdc_field       USING 'BDC_OKCODE'        '=ENTR'.

  PERFORM bdc_dynpro      USING 'SAPLSPO1'          '0100'.
  PERFORM bdc_field       USING 'BDC_OKCODE'        '=NO'.

  PERFORM bdc_dynpro      USING 'SAPLSEUO'          '0100'.
  PERFORM bdc_field       USING 'BDC_OKCODE'        '=GOON'.

  PERFORM bdc_dynpro      USING 'SAPLSPO1'          '0100'.
  PERFORM bdc_field       USING 'BDC_OKCODE'        '=NO'.

  PERFORM bdc_dynpro      USING 'SAPMSSY0'          '0120'.
  PERFORM bdc_field       USING 'BDC_OKCODE'        '=MARK'.

  PERFORM bdc_dynpro      USING 'SAPMSSY0'          '0120'.
  PERFORM bdc_field       USING 'BDC_OKCODE'        '=DETA'.


*  PERFORM bdc_dynpro      USING 'SAPMSSY0'          '0120'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'        '=WB_END'.
*
  PERFORM bdc_transaction.
*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0027   text
*      -->P_0028   text
*----------------------------------------------------------------------*
FORM bdc_dynpro USING   pgmname scrno.
  bdcdata-dynbegin = 'X'.
  bdcdata-program     = pgmname.
  bdcdata-dynpro     = scrno.
  APPEND bdcdata.
  CLEAR  bdcdata.

ENDFORM.                    " bdc_dynpro
*&---------------------------------------------------------------------*
*&      Form  bdc_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0032   text
*      -->P_0033   text
*----------------------------------------------------------------------*
FORM bdc_field USING   fname fval.

  bdcdata-dynbegin = ' '.
  bdcdata-fnam     = fname.
  bdcdata-fval     = fval.
  APPEND bdcdata.
  CLEAR  bdcdata.

ENDFORM.                    " bdc_field
*&---------------------------------------------------------------------*
*&      Form  bdc_transaction
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_transaction.

  ctu_params-dismode = 'E'.
  ctu_params-updmode = 'S'.
*  ctu_params-NOBIEND = 'X'.
*
*  ctu_params-defsize = 'X'.

  CALL TRANSACTION 'SE11_OLD' USING bdcdata
                          OPTIONS FROM ctu_params
                          MESSAGES INTO msgtab.
  LEAVE PROGRAM.

ENDFORM.                    " bdc_transaction

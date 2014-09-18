report Z_FB09 no standard page heading line-size 255.

include bdcrecx1.

parameters: dataset(132) lower case.
***    DO NOT CHANGE - the generated data section - DO NOT CHANGE    ***
*
*   If it is nessesary to change the data section use the rules:
*   1.) Each definition of a field exists of two lines
*   2.) The first line shows exactly the comment
*       '* data element: ' followed with the data element
*       which describes the field.
*       If you don't have a data element use the
*       comment without a data element name
*   3.) The second line shows the fieldname of the
*       structure, the fieldname must consist of
*       a fieldname and optional the character '_' and
*       three numbers and the field length in brackets
*   4.) Each field must be type C.
*
*** Generated data section with specific formatting - DO NOT CHANGE  ***
data: begin of record,
* data element: BELNR_D
        BELNR_001(010),
* data element: BUKRS
        BUKRS_002(004),
* data element: GJAHR
        GJAHR_003(004),
* data element: BUZEI
        BUZEI_004(003),
* data element: VALUT
        VALUT_005(010),
* data element: DZUONR
        ZUONR_006(018),
* data element: FMORE
        FMORE_007(001),
      end of record.

*** End generated data section ***
TABLES :GLT0,BSIS,BKPF,T001,SKAT,BSEG,LFA1,SKA1,KNA1,SKB1.
DATA : IBSIS  LIKE BSIS OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF itab OCCURS 0,
         HKONT  LIKE  BSIS-HKONT,
         BELNR  LIKE  BSIS-BELNR,
         BUZEI  LIKE  BSIS-BUZEI,
         BUDAT  LIKE  BSIS-BUDAT,
         VALUT  LIKE  BSIS-VALUT,
       END OF itab.

PARAMETERS : P_BUKRS LIKE BSIS-BUKRS memory id BUK,
             P_GJAHR(4) type c.
*             P_HKONT LIKE BSIS-HKONT OBLIGATORY.
SELECT-OPTIONS : R_HKONT FOR BSIS-HKONT,
                 S_VALUT FOR BSIS-VALUT,
                 S_BELNR FOR BSIS-BELNR,
                 S_BUDAT FOR BSIS-BUDAT.
parameters: p_run as checkbox.

start-of-selection.

  IF  R_HKONT[] IS INITIAL.
    PERFORM GET_HKONT.
  ENDIF.
  PERFORM GET_BELNR. "???(bsis)

  loop at itab.
    check itab-valut <> itab-budat.

    write:/ itab-belnr, itab-buzei, itab-valut, ' --> ', itab-budat.
    if p_run = 'X'.
      perform update_doc.
    endif.
  endloop.
*  PERFORM GET_BSIS.  "G.L??


*&---------------------------------------------------------------------*
*&      Form  GET_BELNR
*&---------------------------------------------------------------------*
FORM GET_BELNR.

  SELECT DISTINCT S~HKONT S~BELNR S~BUZEI S~BUDAT S~VALUT
    INTO TABLE itab
    FROM BSIS AS S INNER JOIN BKPF AS K
         ON    S~BUKRS = K~BUKRS
           AND S~GJAHR = K~GJAHR
           AND S~BELNR = K~BELNR
   WHERE S~BUKRS = P_BUKRS
     AND S~GJAHR = P_GJAHR
     AND S~HKONT IN R_HKONT
     AND S~BUDAT IN S_BUDAT
     AND S~VALUT IN S_VALUT
     AND S~BELNR IN S_BELNR
     AND K~STBLG = SPACE.
ENDFORM.                    " GET_BELNR
*&---------------------------------------------------------------------*
*&      Form  GET_HKONT
*&---------------------------------------------------------------------*
FORM GET_HKONT.
  CLEAR: R_HKONT[],R_HKONT.
  R_HKONT-SIGN = 'I'.     R_HKONT-OPTION = 'EQ'.
  SELECT SAKNR INTO R_HKONT-LOW
    FROM SKB1
   WHERE BUKRS EQ P_BUKRS
     AND XGKON EQ 'X'
     AND FDLEV LIKE 'B%'.
    APPEND R_HKONT.
  ENDSELECT.
ENDFORM.                    " GET_HKONT
*&---------------------------------------------------------------------*
*&      Form  GET_BSIS
*&---------------------------------------------------------------------*
FORM GET_BSIS.
  SELECT * FROM BSIS INTO TABLE IBSIS
   WHERE BUKRS = P_BUKRS
     AND GJAHR = P_GJAHR
     AND HKONT IN R_HKONT
     AND BUDAT IN S_BUDAT
     AND VALUT IN S_VALUT
     AND BELNR IN S_BELNR.
ENDFORM.                    " GET_BSIS
*&---------------------------------------------------------------------*
*&      Form  update_doc
*&---------------------------------------------------------------------*
FORM update_doc.
  perform bdc_dynpro      using 'SAPMF05L' '0102'.
  perform bdc_field       using 'BDC_CURSOR'    'RF05L-BUZEI'.
  perform bdc_field       using 'BDC_OKCODE'    '/00'.
  perform bdc_field       using 'RF05L-BELNR'     itab-BELNR.
  perform bdc_field       using 'RF05L-BUKRS'     p_BUKRS.
  perform bdc_fld         using 'RF05L-GJAHR'     p_GJAHR.
  perform bdc_fld         using 'RF05L-BUZEI'     itab-BUZEI.
  perform bdc_dynpro      using 'SAPMF05L' '0300'.
  perform bdc_field       using 'BDC_CURSOR'    'BSEG-VALUT'.
  perform bdc_field       using 'BDC_OKCODE'    '=AE'.
  perform bdc_field       using 'BSEG-VALUT'      itab-budat.
  perform bdc_field       using 'DKACB-FMORE' 'X'.
  perform bdc_dynpro      using 'SAPLKACB' '0002'.
  perform bdc_field       using 'BDC_CURSOR'     'COBL-KDAUF'.
  perform bdc_field       using 'BDC_OKCODE'     '=ENTE'.
  perform bdc_transaction using 'FB09'.

ENDFORM.                    " update_doc

FORM BDC_FLD USING FNAM FVAL.
    CLEAR BDCDATA.
    BDCDATA-FNAM = FNAM.
    BDCDATA-FVAL = FVAL.
    APPEND BDCDATA.
ENDFORM.

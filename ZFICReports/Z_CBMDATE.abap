report Z_FB09 no standard page heading line-size 255.

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
  data: l_fdtag like bseg-fdtag.

  IF  R_HKONT[] IS INITIAL.
    PERFORM GET_HKONT.
  ENDIF.
  PERFORM GET_BELNR. "???(bsis)

  loop at itab.
*    if itab-valut <> itab-budat.
*      write:/ 'VALUT:', itab-belnr, itab-buzei,
*              itab-valut, ' --> ', itab-budat.
*      perform update_doc.
*    endif.

    select single * from bseg
      where bukrs = p_bukrs
        and gjahr = p_gjahr
        and belnr = itab-belnr
        and buzei = itab-buzei.

    if bseg-fdtag <> itab-valut.
      write:/ 'FDTAG:', itab-belnr, itab-buzei,
              bseg-fdtag, ' --> ', itab-budat.
      if p_run = 'X'.
        bseg-fdtag = itab-valut.
        update bseg.
        write: '... updated'.
      endif.
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
     AND XGKON EQ 'X'.
*     AND FDLEV LIKE 'B%'.
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
  check p_run = 'X'.
*  perform bdc_dynpro      using 'SAPMF05L' '0102'.
*  perform bdc_field       using 'BDC_CURSOR'    'RF05L-BUZEI'.
*  perform bdc_field       using 'BDC_OKCODE'    '/00'.
*  perform bdc_field       using 'RF05L-BELNR'     itab-BELNR.
*  perform bdc_field       using 'RF05L-BUKRS'     p_BUKRS.
*  perform bdc_fld         using 'RF05L-GJAHR'     p_GJAHR.
*  perform bdc_fld         using 'RF05L-BUZEI'     itab-BUZEI.
*  perform bdc_dynpro      using 'SAPMF05L' '0300'.
*  perform bdc_field       using 'BDC_CURSOR'    'BSEG-VALUT'.
*  perform bdc_field       using 'BDC_OKCODE'    '=AE'.
*  perform bdc_field       using 'BSEG-VALUT'      itab-budat.
*  perform bdc_field       using 'DKACB-FMORE' 'X'.
*  perform bdc_dynpro      using 'SAPLKACB' '0002'.
*  perform bdc_field       using 'BDC_CURSOR'     'COBL-KDAUF'.
*  perform bdc_field       using 'BDC_OKCODE'     '=ENTE'.
*  perform bdc_transaction using 'FB09'.

ENDFORM.                    " update_doc

*---------------------------------------------------------------------*
*       FORM BDC_FLD                                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FNAM                                                          *
*  -->  FVAL                                                          *
*---------------------------------------------------------------------*
*FORM BDC_FLD USING FNAM FVAL.
*  CLEAR BDCDATA.
*  BDCDATA-FNAM = FNAM.
*  BDCDATA-FVAL = FVAL.
*  APPEND BDCDATA.
*ENDFORM.

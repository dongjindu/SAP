*----------------------------------------------------------------------*
***INCLUDE ZXRSAF01 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  GET_EKKN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EKKN_EBELN  text
*      -->P_EKKN_EBELP  text
*      -->P_L_ZAMC02M_0SCL  text
*----------------------------------------------------------------------*
FORM GET_EKKN USING    P_EKKN_EBELN
                       P_EKKN_EBELP
                       P_L_ZAMC02M_0SCL TYPE ZAMC02M_0SCL.
  select SINGLE KOKRS KOSTL
                AUFNR GSBER
                GEBER
                FISTL FIPOS
                SAKTO
                ANLN1 ANLN2
    INTO (P_L_ZAMC02M_0SCL-ZZKOKRS, P_L_ZAMC02M_0SCL-ZZKOSTL,
          P_L_ZAMC02M_0SCL-ZZAUFNR, P_L_ZAMC02M_0SCL-ZZGSBER,
          P_L_ZAMC02M_0SCL-ZZGEBER,
          P_L_ZAMC02M_0SCL-ZZFISTL, P_L_ZAMC02M_0SCL-ZZFIPOS,
          P_L_ZAMC02M_0SCL-ZZSAKTO,
          P_L_ZAMC02M_0SCL-ZZANLN1, P_L_ZAMC02M_0SCL-ZZANLN2)
    FROM EKKN
   WHERE EBELN = P_EKKN_EBELN
     AND EBELP = P_EKKN_EBELP
     AND ZEKKN = '01'.

ENDFORM.                    " GET_EKKN

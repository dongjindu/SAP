*&---------------------------------------------------------------------*
*& Report  ZHBCR00103
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZHBCR00103.
TABLES : ZHBCT001.

DATA : BEGIN OF IT_ZCAL OCCURS 0.
         INCLUDE STRUCTURE ZHBCT001.
DATA : END OF IT_ZCAL.
DATA : W_DO Type I.

Parameter : p_char Type C.

Start-Of-Selection.

 CLEAR : IT_ZCAL, IT_ZCAL[].

 IF P_CHAR = '+' .
    IT_ZCAL-ZDATE = SY-DATUM.
    APPEND IT_ZCAL.

    DO  36000 TIMES .
       CLEAR IT_ZCAL.
       IT_ZCAL-ZDATE = SY-DATUM + SY-INDEX .
       APPEND IT_ZCAL.
    ENDDO.
 ELSEIF P_CHAR = '-'.
    DO  18000 TIMES .
       IT_ZCAL-ZDATE = SY-DATUM - SY-INDEX .
       APPEND IT_ZCAL.
       CLEAR IT_ZCAL.
    ENDDO.
 ENDIF.

 INSERT ZHBCT001 FROM TABLE IT_ZCAL ACCEPTING DUPLICATE KEYS.
 COMMIT WORK.

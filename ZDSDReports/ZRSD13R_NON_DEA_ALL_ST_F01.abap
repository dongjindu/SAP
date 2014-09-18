*----------------------------------------------------------------------*
***INCLUDE ZRSD13R_NON_DEA_ALL_ST_F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
FORM READ_DATA.
  PERFORM GET_ATINN USING 'P_RP_STATUS'.

  SELECT *
         INTO TABLE IT_AUSP
         FROM AUSP
        WHERE OBJEK IN ( SELECT OBJEK
                                FROM AUSP
                               WHERE ATINN = W_ATINN
                                 AND KLART = '002'
                                 AND ATWRT IN ('19',   "C/GATE
                                               '20',   "UNDER BODY COA
                                               '21',   "DEALER ALLO
                                               '22',   "VPC IN
                                               '23',   "VPC OUT
                                               '24',   "TRUCKING IN
                                               '26') ) "RAILING IN
          AND ATINN IN R_ATINN
          AND KLART = '002'.
ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  INIT_CABN
*&---------------------------------------------------------------------*
FORM INIT_CABN.
  REFRESH : IT_CABN, R_ATINN.
  CLEAR   : IT_CABN, R_ATINN.

  R_ATINN-SIGN = 'I'.
  R_ATINN-OPTION = 'EQ'.

  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_RP_STATUS'.
  APPEND IT_CABN.
* R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_RP19_ACTUAL_DATE'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_RP20_ACTUAL_DATE'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_RP22_ACTUAL_DATE'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_RP23_ACTUAL_DATE'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_RP24_ACTUAL_DATE'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_RP26_ACTUAL_DATE'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_MODEL'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_BODY_SERIAL'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_WORK_ORDER'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_EXT_COLOR'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_INT_COLOR'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_MODEL_YEAR'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_MI'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_OCN'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_VIN'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_DEALER_NO'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_DECOMMIT'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
ENDFORM.                    " INIT_CABN
*&---------------------------------------------------------------------*
*&      Form  GET_ATINN
*&---------------------------------------------------------------------*
FORM GET_ATINN USING P_ATNAM.
  READ TABLE IT_CABN WITH KEY ATNAM = P_ATNAM.
  IF SY-SUBRC = 0.
    W_ATINN = IT_CABN-ATINN.
  ENDIF.
ENDFORM.                    " GET_ATINN
*&---------------------------------------------------------------------*
*&      Form  GET_ATNAM
*&---------------------------------------------------------------------*
FORM GET_ATNAM USING P_ATINN.
  READ TABLE IT_CABN WITH KEY ATINN = P_ATINN.
  IF SY-SUBRC = 0.
    W_ATNAM = IT_CABN-ATNAM.
  ENDIF.
ENDFORM.                    " GET_ATINN
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM MODIFY_DATA.
  DATA : W_OBJEK LIKE AUSP-OBJEK.

  REFRESH IT_NON_DEA. CLEAR IT_NON_DEA.

  READ TABLE IT_AUSP INDEX 1.
  W_OBJEK = IT_AUSP-OBJEK.

  LOOP AT IT_AUSP.
    IF W_OBJEK <> IT_AUSP-OBJEK.
      APPEND T_NON_DEA TO IT_NON_DEA. CLEAR T_NON_DEA.
      W_OBJEK = IT_AUSP-OBJEK.
    ENDIF.

    PERFORM GET_ATNAM USING IT_AUSP-ATINN.

    IF W_ATNAM = 'P_RP26_ACTUAL_DATE' AND
       T_NON_DEA-RP24_ACTUAL_DATE IS INITIAL.
      T_NON_DEA-RP24_ACTUAL_DATE = IT_AUSP-ATWRT+0(8).
    ELSE.
      CONCATENATE 'T_NON_DEA-' W_ATNAM+2(20) INTO FIELD.
      ASSIGN (FIELD) TO <FS>.
      <FS> = IT_AUSP-ATWRT+0(8).
    ENDIF.
  ENDLOOP.
  IF SY-SUBRC = 0.
    APPEND T_NON_DEA TO IT_NON_DEA. CLEAR T_NON_DEA.
  ENDIF.

  DELETE IT_NON_DEA WHERE RP19_ACTUAL_DATE > SY-DATUM.
  DELETE IT_NON_DEA WHERE DEALER_NO <> ''
                    AND   DECOMMIT  <> 'H'.

ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN
*&---------------------------------------------------------------------*
FORM CALL_SCREEN.
  SORT IT_NON_DEA BY DEALER_NO.
  DESCRIBE TABLE IT_NON_DEA LINES W_CNT.
  IF W_CNT = 0.
     MESSAGE I000 WITH TEXT-M01.
  ELSE.
     CALL SCREEN 9000.
  ENDIF.
ENDFORM.                    " CALL_SCREEN

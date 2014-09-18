*----------------------------------------------------------------------*
*   INCLUDE MZAHR0006F01                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  CHECK_CODE_BOOK
*&---------------------------------------------------------------------*
FORM CHECK_CODE_BOOK.
  DATA: L_ZVAL1     LIKE ZTHR_PCP02-ZVAL1.
*
  CLEAR W_ZCODE.
  CLEAR R_ZJOBK. REFRESH R_ZJOBK.
  CONCATENATE W_PERNR '%' INTO L_ZVAL1.
*
  CLEAR ZTHR_PCP02.
  SELECT SINGLE ZCODE ZVAL4 INTO (ZTHR_PCP02-ZCODE, ZTHR_PCP02-ZVAL4)
    FROM ZTHR_PCP02 WHERE ZMODL = '02'
                      AND ZGRUP = '1000'
                      AND ZVAL1 LIKE L_ZVAL1
                      AND ZVAL3 > SY-DATUM.
*
  IF SY-SUBRC = 0.
    W_ZCODE = ZTHR_PCP02-ZCODE.
    IF ZTHR_PCP02-ZVAL4 CS 'MASTER'.
      W_MASTR = 'X'.
    ENDIF.
    CLEAR ZTHR_PCP02.
    SELECT ZVAL1 INTO ZTHR_PCP02-ZVAL1
      FROM ZTHR_PCP02 WHERE ZMODL = '02'
                        AND ZGRUP = '1010'.
      R_ZJOBK-SIGN = 'I'.
      R_ZJOBK-OPTION = 'EQ'.
      R_ZJOBK-LOW = ZTHR_PCP02-ZVAL1+(8).
      APPEND R_ZJOBK. CLEAR R_ZJOBK.
    ENDSELECT.
  ELSE.
    MESSAGE W001 WITH 'Check the validity date'.
  ENDIF.
ENDFORM.                    " CHECK_CODE_BOOK
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_FROM_PCP03
*&---------------------------------------------------------------------*
FORM GET_DATA_FROM_PCP03.
  CLEAR IT_PCP03. REFRESH IT_PCP03.
*
  CLEAR ZTHR_PCP03.
  SELECT ZCOST ZJOBK ZHEDC ZWEKN ZWEKE ZSATU ZHOLI
         ZTOTL ERDAT ERZET ERNAM AEDAT AEZET AENAM
    INTO CORRESPONDING FIELDS OF TABLE IT_PCP03
*   INTO (ZTHR_PCP03-ZCOST, ZTHR_PCP03-ZJOBK, ZTHR_PCP03-ZHEDC,
*         ZTHR_PCP03-ZWEKN, ZTHR_PCP03-ZWEKE, ZTHR_PCP03-ZSATU,
*         ZTHR_PCP03-ZHOLI, ZTHR_PCP03-ZTOTL, ZTHR_PCP03-ERDAT,
*         ZTHR_PCP03-ERZET, ZTHR_PCP03-ERNAM, ZTHR_PCP03-AEDAT,
*         ZTHR_PCP03-AEZET, ZTHR_PCP03-AENAM)
    FROM ZTHR_PCP03 WHERE ZYEAR = W_ZYEAR
                      AND ZVERS = W_ZVERS
                      AND ZMONS = W_ZMONS.
*   IT_PCP03-ZYEAR = W_ZYEAR.
*   IT_PCP03-ZVERS = W_ZVERS.
*   IT_PCP03-ZMONS = W_ZMONS.
*   IT_PCP03-ZCOST = ZTHR_PCP03-ZCOST.
*   IT_PCP03-ZJOBK = ZTHR_PCP03-ZJOBK.
*   IT_PCP03-ZHEDC = ZTHR_PCP03-ZHEDC.
*   IT_PCP03-ZWEKN = ZTHR_PCP03-ZWEKN.
*   IT_PCP03-ZWEKE = ZTHR_PCP03-ZWEKE.
*   IT_PCP03-ZSATU = ZTHR_PCP03-ZSATU.
*   IT_PCP03-ZHOLI = ZTHR_PCP03-ZHOLI.
*   IT_PCP03-ZTOTL = ZTHR_PCP03-ZTOTL.
*   APPEND IT_PCP03. CLEAR IT_PCP03.
* ENDSELECT.
*
  IF SY-SUBRC = 0.
    IT_PCP03-ZYEAR = W_ZYEAR.
    IT_PCP03-ZVERS = W_ZVERS.
    IT_PCP03-ZMONS = W_ZMONS.
    MODIFY IT_PCP03 TRANSPORTING ZYEAR ZVERS ZMONS
                    WHERE ZYEAR = SPACE.
  ELSE.
    PERFORM GET_COST_CENTER_OF_ORGUNIT.
  ENDIF.
*
  REFRESH CONTROL 'TC9000' FROM SCREEN 9000.
  DESCRIBE TABLE IT_PCP03 LINES TC9000-LINES.
ENDFORM.                    " GET_DATA_FROM_PCP03
*&---------------------------------------------------------------------*
*&      Form  GET_COST_CENTER_OF_ORGUNIT
*&---------------------------------------------------------------------*
FORM GET_COST_CENTER_OF_ORGUNIT.
  CLEAR: IT_UNITS, IT_PERSN, IT_ORGPN.
  REFRESH: IT_UNITS, IT_PERSN, IT_ORGPN.
*
  CALL FUNCTION 'RH_DIR_ORG_STRUC_GET'
       EXPORTING
            ACT_ORGUNIT           = W_ORGEH
            ACT_PLVAR             = '01'
            ACT_DATE              = SY-DATUM
            SORT_FLAG             = 'X'
            ADD_FLAG_PDATA        = 'X'
       TABLES
            ORG_UNITS             = IT_UNITS
            PERSON_TAB            = IT_PERSN
            ORG_PERS_REL          = IT_ORGPN
       EXCEPTIONS
            NO_ACTIVE_PLVAR       = 1
            OTHERS                = 2.
*
  LOOP AT IT_UNITS WHERE COSTCENTER <> SPACE.
    CLEAR ZTHR_HCP01.
    SELECT ZJOBK ZHEDC INTO (ZTHR_HCP01-ZJOBK, ZTHR_HCP01-ZHEDC)
      FROM ZTHR_HCP01 WHERE ZVERS = W_ZVERS
                        AND ZYEAR = W_ZYEAR
                        AND ZMONS = W_ZMONS
                        AND ZPERA = W_WERKS
                        AND ZCOST = IT_UNITS-COSTCENTER.
      IT_PCP03-ZYEAR = W_ZYEAR.
      IT_PCP03-ZVERS = W_ZVERS.
      IT_PCP03-ZMONS = W_ZMONS.
      IT_PCP03-ZCOST = IT_UNITS-COSTCENTER.
      IT_PCP03-ZJOBK = ZTHR_HCP01-ZJOBK.
      IT_PCP03-ZHEDC = ZTHR_HCP01-ZHEDC.
      APPEND IT_PCP03. CLEAR IT_PCP03.
    ENDSELECT.
  ENDLOOP.
*
  IF IT_PCP03[] IS INITIAL.
    MESSAGE W009. EXIT.
  ENDIF.
*
  LOOP AT IT_PCP03.
    CLEAR HRP1000.
    SELECT SINGLE OBJID INTO HRP1000-OBJID
      FROM HRP1000 WHERE PLVAR = '01'
                     AND OTYPE = 'C'
                     AND ISTAT = '1'
                     AND ENDDA = '99991231'
                     AND LANGU = SY-LANGU
                     AND SHORT = IT_PCP03-ZJOBK.
    IT_PCP03-OBJID = HRP1000-OBJID.
    MODIFY IT_PCP03. CLEAR IT_PCP03.
  ENDLOOP.
*
  LOOP AT IT_PCP03.
    CLEAR R_ZJOBK.
    READ TABLE R_ZJOBK WITH KEY LOW = IT_PCP03-OBJID.
    IF SY-SUBRC = 0.
      CLEAR CSKT.
      SELECT SINGLE KTEXT INTO CSKT-KTEXT
        FROM CSKT WHERE SPRAS = SY-LANGU
                    AND KOSTL = IT_PCP03-ZCOST
                    AND DATBI = '99991231'.
      IT_PCP03-KTEXT = CSKT-KTEXT.
      MODIFY IT_PCP03. CLEAR IT_PCP03.
    ELSE.
      DELETE IT_PCP03.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " GET_COST_CENTER_OF_ORGUNIT
*&---------------------------------------------------------------------*
*&      Form  SAVE_PCP03_DATA
*&---------------------------------------------------------------------*
FORM SAVE_PCP03_DATA.
  CLEAR ZTHR_PCP03.
  MODIFY ZTHR_PCP03 FROM TABLE IT_PCP03.
  IF SY-SUBRC = 0.
    MESSAGE S001 WITH 'DATA SAVED'.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " SAVE_PCP03_DATA
*&---------------------------------------------------------------------*
*&      Form  FINAL_SAVE
*&---------------------------------------------------------------------*
FORM FINAL_SAVE.
  CLEAR ZTHR_PCP02.
  UPDATE ZTHR_PCP02 SET ZVAL3 = SY-DATUM
                  WHERE ZMODL = '02'
                    AND ZGRUP = '1000'
                    AND ZCODE = W_ZCODE.
ENDFORM.                    " FINAL_SAVE

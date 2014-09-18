REPORT Z_FS00 .
*&--------------------------------------------------------------------*
*& Program Name : ZBDC_COA. *
*& Description : Upload Chart of Account *
*& Functional Area : FI-GL *
*&---------------------------------------------------------------------*
*& Change History : *
*& Date Author Description/Reason for Change *
*& ---------- -------------- ------------------------------------ *
*& 2000.11.08 Initial Development
*&---------------------------------------------------------------------*

* From DATA FILE
tables: SKA1, t001.

data: begin of rec occurs 0,
*       BILKT LIKE SKA1-BILKT, " Group account number
        SAKNR LIKE SKA1-SAKNR, " Account number
        TXT20 LIKE SKAT-TXT20, " Short_text
        TXT50 LIKE SKAT-TXT50, " Long_text
        XBILK LIKE SKA1-XBILK, " B/S Account check
*       GVTYP LIKE SKA1-GVTYP, " P/L statement account type
        KTOKS LIKE SKA1-KTOKS, " Account group
* BUKRS LIKE RF02H-BUKRS, " Chart of account
* VBUND LIKE SKA1-VBUND, " Trading partner
        FAREA LIKE SKA1-FUNC_AREA, " Functional area
        WAERS LIKE SKB1-WAERS, " Currency
        XSALH LIKE SKB1-XSALH, " Only balances in local crcy
        KDFSL LIKE SKB1-KDFSL, " Exchange rate difference key
        MWSKZ LIKE SKB1-MWSKZ, " Tex category
        XMWNO LIKE SKB1-XMWNO, " Posting without tax allowed
        MITKZ LIKE SKB1-MITKZ, " Reconciliation account
        XOPVW LIKE SKB1-XOPVW, " Open item management
        XKRES LIKE SKB1-XKRES, " Line Item Management
        ZUAWA LIKE SKB1-ZUAWA, " Sort key
* BEGRU LIKE SKB1-BEGRU, " Authorization group
* BUSAB LIKE SKB1-BUSAB, " Accounting clerk
        FSTAG LIKE SKB1-FSTAG, " Field status group
        XINTB LIKE SKB1-XINTB, " Post automatically only
        XNKON LIKE SKB1-XNKON, " Supplement auto. posting
        FDLEV LIKE SKB1-FDLEV, " Planning level
        XGKON LIKE SKB1-XGKON, " Relevant to cash flow
        FIPOS LIKE SKB1-FIPOS, " Commitment Item
        HBKID LIKE SKB1-HBKID, " House bank
        HKTID LIKE SKB1-HKTID, " Bank account
*       COST1(1) TYPE C, " Cost Element

* KIND, " B/S,P/L KIND
* WMETH LIKE SKB1-WMETH, " Account managed in ext. system
* TOGRU LIKE SKB1-TOGRU, " Tolerance group
      end of rec.


DATA : BEGIN OF BDC_TAB OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA : END OF BDC_TAB.

DATA : BEGIN OF MESSTAB OCCURS 5.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA : END OF MESSTAB.
DATA : FILE_TYPE LIKE RLGRAP-FILETYPE VALUE 'DAT'.
DATA : I TYPE I VALUE 1.
DATA : PX type c value 'X',
       PS type c value ' '.

PARAMETERS : p_bukrs like T001-bukrs memory id buk.
select-options: p_saknr for ska1-saknr.

PARAMETERS: p_create as checkbox default ' '.
PARAMETERS: p_com as checkbox default 'X'.
PARAMETERS: p_file LIKE RLGRAP-FILENAME
                  DEFAULT 'C:\temp\coa.TXT'.

PARAMETERS p_mod LIKE CTU_PARAMS-DISMODE DEFAULT 'N'. " no-display.

*************** MAIN PROGRAM ***************
*-----------------------------------------------------------------------
START-OF-SELECTION.
*-----------------------------------------------------------------------
  data: l_saknr like ska1-saknr.

  select single * from t001 where bukrs = p_bukrs.
  check sy-subrc = 0.

  call FUNCTION 'WS_UPLOAD'
       EXPORTING
            FILENAME = p_file
            FILETYPE = FILE_TYPE
       TABLES
            DATA_TAB = REC.

  LOOP AT REC.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
         EXPORTING
              INPUT  = rec-saknr
         IMPORTING
              OUTPUT = l_saknr.
    check l_saknr in p_saknr.


    IF p_com = 'X'.
      PERFORM GENERATE_BDC_DATA_COM.
    ELSE.
      PERFORM GENERATE_BDC_DATA_COA.
    ENDIF.

    perform call_transaction.

    perform write_log.
    refresh MESSTAB.
  ENDLOOP.

*-----------------------------------------------------------------------
END-OF-SELECTION.
*-----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*& Form GENERATE_BDC_DATA_COM
*&---------------------------------------------------------------------*
FORM GENERATE_BDC_DATA_COM.

  REFRESH BDC_TAB.

  IF p_create = 'X'.
    PERFORM BDC_DYNPRO USING 'SAPMF02H' '402'.
  ELSE.
    PERFORM BDC_DYNPRO USING 'SAPMF02H' '401'.
  ENDIF.
  PERFORM BDC_FIELD USING 'RF02H-SAKNR' REC-SAKNR PS.
  PERFORM BDC_FIELD USING 'RF02H-BUKRS' p_bukrs PS.
  PERFORM BDC_FIELD USING 'BDC_OKCODE' '/00'   PS.

  IF p_create = space.
    PERFORM BDC_DYNPRO USING 'SAPMF02H' '310'.
    PERFORM BDC_FIELD USING 'SKAT-TXT20' REC-TXT20 PS.
    PERFORM BDC_FIELD USING 'SKAT-TXT50' REC-TXT50 PS.
    PERFORM BDC_FIELD USING 'BDC_OKCODE' '/00' PS.
  endif.

  PERFORM BDC_DYNPRO USING 'SAPMF02H' '110'.
  PERFORM BDC_FIELD USING 'SKB1-WAERS' REC-WAERS PS.
  PERFORM BDC_FIELD USING 'SKB1-XSALH' REC-XSALH PX.
  PERFORM BDC_FIELD USING 'SKB1-MWSKZ' REC-MWSKZ PX.
  PERFORM BDC_FIELD USING 'SKB1-XMWNO' REC-XMWNO PX.
  PERFORM BDC_FIELD USING 'SKB1-XOPVW' REC-XOPVW PS.
  PERFORM BDC_FIELD USING 'SKB1-XKRES' REC-XKRES PS.
  PERFORM BDC_FIELD USING 'SKB1-ZUAWA' REC-ZUAWA PS.
* PERFORM BDC_FIELD USING 'SKB1-BUSAB' REC-BUSAB.
  PERFORM BDC_FIELD USING 'SKB1-FSTAG' REC-FSTAG PS.
  PERFORM BDC_FIELD USING 'SKB1-XINTB' REC-XINTB PS.
  PERFORM BDC_FIELD USING 'SKB1-XNKON' REC-XNKON PS.
  PERFORM BDC_FIELD USING 'SKB1-FDLEV' REC-FDLEV PX.
  PERFORM BDC_FIELD USING 'SKB1-FIPOS' REC-FIPOS PS.
  IF REC-XBILK = 'X'.
    PERFORM BDC_FIELD USING 'SKB1-KDFSL' REC-KDFSL PS.
    PERFORM BDC_FIELD USING 'SKB1-MITKZ' REC-MITKZ PX.
* PERFORM BDC_FIELD USING 'SKB1-BEGRU' REC-BEGRU.
    PERFORM BDC_FIELD USING 'SKB1-HBKID' REC-HBKID PX.
    PERFORM BDC_FIELD USING 'SKB1-HKTID' REC-HKTID PX.
    PERFORM BDC_FIELD USING 'SKB1-XGKON' REC-XGKON PS.
  ENDIF.

  PERFORM BDC_FIELD USING 'BDC_OKCODE' '=SICH'  PS.

ENDFORM. " GENERATE_BDC_DATA_COM
*&---------------------------------------------------------------------*
*& Form GENERATE_BDC_DATA_COA
*&---------------------------------------------------------------------*
FORM GENERATE_BDC_DATA_COA.
  REFRESH BDC_TAB.

  IF p_create = 'X'.
    PERFORM BDC_DYNPRO USING 'SAPMF02H' '302'.
  ELSE.
    PERFORM BDC_DYNPRO USING 'SAPMF02H' '301'.
  ENDIF.

  PERFORM BDC_FIELD USING 'RF02H-SAKNR' REC-SAKNR   PS.
  PERFORM BDC_FIELD USING 'RF02H-KTOPL' t001-ktopl  PS.
  PERFORM BDC_FIELD USING 'BDC_OKCODE' '/00'  PS.


  IF REC-XBILK = 'X'.
    PERFORM BDC_DYNPRO USING 'SAPMF02H' '310'.
    PERFORM BDC_FIELD USING 'SKAT-TXT20' REC-TXT20 PS.
    PERFORM BDC_FIELD USING 'SKAT-TXT50' REC-TXT50 PS.
    PERFORM BDC_FIELD USING 'SKA1-XBILK' REC-XBILK PS.
    PERFORM BDC_FIELD USING 'SKA1-KTOKS' REC-KTOKS PS.
    PERFORM BDC_FIELD USING 'SKA1-FUNC_AREA' REC-FAREA PS.
  else.
    PERFORM BDC_DYNPRO USING 'SAPMF02H' '310'.
    PERFORM BDC_FIELD USING 'SKAT-TXT20' REC-TXT20 PS.
    PERFORM BDC_FIELD USING 'SKAT-TXT50' REC-TXT50 PS.
    PERFORM BDC_FIELD USING 'SKA1-XBILK' REC-XBILK PS.
    PERFORM BDC_FIELD USING 'SKA1-KTOKS' REC-KTOKS PS.
    PERFORM BDC_FIELD USING 'SKA1-FUNC_AREA' REC-FAREA PS.
    PERFORM BDC_FIELD USING 'SKA1-GVTYP' 'X' PS.
  ENDIF.

  PERFORM BDC_FIELD USING 'BDC_OKCODE' 'SICH' PS.

ENDFORM. " GENERATE_BDC_DATA_COA

*&---------------------------------------------------------------------*
*& Form BDC_DYNPRO
*&---------------------------------------------------------------------*
FORM BDC_DYNPRO USING P_PROGRAM P_DYNPRO.
  CLEAR BDC_TAB.
  BDC_TAB-PROGRAM = P_PROGRAM.
  BDC_TAB-DYNPRO = P_DYNPRO.
  BDC_TAB-DYNBEGIN = 'X'.
  APPEND BDC_TAB.
ENDFORM. " BDC_DYNPRO

*&---------------------------------------------------------------------*
*& Form BDC_FIELD
*&---------------------------------------------------------------------*
FORM BDC_FIELD USING P_FNAM P_FVAL P_CHK.
  if p_chk = 'X' and p_fval = space.
    exit.
  endif.

  CLEAR BDC_TAB.
  BDC_TAB-FNAM = P_FNAM.
  BDC_TAB-FVAL = P_FVAL.
  APPEND BDC_TAB.
ENDFORM. " BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  write_log
*&---------------------------------------------------------------------*
FORM write_log.
  DATA: L_TEXT(200)     TYPE C,
        L_ERR(26)       TYPE C.
  tables: t100.


  LOOP AT MESSTAB.
    WRITE: MESSTAB-MSGTYP TO L_ERR(1),
           '-'            TO L_ERR+1(1),
           MESSTAB-MSGID  TO L_ERR+2(20),
           '-'            TO L_ERR+22(1),
           MESSTAB-MSGNR  TO L_ERR+23(3).
    CONDENSE L_ERR NO-GAPS.
    SELECT SINGLE * FROM T100
      WHERE SPRSL EQ MESSTAB-MSGSPRA
      AND   ARBGB EQ MESSTAB-MSGID
      AND   MSGNR EQ MESSTAB-MSGNR.
    CLEAR L_TEXT.
    MOVE T100-TEXT TO L_TEXT.
    REPLACE '&' WITH MESSTAB-MSGV1 INTO L_TEXT.
    REPLACE '&' WITH MESSTAB-MSGV2 INTO L_TEXT.
    REPLACE '&' WITH MESSTAB-MSGV3 INTO L_TEXT.
    REPLACE '&' WITH MESSTAB-MSGV4 INTO L_TEXT.
    CONDENSE L_TEXT.

    if MESSTAB-MSGTYP(1) = 'E'.
      format COLOR COL_NEGATIVE.
    else.
      format COLOR COL_NORMAL.
    endif.

    WRITE: / rec-saknr  INTENSIFIED OFF,
            (26) L_ERR  INTENSIFIED ON,
            (73) L_TEXT INTENSIFIED OFF.

  ENDLOOP.

ENDFORM.                    " write_log
*&---------------------------------------------------------------------*
*&      Form  call_transaction
*&---------------------------------------------------------------------*
FORM call_transaction.
  IF p_create = 'X'.
    IF p_com = 'X'.
      CALL TRANSACTION 'FS01'
           USING BDC_TAB
           MODE p_mod
           UPDATE 'S'
           MESSAGES INTO MESSTAB.
      I = I + 1.
    ELSE.
      CALL TRANSACTION 'FSP1'
           USING BDC_TAB
           MODE p_mod
           UPDATE 'S'
           MESSAGES INTO MESSTAB.
      I = I + 1.
    ENDIF.
  ELSE.
    IF p_com = 'X'.
      CALL TRANSACTION 'FS02'
           USING BDC_TAB
           MODE p_mod
           UPDATE 'S'
           MESSAGES INTO MESSTAB.
      I = I + 1.
    ELSE.
      CALL TRANSACTION 'FSP2'
           USING BDC_TAB
           MODE p_mod
           UPDATE 'S'
           MESSAGES INTO MESSTAB.
      I = I + 1.
    ENDIF.
  ENDIF.
ENDFORM.                    " call_transaction

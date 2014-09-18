************************************************************************
* Program Name      : ZIMM_MODULE_DECOMPOSITION
* Creation Date     : 09/05/2008
* Development Request No :
* Addl Documentation:
* Description       : Send Delivery Sum to HMC
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT ZIMM_MODULE_DECOMPOSITION NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMPP.

TYPE-POOLS: SLIS, VRM.
TABLES: EKBE,EINA, A018.
DATA: IT_DATA LIKE TABLE OF ZSMM_MOD_DECOMP WITH HEADER LINE.

DATA: BEGIN OF IT_EKBE OCCURS 0,
      BUDAT LIKE EKBE-BUDAT,
      BWART LIKE EKBE-BWART,
      MATNR LIKE EKBE-MATNR,
      GJAHR LIKE EKBE-GJAHR,
      BELNR LIKE EKBE-BELNR,
      MENGE LIKE EKBE-MENGE,
      SHKZG LIKE EKBE-SHKZG,
      WAERS LIKE EKBE-WAERS,
      END OF IT_EKBE.

DATA : BEGIN OF IT_BOM OCCURS 0,
       MATNR LIKE MARA-MATNR,
       DATAB LIKE ZTMM_ASSY_COST1-DATAB,
       DATBI LIKE ZTMM_ASSY_COST1-DATBI,
*       MAKTX LIKE MAKT-MAKTX,
*       NAME1 LIKE LFA1-NAME1,
       UPGVC LIKE MARA-MATNR,
*       PREF  LIKE STPO-POSNR,  "ZTBM_ABXDULDT-pref
       COMP  LIKE MARA-MATNR,
*       CMAKTX LIKE MAKT-MAKTX,
       QNTY LIKE STPO-MENGE,
       MEINS     LIKE   MARA-MEINS,
*       EKGRP LIKE ZTMM_ASSY_COST1-EKGRP,
*       MCODE LIKE ZTMM_ASSY_COST1-MCODE,
*       DATUV LIKE ZTMM_ASSY_COST1-DATAB,
*       DATUB LIKE ZTMM_ASSY_COST1-DATBI,
*            STGB   LIKE STPO-STGB,
       END OF IT_BOM.

DATA: BEGIN OF IT_SUB OCCURS 0,
*        VTYPE     LIKE   ZTMM_ASSY_COST1-VTYPE,   "Vehicle type
        MATNR     LIKE   MARA-MATNR,  "Material
        UPGVC     LIKE   MARA-MATNR,              "UPG-VC
*        PREF      LIKE   STPO-POSNR,
        COMP      LIKE   MARA-MATNR,
*        MAKTX     LIKE   MAKT-MAKTX,              "Description
        LIFNR     LIKE   LFA1-LIFNR,              "Vendor
        AMOUNT    TYPE   F,                       "Component Amount
        QNTY      LIKE   STPO-MENGE,
*        STGB      LIKE   STPO-STGB,
*        UNIT      LIKE   MARA-MEINS,
        MEINS     LIKE   MARA-MEINS,              "UoM(sub)
        KMEIN     LIKE   KONP-KMEIN,              "UoM(Info)
        DATAB     LIKE   SY-DATUM,                "Valid on(sub)
        DATBI     LIKE   SY-DATUM,                "Valid to(Sub)
        NETPR     LIKE   EKPO-NETPR,              "Component Amount
        PEINH     LIKE   EKPO-PEINH,              "Component Price Unit
        WAERS     LIKE   T001-WAERS,              "Currency
        KZUST     LIKE   KONH-KZUST,              "Reason code
*        INCLUDE STRUCTURE ZSMM_CUSTOM_CONDITION.
*DATA:   ZTIR      LIKE   EKPO-NETPR,
*        STS,                                      "Status
*        MSG(100),                                  "Message
      END   OF IT_SUB.

DATA: WA_SUB LIKE IT_SUB.

CONSTANTS: C_DEST(10) VALUE 'WMHR01'.
DATA: C_CAPID   LIKE RC29L-CAPID VALUE 'PP01',
      W_DATE LIKE SY-DATUM,
      W_DATE_LAST LIKE SY-DATUM.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_MATNR FOR EKBE-MATNR,
            S_DATE FOR SY-DATUM OBLIGATORY. " no-extension.
PARAMETERS: P_SNT AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B1.

AT SELECTION-SCREEN OUTPUT.

START-OF-SELECTION.

  PERFORM GET_DATA.
  IF IT_DATA[] IS INITIAL.
  ELSE.
    PERFORM SEND_DATA.
  ENDIF.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.
  PERFORM GET_MODULE_DATA.
  PERFORM GET_SUBPART_DATA.
ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  write_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEND_DATA.
  DATA: L_RESULT(1),
        L_MSGTXT(100),
        L_YEAR(4) TYPE N,
        L_YEAR_D LIKE SY-DATUM.
  DATA: LT_TEMP LIKE TABLE OF ZTMM_MOD_DECOMP WITH HEADER LINE.

  IF P_SNT IS INITIAL.
  ELSE.
    CALL FUNCTION 'Z_FMM_MODULE_DECOMP'
       DESTINATION C_DEST
       IMPORTING
         FLAG          = L_RESULT
       TABLES
           I_MOD_DECOMP  = IT_DATA
       EXCEPTIONS
              COMMUNICATION_FAILURE = 1 MESSAGE L_MSGTXT
              SYSTEM_FAILURE        = 2 MESSAGE L_MSGTXT.

*  IF SY-SUBRC = 0.
    IF L_RESULT = 'S'.
      MESSAGE I001 WITH 'Data successfully sent out'.
    ELSE.
      L_RESULT = 'E'.
      CONCATENATE '*' L_RESULT '*' L_MSGTXT INTO L_MSGTXT.
      MESSAGE I001 WITH 'Interface Error:' L_MSGTXT.
    ENDIF.
  ENDIF.

  LOOP AT IT_DATA.
    MOVE-CORRESPONDING IT_DATA TO LT_TEMP.
    LT_TEMP-FLAG = L_RESULT.
    LT_TEMP-CRDATE = SY-DATUM.
    LT_TEMP-CRtime = SY-UZEIT.
    APPEND LT_TEMP.
  ENDLOOP.
  MODIFY ZTMM_MOD_DECOMP FROM TABLE LT_TEMP.
  IF SY-SUBRC = 0.
    L_YEAR = SY-DATUM+0(4) - 1.
    CONCATENATE L_YEAR '0101' INTO L_YEAR_D.
    DELETE FROM ZTMM_MOD_DECOMP WHERE CRDATE < L_YEAR_D.
    COMMIT WORK.
    MESSAGE S001 WITH 'DATADBASE TABLE WAS SUCCESSFULLY UPDATED'.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " write_data
*&---------------------------------------------------------------------*
*&      Form  GET_MODULE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_MODULE_DATA.
*  DATA: L_fMONTH like MLCD-POPER,
*        L_tMONTH like MLCD-POPER,
*        L_fYEAR like MLCD-BDATJ,
*        L_tYEAR like MLCD-BDATJ.

  DATA: LT_EKBE1 LIKE TABLE OF IT_EKBE WITH HEADER LINE,
        LT_STB TYPE STPOX OCCURS 0 WITH HEADER LINE,
        L_FIRST(1).

  DATA: BEGIN OF LT_MATNR OCCURS 0,
        MATNR LIKE MARA-MATNR,
        END OF LT_MATNR.

  DATA: L_MENGE LIKE EKBE-MENGE.

  SELECT BUDAT BWART MATNR  GJAHR  BELNR MENGE SHKZG WAERS
     INTO TABLE LT_EKBE1
    FROM EKBE
    WHERE BUDAT IN S_DATE
      AND MATNR IN S_MATNR
      AND VGABE = '1'.

  IF SY-SUBRC <> 0.
    MESSAGE I001 WITH 'No data'.
    EXIT.
  ENDIF.
  SORT LT_EKBE1 BY MATNR BUDAT SHKZG.
  L_FIRST = 'T'.
  LOOP AT LT_EKBE1.
    ON CHANGE OF LT_EKBE1-MATNR OR LT_EKBE1-BUDAT.
      LT_MATNR-MATNR = LT_EKBE1-MATNR.
      COLLECT LT_MATNR.
      IF L_FIRST = 'T'.
        IT_EKBE = LT_EKBE1.
        L_FIRST = 'F'.
      ENDIF.
      IF L_MENGE <> 0.
        APPEND IT_EKBE.
        IT_EKBE-MENGE = L_MENGE.
        IT_EKBE = LT_EKBE1.
        CLEAR: L_MENGE.
      ENDIF.
    ENDON.
    IF LT_EKBE1-SHKZG = 'S'.
      L_MENGE = L_MENGE + LT_EKBE1-MENGE.
    ELSE.
      L_MENGE = L_MENGE - LT_EKBE1-MENGE.
    ENDIF.
  ENDLOOP.
  IF L_MENGE <> 0.
    APPEND IT_EKBE.
    CLEAR: IT_EKBE, L_MENGE.
  ENDIF.

** Explore BOM
  READ TABLE S_DATE INDEX 1.
  CONCATENATE S_DATE-LOW+0(6) '01' INTO W_DATE.

  CALL FUNCTION 'SG_PS_GET_LAST_DAY_OF_MONTH'
       EXPORTING
            DAY_IN            = W_DATE
       IMPORTING
            LAST_DAY_OF_MONTH = W_DATE_LAST
       EXCEPTIONS
            DAY_IN_NOT_VALID  = 1
            OTHERS            = 2.
  IF SY-SUBRC <> 0.
    MESSAGE I001 WITH 'Date error'.
    EXIT.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT LT_MATNR.
    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
         EXPORTING
              CAPID                 = C_CAPID
              DATUV                 = W_DATE_LAST
              MTNRV                 = LT_MATNR-MATNR
              MKTLS                 = 'X'
              STLAL                 = '01'
              STLAN                 = '2'
              WERKS                 = 'P001'
         TABLES
              STB                   = LT_STB
         EXCEPTIONS
              ALT_NOT_FOUND         = 1
              CALL_INVALID          = 2
              MATERIAL_NOT_FOUND    = 3
              MISSING_AUTHORIZATION = 4
              NO_BOM_FOUND          = 5
              NO_PLANT_DATA         = 6
              NO_SUITABLE_BOM_FOUND = 7
              CONVERSION_ERROR      = 8
              OTHERS                = 9.

    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    IT_BOM-MATNR = LT_MATNR-MATNR.
    LOOP AT LT_STB WHERE DATUV <= W_DATE_LAST AND DATUB > W_DATE_LAST.
      IT_BOM-COMP = LT_STB-IDNRK.
      IT_BOM-QNTY = LT_STB-MENGE.
      IT_BOM-DATAB = LT_STB-DATUV.
      IT_BOM-DATBI = LT_STB-DATUB.
*      IT_BOM-CDATAB = LT_STB-DATUV.
*      IT_BOM-CDATBI = LT_STB-DATUB.
      IT_BOM-UPGVC = LT_STB-UPGN.
      IT_BOM-MEINS = LT_STB-MMEIN.
*      SELECT SINGLE MAKTX INTO LT_BOM--CMAKTX
*        FROM MAKT
*        WHERE MATNR = LT_STB-IDNRK.
      APPEND IT_BOM.
    ENDLOOP.

  ENDLOOP.

*  SELECT matnr BDATJ POPER LBKUM INTO TABLE LT_NATN_MODEL_TEMP
*    FROM mlcd as b
*    inner join ckmlhd as a
*    on a~KALNR = a~KALNR
*    where a~BDATJ between l_fyear and l_tyear
*      and a~POPER between l_fmonth and l_tmonth
*      and CATEG = 'ZU'
*      AND PTYP = 'BB'.
*      and b~matnr in s_matnr.

ENDFORM.                    " GET_MODULE_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_SUBPART_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_SUBPART_DATA.
  DATA: BEGIN OF LT_DATA1 OCCURS 0.
          INCLUDE STRUCTURE ZTMM_MOD_DECOMP.
  DATA: ASSY_FULL LIKE MARA-MATNR,
        END OF LT_DATA1.

  DATA: LT_DATA2 LIKE TABLE OF IT_DATA WITH HEADER LINE.
  DATA: L_MENGE LIKE EKBE-MENGE,
        L_FIRST(1).

  SORT IT_BOM BY MATNR.
  LOOP AT IT_EKBE.
    LT_DATA1-RCPT_D = IT_EKBE-BUDAT.
    LT_DATA1-ASSY_NO = IT_EKBE-MATNR+0(5).
    LT_DATA1-ASSY_FULL = IT_EKBE-MATNR.
    LOOP AT IT_BOM WHERE MATNR = IT_EKBE-MATNR.
      LT_DATA1-PART_N = IT_BOM-COMP.
      LT_DATA1-UNIT = IT_BOM-MEINS.
      LT_DATA1-RCPT_Q = IT_EKBE-MENGE * IT_BOM-QNTY.
      APPEND LT_DATA1.
    ENDLOOP.
  ENDLOOP.

  SORT LT_DATA1 BY ASSY_NO PART_N RCPT_D.

  CLEAR: L_MENGE.
  L_FIRST = 'T'.
  LOOP AT LT_DATA1.
    ON CHANGE OF LT_DATA1-ASSY_NO OR
                 LT_DATA1-PART_N OR
                 LT_DATA1-RCPT_D.
      IF L_FIRST = 'T'.
        MOVE-CORRESPONDING LT_DATA1 TO LT_DATA2.
        L_FIRST = 'F'.
      ENDIF.
      IF L_MENGE <> 0.
        LT_DATA2-RCPT_Q = L_MENGE.
        APPEND LT_DATA2.
        MOVE-CORRESPONDING LT_DATA1 TO LT_DATA2.
        CLEAR: L_MENGE.
      ENDIF.
*      lt_comp-matnr = LT_DATA1-PART_N.
*      collect LT_comp.
    ENDON.
    L_MENGE = L_MENGE + LT_DATA1-RCPT_Q.
  ENDLOOP.
  IF L_MENGE <> 0.
    LT_DATA2-RCPT_Q = L_MENGE.
    APPEND LT_DATA2.
    CLEAR: L_MENGE.
  ENDIF.

  LOOP AT LT_DATA2.
    IT_DATA = LT_DATA2.
    CLEAR: WA_SUB.
    PERFORM READ_INFO_RECORD.
    IT_DATA-RCPT_PR = WA_SUB-NETPR.
    IT_DATA-RCPT_AMT = LT_DATA2-RCPT_Q * WA_SUB-NETPR / WA_SUB-PEINH.
    IT_DATA-VEND_C = WA_SUB-LIFNR.
    IT_DATA-CORP = 'A'.
    IT_DATA-PLNT = '  '.
    IT_DATA-CURRENCY = WA_SUB-WAERS.
    IT_DATA-KV = 'V'.
    IT_DATA-LV = 'V'.
    APPEND IT_DATA.
    CLEAR: IT_DATA.
  ENDLOOP.
ENDFORM.                    " GET_SUBPART_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_INFO_RECORD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_INFO_RECORD.
  DATA: LW_SUB(50).
*  FIELD-SYMBOLS: <LW_SUB>.
  CLEAR: IT_SUB.

  READ TABLE IT_SUB WITH KEY COMP = IT_DATA-PART_N.
  IF SY-SUBRC NE 0.
    IT_SUB-MATNR = IT_DATA-ASSY_NO.
    IT_SUB-COMP = IT_DATA-PART_N.
*    MOVE-CORRESPONDING  IT_data TO IT_SUB.
    PERFORM CHECK_RTN.
    IT_SUB-MATNR = IT_DATA-PART_N.
    MOVE: WA_SUB TO IT_SUB.
*  MOVE-CORRESPONDING IT_OUT TO IT_SUB.
*  IT_SUB-MAKTX = IT_OUT-CMAKTX.
*  it_sub-datab = it_output-cdatab.
*  it_sub-datbi = it_output-cdatbi.
    IT_SUB-LIFNR  = WA_SUB-LIFNR.
    IT_SUB-DATAB = WA_SUB-DATAB.
    IT_SUB-DATBI = WA_SUB-DATBI.

*  IF IT_SUB-DATAB IS INITIAL.
*    IT_SUB-DATAB = IT_OUT-CDATAB.
*  ENDIF.
*
*  IF IT_SUB-DATBI IS INITIAL.
*    IT_SUB-DATBI = IT_OUT-CDATBI.
*  ENDIF.

    IF IT_SUB-PEINH EQ 0.
      IT_SUB-PEINH = 1.
    ENDIF.

    IT_SUB-AMOUNT = IT_SUB-QNTY * IT_SUB-NETPR / IT_SUB-PEINH.

    APPEND IT_SUB.
    CLEAR: IT_SUB.

  ELSE.
    MOVE: IT_SUB-LIFNR  TO WA_SUB-LIFNR,
          IT_SUB-AMOUNT TO WA_SUB-AMOUNT,
          IT_SUB-KMEIN  TO WA_SUB-KMEIN,
          IT_SUB-NETPR  TO WA_SUB-NETPR,
          IT_SUB-PEINH  TO WA_SUB-PEINH,
          IT_SUB-WAERS  TO WA_SUB-WAERS,
          IT_SUB-KZUST  TO WA_SUB-KZUST,
*          IT_SUB-STS    TO WA_SUB-STS,
*          IT_SUB-MSG    TO WA_SUB-MSG,
*          IT_SUB-ZTIR   TO WA_SUB-ZTIR,
          IT_SUB-DATAB  TO WA_SUB-DATAB,
          IT_SUB-DATBI  TO WA_SUB-DATBI.
  ENDIF.



*    PERFORM APPEND_SUB_PRICE.

ENDFORM.                    " READ_INFO_RECORD
*&---------------------------------------------------------------------*
*&      Form  CHECK_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_RTN.
  DATA: BEGIN OF LT_CONDITION OCCURS 0,
          KBETR LIKE ZVMM_INFO_CONDI-KBETR,
          KPEIN LIKE ZVMM_INFO_CONDI-KPEIN,
          KMEIN LIKE ZVMM_INFO_CONDI-KMEIN,
          KSCHL LIKE ZVMM_INFO_CONDI-KSCHL,
          KZUST LIKE ZVMM_INFO_CONDI-KZUST,
        END   OF LT_CONDITION.

  DATA: BEGIN OF IT_EINA_EINE_TEMP OCCURS 0,
        MATNR LIKE EINA-MATNR,
        LIFNR LIKE EINA-LIFNR,
        WGLIF LIKE EINA-WGLIF,
        WAERS LIKE EINE-WAERS,
        END OF IT_EINA_EINE_TEMP.
  DATA:  C_KSCHL   LIKE   KONP-KSCHL    VALUE   'PB00'.
*  IF    IT_OUT-CMAKTX IS INITIAL.
*
*    MOVE: C_NO_MATL TO WA_SUB-STS.
*    EXIT.
*  ENDIF.

  SELECT MATNR A~LIFNR WGLIF WAERS INTO TABLE IT_EINA_EINE_TEMP
   FROM EINA AS A INNER JOIN EINE AS B
     ON A~INFNR = B~INFNR
  WHERE A~MATNR = IT_DATA-PART_N
    AND A~URZZT = 'SUB'
    AND A~LOEKZ = ' '
    AND B~WERKS = ' '
*    AND B~EKORG = C_EKORG
    AND B~LOEKZ = ' '.
  IF SY-SUBRC NE 0.
*    MOVE: C_NO_INFO TO WA_SUB-STS.
    EXIT.
  ENDIF.

*----- Read submaterial price
  LOOP AT IT_EINA_EINE_TEMP.
    CLEAR: EINA, A018.
    WA_SUB-LIFNR = IT_EINA_EINE_TEMP-LIFNR.
    WA_SUB-WAERS = IT_EINA_EINE_TEMP-WAERS.
    SELECT SINGLE KNUMH DATAB DATBI
                    INTO (A018-KNUMH, WA_SUB-DATAB, WA_SUB-DATBI)
                    FROM A018
                    WHERE KAPPL =  'M'
                      AND KSCHL =  'PB00'
                      AND MATNR =  IT_DATA-PART_N
                      AND LIFNR =  WA_SUB-LIFNR
*                      AND EKORG =  C_EKORG
                      AND ESOKZ =  '0'
*                      AND DATAB <= L_P_DATE
                      AND DATBI >= W_DATE_LAST.
    IF SY-SUBRC EQ 0.
      EXIT.
    ENDIF.
  ENDLOOP.
*  if sy-subrc ne 0.
  IF A018-KNUMH IS INITIAL.
*    MOVE: C_NO_COND TO WA_SUB-STS.
    EXIT.
  ENDIF.
*** END OF INSERTION

  SELECT SINGLE KBETR KPEIN KMEIN KZUST
     INTO (WA_SUB-NETPR, WA_SUB-PEINH, WA_SUB-KMEIN, WA_SUB-KZUST)
     FROM ZVMM_INFO_CONDI
    WHERE KNUMH = A018-KNUMH
      AND KSCHL = C_KSCHL
      AND LOEVM_KO = ' '.

  IF SY-SUBRC NE 0.
*    MOVE: C_NO_COND TO WA_SUB-STS.
    EXIT.
  ENDIF.

*  SELECT KBETR KPEIN KMEIN KZUST KSCHL
*    INTO CORRESPONDING FIELDS OF TABLE LT_CONDITION
*    FROM ZVMM_INFO_CONDI
*   WHERE KNUMH = A018-KNUMH
*     AND ( KSCHL = C_KSCHL   OR
*           KSCHL = 'ZTIR'    OR
*           KSCHL LIKE 'ZP%' )
*     AND LOEVM_KO = ' '.
*  IF SY-SUBRC NE 0.
*    MOVE: C_NO_COND TO WA_SUB-STS.
*    EXIT.
*  ENDIF.

*  SORT LT_CONDITION BY KSCHL.
*  LOOP AT LT_CONDITION.
*    CASE LT_CONDITION-KSCHL.
*      WHEN C_KSCHL.
*        MOVE: LT_CONDITION-KBETR TO WA_SUB-NETPR,
*              LT_CONDITION-KPEIN TO WA_SUB-PEINH,
*              LT_CONDITION-KMEIN TO WA_SUB-KMEIN,
*              LT_CONDITION-KZUST TO WA_SUB-KZUST.
*        IF IT_EINA_EINE_TEMP-WGLIF = 'ZTIR'.
*          WA_SUB-ZTIR = LT_CONDITION-KBETR.
*        ENDIF.
*
*      WHEN OTHERS.
*        MOVE: LT_CONDITION-KSCHL+2(2) TO W_INDEX.
*
*        CONCATENATE: 'WA_SUB-ZP' W_INDEX INTO W_SUB.
*
*        ASSIGN: (W_SUB) TO <SUB>.
*        IF SY-SUBRC NE 0. CONTINUE. ENDIF.
*
*        <SUB> = LT_CONDITION-KBETR.
*    ENDCASE.
*  ENDLOOP.

*----- A sub material's UoM must be 'EA'.
*----- If UoM is not 'EA', display error message.
*  IF NOT ( ( IT_SUB-MEINS EQ WA_SUB-KMEIN AND
*             IT_SUB-MEINS EQ IT_SUB-UNIT  AND
*             WA_SUB-KMEIN EQ IT_SUB-UNIT )   ).
*    MOVE: C_UOM_ERR TO WA_SUB-STS.
*  ENDIF.

ENDFORM.                    " CHECK_RTN

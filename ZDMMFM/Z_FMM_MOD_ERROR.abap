FUNCTION Z_FMM_MOD_ERROR.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(P_MATNR) TYPE  MARA-MATNR
*"     VALUE(P_INDEX) TYPE  SY-TABIX
*"     VALUE(P_LIFNR) TYPE  EINA-LIFNR
*"     VALUE(P_WERKS) TYPE  MAST-WERKS
*"     VALUE(P_DATE) TYPE  SY-DATUM
*"     VALUE(P_PRICE) TYPE  KONP-KBETR
*"     VALUE(P_CLASS) TYPE  CHAR3
*"     VALUE(P_FDATE) TYPE  SY-DATUM
*"     VALUE(P_TDATE) TYPE  SY-DATUM
*"  EXPORTING
*"     VALUE(PE_REMARK) TYPE  CHAR50
*"     VALUE(PE_INDEX) TYPE  SY-TABIX
*"     VALUE(PE_GRQTY) LIKE  EKBE-MENGE
*"     VALUE(PE_MATNR) LIKE  MARA-MATNR
*"  TABLES
*"      PT_COST1 STRUCTURE  ZTMM_ASSY_COST1
*"----------------------------------------------------------------------
  TABLES: EINA, A018.
  DATA : BEGIN OF LT_EKBE OCCURS 0,
           EBELN LIKE EKBE-EBELN,
           EBELP LIKE EKBE-EBELP,
           GJAHR LIKE EKBE-GJAHR,
           BELNR LIKE EKBE-BELNR,
           LFBNR LIKE EKBE-LFBNR,
           BWART LIKE EKBE-BWART,
           SHKZG LIKE EKBE-SHKZG,
           MENGE LIKE EKBE-MENGE,
           END OF LT_EKBE.

  DATA: LT_EKBE_INV LIKE TABLE OF LT_EKBE WITH HEADER LINE.

  DATA: L_VTYPE   LIKE   ZTMM_ASSY_COST1-VTYPE,
        L_MCODE   LIKE   ZTMM_ASSY_COST1-MCODE.

  DATA : BEGIN OF LT_TAB OCCURS 0,
        VTYPE  LIKE   ZTMM_ASSY_COST1-VTYPE,
        LIFNR  LIKE   LFA1-LIFNR,
        MATNR  LIKE   MARA-MATNR,
        ASYTR  LIKE MSEG-DMBTR,
        DATAB LIKE   ZTMM_ASSY_COST1-DATAB,
        DATBI LIKE   ZTMM_ASSY_COST1-DATBI,
        MAKTX  LIKE   MAKT-MAKTX,
        NAME1 LIKE   LFA1-NAME1,
        UPGVC LIKE MARA-MATNR,
        PREF LIKE STPO-POSNR, "ztbm_abxduldt-pref,
        COMP  LIKE MARA-MATNR,
        MAKTX1 LIKE  MAKT-MAKTX ,
        QNTY LIKE STPO-MENGE,
        UNIT LIKE   MARA-MEINS,
        MEINS LIKE   MARA-MEINS,
        EKGRP LIKE   EKKO-EKGRP,
        MCODE LIKE ZTMM_ASSY_COST1-MCODE,
        DATAB1 LIKE   ZTMM_ASSY_COST1-DATAB ,
        DATBI1 LIKE   ZTMM_ASSY_COST1-DATBI ,
        STGB LIKE STPO-STGB,
       END OF LT_TAB.

  DATA: BEGIN OF IT_SUB OCCURS 0,
          VTYPE     LIKE   ZTMM_ASSY_COST1-VTYPE,   "Vehicle type
          MATNR     LIKE   ZTMM_ASSY_COST1-VTYPE,   "Material
          UPGVC     LIKE   MARA-MATNR,              "UPG-VC
          PREF      LIKE   STPO-POSNR,
          COMP      LIKE   MARA-MATNR,
          MAKTX     LIKE   MAKT-MAKTX,              "Description
          LIFNR     LIKE   LFA1-LIFNR,              "Vendor
          AMOUNT    TYPE   F,                       "Component Amount
          QNTY      LIKE   STPO-MENGE,
          STGB      LIKE   STPO-STGB,
          UNIT      LIKE   MARA-MEINS,
          MEINS     LIKE   MARA-MEINS,              "UoM(sub)
          KMEIN     LIKE   KONP-KMEIN,              "UoM(Info)
          DATAB     LIKE   SY-DATUM,                "Valid on(sub)
          DATBI     LIKE   SY-DATUM,                "Valid to(Sub)
          NETPR     LIKE   EKPO-NETPR,              "Component Amount
         PEINH     LIKE   EKPO-PEINH,              "Component Price Unit
          WAERS     LIKE   T001-WAERS,              "Currency
          KZUST     LIKE   KONH-KZUST.              "Reason code
          INCLUDE STRUCTURE ZSMM_CUSTOM_CONDITION.
  DATA:   ZTIR      LIKE   EKPO-NETPR,
          STS,                                      "Status
          MSG(100),                                  "Message
        END   OF IT_SUB.

  DATA: BEGIN OF IT_MODULE OCCURS 0,
          INDICATOR,                              "Sub Material Status
          VTYPE     LIKE   ZTMM_ASSY_COST1-VTYPE, "Vehicle type
          NAME1     LIKE   LFA1-NAME1,            "Vendor name
          LIFNR     LIKE   LFA1-LIFNR,            "Vendor
          MAKTX     LIKE   MAKT-MAKTX,            "Description
          MATNR     LIKE   MARA-MATNR,            "Material
          DATAB     LIKE   ZTMM_ASSY_COST1-DATAB, "Valid on
          DATBI     LIKE   ZTMM_ASSY_COST1-DATBI, "Valid to
          MOAMT     LIKE   MSEG-DMBTR,            "Module Price
          ASYTR     LIKE   MSEG-DMBTR,            "Module Assy Cost
          DMBTR     LIKE   MSEG-DMBTR,            "Material Cost
        DMAMT     TYPE   F,                     "Material Cost(Floating)
          WAERS     LIKE   T001-WAERS,            "Currency
          EKGRP     LIKE   EKKO-EKGRP,            "Purchasing Group
          NETPR     LIKE   EKPO-NETPR,            "Component Amount
          PEINH     LIKE   EKPO-PEINH,            "Component Price Unit
          MEINS     LIKE   EKPO-MEINS,            "UoM
          MCODE     LIKE   ZTMM_ASSY_COST1-MCODE, "Module code
          KZUST     LIKE   KONH-KZUST.            "Reason code
          INCLUDE STRUCTURE ZSMM_CUSTOM_CONDITION.
  DATA:   ZTIR     LIKE   EKPO-NETPR,
          STS       TYPE   I,                     "Module Info Status
          MSG(100),
          CHBOX,
        END   OF IT_MODULE.

  DATA : BEGIN OF IT_OUT OCCURS 0,
              VTYPE LIKE ZTMM_ASSY_COST1-VTYPE,
              LIFNR LIKE LFA1-LIFNR,
              MATNR LIKE MARA-MATNR,
              ASYTR LIKE MSEG-DMBTR,
              DATAB LIKE ZTMM_ASSY_COST1-DATAB,
              DATBI LIKE ZTMM_ASSY_COST1-DATBI,
              MAKTX LIKE MAKT-MAKTX,
              NAME1 LIKE LFA1-NAME1,
              UPGVC LIKE MARA-MATNR,
              PREF  LIKE STPO-POSNR,  "ZTBM_ABXDULDT-pref
              COMP  LIKE MARA-MATNR,
              CMAKTX LIKE MAKT-MAKTX,
              QNTY LIKE STPO-MENGE,
              UNIT LIKE MARA-MEINS,
              MEINS     LIKE   MARA-MEINS,
              EKGRP LIKE ZTMM_ASSY_COST1-EKGRP,
              MCODE LIKE ZTMM_ASSY_COST1-MCODE,
              CDATAB LIKE ZTMM_ASSY_COST1-DATAB,
              CDATBI LIKE ZTMM_ASSY_COST1-DATBI,
              STGB   LIKE STPO-STGB,     "******
          END OF IT_OUT.
  DATA: BEGIN OF LT_CONDITION OCCURS 0,
            KBETR LIKE ZVMM_INFO_CONDI-KBETR,
            KPEIN LIKE ZVMM_INFO_CONDI-KPEIN,
            KMEIN LIKE ZVMM_INFO_CONDI-KMEIN,
            KSCHL LIKE ZVMM_INFO_CONDI-KSCHL,
            KZUST LIKE ZVMM_INFO_CONDI-KZUST,
          END   OF LT_CONDITION.

  DATA : BEGIN OF IT_INFO_ITEM OCCURS 0,
           KAPPL LIKE KONP-KAPPL,
           KSCHL LIKE KONP-KSCHL,
           KBETR LIKE KONP-KBETR,
           KPEIN LIKE KONP-KPEIN,
           KONWA LIKE KONP-KONWA,
           LIFNR LIKE KONP-LIFNR,
           ERDAT LIKE KONH-ERDAT,
           ERNAM LIKE KONH-ERNAM,
           KZUST LIKE KONH-KZUST,
           DATAB LIKE KONH-DATAB,
           DATBI LIKE KONH-DATBI,
         END OF IT_INFO_ITEM.

  DATA: LT_A018 LIKE TABLE OF A018 WITH HEADER LINE.
  DATA: LT_A017 LIKE TABLE OF A017 WITH HEADER LINE.


  DATA: LT_STB TYPE STPOX OCCURS 0 WITH HEADER LINE.
  DATA: W_INDEX(2)   TYPE   N,
        W_SUB(50),
        W_MODULE(50).


  DATA: LW_SUB(50).
  DATA: WA_MODULE LIKE IT_MODULE.
  DATA: WA_SUB LIKE IT_SUB.
  DATA: L_ABN_PRICE LIKE WA_SUB-NETPR,
        L_BELNR LIKE EKBE-BELNR.

  DATA: LW_AMOUNT TYPE F,
        LW_MTOT LIKE IT_MODULE-MOAMT.

  DATA: LT_RBKP_BLOCKED LIKE TABLE OF RBKP_BLOCKED WITH HEADER LINE.

  DATA: L_INDEX LIKE SY-TABIX.
  DATA : W_KNUMH LIKE KONH-KNUMH.

  DATA: BEGIN OF IT_EINA_EINE_TEMP OCCURS 0,
        MATNR LIKE EINA-MATNR,
        LIFNR LIKE EINA-LIFNR,
        WGLIF LIKE EINA-WGLIF,
        END OF IT_EINA_EINE_TEMP.

  FIELD-SYMBOLS: <LW_SUB>, <MODULE>, <SUB>..

  CLEAR: PE_REMARK, PE_GRQTY, PE_INDEX.

  PE_MATNR = P_MATNR.

  IF P_CLASS = 'Mod'.

    L_VTYPE = P_MATNR+0(3).
    L_MCODE = P_MATNR+3(2).

    READ TABLE PT_COST1 WITH KEY VTYPE = L_VTYPE
                                 MCODE = L_MCODE
                                 LIFNR = P_LIFNR.

    SELECT SINGLE G~MATNR AS MATNR
              D~MAKTX
              M~MEINS AS UNIT M~MEINS AS MEINS
              INTO CORRESPONDING FIELDS OF LT_TAB
                 FROM MAST AS G
                 INNER JOIN MARA AS M
                 ON G~MATNR = M~MATNR
                 INNER JOIN MAKT AS D
                 ON M~MATNR = D~MATNR
                WHERE G~WERKS = P_WERKS
                  AND G~STLAN = '2'
                  AND G~STLAL = '01'
                  AND G~MATNR = P_MATNR
                  AND M~LVORM = ' '
                  AND D~SPRAS = SY-LANGU.

*      IT_TAB-NAME1 = L_NAME1.
    LT_TAB-LIFNR = PT_COST1-LIFNR.
    LT_TAB-VTYPE = PT_COST1-VTYPE.
    LT_TAB-EKGRP = PT_COST1-EKGRP.
    LT_TAB-ASYTR = PT_COST1-ASYTR.
    LT_TAB-DATAB = PT_COST1-DATAB.
    LT_TAB-DATBI = PT_COST1-DATBI.

    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
       EXPORTING
        CAPID                       = 'PP01'  "C_CAPID
        DATUV                       = P_DATE
*    EMENG                       = p_emeng
*    MEHRS                       = p_mehrs
*    MMORY                       = p_mmory
        MTNRV                       = P_MATNR
        MKTLS                       = 'X'
        STLAL                       = '01'
        STLAN                       = '2'
*   STPST                       = 0
*   SVWVO                       = 'X'
        WERKS                       = P_WERKS
* IMPORTING
*    TOPMAT                     =
*   DSTST                       =
        TABLES
          STB                       = LT_STB
*   MATCAT                      =
     EXCEPTIONS
       ALT_NOT_FOUND               = 1
       CALL_INVALID                = 2
       MATERIAL_NOT_FOUND          = 3
       MISSING_AUTHORIZATION       = 4
       NO_BOM_FOUND                = 5
       NO_PLANT_DATA               = 6
       NO_SUITABLE_BOM_FOUND       = 7
       CONVERSION_ERROR            = 8
       OTHERS                      = 9 .

    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    MOVE-CORRESPONDING LT_TAB TO IT_OUT.
    LOOP AT LT_STB WHERE DATUV <= P_DATE AND DATUB > P_DATE.
      IT_OUT-COMP = LT_STB-IDNRK.
      IT_OUT-QNTY = LT_STB-MENGE.
      IT_OUT-DATAB = LT_STB-DATUV.
      IT_OUT-DATBI = LT_STB-DATUB.
      IT_OUT-CDATAB = LT_STB-DATUV.
      IT_OUT-CDATBI = LT_STB-DATUB.
      IT_OUT-UPGVC = LT_STB-UPGN.
*      SELECT SINGLE MAKTX INTO IT_OUT-CMAKTX
*        FROM MAKT
*        WHERE MATNR = LT_STB-IDNRK.
      APPEND IT_OUT.
    ENDLOOP.


    LOOP AT IT_OUT.

      ON CHANGE OF IT_OUT-MATNR OR IT_OUT-LIFNR.
        CLEAR: EINA, A018.
        CLEAR: WA_MODULE-NETPR, WA_MODULE-PEINH, WA_MODULE-MEINS,
               WA_MODULE-MSG,  WA_MODULE-STS.

*----- Check Material Master
        IF IT_OUT-MAKTX IS INITIAL.
*    MOVE: C_NO_MATL TO WA_MODULE-STS.
          EXIT.
        ENDIF.

        SELECT SINGLE MATNR A~LOEKZ WGLIF
          INTO (EINA-MATNR,EINA-LOEKZ,EINA-WGLIF)
          FROM EINA AS A INNER JOIN EINE AS B
            ON A~INFNR = B~INFNR
         WHERE A~MATNR = IT_OUT-MATNR
           AND A~LIFNR = IT_OUT-LIFNR
           AND A~LOEKZ = ' '
           AND B~WERKS = ' '
*     AND B~EKORG = C_EKORG
           AND B~LOEKZ = ' '.
        IF SY-SUBRC = 0.
*        IF SY-SUBRC NE 0.
*    MOVE: C_NEW    TO WA_MODULE-STS.
*          EXIT.
*        ENDIF.
          IF EINA-LOEKZ <> 'X'.

*        IF EINA-LOEKZ EQ 'X'.
*    MOVE: C_DELETED TO WA_MODULE-STS.
*          EXIT.
*        ENDIF.

*----- Read Module price
            SELECT SINGLE *
              FROM A018
             WHERE KAPPL =  'M'
               AND KSCHL =  'PB00'
               AND MATNR =  IT_OUT-MATNR
               AND LIFNR =  IT_OUT-LIFNR
*     AND EKORG =  C_EKORG
               AND ESOKZ =  '0'
               AND DATAB <= P_DATE
               AND DATBI >= P_DATE.
*        IF SY-SUBRC NE 0.
*    MOVE: C_NO_COND TO WA_MODULE-STS.
*          EXIT.
*        ELSE.
*    MOVE: C_EXIST   TO WA_MODULE-STS.
*        ENDIF.
            IF SY-SUBRC = 0.
              SELECT KBETR KPEIN KMEIN KZUST KSCHL
               INTO CORRESPONDING FIELDS OF TABLE LT_CONDITION
               FROM ZVMM_INFO_CONDI
              WHERE KNUMH = A018-KNUMH
                AND ( KSCHL = 'PB00'   OR
                      KSCHL = 'ZTIR'    OR
                      KSCHL LIKE 'ZP%' )
                AND LOEVM_KO = ' '.
*        IF SY-SUBRC NE 0.
*    MOVE: C_DEL_CON TO WA_MODULE-STS.
*          EXIT.
*        ENDIF.
              IF SY-SUBRC = 0.
                SORT LT_CONDITION BY KSCHL.
                LOOP AT LT_CONDITION.
                  CASE LT_CONDITION-KSCHL.
                    WHEN 'PB00'.
                      MOVE: LT_CONDITION-KBETR TO WA_MODULE-NETPR,
                            LT_CONDITION-KPEIN TO WA_MODULE-PEINH,
                            LT_CONDITION-KMEIN TO WA_MODULE-MEINS,
                            LT_CONDITION-KZUST TO WA_MODULE-KZUST.
                    WHEN 'ZTIR'.
                      LW_MTOT = WA_MODULE-NETPR + LT_CONDITION-KBETR.
                      MOVE: LW_MTOT TO WA_MODULE-NETPR.
                    WHEN OTHERS.
                      MOVE: LT_CONDITION-KSCHL+2(2) TO W_INDEX.

                      CONCATENATE: 'WA_MODULE-ZP' W_INDEX INTO W_MODULE.

                      ASSIGN: (W_MODULE) TO <MODULE>.
                      IF SY-SUBRC NE 0. CONTINUE. ENDIF.

                      <MODULE> = LT_CONDITION-KBETR.
                  ENDCASE.
                ENDLOOP.

                CLEAR: IT_MODULE.

                IF IT_MODULE-PEINH EQ 0.
                  IT_MODULE-PEINH = 1.
                ENDIF.

*  MOVE: T001-WAERS TO WA_MODULE-WAERS.

                MOVE: WA_MODULE TO IT_MODULE.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
        MOVE-CORRESPONDING  IT_OUT TO IT_MODULE.

        APPEND IT_MODULE.
        CLEAR: IT_MODULE.


      ENDON.

      READ TABLE IT_SUB WITH KEY COMP = IT_OUT-COMP.
      IF SY-SUBRC NE 0.
        MOVE-CORRESPONDING  IT_OUT TO IT_SUB.

        SELECT MATNR A~LIFNR WGLIF INTO TABLE IT_EINA_EINE_TEMP
               FROM EINA AS A INNER JOIN EINE AS B
               ON A~INFNR = B~INFNR
               WHERE A~MATNR = IT_OUT-COMP
                 AND A~URZZT = 'SUB'
                 AND A~LOEKZ = ' '
                 AND B~WERKS = ' '
                 AND B~EKORG = 'PU01'
                 AND B~LOEKZ = ' '.
*  IF SY-SUBRC NE 0.
*    MOVE: C_NO_INFO TO WA_SUB-STS.
*    EXIT.
*  ENDIF.
        IF SY-SUBRC = 0.
          LOOP AT IT_EINA_EINE_TEMP.
            CLEAR: EINA, A018.
            WA_SUB-LIFNR = IT_EINA_EINE_TEMP-LIFNR.
            SELECT SINGLE KNUMH DATAB DATBI
                          INTO (A018-KNUMH, WA_SUB-DATAB, WA_SUB-DATBI)
                            FROM A018
                            WHERE KAPPL =  'M'
                              AND KSCHL =  'PB00'
                              AND MATNR =  IT_OUT-COMP
                              AND LIFNR =  WA_SUB-LIFNR
                              AND EKORG =  'PU01'
                              AND ESOKZ =  '0'
                              AND DATAB <= P_DATE
                              AND DATBI >= P_DATE.
            IF SY-SUBRC EQ 0.
              EXIT.
            ENDIF.
          ENDLOOP.

          SELECT KBETR KPEIN KMEIN KZUST KSCHL
            INTO CORRESPONDING FIELDS OF TABLE LT_CONDITION
            FROM ZVMM_INFO_CONDI
           WHERE KNUMH = A018-KNUMH
             AND ( KSCHL = 'PB00'   OR
                   KSCHL = 'ZTIR'    OR
                   KSCHL LIKE 'ZP%' )
             AND LOEVM_KO = ' '.
*        IF SY-SUBRC NE 0.
*    MOVE: C_NO_COND TO WA_SUB-STS.
*          EXIT.
*        ENDIF.
          IF SY-SUBRC = 0.
            SORT LT_CONDITION BY KSCHL.
            LOOP AT LT_CONDITION.
              CASE LT_CONDITION-KSCHL.
                WHEN 'PB00'.
                  MOVE: LT_CONDITION-KBETR TO WA_SUB-NETPR,
                        LT_CONDITION-KPEIN TO WA_SUB-PEINH,
                        LT_CONDITION-KMEIN TO WA_SUB-KMEIN,
                        LT_CONDITION-KZUST TO WA_SUB-KZUST.
*        IF IT_EINA_EINE_TEMP-WGLIF = 'ZTIR'.
*          WA_SUB-ZTIR = LT_CONDITION-KBETR.
*        ENDIF.

                WHEN OTHERS.
                  MOVE: LT_CONDITION-KSCHL+2(2) TO W_INDEX.

                  CONCATENATE: 'WA_SUB-ZP' W_INDEX INTO W_SUB.

                  ASSIGN: (W_SUB) TO <SUB>.
                  IF SY-SUBRC NE 0. CONTINUE. ENDIF.

                  <SUB> = LT_CONDITION-KBETR.
              ENDCASE.
            ENDLOOP.

*    PERFORM CHECK_RTN.
            MOVE: WA_MODULE-VTYPE TO WA_SUB-VTYPE,
                   WA_MODULE-MATNR TO WA_SUB-MATNR.

            MOVE: WA_SUB TO IT_SUB.
            MOVE-CORRESPONDING IT_OUT TO IT_SUB.
            IT_SUB-MAKTX = IT_OUT-CMAKTX.
*  it_sub-datab = it_output-cdatab.
*  it_sub-datbi = it_output-cdatbi.
            IT_SUB-LIFNR  = WA_SUB-LIFNR.
            IT_SUB-DATAB = WA_SUB-DATAB.
            IT_SUB-DATBI = WA_SUB-DATBI.

            IF IT_SUB-DATAB IS INITIAL.
              IT_SUB-DATAB = IT_OUT-CDATAB.
            ENDIF.

            IF IT_SUB-DATBI IS INITIAL.
              IT_SUB-DATBI = IT_OUT-CDATBI.
            ENDIF.

            IF IT_SUB-PEINH EQ 0.
              IT_SUB-PEINH = 1.
            ENDIF.

            IT_SUB-AMOUNT = IT_SUB-QNTY * IT_SUB-NETPR / IT_SUB-PEINH.
          ENDIF.
        ENDIF.
        APPEND IT_SUB.
        CLEAR: IT_SUB, WA_SUB, A018.

      ELSE.
        MOVE: IT_SUB-LIFNR  TO WA_SUB-LIFNR,
              IT_SUB-AMOUNT TO WA_SUB-AMOUNT,
              IT_SUB-KMEIN  TO WA_SUB-KMEIN,
              IT_SUB-NETPR  TO WA_SUB-NETPR,
              IT_SUB-PEINH  TO WA_SUB-PEINH,
              IT_SUB-WAERS  TO WA_SUB-WAERS,
              IT_SUB-KZUST  TO WA_SUB-KZUST,
              IT_SUB-STS    TO WA_SUB-STS,
              IT_SUB-MSG    TO WA_SUB-MSG,
              IT_SUB-ZTIR   TO WA_SUB-ZTIR,
              IT_SUB-DATAB  TO WA_SUB-DATAB,
              IT_SUB-DATBI  TO WA_SUB-DATBI.
        DO.
          MOVE: SY-INDEX TO W_INDEX.

          CONCATENATE: 'WA_SUB-ZP' W_INDEX INTO LW_SUB,
                       'IT_SUB-ZP' W_INDEX INTO W_SUB.

          ASSIGN: (W_SUB)  TO <SUB>,
                  (LW_SUB) TO <LW_SUB>.
          IF SY-SUBRC NE 0. EXIT. ENDIF.

          MOVE: <SUB> TO <LW_SUB>.
        ENDDO.

*    IF NOT ( IT_SUB-STS EQ C_NO_MATL OR
*             IT_SUB-STS EQ C_NO_COND OR
*             IT_SUB-STS EQ C_NO_INFO    ).
*      IF NOT ( ( IT_SUB-MEINS EQ WA_SUB-KMEIN AND
*                 IT_SUB-MEINS EQ IT_SUB-UNIT  AND
*                 WA_SUB-KMEIN EQ IT_SUB-UNIT )   ).
*        MOVE: C_UOM_ERR TO WA_SUB-STS.
*      ENDIF.
*    ENDIF.

        MOVE: WA_MODULE-VTYPE TO WA_SUB-VTYPE,
            WA_MODULE-MATNR TO WA_SUB-MATNR.

        MOVE: WA_SUB TO IT_SUB.
        MOVE-CORRESPONDING IT_OUT TO IT_SUB.
        IT_SUB-MAKTX = IT_OUT-CMAKTX.
        IT_SUB-LIFNR  = WA_SUB-LIFNR.
        IT_SUB-DATAB = WA_SUB-DATAB.
        IT_SUB-DATBI = WA_SUB-DATBI.

        IF IT_SUB-DATAB IS INITIAL.
          IT_SUB-DATAB = IT_OUT-CDATAB.
        ENDIF.

        IF IT_SUB-DATBI IS INITIAL.
          IT_SUB-DATBI = IT_OUT-CDATBI.
        ENDIF.

        IF IT_SUB-PEINH EQ 0.
          IT_SUB-PEINH = 1.
        ENDIF.

        IT_SUB-AMOUNT = IT_SUB-QNTY * IT_SUB-NETPR / IT_SUB-PEINH.

        APPEND IT_SUB.
        CLEAR: IT_SUB, WA_SUB.
      ENDIF.
    ENDLOOP.

** Calculate module price
    SORT IT_MODULE BY VTYPE MATNR LIFNR.
    SORT IT_SUB BY VTYPE MATNR UPGVC PREF.

    LOOP AT IT_MODULE.
      READ TABLE IT_SUB WITH KEY VTYPE = IT_MODULE-VTYPE
                                 MATNR = IT_MODULE-MATNR.
      IF SY-SUBRC = 0.
        L_INDEX = SY-TABIX.
        LOOP AT IT_SUB FROM L_INDEX WHERE VTYPE = IT_MODULE-VTYPE
                         AND MATNR = IT_MODULE-MATNR.

          IT_MODULE-DMAMT = IT_MODULE-DMAMT + IT_SUB-AMOUNT.
          IT_MODULE-ZTIR = IT_MODULE-ZTIR + IT_SUB-ZTIR
                           / IT_SUB-PEINH * IT_SUB-QNTY.

*        IF IT_SUB-STS    NE 0 AND
*           IT_MODULE-STS EQ C_EXIST.
*          IT_MODULE-STS = C_SUB_ERR.
*        ENDIF.
        ENDLOOP.
      ENDIF.
      MOVE: IT_MODULE-DMAMT TO IT_MODULE-DMBTR.
      IT_MODULE-MOAMT = IT_MODULE-ASYTR + IT_MODULE-DMBTR.

      LW_MTOT = IT_MODULE-NETPR + IT_MODULE-ZTIR.

      MODIFY IT_MODULE.
    ENDLOOP.
    READ TABLE IT_MODULE INDEX 1.
    LW_MTOT = IT_MODULE-NETPR + IT_MODULE-ZTIR.

    READ TABLE IT_SUB WITH KEY NETPR = 0.
    IF SY-SUBRC = 0.
*      PE_REMARK = 'M1'.
      PE_REMARK = 'Zero sub part'.
**                'Zero sub price'
    ENDIF.
    IF IT_MODULE-MOAMT NE  LW_MTOT.
*      PE_REMARK =  'M2'.
      IF PE_REMARK IS INITIAL.
        PE_REMARK =  'Module Roll up  vs Info price mismatch'.
      ELSE.
        CONCATENATE PE_REMARK 'Module Roll up  vs Info price mismatch'
                    INTO PE_REMARK SEPARATED BY '*'.
      ENDIF.
**                'Roll over rice is not same as module price'
    ENDIF.
    SELECT * INTO TABLE LT_A018
                           FROM A018
                          WHERE KAPPL EQ 'M'
                            AND KSCHL EQ 'PB00'
                            AND LIFNR = P_LIFNR
                            AND MATNR EQ P_MATNR
*                      AND DATAB <= P_DATE
                            AND DATBI < P_DATE.

    IF SY-SUBRC <> 0.
      SELECT * INTO TABLE LT_A017
                       FROM A017
                      WHERE KAPPL EQ 'M'
                        AND KSCHL EQ 'PB00'
                        AND LIFNR = P_LIFNR
                        AND MATNR EQ P_MATNR
*                      AND DATAB <= P_DATE
                        AND DATBI < P_DATE.

    ENDIF.

    IF LT_A018[] IS INITIAL.
      SORT LT_A017 DESCENDING BY DATAB.
      READ TABLE LT_A017 INDEX 1.
      W_KNUMH = LT_A017-KNUMH.
    ELSE.
      SORT LT_A018 DESCENDING BY DATAB.
      READ TABLE LT_A018 INDEX 1.
      W_KNUMH = LT_A018-KNUMH.
    ENDIF.


    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_INFO_ITEM
            FROM KONP
           WHERE KNUMH EQ W_KNUMH
             AND LOEVM_KO EQ SPACE
             AND KBETR > 0.

    READ TABLE IT_INFO_ITEM WITH KEY KSCHL = 'PB00'.
    IF SY-SUBRC = 0 AND IT_INFO_ITEM-KBETR > 0.

      L_ABN_PRICE = ( ( P_PRICE - IT_INFO_ITEM-KBETR ) /
                           IT_INFO_ITEM-KBETR ) * 100.
      L_ABN_PRICE = ABS( L_ABN_PRICE ).
      IF L_ABN_PRICE > 25.
*        PE_REMARK =  'M3'.
        IF PE_REMARK IS INITIAL.
          PE_REMARK =  'Module Abnormal Price Var <> 25%'.
*                        price variance > 5'
        ELSE.
          CONCATENATE PE_REMARK
                 'Module Abnormal Price Var <> 25%'
                 INTO PE_REMARK SEPARATED BY '*'.

        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  PE_INDEX = P_INDEX.

** Get GR QTY
  CLEAR: LT_EKBE[].
  SELECT EBELN EBELP GJAHR BELNR LFBNR BWART SHKZG MENGE
    INTO TABLE LT_EKBE
    FROM EKBE
    WHERE BUDAT BETWEEN P_FDATE AND P_TDATE
      AND BEWTP = 'E'
      AND BWART IN ('101', '102', '122', '123')
      AND MATNR = P_MATNR.

  CLEAR: PE_GRQTY.
  LOOP AT LT_EKBE.
    IF LT_EKBE-SHKZG = 'S'.
      PE_GRQTY = PE_GRQTY + LT_EKBE-MENGE.
    ELSE.
      PE_GRQTY = PE_GRQTY - LT_EKBE-MENGE.
    ENDIF.
  ENDLOOP.

  SORT LT_EKBE BY EBELN EBELP GJAHR LFBNR.
  DELETE ADJACENT DUPLICATES FROM LT_EKBE
        COMPARING EBELN EBELP GJAHR LFBNR.

  LOOP AT LT_EKBE.
    SELECT SINGLE B~BELNR INTO L_BELNR
      FROM EKBE AS A
      INNER JOIN RBKP_BLOCKED AS B
      ON A~BELNR = B~BELNR
     AND A~GJAHR = B~GJAHR
      WHERE EBELN = LT_EKBE-EBELN
        AND EBELP = LT_EKBE-EBELP
        AND A~GJAHR = LT_EKBE-GJAHR
        AND A~BELNR = LT_EKBE-LFBNR
        AND BEWTP = 'Q'.
*        AND MATNR = P_MATNR.
    IF SY-SUBRC = 0.
*      CONCATENATE PE_REMARK 'E1' INTO PE_REMARK SEPARATED BY SPACE.
      CONCATENATE PE_REMARK 'Payment Block' INTO PE_REMARK
        SEPARATED BY '*'.
      EXIT.
    ENDIF.
  ENDLOOP.

*  SELECT EBELN EBELP GJAHR BELNR LFBNR BWART SHKZG MENGE
*       INTO TABLE LT_EKBE_INV
*       FROM EKBE
*       FOR ALL ENTRIES IN LT_EKBE
*       WHERE EBELN = LT_EKBE-EBELN
*         AND EBELP = LT_EKBE-EBELP
*         AND GJAHR = LT_EKBE-GJAHR
*         AND BELNR = LT_EKBE-LFBNR
*         AND BEWTP = 'Q'
*         AND MATNR = P_MATNR.
*
*  IF SY-SUBRC = 0.
*
*  SORT LT_EKBE_INV BY BELNr GJAHR.
*  DELETE ADJACENT DUPLICATES FROM LT_EKBE_INV
*        COMPARING BELNr GJAHR.
*
*    SELECT * INTO TABLE LT_RBKP_BLOCKED
*     FROM RBKP_BLOCKED  UP TO 1 ROWS
*     FOR ALL ENTRIES IN LT_EKBE_INV
*     WHERE BELNR = LT_EKBE_INV-BELNR
*       AND GJAHR = LT_EKBE_INV-GJAHR
*       AND BUKRS = 'H201'.
*
*    IF SY-SUBRC = 0.
*      CONCATENATE PE_REMARK 'E1' INTO PE_REMARK SEPARATED BY SPACE.
*    ENDIF.
*  ENDIF.

ENDFUNCTION.

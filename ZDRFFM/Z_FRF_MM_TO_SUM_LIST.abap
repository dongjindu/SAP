FUNCTION Z_FRF_MM_TO_SUM_LIST.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_REFNR) LIKE  LTAK-REFNR
*"     VALUE(I_STDAT) LIKE  LTAK-STDAT
*"     VALUE(I_VLTYP) LIKE  LTAP-VLTYP OPTIONAL
*"     VALUE(I_TCODE) LIKE  SY-TCODE
*"  EXPORTING
*"     VALUE(E_MESS) TYPE  BAPI_MSG
*"     VALUE(ZRESULT) TYPE  ZRESULT
*"  TABLES
*"      T_SUM STRUCTURE  ZSRF_TO_SUM
*"----------------------------------------------------------------------
  DATA: BEGIN OF LT_LIST OCCURS 0,
          LGNUM TYPE ZSRF_PICKER_LIST-LGNUM,
          TANUM TYPE ZSRF_PICKER_LIST-TANUM,
          MATNR TYPE ZSRF_PICKER_LIST-MATNR,
          MAKTX TYPE ZSRF_PICKER_LIST-MAKTX,
          MEINS TYPE ZSRF_PICKER_LIST-MEINS,
          ALTME TYPE ZSRF_PICKER_LIST-ALTME,
          NSOLM TYPE LTAP_NSOLM, "ZSRF_PICKER_LIST-NSOLM,
          NSOLA TYPE LTAP_NSOLA, "ZSRF_PICKER_LIST-NSOLA,
          WORKS TYPE ZSRF_PICKER_LIST-WORKS,
          RH_LH TYPE ZSRF_PICKER_LIST-RH_LH,
          REFNR TYPE ZSRF_PICKER_LIST-REFNR,
          STDAT TYPE ZSRF_PICKER_LIST-STDAT,
          STUZT TYPE ZSRF_PICKER_LIST-STUZT,
          ENDAT TYPE ZSRF_PICKER_LIST-ENDAT,
          ENUZT TYPE ZSRF_PICKER_LIST-ENUZT,
          VLTYP TYPE ZSRF_PICKER_LIST-VLTYP,
          VLPLA TYPE ZSRF_PICKER_LIST-VLPLA,
          NLTYP TYPE ZSRF_PICKER_LIST-NLTYP,
          NLPLA TYPE ZSRF_PICKER_LIST-NLPLA,
          PQUIT TYPE ZSRF_PICKER_LIST-PQUIT,
          SPPTL TYPE ZTMM_MAST-SPPTL,
          PVQUI TYPE ZSRF_PICKER_LIST-PVQUI,
          EXECUTED_FLG TYPE ZSRF_PICKER_LIST-EXECUTED_FLG,
          CHK TYPE ZSRF_PICKER_LIST-CHK,
        END OF LT_LIST.
  DATA: BEGIN OF LT_SUM OCCURS 0,
          STDAT TYPE ZSRF_TO_SUM-STDAT,
          STUZT TYPE ZSRF_TO_SUM-STUZT,
*          MATNR TYPE MLGT-MATNR,
*          VLTYP TYPE ZSRF_PICKER_LIST-VLTYP,
          MATCOUT TYPE I,
          BXCOUNT TYPE LTAP_NSOLM,
          TOCOUNT TYPE LTAP_NSOLM,
        END OF LT_SUM.

  DATA: BEGIN OF LT_SUM1 OCCURS 0,
          STDAT TYPE ZSRF_TO_SUM-STDAT,
          STUZT TYPE ZSRF_TO_SUM-STUZT,
          MATNR TYPE MLGT-MATNR,
          VLTYP TYPE ZSRF_PICKER_LIST-VLTYP,
          TOTAL TYPE LTAP_NSOLM,
          COCOUNT TYPE LTAP_NSOLM,
        END OF LT_SUM1.

  DATA: L_BOXQT1 TYPE P DECIMALS 5,
        L_BOXQT2 TYPE I.
  DATA: BEGIN OF LT_MLGT OCCURS 0,
          MATNR TYPE MLGT-MATNR,
*          LGNUM TYPE MLGT-LGNUM,
          LGTYP TYPE MLGT-LGTYP,
          RDMNG TYPE MLGT-RDMNG,
        END OF LT_MLGT.

  DATA: L_LGNUM LIKE LTAK-LGNUM VALUE 'P01'.
  DATA: L_TABIX LIKE SY-TABIX.

* For Tcode 'ZMME89', We need Source storage type as Criteria.
  IF I_TCODE = 'ZMME89'.  "Transfer Orders for each group(Transfer)
*   /Begin of Added by Hakchin(20040209)
    IF I_VLTYP IS INITIAL.
*     MESSAGE s999(zmmm) WITH 'Source stor.ty. is empty!'(004).
*     EXIT.
      I_VLTYP = '422'. "Needed by Sunil(20040210)
    ENDIF.
*   /End of Added by Hakchin(20040209)
*   /Begin of Commented by Hakchin(20040209)
*   CHECK NOT ltap-vltyp IS INITIAL.
*   /End of Commented by Hakchin(20040209)
    PERFORM CHECK_VLTYP USING I_VLTYP
                              L_LGNUM . "Check Source storage type
    IF SY-SUBRC <> 0.
      E_MESS  = TEXT-M08.
      ZRESULT = TEXT-M02.

*      MESSAGE S999(ZMMM) WITH 'There is no Source storage type'(001).
      EXIT.
    ENDIF.
  ENDIF.

*/Begin of Added by Hakchin(20040211)  (For tese use)
  IF I_TCODE = 'ZMME88S'."T/O for each group(Pick) with Src.Stor.
    IF I_VLTYP IS INITIAL.
      E_MESS  = TEXT-M09.
      ZRESULT = TEXT-M02.

*      MESSAGE S999(ZMMM) WITH 'Source stor.ty. is empty!'(004).
      EXIT.
    ELSEIF I_VLTYP = '422'. "Needed by Sunil(20040211)
      E_MESS  = TEXT-M10.
      ZRESULT = TEXT-M02.

*      MESSAGE S999(ZMMM)
*          WITH 'Source stor.ty. 422 is not allowed!'(005).
      EXIT.
    ENDIF.
    PERFORM CHECK_VLTYP USING I_VLTYP
                              L_LGNUM. "Check Source storage type
    IF SY-SUBRC <> 0.
      E_MESS  = TEXT-M08.
      ZRESULT = TEXT-M02.

*      MESSAGE S999(ZMMM) WITH 'There is no Source storage type'(001).
      EXIT.
    ENDIF.
  ENDIF.
*/End of Added by Hakchin(20040211)
*  CLEAR : I_STDAT.
  SELECT LTAK~LGNUM  "Warehouse Number
         LTAK~TANUM  "Transfer order number
         LTAP~MATNR  "Material number
         MAKT~MAKTX  "Material description
         LTAP~MEINS  "Base unit of measure
         LTAP~ALTME  "Alternative unit of measure for stockkeeping unit
         LTAP~NSOLM  "Destination target quantity in stockkeeping unit
         LTAP~NSOLA  "Destination target quantity in alternative unit
         ZTMM_MAST~WORKS  "Workstation
         ZTMM_MAST~RH_LH  "RH/LH
         LTAK~REFNR  "Group
         LTAK~STDAT  "Start date of the transfer order
         LTAK~STUZT  "Start time of the transfer order
         LTAK~ENDAT  "Transfer order end date
         LTAK~ENUZT  "Transfer order end time
         LTAP~VLTYP  "Source storage type
         LTAP~VLPLA  "Source storage bin
         LTAP~NLTYP  "Destination storage type
         LTAP~NLPLA  "Destination storage bin
         LTAP~PVQUI  "Indicator: Material pick has been confirmed
         ZTMM_MAST~SPPTL " Supply to Line
    INTO CORRESPONDING FIELDS OF TABLE LT_LIST
    FROM LTAK
    INNER JOIN LTAP
      ON LTAP~LGNUM = LTAK~LGNUM AND "Warehouse Number
         LTAP~TANUM = LTAK~TANUM AND "T/O number
         LTAP~PQUIT = SPACE "Open TO(Indicator: confirmation complete)
    INNER JOIN MAKT  "For desc of matnr
      ON MAKT~SPRAS = SY-LANGU AND  "Language
         MAKT~MATNR = LTAP~MATNR    "Material
    LEFT OUTER JOIN ZTMM_MAST
      ON ZTMM_MAST~WERKS = 'P001' AND "Plant
         ZTMM_MAST~MATNR = LTAP~MATNR "Material
    WHERE LTAK~REFNR = I_REFNR        "Group
* Begin of For Test
    AND   LTAK~STDAT = I_STDAT.       "Start date of the transfer order
  IF SY-SUBRC EQ 0.

    SELECT MATNR
           LGTYP
           RDMNG
         FROM MLGT
         INTO TABLE LT_MLGT
         WHERE LVORM EQ SPACE
         AND   LGNUM EQ 'P01'.
    IF SY-SUBRC EQ 0.
      SORT LT_MLGT BY MATNR LGTYP.
    ENDIF.

  ENDIF.

* For Tcode 'ZMME88', We need only Source storage type = '422' and '522'
.
  IF I_TCODE = 'ZMME88'.  "Transfer Orders for each group(Pick)
    DELETE LT_LIST WHERE ( VLTYP <> '422'
                           AND VLTYP <> '522'
                           AND VLTYP <> '523' ). "Source storage type
  ENDIF.

*/Begin of Added by Hakchin(20040209)
* For Tcode 'ZMME88', We don't need 'Pick Confirmed' Data.
  IF I_TCODE = 'ZMME88'.  "Transfer Orders for each group(Pick)
    DELETE LT_LIST WHERE PVQUI = 'X'.
    "Indicator: Material pick has been confirmed
  ENDIF.
*/End of Added by Hakchin(20040209)

*/ For Tcode 'ZMME89', We need Source storage type as Criteria.
*/Begin of Added by Hakchin(20040205) (Needed by Sunil)
  IF I_TCODE = 'ZMME89'.  "Transfer Orders for each group(Transfer)
    DELETE LT_LIST WHERE VLTYP <> I_VLTYP.
* For Tcode 'ZMME89',
* We need Only 'Pick Confirmed' Data (Partial Open TO).
    DELETE LT_LIST WHERE PVQUI = SPACE.
    "Indicator: Material pick has been confirmed
  ENDIF.
*/End of Added by Hakchin(20040205) (Needed by Sunil)

*/Begin of Added by Hakchin(20040211) (For Test Use)
* For Tcode 'ZMME88S', We don't need 'Pick Confirmed' Data.
  IF I_TCODE = 'ZMME88S'.  "Transfer Orders for each group(Pick)
    DELETE LT_LIST WHERE PVQUI = 'X'.
    "Indicator: Material pick has been confirmed
  ENDIF.
  IF I_TCODE = 'ZMME88S'.  "Transfer Orders for each group(Pick)
    DELETE LT_LIST WHERE VLTYP <> I_VLTYP.
  ENDIF.
*/End of Added by Hakchin(20040211)
  LOOP AT LT_LIST.
    MOVE-CORRESPONDING LT_LIST TO LT_SUM.
    LT_SUM-TOCOUNT = LT_LIST-NSOLM.
    LT_SUM-MATCOUT = 1.
    READ TABLE LT_MLGT WITH KEY MATNR = LT_LIST-MATNR
                                LGTYP = LT_LIST-VLTYP
                       BINARY SEARCH TRANSPORTING RDMNG.
    IF SY-SUBRC EQ 0.
      IF LT_MLGT-RDMNG NE 0.
        L_BOXQT1 = LT_SUM-TOCOUNT / LT_MLGT-RDMNG.
        L_BOXQT2 = CEIL( L_BOXQT1 ).
      ELSE.
        L_BOXQT2 = 1.
      ENDIF.
    ELSE.
      L_BOXQT2 = 1.
    ENDIF.
    LT_SUM-BXCOUNT = L_BOXQT2.
    COLLECT LT_SUM.
    CLEAR LT_SUM.
  ENDLOOP.
  SORT LT_SUM BY STDAT STUZT.
  LOOP AT LT_SUM.
    MOVE-CORRESPONDING LT_SUM TO T_SUM.

    WRITE: LT_SUM-MATCOUT TO T_SUM-MATCOUT
                          LEFT-JUSTIFIED NO-ZERO UNIT 'EA',
           LT_SUM-TOCOUNT TO T_SUM-TOCOUNT
                          LEFT-JUSTIFIED NO-ZERO UNIT 'EA',
           LT_SUM-BXCOUNT TO T_SUM-BXCOUNT
                          LEFT-JUSTIFIED NO-ZERO UNIT 'EA'.

    COLLECT T_SUM. CLEAR LT_SUM.
  ENDLOOP.
* If No transfer orders found
  IF T_SUM[] IS INITIAL.
*        MESSAGE s172(l3).
    E_MESS  = TEXT-M07.
    ZRESULT = TEXT-M14.
*      'There is no data with selection conditions'(002).
*    EXIT.
  ELSE.
    SORT T_SUM BY STDAT STUZT.
    E_MESS  = TEXT-M03.
    ZRESULT = TEXT-M13.

  ENDIF.
ENDFUNCTION.

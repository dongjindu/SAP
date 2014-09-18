FUNCTION Z_FRF_SEL_LTAK_LTAP_LIST .
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_REFNR) LIKE  LTAK-REFNR
*"     VALUE(I_VLTYP) LIKE  LTAP-VLTYP OPTIONAL
*"     VALUE(I_STDAT) LIKE  LTAK-STDAT OPTIONAL
*"     VALUE(I_STUZT) LIKE  LTAK-STUZT OPTIONAL
*"     VALUE(I_TCODE) LIKE  SY-TCODE
*"  EXPORTING
*"     VALUE(E_MESS) TYPE  BAPI_MSG
*"     VALUE(ZRESULT) TYPE  ZRESULT
*"  TABLES
*"      T_LIST STRUCTURE  ZSRF_PICKER_LIST
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
          NISTA TYPE LTAP_NISTA, "ZSRF_PICKER_LIST-NISTA,
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
          ZZFSTP TYPE ZTMM_MAST-ZZFSTP,
          PVQUI TYPE ZSRF_PICKER_LIST-PVQUI,
          EXECUTED_FLG TYPE ZSRF_PICKER_LIST-EXECUTED_FLG,
          CHK TYPE ZSRF_PICKER_LIST-CHK,
        END OF LT_LIST.
  DATA: BEGIN OF LT_MLGT OCCURS 0,
          MATNR TYPE MLGT-MATNR,
*          LGNUM TYPE MLGT-LGNUM,
          LGTYP TYPE MLGT-LGTYP,
          RDMNG TYPE MLGT-RDMNG,
        END OF LT_MLGT.
  DATA: L_BOXQT1 TYPE P DECIMALS 5,
        L_BOXQT2 TYPE I.
  DATA: L_LGNUM LIKE LTAK-LGNUM VALUE 'P01'.
  DATA: L_TABIX LIKE SY-TABIX.
  data: l_ZZFSTP(3).

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

  IF I_STDAT IS INITIAL OR
     I_STUZT IS INITIAL.

*    SELECT LTAK~LGNUM  "Warehouse Number
*           LTAK~TANUM  "Transfer order number
*           LTAP~MATNR  "Material number
*           MAKT~MAKTX  "Material description
*           LTAP~MEINS  "Base unit of measure
*          LTAP~ALTME  "Alternative unit of measure for stockkeeping
*unit
*           LTAP~NSOLM  "Destination target quantity in stockkeeping
*unit
*           LTAP~NSOLA  "Destination target quantity in alternative unit
*           ZTMM_MAST~WORKS  "Workstation
*           ZTMM_MAST~RH_LH  "RH/LH
*           LTAK~REFNR  "Group
*           LTAK~STDAT  "Start date of the transfer order
*           LTAK~STUZT  "Start time of the transfer order
*           LTAK~ENDAT  "Transfer order end date
*           LTAK~ENUZT  "Transfer order end time
*           LTAP~VLTYP  "Source storage type
*           LTAP~VLPLA  "Source storage bin
*           LTAP~NLTYP  "Destination storage type
*           LTAP~NLPLA  "Destination storage bin
*           LTAP~PVQUI  "Indicator: Material pick has been confirmed
*           ZTMM_MAST~SPPTL " Supply to Line
*      INTO CORRESPONDING FIELDS OF TABLE LT_LIST
*      FROM LTAK
*      INNER JOIN LTAP
*        ON LTAP~LGNUM = LTAK~LGNUM AND "Warehouse Number
*           LTAP~TANUM = LTAK~TANUM AND "T/O number
*           LTAP~PQUIT = SPACE "Open TO(Indicator: confirmation
*complete)
*      INNER JOIN MAKT  "For desc of matnr
*        ON MAKT~SPRAS = SY-LANGU AND  "Language
*           MAKT~MATNR = LTAP~MATNR    "Material
*      LEFT OUTER JOIN ZTMM_MAST
*        ON ZTMM_MAST~WERKS = 'P001' AND "Plant
*           ZTMM_MAST~MATNR = LTAP~MATNR "Material
*      WHERE LTAK~REFNR = I_REFNR.   "Group
  ELSE.
    SELECT LTAK~LGNUM  "Warehouse Number
           LTAK~TANUM  "Transfer order number
           LTAP~MATNR  "Material number
           MAKT~MAKTX  "Material description
           LTAP~MEINS  "Base unit of measure
          LTAP~ALTME  "Alternative unit of measure for stockkeeping unit
           LTAP~NSOLM  "Destination target quantity in stockkeeping unit
           LTAP~NSOLA  "Destination target quantity in alternative unit
           LTAP~NISTA  "pick
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
           ZTMM_MAST~ZZFSTP "Feeder stop
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
      WHERE LTAK~REFNR = I_REFNR   AND "Group
* Begin of For Test
           LTAK~STDAT = I_STDAT AND "Start date of the transfer order
           LTAK~STUZT = I_STUZT.    "Start time of the transfer order
* End of For Test
  ENDIF.
  IF SY-SUBRC EQ 0.
*          NSOLM TYPE LTAP_NSOLM, "ZSRF_PICKER_LIST-NSOLM,
*          NSOLA TYPE LTAP_NSOLA, "ZSRF_PICKER_LIST-NSOLA,
    REFRESH T_LIST. CLEAR T_LIST.
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
    LOOP AT LT_LIST.
      MOVE-CORRESPONDING LT_LIST TO T_LIST.
      CASE LT_LIST-SPPTL.
        WHEN 'S'. T_LIST-ZTCDH = 'TH'.  "houry
        WHEN 'N'. T_LIST-ZTCDH = 'TD'.  "Daily
      ENDCASE.
      WRITE: LT_LIST-NSOLM TO T_LIST-NSOLM UNIT LT_LIST-MEINS
                                           LEFT-JUSTIFIED NO-ZERO.
      WRITE: LT_LIST-NISTA TO T_LIST-NISTA UNIT LT_LIST-MEINS
                                           LEFT-JUSTIFIED NO-ZERO.
*             LT_LIST-NSOLA TO T_LIST-NSOLA UNIT LT_LIST-MEINS
*                                           LEFT-JUSTIFIED NO-ZERO.
      CLEAR: L_BOXQT1, L_BOXQT2.
      READ TABLE LT_MLGT WITH KEY MATNR = LT_LIST-MATNR
                                  LGTYP = LT_LIST-VLTYP
                         BINARY SEARCH TRANSPORTING RDMNG.
      IF SY-SUBRC EQ 0.
        IF LT_MLGT-RDMNG NE 0.
          L_BOXQT1 = LT_LIST-NSOLA / LT_MLGT-RDMNG.
          L_BOXQT2 = CEIL( L_BOXQT1 ).
        ELSE.
          L_BOXQT2 = 1.
        ENDIF.
      ELSE.
        L_BOXQT2 = 1.
      ENDIF.
      WRITE: L_BOXQT2 TO T_LIST-NSOLA LEFT-JUSTIFIED NO-ZERO.

      APPEND T_LIST. CLEAR T_LIST.
    ENDLOOP.
    REFRESH LT_LIST. CLEAR LT_LIST.
  ENDIF.

* For Tcode 'ZMME88', We need only Source storage type = '422'.
  IF I_TCODE = 'ZMME88'.  "Transfer Orders for each group(Pick)
    DELETE T_LIST WHERE ( VLTYP <> '422'
** changed by Furong on 07/19/07  "UD1K941063
                         AND VLTYP <> '522'
                         AND VLTYP <> '523' ). "Source storage type
** end of change
  ENDIF.

*/Begin of Added by Hakchin(20040209)
* For Tcode 'ZMME88', We don't need 'Pick Confirmed' Data.
  IF I_TCODE = 'ZMME88'.  "Transfer Orders for each group(Pick)
    DELETE T_LIST WHERE PVQUI = 'X'.
    "Indicator: Material pick has been confirmed
  ENDIF.
*/End of Added by Hakchin(20040209)

*/ For Tcode 'ZMME89', We need Source storage type as Criteria.
*/Begin of Added by Hakchin(20040205) (Needed by Sunil)
  IF I_TCODE = 'ZMME89'.  "Transfer Orders for each group(Transfer)
    DELETE T_LIST WHERE VLTYP <> I_VLTYP.
* For Tcode 'ZMME89',
* We need Only 'Pick Confirmed' Data (Partial Open TO).
    DELETE T_LIST WHERE PVQUI = SPACE.
    "Indicator: Material pick has been confirmed
  ENDIF.
*/End of Added by Hakchin(20040205) (Needed by Sunil)

*/Begin of Added by Hakchin(20040211) (For Test Use)
* For Tcode 'ZMME88S', We don't need 'Pick Confirmed' Data.
  IF I_TCODE = 'ZMME88S'.  "Transfer Orders for each group(Pick)
    DELETE T_LIST WHERE PVQUI = 'X'.
    "Indicator: Material pick has been confirmed
  ENDIF.
  IF I_TCODE = 'ZMME88S'.  "Transfer Orders for each group(Pick)
    DELETE T_LIST WHERE VLTYP <> I_VLTYP.
  ENDIF.
*/End of Added by Hakchin(20040211)

*  SORT T_LIST BY LGNUM TANUM.
  IF T_LIST[] IS INITIAL.
*   MESSAGE s172(l3).
    E_MESS  = TEXT-M07.
    ZRESULT = TEXT-M14.
    EXIT.
  ELSE.

** Cahnge by Furong on 04/22/08
    IF I_TCODE = 'ZMME89'.
      READ TABLE T_LIST INDEX 1.
      IF T_LIST-ZZFSTP = '000'.
        DATA: L_INDEX LIKE SY-TABIX.
        LOOP AT T_LIST.
          L_INDEX = SY-TABIX.
          SELECT SINGLE LGBER INTO l_ZZFSTP
            FROM LAGP
            WHERE LGTYP = T_LIST-NLTYP
              AND LGPLA = T_LIST-NLPLA.
          T_LIST-ZZFSTP = l_ZZFSTP.
          MODIFY T_LIST INDEX L_INDEX TRANSPORTING ZZFSTP.
        ENDLOOP.
        SORT T_LIST BY ZZFSTP NLPLA MATNR TANUM.
      ELSE.
        SORT T_LIST BY ZZFSTP MATNR TANUM.
      ENDIF.
** End of change
    ELSE.
** changed by Fuorng on 06/04/07 for sort sequence improvement
** Help desk ticket: 75UE212586; Transport Request: UD1K940749
      DATA: BEGIN OF LT_LIST_TEMP OCCURS 0,
            KOBER LIKE LAGP-KOBER.
              INCLUDE STRUCTURE ZSRF_PICKER_LIST.
      DATA: END OF LT_LIST_TEMP.
      LOOP AT T_LIST.
        MOVE-CORRESPONDING T_LIST TO LT_LIST_TEMP.
        SELECT SINGLE KOBER INTO LT_LIST_TEMP-KOBER
          FROM LAGP
          WHERE LGNUM = 'P01'
            AND LGTYP = T_LIST-VLTYP
            AND LGPLA = T_LIST-VLPLA.
        APPEND LT_LIST_TEMP.
        CLEAR: LT_LIST_TEMP.
      ENDLOOP.
** Changed on 06/29/07
*      IF  I_TCODE = 'ZMME88'.
*        SORT LT_LIST_TEMP DESCENDING BY REFNR KOBER ZZFSTP MATNR TANUM.
*      ELSE.
*** changed on 08/23/07
*        SORT LT_LIST_TEMP BY REFNR KOBER ZZFSTP MATNR TANUM.
      SORT LT_LIST_TEMP BY REFNR KOBER ZZFSTP VLPLA MATNR TANUM.
*** end of change
*      ENDIF.
      REFRESH T_LIST.
      CLEAR: T_LIST.
      LOOP AT LT_LIST_TEMP.
        MOVE-CORRESPONDING LT_LIST_TEMP TO T_LIST.
        APPEND T_LIST.
        CLEAR: T_LIST.
      ENDLOOP.
**       SORT T_LIST DESCENDING BY ZZFSTP MATNR TANUM.
** end of change
    ENDIF.
    E_MESS  = TEXT-M03.
    ZRESULT = TEXT-M13.

  ENDIF.

ENDFUNCTION.

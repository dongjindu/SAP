FUNCTION Z_FMM_WHERE_USED_MAT_MULTI.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(P_MATNR) LIKE  MARA-MATNR
*"     VALUE(P_WERKS) LIKE  MARC-WERKS
*"  EXPORTING
*"     VALUE(PO_MATNR) LIKE  MARA-MATNR
*"     VALUE(PO_WERKS) LIKE  MARC-WERKS
*"  TABLES
*"      PT_WULTB STRUCTURE  STPOV
*"----------------------------------------------------------------------

  DATA: L_DATUB LIKE SY-DATUM,
        p_DATUV LIKE SY-DATUM,
        p_DATUB LIKE SY-DATUM,
        L_MTART LIKE MARA-MTART,
        L_INDEX LIKE SY-TABIX,
        L_INDEX_NEXT LIKE SY-TABIX,
        l_date_c(8).

  DATA: BEGIN OF WU_CTAB_KEY OCCURS 0,
        MATNR LIKE MARA-MATNR,
        WERKS LIKE T001W-WERKS,
        STLAN LIKE MAST-STLAN,
        MENGE LIKE STPOV-MENGE,
        LEVEL LIKE STPOV-LEVEL,
      END OF WU_CTAB_KEY.

  DATA: WULTB TYPE TABLE OF STPOV WITH HEADER LINE,
        CSCEQUI  TYPE TABLE OF CSCEQUI WITH HEADER LINE,
        CSCKND  TYPE TABLE OF CSCKND WITH HEADER LINE,
        CSCMAT TYPE TABLE OF  CSCMAT WITH HEADER LINE,
        CSCSTD TYPE TABLE OF  CSCSTD WITH HEADER LINE,
        CSCTPL TYPE TABLE OF  CSCTPL WITH HEADER LINE.

  DATA: WU_KEY LIKE LINE OF WU_CTAB_KEY,
        LW_WULTB LIKE LINE OF WULTB.

*  L_DATUB =  SY-DATUM + 300000.
*  L_DATUV = SY-DATUM - 3650.
  p_DATUB =  l_date_c = '29991231'.
  p_DATUV = SY-DATUM - 3650.

  CLEAR WU_CTAB_KEY.
  REFRESH WU_CTAB_KEY.
  WU_CTAB_KEY-MATNR = P_MATNR.
  WU_CTAB_KEY-WERKS = P_WERKS.
  WU_CTAB_KEY-MENGE = 1.
  WU_CTAB_KEY-LEVEL = 1.
  APPEND WU_CTAB_KEY.

  LOOP AT WU_CTAB_KEY.
    REFRESH WULTB.
    CALL FUNCTION 'CS_WHERE_USED_MAT'
         EXPORTING
              DATUB                      = p_DATUB
              DATUV                      = p_DATUV
              MATNR                      = WU_CTAB_KEY-MATNR
              STLAN                      = '1'
              WERKS                      = WU_CTAB_KEY-WERKS
         TABLES
              WULTB                      = WULTB
              EQUICAT                    = CSCEQUI
              KNDCAT                     = CSCKND
              MATCAT                     = CSCMAT
              STDCAT                     = CSCSTD
              TPLCAT                     = CSCTPL
         EXCEPTIONS
              CALL_INVALID               = 1
              MATERIAL_NOT_FOUND         = 2
              NO_WHERE_USED_REC_FOUND    = 3
              NO_WHERE_USED_REC_SELECTED = 4
              NO_WHERE_USED_REC_VALID    = 5
              OTHERS                     = 6.
    CHECK SY-SUBRC EQ 0.

    SORT WULTB BY MATNR DATUV.

    LOOP AT WULTB.
      L_INDEX = SY-TABIX.
      L_INDEX_NEXT = SY-TABIX + 1.
      L_DATUB = WULTB-DATUB.
      READ TABLE WULTB INTO LW_WULTB INDEX L_INDEX_NEXT.
      IF L_DATUB = LW_WULTB-DATUV.
        DELETE WULTB INDEX L_INDEX.
      ENDIF.
    ENDLOOP.

    LOOP AT WULTB.
      SELECT SINGLE MTART
            INTO L_MTART
            FROM MARA
            WHERE MATNR EQ WULTB-MATNR.

      PT_WULTB-WERKS = WULTB-WERKS.
      PT_WULTB-STLAN = WULTB-STLAN.
      PT_WULTB-MATNR = WULTB-MATNR.
*            xwul-stlal = wultb-vwalt.
      PT_WULTB-MEINS = WULTB-MEINS.
      PT_WULTB-MENGE = WULTB-MENGE * WU_CTAB_KEY-MENGE.
      PT_WULTB-DATUB = WULTB-DATUB.
      PT_WULTB-DATUV = WULTB-DATUV.
      PT_WULTB-LEVEL = WU_CTAB_KEY-LEVEL.
      PT_WULTB-IDNRK = WULTB-IDNRK.
      COLLECT PT_WULTB.

      IF L_MTART <> 'FERT'.
        READ TABLE WU_CTAB_KEY INTO WU_KEY
                               WITH KEY MATNR = WULTB-MATNR
                                        WERKS = WULTB-WERKS.
        IF SY-SUBRC <> 0.
          CLEAR WU_KEY.
          WU_KEY-MATNR = WULTB-MATNR.
          WU_KEY-WERKS = WULTB-WERKS.
          WU_KEY-MENGE = WULTB-MENGE * WU_CTAB_KEY-MENGE.
          WU_KEY-LEVEL = WU_CTAB_KEY-LEVEL + 1.
          APPEND WU_KEY TO WU_CTAB_KEY.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  PO_MATNR = P_MATNR.
  PO_WERKS = P_WERKS.
ENDFUNCTION.

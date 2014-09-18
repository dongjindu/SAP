FUNCTION ZIM_NUMBER_GET_NEXT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFREQTY) LIKE  ZTREQHD-ZFREQTY
*"     VALUE(BUKRS) LIKE  ZTBKPF-BUKRS DEFAULT SPACE
*"     VALUE(GJAHR) LIKE  ZTBKPF-GJAHR DEFAULT '0000'
*"  EXPORTING
*"     VALUE(ZFREQNO) LIKE  ZTREQHD-ZFREQNO
*"  EXCEPTIONS
*"      ERROR_DUPLICATE
*"      NOT_FOUND
*"      NOT_INPUT
*"      NOT_TYPE
*"      NOT_RANGE
*"      LOCKED
*"----------------------------------------------------------------------
DATA : L_NRRANGENR LIKE INRI-NRRANGENR,
       L_OBJECT    LIKE INRI-OBJECT,
       L_RET_CD    LIKE INRI-RETURNCODE.

  CASE ZFREQTY.
     WHEN 'LC' OR 'LO' OR 'PU' OR 'DA' OR 'DP' OR 'TT' OR 'GS'.  "
        L_OBJECT     = 'ZIMLC'.
     WHEN 'BL'.         " B/L
        L_OBJECT = 'ZIMBL'.
     WHEN 'IV'.         " INVOICE
        L_OBJECT = 'ZIMIV'.
     WHEN 'OF'.         " LOCAL OFFER SHEET
        L_OBJECT = 'ZIMOF'.
     WHEN 'VT'.         " 세금계산서.
        L_OBJECT = 'ZIMVT'.
     WHEN 'RE'.         " 인수증.
        L_OBJECT = 'ZIMRE'.
    WHEN 'MS'.         " 모선관리.
        L_OBJECT = 'ZIMMS'.
    WHEN 'CI'.         " COMMERCIAL INVOICE.
        L_OBJECT = 'ZIMCI'.
    WHEN 'CG'.         " 하역관리.
        L_OBJECT = 'ZIMCG'.
    WHEN 'CD'.         " 비용문서.
        L_OBJECT = 'ZIMCD'.
    WHEN 'CV'.         " 비용문서.
        L_OBJECT = 'ZIMCC'.
    WHEN 'TB'.
        L_OBJECT = 'ZIMTB'.
    WHEN 'BI'.
        L_OBJECT = 'ZIMBI'.
    WHEN 'PN'.
        L_OBJECT = 'ZIMPN'.
    WHEN 'IS'.
        L_OBJECT = 'ZIMTR'.
    WHEN 'FTZ'.
        L_OBJECT = 'ZIMFTZ'.
    WHEN OTHERS.
  ENDCASE.

  L_NRRANGENR  =  ZFREQTY.

  CALL FUNCTION 'NUMBER_GET_NEXT'
       EXPORTING
            NR_RANGE_NR      =   L_NRRANGENR
            OBJECT           =   L_OBJECT
            SUBOBJECT        =   BUKRS
            TOYEAR           =   GJAHR
       IMPORTING
            NUMBER           =   ZFREQNO
*            QUANTITY         =
            RETURNCODE       =   L_RET_CD
       EXCEPTIONS
            INTERVAL_NOT_FOUND      =  1 ">Interval not found
            NUMBER_RANGE_NOT_INTERN =  2 ">Number range is not internal
            OBJECT_NOT_FOUND        =  3 ">Object not defined in TNRO
**>Number of numbers requested must be > 0
            QUANTITY_IS_0           =  4
**">Number of numbers requested must be 1
            QUANTITY_IS_NOT_1       =  5
**">Interval used up. Change not possible.
            INTERVAL_OVERFLOW       =  6.

  IF SY-SUBRC <> 0.
*   Fehler bei interner Nummernvergabe
    MESSAGE A417(62).
  ELSEIF L_RET_CD >= 2.
*   Das Nummernkreisintervall ist ausgesch?ft, keine Nummer mehr frei
    MESSAGE A593(60).
  ENDIF.

ENDFUNCTION.

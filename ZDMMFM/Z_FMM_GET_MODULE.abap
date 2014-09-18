FUNCTION Z_FMM_GET_MODULE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(P_DATUB) LIKE  RC29L-DATUB
*"     VALUE(P_DATUV) LIKE  RC29L-DATUV
*"     VALUE(P_MATNR) LIKE  RC29L-MATNR
*"     VALUE(P_STLAN) LIKE  RC29L-STLAN
*"     VALUE(P_WERKS) LIKE  RC29L-WERKS
*"  EXPORTING
*"     VALUE(PE_MATNR) TYPE  MARA-MATNR
*"  TABLES
*"      STPOV STRUCTURE  STPOV
*"      EQUICAT STRUCTURE  CSCEQUI
*"      KNDCAT STRUCTURE  CSCKND
*"      MATCAT STRUCTURE  CSCMAT
*"      STDCAT STRUCTURE  CSCSTD
*"      TPLCAT STRUCTURE  CSCTPL
*"----------------------------------------------------------------------

 CALL FUNCTION 'CS_WHERE_USED_MAT'
      EXPORTING
        DATUB                            = p_DATUB
        DATUV                            = p_DATUV
        MATNR                            = p_MATNR
*       POSTP                            = ' '
*       RETCODE_ONLY                     = ' '
        STLAN                            = p_stlan
        WERKS                            = p_WERKS
*       MCLMT                            = ' '
*       MNSTL                            = ' '
*       MXSTL                            = ' '
*       NEWSI                            = ' '
*       STLTP                            = ' '
*       IMPORTING
*          TOPMAT                           = p_MC29S
      TABLES
        WULTB                            = STPOv
        EQUICAT                          = EQUICAT
        KNDCAT                           = KNDCAT
        MATCAT                           = MATCAT
        STDCAT                           = STDCAT
        TPLCAT                           = TPLCAT
*       PRJCAT                           =
       EXCEPTIONS
         CALL_INVALID                     = 1
         MATERIAL_NOT_FOUND               = 2
         NO_WHERE_USED_REC_FOUND          = 3
         NO_WHERE_USED_REC_SELECTED       = 4
         NO_WHERE_USED_REC_VALID          = 5
         OTHERS                           = 6
              .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
*      APPEND LINES OF LT_STPOS TO LT_COMP.
    ENDIF.
  pe_matnr = p_matnr.
ENDFUNCTION.

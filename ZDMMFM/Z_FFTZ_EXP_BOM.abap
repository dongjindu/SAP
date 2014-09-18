FUNCTION Z_FFTZ_EXP_BOM.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(P_CAPID) TYPE  TC04-CAPID
*"     VALUE(P_DATUV) TYPE  STKO-DATUV
*"     VALUE(P_EMENG) TYPE  STKO-BMENG
*"     VALUE(P_MEHRS) TYPE  CSDATA-XFELD
*"     VALUE(P_MMORY) TYPE  CSDATA-XFELD
*"     VALUE(P_MTNRV) TYPE  MARA-MATNR
*"     VALUE(P_STLAL) TYPE  STKO-STLAL DEFAULT SPACE
*"     VALUE(P_STLAN) TYPE  STZU-STLAN
*"     VALUE(P_WERKS) TYPE  T001W-WERKS
*"     VALUE(P_PAMATNR) TYPE  MARA-MATNR OPTIONAL
*"  EXPORTING
*"     VALUE(P_MATNR) TYPE  MARA-MATNR
*"     VALUE(P_STLAL1) TYPE  STKO-STLAL
*"     VALUE(P_DATUV1) TYPE  STKO-DATUV
*"     VALUE(P_PMATNR) TYPE  MARA-MATNR
*"  TABLES
*"      P_STPOX STRUCTURE  STPOX
*"----------------------------------------------------------------------


  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
   EXPORTING
*   FTREL                       = ' '
*   ALEKZ                       = ' '
*   ALTVO                       = ' '
*   AUFSW                       = ' '
*   AUMGB                       = ' '
*   AUMNG                       = 0
*   AUSKZ                       = ' '
*   AMIND                       = ' '
*   BAGRP                       = ' '
*   BEIKZ                       = ' '
*   BESSL                       = ' '
*   BGIXO                       = ' '
*   BREMS                       = ' '
    CAPID                       = p_capid
*   CHLST                       = ' '
*   COSPR                       = ' '
*   CUOBJ                       = 000000000000000
*   CUOVS                       = 0
*   CUOLS                       = ' '
    DATUV                       = p_datuv
*   DELNL                       = ' '
*   DRLDT                       = ' '
*   EHNDL                       = ' '
    EMENG                       = p_emeng
*   ERSKZ                       = ' '
*   ERSSL                       = ' '
*   FBSTP                       = ' '
*   KNFBA                       = ' '
*   KSBVO                       = ' '
*   MBWLS                       = ' '
*   MKTLS                       = 'X'
*   MDMPS                       = ' '
    MEHRS                       = p_mehrs
*   MKMAT                       = ' '
*   MMAPS                       = ' '
*   SALWW                       = ' '
*   SPLWW                       = ' '
    MMORY                       = p_mmory
    MTNRV                       = p_mtnrv
*   NLINK                       = ' '
*   POSTP                       = ' '
*   RNDKZ                       = ' '
*   RVREL                       = ' '
*   SANFR                       = ' '
*   SANIN                       = ' '
*   SANKA                       = ' '
*   SANKO                       = ' '
*   SANVS                       = ' '
*   SCHGT                       = ' '
*   STKKZ                       = ' '
    STLAL                       = p_stlal
    STLAN                       = p_stlan
*   STPST                       = 0
*   SVWVO                       = 'X'
    WERKS                       = p_werks
*   NORVL                       = ' '
*   MDNOT                       = ' '
*   PANOT                       = ' '
*   QVERW                       = ' '
*   VERID                       = ' '
*   VRSVO                       = 'X'
* IMPORTING
*    TOPMAT                     =
*   DSTST                       =
    TABLES
      STB                       = p_stpox
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

*&---Following value(s) are needed after the BOM explosion to relate to
*&---the parent materials.
  p_matnr  = p_mtnrv.
  p_stlal1 = p_stlal.
  p_datuv1 = p_datuv.
  p_pmatnr = p_pamatnr.

ENDFUNCTION.

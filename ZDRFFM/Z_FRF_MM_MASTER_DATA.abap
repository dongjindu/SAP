FUNCTION Z_FRF_MM_MASTER_DATA.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(E_MESS) TYPE  BAPI_MSG
*"     VALUE(ZRESULT) TYPE  ZRESULT
*"  TABLES
*"      T_LTAK STRUCTURE  ZSRF_LTAK
*"      T_T301 STRUCTURE  ZSRF_T301T
*"----------------------------------------------------------------------
* RANGES
  DATA: L_BDATU LIKE LTAK-BDATU.
*  DATA: BEGIN OF LT_LIST OCCURS 0,
*          LGNUM TYPE ZSRF_PICKER_LIST-LGNUM,
*          TANUM TYPE ZSRF_PICKER_LIST-TANUM,
*          MATNR TYPE ZSRF_PICKER_LIST-MATNR,
*          MAKTX TYPE ZSRF_PICKER_LIST-MAKTX,
*          MEINS TYPE ZSRF_PICKER_LIST-MEINS,
*          ALTME TYPE ZSRF_PICKER_LIST-ALTME,
*          NSOLM TYPE LTAP_NSOLM, "ZSRF_PICKER_LIST-NSOLM,
*          NSOLA TYPE LTAP_NSOLA, "ZSRF_PICKER_LIST-NSOLA,
*          WORKS TYPE ZSRF_PICKER_LIST-WORKS,
*          RH_LH TYPE ZSRF_PICKER_LIST-RH_LH,
*          REFNR TYPE ZSRF_PICKER_LIST-REFNR,
*          STDAT TYPE ZSRF_PICKER_LIST-STDAT,
*          STUZT TYPE ZSRF_PICKER_LIST-STUZT,
*          ENDAT TYPE ZSRF_PICKER_LIST-ENDAT,
*          ENUZT TYPE ZSRF_PICKER_LIST-ENUZT,
*          VLTYP TYPE ZSRF_PICKER_LIST-VLTYP,
*          VLPLA TYPE ZSRF_PICKER_LIST-VLPLA,
*          NLTYP TYPE ZSRF_PICKER_LIST-NLTYP,
*          NLPLA TYPE ZSRF_PICKER_LIST-NLPLA,
*          PQUIT TYPE ZSRF_PICKER_LIST-PQUIT,
*          PVQUI TYPE ZSRF_PICKER_LIST-PVQUI,
*          EXECUTED_FLG TYPE ZSRF_PICKER_LIST-EXECUTED_FLG,
*          CHK TYPE ZSRF_PICKER_LIST-CHK,
*        END OF LT_LIST.
*  L_BDATU = SY-DATUM - 7.


  SELECT DISTINCT LGNUM REFNR KZSPE
                FROM T311
                INTO TABLE T_LTAK.

  IF SY-SUBRC EQ 0.
    E_MESS = TEXT-M03.
    ZRESULT = TEXT-M04.
  ELSE.
    E_MESS = TEXT-M05.
    ZRESULT = TEXT-M02.
  ENDIF.
*  SELECT LTAK~LGNUM  "Warehouse Number
*         LTAK~TANUM  "Transfer order number
*         LTAP~MATNR  "Material number
*         MAKT~MAKTX  "Material description
*         LTAP~MEINS  "Base unit of measure
*        LTAP~ALTME  "Alternative unit of measure for stockkeeping unit
*         LTAP~NSOLM  "Destination target quantity in stockkeeping unit
*         LTAP~NSOLA  "Destination target quantity in alternative unit
*         ZTMM_MAST~WORKS  "Workstation
*         ZTMM_MAST~RH_LH  "RH/LH
*         LTAK~REFNR  "Group
*         LTAK~STDAT  "Start date of the transfer order
*         LTAK~STUZT  "Start time of the transfer order
*         LTAK~ENDAT  "Transfer order end date
*         LTAK~ENUZT  "Transfer order end time
*         LTAP~VLTYP  "Source storage type
*         LTAP~VLPLA  "Source storage bin
*         LTAP~NLTYP  "Destination storage type
*         LTAP~NLPLA  "Destination storage bin
*         LTAP~PVQUI  "Indicator: Material pick has been confirmed
*    INTO CORRESPONDING FIELDS OF TABLE LT_LIST
*    FROM LTAK
*    INNER JOIN LTAP
*      ON LTAP~LGNUM = LTAK~LGNUM AND "Warehouse Number
*         LTAP~TANUM = LTAK~TANUM AND "T/O number
*         LTAP~PQUIT = SPACE "Open TO(Indicator: confirmation complete)
*    INNER JOIN MAKT  "For desc of matnr
*      ON MAKT~SPRAS = SY-LANGU AND  "Language
*         MAKT~MATNR = LTAP~MATNR    "Material
*    LEFT OUTER JOIN ZTMM_MAST
*      ON ZTMM_MAST~WERKS = 'P001' AND "Plant
*         ZTMM_MAST~MATNR = LTAP~MATNR "Material
*    WHERE LTAK~BDATU GE L_BDATU.   "Group
*  IF SY-SUBRC EQ 0.
*    REFRESH T_LIST. CLEAR T_LIST.
*    LOOP AT LT_LIST.
*      MOVE-CORRESPONDING LT_LIST TO T_LIST.
*      WRITE: LT_LIST-NSOLM TO T_LIST-NSOLM UNIT LT_LIST-MEINS NO-ZERO,
*             LT_LIST-NSOLA TO T_LIST-NSOLA UNIT LT_LIST-MEINS NO-ZERO.
*      APPEND T_LIST. CLEAR T_LIST.
*    ENDLOOP.
*    REFRESH LT_LIST. CLEAR LT_LIST.
*  ENDIF.

  SELECT LGNUM
         LGTYP
         LTYPT
       FROM T301T
       INTO TABLE T_T301
       WHERE LGNUM EQ 'P01'
       AND   SPRAS EQ SY-LANGU.
  IF SY-SUBRC EQ 0.
    SORT T_T301 BY LGNUM LGTYP.
  ENDIF.

ENDFUNCTION.

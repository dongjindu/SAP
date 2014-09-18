************************************************************************
* Program Name      : ZRRF_MM_MIDDLEWARE_DATA_UPLOAD
* Author            : Bongsoo, Kim
* Creation Date     : 2004.08.13.
* Specifications By : Bongsoo, Kim
* Development Request No : UD1K911256
* Addl Documentation:
* Description       : Middleware synchronization SAP data
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 02/01/2005  Shiva       UD1K914116   Added Feeder stop in the
*                                       internal table it_list.
************************************************************************
REPORT ZRRF_MM_MIDDLEWARE_DATA_UPLOAD
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID ZMRF.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: ZSRF_PICKER_LIST,
        ZTRF_ID_PASS,
        ZTRF_SUPPLY_LINE,
        ZTRF_MW_ER_LOG.
*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_MLGT OCCURS 0,
        MATNR TYPE MLGT-MATNR,
*          LGNUM TYPE MLGT-LGNUM,
        LGTYP TYPE MLGT-LGTYP,
        RDMNG TYPE MLGT-RDMNG,
      END OF IT_MLGT.
DATA: IT_SUPPLY LIKE ZTRF_SUPPLY_LINE OCCURS 0 WITH HEADER LINE,
      IT_LIST   LIKE ZSRF_PICKER_LIST OCCURS 0 WITH HEADER LINE,
      IT_IDPW   LIKE ZTRF_ID_PASS     OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*
* DATA
*----------------------------------------------------------------------*
DATA: WA_BOXQT1 TYPE P DECIMALS 5,
      WA_BOXQT2 TYPE I,
      WA_DATUM  TYPE SY-DATUM,
      WA_UZEIT  TYPE SY-UZEIT.
DATA : C_DEST1(10) VALUE 'XLINK01',   "Outbound Interface Destination
       C_DEST2(10) VALUE 'XLINK02'.   "Outbound Interface Destination
*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
PARAMETERS P_CHK NO-DISPLAY DEFAULT 'X'.

*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM READ_PROCESS.
  PERFORM DATA_PROCESS.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM READ_PROCESS.
  CHECK NOT P_CHK IS INITIAL.
* ROUNDING QTY SELECTION
  PERFORM READ_MLGT.
* T/O SELECTION
  PERFORM LTAK_LTAP_ZTMM_MAST.
*** CBO TABLE DATA SELECTION
**  PERFORM READ_ZTRF_SUPPLY_LINE.
* LOGIN DATA
  PERFORM READ_ZTRF_ID_PASS.

ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  LTAK_LTAP_ZTMM_MAST
*&---------------------------------------------------------------------*
FORM LTAK_LTAP_ZTMM_MAST.
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
          zzfstp type zsrf_picker_list-zzfstp,
          PVQUI TYPE ZSRF_PICKER_LIST-PVQUI,
          EXECUTED_FLG TYPE ZSRF_PICKER_LIST-EXECUTED_FLG,
          CHK TYPE ZSRF_PICKER_LIST-CHK,
        END OF LT_LIST.
  DATA: L_DATUM TYPE SY-DATUM,
        L_UZEIT TYPE SY-UZEIT.
  CLEAR SY-SUBRC.
  L_DATUM = SY-DATUM.
*  L_UZEIT = SY-UZEIT.
*  L_UZEIT(2) = L_UZEIT(2) + 4.
*  IF L_UZEIT(2) GE 24.
*   L_DATUM+6(02) = L_DATUM+6(02) + 1.
*   L_UZEIT(2) = L_UZEIT(2) - 24.
*  ENDIF.
*  EXIT.
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
         ZTMM_MAST~ZZFSTP " Feeder stop
    INTO CORRESPONDING FIELDS OF TABLE LT_LIST
    FROM LTAK
    INNER JOIN LTAP
      ON LTAP~LGNUM = LTAK~LGNUM AND   "Warehouse Number
         LTAP~TANUM = LTAK~TANUM AND   "T/O number
         LTAP~PQUIT = SPACE "Open TO(Indicator: confirmation complete)
    INNER JOIN MAKT                    "For desc of matnr
      ON MAKT~SPRAS = SY-LANGU AND     "Language
         MAKT~MATNR = LTAP~MATNR       "Material
    LEFT OUTER JOIN ZTMM_MAST
      ON ZTMM_MAST~WERKS = 'P001' AND  "Plant
         ZTMM_MAST~MATNR = LTAP~MATNR  "Material
    WHERE LTAK~STDAT LE L_DATUM. "Start date transfer order
  IF SY-SUBRC EQ 0.
    REFRESH IT_LIST. CLEAR IT_LIST.
    LOOP AT LT_LIST.
      MOVE-CORRESPONDING LT_LIST TO IT_LIST.
      CASE LT_LIST-SPPTL.
        WHEN 'S'. IT_LIST-ZTCDH = 'TH'.  "houry
        WHEN 'N'. IT_LIST-ZTCDH = 'TD'.  "Daily
      ENDCASE.
      WRITE: LT_LIST-NSOLM TO IT_LIST-NSOLM UNIT LT_LIST-MEINS
                                            LEFT-JUSTIFIED NO-ZERO.
      CLEAR: WA_BOXQT1, WA_BOXQT2.
      READ TABLE IT_MLGT WITH KEY MATNR = LT_LIST-MATNR
                                  LGTYP = LT_LIST-VLTYP
                         BINARY SEARCH TRANSPORTING RDMNG.
      IF SY-SUBRC EQ 0.
        IF IT_MLGT-RDMNG NE 0.
          WA_BOXQT1 = LT_LIST-NSOLA / IT_MLGT-RDMNG.
          WA_BOXQT2 = CEIL( WA_BOXQT1 ).
        ELSE.
          WA_BOXQT2 = 1.
        ENDIF.
      ELSE.
        WA_BOXQT2 = 1.
      ENDIF.
      WRITE: WA_BOXQT2 TO IT_LIST-NSOLA LEFT-JUSTIFIED NO-ZERO.
      APPEND IT_LIST. CLEAR IT_LIST.
    ENDLOOP.
    REFRESH LT_LIST. CLEAR LT_LIST.
  ENDIF.
ENDFORM.                    " LTAK_LTAP_ZTMM_MAST
*&---------------------------------------------------------------------*
*&      Form  READ_MLGT
*&---------------------------------------------------------------------*
FORM READ_MLGT.
* SELECT MLGT Rounding qty(RDMNG)
  SELECT MATNR
         LGTYP
         RDMNG
       FROM MLGT
       INTO TABLE IT_MLGT
       WHERE LVORM EQ SPACE
       AND   LGNUM EQ 'P01'.
  IF SY-SUBRC EQ 0.
    SORT IT_MLGT BY MATNR LGTYP.
  ENDIF.
ENDFORM.                    " READ_MLGT
*&---------------------------------------------------------------------*
*&      Form  READ_ZTRF_SUPPLY_LINE
*&---------------------------------------------------------------------*
FORM READ_ZTRF_SUPPLY_LINE.
  DATA L_UZEIT TYPE SY-UZEIT.
  WA_DATUM = SY-DATUM.
  WA_UZEIT = SY-UZEIT.
  WA_UZEIT(2) = WA_UZEIT(2) + 4.

  SELECT *
       FROM ZTRF_SUPPLY_LINE
       INTO TABLE IT_SUPPLY
       WHERE STDAT EQ WA_DATUM
       AND   STUZT GE WA_UZEIT.
  IF SY-SUBRC EQ 0.
    CLEAR: IT_SUPPLY, IT_LIST.
    LOOP AT IT_SUPPLY.
      MOVE-CORRESPONDING IT_SUPPLY TO IT_LIST.
      APPEND IT_LIST. CLEAR IT_LIST.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " READ_ZTRF_SUPPLY_LINE
*&---------------------------------------------------------------------*
*&      Form  READ_ZTRF_ID_PASS
*&---------------------------------------------------------------------*
FORM READ_ZTRF_ID_PASS.
  SELECT *
       FROM ZTRF_ID_PASS
       INTO TABLE IT_IDPW.
  IF SY-SUBRC EQ 0.
    SORT IT_IDPW BY PERNR.
  ENDIF.

ENDFORM.                    " READ_ZTRF_ID_PASS
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM DATA_PROCESS.
  CHECK NOT P_CHK IS INITIAL.
  DATA: L_MSGTXT(80),
        L_LINES TYPE SY-TABIX,
        L_LINES1 TYPE SY-TABIX,
        L_MOD   TYPE I,
        L_TIMES TYPE I,
*        L_TOTAL TYPE NUM9,
*        L_CURR  TYPE NUM9,
        L_TOTAL TYPE CHAR9,
        L_CURR  TYPE CHAR9,
        L_FROM  TYPE I VALUE '0',
        L_INDEX TYPE SY-INDEX,
        L_TO    TYPE I VALUE '0',
        L_DIV   TYPE I VALUE '40'.
  DATA: LT_LIST  LIKE ZSRF_PICKER_LIST OCCURS 0 WITH HEADER LINE,
        LT_ERROR LIKE ZTRF_MW_ER_LOG OCCURS 0 WITH HEADER LINE.

  CLEAR: L_MSGTXT, L_MOD, L_TIMES.
  IF NOT IT_LIST[] IS INITIAL.
    SORT IT_LIST BY ZZFSTP.
    DESCRIBE TABLE IT_LIST LINES L_LINES.
    DESCRIBE TABLE IT_IDPW LINES L_LINES1.
    WRITE: L_LINES TO L_TOTAL LEFT-JUSTIFIED,
           L_LINES1 TO L_CURR LEFT-JUSTIFIED.

*     MIDDLEWARE SERVER 1
    CALL FUNCTION 'Z_FRF_MM_MIDDLEWARE_DATA_LIST'
      DESTINATION  C_DEST1
      IMPORTING
        IDPW_COUNT   = L_TOTAL                  " T_IDPW COUNT
        LIST_COUNT   = L_CURR                   " T_LIST COUNT
      TABLES
        T_IDPW        = IT_IDPW
        T_LIST        = IT_LIST
      EXCEPTIONS
        COMMUNICATION_FAILURE = 1  MESSAGE L_MSGTXT
        SYSTEM_FAILURE        = 2  MESSAGE L_MSGTXT.
*          OTHERS                = 3  MESSAGE L_MSGTXT.
    IF SY-SUBRC NE 0.
      LT_ERROR-MANDT = SY-MANDT.
      LT_ERROR-DATUM = SY-DATUM.
      LT_ERROR-UZEIT = SY-UZEIT.
      CONCATENATE C_DEST1 ' : ' 'PICKER & FEEDER' INTO LT_ERROR-MIDWA.
*      LT_ERROR-MIDWA = C_DEST1.
      LT_ERROR-MESSA = L_MSGTXT.
      APPEND LT_ERROR. CLEAR LT_ERROR.
    ENDIF.
*     MIDDLEWARE SERVER 2
    CLEAR: L_MSGTXT.
    SORT IT_LIST BY ZZFSTP.
    CALL FUNCTION 'Z_FRF_MM_MIDDLEWARE_DATA_LIST'
      DESTINATION  C_DEST2
      IMPORTING
        IDPW_COUNT   = L_TOTAL                  " T_IDPW COUNT
        LIST_COUNT   = L_CURR                   " T_LIST COUNT
      TABLES
        T_IDPW        = IT_IDPW
        T_LIST        = IT_LIST
      EXCEPTIONS
        COMMUNICATION_FAILURE = 1  MESSAGE L_MSGTXT
        SYSTEM_FAILURE        = 2  MESSAGE L_MSGTXT.
    IF SY-SUBRC NE 0.
      LT_ERROR-MANDT = SY-MANDT.
      LT_ERROR-DATUM = SY-DATUM.
      LT_ERROR-UZEIT = SY-UZEIT.
      CONCATENATE C_DEST2 ' : ' 'PICKER & FEEDER' INTO LT_ERROR-MIDWA.
*      LT_ERROR-MIDWA = C_DEST2.
      LT_ERROR-MESSA = L_MSGTXT.
      APPEND LT_ERROR. CLEAR LT_ERROR.
    ENDIF.
  ENDIF.
  IF NOT LT_ERROR[] IS INITIAL.
    INSERT ZTRF_MW_ER_LOG CLIENT SPECIFIED FROM TABLE LT_ERROR
                          ACCEPTING DUPLICATE KEYS .
    IF SY-SUBRC EQ 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDIF.
ENDFORM.                    " DATA_PROCESS

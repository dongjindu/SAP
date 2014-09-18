************************************************************************
* Program Name      : ZAPP715U_COMPARE_BOM_BATCH
* Author            : JongOh, Kim
* Creation Date     : 2003.12.09
* Specifications By : JongOh, Kim
* Pattern           : 2.1
* Development Request No : UD1K901977
* Addl Documentation:
* Description       : COMPARING Plnd Order BOM to Standard BOM
*
* Modification Logs
* Date        Developer    RequestNo    Description
* 2003.12.11  jokim        UD1K904837   Released during programming
* 2003.12.13  jokim        UD1K904945   Released during programming
*
************************************************************************

REPORT ZAPP715U_COMPARE_BOM_BATCH  NO STANDARD PAGE HEADING
                                   LINE-SIZE 1023
                                   MESSAGE-ID ZMPP.

TYPE-POOLS: M61X.
*----------------------------------------------------------------------*
*  EXTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
TABLES : PLAF,      "Planned order
         CABN,      "Characteristic
         AUSP.      "Characteristic Values

TABLES : T001W,     "Plants/Branches
         T399D,     "Control Parameters for MRP
         MT61D,     "Material Master: MRP
         CM61M,     "MRP Control Indicator -Material Level-
         CM61W,     "MRP Control Indicator -Plant Level-
         CM61X,     "MRP Control Indicator -Transaction Level-
         MDPA.      "View of planned order/dummy conponents for BOM expl

*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
*DATA: IT_PLAF  LIKE  TABLE OF PLAF WITH HEADER LINE.
*--> Dispobereich
DATA: CM61B    TYPE M61X_CM61B.

DATA: BEGIN OF IT_PLAF OCCURS 0,
       PLWRK   TYPE  PLAF-PLWRK,    "Plant
       MATNR   TYPE  PLAF-MATNR,    "FSC
       STALT   TYPE  PLAF-STALT,    "Alternative BOM
       STLAN   TYPE  PLAF-STLAN,    "BOM Usage
       VERID   TYPE  PLAF-VERID,    "Prod Version
       PSTTR   TYPE  PLAF-PSTTR,    "Order start date in planned order
       RSNUM   TYPE  PLAF-RSNUM,    "No of Reservation Req
       PLNUM   TYPE  PLAF-PLNUM.    "Planned order number
DATA: END OF IT_PLAF.

*--> BOM EXPLOSION
DATA: IT_EXPBOM   LIKE  TABLE OF MDPM  WITH HEADER LINE.
*--> Planned Order BOM
DATA: IT_EXPRESB  LIKE  TABLE OF MDPM  WITH HEADER LINE.
*--> Temparary
DATA: IT_TEMPBOM  LIKE  TABLE OF MDPM  WITH HEADER LINE.

DATA: IT_RESB     LIKE  TABLE OF RESB  WITH HEADER LINE.

*--> Comparing Table
DATA: IT_COMPARE  LIKE  TABLE OF ZTPP_COMPPARTVIN WITH HEADER LINE.
*DATA: IT_STPOX LIKE  TABLE OF STPOX WITH HEADER LINE.
*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
DATA: WA_PLAF_DS    TYPE  PLAF.
DATA: WA_PLNUM      TYPE  PLAF-PLNUM.    "Planned Order
DATA: WA_ATINN      TYPE  CABN-ATINN,
      WA_ATFOR      TYPE  CABN-ATFOR.

DATA: WA_OBJEK      TYPE  AUSP-OBJEK,    "Vehicle Master
      WA_ATWRT      TYPE  AUSP-ATWRT,    "P_RP_STATUS
      WA_RPOINT     TYPE  N,             "P_RP_STATUS No
      WA_WORDR      TYPE  AUSP-ATWRT,    "P_WORK_ORDER
      WA_EXTC       TYPE  AUSP-ATWRT,    "P_EXT_COLOR
      WA_INTC       TYPE  AUSP-ATWRT,    "P_INT_COLOR
      WA_SEQDAT     TYPE  ZTPP_COMPPARTVIN-SEQDAT,  "SEQ DATE
      WA_SEQNO      TYPE  ZTPP_COMPPARTVIN-SEQNO.   "SEQ No

DATA: WA_PSTTR_FLG.
*----------------------------------------------------------------------*
*  SELECTION SCREEN DECLARATION
*----------------------------------------------------------------------*
SELECT-OPTIONS : S_PSTTR FOR PLAF-PSTTR,
                 S_PLNUM FOR PLAF-PLNUM.

************************************************************************
* INITIALIZAION
************************************************************************
INITIALIZATION.
*  PERFORM INITIALIZATION.

************************************************************************
* AT SELECTION-SCREEN
************************************************************************
AT SELECTION-SCREEN.
  PERFORM AT_SELECTION-SCREEN.

************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
  PERFORM EXCUTE_PROCESS.

************************************************************************
* END-OF-SELECTION
************************************************************************
END-OF-SELECTION.
  PERFORM LIST_PROCESS.


*&---------------------------------------------------------------------*
*&      Form  EXCUTE_PROCESS
*&---------------------------------------------------------------------*
FORM EXCUTE_PROCESS.
  CLEAR : WA_ATINN, WA_ATFOR.

  WA_PSTTR_FLG = 'Y'.
  SET PARAMETER ID 'ZBAT' FIELD WA_PSTTR_FLG.

  PERFORM WRITE_START.

*----> SELECT PLAF In condition
  PERFORM SELECT_PLAF.

*----> READ Characteristic number of Planned Order
  PERFORM SELECT_CABN USING 'P_PLAN_ORDER'
                      CHANGING WA_ATINN WA_ATFOR.

*----> COMPARE Planned order BOM with Standard BOM
  PERFORM COMPARING_EXPLOSION_BOM.

ENDFORM.                    " EXCUTE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  WRITE_START
*&---------------------------------------------------------------------*
FORM WRITE_START.
  WRITE :/ '********** BEGIN OF PROCESS ***********'.
  WRITE AT: /001(030) 'Processing Time...(Start)' ,
             031(010) SY-DATUM                    ,
             042(010) SY-UZEIT                    .
  WRITE :/ 'Comparing Planned Order BOM to Standard BOM'.
  WRITE :/ '********** BEGIN OF PROCESS ***********'.
  SKIP 2.
ENDFORM.                    " WRITE_START

*&---------------------------------------------------------------------*
*&      Form  SELECT_CABN
*&---------------------------------------------------------------------*
FORM SELECT_CABN USING P_ATNAM TYPE CABN-ATNAM
                 CHANGING P_ATINN TYPE CABN-ATINN
                          P_ATFOR TYPE CABN-ATFOR.
  SELECT SINGLE ATINN
                ATFOR
              INTO (P_ATINN, P_ATFOR)
              FROM CABN
              WHERE ATNAM EQ P_ATNAM.

ENDFORM.                    " SELECT_CABN
*&---------------------------------------------------------------------*
*&      Form  SELECT_OBJEK
*&---------------------------------------------------------------------*
FORM SELECT_OBJEK USING P_ATINN P_KLART P_ATWRT P_ATFOR
                 CHANGING P_OBJEK.

  DATA : L_NUM(10)  TYPE  N,
         L_INT      TYPE  I,
         L_ATFLV    TYPE  AUSP-ATFLV.

  IF P_ATFOR EQ 'CHAR'.
    SELECT SINGLE OBJEK
                 INTO P_OBJEK
                 FROM AUSP
                 WHERE ATINN EQ P_ATINN
                   AND KLART EQ P_KLART
                   AND ATWRT EQ P_ATWRT.
  ELSE.
    L_NUM = P_ATWRT.
    L_INT = L_NUM.
    L_ATFLV = L_INT.
    SELECT SINGLE OBJEK
                 INTO P_OBJEK
                 FROM AUSP
                 WHERE ATINN EQ P_ATINN
                   AND KLART EQ P_KLART
                   AND ATFLV EQ L_ATFLV.
  ENDIF.
ENDFORM.                    " SELECT_OBJEK
*&---------------------------------------------------------------------*
*&      Form  SELECT_AUSP
*&---------------------------------------------------------------------*
FORM SELECT_AUSP USING    P_OBJEK P_ATINN P_KLART P_ATFOR
                 CHANGING P_ATWRT.
  DATA : L_NUM(10)  TYPE  N,
         L_INT      TYPE  I,
         L_ATFLV    TYPE  AUSP-ATFLV,
         L_ATWRT    TYPE  AUSP-ATWRT.

  SELECT SINGLE ATWRT
                ATFLV
               INTO (L_ATWRT, L_ATFLV)
               FROM AUSP
               WHERE OBJEK EQ P_OBJEK
                 AND ATINN EQ P_ATINN
                 AND KLART EQ P_KLART.
  IF P_ATFOR EQ 'CHAR'.
    P_ATWRT = L_ATWRT.
  ELSE.
    L_NUM = L_ATFLV.
    L_INT = L_NUM.
    WRITE L_INT TO P_ATWRT LEFT-JUSTIFIED NO-GROUPING.
  ENDIF.

ENDFORM.                    " SELECT_AUSP
*&---------------------------------------------------------------------*
*&      Form  SELECT_CLASSIFICATION
*&---------------------------------------------------------------------*
FORM SELECT_CLASSIFICATION USING P_ATNAM
                           CHANGING P_ATWRT.
  DATA : L_ATINN   TYPE  CABN-ATINN,
         L_ATFOR   TYPE  CABN-ATFOR.

  PERFORM SELECT_CABN USING P_ATNAM
                    CHANGING L_ATINN L_ATFOR.
  PERFORM SELECT_AUSP USING WA_OBJEK L_ATINN '002' L_ATFOR
                    CHANGING P_ATWRT.

ENDFORM.                    " SELECT_CLASSIFICATION
*&---------------------------------------------------------------------*
*&      Form  SELECT_PLAF
*&---------------------------------------------------------------------*
FORM SELECT_PLAF.
  CLEAR : IT_PLAF, IT_PLAF[].
  SELECT
         PLWRK       "Plant
         MATNR       "FSC
         STALT       "Alternative BOM
         STLAN       "BOM Usage
         VERID       "Prod Version
         PSTTR       "Order start date in planned order
         RSNUM       "No of Reservation Req
         PLNUM       "Planned order number
         INTO TABLE IT_PLAF
         FROM PLAF
         WHERE PAART EQ 'PE'         "Order Type
           AND BESKZ EQ 'E'          "Procurement Type
           AND SOBES EQ 'E'          "Special procurement type
           AND KNTTP EQ 'M'          "Acct.assgt.category
           AND PSTTR IN S_PSTTR      "Planned Start date
           AND PLNUM IN S_PLNUM.     "Planned Order No

  SORT IT_PLAF BY PLWRK MATNR STALT STLAN VERID PSTTR.

ENDFORM.                    " SELECT_PLAF
*&---------------------------------------------------------------------*
*&      Form  BOM_EXPLOSION
*&---------------------------------------------------------------------*
FORM BOM_EXPLOSION.
  CLEAR WA_PLAF_DS.
  SELECT SINGLE *
     INTO WA_PLAF_DS
     FROM PLAF
     WHERE PLNUM EQ WA_PLNUM.

*-----> Materialstamm lesen
  PERFORM READ_MT61D_CM61B.

*-----> READ MRP Control Indicator -Plant Level-
  PERFORM READ_CM61W.

*-----> READ MRP Control Indicator -Material Level-
  PERFORM READ_CM61M.

*-----> READ BOM
  PERFORM MD_BOM_EXPLOSION.
ENDFORM.                    " BOM_EXPLOSION
*&---------------------------------------------------------------------*
*&      Form  READ_MT61D_CM61B
*&---------------------------------------------------------------------*
FORM READ_MT61D_CM61B.
  CLEAR : MT61D, CM61B.
  CALL FUNCTION 'MD_READ_MATERIAL_SIMPLE'
       EXPORTING
            EWERKS                      = IT_PLAF-PLWRK
            EMATNR                      = IT_PLAF-MATNR
            EBERID                      = WA_PLAF_DS-BERID
       CHANGING
            CMT61D                      = MT61D
            CCM61B                      = CM61B
       EXCEPTIONS
            MATERIAL_MRP_AREA_NOT_FOUND = 1
            MISSING_PARAMETER           = 2
            MATERIAL_PLANT_NOT_FOUND    = 3
            MATERIAL_MRP_AREA_DELETED   = 4
            OTHERS                      = 5.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " READ_MT61D_CM61B

*&---------------------------------------------------------------------*
*&      Form  READ_CM61W
*&---------------------------------------------------------------------*
FORM READ_CM61W.
  CLEAR : T001W, CM61W.
  SELECT SINGLE *
             FROM T001W
             WHERE WERKS EQ MT61D-WERKS.

  MOVE T001W-FABKL  TO  CM61W-FABKL.

  SELECT SINGLE *
             FROM T399D
             WHERE WERKS EQ MT61D-WERKS.
  MOVE T399D-KASTL TO CM61W-KASTL.
  MOVE T399D-GTERM TO CM61W-GTERM.
  MOVE T399D-KZMAL TO CM61W-KZMAL.
  MOVE T399D-KZDRB TO CM61W-KZDRB.
*----> Sichern aktuelles Werk
  MOVE MT61D-WERKS TO CM61W-PLWRK.

ENDFORM.                    " READ_CM61W
*&---------------------------------------------------------------------*
*&      Form  READ_CM61M
*&---------------------------------------------------------------------*
FORM READ_CM61M.
  DATA: WA_CM61X   LIKE  CM61X.
  CLEAR : WA_CM61X.
  MOVE PLAF-PLSCN  TO WA_CM61X-PLSCN.

  CALL FUNCTION 'CM61M_ERSTELLEN'
       EXPORTING
            ECM61W = CM61W
            ECM61X = WA_CM61X
            EMT61D = MT61D
            ET399D = T399D
            ECM61B = CM61B
       IMPORTING
            ICM61M = CM61M.
ENDFORM.                    " READ_CM61M
*&---------------------------------------------------------------------*
*&      Form  MD_BOM_EXPLOSION
*&---------------------------------------------------------------------*
FORM MD_BOM_EXPLOSION.
  DATA : L_CSLID   TYPE AF61Z-SELID,
         L_NOSCRAP TYPE XFELD,
         L_SBFLG   TYPE CM61X-SBFLG.

  CLEAR : IT_EXPBOM, IT_EXPBOM[].

  MOVE T399D-CSLID TO L_CSLID.
  MOVE SPACE       TO L_NOSCRAP.
  MOVE SPACE       TO L_SBFLG.

  CLEAR WA_PLAF_DS-PLNUM.
  CALL FUNCTION 'MD_AUFLOESUNG_PLANAUFTRAG'
       EXPORTING
            EPLAF     = WA_PLAF_DS
            EMT61D    = MT61D
            ESELID    = L_CSLID
            ECM61M    = CM61M
            ECM61B    = CM61B
            ENO_SCRAP = L_NOSCRAP
            ESBFLG    = L_SBFLG
       IMPORTING
            IPLAF     = WA_PLAF_DS
       TABLES
            MDPMX     = IT_EXPBOM.

  SORT IT_EXPBOM BY AUFST
                    AUFWG
                    BAUST
                    XPOSN
                    PMSO1
                    EPOSN
                    STSOR.
ENDFORM.                    " MD_BOM_EXPLOSION
*&---------------------------------------------------------------------*
*&      Form  PLAN_BOM_EXPLOSION
*&---------------------------------------------------------------------*
FORM PLAN_BOM_EXPLOSION.
**----> SELECT RESB with Quantity withdrawn
*  CLEAR : IT_RESB, IT_RESB[].
*  SELECT *
*        INTO TABLE IT_RESB
*        FROM RESB
*        WHERE RSNUM EQ IT_PLAF-RSNUM
*          AND ENMNG GT 0.

*----> Reservation/dependent requirements
  CLEAR : PLAF, MDPA.
  SELECT SINGLE *
             FROM PLAF
             WHERE PLNUM EQ IT_PLAF-PLNUM.
  MOVE-CORRESPONDING PLAF TO MDPA.
  MOVE PLAF-MEINS         TO MDPA-LAGME.
  CALL FUNCTION 'MD_LESEN_KOMPONENTEN'
       EXPORTING
            EMDPA = MDPA
       IMPORTING
            IMDPA = MDPA
       TABLES
            MDPMX = IT_EXPRESB.

  SORT IT_EXPRESB BY AUFST
                     AUFWG
                     BAUST
                     XPOSN
                     PMSO1
                     EPOSN
                     STSOR.
ENDFORM.                    " PLAN_BOM_EXPLOSION
*&---------------------------------------------------------------------*
*&      Form  COMPARING_EXPLOSION_BOM
*&---------------------------------------------------------------------*
FORM COMPARING_EXPLOSION_BOM.
  DATA : L_TABIX  TYPE  SY-TABIX,
         L_ATWRT  TYPE  AUSP-ATWRT.

  CLEAR: IT_COMPARE, IT_COMPARE[].

  LOOP AT IT_PLAF.
    CLEAR : WA_RPOINT, WA_ATWRT, WA_WORDR, WA_EXTC, WA_INTC,
            WA_SEQDAT, WA_SEQNO.

    L_TABIX  = SY-TABIX.
    MOVE IT_PLAF-PLNUM    TO    WA_PLNUM.

    AT NEW PSTTR.
*-----> STANDARD BOM Explosion on Planned order start date condition
      PERFORM BOM_EXPLOSION.
    ENDAT.

*----> Read Vehicle Master
    CLEAR WA_OBJEK.
    PERFORM SELECT_OBJEK USING  WA_ATINN '002' IT_PLAF-PLNUM WA_ATFOR
                         CHANGING WA_OBJEK.

    IF SY-SUBRC NE 0.
      DELETE IT_PLAF INDEX L_TABIX.
    ELSE.
*----> Check P_RP_STATUS
      PERFORM SELECT_CLASSIFICATION USING 'P_RP_STATUS'
                                    CHANGING WA_ATWRT.
      WA_RPOINT = WA_ATWRT.
      IF WA_RPOINT GE '00' AND WA_RPOINT LE '06'.
*----> READ W/O Header, Ext-color, Int-color , SEQ DATE, SEQ No
        PERFORM READ_CLASSIFICATION.

*----> Plan Order BOM Explosion
        PERFORM PLAN_BOM_EXPLOSION.
*----> Compare BOM with Planned Order BOM
        PERFORM COMPARE_BOM.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " COMPARING_EXPLOSION_BOM
*&---------------------------------------------------------------------*
*&      Form  COMPARE_BOM
*&---------------------------------------------------------------------*
FORM COMPARE_BOM.
  CLEAR : IT_TEMPBOM, IT_TEMPBOM[].
  IT_TEMPBOM[] = IT_EXPBOM[].

*----> Comparing Standard Explosion BOM to Planned order Explosion BOM
  PERFORM COMPARE_STDBOM_TO_PLNBOM.

*----> Comparing Planned order Explosion BOM to Standard Explosion BOM
  PERFORM COMPARE_PLNBOM_TO_STDBOM.

ENDFORM.                    " COMPARE_BOM
*&---------------------------------------------------------------------*
*&      Form  READ_CLASSIFICATION
*&---------------------------------------------------------------------*
FORM READ_CLASSIFICATION.
*----> WORK ORDER HEAD
  PERFORM SELECT_CLASSIFICATION USING 'P_WORK_ORDER'
                                CHANGING WA_WORDR.
*----> WORK ORDER EXT COLOR
  PERFORM SELECT_CLASSIFICATION USING 'P_EXT_COLOR'
                                CHANGING WA_EXTC.
*----> WORK ORDER INT COLOR
  PERFORM SELECT_CLASSIFICATION USING 'P_INT_COLOR'
                                CHANGING WA_INTC.
*----> SEQUENCE DATE
  PERFORM SELECT_CLASSIFICATION USING 'P_SEQUENCE_DATE'
                                CHANGING WA_SEQDAT.
*----> SEQUENCE NO
  PERFORM SELECT_CLASSIFICATION USING 'P_SEQUENCE_SERIAL'
                                CHANGING WA_SEQNO.

ENDFORM.                    " READ_CLASSIFICATION
*&---------------------------------------------------------------------*
*&      Form  COMPARE_STDBOM_TO_PLNBOM
*&---------------------------------------------------------------------*
FORM COMPARE_STDBOM_TO_PLNBOM.
  DATA : L_TEMPBOM_IX     TYPE   SY-TABIX,
         L_EXPRESB_IX     TYPE   SY-TABIX.
*----> Comparing Standard Explosion BOM to Planned order Explosion BOM
  LOOP AT IT_TEMPBOM.
    L_TEMPBOM_IX = SY-TABIX.
    READ TABLE IT_EXPRESB
                 WITH KEY AENNR = IT_TEMPBOM-AENNR  "Change No
                          BAUGR = IT_TEMPBOM-BAUGR  "Parent HALB
                          MATNR = IT_TEMPBOM-MATNR. "Part No
    IF SY-SUBRC NE 0.
      CONCATENATE WA_EXTC WA_INTC  INTO IT_COMPARE-COLOR.
      MOVE : SY-MANDT     TO    IT_COMPARE-MANDT,  "
             PLAF-MATNR   TO    IT_COMPARE-MATNR,  "Full Spec Code
             PLAF-VERID   TO    IT_COMPARE-VERID,  "Production version
             WA_OBJEK     TO    IT_COMPARE-EQUNR,  "Vehicle Master
             PLAF-PLNUM   TO    IT_COMPARE-PLNUM,  "Planned order
             'S'          TO    IT_COMPARE-APGRP,  "Gubun
             WA_WORDR     TO    IT_COMPARE-WORDR,  "W/O Header
*             WA_EXTC      TO    IT_COMPARE-EXTC,   "Ext-color
*             WA_INTC      TO    IT_COMPARE-INTC,   "Int-color
             IT_TEMPBOM-BAUST TO IT_COMPARE-BAUST,  "Assembly order lev
             IT_TEMPBOM-BAUWG TO IT_COMPARE-BAUWG,  "Assembly order path
             IT_TEMPBOM-POSNR TO IT_COMPARE-POSNR,  "BOM Item No
             IT_TEMPBOM-MATNR TO IT_COMPARE-PARTOLD, "Disagreement
             IT_TEMPBOM-BAUGR TO IT_COMPARE-BAUGR,  "Parent HALB
             IT_TEMPBOM-ERFMG TO IT_COMPARE-ERFMG,  "Req qty component
             IT_TEMPBOM-ERFME TO IT_COMPARE-ERFME,  "Unit of entry
             IT_TEMPBOM-AENNR TO IT_COMPARE-AENNR,  "Change No
             IT_TEMPBOM-LGPRO TO IT_COMPARE-LGPRO,  "Issue Strage Loca
             IT_TEMPBOM-PRVBE TO IT_COMPARE-PRVBE,  "Supply Area
*             IT_TEMPBOM-RSNUM TO IT_COMPARE-RSNUM,  "RESB No
*             IT_TEMPBOM-RSPOS TO IT_COMPARE-RSPOS,  "POS No
*             IT_TEMPBOM-RSART TO IT_COMPARE-RSART,  "Record type
             PLAF-PSTTR       TO IT_COMPARE-PSTTR,  "Order start date
             WA_SEQDAT        TO IT_COMPARE-SEQDAT, "SEQUENCE DATE
             WA_SEQNO         TO IT_COMPARE-SEQNO.  "SEQUENCE NO

      APPEND IT_COMPARE.  CLEAR IT_COMPARE.
    ELSE.
      L_EXPRESB_IX = SY-TABIX.
      DELETE IT_TEMPBOM   INDEX   L_TEMPBOM_IX.
      DELETE IT_EXPRESB   INDEX   L_EXPRESB_IX.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " COMPARE_STDBOM_TO_PLNBOM
*&---------------------------------------------------------------------*
*&      Form  COMPARE_PLNBOM_TO_STDBOM
*&---------------------------------------------------------------------*
FORM COMPARE_PLNBOM_TO_STDBOM.
  DATA : L_TEMPBOM_IX     TYPE   SY-TABIX,
         L_EXPRESB_IX     TYPE   SY-TABIX.
*----> Comparing Planned order Explosion BOM to Standard Explosion BOM
  LOOP AT IT_EXPRESB.
    L_EXPRESB_IX = SY-TABIX.
    READ TABLE IT_TEMPBOM
                 WITH KEY AENNR = IT_EXPRESB-AENNR  "Change No
                          BAUGR = IT_EXPRESB-BAUGR  "Parent HALB
                          MATNR = IT_EXPRESB-MATNR. "Part No
    IF SY-SUBRC NE 0.
      CONCATENATE WA_EXTC WA_INTC  INTO IT_COMPARE-COLOR.
      MOVE : SY-MANDT     TO    IT_COMPARE-MANDT,  "
             PLAF-MATNR   TO    IT_COMPARE-MATNR,  "Full Spec Code
             PLAF-VERID   TO    IT_COMPARE-VERID,  "Production version
             WA_OBJEK     TO    IT_COMPARE-EQUNR,  "Vehicle Master
             PLAF-PLNUM   TO    IT_COMPARE-PLNUM,  "Planned order
             'P'          TO    IT_COMPARE-APGRP,  "Gubun
             WA_WORDR     TO    IT_COMPARE-WORDR,  "W/O Header
*             WA_EXTC      TO    IT_COMPARE-EXTC,   "Ext-color
*             WA_INTC      TO    IT_COMPARE-INTC,   "Int-color
             IT_EXPRESB-BAUST TO IT_COMPARE-BAUST,  "Assembly order lev
             IT_EXPRESB-BAUWG TO IT_COMPARE-BAUWG,  "Assembly order path
             IT_EXPRESB-POSNR TO IT_COMPARE-POSNR,  "BOM Item No
             IT_EXPRESB-MATNR TO IT_COMPARE-PARTOLD, "Disagreement PART
             IT_EXPRESB-BAUGR TO IT_COMPARE-BAUGR,  "Parent HALB
             IT_EXPRESB-ERFMG TO IT_COMPARE-ERFMG,  "Req qty component
             IT_EXPRESB-ERFME TO IT_COMPARE-ERFME,  "Unit of entry
             IT_EXPRESB-AENNR TO IT_COMPARE-AENNR,  "Change No
             IT_EXPRESB-LGPRO TO IT_COMPARE-LGPRO,  "Issue Strage Loca
             IT_EXPRESB-PRVBE TO IT_COMPARE-PRVBE,  "Supply Area
             IT_EXPRESB-RSNUM TO IT_COMPARE-RSNUM,  "RESB No
             IT_EXPRESB-RSPOS TO IT_COMPARE-RSPOS,  "POS No
             IT_EXPRESB-RSART TO IT_COMPARE-RSART,  "Record type
             PLAF-PSTTR       TO IT_COMPARE-PSTTR,  "Order start date
             WA_SEQDAT        TO IT_COMPARE-SEQDAT, "SEQUENCE DATE
             WA_SEQNO         TO IT_COMPARE-SEQNO.  "SEQUENCE NO
      APPEND IT_COMPARE.  CLEAR IT_COMPARE.
    ELSE.
      L_TEMPBOM_IX = SY-TABIX.
      DELETE IT_TEMPBOM   INDEX   L_TEMPBOM_IX.
      DELETE IT_EXPRESB   INDEX   L_EXPRESB_IX.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " COMPARE_PLNBOM_TO_STDBOM
*&---------------------------------------------------------------------*
*&      Form  LIST_PROCESS
*&---------------------------------------------------------------------*
FORM LIST_PROCESS.
  MOVE : SY-DATUM  TO  IT_COMPARE-ERDAT,
         SY-UZEIT  TO  IT_COMPARE-ERZET,
         SY-UNAME  TO  IT_COMPARE-ERNAM.
  MODIFY IT_COMPARE TRANSPORTING ERDAT ERZET ERNAM
          WHERE MANDT EQ SY-MANDT.
  DELETE FROM ZTPP_COMPPARTVIN
         CLIENT SPECIFIED
         WHERE MANDT EQ SY-MANDT.
  INSERT ZTPP_COMPPARTVIN FROM TABLE IT_COMPARE.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

  PERFORM WRITE_RESULT.
  CLEAR WA_PSTTR_FLG.
  SET PARAMETER ID 'ZBAT' FIELD WA_PSTTR_FLG.
  CALL FUNCTION 'DEQUEUE_EZPP_COMPPARTVIN'
       EXPORTING
            MODE_ZTPP_COMPPARTVIN = 'E'
            MANDT                 = SY-MANDT.

ENDFORM.                    " LIST_PROCESS
*&---------------------------------------------------------------------*
*&      Form  WRITE_RESULT
*&---------------------------------------------------------------------*
FORM WRITE_RESULT.
  SORT IT_COMPARE BY MATNR VERID EQUNR PLNUM BAUST BAUWG POSNR APGRP.
  LOOP AT IT_COMPARE.
    IF IT_COMPARE-APGRP EQ 'S'.
      FORMAT INTENSIFIED ON.
    ELSE.
      FORMAT INTENSIFIED OFF.
    ENDIF.
    WRITE:/ IT_COMPARE-MATNR COLOR COL_KEY,
            IT_COMPARE-VERID COLOR COL_KEY,
            IT_COMPARE-EQUNR COLOR COL_KEY,
            IT_COMPARE-PLNUM COLOR COL_TOTAL,
            IT_COMPARE-BAUST COLOR COL_KEY,
            IT_COMPARE-BAUWG COLOR COL_KEY,
            IT_COMPARE-POSNR COLOR COL_KEY,
            IT_COMPARE-APGRP COLOR COL_KEY,
            IT_COMPARE-PARTOLD COLOR COL_KEY,
            IT_COMPARE-BAUGR COLOR COL_TOTAL,
            IT_COMPARE-PARTNEW COLOR COL_KEY,
            IT_COMPARE-RP_POINT COLOR COL_NORMAL,
            IT_COMPARE-ERFMG COLOR COL_NORMAL,
            IT_COMPARE-ERFME COLOR COL_NORMAL,
            IT_COMPARE-WORDR COLOR COL_NORMAL,
            IT_COMPARE-COLOR COLOR COL_NORMAL,
*            IT_COMPARE-EXTC COLOR COL_NORMAL,
*            IT_COMPARE-INTC COLOR COL_NORMAL,
            IT_COMPARE-AENNR COLOR COL_NORMAL,
            IT_COMPARE-LGPRO COLOR COL_NORMAL,
            IT_COMPARE-PRVBE COLOR COL_NORMAL.
  ENDLOOP.

  FORMAT RESET INTENSIFIED ON.
  SKIP 2.
  WRITE :/ '********** END OF PROCESS ***********'.
  WRITE AT: /001(030) 'Processing Time...(Start)' ,
             031(010) SY-DATUM                    ,
             042(010) SY-UZEIT                    .
  WRITE :/ 'Comparing Planned Order BOM to Standard BOM'.
  WRITE :/ '********** END OF PROCESS ***********'.


ENDFORM.                    " WRITE_RESULT
*&---------------------------------------------------------------------*
*&      Form  AT_SELECTION-SCREEN
*&---------------------------------------------------------------------*
FORM AT_SELECTION-SCREEN.
*  PERFORM ENQUEUE_EZPP_COMPPARTVIN USING P_MATNR P_VERID.
  CALL FUNCTION 'ENQUEUE_EZPP_COMPPARTVIN'
       EXPORTING
            MODE_ZTPP_COMPPARTVIN = 'E'
            MANDT                 = SY-MANDT.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " AT_SELECTION-SCREEN

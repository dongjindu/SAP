************************************************************************
* Program Name      : ZACO05U_MHCC
* Author            : Hyung Jin Youn
* Creation Date     : 2003.10.14
* Specifications By : Dong-Hwan Kim
* Pattern           : Report 1-1
* Development Request No : UD1K902723
* Addl Documentation:
* Description       : This program will allocate variance M/Hs
*                     which are calculated from timesheet, B/F data
*                     and PCC data
*                     This program will generate data for posting,
*                     and post them using MFBF(REM Backflush) and
*                     Production order time ticket
* Modification Logs
*   Date       Developer    RequestNo    Description
*#1 03/09/2005 WSKIM        UD1K914876   WIP ERROR
*   11/06/2006 Manju        UD1K922927   Production fix
************************************************************************

*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
REPORT ZACO05U_MHCC_TEMP MESSAGE-ID ZMCO.
* type-pools
TYPE-POOLS: SLIS.

** Tables
TABLES : ZTCO_MHHRTRANS, TKA01, MARA, MARC, COSS, ZTCO_MHPCPOST,
         AUFK, BLPK, BLPP, MKAL, CAUFV, AFKO, AFPO, RMUSER_TAV,
         CKMLHD, MLCD.
TABLES : PPC_ACT, PPC_HEAD, PPC_SHOW_EXT, PPC_SHOW_EXT_ACT.


** Internal table
*DATA : IT_ZTCO_MHHRTRANS LIKE STANDARD TABLE OF ZTCO_MHHRTRANS
*                         WITH HEADER LINE .
DATA : BEGIN OF IT_ZTCO_MHHRTRANS  OCCURS 0,
        GJAHR   LIKE ZTCO_MHHRTRANS-GJAHR,
        PERID   LIKE ZTCO_MHHRTRANS-PERID,
        KOSTL   LIKE ZTCO_MHHRTRANS-KOSTL,
        LSTAR   LIKE ZTCO_MHHRTRANS-LSTAR,
        VAEQTY  LIKE ZTCO_MHHRTRANS-VAEQTY,
        UNIT    LIKE ZTCO_MHHRTRANS-UNIT,
       END OF  IT_ZTCO_MHHRTRANS.
* For MATNR
DATA : BEGIN OF IT_MARC OCCURS 0,
        MATNR   LIKE MARA-MATNR,
        MTART   LIKE MARA-MTART,
        WERKS   LIKE MARC-WERKS,
        SAUFT   LIKE MARC-SAUFT,
        SFEPR   LIKE MARC-SFEPR,
       END OF   IT_MARC.
* For Object Key
DATA : BEGIN OF IT_MA_OBJ OCCURS 0.
        INCLUDE STRUCTURE IT_MARC.
DATA :  AUFNR   LIKE AFPO-AUFNR,
        KOSTL   LIKE ANLP-KOSTL,
        LSTAR   LIKE CSLA-LSTAR,
        OBJNR   LIKE COSS-OBJNR,
        PAROB   LIKE COSS-PAROB,
        USPOB   LIKE COSS-USPOB,
        VERID   LIKE MKAL-VERID,
       END OF   IT_MA_OBJ.
* For Coss
DATA : BEGIN OF IT_COSS OCCURS 0.
DATA :  KOSTL   LIKE ANLP-KOSTL,
        LSTAR   LIKE CSLA-LSTAR,
        AUFNR   LIKE AFPO-AUFNR.
        INCLUDE STRUCTURE IT_MARC.
        INCLUDE STRUCTURE ZSCO_COSS_KEY01.
        INCLUDE STRUCTURE ZSCO_COSS_MEG01.
DATA : END OF  IT_COSS.
DATA : BEGIN OF IT_COL_PCC OCCURS 0,
        PERID   LIKE RKU01G-PERBI, "Period
        AUFNR   LIKE AFPO-AUFNR,
        KOSTL   LIKE ANLP-KOSTL,
        LSTAR   LIKE CSLA-LSTAR,
        MEGXXX  LIKE COSS-MEG001,
        MEINH   LIKE COSS-MEINH,
        VAEQTY  LIKE ZTCO_MHHRTRANS-VAEQTY,
        UNIT    LIKE ZTCO_MHHRTRANS-UNIT,
        RATE_%(16)  TYPE P DECIMALS 6,
        TOMEG   LIKE COSS-MEG001,
        MEGXXX_RATE_%
                LIKE COSS-MEG001.
DATA : END OF  IT_COL_PCC.
* For ZTCO_MHPCPOST
DATA : IT_ZTCO_MHPCPOST LIKE STANDARD TABLE OF ZTCO_MHPCPOST
                        WITH HEADER LINE .
* For DD data
DATA : IT_ET_FIELDLIST LIKE TABLE OF RFVICP_DDIC_TABL_FIELDNAME
                       WITH HEADER LINE.
* For POSTING (MTO)
DATA : BEGIN OF IT_PO_POST OCCURS 500.
        INCLUDE STRUCTURE IT_ZTCO_MHPCPOST.
DATA :    PO_AUFNR    LIKE  AUFK-AUFNR,
          PLNTY_EXP   LIKE  CAUFVD-PLNTY,
          PLNNR_EXP   LIKE  CAUFVD-PLNNR,
          PLNAL_EXP   LIKE  CAUFVD-PLNAL,
          PLNME_EXP   LIKE  CAUFVD-PLNME,
          ARBID       LIKE  PLPO-ARBID,
          ARBPL       LIKE  CRHD-ARBPL,
          VORNR       LIKE  PLPO-VORNR.
DATA : END OF IT_PO_POST.
* For POSTING (MTS-REM)
DATA : BEGIN OF IT_REM_POST OCCURS 500.
        INCLUDE STRUCTURE IT_ZTCO_MHPCPOST.
DATA :
          PWERK       LIKE BLPK-PWERK,
          PLNTY_EXP   LIKE  CAUFVD-PLNTY,
          PLNNR_EXP   LIKE  CAUFVD-PLNNR,
          PLNAL_EXP   LIKE  CAUFVD-PLNAL,
          PLNME_EXP   LIKE  CAUFVD-PLNME,
          ARBID       LIKE  PLPO-ARBID,
          ARBPL       LIKE  CRHD-ARBPL,
          VORNR       LIKE  PLPO-VORNR.
DATA : END OF IT_REM_POST.
DATA  : WA_REM_POST LIKE IT_REM_POST.
* For DI B/F
DATA : BEGIN OF IT_DI_POST OCCURS 500.
DATA :  FLG_REVERSAL  LIKE BAPI_PPC_APOHEADS-FLG_REVERSAL.
        INCLUDE STRUCTURE IT_ZTCO_MHPCPOST.
DATA :  WRONG_PPC.
DATA : END OF IT_DI_POST.

* For BDC
*       Batchinputdata of single transaction
DATA:   IT_BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE.
*       messages of call transaction
DATA:   IT_MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.

** Variable
* For DD Table name
DATA : GV_CI_TABNAME     TYPE  DDOBJNAME .
DATA : GV_PERCOUNT       LIKE  COSP-PERBL. "Period Counter
* Global Indicator (existence of records to be posted)
DATA : GV_NEW.
DATA : GV_POSTDATE_BDC(10). "    LIKE  SY-DATUM.
DATA : GV_STR_DATE LIKE SY-DATUM.
DATA : GV_END_DATE LIKE SY-DATUM.

** For BAPI
DATA : IT_COSTCENTERLIST LIKE STANDARD TABLE OF BAPI0012_CCLIST
                         WITH HEADER LINE.
DATA : IT_RETURN         LIKE STANDARD TABLE OF BAPIRET2
                         WITH HEADER LINE.
** REM Profile
DATA : GV_REMPF_FSC      LIKE MARC-SFEPR VALUE 'VEHI'.
DATA : GV_REMPF_ENG      LIKE MARC-SFEPR VALUE 'ENGI'.
DATA : GV_REMPF_BLANK    LIKE MARC-SFEPR VALUE SPACE.

** For resource data
DATA: IT_RESGUID16 TYPE KCR_GUID_16_TAB.
DATA: IF_MODEID16 TYPE PPC_MODE_GUID_INT.
DATA: IT_ACT_RAW TYPE PPC_T_ACT_RAW
                 WITH HEADER LINE.
DATA: IT_PPC_SHOW_EXT_ACT TYPE TABLE OF PPC_SHOW_EXT_ACT
                 WITH HEADER LINE.
* For the Combined PPC_Activity master data.
DATA : BEGIN OF IT_PPC_ACT_MOD OCCURS 0.
        INCLUDE STRUCTURE PPC_ACT.
DATA :  HEADID         LIKE PPC_SHOW_EXT_ACT-HEADID       ,
        RESOURCE_EXT   LIKE PPC_SHOW_EXT_ACT-RESOURCE_EXT ,
        ACTIVITY_NAME  LIKE PPC_SHOW_EXT_ACT-ACTIVITY_NAME,
        MODE_NO        LIKE PPC_SHOW_EXT_ACT-MODE_NO      ,
        COST_CENTER    LIKE PPC_SHOW_EXT_ACT-COST_CENTER  ,
        ACTTYPE        LIKE PPC_SHOW_EXT_ACT-ACTTYPE      ,
        CO_BUSPROC     LIKE PPC_SHOW_EXT_ACT-CO_BUSPROC   .
DATA : END OF  IT_PPC_ACT_MOD.

* For PPC DI B/F
DATA : WA_PPC_HEAD  TYPE PPC_VA_HEAD.
DATA:  IT_PPC_HEADS TYPE TABLE OF BAPI_PPC_APOHEADS
                    WITH HEADER LINE .
DATA : IT_APOHEADS  LIKE STANDARD TABLE OF  BAPI_PPC_APOHEADS
                    WITH HEADER LINE ,
       IT_APOCOMPLISTS
                    LIKE STANDARD TABLE OF  BAPI_PPC_APOCOMPLISTS
                    WITH HEADER LINE ,
       IT_APOACTLISTS
                    LIKE STANDARD TABLE OF  BAPI_PPC_APOACTLISTS
                    WITH HEADER LINE .



*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-001.
PARAMETERS : P_KOKRS LIKE CSKS-KOKRS   MEMORY   ID CAC OBLIGATORY,
             P_GJAHR LIKE ANLP-GJAHR   MEMORY   ID GJR OBLIGATORY.

parameters : p_cal  default 'X'  radiobutton group ra01.
parameters : p_pst               radiobutton group ra01.
parameters : p_rev               radiobutton group ra01.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) TEXT-002.
*   From Period.
PARAMETERS: P_FRPER LIKE RKU01G-PERAB OBLIGATORY.
SELECTION-SCREEN COMMENT 52(05) TEXT-003.
*   To Period.
PARAMETERS: P_TOPER LIKE RKU01G-PERBI  NO-DISPLAY. " OBLIGATORY.
SELECTION-SCREEN END OF LINE.
PARAMETERS : P_LSTAR LIKE CSLA-LSTAR            DEFAULT 'MAN_HR'
                                                       OBLIGATORY,
             P_NCOAL LIKE GRPDYNP-NAME_COALL    DEFAULT 'DIRECT'
                                                       OBLIGATORY,
             P_MODE(1)                  DEFAULT 'N'    OBLIGATORY.
* Reverse?
SELECTION-SCREEN END OF BLOCK BL1.

SELECTION-SCREEN BEGIN OF BLOCK BL2 WITH FRAME TITLE TEXT-004.
SELECT-OPTIONS : S_MTART FOR MARA-MTART         OBLIGATORY.
PARAMETERS : P_VERSN LIKE COSS-VERSN            DEFAULT '000'
                                                OBLIGATORY,
             P_WRTTP LIKE COSS-WRTTP            DEFAULT '4'
                                                OBLIGATORY.
SELECT-OPTIONS : s_matnr for mara-matnr.
SELECT-OPTIONS : S_VRGNG FOR COSS-VRGNG        .
select-options : s_kostl for ztco_mhpcpost-kostl.
SELECTION-SCREEN END OF BLOCK BL2.

*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
* 'Fert' 'Halb' are default.
  PERFORM DEFAULT_VALUE_S_MTART.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
* Check period range
  PERFORM CHECK_PERIOD_RANGE.
* Searching for CCtr group
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_NCOAL.
  PERFORM READ_CCTR_GROUP.


*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Controlling Area Information
  PERFORM READ_TKA01.
* Calculating Period Count
  PERFORM CAL_PER_COUNT.
* Read CCtrs
  PERFORM READ_CCTR.
* Enqueue ZTCO_MHHRTRANS
  PERFORM ENQUEUE_ZTCO_MHHRTRANS.
* Enqueue ZTCO_MHHRTRANS
  PERFORM ENQUEUE_ZTCO_MHPCPOST.
* Read resource_information
  PERFORM READ_RESOURCE_DATA.
* Set MFBF Variant
  PERFORM SET_MFBF_INIT.

* posting
  IF P_PST = 'X'.
    PERFORM POST_PROCESS.
* Reversing
  ELSEIF P_REV = 'X'.
    PERFORM REVERSE_PROCESS.
  ELSE.
    perform calculate_process.
  ENDIF.


*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Print Result
  PERFORM RESULT_LIST.
*



*
*&---------------------------------------------------------------------*
*&      Form  calculate_process
*&---------------------------------------------------------------------*
FORM calculate_process.

* Check remained not-posted records .
  PERFORM check_not_post_rc.

  CASE gv_new.
    WHEN 'X'.
      MESSAGE i000 WITH 'New Run, DATA will be created'.
* Read data from ZTCO_MHHRTRANS
      PERFORM read_fr_ztco_mhhrtrans.
* Read Material Data
      PERFORM read_fr_mara_marc.
* Read PCC order
      PERFORM read_pcc_order.
* Read Data from PCC
      PERFORM read_data_pcc.
* Calculte the ratio
      PERFORM cal_quan_ratio.
* Transfer data to IT_ZTCO_MHPCPOST
      PERFORM trans_it_ztco_mhpcpost.
* Insert  ZTCO_MHPCPOST.
      PERFORM update_ztco_mhpcpost.
    WHEN OTHERS. "<- Space
      MESSAGE i000 WITH 'Re-Run, Some data were not posted'.
  ENDCASE.

ENDFORM.                    " calculate_process

*----------------------------------------------------------------------*
***INCLUDE ZACO05L_F002 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  POSTING_USING_PP_PGM
*&---------------------------------------------------------------------*
*       POSTING
*  Production order - Time Ticket         'CO11N'
*  REM backflush    - Activity  Backflush 'MFBF'
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM POSTING_USING_PP_PGM.

* Selecting
  PERFORM READ_DATA_FR_ZTCO_MHPCPOST.

* Production Order method - Mostly "Press, Engine" Material
* MTO production method
*  PERFORM POST_MTO_US_TIMETICKET.
*
*
** REM
*
** STD - "EGINE" Material
** MTS production method
  PERFORM POST_MTS_US_REM_ACT_BF.

* DI - "VEHI" Material - FSC
  PERFORM POST_DI_B_F.


ENDFORM.                    " POSTING_USING_PP_PGM

*&---------------------------------------------------------------------*
*&      Form  read_data_fr_ZTCO_MHPCPOST
*&---------------------------------------------------------------------*
*       Clear data Container and refill data from Table
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA_FR_ZTCO_MHPCPOST.
* Renewal
  CLEAR : IT_ZTCO_MHPCPOST, IT_ZTCO_MHPCPOST[].
* Only for the records which were not posted before
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTCO_MHPCPOST
           FROM ZTCO_MHPCPOST
          WHERE GJAHR = P_GJAHR
            AND PERID BETWEEN P_FRPER AND P_TOPER
            AND MATNR in s_matnr
            and kostl in s_kostl.
*           and MHDOC eq space.

  IF IT_ZTCO_MHPCPOST[] IS INITIAL.
    MESSAGE E046 WITH 'ZTCO_MHPCPOST' P_GJAHR P_FRPER P_TOPER.
  ENDIF.

*// Mod. By Hyung Jin Youn
* Check REM Profile
** REM Profile
*DATA : GV_REMPF_FSC      LIKE MARC-SFEPR VALUE 'VEHI'.
*DATA : GV_REMPF_ENG      LIKE MARC-SFEPR VALUE 'ENGI'.
*DATA : GV_REMPF_BLANK    LIKE MARC-SFEPR VALUE SPACE.

  CLEAR IT_ZTCO_MHPCPOST.
  LOOP AT IT_ZTCO_MHPCPOST WHERE SFEPR NE GV_REMPF_FSC
                             AND SFEPR NE GV_REMPF_ENG
                             AND SFEPR NE GV_REMPF_BLANK.
  ENDLOOP.

  IF SY-SUBRC = 0.
    MESSAGE S000 WITH TEXT-101.
    STOP.
  ENDIF.

*// End of Mod.

ENDFORM.                    " read_data_fr_ZTCO_MHPCPOST

*&---------------------------------------------------------------------*
*&      Form  POST_MTO_US_TIMETICKET
*&---------------------------------------------------------------------*
*       POST - MTO / Production Order method
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM POST_MTO_US_TIMETICKET.

  SORT IT_ZTCO_MHPCPOST BY GJAHR PERID.

* Only RemFlg = ' ' <- NOT RemBF
  CLEAR : IT_PO_POST, IT_PO_POST[].

  LOOP AT IT_ZTCO_MHPCPOST WHERE SAUFT EQ SPACE.
* Transferring data
    MOVE-CORRESPONDING IT_ZTCO_MHPCPOST TO IT_PO_POST.

* Read production order / operation
    PERFORM READ_PO_AND_OPID.
* append
    APPEND IT_PO_POST.
    CLEAR  IT_PO_POST.
  ENDLOOP.

  CLEAR  IT_PO_POST.

* POST using BAPI
  PERFORM POST_MTO_WITH_BAPI_TT.

ENDFORM.                    " POST_MTO_US_TIMETICKET

*&---------------------------------------------------------------------*
*&      Form  SEL_DATE_RANGE
*&---------------------------------------------------------------------*
*       Convert Period to Date
*----------------------------------------------------------------------*
*      -->P_Perid   Period
*----------------------------------------------------------------------*
FORM SEL_DATE_RANGE  USING P_PERID.
** Convert Periods to date Range
*  CLEAR : R_WORKDATE, R_WORKDATE[].
*  CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
*       EXPORTING
*            I_GJAHR = P_GJAHR
*            I_PERIV = TKA01-LMONA
*            I_POPER = P_PERID
*       IMPORTING
*            E_DATE  = R_WORKDATE-LOW.
*
*  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
*       EXPORTING
*            I_GJAHR = P_GJAHR
*            I_PERIV = TKA01-LMONA
*            I_POPER = P_PERID
*       IMPORTING
*            E_DATE  = R_WORKDATE-HIGH.
*  R_WORKDATE-SIGN   = 'I'.
*  R_WORKDATE-OPTION = 'BT'.
*  APPEND R_WORKDATE.
ENDFORM.                    " SEL_DATE_RANGE

*&---------------------------------------------------------------------*
*&      Form  READ_PO_AND_OPID
*&---------------------------------------------------------------------*
*       Read production order / operation
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_PO_AND_OPID.
* Read Production Order (The Latest date - Creation date )
* - Andy;
*FIXME
*    Actual start date should be greater thant 1st date of month.
*    Actual finish date should less thant end of period
  DATA: L_DATE LIKE SY-DATUM,
        LAST_DATE LIKE SY-DATUM.

  DATA : BEGIN OF IT_TAB OCCURS 0,
         VORNR TYPE PLPO-VORNR,
         ARBID TYPE PLPO-ARBID,
         ARBPL TYPE CRHD-ARBPL,
         DATUV TYPE PLKO-DATUV,
         END OF IT_TAB.

  CLEAR IT_TAB[].

  CONCATENATE P_GJAHR P_FRPER+1(2) '01' INTO L_DATE.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      DAY_IN            = L_DATE
    IMPORTING
      LAST_DAY_OF_MONTH = LAST_DATE.
  .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CLEAR AUFK.
*  select single max( GLTRI ) aufnr
*                   into (aufk-erdat, aufk-aufnr)
*                   from CAUFV                         "aufk
*                  where pkosa = it_po_post-aufnr
*                    and autyp = '10' "<- Production order
*                    and GSTRI <> space        "start date
*                   GLTRI >= l_date   OR  "finish date >= 1st date
*                   group by aufnr.
* Begin of changes - UD1K922934
  SELECT SINGLE MAX( GLTRI )  AUFNR
                    INTO (AUFK-ERDAT, AUFK-AUFNR)
                    FROM CAUFV                         "aufk
                   WHERE PKOSA = IT_PO_POST-AUFNR
                    AND AUTYP = '10' AND "<- Production order
                   GLTRI BETWEEN  L_DATE AND LAST_DATE "finish date
                  GROUP BY AUFNR.
  IF SY-SUBRC NE 0.
    SELECT SINGLE MAX( GLTRI )  AUFNR
                     INTO (AUFK-ERDAT, AUFK-AUFNR)
                     FROM CAUFV                         "aufk
                    WHERE PKOSA = IT_PO_POST-AUFNR
                      AND AUTYP = '10' AND "<- Production order
                      GSTRI BETWEEN  L_DATE AND LAST_DATE  "start date
                     GROUP BY AUFNR.
  ENDIF.
* End of changes - UD1K922934

* Set Production order
  IT_PO_POST-PO_AUFNR = AUFK-AUFNR.

* Read Operation with order
  DATA : IT_L_VORG_TAB	LIKE	STANDARD TABLE OF KPZP1
                          WITH HEADER LINE .

  CALL FUNCTION 'RM_OPERATION_READ_MULTI'
    EXPORTING
      AUFNR_IMP          = IT_PO_POST-PO_AUFNR
      PERIOD_IMP         = IT_PO_POST-PERID
      GJAHR_IMP          = IT_PO_POST-GJAHR
      MATNR_IMP          = IT_PO_POST-MATNR
*     APROZ_IMP          =
    IMPORTING
      PLNTY_EXP          = IT_PO_POST-PLNTY_EXP
      PLNNR_EXP          = IT_PO_POST-PLNNR_EXP
      PLNAL_EXP          = IT_PO_POST-PLNAL_EXP
      PLNME_EXP          = IT_PO_POST-PLNME_EXP
    TABLES
      VORG_TAB           = IT_L_VORG_TAB
    EXCEPTIONS
      PARALLEL_SEQ       = 1
      MISSING_PARAMETERS = 2
      NO_SEQUENCE        = 3
      ORDER_NOT_FOUND    = 4
      FALSE_FYEAR        = 5
      NO_ROUTING         = 6
      PROG_ERR           = 7
      NO_CONVERSION      = 8
      OTHERS             = 9.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

* Operation ID
*  select single  plpo~vornr         plpo~arbid
*                  crhd~arbpl
*           into  (it_po_post-vornr, it_po_post-arbid,
*                  it_po_post-arbpl)
  SELECT   PLPO~VORNR      PLPO~ARBID
                  CRHD~ARBPL PLKO~DATUV
           INTO  TABLE IT_TAB
           FROM  ( PLKO INNER JOIN PLPO
             ON  PLKO~PLNTY = PLPO~PLNTY
            AND  PLKO~PLNNR = PLPO~PLNNR )
*            AND  PLKO~ZAEHL = PLPO~ZAEHL )
             INNER JOIN CRHD
            ON
*            CRHD~OBJTY = PLPO~OBJTY  : OBJECT type = 'A' / Work Center
                 CRHD~OBJID = PLPO~ARBID
           WHERE
                 PLKO~PLNTY = IT_PO_POST-PLNTY_EXP
             AND PLKO~PLNNR = IT_PO_POST-PLNNR_EXP
             AND PLKO~PLNAL = IT_PO_POST-PLNAL_EXP
             AND PLKO~VERWE = '1'  "Usage 1 Production
             AND PLKO~STATU IN ('3','4')
             AND PLKO~DATUV  <=  LAST_DATE
             AND PLKO~DELKZ = SPACE
             AND PLPO~WERKS = IT_PO_POST-WERKS
             AND PLPO~LOEKZ = SPACE
             AND CRHD~OBJTY = 'A'
             AND CRHD~ARBPL = IT_PO_POST-KOSTL
             .
  SORT IT_TAB BY DATUV DESCENDING DATUV.
  LOOP AT IT_TAB.
    IT_PO_POST-VORNR = IT_TAB-VORNR.
    IT_PO_POST-ARBID = IT_TAB-ARBID.
    IT_PO_POST-ARBPL = IT_TAB-ARBPL.
    EXIT.
  ENDLOOP.
*      Index : Table key (all)
  IF SY-SUBRC <> 0.
* SKIP Error : Let SAP program generate system errors during posting.
*    MESSAGE E049 WITH  IT_PO_POST-PLNTY_EXP
*                       IT_PO_POST-PLNNR_EXP
*                       IT_PO_POST-PLNAL_EXP
*                       IT_PO_POST-KOSTL.
  ENDIF.

ENDFORM.                    " READ_PO_AND_OPID

*&---------------------------------------------------------------------*
*&      Form  CHECK_NOT_POST_RC
*&---------------------------------------------------------------------*
*       Check Not-posted records
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_NOT_POST_RC.

  CLEAR GV_NEW.
  CLEAR ZTCO_MHPCPOST.
* Only for the records which were not posted before
  SELECT SINGLE *
           FROM ZTCO_MHPCPOST
          WHERE GJAHR = P_GJAHR
            AND PERID BETWEEN P_FRPER AND P_TOPER
            AND RUECK EQ SPACE
            AND RMZHL EQ SPACE
            AND REVERSED = SPACE.
  IF SY-SUBRC = 0.
    GV_NEW = SPACE.
  ELSE.
    GV_NEW = 'X'. "<- No record found, New records should be created
  ENDIF.

  CHECK GV_NEW = 'X'.
  CLEAR ZTCO_MHPCPOST.
  SELECT SINGLE *
           FROM ZTCO_MHPCPOST
          WHERE GJAHR = P_GJAHR
            AND PERID BETWEEN P_FRPER AND P_TOPER
            AND REVERSED = SPACE.
  IF SY-SUBRC = 0.
    MESSAGE E053 WITH P_GJAHR P_FRPER P_TOPER.
*   MESSAGE E052.
  ENDIF .

ENDFORM.                    " CHECK_NOT_POST_RC

*&---------------------------------------------------------------------*
*&      Form  POST_MTO_WITH_BAPI_TT
*&---------------------------------------------------------------------*
*       POST - MTO
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM POST_MTO_WITH_BAPI_TT.

  DATA : IT_L_TIMETICKETS	LIKE STANDARD TABLE OF BAPI_PP_TIMETICKET
                              WITH HEADER LINE .
  DATA : WA_L_RETURN	      LIKE BAPIRET1.
  DATA : IT_L_DETAIL_RETURN	
                              LIKE STANDARD TABLE OF BAPI_CORU_RETURN
                              WITH HEADER LINE .
  DATA : LV_CONF_TEXT         LIKE IT_L_TIMETICKETS-CONF_TEXT .
  DATA : LV_POSTG_DATE        LIKE IT_L_TIMETICKETS-POSTG_DATE.

* Making Time Ticket Data
  SORT IT_PO_POST BY GJAHR PERID.

  LOOP AT IT_PO_POST.
* Clear
    CLEAR :  IT_L_TIMETICKETS, IT_L_TIMETICKETS[].
* Period
    IT_L_TIMETICKETS-ORDERID   = IT_PO_POST-PO_AUFNR.
    IT_L_TIMETICKETS-OPERATION = IT_PO_POST-VORNR.
* Final Confirmation
    IT_L_TIMETICKETS-FIN_CONF  = 'X'.
* Plant/WC(CCtr)
    IT_L_TIMETICKETS-PLANT     = IT_PO_POST-WERKS.
    IT_L_TIMETICKETS-WORK_CNTR = IT_PO_POST-KOSTL.
* Quantity / Unit
    IT_L_TIMETICKETS-CONF_ACTI_UNIT3 =  IT_PO_POST-MEINH.
    IT_L_TIMETICKETS-CONF_ACTIVITY3  =  IT_PO_POST-VARQUAN.
* TEXT
    CLEAR LV_CONF_TEXT.
    CONCATENATE IT_PO_POST-GJAHR IT_PO_POST-PERID IT_PO_POST-MATNR
                SY-UNAME         SY-REPID
           INTO LV_CONF_TEXT
           SEPARATED BY '/'.
    IT_L_TIMETICKETS-CONF_TEXT = LV_CONF_TEXT.
* Posting Date : The Last day of period
*    on change of it_po_post-gjahr
*              or it_po_post-perid.
*      clear : lv_postg_date.
*      call function 'LAST_DAY_IN_PERIOD_GET'
*           exporting
*                i_gjahr = it_po_post-gjahr
*                i_periv = tka01-lmona
*                i_poper = it_po_post-perid
*           importing
*                e_date  = lv_postg_date.
*    endon.
    IT_L_TIMETICKETS-POSTG_DATE  = GV_END_DATE.
* IT_L_TIMETICKETS
    APPEND IT_L_TIMETICKETS.
    CLEAR  IT_L_TIMETICKETS.

* Call Posting FM without Commit Work
* Sinlge Line Posting <- To check Confirnation No
    CLEAR : IT_L_DETAIL_RETURN, IT_L_DETAIL_RETURN[].
    CALL FUNCTION 'BAPI_PRODORDCONF_CREATE_TT'
      EXPORTING
        POST_WRONG_ENTRIES = '0'
        TESTRUN            = SPACE
      IMPORTING
        RETURN             = WA_L_RETURN
      TABLES
        TIMETICKETS        = IT_L_TIMETICKETS
*       GOODSMOVEMENTS     =
*       LINK_CONF_GOODSMOV =
        DETAIL_RETURN      = IT_L_DETAIL_RETURN.

* Skip 'E' 'A' Error
* But Store Error Message .
    LOOP AT IT_L_DETAIL_RETURN WHERE TYPE CA 'EA'.
      IT_PO_POST-MESSAGE = IT_L_DETAIL_RETURN-MESSAGE.
    ENDLOOP.
    LOOP AT IT_L_DETAIL_RETURN WHERE TYPE CA 'SIW'.
      IT_PO_POST-MESSAGE = IT_L_DETAIL_RETURN-MESSAGE.
      IT_PO_POST-RUECK   = IT_L_DETAIL_RETURN-CONF_NO.
      IT_PO_POST-RMZHL   = IT_L_DETAIL_RETURN-CONF_CNT.
    ENDLOOP.
* Storing Message and Conf. No / Item
    MODIFY IT_PO_POST.
* Result
    CLEAR ZTCO_MHPCPOST.
    MOVE-CORRESPONDING IT_PO_POST TO ZTCO_MHPCPOST.

* Update   ZTCO_MHPCPOST - Common Part
    PERFORM UPDATE_ZTCO_MHPCPOST_W_LOG.

    CLEAR  IT_PO_POST.
  ENDLOOP.

  CLEAR  IT_PO_POST.

ENDFORM.                    " POST_MTO_WITH_BAPI_TT

*&---------------------------------------------------------------------*
*&      Form  POST_MTS_US_REM_ACT_BF
*&---------------------------------------------------------------------*
*       REM Activity Backflush
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM POST_MTS_US_REM_ACT_BF.

  SORT IT_ZTCO_MHPCPOST BY GJAHR PERID MATNR WERKS AUFNR.

* Using BDC T-CODE 'MFBF'.
* Only RemFlg = 'X' <- RemBF
  CLEAR : IT_REM_POST, IT_REM_POST[].

  LOOP AT IT_ZTCO_MHPCPOST WHERE SAUFT NE SPACE         " 'X"
                             AND SFEPR NE GV_REMPF_FSC. " Vehicle

* Transferring data
    MOVE-CORRESPONDING IT_ZTCO_MHPCPOST TO IT_REM_POST.
* Read Production Version / Planning Plant
    ON CHANGE
              OF IT_ZTCO_MHPCPOST-GJAHR
              OR IT_ZTCO_MHPCPOST-PERID
              OR IT_ZTCO_MHPCPOST-MATNR
              OR IT_ZTCO_MHPCPOST-WERKS
              OR IT_ZTCO_MHPCPOST-AUFNR.

      PERFORM READ_PV_PPLANT.
    ENDON.
* Additional DATA
    IT_REM_POST-VERID     =   WA_REM_POST-VERID.
    IT_REM_POST-PWERK     =   WA_REM_POST-PWERK.
    IT_REM_POST-PLNTY_EXP =   WA_REM_POST-PLNTY_EXP .
    IT_REM_POST-PLNNR_EXP =   WA_REM_POST-PLNNR_EXP .
    IT_REM_POST-PLNAL_EXP =   WA_REM_POST-PLNAL_EXP .
* read operation no
    PERFORM READ_OPR_NO .
* Appending
    APPEND IT_REM_POST.
    CLEAR  IT_REM_POST.
  ENDLOOP.

  CLEAR  IT_REM_POST.

* POST using MFBF
  PERFORM POST_MTS_WITH_MFBF.

ENDFORM.                    " POST_MTS_US_REM_ACT_BF

*&---------------------------------------------------------------------*
*&      Form  READ_PV_PPLANT
*&---------------------------------------------------------------------*
*       Read Production Version / Planning Plant
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_PV_PPLANT.

  CLEAR WA_REM_POST.

**// Mod. By hyung Jin youn 2004.02.17
* About Production version
  WA_REM_POST-WERKS = IT_ZTCO_MHPCPOST-WERKS.
  WA_REM_POST-MATNR = IT_ZTCO_MHPCPOST-MATNR.
  WA_REM_POST-VERID = IT_ZTCO_MHPCPOST-VERID.
* Production Version and Planning Plant
  CLEAR BLPK.

  SELECT SINGLE PWERK
         INTO WA_REM_POST-PWERK
         FROM BLPK
        WHERE WERKS = WA_REM_POST-WERKS
          AND MATNR = WA_REM_POST-MATNR
          AND VERID = WA_REM_POST-VERID
          AND REPTP = '01'   "<- REM B/F
        GROUP BY PWERK.
  IF SY-SUBRC <> 0.
    MESSAGE E073 WITH WA_REM_POST-WERKS
                      WA_REM_POST-MATNR.
  ENDIF.
**// End of Mod.

* For Operation Nos.
  CLEAR MKAL.

  SELECT SINGLE *  FROM MKAL
                  WHERE MATNR = WA_REM_POST-MATNR
                    AND WERKS = WA_REM_POST-WERKS
                    AND VERID = WA_REM_POST-VERID.
  IF SY-SUBRC <> 0.
    MESSAGE E051 WITH WA_REM_POST-WERKS
                      WA_REM_POST-MATNR
                      WA_REM_POST-VERID.
  ENDIF.
*
  WA_REM_POST-PLNTY_EXP = MKAL-PLTYG .
  WA_REM_POST-PLNNR_EXP = MKAL-PLNNG .
  WA_REM_POST-PLNAL_EXP = MKAL-ALNAG .

ENDFORM.                    " READ_PV_PPLANT

*&---------------------------------------------------------------------*
*&      Form  READ_OPR_NO
*&---------------------------------------------------------------------*
*       Read Operation Numbers
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_OPR_NO.
* Operation ID
  SELECT SINGLE
                  PLPO~VORNR         PLPO~ARBID
                  CRHD~ARBPL

           INTO  (IT_REM_POST-VORNR, IT_REM_POST-ARBID,
                  IT_REM_POST-ARBPL)

           FROM  ( PLKO INNER JOIN PLPO
             ON  PLKO~PLNTY = PLPO~PLNTY
            AND  PLKO~PLNNR = PLPO~PLNNR )
*            AND  PLKO~ZAEHL = PLPO~ZAEHL )
                                           INNER JOIN CRHD
            ON
*            CRHD~OBJTY = PLPO~OBJTY  : OBJECT type = 'A' / Work Center
                 CRHD~OBJID = PLPO~ARBID
           WHERE
                 PLKO~PLNTY = IT_REM_POST-PLNTY_EXP
             AND PLKO~PLNNR = IT_REM_POST-PLNNR_EXP
             AND PLKO~PLNAL = IT_REM_POST-PLNAL_EXP
             AND PLKO~VERWE = '1'  "Usage 1 Production
             AND PLPO~WERKS = IT_REM_POST-WERKS
             AND PLPO~LOEKZ = SPACE
             AND CRHD~OBJTY = 'A'
             AND CRHD~ARBPL = IT_REM_POST-KOSTL.
*      Index : Table key (all)
  IF SY-SUBRC <> 0.
* SKIP Error : Let SAP program generate system errors during posting.
*    MESSAGE E049 WITH  IT_REM_POST-PLNTY_EXP
*                       IT_REM_POST-PLNNR_EXP
*                       IT_REM_POST-PLNAL_EXP
*                       IT_REM_POST-KOSTL.
  ENDIF.

ENDFORM.                    " READ_OPR_NO

*&---------------------------------------------------------------------*
*&      Form  POST_MTS_WITH_MFBF
*&---------------------------------------------------------------------*
*       Using "MFBF' - REM BACKFLUSH
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM POST_MTS_WITH_MFBF.
*       BAPI for Confirmation Document dose not support Activity
*       BackFlush function
*       There is another method for activity backflush - Add-on Program
*       Please, Refer to Technical Spec. for detail


* MFBF dose not give the Confirmation Document NO
* So to catch the final document, it is neccessary to run BDC
* by each record.
  SORT IT_REM_POST BY GJAHR PERID MATNR WERKS AUFNR.

  LOOP AT IT_REM_POST.
* Posting Period
    ON CHANGE OF IT_REM_POST-GJAHR
              OR IT_REM_POST-PERID.
      PERFORM READ_LAST_DAY_OF_PER.
    ENDON.
* Building BDC Data / CALL TR.
    PERFORM BUILD_BDC_DATA.
    CLEAR IT_REM_POST.
  ENDLOOP.

ENDFORM.                    " POST_MTS_WITH_MFBF

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING PROGRAM DYNPRO.
  CLEAR IT_BDCDATA.
  IT_BDCDATA-PROGRAM  = PROGRAM.
  IT_BDCDATA-DYNPRO   = DYNPRO.
  IT_BDCDATA-DYNBEGIN = 'X'.
  APPEND IT_BDCDATA.
ENDFORM.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
  CLEAR IT_BDCDATA.
  IT_BDCDATA-FNAM = FNAM.
  IT_BDCDATA-FVAL = FVAL.
  APPEND IT_BDCDATA.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BUILD_BDC_DATA
*&---------------------------------------------------------------------*
*       Building BDC Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_BDC_DATA.
* Clear BDC Container
  CLEAR : IT_BDCDATA, IT_BDCDATA[].

* 18 CHAR variable (for Qunatity Field in BDC )
  DATA : LV_18CHAR(18).



**** Header DATA - Information " REM B/F
  PERFORM BDC_DYNPRO      USING 'SAPLBARM' '0800'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=RBTYP'.
* Acivity Backflush
  PERFORM BDC_FIELD       USING 'RM61B-RB_LEIST'
                                'X'.

  PERFORM BDC_DYNPRO      USING 'SAPLBARM' '0800'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=RBREF'.
* Acivity Backflush
  PERFORM BDC_FIELD       USING 'RM61B-RB_LEIST'
                                'X'.
* Posting Date / Document Date
  PERFORM BDC_FIELD       USING 'RM61B-BUDAT'
                                 GV_POSTDATE_BDC .
* Matnr
  PERFORM BDC_FIELD       USING 'RM61B-MATNR'
                                IT_REM_POST-MATNR.
* Plant
  PERFORM BDC_FIELD       USING 'RM61B-WERKS'
                                IT_REM_POST-WERKS.
* Production Version
  PERFORM BDC_FIELD       USING 'RM61B-VERID'
                                IT_REM_POST-VERID.
* Planning Plant
  PERFORM BDC_FIELD       USING 'RM61B-PLWERK'
                                IT_REM_POST-PWERK.
* No Planned activities from routing
  PERFORM BDC_FIELD       USING 'RM61B-ROUT_OFF'
                                'X'.

**** Header DATA " REM B/F
  PERFORM BDC_DYNPRO      USING 'SAPLBARM' '0800'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=ISTDA'.
* Acivity Backflush
  PERFORM BDC_FIELD       USING 'RM61B-RB_LEIST'
                                'X'.
* Posting Date / Document Date
  PERFORM BDC_FIELD       USING 'RM61B-BUDAT'
                                 GV_POSTDATE_BDC .
* Matnr
  PERFORM BDC_FIELD       USING 'RM61B-MATNR'
                                IT_REM_POST-MATNR.
* Plant
  PERFORM BDC_FIELD       USING 'RM61B-WERKS'
                                IT_REM_POST-WERKS.
* Production Version
  PERFORM BDC_FIELD       USING 'RM61B-VERID'
                                IT_REM_POST-VERID.
* Planning Plant
  PERFORM BDC_FIELD       USING 'RM61B-PLWERK'
                                IT_REM_POST-PWERK.
* No Planned activities from routing
  PERFORM BDC_FIELD       USING 'RM61B-ROUT_OFF'
                                'X'.

**** OPR View
  PERFORM BDC_DYNPRO      USING 'SAPLRMAA' '0320'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=SELE'.
  PERFORM BDC_DYNPRO      USING 'SAPLRMAA' '0320'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=GOON'.

**** POST
  PERFORM BDC_DYNPRO      USING 'SAPLRMAA' '0300'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=POST'.
* IF only One OPR is assigned to routing,
* The OPR No. '10' is the first number.
* In 'MFBF' BDC, if only one operation number is assigned,
* system blocks BDC input in fields OPR no.
  IF IT_REM_POST-VORNR <> '0010'.
    PERFORM BDC_FIELD       USING 'RM61J-VORNR'
                                  IT_REM_POST-VORNR.
  ENDIF.
* value
  CLEAR LV_18CHAR.
  WRITE IT_REM_POST-VARQUAN TO LV_18CHAR.
  PERFORM BDC_FIELD       USING 'RM61J-ISM03'
                                LV_18CHAR.
  PERFORM BDC_FIELD       USING 'RM61J-ILE03'
                                'HR'.
  PERFORM BDC_FIELD       USING 'RM61J-LAR03'
                                IT_REM_POST-LSTAR.

**** Call transaction
* Call Transaction
  CLEAR   IT_MESSTAB.
  REFRESH IT_MESSTAB.
  CALL TRANSACTION 'MFBF'
                   USING  IT_BDCDATA
                   MODE   P_MODE
                   UPDATE 'S'
                   MESSAGES INTO IT_MESSTAB.

**** Check message
  CLEAR : IT_RETURN , IT_RETURN[].
  CALL FUNCTION 'CONVERT_BDCMSGCOLL_TO_BAPIRET2'
    TABLES
      IMT_BDCMSGCOLL = IT_MESSTAB
      EXT_RETURN     = IT_RETURN.
* the type of all messages in MFBF is either 'S' or 'I'.
* Check Error with Message Numbers
  CLEAR : IT_RETURN.
  LOOP AT IT_RETURN WHERE  ( ID = 'RU' AND NUMBER = '100' )
                       OR  ( ID = 'RM' AND NUMBER = '186' ).
  ENDLOOP.
* Success
  IF SY-SUBRC = 0.
    CLEAR : IT_RETURN.
    READ TABLE  IT_RETURN WITH KEY ID = 'RU' NUMBER = '100' .
    IT_REM_POST-MESSAGE = IT_RETURN-MESSAGE.
*   Searching Confirmation Document No.
*   MFBF tra. dose not send the generated Confirmation Document No
*   in message. so find it in table of AFKO. The table has the last
*   Conf. doc. no. and the counter no.
    CLEAR AFKO.
    SELECT SINGLE RUECK RMZHL
      INTO (IT_REM_POST-RUECK, IT_REM_POST-RMZHL)
      FROM AFKO
     WHERE AUFNR = IT_REM_POST-AUFNR.  "<- PCC order
* Failure
  ELSE.
* Capturing the last Message
    CLEAR IT_RETURN.
    DESCRIBE TABLE IT_RETURN LINES SY-TFILL.
    READ TABLE IT_RETURN INDEX SY-TFILL.
    IT_REM_POST-MESSAGE = IT_RETURN-MESSAGE.
  ENDIF.

* Storing Message and Conf. No / Item
  MODIFY IT_REM_POST.
* Result
  CLEAR ZTCO_MHPCPOST.
  MOVE-CORRESPONDING IT_REM_POST TO ZTCO_MHPCPOST.

* Update   ZTCO_MHPCPOST - Common Part
  PERFORM UPDATE_ZTCO_MHPCPOST_W_LOG.

ENDFORM.                    " BUILD_BDC_DATA

*&---------------------------------------------------------------------*
*&      Form  READ_LAST_DAY_OF_PER
*&---------------------------------------------------------------------*
*       Get Last day of Period
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_LAST_DAY_OF_PER.

  CLEAR GV_POSTDATE_BDC  .

  DATA : LV_DATE LIKE SY-DATUM.
  CLEAR : LV_DATE.

  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
    EXPORTING
      I_GJAHR = P_GJAHR
      I_PERIV = TKA01-LMONA
      I_POPER = IT_REM_POST-PERID
    IMPORTING
      E_DATE  = LV_DATE.

* DATE CONVERSION
  DATA : LV_CON_DATE(10).
  CLEAR LV_CON_DATE.
  CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
    EXPORTING
      DATE_INTERNAL            = LV_DATE
    IMPORTING
      DATE_EXTERNAL            = GV_POSTDATE_BDC
    EXCEPTIONS
      DATE_INTERNAL_IS_INVALID = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " READ_LAST_DAY_OF_PER

*&---------------------------------------------------------------------*
*&      Form  REVERSE_ACT_MTO_MTS
*&---------------------------------------------------------------------*
*       Call Reverse FM
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REVERSE_ACT_MTO_MTS.


**************************************************
*** MTS - Reverse using DI-B/F
  PERFORM REV_MTS_ACT_W_DI_BF.

**************************************************
*** MTO - Reverse using Time Ticket
 PERFORM REV_MTO_ACT_W_TT.

**************************************************
*** MTS - Reverse using REM-B/F
 PERFORM REV_MTS_ACT_W_REMBF.

**************************************************
*** Not Posted data
* PERFORM REV_RES_NOT_POSTED.


ENDFORM.                    " REVERSE_ACT_MTO_MTS

*&---------------------------------------------------------------------*
*&      Form  REV_MTO_CONF
*&---------------------------------------------------------------------*
*       MTO reverse
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REV_MTO_CONF.

  DATA : WA_L_RETURN  LIKE BAPIRET1.
  DATA : LV_LOCKED    LIKE BAPI_CORU_PARAM-LOCKED.
  DATA : LV_CONF_TEXT LIKE BAPI_PP_CONFIRM-CONF_TEXT.

* TEXT
  CLEAR LV_CONF_TEXT.
  CONCATENATE IT_ZTCO_MHPCPOST-GJAHR IT_ZTCO_MHPCPOST-PERID
              IT_ZTCO_MHPCPOST-MATNR
              SY-UNAME         SY-REPID
         INTO LV_CONF_TEXT
         SEPARATED BY '/'.

* Cancellation of Conf. Doc.
  CALL FUNCTION 'BAPI_PRODORDCONF_CANCEL'
    EXPORTING
      CONFIRMATION        = IT_ZTCO_MHPCPOST-RUECK
      CONFIRMATIONCOUNTER = IT_ZTCO_MHPCPOST-RMZHL
      POSTG_DATE          = GV_END_DATE
      CONF_TEXT           = LV_CONF_TEXT
    IMPORTING
      RETURN              = WA_L_RETURN
      LOCKED              = LV_LOCKED
      CREATED_CONF_NO     = IT_ZTCO_MHPCPOST-REV_RUECK
      CREATED_CONF_COUNT  = IT_ZTCO_MHPCPOST-REV_RMZHL.

* Success
  IF     WA_L_RETURN IS INITIAL
    AND  LV_LOCKED   EQ SPACE
    AND  NOT IT_ZTCO_MHPCPOST-REV_RUECK IS INITIAL
    AND  NOT IT_ZTCO_MHPCPOST-REV_RMZHL IS INITIAL.
* Mark Reverse Ind.
    IT_ZTCO_MHPCPOST-REVERSED = 'X'.
* Succ. Message
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        MSGID               = SY-MSGID
        MSGNR               = SY-MSGNO
        MSGV1               = SY-MSGV1
        MSGV2               = SY-MSGV2
        MSGV3               = SY-MSGV3
        MSGV4               = SY-MSGV4
      IMPORTING
        MESSAGE_TEXT_OUTPUT = IT_ZTCO_MHPCPOST-MESSAGE.
* Failure
  ELSE.
    IT_ZTCO_MHPCPOST-MESSAGE = WA_L_RETURN-MESSAGE.
  ENDIF.

* Already Cancelled
* If the original document is not found,
* It is considered that the doc. was already cancelled by other reasons
  IF    WA_L_RETURN-ID     = 'RU'
    AND WA_L_RETURN-NUMBER = '122'.
* Mark Reverse Ind.
    IT_ZTCO_MHPCPOST-REVERSED = 'X'.
    IT_ZTCO_MHPCPOST-MESSAGE  = TEXT-011.
  ENDIF.

* modify
  MODIFY IT_ZTCO_MHPCPOST.

* Result Update
  CLEAR  ZTCO_MHPCPOST.
  MOVE-CORRESPONDING  IT_ZTCO_MHPCPOST  TO  ZTCO_MHPCPOST.

* Update   ZTCO_MHPCPOST - Common Part
  PERFORM UPDATE_ZTCO_MHPCPOST_W_LOG.

ENDFORM.                    " REV_MTO_CONF

*&---------------------------------------------------------------------*
*&      Form  UPDATE_ZTCO_MHPCPOST_W_LOG
*&---------------------------------------------------------------------*
*       Update   ZTCO_MHPCPOST - Common Part
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_ZTCO_MHPCPOST_W_LOG.
* Update LOG
  ZTCO_MHPCPOST-AEDAT = SY-DATUM.
  ZTCO_MHPCPOST-AEZET = SY-UZEIT.
  ZTCO_MHPCPOST-AENAM = SY-UNAME.
  UPDATE ZTCO_MHPCPOST.
  IF SY-SUBRC <> 0.
    ROLLBACK WORK.
    MESSAGE E050 WITH ZTCO_MHPCPOST-GJAHR
                      ZTCO_MHPCPOST-PERID
                      ZTCO_MHPCPOST-MATNR
                      ZTCO_MHPCPOST-KOSTL.
  ELSE.
* Commit Work
* Single Commit : For document links
* Commit Work only when successing
* both in BAPI FM (or BDC)  update and in DB UPDATE
    COMMIT WORK AND WAIT .
  ENDIF.
ENDFORM.                    " UPDATE_ZTCO_MHPCPOST_W_LOG

*&---------------------------------------------------------------------*
*&      Form  RESULT_LIST
*&---------------------------------------------------------------------*
*       List (result)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RESULT_LIST.

* Local Data Definition
  DATA : LV_COUNT TYPE I.
* Total Count
  DESCRIBE TABLE IT_ZTCO_MHPCPOST LINES SY-TFILL.
  WRITE : / 'The Number of Selected Records : ', SY-TFILL.

  SKIP 1.
  IF P_PST = 'X'.
    WRITE : / 'Posting Type : Posting of Confirmation Doc'.
    CASE GV_NEW.
      WHEN 'X'.
        WRITE : / 'Running Type : New Running'.
      WHEN OTHERS.
        WRITE : / 'Running Type : Re-Running'.
    ENDCASE.
* Success Count
    CLEAR LV_COUNT.
    LOOP AT IT_ZTCO_MHPCPOST TRANSPORTING NO FIELDS
                             WHERE NOT RUECK IS INITIAL
                               AND NOT RMZHL IS INITIAL
                               AND REVERSED IS INITIAL.
      ADD 1 TO LV_COUNT.
    ENDLOOP.
    SKIP 1.
    WRITE : / 'Successfully posting : ', LV_COUNT.

  ELSEIF P_REV = 'X'.
    WRITE : / 'Posting Type : Cancellation of Confirmation Doc.'.
* Success Count
    CLEAR LV_COUNT.
    LOOP AT IT_ZTCO_MHPCPOST TRANSPORTING NO FIELDS
                             WHERE REVERSED = 'X'.
      ADD 1 TO LV_COUNT.
    ENDLOOP.
    SKIP 1.
    WRITE : / 'Successfully Cancelled : ', LV_COUNT.
* Success Count
    DATA : LV_COUNT2 TYPE I.
    CLEAR LV_COUNT2.
    LOOP AT IT_ZTCO_MHPCPOST TRANSPORTING NO FIELDS
                             WHERE REV_RUECK IS INITIAL
                               AND REV_RMZHL IS INITIAL
                               AND REVERSED = 'X'.
      ADD 1 TO LV_COUNT2.
    ENDLOOP.
    SKIP 1.
    WRITE : / 'Marked as reversed     : ', LV_COUNT2, ' out of',
              LV_COUNT .
  ENDIF.
ENDFORM.                    " RESULT_LIST

*&---------------------------------------------------------------------*
*&      Form  REV_MTS_REM_CONF
*&---------------------------------------------------------------------*
*       MTS reverse
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REV_MTS_REM_CONF.

  DATA : LV_EXP_PRTNR LIKE BLPP-PRTNR.
  DATA : WA_L_RETURN  LIKE  BAPIRET2.

** TEXT / No text can be input
*  CLEAR LV_CONF_TEXT.
*  CONCATENATE IT_ZTCO_MHPCPOST-GJAHR IT_ZTCO_MHPCPOST-PERID
*              IT_ZTCO_MHPCPOST-MATNR
*              SY-UNAME         SY-REPID
*         INTO LV_CONF_TEXT
*         SEPARATED BY '/'.

  CLEAR BLPP.
  SELECT SINGLE *
           FROM BLPP
          WHERE RUECK = IT_ZTCO_MHPCPOST-RUECK
            AND RMZHL = IT_ZTCO_MHPCPOST-RMZHL.

** Cancellation of Conf. Doc.
  CLEAR LV_EXP_PRTNR.
  CLEAR WA_L_RETURN.
  CALL FUNCTION 'BAPI_REPMANCONF1_CANCEL'
    EXPORTING
      CONFIRMATION     = BLPP-PRTNR
      POSTDATE         = GV_END_DATE
*     CANC_PDCOLLNR    =
    IMPORTING
      CANCCONFIRMATION = LV_EXP_PRTNR
      RETURN           = WA_L_RETURN.

* First Bapi Commit
  COMMIT WORK AND WAIT.

* Success
  IF     NOT LV_EXP_PRTNR IS INITIAL
     AND     WA_L_RETURN  IS INITIAL .
* Mark Reverse Ind.
    IT_ZTCO_MHPCPOST-REVERSED = 'X'.
* Message
    CONCATENATE
     'Confirmation of Doc log. ' LV_EXP_PRTNR ' is cancelled'
     INTO IT_ZTCO_MHPCPOST-MESSAGE.
* Reverse Confirmation No. / '0001'
    SELECT SINGLE RUECK RMZHL
             INTO (IT_ZTCO_MHPCPOST-REV_RUECK,
                   IT_ZTCO_MHPCPOST-REV_RMZHL)
             FROM BLPP
            WHERE PRTNR = LV_EXP_PRTNR
              AND PRTPS = '0001'.
* Failure
  ELSE.
    IT_ZTCO_MHPCPOST-MESSAGE = WA_L_RETURN-MESSAGE.
  ENDIF.

* Already Cancelled
* If the original document is not found,
* It is considered that the doc. was already cancelled by other reasons
  IF    WA_L_RETURN-ID     = 'RM'
    AND WA_L_RETURN-NUMBER = '472'.
* Mark Reverse Ind.
    IT_ZTCO_MHPCPOST-REVERSED = 'X'.
    IT_ZTCO_MHPCPOST-MESSAGE  = TEXT-011.
  ENDIF.

* modify
  MODIFY IT_ZTCO_MHPCPOST.

* Result Update
  CLEAR  ZTCO_MHPCPOST.
  MOVE-CORRESPONDING  IT_ZTCO_MHPCPOST  TO  ZTCO_MHPCPOST.

* Update   ZTCO_MHPCPOST - Common Part
  PERFORM UPDATE_ZTCO_MHPCPOST_W_LOG.

ENDFORM.                    " REV_MTS_REM_CONF

*&---------------------------------------------------------------------*
*&      Form  PUT_RESULT_INTO_IT_ZTCO_MHPCPO
*&---------------------------------------------------------------------*
*       Put Results into IT_ZTCO_MHPCPOST from IT_PO_POST
*                                            & IT_REM_POST
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PUT_RESULT_INTO_IT_ZTCO_MHPCPO.
  LOOP AT IT_ZTCO_MHPCPOST.
    CLEAR IT_PO_POST.
    READ TABLE IT_PO_POST WITH KEY
                                  GJAHR = IT_ZTCO_MHPCPOST-GJAHR
                                  PERID = IT_ZTCO_MHPCPOST-PERID
                                  MATNR = IT_ZTCO_MHPCPOST-MATNR
                                  WERKS = IT_ZTCO_MHPCPOST-WERKS
                                  AUFNR = IT_ZTCO_MHPCPOST-AUFNR
                                  KOSTL = IT_ZTCO_MHPCPOST-KOSTL
                                  LSTAR = IT_ZTCO_MHPCPOST-LSTAR
                                  MHDOC = IT_ZTCO_MHPCPOST-MHDOC.
    IF SY-SUBRC = 0.
      IT_ZTCO_MHPCPOST-RUECK   = IT_PO_POST-RUECK   .
      IT_ZTCO_MHPCPOST-RMZHL   = IT_PO_POST-RMZHL   .
      IT_ZTCO_MHPCPOST-MESSAGE = IT_PO_POST-MESSAGE .
    ELSE.
      CLEAR IT_REM_POST.
      READ TABLE IT_REM_POST WITH KEY
                                    GJAHR = IT_ZTCO_MHPCPOST-GJAHR
                                    PERID = IT_ZTCO_MHPCPOST-PERID
                                    MATNR = IT_ZTCO_MHPCPOST-MATNR
                                    WERKS = IT_ZTCO_MHPCPOST-WERKS
                                    AUFNR = IT_ZTCO_MHPCPOST-AUFNR
                                    KOSTL = IT_ZTCO_MHPCPOST-KOSTL
                                    LSTAR = IT_ZTCO_MHPCPOST-LSTAR
                                    MHDOC = IT_ZTCO_MHPCPOST-MHDOC.
      IF SY-SUBRC = 0.
        IT_ZTCO_MHPCPOST-RUECK   = IT_REM_POST-RUECK   .
        IT_ZTCO_MHPCPOST-RMZHL   = IT_REM_POST-RMZHL   .
        IT_ZTCO_MHPCPOST-MESSAGE = IT_REM_POST-MESSAGE .
      ELSE.
        CLEAR IT_DI_POST.
        READ TABLE IT_DI_POST WITH KEY
                                      GJAHR = IT_ZTCO_MHPCPOST-GJAHR
                                      PERID = IT_ZTCO_MHPCPOST-PERID
                                      MATNR = IT_ZTCO_MHPCPOST-MATNR
                                      WERKS = IT_ZTCO_MHPCPOST-WERKS
                                      AUFNR = IT_ZTCO_MHPCPOST-AUFNR
                                      KOSTL = IT_ZTCO_MHPCPOST-KOSTL
                                      LSTAR = IT_ZTCO_MHPCPOST-LSTAR
                                      MHDOC = IT_ZTCO_MHPCPOST-MHDOC.
        IT_ZTCO_MHPCPOST-RUECK   = IT_DI_POST-RUECK   .
        IT_ZTCO_MHPCPOST-RMZHL   = IT_DI_POST-RMZHL   .
        IT_ZTCO_MHPCPOST-MESSAGE = IT_DI_POST-MESSAGE .
      ENDIF.
    ENDIF.
    MODIFY IT_ZTCO_MHPCPOST.
    CLEAR IT_ZTCO_MHPCPOST.
  ENDLOOP.
ENDFORM.                    " PUT_RESULT_INTO_IT_ZTCO_MHPCPO

*&---------------------------------------------------------------------*
*&      Form  POST_DI_B_F
*&---------------------------------------------------------------------*
*       DI Activity B/F
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM POST_DI_B_F.

  SORT IT_ZTCO_MHPCPOST BY MATNR WERKS.

  CLEAR : IT_DI_POST, IT_DI_POST[].

  LOOP AT IT_ZTCO_MHPCPOST WHERE SAUFT NE SPACE         " 'X"
                             AND SFEPR EQ GV_REMPF_FSC. " FSC
* Transferring data
    MOVE-CORRESPONDING IT_ZTCO_MHPCPOST TO IT_DI_POST.
* Check Production Version
    CLEAR MKAL.

    SELECT SINGLE *  FROM MKAL
                    WHERE MATNR = IT_DI_POST-MATNR
                      AND WERKS = IT_DI_POST-WERKS
                      AND VERID = IT_DI_POST-VERID.
    IF SY-SUBRC <> 0.
      MESSAGE S051 WITH IT_DI_POST-WERKS
                        IT_DI_POST-MATNR
                        IT_DI_POST-VERID
                        IT_DI_POST-AUFNR.
      STOP.
    ENDIF.

* Reversal Flag
* FLG_REVERSAL
   IF IT_DI_POST-VARQUAN < 0.
    IT_DI_POST-FLG_REVERSAL = 'X'.
   ENDIF.

* Unit Always 'STD'
* Refer to program ZACO03U_MHAM -> FORM ADD_UP_DATA.
* Unit conversion was made already.

* Append
    APPEND  IT_DI_POST.
    CLEAR   IT_DI_POST.

    CLEAR IT_ZTCO_MHPCPOST .
  ENDLOOP.

  CLEAR  IT_DI_POST.

* POST using PPCVAR -> Not exactly posting action
* PPCVAR just make data in DI-B/F queue and the data can be posted
* (Activity B/F) when the run of PPCGO is completed
* PPCGO uses the data in DI-B/F queue and posts the activity
* related data
* Then, PPCGO2 posts the components related data
* IN HMMA - Add-on BAckflush Program is using 2 step B/F
* This development part is to make data for DI-B/F queue.
* So actual posting is supposed to be done after running PPCGO
* The reflection of COST data is going to be done when the background
* job for PPCGO is finished - PP module is in charge of running PPCGO
  PERFORM POST_DI_ACT_PPCVAR.

ENDFORM.                    " POST_DI_B_F

*&---------------------------------------------------------------------*
*&      Form  READ_RESOURCE_DATA
*&---------------------------------------------------------------------*
*       Read resource_information
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_RESOURCE_DATA.

  CLEAR : IT_RESGUID16,        IT_RESGUID16[].
  CLEAR : IF_MODEID16.
  CLEAR : IT_ACT_RAW,          IT_ACT_RAW[].
  CLEAR : IT_PPC_SHOW_EXT_ACT, IT_PPC_SHOW_EXT_ACT[].

  RANGES: SO_BUDAT FOR  SY-DATUM.
  SO_BUDAT-OPTION = 'BT'.
  SO_BUDAT-SIGN   = 'I'.
  SO_BUDAT-LOW    = GV_STR_DATE.
  SO_BUDAT-HIGH   = GV_END_DATE.
  APPEND SO_BUDAT.

  CALL FUNCTION 'PPC1DC_ACTS_SELECT'
    EXPORTING
      IT_RESGUIDS = IT_RESGUID16
      IF_MODEGUID = IF_MODEID16
    TABLES
      IR_BUDAT    = SO_BUDAT
*     ir_uname    = so_uname
*     ir_rptid    = so_rptid
*     ir_actid    = so_actid
      ET_ACTS_EXT = IT_ACT_RAW
*     ET_HEADIDS  =
    .

  CALL FUNCTION 'PPC1RT_ACT_RAW_CONVERT'
    TABLES
      IT_ACT_RAW  = IT_ACT_RAW
      ET_ACTS_EXT = IT_PPC_SHOW_EXT_ACT.

* Delete redundant records
  DELETE IT_PPC_SHOW_EXT_ACT WHERE COST_CENTER EQ SPACE
                                OR ACTTYPE     EQ SPACE.

  SORT IT_PPC_SHOW_EXT_ACT BY COST_CENTER ACTTYPE.
  DELETE ADJACENT DUPLICATES FROM IT_PPC_SHOW_EXT_ACT
                  COMPARING COST_CENTER ACTTYPE.

* Clear IT_PPC_ACT_MOD
  CLEAR : IT_PPC_ACT_MOD, IT_PPC_ACT_MOD[].

  LOOP AT IT_PPC_SHOW_EXT_ACT WHERE ACTTYPE = P_LSTAR.
    LOOP AT IT_ACT_RAW
                WHERE ACTID = IT_PPC_SHOW_EXT_ACT-ACTID.
      MOVE-CORRESPONDING IT_PPC_SHOW_EXT_ACT TO IT_PPC_ACT_MOD.
      MOVE-CORRESPONDING IT_ACT_RAW          TO IT_PPC_ACT_MOD.
      APPEND IT_PPC_ACT_MOD.
      CLEAR  IT_PPC_ACT_MOD.
      CLEAR  IT_ACT_RAW.
    ENDLOOP.
    CLEAR IT_PPC_SHOW_EXT_ACT.
  ENDLOOP.
ENDFORM.                    " READ_RESOURCE_DATA

*&---------------------------------------------------------------------*
*&      Form  POST_DI_ACT_PPCVAR
*&---------------------------------------------------------------------*
*       POST using PPCVAR
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM POST_DI_ACT_PPCVAR.

  CLEAR : IT_APOHEADS,     IT_APOHEADS[].
  CLEAR : IT_APOCOMPLISTS, IT_APOCOMPLISTS[].
  CLEAR : IT_APOACTLISTS,  IT_APOACTLISTS[].

  CLEAR IT_PPC_ACT_MOD.

  SORT IT_DI_POST BY FLG_REVERSAL GJAHR PERID MATNR WERKS AUFNR.

* Check invalid resourse
  LOOP AT IT_DI_POST.
    CLEAR IT_PPC_ACT_MOD .
    READ TABLE  IT_PPC_ACT_MOD WITH KEY COST_CENTER = IT_DI_POST-KOSTL
                                        ACTTYPE     = IT_DI_POST-LSTAR.
    IF SY-SUBRC <> 0.
*      MESSAGE S074 WITH 'PPCSA' IT_DI_POST-KOSTL IT_DI_POST-LSTAR.
      IT_DI_POST-WRONG_PPC = 'X'. "Wrong Master
      MODIFY IT_DI_POST.
      CLEAR IT_DI_POST.
    ENDIF.
  ENDLOOP.

* Building Posting Tabs
  LOOP AT IT_DI_POST WHERE WRONG_PPC NE 'X'.
    ON CHANGE OF IT_DI_POST-FLG_REVERSAL
              OR IT_DI_POST-GJAHR
              OR IT_DI_POST-PERID
              OR IT_DI_POST-MATNR
              OR IT_DI_POST-WERKS
              OR IT_DI_POST-AUFNR.
* Creation of PPC Header
      PERFORM CREATE_PPC_HEADER.
    ENDON.

* Creation of PPC Item  - with WA_PPC_HEAD-HEADID
    CLEAR IT_APOACTLISTS.
    IT_APOACTLISTS-HEADID = WA_PPC_HEAD-HEADID.

    CLEAR IT_PPC_ACT_MOD .
    READ TABLE  IT_PPC_ACT_MOD WITH KEY COST_CENTER = IT_DI_POST-KOSTL
                                        ACTTYPE     = IT_DI_POST-LSTAR.
    IF SY-SUBRC <> 0.
      CONTINUE.
    ENDIF.

* Resource Info
    IT_APOACTLISTS-RESOURCE_GUID  = IT_PPC_ACT_MOD-RESOURCE_GUID.
    IT_APOACTLISTS-MODE_GUID      = IT_PPC_ACT_MOD-MODE_GUID.
* Value
    IT_APOACTLISTS-DURATION_VAR = ABS( IT_DI_POST-VARQUAN ).
* DURATION_VAR / Delta
    IT_APOACTLISTS-DELTA_DURATION_VAR = IT_APOACTLISTS-DURATION_VAR.
* DELTA_DURATION_FIX
    IT_APOACTLISTS-DURUNIT = IT_DI_POST-MEINH.

    APPEND IT_APOACTLISTS.
    CLEAR  IT_APOACTLISTS.
    CLEAR IT_DI_POST.
  ENDLOOP.


* Call posting FM
  CLEAR : IT_RETURN, IT_RETURN[].
  CALL FUNCTION 'BAPI_MNFCTCONFRCVR_RECEIVE'
    IMPORTING
      RETURN          = IT_RETURN
    TABLES
      IT_APOHEADS     = IT_APOHEADS
*     IT_APOCOMPLISTS =
      IT_APOACTLISTS  = IT_APOACTLISTS.


* Result
  LOOP AT  IT_DI_POST WHERE WRONG_PPC NE 'X'.
* Skip 'E' 'A' Error
* But Store Error Message .
    LOOP AT IT_RETURN WHERE TYPE CA 'EA'.
      IT_DI_POST-MESSAGE = IT_RETURN-MESSAGE.
    ENDLOOP.
    LOOP AT IT_RETURN WHERE TYPE CA 'SIW'.
* Result Message (DI B/F)
      PERFORM MESSAGE_FOR_DI_BF USING TEXT-110.
    ENDLOOP.
    IF IT_RETURN[] IS INITIAL.
* Result Message (DI B/F)
* Posting     - DI - Act. B/F is successfully saved to PPC Queue
      PERFORM MESSAGE_FOR_DI_BF USING TEXT-110.
    ENDIF.
* Storing Message
    MODIFY IT_DI_POST.
* Result
    CLEAR ZTCO_MHPCPOST.
    MOVE-CORRESPONDING IT_DI_POST TO ZTCO_MHPCPOST.

* Update   ZTCO_MHPCPOST - Common Part
    PERFORM UPDATE_ZTCO_MHPCPOST_W_LOG.

    CLEAR  IT_DI_POST.
  ENDLOOP.

* Invalid Master
  LOOP AT  IT_DI_POST WHERE WRONG_PPC = 'X'.
* Store Error Message .
* No resource data check T-Code &, &/&
    MESSAGE S074 WITH 'PPCSA' IT_DI_POST-KOSTL IT_DI_POST-LSTAR
    INTO  IT_DI_POST-MESSAGE.
* Storing Message
    MODIFY IT_DI_POST.
* Result
    CLEAR ZTCO_MHPCPOST.
    MOVE-CORRESPONDING IT_DI_POST TO ZTCO_MHPCPOST.
* Update   ZTCO_MHPCPOST - Common Part
    PERFORM UPDATE_ZTCO_MHPCPOST_W_LOG.
    CLEAR  IT_DI_POST.
  ENDLOOP.

ENDFORM.                    " POST_DI_ACT_PPCVAR

*&---------------------------------------------------------------------*
*&      Form  CREATE_PPC_HEADER
*&---------------------------------------------------------------------*
*       Creation of PPC Header
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_PPC_HEADER.

* Posting Date
  DATA : LV_DATE LIKE SY-DATUM.
  CLEAR : LV_DATE.

  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
    EXPORTING
      I_GJAHR = IT_DI_POST-GJAHR
      I_PERIV = TKA01-LMONA
      I_POPER = IT_DI_POST-PERID
    IMPORTING
      E_DATE  = LV_DATE.

* Clear Header Information
  CLEAR   WA_PPC_HEAD.
  CLEAR : IT_PPC_HEADS, IT_PPC_HEADS[].

* Header ID
  CALL FUNCTION 'GUID_CREATE'
    IMPORTING
      EV_GUID_32 = WA_PPC_HEAD-HEADID.

* ... fill additional information
  GET TIME STAMP FIELD WA_PPC_HEAD-CONF_TIME.
  MOVE LV_DATE  TO WA_PPC_HEAD-PSTNG_DATE.
  MOVE SY-UNAME TO WA_PPC_HEAD-CONF_USERNAME.
* Posting Ind. (Reversal Ind.)
  WA_PPC_HEAD-FLG_REVERSAL = IT_DI_POST-FLG_REVERSAL.

  MOVE '3' TO WA_PPC_HEAD-FLG_INFO_DEST.    "separate variances posting
  APPEND WA_PPC_HEAD TO IT_PPC_HEADS.

* Header Tab.
  LOOP AT IT_PPC_HEADS .
    MOVE-CORRESPONDING IT_PPC_HEADS TO IT_APOHEADS .
* MAT Infor
    IT_APOHEADS-HEAD_MATNR = IT_DI_POST-MATNR.
    IT_APOHEADS-PRODPLANT  = IT_DI_POST-WERKS.
    IT_APOHEADS-VERSION    = IT_DI_POST-VERID.

    APPEND IT_APOHEADS .
    CLEAR  IT_APOHEADS .
    CLEAR  IT_PPC_HEADS.
  ENDLOOP.

ENDFORM.                    " CREATE_PPC_HEADER

*&---------------------------------------------------------------------*
*&      Form  REV_MTO_ACT_W_TT
*&---------------------------------------------------------------------*
*       MTO - Reverse using Time Ticket
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REV_MTO_ACT_W_TT.

  SORT IT_ZTCO_MHPCPOST BY GJAHR PERID SAUFT SFEPR.

  LOOP AT IT_ZTCO_MHPCPOST WHERE SAUFT = SPACE
                             AND NOT RUECK IS INITIAL
                             AND NOT RMZHL IS INITIAL.
* Posting Period
*   on change of it_ztco_mhpcpost-gjahr
*             or it_ztco_mhpcpost-perid.
*     perform get_last_rev_pos_date.
*   endon.
* Reverse confirmation document with counter
    PERFORM REV_MTO_CONF.
    CLEAR IT_ZTCO_MHPCPOST.
  ENDLOOP.
ENDFORM.                    " REV_MTO_ACT_W_TT

*&---------------------------------------------------------------------*
*&      Form  REV_MTS_ACT_W_REMBF
*&---------------------------------------------------------------------*
*       MTS - Reverse using REM-B/F
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REV_MTS_ACT_W_REMBF.
**// Mod. by Hyung Jin Youn 2004.02.17
  LOOP AT IT_ZTCO_MHPCPOST WHERE SAUFT NE SPACE         " 'X'
                             AND SFEPR EQ GV_REMPF_ENG  " Engine
                             AND NOT RUECK IS INITIAL
                             AND NOT RMZHL IS INITIAL.
**// End of Mod.

* Posting Period
*    on change of it_ztco_mhpcpost-gjahr
*              or it_ztco_mhpcpost-perid.
*      perform get_last_rev_pos_date.
*    endon.
* Reverse confirmation document with counter
    PERFORM REV_MTS_REM_CONF.
    CLEAR IT_ZTCO_MHPCPOST.
  ENDLOOP.

ENDFORM.                    " REV_MTS_ACT_W_REMBF

*&---------------------------------------------------------------------*
*&      Form  REV_MTS_ACT_W_DI_BF
*&---------------------------------------------------------------------*
*       MTS - Reverse using DI-B/F
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REV_MTS_ACT_W_DI_BF.

  SORT IT_ZTCO_MHPCPOST BY MATNR WERKS.

  CLEAR : IT_DI_POST, IT_DI_POST[].

**// Mod. by Hyung Jin Youn 2004.02.17
  LOOP AT IT_ZTCO_MHPCPOST WHERE SAUFT  NE SPACE         " 'X'
                             AND SFEPR EQ GV_REMPF_FSC   " FSC
                             AND NOT RUECK IS INITIAL
                             AND NOT RMZHL IS INITIAL.
**// End of Mod.
    read table IT_COSTCENTERLIST with key COSTCENTER = it_ztco_mhpcpost-kostl.
    check sy-subrc = 0.

* Transferring data
    MOVE-CORRESPONDING IT_ZTCO_MHPCPOST TO IT_DI_POST.
* Check Production Version
    CLEAR MKAL.

    SELECT SINGLE *  FROM MKAL
                    WHERE MATNR = IT_DI_POST-MATNR
                      AND WERKS = IT_DI_POST-WERKS
                      AND VERID = IT_DI_POST-VERID.
    IF SY-SUBRC <> 0.
      MESSAGE S051 WITH IT_DI_POST-WERKS
                        IT_DI_POST-MATNR
                        IT_DI_POST-VERID
                        IT_DI_POST-AUFNR.
      STOP.
    ENDIF.

*** - > Reverse
* Reversal Flag
* FLG_REVERSAL
   IF IT_DI_POST-VARQUAN > 0.
    IT_DI_POST-FLG_REVERSAL = 'X'.
   ENDIF.


* Reverse * (-1)
* SAP advice....no negative value
*   it_di_post-varquan = it_di_post-varquan  * ( -1 ).
    IT_DI_POST-VARQUAN = ABS( IT_DI_POST-VARQUAN ).

* Unit Always 'STD'
* Refer to program ZACO03U_MHAM -> FORM ADD_UP_DATA.
* Unit conversion was made already.

* Append
    APPEND  IT_DI_POST.
    CLEAR   IT_DI_POST.

    CLEAR IT_ZTCO_MHPCPOST .
  ENDLOOP.

  CLEAR  IT_DI_POST.


* Same rutine as normal posting
  PERFORM POST_DI_ACT_PPCVAR.


* Saving result for report
  LOOP AT IT_DI_POST .
    LOOP AT IT_ZTCO_MHPCPOST WHERE
                                   GJAHR = IT_DI_POST-GJAHR
                               AND PERID = IT_DI_POST-PERID
                               AND MATNR = IT_DI_POST-MATNR
                               AND WERKS = IT_DI_POST-WERKS
                               AND AUFNR = IT_DI_POST-AUFNR
                               AND KOSTL = IT_DI_POST-KOSTL
                               AND LSTAR = IT_DI_POST-LSTAR
                               AND MHDOC = IT_DI_POST-MHDOC.

* Dummy Value - no confirmation document
      IT_ZTCO_MHPCPOST-REV_RUECK =  IT_DI_POST-REV_RUECK.
      IT_ZTCO_MHPCPOST-REV_RMZHL =  IT_DI_POST-REV_RMZHL.
      IT_ZTCO_MHPCPOST-REVERSED  =  IT_DI_POST-REVERSED .
      MODIFY  IT_ZTCO_MHPCPOST.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " REV_MTS_ACT_W_DI_BF

*&---------------------------------------------------------------------*
*&      Form  REV_RES_NOT_POSTED
*&---------------------------------------------------------------------*
*       Not Posted data - Reverse
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REV_RES_NOT_POSTED.
  LOOP AT IT_ZTCO_MHPCPOST WHERE RUECK IS INITIAL
                             AND RMZHL IS INITIAL.
* Set reverse mark without cancelled ref. doc.
* because those data were not posted
* Result Update
* Mark Reverse Ind.
    IT_ZTCO_MHPCPOST-REVERSED = 'X'.
* Message
    IT_ZTCO_MHPCPOST-MESSAGE  = TEXT-010.
* modify
    MODIFY IT_ZTCO_MHPCPOST.
* Trans. data
    CLEAR  ZTCO_MHPCPOST.
    MOVE-CORRESPONDING  IT_ZTCO_MHPCPOST  TO  ZTCO_MHPCPOST.

* Update   ZTCO_MHPCPOST - Common Part
    PERFORM UPDATE_ZTCO_MHPCPOST_W_LOG.

    CLEAR IT_ZTCO_MHPCPOST.
  ENDLOOP.

ENDFORM.                    " REV_RES_NOT_POSTED

*&---------------------------------------------------------------------*
*&      Form  MESSAGE_FOR_DI_BF
*&---------------------------------------------------------------------*
*       Result Message (DI B/F)
*----------------------------------------------------------------------*
*      -->P_IT_RETURN_MESSAGE  text
*----------------------------------------------------------------------*
FORM MESSAGE_FOR_DI_BF USING    P_MESSAGE.
  IF P_PST = 'X'.
    IT_DI_POST-MESSAGE      = P_MESSAGE.
* Dummy Value - no confirmation document
*FIXME why ... dummy?
    IT_DI_POST-RUECK        = '9900000000'.
    IT_DI_POST-RMZHL        = '99000000'.

  ELSEIF P_REV = 'X'.
*Reversing  - DI - Act. B/F is successfully saved to PPC Queue
    IT_DI_POST-MESSAGE      = TEXT-111.
* Dummy Value - no confirmation document
    IT_DI_POST-REV_RUECK    = '1100000000'.
    IT_DI_POST-REV_RMZHL    = '11000000'.
    IT_DI_POST-REVERSED     = 'X'.
  ENDIF.
ENDFORM.                    " MESSAGE_FOR_DI_BF

*----------------------------------------------------------------------*
*   INCLUDE ZACO05L_F001                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  READ_CCTR_GROUP
*&---------------------------------------------------------------------*
*       Read CCtr Group (Search Help)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_cctr_group.
  CALL FUNCTION 'K_GROUP_SELECT'
    EXPORTING
*     BUTTONS            = 'X'
      class              = '0101'
*     CRUSER             = '*'
      field_name         = space
*     SEARCHFLD          = '    '
*     SEARCHFLD_INPUT    = 'X'
      searchfld_required = ' '
*     SET                = GV_CCGR_SETID
*     START_COLUMN       = 10
*     START_ROW          = 5
*     TABLE              = 'CCSS'
*     TYPELIST           = 'BS'
*     UPDUSER            = '*'
*     KOKRS              =
*     KTOPL              =
    IMPORTING
*     CLASS_NAME         =
      set_name           = p_ncoal
*     SET_TITLE          =
*     TABLE_NAME         =
*     SETID              =
    EXCEPTIONS
      no_set_picked      = 1
      OTHERS             = 2.

* No error check for F4  SH
  IF sy-subrc <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " READ_CCTR_GROUP

*&---------------------------------------------------------------------*
*&      Form  Read_TKA01
*&---------------------------------------------------------------------*
*       Controlling Area Information
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_tka01.
  CLEAR tka01.
  SELECT SINGLE * FROM tka01
                 WHERE kokrs = p_kokrs.
  IF sy-subrc <> 0.
    MESSAGE e038 WITH p_kokrs.
  ENDIF.
ENDFORM.                    " Read_TKA01

*&---------------------------------------------------------------------*
*&      Form  READ_CCTR
*&---------------------------------------------------------------------*
*       Read CCtrs for retrieval.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_cctr.
* Making an internal table for CCtr to select data
  DATA : lv_datum LIKE sy-datum.
  CONCATENATE p_gjahr p_frper+1(2) '01' INTO lv_datum.

  CLEAR : it_costcenterlist, it_costcenterlist[],
          it_return,         it_return[].

  CALL FUNCTION 'BAPI_COSTCENTER_GETLIST1'
    EXPORTING
      controllingarea = p_kokrs
      date_from       = lv_datum
      costcentergroup = p_ncoal
    TABLES
      costcenterlist  = it_costcenterlist
      return          = it_return.
* Message
  PERFORM dis_bapi_message.

ENDFORM.                    " READ_CCTR

*&---------------------------------------------------------------------*
*&      Form  DIS_BAPI_MESSAGE
*&---------------------------------------------------------------------*
*       Display BAPI Message
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dis_bapi_message.
  IF NOT it_return[] IS INITIAL.
    LOOP AT   it_return.
      MESSAGE ID     it_return-id
              TYPE   it_return-type
              NUMBER it_return-number
              WITH   it_return-message_v1
                     it_return-message_v2
                     it_return-message_v3
                     it_return-message_v4.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " DIS_BAPI_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  ENQUEUE_ZTCO_MHHRTRANS
*&---------------------------------------------------------------------*
*       Enqueue
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM enqueue_ztco_mhhrtrans.

  DATA : lv_perid LIKE ztco_mhhrtrans-perid.

  lv_perid = p_frper.

  DO 16 TIMES .
    IF lv_perid =< p_toper.
      CALL FUNCTION 'ENQUEUE_EZCO_ZTCO_MHHRTR'
        EXPORTING
          mode_ztco_mhhrtrans = 'E'
          mandt               = sy-mandt
          gjahr               = p_gjahr
          perid               = lv_perid
*         KOSTL               =
*         LSTAR               =
*         X_GJAHR             = ' '
*         X_PERID             = ' '
*         X_KOSTL             = ' '
*         X_LSTAR             = ' '
          _scope              = '3'
*         _WAIT               = ' '
*         _COLLECT            = ' '
        EXCEPTIONS
          foreign_lock        = 1
          system_failure      = 2
          OTHERS              = 3.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
* Period Counting
    lv_perid = lv_perid  + 1.

  ENDDO.
ENDFORM.                    " ENQUEUE_ZTCO_MHHRTRANS

*&---------------------------------------------------------------------*
*&      Form  READ_FR_ZTCO_MHHRTRANS
*&---------------------------------------------------------------------*
*       read data from ZTCO_MHHRTRANS
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_fr_ztco_mhhrtrans.

  CLEAR : it_ztco_mhhrtrans, it_ztco_mhhrtrans[].

  CLEAR ztco_mhhrtrans.
  SELECT    gjahr
            perid
            kostl
            lstar
            vaeqty
            unit
           FROM ztco_mhhrtrans
           INTO CORRESPONDING FIELDS OF TABLE it_ztco_mhhrtrans
           FOR  ALL ENTRIES IN it_costcenterlist
           WHERE gjahr = p_gjahr
             AND perid BETWEEN p_frper and p_toper
             and KOSTL = IT_COSTCENTERLIST-COSTCENTER
             AND lstar = p_lstar
             AND actqty NE space
             AND curqty NE space
             AND vaeqty NE space.
* Actual M/H <> '0' and Current M/H <> '0' and Variance M/H <> '0'.
* request on 2003.10.27

  CLEAR   it_ztco_mhhrtrans.

  IF it_ztco_mhhrtrans[] IS INITIAL.
    MESSAGE e047.
  ENDIF.

ENDFORM.                    " READ_FR_ZTCO_MHHRTRANS

*&---------------------------------------------------------------------*
*&      Form  CHECK_PERIOD_RANGE
*&---------------------------------------------------------------------*
*       Check Period range
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_period_range.
*NO-DISPLAY / Only One period
  p_toper = p_frper.
*  IF P_FRPER > P_TOPER.
*    MESSAGE E031.
*  ENDIF.
ENDFORM.                    " CHECK_PERIOD_RANGE

*&---------------------------------------------------------------------*
*&      Form  READ_FR_MARA_MARC
*&---------------------------------------------------------------------*
*       Read Matnr / REM indicator
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_fr_mara_marc.
*// Mod. By Hyung Jin Youn 2004.02.13
* include REM profile
  CLEAR : it_marc, it_marc[].
  SELECT       mara~mtart
               marc~matnr marc~werks
               marc~sauft marc~sfepr
          INTO CORRESPONDING FIELDS OF TABLE it_marc
          FROM mara INNER JOIN marc
            ON mara~matnr = marc~matnr
          WHERE mtart IN s_mtart.
*// End of Mod.
  CLEAR it_marc.
ENDFORM.                    " READ_FR_MARA_MARC

*&---------------------------------------------------------------------*
*&      Form  READ_PCC_ORDER
*&---------------------------------------------------------------------*
*       Read PCC order
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_pcc_order.
* PCC order can not be created without material master data .
* So it is right way to look up PCC order with material data.
  DATA : it_l_e_vkks0	LIKE TABLE OF vkks0
                        WITH HEADER LINE.
* Clear
  CLEAR   it_marc.
  CLEAR : it_ma_obj, it_ma_obj[].

  LOOP AT it_marc.
    CLEAR : it_l_e_vkks0, it_l_e_vkks0[].
    CALL FUNCTION 'KK_F_PKOSA_FIND'
      EXPORTING
        i_matnr                     = it_marc-matnr
        i_werks                     = it_marc-werks
        i_pwerk                     = it_marc-werks
*       I_PROCNR                    = ' '
*       I_SA_AUFNR                  = ' '
*       I_FA_AUFNR                  = ' '
*       I_VERID                     = ' '
*       I_STLAN                     = ' '
*       I_STLAL                     = ' '
*       I_PLNTY                     = ' '
*       I_PLNNR                     = ' '
*       I_PLNAL                     = ' '
*       I_DATE                      = '00000000'
*       I_POPUP                     = ' '
*       I_REM                       = ' '
*       I_INCL_LOEKZ                = ' '
*       I_NO_OLD_PKOSA              = 'X'
*     IMPORTING
*       E_PROCNR                    =
*       E_VERID                     =
*       E_STLAN                     =
*       E_STLAL                     =
*       E_PLNTY                     =
*       E_PLNNR                     =
*       E_PLNAL                     =
*       E_AUFNR                     =
      TABLES
        e_vkks0                     = it_l_e_vkks0
*       E_PKOSA                     =
      EXCEPTIONS
        none_found                  = 1
        wrong_input                 = 2
        none_picked                 = 3
        wrong_rule                  = 4
        rsh_not_valid               = 5
        wrong_characteristics       = 6
        no_rule                     = 7
        version_not_valid           = 8
        OTHERS                      = 9.

* if No PCC order, Skip the record .
    IF sy-subrc <> 0.
      DELETE it_marc.
      CONTINUE.
    ENDIF.
    IF it_l_e_vkks0[]  IS INITIAL .
      DELETE it_marc.
      CONTINUE.
    ENDIF.

    LOOP AT it_l_e_vkks0.
* Copying Data
      MOVE-CORRESPONDING it_marc TO it_ma_obj.
      it_ma_obj-aufnr = it_l_e_vkks0-aufnr.
      it_ma_obj-objnr = it_l_e_vkks0-objnr.

* Read Production Version  - PROCNR
      CALL FUNCTION 'KK_F_PKOSA_FIND'
        EXPORTING
          i_matnr               = it_marc-matnr
          i_werks               = it_marc-werks
          i_pwerk               = it_marc-werks
          i_procnr              = it_l_e_vkks0-procnr
*         I_NO_OLD_PKOSA        = 'X'
        IMPORTING
          e_verid               = it_ma_obj-verid
        EXCEPTIONS
          none_found            = 1
          wrong_input           = 2
          none_picked           = 3
          wrong_rule            = 4
          rsh_not_valid         = 5
          wrong_characteristics = 6
          no_rule               = 7
          version_not_valid     = 8
          OTHERS                = 9.
* Also the Prod. version should be valid
      CLEAR mkal.
      SELECT SINGLE *  FROM mkal
                      WHERE matnr = it_marc-matnr
                        AND werks = it_marc-werks
                        AND verid = it_ma_obj-verid.

      IF sy-subrc <> 0.
* No assigned Production Version -> MAX Production Version
* request By (Functional Member 2004.02.17)
* Read Max - Production Version
        CLEAR mkal.
        SELECT SINGLE MAX( verid )
                         INTO it_ma_obj-verid
                         FROM mkal
                        WHERE matnr = it_marc-matnr
                          AND werks = it_marc-werks.
      ENDIF.

* Making ITAB for CO orders
      COLLECT   it_ma_obj.
      CLEAR     it_ma_obj.
      CLEAR     it_l_e_vkks0.
    ENDLOOP.
    CLEAR     it_marc.
  ENDLOOP.

  SORT  it_ma_obj BY matnr werks.
  CLEAR it_ma_obj.

* Rid off PCC without Receipt Qty.
  LOOP AT it_ma_obj.
    CLEAR ckmlhd.
    SELECT SINGLE * FROM ckmlhd
                   WHERE matnr = it_ma_obj-matnr
                     AND bwkey = it_ma_obj-werks.
    CLEAR mlcd.
    SELECT SINGLE * FROM mlcd
                   WHERE kalnr = ckmlhd-kalnr
                     AND bdatj = p_gjahr
                     AND poper = p_frper
                     AND categ = 'ZU'
                     AND lbkum NE space.

    IF sy-subrc = 0.
    ELSE.
      DELETE it_ma_obj.
    ENDIF.
    CLEAR it_ma_obj.
  ENDLOOP.

ENDFORM.                    " READ_PCC_ORDER

*&---------------------------------------------------------------------*
*&      Form  READ_DATA_PCC
*&---------------------------------------------------------------------*
*       Read DATA from Product Cost Collector
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data_pcc.
* CO order number is unique each Material Code.
* Under the Item level, there can be more than one records
* In CO view point, Only the Order number level is considered.

* PCC orders in HMMA are created at Plant Level not production level
* So only one PCC order is created each Material Code.
* (2003.10.14)

* Making OBJ. Key for CCtr + AT (For COSS-PAROB/USPOB)
  PERFORM making_cctr_at_obj_key.

* Read Dynamic Fields Name
  PERFORM read_field_name_from_dd_coss.

* Read DATA from PCC
  PERFORM read_pcc_data.

* Put In the information about Material Code
  PERFORM put_mat_info.

* Re-orginize by period
  PERFORM re_org_by_per.

ENDFORM.                    " READ_DATA_PCC

*&---------------------------------------------------------------------*
*&      Form  MAKING_CCTR_AT_OBJ_KEY
*&---------------------------------------------------------------------*
*       Make object key from CCtr + AT
*       Build object key combination with PCC order
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM making_cctr_at_obj_key.
* Making Key Combination : COSS-OBJNR + COSS-PAROB + COSS-USPOB
  DATA : BEGIN OF it_l_objkey OCCURS 0,
           kostl  LIKE csks-kostl,
           lstar  LIKE csla-lstar,
           objnr  LIKE coss-objnr,
         END OF   it_l_objkey.

  CLEAR   it_ztco_mhhrtrans.
  CLEAR : it_l_objkey, it_l_objkey[].

* Only CCtrs with Variance Quantity in the table 'ZTCO_MHHRTRANS'
  SORT it_ztco_mhhrtrans BY kostl.
  LOOP AT it_ztco_mhhrtrans.
    ON CHANGE OF it_ztco_mhhrtrans-kostl.
      MOVE-CORRESPONDING  it_ztco_mhhrtrans
                      TO  it_l_objkey.
      COLLECT it_l_objkey.
      CLEAR   it_l_objkey.
    ENDON.
    CLEAR it_ztco_mhhrtrans.
  ENDLOOP.

* Get Object Key
  LOOP AT it_l_objkey.
    CALL FUNCTION 'K_LSTAR_OBJECT_KEY_GET'
      EXPORTING
        kokrs = p_kokrs
        kostl = it_l_objkey-kostl
        lstar = it_l_objkey-lstar
      IMPORTING
        objnr = it_l_objkey-objnr.
    MODIFY it_l_objkey.
    CLEAR  it_l_objkey.
  ENDLOOP.
  CLEAR   it_l_objkey.

* Making Key Combination to be used in selecting data from COSS
  DATA : it_l_tmp_obj     LIKE STANDARD TABLE OF  it_ma_obj
                          WITH HEADER LINE .

  it_l_tmp_obj[] = it_ma_obj[].

  CLEAR : it_ma_obj, it_ma_obj[].

  LOOP AT it_l_tmp_obj.
    LOOP AT it_l_objkey.
      CLEAR it_ma_obj.
      MOVE-CORRESPONDING it_l_tmp_obj TO it_ma_obj.
* PAROB = USPOB.
      it_ma_obj-parob = it_ma_obj-uspob
                      = it_l_objkey-objnr.
      it_ma_obj-kostl = it_l_objkey-kostl.
      it_ma_obj-lstar = it_l_objkey-lstar.
      APPEND it_ma_obj.
      CLEAR  it_ma_obj.
      CLEAR  it_l_objkey.
    ENDLOOP.
    CLEAR it_l_tmp_obj.
  ENDLOOP.

  CLEAR   it_ma_obj.
  CLEAR : it_l_tmp_obj, it_l_tmp_obj[]. FREE it_l_tmp_obj.

ENDFORM.                    " MAKING_CCTR_AT_OBJ_KEY

*&---------------------------------------------------------------------*
*&      Form  READ_FIELD_NAME_FROM_DD_COSS
*&---------------------------------------------------------------------*
*       Read Technical FieldName for COSS
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_field_name_from_dd_coss.

  CLEAR : it_et_fieldlist, it_et_fieldlist[].

* read DD infor. COSS Key Part
  PERFORM read_dd_info  TABLES it_et_fieldlist
                        USING  'ZSCO_COSS_KEY01'.

* read DD infor. COSS Value Part (Total Quantity)
  PERFORM read_dd_info  TABLES it_et_fieldlist
                        USING  'ZSCO_COSS_MEG01'.

ENDFORM.                    " READ_FIELD_NAME_FROM_DD_COSS

*&---------------------------------------------------------------------*
*&      Form  READ_DD_INFO
*&---------------------------------------------------------------------*
*       Read DD information
*----------------------------------------------------------------------*
*      -->IT_l_ET_FIELDLIST  Field-List Table
*      -->P_CI_TABNAME       DD name
*----------------------------------------------------------------------*
FORM read_dd_info TABLES   it_l_et_fieldlist STRUCTURE it_et_fieldlist
                  USING    p_ci_tabname      LIKE gv_ci_tabname.
* Local DATA definition
  DATA : it_l_fdlist LIKE STANDARD TABLE OF it_et_fieldlist
                     WITH HEADER LINE.
* Making FDlist
  CLEAR : it_l_fdlist,     it_l_fdlist[].
  CLEAR gv_ci_tabname.
  gv_ci_tabname = p_ci_tabname.
  CALL FUNCTION 'RECP_DD_TABL_FIELDNAMES_GET'
    EXPORTING
      ic_tabname   = gv_ci_tabname
    TABLES
      et_fieldlist = it_l_fdlist
    EXCEPTIONS
      not_found    = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  APPEND LINES OF  it_l_fdlist       TO it_l_et_fieldlist.

ENDFORM.                    " READ_DD_INFO

*&---------------------------------------------------------------------*
*&      Form  READ_PCC_DATA
*&---------------------------------------------------------------------*
*       Read PCC data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_pcc_data.

  CLEAR : it_ma_obj.

  CLEAR : it_coss, it_coss[].

* Selection Condition
* Using Table Key Index
* Value Type : WRTTP = '04' - Actual / User input
* Ledger     : LEDNR = '00' Standard Ledger
* COSS-PAROB = COSS-USPOB

  CLEAR coss.
  SELECT (it_et_fieldlist)
           INTO CORRESPONDING FIELDS OF TABLE it_coss
           FROM coss
            FOR ALL ENTRIES IN it_ma_obj
          WHERE lednr = '00'
            AND objnr = it_ma_obj-objnr
            AND gjahr = p_gjahr
            AND wrttp = p_wrttp
            AND versn = p_versn
            AND parob = it_ma_obj-parob
            AND uspob = it_ma_obj-uspob
            AND vrgng IN s_vrgng.

  CLEAR it_coss.

  IF it_coss[] IS INITIAL .
    MESSAGE e024 WITH p_versn p_gjahr.
  ENDIF.

ENDFORM.                    " READ_PCC_DATA

*&---------------------------------------------------------------------*
*&      Form  CAL_PER_COUNT
*&---------------------------------------------------------------------*
*       Calculation STD. - period Counter
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cal_per_count.
* Cal. the Counter
  gv_percount = p_toper - p_frper + 1.

  clear gv_end_date .
  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
    EXPORTING
      i_gjahr = p_gjahr
      i_periv = tka01-lmona
      i_poper = P_FRPER
    IMPORTING
      e_date  = gv_end_date.

  CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
    EXPORTING
      i_gjahr = p_gjahr
      i_periv = tka01-lmona
      i_poper = P_FRPER
    IMPORTING
      e_date  = gv_STR_date.

ENDFORM.                    " CAL_PER_COUNT

*&---------------------------------------------------------------------*
*&      Form  PUT_MAT_INFO
*&---------------------------------------------------------------------*
*       set Material  Information
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM put_mat_info.
* Sorting
  CLEAR it_coss.
  SORT it_ma_obj BY objnr  parob.
  SORT it_coss   BY objnr  parob.
  LOOP AT it_coss.
    CLEAR it_ma_obj.
    READ TABLE it_ma_obj
                        WITH KEY objnr = it_coss-objnr
                                 parob = it_coss-parob
                        BINARY SEARCH.
* Transfer CO objects
    it_coss-kostl = it_ma_obj-kostl.
    it_coss-lstar = it_ma_obj-lstar.
    it_coss-aufnr = it_ma_obj-aufnr.
* Transfer Material Data
    it_coss-matnr = it_ma_obj-matnr.
    it_coss-mtart = it_ma_obj-mtart.
    it_coss-werks = it_ma_obj-werks.
    it_coss-sauft = it_ma_obj-sauft.
    MODIFY it_coss.
    CLEAR it_coss.
  ENDLOOP.

ENDFORM.                    " PUT_MAT_INFO

*&---------------------------------------------------------------------*
*&      Form  RE_ORG_BY_PER
*&---------------------------------------------------------------------*
*       Re-Orginize data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM re_org_by_per.

  CLEAR it_coss.
  CLEAR : it_col_pcc, it_col_pcc[].

  FIELD-SYMBOLS: <fs1> TYPE ANY.
  DATA : lv_coss_meg(30).  " VALUE 'IT_COSS-'.
  DATA : lv_cnt  LIKE  coss-perbl.

  LOOP AT it_coss.
* Key Part
    it_col_pcc-aufnr =   it_coss-aufnr.
    it_col_pcc-kostl =   it_coss-kostl.
    it_col_pcc-lstar =   it_coss-lstar.
* Unit
    it_col_pcc-meinh =   it_coss-meinh.
* Period Counter : Set From-Period .
    CLEAR lv_cnt.
    lv_cnt = p_frper .
* Value Part
    DO gv_percount TIMES.
* Period
      it_col_pcc-perid = lv_cnt.
* Value
      CLEAR lv_coss_meg.
      CONCATENATE 'IT_COSS-'  'MEG'   lv_cnt
             INTO lv_coss_meg.
      ASSIGN (lv_coss_meg) TO <fs1>.
      it_col_pcc-megxxx  = <fs1>.
* Collect
      COLLECT it_col_pcc.
* Period Counter
      lv_cnt = lv_cnt + 1.
    ENDDO.
    CLEAR it_coss.
    CLEAR it_col_pcc.
  ENDLOOP.

  CLEAR   it_col_pcc.
* Remove records if qunatity value has initial value.
  DELETE  it_col_pcc WHERE megxxx EQ space.

ENDFORM.                    " RE_ORG_BY_PER

*&---------------------------------------------------------------------*
*&      Form  CAL_QUAN_RATIO
*&---------------------------------------------------------------------*
*       Calculate Quantity Ratio
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cal_quan_ratio.
* In case that there is no COSS quantity . there will be no ratio data
* At that case, The variance  AT Quantity should not be distributed
* (Requested By functional Member 2003.10.16)

* AT Quantity In COSS   : IT_COL_PCC
* Varaince AT Quantity  : IT_ZTCO_MHHRTRANS
  LOOP AT it_col_pcc.
    CLEAR it_ztco_mhhrtrans.
    READ TABLE it_ztco_mhhrtrans WITH KEY perid = it_col_pcc-perid
                                          kostl = it_col_pcc-kostl
                                          lstar = it_col_pcc-lstar.
    IF sy-subrc = 0.
      it_col_pcc-vaeqty = it_ztco_mhhrtrans-vaeqty.
      it_col_pcc-unit   = it_ztco_mhhrtrans-unit.
    ELSE. " only Unit def
      it_col_pcc-unit   = 'STD'.
    ENDIF.
    MODIFY it_col_pcc.
    CLEAR  it_col_pcc.
  ENDLOOP.
  CLEAR it_col_pcc.

* Unit Conversion / total.
  DATA : BEGIN OF it_l_total OCCURS 0,
   perid LIKE it_col_pcc-perid,
   kostl LIKE it_col_pcc-kostl,
   lstar LIKE it_col_pcc-lstar,
   tomeg LIKE it_col_pcc-tomeg,
   meinh LIKE it_col_pcc-meinh.
  DATA : END OF it_l_total.

  SORT it_col_pcc BY perid  kostl  lstar.

  LOOP AT it_col_pcc.
* Unit Conversion
* Already AT 'STD' is used in  IT_ZTCO_MHHRTRANS-UNIT
* Check Previous program -> ZACO03U_MHAM
    IF  it_col_pcc-meinh <> it_col_pcc-unit.
      PERFORM unit_conv USING it_col_pcc-meinh
                              it_col_pcc-unit
                              it_col_pcc-megxxx .
      MODIFY it_col_pcc.
    ENDIF.
* Total
    MOVE-CORRESPONDING it_col_pcc TO it_l_total.
*---start #1 wskim 03/09/05
*    it_l_total-tomeg = it_col_pcc-megxxx.
*     COLLECT  it_l_total.
    IF it_col_pcc-megxxx >= 0.
      it_l_total-tomeg = it_col_pcc-megxxx.
      COLLECT  it_l_total.
      CLEAR    it_l_total.
    ENDIF.
*---end
    CLEAR it_col_pcc.
  ENDLOOP.
  CLEAR    it_l_total.

* Calculate Ratio / Output Quantity
  LOOP AT it_col_pcc.
    CLEAR    it_l_total.
    READ TABLE it_l_total WITH KEY    perid = it_col_pcc-perid
                                      kostl = it_col_pcc-kostl
                                      lstar = it_col_pcc-lstar.
    it_col_pcc-tomeg         = it_l_total-tomeg.
*---Start#1 wskim 03/09/2005:wip error - don't allowed '-' quantity
*   IT_COL_PCC-RATE_%        = IT_COL_PCC-MEGXXX / IT_COL_PCC-TOMEG.
    IF it_col_pcc-megxxx =< 0.
      it_col_pcc-rate_%  = 0.
    ELSE.
      it_col_pcc-rate_%  = it_col_pcc-megxxx / it_col_pcc-tomeg.
    ENDIF.
*---end
    it_col_pcc-megxxx_rate_% = it_col_pcc-rate_% * it_col_pcc-vaeqty.
    MODIFY it_col_pcc.
    CLEAR  it_col_pcc.
  ENDLOOP.
  CLEAR  it_col_pcc.
* No Value , remove it
  DELETE it_col_pcc WHERE megxxx_rate_% EQ space.

ENDFORM.                    " CAL_QUAN_RATIO

*&---------------------------------------------------------------------*
*&      Form  UNIT_CONV
*&---------------------------------------------------------------------*
*       Unit Conversion
*----------------------------------------------------------------------*
*      -->P_UNIT     UNIT
*      -->P_OUTUNIT  Unit For Output
*      -->P_QTY      Quantity
*----------------------------------------------------------------------*
FORM unit_conv USING    p_unit
                        p_outunit
                        p_qty.
  CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
    EXPORTING
      input                = p_qty
*     NO_TYPE_CHECK        = 'X'
*     ROUND_SIGN           = ' '
      unit_in              = p_unit
      unit_out             = p_outunit
    IMPORTING
*     ADD_CONST            =
*     DECIMALS             =
*     DENOMINATOR          =
*     NUMERATOR            =
      output               = p_qty
    EXCEPTIONS
      conversion_not_found = 1
      division_by_zero     = 2
      input_invalid        = 3
      output_invalid       = 4
      overflow             = 5
      type_invalid         = 6
      units_missing        = 7
      unit_in_not_found    = 8
      unit_out_not_found   = 9
      OTHERS               = 10.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  p_unit = p_outunit.

ENDFORM.                    " UNIT_CONV

*&---------------------------------------------------------------------*
*&      Form  TRANS_IT_ZTCO_MHPCPOST
*&---------------------------------------------------------------------*
*       Transferring Key Part
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM trans_it_ztco_mhpcpost.

*        PERID   LIKE RKU01G-PERBI, "Period
*        AUFNR   LIKE AFPO-AUFNR,
*        KOSTL   LIKE ANLP-KOSTL,
*        LSTAR   LIKE CSLA-LSTAR,
*
*        MEINH   LIKE COSS-MEINH,
*        MEGXXX_RATE_%
*            RATE_%  TYPE P DECIMALS 6 ,

  CLEAR : it_ztco_mhpcpost, it_ztco_mhpcpost[].

  SORT it_ma_obj BY aufnr.

  LOOP AT it_col_pcc.
    it_ztco_mhpcpost-gjahr   = p_gjahr.
    it_ztco_mhpcpost-perid   = it_col_pcc-perid.
    it_ztco_mhpcpost-aufnr   = it_col_pcc-aufnr.
    it_ztco_mhpcpost-kostl   = it_col_pcc-kostl.
    it_ztco_mhpcpost-lstar   = it_col_pcc-lstar.
    it_ztco_mhpcpost-meinh   = it_col_pcc-unit.
    it_ztco_mhpcpost-varquan = it_col_pcc-megxxx_rate_%.
    it_ztco_mhpcpost-rate    = it_col_pcc-rate_%.
    it_ztco_mhpcpost-megxxx  = it_col_pcc-megxxx.
    CLEAR it_ma_obj.
    READ TABLE it_ma_obj WITH KEY aufnr = it_ztco_mhpcpost-aufnr.
    it_ztco_mhpcpost-matnr   = it_ma_obj-matnr.
    it_ztco_mhpcpost-werks   = it_ma_obj-werks.
    it_ztco_mhpcpost-sauft   = it_ma_obj-sauft.
*// Mod. By Hyung Jin Youn 2004.02.13
* include REM profile
    it_ztco_mhpcpost-sfepr   = it_ma_obj-sfepr.
    it_ztco_mhpcpost-verid   = it_ma_obj-verid.
*// End of Mod.
    APPEND it_ztco_mhpcpost.
    CLEAR  it_ztco_mhpcpost.
    CLEAR  it_col_pcc.
  ENDLOOP.

  CLEAR it_ztco_mhpcpost.
ENDFORM.                    " TRANS_IT_ZTCO_MHPCPOST

*&---------------------------------------------------------------------*
*&      Form  UPDATE_ZTCO_MHPCPOST
*&---------------------------------------------------------------------*
*       Update Table
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_ztco_mhpcpost.
  LOOP AT it_ztco_mhpcpost.
    CLEAR ztco_mhpcpost.
    MOVE-CORRESPONDING it_ztco_mhpcpost TO ztco_mhpcpost.
* Get Number (MH Doc)
    PERFORM get_number_from_sap.
* Inserting Log
    ztco_mhpcpost-erdat = sy-datum.
    ztco_mhpcpost-erzet = sy-uzeit.
    ztco_mhpcpost-ernam = sy-uname.
* Insertion
    INSERT ztco_mhpcpost.
    IF sy-subrc <> 0.
      ROLLBACK  WORK.
      MESSAGE e045 WITH 'ZTCO_MHPCPOST' ztco_mhpcpost-gjahr
                        it_ztco_mhpcpost-perid .
    ENDIF.
    CLEAR it_ztco_mhpcpost.
  ENDLOOP.

  COMMIT WORK.

ENDFORM.                    " UPDATE_ZTCO_MHPCPOST

*&---------------------------------------------------------------------*
*&      Form  ENQUEUE_ZTCO_MHPCPOST
*&---------------------------------------------------------------------*
*       Enqueue ZTCO_MHPCPOST
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM enqueue_ztco_mhpcpost.

  DATA : lv_perid LIKE ztco_mhhrtrans-perid.

  lv_perid = p_frper.

  DO 16 TIMES .
    IF lv_perid =< p_toper.
      CALL FUNCTION 'ENQUEUE_EZCO_MHPCPOST'
        EXPORTING
          mode_ztco_mhpcpost = 'E'
          mandt              = sy-mandt
          gjahr              = p_gjahr
          perid              = lv_perid
*         MATNR              =
*         WERKS              =
*         AUFNR              =
*         KOSTL              =
*         LSTAR              =
*         X_GJAHR            = ' '
*         X_PERID            = ' '
*         X_MATNR            = ' '
*         X_WERKS            = ' '
*         X_AUFNR            = ' '
*         X_KOSTL            = ' '
*         X_LSTAR            = ' '
          _scope             = '3'
*         _WAIT              = ' '
*         _COLLECT           = ' '
        EXCEPTIONS
          foreign_lock       = 1
          system_failure     = 2
          OTHERS             = 3.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
* Period Counting
    lv_perid = lv_perid  + 1.

  ENDDO.

ENDFORM.                    " ENQUEUE_ZTCO_MHPCPOST

*&---------------------------------------------------------------------*
*&      Form  GET_NUMBER_FROM_SAP
*&---------------------------------------------------------------------*
*       Get Number
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_number_from_sap.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZCO_NR_MHD'
*     QUANTITY                = '1'
*     SUBOBJECT               = ' '
*     TOYEAR                  = '0000'
*     IGNORE_BUFFER           = ' '
    IMPORTING
      number                  = ztco_mhpcpost-mhdoc
*     QUANTITY                =
*     RETURNCODE              =
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " GET_NUMBER_FROM_SAP

*&---------------------------------------------------------------------*
*&      Form  POST_PROCESS
*&---------------------------------------------------------------------*
*       Posting
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM post_process.

* POSTING PART <- Important !!
  PERFORM posting_using_pp_pgm.

* Put Results into IT_ZTCO_MHPCPOST
* from IT_PO_POST & IT_REM_POST & IT_DI_POST

*TEMP FIX - 02/03/2012
* PERFORM put_result_into_it_ztco_mhpcpo.


ENDFORM.                    " POST_PROCESS

*&---------------------------------------------------------------------*
*&      Form  REVERSE_PROCESS
*&---------------------------------------------------------------------*
*       Reversing
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reverse_process.

* Read data to be reversed
  PERFORM read_data_to_reverse.

* Reverse .
  PERFORM reverse_act_mto_mts.

ENDFORM.                    " REVERSE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  READ_DATA_TO_REVERSE
*&---------------------------------------------------------------------*
*       Read data to be reversed
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data_to_reverse.

* Renewal
  CLEAR : it_ztco_mhpcpost, it_ztco_mhpcpost[].
* read data : REVERSED = SPACE
  CLEAR ztco_mhpcpost.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ztco_mhpcpost
           FROM ztco_mhpcpost
          WHERE gjahr = p_gjahr
            AND ( perid BETWEEN p_frper and p_toper )
            and kostl in s_kostl.

*            and MHDOC = space.

  IF it_ztco_mhpcpost[] IS INITIAL.
    MESSAGE e054 WITH 'ZTCO_MHPCPOST' p_gjahr p_frper p_toper.
  ENDIF.

ENDFORM.                    " READ_DATA_TO_REVERSE

*&---------------------------------------------------------------------*
*&      Form  DEFAULT_VALUE_S_MTART
*&---------------------------------------------------------------------*
*       'Fert' 'Halb' are default.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM default_value_s_mtart.
  CLEAR : s_mtart, s_mtart[].

  s_mtart-low  = 'FERT'.
  s_mtart-sign = 'I'.
  s_mtart-option  = 'EQ'.
  APPEND s_mtart. CLEAR s_mtart.

  s_mtart-low = 'HALB'.
  s_mtart-sign = 'I'.
  s_mtart-option  = 'EQ'.
  APPEND s_mtart. CLEAR s_mtart.

ENDFORM.                    " DEFAULT_VALUE_S_MTART

*&---------------------------------------------------------------------*
*&      Form  SET_MFBF_INIT
*&---------------------------------------------------------------------*
*       Set MFBF Variant
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_mfbf_init.
  CLEAR rmuser_tav.
  SELECT SINGLE * FROM rmuser_tav
                 WHERE username = sy-uname
                   AND tcode    = 'MFBF'.
  IF sy-subrc <> 0.
    rmuser_tav-username = sy-uname.
    rmuser_tav-tcode    = 'MFBF'.
    rmuser_tav-tvariant = space.	
    rmuser_tav-scenario = 'LAGER'.
    rmuser_tav-type     = 'L'.
    rmuser_tav-zpkt     = space.

    INSERT rmuser_tav.
  ELSE.
    rmuser_tav-tvariant = space.	
    rmuser_tav-scenario = 'LAGER'.
    rmuser_tav-type     = 'L'.
    rmuser_tav-zpkt     = space.

    MODIFY rmuser_tav.
  ENDIF.

ENDFORM.                    " SET_MFBF_INIT

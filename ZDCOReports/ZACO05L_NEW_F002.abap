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

*// Mod. By Hyung Jin Youn 2004.02.14
* DI B/F nethod - "VEHI" Material - FSC
  PERFORM POST_DI_B_F.
*// End of Mod.

* Production Order method - Mostly "Press, Engine" Material
* MTO production method
  PERFORM POST_MTO_US_TIMETICKET.

* REM B/F Method - "EGINE" Material
* MTS production method
  PERFORM POST_MTS_US_REM_ACT_BF.


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
            AND RUECK EQ SPACE
            AND RMZHL EQ SPACE
            AND REVERSED EQ SPACE.
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
      AUFNR_IMP                = IT_PO_POST-PO_AUFNR
      PERIOD_IMP               = IT_PO_POST-PERID
      GJAHR_IMP                = IT_PO_POST-GJAHR
      MATNR_IMP                = IT_PO_POST-MATNR
*     APROZ_IMP                =
    IMPORTING
      PLNTY_EXP                = IT_PO_POST-PLNTY_EXP
      PLNNR_EXP                = IT_PO_POST-PLNNR_EXP
      PLNAL_EXP                = IT_PO_POST-PLNAL_EXP
      PLNME_EXP                = IT_PO_POST-PLNME_EXP
    TABLES
      VORG_TAB                 = IT_L_VORG_TAB
    EXCEPTIONS
      PARALLEL_SEQ             = 1
      MISSING_PARAMETERS       = 2
      NO_SEQUENCE              = 3
      ORDER_NOT_FOUND          = 4
      FALSE_FYEAR              = 5
      NO_ROUTING               = 6
      PROG_ERR                 = 7
      NO_CONVERSION            = 8
      OTHERS                   = 9.

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
        POST_WRONG_ENTRIES       = '0'
        TESTRUN                  = SPACE
      IMPORTING
        RETURN                   = WA_L_RETURN
      TABLES
        TIMETICKETS              = IT_L_TIMETICKETS
*       GOODSMOVEMENTS           =
*       LINK_CONF_GOODSMOV       =
        DETAIL_RETURN            = IT_L_DETAIL_RETURN.

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

*// Mod. By Hyung Jin Youn 2004.02.14
  LOOP AT IT_ZTCO_MHPCPOST WHERE SAUFT NE SPACE         " 'X"
                             AND SFEPR EQ GV_REMPF_ENG. " Engine
*// End. of Mod.

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
  PERFORM REV_RES_NOT_POSTED.


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
  CASE P_REVS.
    WHEN SPACE.
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
    WHEN 'X'.
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
  ENDCASE.
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
      CONFIRMATION           = BLPP-PRTNR
      POSTDATE               = GV_END_DATE
*     CANC_PDCOLLNR          =
    IMPORTING
      CANCCONFIRMATION       = LV_EXP_PRTNR
      RETURN                 = WA_L_RETURN.

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

* PERFORM POST_DI_ACT_PPCVAR.

  PERFORM POST_DI_ACT_PPCVAR_NEW.

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
      IT_RESGUIDS      = IT_RESGUID16
      IF_MODEGUID      = IF_MODEID16
    TABLES
     IR_BUDAT          = SO_BUDAT
*     ir_uname          = so_uname
*     ir_rptid          = so_rptid
*     ir_actid          = so_actid
     ET_ACTS_EXT       = IT_ACT_RAW
*   ET_HEADIDS        =
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
      RETURN                = IT_RETURN
    TABLES
      IT_APOHEADS           = IT_APOHEADS
*     IT_APOCOMPLISTS       =
      IT_APOACTLISTS        = IT_APOACTLISTS.


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
* Reverse * (-1)
* SAP advice....no negative value
*   it_di_post-varquan = it_di_post-varquan  * ( -1 ).
    IT_DI_POST-VARQUAN = ABS( IT_DI_POST-VARQUAN ).

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
  CASE P_REVS.
    WHEN SPACE.
      IT_DI_POST-MESSAGE      = P_MESSAGE.
* Dummy Value - no confirmation document
      IT_DI_POST-RUECK        = '9900000000'.
      IT_DI_POST-RMZHL        = '99000000'.
    WHEN 'X'.
* --> Reverse.
      IT_DI_POST-MESSAGE      = TEXT-111.
* Dummy Value - no confirmation document
      IT_DI_POST-REV_RUECK    = '1100000000'.
      IT_DI_POST-REV_RMZHL    = '11000000'.
      IT_DI_POST-REVERSED     = 'X'.
  ENDCASE.
ENDFORM.                    " MESSAGE_FOR_DI_BF

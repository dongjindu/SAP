*----------------------------------------------------------------------*
*   INCLUDE ZACO39L_F001                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  CHANGE_SCR_ATTR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHANGE_SCR_ATTR.
  LOOP AT SCREEN.
    CHECK SCREEN-GROUP1 = 'ZPA'.
    IF P_CHG_C = 'X'.
      SCREEN-INVISIBLE = ' '.
      SCREEN-INPUT     = '1'.
    ELSE.
      SCREEN-INVISIBLE = '1'.
      SCREEN-INPUT     = ' '.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.                    " CHANGE_SCR_ATTR

*&---------------------------------------------------------------------*
*&      Form  CHECK_PERIOD_RANGE
*&---------------------------------------------------------------------*
*       Check Period range
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_PERIOD_RANGE.
  IF P_VONPE > P_BISPE.
    MESSAGE E031.
  ENDIF.
** Check Currency IND.
*  IF P_CURRT NA 'CTO'.
*    MESSAGE E000(ZMCO) WITH P_CURRT ' is not a posible value' .
*  ENDIF.
ENDFORM.                    " CHECK_PERIOD_RANGE

*&---------------------------------------------------------------------*
*&      Form  SEARCH_LTP_VR
*&---------------------------------------------------------------------*
*       Searching LTP version
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEARCH_LTP_VR.
* It is only for Checking FI-PP-CO link
  CLEAR *ZVCO_V_T442C.
  SELECT SINGLE * FROM *ZVCO_V_T442C
          WHERE KOKRS = P_KOKRS
            AND VERSN = P_VERSN
            AND GJAHR = P_GJAHR.
  IF SY-SUBRC <> 0.
    MESSAGE E034.
  ENDIF.
ENDFORM.                    " SEARCH_LTP_VR

*&---------------------------------------------------------------------*
*&      Form  CHECK_BEDAE
*&---------------------------------------------------------------------*
*       For Requirement Type
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_BEDAE.
  CLEAR T459A.
  SELECT SINGLE * FROM T459A
                 WHERE BEDAE IN S_BEDAE .
  IF SY-SUBRC <> 0.
    MESSAGE E032.
  ENDIF.
ENDFORM.                    " CHECK_BEDAE

*&---------------------------------------------------------------------*
*&      Form  CHECK_VERSB
*&---------------------------------------------------------------------*
*       For Version (LTP)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_VERSB.
  CLEAR T459V.
  SELECT SINGLE * FROM T459V
                 WHERE SPRAS = SY-LANGU
                   AND VERSB = P_VERSB.

  IF SY-SUBRC <> 0.
    MESSAGE E033.
  ENDIF.
ENDFORM.                    " CHECK_VERSB

*&---------------------------------------------------------------------*
*&      Form  READ_ZTCO_VEHI_TYPE
*&---------------------------------------------------------------------*
*       Read Vehicle Model DATA
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_ZTCO_VEHI_TYPE.
  CLEAR : IT_ZTCO_VEHI_TYPE, IT_ZTCO_VEHI_TYPE[].
* read the relationship between Material Code and Vehicle Model
* Read All
  SELECT DISTINCT
                 MTART VEHTP MATNR
            INTO CORRESPONDING FIELDS OF TABLE IT_ZTCO_VEHI_TYPE
            FROM ZTCO_VEHI_TYPE.
  IF IT_ZTCO_VEHI_TYPE[] IS INITIAL.
    MESSAGE E020.
  ENDIF.
ENDFORM.                    " READ_ZTCO_VEHI_TYPE

*&---------------------------------------------------------------------*
*&      Form  ENQUEUE_ZTCO_PLANDE
*&---------------------------------------------------------------------*
*       Enqueue for Mandt, KOKRS, BUKRS, GJAHR, VERSN
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ENQUEUE_ZTCO_PLANDE.
  CALL FUNCTION 'ENQUEUE_EZCO_ZTCO_PLANDE'
    EXPORTING
      MODE_ZTCO_PLANDEP       = 'E'
      MANDT                   = SY-MANDT
      KOKRS                   = P_KOKRS
      BUKRS                   = P_BUKRS
      GJAHR                   = P_GJAHR
      VERSN                   = P_VERSN
*     KOSTL                   =
*     ANLN1                   =
*     ANLN2                   =
*     KOART                   =
*     X_KOKRS                 = ' '
*     X_BUKRS                 = ' '
*     X_GJAHR                 = ' '
*     X_VERSN                 = ' '
*     X_KOSTL                 = ' '
*     X_ANLN1                 = ' '
*     X_ANLN2                 = ' '
*     X_KOART                 = ' '
*     _SCOPE                  = '2'
*     _WAIT                   = ' '
*     _COLLECT                = ' '
    EXCEPTIONS
      FOREIGN_LOCK            = 1
      SYSTEM_FAILURE          = 2
      OTHERS                  = 3
            .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " ENQUEUE_ZTCO_PLANDE

*&---------------------------------------------------------------------*
*&      Form  READ_FR_ZTCO_PLANDEP
*&---------------------------------------------------------------------*
*       Read Source DATA
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_FR_ZTCO_PLANDEP.

* Progress Ind.
  PERFORM PROGRESS_IND USING '10'
                             TEXT-011.

  CLEAR : IT_ZTCO_PLANDEP, IT_ZTCO_PLANDEP[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTCO_PLANDEP
           FROM ZTCO_PLANDEP
          WHERE KOKRS = P_KOKRS
            AND BUKRS = P_BUKRS
            AND GJAHR = P_GJAHR
            AND VERSN = P_VERSN.

  IF IT_ZTCO_PLANDEP[] IS INITIAL .
    MESSAGE E037 WITH P_KOKRS P_BUKRS P_GJAHR P_VERSN.
  ENDIF.

* Period Splitting
  CLEAR : IT_PLANDEP, IT_PLANDEP[].
* Local Data definition
  FIELD-SYMBOLS: <FS1> TYPE ANY.
  DATA : LV_ORG_FN(50).
  DATA : LV_CNT(2) TYPE N.

  LOOP AT IT_ZTCO_PLANDEP.
* Key Part
    CLEAR IT_PLANDEP.
    MOVE-CORRESPONDING IT_ZTCO_PLANDEP TO IT_PLANDEP.
* Vehicle Model
    SELECT SINGLE
           B~VEHTP
      INTO IT_PLANDEP-VEHTP
      FROM ANLA AS A INNER JOIN ZTCO_VEHI_TYPE AS B
        ON A~IZWEK = B~IZWEK
     WHERE A~BUKRS = P_BUKRS
       AND A~ANLN1 = IT_ZTCO_PLANDEP-ANLN1
       AND A~ANLN2 = IT_ZTCO_PLANDEP-ANLN2.
    IF SY-SUBRC <> 0.
      MESSAGE E040 WITH IT_ZTCO_PLANDEP-ANLN1
                        IT_ZTCO_PLANDEP-ANLN2.
    ENDIF.
* Period Counter : Set From-Period .
    CLEAR LV_CNT.
    LV_CNT = P_VONPE .
* Passing Value as period
    DO GV_PERCOUNT TIMES.
* Period
      IT_PLANDEP-PERIOD = LV_CNT.
* Value Part
      CLEAR LV_ORG_FN.
      CONCATENATE 'IT_ZTCO_PLANDEP-VAL'    LV_CNT
             INTO LV_ORG_FN.
      ASSIGN (LV_ORG_FN) TO <FS1>.

      IT_PLANDEP-VALXX = <FS1>.
* Collect data
      COLLECT IT_PLANDEP.
* Period Counter
      LV_CNT = LV_CNT + 1.
    ENDDO.
    CLEAR IT_PLANDEP.
    CLEAR IT_ZTCO_PLANDEP.
  ENDLOOP.

  CLEAR IT_PLANDEP.
ENDFORM.                    " READ_FR_ZTCO_PLANDEP

*&---------------------------------------------------------------------*
*&      Form  READ_LTP_AT_QUAN
*&---------------------------------------------------------------------*
*       Read AT quantity in LTP
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_LTP_AT_QUAN.

*  CLEAR : IT_ZSCO_LTP_ATQ, IT_ZSCO_LTP_ATQ[].

*  CALL FUNCTION 'Z_FCO_LTP_AT_QUANTITY'
*       STARTING NEW TASK 'TSK01'
*       PERFORMING RETURN_LTP_INFO ON END OF TASK
*       EXPORTING
*              I_KSPP                = WA_L_KSPP
*       TABLES
*              R_RSLSTAR             = R_RSLSTAR
*              IT_ZSCO_LTP_ATQ       = IT_ZSCO_LTP_ATQ
*       EXCEPTIONS
*              T442C                 = 1
*              CONV_UNIT_ERROR       = 2
*              COMMUNICATION_FAILURE = 3  MESSAGE   LV_MSG_TEXT
*              SYSTEM_FAILURE        = 4  MESSAGE   LV_MSG_TEXT
*              OTHERS                = 5.
*

*  CALL FUNCTION 'Z_FCO_LTP_AT_QUANTITY'
*       EXPORTING
*            I_KSPP          = KSPP
*       TABLES
*            R_RSLSTAR       = R_RSLSTAR
*            IT_ZSCO_LTP_ATQ = IT_ZSCO_LTP_ATQ
*       EXCEPTIONS
*            T442C           = 1
*            CONV_UNIT_ERROR = 2
*            OTHERS          = 3.
*
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*
ENDFORM.                    " READ_LTP_AT_QUAN

*---------------------------------------------------------------------*
*       FORM RETURN_LTP_INFO                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  TASKNAME                                                      *
*---------------------------------------------------------------------*
*FORM RETURN_LTP_INFO USING TASKNAME.
*
*  DATA : LV_MSG_TEXT(80) TYPE C. "Message text
*
*  RECEIVE RESULTS FROM FUNCTION 'Z_FCO_LTP_AT_QUANTITY'
*      TABLES
**             R_RSLSTAR             =
*              IT_ZSCO_LTP_ATQ       = IT_ZSCO_LTP_ATQ
*       EXCEPTIONS
*              T442C                 = 1
*              CONV_UNIT_ERROR       = 2
*              COMMUNICATION_FAILURE = 3  MESSAGE   LV_MSG_TEXT
*              SYSTEM_FAILURE        = 4  MESSAGE   LV_MSG_TEXT
*              OTHERS                = 5.
*ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  READ_LTP_VEH_QUANTITY
*&---------------------------------------------------------------------*
*       Read LTP Quantity
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_LTP_VEH_QUANTITY.

* Progress Ind.
  PERFORM PROGRESS_IND USING '20'
                             TEXT-012.

* Read data from MARA
  DATA : BEGIN OF IT_L_TMP_MARA OCCURS 0.
          INCLUDE STRUCTURE   IT_MARA.
  DATA : END OF  IT_L_TMP_MARA.

  CLEAR : IT_MARA, IT_MARA[].

  LOOP AT IT_ZTCO_VEHI_TYPE.
    CLEAR : IT_L_TMP_MARA, IT_L_TMP_MARA[].
    SELECT MATNR  MTART MEINS
                       INTO CORRESPONDING FIELDS OF TABLE IT_L_TMP_MARA
                         FROM MARA
                        WHERE MATNR LIKE IT_ZTCO_VEHI_TYPE-MATNR
                          AND MTART =    IT_ZTCO_VEHI_TYPE-MTART.
    IF NOT IT_L_TMP_MARA[] IS INITIAL.
* Copying Vehicle Model
      IT_L_TMP_MARA-VEHTP = IT_ZTCO_VEHI_TYPE-VEHTP.
      MODIFY IT_L_TMP_MARA TRANSPORTING VEHTP  WHERE VEHTP EQ SPACE .
      APPEND LINES OF IT_L_TMP_MARA  TO IT_MARA.
    ENDIF.
    CLEAR IT_ZTCO_VEHI_TYPE.
    CLEAR IT_L_TMP_MARA.
    CLEAR IT_MARA.
  ENDLOOP.

  IF IT_MARA[] IS INITIAL .
    MESSAGE E021.
  ENDIF.

* Set range of Delivery/order finish date
  RANGES  R_DATVE FOR SY-DATUM.
  CLEAR : R_DATVE, R_DATVE[].
  CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
       EXPORTING
            I_GJAHR = P_GJAHR
            I_PERIV = TKA01-LMONA
            I_POPER = P_VONPE
       IMPORTING
            E_DATE  = R_DATVE-LOW.

  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
       EXPORTING
            I_GJAHR = P_GJAHR
            I_PERIV = TKA01-LMONA
            I_POPER = P_BISPE
       IMPORTING
            E_DATE  = R_DATVE-HIGH.
  R_DATVE-SIGN   = 'I'.
  R_DATVE-OPTION = 'BT'.
  APPEND R_DATVE.

* Read LTP data
  CLEAR MDPB. CLEAR IT_MARA.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_MDPB
           FROM MDPB
           FOR ALL ENTRIES IN IT_MARA
           WHERE
                MATNR =  IT_MARA-MATNR
            AND WERKS =  P_WERKS
            AND BEDAE IN S_BEDAE
            AND VERSB =  P_VERSB
            AND PDATU IN R_DATVE.
**// Mod. by Hyung Jin Youn 2004.01.13
* All data should be checked - not only monthly data but also
* weekly and daily data.
*                     AND  ENTLI =  '3'. "month
**// End of Mod.

  IF IT_MDPB[] IS INITIAL.
    MESSAGE E039 WITH P_WERKS P_VERSB.
  ENDIF.

* Period Splitting
  CLEAR : IT_LTP_MDPB, IT_LTP_MDPB[].

  LOOP AT IT_MDPB.
* Key Part
    IT_LTP_MDPB-WERKS = IT_MDPB-WERKS.
    IT_LTP_MDPB-MATNR = IT_MDPB-MATNR.
* Read Vehicle Model
    CLEAR IT_MARA.
    READ TABLE IT_MARA WITH KEY MATNR = IT_MDPB-MATNR.
    IF SY-SUBRC <> 0.
      CONTINUE.
    ELSE.
      IT_LTP_MDPB-VEHTP = IT_MARA-VEHTP.
      IT_LTP_MDPB-MEINS = IT_MARA-MEINS.
    ENDIF.
* Period.
    CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
      EXPORTING
        I_DATE               = IT_MDPB-PDATU
*       I_MONMIT             = 00
        I_PERIV              = TKA01-LMONA
     IMPORTING
        E_BUPER              = IT_LTP_MDPB-PERIOD
*       E_GJAHR              =
     EXCEPTIONS
        INPUT_FALSE          = 1
        T009_NOTFOUND        = 2
        T009B_NOTFOUND       = 3
        OTHERS               = 4.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
* Value Part

*// Mod. By Hyung Jin Youn  2004.04.19
* Obsolete - For HALB Material, Read requirement quantity instead of PIR
* qty.
* (2004.04.19)
* For both cases - FSC and HALB material, Read Receipt Qty
* (2004.04.20)
*    CLEAR MARA.
*    SELECT SINGLE * FROM MARA
*                   WHERE MATNR = IT_LTP_MDPB-MATNR
*                     AND MTART = 'HALB'.
*    IF SY-SUBRC = 0.
    PERFORM READ_LTP_QTY_HALB USING IT_LTP_MDPB-MATNR
                                    IT_LTP_MDPB-WERKS
                                    IT_LTP_MDPB-PERIOD
                                    IT_LTP_MDPB-PLNMG.
*    ELSE.
*      IT_LTP_MDPB-PLNMG = IT_MDPB-PLNMG.
*    ENDIF.
*// End Of Mod.
* Collect
    COLLECT  IT_LTP_MDPB.
    CLEAR  IT_LTP_MDPB.
    CLEAR  IT_MDPB.
  ENDLOOP.

  CLEAR  IT_LTP_MDPB.

ENDFORM.                    " READ_LTP_VEH_QUANTITY

*&---------------------------------------------------------------------*
*&      Form  Read_TKA01
*&---------------------------------------------------------------------*
*       Controlling Area Information
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_TKA01.
  CLEAR TKA01.
  SELECT SINGLE * FROM TKA01
                 WHERE KOKRS = P_KOKRS.
  IF SY-SUBRC <> 0.
    MESSAGE E038 WITH P_KOKRS.
  ENDIF.
ENDFORM.                    " Read_TKA01


*&---------------------------------------------------------------------*
*&      Form  MAKING_KSPP_N_AT
*&---------------------------------------------------------------------*
*       Fill out KSPP data and LSTAR data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKING_KSPP_N_AT.
* AT
  CLEAR: R_RSLSTAR, R_RSLSTAR[].
  R_RSLSTAR-LOW    = P_LSTAR.
  R_RSLSTAR-SIGN   = 'I'.
  R_RSLSTAR-OPTION = 'EQ'.
  APPEND R_RSLSTAR.
  CLEAR  R_RSLSTAR.
* KSPP
  CLEAR KSPP.
  KSPP-TO_P   = P_BISPE.
  KSPP-FROM_P = P_VONPE.
  KSPP-VERSN  = P_VERSN.
  KSPP-KOKRS  = P_KOKRS.
  KSPP-WERKS  = P_WERKS.
  KSPP-GJAHR  = P_GJAHR.
  KSPP-PCORR  = P_PCORR.
* *** Don NOT change bellow flags
  KSPP-TESTLAUF  = 'X'.
  KSPP-DETAIL_MW = 'X'.
  KSPP-LIST      = 'X'.

ENDFORM.                    " MAKING_KSPP_N_AT

*&---------------------------------------------------------------------*
*&      Form  READ_LTP_AT_QUAN_2
*&---------------------------------------------------------------------*
*       Read AT quantity in LTP
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_LTP_AT_QUAN_2.

* Progress Ind.
  PERFORM PROGRESS_IND USING '90'
                             TEXT-013.

  DATA LV_MMID TYPE CHAR30.

  LV_MMID = 'ZMIDCO_002'.

*// Mod. by Hyung Jin Youn 2004.03.15
* It seems that Transaction "KSPP" dose not work properly.
* KSPP dose not output any data with the plant key "P001".
* It is required to check the related configuration later
* * This part is ah-hoc change of source code,
*   Mark this part as comment when the configuration problem is solved.
  CLEAR KSPP-WERKS.
*// End of Mod.


  FREE MEMORY ID LV_MMID.

  CALL FUNCTION 'Z_FCO_LTP_AT_QUANTITY_2'
       EXPORTING
            I_KSPP        = KSPP
            I_MEMID       = LV_MMID
       EXCEPTIONS
            PROCESS_ERROR = 1
            UPD_TASK      = 2
            T442C         = 3
            READ_ERROR    = 4
            OTHERS        = 5.

  IF SY-SUBRC <> 0.
    IF SY-MSGID NE SPACE.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
      MESSAGE E000 WITH 'An error occurs in reading LTP AT Quan.'.
    ENDIF.
  ENDIF.

* Get DATA from FM
  CLEAR : GT_DISPLAYLIST, GT_DISPLAYLIST[].
  IMPORT GT_DISPLAYLIST  FROM MEMORY ID  LV_MMID.
  FREE MEMORY ID LV_MMID.
  CLEAR : GT_DISPLAYLIST.

*// Mod. by Hyung Jin Youn 2004.03.15
* It seems that Transaction "KSPP" dose not work properly.
* KSPP dose not output any data with the plant key "P001".
* It is required to check the related configuration later
* * This part is ah-hoc change of source code,
*   Mark this part as comment when the configuration problem is solved.
  DELETE GT_DISPLAYLIST WHERE WERKS NE P_WERKS.
  CLEAR : GT_DISPLAYLIST.
*// End of Mod.

* Delete records with Not Matched AT - (ex Not Mch_HR )
  DELETE GT_DISPLAYLIST WHERE NOT LSTAR IN R_RSLSTAR.
  CLEAR : GT_DISPLAYLIST.

* Period Spliting
* Local Data definition
  DATA : BEGIN OF WA_L_KSPP_KEY.
  INCLUDE TYPE KSPP_T_KEY_RESULTLIST.
  DATA : END OF   WA_L_KSPP_KEY.

  FIELD-SYMBOLS: <FS1> TYPE ANY.
  DATA : LV_ORG_FN(50).
  DATA : LV_CNT  LIKE P_VONPE.

  CLEAR : IT_LTP_KSPP, IT_LTP_KSPP[].

  LOOP AT GT_DISPLAYLIST.
* Key Part
    CLEAR WA_L_KSPP_KEY.
    CLEAR IT_LTP_KSPP.
    MOVE-CORRESPONDING  GT_DISPLAYLIST TO  WA_L_KSPP_KEY.
    MOVE-CORRESPONDING  WA_L_KSPP_KEY  TO  IT_LTP_KSPP.
* Read Vehicle Model
    CLEAR IT_MARA.
    READ TABLE IT_MARA WITH KEY MATNR = GT_DISPLAYLIST-MATNR.
    IF SY-SUBRC <> 0.
      CONTINUE.
    ELSE.
      IT_LTP_KSPP-VEHTP = IT_MARA-VEHTP.
    ENDIF.
* Period Counter : Set From-Period .
    CLEAR LV_CNT.
    LV_CNT = P_VONPE .
* Passing Value as period
    DO GV_PERCOUNT TIMES.
* Period
      IT_LTP_KSPP-PERIOD = LV_CNT.
* Value Part
      CLEAR LV_ORG_FN.
      CONCATENATE 'GT_DISPLAYLIST-MEG'    LV_CNT
             INTO LV_ORG_FN.
      ASSIGN (LV_ORG_FN) TO <FS1>.

      IT_LTP_KSPP-MEGBTR = <FS1>.
      IT_LTP_KSPP-MEINH  = GT_DISPLAYLIST-MEINH.
* Collect data
      COLLECT IT_LTP_KSPP.
* Period Counter
      LV_CNT = LV_CNT + 1.
    ENDDO.
    CLEAR GT_DISPLAYLIST.
    CLEAR IT_LTP_KSPP.
  ENDLOOP.

  CLEAR IT_LTP_KSPP.

ENDFORM.                    " READ_LTP_AT_QUAN_2

*&---------------------------------------------------------------------*
*&      Form  CAL_PER_COUNT
*&---------------------------------------------------------------------*
*       Calculation STD. - period Counter
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CAL_PER_COUNT.
* Cal. the Counter
  GV_PERCOUNT = P_BISPE - P_VONPE + 1.
ENDFORM.                    " CAL_PER_COUNT

*&---------------------------------------------------------------------*
*&      Form  RE_ORG_DATA
*&---------------------------------------------------------------------*
*       Re-organizing  data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RE_ORG_DATA.
  CLEAR IT_PLANDEP.
  CLEAR IT_LTP_MDPB.
  CLEAR IT_LTP_KSPP.
  CLEAR :  IT_COL_TAB ,  IT_COL_TAB[].

* All Planned Dep. Cost should be posted
* If LTP data contains CCtrs which do not have links
* to Planned DEP. Cost, The LTP data should be removed
  LOOP AT IT_PLANDEP.
* Dep DATA
    CLEAR IT_COL_TAB.
    MOVE-CORRESPONDING IT_PLANDEP TO  IT_COL_TAB.
* KSPP data
    LOOP AT IT_LTP_KSPP WHERE PERIOD = IT_COL_TAB-PERIOD
                          AND VEHTP  = IT_COL_TAB-VEHTP
                          AND KOSTL  = IT_COL_TAB-KOSTL.
      MOVE-CORRESPONDING IT_LTP_KSPP TO  IT_COL_TAB.
* LTP QUAN. data
      LOOP AT IT_LTP_MDPB WHERE WERKS  = IT_COL_TAB-WERKS
                            AND PERIOD = IT_COL_TAB-PERIOD
                            AND VEHTP  = IT_COL_TAB-VEHTP
                            AND MATNR  = IT_COL_TAB-MATNR.
        MOVE-CORRESPONDING IT_LTP_MDPB TO  IT_COL_TAB.
        APPEND IT_COL_TAB.
        CLEAR  IT_LTP_MDPB.
      ENDLOOP.
      CLEAR IT_LTP_KSPP.
    ENDLOOP.
  ENDLOOP.

  SORT IT_COL_TAB BY WERKS LSTAR PERIOD VEHTP KOSTL MATNR KOART.
  CLEAR   IT_COL_TAB.

** Cal. AT_SUM (VM + CCtr Level)
*  Cal. Other Factors
  DATA : BEGIN OF IT_L_AT_SUM OCCURS 100,
         WERKS  LIKE CKI64A-WERKS, "Plant
         LSTAR  LIKE CSSL-LSTAR  , "AT
         PERIOD LIKE RKU01G-PERBI, "Period
         VEHTP  LIKE ZTCO_VEHI_TYPE-VEHTP,
*        MATNR  LIKE MARA-MATNR,
         KOSTL  LIKE CSKS-KOSTL,
*        KOART  LIKE ZTCO_PLANDEP-KOART,
         AT_SUM  LIKE COSSA-MEG001,
         MEINH     LIKE COSSA-MEINH.
  DATA : END OF IT_L_AT_SUM .

  CLEAR : IT_L_AT_SUM, IT_L_AT_SUM[].
** From the result ( KSPP data) (AT Quantity SUM)
  SORT IT_LTP_KSPP BY WERKS LSTAR  PERIOD  VEHTP  KOSTL .
  LOOP AT IT_LTP_KSPP.
* Screening from IT_COL_TAB
* (Rid off the data in which material code dose not exist in IT_COL_TAB)
    CLEAR IT_COL_TAB.
    READ TABLE IT_COL_TAB
                          WITH KEY
                             WERKS  = IT_LTP_KSPP-WERKS
                             LSTAR  = IT_LTP_KSPP-LSTAR
                             PERIOD = IT_LTP_KSPP-PERIOD
                             VEHTP  = IT_LTP_KSPP-VEHTP
                             MATNR  = IT_LTP_KSPP-MATNR
                             KOSTL  = IT_LTP_KSPP-KOSTL.
    IF SY-SUBRC = 0.
      CLEAR IT_L_AT_SUM.
      MOVE-CORRESPONDING  IT_LTP_KSPP TO IT_L_AT_SUM.
      IT_L_AT_SUM-AT_SUM = IT_LTP_KSPP-MEGBTR.
      COLLECT IT_L_AT_SUM.
    ENDIF.
    CLEAR   IT_L_AT_SUM.
    CLEAR   IT_LTP_KSPP.
  ENDLOOP.
  CLEAR IT_L_AT_SUM.

* Into the result tab "IT_COL_TAB". (VM + CCTR)
  LOOP AT IT_COL_TAB.
    CLEAR IT_L_AT_SUM.
    READ TABLE  IT_L_AT_SUM WITH KEY
                             WERKS  = IT_COL_TAB-WERKS
                             LSTAR  = IT_COL_TAB-LSTAR
                             PERIOD = IT_COL_TAB-PERIOD
                             VEHTP  = IT_COL_TAB-VEHTP
                             KOSTL  = IT_COL_TAB-KOSTL.
* ATQ sum.
    IT_COL_TAB-AT_SUM   =  IT_L_AT_SUM-AT_SUM.
* ATQ %
    IF NOT IT_COL_TAB-AT_SUM IS INITIAL.
      IT_COL_TAB-AT_%     =  IT_COL_TAB-MEGBTR / IT_COL_TAB-AT_SUM.
    ELSE.
      CLEAR IT_COL_TAB-AT_%.
    ENDIF.
* DEP * ATQ%
    IT_COL_TAB-DEP_AT_% =  IT_COL_TAB-VALXX * IT_COL_TAB-AT_%.
* DEP * ATQ%  / LTP Quantity
    IF NOT IT_COL_TAB-PLNMG IS INITIAL .
      IT_COL_TAB-DEP_AT_%_PLM
       = IT_COL_TAB-DEP_AT_% / IT_COL_TAB-PLNMG.
    ELSE.
      CLEAR IT_COL_TAB-DEP_AT_%_PLM.
    ENDIF.
* Modify Itab
    MODIFY IT_COL_TAB.
    CLEAR  IT_COL_TAB.
  ENDLOOP.

  CLEAR IT_COL_TAB.

* Adjusting Changed DEP. Cost.
* Period Total of Subtracted DEP cost should be eaqual to that of
* DEP. Cost which are supposed to be posted
* to material planned unit cost.
* The final data below the currency range (13.2) of SAP standard will be
* set as 0.00
* Those data is useless. -> It is not to be posted
* At last,
* Period Total of Subtracted DEP cost CAN NOT be eaqual to that of
* DEP. Cost which are supposed to be posted

*  DELETE IT_COL_TAB WHERE DEP_AT_%_PLM IS INITIAL.
*  CLEAR IT_COL_TAB.

ENDFORM.                    " RE_ORG_DATA

*&---------------------------------------------------------------------*
*&      Form  PRE_REPORT_ADJ
*&---------------------------------------------------------------------*
*       The Preparation for ALV List
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRE_REPORT_ADJ.
* Building Field Cat.
  PERFORM FIELDCAT_INIT .

* Sort IT_COL_TAB.
  SORT IT_COL_TAB BY WERKS LSTAR PERIOD VEHTP MATNR KOART KOSTL.
  CLEAR IT_COL_TAB.

  IT_SORT-FIELDNAME = 'PERIOD'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.
  IT_SORT-FIELDNAME = 'VEHTP'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.
  IT_SORT-FIELDNAME = 'MATNR'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.

* Set Event
  DATA : WA_L_EVENT  TYPE SLIS_ALV_EVENT.
  WA_L_EVENT-NAME = SLIS_EV_TOP_OF_PAGE.
  WA_L_EVENT-FORM = 'BASIC_TOP_OF_PAGE'.
  APPEND WA_L_EVENT TO IT_EVENTS.

ENDFORM.                    " PRE_REPORT_ADJ

*&---------------------------------------------------------------------*
*&      Form  BUILD_POST_DATA
*&---------------------------------------------------------------------*
*       Building DATA for posting
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_POST_DATA.
  CLEAR : IT_POST, IT_POST[].
  LOOP AT IT_COL_TAB.
    MOVE-CORRESPONDING IT_COL_TAB TO IT_POST.
    COLLECT IT_POST.
    CLEAR   IT_POST.
    CLEAR   IT_COL_TAB.
  ENDLOOP.
  CLEAR IT_POST.
ENDFORM.                    " BUILD_POST_DATA

*&---------------------------------------------------------------------*
*&      Form  PROGRESS_IND
*&---------------------------------------------------------------------*
*       Progress IND.
*----------------------------------------------------------------------*
*      -->P_%         %
*      -->P_TEXT      TEXT
*----------------------------------------------------------------------*
FORM PROGRESS_IND USING    P_%
                           P_TEXT.
  CALL FUNCTION 'FI_PROGRESS_INDICATOR'
    EXPORTING
      PERCENTAGE          = P_%
      TEXT                = P_TEXT
*     MESSAGECLASS        = ' '
*     MESSAGENUMBER       = ' '
*     MESSAGEPAR1         = ' '
*     MESSAGEPAR2         = ' '
*     MESSAGEPAR3         = ' '
*     MESSAGEPAR4         = ' '
            .
ENDFORM.                    " PROGRESS_IND

*&---------------------------------------------------------------------*
*&      Form  set_init_val
*&---------------------------------------------------------------------*
*       set init. values
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_INIT_VAL.
**// Mod By Hyung Jin Youn 2004.01.13
* Adding 'VSE'. So there are two type of Req. Type.
* 'VSF' and 'VSE'
  CLEAR : S_BEDAE, S_BEDAE[].

  S_BEDAE-LOW = 'VSF'.
  S_BEDAE-SIGN = 'I'.
  S_BEDAE-OPTION = 'EQ'.
  APPEND S_BEDAE.
  CLEAR  S_BEDAE.

  S_BEDAE-LOW = 'VSE'.
  S_BEDAE-SIGN = 'I'.
  S_BEDAE-OPTION = 'EQ'.
  APPEND S_BEDAE.
  CLEAR  S_BEDAE.
**// End of Mod.
ENDFORM.                    " set_init_val

*&---------------------------------------------------------------------*
*&      Form  READ_LTP_QTY_HALB
*&---------------------------------------------------------------------*
*       For HALB Material, Read requirement quantity instead of PIR qty.
*----------------------------------------------------------------------*
*      -->P_MATNR  Material
*      -->P_WERKS  Plant
*      -->P_PERIOD PERIOD
*      -->P_PLNMG  Qty
*----------------------------------------------------------------------*
FORM READ_LTP_QTY_HALB USING    P_MATNR
                                P_WERKS
                                P_PERIOD
                                P_PLNMG.

  DATA : WA_L_MT61D LIKE MT61D,
         WA_L_MDKP  LIKE MDKP.
  DATA : IT_L_MDSU  LIKE STANDARD TABLE OF MDSU
                    WITH HEADER LINE .
  TABLES : T009B.

  CALL FUNCTION 'MD_STOCK_REQUIREMENTS_LIST_API'
    EXPORTING
      PLSCN                          = *ZVCO_V_T442C-PLSCN
      MATNR                          = P_MATNR
      WERKS                          = P_WERKS
*     BERID                          =
*     ERGBZ                          =
*     AFIBZ                          =
*     INPER                          =
*     DISPLAY_LIST_MDPSX             =
*     DISPLAY_LIST_MDEZX             =
*     DISPLAY_LIST_MDSUX             =
    IMPORTING
      E_MT61D                        = WA_L_MT61D
      E_MDKP                         = WA_L_MDKP
    TABLES
*     MDPSX                          =
*     MDEZX                          =
      MDSUX                          = IT_L_MDSU
    EXCEPTIONS
      MATERIAL_PLANT_NOT_FOUND       = 1
      PLANT_NOT_FOUND                = 2
      OTHERS                         = 3.

  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

* Read Requirement Qty
  CLEAR P_PLNMG.
  LOOP AT IT_L_MDSU WHERE DELKZ EQ SPACE.
*   DAT00
* Period.
    CLEAR T009B.
    CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
      EXPORTING
        I_DATE               = IT_L_MDSU-DAT00
*       I_MONMIT             = 00
        I_PERIV              = TKA01-LMONA
     IMPORTING
        E_BUPER              = T009B-POPER
        E_GJAHR              = T009B-BDATJ
     EXCEPTIONS
        INPUT_FALSE          = 1
        T009_NOTFOUND        = 2
        T009B_NOTFOUND       = 3
        OTHERS               = 4.

    IF    T009B-BDATJ = P_GJAHR
      AND T009B-POPER = P_PERIOD.
      P_PLNMG = P_PLNMG + IT_L_MDSU-MNG03.
    ENDIF.
    CLEAR IT_L_MDSU.
  ENDLOOP.

ENDFORM.                    " READ_LTP_QTY_HALB

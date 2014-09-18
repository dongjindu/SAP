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
FORM READ_CCTR_GROUP.
  CALL FUNCTION 'K_GROUP_SELECT'
    EXPORTING
*     BUTTONS                  = 'X'
      CLASS                    = '0101'
*     CRUSER                   = '*'
      FIELD_NAME               = SPACE
*     SEARCHFLD                = '    '
*     SEARCHFLD_INPUT          = 'X'
      SEARCHFLD_REQUIRED       = ' '
*     SET                      = GV_CCGR_SETID
*     START_COLUMN             = 10
*     START_ROW                = 5
*     TABLE                    = 'CCSS'
*     TYPELIST                 = 'BS'
*     UPDUSER                  = '*'
*     KOKRS                    =
*     KTOPL                    =
    IMPORTING
*     CLASS_NAME               =
      SET_NAME                 = P_NCOAL
*     SET_TITLE                =
*     TABLE_NAME               =
*     SETID                    =
    EXCEPTIONS
      NO_SET_PICKED            = 1
      OTHERS                   = 2.

* No error check for F4  SH
  IF SY-SUBRC <> 0.
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
FORM READ_TKA01.
  CLEAR TKA01.
  SELECT SINGLE * FROM TKA01
                 WHERE KOKRS = P_KOKRS.
  IF SY-SUBRC <> 0.
    MESSAGE E038 WITH P_KOKRS.
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
FORM READ_CCTR.
* Making an internal table for CCtr to select data
  DATA : LV_DATUM LIKE SY-DATUM.
  CONCATENATE P_GJAHR P_FRPER+1(2) '01' INTO LV_DATUM.

  CLEAR : IT_COSTCENTERLIST, IT_COSTCENTERLIST[],
          IT_RETURN,         IT_RETURN[].

  CALL FUNCTION 'BAPI_COSTCENTER_GETLIST1'
       EXPORTING
            CONTROLLINGAREA = P_KOKRS
            DATE_FROM       = LV_DATUM
            COSTCENTERGROUP = P_NCOAL
       TABLES
            COSTCENTERLIST  = IT_COSTCENTERLIST
            RETURN          = IT_RETURN.
* Message
  PERFORM DIS_BAPI_MESSAGE.

ENDFORM.                    " READ_CCTR

*&---------------------------------------------------------------------*
*&      Form  DIS_BAPI_MESSAGE
*&---------------------------------------------------------------------*
*       Display BAPI Message
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DIS_BAPI_MESSAGE.
  IF NOT IT_RETURN[] IS INITIAL.
    LOOP AT   IT_RETURN.
      MESSAGE ID     IT_RETURN-ID
              TYPE   IT_RETURN-TYPE
              NUMBER IT_RETURN-NUMBER
              WITH   IT_RETURN-MESSAGE_V1
                     IT_RETURN-MESSAGE_V2
                     IT_RETURN-MESSAGE_V3
                     IT_RETURN-MESSAGE_V4.
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
FORM ENQUEUE_ZTCO_MHHRTRANS.

  DATA : LV_PERID LIKE ZTCO_MHHRTRANS-PERID.

  LV_PERID = P_FRPER.

  DO 16 TIMES .
    IF LV_PERID =< P_TOPER.
      CALL FUNCTION 'ENQUEUE_EZCO_ZTCO_MHHRTR'
        EXPORTING
          MODE_ZTCO_MHHRTRANS       = 'E'
          MANDT                     = SY-MANDT
          GJAHR                     = P_GJAHR
          PERID                     = LV_PERID
*         KOSTL                     =
*         LSTAR                     =
*         X_GJAHR                   = ' '
*         X_PERID                   = ' '
*         X_KOSTL                   = ' '
*         X_LSTAR                   = ' '
          _SCOPE                    = '3'
*         _WAIT                     = ' '
*         _COLLECT                  = ' '
        EXCEPTIONS
          FOREIGN_LOCK              = 1
          SYSTEM_FAILURE            = 2
          OTHERS                    = 3.

      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
    ENDIF.
* Period Counting
    LV_PERID = LV_PERID  + 1.

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
FORM READ_FR_ZTCO_MHHRTRANS.

  CLEAR : IT_ZTCO_MHHRTRANS, IT_ZTCO_MHHRTRANS[].

  CLEAR ZTCO_MHHRTRANS.
  SELECT    GJAHR
            PERID
            KOSTL
            LSTAR
            VAEQTY
            UNIT
           FROM ZTCO_MHHRTRANS
           INTO CORRESPONDING FIELDS OF TABLE IT_ZTCO_MHHRTRANS
           FOR  ALL ENTRIES IN IT_COSTCENTERLIST
           WHERE GJAHR = P_GJAHR
             AND PERID BETWEEN P_FRPER AND P_TOPER
             AND KOSTL = IT_COSTCENTERLIST-COSTCENTER
             AND LSTAR = P_LSTAR
             AND ACTQTY NE SPACE
             AND CURQTY NE SPACE
             AND VAEQTY NE SPACE.
* Actual M/H <> '0' and Current M/H <> '0' and Variance M/H <> '0'.
* request on 2003.10.27

  CLEAR   IT_ZTCO_MHHRTRANS.

  IF IT_ZTCO_MHHRTRANS[] IS INITIAL.
    MESSAGE E047.
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
FORM CHECK_PERIOD_RANGE.
*NO-DISPLAY / Only One period
  P_TOPER = P_FRPER.
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
FORM READ_FR_MARA_MARC.
*// Mod. By Hyung Jin Youn 2004.02.13
* include REM profile
  CLEAR : IT_MARC, IT_MARC[].
  SELECT       MARA~MTART
               MARC~MATNR MARC~WERKS
               MARC~SAUFT MARC~SFEPR
          INTO CORRESPONDING FIELDS OF TABLE IT_MARC
          FROM MARA INNER JOIN MARC
            ON MARA~MATNR = MARC~MATNR
          WHERE MTART IN S_MTART.
*// End of Mod.
  CLEAR IT_MARC.
ENDFORM.                    " READ_FR_MARA_MARC

*&---------------------------------------------------------------------*
*&      Form  READ_PCC_ORDER
*&---------------------------------------------------------------------*
*       Read PCC order
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_PCC_ORDER.
* PCC order can not be created without material master data .
* So it is right way to look up PCC order with material data.
  DATA : IT_L_E_VKKS0	LIKE TABLE OF VKKS0
                        WITH HEADER LINE.
* Clear
  CLEAR   IT_MARC.
  CLEAR : IT_MA_OBJ, IT_MA_OBJ[].

  LOOP AT IT_MARC.
    CLEAR : IT_L_E_VKKS0, IT_L_E_VKKS0[].
    CALL FUNCTION 'KK_F_PKOSA_FIND'
      EXPORTING
        I_MATNR                     = IT_MARC-MATNR
        I_WERKS                     = IT_MARC-WERKS
        I_PWERK                     = IT_MARC-WERKS
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
        E_VKKS0                     = IT_L_E_VKKS0
*       E_PKOSA                     =
      EXCEPTIONS
        NONE_FOUND                  = 1
        WRONG_INPUT                 = 2
        NONE_PICKED                 = 3
        WRONG_RULE                  = 4
        RSH_NOT_VALID               = 5
        WRONG_CHARACTERISTICS       = 6
        NO_RULE                     = 7
        VERSION_NOT_VALID           = 8
        OTHERS                      = 9.

* if No PCC order, Skip the record .
    IF SY-SUBRC <> 0.
      DELETE IT_MARC.
      CONTINUE.
    ENDIF.
    IF IT_L_E_VKKS0[]  IS INITIAL .
      DELETE IT_MARC.
      CONTINUE.
    ENDIF.

    LOOP AT IT_L_E_VKKS0.
* Copying Data
      MOVE-CORRESPONDING IT_MARC TO IT_MA_OBJ.
      IT_MA_OBJ-AUFNR = IT_L_E_VKKS0-AUFNR.
      IT_MA_OBJ-OBJNR = IT_L_E_VKKS0-OBJNR.

* Read Production Version  - PROCNR
      CALL FUNCTION 'KK_F_PKOSA_FIND'
           EXPORTING
                I_MATNR               = IT_MARC-MATNR
                I_WERKS               = IT_MARC-WERKS
                I_PWERK               = IT_MARC-WERKS
                I_PROCNR              = IT_L_E_VKKS0-PROCNR
*               I_NO_OLD_PKOSA        = 'X'
           IMPORTING
                E_VERID               = IT_MA_OBJ-VERID
           EXCEPTIONS
                NONE_FOUND            = 1
                WRONG_INPUT           = 2
                NONE_PICKED           = 3
                WRONG_RULE            = 4
                RSH_NOT_VALID         = 5
                WRONG_CHARACTERISTICS = 6
                NO_RULE               = 7
                VERSION_NOT_VALID     = 8
                OTHERS                = 9.
* Also the Prod. version should be valid
      CLEAR MKAL.
      SELECT SINGLE *  FROM MKAL
                      WHERE MATNR = IT_MARC-MATNR
                        AND WERKS = IT_MARC-WERKS
                        AND VERID = IT_MA_OBJ-VERID.

      IF SY-SUBRC <> 0.
* No assigned Production Version -> MAX Production Version
* request By (Functional Member 2004.02.17)
* Read Max - Production Version
        CLEAR MKAL.
        SELECT SINGLE MAX( VERID )
                         INTO IT_MA_OBJ-VERID
                         FROM MKAL
                        WHERE MATNR = IT_MARC-MATNR
                          AND WERKS = IT_MARC-WERKS.
      ENDIF.

* Making ITAB for CO orders
      COLLECT   IT_MA_OBJ.
      CLEAR     IT_MA_OBJ.
      CLEAR     IT_L_E_VKKS0.
    ENDLOOP.
    CLEAR     IT_MARC.
  ENDLOOP.

  SORT  IT_MA_OBJ BY MATNR WERKS.
  CLEAR IT_MA_OBJ.

* Rid off PCC without Receipt Qty.
  LOOP AT IT_MA_OBJ.
    CLEAR CKMLHD.
    SELECT SINGLE * FROM CKMLHD
                   WHERE MATNR = IT_MA_OBJ-MATNR
                     AND BWKEY = IT_MA_OBJ-WERKS.
    CLEAR MLCD.
    SELECT SINGLE * FROM MLCD
                   WHERE KALNR = CKMLHD-KALNR
                     AND BDATJ = P_GJAHR
                     AND POPER = P_FRPER
                     AND CATEG = 'ZU'
                     AND LBKUM NE SPACE.

    IF SY-SUBRC = 0.
    ELSE.
      DELETE IT_MA_OBJ.
    ENDIF.
    CLEAR IT_MA_OBJ.
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
FORM READ_DATA_PCC.
* CO order number is unique each Material Code.
* Under the Item level, there can be more than one records
* In CO view point, Only the Order number level is considered.

* PCC orders in HMMA are created at Plant Level not production level
* So only one PCC order is created each Material Code.
* (2003.10.14)

* Making OBJ. Key for CCtr + AT (For COSS-PAROB/USPOB)
  PERFORM MAKING_CCTR_AT_OBJ_KEY.

* Read Dynamic Fields Name
  PERFORM READ_FIELD_NAME_FROM_DD_COSS.

* Read DATA from PCC
  PERFORM READ_PCC_DATA.

* Put In the information about Material Code
  PERFORM PUT_MAT_INFO.

* Re-orginize by period
  PERFORM RE_ORG_BY_PER.

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
FORM MAKING_CCTR_AT_OBJ_KEY.
* Making Key Combination : COSS-OBJNR + COSS-PAROB + COSS-USPOB
  DATA : BEGIN OF IT_L_OBJKEY OCCURS 0,
           KOSTL  LIKE CSKS-KOSTL,
           LSTAR  LIKE CSLA-LSTAR,
           OBJNR  LIKE COSS-OBJNR,
         END OF   IT_L_OBJKEY.

  CLEAR   IT_ZTCO_MHHRTRANS.
  CLEAR : IT_L_OBJKEY, IT_L_OBJKEY[].

* Only CCtrs with Variance Quantity in the table 'ZTCO_MHHRTRANS'
  SORT IT_ZTCO_MHHRTRANS BY KOSTL.
  LOOP AT IT_ZTCO_MHHRTRANS.
    ON CHANGE OF IT_ZTCO_MHHRTRANS-KOSTL.
      MOVE-CORRESPONDING  IT_ZTCO_MHHRTRANS
                      TO  IT_L_OBJKEY.
      COLLECT IT_L_OBJKEY.
      CLEAR   IT_L_OBJKEY.
    ENDON.
    CLEAR IT_ZTCO_MHHRTRANS.
  ENDLOOP.

* Get Object Key
  LOOP AT IT_L_OBJKEY.
    CALL FUNCTION 'K_LSTAR_OBJECT_KEY_GET'
         EXPORTING
              KOKRS = P_KOKRS
              KOSTL = IT_L_OBJKEY-KOSTL
              LSTAR = IT_L_OBJKEY-LSTAR
         IMPORTING
              OBJNR = IT_L_OBJKEY-OBJNR.
    MODIFY IT_L_OBJKEY.
    CLEAR  IT_L_OBJKEY.
  ENDLOOP.
  CLEAR   IT_L_OBJKEY.

* Making Key Combination to be used in selecting data from COSS
  DATA : IT_L_TMP_OBJ     LIKE STANDARD TABLE OF  IT_MA_OBJ
                          WITH HEADER LINE .

  IT_L_TMP_OBJ[] = IT_MA_OBJ[].

  CLEAR : IT_MA_OBJ, IT_MA_OBJ[].

  LOOP AT IT_L_TMP_OBJ.
    LOOP AT IT_L_OBJKEY.
      CLEAR IT_MA_OBJ.
      MOVE-CORRESPONDING IT_L_TMP_OBJ TO IT_MA_OBJ.
* PAROB = USPOB.
      IT_MA_OBJ-PAROB = IT_MA_OBJ-USPOB
                      = IT_L_OBJKEY-OBJNR.
      IT_MA_OBJ-KOSTL = IT_L_OBJKEY-KOSTL.
      IT_MA_OBJ-LSTAR = IT_L_OBJKEY-LSTAR.
      APPEND IT_MA_OBJ.
      CLEAR  IT_MA_OBJ.
      CLEAR  IT_L_OBJKEY.
    ENDLOOP.
    CLEAR IT_L_TMP_OBJ.
  ENDLOOP.

  CLEAR   IT_MA_OBJ.
  CLEAR : IT_L_TMP_OBJ, IT_L_TMP_OBJ[]. FREE IT_L_TMP_OBJ.

ENDFORM.                    " MAKING_CCTR_AT_OBJ_KEY

*&---------------------------------------------------------------------*
*&      Form  READ_FIELD_NAME_FROM_DD_COSS
*&---------------------------------------------------------------------*
*       Read Technical FieldName for COSS
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_FIELD_NAME_FROM_DD_COSS.

  CLEAR : IT_ET_FIELDLIST, IT_ET_FIELDLIST[].

* read DD infor. COSS Key Part
  PERFORM READ_DD_INFO  TABLES IT_ET_FIELDLIST
                        USING  'ZSCO_COSS_KEY01'.

* read DD infor. COSS Value Part (Total Quantity)
  PERFORM READ_DD_INFO  TABLES IT_ET_FIELDLIST
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
FORM READ_DD_INFO TABLES   IT_L_ET_FIELDLIST STRUCTURE IT_ET_FIELDLIST
                  USING    P_CI_TABNAME      LIKE GV_CI_TABNAME.
* Local DATA definition
  DATA : IT_L_FDLIST LIKE STANDARD TABLE OF IT_ET_FIELDLIST
                     WITH HEADER LINE.
* Making FDlist
  CLEAR : IT_L_FDLIST,     IT_L_FDLIST[].
  CLEAR GV_CI_TABNAME.
  GV_CI_TABNAME = P_CI_TABNAME.
  CALL FUNCTION 'RECP_DD_TABL_FIELDNAMES_GET'
       EXPORTING
            IC_TABNAME   = GV_CI_TABNAME
       TABLES
            ET_FIELDLIST = IT_L_FDLIST
       EXCEPTIONS
            NOT_FOUND    = 1
            OTHERS       = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  APPEND LINES OF  IT_L_FDLIST       TO IT_L_ET_FIELDLIST.

ENDFORM.                    " READ_DD_INFO

*&---------------------------------------------------------------------*
*&      Form  READ_PCC_DATA
*&---------------------------------------------------------------------*
*       Read PCC data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_PCC_DATA.

  CLEAR : IT_MA_OBJ.

  CLEAR : IT_COSS, IT_COSS[].

* Selection Condition
* Using Table Key Index
* Value Type : WRTTP = '04' - Actual / User input
* Ledger     : LEDNR = '00' Standard Ledger
* COSS-PAROB = COSS-USPOB

  CLEAR COSS.
  SELECT (IT_ET_FIELDLIST)
           INTO CORRESPONDING FIELDS OF TABLE IT_COSS
           FROM COSS
            FOR ALL ENTRIES IN IT_MA_OBJ
          WHERE LEDNR = '00'
            AND OBJNR = IT_MA_OBJ-OBJNR
            AND GJAHR = P_GJAHR
            AND WRTTP = P_WRTTP
            AND VERSN = P_VERSN
            AND PAROB = IT_MA_OBJ-PAROB
            AND USPOB = IT_MA_OBJ-USPOB
            AND VRGNG IN S_VRGNG.

  CLEAR IT_COSS.

  IF IT_COSS[] IS INITIAL .
    MESSAGE E024 WITH P_VERSN P_GJAHR.
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
FORM CAL_PER_COUNT.
* Cal. the Counter
  GV_PERCOUNT = P_TOPER - P_FRPER + 1.

  CLEAR GV_END_DATE .
  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
       EXPORTING
            I_GJAHR = P_GJAHR
            I_PERIV = TKA01-LMONA
            I_POPER = P_FRPER
       IMPORTING
            E_DATE  = GV_END_DATE.

  CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
       EXPORTING
            I_GJAHR = P_GJAHR
            I_PERIV = TKA01-LMONA
            I_POPER = P_FRPER
       IMPORTING
            E_DATE  = GV_STR_DATE.

ENDFORM.                    " CAL_PER_COUNT

*&---------------------------------------------------------------------*
*&      Form  PUT_MAT_INFO
*&---------------------------------------------------------------------*
*       set Material  Information
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PUT_MAT_INFO.
* Sorting
  CLEAR IT_COSS.
  SORT IT_MA_OBJ BY OBJNR  PAROB.
  SORT IT_COSS   BY OBJNR  PAROB.
  LOOP AT IT_COSS.
    CLEAR IT_MA_OBJ.
    READ TABLE IT_MA_OBJ
                        WITH KEY OBJNR = IT_COSS-OBJNR
                                 PAROB = IT_COSS-PAROB
                        BINARY SEARCH.
* Transfer CO objects
    IT_COSS-KOSTL = IT_MA_OBJ-KOSTL.
    IT_COSS-LSTAR = IT_MA_OBJ-LSTAR.
    IT_COSS-AUFNR = IT_MA_OBJ-AUFNR.
* Transfer Material Data
    IT_COSS-MATNR = IT_MA_OBJ-MATNR.
    IT_COSS-MTART = IT_MA_OBJ-MTART.
    IT_COSS-WERKS = IT_MA_OBJ-WERKS.
    IT_COSS-SAUFT = IT_MA_OBJ-SAUFT.
    MODIFY IT_COSS.
    CLEAR IT_COSS.
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
FORM RE_ORG_BY_PER.

  CLEAR IT_COSS.
  CLEAR : IT_COL_PCC, IT_COL_PCC[].

  FIELD-SYMBOLS: <FS1> TYPE ANY.
  DATA : LV_COSS_MEG(30).  " VALUE 'IT_COSS-'.
  DATA : LV_CNT  LIKE  COSS-PERBL.

  LOOP AT IT_COSS.
* Key Part
    IT_COL_PCC-AUFNR =   IT_COSS-AUFNR.
    IT_COL_PCC-KOSTL =   IT_COSS-KOSTL.
    IT_COL_PCC-LSTAR =   IT_COSS-LSTAR.
* Unit
    IT_COL_PCC-MEINH =   IT_COSS-MEINH.
* Period Counter : Set From-Period .
    CLEAR LV_CNT.
    LV_CNT = P_FRPER .
* Value Part
    DO GV_PERCOUNT TIMES.
* Period
      IT_COL_PCC-PERID = LV_CNT.
* Value
      CLEAR LV_COSS_MEG.
      CONCATENATE 'IT_COSS-'  'MEG'   LV_CNT
             INTO LV_COSS_MEG.
      ASSIGN (LV_COSS_MEG) TO <FS1>.
      IT_COL_PCC-MEGXXX  = <FS1>.
* Collect
      COLLECT IT_COL_PCC.
* Period Counter
      LV_CNT = LV_CNT + 1.
    ENDDO.
    CLEAR IT_COSS.
    CLEAR IT_COL_PCC.
  ENDLOOP.

  CLEAR   IT_COL_PCC.
* Remove records if qunatity value has initial value.
  DELETE  IT_COL_PCC WHERE MEGXXX EQ SPACE.

ENDFORM.                    " RE_ORG_BY_PER

*&---------------------------------------------------------------------*
*&      Form  CAL_QUAN_RATIO
*&---------------------------------------------------------------------*
*       Calculate Quantity Ratio
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CAL_QUAN_RATIO.
* In case that there is no COSS quantity . there will be no ratio data
* At that case, The variance  AT Quantity should not be distributed
* (Requested By functional Member 2003.10.16)

* AT Quantity In COSS   : IT_COL_PCC
* Varaince AT Quantity  : IT_ZTCO_MHHRTRANS
  LOOP AT IT_COL_PCC.
    CLEAR IT_ZTCO_MHHRTRANS.
    READ TABLE IT_ZTCO_MHHRTRANS WITH KEY PERID = IT_COL_PCC-PERID
                                          KOSTL = IT_COL_PCC-KOSTL
                                          LSTAR = IT_COL_PCC-LSTAR.
    IF SY-SUBRC = 0.
      IT_COL_PCC-VAEQTY = IT_ZTCO_MHHRTRANS-VAEQTY.
      IT_COL_PCC-UNIT   = IT_ZTCO_MHHRTRANS-UNIT.
    ELSE. " only Unit def
      IT_COL_PCC-UNIT   = 'STD'.
    ENDIF.
    MODIFY IT_COL_PCC.
    CLEAR  IT_COL_PCC.
  ENDLOOP.
  CLEAR IT_COL_PCC.

* Unit Conversion / total.
  DATA : BEGIN OF IT_L_TOTAL OCCURS 0,
   PERID LIKE IT_COL_PCC-PERID,
   KOSTL LIKE IT_COL_PCC-KOSTL,
   LSTAR LIKE IT_COL_PCC-LSTAR,
   TOMEG LIKE IT_COL_PCC-TOMEG,
   MEINH LIKE IT_COL_PCC-MEINH.
  DATA : END OF IT_L_TOTAL.

  SORT IT_COL_PCC BY PERID  KOSTL  LSTAR.

  LOOP AT IT_COL_PCC.
* Unit Conversion
* Already AT 'STD' is used in  IT_ZTCO_MHHRTRANS-UNIT
* Check Previous program -> ZACO03U_MHAM
    IF  IT_COL_PCC-MEINH <> IT_COL_PCC-UNIT.
      PERFORM UNIT_CONV USING IT_COL_PCC-MEINH
                              IT_COL_PCC-UNIT
                              IT_COL_PCC-MEGXXX .
      MODIFY IT_COL_PCC.
    ENDIF.
* Total
    MOVE-CORRESPONDING IT_COL_PCC TO IT_L_TOTAL.
*---start #1 wskim 03/09/05
*    it_l_total-tomeg = it_col_pcc-megxxx.
*     COLLECT  it_l_total.
    IF IT_COL_PCC-MEGXXX >= 0.
      IT_L_TOTAL-TOMEG = IT_COL_PCC-MEGXXX.
      COLLECT  IT_L_TOTAL.
      CLEAR    IT_L_TOTAL.
    ENDIF.
*---end
    CLEAR IT_COL_PCC.
  ENDLOOP.
  CLEAR    IT_L_TOTAL.

* Calculate Ratio / Output Quantity
  LOOP AT IT_COL_PCC.
    CLEAR    IT_L_TOTAL.
    READ TABLE IT_L_TOTAL WITH KEY    PERID = IT_COL_PCC-PERID
                                      KOSTL = IT_COL_PCC-KOSTL
                                      LSTAR = IT_COL_PCC-LSTAR.
    IT_COL_PCC-TOMEG         = IT_L_TOTAL-TOMEG.
*---Start#1 wskim 03/09/2005:wip error - don't allowed '-' quantity
*   IT_COL_PCC-RATE_%        = IT_COL_PCC-MEGXXX / IT_COL_PCC-TOMEG.
    IF IT_COL_PCC-MEGXXX =< 0.
      IT_COL_PCC-RATE_%  = 0.
    ELSE.
      IT_COL_PCC-RATE_%  = IT_COL_PCC-MEGXXX / IT_COL_PCC-TOMEG.
    ENDIF.
*---end
    IT_COL_PCC-MEGXXX_RATE_% = IT_COL_PCC-RATE_% * IT_COL_PCC-VAEQTY.
    MODIFY IT_COL_PCC.
    CLEAR  IT_COL_PCC.
  ENDLOOP.
  CLEAR  IT_COL_PCC.
* No Value , remove it
  DELETE IT_COL_PCC WHERE MEGXXX_RATE_% EQ SPACE.

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
FORM UNIT_CONV USING    P_UNIT
                        P_OUTUNIT
                        P_QTY.
  CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
   EXPORTING
     INPUT                      = P_QTY
*    NO_TYPE_CHECK              = 'X'
*    ROUND_SIGN                 = ' '
     UNIT_IN                    = P_UNIT
     UNIT_OUT                   = P_OUTUNIT
   IMPORTING
*    ADD_CONST                  =
*    DECIMALS                   =
*    DENOMINATOR                =
*    NUMERATOR                  =
     OUTPUT                     = P_QTY
   EXCEPTIONS
     CONVERSION_NOT_FOUND       = 1
     DIVISION_BY_ZERO           = 2
     INPUT_INVALID              = 3
     OUTPUT_INVALID             = 4
     OVERFLOW                   = 5
     TYPE_INVALID               = 6
     UNITS_MISSING              = 7
     UNIT_IN_NOT_FOUND          = 8
     UNIT_OUT_NOT_FOUND         = 9
     OTHERS                     = 10.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  P_UNIT = P_OUTUNIT.

ENDFORM.                    " UNIT_CONV

*&---------------------------------------------------------------------*
*&      Form  TRANS_IT_ZTCO_MHPCPOST
*&---------------------------------------------------------------------*
*       Transferring Key Part
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM TRANS_IT_ZTCO_MHPCPOST.

*        PERID   LIKE RKU01G-PERBI, "Period
*        AUFNR   LIKE AFPO-AUFNR,
*        KOSTL   LIKE ANLP-KOSTL,
*        LSTAR   LIKE CSLA-LSTAR,
*
*        MEINH   LIKE COSS-MEINH,
*        MEGXXX_RATE_%
*            RATE_%  TYPE P DECIMALS 6 ,

  CLEAR : IT_ZTCO_MHPCPOST, IT_ZTCO_MHPCPOST[].

  SORT IT_MA_OBJ BY AUFNR.

  LOOP AT IT_COL_PCC.
    IT_ZTCO_MHPCPOST-GJAHR   = P_GJAHR.
    IT_ZTCO_MHPCPOST-PERID   = IT_COL_PCC-PERID.
    IT_ZTCO_MHPCPOST-AUFNR   = IT_COL_PCC-AUFNR.
    IT_ZTCO_MHPCPOST-KOSTL   = IT_COL_PCC-KOSTL.
    IT_ZTCO_MHPCPOST-LSTAR   = IT_COL_PCC-LSTAR.
    IT_ZTCO_MHPCPOST-MEINH   = IT_COL_PCC-UNIT.
    IT_ZTCO_MHPCPOST-VARQUAN = IT_COL_PCC-MEGXXX_RATE_%.
    IT_ZTCO_MHPCPOST-RATE    = IT_COL_PCC-RATE_%.
    IT_ZTCO_MHPCPOST-MEGXXX  = IT_COL_PCC-MEGXXX.
    CLEAR IT_MA_OBJ.
    READ TABLE IT_MA_OBJ WITH KEY AUFNR = IT_ZTCO_MHPCPOST-AUFNR.
    IT_ZTCO_MHPCPOST-MATNR   = IT_MA_OBJ-MATNR.
    IT_ZTCO_MHPCPOST-WERKS   = IT_MA_OBJ-WERKS.
    IT_ZTCO_MHPCPOST-SAUFT   = IT_MA_OBJ-SAUFT.
*// Mod. By Hyung Jin Youn 2004.02.13
* include REM profile
    IT_ZTCO_MHPCPOST-SFEPR   = IT_MA_OBJ-SFEPR.
    IT_ZTCO_MHPCPOST-VERID   = IT_MA_OBJ-VERID.
*// End of Mod.
    APPEND IT_ZTCO_MHPCPOST.
    CLEAR  IT_ZTCO_MHPCPOST.
    CLEAR  IT_COL_PCC.
  ENDLOOP.

  CLEAR IT_ZTCO_MHPCPOST.
ENDFORM.                    " TRANS_IT_ZTCO_MHPCPOST

*&---------------------------------------------------------------------*
*&      Form  UPDATE_ZTCO_MHPCPOST
*&---------------------------------------------------------------------*
*       Update Table
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_ZTCO_MHPCPOST.
  LOOP AT IT_ZTCO_MHPCPOST.
    CLEAR ZTCO_MHPCPOST.
    MOVE-CORRESPONDING IT_ZTCO_MHPCPOST TO ZTCO_MHPCPOST.
* Get Number (MH Doc)
    PERFORM GET_NUMBER_FROM_SAP.
* Inserting Log
    ZTCO_MHPCPOST-ERDAT = SY-DATUM.
    ZTCO_MHPCPOST-ERZET = SY-UZEIT.
    ZTCO_MHPCPOST-ERNAM = SY-UNAME.
* Insertion
    INSERT ZTCO_MHPCPOST.
    IF SY-SUBRC <> 0.
      ROLLBACK  WORK.
      MESSAGE E045 WITH 'ZTCO_MHPCPOST' ZTCO_MHPCPOST-GJAHR
                        IT_ZTCO_MHPCPOST-PERID .
    ENDIF.
    CLEAR IT_ZTCO_MHPCPOST.
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
FORM ENQUEUE_ZTCO_MHPCPOST.

  DATA : LV_PERID LIKE ZTCO_MHHRTRANS-PERID.

  LV_PERID = P_FRPER.

  DO 16 TIMES .
    IF LV_PERID =< P_TOPER.
      CALL FUNCTION 'ENQUEUE_EZCO_MHPCPOST'
       EXPORTING
        MODE_ZTCO_MHPCPOST       = 'E'
        MANDT                    = SY-MANDT
        GJAHR                    = P_GJAHR
        PERID                    = LV_PERID
*       MATNR                    =
*       WERKS                    =
*       AUFNR                    =
*       KOSTL                    =
*       LSTAR                    =
*       X_GJAHR                  = ' '
*       X_PERID                  = ' '
*       X_MATNR                  = ' '
*       X_WERKS                  = ' '
*       X_AUFNR                  = ' '
*       X_KOSTL                  = ' '
*       X_LSTAR                  = ' '
        _SCOPE                   = '3'
*       _WAIT                    = ' '
*       _COLLECT                 = ' '
       EXCEPTIONS
         FOREIGN_LOCK             = 1
         SYSTEM_FAILURE           = 2
         OTHERS                   = 3.

      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
    ENDIF.
* Period Counting
    LV_PERID = LV_PERID  + 1.

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
FORM GET_NUMBER_FROM_SAP.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      NR_RANGE_NR                   = '01'
      OBJECT                        = 'ZCO_NR_MHD'
*     QUANTITY                      = '1'
*     SUBOBJECT                     = ' '
*     TOYEAR                        = '0000'
*     IGNORE_BUFFER                 = ' '
    IMPORTING
      NUMBER                        = ZTCO_MHPCPOST-MHDOC
*     QUANTITY                      =
*     RETURNCODE                    =
    EXCEPTIONS
      INTERVAL_NOT_FOUND            = 1
      NUMBER_RANGE_NOT_INTERN       = 2
      OBJECT_NOT_FOUND              = 3
      QUANTITY_IS_0                 = 4
      QUANTITY_IS_NOT_1             = 5
      INTERVAL_OVERFLOW             = 6
      BUFFER_OVERFLOW               = 7
      OTHERS                        = 8.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
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
FORM POST_PROCESS.

* Check remained not-posted records .
  PERFORM CHECK_NOT_POST_RC.

  CASE GV_NEW.
    WHEN 'X'.
      MESSAGE I000 WITH 'New Run, DATA will be created'.
* Read data from ZTCO_MHHRTRANS
      PERFORM READ_FR_ZTCO_MHHRTRANS.
* Read Material Data
      PERFORM READ_FR_MARA_MARC.
* Read PCC order
      PERFORM READ_PCC_ORDER.
* Read Data from PCC
      PERFORM READ_DATA_PCC.
* Calculte the ratio
      PERFORM CAL_QUAN_RATIO.
* Transfer data to IT_ZTCO_MHPCPOST
      PERFORM TRANS_IT_ZTCO_MHPCPOST.
* Insert  ZTCO_MHPCPOST.
      PERFORM UPDATE_ZTCO_MHPCPOST.
    WHEN OTHERS. "<- Space
      MESSAGE I000 WITH 'Re-Run, Some data were not posted'.
  ENDCASE.

* POSTING PART <- Important !!
  PERFORM POSTING_USING_PP_PGM.

* Put Results into IT_ZTCO_MHPCPOST
* from IT_PO_POST & IT_REM_POST & IT_DI_POST
  PERFORM PUT_RESULT_INTO_IT_ZTCO_MHPCPO.


ENDFORM.                    " POST_PROCESS

*&---------------------------------------------------------------------*
*&      Form  REVERSE_PROCESS
*&---------------------------------------------------------------------*
*       Reversing
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REVERSE_PROCESS.

* Read data to be reversed
  PERFORM READ_DATA_TO_REVERSE.

* Reverse .
  PERFORM REVERSE_ACT_MTO_MTS.

ENDFORM.                    " REVERSE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  READ_DATA_TO_REVERSE
*&---------------------------------------------------------------------*
*       Read data to be reversed
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA_TO_REVERSE.

* Renewal
  CLEAR : IT_ZTCO_MHPCPOST, IT_ZTCO_MHPCPOST[].
* read data : REVERSED = SPACE
  CLEAR ZTCO_MHPCPOST.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTCO_MHPCPOST
           FROM ZTCO_MHPCPOST
          WHERE GJAHR = P_GJAHR
            AND PERID BETWEEN P_FRPER AND P_TOPER
            AND REVERSED = SPACE
            AND MHDOC = SPACE.

  IF IT_ZTCO_MHPCPOST[] IS INITIAL.
    MESSAGE E054 WITH 'ZTCO_MHPCPOST' P_GJAHR P_FRPER P_TOPER.
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
FORM DEFAULT_VALUE_S_MTART.
  CLEAR : S_MTART, S_MTART[].

  S_MTART-LOW  = 'FERT'.
  S_MTART-SIGN = 'I'.
  S_MTART-OPTION  = 'EQ'.
  APPEND S_MTART. CLEAR S_MTART.

  S_MTART-LOW = 'HALB'.
  S_MTART-SIGN = 'I'.
  S_MTART-OPTION  = 'EQ'.
  APPEND S_MTART. CLEAR S_MTART.

ENDFORM.                    " DEFAULT_VALUE_S_MTART

*&---------------------------------------------------------------------*
*&      Form  SET_MFBF_INIT
*&---------------------------------------------------------------------*
*       Set MFBF Variant
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_MFBF_INIT.
  CLEAR RMUSER_TAV.
  SELECT SINGLE * FROM RMUSER_TAV
                 WHERE USERNAME = SY-UNAME
                   AND TCODE    = 'MFBF'.
  IF SY-SUBRC <> 0.
    RMUSER_TAV-USERNAME = SY-UNAME.
    RMUSER_TAV-TCODE    = 'MFBF'.
    RMUSER_TAV-TVARIANT = SPACE.	
    RMUSER_TAV-SCENARIO = 'LAGER'.
    RMUSER_TAV-TYPE     = 'L'.
    RMUSER_TAV-ZPKT     = SPACE.

    INSERT RMUSER_TAV.
  ELSE.
    RMUSER_TAV-TVARIANT = SPACE.	
    RMUSER_TAV-SCENARIO = 'LAGER'.
    RMUSER_TAV-TYPE     = 'L'.
    RMUSER_TAV-ZPKT     = SPACE.

    MODIFY RMUSER_TAV.
  ENDIF.

ENDFORM.                    " SET_MFBF_INIT

*----------------------------------------------------------------------*
*   INCLUDE ZACO18L_F001                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  READ_CEGRP_GROUP
*&---------------------------------------------------------------------*
*       Search Help for Cost element Group / CCTr Group
*----------------------------------------------------------------------*
*  -->  p_class      Class Name
*  <--  P_SET_NAME   Result Group Name
*----------------------------------------------------------------------*
FORM READ_CEGRP_GROUP  USING P_CLASS
                             P_SET_NAME.
  CALL FUNCTION 'K_GROUP_SELECT'
    EXPORTING
*     BUTTONS                  = 'X'
      CLASS                    = P_CLASS
*     CRUSER                   = '*'
      FIELD_NAME               = SPACE
*     SEARCHFLD                = '    '
*     SEARCHFLD_INPUT          = 'X'
      SEARCHFLD_REQUIRED       = 'X'
*     SET                      = '*'
*     START_COLUMN             = 10
*     START_ROW                = 5
*     TABLE                    = 'CCSS'
*     TYPELIST                 = 'BS'
*     UPDUSER                  = '*'
*     KOKRS                    =
*     KTOPL                    =
    IMPORTING
*     CLASS_NAME               =
      SET_NAME                 = P_SET_NAME
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

ENDFORM.                    " READ_CEGRP_GROUP

*&---------------------------------------------------------------------*
*&      Form  CHK_INPUT_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHK_INPUT_VALUE.

* Check Input Value (Period)
  IF P_FRPER > P_TOPER.
    MESSAGE E003(ZMCO) WITH P_FRPER P_TOPER.
  ENDIF.

  IF P_FRPER < 0 OR P_FRPER > 12.
    MESSAGE E007(ZMCO) WITH P_FRPER .
  ENDIF.

  IF P_TOPER < 0 OR P_TOPER > 12.
    MESSAGE E007(ZMCO) WITH P_TOPER.
  ENDIF.

* Check TEST-RUN  Flag
  IF P_TRUN NA 'X '.
    MESSAGE E008(ZMCO).
  ENDIF.

* Check Currency IND.
  IF P_CURRT NA 'CTO'.
    MESSAGE E000(ZMCO) WITH P_CURRT ' is not a posible value' .
  ENDIF.

* Check Cost element/Cost element Group
  IF     S_KSTAR[] IS INITIAL
     AND P_CEGRP   IS INITIAL .
    MESSAGE E012(ZMCO).
  ELSEIF
         NOT S_KSTAR[] IS INITIAL
     AND NOT P_CEGRP   IS INITIAL .
    MESSAGE E013(ZMCO).
  ENDIF.

* Check Cost Center/Cost Center Group
  IF     S_KOSTL[] IS INITIAL
     AND P_NCOAL   IS INITIAL .
    MESSAGE E016(ZMCO).
  ELSEIF
         NOT S_KOSTL[] IS INITIAL
     AND NOT P_NCOAL   IS INITIAL .
    MESSAGE E017(ZMCO).
  ENDIF.

ENDFORM.                    " CHK_INPUT_VALUE

*&---------------------------------------------------------------------*
*&      Form  READ_CCTRS_FROM_HMMA
*&---------------------------------------------------------------------*
*       Read CCtrs from 'HMMA'. All CCtrs should be considered.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_CCTRS_FROM_HMMA.
* Making an internal table for CCtr to select data
* All CCtrs (Standard CCtr group 'HMMA' ) or Selected Group on screen
  CLEAR : IT_COSTCENTERLIST, IT_COSTCENTERLIST[].
  CLEAR : IT_RETURN, IT_RETURN[].

* Set Validity Date (Start)
  DATA : LV_DATUM LIKE SY-DATUM.
  CONCATENATE P_GJAHR P_FRPER+1(2) '01' INTO LV_DATUM.

* From CCtr Group
  IF NOT P_NCOAL IS INITIAL.
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
  ENDIF.

* From CCtrs on selection screen
* From Select-options.
  DATA : IT_L_CCTR LIKE STANDARD TABLE OF IT_COSTCENTERLIST
                 WITH HEADER LINE.

  IF NOT S_KOSTL[] IS INITIAL.
    LOOP AT S_KOSTL.
      CLEAR : IT_L_CCTR, IT_L_CCTR[].
      CALL FUNCTION 'BAPI_COSTCENTER_GETLIST1'
           EXPORTING
                CONTROLLINGAREA = P_KOKRS
                DATE_FROM       = LV_DATUM
                COSTCENTER_FROM = S_KOSTL-LOW
                COSTCENTER_TO   = S_KOSTL-HIGH
           TABLES
                COSTCENTERLIST  = IT_L_CCTR
                RETURN          = IT_RETURN.
* Message
      PERFORM DIS_BAPI_MESSAGE.
* Appending CE list
      APPEND LINES OF IT_L_CCTR  TO IT_COSTCENTERLIST.
      CLEAR IT_COSTCENTERLIST.
      CLEAR S_KOSTL.
    ENDLOOP.
  ENDIF.

* Sorting
  SORT IT_COSTCENTERLIST BY CO_AREA COSTCENTER.
  DELETE ADJACENT DUPLICATES FROM IT_COSTCENTERLIST.

* Check CCtr
  IF IT_COSTCENTERLIST[] IS INITIAL .
    MESSAGE E018(ZMCO) WITH 'Cost Center'.
  ENDIF.

ENDFORM.                    " READ_CCTRS_FROM_HMMA

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
*&      Form  READ_COST_ELEMENT
*&---------------------------------------------------------------------*
*       Read Cost Elements
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_COST_ELEMENT.

  CLEAR : IT_COSTELEMENTLIST, IT_COSTELEMENTLIST[].
  CLEAR : IT_RETURN, IT_RETURN[].

* Set Validity Date (Start)
  DATA : LV_DATUM LIKE SY-DATUM.
  CONCATENATE P_GJAHR P_FRPER+1(2) '01' INTO LV_DATUM.

* From Select-options.
  DATA : IT_L_CE LIKE STANDARD TABLE OF IT_COSTELEMENTLIST
                 WITH HEADER LINE.

  IF NOT S_KSTAR[] IS INITIAL.
    LOOP AT S_KSTAR.
      CLEAR : IT_L_CE, IT_L_CE[].
      CALL FUNCTION 'BAPI_COSTELEM_GETLIST'
           EXPORTING
                COAREA          = P_KOKRS
                DATE            = LV_DATUM
                COSTELEMENTFROM = S_KSTAR-LOW
                COSTELEMENTTO   = S_KSTAR-HIGH
           TABLES
                COSTELEMENTLIST = IT_L_CE
                RETURN          = IT_RETURN.
* Message
      PERFORM DIS_BAPI_MESSAGE.
* Appending CE list
      APPEND LINES OF IT_L_CE  TO IT_COSTELEMENTLIST.
      CLEAR IT_COSTELEMENTLIST.

      CLEAR S_KSTAR.
    ENDLOOP.
  ENDIF.

* From CEgroup
  IF NOT P_CEGRP   IS INITIAL .
    CALL FUNCTION 'BAPI_COSTELEM_GETLIST'
         EXPORTING
              COAREA           = P_KOKRS
              DATE             = LV_DATUM
              COSTELEMENTGROUP = P_CEGRP
         TABLES
              COSTELEMENTLIST  = IT_COSTELEMENTLIST
              RETURN           = IT_RETURN.
* Message
    PERFORM DIS_BAPI_MESSAGE.
  ENDIF.

* Checking Duplicated Records
  SORT IT_COSTELEMENTLIST BY CO_AREA COST_ELEM.
  DELETE ADJACENT DUPLICATES FROM IT_COSTELEMENTLIST.

* Check CE
  IF IT_COSTELEMENTLIST[] IS INITIAL .
    MESSAGE E018(ZMCO) WITH 'Cost Element'.
  ENDIF.

ENDFORM.                    " READ_COST_ELEMENT

*&---------------------------------------------------------------------*
*&      Form  READ_COM_OBJ
*&---------------------------------------------------------------------*
*       Read OBJ Key Combination
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_COM_OBJ.

  CLEAR : IT_OBJ_CCTR,     IT_OBJ_CCTR[],
          IT_OBJ_CCTR_AT,  IT_OBJ_CCTR_AT[],
          IT_OBJ_ALL,      IT_OBJ_ALL[].

  LOOP AT IT_COSTCENTERLIST.
* CCtr
    CALL FUNCTION 'K_KOSTL_OBJECT_KEY_GET'
         EXPORTING
              KOKRS = P_KOKRS
              KOSTL = IT_COSTCENTERLIST-COSTCENTER
         IMPORTING
              OBJNR = IT_OBJ_CCTR-OBJNR.

    IT_OBJ_CCTR-KOSTL = IT_COSTCENTERLIST-COSTCENTER.

    APPEND IT_OBJ_CCTR. CLEAR IT_OBJ_CCTR.
    CLEAR IT_COSTCENTERLIST.
  ENDLOOP.


* CCtr + AT ( Not using Object Key FM for performance)
  CLEAR CSSL.
  SELECT  OBJNR KOSTL LSTAR
                      INTO CORRESPONDING FIELDS OF TABLE IT_OBJ_CCTR_AT
                      FROM CSSL
                     FOR ALL ENTRIES IN IT_COSTCENTERLIST
                     WHERE KOKRS = P_KOKRS
                       AND KOSTL = IT_COSTCENTERLIST-COSTCENTER
                       AND GJAHR = P_GJAHR.

* For all Obj keys
  APPEND LINES OF  IT_OBJ_CCTR       TO IT_OBJ_ALL.
  APPEND LINES OF  IT_OBJ_CCTR_AT    TO IT_OBJ_ALL.

ENDFORM.                    " READ_COM_OBJ

*&---------------------------------------------------------------------*
*&      Form  READ_QUAN_INFO
*&---------------------------------------------------------------------*
*       Read Quantity data from COSS
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_QUAN_INFO.
* Total Quantity in COSS can be readed
* only when the records have Ativity Type
* So CCtr+AT object key should be used
* IT_OBJ_CCTR_AT

* Read Dynamic Fields Name
  PERFORM READ_FIELD_NAME_FROM_DD_COSS.

* Read Quantity DATA from COSS
  PERFORM READ_QUAN_DATA_FR_COSS.

ENDFORM.                    " READ_QUAN_INFO

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
*&      Form  READ_QUAN_DATA_FR_COSS
*&---------------------------------------------------------------------*
*       Read Quantity DATA from COSS
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_QUAN_DATA_FR_COSS.
* Internal Tables in this Code Block
* IT_OBJ_CCTR_AT IT_QUAN_PLAN IT_QUAN_STD

* For Plan Version
  PERFORM READ_COSS_TOT_QUAN TABLES IT_QUAN_PLAN
                             USING P_FRVER.   "Plan Verion
* For Standard Version
  PERFORM READ_COSS_TOT_QUAN TABLES IT_QUAN_STD
                             USING P_TOVER.   "Std. Verion

ENDFORM.                    " READ_QUAN_DATA_FR_COSS

*&---------------------------------------------------------------------*
*&      Form  READ_COSS_TOT_QUAN
*&---------------------------------------------------------------------*
*       To read COSS table
*----------------------------------------------------------------------*
*      -->IT_L_COSS  Target ITAB
*      -->P_VER      Plan/Standard Version
*----------------------------------------------------------------------*
FORM READ_COSS_TOT_QUAN TABLES   IT_L_COSS  STRUCTURE IT_QUAN_PLAN
                        USING    P_VER.
  CLEAR : IT_L_COSS, IT_L_COSS[].
  CLEAR COSS.
  SELECT (IT_ET_FIELDLIST)
         INTO CORRESPONDING FIELDS OF TABLE IT_L_COSS
         FROM COSS
          FOR ALL ENTRIES IN IT_OBJ_CCTR_AT
        WHERE LEDNR = '00'
          AND OBJNR = IT_OBJ_CCTR_AT-OBJNR
          AND GJAHR = P_GJAHR
          AND WRTTP = '01'
          AND VERSN = P_VER
          AND USPOB = IT_OBJ_CCTR_AT-OBJNR.
  CLEAR IT_L_COSS.

* Consider Debit/Credit and Allocated Quantity.
*  - 2003.11.06
  DATA : IT_TMP_L_COSS LIKE STANDARD TABLE OF  IT_L_COSS
                       WITH HEADER LINE .
  DATA : WA_L_COSS_MEG01 LIKE ZSCO_COSS_MEG01.
  DATA : WA_L_COSS_MINUS LIKE ZSCO_COSS_MEG01.

  CLEAR : IT_TMP_L_COSS, IT_TMP_L_COSS[].
  LOOP AT IT_L_COSS.
    MOVE-CORRESPONDING IT_L_COSS TO IT_TMP_L_COSS.
* Debit Credit
    CASE IT_TMP_L_COSS-BEKNZ.
      WHEN 'O' OR 'C' OR 'S'.
        MINUS_VALUE 001. MINUS_VALUE 002. MINUS_VALUE 003.
        MINUS_VALUE 004. MINUS_VALUE 005. MINUS_VALUE 006.
        MINUS_VALUE 007. MINUS_VALUE 008. MINUS_VALUE 009.
        MINUS_VALUE 010. MINUS_VALUE 011. MINUS_VALUE 012.
        MINUS_VALUE 013. MINUS_VALUE 014. MINUS_VALUE 015.
        MINUS_VALUE 016.
      WHEN OTHERS. " 'D'.

    ENDCASE.
* Clear not useful fields
    CLEAR IT_TMP_L_COSS-HRKFT.
    CLEAR IT_TMP_L_COSS-VRGNG.
    CLEAR IT_TMP_L_COSS-PAROB.
    CLEAR IT_TMP_L_COSS-BEKNZ.

    COLLECT IT_TMP_L_COSS.
    CLEAR   IT_TMP_L_COSS.
  ENDLOOP.

* Replace to Summed data
  CLEAR : IT_L_COSS, IT_L_COSS[].
  IT_L_COSS[] = IT_TMP_L_COSS[].
  CLEAR : IT_TMP_L_COSS, IT_TMP_L_COSS[].

ENDFORM.                    " READ_COSS_TOT_QUAN

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

ENDFORM.                    " CAL_PER_COUNT

*&---------------------------------------------------------------------*
*&      Form  MAKING_ALL_FAC
*&---------------------------------------------------------------------*
*       Building Allocation Factor From Quantity Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKING_ALL_FAC.
*IT_OBJ_CCTR_AT IT_QUAN_PLAN IT_QUAN_STD

  SORT IT_OBJ_CCTR_AT BY OBJNR.
  SORT IT_QUAN_PLAN   BY OBJNR.
  SORT IT_QUAN_STD    BY OBJNR.

* Calculation Formula
* -> STD quantity / PLAN quantity
* If STD quantity = '0' or PLAN quantity = '0',
* then The factor is set as 0.
* Therefore Plan data is always the base factor to calculate .

* Check Plan DATA (Quan.)
  IF IT_QUAN_PLAN[] IS INITIAL.
    MESSAGE E014(ZMCO) WITH P_FRVER.
  ENDIF.

* Check Initial Value in Plan Data
  PERFORM REMOVE_INI_DATA_FR_PL_QUAN.

* Calculating Ratio
  PERFORM CAL_RATE.

* Check IT_RATE
  IF IT_RATE[] IS INITIAL.
    MESSAGE E015(ZMCO) WITH P_GJAHR P_FRPER P_TOPER.
  ENDIF.

ENDFORM.                    " MAKING_ALL_FAC

*&---------------------------------------------------------------------*
*&      Form  REMOVE_INI_DATA_FR_PL_QUAN
*&---------------------------------------------------------------------*
*       Check Initial Value in Plan Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REMOVE_INI_DATA_FR_PL_QUAN.
* Local Data definition
  FIELD-SYMBOLS: <FS1> TYPE ANY.
  DATA : LV_MEG_PLN(30).
  DATA : LV_CNT  LIKE  COSP-PERBL.
  DATA : LV_MEG LIKE COSS-MEG001.

  LOOP AT IT_QUAN_PLAN.
* Period Counter : Set From-Period .
    CLEAR LV_CNT.
    LV_CNT = P_FRPER .
    CLEAR LV_MEG.
* Summing Quantity within the selected period
    DO GV_PERCOUNT TIMES.
      CLEAR LV_MEG_PLN.
      CONCATENATE 'IT_QUAN_PLAN-'  GV_FIELDGROUP_MEG  LV_CNT
             INTO LV_MEG_PLN.
      ASSIGN (LV_MEG_PLN) TO <FS1>.
* Summation
      LV_MEG = LV_MEG + <FS1>.
* Period Counter
      LV_CNT = LV_CNT + 1.
    ENDDO.

    IF LV_MEG EQ SPACE.
* Remove the data
      DELETE IT_QUAN_PLAN.
    ENDIF.
    CLEAR IT_QUAN_PLAN.
  ENDLOOP.
ENDFORM.                    " REMOVE_INI_DATA_FR_PL_QUAN

*&---------------------------------------------------------------------*
*&      Form  cal_rate
*&---------------------------------------------------------------------*
*       Calculating Ratio
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CAL_RATE.
* Making Ratio Table
  CLEAR : IT_RATE, IT_RATE[].
* Local Data Definition.
  FIELD-SYMBOLS: <FS1> TYPE ANY,
                 <FS2> TYPE ANY,
                 <FS3> TYPE ANY.
  DATA : LV_MEG_PLN(30).
  DATA : LV_MEG_STD(30).
  DATA : LV_MEG_RAT(30).
  DATA : LV_CNT  LIKE  COSP-PERBL.

  LOOP AT IT_QUAN_PLAN.
* key Part
    MOVE-CORRESPONDING IT_QUAN_PLAN TO IT_RATE.
* Read data from STD Table.
    CLEAR IT_QUAN_STD.
    READ TABLE IT_QUAN_STD WITH KEY
                                OBJNR = IT_QUAN_PLAN-OBJNR
                                LEDNR = IT_QUAN_PLAN-LEDNR
                                GJAHR = IT_QUAN_PLAN-GJAHR
                                WRTTP = IT_QUAN_PLAN-WRTTP
*                               VERSN =
                                KSTAR = IT_QUAN_PLAN-KSTAR
                                HRKFT = IT_QUAN_PLAN-HRKFT
                                VRGNG = IT_QUAN_PLAN-VRGNG
                                PAROB = IT_QUAN_PLAN-PAROB
                                USPOB = IT_QUAN_PLAN-USPOB
                                BEKNZ = IT_QUAN_PLAN-BEKNZ
                                TWAER = IT_QUAN_PLAN-TWAER
                                PERBL = IT_QUAN_PLAN-PERBL.
    IF SY-SUBRC <> 0.
* IF no data found, it should not be included
      CONTINUE.
    ENDIF.
* Period Counter : Set From-Period .
    CLEAR LV_CNT.
    LV_CNT = P_FRPER .

* Calculating
    DO GV_PERCOUNT TIMES.
* For Plan
      CLEAR LV_MEG_PLN.
      CONCATENATE 'IT_QUAN_PLAN-'  GV_FIELDGROUP_MEG  LV_CNT
             INTO LV_MEG_PLN.
      ASSIGN (LV_MEG_PLN) TO <FS1>.
* For STD
      CLEAR LV_MEG_STD.
      CONCATENATE 'IT_QUAN_STD-'   GV_FIELDGROUP_MEG  LV_CNT
             INTO LV_MEG_STD.
      ASSIGN (LV_MEG_STD) TO <FS2>.
* For Ratio Table
      CLEAR LV_MEG_RAT.
      CONCATENATE 'IT_RATE-'       GV_FIELDGROUP_RAT  LV_CNT
             INTO LV_MEG_RAT.
      ASSIGN (LV_MEG_RAT) TO <FS3>.

* CAL. Rate
* Set Rate = '0' if Plan data = '0' .
      IF   <FS1> IS INITIAL.
        CLEAR <FS3>.
      ELSE.
        <FS3> = <FS2> / <FS1> .
      ENDIF.
* Period Counter
      LV_CNT = LV_CNT + 1.
    ENDDO.

* Appending to Ratio Table
    APPEND IT_RATE.
    CLEAR  IT_RATE.
    CLEAR  IT_QUAN_PLAN.
  ENDLOOP.

  CLEAR IT_RATE.
ENDFORM.                    " cal_rate

*&---------------------------------------------------------------------*
*&      Form  READ_AMT_DATA
*&---------------------------------------------------------------------*
*       Read Amount DATA from COSP
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_AMT_DATA.

* Read Dynamic Fields Name
  PERFORM READ_FIELD_NAME_FROM_DD_COSP.

* Read Amount DATA from COSP (Plan Version)
  PERFORM READ_AMT_DATA_FR_COSP.

* Calculating Fix-Var-Total Amount
  PERFORM CAL_FIX_VAR_TOT_AMT.

ENDFORM.                    " READ_AMT_DATA

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
*&      Form  READ_FIELD_NAME_FROM_DD_COSP
*&---------------------------------------------------------------------*
*       Building Field List for COSP
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_FIELD_NAME_FROM_DD_COSP.

  CLEAR : IT_ET_FIELDLIST, IT_ET_FIELDLIST[].

* read DD infor. COSP Key Part (FIX + KEY)
  PERFORM READ_DD_INFO  TABLES IT_ET_FIELDLIST
                        USING  'ZSCO_COSP_AMT01'.

* read DD infor. COSP Value Part (Total AMT)
  PERFORM READ_DD_INFO  TABLES IT_ET_FIELDLIST
                        USING  'ZSCO_COSP_AMT03'.

ENDFORM.                    " READ_FIELD_NAME_FROM_DD_COSP

*&---------------------------------------------------------------------*
*&      Form  READ_AMT_DATA_FR_COSP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_AMT_DATA_FR_COSP.
* Additional Selection Condition
*IT_OBJ_ALL : ALL Object Key / IT_COSTELEMENTLIST : selected CE
  CLEAR : IT_COSP, IT_COSP[].

* Set Interal Table to select data in the key combination
* - CCtrs + (ATs) + CEs
  DATA : BEGIN OF IT_L_OBJ_CE OCCURS 0,
            OBJNR LIKE COSP-OBJNR,
            KSTAR LIKE COSP-KSTAR,
         END OF   IT_L_OBJ_CE.
  LOOP AT IT_OBJ_ALL.
    LOOP AT IT_COSTELEMENTLIST.
      IT_L_OBJ_CE-OBJNR = IT_OBJ_ALL-OBJNR.
      IT_L_OBJ_CE-KSTAR = IT_COSTELEMENTLIST-COST_ELEM.
      APPEND IT_L_OBJ_CE.
      CLEAR  IT_L_OBJ_CE.
    ENDLOOP.
  ENDLOOP.

* seletion from COSP (PLAN Version)
  SORT  IT_L_OBJ_CE BY OBJNR KSTAR.
  CLEAR IT_L_OBJ_CE.

  CLEAR COSP.
  SELECT (IT_ET_FIELDLIST)
         INTO CORRESPONDING FIELDS OF IT_COSP
         FROM COSP
          FOR ALL ENTRIES IN IT_L_OBJ_CE
        WHERE
              OBJNR = IT_L_OBJ_CE-OBJNR
          AND LEDNR = '00'
          AND VERSN = P_FRVER " Plan Version
          AND WRTTP = '01'
          AND GJAHR = P_GJAHR
          AND KSTAR = IT_L_OBJ_CE-KSTAR.

    clear: it_cosp-BEKNZ, it_cosp-VRGNG.
    collect it_cosp.
  ENDSELECT.

  CLEAR IT_COSP.

* delete blank line.
  loop at it_cosp.
    if  it_cosp-WKF001 = 0
    and it_cosp-WKF002 = 0
    and it_cosp-WKF003 = 0
    and it_cosp-WKF004 = 0
    and it_cosp-WKF005 = 0
    and it_cosp-WKF006 = 0
    and it_cosp-WKF007 = 0
    and it_cosp-WKF008 = 0
    and it_cosp-WKF009 = 0
    and it_cosp-WKF010 = 0
    and it_cosp-WKF011 = 0
    and it_cosp-WKF012 = 0.

      delete it_cosp.
    endif.

  endloop.

ENDFORM.                    " READ_AMT_DATA_FR_COSP

*&---------------------------------------------------------------------*
*&      Form  CAL_FIX_VAR_TOT_AMT
*&---------------------------------------------------------------------*
*       Spliting Total Amount to Fixed Amt and Var Amt.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CAL_FIX_VAR_TOT_AMT.

* Extracting Variable Amount From Total Amount using Fixed Amount
* Total AMT - Fixed AMT = Var. AMT
  FIELD-SYMBOLS <FS_VAR_T>    TYPE ZSCO_COSP_AMT02. "Var.
  FIELD-SYMBOLS <FS_VAR_F>    TYPE ZSCO_COSP_AMT02. "Var.
  DATA : WA_L_ZSCO_COSP_TOT   LIKE ZSCO_COSP_AMT03. "Total
  DATA : WA_L_ZSCO_COSP_FIX   LIKE ZSCO_COSP_AMT04. "FIX

* Sort Object Info. ITAB
  SORT IT_OBJ_ALL BY OBJNR.

* In case of Mass Calculation, It is unnecessary to check period
  CLEAR IT_COSP.
  LOOP AT IT_COSP.
* Copying Total Values => Var.  Values
    CLEAR WA_L_ZSCO_COSP_TOT.
    MOVE-CORRESPONDING IT_COSP  TO WA_L_ZSCO_COSP_TOT.
    ASSIGN WA_L_ZSCO_COSP_TOT   TO <FS_VAR_T> CASTING.
* Copying FIxed Values => Var'. Values
    CLEAR WA_L_ZSCO_COSP_FIX.
    MOVE-CORRESPONDING IT_COSP  TO WA_L_ZSCO_COSP_FIX.
    ASSIGN WA_L_ZSCO_COSP_FIX   TO <FS_VAR_F> CASTING.
* Subtraction  Var - Var' -> Result : <FS_VAR_T>.
    SUBTRACT-CORRESPONDING <FS_VAR_F> FROM <FS_VAR_T>.
* Copying DATA to Internal table (Variable Value)
    MOVE-CORRESPONDING <FS_VAR_T> TO IT_COSP.
* Read Object Info.
    CLEAR IT_OBJ_ALL.
    READ TABLE IT_OBJ_ALL WITH KEY OBJNR = IT_COSP-OBJNR.
    IT_COSP-KOSTL = IT_OBJ_ALL-KOSTL.
    IT_COSP-LSTAR = IT_OBJ_ALL-LSTAR.
* Modify
    MODIFY IT_COSP.
    CLEAR IT_COSP.
  ENDLOOP.

  CLEAR IT_COSP.

ENDFORM.                    " CAL_FIX_VAR_TOT_AMT

*&---------------------------------------------------------------------*
*&      Form  PRE_TO_REPORT
*&---------------------------------------------------------------------*
*       Making an itab for reporting
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRE_TO_REPORT.

* Collecting DATA each Period for reporting
  PERFORM COLLECT_EACH_PER.

* Reading Rate and Calculating AMT (Changing Variable AMT)
* Only for reporting
  PERFORM READ_AND_CAL_CH_VAR.

ENDFORM.                    " PRE_TO_REPORT

*&---------------------------------------------------------------------*
*&      Form  COLLECT_EACH_PER
*&---------------------------------------------------------------------*
*       Collecting DATA each Period for Reporting
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM COLLECT_EACH_PER.

  CLEAR IT_COSP.
* Init. Report ITAB
  CLEAR : IT_REPORT, IT_REPORT[].
* Local Data definition to distribute amount as period
  FIELD-SYMBOLS: <FS1> TYPE ANY, <FS2> TYPE ANY.
  DATA : LV_COSP_WKF(30).
  DATA : LV_COSP_VAR(30).
  DATA : LV_CNT  LIKE  COSP-PERBL.

* Summation for Fixed Amount/Variable Amount
  LOOP AT IT_COSP.
* Period Counter : Set From-Period .
    CLEAR LV_CNT.
    LV_CNT = P_FRPER .

    DO GV_PERCOUNT TIMES.
* Period
      IT_REPORT-PERIOD  = LV_CNT.
* CCTr + AT
      IT_REPORT-KOSTL = IT_COSP-KOSTL.
      IT_REPORT-LSTAR = IT_COSP-LSTAR.
      IT_REPORT-OBJNR = IT_COSP-OBJNR.

* values (Fixed AMT)
      CLEAR LV_COSP_WKF.
      CONCATENATE 'IT_COSP-'  GV_FIELDGROUP_WKF  LV_CNT
             INTO LV_COSP_WKF.
      ASSIGN (LV_COSP_WKF) TO <FS1>.
      IT_REPORT-FIX_AMT = <FS1>.
* values (Variable AMT)
      CLEAR LV_COSP_VAR.
      CONCATENATE 'IT_COSP-'  GV_FIELDGROUP_VAR  LV_CNT
             INTO LV_COSP_VAR.
      ASSIGN (LV_COSP_VAR) TO <FS2>.
      IT_REPORT-VAR_AMT = <FS2>.
* Collect
      COLLECT  IT_REPORT.
      CLEAR    IT_REPORT.
* Period Counter
      LV_CNT = LV_CNT + 1.
    ENDDO.

    CLEAR IT_COSP.
  ENDLOOP.

  CLEAR IT_REPORT.

ENDFORM.                    " COLLECT_EACH_PER

*&---------------------------------------------------------------------*
*&      Form  READ_AND_CAL_CH_VAR
*&---------------------------------------------------------------------*
*       Reading Rate and Calculating AMT (Changing Variable AMT)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_AND_CAL_CH_VAR.

* All records in ratio table are unique
* for OBJECT KEY because a cost element can be assigned
* each Object Key

* There might be no matched data in it_rate because
* the Quantity data in Plan or Standard version would not exist
* or users could not input proper data

* The missing points can be checked on this report before posting

  SORT IT_RATE BY OBJNR.

  FIELD-SYMBOLS: <FS1> TYPE ANY.
  DATA : LV_COSP_RAT(30).

  LOOP AT IT_REPORT.
* read Rate Table
    CLEAR IT_RATE.
    READ TABLE IT_RATE  WITH KEY OBJNR = IT_REPORT-OBJNR
                        BINARY SEARCH.
* values (Rate)
    CLEAR LV_COSP_RAT.
    CONCATENATE 'IT_RATE-'  GV_FIELDGROUP_RAT  IT_REPORT-PERIOD
           INTO LV_COSP_RAT.
    ASSIGN (LV_COSP_RAT) TO <FS1>.
    IT_REPORT-RATE = <FS1>.
* Calculating : Rate * VAR_AMT = CH_VAR_AMT.
    IT_REPORT-CH_VAR_AMT = IT_REPORT-VAR_AMT * IT_REPORT-RATE.

    MODIFY IT_REPORT.
    CLEAR IT_REPORT.
  ENDLOOP.

  CLEAR IT_REPORT.

ENDFORM.                    " READ_AND_CAL_CH_VAR

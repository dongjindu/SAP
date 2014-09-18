************************************************************************
* Program Name      : ZACO80U_UPCP
* Author            : Hyung Jin Youn
* Creation Date     : 19/03/2004
* Specifications By : Dong-Hwan Kim
* Pattern           : Report 1-1
* Development Request No: UD1K908199
* Add documentation :
* Description       : To upload data using "KP06"
*                     Main purpose is to upload data of "Cost Plan"
*                     - also using an Excel File
* the BDC structures for BATCH INPUT processing
*
* Modifications Log
* Date   Developer   Request ID    Description
*
************************************************************************
REPORT ZACO80U_UPCP MESSAGE-ID ZMCO.


*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
*Type-Pools
TYPE-POOLS: KCDE.

* Tables
DATA : IT_ZSCO_KP06_EXCEL LIKE STANDARD TABLE OF ZSCO_KP06_EXCEL
                          WITH HEADER LINE .

DATA : BEGIN OF IT_POST  OCCURS 0.
DATA :  KEY(70).
        INCLUDE STRUCTURE  IT_ZSCO_KP06_EXCEL.
DATA : END OF   IT_POST.

DATA : IT_INTERN  TYPE KCDE_INTERN WITH HEADER LINE .
DATA : IT_DFTAB   LIKE STANDARD TABLE OF  DFIES
                  WITH HEADER LINE .

* For BAPI FM
DATA : IT_RETURN         LIKE STANDARD TABLE OF BAPIRET2
                         WITH HEADER LINE.
DATA : WA_HEADERINFO     LIKE BAPIPLNHDR.
DATA : BEGIN OF IT_HEADERINFO OCCURS 0.
DATA :  KEY(70).
        INCLUDE STRUCTURE BAPIPLNHDR.
DATA : END OF   IT_HEADERINFO.
DATA : IT_INDEXSTRUCTURE LIKE STANDARD TABLE OF BAPIACPSTRU
                         WITH HEADER LINE.
DATA : IT_COOBJECT       LIKE STANDARD TABLE OF BAPIPCPOBJ
                         WITH HEADER LINE.
DATA : IT_PERVALUE       LIKE STANDARD TABLE OF BAPIPCPVAL
                         WITH HEADER LINE.
DATA : IT_TOTVALUE       LIKE STANDARD TABLE OF BAPIPCPTOT
                         WITH HEADER LINE.
DATA : GV_DELTA.


*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_FNAME  LIKE RLGRAP-FILENAME OBLIGATORY,
            P_EROW   TYPE I OBLIGATORY.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BL2 WITH FRAME TITLE TEXT-002.
PARAMETERS : P_UPD   RADIOBUTTON GROUP RA01,
             P_DLT   RADIOBUTTON GROUP RA01.
SELECTION-SCREEN END OF BLOCK BL2.
PARAMETERS : P_TRUN.
SELECTION-SCREEN END OF BLOCK BL1.


*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON P_FNAME.
* Check File Existency
  PERFORM CHK_FILE_EXISTENCY.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FNAME.
* Browsing File Path
  PERFORM BROWSING_FILE_PATH.


*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Delta Posting or Update Posting
  PERFORM SET_POSTING_MODE.
* Get Field info.
  PERFORM READ_FIELD_LIST_DDIF.
* Upload File
  PERFORM UPLOAD_FILE.
* Making DATA tab
  PERFORM MAKE_DATA_TAB.
* Preparation for posting
  PERFORM PRE_FOR_POSTING.
* POST PLAN DATA using BAPI FM "KP06"
  PERFORM POST_PL_CCTR_AT_CE.


*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Log.
  PERFORM DISPLAY_LOG.


*----------------------------------------------------------------------*
* Sub-Rutines
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  BROWSING_FILE_PATH
*&---------------------------------------------------------------------*
*       Browsing File Paths
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BROWSING_FILE_PATH.
* Browsing
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
       EXPORTING
            MASK          = '*.xls'
            STATIC        = 'X'
       CHANGING
            FILE_NAME     = P_FNAME
       EXCEPTIONS
            MASK_TOO_LONG = 1
            OTHERS        = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " BROWSING_FILE_PATH

*&---------------------------------------------------------------------*
*&      Form  CHK_FILE_EXISTENCY
*&---------------------------------------------------------------------*
*       Check File Existency
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHK_FILE_EXISTENCY.
* Check File Existency
  DATA : LV_EXIST.
  CLEAR  LV_EXIST.
  CALL FUNCTION 'TMP_GUI_GET_FILE_EXIST'
    EXPORTING
      FNAME                = P_FNAME
    IMPORTING
      EXIST                = LV_EXIST
*     ISDIR                =
*     FILESIZE             =
    EXCEPTIONS
      FILEINFO_ERROR       = 1
      OTHERS               = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  IF LV_EXIST NE 'X'.
    MESSAGE E075 WITH P_FNAME.
  ENDIF.

ENDFORM.                    " CHK_FILE_EXISTENCY

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_FILE
*&---------------------------------------------------------------------*
*       Upload File data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD_FILE.

  CLEAR : IT_ZSCO_KP06_EXCEL, IT_ZSCO_KP06_EXCEL[].

  CLEAR : IT_INTERN, IT_INTERN[].

* Cal. The No. of End.COL.
  DATA : LV_END_COL LIKE SY-TFILL.
  DESCRIBE TABLE   IT_DFTAB LINES LV_END_COL.

  CALL FUNCTION 'KCD_EXCEL_OLE_TO_INT_CONVERT'
       EXPORTING
            FILENAME                = P_FNAME
            I_BEGIN_COL             = '1'
            I_BEGIN_ROW             = '1'
            I_END_COL               = LV_END_COL
            I_END_ROW               = P_EROW
       TABLES
            INTERN                  = IT_INTERN
       EXCEPTIONS
            INCONSISTENT_PARAMETERS = 1
            UPLOAD_OLE              = 2
            OTHERS                  = 3.

  IF SY-SUBRC <> 0.
    MESSAGE E000 WITH TEXT-010.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " UPLOAD_FILE

*&---------------------------------------------------------------------*
*&      Form  READ_FIELD_LIST_DDIF
*&---------------------------------------------------------------------*
*       Read Field info.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_FIELD_LIST_DDIF.

  CLEAR : IT_DFTAB, IT_DFTAB[].

  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      TABNAME              = 'ZSCO_KP06_EXCEL'
*     FIELDNAME            = ' '
*     LANGU                = SY-LANGU
*     LFIELDNAME           = ' '
*     ALL_TYPES            = ' '
*   IMPORTING
*     X030L_WA             =
*     DDOBJTYPE            =
*     DFIES_WA             =
*     LINES_DESCR          =
    TABLES
      DFIES_TAB            = IT_DFTAB
*     FIXED_VALUES         =
   EXCEPTIONS
     NOT_FOUND            = 1
     INTERNAL_ERROR       = 2
     OTHERS               = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " READ_FIELD_LIST_DDIF

*&---------------------------------------------------------------------*
*&      Form  MAKE_DATA_TAB
*&---------------------------------------------------------------------*
*       Making Data Tab
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_DATA_TAB.

  CLEAR : IT_ZSCO_KP06_EXCEL, IT_ZSCO_KP06_EXCEL[].

  FIELD-SYMBOLS : <FS> TYPE ANY.
  DATA : LV_FNAME(60).
  SORT IT_INTERN BY ROW COL.

  LOOP AT IT_INTERN.
* New line
    AT NEW ROW.
      CLEAR IT_ZSCO_KP06_EXCEL.
    ENDAT.

* Set Value
    CLEAR IT_DFTAB.
    READ TABLE IT_DFTAB WITH KEY POSITION = IT_INTERN-COL.
    IF SY-SUBRC = 0.
      CLEAR LV_FNAME.
      CONCATENATE 'IT_ZSCO_KP06_EXCEL'
                  '-'
                  IT_DFTAB-FIELDNAME
             INTO LV_FNAME.

      ASSIGN (LV_FNAME) TO <FS>.
      <FS> = IT_INTERN-VALUE.
    ENDIF.

* Append
    AT END OF ROW.
      APPEND IT_ZSCO_KP06_EXCEL.
      CLEAR  IT_ZSCO_KP06_EXCEL.
    ENDAT.

    CLEAR IT_INTERN.
  ENDLOOP.

  CLEAR  IT_ZSCO_KP06_EXCEL.

ENDFORM.                    " MAKE_DATA_TAB

*&---------------------------------------------------------------------*
*&      Form  PRE_FOR_POSTING
*&---------------------------------------------------------------------*
*       Preparation for posting
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRE_FOR_POSTING.

  CLEAR  IT_ZSCO_KP06_EXCEL.

* Collect Data
  CLEAR : IT_POST, IT_POST[].
  LOOP AT IT_ZSCO_KP06_EXCEL.
    MOVE-CORRESPONDING IT_ZSCO_KP06_EXCEL TO IT_POST.
    CONCATENATE IT_POST-CO_AREA
                IT_POST-FISC_YEAR
                IT_POST-PERIOD
                IT_POST-VERSION
                IT_POST-DOC_HDR_TX
                IT_POST-PLAN_CURRTYPE
           INTO IT_POST-KEY.
    COLLECT IT_POST.
    CLEAR   IT_POST.
  ENDLOOP.

* Sorting
  CLEAR   IT_POST.
  SORT IT_POST BY  KEY.

ENDFORM.                    " PRE_FOR_POSTING

*&---------------------------------------------------------------------*
*&      Form  POST_PL_CCTR_AT_CE
*&---------------------------------------------------------------------*
*       POST PLAN DATA using BAPI FM
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM POST_PL_CCTR_AT_CE.

  SORT IT_POST BY KEY
                  COSTCENTER
                  ACTTYPE
                  COST_ELEM.

  CLEAR : IT_HEADERINFO, IT_HEADERINFO[].

** Header Part / Init Data Containers
  LOOP AT IT_POST.
    ON CHANGE OF IT_POST-KEY.
*     Fill Header DATA
      PERFORM FILL_HEADER_DATA.
    ENDON.
    CLEAR IT_POST.
  ENDLOOP.

** Fill Object List and Plan Values per Period
  LOOP AT IT_HEADERINFO.
*   Clear Str.
    PERFORM CLEAR_BAPI_STR.
*   Set Header Info.
    MOVE-CORRESPONDING IT_HEADERINFO TO WA_HEADERINFO.
*   Set Value/Object
    LOOP AT IT_POST WHERE KEY = IT_HEADERINFO-KEY.
*   Obj
      ON CHANGE OF  IT_POST-KEY
                OR  IT_POST-COSTCENTER
                OR  IT_POST-ACTTYPE .
*     Index of Object Key
        IT_INDEXSTRUCTURE-OBJECT_INDEX
             = IT_INDEXSTRUCTURE-OBJECT_INDEX + 1 .

        CLEAR IT_COOBJECT.
        IT_COOBJECT-OBJECT_INDEX = IT_INDEXSTRUCTURE-OBJECT_INDEX.

        IT_COOBJECT-COSTCENTER   = IT_POST-COSTCENTER.
        IT_COOBJECT-ACTTYPE      = IT_POST-ACTTYPE.
        APPEND IT_COOBJECT.
        CLEAR  IT_COOBJECT.
      ENDON.

*   Value.
*     Index of Value
      IT_INDEXSTRUCTURE-VALUE_INDEX
           = IT_INDEXSTRUCTURE-VALUE_INDEX + 1.

      CLEAR IT_PERVALUE.
      IT_PERVALUE-VALUE_INDEX = IT_INDEXSTRUCTURE-VALUE_INDEX.
      IT_PERVALUE-COST_ELEM   = IT_POST-COST_ELEM.
*     Set Value( Fixed  AMT. )
      PERFORM SET_FIXED_VAL.
*     Append PerValues
      APPEND IT_PERVALUE.
      CLEAR  IT_PERVALUE.

*     Append Index
      APPEND IT_INDEXSTRUCTURE.
      CLEAR IT_POST.
    ENDLOOP.

** Call BAPI FM
    PERFORM CALL_POST_FM.

    CLEAR IT_HEADERINFO.
  ENDLOOP.

* Commit
  IF P_TRUN = 'X'.
    MESSAGE S009(ZMCO) WITH '- TEST RUN'.
  ELSE.
*// Important Warning
* During Cycle test 3 basis staffs adjusted the attribute for
* update process in R/3 Server.
* Some crucial Basis change can affect Standard Update Logic
* "Wait option" should be set for the Basis changes
* Take care this part when you see system update errors.
    WAIT UP TO 5 SECONDS .
    COMMIT WORK AND WAIT.
    MESSAGE S009(ZMCO) WITH '- POST'.
  ENDIF.

ENDFORM.                    " POST_PL_CCTR_AT_CE

*&---------------------------------------------------------------------*
*&      Form  CLEAR_BAPI_STR
*&---------------------------------------------------------------------*
*       Clear Str. (For BAPI)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEAR_BAPI_STR.
  CLEAR : IT_INDEXSTRUCTURE, IT_INDEXSTRUCTURE[].
  CLEAR : IT_COOBJECT,       IT_COOBJECT[].
  CLEAR : IT_PERVALUE,       IT_PERVALUE[].
  CLEAR : IT_TOTVALUE,       IT_TOTVALUE[].
  CLEAR WA_HEADERINFO.
ENDFORM.                    " CLEAR_BAPI_STR

*&---------------------------------------------------------------------*
*&      Form  FILL_HEADER_DATA
*&---------------------------------------------------------------------*
*       Fill Header DATA
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_HEADER_DATA.
  CLEAR IT_HEADERINFO.
  MOVE-CORRESPONDING IT_POST TO IT_HEADERINFO.
  IT_HEADERINFO-PERIOD_FROM  = IT_POST-PERIOD.
  IT_HEADERINFO-PERIOD_TO    = IT_POST-PERIOD.
  APPEND IT_HEADERINFO.
  CLEAR  IT_HEADERINFO.
ENDFORM.                    " FILL_HEADER_DATA

*&---------------------------------------------------------------------*
*&      Form  SET_FIXED_VAL
*&---------------------------------------------------------------------*
*       Set Value( Fixed  AMT. )
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_FIXED_VAL.
*  IT_PERVALUE-FIX_VAL_PER&1  =  IT_COSP-WKF0&1.
*  IT_HEADERINFO-PERIOD_FROM  = IT_POST-PERIOD.
*  IT_HEADERINFO-PERIOD_TO    = IT_POST-PERIOD.
  FIELD-SYMBOLS: <FS1> TYPE ANY.
  DATA : LV_FIX(40).
  DATA : LV_CNT(2)  TYPE N.

* Period Counter : Set From-Period .
  CLEAR LV_CNT.
  LV_CNT = IT_POST-PERIOD.

  CLEAR LV_FIX.
  CONCATENATE 'IT_PERVALUE-FIX_VAL_PER' LV_CNT
         INTO LV_FIX.
  ASSIGN (LV_FIX) TO <FS1>.
  <FS1> = IT_POST-FIX_VAL_PERXX.

ENDFORM.                    " SET_FIXED_VAL

*&---------------------------------------------------------------------*
*&      Form  CALL_POST_FM
*&---------------------------------------------------------------------*
*       POSTING
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_POST_FM.

  CLEAR : IT_RETURN, IT_RETURN[].


  CALL FUNCTION 'BAPI_COSTACTPLN_POSTPRIMCOST'
    EXPORTING
      HEADERINFO           = WA_HEADERINFO
      DELTA                = GV_DELTA
    TABLES
      INDEXSTRUCTURE       = IT_INDEXSTRUCTURE
      COOBJECT             = IT_COOBJECT
      PERVALUE             = IT_PERVALUE
*     TOTVALUE             =
*     CONTRL               =
      RETURN               = IT_RETURN.
*
* Check error
  CLEAR  IT_RETURN.
  LOOP AT IT_RETURN  WHERE TYPE CA 'AE'.
    MESSAGE ID     IT_RETURN-ID
            TYPE   IT_RETURN-TYPE
            NUMBER IT_RETURN-NUMBER
            WITH   IT_RETURN-MESSAGE_V1
                   IT_RETURN-MESSAGE_V2
                   IT_RETURN-MESSAGE_V3
                   IT_RETURN-MESSAGE_V4.
    CLEAR IT_RETURN.
  ENDLOOP.

ENDFORM.                    " CALL_POST_FM

*&---------------------------------------------------------------------*
*&      Form  SET_POSTING_MODE
*&---------------------------------------------------------------------*
*       Delta Posting or Update Posting
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_POSTING_MODE.

  CASE 'X'.
    WHEN P_UPD. CLEAR GV_DELTA.
    WHEN P_DLT. GV_DELTA = 'X'.
  ENDCASE.

ENDFORM.                    " SET_POSTING_MODE

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LOG
*&---------------------------------------------------------------------*
*       Log.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_LOG.

  DATA : LV_EXECNT LIKE SY-TFILL.
  DATA : LV_POSCNT LIKE SY-TFILL.

  DESCRIBE TABLE IT_ZSCO_KP06_EXCEL LINES LV_EXECNT.
  DESCRIBE TABLE IT_POST            LINES LV_POSCNT.

  WRITE : / TEXT-011 , LV_EXECNT.
  SKIP 1.
  WRITE : / TEXT-012 , LV_POSCNT.
  SKIP 1.
  WRITE : / TEXT-013.

ENDFORM.                    " DISPLAY_LOG

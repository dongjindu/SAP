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

* For TOP include
INCLUDE ZACO05U_NEW_1TOP.
* For Sub-routine
INCLUDE ZACO05L_NEW_F001.
* For Sub-routine - Posting
INCLUDE ZACO05L_NEW_F002.


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

  CASE P_REVS.
* posting
    WHEN SPACE.
      PERFORM POST_PROCESS.
* Reversing
    WHEN 'X'.
      PERFORM REVERSE_PROCESS.
  ENDCASE.


*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Print Result
  PERFORM RESULT_LIST.
*
*
*&---------------------------------------------------------------------*
*&      Form  POST_DI_ACT_PPCVAR_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM POST_DI_ACT_PPCVAR_NEW.

  DATA : IT_BAPIDOCHDRP LIKE BAPIDOCHDRP OCCURS 0 WITH HEADER LINE,
         IT_BAPIAAITM LIKE BAPIAAITM OCCURS 0 WITH HEADER LINE,
         DOC_NO LIKE BAPIDOCHDRP-DOC_NO,
         LV_CONF_TEXT(50).

  CLEAR IT_PPC_ACT_MOD.

  SORT : IT_DI_POST BY FLG_REVERSAL GJAHR PERID MATNR WERKS AUFNR,
         IT_PPC_ACT_MOD BY COST_CENTER ACTTYPE.
* Check invalid resourse
  LOOP AT IT_DI_POST.
    CLEAR IT_PPC_ACT_MOD .
    READ TABLE  IT_PPC_ACT_MOD WITH KEY COST_CENTER = IT_DI_POST-KOSTL
                                        ACTTYPE     = IT_DI_POST-LSTAR
                                        BINARY SEARCH.
    IF SY-SUBRC <> 0.
      IT_DI_POST-WRONG_PPC = 'X'. "Wrong Master
      MODIFY IT_DI_POST.
      CLEAR IT_DI_POST.
    ENDIF.
  ENDLOOP.

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

* TEXT
  CLEAR LV_CONF_TEXT.
  CONCATENATE SY-UNAME  SY-DATUM  SY-REPID
         INTO LV_CONF_TEXT
         SEPARATED BY '/'.

  IT_BAPIDOCHDRP-CO_AREA = P_KOKRS.
  IT_BAPIDOCHDRP-DOCDATE = LV_DATE.
  IT_BAPIDOCHDRP-POSTGDATE = LV_DATE.
  IT_BAPIDOCHDRP-VERSION = P_VERSN.
  IT_BAPIDOCHDRP-DOC_HDR_TX = LV_CONF_TEXT.
  IT_BAPIDOCHDRP-USERNAME = SY-UNAME.
  APPEND IT_BAPIDOCHDRP.CLEAR IT_BAPIDOCHDRP.

  LOOP AT IT_DI_POST WHERE WRONG_PPC NE 'X'.
    IT_BAPIAAITM-SENBUSPROC = IT_DI_POST-KOSTL.
    IT_BAPIAAITM-REC_ORDER = IT_DI_POST-AUFNR.
    IT_BAPIAAITM-ACTTYPE = IT_DI_POST-LSTAR.
    IT_BAPIAAITM-ACTVTY_QTY = IT_DI_POST-MEGXXX.
    IT_BAPIAAITM-ACTIVITYUN = IT_DI_POST-MEINH.
    APPEND IT_BAPIAAITM.CLEAR IT_BAPIAAITM.
  ENDLOOP.

  CLEAR : IT_RETURN, IT_RETURN[].

  CALL FUNCTION 'BAPI_ACC_ACTIVITY_ALLOC_POST'
       EXPORTING
            DOC_HEADER      = IT_BAPIDOCHDRP
            IGNORE_WARNINGS = 'X'
       IMPORTING
            DOC_NO          = DOC_NO
       TABLES
            DOC_ITEMS       = IT_BAPIAAITM
            RETURN          = IT_RETURN.
*     CRITERIA              =        .

  LOOP AT  IT_DI_POST WHERE WRONG_PPC NE 'X'.

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


ENDFORM.                    " POST_DI_ACT_PPCVAR_NEW

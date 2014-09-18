*----------------------------------------------------------------------
* Program ID        : ZACOU102
* Title             : [CO] Review Costing Result
* Created on        : 08/10/2006
* Created by        : Michelle Jeong
* Specifications By : Andy Choi
* Description       : Error management program for review costing
*                     result and change information of cost
* @ if choose MI costing type,
*   clear duty, freight, others
*   when get data from CKIS table(Items Unit Costing)
*   or ZTCOU102(Costing Result).
*----------------------------------------------------------------------
* need Maintain user profile (T-Code: SU3)
*   Parameter ID: ZCOUC
*   Values      : COADM, COUSR, PUADM, PUUSR
*    . PUADM : PU admin. has change pur.grp, info reason, source.
*    . COADM : CO admin. has authorization for recosting.
*    . COUSR : CO user. has no authorization for change. display only.
*    . PUUSR : PU user. has no authorization for change. display only.
* Date        Developer  Request    Description
* 03/26/2007  Manju      UD1K940162 Program corrections
* 06/11/2007  IG.MOON    UD1K940727 Program corrections
* 04/29/2010  Valerian              Refer vendor to table ZTCOU137
*             HIS20094              instead of table CKIS
* 10/11/2010  Valerian   UD1K949919 Add 'Material Level II', 'Material
*                                   Level I', Description and Category
*                                   to Rep.Layout
* 05/24/2010  Valerian   UD1K951804 Add user authorization check based
*                                   on costing type.
* 12/02/2011  KDM        UD1K953348 Binary search error change.
* 03/28/2012  Valerian   UD1K954378 Clear duty for Korea Country
* 05/17/2012  Valerian   UD1K954962 Override duty ratio if mat. found
*                                   in ZFTA_DUTY table and Costing
*                                   type EQ 'U1'
* 10/16/2012  Valerian   UD1K955659 Enable duty calculation for
*                                   costing type "BP"
* 07/03/2013  T00303     UD1K957567 U1: Apply Archiving
*----------------------------------------------------------------------

* TODO
*  1. if price changed, check product is locked in 100.
*  2. ...

REPORT ZACOU102 NO STANDARD PAGE HEADING MESSAGE-ID ZMCO.

INCLUDE ZACOUI00.
INCLUDE ZACOU102_TOP.

*----------------------------------------------------------------------*
* Select-Options & Parameters
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B0 WITH FRAME TITLE TEXT-000.
* Block: Costing types
SELECT-OPTIONS S_KALKA FOR KEKO-KALKA NO INTERVALS
               OBLIGATORY MEMORY ID KKA. "FAULT 'R1'.
PARAMETERS: P_KOKRS LIKE KEKO-KOKRS OBLIGATORY MEMORY ID CAC,
            P_YEAR  LIKE KEKO-BDATJ OBLIGATORY MEMORY ID BDTJ,
            P_POPER LIKE KEKO-POPER OBLIGATORY MEMORY ID POPR.
*            P_werks LIKE KEKO-WERKS OBLIGATORY MEMORY ID WERKS Default
*'P001'.
PARAMETER : P_VER LIKE ZTCOU102-VER   MODIF ID A,  " BP Version
            P_DUTY LIKE GT_A902-KBETR DEFAULT '2.5'.
SELECTION-SCREEN END OF BLOCK B0.
SELECTION-SCREEN BEGIN OF BLOCK M1 WITH FRAME TITLE TEXT-014.

*UD1K940162
SELECT-OPTIONS: S_MATNR  FOR CKIS-MATNR,              " Material
                S_LIFNR  FOR CKIS-LIFNR,              " Vendor
                S_KSTAR  FOR CKIS-KSTAR,              " C.Elemn.
                S_EKGRP  FOR MARC-EKGRP,              " Pur.Grp
                S_DISPO  FOR MARC-DISPO,              " MRP controller
                S_MATKL  FOR MARA-MATKL.              " Material Group

SELECT-OPTIONS: S_ARTNR  FOR KEKO-MATNR.              " Product
SELECTION-SCREEN END OF BLOCK M1.

* Block: Items
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-003.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETER P_ALL RADIOBUTTON GROUP R1 DEFAULT 'X' USER-COMMAND UCOM."All
SELECTION-SCREEN COMMENT 3(12) TEXT-P02 FOR FIELD P_ALL.

PARAMETER P_ERR  RADIOBUTTON GROUP R1.    " Error
SELECTION-SCREEN COMMENT 22(12) TEXT-P01 FOR FIELD P_ERR.

PARAMETER P_CHG  RADIOBUTTON GROUP R1.                  " Changed
SELECTION-SCREEN COMMENT 41(7) TEXT-P03 FOR FIELD P_CHG.

SELECTION-SCREEN END OF LINE.

*SELECTION-SCREEN: PUSHBUTTON /1(30) pushb_o1         "Open Block 01
*                    USER-COMMAND ucomm_o1 MODIF ID mo1,     "#EC NEEDED
*                  PUSHBUTTON /1(30) pushb_c1         "Close Block 01
*                    USER-COMMAND ucomm_c1 MODIF ID mc1.     "#EC NEEDED
*SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME.

*SELECTION-SCREEN BEGIN OF LINE.
*PARAMETER P_OTH2 RADIOBUTTON GROUP R2 USER-COMMAND HIDE.
*" Show Option
*SELECTION-SCREEN COMMENT 05(15) TEXT-P05 FOR FIELD P_OTH2.
*PARAMETER P_OTH  RADIOBUTTON GROUP R2.                  " Hide
*SELECTION-SCREEN COMMENT 31(15) TEXT-P04 FOR FIELD P_OTH.
*SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B1.


* Block: Execptions
SELECTION-SCREEN BEGIN OF BLOCK EXCEPT WITH FRAME TITLE TEXT-004.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) TEXT-P14 FOR FIELD P_CP MODIF ID RP.
PARAMETERS: P_CP TYPE KKB_CAL2_LIGHTS_N MODIF ID RP.
SELECTION-SCREEN COMMENT 38(50) T_LIGHTS FOR FIELD P_CP MODIF ID RP.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) TEXT-P11 FOR FIELD P_RED MODIF ID RP.
PARAMETER P_RED TYPE KKB_CAL2_LIGHTS_X MODIF ID RP.
SELECTION-SCREEN COMMENT 52(3) TEXT-P12 FOR FIELD P_REDP MODIF ID RP.
PARAMETER P_REDP TYPE KKB_CAL2_LIGHTSPX MODIF ID RP.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) TEXT-P13 FOR FIELD P_YELL MODIF ID RP.
PARAMETER P_YELL TYPE KKB_CAL2_LIGHTS_X MODIF ID RP.
SELECTION-SCREEN COMMENT 52(3) TEXT-P12 FOR FIELD P_YELLP MODIF ID RP.
PARAMETER P_YELLP TYPE KKB_CAL2_LIGHTSPX MODIF ID RP.
SELECTION-SCREEN END OF LINE.

PARAMETERS: P_ABS  TYPE KKB_CAL2_LGHT_ABS DEFAULT 'X' MODIF ID RP,
            P_EXPT TYPE KKB_CAL2_EXCEPT DEFAULT 'X' MODIF ID RP.
SELECTION-SCREEN END OF BLOCK EXCEPT.

PARAMETER P_LDC DEFAULT 'X' NO-DISPLAY .
"  AS CHECKBOX  MODIF ID RP. " Use ABP LDC Rate%

* Refresh
PARAMETERS: P_REF1 AS CHECKBOX MODIF ID RP.  "Refresh from Cost.etsimate
*            P_REF2 DEFAULT 'X' NO-DISPLAY .
" AS CHECKBOX MODIF ID RP.  "Refresh from Price Info.

*Batch update
PARAMETER P_BATCH AS CHECKBOX MODIF ID RP.
*Lock(display)/Change
PARAMETER P_MODE  TYPE CHAR01 NO-DISPLAY MODIF ID RP.

*ARAMETER P_LOGIC DEFAULT 'X' NO-DISPLAY. " MODIF ID RP.
*- U1 Start
INCLUDE ZIARCH_COMM01.
*- U1 End

INCLUDE ZACOU102_ALV.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN .
* BEGIN OF UD1K951804
  DATA: T_ZTCO_UNITUSER LIKE ZTCO_UNITUSER OCCURS 0 WITH HEADER LINE.

  CHECK NOT P_REF1 IS INITIAL.

  SELECT * INTO TABLE T_ZTCO_UNITUSER
    FROM ZTCO_UNITUSER
   WHERE KALKA IN S_KALKA
     AND BNAME = SY-UNAME.

  LOOP AT S_KALKA.
    READ TABLE T_ZTCO_UNITUSER WITH KEY KALKA = S_KALKA-LOW.
    IF SY-SUBRC <> 0.
      MESSAGE E000 WITH 'No authorization to update costing type:'
                        S_KALKA-LOW.
    ENDIF.
  ENDLOOP.

* Refresh select-option to make sure user use valid authorization.
  REFRESH S_KALKA. CLEAR S_KALKA.
  S_KALKA-SIGN = 'I'.
  S_KALKA-OPTION = 'EQ'.

  LOOP AT T_ZTCO_UNITUSER.
    S_KALKA-LOW = T_ZTCO_UNITUSER-KALKA.
    APPEND S_KALKA.
  ENDLOOP.
* END OF UD1K951804

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_CP.
  PERFORM POPUP_FIELDNR.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_KALKA-LOW.
  PERFORM POPUP_KALKA USING S_KALKA-LOW 'S_KALKA-LOW'.

AT SELECTION-SCREEN OUTPUT.
*  IF P_OTH EQ 'X'.
*    LOOP AT SCREEN.
*      IF SCREEN-GROUP1 = 'RP'.
*        SCREEN-INPUT = '1'.
*        SCREEN-INVISIBLE = '0'.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*  ELSE.
*    LOOP AT SCREEN.
*      IF SCREEN-GROUP1 = 'RP' OR
*         SCREEN-GROUP1 = 'A'.
*        SCREEN-INPUT = '0'.
*        SCREEN-INVISIBLE = '1'.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.

*AT SELECTION-SCREEN ON RADIOBUTTON GROUP R2.
*  IF P_OTH EQ 'X'.
*    LOOP AT SCREEN.
*      IF SCREEN-GROUP1 = 'RP' OR
*         SCREEN-GROUP1 = 'A'.
*        SCREEN-INPUT = '1'.
*        SCREEN-INVISIBLE = '0'.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*  ELSE.
*    LOOP AT SCREEN.
*      IF SCREEN-GROUP1 = 'RP'.
*        SCREEN-INPUT = '0'.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.
*

*----------------------------------------------------------------------*
*  Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
* Check authorization
*   PUADM : PU admin. has change pur.grp, info reason, source.
*   COADM : CO admin. has authorization for recosting.
*   COUSR : CO user. has no authorization for change. display only.
*   PUUSR : PU user. has no authorization for change. display only.
  GV_LEVEL = 'PUUSR'.
  GET PARAMETER ID 'ZCOUC' FIELD GV_LEVEL.

* Check Lock
*   Parameter ID: ZLCK : when other user using this program, check 'X'.
*                 ZUSER : user id who has using this program
  CLEAR: GV_LOCK, GV_USER.
  GET PARAMETER ID 'ZLCK' FIELD GV_LOCK.
  GET PARAMETER ID 'ZUSER' FIELD GV_USER.

*  CLEAR: S_DISPO.
*  S_DISPO-SIGN = 'E'.
*  S_DISPO-OPTION = 'EQ'.
*  S_DISPO-LOW = 'M02'.
*  APPEND S_DISPO.

* Set Text & Icon for Pushbutton c1 - c5, o1 - o5
*  CONCATENATE icon_collapse: 'More Details' INTO pushb_c1.
*  CONCATENATE icon_expand:   'More Details' INTO pushb_o1.

*----------------------------------------------------------------------*
* Start of selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  CLEAR G_ERROR.
  IF GV_LOCK = 'X'.
    MESSAGE S000 WITH GV_USER 'using this program.'.
  ENDIF.

  IF NOT P_REDP IS INITIAL AND NOT P_YELLP IS INITIAL AND
         P_REDP < P_YELLP OR
     NOT P_RED IS INITIAL AND NOT P_YELL IS INITIAL AND
         NOT P_RED IS INITIAL AND NOT P_YELL IS INITIAL AND
         P_RED < P_YELL.
    MESSAGE S330(KN). EXIT.
  ENDIF.

  IF NOT P_ABS IS INITIAL AND
     ( P_REDP < 0 OR P_YELLP < 0 OR P_RED < 0 OR P_YELL < 0 ).
    MESSAGE S462(KN). EXIT.
  ENDIF.

  PERFORM CHECK_DATA_LOCK.
  PERFORM GET_DATA.

*----------------------------------------------------------------------*
* End of selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
  CHECK G_ERROR NE 'X'.

  PERFORM GET_SET_STATUS.

*  DESCRIBE TABLE GT_CKIS LINES TOT_LINES.
*  WRITE : / 'total lines :', TOT_LINES.

  IF P_BATCH = 'X' AND P_REF1 = 'X'.
    PERFORM SAVE_ZTCOU102_BATCH.
  ELSE.
    CALL SCREEN 50.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0050  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0050 OUTPUT.
  SET PF-STATUS '50'.
  SET TITLEBAR '50'.
  PERFORM CREATE_ALV_CONTROL1.

*  Set parameter for checking Lock
*   Parameter ID: ZLCK  : 'X'
*                 ZUSER : current user id
  CLEAR: GV_LCHK, GV_CUSER.
  GV_LCHK  = 'X'.
  GV_CUSER = SY-UNAME.

  SET PARAMETER ID 'ZLCK' FIELD GV_LCHK.
  SET PARAMETER ID 'ZUSER' FIELD GV_CUSER.

ENDMODULE.                 " STATUS_0050  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0050  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0050 INPUT.
  DATA: LV_CNT(5),
        L_CHK,
        L_ANSWER(1).

  OK_CODE = SY-UCOMM.
  CLEAR: L_CHK, SY-UCOMM.

* Check authorization
*   PUADM : PU admin. has change pur.grp, info reason, source.
*   COADM : CO admin. has authorization for recosting.
*   COUSR : CO user. has no authorization for change. display only.
*   PUUSR : PU user. has no authorization for change. display only.

  IF OK_CODE = 'BACK' OR OK_CODE = 'CANC' OR OK_CODE = 'EXIT'.
*   Initialization parameter id for lock
    CLEAR: GV_LOCK, GV_USER.
    SET PARAMETER ID 'ZLCK'  FIELD GV_LOCK.
    SET PARAMETER ID 'ZUSER' FIELD GV_USER.

    LEAVE TO SCREEN 0.

  ELSE.
    IF GV_LOCK = 'X'.
      MESSAGE S000 WITH GV_USER 'using this program.'.
      EXIT.
    ENDIF.

    CASE OK_CODE.
*   Save
      WHEN 'SAVE'.

        IF GV_LEVEL NA 'CO'.
          MESSAGE S000 WITH 'You have no authorization.'.
          EXIT.
        ENDIF.

        IF P_MODE = 'L'.  "locked
          MESSAGE S000 WITH
         'All records have been locked. Check costing status ZCOA101.'.
          EXIT.
        ELSE.
          PERFORM SAVE_ZTCOU102 CHANGING LV_CNT.
          IF LV_CNT > 0.
            MESSAGE S000 WITH 'You have saved ; ' LV_CNT 'records.'.
            GV_SAVE = 'X'.
            __SET_REFRESH_MODE 'X'.

            CALL METHOD G_GRID1->REFRESH_TABLE_DISPLAY
              EXPORTING
                IS_STABLE = STABLE.

            CALL METHOD CL_GUI_CFW=>FLUSH.

          ENDIF.
        ENDIF.

*   Rollup
      WHEN 'RUP'.
        IF GV_LEVEL NA 'CO'.
          MESSAGE S000 WITH 'You have no authorization.'.
          EXIT.
        ENDIF.

        READ TABLE S_KALKA WITH KEY LOW = 'R1'.
        IF SY-SUBRC <> 0.
* <<< ig.Moon 8/6/07>>>
          __POPUP ''
          'Do you really want to roll up the selected item(s)?' ''.

          PERFORM ROLL_UP.
        ELSE.
          MESSAGE S000 WITH 'You can not rollup for standard costing.'.
*          EXIT.
        ENDIF.


*   Call Info History Report
      WHEN 'INFO' OR 'INFO2'.
        PERFORM CALL_ZACOU112.

*   Copy from STD
      WHEN 'STD'.
        IF GV_LEVEL = 'COUSR' OR GV_LEVEL = 'PUUSR'.
          MESSAGE S000 WITH 'You have no authorization.'.
          EXIT.
        ENDIF.

* <<< ig.Moon 8/6/07>>>
        __POPUP ''
        'Do you want to copy the price of standard?' ''.
        PERFORM COPY_PRICE USING 'GT_OUT-STPRS'.

*   Copy from MAP
      WHEN 'MAP'.
        IF GV_LEVEL = 'COUSR' OR GV_LEVEL = 'PUUSR'.
          MESSAGE S000 WITH 'You have no authorization.'.
          EXIT.
        ENDIF.

* <<< ig.Moon 8/6/07>>>
        __POPUP '' 'Do you want to copy the price of MAP?' ''.
        PERFORM COPY_PRICE USING 'GT_OUT-VERPR'.

*   Mass Changing
      WHEN 'MCH'.
        IF GV_LEVEL = 'COUSR' OR GV_LEVEL = 'PUUSR'.
          MESSAGE S000 WITH 'You have no authorization.'.
          EXIT.
        ENDIF.

        IF GV_LOCK = 'X'.
          MESSAGE S000 WITH GV_USER 'using this program.'.
          EXIT.
        ENDIF.

        PERFORM MASS_CHANGE.

*   Transfer to Plan price (OLD LOGIC..NO LONGER VALID--ANDY)
      WHEN 'PLAN'.
        IF GV_LEVEL = 'COUSR' OR GV_LEVEL = 'PUUSR'.
          MESSAGE S000 WITH 'You have no authorization.'.
          EXIT.
        ENDIF.

        IF GV_LOCK = 'X'.
          MESSAGE S000 WITH GV_USER 'using this program.'.
          EXIT.
        ENDIF.

        READ TABLE S_KALKA WITH KEY LOW = 'BP'.
        IF SY-SUBRC = 0.
          L_CHK = 'X'.
        ELSE.
          READ TABLE S_KALKA WITH KEY LOW = 'R1'.
          IF SY-SUBRC = 0.
            L_CHK = 'X'.
          ENDIF.
        ENDIF.

        IF L_CHK = 'X'.  " ABP, STD only
* <<< ig.Moon 8/6/07>>>
          __POPUP '' 'Do you want to transfer to plan price?' ''.

          PERFORM TRANSFER_PLAN_PRICE.
*          PERFORM REFRESH_FIELD1.
        ELSE.
          MESSAGE S000 WITH 'Check costing type!'.
          EXIT.
        ENDIF.

*   Copy Version
      WHEN 'VER'.
        IF GV_LEVEL = 'COUSR' OR GV_LEVEL = 'PUUSR'.
          MESSAGE S000 WITH 'You have no authorization.'.
          EXIT.
        ENDIF.

        READ TABLE S_KALKA WITH KEY LOW = 'BP'.
        IF SY-SUBRC = 0.
* <<< ig.Moon 8/6/07>>>
          __POPUP '' 'Do you really want to copy version?' ''.

          PERFORM COPY_VER.
        ELSE.
          MESSAGE S000 WITH 'Copy version available ABP only!'.
          EXIT.
        ENDIF.

*   Download
      WHEN 'DOWN'.
        PERFORM DOWNLOAD.

*   Upload
      WHEN 'UPLOAD'.
        IF GV_LEVEL = 'COUSR' OR GV_LEVEL = 'PUUSR'.
          MESSAGE S000 WITH 'You have no authorization.'.
          EXIT.
        ENDIF.

        IF GV_SAVE IS INITIAL.
          MESSAGE W000 WITH 'Pleas save first.'.
          EXIT.
        ELSE.
          PERFORM UPLOAD.
        ENDIF.

*   Recosting
      WHEN 'RCOST'.
*     CO admin: COADM : recosting
        IF GV_LEVEL = 'COADM'.
* <<< ig.Moon 8/6/07>>>
          __POPUP ''
          'Do you really want to recost the selected item(s)?' ''.
          PERFORM RECOSTING.
        ELSE.
          MESSAGE S000 WITH 'You have no authorization.'.
          EXIT.
        ENDIF.

*   Apply LDC
      WHEN 'LDC'.
        IF GV_LEVEL = 'COUSR' OR GV_LEVEL = 'PUUSR'.
          MESSAGE S000 WITH 'You have no authorization.'.
          EXIT.
        ENDIF.

        PERFORM APPLY_LDC.
*        IF SY-SUBRC = 0.
        PERFORM REFRESH_SCREEN USING 'X'.
*          perform refresh_field1.
*        ENDIF.

*   Change Pur.Group
      WHEN 'PURG'.

        IF GV_LEVEL = 'PUADM'.
* <<< ig.Moon 8/6/07>>>
          __POPUP '' 'Do you want to change purchasing group?' ''.

          PERFORM CHANGE_PURGR.
        ELSE.
          MESSAGE S000 WITH 'You have no authorization.'.
          EXIT.
        ENDIF.

*   Change Source
      WHEN 'SRC'.
        IF GV_LEVEL = 'PUADM'.
* <<< ig.Moon 8/6/07>>>
          __POPUP '' 'Do you want to change source?' ''.
          PERFORM CHANGE_SRC.
        ELSE.
          MESSAGE S000 WITH 'You have no authorization.'.
          EXIT.
        ENDIF.

*   Change Reason
      WHEN 'RSN'.
        IF GV_LEVEL = 'PUADM'.
* <<< ig.Moon 8/6/07>>>
          __POPUP '' 'Do you want to change reason?' ''.
          PERFORM CHANGE_REASON.
        ELSE.
          MESSAGE S000 WITH 'You have no authorization.'.
          EXIT.
        ENDIF.

      WHEN 'REFRESH'.
        PERFORM REFRESH_FIELD1.
* UD1K941202 - by IG.MOON 8/2/2007 {
      WHEN 'LOCK'.
        IF GV_LEVEL NA 'CO'.
          MESSAGE S000 WITH 'You have no authorization.'.
          EXIT.
        ENDIF.

        IF P_MODE = 'L'.  "locked
          MESSAGE S000 WITH
            'All records have locked. CHECK COSTING STATUS ZCOA101.'.
          EXIT.
        ELSE.
* <<< ig.Moon 8/6/07>>>
          __POPUP ''
          'Do you really want to lock the selected item(s)?' ''.

          PERFORM LOCK_DATA CHANGING LV_CNT.
          IF LV_CNT > 0.
            MESSAGE S000 WITH
                     LV_CNT 'record(s)' ' has been locked ; '.
          ENDIF.
        ENDIF.
        PERFORM REFRESH_SCREEN USING 'X'.
        CALL METHOD CL_GUI_CFW=>FLUSH.

      WHEN 'UNLOCK'.
        IF GV_LEVEL NA 'CO'.
          MESSAGE S000 WITH 'You have no authorization.'.
          EXIT.
        ENDIF.

        IF P_MODE = 'L'.  "locked
          MESSAGE S000 WITH
            'All records have locked. CHECK COSTING STATUS ZCOA101.'.
          EXIT.
        ELSE.
* <<< ig.Moon 8/6/07>>>
          __POPUP ''
          'Do you really want to unlock the selected item(s)?' ''.

          PERFORM UNLOCK_DATA CHANGING LV_CNT.
          IF LV_CNT > 0.
            MESSAGE S000 WITH
                     LV_CNT 'record(s)' ' has been unlocked ; '.
            PERFORM REFRESH_SCREEN USING 'X'.
            CALL METHOD CL_GUI_CFW=>FLUSH.
          ENDIF.
        ENDIF.
*   Adjust Info.Rec
      WHEN 'AJI'.
        IF GV_LEVEL = 'COUSR' OR GV_LEVEL = 'PUUSR'.
          MESSAGE S000 WITH 'You have no authorization.'.
          EXIT.
        ENDIF.

* <<< ig.Moon 8/6/07>>>
        __POPUP '' 'Do you want to apply Info.rec?' ''.

        PERFORM APPLY_INFO_REC.

        IF SY-SUBRC = 0.
          PERFORM REFRESH_SCREEN USING 'X'.
*          perform refresh_field1.
        ENDIF.

    ENDCASE.
* }
  ENDIF.

ENDMODULE.                 " USER_COMMAND_0050  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS '100'.

* Pur.Grp Text
  CLEAR EKNAM.
  SELECT SINGLE EKNAM INTO EKNAM
    FROM T024
   WHERE EKGRP = GT_OUT-EKGRP.

  PERFORM CREATE_ALV_CONTROL.

ENDMODULE.                 " CREATE_ALV_CONTROL  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.

  CASE OK_CODE.
    WHEN 'CLOS'.
      PERFORM MODIFY_GT_OUT.
      PERFORM REFRESH_FIELD.

      CALL METHOD G_GRID->FREE.
      CALL METHOD G_CUSTOM_CONTAINER->FREE.

      CLEAR: G_CUSTOM_CONTAINER, G_GRID.

      PERFORM REFRESH_FIELD1.

      CLEAR: GV_LOCK, GV_USER.
      SET PARAMETER ID 'ZLCK'  FIELD GV_LOCK.
      SET PARAMETER ID 'ZUSER' FIELD GV_USER.

      LEAVE TO SCREEN 0.

    WHEN 'EXIT'.
      CALL METHOD G_GRID->FREE.
      CALL METHOD G_CUSTOM_CONTAINER->FREE.

      CLEAR: G_CUSTOM_CONTAINER, G_GRID.

      PERFORM REFRESH_FIELD1.

      CLEAR: GV_LOCK, GV_USER.
      SET PARAMETER ID 'ZLCK'  FIELD GV_LOCK.
      SET PARAMETER ID 'ZUSER' FIELD GV_USER.

      LEAVE TO SCREEN 0.

    WHEN 'ENTR'.
      PERFORM MODIFY_GT_OUT.
      PERFORM REFRESH_FIELD.

    WHEN 'INFO'.
      PERFORM CALL_ZACOU112.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS '200'.
  SET TITLEBAR '200'.

ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.
  DATA LV_AMT TYPE CK_KWT.

  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.

  CASE OK_CODE.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.

    WHEN 'ENTR'.
      CLEAR: GV_EKGRP,  GV_PROFL, GV_BKLAS, GV_LIFNR, GV_QTA,
             GV_KZUST1, GV_PCT.

      IF NOT EKGRP IS INITIAL.
        GV_EKGRP = EKGRP.
      ENDIF.

      IF NOT PROFL IS INITIAL.
        GV_PROFL = PROFL.
      ENDIF.

      IF NOT BKLAS IS INITIAL.
        GV_BKLAS  = BKLAS.
      ENDIF.

      IF NOT LIFNR IS INITIAL.
        GV_LIFNR  = LIFNR.
      ENDIF.

      IF NOT QTA IS INITIAL.
        GV_QTA = QTA.
      ENDIF.

      IF NOT KZUST1 IS INITIAL.
        GV_KZUST1 = KZUST1.
      ENDIF.

      IF RATE <> 0 OR NOT RATE IS INITIAL.
        GV_PCT = RATE.
      ENDIF.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0300 OUTPUT.
  SET PF-STATUS '200'.
  SET TITLEBAR '300'.

ENDMODULE.                 " STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  get_prev_date
*&---------------------------------------------------------------------*
FORM GET_PREV_DATE USING    F_BWDAT.

  CALL FUNCTION 'CCM_GO_BACK_MONTHS'
    EXPORTING
      CURRDATE   = F_BWDAT
      BACKMONTHS = 1
    IMPORTING
      NEWDATE    = GV_PRVDT.

ENDFORM.                    " get_prev_date
*&---------------------------------------------------------------------*
*&      Module  F4_REASON  INPUT
*&---------------------------------------------------------------------*
MODULE F4_REASON INPUT.
  PERFORM POPUP_RSN USING KZUST1 'KZUST1'.

ENDMODULE.                 " F4_REASON  INPUT
*&---------------------------------------------------------------------*
*&      Form  get_info_fr_material
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_F_MATNR  text
*----------------------------------------------------------------------*
FORM GET_INFO_FR_MATERIAL USING   F_MATNR TYPE MATNR
                                  P_DATE TYPE SYDATUM.

* UD1K941202 - by IG.MOON 8/3/2007 {
*  SELECT MATNR LIFNR DATBI DATAB KNUMH
*    INTO TABLE LT_018
*    FROM A018
*   WHERE KAPPL = 'M'                " Purchasing
*     AND KSCHL = 'PB00'             " ZTIR = PB00
*     AND LIFNR IN R_LIFNR
*     AND MATNR = F_MATNR
*     AND EKORG = C_EKORG
*     AND ESOKZ = '0'                " Standard
*     AND DATAB =< P_DATE
*     AND DATBI >= P_DATE.
*
*  IF SY-DBCNT > 1.
*    SORT LT_018 BY DATAB  DESCENDING.
*    READ TABLE LT_018 INDEX 1.
*    GT_A018 = LT_018.
*    APPEND GT_A018.
*    Vendor determination

*  ELSE.
*    APPEND LINES OF LT_018 TO GT_A018.
*  ENDIF.

  DATA LT_018   LIKE GT_A018 OCCURS 0  WITH HEADER LINE.

*  PERFORM GET_INFO_REC_NEW TABLES LT_018
*                           USING F_MATNR P_DATE ' '.

  PERFORM GET_INFO_REC_NEW TABLES LT_018
                           USING F_MATNR P_DATE P_REF1.

  APPEND LINES OF LT_018 TO GT_A018.

* }

ENDFORM.                    " get_info_fr_material
*&---------------------------------------------------------------------*
*&      Module  STATUS_0400  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0400 OUTPUT.
  SET PF-STATUS '400'.
  SET TITLEBAR '400'.

ENDMODULE.                 " STATUS_0400  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0400  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0400 INPUT.
  OK_CODE = SY-UCOMM.
  CLEAR: SY-UCOMM, GV_CANC.

  CASE OK_CODE.
    WHEN 'ENTR'.
      GT_OUT-BWDAT = GV_BWDAT.
      LEAVE TO SCREEN 0.

    WHEN 'CANC'.
      GV_CANC = 'X'.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0400  INPUT


*----------------------------------------------------------------------*
*   INCLUDE ZACOU102_F01                                               *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       Get costing result
*----------------------------------------------------------------------*
FORM GET_DATA.
  CLEAR: GT_ZTCOU102, GT_OUT, GV_CSTDT, GV_SAVE.
  REFRESH: GT_ZTCOU102, GT_OUT.

* Prepare global variable
  PERFORM SET_GLOBAL_VARIABLE.

* Plants
  PERFORM GET_PLANT.

* Get LDC
  PERFORM GET_GT_LDC.

* Get previous 102 data
  PERFORM GET_PREV_102.

* BEGIN OF UD1K954962
* Get duty ratio data
  PERFORM GET_DUTY_RATIO.
* END OF UD1K954962

* Get Costing Information
*   If marked [Refresh]
*     1. get data from SAP: table CKIS,KEKO
*     2. except roll-up data (table ZTCOU102-STAT: 'O','R')
*     3. if table ZTCOU102 has data, display popup message
*     4. choose 'Yes', display list
  IF P_REF1 = 'X' OR P_BATCH = 'X'.
    PERFORM SELECT_REFRESH_DATA.

*   If unmarked [Refresh]
*     1. get data from table ZTCOU102
*     2. when it has no data, get data from SAP: table CKIS,KEKO
  ELSE.
    PERFORM SELECT_FROM_ZTCOU102.
  ENDIF.


ENDFORM.                    " GET_DATA
**&---------------------------------------------------------------------
**
**&      Form  GET_GT_OUT_GT_CKIS
**&---------------------------------------------------------------------
**
**       Get other info for Costing result
**----------------------------------------------------------------------
**
*FORM GET_GT_OUT_FROM_GT_CKIS.
*  DATA: L_DUTY  TYPE CK_KWT,      " Duty
*        L_FRG   TYPE CK_KWT,      " Freight
*        L_OTH   TYPE CK_KWT,      " Others
*        L_WERTN TYPE CK_KWT.      " Info-price
*
*  CLEAR: L_DUTY, L_FRG, L_OTH, L_WERTN, GT_VENDOR.
*  REFRESH GT_VENDOR.
*
*  LOOP AT GT_CKIS.
*    GT_OUT-KALKA  = GT_CKIS-KALKA.            " Costing type
*    GT_OUT-KADKY  = GT_CKIS-KADKY.            " Costing date
*    GT_OUT-WERKS  = GT_CKIS-WERKS.            " Plant
*    GT_OUT-KOKRS  = GT_CKIS-KOKRS.            " Controling Area
*    GT_OUT-LIFNR  = GT_CKIS-LIFNR.            " Vendor
*    GT_OUT-INFNR  = GT_CKIS-INFNR.            " Inforecord No.
*    GT_OUT-LAND1  = GT_CKIS-LAND1.            " Country key
*    GT_OUT-NAME1  = GT_CKIS-NAME1.            " Vendor name
*    GT_OUT-PMEHT  = GT_CKIS-PMEHT.            " uOm
*
*    GT_OUT-STRAT     = GT_CKIS-STRAT.
*    GT_OUT-SUBSTRAT  = GT_CKIS-SUBSTRAT.
*    GT_OUT-ELEMT     = GT_CKIS-ELEMT.
*    GT_OUT-ELEMTNS   = GT_CKIS-ELEMTNS.
*
*
**---only component exist...no break-down
*    IF GT_CKIS-COMPN = 'X'.
*      L_WERTN  = GT_CKIS-WERTN / GT_CKIS-MENGE.
*
*    ELSE.
*      CASE GT_CKIS-HRKFT.
*        WHEN 'KD-D'.
*          L_DUTY = L_DUTY + GT_CKIS-WERTN.      " Duty
*        WHEN 'KD-F'.
*          L_FRG  = L_FRG + GT_CKIS-WERTN.       " Freight
*        WHEN 'KD-O'.
*          L_OTH  = L_OTH + GT_CKIS-WERTN.       " Others
*        WHEN SPACE.
*          L_WERTN = L_WERTN + GT_CKIS-WERTN.    " Info-Price
*      ENDCASE.
*    ENDIF.
*
*    IF GT_OUT-LIFNR <> SPACE.
*      GT_OUT-QTA = 100.                       " Quota
*    ENDIF.
*
*    GT_OUT-BDATJ  = GT_CKIS-BDATJ.            " Year
*    GT_OUT-POPER  = GT_CKIS-POPER.            " Period
*    GT_OUT-BWDAT  = GT_CKIS-BWDAT.            " Valuation date
*    GT_OUT-ALDAT  = GT_CKIS-ALDAT.            " Qty structure date
*
*    AT END OF MATNR.
*      GT_OUT-MATNR  = GT_CKIS-MATNR.          " End Item
*
**     Create internal table for MAP, STD
*      PERFORM APPEND_GT_MAT_TEMP.
*
**     if costing type is MI, clear duty, freight, other
*      IF GT_OUT-KALKA = 'M1'.
*        CLEAR: GT_OUT-DUTY, GT_OUT-FRG, GT_OUT-OTH.
*        GT_OUT-WERTN    = L_WERTN.     " Net Price
*        GT_OUT-WERTN_V1 = L_WERTN.     " Net Price of 1st vendor
*      ELSE.
*        GT_OUT-DUTY     = L_DUTY.      " Duty
*        GT_OUT-FRG      = L_FRG.       " Freight
*        GT_OUT-OTH      = L_OTH.       " Others
*        GT_OUT-WERTN    = L_WERTN.     " Net Price
*        GT_OUT-WERTN_V1 = L_WERTN.     " Net Price of 2nd vendor
*      ENDIF.
*
*      IF NOT GT_CKIS-LIFNR IS INITIAL.
*        GT_VENDOR-LIFNR = GT_CKIS-LIFNR.
*        APPEND GT_VENDOR.
*        CLEAR GT_VENDOR.
*      ENDIF.
*
*      APPEND GT_OUT.
*      CLEAR: GT_OUT, L_DUTY, L_FRG, L_OTH, L_WERTN.
*    ENDAT.
*  ENDLOOP.
*
** Vendor Info.
*  PERFORM READ_VENDOR_DATA.
*
** Get Material Information
*  PERFORM READ_MAT_INFO.
*  PERFORM MODIFY_MAT_INFO.
*
** Get Info Price
*  IF P_REF2 = 'X' AND NOT GT_OUT[] IS INITIAL.
*    PERFORM GET_GT_A018.
*  ENDIF.
*
*  SORT: GT_OUT BY KALKA MATNR WERKS,
*        GT_ZTCOU102 BY KALKA MATNR WERKS,
*        GT_A018 BY MATNR LIFNR.
*
*  LOOP AT GT_OUT.
*
**   Vendor Info.
*    IF NOT GT_OUT-LIFNR IS INITIAL.
*      READ TABLE GT_VENDOR WITH KEY LIFNR = GT_OUT-LIFNR BINARY SEARCH.
*      IF SY-SUBRC = 0.
*        GT_OUT-LAND1 = GT_VENDOR-LAND1.
*        GT_OUT-NAME1 = GT_VENDOR-NAME1.
*      ENDIF.
*    ENDIF.
*
**   Previous info-price
*    PERFORM put_cur_prv_info_price.
*
**   Reason (current period)
*    PERFORM GET_REASON.
*    GT_OUT-KZUST1_IN = GT_OUT-KZUST1.
*
*
** UD1K941202 - by IG.MOON 8/2/2007 {
*    PERFORM GT_OUT_STATUS_CHANGE.
** }
*
*    MODIFY GT_OUT.
*
*  ENDLOOP.
*
*ENDFORM.                    " GET_GT_OUT_FROM_GT_CKIS
*&---------------------------------------------------------------------*
*&      Form  GET_REASON
*&---------------------------------------------------------------------*
*       Get Reason
*----------------------------------------------------------------------*
*       --> P_KNUMH   Condition record number
*----------------------------------------------------------------------*
FORM GET_REASON.

  CHECK NOT GT_A018-KNUMH IS INITIAL.

* Get reason of condition record number
  SELECT SINGLE KZUST INTO GT_OUT-KZUST1
    FROM KONH
   WHERE KNUMH = GT_A018-KNUMH.
*- U1 Start
  IF P_ARCH EQ 'X' AND SY-SUBRC <> 0.
    PERFORM ARCHIVE_READ_KONH.
  ENDIF.
*- U1 End
* If reason code is 'ZLC'
*   US vendor: clear reason code
*   International vendor: KE1(equal), KU1(up), KD1(down)
  IF SY-SUBRC = 0.
    IF GT_OUT-KZUST1 = 'ZLC' OR GT_OUT-KZUST1 IS INITIAL.
      IF GT_OUT-LAND1 <> 'US'.
        IF GT_OUT-WERTN = GT_OUT-PWERTN.
          GT_OUT-KZUST1 = 'KE1'.
        ELSEIF GT_OUT-WERTN > GT_OUT-PWERTN.
          GT_OUT-KZUST1 = 'KU1'.
        ELSEIF GT_OUT-WERTN < GT_OUT-PWERTN.
          GT_OUT-KZUST1 = 'KD1'.
        ENDIF.

      ELSE.
        CLEAR GT_OUT-KZUST1.
      ENDIF.

    ENDIF.
  ENDIF.

*Desciption of reason.


ENDFORM.                    " GET_REASON
*&---------------------------------------------------------------------*
*&      Form  GET_ERR_CATEGORY
*&---------------------------------------------------------------------*
*       Get Error Category
*----------------------------------------------------------------------*
FORM GET_ERR_CATEGORY USING    P_TAB STRUCTURE GT_OUT
                      CHANGING P_EC_G
                               P_EC_A
                               P_EC_S
                               P_EC_V
                               P_EC_P
                               P_EC_R.
*-LOCKED record - no change allowed!!!
  CHECK P_TAB-ZLOCK = SPACE.

  CLEAR: P_EC_G, P_EC_A, P_EC_S,
         P_EC_V, P_EC_P, P_EC_R.

* Error Category-Purchasing Group
*  : marking 'X' in case of has no pur.grp
  IF P_TAB-EKGRP = SPACE.
    P_EC_G = 'X'.
  ENDIF.

* Error Category-Account
  PERFORM GET_ERROR_ACCOUNT USING P_TAB
                            CHANGING P_EC_A.

* Error Category-Source: 'X' marking in case of has no source
  IF P_TAB-PROFL = SPACE.
    P_EC_S = 'X'.
  ENDIF.

* Error Category-Vendor
*  : 'X' marking in case of the vandor code start of 'U'
  IF P_TAB-LIFNR IS INITIAL.
    P_EC_V = 'X'.
  ENDIF.

* Error Category-Price
*  : 'X' marking in case of exist the vandor but price is zero
  IF P_TAB-WERTN = 0.
    P_EC_P = 'X'.
  ENDIF.

* Error Category-Reason: 'X' marking in case of has no reason
  IF P_TAB-KZUST1 = SPACE.
    P_EC_R = 'X'.
  ENDIF.

ENDFORM.                    " GET_ERR_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  SAVE_ZTCOU102_BATCH
*&---------------------------------------------------------------------*
FORM SAVE_ZTCOU102_BATCH.
  DATA: LT_ZTCOU102 TYPE TABLE OF ZTCOU102 WITH HEADER LINE.

  REFRESH: LT_ZTCOU102.

  LOOP AT GT_OUT.
    PERFORM GET_ERR_CATEGORY USING    GT_OUT
                             CHANGING GT_OUT-EC_G
                                      GT_OUT-EC_A
                                      GT_OUT-EC_S
                                      GT_OUT-EC_V
                                      GT_OUT-EC_P
                                      GT_OUT-EC_R.

    MOVE-CORRESPONDING GT_OUT TO LT_ZTCOU102.
    LT_ZTCOU102-AEDAT = SY-DATUM.
    LT_ZTCOU102-CPUTM = SY-UZEIT.
    LT_ZTCOU102-AENAM = SY-UNAME.
* 9/2007
    LT_ZTCOU102-GRSPR = LT_ZTCOU102-WERTN +
                        LT_ZTCOU102-DUTY +
                        LT_ZTCOU102-FRG +
                        LT_ZTCOU102-OTH.
*
    IF LT_ZTCOU102-ZLOCK EQ SPACE.
      APPEND LT_ZTCOU102.
      CLEAR LT_ZTCOU102.
    ENDIF.
  ENDLOOP.

  CHECK SY-TABIX > 0.

  DELETE FROM ZTCOU102 WHERE KOKRS = P_KOKRS
                         AND BDATJ = P_YEAR
                         AND POPER = P_POPER
                         AND KALKA IN S_KALKA
                         AND ZLOCK EQ SPACE     "unlocked item only
                         .
  INSERT ZTCOU102 FROM TABLE LT_ZTCOU102.
  MESSAGE S000 WITH 'Component data is saved! ' SY-DBCNT ' records'.

ENDFORM.                    "save_ztcou102_batch
*&---------------------------------------------------------------------*
*&      Form  SAVE_ZTCOU102
*&---------------------------------------------------------------------*
*       Save data to Table ZTCOU102
*----------------------------------------------------------------------*
FORM SAVE_ZTCOU102 CHANGING P_CNT.

  DATA: L_MATNR     TYPE MATNR,
        LS_ZTCOU102 LIKE ZTCOU102,
        LT_ZTCOU102 TYPE TABLE OF ZTCOU102 WITH HEADER LINE,
        LT_DEL_ROWS TYPE TABLE OF ZTCOU102.
* UD1K940725 by IG.MOON
*       Delete Lines
  CALL METHOD G_EVENT_RECEIVER1->GET_DELETED_ROWS
    IMPORTING
      DELETED_ROWS = LT_DEL_ROWS.
* end of UD1K940725

  RANGES R_KALKA FOR KEKO-KALKA.

  CLEAR: GT_ROW[], GT_ROID[], P_CNT, LS_ZTCOU102, LT_ZTCOU102, R_KALKA.
  REFRESH: LT_ZTCOU102, R_KALKA.

* Get selected rows
  IF P_REF1 = 'X'.
    LOOP AT GT_OUT.
      GS_ROW-INDEX = SY-TABIX.
      APPEND GS_ROW TO GT_ROW.
    ENDLOOP.
  ELSE.
    CALL METHOD G_GRID1->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = GT_ROW
        ET_ROW_NO     = GT_ROID.
  ENDIF.

  R_KALKA-SIGN = 'I'.
  R_KALKA-OPTION = 'EQ'.

  LOOP AT GT_ROW INTO GS_ROW.
    READ TABLE GT_OUT INDEX GS_ROW-INDEX.

    PERFORM GET_ERR_CATEGORY USING    GT_OUT
                             CHANGING GT_OUT-EC_G
                                      GT_OUT-EC_A
                                      GT_OUT-EC_S
                                      GT_OUT-EC_V
                                      GT_OUT-EC_P
                                      GT_OUT-EC_R.
    IF NOT GT_OUT-KZUST1_IN IS INITIAL.
      GT_OUT-KZUST1 = GT_OUT-KZUST1_IN.
    ENDIF.

    PERFORM GT_OUT_STATUS_CHANGE.
    MODIFY GT_OUT INDEX GS_ROW-INDEX.

*    MODIFY GT_OUT INDEX GS_ROW-INDEX
*       TRANSPORTING EC_G EC_A EC_S EC_D
*                    EC_V EC_P EC_R STAT.


* UD1K940725 by IG.MOON 6/26/2007
*    IF GV_CHK = 'X'.
*      READ TABLE GT_ZTCOU102 WITH KEY KALKA = GT_OUT-KALKA
*                                      MATNR = GT_OUT-MATNR
*                                      WERKS = GT_OUT-WERKS
*                                      BINARY SEARCH.
*      IF SY-SUBRC EQ 0.
*        IF GT_ZTCOU102-WERTN <> GT_OUT-WERTN.
*          LT_ZTCOU102-STAT = 'C'.
*          LT_ZTCOU102-AENAM = SY-UNAME.
*          LT_ZTCOU102-CPUTM = SY-UZEIT.
*          LT_ZTCOU102-AEDAT = SY-DATUM.
*        ELSE.
*          LT_ZTCOU102-STAT = 'C'.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*    IF GT_OUT-STAT <> SPACE.
*      IF GV_CHK = 'X' AND GT_OUT-STAT <> 'O' AND GT_OUT-STAT <> 'R' .
*        LT_ZTCOU102-STAT = 'C'.
*      ENDIF.
*    ENDIF.
* end of UD1K940725

*    LT_ZTCOU102-MANDT = SY-MANDT.
*    LT_ZTCOU102-AEDAT = SY-DATUM.
*    LT_ZTCOU102-AENAM = SY-UNAME.

    R_KALKA-LOW = GT_OUT-KALKA.
    APPEND R_KALKA.

    MOVE-CORRESPONDING GT_OUT TO LT_ZTCOU102.

    LT_ZTCOU102-GRSPR = LT_ZTCOU102-WERTN +
                        LT_ZTCOU102-DUTY +
                        LT_ZTCOU102-FRG +
                        LT_ZTCOU102-OTH.
*

    APPEND LT_ZTCOU102.
    CLEAR LT_ZTCOU102.
  ENDLOOP.

*  if p_all = space.  "show error only, changed only
*    insert lines of gt_out1 into table gt_out.
*  endif.

  SORT R_KALKA.
  DELETE ADJACENT DUPLICATES FROM R_KALKA.

  IF P_REF1 = 'X'.
*    CALL METHOD G_GRID1->GET_FILTERED_ENTRIES
*      IMPORTING
*         ET_FILTERED_ENTRIES = GT_FILTER.
*
*
*    CALL METHOD G_GRID1->get_filter_criteria
*    IMPORTING
*         ET_FILTER     = GT_FILTER.
    DELETE FROM ZTCOU102 WHERE KOKRS = P_KOKRS
                           AND BDATJ = P_YEAR
                           AND POPER = P_POPER
                           AND KALKA IN R_KALKA
                           AND ZLOCK EQ SPACE     "unlocked item only
                           AND MATNR IN S_MATNR
                           AND LIFNR IN S_LIFNR
                           AND EKGRP IN S_EKGRP.
  ENDIF.

  LOOP AT LT_ZTCOU102 INTO LS_ZTCOU102.
    READ TABLE GT_ZTCOU102 WITH KEY MATNR = LS_ZTCOU102-MATNR
               BINARY SEARCH.
    IF SY-SUBRC = 0.
      IF GT_ZTCOU102-ZLOCK = 'X'.

      ELSE.
*    CLEAR *ZTCOU102.
*    SELECT SINGLE * INTO *ZTCOU102
*               FROM ZTCOU102 WHERE KOKRS EQ LS_ZTCOU102-KOKRS
*                                   AND  BDATJ EQ LS_ZTCOU102-BDATJ
*                                   AND  POPER EQ LS_ZTCOU102-POPER
*                                   AND  KALKA EQ LS_ZTCOU102-KALKA
*                                   AND  VER   EQ LS_ZTCOU102-VER
*                                   AND  MATNR EQ LS_ZTCOU102-MATNR.
*    IF SY-SUBRC EQ 0.
*      IF *ZTCOU102-ZLOCK EQ 'X'.
*        IF LS_ZTCOU102-ZLOCK EQ SPACE.
*           *ZTCOU102-ZLOCK = SPACE.
*          MODIFY ZTCOU102 FROM *ZTCOU102.
*          IF SY-SUBRC = 0.
*            P_CNT = P_CNT + 1.
*          ENDIF.
*        ENDIF.
*      ELSE.
        MODIFY ZTCOU102 FROM LS_ZTCOU102.
        IF SY-SUBRC = 0.
          P_CNT = P_CNT + 1.
        ENDIF.
      ENDIF.
    ELSE.
      INSERT INTO ZTCOU102 VALUES LS_ZTCOU102.
      IF SY-SUBRC = 0.
        P_CNT = P_CNT + 1.
      ENDIF.
    ENDIF.
  ENDLOOP.

*  perform refresh_field1.
  PERFORM READ_ZTCOU102_FROM_DB CHANGING SY-SUBRC.

ENDFORM.                    " SAVE_ZTCOU102
*&---------------------------------------------------------------------*
*&      Form  ROLL_UP
*&---------------------------------------------------------------------*
*       Roll-Up
*----------------------------------------------------------------------*
FORM ROLL_UP.
  DATA L_CHK.

  SUBMIT ZACOU103 "VIA SELECTION-SCREEN
                       WITH P_KOKRS = P_KOKRS
                       WITH S_KALKA IN S_KALKA
                       WITH P_YEAR  = P_YEAR
                       WITH P_POPER = P_POPER
                       WITH P_CHANGE = 'X'
         AND RETURN.

  CLEAR L_CHK.
  GET PARAMETER ID 'ZRU' FIELD L_CHK.

* Status
*   C: Changed
*   O: Original -> Roll-Up
*   R: Changed -> Roll-Up
  IF L_CHK = 'X'.
    GT_OUT-STAT = 'O'.
    MODIFY GT_OUT TRANSPORTING STAT WHERE STAT = 'C' OR STAT = SPACE.

    GT_OUT-STAT = 'R'.
    MODIFY GT_OUT TRANSPORTING STAT WHERE STAT = 'O'.

    PERFORM REFRESH_FIELD1.
  ENDIF.

ENDFORM.                    " ROLL_UP
*&---------------------------------------------------------------------*
*&      Form  MODIFY_GT_OUT
*&---------------------------------------------------------------------*
*       Modify Internal Table
*----------------------------------------------------------------------*
FORM MODIFY_GT_OUT.
  DATA: LV_LINE  TYPE I,         " Count of detail
        LV_WERTN TYPE CK_KWT,    " Info-price
        LV_LIFNR TYPE LIFNR.

  CLEAR: LV_LINE, LV_LIFNR.
  DESCRIBE TABLE GT_DTL LINES LV_LINE.

  LV_LIFNR = GT_OUT-LIFNR.

  IF LV_LINE > 2.
    MESSAGE I000 WITH 'You can set up only two vendors!'.
    EXIT.
  ENDIF.

* Modify internal table for information
  PERFORM MODIFY_GT_DTL USING LV_LINE.

  IF LV_LINE > 1.
*   New line : move data to gt_out as information of 2nd vendor
    READ TABLE GT_DTL INDEX 1.
    IF SY-SUBRC = 0.
      IF GT_DTL-LIFNR = GT_OUT-LIFNR.
        PERFORM MODIFY_GT_OUT_DATA USING    LV_LINE
                                   CHANGING LV_WERTN.
      ELSE.
        PERFORM MODIFY_GT_OUT_DATA_V2 USING LV_WERTN.
      ENDIF.
    ENDIF.

*   Existing line
    READ TABLE GT_DTL INDEX 2.
    IF SY-SUBRC = 0.
      IF GT_DTL-LIFNR = GT_OUT-LIFNR.
        PERFORM MODIFY_GT_OUT_DATA USING    LV_LINE
                                   CHANGING LV_WERTN.
      ELSE.
        PERFORM MODIFY_GT_OUT_DATA_V2 USING LV_WERTN.
      ENDIF.
    ENDIF.

  ELSE.
    CLEAR : GT_OUT-LIFNR2,
            GT_OUT-LIFNR2,
            GT_OUT-QTA_V2,
            GT_OUT-WERTN_V2,
            GT_OUT-KZUST1_V2,
            GT_OUT-WERTN1_V2,
            GT_OUT-KZUST2_V2,
            GT_OUT-WERTN2_V2.

    PERFORM MODIFY_GT_OUT_DATA USING    LV_LINE
                               CHANGING LV_WERTN.
  ENDIF.

* UD1K941202 - by IG.MOON 8/2/2007 {

* Status
*  PERFORM GET_STAT.
*
*  GT_OUT-DIFF = GT_OUT-WERTN - GT_OUT-PWERTN.
*  GT_OUT-AENAM = SY-UNAME.
*  GT_OUT-AEDAT = SY-DATUM.
  PERFORM GT_OUT_STATUS_CHANGE.

* }

  MODIFY GT_OUT INDEX GV_INDEX.

ENDFORM.                    " MODIFY_GT_OUT
*&---------------------------------------------------------------------*
*&      Form  CHANGE_LIFNR
*&---------------------------------------------------------------------*
*       Change Vendor & Description
*----------------------------------------------------------------------*
FORM CHANGE_LIFNR
     USING RR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
           LS_MOD_CELLS    TYPE LVC_S_MODI.

  DATA: LV_NAME1 TYPE NAME1_GP,
        L_LAND1  TYPE LAND1_GP.

  CALL METHOD RR_DATA_CHANGED->MODIFY_CELL
    EXPORTING
      I_ROW_ID    = LS_MOD_CELLS-ROW_ID
      I_FIELDNAME = LS_MOD_CELLS-FIELDNAME
      I_VALUE     = LS_MOD_CELLS-VALUE.

  GT_DTL-LIFNR = LS_MOD_CELLS-VALUE.

* Vedor Description
  CLEAR: LV_NAME1, L_LAND1.

  SELECT SINGLE NAME1 LAND1
    INTO (LV_NAME1, L_LAND1)
    FROM LFA1
   WHERE LIFNR = LS_MOD_CELLS-VALUE.

  IF SY-SUBRC = 0.
    CALL METHOD RR_DATA_CHANGED->MODIFY_CELL
      EXPORTING
        I_ROW_ID    = LS_MOD_CELLS-ROW_ID
        I_FIELDNAME = 'NAME1'
        I_VALUE     = LV_NAME1.

    CALL METHOD RR_DATA_CHANGED->MODIFY_CELL
      EXPORTING
        I_ROW_ID    = LS_MOD_CELLS-ROW_ID
        I_FIELDNAME = 'LAND1'
        I_VALUE     = L_LAND1.
  ENDIF.

  IF GT_DTL-PWERTN = 0.
    CALL METHOD RR_DATA_CHANGED->MODIFY_CELL
      EXPORTING
        I_ROW_ID    = LS_MOD_CELLS-ROW_ID
        I_FIELDNAME = 'PWERTN'
        I_VALUE     = GT_OUT-PWERTN.

    GT_DTL-PWERTN = GT_OUT-PWERTN.
  ENDIF.

ENDFORM.                    " CHANGE_LIFNR
*&---------------------------------------------------------------------*
*&      Form  CHANGE_WERTN1
*&---------------------------------------------------------------------*
*       Change Info-Price & Duty & Freight & Other
*----------------------------------------------------------------------*
FORM CHANGE_WERTN1
     USING RR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
           LS_MOD_CELLS    TYPE LVC_S_MODI.

  CALL METHOD RR_DATA_CHANGED->MODIFY_CELL
    EXPORTING
      I_ROW_ID    = LS_MOD_CELLS-ROW_ID
      I_FIELDNAME = LS_MOD_CELLS-FIELDNAME
      I_VALUE     = LS_MOD_CELLS-VALUE.

  GT_DTL-WERTN1 = LS_MOD_CELLS-VALUE.
  MODIFY GT_DTL INDEX LS_MOD_CELLS-ROW_ID.

ENDFORM.                    " CHANGE_WERTN1
*&---------------------------------------------------------------------*
*&      Form  CALL_CHANGE_INFO
*&---------------------------------------------------------------------*
*       Execute CHANGE_INFO
*----------------------------------------------------------------------*
FORM CALL_CHANGE_INFO.
  CLEAR GT_DTL.
  REFRESH GT_DTL.

* if the material have 2nd vendor
  IF GT_OUT-LIFNR2 <> SPACE.
    MOVE-CORRESPONDING GT_OUT TO GT_DTL.
*   1st vendor information
    GT_DTL-LIFNR  = GT_OUT-LIFNR.        " 1st vendor
    GT_DTL-QTA    = GT_OUT-QTA.          " Quata
    GT_DTL-PWERTN = GT_OUT-PWERTN.       " Previous Info-price
    GT_DTL-WERTN  = GT_OUT-WERTN_V1.     " Info-price
    GT_DTL-KZUST1 = GT_OUT-KZUST1.                          " Reason1
    GT_DTL-KZUST1_IN = GT_OUT-KZUST1.    " Reason1 for Input
    GT_DTL-WERTN1 = GT_OUT-WERTN1.                          " RS1 $
    GT_DTL-KZUST2 = GT_OUT-KZUST2.                          " Reason2
    GT_DTL-WERTN2 = GT_OUT-WERTN2.                          " RS1 $
    GT_DTL-NAME1 = GT_OUT-NAME1.         " Vendor name
    GT_DTL-INDX = 1.                     " Index

    APPEND GT_DTL.
    CLEAR GT_DTL.

    MOVE-CORRESPONDING GT_OUT TO GT_DTL.
*   2nd vendor information
    GT_DTL-LIFNR  = GT_OUT-LIFNR2.

    GT_DTL-QTA    = GT_OUT-QTA_V2.
    GT_DTL-PWERTN = GT_OUT-PWERTN.
    GT_DTL-WERTN  = GT_OUT-WERTN_V2.
    GT_DTL-KZUST1 = GT_OUT-KZUST1_V2.
    GT_DTL-WERTN1 = GT_OUT-WERTN1_V2.
    GT_DTL-KZUST2 = GT_OUT-KZUST2_V2.
    GT_DTL-WERTN2 = GT_OUT-WERTN2_V2.

    READ TABLE GT_LFA1 WITH KEY LIFNR = GT_OUT-LIFNR2 BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_DTL-NAME1 = GT_LFA1-NAME1.
    ELSE.
      SELECT SINGLE NAME1 INTO GT_DTL-NAME1 FROM LFA1
      WHERE LIFNR EQ GT_OUT-LIFNR2.
    ENDIF.


    GT_DTL-INDX = 2.

    APPEND GT_DTL.
    CLEAR GT_DTL.

* if the material have only one vendor
  ELSE.
    MOVE-CORRESPONDING GT_OUT TO GT_DTL.
    GT_DTL-INDX = 1.                     " Index

    APPEND GT_DTL.
    CLEAR GT_DTL.
  ENDIF.

  PERFORM GET_RATE USING GT_OUT-WERTN.  " Duty, Freight, Other Rate

  CALL SCREEN 100 STARTING AT 20 8 ENDING AT 110 20.

ENDFORM.                    " CALL_CHANGE_INFO
*&---------------------------------------------------------------------*
*&      Form  put_cur_prv_info_price
*&---------------------------------------------------------------------*
*       Get info-price of previous period
*----------------------------------------------------------------------*
FORM PUT_CUR_PRV_INFO_PRICE.

*current info price, reason code!!!! - FIXME

*---TEMP condition... - FIXME
  IF GT_OUT-WERTN IS INITIAL.
    DATA: L_OUTPUT TYPE P DECIMALS 4 .
    PERFORM UNIT_CONVERION USING  GT_A018-KPEIN  "p_input
                                  GT_A018-KMEIN "p_unit_in
                                  GT_OUT-PMEHT  "p_unit_out
                        CHANGING  L_OUTPUT.

    GT_OUT-WERTN_V1 = GT_OUT-PEINH *
             ( GT_A018-TKBETR / L_OUTPUT ).
    GT_OUT-WERTN = GT_OUT-WERTN_V1.
  ENDIF.

  IF GT_OUT-LIFNR IS INITIAL.
    GT_OUT-LIFNR = GT_A018-LIFNR.
  ENDIF.

*previous info price
  IF GV_PRVDT IS INITIAL.
    PERFORM GET_PREV_DATE USING GT_OUT-BWDAT.
  ENDIF.

* previous Info-Price
*   : in case of ABP(Costing Type: BP),
*     previous Info-Price = current Info-Price
  IF GT_OUT-KALKA = 'BP'.
    GT_OUT-PWERTN = GT_OUT-WERTN.
  ELSE.
    READ TABLE GT_102P WITH KEY MATNR = GT_OUT-MATNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_OUT-PWERTN = GT_102P-WERTN.

    ELSE.
* use 102 (previous), if not exist, search info-record

      READ TABLE GT_P018 WITH KEY MATNR = GT_OUT-MATNR BINARY SEARCH.

      IF SY-SUBRC <> 0.
        DATA L_FMATNR TYPE MATNR.

        CLEAR L_FMATNR.
        SELECT SINGLE FMATNR INTO L_FMATNR FROM ZTCOU105
           WHERE KOKRS  = P_KOKRS
             AND TMATNR = GT_OUT-MATNR.

        IF SY-SUBRC = 0.
          READ TABLE GT_P018 WITH KEY MATNR = L_FMATNR BINARY SEARCH.
        ENDIF.
      ENDIF.

      DATA LT_018T   LIKE GT_A018 OCCURS 0  WITH HEADER LINE.
      IF SY-SUBRC = 0.
        REFRESH LT_018T.
        APPEND GT_P018 TO LT_018T.
        PERFORM GET_INFO_PRICE TABLES LT_018T.   " USING f_matnr.

        DATA: L_KPEIN LIKE KONP-KPEIN.

*        SELECT SUM( kbetr ) AVG( kpein )
*          INTO (gt_out-pwertn, l_kpein)
*          FROM konp
*         WHERE knumh = gt_p018-knumh
*           AND kappl = 'M'
*         AND ( kschl = 'PB00' OR        " Gross price
*               kschl = 'ZTIR' ).
        READ TABLE LT_018T INDEX 1.
        PERFORM UNIT_CONVERION USING  GT_A018-KPEIN  "p_input
                                      GT_A018-KMEIN "p_unit_in
                                      GT_OUT-PMEHT  "p_unit_out
                            CHANGING  L_OUTPUT.

        GT_OUT-PWERTN = TRUNC( ( LT_018T-TKBETR * 1000 ) / L_OUTPUT ).
        GT_OUT-PWERTN = GT_OUT-PEINH * GT_OUT-PWERTN / 1000.

* costing unit <> purchasing price unit
*        gt_out-pwertn = gt_out-peinh *
*                       ( gt_out-pwertn / l_kpein ).

* use same price
      ELSE.
        GT_OUT-PWERTN = GT_OUT-WERTN_V1.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.                    " put_cur_prv_info_price
*&---------------------------------------------------------------------*
*&      Form  CAL_PRICE
*&---------------------------------------------------------------------*
*       Calculate Price
*       NOT VALID for HMMA
*----------------------------------------------------------------------*
FORM CAL_PRICE.
  GT_OUT-DUTY = GT_OUT-WERTN * GV_DRATE / 100.
  GT_OUT-FRG  = GT_OUT-WERTN * GV_FRATE / 100.
  GT_OUT-OTH  = GT_OUT-WERTN * GV_ORATE / 100.

  GT_OUT-GPRC = GT_OUT-WERTN + GT_OUT-DUTY + GT_OUT-FRG + GT_OUT-OTH.
ENDFORM.                    " CAL_PRICE
*&---------------------------------------------------------------------*
*&      Form  CHANGE_WERTN2
*&---------------------------------------------------------------------*
*       Check Info-Price
*----------------------------------------------------------------------*
FORM CHANGE_WERTN2 USING RR_DATA_CHANGED TYPE REF TO
                                         CL_ALV_CHANGED_DATA_PROTOCOL
                         LS_MOD_CELLS    TYPE LVC_S_MODI.
  DATA: LV_TOT  TYPE CK_KWT,
        LV_DIFF TYPE CK_KWT.

  CALL METHOD RR_DATA_CHANGED->MODIFY_CELL
    EXPORTING
      I_ROW_ID    = LS_MOD_CELLS-ROW_ID
      I_FIELDNAME = LS_MOD_CELLS-FIELDNAME
      I_VALUE     = LS_MOD_CELLS-VALUE.

  CLEAR: LV_TOT, LV_DIFF.

  LV_DIFF = GT_DTL-WERTN - GT_DTL-PWERTN.
  LV_TOT = GT_DTL-WERTN1 + LS_MOD_CELLS-VALUE.

  IF LV_TOT <> LV_DIFF.
    PERFORM DATA_INPUT_ERROR USING RR_DATA_CHANGED
                                   LS_MOD_CELLS
                                   'E'
                                   'Check RS2 price!'
                                   'WERTN2'.

  ELSE.
    GT_DTL-WERTN2 = LS_MOD_CELLS-VALUE.
  ENDIF.

ENDFORM.                    " CHANGE_WERTN2
*&---------------------------------------------------------------------*
*&      Form  GET_GT_OUT_FROM_GT_ZTCOU102
*&---------------------------------------------------------------------*
FORM GET_GT_OUT_FROM_GT_ZTCOU102.

  DESCRIBE TABLE GT_ZTCOU102 LINES SY-TABIX.
  CHECK SY-TABIX > 0.

  LOOP AT GT_ZTCOU102.
    MOVE-CORRESPONDING GT_ZTCOU102 TO GT_OUT.

*   if costing type is MI, clear duty, freight, other
    IF GT_OUT-KALKA = 'M1'.
      CLEAR: GT_OUT-DUTY, GT_OUT-FRG, GT_OUT-OTH.
    ENDIF.

    APPEND GT_OUT.
    CLEAR GT_OUT.
  ENDLOOP.

  IF P_REF1 = 'X'.
    PERFORM GET_GT_A018.
  ENDIF.


  PERFORM GET_VENDOR_FR_GT_OUT.

  LOOP AT GT_OUT.
**   Get info-price of previous period
*      PERFORM put_cur_prv_info_price.

*   Create internal table for MAP, STD
*    PERFORM APPEND_GT_MAT_TEMP.

    PERFORM GET_MAT_LEVEL.                                  "UD1K949919
    PERFORM GT_OUT_STATUS_CHANGE.

    MODIFY GT_OUT.
  ENDLOOP.

ENDFORM.                    " GET_GT_OUT_FROM_GT_ZTCOU102
*&---------------------------------------------------------------------*
*&      Form  MODIFY_GT_DTL
*&---------------------------------------------------------------------*
FORM MODIFY_GT_DTL USING P_LINE TYPE I.
  DATA: LV_QTA TYPE ZQTA,
        LV_TOT TYPE ZQTA.

  IF P_LINE = 1.
*   recalculate 1st vendor quota
    READ TABLE GT_DTL INDEX 1.
    IF SY-SUBRC = 0.
      IF GT_DTL-QTA <> 100.
        MESSAGE S000 WITH 'Check quota!'.
        EXIT.
      ENDIF.
    ENDIF.

  ELSEIF P_LINE > 1.
*   Case of add vendor
    READ TABLE GT_DTL INDEX 1.
    IF GT_DTL-QTA <> SPACE AND GT_DTL-QTA > 0.
      CLEAR LV_QTA.
      LV_QTA = GT_DTL-QTA.
    ENDIF.

*   recalculate 1st vendor quota
    READ TABLE GT_DTL INDEX 2.

    GT_DTL-QTA = 100 - LV_QTA.

    CLEAR LV_TOT.
    LV_TOT = LV_QTA + GT_DTL-QTA.

    IF LV_TOT = 100.
      MODIFY GT_DTL INDEX 2 TRANSPORTING QTA. " WHERE BDATJ = P_YEAR.
    ELSE.
      MESSAGE S000 WITH 'Check quota!'.
      EXIT.
    ENDIF.

  ENDIF.

ENDFORM.                    " MODIFY_GT_DTL
*&---------------------------------------------------------------------*
*&      Form  GET_RATE
*&---------------------------------------------------------------------*
*       Duty, Freight, Other Rate from STD cost.estimates...
*       NO LONGER USED in HMMA - ANDY
*----------------------------------------------------------------------*
FORM GET_RATE USING P_WERTN TYPE ZWERTN1.
  CLEAR: GV_DRATE, GV_FRATE.

  IF GT_OUT-WERTN <> 0.
    IF GT_OUT-DUTY <> 0.
      GV_DRATE = 100 * GT_OUT-DUTY / P_WERTN.
    ENDIF.

    IF GT_OUT-FRG <> 0.
      GV_FRATE = 100 * GT_OUT-FRG / P_WERTN.
    ENDIF.

    IF GT_OUT-OTH <> 0.
      GV_ORATE = 100 * GT_OUT-OTH / P_WERTN.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_RATE
*&---------------------------------------------------------------------*
*&      Form  GET_STAT
*&---------------------------------------------------------------------*
*       Status
*----------------------------------------------------------------------*
FORM GET_STAT.
  DATA LV_MATNR.

  CLEAR LV_MATNR.
  SELECT SINGLE COMPN INTO LV_MATNR
    FROM ZTCOU103
   WHERE BDATJ = P_YEAR
     AND POPER = P_POPER
     AND COMPN = GT_OUT-MATNR.

*- U1 Start
  IF P_ARCH EQ 'X' AND SY-SUBRC <> 0.
    PERFORM ARCHIVE_READ_ZTCOU103 CHANGING LV_MATNR.
  ENDIF.
*- U1 End

  IF SY-SUBRC = 0.
    IF GT_OUT-STAT = 'O'.
      GT_OUT-STAT = 'R'.
    ELSEIF GT_OUT-STAT = 'C'.
      GT_OUT-STAT = 'O'.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_STAT
*&---------------------------------------------------------------------*
*&      Form  SELECT_FROM_ZTCOU102
*&---------------------------------------------------------------------*
FORM SELECT_FROM_ZTCOU102.
* 1. get data from table ZTCOU102
  PERFORM READ_ZTCOU102_FROM_DB CHANGING SY-SUBRC.

  IF SY-SUBRC = 0.
    IF P_CHG = 'X'.  "show changed item only
      DELETE GT_ZTCOU102 WHERE STAT <> 'C'
                           AND STAT <> 'R'.
    ENDIF.

    PERFORM GET_GT_OUT_FROM_GT_ZTCOU102.

* 2. when it has no data, get data from SAP: table CKIS,KEKO
  ELSE.
    P_REF1 = 'X'.

* UD1K940725 by IG.MOON
*    IF P_LOGIC EQ SPACE.
*      PERFORM GET_GT_CKIS_OLD_LOGIC.
*      PERFORM GET_GT_OUT_FROM_GT_CKIS.
*    ELSE.
    PERFORM GET_GT_CKIS_NEW_LOGIC.
    PERFORM GET_GT_OUT_FROM_GT_CKIS_N.
    PERFORM GET_GT_OUT_OTHERS.
*    ENDIF.
* end of UD1K940725
  ENDIF.

ENDFORM.                    " SELECT_FROM_ZTCOU102
*&---------------------------------------------------------------------*
*&      Form  MODIFY_GT_OUT_DATA
*&---------------------------------------------------------------------*
FORM MODIFY_GT_OUT_DATA USING    P_LINE  TYPE I
                        CHANGING P_WERTN TYPE CK_KWT.

* Vendor
  IF GT_OUT-LIFNR <> GT_DTL-LIFNR.
    GT_OUT-LIFNR = GT_DTL-LIFNR.
    GT_OUT-LAND1 = GT_DTL-LAND1.
  ENDIF.

* Reason1
  IF GT_DTL-KZUST1_IN <> SPACE.
    GT_OUT-KZUST1_IN = GT_DTL-KZUST1_IN.
    GT_OUT-KZUST1 = GT_OUT-KZUST1_IN.
  ENDIF.

* Reason2
  IF GT_DTL-KZUST2_IN <> SPACE.
    GT_OUT-KZUST2_IN = GT_DTL-KZUST2_IN.
    GT_OUT-KZUST2 = GT_DTL-KZUST2.
  ENDIF.

* 1st vendor Info-price
  GT_OUT-WERTN_V1 = GT_DTL-WERTN.

  GT_OUT-QTA    = GT_DTL-QTA.        " Quota
  GT_OUT-WERTN1 = GT_DTL-WERTN1.                            " RS1$
  GT_OUT-WERTN2 = GT_DTL-WERTN2.                            " RS2$

  IF P_LINE = 1.
    GT_OUT-WERTN = GT_DTL-WERTN.     " Info-price

    IF P_LDC = 'X'.
      PERFORM GET_ABP_LDC_GT_OUT.
    ELSE.
      PERFORM CAL_PRICE.
    ENDIF.

    MODIFY GT_OUT INDEX GV_INDEX.
  ENDIF.

  IF GT_DTL-QTA = SPACE.
    P_WERTN = GT_DTL-WERTN.
  ELSE.
    P_WERTN = GT_DTL-WERTN * ( GT_DTL-QTA / 100 ).
  ENDIF.

ENDFORM.                    " MODIFY_GT_OUT_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_GT_OUT_DATA_V2
*&---------------------------------------------------------------------*
FORM MODIFY_GT_OUT_DATA_V2 USING  P_WERTN TYPE CK_KWT.
  GT_OUT-LIFNR2 = GT_DTL-LIFNR.           " 2nd vendor

* 2nd vendor reason1
  IF GT_DTL-KZUST1 <> SPACE.
    GT_OUT-KZUST1_V2 = GT_DTL-KZUST1.
  ENDIF.

* 2nd vendor reason2
  IF GT_DTL-KZUST2 <> SPACE.
    GT_OUT-KZUST2_V2 = GT_DTL-KZUST2.
  ENDIF.

* Info-price
  IF GT_DTL-QTA = SPACE.
    GT_OUT-WERTN = ( P_WERTN + GT_DTL-WERTN ) / 2.
  ELSE.
    GT_OUT-WERTN = P_WERTN +
                   ( GT_DTL-WERTN * ( GT_DTL-QTA / 100 ) ).
  ENDIF.

* 2nd vendor Info-price
  GT_OUT-WERTN_V2 = GT_DTL-WERTN.

* 2nd vendor quata
  GT_OUT-QTA_V2 = GT_DTL-QTA.

* 2nd vendor RS1 $
  GT_OUT-WERTN1_V2 = GT_DTL-WERTN1.

* 2nd vendor RS2 $
  GT_OUT-WERTN2_V2 = GT_DTL-WERTN2.

  IF P_LDC = 'X'.
    PERFORM GET_ABP_LDC_GT_OUT.
  ELSE.
    PERFORM CAL_PRICE.
  ENDIF.

ENDFORM.                    " MODIFY_GT_OUT_DATA_V2
*&---------------------------------------------------------------------*
*&      Form  SELECT_REFRESH_DATA
*&---------------------------------------------------------------------*
*   If marked [Refresh]
*     1. get data from SAP: table CKIS,KEKO,MBEW,MARC
*     2. except roll-up data (table ZTCOU102-STAT: 'O','R')
*     3. if table ZTCOU102 has the data, display popup message
*     4. if choose 'Yes', display list
*----------------------------------------------------------------------*
FORM SELECT_REFRESH_DATA.
  DATA: L_CNT TYPE I,
        L_TEXT(70),
        L_ANSWER.

* UD1K941202 - by IG.MOON 8/3/2007 {
*  SELECT KALKA VER MATNR WERKS STAT AEDAT AENAM
*    INTO CORRESPONDING FIELDS OF TABLE GT_ZTCOU102
*    FROM ZTCOU102
*   WHERE KOKRS = P_KOKRS
*     AND BDATJ = P_YEAR
*     AND POPER = P_POPER
*     AND KALKA IN S_KALKA
*     AND VER   = P_VER
*     AND MATNR IN S_MATNR
*     AND LIFNR IN S_LIFNR
*     AND EKGRP IN S_EKGRP.
*

  SELECT COUNT( * ) INTO SY-INDEX
*    INTO CORRESPONDING FIELDS OF TABLE GT_ZTCOU102
    FROM ZTCOU102 AS A
    JOIN MARA AS B
      ON B~MATNR = A~MATNR
   WHERE A~KOKRS = P_KOKRS
     AND A~BDATJ = P_YEAR
     AND A~POPER = P_POPER
     AND A~KALKA IN S_KALKA
     AND VER = P_VER
     AND A~MATNR IN S_MATNR
     AND A~LIFNR IN S_LIFNR
     AND A~EKGRP IN S_EKGRP
     AND B~MATKL IN S_MATKL.
* }

*  CLEAR L_CNT.
*  LOOP AT GT_ZTCOU102 WHERE STAT <> 'O' AND STAT <> 'R'.
*    L_CNT = L_CNT + 1.
*    EXIT.
*  ENDLOOP.

  IF SY-INDEX > 0.                                          "L_CNT > 0.
    IF P_BATCH = 'X'.
      L_ANSWER = 'J'.
    ELSE.
      CLEAR: L_TEXT, L_ANSWER.
      PERFORM POP_UP USING
          'Raw material price data exists.'
          'Do you want to refresh?' 'X'
                     CHANGING L_ANSWER.
    ENDIF.

    IF L_ANSWER = 'J'.
* UD1K940725 by IG.MOON
*      IF P_LOGIC EQ SPACE.
*        PERFORM GET_GT_CKIS_OLD_LOGIC.
*        PERFORM GET_GT_OUT_FROM_GT_CKIS.
*      ELSE.

*---- get additional records; replace old data; except locked records
      PERFORM READ_ZTCOU102_FROM_DB CHANGING SY-SUBRC.

      PERFORM GET_GT_CKIS_NEW_LOGIC.
      PERFORM GET_GT_OUT_FROM_GT_CKIS_N.
      PERFORM GET_GT_OUT_OTHERS.
*      ENDIF.
* end of UD1K940725
    ELSEIF L_ANSWER = 'A'.
      G_ERROR = 'X'.
      EXIT.
    ELSE.
      PERFORM SELECT_FROM_ZTCOU102.
    ENDIF.
* If no data in table ZTCOU102, select data from table CKIS & KEKO
  ELSE.

* UD1K940725 by IG.MOON
*    IF P_LOGIC EQ SPACE.
*      PERFORM GET_GT_CKIS_OLD_LOGIC.
*      PERFORM GET_GT_OUT_FROM_GT_CKIS.
*    ELSE.
    PERFORM GET_GT_CKIS_NEW_LOGIC.
    PERFORM GET_GT_OUT_FROM_GT_CKIS_N.
    PERFORM GET_GT_OUT_OTHERS.
*    ENDIF.
* end of UD1K940725
  ENDIF.


ENDFORM.                    " SELECT_REFRESH_DATA
*&---------------------------------------------------------------------*
*&      Form  CHANGE_KZUST1
*&---------------------------------------------------------------------*
*       Check Reason Code
*----------------------------------------------------------------------*
FORM CHANGE_KZUST USING  RR_DATA_CHANGED TYPE REF TO
                             CL_ALV_CHANGED_DATA_PROTOCOL
                         LS_MOD_CELLS    TYPE LVC_S_MODI
                         P_TAB.

  DATA: LV_KZUST TYPE KZUST,
        LV_CHK,
        LV_WERTN TYPE CK_KWT,
        L_KZUST(3),
        L_MID,
        L_NAME(15).

  FIELD-SYMBOLS: <FS1> TYPE ANY,
                 <FS2> TYPE ANY.

  CLEAR L_NAME.

  IF P_TAB = 'GT_OUT'.
    CONCATENATE P_TAB '-WERTN' INTO L_NAME.
  ELSE.
    CONCATENATE P_TAB '-WERTN1' INTO L_NAME.
  ENDIF.

  ASSIGN (L_NAME) TO <FS1>.

  CONCATENATE P_TAB '-WERTN2' INTO L_NAME.
  ASSIGN (L_NAME) TO <FS2>.

  CLEAR: LV_KZUST, LV_CHK, LV_WERTN.
  LV_KZUST = LS_MOD_CELLS-VALUE.

  IF LS_MOD_CELLS-FIELDNAME = 'KZUST1_IN'.
    LV_WERTN = <FS1>.
  ELSEIF LS_MOD_CELLS-FIELDNAME = 'KZUST2_IN'.
    LV_WERTN = <FS2>.
  ENDIF.

  CLEAR: L_KZUST, L_MID.

  L_KZUST = LS_MOD_CELLS-VALUE.

  IF L_KZUST+0(1) = 'X'.
    IF P_TAB = 'GT_OUT'.
      GT_OUT-KZUST1_IN = GT_OUT-KZUST1 = L_KZUST.

    ELSE.
      IF LS_MOD_CELLS-FIELDNAME = 'KZUST1'.
        GT_OUT-KZUST1_IN = GT_DTL-KZUST1 = L_KZUST.
      ELSEIF LS_MOD_CELLS-FIELDNAME = 'KZUST2'.
        GT_OUT-KZUST2_IN = GT_DTL-KZUST2 = L_KZUST.
      ENDIF.
    ENDIF.

  ELSE.
    IF LV_WERTN > GT_OUT-PWERTN.
      L_MID = 'U'.
    ELSEIF LV_WERTN = GT_OUT-PWERTN.
      L_MID = 'E'.
    ELSEIF LV_WERTN < GT_OUT-PWERTN.
      L_MID = 'D'.
    ENDIF.

    IF P_TAB = 'GT_OUT'.
      GT_OUT-KZUST1_IN = L_KZUST.
      CONCATENATE L_KZUST+0(1) L_MID L_KZUST+1(1) INTO GT_OUT-KZUST1.

    ELSE.
      IF LS_MOD_CELLS-FIELDNAME = 'KZUST1_IN'.
        GT_DTL-KZUST1_IN = L_KZUST.
        CONCATENATE L_KZUST+0(1) L_MID L_KZUST+1(1) INTO GT_DTL-KZUST1.
      ELSEIF LS_MOD_CELLS-FIELDNAME = 'KZUST2_IN'.
        GT_DTL-KZUST2_IN = L_KZUST.
        CONCATENATE L_KZUST+0(1) L_MID L_KZUST+1(1) INTO GT_DTL-KZUST2.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.                    " CHANGE_KZUST
*&---------------------------------------------------------------------*
*&      Form  CALL_ZACOU112
*&---------------------------------------------------------------------*
*       Call Info History Report
*----------------------------------------------------------------------*
FORM CALL_ZACOU112.
  RANGES S_MATNR FOR ZVMM_INFORECORD-MATNR.
  RANGES S_MAKTG FOR MAKT-MAKTG.

  CLEAR: GT_ROW[], GT_ROID[].

  IF SY-DYNNR = 50.
    CALL METHOD G_GRID1->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = GT_ROW
        ET_ROW_NO     = GT_ROID.
  ELSEIF SY-DYNNR = 100.
    CALL METHOD G_GRID->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = GT_ROW
        ET_ROW_NO     = GT_ROID.
  ENDIF.

  REFRESH S_MATNR.
  CLEAR S_MATNR.

  IF SY-DYNNR = 50.
    LOOP AT GT_ROW INTO GS_ROW.
      READ TABLE GT_OUT INDEX GS_ROW-INDEX.

      IF SY-SUBRC = 0.
        IF OK_CODE = 'INFO'.
          S_MATNR-SIGN = 'I'.
          S_MATNR-OPTION = 'EQ'.
          S_MATNR-LOW = GT_OUT-MATNR.
          APPEND S_MATNR.  CLEAR S_MATNR.
        ELSE.
          S_MAKTG-SIGN = 'I'.
          S_MAKTG-OPTION = 'CP'.
          CONCATENATE GT_OUT-MAKTG '*' INTO S_MAKTG-LOW.
*       S_MAKTG-LOW = GT_OUT-MAKTG.
          APPEND S_MAKTG.  CLEAR S_MAKTG.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ELSEIF SY-DYNNR = 100.
    S_MATNR-SIGN = 'I'.
    S_MATNR-OPTION = 'EQ'.
    S_MATNR-LOW = GT_OUT-MATNR.
    APPEND S_MATNR.
    CLEAR S_MATNR.
  ENDIF.

  IF OK_CODE = 'INFO'.
    DESCRIBE TABLE S_MATNR LINES SY-TABIX.
  ELSE.
    DESCRIBE TABLE S_MAKTG LINES SY-TABIX.
  ENDIF.
  CHECK SY-TABIX > 0.

  IF OK_CODE = 'INFO'.
    SUBMIT ZACOU112 WITH S_MATNR IN S_MATNR AND RETURN.
  ELSE.
    SUBMIT ZACOU112 WITH S_MAKTG IN S_MAKTG AND RETURN.
  ENDIF.
ENDFORM.                    " CALL_ZACOU112
*&---------------------------------------------------------------------*
*&      Form  COPY_PRICE
*&---------------------------------------------------------------------*
*       Copy STD or MAP to info price
*----------------------------------------------------------------------*
FORM COPY_PRICE USING P_FNAME.
  FIELD-SYMBOLS <FS> TYPE ANY.

  CLEAR: GT_ROW[], GT_ROID[].

* Get selected rows
  CALL METHOD G_GRID1->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = GT_ROW
      ET_ROW_NO     = GT_ROID.

  LOOP AT GT_ROW INTO GS_ROW.
    READ TABLE GT_OUT INDEX GS_ROW-INDEX.

    IF SY-SUBRC = 0.
      ASSIGN (P_FNAME) TO <FS>.
      GT_OUT-WERTN = <FS>.

      PERFORM GET_ABP_LDC_GT_OUT.

*     UD1K941202 - by IG.MOON 8/2/2007 {
      PERFORM GT_OUT_STATUS_CHANGE.
*     }

      MODIFY GT_OUT INDEX GS_ROW-INDEX.
    ENDIF.
  ENDLOOP.

  PERFORM REFRESH_FIELD1.

ENDFORM.                    " COPY_PRICE
*&---------------------------------------------------------------------*
*&      Form  MASS_CHANGE
*&---------------------------------------------------------------------*
*       Mass Changing
*----------------------------------------------------------------------*
FORM MASS_CHANGE.
  CLEAR: EKGRP, BKLAS, PROFL, LIFNR, QTA.
  CALL SCREEN 200 STARTING AT 20 5 ENDING AT 65 15.

  CHECK OK_CODE NE 'EXIT'.
  CHECK NOT GV_EKGRP IS INITIAL OR
        NOT GV_PROFL IS INITIAL OR
        NOT GV_BKLAS IS INITIAL OR
        NOT GV_LIFNR IS INITIAL OR
        NOT GV_EKGRP IS INITIAL OR
        NOT GV_QTA IS INITIAL OR
        NOT GV_KZUST1 IS INITIAL OR
        NOT GV_PCT IS INITIAL.

* <<< ig.Moon 8/6/07>>>
  __POPUP '' 'Do you really want to adjust the changing?' ''.

  CLEAR: GT_ROW[], GT_ROID[].
* Get selected rows
  CALL METHOD G_GRID1->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = GT_ROW
      ET_ROW_NO     = GT_ROID.

  LOOP AT GT_ROW INTO GS_ROW.
    READ TABLE GT_OUT INDEX GS_ROW-INDEX.

    IF SY-SUBRC = 0.
      IF NOT GV_EKGRP IS INITIAL.
        GT_OUT-EKGRP  = GV_EKGRP.
      ENDIF.

      IF NOT GV_PROFL IS INITIAL.
        GT_OUT-PROFL  = GV_PROFL.
      ENDIF.

      IF NOT GV_BKLAS IS INITIAL.
        GT_OUT-BKLAS  = GV_BKLAS.
      ENDIF.

      IF NOT GV_LIFNR IS INITIAL.
        GT_OUT-LIFNR  = GV_LIFNR.
      ENDIF.

      IF NOT GV_QTA IS INITIAL.
        GT_OUT-QTA    = GV_QTA.
      ENDIF.

      IF NOT GV_KZUST1 IS INITIAL.
        GT_OUT-KZUST1 = GV_KZUST1.
      ENDIF.

      IF NOT GV_PCT IS INITIAL.
        GT_OUT-WERTN    = GT_OUT-WERTN * ( 1 + GV_PCT / 100 ).
        GT_OUT-WERTN_V1 = GT_OUT-WERTN_V1 * ( 1 + GV_PCT / 100 ).
        GT_OUT-WERTN_V2 = GT_OUT-WERTN_V2 * ( 1 + GV_PCT / 100 ).

* FIXME
        GT_OUT-WERTN1      = GT_OUT-PWERTN - GT_OUT-WERTN_V1.
        GT_OUT-WERTN1_V2   = GT_OUT-PWERTN - GT_OUT-WERTN_V2.
      ENDIF.

* UD1K941202 - by IG.MOON 8/2/2007 {
      PERFORM GT_OUT_STATUS_CHANGE.
* }

      MODIFY GT_OUT INDEX GS_ROW-INDEX.
    ENDIF.
  ENDLOOP.

  PERFORM REFRESH_FIELD1.

ENDFORM.                    " MASS_CHANGE
*&---------------------------------------------------------------------*
*&      Form  TRANSFER_PLAN_PRICE
*&---------------------------------------------------------------------*
*       Transfer to Plan price
*----------------------------------------------------------------------*
FORM TRANSFER_PLAN_PRICE.
  DATA: LV_ZPLP1 TYPE DZPLP1,    " Planed Price 1
        LV_ZPLP3 TYPE DZPLP3,    " Planed Price 3
        LV_CNT  TYPE I,
        LV_SCNT TYPE I,
        LV_STAT,
        L_BESKZ TYPE BESKZ,
        L_FLAG(1) TYPE C.

  CLEAR: GT_ROW[], GT_ROID[].

* Get selected rows
  CALL METHOD G_GRID1->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = GT_ROW
      ET_ROW_NO     = GT_ROID.

  LOOP AT GT_ROW INTO GS_ROW.
    READ TABLE GT_OUT INDEX GS_ROW-INDEX.

    IF SY-SUBRC = 0.
      IF GT_OUT-KALKA <> 'BP' AND GT_OUT-KALKA <> 'R1'.
        CONTINUE.
      ELSE.
        IF GT_OUT-KALKA = 'BP' AND P_VER <> 0.
          MESSAGE S000 WITH 'Check BP Version!'.
          EXIT.
        ELSE.
          CLEAR: LV_CNT, LV_SCNT, LV_STAT.

          SELECT COUNT(*) INTO LV_CNT
            FROM MBEW
            WHERE MATNR = GT_OUT-MATNR
              AND BWKEY IN R_BWKEY.

          IF LV_CNT = 1.
            CLEAR : L_BESKZ, L_FLAG.
            PERFORM CHECK_PLANT USING    GT_OUT-WERKS
                                CHANGING L_BESKZ L_FLAG.

            IF L_BESKZ = 'F' AND L_FLAG EQ ''.
              PERFORM CALL_BAPI_MATERIAL_SAVEDATA USING GT_OUT-WERKS.
              GT_OUT-RSTAT = GV_STAT.
            ENDIF.

          ELSEIF LV_CNT > 1.
            LOOP AT R_BWKEY.
              CLEAR: L_BESKZ, L_FLAG.
              PERFORM CHECK_PLANT USING    R_BWKEY-LOW
                                  CHANGING L_BESKZ L_FLAG.

              IF L_BESKZ = 'F' AND L_FLAG EQ ''.
                PERFORM CALL_BAPI_MATERIAL_SAVEDATA USING R_BWKEY-LOW.
                IF GV_STAT = 'X'.
                  LV_SCNT = LV_SCNT + 1.
                ENDIF.
              ENDIF.
            ENDLOOP.

            IF LV_SCNT = LV_CNT.
              GT_OUT-RSTAT = GV_STAT.
            ENDIF.

          ENDIF.
          CLEAR GT_OUT-MESS.
          IF GT_OUT-RSTAT = 'X'.
            GT_OUT-ICON = ICON_LED_GREEN.
            GT_OUT-MESS = 'Updated Sucessfully'.
          ELSE.
            GT_OUT-ICON = ICON_LED_RED.
            LOOP AT GT_RETURN WHERE TYPE EQ 'E'.
              CALL FUNCTION 'MESSAGE_TEXT_BUILD'
                EXPORTING
                  MSGID               = GT_RETURN-ID
                  MSGNR               = GT_RETURN-NUMBER
                  MSGV1               = GT_RETURN-MESSAGE_V1
                  MSGV2               = GT_RETURN-MESSAGE_V2
                  MSGV3               = GT_RETURN-MESSAGE_V3
                  MSGV4               = GT_RETURN-MESSAGE_V4
                IMPORTING
                  MESSAGE_TEXT_OUTPUT = GT_OUT-MESS.
              EXIT.
            ENDLOOP.
            IF    GT_OUT-MESS IS INITIAL.
              LOOP AT GT_RETURN WHERE TYPE EQ 'S' .
                CALL FUNCTION 'MESSAGE_TEXT_BUILD'
                  EXPORTING
                    MSGID               = GT_RETURN-ID
                    MSGNR               = GT_RETURN-NUMBER
                    MSGV1               = GT_RETURN-MESSAGE_V1
                    MSGV2               = GT_RETURN-MESSAGE_V2
                    MSGV3               = GT_RETURN-MESSAGE_V3
                    MSGV4               = GT_RETURN-MESSAGE_V4
                  IMPORTING
                    MESSAGE_TEXT_OUTPUT = GT_OUT-MESS.
                EXIT.
              ENDLOOP.
            ENDIF.
            IF    GT_OUT-MESS IS INITIAL.
              LOOP AT GT_RETURN .
                CALL FUNCTION 'MESSAGE_TEXT_BUILD'
                  EXPORTING
                    MSGID               = GT_RETURN-ID
                    MSGNR               = GT_RETURN-NUMBER
                    MSGV1               = GT_RETURN-MESSAGE_V1
                    MSGV2               = GT_RETURN-MESSAGE_V2
                    MSGV3               = GT_RETURN-MESSAGE_V3
                    MSGV4               = GT_RETURN-MESSAGE_V4
                  IMPORTING
                    MESSAGE_TEXT_OUTPUT = GT_OUT-MESS.
                EXIT.
              ENDLOOP.

            ENDIF.
*               gt_out-mess = 'Error'.
          ENDIF.

* UD1K941202 - by IG.MOON 8/2/2007 {

          PERFORM GT_OUT_STATUS_CHANGE.

*    MODIFY GT_OUT INDEX GS_ROW-INDEX TRANSPORTING RSTAT ICON MESS.
          MODIFY GT_OUT INDEX GS_ROW-INDEX.

* }


        ENDIF.
      ENDIF.

    ENDIF.
  ENDLOOP.

ENDFORM.                    " TRANSFER_PLAN_PRICE
*&---------------------------------------------------------------------*
*&      Form  CALL_BAPI_MATERIAL_SAVEDATA
*&---------------------------------------------------------------------*
FORM CALL_BAPI_MATERIAL_SAVEDATA USING    P_BWKEY TYPE BWKEY.
  CLEAR: GV_STAT, GS_HEAD.

  GS_HEAD-MATERIAL = GT_OUT-MATNR.

  CASE OK_CODE.
*   Change Plan Price
    WHEN 'PLAN'.
      PERFORM CHANGE_PRICE USING P_BWKEY.

*   Change Purchasing Group
    WHEN 'PURG'.
      PERFORM CHANGE_MRP1_VIEW.

*   Change Source
    WHEN 'SRC'.
      PERFORM CHANGE_BASIC_VIEW.
  ENDCASE.

  READ TABLE GT_RETURN WITH KEY TYPE = 'S'
                                ID = 'M3'
                                NUMBER = '801'.

  IF SY-SUBRC = 0.
    IF OK_CODE = 'PLAN'.
      GV_STAT = 'X'.
    ELSE.
      GV_SCNT = GV_SCNT + 1.
    ENDIF.
  ENDIF.

ENDFORM.                    " CALL_BAPI_MATERIAL_SAVEDATA
*&---------------------------------------------------------------------*
*&      Form  POPUP_FIELDNR
*&---------------------------------------------------------------------*
FORM POPUP_FIELDNR.
  DATA: BEGIN OF FIELDS_TAB OCCURS 1,
            NR(2) TYPE N,
            TEXT(60),
            COLOR(3),
         END OF FIELDS_TAB.

  DATA: BEGIN OF DYNPFIELDS OCCURS 3.
          INCLUDE STRUCTURE DYNPREAD.
  DATA: END OF DYNPFIELDS.

  DATA: DYNAME LIKE D020S-PROG,
        DYNUMB LIKE D020S-DNUM,
        EXC_EXCTAB     TYPE SLIS_T_EXTAB,
        POPUP_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
        F              TYPE SLIS_FIELDCAT_ALV,
        SELFIELD       TYPE SLIS_SELFIELD,
        EXITFIELD,
        COLOR_ACTIVE(3)  VALUE 'C50',
        TABIX LIKE SY-TABIX.

  DYNPFIELDS-FIELDNAME = 'P_CP'.

  APPEND DYNPFIELDS.
  DYNAME = SY-REPID.
  DYNUMB = SY-DYNNR.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      DYNAME             = DYNAME
      DYNUMB             = DYNUMB
      TRANSLATE_TO_UPPER = 'X'
    TABLES
      DYNPFIELDS         = DYNPFIELDS
    EXCEPTIONS
      OTHERS             = 9.

  CLEAR F.
  F-REPTEXT_DDIC  = 'NO'.
  F-FIELDNAME = 'NR'.
  F-OUTPUTLEN = 2.
  APPEND F TO POPUP_FIELDCAT.
  CLEAR F.

  F-REPTEXT_DDIC = 'Name'.
  F-FIELDNAME = 'TEXT'.

  DESCRIBE FIELD FIELDS_TAB-TEXT LENGTH F-OUTPUTLEN.
  APPEND F TO POPUP_FIELDCAT.

* Excluding-Table
  APPEND: '%SC ' TO EXC_EXCTAB,       " Search
          '%SC+' TO EXC_EXCTAB,       " Search+
          '&OUP' TO EXC_EXCTAB,       " Sort Up
          '&ODN' TO EXC_EXCTAB,       " Sort Dn
          '&ILT' TO EXC_EXCTAB,       " Filter
          '&OL0' TO EXC_EXCTAB.

* Popup
  TABIX = SY-TABIX.

  FIELDS_TAB-NR = '00'.
  FIELDS_TAB-TEXT = '< no comparison value >'.
  APPEND FIELDS_TAB.

  FIELDS_TAB-NR = '01'.
  FIELDS_TAB-TEXT = TEXT-005.
  APPEND FIELDS_TAB.

  FIELDS_TAB-NR = '02'.
  FIELDS_TAB-TEXT = TEXT-006.
  APPEND FIELDS_TAB.

  FIELDS_TAB-NR = '03'.
  FIELDS_TAB-TEXT = TEXT-007.
  APPEND FIELDS_TAB.
  CLEAR FIELDS_TAB.

  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      I_TITLE                 = TEXT-004
      I_LINEMARK_FIELDNAME    = 'COLOR'
      I_TABNAME               = 'FIELDNR_TAB'
      IT_FIELDCAT             = POPUP_FIELDCAT
      I_CALLBACK_USER_COMMAND = 'USER_COMMAND_POPUP_LIGHTS_N'
      I_CALLBACK_PROGRAM      = DYNAME
      IT_EXCLUDING            = EXC_EXCTAB
    IMPORTING
      ES_SELFIELD             = SELFIELD
      E_EXIT                  = EXITFIELD
    TABLES
      T_OUTTAB                = FIELDS_TAB.

  READ TABLE FIELDS_TAB INDEX TABIX.
  CLEAR FIELDS_TAB-COLOR.
  MODIFY FIELDS_TAB INDEX TABIX.

  IF EXITFIELD IS INITIAL.
    READ TABLE FIELDS_TAB INDEX SELFIELD-TABINDEX.
    P_CP = FIELDS_TAB-NR.
    T_LIGHTS = FIELDS_TAB-TEXT.

    DYNPFIELDS-FIELDNAME = 'P_CP'.
    DYNPFIELDS-FIELDVALUE = FIELDS_TAB-NR.
    APPEND DYNPFIELDS.

    DYNPFIELDS-FIELDNAME = 'T_LIGHTS'.
    DYNPFIELDS-FIELDVALUE = T_LIGHTS.
    APPEND DYNPFIELDS.

    DYNAME = SY-REPID.
    DYNUMB = SY-DYNNR.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        DYNAME     = DYNAME
        DYNUMB     = DYNUMB
      TABLES
        DYNPFIELDS = DYNPFIELDS.

  ENDIF.

ENDFORM.                    " POPUP_FIELDNR
*&---------------------------------------------------------------------*
*&      Form  GET_ESTAT
*&---------------------------------------------------------------------*
FORM GET_ESTAT.
  DATA: LV_AMT  TYPE CK_KWT,
        LV_ABS  TYPE CK_KWT.

  CHECK NOT P_CP IS INITIAL OR P_CP <> 0.
  CLEAR: LV_AMT, LV_ABS.

* Case by Comparable Value
* 01: Variance between cost estimates and standard price
*  : Diff.Price = current price - standard price
*    Diff.Rate = (current price - standard price)/standard price
* 02: Variance between cost estimates and moving avg. price
*  : Diff.Price = current price - moving avg. price
*    Diff.Rate = (current price - moving avg. price)/moving avg. price
* 03: Variance between costing runs
*  : Diff.Price = current price - previous price
*    Diff.Rate = (current price - previous price)/previous price
  CASE P_CP.
    WHEN '01'.
      IF NOT GT_OUT-STPRS IS INITIAL.
        LV_AMT = GT_OUT-STPRS.
      ENDIF.

    WHEN '02'.
      IF NOT GT_OUT-VERPR IS INITIAL.
        LV_AMT = GT_OUT-VERPR.
      ENDIF.

    WHEN '03'.
      IF NOT GT_OUT-PWERTN IS INITIAL.
        LV_AMT = GT_OUT-PWERTN.
      ENDIF.
  ENDCASE.

  GT_OUT-AMT_DIF = GT_OUT-WERTN - LV_AMT.
  LV_ABS = ABS( GT_OUT-AMT_DIF ).

  IF GT_OUT-AMT_DIF <> 0 AND GT_OUT-STPRS <> 0.
    IF LV_AMT = 0.
      GT_OUT-RATE_DIF = 999.
    ELSE.
      GT_OUT-RATE_DIF = GT_OUT-AMT_DIF / LV_AMT * 100.
    ENDIF.
  ENDIF.

  IF P_ABS = 'X'.
    IF LV_ABS > P_RED OR GT_OUT-RATE_DIF > P_REDP.
      GT_OUT-ESTAT = ICON_LED_RED.
    ELSEIF ( LV_ABS <= P_RED AND LV_ABS > P_YELL ) OR
           ( GT_OUT-RATE_DIF <= P_REDP AND GT_OUT-RATE_DIF > P_REDP ).
      GT_OUT-ESTAT = ICON_LED_YELLOW.
    ELSE.
      GT_OUT-ESTAT = ICON_LED_GREEN.
    ENDIF.

  ELSE.
    IF GT_OUT-AMT_DIF > P_RED OR
      GT_OUT-RATE_DIF > P_REDP.
      GT_OUT-ESTAT = ICON_LED_RED.
    ELSEIF ( GT_OUT-AMT_DIF <= P_RED AND GT_OUT-AMT_DIF > P_YELL ) OR
           ( GT_OUT-RATE_DIF <= P_REDP AND GT_OUT-RATE_DIF > P_REDP ).
      GT_OUT-ESTAT = ICON_LED_YELLOW.
    ELSE.
      GT_OUT-ESTAT = ICON_LED_GREEN.
    ENDIF.

  ENDIF.

ENDFORM.                    " GET_ESTAT
*&---------------------------------------------------------------------*
*&      Form  CHANGE_WERTN
*&---------------------------------------------------------------------*
*       Check Price
*----------------------------------------------------------------------*
FORM CHANGE_WERTN USING RR_DATA_CHANGED TYPE REF TO
                                        CL_ALV_CHANGED_DATA_PROTOCOL
                        LS_MOD_CELLS    TYPE LVC_S_MODI
                        P_TAB
                        P_WERTN TYPE ZWERTN.

  DATA L_NAME(15).
  FIELD-SYMBOLS: <FS>  TYPE ANY,
                 <FS1> TYPE ANY.

  CLEAR L_NAME.
  CONCATENATE P_TAB '-WERTN' INTO L_NAME.
  ASSIGN (L_NAME) TO <FS>.

  CLEAR L_NAME.
  CONCATENATE P_TAB '-WERTN1' INTO L_NAME.
  ASSIGN (L_NAME) TO <FS1>.

  IF GT_OUT-KALKA+0(1) = 'U' OR GT_OUT-KALKA = 'M1'.
    IF LS_MOD_CELLS-VALUE IS INITIAL.
      PERFORM DATA_INPUT_ERROR USING RR_DATA_CHANGED
                                     LS_MOD_CELLS
                                     'E'
                                     'Check Price! '
                                     LS_MOD_CELLS-FIELDNAME.
    ELSE.
      <FS> = LS_MOD_CELLS-VALUE.
    ENDIF.
  ELSE.
    <FS> = LS_MOD_CELLS-VALUE.
  ENDIF.

* RS1 price : Info.price - Prv.Info.price
  <FS1> = LS_MOD_CELLS-VALUE - GT_OUT-PWERTN.

  IF P_LDC = 'X'.
    PERFORM GET_ABP_LDC_GT_OUT.
  ELSE.
    PERFORM GET_RATE USING P_WERTN.
    PERFORM CAL_PRICE.
  ENDIF.

ENDFORM.                    " CHANGE_WERTN
*&---------------------------------------------------------------------*
*&      Form  COPY_VER
*&---------------------------------------------------------------------*
*&      Form  UPLOAD
*&---------------------------------------------------------------------*
FORM UPLOAD.
  DATA: LT_INTERN   TYPE TABLE OF KCDE_CELLS WITH HEADER LINE,
        LT_ZTCOU102 TYPE TABLE OF ZTCOU102 WITH HEADER LINE.

  DATA: L_CNT TYPE I,
        L_FNAME(60).

  FIELD-SYMBOLS : <FS> TYPE ANY.

* Get ZTCOU102 Table Fields
  PERFORM GET_GT_DD03L.

* Get File name
  PERFORM GET_FILENAME.

* Upload
  CLEAR: LT_INTERN, LT_ZTCOU102.
  REFRESH: LT_INTERN, LT_ZTCOU102.

  CALL FUNCTION 'KCD_EXCEL_OLE_TO_INT_CONVERT'
    EXPORTING
      FILENAME                = GV_FILE
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 1
      I_END_COL               = 53
      I_END_ROW               = 65535
    TABLES
      INTERN                  = LT_INTERN
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.

  SORT LT_INTERN BY ROW COL.

* If existing same end item,
*  overwrite Internal Table GT_OUT by uploaded data
* else,
*  append uploaded data + material info & eror category
*  to Internal Table GT_OUT

* ...Create Internal Table LT_ZTCOU102 like Table ZTCOU102
  LOOP AT LT_INTERN.
    CHECK LT_INTERN-ROW > 1.    " <-- Except title line
*   New line
    AT NEW ROW.
      CLEAR LT_ZTCOU102.
    ENDAT.

*   Set Value
    L_CNT = LT_INTERN-COL + 1.
    READ TABLE GT_DD03L WITH KEY POSITION = L_CNT.

    IF SY-SUBRC = 0.
      CASE GT_DD03L-FIELDNAME.
        WHEN 'INFNR'.
          PERFORM CONVERSION_INPUT USING    LT_INTERN-VALUE
                                   CHANGING LT_ZTCOU102-INFNR.

        WHEN 'LIFNR'.
          LT_ZTCOU102-LIFNR = LT_INTERN-VALUE.
          PERFORM CONVERSION_INPUT USING    LT_INTERN-VALUE
                                   CHANGING LT_ZTCOU102-LIFNR.
        WHEN 'LIFNR2'.
          LT_ZTCOU102-LIFNR2 = LT_INTERN-VALUE.
          PERFORM CONVERSION_INPUT USING    LT_INTERN-VALUE
                                   CHANGING LT_ZTCOU102-LIFNR2.

        WHEN OTHERS.
          CLEAR L_FNAME.

          CONCATENATE 'LT_ZTCOU102-' GT_DD03L-FIELDNAME
                 INTO L_FNAME.

          ASSIGN (L_FNAME) TO <FS>.
          <FS> = LT_INTERN-VALUE.
      ENDCASE.
    ENDIF.

    AT END OF ROW.
      APPEND LT_ZTCOU102.
      CLEAR  LT_ZTCOU102.
    ENDAT.

    CLEAR LT_INTERN.
  ENDLOOP.

* ...Append uploaded data to Internal Table GT_OUT
  PERFORM APPEND_GT_OUT TABLES LT_ZTCOU102.

  PERFORM REFRESH_FIELD1.

ENDFORM.                    " UPLOAD
*&---------------------------------------------------------------------*
*&      Form  GET_FILENAME
*&---------------------------------------------------------------------*
*       Get File Name
*----------------------------------------------------------------------*
FORM GET_FILENAME.
  CLEAR GV_FILE.
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      MASK             = ',*.XLS,*.XLS.'
      MODE             = 'O'
      TITLE            = 'Select file'
    IMPORTING
      FILENAME         = GV_FILE
    EXCEPTIONS
      INV_WINSYS       = 1
      NO_BATCH         = 2
      SELECTION_CANCEL = 3
      SELECTION_ERROR  = 4
      OTHERS           = 5.

  IF GV_FILE NA '.XLS'.
    CONCATENATE GV_FILE '.XLS' INTO GV_FILE.
  ENDIF.

ENDFORM.                    " GET_FILENAME
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD
*&---------------------------------------------------------------------*
FORM DOWNLOAD.
  CLEAR: GT_DOWN, GT_FIELDNAMES.
  REFRESH: GT_DOWN, GT_FIELDNAMES.

* Get File name
  PERFORM GET_FILENAME.

* Get header of excel file
  PERFORM GET_FIELDNAME.

  LOOP AT GT_OUT.
    MOVE-CORRESPONDING GT_OUT TO GT_DOWN.

    PERFORM CONVERSION_OUTPUT USING GT_OUT-INFNR
                              CHANGING GT_DOWN-INFNR.

    PERFORM CONVERSION_OUTPUT USING GT_OUT-LIFNR
                              CHANGING GT_DOWN-LIFNR.

    PERFORM CONVERSION_OUTPUT USING GT_OUT-LIFNR2
                              CHANGING GT_DOWN-LIFNR2.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        INPUT          = GT_OUT-PMEHT
        LANGUAGE       = SY-LANGU
      IMPORTING
        OUTPUT         = GT_DOWN-PMEHT
      EXCEPTIONS
        UNIT_NOT_FOUND = 1
        OTHERS         = 2.

    CASE GT_OUT-ESTAT.
      WHEN ICON_LED_GREEN.
        GT_DOWN-ESTAT = 'G'.
      WHEN ICON_LED_YELLOW.
        GT_DOWN-ESTAT = 'Y'.
      WHEN ICON_LED_RED.
        GT_DOWN-ESTAT = 'R'.
    ENDCASE.

    APPEND GT_DOWN.
    CLEAR GT_DOWN.
  ENDLOOP.

  CALL FUNCTION 'WS_DOWNLOAD'
    EXPORTING
      FILENAME                = GV_FILE
      FILETYPE                = 'DAT'
    TABLES
      DATA_TAB                = GT_DOWN
      FIELDNAMES              = GT_FIELDNAMES
    EXCEPTIONS
      FILE_OPEN_ERROR         = 1
      FILE_WRITE_ERROR        = 2
      INVALID_FILESIZE        = 3
      INVALID_TYPE            = 4
      NO_BATCH                = 5
      UNKNOWN_ERROR           = 6
      INVALID_TABLE_WIDTH     = 7
      GUI_REFUSE_FILETRANSFER = 8
      CUSTOMER_ERROR          = 9
      OTHERS                  = 10.

  IF SY-SUBRC = 0.
    MESSAGE S000 WITH 'Downloaded successfully.'.
  ELSE.
    MESSAGE S000 WITH 'Failed download.'.
  ENDIF.

ENDFORM.                    " DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  GET_FIELDNAME
*&---------------------------------------------------------------------*
FORM GET_FIELDNAME.
  PERFORM APPEND_FIELDNAME USING: 'Controlling Area',
                                  'Year',
                                  'Period',
                                  'CstType',
                                  'Ver',
                                  'Material',
                                  'Plant',
                                  'MType',
                                  'Valuation date',
                                  'Costing date',
                                  'Qty struc.date',
                                  'Pur.GRP',
                                  'Src',
                                  'VClass',
                                  'Info record #',
                                  'Commodity code',
                                  'MAP',
                                  'STD',
                                  'Info-Price',
                                  'Per',
                                  'UoM',
                                  'Prv.Info$',
                                  'Vendor1',
                                  'Quota-Vendor1',
                                  'Info$-Vendor1',
                                  'Duty',
                                  'Freight',
                                  'Other',
                                  'RSN1-Vendor1',
                                  'RS1$-Vendor1',
                                  'RSN2-Vendor1',
                                  'RS2$-Vendor1',
                                  'Vendor2',
                                  'Quota-Vendor2',
                                  'Info$-Vendor2',
                                  'RSN1-Vendor2',
                                  'RSN1$-Vendor2',
                                  'RSN2-Vendor2',
                                  'RSN2$-Vendor2',
                                  'G',
                                  'A',
                                  'S',
                                  'Q',
                                  'V',
                                  'P',
                                  'R',
                                  'Status',
                                  'Changed by',
                                  'Changed on',
                                  'Description',
                                  'Info-price Diff.'
                                                    ,
                                  'Diff$',
                                  'Diff%',
                                  'Exception',
                                  'Grs.Prc$'.

ENDFORM.                    " GET_FIELDNAME
*&---------------------------------------------------------------------*
*&      Form  GET_PLANT
*&---------------------------------------------------------------------*
*       Get Plant
*----------------------------------------------------------------------*
FORM GET_PLANT.
  CLEAR  : GT_PLANT, R_BWKEY.
  REFRESH: GT_PLANT, R_BWKEY.

* Get Company Code
  SELECT SINGLE BUKRS INTO GV_BUKRS
    FROM TKA02
   WHERE KOKRS = P_KOKRS.

* Get plant
  SELECT BWKEY INTO TABLE GT_PLANT
    FROM T001K
   WHERE BUKRS = P_KOKRS.

  LOOP AT GT_PLANT.
    R_BWKEY-SIGN = 'I'.
    R_BWKEY-OPTION = 'EQ'.
    R_BWKEY-LOW = GT_PLANT-BWKEY.

    APPEND R_BWKEY.
    CLEAR R_BWKEY.
  ENDLOOP.

ENDFORM.                    " GET_PLANT
*&---------------------------------------------------------------------*
*&      Form  RECOSTING
*&---------------------------------------------------------------------*
*       Recosting : 1. Change valuation date
*                   2. Execute CK11: Create Cost Estimates for Material
*                   3. Reselect Data from KEKO/CKIS
*                   4. Update table ZTCOU102
*----------------------------------------------------------------------*
FORM RECOSTING.
  DATA: L_CNT  TYPE I,
        L_RC   TYPE SYSUBRC.

* UD1K940931 by IG.MOON
  PERFORM GET_GV_BWDAT.
*
  CLEAR: GT_ROW[], GT_ROID[].


* Get selected rows
  CALL METHOD G_GRID1->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = GT_ROW
      ET_ROW_NO     = GT_ROID.

*     1. Change valuation date
  GV_BWDAT_IN = GV_BWDAT.
  CALL SCREEN 400 STARTING AT 40 8 ENDING AT 74 10.

  IF GV_CANC = 'X'.
    GT_OUT-CANC = 'X'.
*    MODIFY GT_OUT INDEX GS_ROW-INDEX TRANSPORTING CANC.
    MESSAGE S000 WITH 'Canceled Recosting.'.

  ELSE.

    LOOP AT GT_ROW INTO GS_ROW.
      READ TABLE GT_OUT INDEX GS_ROW-INDEX.
      CHECK GT_OUT-ZLOCK NE 'X'.

      IF SY-SUBRC = 0.
*     2. Execute CK11: Create Cost Estimates for Material
        PERFORM BDC_CK11 CHANGING L_RC.

        IF L_RC = 0.
*     3. Reselect Data from KEKO/CKIS
          PERFORM RESELECT_COSTING_INFO.
          MODIFY GT_OUT INDEX GS_ROW-INDEX.
        ENDIF.
*     4. Update table ZTCOU102
        PERFORM SAVE_ZTCOU102_BY_RECOSTING CHANGING L_CNT.
      ENDIF.

    ENDLOOP.

    IF L_CNT > 0.
      MESSAGE S000 WITH 'You have saved ; ' L_CNT 'records.'.
    ENDIF.

    PERFORM REFRESH_FIELD1.

  ENDIF.

ENDFORM.                    " RECOSTING
*&---------------------------------------------------------------------*
*&      Form  BDC_CK11
*&---------------------------------------------------------------------*
*       Execute BDC Transaction: Create Cost Estimates for Material
*----------------------------------------------------------------------*
FORM BDC_CK11 CHANGING P_RC TYPE SYSUBRC.
  DATA: L_DAT1(10) TYPE C,
        L_DAT2(10) TYPE C,
        L_DAT3(10) TYPE C.

  REFRESH: GT_BDC, GT_MSG.
  CLEAR  : GT_BDC, GT_MSG.

* Get BDC Option
  PERFORM GET_OPT USING 'N'.

* Convert Date to External
  PERFORM CONVERT_DATE USING: GV_CSTDT     L_DAT1,
                              GV_BWDAT_IN  L_DAT2,
                              GV_BWDAT_IN  L_DAT3.

* Costing Variant
  PERFORM GET_KLVAR.

* Create Cost Estimates for Material
  PERFORM DYNPRO USING:
     'X'  'SAPLCKDI'        '0111',
     ' '  'CKI64A-KLVAR'    GV_KLVAR,         " Costing Version
     ' '  'CKI64A-MATNR'    GT_OUT-MATNR,     " Material
     ' '  'CKI64A-WERKS'    GT_OUT-WERKS,     " Plant
     ' '  'BDC_OKCODE'      '/00',            " [Enter]

* Date
     'X'  'SAPLCKDI'        '0400',
     ' '  'CKI64A-KADAT'    L_DAT1,           " Costing date from
     ' '  'CKI64A-BIDAT'    L_DAT1,           " Costing date to
     ' '  'CKI64A-ALDAT'    L_DAT2,           " Qty structure date
     ' '  'CKI64A-BWDAT'    L_DAT3,           " Valuation date
     ' '  'BDC_OKCODE'       '=ENTR',         " [Enter]

     'X'  'SAPLCKDI'        '2100',
     ' '  'BDC_OKCODE'       '=BUCA'.         " [Save]

  CALL TRANSACTION 'CK11'   USING         GT_BDC
                            OPTIONS FROM  GS_OPT
                            MESSAGES INTO GT_MSG.

  READ TABLE GT_MSG WITH KEY MSGTYP = 'S'
                             MSGID = 'CK'
                             MSGNR = '039'.

  P_RC = SY-SUBRC.

ENDFORM.                                                    " BDC_CK11
*&---------------------------------------------------------------------*
*&      Form  GET_KLVAR
*&---------------------------------------------------------------------*
*&      Form  RESELECT_COSTING_INFO
*&---------------------------------------------------------------------*
* ANDY
* TOO MANY BUG
* FIX ME PLEASE
FORM RESELECT_COSTING_INFO.

  DATA: L_KNUMH TYPE KNUMH,       " Condition record No.
        L_HRKFT TYPE HRKFT,       " Origin Group
        L_DUTY  TYPE CK_KWT,      " Duty
        L_FRG   TYPE CK_KWT,      " Freight
        L_OTH   TYPE CK_KWT,      " Others
        L_WERTN TYPE CK_KWT,      " Value in controlling area currency
        LT_NEW  TYPE TABLE OF TY_CKIS WITH HEADER LINE.

  DATA FLAG(1).

  CLEAR: L_KNUMH, L_HRKFT, L_WERTN, LT_NEW.
  REFRESH LT_NEW.

  SELECT A~KALNR A~KALKA A~KADKY A~MATNR A~KOKRS A~WERKS
         A~BDATJ A~POPER A~BWDAT A~ALDAT
         B~HRKFT B~PMEHT
         B~MENGE B~WERTN B~INFNR B~LIFNR
         B~STRAT B~SUBSTRAT B~ELEMT B~ELEMTNS
    INTO CORRESPONDING FIELDS OF TABLE LT_NEW
    FROM KEKO AS A
    JOIN CKIS AS B
      ON B~BZOBJ = A~BZOBJ
     AND B~KALNR = A~KALNR
     AND B~KALKA = A~KALKA
     AND B~KADKY = A~KADKY
     AND B~TVERS = A~TVERS
     AND B~BWVAR = A~BWVAR
     AND B~KKZMA = A~KKZMA
   WHERE B~LEDNR = C_LEDNR
     AND A~MATNR = GT_OUT-MATNR
     AND A~WERKS = GT_OUT-WERKS
     AND A~BDATJ = P_YEAR
     AND A~POPER = P_POPER
     AND A~KALKA = GT_OUT-KALKA
     AND A~TVERS = GC_TVERS
     AND B~HRKFT EQ SPACE     .

*- U1 Start
  IF P_ARCH EQ 'X'.
    PERFORM ARCHIVE_READ_CKIS TABLES LT_NEW.
  ENDIF.
*- U1 End

*///
  IF SY-SUBRC = 0.
    LOOP AT LT_NEW.
      GT_OUT-KALKA  = LT_NEW-KALKA.            " Costing type
      GT_OUT-KADKY  = LT_NEW-KADKY.            " Costing date
      GT_OUT-WERKS  = LT_NEW-WERKS.            " Plant
      GT_OUT-KOKRS  = LT_NEW-KOKRS.            " Controling Area
      GT_OUT-LIFNR  = LT_NEW-LIFNR.            " Vendor
      GT_OUT-INFNR  = LT_NEW-INFNR.            " Inforecord No.
      GT_OUT-LAND1  = LT_NEW-LAND1.            " Country key
      GT_OUT-NAME1  = LT_NEW-NAME1.            " Vendor name
      GT_OUT-PMEHT  = LT_NEW-PMEHT.            " uOm


      GT_OUT-STRAT  = LT_NEW-STRAT.
      GT_OUT-SUBSTRAT  = LT_NEW-SUBSTRAT.

      L_WERTN = L_WERTN + LT_NEW-WERTN .    " Info-Price
      AT LAST.
        FLAG = 'X'.
      ENDAT.

      CHECK FLAG EQ 'X'.

*       if costing type is MI, clear duty, freight, other
      IF GT_OUT-KALKA = 'M1'.
        CLEAR: GT_OUT-DUTY, GT_OUT-FRG, GT_OUT-OTH.
        GT_OUT-WERTN    = L_WERTN.     " Info-Price
        GT_OUT-WERTN_V1 = L_WERTN.     " Info-Price of 1st vendor
      ELSE.
        GT_OUT-DUTY = L_DUTY.          " Duty
        GT_OUT-FRG  = L_FRG.           " Freight
        GT_OUT-OTH  = L_OTH.           " Others
        GT_OUT-WERTN = L_WERTN.        " Info-Price
        GT_OUT-WERTN_V1 = L_WERTN.     " Info-Price of 1st vendor
      ENDIF.

      PERFORM GT_OUT_STATUS_CHANGE.

      GT_OUT-AENAM = SY-UNAME.   " Changed by
      GT_OUT-AEDAT = SY-DATUM.   " Changed on
      GT_OUT-CPUTM = SY-UZEIT.
      GT_OUT-STAT  = 'C'.        " Status
      GT_OUT-PRICE_MANUAL = SPACE.
    ENDLOOP.

*   Refresh of ABP Case
*   : if vendor is not local,
*     change freight & other by table ZTCOU116
    PERFORM GET_ABP_LDC_GT_OUT.

*       Error Category
    PERFORM GET_ERR_CATEGORY USING GT_OUT
                          CHANGING GT_OUT-EC_G
                                   GT_OUT-EC_A
                                   GT_OUT-EC_S
                                   GT_OUT-EC_V
                                   GT_OUT-EC_P
                                   GT_OUT-EC_R.

  ENDIF.

ENDFORM.                    " RESELECT_COSTING_INFO
*&---------------------------------------------------------------------*
*&      Form  COPY_VER
*&---------------------------------------------------------------------*
*       Copy Version : ABP only
*----------------------------------------------------------------------*
FORM COPY_VER.
  CLEAR: VER, GV_VER.
  CALL SCREEN 300 STARTING AT 20 5 ENDING AT 65 10.

  CHECK NOT GV_VER IS INITIAL.
  CLEAR: GT_ROW[], GT_ROID[].

* Get selected rows
  CALL METHOD G_GRID1->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = GT_ROW
      ET_ROW_NO     = GT_ROID.

  LOOP AT GT_ROW INTO GS_ROW.
    READ TABLE GT_OUT INDEX GS_ROW-INDEX.

    IF SY-SUBRC = 0 AND GT_OUT-KALKA = 'BP'.
      GT_OUT-VER = GV_VER.
*     UD1K941202 - by IG.MOON 8/2/2007 {
      PERFORM GT_OUT_STATUS_CHANGE.
*     }
      MODIFY GT_OUT INDEX GS_ROW-INDEX.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " COPY_VER
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0300 INPUT.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.

  CASE OK_CODE.
    WHEN 'ENTR'.
      GV_VER = VER.
    WHEN  'EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*&      Form  GET_ERROR_ACCOUNT
*&---------------------------------------------------------------------*
*       Get Error Category-Account
*----------------------------------------------------------------------*
*  : 1. in case of different mathcing(Src vs Val.Class),
*       Error Category-Account : 'X'
*       ------------------------
*       Src      Valuation Class
*       ------------------------
*       M        7900
*       K        3000
*       ------------------------
*    2. if country key of vendor is different country key of company,
*       Error Category-Account : 'X'
*----------------------------------------------------------------------*
FORM GET_ERROR_ACCOUNT USING    P_TAB STRUCTURE GT_OUT
                       CHANGING P_EC_A.
  DATA: LV_FLAG,
        LV_BLAND TYPE LAND1.

* 1. in case of different mathcing(Src vs Val.Class),
*    Error Category-Account : 'X'
  CLEAR LV_FLAG.

  CASE P_TAB-PROFL.
    WHEN 'M'.
      IF P_TAB-BKLAS = '7900'.
        LV_FLAG = 'T'.
      ENDIF.
    WHEN 'K'.
      IF P_TAB-BKLAS = '3000'.
        LV_FLAG = 'T'.
      ENDIF.
    WHEN OTHERS.
      IF NOT P_TAB-BKLAS IS INITIAL.
        LV_FLAG = 'T'.
      ENDIF.
  ENDCASE.

  IF LV_FLAG = SPACE.
    P_EC_A = 'X'.
  ENDIF.

* 2. if country key of vendor is different country key of company,
*    Error Category-Account : 'X'
  CLEAR LV_BLAND.

  READ TABLE GT_LDC WITH KEY LAND1 = P_TAB-LAND1.
  IF SY-SUBRC = 0.
    IF P_TAB-BKLAS = C_KD_BKLAS.
    ELSE.
      P_EC_A = 'X'.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_ERROR_ACCOUNT
*&---------------------------------------------------------------------*
*&      Form  CONVERT_DATE
*&---------------------------------------------------------------------*
*       Convert Date to External
*----------------------------------------------------------------------*
*      -->P_INDATE  Internal Date
*      -->P_EXDATE  External Date
*----------------------------------------------------------------------*
FORM CONVERT_DATE USING P_INDATE TYPE SYDATUM
                        P_EXDATE TYPE CHAR10.

  CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
    EXPORTING
      DATE_INTERNAL = P_INDATE
    IMPORTING
      DATE_EXTERNAL = P_EXDATE.

ENDFORM.                    " CONVERT_DATE
*&---------------------------------------------------------------------*
*&      Form  APPEND_FIELDNAME
*&---------------------------------------------------------------------*
*       Appedn Header Column for Download
*----------------------------------------------------------------------*
FORM APPEND_FIELDNAME USING P_NAME.
  GT_FIELDNAMES-NAME = P_NAME.
  APPEND GT_FIELDNAMES.

ENDFORM.                    " APPEND_FIELDNAME
*&---------------------------------------------------------------------*
*&      Form  append_gt_mat_temp
*&---------------------------------------------------------------------*
*       Create internal table for MAP, STD
*----------------------------------------------------------------------*
*FORM APPEND_GT_MAT_TEMP.
*  GT_MAT_TEMP-MATNR = GT_OUT-MATNR.
*  GT_MAT_TEMP-BWKEY = GT_OUT-WERKS.
*  APPEND GT_MAT_TEMP.
*  CLEAR GT_MAT_TEMP.
*
*ENDFORM.                    " append_gt_mat_temp
*&---------------------------------------------------------------------*
*&      Form  APPLY_LDC_RATE_GT_OUT
*&---------------------------------------------------------------------*
*   1. Unit / Standard Case: if cheked [Use ABP LDC Rate%]
*   2. ABP Case
*   if vendor is not local, change freight & other
*   by table ZTCOU116
*----------------------------------------------------------------------*
*FORM APPLY_LDC_RATE_GT_OUT.
*
*  CHECK P_LDC = 'X'.
*
**  DATA: $IX TYPE I.
**  LOOP AT GT_OUT.
*  CHECK GT_OUT-ZLOCK = SPACE.
**    $IX = SY-TABIX.
*
*  PERFORM GET_ABP_LDC_GT_OUT.
*
*  GT_OUT-GPRC = GT_OUT-WERTN + GT_OUT-DUTY
*                    + GT_OUT-FRG + GT_OUT-OTH.
*
**    MODIFY GT_OUT INDEX $IX transporting GPRC DUTY FRG OTH.
**  ENDLOOP.
**  ENDIF.
*
*ENDFORM.                    " APPLY_LDC_RATE_GT_OUT
*&---------------------------------------------------------------------*
*&      Form  GET_GT_DD03L
*&---------------------------------------------------------------------*
*       Get Table Fields of ZTCOU102
*----------------------------------------------------------------------*
FORM GET_GT_DD03L.
  CLEAR GT_DD03L.
  REFRESH GT_DD03L.

  SELECT FIELDNAME POSITION
    INTO TABLE GT_DD03L
    FROM DD03L
   WHERE TABNAME = 'ZTCOU102'.

  DELETE GT_DD03L WHERE FIELDNAME = 'MANDT'.
  SORT GT_DD03L BY POSITION.

ENDFORM.                    " GET_GT_DD03L
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_INPUT
*&---------------------------------------------------------------------*
FORM CONVERSION_INPUT USING    P_INPUT
                     CHANGING P_OUTPUT.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = P_INPUT
    IMPORTING
      OUTPUT = P_OUTPUT.

ENDFORM.                    " CONVERSION_INPUT
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_OUTPUT
*&---------------------------------------------------------------------*
FORM CONVERSION_OUTPUT USING    P_INPUT
                      CHANGING P_OUTPUT.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      INPUT  = P_INPUT
    IMPORTING
      OUTPUT = P_OUTPUT.

ENDFORM.                    " CONVERSION_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  MODIFY_MAT_INFO
*&---------------------------------------------------------------------*
FORM MODIFY_MAT_INFO.

*  DATA : L_IDX TYPE I.
*  LOOP AT GT_OUT.
*    L_IDX = SY-TABIX.

  READ TABLE GT_MAT WITH KEY MATNR = GT_OUT-MATNR BINARY SEARCH.
  IF SY-SUBRC <> 0.
    BREAK-POINT.
  ENDIF.

* UD1K941202 - by IG.MOON 8/2/2007 {
*    IF GT_MAT-MTART EQ 'HALB' AND GT_MAT-SOBSL EQ '50'.
*      DELETE GT_OUT WHERE MATNR = GT_MAT-MATNR
*                      AND WERKS = GT_MAT-WERKS.
*    ENDIF.
* }

* Get Price unit, MAP, STD
  GT_OUT-STPRS = GT_MAT-STPRS." * gt_mat-peinh.  " Standard price
  GT_OUT-VERPR = GT_MAT-PVPRS." * gt_mat-peinh.  " Moving average price

  GT_OUT-PEINH = GT_MAT-PEINH.  " Price unit
  GT_OUT-BKLAS = GT_MAT-BKLAS.    " Valuation class

* Get Meterial Info.
  GT_OUT-MAKTG = GT_MAT-MAKTG.     " Material description
  GT_OUT-MTART = GT_MAT-MTART.     " Material type
  GT_OUT-PROFL = GT_MAT-PROFL.     " Src
  GT_OUT-MSTAE = GT_MAT-MSTAE.     " Cross-Plant Material Status
  GT_OUT-EKGRP = GT_MAT-EKGRP.     " Pur.grp
  GT_OUT-STLAN = GT_MAT-STLAN.     " BOM usage
  GT_OUT-STAWN = GT_MAT-STAWN.     " Commodity code
  GT_OUT-DISPO = GT_MAT-DISPO.     " MRP controller
  GT_OUT-MMSTA = GT_MAT-MMSTA.     " Plant-Specific Material Status

* UD1K941202 - by IG.MOON 8/2/2007 {
  GT_OUT-MATKL = GT_MAT-MATKL.

* }

*    MODIFY GT_OUT INDEX L_IDX
*           TRANSPORTING STPRS VERPR PEINH BKLAS
*                               MAKTG MTART PROFL MSTAE
*                               EKGRP STLAN STAWN DISPO
*                               MMSTA MATKL.
*
*
*  ENDLOOP.

* UD1K941202 - by IG.MOON 8/2/2007 { commented
*  IF SY-SUBRC EQ 0.
*
*  SORT GT_OUT BY CHK.
*  DELETE ADJACENT DUPLICATES FROM GT_OUT.
**  ELSE.
*
*  LOOP AT GT_OUT.
*    L_INDEX = SY-TABIX.
*    READ TABLE GT_MAT WITH KEY MATNR = GT_OUT-MATNR.
*    IF SY-SUBRC NE 0.
*      DELETE GT_OUT INDEX L_INDEX.
*    ENDIF.
*  ENDLOOP.

*  ENDIF.


ENDFORM.                    " MODIFY_MAT_INFO
*&---------------------------------------------------------------------*
*&      Form  APPEND_GT_OUT
*&---------------------------------------------------------------------*
*       Append uploaded data to Internal Table GT_OUT
*----------------------------------------------------------------------*
FORM APPEND_GT_OUT TABLES LT_ZTCOU102 STRUCTURE ZTCOU102.
  DATA: LT_OUT    LIKE GT_OUT OCCURS 0 WITH HEADER LINE,
        LV_KNUMH  TYPE KNUMH,
        LV_PKNUMH TYPE KNUMH,
        LV_INDEX  TYPE SYTABIX.

  CLEAR LT_OUT.
  REFRESH LT_OUT.

* Create Internal Table LT_ZTCOU102 like Internal Table GT_OUT
  LOOP AT LT_ZTCOU102.
    MOVE-CORRESPONDING LT_ZTCOU102 TO LT_OUT.

*   Get Price unit, MAP, STD
    SELECT SINGLE STPRS PVPRS
      INTO (LT_OUT-STPRS, LT_OUT-VERPR)
      FROM CKMLCR AS A
      JOIN CKMLHD AS B
        ON B~KALNR = A~KALNR
       AND B~MATNR = LT_OUT-MATNR
       AND B~BWKEY = LT_OUT-WERKS
     WHERE A~BDATJ = P_YEAR
       AND A~POPER = P_POPER
       AND A~UNTPER = '000'
       AND A~CURTP = '10'.

*   Get VC
    SELECT SINGLE PEINH BKLAS INTO (LT_OUT-PEINH, LT_OUT-BKLAS)
      FROM MBEW
     WHERE MATNR = LT_OUT-MATNR
       AND BWKEY = LT_OUT-WERKS.

*   Get material info.
    SELECT SINGLE A~MAKTG B~MTART B~PROFL B~MSTAE
                  C~EKGRP C~STLAN C~STAWN
                  C~DISPO C~MMSTA
             INTO (LT_OUT-MAKTG, LT_OUT-MTART, LT_OUT-PROFL,
                   LT_OUT-MSTAE, LT_OUT-EKGRP, LT_OUT-STLAN,
                   LT_OUT-STAWN, LT_OUT-DISPO, LT_OUT-MMSTA)
      FROM MAKT AS A
      JOIN MARA AS B
        ON B~MATNR = A~MATNR
      JOIN MARC AS C
        ON C~MATNR = A~MATNR
       AND C~WERKS = LT_OUT-WERKS
     WHERE A~MATNR = LT_OUT-MATNR.

*   Price Diff.
    LT_OUT-DIFF = LT_OUT-WERTN_V1 - LT_OUT-PWERTN.

*   Changed by, Changed on
    SELECT SINGLE AENAM AEDAT
      INTO (LT_OUT-AENAM, LT_OUT-AEDAT)
      FROM ZTCOU102
     WHERE BDATJ  = LT_OUT-BDATJ
       AND POPER  = LT_OUT-POPER
       AND KALKA  = LT_OUT-KALKA
       AND KADKY  = LT_OUT-KADKY
       AND MATNR  = LT_OUT-MATNR
       AND WERKS  = LT_OUT-WERKS
       AND EKGRP  = LT_OUT-EKGRP
       AND PROFL  = LT_OUT-PROFL
       AND LIFNR  = LT_OUT-LIFNR.

    IF SY-SUBRC = 0.
      LT_OUT-STAT = 'C'.    " Status
    ENDIF.

*   Get Error Category
    PERFORM GET_ERR_CATEGORY USING LT_OUT
                             CHANGING LT_OUT-EC_G
                                      LT_OUT-EC_A
                                      LT_OUT-EC_S
                                      LT_OUT-EC_V
                                      LT_OUT-EC_P
                                      LT_OUT-EC_R.

    CASE LT_OUT-ESTAT.
      WHEN 'G'.
        LT_OUT-ESTAT = ICON_LED_GREEN.
      WHEN 'R'.
        LT_OUT-ESTAT = ICON_LED_RED.
      WHEN 'Y'.
        LT_OUT-ESTAT = ICON_LED_YELLOW.
    ENDCASE.

    CLEAR LV_INDEX.
    READ TABLE GT_OUT WITH KEY KOKRS = LT_ZTCOU102-KOKRS
                               BDATJ = LT_ZTCOU102-BDATJ
                               POPER = LT_ZTCOU102-POPER
                               KALKA = LT_ZTCOU102-KALKA
                               VER = LT_ZTCOU102-VER
                               MATNR = LT_ZTCOU102-MATNR
                               WERKS = LT_ZTCOU102-WERKS.

    IF SY-SUBRC = 0.
      LV_INDEX = SY-TABIX.
      MODIFY GT_OUT FROM LT_OUT INDEX LV_INDEX.
    ELSE.
      APPEND LT_OUT.
    ENDIF.

    CLEAR LT_OUT.
  ENDLOOP.

* Append to Internal Table GT_OUT
  LOOP AT LT_OUT.
    APPEND LT_OUT TO GT_OUT.
  ENDLOOP.

ENDFORM.                    " APPEND_GT_OUT
*&---------------------------------------------------------------------*
*&      Form  GET_ABP_LDC_GT_OUT
*&---------------------------------------------------------------------*
*       LDC ABP Rate
*----------------------------------------------------------------------*
FORM GET_ABP_LDC_GT_OUT.
  DATA: L_FRA1 TYPE ZFRA1,
        L_ZOTH TYPE ZOTH,
        L_ZOTI TYPE ZOTI,
        L_OTH  TYPE ZOTH1.

*LOCKED record - no change allowed!!!
  CHECK GT_OUT-ZLOCK = SPACE.

  CLEAR: GT_OUT-FRG, GT_OUT-OTH, GT_OUT-DUTY.
  CLEAR: L_FRA1, L_ZOTH, L_ZOTI.

* Freight, Other
  READ TABLE GT_LDC WITH KEY KOKRS = GT_OUT-KOKRS
                             BDATJ = GT_OUT-BDATJ
                             VER = GT_OUT-VER
                             LAND1 = GT_OUT-LAND1
                             MATNR = GT_OUT-MATNR.
  IF SY-SUBRC <> 0.
    READ TABLE GT_LDC WITH KEY KOKRS = GT_OUT-KOKRS
                               BDATJ = GT_OUT-BDATJ
                               VER = GT_OUT-VER
                               LAND1 = GT_OUT-LAND1.
  ENDIF.

  IF SY-SUBRC = 0.
    L_FRA1 = GT_LDC-FRA1.
    L_ZOTH = GT_LDC-ZOTH.
    L_ZOTI = GT_LDC-ZOTI.

    GT_OUT-FRG = GT_OUT-WERTN * ( L_FRA1 / 100 ).   " Freight

    CLEAR L_OTH.
    L_OTH = L_ZOTH  + L_ZOTI.
    GT_OUT-OTH = GT_OUT-WERTN * ( L_OTH / 100 ).    " Other

* Duty
    READ TABLE GT_A902 WITH KEY STAWN = GT_OUT-STAWN BINARY SEARCH.

    IF SY-SUBRC = 0.
      DATA $P_DUTY LIKE P_DUTY.
      $P_DUTY = P_DUTY * 10.
      IF $P_DUTY < GT_A902-KBETR.
        GT_A902-KBETR = $P_DUTY.
      ENDIF.

      GT_OUT-DUTY = GT_OUT-WERTN * ( GT_A902-KBETR / 1000 ).

    ELSE.
      CLEAR GT_OUT-DUTY.
      GT_OUT-EC_D = 'X'.
    ENDIF.

*-NO KD
  ELSE.
    CLEAR: GT_OUT-FRG, GT_OUT-OTH, GT_OUT-DUTY.
  ENDIF.

*-Gross Price
  GT_OUT-GPRC = GT_OUT-WERTN + GT_OUT-DUTY + GT_OUT-FRG + GT_OUT-OTH.

ENDFORM.                    " GET_ABP_LDC_GT_OUT
*&---------------------------------------------------------------------*
*&      Form  GET_GT_A018
*&---------------------------------------------------------------------*
FORM GET_GT_A018.
  DATA L_DATE    TYPE SYDATUM.
  DATA LT_018V   LIKE GT_A018 OCCURS 0  WITH HEADER LINE.

  CLEAR: L_DATE.

  CLEAR: GT_A018.
  REFRESH GT_A018.

*  READ TABLE GT_OUT INDEX 1.
*  CHECK GT_OUT-KALKA <> 'BP'.    "Exclude BP costing

  DATA $IX LIKE SY-TABIX.

  LOOP AT GT_OUT.
    $IX = SY-TABIX.
    L_DATE = GT_OUT-BWDAT.

* <<<< failed CK11
    IF GT_OUT-INFNR IS INITIAL OR GT_OUT-WERTN IS INITIAL.
* get info-record with material (without vendor)
      PERFORM GET_INFO_FR_MATERIAL
              USING GT_OUT-MATNR L_DATE.

*ANDY!!!
*     MODIFY gt_out INDEX $ix.

* <<<< vendor determined from cost estimate. info-record...
    ELSE.
      LT_MATNR-MATNR = GT_OUT-MATNR.
      LT_MATNR-LIFNR = GT_OUT-LIFNR.
      APPEND LT_MATNR.
    ENDIF.
  ENDLOOP.

* get info-record with material + vendor
  IF NOT LT_MATNR[] IS INITIAL.
    SELECT A~MATNR A~LIFNR A~DATBI A~DATAB A~KNUMH
      INTO CORRESPONDING FIELDS OF TABLE LT_018V
      FROM A018 AS A
      FOR ALL ENTRIES IN LT_MATNR
     WHERE A~KAPPL = 'M'                " Purchasing
       AND A~KSCHL = 'PB00'             " ZTIR = PB00
       AND A~LIFNR = LT_MATNR-LIFNR
       AND A~MATNR = LT_MATNR-MATNR
       AND A~EKORG = C_EKORG
       AND A~ESOKZ = '0'                " Standard
       AND A~DATAB =< L_DATE
       AND A~DATBI >= L_DATE.

    DATA LT_018T   LIKE GT_A018 OCCURS 0  WITH HEADER LINE.
    LOOP AT LT_018V.
      REFRESH LT_018T.
      APPEND LT_018V TO LT_018T.
      PERFORM GET_INFO_PRICE TABLES LT_018T.   " USING f_matnr.

*      SELECT SINGLE a~kzust b~kpein b~kmein
*         INTO (lt_018v-kzust, lt_018v-kpein, lt_018v-kmein)
*         FROM konh as a
*       INNER JOIN konp as b
*          ON a~knumh = b~knumh
*         WHERE a~knumh = lt_018v-knumh
*           and b~kschl = 'PB00'.

      APPEND LINES OF LT_018T TO GT_A018.
    ENDLOOP.

  ENDIF.
*---


* Check following material.
  DATA: BEGIN OF L_FMATNR OCCURS 0,
           FMATNR TYPE MATNR,
        END OF L_FMATNR.
** Fuorng on 10/31/11
  IF NOT GT_A018[] IS INITIAL.
** end on 10/31/11
    REFRESH: L_FMATNR.
    SELECT FMATNR INTO TABLE L_FMATNR
       FROM ZTCOU105
       FOR ALL ENTRIES IN GT_A018
       WHERE KOKRS = P_KOKRS
         AND TMATNR = GT_A018-MATNR.
    LOOP AT L_FMATNR.
      PERFORM GET_INFO_FR_MATERIAL USING L_FMATNR-FMATNR L_DATE.
    ENDLOOP.
  ENDIF.

* Previous date..info----
  PERFORM GET_INFO_PREV_PERIOD USING GT_OUT-BWDAT.

ENDFORM.                    " GET_GT_A018
*&---------------------------------------------------------------------*
*&      Form  READ_MAT_INFO
*&---------------------------------------------------------------------*
*       Get Material Information
*----------------------------------------------------------------------*
FORM READ_MAT_INFO.
  DATA: L_IDX LIKE SY-TABIX.

  CLEAR: GT_MAT.
  REFRESH: GT_MAT.

* Get STD, MAP, VC, Material Info when get data from CKIS, KEKO
*  SORT GT_MAT_TEMP BY MATNR BWKEY.
*  DELETE ADJACENT DUPLICATES FROM GT_MAT_TEMP.

*   Get Meterial Info.
*   Get VC - fixme... current VC??

  SELECT A~MATNR A~MAKTG B~MTART B~PROFL B~MSTAE B~MEINS
         C~WERKS C~EKGRP C~STLAN C~STAWN C~DISPO C~MMSTA
* UD1K941202 - by IG.MOON 8/29/2007 {
*          E~PEINH
         C~LOSGR
* }
         E~BWKEY E~BKLAS
         E~STPRS E~VERPR AS PVPRS
* UD1K941202 - by IG.MOON 8/2/2007 {
         C~SOBSL B~MATKL
* }
    INTO TABLE GT_MAT
*      INTO CORRESPONDING FIELDS OF TABLE GT_MAT

    FROM MAKT AS A
    JOIN MARA AS B
      ON B~MATNR = A~MATNR
    JOIN MARC AS C
      ON C~MATNR = A~MATNR
    JOIN CKMLHD AS D
      ON D~MATNR = A~MATNR
     AND D~BWKEY = C~WERKS
    JOIN MBEW AS E
      ON E~KALN1 = D~KALNR
   FOR ALL ENTRIES IN GT_OUT  "MAT_TEMP
 WHERE A~MATNR = GT_OUT-MATNR
   AND C~WERKS = GT_OUT-WERKS
   AND C~EKGRP IN S_EKGRP
   AND C~DISPO IN S_DISPO.

**   Get Price unit, MAP, STD
*    select b~matnr b~peinh b~bwkey b~bklas
*           stprs VERPR as pvprs
*      into table gt_ckmlcr
*      from ckmlhd as a
*      join mbew   as b
*        on b~kaln1 = a~kalnr
*       for all entries in gt_mat
*     where a~matnr = gt_mat-matnr
*       and a~bwkey = gt_mat-werks.
*
  SORT GT_MAT    BY MATNR WERKS.

*    if not s_dispo[] is initial.
*     delete gt_mat where not DISPO in s_dispo.
*    endif.


ENDFORM.                    " READ_MAT_INFO
*&---------------------------------------------------------------------*
*&      Form  READ_VENDOR_DATA
*&---------------------------------------------------------------------*
FORM READ_VENDOR_DATA.
  DATA L_IDX TYPE SYTABIX.

  SORT GT_VENDOR BY LIFNR.
  DELETE ADJACENT DUPLICATES FROM GT_VENDOR.

  LOOP AT GT_VENDOR.
    CLEAR L_IDX.

    L_IDX = SY-TABIX.

    SELECT SINGLE LAND1 NAME1
      INTO (GT_VENDOR-LAND1, GT_VENDOR-NAME1)
      FROM LFA1
     WHERE LIFNR = GT_VENDOR-LIFNR.

    MODIFY GT_VENDOR INDEX L_IDX TRANSPORTING LAND1 NAME1.
  ENDLOOP.

ENDFORM.                    " READ_VENDOR_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_KLVAR
*&---------------------------------------------------------------------*
*       Get Costing Variant
*----------------------------------------------------------------------*
FORM GET_KLVAR.
  CLEAR GV_KLVAR.

  CASE GT_OUT-KALKA+0(1).
    WHEN 'U'.
      GV_KLVAR = 'ZU01'.
    WHEN 'M'.
      GV_KLVAR = 'ZM01'.
    WHEN 'B'.
      GV_KLVAR = 'ZUBP'.
    WHEN 'R'.
      GV_KLVAR = 'ZR01'.
  ENDCASE.

ENDFORM.                    " GET_KLVAR
*&---------------------------------------------------------------------*
*&      Form  CHECK_PLANT
*&---------------------------------------------------------------------*
*       Check Plant
*----------------------------------------------------------------------*
FORM CHECK_PLANT USING    P_WERKS TYPE WERKS_D
                 CHANGING P_BESKZ TYPE BESKZ
                          L_FLAG  TYPE C.
  DATA : L_MMSTA LIKE MARC-MMSTA,
         L_NCOST LIKE MARC-NCOST.

* FLAG = 'X'  - Don't update
  SELECT SINGLE BESKZ MMSTA NCOST INTO (P_BESKZ, L_MMSTA, L_NCOST)
    FROM MARC
   WHERE MATNR = GT_OUT-MATNR
     AND WERKS = P_WERKS
     AND BESKZ = 'F' .
*     and sobsl = space.

* No costing
  IF L_NCOST EQ 'X' .
    L_FLAG = 'X'.
  ENDIF.

* material status
  SELECT SINGLE * FROM T141  WHERE MMSTA EQ L_MMSTA.
  IF SY-SUBRC EQ 0 AND ( T141-DERZK EQ 'B' OR T141-DERZK EQ 'D' ).
    L_FLAG = 'X'.
  ENDIF.

ENDFORM.                    " CHECK_PLANT
*&---------------------------------------------------------------------*
*&      Form  GET_GT_LDC
*&---------------------------------------------------------------------*
*       Get LDC
*----------------------------------------------------------------------*
FORM GET_GT_LDC.
  CLEAR:   GT_LDC, GT_A902.
  REFRESH: GT_LDC, GT_A902.

  SELECT KOKRS BDATJ VER LAND1 MATNR FRA1 ZOTH ZOTI
    INTO TABLE GT_LDC
    FROM ZTCOU116
   WHERE KOKRS = P_KOKRS
     AND BDATJ = P_YEAR
     AND VER = P_VER.

  SELECT STAWN KBETR INTO TABLE GT_A902
    FROM A902 AS A
    JOIN KONP AS B
      ON A~KNUMH = B~KNUMH
   WHERE A~KAPPL = 'M'
     AND A~KSCHL = 'ZOA1'.

  SORT GT_A902 BY STAWN.

ENDFORM.                    " GET_GT_LDC
*&---------------------------------------------------------------------*
*&      Form  APPLY_LDC
*&---------------------------------------------------------------------*
*       Apply LDC
*----------------------------------------------------------------------*
FORM APPLY_LDC.
  CLEAR: GT_ROW[], GT_ROID[].

* Get selected rows
  CALL METHOD G_GRID1->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = GT_ROW
      ET_ROW_NO     = GT_ROID.
  DATA L_BESKZ TYPE BESKZ.

  LOOP AT GT_ROW INTO GS_ROW.
    READ TABLE GT_OUT INDEX GS_ROW-INDEX.

    IF SY-SUBRC = 0.
      PERFORM GET_ABP_LDC_GT_OUT.
      PERFORM GT_OUT_STATUS_CHANGE.
      MODIFY GT_OUT INDEX GS_ROW-INDEX. " TRANSPORTING FRG OTH DUTY.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " APPLY_LDC
*&---------------------------------------------------------------------*
*&      Form  CHANGE_PURGR
*&---------------------------------------------------------------------*
*  Change Purchasing Group by BAPI Func:BAPI_MATERIAL_SAVEDATA
*----------------------------------------------------------------------*
FORM CHANGE_PURGR.
  DATA: L_TEXT(100),
        L_ANSWER.

  CLEAR: GT_ROW[], GT_ROID[], GV_TCNT, GV_SCNT, GV_FCNT,
         L_TEXT, L_ANSWER.

  L_TEXT = 'Are you sure change puchasing group?'.

* Get selected rows
  CALL METHOD G_GRID1->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = GT_ROW
      ET_ROW_NO     = GT_ROID.


  PERFORM POP_UP USING    L_TEXT '' 'X'
                 CHANGING L_ANSWER.

  IF L_ANSWER = 'J'.
    LOOP AT GT_ROW INTO GS_ROW.
      READ TABLE GT_OUT INDEX GS_ROW-INDEX.

      IF SY-SUBRC = 0.
        GV_TCNT = GV_TCNT + 1.
*       Change Pur.Grp by BAPI Func:BAPI_MATERIAL_SAVEDATA
        PERFORM CALL_BAPI_MATERIAL_SAVEDATA USING GT_OUT-WERKS.
      ENDIF.
    ENDLOOP.

    IF GV_TCNT > 0.
      GV_FCNT = GV_TCNT - GV_SCNT.
      IF GV_TCNT = GV_SCNT.
        MESSAGE S000 WITH 'Successfully changed purchasing group.'.
      ELSE.
        MESSAGE S000 WITH 'Success:' GV_SCNT 'Fail:' GV_FCNT.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.                    " CHANGE_PURGR
*&---------------------------------------------------------------------*
*&      Form  CHANGE_PRICE
*&---------------------------------------------------------------------*
*       Change Price
*----------------------------------------------------------------------*
FORM CHANGE_PRICE USING P_BWKEY TYPE BWKEY.
  DATA L_CHK VALUE 'X'.

  GS_HEAD-COST_VIEW = 'X'.             " Select Costing View

  CLEAR: GS_MBEW, GS_MBEWX.

  GS_MBEW-VAL_AREA = P_BWKEY.          " Val.Area
  GS_MBEW-PRICE_UNIT = GT_OUT-PEINH.   " Price Unit

  GS_MBEWX-VAL_AREA = P_BWKEY.
  GS_MBEWX-PRICE_UNIT = 'X'.

* ABP Costing
  IF GT_OUT-KALKA = 'BP'.
    GS_MBEW-PLNDPRICE3 = GT_OUT-WERTN + GT_OUT-DUTY
                         + GT_OUT-FRG + GT_OUT-OTH.
    GS_MBEW-PLNDPRDATE3  = GT_OUT-KADKY.  "from planning year
    GS_MBEWX-PLNDPRICE3  = 'X'.
    GS_MBEWX-PLNDPRDATE3 = 'X'.

    IF GS_MBEW-PLNDPRICE3 = 0. CLEAR L_CHK. ENDIF.

* Standard Costing
  ELSEIF GT_OUT-KALKA = 'R1'.
    GS_MBEW-PLNDPRICE1 = GT_OUT-WERTN + GT_OUT-DUTY
                         + GT_OUT-FRG + GT_OUT-OTH.
    GS_MBEW-PLNDPRDATE1 = GT_OUT-KADKY.
    GS_MBEWX-PLNDPRICE1 = 'X'.
    GS_MBEWX-PLNDPRDATE1 = 'X'.

    IF GS_MBEW-PLNDPRICE1 = 0. CLEAR L_CHK. ENDIF.
  ENDIF.

  IF L_CHK = 'X'.
    CLEAR GT_RETURN.
    REFRESH GT_RETURN.

    CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
      EXPORTING
        HEADDATA       = GS_HEAD
        VALUATIONDATA  = GS_MBEW
        VALUATIONDATAX = GS_MBEWX
      TABLES
        RETURNMESSAGES = GT_RETURN.
  ENDIF.

ENDFORM.                    " CHANGE_PRICE
*&---------------------------------------------------------------------*
*&      Form  CHANGE_MRP1_VIEW
*&---------------------------------------------------------------------*
*       Change MRP1 View: Purchasing Group
*----------------------------------------------------------------------*
FORM CHANGE_MRP1_VIEW.
  GS_HEAD-PURCHASE_VIEW = 'X'.               " Select MRP View

  CLEAR: GS_MARC, GS_MARCX.

  GS_MARC-PLANT = GT_OUT-WERKS.         " Plant
  GS_MARC-PUR_GROUP = GT_OUT-EKGRP.     " Pur.Grp

  GS_MARCX-PLANT = GT_OUT-WERKS.
  GS_MARCX-PUR_GROUP = 'X'.

  CLEAR GT_RETURN.
  REFRESH GT_RETURN.

  CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
    EXPORTING
      HEADDATA       = GS_HEAD
      PLANTDATA      = GS_MARC
      PLANTDATAX     = GS_MARCX
    TABLES
      RETURNMESSAGES = GT_RETURN.

ENDFORM.                    " CHANGE_MRP1_VIEW
*&---------------------------------------------------------------------*
*&      Form  CHANGE_BASIC_VIEW
*&---------------------------------------------------------------------*
*       Change Basic View: Source
*----------------------------------------------------------------------*
FORM CHANGE_BASIC_VIEW.
  GS_HEAD-BASIC_VIEW = 'X'.

  CLEAR: GS_MARA, GS_MARAX.

  GS_MARA-HAZMATPROF  = GT_OUT-PROFL.    " MIP/LP/KD
  GS_MARAX-HAZMATPROF = 'X'.

  CLEAR GT_RETURN.
  REFRESH GT_RETURN.

  CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
    EXPORTING
      HEADDATA       = GS_HEAD
      CLIENTDATA     = GS_MARA
      CLIENTDATAX    = GS_MARAX
    TABLES
      RETURNMESSAGES = GT_RETURN.

ENDFORM.                    " CHANGE_BASIC_VIEW
*&---------------------------------------------------------------------*
*&      Form  CHANGE_SRC
*&---------------------------------------------------------------------*
*       Change Source by BAPI Func:BAPI_MATERIAL_SAVEDATA
*----------------------------------------------------------------------*
FORM CHANGE_SRC.
  DATA: L_TEXT(100),
        L_ANSWER.

  CLEAR: GT_ROW[], GT_ROID[], GV_TCNT, GV_SCNT, GV_FCNT,
         L_TEXT, L_ANSWER.

* Get selected rows
  CALL METHOD G_GRID1->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = GT_ROW
      ET_ROW_NO     = GT_ROID.

  PERFORM POP_UP USING    'Are you sure change source?' '' 'X'
                 CHANGING L_ANSWER.

  IF L_ANSWER = 'J'.

    LOOP AT GT_ROW INTO GS_ROW.
      READ TABLE GT_OUT INDEX GS_ROW-INDEX.

      IF SY-SUBRC = 0.
        GV_TCNT = GV_TCNT + 1.
*       Change Pur.Grp by BAPI Func:BAPI_MATERIAL_SAVEDATA
        PERFORM CALL_BAPI_MATERIAL_SAVEDATA USING SPACE.

      ENDIF.
    ENDLOOP.

    IF GV_TCNT > 0.
      GV_FCNT = GV_TCNT - GV_SCNT.

      IF GV_TCNT = GV_SCNT.
        MESSAGE S000 WITH 'Successfully changed source.'.
      ELSE.
        MESSAGE S000 WITH 'Success:' GV_SCNT 'Fail:' GV_FCNT.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.                    " CHANGE_SRC
*&---------------------------------------------------------------------*
*&      Form  CHANGE_REASON
*&---------------------------------------------------------------------*
*       Change Reason
*----------------------------------------------------------------------*
FORM CHANGE_REASON.
  DATA: L_TEXT(100),
        L_ANSWER.

  CLEAR: GT_ROW[], GT_ROID[], GV_TCNT, GV_SCNT, GV_FCNT,
         L_TEXT, L_ANSWER.

* Get selected rows
  CALL METHOD G_GRID1->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = GT_ROW
      ET_ROW_NO     = GT_ROID.
  PERFORM POP_UP USING    'Are you sure change reasons?' '' ''
                 CHANGING L_ANSWER.

  IF L_ANSWER = 'J'.
    LOOP AT GT_ROW INTO GS_ROW.
      READ TABLE GT_OUT INDEX GS_ROW-INDEX.

      IF SY-SUBRC = 0.
        GV_TCNT = GV_TCNT + 1.
*   Change Reason
        PERFORM UPDATE_KONH.
      ENDIF.
    ENDLOOP.

    IF GV_TCNT > 0.
      GV_FCNT = GV_TCNT - GV_SCNT.

      IF GV_TCNT = GV_SCNT.
        MESSAGE S000 WITH 'Successfully changed reason.'.
      ELSE.
        MESSAGE S000 WITH 'Success:' GV_SCNT 'Fail:' GV_FCNT.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.                    " CHANGE_REASON
*&---------------------------------------------------------------------*
*&      Form  UPDATE_KONH
*&---------------------------------------------------------------------*
*       Update Table KONH
*----------------------------------------------------------------------*
FORM UPDATE_KONH.
*refer: RM06K050

  DATA: L_KNUMH TYPE KNUMH,
        LS_XKONH LIKE KONH,
        LS_YKONH LIKE KONH.

  CLEAR L_KNUMH.

  SELECT SINGLE KNUMH INTO L_KNUMH
    FROM A018
   WHERE KAPPL = 'M'                " Purchasing
     AND KSCHL = 'PB00'             " ZTIR = PB00
     AND LIFNR = GT_OUT-LIFNR
     AND MATNR = GT_OUT-MATNR
     AND EKORG = C_EKORG
     AND ESOKZ = '0'                " Standard
     AND DATAB =< GT_OUT-BWDAT
     AND DATBI >= GT_OUT-BWDAT.

  IF SY-SUBRC = 0.
    CLEAR: LS_XKONH, LS_YKONH.

    SELECT SINGLE * INTO LS_XKONH
      FROM KONH
     WHERE KNUMH = L_KNUMH.

*- U1 Start
    IF P_ARCH EQ 'X' AND SY-SUBRC <> 0.
      PERFORM ARCHIVE_READ_KONH_2 USING L_KNUMH CHANGING LS_XKONH.
    ENDIF.
*- U1 End

    MOVE-CORRESPONDING LS_XKONH TO LS_YKONH.
    LS_YKONH-KZUST = GT_OUT-KZUST1.

    UPDATE KONH SET KZUST = GT_OUT-KZUST1
       WHERE KNUMH = L_KNUMH.

    IF SY-SUBRC = 0.
      PERFORM COND_CHANGE_DOC USING LS_XKONH
                                    LS_YKONH.
    ENDIF.
  ENDIF.

ENDFORM.                    " UPDATE_KONH
*&---------------------------------------------------------------------*
*&      Form  COND_CHANGE_DOC
*&---------------------------------------------------------------------*
*       Change Document Condition
*----------------------------------------------------------------------*
FORM COND_CHANGE_DOC USING LS_XKONH STRUCTURE KONH
                           LS_YKONH STRUCTURE KONH.

* Tables starting with 'x' contain the old values,
* those starting with 'y' contain the new values
  DATA: L_OBJECTID                LIKE CDHDR-OBJECTID,
        L_TCODE                   LIKE CDHDR-TCODE VALUE 'ME14',
        L_PLANNED_CHANGE_NUMBER   LIKE CDHDR-PLANCHNGNR,
        L_CDOC_PLANNED_OR_REAL    LIKE CDHDR-CHANGE_IND,
        L_CDOC_UPD_OBJECT         LIKE CDHDR-CHANGE_IND VALUE 'U',
        L_CDOC_NO_CHANGE_POINTERS LIKE CDHDR-CHANGE_IND.

  DATA: LT_XKONDAT TYPE TABLE OF VKONDAT WITH HEADER LINE,
        LT_YKONDAT TYPE TABLE OF VKONDAT WITH HEADER LINE,
        LT_XKONM   TYPE TABLE OF VKONM   WITH HEADER LINE,
        LT_YKONM   TYPE TABLE OF VKONM   WITH HEADER LINE,
        LT_XKONPAE TYPE TABLE OF VKONPAE WITH HEADER LINE,
        LT_YKONPAE TYPE TABLE OF VKONPAE WITH HEADER LINE,
        LT_XKONW   TYPE TABLE OF VKONW   WITH HEADER LINE,
        LT_YKONW   TYPE TABLE OF VKONW   WITH HEADER LINE.

  CLEAR: L_OBJECTID, L_PLANNED_CHANGE_NUMBER,
         L_CDOC_PLANNED_OR_REAL,
         L_CDOC_UPD_OBJECT, L_CDOC_NO_CHANGE_POINTERS.

  CLEAR: LT_XKONDAT, LT_YKONDAT, LT_XKONM, LT_YKONM,
         LT_XKONPAE, LT_YKONPAE, LT_XKONW, LT_YKONW.

  REFRESH: LT_XKONDAT, LT_YKONDAT, LT_XKONM, LT_YKONM,
           LT_XKONPAE, LT_YKONPAE, LT_XKONW, LT_YKONW.

* Write Change Documents
  L_OBJECTID = LS_XKONH-KNUMH.

  CALL FUNCTION 'COND_A_WRITE_DOCUMENT' IN UPDATE TASK
    EXPORTING
      OBJECTID                = L_OBJECTID
      TCODE                   = L_TCODE
      UTIME                   = SY-UZEIT
      UDATE                   = SY-DATUM
      USERNAME                = SY-UNAME
      PLANNED_CHANGE_NUMBER   = L_PLANNED_CHANGE_NUMBER
      OBJECT_CHANGE_INDICATOR = 'U'
      PLANNED_OR_REAL_CHANGES = L_CDOC_PLANNED_OR_REAL
      NO_CHANGE_POINTERS      = L_CDOC_NO_CHANGE_POINTERS
      UPD_KONDAT              = ' '
      O_KONH                  = LS_XKONH
      N_KONH                  = LS_YKONH
      UPD_KONH                = 'U'
      UPD_KONM                = ' '
      UPD_KONPAE              = ' '
      UPD_KONW                = ' '
    TABLES
      XKONDAT                 = LT_XKONDAT
      YKONDAT                 = LT_YKONDAT
      XKONM                   = LT_XKONM
      YKONM                   = LT_YKONM
      XKONPAE                 = LT_XKONPAE
      YKONPAE                 = LT_YKONPAE
      XKONW                   = LT_XKONW
      YKONW                   = LT_YKONW.
  CLEAR L_PLANNED_CHANGE_NUMBER.

  IF SY-SUBRC = 0.
    GV_SCNT = GV_SCNT + 1.
  ENDIF.

ENDFORM.                    " COND_CHANGE_DOC
*&---------------------------------------------------------------------*
*&      Form  POP_UP
*&---------------------------------------------------------------------*
FORM POP_UP USING    P_TEXT P_TEXT2 P_CANC
            CHANGING P_ANSWER.
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      TEXTLINE1      = P_TEXT
      TEXTLINE2      = P_TEXT2
      TITEL          = 'Check!'
      CANCEL_DISPLAY = P_CANC
    IMPORTING
      ANSWER         = P_ANSWER.


ENDFORM.                    " POP_UP
*&---------------------------------------------------------------------*
*&      Form  SAVE_ZTCOU102_BY_RECOSTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SAVE_ZTCOU102_BY_RECOSTING CHANGING P_CNT TYPE I.
  DATA: LS_ZTCOU102 LIKE ZTCOU102,
        L_MATNR     TYPE MATNR.

  PERFORM GET_ERR_CATEGORY USING    GT_OUT
                           CHANGING GT_OUT-EC_G
                                    GT_OUT-EC_A
                                    GT_OUT-EC_S
                                    GT_OUT-EC_V
                                    GT_OUT-EC_P
                                    GT_OUT-EC_R.

  MODIFY GT_OUT INDEX GS_ROW-INDEX.

  CLEAR LS_ZTCOU102.
  MOVE-CORRESPONDING GT_OUT TO LS_ZTCOU102.

*  IF GV_CHK = 'X' AND
*     GT_OUT-STAT <> 'O' AND
*     GT_OUT-STAT <> 'R'.
*    LS_ZTCOU102-STAT = 'C'.
*  ENDIF.

  LS_ZTCOU102-AEDAT = SY-DATUM.
  LS_ZTCOU102-AENAM = SY-UNAME.
  LS_ZTCOU102-CPUTM = SY-UZEIT.

  CLEAR L_MATNR.
  SELECT SINGLE MATNR INTO L_MATNR
    FROM ZTCOU102
   WHERE KOKRS = P_KOKRS
     AND BDATJ = P_YEAR
     AND POPER = P_POPER
     AND KALKA = GT_OUT-KALKA
     AND VER   = GT_OUT-VER
     AND MATNR = GT_OUT-MATNR
     AND WERKS = GT_OUT-WERKS.

  IF SY-SUBRC = 0.
    UPDATE ZTCOU102 FROM LS_ZTCOU102.
  ELSE.
    INSERT INTO ZTCOU102 VALUES LS_ZTCOU102.
  ENDIF.

  IF SY-SUBRC = 0.
    P_CNT = P_CNT + 1.
  ENDIF.

ENDFORM.                    " SAVE_ZTCOU102_BY_RECOSTING
*&---------------------------------------------------------------------*
*&      Form  check_data_lock
*&---------------------------------------------------------------------*
FORM CHECK_DATA_LOCK.

  SELECT COUNT( * ) INTO SY-DBCNT
    FROM ZTCOU100
     WHERE KOKRS = P_KOKRS
       AND BDATJ = P_YEAR
       AND POPER = P_POPER
       AND KALKA IN S_KALKA.

  IF SY-DBCNT > 0.
    SELECT COUNT( * ) INTO SY-DBCNT
      FROM ZTCOU100
       WHERE KOKRS = P_KOKRS
         AND BDATJ = P_YEAR
         AND POPER = P_POPER
         AND KALKA IN S_KALKA
         AND LSTAT = SPACE.

    IF SY-DBCNT = 0.
      CLEAR: P_REF1.                                        ", P_REF2.
      P_MODE = 'L'.
    ENDIF.
  ENDIF.

ENDFORM.                    " check_data_lock
*&---------------------------------------------------------------------*
*&      Form  GET_GT_CKIS_new_logic
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_GT_CKIS_NEW_LOGIC.

  DATA I_KEKO LIKE KEKO OCCURS 0 WITH HEADER LINE.


  __CLS GR_BESKZ.
  GR_BESKZ-OPTION = 'EQ'.
  GR_BESKZ-SIGN = 'I'.
  GR_BESKZ-LOW = 'F'. APPEND GR_BESKZ.
* if s_kalka-low = 'M1'.
  GR_BESKZ-LOW = ' '. APPEND GR_BESKZ.
* endif.

  __CLS: GT_CKIS, GT_COMP, GT_STOCK.

* KEKO = header, CKIS = line
* CKHS = costed material(parent)

  PERFORM : GET_GT_CKIS_COSTED_NEW_LOGIC,    " GT_CKIS
*            get_gt_ckis_trf_new_logic,  " GT_STOCK "1/7/08 ig
            GET_GT_CKIS_MISSING_NEW_LOGIC.   " GT_COMP

  SORT : GT_CKIS BY MATNR SOBES,
*         gt_stock BY matnr, " 1/7/08 ig
         GT_COMP BY MATNR.

  SORT GT_CKIS BY MATNR. "BESKZ.
  LOOP AT GT_COMP.
    READ TABLE GT_CKIS WITH KEY
                       MATNR = GT_COMP-MATNR. "" UD1K953348
*                       BESKZ = GT_COMP-BESKZ
*                       BINARY SEARCH.        "" UD1K953348
    IF SY-SUBRC <> 0.
      GT_COMP-COMPN = 'X'.
      APPEND GT_COMP TO GT_CKIS.
    ENDIF.

  ENDLOOP.

*    SORT GT_CKIS BY MATNR SOBES.
*
*    LOOP AT GT_STOCK.
*      READ TABLE GT_CKIS WITH KEY
*                         MATNR = GT_STOCK-MATNR
**                         SOBES = '7'
*                         BINARY SEARCH.
*      IF SY-SUBRC EQ 0.
*        DELETE GT_CKIS WHERE MATNR = GT_STOCK-MATNR
*                         AND SOBES = '7' .
*        APPEND GT_STOCK TO GT_CKIS.
*      ELSE.
*        APPEND GT_STOCK TO GT_CKIS.
*      ENDIF.
*    ENDLOOP.


*Must use index  D~KALAID/KALADAT  (find product)
  SELECT *
INTO CORRESPONDING FIELDS OF TABLE
    I_KEKO
    FROM  KEKO
  WHERE  KALAID  IN GR_KALAID
     AND KALADAT = GV_KALADAT
     AND KALKA IN S_KALKA
     AND BDATJ = P_YEAR
     AND POPER = P_POPER
*    AND BESKZ NE 'F'   "( BESKZ NE 'E' OR BESKZ EQ 'X' )
     AND STLAN NE SPACE
     AND TVERS = GC_TVERS
     AND WERKS IN R_BWKEY
     AND BWSMR = SPACE.    "valuation strategy used
*- U1 Start
  IF P_ARCH EQ 'X'.
    PERFORM ARCHIVE_READ_KEKO TABLES I_KEKO USING 'P'.
  ENDIF.
*- U1 End
*Find manual costing
  SELECT *
  APPENDING CORRESPONDING FIELDS OF TABLE
  I_KEKO
  FROM  KEKO
  WHERE  KALAID  = SPACE
     AND KALADAT = SPACE
     AND KALKA IN S_KALKA
     AND BDATJ = P_YEAR
     AND POPER = P_POPER
*    AND ( BESKZ EQ 'E' OR BESKZ EQ 'X' )
     AND STLAN NE SPACE
     AND TVERS = GC_TVERS
     AND WERKS IN R_BWKEY
     AND BWSMR = SPACE.    "valuation strategy used
*- U1 Start
  IF P_ARCH EQ 'X'.
    PERFORM ARCHIVE_READ_KEKO TABLES I_KEKO USING 'C'.
  ENDIF.
*- U1 End
* SORT I_KEKO BY MATNR.
  SORT I_KEKO BY KALNR.

*remove product material
  CLEAR GT_CKIS.
  DATA $IX LIKE SY-TABIX.
  LOOP AT GT_CKIS.
    $IX = SY-TABIX.
*    READ TABLE I_KEKO WITH KEY MATNR = GT_CKIS-MATNR.
    READ TABLE I_KEKO WITH KEY KALNR = GT_CKIS-KALNR.
    IF SY-SUBRC EQ 0.
      DELETE GT_CKIS INDEX $IX.
    ENDIF.
  ENDLOOP.

*remove locked item
  LOOP AT GT_CKIS.
    $IX = SY-TABIX.
    READ TABLE GT_ZTCOU102 WITH KEY MATNR = GT_CKIS-MATNR
         BINARY SEARCH.
    IF SY-SUBRC = 0 AND GT_ZTCOU102-ZLOCK = 'X'.
      DELETE GT_CKIS INDEX $IX.
    ENDIF.
  ENDLOOP.

* start of UD1K940727
*delete duplicate materials (P001 > E001)
* SOBES : To delete stock transfer record ex) F - F'
  SORT GT_CKIS BY MATNR WERKS DESCENDING SOBES ASCENDING.
*  DELETE ADJACENT DUPLICATES FROM GT_CKIS COMPARING MATNR .
  DELETE ADJACENT DUPLICATES FROM GT_CKIS COMPARING MATNR HRKFT .
* end of UD1K940727

* by ig.moon 1/12/2012 {
  LOOP AT GT_CKIS.
    IF GT_CKIS-PMEHT EQ 'ROL'.
      $IX = SY-TABIX.
      SELECT SINGLE LMEIN INTO GT_CKIS-PMEHT
      FROM EINA WHERE INFNR EQ GT_CKIS-INFNR.
      IF SY-SUBRC EQ 0.
        MODIFY GT_CKIS INDEX $IX TRANSPORTING PMEHT.
      ENDIF.
    ELSE.
      CONTINUE.
    ENDIF.
  ENDLOOP.
* }

ENDFORM.                    " GET_GT_CKIS_new_logic
*&---------------------------------------------------------------------*
*&      Form  GET_GT_CKIS_COSTED_new_logic
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_GT_CKIS_COSTED_NEW_LOGIC.

  SELECT A~KALNR A~KALKA A~KADKY A~MATNR A~KOKRS
         A~WERKS
         A~BDATJ A~POPER A~BWDAT A~ALDAT
         B~HRKFT B~PMEHT B~MENGE SUM( B~WERTN ) AS WERTN
         B~INFNR B~LIFNR C~LAND1 A~BESKZ A~SOBES B~BAUGR D~MATKL
         B~STRAT B~SUBSTRAT B~ELEMT B~ELEMTNS
    INTO CORRESPONDING FIELDS OF TABLE GT_CKIS
    FROM KEKO AS A
    JOIN CKIS AS B
      ON B~LEDNR = C_LEDNR
     AND B~BZOBJ = A~BZOBJ
     AND B~KALNR = A~KALNR
     AND B~KALKA = A~KALKA
     AND B~KADKY = A~KADKY
     AND B~TVERS = A~TVERS
     AND B~BWVAR = A~BWVAR
     AND B~KKZMA = A~KKZMA
* UD1K941202 - by IG.MOON {
    JOIN MARA AS D
     ON D~MATNR EQ B~MATNR
* }
     LEFT OUTER JOIN LFA1 AS C
       ON C~LIFNR = B~LIFNR
   WHERE A~MATNR IN S_MATNR
     AND A~MATNR IN S_ARTNR     "ANDY
     AND A~BDATJ = P_YEAR
     AND A~POPER = P_POPER
     AND A~KALKA IN S_KALKA
     AND A~TVERS = GC_TVERS
     AND A~BESKZ IN GR_BESKZ
     AND A~WERKS IN R_BWKEY
     AND A~BWSMR <> SPACE   " L-info, U-exit
     AND B~LIFNR IN S_LIFNR
     AND B~KSTAR IN S_KSTAR
* UD1K941202 - by IG.MOON {
     AND B~HRKFT EQ SPACE
     AND D~MATKL IN S_MATKL

* }
 GROUP BY
       A~KALNR A~KALKA A~KADKY A~MATNR A~KOKRS
       A~WERKS
       A~BDATJ A~POPER A~BWDAT A~ALDAT
       B~HRKFT B~PMEHT B~MENGE B~INFNR B~LIFNR C~LAND1 A~BESKZ
       A~SOBES B~BAUGR D~MATKL
       B~STRAT B~SUBSTRAT B~ELEMT B~ELEMTNS
.

*- U1 Start
  IF P_ARCH EQ 'X'.
    PERFORM ARCHIVE_READ_CKIS_3.
  ENDIF.
*- U1 End

ENDFORM.                    " GET_GT_CKIS_COSTED_new_logic
*&---------------------------------------------------------------------*
*&      Form  GET_GT_CKIS_COSTING_MISSING_NE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_GT_CKIS_MISSING_NEW_LOGIC.

  SELECT D~KALNR A~KALKA A~KADKY B~MATNR A~KOKRS
         A~WERKS
         A~BDATJ A~POPER A~BWDAT A~ALDAT
         B~HRKFT B~PMEHT B~MENGE B~WERTN
         B~INFNR B~LIFNR C~LAND1 A~BESKZ A~SOBES B~BAUGR E~MATKL
         B~STRAT B~SUBSTRAT B~ELEMT B~ELEMTNS
   INTO CORRESPONDING FIELDS OF TABLE GT_COMP
    FROM KEKO AS A
    JOIN CKIS AS B
      ON B~LEDNR = C_LEDNR
     AND B~BZOBJ = A~BZOBJ
     AND B~KALNR = A~KALNR
     AND B~KALKA = A~KALKA
     AND B~KADKY = A~KADKY
     AND B~TVERS = A~TVERS
     AND B~BWVAR = A~BWVAR
     AND B~KKZMA = A~KKZMA
* UD1K941202 - by IG.MOON {
    JOIN MARA AS E
     ON E~MATNR EQ B~MATNR
* }
    JOIN CKMLHD AS D
      ON D~MATNR = B~MATNR
     AND D~BWKEY = B~WERKS
     LEFT OUTER JOIN LFA1 AS C
       ON C~LIFNR = B~LIFNR
   WHERE A~BDATJ = P_YEAR
     AND A~POPER = P_POPER
     AND A~KALKA IN S_KALKA
     AND A~MATNR IN S_ARTNR     "ANDY
     AND A~TVERS = GC_TVERS
     AND A~BESKZ = 'E'      " In-house production
     AND A~WERKS IN R_BWKEY
     AND A~BWSMR = SPACE    " L-info, U-exit
     AND B~MATNR IN S_MATNR
     AND B~LIFNR IN S_LIFNR
     AND B~KSTAR IN S_KSTAR
*    AND b~baugr = space    " No Assy.
*    AND a~stnum EQ space   "ANDY 12/6/07
     AND E~MATKL IN S_MATKL
 GROUP BY
       D~KALNR A~KALKA A~KADKY B~MATNR A~KOKRS
       A~WERKS
       A~BDATJ A~POPER A~BWDAT A~ALDAT
       B~HRKFT B~PMEHT B~MENGE B~WERTN
       B~INFNR B~LIFNR C~LAND1 A~BESKZ A~SOBES B~BAUGR E~MATKL
       B~STRAT B~SUBSTRAT B~ELEMT B~ELEMTNS
.

*- U1 Start
  IF P_ARCH EQ 'X'.
    PERFORM ARCHIVE_READ_CKIS_5.
  ENDIF.
*- U1 End

* loop at GT_COMP.
*   select single * from keko
*      where matnr = gt_comp-matnr
*        and STLAN <> space      "MIP check
*        and KALKA in s_kalka
*        and KADKY = gv_kadky
*        and KOKRS = p_kokrs.
*   if sy-subrc = 0.
*     delete gt_comp index sy-tabix.
*   endif.
* endloop.


ENDFORM.                    " GET_GT_CKIS_COSTING_MISSING_NE
*&---------------------------------------------------------------------*
*&      Form  GET_GT_CKIS_TRF_NEW_LOGIC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_GT_CKIS_TRF_NEW_LOGIC.
* Get Addtion: Stock Trf exist, Actual Costing not exist

  SELECT A~KALNR A~KALKA A~KADKY A~MATNR A~KOKRS
         A~SOWRK AS WERKS
         A~BDATJ A~POPER A~BWDAT A~ALDAT
         B~HRKFT B~PMEHT B~MENGE SUM( B~WERTN ) AS WERTN
         B~INFNR A~BESKZ A~SOBES B~BAUGR D~MATKL
         B~STRAT B~SUBSTRAT B~ELEMT B~ELEMTNS
    INTO  CORRESPONDING FIELDS OF TABLE GT_STOCK
    FROM KEKO AS A
    JOIN CKIS AS B
      ON B~LEDNR = C_LEDNR
     AND B~BZOBJ = A~BZOBJ
     AND B~KALNR = A~KALNR
     AND B~KALKA = A~KALKA
     AND B~KADKY = A~KADKY
     AND B~TVERS = A~TVERS
     AND B~BWVAR = A~BWVAR
     AND B~KKZMA = A~KKZMA
* UD1K941202 - by IG.MOON {
    JOIN MARA AS D
     ON D~MATNR EQ B~MATNR
* }
   WHERE A~MATNR IN S_MATNR
     AND A~MATNR IN S_ARTNR     "ANDY
     AND A~BDATJ = P_YEAR
     AND A~POPER = P_POPER
     AND A~KALKA IN S_KALKA
     AND A~TVERS = GC_TVERS
     AND A~BESKZ IN GR_BESKZ
     AND A~SOBES = '7'
     AND B~LIFNR IN S_LIFNR
     AND B~KSTAR IN S_KSTAR
     AND B~BAUGR = SPACE     "not assembly
     AND D~MATKL IN S_MATKL
 GROUP BY
       A~KALNR A~KALKA A~KADKY A~MATNR A~KOKRS
       A~SOWRK
       A~BDATJ A~POPER A~BWDAT A~ALDAT
       B~HRKFT B~PMEHT B~MENGE B~INFNR A~BESKZ A~SOBES B~BAUGR D~MATKL
       B~STRAT B~SUBSTRAT B~ELEMT B~ELEMTNS
.

*- U1 Start
  IF P_ARCH EQ 'X'.
    PERFORM ARCHIVE_READ_CKIS_4.
  ENDIF.
*- U1 End

ENDFORM.                    " GET_GT_CKIS_TRF_NEW_LOGIC
*&---------------------------------------------------------------------*
*&      Form  GET_GV_BWDAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_GV_BWDAT.

  SELECT SINGLE * INTO *ZTCOU102 FROM ZTCOU102
                        WHERE KOKRS = P_KOKRS
                         AND BDATJ = P_YEAR
                         AND POPER = P_POPER
                         AND KALKA IN S_KALKA.
  IF SY-SUBRC EQ 0.
    GV_BWDAT = *ZTCOU102-BWDAT.
  ENDIF.

ENDFORM.                    " GET_GV_BWDAT
*&---------------------------------------------------------------------*
*&      Form  set_global_variable
*&---------------------------------------------------------------------*
FORM SET_GLOBAL_VARIABLE.
  REFRESH: GR_KALAID.

*-default...
  IF S_KALKA-LOW = 'BP'. P_LDC = 'X'. ENDIF.
  IF S_KALKA-LOW = 'M1'. P_LDC = ' '. ENDIF.

  GR_KALAID-OPTION  = 'EQ'.
  GR_KALAID-SIGN    = 'I'.
  LOOP AT S_KALKA.
    CONCATENATE S_KALKA-LOW 'F' INTO GR_KALAID-LOW.
    APPEND GR_KALAID.  "vehicle
    CONCATENATE S_KALKA-LOW 'M' INTO GR_KALAID-LOW.
    APPEND GR_KALAID.  "MIP
  ENDLOOP.

  CONCATENATE P_YEAR P_POPER+1(2) '01' INTO GV_KALADAT.

* Costing Date
  GV_CSTDT = GV_KALADAT.
  GV_KADKY = GV_KALADAT.

  IF P_POPER = '1'.
    GV_PYEAR = P_YEAR - 1.
    GV_PPOPER = '12'.
  ELSE.
    GV_PYEAR = P_YEAR.
    GV_PPOPER = P_POPER - 1.
  ENDIF.

ENDFORM.                    " set_global_variable
*&---------------------------------------------------------------------*
*&      Form  get_info_prev_period
*&---------------------------------------------------------------------*
FORM GET_INFO_PREV_PERIOD USING    F_DATE LIKE SY-DATUM.

  DATA L_DATE   TYPE SYDATUM.
  CLEAR: L_DATE.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      DATE      = GT_OUT-BWDAT
      DAYS      = '00'
      MONTHS    = '01'
      SIGNUM    = '-'
      YEARS     = '00'
    IMPORTING
      CALC_DATE = L_DATE.

  LOOP AT GT_A018.
    READ TABLE GT_102P WITH KEY MATNR = GT_A018-MATNR BINARY SEARCH.
    IF SY-SUBRC = 0.

    ELSE.
      IF GT_A018-DATAB =< L_DATE AND GT_A018-DATBI >= L_DATE.
        GT_P018 = GT_A018.
        APPEND GT_P018.
      ELSE.
        SELECT SINGLE MATNR LIFNR DATBI DATAB KNUMH
          INTO GT_P018
          FROM A018
         WHERE KAPPL = 'M'                " Purchasing
           AND KSCHL = 'PB00'             " ZTIR = PB00
           AND LIFNR = GT_A018-LIFNR
           AND MATNR = GT_A018-MATNR
           AND EKORG = C_EKORG
           AND ESOKZ = '0'                " Standard
           AND DATAB =< L_DATE
           AND DATBI >= L_DATE.
        APPEND GT_P018.
      ENDIF.
    ENDIF.

  ENDLOOP.

  SORT GT_P018 BY MATNR.

ENDFORM.                    " get_info_prev_period
*&---------------------------------------------------------------------*
*&      Form  get_prev_102
*&---------------------------------------------------------------------*
FORM GET_PREV_102.

  SELECT A~MATNR A~WERTN
    INTO TABLE GT_102P
    FROM ZTCOU102 AS A
    JOIN MARA AS B
      ON B~MATNR = A~MATNR
   WHERE A~KOKRS = P_KOKRS
     AND A~BDATJ = GV_PYEAR
     AND A~POPER = GV_PPOPER
     AND A~KALKA IN S_KALKA
     AND A~VER   = C_VER
     AND A~MATNR IN S_MATNR
     AND A~LIFNR IN S_LIFNR
     AND A~EKGRP IN S_EKGRP
     AND B~MATKL IN S_MATKL.

  SORT GT_102P BY MATNR.

ENDFORM.                    " get_prev_102
*&---------------------------------------------------------------------*
*&      Form  GET_GT_OUT_FROM_GT_CKIS_N
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_GT_OUT_FROM_GT_CKIS_N.
  DATA: L_DUTY  TYPE CK_KWT,      " Duty
        L_FRG   TYPE CK_KWT,      " Freight
        L_OTH   TYPE CK_KWT,      " Others
        L_WERTN TYPE CK_KWT.      " Info-price

  DATA: FLAG(1).

  CLEAR: L_DUTY, L_FRG, L_OTH, L_WERTN, GT_VENDOR.
  REFRESH GT_VENDOR.

  LOOP AT GT_CKIS.
    GT_OUT-KALKA  = GT_CKIS-KALKA.            " Costing type
    GT_OUT-KADKY  = GT_CKIS-KADKY.            " Costing date
    GT_OUT-WERKS  = GT_CKIS-WERKS.            " Plant
    GT_OUT-KOKRS  = GT_CKIS-KOKRS.            " Controling Area
    GT_OUT-LIFNR  = GT_CKIS-LIFNR.            " Vendor
    GT_OUT-INFNR  = GT_CKIS-INFNR.            " Inforecord No.
    GT_OUT-LAND1  = GT_CKIS-LAND1.            " Country key
    GT_OUT-NAME1  = GT_CKIS-NAME1.            " Vendor name
    GT_OUT-PMEHT  = GT_CKIS-PMEHT.            " uOm

*---only component exist...no break-down
    IF GT_CKIS-COMPN = 'X'.
      L_WERTN  = GT_CKIS-WERTN / GT_CKIS-MENGE.
    ELSE.

* UD1K941202 - by IG.MOON {
*      CASE GT_CKIS-HRKFT.
*        WHEN 'KD-D'.
*          L_DUTY = L_DUTY + GT_CKIS-WERTN.      " Duty
*        WHEN 'KD-F'.
*          L_FRG  = L_FRG + GT_CKIS-WERTN.       " Freight
*        WHEN 'KD-O'.
*          L_OTH  = L_OTH + GT_CKIS-WERTN.       " Others
*        WHEN SPACE.
*          L_WERTN = L_WERTN + GT_CKIS-WERTN.    " Info-Price
*      ENDCASE.

      L_WERTN = L_WERTN + GT_CKIS-WERTN.    " Info-Price

* }
    ENDIF.

    IF GT_OUT-LIFNR <> SPACE.
      GT_OUT-QTA = 100.                       " Quota
    ENDIF.

    GT_OUT-BDATJ  = GT_CKIS-BDATJ.            " Year
    GT_OUT-POPER  = GT_CKIS-POPER.            " Period
    GT_OUT-BWDAT  = GT_CKIS-BWDAT.            " Valuation date
    GT_OUT-ALDAT  = GT_CKIS-ALDAT.            " Qty structure date

    AT END OF MATNR.
      FLAG = 'X'.
    ENDAT.

    CHECK FLAG EQ 'X'.
    GT_OUT-MATNR  = GT_CKIS-MATNR.          " End Item

**     Create internal table for MAP, STD
*    PERFORM APPEND_GT_MAT_TEMP.

*     if costing type is MI, clear duty, freight, other
    IF GT_OUT-KALKA = 'M1'.
      CLEAR: GT_OUT-DUTY, GT_OUT-FRG, GT_OUT-OTH.
      GT_OUT-WERTN    = L_WERTN.     " Net Price
      GT_OUT-WERTN_V1 = L_WERTN.     " Net Price of 1st vendor
    ELSE.
      GT_OUT-DUTY     = L_DUTY.      " Duty
      GT_OUT-FRG      = L_FRG.       " Freight
      GT_OUT-OTH      = L_OTH.       " Others
      GT_OUT-WERTN    = L_WERTN.     " Net Price
      GT_OUT-WERTN_V1 = L_WERTN.     " Net Price of 2nd vendor
    ENDIF.

    IF NOT GT_CKIS-LIFNR IS INITIAL.
      GT_VENDOR-LIFNR = GT_CKIS-LIFNR.
      APPEND GT_VENDOR.
      CLEAR GT_VENDOR.
    ENDIF.

    GT_OUT-STRAT  = GT_CKIS-STRAT.
    GT_OUT-SUBSTRAT  = GT_CKIS-SUBSTRAT.
    GT_OUT-ELEMT  = GT_CKIS-ELEMT.
    GT_OUT-ELEMTNS  = GT_CKIS-ELEMTNS.
    GT_OUT-PRICE_MANUAL = SPACE.

* BEGIN OF HIS20094
    DATA: L_ENDDATE TYPE SY-DATUM,
          L_LIFNR   TYPE LFA1-LIFNR.

    CONCATENATE P_YEAR P_POPER+1(2) '01' INTO L_ENDDATE.

    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        DAY_IN            = L_ENDDATE
      IMPORTING
        LAST_DAY_OF_MONTH = L_ENDDATE.

    SELECT SINGLE LIFNR INTO L_LIFNR
      FROM ZTCOU137
     WHERE BUKRS = GV_BUKRS
       AND MATNR = GT_OUT-MATNR
       AND ZDTFR LT L_ENDDATE
       AND ZDTTO GE L_ENDDATE.

    IF SY-SUBRC EQ 0 AND NOT L_LIFNR IS INITIAL.
      GT_OUT-LIFNR = L_LIFNR.
    ENDIF.
* END OF HIS20094

    APPEND GT_OUT.
    CLEAR: GT_OUT, L_DUTY, L_FRG, L_OTH, L_WERTN.
    CLEAR FLAG.
  ENDLOOP.

ENDFORM.                    " GET_GT_OUT_FROM_GT_CKIS_N
*&---------------------------------------------------------------------*
*&      Form  LOCK_DATA
*&---------------------------------------------------------------------*
FORM LOCK_DATA CHANGING P_LV_CNT.
  CLEAR P_LV_CNT.
  PERFORM LOCK_UNLOCK USING 'X' CHANGING P_LV_CNT.
ENDFORM.                    " LOCK_DATA
*&---------------------------------------------------------------------*
*&      Form  UNLOCK_DATA
*&---------------------------------------------------------------------*
FORM UNLOCK_DATA CHANGING P_LV_CNT.
  CLEAR P_LV_CNT.
  PERFORM LOCK_UNLOCK USING '' CHANGING P_LV_CNT.
ENDFORM.                    " LOCK_DATA
*&---------------------------------------------------------------------*
*&      Form  lock_unlock
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1284   text
*----------------------------------------------------------------------*
FORM LOCK_UNLOCK USING P_LOCK CHANGING P_LV_CNT.
  DATA: LS_ZTCOU102 LIKE ZTCOU102,
        LT_ZTCOU102 TYPE TABLE OF ZTCOU102 WITH HEADER LINE.

  __CLS : GT_ROW, GT_ROID.

* Get selected rows
  CALL METHOD G_GRID1->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = GT_ROW
      ET_ROW_NO     = GT_ROID.


  LOOP AT GT_ROW INTO GS_ROW.
    READ TABLE GT_OUT INDEX GS_ROW-INDEX.

    PERFORM GET_ERR_CATEGORY USING    GT_OUT
                             CHANGING GT_OUT-EC_G
                                      GT_OUT-EC_A
                                      GT_OUT-EC_S
                                      GT_OUT-EC_V
                                      GT_OUT-EC_P
                                      GT_OUT-EC_R.

    PERFORM GT_OUT_STATUS_CHANGE.

*   no locking allowed due to error in record
    IF GT_OUT-EC_P EQ 'X' OR GT_OUT-EC_V EQ 'X' AND P_LOCK EQ 'X'.
      MESSAGE S000 WITH 'No locking is allowed due to error in record.'.
      MODIFY GT_OUT INDEX GS_ROW-INDEX.
    ELSE.
      GT_OUT-ZLOCK = P_LOCK.
      IF GT_OUT-ZLOCK EQ 'X'.
        GT_OUT-ICNLCK = ICON_LOCKED.
      ELSE.
        GT_OUT-ICNLCK = ''.
      ENDIF.

      __CLS GT_OUT-CELLTAB.
      MODIFY GT_OUT INDEX GS_ROW-INDEX.

      MOVE-CORRESPONDING GT_OUT TO LT_ZTCOU102.

** UD1K940725 by IG.MOON 6/26/2007
*      IF GV_CHK = 'X'.
*        READ TABLE GT_ZTCOU102 WITH KEY KALKA = GT_OUT-KALKA
*                                        MATNR = GT_OUT-MATNR
*                                        WERKS = GT_OUT-WERKS
*                                        BINARY SEARCH.
*        IF SY-SUBRC EQ 0.
*          IF GT_ZTCOU102-WERTN <> GT_OUT-WERTN.
*            LT_ZTCOU102-STAT = 'C'.
*            LT_ZTCOU102-AENAM = SY-UNAME.
*            LT_ZTCOU102-AEDAT = SY-DATUM.
*          ELSE.
*            LT_ZTCOU102-STAT = 'C'.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    IF GT_OUT-STAT NE SPACE.
*      IF GV_CHK = 'X' AND GT_OUT-STAT <> 'O' AND GT_OUT-STAT <> 'R'.
*        LT_ZTCOU102-STAT = 'C'.
*      ENDIF.
*    ENDIF.
* end of UD1K940725

      LT_ZTCOU102-MANDT = SY-MANDT.
*      LT_ZTCOU102-AEDAT = SY-DATUM.
*      LT_ZTCOU102-AENAM = SY-UNAME.
*      LT_ZTCOU102-CPUTM = SY-UZEIT.

      LT_ZTCOU102-ZLOCK = P_LOCK .

      APPEND LT_ZTCOU102.
      CLEAR LT_ZTCOU102.


*      MODIFY GT_OUT INDEX GS_ROW-INDEX
*         TRANSPORTING EC_G EC_A EC_S EC_D CELLTAB
*                      EC_V EC_P EC_R ZLOCK ICNLCK.
    ENDIF.

  ENDLOOP.

  P_LV_CNT = 0.

  LOOP AT LT_ZTCOU102 INTO LS_ZTCOU102.
    ADD 1 TO P_LV_CNT.
    MODIFY ZTCOU102 FROM LS_ZTCOU102.
  ENDLOOP.

  PERFORM BUILD_CELL_ATTR1.
  PERFORM READ_ZTCOU102_FROM_DB CHANGING SY-SUBRC.

*  IF L_CNT > 0.
*    MESSAGE S000 WITH L_CNT
*                      ' items are not locked due to price/vendor'.
*  ENDIF.

ENDFORM.                    " lock_unlock
*&---------------------------------------------------------------------*
*&      Form  BUILD_CELL_ATTR1_LOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_CELL_ATTR1_LOCK.

  DATA: LT_CELLTAB TYPE LVC_T_STYL,
        LS_CELLTAB TYPE LVC_S_STYL.

  CLEAR LT_CELLTAB.
  REFRESH LT_CELLTAB.

  __CLS GT_OUT-CELLTAB.
  MODIFY GT_OUT TRANSPORTING CELLTAB WHERE ZLOCK EQ 'X'.

  CLEAR GS_FCAT1.

  LOOP AT GT_FCAT1 INTO GS_FCAT1.
    LS_CELLTAB-FIELDNAME = GS_FCAT1-FIELDNAME.
    LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    INSERT LS_CELLTAB INTO TABLE LT_CELLTAB.
  ENDLOOP.

  INSERT LINES OF LT_CELLTAB INTO TABLE GT_OUT-CELLTAB.
  MODIFY GT_OUT TRANSPORTING CELLTAB WHERE ZLOCK EQ 'X'.

ENDFORM.                    " BUILD_CELL_ATTR1_LOCK
*&---------------------------------------------------------------------*
*&      Form  status_change
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GT_OUT_STATUS_CHANGE.

  GT_OUT-GRSPR = GT_OUT-WERTN +
                 GT_OUT-DUTY +
                 GT_OUT-FRG +
                 GT_OUT-OTH.

* change status; if amount is different
  CLEAR GT_ZTCOU102.
  READ TABLE GT_ZTCOU102 WITH KEY MATNR = GT_OUT-MATNR
       BINARY SEARCH.

*--new entry or refresh on unlocked item
  IF SY-SUBRC <> 0 OR ( P_REF1 EQ 'X' AND GT_ZTCOU102-ZLOCK = '' ).
    CLEAR: GT_OUT-STAT.
*    GT_OUT-AENAM = SY-UNAME.   " Changed by
*    GT_OUT-AEDAT = SY-DATUM.   " Changed on
*    GT_OUT-CPUTM = SY-UZEIT.
  ELSE.
    IF GT_ZTCOU102-STAT CA ' OR'
            AND ( GT_ZTCOU102-WERTN <> GT_OUT-WERTN
              OR  GT_ZTCOU102-GRSPR <> GT_OUT-GRSPR ).   "LDC change
      GT_OUT-STAT = 'C'.                  " Status
*      GT_OUT-AENAM = SY-UNAME.   " Changed by
*      GT_OUT-AEDAT = SY-DATUM.   " Changed on
*      GT_OUT-CPUTM = SY-UZEIT.
    ENDIF.
  ENDIF.

* lock status show
  IF GT_OUT-ZLOCK EQ 'X'.
    GT_OUT-ICNLCK = ICON_LOCKED.
  ELSE.
    GT_OUT-ICNLCK = ''.
  ENDIF.

  CHECK GT_OUT-ZLOCK = ''.

* Reason code change (LDC change; old data)
  IF GT_OUT-KZUST1 = 'ZLC' OR GT_OUT-KZUST1 IS INITIAL.
    IF GT_OUT-LAND1 <> 'US'.     "FIXME - ANDY
      IF GT_OUT-WERTN = GT_OUT-PWERTN.
        GT_OUT-KZUST1 = 'KE1'.
      ELSEIF GT_OUT-WERTN > GT_OUT-PWERTN.
        GT_OUT-KZUST1 = 'KU1'.
      ELSEIF GT_OUT-WERTN < GT_OUT-PWERTN.
        GT_OUT-KZUST1 = 'KD1'.
      ENDIF.
    ELSE.
      CLEAR GT_OUT-KZUST1.
    ENDIF.
  ENDIF.

  IF GT_OUT-KZUST1_IN IS INITIAL.
    GT_OUT-KZUST1_IN = GT_OUT-KZUST1.
  ENDIF.

*       Price Diff.
  GT_OUT-DIFF = GT_OUT-WERTN - GT_OUT-PWERTN.

*       Exception Statu
  PERFORM GET_ESTAT.

*       RS1$
  IF GT_OUT-WERTN1 = 0.
    GT_OUT-WERTN1 = GT_OUT-DIFF.
  ENDIF.


  GT_OUT-GPRC = GT_OUT-WERTN
              + GT_OUT-DUTY + GT_OUT-FRG + GT_OUT-OTH.

  IF GT_OUT-LIFNR <> SPACE.
    GT_OUT-QTA = 100.                       " Quota
  ENDIF.

* BEGIN OF UD1K954378
  IF GT_OUT-LAND1 = 'KR'.
    CLEAR: GT_OUT-DUTY.
  ENDIF.
* END OF UD1K954378

* BEGIN OF UD1K954962
  IF GT_OUT-KALKA = 'U1' OR
     GT_OUT-KALKA = 'BP'.                                   "UD1K955659
    READ TABLE GT_ZFTA_DUTY WITH KEY MATNR = GT_OUT-MATNR
                                 BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_OUT-DUTY = GT_OUT-WERTN * GT_ZFTA_DUTY-DUTY / 100.
    ENDIF.
  ENDIF.
* END OF UD1K954962
ENDFORM.                    " status_change
*&---------------------------------------------------------------------*
*&      Form  get_vendor
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_EINA  text
*      -->P_P_DATE  text
*----------------------------------------------------------------------*
FORM GET_VENDOR TABLES   P_EINA STRUCTURE GT_EINA
                USING    P_P_DATE P_LIFNR CLEAR_LIFNR.

  DATA $EINA LIKE P_EINA OCCURS 0 WITH HEADER LINE.

  READ TABLE P_EINA INDEX 1.
  CHECK SY-SUBRC EQ 0.

  IF NOT P_LIFNR IS INITIAL.
    LOOP AT P_EINA.
      IF P_EINA-LIFNR EQ P_LIFNR.
        $EINA = P_EINA.
        APPEND $EINA.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF NOT $EINA[] IS INITIAL.
      __CLS P_EINA.
      P_EINA[] = $EINA[].
      EXIT.
    ENDIF.

  ENDIF.

  SORT P_EINA BY URZDT KBETR.

  LOOP AT P_EINA.
    IF NOT P_EINA-URZDT IS INITIAL.
      IF P_P_DATE <= P_EINA-URZDT.
        $EINA = P_EINA.
        APPEND $EINA.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF NOT $EINA[] IS INITIAL.
    __CLS P_EINA.
    P_EINA[] = $EINA[].
    EXIT.
  ENDIF.

  SORT P_EINA BY LIFAB KBETR.
  LOOP AT P_EINA.
    IF NOT P_EINA-LIFAB IS INITIAL.
      IF P_P_DATE >= P_EINA-LIFAB.
        $EINA = P_EINA.
        APPEND $EINA.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF NOT $EINA[] IS INITIAL.
    __CLS P_EINA.
    P_EINA[] = $EINA[].
    EXIT.
  ENDIF.

  SORT P_EINA BY KBETR.
  LOOP AT P_EINA.
    IF NOT P_EINA-KBETR IS INITIAL.
      $EINA = P_EINA.
      APPEND $EINA.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF NOT $EINA[] IS INITIAL.
    __CLS P_EINA.
    P_EINA[] = $EINA[].
    EXIT.
  ENDIF.

ENDFORM.                    " get_vendor
*&---------------------------------------------------------------------*
*&      Form  get_info_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_018  text
*----------------------------------------------------------------------*
FORM GET_INFO_PRICE TABLES   P_LT_018 STRUCTURE GT_A018.
* USING f_matnr.

  DATA: BEGIN OF IT_INFO_CONDI OCCURS 0,
            KNUMH      LIKE KONH-KNUMH,
            KOPOS      LIKE KONP-KOPOS,
            KSCHL      LIKE KONP-KSCHL,
            KSCHL_KONH LIKE KONH-KSCHL,
*            VAKEY      LIKE KONH-VAKEY,
            DATAB      LIKE KONH-DATAB,
            DATBI      LIKE KONH-DATBI,
            KZUST      LIKE KONH-KZUST,
            KBETR      LIKE KONP-KBETR,
            KONWA      LIKE KONP-KONWA,
            KPEIN      LIKE KONP-KPEIN,  "!!!!
            KMEIN      LIKE KONP-KMEIN,  "!!!!
            KUMZA      LIKE KONP-KUMZA,
            KUMNE      LIKE KONP-KUMNE,
            MEINS      LIKE KONP-MEINS,
            LOEVM_KO   LIKE KONP-LOEVM_KO,
            LIFNR      LIKE KONP-LIFNR,
            KOSRT      LIKE KONH-KOSRT,
            ERNAM      LIKE KONH-ERNAM,
            ERDAT      LIKE KONH-ERDAT,
         END OF IT_INFO_CONDI.

  READ TABLE P_LT_018 INDEX 1.
  CHECK SY-SUBRC = 0.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_INFO_CONDI
    FROM KONH
       INNER JOIN KONP
          ON KONH~KNUMH = KONP~KNUMH
      FOR ALL ENTRIES IN P_LT_018
    WHERE KONP~KNUMH  = P_LT_018-KNUMH
      AND LOEVM_KO = ' '
      AND (
            KONP~KSCHL = 'PB00' OR
            KONP~KSCHL = 'ZTIR'
*            konp~kschl = 'ZP13' OR
*            konp~kschl = 'ZP16' OR
*            konp~kschl = 'ZP17' OR
*            konp~kschl = 'ZP18' OR
*            konp~kschl = 'FRA1' OR
*            konp~kschl = 'ZOA1' OR
*            konp~kschl = 'ZOTH' OR
*            konp~kschl = 'ZOTI'
           ) .

*- U1 Start
  IF P_ARCH EQ 'X'.
    PERFORM ARCHIVE_READ_KONP TABLES IT_INFO_CONDI P_LT_018.
  ENDIF.
*- U1 End

  DATA  $P_LT_018 LIKE P_LT_018 OCCURS 0 WITH HEADER LINE.
  DATA : $KPEIN LIKE P_LT_018-KPEIN,
         $KMEIN LIKE P_LT_018-KMEIN,
         $KONWA LIKE P_LT_018-KONWA,
         $KOSRT LIKE P_LT_018-KOSRT.

  LOOP AT IT_INFO_CONDI.
    $P_LT_018-KNUMH = IT_INFO_CONDI-KNUMH.
    $P_LT_018-MATNR = P_LT_018-MATNR.
*    $P_LT_018-KSCHL = IT_INFO_CONDI-KSCHL.
*    $P_LT_018-URZDT = P_LT_018-URZDT.
    $P_LT_018-LIFNR = P_LT_018-LIFNR.
    $P_LT_018-DATAB = P_LT_018-DATAB.
    $P_LT_018-DATBI = P_LT_018-DATBI.
    $P_LT_018-EKGRP = P_LT_018-EKGRP.
    $P_LT_018-KZUST = IT_INFO_CONDI-KZUST.

    CASE IT_INFO_CONDI-KSCHL.
      WHEN 'PB00'.
        $P_LT_018-KBETR1 = IT_INFO_CONDI-KBETR.
        $KPEIN = IT_INFO_CONDI-KPEIN.
        $KMEIN = IT_INFO_CONDI-KMEIN.
        $KONWA = IT_INFO_CONDI-KONWA.
        $KOSRT = IT_INFO_CONDI-KOSRT.
      WHEN 'ZTIR'.
        $P_LT_018-KBETR2 = IT_INFO_CONDI-KBETR.
*      WHEN 'ZP12'.
*        $p_lt_018-zp12 = it_info_condi-kbetr.
*      WHEN 'ZP13'.
*        $p_lt_018-zp13 = it_info_condi-kbetr.
*      WHEN 'ZP16'.
*        $p_lt_018-zp16 = it_info_condi-kbetr.
*      WHEN 'ZP17'.
*        $p_lt_018-zp17 = it_info_condi-kbetr.
*      WHEN 'ZP18'.
*        $p_lt_018-zp18 = it_info_condi-kbetr.
*      WHEN 'FRA1'.
*        $p_lt_018-fra1 = it_info_condi-kbetr.
*      WHEN 'ZOA1'.
*        $p_lt_018-zoa1 = it_info_condi-kbetr.
*      WHEN 'ZOTH'.
*        $p_lt_018-zoth = it_info_condi-kbetr.
*      WHEN 'ZOTI'.
*        $p_lt_018-zoti = it_info_condi-kbetr.
    ENDCASE.

    $P_LT_018-TKBETR = IT_INFO_CONDI-KBETR.

    COLLECT $P_LT_018.
    CLEAR $P_LT_018.
  ENDLOOP.



*FIXME - costing unit <> purchasing price unit!!!!
  DATA: $IX TYPE I.
  READ TABLE $P_LT_018 INDEX 1.

*??????????????????????????? ANDY
**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< FIXME why update GT_OUT???
*  IF gt_out-lifnr IS INITIAL.
*    gt_out-lifnr = $p_lt_018-lifnr.
*  ENDIF.
*  gt_out-kzust1 = $p_lt_018-kzust.   "KONH
**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< FIXME why update GT_OUT???


  $P_LT_018-KPEIN = $KPEIN.
  $P_LT_018-KMEIN = $KMEIN.
  $P_LT_018-KONWA = $KONWA.
  $P_LT_018-KOSRT = $KOSRT.

  MODIFY $P_LT_018 INDEX 1.
  P_LT_018[] = $P_LT_018[].

ENDFORM.                    " get_info_price
*&---------------------------------------------------------------------*
*&      Form  get_info_rec_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_018  text
*      -->P_P_DATE  text
*----------------------------------------------------------------------*
FORM GET_INFO_REC_NEW TABLES   LT_018 STRUCTURE GT_A018
                      USING    F_MATNR P_DATE
                               P_FORCE.

  DATA: LW_LIFNR TYPE LIFNR.
  DATA: CLEAR_LIFNR.
  DATA: $LIFNR TYPE LIFNR.

  RANGES R_LIFNR FOR A018-LIFNR.

* no info-found, find again!!!!
  DESCRIBE TABLE LT_018 LINES SY-TABIX.
  IF SY-TABIX = 0.
*---single vendor

    IF GT_OUT-LIFNR IS INITIAL.
      SELECT SINGLE LIFNR INTO GT_OUT-LIFNR
                   FROM ZTCOU137
                  WHERE BUKRS EQ P_KOKRS
                    AND MATNR EQ F_MATNR
                    AND ZDTFR <= P_DATE
                    AND ZDTTO >= P_DATE .
    ENDIF.

    IF GT_OUT-LIFNR NE SPACE.
      SELECT MATNR LIFNR DATBI DATAB KNUMH
        INTO CORRESPONDING FIELDS OF TABLE LT_018
        FROM A018
       WHERE KAPPL =  'M'
         AND KSCHL =  'PB00'
         AND LIFNR = GT_OUT-LIFNR
         AND MATNR = F_MATNR
         AND EKORG =  C_EKORG
         AND ESOKZ =  '0'
         AND DATAB =< P_DATE
         AND DATBI >= P_DATE.

    ELSE.
*---select possible vendors...
      SELECT LIFNR INTO LW_LIFNR
        FROM EINA
         WHERE MATNR = F_MATNR
           AND LOEKZ = SPACE.   " Not deleted
        R_LIFNR-SIGN = 'I'.
        R_LIFNR-OPTION = 'EQ'.
        R_LIFNR-LOW  = LW_LIFNR.
        APPEND R_LIFNR.
      ENDSELECT.

      IF SY-DBCNT = 0. EXIT. ENDIF.  "Fatal error....

      SELECT MATNR LIFNR DATBI DATAB KNUMH
        INTO CORRESPONDING FIELDS OF TABLE LT_018
        FROM A018
       WHERE KAPPL =  'M'
         AND KSCHL =  'PB00'
         AND MATNR = F_MATNR
         AND LIFNR IN R_LIFNR
         AND EKORG =  C_EKORG
         AND ESOKZ =  '0'
         AND DATAB =< P_DATE
         AND DATBI >= P_DATE.
    ENDIF.
  ENDIF.

*/// NOW WE HAVE LT_018 entry!!!
  DESCRIBE TABLE LT_018 LINES SY-TABIX.
  CHECK SY-TABIX > 0.


  __CLS GT_EINA.
  SELECT MATNR A~LIFNR EKGRP A~LIFAB A~LIFBI A~URZDT
    B~NETPR AS KBETR B~EKGRP A~LMEIN
    INTO CORRESPONDING FIELDS OF TABLE GT_EINA
    FROM EINA AS A
   INNER JOIN EINE AS B
      ON A~INFNR =  B~INFNR
      FOR ALL ENTRIES IN LT_018
   WHERE A~MATNR =  LT_018-MATNR
     AND A~LIFNR =  LT_018-LIFNR
     AND A~LOEKZ =  ' '
     AND B~WERKS =  ' '
     AND B~EKORG =  C_EKORG
     AND B~LOEKZ =  ' '.

  SORT GT_EINA BY MATNR LIFNR.
  DELETE ADJACENT DUPLICATES FROM GT_EINA COMPARING MATNR LIFNR.
  IF  SY-DBCNT > 1.

    SELECT SINGLE LIFNR INTO $LIFNR
       FROM EORD
       WHERE MATNR = F_MATNR
         AND BDATU >= P_DATE.

    PERFORM GET_VENDOR TABLES GT_EINA " <<<<< Vendor Determination.
                               USING P_DATE $LIFNR CLEAR_LIFNR.

    .

  ENDIF.
  READ TABLE GT_EINA INDEX 1.
  IF SY-SUBRC EQ 0.
    LOOP AT LT_018.
      IF LT_018-LIFNR <> GT_EINA-LIFNR.
        DELETE LT_018.
      ENDIF.
    ENDLOOP.
  ENDIF.

* RETURN VALUE!!!
*  lt_018-urzdt = gt_eina-urzdt.
  LT_018-EKGRP = GT_EINA-EKGRP.
  IF GT_OUT-WERTN IS INITIAL OR P_FORCE EQ 'X'.
    PERFORM GET_INFO_PRICE TABLES LT_018.   " USING f_matnr.
  ENDIF.


ENDFORM.                    " get_ldc_info_new
*&---------------------------------------------------------------------*
*&      Form  GET_ABP_LDC_GT_OUT_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_MATNR  text
*----------------------------------------------------------------------*
FORM GET_INFO_RECORD USING F_MATNR P_DATE.

  DATA LT_018   LIKE GT_A018 OCCURS 0  WITH HEADER LINE.

  READ TABLE GT_A018 INTO LT_018 WITH KEY MATNR = F_MATNR.
  IF SY-SUBRC = 0.
    APPEND LT_018.
  ENDIF.

  PERFORM GET_INFO_REC_NEW TABLES LT_018
                           USING F_MATNR P_DATE 'X'.

  PERFORM CHANGE_GT_OUT_WITH_LDC TABLES LT_018
                                  USING GT_OUT-MATNR.

ENDFORM.                    " GET_ABP_LDC_GT_OUT_new
*&---------------------------------------------------------------------*
*&      Form  change_gt_out_with_ldc
*&---------------------------------------------------------------------*
FORM CHANGE_GT_OUT_WITH_LDC TABLES P_A018 STRUCTURE GT_A018
                            USING F_MATNR.
*-don't change if record is locked.
  CHECK GT_OUT-ZLOCK = SPACE.
*-module costing will not consider LDC rate...!!! - ANDY
  CHECK S_KALKA-LOW <> 'M1'.

  READ TABLE P_A018 WITH KEY MATNR = F_MATNR BINARY SEARCH.
  CHECK SY-SUBRC = 0.

*-Net Price
  DATA: L_OUTPUT TYPE P DECIMALS 4 .
  PERFORM UNIT_CONVERION USING  P_A018-KPEIN  "p_input
                                P_A018-KMEIN "p_unit_in
                                GT_OUT-PMEHT  "p_unit_out
                      CHANGING  L_OUTPUT.

  GT_OUT-WERTN_V1 = GT_OUT-PEINH *
           ( P_A018-TKBETR / L_OUTPUT ).
  GT_OUT-WERTN = GT_OUT-WERTN_V1.

*-LDC cost & gross price
  CLEAR: GT_OUT-FRG, GT_OUT-OTH, GT_OUT-DUTY.
  IF P_LDC = 'X'.
    PERFORM GET_ABP_LDC_GT_OUT.
  ENDIF.

**- Gross Price
*  gt_out-gprc = gt_out-wertn + gt_out-duty + gt_out-frg + gt_out-oth.

ENDFORM.                    " change_gt_out_with_ldc
*&---------------------------------------------------------------------*
*&      Form  UNIT_CONVERION
*&---------------------------------------------------------------------*
*       Unit Conversion
*----------------------------------------------------------------------*
FORM UNIT_CONVERION USING    P_INPUT
                             P_UNIT_IN
                             P_UNIT_OUT
                    CHANGING P_OUTPUT.

  IF P_UNIT_IN = P_UNIT_OUT.
    P_OUTPUT = P_INPUT.
  ELSE.
    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
      EXPORTING
        INPUT                = P_INPUT
        UNIT_IN              = P_UNIT_IN
        UNIT_OUT             = P_UNIT_OUT
      IMPORTING
        OUTPUT               = P_OUTPUT
      EXCEPTIONS
        CONVERSION_NOT_FOUND = 1
        DIVISION_BY_ZERO     = 2
        INPUT_INVALID        = 3
        OUTPUT_INVALID       = 4
        OVERFLOW             = 5
        TYPE_INVALID         = 6
        UNITS_MISSING        = 7
        UNIT_IN_NOT_FOUND    = 8
        UNIT_OUT_NOT_FOUND   = 9
        OTHERS               = 10.
*    IF sy-subrc <> 0.
*      BREAK-POINT.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.


    IF SY-SUBRC <> 0.
*           ALT UoM
      DATA : L_UMREZ_F TYPE UMREZ,
             L_UMREZ_T TYPE UMREZ.

      CLEAR : L_UMREZ_F,L_UMREZ_T.

      SELECT SINGLE UMREZ UMREN INTO :
                (L_UMREZ_F, L_UMREZ_T) FROM MARM
               WHERE MATNR = GT_OUT-MATNR
               AND MEINH = P_UNIT_IN.

*                l_umrez_t FROM marm
*               WHERE matnr = gt_out-matnr
*               AND meinh = p_unit_out.

      IF L_UMREZ_F <> 0 AND  L_UMREZ_T <> 0.
        P_OUTPUT = P_INPUT * ( L_UMREZ_F / L_UMREZ_T ).
*        p_input = p_output.
*        p_unit_in = p_unit_out.
      ELSE.
* error
        P_OUTPUT = 1.
      ENDIF.
    ENDIF.

  ENDIF.
ENDFORM.                    " UNIT_CONVERION
*&---------------------------------------------------------------------*
*&      Form  refresh_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1233   text
*----------------------------------------------------------------------*
FORM REFRESH_SCREEN USING P_VALUE.

  __SET_REFRESH_MODE P_VALUE.
  CALL METHOD G_GRID1->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = STABLE.

ENDFORM.                    " refresh_screen
*&---------------------------------------------------------------------*
*&      Form  APPLY_INFO_REC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM APPLY_INFO_REC.

  CLEAR: GT_ROW[], GT_ROID[].

* Get selected rows
  CALL METHOD G_GRID1->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = GT_ROW
      ET_ROW_NO     = GT_ROID.
  DATA L_BESKZ TYPE BESKZ.

  LOOP AT GT_ROW INTO GS_ROW.
    READ TABLE GT_OUT INDEX GS_ROW-INDEX.

    IF SY-SUBRC = 0.
      PERFORM GET_INFO_RECORD USING GT_OUT-MATNR GT_OUT-BWDAT.
* by ig.moon 12/11 {
      CLEAR GT_OUT-KZUST1_IN.
* }
      PERFORM GT_OUT_STATUS_CHANGE.
      MODIFY GT_OUT INDEX GS_ROW-INDEX. " TRANSPORTING FRG OTH DUTY.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " APPLY_INFO_REC
*&---------------------------------------------------------------------*
*&      Form  convert_reason
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_KZUST1_IN  text
*      <--P_GT_OUT_$RSN  text
*----------------------------------------------------------------------*
FORM CONVERT_REASON USING    P_KZUST1_IN
                    CHANGING P_$RSN.
*  IF P_KZUST1_IN+0(1) EQ 'X'.
*    P_$RSN = P_KZUST1_IN.
*    EXIT.
*  ENDIF.
*
*  DATA $STRLEN TYPE I.
*
*  $STRLEN = STRLEN( P_KZUST1_IN ).
*  IF $STRLEN EQ '3'.
*    CONCATENATE P_KZUST1_IN+0(1) P_KZUST1_IN+2(1) INTO P_$RSN.
*    EXIT.
*  ENDIF.
*
*  P_$RSN = P_KZUST1_IN.

ENDFORM.                    " convert_reason
*&---------------------------------------------------------------------*
*&      Form  read_ztcou102_from_db
*&---------------------------------------------------------------------*
FORM READ_ZTCOU102_FROM_DB CHANGING P_SY_SUBRC.
  REFRESH GT_ZTCOU102.
  SELECT A~KOKRS A~BDATJ A~POPER A~KALKA A~KADKY A~VER A~MATNR
         A~WERKS A~ZLOCK A~STAT
         A~MTART A~BWDAT A~ALDAT A~EKGRP A~PROFL A~BKLAS
         A~INFNR A~STAWN A~VERPR A~STPRS A~PMEHT
         A~WERTN A~PEINH A~PWERTN A~GRSPR
         A~LIFNR A~QTA A~WERTN_V1 A~DUTY A~FRG A~OTH A~KZUST1
         A~WERTN1 A~KZUST2 A~WERTN2 A~LIFNR2 A~QTA_V2 A~WERTN_V2
         A~KZUST1_V2 A~WERTN1_V2 A~KZUST2_V2 A~WERTN2_V2
         A~EC_G A~EC_A A~EC_S A~EC_D A~EC_V A~EC_P A~EC_R
         A~RSTAT A~AEDAT A~AENAM A~CPUTM
         A~ZTEXT
         B~MAKTG C~STLAN D~MSTAE C~DISPO C~MMSTA
         E~LAND1 E~NAME1 D~MATKL
         A~STRAT A~SUBSTRAT A~ELEMT A~ELEMTNS
         A~PRICE_MANUAL

    INTO CORRESPONDING FIELDS OF TABLE GT_ZTCOU102
    FROM ZTCOU102 AS A
    JOIN MAKT AS B
      ON B~MATNR = A~MATNR
     AND B~SPRAS = SY-LANGU
    JOIN MARC AS C
      ON C~MATNR = A~MATNR
     AND C~WERKS = A~WERKS
    JOIN MARA AS D
      ON D~MATNR = A~MATNR
    LEFT JOIN LFA1 AS E
      ON E~LIFNR = A~LIFNR
   WHERE BDATJ = P_YEAR
     AND POPER = P_POPER
     AND KALKA IN S_KALKA
     AND A~MATNR IN S_MATNR
     AND A~LIFNR IN S_LIFNR
     AND C~EKGRP IN S_EKGRP
     AND D~MATKL IN S_MATKL.

  P_SY_SUBRC = SY-SUBRC.

  PERFORM RESTRICT_102_WITH_PRODUCT.

  SORT GT_ZTCOU102 BY MATNR.

ENDFORM.                    " read_ztcou102_from_db
*&---------------------------------------------------------------------*
*&      Form  restrict_102_with_product
*&---------------------------------------------------------------------*
FORM RESTRICT_102_WITH_PRODUCT.

  DESCRIBE TABLE S_ARTNR LINES SY-TABIX.
  CHECK SY-TABIX > 0.

  DATA: BEGIN OF LT_CKIS OCCURS 0,
          ARTNR  LIKE KEKO-MATNR,
          MATNR  LIKE CKIS-MATNR,
        END OF LT_CKIS.
  DATA: L_IDX LIKE SY-TABIX.

  SELECT A~MATNR C~MATNR
    INTO TABLE LT_CKIS
    FROM CKMLHD AS B
    JOIN KEKO AS A
      ON B~KALNR = A~KALNR
    JOIN CKIS AS C
      ON C~LEDNR = C_LEDNR
     AND C~BZOBJ = A~BZOBJ
     AND C~KALNR = A~KALNR
     AND C~KALKA = A~KALKA
     AND C~KADKY = A~KADKY
     AND C~TVERS = A~TVERS
     AND C~BWVAR = A~BWVAR
     AND C~KKZMA = A~KKZMA
   WHERE B~MATNR IN S_ARTNR
     AND B~BWKEY IN R_BWKEY
     AND A~BZOBJ = C_BZOBJ
     AND A~KALKA IN S_KALKA
     AND A~KADKY = GV_KADKY
     AND A~TVERS = GC_TVERS
     AND A~BDATJ = P_YEAR
     AND A~POPER = P_POPER.

*- U1 Start
  IF P_ARCH EQ 'X'.
    PERFORM ARCHIVE_READ_CKIS_2 TABLES LT_CKIS.
  ENDIF.
*- U1 End

  SORT LT_CKIS BY MATNR.
  LOOP AT GT_ZTCOU102.
    L_IDX = SY-TABIX.
    READ TABLE LT_CKIS WITH KEY MATNR = GT_ZTCOU102-MATNR
         BINARY SEARCH.
    IF SY-SUBRC <> 0.
      DELETE GT_ZTCOU102 INDEX L_IDX.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " restrict_102_with_product
*&---------------------------------------------------------------------*
*&      Form  get_vendor_fr_gt_out
*&---------------------------------------------------------------------*
FORM GET_VENDOR_FR_GT_OUT.

  CLEAR GT_LFA1.
  REFRESH GT_LFA1.

  SELECT LIFNR NAME1 INTO TABLE GT_LFA1
    FROM LFA1
     FOR ALL ENTRIES IN GT_OUT
   WHERE LIFNR = GT_OUT-LIFNR2.

  SORT GT_LFA1 BY LIFNR.

ENDFORM.                    " get_vendor_fr_gt_out
*&---------------------------------------------------------------------*
*&      Form  GET_SET_STATUS
*&---------------------------------------------------------------------*
FORM GET_SET_STATUS.
  CHECK G_ERROR NE 'X'.

* Get error category when choose [Refresh]
  IF P_REF1 = 'X'.
    LOOP AT GT_OUT.
      G_IDX = SY-TABIX.
*     Error Category
      PERFORM GET_ERR_CATEGORY USING    GT_OUT
                               CHANGING GT_OUT-EC_G
                                        GT_OUT-EC_A
                                        GT_OUT-EC_S
                                        GT_OUT-EC_V
                                        GT_OUT-EC_P
                                        GT_OUT-EC_R.
      MODIFY GT_OUT INDEX G_IDX.
    ENDLOOP.
  ENDIF.

* Case of choose [Error only, Changed only]
  IF P_ALL = SPACE.
    REFRESH GT_OUT1.

    LOOP AT GT_OUT.
      G_IDX = SY-TABIX.

      IF GT_OUT-EC_G = SPACE AND
         GT_OUT-EC_A = SPACE AND
         GT_OUT-EC_S = SPACE AND
         GT_OUT-EC_D = SPACE AND
         GT_OUT-EC_V = SPACE AND
         GT_OUT-EC_P = SPACE AND
         GT_OUT-EC_R = SPACE.

        APPEND GT_OUT TO GT_OUT1.
        DELETE GT_OUT INDEX G_IDX.
      ENDIF.
    ENDLOOP.
  ENDIF.

* Case of [Exception]
  IF ( P_CP <> SPACE OR P_CP <> '00' ) AND
     P_EXPT = 'X'.
    DELETE GT_OUT WHERE ESTAT = SPACE OR
                        ESTAT = ICON_LED_GREEN.
  ENDIF.

ENDFORM.                    " GET_SET_STATUS
*&---------------------------------------------------------------------*
*&      Form  GET_GT_OUT_OTHERS
*&---------------------------------------------------------------------*
FORM GET_GT_OUT_OTHERS.
  DESCRIBE TABLE GT_OUT LINES SY-TABIX.
  CHECK SY-TABIX > 0.

* Vendor Info.
  PERFORM READ_VENDOR_DATA.

* Get Material Information
  PERFORM READ_MAT_INFO.

* Get info-record to determine blank vendor

*<<<<<<<<<<<<< Vendor Determination >>>>>>>>>>>>>>>>>>>>>>>>>>>
  IF P_REF1 = 'X'.

    PERFORM GET_GT_A018. " <<<<<<< -- UD1K941194
  ENDIF.
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


  SORT: GT_OUT BY KALKA MATNR WERKS,
        GT_ZTCOU102 BY KALKA MATNR WERKS,
        GT_A018 BY MATNR LIFNR.

  DATA $IX LIKE SY-TABIX.

* PUT OTHER INFORMATION

  LOOP AT GT_OUT.
    $IX = SY-TABIX.
    CHECK GT_OUT-ZLOCK NE 'X'.

    PERFORM MODIFY_MAT_INFO.

    PERFORM READ_GT_108.

*   Current & Previous info-price
    PERFORM PUT_CUR_PRV_INFO_PRICE.

*   Reason (current period)
    PERFORM GET_REASON.
    GT_OUT-KZUST1_IN = GT_OUT-KZUST1.

    GT_OUT-WERTN1 = GT_OUT-WERTN - GT_OUT-PWERTN.

    PERFORM CHANGE_GT_OUT_WITH_LDC TABLES GT_A018
                                   USING GT_OUT-MATNR.
*   PERFORM APPLY_LDC_RATE_GT_OUT.

* derive other attribute
    IF NOT GT_OUT-LIFNR IS INITIAL.
      READ TABLE GT_VENDOR WITH KEY LIFNR = GT_OUT-LIFNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        GT_OUT-LAND1 = GT_VENDOR-LAND1.
        GT_OUT-NAME1 = GT_VENDOR-NAME1.
      ENDIF.
    ENDIF.

* UD1K941202 - by IG.MOON 8/2/2007 {
    PERFORM GT_OUT_STATUS_CHANGE.
* }

    MODIFY GT_OUT INDEX $IX.
*           TRANSPORTING STPRS VERPR PEINH BKLAS
*                               MAKTG MTART PROFL MSTAE
*                               EKGRP STLAN STAWN DISPO
*                               MMSTA MATKL.

  ENDLOOP.

ENDFORM.                    " GET_GT_OUT_OTHERS
*&---------------------------------------------------------------------*
*&      Form  change_vendor_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHANGE_VENDOR_INFO  USING RR_DATA_CHANGED TYPE REF TO
                               CL_ALV_CHANGED_DATA_PROTOCOL
                               LS_MOD_CELLS    TYPE LVC_S_MODI.
  DATA: LV_NAME1 TYPE NAME1_GP,
        L_LAND1  TYPE LAND1_GP.

  CALL METHOD RR_DATA_CHANGED->MODIFY_CELL
    EXPORTING
      I_ROW_ID    = LS_MOD_CELLS-ROW_ID
      I_FIELDNAME = LS_MOD_CELLS-FIELDNAME
      I_VALUE     = LS_MOD_CELLS-VALUE.

* Vedor Description
  CLEAR: LV_NAME1, L_LAND1.

  SELECT SINGLE NAME1 LAND1
    INTO (LV_NAME1, L_LAND1)
    FROM LFA1
   WHERE LIFNR = LS_MOD_CELLS-VALUE.

  IF SY-SUBRC = 0.
    CALL METHOD RR_DATA_CHANGED->MODIFY_CELL
      EXPORTING
        I_ROW_ID    = LS_MOD_CELLS-ROW_ID
        I_FIELDNAME = 'NAME1'
        I_VALUE     = LV_NAME1.

    CALL METHOD RR_DATA_CHANGED->MODIFY_CELL
      EXPORTING
        I_ROW_ID    = LS_MOD_CELLS-ROW_ID
        I_FIELDNAME = 'LAND1'
        I_VALUE     = L_LAND1.
  ENDIF.

ENDFORM.                    " change_vendor_info
*&---------------------------------------------------------------------*
*&      Form  read_gt_108
*&---------------------------------------------------------------------*
FORM READ_GT_108.

  CLEAR GT_A018.

  IF GT_OUT-LIFNR IS INITIAL.
    READ TABLE GT_A018 WITH KEY MATNR = GT_OUT-MATNR BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GT_OUT-LIFNR = GT_A018-LIFNR.
    ENDIF.
  ENDIF.

  READ TABLE GT_A018 WITH KEY MATNR = GT_OUT-MATNR
                              LIFNR = GT_OUT-LIFNR BINARY SEARCH.

  IF SY-SUBRC <> 0.
    DATA L_FMATNR TYPE MATNR.

*---[CO] Spec Change Info;;; FIXME LATER
    CLEAR L_FMATNR.
    SELECT SINGLE FMATNR INTO L_FMATNR FROM ZTCOU105
       WHERE KOKRS  = P_KOKRS
         AND TMATNR = GT_OUT-MATNR.

    IF SY-SUBRC = 0.
      READ TABLE GT_A018 WITH KEY MATNR = L_FMATNR BINARY SEARCH.
*                                  lifnr = gt_out-lifnr BINARY SEARCH.
    ENDIF.

  ENDIF.

ENDFORM.                    " read_gt_108
*&---------------------------------------------------------------------*
*&      Form  get_mat_level
*&---------------------------------------------------------------------*
*       Get material level
*----------------------------------------------------------------------*
FORM GET_MAT_LEVEL.
  SELECT SINGLE ZTCO_UPG~MATNR ZTCO_UPG~WRKTS
                ZTCO_UPG~MAKTX ZTCO_UPG~ZCATX
    INTO (GT_OUT-MATNR2, GT_OUT-MATNR1, GT_OUT-MAKTX, GT_OUT-ZCATX)
    FROM MARA JOIN ZTCO_UPG
                ON MARA~WRKST = ZTCO_UPG~MATNR
   WHERE MARA~MATNR = GT_OUT-MATNR.
ENDFORM.                    " get_mat_level
* BEGIN OF UD1K954962
*&---------------------------------------------------------------------*
*&      Form  GET_DUTY_RATIO
*&---------------------------------------------------------------------*
*       Get duty ratio override data
*----------------------------------------------------------------------*
FORM GET_DUTY_RATIO.
  SELECT * INTO TABLE GT_ZFTA_DUTY
    FROM ZFTA_DUTY.

  SORT GT_ZFTA_DUTY BY MATNR.
ENDFORM.                    " GET_DUTY_RATIO
* END OF UD1K954962
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_KONH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ARCHIVE_READ_KONH .

  TYPES: BEGIN OF TY_KONH,
         KNUMH    TYPE KNUMH,
         KOPOS    TYPE KOPOS,
         KAPPL    TYPE KAPPL,
         KSCHL    TYPE KSCHA,
         LOEVM_KO TYPE LOEVM_KO,
           ARCHIVEKEY TYPE ARKEY,
           ARCHIVEOFS TYPE ADMI_OFFST.
  TYPES: END OF TY_KONH.

  DATA: L_HANDLE    TYPE SYTABIX,
        LT_KONH     TYPE TABLE OF KONH WITH HEADER LINE,
        L_ARCHINDEX LIKE AIND_STR2-ARCHINDEX,
        L_GENTAB    LIKE AIND_STR2-GENTAB.

  DATA: LT_INX_KONH TYPE TABLE OF TY_KONH,
        LS_INX_KONH TYPE TY_KONH.

* 1. Input the archive infostructure name
  CLEAR L_ARCHINDEX.
  L_ARCHINDEX = 'ZKONP_002'.

* 2. Get the structure table using infostructure
  CLEAR L_GENTAB.
  SELECT SINGLE GENTAB INTO L_GENTAB FROM AIND_STR2
   WHERE ARCHINDEX = L_ARCHINDEX.

  CHECK SY-SUBRC = 0 AND NOT L_GENTAB IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR LT_INX_KONH[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_INX_KONH
    FROM (L_GENTAB)
   WHERE KNUMH = GT_A018-KNUMH.

  CHECK NOT LT_INX_KONH[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: GT_KONH_A, GT_KONH_A[].
  LOOP AT LT_INX_KONH INTO LS_INX_KONH.
*  4.1 Read information from archivekey & offset
    CLEAR L_HANDLE.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        OBJECT                    = 'MM_EKKO'
        ARCHIVKEY                 = LS_INX_KONH-ARCHIVEKEY
        OFFSET                    = LS_INX_KONH-ARCHIVEOFS
      IMPORTING
        ARCHIVE_HANDLE            = L_HANDLE
      EXCEPTIONS
        NO_RECORD_FOUND           = 1
        FILE_IO_ERROR             = 2
        INTERNAL_ERROR            = 3
        OPEN_ERROR                = 4
        CANCELLED_BY_USER         = 5
        ARCHIVELINK_ERROR         = 6
        OBJECT_NOT_FOUND          = 7
        FILENAME_CREATION_FAILURE = 8
        FILE_ALREADY_OPEN         = 9
        NOT_AUTHORIZED            = 10
        FILE_NOT_FOUND            = 11
        ERROR_MESSAGE             = 12
        OTHERS                    = 13.

    CHECK SY-SUBRC = 0.

*  4.2 Read table from information
    CLEAR: LT_KONH, LT_KONH[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        ARCHIVE_HANDLE          = L_HANDLE
        RECORD_STRUCTURE        = 'KONH'
        ALL_RECORDS_OF_OBJECT   = 'X'
      TABLES
        TABLE                   = LT_KONH
      EXCEPTIONS
        END_OF_OBJECT           = 1
        INTERNAL_ERROR          = 2
        WRONG_ACCESS_TO_ARCHIVE = 3
        OTHERS                  = 4.

    CHECK SY-SUBRC = 0 AND NOT LT_KONH[] IS INITIAL.

    READ TABLE LT_KONH INDEX 1.
    CHECK SY-SUBRC = 0.
    GT_OUT-KZUST1 = LT_KONH-KZUST.
    EXIT.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_KONH
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_KONH_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LS_XKONH  text
*----------------------------------------------------------------------*
FORM ARCHIVE_READ_KONH_2 USING P_KNUMH CHANGING PS_XKONH STRUCTURE KONH.

  TYPES: BEGIN OF TY_KONH,
         KNUMH    TYPE KNUMH,
         KOPOS    TYPE KOPOS,
         KAPPL    TYPE KAPPL,
         KSCHL    TYPE KSCHA,
         LOEVM_KO TYPE LOEVM_KO,
           ARCHIVEKEY TYPE ARKEY,
           ARCHIVEOFS TYPE ADMI_OFFST.
  TYPES: END OF TY_KONH.

  DATA: L_HANDLE    TYPE SYTABIX,
        LT_KONH     TYPE TABLE OF KONH WITH HEADER LINE,
        L_ARCHINDEX LIKE AIND_STR2-ARCHINDEX,
        L_GENTAB    LIKE AIND_STR2-GENTAB.

  DATA: LT_INX_KONH TYPE TABLE OF TY_KONH,
        LS_INX_KONH TYPE TY_KONH.

* 1. Input the archive infostructure name
  CLEAR L_ARCHINDEX.
  L_ARCHINDEX = 'ZKONP_002'.

* 2. Get the structure table using infostructure
  CLEAR L_GENTAB.
  SELECT SINGLE GENTAB INTO L_GENTAB FROM AIND_STR2
   WHERE ARCHINDEX = L_ARCHINDEX.

  CHECK SY-SUBRC = 0 AND NOT L_GENTAB IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR LT_INX_KONH[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_INX_KONH
    FROM (L_GENTAB)
   WHERE KNUMH = P_KNUMH.

  CHECK NOT LT_INX_KONH[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: GT_KONH_A, GT_KONH_A[].
  LOOP AT LT_INX_KONH INTO LS_INX_KONH.
*  4.1 Read information from archivekey & offset
    CLEAR L_HANDLE.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        OBJECT                    = 'MM_EKKO'
        ARCHIVKEY                 = LS_INX_KONH-ARCHIVEKEY
        OFFSET                    = LS_INX_KONH-ARCHIVEOFS
      IMPORTING
        ARCHIVE_HANDLE            = L_HANDLE
      EXCEPTIONS
        NO_RECORD_FOUND           = 1
        FILE_IO_ERROR             = 2
        INTERNAL_ERROR            = 3
        OPEN_ERROR                = 4
        CANCELLED_BY_USER         = 5
        ARCHIVELINK_ERROR         = 6
        OBJECT_NOT_FOUND          = 7
        FILENAME_CREATION_FAILURE = 8
        FILE_ALREADY_OPEN         = 9
        NOT_AUTHORIZED            = 10
        FILE_NOT_FOUND            = 11
        ERROR_MESSAGE             = 12
        OTHERS                    = 13.

    CHECK SY-SUBRC = 0.

*  4.2 Read table from information
    CLEAR: LT_KONH, LT_KONH[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        ARCHIVE_HANDLE          = L_HANDLE
        RECORD_STRUCTURE        = 'KONH'
        ALL_RECORDS_OF_OBJECT   = 'X'
      TABLES
        TABLE                   = LT_KONH
      EXCEPTIONS
        END_OF_OBJECT           = 1
        INTERNAL_ERROR          = 2
        WRONG_ACCESS_TO_ARCHIVE = 3
        OTHERS                  = 4.

    CHECK SY-SUBRC = 0 AND NOT LT_KONH[] IS INITIAL.

    READ TABLE LT_KONH INDEX 1.
    CHECK SY-SUBRC = 0.
    CLEAR PS_XKONH.
    MOVE-CORRESPONDING LT_KONH TO PS_XKONH.
    EXIT.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_KONH_2
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_KONP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_INFO_CONDI  text
*----------------------------------------------------------------------*
FORM ARCHIVE_READ_KONP  TABLES   PT_INFO_CONDI STRUCTURE GT_INFO_CONDI_A
                                 PT_LT_018     STRUCTURE GT_A018.

  TYPES: BEGIN OF TY_KONP,
         KNUMH    TYPE KNUMH,
         KOPOS    TYPE KOPOS,
         KAPPL    TYPE KAPPL,
         KSCHL    TYPE KSCHA,
         LOEVM_KO TYPE LOEVM_KO,
           ARCHIVEKEY TYPE ARKEY,
           ARCHIVEOFS TYPE ADMI_OFFST.
  TYPES: END OF TY_KONP.

  DATA: L_HANDLE    TYPE SYTABIX,
        LT_KONH     TYPE TABLE OF KONH WITH HEADER LINE,
        LT_KONP     TYPE TABLE OF KONH WITH HEADER LINE,
        L_ARCHINDEX LIKE AIND_STR2-ARCHINDEX,
        L_GENTAB    LIKE AIND_STR2-GENTAB.

  DATA: LT_INX_KONP TYPE TABLE OF TY_KONP,
        LS_INX_KONP TYPE TY_KONP.

* 1. Input the archive infostructure name
  CLEAR L_ARCHINDEX.
  L_ARCHINDEX = 'ZKONP_002'.

* 2. Get the structure table using infostructure
  CLEAR L_GENTAB.
  SELECT SINGLE GENTAB INTO L_GENTAB FROM AIND_STR2
   WHERE ARCHINDEX = L_ARCHINDEX.

  CHECK SY-SUBRC = 0 AND NOT L_GENTAB IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR LT_INX_KONP[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_INX_KONP
    FROM (L_GENTAB)
    FOR ALL ENTRIES IN PT_LT_018
   WHERE KNUMH  = PT_LT_018-KNUMH
     AND LOEVM_KO = ' '
     AND (
           KSCHL = 'PB00' OR
           KSCHL = 'ZTIR'
*           konp~kschl = 'ZP13' OR
*           konp~kschl = 'ZP16' OR
*           konp~kschl = 'ZP17' OR
*           konp~kschl = 'ZP18' OR
*           konp~kschl = 'FRA1' OR
*           konp~kschl = 'ZOA1' OR
*           konp~kschl = 'ZOTH' OR
*           konp~kschl = 'ZOTI'
           ).

  CHECK NOT LT_INX_KONP[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: GT_KONH_A, GT_KONH_A[], GT_KONP_A, GT_KONP_A[].
  LOOP AT LT_INX_KONP INTO LS_INX_KONP.
*  4.1 Read information from archivekey & offset
    CLEAR L_HANDLE.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        OBJECT                    = 'MM_EKKO'
        ARCHIVKEY                 = LS_INX_KONP-ARCHIVEKEY
        OFFSET                    = LS_INX_KONP-ARCHIVEOFS
      IMPORTING
        ARCHIVE_HANDLE            = L_HANDLE
      EXCEPTIONS
        NO_RECORD_FOUND           = 1
        FILE_IO_ERROR             = 2
        INTERNAL_ERROR            = 3
        OPEN_ERROR                = 4
        CANCELLED_BY_USER         = 5
        ARCHIVELINK_ERROR         = 6
        OBJECT_NOT_FOUND          = 7
        FILENAME_CREATION_FAILURE = 8
        FILE_ALREADY_OPEN         = 9
        NOT_AUTHORIZED            = 10
        FILE_NOT_FOUND            = 11
        ERROR_MESSAGE             = 12
        OTHERS                    = 13.

    CHECK SY-SUBRC = 0.

*  4.2 Read table from information
    CLEAR: LT_KONH, LT_KONH[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        ARCHIVE_HANDLE          = L_HANDLE
        RECORD_STRUCTURE        = 'KONH'
        ALL_RECORDS_OF_OBJECT   = 'X'
      TABLES
        TABLE                   = LT_KONH
      EXCEPTIONS
        END_OF_OBJECT           = 1
        INTERNAL_ERROR          = 2
        WRONG_ACCESS_TO_ARCHIVE = 3
        OTHERS                  = 4.

    CHECK SY-SUBRC = 0 AND NOT LT_KONH[] IS INITIAL.

*  4.2 Read table from information
    CLEAR: LT_KONP, LT_KONP[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        ARCHIVE_HANDLE          = L_HANDLE
        RECORD_STRUCTURE        = 'KONP'
        ALL_RECORDS_OF_OBJECT   = 'X'
      TABLES
        TABLE                   = LT_KONP
      EXCEPTIONS
        END_OF_OBJECT           = 1
        INTERNAL_ERROR          = 2
        WRONG_ACCESS_TO_ARCHIVE = 3
        OTHERS                  = 4.

    CHECK SY-SUBRC = 0 AND NOT LT_KONP[] IS INITIAL.

    INSERT LINES OF: LT_KONH INTO TABLE GT_KONH_A,
                     LT_KONP INTO TABLE GT_KONP_A.
  ENDLOOP.

  SORT: GT_KONH_A, GT_KONP_A.
  DELETE ADJACENT DUPLICATES FROM: GT_KONH_A COMPARING ALL FIELDS,
                                   GT_KONP_A COMPARING ALL FIELDS.

  LOOP AT GT_KONP_A.
    MOVE-CORRESPONDING GT_KONP_A TO PT_INFO_CONDI.

    CLEAR GT_KONP_A.
    READ TABLE GT_KONP_A WITH KEY KNUMH = GT_KONP_A-KNUMH.
    CHECK SY-SUBRC = 0.
    MOVE-CORRESPONDING GT_KONP_A TO PT_INFO_CONDI.

    APPEND PT_INFO_CONDI.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_KONP
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_ZTCOU103
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ARCHIVE_READ_ZTCOU103 CHANGING P_MATNR.

  TYPES: BEGIN OF TY_ZTCOU103,
         KOKRS TYPE KOKRS,
         BDATJ TYPE BDATJ,
         KALKA TYPE CK_KALKA,
         POPER TYPE POPER,
         ARTNR TYPE ARTNR,
         VER   TYPE ZVER1,
         WERKS TYPE WERKS_D,
         INDX  TYPE ZINDEX,
           ARCHIVEKEY TYPE ARKEY,
           ARCHIVEOFS TYPE ADMI_OFFST.
  TYPES: END OF TY_ZTCOU103.

  DATA: L_HANDLE    TYPE SYTABIX,
        LT_ZTCOU103     TYPE TABLE OF ZTCOU103 WITH HEADER LINE,
        L_ARCHINDEX LIKE AIND_STR2-ARCHINDEX,
        L_GENTAB    LIKE AIND_STR2-GENTAB.

  DATA: LT_INX_ZTCOU103 TYPE TABLE OF TY_ZTCOU103,
        LS_INX_ZTCOU103 TYPE TY_ZTCOU103.

* 1. Input the archive infostructure name
  CLEAR L_ARCHINDEX.
  L_ARCHINDEX = 'ZTCOU103_001'.

* 2. Get the structure table using infostructure
  CLEAR L_GENTAB.
  SELECT SINGLE GENTAB INTO L_GENTAB FROM AIND_STR2
   WHERE ARCHINDEX = L_ARCHINDEX.

  CHECK SY-SUBRC = 0 AND NOT L_GENTAB IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR LT_INX_ZTCOU103[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_INX_ZTCOU103
    FROM (L_GENTAB)
   WHERE BDATJ = P_YEAR
     AND POPER = P_POPER
     AND COMPN = GT_OUT-MATNR.

  CHECK NOT LT_INX_ZTCOU103[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: GT_ZTCOU103_A, GT_ZTCOU103_A[].
  LOOP AT LT_INX_ZTCOU103 INTO LS_INX_ZTCOU103.
*  4.1 Read information from archivekey & offset
    CLEAR L_HANDLE.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        OBJECT                    = 'ZTCOU103'
        ARCHIVKEY                 = LS_INX_ZTCOU103-ARCHIVEKEY
        OFFSET                    = LS_INX_ZTCOU103-ARCHIVEOFS
      IMPORTING
        ARCHIVE_HANDLE            = L_HANDLE
      EXCEPTIONS
        NO_RECORD_FOUND           = 1
        FILE_IO_ERROR             = 2
        INTERNAL_ERROR            = 3
        OPEN_ERROR                = 4
        CANCELLED_BY_USER         = 5
        ARCHIVELINK_ERROR         = 6
        OBJECT_NOT_FOUND          = 7
        FILENAME_CREATION_FAILURE = 8
        FILE_ALREADY_OPEN         = 9
        NOT_AUTHORIZED            = 10
        FILE_NOT_FOUND            = 11
        ERROR_MESSAGE             = 12
        OTHERS                    = 13.

    CHECK SY-SUBRC = 0.

*  4.2 Read table from information
    CLEAR: LT_ZTCOU103, LT_ZTCOU103[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        ARCHIVE_HANDLE          = L_HANDLE
        RECORD_STRUCTURE        = 'ZTCOU103'
        ALL_RECORDS_OF_OBJECT   = 'X'
      TABLES
        TABLE                   = LT_ZTCOU103
      EXCEPTIONS
        END_OF_OBJECT           = 1
        INTERNAL_ERROR          = 2
        WRONG_ACCESS_TO_ARCHIVE = 3
        OTHERS                  = 4.

    CHECK SY-SUBRC = 0 AND NOT LT_ZTCOU103[] IS INITIAL.

* 5. Append archived data table to finally interal table
    INSERT LINES OF LT_ZTCOU103 INTO TABLE GT_ZTCOU103_A.
  ENDLOOP.

  SORT GT_ZTCOU103_A.
  DELETE ADJACENT DUPLICATES FROM GT_ZTCOU103_A COMPARING ALL FIELDS.

  CLEAR GT_ZTCOU103_A.
  READ TABLE GT_ZTCOU103_A INDEX 1.
  P_MATNR = GT_ZTCOU103_A-COMPN.

  READ TABLE GT_ZTCOU103_A INDEX 1.

ENDFORM.                    " ARCHIVE_READ_ZTCOU103
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_KEKO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_8786   text
*----------------------------------------------------------------------*
FORM ARCHIVE_READ_KEKO TABLES PT_KEKO STRUCTURE KEKO
                        USING P_FLAG.

  TYPES: BEGIN OF TY_KEKO,
         BZOBJ   TYPE CK_OBJ,
         KALNR   TYPE CK_KALNR1,
         KALKA   TYPE CK_KALKA,
         KADKY   TYPE CK_KADKY,
         TVERS   TYPE CK_TVERS,
         BWVAR   TYPE CK_BWVAR,
         KKZMA   TYPE CK_KKZMA,
         WERKS   TYPE WERKS_D,
         STLAN   TYPE STLAN,
         KALAID  TYPE CK_KALAID,
         KALADAT TYPE CK_KALADAT,
         BDATJ   TYPE BDATJ,
         POPER   TYPE POPER,
         BWSMR   TYPE CK_BWSMR,
           ARCHIVEKEY TYPE ARKEY,
           ARCHIVEOFS TYPE ADMI_OFFST.
  TYPES: END OF TY_KEKO.

  DATA: L_HANDLE    TYPE SYTABIX,
        LT_KEKO     TYPE TABLE OF KEKO WITH HEADER LINE,
        L_ARCHINDEX LIKE AIND_STR2-ARCHINDEX,
        L_GENTAB    LIKE AIND_STR2-GENTAB.

  DATA: LT_INX_KEKO TYPE TABLE OF TY_KEKO,
        LS_INX_KEKO TYPE TY_KEKO.

* 1. Input the archive infostructure name
  CLEAR L_ARCHINDEX.
  L_ARCHINDEX = 'ZKEKO_001'.

* 2. Get the structure table using infostructure
  CLEAR L_GENTAB.
  SELECT SINGLE GENTAB INTO L_GENTAB FROM AIND_STR2
   WHERE ARCHINDEX = L_ARCHINDEX.

  CHECK SY-SUBRC = 0 AND NOT L_GENTAB IS INITIAL.

* 3. Get the archived data from structure table
  IF P_FLAG = 'P'.
    CLEAR LT_INX_KEKO[].
    SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_INX_KEKO
      FROM (L_GENTAB)
     WHERE KALAID  IN GR_KALAID
       AND KALADAT = GV_KALADAT
       AND KALKA IN S_KALKA
       AND BDATJ = P_YEAR
       AND POPER = P_POPER
*      AND BESKZ NE 'F'   "( BESKZ NE 'E' OR BESKZ EQ 'X' )
       AND STLAN NE SPACE
       AND TVERS = GC_TVERS
       AND WERKS IN R_BWKEY
       AND BWSMR = SPACE.    "valuation strategy used.
  ELSEIF P_FLAG = 'C'.
    CLEAR LT_INX_KEKO[].
    SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_INX_KEKO
      FROM (L_GENTAB)
     WHERE KALAID  = SPACE
       AND KALADAT = SPACE
       AND KALKA IN S_KALKA
       AND BDATJ = P_YEAR
       AND POPER = P_POPER
*      AND ( BESKZ EQ 'E' OR BESKZ EQ 'X' )
       AND STLAN NE SPACE
       AND TVERS = GC_TVERS
       AND WERKS IN R_BWKEY
       AND BWSMR = SPACE.    "valuation strategy used.
  ENDIF.

  CHECK NOT LT_INX_KEKO[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: GT_KEKO_A, GT_KEKO_A[].
  LOOP AT LT_INX_KEKO INTO LS_INX_KEKO.
*  4.1 Read information from archivekey & offset
    CLEAR L_HANDLE.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        OBJECT                    = 'CO_COPC'
        ARCHIVKEY                 = LS_INX_KEKO-ARCHIVEKEY
        OFFSET                    = LS_INX_KEKO-ARCHIVEOFS
      IMPORTING
        ARCHIVE_HANDLE            = L_HANDLE
      EXCEPTIONS
        NO_RECORD_FOUND           = 1
        FILE_IO_ERROR             = 2
        INTERNAL_ERROR            = 3
        OPEN_ERROR                = 4
        CANCELLED_BY_USER         = 5
        ARCHIVELINK_ERROR         = 6
        OBJECT_NOT_FOUND          = 7
        FILENAME_CREATION_FAILURE = 8
        FILE_ALREADY_OPEN         = 9
        NOT_AUTHORIZED            = 10
        FILE_NOT_FOUND            = 11
        ERROR_MESSAGE             = 12
        OTHERS                    = 13.

    CHECK SY-SUBRC = 0.

*  4.2 Read table from information
    CLEAR: LT_KEKO, LT_KEKO[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        ARCHIVE_HANDLE          = L_HANDLE
        RECORD_STRUCTURE        = 'KEKO'
        ALL_RECORDS_OF_OBJECT   = 'X'
      TABLES
        TABLE                   = LT_KEKO
      EXCEPTIONS
        END_OF_OBJECT           = 1
        INTERNAL_ERROR          = 2
        WRONG_ACCESS_TO_ARCHIVE = 3
        OTHERS                  = 4.

    CHECK SY-SUBRC = 0 AND NOT LT_KEKO[] IS INITIAL.

* 5. Append archived data table to finally interal table
    INSERT LINES OF LT_KEKO INTO TABLE GT_KEKO_A.
  ENDLOOP.

  SORT GT_KEKO_A.
  DELETE ADJACENT DUPLICATES FROM GT_KEKO_A COMPARING ALL FIELDS.

  LOOP AT GT_KEKO_A.
    CLEAR PT_KEKO.
    MOVE-CORRESPONDING GT_KEKO_A TO PT_KEKO.
    APPEND PT_KEKO.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_KEKO
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_CKIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_NEW  text
*----------------------------------------------------------------------*
FORM ARCHIVE_READ_CKIS  TABLES  PT_NEW STRUCTURE GT_NEW_A.

  TYPES: BEGIN OF TY_CKIS,
         LEDNR  TYPE LEDNR,
         BZOBJ  TYPE CK_OBJ,
         KALNR  TYPE CK_KALNR,
         KALKA  TYPE CK_KALKA,
         KADKY  TYPE CK_KADKY,
         TVERS  TYPE CK_TVERS,
         BWVAR  TYPE CK_BWVAR,
         KKZMA  TYPE CK_KKZMA,
         POSNR  TYPE CK_POSNR,
         MATNR  TYPE MATNR,
         HRKFT  TYPE HRKFT,
         WERKS  TYPE WERKS_D,
         BDATJ  TYPE BDATJ,
         POPER  TYPE POPER,
         BESKZ  TYPE BESKZ,
         SOBES  TYPE SOBES,
         BWSMR  TYPE CK_BWSMR,
         LIFNR  TYPE LIFNR,
         KSTAR  TYPE KSTAR,
         BAUGR  TYPE CK_BAUGR,
         MATNR2 TYPE MATNR,
           ARCHIVEKEY TYPE ARKEY,
           ARCHIVEOFS TYPE ADMI_OFFST.
  TYPES: END OF TY_CKIS.

  DATA: L_HANDLE    TYPE SYTABIX,
        LT_KEKO     TYPE TABLE OF KEKO WITH HEADER LINE,
        LT_CKIS     TYPE TABLE OF CKIS WITH HEADER LINE,
        L_ARCHINDEX LIKE AIND_STR2-ARCHINDEX,
        L_GENTAB    LIKE AIND_STR2-GENTAB.

  DATA: LT_INX_CKIS TYPE TABLE OF TY_CKIS,
        LS_INX_CKIS TYPE TY_CKIS.

* 1. Input the archive infostructure name
  CLEAR L_ARCHINDEX.
  L_ARCHINDEX = 'ZCKIS_001'.

* 2. Get the structure table using infostructure
  CLEAR L_GENTAB.
  SELECT SINGLE GENTAB INTO L_GENTAB FROM AIND_STR2
   WHERE ARCHINDEX = L_ARCHINDEX.

  CHECK SY-SUBRC = 0 AND NOT L_GENTAB IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR LT_INX_CKIS[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_INX_CKIS
    FROM (L_GENTAB)
   WHERE LEDNR = C_LEDNR
     AND MATNR = GT_OUT-MATNR
     AND WERKS = GT_OUT-WERKS
     AND BDATJ = P_YEAR
     AND POPER = P_POPER
     AND KALKA = GT_OUT-KALKA
     AND TVERS = GC_TVERS
     AND HRKFT EQ SPACE.

  CHECK NOT LT_INX_CKIS[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: GT_KEKO_A, GT_KEKO_A[], GT_CKIS_A, GT_CKIS_A[].
  LOOP AT LT_INX_CKIS INTO LS_INX_CKIS.
*  4.1 Read information from archivekey & offset
    CLEAR L_HANDLE.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        OBJECT                    = 'CO_COPC'
        ARCHIVKEY                 = LS_INX_CKIS-ARCHIVEKEY
        OFFSET                    = LS_INX_CKIS-ARCHIVEOFS
      IMPORTING
        ARCHIVE_HANDLE            = L_HANDLE
      EXCEPTIONS
        NO_RECORD_FOUND           = 1
        FILE_IO_ERROR             = 2
        INTERNAL_ERROR            = 3
        OPEN_ERROR                = 4
        CANCELLED_BY_USER         = 5
        ARCHIVELINK_ERROR         = 6
        OBJECT_NOT_FOUND          = 7
        FILENAME_CREATION_FAILURE = 8
        FILE_ALREADY_OPEN         = 9
        NOT_AUTHORIZED            = 10
        FILE_NOT_FOUND            = 11
        ERROR_MESSAGE             = 12
        OTHERS                    = 13.

    CHECK SY-SUBRC = 0.

*  4.2 Read table from information
    CLEAR: LT_KEKO, LT_KEKO[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        ARCHIVE_HANDLE          = L_HANDLE
        RECORD_STRUCTURE        = 'KEKO'
        ALL_RECORDS_OF_OBJECT   = 'X'
      TABLES
        TABLE                   = LT_KEKO
      EXCEPTIONS
        END_OF_OBJECT           = 1
        INTERNAL_ERROR          = 2
        WRONG_ACCESS_TO_ARCHIVE = 3
        OTHERS                  = 4.

    CHECK SY-SUBRC = 0 AND NOT LT_KEKO[] IS INITIAL.

*  4.2 Read table from information
    CLEAR: LT_CKIS, LT_CKIS[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        ARCHIVE_HANDLE          = L_HANDLE
        RECORD_STRUCTURE        = 'CKIS'
        ALL_RECORDS_OF_OBJECT   = 'X'
      TABLES
        TABLE                   = LT_CKIS
      EXCEPTIONS
        END_OF_OBJECT           = 1
        INTERNAL_ERROR          = 2
        WRONG_ACCESS_TO_ARCHIVE = 3
        OTHERS                  = 4.

    CHECK SY-SUBRC = 0 AND NOT LT_CKIS[] IS INITIAL.

* 5. Append archived data table to finally interal table
    INSERT LINES OF: LT_KEKO INTO TABLE GT_KEKO_A,
                     LT_CKIS INTO TABLE GT_CKIS_A.
  ENDLOOP.

  SORT: GT_KEKO_A, GT_CKIS_A.
  DELETE ADJACENT DUPLICATES FROM: GT_KEKO_A COMPARING ALL FIELDS,
                                   GT_CKIS_A COMPARING ALL FIELDS.

  LOOP AT GT_CKIS_A.
    CLEAR PT_NEW.
    MOVE-CORRESPONDING GT_CKIS_A TO PT_NEW.

    CLEAR GT_KEKO_A.
    READ TABLE GT_KEKO_A WITH KEY BZOBJ = GT_CKIS_A-BZOBJ
                                  KALNR = GT_CKIS_A-KALNR
                                  KALKA = GT_CKIS_A-KALKA
                                  KADKY = GT_CKIS_A-KADKY
                                  TVERS = GT_CKIS_A-TVERS
                                  BWVAR = GT_CKIS_A-BWVAR
                                  KKZMA = GT_CKIS_A-KKZMA.
    CHECK SY-SUBRC = 0.
    MOVE-CORRESPONDING GT_KEKO_A TO PT_NEW.

    APPEND PT_NEW.
  ENDLOOP.

  READ TABLE PT_NEW INDEX 1.

ENDFORM.                    " ARCHIVE_READ_CKIS
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_CKIS_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_CKIS  text
*----------------------------------------------------------------------*
FORM ARCHIVE_READ_CKIS_2  TABLES  PT_CKIS STRUCTURE GT_CKIS_A2.

  TYPES: BEGIN OF TY_CKIS,
         LEDNR  TYPE LEDNR,
         BZOBJ  TYPE CK_OBJ,
         KALNR  TYPE CK_KALNR,
         KALKA  TYPE CK_KALKA,
         KADKY  TYPE CK_KADKY,
         TVERS  TYPE CK_TVERS,
         BWVAR  TYPE CK_BWVAR,
         KKZMA  TYPE CK_KKZMA,
         POSNR  TYPE CK_POSNR,
         HRKFT  TYPE HRKFT,
         MATNR  TYPE MATNR,
         WERKS  TYPE WERKS_D,
         BDATJ  TYPE BDATJ,
         POPER  TYPE POPER,
         BESKZ  TYPE BESKZ,
         SOBES  TYPE SOBES,
         BWSMR  TYPE CK_BWSMR,
         LIFNR  TYPE LIFNR,
         KSTAR  TYPE KSTAR,
         BAUGR  TYPE CK_BAUGR,
         MATNR2 TYPE MATNR,
           ARCHIVEKEY TYPE ARKEY,
           ARCHIVEOFS TYPE ADMI_OFFST.
  TYPES: END OF TY_CKIS.

  DATA: L_HANDLE    TYPE SYTABIX,
        LT_KEKO     TYPE TABLE OF KEKO WITH HEADER LINE,
        LT_CKIS     TYPE TABLE OF CKIS WITH HEADER LINE,
        LT_CKIS_TMP TYPE TABLE OF CKIS WITH HEADER LINE,
        LT_CKMLHD   TYPE TABLE OF CKMLHD WITH HEADER LINE,
        L_ARCHINDEX LIKE AIND_STR2-ARCHINDEX,
        L_GENTAB    LIKE AIND_STR2-GENTAB.

  DATA: LT_INX_CKIS TYPE TABLE OF TY_CKIS,
        LS_INX_CKIS TYPE TY_CKIS.

* 1. Input the archive infostructure name
  CLEAR L_ARCHINDEX.
  L_ARCHINDEX = 'ZCKIS_001'.

* 2. Get the structure table using infostructure
  CLEAR L_GENTAB.
  SELECT SINGLE GENTAB INTO L_GENTAB FROM AIND_STR2
   WHERE ARCHINDEX = L_ARCHINDEX.

  CHECK SY-SUBRC = 0 AND NOT L_GENTAB IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR LT_INX_CKIS[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_INX_CKIS
    FROM (L_GENTAB)
   WHERE LEDNR = C_LEDNR
     "b~matnr IN s_artnr
     "AND "b~bwkey IN r_bwkey
     AND BZOBJ = C_BZOBJ
     AND KALKA IN S_KALKA
     AND KADKY = GV_KADKY
     AND TVERS = GC_TVERS
     AND BDATJ = P_YEAR
     AND POPER = P_POPER.

  CHECK NOT LT_INX_CKIS[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: GT_KEKO_A, GT_KEKO_A[], GT_CKIS_A, GT_CKIS_A[].
  LOOP AT LT_INX_CKIS INTO LS_INX_CKIS.
*  4.1 Read information from archivekey & offset
    CLEAR L_HANDLE.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        OBJECT                    = 'CO_COPC'
        ARCHIVKEY                 = LS_INX_CKIS-ARCHIVEKEY
        OFFSET                    = LS_INX_CKIS-ARCHIVEOFS
      IMPORTING
        ARCHIVE_HANDLE            = L_HANDLE
      EXCEPTIONS
        NO_RECORD_FOUND           = 1
        FILE_IO_ERROR             = 2
        INTERNAL_ERROR            = 3
        OPEN_ERROR                = 4
        CANCELLED_BY_USER         = 5
        ARCHIVELINK_ERROR         = 6
        OBJECT_NOT_FOUND          = 7
        FILENAME_CREATION_FAILURE = 8
        FILE_ALREADY_OPEN         = 9
        NOT_AUTHORIZED            = 10
        FILE_NOT_FOUND            = 11
        ERROR_MESSAGE             = 12
        OTHERS                    = 13.

    CHECK SY-SUBRC = 0.

*  4.2 Read table from information
    CLEAR: LT_KEKO, LT_KEKO[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        ARCHIVE_HANDLE          = L_HANDLE
        RECORD_STRUCTURE        = 'KEKO'
        ALL_RECORDS_OF_OBJECT   = 'X'
      TABLES
        TABLE                   = LT_KEKO
      EXCEPTIONS
        END_OF_OBJECT           = 1
        INTERNAL_ERROR          = 2
        WRONG_ACCESS_TO_ARCHIVE = 3
        OTHERS                  = 4.

    CHECK SY-SUBRC = 0 AND NOT LT_KEKO[] IS INITIAL.

*  4.2 Read table from information
    CLEAR: LT_CKIS, LT_CKIS[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        ARCHIVE_HANDLE          = L_HANDLE
        RECORD_STRUCTURE        = 'CKIS'
        ALL_RECORDS_OF_OBJECT   = 'X'
      TABLES
        TABLE                   = LT_CKIS
      EXCEPTIONS
        END_OF_OBJECT           = 1
        INTERNAL_ERROR          = 2
        WRONG_ACCESS_TO_ARCHIVE = 3
        OTHERS                  = 4.

    CHECK SY-SUBRC = 0 AND NOT LT_CKIS[] IS INITIAL.

* 5. Append archived data table to finally interal table
    INSERT LINES OF: LT_KEKO INTO TABLE GT_KEKO_A,
                     LT_CKIS INTO TABLE GT_CKIS_A.
  ENDLOOP.

  SORT: GT_KEKO_A, GT_CKIS_A.
  DELETE ADJACENT DUPLICATES FROM: GT_KEKO_A COMPARING ALL FIELDS,
                                   GT_CKIS_A COMPARING ALL FIELDS.

  CLEAR: LT_CKIS_TMP, LT_CKIS_TMP[].
  LT_CKIS_TMP[] = GT_CKIS_A[].

  SORT LT_CKIS_TMP BY KALNR.
  DELETE ADJACENT DUPLICATES FROM LT_CKIS_TMP COMPARING KALNR.

  CLEAR: LT_CKMLHD, LT_CKMLHD[].
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE LT_CKMLHD
    FROM CKMLHD
    FOR ALL ENTRIES IN LT_CKIS_TMP
   WHERE KALNR = LT_CKIS_TMP-KALNR
     AND MATNR IN S_ARTNR
     AND BWKEY IN R_BWKEY.

  CHECK SY-SUBRC = 0 AND NOT LT_CKMLHD[] IS INITIAL.

  LOOP AT GT_CKIS_A.
    CLEAR PT_CKIS.
    "MOVE-CORRESPONDING gt_ckis_a TO pt_ckis.
    PT_CKIS-MATNR = GT_CKIS_A-MATNR.

    CLEAR GT_KEKO_A.
    READ TABLE GT_KEKO_A WITH KEY BZOBJ = GT_CKIS_A-BZOBJ
                                  KALNR = GT_CKIS_A-KALNR
                                  KALKA = GT_CKIS_A-KALKA
                                  KADKY = GT_CKIS_A-KADKY
                                  TVERS = GT_CKIS_A-TVERS
                                  BWVAR = GT_CKIS_A-BWVAR
                                  KKZMA = GT_CKIS_A-KKZMA.
    CHECK SY-SUBRC = 0.
    PT_CKIS-ARTNR = GT_KEKO_A-MATNR.

    CLEAR LT_CKMLHD.
    READ TABLE LT_CKMLHD WITH KEY KALNR = GT_KEKO_A-KALNR.
    CHECK SY-SUBRC = 0.

    APPEND PT_CKIS.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_CKIS_2
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_CKIS_3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ARCHIVE_READ_CKIS_3 .

  TYPES: BEGIN OF TY_CKIS,
         LEDNR  TYPE LEDNR,
         BZOBJ  TYPE CK_OBJ,
         KALNR  TYPE CK_KALNR,
         KALKA  TYPE CK_KALKA,
         KADKY  TYPE CK_KADKY,
         TVERS  TYPE CK_TVERS,
         BWVAR  TYPE CK_BWVAR,
         KKZMA  TYPE CK_KKZMA,
         POSNR  TYPE CK_POSNR,
         HRKFT  TYPE HRKFT,
         MATNR  TYPE MATNR,
         WERKS  TYPE WERKS_D,
         BDATJ  TYPE BDATJ,
         POPER  TYPE POPER,
         BESKZ  TYPE BESKZ,
         SOBES  TYPE SOBES,
         BWSMR  TYPE CK_BWSMR,
         LIFNR  TYPE LIFNR,
         KSTAR  TYPE KSTAR,
         BAUGR  TYPE CK_BAUGR,
         MATNR2 TYPE MATNR,
           ARCHIVEKEY TYPE ARKEY,
           ARCHIVEOFS TYPE ADMI_OFFST.
  TYPES: END OF TY_CKIS.

  DATA: L_HANDLE    TYPE SYTABIX,
        LT_KEKO     TYPE TABLE OF KEKO WITH HEADER LINE,
        LT_CKIS     TYPE TABLE OF CKIS WITH HEADER LINE,
        LT_CKIS_TMP TYPE TABLE OF CKIS WITH HEADER LINE,
        LT_MARA     TYPE TABLE OF MARA WITH HEADER LINE,
        LT_LFA1     TYPE TABLE OF LFA1 WITH HEADER LINE,
        L_ARCHINDEX LIKE AIND_STR2-ARCHINDEX,
        L_GENTAB    LIKE AIND_STR2-GENTAB.

  DATA: LT_INX_CKIS TYPE TABLE OF TY_CKIS,
        LS_INX_CKIS TYPE TY_CKIS.

* 1. Input the archive infostructure name
  CLEAR L_ARCHINDEX.
  L_ARCHINDEX = 'ZCKIS_001'.

* 2. Get the structure table using infostructure
  CLEAR L_GENTAB.
  SELECT SINGLE GENTAB INTO L_GENTAB FROM AIND_STR2
   WHERE ARCHINDEX = L_ARCHINDEX.

  CHECK SY-SUBRC = 0 AND NOT L_GENTAB IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR LT_INX_CKIS[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_INX_CKIS
    FROM (L_GENTAB)
   WHERE LEDNR = C_LEDNR
     AND MATNR IN S_MATNR
     AND MATNR IN S_ARTNR     "ANDY
     AND BDATJ = P_YEAR
     AND POPER = P_POPER
     AND KALKA IN S_KALKA
     AND TVERS = GC_TVERS
     AND BESKZ IN GR_BESKZ
     AND WERKS IN R_BWKEY
     AND BWSMR <> SPACE   " L-info, U-exit
     AND LIFNR IN S_LIFNR
     AND KSTAR IN S_KSTAR
* UD1K941202 - by IG.MOON {
     AND HRKFT EQ SPACE.
  "AND d~matkl IN s_matkl.

  CHECK NOT LT_INX_CKIS[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: GT_KEKO_A, GT_KEKO_A[], GT_CKIS_A, GT_CKIS_A[].
  LOOP AT LT_INX_CKIS INTO LS_INX_CKIS.
*  4.1 Read information from archivekey & offset
    CLEAR L_HANDLE.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        OBJECT                    = 'CO_COPC'
        ARCHIVKEY                 = LS_INX_CKIS-ARCHIVEKEY
        OFFSET                    = LS_INX_CKIS-ARCHIVEOFS
      IMPORTING
        ARCHIVE_HANDLE            = L_HANDLE
      EXCEPTIONS
        NO_RECORD_FOUND           = 1
        FILE_IO_ERROR             = 2
        INTERNAL_ERROR            = 3
        OPEN_ERROR                = 4
        CANCELLED_BY_USER         = 5
        ARCHIVELINK_ERROR         = 6
        OBJECT_NOT_FOUND          = 7
        FILENAME_CREATION_FAILURE = 8
        FILE_ALREADY_OPEN         = 9
        NOT_AUTHORIZED            = 10
        FILE_NOT_FOUND            = 11
        ERROR_MESSAGE             = 12
        OTHERS                    = 13.

    CHECK SY-SUBRC = 0.

*  4.2 Read table from information
    CLEAR: LT_KEKO, LT_KEKO[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        ARCHIVE_HANDLE          = L_HANDLE
        RECORD_STRUCTURE        = 'KEKO'
        ALL_RECORDS_OF_OBJECT   = 'X'
      TABLES
        TABLE                   = LT_KEKO
      EXCEPTIONS
        END_OF_OBJECT           = 1
        INTERNAL_ERROR          = 2
        WRONG_ACCESS_TO_ARCHIVE = 3
        OTHERS                  = 4.

    CHECK SY-SUBRC = 0 AND NOT LT_KEKO[] IS INITIAL.

*  4.2 Read table from information
    CLEAR: LT_CKIS, LT_CKIS[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        ARCHIVE_HANDLE          = L_HANDLE
        RECORD_STRUCTURE        = 'CKIS'
        ALL_RECORDS_OF_OBJECT   = 'X'
      TABLES
        TABLE                   = LT_CKIS
      EXCEPTIONS
        END_OF_OBJECT           = 1
        INTERNAL_ERROR          = 2
        WRONG_ACCESS_TO_ARCHIVE = 3
        OTHERS                  = 4.

    CHECK SY-SUBRC = 0 AND NOT LT_CKIS[] IS INITIAL.

* 5. Append archived data table to finally interal table
    INSERT LINES OF: LT_KEKO INTO TABLE GT_KEKO_A,
                     LT_CKIS INTO TABLE GT_CKIS_A.
  ENDLOOP.

  SORT: GT_KEKO_A, GT_CKIS_A.
  DELETE ADJACENT DUPLICATES FROM: GT_KEKO_A COMPARING ALL FIELDS,
                                   GT_CKIS_A COMPARING ALL FIELDS.

  CLEAR: LT_CKIS_TMP, LT_CKIS_TMP[].
  LT_CKIS_TMP[] = GT_CKIS_A[].
  SORT LT_CKIS_TMP BY MATNR.
  DELETE ADJACENT DUPLICATES FROM LT_CKIS_TMP COMPARING MATNR.

  CLEAR: LT_MARA, LT_MARA[].
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE LT_MARA
    FROM MARA
    FOR ALL ENTRIES IN LT_CKIS_TMP
   WHERE MATNR = LT_CKIS_TMP-MATNR
     AND MATKL IN S_MATKL.

  CHECK SY-SUBRC = 0 AND NOT LT_MARA[] IS INITIAL.

  CLEAR: LT_CKIS_TMP, LT_CKIS_TMP[].
  LT_CKIS_TMP[] = GT_CKIS_A[].
  SORT LT_CKIS_TMP BY LIFNR.
  DELETE ADJACENT DUPLICATES FROM LT_CKIS_TMP COMPARING LIFNR.

  CLEAR: LT_LFA1, LT_LFA1[].
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE LT_LFA1
    FROM LFA1
    FOR ALL ENTRIES IN LT_CKIS_TMP
   WHERE LIFNR = LT_CKIS_TMP-LIFNR.

  "CHECK sy-subrc = 0 AND NOT lt_lfa1[] IS INITIAL.

  LOOP AT GT_CKIS_A.
    CLEAR GT_CKIS.
    MOVE-CORRESPONDING GT_CKIS_A TO GT_CKIS.

    CLEAR GT_KEKO_A.
    READ TABLE GT_KEKO_A WITH KEY BZOBJ = GT_CKIS_A-BZOBJ
                                  KALNR = GT_CKIS_A-KALNR
                                  KALKA = GT_CKIS_A-KALKA
                                  KADKY = GT_CKIS_A-KADKY
                                  TVERS = GT_CKIS_A-TVERS
                                  BWVAR = GT_CKIS_A-BWVAR
                                  KKZMA = GT_CKIS_A-KKZMA.
    CHECK SY-SUBRC = 0.
    MOVE-CORRESPONDING GT_KEKO_A TO GT_CKIS.

    CLEAR LT_MARA.
    READ TABLE LT_MARA WITH KEY MATNR = GT_CKIS_A-MATNR.
    CHECK SY-SUBRC = 0.
    GT_CKIS-MATKL = LT_MARA-MATKL.

    CLEAR LT_LFA1.
    READ TABLE LT_LFA1 WITH KEY LIFNR = GT_CKIS_A-LIFNR.
    "CHECK sy-subrc = 0.
    GT_CKIS-LAND1 = LT_LFA1-LAND1.

    COLLECT GT_CKIS.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_CKIS_3
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_CKIS_4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ARCHIVE_READ_CKIS_4 .

  TYPES: BEGIN OF TY_CKIS,
         LEDNR  TYPE LEDNR,
         BZOBJ  TYPE CK_OBJ,
         KALNR  TYPE CK_KALNR,
         KALKA  TYPE CK_KALKA,
         KADKY  TYPE CK_KADKY,
         TVERS  TYPE CK_TVERS,
         BWVAR  TYPE CK_BWVAR,
         KKZMA  TYPE CK_KKZMA,
         POSNR  TYPE CK_POSNR,
         HRKFT  TYPE HRKFT,
         MATNR  TYPE MATNR,
         WERKS  TYPE WERKS_D,
         BDATJ  TYPE BDATJ,
         POPER  TYPE POPER,
         BESKZ  TYPE BESKZ,
         SOBES  TYPE SOBES,
         BWSMR  TYPE CK_BWSMR,
         LIFNR  TYPE LIFNR,
         KSTAR  TYPE KSTAR,
         BAUGR  TYPE CK_BAUGR,
         MATNR2 TYPE MATNR,
           ARCHIVEKEY TYPE ARKEY,
           ARCHIVEOFS TYPE ADMI_OFFST.
  TYPES: END OF TY_CKIS.

  DATA: L_HANDLE    TYPE SYTABIX,
        LT_KEKO     TYPE TABLE OF KEKO WITH HEADER LINE,
        LT_CKIS     TYPE TABLE OF CKIS WITH HEADER LINE,
        LT_CKIS_TMP TYPE TABLE OF CKIS WITH HEADER LINE,
        LT_MARA     TYPE TABLE OF MARA WITH HEADER LINE,
        L_ARCHINDEX LIKE AIND_STR2-ARCHINDEX,
        L_GENTAB    LIKE AIND_STR2-GENTAB.

  DATA: LT_INX_CKIS TYPE TABLE OF TY_CKIS,
        LS_INX_CKIS TYPE TY_CKIS.

* 1. Input the archive infostructure name
  CLEAR L_ARCHINDEX.
  L_ARCHINDEX = 'ZCKIS_001'.

* 2. Get the structure table using infostructure
  CLEAR L_GENTAB.
  SELECT SINGLE GENTAB INTO L_GENTAB FROM AIND_STR2
   WHERE ARCHINDEX = L_ARCHINDEX.

  CHECK SY-SUBRC = 0 AND NOT L_GENTAB IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR LT_INX_CKIS[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_INX_CKIS
    FROM (L_GENTAB)
   WHERE LEDNR = C_LEDNR
     AND MATNR IN S_MATNR
     AND MATNR IN S_ARTNR     "ANDY
     AND BDATJ = P_YEAR
     AND POPER = P_POPER
     AND KALKA IN S_KALKA
     AND TVERS = GC_TVERS
     AND BESKZ IN GR_BESKZ
     AND SOBES = '7'
     AND LIFNR IN S_LIFNR
     AND KSTAR IN S_KSTAR
     AND BAUGR = SPACE.     "not assembly
  "AND d~matkl IN s_matkl.

  CHECK NOT LT_INX_CKIS[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: GT_KEKO_A, GT_KEKO_A[], GT_CKIS_A, GT_CKIS_A[].
  LOOP AT LT_INX_CKIS INTO LS_INX_CKIS.
*  4.1 Read information from archivekey & offset
    CLEAR L_HANDLE.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        OBJECT                    = 'CO_COPC'
        ARCHIVKEY                 = LS_INX_CKIS-ARCHIVEKEY
        OFFSET                    = LS_INX_CKIS-ARCHIVEOFS
      IMPORTING
        ARCHIVE_HANDLE            = L_HANDLE
      EXCEPTIONS
        NO_RECORD_FOUND           = 1
        FILE_IO_ERROR             = 2
        INTERNAL_ERROR            = 3
        OPEN_ERROR                = 4
        CANCELLED_BY_USER         = 5
        ARCHIVELINK_ERROR         = 6
        OBJECT_NOT_FOUND          = 7
        FILENAME_CREATION_FAILURE = 8
        FILE_ALREADY_OPEN         = 9
        NOT_AUTHORIZED            = 10
        FILE_NOT_FOUND            = 11
        ERROR_MESSAGE             = 12
        OTHERS                    = 13.

    CHECK SY-SUBRC = 0.

*  4.2 Read table from information
    CLEAR: LT_KEKO, LT_KEKO[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        ARCHIVE_HANDLE          = L_HANDLE
        RECORD_STRUCTURE        = 'KEKO'
        ALL_RECORDS_OF_OBJECT   = 'X'
      TABLES
        TABLE                   = LT_KEKO
      EXCEPTIONS
        END_OF_OBJECT           = 1
        INTERNAL_ERROR          = 2
        WRONG_ACCESS_TO_ARCHIVE = 3
        OTHERS                  = 4.

    CHECK SY-SUBRC = 0 AND NOT LT_KEKO[] IS INITIAL.

*  4.2 Read table from information
    CLEAR: LT_CKIS, LT_CKIS[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        ARCHIVE_HANDLE          = L_HANDLE
        RECORD_STRUCTURE        = 'CKIS'
        ALL_RECORDS_OF_OBJECT   = 'X'
      TABLES
        TABLE                   = LT_CKIS
      EXCEPTIONS
        END_OF_OBJECT           = 1
        INTERNAL_ERROR          = 2
        WRONG_ACCESS_TO_ARCHIVE = 3
        OTHERS                  = 4.

    CHECK SY-SUBRC = 0 AND NOT LT_CKIS[] IS INITIAL.

* 5. Append archived data table to finally interal table
    INSERT LINES OF: LT_KEKO INTO TABLE GT_KEKO_A,
                     LT_CKIS INTO TABLE GT_CKIS_A.
  ENDLOOP.

  SORT: GT_KEKO_A, GT_CKIS_A.
  DELETE ADJACENT DUPLICATES FROM: GT_KEKO_A COMPARING ALL FIELDS,
                                   GT_CKIS_A COMPARING ALL FIELDS.

  CLEAR: LT_CKIS_TMP, LT_CKIS_TMP[].
  LT_CKIS_TMP[] = GT_CKIS_A[].

  SORT LT_CKIS_TMP BY MATNR.
  DELETE ADJACENT DUPLICATES FROM LT_CKIS_TMP COMPARING MATNR.

  CLEAR: LT_MARA, LT_MARA[].
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE LT_MARA
    FROM CKMLHD
    FOR ALL ENTRIES IN LT_CKIS_TMP
   WHERE MATNR = LT_CKIS_TMP-MATNR.

  CHECK SY-SUBRC = 0 AND NOT LT_MATNR[] IS INITIAL.

  LOOP AT GT_CKIS_A.
    CLEAR GT_STOCK.
    MOVE-CORRESPONDING GT_CKIS_A TO GT_STOCK.

    CLEAR GT_KEKO_A.
    READ TABLE GT_KEKO_A WITH KEY BZOBJ = GT_CKIS_A-BZOBJ
                                  KALNR = GT_CKIS_A-KALNR
                                  KALKA = GT_CKIS_A-KALKA
                                  KADKY = GT_CKIS_A-KADKY
                                  TVERS = GT_CKIS_A-TVERS
                                  BWVAR = GT_CKIS_A-BWVAR
                                  KKZMA = GT_CKIS_A-KKZMA.
    CHECK SY-SUBRC = 0.
    GT_STOCK-MATNR = GT_KEKO_A-MATNR.
    GT_STOCK-WERKS = GT_KEKO_A-SOWRK.

    CLEAR LT_MARA.
    READ TABLE LT_MARA WITH KEY MATNR = GT_CKIS_A-MATNR.
    CHECK SY-SUBRC = 0.
    GT_STOCK-MATKL = LT_MARA-MATKL.

    COLLECT GT_STOCK.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_CKIS_4
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_CKIS_5
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ARCHIVE_READ_CKIS_5 .

  TYPES: BEGIN OF TY_CKIS,
         LEDNR  TYPE LEDNR,
         BZOBJ  TYPE CK_OBJ,
         KALNR  TYPE CK_KALNR,
         KALKA  TYPE CK_KALKA,
         KADKY  TYPE CK_KADKY,
         TVERS  TYPE CK_TVERS,
         BWVAR  TYPE CK_BWVAR,
         KKZMA  TYPE CK_KKZMA,
         POSNR  TYPE CK_POSNR,
         HRKFT  TYPE HRKFT,
         MATNR  TYPE MATNR,
         WERKS  TYPE WERKS_D,
         BDATJ  TYPE BDATJ,
         POPER  TYPE POPER,
         BESKZ  TYPE BESKZ,
         SOBES  TYPE SOBES,
         BWSMR  TYPE CK_BWSMR,
         LIFNR  TYPE LIFNR,
         KSTAR  TYPE KSTAR,
         BAUGR  TYPE CK_BAUGR,
         MATNR2 TYPE MATNR,
           ARCHIVEKEY TYPE ARKEY,
           ARCHIVEOFS TYPE ADMI_OFFST.
  TYPES: END OF TY_CKIS.

  DATA: L_HANDLE    TYPE SYTABIX,
        LT_KEKO     TYPE TABLE OF KEKO WITH HEADER LINE,
        LT_CKIS     TYPE TABLE OF CKIS WITH HEADER LINE,
        LT_CKIS_TMP TYPE TABLE OF CKIS WITH HEADER LINE,
        LT_MARA     TYPE TABLE OF MARA WITH HEADER LINE,
        LT_CKMLHD   TYPE TABLE OF CKMLHD WITH HEADER LINE,
        L_ARCHINDEX LIKE AIND_STR2-ARCHINDEX,
        L_GENTAB    LIKE AIND_STR2-GENTAB.

  DATA: LT_INX_CKIS TYPE TABLE OF TY_CKIS,
        LS_INX_CKIS TYPE TY_CKIS.

* 1. Input the archive infostructure name
  CLEAR L_ARCHINDEX.
  L_ARCHINDEX = 'ZCKIS_001'.

* 2. Get the structure table using infostructure
  CLEAR L_GENTAB.
  SELECT SINGLE GENTAB INTO L_GENTAB FROM AIND_STR2
   WHERE ARCHINDEX = L_ARCHINDEX.

  CHECK SY-SUBRC = 0 AND NOT L_GENTAB IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR LT_INX_CKIS[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_INX_CKIS
    FROM (L_GENTAB)
   WHERE LEDNR = C_LEDNR
     AND BDATJ = P_YEAR
     AND POPER = P_POPER
     AND KALKA IN S_KALKA
     AND MATNR IN S_ARTNR     "ANDY
     AND TVERS = GC_TVERS
     AND BESKZ = 'E'      " In-house production
     AND WERKS IN R_BWKEY
     AND BWSMR = SPACE    " L-info, U-exit
     AND MATNR IN S_MATNR
     AND LIFNR IN S_LIFNR
     AND KSTAR IN S_KSTAR.
*    AND b~baugr = space    " No Assy.
*    AND a~stnum EQ space   "ANDY 12/6/07
  "AND e~matkl IN s_matkl.

  CHECK NOT LT_INX_CKIS[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: GT_KEKO_A, GT_KEKO_A[], GT_CKIS_A, GT_CKIS_A[].
  LOOP AT LT_INX_CKIS INTO LS_INX_CKIS.
*  4.1 Read information from archivekey & offset
    CLEAR L_HANDLE.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        OBJECT                    = 'CO_COPC'
        ARCHIVKEY                 = LS_INX_CKIS-ARCHIVEKEY
        OFFSET                    = LS_INX_CKIS-ARCHIVEOFS
      IMPORTING
        ARCHIVE_HANDLE            = L_HANDLE
      EXCEPTIONS
        NO_RECORD_FOUND           = 1
        FILE_IO_ERROR             = 2
        INTERNAL_ERROR            = 3
        OPEN_ERROR                = 4
        CANCELLED_BY_USER         = 5
        ARCHIVELINK_ERROR         = 6
        OBJECT_NOT_FOUND          = 7
        FILENAME_CREATION_FAILURE = 8
        FILE_ALREADY_OPEN         = 9
        NOT_AUTHORIZED            = 10
        FILE_NOT_FOUND            = 11
        ERROR_MESSAGE             = 12
        OTHERS                    = 13.

    CHECK SY-SUBRC = 0.

*  4.2 Read table from information
    CLEAR: LT_KEKO, LT_KEKO[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        ARCHIVE_HANDLE          = L_HANDLE
        RECORD_STRUCTURE        = 'KEKO'
        ALL_RECORDS_OF_OBJECT   = 'X'
      TABLES
        TABLE                   = LT_KEKO
      EXCEPTIONS
        END_OF_OBJECT           = 1
        INTERNAL_ERROR          = 2
        WRONG_ACCESS_TO_ARCHIVE = 3
        OTHERS                  = 4.

    CHECK SY-SUBRC = 0 AND NOT LT_KEKO[] IS INITIAL.

*  4.2 Read table from information
    CLEAR: LT_CKIS, LT_CKIS[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        ARCHIVE_HANDLE          = L_HANDLE
        RECORD_STRUCTURE        = 'CKIS'
        ALL_RECORDS_OF_OBJECT   = 'X'
      TABLES
        TABLE                   = LT_CKIS
      EXCEPTIONS
        END_OF_OBJECT           = 1
        INTERNAL_ERROR          = 2
        WRONG_ACCESS_TO_ARCHIVE = 3
        OTHERS                  = 4.

    CHECK SY-SUBRC = 0 AND NOT LT_CKIS[] IS INITIAL.

* 5. Append archived data table to finally interal table
    INSERT LINES OF: LT_KEKO INTO TABLE GT_KEKO_A,
                     LT_CKIS INTO TABLE GT_CKIS_A.
  ENDLOOP.

  SORT: GT_KEKO_A, GT_CKIS_A.
  DELETE ADJACENT DUPLICATES FROM: GT_KEKO_A COMPARING ALL FIELDS,
                                   GT_CKIS_A COMPARING ALL FIELDS.

  CLEAR: LT_CKIS_TMP, LT_CKIS_TMP[].
  LT_CKIS_TMP[] = GT_CKIS_A[].

  SORT LT_CKIS_TMP BY MATNR.
  DELETE ADJACENT DUPLICATES FROM LT_CKIS_TMP COMPARING MATNR.

  CLEAR: LT_MARA, LT_MARA[].
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE LT_MARA
    FROM MARA
    FOR ALL ENTRIES IN LT_CKIS_TMP
   WHERE MATNR = LT_CKIS_TMP-MATNR.

  CHECK SY-SUBRC = 0 AND NOT LT_MARA[] IS INITIAL.

  CLEAR: LT_CKIS_TMP, LT_CKIS_TMP[].
  LT_CKIS_TMP[] = GT_CKIS_A[].

  SORT LT_CKIS_TMP BY MATNR WERKS.
  DELETE ADJACENT DUPLICATES FROM LT_CKIS_TMP COMPARING MATNR WERKS.

  CLEAR: LT_CKMLHD, LT_CKMLHD[].
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE LT_CKMLHD
    FROM CKMLHD
    FOR ALL ENTRIES IN LT_CKIS_TMP
   WHERE MATNR = LT_CKIS_TMP-MATNR
     AND BWKEY = LT_CKIS_TMP-WERKS.

  "CHECK sy-subrc = 0 AND NOT lt_ckmlhd[] IS INITIAL.

  LOOP AT GT_CKIS_A.
    CLEAR GT_COMP.
    MOVE-CORRESPONDING GT_CKIS_A TO GT_COMP.

    CLEAR GT_KEKO_A.
    READ TABLE GT_KEKO_A WITH KEY BZOBJ = GT_CKIS_A-BZOBJ
                                  KALNR = GT_CKIS_A-KALNR
                                  KALKA = GT_CKIS_A-KALKA
                                  KADKY = GT_CKIS_A-KADKY
                                  TVERS = GT_CKIS_A-TVERS
                                  BWVAR = GT_CKIS_A-BWVAR
                                  KKZMA = GT_CKIS_A-KKZMA.
    CHECK SY-SUBRC = 0.
    MOVE-CORRESPONDING GT_KEKO_A TO GT_COMP.
    GT_COMP-MATNR = GT_CKIS_A-MATNR.

    CLEAR LT_MARA.
    READ TABLE LT_MARA WITH KEY MATNR = GT_CKIS_A-MATNR.
    CHECK SY-SUBRC = 0.
    GT_COMP-MATKL = LT_MARA-MATKL.

    CLEAR LT_CKMLHD.
    READ TABLE LT_CKMLHD WITH KEY MATNR = GT_CKIS_A-MATNR
                                  BWKEY = GT_CKIS_A-WERKS.
    "CHECK sy-subrc = 0.
    GT_COMP-KALNR = LT_CKMLHD-KALNR.

    APPEND GT_COMP.
  ENDLOOP.

  SORT GT_COMP BY KALNR KALKA KADKY MATNR KOKRS
                  WERKS
                  BDATJ POPER BWDAT ALDAT
                  HRKFT PMEHT MENGE WERTN
                  INFNR LIFNR LAND1 BESKZ SOBES BAUGR MATKL
                  STRAT SUBSTRAT ELEMT ELEMTNS.

  DELETE ADJACENT DUPLICATES FROM GT_COMP
                             COMPARING KALNR KALKA KADKY MATNR KOKRS
                                       WERKS
                                       BDATJ POPER BWDAT ALDAT
                                       HRKFT PMEHT MENGE WERTN
                                       INFNR LIFNR LAND1 BESKZ SOBES BAUGR MATKL
                                       STRAT SUBSTRAT ELEMT ELEMTNS.

ENDFORM.                    " ARCHIVE_READ_CKIS_5

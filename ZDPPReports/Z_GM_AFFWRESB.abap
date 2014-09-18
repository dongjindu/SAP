*&---------------------------------------------------------------------*
*& Report  ZMDF_GM_AFFWRESB                                            *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  ZMDF_GM_AFFWRESB LINE-SIZE 255.
*&---------------------------------------------------------------------*
*& PLEASE START THE CORRECTION REPORT Z_PPC_CORRECT_AFFW (note 730504) *
*& BEFORE YOU START THIS REPORT                                        *
*&---------------------------------------------------------------------*

TYPE-POOLS SLIS.
TYPES:
  BEGIN OF TY_AFFWRESB,
    WEBLNR  TYPE AFFW-WEBLNR,
    WEBLPOS TYPE AFFW-WEBLPOS,
    RSNUM   TYPE AFFW-RSNUM,
    RSPOS   TYPE AFFW-RSPOS,
    C_MATNR TYPE AFFW-MATNR,
    C_WERKS TYPE AFFW-WERKS,
    C_LGORT TYPE AFFW-LGORT,
    C_CHARG TYPE AFFW-CHARG,
    C_SOBKZ TYPE AFFW-SOBKZ,
    C_BWART TYPE AFFW-BWART,
    C_ERFME TYPE AFFW-ERFME,
    R_MATNR TYPE AFFW-MATNR,
    R_WERKS TYPE AFFW-WERKS,
    R_LGORT TYPE AFFW-LGORT,
    R_CHARG TYPE AFFW-CHARG,
    R_SOBKZ TYPE AFFW-SOBKZ,
    R_BWART TYPE AFFW-BWART,
    R_ERFME TYPE AFFW-ERFME,
  END OF TY_AFFWRESB,
  TY_TAB_AFFWRESB TYPE TABLE OF TY_AFFWRESB.
DATA:
  GF_MATNR  TYPE MATNR,
  GF_WERKS  TYPE WERKS_D,
  GF_RSNUM  TYPE RSNUM.
*-------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
SELECT-OPTIONS:
  SO_MATNR  FOR GF_MATNR,
  SO_WERKS  FOR GF_WERKS.
SELECTION-SCREEN END OF BLOCK B1.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME.
PARAMETERS:
  P_CHECK1 AS CHECKBOX DEFAULT 'X',
  P_CHECK2 AS CHECKBOX DEFAULT 'X',
  P_CHECK3 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK B2.
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME.
PARAMETERS:
  PA_TEST1 AS CHECKBOX DEFAULT 'X',
  PA_TEST2 AS CHECKBOX DEFAULT 'X',
  PA_TEST3 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK B3.
*-------------------------------------------

FIELD-SYMBOLS:
  <FS_AFFW> TYPE AFFW,
  <FS_RESB> TYPE RESB.
RANGES:
  SO_RSNUM  FOR GF_RSNUM.
DATA:
  LS_RESB   TYPE RESB,
  LT_AFFW   TYPE TABLE OF AFFW,
  LT_AFFW_U TYPE TABLE OF AFFW,
  LT_RESB   TYPE TABLE OF RESB,
  LT_RESB_I TYPE TABLE OF RESB,
  LT_RESB_U TYPE TABLE OF RESB,
  LT_RESB_D TYPE TABLE OF RESB,
  LT_OUTPUT TYPE TY_TAB_AFFWRESB,
  LT_RESCO LIKE TABLE OF RESCO WITH HEADER LINE,
  L_BIGSELECT(1) TYPE C.

INITIALIZATION.
  CLEAR: SO_RSNUM, LT_RESCO.
  REFRESH: LT_RESB_D,LT_RESB_I,LT_RESB_U,LT_RESCO,LT_AFFW,SO_RSNUM,
           LT_RESB, LT_AFFW_U.

START-OF-SELECTION.
 IF P_CHECK1 IS INITIAL AND P_CHECK2 IS INITIAL AND P_CHECK3 IS INITIAL.
*   in case background start we have to see the message in the log
    WRITE: /1 'No check was selected'.
    EXIT.
  ENDIF.
  IF PA_TEST1 IS INITIAL OR PA_TEST2 IS INITIAL OR PA_TEST3 IS INITIAL .
*   question?
    DATA L_ANSWER(1).
    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        TITEL          = 'Confirmation'
        TEXTLINE1      = 'This is not a test run'
        TEXTLINE2      = 'Do you want to continue?'
        CANCEL_DISPLAY = SPACE
      IMPORTING
        ANSWER         = L_ANSWER.
    IF L_ANSWER = 'N' OR
       L_ANSWER = 'A'.
      EXIT.
    ENDIF.
  ENDIF.
  IF SO_MATNR[] IS INITIAL AND SO_WERKS[] IS INITIAL.
    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
         EXPORTING
              TITEL          = 'Confirmation'
              TEXTLINE1      =
'Without restrictions, a very long runtime expected for this selection.'
              TEXTLINE2      = 'Do you want to continue?'
              CANCEL_DISPLAY = SPACE
         IMPORTING
              ANSWER         = L_ANSWER.
    IF L_ANSWER = 'N' OR
       L_ANSWER = 'A'.
      EXIT.
    ENDIF.
    L_BIGSELECT = 'X'.
  ENDIF.

* lock the objects
  IF PA_TEST1 IS INITIAL OR PA_TEST2 IS INITIAL OR PA_TEST3 IS INITIAL .
* lock the entries
    CALL FUNCTION 'ENQUEUE_ESAFFW'
      EXPORTING
        MODE_AFFW      = 'E'
        MANDT          = SY-MANDT
        _SCOPE         = '3'
        _WAIT          = 'X'
      EXCEPTIONS
        FOREIGN_LOCK   = 1
        SYSTEM_FAILURE = 2
        OTHERS         = 3.
    IF SY-SUBRC NE 0.
** Furong on 08/08/12
*      MESSAGE e167(rm).
      MESSAGE S167(RM).
      EXIT.
** End on 08/08/12
    ENDIF.
* LOCK PPCGO RUN
    CALL FUNCTION 'ENQUEUE_E_PPC_CONF_MAT'
      EXPORTING
        MODE_PPC_CONF_MAT = 'E'
        MANDT             = SY-MANDT
        _SCOPE            = '3'
        _WAIT             = 'X'
      EXCEPTIONS
        FOREIGN_LOCK      = 1
        SYSTEM_FAILURE    = 2
        OTHERS            = 3.
    IF SY-SUBRC NE 0.
** Furong on 08/08/12
*      MESSAGE e020(ppc1pr) WITH sy-msgv1 RAISING enqueue_error.
      MESSAGE S020(PPC1PR) WITH SY-MSGV1 RAISING ENQUEUE_ERROR.
      EXIT.
** End
    ENDIF.
    CALL FUNCTION 'ENQUEUE_E_PPC_RES_HDR'
      EXPORTING
        MODE_PPC_RES_HDR = 'E'
        MANDT            = SY-MANDT
        _SCOPE           = '3'
        _WAIT            = 'X'
      EXCEPTIONS
        FOREIGN_LOCK     = 1
        SYSTEM_FAILURE   = 2
        OTHERS           = 3.
    IF SY-SUBRC NE 0.
** Furong on 08/08/12
*      MESSAGE e020(ppc1pr) WITH sy-msgv1
*      RAISING enqueue_error.
      MESSAGE S020(PPC1PR) WITH SY-MSGV1.
      EXIT.
** End
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
* PART 1:                                                              *
*         -Check the wrong reservations without reprocessing records   *
*           RESB entry without AFFW -> delete them                     *
*----------------------------------------------------------------------*
  IF NOT P_CHECK1 IS INITIAL.
    WRITE: /1 'Part 1. Check wrong reservations'.

    SELECT * INTO TABLE LT_RESB
             FROM RESB
             WHERE MATNR IN SO_MATNR
               AND WERKS IN SO_WERKS
               AND BDART = 'SB'
               AND PLNUM = '          '
             ORDER BY RSNUM RSPOS.
    IF SY-SUBRC IS INITIAL.
      IF NOT LT_RESB[] IS INITIAL.
*   records against run schedule headers = records from <= 4.0B
        SELECT RSNUM FROM RESCO
               INTO CORRESPONDING FIELDS OF TABLE LT_RESCO
               FOR ALL ENTRIES IN LT_RESB
               WHERE RSNUM = LT_RESB-RSNUM.
      ENDIF.
      IF SY-SUBRC IS INITIAL.
        LOOP AT LT_RESCO.
          READ TABLE LT_RESB WITH KEY RSNUM = LT_RESCO-RSNUM
               BINARY SEARCH TRANSPORTING NO FIELDS.
          CHECK SY-SUBRC IS INITIAL.
          DELETE LT_RESB FROM SY-TABIX WHERE RSNUM = LT_RESCO-RSNUM.
        ENDLOOP.
      ENDIF.

      IF NOT LT_RESB[] IS INITIAL.
*   check the entries corresponding entries in the table AFFW
        SELECT * FROM AFFW INTO TABLE LT_AFFW
                 FOR ALL ENTRIES IN LT_RESB
                 WHERE RSNUM EQ LT_RESB-RSNUM
                 AND RSPOS EQ LT_RESB-RSPOS.
      ENDIF.
      SORT LT_AFFW BY RSNUM RSPOS.
      LOOP AT LT_RESB ASSIGNING <FS_RESB>.
        READ TABLE LT_AFFW TRANSPORTING NO FIELDS
                           WITH KEY RSNUM = <FS_RESB>-RSNUM
                                    RSPOS = <FS_RESB>-RSPOS
                           BINARY SEARCH.
        IF NOT SY-SUBRC IS INITIAL.
*       for the reservation there is no AFFW entry -> wrong reservation
          WRITE: /1 'RESB w/o AFFW entry:',
                    <FS_RESB>-RSNUM,
                    <FS_RESB>-RSPOS,
                    <FS_RESB>-MATNR,
                    <FS_RESB>-WERKS.
          APPEND <FS_RESB> TO LT_RESB_D.
        ENDIF.
      ENDLOOP.

      IF LT_RESB_D[] IS INITIAL.
        WRITE: /1 'No inconsistency detected.'.
      ELSEIF PA_TEST1 IS INITIAL.
        DELETE RESB FROM TABLE LT_RESB_D.
        PERFORM SEND_RESERVATION USING 'D'
                                       LT_RESB_D.
        COMMIT WORK.
      ENDIF.
    ENDIF.
  ENDIF. " if not p_check1 is initial.

*----------------------------------------------------------------------*
* PART 2:                                                              *
*         -Compare the key fields of the tables AFFW-RESB              *
*           AFFW entry should be the right one                         *
*           do not check the quantity                                  *
*----------------------------------------------------------------------*
  IF NOT P_CHECK2 IS INITIAL.
    SKIP.
    WRITE: /1 'Part 2. Check the key fields of AFFW/RESB'.

    REFRESH: LT_RESB_D, LT_RESB, LT_RESCO, LT_AFFW.
    FREE:  LT_RESB_D, LT_RESB, LT_RESCO, LT_AFFW.

* Get affw entries for selected materials
    SELECT * FROM AFFW
      INTO TABLE LT_AFFW
      WHERE MATNR IN SO_MATNR
        AND WERKS IN SO_WERKS
        AND FLG_ORIG = '1'.

* Get referrenced resb entries
    SO_RSNUM-SIGN = 'I'.
    SO_RSNUM-OPTION = 'EQ'.
    LOOP AT LT_AFFW ASSIGNING <FS_AFFW>.
      SO_RSNUM-LOW = <FS_AFFW>-RSNUM.
      COLLECT SO_RSNUM.
    ENDLOOP.

  IF NOT SO_RSNUM[] IS INITIAL. "otherwise the system selects everything
* select the reservations
      SELECT * FROM RESB
        INTO TABLE LT_RESB
        WHERE RSNUM IN SO_RSNUM AND
              BDART EQ 'SB'.
      SORT LT_RESB BY RSNUM RSPOS.
    ENDIF.

* Start comparing entries
    LOOP AT LT_AFFW ASSIGNING <FS_AFFW>.

*   ... get referenced resb line
      READ TABLE LT_RESB WITH KEY RSNUM = <FS_AFFW>-RSNUM
                                  RSPOS = <FS_AFFW>-RSPOS
                                  ASSIGNING <FS_RESB>
                                  BINARY SEARCH.
      IF NOT SY-SUBRC IS INITIAL.
*     no resb entry for this affw!!!
*     create the missing entry
        PERFORM CREATE_NEW_RESB USING    <FS_AFFW>
                                CHANGING LT_RESB_I
                                         LT_AFFW_U
                                         LT_OUTPUT.
        CONTINUE. "to next pass
      ENDIF.

*   .. do the comparison of the key fields
      IF NOT ( <FS_AFFW>-MATNR = <FS_RESB>-MATNR AND
               <FS_AFFW>-WERKS = <FS_RESB>-WERKS AND
               <FS_AFFW>-LGORT = <FS_RESB>-LGORT AND
               <FS_AFFW>-CHARG = <FS_RESB>-CHARG AND
               <FS_AFFW>-SOBKZ = <FS_RESB>-SOBKZ AND
               <FS_AFFW>-BWART = <FS_RESB>-BWART AND
               <FS_AFFW>-ERFME = <FS_RESB>-ERFME ).
*     entries do not match! change the RESB entry!
        PERFORM MODIF_WRONG_RESB USING    LT_AFFW
                                 CHANGING <FS_RESB>
                                          <FS_AFFW>
                                          LT_RESB_I
                                          LT_RESB_U
                                          LT_AFFW_U
                                          LT_OUTPUT.
      ENDIF.

    ENDLOOP. "end of comparison


    IF LT_RESB_I IS INITIAL AND LT_RESB_U IS INITIAL.

      WRITE: /1 'No inconsistency detected.'.

    ELSEIF PA_TEST2 IS INITIAL.

*   Now we have to modify database
      IF NOT LT_RESB_I IS INITIAL.
*     check whether RSPOS are used. shouldn't be because we locked
        SELECT * FROM RESB INTO TABLE LT_RESB
                 FOR ALL ENTRIES IN LT_RESB_I
                 WHERE RSNUM EQ LT_RESB_I-RSNUM
                   AND RSPOS EQ LT_RESB_I-RSPOS.
        LOOP AT LT_RESB ASSIGNING <FS_RESB>.
          DELETE LT_RESB_I WHERE RSNUM EQ <FS_RESB>-RSNUM
                             AND RSPOS EQ <FS_RESB>-RSPOS.
          WRITE: /1 'Please check RESB entry manually',
                    <FS_RESB>-RSNUM,
                    <FS_RESB>-RSPOS,
                    <FS_RESB>-MATNR.
        ENDLOOP.
*     INSERT THE RESB ENTRIES
        INSERT RESB FROM TABLE LT_RESB_I.
        PERFORM SEND_RESERVATION USING 'I'
                                       LT_RESB_I.
      ENDIF.
      IF NOT LT_RESB_U IS INITIAL.
*     UPDATE THE RESB ENTRIES
        UPDATE RESB FROM TABLE LT_RESB_U.
        PERFORM SEND_RESERVATION USING 'U'
                                       LT_RESB_U.
      ENDIF.
      IF NOT LT_AFFW_U[] IS INITIAL.
*     update AFFW entries with new RSPOS
        UPDATE AFFW FROM TABLE LT_AFFW_U.
      ENDIF.
      COMMIT WORK.

    ENDIF.

* do protocol:
    IF NOT LT_RESB_I[] IS INITIAL.
      SKIP.
      WRITE: /1 'Following entries were inserted into table RESB'.
      LOOP AT LT_RESB_I ASSIGNING <FS_RESB>.
        PERFORM WRITE_RESB USING LT_AFFW
                                 <FS_RESB>.
      ENDLOOP.
    ENDIF.

    IF NOT LT_RESB_U[] IS INITIAL.
      SKIP.
      WRITE: /1 'Following entries were updated in table RESB'.
      LOOP AT LT_RESB_U ASSIGNING <FS_RESB>.
        PERFORM WRITE_RESB USING LT_AFFW
                                 <FS_RESB>.
      ENDLOOP.

      IF NOT LT_AFFW_U[] IS INITIAL.
        SKIP.
        WRITE: /1 'Following RSNUMs were updated in table AFFW'.
        LOOP AT LT_AFFW_U ASSIGNING <FS_AFFW>.
          WRITE: /1 <FS_AFFW>-RSNUM,
                    <FS_AFFW>-RSPOS,
                    <FS_AFFW>-MATNR,
                    <FS_AFFW>-WERKS.
        ENDLOOP.

      ENDIF.
    ENDIF.
  ENDIF. " if not p_check2 is initial.
*----------------------------------------------------------------------*
* PART 3:                                                              *
*         -Check the quantity between the tables AFFW-RESB             *
*           AFFW entry should be the right one                         *
*----------------------------------------------------------------------*
  IF NOT P_CHECK3 IS INITIAL.
    SKIP.
    WRITE: /1 'Part 3. Check quantity'.

    REFRESH: LT_RESB_D, LT_RESB_I, LT_RESB_U, LT_RESB, LT_AFFW.
    FREE: LT_RESB_D, LT_RESB_I, LT_RESB_U, LT_RESB, LT_AFFW.

    DATA: LF_QUANT   TYPE ERFMG,
          LF_REDFLAG TYPE C.

* Get affw entries for selected materials
    SELECT * FROM AFFW
      INTO TABLE LT_AFFW
      WHERE MATNR IN SO_MATNR
        AND WERKS IN SO_WERKS
        AND FLG_ORIG = '1'.

    IF NOT LT_AFFW[] IS INITIAL.
      SELECT * FROM RESB
        INTO TABLE LT_RESB
        FOR ALL ENTRIES IN LT_AFFW
        WHERE RSNUM EQ LT_AFFW-RSNUM
         AND  RSPOS EQ LT_AFFW-RSPOS
         AND  BDART EQ 'SB'.
    ENDIF.
    SORT LT_RESB BY RSNUM RSPOS.
    SORT LT_AFFW BY RSNUM RSPOS.

* for each resb entry (individual component...)
    LOOP AT LT_RESB ASSIGNING <FS_RESB>.

      CLEAR: LF_QUANT, LF_REDFLAG.
*   1. get individual affw entries
      LOOP AT LT_AFFW ASSIGNING <FS_AFFW>
                WHERE RSNUM = <FS_RESB>-RSNUM
                  AND RSPOS = <FS_RESB>-RSPOS.
        ADD <FS_AFFW>-ERFMG TO LF_QUANT.
*     double check consistency affw-resb
        IF NOT ( <FS_AFFW>-MATNR = <FS_RESB>-MATNR AND
                 <FS_AFFW>-WERKS = <FS_RESB>-WERKS AND
                 <FS_AFFW>-LGORT = <FS_RESB>-LGORT AND
                 <FS_AFFW>-CHARG = <FS_RESB>-CHARG AND
                 <FS_AFFW>-SOBKZ = <FS_RESB>-SOBKZ AND
                 <FS_AFFW>-BWART = <FS_RESB>-BWART AND
                 <FS_AFFW>-ERFME = <FS_RESB>-ERFME ).
*       key fields should have been corrected!
          LF_REDFLAG = 'X'.
        ENDIF.
      ENDLOOP. "loop at referenced affw entries
*   2. check errors
      IF NOT LF_REDFLAG IS INITIAL.
*    consistency error (wrong key fields)
        WRITE: /1 'CHECK ERROR MANUALLY',
                   <FS_RESB>-RSNUM,
                   <FS_RESB>-RSPOS,
                   <FS_RESB>-MATNR.
      ELSEIF NOT ( LF_QUANT = <FS_RESB>-BDMNG AND
                   LF_QUANT = <FS_RESB>-ERFMG ).
*     quantity error
        PERFORM MODIF_WRONG_RESB_WITH_Q USING    LF_QUANT
                                        CHANGING <FS_RESB>
                                                 LT_RESB_U.
      ENDIF.

    ENDLOOP. "loop at resb entries


    IF LT_RESB_U IS INITIAL.

      WRITE: /1 'No inconsistency detected.'.

    ELSEIF PA_TEST3 IS INITIAL.

*   ... correct wrong stuff
      IF NOT LT_RESB_U IS INITIAL.
*     UPDATE THE RESB ENTRIES with quantity
        UPDATE RESB FROM TABLE LT_RESB_U.
        PERFORM SEND_RESERVATION USING 'U'
                                       LT_RESB_U.
      ENDIF.
      COMMIT WORK.

    ENDIF.

* do protocol:
    IF NOT LT_RESB_U[] IS INITIAL.
      SKIP.
    WRITE: /1 'Following entries were updated in table RESB (quantity)'.
      LOOP AT LT_RESB_U ASSIGNING <FS_RESB>.
        PERFORM WRITE_RESB USING LT_AFFW
                                 <FS_RESB>.
      ENDLOOP.
    ENDIF.

  ENDIF. " if not p_check3 is initial.


  IF PA_TEST1 IS INITIAL OR PA_TEST2 IS INITIAL OR PA_TEST3 IS INITIAL.
*   unlock the locking objects
    CALL FUNCTION 'DEQUEUE_ESAFFW'
      EXPORTING
        MODE_AFFW = 'E'
        MANDT     = SY-MANDT.
    CALL FUNCTION 'DEQUEUE_ESAFFW'
      EXPORTING
        MODE_AFFW = 'E'
        MANDT     = SY-MANDT
        _SCOPE    = '3'.
    CALL FUNCTION 'DEQUEUE_E_PPC_CONF_MAT'
      EXPORTING
        MODE_PPC_CONF_MAT = 'E'
        MANDT             = SY-MANDT
        _SCOPE            = '3'
        _SYNCHRON         = ' '
        _COLLECT          = ' '.
    CALL FUNCTION 'DEQUEUE_E_PPC_RES_HDR'
      EXPORTING
        MODE_PPC_RES_HDR = 'E'
        MANDT            = SY-MANDT
        _SCOPE           = '3'.
  ELSE.
    MESSAGE S999(PP) WITH 'Nothing has been posted, as requested.'.
  ENDIF.


*---------------------------------------------------------------------*
*       FORM create_new_resb                                          *
*---------------------------------------------------------------------*
FORM CREATE_NEW_RESB  USING    IS_AFFW   TYPE AFFW
                      CHANGING ET_RESB_I LIKE LT_RESB
                               ET_AFFW_U LIKE LT_AFFW
                               ET_OUTPUT TYPE TY_TAB_AFFWRESB.
  DATA:
    LS_RESB TYPE RESB.

  CLEAR LS_RESB.

* basically we have to create a new entry for is_affw-rsnum and -rspos
* but we have to check whether this entry has been already created
* by a previous affw entry -> to avoid duplicate_insert dumps
  READ TABLE ET_RESB_I
      INTO LS_RESB
      WITH KEY RSNUM = IS_AFFW-RSNUM
               RSPOS = IS_AFFW-RSPOS.
  IF SY-SUBRC GT 0.

*   it is safe to create new resb entry
*   it does not exist in database, nor in resb_i
    CLEAR LS_RESB.
    MOVE IS_AFFW-RSNUM TO LS_RESB-RSNUM.
    MOVE IS_AFFW-RSPOS TO LS_RESB-RSPOS.
    MOVE 'SB'          TO LS_RESB-BDART.
    MOVE IS_AFFW-MATNR TO LS_RESB-MATNR.
    MOVE IS_AFFW-WERKS TO LS_RESB-WERKS.
    MOVE IS_AFFW-LGORT TO LS_RESB-LGORT.
    MOVE IS_AFFW-PRVBE TO LS_RESB-PRVBE.
    MOVE IS_AFFW-CHARG TO LS_RESB-CHARG.
    MOVE IS_AFFW-ERSDA TO LS_RESB-BDTER.
    MOVE IS_AFFW-ERSDA TO LS_RESB-SBTER.
    MOVE IS_AFFW-ERFMG TO LS_RESB-BDMNG.
    MOVE IS_AFFW-ERFME TO LS_RESB-MEINS.
    MOVE IS_AFFW-ERFMG TO LS_RESB-ERFMG.
    MOVE IS_AFFW-ERFME TO LS_RESB-ERFME.
    MOVE IS_AFFW-SHKZG TO LS_RESB-SHKZG.
    MOVE IS_AFFW-AUFNR TO LS_RESB-AUFNR.
    MOVE IS_AFFW-SOBKZ TO LS_RESB-BWART.
    MOVE IS_AFFW-BWART TO LS_RESB-BWART.
*   and insert it into resb_i
    APPEND LS_RESB TO ET_RESB_I.

  ELSE.

*   check if this new resb entry is consistent with the affw
    IF IS_AFFW-MATNR = LS_RESB-MATNR AND
           IS_AFFW-WERKS = LS_RESB-WERKS AND
           IS_AFFW-LGORT = LS_RESB-LGORT AND
           IS_AFFW-CHARG = LS_RESB-CHARG AND
           IS_AFFW-SOBKZ = LS_RESB-SOBKZ AND
           IS_AFFW-BWART = LS_RESB-BWART AND
           IS_AFFW-ERFME = LS_RESB-ERFME.
*      yes, consistent, then add the qty
      ADD IS_AFFW-ERFMG TO LS_RESB-BDMNG.
      ADD IS_AFFW-ERFMG TO LS_RESB-ERFMG.
      MODIFY ET_RESB_I INDEX SY-TABIX FROM LS_RESB.

    ELSE.

*     no, this refer to different material
*     CHANGE ALSO THE AFFW RSPOS because it used already by an other
*     AFFW item!!!
      MOVE 9999 TO IS_AFFW-RSPOS.
*     check whether this entry is used (hope only by us)
      READ TABLE ET_RESB_I TRANSPORTING NO FIELDS
                         WITH KEY RSNUM = IS_AFFW-RSNUM
                                  RSPOS = IS_AFFW-RSPOS.
      WHILE SY-SUBRC IS INITIAL AND
          IS_AFFW-RSPOS NE 1.
        IS_AFFW-RSPOS = IS_AFFW-RSPOS - 1.
        READ TABLE ET_RESB_I TRANSPORTING NO FIELDS
                             WITH KEY RSNUM = IS_AFFW-RSNUM
                                      RSPOS = IS_AFFW-RSPOS.
      ENDWHILE.
*     please process this entry asap with COGI
      IF IS_AFFW-RSPOS NE 9999.
        IF IS_AFFW-RSPOS EQ 1.
*         do not update RESB
          WRITE: /1 'RESB should be checked manually',
                    LS_RESB-MATNR,
                    LS_RESB-RSNUM,
                    LS_RESB-RSPOS.
          EXIT.
        ENDIF.
        WRITE: /1 'Please process this entry in COGI asap:',
                  LS_RESB-MATNR,
                  LS_RESB-RSNUM,
                  LS_RESB-RSPOS.
      ENDIF.
      PERFORM CREATE_NEW_RESB USING IS_AFFW
                           CHANGING ET_RESB_I
                                    ET_AFFW_U
                                    ET_OUTPUT.
*     update AFFW rspos
      APPEND IS_AFFW TO ET_AFFW_U.

    ENDIF.  "if the resb entry does not match

  ENDIF. "if the

ENDFORM.                    "create_new_resb

*---------------------------------------------------------------------*
*       FORM modif_wrong_resb                                         *
*---------------------------------------------------------------------*
FORM MODIF_WRONG_RESB USING    IT_AFFW   LIKE LT_AFFW
                      CHANGING CS_RESB   TYPE RESB
                               CS_AFFW   TYPE AFFW
                               ET_RESB_I LIKE LT_RESB
                               ET_RESB_U LIKE LT_RESB
                               ET_AFFW_U LIKE LT_AFFW
                               ET_OUTPUT TYPE TY_TAB_AFFWRESB.

  DATA:
    LF_WEBLNR TYPE AFFW-WEBLNR,
    LF_SUBRC  TYPE SYSUBRC.

* basically, we have resb which does not match affw
* we should modify it.
* before that, we must check whether or not this actual entry
* is used in any affw entry (in this case, we shouldn't modify it
* - instead, create a new matching entry.

  SELECT SINGLE WEBLNR FROM AFFW INTO LF_WEBLNR
      WHERE RSNUM = CS_RESB-RSNUM AND
            RSPOS = CS_RESB-RSPOS AND
            MATNR = CS_RESB-MATNR AND
            WERKS = CS_RESB-WERKS AND
            LGORT = CS_RESB-LGORT AND
            CHARG = CS_RESB-CHARG AND
            SOBKZ = CS_RESB-SOBKZ AND
            BWART = CS_RESB-BWART AND
            ERFME = CS_RESB-ERFME AND
            FLG_ORIG = '1'.
  MOVE SY-SUBRC TO LF_SUBRC.
  READ TABLE ET_AFFW_U TRANSPORTING NO FIELDS
         WITH KEY RSNUM = CS_RESB-RSNUM
                  RSPOS = CS_RESB-RSPOS
                  MATNR = CS_RESB-MATNR
                  WERKS = CS_RESB-WERKS
                  LGORT = CS_RESB-LGORT
                  CHARG = CS_RESB-CHARG
                  SOBKZ = CS_RESB-SOBKZ
                  BWART = CS_RESB-BWART
                  ERFME = CS_RESB-ERFME.
  IF SY-SUBRC LT LF_SUBRC.
    MOVE SY-SUBRC TO LF_SUBRC.
  ENDIF.

  IF LF_SUBRC EQ 0. "or in database, or in affw_u

*   CHANGE ALSO THE AFFW RSPOS because it used already by an other
*   AFFW item!!!
    MOVE 9999 TO CS_AFFW-RSPOS.
*   check whether this entry is used (hope only by us)
    READ TABLE ET_RESB_I TRANSPORTING NO FIELDS
                         WITH KEY RSNUM = CS_AFFW-RSNUM
                                  RSPOS = CS_AFFW-RSPOS.
    WHILE SY-SUBRC IS INITIAL AND
          CS_AFFW-RSPOS NE 1.
      CS_AFFW-RSPOS = CS_AFFW-RSPOS - 1.
      READ TABLE ET_RESB_I TRANSPORTING NO FIELDS
                           WITH KEY RSNUM = CS_AFFW-RSNUM
                                    RSPOS = CS_AFFW-RSPOS.
    ENDWHILE.
*   please process this entry asap with COGI
    IF CS_AFFW-RSPOS NE 9999.
      IF CS_AFFW-RSPOS EQ 1.
*       do not update RESB
        WRITE: /1 'RESB should be checked manually',
                  CS_RESB-MATNR,
                  CS_RESB-RSNUM,
                  CS_RESB-RSPOS.
        EXIT.
      ENDIF.
      WRITE: /1 'Please process this entry in COGI asap:',
                CS_RESB-MATNR,
                CS_RESB-RSNUM,
                CS_RESB-RSPOS.
    ENDIF.
    PERFORM CREATE_NEW_RESB USING    CS_AFFW
                            CHANGING ET_RESB_I
                                     ET_AFFW_U
                                     ET_OUTPUT.
*   update AFFW rspos
    APPEND CS_AFFW TO ET_AFFW_U.

  ELSE.

*   this resb entry is wrong, but not used anywhere else
*   then correct the entry!!
    MOVE CS_AFFW-MATNR TO CS_RESB-MATNR.
    MOVE CS_AFFW-WERKS TO CS_RESB-WERKS.
    MOVE CS_AFFW-LGORT TO CS_RESB-LGORT.
    MOVE CS_AFFW-CHARG TO CS_RESB-CHARG.
    MOVE CS_AFFW-SOBKZ TO CS_RESB-SOBKZ.
    MOVE CS_AFFW-BWART TO CS_RESB-BWART.
    MOVE CS_AFFW-ERFME TO CS_RESB-ERFME.
    MOVE CS_AFFW-ERFME TO CS_RESB-MEINS.
    APPEND CS_RESB TO ET_RESB_U.

  ENDIF.

ENDFORM.                    "modif_wrong_resb



*&---------------------------------------------------------------------*
*&      Form  write_resb
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_RESB>  text
*----------------------------------------------------------------------*
FORM WRITE_RESB  USING IT_AFFW  LIKE LT_AFFW
                       CS_RESB  TYPE RESB.

  WRITE: /1 CS_RESB-RSNUM,
            CS_RESB-RSPOS,
            CS_RESB-MATNR,
            CS_RESB-WERKS,
            CS_RESB-LGORT,
            CS_RESB-CHARG,
            CS_RESB-SOBKZ,
            CS_RESB-ERFMG,
            CS_RESB-ERFME.

ENDFORM.                    " write_resb
*---------------------------------------------------------------------*
*       FORM modif_wrong_resb_with_q
*
*---------------------------------------------------------------------*
FORM MODIF_WRONG_RESB_WITH_Q USING    IF_QUANT  TYPE ERFMG
                             CHANGING CS_RESB   TYPE RESB
                                      ET_RESB_U LIKE LT_RESB.

* just take the sum of quantity of the corresponding AFFW
  CS_RESB-ERFMG = IF_QUANT.
  CS_RESB-BDMNG = IF_QUANT.

  APPEND CS_RESB TO ET_RESB_U.

ENDFORM.                    "modif_wrong_resb_with_q
*&---------------------------------------------------------------------*
*&      Form  send_reservation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0581   text
*      -->P_LT_RESB_D  text
*----------------------------------------------------------------------*
FORM SEND_RESERVATION USING    I_ACTION TYPE CHAR01
                               IT_RESB LIKE LT_RESB.
  DATA: LT_RES_HDR LIKE PPC_RES_HDR OCCURS 0,
        LS_RES_HDR LIKE PPC_RES_HDR,
        LS_RESB    LIKE RESB,
        L_TABIX    TYPE SYTABIX,
        BEGIN OF LT_TAB OCCURS 0,
         CONFLOGSYS  TYPE PPC_CONFLOGSYS,
         FLG_INFO_DEST TYPE  PPC_FLG_INFO_DEST,
         RESB LIKE LT_RESB,
       END OF LT_TAB.

  SELECT * INTO TABLE LT_RES_HDR
           FROM PPC_RES_HDR FOR ALL ENTRIES IN IT_RESB
           WHERE RSNUM = IT_RESB-RSNUM.

  LOOP AT LT_RES_HDR INTO LS_RES_HDR.
    READ TABLE LT_TAB WITH KEY CONFLOGSYS = LS_RES_HDR-CONFLOGSYS
                               FLG_INFO_DEST = LS_RES_HDR-FLG_INFO_DEST.
    IF SY-SUBRC EQ 0.
      L_TABIX = SY-TABIX.
      LOOP AT IT_RESB INTO LS_RESB.
        IF I_ACTION = 'D'.
          CLEAR: LS_RESB-BDMNG, LS_RESB-ERFMG, LS_RESB-ENMNG.
        ENDIF.
        APPEND LS_RESB TO LT_TAB-RESB[].
      ENDLOOP.
      MODIFY LT_TAB INDEX L_TABIX.
    ELSE.
      LT_TAB-CONFLOGSYS = LS_RES_HDR-CONFLOGSYS.
      LT_TAB-FLG_INFO_DEST = LS_RES_HDR-FLG_INFO_DEST.
      REFRESH LT_TAB-RESB.
      LOOP AT IT_RESB INTO LS_RESB.
        IF I_ACTION = 'D'.
          CLEAR: LS_RESB-BDMNG, LS_RESB-ERFMG.
        ENDIF.
        APPEND LS_RESB TO LT_TAB-RESB[].
      ENDLOOP.
      APPEND LT_TAB.
    ENDIF.
  ENDLOOP.

  LOOP AT LT_TAB.
    IF I_ACTION = 'D'.
      CALL FUNCTION 'PPC1IF_RESERV_DISTRIB'
        EXPORTING
          IF_FLT_VAL            = LT_TAB-FLG_INFO_DEST
          IF_LOGDESTSYS         = LT_TAB-CONFLOGSYS
        TABLES
          IT_DEP_REQUIRE_DELETE = LT_TAB-RESB[].
    ELSEIF I_ACTION = 'U'.
      CALL FUNCTION 'PPC1IF_RESERV_DISTRIB'
        EXPORTING
          IF_FLT_VAL            = LT_TAB-FLG_INFO_DEST
          IF_LOGDESTSYS         = LT_TAB-CONFLOGSYS
        TABLES
          IT_DEP_REQUIRE_CHANGE = LT_TAB-RESB[].
    ELSEIF I_ACTION = 'I'.
      CALL FUNCTION 'PPC1IF_RESERV_DISTRIB'
        EXPORTING
          IF_FLT_VAL            = LT_TAB-FLG_INFO_DEST
          IF_LOGDESTSYS         = LT_TAB-CONFLOGSYS
        TABLES
          IT_DEP_REQUIRE_CREATE = LT_TAB-RESB[].
    ENDIF.
  ENDLOOP.

ENDFORM.                    " send_reservation

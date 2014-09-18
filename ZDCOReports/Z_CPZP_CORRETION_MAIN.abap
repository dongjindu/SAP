************************************************************************
* Program Name      : Z_CPZP_CORRETION_MAIN
* Author            : IG.Moon
* Creation Date     : 7/9/2008
* Specifications By : Andy.Choi
* Pattern           : Update Program
* Description       : Main program for CPZP correction
* Modifications Log
* Date   Developer   Request ID    Description
************************************************************************
REPORT  Z_CPZP_CORRETION_MAIN  MESSAGE-ID ZMCO
LINE-SIZE 250 NO STANDARD PAGE HEADING.
TABLES : MLCD, CKMLHD, AUFK, CPZP, MARV .
DEFINE U_BREAK.
  IF NOT P_DEBUG IS INITIAL.
    BREAK-POINT.
  ENDIF.
END-OF-DEFINITION.
*/

DATA : BEGIN OF IT_MLCD OCCURS 0,
        MATNR LIKE MARA-MATNR,
        BWKEY LIKE CKMLHD-BWKEY,
       END OF IT_MLCD.
DATA: BEGIN OF IT_PCC OCCURS 0,
        OBJNR       LIKE AUFK-OBJNR,
        AUFNR       LIKE AUFK-AUFNR,
        PKOSA       LIKE CKMLMV013-PKOSA,  "Cost Collector
        KALNR_PROC  LIKE CKMLMV013-KALNR_PROC,
        PRWRK       LIKE CKMLMV013-PRWRK,
        PMATN       LIKE CKMLMV013-PMATN,
        VERID       LIKE CKMLMV013-VERID,
        KLVARP      LIKE AFKO-KLVARP,      "CostingVariant-plan
        KDAUF       LIKE CKMLMV013-KDAUF,
        KDPOS       LIKE CKMLMV013-KDPOS,
        KZBWS       LIKE CKMLMV013-KZBWS,
      END OF IT_PCC.


* not important!!!
DATA : PERCENTAGE TYPE P,
       $PROG_TEXT(50),$CURRENT_CNT(10),$TOTAL_CNT(10),$TEXT(40) .
DATA  : TOTAL_DOC_CNT TYPE I,$MOD TYPE I,
        CURRENT_DOC_CNT TYPE I.
DATA $MOD2 TYPE I.
DATA :PROC_TIME  TYPE P,
      TIME_START TYPE F,
      TIME_END   TYPE F,
      $PROC_TIME(10),
      I_CNT TYPE I .

*/

DATA: BEGIN OF IT_CPZP OCCURS 0,
        OBJNR LIKE CPZP-OBJNR,
        F_OBJNR LIKE CPZP-F_OBJNR,
        GJPER LIKE CPZP-GJPER,
        ISTMN LIKE CPZP-ISTMN,
        GMPER LIKE CPZP-GMPER,
        GMSUM LIKE CPZP-GMSUM,
      END OF IT_CPZP.

DATA : IT_CPZP_PRV LIKE IT_CPZP OCCURS 0 WITH HEADER LINE,
       IT_CPZP_CUR LIKE IT_CPZP OCCURS 0 WITH HEADER LINE.


DEFINE __CLS.                          " clear & refresh
  CLEAR &1.REFRESH &1.
END-OF-DEFINITION.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-SL1.
PARAMETERS:
  P_KOKRS  TYPE KOKRS DEFAULT 'H201'.
SELECT-OPTIONS :
  S_BWKEY FOR CKMLHD-BWKEY,
  S_MATNR FOR CKMLHD-MATNR,
  S_AUFNR FOR AUFK-AUFNR.

SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-SL1.
PARAMETERS:
  P_GJPER  TYPE CO_GJPER.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-SL2.
PARAMETERS:
  P_DEBUG  TYPE C AS CHECKBOX,
  P_ATREP  TYPE C AS CHECKBOX,
  P_LOCK   TYPE C AS CHECKBOX.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK B4S WITH FRAME TITLE TEXT-TS4.

PARAMETERS       P_CPZP TYPE C AS CHECKBOX  DEFAULT 'X'  USER-COMMAND
UCOM.
PARAMETERS       P_NOGO TYPE C AS CHECKBOX  DEFAULT 'X' MODIF ID NOG .

SELECTION-SCREEN END   OF BLOCK B4S.
SELECTION-SCREEN END OF BLOCK B3.

SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-T04.
PARAMETERS     : P_MAT  RADIOBUTTON GROUP REPT USER-COMMAND UCOMM
                                                DEFAULT 'X',
                 P_ACT  RADIOBUTTON GROUP REPT.
SELECTION-SCREEN END   OF BLOCK B4.


SELECTION-SCREEN BEGIN OF BLOCK B5.
PARAMETERS     : P_FORC AS CHECKBOX.
SELECTION-SCREEN END   OF BLOCK B5.

AT SELECTION-SCREEN OUTPUT.
  PERFORM MODIFY_SCREEN.

*///////////////////////////////////////////////////////////////////
*// start
*///////////////////////////////////////////////////////////////////
START-OF-SELECTION.

  PERFORM CHK_RUN_DATE.

* <Lock> {

  IF P_ATREP EQ 'X' AND P_LOCK NE 'X'.
    DATA L_ANSWER(1).
    PERFORM POP_UP USING
        'Program recommends, Do lock! the order# while updating.'
        'Do you want to lock the order# while processing?' 'X'
                   CHANGING L_ANSWER.
    CASE L_ANSWER.
      WHEN 'J'.
        P_LOCK = 'X'.
      WHEN 'A'.
        MESSAGE S000 WITH 'Processing was canceled by user.'.
        STOP.
    ENDCASE.
  ENDIF.
* }



* <only for performace improve.>
* - select plant
* {

  RANGES: R_BWKEY FOR CKMLHD-BWKEY.
  TABLES: T001K.
  R_BWKEY-SIGN = 'I'. R_BWKEY-OPTION = 'EQ'.
  SELECT * FROM T001K WHERE BUKRS = P_KOKRS.
    IF T001K-BWKEY IN S_BWKEY.
      R_BWKEY-LOW = T001K-BWKEY. APPEND R_BWKEY.
    ENDIF.
  ENDSELECT.
* }


* <Current Period>
* {
  DATA : P_BDATJ(4) TYPE N,
         P_POPER(3) TYPE N.
  P_BDATJ = P_GJPER DIV 1000.
  P_POPER = P_GJPER - ( P_BDATJ * 1000 ).

* }

* <Prevoius Period>
* {
  DATA : $YEAR(4) TYPE N,
         $PERI(3) TYPE N,
         IV_PRVPER TYPE CO_GJPER.

  IV_PRVPER = P_GJPER - 1.

  $YEAR = IV_PRVPER DIV 1000.
  $PERI = IV_PRVPER - ( $YEAR * 1000 ).

  IF $PERI EQ '000'.
    $YEAR = $YEAR - 1 .
    CONCATENATE $YEAR '012' INTO IV_PRVPER.
  ENDIF.
* }


  __CLS : IT_MLCD, IT_PCC.


  SELECT DISTINCT CKMLHD~MATNR CKMLHD~BWKEY
  INTO TABLE IT_MLCD
  FROM  MLCD
         INNER JOIN CKMLHD
         ON CKMLHD~KALNR = MLCD~KALNR
         INNER JOIN MARA AS C
         ON C~MATNR = CKMLHD~MATNR
         WHERE MLCD~BDATJ EQ P_BDATJ
           AND MLCD~POPER EQ P_POPER
           AND CKMLHD~BWKEY IN R_BWKEY
           AND CKMLHD~MATNR IN S_MATNR
           AND C~KZKFG EQ 'X'
  %_HINTS ORACLE 'FIRST_ROWS(10)'.

  IF SY-SUBRC NE 0.
    MESSAGE S000 WITH 'No data was found to check.'.
    STOP.
  ENDIF.

  SORT IT_MLCD.
  DELETE ADJACENT DUPLICATES FROM IT_MLCD.

  DESCRIBE TABLE IT_MLCD LINES TOTAL_DOC_CNT.
  $TOTAL_CNT = TOTAL_DOC_CNT.
  CLEAR CURRENT_DOC_CNT.

  DATA %ABAPLIST LIKE ABAPLIST OCCURS 0 WITH HEADER LINE.
  DATA GOGO(1).

  SELECT AUFK~OBJNR AUFK~AUFNR
         CKMLMV013~PKOSA   CKMLMV013~KALNR_PROC
         CKMLMV013~PRWRK   CKMLMV013~PMATN      CKMLMV013~VERID
         AFKO~KLVARP
     INTO CORRESPONDING FIELDS OF TABLE IT_PCC
     FROM CKMLMV013
        INNER JOIN AUFK
           ON AUFK~AUFNR   = CKMLMV013~PKOSA
        INNER JOIN AFKO
           ON AFKO~AUFNR   = AUFK~AUFNR
        INNER JOIN MKAL
           ON MKAL~MATNR   = CKMLMV013~PMATN
     FOR ALL ENTRIES IN IT_MLCD
     WHERE
           CKMLMV013~AUTYP = '05'
       AND CKMLMV013~PRWRK IN R_BWKEY
       AND CKMLMV013~PMATN = IT_MLCD-MATNR
       AND CKMLMV013~LOEKZ = SPACE       "deletion
       AND AUFK~AUFNR IN S_AUFNR
    %_HINTS ORACLE 'FIRST_ROWS(10)'.
  .       "PCC

  DESCRIBE TABLE IT_PCC LINES TOTAL_DOC_CNT.
  $TOTAL_CNT = TOTAL_DOC_CNT.
  CLEAR CURRENT_DOC_CNT.

  LOOP AT IT_PCC.

    ADD 1 TO CURRENT_DOC_CNT.
    $MOD = CURRENT_DOC_CNT MOD 10.
    I_CNT = 0.
    $CURRENT_CNT = CURRENT_DOC_CNT.

    CONCATENATE IT_PCC-AUFNR ':' $CURRENT_CNT '/' $TOTAL_CNT
    INTO $TEXT.
    CONDENSE $TEXT.
    CONCATENATE '...' $TEXT INTO $PROG_TEXT.
    PERCENTAGE = CURRENT_DOC_CNT / TOTAL_DOC_CNT * 100.
    PERFORM SHOW_PROGRESS USING $PROG_TEXT PERCENTAGE.

    IF P_CPZP EQ 'X'.
      PERFORM CHECK_CPZP  USING IT_PCC-PMATN
                                IT_PCC-OBJNR
                                IV_PRVPER
                             CHANGING GOGO.
      IF GOGO EQ 'X'.
        WRITE :/ '*',
                 IT_PCC-PMATN,'*',IT_PCC-AUFNR,
                 '*(Good data, skipped & not being locked!)'
                 COLOR COL_POSITIVE.
        CONTINUE.
      ELSE.
        IF P_NOGO EQ 'X'.
          WRITE :/ '*',
                   IT_PCC-PMATN,'*',IT_PCC-AUFNR,
                   '*(Inconsistency at CPZP, error !)'
                   COLOR COL_NEGATIVE.
          CONTINUE.
        ENDIF.
      ENDIF.

    ENDIF.


    CASE 'X'.
      WHEN P_MAT.
        SUBMIT Z_CPZP_CORRETION_COMP_NEW WITH P_MATNR EQ IT_PCC-PMATN
                                         WITH P_AUFNR EQ IT_PCC-AUFNR
                                         WITH P_WERKS EQ IT_PCC-PRWRK
                                         WITH P_GJPER EQ P_GJPER
                                         WITH P_DUBUG EQ P_DEBUG
                                         WITH P_ATREP EQ P_ATREP
                                         WITH P_LOCK EQ P_LOCK
                                         WITH P_VER  EQ IT_PCC-VERID
                                         WITH P_FORC EQ P_FORC
                                         EXPORTING LIST TO MEMORY
                                         AND RETURN .

      WHEN P_ACT.
        SUBMIT Z_CPZP_CORRETION_ACT      WITH P_MATNR EQ IT_PCC-PMATN
                                         WITH P_AUFNR EQ IT_PCC-AUFNR
                                         WITH P_WERKS EQ IT_PCC-PRWRK
                                         WITH P_GJPER EQ P_GJPER
                                         WITH P_DUBUG EQ P_DEBUG
                                         WITH P_ATREP EQ P_ATREP
                                         WITH P_LOCK EQ P_LOCK
                                         WITH P_VER  EQ IT_PCC-VERID
                                         WITH P_FORC EQ P_FORC
                                         EXPORTING LIST TO MEMORY
                                         AND RETURN.
    ENDCASE.

    WRITE :/ '*',
             IT_PCC-PMATN,'*',IT_PCC-AUFNR,
             '*'.

    CALL FUNCTION 'LIST_FROM_MEMORY'
         TABLES
              LISTOBJECT = %ABAPLIST
         EXCEPTIONS
              NOT_FOUND  = 1
              OTHERS     = 2.

    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CALL FUNCTION 'WRITE_LIST'
         TABLES
              LISTOBJECT = %ABAPLIST
         EXCEPTIONS
              EMPTY_LIST = 01.

  ENDLOOP.


END-OF-SELECTION.

*---------------------------------------------------------------------*
*       FORM SHOW_PROGRESS                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PF_TEXT                                                       *
*  -->  VALUE(PF_VAL)                                                 *
*---------------------------------------------------------------------*
FORM SHOW_PROGRESS USING    PF_TEXT
                            VALUE(PF_VAL).
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            PERCENTAGE = PF_VAL
            TEXT       = PF_TEXT.

ENDFORM.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  check_cpzp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GOGO  text
*----------------------------------------------------------------------*
FORM CHECK_CPZP  USING P_MATNR
                       P_OBJNR
                       P_PRVPER
                 CHANGING P_GOGO.

  __CLS : IT_CPZP, IT_CPZP_PRV, IT_CPZP_CUR .
  CLEAR P_GOGO.

  DATA $ERROR(1).

  SELECT OBJNR F_OBJNR GJPER
         SUM( ISTMN )
         SUM( GMPER )
         SUM( GMSUM )
         INTO TABLE IT_CPZP
                       FROM CPZP
                       WHERE OBJNR EQ P_OBJNR
                       AND ( GJPER EQ P_PRVPER OR GJPER EQ P_GJPER )
                       GROUP BY OBJNR F_OBJNR GJPER
                       .

  IF P_MAT EQ 'X'.
    DELETE IT_CPZP WHERE F_OBJNR CP 'KL*'.
  ELSE.
    DELETE IT_CPZP WHERE F_OBJNR NP 'KL*'.
  ENDIF.

  LOOP AT IT_CPZP.
    IF IT_CPZP-GJPER EQ P_PRVPER.
      MOVE IT_CPZP TO IT_CPZP_PRV.
      APPEND IT_CPZP_PRV.
    ELSE.
      MOVE IT_CPZP TO IT_CPZP_CUR.
      APPEND IT_CPZP_CUR.
    ENDIF.
  ENDLOOP.

  SORT IT_CPZP_CUR BY OBJNR F_OBJNR GJPER.

  DATA : $BEGIN LIKE CPZP-ISTMN,
         $END   LIKE CPZP-ISTMN,
        $NISTMN    TYPE IST_MENGE.

  LOOP AT IT_CPZP_PRV.

    $END =   IT_CPZP_PRV-ISTMN - IT_CPZP_PRV-GMSUM .

    READ TABLE IT_CPZP_CUR WITH KEY OBJNR   = IT_CPZP_PRV-OBJNR
                                    F_OBJNR = IT_CPZP_PRV-F_OBJNR
                                    GJPER   = P_GJPER.
    IF SY-SUBRC EQ 0.
      $BEGIN = IT_CPZP_CUR-ISTMN - IT_CPZP_CUR-GMPER .

*      $NISTMN = ( IT_CPZP_PRV-ISTMN - IT_CPZP_PRV-GMSUM ) +
*                           IT_CPZP_CUR-GMPER .
*
*      IF IT_CPZP_CUR-ISTMN <> $NISTMN.
*        U_BREAK.
*        $ERROR = 'X'.
*      ENDIF.
*
      IF $END <> $BEGIN.
        U_BREAK.
        $ERROR = 'X'.
      ENDIF.

*      IF $BEGIN <> $END.
*        U_BREAK.
*        $ERROR = 'X'.
*      ENDIF.
*    ELSE.
*      IF $END <> 0.
*        U_BREAK.
*        $ERROR = 'X'.
*      ENDIF.

    ENDIF.
  ENDLOOP.

  CHECK $ERROR EQ SPACE.
  P_GOGO = 'X'.
ENDFORM.                    " check_cpzp
*&---------------------------------------------------------------------*
*&      Form  POP_UP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0347   text
*      -->P_0348   text
*      -->P_0349   text
*      <--P_L_ANSWER  text
*----------------------------------------------------------------------*
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
*&      Form  chk_run_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHK_RUN_DATE.

  DATA: H_DONTPANIC LIKE SY-DATLO.

  GET PARAMETER ID 'DONTPANIC' FIELD H_DONTPANIC.
  IF H_DONTPANIC = SY-DATLO.
    EXIT.
  ENDIF.

  DATA : $YEAR(4) TYPE N,
         $MON(3) TYPE N,
         YVPER TYPE CO_GJPER.

  SELECT SINGLE * FROM MARV WHERE BUKRS EQ P_KOKRS.
  IF SY-SUBRC EQ 0.
    $YEAR = MARV-LFGJA.
    $MON = MARV-LFMON.
    CONCATENATE $YEAR $MON INTO YVPER.

    IF P_GJPER <> YVPER.

      $YEAR = MARV-VMGJA.
      $MON = MARV-VMMON.
      CONCATENATE $YEAR $MON INTO YVPER.
      IF P_GJPER <> YVPER.
 MESSAGE E000 WITH 'The date must be greater or equal to current Date!'.
        STOP.
      ENDIF.
    ELSE.
      EXIT.
    ENDIF.

  ELSE.
    MESSAGE E000 WITH 'Invalid Company!'.
    STOP.
  ENDIF.


*    yvper = sy-datum(6).
*
*    $year = yvper DIV 100.
*    $peri = yvper - ( $year * 100 ).
*
*    CONCATENATE $year $peri INTO yvper.
*    IF p_gjper < yvper.
*
*
* MESSAGE e000 WITH 'The date must be greater or equal to current Date!'
*.
*      STOP.
*    ENDIF.
*

ENDFORM.                    " chk_run_date
*&---------------------------------------------------------------------*
*&      Form  modify_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MODIFY_SCREEN.

  LOOP AT SCREEN.
    IF SCREEN-GROUP1 = 'NOG'.
      IF P_CPZP EQ 'X'.
        SCREEN-INVISIBLE = 0.
      ELSE.
        SCREEN-INVISIBLE = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " modify_screen

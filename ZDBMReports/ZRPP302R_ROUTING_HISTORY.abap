************************************************************************
* Program Name      : ZRPP302R_ROUTING_HISTORY
* Author            : Bongsoo, Kim
* Creation Date     : 2004.01.16.
* Specifications By : Bongsoo, Kim
* Pattern           : 1.1
* Development Request No : UD1K906001
* Addl Documentation:
* Description       : Display History of Reference rate routing
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZRPP302R_ROUTING_HISTORY
                NO STANDARD PAGE HEADING
                LINE-SIZE  120
*                LINE-COUNT 65
                MESSAGE-ID ZMBM.
*----------------------------------------------------------------------*
* STANDARD-TABLE AREA ( TABLE)
*----------------------------------------------------------------------*
TABLES: PLAS,  "Task list - selection of operations/activities
        PLKO,  "Task list - header
        PLPO,  "Task list - operation/activity
        MKAL.  "Production Versions of Material
*----------------------------------------------------------------------*
* INTERNAL-TABLE AREA
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_PLKO OCCURS 0,
        WERKS TYPE PLKO-WERKS, "Plant
        PLNTY TYPE PLKO-PLNTY, "Task list type
        PLNNR TYPE PLKO-PLNNR, "Key for task list group
        PLNAL TYPE PLKO-PLNAL, "Group counter
        KTEXT TYPE PLKO-KTEXT, "Task list description
        VERWE TYPE PLKO-VERWE, "Task list usage
        STATU TYPE PLKO-STATU, "Status
        ANDAT TYPE PLKO-ANDAT, "Date record created on
        ANNAM TYPE PLKO-ANNAM, "User who created record
      END   OF IT_PLKO.

*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_WERKS FOR PLKO-WERKS,
                S_PLNNR FOR PLKO-PLNNR, "Key for task list group
                S_ANDAT FOR PLKO-ANDAT OBLIGATORY, "
                S_ANNAM FOR PLKO-ANNAM. "User who created record
SELECTION-SCREEN END   OF BLOCK B1.
*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*
************************************************************************
* INITIALIZATION
************************************************************************
INITIALIZATION.
  PERFORM INITIALIZATION.
************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
  PERFORM READ_PROCESS.
  PERFORM WRITE_PROCESS.
************************************************************************
* END-OF-SELECTION
************************************************************************
END-OF-SELECTION.
************************************************************************
* TOP-OF-PAGE
************************************************************************
TOP-OF-PAGE.
  PERFORM TOP_OR_PAGE.
************************************************************************
* END-OF-PAGE
************************************************************************
END-OF-PAGE.
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION.
  CONCATENATE SY-DATUM(6) '01' INTO S_ANDAT-LOW.
  S_ANDAT-HIGH = SY-DATUM.
  S_ANDAT-SIGN = 'I'.
  S_ANDAT-OPTION = 'BT'.
  APPEND S_ANDAT.
  S_WERKS-LOW = 'P001'.
  S_WERKS-SIGN = 'I'.
  S_WERKS-OPTION = 'EQ'.
  APPEND S_WERKS.
ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM READ_PROCESS.
* SELECT MAPL
  SELECT A~WERKS     "Plant
         A~PLNTY     "Task list type
         A~PLNNR     "Key for task list group
         A~PLNAL     "Group counter
         B~KTEXT     "Task list description
         B~VERWE     "Task list usage
         B~STATU     "Status
         A~ANDAT     "Date record created on
         A~ANNAM     "User who created record
*       FROM PLKO
       FROM MAPL AS A INNER JOIN PLKO AS B
                      ON  A~PLNTY EQ B~PLNTY AND
                       A~PLNNR EQ B~PLNNR
                      AND A~PLNAL EQ B~PLNAL
                      AND A~ZAEHL EQ B~ZAEHL
       INTO TABLE IT_PLKO
       WHERE A~WERKS IN S_WERKS
       AND   A~PLNNR IN S_PLNNR
       AND   A~ANDAT IN S_ANDAT
       AND   A~ANNAM IN S_ANNAM
       AND   A~PLNTY EQ 'R'
       AND   A~LOEKZ NE 'X'.
  IF SY-SUBRC NE 0.
    FORMAT RESET.
    WRITE: TEXT-002 COLOR 6.
  ENDIF.
* SELECT PLKO
  SELECT  WERKS     "Plant
          PLNTY     "Task list type
          PLNNR     "Key for task list group
          PLNAL     "Group counter
          KTEXT     "Task list description
          VERWE     "Task list usage
          STATU     "Status
          ANDAT     "Date record created on
          ANNAM     "User who created record
*       FROM PLKO
       FROM PLKO
       APPENDING TABLE IT_PLKO
       WHERE  WERKS IN S_WERKS
       AND    PLNNR IN S_PLNNR
       AND    ANDAT IN S_ANDAT
       AND    ANNAM IN S_ANNAM
       AND    PLNTY EQ 'M'
       AND    LOEKZ NE 'X'.
  IF SY-SUBRC NE 0.
    FORMAT RESET.
    WRITE: TEXT-002 COLOR 6.
  ENDIF.
ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  WRITE_PROCESS
*&---------------------------------------------------------------------*
FORM WRITE_PROCESS.
  FORMAT RESET.
  SORT IT_PLKO BY WERKS PLNTY ANDAT PLNNR PLNAL VERWE STATU ANNAM.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  LOOP AT IT_PLKO.
    WRITE: /(06) IT_PLKO-WERKS,    "Plant
            (05) IT_PLKO-PLNTY,    "Task list type
            (08) IT_PLKO-PLNNR,    "Key for task list group
            (14) IT_PLKO-PLNAL,    "Group counter
            (40) IT_PLKO-KTEXT,    "Task list description
            (06) IT_PLKO-VERWE,    "Task list usage
            (07) IT_PLKO-STATU,    "Status
            (12) IT_PLKO-ANDAT,    "Date record created on
            (12) IT_PLKO-ANNAM.    "User who created record
  ENDLOOP.
  FORMAT COLOR OFF.
ENDFORM.                    " WRITE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  TOP_OR_PAGE
*&---------------------------------------------------------------------*
FORM TOP_OR_PAGE.
  FORMAT RESET.
*  WRITE: /20(41) TEXT-003 COLOR 3 .
  FORMAT COLOR COL_GROUP INTENSIFIED OFF.
  WRITE: / TEXT-016, (15) S_WERKS-LOW, (01) TEXT-017, (15) S_WERKS-HIGH.
  WRITE: / TEXT-015, (15) S_PLNNR-LOW, (01) TEXT-017, (15) S_PLNNR-HIGH.
  WRITE: / TEXT-013, (15) S_ANDAT-LOW, (01) TEXT-017, (15) S_ANDAT-HIGH.
  WRITE: / TEXT-014, (15) S_ANNAM-LOW, (01) TEXT-017, (15) S_ANNAM-HIGH.
  FORMAT COLOR OFF.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: /(06) TEXT-004,    "Plant
          (05) TEXT-005,    "Task list type
          (08) TEXT-006,    "Key for task list group
          (14) TEXT-007,    "Group counter
          (40) TEXT-008,    "Task list description
          (06) TEXT-009,    "Task list usage
          (07) TEXT-010,    "Status
          (12) TEXT-011,    "Date record created on
          (12) TEXT-012.    "User who created record
  FORMAT COLOR OFF.
ENDFORM.                    " TOP_OR_PAGE

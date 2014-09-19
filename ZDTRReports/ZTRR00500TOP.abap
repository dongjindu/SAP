*&---------------------------------------------------------------------*
*&  Include           ZTRR00500TOP
*&---------------------------------------------------------------------*

*Define
DEFINE ICLEAR.
  CLEAR : &1, &1[].
END-OF-DEFINITION.

DEFINE IAPPEND.
  APPEND : &1.
  CLEAR  : &1.
END-OF-DEFINITION.

DEFINE ICOLLECT.
  COLLECT : &1.
  CLEAR   : &1.
END-OF-DEFINITION.

DATA: BEGIN OF HIER_TB OCCURS 100.
        INCLUDE STRUCTURE TKCHS.
DATA: END OF HIER_TB.

DATA: BEGIN OF HIER_DB OCCURS 100.
        INCLUDE STRUCTURE TKCHA.
DATA: END OF HIER_DB.

DATA : BEGINNING  LIKE  FDSR     OCCURS 0 WITH HEADER LINE,
       PLAN       LIKE  FDSR     OCCURS 0 WITH HEADER LINE,
       ACTUAL     LIKE  FDSR     OCCURS 0 WITH HEADER LINE,
       ENDING     LIKE  FDSR     OCCURS 0 WITH HEADER LINE,
       DATE       LIKE  SY-DATUM OCCURS 0 WITH HEADER LINE,
       ACT_DATE   LIKE  SY-DATUM OCCURS 0 WITH HEADER LINE,
       PLAN_DATE  LIKE  SY-DATUM OCCURS 0 WITH HEADER LINE,

       BEGIN OF GT_DATE OCCURS 0,
         INDEX(2) TYPE N,
         DATUM LIKE SY-DATUM,
       END OF GT_DATE.

DATA : FIELDNAME(30).

CLASS LCL_APPLICATION DEFINITION DEFERRED.

TYPES: ITEM_TABLE_TYPE LIKE STANDARD TABLE OF MTREEITM
       WITH DEFAULT KEY.

DATA : G_CUSTOM_CONTAINER  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       G_GRID              TYPE REF TO CL_GUI_COLUMN_TREE,
       GT_NODE_TABLE       TYPE TREEV_NTAB WITH HEADER LINE,
       GT_ITEM_TABLE       TYPE ITEM_TABLE_TYPE WITH HEADER LINE,
       GS_HIERARCHY_HEADER TYPE TREEV_HHDR.
DATA : G_EVENTS            TYPE CNTL_SIMPLE_EVENTS.
DATA : G_EVENT             TYPE CNTL_SIMPLE_EVENT.
DATA : G_APPLICATION       TYPE REF TO LCL_APPLICATION.
DATA : OK_CODE LIKE SY-UCOMM.

DATA : FIRST_MONTH(1)       VALUE  'D',
       SECOND_MONTH(1)      VALUE  'M',
       THIRD_MONTH(1)       VALUE  'M',
       LATER(1)             VALUE  ' ',
       ALL_DAY              VALUE  SPACE,
       G_WAERS              LIKE T001-WAERS,
       G_NODE_KEY_TABLE     TYPE TREEV_NKS WITH HEADER LINE.

CONSTANTS : C_DAILY         TYPE ZPLAN VALUE '1',
            C_MONTHLY       TYPE ZPLAN VALUE '2',
            C_QUARTERLY     TYPE ZPLAN VALUE '3',
            C_YEARLY        TYPE ZPLAN VALUE '4',
            C_BEGIN(12)                VALUE 'Begin',
            C_END(12)                  VALUE 'End'.

*---------------------------------------------------------------------*
*  Selection Screen                                                   *
*---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE  TEXT-T01.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) text-h01.
PARAMETERS: p_bukrs  LIKE t001-bukrs   OBLIGATORY  MEMORY ID buk.
SELECTION-SCREEN COMMENT 52(40) p_butxt.
SELECTION-SCREEN END OF LINE.

PARAMETERS: P_GJAHR  TYPE GJAHR DEFAULT SY-DATUM(4),
            P_MONTH  TYPE MONTH DEFAULT SY-DATUM+4(2),
            p_kdate  like bsis-budat DEFAULT SY-DATUM OBLIGATORY.
SELECTION-SCREEN skip 1.
*parameters: p_act as checkbox.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-T02.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 01(11) TEXT-T21 FOR FIELD P_SKALV. "Skalierung
PARAMETERS: P_SKALV TYPE TS70SKAL_VOR  DEFAULT '0'.
SELECTION-SCREEN COMMENT 16(16) TEXT-T22 FOR FIELD P_DECIM. "Nachkommast
PARAMETERS: P_DECIM TYPE TS70SKAL_NACH DEFAULT '2'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-T03.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: P_R1 RADIOBUTTON GROUP g1  DEFAULT 'X' USER-COMMAND F1. "
SELECTION-SCREEN   COMMENT    3(27) text-h03.   "Planning
PARAMETERS: P_R2 RADIOBUTTON GROUP g1 .
SELECTION-SCREEN   COMMENT    33(15) text-h04.  "display
SELECTION-SCREEN END OF LINE.

PARAMETERS: P_HIER  LIKE TKCHH-ID2 DEFAULT 'H01'
                    VISIBLE LENGTH 3 NO-DISPLAY ,
***         P_R1 RADIOBUTTON GROUP G1 DEFAULT 'X' USER-COMMAND F1,
***         P_R2 RADIOBUTTON GROUP G1,
*//         Planning period
            P_PDAT1 LIKE ZTTR0008-PDAT1 MODIF ID G1 DEFAULT SY-DATUM,
            P_SEQNO LIKE ZTTR0008-SEQNO MODIF ID G1.
*//         "Sequence Number

SELECTION-SCREEN END OF BLOCK B3.
SELECTION-SCREEN END   OF BLOCK B1.

*---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  IF P_R1 = 'X'.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = 'G1'.
        SCREEN-INPUT     = 0.
        SCREEN-INVISIBLE = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_SEQNO.

  DATA : GT_SCRFIELD LIKE  DYNPREAD OCCURS 0 WITH HEADER LINE.
  GT_SCRFIELD-FIELDNAME = 'P_GJAHR'.
  IAPPEND GT_SCRFIELD.
  GT_SCRFIELD-FIELDNAME = 'P_PDAT1'.
  IAPPEND GT_SCRFIELD.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      DYNAME             = SY-CPROG
      DYNUMB             = SY-DYNNR
      TRANSLATE_TO_UPPER = 'X'
    TABLES
      DYNPFIELDS         = GT_SCRFIELD.

  READ TABLE GT_SCRFIELD WITH KEY FIELDNAME = 'P_GJAHR'.
  IF SY-SUBRC = 0 AND GT_SCRFIELD-FIELDVALUE IS NOT INITIAL.
    P_GJAHR = GT_SCRFIELD-FIELDVALUE.
  ENDIF.

  READ TABLE GT_SCRFIELD WITH KEY FIELDNAME = 'P_PDAT1'.
  IF SY-SUBRC = 0 AND GT_SCRFIELD-FIELDVALUE IS NOT INITIAL.

    CALL FUNCTION 'RP_FORMATING_DATE'
      EXPORTING
        DATE_I       = GT_SCRFIELD-FIELDVALUE
      IMPORTING
        DATE_O       = P_PDAT1
      EXCEPTIONS
        DATE_INVALID = 1
        OTHERS       = 2.
  ENDIF.


  DATA : BEGIN OF GT_SEQ OCCURS 0,
           SEQNO LIKE ZTTR0008-SEQNO,
           ERNAM LIKE ZTTR0008-ERNAM,
           ERDAT LIKE ZTTR0008-ERDAT,
           ERZET LIKE ZTTR0008-ERZET,
           AENAM LIKE ZTTR0008-AENAM,
           AEDAT LIKE ZTTR0008-AEDAT,
           AEZET LIKE ZTTR0008-AEZET,
         END OF GT_SEQ.

  DATA: L_TITLE(40).

  ICLEAR GT_SEQ.

  SELECT DISTINCT SEQNO ERNAM ERDAT ERZET AENAM AEDAT AEZET
    INTO CORRESPONDING FIELDS OF TABLE GT_SEQ
    FROM ZTTR0008
   WHERE BUKRS = P_BUKRS
     AND PDAT1 = P_PDAT1
     AND GJAHR = P_GJAHR
     AND ZTYPE = C_DAILY.
  SORT GT_SEQ.

  CONCATENATE 'Plan Version :' P_PDAT1 INTO L_TITLE.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'SEQNO'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'P_SEQNO'
      WINDOW_TITLE    = L_TITLE
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = GT_SEQ
    EXCEPTIONS
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2
      OTHERS          = 3.
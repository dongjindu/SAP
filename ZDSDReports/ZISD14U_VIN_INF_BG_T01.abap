*----------------------------------------------------------------------*
*   INCLUDE ZISD14U_VIN_INF_T01                                        *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* TABLE
*----------------------------------------------------------------------*
TABLES: AUSP, CABN.
*----------------------------------------------------------------------
* TYPES
*----------------------------------------------------------------------
TYPE-POOLS: SLIS, KKBLO, VRM.
*--------------------------------------------------------------------
* HEADER ITAB
*--------------------------------------------------------------------
****DATA :   IT_HEADER1(90) OCCURS 0 WITH HEADER LINE,
****         IT_HEADER2(90) OCCURS 0 WITH HEADER LINE,
****         IT_HEADER3(90) OCCURS 0 WITH HEADER LINE.
****
****DATA :   P_TEXT1(30)  TYPE C,
****         P_TEXT2(30)  TYPE C,
****         P_TEXT3(30)  TYPE C,
****         P_TEXT4(30)  TYPE C.
*----------------------------------------------------------------------*
* INTERNAL TABLE
*----------------------------------------------------------------------*
DATA : BEGIN OF ITAB1 OCCURS 0,
          OBJEK  LIKE AUSP-OBJEK,
          ATINN  LIKE AUSP-ATINN,
          ATWRT  LIKE AUSP-ATWRT,
          ATFLV  LIKE AUSP-ATFLV,
       END OF ITAB1.

DATA : BEGIN OF ITAB3 OCCURS 0,
          OBJEK  LIKE AUSP-OBJEK,
          ATINN  LIKE AUSP-ATINN,
          ATWRT  LIKE AUSP-ATWRT,
          ATFLV  LIKE AUSP-ATFLV,
       END OF ITAB3.

  DATA: BEGIN OF IT_OBJEK OCCURS 0,
            OBJEK LIKE AUSP-OBJEK,
        END OF IT_OBJEK.

  DATA: L_ATINN1 LIKE CABN-ATINN,
*      L_ATINN2 LIKE CABN-ATINN,
        L_ATINN3 LIKE CABN-ATINN,
        L_ATINN4 LIKE CABN-ATINN,
*      L_ATINN5 LIKE CABN-ATINN,
*      L_ATINN6 LIKE CABN-ATINN,
        L_ATINN7 LIKE CABN-ATINN,
        L_ATINN8 LIKE CABN-ATINN,
        L_ATINN9 LIKE CABN-ATINN,
        L_ATINN10 LIKE CABN-ATINN,
*      L_ATINN11 LIKE CABN-ATINN,
        L_ATINN12 LIKE CABN-ATINN,
        L_ATINN13 LIKE CABN-ATINN,
        L_ATINN14 LIKE CABN-ATINN,
        L_ATINN15 LIKE CABN-ATINN.

DATA : BEGIN OF ITAB2 OCCURS 0,

          F1(17), "VIN
          F2(5),  "MANUFACURER
          F3(5),  "DISTRUBUTOR CODE
          F4(8),  "PRODUCTION DATE
          F5(8),  "SHIPPING DATE
          F6(20), "VEHICLE SPEC
          F7(15), "WORK ORDER NUMBER
          F8(11), "ENGINE NUMBER
          F9(1),  "AIR/CON CODE
          F10(1), "TRANSMISSION CODE
          F11(1), "SPARE FLAG
          F12(5), "KEY NUMBER

       END OF ITAB2.

DATA : ETAB  LIKE ITAB OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*
* BDC TABLE
*----------------------------------------------------------------------*
DATA : BEGIN OF BDCDATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA : END OF BDCDATA.

DATA : BEGIN OF MESSTAB OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA : END OF MESSTAB.

*-------------------------------------------------------------------
* General Data FOR ALV.
*--------------------------------------------------------------------
*TYPES: BEGIN OF SLIS_FIELDCAT_ALV.
*INCLUDE TYPE SLIS_FIELDCAT_MAIN.
*INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
*TYPES: END OF SLIS_FIELDCAT_ALV.
*
*DATA: W_STATUS TYPE SLIS_FORMNAME VALUE 'STATUS_SET'.
*DATA: GT_FIELDCAT    TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE ,
*      LT_FIELDCAT    TYPE  SLIS_FIELDCAT_ALV,
*      GS_LAYOUT      TYPE SLIS_LAYOUT_ALV,
*      G_REPID        LIKE SY-REPID,
*      T_VARIANT      TYPE DISVARIANT,
*      G_EXIT         TYPE C,
*      LT_EVENT       TYPE SLIS_T_EVENT,
*      LT_SORTINFO    TYPE SLIS_T_SORTINFO_ALV,
*      LT_EXTAB       TYPE SLIS_T_EXTAB,
*      GT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER,
*      SPEC_VARIANT   TYPE DISVARIANT,    "specific variant
*      GS_VARIANT     TYPE DISVARIANT.    "finally chosen variant.
*
*DATA: GT_EVENTS      TYPE SLIS_T_EVENT,
*      GT_LIST_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER,
*      G_STATUS_SET   TYPE SLIS_FORMNAME VALUE 'PF_STATUS_SET',
*      G_USER_COMMAND TYPE SLIS_FORMNAME VALUE 'USER_COMMAND',
*      G_TOP_OF_PAGE  TYPE SLIS_FORMNAME VALUE 'TOP_OF_PAGE',
*      G_TOP_OF_LIST  TYPE SLIS_FORMNAME VALUE 'TOP_OF_LIST',
*      G_END_OF_LIST  TYPE SLIS_FORMNAME VALUE 'END_OF_LIST'.
*
*
*TYPES: BEGIN OF SLIS_EXTAB,
*         FCODE LIKE RSMPE-FUNC,
*       END OF SLIS_EXTAB.
*
**types: slis_t_extab type slis_extab occurs 0.
*DATA: GT_EXCLUDING TYPE SLIS_EXTAB OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*
* LIST BOX
*----------------------------------------------------------------------*
*TYPES: BEGIN OF PTY_S_LIST,
*          ZFORDTP(6) TYPE C,
*          VALUE      TYPE VAL_TEXT,
*       END OF PTY_S_LIST.
*
*DATA: PT_LIST   TYPE TABLE OF PTY_S_LIST   WITH HEADER LINE.
*
*DATA: NAME   TYPE VRM_ID,    "LISTBOX? ??
*      XBOX   TYPE VRM_VALUES,
*      VALUE  LIKE LINE OF XBOX.
*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* RANGE TABLE
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* GLOBAL VARIABLE
*----------------------------------------------------------------------*
DATA : W_CHECK(1).
DATA : W_ANSWER(1).

DATA : W_N_8(8) TYPE N.

DATA : VARIANT LIKE INDX-SRTFD VALUE 'ISD14_01'.
*----------------------------------------------------------------------*
* CONSTANT TABLE
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_DATE  FOR AUSP-ATWRT NO-DISPLAY. " ATFLV
SELECTION-SCREEN END OF BLOCK B1 .

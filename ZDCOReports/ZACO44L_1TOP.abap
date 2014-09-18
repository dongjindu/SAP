*----------------------------------------------------------------------*
*   INCLUDE ZACO44L_1TOP                                               *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
** Tables
TABLES : ZTCO_EBUSPLANBOM, STKO, MARA, CE0H201, MKAL, PLKO, MAPL.

** Internal Table
* For Production Version/Routing
DATA : IT_MKAL LIKE STANDARD TABLE OF MKAL
               WITH HEADER LINE
               WITH DEFAULT KEY .
* For Update/Change
DATA : IT_ZTCO_EBUSPLANBOM
               LIKE STANDARD TABLE OF ZTCO_EBUSPLANBOM
               WITH HEADER LINE
               WITH DEFAULT KEY .

*----------------------------------------------------------------------*
*   Macro                                                *
*----------------------------------------------------------------------*
DEFINE EXPLODE_BOM.
  LOOP AT &1.
    CLEAR : IT_L_STB[], IT_L_STB.
* Explode :
* A Developer for BOM in PP module advised
    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
      EXPORTING
        CAPID                       = LV_CAPID
        DATUV                       = P_DATUV
        EHNDL                       = '1'
        MKTLS                       = 'X'
        MEHRS                       = 'X'
        MMORY                       = '1'
        MTNRV                       = &1-MATNR
        STLAN                       = '1' "BOM Usage
*       STPST                       = 0
        SVWVO                       = 'X'
        WERKS                       = &1-WERKS
        VRSVO                       = 'X'
*     IMPORTING
*       TOPMAT                      =
*       DSTST                       =
      TABLES
        STB                         = IT_L_STB
*       MATCAT                      =
      EXCEPTIONS
        ALT_NOT_FOUND               = 1
        CALL_INVALID                = 2
        MATERIAL_NOT_FOUND          = 3
        MISSING_AUTHORIZATION       = 4
        NO_BOM_FOUND                = 5
        NO_PLANT_DATA               = 6
        NO_SUITABLE_BOM_FOUND       = 7
        CONVERSION_ERROR            = 8
        OTHERS                      = 9.

    IF SY-SUBRC <> 0.
*     Do Nothing
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    LOOP AT IT_L_STB WHERE MTART = 'HALB'
                       AND DUMPS = SPACE.
      IT_L_MAT-MATNR = IT_L_STB-IDNRK.
      IT_L_MAT-WERKS = IT_L_STB-WERKS.
*     IT_L_MAT-VERID = IT_L_STB-VERID.

      COLLECT IT_L_MAT.
      CLEAR   IT_L_MAT.
      CLEAR   IT_L_STB.
    ENDLOOP.
    CLEAR &1.
  ENDLOOP.

  SORT IT_L_MAT BY MATNR  WERKS.

  DELETE ADJACENT DUPLICATES FROM IT_L_MAT.

END-OF-DEFINITION.

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_MATNR FOR  MARA-MATNR MEMORY   ID MAT.
SELECTION-SCREEN END OF BLOCK BL1.

SELECTION-SCREEN BEGIN OF BLOCK BL2 WITH FRAME TITLE TEXT-002.
PARAMETERS :     P_DATUV LIKE STKO-DATUV OBLIGATORY
                         DEFAULT SY-DATUM.
PARAMETERS :     P_GJAHR LIKE CE0H201-GJAHR
                         MEMORY   ID GJR OBLIGATORY.
PARAMETERS :     P_BOMTY LIKE ZTCO_EBUSPLANBOM-BOMTYPE default 'P'
                                         OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BL2.
SELECTION-SCREEN SKIP 1.

PARAMETERS :     P_UP   RADIOBUTTON GROUP RA01,
                 P_NEW  RADIOBUTTON GROUP RA01.

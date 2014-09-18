

************************************************************************
* Program Name      : ZRPM12_WEEKEND_SCH_PLAN_ORD
* Author            : Myoung ho, Park
* Creation Date     : 2003.11.24.
* Specifications By :
* Pattern           : Report 1-1
* Development Request No :
* Addl Documentation:
* Description       : Print Daily work order list (SmartForm)and
*                     Countermeasure report(Excel)
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 03/29/05    Furong Wang               Add in long text in work order
* 08.26.2014  Victor      Smartform change : ZSPM01_WORK_ORDER2_LTEXT
*                       ->keep original: ZSPM01_WORK_ORDER2_LTEXT_ORGI
*
************************************************************************
REPORT ZRPM12_WEEKEND_SCH_PLAN_ORD.

TABLES: ZSPM_WOKO,   "//Work Order Header
        ZSPM_WOPO,  "//Work Order Items
        VIAUFKST,   "//PM Order Selection by Status
        DIAUFK,     "//PM Order
        RIHEA,      "//PM hierarchy selection/list screen
        IFLO,       "//Functional Location (View)
        VIAFVC,     "//PM: MaintOperations
        VIQMEL,     "//Notification Header
        AFVC,       "//Operation within an order
        AFRU,       "//Order completion confirmations
        AFKO,
        EQKT,
        V_USERNAME,
        IFLOTX,
        IHPA,
        USR02,
        ZLONGTEXT,
        PA0001,
        DIADR,
        QMMA,
        USER_ADDR.

*** internal table for orders list
*DATA : IT_TEMP_ORDER LIKE ZSPM_WOPO OCCURS 0 WITH HEADER LINE.
* Modification by 100565
DATA: BEGIN OF IT_TEMP_ORDER OCCURS 0.
        INCLUDE STRUCTURE ZSPM_WOPO.
DATA: AUSVN LIKE VIQMEL-AUSVN,
      AUSBS LIKE VIQMEL-AUSBS,
      AUZTV LIKE VIQMEL-AUZTV,
      AUZTB LIKE VIQMEL-AUZTB,
      AUSZT LIKE VIQMEL-AUSZT,
      MAUEH LIKE VIQMEL-MAUEH,
      INTERVAL TYPE I,
      IDAUR LIKE AFRU-IDAUR,
      EQKTX LIKE EQKT-EQKTX,
      PLTXT LIKE IFLOTX-PLTXT,
*       ENAME LIKE PA0001-ENAME,
      GSTRP LIKE CAUFVD-GSTRP,
      GLTRP LIKE CAUFVD-GLTRP,
      GSUZP LIKE CAUFVD-GSUZP,
      GLUZP LIKE CAUFVD-GLUZP,
      OBJNR LIKE VIQMEL-OBJNR,
      NAME_TEXTC LIKE USER_ADDR-NAME_TEXTC,
  END OF IT_TEMP_ORDER.
DATA: ZTRAN(4).
*DATA : IT_ORDER LIKE ZSPM_WOPO OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF IT_ORDER OCCURS 0.
        INCLUDE STRUCTURE ZSPM_WOPO.
DATA: AUSVN LIKE VIQMEL-AUSVN,
      AUSBS LIKE VIQMEL-AUSBS,
      AUZTV LIKE VIQMEL-AUZTV,
      AUZTB LIKE VIQMEL-AUZTB,
      AUSZT LIKE VIQMEL-AUSZT,
      MAUEH LIKE VIQMEL-MAUEH,
      INTERVAL TYPE I,
      IDAUR LIKE AFRU-IDAUR,
      EQKTX LIKE EQKT-EQKTX,
      PLTXT LIKE IFLOTX-PLTXT,
*       ENAME LIKE PA0001-ENAME,
      GSTRP LIKE CAUFVD-GSTRP,
      GLTRP LIKE CAUFVD-GLTRP,
      GSUZP LIKE CAUFVD-GSUZP,
      GLUZP LIKE CAUFVD-GLUZP,
       OBJNR LIKE VIQMEL-OBJNR,
       NAME_TEXTC LIKE USER_ADDR-NAME_TEXTC,
  END OF IT_ORDER.

* end of modification
INCLUDE <ICON>.
DATA : IT_ORDER_NUM LIKE ZSPM_WOPO OCCURS 0 WITH HEADER LINE.

*** internal table for selected opreations list
DATA : IT_ZSPM_WOPO LIKE IT_ORDER OCCURS 0 WITH HEADER LINE.

*** internal table for All opreations list
DATA : IT_TEMP LIKE IT_ORDER OCCURS 0 WITH HEADER LINE.

*** Header info structure
DATA : WA_ZSPM_WOKO LIKE ZSPM_WOKO.

DATA: WA_INDEX LIKE SY-TABIX.

*** Range for Maintenance processing stage
RANGES: R_IPHAS  FOR VIAUFKS-IPHAS.

** for read long text

DATA: IT_LONGTEXT LIKE ZLONGTEXT OCCURS 0 WITH HEADER LINE.

*data: begin of it_longtext occurs 0,
*      aufnr like afko-aufnr,
*      vornr like afvc-vornr,
*      LTXA1 like zspm_wopo-LTXA1,
*      longtext like tline-tdline,
*      end of it_longtext.

DATA: BEGIN OF WA_AFKO OCCURS 0,
        AUFNR LIKE AFKO-AUFNR,
        VORNR LIKE AFVC-VORNR,
        AUFPL LIKE AFKO-AUFPL,
        APLZL LIKE AFVC-APLZL,
      END OF WA_AFKO.

DATA: IT_AFKO LIKE WA_AFKO OCCURS 0 WITH HEADER LINE.

DATA: IT_TLINE LIKE TLINE OCCURS 0 WITH HEADER LINE.

** For ALV
TYPE-POOLS: SLIS.

DATA : GV_REPID LIKE SY-REPID.
DATA : GV_STATUS       TYPE SLIS_FORMNAME VALUE 'PF_STATUS'.
DATA : GV_USER_COMMAND TYPE SLIS_FORMNAME VALUE 'USER_COMMAND'.
DATA : GV_LAYOUT       TYPE SLIS_LAYOUT_ALV.
DATA : IT_SORT         TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE .
DATA : GV_COL_POS TYPE I.
DATA : GV_COL_POS_1 TYPE I.

DATA : IT_FIELDCAT          TYPE SLIS_T_FIELDCAT_ALV,
       WA_FIELDCAT          LIKE LINE OF IT_FIELDCAT,
       IT_EVENTCAT          TYPE SLIS_T_EVENT,
       WA_EVENTCAT          LIKE LINE OF IT_EVENTCAT.

DATA: IT_T_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
 WA_T_FIELDCAT          LIKE LINE OF IT_T_FIELDCAT.


DATA : IT_EVENTS            TYPE SLIS_T_EVENT,
       IT_EVENT_EXIT      TYPE SLIS_T_EVENT_EXIT.
DATA : IT_SYMBOL TYPE ICON.
DATA : WA_INTERVAL TYPE I.

**** Confirmation counter
DATA: WA_RMZHL LIKE AFRU-RMZHL,
      WA_STOKZ LIKE AFRU-STOKZ.

*-- PF-Status : Excluding Function Code table
TYPES: BEGIN OF TY_FCODE,
        FCODE LIKE RSMPE-FUNC,
      END OF TY_FCODE.

DATA: IT_EX_FUNC TYPE STANDARD TABLE OF TY_FCODE WITH
                       NON-UNIQUE DEFAULT KEY INITIAL SIZE 5,
      WA_EX_FUNC TYPE TY_FCODE.

DATA : IT_TOOLBAR_EXCLUDING TYPE UI_FUNCTIONS WITH HEADER LINE.
DATA: WA_PARNR LIKE IHPA-PARNR.

*//Constants ;(C_) ==> True:'X' or '1' False:Space or '0'
CONSTANTS : C_MARK   VALUE 'X'.
*-- Screen Control Mode
CONSTANTS : C_CREATE(7)  TYPE C VALUE 'CREATE',
            C_CHANGE(7)  TYPE C VALUE 'CHANGE',
            C_DISPLAY(7) TYPE C VALUE 'DISPLAY'.
*-- Process Status
CONSTANTS : C_UPLOADED(8)  TYPE C VALUE 'UPLOADED',
            C_SAVED(8)     TYPE C VALUE 'SAVED'.

**//-- Global : used Variable just in this Program
*-- Function Control
DATA : OK_CODE LIKE SY-UCOMM.
DATA : WA_MODE(7) TYPE C,
       WA_STATUS(8) TYPE C.

DATA :  WA_RENEWAL_FLG.
*--
DATA : WA_RETURN     LIKE	BAPIRETURN1.   "Return Values

*-- User Confirm for pop-up Message
DATA : WA_ANSWER TYPE C.
DATA : WA_REPID LIKE SY-REPID,
       WA_DYNNR LIKE SY-DYNNR.

*-- Table Control Field Variables
DATA : WA_FLDTXT    LIKE FELD-NAME,  "Field Name Variable
       WA_CUR_LINE  LIKE FELD-LINE.  "Field Line Variable


*//Data(Work Area or (Internal) Structures);(WA_ )(ST_)?
*-- Global Structure of Radio Button for Screen Control according
*-- Selection of ISIR/Regular
DATA : BEGIN OF ST_DIST,
         REGU   TYPE C  VALUE C_MARK,
         ISIR   TYPE C,
       END OF ST_DIST.

DATA : WA_FILENAME   LIKE RLGRAP-FILENAME .

DATA : WA_SELECTED_INDEX LIKE SY-TABIX. "/Index for selected item

DATA : WA_CHG_INDEX LIKE SY-TABIX. "/Index for changed data

*-- BDC Mode  control
DATA : WA_BDC_MODE TYPE TB_BDCMODE VALUE 'N'.
DATA: TEXT05(5).
*//Ranges; (R_)
*RANGES :  "/

*********** SELECTION-SCREEN ***********************************
****************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETER : DY_OFN LIKE RIHEA-DY_OFN.
SELECTION-SCREEN COMMENT 3(29) TEXT-002 FOR FIELD DY_OFN.
SELECTION-SCREEN POSITION 40.
****Outstanding (Notification or Order)
PARAMETER : DY_MAB LIKE RIHEA-DY_MAB.
SELECTION-SCREEN COMMENT 50(10) TEXT-003 FOR FIELD DY_MAB.
****Completed (notifications or orders)
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK BLOCK1.



SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME TITLE TEXT-004.

*** 12/10/2003
*PARAMETER : P_BEBER LIKE VIAUFKST-BEBER OBLIGATORY, "//Plant section

*PARAMETER : P_INGRP LIKE VIQMEL-INGRP OBLIGATORY, "//Planner Group
*            P_TPLNR LIKE DIAUFK-TPLNR OBLIGATORY.
**                                            "//Functional location

SELECT-OPTIONS : S_INGRP FOR VIQMEL-INGRP NO-EXTENSION NO INTERVALS
                                          OBLIGATORY ,
                 S_TPLNR FOR DIAUFK-TPLNR NO-EXTENSION NO INTERVALS.

SELECT-OPTIONS : S_AUART  FOR DIAUFK-AUART DEFAULT 'PM01',"//Order type
                 S_DATUV  FOR RIHEA-TERMAB.   "//Period

SELECT-OPTIONS : S_STEUS FOR  VIAFVC-STEUS.   "//Control key

SELECTION-SCREEN END OF BLOCK BLOCK2.

****addition by 100565
SELECTION-SCREEN BEGIN OF BLOCK BLOCK3 WITH FRAME TITLE TEXT-010.

SELECT-OPTIONS:

                S_BNAME FOR USR02-BNAME MATCHCODE OBJECT USER_COMP.
*                  s_text for text05.

SELECTION-SCREEN END OF BLOCK BLOCK3.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK4 WITH FRAME TITLE TEXT-015.

SELECT-OPTIONS:

                S_BNAME1 FOR USR02-BNAME MATCHCODE OBJECT USER_COMP.
*                  s_text1 for text05.



SELECTION-SCREEN END OF BLOCK BLOCK4.




SELECTION-SCREEN BEGIN OF BLOCK BLOCK5 WITH FRAME TITLE TEXT-015.

*SELECT-OPTIONS:
*
*                S_BNAME1 FOR USR02-BNAME MATCHCODE OBJECT USER_COMP.

PARAMETER: TEXT04(250).

SELECTION-SCREEN END OF BLOCK BLOCK5.


* end of addition

******************* INITIALIZATION ********************************
*******************************************************************
*** default check Outstanding
*** Period current month
INITIALIZATION.
  DY_OFN = 'X'.
  CONCATENATE SY-DATUM(6) '01' INTO S_DATUV-LOW.
  S_DATUV-HIGH = SY-DATUM.
  APPEND S_DATUV.

***************** AT SELECTION-SCREEN ******************************
********************************************************************
AT SELECTION-SCREEN.

  CASE SY-UCOMM.
    WHEN 'ONLI'.
      CLEAR : SY-UCOMM.
      CLEAR : IT_TEMP_ORDER, IT_TEMP_ORDER[].
      CLEAR : IT_ORDER, IT_ORDER[].
      PERFORM READ_DATA.
**** { INSERT 2003/12/29...
      PERFORM CHECK_COUNTERMEASURE.
****  INSERT 2003/12/29... }

***Addition by 100565
* Calculate malfunction duration
      LOOP AT IT_ORDER.
        CLEAR: WA_INTERVAL.

    IF IT_ORDER-AUSVN <> '00000000' AND IT_ORDER-AUZTV  <> '000000' AND
           IT_ORDER-AUSBS <> '00000000'  AND IT_ORDER-AUZTB <> '000000'.

          CALL FUNCTION 'Z_FCA_GET_TIME_INTERVAL'
               EXPORTING
                    S_DATE   = IT_ORDER-AUSVN
                    S_TIME   = IT_ORDER-AUZTV
                    E_DATE   = IT_ORDER-AUSBS
                    E_TIME   = IT_ORDER-AUZTB
               IMPORTING
                    INTERVAL = WA_INTERVAL.
        ENDIF.
*Get Equipment Text
        SELECT SINGLE EQKTX INTO IT_ORDER-EQKTX
                 FROM  EQKT
                 WHERE EQUNR = IT_ORDER-EQUNR.

* Get Function Location Text
        SELECT SINGLE PLTXT INTO IT_ORDER-PLTXT
                 FROM  IFLOTX
                 WHERE TPLNR = IT_ORDER-TPLNR.
* calculate Line down time.
        SELECT SINGLE MAX( RMZHL ) INTO WA_RMZHL
             FROM  AFRU
             WHERE AUFNR = IT_ORDER-AUFNR
             AND   VORNR = '0010'.

        SELECT SINGLE IDAUR INTO IT_ORDER-IDAUR
               FROM  AFRU
               WHERE AUFNR = IT_ORDER-AUFNR
               AND   VORNR = '0010'
               AND   RMZHL = WA_RMZHL.
* GET EMPLOYEE NAME FROM EMPLOYEE ID

** Changed by Furong on 09/14/09
*IF IT_ORDER-QMNAM NE SPACE.
*SELECT SINGLE ENAME INTO IT_ORDER-ENAME
*         FROM  PA0001
*         WHERE PERNR = IT_ORDER-QMNAM.
*ENDIF.
** End of change.

*MOVE WA_RMZHL TO IT_ORDER-RMZHL.
        MOVE WA_INTERVAL TO IT_ORDER-INTERVAL.
        MODIFY IT_ORDER.
      ENDLOOP.
***End Addition
      CALL SCREEN 0100  .
    WHEN 'F03'.
      CLEAR: SY-UCOMM.
      LEAVE SCREEN.

    WHEN  '%012'.
      CLEAR: SY-UCOMM.
    WHEN  '%013'.
      CLEAR: SY-UCOMM.
    WHEN  '%014'.
      CLEAR: SY-UCOMM.
    WHEN  '%015'.
      CLEAR: SY-UCOMM.
    WHEN  '%016'.
      CLEAR: SY-UCOMM.
    WHEN  '%017'.
      CLEAR: SY-UCOMM.
    WHEN  '%018'.
      CLEAR: SY-UCOMM.
    WHEN  '%019'.
      CLEAR: SY-UCOMM.
    WHEN  '%020'.
      CLEAR: SY-UCOMM.

    WHEN  '%021'.
      CLEAR: SY-UCOMM.
    WHEN  '%022'.
      CLEAR: SY-UCOMM.

    WHEN OTHERS.
      CLEAR: SY-UCOMM.
      LEAVE SCREEN.
  ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA.

  DATA : WA_RUECK LIKE AFRU-RUECK,
         WA_RMZHL LIKE AFRU-RMZHL.



** {{ 12/10/2003

*** Set select Condtion for check-box
*  RANGES : R_AUERU FOR AFRU-AUERU.
*
*  CLEAR: R_AUERU, R_AUERU[].
*
*  IF DY_OFN EQ 'X' AND DY_MAB EQ 'X'.
*
*  ELSEIF DY_OFN EQ 'X' AND DY_MAB EQ ' '.
*    R_AUERU-SIGN   = 'I'.
*    R_AUERU-OPTION = 'EQ'.
*    R_AUERU-LOW    = 'X'.
*    APPEND R_AUERU.
*
*  ELSEIF DY_OFN EQ ' ' AND DY_MAB EQ 'X'.
*    R_AUERU-SIGN   = 'I'.
*    R_AUERU-OPTION = 'EQ'.
*    R_AUERU-LOW    = ' '.
*    APPEND R_AUERU.
*
*  ELSEIF DY_OFN EQ ' ' AND DY_MAB EQ ' '.
*    MESSAGE E000(ZMPM) WITH TEXT-M01.
*  ENDIF.


  IF DY_OFN EQ ' ' AND DY_MAB EQ ' '.
    MESSAGE E000(ZMPM) WITH TEXT-M01.
  ENDIF.

  PERFORM FILL_R_IPHAS.

*  selec  single max()
*          form AFRU
*          where AUFNR =


  IF DY_OFN EQ 'X' AND DY_MAB EQ ' '.
***  Selection Outstanding (not exits AFRU)
*** I0076         E        DLFL   Deletion flag
*** I0043         E        LKD    Locked
*** I0002         E        REL    Released
*** I0001         E        CRTD   Created
*** I0009         E        CNF    Confirmed


    SELECT DISTINCT A~AUFNR A~AUART A~TPLNR A~EQUNR A~VORNR
                    B~MSGRP C~KTEXT B~QMNUM B~AUSVN B~AUSBS B~AUZTV
                    B~AUZTB B~AUSZT B~MAUEH
                    B~QMNAM B~OBJNR                         "100565
            INTO CORRESPONDING FIELDS OF TABLE IT_TEMP_ORDER
            FROM VIAUF_AFVC AS A
                  INNER JOIN VIQMEL AS B
                  ON A~AUFNR = B~AUFNR
                      INNER JOIN CAUFV AS C
                      ON B~AUFNR = C~AUFNR
            WHERE A~TPLNR IN S_TPLNR
            AND   A~INGPR IN S_INGRP
            AND   A~AUART IN S_AUART
            AND   A~ADDAT IN S_DATUV
            AND   A~IPHAS IN R_IPHAS
            AND   A~STEUS IN S_STEUS
            AND   NOT EXISTS ( SELECT * FROM JEST
                              WHERE OBJNR = A~OBJNR
                              AND ( ( STAT  = 'I0076' AND INACT <> 'X' )
                              OR    ( STAT  = 'I0043' AND INACT <> 'X' )
                              OR  ( STAT  = 'I0009' AND INACT <> 'X' ) )
                              )
            AND   EXISTS ( SELECT * FROM JEST
                           WHERE OBJNR = A~OBJNR
                           AND   ( STAT  = 'I0002' AND INACT <> 'X' ) ).
    IF SY-SUBRC EQ 0.
      LOOP AT IT_TEMP_ORDER.
*        SELECT SINGLE RUECK MAX( RMZHL ) INTO (WA_RUECK, WA_RMZHL)
*               FROM  AFRU
*               WHERE AUFNR = IT_TEMP_ORDER-AUFNR
*               AND   VORNR = IT_TEMP_ORDER-VORNR
*               AND   RUECK = ( SELECT MAX( RUECK )
*                               FROM  AFRU
*                               WHERE AUFNR = IT_TEMP_ORDER-AUFNR
*                               AND   VORNR = IT_TEMP_ORDER-VORNR )
*               GROUP BY RUECK.

*** tunning
        SELECT SINGLE RUECK  RMZHL  INTO (WA_RUECK, WA_RMZHL)
               FROM  AFRU
               WHERE AUFNR = IT_TEMP_ORDER-AUFNR
               AND   VORNR = IT_TEMP_ORDER-VORNR
               %_HINTS ORACLE 'INDEX_DESC(AFRU, "AFRU~Z01")'.

        IF SY-SUBRC EQ 0.
          SELECT  SINGLE *
                  FROM AFRU
                  WHERE AUFNR = IT_TEMP_ORDER-AUFNR
                  AND   VORNR = IT_TEMP_ORDER-VORNR
                  AND   RUECK = WA_RUECK
                  AND   RMZHL = WA_RMZHL
                  AND   AUERU EQ 'X'
                  AND   STOKZ EQ ' '
                  AND   STZHL EQ SPACE.
          IF SY-SUBRC EQ 0.

          ELSE.
            MOVE-CORRESPONDING IT_TEMP_ORDER TO IT_ORDER.
            IF IT_TEMP_ORDER-EQUNR IS INITIAL.
              MOVE IT_TEMP_ORDER-TPLNR TO IT_ORDER-EQUNR.
            ENDIF.
            MOVE ''  TO IT_ORDER-VORNR.
            COLLECT IT_ORDER.
          ENDIF.
        ELSE.
          MOVE-CORRESPONDING IT_TEMP_ORDER TO IT_ORDER.
          IF IT_TEMP_ORDER-EQUNR IS INITIAL.
            MOVE IT_TEMP_ORDER-TPLNR TO IT_ORDER-EQUNR.
          ENDIF.
          MOVE ''  TO IT_ORDER-VORNR.
          COLLECT IT_ORDER.
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE E000(ZMPM) WITH TEXT-M02.
    ENDIF.
*    SELECT DISTINCT A~AUFNR A~AUART A~TPLNR A~EQUNR B~MSGRP C~KTEXT
*            INTO CORRESPONDING FIELDS OF TABLE IT_ORDER
*            FROM VIAUF_AFVC AS A
*                  INNER JOIN VIQMEL AS B
*                  ON A~AUFNR = B~AUFNR
*                      INNER JOIN CAUFV AS C
*                      ON B~AUFNR = C~AUFNR
*            WHERE A~TPLNR IN S_TPLNR
*            AND   A~INGPR IN S_INGRP
*            AND   A~AUART IN S_AUART
*            AND   A~ADDAT IN S_DATUV
*            AND   A~IPHAS IN R_IPHAS
*            AND   A~STEUS IN S_STEUS
*            AND   NOT EXISTS ( SELECT * FROM AFRU
*                               WHERE AUFNR = A~AUFNR
*                               AND   AUERU EQ 'X'
*                               AND   STOKZ EQ ' ' ).

  ELSEIF DY_OFN EQ ' ' AND DY_MAB EQ 'X'.
***  Selection Complete
    SELECT DISTINCT A~AUFNR A~AUART A~TPLNR A~EQUNR A~VORNR
                    B~MSGRP C~KTEXT B~QMNUM A~IEDD B~QMNAM
                    B~AUSVN B~AUSBS B~AUZTV                 "100565
                    B~AUZTB B~AUSZT B~MAUEH                 "100565
                    B~QMNAM B~OBJNR                         "100565
            INTO CORRESPONDING FIELDS OF TABLE IT_TEMP_ORDER
            FROM VIAUF_AFVC AS A
                  INNER JOIN VIQMEL AS B
                  ON A~AUFNR = B~AUFNR
                      INNER JOIN CAUFV AS C
                      ON B~AUFNR = C~AUFNR
            WHERE A~TPLNR IN S_TPLNR
            AND   A~INGPR IN S_INGRP
            AND   A~AUART IN S_AUART
            AND   A~ADDAT IN S_DATUV
            AND   A~IPHAS IN R_IPHAS
            AND   A~STEUS IN S_STEUS
            AND   NOT EXISTS ( SELECT * FROM JEST
                       WHERE OBJNR = A~OBJNR
                       AND   ( ( STAT  = 'I0076' AND INACT <> 'X' )
                       OR   ( STAT  = 'I0043' AND INACT <> 'X' ) ) )
            AND   EXISTS ( SELECT * FROM JEST
                       WHERE OBJNR = A~OBJNR
                       AND   ( STAT  = 'I0002' AND INACT <> 'X' ) ).

    IF SY-SUBRC EQ 0.
      LOOP AT IT_TEMP_ORDER.
*        SELECT SINGLE RUECK MAX( RMZHL ) INTO (WA_RUECK, WA_RMZHL)
*             FROM  AFRU
*             WHERE AUFNR = IT_TEMP_ORDER-AUFNR
*             AND   VORNR = IT_TEMP_ORDER-VORNR
*             AND   RUECK = ( SELECT MAX( RUECK )
*                             FROM  AFRU
*                             WHERE AUFNR = IT_TEMP_ORDER-AUFNR
*                             AND   VORNR = IT_TEMP_ORDER-VORNR )
*             GROUP BY RUECK.

        SELECT SINGLE RUECK  RMZHL  INTO (WA_RUECK, WA_RMZHL)
               FROM  AFRU
               WHERE AUFNR = IT_TEMP_ORDER-AUFNR
               AND   VORNR = IT_TEMP_ORDER-VORNR
               %_HINTS ORACLE 'INDEX_DESC(AFRU, "AFRU~Z01")'.

        IF SY-SUBRC EQ 0.
          SELECT  SINGLE *
                  FROM AFRU
                  WHERE AUFNR = IT_TEMP_ORDER-AUFNR
                  AND   VORNR = IT_TEMP_ORDER-VORNR
                  AND   RUECK = WA_RUECK
                  AND   RMZHL = WA_RMZHL
                  AND   AUERU EQ 'X'
                  AND   STOKZ EQ ' '
                  AND   STZHL EQ SPACE.
          IF SY-SUBRC EQ 0.
            MOVE-CORRESPONDING IT_TEMP_ORDER TO IT_ORDER.
            IF IT_TEMP_ORDER-EQUNR IS INITIAL.
              MOVE IT_TEMP_ORDER-TPLNR TO IT_ORDER-EQUNR.
            ENDIF.
            MOVE ''  TO IT_ORDER-VORNR.
            COLLECT IT_ORDER.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE E000(ZMPM) WITH TEXT-M02.
    ENDIF.

  ELSEIF DY_OFN EQ 'X' AND DY_MAB EQ 'X'.
****  Selection Outstanding & Complete
    SELECT DISTINCT A~AUFNR A~AUART A~TPLNR A~EQUNR A~VORNR
                    B~MSGRP C~KTEXT B~QMNUM A~IEDD B~QMNAM
                    B~AUSVN B~AUSBS B~AUZTV                 "100565
                    B~AUZTB B~AUSZT B~MAUEH                 "100565
                    B~QMNAM B~OBJNR                         "100565
            INTO CORRESPONDING FIELDS OF TABLE IT_TEMP_ORDER
            FROM VIAUF_AFVC AS A
                  INNER JOIN VIQMEL AS B
                  ON A~AUFNR = B~AUFNR
                      INNER JOIN CAUFV AS C
                      ON B~AUFNR = C~AUFNR
            WHERE A~TPLNR IN S_TPLNR
            AND   A~INGPR IN S_INGRP
            AND   A~AUART IN S_AUART
            AND   A~ADDAT IN S_DATUV
            AND   A~IPHAS IN R_IPHAS
            AND   A~STEUS IN S_STEUS
            AND   NOT EXISTS ( SELECT * FROM JEST
                      WHERE OBJNR = A~OBJNR
                      AND   ( ( STAT  = 'I0076' AND INACT <> 'X' )
                      OR  ( STAT  = 'I0043' AND INACT <> 'X' ) ) )
            AND   EXISTS ( SELECT * FROM JEST
                      WHERE OBJNR = A~OBJNR
                      AND  ( STAT  = 'I0002' AND INACT <> 'X' ) ).
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZMPM) WITH TEXT-M02.
    ELSE.
      LOOP AT IT_TEMP_ORDER.
        MOVE-CORRESPONDING IT_TEMP_ORDER TO IT_ORDER.
        IF IT_TEMP_ORDER-EQUNR IS INITIAL.
          MOVE IT_TEMP_ORDER-TPLNR TO IT_ORDER-EQUNR.
        ENDIF.
        MOVE ''  TO IT_ORDER-VORNR.
        COLLECT IT_ORDER.
      ENDLOOP.
    ENDIF.
  ENDIF.
*  SELECT DISTINCT A~AUFNR B~AUART A~BEBER A~TPLNR A~EQUNR  A~MSGRP
*          B~KTEXT
*         INTO CORRESPONDING FIELDS OF TABLE IT_ORDER
*         FROM VIQMEL AS A
*              INNER JOIN CAUFV AS B
*              ON A~AUFNR = B~AUFNR
*                   INNER JOIN AFIH AS C
*                   ON B~AUFNR = C~AUFNR
*                      INNER JOIN AFRU AS D
*                      ON C~AUFNR = D~AUFNR
*                         INNER JOIN AFVC AS E
*                         ON  B~AUFPL = E~AUFPL
**          WHERE A~BEBER =  P_BEBER
*          WHERE A~TPLNR =  P_TPLNR
*          AND   A~INGRP =  P_INGRP
*          AND   B~AUART IN S_AUART
*          AND   C~ADDAT IN S_DATUV
*          AND   D~AUERU IN R_AUERU
*          AND   E~STEUS IN S_STEUS.
  IF SY-SUBRC EQ 0.

*    LOOP AT IT_ORDER.
*      WA_INDEX = SY-TABIX.
*      IF NOT IT_ORDER-EQUNR IS INITIAL.
*        SELECT SINGLE EQKTX INTO IT_ORDER-TXTCDOT
*                      FROM EQKT
*                      WHERE EQUNR = IT_ORDER-EQUNR
*                      AND   SPRAS = SY-LANGU.
*        IF SY-SUBRC EQ 0.
*          MODIFY IT_ORDER INDEX WA_INDEX TRANSPORTING TXTCDOT.
*        ENDIF.
*      ELSE.
*        SELECT SINGLE PLTXT INTO IT_ORDER-TXTCDOT
*                      FROM IFLOTX
*                      WHERE TPLNR = IT_ORDER-TPLNR
*                      AND   SPRAS = SY-LANGU.
*        IF SY-SUBRC EQ 0.
*          MODIFY IT_ORDER INDEX WA_INDEX TRANSPORTING TXTCDOT.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
  ENDIF.

  LOOP AT IT_ORDER.
    SELECT SINGLE GSTRP GLTRP GSUZP GLUZP INTO
    (IT_ORDER-GSTRP,IT_ORDER-GLTRP,IT_ORDER-GSUZP,IT_ORDER-GLUZP)
     FROM VIAUFKS
    WHERE AUFNR = IT_ORDER-AUFNR.
    MODIFY IT_ORDER.
  ENDLOOP.

  LOOP AT IT_ORDER.
    CLEAR: WA_PARNR.
    SELECT SINGLE PARNR INTO WA_PARNR FROM IHPA
    WHERE OBJNR = IT_ORDER-OBJNR
      AND  PARVW = 'VW'.

    SELECT SINGLE ENAME INTO IT_ORDER-ENAME FROM PA0001
    WHERE PERNR = WA_PARNR.

    MODIFY IT_ORDER.

  ENDLOOP.

**** read Plant section text(Shop)
*  SELECT SINGLE FING
*         INTO  ZSPM_WOKO-FING
*         FROM  T357
*         WHERE BEBER = P_BEBER.

ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET TITLEBAR '100'.
  SET PF-STATUS SPACE.

  GV_REPID = SY-REPID.

* Preparation of ALV
  PERFORM PRE_REPORT_ADJ.

* Call ALV LIST
  PERFORM CALL_ALV_LIST.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  PRE_REPORT_ADJ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRE_REPORT_ADJ.
* Building Field Cat.
  CLEAR : GV_COL_POS, IT_FIELDCAT, IT_FIELDCAT[].


  PERFORM BUILD_FIELDCAT USING
     'IT_ORDER' 'TPLNR'  'X'     SPACE SPACE
     SPACE    '12'     'Functional Location'  SPACE SPACE SPACE SPACE
SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE
SPACE  SPACE.
****ADDITION BY 100565
  PERFORM BUILD_FIELDCAT USING
      'IT_ORDER' 'EQKTX'  ' '     SPACE SPACE
      SPACE    '40'     'Equipment'  SPACE SPACE SPACE SPACE SPACE
SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE
   SPACE.

  PERFORM BUILD_FIELDCAT USING
      'IT_ORDER' 'PLTXT'  ' '     SPACE SPACE
      SPACE    '40'     'Functional Location'  SPACE SPACE SPACE SPACE
SPACE SPACE SPACE SPACE SPACE SPACE  SPACE SPACE SPACE SPACE SPACE SPACE
   SPACE  SPACE.


***END
  PERFORM BUILD_FIELDCAT USING
    'IT_ORDER' 'AUFNR'  'X'     SPACE SPACE
    SPACE    '12'     'Order Number'  SPACE SPACE SPACE SPACE SPACE
SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE
 SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_ORDER' 'AUART'  ' '     SPACE SPACE
     SPACE    '4'     'Order Type'  SPACE SPACE SPACE SPACE SPACE SPACE
SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE
SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_ORDER' 'EQUNR'  ' '     SPACE SPACE
     SPACE    '18'     'Object'  SPACE SPACE SPACE SPACE SPACE
SPACE SPACE SPACE SPACE SPACE SPACE
SPACE SPACE  SPACE SPACE SPACE SPACE SPACE.

*  PERFORM BUILD_FIELDCAT USING
*    'IT_ORDER' 'TXTCDOT'  ' '     SPACE SPACE
*     SPACE    '40'     'Object Name'  SPACE SPACE SPACE space.

  PERFORM BUILD_FIELDCAT USING
    'IT_ORDER' 'MSGRP'  ' '     SPACE SPACE
     SPACE    '8'     'Process'  SPACE SPACE SPACE SPACE SPACE SPACE
SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE
SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_ORDER' 'KTEXT'  ' '     SPACE SPACE
     SPACE    '40'     'Order description'  SPACE SPACE SPACE SPACE
SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE
SPACE SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_ORDER' 'ZCOUNTER'  ' '     SPACE SPACE
     SPACE    ' '     'Countermeasure'  SPACE SPACE SPACE 'X' SPACE
SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE
SPACE SPACE.



*  PERFORM BUILD_FIELDCAT USING
*    'IT_ORDER' 'VORNR'  ' '     SPACE SPACE
*     SPACE    '4'     'OpAc'  SPACE SPACE SPACE.
*
*  PERFORM BUILD_FIELDCAT USING
*    'IT_ORDER' 'LTXA1'  ' '     SPACE SPACE
*    SPACE    '40'     'OP Desc'  SPACE SPACE SPACE.
********addition by 100565***
  PERFORM BUILD_FIELDCAT USING
    'IT_ORDER' 'AUSVN'  ' '     SPACE SPACE
     SPACE    '8'     'Mal Start Date'  SPACE SPACE SPACE SPACE
SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE
SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_ORDER' 'AUSBS'  ' '     SPACE SPACE
    SPACE    '8'     'Mal End Date'  SPACE SPACE SPACE SPACE SPACE
 SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE
SPACE.

  PERFORM BUILD_FIELDCAT USING
  'IT_ORDER' 'AUZTV'  ' '     SPACE SPACE
   SPACE    '8'     'Mal Start Time'  SPACE SPACE SPACE SPACE
SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE
SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_ORDER' 'AUZTB'  ' '     SPACE SPACE
    SPACE    '8'     'Mal End Time'  SPACE SPACE SPACE SPACE SPACE
 SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE
SPACE.

  PERFORM BUILD_FIELDCAT USING
      'IT_ORDER' 'INTERVAL'  ' '     SPACE SPACE
      SPACE    '8'     'Mal Interval'  SPACE SPACE SPACE SPACE SPACE
SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE
  SPACE.

  PERFORM BUILD_FIELDCAT USING
     'IT_ORDER' 'IDAUR'  ' '     SPACE SPACE
     SPACE    '8'     'Line Down Time'  SPACE SPACE SPACE SPACE
SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE
 SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
     'IT_ORDER' 'ENAME'  ' '     SPACE SPACE
     SPACE    '40'     'Responsible Person'  SPACE SPACE SPACE SPACE
SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE
 SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
      'IT_ORDER' 'GSTRP'  ' '     SPACE SPACE
      SPACE    '8'     'Start Date'  SPACE SPACE SPACE SPACE
SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE
  SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
      'IT_ORDER' 'GLTRP'  ' '     SPACE SPACE
      SPACE    '8'     'End Date'  SPACE SPACE SPACE SPACE SPACE
  SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE
  SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
      'IT_ORDER' 'GSUZP'  ' '     SPACE SPACE
      SPACE    '8'     'Start time'  SPACE SPACE SPACE SPACE SPACE
  SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE
  SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
      'IT_ORDER' 'GLUZP'  ' '     SPACE SPACE
      SPACE    '20'     'End Time'  SPACE SPACE SPACE SPACE SPACE
  SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE
  SPACE
  SPACE.




****end addition by 100565***

*** Sort
  SORT IT_ORDER BY AUFNR.
  CLEAR: IT_ORDER .

  IT_SORT-FIELDNAME = 'TPLNR'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.

  IT_SORT-FIELDNAME = 'AUFNR'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.

  IT_SORT-FIELDNAME = 'AUART'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.

  IT_SORT-FIELDNAME = 'EQUNR'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.

*  IT_SORT-FIELDNAME = 'TXTCDOT'.
*  IT_SORT-UP        = 'X'.
*  IT_SORT-EXPA      = 'X'.
*  IT_SORT-SUBTOT    = 'X'.
*  APPEND IT_SORT.


  IT_SORT-FIELDNAME = 'MSGRP'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.

  IT_SORT-FIELDNAME = 'KTEXT'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.


*  IT_SORT-FIELDNAME = 'VORNR'.
*  IT_SORT-UP        = 'X'.
*  IT_SORT-EXPA      = 'X'.
*  IT_SORT-SUBTOT    = 'X'.
*  APPEND IT_SORT.
*
*  IT_SORT-FIELDNAME = 'LTXA1'.
*  IT_SORT-UP        = 'X'.
*  IT_SORT-EXPA      = 'X'.
*  IT_SORT-SUBTOT    = 'X'.
*  APPEND IT_SORT.

  IT_SORT-FIELDNAME = 'AUSVN'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.

  IT_SORT-FIELDNAME = 'AUSBS'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.

  IT_SORT-FIELDNAME = 'AUZTV'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.

  IT_SORT-FIELDNAME = 'AUZTB'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.

  IT_SORT-FIELDNAME = 'INTERVAL'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.

  IT_SORT-FIELDNAME = 'IDAUR'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.

  IT_SORT-FIELDNAME = 'EQKTX'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.

  IT_SORT-FIELDNAME = 'PLTXT'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.

*** Set Event
  DATA : WA_L_EVENT  TYPE SLIS_ALV_EVENT.
  WA_L_EVENT-NAME = SLIS_EV_TOP_OF_PAGE.
  WA_L_EVENT-FORM = 'BASIC_TOP_OF_PAGE'.
  APPEND WA_L_EVENT TO IT_EVENTS.

****
****modif by 100565
  DATA : WA_T_EVENT  TYPE SLIS_ALV_EVENT.
  WA_T_EVENT-NAME = SLIS_EV_END_OF_PAGE.
  WA_T_EVENT-FORM = 'BASIC_END_OF_PAGE'.
  APPEND WA_T_EVENT TO IT_EVENTS.

***end modif
  GV_LAYOUT-BOX_FIELDNAME = 'CHECK'.
*  GV_LAYOUT-LIGHTS_FIELDNAME = 'ZCOUNTER'.

ENDFORM.                    " PRE_REPORT_ADJ
*&---------------------------------------------------------------------*
*&      Form  CALL_ALV_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_ALV_LIST.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM       = GV_REPID
            I_CALLBACK_PF_STATUS_SET = GV_STATUS
            I_CALLBACK_USER_COMMAND  = GV_USER_COMMAND
            IT_FIELDCAT              = IT_FIELDCAT[]
            IT_SORT                  = IT_SORT[]
            I_SAVE                   = 'A'
            IT_EVENTS                = IT_EVENTS
            IT_EVENT_EXIT            = IT_EVENT_EXIT  "
            IS_LAYOUT                = GV_LAYOUT
*            I_SCREEN_START_COLUMN    = 5
*            I_SCREEN_START_LINE      = 10
*            I_SCREEN_END_COLUMN      = 25
*            I_SCREEN_END_LINE        = 20
            I_HTML_HEIGHT_TOP        = 10
            I_HTML_HEIGHT_END        = 15
       TABLES
            T_OUTTAB                 = IT_ORDER
       EXCEPTIONS
            PROGRAM_ERROR            = 1
            OTHERS                   = 2.

*
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " CALL_ALV_LIST
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1197   text
*      -->P_1198   text
*      -->P_1199   text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_1203   text
*      -->P_1204   text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM BUILD_FIELDCAT USING    VALUE(P_0100)
                             VALUE(P_0101)
                             VALUE(P_0102)
                             VALUE(P_0103)
                             VALUE(P_0104)
                             VALUE(P_0105)
                             VALUE(P_0106)
                             VALUE(P_0107)
                             VALUE(P_0108)
                             VALUE(P_0109)
                             VALUE(P_0110)
*                            VALUE(P_0120).
                            VALUE(P_0120)
                            VALUE(P_0121)
                            VALUE(P_0122)
                            VALUE(P_0123)
                            VALUE(P_0124)
                            VALUE(P_0125)
                            VALUE(P_0126)
                            VALUE(P_0127)
                            VALUE(P_0128)
                            VALUE(P_0129)
                            VALUE(P_0130)
                            VALUE(P_0131)
                            VALUE(P_0132)
                            VALUE(P_0133)
                            VALUE(P_0134).



  ADD 1 TO GV_COL_POS.
  WA_FIELDCAT-TABNAME     = P_0100.
  WA_FIELDCAT-FIELDNAME   = P_0101.
  WA_FIELDCAT-KEY         = P_0102.
  WA_FIELDCAT-DO_SUM      = P_0103.
  WA_FIELDCAT-CFIELDNAME  = P_0104.
  WA_FIELDCAT-CTABNAME    = P_0105.
  WA_FIELDCAT-OUTPUTLEN   = P_0106.
  WA_FIELDCAT-SELTEXT_L   = P_0107.
  WA_FIELDCAT-DATATYPE    = P_0108.
  WA_FIELDCAT-QFIELDNAME  = P_0109.
  WA_FIELDCAT-QTABNAME    = P_0110.
  WA_FIELDCAT-ICON        = P_0120.
  WA_FIELDCAT-COL_POS     = GV_COL_POS.


  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

ENDFORM.                    " BUILD_FIELDCAT
*---------------------------------------------------------------------*
*       FORM BASIC_TOP_OF_PAGE                                        *
*---------------------------------------------------------------------*
*       Display Title                                                 *
*---------------------------------------------------------------------*
FORM BASIC_TOP_OF_PAGE.
  DATA: IT_COMMENTARY TYPE SLIS_T_LISTHEADER WITH HEADER LINE.
  DATA: BEGIN OF IT_NAME OCCURS 0,
  NAMETEXT(60),
  TIME(5),
  END OF IT_NAME.


  IT_COMMENTARY-TYP = 'H'.
  IT_COMMENTARY-KEY = 'Absentees'.
  IT_COMMENTARY-INFO = 'Weekend / Holiday Work Schedule'.
  APPEND IT_COMMENTARY.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            IT_LIST_COMMENTARY = IT_COMMENTARY[].


***addition by 100565
  DATA: IT_COMMENTARY1 TYPE SLIS_T_LISTHEADER WITH HEADER LINE.


  IT_COMMENTARY1-TYP = 'A'.
  IT_COMMENTARY1-INFO = 'List of Engineers'.
  APPEND IT_COMMENTARY1.
  CLEAR IT_COMMENTARY1-INFO.

  IT_COMMENTARY1-TYP = 'A'.
  IT_COMMENTARY1-INFO = '  '.
  APPEND IT_COMMENTARY1.
  CLEAR IT_COMMENTARY1-INFO.

  DATA: I(2).
  CLEAR I.
  I = 1.
  IF S_BNAME NE SPACE.
    SELECT DISTINCT NAME_TEXT FROM V_USERNAME INTO IT_COMMENTARY1-INFO
    WHERE
    BNAME IN S_BNAME.
      IT_COMMENTARY1-TYP = 'A'.
*read table s_text  INDEX i.
*concatenate  it_commentary1-info '     ' s_text-low '  ' 'Hrs' into
*it_commentary1-info
*separated by space.
      APPEND IT_COMMENTARY1.
      CLEAR IT_COMMENTARY1-INFO.
*i = i + 1.
    ENDSELECT.
  ENDIF.

  IF S_BNAME1 NE SPACE.
    IT_COMMENTARY1-TYP = 'A'.
    IT_COMMENTARY1-INFO = '  '.
    APPEND IT_COMMENTARY1.
    CLEAR IT_COMMENTARY1-INFO.

    IT_COMMENTARY1-TYP = 'A'.
    IT_COMMENTARY1-INFO = 'List of Team Members'.
    APPEND IT_COMMENTARY1.
    CLEAR IT_COMMENTARY1-INFO.

    IT_COMMENTARY1-TYP = 'A'.
    IT_COMMENTARY1-INFO = '  '.
    APPEND IT_COMMENTARY1.
    CLEAR IT_COMMENTARY1-INFO.
  ENDIF.

  DATA: J(2).
  CLEAR J.
  J = 1.

  IF S_BNAME1 NE SPACE.
    SELECT DISTINCT NAME_TEXT FROM V_USERNAME INTO IT_COMMENTARY1-INFO
    WHERE
    BNAME IN S_BNAME1.
      IT_COMMENTARY1-TYP = 'A'.

*read table s_text  INDEX j.
*concatenate  it_commentary1-info '     ' s_text-low '  ' 'Hrs' into
*it_commentary1-info
*separated by space.

      APPEND IT_COMMENTARY1.
      CLEAR IT_COMMENTARY1-INFO.
*  j = j + 1.
    ENDSELECT.
  ENDIF.

  IF TEXT04 <> SPACE.
    IT_COMMENTARY1-TYP = 'A'.
    IT_COMMENTARY1-INFO = '  '.
    APPEND IT_COMMENTARY1.
    CLEAR IT_COMMENTARY1-INFO.

    IT_COMMENTARY1-TYP = 'A'.
    IT_COMMENTARY1-INFO = 'Information'.
    APPEND IT_COMMENTARY1.
    CLEAR IT_COMMENTARY1-INFO.

    IT_COMMENTARY1-TYP = 'A'.
    IT_COMMENTARY1-INFO = '  '.
    APPEND IT_COMMENTARY1.
    CLEAR IT_COMMENTARY1-INFO.

    IT_COMMENTARY1-INFO = TEXT04.
    APPEND IT_COMMENTARY1.
  ENDIF.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            IT_LIST_COMMENTARY = IT_COMMENTARY1[].


*******For Popup window******
*data: k(2), l(2).
*clear: k, l.
*k = 1.
*l = 1.
*
*IF S_BNAME NE SPACE.
*SELECT DISTINCT NAME_TEXT FROM V_USERNAME INTO IT_NAME-NAMETEXT
*WHERE
*BNAME IN S_BNAME.
**read table s_text  INDEX k.
**IT_NAME-TIME = s_text-low.
*  APPEND IT_NAME.
*  CLEAR IT_NAME-NAMETEXT.
**  k = k + 1.
*   ENDSELECT.
*ENDIF.
*
*IF S_BNAME1 NE SPACE.
*SELECT DISTINCT NAME_TEXT FROM V_USERNAME INTO IT_NAME-NAMETEXT
*WHERE
*BNAME IN S_BNAME1.
**read table s_text1  INDEX l.
**IT_NAME-TIME = s_text1-low.
*  APPEND IT_NAME.
*  CLEAR IT_NAME-NAMETEXT.
**  l = l + 1.
* ENDSELECT.
*ENDIF.
*
*
*PERFORM BUILD_FIELDCAT_T USING
*     'IT_NAME' 'NAMETEXT'  'X'     SPACE SPACE
*     SPACE    '20'     'NAME'  SPACE SPACE SPACE SPACE
*SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE.
*PERFORM BUILD_FIELDCAT_T USING
*     'IT_NAME' 'TIME'  'X'     SPACE SPACE
*     SPACE    '5'     'TIME'  SPACE SPACE SPACE SPACE
*SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE.
*
*
*
*CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
* EXPORTING
**   I_INTERFACE_CHECK              = ' '
**   I_BYPASSING_BUFFER             =
**   I_BUFFER_ACTIVE                = ' '
*   I_CALLBACK_PROGRAM             = GV_REPID
**   I_CALLBACK_PF_STATUS_SET       = ' '
**   I_CALLBACK_USER_COMMAND        = ' '
**   I_STRUCTURE_NAME               = IT_NAME
**   IS_LAYOUT                      = GV_LAYOUT
*   IT_FIELDCAT                    = IT_T_FIELDCAT
**   IT_EXCLUDING                   =
**   IT_SPECIAL_GROUPS              =
**   IT_SORT                        =
**   IT_FILTER                      =
**   IS_SEL_HIDE                    =
**   I_DEFAULT                      = 'X'
**   I_SAVE                         = ' '
**   IS_VARIANT                     =
**   IT_EVENTS                      =
**   IT_EVENT_EXIT                  =
**   IS_PRINT                       =
**   IS_REPREP_ID                   =
*   I_SCREEN_START_COLUMN          = 1
*   I_SCREEN_START_LINE            = 1
*   I_SCREEN_END_COLUMN            = 50
*   I_SCREEN_END_LINE              = 6
** IMPORTING
**   E_EXIT_CAUSED_BY_CALLER        =
**   ES_EXIT_CAUSED_BY_USER         =
*  TABLES
*    T_OUTTAB                       = IT_NAME
** EXCEPTIONS
**   PROGRAM_ERROR                  = 1
**   OTHERS                         = 2
*          .
*IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*ENDIF.
*

ENDFORM.
*-----------------------------------------------------------------------
*    FORM PF_STATUS_VAR
*-----------------------------------------------------------------------
FORM PF_STATUS USING  EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'BALVLIST'  EXCLUDING EXTAB. " OF PROGRAM 'ZAPM08_ANBD'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       User Command for ALV List
*----------------------------------------------------------------------*
FORM USER_COMMAND  USING R_UCOMM
                         RS_SELFIELD TYPE SLIS_SELFIELD.

  CASE R_UCOMM.
    WHEN 'IC1'.
***  Work Order List (Smartform)
      CLEAR: R_UCOMM.
      PERFORM CHECK_ORDER.
      PERFORM CALL_WORK_ORDER_LIST.
***  Call SmartForm 'ZSPM01_WORK_ORDER2'
***  PM Work Order Form

** Insert by furong
      PERFORM GET_LONGTEXT.
** end of insert

      PERFORM PRINT_FORM.
** Added by Fuorng on 03/14/11
      PERFORM PRINT_ATTACHMENT.
** End of addition

    WHEN 'COUNT'.
***  Countermeasure report (Excel)
      CLEAR: R_UCOMM.
      PERFORM CALL_COUNTERMEASURE_REPORT.

*    WHEN 'TIME'.
****  Countermeasure report (Excel)
*      CLEAR: R_UCOMM.
*      CALL SCREEN '500'.
*

    WHEN 'F03' OR  'F15' OR 'F12'.
      CLEAR: R_UCOMM.
      CALL SCREEN '1000'.
    WHEN '&IC1'.
***  Call Transaction 'IW33' : Display PM Order
      CLEAR: R_UCOMM.
      READ TABLE IT_ORDER INDEX RS_SELFIELD-TABINDEX.
      SET PARAMETER ID 'ANR' FIELD IT_ORDER-AUFNR.
      CALL TRANSACTION 'IW33' AND SKIP FIRST SCREEN.

  ENDCASE.
ENDFORM.                 " USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  CALL_WORK_ORDER_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_WORK_ORDER_LIST.
  DATA: WA_RUECK LIKE AFRU-RUECK,
        WA_RMZHL LIKE AFRU-RMZHL,
        WA_FIRST_ITEM.

  CLEAR: IT_ZSPM_WOPO, IT_ZSPM_WOPO[].

  LOOP AT IT_ORDER WHERE CHECK = 'X'.

    PERFORM READ_OPERATION.

    MOVE-CORRESPONDING IT_ORDER TO IT_TEMP.

    IF  IT_ORDER-EQUNR IS INITIAL .
      MOVE IT_ORDER-TPLNR TO IT_TEMP-EQUNR.
    ENDIF.

    MODIFY IT_TEMP  TRANSPORTING  AUART
                                  EQUNR
                                  MSGRP
                                  KTEXT
                                  QMNAM
                                  IEDD
*                                  MNCOD
                                 ENAME                      "100565
                     WHERE AUFNR NE ' '.

    CLEAR: WA_FIRST_ITEM.

    LOOP AT IT_TEMP .
      IF DY_OFN EQ 'X' AND DY_MAB EQ ' '.
*        SELECT SINGLE RUECK MAX( RMZHL ) INTO (WA_RUECK, WA_RMZHL)
*             FROM  AFRU
*             WHERE AUFNR = IT_TEMP-AUFNR
*             AND   VORNR = IT_TEMP-VORNR
*             AND   RUECK = ( SELECT MAX( RUECK )
*                             FROM  AFRU
*                             WHERE AUFNR = IT_TEMP-AUFNR
*                             AND   VORNR = IT_TEMP-VORNR )
*             GROUP BY RUECK.
        SELECT SINGLE RUECK  RMZHL  INTO (WA_RUECK, WA_RMZHL)
               FROM  AFRU
               WHERE AUFNR = IT_TEMP-AUFNR
               AND   VORNR = IT_TEMP-VORNR
               %_HINTS ORACLE 'INDEX_DESC(AFRU, "AFRU~Z01")'.

        IF SY-SUBRC EQ 0.
          SELECT  SINGLE *
                  FROM AFRU
                  WHERE AUFNR = IT_TEMP-AUFNR
                  AND   VORNR = IT_TEMP-VORNR
                  AND   RUECK = WA_RUECK
                  AND   RMZHL = WA_RMZHL
                  AND   AUERU EQ 'X'
                  AND   STOKZ EQ ' '
                  AND   STZHL EQ SPACE.
          IF SY-SUBRC EQ 0.
          ELSE.
            IF WA_FIRST_ITEM EQ ' '.
              WA_FIRST_ITEM = 'X'.
            ELSE.
              CLEAR: IT_TEMP-AUFNR, IT_TEMP-AUART,
                     IT_TEMP-EQUNR, IT_TEMP-MSGRP, IT_TEMP-KTEXT.
            ENDIF.
            MOVE-CORRESPONDING IT_TEMP TO IT_ZSPM_WOPO.
            APPEND IT_ZSPM_WOPO.
          ENDIF.
        ELSE.
          IF WA_FIRST_ITEM EQ ' '.
            WA_FIRST_ITEM = 'X'.
          ELSE.
            CLEAR: IT_TEMP-AUFNR, IT_TEMP-AUART,
                   IT_TEMP-EQUNR, IT_TEMP-MSGRP, IT_TEMP-KTEXT.
          ENDIF.
          MOVE-CORRESPONDING IT_TEMP TO IT_ZSPM_WOPO.
          APPEND IT_ZSPM_WOPO.
        ENDIF.
      ELSEIF DY_OFN EQ ' ' AND DY_MAB EQ 'X'.
*        SELECT SINGLE RUECK MAX( RMZHL ) INTO (WA_RUECK, WA_RMZHL)
*             FROM  AFRU
*             WHERE AUFNR = IT_TEMP-AUFNR
*             AND   VORNR = IT_TEMP-VORNR
*             AND   RUECK = ( SELECT MAX( RUECK )
*                             FROM  AFRU
*                             WHERE AUFNR = IT_TEMP-AUFNR
*                             AND   VORNR = IT_TEMP-VORNR )
*             GROUP BY RUECK.
        SELECT SINGLE RUECK  RMZHL  INTO (WA_RUECK, WA_RMZHL)
               FROM  AFRU
               WHERE AUFNR = IT_TEMP-AUFNR
               AND   VORNR = IT_TEMP-VORNR
               %_HINTS ORACLE 'INDEX_DESC(AFRU, "AFRU~Z01")'.
        IF SY-SUBRC EQ 0.
          SELECT  SINGLE *
                  FROM AFRU
                  WHERE AUFNR = IT_TEMP-AUFNR
                  AND   VORNR = IT_TEMP-VORNR
                  AND   RUECK = WA_RUECK
                  AND   RMZHL = WA_RMZHL
                  AND   AUERU EQ 'X'
                  AND   STOKZ EQ ' '
                  AND   STZHL EQ SPACE.
          IF SY-SUBRC EQ 0.
            IF WA_FIRST_ITEM EQ ' '.
              WA_FIRST_ITEM = 'X'.
            ELSE.
              CLEAR: IT_TEMP-AUFNR, IT_TEMP-AUART,
                     IT_TEMP-EQUNR, IT_TEMP-MSGRP, IT_TEMP-KTEXT.
            ENDIF.
            MOVE-CORRESPONDING IT_TEMP TO IT_ZSPM_WOPO.

            CASE IT_TEMP-MNCOD.
              WHEN '01'.
                MOVE 'v' TO IT_ZSPM_WOPO-BAD.
              WHEN '02'.
                MOVE 'v' TO IT_ZSPM_WOPO-COMP.
              WHEN '03'.
                MOVE 'v' TO IT_ZSPM_WOPO-GOOD.
              WHEN OTHERS.
            ENDCASE.

            APPEND IT_ZSPM_WOPO.
          ENDIF.
        ENDIF.
      ELSEIF DY_OFN EQ 'X' AND DY_MAB EQ 'X'.

        IF WA_FIRST_ITEM EQ ' '.
          WA_FIRST_ITEM = 'X'.
        ELSE.
          CLEAR: IT_TEMP-AUFNR, IT_TEMP-AUART, IT_TEMP-ENAME,
                 IT_TEMP-EQUNR, IT_TEMP-MSGRP, IT_TEMP-KTEXT.
        ENDIF.
        MOVE-CORRESPONDING IT_TEMP TO IT_ZSPM_WOPO.

        CASE IT_TEMP-MNCOD.
          WHEN '01'.
            MOVE 'v' TO IT_ZSPM_WOPO-BAD.
          WHEN '02'.
            MOVE 'v' TO IT_ZSPM_WOPO-COMP.
          WHEN '03'.
            MOVE 'v' TO IT_ZSPM_WOPO-GOOD.
          WHEN OTHERS.

        ENDCASE.
        APPEND IT_ZSPM_WOPO.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " CALL_WORK_ORDER_LIST
*&---------------------------------------------------------------------*
*&      Form  READ_OPERATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_OPERATION.
  CLEAR: IT_TEMP, IT_TEMP[].

  SELECT  A~AUFNR B~VORNR B~LTXA1 C~QMNUM
          INTO CORRESPONDING FIELDS OF TABLE IT_TEMP
          FROM CAUFV AS A
               INNER JOIN AFVC AS B
               ON  A~AUFPL = B~AUFPL
*              AND A~APLZL = B~APLZL
                   INNER JOIN VIQMEL AS C
                   ON A~AUFNR = C~AUFNR
           WHERE A~AUFNR = IT_ORDER-AUFNR.
  LOOP AT  IT_TEMP.
    SELECT SINGLE *
                  FROM QMMA
                  WHERE QMNUM = IT_TEMP-QMNUM.
    MOVE QMMA-MNCOD TO IT_TEMP-MNCOD.
    MODIFY IT_TEMP.
  ENDLOOP.

ENDFORM.                    " READ_OPERATION
*&---------------------------------------------------------------------*
*&      Form  PRINT_FORM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRINT_FORM.
  DATA : WA_FORMNAME TYPE TDSFNAME VALUE 'ZSPM01_WORK_ORDER2_LTEXT'.
  DATA : WA_FUNCNAME TYPE RS38L_FNAM .
  DATA: ZOBJNR LIKE QMEL-OBJNR,
        ZPARNR LIKE IHPA-PARNR.
  CLEAR: IT_ZSPM_WOPO.

**** read Functional location text
  CLEAR: IFLO.

  SELECT  SINGLE *
          FROM   IFLO
          WHERE  TPLNR = IT_ORDER-TPLNR
          AND    SPRAS = SY-LANGU
          AND    LVORM = SPACE.

**** Read Name of the Maintenance Planner Group
  SELECT SINGLE INNAM INTO ZSPM_WOKO-INNAM
         FROM   T024I
         WHERE  IWERK = IFLO-IWERK
         AND    INGRP = S_INGRP-LOW.

*** 12/10/2003 }}

**** Read Description of functional location
  READ TABLE IT_ZSPM_WOPO INDEX 1.
  SELECT SINGLE PLTXT INTO ZSPM_WOKO-PLTXT
         FROM IFLOTX
         WHERE TPLNR = IT_ORDER-TPLNR
         AND   SPRAS = SY-LANGU.

**** Reported by
  SELECT SINGLE B~NAME_TEXT
         INTO WA_ZSPM_WOKO-ERNAM
         FROM USR21 AS A
              INNER JOIN ADRP AS B
              ON A~PERSNUMBER = B~PERSNUMBER
         WHERE A~BNAME = SY-UNAME.


*clear: zobjnr, zparnr.
*select single objnr into zobjnr from qmel where aufnr = it_order-aufnr.
*select single parnr into zparnr from ihpa where objnr = zobjnr.
*SELECT SINGLE B~NAME_TEXT
*         INTO WA_ZSPM_WOKO-ERNAM
*         FROM USR21 AS A
*              INNER JOIN ADRP AS B
*              ON A~PERSNUMBER = B~PERSNUMBER
*         WHERE A~BNAME = zparnr.

*clear wa_zspm_woko-ernam.
*if wa_zspm_woko-ernam = space.
*wa_zspm_woko-ernam = IT_ZSPM_WOPO-ename.
*endif.

  IF DY_MAB EQ 'X'.
    READ TABLE IT_ZSPM_WOPO INDEX 1.
    MOVE : SY-DATUM        TO WA_ZSPM_WOKO-GSTRP,
        ZSPM_WOKO-FING     TO WA_ZSPM_WOKO-FING,
        ZSPM_WOKO-PLTXT    TO WA_ZSPM_WOKO-PLTXT,
        ZSPM_WOKO-INNAM    TO WA_ZSPM_WOKO-INNAM,
        IT_ZSPM_WOPO-QMNAM TO WA_ZSPM_WOKO-ERNAM,
        IT_ZSPM_WOPO-IEDD  TO WA_ZSPM_WOKO-ERDAT.

  ELSE.
    MOVE : SY-DATUM        TO WA_ZSPM_WOKO-GSTRP,
           ZSPM_WOKO-FING  TO WA_ZSPM_WOKO-FING,
           ZSPM_WOKO-PLTXT TO WA_ZSPM_WOKO-PLTXT,
           ZSPM_WOKO-INNAM TO WA_ZSPM_WOKO-INNAM,
*           ' '             TO WA_ZSPM_WOKO-ERNAM,"100565
           SY-DATUM        TO WA_ZSPM_WOKO-ERDAT.
  ENDIF.


*** SAP Smart Forms: Form call
*** Get SmartForm's function name (like : /1BCDWB/SF00000036 )
*** Because funtion name is dependent on system client...
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
     EXPORTING
          FORMNAME           = WA_FORMNAME
*         VARIANT            = ' '
*         DIRECT_CALL        = ' '
     IMPORTING
          FM_NAME            = WA_FUNCNAME
     EXCEPTIONS
          NO_FORM            = 1
          NO_FUNCTION_MODULE = 2
          OTHERS             = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CHECK SY-SUBRC = 0.

*** Call Smart Form ZSPM01_WORK_ORDER2 Call function...
  CALL FUNCTION WA_FUNCNAME
       EXPORTING
            I_ZSPM_WOKO      = WA_ZSPM_WOKO
       TABLES
            IT_ZSPM_WOPO     = IT_ZSPM_WOPO
            IT_LONGTEXT      = IT_LONGTEXT
            IT_ORDER_NUM     = IT_ORDER_NUM
       EXCEPTIONS
            FORMATTING_ERROR = 1
            INTERNAL_ERROR   = 2
            SEND_ERROR       = 3
            USER_CANCELED    = 4
            OTHERS           = 5.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " PRINT_FORM
*&---------------------------------------------------------------------*
*&      Form  CALL_COUNTERMEASURE_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_COUNTERMEASURE_REPORT.
  DATA: WA_COUNT TYPE I.

*** call transaction 'ZPMR02' Breakdwon Countermeasure report
  LOOP AT IT_ORDER WHERE CHECK = 'X'.
    WA_COUNT = WA_COUNT + 1.
    IF WA_COUNT > 1.
      MESSAGE E000(ZDPM) WITH TEXT-M03.
    ENDIF.
  ENDLOOP.

  READ TABLE IT_ORDER WITH KEY CHECK = 'X'.
  IF SY-SUBRC EQ 0.
    SET PARAMETER ID 'ANR' FIELD IT_ORDER-AUFNR.
    CALL TRANSACTION 'ZPMR02' AND SKIP FIRST SCREEN .
  ENDIF.
ENDFORM.                    " CALL_COUNTERMEASURE_REPORT
*&---------------------------------------------------------------------*
*&      Form  FILL_R_IPHAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_R_IPHAS.
  REFRESH R_IPHAS.

  R_IPHAS-SIGN   = 'I'.
  R_IPHAS-OPTION = 'EQ'.

  R_IPHAS-LOW = '2'.                   "FREI
  APPEND R_IPHAS.
  R_IPHAS-LOW = '3'.                   "technisch abgeschlossen
  APPEND R_IPHAS.
*--- Bei IW48 nur Auftr?e die r?kgemeldet werden k?nen -----------*
  IF SY-TCODE <> 'IW48'.
    R_IPHAS-LOW = '0'.                 "OFFEN
    APPEND R_IPHAS.
    R_IPHAS-LOW = '1'.                 "ZUR?KGEST
    APPEND R_IPHAS.
    R_IPHAS-LOW = '4'.                 "L?chkennzeichen
    APPEND R_IPHAS.
    R_IPHAS-LOW = '6'.                 "kaufm?nisch abgeschlossen(ab46)
    APPEND R_IPHAS.
  ENDIF.

ENDFORM.                    " FILL_R_IPHAS
*&---------------------------------------------------------------------*
*&      Form  CHECK_COUNTERMEASURE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_COUNTERMEASURE.

  LOOP AT IT_ORDER.
    MOVE : SY-TABIX TO WA_INDEX.

    SELECT SINGLE *
           FROM VIQMEL
           WHERE QWRNUM = IT_ORDER-QMNUM
           AND  NOT EXISTS ( SELECT * FROM JEST
                               WHERE OBJNR   = VIQMEL~OBJNR
                               AND   ( STAT  = 'I0076'
                               OR      STAT  = 'I0043' ) ).
    IF SY-SUBRC EQ 0.
      MOVE : '@01@' TO IT_ORDER-ZCOUNTER.
      MODIFY IT_ORDER INDEX WA_INDEX.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " CHECK_COUNTERMEASURE
*&---------------------------------------------------------------------*
*&      Form  CHECK_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_ORDER.

ENDFORM.                    " CHECK_ORDER

*** INSERT BY FURONG

FORM READ_LONGTEXT TABLES   P_TLINE STRUCTURE TLINE
                   USING    P_THEAD    STRUCTURE THEAD.

  CALL FUNCTION 'READ_TEXT'
       EXPORTING
            CLIENT                  = SY-MANDT
            ID                      = P_THEAD-TDID
            LANGUAGE                = SY-LANGU
            NAME                    = P_THEAD-TDNAME
            OBJECT                  = P_THEAD-TDOBJECT
            ARCHIVE_HANDLE          = 0
            LOCAL_CAT               = ' '
*       IMPORTING
*            HEADER                  =
       TABLES
            LINES                   = P_TLINE
       EXCEPTIONS
            ID                      = 1
            LANGUAGE                = 2
            NAME                    = 3
            NOT_FOUND               = 4
            OBJECT                  = 5
            REFERENCE_CHECK         = 6
            WRONG_ACCESS_TO_ARCHIVE = 7
            OTHERS                  = 8.

  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " READ_LONGTEXT
*&---------------------------------------------------------------------*
*&      Form  get_longtext
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_LONGTEXT.
  DATA : LW_THEAD LIKE THEAD.
  DATA: WA_AUFNR LIKE AFKO-AUFNR.
  DATA: W_NEWORDER(1).

  REFRESH IT_AFKO.
  REFRESH IT_LONGTEXT.
  REFRESH IT_ORDER_NUM.
  CLEAR IT_ORDER_NUM.
  CLEAR IT_AFKO.
  CLEAR IT_LONGTEXT.
  SELECT DISTINCT A~AUFNR B~VORNR A~AUFPL B~APLZL
                                  INTO CORRESPONDING FIELDS
                                  OF TABLE IT_AFKO FROM AFKO AS A
                                  INNER JOIN AFVC AS B
                                  ON A~AUFPL = B~AUFPL
                                  FOR ALL ENTRIES IN IT_ZSPM_WOPO
*                                  where aufnr = it_zspm_wopo-aufnr
                                  WHERE B~VORNR = IT_ZSPM_WOPO-VORNR.
  LOOP AT IT_ZSPM_WOPO.
    REFRESH IT_TLINE.
    CLEAR IT_TLINE.
    CLEAR LW_THEAD.
    IF NOT IT_ZSPM_WOPO-AUFNR IS INITIAL.
      WA_AUFNR = IT_ZSPM_WOPO-AUFNR.
      IT_ORDER_NUM-AUFNR = IT_ZSPM_WOPO-AUFNR.
      W_NEWORDER = 'Y'.
      APPEND IT_ORDER_NUM.
      CLEAR IT_ORDER_NUM.
    ELSE.
*      clear w_neworder.
      IT_ZSPM_WOPO-AUFNR = WA_AUFNR.
      MODIFY IT_ZSPM_WOPO.
    ENDIF.
    READ TABLE IT_AFKO WITH KEY AUFNR = WA_AUFNR
                       VORNR = IT_ZSPM_WOPO-VORNR.
    IF SY-SUBRC = 0.
      CONCATENATE SY-MANDT IT_AFKO-AUFPL IT_AFKO-APLZL
                  INTO LW_THEAD-TDNAME.
      MOVE : SY-MANDT TO LW_THEAD-TDSPRAS,
             'AVOT' TO LW_THEAD-TDID,
             'AUFK' TO LW_THEAD-TDOBJECT.
      PERFORM READ_LONGTEXT  TABLES  IT_TLINE
                             USING   LW_THEAD.
      IF SY-SUBRC = 0 AND NOT IT_TLINE[] IS INITIAL.
        IT_ZSPM_WOPO-LTEXT = 'Y'.
*        it_longtext-aufnr = it_zspm_wopo-aufnr.
        IT_LONGTEXT-AUFNR = WA_AUFNR.
        IT_LONGTEXT-NEWORDER = W_NEWORDER.
        IT_LONGTEXT-VORNR = IT_ZSPM_WOPO-VORNR.
        IT_LONGTEXT-LTXA1 = IT_ZSPM_WOPO-LTXA1.
        IT_LONGTEXT-NEWOP = 'X'.
        LOOP AT IT_TLINE.
          IT_LONGTEXT-LONGTEXT = IT_TLINE-TDLINE.
          APPEND IT_LONGTEXT.
          CLEAR IT_LONGTEXT-NEWOP.
          CLEAR W_NEWORDER.
        ENDLOOP.
        CLEAR IT_LONGTEXT.
        MODIFY IT_ZSPM_WOPO.
      ENDIF.
    ENDIF.
  ENDLOOP.
  DELETE ADJACENT DUPLICATES FROM IT_ORDER_NUM.
  SORT IT_ORDER_NUM BY AUFNR.
  SORT IT_ZSPM_WOPO BY AUFNR VORNR.
  LOOP AT IT_ORDER_NUM.
    IT_ORDER_NUM-LTEXT = 'Y'.
    MODIFY IT_ORDER_NUM.
    EXIT.
  ENDLOOP.
  LOOP AT IT_LONGTEXT.
    CLEAR IT_LONGTEXT-NEWORDER.
    MODIFY IT_LONGTEXT.
    EXIT.
  ENDLOOP.

ENDFORM.                    " get_longtext
*&---------------------------------------------------------------------*
*&      Form  BASIC_END_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BASIC_END_OF_PAGE.

  DATA: IT_COMMENTARY_T TYPE SLIS_T_LISTHEADER WITH HEADER LINE.

  IT_COMMENTARY_T-TYP = 'A'.

  IT_COMMENTARY_T-INFO = 'New Weekend / Holiday Work Schedule'.
  APPEND IT_COMMENTARY_T.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            IT_LIST_COMMENTARY = IT_COMMENTARY_T[].


ENDFORM.                    " BASIC_END_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT_T
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2391   text
*      -->P_2392   text
*      -->P_2393   text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_2397   text
*      -->P_2398   text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
*FORM BUILD_FIELDCAT_T USING    VALUE(P_2391)
*                               VALUE(P_2392)
*                               VALUE(P_2393)
*                               P_SPACE
*                               P_SPACE
*                               P_SPACE
*                               VALUE(P_2397)
*                               VALUE(P_2398)
*                               P_SPACE
*                               P_SPACE
*                               P_SPACE
*                               P_SPACE
*                               P_SPACE
*                               P_SPACE
*                               P_SPACE
*                               P_SPACE
*                               P_SPACE
*                               P_SPACE
*                               P_SPACE
*                               P_SPACE
*                               P_SPACE.

FORM BUILD_FIELDCAT_T USING    VALUE(P_0100)
                            VALUE(P_0101)
                            VALUE(P_0102)
                            VALUE(P_0103)
                            VALUE(P_0104)
                            VALUE(P_0105)
                            VALUE(P_0106)
                            VALUE(P_0107)
                            VALUE(P_0108)
                            VALUE(P_0109)
                            VALUE(P_0110)
*                            VALUE(P_0120).
                           VALUE(P_0120)
                           VALUE(P_0121)
                           VALUE(P_0122)
                           VALUE(P_0123)
                           VALUE(P_0124)
                           VALUE(P_0125)
                           VALUE(P_0126)
                           VALUE(P_0127)
                           VALUE(P_0128)
                           VALUE(P_0129).


  ADD 1 TO GV_COL_POS_1.
  WA_T_FIELDCAT-TABNAME     = P_0100.
  WA_T_FIELDCAT-FIELDNAME   = P_0101.
  WA_T_FIELDCAT-KEY         = P_0102.
  WA_T_FIELDCAT-DO_SUM      = P_0103.
  WA_T_FIELDCAT-CFIELDNAME  = P_0104.
  WA_T_FIELDCAT-CTABNAME    = P_0105.
  WA_T_FIELDCAT-OUTPUTLEN   = P_0106.
  WA_T_FIELDCAT-SELTEXT_L   = P_0107.
  WA_T_FIELDCAT-DATATYPE    = P_0108.
  WA_T_FIELDCAT-QFIELDNAME  = P_0109.
  WA_T_FIELDCAT-QTABNAME    = P_0110.
  WA_T_FIELDCAT-ICON        = P_0120.
  WA_T_FIELDCAT-COL_POS     = GV_COL_POS.


  APPEND WA_T_FIELDCAT TO IT_T_FIELDCAT.
  CLEAR WA_T_FIELDCAT.



ENDFORM.                    " BUILD_FIELDCAT_T
*&---------------------------------------------------------------------*
*&      Form  PRINT_ATTACHMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRINT_ATTACHMENT.

  DATA: L_HIDE(1),
        L_OBJKEY(15),
        L_PLNNR LIKE AFKO-PLNNR,
        L_PLNAL LIKE  AFKO-PLNAL.

  DATA: IT_ATTACHMENT LIKE TABLE OF PLKO WITH HEADER LINE.

  L_HIDE = 'Y'.


  LOOP AT IT_ORDER_NUM.
    CLEAR: L_PLNNR, L_PLNAL.
    REFRESH: IT_ATTACHMENT.

    SELECT SINGLE PLNNR PLNAL INTO (L_PLNNR, L_PLNAL)
     FROM AFKO
     WHERE AUFNR = IT_ORDER_NUM-AUFNR.

    SELECT * INTO TABLE IT_ATTACHMENT
     FROM PLKO
     WHERE ( PLNTY = 'E' OR PLNTY = 'A' )
       AND PLNNR = L_PLNNR
       AND PLNAl = L_PLNAL
       AND LOEKZ = ' '.
*  IFLO-ppsid.
    LOOP AT IT_ATTACHMENT.
      CONCATENATE IT_ATTACHMENT-PLNTY IT_ATTACHMENT-PLNNR
                  IT_ATTACHMENT-PLNAL INTO L_OBJKEY.

      PERFORM RETRIEVE_AND_PRINT_DOCUMENTS
       USING 'BUS1019'
*            'E9000005201'  " Probably Equipment Number
              L_OBJKEY
              L_HIDE.
*    WAIT UP TO 8 SECONDS.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " PRINT_ATTACHMENT
*&---------------------------------------------------------------------*
*&      Form  RETRIEVE_AND_PRINT_DOCUMENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_4192   text
*      -->P_L_OBJKEY  text
*      -->P_L_HIDE  text
*----------------------------------------------------------------------*
FORM RETRIEVE_AND_PRINT_DOCUMENTS USING OBJTYP
                                        OBJKEY
                                        HIDE.

  TABLES: PLKO, SOOD.
  TYPE-POOLS OLE2 .

** Furong on 01/30/12
*  DATA: DOCUMENTS TYPE STANDARD TABLE OF NEIGHBOR.
*  DATA: DOCUMENT  TYPE NEIGHBOR.
*  DATA: XOBJECT LIKE BORIDENT.
   DATA: DOCUMENTS TYPE OBL_T_LINK.
   DATA: DOCUMENT  TYPE OBL_T_LINK.
   DATA: DOCS      LIKE LINE OF DOCUMENTS.
  data: XOBJECT type SIBFLPORB.
** end on 01/30/12

  DATA: OBJECTS TYPE STANDARD TABLE OF SOOD4.
  DATA: OBJECT  TYPE SOOD4.
  DATA: WORD TYPE OLE2_OBJECT.
  DATA: EXCEL TYPE OLE2_OBJECT.
  DATA: WORKBOOKS TYPE OLE2_OBJECT.
  DATA: APP TYPE OLE2_OBJECT.
  DATA: FILENAME(200) TYPE C.

* If background, then do even try it!
*  check sy-batch = 'X'.

** Changed on 01/30/12 by Furong  for ECC

* Retrieve Documents
*  XOBJECT-OBJTYPE = OBJTYP.
*  XOBJECT-OBJKEY  = OBJKEY.

   XOBJECT-INSTID = OBJKEY.
   XOBJECT-TYPEID = OBJTYP.
   XOBJECT-CATID = 'BO'.
  CLEAR DOCUMENT. REFRESH DOCUMENTS.


*  CALL FUNCTION 'SREL_GET_NEXT_NEIGHBORS'
*       EXPORTING
*            OBJECT       = XOBJECT
*            ROLETYPE     = 'APPLOBJ'
*            RELATIONTYPE = 'ATTA'
*       TABLES
*            NEIGHBORS    = DOCUMENTS
*       EXCEPTIONS
*            INTERNAL_ERROR       = 1
*            NO_LOGSYS            = 2
*            OTHERS               = 3.

  TRY.

      DATA : l_rol TYPE oblroltype.


      l_rol = 'GOSAPPLOBJ'.

      CALL METHOD cl_binary_relation=>read_links_of_binrel
        exporting
          is_object   = XOBJECT

*          ip_logsys   = g_bor_logsys

          ip_relation = 'ATTA'

          ip_role     = l_rol

        IMPORTING

          et_links    = DOCUMENTS.

    CATCH cx_obl_parameter_error .

    CATCH cx_obl_internal_error .

    CATCH cx_obl_model_error .

  ENDTRY.
** End on 01/30/12

* If no documents, then exit.
  IF DOCUMENTS[] IS INITIAL.
    EXIT.
  ENDIF.

  LOOP AT DOCUMENTS INTO DOCS.

** Changed on 01/30/12 by Furong  for ECC
*    SELECT SINGLE * FROM SOOD
*                    WHERE OBJTP = DOCS-OBJKEY+17(3)
*                      AND OBJYR = DOCS-OBJKEY+20(2)
*                      AND OBJNO = DOCS-OBJKEY+22(12).

  SELECT SINGLE * FROM SOOD
                    WHERE OBJTP = DOCS-INSTID_B+17(3)
                      AND OBJYR = DOCS-INSTID_B+20(2)
                      AND OBJNO = DOCS-INSTID_B+22(12).
** eND ON 01/30/12
    IF SY-SUBRC <> 0.
      CONTINUE.
    ENDIF.


* Create the Microsoft Application Object
    CASE SOOD-FILE_EXT.
      WHEN 'DOC'.
        CREATE OBJECT WORD  'WORD.BASIC'.
      WHEN 'XLS'.
        CREATE OBJECT EXCEL 'EXCEL.APPLICATION'.
    ENDCASE.

* Open Document.....
    CLEAR OBJECT.
    CLEAR OBJECTS. REFRESH OBJECTS.

** Changed on 01/30/12 by Furong  for ECC
*   OBJECT = DOCS-OBJKEY.  APPEND OBJECT TO OBJECTS.
   OBJECT = DOCS-INSTID_B.  APPEND OBJECT TO OBJECTS.
** end on 01/30/12

    CALL FUNCTION 'SO_DOCUMENT_DISPATCH_MANAGER'
         EXPORTING
              ACTIVITY = 'DISP'
         TABLES
              OBJECTS  = OBJECTS.

    IF HIDE =  'Y'.
*
* Hide Microsoft Application
      CASE SOOD-FILE_EXT.
        WHEN 'DOC'.
          CALL METHOD OF WORD 'APPHIDE'.
        WHEN 'XLS'.
          SET PROPERTY OF EXCEL 'VISIBLE' = 0.
      ENDCASE.


      WAIT UP TO 2 SECONDS.

* Print the Document
      CASE SOOD-FILE_EXT.
        WHEN 'DOC'.
          CALL METHOD OF WORD 'FILEPRINT'.
        WHEN 'XLS'.
          GET PROPERTY OF EXCEL 'ACTIVEWORKBOOK' = WORKBOOKS.
          CALL METHOD OF WORKBOOKS 'PRINTOUT'.
      ENDCASE.

*      WAIT UP TO 8 SECONDS.

* Close Microsoft Application
      CASE SOOD-FILE_EXT.
        WHEN 'DOC'.
          CALL METHOD OF WORD 'APPCLOSE'.
        WHEN 'XLS'.
          CALL METHOD OF EXCEL 'QUIT'.
      ENDCASE.

    ELSE.

* Minimize Microsoft Application
      CASE SOOD-FILE_EXT.
        WHEN 'DOC'.
          CALL METHOD OF WORD 'APPMINIMIZE'.
        WHEN 'XLS'.
          GET PROPERTY OF EXCEL 'APPLICATION' = APP.
*          set property of app 'WINDOWSTATE' = 2.
      ENDCASE.

    ENDIF.

* Free the Object
    CASE SOOD-FILE_EXT.
      WHEN 'DOC'.
        FREE OBJECT WORD.
      WHEN 'XLS'.
        FREE OBJECT EXCEL.
    ENDCASE.

  ENDLOOP.

ENDFORM.                    " RETRIEVE_AND_PRINT_DOCUMENTS

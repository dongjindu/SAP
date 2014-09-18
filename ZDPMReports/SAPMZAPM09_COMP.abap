************************************************************************
* Program Name      : SAPMZAPM09_COMP
* Author            : Myoungho, Park
* Creation Date     : 2003.11.04.
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       :   This program enables the user to reduce Order
*               completion steps and Create new notification and order.
*
* Modification Logs
* Date       Developer        RequestNo     Description
* 2003.11.17. Myoungho, Park
*
* 2003.11.19. Myoungho, Park
*
************************************************************************


*&---------------------------------------------------------------------*
*& Report  SAPMZAPM09_COMP                                             *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  SAPMZAPM09_COMP              MESSAGE-ID IM  .

TYPE-POOLS CXTAB .  "//Table_control Object type pool

FIELD-SYMBOLS: <TC>  TYPE CXTAB_CONTROL.  "for table control

*** Screen Structure...
TABLES: ZSPM_COMP,        "//Order Completetion Confirmation
        ZSPM_TIME_CONF,   "//Time Confirmation
        ZSPM_COUNTER.     "//Countermeasure

*** Data Select Table...
TABLES: QMUR,             "//Quality notification - causes
        QMMA,             "//Quality notification - activities.
        VIQMFE,           "//PM Notification - Item
        MKPF,             "//Header: Material Document
        MSEG,             "//Document Segment: Material
        CAUFV.

*** Interanl Table For Time Confirmation
DATA: IT_TIME_CONF LIKE ZSPM_TIME_CONF OCCURS 0 WITH HEADER LINE.
*** Internal Table For Countermeasure
DATA: IT_COUNTER   LIKE ZSPM_COUNTER   OCCURS 0 WITH HEADER LINE.

**** Internal Table For BDC Message
DATA: BEGIN OF IT_MESSAGE OCCURS 0,
        TYPE    LIKE BAPIRET2-TYPE,
        ID      LIKE BAPIRET2-ID,
        NUMBER  LIKE BAPIRET2-NUMBER,
        MESSAGE LIKE BAPIRET2-MESSAGE,
      END OF IT_MESSAGE.

**** Confirmation counter
DATA: WA_RMZHL LIKE AFRU-RMZHL,
      WA_STOKZ LIKE AFRU-STOKZ.

**** ETC Variables...
DATA : WA_MODE VALUE 'E'.        "//BDC run mode
DATA : WA_TCNAME LIKE FELD-NAME. "//table control name
DATA:  WA_ANSWER.                "//for popup message
DATA : WA_SEL.
DATA : WA_SUBRC LIKE SY-SUBRC.
DATA : WA_ORDERID TYPE AUFNR.    "//Order number
DATA : WA_INTERVAL TYPE I.
DATA : WA_COMPLETE.
DATA : WA_TABIX LIKE SY-TABIX.
DATA : WA_INIT_TXT.
DATA : WA_SAVE.
DATA : WA_VORNR LIKE ZSPM_TIME_CONF-VORNR.
DATA : WA_NO_TASK.
DATA : WA_DISP.

**** Table Control
CONTROLS: TC_0101 TYPE TABLEVIEW USING SCREEN 0100,
          TC_0102 TYPE TABLEVIEW USING SCREEN 0100.

DATA:     G_TC_0101_LINES  LIKE SY-LOOPC,
          G_TC_0102_LINES  LIKE SY-LOOPC.

*** For Status
TYPES: BEGIN OF TAB_TYPE,
        FCODE LIKE RSMPE-FUNC,
      END OF TAB_TYPE.

DATA: TAB TYPE STANDARD TABLE OF TAB_TYPE WITH
               NON-UNIQUE DEFAULT KEY INITIAL SIZE 10,
      WA_TAB TYPE TAB_TYPE.

*** For ALV
TYPE-POOLS: SLIS.

DATA : GV_REPID LIKE SY-REPID.
DATA : GV_STATUS       TYPE SLIS_FORMNAME VALUE 'PF_STATUS'.
DATA : GV_USER_COMMAND TYPE SLIS_FORMNAME VALUE 'USER_COMMAND'.
DATA : IT_SORT         TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE .
DATA : GV_COL_POS TYPE I.

DATA : IT_FIELDCAT          TYPE SLIS_T_FIELDCAT_ALV,
       WA_FIELDCAT          LIKE LINE OF IT_FIELDCAT,
       IT_EVENTCAT          TYPE SLIS_T_EVENT,
       WA_EVENTCAT          LIKE LINE OF IT_EVENTCAT.
DATA : IT_EVENTS	          TYPE SLIS_T_EVENT,
       IT_EVENT_EXIT	    TYPE SLIS_T_EVENT_EXIT.

*** FOR TEXT EDITOR
DATA : HEADLTX    LIKE THEAD.
DATA: WA_TDNAME01 LIKE HEADLTX-TDNAME.
DATA: WA_TDNAME02 LIKE HEADLTX-TDNAME.
DATA: WA_TDNAME03 LIKE HEADLTX-TDNAME.
DATA: WA_TDNAME04 LIKE HEADLTX-TDNAME.
DATA: WA_TDNAME05 LIKE HEADLTX-TDNAME.
DATA: WA_FENUM(4) TYPE N.
DATA: WA_LINES(4) TYPE N.


DATA: BEGIN OF LTXTTAB OCCURS 100.     "Langtext-Zeilen
        INCLUDE STRUCTURE TLINE.            "Zeile
DATA: END OF LTXTTAB.

DATA: BEGIN OF IT_LTXTTAB01 OCCURS 100.
        INCLUDE STRUCTURE TLINE.
DATA: END OF IT_LTXTTAB01.

DATA: BEGIN OF IT_LTXTTAB02 OCCURS 100.
        INCLUDE STRUCTURE TLINE.
DATA: END OF IT_LTXTTAB02.

DATA: BEGIN OF IT_LTXTTAB03 OCCURS 100.
        INCLUDE STRUCTURE TLINE.
DATA: END OF IT_LTXTTAB03.

DATA: BEGIN OF IT_LTXTTAB04 OCCURS 100,
        WA_TDNAME LIKE HEADLTX-TDNAME.
        INCLUDE STRUCTURE TLINE.
DATA: END OF IT_LTXTTAB04.

**** For Table Control (general function)
INCLUDE MZAPM09_COMPF01.

***** For Order Read...
INCLUDE LCMFUTPT.               "Tables
INCLUDE LCMFUTPC.               "Constants
INCLUDE LCMFUTPG.               "Global variables
INCLUDE LCMFUTPS.               "Global structures/internal tables

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  IF WA_SAVE EQ 'X' AND WA_DISP EQ ' '.
    SET PF-STATUS '0100' EXCLUDING 'SAVE'.
  ELSE.
*** If Display Mode is actived, deactivates some of the status
***  functions...
    IF WA_DISP EQ 'X'.
      CLEAR TAB.
      MOVE 'HIS' TO WA_TAB-FCODE.
      APPEND WA_TAB TO TAB.

      MOVE 'LOG' TO WA_TAB-FCODE.
      APPEND WA_TAB TO TAB.

      MOVE 'SAVE' TO WA_TAB-FCODE.
      APPEND WA_TAB TO TAB.

      SET PF-STATUS '0100' EXCLUDING TAB.

      SET TITLEBAR '0101'.

    ELSE.
      SET PF-STATUS '0100'.
      SET TITLEBAR '0100'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INITIAL_VALUE  OUTPUT
*&---------------------------------------------------------------------*
*       Initial Value for Posting date
*----------------------------------------------------------------------*
MODULE INITIAL_VALUE OUTPUT.
  IF ZSPM_COMP-IDAT2 IS INITIAL.
    ZSPM_COMP-IDAT2 = SY-DATUM.
  ENDIF.

IF ZSPM_COMP-AUSBS IS INITIAL.
    ZSPM_COMP-AUSBS = SY-DATUM.
  ENDIF.


  IF ZSPM_COMP-IDAUE IS INITIAL.
    ZSPM_COMP-IDAUE = 'MIN'.
  ENDIF.

IF ZSPM_COMP-QMNAM IS INITIAL.
    ZSPM_COMP-QMNAM = SY-UNAME.
  ENDIF.

ENDMODULE.                 " INITIAL_VALUE  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TABLE_CONTROL_OUTPUT_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TABLE_CONTROL_OUTPUT_0101 OUTPUT.

  READ TABLE IT_TIME_CONF INDEX TC_0101-CURRENT_LINE .
  IF SY-SUBRC EQ 0.
    IT_TIME_CONF-VORNR = TC_0101-CURRENT_LINE * 10.
    MOVE-CORRESPONDING IT_TIME_CONF TO ZSPM_TIME_CONF.
  ELSE.
    CLEAR ZSPM_TIME_CONF.
  ENDIF.
ENDMODULE.                 " TABLE_CONTROL_OUTPUT_0101  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_TABLE_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_SCREEN_TABLE_0101 OUTPUT.
  PERFORM MODIFY_SCREEN_INPUT_TABLE_0101.
ENDMODULE.                 " MODIFY_SCREEN_TABLE_0101  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TABLE_CONTROL_LINES_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TABLE_CONTROL_LINES_0101 OUTPUT.
*--- Internal Table Lines Number to Table Contral Lines Number.

  DESCRIBE TABLE IT_TIME_CONF LINES TC_0101-LINES.
ENDMODULE.                 " TABLE_CONTROL_LINES_0101  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TABLE_CONTROL_OUTPUT_0102  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TABLE_CONTROL_OUTPUT_0102 OUTPUT.
  READ TABLE IT_COUNTER INDEX TC_0102-CURRENT_LINE .
  IF SY-SUBRC EQ 0.
    IT_COUNTER-POSNR = TC_0102-CURRENT_LINE .
    MOVE-CORRESPONDING IT_COUNTER TO ZSPM_COUNTER.
  ELSE.
    CLEAR ZSPM_COUNTER.
  ENDIF.
ENDMODULE.                 " TABLE_CONTROL_OUTPUT_0102  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_TABLE_0102  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_SCREEN_TABLE_0102 OUTPUT.
  PERFORM MODIFY_SCREEN_INPUT.
ENDMODULE.                 " MODIFY_SCREEN_TABLE_0102  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TABLE_CONTROL_LINES_0102  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TABLE_CONTROL_LINES_0102 OUTPUT.
*--- Internal Table Lines Number to Table Contral Lines Number.
  DESCRIBE TABLE IT_COUNTER LINES TC_0102-LINES.
ENDMODULE.                 " TABLE_CONTROL_LINES_0102  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.
*--- FUNCTION  TYPE E.
  CASE SY-UCOMM.
    WHEN 'BACK'.
      CLEAR: SY-UCOMM.
      PERFORM LEAVE_PROGRAM.

    WHEN 'RW'.
      CLEAR: SY-UCOMM.
      PERFORM LEAVE_PROGRAM.

    WHEN '%EX'.
      CLEAR: SY-UCOMM.
      PERFORM LEAVE_PROGRAM.

    WHEN OTHERS.
      CLEAR: SY-UCOMM.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Form  LEAVE_PROGRAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LEAVE_PROGRAM.
  IF SY-DYNNR = '0100' AND WA_SAVE EQ SPACE AND WA_DISP EQ SPACE.
    CLEAR: WA_DISP.

    PERFORM CONFIRM_NEXT_STEP USING  TEXT-M02
                                     TEXT-M03 .
    IF  WA_ANSWER EQ 'J'.
      CLEAR: SY-UCOMM.
      CLEAR: IT_MESSAGE, IT_MESSAGE[].

**** Lock Tables...
      PERFORM ORDER_DELOCK.

**** Change order...
      PERFORM ORDER_CHANGE.
      SORT IT_TIME_CONF BY VORNR.
      IF WA_SUBRC EQ 0.
        LOOP AT IT_TIME_CONF WHERE LEKNW EQ ' '.
          CLEAR: WA_VORNR.
          WA_VORNR = IT_TIME_CONF-VORNR * 10.
          PERFORM TIME_CONFIRMATION USING WA_VORNR.
        ENDLOOP.

        IF WA_SUBRC EQ 0.
          LOOP AT IT_COUNTER.
            WA_TABIX = SY-TABIX.
*          READ TABLE IT_COUNTER INDEX 1.
*          IF SY-SUBRC EQ 0.
            PERFORM CREATE_ORDER_REF_NOTI.
*          ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.

      LEAVE TO SCREEN 0.

    ELSEIF WA_ANSWER EQ 'N'.
      CLEAR: WA_DISP.
      IF SY-DYNNR EQ '0100'.
**** Lock Tables...
        PERFORM ORDER_DELOCK.
      ENDIF.
      LEAVE TO SCREEN 0.
    ENDIF.
  ELSE.
    CLEAR: WA_DISP.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDFORM.                    " LEAVE_PROGRAM
*&---------------------------------------------------------------------*
*&      Form  CONFIRM_NEXT_STEP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_M02  text
*      -->P_TEXT_M03  text
*----------------------------------------------------------------------*
FORM CONFIRM_NEXT_STEP USING    P_TITLE
                                P_TEXT.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            DEFAULTOPTION  = 'Y'
            TEXTLINE1      = P_TEXT
            TITEL          = P_TITLE
            CANCEL_DISPLAY = 'X'
       IMPORTING
            ANSWER         = WA_ANSWER.
ENDFORM.                    " CONFIRM_NEXT_STEP
*&---------------------------------------------------------------------*
*&      Module  CHECK_ORDER  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_ORDER INPUT.
  PERFORM READ_ORDER.
ENDMODULE.                 " CHECK_ORDER  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_POSTING_DATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_POSTING_DATE INPUT.
*  PERFORM CHECK_BUDAT.
ENDMODULE.                 " CHECK_POSTING_DATE  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_MALFUNTION_INFO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_MALFUNTION_INFO INPUT.
  IF ZSPM_COMP-AUSVN IS INITIAL
*     OR ZSPM_COMP-AUSBS IS INITIAL
     OR ZSPM_COMP-AUZTV IS INITIAL.
*     OR ZSPM_COMP-AUZTB IS INITIAL.
*    MESSAGE E000(ZMPM) WITH TEXT-M10.
  ELSE.
    CHECK NOT ZSPM_COMP-AUSBS IS INITIAL
          AND   NOT ZSPM_COMP-AUZTB IS INITIAL.

    CLEAR: WA_INTERVAL.
    PERFORM CHECK_TIME_INTERVAL USING  ZSPM_COMP-AUSVN
                                       ZSPM_COMP-AUZTV
                                       ZSPM_COMP-AUSBS
                                       ZSPM_COMP-AUZTB
                             CHANGING  WA_INTERVAL.
    IF WA_INTERVAL < 0.
      MESSAGE E000(ZMPM) WITH TEXT-M07.
    ELSE.
      MOVE : WA_INTERVAL TO ZSPM_COMP-EAUSZT,
             'MIN'       TO ZSPM_COMP-MAUEH.
    ENDIF.

**** CHECK malfunction start time is in the future
    CLEAR: WA_INTERVAL.
    PERFORM CHECK_TIME_INTERVAL USING  ZSPM_COMP-AUSVN
                                       ZSPM_COMP-AUZTV
                                       SY-DATUM  "//ZSPM_COMP-AUSBS
                                       SY-UZEIT  "//ZSPM_COMP-AUZTB
                             CHANGING  WA_INTERVAL.
    IF WA_INTERVAL < 0.
      MESSAGE E000(ZMPM) WITH TEXT-M14.
    ENDIF.

**** CHECK malfunction end time is in the future
    CLEAR: WA_INTERVAL.
    PERFORM CHECK_TIME_INTERVAL USING  ZSPM_COMP-AUSBS
                                       ZSPM_COMP-AUZTB
                                       SY-DATUM
                                       SY-UZEIT
                             CHANGING  WA_INTERVAL.
    IF WA_INTERVAL < 0.
      MESSAGE E000(ZMPM) WITH TEXT-M15.
    ENDIF.
  ENDIF.
ENDMODULE.                 " CHECK_MALFUNTION_INFO  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_WORK_TIME_INFO  INPUT
*&---------------------------------------------------------------------*
*       Check Time confirmation data...
*----------------------------------------------------------------------*
MODULE CHECK_WORK_TIME_INFO INPUT.

  IF NOT ZSPM_TIME_CONF-VORNR IS INITIAL.
**** Check for Confirmed date for start of execution..

    IF ZSPM_TIME_CONF-ISDD IS INITIAL
       OR ZSPM_TIME_CONF-ISDZ EQ '000000'
       OR ZSPM_TIME_CONF-IEDD IS INITIAL
       OR ZSPM_TIME_CONF-IEDZ EQ '000000'.
*      MESSAGE E000(ZMPM) WITH TEXT-M04.
    ELSE.


*      IF ZSPM_TIME_CONF-ISDD > SY-DATUM
*         OR ZSPM_TIME_CONF-IEDD > SY-DATUM.
*        MESSAGE E000(ZMPM) WITH TEXT-M05.
*      ELSE.
**** Check Work start time is inthe future.
      CLEAR: WA_INTERVAL.
      PERFORM CHECK_TIME_INTERVAL USING ZSPM_TIME_CONF-ISDD
                                        ZSPM_TIME_CONF-ISDZ
                                        SY-DATUM  "//ZSPM_TIME_CONF-IEDD
                                        SY-UZEIT  "//ZSPM_TIME_CONF-IEDZ
                                  CHANGING WA_INTERVAL.
      IF WA_INTERVAL < 0.
        MESSAGE E000(ZMPM) WITH TEXT-M16.
      ENDIF.

**** Check Work end time is inthe future.
      CLEAR: WA_INTERVAL.
      PERFORM CHECK_TIME_INTERVAL USING ZSPM_TIME_CONF-IEDD
                                        ZSPM_TIME_CONF-IEDZ
                                        SY-DATUM
                                        SY-UZEIT
                                  CHANGING WA_INTERVAL.
      IF WA_INTERVAL < 0.
        MESSAGE E000(ZMPM) WITH TEXT-M17.
      ENDIF.

      CLEAR: WA_INTERVAL.
      PERFORM CHECK_TIME_INTERVAL USING ZSPM_TIME_CONF-ISDD
                                        ZSPM_TIME_CONF-ISDZ
                                        ZSPM_TIME_CONF-IEDD
                                        ZSPM_TIME_CONF-IEDZ
                                  CHANGING WA_INTERVAL.
      IF WA_INTERVAL < 0.
        MESSAGE E000(ZMPM) WITH TEXT-M06.
      ELSE.
*        CLEAR: WA_INTERVAL.
*        PERFORM CHECK_TIME_INTERVAL USING ZSPM_COMP-AUSVN
*                                          ZSPM_COMP-AUZTV
*                                          ZSPM_TIME_CONF-ISDD
*                                          ZSPM_TIME_CONF-ISDZ
*                                    CHANGING WA_INTERVAL.
*        IF WA_INTERVAL < 0.
*          MESSAGE E000(ZMPM) WITH TEXT-M08.
*        ENDIF.

      ENDIF.

    ENDIF.
  ENDIF.
ENDMODULE.                 " CHECK_WORK_TIME_INFO  INPUT
*&---------------------------------------------------------------------*
*&      Module  TABLE_CONTROL_INPUT_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TABLE_CONTROL_INPUT_0101 INPUT.

  READ TABLE IT_TIME_CONF INDEX TC_0101-CURRENT_LINE.
  IF SY-SUBRC NE 0.
    ZSPM_TIME_CONF-VORNR = TC_0101-CURRENT_LINE * 10.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
         EXPORTING
              INPUT  = ZSPM_TIME_CONF-VORNR
         IMPORTING
              OUTPUT = ZSPM_TIME_CONF-VORNR.

    MOVE-CORRESPONDING ZSPM_TIME_CONF TO IT_TIME_CONF.
    APPEND IT_TIME_CONF.
  ELSE.
    MOVE-CORRESPONDING ZSPM_TIME_CONF TO IT_TIME_CONF.

    MODIFY IT_TIME_CONF INDEX TC_0101-CURRENT_LINE TRANSPORTING
                                                        VORNR
                                                        LTXA1
                                                        ISDD
                                                        ISDZ
                                                        IEDD
                                                        IEDZ
                                                        ANZZL
                                                        CHECK.
  ENDIF.
ENDMODULE.                 " TABLE_CONTROL_INPUT_0101  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0101_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_0101_USER_COMMAND INPUT.
  PERFORM USER_OK_TC USING    'TC_0101'
                              'IT_TIME_CONF'
                              'CHECK'
                     CHANGING SY-UCOMM.
ENDMODULE.                 " TC_0101_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*&      Module  TABLE_CONTROL_INPUT_0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TABLE_CONTROL_INPUT_0102 INPUT.

  READ TABLE IT_COUNTER INDEX TC_0102-CURRENT_LINE.
  IF SY-SUBRC NE 0.
    MOVE TC_0102-CURRENT_LINE TO ZSPM_COUNTER-POSNR.
    MOVE-CORRESPONDING ZSPM_COUNTER TO IT_COUNTER.
    APPEND IT_COUNTER.
  ELSE.
    MOVE-CORRESPONDING ZSPM_COUNTER TO IT_COUNTER.

    MODIFY IT_COUNTER INDEX TC_0102-CURRENT_LINE TRANSPORTING
                                                      POSNR
                                                      KTEXT
                                                      IDAT2
                                                      AUFNR
                                                      CHECK.
  ENDIF.
* MODIFICATION BY 100565
READ TABLE IT_COUNTER INDEX TC_0102-CURRENT_LINE.
  IF SY-SUBRC = 0.
  IF ZSPM_COMP-MNCOD IS INITIAL OR
  ZSPM_COMP-URCOD IS INITIAL OR
  ZSPM_COMP-FECOD IS INITIAL OR
  ZSPM_COMP-OTEIL IS INITIAL.
   MESSAGE E000(ZMPM) WITH TEXT-M20.

   ENDIF.
   ENDIF.
* END MODIFICATION

ENDMODULE.                 " TABLE_CONTROL_INPUT_0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0102_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_0102_USER_COMMAND INPUT.
  PERFORM USER_OK_TC USING    'TC_0102'
                              'IT_COUNTER'
                              'CHECK'
                     CHANGING SY-UCOMM.

ENDMODULE.                 " TC_0102_USER_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_MESSAGE.
  CALL SCREEN 0110.
ENDFORM.                    " DISPLAY_MESSAGE

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE SY-UCOMM.
*    WHEN 'BACK'.
*      CLEAR: SY-UCOMM.
*      PERFORM LEAVE_PROGRAM.

    WHEN 'HIS'.
*** Call List program PM Order Goods movement ....
      CLEAR: SY-UCOMM.
      PERFORM GOODS_MOVEMENT_HISTORY.

* Changed by 100565 req by CK Lee 07.26.04 - start
  WHEN 'LNK'.
*** Call URL to display lead time ....
      CLEAR: SY-UCOMM.
*CALL SCREEN '0999'.
CALL FUNCTION 'RH_URL_CALL_BROWSER'
 EXPORTING
   URL      = 'http://10.37.107.217/emossm-PS/index.jsp'
* EXCEPTIONS
*   BROWSER_START_FAILED       = 1
*   NO_BATCH                   = 2
*   URL_IS_EMPTY               = 3
*   OTHERS                     = 4
          .
IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

* Changed by 100565 req by CK Lee 07.26.04 - end


    WHEN 'TXT01'.
      CLEAR: SY-UCOMM.
*** Long Text for Object Part Code & Problem Code

      CLEAR: WA_TDNAME01, HEADLTX, WA_INIT_TXT.
      CLEAR: LTXTTAB, LTXTTAB[].

      CONCATENATE ZSPM_COMP-QMNUM
*                  ZSPM_COMP-FENUM
                   '0001'
*                  ZSPM_COMP-URNUM
                  INTO WA_TDNAME01.

      HEADLTX-TDID    = 'LTXT'.
      HEADLTX-TDSPRAS = 'E'.
      HEADLTX-TDNAME  = WA_TDNAME01.
      HEADLTX-TDOBJECT = 'QMFE'.
      HEADLTX-TDLINESIZE = 072.

      IF IT_LTXTTAB01[] IS INITIAL.
        WA_INIT_TXT = 'X'.
      ELSE.
        LTXTTAB[] = IT_LTXTTAB01[].
      ENDIF.
      PERFORM CALL_EDITOR USING HEADLTX
                                WA_INIT_TXT.

      IT_LTXTTAB01[] =  LTXTTAB[].

    WHEN 'TXT02'.
      CLEAR: SY-UCOMM.
*** Long Text for Cause Code

      CLEAR: WA_TDNAME02, HEADLTX, WA_INIT_TXT.
      CLEAR: LTXTTAB, LTXTTAB[].

      CONCATENATE ZSPM_COMP-QMNUM
*                  ZSPM_COMP-FENUM
                  '0001'
*                  ZSPM_COMP-URNUM
                  '0001'
                  INTO WA_TDNAME02.

      HEADLTX-TDID    = 'LTXT'.
      HEADLTX-TDSPRAS = 'E'.
      HEADLTX-TDNAME  = WA_TDNAME02.
      HEADLTX-TDOBJECT = 'QMUR'.
      HEADLTX-TDLINESIZE = 072.

      IF IT_LTXTTAB02[] IS INITIAL.
        WA_INIT_TXT = 'X'.
      ELSE.
        LTXTTAB[] = IT_LTXTTAB02[].
      ENDIF.
      PERFORM CALL_EDITOR USING HEADLTX
                                WA_INIT_TXT.

      IT_LTXTTAB02[] =  LTXTTAB[].

    WHEN 'TXT03'.
      CLEAR: SY-UCOMM.
**** Long  Text for Activity Code

      CLEAR: WA_TDNAME03, HEADLTX, WA_INIT_TXT.
      CLEAR: LTXTTAB, LTXTTAB[].

      CONCATENATE ZSPM_COMP-QMNUM
                  '0001'
*                  ZSPM_COMP-MANUM
                  INTO WA_TDNAME03.

      HEADLTX-TDID    = 'LTXT'.
      HEADLTX-TDSPRAS = 'E'.
      HEADLTX-TDNAME  = WA_TDNAME03.
      HEADLTX-TDOBJECT = 'QMMA'.
      HEADLTX-TDLINESIZE = 072.

      IF IT_LTXTTAB03[] IS INITIAL.
        WA_INIT_TXT = 'X'.
      ELSE.
        LTXTTAB[] = IT_LTXTTAB03[].
      ENDIF.
      PERFORM CALL_EDITOR USING HEADLTX
                                WA_INIT_TXT.

      IT_LTXTTAB03[] =  LTXTTAB[].

    WHEN 'SAVE'.
      CLEAR: SY-UCOMM.
      CLEAR: IT_MESSAGE, IT_MESSAGE[].

      LOOP AT  IT_TIME_CONF.
        IF  IT_TIME_CONF-ISDD IS INITIAL
            AND IT_TIME_CONF-ISDZ EQ '000000'
            AND IT_TIME_CONF-IEDD IS INITIAL
            AND IT_TIME_CONF-IEDZ EQ '000000'.
          CONTINUE.
        ENDIF.

        IF IT_TIME_CONF-ISDD IS INITIAL
            OR IT_TIME_CONF-ISDZ EQ '000000'
            OR IT_TIME_CONF-IEDD IS INITIAL
            OR IT_TIME_CONF-IEDZ EQ '000000'.
          MESSAGE E000(ZMPM) WITH TEXT-M04.
        ENDIF.
      ENDLOOP.

**** Lock Tables...
      PERFORM ORDER_DELOCK.

**** Change order...
      PERFORM ORDER_CHANGE.
      IF WA_SUBRC EQ 0.
**** Check No Remaining Work Expected Indicator
**** process only indicator is space.

        LOOP AT IT_TIME_CONF WHERE LEKNW EQ ' '.
          IF NOT  IT_TIME_CONF-ISDZ IS INITIAL.
            CLEAR: WA_VORNR.
            WA_VORNR = IT_TIME_CONF-VORNR.
            PERFORM TIME_CONFIRMATION USING WA_VORNR.
          ENDIF.
        ENDLOOP.
        IF WA_SUBRC EQ 0.
          LOOP AT IT_COUNTER.
            WA_TABIX = SY-TABIX.
*          READ TABLE IT_COUNTER INDEX 1.
*          IF SY-SUBRC EQ 0.
            PERFORM CREATE_ORDER_REF_NOTI.
*          ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.

      IF WA_SUBRC NE 0.
        MESSAGE E000(ZMPM) WITH TEXT-M13.
      ELSE.
        WA_SAVE = 'X'.
        MESSAGE S000(ZMPM) WITH TEXT-M18.
      ENDIF.

    WHEN 'LOG'.
      CLEAR: SY-UCOMM.
      PERFORM DISPLAY_MESSAGE.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  OTEIL_F4  INPUT
*&---------------------------------------------------------------------*
*       Search Help for Part of Object
*----------------------------------------------------------------------*
MODULE OTEIL_F4 INPUT.
  PERFORM CALL_CODE_SELEKTION_F40 USING 'B'
                                        'PM01'.

ENDMODULE.                 " OTEIL_F4  INPUT
*&---------------------------------------------------------------------*
*&      Form  CALL_CODE_SELEKTION_F40
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_CODE_SELEKTION_F40 USING P_LKATART
                                   P_CDGRP.

  DATA: H_CODE LIKE VIQMFE-FECOD VALUE ' ' .

  DATA: BEGIN OF IQPK1CD OCCURS 10.
          INCLUDE STRUCTURE QPK1CD.
  DATA: END   OF IQPK1CD.

  DATA: BEGIN OF G_CODEGRPTAB OCCURS 0.
          INCLUDE STRUCTURE QPK1CODEGRP.
  DATA: END   OF G_CODEGRPTAB.

  H_CODE = '*'.

*--- Code Selektion ---------------------------------------------------*
  CALL FUNCTION 'QPK1_GP_CODE_SELECTION'
       EXPORTING
            I_KATALOGART           = P_LKATART
            I_CODEGRUPPE           = P_CDGRP
            I_CODE                 = H_CODE
            I_SPRACHE              = SY-LANGU
            I_PICKUP_MODE          = 'X'
*            I_DISPLAY_MODE         = LDISP
            I_RETURN_IF_ONE        = 'X'
       TABLES
            T_QPK1CDTAB            = IQPK1CD
            T_CODEGRPTAB           = G_CODEGRPTAB
       EXCEPTIONS
            NO_MATCH_IN_RANGE      = 01
            NO_USER_SELECTION      = 02
            NO_AUTHORIZATION       = 03
            NO_SELECTION_SPECIFIED = 04
            OBJECT_LOCKED          = 05
            LOCK_ERROR             = 06
            OBJECT_MISSING         = 07.

  CASE SY-SUBRC.
    WHEN 0.
*--- Pruefen ob was selektiert worden ist -----------------------------*
      DESCRIBE TABLE IQPK1CD LINES SY-TFILL.
      CHECK SY-TFILL > 0.

      LOOP AT IQPK1CD.
        PERFORM SELEKTION_BEARBEITEN_F40 USING P_LKATART
                                               IQPK1CD.
      ENDLOOP.


    WHEN 1.
      MESSAGE E103(IM) WITH  P_CDGRP H_CODE.

    WHEN 2.

    WHEN 3.
      MESSAGE E291(QS).

    WHEN 4.
      MESSAGE E343(QS).

    WHEN 5.
      MESSAGE E100(Q0).

    WHEN 6.
      MESSAGE E101(Q0).

    WHEN 7.
      MESSAGE E267(QS).

  ENDCASE.

ENDFORM.                    " CALL_CODE_SELEKTION_F40
*&---------------------------------------------------------------------*
*&      Module  FECOD_F4  INPUT
*&---------------------------------------------------------------------*
*       Search Help for Problem
*----------------------------------------------------------------------*
MODULE FECOD_F4 INPUT.
  PERFORM CALL_CODE_SELEKTION_F40 USING 'C'
                                        'PM01'.
ENDMODULE.                 " FECOD_F4  INPUT
*&---------------------------------------------------------------------*
*&      Module  URCOD_F4  INPUT
*&---------------------------------------------------------------------*
*       Search Help for Cause Code
*----------------------------------------------------------------------*
MODULE URCOD_F4 INPUT.
  PERFORM CALL_CODE_SELEKTION_F40 USING '5'
                                        'PM01'.
ENDMODULE.                 " URCOD_F4  INPUT
*&---------------------------------------------------------------------*
*&      Module  MNCOD_F4  INPUT
*&---------------------------------------------------------------------*
*       Search Help for Activity Code
*----------------------------------------------------------------------*
MODULE MNCOD_F4 INPUT.
  PERFORM CALL_CODE_SELEKTION_F40 USING 'A'
                                        'PM01'.
ENDMODULE.                 " MNCOD_F4  INPUT
*&---------------------------------------------------------------------*
*&      Form  GOODS_MOVEMENT_HISTORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GOODS_MOVEMENT_HISTORY.
  DATA: WA_SELTAB  LIKE RSPARAMS OCCURS 0 WITH HEADER LINE.

  MOVE: 'AUFNR'         TO WA_SELTAB-SELNAME,
        'S'             TO WA_SELTAB-KIND,      " SELECT-OPTION
        'I'             TO WA_SELTAB-SIGN,
        'EQ'            TO WA_SELTAB-OPTION,
        ZSPM_COMP-AUFNR TO WA_SELTAB-LOW.
  APPEND WA_SELTAB.

  SUBMIT RIAUFM20 WITH  SELECTION-TABLE WA_SELTAB
                  WITH DY_WAP EQ 'X'
                  WITH DY_WAU EQ 'X'
                  WITH DY_WEB EQ 'X'
                  WITH DY_WEF EQ 'X'
                  AND RETURN.
ENDFORM.                    " GOODS_MOVEMENT_HISTORY
*&---------------------------------------------------------------------*
*&      Form  CALL_EDITOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_HEADLTX  text
*----------------------------------------------------------------------*
FORM CALL_EDITOR USING  P_HEADLTX LIKE THEAD
                        P_INIT_TXT.

  DATA: TXFUNCTION(1) TYPE C,                 "Langtextfunktion (I,U,D)
        RESULT LIKE ITCER.

  IF P_INIT_TXT EQ 'X'.
    CALL FUNCTION 'READ_TEXT'
         EXPORTING
              ID        = P_HEADLTX-TDID
              LANGUAGE  = P_HEADLTX-TDSPRAS
              NAME      = P_HEADLTX-TDNAME
              OBJECT    = P_HEADLTX-TDOBJECT
         IMPORTING
              HEADER    = P_HEADLTX
         TABLES
              LINES     = LTXTTAB
         EXCEPTIONS
              NOT_FOUND = 1.
  ENDIF.

  CALL FUNCTION 'EDIT_TEXT'
       EXPORTING
            DISPLAY      = WA_DISP
            HEADER       = P_HEADLTX
            SAVE         = ' '
*            EDITOR_TITLE = ' '
*            LINE_EDITOR  = ' '
*            CONTROL      = ' '
*            PROGRAM      = ' '
       IMPORTING
            FUNCTION     = TXFUNCTION
            NEWHEADER    = P_HEADLTX
            RESULT       = RESULT
       TABLES
            LINES        = LTXTTAB.

ENDFORM.                    " CALL_EDITOR
*&---------------------------------------------------------------------*
*&      Form  SELEKTION_BEARBEITEN_F40
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IQPK1CD  text
*----------------------------------------------------------------------*
FORM SELEKTION_BEARBEITEN_F40 USING    PP_LKATART
                                       P_IQPK1CD LIKE QPK1CD.

  DATA: IT_DYNPFIELDS LIKE DYNPREAD OCCURS 0 WITH HEADER LINE,
        WA_DYNAME LIKE SY-REPID,
        WA_DYNUMB LIKE SY-DYNNR.

  CASE PP_LKATART.
    WHEN 'B'.
      ZSPM_COMP-OTEIL   = P_IQPK1CD-CODE .   "//Part of Object
***   Short Text for Object Part Code
      WA_DYNAME = SY-REPID.
      WA_DYNUMB = SY-DYNNR.

      IT_DYNPFIELDS-FIELDNAME  = 'ZSPM_COMP-TXTCDOT'.
      IT_DYNPFIELDS-STEPL      = SY-STEPL.
      IT_DYNPFIELDS-FIELDVALUE = P_IQPK1CD-KURZTEXTCD.
      APPEND IT_DYNPFIELDS.

    WHEN 'C'.
      ZSPM_COMP-FECOD = P_IQPK1CD-CODE.	"//Problem
***   Problem Code Short Text
      WA_DYNAME = SY-REPID.
      WA_DYNUMB = SY-DYNNR.

      IT_DYNPFIELDS-FIELDNAME  = 'ZSPM_COMP-TXTCDFE'.
      IT_DYNPFIELDS-STEPL      = SY-STEPL.
      IT_DYNPFIELDS-FIELDVALUE = P_IQPK1CD-KURZTEXTCD.
      APPEND IT_DYNPFIELDS.

    WHEN '5'.
      ZSPM_COMP-URCOD = P_IQPK1CD-CODE.	"//Cause Code	
***   Short Text for Cause Code
      WA_DYNAME = SY-REPID.
      WA_DYNUMB = SY-DYNNR.

      IT_DYNPFIELDS-FIELDNAME  = 'ZSPM_COMP-TXTCDUR'.
      IT_DYNPFIELDS-STEPL      = SY-STEPL.
      IT_DYNPFIELDS-FIELDVALUE = P_IQPK1CD-KURZTEXTCD.
      APPEND IT_DYNPFIELDS.

    WHEN 'A'.
      ZSPM_COMP-MNCOD = P_IQPK1CD-CODE.	"//Activity Code	
***   Short Text for Activity Code
      WA_DYNAME = SY-REPID.
      WA_DYNUMB = SY-DYNNR.

      IT_DYNPFIELDS-FIELDNAME  = 'ZSPM_COMP-TXTCDMA'.
      IT_DYNPFIELDS-STEPL      = SY-STEPL.
      IT_DYNPFIELDS-FIELDVALUE = P_IQPK1CD-KURZTEXTCD.
      APPEND IT_DYNPFIELDS.
  ENDCASE.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
       EXPORTING
            DYNAME     = WA_DYNAME
            DYNUMB     = WA_DYNUMB
       TABLES
            DYNPFIELDS = IT_DYNPFIELDS.
ENDFORM.                    " SELEKTION_BEARBEITEN_F40
*&---------------------------------------------------------------------*
*&      Form  ORDER_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ORDER_READ.
  CLEAR :  GT_OPER, GT_OPER[],
           GT_SOPER, GT_SOPER[],
           GT_AFVGD, GT_AFVGD[].

  CLEAR :  GS_CAUFVD_START.

  IF CMFUD-AUFNR <> GS_CMFUD_OLD-AUFNR.
    IF GS_CMFUD_OLD-AUFNR IS INITIAL.
*** Check order and read order header data
      PERFORM ORDER_CHECK_READ TABLES   GT_OPER
                                        GT_SOPER
                               USING    CMFUD
                               CHANGING CAUFVD
                                        CORUF.

      CMFUD-KTEXT = CAUFVD-KTEXT.                           "note216322
      CMFUD-KUNUM = CAUFVD-KUNUM.                           "note216322
*...Save order like it was when transaction started
      GS_CAUFVD_START = CAUFVD.
*...Read table with all operations and suboperations
      PERFORM OPER_TABLE_CREATE TABLES  GT_OPER
                                        GT_SOPER
                                        GT_AFVGD.
    ELSE.
*...New order number entered. Send PopUP to confirm
*      PERFORM ORDER_CHANGE USING    GS_CMFUD_OLD
*                           CHANGING OK-CODE
*                                    CMFUD.
    ENDIF.
*...Lock the order
*    PERFORM ORDER_LOCK USING CAUFVD.
  ENDIF.

ENDFORM.                    " ORDER_READ
*&---------------------------------------------------------------------*
*&      Form  order_check_read
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OPER  text
*      -->P_GT_SOPER  text
*      -->P_CMFUD  text
*      <--P_CAUFVD  text
*      <--P_CORUF  text
*----------------------------------------------------------------------*
FORM ORDER_CHECK_READ TABLES   P_T_OPER STRUCTURE GT_OPER
                               P_T_SOPER STRUCTURE GT_SOPER
                      USING    P_CMFUD LIKE CMFUD
                      CHANGING P_CAUFVD LIKE CAUFVD
                               P_CORUF LIKE CORUF.

* Fill CORUF for function interface
  MOVE-CORRESPONDING P_CMFUD TO P_CORUF.

  CALL FUNCTION 'CO_RU_RESET_ORDER_HEADER'.
  CALL FUNCTION 'CO_RU_DATA_RESET'.

* Read order header
  CALL FUNCTION 'CO_RU_GET_ORDER_HEADER'
       EXPORTING
            CORUF_IMP                = P_CORUF
            TRANS_TYP_IMP            = CON_H
            TRANS_AUTYP_IMP          = CON_AUTYP_INST
       IMPORTING
            CORUF_EXP                = P_CORUF
            CAUFVD_EXP               = P_CAUFVD
       EXCEPTIONS
            KEY_NOT_DEFINED          = 1
            MISSING_AUTHORITY        = 2
            ORDER_CATEGORY_NOT_VALID = 3
            ORDER_NOT_FOUND          = 4
            TABLE_ENTRY_NOT_FOUND    = 5
            OTHERS                   = 6.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
* Set Parameters: TCORU CORUF CORF
  PERFORM PARAMETER_SET.

* Check order header
*  CALL FUNCTION 'CO_RU_CHECK_ORDER_HEADER'
*       EXPORTING
*            AUFNR_IMP               = P_CMFUD-AUFNR
*            TRANS_TYPE_IMP          = CON_H
*            COLLECT_FLAG            = CON_X
*       EXCEPTIONS
*            MIXED_CONFIRMATION      = 1
*            NEW_STATUS_NOT_POSSIBLE = 2
*            ORDER_CONFIRMED         = 3
*            ORDER_DATA_MISSING      = 4
*            ORDER_DELETED           = 5
*            ORDER_NOT_FREE          = 6
*            ORDER_CLOSED            = 7
*            ORDER_SPLIT             = 8
*            OTHERS                  = 9.
*
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.

* Get order data
  CALL FUNCTION 'CO_RU_GET_ORDER_DATA'
       EXPORTING
            AUFNR_IMP                    = P_CMFUD-AUFNR
            FLG_DIALOGTAB_CREATE         = CON_X
            TRTYP_IMP                    = CON_H
       IMPORTING
            CAUFVD_EXP                   = P_CAUFVD
       TABLES
            DIATAB_POS                   = P_T_OPER
            DIATAB_SOP                   = P_T_SOPER
       EXCEPTIONS
            MATERIAL_DATA_NOT_FOUND      = 1
            UNIT_CONVERSION_NOT_POSSIBLE = 2
            PREDEC_NOT_CONFIRMED         = 3
            OTHERS                       = 4.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " order_check_read
*&---------------------------------------------------------------------*
*&      Form  oper_table_create
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OPER  text
*      -->P_GT_SOPER  text
*      -->P_GT_AFVGD  text
*----------------------------------------------------------------------*
FORM OPER_TABLE_CREATE TABLES   P_OPER STRUCTURE RCLST
                              P_SOPER STRUCTURE RCLST
                              P_T_AFVGD STRUCTURE AFVGD.

* Operations
  LOOP AT P_OPER
    WHERE INDEX_SOPR EQ 0.
    CALL FUNCTION 'CO_RU_READ_AFVGD_WITH_INDEX'
         EXPORTING
              INDEX_IMP = P_OPER-INDEX_PLPO
         IMPORTING
              AFVGD_EXP = P_T_AFVGD.
*   Append operations
    APPEND P_T_AFVGD.
  ENDLOOP.

* Suboperations
  LOOP AT P_SOPER.
    CALL FUNCTION 'CO_RU_READ_AFVGD_WITH_INDEX'
         EXPORTING
              INDEX_IMP = P_SOPER-INDEX_SOPR
         IMPORTING
              AFVGD_EXP = P_T_AFVGD.

*   Append operations to table P_T_RUECKSAM_FILL
    APPEND P_T_AFVGD.
  ENDLOOP.

* Sort by operations and suboperations
  SORT P_T_AFVGD BY VORNR UVORN.

  CLEAR: IT_TIME_CONF, IT_TIME_CONF[].

  LOOP AT P_T_AFVGD.
    MOVE-CORRESPONDING P_T_AFVGD TO IT_TIME_CONF.
    APPEND IT_TIME_CONF.
  ENDLOOP.

ENDFORM.                    " oper_table_create
*&---------------------------------------------------------------------*
*&      Form  order_change
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_CMFUD_OLD  text
*      <--P_OK_CODE  text
*      <--P_CMFUD  text
*----------------------------------------------------------------------*
FORM ORDER_CHANGE.
  DATA: IT_MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.
  DATA: WA_TEXT(200).
  DATA: WA_MSGNR LIKE SY-MSGNO,
        WA_MSGID LIKE SY-MSGID,
        WA_MSGV1 LIKE SY-MSGV1,
        WA_MSGV2 LIKE SY-MSGV2,
        WA_MSGV3 LIKE SY-MSGV3,
        WA_MSGV4 LIKE SY-MSGV4.

  CLEAR: WA_SUBRC.

  CLEAR: WA_COMPLETE.

  READ TABLE IT_COUNTER INDEX 1.
  IF SY-SUBRC EQ 0.
    WA_COMPLETE = 'X'.
  ENDIF.
  CLEAR: IT_TIME_CONF, IT_COUNTER.

  CALL FUNCTION 'Z_FPM_CHANGE_ORDER'
       EXPORTING
            CTU            = 'X'
            MODE           = WA_MODE
            UPDATE         = 'L'
            ZSPM_COMP      = ZSPM_COMP
            COMPLETE       = WA_COMPLETE
       IMPORTING
            SUBRC          = WA_SUBRC
       TABLES
            ZSPM_TIME_CONF = IT_TIME_CONF
            ZSPM_COUNTER   = IT_COUNTER
            MESSTAB        = IT_MESSTAB.

  LOOP AT IT_MESSTAB.
    CLEAR: WA_TEXT.
    MOVE:   IT_MESSTAB-MSGNR TO WA_MSGNR,
            IT_MESSTAB-MSGID TO WA_MSGID,
            IT_MESSTAB-MSGV1 TO WA_MSGV1,
            IT_MESSTAB-MSGV2 TO WA_MSGV2,
            IT_MESSTAB-MSGV3 TO WA_MSGV3,
            IT_MESSTAB-MSGV4 TO WA_MSGV4.

    CALL FUNCTION 'TB_MESSAGE_BUILD_TEXT'
         EXPORTING
              LANGU = SY-LANGU
              MSGID = WA_MSGID
              MSGNO = WA_MSGNR
              MSGV1 = WA_MSGV1
              MSGV2 = WA_MSGV2
              MSGV3 = WA_MSGV3
              MSGV4 = WA_MSGV4
         IMPORTING
              TEXT  = WA_TEXT.
    CLEAR:  IT_MESSAGE.
    MOVE : WA_TEXT TO IT_MESSAGE-MESSAGE,
           IT_MESSTAB-MSGTYP TO IT_MESSAGE-TYPE,
           IT_MESSTAB-MSGID  TO IT_MESSAGE-ID,
           IT_MESSTAB-MSGNR  TO IT_MESSAGE-NUMBER.
    APPEND IT_MESSAGE.
  ENDLOOP.
  READ TABLE IT_MESSTAB WITH KEY MSGTYP = 'S'
                                 MSGID  = 'IW'
                                 MSGNR  = '085'.
  WA_SUBRC = SY-SUBRC.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
    PERFORM READ_ORDER_02.
    PERFORM UPDATE_LONG_TEXT.

    WA_ORDERID = IT_MESSTAB-MSGV1.


    READ TABLE IT_COUNTER INDEX 1.
    IF SY-SUBRC NE 0.
      WA_NO_TASK = 'X'.
    ENDIF.

    IF WA_NO_TASK EQ SPACE.
      PERFORM COMPLETE_TASK.
    ENDIF.

*    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*         EXPORTING
*              PERCENTAGE = 100
*              TEXT       = TEXT-M11.
  ENDIF.

ENDFORM.                    " order_change
*&---------------------------------------------------------------------*
*&      Form  order_lock
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CAUFVD  text
*----------------------------------------------------------------------*
FORM ORDER_LOCK USING    P_CAUFVD LIKE CAUFVD.

  DATA LT_ENQUEUE LIKE ORDTYP_PRE OCCURS 0 WITH HEADER LINE.
  DATA LT_NOT_LOCKED LIKE ORD_PRE OCCURS 0 WITH HEADER LINE.

* Move order to enqueue table
  MOVE-CORRESPONDING P_CAUFVD TO LT_ENQUEUE.
  APPEND LT_ENQUEUE.

* Enqueue order(s)
  CALL FUNCTION 'CO_ZF_ORDER_LOCK_MULTI'
       EXPORTING
            LOCK_MODE   = 'S'
       TABLES
            ENQUEUE_TAB = LT_ENQUEUE
            NOT_LOCKED  = LT_NOT_LOCKED.
ENDFORM.                    " order_lock
*&---------------------------------------------------------------------*
*&      Form  PARAMETER_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PARAMETER_SET.
  CLEAR: GS_TCORU.

  DATA L_CORUF-OFFVG LIKE CORUF-OFFVG.
  DATA L_CORUF-RUPFL LIKE CORUF-RUPFL.

*  Get Parameters from table TCORU
  CALL FUNCTION 'CO_RU_TCORU_READ'
       EXPORTING
            AUART_IMP = CAUFVD-AUART
            AUTYP_IMP = CAUFVD-AUTYP
            WERKS_IMP = CAUFVD-WERKS
       IMPORTING
            TCORU_EXP = GS_TCORU
       EXCEPTIONS
            NO_ENTRY  = 1
            OTHERS    = 2.

  IF SY-SUBRC = 0.
* Show every time all components
    GS_TCORU-ACOMP = CON_X.
* Show also confirmed operations
    GS_TCORU-ERUVG = CON_X.
    L_CORUF-OFFVG  = SPACE.

* Customer-Exit
*    CALL CUSTOMER-FUNCTION '002'
*         CHANGING
*              C_TCORU       = GS_TCORU
*              C_CORUF_OFFVG = L_CORUF-OFFVG
*              C_CORUF_RUPFL = L_CORUF-RUPFL.

* Set TCORU parameters in CORU
    CALL FUNCTION 'CO_RU_TCORU_REFRESH'
         EXPORTING
              TCORU_IMP = GS_TCORU.

* Set selection parameters in CORU
    CALL FUNCTION 'CO_RU_SET_SELECTION_PARAM'
         EXPORTING
              OFFVG_IMP = L_CORUF-OFFVG
              RUPFL_IMP = L_CORUF-RUPFL.

* Set selection parameters in CORF
    CALL FUNCTION 'CO_RF_SET_SELECTION_PARAM'
         EXPORTING
              OFFVG_IMP = L_CORUF-OFFVG
              RUPFL_IMP = L_CORUF-RUPFL.

  ENDIF.

ENDFORM.                    " PARAMETER_SET
*&---------------------------------------------------------------------*
*&      Form  CHECK_BUDAT
*&---------------------------------------------------------------------*
*       Check Posting Date
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_BUDAT.
  MKPF-BLDAT  = ZSPM_COMP-IDAT2.
  MKPF-BUDAT  = ZSPM_COMP-IDAT2.

  IF MKPF-BLDAT > SY-DATLO.
    PERFORM NACHRICHTENCODE_ERMITTELN(SAPFM07M) USING 'M7' '088'. "ok
  ENDIF.

  MSEG-GJAHR = SY-DATUM.
  MSEG-BUKRS = 'H201'.   "ZSPM_COMP-BUKRS.

  DATA : MONAT LIKE VM07M-MONAT.
  DATA : GJAHR LIKE MSEG-GJAHR.

  CALL FUNCTION 'FI_PERIOD_DETERMINE'
       EXPORTING
            I_BUKRS = MSEG-BUKRS
            I_BUDAT = MKPF-BLDAT
       IMPORTING
            E_MONAT = MONAT
            E_GJAHR = GJAHR.
  IF NOT MSEG-GJAHR = GJAHR.
    PERFORM NACHRICHTENCODE_ERMITTELN(SAPFM07M) USING 'M7' '089'."ok
  ENDIF.

ENDFORM.                    " CHECK_BUDAT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0110 OUTPUT.
  SUPPRESS DIALOG.
  LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
  SET TITLEBAR '110'.
  SET PF-STATUS SPACE.

  GV_REPID = SY-REPID.

* Preparation of ALV
  PERFORM PRE_REPORT_ADJ.

* Call ALV LIST
  PERFORM CALL_ALV_LIST.


ENDMODULE.                 " STATUS_0110  OUTPUT
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

* Key
  PERFORM BUILD_FIELDCAT USING
    'IT_MESSAGE' 'TYPE'  'X'     SPACE SPACE
     SPACE    '1'     'Type'  SPACE SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_MESSAGE' 'ID'  'X'     SPACE SPACE
     SPACE    '20'     'Message ID'  SPACE SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_MESSAGE' 'NUMBER'  'X'     SPACE SPACE
     SPACE    '3'     'Message Number'  SPACE SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_MESSAGE' 'MESSAGE'  'X'     SPACE SPACE
     SPACE    '100'     'Message'  SPACE SPACE SPACE.

*** Sort
*  SORT IT_MESSAGE BY TYPE ID NUMBER MESSAGE.
  CLEAR: IT_MESSAGE.

  IT_SORT-FIELDNAME = 'TYPE'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.

  IT_SORT-FIELDNAME = 'ID'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.

  IT_SORT-FIELDNAME = 'NUMBER'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.

  IT_SORT-FIELDNAME = 'MESSAGE'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.

* Set Event
  DATA : WA_L_EVENT  TYPE SLIS_ALV_EVENT.
  WA_L_EVENT-NAME = SLIS_EV_TOP_OF_PAGE.
  WA_L_EVENT-FORM = 'BASIC_TOP_OF_PAGE'.
  APPEND WA_L_EVENT TO IT_EVENTS.

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
  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM       = GV_REPID
            I_CALLBACK_PF_STATUS_SET = GV_STATUS
            I_CALLBACK_USER_COMMAND  = GV_USER_COMMAND
            IT_FIELDCAT              = IT_FIELDCAT[]
            IT_SORT                  = IT_SORT[]
            I_SAVE                   = 'A'
            IT_EVENTS                = IT_EVENTS
            IT_EVENT_EXIT            = IT_EVENT_EXIT  "
       TABLES
            T_OUTTAB                 = IT_MESSAGE
       EXCEPTIONS
            PROGRAM_ERROR            = 1
            OTHERS                   = 2.

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
                             VALUE(P_0110).

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

  WRITE  AT 20 'Resault of Order completion confirmation'
         INVERSE COLOR 3.
  SKIP.
ENDFORM.
*-----------------------------------------------------------------------
*    FORM PF_STATUS_VAR
*-----------------------------------------------------------------------
FORM PF_STATUS USING  EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'BALVLIST'  EXCLUDING EXTAB. " OF PROGRAM 'ZAPM08_ANBD'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MODIFY_SCREEN_INPUT.
  LOOP AT SCREEN .
    IF SCREEN-GROUP2 = 'GB2'.

      IF WA_DISP = 'X'.
        SCREEN-INPUT = 0.
      ENDIF.

    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.                    " MODIFY_SCREEN_INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0101_GET_LINES  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_0101_GET_LINES OUTPUT.
  G_TC_0101_LINES = SY-LOOPC.
ENDMODULE.                 " TC_0101_GET_LINES  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0102_GET_LINES  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_0102_GET_LINES OUTPUT.
  G_TC_0102_LINES = SY-LOOPC.
ENDMODULE.                 " TC_0102_GET_LINES  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CHECK_TIME_INTERVAL
*&---------------------------------------------------------------------*
*        Calculate intervals between two times
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_TIME_INTERVAL USING P_STR_DATE
                               P_STR_TIME
                               P_END_DATE
                               P_END_TIME
                         CHANGING P_INTERVAL.

  CALL FUNCTION 'Z_FCA_GET_TIME_INTERVAL'
       EXPORTING
            S_DATE   = P_STR_DATE
            S_TIME   = P_STR_TIME
            E_DATE   = P_END_DATE
            E_TIME   = P_END_TIME
       IMPORTING
            INTERVAL = P_INTERVAL.

ENDFORM.                    " CHECK_TIME_INTERVAL
*&---------------------------------------------------------------------*
*&      Module  CHECK_COUNTER_INFO  INPUT
*&---------------------------------------------------------------------*
*       Check Countermeasure data...
*----------------------------------------------------------------------*
MODULE CHECK_COUNTER_INFO INPUT.
  IF NOT ZSPM_COUNTER-POSNR IS INITIAL.
    IF ZSPM_COUNTER-KTEXT IS INITIAL
       OR ZSPM_COUNTER-IDAT2 IS INITIAL.
      MESSAGE E000(ZMPM) WITH TEXT-M09.
    ENDIF.
  ENDIF.
ENDMODULE.                 " CHECK_COUNTER_INFO  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0090  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0090 OUTPUT.
  CLEAR: WA_NO_TASK.

  SET PF-STATUS '0090'.
  SET TITLEBAR '0090'.
ENDMODULE.                 " STATUS_0090  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0090  INPUT
*&---------------------------------------------------------------------*
*      *** Order check & Read....(Copy from Satandard)
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0090 INPUT.
  CASE SY-UCOMM.
*    WHEN 'BACK'.
*      CLEAR: SY-UCOMM.
*      PERFORM LEAVE_PROGRAM.

    WHEN 'EXEC'.
      CLEAR: SY-UCOMM.

**** Check Order completion confirmation


*      SELECT SINGLE MAX( RMZHL )
*             INTO  WA_RMZHL
*             FROM  AFRU
*             WHERE AUFNR = ZSPM_COMP-AUFNR.
*      IF SY-SUBRC EQ 0.
*        IF NOT WA_RMZHL IS INITIAL.
*          SELECT  SINGLE STOKZ
*                   INTO  WA_STOKZ
*                   FROM  AFRU
*                   WHERE AUFNR = ZSPM_COMP-AUFNR
*                   AND   RMZHL = WA_RMZHL
*                   AND   STOKZ = ' '
*                   AND   STZHL = ' '.
**          IF WA_STOKZ = 'X'.
*          IF SY-SUBRC EQ 0.
*            MESSAGE E000(ZMPM) WITH TEXT-001 ZSPM_COMP-AUFNR TEXT-M12.
*          ENDIF.
**          ENDIF.
*        ENDIF.
*      ENDIF.

      PERFORM READ_ORDER.
      PERFORM RUN_PROGRAM.

    WHEN 'DISP'.
      CLEAR: SY-UCOMM.
      WA_DISP = 'X'.
      PERFORM READ_ORDER.
      PERFORM READ_COUNTER_ORDER.
      PERFORM ORDER_DELOCK.
      CALL SCREEN '0100'.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0090  INPUT
*&---------------------------------------------------------------------*
*&      Form  RUN_PROGRAM
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RUN_PROGRAM.
  CLEAR : IT_LTXTTAB01, IT_LTXTTAB01[],
          IT_LTXTTAB02, IT_LTXTTAB02[],
          IT_LTXTTAB03, IT_LTXTTAB03[],
          IT_LTXTTAB04, IT_LTXTTAB04[].

  CLEAR : IT_COUNTER, IT_COUNTER[].
  CLEAR : WA_SAVE.
  CALL SCREEN '0100'.
ENDFORM.                    " RUN_PROGRAM
*&---------------------------------------------------------------------*
*&      Form  ORDER_DELOCK
*&---------------------------------------------------------------------*
*       Dequeue all table locks...
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ORDER_DELOCK.
  CALL FUNCTION 'DEQUEUE_ALL'.
ENDFORM.                    " ORDER_DELOCK
*&---------------------------------------------------------------------*
*&      Form  CREATE_ORDER_REF_NOTI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_ORDER_REF_NOTI.
  DATA: IT_MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.
  DATA: WA_TEXT(200).
  DATA: WA_MSGNR LIKE SY-MSGNO,
        WA_MSGID LIKE SY-MSGID,
        WA_MSGV1 LIKE SY-MSGV1,
        WA_MSGV2 LIKE SY-MSGV2,
        WA_MSGV3 LIKE SY-MSGV3,
        WA_MSGV4 LIKE SY-MSGV4.

  DATA: WA_ZSPM_COUNTER LIKE ZSPM_COUNTER.

  CLEAR: WA_SUBRC.
  MOVE-CORRESPONDING IT_COUNTER TO WA_ZSPM_COUNTER.

  CALL FUNCTION 'Z_FPM_CREATE_OR_REF_NOTI'
    EXPORTING
     CTU                = 'X'
     MODE               = WA_MODE
     UPDATE             = 'L'
*   GROUP              =
*   USER               =
*   KEEP               =
*   HOLDDATE           =
      NODATA             = ' '
      QMART              = 'M1'
      QWRNUM             = ZSPM_COMP-QMNUM
      AUART              = 'PM01'
      ZSPM_COUNTER       = WA_ZSPM_COUNTER
      NO_TASK            = WA_NO_TASK
   IMPORTING
     SUBRC              = WA_SUBRC
   TABLES
     MESSTAB            = IT_MESSTAB.

  LOOP AT IT_MESSTAB.
    CLEAR: WA_TEXT.
    MOVE:   IT_MESSTAB-MSGNR TO WA_MSGNR,
            IT_MESSTAB-MSGID TO WA_MSGID,
            IT_MESSTAB-MSGV1 TO WA_MSGV1,
            IT_MESSTAB-MSGV2 TO WA_MSGV2,
            IT_MESSTAB-MSGV3 TO WA_MSGV3,
            IT_MESSTAB-MSGV4 TO WA_MSGV4.

    CALL FUNCTION 'TB_MESSAGE_BUILD_TEXT'
         EXPORTING
              LANGU = SY-LANGU
              MSGID = WA_MSGID
              MSGNO = WA_MSGNR
              MSGV1 = WA_MSGV1
              MSGV2 = WA_MSGV2
              MSGV3 = WA_MSGV3
              MSGV4 = WA_MSGV4
         IMPORTING
              TEXT  = WA_TEXT.
    CLEAR:  IT_MESSAGE.
    MOVE : WA_TEXT TO IT_MESSAGE-MESSAGE,
           IT_MESSTAB-MSGTYP TO IT_MESSAGE-TYPE,
           IT_MESSTAB-MSGID  TO IT_MESSAGE-ID,
           IT_MESSTAB-MSGNR  TO IT_MESSAGE-NUMBER.
    APPEND IT_MESSAGE.
  ENDLOOP.
  READ TABLE IT_MESSTAB WITH KEY MSGTYP = 'S'
                                 MSGID  = 'IW'
                                 MSGNR  = '085'.
  WA_SUBRC = SY-SUBRC.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.

    WA_ORDERID = IT_MESSTAB-MSGV1.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
         EXPORTING
              INPUT  = WA_ORDERID
         IMPORTING
              OUTPUT = IT_COUNTER-AUFNR.

    PERFORM UPDATE_NEW_ORDER_LONG_TEXT USING IT_COUNTER-AUFNR
                                             IT_COUNTER-POSNR.

    MODIFY IT_COUNTER INDEX WA_TABIX TRANSPORTING AUFNR.

  ENDIF.
ENDFORM.                    " CREATE_ORDER_REF_NOTI
*&---------------------------------------------------------------------*
*&      Form  READ_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_ORDER.
  CLEAR : IT_TIME_CONF, IT_TIME_CONF[].
  CLEAR : IT_COUNTER,   IT_COUNTER[].

  CLEAR : ZSPM_COMP-QMNAM,
          ZSPM_COMP-IDAUR,
          ZSPM_COMP-EAUSZT,
          ZSPM_COMP-MAUEH.

**** Read Order detail info
**** Copy & Modify Sandard....
  CLEAR:  CMFUD, CAUFVD, CORUF.
  CMFUD-AUFNR = ZSPM_COMP-AUFNR.

  PERFORM ORDER_READ.
**** Copy & Modify Sandard

**** Check order already complete.
  IF WA_DISP EQ ' '.
    READ TABLE IT_TIME_CONF WITH KEY LEKNW = ' '.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZMPM) WITH TEXT-001 ZSPM_COMP-AUFNR TEXT-M12.
    ENDIF.
  ENDIF.

  CLEAR: ZSPM_COMP-QMNUM, ZSPM_COMP-KTEXT,
         ZSPM_COMP-AUSVN, ZSPM_COMP-AUSBS,
         ZSPM_COMP-AUZTV, ZSPM_COMP-AUZTB.

  CLEAR: ZSPM_COMP-POSNR, ZSPM_COMP-FENUM, ZSPM_COMP-OTEIL,
         ZSPM_COMP-FECOD, ZSPM_COMP-FETXT, ZSPM_COMP-TXTCDOT.

  CLEAR: ZSPM_COMP-URNUM, ZSPM_COMP-URCOD,
         ZSPM_COMP-URTXT, ZSPM_COMP-TXTCDUR, ZSPM_COMP-TXTCDFE,
         ZSPM_COMP-TXTCDMA.

  SELECT SINGLE MAX( RMZHL ) INTO WA_RMZHL
       FROM  AFRU
       WHERE AUFNR = ZSPM_COMP-AUFNR
       AND   VORNR = '0010'.

  SELECT SINGLE IDAUR INTO ZSPM_COMP-IDAUR
         FROM  AFRU
         WHERE AUFNR = ZSPM_COMP-AUFNR
         AND   VORNR = '0010'
         AND   RMZHL = WA_RMZHL.

**** Check Notification & Read Description
  SELECT SINGLE A~QMNUM B~KTEXT A~AUSVN
                A~AUSBS
*                A~AUZTV "100565
*                A~AUZTB
         INTO  (ZSPM_COMP-QMNUM, ZSPM_COMP-KTEXT,
                ZSPM_COMP-AUSVN, ZSPM_COMP-AUSBS)
*                ZSPM_COMP-AUZTV, "100565
*                ZSPM_COMP-AUZTB)
                         FROM   VIQMEL AS A
                                INNER JOIN AUFK AS B
                                      ON A~AUFNR = B~AUFNR
                         WHERE  A~AUFNR = ZSPM_COMP-AUFNR.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZMPM) WITH TEXT-001 ZSPM_COMP-AUFNR TEXT-M01.
  ELSE.
    CLEAR: QMUR, QMMA, VIQMFE.

**** Read Object Part Code , Damage & Notification Item Short Text
    SELECT SINGLE  POSNR FENUM OTEIL  FECOD FETXT
                   INTO (ZSPM_COMP-POSNR, ZSPM_COMP-FENUM,
                         ZSPM_COMP-OTEIL,
                         ZSPM_COMP-FECOD, ZSPM_COMP-FETXT)
                   FROM VIQMFE
                   WHERE QMNUM = ZSPM_COMP-QMNUM.
    IF SY-SUBRC EQ 0.
**** Read Object Part Code text
      IF NOT ZSPM_COMP-OTEIL IS INITIAL.
        CALL FUNCTION 'QPK1_CODE_TEXT'
             EXPORTING
                  I_KATALOGART = 'B'
                  I_CODEGRUPPE = 'PM01'
                  I_CODE       = ZSPM_COMP-OTEIL
             IMPORTING
                  E_TEXT       = ZSPM_COMP-TXTCDOT.
      ENDIF.
**** Read Damage Code text
      IF NOT ZSPM_COMP-FECOD IS INITIAL.
        CALL FUNCTION 'QPK1_CODE_TEXT'
             EXPORTING
                  I_KATALOGART = 'C'
                  I_CODEGRUPPE = 'PM01'
                  I_CODE       = ZSPM_COMP-FECOD
             IMPORTING
                  E_TEXT       = ZSPM_COMP-TXTCDFE.
      ENDIF.
    ENDIF.
**** Read Cause Code & Short Text for Cause Code
    SELECT SINGLE URNUM URCOD URTXT
           INTO  (ZSPM_COMP-URNUM,
                  ZSPM_COMP-URCOD,
                  ZSPM_COMP-URTXT)
           FROM   QMUR
           WHERE  QMNUM = ZSPM_COMP-QMNUM.
    IF SY-SUBRC EQ 0.
**** Read Cause Code text
      CALL FUNCTION 'QPK1_CODE_TEXT'
           EXPORTING
                I_KATALOGART = '5'
                I_CODEGRUPPE = 'PM01'
                I_CODE       = ZSPM_COMP-URCOD
           IMPORTING
                E_TEXT       = ZSPM_COMP-TXTCDUR.
    ENDIF.
***** Activity Code & Short Text for Activity Code
    CLEAR: ZSPM_COMP-MANUM,
           ZSPM_COMP-MNCOD, ZSPM_COMP-MATXT.
    SELECT SINGLE MANUM MNCOD MATXT
           INTO  (ZSPM_COMP-MANUM,
                  ZSPM_COMP-MNCOD,
                  ZSPM_COMP-MATXT)
           FROM   QMMA
           WHERE  QMNUM = ZSPM_COMP-QMNUM.
    IF SY-SUBRC EQ 0.
***** Activity Code text
      CALL FUNCTION 'QPK1_CODE_TEXT'
           EXPORTING
                I_KATALOGART = 'A'
                I_CODEGRUPPE = 'PM01'
                I_CODE       = ZSPM_COMP-MNCOD
           IMPORTING
                E_TEXT       = ZSPM_COMP-TXTCDMA.
    ENDIF.
  ENDIF.

  CLEAR: ZSPM_COMP-EAUSZT, ZSPM_COMP-MAUEH.

  CHECK NOT ZSPM_COMP-AUSBS IS INITIAL
        AND   NOT ZSPM_COMP-AUZTB IS INITIAL.

  CLEAR: WA_INTERVAL.
  PERFORM CHECK_TIME_INTERVAL USING  ZSPM_COMP-AUSVN
                                     ZSPM_COMP-AUZTV
                                     ZSPM_COMP-AUSBS
                                     ZSPM_COMP-AUZTB
                           CHANGING  WA_INTERVAL.
  IF WA_INTERVAL < 0.
    MESSAGE E000(ZMPM) WITH TEXT-M07.
  ELSE.
    MOVE : WA_INTERVAL TO ZSPM_COMP-EAUSZT,
           'MIN'       TO ZSPM_COMP-MAUEH.
  ENDIF.


ENDFORM.                    " READ_ORDER
*&---------------------------------------------------------------------*
*&      Form  TIME_CONFIRMATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM TIME_CONFIRMATION USING P_VORNR.
  DATA: IT_MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.
  DATA: WA_TEXT(200).
  DATA: WA_IDAUR LIKE ZSPM_COMP-IDAUR.
  DATA: WA_MSGNR LIKE SY-MSGNO,
        WA_MSGID LIKE SY-MSGID,
        WA_MSGV1 LIKE SY-MSGV1,
        WA_MSGV2 LIKE SY-MSGV2,
        WA_MSGV3 LIKE SY-MSGV3,
        WA_MSGV4 LIKE SY-MSGV4.


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            INPUT  = WA_VORNR
       IMPORTING
            OUTPUT = WA_VORNR.

  IF WA_VORNR = '0010'.
    WA_IDAUR = ZSPM_COMP-IDAUR.
  ELSE.
    CLEAR: WA_IDAUR.
  ENDIF.
  CALL FUNCTION 'Z_FPM_TIME_CONFIRMATION2'
       EXPORTING
            CTU            = 'X'
            MODE           = WA_MODE
            UPDATE         = 'L'
            AUFNR          = ZSPM_COMP-AUFNR
            VORNR          = P_VORNR
*            BUDAT          = ZSPM_COMP-IDAT2
            IDAUR          = WA_IDAUR
            IDAUE          = ZSPM_COMP-IDAUE
            ZSPM_TIME_CONF = IT_TIME_CONF
       IMPORTING
            SUBRC          = WA_SUBRC
       TABLES
            MESSTAB        = IT_MESSTAB.

  LOOP AT IT_MESSTAB.
    CLEAR: WA_TEXT.
    MOVE:   IT_MESSTAB-MSGNR TO WA_MSGNR,
            IT_MESSTAB-MSGID TO WA_MSGID,
            IT_MESSTAB-MSGV1 TO WA_MSGV1,
            IT_MESSTAB-MSGV2 TO WA_MSGV2,
            IT_MESSTAB-MSGV3 TO WA_MSGV3,
            IT_MESSTAB-MSGV4 TO WA_MSGV4.

    CALL FUNCTION 'TB_MESSAGE_BUILD_TEXT'
         EXPORTING
              LANGU = SY-LANGU
              MSGID = WA_MSGID
              MSGNO = WA_MSGNR
              MSGV1 = WA_MSGV1
              MSGV2 = WA_MSGV2
              MSGV3 = WA_MSGV3
              MSGV4 = WA_MSGV4
         IMPORTING
              TEXT  = WA_TEXT.
    CLEAR:  IT_MESSAGE.
    MOVE : WA_TEXT TO IT_MESSAGE-MESSAGE,
           IT_MESSTAB-MSGTYP TO IT_MESSAGE-TYPE,
           IT_MESSTAB-MSGID  TO IT_MESSAGE-ID,
           IT_MESSTAB-MSGNR  TO IT_MESSAGE-NUMBER.
    APPEND IT_MESSAGE.
  ENDLOOP.

  READ TABLE IT_MESSTAB WITH KEY               "MSGTYP = 'S'
                                 MSGID  = 'RU'
                                 MSGNR  = '163'.
  WA_SUBRC = SY-SUBRC.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
  ENDIF.

ENDFORM.                    " TIME_CONFIRMATION
*&---------------------------------------------------------------------*
*&      Form  SAVE_LONG_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_HEADLTX  text
*----------------------------------------------------------------------*
FORM SAVE_LONG_TEXT USING  P_HEADLTX LIKE THEAD.
  DATA: TXFUNCTION(1) TYPE C.

  DATA: BEGIN OF REFHEAD.                "Struktur des Referenzkopfes
          INCLUDE STRUCTURE THEAD.            "Kopf
  DATA: END OF REFHEAD.

  CALL FUNCTION 'SAVE_TEXT'
       EXPORTING
            HEADER    = P_HEADLTX
            INSERT    = SPACE
       IMPORTING
            FUNCTION  = TXFUNCTION
            NEWHEADER = REFHEAD
       TABLES
            LINES     = LTXTTAB.


  CALL FUNCTION 'COMMIT_TEXT'
       EXPORTING
            OBJECT          = P_HEADLTX-TDOBJECT
            SAVEMODE_DIRECT = 'X'.

ENDFORM.                    " SAVE_LONG_TEXT
*&---------------------------------------------------------------------*
*&      Form  update_long_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_LONG_TEXT.
*** Long Text for Object Part Code & Problem Code
  CLEAR: WA_TDNAME01, HEADLTX.
  CLEAR: LTXTTAB, LTXTTAB[].

  CONCATENATE ZSPM_COMP-QMNUM
              ZSPM_COMP-FENUM
*              ZSPM_COMP-POSNR
              INTO WA_TDNAME01.

  HEADLTX-TDID    = 'LTXT'.
  HEADLTX-TDSPRAS = 'E'.
  HEADLTX-TDNAME  = WA_TDNAME01.
  HEADLTX-TDOBJECT = 'QMFE'.
  HEADLTX-TDLINESIZE = 072.

  LTXTTAB[] = IT_LTXTTAB01[].

  PERFORM SAVE_LONG_TEXT USING HEADLTX.

*** Long Text for Cause Code
  CLEAR: WA_TDNAME02, HEADLTX.
  CLEAR: LTXTTAB, LTXTTAB[].

  CONCATENATE ZSPM_COMP-QMNUM
              ZSPM_COMP-FENUM
              '0001'
*              ZSPM_COMP-URNUM
              INTO WA_TDNAME02.

  HEADLTX-TDID    = 'LTXT'.
  HEADLTX-TDSPRAS = 'E'.
  HEADLTX-TDNAME  = WA_TDNAME02.
  HEADLTX-TDOBJECT = 'QMUR'.
  HEADLTX-TDLINESIZE = 072.
  HEADLTX-TDFORM = 'PM_QMEL'.

  LTXTTAB[] = IT_LTXTTAB02[].

  PERFORM SAVE_LONG_TEXT USING HEADLTX.

**** Long  Text for Activity Code
  CLEAR: WA_TDNAME03, HEADLTX.
  CLEAR: LTXTTAB, LTXTTAB[].

  CONCATENATE ZSPM_COMP-QMNUM
*              ZSPM_COMP-MANUM
               '0001'
              INTO WA_TDNAME03.

  HEADLTX-TDID    = 'LTXT'.
  HEADLTX-TDSPRAS = 'E'.
  HEADLTX-TDNAME  = WA_TDNAME03.
  HEADLTX-TDOBJECT = 'QMMA'.
  HEADLTX-TDLINESIZE = 072.

  LTXTTAB[] = IT_LTXTTAB03[].

  PERFORM SAVE_LONG_TEXT USING HEADLTX.


  LOOP AT IT_COUNTER.
**** Long  Text for Task list (Old Order)
    CLEAR: WA_TDNAME04, HEADLTX.
    CLEAR: LTXTTAB, LTXTTAB[].

    MOVE IT_COUNTER-POSNR TO WA_FENUM .

    CONCATENATE ZSPM_COMP-QMNUM
                WA_FENUM
*                ZSPM_COMP-MANUM
                INTO WA_TDNAME04.

    HEADLTX-TDID       = 'LTXT'.
    HEADLTX-TDSPRAS    = 'E'.
    HEADLTX-TDNAME     = WA_TDNAME04.
    HEADLTX-TDOBJECT   = 'QMSM'.
    HEADLTX-TDLINESIZE = 072.

    LOOP AT IT_LTXTTAB04 WHERE WA_TDNAME = WA_TDNAME04.
      MOVE-CORRESPONDING IT_LTXTTAB04 TO LTXTTAB.
      APPEND LTXTTAB.
    ENDLOOP.

    PERFORM SAVE_LONG_TEXT USING HEADLTX.

  ENDLOOP.

ENDFORM.                    " update_long_text
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM USER_COMMAND USING R_UCOMM
                        RS_SELFIELD.
  CASE R_UCOMM.
    WHEN 'F03' OR  'F15' OR 'F12'.
      CLEAR: R_UCOMM.
      LEAVE TO SCREEN 0.
*      CALL SCREEN '0090'.
  ENDCASE.
ENDFORM.                    " USER_COMMAND
*&---------------------------------------------------------------------*
*&      Module  OK_CODE_TXT04  INPUT
*&---------------------------------------------------------------------*
*       countermeasure long text...
*----------------------------------------------------------------------*
MODULE OK_CODE_TXT04 INPUT.
  CASE SY-UCOMM.
    WHEN 'TXT04'.
      CLEAR: SY-UCOMM.
      CLEAR: WA_TDNAME04, HEADLTX, WA_INIT_TXT, WA_TABIX.
      CLEAR: LTXTTAB, LTXTTAB[].

      GET CURSOR LINE WA_LINES.
      WA_FENUM = WA_LINES + TC_0102-TOP_LINE - 1.

*** Make Long text Header...

      CONCATENATE ZSPM_COMP-QMNUM
*                  ZSPM_COMP-MANUM
                  WA_FENUM
                  INTO WA_TDNAME04.

      HEADLTX-TDID       = 'LTXT'.
      HEADLTX-TDSPRAS    = 'E'.
      HEADLTX-TDNAME     = WA_TDNAME04.
      HEADLTX-TDOBJECT   = 'QMSM'.
      HEADLTX-TDLINESIZE = 072.

      READ TABLE IT_LTXTTAB04 WITH KEY WA_TDNAME = WA_TDNAME04.
      IF SY-SUBRC NE 0.
        WA_INIT_TXT = 'X'.
      ELSE.
        LOOP AT IT_LTXTTAB04 WHERE WA_TDNAME = WA_TDNAME04.
          WA_TABIX = SY-TABIX.

          MOVE-CORRESPONDING IT_LTXTTAB04 TO LTXTTAB.
          APPEND LTXTTAB.
          DELETE IT_LTXTTAB04 INDEX WA_TABIX .
          CLEAR: WA_TABIX .
        ENDLOOP.
      ENDIF.
*** Call editor...
      PERFORM CALL_EDITOR USING HEADLTX
                                WA_INIT_TXT.

      LOOP AT LTXTTAB.
        MOVE-CORRESPONDING LTXTTAB TO IT_LTXTTAB04.
        MOVE WA_TDNAME04 TO IT_LTXTTAB04-WA_TDNAME.
        APPEND IT_LTXTTAB04.
      ENDLOOP.
  ENDCASE.
ENDMODULE.                 " OK_CODE_TXT04  INPUT
*&---------------------------------------------------------------------*
*&      Form  UPDATE_NEW_ORDER_LONG_TEXT
*&---------------------------------------------------------------------*
*       Save Long text for new Order's operations...
*----------------------------------------------------------------------*
*      -->P_IT_COUNTER_AUFNR  text
*----------------------------------------------------------------------*
FORM UPDATE_NEW_ORDER_LONG_TEXT USING  P_AUFNR
                                       P_POSNR.
  DATA: WA_AUFPL LIKE AFVC-AUFPL,
        WA_APLZL LIKE AFVC-APLZL.

  CLEAR: WA_TDNAME04, HEADLTX.
  CLEAR: LTXTTAB, LTXTTAB[].
*
*
*
***** Long  Text for Operation (New, Order)
***** New order operation long text key
*  SELECT   A~AUFPL B~APLZL
*           INTO (WA_AUFPL, WA_APLZL)
*           FROM CAUFV  AS A
*                 INNER JOIN AFVC AS B
*                 ON A~AUFPL = B~AUFPL
*           WHERE  A~AUFNR =  P_AUFNR.
*
*    CALL FUNCTION 'CO_ZK_TEXTKEY_AFVG'
*         EXPORTING
*              APLZL = WA_APLZL
*              AUFPL = WA_AUFPL
*         IMPORTING
*              LTSCH = WA_TDNAME05.
*
**** Old order long text key for selecting internal table IT_LTXTTAB04
  MOVE P_POSNR TO WA_FENUM .
  CONCATENATE ZSPM_COMP-QMNUM
              WA_FENUM
              INTO WA_TDNAME04.
*
*    HEADLTX-TDID       = 'AVOT'.
*    HEADLTX-TDSPRAS    = 'E'.
*    HEADLTX-TDNAME     = WA_TDNAME05.
*    HEADLTX-TDOBJECT   = 'AUFK'.
*    HEADLTX-MANDT      = SY-MANDT.
*    HEADLTX-TDLINESIZE = 072.
*    HEADLTX-TDMACODE1  = 'IW22SAPLIQS0'.
*    HEADLTX-TDFORM     = 'SYSTEM'.


  LOOP AT IT_LTXTTAB04 WHERE WA_TDNAME = WA_TDNAME04.
    MOVE-CORRESPONDING IT_LTXTTAB04 TO LTXTTAB.
    APPEND LTXTTAB.
  ENDLOOP.

*    PERFORM SAVE_LONG_TEXT USING HEADLTX.

  CALL FUNCTION 'Z_FPM_CHAGE_LONGTEXT'
    EXPORTING
     CTU            = 'X'
     MODE           = 'N'
     UPDATE         = 'L'
*       GROUP          =
*       USER           =
*       KEEP           =
*       HOLDDATE       =
       NODATA        = ' '
      AUFNR          = P_AUFNR
   IMPORTING
     SUBRC          = WA_SUBRC
   TABLES
*       MESSTAB        =
     T_TLINE        = LTXTTAB.



*ENDSELECT.
ENDFORM.                    " UPDATE_NEW_ORDER_LONG_TEXT
*&---------------------------------------------------------------------*
*&      Form  COMPLETE_TASK
*&---------------------------------------------------------------------*
*      Complete all tasks of notificatin
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM COMPLETE_TASK.
  DATA: IT_MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.
  DATA: WA_TEXT(200).
  DATA: WA_MSGNR LIKE SY-MSGNO,
        WA_MSGID LIKE SY-MSGID,
        WA_MSGV1 LIKE SY-MSGV1,
        WA_MSGV2 LIKE SY-MSGV2,
        WA_MSGV3 LIKE SY-MSGV3,
        WA_MSGV4 LIKE SY-MSGV4.

  CLEAR: WA_SUBRC.

  CALL FUNCTION 'Z_FPM_COMPLETE_TASK'
       EXPORTING
            CTU     = 'X'
            MODE    = WA_MODE
            UPDATE  = 'L'
            QMNUM   = ZSPM_COMP-QMNUM
       IMPORTING
            SUBRC   = WA_SUBRC
       TABLES
            MESSTAB = IT_MESSTAB.

  LOOP AT IT_MESSTAB.
    CLEAR: WA_TEXT.
    MOVE:   IT_MESSTAB-MSGNR TO WA_MSGNR,
            IT_MESSTAB-MSGID TO WA_MSGID,
            IT_MESSTAB-MSGV1 TO WA_MSGV1,
            IT_MESSTAB-MSGV2 TO WA_MSGV2,
            IT_MESSTAB-MSGV3 TO WA_MSGV3,
            IT_MESSTAB-MSGV4 TO WA_MSGV4.

    CALL FUNCTION 'TB_MESSAGE_BUILD_TEXT'
         EXPORTING
              LANGU = SY-LANGU
              MSGID = WA_MSGID
              MSGNO = WA_MSGNR
              MSGV1 = WA_MSGV1
              MSGV2 = WA_MSGV2
              MSGV3 = WA_MSGV3
              MSGV4 = WA_MSGV4
         IMPORTING
              TEXT  = WA_TEXT.
    CLEAR:  IT_MESSAGE.
    MOVE : WA_TEXT TO IT_MESSAGE-MESSAGE,
           IT_MESSTAB-MSGTYP TO IT_MESSAGE-TYPE,
           IT_MESSTAB-MSGID  TO IT_MESSAGE-ID,
           IT_MESSTAB-MSGNR  TO IT_MESSAGE-NUMBER.
    APPEND IT_MESSAGE.
  ENDLOOP.

  READ TABLE IT_MESSTAB WITH KEY MSGTYP = 'S'
                                 MSGID  = 'IM'
                                 MSGNR  = '405'.
  WA_SUBRC = SY-SUBRC.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
  ENDIF.
ENDFORM.                    " COMPLETE_TASK
*&---------------------------------------------------------------------*
*&      Form  READ_ORDER_02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_ORDER_02.

  CLEAR: QMUR, QMMA, VIQMFE.
  CLEAR:  ZSPM_COMP-OTEIL, ZSPM_COMP-FECOD, ZSPM_COMP-FETXT.
**** Read Object Part Code , Damage & Notification Item Short Text
  SELECT SINGLE  FENUM OTEIL  FECOD FETXT
                 INTO (ZSPM_COMP-FENUM, ZSPM_COMP-OTEIL,
                       ZSPM_COMP-FECOD, ZSPM_COMP-FETXT)
                 FROM  VIQMFE
                 WHERE QMNUM = ZSPM_COMP-QMNUM
                 AND   FENUM = 1.

**** Read Cause Code & Short Text for Cause Code
  CLEAR: ZSPM_COMP-URNUM, ZSPM_COMP-URCOD, ZSPM_COMP-URTXT.
  SELECT SINGLE URNUM URCOD URTXT
         INTO  (ZSPM_COMP-URNUM,
                ZSPM_COMP-URCOD,
                ZSPM_COMP-URTXT)
         FROM   QMUR
         WHERE  QMNUM = ZSPM_COMP-QMNUM
         AND    FENUM = 1.

***** Activity Code & Short Text for Activity Code
  CLEAR: ZSPM_COMP-MANUM, ZSPM_COMP-MNCOD, ZSPM_COMP-MATXT.
  SELECT SINGLE MANUM MNCOD MATXT
         INTO  (ZSPM_COMP-MANUM,
                ZSPM_COMP-MNCOD,
                ZSPM_COMP-MATXT)
         FROM   QMMA
         WHERE  QMNUM = ZSPM_COMP-QMNUM
         AND    MANUM = 1.

ENDFORM.                    " READ_ORDER_02
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_SCREEN_0100 OUTPUT.
  PERFORM MODIFY_SCREEN_INPUT.
ENDMODULE.                 " MODIFY_SCREEN_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  READ_COUNTER_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_COUNTER_ORDER.
  CLEAR: IT_COUNTER, IT_COUNTER[].

  SELECT  A~AUFNR A~QMTXT AS KTEXT B~GSTRP AS IDAT2
          INTO CORRESPONDING FIELDS OF TABLE  IT_COUNTER
          FROM VIQMEL AS A
               INNER JOIN AFKO AS B
               ON A~AUFNR = B~AUFNR
          WHERE A~QWRNUM = ZSPM_COMP-QMNUM.


ENDFORM.                    " READ_COUNTER_ORDER
*&---------------------------------------------------------------------*
*&      Form  CHECK_OPERATION_FINAL_CONF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_OPERATION_FINAL_CONF.

ENDFORM.                    " CHECK_OPERATION_FINAL_CONF
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN_INPUT_TABLE_0101
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MODIFY_SCREEN_INPUT_TABLE_0101.
  LOOP AT SCREEN .
    IF SCREEN-GROUP2 = 'GB2'.

      IF WA_DISP = 'X'.
        SCREEN-INPUT = 0.
      ENDIF.

*** Indicator: No Remaining Work Expected
      IF IT_TIME_CONF-LEKNW = 'X'.
        SCREEN-INPUT = 0.
      ENDIF.

    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.                    " MODIFY_SCREEN_INPUT_TABLE_0101

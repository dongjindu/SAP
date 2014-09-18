************************************************************************
* Program Name      : ZRPP304R_MAT_NOTIFICATION
* Author            : Bongsoo, Kim
* Creation Date     : 2003.09.26.
* Specifications By : Bongsoo, Kim
* Pattern           : 1.1
* Development Request No : UD1K902302
* Addl Documentation:
* Description       : Material Master Notification
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZRPP304R_MAT_NOTIFICATION
                NO STANDARD PAGE HEADING
                LINE-SIZE  139
                LINE-COUNT 65
                MESSAGE-ID ZMBM.
*----------------------------------------------------------------------*
*  EXTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
TABLES : ZTBM_ABXMMRDT,
         ZTBM_AMMR,
         MAKT.

*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_AMMR OCCURS 0,
        MTNO  TYPE ZTBM_AMMR-MTNO,
        PLNT  TYPE ZTBM_AMMR-PLNT,
        ZDATE TYPE ZTBM_AMMR-ZDATE,
        NMTY  TYPE ZTBM_AMMR-NMTY,
        OMTY  TYPE ZTBM_AMMR-OMTY,
        NPTY  TYPE ZTBM_AMMR-NPTY,
        OPTY  TYPE ZTBM_AMMR-OPTY,
        ZDESC TYPE ZTBM_AMMR-ZDESC,
        NMRP  TYPE ZTBM_AMMR-NMRP,
        OMRP  TYPE ZTBM_AMMR-OMRP,
      END OF IT_AMMR.
*----------------------------------------------------------------------*
*  CONSTANTS DECLARATION
*----------------------------------------------------------------------*
CONSTANTS: C_MARK    VALUE  'X',
           C_GUBB    VALUE  '*',
           C_MITU    VALUE  'M'.
DATA P_POS(02) TYPE N.
*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
SELECT-OPTIONS: S_ZDATE FOR ZTBM_AMMR-ZDATE OBLIGATORY
                                            NO-EXTENSION,
                S_MTNO  FOR ZTBM_AMMR-MTNO NO-EXTENSION,
                S_PLNT  FOR ZTBM_AMMR-PLNT NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK B1.
************************************************************************
* INITIALIZATION
************************************************************************
INITIALIZATION.
  PERFORM INITIALIZATION.
************************************************************************
* AT SELECTION-SCREEN
************************************************************************
AT SELECTION-SCREEN OUTPUT.
*  PERFORM SCREEN_MODIFY.
************************************************************************
* AT SELECTION-SCREEN
************************************************************************
AT SELECTION-SCREEN.
  PERFORM AT_SELECTION-SCREEN.
************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
  PERFORM EXECUTE_PROCESS.
  PERFORM WRITE_PROCESS.
************************************************************************
* END-OF-SELECTION
************************************************************************
END-OF-SELECTION.
************************************************************************
* TOP-OF-PAGE
************************************************************************
TOP-OF-PAGE.
  PERFORM TOP_OF_PAGE.
************************************************************************
* END-OF-PAGE
************************************************************************
END-OF-PAGE.
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION.
  CONCATENATE SY-DATUM(06) '01' INTO S_ZDATE-LOW.
  S_ZDATE-HIGH = SY-DATUM.
  S_ZDATE-SIGN = 'I'.
  S_ZDATE-OPTION = 'BT'.
  APPEND S_ZDATE.
ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  EXECUTE_PROCESS
*&---------------------------------------------------------------------*
FORM EXECUTE_PROCESS.

  REFRESH IT_AMMR. CLEAR IT_AMMR.
* DATA SELECTION
  SELECT MTNO  "TYPE ZTBM_AMMR-MTNO,
         PLNT  "TYPE ZTBM_AMMR-PLNT,
         ZDATE "TYPE ZTBM_AMMR-ZDATE,
         NMTY  "TYPE ZTBM_AMMR-NMTY,
         OMTY  "TYPE ZTBM_AMMR-OMTY,
         NPTY  "TYPE ZTBM_AMMR-NPTY,
         OPTY  "TYPE ZTBM_AMMR-OPTY,
         ZDESC "TYPE ZTBM_AMMR-ZDESC,
         NMRP  "TYPE ZTBM_AMMR-NMRP,
         OMRP  "TYPE ZTBM_AMMR-OMRP,
       FROM  ZTBM_AMMR
       INTO TABLE IT_AMMR
       WHERE  MTNO   IN S_MTNO
       AND    PLNT   IN S_PLNT
       AND    ZDATE  IN S_ZDATE.
  IF SY-SUBRC EQ 0.
    SORT IT_AMMR BY MTNO PLNT ZDATE.
  ENDIF.


ENDFORM.                    " EXECUTE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  AT_SELECTION-SCREEN
*&---------------------------------------------------------------------*
FORM AT_SELECTION-SCREEN.

ENDFORM.                    " AT_SELECTION-SCREEN
*&---------------------------------------------------------------------*
*&      Form  WRITE_PROCESS
*&---------------------------------------------------------------------*
FORM WRITE_PROCESS.
  DATA: BEGIN OF LT_AMMR OCCURS 0,
          PLNT TYPE ZTBM_AMMR-PLNT,
          MTNO TYPE ZTBM_AMMR-MTNO,
          ZDESC TYPE ZTBM_AMMR-ZDESC,
          NMTY TYPE ZTBM_AMMR-NMTY,
          OMTY TYPE ZTBM_AMMR-OMTY,
          NPTY TYPE ZTBM_AMMR-NPTY,
          OPTY TYPE ZTBM_AMMR-OPTY,
          NMRP TYPE ZTBM_AMMR-NMRP,
          OMRP TYPE ZTBM_AMMR-OMRP,
          ZDATE TYPE ZTBM_AMMR-ZDATE,
        END OF LT_AMMR.
  DATA: L_ZDESC(40).
  LOOP AT IT_AMMR.
    MOVE-CORRESPONDING IT_AMMR TO LT_AMMR.
    APPEND LT_AMMR.
    CLEAR: IT_AMMR, LT_AMMR.
  ENDLOOP.
  SORT LT_AMMR BY PLNT MTNO ZDESC ZDATE.
  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  LOOP AT LT_AMMR.
    L_ZDESC = LT_AMMR-ZDESC.
    WRITE AT:   "003(04) LT_AMMR-PLNT NO-GAP,
               /008(01) SY-VLINE NO-GAP,
                 "009(18) LT_AMMR-MTNO NO-GAP,
               027(01) SY-VLINE NO-GAP,
*               028(40) LT_AMMR-ZDESC ,
               068(01) SY-VLINE NO-GAP,
               069(08) LT_AMMR-NMTY  CENTERED,
               077(08) LT_AMMR-OMTY  CENTERED,
               085(10) LT_AMMR-NPTY  CENTERED,
               095(10) LT_AMMR-OPTY  CENTERED,
               105(12) LT_AMMR-NMRP  CENTERED,
               117(12) LT_AMMR-OMRP  CENTERED,
               129(10) LT_AMMR-ZDATE.
    AT NEW PLNT.
      WRITE AT:  003(04) LT_AMMR-PLNT NO-GAP.
    ENDAT.
    AT NEW MTNO.
      WRITE AT:  009(18) LT_AMMR-MTNO NO-GAP,
                 028(40) L_ZDESC.
    ENDAT.
  ENDLOOP.
  FORMAT COLOR OFF.
ENDFORM.                    " WRITE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM TOP_OF_PAGE.
  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: /03(136) TEXT-001 CENTERED.
  WRITE : /03(20) TEXT-002,
           (18) S_ZDATE-LOW , (02) TEXT-005, (18) S_ZDATE-HIGH,
          /03(20) TEXT-003,
           (18) S_MTNO-LOW , (02) TEXT-005, (18) S_MTNO-HIGH,
          /03(20) TEXT-004,
           (18) S_PLNT-LOW , (02) TEXT-005, (18) S_PLNT-HIGH.
  FORMAT COLOR OFF.
  WRITE AT: /03(136) SY-ULINE.
  FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
  WRITE AT: /003(04)  TEXT-006  NO-GAP,
             009(18)  TEXT-007  NO-GAP,
             028(40)  TEXT-008 ,
             069(08)  TEXT-009 ,
             077(08)  TEXT-010 ,
             085(10)  TEXT-011 ,
             095(10)  TEXT-012  ,
             105(12)  TEXT-013 ,
             117(12)  TEXT-014  ,
             129(10)  TEXT-015 .
  FORMAT COLOR OFF.
  WRITE AT: /03(136) SY-ULINE.
ENDFORM.                    " TOP_OF_PAGE

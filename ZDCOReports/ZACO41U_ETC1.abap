************************************************************************
* Program Name      : ZACO41U_ETC1
* Author            : Hyung Jin Youn
* Creation Date     : 29/10/2003
* Specifications By : Bong-Doo Moon
* Pattern           : Report 1-1
* Development Request No : UD1K903453
* Addl Documentation:
* Description       : Copy the last Production version to '0'
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZACO41U_ETC1 MESSAGE-ID ZMCO.


*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
** Tables
TABLES : MKAL, MKAL_AEND, MKAL_EXPAND, *MKAL_AEND, MARA.

** Internal Table
DATA : IT_MKAL LIKE STANDARD TABLE OF MKAL
               WITH HEADER LINE .

** Work area
DATA : WA_MKAL_EXPAND LIKE MKAL_EXPAND.
DATA : WA_LOG_EXPAND LIKE MKAL_AEND.

** Variable
* For Log
DATA : GV_TOTAL  TYPE I.
DATA : GV_INSERT TYPE I.
DATA : GV_UPDATE TYPE I.
DATA : GV_NOCHG  TYPE I.


*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.


*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
  SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-001.

  PARAMETERS : P_CONF AS CHECKBOX.
  SELECT-OPTIONS : S_MATNR FOR MKAL-MATNR.

**// Mod. By hyung Jin Youn 2004.01.15
**   Only for Material Type 'FERT'
**   and for plant 'P001'
**   Block Input for those fields
*                   S_WERKS FOR MKAL-WERKS.
  PARAMETERS :     P_WERKS LIKE MKAL-WERKS DEFAULT 'P001'
                                           MODIF ID NIN
                                           OBLIGATORY.
  PARAMETERS :     P_MTART LIKE MARA-MTART DEFAULT 'FERT'
                                           MODIF ID NIN
                                           OBLIGATORY.
**// End. of Mod.

  SELECTION-SCREEN END OF BLOCK BL1.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

**// Mod. By hyung Jin Youn 2004.01.15
**   Only for Material Type 'FERT'
**   and for plant 'P001'
**   Block Input for those fields
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CHECK SCREEN-GROUP1 = 'NIN'.
    SCREEN-INPUT = '0'.
    MODIFY SCREEN.
  ENDLOOP.
**// End. of Mod.


*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Check Conf .
  IF P_CONF <> 'X'.
    MESSAGE E056.
  ENDIF.
* Enqueue
  PERFORM ENQUEUE_EMMKALE.
* Read table from MKAL.
  PERFORM READ_DATA_FR_MKAL.
* Check if Pro. Ver. is changeable. and Copy Verion
  PERFORM PRD_VR_CHANGEABLE.
* Dequeue.
  PERFORM DEQUEUE_EMMKALE.


*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Write Log
  PERFORM WRITE_LOG.


*----------------------------------------------------------------------*
* Sub-Routine
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  READ_DATA_FR_MKAL
*&---------------------------------------------------------------------*
*       Read Data From MKAL
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA_FR_MKAL.
* All data with the hightest version should be selected
* No where condition except version
* Index Table Key
  CLEAR : IT_MKAL, IT_MKAL[].

**// Mod. By hyung Jin Youn 2004.01.15
**   Only for Material Type 'FERT'
**   and for plant 'P001'
**   Block Input for those fields
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_MKAL
           FROM MKAL AS A
           WHERE MATNR IN S_MATNR
             AND WERKS =  P_WERKS
             AND VERID = ( SELECT MAX( VERID )
                             FROM MKAL
                            WHERE MATNR = A~MATNR
                              AND WERKS = A~WERKS )
             AND EXISTS
                         ( SELECT *
                             FROM MARA
                            WHERE MATNR = A~MATNR
                              AND MTART = P_MTART ).
**// End of Mod.

  SORT IT_MKAL BY MATNR.
  CLEAR : IT_MKAL.

  IF IT_MKAL[] IS INITIAL.
    MESSAGE E026.
  ENDIF.

ENDFORM.                    " READ_DATA_FR_MKAL

*&---------------------------------------------------------------------*
*&      Form  ENQUEUE_EMMKALE
*&---------------------------------------------------------------------*
*       Enqueue / mandt level.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ENQUEUE_EMMKALE.
  CALL FUNCTION 'ENQUEUE_EMMKALE'
    EXPORTING
      MODE_MKAL            = 'E'
      MANDT                = SY-MANDT
*     MATNR                =
*     WERKS                =
*     VERID                =
*     X_MATNR              = ' '
*     X_WERKS              = ' '
*     X_VERID              = ' '
      _SCOPE               = '2'
*     _WAIT                = ' '
*     _COLLECT             = ' '
    EXCEPTIONS
      FOREIGN_LOCK         = 1
      SYSTEM_FAILURE       = 2
      OTHERS               = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " ENQUEUE_EMMKALE

*&---------------------------------------------------------------------*
*&      Form  PRD_VR_CHANGEABLE
*&---------------------------------------------------------------------*
*       Check production version changeable and Copy Version
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRD_VR_CHANGEABLE.

  DATA : WA_L_IS_MKAL_Y LIKE MKAL_EXPAND.
  DATA : WA_L_ES_MKAL_Y LIKE MKAL_EXPAND.
  DATA : WA_L_ES_MKAL_HIST LIKE MKAL.
  DATA : LV_E_STATE TYPE  C.
  DATA : L_COUNT TYPE I.

** For Log
*DATA : GV_TOTAL  TYPE I.
*DATA : GV_INSERT TYPE I.
*DATA : GV_UPDATE TYPE I.
*DATA : GV_NOCHG  TYPE I.

  CLEAR : GV_TOTAL, GV_INSERT, GV_UPDATE, GV_NOCHG.

  LOOP AT IT_MKAL.

    CLEAR : WA_L_IS_MKAL_Y,    WA_L_ES_MKAL_Y,
            WA_L_ES_MKAL_HIST, LV_E_STATE.

* Version Change
* Copy the highest version to '0' version neither '00' nor '000'.
    IT_MKAL-VERID = '0'.
* modify
    MODIFY IT_MKAL.
* Copy data to Export Work Area
    MOVE-CORRESPONDING  IT_MKAL TO   WA_L_IS_MKAL_Y.
* Read Status
    CALL FUNCTION 'CM_FV_PROD_VERS_CHANGEABLE'
         EXPORTING
              IS_MKAL_Y      = WA_L_IS_MKAL_Y
         IMPORTING
              ES_MKAL_Y      = WA_L_ES_MKAL_Y
              ES_MKAL_HIST   = WA_L_ES_MKAL_HIST
              E_STATE        = LV_E_STATE
         EXCEPTIONS
              FOREIGN_LOCK   = 1
              SYSTEM_FAILURE = 2
              OTHERS         = 3.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

* Transfer data
    CLEAR WA_LOG_EXPAND.
    CLEAR WA_MKAL_EXPAND.
    MOVE-CORRESPONDING  WA_L_IS_MKAL_Y TO WA_MKAL_EXPAND.

    CASE LV_E_STATE.
      WHEN 'D'. " No data Found in DB -> Insert
* Log data -> insert
        MOVE-CORRESPONDING  WA_L_ES_MKAL_Y TO WA_LOG_EXPAND.
        WA_LOG_EXPAND-ANDAT = SY-DATUM.
        WA_LOG_EXPAND-ANNAM = SY-UNAME.
        WA_LOG_EXPAND-VBKZ  = 'I'.
*  insert count
        GV_INSERT = GV_INSERT + 1.
      WHEN 'C'. " Chnaged
* Log data -> update
        MOVE-CORRESPONDING  WA_L_IS_MKAL_Y TO WA_LOG_EXPAND.
        WA_LOG_EXPAND-AEDAT = SY-DATUM.
        WA_LOG_EXPAND-AENAM = SY-UNAME.
        WA_LOG_EXPAND-VBKZ  = 'U'.
*  update count
        GV_UPDATE = GV_UPDATE + 1.
* Read Creation Information
        CLEAR *MKAL_AEND.
        SELECT SINGLE * FROM *MKAL_AEND
                WHERE MATNR = WA_LOG_EXPAND-MATNR
                  AND WERKS = WA_LOG_EXPAND-WERKS
                  AND VERID = WA_LOG_EXPAND-VERID
                  AND VBKZ  = 'I'.
        WA_LOG_EXPAND-ANDAT = *MKAL_AEND-ANDAT.
        WA_LOG_EXPAND-ANNAM = *MKAL_AEND-ANNAM.
      WHEN OTHERS. " " " Not Changed
*  No changed Count
        GV_NOCHG = GV_NOCHG + 1.
    ENDCASE.

* Additional data
    WA_LOG_EXPAND-DATUB = WA_MKAL_EXPAND-BDATU.
    WA_LOG_EXPAND-DATUV = WA_MKAL_EXPAND-ADATU.
    WA_LOG_EXPAND-AENNR = WA_MKAL_EXPAND-AENNR.
*     Fill change table with highest counter entry
    CLEAR L_COUNT.
    SELECT COUNT( * )  INTO L_COUNT  FROM MKAL_AEND
                       WHERE MATNR = WA_LOG_EXPAND-MATNR AND
                             WERKS = WA_LOG_EXPAND-WERKS AND
                             VERID = WA_LOG_EXPAND-VERID.
    WA_LOG_EXPAND-ZAEHL = L_COUNT + 1 .

* DB handling
    IF WA_LOG_EXPAND-VBKZ NE SPACE.
      PERFORM CALL_UPDATE_FM.
    ENDIF.

    CLEAR IT_MKAL.
  ENDLOOP.

* Commit work
  COMMIT WORK AND WAIT.

* Total Count
  CLEAR GV_TOTAL.
  DESCRIBE TABLE IT_MKAL LINES GV_TOTAL.

ENDFORM.                    " PRD_VR_CHANGEABLE

*&---------------------------------------------------------------------*
*&      Form  CALL_UPDATE_FM
*&---------------------------------------------------------------------*
*       Update Handling
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_UPDATE_FM.

  DATA BEGIN OF IT_L_MKAL_U OCCURS 0.
          INCLUDE STRUCTURE MKAL.
  DATA END OF IT_L_MKAL_U.
  CLEAR IT_L_MKAL_U. REFRESH IT_L_MKAL_U.

  DATA BEGIN OF IT_L_MKAL_I OCCURS 0.
          INCLUDE STRUCTURE MKAL.
  DATA END OF IT_L_MKAL_I.
  CLEAR IT_L_MKAL_I. REFRESH IT_L_MKAL_I.

  DATA BEGIN OF IT_L_MKAL_D OCCURS 0.
          INCLUDE STRUCTURE MKAL.
  DATA END OF IT_L_MKAL_D.
  CLEAR IT_L_MKAL_D. REFRESH IT_L_MKAL_D.

  DATA IT_L_MKAL_AEND LIKE MKAL_AEND OCCURS 0 WITH HEADER LINE.
  CLEAR IT_L_MKAL_AEND. REFRESH IT_L_MKAL_AEND.

* Update
  IF      WA_LOG_EXPAND-VBKZ = 'U'.
    MOVE-CORRESPONDING  WA_MKAL_EXPAND TO IT_L_MKAL_U.
    APPEND IT_L_MKAL_U.    CLEAR IT_L_MKAL_U.
    MOVE-CORRESPONDING  WA_LOG_EXPAND  TO  IT_L_MKAL_AEND.
    APPEND IT_L_MKAL_AEND. CLEAR IT_L_MKAL_AEND.
* Insert
  ELSEIF  WA_LOG_EXPAND-VBKZ = 'I'.
    MOVE-CORRESPONDING WA_MKAL_EXPAND TO IT_L_MKAL_I.
    APPEND IT_L_MKAL_I. CLEAR IT_L_MKAL_I.
    MOVE-CORRESPONDING  WA_LOG_EXPAND  TO  IT_L_MKAL_AEND.
    APPEND IT_L_MKAL_AEND. CLEAR IT_L_MKAL_AEND.
  ENDIF.

* Database updates on MKAL and MKAL_AEND
  CALL FUNCTION 'CM_FV_PROD_VERS_DB_UPDATE' IN UPDATE TASK
       TABLES
            IT_MKAL_I    = IT_L_MKAL_I
            IT_MKAL_U    = IT_L_MKAL_U
            IT_MKAL_D    = IT_L_MKAL_D
            IT_MKAL_AEND = IT_L_MKAL_AEND.

ENDFORM.                    " CALL_UPDATE_FM

*&---------------------------------------------------------------------*
*&      Form  WRITE_LOG
*&---------------------------------------------------------------------*
*       Log
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_LOG.
** For Log
*DATA : GV_TOTAL  TYPE I.
*DATA : GV_INSERT TYPE I.
*DATA : GV_UPDATE TYPE I.
*DATA : GV_NOCHG  TYPE I.

* Update Task is managed by system
* Can be checked using SAP Office

  WRITE : / 'Total Count      : ' , GV_TOTAL.
  WRITE : / 'Created Count    : ' , GV_INSERT.
  WRITE : / 'Updated Count    : ' , GV_UPDATE.
  WRITE : / 'No changed Count : ' , GV_NOCHG.

ENDFORM.                    " WRITE_LOG

*&---------------------------------------------------------------------*
*&      Form  DEQUEUE_EMMKALE
*&---------------------------------------------------------------------*
*       DEQUEUE
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DEQUEUE_EMMKALE.

  CALL FUNCTION 'DEQUEUE_EMMKALE'
    EXPORTING
      MODE_MKAL       = 'E'
      MANDT           = SY-MANDT
*     MATNR           =
*     WERKS           =
*     VERID           =
*     X_MATNR         = ' '
*     X_WERKS         = ' '
*     X_VERID         = ' '
*     _SCOPE          = '3'
*     _SYNCHRON       = ' '
*     _COLLECT        = ' '
            .

  LOOP AT IT_MKAL.
    CALL FUNCTION 'DEQUEUE_EMMKALE'
      EXPORTING
        MODE_MKAL       = 'E'
        MANDT           = SY-MANDT
        MATNR           = IT_MKAL-MATNR
        WERKS           = IT_MKAL-WERKS
        VERID           = IT_MKAL-VERID
*       X_MATNR         = ' '
*       X_WERKS         = ' '
*       X_VERID         = ' '
*       _SCOPE          = '3'
*       _SYNCHRON       = ' '
*       _COLLECT        = ' '
                .
    CLEAR IT_MKAL.
  ENDLOOP.

ENDFORM.

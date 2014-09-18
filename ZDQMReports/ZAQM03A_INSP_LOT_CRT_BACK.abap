************************************************************************
* Program Name      : ZAQM03A_INSP_LOT_CRT_BACK
* Author            : SeungLyong, Lee
* Creation Date     : 2003.09.24.
* Specifications By : SeungLyong, Lee
* Development Request No :
* Addl Documentation:
* Description       : Inspection Scheduling - Creating Inspection Lot
*                     is automatically scheduled as background jobs .
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT  ZAQM03A_INSP_LOT_CRT_BACK     .

*-- Include Program ( Include Constants or etc)
INCLUDE : ZQM_INCLUDE_POOL01. "/Inspection Constants and etc


*-- SAP Scripts Object Interface
*TABLES : THEAD. "/SAPscript: Text Header

*//Tables;(TABLES : Table_Name  "//Table Description)
TABLES : ZTQM_INSP_HDR,  "/Inspection Scheduling Header
         ZTQM_INSP_ITEM, "/Inspection Scheduling Item
         ZTQM_INSP_ITEM_F, "/Item(Flat Type)-Collective Summary
         ZTQM_INSP_S_ITEM. "/Sub Item : ISIR, Regualr, MS

*//Structures Declaration(TABLES : Structure Name."/Description)
TABLES : ZSQM_INSP_SCH_HDR,   "/Inspection Scheduling Header Str
         ZSQM_INSP_SCH_ITEM_F. "/Inspection Scheduling Item(Flat Type)

TABLES : ZSCA_TIME_STAMP.   "/Time Stamp Structre.


*-- Internale Tables with structure as sama as DB
DATA: IT_ZTQM_INSP_HDR    LIKE ZTQM_INSP_HDR
                              OCCURS 0 WITH HEADER LINE,
      IT_ZTQM_INSP_ITEM   LIKE ZTQM_INSP_ITEM
                              OCCURS 0 WITH HEADER LINE,
      IT_ZTQM_INSP_ITEM_F LIKE ZTQM_INSP_ITEM_F
                              OCCURS 0 WITH HEADER LINE,
      IT_ZTQM_INSP_S_ITEM LIKE ZTQM_INSP_S_ITEM
                              OCCURS 0 WITH HEADER LINE.

DATA : IT_BDC_MSG LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.

DATA : IT_MSG_HISTORY LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF IT_SUCCESS OCCURS 0,
         VEHICLE     TYPE   ZVEHICLE,
         IYEAR       TYPE   ZQYEAR,
         ART         TYPE   QPART,
         MATNR       TYPE   MATNR,
         PRUEFLOS    TYPE   QPLOS,
         PRUEFLOS_MS TYPE   QPLOS,
       END OF IT_SUCCESS.

DATA : IT_FAIL LIKE IT_SUCCESS OCCURS 0 WITH HEADER LINE.

*-- Global Variable
DATA : WA_ITEM_INDEX LIKE SY-TABIX.

DATA : WA_INSP_HDR  TYPE ZTQM_INSP_HDR,
       WA_INSP_ITEM TYPE ZTQM_INSP_ITEM_F.

DATA : WA_RETURN  TYPE BAPIRETURN.

DATA : WA_PRUEFLOS    TYPE QPLOS,
       WA_PRUEFLOS_MS TYPE QPLOS.


***//Macro Definitions
DEFINE FAIL_ITEM_HISTORY.
  CLEAR IT_FAIL.
  MOVE-CORRESPONDING IT_ZTQM_INSP_ITEM_F TO IT_FAIL.
  APPEND IT_FAIL.
END-OF-DEFINITION.

*// Event Handling.
*LOAD-OF-PROGRAM.

START-OF-SELECTION.
  PERFORM ENQUEUE_DB.
*--  Get Inspection Scheduling data : fetch data, its Date of inspection
*--  start is sy-datum and status is CREATE from ZTQM_INSP_S_ITEM.

  SELECT  DISTINCT *
    INTO CORRESPONDING FIELDS OF TABLE IT_ZTQM_INSP_S_ITEM
      FROM ZVQM_INSP_SCH
        WHERE STATUS     = C_CREATION   "/STATUS = CREATEION
          AND PRUEFLOS   = '000000000000'
          AND PRUEFDATUV = SY-DATUM
          AND PRUEFDATUB = '00000000'.

  CHECK SY-SUBRC = 0.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_ZTQM_INSP_ITEM_F
      FROM ZTQM_INSP_ITEM_F
       FOR ALL ENTRIES IN IT_ZTQM_INSP_S_ITEM
       WHERE  VEHICLE = IT_ZTQM_INSP_S_ITEM-VEHICLE
         AND  ART     = IT_ZTQM_INSP_S_ITEM-ART
         AND  IYEAR   = IT_ZTQM_INSP_S_ITEM-IYEAR
         AND  MATNR   = IT_ZTQM_INSP_S_ITEM-MATNR
         AND  STATUS  = C_CREATION.               "/STATUS = CREATEION

  SORT IT_ZTQM_INSP_S_ITEM BY VEHICLE IYEAR ART.

END-OF-SELECTION.

*--  Check exist item for creating inspection lot at today.
  CHECK NOT IT_ZTQM_INSP_S_ITEM[] IS INITIAL.

  LOOP AT IT_ZTQM_INSP_S_ITEM.

    CLEAR IT_ZTQM_INSP_ITEM_F .
    CLEAR WA_ITEM_INDEX.
    READ TABLE IT_ZTQM_INSP_ITEM_F
                       WITH KEY VEHICLE = IT_ZTQM_INSP_S_ITEM-VEHICLE
                                IYEAR   = IT_ZTQM_INSP_S_ITEM-IYEAR
                                ART     = IT_ZTQM_INSP_S_ITEM-ART
                                MATNR   = IT_ZTQM_INSP_S_ITEM-MATNR.

    CHECK SY-SUBRC = 0.
    WA_ITEM_INDEX = SY-TABIX.
    CLEAR IT_ZTQM_INSP_ITEM_F.

*-- Create Instpection lot using BDC function

    CLEAR : WA_INSP_HDR,    WA_INSP_ITEM, WA_PRUEFLOS,
            WA_PRUEFLOS_MS, WA_RETURN.
    REFRESH IT_BDC_MSG.

    MOVE-CORRESPONDING : IT_ZTQM_INSP_ITEM_F TO WA_INSP_HDR,
                         IT_ZTQM_INSP_ITEM_F TO WA_INSP_ITEM.

    CALL FUNCTION 'Z_FQM_INSPECTION_LOT_CREATE'
         EXPORTING
              I_INSP_HDR              = WA_INSP_HDR
              I_INSP_ITEM             = WA_INSP_ITEM
         IMPORTING
              E_PRUEFLOS              = WA_PRUEFLOS
              E_PRUEFLOS_MS           = WA_PRUEFLOS_MS
              RETURN                  = WA_RETURN
         TABLES
              T_BDC_MSG               = IT_BDC_MSG
         EXCEPTIONS
              ERROR_DURING_CREATE_LOT = 1
              OTHERS                  = 2.



    IF SY-SUBRC <> 0.
      READ TABLE IT_BDC_MSG WITH KEY MSGTYP = 'E'.

*-- fill  BDC Message Hostory table
      APPEND IT_BDC_MSG TO IT_MSG_HISTORY.
      FAIL_ITEM_HISTORY.
      CONTINUE.
    ENDIF.


    READ TABLE IT_BDC_MSG WITH KEY MSGTYP = 'S'
                                   MSGNR  = '100'.

    IF SY-SUBRC NE 0.
*      MESSAGE E000(ZMQM) WITH TEXT-E08.
*-- fill  BDC Message Hostory table
      APPEND IT_BDC_MSG TO IT_MSG_HISTORY.
      FAIL_ITEM_HISTORY.
      CONTINUE.
    ENDIF.

*/-- Set DB data - START
*- ZTQM_INSP_ITEM_F
    UPDATE ZTQM_INSP_ITEM_F  SET PRUEFLOS    = WA_PRUEFLOS
                                 PRUEFLOS_MS = WA_PRUEFLOS_MS
                                 AEDAT       = SY-DATUM
                                 AEZET       = SY-UZEIT
                                 AENAM       = SY-UNAME
              WHERE VEHICLE = IT_ZTQM_INSP_ITEM_F-VEHICLE
                AND IYEAR   = IT_ZTQM_INSP_ITEM_F-IYEAR
                AND ART     = IT_ZTQM_INSP_ITEM_F-ART
                AND MATNR   = IT_ZTQM_INSP_ITEM_F-MATNR.
    IF SY-SUBRC NE 0.
      ROLLBACK WORK.
      CONTINUE.
    ELSE.
*-     ZTQM_INSP_ITEM.
      UPDATE ZTQM_INSP_ITEM    SET PRUEFLOS    = WA_PRUEFLOS
                                   PRUEFLOS_MS = WA_PRUEFLOS_MS
                WHERE VEHICLE = IT_ZTQM_INSP_ITEM_F-VEHICLE
                  AND IYEAR   = IT_ZTQM_INSP_ITEM_F-IYEAR
                  AND ART     = IT_ZTQM_INSP_ITEM_F-ART
                  AND MATNR   = IT_ZTQM_INSP_ITEM_F-MATNR.

      IF SY-SUBRC NE 0.
        ROLLBACK WORK.
        CONTINUE.
      ELSE.
*-        ZTQM_INSP_S_ITEM - 8910/8920.
        UPDATE ZTQM_INSP_S_ITEM  SET PRUEFLOS    = WA_PRUEFLOS
                  WHERE VEHICLE = IT_ZTQM_INSP_ITEM_F-VEHICLE
                    AND IYEAR   = IT_ZTQM_INSP_ITEM_F-IYEAR
                    AND ART     = IT_ZTQM_INSP_ITEM_F-ART
                    AND MATNR   = IT_ZTQM_INSP_ITEM_F-MATNR
                    AND VORGLFNR = '00000001'
                    AND MERKNR BETWEEN '0010' AND '0100'.

        IF SY-SUBRC NE 0.
          ROLLBACK WORK.
          CONTINUE.
        ELSE.
*-        ZTQM_INSP_S_ITEM - 8930.: MS
          UPDATE ZTQM_INSP_S_ITEM  SET PRUEFLOS    = WA_PRUEFLOS_MS
                    WHERE VEHICLE = IT_ZTQM_INSP_ITEM_F-VEHICLE
                      AND IYEAR   = IT_ZTQM_INSP_ITEM_F-IYEAR
                      AND ART     = C_INSP_TYPE_MS          "/'8930'
                      AND MATNR   = IT_ZTQM_INSP_ITEM_F-MATNR
                      AND VORGLFNR IN ('00000001', '00000002')
                      AND MERKNR  = '0010'.
          IF SY-SUBRC NE 0.
            ROLLBACK WORK.
            CONTINUE.
          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

    CHECK SY-SUBRC = 0.
    COMMIT WORK.

*/-- Set DB data - END

*/-  Set Internal Tables.

    MOVE : WA_PRUEFLOS    TO IT_ZTQM_INSP_ITEM_F-PRUEFLOS,
           WA_PRUEFLOS_MS TO IT_ZTQM_INSP_ITEM_F-PRUEFLOS_MS.

    IT_ZTQM_INSP_ITEM_F-STATUS = C_RELEASE.

    MOVE : SY-DATUM TO IT_ZTQM_INSP_ITEM_F-AEDAT,
           SY-UZEIT TO IT_ZTQM_INSP_ITEM_F-AEZET,
           SY-UNAME TO IT_ZTQM_INSP_ITEM_F-AENAM.


    MODIFY  IT_ZTQM_INSP_ITEM_F INDEX WA_ITEM_INDEX.

*--  Success item append to IT_SUCCESS.
    CLEAR IT_SUCCESS.
    MOVE-CORRESPONDING IT_ZTQM_INSP_ITEM_F TO IT_SUCCESS.
    APPEND IT_SUCCESS.

*-- SET HEADER STATUS AFTER CHECK ITEM STATUS.
    SELECT SINGLE *
       FROM ZTQM_INSP_ITEM_F
          WHERE VEHICLE = IT_ZTQM_INSP_ITEM_F-VEHICLE
            AND IYEAR   = IT_ZTQM_INSP_ITEM_F-IYEAR
            AND ART     = IT_ZTQM_INSP_ITEM_F-ART
            AND STATUS  = C_CREATION.

    IF SY-SUBRC NE 0.

      UPDATE ZTQM_INSP_HDR   SET STATUS = C_RELEASE
                    WHERE VEHICLE = IT_ZTQM_INSP_ITEM_F-VEHICLE
                      AND IYEAR   = IT_ZTQM_INSP_ITEM_F-IYEAR
                      AND ART     = IT_ZTQM_INSP_ITEM_F-ART.

      IF SY-SUBRC NE 0.
        ROLLBACK WORK.
        CONTINUE.
      ENDIF.
    ENDIF.

  ENDLOOP.

*- Dequeue table from lock
  PERFORM DEQUEUE_DB.

  MESSAGE S000(ZMQM)
  WITH 'End of Creating Inspection Lot :'(S02) .

*&------------------------------------------------------------------*
*&      Form  ENQUEUE_DB
*&------------------------------------------------------------------*
FORM ENQUEUE_DB.

  CALL FUNCTION 'ENQUEUE_EZ_ZTQM_INSP_HDR'
       EXPORTING
            MODE_ZTQM_INSP_HDR = 'X'
            MANDT              = SY-MANDT
            VEHICLE            = ZTQM_INSP_HDR-VEHICLE
            ART                = ZTQM_INSP_HDR-ART
            IYEAR              = ZTQM_INSP_HDR-IYEAR
       EXCEPTIONS
            FOREIGN_LOCK       = 1
            SYSTEM_FAILURE     = 2
            OTHERS             = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


  CALL FUNCTION 'ENQUEUE_EZ_ZTQM_INSP_IT1'
       EXPORTING
            MODE_ZTQM_INSP_ITEM = 'X'
            MANDT               = SY-MANDT
            VEHICLE             = ZTQM_INSP_HDR-VEHICLE
            ART                 = ZTQM_INSP_HDR-ART
            IYEAR               = ZTQM_INSP_HDR-IYEAR
*            MATNR               =
       EXCEPTIONS
            FOREIGN_LOCK        = 1
            SYSTEM_FAILURE      = 2
            OTHERS              = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL FUNCTION 'ENQUEUE_EZ_ZTQM_INSP_IT2'
       EXPORTING
            MODE_ZTQM_INSP_ITEM_F = 'X'
            MANDT                 = SY-MANDT
            VEHICLE               = ZTQM_INSP_HDR-VEHICLE
            ART                   = ZTQM_INSP_HDR-ART
            IYEAR                 = ZTQM_INSP_HDR-IYEAR
*            MATNR                 =
       EXCEPTIONS
            FOREIGN_LOCK          = 1
            SYSTEM_FAILURE        = 2
            OTHERS                = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL FUNCTION 'ENQUEUE_EZ_ZTQM_INSP_IT3'
       EXPORTING
            MODE_ZTQM_INSP_S_ITEM = 'X'
            MANDT                 = SY-MANDT
            VEHICLE               = ZTQM_INSP_HDR-VEHICLE
            ART                   = ZTQM_INSP_HDR-ART
            IYEAR                 = ZTQM_INSP_HDR-IYEAR
*            MATNR                 =
*            VORGLFNR              =
*            MERKNR                =
       EXCEPTIONS
            FOREIGN_LOCK          = 1
            SYSTEM_FAILURE        = 2
            OTHERS                = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " ENQUEUE_DB
*&------------------------------------------------------------------*
*&      Form  DEQUEUE_DB
*&------------------------------------------------------------------*
FORM DEQUEUE_DB.

  CALL FUNCTION 'DEQUEUE_EZ_ZTQM_INSP_HDR'
       EXPORTING
            MODE_ZTQM_INSP_HDR = 'X'
            MANDT              = SY-MANDT
            VEHICLE            = ZTQM_INSP_HDR-VEHICLE
            ART                = ZTQM_INSP_HDR-ART
            IYEAR              = ZTQM_INSP_HDR-IYEAR.


  CALL FUNCTION 'DEQUEUE_EZ_ZTQM_INSP_IT1'
       EXPORTING
            MODE_ZTQM_INSP_ITEM = 'X'
            MANDT               = SY-MANDT
            VEHICLE             = ZTQM_INSP_HDR-VEHICLE
            ART                 = ZTQM_INSP_HDR-ART
            IYEAR               = ZTQM_INSP_HDR-IYEAR.
*            MATNR               = ''.


  CALL FUNCTION 'DEQUEUE_EZ_ZTQM_INSP_IT2'
       EXPORTING
            MODE_ZTQM_INSP_ITEM_F = 'X'
            MANDT                 = SY-MANDT
            VEHICLE               = ZTQM_INSP_HDR-VEHICLE
            ART                   = ZTQM_INSP_HDR-ART
            IYEAR                 = ZTQM_INSP_HDR-IYEAR.
*            MATNR                 = ''.


  CALL FUNCTION 'DEQUEUE_EZ_ZTQM_INSP_IT3'
       EXPORTING
            MODE_ZTQM_INSP_S_ITEM = 'X'
            MANDT                 = SY-MANDT
            VEHICLE               = ZTQM_INSP_HDR-VEHICLE
            ART                   = ZTQM_INSP_HDR-ART
            IYEAR                 = ZTQM_INSP_HDR-IYEAR.
*            MATNR                 = ''
*            VORGLFNR              = ''
*            MERKNR                = ''.




ENDFORM.                    " DEQUEUE_DB

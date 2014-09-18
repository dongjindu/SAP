************************************************************************
* Program Name      : ZRRF_MM_MIDDLEWARE_DATA_UPLOAD
* Author            : Bongsoo, Kim
* Creation Date     : 2004.09.07.
* Specifications By : Bongsoo, Kim
* Development Request No : UD1K912102
* Addl Documentation:
* Description       : Middleware receiver data upload
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
************************************************************************
REPORT ZRRF_MM_MIDDLEWARE_RECEIVER_UP
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID ZMRF.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: VBUK,  "Sales Document: Header Status and Administrative Data
        LIKP,  "SD Document: Delivery Header Data
        LIPS,  "SD document: Delivery: Item data
        ZTRF_MW_ER_LOG.  "Midderware error log
*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_RECE OCCURS 0,
        LIFEX TYPE ZSRF_RECEIVER-LIFEX,
        BORGR TYPE ZSRF_RECEIVER-BORGR,
        VBELN TYPE ZSRF_RECEIVER-VBELN,
        LIFNR TYPE ZSRF_RECEIVER-LIFNR,
        LFDAT TYPE ZSRF_RECEIVER-LFDAT,
        LFUHR TYPE ZSRF_RECEIVER-LFUHR,
        WBSTK TYPE ZSRF_RECEIVER-WBSTK,
        KOSTK TYPE ZSRF_RECEIVER-KOSTK,
        KDMAT TYPE ZSRF_RECEIVER-KDMAT,
     END   OF IT_RECE.

*----------------------------------------------------------------------*
* DATA
*----------------------------------------------------------------------*
DATA : C_DEST1(10) VALUE 'XLINK01',   "Outbound Interface Destination
       C_DEST2(10) VALUE 'XLINK02'.   "Outbound Interface Destination

*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
PARAMETERS P_CHK NO-DISPLAY DEFAULT 'X'.



*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM READ_PROCESS.
  PERFORM DATA_PROCESS.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_PROCESS.
  SELECT A~LIFEX
         A~BORGR_GRP
         B~VBELN
         A~LIFNR
         A~LFDAT
         A~LFUHR
         B~WBSTK
         B~KOSTK
         C~KDMAT
        FROM LIKP AS A INNER JOIN VBUK AS B
                         ON  B~VBELN EQ A~VBELN
                         AND B~WBSTK EQ 'A'
                       INNER JOIN LIPS AS C
                         ON  A~VBELN EQ C~VBELN
        INTO TABLE IT_RECE
        WHERE B~KOSTK     IN ('A', ' ').
  IF SY-SUBRC EQ 0.
    SORT IT_RECE BY VBELN.
    DELETE IT_RECE WHERE LIFEX EQ SPACE
                   OR    BORGR EQ SPACE
                   OR    KDMAT EQ SPACE.
    DELETE ADJACENT DUPLICATES FROM IT_RECE COMPARING KDMAT.  "VBELN.

  ENDIF.
ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DATA_PROCESS.
  DATA: LT_ERROR LIKE ZTRF_MW_ER_LOG OCCURS 0 WITH HEADER LINE,
        LT_RECE  LIKE ZSRF_RECE_DATA  OCCURS 0 WITH HEADER LINE.
  DATA: L_MSGTXT(80).
  DATA: L_LINES1 TYPE I,
        L_TLINE1(9).
  CHECK NOT IT_RECE[] IS INITIAL.
  DESCRIBE TABLE IT_RECE LINES L_LINES1.

  LOOP AT IT_RECE.
    MOVE-CORRESPONDING IT_RECE TO LT_RECE.
    APPEND LT_RECE. CLEAR LT_RECE.
  ENDLOOP.

  WRITE: L_LINES1 TO L_TLINE1 LEFT-JUSTIFIED.
*     MIDDLEWARE SERVER 1
  CALL FUNCTION 'Z_FRF_MM_MW_RECEIVER_UP'
        DESTINATION  C_DEST1
    EXPORTING
      LIST_COUNT       = L_TLINE1
    TABLES
      T_RECEIVER       = LT_RECE
    EXCEPTIONS
      COMMUNICATION_FAILURE = 1  MESSAGE L_MSGTXT
      SYSTEM_FAILURE        = 2  MESSAGE L_MSGTXT.
  IF SY-SUBRC NE 0.
    LT_ERROR-MANDT = SY-MANDT.
    LT_ERROR-DATUM = SY-DATUM.
    LT_ERROR-UZEIT = SY-UZEIT.
    CONCATENATE C_DEST1 ' : ' 'RECEIVER' INTO LT_ERROR-MIDWA.
*      LT_ERROR-MIDWA = C_DEST1.
    LT_ERROR-MESSA = L_MSGTXT.
    APPEND LT_ERROR. CLEAR LT_ERROR.
  ENDIF.
  CLEAR: L_MSGTXT.
*     MIDDLEWARE SERVER 2

  CALL FUNCTION 'Z_FRF_MM_MW_RECEIVER_UP'
        DESTINATION  C_DEST2
    EXPORTING
      LIST_COUNT       = L_TLINE1
    TABLES
      T_RECEIVER       = LT_RECE
    EXCEPTIONS
      COMMUNICATION_FAILURE = 1  MESSAGE L_MSGTXT
      SYSTEM_FAILURE        = 2  MESSAGE L_MSGTXT.
  IF SY-SUBRC NE 0.
    LT_ERROR-MANDT = SY-MANDT.
    LT_ERROR-DATUM = SY-DATUM.
    LT_ERROR-UZEIT = SY-UZEIT.
    CONCATENATE C_DEST2 ' : ' 'RECEIVER' INTO LT_ERROR-MIDWA.
*      LT_ERROR-MIDWA = C_DEST1.
    LT_ERROR-MESSA = L_MSGTXT.
    APPEND LT_ERROR. CLEAR LT_ERROR.
  ENDIF.

  CLEAR: L_MSGTXT.
  IF NOT LT_ERROR[] IS INITIAL.
    INSERT ZTRF_MW_ER_LOG CLIENT SPECIFIED FROM TABLE LT_ERROR
                          ACCEPTING DUPLICATE KEYS .
    IF SY-SUBRC EQ 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
*    WRITE: / 'Error'.
  ELSE.
*    WRITE: / 'Success'.
  ENDIF.













ENDFORM.                    " DATA_PROCESS

************************************************************************
* Author                 : Furong Wang
* Creation Date          : 1/31/2006
* Development Request No :
* Addl documentation     :
* Description            : OTD data to HMA
* Modification Log
* Date       Developer    Request ID Description
* Description            : This interface includes data from two tables
*                          MARA and AUSP.
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************

REPORT ZIPP_ACTIVE_WO_LIST MESSAGE-ID ZMPP.

TABLES: ZTPP_WOSUM.
DATA: BEGIN OF IT_LIST_OUT OCCURS 0,
        REC_TYPE(3) TYPE C,
        WO_NO(15) TYPE C,
        REGION(2) TYPE C ,
        PORT(2) TYPE C ,
        MI(15) TYPE C,
        OCN(4) TYPE C,
        EXTC(3) TYPE C,
        INTC(3) TYPE C,
        START_PO_YR(4) TYPE C ,
        START_PO_MO(2) TYPE C ,
        END_PO_YR(6) TYPE C,
        FILLER(21) TYPE C ,
      END OF IT_LIST_OUT.

DATA: IT_WOSUM LIKE TABLE OF ZTPP_WOSUM WITH HEADER LINE.
DATA: W_CNT TYPE I.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_DATE  FOR SY-DATUM OBLIGATORY DEFAULT SY-DATUM.
SELECT-OPTIONS : S_WO_SER FOR ZTPP_WOSUM-WO_SER.
SELECTION-SCREEN END OF BLOCK B1.

START-OF-SELECTION.

  PERFORM GET_PROCESS_DATA.
*
END-OF-SELECTION.
  PERFORM MAKE_FILE.
  PERFORM SAVE_TO_TABLE.


*&---------------------------------------------------------------------*
*&      Form  MAKE_FILE.  Makes the output file.
*&---------------------------------------------------------------------*
FORM MAKE_FILE.
  DATA: L_DATE(6).

  DATA : BEGIN OF IT_LIST_HEAD,
         REC_TYPE(3) TYPE C,
         T_DATE TYPE DATS,
         T_TIME TYPE TIMS,
       END OF IT_LIST_HEAD,
       BEGIN OF IT_LIST_TAIL,
         REC_TYPE(3) TYPE C,
         T_COUNT(7) TYPE C,
         FILLER(7) TYPE C,
       END OF IT_LIST_TAIL.
  DATA : DSN(120).

  DESCRIBE TABLE IT_LIST_OUT LINES W_CNT.
  IF W_CNT = 0.
    MESSAGE I000 WITH 'No Data Found'.
    STOP.
  ENDIF.

  L_DATE = SY-DATUM+2(6).

  CONCATENATE  '/usr/sap/EDI_SAP/'
               'HOTD' L_DATE
               '.txt'
               INTO DSN.
* make HEADER info for file.
  IT_LIST_HEAD-REC_TYPE = 'O3H'.
  IT_LIST_HEAD-T_DATE = SY-DATUM.
  IT_LIST_HEAD-T_TIME = SY-UZEIT.

  OPEN DATASET DSN IN TEXT MODE FOR OUTPUT.

  TRANSFER IT_LIST_HEAD TO DSN.
* Transfer the data to File.
  LOOP AT IT_LIST_OUT.
    OPEN DATASET DSN IN TEXT MODE FOR APPENDING.
    TRANSFER IT_LIST_OUT TO DSN.
  ENDLOOP.
* make Tail info for file.
  IT_LIST_TAIL-REC_TYPE = 'O3T'.
  IT_LIST_TAIL-T_COUNT = W_CNT.
  IT_LIST_TAIL-FILLER = '             '.

  OPEN DATASET DSN IN TEXT MODE FOR APPENDING.
  TRANSFER IT_LIST_TAIL TO DSN.

  CLOSE DATASET DSN.

  IF SY-SUBRC = 0.

    WRITE: /10 'FILE IS DOWNLOADED SUCCESSFULLY.'.
    SKIP.
    WRITE: /10 'File Name:', DSN.
    SKIP.
    WRITE: /10 'TOTAL RECORDS:', W_CNT.
  ELSE.
    FORMAT COLOR 6.
    WRITE: /10 'TOTAL RECORDS: ', W_CNT.
    SKIP.
    WRITE: /10 'FILE DOWNLOAD FAILED!'.
    FORMAT COLOR OFF.
    MESSAGE E000 WITH 'FILE DOWLOAD FAILED.'.
  ENDIF.

ENDFORM.    "MAKE_FILE END.

*---------------------------------------------------------------------*
*       FORM get_process_data                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM GET_PROCESS_DATA.
  DATA: L_DEALER(2) VALUE 'X '.

  DATA: L_MATNR LIKE MARA-MATNR,
        LT_MATNR LIKE MARA-MATNR.

  DATA: L_CONF LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE.

  DATA : BEGIN OF IT_LIST OCCURS 0,
         REC_TYPE(3) TYPE C VALUE 'O3D',
         WO_NO(15) TYPE C,
         REGION(2) TYPE C VALUE '  ',
         PORT(2) TYPE C VALUE '  ',
         MI(15) TYPE C,
         OCN(4) TYPE C,
         EXTC(3) TYPE C,
         INTC(3) TYPE C,
         PERF_YN(1) TYPE C,
         P_NATION(3) TYPE C,
         WO_CREATE_DATE(16) TYPE C,
         WO_MODI_DATE(16) TYPE C,
         START_PO_YR(4) TYPE C VALUE '    ',
         START_PO_MO(2) TYPE C VALUE '  ',
         END_PO_YR(6) TYPE C,
         FILLER(21) TYPE C VALUE '   ',
       END OF IT_LIST.

  SELECT * INTO TABLE IT_WOSUM
    FROM ZTPP_WOSUM
    WHERE WO_SER IN S_WO_SER
      AND WOCREDATE IN S_DATE
      AND NATION = 'B28'.
*      AND dealer LIKE l_dealer.

  LOOP AT IT_WOSUM.
** Changed by Furong on 11/26/07
*    IF it_wosum-dealer+0(1) = 'X'.
*      CONTINUE.
*    ENDIF.
    IF IT_WOSUM-DEALER+0(1) = 'A'.
** End of change
      CONCATENATE IT_WOSUM-WO_SER IT_WOSUM-NATION IT_WOSUM-DEALER
             INTO L_MATNR.
      IF L_MATNR <> LT_MATNR.
        CLEAR: L_CONF, L_CONF[].

        LT_MATNR = L_MATNR.

        L_CONF-ATNAM = 'P_PROD_FLAG'.
        L_CONF-ATWRT = 'Y'.
        APPEND L_CONF.

        CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
             EXPORTING
                  OBJECT       = L_MATNR
                  CTYPE        = '001'
             TABLES
                  VAL_TABLE    = L_CONF
             EXCEPTIONS
                  NO_DATA      = 1
                  ERROR_MODE   = 2
                  ERROR_OBJECT = 3
                  ERROR_VALUE  = 4
                  OTHERS       = 5.
        READ TABLE L_CONF INDEX 1.
        IT_LIST-WO_NO = L_MATNR.
        IT_LIST-P_NATION = IT_WOSUM-NATION.
        IT_LIST-REGION = IT_WOSUM-DEALER.
        IT_LIST-PERF_YN = L_CONF-ATWRT.
        IT_LIST-END_PO_YR = ' '.
        IT_LIST-REC_TYPE = 'O3D'.
        IT_LIST-REGION = '  '.
        IT_LIST-PORT = '  '.
        IT_LIST-FILLER = '                '.
      ENDIF.
      IT_LIST-PORT = IT_WOSUM-WO_SER+5(1).
** Changed by Fuorng on 11/08/07
      IF IT_WOSUM-FSC+13(1) = SPACE.
        IT_LIST-MI = IT_WOSUM-FSC+6(8).
      ELSE.
        IT_LIST-MI = IT_WOSUM-FSC+5(9).
      ENDIF.
** end of change
      IT_LIST-OCN = IT_WOSUM-FSC+14(4).
      IT_LIST-EXTC = IT_WOSUM-EXTC.
      IT_LIST-INTC = IT_WOSUM-INTC.
      IT_LIST-START_PO_YR = IT_WOSUM-WOCREDATE+0(4).
      IT_LIST-START_PO_MO = IT_WOSUM-WOCREDATE+4(2).

      IF IT_LIST-PERF_YN = 'Y'.
        APPEND IT_LIST.
      ENDIF.
    ENDIF.
*  CLEAR it_list.
  ENDLOOP.

  LOOP AT IT_LIST.
    MOVE-CORRESPONDING IT_LIST TO IT_LIST_OUT.
    APPEND IT_LIST_OUT.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SAVE_TO_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_TO_TABLE.
  DATA: IT_ACT_WOLIST LIKE TABLE OF ZTPP_ACT_WOLIST WITH HEADER LINE.
  LOOP AT IT_LIST_OUT.
    MOVE-CORRESPONDING IT_LIST_OUT TO IT_ACT_WOLIST.
    IT_ACT_WOLIST-ZSDAT = SY-DATUM.
    IT_ACT_WOLIST-ZSTIM = SY-UZEIT.
    APPEND IT_ACT_WOLIST.
    CLEAR: IT_ACT_WOLIST.
  ENDLOOP.
  INSERT ZTPP_ACT_WOLIST FROM TABLE IT_ACT_WOLIST
         ACCEPTING DUPLICATE KEYS.
  IF SY-SUBRC = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
  CLEAR: IT_LIST_OUT[].
ENDFORM.                    " SAVE_TO_TABLE

FUNCTION ZIM_LOCAL_EDI_DATA_SPLIT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(BUKRS) TYPE  BUKRS
*"  TABLES
*"      IT_FILENAME OPTIONAL
*"----------------------------------------------------------------------
TABLES : ZTIMIMGTX.

DATA: LOCK_FILE(200)  TYPE C,
      UNIXFILE(300)   TYPE C,
      MI_HANDLE       TYPE I,
      ANTWORT(1)      TYPE C,             " 공통 popup Screen에서 사?
      L_LEN           TYPE I,
      L_STRLEN        TYPE I,
      L_SIZE          TYPE I,
      W_MOD           TYPE I,
      L_NOT_FOUND     TYPE C     VALUE 'N',
      L_DATE          TYPE SY-DATUM,
      L_SEQ           LIKE ZTREQST-ZFAMDNO VALUE  '00001',
      INCLUDE(8)      TYPE C,             "
      L_TIME          TYPE SY-UZEIT,
      W_EDI_RECORD(65535),
      W_ZFDHREF       LIKE  ZTDHF1-ZFDHREF,
      L_ZFDHDOC       LIKE  ZTDHF1-ZFDHDOC,
      L_GUBUN,
      W_ZFDHENO       LIKE  ZTDHF1-ZFDHENO.
*------ EDI
DATA  :  UPLOAD_PATH(300)     TYPE       C.     " loading data
DATA  :  FILE_NAME(300)       TYPE       C.


DATA: BEGIN OF IT_FLATFILE OCCURS 0,
      FILENAME(100),
      END OF IT_FLATFILE.

DATA: BEGIN OF IT_FLATDATA OCCURS 0,
      RECORD(65535),
      END OF IT_FLATDATA.

   SELECT SINGLE * FROM ZTIMIMGTX
          WHERE    BUKRS EQ  BUKRS.


   CONCATENATE ZTIMIMGTX-ZFRECV '/' 'lock' INTO LOCK_FILE.
   UPLOAD_PATH = ZTIMIMGTX-ZFRECV.

   PERFORM  P2000_FTP_CONNECT   CHANGING   ZTIMIMGTX-UNAME
                                           ZTIMIMGTX-PASSW
                                           ZTIMIMGTX-HOST
                                           MI_HANDLE.

   PERFORM  P2000_GET_CURRENT_DIR USING UPLOAD_PATH
                                        MI_HANDLE.


   DO.
       OPEN  DATASET  LOCK_FILE.
       IF SY-SUBRC NE 0.
          OPEN DATASET LOCK_FILE FOR OUTPUT IN TEXT MODE.
*          TRANSFER     SPACE  TO     LOCK_FILE.
          CLOSE DATASET    LOCK_FILE.
          EXIT.
       ENDIF.
       WAIT UP TO 1 SECONDS.
   ENDDO.


   REFRESH : MTAB_DATA.
   CALL FUNCTION 'FTP_COMMAND'
        EXPORTING
           HANDLE        = MI_HANDLE
           COMMAND       = 'dir *.TXT -Nf'
*        IMPORTING
*           FILEDATE      = L_DATE
*           FILETIME      = L_TIME
*           FILESIZE      = L_SIZE
        TABLES
           DATA          = MTAB_DATA
        EXCEPTIONS
           TCPIP_ERROR   = 1
           COMMAND_ERROR = 2
           DATA_ERROR    = 3
           OTHERS        = 4.

   W_SUBRC = SY-SUBRC.

   IF W_SUBRC EQ 0.
      REFRESH : IT_FLATFILE.
      DESCRIBE TABLE MTAB_DATA LINES W_LINE.
      IF W_LINE LT 4.
*         MOVE 'Y'        TO     W_ERR_CHK.
*         MESSAGE S920.
         EXIT.
      ENDIF.
      W_LINE = W_LINE - 1.
      LOOP AT MTAB_DATA FROM 4 TO W_LINE.
         CASE MTAB_DATA+54(6).
            WHEN 'LOCADV' OR 'LOCAMA' OR 'FINBIL' OR
                 'INF700' OR 'INF707' OR 'DEBADV' OR
                 'DISCHG' OR 'DOANTC' OR 'LDANTC' OR
                 'IMPRES'.
               CLEAR : IT_FLATFILE.
               CONCATENATE UPLOAD_PATH MTAB_DATA+54 INTO IT_FLATFILE.
               APPEND IT_FLATFILE.
            WHEN OTHERS.
         ENDCASE.
      ENDLOOP.
   ENDIF.

*> FTP DISCONNET.
   CALL FUNCTION 'FTP_DISCONNECT'
      EXPORTING
            HANDLE   =   MI_HANDLE
      EXCEPTIONS
            OTHERS   =   1.

  REFRESH : IT_FILENAME.

  LOOP AT IT_FLATFILE.
*>>   file ---> internal table
      MOVE IT_FLATFILE-FILENAME TO FILE_NAME.
      OPEN DATASET    FILE_NAME     FOR     INPUT   IN  TEXT  MODE.
      IF SY-SUBRC NE 0.
         CONTINUE.
      ENDIF.
      REFRESH : IT_FLATDATA.
      L_NOT_FOUND = 'N'.
      CLEAR : W_ZFDHREF.

      DO.
          READ    DATASET    FILE_NAME     INTO    W_EDI_RECORD.
          IF SY-SUBRC    EQ    4.
             IF NOT IT_FLATDATA[] IS INITIAL.
                IF  L_ZFDHDOC    EQ 'FINBIL' AND
                    L_NOT_FOUND EQ 'N'.
                    L_GUBUN = 'I'.
                ELSE.
                    L_GUBUN = 'E'.
                ENDIF.
                CONCATENATE   UPLOAD_PATH L_GUBUN L_ZFDHDOC
                              SY-DATUM SY-UZEIT L_SEQ '.TXT'
                              INTO   UNIXFILE.

                OPEN DATASET UNIXFILE.
                DO.
                  IF SY-SUBRC EQ 0.
                     CLOSE DATASET UNIXFILE.
                  ELSE.
                     EXIT.
                  ENDIF.
                  L_SEQ = L_SEQ + 1.
                  CONCATENATE  UPLOAD_PATH L_GUBUN L_ZFDHDOC
                               SY-DATUM SY-UZEIT L_SEQ '.TXT'
                              INTO   UNIXFILE.
                  OPEN DATASET UNIXFILE.
               ENDDO.
               OPEN DATASET UNIXFILE FOR OUTPUT IN TEXT MODE.

               MOVE : UNIXFILE TO IT_FILENAME.
               APPEND IT_FILENAME.

               LOOP AT IT_FLATDATA.
                  TRANSFER IT_FLATDATA-RECORD  TO  UNIXFILE.
               ENDLOOP.
               CLOSE DATASET    UNIXFILE.

             ENDIF.
             CLOSE DATASET   FILE_NAME.
             EXIT.
          ELSE.
             IF W_EDI_RECORD(2) EQ '<<'.

                IF NOT IT_FLATDATA[] IS INITIAL.

                   IF  L_ZFDHDOC    EQ 'FINBIL' AND
                       L_NOT_FOUND EQ 'N'.
                       L_GUBUN = 'I'.
                   ELSE.
                       L_GUBUN = 'E'.
                   ENDIF.

                   CONCATENATE   UPLOAD_PATH  L_GUBUN L_ZFDHDOC
                                 SY-DATUM SY-UZEIT L_SEQ '.TXT'
                                 INTO   UNIXFILE.

                   OPEN DATASET UNIXFILE.
                   DO.
                     IF SY-SUBRC EQ 0.
                        CLOSE DATASET UNIXFILE.
                     ELSE.
                        EXIT.
                     ENDIF.
                     L_SEQ = L_SEQ + 1.
                     CONCATENATE  UPLOAD_PATH L_GUBUN L_ZFDHDOC
                                  SY-DATUM SY-UZEIT L_SEQ '.TXT'
                                 INTO   UNIXFILE.
                     OPEN DATASET UNIXFILE.
                  ENDDO.
                  OPEN DATASET UNIXFILE FOR OUTPUT IN TEXT MODE.

                  MOVE : UNIXFILE TO IT_FILENAME.
                  APPEND IT_FILENAME.

                  LOOP AT IT_FLATDATA.
                     TRANSFER IT_FLATDATA-RECORD  TO  UNIXFILE.
                  ENDLOOP.
                  CLOSE DATASET    UNIXFILE.
                ENDIF.

                REFRESH : IT_FLATDATA.
                L_ZFDHDOC = W_EDI_RECORD+65(06).
                CLEAR : L_GUBUN.
                CLEAR : W_ZFDHREF.
                L_NOT_FOUND = 'N'.
             ELSE.
                CASE  L_ZFDHDOC.
                   WHEN 'LOCADV'.   ">내국신용장 응답서.
                      IF W_EDI_RECORD(02) EQ '03'.
                         IF W_EDI_RECORD+2(03) EQ 'DM '.
                            MOVE: W_EDI_RECORD+5(35) TO W_ZFDHENO.
                            SELECT SINGLE * FROM  ZTDHF1
                                   WHERE ZFDHENO  EQ W_ZFDHENO.
                            IF SY-SUBRC EQ 0.
                               L_GUBUN = 'I'.  ">수입.
                            ELSE.
                               L_GUBUN = 'E'.  ">수출.
                            ENDIF.
                         ENDIF.
                      ENDIF.
                   WHEN 'LOCAMA'.   ">내국신용장 변경 응답서.
                      IF W_EDI_RECORD(02) EQ '02'.
                         IF W_EDI_RECORD+2(03) EQ 'DM '.
                            MOVE: W_EDI_RECORD+5(35) TO W_ZFDHENO.
                            SELECT SINGLE * FROM  ZTDHF1
                                   WHERE ZFDHENO  EQ W_ZFDHENO.
                            IF SY-SUBRC EQ 0.
                               L_GUBUN = 'I'.  ">수입.
                            ELSE.
                               L_GUBUN = 'E'.  ">수출.
                            ENDIF.
                         ENDIF.
                      ENDIF.
                   WHEN 'FINBIL'.         ">계산서...
                      CASE  W_EDI_RECORD(02).
                         WHEN '01'.
                            IF W_EDI_RECORD+2(03)  NE '2AR'. ">매입번호.
                               L_NOT_FOUND = 'Y'.
                            ENDIF.
                         WHEN '02'.     ">계산서용도.
                            CASE W_EDI_RECORD+5(03).
                               WHEN '2BE' OR '2BF' OR
                                    '2BG' OR '2BH' OR 'ZZZ'.
                               WHEN OTHERS.
                                  L_NOT_FOUND = 'Y'.
                            ENDCASE.
                         WHEN '03'.      ">참조번호.
*>매입번호. / 신용장 번호.
                            IF ( W_EDI_RECORD+2(03) EQ 'ACK' OR
                                 W_EDI_RECORD+2(03)  EQ  'AAC' )
                                 AND W_ZFDHREF IS INITIAL.
                               MOVE : W_EDI_RECORD+5(35) TO W_ZFDHREF.
                            ENDIF.
                            CASE W_ZFDHREF(1).
                               WHEN 'M'.
                                 CASE IT_ZTFINHD-ZFREFNO+8(2).
                                   WHEN 'ES' OR ">수출용원자재 AT SIGHT.
                                        'EU' OR ">수출용원자재 USANCE
                                        'NU' OR ">내수용원자재 USNACE
                                        'NS'.   ">내수용원자재 AT SIGHT.
                                   WHEN OTHERS.
                                      L_NOT_FOUND = 'Y'.
                                 ENDCASE.
                               WHEN 'L' OR 'D' OR 'I' OR 'P'.
                               WHEN OTHERS.
                                 L_NOT_FOUND = 'Y'.
                            ENDCASE.
                         WHEN OTHERS.
                      ENDCASE.
                   WHEN 'INF700' OR 'INF707' OR 'DEBADV' OR
                        'IMPRES' OR 'DOANTC' OR 'LDANTC' OR
                        'DISCHG'.
                        L_GUBUN = 'I'.  ">수입.
                   WHEN OTHERS.
                      CONTINUE.
                ENDCASE.
             ENDIF.
             IT_FLATDATA-RECORD = W_EDI_RECORD.
             APPEND IT_FLATDATA.
          ENDIF.
      ENDDO.
      DELETE DATASET FILE_NAME.
  ENDLOOP.

  DELETE  DATASET    LOCK_FILE.

ENDFUNCTION.

FUNCTION ZIM_LG_DOANTC_EDI_DOC.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(W_FILENAME) LIKE  ZTDHF1-FILENAME
*"     REFERENCE(BACK_PATH) LIKE  ZTIMIMGTX-ZFRBAK
*"  TABLES
*"      RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"  EXCEPTIONS
*"      UPDATE_ERROR
*"      NOT_FOUND
*"      NO_REFERENCE
*"      DOCUMENT_LOCKED
*"      DATE_ERROR
*"      NOT_FILE_OPEN
*"----------------------------------------------------------------------
DATA : C_ZFDDFDA1(3),
       WL_VIA(1)    TYPE C,
       L_ZFDHDOC    LIKE ZTDHF1-ZFDHDOC,
       L_NOT_FOUND  TYPE C VALUE 'N',
       W_MAILID     LIKE ZTIMIMGTX-ZFMAILID,
       W_ZFDHREF    LIKE ZTDHF1-ZFDHREF,
       W_EDI_RECORD(65535).

DATA : BEGIN OF XRETURN    OCCURS 0.
       INCLUDE  STRUCTURE  BAPIRET2.
DATA : END   OF XRETURN.

DATA : BEGIN OF IT_ZTPMTHD OCCURS 0.
       INCLUDE STRUCTURE ZTPMTHD.
       DATA : FILENAME   LIKE   ZTDHF1-FILENAME.
DATA : END   OF IT_ZTPMTHD.

*>> EDI용 미적용..
DATA : BEGIN OF IT_ZTPMTEDI OCCURS 0.
       INCLUDE STRUCTURE ZTPMTEDI.
DATA : END   OF IT_ZTPMTEDI.

  REFRESH : IT_TAB, RETURN, IT_ZTPMTEDI.
  CLEAR : L_ZFDHDOC,  W_TABIX, IT_TAB, RETURN, IT_ZTPMTEDI.
  L_NOT_FOUND = 'N'.

  OPEN    DATASET   W_FILENAME     FOR     INPUT   IN  TEXT  MODE.
  IF SY-SUBRC NE 0.
     MESSAGE E970 WITH W_FILENAME RAISING NOT_FILE_OPEN.
     EXIT.
  ENDIF.

  DO.
     READ    DATASET   W_FILENAME     INTO    W_EDI_RECORD.
     IF SY-SUBRC    EQ    4.
        EXIT.
     ENDIF.

*>> 문서의 시작.
     IF W_EDI_RECORD(2) EQ '<<'.
        CLEAR: IT_ZTPMTHD, IT_ZTPMTEDI.

        MOVE : W_FILENAME              TO IT_ZTPMTEDI-FILENAME,
               W_EDI_RECORD+65(06)     TO IT_ZTPMTEDI-ZFDHDOC,
               W_EDI_RECORD+16(08)     TO IT_ZTPMTEDI-ZFSDBK,
               SY-MANDT                TO IT_ZTPMTEDI-MANDT,
               W_EDI_RECORD+12(03)     TO W_MAILID,
               'N'                     TO IT_ZTPMTEDI-ZFDBYN.
        APPEND  IT_ZTPMTEDI.
        W_TABIX = SY-TABIX.
        L_ZFDHDOC = IT_ZTPMTEDI-ZFDHDOC.
      ELSE.
*>> 찾지 못했을 경우, CONTINUE.
         IF L_ZFDHDOC EQ 'DOANTC'.       ">선적서류도착통보서.
            CASE  W_EDI_RECORD(02).
               WHEN '02'.     ">NAD TAG.
                  CASE W_EDI_RECORD+2(03).
                     WHEN 'MR'.        ">MR(수신인)
                        MOVE: W_EDI_RECORD+5(35) TO IT_ZTPMTEDI-ZFRCVNM.
                     WHEN 'AX'.        ">AX(통지은행)
                        MOVE: W_EDI_RECORD+5(35) TO IT_ZTPMTEDI-ZFADVNM.
                     WHEN OTHERS.
                  ENDCASE.
                  MODIFY IT_ZTPMTEDI INDEX W_TABIX.
               WHEN '03'.     ">참조번호.
                  CASE W_EDI_RECORD+2(03).
                     WHEN 'AAC'.         ">신용장 번호.
                        MOVE: W_EDI_RECORD+5(35) TO IT_ZTPMTEDI-ZFOPNNO.
                        W_ZFDHREF = IT_ZTPMTEDI-ZFOPNNO.

                        W_MAILID+3(1) =  '%'.

                        SELECT * FROM  ZTIMIMGTX UP TO 1 ROWS
                                 WHERE ZFMAILID LIKE W_MAILID.
                        ENDSELECT.

                        W_SUBRC = SY-SUBRC.
                        IF W_SUBRC NE 0.
                           MESSAGE S659 WITH W_MAILID.
                           PERFORM P2000_SINGLE_MAKE   TABLES  RETURN
                                                       USING   'E'.
*                           CONTINUE.
                        ENDIF.
*>> 전자문서 GET!
                        CLEAR : W_ZFDHENO.
                        CALL FUNCTION 'ZIM_EDI_NUMBER_GET_NEXT'
                           EXPORTING
                              W_ZFCDDOC = L_ZFDHDOC
                              W_ZFDHSRO = SPACE
                              W_ZFDHREF = W_ZFDHREF
                              W_BUKRS   = ZTIMIMGTX-BUKRS
                          CHANGING
                              W_ZFDHENO = W_ZFDHENO
                          EXCEPTIONS
                              DB_ERROR  = 4
                              NO_TYPE   = 8.

                       CASE SY-SUBRC.
                          WHEN  4.
                             MESSAGE S118 WITH   W_ZFDHENO.
                             PERFORM P2000_SINGLE_MAKE   TABLES  RETURN
                                                         USING   'E'.
                          WHEN  8.
                             MESSAGE S117 WITH   L_ZFDHDOC.
                             PERFORM P2000_SINGLE_MAKE   TABLES  RETURN
                                                         USING   'E'.
                       ENDCASE.
                       MOVE W_ZFDHENO       TO     IT_ZTPMTEDI-ZFDHENO.
                       MODIFY  IT_ZTPMTEDI  INDEX  W_TABIX.
                     WHEN 'CT'.          ">계약서 번호.
                        MOVE: W_EDI_RECORD+5(35) TO IT_ZTPMTEDI-EBELN.
                     WHEN 'BM' OR 'AWB'. ">B/L 번호(해상/항공).
                        MOVE: W_EDI_RECORD+5(35) TO IT_ZTPMTEDI-ZFHBLNO.
                     WHEN OTHERS.
                  ENDCASE.
                  MODIFY IT_ZTPMTEDI INDEX W_TABIX.


               WHEN '04'.     ">어음금액/기타 수수료..
                  CASE W_EDI_RECORD+2(03).
                     WHEN '154'.
                        MOVE:W_EDI_RECORD+5(18) TO IT_ZTPMTEDI-ZFPNAM.
                        MOVE:W_EDI_RECORD+23(3) TO IT_ZTPMTEDI-ZFPNAMC.
                        PERFORM  SET_CURR_CONV_TO_INTERNAL
                                 CHANGING IT_ZTPMTEDI-ZFPNAM
                                          IT_ZTPMTEDI-ZFPNAMC.
                     WHEN '304'.
                        MOVE:W_EDI_RECORD+5(18) TO IT_ZTPMTEDI-ZFBKCH.
                        MOVE:W_EDI_RECORD+23(3) TO IT_ZTPMTEDI-ZFBKCHC.
                        PERFORM  SET_CURR_CONV_TO_INTERNAL
                                 CHANGING IT_ZTPMTEDI-ZFBKCH
                                          IT_ZTPMTEDI-ZFBKCHC.
                     WHEN OTHERS.
                  ENDCASE.
                  MODIFY IT_ZTPMTEDI INDEX W_TABIX.
               WHEN '05'.     ">통지일자/최종결제일.
                  CASE W_EDI_RECORD+2(03).
                     WHEN '184'.     ">통지일자.
                        CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
                             EXPORTING
                                 DATE_EXTERNAL = W_EDI_RECORD+5(08)
                             IMPORTING
                                 DATE_INTERNAL = IT_ZTPMTEDI-ZFNTDT.

                     WHEN '265'.     ">최종결제일.
                        CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
                             EXPORTING
                                 DATE_EXTERNAL = W_EDI_RECORD+5(08)
                             IMPORTING
                                 DATE_INTERNAL = IT_ZTPMTEDI-ZFPYDT.
                  ENDCASE.
                  MODIFY IT_ZTPMTEDI INDEX W_TABIX.
               WHEN '06'.     ">기타사항..
                  IF W_EDI_RECORD+2(03) EQ 'OSI'.
                     MOVE : W_EDI_RECORD+05(70)  TO IT_ZTPMTEDI-ZFRMK1,
                            W_EDI_RECORD+75(70)  TO IT_ZTPMTEDI-ZFRMK2,
                            W_EDI_RECORD+145(70) TO IT_ZTPMTEDI-ZFRMK3,
                            W_EDI_RECORD+215(70) TO IT_ZTPMTEDI-ZFRMK4,
                            W_EDI_RECORD+285(70) TO IT_ZTPMTEDI-ZFRMK5.
                     MODIFY IT_ZTPMTEDI INDEX W_TABIX.
                  ENDIF.
               WHEN OTHERS.
            ENDCASE.
         ENDIF.
      ENDIF.
  ENDDO.
  CLOSE DATASET    W_FILENAME.

  LOOP AT IT_ZTPMTEDI.
     W_TABIX = SY-TABIX.

     MOVE-CORRESPONDING   IT_ZTPMTEDI  TO  ZTPMTEDI.
     INSERT ZTPMTEDI.

*> FILE DELETE.
     CALL FUNCTION 'ZIM_INBOUND_DATA_BACKUP'
           EXPORTING
              FILE_NAME   =   IT_ZTPMTEDI-FILENAME
              BACK_PATH   =   BACK_PATH
           EXCEPTIONS
              NOT_FOUND   =   4
              NO_DATA     =   8.

   ENDLOOP.
   COMMIT WORK.

*>> BDC CALL.
   LOOP AT IT_ZTPMTEDI.
*>> BDC CALL.
      CALL FUNCTION 'ZIM_BDC_CALL_TRANSACTION_ZIMP2'
           EXPORTING
              ZFDHENO    =   IT_ZTPMTEDI-ZFDHENO
           TABLES
              RETURN     =   XRETURN
           EXCEPTIONS
              OTHERS     =   4.

     IF SY-SUBRC EQ 0.
        GET PARAMETER ID 'ZPPNNO'   FIELD ZTPMTEDI-ZFPNNO.
        MESSAGE S702(ZIM1) WITH ZTPMTEDI-ZFPNNO.
        PERFORM P2000_SINGLE_MAKE   TABLES  RETURN
                                    USING   'S'.
     ELSE.
        LOOP AT XRETURN.
           MOVE : XRETURN-TYPE        TO     RETURN-TYPE,
                  XRETURN-ID          TO     RETURN-ID,
                  XRETURN-NUMBER      TO     RETURN-NUMBER,
                  XRETURN-MESSAGE_V1  TO     RETURN-MESSAGE_V1,
                  XRETURN-MESSAGE_V2  TO     RETURN-MESSAGE_V2,
                  XRETURN-MESSAGE_V3  TO     RETURN-MESSAGE_V3,
                  XRETURN-MESSAGE_V4  TO     RETURN-MESSAGE_V4.

           CALL FUNCTION 'MESSAGE_TEXT_BUILD'
                     EXPORTING
                            MSGID     = RETURN-ID
                            MSGNR     = RETURN-NUMBER
                            MSGV1     = RETURN-MESSAGE_V1
                            MSGV2     = RETURN-MESSAGE_V2
                            MSGV3     = RETURN-MESSAGE_V3
                            MSGV4     = RETURN-MESSAGE_V4
                    IMPORTING
                            MESSAGE_TEXT_OUTPUT = RETURN-MESSAGE.
            APPEND  RETURN.
        ENDLOOP.
        MESSAGE S701(ZIM1) WITH IT_ZTPMTEDI-ZFDHENO.
        PERFORM P2000_SINGLE_MAKE   TABLES  RETURN
                                    USING   'W'.
     ENDIF.

*     IF SY-SUBRC EQ 0.
*        MESSAGE S702(ZIM1) WITH IT_ZTPMTHD-ZFPNNO.
*        PERFORM P2000_SINGLE_MAKE   TABLES  RETURN
*                                    USING   'S'.
*        DELETE IT_ZTPMTEDI INDEX  W_TABIX.
*     ELSE.
*        MESSAGE S701(ZIM1) WITH IT_ZTPMTHD-ZFPNNO.
*        PERFORM P2000_SINGLE_MAKE   TABLES  RETURN
*                                    USING   'E'.
*     ENDIF.
   ENDLOOP.

ENDFUNCTION.

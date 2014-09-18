FUNCTION ZIM_LG_FINBIL_EDI_DOC.
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
       W_EDI_RECORD(65535),
       W_MAILID     LIKE ZTIMIMGTX-ZFMAILID,
       W_SEQ        TYPE I,
       W_ZFDHSRO    LIKE ZTDHF1-ZFDHSRO,
       W_ZFDHREF    LIKE ZTDHF1-ZFDHREF.

*>> FINBIL HEADER INTERNAL TABLE DECLARE
DATA : BEGIN OF IT_ZTFINHD OCCURS 0.
       INCLUDE STRUCTURE ZTFINHD.
DATA : ZFDHDOC  LIKE  ZTDHF1-ZFDHDOC,
       FILENAME LIKE  ZTDHF1-FILENAME,
       END   OF IT_ZTFINHD.

*>> FINBIL ITEM INTERNAL TABLE DECLARE
DATA : BEGIN OF IT_ZTFINIT OCCURS 0.
       INCLUDE STRUCTURE ZTFINIT.
DATA : ZFDHDOC  LIKE  ZTDHF1-ZFDHDOC,
       END   OF IT_ZTFINIT.

  REFRESH : IT_ZTFINHD, IT_ZTFINIT, RETURN.
  CLEAR : L_ZFDHDOC,  W_TABIX, IT_TAB, RETURN.
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
        CLEAR: IT_ZTFINHD, IT_ZTFINIT.
*        MOVE : W_EDI_RECORD+12(12)     TO W_MAILID,
        MOVE : W_EDI_RECORD+12(03)     TO W_MAILID,
               W_EDI_RECORD+65(06)     TO L_ZFDHDOC,
               W_EDI_RECORD+65(06)     TO IT_ZTFINHD-ZFDHDOC,
               W_FILENAME              TO IT_ZTFINHD-FILENAME.
        APPEND IT_ZTFINHD.
        W_TABIX = SY-TABIX.
      ELSE.
*>> 찾지 못했을 경우, CONTINUE.
         IF L_ZFDHDOC EQ 'FINBIL'.        ">계산서.
            CASE  W_EDI_RECORD(02).
               WHEN '01'.
                  IF W_EDI_RECORD+2(03)  NE '2AR'. ">매입번호.
                     L_NOT_FOUND = 'Y'.
                     EXIT.
                  ENDIF.
               WHEN '02'.     ">계산서용도.
                  MOVE W_EDI_RECORD+5(03)       TO  IT_ZTFINHD-ZFBILCD.
                  MODIFY  IT_ZTFINHD  INDEX  W_TABIX.

                  CASE IT_ZTFINHD-ZFBILCD.
                     WHEN '2BE' OR '2BF' OR '2BG' OR '2BH' OR 'ZZZ'.
                     WHEN OTHERS.
                        L_NOT_FOUND = 'Y'.
                        EXIT.
                  ENDCASE.
               WHEN '03'.      ">참조번호.
                  IF ( W_EDI_RECORD+2(03)  EQ  'ACK' OR   ">매입번호.
                       W_EDI_RECORD+2(03)  EQ  'AAC' )    ">신용장 번호.
                       AND IT_ZTFINHD-ZFREFNO IS INITIAL.
                     MOVE : W_EDI_RECORD+5(35) TO IT_ZTFINHD-ZFREFNO,
                            W_EDI_RECORD+5(35) TO W_ZFDHREF.
                  ELSE.
                     CONTINUE.
                  ENDIF.

                  CASE IT_ZTFINHD-ZFREFNO(1).
                     WHEN 'M'.
                        CASE IT_ZTFINHD-ZFREFNO+8(2).
                           WHEN 'ES' OR  ">수출용 원자재 AT SIGHT.
                                'EU' OR  ">수출용 원자재 USANCE
                                'NU' OR  ">내수용 원자재 USNACE
                                'NS'.    ">내수용 원자재 AT SIGHT.

                           WHEN OTHERS.
                              L_NOT_FOUND = 'Y'.
                              EXIT.
                        ENDCASE.
                     WHEN 'L' OR 'D' OR 'I' OR 'P'.

                     WHEN OTHERS.
                        L_NOT_FOUND = 'Y'.
                        EXIT.
                  ENDCASE.

                  MODIFY  IT_ZTFINHD  INDEX  W_TABIX.
*>> 전자문서 번호 GET
*>>회사코드 SELECT
                  W_MAILID+3(1) = '%'.
                  SELECT * FROM  ZTIMIMGTX UP TO 1 ROWS
                                           WHERE ZFMAILID LIKE W_MAILID.
                  ENDSELECT.
                  IF SY-SUBRC NE 0.
                     MESSAGE S659 WITH W_MAILID.
                     PERFORM P2000_SINGLE_MAKE   TABLES  RETURN
                                                 USING   'E'.
                     DELETE IT_ZTFINHD INDEX W_TABIX.
                     CONTINUE.
                   ENDIF.

                   CLEAR : W_ZFDHSRO, W_ZFDHENO.
*>> 전자문서 GET!
                   CALL FUNCTION 'ZIM_EDI_NUMBER_GET_NEXT'
                        EXPORTING
                           W_ZFCDDOC = L_ZFDHDOC
                           W_ZFDHSRO = W_ZFDHSRO
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
                  MOVE W_ZFDHENO    TO  IT_ZTFINHD-ZFDHENO.
                  MODIFY  IT_ZTFINHD  INDEX  W_TABIX.

               WHEN '04'.
                  CASE W_EDI_RECORD+2(03).
                     WHEN  '97'.            ">거래일자.
                        CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
                             EXPORTING
                                 DATE_EXTERNAL = W_EDI_RECORD+5(06)
                             IMPORTING
                                 DATE_INTERNAL = IT_ZTFINHD-ZFTRDT.
                     WHEN  '137'.           ">통지일자.
                        CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
                             EXPORTING
                                 DATE_EXTERNAL = W_EDI_RECORD+5(06)
                             IMPORTING
                                 DATE_INTERNAL = IT_ZTFINHD-ZFRCDT.
                  ENDCASE.
                  MODIFY  IT_ZTFINHD  INDEX  W_TABIX.

               WHEN '05'.    ">결제방법, 어음조건.
                  MOVE : W_EDI_RECORD+5(03)   TO IT_ZTFINHD-ZFTERM,
                         W_EDI_RECORD+22(3)   TO IT_ZTFINHD-ZFNOTE.
                  MODIFY  IT_ZTFINHD  INDEX  W_TABIX.

               WHEN '07'.   ">기타정보.
                  MOVE : W_EDI_RECORD+5(70)   TO IT_ZTFINHD-ZFRMK1,
                         W_EDI_RECORD+75(70)  TO IT_ZTFINHD-ZFRMK2,
                         W_EDI_RECORD+145(70) TO IT_ZTFINHD-ZFRMK3,
                         W_EDI_RECORD+215(70) TO IT_ZTFINHD-ZFRMK4,
                         W_EDI_RECORD+285(70) TO IT_ZTFINHD-ZFRMK5.
                  MODIFY  IT_ZTFINHD  INDEX  W_TABIX.

               WHEN '08'.      ">발급은행.
                  IF W_EDI_RECORD+2(03) EQ 'BC'.
                     MOVE : W_EDI_RECORD+5(05)   TO IT_ZTFINHD-ZFBKCD,
                            W_EDI_RECORD+22(70)  TO IT_ZTFINHD-ZFBKNM.
*                            W_EDI_RECORD+93(70)  TO IT_ZTFINHD-ZFBRNM.
                     MODIFY  IT_ZTFINHD  INDEX  W_TABIX.
                 ENDIF.
              WHEN '12'.
                  CASE W_EDI_RECORD+2(03).
                     WHEN '2AD'.      ">외화.
                        MOVE : W_EDI_RECORD+5(16)  TO IT_ZTFINHD-ZFAMT,
                               W_EDI_RECORD+23(3)  TO IT_ZTFINHD-ZFCUR.
                        PERFORM  SET_CURR_CONV_TO_INTERNAL
                                 CHANGING IT_ZTFINHD-ZFAMT
                                          IT_ZTFINHD-ZFCUR.
                        MODIFY  IT_ZTFINHD  INDEX  W_TABIX.
                     WHEN '2AE'.      ">원화.
                        MOVE : W_EDI_RECORD+5(16)  TO IT_ZTFINHD-ZFMOA,
                               W_EDI_RECORD+23(3)  TO IT_ZTFINHD-ZFKRW.
                        PERFORM  SET_CURR_CONV_TO_INTERNAL
                                 CHANGING IT_ZTFINHD-ZFMOA
                                          IT_ZTFINHD-ZFKRW.
                        MODIFY  IT_ZTFINHD  INDEX  W_TABIX.
                  ENDCASE.

              WHEN '13'.
                  MOVE W_EDI_RECORD+2(12)  TO  IT_ZTFINHD-ZFEXRT.
                  MODIFY  IT_ZTFINHD  INDEX  W_TABIX.
              WHEN '15'.     ">수수료 합계.
                  IF W_EDI_RECORD+2(03)  EQ  '131'.
                     MOVE : W_EDI_RECORD+5(18)  TO IT_ZTFINHD-ZFTFEE,
                            W_EDI_RECORD+23(3)  TO IT_ZTFINHD-WAERS.
                     PERFORM  SET_CURR_CONV_TO_INTERNAL
                              CHANGING IT_ZTFINHD-ZFTFEE
                                       IT_ZTFINHD-WAERS.
                  ENDIF.
                  MODIFY  IT_ZTFINHD  INDEX  W_TABIX.

*--------------------------< ITEM LEVEL >------------------------------*
               WHEN '16'.     ">수수료 유형.
                  MOVE : IT_ZTFINHD-ZFDHENO  TO  IT_ZTFINIT-ZFDHENO,
                         W_EDI_RECORD+40(03) TO  IT_ZTFINIT-ZFCD.
                  APPEND IT_ZTFINIT.
                  MOVE  SY-TABIX      TO  W_IT_INDEX.

               WHEN '17'.     ">RATE.
                  MOVE : W_EDI_RECORD+5(08)  TO  IT_ZTFINIT-ZFRATE.
                  MODIFY IT_ZTFINIT  INDEX  W_IT_INDEX.

               WHEN '18'.     ">
                  CASE W_EDI_RECORD+2(03).
                     WHEN '25'.       ">대상금액.
                        MOVE:W_EDI_RECORD+5(18) TO IT_ZTFINIT-ZFAMT,
                             W_EDI_RECORD+23(3) TO IT_ZTFINIT-WAERS.
                        PERFORM  SET_CURR_CONV_TO_INTERNAL
                                 CHANGING IT_ZTFINIT-ZFAMT
                                          IT_ZTFINIT-WAERS.
                     WHEN '23'.       ">산출금액.
                        MOVE:W_EDI_RECORD+5(18) TO IT_ZTFINIT-ZFFEE,
                             W_EDI_RECORD+23(3) TO IT_ZTFINIT-ZFKRW.
                        PERFORM  SET_CURR_CONV_TO_INTERNAL
                                 CHANGING IT_ZTFINIT-ZFFEE
                                          IT_ZTFINIT-ZFKRW.
                     WHEN OTHERS.
                  ENDCASE.
                  MODIFY IT_ZTFINIT  INDEX  W_IT_INDEX.

               WHEN '19'.    ">환율.
                  MOVE W_EDI_RECORD+2(12)  TO  IT_ZTFINIT-ZFEXRT.
                  MODIFY IT_ZTFINIT  INDEX  W_IT_INDEX.

               WHEN '20'.     ">적용일수.
                  CASE W_EDI_RECORD+2(03).
                     WHEN '221'.     ">적용일수.
                        MOVE W_EDI_RECORD+5(03)   TO  IT_ZTFINIT-ZFDAY.

                     WHEN '257'.     ">적용기간.
                        CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
                             EXPORTING
                                 DATE_EXTERNAL = W_EDI_RECORD+5(06)
                             IMPORTING
                                 DATE_INTERNAL = IT_ZTFINIT-ZFFROM.

                        CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
                             EXPORTING
                                 DATE_EXTERNAL = W_EDI_RECORD+11(06)
                             IMPORTING
                                 DATE_INTERNAL = IT_ZTFINIT-ZFEND.

                  ENDCASE.
                  MODIFY IT_ZTFINIT  INDEX  W_IT_INDEX.
               WHEN OTHERS.
            ENDCASE.
         ENDIF.
      ENDIF.
  ENDDO.

  CLOSE DATASET    W_FILENAME.
  IF L_NOT_FOUND = 'Y'.
     MESSAGE S703(ZIM1) WITH IT_ZTFINHD-ZFBILCD W_FILENAME.
     PERFORM P2000_SINGLE_MAKE   TABLES  RETURN
                                 USING   'E'.
     EXIT.
  ENDIF.

  LOOP AT IT_ZTFINHD.

      W_TABIX = SY-TABIX.
      CLEAR : ZTFINHD, W_SEQ.

      MOVE-CORRESPONDING  IT_ZTFINHD  TO  ZTFINHD.
      MOVE                'N'         TO  ZTFINHD-ZFDBYN.

      INSERT  ZTFINHD.
      IF SY-SUBRC NE 0.
         MESSAGE S658 WITH IT_ZTFINHD-ZFREFNO.
         PERFORM P2000_SINGLE_MAKE   TABLES  RETURN
                                     USING   'E'.
      ENDIF.

*--------------------------< ITEM 내역 반영 >--------------------------*
      LOOP AT IT_ZTFINIT WHERE ZFDHENO EQ  IT_ZTFINHD-ZFDHENO.

         W_SEQ  =  W_SEQ + 10.
         MOVE-CORRESPONDING  IT_ZTFINIT  TO  ZTFINIT.
         MOVE  W_SEQ     TO   ZTFINIT-ZFSEQ.
         INSERT  ZTFINIT.
         IF SY-SUBRC NE 0.
            MESSAGE S658 WITH IT_ZTFINHD-ZFREFNO.
            PERFORM P2000_SINGLE_MAKE   TABLES  RETURN
                                        USING   'E'.
         ENDIF.

      ENDLOOP.

      SELECT SINGLE * FROM ZTDHF1
                      WHERE ZFDHENO EQ IT_ZTFINHD-ZFDHENO.
      MOVE : IT_ZTFINHD-FILENAME TO ZTDHF1-FILENAME.
      UPDATE ZTDHF1.

      MESSAGE S700(ZIM1) WITH IT_ZTFINHD-ZFDHENO.
      PERFORM P2000_SINGLE_MAKE   TABLES  RETURN
                                  USING   'S'.
*> FILE DELETE.
      CALL FUNCTION 'ZIM_INBOUND_DATA_BACKUP'
           EXPORTING
              FILE_NAME   =   IT_ZTFINHD-FILENAME
              BACK_PATH   =   BACK_PATH
           EXCEPTIONS
              NOT_FOUND   =   4
              NO_DATA     =   8.

   ENDLOOP.

ENDFUNCTION.

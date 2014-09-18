FUNCTION ZIM_BDC_CALL_TRANSACTION_ZIMP2.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFDHENO) LIKE  ZTPMTEDI-ZFDHENO
*"     VALUE(MODE) TYPE  C DEFAULT 'N'
*"  EXPORTING
*"     VALUE(ZFPNNO) LIKE  ZTPMTHD-ZFPNNO
*"  TABLES
*"      RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"  EXCEPTIONS
*"      CREATE_ERROR
*"----------------------------------------------------------------------
DATA : W_ZTRED   LIKE  ZTRED,
       W_AMOUNT(19),
       W_ZFEXRT(12),
       W_FFACT(11),
       W_ZFUSIRP LIKE  ZTPMTHD-ZFUSIRP,
       W_ZFBKCHP LIKE  ZTPMTHD-ZFBKCHP,
       W_ZSREQHD LIKE  ZSREQHD,
       W_ZFUSITR(08).

   CLEAR : ZFPNNO.


   SELECT SINGLE * FROM ZTPMTEDI
          WHERE ZFDHENO  EQ  ZFDHENO.
   IF SY-SUBRC NE 0.
      RAISE CREATE_ERROR.
   ENDIF.

   IF ZTPMTEDI-ZFDBYN EQ 'Y'.
*      MESSAGE
      RAISE CREATE_ERROR.
   ENDIF.

   MOVE ZTPMTEDI TO *ZTPMTEDI.

   SELECT * FROM  ZTREQST
            WHERE ZFOPNNO EQ ZTPMTEDI-ZFOPNNO.
   ENDSELECT.

   IF SY-SUBRC NE 0.
      RAISE CREATE_ERROR.
   ENDIF.

   REFRESH : BDCDATA, RETURN.
   PERFORM P2000_DYNPRO USING :
          'X' 'SAPMZIM01'       '8100',
          ' ' 'W_EBELN'         SPACE,
          ' ' 'W_ZFOPNNO'       SPACE,
          ' ' 'W_ZFREQNO'       ZTREQST-ZFREQNO,
          ' ' 'ZTRED-ZFREDNO'   SPACE,
          ' ' 'ZTRED-ZFISNO'    SPACE,
          ' ' 'BDC_OKCODE'     '=ENTR'.


   PERFORM P2000_DYNPRO USING :
           'X' 'SAPMZIM01'        '8210'.
*>결제완료일.
   IF NOT ZTPMTEDI-ZFPYDT IS INITIAL.
      PERFORM P2000_DYNPRO USING :
         ' ' 'ZTPMTHD-ZFPYDT'   ZTPMTEDI-ZFPYDT.   ">결제완료일.
   ENDIF.
*>통지일자.
   IF NOT ZTPMTEDI-ZFNTDT IS INITIAL.
      PERFORM P2000_DYNPRO USING :
         ' ' 'ZTPMTHD-ZFNTDT'   ZTPMTEDI-ZFNTDT.   ">통지일자.
   ENDIF.
*>할인일.
   IF NOT ZTPMTEDI-ZFDSDT IS INITIAL.
      PERFORM P2000_DYNPRO USING :
         ' ' 'ZTPMTHD-ZFDSDT'   ZTPMTEDI-ZFDSDT.   ">할인일.
   ENDIF.
*>만기일.
   IF NOT ZTPMTEDI-ZFPWDT IS INITIAL.
      PERFORM P2000_DYNPRO USING :
          ' ' 'ZTPMTHD-ZFPWDT'   ZTPMTEDI-ZFPWDT.   ">만기일.
   ENDIF.
*>NEGO일.
   IF NOT ZTPMTEDI-NEGODT IS INITIAL.
      PERFORM P2000_DYNPRO USING :
          ' ' 'ZTPMTHD-NEGODT'   ZTPMTEDI-NEGODT.   ">NEGO일.
   ENDIF.

*> 기타 수수료.
   IF NOT ZTPMTEDI-ZFBKCH IS INITIAL.
      WRITE : ZTPMTEDI-ZFBKCH TO W_AMOUNT CURRENCY ZTPMTEDI-ZFBKCHC.
      PERFORM P2000_DYNPRO USING :
          ' ' 'ZTPMTHD-ZFBKCHC'  ZTPMTEDI-ZFBKCHC,  "Notice Currency
          ' ' 'ZTPMTHD-ZFBKCH'   W_AMOUNT.          ">기타수수료.
   ENDIF.

*<<<<<<<<<<<<<<<<< 문서 종류에 따라 >>>>>>>>>>>>>>>>>>>>
   CASE ZTPMTEDI-ZFDHDOC.
      WHEN 'DOANTC'.
         WRITE : ZTPMTEDI-ZFPNAM TO W_AMOUNT CURRENCY ZTPMTEDI-ZFPNAMC.
         PERFORM P2000_DYNPRO USING :
            ' ' 'ZTPMTHD-ZFPNAMC'  ZTPMTEDI-ZFPNAMC,  "Notice Currency
            ' ' 'ZTPMTHD-ZFPNAM'   W_AMOUNT.          "Notice AMOUNT

      WHEN 'DISCHG'.
*> 원금.
         CLEAR : W_ZFBKCHP, W_ZFUSIRP.
         IF NOT ZTPMTEDI-ZFPNAM IS INITIAL.
            IF NOT ZTPMTEDI-ZFUSIT IS INITIAL.
               W_ZFBKCHP = 'Y'.
               W_ZFUSIRP = 'Y'.
            ELSE.
               W_ZFBKCHP = 'N'.
            ENDIF.
         ELSE.
            W_ZFUSIRP = 'N'.
         ENDIF.

         IF NOT ZTPMTEDI-ZFPNAM IS INITIAL.

            WRITE : ZTPMTEDI-ZFPNAM TO W_AMOUNT
                    CURRENCY ZTPMTEDI-ZFPNAMC.
            PERFORM P2000_DYNPRO USING :
               ' ' 'ZTPMTHD-ZFPNAMC'  ZTPMTEDI-ZFPNAMC,  "Currency
               ' ' 'ZTPMTHD-ZFPNAM'   W_AMOUNT,          "Notice
               ' ' 'ZTPMTHD-ZFBKCHP'  W_ZFBKCHP.

         ENDIF.
*> 이자금액.
         IF NOT ZTPMTEDI-ZFUSIT IS INITIAL.
            WRITE : ZTPMTEDI-ZFUSIT TO W_AMOUNT
                    CURRENCY ZTPMTEDI-ZFUSITC.
            PERFORM P2000_DYNPRO USING :
               ' ' 'ZTPMTHD-ZFUSITC'  ZTPMTEDI-ZFUSITC,  "Currency
               ' ' 'ZTPMTHD-ZFUSIT'   W_AMOUNT.          "Notice
         ENDIF.
         IF NOT ZTPMTEDI-ZFUSITR  IS INITIAL.
            WRITE ZTPMTEDI-ZFUSITR TO W_ZFUSITR.
            PERFORM P2000_DYNPRO USING :
               ' ' 'ZTPMTHD-ZFUSITR'  W_ZFUSITR.         "Rate.
         ENDIF.

*> 선취/후취 여부...
         PERFORM P2000_DYNPRO USING :
            ' ' 'ZTPMTHD-ZFBKCHP'  W_ZFBKCHP,          "Currency
            ' ' 'ZTPMTHD-ZFUSIRP'  W_ZFUSIRP.          "Notice

      WHEN 'LDANTC'.

         WRITE : ZTPMTEDI-ZFPNAM TO W_AMOUNT CURRENCY ZTPMTEDI-ZFPNAMC.
         PERFORM P2000_DYNPRO USING :
            ' ' 'ZTPMTHD-ZFPNAMC'  ZTPMTEDI-ZFPNAMC,  "Notice Currency
            ' ' 'ZTPMTHD-ZFPNAM'   W_AMOUNT.          "Notice AMOUNT
*> 원화금액.
         IF NOT ZTPMTEDI-ZFMOA IS INITIAL.
            WRITE : ZTPMTEDI-ZFMOA TO W_AMOUNT
                    CURRENCY ZTPMTEDI-WAERS.
            PERFORM P2000_DYNPRO USING :
               ' ' 'ZTPMTHD-ZFKRW'    ZTPMTEDI-WAERS,  " Currency
               ' ' 'ZTPMTHD-ZFPNAM'   W_AMOUNT.          " AMOUNT
         ENDIF.

*> 환율.
         IF NOT ZTPMTEDI-ZFEXRT IS INITIAL.
            CALL FUNCTION 'ZIM_GET_EXCHANGE_RATE'
                 EXPORTING
                    P_WAERS        =   ZTPMTEDI-ZFPNAMC
                    P_DATE         =   SY-DATUM
                    P_KURST        =   'M'
                    P_TO_WAERS     =   'KRW'
                 IMPORTING
*                    P_EXRT         =   ZTCIVHD-ZFEXRT
                    P_FFACT        =   ZTPMTHD-FFACT
                 EXCEPTIONS
                    NO_INPUT       = 4
                    OTHERS         = 6.

             CASE SY-SUBRC.
                WHEN 4.
                   MESSAGE S094.
                   RAISE   REQ_ERROR.
                WHEN 6.
                   MESSAGE S094.
                   RAISE   REQ_ERROR.
                WHEN OTHERS.
             ENDCASE.

             IF ZTPMTHD-FFACT IS INITIAL.
                ZTPMTHD-FFACT = 1.
             ENDIF.

             ZTPMTEDI-ZFEXRT = ZTPMTEDI-ZFEXRT * ZTPMTHD-FFACT.
             WRITE : ZTPMTEDI-ZFEXRT TO W_ZFEXRT,
                     ZTPMTHD-FFACT   TO W_FFACT.

             PERFORM P2000_DYNPRO USING :
                ' ' 'ZTPMTHD-ZFEXRT'  W_ZFEXRT,  " 환율.
                ' ' 'ZTPMTHD-FFACT'   W_FFACT.
         ENDIF.

   ENDCASE.

   PERFORM P2000_DYNPRO USING :
          ' ' 'ZTPMTHD-ZFRMK1'   ZTPMTEDI-ZFRMK1,
          ' ' 'ZTPMTHD-ZFRMK2'   ZTPMTEDI-ZFRMK2,
          ' ' 'ZTPMTHD-ZFRMK3'   ZTPMTEDI-ZFRMK3,
          ' ' 'ZTPMTHD-ZFRMK4'   ZTPMTEDI-ZFRMK4,
          ' ' 'ZTPMTHD-ZFRMK5'   ZTPMTEDI-ZFRMK5,
          ' ' 'BDC_OKCODE'       '=SAVE'.

   PERFORM P2000_DYNPRO USING :
          'X' 'SAPMZIM01'        '0001',
          ' ' 'BDC_OKCODE'       '=YES'.

   SET PARAMETER ID 'ZPPNNO'   FIELD ''.
*>> BDC CALL.
   REFRESH:MESSTAB.
   CALL TRANSACTION 'ZIMP2'  USING       BDCDATA
*                             MODE        'N'
                             MODE        MODE
                             UPDATE      'V'
*                             UPDATE      'S'
                             MESSAGES    INTO   MESSTAB.

  GET PARAMETER ID 'ZPPNNO'   FIELD ZFPNNO.

  W_SUBRC = SY-SUBRC.

  IF W_SUBRC NE 0.      ">> ERROR 발생시.
     LOOP AT MESSTAB.
        MOVE : MESSTAB-MSGTYP  TO     RETURN-TYPE,
               MESSTAB-MSGID   TO     RETURN-ID,
               MESSTAB-MSGNR   TO     RETURN-NUMBER,
               MESSTAB-MSGV1   TO     RETURN-MESSAGE_V1,
               MESSTAB-MSGV2   TO     RETURN-MESSAGE_V2,
               MESSTAB-MSGV3   TO     RETURN-MESSAGE_V3,
               MESSTAB-MSGV4   TO     RETURN-MESSAGE_V4.

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
      ROLLBACK WORK.
      RAISE   REQ_ERROR.
   ELSE.
      IF ZFPNNO IS INITIAL.
         ROLLBACK WORK.
         RAISE   REQ_ERROR.
      ELSE.
         MOVE : ZFPNNO    TO     ZTPMTEDI-ZFPNNO,
                'Y'       TO     ZTPMTEDI-ZFDBYN,
                SY-DATUM  TO     ZTPMTEDI-ZFDBDT,
                SY-UZEIT  TO     ZTPMTEDI-ZFDBTM,
                SY-UNAME  TO     ZTPMTEDI-ZFDBID.
         UPDATE ZTPMTEDI.
         COMMIT WORK.
      ENDIF.
   ENDIF.

ENDFUNCTION.

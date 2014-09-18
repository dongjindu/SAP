*&---------------------------------------------------------------------*
*& Report  ZRIMAPP707                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : L/C 조건변경신청서                                    *
*&      작성자 : 이채경 INFOLINK Ltd.                                  *
*&      작성일 : 2001.07.09                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :  L/C 조건변경신청서.                                  *
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMAPP707   MESSAGE-ID ZIM
                     LINE-SIZE 116
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* L/C 입수내역 리스트용 TABLE
*-----------------------------------------------------------------------
DATA : W_MAX_ZFAMDNO LIKE ZTREQST-ZFAMDNO,
       W_ERR_CHK(1),
       W_DOM_TEX1 LIKE DD07T-DDTEXT,
       W_COUNT    LIKE SY-TABIX,
       W_DOM_TEX2 LIKE DD07T-DDTEXT.

DATA : BEGIN  OF    IT_ZTMLCSG7G OCCURS 100,
       ZFREQNO  LIKE ZTMLCSG7G-ZFREQNO,   " 수입의뢰 관리번호.
       ZFLSG7G  LIKE ZTMLCSG7G-ZFLSG7G,   " 반복수 Seg 7 상품명세.
       ZFDSOG1   LIKE ZTMLCSG7G-ZFDSOG1,  " 상품용역명세.
END OF IT_ZTMLCSG7G.

DATA:  BEGIN OF  IT_ZTMLCSG8E OCCURS 100,
       ZFREQNO  LIKE ZTMLCSG8E-ZFREQNO,
       ZFLSG8E  LIKE ZTMLCSG8E-ZFLSG8E,
       ZFOACD1  LIKE ZTMLCSG8E-ZFOACD1,
END OF IT_ZTMLCSG8E.

DATA:  BEGIN OF IT_ZTMLCSG9O OCCURS 10,
       ZFREQNO  LIKE ZTMLCSG9O-ZFREQNO,
       ZFLSG9O  LIKE ZTMLCSG9O-ZFLSG9O,
       ZFODOC1  LIKE ZTMLCSG9O-ZFODOC1,
END OF IT_ZTMLCSG9O.
DATA:  BEGIN OF IT_ZTMLCAMNARR OCCURS 10,
       ZFREQNO  LIKE  ZTMLCAMNARR-ZFREQNO,
       ZFAMDNO  LIKE  ZTMLCAMNARR-ZFAMDNO,
       ZFLNARR  LIKE  ZTMLCAMNARR-ZFLNARR,
       ZFNARR   LIKE  ZTMLCAMNARR-ZFNARR,
       ZFFIELD  LIKE  ZTMLCAMNARR-ZFFIELD,
END OF IT_ZTMLCAMNARR.

DATA:  BEGIN OF IT_ZTREQIL OCCURS 0,
       ZFREQNO  LIKE  ZTREQIL-ZFREQNO, " 수입의뢰 관리번호.
       ZFILSEQ  LIKE  ZTREQIL-ZFILSEQ, " 반복수 수입추천.
       ZFRECNO  LIKE  ZTREQIL-ZFRECNO, " 수입추천번호.
       ZFRECAM  LIKE  ZTREQIL-ZFRECAM, " 수입추천 금액.
       ZFRECCU  LIKE  ZTREQIL-ZFRECCU, " 	통화키.
END OF IT_ZTREQIL.


*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
TABLES : ZTREQHD,ZTREQIT,LFA1,ZTMLCAMHD,ZTREQST,ZTMLCHD,ZTMLCSG2,
         ZTMLCSG910,ZTMLCSG7G,ZTMLCSG9O,DD07T.

*---------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   PARAMETERS    : P_REQNO   LIKE ZTREQHD-ZFREQNO,
                   P_AMDNO   LIKE ZTREQST-ZFAMDNO.
SELECTION-SCREEN END OF BLOCK B1.


* PARAMETER 초기값 Setting
INITIALIZATION.                          " 초기값 SETTING
    PERFORM   P2000_SET_PARAMETER.

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.
* 권한 검증 함?
    PERFORM   P2000_AUTHORITY_CHECK     USING   W_ERR_CHK.
    IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
*  테이블 SELECT
    PERFORM   P1000_GET_IT_TAB           USING W_ERR_CHK.
    IF W_ERR_CHK EQ 'Y'.
      MESSAGE S966.  EXIT.
    ENDIF.
* 레포트 Write
    PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.
    IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

*  GET PARAMETER ID 'ZPREQNO' FIELD ZTREQHD-ZFREQNO.
*  GET PARAMETER ID 'ZPAMDNO' FIELD ZTREQST-ZFAMDNO.
*  P_REQNO = ZTREQHD-ZFREQNO.
*  P_AMDNO = ZTREQST-ZFAMDNO.

  SET  TITLEBAR 'APP707'.          " TITLE BAR

ENDFORM.                    " P2000_SET_PARAMETER

*&---------------------------------------------------------------------*
*&      Form  P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_AUTHORITY_CHECK          USING   W_ERR_CHK.

*   W_ERR_CHK = 'N'.
*-----------------------------------------------------------------------
*  해당 화면 AUTHORITY CHECK
*-----------------------------------------------------------------------
*   AUTHORITY-CHECK OBJECT 'ZI_BL_MGT'
*           ID 'ACTVT' FIELD '*'.

*   IF SY-SUBRC NE 0.
*      MESSAGE S960 WITH SY-UNAME 'B/L Doc Transaction'.
*      W_ERR_CHK = 'Y'.   EXIT.
*   ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK


*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

  SET PF-STATUS 'APP707'.           " GUI STATUS SETTING
  SET  TITLEBAR 'APP707'.           " GUI TITLE SETTING..
  PERFORM P3000_LINE_WRITE.

ENDFORM.                    " P3000_DATA_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

DATA: L_TEXT(5), L_TEXT1(5), L_TEXT2(12).

  SKIP 2.
  WRITE:/33'(Irrevocable Documentary Credit Amendment Application)'.

  SKIP 2.
  WRITE:/ SY-ULINE,
        / SY-VLINE,' ', 116 SY-VLINE,
        / SY-VLINE,8 'Exept so far as otherwise expressly,this',
                     'documentarry credit is',
                     'subject to the',      116 SY-VLINE,
        / SY-VLINE,8 "Uniform Customs and Pratice for',
                     'Documentary Credits"','(1993 Revision)',
                     'International Chamber of Commerce',
                    '(Publication No.500)',116 SY-VLINE,
        / SY-VLINE,                        116 SY-VLINE.
  WRITE:/ SY-ULINE.
  SKIP 2.
  WRITE:/8 'Document/EDI No:',ZTREQST-ZFDOCNO.
  SKIP 2.
  ULINE AT /1(47). WRITE: '< General info >'.ULINE AT 67(116).
  SKIP 1.
* DOMAIN.-----------------------------------------------------------
  PERFORM  GET_DD07T_SELECT USING 'ZDOPME' ZTMLCAMHD-ZFOPME
                            CHANGING   W_DOM_TEX1.
*--------------------------------------------------------------------

  WRITE:/8 'Condition amend date  :',ZTREQST-ZFRVDT,
        /8 'Opening method        :',W_DOM_TEX1,
        /8 'Opening(request) bank :',ZTMLCHD-ZFOBNM,
        /8 '                       ',ZTMLCHD-ZFOBBR,
        /8 '                       ','[Tel No.]',ZTMLCHD-ZFOBPH,
        /8 'Advising bank         :',ZTMLCHD-ZFABNM,ZTMLCHD-ZFABBR,
        /8 'Opening applicant     :',ZTMLCSG2-ZFAPPNM,
                                     ZTMLCSG2-ZFAPPAD1,
        /40 ZTMLCSG2-ZFAPPAD2,
        /40 ZTMLCSG2-ZFAPPAD3,
        /40 ZTMLCSG2-ZFELEAD1,
        /40 ZTMLCSG2-ZFELEAD2,
        /40 '[Tel No.]',ZTMLCSG2-ZFTELNO.
*>> 수입추천번호.
  W_COUNT = 0.
  LOOP AT IT_ZTREQIL.
       IF IT_ZTREQIL-ZFRECNO IS INITIAL.
          CONTINUE.
       ENDIF.
       ADD 1 TO W_COUNT.
       IF W_COUNT EQ 1.
          WRITE: /8 'Import license No(amount):',40 IT_ZTREQIL-ZFRECNO,
                IT_ZTREQIL-ZFRECCU RIGHT-JUSTIFIED,
          IT_ZTREQIL-ZFRECAM CURRENCY IT_ZTREQIL-ZFRECCU LEFT-JUSTIFIED.
        ELSE.
          WRITE:/40 IT_ZTREQIL-ZFRECNO,
                    IT_ZTREQIL-ZFRECCU RIGHT-JUSTIFIED,
          IT_ZTREQIL-ZFRECAM CURRENCY IT_ZTREQIL-ZFRECCU LEFT-JUSTIFIED.
       ENDIF.
  ENDLOOP.

  IF  NOT ZTMLCAMHD-ZFETC1 IS INITIAL
   OR NOT ZTMLCAMHD-ZFETC2 IS INITIAL
   OR NOT ZTMLCAMHD-ZFETC3 IS INITIAL
   OR NOT ZTMLCAMHD-ZFETC4 IS INITIAL.
      WRITE:/8 'Other info :',ZTMLCAMHD-ZFETC1.
  ENDIF.
  IF NOT ZTMLCAMHD-ZFETC2 IS INITIAL.
     WRITE:/21 ZTMLCAMHD-ZFETC2.
  ENDIF.
  IF NOT ZTMLCAMHD-ZFETC3 IS INITIAL.
     WRITE:/21 ZTMLCAMHD-ZFETC3.
  ENDIF.
  IF NOT ZTMLCAMHD-ZFETC4 IS INITIAL.
     WRITE:/21 ZTMLCAMHD-ZFETC4.
  ENDIF.
  IF NOT ZTMLCAMHD-ZFETC5 IS INITIAL.
     WRITE:/21 ZTMLCAMHD-ZFETC5.
  ENDIF.

  ULINE AT /1(47). WRITE: '<    Swift    >'.ULINE AT 67(116).
  SKIP 1.
* DOMAIN.-----------------------------------------------------------
  CLEAR: W_DOM_TEX1.
  PERFORM  GET_DD07T_SELECT USING 'ZDLCTY' ZTMLCHD-ZFLCTY
                                   CHANGING   W_DOM_TEX1.
*--------------------------------------------------------------------

  WRITE:/8 '20 ','Sender''s Reference(L/C No):',ZTREQST-ZFOPNNO,
        /8 '31C','Date of Issue             :',ZTREQST-ZFOPNDT,
        /8 '26E','Number of Amendment       :',ZTMLCAMHD-ZFAMDNO,
        /8 '59 ','Beneficiary               :',ZTMLCAMHD-ZFBENI1,
        /12'(Before this amendment)   :',ZTMLCAMHD-ZFBENI2,
        /40                                    ZTMLCAMHD-ZFBENI3,
        /40 ZTMLCAMHD-ZFBENI4,
        /40 '[Account number]',ZTMLCAMHD-ZFBENIA.
  IF NOT ZTMLCAMHD-ZFNEXDT IS INITIAL.
     WRITE:
        /8 '31E','New Data of Expiry        :',36 ZTMLCAMHD-ZFNEXDT.
  ENDIF.

  IF ZTMLCAMHD-ZFIDCD EQ '+'.
     WRITE:/8 '32B','Increase of Documentary Credit Amount       :',
           ZTMLCAMHD-WAERS RIGHT-JUSTIFIED,
           ZTMLCAMHD-ZFIDAM  CURRENCY ZTMLCAMHD-WAERS LEFT-JUSTIFIED.
  ENDIF.
  IF ZTMLCAMHD-ZFIDCD EQ '-'.
     WRITE:/8 '33B','Decrease of Documentary Credit Amount       :',
            ZTMLCAMHD-WAERS RIGHT-JUSTIFIED,
            ZTMLCAMHD-ZFIDAM CURRENCY ZTMLCAMHD-WAERS LEFT-JUSTIFIED.
  ENDIF.
  IF NOT ZTMLCAMHD-ZFIDCD IS INITIAL.
     WRITE:/8 '34B','New Documentary Credit Amount After Amendent:',
             ZTMLCAMHD-WAERS RIGHT-JUSTIFIED,
             ZTMLCAMHD-ZFNDAMT CURRENCY ZTMLCAMHD-WAERS LEFT-JUSTIFIED.
  ENDIF.

  CASE ZTMLCAMHD-ZFALCQ.
    WHEN 'T'.
       IF NOT ZTMLCAMHD-ZFALCP IS INITIAL.
          L_TEXT   = ZTMLCAMHD-ZFALCP. CONDENSE L_TEXT NO-GAPS.
          CONCATENATE '＋' L_TEXT '%' INTO L_TEXT.
       ENDIF.
       IF NOT ZTMLCAMHD-ZFALCM IS INITIAL.
          L_TEXT1  = ZTMLCAMHD-ZFALCM. CONDENSE L_TEXT1 NO-GAPS.
          CONCATENATE '－' L_TEXT1 '%' INTO L_TEXT1.
       ENDIF.
*       L_TEXT1 = ZTMLCHD-ZFALCP. CONDENSE L_TEXT1 NO-GAPS.
       CONCATENATE L_TEXT  L_TEXT1 INTO L_TEXT2
                   SEPARATED BY SPACE.

       WRITE: /8 '39A','Percentage Credit Amount Tolerance :',
               L_TEXT2.
    WHEN 'X'.
       WRITE: /8 '39B','Maximum Credit Amount     :'."ZTMLCAMHD-ZFALCP.
    WHEN '2AA'.
       WRITE: /8 '39C','Up to Credit Amount      :'."ZTMLCAMHD-ZFALCP.
    WHEN '2AB'.
       WRITE:/8  '39C','Not Exceeding Credit Amount :'."ZTMLCAHD-ZFALCP.
    WHEN OTHERS.

  ENDCASE.

*  CLEAR: W_DOM_TEX1.
*  PERFORM  GET_DD07T_SELECT USING 'ZDALCQ' ZTMLCAMHD-ZFALCQ
*                            CHANGING   W_DOM_TEX1.
*  WRITE:/8 '39B','Maximum Credit Amount     :',W_DOM_TEX1.
  IF NOT ZTMLCAMHD-ZFNAMT1  IS INITIAL
   OR NOT ZTMLCAMHD-ZFNAMT2 IS INITIAL
   OR NOT ZTMLCAMHD-ZFNAMT3 IS INITIAL
   OR NOT ZTMLCAMHD-ZFNAMT4 IS INITIAL.
   WRITE:/8 '39C','Additional Amounts Covered:','INTEREST:'.
  ENDIF.
  IF NOT ZTMLCAMHD-ZFNAMT1 IS INITIAL.
     WRITE:40 ZTMLCAMHD-ZFNAMT1.
  ENDIF.
  IF NOT ZTMLCAMHD-ZFNAMT2 IS INITIAL.
     WRITE:/40 ZTMLCAMHD-ZFNAMT2.
  ENDIF.
  IF NOT ZTMLCAMHD-ZFNAMT3 IS INITIAL.
     WRITE:/40 ZTMLCAMHD-ZFNAMT3.
  ENDIF.
  IF NOT ZTMLCAMHD-ZFNAMT4 IS INITIAL.
     WRITE:/40 ZTMLCAMHD-ZFNAMT4.
  ENDIF.
  WRITE:/8 '44A','Loading on Board/Dispatch/Taking in Charge at/from..:'
       ,/40 ZTMLCAMHD-ZFNSPRT,
        /8 '44B','For Transportaion to...   : ',
         40 ZTMLCAMHD-ZFNAPRT.
*>> 최종선적일.
  IF NOT ZTMLCAMHD-ZFNLTSD IS INITIAL.
     WRITE:/8 '44C','Latest Date of Shipment   :', 40 ZTMLCAMHD-ZFNLTSD.
  ELSE.
     IF NOT ZTMLCAMHD-ZFNSHPR1 IS INITIAL.
        WRITE:/8 '44D',40 ZTMLCAMHD-ZFNSHPR1.
     ENDIF.
     IF NOT ZTMLCAMHD-ZFNSHPR2 IS INITIAL.
        WRITE:/40 ZTMLCAMHD-ZFNSHPR2.
     ENDIF.
     IF NOT ZTMLCAMHD-ZFNSHPR3 IS INITIAL.
        WRITE:/40 ZTMLCAMHD-ZFNSHPR3.
     ENDIF.
  ENDIF.
*>> NARRATIVE.
  LOOP AT IT_ZTMLCAMNARR.
     IF SY-TABIX EQ 1.
        WRITE:/8 '79 ','Narrative:'.
     ENDIF.
        WRITE:/12 IT_ZTMLCAMNARR-ZFNARR.
  ENDLOOP.
  SKIP 1.
  ULINE AT /1(42). WRITE: '< Electronic signature >'.ULINE AT 70(116).
  SKIP 1.
  WRITE:/24 'Applicant electronic signature:',ZTMLCSG2-ZFELENM,
        /44  ZTMLCSG2-ZFREPRE,
        /44  ZTMLCSG2-ZFELEID,
        /37 '[Address]',   ZTMLCSG2-ZFELEAD1,
                    /44 ZTMLCSG2-ZFELEAD2.


*  WRITE:/ SY-ULINE,
*        / SY-VLINE,' ', 116 SY-VLINE,
*        / SY-VLINE,8 '이 전자문서는 무역법자동화촉진에 관하법률 제2조',
*                     '제7호,제10조 제1항 및 동법 시행령 ',
*                      116 SY-VLINE,
*        / SY-VLINE,8 '제12조에 의거 발행된 전자문서입니다.',
*                                           116 SY-VLINE,
*        / SY-VLINE,                        116 SY-VLINE.
*  WRITE:/ SY-ULINE.

ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P1000_GET_IT_TAB
*&---------------------------------------------------------------------*
FORM P1000_GET_IT_TAB USING W_ERR_CHK.

   W_ERR_CHK = 'N'.
   SELECT SINGLE *
     FROM ZTMLCAMHD
    WHERE ZFREQNO =  P_REQNO
      AND ZFAMDNO =  P_AMDNO.

   IF SY-SUBRC NE 0.
      W_ERR_CHK = 'Y'.
   ENDIF.
*>>수입의뢰 Status
   CLEAR ZTREQST.
   SELECT SINGLE *
     FROM ZTREQST
    WHERE ZFREQNO = P_REQNO
      AND ZFAMDNO = P_AMDNO.

   SELECT SINGLE *
      FROM ZTMLCHD
      WHERE ZFREQNO = P_REQNO.
   MOVE ZTMLCHD TO ZTMLCHD.

   CLEAR ZTMLCSG2.
   SELECT SINGLE *
      FROM ZTMLCSG2
      WHERE ZFREQNO = P_REQNO.


   CLEAR ZTMLCSG910.
   SELECT SINGLE *
      FROM  ZTMLCSG910
      WHERE ZFREQNO = P_REQNO.

   SELECT SINGLE *
      FROM  ZTMLCSG2
      WHERE ZFREQNO = P_REQNO.

   REFRESH: IT_ZTMLCAMNARR.
   CLEAR:IT_ZTMLCAMNARR.

   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTMLCAMNARR
      FROM  ZTMLCAMNARR
      WHERE ZFREQNO = P_REQNO
        AND ZFAMDNO = P_AMDNO.

*>> 수입추천번호.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTREQIL
      FROM  ZTREQIL
      WHERE ZFREQNO = P_REQNO.

ENDFORM.                    " P1100_GET_IT_TAB
*&---------------------------------------------------------------------*
*&      Form  GET_DD07T_SELECT
*&---------------------------------------------------------------------*
FORM GET_DD07T_SELECT USING    P_DOMNAME
                               P_FIELD
                      CHANGING P_W_NAME.
  CLEAR : DD07T, P_W_NAME.
  IF P_FIELD IS INITIAL.   EXIT.   ENDIF.

  SELECT * FROM DD07T WHERE DOMNAME     EQ P_DOMNAME
                      AND   DDLANGUAGE  EQ SY-LANGU
                      AND   AS4LOCAL    EQ 'A'
                      AND   DOMVALUE_L  EQ P_FIELD
                      ORDER BY AS4VERS DESCENDING.
    EXIT.
  ENDSELECT.
*   TRANSLATE DD07T-DDTEXT TO UPPER CASE.
  P_W_NAME   = DD07T-DDTEXT.
  TRANSLATE P_W_NAME TO UPPER CASE.
ENDFORM.                    " GET_DD07T_SELECT

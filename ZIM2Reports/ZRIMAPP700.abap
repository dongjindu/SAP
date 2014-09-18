*&---------------------------------------------------------------------*
*& Report  ZRIMAPP700                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : L/C 개설신청서                                        *
*&      작성자 : 이채경 INFOLINK Ltd.                                  *
*&      작성일 : 2001.07.04                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :  L/C 개설신청서.                                      *
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMAPP700   MESSAGE-ID ZIM
                     LINE-SIZE 116
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* L/C 입수내역 리스트용 TABLE
*-----------------------------------------------------------------------
DATA : W_MAX_ZFAMDNO LIKE ZTREQST-ZFAMDNO,
       W_ERR_CHK(1),
       W_COUNT    LIKE SY-TABIX,
       W_DOM_TEX1 LIKE DD07T-DDTEXT,
       W_DOM_TEX2 LIKE DD07T-DDTEXT.

DATA : BEGIN  OF    IT_ZTMLCSG7G OCCURS 100,
       ZFREQNO  LIKE ZTMLCSG7G-ZFREQNO, " 수입의뢰 관리번호.
       ZFLSG7G  LIKE ZTMLCSG7G-ZFLSG7G, " 반복수 Seg 7 상품명세.
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
TABLES : ZTREQHD,ZTREQIT,LFA1,ZTREQST,ZTMLCHD,ZTMLCSG2, ZTMLCSG910,
         ZTMLCSG7G,ZTMLCSG9O,DD07T.

*---------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.  " 2 LINE SKIP

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

   PARAMETERS    : P_REQNO   LIKE ZTREQHD-ZFREQNO.

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
*  P_REQNO = ZTREQHD-ZFREQNO.
  SET  TITLEBAR 'APP700'.          " TITLE BAR

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
*      MESSAGE S960 WITH SY-UNAME 'B/L Doc transaction'.
*      W_ERR_CHK = 'Y'.   EXIT.
*   ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK


*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

  SET PF-STATUS 'APP700'.           " GUI STATUS SETTING
  SET  TITLEBAR 'APP700'.           " GUI TITLE SETTING..
  PERFORM P3000_LINE_WRITE.

ENDFORM.                    " P3000_DATA_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.
DATA : L_TEXT(5), L_TEXT1(5), L_TEXT2(12).

  SKIP 2.
  WRITE:/37  '(Irrevocable Documentary Credit Application)'.

  SKIP 2.
  WRITE:/ SY-ULINE,
        / SY-VLINE,' ', 116 SY-VLINE,
       / SY-VLINE,8 'Except so far as otherwise expressly stated, this',
                     'documentary credit is subject to the',
                     116 SY-VLINE,
        / SY-VLINE,8 '"Uniform Customs and Practice for',
                     'Documentary Credits" (1993 Revision)',
                     'International',
                     116 SY-VLINE,
        / SY-VLINE,8 'Chamber of Commerce',
                     '(Publication No.500)',116 SY-VLINE,
        / SY-VLINE,                        116 SY-VLINE.
  WRITE:/ SY-ULINE.
  SKIP 2.
  WRITE:/8 'Document/EDI No : ',ZTREQST-ZFDOCNO.
  SKIP 2.
  ULINE AT /1(47). WRITE: '< General Info >'.ULINE AT 67(116).
  SKIP 1.
* DOMAIN.-----------------------------------------------------------
  PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDOPME' ZTMLCHD-ZFOPME
                                   CHANGING   W_DOM_TEX1.

  PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDPAGR' ZTMLCHD-ZFPAGR
                                    CHANGING   W_DOM_TEX2.
*--------------------------------------------------------------------

  WRITE:/8 'Opening application date :',ZTREQST-ZFRVDT,
        /8 'Opening method           :',W_DOM_TEX1,
        /8 'Opening(request) bank    :',ZTMLCHD-ZFOBNM,
        /8 '                          ',ZTMLCHD-ZFOBBR,
        /8 '                          ','[Tel No.]',ZTMLCHD-ZFOBPH.

*>통지은행.
  IF NOT ZTMLCHD-ZFABNM IS INITIAL.
     WRITE:/8 'Advising bank      :', ZTMLCHD-ZFABNM.
  ENDIF.

  WRITE:/8 'Credit providing subject :',W_DOM_TEX2.

*>> 수입추천번호.
  W_COUNT = 0.
  LOOP AT IT_ZTREQIL.
       IF IT_ZTREQIL-ZFRECNO IS INITIAL.
          CONTINUE.
       ENDIF.
       ADD 1 TO W_COUNT.
       IF W_COUNT EQ 1.
          WRITE: /8 'Import License No(amount):',28 IT_ZTREQIL-ZFRECNO,
                IT_ZTREQIL-ZFRECCU RIGHT-JUSTIFIED,
          IT_ZTREQIL-ZFRECAM CURRENCY IT_ZTREQIL-ZFRECCU LEFT-JUSTIFIED.
        ELSE.
          WRITE:/28 IT_ZTREQIL-ZFRECNO,
                    IT_ZTREQIL-ZFRECCU RIGHT-JUSTIFIED,
          IT_ZTREQIL-ZFRECAM CURRENCY IT_ZTREQIL-ZFRECCU LEFT-JUSTIFIED.
       ENDIF.
  ENDLOOP.
*>> 기타정보.
  IF  NOT ZTMLCHD-ZFETC1 IS INITIAL
   OR NOT ZTMLCHD-ZFETC2 IS INITIAL
   OR NOT ZTMLCHD-ZFETC3 IS INITIAL
   OR NOT ZTMLCHD-ZFETC4 IS INITIAL
   OR NOT ZTMLCHD-ZFETC5 IS INITIAL.
     WRITE:/8 'Other info:'.
  ENDIF.
  IF NOT ZTMLCHD-ZFETC1 IS INITIAL.
     WRITE:20 ZTMLCHD-ZFETC1.
  ENDIF.
  IF NOT ZTMLCHD-ZFETC2 IS INITIAL.
     WRITE:20 ZTMLCHD-ZFETC2.
  ENDIF.
  IF NOT ZTMLCHD-ZFETC3 IS INITIAL.
     WRITE:20 ZTMLCHD-ZFETC3.
  ENDIF.
  IF NOT ZTMLCHD-ZFETC4 IS INITIAL.
     WRITE:20 ZTMLCHD-ZFETC4.
  ENDIF.
  IF NOT ZTMLCHD-ZFETC5 IS INITIAL.
     WRITE:20 ZTMLCHD-ZFETC5.
  ENDIF.

  ULINE AT /1(47). WRITE: '<    Swift    >'.ULINE AT 67(116).
  SKIP 1.
* DOMAIN.-----------------------------------------------------------
  CLEAR: W_DOM_TEX1,W_DOM_TEX2.
  PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDLCTY' ZTMLCHD-ZFLCTY
                                   CHANGING   W_DOM_TEX1.

  PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDPAGR' ZTMLCHD-ZFPAGR
                                   CHANGING   W_DOM_TEX2.
*--------------------------------------------------------------------

  WRITE:/8 '40A','Form of Documentary Credit:',W_DOM_TEX1,
        /8 '31D','Date and Place of Expiry  :','(date)',
                                               ZTMLCHD-ZFEXDT,
        /8 '                               ','(place)',
        /40                                    ZTMLCHD-ZFEXPL,
        /8 '50 ', 'Applicant                 :',ZTMLCSG2-ZFAPPNM,
        /40                                     ZTMLCSG2-ZFAPPAD1.
  IF NOT ZTMLCSG2-ZFAPPAD2 IS INITIAL.
     WRITE:/40                            ZTMLCSG2-ZFAPPAD2.
  ENDIF.
  IF NOT ZTMLCSG2-ZFAPPAD3 IS INITIAL.
     WRITE:/40                            ZTMLCSG2-ZFAPPAD3.
  ENDIF.
  WRITE:/40 '[Tel No.]',                  ZTMLCSG2-ZFTELNO.
  WRITE:/8 '59 ', 'Beneficiary               :',ZTMLCSG2-ZFBENI1.
  IF NOT ZTMLCSG2-ZFBENI2 IS INITIAL.
     WRITE:/40 ZTMLCSG2-ZFBENI2.
  ENDIF.
  IF NOT ZTMLCSG2-ZFBENI3 IS INITIAL.
     WRITE:/40 ZTMLCSG2-ZFBENI3.
  ENDIF.
  IF NOT ZTMLCSG2-ZFBENI4 IS INITIAL.
     WRITE:/40 ZTMLCSG2-ZFBENI4.
  ENDIF.
  IF NOT ZTMLCSG2-ZFBENIA IS INITIAL.
     WRITE:/40 '[Account number]',ZTMLCSG2-ZFBENIA.
  ENDIF.
  WRITE:/8 '32B','Currency Code, Amount     :',
                  ZTMLCHD-WAERS  RIGHT-JUSTIFIED,
  ZTMLCHD-ZFOPAMT CURRENCY ZTMLCHD-WAERS LEFT-JUSTIFIED.

  CASE ZTMLCHD-ZFALCQ.
    WHEN 'T'.
       IF NOT ZTMLCHD-ZFALCP IS INITIAL.
          L_TEXT   = ZTMLCHD-ZFALCP. CONDENSE L_TEXT NO-GAPS.
          CONCATENATE '＋' L_TEXT '%' INTO L_TEXT.
       ENDIF.
       IF NOT ZTMLCHD-ZFALCM IS INITIAL.
          L_TEXT1  = ZTMLCHD-ZFALCM. CONDENSE L_TEXT1 NO-GAPS.
          CONCATENATE '－' L_TEXT1 '%' INTO L_TEXT1.
       ENDIF.
*       L_TEXT1 = ZTMLCHD-ZFALCP. CONDENSE L_TEXT1 NO-GAPS.
       CONCATENATE L_TEXT  L_TEXT1 INTO L_TEXT2
                   SEPARATED BY SPACE.

       WRITE: /8 '39A','Percentage Credit Amount Tolerance :',
               L_TEXT2.
    WHEN 'X'.
       WRITE: /8 '39B','Maximum Credit Amount     :'."ZTMLCHD-ZFALCP.
    WHEN '2AA'.
       WRITE: /8 '39C','Up to Credit Amount      :'."ZTMLCHD-ZFALCP.
    WHEN '2AB'.
       WRITE:/8  '39C','Not Exceeding Credit Amount :'."ZTMLCHD-ZFALCP.
    WHEN OTHERS.
  ENDCASE.

  IF  NOT ZTMLCHD-ZFAAMT1 IS INITIAL
   OR NOT ZTMLCHD-ZFAAMT2 IS INITIAL
   OR NOT ZTMLCHD-ZFAAMT3 IS INITIAL
   OR NOT ZTMLCHD-ZFAAMT4 IS INITIAL.
     WRITE:/8 '39C','Additional Amounts Covered:'.
  ENDIF.
  IF NOT ZTMLCHD-ZFAAMT1 IS INITIAL.
     WRITE:40 ZTMLCHD-ZFAAMT1.
  ENDIF.
  IF NOT ZTMLCHD-ZFAAMT2 IS INITIAL.
     WRITE:/40 ZTMLCHD-ZFAAMT2.
  ENDIF.
  IF NOT ZTMLCHD-ZFAAMT3 IS INITIAL.
     WRITE:/40 ZTMLCHD-ZFAAMT3.
  ENDIF.
  IF NOT ZTMLCHD-ZFAAMT4 IS INITIAL.
     WRITE:/40 ZTMLCHD-ZFAAMT4.
  ENDIF.

* DOMAIN.-----------------------------------------------------------
  CLEAR: W_DOM_TEX1,W_DOM_TEX2.
  PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDTRMB' ZTMLCHD-ZFTRMB
                                      CHANGING  W_DOM_TEX1 .
  PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDUSAT' ZTMLCHD-ZFUSAT
                                      CHANGING  W_DOM_TEX2.
*-------------------------------------------------------------------
  SHIFT:  W_DOM_TEX2 LEFT  DELETING LEADING SPACE.
  CASE ZTMLCHD-ZFTRMB.
       WHEN '2AO' OR SPACE.
          WRITE:/8 '42C','Drafts at ...             :',W_DOM_TEX2.
*          /40 ZTMLCHD-ZFUSPR,'DAYS'.
       WHEN '2AM'.
          WRITE:/8 '42M','Mixed Payment Details     :',W_DOM_TEX2.

       WHEN '2AN'.
          WRITE:/8 '42P','Deferred Payment Details  :',W_DOM_TEX2.
       WHEN OTHERS.
  ENDCASE.

  IF ZTMLCHD-ZFTRMB NE '2AO'.
     IF NOT ZTMLCHD-ZFTRTX1 IS INITIAL.
        WRITE:/40 ZTMLCHD-ZFTRTX1.
     ENDIF.
     IF NOT ZTMLCHD-ZFTRTX2 IS INITIAL.
        WRITE:/40 ZTMLCHD-ZFTRTX2.
     ENDIF.
     IF NOT ZTMLCHD-ZFTRTX3 IS INITIAL.
        WRITE:/40 ZTMLCHD-ZFTRTX3.
     ENDIF.
     IF NOT ZTMLCHD-ZFTRTX4 IS INITIAL.
        WRITE:/40 ZTMLCHD-ZFTRTX4.
     ENDIF.
  ENDIF.

* DOMAIN.-----------------------------------------------------------
  CLEAR: W_DOM_TEX1,W_DOM_TEX2.
  PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDPRMT' ZTMLCHD-ZFPRMT
                                      CHANGING   W_DOM_TEX1.

  PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDTRMT' ZTMLCHD-ZFTRMT
                                   CHANGING   W_DOM_TEX2.
*--------------------------------------------------------------------

  WRITE:/8 '43P','Partial Shipment          :',W_DOM_TEX1.
  WRITE:/8 '43T','Transshipment             :',W_DOM_TEX2.
  WRITE:/8 '44A','Loading on Board/ Dispatch/Taking in charge',
        /28        'at / from :',ZTMLCHD-ZFSPRT.
*        /40    ZTMLCHD-ZFAPRT,
*        /40    ZTMLCHD-ZFINCN,
*        /40    ZTMLCHD-ZFINCP.
  IF NOT ZTMLCHD-ZFAPRT IS INITIAL.
     WRITE:/8 '44B','For Transportation to....:',ZTMLCHD-ZFAPRT.
  ENDIF.
  IF  NOT ZTMLCHD-ZFLTSD IS INITIAL
   OR NOT ZTMLCHD-ZFSHPR2 IS INITIAL
   OR NOT ZTMLCHD-ZFSHPR3 IS INITIAL.
      WRITE:/8 '44D','Latest Date of Shipment.... :'.
  ENDIF.

  IF ZTMLCHD-ZFLTSD IS INITIAL.
     IF NOT ZTMLCHD-ZFSHPR1 IS INITIAL.
        WRITE:/40 ZTMLCHD-ZFSHPR1.
     ENDIF.
     IF NOT ZTMLCHD-ZFSHPR2 IS INITIAL.
        WRITE:/40 ZTMLCHD-ZFSHPR2.
     ENDIF.
     IF NOT ZTMLCHD-ZFSHPR3 IS INITIAL.
        WRITE:/40 ZTMLCHD-ZFSHPR3.
     ENDIF.
  ELSE.
     WRITE:40 ZTMLCHD-ZFLTSD.
  ENDIF.

  WRITE:/8 '45A','Description of Goods and / or services:'.
  LOOP AT IT_ZTMLCSG7G.
     WRITE:/12 IT_ZTMLCSG7G-ZFDSOG1.
  ENDLOOP.
  IF ZTMLCSG910-ZFCOMYN EQ 'X'
   OR ZTMLCSG910-ZFOCEYN EQ 'X'
   OR ZTMLCSG910-ZFAIRYN EQ 'X'
   OR ZTMLCSG910-ZFINYN EQ 'X'.
      WRITE:/8 '46A','Documents Required       :'.
  ENDIF.
  PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDOCEAC'
                                   ZTMLCSG910-ZFOCEAC
                                  CHANGING   W_DOM_TEX2.
*>> 선적서류.
  IF ZTMLCSG910-ZFCOMYN EQ 'X'.
     WRITE:/12 '+ SIGNED COMMERCIAL INVOICE IN',ZTMLCSG910-ZFNOCOM,
           'COPIES'.
  ENDIF.
  IF ZTMLCSG910-ZFOCEYN EQ 'X'.
    WRITE:/12 '+ FULL SET OF CLEAN ON BOARD OCEAN BILLS OF LADING',
    'MADE OUT TO THE ORDER OF',
          /12  ZTMLCSG910-ZFOCEC1,ZTMLCSG910-ZFOCEC2,
          /12 '+ MARKED FREIGHT ',(08)W_DOM_TEX2,'AND NOTIFY',
              ZTMLCSG910-ZFOCEAN.
  ENDIF.
  IF ZTMLCSG910-ZFAIRYN EQ 'X'.
     WRITE:/12 '+ AIRWAY BILL CONSIGNED TO',ZTMLCSG910-ZFAIRC1,
               ZTMLCSG910-ZFAIRC2,
           /12 'MARKED FREIGHT',ZTMLCSG910-ZFAIRAC,'AND NOTIFY',
               ZTMLCSG910-ZFAIRAN.
  ENDIF.
  IF ZTMLCSG910-ZFINYN EQ 'X'.
     WRITE:/12 '+ FULL SET OF INSURANCE POLICES OR CERTIFICATES,',
     'ENDORSED IN BLANK',
     /12 'FOR 110% OF THE INVOICE VALUE, EXPRESSLY',
     'STIPULATING THAT CLAIMS',
     /12 'ARE PAYABLE IN KOREA AND IT MUST INCLUDE :',
        'INSTITUTE CARGO CLAUSE'.
  ENDIF.

  IF ZTMLCSG910-ZFPACYN EQ 'X'.
      WRITE:/12 '+PACKING LIST IN',ZTMLCSG910-ZFNOPAC,'COPIES'.
  ENDIF.
  IF ZTMLCSG910-ZFCEOYN EQ 'X'.
     WRITE:/12 '+ CERTIFICATE OF ORIGIN'.
  ENDIF.
  IF ZTMLCSG910-ZFOTDYN EQ 'X'.
     WRITE:/12 '+ OTHERS DOCUMENT(S) [IF ANY]'.
  ENDIF.
  LOOP AT IT_ZTMLCSG8E.
      WRITE:/12 IT_ZTMLCSG8E-ZFOACD1.
  ENDLOOP.



*>> 추가조건.
  WRITE:/8 '47A','Additional Conditions     :'.
  IF ZTMLCHD-ZFADCD1 EQ 'X'.
     WRITE:/12 '+ SHIPMENT BY',ZTMLCHD-ZFCARR.
  ENDIF.
  IF ZTMLCHD-ZFADCD2 EQ 'X'.
     WRITE:/12 '+ ACCEPTANCE COMMISSION AND DISCOUNT CHARGES ARE FOR',
            'FOR BUYER''S ACCOUNT'.
  ENDIF.
  IF ZTMLCHD-ZFADCD3 EQ 'X'.
     WRITE:/12 '+ ALL DOCUMENTS MUST BEAR OUR CREDIT NUMBER'.
  ENDIF.
  IF ZTMLCHD-ZFADCD4 EQ 'X'.
      WRITE:/12 '+ LATE PERSENTATION B/L ACCEPTANCE'.
  ENDIF.
  IF ZTMLCHD-ZFADCD5 EQ 'X'.
     WRITE:/12 '+ OTHER ADDITIONAL CONDITIONS (IF ANY)'.

  ENDIF.
  LOOP AT IT_ZTMLCSG9O.
         WRITE:/12 IT_ZTMLCSG9O-ZFODOC1.
  ENDLOOP.

  IF  NOT ZTMLCHD-ZFAAMT1 IS INITIAL
  AND NOT ZTMLCHD-ZFAAMT2 IS INITIAL
  AND NOT ZTMLCHD-ZFAAMT3 IS INITIAL
  AND NOT ZTMLCHD-ZFAAMT4 IS INITIAL.
      WRITE:/8 '71B','Charges                   :'.
  ENDIF.
  IF NOT ZTMLCHD-ZFAAMT1 IS INITIAL.
     WRITE:/12 ZTMLCHD-ZFAAMT1.
  ENDIF.
  IF NOT ZTMLCHD-ZFAAMT2 IS INITIAL.
     WRITE:/12 ZTMLCHD-ZFAAMT2.
  ENDIF.
  IF NOT ZTMLCHD-ZFAAMT3 IS INITIAL.
     WRITE:/12 ZTMLCHD-ZFAAMT3.
  ENDIF.
  IF NOT ZTMLCHD-ZFAAMT4 IS INITIAL.
     WRITE:/12 ZTMLCHD-ZFAAMT4.
  ENDIF.

  WRITE:/8 '48','Period for Presentation :',
      /12 'DOCUMENT TO BE PRESENTED WITHIN',
     /12  ZTMLCHD-ZFPFPR,'DAYS AFTER THE DATE OF SHIPMENT',
     /12  'BUT WITHIN THE VALIDITY OF THE',
     /12  'CREDIT'.

* DOMAIN.-----------------------------------------------------------
  CLEAR: W_DOM_TEX1,W_DOM_TEX2.
  PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDCNIS' ZTMLCHD-ZFCNIS
                                      CHANGING   W_DOM_TEX1.
*___________________________________________________________________
  WRITE:/8 '49','Confirmation Instructions :',W_DOM_TEX1.
  SKIP 1.
  ULINE AT /1(45). WRITE: '< Electronic Signature >'.ULINE AT 72(116).
  SKIP 1.
  WRITE:/24 'Applicant electronic signature:',ZTMLCSG2-ZFELENM,
        /44  ZTMLCSG2-ZFREPRE,
        /44  ZTMLCSG2-ZFELEID,
        /37 '[Address]',ZTMLCSG2-ZFELEAD1,
        /44          ZTMLCSG2-ZFELEAD2.


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
     FROM ZTREQHD
    WHERE ZFREQNO =  P_REQNO.

   IF SY-SUBRC NE 0.
      W_ERR_CHK = 'Y'.
   ENDIF.
*>>수입의뢰 Status
   SELECT MAX( ZFAMDNO ) INTO W_MAX_ZFAMDNO
     FROM  ZTREQST
     WHERE ZFREQNO = P_REQNO.
*       AND ZFDOCST = 'O'.
   CLEAR ZTREQST.
   SELECT SINGLE *
     FROM ZTREQST
    WHERE ZFREQNO = P_REQNO
      AND ZFAMDNO = W_MAX_ZFAMDNO.

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

   REFRESH: IT_ZTMLCSG7G,IT_ZTMLCSG8E,
            IT_ZTREQIL.
   CLEAR:IT_ZTMLCSG7G,IT_ZTMLCSG8E.

   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTMLCSG7G
      FROM  ZTMLCSG7G
      WHERE ZFREQNO = P_REQNO.
   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTMLCSG8E
      FROM  ZTMLCSG8E
      WHERE ZFREQNO = P_REQNO.
   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTMLCSG9O
      FROM  ZTMLCSG9O
      WHERE ZFREQNO = P_REQNO.

*>> 수입추천번호.
   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTREQIL
      FROM  ZTREQIL
      WHERE ZFREQNO = P_REQNO.

ENDFORM.                    " P1160_GET_IT_TAB

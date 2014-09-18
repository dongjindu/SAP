*&---------------------------------------------------------------------*
*& Report  ZRIMLGPRT                                                   *
*&---------------------------------------------------------------------*
*&  프로그램명 : L/G 발급신청서                                        *
*&      작성자 : 이채경 INFOLINK Ltd.                                  *
*&      작성일 : 2001.07.13                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :  L/G 발급신청서 인쇄.                                 *
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMLGPRT   MESSAGE-ID ZIM
                     LINE-SIZE 116
                     NO STANDARD PAGE HEADING.
TABLES : ZTLG,ZTREQHD,LFA1,ZTREQST,ZTBL,ZTBLIT,ZTLGGOD.

*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMLGLSTTOP.
INCLUDE   ZRIMUTIL01.     " Utility function 모음.


*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   PARAMETERS    : P_ZFBLNO   LIKE ZTLG-ZFBLNO,
                   P_LGSEQ  LIKE ZTLG-ZFLGSEQ.

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
    PERFORM   P1000_GET_ZTLG           USING W_ERR_CHK.

* 레포트 Write
    PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.
    IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

   SET  TITLEBAR 'ZRIMLG'.          " TITLE BAR

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
*      MESSAGE S960 WITH SY-UNAME 'B/L 관리 트랜잭션'.
*      W_ERR_CHK = 'Y'.   EXIT.
*   ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK


*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

   SET PF-STATUS 'ZRIMLG'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZRIMLG'.           " GUI TITLE SETTING..

   CASE ZTBL-ZFVIA.
      WHEN 'VSL'.
        PERFORM P3000_LINE_WRITE_VSL.
      WHEN 'AIR'.
        PERFORM P3000_LINE_WRITE_AIR.
      WHEN OTHERS.
   ENDCASE.

ENDFORM.                    " P3000_DATA_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE_VSL.
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE_VSL.
  SKIP 2.
  WRITE:/3 'P/O No.:',W_PONO.
  WRITE:/48  ' LETTER OF GUARANTEE'.
  SKIP 1.
  WRITE:/3 'To',8 ZTLG-ZFCARR1.
  WRITE  : 75 'L/G No',85 '  '. ULINE AT /5(30).
*  WRITE:  85 SY-ULINE.
  WRITE:/10 '[Shipping Company]', 75 'Date',85 ' '.
*  WRITE:/85 SY-ULINE.
  SKIP 1.
  WRITE:/  SY-ULINE.
  WRITE:/  SY-VLINE, (55) 'Vessel Name/Voyage No.',
         59 SY-VLINE,  61  'L/C No.',
         89 SY-VLINE,  91  'Date of Issue       ',116 SY-VLINE,
         /  SY-VLINE,   8   ZTLG-ZFCARNM,
         59 SY-VLINE,  63   ZTLG-ZFDCNO ,
         89 SY-VLINE,  93   ZTREQST-ZFOPNDT,       116 SY-VLINE,
         /  SY-ULINE,
         /  SY-VLINE, (55) 'Port of Loading  ',
            SY-VLINE, (55) 'Invoice Value',       116 SY-VLINE,
         /  SY-VLINE,   8   ZTLG-ZFSHCUNM,
         59 SY-VLINE,  63   ZTLG-ZFCIAMC ,
                       67   ZTLG-ZFCIAM CURRENCY ZTLG-ZFCIAMC
                                                 LEFT-JUSTIFIED,
                                                  116 SY-VLINE,
        /  SY-ULINE,
        /  SY-VLINE, (55) 'Port of Discharge(or Place of Delivery)',
           SY-VLINE, (55) 'Description of Cargo',116 SY-VLINE,
        /  SY-VLINE,   8   ZTLG-ZFARCUNM,
        59 SY-VLINE, 116 SY-VLINE.
 ULINE AT /1(58).
 WRITE: 63 W_GODS1.
 WRITE: 59 SY-VLINE,                             116 SY-VLINE.
 WRITE:/ SY-VLINE, (30) 'Bill of lading No.',
        30 SY-VLINE,  32  'Date of Issue',
        59 SY-VLINE,  63   W_GODS2,              116 SY-VLINE,
        /  SY-VLINE,   8   ZTLG-ZFHBLNO,
        30 SY-VLINE,  34   ZTLG-ZFTBIDT,
        59 SY-VLINE,  63   W_GODS3,       116 SY-VLINE.
 ULINE AT /1(58).
 WRITE: 59 SY-VLINE,63 W_GODS4,116 SY-VLINE.

 SELECT SINGLE *
          FROM LFA1
         WHERE LIFNR = ZTLG-ZFCARIR.

 WRITE:/  SY-VLINE, (55) 'Shipper',  59 SY-VLINE,
          63 W_GODS5,116 SY-VLINE,
        /  SY-VLINE,     8 LFA1-NAME1, 59 SY-VLINE,
          116 SY-VLINE.
 WRITE: 59 SY-VLINE,     116 SY-VLINE.
 WRITE:/ SY-ULINE.
 WRITE:/ SY-VLINE, (55) 'Consignee',
        59 SY-VLINE,  61 'No.of Packages',
        89 SY-VLINE,  91 'Marks & Nos.    ',    116 SY-VLINE,
        /  SY-VLINE, 8  W_Consignee,
        59 SY-VLINE,
        89 SY-VLINE,  116 SY-VLINE.

 ULINE AT /1(58).
 WRITE: 59 SY-VLINE, 63 ZTLG-ZFPKCN UNIT ZTLG-ZFPKCNM RIGHT-JUSTIFIED,
                     72 ZTLG-ZFPKCNM,
                      89 SY-VLINE,116 SY-VLINE.
 WRITE:/   SY-VLINE, (55) 'Party to be delivered',
        59 SY-VLINE,  61 '         ',89 SY-VLINE,116 SY-VLINE,
           SY-VLINE,  8 ' ',59 SY-VLINE,89 SY-VLINE,
       116 SY-VLINE.
 WRITE:/   SY-ULINE.
 WRITE:/ SY-VLINE, 116 SY-VLINE.
 WRITE:/ SY-VLINE, 116 SY-VLINE.
 WRITE:/ SY-VLINE, 116 SY-VLINE.

 WRITE:/ SY-VLINE,6
'Whereas you have issued a Bill of Lading covering the above',
 'shipment and the above cargo has been arrived ',116
SY-VLINE,
 / SY-VLINE,'at the above port',
'of discharge(or the above place of',
'delivery),we hereby request you to give delivery of the ',
116 SY-VLINE,
/ SY-VLINE,
'said cargo to the above ',
'mentioned party without',
'production of the original Bill of Lading.',116 SY-VLINE,
/ SY-VLINE,
 '',116 SY-VLINE.
WRITE:/ SY-VLINE,6
'In consideration of your complying with our above request,',
'we hereby agree as follows;',116 SY-VLINE.

 WRITE:/ SY-VLINE,116 SY-VLINE,
  / SY-VLINE,
'1. To indemnify, your servants and agents and to hold all',
 'of you harmless in respect of liablity, loss, damage ',116 SY-VLINE,
 / SY-VLINE,
'   or expenses which you may sustain by reason of delivering the',
'cargo in accordance with our request, provided',116 SY-VLINE,
/ SY-VLINE,
'   that the undersigned bank shall be exempt from liability',
  'for freight,demurrage or expenses in respect of the',116 SY-VLINE,
/ SY-VLINE,
'   contact of carrage.', 116 SY-VLINE.
WRITE:/ SY-VLINE, 116 SY-VLINE.
WRITE:/ SY-VLINE,
'2. AS soon as the original Bill of Lading corresponding to',
'the above cargo comes into our possession,We shall ',116 SY-VLINE,
/ SY-VLINE,
'   surrender the same to you,  whereupon our',
'liability hereunder shall cease.',116 SY-VLINE.
WRITE:/ SY-VLINE, 116 SY-VLINE.
WRITE: / SY-VLINE,
'3. The liability of each and every person under this',
'guarantee shall be joint and several and shall not be',116 SY-VLINE,
/ SY-VLINE,
'   condition upon your proceeding first against any person,',
'whether or not such person is party to or liable',116 SY-VLINE,
/ SY-VLINE,
'   under this guarantee.',116 SY-VLINE.
WRITE:/ SY-VLINE, 116 SY-VLINE.

WRITE: / SY-VLINE,
'4. This guarantee shall be governed by and construed in',
'accordance with US law and the jurisdiction of', 116 SY-VLINE,
/ SY-VLINE,
 6 'the competent court in US.',116 SY-VLINE.

WRITE:/ SY-VLINE, 116 SY-VLINE,
/ SY-VLINE,
6 'Should the Bill of lading holder file claim or bring a',
'lawsuit against you,you shall notify the undesigned',116 SY-VLINE,
/ SY-VLINE,
'Bank as soon as possible.',116 SY-VLINE.

 WRITE:/ SY-VLINE, 116 SY-VLINE,
       / SY-VLINE, 116 SY-VLINE,
       / SY-VLINE, 116 SY-VLINE.

 WRITE:/ SY-VLINE,
 6 'Yours faithfully,',116 SY-VLINE.
 WRITE:/ SY-VLINE, 116 SY-VLINE.

 WRITE:/ SY-VLINE,
 6 'For and on behalf of',
 65 'For and on behalf of',116 SY-VLINE,
 / SY-VLINE,
 6'[Name of Requestor]',
 65 '[Name of bank]',116 SY-VLINE.
 WRITE:/ SY-VLINE, 116 SY-VLINE.
 WRITE:/ SY-VLINE,6 ZTLG-ZFELENM,65 ZTLG-ZFISBNM, 116 SY-VLINE.
 WRITE:/ SY-VLINE,116 SY-VLINE.
 WRITE:/ SY-VLINE,116 SY-VLINE.
 WRITE:/ SY-VLINE,116 SY-VLINE.
 WRITE:/ SY-VLINE,116 SY-VLINE.
 WRITE:/ SY-VLINE,116 SY-VLINE.
 WRITE:/ SY-VLINE,116 SY-VLINE.
 WRITE:/ SY-VLINE,116 SY-VLINE.

 WRITE:/ SY-VLINE,116 SY-VLINE.
 ULINE AT 6(40).
 ULINE AT 65(40).
 WRITE: 116 SY-VLINE.
*       /5 'Authorized Signature', 65 'Authorized Signature'.
 WRITE:/ SY-VLINE, 116 SY-VLINE.
 WRITE:/ SY-VLINE, 116 SY-VLINE.
 WRITE:/ SY-VLINE, 116 SY-VLINE.
 WRITE:/ SY-VLINE, 116 SY-VLINE.

 WRITE:/ SY-ULINE.
 SKIP 9.
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*
*  WRITE:/3 'P/O No:',W_PONO.
*  WRITE:/47  '수 입 화 물 선 취 보 증 서'.
*  SKIP 1.
*  WRITE :/3 '수신:', 8 ZTLG-ZFCARR1, 95 '보증서번호:'.
*  ULINE AT /3(40).
*  ULINE AT 95(40).
*  WRITE :/3 '[선박회사명]',  95 '발행일자'.
*  ULINE AT /95(40).
*  WRITE:  SY-ULINE.
**----------------------------------
**-------------------------------------
*  WRITE:/  SY-VLINE, (55) '선명 / 항차번호',
*         59 SY-VLINE,  61  '신용장번호',
*         89 SY-VLINE,  91  '개설일    ',116 SY-VLINE,
*         /  SY-VLINE,   8   ZTLG-ZFCARNM,
*         59 SY-VLINE,  63   ZTLG-ZFDCNO ,
*         89 SY-VLINE,  93   ZTREQST-ZFOPNDT,       116 SY-VLINE,
*         /  SY-ULINE,
*         /  SY-VLINE, (55) '선적항 ',
*            SY-VLINE, (55) '송장금액',       116 SY-VLINE,
*         /  SY-VLINE,   8   ZTLG-ZFSHCUNM,
*         59 SY-VLINE,  63   ZTLG-ZFCIAMC ,
*                       67   ZTLG-ZFCIAM CURRENCY ZTLG-ZFCIAMC
*                            LEFT-JUSTIFIED,116 SY-VLINE,
*        /  SY-ULINE,
*        /  SY-VLINE, (55) '양하항(또는 인도장소)',
*           SY-VLINE, (55) '화물명세',116 SY-VLINE,
*        /  SY-VLINE,   8   ZTLG-ZFARCUNM,
*        59 SY-VLINE,  116 SY-VLINE.
* ULINE AT /1(58).
* WRITE: 59 SY-VLINE, 63   W_GODS1,116 SY-VLINE.
* WRITE:/ SY-VLINE, (30) '선하증권번호',
*        30 SY-VLINE,  32  '발행일',
*        59 SY-VLINE,  63  W_GODS2,              116 SY-VLINE,
*        /  SY-VLINE,   8   ZTLG-ZFHBLNO,
*        30 SY-VLINE,  34   ZTLG-ZFTBIDT,
*        59 SY-VLINE,  63   W_GODS3,       116 SY-VLINE.
* ULINE AT /1(58).
* WRITE: 59 SY-VLINE,63 W_GODS4,116 SY-VLINE.
*
* WRITE:/  SY-VLINE, (55) '송하인',  59 SY-VLINE,63 W_GODS5,
*          116 SY-VLINE,
*       /  SY-VLINE,     8 ZTLG-ZFGSNM1, 59 SY-VLINE, 116 SY-VLINE.
* WRITE: 59 SY-VLINE,                                  116 SY-VLINE.
* WRITE:/ SY-ULINE.
*
* WRITE:/ SY-VLINE, (55) '수하인',
*        59 SY-VLINE,  61 '포장개수',
*        89 SY-VLINE,  91 '화인 및 번호',116 SY-VLINE,
*        /  SY-VLINE, 8  W_Consignee,
*        59 SY-VLINE,
*        89 SY-VLINE, 116 SY-VLINE.
*
* ULINE AT /1(58).
* WRITE: 59 SY-VLINE, 63 ZTLG-ZFPKCN UNIT ZTLG-ZFPKCNM RIGHT-JUSTIFIED,
*                     72 ZTLG-ZFPKCNM,89 SY-VLINE,116 SY-VLINE.
* WRITE:/   SY-VLINE, (55) '인수예정자',
*        59 SY-VLINE,  61 '         ',89 SY-VLINE,116 SY-VLINE,
*           SY-VLINE,  8 ' ',59 SY-VLINE,89 SY-VLINE,
*       116 SY-VLINE.
* WRITE:/   SY-ULINE.
* WRITE:/ SY-VLINE, 116 SY-VLINE.
* WRITE:/ SY-VLINE,6 '당사는 귀사가 상기 선적화물에 관한 선하증권을',
*'발행하였고 상기화물이 상기 양하항(또는 인도장소)에 도착하였고,'
* ,116 SY-VLINE.
* WRITE:/ SY-VLINE,'우리는 선하증권원본을 제시함이 없이 상기'
* ,'당사자에게 상기화물을 인도해 줄 것을 귀사에게 요청합니다.',
*  116 SY-VLINE.
* WRITE:/ SY-VLINE, 116 SY-VLINE.
* WRITE:/ SY-VLINE, 116 SY-VLINE.
* WRITE:/ SY-VLINE,6
*   '귀사가 상기와 같이 당사의 요청에 따를 경우 우리는',
*  '아래와 같이 합의 합니다.',116 SY-VLINE.
* WRITE:/ SY-VLINE, 116 SY-VLINE.
* WRITE:/ SY-VLINE,
*   '1. 귀사와 귀사가 지정한 고용인,대리점 모두는 당사의',
* '요청에 의하여 화물을 인도함으로써 발생 할지도 모를 채무,'
* ,116 SY-VLINE.
* WRITE:/ SY-VLINE,6 '손실,손해 또는 비용에 대하여 면책한다.',
* '다만 은행은 운송계약과 관련하여 발생하는 채무, 운임, 체선료,'
* , 116 SY-VLINE.
* WRITE:/ SY-VLINE,6 '기타비용은 책임을 지지 않는다.'
* ,116 SY-VLINE.
* WRITE:/ SY-VLINE, 116 SY-VLINE.
* WRITE:/ SY-VLINE,'2. 상기화물의 선하증권원본을 입수하는대로 귀사에게',
*'전달하겠으며, 이 때 당사의 책임은 종료된다.',116 SY-VLINE.
* WRITE:/ SY-VLINE, 116 SY-VLINE.
* WRITE:/ SY-VLINE,
*   '3. 본 보증서상 하기 서명자 각각, 그리고 모두가 연대',
* '및 단독 책임이 있으며 귀사가 하기 서명자중 한 사람에게 먼저',
* 116 SY-VLINE.
* WRITE:/ SY-VLINE,6 '소송을 제기 하였을 때 본 보증서상'
* ,'해당인(피고인)이 당사이든 책임이 있든 없든 조건부가 아니다.',
* 116 SY-VLINE.
* WRITE:/ SY-VLINE, 116 SY-VLINE.
* WRITE:/ SY-VLINE,'4. 이 보증서의 준거법 및 관할 법원은 한국법 및',
*'한국법원으로 한다.',116 SY-VLINE.
* WRITE:/ SY-VLINE, 116 SY-VLINE.
* WRITE:/ SY-VLINE,6'선하증권 소지인이 분쟁 또는 소송을 제기하는',
*'경우에는, 가능한 빨리 은행에 통보하여야 한다.', 116 SY-VLINE.
* WRITE:/ SY-VLINE, 116 SY-VLINE.
* WRITE:/ SY-VLINE, 116 SY-VLINE.
* WRITE:/ SY-VLINE, 116 SY-VLINE.
* WRITE:/ SY-VLINE,6 '화           주',
*         65 '은           행',116 SY-VLINE.
* WRITE:/ SY-VLINE,116 SY-VLINE.
* WRITE:/ SY-VLINE,6 ZTLG-ZFELENM,65 ZTLG-ZFISBNM,116 SY-VLINE.
* WRITE:/ SY-VLINE, 116 SY-VLINE.
* WRITE:/ SY-VLINE, 116 SY-VLINE.
* WRITE:/ SY-VLINE, 116 SY-VLINE.
* WRITE:/ SY-VLINE, 116 SY-VLINE.
* WRITE:/ SY-VLINE,116 SY-VLINE.
* ULINE AT 6(40).
* ULINE AT 65(40).
* WRITE:/ SY-VLINE,116 SY-VLINE.
* WRITE:/ SY-VLINE,116 SY-VLINE.
* WRITE:/ SY-VLINE,116 SY-VLINE.
* WRITE:/ SY-VLINE,116 SY-VLINE.
* WRITE:/ SY-ULINE.

ENDFORM.                    " P3000_LINE_WRITE_VSL.

*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ZTLG
*&---------------------------------------------------------------------*
FORM P1000_GET_ZTLG USING W_ERR_CHK.

   W_ERR_CHK = 'N'.
   CLEAR ZTLG.
   SELECT SINGLE *
     FROM ZTLG
    WHERE ZFBLNO  =  P_ZFBLNO
      AND ZFLGSEQ =  P_LGSEQ.
   IF SY-SUBRC NE 0.
      W_ERR_CHK = 'Y'.
      EXIT.
   ENDIF.

   CLEAR ZTBL.
   SELECT SINGLE *
      FROM ZTBL
     WHERE ZFBLNO = P_ZFBLNO.

   IF NOT ZTBL-ZFSHNO IS INITIAL.
      CONCATENATE ZTBL-ZFREBELN '-' ZTBL-ZFSHNO INTO W_PONO.
   ELSE.
      MOVE ZTBL-ZFREBELN TO W_PONO.
   ENDIF.

   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTLGGOD
      FROM  ZTLGGOD
      WHERE ZFBLNO = P_ZFBLNO
        AND ZFLGSEQ = P_LGSEQ.
   SORT IT_ZTLGGOD BY ZFLGOD.
   READ TABLE IT_ZTLGGOD INDEX 1.
   IF SY-SUBRC EQ 0.
      MOVE IT_ZTLGGOD-ZFGODS  TO W_GODS1.
   ENDIF.
   READ TABLE IT_ZTLGGOD INDEX 2.
   IF SY-SUBRC EQ 0.
      MOVE IT_ZTLGGOD-ZFGODS  TO W_GODS2.
   ENDIF.
    READ TABLE IT_ZTLGGOD INDEX 3.
   IF SY-SUBRC EQ 0.
      MOVE IT_ZTLGGOD-ZFGODS  TO W_GODS3.
   ENDIF.
    READ TABLE IT_ZTLGGOD INDEX 4.
   IF SY-SUBRC EQ 0.
      MOVE IT_ZTLGGOD-ZFGODS  TO W_GODS4.
   ENDIF.
    READ TABLE IT_ZTLGGOD INDEX 5.
   IF SY-SUBRC EQ 0.
      MOVE IT_ZTLGGOD-ZFGODS  TO W_GODS5.
   ENDIF.

   CLEAR ZTREQHD.
   SELECT SINGLE *
          FROM ZTREQHD
         WHERE ZFREQNO = ZTLG-ZFREQNO.
   CLEAR LFA1.
   SELECT SINGLE *
          FROM LFA1
         WHERE LIFNR = ZTLG-ZFGSCD.

   IF ZTREQHD-ZFREQTY = 'LC'.
      MOVE  LFA1-NAME1   TO W_Consignee.
   ELSE.
      MOVE  ZTLG-ZFELENM TO W_Consignee.
   ENDIF.
*>>수입의뢰 Status
   SELECT MAX( ZFAMDNO ) INTO W_MAX_ZFAMDNO
     FROM  ZTREQST
     WHERE ZFREQNO = ZTLG-ZFREQNO
       AND ZFDOCST = 'O'.

   SELECT SINGLE *
     FROM ZTREQST
    WHERE ZFREQNO = ZTLG-ZFREQNO
      AND ZFAMDNO = W_MAX_ZFAMDNO.

   CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
             INPUT   =   ZTLG-ZFPKCN
       IMPORTING
             OUTPUT  =   ZTLG-ZFPKCN.

 ENDFORM.                    " P1000_GET_ZTLG
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE_AIR
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE_AIR.

  WRITE:/3 'P/O No:',W_PONO.
  WRITE:/35 '항공화물운송장에의한수입화물인도승락(신청)서'.
  ULINE AT /35(44).
  SKIP 3.
  WRITE:/ SY-ULINE.
  WRITE:/ SY-VLINE,'운송회사명',58 SY-VLINE,
        62 '신용장등번호',76 SY-VLINE,80 ZTLG-ZFDCNO,116 SY-VLINE.
  WRITE:/ SY-VLINE,58 SY-ULINE, 116 SY-VLINE.
  WRITE:/ SY-VLINE,6 ZTLG-ZFCARR1,58 SY-VLINE,
        72 '항공화물 운송장 내용',116 SY-VLINE.
  WRITE:/ SY-VLINE,6 ZTLG-ZFCARR2,58 SY-ULINE, 116 SY-VLINE.
  WRITE:/ SY-VLINE,6 ZTLG-ZFCARR3,58 SY-VLINE, 116 SY-VLINE.
  ULINE AT 1(57).
  WRITE: 58 SY-VLINE, 62 '운송장번호',76 SY-VLINE,80 ZTLG-ZFHBLNO,
  116 SY-VLINE.
  WRITE:/ SY-VLINE,'송  하  인',58 SY-VLINE,59 SY-ULINE, 116 SY-VLINE.
  WRITE:/ SY-VLINE,58 SY-VLINE,62 '발  급  일',
  76 SY-VLINE, 80 ZTLG-ZFTBIDT, 116 SY-VLINE.

  WRITE:/ SY-VLINE,6 ZTLG-ZFGSNM1,58 SY-ULINE, 116 SY-VLINE.
  WRITE:/ SY-VLINE,6 ZTLG-ZFGSNM2,58 SY-VLINE,
  62 '비행편번호',76 SY-VLINE,
  80 ZTLG-ZFCARNM,116 SY-VLINE.

  WRITE:/ SY-VLINE,6 ZTLG-ZFGSNM3,58 SY-ULINE, 116 SY-VLINE.
  WRITE:/ SY-VLINE,58 SY-VLINE, 116 SY-VLINE.
  ULINE AT 1(57).
  WRITE: 58 SY-VLINE, 62 '도  착  일',76 SY-VLINE,80 ZTLG-ZFETA,
  116 SY-VLINE.
  WRITE:/ SY-VLINE, 58 SY-ULINE,116 SY-VLINE.
  WRITE:/ SY-VLINE,'송 장 금 액',58 SY-VLINE,62 '출  발  일',
  76 SY-VLINE,80 ZTBL-ZFETD,116 SY-VLINE.
  WRITE:/ SY-VLINE, 58 SY-ULINE,116 SY-VLINE.
  WRITE:/ SY-VLINE,6 ZTLG-ZFCIAMC,
         10 ZTLG-ZFCIAM CURRENCY ZTLG-ZFCIAMC LEFT-JUSTIFIED,
  58 SY-VLINE,62 '도  착  지',76 SY-VLINE,80 ZTLG-ZFARCUNM,116 SY-VLINE.
  WRITE:/ SY-ULINE.
  READ TABLE IT_ZTLGGOD INDEX 1.
  WRITE:/ SY-VLINE,8 SY-VLINE,12 '물품명',20 SY-VLINE,
  22 W_GODS1,76 SY-VLINE,80 '화물표시 및 번호',
  116 SY-VLINE.
  WRITE:/ SY-VLINE,5 '물', 8 SY-VLINE,20 SY-VLINE,76 SY-VLINE,
  116 SY-VLINE.
  ULINE AT 8(69).
  WRITE:/ SY-VLINE,5'품',8 SY-VLINE,12 '수량',20 SY-VLINE,
  22 ZTLG-ZFPKCN UNIT ZTLG-ZFPKCNM RIGHT-JUSTIFIED,
  32 ZTLG-ZFPKCNM,
  76 SY-VLINE,80 W_GODS2, 116 SY-VLINE.
  WRITE:/ SY-VLINE,5'명',116 SY-VLINE.
  ULINE AT 8(69).
  WRITE: 80 IT_ZTLGGOD-ZFGODS.
  WRITE:/ SY-VLINE,5'세',8 SY-VLINE,12 '단가',20 SY-VLINE,
  76 SY-VLINE,80 W_GODS3,116 SY-VLINE.

  WRITE:/ SY-VLINE, 116 SY-VLINE.
  ULINE AT 8(69).
  WRITE: 80 W_GODS4.
  WRITE:/ SY-VLINE,8 SY-VLINE,12'금액',20 SY-VLINE,22 ZTLG-ZFCIAMC,
  26 ZTLG-ZFCIAM CURRENCY ZTLG-ZFCIAMC
                  LEFT-JUSTIFIED, 76 SY-VLINE,80 W_GODS5,
  116 SY-VLINE.
  WRITE:/ SY-ULINE.
  WRITE:/ SY-VLINE, 116 SY-VLINE.
  WRITE:/ SY-VLINE,6 '본인은 위 신용장 등에 의하여 이미 도착한',
        '수입화물을 관계선적서류가 귀행에 도착하기전 항공화물 운송장',
        116 SY-VLINE,
  / SY-VLINE ,'의 배서인도에 의하여 선취하고자 신청하며 다음사항에'
     ,'따를 것을 확약 합니다.',
          116 SY-VLINE.
  WRITE:/ SY-VLINE, 116 SY-VLINE.
  WRITE:/ SY-VLINE,'1. 귀행이 수입화물선취보증서에 서명함으로써',
  '발생하는 위험과 책임 및 비용은 모두','본인이 부담하겠습니다.',
  116 SY-VLINE.

  WRITE:/ SY-VLINE, 116 SY-VLINE.
  WRITE:/ SY-VLINE,'2. 본인은 위 수입화물에 대하여는 귀행에 소유권이',
  '있음을 확인하며 귀행이 수입화물 선취보증서에 따른 보증채무를',
  116 SY-VLINE.
  WRITE:/ SY-VLINE,6 '이행하여야 할 것이 예상될',
  '경우 또는 본인에 대하여  은행여신거래 기본약관 제7조 제1항- 제5항',
  '각호의 사유가', 116 SY-VLINE.
  WRITE:/ SY-VLINE,6 '발행할',
  '경우,귀행의 청구를 받는 즉시 위 수입화물을 귀행에 인도하겠으며',
   '수입화물의 인도가 불가능한 경우에는',
   116 SY-VLINE.
  WRITE:/ SY-VLINE,6
 '위 수입화물에 상당하는 대금으로',
 '상환하겠습니다.', 116 SY-VLINE.
  WRITE:/ SY-VLINE, 116 SY-VLINE.
  WRITE:/ SY-VLINE,'3. 본인은 위 수입화물에 관한 관계선적서류를',
  '제 3자에게 담보로 제공하지 않았음을 확인하며,',
  '또한 귀행의 서면동의', 116 SY-VLINE.
  WRITE:/ SY-VLINE,6'없이 이를 담보로 제공하지',
  '않겠습니다.', 116 SY-VLINE.
  WRITE:/ SY-VLINE, 116 SY-VLINE.
  WRITE:/ SY-VLINE,'4. 본인은 위 수입화물에 관한 관계 선적서류가',
  '도착할 때에는 신용장조건과의 불일치 등 어떠한 흠결에도 불구하고 ',
  116 SY-VLINE.
  WRITE:/ SY-VLINE,6'이 서류를 반드시 인수하겠습니다. ',
  116 SY-VLINE.
  WRITE:/ SY-VLINE, 116 SY-VLINE.
  WRITE:/ SY-VLINE, 116 SY-VLINE.
  WRITE:/ SY-VLINE, 116 SY-VLINE.
  ULINE AT 100(11).
  WRITE:/ SY-VLINE, 100 SY-VLINE,103'인감대',110 SY-VLINE,116 SY-VLINE.
  WRITE:/ SY-VLINE, 116 SY-VLINE.
  ULINE AT 100(11).
  WRITE:/ SY-VLINE,50 '년', 65 '월', 80 '일',
  100 SY-VLINE,110 SY-VLINE,116 SY-VLINE.
  WRITE:/ SY-VLINE,6 '신청인:',17 ZTLG-ZFELENM,
  100 SY-VLINE,110 SY-VLINE,116 SY-VLINE.
  DATA: W_ADD(70) TYPE C.
  CONCATENATE: ZTLG-ZFAPPAD1 ZTLG-ZFAPPAD2
               ZTLG-ZFAPPAD3 INTO W_ADD  SEPARATED BY SPACE.

  WRITE:/ SY-VLINE,6 '주  소:',17 W_ADD NO-GAP,
  100 SY-VLINE,110 SY-VLINE,116 SY-VLINE.
  ULINE AT 100(11).
  WRITE:/ SY-VLINE, 116 SY-VLINE.
  ULINE AT 70(41).
  WRITE:/ SY-VLINE, 25 '앞',70 SY-VLINE,73'발급번호',83 SY-VLINE,
  110 SY-VLINE, 116 SY-VLINE.
  WRITE:/ SY-VLINE, 116 SY-VLINE.
  ULINE AT 6(22).
  ULINE AT 70(41).
  WRITE:/ SY-VLINE, 116 SY-VLINE.

  WRITE:/ SY-VLINE, 6'상기 신청내용과 같이 수입화물을 인도할 것을',
  '승낙합니다.',116 SY-VLINE.

  WRITE:/ SY-VLINE, 116 SY-VLINE.
  WRITE:/ SY-VLINE,50 '년', 65 '월', 80 '일',
  116 SY-VLINE.
  WRITE:/ SY-VLINE, 116 SY-VLINE.
  DATA: W_ZFISBNM(45) TYPE C.
  CONCATENATE ZTLG-ZFISBNM ' ' INTO W_ZFISBNM.
  WRITE:/ SY-VLINE,3 W_ZFISBNM RIGHT-JUSTIFIED,
  50'은 행 장', 116 SY-VLINE.
  WRITE:/ SY-VLINE, 116 SY-VLINE.
  WRITE:/ SY-VLINE,55'(IL상의 인감을 날인)', 116 SY-VLINE.
  WRITE:/ SY-VLINE, 116 SY-VLINE.
  WRITE:/ SY-VLINE, 116 SY-VLINE.
  WRITE:/ SY-VLINE, 116 SY-VLINE.
  WRITE:/ SY-VLINE, 116 SY-VLINE.
  WRITE:/ SY-ULINE.

ENDFORM.                    " P3000_LINE_WRITE_AIR

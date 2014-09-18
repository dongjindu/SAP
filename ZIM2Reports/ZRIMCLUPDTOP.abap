*----------------------------------------------------------------------*
*   INCLUDE ZRIMCLUPDTOP                                               *
*----------------------------------------------------------------------*
*&  프로그램명 : 수입면허 자료의 Upload를 위한 Include                 *
*&      작성자 : 김연중 INFOLINK Ltd.                                  *
*&      작성일 : 2000.04.12                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
TABLES : ZTIDS,           " 수입면?
         ZTIDSHS,         " 수입면허 란사?
         ZTIDSHSD,        " 수입면허 규격(행)사?
         ZTIDSHSL,        " 수입면허 요건확?
         ZTIDRCR,         " 수입관?
         ZTIDRCRIT,       " 감면허가품?
         ZTCUCL,          " 통?
         ZTCUCLCST,       " 통관비?
         ZTCUCLIV,        " 통관 Invoice
         ZTIDR,           " 수입신?
         ZTIV,            " Invoice
         ZTIVIT,          " Invoice Item
         ZTBL,            " Bill Of Lading
         ZTBLOUR,         " B/L 반출신?
         ZTBLINR,         " B/L 반입신?
         ZTIMIMG02,       " 세관코?
         ZTIMIMG06,       " 관세청 고시환?
         ZTIMIMG07,       " 통관수수료?
         ZTIMIMG10,       " 관세사코?
         LFA1,            " Vendor
         SPOP.     " POPUP_TO_CONFIRM_... function 모듈 팝업화면 필?
*-----------------------------------------------------------------------
* Upload ?
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_ZTIDS OCCURS 0,
      ZFIDRNO(14)         TYPE C,                " 수입신고번?
      ZFIDWDT(8)          TYPE C,                " 수입신고희망?
      ZFINAMTC(3)         TYPE C,                " 보험료 통?
      ZFINAMT(17)         TYPE C,                " 보험?
      ZFINAMTS(17)        TYPE C,                " 총보험?
      ZFTFAC(3)           TYPE C,                " 운임 A 통?
      ZFTFA(17)           TYPE C,                " 운임 A
      ZFTFBC(3)           TYPE C,                " 운임 B 통?
      ZFTFB(17)           TYPE C,                " 운임 B
      ZFTRT(17)           TYPE C,                " 총운?
      ZFADAMC(5)          TYPE C,                " 가산금구?
      ZFADAMCU(3)         TYPE C,                " 가산금 통?
      ZFADAM(17)          TYPE C,                " 가산금?
      ZFADAMK(17)         TYPE C,                " 가산금액(원화)
      ZFDUAMC(5)          TYPE C,                " 공제금구?
      ZFDUAMCU(3)         TYPE C,                " 공제금 통?
      ZFDUAM(17)          TYPE C,                " 공제금?
      ZFDUAMK(17)         TYPE C,                " 공제금액(원화)
      ZFPONC(2)           TYPE C,                " 수입거래구?
      ZFITKD(1)           TYPE C,                " 수입신고종?
      ZFINRC(3)           TYPE C,                " 신고지 세?
      ZFINRCD(2)          TYPE C,                " 세관의 담당 과부?
      ZFAPRTC(4)          TYPE C,                " 도착항코?
      ZFSCON(2)           TYPE C,                " 적출?
      ZFISPL(18)          TYPE C,                " 검사장소 장치장 부?
      ZFENDT(8)           TYPE C,                " 입항?
      ZFINDT(8)           TYPE C,                " 반입?
      ZFIMCD(1)           TYPE C,                " 수입자구?
      ZFIDRCD(1)          TYPE C,                " 신고구?
      ZFAMCD(2)           TYPE C,                " 대금결제방?
      ZFCTW(3)            TYPE C,                " 관세사 기재부?
      ZFCOCD(2)           TYPE C,                " 관세징수형?
      ZFCUPR(1)           TYPE C,                " 통관계획부?
      ZFTRCN(3)           TYPE C,                " 운송용?
      ZFHBLNO(20)         TYPE C,                " House B/L No
      ZFGOMNO(18)         TYPE C,                " 화물관리번?
      ZFIMCR(20)          TYPE C,                " 무역업체 참조번?
      ZFTRMET(2)          TYPE C,                " 운송수?
      ZFCARNM(20)         TYPE C,                " 선기?
      ZFCAC(2)            TYPE C,                " 선기국?
      ZFAPNM(28)          TYPE C,                " 신고자상?
      ZFAPNO(8)           TYPE C,                " 수입자 무역업등록번?
      ZFIAPNM(28)         TYPE C,                " 수입자상?
      ZFTDNO(15)          TYPE C,                " 납세자 통관고유번?
      ZFTDNM1(28)         TYPE C,                " 납세자 상?
      ZFTDNM2(12)         TYPE C,                " 납세자 성?
      ZFTDAD1(20)         TYPE C,                " 납세자 주소 1
      ZFTDAD2(20)         TYPE C,                " 납세자 주소 2
      ZFTDTC(13)          TYPE C,                " 납세자 사업자등록번?
      ZFTRDNO(7)          TYPE C,                " 무역대리점 부?
      ZFTRDNM(28)         TYPE C,                " 무역대리점 상?
      ZFSUPNO(10)         TYPE C,                " 공급자부?
      ZFSUPNM(26)         TYPE C,                " 공급자상?
      ZFSUPC(2)           TYPE C,                " 공급자 국적부?
      ZFSTRCD(2)          TYPE C,                " 특송업체부?
      INCO1(3)            TYPE C,                " Incoterms (part 1)
      ZFSTAMT(12)         TYPE C,                " 결제금?
      ZFSTAMC(3)          TYPE C,                " 결제금액통?
      ZFPKCNT(8)          TYPE C,                " 총포장개?
      ZFPKNM(2)           TYPE C,                " 포장종?
      ZFTOWT(10)          TYPE C,                " 총중?
      ZFTOWTMM(2)         TYPE C,                " 총중량단?
      ZFCUAMTS(12)        TYPE C,                " 총관?
      ZFSCAMTS(12)        TYPE C,                " 총특소?
      ZFDRAMTS(12)        TYPE C,                " 총주?
      ZFTRAMTS(12)        TYPE C,                " 총교통?
      ZFEDAMTS(12)        TYPE C,                " 총교육?
      ZFAGAMTS(12)        TYPE C,                " 총농특?
      ZFVAAMTS(12)        TYPE C,                " 총부가?
      ZFIDAMTS(12)        TYPE C,                " 총신고지연 가산?
      ZFTXAMTS(12)        TYPE C,                " 총세?
      ZFTBAK(12)          TYPE C,                " 과세가격-원?
      ZFTBAU(12)          TYPE C,                " 과세가격-미?
      ZFCTW1(60)          TYPE C,                " 관세사 기재란 1
      ZFCTW2(60)          TYPE C,                " 관세사 기재란 2
      ZFCTW3(60)          TYPE C,                " 관세사 기재란 3
      ZFCTW4(60)          TYPE C,                " 관세사 기재란 4
      ZFCTW5(60)          TYPE C,                " 관세사 기재란 5
      ZFIDSDT(8)          TYPE C,                " 신고수리?
      ZFCR_LF(1)          TYPE C,
END OF IT_ZTIDS.

DATA: BEGIN OF IT_ZTIDSHS OCCURS 0,
      ZFIDRNO(14)         TYPE C,                " 수입신고번?
      ZFCONO(3)           TYPE C,                " 란번?
      ZFCURT(6)           TYPE C,                " 관세?
      ZFTXPER(10)         TYPE C,                " 단위당 세?
      ZFHSAM(17)          TYPE C,                " 란신고금?
      STAWN(10)           TYPE C,                " Commodity code / Impo
      ZFGDNM(50)          TYPE C,                " 품?
      ZFTGDNM(50)         TYPE C,                " 거래품?
      ZFGCNM(50)          TYPE C,                " 상표품?
      ZFGCCD(50)          TYPE C,                " 상표코?
      ZFORIG(2)           TYPE C,                " Material's country of
      ZFWETM(2)           TYPE C,                " 중량단?
      ZFWET(10)           TYPE C,                " 중?
      ZFQNTM(2)           TYPE C,                " 수량단?
      ZFQNT(10)           TYPE C,                " 수?
      ZFORYN(1)           TYPE C,                " 원산지 표시유?
      ZFORME(1)           TYPE C,                " 원산지 표시방?
      ZFORTY(1)           TYPE C,                " 원산지 표시형?
      ZFSTCS(1)           TYPE C,                " 특송업체C/S
      ZFTXCD(2)           TYPE C,                " 관세구?
      ZFRDRT(6)           TYPE C,                " 관세감면?
      ZFTXAMCD(1)         TYPE C,                " 관세액기?
      ZFCDPCD(3)          TYPE C,                " 관세감면/분납구?
      ZFCDPNO(12)         TYPE C,                " 관세감면/분납부?
      ZFCUAMT(12)         TYPE C,                " 관?
      ZFCCAMT(12)         TYPE C,                " 관세감면?
      ZFHMTCD(2)          TYPE C,                " 내국세구?
      ZFHMTRT(6)          TYPE C,                " 내국세?
      ZFHMTTY(6)          TYPE C,                " 내국세 세종구?
      ZFHMAMT(12)         TYPE C,                " 내국?
      ZFHCAMT(12)         TYPE C,                " 내국세 감면?
      ZFSCCD(7)           TYPE C,                " 특소세 면세부?
      ZFETXCD(1)          TYPE C,                " 교육세 구?
      ZFEDAMT(12)         TYPE C,                " 교육?
      ZFECAMT(12)         TYPE C,                " 교육세 감면?
      ZFATXCD(1)          TYPE C,                " 농특세 구?
      ZFAGAMT(12)         TYPE C,                " 농특?
      ZFVTXCD(1)          TYPE C,                " 부가세 구?
      ZFVTXTY(7)          TYPE C,                " 부가세 감면부?
      ZFVAAMT(12)         TYPE C,                " 부가?
      ZFVCAMT(12)         TYPE C,                " 부가세 감면?
      ZFSCCS(8)           TYPE C,                " 특수세액 계산근?
      ZFMOR1(3)           TYPE C,                " 사후확인기관1
      ZFMOR2(3)           TYPE C,                " 사후확인기관2
      ZFMOR3(3)           TYPE C,                " 사후확인기관3
      ZFREQN(13)          TYPE C,                " 환급물?
      ZFREQNM(2)          TYPE C,                " 환급물량 단?
      ZFTBAK(12)          TYPE C,                " 과세가격-원?
      ZFTBAU(12)          TYPE C,                " 과세가격-미?
      ZFCR_LF(1)          TYPE C,
END OF IT_ZTIDSHS.

DATA: BEGIN OF IT_ZTIDSHSD OCCURS 0,
      ZFIDRNO(14)         TYPE C,                " 수입신고번?
      ZFCONO(3)           TYPE C,                " 란번?
      ZFRONO(3)           TYPE C,                " 규격(행)번?
      ZFSTCD(30)          TYPE C,                " 규격코?
      ZFGDDS1(30)         TYPE C,                " 규격1
      ZFGDDS2(30)         TYPE C,                " 규격2
      ZFGDDS3(30)         TYPE C,                " 규격3
      ZFGDIN1(25)         TYPE C,                " 성분1
      ZFGDIN2(25)         TYPE C,                " 성분2
      ZFQNT(15)           TYPE C,                " 수?
      ZFQNTM(3)           TYPE C,                " 수량단?
      NETPR(16)           TYPE C,                " Net price
      ZFAMT(15)           TYPE C,                " 금?
      ZFCR_LF(1)          TYPE C,
END OF IT_ZTIDSHSD.

DATA: BEGIN OF IT_ZTIDSHSL OCCURS 0,
      ZFIDRNO(14)         TYPE C,                " 수입신고번?
      ZFCONO(3)           TYPE C,                " 란번?
      ZFCNDC(3)           TYPE C,                " 요건확인구?
      ZFCNNO(20)          TYPE C,                " 요건번?
      ZFLACD(2)           TYPE C,                " 법령코?
      ZFISZDT(8)          TYPE C,                " 발급?
      ZFCUQN(10)          TYPE C,                " 통관사용수?
      ZFCUQNM(3)          TYPE C,                " 통관사용수량단?
      ZFCR_LF(1)          TYPE C,
END OF IT_ZTIDSHSL.

DATA   W_ZFBTSEQ         LIKE ZTBLINR-ZFBTSEQ.

DATA : W_PROC_CNT        TYPE I,             " 처리건?
       OPTION(1)         TYPE C,
       ANTWORT(1)        TYPE C,
       CANCEL_OPTION     TYPE C,
       TEXTLEN           TYPE I,
       DIGITS            TYPE I VALUE 20,
       W_ZFAPLDT         LIKE ZTIMIMG06-ZFAPLDT,
       W_ZFCAMT          LIKE ZTCUCLCST-ZFCAMT,
       W_TMP(1)          TYPE N,
       W_ZFCSQ           LIKE ZTCUCLCST-ZFCSQ,
       W_ZFIVAMT         LIKE BAPICURR-BAPICURR,
       W_ZFTBAK          LIKE BAPICURR-BAPICURR,
       W_DATE            LIKE SY-DATUM.

DATA : W_ERR_CHK(1)      TYPE C,
       W_SELECTED_LINES  TYPE P,             " 선택 LINE COUNT
       W_PAGE            TYPE I,             " Page Counter
       W_LINE            TYPE I,             " 페이지당 LINE COUNT
       W_COUNT           TYPE I,             " 전체 COUNT
       W_LIST_INDEX      LIKE SY-TABIX,
       W_FIELD_NM        LIKE DD03D-FIELDNAME,   " 필드?
       W_TABIX           LIKE SY-TABIX,      " TABLE INDEX
       W_UPDATE_CNT      TYPE I,
       W_BUTTON_ANSWER   TYPE C.

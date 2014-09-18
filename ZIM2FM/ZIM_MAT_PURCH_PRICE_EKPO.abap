FUNCTION ZIM_MAT_PURCH_PRICE_EKPO.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(IP_WERKS) LIKE  MARC-WERKS
*"     VALUE(IP_MATNR) LIKE  MARA-MATNR
*"     VALUE(IP_EKORG) LIKE  EORD-EKORG
*"     VALUE(IP_LIFNR) LIKE  EKKO-LIFNR OPTIONAL
*"     VALUE(IP_GUBUN) LIKE  EKKO-ABGRU
*"     VALUE(IP_BEDAT) LIKE  EKKO-BEDAT DEFAULT SY-DATUM
*"  EXPORTING
*"     VALUE(EP_LIFNR) LIKE  EKKO-LIFNR
*"     VALUE(EP_WAERS) LIKE  EKKO-WAERS
*"     VALUE(EP_PRICE) LIKE  EKPO-NETPR
*"     VALUE(EP_PEINH) LIKE  EKPO-PEINH
*"     VALUE(EP_KTMNG) LIKE  EKPO-KTMNG
*"     VALUE(EP_IDNLF) LIKE  EKPO-IDNLF
*"     VALUE(EP_BEDAT) LIKE  EKKO-BEDAT
*"     VALUE(EP_BPRME) LIKE  EKPO-BPRME
*"----------------------------------------------------------------------

*-----------------------------------------------------------------------
*  [ 변 경 내 용 ]
*   - 1999/10/11 강석봉 추가
*   - 플랜트 미입력시 전체 플랜트를 찾기 위해 ( 최근 구매단가 발췌 )
*-----------------------------------------------------------------------
*  [ 변 경 내 용 ]
*   - 1999/11/09 강석봉 추가
*   - 최근구매단가 발췌시 자재로 기발주를 검색하던 로직을
*     자재와 공급업체로 검색하는 로직으로 변경
*-----------------------------------------------------------------------

    IF  IP_GUBUN  = 'CF'.             "<ZMMP_POC11;최근구매단가 발췌.
        PERFORM  PRICE_UNIT_SELECT_PO USING
                       IP_WERKS IP_MATNR IP_EKORG IP_BEDAT IP_LIFNR
                       EP_LIFNR  EP_WAERS
                       EP_PRICE  EP_PEINH
                       EP_KTMNG  EP_IDNLF
                       EP_BEDAT  EP_BPRME.
    ELSEIF  IP_GUBUN  = 'GR'.         "<ZMMP_POC11;최근입고단가 발췌.
        PERFORM  PRICE_UNIT_SELECT_GR USING
                       IP_WERKS IP_MATNR IP_EKORG IP_BEDAT
                       EP_LIFNR  EP_WAERS
                       EP_PRICE  EP_PEINH
                       EP_KTMNG  EP_IDNLF
                       EP_BEDAT.
    ELSEIF  IP_GUBUN  = 'IF'.         "<ZMMP_IVG08;자재구입품의서
        PERFORM  PRICE_UNIT_SELECT_INFO USING
                       IP_WERKS IP_MATNR IP_EKORG IP_LIFNR
                       EP_LIFNR  EP_WAERS
                       EP_PRICE  EP_PEINH
                       EP_KTMNG.
    ELSEIF  IP_GUBUN  = 'BF'.         "<ZMMP_IVG08
        PERFORM  PRICE_UNIT_SELECT_BFPO USING
                       IP_WERKS IP_MATNR IP_EKORG IP_LIFNR
                       EP_LIFNR  EP_WAERS
                       EP_PRICE  EP_PEINH
                       EP_KTMNG.
    ELSE.                             "<업체별계약유효단가
        PERFORM  PRICE_UNIT_SELECT USING
                       IP_WERKS IP_MATNR IP_EKORG
                       EP_LIFNR  EP_WAERS
                       EP_PRICE  EP_PEINH.
                      "ep_ktmng.
    ENDIF.

ENDFUNCTION.
*

*&---------------------------------------------------------------------*
*& INCLUDE ZRIM00I03 .
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입의뢰 Main PAI MODULE Include(New)                 *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2001.08.25                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Module  SET_IMPORTER_TEXT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_IMPORTER_TEXT_SCRCOM INPUT.

*> 회사코드 정보 GET..(2001.08.21 KSB INSERT)
  PERFORM  P1000_READ_COMPANY_DATA  USING ZTREQHD-BUKRS
                                          ZTREQHD-IMTRD.

  PERFORM  P3000_SET_COMPANY_DATA.

  PERFORM  P2000_SET_SHIPING_TEXT.

ENDMODULE.                 " SET_IMPORTER_TEXT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_CALC_AMOUNT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_CALC_AMOUNT_SCRCOM INPUT.

  IF ZTINS-ZFALCP EQ 0.
     ZTINS-ZFIVAMT = ZTREQHD-ZFLASTAM.
  ELSE.
     ZTINS-ZFIVAMT = ZTREQHD-ZFLASTAM +
                     ZTREQHD-ZFLASTAM * ( ZTINS-ZFALCP / 100 ).
  ENDIF.

ENDMODULE.                 " SET_CALC_AMOUNT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  PO_DATA_CHECK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PO_DATA_CHECK INPUT.

* Display MODE MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
     EXIT.
  ENDIF.

* Internal Table Read
  READ TABLE IT_ZSREQIT   INDEX TC_0103-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE-CORRESPONDING ZSREQIT  TO IT_ZSREQIT.

*>> P/O No Input.
  IF ZSREQIT-EBELN IS INITIAL.
     MESSAGE W733.   EXIT.
  ELSE.
*>> P/O Input.
     SELECT SINGLE * FROM  EKKO
                     WHERE EBELN EQ IT_ZSREQIT-EBELN.
     IF SY-SUBRC NE 0.
        CLEAR : ZSREQIT.
        MESSAGE W001(ZIM1) WITH IT_ZSREQIT-EBELN.
        EXIT.
     ENDIF.

*>> Payment Term CHECK.
     IF ZTREQHD-ZTERM  IS  INITIAL.
        ZTREQHD-ZTERM  =  EKKO-ZTERM.
     ENDIF.

     IF ZTREQHD-ZTERM  NE  EKKO-ZTERM.
        MESSAGE E001(ZIM1) WITH ZTREQHD-ZTERM IT_ZSREQIT-EBELN
                                ZTREQHD-ZTERM.
     ENDIF.

*>> Currency Check
     IF ZTREQHD-WAERS IS INITIAL.
        ZTREQHD-WAERS  = EKKO-WAERS.
     ENDIF.

     IF ZTREQHD-WAERS NE EKKO-WAERS.
        MESSAGE E379 WITH ZTREQHD-WAERS IT_ZSREQIT-EBELN
                                ZTREQHD-WAERS.
     ENDIF.

*>> Vendor Check
     IF ZTREQHD-LIFNR IS INITIAL.
        ZTREQHD-LIFNR  =  EKKO-LIFNR.
     ENDIF.
     IF ZTREQHD-LIFNR NE EKKO-LIFNR.
        MESSAGE E380 WITH ZTREQHD-LIFNR IT_ZSREQIT-EBELN ZTREQHD-LIFNR.
     ENDIF.

*>> Company Code Check
     IF ZTREQHD-BUKRS IS INITIAL.
        ZTREQHD-BUKRS  =  EKKO-BUKRS.
     ENDIF.
     IF ZTREQHD-BUKRS NE EKKO-BUKRS.
        MESSAGE E382 WITH ZTREQHD-BUKRS IT_ZSREQIT-EBELN EKKO-BUKRS.
     ENDIF.

*>> Purchaing Group, Purchsing Organization Check
     IF ZTREQST-EKORG IS INITIAL.
        ZTREQST-EKORG  =  EKKO-EKORG.
     ENDIF.
     IF ZTREQST-EKGRP IS INITIAL.
        ZTREQST-EKGRP  =  EKKO-EKGRP.
     ENDIF.

*>> P/O Item No. Input
     IF NOT IT_ZSREQIT-EBELP IS INITIAL.
*----------------------------------------------------------------------
        READ TABLE  IT_ZSREQIT WITH KEY EBELN = ZSREQIT-EBELN
                                        EBELP = ZSREQIT-EBELP.
        IF SY-SUBRC EQ 0 AND W_SY_SUBRC NE 0.
           CLEAR : ZSREQIT.
           MESSAGE S358 WITH ZSREQIT-EBELN ZSREQIT-EBELP
                             IT_ZSREQIT-ZFITMNO.
           EXIT.
        ENDIF.
*----------------------------------------------------------------------
        MOVE-CORRESPONDING ZSREQIT  TO IT_ZSREQIT.
        CLEAR : EKPO.
        SELECT SINGLE * FROM EKPO
               WHERE    EBELN  EQ ZSREQIT-EBELN
               AND      EBELP  EQ ZSREQIT-EBELP.
        IF SY-SUBRC NE 0.
           MESSAGE W357 WITH ZSREQIT-EBELN ZSREQIT-EBELP.
           EXIT.
        ENDIF.

        " Delete Check
        IF EKPO-LOEKZ NE SPACE.
           MESSAGE W069 WITH ZSREQIT-EBELN ZSREQIT-EBELP.
           EXIT.
        ENDIF.
        " Delivery Complete MARKING CHECK!
        IF EKPO-ELIKZ EQ 'X'.
           MESSAGE W359 WITH ZSREQIT-EBELN ZSREQIT-ZFITMNO.
           EXIT.
        ENDIF.

        MOVE-CORRESPONDING EKPO  TO  IT_ZSREQIT.
        IF EKKO-BSTYP EQ 'F'.
           MOVE EKPO-MENGE TO IT_ZSREQIT-ZFPOMENGE.
        ELSEIF EKKO-BSTYP EQ 'L'.
           MOVE EKPO-KTMNG TO IT_ZSREQIT-ZFPOMENGE.
        ELSEIF EKKO-BSTYP EQ 'K'.
           MOVE EKPO-KTMNG TO IT_ZSREQIT-ZFPOMENGE.
        ENDIF.

        SELECT * FROM  EKET UP TO 1 ROWS
                                WHERE EBELN EQ IT_ZSREQIT-EBELN
                                AND   EBELP EQ IT_ZSREQIT-EBELP
                                ORDER BY EINDT.
           EXIT.
        ENDSELECT.
        MOVE  : EKET-EINDT  TO  IT_ZSREQIT-ZFEEIND,
                EKET-EINDT  TO  IT_ZSREQIT-EEIND,
                EKET-LPEIN  TO  IT_ZSREQIT-LPEIN.

        SELECT SUM( MENGE )  INTO  IT_ZSREQIT-ZFLCMENGE
        FROM   ZTREQIT
        WHERE  EBELN         EQ    IT_ZSREQIT-EBELN
        AND    EBELP         EQ    IT_ZSREQIT-EBELP.

        IT_ZSREQIT-MENGE  = IT_ZSREQIT-ZFPOMENGE - IT_ZSREQIT-ZFLCMENGE.
        IF IT_ZSREQIT-MENGE LT 0.
           IT_ZSREQIT-MENGE = 0.
        ENDIF.

        IF IT_ZSREQIT-ZFITMNO  IS INITIAL.
           IT_ZSREQIT-ZFITMNO  =  W_ZFITMNO  +  10.
        ENDIF.

        IF W_SY_SUBRC EQ 0.
           IT_ZSBLIT-EBELN = ZTREQHD-EBELN.
           MODIFY IT_ZSREQIT  INDEX W_TABIX.
        ELSE.
           APPEND  IT_ZSREQIT.
        ENDIF.
        EXIT.
     ENDIF.

     MOVE-CORRESPONDING ZSREQIT  TO IT_ZSREQIT.
     IF W_SY_SUBRC EQ 0.
        DELETE IT_ZSREQIT  INDEX W_TABIX.
     ENDIF.

  ENDIF.

ENDMODULE.                 " PO_DATA_CHECK  INPUT

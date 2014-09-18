*----------------------------------------------------------------------*
*   INCLUDE ZEMMPM53C_INFO__DOWNLOAD_TOP                               *
*----------------------------------------------------------------------*
REPORT  zemmpm53c_info__download  MESSAGE-ID zmmm .

TYPE-POOLS: slis, vrm.
TABLES:ztmm_if_price,lfa1,eina,t685,usr01,eine.

DATA : BEGIN OF it_eina OCCURS 0,
        infnr LIKE eina-infnr,  "info internal number
        matnr LIKE eina-matnr,  "Material
        lifnr LIKE eina-lifnr,  "vendor
        urzzt LIKE eina-urzzt,  "Number
        ekorg LIKE eine-ekorg,  "Pur.org
        aplfz LIKE eine-aplfz,  "del time
        ekgrp LIKE eine-ekgrp,  "pur group
        norbm LIKE eine-norbm,  "standard qty
        uebtk LIKE eine-uebtk,  "unlimited
        bstae LIKE eine-bstae,  "ConfContrK
        webre LIKE eine-webre,  "GR Based
        mwskz LIKE eine-mwskz,  "TAX code
        netpr LIKE eine-netpr,  "net price
        peinh LIKE eine-peinh,  "price unit
        waers LIKE eine-waers,  "currency
        loekz LIKE eina-loekz,  "General data flagged for deletion
        werks like eine-werks,
       END OF it_eina.

DATA : BEGIN OF it_out OCCURS 0,
*        infnr LIKE eina-infnr,  "info internal number
        lifnr LIKE eina-lifnr,  "vendor
        matnr LIKE eina-matnr,  "Material
        ekorg LIKE eine-ekorg,  "Pur.org
        urzzt LIKE eina-urzzt,  "Number
        aplfz LIKE eine-aplfz,  "del time
        ekgrp LIKE eine-ekgrp,  "pur group
        norbm LIKE eine-norbm,  "standard qty
        uebtk LIKE eine-uebtk,  "unlimited
        bstae LIKE eine-bstae,  "ConfContrK
        webre LIKE eine-webre,  "GR Based
        mwskz LIKE eine-mwskz,  "TAX code
        netpr LIKE eine-netpr,  "net price
        peinh LIKE eine-peinh,  "price unit
        waers LIKE eine-waers,  "currency
        datab(8)," LIKE a018-datab,   "valid FROM
        datbi(8)," LIKE a018-datbi,   "valid TO
        del_con TYPE c,
        del_inf TYPE c,
        only_gen TYPE c,
        fra1  LIKE konp-kbetr,   "condition value
        zoth  LIKE konp-kbetr,   "condition value
        zoti  LIKE konp-kbetr,   "condition value
        zp01  LIKE konp-kbetr,   "condition value
        zp02  LIKE konp-kbetr,   "condition value
        zp03  LIKE konp-kbetr,   "condition value
        zp04  LIKE konp-kbetr,   "condition value
        zp05  LIKE konp-kbetr,   "condition value
        zp06  LIKE konp-kbetr,   "condition value
        zp07  LIKE konp-kbetr,   "condition value
        zp08  LIKE konp-kbetr,   "condition value
        zp09  LIKE konp-kbetr,   "condition value
        zp10  LIKE konp-kbetr,   "condition value
        zp11  LIKE konp-kbetr,   "condition value
        zp12  LIKE konp-kbetr,   "condition value
        zp13  LIKE konp-kbetr,   "condition value
        zp14  LIKE konp-kbetr,   "condition value
        zp15  LIKE konp-kbetr,   "condition value
        zp16  LIKE konp-kbetr,   "condition value
        kosrt LIKE konh-kosrt,   "approval num
        kzust LIKE konh-kzust,   "reason
        ernam LIKE konh-ernam,   "creater
        erdat(8)," LIKE konh-erdat,   "creation date
        qty1  LIKE ztmm_cond-menge,
        qty2  LIKE ztmm_cond-menge,
        qty3  LIKE ztmm_cond-menge,
      END OF it_out.

DATA : BEGIN OF it_konp OCCURS 0,
       kosrt LIKE konh-kosrt,   "approval num
       kzust LIKE konh-kzust,   "reason
       ernam LIKE konh-ernam,   "creater
       erdat LIKE konh-erdat,   "creation date
       kschl LIKE konp-kschl,
       kbetr LIKE konp-kbetr,
       loevm_ko LIKE konp-loevm_ko, "Deletion flag
END OF it_konp.

DATA : w_int TYPE i.

INCLUDE ole2incl.
DATA : excel TYPE ole2_object,
       books TYPE ole2_object,
       book  TYPE ole2_object,
       cell  TYPE ole2_object.
*********************************************************************
*--Select OPTION
*--------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS : s_lifnr FOR eina-lifnr.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(10) text-005.
SELECTION-SCREEN POSITION 12.
PARAMETERS  p_t TYPE c  RADIOBUTTON GROUP ra01.
SELECTION-SCREEN COMMENT  22(9) text-004.
SELECTION-SCREEN POSITION 31.
PARAMETERS  p_e TYPE c  RADIOBUTTON GROUP ra01.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(10) text-006.
SELECTION-SCREEN POSITION 12.
PARAMETERS  p_v TYPE c  RADIOBUTTON GROUP ra02.
SELECTION-SCREEN COMMENT  22(9) text-007.
SELECTION-SCREEN POSITION 31.
PARAMETERS  p_i TYPE c  RADIOBUTTON GROUP ra02.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------*
*   INCLUDE ZXM06U43                                                   *
*----------------------------------------------------------------------*
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(I_EKKO) LIKE  EKKO STRUCTURE  EKKO
*"     VALUE(I_TRTYP)
*"     VALUE(I_BSTYP) LIKE  EKKO-BSTYP
*"     VALUE(I_NO_SCREEN)
*"     VALUE(I_LFA1) LIKE  LFA1 STRUCTURE  LFA1
*"     VALUE(I_LFM1) LIKE  LFM1 STRUCTURE  LFM1
*"     VALUE(I_KEKKO) LIKE  EKKO STRUCTURE  EKKO
*"     VALUE(I_AEKKO) LIKE  EKKO STRUCTURE  EKKO
*"     VALUE(I_REKKO) LIKE  EKKO STRUCTURE  EKKO
*"     VALUE(I_CI_EKKO) LIKE  EKKO_CI STRUCTURE  EKKO_CI
*"     VALUE(I_VORGA) LIKE  T160-VORGA
*"  TABLES
*"      TEKPO STRUCTURE  BEKPO OPTIONAL
*"      TEKET STRUCTURE  BEKET OPTIONAL
*"      TEKKN STRUCTURE  EKKNU OPTIONAL
*"      TKOMV STRUCTURE  KOMV OPTIONAL
*"  CHANGING
*"     VALUE(C_CI_EKKO) LIKE  EKKO_CI STRUCTURE  EKKO_CI OPTIONAL
*"----------------------------------------------------------------------

"----------------------------------------------------------------------

************************************************************************
* Enhancement
* Project Name      : Z_MM_PUR
* Enhancement Name  : MM06E005                                         *
* Function Name     : EXIT_SAPMM06E_006                                *
* Author            : Jaesung-LEE                                      *
* Creation Date     : 2003.10.02.                                      *
* Specifications By : Jaesung-LEE                                      *
* Development Request No :                                             *
* Addl Documentation:                                                  *
* Description       : EMMPM31 Field Exit for PO Enhancement            *
* Export Data to Customer Subscreen for Purchasing Document Header (PBO)
*                                                                      *
* Modification Logs                                                    *
* Date            Developer          RequestNo    Description          *
* 2003.10.01.     Jaesung-LEE                     Initial Coding       *
* 2004.02.23.     Sung-Tae Lim
* 07/28/06        Manju             UD1K921565   Change logic for      *
*                                                HMC Order# creation
* 08/23/06        Manju             UD1K921859   Override above changes
*                                                during PO change
* 11/27/06        Manju             UD1K923229   Allow KD PO change
************************************************************************

* kdweb number ( 10 Digit Number )
* 1 country  A
* 2 kind     C-CM , N-NF : MATERIAL GROUP
* 3 Order type R - normal or blank
*              I - others
*              C - Claim
* 4~5  Year        03 - 2003
* 6  month         01 -> A
*                  02 -> B
*                  03 -> C
* 7  -             Engine Material group - 1
*    -             Transmission Material group - 5
*                  Engine or Tranmission + Others Material GRP - Z
* 8-10  -          Last 3 Digit of PO

TABLES : EKKO.

DATA: L_COUNTRY(1) ," defalut 'A',
      L_KIND(1),
      L_ORDER_TYPE(2),
*      l_order_type(1),
      L_YEAR(2) ,
*      l_year(1) ,
      L_MONTH(1),
      L_SERIAL(4) TYPE N,
      L_MONTH_TMP(2),
      L_NUMBER LIKE NRIV-NRLEVEL,
      L_RCNT TYPE I,
      L_ENGC TYPE I,
      L_TRANC TYPE I.

*CHECK sy-tcode NE 'ME22N'.
*CHECK sy-tcode NE 'ME22'.

*check i_ekko-bsart = 'KD'.

IF I_EKKO-BSART = 'KD'.

  IF I_EKKO-EBELN IS INITIAL.

*  Country
**--- blocked by stlim (2004/04/19)
*    l_country = 'A'.
**--- end of block
**--- insert by stlim (2004/04/19)
    L_COUNTRY = 'B'.
**--- end of insert

* kind
**--- insert by stlim (2004/06/04)
    MOVE : '6' TO L_KIND.
**--- end of insert
**--- insert by stlim (2004/05/11)
*    MOVE : '8' TO l_kind.
**--- end of insert

**--- blocked by stlim (2004/05/11)
*    READ TABLE tekpo INDEX 1.
*    IF sy-subrc EQ 0.
*      l_kind = tekpo-matkl(1).
**    IF tekpo-matkl = 'CM'.
**      l_kind = 'R'.
**    ELSE.
**      l_kind = 'N'.
**    ENDIF.
*    ENDIF.
**--- end of block

* order type
    MOVE : 'IR' TO L_ORDER_TYPE.
*  IF i_ekko-angnr = ''.
*    l_order_type = 'R'.
*  ELSE.
*    l_order_type = i_ekko-angnr.
*  ENDIF.

* year
    MOVE : I_EKKO-BEDAT+3(1) TO L_YEAR.
*  MOVE i_ekko-bedat+2(2) TO l_year.

* month
    MOVE I_EKKO-BEDAT+4(2) TO L_MONTH_TMP.

    CASE L_MONTH_TMP.

      WHEN '01'.
        L_MONTH = 'A'.
      WHEN '02'.
        L_MONTH = 'B'.
      WHEN '03'.
        L_MONTH = 'C'.
      WHEN '04'.
        L_MONTH = 'D'.
      WHEN '05'.
        L_MONTH = 'E'.
      WHEN '06'.
        L_MONTH = 'F'.
      WHEN '07'.
        L_MONTH = 'G'.
      WHEN '08'.
        L_MONTH = 'H'.
      WHEN '09'.
        L_MONTH = 'I'.
      WHEN '10'.
        L_MONTH = 'J'.
      WHEN '11'.
        L_MONTH = 'K'.
      WHEN '12'.
        L_MONTH = 'L'.
    ENDCASE.


* SERIAL
    SELECT SINGLE NRLEVEL INTO L_NUMBER
                 FROM NRIV
                 WHERE OBJECT = 'EINKBELEG'
                   AND NRRANGENR = '42' .

    MOVE L_NUMBER+16(4) TO L_SERIAL.

**--- insert by stlim (2004/05/11)
    IF L_SERIAL EQ '0000'.
      MOVE : '0000' TO L_SERIAL.
    ELSE.
      L_SERIAL = L_SERIAL + 1 .
    ENDIF.
**--- end of insert

*IF SY-UNAME = 'JSLEE72'.
*   BREAK-POINT.
    CLEAR C_CI_EKKO-ZZKDWEBPO.

    CONCATENATE L_COUNTRY L_KIND L_ORDER_TYPE
                L_YEAR L_MONTH L_SERIAL INTO C_CI_EKKO-ZZKDWEBPO.

  ELSE.

    CLEAR : EKKO-ZZKDWEBPO.


    SELECT SINGLE ZZKDWEBPO INTO C_CI_EKKO-ZZKDWEBPO
                            FROM EKKO
                           WHERE EBELN EQ I_EKKO-EBELN.

* Begin of changes - UD1K923229
    IF  EKKO_CI-ZZKDWEBPO  NE C_CI_EKKO-ZZKDWEBPO.
      C_CI_EKKO-ZZKDWEBPO = EKKO_CI-ZZKDWEBPO.
    ENDIF.
* End of changes - UD1K923229

    IF C_CI_EKKO-ZZKDWEBPO IS INITIAL.
      L_COUNTRY = 'B'.
      MOVE : '6' TO L_KIND.
      MOVE : 'IR' TO L_ORDER_TYPE.
      MOVE : I_EKKO-BEDAT+3(1) TO L_YEAR.
      MOVE I_EKKO-BEDAT+4(2) TO L_MONTH_TMP.
      CASE L_MONTH_TMP.
        WHEN '01'.
          L_MONTH = 'A'.
        WHEN '02'.
          L_MONTH = 'B'.
        WHEN '03'.
          L_MONTH = 'C'.
        WHEN '04'.
          L_MONTH = 'D'.
        WHEN '05'.
          L_MONTH = 'E'.
        WHEN '06'.
          L_MONTH = 'F'.
        WHEN '07'.
          L_MONTH = 'G'.
        WHEN '08'.
          L_MONTH = 'H'.
        WHEN '09'.
          L_MONTH = 'I'.
        WHEN '10'.
          L_MONTH = 'J'.
        WHEN '11'.
          L_MONTH = 'K'.
        WHEN '12'.
          L_MONTH = 'L'.
      ENDCASE.
      MOVE : I_EKKO-EBELN+6(4) TO L_SERIAL.
      CLEAR C_CI_EKKO-ZZKDWEBPO.
      CONCATENATE L_COUNTRY L_KIND L_ORDER_TYPE
                  L_YEAR L_MONTH L_SERIAL INTO C_CI_EKKO-ZZKDWEBPO.
    ENDIF.
  ENDIF.

* Begin of changes -  UD1K921565
* For vendor SBC3 Set Seventh Digit as
* = '1'   ( Engine Materials )
* if  Material group = CM-KD-EN,CO-KD-EN,NF-KD-24E,NF-KD-EN
* = '5' ( Transmission  - Materials )
* if  Material group = CM-KD-TM,CO-KD-TM,NF-KD-24T,NF-KD-TM
  CLEAR: L_ENGC,L_TRANC, L_RCNT.
  IF I_EKKO-LIFNR  =  'SBC3' AND ( SY-TCODE EQ 'ME21' OR    "UD1K921859
                                   SY-TCODE EQ 'ME21N' ).   "UD1K921859
    DESCRIBE TABLE TEKPO LINES L_RCNT.
    LOOP AT TEKPO .
      CASE TEKPO-MATKL.
        WHEN 'CM-KD-EN'  OR 'CO-KD-EN' OR
             'NF-KD-24E' OR 'NF-KD-EN' .
          L_ENGC = L_ENGC +  1.
        WHEN 'CM-KD-TM' OR 'CO-KD-TM' OR
             'NF-KD-24T' OR 'NF-KD-TM' .
          L_TRANC  = L_TRANC + 1.
      ENDCASE.
    ENDLOOP.
* IF PO contains both  above Material Groups- FLAG PO as error
    IF L_ENGC > 0 AND L_TRANC > 0.
      MESSAGE E011(ZMM01).
    ELSEIF  L_ENGC = L_RCNT AND L_TRANC = 0.
      MOVE '1' TO C_CI_EKKO-ZZKDWEBPO+6(1).
    ELSEIF L_TRANC = L_RCNT AND L_ENGC = 0.
      MOVE '5' TO C_CI_EKKO-ZZKDWEBPO+6(1).
    ELSE. "If Po contains other Material grp materials
      MOVE 'Z' TO C_CI_EKKO-ZZKDWEBPO+6(1).
    ENDIF.
  ENDIF.
* End of changes -  UD1K921565


*
ENDIF.




************************************************************************
* Enhancement
* Project Name      : Z_MM_PUR
* Enhancement Name  : MM06E005                                         *
* Function Name     : EXIT_SAPMM06E_016                                *
* Author            : Sung-Tae Lim                                     *
* Creation Date     : 2003.11.04.                                      *
* Specifications By : Sung-Tae Lim                                     *
* Development Request No :                                             *
* Addl Documentation:                                                  *
* Description       : EMMPM37 Creating ETA Request                     *
* Export Data to Customer Subscreen for Purchasing Document Header (PBO)
*                                                                      *
* Modification Logs                                                    *
* Date            Developer          RequestNo    Description          *
* 2003.11.04.     Sung-Tae Lim                    Initial Coding       *
* 2003.12.29.     Sung-Tae Lim                    Logic Change         *
************************************************************************

*if i_ekko-bsart eq 'NKC' or i_ekko-bsart eq 'NKU' or
*   i_ekko-bsart eq 'CKC' or i_ekko-bsart eq 'CKU'.

IF I_EKKO-BSART EQ 'KD'.

  TABLES : EBAN, MARC.

  CLEAR : C_CI_EKKO-ZZETA.

*  clear : eban.
*
*  read table teket index 1.
*
*  select single badat into eban-badat
*                      from eban
*                     where banfn eq teket-banfn
*                       and bnfpo eq teket-bnfpo.
*
*  c_ci_ekko-zzeta = eban-badat - 60.

  IF I_EKKO-EBELN IS INITIAL.

    READ TABLE TEKPO INDEX 1.

    IF SY-SUBRC EQ 0.
      CLEAR : MARC-PLIFZ.
      SELECT SINGLE PLIFZ INTO MARC-PLIFZ
                          FROM MARC
                         WHERE MATNR EQ TEKPO-MATNR
                           AND WERKS EQ TEKPO-WERKS.
      C_CI_EKKO-ZZETA = SY-DATUM + MARC-PLIFZ.
    ENDIF.

  ELSE.

    CLEAR : EKKO-ZZETA.
    SELECT SINGLE ZZETA INTO C_CI_EKKO-ZZETA
                        FROM EKKO
                       WHERE EBELN EQ I_EKKO-EBELN.

    IF C_CI_EKKO-ZZETA IS INITIAL.
      READ TABLE TEKPO INDEX 1.
      IF SY-SUBRC EQ 0.
        CLEAR : MARC-PLIFZ.
        SELECT SINGLE PLIFZ INTO MARC-PLIFZ
                            FROM MARC
                           WHERE MATNR EQ TEKPO-MATNR
                             AND WERKS EQ TEKPO-WERKS.
        C_CI_EKKO-ZZETA = SY-DATUM + MARC-PLIFZ.
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.

*---

** Added by Fuorng on 07/28/10
DATA: L_MENGE LIKE EKPO-MENGE,
      L_FLAG(1).
.
** Changed by Furong on 09/20/10
*IF I_TRTYP = 'V' AND I_BSTYP = 'F' AND I_EKKO-BSART = 'KD'.
IF ( I_TRTYP = 'V'  OR I_TRTYP = 'H' )
  AND I_BSTYP = 'F' AND I_EKKO-BSART = 'KD'.
** End of change
  READ TABLE TEKPO INDEX 1.
  SELECT SINGLE FLAG INTO L_FLAG
    FROM ZTMM_KD_PO
    WHERE EBELN = TEKPO-EBELN.
  IF L_FLAG = 'S'.
    LOOP AT TEKPO.
      SELECT SINGLE MENGE INTO L_MENGE
       FROM EKPO
       WHERE EBELN = TEKPO-EBELN
         AND EBELP = TEKPO-EBELP.
      IF TEKPO-MENGE > L_MENGE.
        MESSAGE E012(ZMM01).
        EXIT.
      ENDIF.
      CLEAR: L_MENGE.
    ENDLOOP.
  ENDIF.
** Added by Fuorng on 08/16/10
  LOOP AT TEKPO.
    IF TEKPO-LOEKZ = ' '.
      READ TABLE TKOMV WITH KEY KPOSN = TEKPO-EBELP
                            KSCHL = 'FRA1'.
      IF SY-SUBRC <> 0.
        MESSAGE E013(ZMM01) with TEKPO-EBELP.
      ELSE.
        READ TABLE TKOMV WITH KEY KPOSN = TEKPO-EBELP
                           KSCHL = 'ZOTI'.
        IF SY-SUBRC <> 0.
          MESSAGE E014(ZMM01) with TEKPO-EBELP.
        ELSE.
          READ TABLE TKOMV WITH KEY KPOSN = TEKPO-EBELP
                        KSCHL = 'ZOA1'.
          IF SY-SUBRC <> 0.
            MESSAGE E015(ZMM01) with TEKPO-EBELP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
** End of change
ENDIF.
** End of change on 07/28/10
*---

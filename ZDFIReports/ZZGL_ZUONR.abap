REPORT zzvbund1.
TABLES: bseg, bkpf, t003, kna1, lfa1, bsis, bsas, bsik, bsak, bsid,bsad.
TABLES: vf_debi, vf_kred.


*TYPES: BKPF_TYPE LIKE BKPF,
*       BSEG_TYPE LIKE BSEG,
*       BSIS_TYPE LIKE BSIS,
*       BSAS_TYPE LIKE BSAS,
*       BSIK_TYPE LIKE BSIK,
*       BSAK_TYPE LIKE BSAK,
*       BSID_TYPE LIKE BSID,
*       BSAD_TYPE LIKE BSAD.
*
*DATA: XBKPF TYPE BKPF_TYPE OCCURS 0 WITH HEADER LINE,
*      EBKPF TYPE BKPF_TYPE OCCURS 0 WITH HEADER LINE,
*      XBSEG TYPE BSEG_TYPE OCCURS 0 WITH HEADER LINE,
*      YBSEG TYPE BSEG_TYPE OCCURS 0 WITH HEADER LINE,
*      XBSIS TYPE BSIS_TYPE OCCURS 0 WITH HEADER LINE,
*      XBSAS TYPE BSAS_TYPE OCCURS 0 WITH HEADER LINE,
*      XBSIK TYPE BSIK_TYPE OCCURS 0 WITH HEADER LINE,
*      XBSAK TYPE BSAK_TYPE OCCURS 0 WITH HEADER LINE,
*      XBSID TYPE BSID_TYPE OCCURS 0 WITH HEADER LINE,
*      XBSAD TYPE BSAD_TYPE OCCURS 0 WITH HEADER LINE,
*      XVBUND LIKE LFA1-VBUND,
*      V_ERROR TYPE C,
*      V_FLAG  TYPE C.

*DATA: IT_BSIK LIKE BSIK OCCURS 0.
DATA: BEGIN OF it_bsis OCCURS 0,
        bukrs LIKE bseg-bukrs,
        gjahr LIKE bsik-gjahr,
        belnr LIKE bseg-belnr,
        buzei LIKE bseg-buzei,
        vbund LIKE bsik-vbund,
        hkont LIKE bseg-hkont,
      END OF it_bsis.

DATA: BEGIN OF it_bseg OCCURS 0,
        bukrs LIKE bseg-bukrs,
        belnr LIKE bseg-belnr,
        gjahr LIKE bseg-gjahr,
        buzei LIKE bseg-buzei,
        hkont LIKE bseg-hkont,
        koart LIKE bseg-koart,
        augdt LIKE bseg-augdt,
        augbl LIKE bseg-augbl,
        ebeln LIKE bseg-ebeln,
        ebelp LIKE bseg-ebelp,
      END OF it_bseg.
*----------------------------------------------------------------
PARAMETERS:
          p_bukrs     LIKE bkpf-bukrs MEMORY ID buk,
          p_hkont     LIKE bsis-hkont.

*SELECT-OPTIONS:
*          S_LIFNR     FOR  BSIK-LIFNR.

SELECT-OPTIONS:
          s_gjahr     FOR  bkpf-gjahr,
          s_belnr     FOR  bkpf-belnr.
*          S_CPUDT     FOR  BKPF-CPUDT.

PARAMETERS: update DEFAULT space AS CHECKBOX,
            awtyp LIKE ttyp-awtyp. " DEFAULT 'MKPF' .

*----------------------------------------------------------------
*
* COep, coej,
* cooi
* bsad, bsak, bsid, bsik
* glfunct
* glt1
* anla, anek
* regup
*

*TABLES: BSIK, LFA1, BSEG.

START-OF-SELECTION.

  SELECT bukrs gjahr belnr buzei vbund hkont
    INTO TABLE it_bsis
    FROM bsis
    WHERE bukrs =  p_bukrs
      AND hkont =  p_hkont
      AND augdt = '00000000'
      AND augbl = ''
      AND gjahr IN s_gjahr
      AND belnr IN s_belnr.

  LOOP AT it_bsis.
    PERFORM update_vbund.
  ENDLOOP.

*&---------------------------------------------------------------------*
*&      Form  update_vbund
*&---------------------------------------------------------------------*
FORM update_vbund.

  DATA : lv_zuonr LIKE bseg-zuonr.

  DATA: p_vbund LIKE bseg-vbund.



  CHECK update = 'X'.
  REFRESH it_bseg.
  SELECT bukrs belnr gjahr buzei hkont koart augdt augbl ebeln ebelp
     INTO TABLE it_bseg
     FROM bseg
      WHERE bukrs = it_bsis-bukrs
        AND gjahr = it_bsis-gjahr
        AND belnr = it_bsis-belnr
        AND buzei = it_bsis-buzei.

  READ TABLE it_bseg INDEX 1.
  CHECK sy-subrc = 0.

  CLEAR p_vbund.
*  SELECT SINGLE VBUND INTO P_VBUND
*    FROM EKKO AS A
*    INNER JOIN LFA1 AS B
*       ON A~LIFNR = B~LIFNR
*    WHERE A~EBELN = IT_BSEG-EBELN.
*

  CONCATENATE it_bseg-ebeln  it_bseg-ebelp INTO lv_zuonr.

  UPDATE bseg SET zuonr = lv_zuonr
     WHERE bukrs = it_bseg-bukrs
       AND gjahr = it_bseg-gjahr
       AND belnr = it_bseg-belnr
       AND buzei = it_bseg-buzei.

  UPDATE bsis SET zuonr = lv_zuonr
     WHERE bukrs = it_bseg-bukrs
       AND hkont = it_bseg-hkont
       AND augdt = it_bseg-augdt
       AND augbl = it_bseg-augbl
       AND gjahr = it_bseg-gjahr
       AND belnr = it_bseg-belnr
       AND buzei = it_bseg-buzei.
  WRITE:/ 'UPDATED ', it_bsis-gjahr, it_bsis-belnr, 'with:', lv_zuonr.

ENDFORM.                    " update_vbund

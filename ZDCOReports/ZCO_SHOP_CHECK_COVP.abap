REPORT zco_shop_check_covp
       LINE-SIZE 255.

SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
* General Info.
PARAMETERS : p_kokrs LIKE csks-kokrs   MEMORY ID cac ." OBLIGATORY.

* Posted Yr.
PARAMETERS : p_bdatj LIKE keko-bdatj MEMORY ID bdtj." OBLIGATORY.
* periods
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(30) text-021. "From
SELECTION-SCREEN POSITION 33.
PARAMETERS: p_perab LIKE covja-perab MEMORY ID vpe
            MODIF ID per." OBLIGATORY.

SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK bl1.


*DATA : BEGIN OF it_auak OCCURS 0,
*
*       END OF it_auak.

DATA : it_covp LIKE covp OCCURS 0 WITH HEADER LINE.
DATA : it_auak LIKE covp OCCURS 0 WITH HEADER LINE.


DATA : l_k_timestmp LIKE covp-k_timestmp.
DATA : l_cpudt TYPE datum,
       l_cputm TYPE uzeit.


SELECT * INTO CORRESPONDING FIELDS OF TABLE it_auak
  FROM covp
  WHERE lednr = '00'
    AND gjahr = p_bdatj
    AND perio = p_perab
*    and OBJNR like 'OR%'
    AND wrttp = '04'
    AND versn = '000'
    AND scope = 'PRODT'
    AND awtyp = 'AUAK' .

DELETE it_auak WHERE objnr(2) <> 'OR'.

SORT it_auak DESCENDING BY k_timestmp.
LOOP AT it_auak     .
  l_k_timestmp = it_auak-k_timestmp.
  l_cpudt = it_auak-cpudt.
  l_cputm = it_auak-cputm.
  EXIT.
ENDLOOP.


SELECT  * INTO CORRESPONDING FIELDS OF TABLE  it_covp
  FROM covp
  WHERE lednr = '00'
    AND gjahr = p_bdatj
    AND wrttp = '04'
    AND versn = '000'
*    AND ( awtyp <> 'AFRU' )
    AND perio = p_perab
    AND scope = 'PRODT'
    AND k_timestmp > l_k_timestmp .

DELETE it_covp WHERE objnr(2) <> 'OR'.

WRITE : 'Period : ' ,     p_bdatj, p_perab,
        /'Settle date :',  l_cpudt, l_cputm, l_k_timestmp.

SKIP.

LOOP AT it_covp.

  WRITE : / it_covp-objnr,
            it_covp-usnam,
            it_covp-uspob,
            it_covp-sgtxt(25),
            it_covp-TIMESTMP,
            it_covp-cpudt,
            it_covp-cputm,
            it_covp-wkgbtr,
            it_covp-mbgbtr.

ENDLOOP.

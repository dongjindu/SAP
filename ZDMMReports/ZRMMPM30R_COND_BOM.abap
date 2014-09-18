************************************************************************
* Program Name      : ZRMMPM30R_COND_BOM
* Author            : Sung-Tae, Lim
* Creation Date     : 2004.04.21.
* Specifications By : Sung-Tae, Lim
* Pattern           : Report 1-1
* Development Request No : UD1K907463
* Addl Documentation:
* Description       : Condition Status by Material/Vendor
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2004.04.21.     Sung-Tae Lim     UD1K907463     Initial Coding
*
************************************************************************

REPORT zrmmpm30r_cond_bom NO STANDARD PAGE HEADING
                          LINE-SIZE 500
                          LINE-COUNT 64(1)
                          MESSAGE-ID zmmm.

**---
INCLUDE : zrmmpmxxr_incl.


**---
DATA : BEGIN OF it_fsc OCCURS 0,
         matnr LIKE mara-matnr,
       END OF it_fsc.

DATA : BEGIN OF it_ibsymbol OCCURS 0,
         atwrt TYPE ibsymbol-atwrt,
         atnam TYPE cabn-atnam,
       END OF it_ibsymbol.

DATA : it_stpox_alv LIKE stpox_alv OCCURS 0 WITH HEADER LINE.

DATA : it_itab LIKE ztmm_bom OCCURS 0 WITH HEADER LINE,
       it_temp LIKE it_itab OCCURS 0 WITH HEADER LINE.


DATA : w_atwrt(3),     "  LIKE ibsymbol-atwrt,
       w_atwri(3).     "  LIKE ibsymbol-atwrt.

**--- Constants
CONSTANTS : c_werks LIKE mast-werks VALUE 'P001',
            c_stlan LIKE mast-stlan VALUE '1',
            c_kappl LIKE a018-kappl VALUE 'M',
            c_pb00  LIKE a018-kschl VALUE 'PB00',
            c_ekorg LIKE a018-ekorg VALUE 'PU01'.


**---
*SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
*PARAMETERS : p_check AS CHECKBOX DEFAULT 'X'.
*SELECTION-SCREEN END OF BLOCK block1.


**---
START-OF-SELECTION.
  PERFORM get_data.


**---
END-OF-SELECTION.
  IF it_fsc[] IS INITIAL.
    MESSAGE s999 WITH text-m01.
  ELSE.
    PERFORM read_&_update.
  ENDIF.






*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
*---
  CLEAR : it_fsc, it_fsc[].

  SELECT matnr INTO CORRESPONDING FIELDS OF TABLE it_fsc
               FROM mara
              WHERE mtart EQ 'FERT'
                AND lvorm EQ space.
ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  color_search
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FSC_MATNR  text
*----------------------------------------------------------------------*
FORM color_search USING    p_it_mara_matnr.
*---
  CLEAR : it_ibsymbol, it_ibsymbol[], w_atwrt, w_atwri.

  SELECT c~atwrt
         d~atnam
                 INTO TABLE it_ibsymbol
                 FROM marc AS a INNER JOIN ibin AS b
                   ON a~mandt EQ b~mandt
                  AND a~cuobj EQ b~instance
                      INNER JOIN v_ibin_syval AS c
                         ON b~mandt EQ c~mandt
                        AND b~in_recno EQ c~in_recno
                            INNER JOIN cabn AS d
                               ON c~mandt EQ d~mandt
                              AND c~atinn EQ d~atinn
                WHERE d~atnam IN ('COLOREXT', 'COLORINT')
                  AND a~matnr EQ p_it_mara_matnr.

  CLEAR : it_ibsymbol.
  READ TABLE it_ibsymbol WITH KEY atnam = 'COLOREXT'.
  MOVE : it_ibsymbol-atwrt TO w_atwrt.

  CLEAR : it_ibsymbol.
  READ TABLE it_ibsymbol WITH KEY atnam = 'COLORINT'.
  MOVE : it_ibsymbol-atwrt TO w_atwri.

  TRANSLATE : w_atwrt TO UPPER CASE,
              w_atwri TO UPPER CASE.
ENDFORM.                    " color_search

*&---------------------------------------------------------------------*
*&      Form  call_function_bom
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_STPOX_ALV  text
*      -->P_IT_TEMP  text
*----------------------------------------------------------------------*
FORM call_function_bom TABLES   it_stpox_alv STRUCTURE stpox_alv
                                it_temp STRUCTURE it_temp.
*---
  DATA : l_stlal LIKE mast-stlal.

  CLEAR : l_stlal, it_stpox_alv, it_stpox_alv[], it_temp, it_temp[].

*---
  PERFORM get_max_alternative USING it_fsc-matnr
                              CHANGING l_stlal.

*---
  CALL FUNCTION 'Z_FMM_BOM_EXPL'
       EXPORTING
            im_matnr      = it_fsc-matnr
            im_werks      = c_werks
            im_stlan      = c_stlan
            im_stlal      = l_stlal
            im_atwre      = w_atwrt
            im_atwri      = w_atwri
            im_datuv      = sy-datum
       TABLES
            ext_stpox_alv = it_stpox_alv.

**--- insert by stlim (2004/04/27)
  DELETE it_stpox_alv WHERE NOT ( eitm EQ 'M' OR eitm EQ 'K' OR
                                  eitm EQ 'G' OR eitm EQ 'C' OR
                                  eitm EQ '1' OR eitm EQ '2' )
                        AND NOT ( zinfo EQ 'ENG' OR zinfo EQ 'TM' ).
**---

**--- blocked by stlim (2004/05/17)
**---
*  DELETE it_stpox_alv WHERE mtart NE 'ROH'.
**--- end of block

  LOOP AT it_stpox_alv.
    MOVE : it_fsc-matnr       TO it_temp-fsccd.
    MOVE : it_stpox_alv-idnrk TO it_temp-matnr,
           it_fsc-matnr+6(2)  TO it_temp-vkind,
           it_stpox_alv-menge TO it_temp-menge,
           it_stpox_alv-meins TO it_temp-meins.
    COLLECT it_temp.
    CLEAR : it_temp, it_stpox_alv.
  ENDLOOP.

*---
  it_temp-ernam = it_temp-aenam = sy-uname.
  it_temp-erdat = it_temp-aedat = sy-datum.
  it_temp-erzet = it_temp-aezet = sy-uzeit.
  MODIFY it_temp TRANSPORTING ernam erdat erzet
                              aenam aedat aezet
                        WHERE fsccd NE space.

*---
  MODIFY ztmm_bom FROM TABLE it_temp.

  COMMIT WORK.
ENDFORM.                    " call_function_bom

*&---------------------------------------------------------------------*
*&      Form  get_max_alternative
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MARA_MATNR  text
*      <--P_L_STLAL  text
*----------------------------------------------------------------------*
FORM get_max_alternative USING    p_it_mara_matnr
                         CHANGING p_l_stlal.
*---
  SELECT SINGLE MAX( stlal ) INTO p_l_stlal
                             FROM mast
                            WHERE matnr EQ p_it_mara_matnr
                              AND werks EQ c_werks
                              AND stlan EQ c_stlan.
ENDFORM.                    " get_max_alternative

*&---------------------------------------------------------------------*
*&      Form  read_&_update
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_&_update.
*---
  DELETE FROM ztmm_bom WHERE matnr NE space.

  COMMIT WORK.

*---
  LOOP AT it_fsc.
**--- get color
*    PERFORM color_search USING it_fsc-matnr.
*--- BOM explosion
    PERFORM call_function_bom TABLES it_stpox_alv it_temp.
  ENDLOOP.
ENDFORM.                    " read_&_update

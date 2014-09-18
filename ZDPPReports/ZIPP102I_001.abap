************************************************************************
* Program Name      : ZIPP102I_101
* Author            : Bobby
* Creation Date     : 2003.09.03.
* Specifications By : Bobby
* Development Request No : UD1K902140
* Addl Documentation:
* Description       : VEHICLE ORDER CREATION
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
REPORT  zipp102i_001  MESSAGE-ID zmpp .

DATA: it_7jb              LIKE TABLE OF ztpp_pmt07jb_b WITH HEADER LINE.

DATA: wa_cdate            LIKE sy-datum.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
PARAMETERS : P_WERKS      LIKE   T001W-WERKS DEFAULT 'P001'
                                             OBLIGATORY MEMORY ID WRK,
             P_HORIZ(2)   TYPE   N   DEFAULT '30'.
SELECTION-SCREEN SKIP 1.
PARAMETERS : P_SHORT    AS CHECKBOX  DEFAULT 'X',
             P_LONG     AS CHECKBOX ,
             P_PO       AS CHECKBOX  DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK B1.


START-OF-SELECTION.
  WA_CDATE = SY-DATUM + P_HORIZ + 1 .

  IF P_PO = 'X'.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_7jb
    FROM ztpp_pmt07jb_a
   WHERE gubb = '*'   .

  DELETE FROM ztpp_pmt07jb_b WHERE sqdt >= space .
  WRITE AT: /001(050)  'Delete Table Record... (ZTPP_PMT07JB_B) : ' ,
             055(010)   SY-SubRC               .

  MODIFY ztpp_pmt07jb_b FROM TABLE it_7jb .
  WRITE AT: /001(050)  'Create Table Record... (ZTPP_PMT07JB_B) : ' ,
             055(010)   SY-SubRC               .
  endif.

  IF P_SHORT = 'X' .
  CLEAR: it_7jb, it_7jb[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_7jb
    FROM ztpp_pmt07jb_a
   WHERE sqdt < wa_cdate
     AND gubb ne '*' .

  DELETE FROM ztpp_pmt07jb_c WHERE werks = 'P001'.
  PERFORM saving_ztpp_pmt07jb_c .
  WRITE AT: /001(050)  'Recreate Table Record... (ZTPP_PMT07JB_C) : ' ,
             055(010)   SY-SubRC               .
  endif.

  IF P_LONG = 'X'.
  CLEAR: it_7jb, it_7jb[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_7jb
    FROM ztpp_pmt07jb_a
   WHERE sqdt >= wa_cdate .
*     AND gubb =  space .

  DELETE FROM ztpp_pmt07jb_d WHERE werks = 'P001'.
  PERFORM saving_ztpp_pmt07jb_d .
  WRITE AT: /001(050)  'Recreate Table Record... (ZTPP_PMT07JB_D) : ' ,
             055(010)   SY-SubRC               .
  endif.

END-OF-SELECTION.
  IF P_PO = 'X'.
    SUBMIT  zipp102i_002   .
    WRITE AT: /001(050)  'Call the Program... (ZIPP102I_002): ' ,
               055(010)   SY-SubRC               .
  ENDIF.

  IF P_SHORT = 'X' .
     SUBMIT  ZIPP101U_PIR_MANAGEMENT WITH  P_WERKS = 'P001'
                                     WITH  P_HORIZ = P_HORIZ
                                     WITH  RA_SHORT = 'X'
                                     WITH  RA_LONG  = ' '    .
  WRITE AT: /001(050)  'Call Short-Term.... (ZIPP101U_PIR_MANAGEMENT):',
             055(010)   SY-SubRC               .
  ENDIF.

  IF P_LONG = 'X'.
     SUBMIT  ZIPP101U_PIR_MANAGEMENT WITH  P_WERKS = 'P001'
                                     WITH  P_HORIZ = P_HORIZ
                                     WITH  RA_SHORT = ' '
                                     WITH  RA_LONG  = 'X'    .
  WRITE AT: /001(050)  'Call Long-Term..... (ZIPP101U_PIR_MANAGEMENT):',
             055(010)   SY-SubRC               .
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  saving_ztpp_pmt07jb_c
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM saving_ztpp_pmt07jb_c.
  DATA: l_lines           TYPE i,
        l_plmng           LIKE ztpp_pmt07jb_c-plnmg ,
        it_cfile          LIKE TABLE OF ztpp_pmt07jb_c WITH HEADER LINE,
        it_cfile2         LIKE TABLE OF ztpp_pmt07jb_c WITH HEADER LINE.

  LOOP AT it_7jb.
    CLEAR:it_cfile.
    it_cfile-werks = 'P001' .
    it_cfile-pbdnr = it_7jb-ordr .
    PERFORM call_nation USING it_7jb-dist it_cfile-pbdnr .
    CONCATENATE it_7jb-moye it_7jb-dist it_7jb-bmdl INTO it_cfile-matnr.
    concatenate it_cfile-matnr it_7jb-ocnn          INTO it_cfile-matnr
                                                    separated by space.
    it_cfile-pdatu = it_7jb-sqdt .
    it_cfile-cogub = 'E'         .
    it_cfile-inexc = it_7jb-extc .
    it_cfile-plnmg = it_7jb-pqty .
    it_cfile-pver  = it_7jb-Pver .   APPEND it_cfile .
    it_cfile-cogub = 'I'         .
    it_cfile-inexc = it_7jb-intc .
    it_cfile-plnmg = it_7jb-pqty .
    it_cfile-pver  = it_7jb-Pver .   APPEND it_cfile .
  ENDLOOP.

  SORT it_cfile BY pdatu pbdnr matnr cogub inexc .
  READ TABLE it_cfile INDEX 1.
  it_cfile2 = it_cfile .  CLEAR: it_cfile2-plnmg .

  LOOP AT it_cfile .
    IF it_cfile-pdatu = it_cfile2-pdatu AND
       it_cfile-matnr = it_cfile2-matnr AND
       it_cfile-cogub = it_cfile2-cogub AND
       it_cfile-inexc = it_cfile2-inexc .
      l_plmng = it_cfile-plnmg + l_plmng.
      it_cfile2 = it_cfile.
      it_cfile2-plnmg = l_plmng.
*     APPEND it_cfile2.  " CLEAR: it_cfile2-plnmg.
    ELSE.
      it_cfile2-plnmg = l_plmng.
      APPEND it_cfile2.
      it_cfile2 = it_cfile.
      l_plmng = it_cfile-plnmg .
      CLEAR: it_cfile2-plnmg.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_cfile LINES l_lines.
  IF l_lines > 0 .
    it_cfile2-plnmg = l_plmng.
    APPEND it_cfile2.
  ENDIF.
  MODIFY ztpp_pmt07jb_c FROM TABLE it_cfile2 .
ENDFORM.                    " saving_ztpp_pmt07jb_c

*&---------------------------------------------------------------------*
*&      Form  saving_ztpp_pmt07jb_D
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM saving_ztpp_pmt07jb_d.
  DATA: l_lines           TYPE i,
        l_plmng           LIKE ztpp_pmt07jb_d-plnmg ,
        it_dfile          LIKE TABLE OF ztpp_pmt07jb_d WITH HEADER LINE,
        it_dfile2         LIKE TABLE OF ztpp_pmt07jb_d WITH HEADER LINE.

  LOOP AT it_7jb.
    CLEAR:it_dfile.
    it_dfile-werks = 'P001' .
    it_dfile-pbdnr = it_7jb-ordr .
    PERFORM call_nation USING it_7jb-dist it_dfile-pbdnr .
    CONCATENATE it_7jb-moye it_7jb-dist it_7jb-bmdl INTO it_dfile-matnr.
    concatenate it_dfile-matnr it_7jb-ocnn          INTO it_dfile-matnr
                                                    separated by space.
    it_dfile-pdatu = it_7jb-sqdt .
    it_dfile-cogub = 'E'         .
    it_dfile-inexc = it_7jb-extc .
    it_dfile-plnmg = it_7jb-pqty .
    it_dfile-pver  = it_7jb-Pver .   APPEND it_dfile .
    it_dfile-cogub = 'I'         .
    it_dfile-inexc = it_7jb-intc .
    it_dfile-plnmg = it_7jb-pqty .
    it_dfile-pver  = it_7jb-Pver .   APPEND it_dfile .
  ENDLOOP.

  SORT it_dfile BY pdatu pbdnr matnr cogub inexc .
  READ TABLE it_dfile INDEX 1.
  it_dfile2 = it_dfile .  CLEAR: it_dfile2-plnmg, l_plmng .

  LOOP AT it_dfile .
    IF it_dfile-pdatu = it_dfile2-pdatu AND
       it_dfile-matnr = it_dfile2-matnr AND
       it_dfile-cogub = it_dfile2-cogub AND
       it_dfile-inexc = it_dfile2-inexc .
      l_plmng = it_dfile-plnmg + l_plmng.
      it_dfile2 = it_dfile.
      it_dfile2-plnmg = l_plmng.
*     APPEND it_dfile2.  " CLEAR: it_dfile2-plnmg.
    ELSE.
      it_dfile2-plnmg = l_plmng.
      APPEND it_dfile2.
      it_dfile2 = it_dfile.
      l_plmng = it_dfile-plnmg .
      CLEAR: it_dfile2-plnmg.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_dfile LINES l_lines.
  IF l_lines > 0 .
    it_dfile2-plnmg = l_plmng.
    APPEND it_dfile2.
  ENDIF.
  MODIFY ztpp_pmt07jb_d FROM TABLE it_dfile2 .
ENDFORM.                    " saving_ztpp_pmt07jb_c

*&---------------------------------------------------------------------*
*&      Form  call_nation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_7JB_DIST  text
*      -->P_IT_CFILE_PBDNR  text
*----------------------------------------------------------------------*
FORM call_nation USING    pa_dist  pa_pbdnr.
  DATA: l_code               LIKE ztpp_nation_DEF-n_code .

  CALL FUNCTION 'Z_FPP_NATION_CODE'
       EXPORTING
            dist   = pa_dist
       IMPORTING
            n_code = l_code.

  CONCATENATE pa_pbdnr l_code INTO pa_pbdnr .
ENDFORM.                    " call_nation

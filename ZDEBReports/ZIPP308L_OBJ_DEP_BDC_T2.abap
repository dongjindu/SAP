*----------------------------------------------------------------------*
*   INCLUDE ZIPP308L_OBJ_DEP_BDC_T                                     *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* STANDARD-TABLE AREA ( TABLE)
*----------------------------------------------------------------------*
tables: ztbm_abyodpdt, ztbm_abycfidt,cukb .
*----------------------------------------------------------------------*
* INTERNAL-TABLE AREA
*----------------------------------------------------------------------*
data: it_aodp type ztbm_abyodpdt occurs 0 with header line.
define __cls.                          " clear & refresh
  clear &1.refresh &1.
end-of-definition.

data : begin of i_string occurs 0,
         $code(2),
       end   of  i_string.




* EXCEL UPLOAD
data: begin of it_excl occurs 0,
        dpid(30),
        eono(12),
        ddes(30),
        vald(08),
        stat(01),
        lin1(06),
        dpc1(72),
        lin2(06),
        dpc2(72),
        lin3(06),
        dpc3(72),
        lin4(06),
        dpc4(72),
        lin5(06),
        dpc5(72),
        lin6(06),
        dpc6(72),
        synt(01),
        zresult type ztbm_abxodpdt-zresult,
        zmsg    like cfgnl-msglin,
      end   of it_excl.

data: begin of wa_opt occurs 0.
        include structure ctu_params.
data: end of wa_opt.
data: begin of it_bdc occurs 0.
        include structure bdcdata.
data: end of it_bdc.
data: begin of it_mess occurs 0.
        include structure bdcmsgcoll.
data: end of it_mess.
*----------------------------------------------------------------------*
* DATA
*----------------------------------------------------------------------*
data: wa_line_idx type i,
      wa_erro_idx type i,
       ln_cnt(6) type c,
       last_line type i,
       last_fline type i,
       ln_c(120) type c.
*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
selection-screen begin of block b2 with frame title text-001.
*PARAMETERS: P_RDO1 type RADIOBUTTON DEFAULT 'X'.

selection-screen end   of block b2.

* TABLE SELECTION
selection-screen begin of block b1 with frame title text-002.
parameters:
*  P_GROUP LIKE APQI-GROUPID DEFAULT SY-REPID  OBLIGATORY,
  p_zedat like ztbm_abycfidt-zedat default sy-datum,
  p_zbtim like ztbm_abycfidt-zbtim.

* EXCEL DATA UPLOAD
parameters:
*  P_GROUP LIKE APQI-GROUPID DEFAULT SY-REPID  OBLIGATORY,
*  P_FILE  LIKE RLGRAP-FILENAME DEFAULT 'C:\       .TXT' OBLIGATORY,
*  P_FILETY LIKE RLGRAP-FILETYPE DEFAULT 'DAT',
  p_tcode like tstc-tcode default 'CU01'.
selection-screen end   of block b1.

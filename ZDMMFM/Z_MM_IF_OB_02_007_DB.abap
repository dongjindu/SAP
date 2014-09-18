FUNCTION Z_MM_IF_OB_02_007_DB .
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(IW_WERKS) LIKE  ZSMM_PLANT STRUCTURE  ZSMM_PLANT OPTIONAL
*"     VALUE(I_FDATE) TYPE  SY-DATUM OPTIONAL
*"     VALUE(I_TDATE) TYPE  SY-DATUM OPTIONAL
*"     VALUE(I_MTART) TYPE  MTART DEFAULT 'ROH'
*"  EXPORTING
*"     VALUE(E_RETURN) LIKE  ZMMS0053 STRUCTURE  ZMMS0053
*"  TABLES
*"      IT_M086 STRUCTURE  ZMMT0086 OPTIONAL
*"----------------------------------------------------------------------
  RANGES : R_LAEDA FOR MARA-LAEDA.
  DATA : LT_CDHDR LIKE CDHDR   OCCURS 0 WITH HEADER LINE.
  DATA : LT_PACKKP LIKE PACKKP OCCURS 0 WITH HEADER LINE,
         LT_MARA   LIKE MARA   OCCURS 0 WITH HEADER LINE,
         LT_PACKPO LIKE PACKPO OCCURS 0 WITH HEADER LINE.
  DATA : BEGIN OF IT_MAIN OCCURS 0.
          INCLUDE STRUCTURE ZMMT0086.
  DATA : RMATP LIKE MARA-RMATP.
  DATA :  END OF IT_MAIN.

*"----------------------------------------------------------------------
*"  Start of defining data types for using 'CS_WHERE_USED_MAT'
*"----------------------------------------------------------------------
  DATA IT_RETURN TYPE TABLE OF BAPIRET2 WITH HEADER LINE.
  DATA:
    EQUICAT   TYPE TABLE OF CSCEQUI WITH HEADER LINE,
    KNDCAT    TYPE TABLE OF CSCKND  WITH HEADER LINE,
    MATCAT    TYPE TABLE OF CSCMAT  WITH HEADER LINE,
    STDCAT    TYPE TABLE OF CSCSTD  WITH HEADER LINE,
    TPLCAT    TYPE TABLE OF CSCTPL  WITH HEADER LINE,
    STPOX     TYPE TABLE OF STPOX   WITH HEADER LINE.
  DATA:
    LV_BEIKZ  TYPE STPO-BEIKZ.
  DATA:
    IT_WULTB  TYPE TABLE OF STPOV   WITH HEADER LINE.
  DATA: L_TABIX TYPE SY-TABIX.

  RANGES: R_WERKS FOR ZMMT0086-WERKS.
*"----------------------------------------------------------------------
*"  End of defining data types for using 'CS_WHERE_USED_MAT'
*"----------------------------------------------------------------------

  CLEAR : IT_M086, IT_M086[],
          IT_MAIN, IT_MAIN[],
          LT_MARA,   LT_MARA[],
          LT_PACKKP, LT_PACKKP[],
          LT_PACKPO, LT_PACKPO[],
          R_LAEDA, R_LAEDA[].
  CLEAR : LT_CDHDR, LT_CDHDR[],
          XCHG,     XCHG[].

  G_DEST = 'WMPM01'.

  R_WERKS-OPTION = 'EQ'.
  R_WERKS-SIGN = 'I'.
  IF NOT IW_WERKS-WERKS1 IS INITIAL.
    R_WERKS-LOW = IW_WERKS-WERKS1.
    APPEND R_WERKS.
  ENDIF.
  IF NOT IW_WERKS-WERKS2 IS INITIAL.
    R_WERKS-LOW = IW_WERKS-WERKS2.
    APPEND R_WERKS.
  ENDIF.
  IF NOT IW_WERKS-WERKS3 IS INITIAL.
    R_WERKS-LOW = IW_WERKS-WERKS3.
    APPEND R_WERKS.
  ENDIF.
**S__Paul I_FDATE - 1 DAY : 06/28/11
  IF SY-BATCH = 'X' OR SY-CPROG = 'ZMMR90500T'.
    IW_WERKS-WERKS1 = 'E001'.
    IW_WERKS-WERKS2 = 'P001'.
* by ig.moon - Engine Plant Split {
    IW_WERKS-WERKS2 = 'E002'.
* }
    I_TDATE = SY-DATUM.
    I_FDATE = I_TDATE - 1.
  ENDIF.

*"----------------------------------------------------------------------
*"  If there exist search condtion for end date,
*"  find the material which was changed during searching condition.
*"----------------------------------------------------------------------
  IF I_TDATE NE '00000000'.

*"----------------------------------------------------------------------
*"  Make searching condition for specific date.
*"----------------------------------------------------------------------
    R_LAEDA-SIGN   = 'I'.
    R_LAEDA-OPTION = 'BT'.
    R_LAEDA-LOW    = I_FDATE.
    R_LAEDA-HIGH   = I_TDATE.
    APPEND R_LAEDA.

*"----------------------------------------------------------------------
*"  Search History Header for specific field which interfaced
*"  to GCS system.
*"----------------------------------------------------------------------
    SELECT  *
      INTO  CORRESPONDING FIELDS OF TABLE LT_CDHDR
      FROM  CDHDR
     WHERE  OBJECTCLAS  =  C_OBJECTCLAS  " find only Material.
       AND  UDATE      IN  R_LAEDA
       AND  TCODE      IN ('MM01','MM02','MM03','MMAM').

    SELECT  *
      INTO  CORRESPONDING FIELDS OF TABLE LT_PACKKP
      FROM  PACKKP
     WHERE  CHDAT      IN  R_LAEDA.

*"----------------------------------------------------------------------
*"  If there's no end date for searching,
*"  (i.e start date as required field, so changed date from start date~)
*"  check changing data from start date.
*"----------------------------------------------------------------------
  ELSE.

    SELECT  *
      INTO  CORRESPONDING FIELDS OF TABLE LT_CDHDR
      FROM  CDHDR
     WHERE  OBJECTCLAS = C_OBJECTCLAS
       AND  UDATE      = I_FDATE                    " Required field
       AND  TCODE IN ('MM01','MM02','MM03','MMAM').

    SELECT  *
      INTO  CORRESPONDING FIELDS OF TABLE LT_PACKKP
      FROM  PACKKP
     WHERE  CHDAT      =  I_FDATE.

  ENDIF.

  E_RETURN-TYPE     = 'E'.
  E_RETURN-MESSAGE  = 'There is no data for search criteria!'.


*"----------------------------------------------------------------------
*"  Gather item detail information from item level tables
*"  with header data
*"  Selected field :
*"  MATNR in MARA(A)
*"  WERKS in MARC(B)
*"  MAKTX in MAKT(C)
*"  MSTAE in MARA(A)
*"  MTART in MARA(A)
*"  MATKL in MARA(A)
*"  LABOR in MARA(A)
*"  BEIKZ from CS_WHERE_USED_MAT
*"  LGPRO in MARC(B)
*"  LGPBE in MARD(D) and MARD~LGORT EQ MARC~LGPRO
*"  VSPVB in MARC(B)
*"  LGFSB in MARC(B)
*"----------------------------------------------------------------------
  LOOP AT LT_CDHDR.
    XCHG-MATNR = LT_CDHDR-OBJECTID(18).
    XCHG-WERKS = LT_CDHDR-OBJECTID+18(4).
    COLLECT XCHG.
    CLEAR XCHG.
  ENDLOOP.

  IF NOT XCHG[] IS INITIAL.
    SELECT DISTINCT A~NORMT
      INTO CORRESPONDING FIELDS OF TABLE LT_MARA
      FROM  MARA AS A
      FOR ALL ENTRIES IN XCHG
      WHERE A~MATNR    EQ  XCHG-MATNR
       AND  A~MTART    EQ  I_MTART.

    LOOP AT LT_MARA.
      SELECT  *
        APPENDING CORRESPONDING FIELDS OF TABLE LT_PACKKP
        FROM  PACKKP
       WHERE POBJID = LT_MARA-NORMT.
    ENDLOOP.
  ENDIF.

  CLEAR : LT_MARA, LT_MARA[].

  LOOP AT LT_PACKKP.
    SELECT  A~MATNR
      APPENDING CORRESPONDING FIELDS OF TABLE LT_MARA
      FROM  MARA AS A
      WHERE A~NORMT    EQ  LT_PACKKP-POBJID
       AND  A~MTART    EQ  I_MTART.
  ENDLOOP.

  LOOP AT LT_MARA.
    XCHG-MATNR = LT_MARA-MATNR.
    COLLECT XCHG.
    CLEAR XCHG.
  ENDLOOP.


  IF NOT XCHG[] IS INITIAL.

    SELECT  A~MATNR
            B~WERKS
            C~MAKTX
            A~MSTAE
            A~MTART
            A~FERTH
            A~FORMT
            A~MATKL
            A~PROFL AS LABOR
            A~TEMPB
            A~RMATP
            B~LGPRO
            B~EPRIO
            B~VSPVB
            B~LGFSB
            D~BEHMG AS TRGQTY
            D~ZRHLH
            D~ZZFSTP

      APPENDING CORRESPONDING FIELDS OF TABLE IT_MAIN
      FROM  MARA AS A
           INNER JOIN MARC AS B
              ON A~MATNR EQ B~MATNR
           INNER JOIN MAKT AS C
              ON A~MATNR EQ C~MATNR
             AND C~SPRAS EQ SY-LANGU
      LEFT OUTER JOIN PKHD AS D
**Paul Changed with Matt 07/08/11
*              ON b~MATNR EQ D~MATNR
              ON B~MATNR EQ D~MATNR
             AND B~WERKS EQ D~WERKS
**Paul Changed with Matt 07/06/11
*             AND B~VSPVB EQ D~PRVBE

      FOR  ALL ENTRIES IN XCHG
      WHERE A~MATNR   EQ  XCHG-MATNR
        AND B~WERKS   IN  R_WERKS
        AND A~MTART   EQ  I_MTART.

  ENDIF.

  IF NOT LT_PACKKP[] IS INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_PACKPO
          FROM PACKPO
          FOR ALL ENTRIES IN LT_PACKKP
          WHERE PACKNR = LT_PACKKP-PACKNR.
  ENDIF.

  CHECK NOT IT_MAIN[] IS INITIAL.
  E_RETURN-TYPE     = 'S'.
  E_RETURN-MESSAGE  = 'Success'.

  LOOP AT IT_MAIN.
    MOVE-CORRESPONDING IT_MAIN TO IT_M086.

    REFRESH: IT_WULTB,
             STPOX.
    CALL FUNCTION 'CS_WHERE_USED_MAT'
         EXPORTING
              DATUB                      = '99991231'
              DATUV                      = I_FDATE
              MATNR                      = IT_MAIN-MATNR
              STLAN                      = '1'
              WERKS                      = IT_MAIN-WERKS
         TABLES
              WULTB                      = IT_WULTB
              EQUICAT                    = EQUICAT
              KNDCAT                     = KNDCAT
              MATCAT                     = MATCAT
              STDCAT                     = STDCAT
              TPLCAT                     = TPLCAT
         EXCEPTIONS
              CALL_INVALID               = 1
              MATERIAL_NOT_FOUND         = 2
              NO_WHERE_USED_REC_FOUND    = 3
              NO_WHERE_USED_REC_SELECTED = 4
              NO_WHERE_USED_REC_VALID    = 5
              OTHERS                     = 6.
*-----Material Provision Indicator.( F / C )
*-----Material provided by vendor
    READ TABLE IT_WULTB WITH KEY BEIKZ = 'C'.
    IF SY-SUBRC EQ 0.
      IT_M086-BEIKZ = 'C'.
    ELSE.
      READ TABLE IT_WULTB WITH KEY BEIKZ = 'F'.
      IF SY-SUBRC EQ 0.
        IT_M086-BEIKZ = 'F'.
      ELSE.
        IT_M086-BEIKZ = ' '.
      ENDIF.
    ENDIF.

    IF NOT IT_M086-LGPRO IS INITIAL.
      SELECT SINGLE LGPBE
             INTO IT_M086-LGPBE
        FROM MARD
       WHERE MATNR = IT_M086-MATNR
         AND WERKS = IT_M086-WERKS
         AND LGORT = IT_M086-LGPRO.
    ENDIF.
    IF NOT IT_M086-LGFSB IS INITIAL.
      SELECT SINGLE LGPBE
             INTO IT_M086-SGPBE
        FROM MARD
       WHERE MATNR = IT_M086-MATNR
         AND WERKS = IT_M086-WERKS
         AND LGORT = IT_M086-LGFSB.
    ENDIF.
    IF NOT IT_M086-EPRIO IS INITIAL.
      SELECT SINGLE LGPBE
             INTO IT_M086-EGPBE
        FROM MARD
       WHERE MATNR = IT_M086-MATNR
         AND WERKS = IT_M086-WERKS
         AND LGORT = IT_M086-EPRIO.
    ENDIF.

**Paul Changed with Matt 07/08/11
    IF IT_M086-WERKS = 'P001'.
      IF NOT IT_M086-VSPVB IS INITIAL.
        SELECT SINGLE ZFEEDER
               INTO IT_M086-ZFEEDER
               FROM PKHD
              WHERE MATNR = IT_M086-MATNR
                AND PRVBE = IT_M086-VSPVB
                AND WERKS = IT_M086-WERKS.
      ENDIF.
    ELSE.
      SELECT SINGLE ZFEEDER
               INTO IT_M086-ZFEEDER
               FROM PKHD
              WHERE MATNR = IT_M086-MATNR
                AND WERKS = IT_M086-WERKS.
    ENDIF.
**End Paul Changed with Matt 07/08/11

    SELECT SINGLE LIFNR
      INTO IT_M086-LIFNR
      FROM EINA
     WHERE LOEKZ = ''
       AND MATNR = IT_M086-MATNR.


    READ TABLE LT_PACKKP WITH KEY POBJID = IT_M086-MATNR.

    READ TABLE LT_PACKPO WITH KEY PACKNR = LT_PACKKP-PACKNR
                                  PAITEMTYPE = 'P'.
    IF SY-SUBRC EQ 0.
      IT_M086-PLMAT = LT_PACKPO-MATNR.
    ENDIF.
    IF NOT IT_MAIN-RMATP IS INITIAL.
      PERFORM CHECK_PACKING_INSTRUCTION USING IT_MAIN-RMATP
                                     CHANGING IT_M086-TRGQTY.
    ENDIF.

    APPEND IT_M086. CLEAR IT_M086.
  ENDLOOP.

  CHECK NOT IT_M086[] IS INITIAL.

  CALL FUNCTION 'Z_MM_IF_OB_02_007'
    DESTINATION G_DEST
    IMPORTING
      E_RETURN              = E_RETURN
    TABLES
      IT_BODY               = IT_M086
    EXCEPTIONS
      COMMUNICATION_FAILURE = 1
      SYSTEM_FAILURE        = 2
      RESOURCE_FAILURE      = 3
      OTHERS                = 4.

  CASE SY-SUBRC.
    WHEN '1'.
      E_RETURN-MESSAGE = 'communication_failure'.
      E_RETURN-TYPE    = 'E'.
    WHEN '2'.
      E_RETURN-MESSAGE = 'system_failure'.
      E_RETURN-TYPE    = 'E'.
    WHEN '3'.
      E_RETURN-MESSAGE = 'resource_failure'.
      E_RETURN-TYPE    = 'E'.
    WHEN '4'.
      E_RETURN-MESSAGE = 'Others'.
      E_RETURN-TYPE    = 'E'.
  ENDCASE.


  IT_M086-TYPE     = E_RETURN-TYPE.
  IT_M086-ETDAT    = SY-DATUM.
  IT_M086-ETTIM    = SY-UZEIT.
  IT_M086-ETNAM    = SY-UNAME.
  IT_M086-MESSAGE  = E_RETURN-MESSAGE.
  MODIFY IT_M086 TRANSPORTING TYPE ETDAT ETTIM ETNAM MESSAGE
                        WHERE TYPE = ' '.

  MODIFY ZMMT0086 FROM TABLE IT_M086.

*-- Error Log -----------------------------------
  LOOP AT IT_M086.

    GV_RETURN =  E_RETURN-TYPE.

    CALL METHOD ZMMC_CL_IF=>IF_SET_KEY(  IFKEY = 'MMIF207_ECC_OB'
                             MODLE = 'GCS'       " 'MM', 'SD', 'FI'
                             CENTY = 'US'       " '
                             DIRCT = 'O'  " 'O':Outbound, 'I':Inbound
                             LOGOX = ' '
                             TTYPE = 'S'
                             CPARM = '2'
                           ).

    CALL METHOD ZMMC_CL_IF=>IF_SET_MESSG( TYPE    = E_RETURN-TYPE
                              ID      = ' '    "gt_retmsg-id
                              MESSAGE = E_RETURN-MESSAGE
                            ).

    CALL METHOD ZMMC_CL_IF=>IF_SET_PARAM( ISTAT = GV_RETURN
                              IFP01 = IT_M086-MATNR
                              IFP02 = IT_M086-WERKS
*                              IFP03 = IT_M086-MATKL
*                              IFP04 = IT_M086-LGPRO
                            ).
    CALL METHOD ZMMC_CL_IF=>IF_SAVE_DATA( ).
  ENDLOOP.

ENDFUNCTION.

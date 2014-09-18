*
* provided by Andy Choi
*
*----------------------------------------------------------------------
*  Modification Log
*  Date        Developer Issue No    Description
*======================================================================
*  02/03/2012  Valerian  UD1K953663  HMMA Engine Plant split
*                                    implementation
*----------------------------------------------------------------------
PROGRAM ZGGBR000 .
*---------------------------------------------------------------------*
*                                                                     *
*   Regeln: EXIT-Formpool for Uxxx-Exits                              *
*                                                                     *
*   This formpool is used by SAP for demonstration purposes only.     *
*                                                                     *
*   Note: If you define a new user exit, you have to enter your       *
*         user exit in the form routine GET_EXIT_TITLES.              *
*                                                                     *
*---------------------------------------------------------------------*
INCLUDE FGBBGD00.               "Data types

*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*
*    PLEASE INCLUDE THE FOLLOWING "TYPE-POOL"  AND "TABLES" COMMANDS  *
*        IF THE ACCOUNTING MODULE IS INSTALLED IN YOUR SYSTEM         *
TYPE-POOLS: GB002. " TO BE INCLUDED IN
TABLES: BKPF,      " ANY SYSTEM THAT
        BSEG,      " HAS 'FI' INSTALLED
        COBL,
        GLU1.
*{   INSERT         KA5K001798                                        1
TYPE-POOLS: GB002.
TABLES: CNMMDATES.
*}   INSERT
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*

TABLES: T025.
TABLES: ZTFI_CTL.

*----------------------------------------------------------------------*
*       FORM GET_EXIT_TITLES                                           *
*----------------------------------------------------------------------*
*       returns name and title of all available standard-exits         *
*       every exit in this formpool has to be added to this form.      *
*       You have to specify a parameter type in order to enable the    *
*       code generation program to determine correctly how to          *
*       generate the user exit call, i.e. how many and what kind of    *
*       parameter(s) are used in the user exit.                        *
*       The following parameter types exist:                           *
*                                                                      *
*       TYPE                Description              Usage             *
*    ------------------------------------------------------------      *
*       C_EXIT_PARAM_NONE   Use no parameter         Subst. and Valid. *
*                           except B_RESULT                            *
*       C_EXIT_PARAM_CLASS  Use a type as parameter  Subst. and Valid  *
*----------------------------------------------------------------------*
*  -->  EXIT_TAB  table with exit-name and exit-titles                 *
*                 structure: NAME(5), PARAM(1), TITEL(60)
*----------------------------------------------------------------------*
FORM GET_EXIT_TITLES TABLES ETAB.

  DATA: BEGIN OF EXITS OCCURS 50,
          NAME(5)   TYPE C,
          PARAM     LIKE C_EXIT_PARAM_NONE,
          TITLE(60) TYPE C,
        END OF EXITS.
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
* PLEASE DELETE THE FIRST '*' FORM THE BEGINING OF THE FOLLOWING LINES *
*        IF THE ACCOUNTING MODULE IS INSTALLED IN YOUR SYSTEM:         *
*  EXITS-NAME  = 'U101'.
*  EXITS-PARAM = C_EXIT_PARAM_CLASS.
*  EXITS-TITLE = TEXT-100.                 "Posting date check
*  APPEND EXITS.

  EXITS-NAME  = 'U100'.
  EXITS-PARAM = C_EXIT_PARAM_NONE.        "Complete data used in exit.
  EXITS-TITLE = TEXT-101.                 "Posting date check
  APPEND EXITS.

  EXITS-NAME  = 'UZ01'.
  EXITS-PARAM = C_EXIT_PARAM_NONE.        "Complete data used in exit.
  EXITS-TITLE = 'Check MFBF'.
  APPEND EXITS.

* forms for SAP_EIS
*   EXITS-NAME  = 'US001'.                  "single validation: only one
*   EXITS-PARAM = C_EXIT_PARAM_NONE.        "data record used
*   EXITS-TITLE = TEXT-102.                 "Example EIS
*   APPEND EXITS.
*
*   EXITS-NAME  = 'UM001'.                  "matrix validation:
*   EXITS-PARAM = C_EXIT_PARAM_CLASS.       "complete data used in exit.
*   EXITS-TITLE = TEXT-103.                 "Example EIS
*   APPEND EXITS.

* added by Andy Choi
  EXITS-NAME  = 'UV01'.
  EXITS-PARAM = C_EXIT_PARAM_NONE.
  EXITS-TITLE = 'NO USE'.
  APPEND EXITS.

  EXITS-NAME  = 'UV02'.
  EXITS-PARAM = C_EXIT_PARAM_NONE.
  EXITS-TITLE = 'Check Tax Code'.
  APPEND EXITS.

  EXITS-NAME  = 'UVC1'.
  EXITS-PARAM = C_EXIT_PARAM_NONE.
  EXITS-TITLE = 'CO Material Check'.
  APPEND EXITS.
  EXITS-NAME  = 'UVC2'.
  EXITS-PARAM = C_EXIT_PARAM_NONE.
  EXITS-TITLE = 'CO Account,Order Check'.
  APPEND EXITS.

  REFRESH ETAB.
  LOOP AT EXITS.
    ETAB = EXITS.
    APPEND ETAB.
  ENDLOOP.

ENDFORM.

*eject
FORM UV01  USING B_RESULT.
  B_RESULT  = B_TRUE.              " Default
ENDFORM.


*----------------------------------------------------------------------*
* Check MM Std cost exist
*----------------------------------------------------------------------*
*
*  CHECK bseg-bschl >= '80'.
*
*  TABLES: mbew.
*  DATA: l_stprs LIKE mbew-stprs,  "standard cost
*        l_vprsv LIKE mbew-vprsv.
*
*  CLEAR l_stprs.
*  SELECT SINGLE stprs vprsv
*    INTO (l_stprs, l_vprsv)
*    FROM mbew
*    WHERE bwkey = bseg-bwkey
*      AND matnr = bseg-matnr
*      AND bwtar = bseg-bwtar.
*
*  CHECK l_vprsv = 'S'.
*  IF l_stprs = 0.
*    b_result  = b_false.
*  ENDIF.
*
*----------------------------------------------------------------------*
* Check tax code
*----------------------------------------------------------------------*
FORM UV02  USING B_RESULT.

  B_RESULT  = B_TRUE.              " Default

  CHECK BSEG-MWSKZ <> SPACE AND BSEG-MWSKZ <> '**'.

  TABLES: Z903.
  SELECT SINGLE * FROM Z903
     WHERE BUKRS = BKPF-BUKRS
       AND MWSKZ = BSEG-MWSKZ
       AND DATBI >= BKPF-BUDAT.

  IF SY-SUBRC <> 0.
* no entry... default = true.
    B_RESULT = B_FALSE.
  ELSE.
* valid start date > document date
    IF Z903-DATAB > BKPF-BUDAT.
      B_RESULT = B_FALSE.
    ENDIF.
  ENDIF.

* check invoice :production material = 'U0'.
  CHECK Z903-NOCHK = SPACE.

  DATA: L_BKLAS LIKE MBEW-BKLAS,
        L_KKREF LIKE T025-KKREF.

  SELECT SINGLE * FROM ZTFI_CTL
        WHERE BUKRS = BKPF-BUKRS
          AND CATEG = 'ZGG_TAX'
          AND FLAG  = 'X'.
  IF SY-SUBRC = 0 AND BKPF-GLVOR = 'RMRP' AND BSEG-BSCHL >= '80'.

    SELECT SINGLE BKLAS INTO L_BKLAS
      FROM MBEW
      WHERE BWKEY = BSEG-BWKEY
        AND MATNR = BSEG-MATNR
        AND BWTAR = BSEG-BWTAR.
    SELECT SINGLE KKREF INTO L_KKREF
      FROM T025
      WHERE BKLAS = L_BKLAS.

    IF L_KKREF CA '12' AND BSEG-MWSKZ <> 'U0'.
      B_RESULT = B_FALSE.
    ENDIF.
  ENDIF.

*BKLAS KKREF  BKBEZ
*3000  0001   Production Mat'l-Import
*3001  0001   Production Mat'l-Domesti
*3002  0002   Raw Mat'l-Import
*3003  0002   Raw Mat'l-Domestic
*3004  0002   Sub Material
*3005  0001   Production Mat'l-JIS
*3040  0003   Non-Production Stock Mat
*3050  0004   Packaging Material
*3100  0005   Trading Goods
*3200  0006   Services
*7900  0001   Semifinished products
*7920  0009   Finished products

ENDFORM.
*----------------------------------------------------------------------*
* Check CO material
*----------------------------------------------------------------------*
FORM UVC1  USING B_RESULT.
  RANGES: R_BKLAS FOR T025-BKLAS.
  B_RESULT  = B_TRUE.              " Default

* CHECK cobl-glvor = 'RMRU'.
*RMRP-invoice, RMRU-PP,   RMWE-GR, RMWA-GR etc,
*RMWI-physical inventory
*RMPR-PR chg,  RMBL-mr22, RMM1-ML,
*RMWL-Delivery after price chg

*COBL-AWTYP = 'MKPF'

*Check valuation amount
  DATA: L_ABSAMT LIKE COBL-DMBTR.

  IF COBL-KTOSL = 'GBB'.  "GR:GBB-AUF,GI-GBB-OTH
    SELECT SINGLE * FROM ZTFI_CTL
        WHERE BUKRS = COBL-BUKRS
          AND CATEG = 'ZGG_STD'
          AND FLAG  = 'X'.
    IF SY-SUBRC = 0 AND COBL-DMBTR IS INITIAL.
      B_RESULT = B_FALSE.
    ENDIF.
  ENDIF.

**Check valuation amount
*  IF cobl-dmbtr IS INITIAL.
*    b_result = b_false.
*  ENDIF.

*Block FSC material GI to PCC
  IF   COBL-KTOSL = 'GBB'
  AND  COBL-AUTYP = '05'   "PCC
  AND ( COBL-BWART = '261' OR COBL-BWART = '262' ).
    IF COBL-BKLAS = '7920'.  " IN r_bklas.
      B_RESULT = B_FALSE.
    ENDIF.
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
* Check CO order, account, ...
*----------------------------------------------------------------------*
FORM UVC2  USING B_RESULT.
  DATA: L_CNT LIKE SY-TABIX.
  B_RESULT  = B_TRUE.              " Default

  IF COBL-HKONT+4(2) = '54'.  " material cost.
    CASE COBL-AUFNR.
      WHEN 'CP001'.
        IF COBL-WERKS <> 'P001'.
          B_RESULT = B_FALSE.
        ENDIF.

      WHEN 'CE001'.
        IF COBL-WERKS <> 'E001'.
          B_RESULT = B_FALSE.
        ENDIF.

* BEGIN OF UD1K953663
      WHEN 'CE002'.
        IF COBL-WERKS <> 'E002'.
          B_RESULT = B_FALSE.
        ENDIF.
* END OF UD1K953663

      WHEN OTHERS.
* Subcontract GR/GI - NO CHECK!!!
        IF COBL-EBELN = SPACE.
          SELECT COUNT( * ) INTO L_CNT FROM AFKO
            WHERE AUFNR = COBL-AUFNR.
          IF SY-SUBRC <> 0.
            B_RESULT = B_FALSE.
          ENDIF.
        ENDIF.
    ENDCASE.

  ENDIF.

* check MIP not CP001, CE001
* valuation class & material type
  DATA: L_ABSAMT LIKE COBL-DMBTR.
  IF  ( COBL-BKLAS = '7900'  OR COBL-BKLAS = '7920' )
  AND ( COBL-AUFNR = 'CP001' OR COBL-AUFNR = 'CE001' ).
    DATA: L_MTART TYPE MTART.
    SELECT SINGLE MTART INTO L_MTART FROM MARA WHERE MATNR = COBL-MATNR.
    IF L_MTART(3) = 'ROH'.
      L_ABSAMT = ABS( COBL-DMBTR ).
      SELECT SINGLE * FROM ZTFI_CTL
          WHERE BUKRS = COBL-BUKRS
            AND CATEG = 'ZGG_MIP'
            AND FLAG  = 'X'.
      IF SY-SUBRC = 0 AND ZTFI_CTL-ZUONR <> COBL-MATNR
      AND L_ABSAMT > ZTFI_CTL-DMBTR.
        B_RESULT = B_FALSE.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*       FORM U100                                                      *
*----------------------------------------------------------------------*
*       Example of an exit for a boolean rule                          *
*       This exit can be used in FI for callup points 1,2 or 3.        *
*----------------------------------------------------------------------*
*  <--  B_RESULT    T = True  F = False                                *
*----------------------------------------------------------------------*
FORM U100  USING B_RESULT.

*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
* PLEASE DELETE THE FIRST '*' FORM THE BEGINING OF THE FOLLOWING LINES *
*        IF THE ACCOUNTING MODULE IS INSTALLED IN YOUR SYSTEM:         *
*
*   IF SY-DATUM = BKPF-BUDAT.
*     B_RESULT  = B_TRUE.
*  ELSE.
*    B_RESULT  = B_FALSE.
*  ENDIF.
*{   INSERT         KA5K001798                                        1
  DATA: TMP_DATUM LIKE SY-DATUM.
  TMP_DATUM = CNMMDATES-BADAT + 10.
  IF CNMMDATES-BDTER > TMP_DATUM.
    B_RESULT = B_TRUE.
  ENDIF.
*}   INSERT

ENDFORM.

*---------------------------------------------------------------------*
*       FORM UZ01                                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  B_RESULT                                                      *
*---------------------------------------------------------------------*
FORM UZ01  USING B_RESULT.
*
*   IF SY-DATUM = BKPF-BUDAT.
*     B_RESULT  = B_TRUE.
*  ELSE.
*    B_RESULT  = B_FALSE.
*  ENDIF.
ENDFORM.

*eject
*----------------------------------------------------------------------*
*       FORM U101                                                      *
*----------------------------------------------------------------------*
*       Example of an exit using the complete data from one            *
*       multi-line rule.                                               *
*       This exit is intended for use from callup point 3, in FI.      *
*                                                                      *
*       If account 400000 is used, then account 399999 must be posted  *
*       to in another posting line.                                    *
*----------------------------------------------------------------------*
*  -->  BOOL_DATA   The complete posting data.                         *
*  <--  B_RESULT    T = True  F = False                                *
*----------------------------------------------------------------------*

*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
* PLEASE DELETE THE FIRST '*' FORM THE BEGINING OF THE FOLLOWING LINES *
*        IF THE ACCOUNTING MODULE IS INSTALLED IN YOUR SYSTEM:         *
*FORM u101 USING    bool_data TYPE gb002_015
*          CHANGING B_RESULT.
*  DATA: B_ACC_400000_USED LIKE D_BOOL VALUE 'F'.
*
*  B_RESULT = B_TRUE.
** Has account 400000 has been used?
*  LOOP AT BOOL_DATA-BSEG INTO BSEG
*                 WHERE HKONT  = '0000400000'.
*     B_ACC_400000_USED = B_TRUE.
*     EXIT.
*  ENDLOOP.
*
** Check that account 400000 has been used.
*  CHECK B_ACC_400000_USED = B_TRUE.
*
*  B_RESULT = B_FALSE.
*  LOOP AT BOOL_DATA-BSEG INTO BSEG
*                 WHERE HKONT  = '0000399999'.
*     B_RESULT = B_TRUE.
*     EXIT.
* ENDLOOP.
*
*ENDFORM.

*eject
*----------------------------------------------------------------------*
*       FORM US001
*----------------------------------------------------------------------*
*       Example of an exit for a boolean rule in SAP-EIS
*       for aspect 001 (single validation).
*       one data record is transfered in structure CF<asspect>
*----------------------------------------------------------------------
*       Attention: for any FORM one has to make an entry in the
*       form GET_EXIT_TITLES at the beginning of this include
*----------------------------------------------------------------------*
*  <--  B_RESULT    T = True  F = False                                *
*----------------------------------------------------------------------*
FORM US001 USING B_RESULT.

*TABLES CF001.                                 "table name aspect 001
*
*  IF ( CF001-SPART = '00000001' OR
*       CF001-GEBIE = '00000001' ) AND
*       CF001-ERLOS >= '1000000'.
*
**   further checks ...
*
*    B_RESULT  = B_TRUE.
*  ELSE.
*
**   further checks ...
*
*    B_RESULT  = B_FALSE.
*  ENDIF.

ENDFORM.                                                    "US001

*eject
*----------------------------------------------------------------------*
*       FORM UM001
*----------------------------------------------------------------------*
*       Example of an exit for a boolean rule in SAP-EIS
*       for aspect 001 (matrix validation).
*       Data is transfered in BOOL_DATA:
*       BOOL_DATA-CF<aspect> is intern table of structure CF<asspect>
*----------------------------------------------------------------------
*       Attention: for any FORM one has to make an entry in the
*       form GET_EXIT_TITLES at the beginning of this include
*----------------------------------------------------------------------*
*  <--  B_RESULT    T = True  F = False                                *
*----------------------------------------------------------------------*
FORM UM001 USING BOOL_DATA    "TYPE GB002_<boolean class of aspect 001>
           CHANGING B_RESULT.

*DATA: LC_CF001 LIKE CF001.
*DATA: LC_COUNT TYPE I.

*  B_RESULT = B_TRUE.
*  CLEAR LC_COUNT.
*  process data records in BOOL_DATA
*  LOOP AT BOOL_DATA-CF001 INTO LC_CF001.
*    IF LC_CF001-SPART = '00000001'.
*      ADD 1 TO LC_COUNT.
*      IF LC_COUNT >= 2.
**       division '00000001' may only occur once !
*        B_RESULT = B_FALSE.
*        EXIT.
*      ENDIF.
*    ENDIF.
*
**   further checks ....
*
*  ENDLOOP.

ENDFORM.                                                    "UM001

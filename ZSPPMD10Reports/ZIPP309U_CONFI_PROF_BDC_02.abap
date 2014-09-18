************************************************************************
* Program Name      : ZIPP309U_CONFI_PROF_BDC_02
*   ( Copy of ZIPP309U_CONFI_PROF_BDC )
* Author            : Manjunath
* Creation Date     : 06/15/2007
* Specifications By : CH
* Development Request No : UD1K902103
* Addl Documentation:
* Description       : CONFIGURATION PROFILE
************************************************************************
* Modification Logs
* Date       Developer    RequestNo    Description
* 06/15/2007 Manju        UD1K905279   CONFIGURATION PROFILE
*
************************************************************************
REPORT ZIPP309U_CONFI_PROF_BDC_02
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID ZMBM.

TABLES : KLAH, KSML, CABNT.

DEFINE __CLS.                          " clear & refresh
  CLEAR &1.REFRESH &1.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
* INCLUDE
*----------------------------------------------------------------------*
INCLUDE ZIPP309L_CONFI_PROF_BDC_T_02.
INCLUDE ZIPP309L_CONFI_PROF_BDC_F01_02.
*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.
*  PERFORM SCREEN_MODIFY.


START-OF-SELECTION.
  PERFORM READ_PROCESS.
  PERFORM DATA_MAST_CHECK.
  PERFORM DATA_PROCESS.
  PERFORM BDC_PROCESS.
  PERFORM UPDATE_PROCESS.


END-OF-SELECTION.

  PERFORM WRITE_PROCESS.
*&---------------------------------------------------------------------*
*&      Form  CU41_BDC_PROCESS_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CU41_BDC_PROCESS_NEW.

  DATA: L_TABIX TYPE SY-TABIX,
        L_MSG  LIKE CFGNL-MSGLIN,
        L_INDEX TYPE I,
        L_CNT(2) TYPE N,
        L_FIELD(30) TYPE C,
        L_FIELD1(30) TYPE C.

*/////////////////////////////////////////*
* by IG.MOON 8/24/2007 {

  DATA $OPT(300).
  DATA : BEGIN OF IT_ATBEZ OCCURS 0, " For Status Modify
           CLID     LIKE KLAH-CLASS,
           ATNAM     LIKE CABN-ATNAM,
           ATBEZ     LIKE CABNT-ATBEZ,
           VALUE(2),
         END   OF  IT_ATBEZ.
* }
*/////////////////////////////////////////*

  LOOP AT IT_ACFI.

    L_TABIX = SY-TABIX.

*/////////////////////////////////////////*
* by IG.MOON 8/24/2007 {
    CLEAR $OPT.
    CONCATENATE IT_ACFI-OPT1 IT_ACFI-OPT2 INTO $OPT.
    CONDENSE $OPT.

    SELECT A~CLASS
           C~ATNAM
           D~ATBEZ INTO TABLE IT_ATBEZ
    FROM KLAH AS A
    INNER JOIN KSML AS B
    ON B~CLINT EQ A~CLINT
    INNER JOIN CABN AS C
    ON C~ATINN EQ B~IMERK
    INNER JOIN CABNT AS D
    ON D~ATINN EQ C~ATINN
    WHERE A~CLASS EQ IT_ACFI-CLID.

    LOOP AT IT_ATBEZ.
      CLEAR L_INDEX.

      IF IT_ATBEZ-ATNAM NE 'COL_EXT'
          AND IT_ATBEZ-ATNAM NE 'COL_INT'.
        IT_ATBEZ-VALUE = '-'.
      ENDIF.

      SEARCH $OPT FOR IT_ATBEZ-ATBEZ AND MARK.

      IF SY-SUBRC EQ 0.
        L_INDEX = SY-FDPOS + 4.
        IT_ATBEZ-VALUE = $OPT+L_INDEX(2).
      ENDIF.
      MODIFY IT_ATBEZ.

    ENDLOOP.
* }
*/////////////////////////////////////////*

    PERFORM DYNPRO USING:
       'X' 'SAPLCUCO'    '0110',
       ' ' 'RCUCO-MATNR' IT_ACFI-MTNO,   "
       ' ' 'RMCLF-AENNR1' IT_ACFI-EONO,   "
       ' ' 'BDC_OKCODE'  '/00',

       'X' 'SAPLCUCO'    '0200',
       ' ' 'RCUCO-PRIO(01)' IT_ACFI-PRIT,   "
       ' ' 'RCUCO-PRFID(01)' IT_ACFI-PROF,   "
       ' ' 'RCUCO-KLART(01)' IT_ACFI-CLTY,   "
       ' ' 'BDC_OKCODE'  '/00',

       'X' 'SAPLCUCO'    '0200',
       ' ' 'RCUCO-CLSEL(01)' 'X',   "
       ' ' 'BDC_OKCODE'  '=MCLA',

       'X' 'SAPLCUCO'    '0200',
       ' ' 'RCUCO-CLSEL(01)' 'X',
       ' ' 'BDC_OKCODE'  '=OCLA'.

*    IF IT_ACFI-CLID EQ 'CR_COL'.
*      PERFORM DYNPRO USING:
*       'X' 'SAPLCLFM'    '0500',
*       ''  'RMCLF-KREUZ(01)' 'X',
*      ''  'RMCLF-CLASS(01)' IT_ACFI-CLID,   "
*        '' 'RMCLF-ZAEHL(01)' '30',
*       ' ' 'BDC_OKCODE'  '=AUSW'.
*    ELSE.
      PERFORM DYNPRO USING:
    'X' 'SAPLCLFM'    '0500',
    ''  'RMCLF-KREUZ(01)' 'X',
   ' ' 'RMCLF-CLASS(01)' IT_ACFI-CLID,   "
     '' 'RMCLF-ZAEHL(01)' '30',
    ' ' 'BDC_OKCODE'  '=AUSW'.
*    ENDIF.

    PERFORM DYNPRO USING:
         'X' 'SAPLCTMS'    '0109'.
    L_INDEX = 0.
    L_CNT = 0.

    LOOP AT IT_ATBEZ.
      L_CNT = L_CNT + 1.
      CONCATENATE 'RCTMS-MNAME(' L_CNT ')' INTO L_FIELD.
      CONCATENATE 'RCTMS-MWERT(' L_CNT ')' INTO L_FIELD1.

      CASE IT_ATBEZ-ATNAM.
        WHEN 'COL_CAR_SPEC'.
          PERFORM DYNPRO USING:
        ''  L_FIELD   IT_ATBEZ-ATNAM,
        ''  L_FIELD1  IT_ACFI-CSPEC.
        WHEN 'COL_DT'.
          PERFORM DYNPRO USING:
         ''  L_FIELD  IT_ATBEZ-ATNAM,
         ''  L_FIELD1 IT_ACFI-DRTY.
        WHEN 'COL_WT'.
          PERFORM DYNPRO USING:
         ''  L_FIELD  IT_ATBEZ-ATNAM,
         ''  L_FIELD1 IT_ACFI-WETY.
        WHEN 'COL_BT'.
          PERFORM DYNPRO USING:
         ''  L_FIELD  IT_ATBEZ-ATNAM,
         ''  L_FIELD1 IT_ACFI-BOTY.
        WHEN 'COL_EC'.
          PERFORM DYNPRO USING:
         ''  L_FIELD  IT_ATBEZ-ATNAM,
         ''  L_FIELD1 IT_ACFI-ENCAPA.
        WHEN 'COL_ET'.
          PERFORM DYNPRO USING:
         ''  L_FIELD  IT_ATBEZ-ATNAM,
         ''  L_FIELD1 IT_ACFI-ENTY.
        WHEN 'COL_FT'.
          PERFORM DYNPRO USING:
         ''  L_FIELD  IT_ATBEZ-ATNAM,
         ''  L_FIELD1 IT_ACFI-FUTY.
        WHEN 'COL_TM'.
          PERFORM DYNPRO USING:
         ''  L_FIELD  IT_ATBEZ-ATNAM,
         ''  L_FIELD1 IT_ACFI-TMCD.

        WHEN 'COL_SP'.
          PERFORM DYNPRO USING:
         ''  L_FIELD  IT_ATBEZ-ATNAM,
         ''  L_FIELD1 '-'.

        WHEN 'COL_NAT'.
          PERFORM DYNPRO USING:
         ''  L_FIELD  IT_ATBEZ-ATNAM,
         ''  L_FIELD1 IT_ACFI-NATION.

        WHEN 'COL_GR'.
          PERFORM DYNPRO USING:
         ''  L_FIELD  IT_ATBEZ-ATNAM,
         ''  L_FIELD1 IT_ACFI-GRAD.

        WHEN OTHERS.
          PERFORM DYNPRO USING:
             ' ' L_FIELD  IT_ATBEZ-ATNAM,
             ''  L_FIELD1 IT_ATBEZ-VALUE.

      ENDCASE.

* Page Down after entering 10 Rows
      IF L_CNT EQ 10.
        PERFORM DYNPRO USING:
      ''  'BDC_CURSOR'  L_FIELD1,
      ''  'BDC_OKCODE'  '/00',
      'X' 'SAPLCTMS'    '0109',
      ''  'BDC_CURSOR'  'RCTMS-MNAME(01)'.
        L_CNT = 0.
      ENDIF.

    ENDLOOP.

    PERFORM DYNPRO USING:
        ''  'BDC_CURSOR'  L_FIELD1,
        ''  'BDC_OKCODE'   '/00',
        ''  'BDC_CURSOR'  'RCTMS-MWERT(01)',
        ' ' 'BDC_OKCODE'  '=BACK',
        'X' 'SAPLCLFM'    '0500',
        ' ' 'BDC_OKCODE'  '=SAVE'.

    CALL TRANSACTION 'CU41'
                            USING IT_BDC
                            OPTIONS FROM WA_OPT
                            MESSAGES INTO IT_MESS.
    IT_ACFI-ZRESULT = SY-MSGTY.
    IT_ACFI-ZMODE = 'C'.
    PERFORM RKC_MSG_STRING CHANGING L_MSG.

    PERFORM IT_ACFI_MODIFY USING L_MSG
                                 SY-MSGTY
                                 L_TABIX.
    REFRESH: IT_BDC, IT_MESS.
    CLEAR: IT_BDC, IT_MESS, IT_ACFI.
  ENDLOOP.

ENDFORM.                    " CU41_BDC_PROCESS_NEW

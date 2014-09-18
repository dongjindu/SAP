************************************************************************
* Program Name      : ZASD03R_REC_LIST2
* Author            : jun ho choi
* Creation Date     : 2003.08.12.
* Specifications By : jun ho choi
* Pattern           : 1-2
* Development Request No : UD1K904910
* Addl Documentation:
* Description       : Display REC list(detail)
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 08.19.2014      Victor     T-code has been deleted for APM         *
*
*
************************************************************************
REPORT ZASD03R_REC_LIST2 NO STANDARD PAGE HEADING
                         LINE-SIZE 220
                         MESSAGE-ID ZMSD.


*
TABLES : ZTSD_REC_H,
         ZTSD_REC_I,
         ZTSD_ACM_H,
         ZTSD_ACM_I.


*
DATA : BEGIN OF IT_REC_H OCCURS 0.
       INCLUDE STRUCTURE ZTSD_REC_H.
DATA : END OF IT_REC_H.

DATA : BEGIN OF IT_REC_I OCCURS 0.
       INCLUDE STRUCTURE ZTSD_REC_I.
DATA : END OF IT_REC_I.

DATA : BEGIN OF IT_ACM_H OCCURS 0.
       INCLUDE STRUCTURE ZTSD_ACM_H.
DATA : END OF IT_ACM_H.

DATA : BEGIN OF IT_ACM_I OCCURS 0.
       INCLUDE STRUCTURE ZTSD_ACM_I.
DATA : END OF IT_ACM_I.

DATA : W_CNT TYPE I.


*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS : P_ZISSN LIKE ZTSD_REC_I-ZISSN,
             P_ZVEND LIKE ZTSD_REC_I-ZVEND,
             P_ZACLN LIKE ZTSD_REC_I-ZACLN,
             P_ZCDST LIKE ZTSD_REC_I-ZCDST,
             P_ZCDLR LIKE ZTSD_REC_I-ZCDLR,
             P_ZCSER LIKE ZTSD_REC_I-ZCSER.
SELECTION-SCREEN END OF BLOCK B1.


*
START-OF-SELECTION.
  PERFORM GET_DATA.


*
END-OF-SELECTION.
  PERFORM DISPLAY_DATA.







*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA.
  SELECT *
         INTO TABLE IT_REC_H
         FROM ZTSD_REC_H
        WHERE ZISSN EQ P_ZISSN
        AND   ZVEND EQ P_ZVEND
        AND   ZACLN EQ P_ZACLN
        AND   ZCDST EQ P_ZCDST
        AND   ZCDLR EQ P_ZCDLR
        AND   ZCSER EQ P_ZCSER.

  SELECT *
         INTO TABLE IT_REC_I
         FROM ZTSD_REC_I
        WHERE ZISSN EQ P_ZISSN
        AND   ZVEND EQ P_ZVEND
        AND   ZACLN EQ P_ZACLN
        AND   ZCDST EQ P_ZCDST
        AND   ZCDLR EQ P_ZCDLR
        AND   ZCSER EQ P_ZCSER.

  SELECT *
         INTO TABLE IT_ACM_H
         FROM ZTSD_ACM_H
        WHERE ZACLN EQ P_ZACLN
        AND   ZCDST EQ P_ZCDST
        AND   ZCDLR EQ P_ZCDLR
        AND   ZCSER EQ P_ZCSER.

  SELECT *
         INTO TABLE IT_ACM_I
         FROM ZTSD_ACM_I
        WHERE ZACLN EQ P_ZACLN
        AND   ZCDST EQ P_ZCDST
        AND   ZCDLR EQ P_ZCDLR
        AND   ZCSER EQ P_ZCSER.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
FORM DISPLAY_DATA.
  DESCRIBE TABLE IT_REC_H LINES W_CNT.
  IF W_CNT = 0.
    MESSAGE I000 WITH TEXT-M01.
    STOP.
  ENDIF.

  READ TABLE IT_REC_H INDEX 1.
  READ TABLE IT_ACM_H INDEX 1.

  IF IT_ACM_H-ZCTYP = 'C'.
    PERFORM DISPLAY_CAMPAIGN.
  ELSE.
    PERFORM DISPLAY_WARRANTY.
  ENDIF.
ENDFORM.                    " DISPLAY_ITEM
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_WARRANTY
*&---------------------------------------------------------------------*
FORM DISPLAY_WARRANTY.
  DATA : W_CLAIM(26).

  CONCATENATE IT_ACM_H-ZCDST IT_ACM_H-ZCDLR IT_ACM_H-ZCSER
              INTO W_CLAIM SEPARATED BY SPACE.

  WRITE:/0  'Vendor '   COLOR COL_HEADING, IT_REC_H-ZVEND,
         30 'Issue No.' COLOR COL_HEADING, IT_REC_H-ZISSN.
  WRITE:/0  'ACL No.'   COLOR COL_HEADING, IT_ACM_H-ZACLN,
         30 'Claim No.' COLOR COL_HEADING, W_CLAIM,
         70 'Type'      COLOR COL_HEADING, IT_ACM_H-ZCTYP,
         95 'Status'    COLOR COL_HEADING, IT_REC_H-ZCSTS.

  WRITE:/(121) SY-ULINE.
  WRITE:/(11)   'P/V Flag'      COLOR COL_HEADING,
                                IT_ACM_H-ZRSFG, '/', IT_ACM_H-ZVSFG,
         40(13) 'RO No'         COLOR COL_HEADING, IT_ACM_H-ZRONM,
         73(09) 'PWA Type'      COLOR COL_HEADING, IT_ACM_H-ZPWT1,
        102(06) 'PWA No'        COLOR COL_HEADING, IT_ACM_H-ZPWNO.
  WRITE:/(11)   'VIN'           COLOR COL_HEADING, IT_ACM_H-ZVIN,
         40(13) 'Dlvy Dt'       COLOR COL_HEADING, IT_ACM_H-ZDLVY,
         73(09) 'Repr Dt'       COLOR COL_HEADING, IT_ACM_H-ZRPDT,
        102(06) 'Odmt'          COLOR COL_HEADING, IT_ACM_H-ZODRD.
  WRITE:/40(13) 'P/RO C.Ticket' COLOR COL_HEADING, IT_ACM_H-ZPRON,
         73(09) 'P/Repr Dt'     COLOR COL_HEADING, IT_ACM_H-ZPIDT,
        102(06) 'P/Odmt'        COLOR COL_HEADING, IT_ACM_H-ZPODR.
  WRITE:/(11)   'Causal Part'   COLOR COL_HEADING, IT_ACM_H-ZCPTN,
         40(13) 'Nat/Cau'       COLOR COL_HEADING,
                                IT_ACM_H-ZNATR, '/', IT_ACM_H-ZCAUS,
         73(09) 'Cond-Des'      COLOR COL_HEADING, IT_ACM_H-ZDESC.
  WRITE:/(11)   'Sublet Type'   COLOR COL_HEADING,
                                IT_ACM_H-ZSBLA, '/', IT_ACM_H-ZSBLB,
         40(13) 'Sublet'        COLOR COL_HEADING,
                                IT_ACM_H-ZRMSS CURRENCY IT_ACM_H-ZPYCR,
                           '/', IT_REC_H-ZRCSS CURRENCY IT_REC_H-ZPYCR.
  WRITE:/(11)   'Adj. Code'     COLOR COL_HEADING, IT_REC_H-ZADJH,
         40(13) 'GR/Ratio'      COLOR COL_HEADING, IT_REC_H-ZSPRT,
         73(09) 'S/Ratio'       COLOR COL_HEADING, IT_REC_H-ZSHAR,
        102(06) 'Curr'          COLOR COL_HEADING, IT_ACM_H-ZPYCR,
                                              '/', IT_REC_H-ZPYCR.

  FORMAT COLOR COL_HEADING.
  WRITE:/(121) SY-ULINE.
  WRITE:/(02) 'Ln',
         (18) 'Replacement',
         (03) 'Qty',
         (12) 'Unit/Price',
         (12) 'App Part',
         (08) 'OP Code',
         (03) 'Qty',
         (04) 'Time',
         (12) 'App Labor',
         (03) 'CAU',
         (03) 'MUP',
         (30) 'Adj. Code'.
  WRITE:/(02) '',
         (18) '',
         (03) '',
         (12) 'Purc/Price',
         (12) 'Rec Part',
         (08) '',
         (03) '',
         (04) '',
         (12) 'Rec Labor',
         (03) '',
         (03) '',
         (30) ''.
  WRITE:/(121) SY-ULINE.
  FORMAT COLOR COL_HEADING OFF.

  LOOP AT IT_ACM_I WHERE ZACLN = IT_ACM_H-ZACLN
                   AND   ZCDST = IT_ACM_H-ZCDST
                   AND   ZCDLR = IT_ACM_H-ZCDLR
                   AND   ZCSER = IT_ACM_H-ZCSER.

    READ TABLE IT_REC_I WITH KEY ZISSN = IT_REC_H-ZISSN
                                 ZVEND = IT_REC_H-ZVEND
                                 ZACLN = IT_REC_H-ZACLN
                                 ZCDST = IT_REC_H-ZCDST
                                 ZCDLR = IT_REC_H-ZCDLR
                                 ZCSER = IT_REC_H-ZCSER
                                 ZLINE = IT_ACM_I-ZLINE.

    WRITE:/(02) IT_ACM_I-ZLINE,
           (18) IT_ACM_I-ZRPPN,
           (03) IT_ACM_I-ZRMPQ,
           (12) IT_ACM_I-ZRMPU CURRENCY IT_ACM_I-ZPYCR,
           (12) IT_ACM_I-ZRMPP CURRENCY IT_ACM_I-ZPYCR,
           (08) IT_ACM_I-ZOPER,
           (03) IT_ACM_I-ZRMLQ,
           (04) IT_ACM_I-ZRMLT+2(3) USING EDIT MASK '__._',
           (12) IT_ACM_I-ZRMLL CURRENCY IT_ACM_I-ZPYCR,
           (03) IT_ACM_I-ZPCAU,
           (03) IT_ACM_I-ZRMUP,
           (30) ''.
    WRITE:/(02) '',
           (18) '',
           (03) '',
           (12) IT_REC_I-ZVPRC CURRENCY IT_REC_I-ZPYCR,
           (12) IT_REC_I-ZRCPP CURRENCY IT_REC_I-ZPYCR,
           (08) '',
           (03) '',
           (04) '',
           (12) IT_REC_I-ZRCLL CURRENCY IT_REC_I-ZPYCR,
           (03) '',
           (03) IT_REC_I-ZVMUP,
           (30) IT_REC_I-ZADJD.
  ENDLOOP.
  WRITE:/(121) SY-ULINE.
ENDFORM.                    " DISPLAY_WARRANTY
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_CAMPAIGN
*&---------------------------------------------------------------------*
FORM DISPLAY_CAMPAIGN.
  DATA : W_CLAIM(26).

  CONCATENATE IT_ACM_H-ZCDST IT_ACM_H-ZCDLR IT_ACM_H-ZCSER
              INTO W_CLAIM SEPARATED BY SPACE.

  WRITE:/0  'Vendor '   COLOR COL_HEADING, IT_REC_H-ZVEND,
         30 'Issue No.' COLOR COL_HEADING, IT_REC_H-ZISSN.
  WRITE:/0  'ACL No.'   COLOR COL_HEADING, IT_ACM_H-ZACLN,
         30 'Claim No.' COLOR COL_HEADING, W_CLAIM,
         70 'Type'      COLOR COL_HEADING, IT_ACM_H-ZCTYP.

  FORMAT COLOR COL_HEADING.
  WRITE:/(124) SY-ULINE.
  WRITE:/(03) 'Seq',
         (06) 'P/Flag',
         (17) 'Vehicle',
         (01) 'V',
         (10) 'Delivery',
         (10) 'Repair',
         (08) 'Odometer',
         (08) 'Issue No',
         (13) 'Curr CL/RCL',
         (10) 'GR/Ratio',
         (10) 'S/Ratio'.
  WRITE:/(03) '',
         (25) 'Part(A)/(R)',
         (25) 'Labor(A)/(R)',
         (25) 'Sublet(A)/(R)',
         (03) 'STS',
         (30) 'Adj. Code'.
  WRITE:/(124) SY-ULINE.
  FORMAT COLOR COL_HEADING OFF.

*  LOOP AT IT_ACM_H.
    WRITE:/(03) IT_ACM_H-ZCSEQ,
           (06) IT_ACM_H-ZRSFG,
           (17) IT_ACM_H-ZVIN,
           (01) IT_ACM_H-ZVSFG,
           (10) IT_ACM_H-ZDLVY,
           (10) IT_ACM_H-ZRPDT,
           (08) IT_ACM_H-ZODRD,
           (08) IT_ACM_H-ZMNOP,
           (05) IT_ACM_H-ZPYCR, '/',
           (05) IT_REC_H-ZPYCR,
           (10) IT_REC_H-ZSPRT,
           (10) IT_REC_H-ZSHAR.
    WRITE:/(03) '',
           (12) IT_ACM_H-ZRMPP CURRENCY IT_ACM_H-ZPYCR,
           (12) IT_REC_H-ZRCPP CURRENCY IT_REC_H-ZPYCR,
           (12) IT_ACM_H-ZRMLL CURRENCY IT_ACM_H-ZPYCR,
           (12) IT_REC_H-ZRCLL CURRENCY IT_REC_H-ZPYCR,
           (12) IT_ACM_H-ZRMSS CURRENCY IT_ACM_H-ZPYCR,
           (12) IT_REC_H-ZRCSS CURRENCY IT_REC_H-ZPYCR,
           (03) IT_REC_H-ZCSTS,
           (30) IT_REC_H-ZADJH.
*  ENDLOOP.
  WRITE:/(124) SY-ULINE.
ENDFORM.                    " DISPLAY_CAMPAIGN

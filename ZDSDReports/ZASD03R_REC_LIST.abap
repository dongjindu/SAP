************************************************************************
* Program Name      : ZASD03R_REC_LIST
* Author            : jun ho choi
* Creation Date     : 2003.08.12.
* Specifications By : jun ho choi
* Pattern           : 1-2
* Development Request No : UD1K904910
* Addl Documentation:
* Description       : Display REC list(header, detail)
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 08.19.2014      Victor     T-code has been deleted for APM         *
*
************************************************************************
REPORT ZASD03R_REC_LIST NO STANDARD PAGE HEADING
                        LINE-SIZE 220
                        MESSAGE-ID ZMSD.


*
TABLES : ZTSD_REC_H,
         ZTSD_REC_I,
         ZTSD_REC_L,
         ZTSD_ACM_H,
         ZTSD_ACM_I.


*
DATA : BEGIN OF IT_REC_H OCCURS 0.
       INCLUDE STRUCTURE ZTSD_REC_H.
DATA : END OF IT_REC_H.

DATA : BEGIN OF IT_REC_I OCCURS 0.
       INCLUDE STRUCTURE ZTSD_REC_I.
DATA : END OF IT_REC_I.

DATA : BEGIN OF IT_REC_L OCCURS 0.
       INCLUDE STRUCTURE ZTSD_REC_L.
DATA : END OF IT_REC_L.

DATA : BEGIN OF IT_ACM_H OCCURS 0.
       INCLUDE STRUCTURE ZTSD_ACM_H.
DATA : END OF IT_ACM_H.

DATA : BEGIN OF IT_ACM_I OCCURS 0.
       INCLUDE STRUCTURE ZTSD_ACM_I.
DATA : END OF IT_ACM_I.

DATA : BEGIN OF IT_ZVEND OCCURS 0,
       SIGN(1),
       OPTION(2),
       LOW LIKE ZTSD_REC_L-ZVEND,
       HIGH LIKE ZTSD_REC_L-ZVEND,
       END   OF IT_ZVEND.

DATA : BEGIN OF IT_ZISSN OCCURS 0,
       SIGN(1),
       OPTION(2),
       LOW LIKE ZTSD_REC_L-ZISSN,
       HIGH LIKE ZTSD_REC_L-ZISSN,
       END   OF IT_ZISSN.

DATA : W_CNT TYPE I,
       W_GUBUN(3),
       W_CURSORFIELD(15).


*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_ZVEND FOR ZTSD_REC_H-ZVEND NO-EXTENSION,
                 S_ZISSN FOR ZTSD_REC_H-ZISSN NO-EXTENSION,
                 S_ZCSTS FOR ZTSD_REC_H-ZCSTS NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN SKIP 1.
PARAMETER : P_ZVEND RADIOBUTTON GROUP RADI,
            P_ZISSN RADIOBUTTON GROUP RADI.
SELECTION-SCREEN SKIP 1.
PARAMETER : P_SUM   RADIOBUTTON GROUP RAD2,
            P_DET   RADIOBUTTON GROUP RAD2.
SELECTION-SCREEN END OF BLOCK B1.



*
TOP-OF-PAGE.
  PERFORM TOP_OF_PAGE USING W_GUBUN P_ZVEND.


*
TOP-OF-PAGE DURING LINE-SELECTION.
  PERFORM TOP_OF_PAGE USING W_GUBUN P_ZVEND.


*
START-OF-SELECTION.
  PERFORM GET_DATA.


*
END-OF-SELECTION.
  PERFORM DISPLAY_DATA.


*
AT LINE-SELECTION.
  GET CURSOR FIELD W_CURSORFIELD.
  CHECK W_CURSORFIELD+0(2) = 'IT'.

  CASE W_GUBUN.
    WHEN 'SUM'.
      W_GUBUN = 'DET'.
      PERFORM DISPLAY_DET USING P_ZVEND.
    WHEN 'DET'.
      W_GUBUN = '   '.
      PERFORM DISPLAY_ITEM.
  ENDCASE.








*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA.
  SELECT *
         INTO TABLE IT_REC_L
         FROM ZTSD_REC_L
        WHERE ZVEND IN S_ZVEND
        AND   ZISSN IN S_ZISSN.

  SELECT *
         INTO TABLE IT_REC_H
         FROM ZTSD_REC_H
        WHERE ZISSN IN S_ZISSN
        AND   ZVEND IN S_ZVEND
        AND   ZCSTS IN S_ZCSTS.

  SELECT *
         INTO TABLE IT_REC_I
         FROM ZTSD_REC_I
        WHERE ZISSN IN S_ZISSN
        AND   ZVEND IN S_ZVEND.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
FORM DISPLAY_DATA.
  DESCRIBE TABLE IT_REC_L LINES W_CNT.
  IF W_CNT = 0.
    MESSAGE I000 WITH TEXT-M01.
    STOP.
  ELSE.
    CASE P_SUM.
      WHEN 'X'.
        W_GUBUN = 'SUM'.
        PERFORM DISPLAY_SUM USING P_ZVEND.
      WHEN ' '.
        W_GUBUN = 'DET'.
        PERFORM DISPLAY_DET USING P_ZVEND.
    ENDCASE.
  ENDIF.
ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_SUM
*&---------------------------------------------------------------------*
FORM DISPLAY_SUM USING VEND.
  IF VEND = 'X'.
    LOOP AT IT_REC_L .
      WRITE:/ SY-VLINE, (06) IT_REC_L-ZVEND,
              SY-VLINE, (08) IT_REC_L-ZISSN,
              SY-VLINE, (14) IT_REC_L-ZRPAA CURRENCY IT_REC_L-ZPYCR,
              SY-VLINE, (14) IT_REC_L-ZRLAA CURRENCY IT_REC_L-ZPYCR,
              SY-VLINE, (14) IT_REC_L-ZRSAA CURRENCY IT_REC_L-ZPYCR,
              SY-VLINE, (07) IT_REC_L-ZRCQT ,
              SY-VLINE, (14) IT_REC_L-ZRCTT CURRENCY IT_REC_L-ZPYCR,
              SY-VLINE.
      HIDE : IT_REC_L-ZVEND, IT_REC_L-ZISSN, W_GUBUN.
    ENDLOOP.
    WRITE:/(99) SY-ULINE.
  ELSE.
    LOOP AT IT_REC_L .
      WRITE:/ SY-VLINE, (08) IT_REC_L-ZISSN,
              SY-VLINE, (06) IT_REC_L-ZVEND,
              SY-VLINE, (14) IT_REC_L-ZRPAA CURRENCY IT_REC_L-ZPYCR,
              SY-VLINE, (14) IT_REC_L-ZRLAA CURRENCY IT_REC_L-ZPYCR,
              SY-VLINE, (14) IT_REC_L-ZRSAA CURRENCY IT_REC_L-ZPYCR,
              SY-VLINE, (07) IT_REC_L-ZRCQT ,
              SY-VLINE, (14) IT_REC_L-ZRCTT CURRENCY IT_REC_L-ZPYCR,
              SY-VLINE.
      HIDE : IT_REC_L-ZVEND, IT_REC_L-ZISSN, W_GUBUN.
    ENDLOOP.
    WRITE:/(99) SY-ULINE.
  ENDIF.
ENDFORM.                    " DISPLAY_SUM
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DET
*&---------------------------------------------------------------------*
FORM DISPLAY_DET USING VEND.
  REFRESH : IT_ZVEND, IT_ZISSN.
  CLEAR   : IT_ZVEND, IT_ZISSN.

  IF P_SUM = 'X'.
    IT_ZVEND-SIGN = 'I'.
    IT_ZVEND-OPTION = 'EQ'.
    IT_ZVEND-LOW = IT_REC_L-ZVEND.
    APPEND IT_ZVEND. CLEAR IT_ZVEND.
    IT_ZISSN-SIGN = 'I'.
    IT_ZISSN-OPTION = 'EQ'.
    IT_ZISSN-LOW = IT_REC_L-ZISSN.
    APPEND IT_ZISSN. CLEAR IT_ZISSN.
  ELSE.
    IT_ZVEND[] = S_ZVEND[].
    IT_ZISSN[] = S_ZISSN[].
  ENDIF.

  IF VEND = 'X'.
    LOOP AT IT_REC_H WHERE ZISSN IN IT_ZISSN
                     AND   ZVEND IN IT_ZVEND.
      WRITE:/ SY-VLINE, (06) IT_REC_H-ZVEND,
              SY-VLINE, (08) IT_REC_H-ZISSN,
              SY-VLINE, (07) IT_REC_H-ZACLN,
              SY-VLINE, (10) IT_REC_H-ZCDST,
              SY-VLINE, (10) IT_REC_H-ZCDLR,
              SY-VLINE, (06) IT_REC_H-ZCSER,
              SY-VLINE, (04) IT_REC_H-ZCSEQ,
              SY-VLINE, (06) IT_REC_H-ZCSTS,
              SY-VLINE, (10) IT_REC_H-ZRCDT,
              SY-VLINE, (10) IT_REC_H-ZSPRT,
              SY-VLINE, (10) IT_REC_H-ZSHAR,
              SY-VLINE, (10) IT_REC_H-ZVOFG,
              SY-VLINE, (05) IT_REC_H-ZPYCR,
              SY-VLINE, (12) IT_REC_H-ZRCPP CURRENCY IT_REC_H-ZPYCR,
              SY-VLINE, (12) IT_REC_H-ZRCLL CURRENCY IT_REC_H-ZPYCR,
              SY-VLINE, (12) IT_REC_H-ZRCSS CURRENCY IT_REC_H-ZPYCR,
              SY-VLINE, (30) IT_REC_H-ZADJH,
              SY-VLINE.
      HIDE : IT_REC_H-ZISSN, IT_REC_H-ZVEND,
             IT_REC_H-ZACLN, IT_REC_H-ZCDST,
             IT_REC_H-ZCDLR, IT_REC_H-ZCSER, W_GUBUN.
    ENDLOOP.
    WRITE:/(220) SY-ULINE.
  ELSE.
    LOOP AT IT_REC_H WHERE ZISSN IN IT_ZISSN
                     AND   ZVEND IN IT_ZVEND.
      WRITE:/ SY-VLINE, (08) IT_REC_H-ZISSN,
              SY-VLINE, (06) IT_REC_H-ZVEND,
              SY-VLINE, (07) IT_REC_H-ZACLN,
              SY-VLINE, (10) IT_REC_H-ZCDST,
              SY-VLINE, (10) IT_REC_H-ZCDLR,
              SY-VLINE, (06) IT_REC_H-ZCSER,
              SY-VLINE, (04) IT_REC_H-ZCSEQ,
              SY-VLINE, (06) IT_REC_H-ZCSTS,
              SY-VLINE, (10) IT_REC_H-ZRCDT,
              SY-VLINE, (10) IT_REC_H-ZSPRT,
              SY-VLINE, (10) IT_REC_H-ZSHAR,
              SY-VLINE, (10) IT_REC_H-ZVOFG,
              SY-VLINE, (05) IT_REC_H-ZPYCR,
              SY-VLINE, (12) IT_REC_H-ZRCPP CURRENCY IT_REC_H-ZPYCR,
              SY-VLINE, (12) IT_REC_H-ZRCLL CURRENCY IT_REC_H-ZPYCR,
              SY-VLINE, (12) IT_REC_H-ZRCSS CURRENCY IT_REC_H-ZPYCR,
              SY-VLINE, (30) IT_REC_H-ZADJH,
              SY-VLINE.
      HIDE : IT_REC_H-ZISSN, IT_REC_H-ZVEND,
             IT_REC_H-ZACLN, IT_REC_H-ZCDST,
             IT_REC_H-ZCDLR, IT_REC_H-ZCSER, W_GUBUN.
    ENDLOOP.
    WRITE:/(220) SY-ULINE.
  ENDIF.
ENDFORM.                    " DISPLAY_DET
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ITEM
*&---------------------------------------------------------------------*
FORM DISPLAY_ITEM.
  SELECT *
         INTO TABLE IT_ACM_H
         FROM ZTSD_ACM_H
        WHERE ZACLN = IT_REC_H-ZACLN
        AND   ZCDST = IT_REC_H-ZCDST
        AND   ZCDLR = IT_REC_H-ZCDLR
        AND   ZCSER = IT_REC_H-ZCSER.
  CHECK SY-SUBRC = 0.

  SELECT *
         INTO TABLE IT_ACM_I
         FROM ZTSD_ACM_I
        WHERE ZACLN = IT_REC_H-ZACLN
        AND   ZCDST = IT_REC_H-ZCDST
        AND   ZCDLR = IT_REC_H-ZCDLR
        AND   ZCSER = IT_REC_H-ZCSER.

  READ TABLE IT_REC_H WITH KEY ZISSN = IT_REC_H-ZISSN
                               ZVEND = IT_REC_H-ZVEND
                               ZACLN = IT_REC_H-ZACLN
                               ZCDST = IT_REC_H-ZCDST
                               ZCDLR = IT_REC_H-ZCDLR
                               ZCSER = IT_REC_H-ZCSER.

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
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM TOP_OF_PAGE USING GUBUN VEND.
  FORMAT COLOR COL_HEADING.
  CASE GUBUN.
    WHEN 'SUM'.
      IF VEND = 'X'.
        WRITE:/(99) SY-ULINE.
        WRITE:/ SY-VLINE, (06) 'Vendor',
                SY-VLINE, (08) 'Issue No',
                SY-VLINE, (14) '   Part Amount',
                SY-VLINE, (14) '  Labor Amount',
                SY-VLINE, (14) ' Sublet Amount',
                SY-VLINE, (07) 'Tot Qty',
                SY-VLINE, (14) '  Total Amount',
                SY-VLINE.
        WRITE:/(99) SY-ULINE.
      ELSE.
        WRITE:/(99) SY-ULINE.
        WRITE:/ SY-VLINE, (08) 'Issue No',
                SY-VLINE, (06) 'Vendor',
                SY-VLINE, (14) '   Part Amount',
                SY-VLINE, (14) '  Labor Amount',
                SY-VLINE, (14) ' Sublet Amount',
                SY-VLINE, (07) 'Tot Qty',
                SY-VLINE, (14) '  Total Amount',
                SY-VLINE.
        WRITE:/(99) SY-ULINE.
      ENDIF.
    WHEN 'DET'.
      IF VEND = 'X'.
        WRITE:/(220) SY-ULINE.
        WRITE:/ SY-VLINE, (06) 'Vendor',
                SY-VLINE, (08) 'Issue No',
                SY-VLINE, (07) 'Acl No',
                SY-VLINE, (10) 'Dist Code',
                SY-VLINE, (10) 'Deal Code',
                SY-VLINE, (06) 'Serial',
                SY-VLINE, (04) 'Line',
                SY-VLINE, (06) 'Status',
                SY-VLINE, (10) 'Process Dt',
                SY-VLINE, (10) 'Ven Ratio',
                SY-VLINE, (10) 'Shr Ratio',
                SY-VLINE, (10) 'Ven Obj Fg',
                SY-VLINE, (05) 'CuKy',
                SY-VLINE, (12) '   Part Amt',
                SY-VLINE, (12) '   Labor Amt',
                SY-VLINE, (12) '  Sublet Amt',
                SY-VLINE, (30) 'Error Message',
                SY-VLINE.
        WRITE:/(220) SY-ULINE.
      ELSE.
        WRITE:/(220) SY-ULINE.
        WRITE:/ SY-VLINE, (08) 'Issue No',
                SY-VLINE, (06) 'Vendor',
                SY-VLINE, (07) 'Acl No',
                SY-VLINE, (10) 'Dist Code',
                SY-VLINE, (10) 'Deal Code',
                SY-VLINE, (06) 'Serial',
                SY-VLINE, (04) 'Line',
                SY-VLINE, (06) 'Status',
                SY-VLINE, (10) 'Process Dt',
                SY-VLINE, (10) 'Ven Ratio',
                SY-VLINE, (10) 'Shr Ratio',
                SY-VLINE, (10) 'Ven Obj Fg',
                SY-VLINE, (05) 'CuKy',
                SY-VLINE, (12) '   Part Amt',
                SY-VLINE, (12) '   Labor Amt',
                SY-VLINE, (12) '  Sublet Amt',
                SY-VLINE, (30) 'Error Message',
                SY-VLINE.
        WRITE:/(220) SY-ULINE.
      ENDIF.
  ENDCASE.
  FORMAT COLOR COL_HEADING OFF.
ENDFORM.                    " TOP_OF_PAGE

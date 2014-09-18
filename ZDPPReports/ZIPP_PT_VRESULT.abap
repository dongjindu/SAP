************************************************************************
* Program Name      : ZIPP_PT_VRESULT
* Author            : Furong Wang
* Creation Date     : 07/01/2010
* Specifications By : Daniel Kim
* Development Request No :
* Addl Documentation:
* Description       : PT: Send Engine Code
* Modification Logs
* Date       Developer    RequestNo    Description
* 08/17/2010 Daniel Kim   UD1K949650   Plant code change (Eng->Veh)
*********************************************************************

REPORT ZIPP_PT_VRESULT NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMPP.
*TABLES:  .

DATA : L_MSGTXT(100),
       L_RESULT(1).

CONSTANTS: C_DEST(10) VALUE 'WMHR01'.   "Interface Destination.

DATA: BEGIN OF IT_PT_VRESULT OCCURS 0.
        INCLUDE STRUCTURE ZTPP_PT_VRESULT.
DATA: END OF IT_PT_VRESULT.

DATA: BEGIN OF IT_OBJEK OCCURS 0,
      OBJEK TYPE AUSP-OBJEK,
        ATWRT TYPE AUSP-ATWRT,
      END OF IT_OBJEK.
DATA: BEGIN OF IT_RP OCCURS 0,
        RP(2),
      END OF IT_RP.

DATA: BEGIN OF IT_TEMP OCCURS 0,
       PRDN_VEHL_CD LIKE ZTPP_PT_VRESULT-PRDN_VEHL_CD,
       NAT_CD LIKE ZTPP_PT_VRESULT-NAT_CD,
       MC_CD LIKE ZTPP_PT_VRESULT-MC_CD,
        OCN_CD LIKE ZTPP_PT_VRESULT-OCN_CD,
       VER_CD LIKE ZTPP_PT_VRESULT-VER_CD,
       USF_SCN_CD LIKE ZTPP_PT_VRESULT-USF_SCN_CD,
       DATE LIKE SY-DATUM,
       MENGE LIKE mseg-menge,
       END OF IT_TEMP.

DATA:  W_RP(2),
W_FR_DATE LIKE SY-DATUM.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
PARAMETERS: P_DATE LIKE SY-DATUM.

SELECTION-SCREEN SKIP 1.
PARAMETERS: P_SEND AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK B1.

INITIALIZATION.
  PERFORM INIT_DATA.

START-OF-SELECTION.
  PERFORM GET_DATA.
  PERFORM SEND_DATA.


END-OF-SELECTION.

*---------------------------------------------------------------------*
*       FORM GET_DATA                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM GET_DATA.


  DATA: L_DATE_C(8),
       L_TIME_C(6).

  L_DATE_C = SY-DATUM.
  L_TIME_C = SY-UZEIT.

  IT_RP-RP = '07'.
  APPEND IT_RP.
  IT_RP-RP = '11'.
  APPEND IT_RP.
  IT_RP-RP = '18'.
  APPEND IT_RP.
  IT_RP-RP = '06'.
  APPEND IT_RP.

  LOOP AT IT_RP.
    W_RP = IT_RP-RP.
    CLEAR: IT_OBJEK, IT_OBJEK[].
    PERFORM GET_VEHICLE_MASTER TABLES IT_OBJEK.
    PERFORM CREATE_DATA.
  ENDLOOP.
  PERFORM MODIFY_DATA.

ENDFORM.


*---------------------------------------------------------------------*
*       FORM SEND_DATA                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM SEND_DATA.
  DATA: IT_SAVE LIKE TABLE OF ZTPP_PT_VRESULT WITH HEADER LINE,
        L_FLAG(1),
      L_YYMM(6),
        L_DATE_C(8).

  L_DATE_C = P_DATE.
  L_YYMM =  L_DATE_C+0(6).

  IF IT_PT_VRESULT[] IS INITIAL.
    MESSAGE I000 WITH 'No data'.
    EXIT.
  ENDIF.
  IF P_SEND IS INITIAL.
    LOOP AT IT_PT_VRESULT.
      MOVE-CORRESPONDING IT_PT_VRESULT TO IT_SAVE.
      IT_SAVE-ZSDAT = SY-DATUM.
      IT_SAVE-ZSTIM = SY-UZEIT.
      APPEND IT_SAVE.
      CLEAR: IT_SAVE.
    ENDLOOP.

*    DELETE FROM ZTPP_PT_PROD  CLIENT SPECIFIED
*              WHERE MANDT = SY-MANDT.
    DELETE FROM ZTPP_PT_VRESULT WHERE CRTN_YYMM = L_YYMM.

    INSERT ZTPP_PT_VRESULT FROM TABLE IT_SAVE.
    IF SY-SUBRC = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
      MESSAGE I000 WITH 'Database table update error'.
    ENDIF.
  ELSE.
    CALL FUNCTION 'Z_FPP_PT_VRESULT'
      DESTINATION C_DEST
      IMPORTING
        FLAG          = L_RESULT
      TABLES
        I_DATA        = IT_PT_VRESULT
      EXCEPTIONS
             COMMUNICATION_FAILURE = 1 MESSAGE L_MSGTXT
             SYSTEM_FAILURE        = 2 MESSAGE L_MSGTXT.

    IF SY-SUBRC = 0.
      IF L_RESULT = 'S'.
        L_FLAG = 'S'.
        L_MSGTXT = 'Data successfully sent out'.
        MESSAGE I001 WITH L_MSGTXT.

      ELSE.
        L_FLAG = 'E'.
        L_MSGTXT =  'Data unsuccessfully sent out'.
        MESSAGE E001 WITH L_MSGTXT.
      ENDIF.
    ELSE.
      L_FLAG = 'E'.
      MESSAGE I001 WITH L_MSGTXT.
    ENDIF.

    LOOP AT IT_PT_VRESULT.
      MOVE-CORRESPONDING IT_PT_VRESULT TO IT_SAVE.
      IT_SAVE-ZRESULT = L_FLAG.
      IT_SAVE-ZMSG = L_MSGTXT.
      IT_SAVE-ZSDAT = SY-DATUM.
      IT_SAVE-ZSTIM = SY-UZEIT.
      IT_SAVE-ZEDAT = SY-DATUM.
      IT_SAVE-ZETIM = SY-UZEIT.
      APPEND IT_SAVE.
      CLEAR: IT_SAVE.
    ENDLOOP.

    DELETE FROM ZTPP_PT_VRESULT WHERE CRTN_YYMM = L_YYMM.

    INSERT ZTPP_PT_VRESULT FROM TABLE IT_SAVE.
    IF SY-SUBRC = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
      MESSAGE I000 WITH 'Table saving error'.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  read_normal_class
*&---------------------------------------------------------------------*
FORM READ_NORMAL_CLASS USING  P_VMNO  P_CHAR
                             CHANGING P_VALUE.
  SELECT SINGLE AU~ATWRT
    INTO P_VALUE
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE OBJEK = P_VMNO      AND
          KLART = '002'       AND
          CA~ATNAM = P_CHAR  .
ENDFORM.                    " read_normal_classification
*&---------------------------------------------------------------------*
*&      Form  READ_NORMAL_CLASS_ATFLV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_OBJEK_OBJEK  text
*      -->P_0461   text
*      <--P_L_ATFLV_TEMP  text
*----------------------------------------------------------------------*
FORM READ_NORMAL_CLASS_ATFLV USING  P_VMNO  P_CHAR
                             CHANGING P_VALUE.
  SELECT SINGLE AU~ATFLV
      INTO P_VALUE
      FROM AUSP AS AU
        INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
      WHERE OBJEK = P_VMNO      AND
            KLART = '002'       AND
            CA~ATNAM = P_CHAR  .
ENDFORM.                    " READ_NORMAL_CLASS_ATFLV
*&---------------------------------------------------------------------*
*&      Form  init_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_DATA.
*  S_DATE-LOW = SY-DATUM - 1.
*  S_DATE-SIGN = 'I'.
*  S_DATE-OPTION = 'EQ'.
*  APPEND S_DATE.

ENDFORM.                    " init_data
*&---------------------------------------------------------------------*
*&      Form  get_vehicle_master_no_app245
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OBJEK  text
*----------------------------------------------------------------------*
FORM GET_VEHICLE_MASTER TABLES  P_IT_OBJEK STRUCTURE IT_OBJEK.

  RANGES: R_DATE   FOR  AUSP-ATFLV .
  DATA: L_SUBRC    TYPE SY-SUBRC ,
        L_ATNAM    TYPE CABN-ATNAM,
        L_ATNAM1   TYPE CABN-ATNAM,
        L_ATWRT    TYPE AUSP-ATWRT,
        L_ATFLV_ST TYPE AUSP-ATFLV,
        L_TEMP(06),
        L_DATUM    TYPE SY-DATUM,
        L_ATFLV_EN TYPE AUSP-ATFLV,
        L_NUM(08) TYPE N,
        L_DATE LIKE SY-DATUM,
        L_DATE_C(8).
  DATA: BEGIN OF LT_OBJEK OCCURS 0,
          OBJEK   LIKE AUSP-OBJEK,
        END OF LT_OBJEK.
  CLEAR L_SUBRC.

  CONCATENATE 'P_RP' W_RP '_SHOP_DATE'  INTO L_ATNAM.

  L_DATE_C = P_DATE.

  CONCATENATE  L_DATE_C+0(6) '01' INTO  L_DATE_C.
  W_FR_DATE = L_DATE_C.

  L_ATFLV_ST = L_NUM = W_FR_DATE.

  L_ATFLV_EN = L_NUM = P_DATE.

  SELECT AU~OBJEK
    INTO TABLE LT_OBJEK
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE KLART = '002'          AND
          AU~ATFLV >= L_ATFLV_ST AND
          AU~ATFLV <= L_ATFLV_EN AND
          CA~ATNAM = L_ATNAM.

  LOOP AT LT_OBJEK.
    IT_OBJEK-OBJEK = LT_OBJEK-OBJEK.

*    R_DATE-SIGN = 'I'. R_DATE-OPTION = 'BT' .

    IF W_RP >= '01' AND W_RP <= '17'.
      PERFORM CHECK_SCRAP_CAR  USING IT_OBJEK-OBJEK   'P_USAGE_CAR'
                                  CHANGING L_SUBRC .

      IF L_SUBRC = 0.
        CONTINUE.
      ENDIF.
    ENDIF.
*    PERFORM CHECK_219_CODE        USING    IT_OBJEK-OBJEK
*                           CHANGING L_SUBRC .
*    IF L_SUBRC <> 0.  CONTINUE.  ENDIF.

    APPEND IT_OBJEK.

  ENDLOOP .
  SORT IT_OBJEK BY OBJEK .
ENDFORM.                    " get_vehicle_master
*---------------------------------------------------------------------*
*       FORM create_data                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM CREATE_DATA.

  DATA: L_RPNO(02) TYPE N          ,
        L_OBJEK    LIKE MARA-MATNR ,
        L_ATNAM    TYPE CABN-ATNAM ,
        L_ATNAM1    TYPE CABN-ATNAM,
*        L_LEN TYPE I,
        L_DATE LIKE SY-DATUM.

*  CLEAR: IT_TEMP, IT_TEMP[].
  LOOP AT IT_OBJEK.

    CLEAR IT_TEMP.

    PERFORM READ_NORMAL_CLASS USING  IT_OBJEK-OBJEK
                                     'P_DESTINATION_CODE'
                              CHANGING L_OBJEK .
    IT_TEMP-NAT_CD = L_OBJEK.

    PERFORM READ_NORMAL_CLASS USING  IT_OBJEK-OBJEK
                                               'P_MI'
                                     CHANGING L_OBJEK .

    IT_TEMP-MC_CD = L_OBJEK.
    PERFORM READ_NORMAL_CLASS USING    IT_OBJEK-OBJEK
                                       'P_OCN'
                                     CHANGING L_OBJEK .
    IT_TEMP-OCN_CD = L_OBJEK.

    PERFORM READ_NORMAL_CLASS USING    IT_OBJEK-OBJEK
                                                'P_MODEL'
                                     CHANGING L_OBJEK .
    IT_TEMP-PRDN_VEHL_CD = L_OBJEK.


    PERFORM READ_NORMAL_CLASS USING    IT_OBJEK-OBJEK
                                                     'P_VERSION'
                                      CHANGING IT_TEMP-VER_CD.


    CONCATENATE 'P_RP' W_RP '_SHOP_DATE'  INTO L_ATNAM .
    PERFORM READ_SHOP_DATE USING    IT_OBJEK-OBJEK
                                   L_ATNAM  L_ATNAM1
                           CHANGING IT_TEMP-DATE.
    CASE W_RP.
      WHEN '07'.
        IT_TEMP-USF_SCN_CD = 'T'.
      WHEN '11'.
        IT_TEMP-USF_SCN_CD = 'C'.
      WHEN '18'.
        IT_TEMP-USF_SCN_CD = 'S'.
      WHEN '06'.
        IT_TEMP-USF_SCN_CD = 'P'.
    ENDCASE.

    IT_TEMP-MENGE = 1.
    COLLECT IT_TEMP.
  ENDLOOP.

ENDFORM.                    " create_data

*---------------------------------------------------------------------*
*       FORM modify_data                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM MODIFY_DATA.

  DATA: BEGIN OF LT_FSC OCCURS 0,
         PRDN_VEHL_CD LIKE ZTPP_PT_VRESULT-PRDN_VEHL_CD,
         NAT_CD LIKE ZTPP_PT_VRESULT-NAT_CD,
         MC_CD LIKE ZTPP_PT_VRESULT-MC_CD,
         OCN_CD LIKE ZTPP_PT_VRESULT-OCN_CD,
         VER_CD LIKE ZTPP_PT_VRESULT-VER_CD,
         USF_SCN_CD LIKE ZTPP_PT_VRESULT-USF_SCN_CD,
         END OF LT_FSC.
  DATA: WA_GR LIKE ZTPP_PT_VRESULT.
  DATA:  L_DATE_C(8),
  L_PDATE_C(8),
         L_TIME_C(6),
         L_DATE LIKE SY-DATUM,

        L_QTY LIKE ZTPP_PT_VRESULT-D01_PRDN_PRD_QTY,
          L_TOT LIKE ZTPP_PT_VRESULT-D01_PRDN_PRD_QTY,
         L_TEXT(40),
         L_CN(2) TYPE N.

  FIELD-SYMBOLS: <FS>.

  L_PDATE_C = P_DATE.

  L_DATE_C = SY-DATUM.
  L_TIME_C = SY-UZEIT.

  SORT IT_TEMP BY PRDN_VEHL_CD NAT_CD MC_CD OCN_CD VER_CD DATE.
  LOOP AT IT_TEMP.
    MOVE-CORRESPONDING IT_TEMP TO LT_FSC.
    COLLECT LT_FSC.
  ENDLOOP.

  LOOP AT LT_FSC.
    MOVE-CORRESPONDING LT_FSC TO WA_GR.
    L_DATE = W_FR_DATE.
    L_CN = '01'.
    WHILE L_DATE <= P_DATE.
      CLEAR: L_QTY.

      CONCATENATE 'WA_GR-D' L_CN '_PRDN_PRD_QTY' INTO L_TEXT.
      ASSIGN (L_TEXT) TO <FS>.

      LOOP AT IT_TEMP WHERE PRDN_VEHL_CD = LT_FSC-PRDN_VEHL_CD
                     AND NAT_CD = LT_FSC-NAT_CD
                     AND MC_CD = LT_FSC-MC_CD
                     AND OCN_CD = LT_FSC-OCN_CD
                     AND VER_CD = LT_FSC-VER_CD
                     AND USF_SCN_CD = LT_FSC-USF_SCN_CD
                     AND DATE = L_DATE.
        L_QTY = L_QTY + IT_TEMP-MENGE.
      ENDLOOP.
      IF L_QTY > 0.

*        WRITE L_QTY TO <FS> NO-ZERO DECIMALS 0.
        <FS> = L_QTY.
        L_TOT = L_TOT + L_QTY.
      ENDIF.
      L_CN = L_CN + 1.
      L_DATE = L_DATE + 1.
    ENDWHILE.

*    WRITE L_TOT TO WA_GR-M0_PRDN_PRD_QTY NO-ZERO DECIMALS 0.
    WA_GR-M0_PRDN_PRD_QTY = L_TOT.

    IF L_TOT <> 0.
      WA_GR-MKR_CD = 'H201'.
* UD1K949650 - Begine changed by Daniel Kim on 08/17/2010
*     WA_GR-PRDN_PLNT_CD = 'HEA1'.
      WA_GR-PRDN_PLNT_CD = 'HVA1'.
* UD1K949650 - End changed by Daniel Kim on 08/17/2010
      WA_GR-CRTN_YYMM =  L_PDATE_C+0(6).
      CONCATENATE L_DATE_C L_TIME_C INTO WA_GR-CREATEDATE.
      WA_GR-CHANGEDATE = WA_GR-CREATEDATE.
      APPEND WA_GR TO IT_PT_VRESULT.
    ENDIF.
    CLEAR: WA_GR, L_TOT, IT_PT_VRESULT.
  ENDLOOP.

ENDFORM.                    " modify_data
*---------------------------------------------------------------------*
*       FORM read_shop_date                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_OBJEK                                                       *
*  -->  P_ATNAM                                                       *
*  -->  P_ATNAM1                                                      *
*  -->  P_DATE                                                        *
*---------------------------------------------------------------------*
FORM READ_SHOP_DATE USING    P_OBJEK
                             P_ATNAM P_ATNAM1
                    CHANGING P_DATE. "p_atflv.
  DATA: L_ATFLV   TYPE AUSP-ATFLV,
        L_NUM(08) TYPE N         .

  SELECT SINGLE AU~ATFLV
    INTO L_ATFLV
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON CA~ATINN = AU~ATINN
    WHERE AU~OBJEK =  P_OBJEK     AND
          AU~KLART =  '002'       AND
          CA~ATNAM =  P_ATNAM       .
  IF SY-SUBRC NE 0.
    SELECT SINGLE AU~ATFLV
    INTO L_ATFLV
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON CA~ATINN = AU~ATINN
    WHERE AU~OBJEK =  P_OBJEK     AND
          AU~KLART =  '002'       AND
          CA~ATNAM =  P_ATNAM1       .
  ENDIF.
  P_DATE = L_NUM = L_ATFLV .

ENDFORM.                    " read_shop_date
*---------------------------------------------------------------------*
*       FORM check_scrap_car                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_VMNO                                                        *
*  -->  P_CHAR                                                        *
*  -->  P_SUBRC                                                       *
*---------------------------------------------------------------------*
FORM CHECK_SCRAP_CAR USING    P_VMNO
                              P_CHAR
                     CHANGING P_SUBRC.

  IF W_RP >= '01' AND W_RP <= '17'.
    SELECT SINGLE OBJEK
              INTO IT_OBJEK-OBJEK
              FROM AUSP AS AU
        INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
             WHERE OBJEK    = P_VMNO      AND
                   KLART    = '002'       AND
                   ATWRT = 'S'            AND
                   CA~ATNAM =  P_CHAR.
    P_SUBRC = SY-SUBRC.
  ELSE.
    SELECT SINGLE OBJEK
               INTO IT_OBJEK-OBJEK
               FROM AUSP AS AU
         INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
              WHERE OBJEK    = P_VMNO      AND
                    KLART    = '002'       AND
                    ATWRT = 'D'    AND
                    CA~ATNAM =  P_CHAR.
    P_SUBRC = SY-SUBRC.
  ENDIF.
ENDFORM.                    " check_scrap_car

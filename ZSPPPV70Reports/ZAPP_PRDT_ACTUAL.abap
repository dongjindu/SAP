************************************************************************
* Program Name      : ZAPP_PRDT_ACTUAL
* Creation Date     : 10/11/2007
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT ZAPP_PRDT_ACTUAL NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMPP.
TYPE-POOLS: SLIS, VRM.

CONSTANTS: C_CG_DATE TYPE D VALUE '20100731'.
DATA: W_ST_DATE LIKE SY-DATUM,
      W_RP(2).

DATA: W_UNAME LIKE SY-UNAME,
      W_RUNDATE LIKE SY-DATUM,
      W_RUNTIME LIKE SY-UZEIT.

DATA: BEGIN OF IT_DATE OCCURS 0,
        DATE TYPE SY-DATUM,
        NUM(02) TYPE N,
      END OF IT_DATE.
DATA: BEGIN OF IT_OBJEK OCCURS 0,
        OBJEK TYPE AUSP-OBJEK,
        ATWRT TYPE AUSP-ATWRT,
      END OF IT_OBJEK.
DATA: BEGIN OF IT_RP OCCURS 0,
        RP(2),
      END OF IT_RP.

*data: it_temp like table of ztpp_prod_actual with header line,
DATA:  IT_PROD_ACTUAL LIKE TABLE OF ZTPP_PROD_ACTUAL WITH HEADER LINE.

DATA:  BEGIN OF IT_TEMP OCCURS 0,
        OBJEK LIKE AUSP-OBJEK,
        FSC(18).
        INCLUDE STRUCTURE ZTPP_PROD_ACTUAL.
DATA:  END OF IT_TEMP.

DATA:  BEGIN OF IT_TEMP_DISPOSAL OCCURS 0,
        OBJEK LIKE AUSP-OBJEK,
        FSC(18),
        ATWRT LIKE AUSP-ATWRT.
        INCLUDE STRUCTURE ZTPP_PROD_ACTUAL.
DATA:  END OF IT_TEMP_DISPOSAL.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_DATE LIKE SY-DATUM.
PARAMETERS: P_DAYS(3) TYPE N.
SELECTION-SCREEN END OF BLOCK B1.

AT SELECTION-SCREEN OUTPUT.

START-OF-SELECTION.
  PERFORM SET_DATA TABLES IT_DATE.
  PERFORM GET_DATA.
  PERFORM WRITE_DATA.

END-OF-SELECTION.

*---------------------------------------------------------------------*
*       FORM set_data                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_IT_DATE                                                     *
*---------------------------------------------------------------------*
FORM SET_DATA TABLES  P_IT_DATE STRUCTURE  IT_DATE .
  DATA: L_DATE    TYPE SY-DATUM,
        L_NUM(02) TYPE N.

  W_ST_DATE = P_DATE - P_DAYS.
  MOVE W_ST_DATE TO L_DATE.
  L_NUM = '01'.
  REFRESH: P_IT_DATE.
  DO 31 TIMES.
    CLEAR P_IT_DATE.
    MOVE: L_DATE TO P_IT_DATE-DATE,
          L_NUM  TO P_IT_DATE-NUM .
    APPEND P_IT_DATE.
    L_DATE = L_DATE + 1.
    L_NUM  = L_NUM  + 1.
    IF L_DATE > P_DATE.
      EXIT .
    ENDIF.
  ENDDO.

  IT_RP-RP = '18'.
  APPEND IT_RP.
  IT_RP-RP = '01'.
  APPEND IT_RP.
  IT_RP-RP = '04'.
  APPEND IT_RP.
  IT_RP-RP = '07'.
  APPEND IT_RP.
  IT_RP-RP = '19'.
  APPEND IT_RP.
** Added by Furong on 05/20/09
  IT_RP-RP = '23'.
  APPEND IT_RP.
** End of addition on 05/20/09
** Added by Furong on 07/30/09
  IT_RP-RP = '24'.
  APPEND IT_RP.
** End of addition on 07/30/09
  IT_RP-RP = '25'.
  APPEND IT_RP.

ENDFORM.                    " SET_DATE

*  perform set_init_data.
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.
  DATA: L_ERROR ,
        L_TEXT(50) .

  W_UNAME = SY-UNAME.
  W_RUNDATE = SY-DATUM.
  W_RUNTIME = SY-UZEIT.

  LOOP AT IT_RP.
    W_RP = IT_RP-RP.
    CLEAR L_ERROR.
    CLEAR: IT_OBJEK, IT_OBJEK[].   ", it_act, it_atc[].
    PERFORM GET_VEHICLE_MASTER TABLES IT_OBJEK.
    PERFORM CREATE_DATA.
    PERFORM MODIFY_DATA.
  ENDLOOP.

** Changed by furong on 01/11/10
  CLEAR L_ERROR.
  CLEAR: IT_OBJEK, IT_OBJEK[].   ", it_act, it_atc[].
  PERFORM GET_VEHICLE_MASTER_DISPOSAL TABLES IT_OBJEK.
  PERFORM CREATE_DATA_DISPOSAL.
  PERFORM MODIFY_DATA_DISPOSAL.


** End of change

ENDFORM.                    " get_data
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
        L_DATE LIKE SY-DATUM.
  DATA: BEGIN OF LT_OBJEK OCCURS 0,
          OBJEK   LIKE AUSP-OBJEK,
        END OF LT_OBJEK.
  CLEAR L_SUBRC.

  CONCATENATE 'P_RP' W_RP '_SHOP_DATE'  INTO L_ATNAM.

  IF W_RP = '24'.
    CONCATENATE 'P_RP' '26' '_SHOP_DATE'  INTO L_ATNAM1 .
  ENDIF.

  IF W_RP = '25'.
    CONCATENATE 'P_RP' '27' '_SHOP_DATE'  INTO L_ATNAM1 .
  ENDIF.

  R_DATE-LOW = L_ATFLV_ST = L_NUM = W_ST_DATE.

  R_DATE-HIGH = L_ATFLV_EN = L_NUM = P_DATE.

  SELECT AU~OBJEK
    INTO TABLE LT_OBJEK
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE KLART = '002'          AND
          AU~ATFLV >= L_ATFLV_ST AND
          AU~ATFLV <= L_ATFLV_EN AND
          CA~ATNAM = L_ATNAM.

  IF W_RP = '24' OR W_RP = '25'.
    SELECT AU~OBJEK
      APPENDING TABLE LT_OBJEK
      FROM AUSP AS AU
        INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
      WHERE KLART = '002'          AND
            AU~ATFLV >= L_ATFLV_ST AND
            AU~ATFLV <= L_ATFLV_EN AND
            CA~ATNAM = L_ATNAM1.
  ENDIF.

  LOOP AT LT_OBJEK.
    IT_OBJEK-OBJEK = LT_OBJEK-OBJEK.

    R_DATE-SIGN = 'I'. R_DATE-OPTION = 'BT' .

    IF W_RP >= '01' AND W_RP <= '17'.
      PERFORM CHECK_SCRAP_CAR  USING IT_OBJEK-OBJEK   'P_USAGE_CAR'
                                  CHANGING L_SUBRC .

      IF L_SUBRC = 0.
        CONTINUE.
      ENDIF.
    ENDIF.
**    P_EXTC_APP245,       "P_EXT_COLOR
*    IF p_extc_app245 <> space.
*      CLEAR l_subrc .
*      MOVE p_extc_app245 TO l_atwrt .
*      PERFORM check_data_of_vm_app245 USING it_objek-objek
*                                     'P_EXT_COLOR'
*                                     l_atwrt
*                               CHANGING l_subrc .
*      IF l_subrc <> 0.  CONTINUE.  ENDIF.
*    ENDIF.
**    P_INTC_APP245.       "P_INT_COLOR
*    IF p_intc_app245 <> space.
*      CLEAR l_subrc .
*      MOVE p_intc_app245 TO l_atwrt .
*      PERFORM check_data_of_vm_app245 USING it_objek-objek
*                                     'P_INT_COLOR'
*                                     l_atwrt
*                               CHANGING l_subrc .
*      IF l_subrc <> 0.  CONTINUE.  ENDIF.
*    ENDIF.
**    P_COLUMN01_APP245 ~ 10  "P_219_xxx
    PERFORM CHECK_219_CODE        USING    IT_OBJEK-OBJEK
                           CHANGING L_SUBRC .
    IF L_SUBRC <> 0.  CONTINUE.  ENDIF.

** Changed by Furong on 05/20/09
** CHECK VPC OUT
    IF W_RP = '23'.
      PERFORM CHECK_VPC_OUT USING    IT_OBJEK-OBJEK
                                      'P_RP19_SHOP_DATE'
                              CHANGING L_DATE.
*** Chaged by Furong pn 07/29/09
*      IF L_DATE <= C_CG_DATE.
*        CONTINUE.
*      ENDIF.
*** End of change on 07/29/09

*      PERFORM CHECK_VPC_OUT USING    IT_TEMP-OBJEK
*                                      'P_RP23_SHOP_DATE'
*                              CHANGING L_DATE.
*      IF L_DATE <= C_CG_DATE.
*        CONTINUE.
*      ENDIF.
    ENDIF.
** End of change

    APPEND IT_OBJEK.

  ENDLOOP .
  SORT IT_OBJEK BY OBJEK .
ENDFORM.                    " get_vehicle_master


*---------------------------------------------------------------------*
*       FORM check_219_code                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_OBJEK                                                       *
*  -->  P_SUBRC                                                       *
*---------------------------------------------------------------------*
FORM CHECK_219_CODE USING    P_OBJEK
                    CHANGING P_SUBRC.
  DATA: LC_COLUMN(20) TYPE C,
        LC_VALUE(20)  TYPE C,
        LC_NUM(02)    TYPE N ,
        L_INT         TYPE I ,
        L_ATWRT       TYPE AUSP-ATWRT,
        L_ATNAM       TYPE CABN-ATNAM.
  FIELD-SYMBOLS: <FS_COLUMN> ,
                 <FS_VALUE>  .

  DO 10 TIMES.
    CLEAR P_SUBRC.
*   Defining Column
    LC_NUM = LC_NUM + 1 .
    CONCATENATE 'P_COLUMN' LC_NUM  INTO LC_COLUMN.
    ASSIGN (LC_COLUMN) TO <FS_COLUMN> .
*   Defining 219 Code Name         "ATNAM
    IF NOT <FS_COLUMN> IS INITIAL.
      L_ATNAM = L_INT =  <FS_COLUMN> .   CONDENSE L_ATNAM.
      CONCATENATE 'P_219_' L_ATNAM   INTO L_ATNAM .
    ENDIF.
*   Defining 219 Code's Value      "ATWRT
    CONCATENATE 'P_VALUE' LC_NUM  INTO LC_VALUE .
    ASSIGN (LC_VALUE) TO <FS_VALUE> .
    MOVE <FS_VALUE> TO L_ATWRT .
*
    IF <FS_COLUMN> IS INITIAL  .
      CONTINUE .
    ENDIF.
*
    PERFORM CHECK_DATA_OF_VM USING IT_OBJEK-OBJEK
                                   L_ATNAM
                                   L_ATWRT
                             CHANGING P_SUBRC.
    IF P_SUBRC <> 0.
      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.                    " check_219_code

*---------------------------------------------------------------------*
*       FORM check_data_of_vm                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_VMNO                                                        *
*  -->  P_CHAR                                                        *
*  -->  P_VALUE                                                       *
*  -->  P_SUBRC                                                       *
*---------------------------------------------------------------------*
FORM CHECK_DATA_OF_VM USING    P_VMNO
                               P_CHAR
                               P_VALUE
                      CHANGING P_SUBRC.
  SELECT SINGLE OBJEK
    INTO IT_OBJEK-OBJEK
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE OBJEK = P_VMNO         AND
          KLART = '002'          AND
          AU~ATWRT = P_VALUE     AND
          CA~ATNAM = P_CHAR      .
  P_SUBRC = SY-SUBRC.
ENDFORM.                    " check_data_of_vm

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



** Changed by Furong on 01/22/09
*  SELECT SINGLE OBJEK
*           INTO IT_OBJEK-OBJEK
*           FROM AUSP AS AU
*     INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
*          WHERE OBJEK    = P_VMNO      AND
*                KLART    = '002'       AND
*                ( ATWRT    = 'S' OR ATWRT = 'D' ) AND
*                CA~ATNAM =  P_CHAR.

** Changed by Furong on 07/31/09
** Changed by Furong on 06/04/09
*  IF W_RP >= '18' AND W_RP <= '27'.
*    SELECT SINGLE OBJEK
*              INTO IT_OBJEK-OBJEK
*              FROM AUSP AS AU
*        INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
*             WHERE OBJEK    = P_VMNO      AND
*                   KLART    = '002'       AND
*                   ATWRT = 'D'            AND
*                   CA~ATNAM =  P_CHAR.
*    P_SUBRC = SY-SUBRC.
*
*  ENDIF.
*  IF   P_SUBRC = 0.
*  ELSE.
*    SELECT SINGLE OBJEK
*                  INTO IT_OBJEK-OBJEK
*                  FROM AUSP AS AU
*            INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
*                 WHERE OBJEK    = P_VMNO      AND
*                       KLART    = '002'       AND
*                       ATWRT    = 'S'         AND
*                       CA~ATNAM =  P_CHAR.
*
*    P_SUBRC = SY-SUBRC.
*  ENDIF.

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
*** End of change on 07/31/09

*  IF W_RP = '19' OR W_RP = '25'.
*    SELECT SINGLE OBJEK
*              INTO IT_OBJEK-OBJEK
*              FROM AUSP AS AU
*        INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
*             WHERE OBJEK    = P_VMNO      AND
*                   KLART    = '002'       AND
*                   ATWRT = 'D'            AND
*                   CA~ATNAM =  P_CHAR.
*  ELSE.
*
*    SELECT SINGLE OBJEK
*               INTO IT_OBJEK-OBJEK
*               FROM AUSP AS AU
*         INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
*              WHERE OBJEK    = P_VMNO      AND
*                    KLART    = '002'       AND
*                    ATWRT    = 'S'         AND
*                    CA~ATNAM =  P_CHAR.
*  ENDIF.
*** End of change
*  P_SUBRC = SY-SUBRC.

** End of change on 06/04/09
ENDFORM.                    " check_scrap_car

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
        L_LEN TYPE I,
        L_DATE LIKE SY-DATUM.

  CLEAR: IT_TEMP, IT_TEMP[].
  LOOP AT IT_OBJEK.
    CLEAR IT_TEMP.
    IT_TEMP-OBJEK = IT_OBJEK-OBJEK.
    PERFORM READ_NORMAL_CLASS USING IT_TEMP-OBJEK
                                                'P_MODEL_YEAR'
                                     CHANGING L_OBJEK .
    IT_TEMP-FSC = L_OBJEK .

    PERFORM READ_NORMAL_CLASS USING  IT_TEMP-OBJEK
                                     'P_DESTINATION_CODE'
                              CHANGING L_OBJEK .
    CONCATENATE IT_TEMP-FSC L_OBJEK
           INTO IT_TEMP-FSC.

    IT_TEMP-DEST = L_OBJEK.
    IT_TEMP-NATN = L_OBJEK.
    PERFORM READ_NORMAL_CLASS USING  IT_TEMP-OBJEK
                                               'P_MI'
                                     CHANGING L_OBJEK .

    CONCATENATE IT_TEMP-FSC L_OBJEK
           INTO IT_TEMP-FSC.
    IT_TEMP-BMDL = L_OBJEK.
    PERFORM READ_NORMAL_CLASS USING    IT_TEMP-OBJEK
                                       'P_OCN'
                                     CHANGING L_OBJEK .
    L_LEN = STRLEN( L_OBJEK ).
    IF L_LEN < 8.
      CONCATENATE IT_TEMP-FSC L_OBJEK
           INTO IT_TEMP-FSC SEPARATED BY SPACE.
    ELSE.
      CONCATENATE IT_TEMP-FSC+0(5) IT_TEMP-FSC+6(9) L_OBJEK
             INTO IT_TEMP-FSC.
    ENDIF.

    IT_TEMP-OCN = L_OBJEK.

    PERFORM READ_NORMAL_CLASS USING    IT_TEMP-OBJEK
                                                'P_MODEL'
                                     CHANGING L_OBJEK .
    IT_TEMP-MODEL = L_OBJEK.


    PERFORM READ_NORMAL_CLASS USING    IT_TEMP-OBJEK
                                                     'P_EXT_COLOR'
                                      CHANGING IT_TEMP-EXTC.

    PERFORM READ_NORMAL_CLASS USING    IT_TEMP-OBJEK
                                                     'P_INT_COLOR'
                                      CHANGING IT_TEMP-INTC.

    CONCATENATE 'P_RP' W_RP '_SHOP_DATE'  INTO L_ATNAM .
    IF W_RP = '24'.
      CONCATENATE 'P_RP' '26' '_SHOP_DATE'  INTO L_ATNAM1 .
    ENDIF.

    IF W_RP = '25'.
      CONCATENATE 'P_RP' '27' '_SHOP_DATE'  INTO L_ATNAM1 .
    ENDIF.
    PERFORM READ_SHOP_DATE USING    IT_TEMP-OBJEK
                                   L_ATNAM  L_ATNAM1
                           CHANGING IT_TEMP-PRDT_DATE.

    APPEND IT_TEMP.
  ENDLOOP.
  SORT IT_TEMP BY PRDT_DATE FSC EXTC INTC.
ENDFORM.                    " create_data

*---------------------------------------------------------------------*
*       FORM read_normal_class                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_VMNO                                                        *
*  -->  P_CHAR                                                        *
*  -->  P_VALUE                                                       *
*---------------------------------------------------------------------*
FORM READ_NORMAL_CLASS USING    P_VMNO  P_CHAR
                              CHANGING P_VALUE.
  SELECT SINGLE AU~ATWRT
    INTO P_VALUE
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE OBJEK = P_VMNO      AND
          KLART = '002'       AND
          CA~ATNAM = P_CHAR  .
ENDFORM.                    " read_normal_classification

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
*       FORM modify_data                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM MODIFY_DATA.
  DATA: L_CODE LIKE ZTPP_PROD_ACTUAL-HKCODE,
        L_PLANT LIKE ZTPP_PROD_ACTUAL-PLANT,
               L_ATNAM    TYPE CABN-ATNAM ,
        L_ATNAM1    TYPE CABN-ATNAM,
        L_RP19_SHOPDATE LIKE IT_TEMP-PRDT_DATE.

*  CLEAR: it_date, it_date[].
*  PERFORM set_date TABLES it_date.
  L_PLANT = 'A1'.
  L_CODE = 'H'.
  LOOP AT IT_TEMP.
    CLEAR IT_PROD_ACTUAL.
    MOVE-CORRESPONDING IT_TEMP TO IT_PROD_ACTUAL.
    IT_PROD_ACTUAL-HKCODE = L_CODE.
    IT_PROD_ACTUAL-PLANT = L_PLANT.
    IT_PROD_ACTUAL-USERID = W_UNAME.
    IT_PROD_ACTUAL-CHDATE =  W_RUNDATE.
    IT_PROD_ACTUAL-CHTIME =  W_RUNTIME.

*    it_prod_Actual-RP = W_RP.
*    it_prod_Actual-total = 1 .
*    READ TABLE it_date WITH KEY date = it_temp-date.
*    CONCATENATE 'IT_PROD_ACTUAL-QTY_SIGNOFF' it_date-num 'QTY'
*      INTO l_field_name .
*    ASSIGN (l_field_name) TO <fs_dqty>.
*    <fs_dqty> = 1 .
    CASE W_RP.
      WHEN '18'.
        IT_PROD_ACTUAL-QTY_SIGNOFF = 1.
      WHEN '01'.
        IT_PROD_ACTUAL-QTY_BODY = 1.
      WHEN '04'.
        IT_PROD_ACTUAL-QTY_PAINT = 1.
      WHEN '07'.
        IT_PROD_ACTUAL-QTY_TRIM = 1.
      WHEN '19'.
        IT_PROD_ACTUAL-QTY_CGATE = 1.
** Changed on 08/03/10
*        IF IT_TEMP-DEST+0(3) = 'B28' AND
*           IT_TEMP-PRDT_DATE <= C_CG_DATE.
*          IT_PROD_ACTUAL-QTY_MGATE = 1.
*        ENDIF.
** end of change on 08/03/10
** Added by Furong on 05/20/09
      WHEN '23'.
        IT_PROD_ACTUAL-QTY_VPCOUT = 1.
** Changed on 08/03/10
        IF IT_TEMP-DEST+0(3) = 'B28'.

          CONCATENATE 'P_RP' '19' '_SHOP_DATE'  INTO L_ATNAM.
          CLEAR: L_RP19_SHOPDATE.
          PERFORM READ_SHOP_DATE USING    IT_TEMP-OBJEK
                                         L_ATNAM  L_ATNAM1
                                 CHANGING L_RP19_SHOPDATE.

          IF L_RP19_SHOPDATE > C_CG_DATE.
            IT_PROD_ACTUAL-QTY_MGATE = 1.
          ENDIF.
        ENDIF.
** end of change on 08/03/10

** end of addition
** Added by Furong on 07/30/09
      WHEN '24' OR '26'.
        IT_PROD_ACTUAL-QTY_SHIPIN = 1.
** end of addition
      WHEN '25' OR '27'.
        IT_PROD_ACTUAL-QTY_SHIPOUT = 1.
** Changed on 08/03/10
        IF IT_TEMP-PRDT_DATE > C_CG_DATE.
          IF IT_TEMP-DEST+0(3) <> 'B28'.
            IT_PROD_ACTUAL-QTY_MGATE = 1.
          ENDIF.
        ENDIF.
** end of change on 08/03/10
    ENDCASE.
    COLLECT IT_PROD_ACTUAL.
  ENDLOOP.

ENDFORM.                    " modify_data
*&---------------------------------------------------------------------*
*&      Form  write_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_DATA.
** Changed by Furong on 01/22/09
*  MODIFY ZTPP_PROD_ACTUAL FROM TABLE IT_PROD_ACTUAL.
  DELETE FROM ZTPP_PROD_ACTUAL
             WHERE PRDT_DATE >= W_ST_DATE
               AND PRDT_DATE <= P_DATE.
*  IF SY-SUBRC = 0.
  INSERT ZTPP_PROD_ACTUAL FROM TABLE IT_PROD_ACTUAL
         ACCEPTING DUPLICATE KEYS .
*  IF SY-SUBRC = 0.
  COMMIT WORK.
*  ELSE.
*    ROLLBACK WORK.
*  ENDIF.
*  ELSE.
*    ROLLBACK WORK.
*  ENDIF.
** End of change

ENDFORM.                    " write_data
*&---------------------------------------------------------------------*
*&      Form  CHECK_VPC_OUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_VPC_OUT  USING    P_OBJEK
                             P_ATNAM
                    CHANGING P_DATE. "p_atflv.
  DATA: L_ATFLV LIKE AUSP-ATFLV,
           L_NUM(08) TYPE N.

  SELECT SINGLE AU~ATFLV
  INTO L_ATFLV
  FROM AUSP AS AU
    INNER JOIN CABN AS CA ON CA~ATINN = AU~ATINN
  WHERE AU~OBJEK =  P_OBJEK     AND
        AU~KLART =  '002'       AND
        CA~ATNAM =  P_ATNAM       .

  P_DATE = L_NUM = L_ATFLV .

ENDFORM.                    " CHECK_VPC_OUT
*&---------------------------------------------------------------------*
*&      Form  GET_VEHICLE_MASTER_DISPOSAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OBJEK  text
*----------------------------------------------------------------------*
FORM GET_VEHICLE_MASTER_DISPOSAL TABLES  P_IT_OBJEK STRUCTURE IT_OBJEK.

* RANGES: R_DATE   FOR  AUSP-ATFLV .
  DATA: L_SUBRC    TYPE SY-SUBRC ,
*        L_ATNAM    TYPE CABN-ATNAM,
*        L_ATNAM1   TYPE CABN-ATNAM,
        L_ATWRT    TYPE AUSP-ATWRT,
        L_ATFLV_ST TYPE AUSP-ATFLV,
        L_TEMP(06),
        L_DATUM    TYPE SY-DATUM,
        L_ATFLV_EN TYPE AUSP-ATFLV,
        L_NUM(08) TYPE N.
*        L_DATE LIKE SY-DATUM.
  DATA: BEGIN OF LT_OBJEK OCCURS 0,
          OBJEK   LIKE AUSP-OBJEK,
        END OF LT_OBJEK.
  CLEAR L_SUBRC.


  L_ATFLV_EN = L_NUM = P_DATE.

* By Daniel on 11/03/10 {
  L_ATFLV_ST = L_NUM = W_ST_DATE.

*  SELECT AU~OBJEK
*    INTO TABLE LT_OBJEK
*    FROM AUSP AS AU
*      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
*    WHERE KLART = '002'     AND
*          AU~ATFLV = L_ATFLV_EN AND
*          CA~ATNAM = 'P_SCRAP_DATE'.

  SELECT AU~OBJEK
    INTO TABLE LT_OBJEK
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE KLART = '002'     AND
          AU~ATFLV <= L_ATFLV_EN AND
          AU~ATFLV >= L_ATFLV_ST AND
          CA~ATNAM = 'P_SCRAP_DATE'.
* }

  LOOP AT LT_OBJEK.
    CLEAR: IT_OBJEK.
    IT_OBJEK-OBJEK = LT_OBJEK-OBJEK.

    SELECT SINGLE OBJEK
              INTO IT_OBJEK-OBJEK
              FROM AUSP AS AU
        INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
             WHERE OBJEK = IT_OBJEK-OBJEK
               AND KLART = '002'
               AND ATWRT = 'D'
               AND CA~ATNAM =  'P_USAGE_CAR'.
    IF SY-SUBRC <> 0.
      CONTINUE.
    ENDIF.

    SELECT SINGLE ATWRT INTO IT_OBJEK-ATWRT
           FROM AUSP AS AU
     INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
          WHERE OBJEK = IT_OBJEK-OBJEK
            AND KLART = '002'
            AND CA~ATNAM =  'P_RP_STATUS'.
    IF IT_OBJEK-ATWRT < 18.
      CONTINUE.
    ENDIF.

    PERFORM CHECK_219_CODE USING    IT_OBJEK-OBJEK
                           CHANGING L_SUBRC .

    IF L_SUBRC = 0.
      APPEND IT_OBJEK.
    ENDIF.

  ENDLOOP .
  SORT IT_OBJEK BY OBJEK .

ENDFORM.                    " GET_VEHICLE_MASTER_DISPOSAL
*&---------------------------------------------------------------------*
*&      Form  CREATE_DATA_DISPOSAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_DATA_DISPOSAL.
* DATA: L_RPNO(02) TYPE N          ,
  DATA:       L_OBJEK    LIKE MARA-MATNR ,
*        L_ATNAM    TYPE CABN-ATNAM ,
*        L_ATNAM1    TYPE CABN-ATNAM,
          L_LEN TYPE I,
          L_DATE LIKE SY-DATUM.

  CLEAR: IT_TEMP_DISPOSAL, IT_TEMP_DISPOSAL[].
  LOOP AT IT_OBJEK.
    CLEAR IT_TEMP_DISPOSAL.
    IT_TEMP_DISPOSAL-OBJEK = IT_OBJEK-OBJEK.
    IT_TEMP_DISPOSAL-ATWRT = IT_OBJEK-ATWRT.

    PERFORM READ_NORMAL_CLASS USING IT_TEMP_DISPOSAL-OBJEK
                                                'P_MODEL_YEAR'
                                     CHANGING L_OBJEK .
    IT_TEMP_DISPOSAL-FSC = L_OBJEK .

    PERFORM READ_NORMAL_CLASS USING  IT_TEMP_DISPOSAL-OBJEK
                                     'P_DESTINATION_CODE'
                              CHANGING L_OBJEK .
    CONCATENATE IT_TEMP_DISPOSAL-FSC L_OBJEK
           INTO IT_TEMP_DISPOSAL-FSC.

    IT_TEMP_DISPOSAL-DEST = L_OBJEK.
    IT_TEMP_DISPOSAL-NATN = L_OBJEK.
    PERFORM READ_NORMAL_CLASS USING  IT_TEMP_DISPOSAL-OBJEK
                                               'P_MI'
                                     CHANGING L_OBJEK .

    CONCATENATE IT_TEMP_DISPOSAL-FSC L_OBJEK
           INTO IT_TEMP_DISPOSAL-FSC.
    IT_TEMP_DISPOSAL-BMDL = L_OBJEK.
    PERFORM READ_NORMAL_CLASS USING    IT_TEMP_DISPOSAL-OBJEK
                                       'P_OCN'
                                     CHANGING L_OBJEK .
    L_LEN = STRLEN( L_OBJEK ).
    IF L_LEN < 8.
      CONCATENATE IT_TEMP_DISPOSAL-FSC L_OBJEK
           INTO IT_TEMP_DISPOSAL-FSC SEPARATED BY SPACE.
    ELSE.
      CONCATENATE IT_TEMP_DISPOSAL-FSC+0(5)
             IT_TEMP_DISPOSAL-FSC+6(9) L_OBJEK
             INTO IT_TEMP_DISPOSAL-FSC.
    ENDIF.

    IT_TEMP_DISPOSAL-OCN = L_OBJEK.

    PERFORM READ_NORMAL_CLASS USING   IT_TEMP_DISPOSAL-OBJEK
                                                'P_MODEL'
                                     CHANGING L_OBJEK .
    IT_TEMP_DISPOSAL-MODEL = L_OBJEK.


    PERFORM READ_NORMAL_CLASS USING    IT_TEMP_DISPOSAL-OBJEK
                                                     'P_EXT_COLOR'
                                      CHANGING IT_TEMP_DISPOSAL-EXTC.

    PERFORM READ_NORMAL_CLASS USING    IT_TEMP_DISPOSAL-OBJEK
                                                     'P_INT_COLOR'
                                      CHANGING IT_TEMP_DISPOSAL-INTC.

* by Daniel on 11/03/10 {
     PERFORM READ_SHOP_DATE USING    IT_TEMP_DISPOSAL-OBJEK
                                     'P_SCRAP_DATE' ''
                            CHANGING IT_TEMP_DISPOSAL-PRDT_DATE.

*    IT_TEMP_DISPOSAL-PRDT_DATE = P_DATE.
* }

    APPEND IT_TEMP_DISPOSAL.
  ENDLOOP.
  SORT IT_TEMP_DISPOSAL BY PRDT_DATE FSC EXTC INTC.

ENDFORM.                    " CREATE_DATA_DISPOSAL
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA_DISPOSAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MODIFY_DATA_DISPOSAL.
  DATA: L_CODE LIKE ZTPP_PROD_ACTUAL-HKCODE,
         L_PLANT LIKE ZTPP_PROD_ACTUAL-PLANT.

*  CLEAR: it_date, it_date[].
*  PERFORM set_date TABLES it_date.
  L_PLANT = 'A1'.
  L_CODE = 'H'.
  LOOP AT IT_TEMP_DISPOSAL.
    CLEAR IT_PROD_ACTUAL.
    MOVE-CORRESPONDING IT_TEMP_DISPOSAL TO IT_PROD_ACTUAL.
    IT_PROD_ACTUAL-HKCODE = L_CODE.
    IT_PROD_ACTUAL-PLANT = L_PLANT.
    IT_PROD_ACTUAL-USERID = W_UNAME.
    IT_PROD_ACTUAL-CHDATE =  W_RUNDATE.
    IT_PROD_ACTUAL-CHTIME =  W_RUNTIME.

* by Daniel on 11/04/10 {
*    IF IT_TEMP_DISPOSAL-ATWRT = '18'.
*      IT_PROD_ACTUAL-S_DISPOSAL = 1.
*    ELSEIF IT_TEMP_DISPOSAL-ATWRT between '19' and '23'.
*      IT_PROD_ACTUAL-V_DISPOSAL = 1.
*    ENDIF.

    IF IT_TEMP_DISPOSAL-ATWRT = '18'.
      IT_PROD_ACTUAL-S_DISPOSAL = 1.
    ELSEIF IT_TEMP_DISPOSAL-ATWRT between '19' and '23'.
      IT_PROD_ACTUAL-V_DISPOSAL = 1.
    ELSEIF IT_TEMP_DISPOSAL-ATWRT = '24' OR
           IT_TEMP_DISPOSAL-ATWRT = '26'.
      IT_PROD_ACTUAL-H_DISPOSAL = 1.
    ENDIF.

* }

    COLLECT IT_PROD_ACTUAL.
  ENDLOOP.

ENDFORM.                    " MODIFY_DATA_DISPOSAL

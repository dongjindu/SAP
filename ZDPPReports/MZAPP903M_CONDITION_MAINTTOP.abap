*&---------------------------------------------------------------------*
*& Include MZ903M_CODITION_MAINTTOP                                    *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM  SAPMZ903M_CODITION_MAINT   message-id zmpp   .

TABLES: ZTPP_PLAN_KEY.              " CONDITION TABLE

DATA: BEGIN OF IT_PLAN_KEY         OCCURS 0.
        INCLUDE STRUCTURE          ZTPP_PLAN_KEY.
DATA:   MARK,
      END OF IT_PLAN_KEY.

DATA: WA_PLAN          LIKE ZTPP_PLAN_KEY,
      WA_9000          TYPE C       ,
      WA_INIT          TYPE C       ,
      WA_FLAG          TYPE C       ,
      WA_EDIT          TYPE C       ,
      WA_SAMPLE        LIKE ZTPP_PLAN_KEY-COND,
      WA_CODE          LIKE SY-UCOMM,
      SV_CODE          LIKE SY-UCOMM,
      OK_CODE          LIKE SY-UCOMM.

DATA: C_SAMPLE1(100)   VALUE
      '(P_219_1 = ''A'' & P_219_219 =''4'') @ (P_NATION = ''B28'' & ',
      C_SAMPLE2(100)   VALUE
      'P_RP_STATUS = ''03'') & (P_219_5 ! ''F'' & P_219_5 ! ''G'' )   '.

CONTROLS: TC_9000 TYPE TABLEVIEW USING SCREEN 9000.

*&---------------------------------------------------------------------*
*& report  ZRSYSID_UPDATE                                              *
*& writer : 강석봉                                                     *
*& date   : 2000/01/03                                                 *
*& title  : original system to be changed                              *
*& 소속   : infolink ltd.                                              *
*&---------------------------------------------------------------------*
REPORT  ZRSYSID_UPDATE MESSAGE-ID ZIM.



TABLES : TADIR,              " R/3 저장소오브젝트의 디렉토?
         T100,               " 메세?
         DDFTX,              " 스크린페인터 텍스트를 가진 오브젝?
         DD01T,              " R/3 DD: 도메인 텍스?
         DD02T,              " R/3-DD: SAP 테이블 텍스?
         DD02L,              " SAP tables
         DD03L,              " 테이블필?
         DD03T,              " DD: 필드에 대한 텍스트 (언어종속)
         DD04L,              " Data element
         DD04T,              " R/3 DD: Data element 텍스?
         DD06T,              " R/3 DD: SQL 테이블에 대한 텍스?
         DD07T,              " DD: 도메인 고정값에 대한 텍스?
         DD08T,              " 관계 정의에 대한 텍스?
         DD25T,              " 뷰 및 잠금오브젝트에 대한 단?
         DD20T,              " AS400-T_MCOBJECT: MC 오브젝트 텍스?
         DD23T,              " AS400_L-MCID: 매치코드 ID 텍스?
         D010SINF,           " ABAP: ABAP프로그램 소스코드 정?
         D010TINF,           " ABAP: ABAP 텍스트요소에 대한 정?
         D020S,              " 시스템 테이블 D020S (화면소스)
         D020T,              " 화면의 간단한 설?
         D021T,              " 화면 키워드 텍스?
*         D021L,              " 화면키 단어 (실행시간 포맷)
         EUDB,               " 개발환경 오브젝?
         RSMPTEXTS,          " 메뉴페인터: 텍스?
         D347T,              " GUI 제?
         TSTCT,              " 트랜잭션코드 텍스?
         TLIBT,              " Function 그룹 단?
         TFTIT,              " Function 모듈단?
         ENLFDIR,            " Function 모듈의 추가 속?
         FUNCT,              " Function 모듈 단?
         TOBCT,              " 권한오브젝트 클래스에 대한 텍스?
         AGR_PROF,           " 액티비티그룹에 대한 프로파일이?
         TOBJT,              " 테이블 TOBJ의 오브젝트 텍스?
         TPARAT,             " 메모리 ID 단문.
         TRDIR,              "
         TRDIRT,             " TRDIR의 프로그램에 대한 제목텍스?
         TCDOBT,             " 변경문서생성을 위한 오브젝트텍스?
         DD30T,              " 탐색도움말텍스?
         SMODILOG,           " Log of customer modifications to dev. env
         SMODISRC,           " Customer modification log: Object data
         TEXTPOOL,           " ABAP Text Pool Definition
         TTREE.              " 구조에 대한 정의테이?


DATA : IT_TADIR LIKE TADIR   OCCURS  0 WITH HEADER LINE.
DATA : IT_T100  LIKE T100    OCCURS  0 WITH HEADER LINE.
DATA : IT_DD01T LIKE DD01T   OCCURS  0 WITH HEADER LINE,
       IT_DD02T LIKE DD02T   OCCURS  0 WITH HEADER LINE,
       IT_DD02L LIKE DD02L   OCCURS  0 WITH HEADER LINE,
       IT_DD03L LIKE DD03L   OCCURS  0 WITH HEADER LINE,
       IT_DD03T LIKE DD03T   OCCURS  0 WITH HEADER LINE,
       IT_DD04L LIKE DD04L   OCCURS  0 WITH HEADER LINE,
       IT_DD04T LIKE DD04T   OCCURS  0 WITH HEADER LINE,
       IT_DD06T LIKE DD06T   OCCURS  0 WITH HEADER LINE,
       IT_DD25T LIKE DD25T   OCCURS  0 WITH HEADER LINE,
       IT_DD30T LIKE DD30T   OCCURS  0 WITH HEADER LINE,
       IT_DD07T LIKE DD07T   OCCURS  0 WITH HEADER LINE,
       IT_DD08T LIKE DD08T   OCCURS  0 WITH HEADER LINE,
       IT_D020T LIKE D020T   OCCURS  0 WITH HEADER LINE,
       IT_D020S LIKE D020S   OCCURS  0 WITH HEADER LINE,
       IT_D021T LIKE D021T   OCCURS  0 WITH HEADER LINE,
*       IT_D021L LIKE D021L   OCCURS  0 WITH HEADER LINE,
       IT_EUDB  LIKE EUDB    OCCURS  0 WITH HEADER LINE,
       IT_D347T LIKE D347T   OCCURS  0 WITH HEADER LINE,
       IT_DD20T LIKE DD20T   OCCURS  0 WITH HEADER LINE,
       IT_DD23T LIKE DD23T   OCCURS  0 WITH HEADER LINE,
       IT_TSTCT LIKE TSTCT   OCCURS  0 WITH HEADER LINE,
       IT_TPARAT LIKE TPARAT OCCURS  0 WITH HEADER LINE,
       IT_TRDIR  LIKE TRDIR  OCCURS  0 WITH HEADER LINE,
       IT_TRDIRT LIKE TRDIRT OCCURS  0 WITH HEADER LINE,
       IT_TLIBT  LIKE TLIBT  OCCURS  0 WITH HEADER LINE,
       IT_TFTIT  LIKE TFTIT  OCCURS  0 WITH HEADER LINE,
       IT_FUNCT  LIKE FUNCT  OCCURS  0 WITH HEADER LINE,
       IT_TOBCT  LIKE TOBCT  OCCURS  0 WITH HEADER LINE,
       IT_TOBJT  LIKE TOBJT  OCCURS  0 WITH HEADER LINE,
       IT_DDFTX  LIKE DDFTX  OCCURS  0 WITH HEADER LINE,
       IT_AGR_PROF LIKE AGR_PROF OCCURS 0 WITH HEADER LINE,
       IT_D010SINF LIKE D010SINF OCCURS 0 WITH HEADER LINE,
       IT_D010TINF LIKE D010TINF OCCURS 0 WITH HEADER LINE,
       IT_RSMPTEXTS LIKE RSMPTEXTS OCCURS 0 WITH HEADER LINE,
       IT_TCDOBT   LIKE TCDOBT   OCCURS 0 WITH HEADER LINE,
       IT_TTREE    LIKE TTREE    OCCURS 0 WITH HEADER LINE,
       IT_TEXTPOOL LIKE TEXTPOOL OCCURS 0 WITH HEADER LINE,
       IT_ENLFDIR  LIKE ENLFDIR  OCCURS 0 WITH HEADER LINE.


DATA : MAT_CNT TYPE P.


SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   PARAMETERS : FROM_SYS  LIKE TADIR-SRCSYSTEM  OBLIGATORY.
   PARAMETERS : FR_DEVCL  LIKE TADIR-DEVCLASS   OBLIGATORY.
   SELECT-OPTIONS : OBJ_NAME  FOR TADIR-OBJ_NAME.
   SELECT-OPTIONS : PGMID     FOR TADIR-PGMID.
   SELECT-OPTIONS : OBJECT    FOR TADIR-OBJECT.
   SELECT-OPTIONS : F_AUTHOR  FOR TADIR-AUTHOR.
   SELECT-OPTIONS : P_FLANG   FOR TADIR-MASTERLANG
                    NO-EXTENSION NO INTERVALS OBLIGATORY.

SELECTION-SCREEN END OF BLOCK B1.
SELECTION-SCREEN SKIP 2.                           " 2 LINE SKIP

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
   PARAMETERS : TO_SYS    LIKE TADIR-SRCSYSTEM  OBLIGATORY.
   PARAMETERS : DEVCLASS  LIKE TADIR-DEVCLASS   OBLIGATORY.
   PARAMETERS : AUTHOR    LIKE TADIR-AUTHOR     OBLIGATORY.
   PARAMETERS : P_LANG    LIKE TADIR-MASTERLANG OBLIGATORY.
   PARAMETERS : P_TRANS   AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B2.

INITIALIZATION.                          " 초기값 SETTING
   AUTHOR = SY-UNAME.

START-OF-SELECTION.
   PERFORM   SET_TADIR.

*&---------------------------------------------------------------------*
*&      Form  SET_TADIR
*&---------------------------------------------------------------------*
FORM SET_TADIR.
  CLEAR :  MAT_CNT.
  refresh : IT_TADIR.
  select * into table IT_TADIR from TADIR
           WHERE SRCSYSTEM  EQ FROM_SYS
           and   devclass   EQ FR_DEVCL
           AND   PGMID      IN PGMID
           AND   OBJECT     IN OBJECT
           AND   OBJ_NAME   IN OBJ_NAME
           AND   AUTHOR     IN F_AUTHOR.
*           AND   MASTERLANG IN P_FLANG.

  if sy-subrc ne 0.
     message s966.   exit.
  endif.

  MAT_CNT  =  0.

  SORT IT_TADIR BY PGMID OBJECT OBJ_NAME.

  loop at it_tadir.
     WRITE : /  IT_TADIR-PGMID,
                IT_TADIR-OBJECT,
                IT_TADIR-OBJ_NAME,
                IT_TADIR-SRCSYSTEM,
                IT_TADIR-AUTHOR,
                IT_TADIR-DEVCLASS,
                IT_TADIR-MASTERLANG.

     MAT_CNT =  MAT_CNT + 1.
  endloop.

  WRITE : / MAT_CNT, '건 조회 완료!!'.
  SET PF-STATUS 'ZIMST'.           " GUI STATUS SETTING

ENDFORM.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

   CASE SY-UCOMM.
      WHEN 'EXEC'.    " DB UPDATE
         PERFORM   P3000_DB_UPDATE.

   ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  P3000_DB_UPDATE
*&---------------------------------------------------------------------*
FORM P3000_DB_UPDATE.
  DATA : L_ARBGB    LIKE    T100-ARBGB.
  DATA : L_TABIX    LIKE    SY-TABIX.
  MAT_CNT  =  0.

  loop at it_tadir.
      L_TABIX = SY-TABIX.
      it_tadir-srcsystem = to_sys.
      it_tadir-author    = author.
      it_tadir-devclass  = devclass.
      IF P_TRANS EQ 'X'.
         IT_TADIR-MASTERLANG = P_LANG.
      ENDIF.
      MODIFY it_tadir index L_TABIX.

      CHECK P_TRANS EQ 'X'.

*>>> MESSAGE TEXT
      CASE IT_TADIR-OBJECT.
         WHEN 'MSAG'.        " MESSAGE CLASS
            REFRESH : IT_T100.
            L_ARBGB = IT_TADIR-OBJ_NAME.
            SELECT * FROM   T100 INTO TABLE IT_T100
                     WHERE  ARBGB  EQ  L_ARBGB.
            IF SY-SUBRC EQ 0.
               LOOP AT IT_T100.
                  IT_T100-SPRSL =  P_LANG.
                  MODIFY IT_T100 INDEX SY-TABIX.
               ENDLOOP.
               MODIFY T100 FROM TABLE IT_T100.
            ENDIF.
         WHEN 'TABL'.       " TABLE
            REFRESH : IT_DD02L.
            SELECT * FROM   DD02L INTO TABLE IT_DD02L
                     WHERE  TABNAME    EQ  IT_TADIR-OBJ_NAME.

            IF SY-SUBRC EQ 0.
               LOOP AT IT_DD02L.
                  IT_DD02L-MASTERLANG =  P_LANG.
                  MODIFY IT_DD02L INDEX SY-TABIX.
               ENDLOOP.
               MODIFY DD02L FROM TABLE IT_DD02L.
            ENDIF.

            REFRESH : IT_DD02T.
            SELECT * FROM   DD02T INTO TABLE IT_DD02T
                     WHERE  TABNAME    EQ  IT_TADIR-OBJ_NAME
                     AND    DDLANGUAGE IN  P_FLANG.

            IF SY-SUBRC EQ 0.
               LOOP AT IT_DD02T.
                  IT_DD02T-DDLANGUAGE =  P_LANG.
                  MODIFY IT_DD02T INDEX SY-TABIX.
               ENDLOOP.
               MODIFY DD02T FROM TABLE IT_DD02T.
            ENDIF.

            REFRESH : IT_DD03T.
            SELECT * FROM   DD03T INTO TABLE IT_DD03T
                     WHERE  TABNAME    EQ  IT_TADIR-OBJ_NAME
                     AND    DDLANGUAGE IN  P_FLANG.

            IF SY-SUBRC EQ 0.
               LOOP AT IT_DD03T.
                  IT_DD03T-DDLANGUAGE =  P_LANG.
                  MODIFY IT_DD03T INDEX SY-TABIX.
               ENDLOOP.
               MODIFY DD03T FROM TABLE IT_DD03T.
*           ELSE.
*              REFRESH : IT_DD03L.
*              SELECT * FROM   DD03L
*                       INTO TABLE IT_DD03L
*                       WHERE  TABNAME    EQ  IT_TADIR-OBJ_NAME.
*
*              IF SY-SUBRC EQ 0.
*                 REFRESH : IT_DD03T.
*                 LOOP AT IT_DD03L.
*                    MOVE-CORRESPONDING IT_DD03L TO IT_DD03T.
*
*                    SELECT SINGLE DDTEXT INTO IT_DD03T-DDTEXT
*                           FROM  DD04T
*                           WHERE ROLLNAME EQ IT_DD03L-ROLLNAME
*                           AND   DDLANGUAGE = P_LANG.
*
*                    IT_DD03T-DDLANGUAGE =  P_LANG.
*                    APPEND IT_DD03T.
*                 ENDLOOP.
*                 MODIFY DD03T FROM TABLE IT_DD03T.
*              ENDIF.
            ENDIF.

            REFRESH : IT_DD08T.
            SELECT * FROM   DD08T INTO TABLE IT_DD08T
                     WHERE  TABNAME    EQ  IT_TADIR-OBJ_NAME
                     AND    DDLANGUAGE IN  P_FLANG.

            IF SY-SUBRC EQ 0.
               LOOP AT IT_DD08T.
                  IT_DD08T-DDLANGUAGE =  P_LANG.
                  MODIFY IT_DD08T INDEX SY-TABIX.
               ENDLOOP.
               MODIFY DD08T FROM TABLE IT_DD08T.
            ENDIF.

            REFRESH : IT_DDFTX.
            SELECT * FROM   DDFTX INTO TABLE IT_DDFTX
                     WHERE  TABNAME    EQ  IT_TADIR-OBJ_NAME
                     AND    DDLANGUAGE IN  P_FLANG.

            IF SY-SUBRC EQ 0.
               LOOP AT IT_DDFTX.
                  IT_DDFTX-DDLANGUAGE =  P_LANG.
                  MODIFY IT_DDFTX INDEX SY-TABIX.
               ENDLOOP.
               MODIFY DDFTX FROM TABLE IT_DDFTX.
            ENDIF.


         WHEN 'VIEW' OR 'ENQU'.    " VIEW 및 잠금오브젝?
            REFRESH : IT_DD25T.
            SELECT * FROM   DD25T INTO TABLE IT_DD25T
                     WHERE  VIEWNAME   EQ  IT_TADIR-OBJ_NAME
                     AND    DDLANGUAGE IN  P_FLANG.

            IF SY-SUBRC EQ 0.
               LOOP AT IT_DD25T.
                  IT_DD25T-DDLANGUAGE =  P_LANG.
                  MODIFY IT_DD25T INDEX SY-TABIX.
               ENDLOOP.
               MODIFY DD25T FROM TABLE IT_DD25T.
            ENDIF.
         WHEN 'SHI3'.       " 영역메?
            REFRESH : IT_TTREE.
            SELECT  * FROM  TTREE INTO TABLE IT_TTREE
                      WHERE ID    EQ   IT_TADIR-OBJ_NAME.
            IF SY-SUBRC EQ 0.
               LOOP AT IT_TTREE.
                  IT_TTREE-MASTERLANG = P_LANG.
                  MODIFY IT_TTREE INDEX SY-TABIX.
               ENDLOOP.
               MODIFY TTREE  FROM TABLE IT_TTREE.
            ENDIF.
         WHEN 'SHLP'.       " SERACH HELP OBJECT
            REFRESH : IT_DD30T.
            SELECT * FROM   DD30T INTO TABLE IT_DD30T
                     WHERE  SHLPNAME   EQ  IT_TADIR-OBJ_NAME
                     AND    DDLANGUAGE IN  P_FLANG.

            IF SY-SUBRC EQ 0.
               LOOP AT IT_DD30T.
                  IT_DD30T-DDLANGUAGE =  P_LANG.
                  MODIFY IT_DD30T INDEX SY-TABIX.
               ENDLOOP.
               MODIFY DD30T FROM TABLE IT_DD30T.
            ENDIF.

         WHEN 'DOMA'.       " DATA DOMAIN
            REFRESH : IT_DD01T.
            SELECT * FROM   DD01T INTO TABLE IT_DD01T
                     WHERE  DOMNAME    EQ  IT_TADIR-OBJ_NAME
                     AND    DDLANGUAGE IN  P_FLANG.

            IF SY-SUBRC EQ 0.
               LOOP AT IT_DD01T.
                  IT_DD01T-DDLANGUAGE =  P_LANG.
                  MODIFY IT_DD01T INDEX SY-TABIX.
               ENDLOOP.
               MODIFY DD01T FROM TABLE IT_DD01T.
            ENDIF.

            REFRESH : IT_DD07T.
            SELECT * FROM   DD07T INTO TABLE IT_DD07T
                     WHERE  DOMNAME    EQ  IT_TADIR-OBJ_NAME
                     AND    DDLANGUAGE IN  P_FLANG.

            IF SY-SUBRC EQ 0.
               LOOP AT IT_DD07T.
                  IT_DD07T-DDLANGUAGE =  P_LANG.
                  MODIFY IT_DD07T INDEX SY-TABIX.
               ENDLOOP.
               MODIFY DD07T FROM TABLE IT_DD07T.
*              DELETE FROM DD07T
*                     WHERE  DOMNAME    EQ  IT_TADIR-OBJ_NAME
*                     AND    DDLANGUAGE EQ  ' '.
            ENDIF.

         WHEN 'DTEL'.         " DATA ELEMENT
            REFRESH : IT_DD04L.
            SELECT * FROM   DD04L INTO TABLE IT_DD04L
                     WHERE  ROLLNAME   EQ  IT_TADIR-OBJ_NAME.
*                    AND    DTELMASTER IN  P_FLANG.

            IF SY-SUBRC EQ 0.
               LOOP AT IT_DD04L.
                  IT_DD04L-DTELMASTER =  P_LANG.
                  MODIFY IT_DD04L INDEX SY-TABIX.
               ENDLOOP.
               MODIFY DD04L FROM TABLE IT_DD04L.
            ENDIF.

            REFRESH : IT_DD04T.
            SELECT * FROM   DD04T INTO TABLE IT_DD04T
                     WHERE  ROLLNAME   EQ  IT_TADIR-OBJ_NAME
                     AND    DDLANGUAGE IN  P_FLANG.

            IF SY-SUBRC EQ 0.
               LOOP AT IT_DD04T.
                  IT_DD04T-DDLANGUAGE =  P_LANG.
                  MODIFY IT_DD04T INDEX SY-TABIX.
               ENDLOOP.
               MODIFY DD04T FROM TABLE IT_DD04T.
            ENDIF.

         WHEN 'PROG'.         " PROGRAM
            DELETE FROM SMODISRC
                   WHERE  OBJ_NAME  EQ  IT_TADIR-OBJ_NAME.
            DELETE FROM SMODILOG
                   WHERE  OBJ_NAME  EQ  IT_TADIR-OBJ_NAME.

            REFRESH : IT_D010SINF.
            SELECT * FROM   D010SINF INTO TABLE IT_D010SINF
                     WHERE  PROG     EQ  IT_TADIR-OBJ_NAME.
*                    AND    RLOAD    IN  P_FLANG.

            IF SY-SUBRC EQ 0.
               LOOP AT IT_D010SINF.
                  IT_D010SINF-RLOAD      =  P_LANG.
                  MODIFY IT_D010SINF INDEX SY-TABIX.
               ENDLOOP.
*               MODIFY D010SINF FROM TABLE IT_D010SINF.
            ENDIF.

            REFRESH : IT_TEXTPOOL.
            READ TEXTPOOL IT_TADIR-OBJ_NAME INTO IT_TEXTPOOL
                                            LANGUAGE P_FLANG-LOW.

            DELETE TEXTPOOL IT_TADIR-OBJ_NAME LANGUAGE P_LANG.
            INSERT TEXTPOOL IT_TADIR-OBJ_NAME
                            FROM IT_TEXTPOOL  LANGUAGE P_LANG.

            REFRESH : IT_D010TINF.
            SELECT * FROM   D010TINF INTO TABLE IT_D010TINF
                     WHERE  PROG     EQ  IT_TADIR-OBJ_NAME
                     AND    LANGUAGE IN  P_FLANG.

            IF SY-SUBRC EQ 0.
               LOOP AT IT_D010TINF.
                  IT_D010TINF-LANGUAGE   =  P_LANG.
                  MODIFY IT_D010TINF INDEX SY-TABIX.
               ENDLOOP.
*               MODIFY D010TINF FROM TABLE IT_D010TINF.
            ENDIF.

            REFRESH : IT_D020S.
            SELECT * FROM   D020S    INTO TABLE IT_D020S
                     WHERE  PROG     EQ  IT_TADIR-OBJ_NAME.
*                    AND    LANG     IN  P_FLANG.

            IF SY-SUBRC EQ 0.
               LOOP AT IT_D020S.
                  IT_D020S-SPRA       =  P_LANG.
                  MODIFY IT_D020S INDEX SY-TABIX.
               ENDLOOP.
               MODIFY D020S    FROM TABLE IT_D020S.
            ENDIF.

*            REFRESH : IT_D021L.
*            SELECT * FROM   D021L    INTO TABLE IT_D021L
*                     WHERE  PROG     EQ  IT_TADIR-OBJ_NAME
*                     AND    LANGUAGE IN  P_FLANG.

            IF SY-SUBRC EQ 0.
*               LOOP AT IT_D021L.
*                  IT_D021L-LANGUAGE   =  P_LANG.
*                  MODIFY IT_D021L INDEX SY-TABIX.
*               ENDLOOP.
*               MODIFY D021L    FROM TABLE IT_D021L.
            ENDIF.

            REFRESH : IT_D020T.
            SELECT * FROM   D020T    INTO TABLE IT_D020T
                     WHERE  PROG     EQ  IT_TADIR-OBJ_NAME
                     AND    LANG     IN  P_FLANG.

            IF SY-SUBRC EQ 0.
               LOOP AT IT_D020T.
                  IT_D020T-LANG       =  P_LANG.
                  MODIFY IT_D020T INDEX SY-TABIX.
               ENDLOOP.
               MODIFY D020T    FROM TABLE IT_D020T.
            ENDIF.

            REFRESH : IT_D021T.
            SELECT * FROM   D021T    INTO TABLE IT_D021T
                     WHERE  PROG     EQ  IT_TADIR-OBJ_NAME
                     AND    LANG     IN  P_FLANG.

            IF SY-SUBRC EQ 0.
               LOOP AT IT_D021T.
                  IT_D021T-LANG       =  P_LANG.
                  MODIFY IT_D021T INDEX SY-TABIX.
               ENDLOOP.
               MODIFY D021T    FROM TABLE IT_D021T.
*              DELETE FROM D021T
*                    WHERE  PROG     EQ  IT_TADIR-OBJ_NAME
*                    AND    LANG     NE  P_LANG.
            ENDIF.
*>>> PROGRAM HEADER TEXT
            REFRESH : IT_TRDIR.
            SELECT * FROM   TRDIR    INTO TABLE IT_TRDIR
                     WHERE  NAME     EQ  IT_TADIR-OBJ_NAME.
*                    AND    SPRSL    IN  P_FLANG.

            IF SY-SUBRC EQ 0.
               LOOP AT IT_TRDIR.
                  IT_TRDIR-RLOAD       =  P_LANG.
                  MODIFY IT_TRDIR  INDEX SY-TABIX.
               ENDLOOP.
*               MODIFY TRDIR     FROM TABLE IT_TRDIR.
*              DELETE FROM TRDIRT
*                    WHERE  NAME     EQ  IT_TADIR-OBJ_NAME
*                    AND    SPRSL    NE  P_LANG.
            ENDIF.

            REFRESH : IT_TRDIRT.
            SELECT * FROM   TRDIRT   INTO TABLE IT_TRDIRT
                     WHERE  NAME     EQ  IT_TADIR-OBJ_NAME
                     AND    SPRSL    IN  P_FLANG.

            IF SY-SUBRC EQ 0.
               LOOP AT IT_TRDIRT.
                  IT_TRDIRT-SPRSL       =  P_LANG.
                  MODIFY IT_TRDIRT INDEX SY-TABIX.
               ENDLOOP.
               MODIFY TRDIRT    FROM TABLE IT_TRDIRT.
*              DELETE FROM TRDIRT
*                    WHERE  NAME     EQ  IT_TADIR-OBJ_NAME
*                    AND    SPRSL    NE  P_LANG.
            ENDIF.

            REFRESH : IT_EUDB.
            SELECT * FROM   EUDB     INTO TABLE IT_EUDB
                     WHERE  NAME     EQ  IT_TADIR-OBJ_NAME
                     AND    SPRSL    IN  P_FLANG.
            IF SY-SUBRC EQ 0.
               LOOP AT IT_EUDB.
                  IT_EUDB-SPRSL       =  P_LANG.
                  MODIFY IT_EUDB INDEX SY-TABIX.
               ENDLOOP.
               MODIFY EUDB    FROM TABLE IT_EUDB.
            ENDIF.

            REFRESH : IT_RSMPTEXTS.
            SELECT * FROM   RSMPTEXTS INTO TABLE IT_RSMPTEXTS
                     WHERE  PROGNAME EQ  IT_TADIR-OBJ_NAME
                     AND    SPRSL    IN  P_FLANG.
            IF SY-SUBRC EQ 0.
               LOOP AT IT_RSMPTEXTS.
                  IT_RSMPTEXTS-SPRSL       =  P_LANG.
                  MODIFY IT_RSMPTEXTS INDEX SY-TABIX.
               ENDLOOP.
               MODIFY RSMPTEXTS    FROM TABLE IT_RSMPTEXTS.
            ENDIF.

            REFRESH : IT_D347T.
            SELECT * FROM   D347T     INTO TABLE IT_D347T
                     WHERE  PROGNAME EQ  IT_TADIR-OBJ_NAME
                     AND    SPRSL    IN  P_FLANG.
            IF SY-SUBRC EQ 0.
               LOOP AT IT_D347T.
                  IT_D347T-SPRSL       =  P_LANG.
                  MODIFY IT_D347T INDEX SY-TABIX.
               ENDLOOP.
               MODIFY D347T        FROM TABLE IT_D347T.
            ENDIF.

         WHEN 'TRAN'.          ">> TRANSACTION
            REFRESH : IT_TSTCT.
            SELECT * FROM   TSTCT    INTO TABLE IT_TSTCT
                     WHERE  TCODE    EQ  IT_TADIR-OBJ_NAME
                     AND    SPRSL    IN  P_FLANG.

            IF SY-SUBRC EQ 0.
               LOOP AT IT_TSTCT.
                  IT_TSTCT-SPRSL      =  P_LANG.
                  MODIFY IT_TSTCT INDEX SY-TABIX.
               ENDLOOP.
               MODIFY TSTCT    FROM TABLE IT_TSTCT.
            ENDIF.
         WHEN 'MCOB'.
            REFRESH : IT_DD20T.
            SELECT * FROM   DD20T       INTO TABLE IT_DD20T
                     WHERE  MCONAME     EQ  IT_TADIR-OBJ_NAME
                     AND    DDLANGUAGE  IN  P_FLANG.

            IF SY-SUBRC EQ 0.
               LOOP AT IT_DD20T.
                  IT_DD20T-DDLANGUAGE =  P_LANG.
                  MODIFY IT_DD20T INDEX SY-TABIX.
               ENDLOOP.
               MODIFY DD20T    FROM TABLE IT_DD20T.
            ENDIF.
*>>> MATCHCODE ID
            REFRESH : IT_DD23T.
            SELECT * FROM   DD23T       INTO TABLE IT_DD23T
                     WHERE  MCONAME     EQ  IT_TADIR-OBJ_NAME
                     AND    DDLANGUAGE  IN  P_FLANG.

            IF SY-SUBRC EQ 0.
               LOOP AT IT_DD23T.
                  IT_DD23T-DDLANGUAGE =  P_LANG.
                  MODIFY IT_DD23T INDEX SY-TABIX.
               ENDLOOP.
               MODIFY DD23T    FROM TABLE IT_DD23T.
            ENDIF.

         WHEN 'FUGR'.          ">> FUNCTION GROUP
            REFRESH : IT_TLIBT.
            SELECT * FROM   TLIBT    INTO TABLE IT_TLIBT
                     WHERE  AREA     EQ  IT_TADIR-OBJ_NAME
                     AND    SPRAS    IN  P_FLANG.

            IF SY-SUBRC EQ 0.
               LOOP AT IT_TLIBT.
                  IT_TLIBT-SPRAS      =  P_LANG.
                  MODIFY IT_TLIBT INDEX SY-TABIX.
               ENDLOOP.
               MODIFY TLIBT    FROM TABLE IT_TLIBT.
            ENDIF.

            REFRESH : IT_ENLFDIR.
            SELECT * FROM   ENLFDIR  INTO TABLE IT_ENLFDIR
                     WHERE  AREA     EQ  IT_TADIR-OBJ_NAME
                     AND    EXTEN1   IN  P_FLANG.

            IF SY-SUBRC EQ 0.
               LOOP AT IT_ENLFDIR.
                  IT_ENLFDIR-EXTEN1     =  P_LANG.
                  MODIFY IT_ENLFDIR INDEX SY-TABIX.
               ENDLOOP.
               MODIFY ENLFDIR    FROM TABLE IT_ENLFDIR.

               REFRESH : IT_TFTIT.
               SELECT * FROM   TFTIT    INTO TABLE IT_TFTIT
                        FOR ALL ENTRIES IN  IT_ENLFDIR
                        WHERE  FUNCNAME EQ  IT_ENLFDIR-FUNCNAME
                        AND    SPRAS    IN  P_FLANG.

               IF SY-SUBRC EQ 0.
                  LOOP AT IT_TFTIT.
                     IT_TFTIT-SPRAS      =  P_LANG.
                     MODIFY IT_TFTIT INDEX SY-TABIX.
                  ENDLOOP.
                  MODIFY TFTIT    FROM TABLE IT_TFTIT.
               ENDIF.

               REFRESH : IT_FUNCT.
               SELECT * FROM   FUNCT    INTO TABLE IT_FUNCT
                        FOR ALL ENTRIES IN  IT_TFTIT
                        WHERE  FUNCNAME EQ  IT_TFTIT-FUNCNAME
                        AND    SPRAS    IN  P_FLANG.

               IF SY-SUBRC EQ 0.
                  LOOP AT IT_FUNCT.
                     IT_FUNCT-SPRAS      =  P_LANG.
                     MODIFY IT_FUNCT INDEX SY-TABIX.
                  ENDLOOP.
                  MODIFY FUNCT    FROM TABLE IT_FUNCT.
               ENDIF.

            ENDIF.

         WHEN 'PARA'.          ">> PARAMETERS
            REFRESH : IT_TPARAT.
            SELECT * FROM   TPARAT   INTO TABLE IT_TPARAT
                     WHERE  PARAMID  EQ  IT_TADIR-OBJ_NAME
                     AND    SPRACHE  IN  P_FLANG.

            IF SY-SUBRC EQ 0.
               LOOP AT IT_TPARAT.
                  IT_TPARAT-SPRACHE   =  P_LANG.
                  MODIFY IT_TPARAT INDEX SY-TABIX.
               ENDLOOP.
               MODIFY TPARAT   FROM TABLE IT_TPARAT.
            ENDIF.

         WHEN 'CHDO'.         ">> Change Document

            SELECT * FROM   TCDOBT   INTO TABLE IT_TCDOBT
                     WHERE  OBJECT   EQ  IT_TADIR-OBJ_NAME
                     AND    SPRAS    IN  P_FLANG.

            IF SY-SUBRC EQ 0.
               LOOP AT IT_TCDOBT.
                  IT_TCDOBT-SPRAS     =  P_LANG.
                  MODIFY IT_TCDOBT INDEX SY-TABIX.
               ENDLOOP.
               MODIFY TCDOBT   FROM TABLE IT_TCDOBT.
            ENDIF.

         WHEN 'SUSC'.         ">> 권한 오브젝?

            SELECT * FROM   TOBCT    INTO TABLE IT_TOBCT
                     WHERE  OCLSS    EQ  IT_TADIR-OBJ_NAME
                     AND    LANGU    IN  P_FLANG.

            IF SY-SUBRC EQ 0.
               LOOP AT IT_TOBCT.
                  IT_TOBCT-LANGU     =  P_LANG.
                  MODIFY IT_TOBCT  INDEX SY-TABIX.
               ENDLOOP.
               MODIFY TOBCT    FROM TABLE IT_TOBCT.
            ENDIF.

         WHEN 'SUSO'.         ">> 권한 오브젝?

            SELECT * FROM   TOBJT    INTO TABLE IT_TOBJT
                     WHERE  OBJECT   EQ  IT_TADIR-OBJ_NAME
                     AND    LANGU    IN  P_FLANG.

            IF SY-SUBRC EQ 0.
               LOOP AT IT_TOBJT.
                  IT_TOBJT-LANGU     =  P_LANG.
                  MODIFY IT_TOBJT  INDEX SY-TABIX.
               ENDLOOP.
               MODIFY TOBJT    FROM TABLE IT_TOBJT.
            ENDIF.

            SELECT * FROM   AGR_PROF INTO TABLE IT_AGR_PROF
                     WHERE  AGR_NAME EQ  IT_TADIR-OBJ_NAME
                     AND    LANGU    IN  P_FLANG.

            IF SY-SUBRC EQ 0.
               LOOP AT AGR_PROF.
                  IT_AGR_PROF-LANGU     =  P_LANG.
                  MODIFY IT_AGR_PROF  INDEX SY-TABIX.
               ENDLOOP.
               MODIFY AGR_PROF FROM TABLE IT_AGR_PROF.
            ENDIF.


      ENDCASE.

*      MODIFY it_tadir index L_TABIX.
*
*      if sy-subrc ne 0.
*        MESSAGE E296 WITH IT_TADIR-OBJ_NAME.
*      endif.
*
     MAT_CNT =  MAT_CNT + 1.

  endloop.

  MODIFY tadir FROM TABLE it_tadir.

  IF SY-SUBRC NE 0.
     MESSAGE S297.
  ENDIF.

  WRITE : / MAT_CNT, '건 처리 완료!!'.

  LEAVE TO SCREEN 0.

ENDFORM.                    " P3000_DB_UPDATE

*&---------------------------------------------------------------------*
*& Report  ZRIMIMGMAIN                                                 *
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입관리 System Configuration Tree Report             *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2000.01.31                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
report  zrimimgmain          message-id zim.

include zrimtreecom.      " 계층구조 구조용 공용 데이타 및 함?

*-----------------------------------------------------------------------
* SELECTION START
*-----------------------------------------------------------------------
start-of-selection.
   set titlebar 'ZIMG'.
   set pf-status 'ZIMG'.

   refresh : it_treelist.
* HEAD WRITE
   perform   p2000_node_write    using   1    'ROOT'    '0'   1
       'Implementation Guide for Import System Customizing (IMG)' ''.

** LEVEL 2
*   PERFORM   P2000_NODE_WRITE    USING   2    'LEV1'    '3'   0
*        'Global Settings' ''.

* LEVEL 3
   perform   p2000_node_write    using   2    'EXEC'    '0'   1
        '수입시스템 EDI Text 관리(회사코드별)' 'ZIMG02'.

* LEVEL 3
   perform   p2000_node_write    using   2    'EXEC'    '0'   1
        '수입시스템 Basic Config. 관리'  'ZIMG03'.

* LEVEL 3
*   perform   p2000_node_write    using   2    'EXEC'    '0'   1
*        '수입시스템 Number Ranges 관리' 'ZIMG04'.

* LEVEL 3
   perform   p2000_node_write    using   2    'EXEC'    '0'   1
        '수입시스템 비용처리 Config. 관리' 'ZIMG05'.

** LEVEL 2
*   PERFORM   P2000_NODE_WRITE    USING   2    'LEV1'    '3'   0
*        'Other Code Maintenence'  ''.
*
** LEVEL 3
*   PERFORM   P2000_NODE_WRITE    USING   3    'EXEC'    '0'   1
*       'Other Additional Conditions'   ''.

*-----------------------------------------------------------------------
* SELECTION END
*-----------------------------------------------------------------------
end-of-selection.
* Hierarchy output
   perform hierarchy.                   " construct & draw the tree

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
at user-command.
  case sy-ucomm.
    when 'ERLE'.
      perform    p2000_node_action.
    when others.
  endcase.

*&---------------------------------------------------------------------*
*&      Form  P2000_NODE_SELECT
*&---------------------------------------------------------------------*
FORM P2000_NODE_SELECT TABLES   KNOTEN   STRUCTURE     SEUCOMM
                       USING    EXIT.
   CASE SY-TCODE.
      WHEN 'ZIM93'.     " 구매오더별 Hierarchy
         IF KNOTEN-HIDE IS INITIAL.
            MESSAGE E962.
         ELSE.
            CASE KNOTEN-NAME.
               WHEN 'LEVEL1'.     " Vendor
                  SET PARAMETER ID 'KDY' FIELD '/110/120/130'.
                  SET PARAMETER ID 'LIF' FIELD KNOTEN-TEXT.
                  SET PARAMETER ID 'EKO' FIELD ''.
               WHEN 'LEVEL2'.     " P/O
                  SET PARAMETER ID 'BES' FIELD KNOTEN-TEXT.
               WHEN 'LEVEL3'.     " L/C
                  SET PARAMETER ID 'ZPREQNO' FIELD KNOTEN-TEXT.
                  SET PARAMETER ID 'ZPOPNNO' FIELD ''.
                  SET PARAMETER ID 'BES'     FIELD ''.
               WHEN 'LEVEL4'.     " Amend
                  SET PARAMETER ID 'ZPAMDNO' FIELD KNOTEN-TEXT.
                  SET PARAMETER ID 'ZPREQNO' FIELD KNOTEN-HIDE+15(10).
                  SET PARAMETER ID 'ZPOPNNO' FIELD ''.
                  SET PARAMETER ID 'BES'     FIELD KNOTEN-HIDE+5(10).
               WHEN 'LEVEL5'.     " B/L
                  SET PARAMETER ID 'ZPBLNO'  FIELD KNOTEN-TEXT.
                  SET PARAMETER ID 'ZPHBLNO' FIELD ''.
               WHEN 'LEVEL6'.     " Invoice, 반입신?
                  IF KNOTEN-HIDE(5) EQ 'ZIMI8'.     " 반입신?
                     SET PARAMETER ID 'ZPBLNO'  FIELD KNOTEN-HIDE+5(10).
                     SET PARAMETER ID 'ZPBTSEQ' FIELD KNOTEN-TEXT.
                     SET PARAMETER ID 'ZPHBLNO' FIELD ''.
                  ENDIF.
                  IF KNOTEN-HIDE(5) EQ 'ZIM76'.     " 수입면?
                     SET PARAMETER ID 'ZPHBLNO' FIELD ''.
                     SET PARAMETER ID 'ZPBLNO'  FIELD KNOTEN-HIDE+5(10).
                     SET PARAMETER ID 'ZPCLSEQ' FIELD KNOTEN-TEXT.
                     SET PARAMETER ID 'ZPIDRNO' FIELD KNOTEN-TEXT1.
                  ENDIF.
                  IF KNOTEN-HIDE(5) EQ 'ZIM33'.     " Invoice
                     SET PARAMETER ID 'ZPIVNO'  FIELD KNOTEN-TEXT.
                     SET PARAMETER ID 'ZPCIVNO' FIELD ''.
                  ENDIF.
               WHEN 'LEVEL7'.     " 자재문서 및 회계문서.
                  IF KNOTEN-HIDE(4) EQ 'MB03'.     " 자재문서.
                     SET PARAMETER ID 'MJA'     FIELD KNOTEN-TEXT.
                     SET PARAMETER ID 'MBN'     FIELD KNOTEN-TEXT1.
                     SET PARAMETER ID 'POS'     FIELD ''.
                  ELSEIF KNOTEN-HIDE(4) EQ 'FB03'. " 회계문서.

                     SELECT MAX( BEWTP ) INTO W_BEWTP FROM EKBE
                            WHERE GJAHR EQ  KNOTEN-TEXT
                            AND   BELNR EQ  KNOTEN-TEXT1.

                     IF W_BEWTP EQ 'Q' OR W_BEWTP EQ 'N'.   " LIV
                        SET PARAMETER ID 'BUK'     FIELD '1000'.
                        SET PARAMETER ID 'GJR'     FIELD KNOTEN-TEXT.
                        SET PARAMETER ID 'RBN'     FIELD KNOTEN-TEXT1.
                        KNOTEN-HIDE(4) = 'MR3M'.
                     ELSE.
                        SET PARAMETER ID 'BUK'     FIELD '1000'.
                        SET PARAMETER ID 'GJR'     FIELD KNOTEN-TEXT.
                        SET PARAMETER ID 'BLN'     FIELD KNOTEN-TEXT1.
                     ENDIF.
                  ENDIF.
               WHEN OTHERS.
                  MESSAGE E962.
            ENDCASE.
            CALL TRANSACTION KNOTEN-HIDE(5) AND SKIP  FIRST SCREEN.
         ENDIF.
      WHEN OTHERS.      " 기타....
         CASE KNOTEN-NAME.
            WHEN 'EXEC'.
               IF KNOTEN-HIDE IS INITIAL.
                  MESSAGE E962.
               ELSE.
                  CALL TRANSACTION KNOTEN-HIDE.
               ENDIF.
            WHEN OTHERS.
               MESSAGE E962.
         ENDCASE.
   ENDCASE.
   EXIT = ' '.
ENDFORM.                    "

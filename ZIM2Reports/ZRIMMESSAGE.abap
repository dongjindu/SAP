*&---------------------------------------------------------------------*
*& INCLUDE ZRIMMESSAGE .
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입시스템 Message function Include                   *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2000.06.25                                            *
*&  적용회사PJT: 현대전자산업 주식회사                                 *
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  P2000_POPUP_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_POPUP_MESSAGE  USING VALUE(P_TITLE)
                                VALUE(P_QUESTION)
                                VALUE(P_BUTTON1)
                                VALUE(P_BUTTON2)
                                VALUE(P_DEFAULT)
                          CHANGING    P_ANSWER.

   CLEAR : P_ANSWER.

   CALL  FUNCTION  'POPUP_TO_CONFIRM'
         EXPORTING
             TITLEBAR        = P_TITLE
             DIAGNOSE_OBJECT = ''
             TEXT_QUESTION   = P_QUESTION
             TEXT_BUTTON_1   = P_BUTTON1
             TEXT_BUTTON_2   = P_BUTTON2
             DEFAULT_BUTTON  = P_DEFAULT
             DISPLAY_CANCEL_BUTTON = 'X'
             START_COLUMN    = 30
             START_ROW       = 8
         IMPORTING
             ANSWER          =  P_ANSWER.

ENDFORM.                    " P2000_POPUP_MESSAGE

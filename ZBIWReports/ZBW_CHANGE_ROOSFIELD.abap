*&---------------------------------------------------------------------*
*& Report  ZBW_CHANGE_ROOSFIELD
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZBW_CHANGE_ROOSFIELD.

UPDATE ROOSFIELD SET SELECTION = 'X'
WHERE OLTPSOURCE = '0FI_GL_4' and OBJVERS = 'A'
AND FIELD IN ('CPUDT', 'BUDAT').

WRITE : / SY-SUBRC.

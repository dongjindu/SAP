*&---------------------------------------------------------------------*
*& Report  Z_CORRECT_T529F
*&  for note: 691631
*&---------------------------------------------------------------------*
*& 2011.09.12  by yn.kim
*&
*&---------------------------------------------------------------------*

REPORT  Z_CORRECT_T529F.

TABLES: t529f, t529a, t588ys.
DATA wa_t529f  LIKE t529f.

* Begin of Block--------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK block WITH FRAME .

* Header
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(5)  c_tclas.
SELECTION-SCREEN COMMENT 8(6)  c_massn.
SELECTION-SCREEN COMMENT 15(6) c_userg.
SELECTION-SCREEN COMMENT 23(5) c_igmod.
SELECTION-SCREEN COMMENT 34(6) c_kennz.
SELECTION-SCREEN COMMENT 45(5) c_msdna.
SELECTION-SCREEN END   OF LINE.

* Parameter
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 2.
PARAMETERS: tclas  TYPE tclas OBLIGATORY DEFAULT 'A'.
SELECTION-SCREEN POSITION 9.
PARAMETERS: massn  TYPE massn OBLIGATORY.
SELECTION-SCREEN POSITION 16.
PARAMETERS: userg  TYPE userg.
SELECTION-SCREEN POSITION 24.
PARAMETERS: igmod  TYPE igmod.
SELECTION-SCREEN POSITION 35.
PARAMETERS: kennz TYPE kennz OBLIGATORY  DEFAULT 'D'.
SELECTION-SCREEN POSITION 46.
PARAMETERS: text  TYPE msdna OBLIGATORY.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN END OF BLOCK block.
* End of Block----------------------------------------

AT SELECTION-SCREEN OUTPUT.
* Header
  c_tclas = 'TCLAS'.
  c_massn = 'MASSN'.
  c_userg = 'USERG'.
  c_igmod = 'IGMOD'.
  c_kennz = 'KENNZ'.
  c_msdna = 'NAME'.

START-OF-SELECTION.
* Change T529F
  CLEAR: wa_t529f, t529f.

  SELECT SINGLE * FROM t529f WHERE tclas = tclas AND
                                   massn = massn AND
                                   userg = userg AND
                                   igmod = igmod.
  wa_t529f = t529f.

  IF sy-subrc IS INITIAL.
    t529f-kennz = kennz.
    t529f-dname = text.
    UPDATE t529f.
    WRITE:/'Old Record:', wa_t529f,
          /'New Record:', t529f.
  ELSE.

    t529f-tclas = tclas.
    t529f-massn = massn.
    t529f-userg = userg.
    t529f-igmod = igmod.
    t529f-kennz = kennz.
    t529f-dname = text.
    INSERT t529f.
    WRITE:/'No Old Record found.',
          /'New Record:', t529f.
  ENDIF.

* Change T588YS
  IF kennz = 'D'.
    CLEAR t529a.

    SELECT SINGLE * FROM t529a WHERE massn = massn.

    t588ys-prog  = text.
    t588ys-dnum  = '2000'.
    t588ys-massn = massn.
    t588ys-itygr = t529a-itygr.
    t588ys-userg = userg.
    t588ys-igmod = igmod.
    INSERT t588ys.
  ENDIF.

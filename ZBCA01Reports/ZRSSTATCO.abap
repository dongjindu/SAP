
************************************************************************
*
* RSSTATCO
*
* Enthält Farbsetzroutinen für alle RSSTAT..-Reports.
*
* Autor: F.Klausner
* Letzte Änderung: 28.02.1994
************************************************************************

*---------------------------------------------------------------------*
*       FORM FLIP_FLOP                                                *
*---------------------------------------------------------------------*
*       Schaltet zwischen zwei Farben hin und her.                    *
*---------------------------------------------------------------------*
*  -->  COLFLAG  Flag für aktuellen Zustand.                          *
*---------------------------------------------------------------------*
form flip_flop changing colflag.
  if colflag = 'X'.
    colflag = ' '. format intensified color 2 inverse off.
  else.
    colflag = 'X'. format intensified off color 2 inverse off.
  endif.
endform.

*---------------------------------------------------------------------*
*       FORM SET_INT_HEADER                                           *
*---------------------------------------------------------------------*
*       Setzt INTENSIFIED ON                                          *
*---------------------------------------------------------------------*
form set_int_header.
  format intensified on color 2.
endform.

*---------------------------------------------------------------------*
*       FORM SET_NONINT_HEADER                                        *
*---------------------------------------------------------------------*
*       Setzt INTENSIFIED OFF                                         *
*---------------------------------------------------------------------*
form set_nonint_header.
  format intensified off color 2.
endform.

*---------------------------------------------------------------------*
*       FORM CHANGE_COL                                               *
*---------------------------------------------------------------------*
*       Schaltet zwischen zwei Farben hin und her.                    *
*---------------------------------------------------------------------*
*  -->  COLFLAG  Flag für aktuellen Zustand.                          *
*---------------------------------------------------------------------*
form change_col using colflag.
  if colflag = 'X'.
    format intensified off color 7 inverse off.
  else.
    format intensified color 7 inverse off.
  endif.
endform.

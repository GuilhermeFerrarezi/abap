*&---------------------------------------------------------------------*
*&  Include           ZTSGF_ALV_SPLIT_PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9010  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_9010 input.

  case sy-ucomm.
    when 'BACK' or 'CANC' or 'EXIT'.
      set screen 0.
      leave screen.
  endcase.

endmodule.                 " USER_COMMAND_9010  INPUT
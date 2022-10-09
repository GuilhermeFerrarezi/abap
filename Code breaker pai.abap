*&---------------------------------------------------------------------*
*&  Include           ZTSGF_CODE_BREAKER_PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module zm_user_command_9000 input.

  case sy-ucomm.
    when c_back_button or c_canc_button or c_exit_button.
      set screen 0.
      leave screen.

    when c_begin_button.
      clear v_tries.
      perform zf_randomize_game.
      case abap_true.
        when rb_easy.
          v_dif = c_easy_dificulty.
          perform zf_prepare_game_easy.
          perform zf_check_if_is_new.
          perform zf_garantee_creation.
          perform zf_change_screen_easy.
        when rb_medium.
          v_dif = c_medium_dificulty.
          perform zf_prepare_game_medium.
          perform zf_check_if_is_new.
          perform zf_garantee_creation.
          perform zf_change_screen_medium.
        when rb_hard.
          v_dif = c_hard_dificulty.
          perform zf_prepare_game_hard.
          perform zf_check_if_is_new.
          perform zf_garantee_creation.
          perform zf_change_screen_hard.
      endcase.
*      IF rb_easy = abap_true.
*        PERFORM zf_prepare_game_easy.
*        v_dif = c_easy_dificulty.
*        PERFORM zf_change_screen_easy.
*      ELSEIF rb_medium = abap_true.
*        PERFORM zf_prepare_game_medium.
*        v_dif = c_medium_dificulty.
*        PERFORM zf_change_screen_medium.
*      ELSEIF rb_hard = abap_true.
*        PERFORM zf_prepare_game_hard.
*        v_dif = c_hard_dificulty.
*        PERFORM zf_change_screen_hard.
*      ENDIF.
    when c_help_button.
      call screen 9010 starting at 5 5.
    when c_rank_button.
      call screen 9040 starting at 5 5.
  endcase.

endmodule.                 " USER_COMMAND_9000  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9010  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module zm_user_command_9010 input.

  case sy-ucomm.
    when c_back_button or c_canc_button or c_exit_button.
      set screen 0.
      leave screen.
  endcase.

endmodule.                 " USER_COMMAND_9010  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9020  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module zm_user_command_9020 input.

  case sy-ucomm.
    when c_back_button or c_canc_button or c_exit_button.
      set screen 0.
      leave screen.
  endcase.

  v_tries = v_tries + 1.

  perform zf_check_password_easy.

endmodule.                 " USER_COMMAND_9020  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9030  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module zm_user_command_9030 input.

  case sy-ucomm.
    when c_back_button or c_canc_button or c_exit_button.
      set screen 0.
      leave screen.
  endcase.

endmodule.                 " USER_COMMAND_9030  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9040  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module zm_user_command_9040 input.

  call method cl_gui_cfw=>dispatch.

  case sy-ucomm.
    when c_back_button or c_canc_button or c_exit_button.
      set screen 0.
      leave screen.
  endcase.

endmodule.                 " USER_COMMAND_9040  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9050  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_9050 input.

  case sy-ucomm.
    when c_back_button or c_canc_button or c_exit_button.
      set screen 0.
      leave screen.
  endcase.

  perform zf_check_password_medium.

  v_tries = v_tries + 1.

endmodule.                 " USER_COMMAND_9050  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9060  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module zm_user_command_9060 input.

  case sy-ucomm.
    when c_back_button or c_canc_button or c_exit_button.
      set screen 0.
      leave screen.
  endcase.

  perform zf_check_password_hard.

  v_tries = v_tries + 1.

endmodule.                 " USER_COMMAND_9060  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9070  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module zm_user_command_9070 input.

  case sy-ucomm.
    when c_back_button or c_canc_button or c_exit_button.
      set screen 0.
      leave screen.
    when c_create_button.
      ztbgf_code_break-dificuldade = v_dif.
      insert ztbgf_code_break from ztbgf_code_break.
      commit work and wait.
      case abap_true.
        when rb_easy.
          set screen 0.
          leave screen.
          perform zf_change_screen_easy.
        when rb_medium.
          set screen 0.
          leave screen.
          perform zf_change_screen_medium.
        when rb_hard.
          set screen 0.
          leave screen.
          perform zf_change_screen_hard.
      endcase.

  endcase.

endmodule.                 " USER_COMMAND_9070  INPUT

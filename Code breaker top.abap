*&---------------------------------------------------------------------*
*&  Include           ZTSGF_CODE_BREAKER_TOP
*&---------------------------------------------------------------------*

program  ztsgf_code_breaker.

type-pools: abap.

types:
begin of y_password,
  password type n length 1,
end of y_password,

y_t_code_break type standard table of ztbgf_code_break.

tables: ztbgf_code_break.

data: rb_easy type flag,
      rb_medium type flag,
      rb_hard type flag,
      v_password1 type y_password,
      v_password2 type y_password,
      v_password3 type y_password,
      v_password4 type y_password,
      v_password5 type y_password,
      v_password6 type y_password,
      v_password7 type y_password,
      v_password8 type y_password,
      v_show_password1 type y_password,
      v_show_password2 type y_password,
      v_show_password3 type y_password,
      v_show_password4 type y_password,
      v_show_password5 type y_password,
      v_show_password6 type y_password,
      v_show_password7 type y_password,
      v_show_password8 type y_password,
      v_num1 type y_password,
      v_num2 type y_password,
      v_num3 type y_password,
      v_num4 type y_password,
      v_num5 type y_password,
      v_num6 type y_password,
      v_num7 type y_password,
      v_num8 type y_password,
      v_random type ref to cl_abap_random_int,
      v_seed      type i,
      icon1     type icons-text,
      icon2     type icons-text,
      icon3     type icons-text,
      icon4     type icons-text,
      icon5     type icons-text,
      icon6     type icons-text,
      icon7     type icons-text,
      icon8     type icons-text,
*      v_light     TYPE icon-name,
      v_custom_container type ref to cl_gui_custom_container,
      v_grid type ref to cl_gui_alv_grid,
      t_code_break type y_t_code_break,
      v_dif type char6,
      v_tries type n length 5.

constants: c_green_icon type c length 14 value 'ICON_LED_GREEN',
           c_red_icon type c length 15 value 'ICON_INCOMPLETE',
*           c_red_icon TYPE c LENGTH 12 VALUE 'ICON_LED_RED',
           c_yellow_icon type c length 15 value 'ICON_LED_YELLOW',
           c_inactive_icon type c length 17 value 'ICON_LED_INACTIVE',
           c_easy_dificulty type c length 5 value 'FÁCIL',
           c_medium_dificulty type c length 5 value 'MÉDIO',
           c_hard_dificulty type c length 7 value 'DIFÍCIL',
           c_canc_button type c length 4 value 'CANC',
           c_back_button type c length 4 value 'BACK',
           c_exit_button type c length 4 value 'EXIT',
           c_begin_button type c length 4 value 'INIC',
           c_create_button type c length 6 value 'CREATE',
           c_help_button type c length 4 value 'HELP',
           c_rank_button type c length 4 value 'RANK'.

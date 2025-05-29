CREATE TABLE error_log (
    error_id          NUMBER PRIMARY KEY,
    error_code        NUMBER NOT NULL,
    error_message     VARCHAR2(4000) NOT NULL,
    package_owner     VARCHAR2(128),
    package_name      VARCHAR2(128),
    line_number       NUMBER NOT NULL,
    username          VARCHAR2(128) NOT NULL,
    error_time        DATE NOT NULL,
    sql_string        VARCHAR2(4000)
);
/

CREATE SEQUENCE error_log_seq INCREMENT BY 1;
/
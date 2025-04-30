----------------CREATE ERROR TABLE----------------------

CREATE TABLE error_log (
    error_id          NUMBER GENERATED ALWAYS AS IDENTITY
                            CONSTRAINT pk_error_log PRIMARY KEY,
    error_code        NUMBER NOT NULL,
    error_message     VARCHAR2(4000) NOT NULL,
    package_owner     VARCHAR2(128),
    package_name      VARCHAR2(128),
    line_number       NUMBER NOT NULL,
    username          VARCHAR2(128) NOT NULL,
    error_time        DATE NOT NULL
);
/
----------------CREATE ERROR TABLE----------------------

CREATE TABLE error_log (
    error_id          NUMBER GENERATED ALWAYS AS IDENTITY
                            CONSTRAINT pk_error_log PRIMARY KEY,
    error_code        NUMBER NOT NULL,
    error_message     VARCHAR2(4000) NOT NULL,
    backtrace         VARCHAR2(4000) NOT NULL,
    username          VARCHAR2(128) NOT NULL,
    error_time        TIMESTAMP NOT NULL
);
/
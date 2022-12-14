create table if not exists users
(
 id bigserial,
 name text,
 login text unique,
 password text not null,
 passsault text not null,
 created_at timestamp(0),
 updated_at timestamp(0),
 token text unique,
 token_expiration timestamp(0)
);
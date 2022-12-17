create table if not exists users
(
 id bigserial,
 name text,
 login varchar(100) unique,
 password bytea not null,
 passsalt bytea not null,
 created_at integer,
 updated_at integer,
 token text unique,
 token_expiration integer
);
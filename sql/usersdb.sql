create table if not exists users
(
 id bigserial,
 name text,
 login varchar(100) unique,
 password bytea not null,
 passsalt bytea not null,
 created_at bigint,
 updated_at bigint,
 token text,
 token_expiration bigint
);
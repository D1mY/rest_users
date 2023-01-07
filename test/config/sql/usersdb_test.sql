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
-- truncate users;
-- insert into users
-- (
--   name,
--   login,
--   password,
--   passsalt,
--   created_at,
--   token,
--   token_expiration
-- )
-- values
-- (
--   'Joe A',
--   'joe',
--   '\xca1813fb01612604860ac13f8c769fe470cea7088f9fb312929641f2d6f7e64e',
--   '\x6ce535c3e0e261cea41d53ba46d0c2c25c9933d722e3202d94057a11ed2bf758',
--   1555740366,
--   'sT1eGzQVP3F5IaFid6XqvzshozsVakTa',
--   2555737627
-- );
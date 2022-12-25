create table if not exists users
(
 id bigserial,
 name text,
 login varchar(100) unique,
 password bytea not null,
 passsalt bytea not null,
 created_at integer,
 updated_at integer,
 token text,
 token_expiration integer
);
insert into users
(
  name,
  login,
  password,
  passsalt,
  created_at,
  token,
  token_expiration
)
values
(
  'Joe A',
  'joe',
  '\x18cafb13610104260a863fc1768ce49fce7008a79f8f12b39692f241f7d64ee6',
  '\xe56cc335e2e0ce611da4ba53d046c2c2995cd733e3222d200594117a2bed58f7',
  1555740366,
  'sT1eGzQVP3F5IaFid6XqvzshozsVakTa',
  1655740366
);
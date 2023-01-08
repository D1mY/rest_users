# rest_users

<details><summary>OTP application as REST API service</summary>
<p>
  
REST API сервис, предоставляющий функционал для:
- регистрации пользователя;
- авторизации пользователя;
- смены пароля пользователя;
- получения списка пользователей.

</p>
</details>

## Pre

Requires `Docker` & `Docker Compose`

## Get

`git clone`

## Set

Edit `.env` file if preferred.

## Test

`docker compose up ct`

## Deploy

`docker compose up`

## Usage

Start abuse it by URI:

`https://localhost:8443/`

<details>
<p>
  
  Метод `OPTIONS` возвращает описание ресурса.

  Регистрация / авторизация / изменение пароля возвращают новый токен.
  
  Изменение пароля / список пользователей доступен после аутентификации по токену.
  
  Если при регистрации не указано имя, будет использовано значение `Incognito`.
  
#### Регистрация пользователя:

> request
```
   POST /users
   Content-Type: application/json

    {
      "name": "string",
      "login": "string(max 100)",
      "password": "string"
    }
```
> response
```
  Content-Type: application/json
  Cache-Control: no-store
  
  {
    "token": "string",
    "token_expiration": "number(POSIX time)",
    "token_type": "Bearer"
  }
```
#### Авторизация пользователя:

> request
```
   POST /auth
   Content-Type: application/json

    {
      "login": "string",
      "password": "string"
    }
```
> response
```
  Content-Type: application/json
  Cache-Control: no-store
  
  {
    "token": "string",
    "token_expiration": "number(POSIX time)",
    "token_type": "Bearer"
  }
```
#### Изменение пароля:

> request
```
   PUT /users
   Authorization: Bearer <token>
   Content-Type: application/json

    {
      "login": "string",
      "password": "string",
      "newpassword": "string"
    }
```
> response
```
  Content-Type: application/json
  Cache-Control: no-store
  
  {
    "token": "string",
    "token_expiration": "number(POSIX time)",
    "token_type": "Bearer"
  }
```
#### Список пользователей:

> request
```
   GET /users
   Authorization: Bearer <token>
   Content-Type: application/json
```
> response
```
  Content-Type: application/json
  
  {
    "count": "number",
    "users": [
      {
        "id": "number",
        "name": "string"
      }
    ]
  }
```
  
</p>
</details>

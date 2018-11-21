## GET /

### Response:

- Status code 200
- Headers: []

- No response body

## GET /api/item

### GET Parameters:

- search
     - **Values**: *word0, word1, wordN*
     - **Description**: Search Item
     - This parameter is a **list**. All GET parameters with the name search[] will forward their values in a list to the handler.


### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

    ```javascript
[]
    ```

## POST /api/item

### Headers:

- This endpoint is sensitive to the value of the **Authorization** HTTP header.

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

    ```javascript
[]
    ```

## POST /api/login

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

    ```javascript
{"password":"Password","ident":"Identity"}
    ```

### Response:

- Status code 200
- Headers: [("access_token",""),("token_type",""),("expires_in",""),("refresh_token","")]

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

    ```javascript

    ```

## GET /api/vector

### GET Parameters:

- word
     - **Values**: *word0, word1, wordN*
     - **Description**: Search Vector


### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

    ```javascript
{"result":[{"sim":1,"word":"ResultWord0"},{"sim":0.1,"word":"ResultWord1"}]}
    ```


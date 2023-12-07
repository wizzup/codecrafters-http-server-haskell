[![progress-banner](https://backend.codecrafters.io/progress/http-server/2c230502-62c2-456a-84aa-4df7c91df9dd)](https://app.codecrafters.io/users/wizzup?r=2qF)

Haskell solutions to the
["Build Your Own HTTP server" Challenge](https://app.codecrafters.io/courses/http-server/overview).

[HTTP](https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol) is the
protocol that powers the web. In this challenge, you'll build a HTTP/1.1 server
that is capable of serving multiple clients.

Along the way you'll learn about TCP servers,
[HTTP request syntax](https://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html),
and more.

**Note**: If you're viewing this repo on GitHub, head over to
[codecrafters.io](https://codecrafters.io) to try the challenge.

```
$ nix build && cat result
[1/8] Running tests for connect-to-port
[1/8] Connecting to localhost:4221 using TCP
[1/8] Success! Closing connection
[1/8] Test passed.

[2/8] Running tests for respond-with-200
[2/8] You can use the following curl command to test this locally
[2/8] $ curl -v -X GET http://localhost:4221/
[2/8] Sending request (status line): GET / HTTP/1.1
[2/8] Test passed.

[3/8] Running tests for respond-with-404
[3/8] You can use the following curl command to test this locally
[3/8] $ curl -v -X GET http://localhost:4221/yikes/Coo-dooby
[3/8] Sending request (status line): GET /yikes/Coo-dooby HTTP/1.1
[3/8] Test passed.

[4/8] Running tests for respond-with-content
[4/8] You can use the following curl command to test this locally
[4/8] $ curl -v -X GET http://localhost:4221/echo/Horsey/Horsey
[4/8] Sending request (status line): GET /echo/Horsey/Horsey HTTP/1.1
[4/8] Test passed.

[5/8] Running tests for parse-headers
[5/8] You can use the following curl command to test this locally
[5/8] $ curl -v -X GET http://localhost:4221/user-agent -H "User-Agent: humpty/Monkey"
[5/8] Sending request (status line): GET /user-agent HTTP/1.1
[5/8] Test passed.

[6/8] Running tests for concurrent-connections
[6/8] Creating 3 parallel connections
[6/8] Sending request on 3 (status line): GET / HTTP/1.1
[6/8] Sending request on 2 (status line): GET / HTTP/1.1
[6/8] Sending request on 1 (status line): GET / HTTP/1.1
[6/8] Test passed.

[7/8] Running tests for get-file
[7/8] Testing existing file
[7/8] You can use the following curl command to test this locally
[7/8] $ curl -v -X GET http://localhost:4221/files/dooby_scooby_humpty_monkey
[7/8] Sending request (status line): GET /files/dooby_scooby_humpty_monkey HTTP/1.1
[7/8] Testing non existent file returns 404
[7/8] You can use the following curl command to test this locally
[7/8] $ curl -v -X GET http://localhost:4221/files/non-existentdooby_vanilla_vanilla_Monkey
[7/8] Sending request (status line): GET /files/non-existentdooby_vanilla_vanilla_Monkey HTTP/1.1
[7/8] Test passed.

[8/8] Running tests for post-file
[8/8] You can use the following curl command to test this locally
[8/8] $ curl -v -X POST http://localhost:4221/files/scooby_Coo_dooby_Horsey -d 'dumpty scooby humpty Monkey scooby donkey monkey yikes'
[8/8] Sending request (status line): POST /files/scooby_Coo_dooby_Horsey HTTP/1.1
[8/8] Test passed.
```

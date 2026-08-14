// tests/h2_server.js — HTTP/2 servers for tests/http2_test.ag.
//
// Node's built-in http2 module serves two endpoints:
//   - h2c (prior-knowledge plaintext) on 127.0.0.1:18444
//   - h2 over TLS (ALPN) on 127.0.0.1:18445, reusing tests/certs/server.pem
//
// Used by run_tests.sh only when node is available (same gate as tls_test).

const http2 = require('http2');
const fs = require('fs');

function handler(stream, headers) {
    const path = headers[':path'];
    if (path === '/hello') {
        stream.respond({
            ':status': 200,
            'content-type': 'text/plain',
            'x-h2': 'true',
        });
        stream.end('hello h2');
    } else if (path === '/redirect') {
        stream.respond({
            ':status': 302,
            location: '/hello',
        });
        stream.end('');
    } else {
        stream.respond({ ':status': 404 });
        stream.end('not found');
    }
}

const h2c = http2.createServer();
h2c.on('stream', handler);
h2c.listen(18444, '127.0.0.1', () => console.log('H2C_NODE_READY'));

const options = {
    key: fs.readFileSync('tests/certs/server.pem'),
    cert: fs.readFileSync('tests/certs/server.pem'),
};
const h2 = http2.createSecureServer(options);
h2.on('stream', handler);
h2.listen(18445, '127.0.0.1', () => console.log('H2_NODE_READY'));

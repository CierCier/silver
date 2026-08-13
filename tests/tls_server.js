// tests/tls_server.js — HTTPS server for tests/tls_test.ag.
//
// Serves the committed self-signed certificate (tests/certs/server.pem,
// SANs localhost + 127.0.0.1) on 127.0.0.1:18443. The Silver client
// verifies it against tests/certs/ca.pem. Used by run_tests.sh only when
// node is available.
const https = require('https');
const fs = require('fs');

const options = {
    key: fs.readFileSync('tests/certs/server.pem'),
    cert: fs.readFileSync('tests/certs/server.pem'),
};

const server = https.createServer(options, (req, res) => {
    if (req.url === '/secure') {
        res.writeHead(200, { 'Content-Type': 'text/plain', 'Content-Length': '5' });
        res.end('hello');
    } else if (req.url === '/redirect') {
        res.writeHead(302, { 'Location': '/secure', 'Content-Length': '0' });
        res.end();
    } else if (req.url === '/big-header') {
        // ~18KB of response headers for the TLS header-parsing benchmark.
        const h = { 'Content-Length': '2' };
        for (let i = 0; i < 300; i++) { h['X-Big-' + i] = 'abcdefghijklmnopqrstuvwxyz0123456789'; }
        res.writeHead(200, h);
        res.end('ok');
    } else if (req.url === '/cookie') {
        res.writeHead(200, { 'Set-Cookie': 'sec=1; Secure; Path=/', 'Content-Length': '2' });
        res.end('ok');
    } else if (req.url === '/cookie-check') {
        const cookie = req.headers.cookie || '';
        res.writeHead(200, { 'Content-Type': 'text/plain', 'Content-Length': String(cookie.length) });
        res.end(cookie);
    } else {
        res.writeHead(404, { 'Content-Type': 'text/plain' });
        res.end('nope');
    }
});

server.listen(18443, '127.0.0.1', () => console.log('TLS_NODE_READY'));

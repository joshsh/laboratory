#!/usr/bin/env python
import sys
import web  # sudo easy_install web.py

urls = (
    '/test', 'test',
)

if ( len(sys.argv) >= 3 ):
    host = sys.argv[1]
    port = int(sys.argv[2])
else:
    # if in doubt, use 0.0.0.0
    host = '0.0.0.0'
    port = 8080

print "Minimal REST server for network troubleshooting"
print "Usage: ./minimal-rest-server.py [host] [port]"

class MyApplication(web.application):
    def run(self, *middleware):
        func = self.wsgifunc(*middleware)
        return web.httpserver.runsimple(func, (host, port))

class test:
    def GET(self):
        return 'Hello, World!'
    def POST(self):
        return "Received: " + web.data()

app = MyApplication(urls, globals())

if __name__ == "__main__":
    app.run()


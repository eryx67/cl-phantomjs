//  Derived from ghostweb: phantomjs controlling server of Nic Ferrier
try {

    var system = require('system');
   // initialize CasperJS
    phantom.casperPath =  system.args[2] + '/casperjs';
    // we want to use Casper patched require
    phantom.casperScriptBaseDir = system.args[2];
    phantom.injectJs(phantom.casperPath + '/bootstrap.js');

    var base64 = require('base64');
    var server = require('webserver').create();
    // Start the server on whatever we were told to listen to.

    var service = server.listen(
        system.args[1], function (request, response) {
            if (request.headers.command == "call") {

                var cmdArg = base64.decode(request.headers.commandarg)
                        .replace(/^\x00+|\x00+$/g, '');

                var f = Function(
                    "try { return "
                        + cmdArg
                        + "} catch (e) { return {'type': 'error', 'name': e.name, 'message': e.message }; }"
                );
                var retval = f();

                if (retval["type"] == 'error') {
                    response.statusCode = 400;
                    retval.command = cmdArg;
                }
                else {
                    response.statusCode = 200;
                }
                response.write(base64.encode(JSON.stringify(retval)));
                response.close();
            }
            else if (request.headers.command == "exit") {
                response.statusCode = 200;
                response.write("Ok\n");
                response.close();
                phantom.exit();
            }
            else {
                response.statusCode = 404;
                response.write("Unknown\n");
                response.close();
            }
        });

    console.log(service ? "$phjs> started" : "$phjs> failed");
}
catch (e) {
    console.log(e);
    phantom.exit();
}

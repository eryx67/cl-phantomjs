//  Derived from ghostweb: phantomjs controlling server of Nic Ferrier
try {
    var system = require('system');
    var server = require('webserver').create();
    // Start the server on whatever we were told to listen to.
    var service = server.listen(
        system.args[1], function (request, response) {
            if (request.headers.command == "call") {
                var cmdArg = atob(request.headers.commandarg);
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
                response.write(btoa(JSON.stringify(retval)));
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

    // initialize CasperJS
    phantom.casperPath =  system.args[2] + '/casperjs';
    phantom.injectJs(phantom.casperPath + '/bootstrap.js');
    console.log(service ? "$phjs> started" : "$phjs> failed");
}
catch (e) {
    console.log(e);
    phantom.exit();
}

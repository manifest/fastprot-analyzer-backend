# REST API

FAST Protocol Analyzer Service, version 0.1.0.

With REST API you can decode FAST encoded messages and access available templates. 

## Base URL Path

All API paths listed below are relative to the following base URL path:

	http://fpas.yanot.org/api

and example with specified version number:

	http://fpas.yanot.org/api/1

If you open those URLs in the browser, you will be redirected to documentation page:

	> GET /api
	
	< HTTP/1.1 303 See Other
	< location: http://manifest.github.io/fastprot-analyzer-backend/manual.html

## API Reference

### Templates

To get a list of available templates:

	> GET /api/templates
	
	< HTTP/1.1 200 OK
	< content-type: application/json
	
	["rtsx"]

To get specified template:

	> GET /api/templates/:template
	
	< HTTP/1.1 200 OK
	< content-type: application/json

	{"data":"<templates xmlns=\"http://www.fixprotocol.org/ns/fast/td/1.1\">
	...

If template doesn't exist, error would be received:

	> GET /api/templates/:not_exist_template
	
	< HTTP/1.1 404 Not Found
	< content-type: application/json

	{"error":"Requested template does not exist."}

It possible to retrieve the xml content of template without json cover. Set the __accept__ header for that:

	> GET /api/templates/:template
	> accept: application/xml

	< HTTP/1.1 200 OK
	< content-type: application/xml

	<templates xmlns="http://www.fixprotocol.org/ns/fast/td/1.1">
	...

In this case, the error is also returned as xml:

	> GET /api/templates/:not_exist_template
	> accept: application/xml

	< HTTP/1.1 404 Not Found
	< content-type: application/xml

	<error>Requested template does not exist.</error>

### Messages

To decode message:

	> POST /api/templates/:template/decode
	> content-type: application/octet-stream

	{FAST encoded message}

	< HTTP/1.1 200 OK
	< content-type: application/json

	[{"type":"field","tag":"1128","name":"ApplVerID","value":"8"} ...

In case of corrupted payload data, the response's body will contain an error reason:

	> POST /api/templates/:template/decode
	> content-type: application/octet-stream

	{corrupted message}
	
	< HTTP/1.1 422 Unprocessable Entity
	< content-type: application/json

	{"error":"[ERR U03] EOF while decoding presence map."}

if the request does not contain data, error will be returned:

	> POST /api/templates/:template/decode
	> content-type: application/octet-stream
	
	< HTTP/1.1 400 Bad Request
	< content-type: application/json

	{"error":"Request does not contain data."}

## Cross-origin resource sharing (CORS)

Each response contains HTTP access control headers which make possible cross-site requests.

	< access-control-allow-credentials: true
	< access-control-allow-origin: *

## HTML Status Codes

The API commonly returns the following codes.  For further reference, please see [W3C's documentation][w3c_status_codes].

__200 OK__
Returned whenever a resource is listed, viewed, updated or deleted.

__400 Bad Request__
Returned whenever a request cannot be fulfilled because of an error with the input.

__404 Not Found__
Returned whenever the resource being requested does not exist.

__422 Unprocessable Entity__
Returned whenever a request was well-formed but was unable to be followed due to semantic errors.

__405 Method Not Allowed__
Returned whenever the HTTP method (e.g. GET or POST) is not supported for the resource being requested.

__415 Unsupported Media Type__
Returned whenever a request entity has a media type which the server or resource does not support.

[w3c_status_codes]:http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html


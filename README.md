# FAST Protocol Analyzer Service

RESTful web service for analyzing FAST protocol messages.

### On the Web
- [REST API Manual][manual]
- [Web Client][fpaw]

### Depends on
- [fastprot][fastprot] - FAST protocol message decoder for erlang
- [cowboy][cowboy] - small, fast, modular HTTP server

### Get the Sources

	$ git clone https://github.com/manifest/fastprot-analyzer-backend.git
	$ cd fastprot-analyzer-backend

### Getting Started

1. Build and install [fastprot][fastprot] dependencies (native libraries). Read more on his page.

2. Get other dependencies (erlang applications):

		$ make deps

3. Build application: 

		$ make

4. It is recommended to run the unit tests as well:

		$ make tests

5. Run application:

		$ make start

### License

FAST Protocol Analyzer Service is provided under the terms of [the MIT license][license].

[fpaw]:https://github.com/manifest/fastprot-analyzer-frontend
[manual]:http://manifest.github.io/fastprot-analyzer-backend/manual.html
[fastprot]:https://github.com/manifest/fastprot
[cowboy]:https://github.com/extend/cowboy
[license]:http://www.opensource.org/licenses/MIT


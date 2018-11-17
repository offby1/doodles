import logging
import responder

api = responder.API()


@api.route("/{greeting}")
async def greet_world(req, resp, *, greeting):
    resp.text = f"{greeting}, world!"


@api.route("/dumb")
async def dumb(req, resp):
    resp.content = api.template('dumb.html', variable='wobbly')

if __name__ == '__main__':
    # fix logging to be more like RFC3339
    logging.basicConfig(level=logging.DEBUG,
                        format='%(asctime)s %(levelname)-5.5s [%(name)s][%(threadName)s] %(message)s')
    logging.Formatter.default_time_format = '%FT%T'
    logging.Formatter.default_msec_format = '%s.%03dZ'

    api.run()

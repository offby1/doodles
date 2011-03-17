from pyramid.config import Configurator
from howdy.resources import Root

def main(global_config, **settings):
    """ This function returns a Pyramid WSGI application.
    """
    config = Configurator(root_factory=Root, settings=settings)
    config.add_view('howdy.views.my_view',
                    context='howdy:resources.Root',
                    renderer='howdy:templates/mytemplate.pt')
    config.add_static_view('static', 'howdy:static')
    return config.make_wsgi_app()


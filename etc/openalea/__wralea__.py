

__doc__ = """ Capsis nodes """
__revision__=" $Id "


from openalea.core import *
from openalea.core.pkgdict import protected

__name__ = "capsis"

__version__ = '0.0.1',
__license__ = 'CECILL-C'
__authors__ = 'S. Dufour-Kowalski, S. de Coligny'
__institutes__ = 'INRA'
__description__ = 'Capsis models'
__url__ = 'http://capsis.cirad.fr'



__all__ = ['maddmodule1', 'list2objs']

maddmodule1 = Factory(name='maddmodule1', 
                   description='MaddModule', 
                   category='forestry', 
                   nodemodule='maddmodule',
                   nodeclass='maddmodule1',
                   inputs = (dict(name='filename', interface=IFileStr, value=""),
                             dict(name='nbyear', interface=IInt, value=10),
                             ),
                   outputs=(dict(name='stand', interface=ISequence),)
                   )


list2objs = Factory(name='list2objs', 
                    description='MaddModule', 
                    category='forestry', 
                    nodemodule='maddmodule',
                    nodeclass='list2objs',
                    inputs = (dict(name='list', interface=ISequence),
                              ),
                    outputs=(dict(name='objs', interface=ISequence),)
                   )


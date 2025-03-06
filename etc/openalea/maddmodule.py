import os

# import fake package to get the directory
import path

from jpype import *



def maddmodule1(filename, nbyear):

    return run(filename, nbyear)




def build_classpath(basedir):
    """ list jar in a directory """
    res = ""
    
    for f in os.listdir(basedir):
        if(f.endswith(".jar")):
            res += basedir + os.sep + f + os.pathsep
        
    return res[:-1]


def run(filename, nbyear):

    # find capsis path with fake package path
    BASEDIR = os.path.dirname(str(path).split()[3][1:-9])
    BASEDIR = os.path.normpath(BASEDIR + "/../../")


    # Start JVM : warning JAVA_HOME must be set
    if not isJVMStarted() : 
        jarcp = build_classpath(BASEDIR + "/ext")
        startJVM(getDefaultJVMPath(), 
                 "-Djava.class.path="+BASEDIR+"/bin:"+BASEDIR+"/class/:" + jarcp)


    # Start capsis script
    script = JPackage ('maddmodule').myscripts.MaddScript1

    s = script()
    s.run(filename, nbyear)

    stand = s.getResult().getScene()

    return stand_to_list(stand)



def stand_to_list(stand):
    """ Convert a TreeList to a list 
    [(age, dbh, height, x, y), (age, dbh, height, x, y), ...]
    """
    
    ret = []
    
    for t in stand.getTrees():
        
        age = t.getAge()
        dbh = t.getDbh()
        h = t.getHeight()
        x = t.getX()
        y = t.getY()
        cbh = t.getCrownBaseHeight()
        cr = t.getCrownRadius()
    
        ret.append( (age, x, y, dbh, h, cbh, cr ) )
    
    
    return ret



def list2objs (l):

    from openalea.csv.csv import Obj

    header = "X,Y,Circonference,Haut,BaseHoup,r_houp1,a_houp1,r_houp2,a_houp2,r_houp3,a_houp3,r_houp4,a_houp4,r_houp5,a_houp5,r_houp6,a_houp6,r_houp7,a_houp7"

    header = header.split(',')

    ret = []
    for i, values in enumerate(l):

        cr = values[6] * 100
        o = Obj(i+1, header, [str(values[1] * 100), str(values[2]*100), str(values[3]), 
                              str(values[4]), str(values[5]), 
                              str(cr), str(214), str(cr), str(357), str(cr), str(26), 
                              str(cr), str(116),])
                
        ret.append(o)

    return ret

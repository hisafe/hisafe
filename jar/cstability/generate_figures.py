#!/usr/bin/env python3
##############################################################################################
# generate_figures.py : a generator of figures for C-STABILITY outputs.
#
#   run with python3, the library matplotlib is mandatory.
#
#   figures are plotted in a subdirectory of observation-files directory named figures.
#
#   arguments required : 
#     - directory: Directory containing C-STABILITY observation files to plot. Can be a
#     relative or absolute path. All files are plotted by default.
#
#   optional arguments :
#     --observation-files: Names of C-STABILTY observation files to plot in directory. If not
#     precised by user, all obervation files from the directory are plotted.
#     --file-extension: Extension of figure files, default *.png
#    
# @author: J. Sainte-Marie - May 2021
#
#############################################################################################

import sys, argparse, os, csv, copy
try:
    import matplotlib.pyplot as plt
except:
    sys.exit("ERROR: the library matplotlib is mandatory.")
    
#############################################################################################
# Definition of parsed arguments
parser = argparse.ArgumentParser()

parser.add_argument('directory', type=str, help="Directory containing C-STABILITY \
observation files to plot. Can be a relative or absolute path. All files are plotted by \
default.")

parser.add_argument('-of', '--observation-files', nargs='+', type=str,
                    dest='observationFiles', default=[], help="Names of C-STABILTY \
observation files to plot in directory. If not precised by user, all obervation files from \
the directory are plotted.",
)

parser.add_argument('-ext', '--file-extension', type=str, dest='ext', default="png",
                    help="Extension of figure files, default *.png")

args = parser.parse_args()

#############################################################################################
# Creation if needed of figures directory
if not os.path.isabs(args.directory):
    args.directory = os.path.abspath(args.directory)

figurePath = args.directory + "/figures"

if not os.path.exists(figurePath):
    os.mkdir(figurePath)

#############################################################################################
# Clear figurePath
for root, dirs, files in os.walk(figurePath):
    for file in files:
        os.remove(os.path.join(root, file))
for root, dirs, files in os.walk(figurePath):
    for dir in dirs:
        os.rmdir(os.path.join(root, dir))

#############################################################################################
# Creation of fileNames list of files to plot
if len(args.observationFiles) > 0 :
    observationFileNames = []
    # if -of is not empty, we check if files exists
    for fn in args.observationFiles:
        if os.path.exists(args.directory + fn) and fn.endswith(".csv"):
            observationFileNames.append(fn)
        else:
            print("WARNING: the file " + fn + " does not exist or is not a *.csv file.")
else :   
    # if -of is empty, all files are plotted
    observationFileNames = [fn for fn in os.listdir(args.directory) if fn.endswith(".csv")]
    
#############################################################################################
# observationType(): determines the type of observation of the file with file name fn
# available type:
# * double
# * distribution
# * map of distribution
def observationType(fn:str) :
    type = "Double"
    if fn.endswith("distribution.csv"):
        type = "Distribution"
    elif fn.endswith("distribution_map.csv"):
        type = "DistributionMap"
    return type

#############################################################################################
# scatter():
def scatter(x ,y , xlabel, ylabel, title, savepath, ext) :
    fig, ax = plt.subplots()
    ax.scatter(x, y, color='k')
    ax.set(xlabel=xlabel, ylabel=ylabel, title=title)
    ax.grid()
    fig.savefig(savepath + "." + ext)
    plt.close(fig)

#############################################################################################
# plotDouble(): plot observation file of type double
# structure of the file: date \t observation
def plotDouble(ofn:str, ext:str) :
    tl = [] # timeline
    ol = [] # observationList
    with open(os.path.join(args.directory,ofn)) as csv_file:
        csv_reader = csv.reader(csv_file, delimiter='\t')
        for row in csv_reader:
            try: 
                tl.append(int(row[0]))
                ol.append(float(row[1]))
            except :
                headerDate = row[0]
                headerObservation = row[1]
                continue
            
    scatter(tl, ol, headerDate, headerObservation, os.path.splitext(ofn)[0],
            figurePath + "/" + os.path.splitext(ofn)[0], ext)
        
#############################################################################################
# plotDistribution(): plot observation file of type distribution
# structure of the file: date \t x \t y
def plotDistribution(ofn:str, ext:str) :
    
    distpath = os.path.join(figurePath, os.path.splitext(ofn)[0])
    if not os.path.exists(distpath):
        os.mkdir(distpath)

    tl = [] # timeline
    dl = [] # distributionList
    
    with open(os.path.join(args.directory,ofn)) as csv_file:
        csv_reader = csv.reader(csv_file, delimiter='\t')
        d = []
        for row in csv_reader:
            try:
                if len(tl) == 0:
                    tl.append(int(row[0]))
                    d.append(list(row[1:3]))
                else :
                    if int(row[0]) == tl[-1]:
                        d.append(list(row[1:3]))
                    else :
                        tl.append(int(row[0]))
                        dl.append(copy.deepcopy(d))
                        d = []
            except:
                headerDate = row[0]
                headerX = row[1]
                headerY = row[2]
                continue
    dl.append(copy.deepcopy(d))

    for t in tl:
        headerDateT = headerDate + "_" + str(t)
        d = dl[tl.index(t)]
        xl = []
        yl = []
        for xy in d :
            xl.append(float(xy[0]))
            yl.append(float(xy[1]))
        scatter(xl, yl, headerX, headerY, headerDateT,
                distpath + "/" + os.path.splitext(ofn)[0] + "_" + headerDateT, ext)
         
#############################################################################################
# plotDistributionMap(): plot observation file of type distributionMap
# structure of the file: date \t mapKey \t x \t y
def plotDistributionMap(ofn:str, ext:str) :
        
    distpath = os.path.join(figurePath, os.path.splitext(ofn)[0])
    if not os.path.exists(distpath):
        os.mkdir(distpath)

    tl = [] # timeline
    ml = [] # mapList
    with open(os.path.join(args.directory,ofn)) as csv_file:
        csv_reader = csv.reader(csv_file, delimiter='\t')
        m = []
        for row in csv_reader :
            try:
                if len(tl) == 0:
                    tl.append(int(row[0]))
                    m.append(list(row[1:4]))
                else :
                    if int(row[0]) == tl[-1]:
                        m.append(list(row[1:4]))
                    else :
                        tl.append(int(row[0]))
                        ml.append(copy.deepcopy(m))
                        m = []
            except:
                headerDate = row[0]
                headerMapKey = row[1]
                headerX = row[2]
                headerY = row[3]
    ml.append(copy.deepcopy(m))
                
    for t in tl:
        headerDateT = headerDate + "_" + str(t)
        map = ml[tl.index(t)]
        keyList = [] # mapKeys
        dl = [] # distribution list
        d = []
        for row in map :
            if len(keyList) == 0 :
                keyList.append(row[0])
                d.append(copy.deepcopy(row[1:3]))
            else :
                if row[0] == keyList[-1]:
                    d.append(copy.deepcopy(row[1:3]))
                else :
                    keyList.append(row[0])
                    dl.append(copy.deepcopy(d))
                    d = []
        dl.append(copy.deepcopy(d))

        for key in keyList:
            headerDateTKey = headerDateT + "_" + key
            d = dl[keyList.index(key)]
            xl = []
            yl = []
            for xy in d :
                xl.append(float(xy[0]))
                yl.append(float(xy[1]))
            scatter(xl, yl, headerX, headerY, headerDateTKey,
                    distpath + "/" + os.path.splitext(ofn)[0] + "_" + headerDateTKey, ext)
                    
#############################################################################################
# plot(): plot observation file
def plot(ofn:str, ext:str) :
    fileType = observationType(ofn)
    if fileType == "Double":
        plotDouble(ofn, ext)
    elif fileType == "Distribution":
        plotDistribution(ofn, ext)
    elif fileType == "DistributionMap":
        plotDistributionMap(ofn, ext)
    else :
        print("WARNING: unrecognized type")        

#############################################################################################
# Call of the plot function for observation files in fileNames list
def main(ofnl, ext):
    for ofn in ofnl :
        print("Plotting : " + ofn)
        plot(ofn, ext)

#############################################################################################
main(observationFileNames, args.ext)

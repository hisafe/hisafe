# Convert one or more trees into logs
# Input file contains one or more trees described in AS format and a set of log specifications
# Output file contains logs described in AS format
# At this stage the output file does not contain clusters or pith !!!

# the following line is for compiled version only
#global jobId, treeId, logId;
#global NSPECS, logLength, logMinSED;
#global NSECTIONS, secz, secDiam;
#global tl_p, tl_d, is_d;


BEGIN {
	FN = 1;
	outfile = ARGV[2];
#	print outfile;
}

#1
#User selection of trees to buck into logs. Copied from D:\pontd\P11PFGQ\GMWQ\TREEGROW\SYSTEM1\DEMO\T1012717
#1
FN==1 && FNR==1 {
	FN++;
	
#	local i, logSpec, NTREES, t, sec, NCLUSTERS, clu, cluz, NBRANCHES, j, bra, NPITH, pit, pitz;

	jobId=$0;
	getline;	
#	jobDescription=$0;
	printf( "jobId:%s\n", jobId ) > "CON";

# "Read Specs";
# Log specs: length, minSED	!!! could support a length range, max branch...
#2
#4.1 300
#6.1 200
	getline;
	NSPECS = $0+0;
# "NSPECS:" NSPECS;
	for( i=0; i<NSPECS; i++ ) {
		getline;
		logSpec[i] = $0;
		split( " ", logSpec[i] );
		logLength[i] = $1+0;
		logMinSED[i] = $2+0;
	}


#print "Read Trees";
	getline;	
	NTREES=$0+0;
#print "NTREES:" NTREES;
	for( t=0; t<NTREES; t++ ) {
		getline;	
		treeId=$0;
		printf( "treeId:%s\n", treeId ) > "CON";

#print "Read Sections";
# Sections
#28
#0 0 0 258 258 0
		getline;
		NSECTIONS = $0+0;
#print "NSECTIONS:" NSECTIONS;
		for( i=0; i<NSECTIONS; i++ ) {
			getline;
			sec[i] = $0;
			split( " ", sec[i] );
			secz[i] = $3+0;
			secDiam[i] = $4*2;	# !!! just using first radius
		}

# Clusters
#98
#0 0 99 12
#print "Read Clusters";
		getline;
		NCLUSTERS = $0+0;
#print "NCLUSTERS:" NCLUSTERS;
		for( i=0; i<NCLUSTERS; i++ ) {
			getline;
			clu[i] = $0;
			split( " ", clu[i] );
			cluz[i] = $3+0;
			NBRANCHES[i] = $4+0;
#print "NBRANCHES:" NBRANCHES[i];
			clut[i] = 0;	# cluster top
			for( j=0; j<NBRANCHES[i]; j++ ) {
# Branches
#309  0.34 15  0.58 0
				getline;
				bra[i][j] = $0;
				split( " ", bra[i][j] );
				# calculate longitudinal extent of each branch
				# taking into account length, rake and radius
				blen_mm  = $1+0;	# branch length
				brad_mm  = $3+0;	# branch radius
				brak_rad = $4+0;	# branch rake
				cb = blen_mm*sin( brak_rad );
				cc = brad_mm*cos( brak_rad );
				ct = cluz[i] + cb + cc;
				# keep maximum for the cluster
				if( ct > clut[i] ) {
					clut[i] = ct;
				}
			}
		}

#print "Read Pith";
# Pith
#2
#0 0 0
#0 0 41269
		getline;
		NPITH = $0+0;
#print "NPITH:" NPITH;
		for( i=0; i<NPITH; i++ ) {
			getline;
			pit[i] = $0;
			split( " ", pit[i] );
			pitz[i] = $3+0;
		}

		
#print "Cut logs";
		# cut this tree into logs and add them to the output
		cutLogs();
		
	}	# tree loop

}

function cutLogs() {
#	local cl_logStart, cl_l;
	
	# logs are tried in order, crude priority cutting list
	cl_logStart=0;
	cl_l=0;
	while( cl_l<NSPECS ) {
#print "Log spec:" cl_l+1	;
		if( tryLog( cl_logStart, cl_l ) ) {
			writeLog();	
			cl_logStart = tl_p[1];	# start of next log is end of this log
			cl_l=0;	# reset spec index to try again from top of list
#break;	# !!!			
		}
		else {
			cl_l++;	# try next spec in list
		}
	}
}

function tryLog( tl_logStart, tl_l ) {
#	tl_logPossible = 0;
	
	if( interpolateSection( tl_logStart ) ) {
		tl_p[0] = tl_logStart;	# log start position
		tl_d[0] = is_d[2];	# log LED
		if( interpolateSection( tl_logStart+logLength[tl_l] ) ) {
			tl_p[1] = tl_logStart+logLength[tl_l];	# log end position
			tl_d[1] = is_d[2];	# log SED
#printf( "Log start:%g end%g\n", tl_p[0], tl_p[1] );
#printf( "Is tl_d[1] >= logMinSED[tl_l] %g > %g ?\n", tl_d[1], logMinSED[tl_l] );
			if( tl_d[1] >= logMinSED[tl_l] ) {	# if log SED meets or exceeds the limit
#				tl_logPossible = 1;
				return 1;
			}
		}
	}
#	return tl_logPossible;
	return 0;
}

function interpolateSection( is_position ) {
#	local is_s, is_r, is_p;
#	sectionNew = 0;
#	sectionIndex = -1;
#	is_sectionPossible = 0;

	# find first section >= position
	for( is_s=0; is_s<NSECTIONS; is_s++ ) {
		if( secz[is_s] == is_position ) {	# this section at position
			is_d[2] = secDiam[is_s];
#			sectionNew = 0;
#			sectionIndex = is_s;
#			is_sectionPossible = 1;
#			break;
			return 1;
		}
		else if( is_s>0 && secz[is_s] > is_position ) {	# this section beyond position
			# positions and diameters of the surrounding sections
			is_p[0] = secz[is_s-1];
			is_d[0] = secDiam[is_s-1];
			is_p[1] = secz[is_s];
			is_d[1] = secDiam[is_s];

			# interpolate log SED
			is_r = (is_position-is_p[0])/(is_p[1]-is_p[0]);
			is_d[2] = is_d[1] + is_r*(is_d[0]-is_d[1]);	# NB: diameter decreases with length

#			sectionNew = 1;
#			sectionIndex = is_s;
#			is_sectionPossible = 1;
#			break;
			return 1;
		}
	}
#	return is_sectionPossible;
	return 0;
}

function writeLog() {
#	local wl_nSections, wl_s;
	
	logId++;
	printf( "Log:%4d\n", logId ) > "CON";
	printf( "Job %s Tree %s Log %d\n", jobId, treeId, logId ) > outfile;
	
	# count sections
	wl_nSections = 2;	# at least 2 sections, now count tree sections between log start and end
	for( wl_s=0; wl_s<NSECTIONS; wl_s++ ) {
		if( secz[wl_s] > tl_p[0] && secz[wl_s] < tl_p[1] ) {
			wl_nSections++;
		}
	}
	
	# write sections
	printf( "%d\n", wl_nSections ) >> outfile;
#0 0 0 258 258 0
	printf( "0 0 0 %d %d 0\n", tl_d[0], tl_d[0] ) >> outfile;	# SE section
	for( wl_s=0; wl_s<NSECTIONS; wl_s++ ) {
		if( secz[wl_s] > tl_p[0] && secz[wl_s] < tl_p[1] ) {
			# section lies between log end
			printf( "0 0 %g %d %d 0\n", secz[wl_s]-tl_p[0], secDiam[wl_s], secDiam[wl_s] ) >> outfile;	# intermediate section
		}
	}
	printf( "0 0 %d %d %d 0\n", tl_p[1]-tl_p[0], tl_d[1], tl_d[1] ) >> outfile;	# LE section
	
# !!! cluster positions need work...
#	# count clusters which intersect the log
#	wl_nClusters = 0;
#	for( wl_c=0; wl_c<NCLUSTERS; wl_c++ ) {
#		# if cluster base inside log or cluster top inside log
##printf( "if( (cluz[wl_c]:%g > tl_p[0]:%g && cluz[wl_c]:%g < tl_p[1]:%g) || (clut[wl_c]:%g > tl_p[0]:%g && clut[wl_c]:%g < tl_p[1]:%g) )\n", cluz[wl_c], tl_p[0], cluz[wl_c], tl_p[0], clut[wl_c], tl_p[1], clut[wl_c], tl_p[1] );
#		if( (cluz[wl_c] > tl_p[0] && cluz[wl_c] < tl_p[1]) || (clut[wl_c] > tl_p[0] && clut[wl_c] < tl_p[1]) ) {
#			wl_nClusters++;
#		}
#	}
#
#	# write clusters and their branches
#	printf( "%d\n", wl_nClusters ) >> outfile;
#	for( wl_c=0; wl_c<NCLUSTERS; wl_c++ ) {
#		# if cluster base inside log or cluster top inside log
#		if( (cluz[wl_c] > tl_p[0] && cluz[wl_c] < tl_p[1]) || (clut[wl_c] > tl_p[0] && clut[wl_c] < tl_p[1]) ) {
#			split( " ", clu[wl_c] );
#			printf( "%g %g %g %d\n", $1+0, $2+0, $3+0-tl_p[0], $4+0 ) >> outfile;
#			for( wl_b=0; wl_b<NBRANCHES[wl_c]; wl_b++ ) {
#				printf( "%s\n", bra[wl_c][wl_b] ) >> outfile;
#			}
#		}
#	}
}
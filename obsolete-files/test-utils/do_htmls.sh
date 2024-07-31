#!/bin/bash

DATE=`date +%F`
cd TESTS/${DATE}

OTHERS=`ls | grep -v html | grep -v \.md | grep -v \.txt | grep -v McStas-2.5_CPU_MPICC_5e7`

echo Reference is McStas-2.5_CPU_MPICC_5e7
echo Other datasets are these:
echo $OTHERS

for folder in `find McStas-2.5_CPU_MPICC_5e7 -name mccode.sim | cut -f2-3 -d/`
do
    # First, create index on the ref side
    echo Working on $folder
    cp ${HOME}/McCode/generate_testplots/header.html McStas-2.5_CPU_MPICC_5e7/$folder/browse.html
    echo "<h1>$folder - Full overview</h1>" >> McStas-2.5_CPU_MPICC_5e7/$folder/browse.html
    echo "(<a href=\"/${DATE}/McStas-2.5_CPU_MPICC_5e7/${folder}\">Click to access files</a>)" >> McStas-2.5_CPU_MPICC_5e7/$folder/browse.html
    echo "<hr>" >> McStas-2.5_CPU_MPICC_5e7/$folder/browse.html    
    echo "<hr>" >> McStas-2.5_CPU_MPICC_5e7/$folder/browse.html
    echo "<h2>McStas-2.5_CPU_MPICC_5e7 (reference)</h2>" >> McStas-2.5_CPU_MPICC_5e7/$folder/browse.html
    echo "<table width=\"100%\">" >> McStas-2.5_CPU_MPICC_5e7/$folder/browse.html
    echo "<tr>" >> McStas-2.5_CPU_MPICC_5e7/$folder/browse.html
    i=1
    for ref_png in `ls -rt McStas-2.5_CPU_MPICC_5e7/${folder}/*.png`
    do 
	img="/${DATE}/$ref_png"
	echo "<td><a href=\"${img}\"><img width=\"100%\" src=\"${img}\"}</a><td>" >> McStas-2.5_CPU_MPICC_5e7/$folder/browse.html
    done
    echo "</tr></table>" >> McStas-2.5_CPU_MPICC_5e7/$folder/browse.html
    for other in `echo $OTHERS`
    do
	echo "<hr>" >> McStas-2.5_CPU_MPICC_5e7/$folder/browse.html
	echo "<h2>${other}</h2>" >> McStas-2.5_CPU_MPICC_5e7/$folder/browse.html
	echo "<table width=\"100%\">" >> McStas-2.5_CPU_MPICC_5e7/$folder/browse.html
	
	# Header and reference for detailed comparison
	cp ${HOME}/McCode/generate_testplots/header.html ${other}/$folder/browse.html
	echo "<h1>$folder - comparison McStas-2.5_CPU_MPICC_5e7 vs ${other}</h1>" >> ${other}/$folder/browse.html
	echo "(<a href=\"/${DATE}/${other}/${folder}\">Click to access files</a>)" >> ${other}/$folder/browse.html
	echo "<hr>" >> McStas-2.5_CPU_MPICC_5e7/$folder/browse.html    
	echo "<hr>" >> ${other}/$folder/browse.html
	echo "<h2>McStas-2.5_CPU_MPICC_5e7 (reference)</h2>" >> ${other}/$folder/browse.html
	echo "<table width=\"100%\">" >> ${other}/$folder/browse.html
	echo "<tr>" >> ${other}/$folder/browse.html
	for ref_png in `ls -rt McStas-2.5_CPU_MPICC_5e7/${folder}/*.png`
	do
            img="/${DATE}/$ref_png"
            echo "<td><a href=\"${img}\"><img width=\"100%\" src=\"${img}\"}</a><td>" >> ${other}/$folder/browse.html
	done
	echo "</tr></table>" >> ${other}/$folder/browse.html
	
	echo "<hr>" >> ${other}/$folder/browse.html
        echo "<h2>${other}</h2>" >> ${other}/$folder/browse.html
        echo "<table width=\"100%\">" >> ${other}/$folder/browse.html
	for otherref_png in `ls -rt ${other}/${folder}/*.png | grep -v _diff.png`
	do  
	    img="/${DATE}/$otherref_png"
	    echo "<td><a href=\"${img}\"><img width=\"100%\" src=\"${img}\"}</a><td>" >> McStas-2.5_CPU_MPICC_5e7/$folder/browse.html
	    echo "<td><a href=\"${img}\"><img width=\"100%\" src=\"${img}\"}</a><td>" >> ${other}/$folder/browse.html
	done
	echo "</tr></table>" >> McStas-2.5_CPU_MPICC_5e7/$folder/browse.html
	echo "</tr></table>" >> ${other}/$folder/browse.html

	echo "<hr>" >> ${other}/$folder/browse.html
	echo "<h2>Difference</h2>" >> ${other}/$folder/browse.html
        echo "<table width=\"100%\">" >> ${other}/$folder/browse.html
        for otherref_png in `ls -rt ${other}/${folder}/*_diff.png`
        do
            img="/${DATE}/$otherref_png"
            echo "<td><a href=\"${img}\"><img width=\"100%\" src=\"${img}\"}</a><td>" >> ${other}/$folder/browse.html
        done
	echo "</tr></table>" >> ${other}/$folder/browse.html
	cat ${HOME}/McCode/generate_testplots/footer.html >> ${other}/$folder/browse.html
    done
    #echo "</tr>" >> McStas-2.5_CPU_MPICC/$folder/index.html
    #for other in `echo $OTHERS`
    #do
    #	`ls -rt ${other}/${folder}/*.png | grep -v _diff.png > ${other}/${folder}/pngs`
    #	`ls -rt ${other}/${folder}/*_diff.png > ${other}/${folder}/diff_pngs`
    #done
    cat ${HOME}/McCode/generate_testplots/footer.html >> McStas-2.5_CPU_MPICC_5e7/$folder/browse.html
done

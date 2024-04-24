macro "OIB_to_TIFF"{
dir = getDirectory("Choose a Directory ");
list = getFileList(dir);
setBatchMode(true);
for (f=0; f<list.length; f++) {
   if (endsWith(list[f], ".tif")){ 
   	print(list[f]);
  // run("Bio-Formats Importer", "open=["+dir+list[f]+"] autoscale color_mode=Default rois_import=[ROI manager] view=Hyperstack stack_order=XYCZT");
 	open(list[f]);
 	name = getTitle();
 	selectWindow(name);
 	setAutoThreshold("Otsu dark");
	setOption("BlackBackground", true);
	run("Convert to Mask");
 	name= replace(name,"AVG", "THR");
    saveDir = replace(dir,"AVG", "THR");
 	pathToTIF = saveDir + name;
 	saveAs("Tiff", pathToTIF); 
	}
}
setBatchMode(false);	
}

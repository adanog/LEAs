macro "OIB_to_TIFF"{
dir = getDirectory("Choose a Directory ");
list = getFileList(dir);
setBatchMode(true);
for (f=0; f<list.length; f++) {
   if (endsWith(list[f], ".oib")){ 
   	print(list[f]);
   run("Bio-Formats Importer", "open=["+dir+list[f]+"] autoscale color_mode=Default rois_import=[ROI manager] view=Hyperstack stack_order=XYCZT");
 	//open(list[f]);
 	run("Grays");
 	name = getTitle();
 	selectWindow(name);
 	replace(name,".oib", ".tif");
 	 pathToTIF = dir + name;
 	 saveAs("Tiff", pathToTIF); 
	}
}
setBatchMode(false);	
}


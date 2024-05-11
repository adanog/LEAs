macro "OIB_to_TIFF" {
    dir = getDirectory("Choose a Directory "); // Prompts user to select a directory and stores the path in 'dir'
    list = getFileList(dir); // Retrieves a list of all files in the chosen directory
    setBatchMode(true); // Activates batch mode to prevent display of images during processing, which speeds up execution

    for (f = 0; f < list.length; f++) { // Loop through all files in the directory
        if (endsWith(list[f], ".oib")) { // Check if the current file has an '.oib' extension
            print(list[f]); // Print the name of the OIB file being processed
            // Use Bio-Formats Importer to open OIB files with specified settings
            run("Bio-Formats Importer", "open=[" + dir + list[f] + "] autoscale color_mode=Default rois_import=[ROI manager] view=Hyperstack stack_order=XYCZT");
            // Optionally, you could directly open the file with: open(list[f]);
            run("Grays"); // Convert the image to grayscale
            name = getTitle(); // Get the title of the currently open image window
            selectWindow(name); // Select the window that has the image
            replace(name, ".oib", ".tif"); // Replace the file extension in the title from '.oib' to '.tif'
            pathToTIF = dir + name; // Create the full path for the new TIFF file
            saveAs("Tiff", pathToTIF); // Save the current image as a TIFF at the specified path
        }
    }
    setBatchMode(false); // Deactivate batch mode, allowing images to be displayed again
}

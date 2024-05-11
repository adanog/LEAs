macro "TIFF_to_Thresholded_Mask" {
    dir = getDirectory("Choose a Directory "); // Prompt user to select a directory and store the path in 'dir'
    list = getFileList(dir); // Retrieve a list of all files in the chosen directory
    setBatchMode(true); // Activate batch mode to prevent display of images during processing, speeding up the execution

    for (f = 0; f < list.length; f++) { // Loop through all files in the directory
        if (endsWith(list[f], ".tif")) { // Check if the current file has a '.tif' extension
            print(list[f]); // Print the name of the TIFF file being processed
            // Use Bio-Formats Importer to open TIFF files with specified settings; this line is commented out
            // run("Bio-Formats Importer", "open=[" + dir + list[f] + "] autoscale color_mode=Default rois_import=[ROI manager] view=Hyperstack stack_order=XYCZT");
            open(list[f]); // Open the TIFF file directly
            name = getTitle(); // Get the title of the currently open image window
            selectWindow(name); // Select the window that has the image
            setAutoThreshold("Otsu dark"); // Automatically apply the Otsu thresholding method to differentiate foreground from background
            setOption("BlackBackground", true); // Ensure the background is considered black for thresholding
            run("Convert to Mask"); // Convert the image to a binary mask based on the thresholding
            name = replace(name, "AVG", "THR"); // Replace 'AVG' with 'THR' in the filename, typically indicating a thresholded version
            saveDir = replace(dir, "AVG", "THR"); // Change directory path from 'AVG' to 'THR' for saving
            pathToTIF = saveDir + name; // Create the full path for the new TIFF file
            saveAs("Tiff", pathToTIF); // Save the current image as a TIFF at the specified path
        }
    }
    setBatchMode(false); // Deactivate batch mode, allowing images to be displayed again
}

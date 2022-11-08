//Macro to batch process RGB vegetation indexes for the Densiplant Project

#@ File(label="Input directory RGB microplot images", description="Select the directory with input images", style="directory") inputDir1

#@ File(label="Output directory", description="Select the output directory", style="directory") outputDir

//quick technical fix for the nifty interactive file location selections above
input = inputDir1 + "/";

output = outputDir + "/";

//activate if you do not want to watch the computer process (delete the "//" at the start of the following line)
setBatchMode(true);

//activate if you are importing files from QGIS in TIFF Format!
//run("ImageJ2...", "scijavaio=true loglevel=INFO");

//start out with a clean slate
	call("java.lang.System.gc");
	run("Clear Results");
	run("Close All");
	print("\\Clear");

//set up the log file for transposing the results
	print("Densiplant microplots RGB Vegetation Indexes RGB HSI LAB GA GGA NGRDI TGI MGRVI ExcessGreen ExcessRed NDLab uVA x QGIS-Plot saturation-s and -veg (values 0-1 NGRDIveg selection)");
a = newArray("filename","Red","Green","Blue","Hue","Saturation","Intensity","Lightness","a*","b*","GA","GGA","NGRDI","NGRDIs","NGRDIveg","TGI","TGIs","TGIveg","MGRVI","ExcessGreen","ExcessGreens","ExcessGreenveg","ExcessGreen-ExcessRed","ExcessGreen-ExcessReds","ExcessGreen-ExcessRedveg","NDLab","NDLabs","NDLabveg","uVA","uVAs");
Array.print(a);

//start batching
list = getFileList(input);
for (i = 0; i < list.length; i++)
       action(input, output, list[i]);
function action(input, output, filename) {

open(input + filename);

/*
//open QGIS TIFF files, process and create QGIS rotated image mask to exclude 0,0,0, values
open(input + filename);
selectWindow(filename);
run("Stack to Images");
run("Merge Channels...", "c1=1 c2=2 c3=3");
rename(filename);

//create rotated image mask and selection
selectWindow("4");
setAutoThreshold("Default dark");
setThreshold(1, 255);
setOption("BlackBackground", true);
run("Convert to Mask");
rename("QGISMask");
selectWindow("QGISMask");
run("Create Selection");
*/

//create optional saturation-based filtering 
selectWindow(filename);

//optional saturation filtering
	run("HSB Stack");
	run("Stack to Images");
	close("Hue");
	close("Brightness");
selectWindow("Saturation");
	setThreshold(25, 255); //remove bottom 5th or 10th "pale" pixels 51=255/5 25= 255/10
	run("Convert to Mask");
	rename("SaturationMask");
selectWindow("SaturationMask");
	
//start common literature-based RGB Vegetation Indexes by opening a new copy of the same image

//Get average band values for R,G,B, HSI, and CieLab bands

//open and get id and base name for RGB images
    open(input + filename);
selectWindow(filename);
//run("Stack to Images");
//run("Merge Channels...", "c1=1 c2=2 c3=3");
//rename(filename);
//close("4");

//Create HSB images
selectWindow(filename);
	run("RGB Stack");
	run("32-bit");
	run("Stack to Images");
//QGIS MASK RE-CREATION
//selectWindow("QGISMask");
//run("Create Selection");	
// Measure and define HSB one image at a time based on the QGIS mask selection
selectWindow("Red");
run("Restore Selection");
	run("Set Measurements...", "mean redirect=None decimal=2");
	run("Measure");
R = getResult("Mean");
selectWindow("Green");
run("Restore Selection");
	run("Set Measurements...", "mean redirect=None decimal=2");
	run("Measure");
G = getResult("Mean");
selectWindow("Blue");
run("Restore Selection");
	run("Set Measurements...", "mean redirect=None decimal=2");
	run("Measure");
B = getResult("Mean");
//close RGB
	close("Red");
	close("Green");
	close("Blue");

//open and get id and base name for HSI
    open(input + filename);
//selectWindow(filename);
//run("Stack to Images");
//run("Merge Channels...", "c1=1 c2=2 c3=3");
//rename(filename);
//close("4");

//Create HSB images
selectWindow(filename);
	run("HSB Stack");
	run("32-bit");
selectWindow(filename);
	run("Stack to Images");
//QGIS MASK RE-CREATION
//selectWindow("QGISMask");
//run("Create Selection");
// Measure and define HSB one image at a time based on the scanner mask selection
selectWindow("Hue");
run("Restore Selection");
	run("Set Measurements...", "mean redirect=None decimal=2");
	run("Measure");
H = getResult("Mean");
selectWindow("Saturation");
run("Restore Selection");
	run("Set Measurements...", "mean redirect=None decimal=2");
	run("Measure");
S = getResult("Mean");
selectWindow("Brightness");
run("Restore Selection");
	run("Set Measurements...", "mean redirect=None decimal=2");
	run("Measure");
I = getResult("Mean");
//close HSB
selectWindow("Hue");
	close("Hue");
selectWindow("Saturation");	
	close("Saturation");
selectWindow("Brightness");
	close("Brightness");
	
//open and get id and base name for CIE Lab
    open(input + filename);
selectWindow(filename);
//run("Stack to Images");
//run("Merge Channels...", "c1=1 c2=2 c3=3");
//rename(filename);
//close("4");

//Create Lab images
selectWindow(filename);
	run("Lab Stack");
selectWindow(filename);
	run("Stack to Images");
//QGIS MASK RE-CREATION
//selectWindow("QGISMask");
//run("Create Selection");
// Measure and define CIELab one image at a time based on the scanner mask selection
selectWindow("L*");
run("Restore Selection");
	run("Set Measurements...", "mean redirect=L* decimal=2");
	run("Measure");
L = getResult("Mean");
selectWindow("a*");
run("Restore Selection");
	run("Set Measurements...", "mean redirect=a* decimal=2");
	run("Measure");
a = getResult("Mean");
selectWindow("b*");
run("Restore Selection");
	run("Set Measurements...", "mean redirect=b* decimal=2");
	run("Measure");
b = getResult("Mean");
//close CIELab
selectWindow("L*");
	close("L*");
selectWindow("a*");	
	close("a*");
selectWindow("b*");
	close("b*");

//create threshold classification of green vegetation (GA or greenveg)
//open and get id and base name
    open(input + filename);
//selectWindow(filename);
//run("Stack to Images");
//run("Merge Channels...", "c1=1 c2=2 c3=3");
//rename(filename);
//close("4");

//convert to HSB image space and split channels for processing
selectWindow(filename);
	run("HSB Stack");
	run("32-bit");
selectWindow(filename);
	run("Stack to Images");
//QGIS MASK RE-CREATION
//selectWindow("QGISMask");
//run("Create Selection");
//create greenveg mask from HSB Hue
selectWindow("Saturation");
	close("Saturation");
selectWindow("Brightness");
	close("Brightness");
selectWindow("Hue");
	setThreshold(42.5, 127.5);
	rename("greenveg");
selectWindow("greenveg");
run("Restore Selection");
	run("Set Measurements...", "area_fraction redirect=None decimal=2");
	run("Measure");
GA = getResult("%Area");
selectWindow("greenveg");
	close("greenveg");
//
//create threshold classification of greener green vegetation (GGA or greenerveg)
//open and get id and base name
    open(input + filename);
selectWindow(filename);
//run("Stack to Images");
//run("Merge Channels...", "c1=1 c2=2 c3=3");
//rename(filename);
//close("4");

//convert to HSB image space and split channels for processing
selectWindow(filename);
	run("HSB Stack");
	run("32-bit");
selectWindow(filename);
	run("Stack to Images");
//QGIS MASK RE-CREATION
//selectWindow("QGISMask");
//run("Create Selection");
//create greenerveg mask from HSB Hue
selectWindow("Saturation");
	 close("Saturation");
selectWindow("Brightness");
	 close("Brightness");
selectWindow("Hue");
	setThreshold(56.67, 127.5);
	rename("greenerveg");
selectWindow("greenerveg");
run("Restore Selection");
	run("Set Measurements...", "area_fraction redirect=None decimal=2");
	run("Measure");
GGA = getResult("%Area");
selectWindow("greenerveg");
	close("greenerveg");

//open and get id and base name
    open(input + filename);
selectWindow(filename);
//run("Stack to Images");
//run("Merge Channels...", "c1=1 c2=2 c3=3");
//rename(filename);
//close("4");

//get info from original image
    original = getImageID;
    title = getTitle; 
    base = File.nameWithoutExtension;

//Calculate the Normalized Green Red Difference Index (NGRDI) and Triangular Greeness (TGI) //for the leaf area
//These are vegetation indexes from RGB components based on Hunt et al. 2014. 
selectWindow(filename);

//convert to separate red, green and blue images for processing
	run("Split Channels");
selectWindow(filename + " (red)");
	rename("IMG_(red)");
selectWindow(filename + " (green)");
	rename("IMG_(green)");
selectWindow(filename + " (blue)");
	rename("IMG_(blue)");

//use image math calculator plus functions to calculate the NGRDI
	imageCalculator("Add create 32-bit", "IMG_(green)","IMG_(red)");
	rename("ResultG+R");
	imageCalculator("Subtract create 32-bit", "IMG_(green)","IMG_(red)");
	rename("ResultG-R");
	imageCalculator("Divide create 32-bit", "ResultG-R","ResultG+R");
	rename("IMG_NGRDI");
selectWindow("IMG_NGRDI");

//QGIS MASK RE-CREATION
//selectWindow("QGISMask");
//run("Create Selection");
	
//Calculate the mean NGRDI value for the total of the leaf area
selectWindow("IMG_NGRDI");
run("Restore Selection");
	run("Set Measurements...", "mean redirect=None decimal=3");
	run("Measure");
NGRDI = getResult("Mean");
	run("Select None");	

//Create NGRDI VegMask image
selectWindow("IMG_NGRDI");
	run("Duplicate...", "title=VegMask");
	setThreshold(0.25, 1);
	run("Convert to Mask");
	rename("VegMask");

//// Minimal Color Saturation Mask NGRDI
selectWindow("SaturationMask");
	run("Create Selection");
selectWindow("IMG_NGRDI");
	run("Restore Selection");
	run("Set Measurements...", "mean redirect=None decimal=3");
	run("Measure");
NGRDIs = getResult("Mean");	
	run("Select None");

////Minimal Vegetation Mask NGRDI
selectWindow("VegMask");
	run("Create Selection");
selectWindow("IMG_NGRDI");
	run("Restore Selection");
	run("Set Measurements...", "mean redirect=None decimal=3");
	run("Measure");
NGRDIveg = getResult("Mean");	
	run("Select None");

//use image math calculator plus functions to calculate the TGI
imageCalculator("Subtract create 32-bit", "IMG_(red)","IMG_(blue)");
	rename("ResultR-B");
imageCalculator("Subtract create 32-bit", "IMG_(red)","IMG_(green)");
	rename("ResultR-G");
selectWindow("ResultR-G");
	run("Multiply...", "value=190");
selectWindow("ResultR-B");
	run("Multiply...", "value=120");
	run("Calculator Plus", "i1=ResultR-G i2=ResultR-B operation=[Subtract: i2 = (i1-i2) x k1 + k2] k1=-0.5 k2=0 create");
rename("IMG_TGI");

//QGIS MASK RE-CREATION
//selectWindow("QGISMask");
//run("Create Selection");

//Calculate the mean TGI value for the total of the leaf area
selectWindow("IMG_TGI");
run("Restore Selection");
	run("Set Measurements...", "mean redirect=None decimal=3");
	run("Measure");
TGI = getResult("Mean");
	run("Select None");	

//Minimal Color Saturation Mask TGI
selectWindow("SaturationMask");
	run("Create Selection");
selectWindow("IMG_TGI");
	run("Restore Selection");
	run("Set Measurements...", "mean redirect=None decimal=3");
	run("Measure");
TGIs = getResult("Mean");	
	run("Select None");	

//Minimal Vegetation Mask TGI
selectWindow("VegMask");
	run("Create Selection");
selectWindow("IMG_TGI");
	run("Restore Selection");
	run("Set Measurements...", "mean redirect=None decimal=3");
	run("Measure");
TGIveg = getResult("Mean");	
	run("Select None");	

//use image math calculator plus functions to calculate the MGRVI
imageCalculator("Multiply create 32-bit", "IMG_(green)","IMG_(green)");
	rename("G2");
imageCalculator("Multiply create 32-bit", "IMG_(red)","IMG_(red)");
	rename("R2");
imageCalculator("Add create 32-bit", "G2","R2");
	rename("ResultG2+R2");
imageCalculator("Subtract create 32-bit", "G2","R2");
	rename("ResultG2-R2");
imageCalculator("Divide create 32-bit", "ResultG2-R2","ResultG2+R2");
	rename("IMG_MGRVI");
selectWindow("IMG_MGRVI");

//QGIS MASK RE-CREATION
//selectWindow("QGISMask");
//run("Create Selection");

//Calculate the mean MGRVI value for the microplot
selectWindow("IMG_MGRVI");
run("Restore Selection");
	run("Set Measurements...", "mean redirect=None decimal=3");
	run("Measure");
MGRVI = getResult("Mean");
	run("Select None");	

//Excess Green Index and Excess Green-Excess Red
//Meyer, G. E., & Neto, J. C. (2008). Verification of color vegetation indices for automated crop imaging applications. Computers and electronics in agriculture, 63(2), 282-293.
//Torres-Sánchez, J., López-Granados, F., & Peña, J. M. (2015). An automatic object-based method for optimal thresholding in UAV images: Application for vegetation detection in herbaceous crops. Computers and Electronics in Agriculture, 114, 43-52.
	run("Image Expression Parser (Macro)", "expression=(2*B-(A+C))/(A+B+C) a=IMG_(red) b=IMG_(green) c=IMG_(blue)");
	rename("excessGreen");

//QGIS MASK RE-CREATION
//selectWindow("QGISMask");
//run("Create Selection");

//Calculate the mean excessGreen value for the total of the leaf area
selectWindow("excessGreen");
run("Restore Selection");
	run("Set Measurements...", "mean redirect=None decimal=3");
	run("Measure");
xGreen = getResult("Mean");

//Minimal Color Saturation Mask excessGreen
selectWindow("SaturationMask");
	run("Create Selection");
selectWindow("excessGreen");
	run("Restore Selection");
	run("Set Measurements...", "mean redirect=None decimal=3");
	run("Measure");
xGreens = getResult("Mean");	
	run("Select None");	

//Minimal Vegetation Mask excessGreen
selectWindow("VegMask");
	run("Create Selection");
selectWindow("excessGreen");
	run("Restore Selection");
	run("Set Measurements...", "mean redirect=None decimal=3");
	run("Measure");
xGreenveg = getResult("Mean");
	run("Select None");	

//QGIS MASK RE-CREATION
//selectWindow("QGISMask");
//run("Create Selection");

//Calculate excessRed for xGreen-xRed
run("Image Expression Parser (Macro)", "expression=(2*A-(B+C))/(A+B+C) a=IMG_(red) b=IMG_(green) c=IMG_(blue)");
	rename("excessRed");
	
//Calculate xGreen-xRed	
	imageCalculator("Subtract create 32-bit", "excessGreen","excessRed");
selectWindow("Result of excessGreen");
	rename("excessGreen-excessRed");
	run("Restore Selection");
	run("Set Measurements...", "mean redirect=None decimal=3");
	run("Measure");
xGreenxRed = getResult("Mean");	

//Minimal Color Saturation Mask excessGreen-excessRed
selectWindow("SaturationMask");
	run("Create Selection");
selectWindow("excessGreen-excessRed");
	run("Restore Selection");
	run("Set Measurements...", "mean redirect=None decimal=3");
	run("Measure");
xGreenxReds = getResult("Mean");	
	run("Select None");	

//Minimal Vegetation Mask excessGreen-excessRed
selectWindow("VegMask");
	run("Create Selection");
selectWindow("excessGreen-excessRed");
	run("Restore Selection");
	run("Set Measurements...", "mean redirect=None decimal=3");
	run("Measure");
xGreenxRedveg = getResult("Mean");
	run("Select None");

//Optionally save and close all RGB-based vegetation images before next in the batch
selectWindow("IMG_NGRDI");
//	saveAs("Tiff", output+base+"_NGRDI");
selectWindow("IMG_TGI");
//	saveAs("Tiff", output+base+"_TGI");
selectWindow("excessGreen");
//	saveAs("Tiff", output+base+"_xGreen");
selectWindow("excessGreen-excessRed");
//	saveAs("Tiff", output+base+"_xGreen-xRed");

//close out old files
close("IMG_(blue)");
close("IMG_(green)");
close("IMG_(red)");
close("R+G+B");
close("ResultG-R");
close("ResultG+R");
close("IMG_NGRDI");
close("IMG_TGI");
close("excessGreen");
close("excessRed");
close("excessGreen-excessRed");

//open new file for CIELab index NDLab, NDLab = (((1 − a*) − b*)/((1 − a*) + b*) + 1).
//Buchaillot, M., Gracia-Romero, A., Vergara-Diaz, O., Zaman-Allah, M. A., Tarekegne, A., Cairns, J. E., ... & Kefauver, S. C. (2019). Evaluating maize genotype performance under low nitrogen conditions using RGB UAV phenotyping techniques. Sensors, 19(8), 1815. 
open(input + filename);
selectWindow(filename);
//run("Stack to Images");
//run("Merge Channels...", "c1=1 c2=2 c3=3");
//rename(filename);    
//close("4");

//CIELab color space
selectWindow(filename);
	run("Lab Stack");
	run("Stack to Images");
selectWindow("L*");
	run("Grays");
selectWindow("a*");
	run("Grays");
selectWindow("b*");
	run("Grays");  
run("Image Expression Parser (Macro)", "expression=(((1-A)-B)/((1-A)+B)+1) a=a* b=b*");
	rename("NDLab");

//QGIS MASK RE-CREATION
//selectWindow("QGISMask");
//run("Create Selection");

//Calculate the mean NDLab value for the total of the leaf area
selectWindow("NDLab");
run("Restore Selection");
	run("Set Measurements...", "mean redirect=None decimal=3");
	run("Measure");
NDLab = getResult("Mean");

//Minimal Color Saturation Mask NDLab
selectWindow("SaturationMask");
	run("Create Selection");
selectWindow("NDLab");
	run("Restore Selection");
	run("Set Measurements...", "mean redirect=None decimal=3");
	run("Measure");
NDLabs = getResult("Mean");	
	run("Select None");	

//Minimal Vegetation Mask NDLab
selectWindow("VegMask");
	run("Create Selection");
selectWindow("NDLab");
	run("Restore Selection");
	run("Set Measurements...", "mean redirect=None decimal=3");
	run("Measure");
NDLabveg = getResult("Mean");
	run("Select None");

//uVA is a bit special so we'll do that last
open(input + filename);
selectWindow(filename);
//run("Stack to Images");
//run("Merge Channels...", "c1=1 c2=2 c3=3");
//rename(filename);    
//close("4");

//QGIS MASK RE-CREATION
//selectWindow("QGISMask");
//run("Create Selection");

//Calculate the uVA vegetation index to select green pixels based on the CIELuv color space
	run("Color Modified2", "colour=LCHLuv2");		// Obtained from Color_Modified2.ijm 
	run("Stack to Images");							// H Matrix save angle in degrees
selectWindow("C");								// C Matrix save 'Binary' output
rename("uVA");
run("Restore Selection");
	run("Set Measurements...", "area_fraction redirect=None decimal=3");
	run("Measure");
uVA = getResult("%Area");

//Minimal Color Saturation Mask uVA
selectWindow("SaturationMask");
	run("Create Selection");
selectWindow("uVA");
	run("Duplicate...", "title=uVAs");
	run("Restore Selection");
	run("Clear Outside");
	run("Select All");
	run("Set Measurements...", "area_fraction redirect=None decimal=3");
	run("Measure");
uVAs = getResult("%Area");	
	run("Select None");	

//saveAs("Tiff", output+base+"_Seg_ColorSpace-LCHLuv-Otsu-C");

//clean up memory after each file
	run("Clear Results");
	run("Close All");
	call("java.lang.System.gc");	

//memory should be clear and all windows closed except for Results and Log files
//copy the data over to an organized transposed log of the filename and 2 measurements from //each image 
r = newArray(filename,R,G,B,H,S,I,L,a,b,GA,GGA,NGRDI,NGRDIs,NGRDIveg,TGI,TGIs,TGIveg,MGRVI,xGreen,xGreens,xGreenveg,xGreenxRed,xGreenxReds,xGreenxRedveg,NDLab,NDLabs,NDLabveg,uVA,uVAs);
Array.print(r);

//end of process move on to next in batch
}

//save the result from the log window printouts
//Log with ordered Results 
selectWindow("Log");  
	saveAs("Text", output + "Densiplant microplots RGB-VIs RGB HSI CIELab NGRDI TGI MGRVI ExcessGreen NDLab NDLuv uVA x Optional Saturation-S and Area-A with Plot-Veg masks.csv");

// deactivate scijavaio
//run("ImageJ2...", "scijavaio=false loglevel=INFO");

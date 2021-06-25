./ ADD NAME=$$DOC$$  IEBUPDTE DOCUMENTATION (MEMBER $$DOC$$ IS UNUSED!) 00001   
                                                                        00002   
THIS PANVALET MEMBER IS THE SYSIN INPUT TO IBM'S IEBUPDTE UTILITY,      00003   
WHICH CAN BE USED TO DYNAMICALLY CREATE PDS MEMBERS IN A DATASET        00004   
REFERENCED BY DDNAME SYSUT2 (TYPICALLY, A TEMPORARY DATASET).           00005   
                                                                        00006   
EACH "./ ADD NAME=" STATEMENT IN THIS PANVALET MEMBER CAUSES THE        00007   
CREATION OF A PDS MEMBER WITH THE GIVEN NAME IN THE SYSUT2 DATASET.     00008   
THESE MEMBERS CAN THEN BE REFERENCED IN SUBSEQUENT JOB STEP(S).         00009   
                                                                        00010   
THIS IS A VERY USEFUL TECHNIQUE TO EMPLOY WHEN MULTIPLE SETS OF         00011   
CONTROL CARDS ARE NEEDED WITHIN A SINGLE JOB, BECAUSE IT OBVIATES THE   00012   
NEED TO MAINTAIN A SEPARATE PANVALET MEMBER FOR EACH SET OF CONTROL     00013   
CARDS REQUIRED BY THE JOB.                                              00014   
                                                                        00015   
NOTE THAT THE PANVALET SUBSYSTEM MUST BE USED TO READ THIS MEMBER       00016   
VIA A SYSIN DD STATEMENT. EXAMPLE:                                      00017   
                                                                        00018   
 //SYSIN   DD DSN=PAN.APPL.LIBRARY,SUBSYS=(PANV,,MEMBERNAME),DISP=SHR   00019   
                                                                        00020   
FOR TESTING PURPOSES, CONCATENATION OF PANVALET LIBRARIES IS POSSIBLE   00021   
BY USING A PANDD11 DD STATEMENT. EXAMPLE:                               00022   
                                                                        00023   
 //SYSIN   DD DSN=PAN.DEIS.LIBRARY,SUBSYS=(PANV,,MEMBERNAME),DISP=SHR   00024   
 //PANDD11 DD DSN=PAN.APPL.LIBRARY,DISP=SHR                             00025   
                                                                        00026   
HOWEVER, IF THE MEMBER BEING READ BEGINS WITH A PANVALET "SPECIAL       00027**2
COMMENT" RECORD (WHICH WILL BE TRUE IF THE MEMBER'S LANGUAGE CODE IS    00028**2
PL/1, ASM, ETC), THEN THE IEBUPDTE STEP WILL COMPLETE WITH CC=4 UNLESS  00029**2
WE FORCE AN IEBUPDTE CONTROL CARD FIRST IN THE SYSIN CONCATENATION. A   00030**2
PANVALET MEMBER NAMED IEB$ADDCOM HAS BEEN ESPECIALLY CREATED FOR THIS   00031**2
PURPOSE, AND WILL CAUSE THE IEBUPDTE CC TO BE ZERO. E.G.:               00032**2
                                                                        00033**2
 //SYSIN   DD DSN=PAN.APPL.LIBRARY,SUBSYS=(PANV,,IEB$ADDCOM),DISP=SHR   00034**2
 //        DD DSN=PAN.APPL.LIBRARY,SUBSYS=(PANV,,MEMBERNAME),DISP=SHR   00035**2
                                                                        00036**2
OR                                                                      00037**2
                                                                        00038**2
 //SYSIN   DD DSN=PAN.APPL.LIBRARY,SUBSYS=(PANV,,IEB$ADDCOM),DISP=SHR   00039**2
 //        DD DSN=PAN.DEIS.LIBRARY,SUBSYS=(PANV,,MEMBERNAME),DISP=SHR   00040**2
 //PANDD11 DD DSN=PAN.APPL.LIBRARY,DISP=SHR                             00041**2
                                                                        00042**2

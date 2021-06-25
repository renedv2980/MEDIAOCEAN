*          DATA SET FABSAMOPCL AT LEVEL 004 AS OF 03/06/98                      
*PHASE BSAMOPCL                                                                 
*----------------------------------------------------------------*              
* THIS PROGRAM OPENS AND CLOSES A SEQ OUTPUT FILE TO REMOVE ALL  *              
* RECORDS.                                                       *              
* ORIGINAL PURPOSE WAS FOR PACK TO ERASE PBUYREQ FILE            *              
*----------------------------------------------------------------*              
         SPACE 1                                                                
BSAMOPCL CSECT                                                                  
         NBASE 0,BSAMOPCL,REGSAVE                                               
         OPEN  (FILEOUT,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLOSE FILEOUT                                                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         XBASE                                                                  
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,RECFM=FB,LRECL=4000,            X        
               BLKSIZE=8000,MACRF=PM                                            
         LTORG                                                                  
*                                                                               
REGSAVE  DS    100D                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004FABSAMOPCL03/06/98'                                      
         END                                                                    

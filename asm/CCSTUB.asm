*          DATA SET CCSTUB     AT LEVEL 176 AS OF 10/13/98                      
*PHASE TA0C00B                                                                  
*INCLUDE MADUSUK                                                                
TA0C00   TITLE 'C MODULE TEST STUB'                                             
*                                                                               
TA0C00   START                                                                  
         PRINT NOGEN                                                            
         NMODL LENWORK,**MAD0**,RA,R9,CLEAR=YES,RR=RE                           
         USING WORKD,RC                                                         
         ST    RE,RELO                                                          
*        L     RF,=V(@STATICP)                                                  
*        L     RF,=V(MAIN)                                                      
         L     RF,=V(MADUSUK)                                                   
*        L     RF,=V(@@DOPLNK)                                                  
*        L     RF,=V(@@IPAOBJ)                                                  
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         DC    H'0'                                                             
*                                  RC = A(NMOD WORK AREA)                       
***********************************************************************         
*        SAVE ADDRESSES PASSED ON THE PARAMETER LIST FROM FAMONITOR             
***********************************************************************         
*        L     RF,0(R1)            SAVE A(SYSFACS)                              
*        ST    RF,ASYSFACS                                                      
*        L     RF,4(R1)            SAVE A(TIA)                                  
*        ST    RF,ATIA                                                          
*        L     RF,8(R1)            SAVE A(UTL)                                  
*        ST    RF,AUTL                                                          
*        L     RF,12(R1)           SAVE A(COMFACS)                              
*        ST    RF,ACOMFACS                                                      
*        L     RF,20(R1)           SAVE A(TWA)                                  
*        ST    RF,ATWA                                                          
*        L     RF,28(R1)           SAVE A(TIOB)                                 
*        ST    RF,ATIOB                                                         
         LTORG                                                                  
         EJECT                                                                  
                                                                                
WORKD    DSECT                                                                  
RELO     DS    A                                                                
LENWORK  EQU   *-WORKD                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'176CCSTUB    10/13/98'                                      
         END                                                                    
